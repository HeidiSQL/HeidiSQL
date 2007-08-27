unit Childwin;


// -------------------------------------
// MDI-Child-Window
// -------------------------------------


interface


uses
  Synchronization,
  Windows, Classes, Graphics, Forms, Controls, StdCtrls,
  ExtCtrls, ComCtrls, ImgList, SysUtils, Dialogs, Menus,
  SynEdit, SynMemo, SynEditHighlighter, SynHighlighterSQL, SynEditSearch,
  SynEditTypes, Registry, Clipbrd,
  Buttons, CheckLst, ToolWin, Db, DBGrids,
  DBCtrls, helpers,
  Grids, ZDataset,
  ZAbstractRODataset, ZConnection,
  ZSqlMonitor, EDBImage, ZDbcLogging,
  SynCompletionProposal, HeidiComp, SynEditMiscClasses, MysqlQuery,
  MysqlQueryThread, queryprogress, communication, MysqlConn, smdbgrid, Tabs,
  VirtualTrees;


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
    menuinsert: TMenuItem;
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
    Insertrecord2: TMenuItem;
    MenuAddField: TMenuItem;
    MenuEditField: TMenuItem;
    popupResultGrid: TPopupMenu;
    Copyrecords1: TMenuItem;
    CopyasCSVData1: TMenuItem;
    N9: TMenuItem;
    LabelResultinfo: TLabel;
    MenuAdvancedProperties: TMenuItem;
    N10: TMenuItem;
    MenuRenameTable: TMenuItem;
    MenuViewBlob: TMenuItem;
    TimerConnected: TTimer;
    N12: TMenuItem;
    MenuTableComment: TMenuItem;
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
    gridData: TSMDBGrid;
    DataSource1: TDataSource;
    gridQuery: TSMDBGrid;
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
    MenuChangeType: TMenuItem;
    MenuChangeType2: TMenuItem;
    MenuChangeType3: TMenuItem;
    MenuChangeTypeOther: TMenuItem;
    N8: TMenuItem;
    MenuChangeType1: TMenuItem;
    MenuChangeType4: TMenuItem;
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
    MenuChangeType5: TMenuItem;
    selectall1: TMenuItem;
    MenuAutoupdate: TMenuItem;
    TimerProcesslist: TTimer;
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
    DefaultColumnLayout1: TMenuItem;
    N20: TMenuItem;
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
    btnAltTerminator: TToolButton;
    pnlQueryHelpers: TPanel;
    tabsetQueryHelpers: TTabSet;
    lboxQueryHelpers: TListBox;
    popupQuery: TPopupMenu;
    MenuRun: TMenuItem;
    MenuRunSelection: TMenuItem;
    MenuRunLine: TMenuItem;
    MenuSetFilter: TMenuItem;
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
    btnDbInsertRecord: TToolButton;
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
    btnTableInsertRecord: TToolButton;
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
    procedure btnDbViewDataClick(Sender: TObject);
    procedure btnDataClick(Sender: TObject);
    procedure DBtreeExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure ListTablesClick(Sender: TObject);
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
      const Value: String; Shift: TShiftState; Index: Integer; EndToken: Char);
    procedure btnDbPropertiesClick(Sender: TObject);
    procedure popupDbGridPopup(Sender: TObject);
    procedure SynCompletionProposal1CodeCompletion(Sender: TObject;
      var Value: String; Shift: TShiftState; Index: Integer; EndToken: Char);
    procedure SynCompletionProposal1Execute(Kind: TSynCompletionType;
      Sender: TObject; var CurrentInput: String; var x, y: Integer;
      var CanExecute: Boolean);
    procedure PerformConnect;
    procedure ToolButton4Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ReadDatabasesAndTables(Sender: TObject);
    procedure DBtreeChange(Sender: TObject; Node: TTreeNode);
    procedure pcChange(Sender: TObject);
    procedure viewdata(Sender: TObject);
    procedure ShowDBProperties(Sender: TObject);
    procedure ShowTableProperties(Sender: TObject);
    procedure ValidateControls(FrmIsFocussed: Boolean = true);
    procedure SelectHost;
    procedure SelectDatabase(db: String);
    procedure SelectTable(db: String; table: String; switchView: boolean = true);
    procedure ShowTableData(table: string);
    procedure ShowTable(Sender: TObject);
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
    procedure MenuAdvancedPropertiesClick(Sender: TObject);
    procedure ListTablesNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; NewText: WideString);
    procedure MenuRenameTableClick(Sender: TObject);
    procedure MenuViewBlobClick(Sender: TObject);
    procedure TimerConnectedTimer(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure MenuTableCommentClick(Sender: TObject);
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
    procedure MenuChangeTypeClick(Sender: TObject);
    procedure MenuChangeTypeOtherClick(Sender: TObject);
    procedure InsertRecord(Sender: TObject);
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
      AFiles: TStrings);
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
    procedure DBGridGetCellParams(Sender: TObject; Field: TField;
      AFont: TFont; var Background: TColor; Highlight: Boolean);
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
    procedure ZQuery1EditError(DataSet: TDataSet; E: EDatabaseError;
      var Action: TDataAction);
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

    private
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
      CachedTableLists           : TStringList;
      QueryHelpersSelectedItems  : Array[0..3] of Integer;

      function GetQueryRunning: Boolean;
      procedure SetQueryRunning(running: Boolean);
      procedure GridHighlightChanged(Sender: TObject);
      procedure SaveBlob;
      function GetActiveGrid: TSMDBGrid;
      procedure WaitForQueryCompletion(WaitForm: TForm);
      function RunThreadedQuery(AQuery : String) : TMysqlQuery;
      procedure DisplayRowCountStats(ds: TDataSet);
      procedure insertFunction(Sender: TObject);

    public
      ActualDatabase             : String;
      ActualTable                : String;
      dataselected               : Boolean;
      editing                    : Boolean;
      mysql_version              : Integer;
      tnodehost                  : TTreeNode;
      Description                : String;
      DBRightClickSelectedItem   : TTreeNode;    // TreeNode for dropping with right-click
      VTRowDataListVariables,
      VTRowDataListProcesses,
      VTRowDataListCommandStats,
      VTRowDataListTables,
      VTRowDataListColumns       : TVTreeDataArray;

      FProgressForm              : TFrmQueryProgress;
      procedure Init(AConn : POpenConnProf; AMysqlConn : TMysqlConn);
      //procedure HandleQueryNotification(ASender : TMysqlQuery; AEvent : Integer);
      function GetVisualDataset() : TDataSet;

      function ExecUpdateQuery(sql: string; HandleErrors: Boolean = false; DisplayErrors: Boolean = false): Int64;
      function ExecSelectQuery(sql: string; HandleErrors: Boolean = false; DisplayErrors: Boolean = false): TDataSet;
      procedure ExecUseQuery(db: string; HandleErrors: Boolean = false; DisplayErrors: Boolean = false);

      property FQueryRunning: Boolean read GetQueryRunning write SetQueryRunning;
      property ActiveGrid: TSMDBGrid read GetActiveGrid;
      property MysqlConn : TMysqlConn read FMysqlConn;
      property Conn : TOpenConnProf read FConn;

      function FetchActiveDbTableList: TDataSet;
      function RefreshActiveDbTableList: TDataSet;
      procedure ClearAllTableLists;
      procedure UpdateTreeTableList;
      procedure EnsureActiveDatabase;
  end;


type
  // Represents errors already "handled" (shown to user),
  // which can thus safely be ignored.
  THandledSQLError = class(Exception)
  end;


// -----------------------------------------------------------------------------
implementation


uses
  Main, createtable, fieldeditor, tbl_properties, tblcomment,
  optimizetables, copytable, sqlhelp, printlist,
  column_selection, data_sorting, runsqlfile, mysql;


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


// Check the tabletype of the selected table in the Popupmenu of ListTables
procedure TMDIChild.popupDbGridPopup(Sender: TObject);
var
  i                 : byte;
  SelectedEngine    : String;
  NodeData          : PVTreeData;
begin
  ValidateControls;

  if ListTables.SelectedCount <> 1 then
  begin
    Exit;
  end;

  for i := 0 to ListTables.Header.Columns.Count - 1 do
  begin
    if (ListTables.Header.Columns[i].Text = 'Engine') or (ListTables.Header.Columns[i].Text = 'Type') then
    begin
      NodeData := ListTables.GetNodeData(ListTables.FocusedNode);
      SelectedEngine := NodeData.Captions[i];
      Break;
    end;
  end;

  for i := 0 to ( MenuChangeType.Count - 1 ) do
  begin
    MenuChangeType.Items[i].Checked := ( MenuChangeType.Items[i].Caption = SelectedEngine );
  end;
end;


procedure TMDIChild.Init(AConn : POpenConnProf; AMysqlConn : TMysqlConn);
var
  AutoReconnect    : Boolean;
  menuitem         : TMenuItem;
  winName          : String;
  i, j             : Integer;
  treenode         : TTreeNode;
  ds               : TDataSet;
  miGroup,
  miFunction      : TMenuItem;
  functioncats     : TStringList;
begin
  QueryRunningInterlock := 0;
  UserQueryFired := False;
  CachedTableLists := TStringList.Create;

  FConn := AConn^;
  FMysqlConn := AMysqlConn; // we're now responsible to free it

  FConn.MysqlConn := FMysqlConn.Connection; // use this connection (instead of zConn)

  // Initialization: establish connection and read some vars from registry
  MainForm.Showstatus( 'Creating window...', 2, true );

  // Temporarily disable AutoReconnect in Registry
  // in case of unexpected application-termination
  AutoReconnect := false;
  with ( TRegistry.Create() ) do
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
    with ( TRegistry.Create() )  do
    begin
      OpenKey( REGPATH, true );
      WriteBool( 'AutoReconnect', true );
      CloseKey();
    end;
  end;

  // Set some defaults
  ActualDatabase := '';
  ActualTable := '';

  // Read engine-types for popupmenu in database tab
  if ( mysql_version >= 40102 ) then
  begin
    for i := ( MenuChangeType.Count - 1 ) downto 0 do
    begin
      MenuChangeType.Delete(i);
    end;
    ds := GetResults( 'SHOW ENGINES' );
    for i := 0 to ( ds.RecordCount - 1 ) do
    begin
      menuitem := TMenuItem.Create(self);
      menuitem.Caption := ds.FieldByName('Engine').AsString;
      menuitem.Hint := ds.FieldByName('Comment').AsString;
      if ( UpperCase( ds.FieldByName('Support').AsString ) = 'NO' ) then
      begin
        menuitem.Enabled := false;
        menuitem.Hint := menuitem.Hint + ' ('+STR_NOTSUPPORTED+')';
      end;
      menuitem.OnClick := MenuChangeTypeClick;
      MenuChangeType.Add( menuitem );
      ds.Next();
    end;
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
    for j := 0 to ( DBTree.Items.Count - 1 ) do
    begin
      treenode := DBTree.Items[j];
      if ( ( treenode.Level = 1 ) and ( treenode.Text = lastUsedDB ) ) then
      begin
        DBTree.Selected := treenode;
        Break;
      end;
    end;
  end;

  // Set the grid-cells to always-edit-mode.
  gridData.Options := gridData.Options + [dgAlwaysShowEditor];
  gridQuery.Options := gridQuery.Options + [dgAlwaysShowEditor];

  
  // read function-list into menu
  functioncats := GetFunctionCategories;

  for i:=0 to functioncats.Count-1 do
  begin
    // Create a menu item which gets subitems later
    miGroup := TMenuItem.Create(self);
    miGroup.Caption := functioncats[i];
    popupQuery.Items.add(miGroup);
    for j:=0 to Length(MySqlFunctions)-1 do
    begin
      if MySqlFunctions[j].Category <> functioncats[i] then
        continue;
      miFunction := TMenuItem.Create(self);
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
      miFunction.Hint := StringReplace( miFunction.Hint, '|', '�', [rfReplaceAll] );
      miGroup.Add(miFunction);
    end;
  end;

end;


procedure TMDIChild.ReadWindowOptions;
var
  ws          : String;
  i           : Integer;
  menuitem    : Tmenuitem;
begin
  with ( TRegistry.Create() ) do
  begin
    if OpenKey( REGPATH, true ) then
    begin
      ws := ReadString( 'childwinstate' );
      if ( ws = 'Normal' ) then
      begin
        WindowState := wsNormal;
        if ValueExists( 'childwinleft' ) then
        begin
          Left := ReadInteger( 'childwinleft' );
          Top := ReadInteger( 'childwintop' );
          Width := ReadInteger( 'childwinwidth' );
          Height := ReadInteger( 'childwinheight' );
        end;
      end
      else
      if ( ws = 'Minimized' ) then
      begin
        WindowState := wsMinimized
      end
      else
      if ( ws = 'Maximized' ) then
      begin
        WindowState := wsMaximized;
      end;

      // Other values:
      if ( ValueExists( 'querymemoheight' ) ) then
      begin
        pnlQueryMemo.Height := ReadInteger('querymemoheight');
      end;

      if ValueExists( 'queryhelperswidth' ) then
      begin
        pnlQueryHelpers.Width := ReadInteger('queryhelperswidth');
      end;

      if ( ValueExists( 'dbtreewidth' ) ) then
      begin
        DBtree.Width := ReadInteger( 'dbtreewidth' );
      end;

      if ( ValueExists( 'sqloutheight' ) ) then
      begin
        PageControlBottom.Height := ReadInteger( 'sqloutheight' );
      end;

      if ( ValueExists( 'DefaultColWidth' ) ) then
      begin
        Mainform.DefaultColWidth := ReadInteger( 'DefaultColWidth' );
      end
      else
      begin
        Mainform.DefaultColWidth := 100;
      end;

      // SQL-Font:
      if ( ( ValueExists( 'FontName' ) ) and ( ValueExists('FontSize') ) ) then
      begin
        SynMemoQuery.Font.Name := ReadString( 'FontName' );
        SynMemoSQLLog.Font.Name := ReadString( 'FontName' );
        SynMemoQuery.Font.Size := ReadInteger( 'FontSize' );
        SynMemoSQLLog.Font.Size := ReadInteger( 'FontSize' );
      end;

      // Data-Font:
      if ( ( ValueExists( 'DataFontName' ) ) and ( ValueExists( 'DataFontSize' ) ) ) then
      begin
        gridData.Font.Name := ReadString( 'DataFontName' );
        gridQuery.Font.Name := ReadString( 'DataFontName' );
        DBMemo1.Font.Name := ReadString( 'DataFontName' );
        gridData.Font.Size := ReadInteger( 'DataFontSize' );
        gridQuery.Font.Size := ReadInteger( 'DataFontSize' );
        DBMemo1.Font.Size := ReadInteger( 'DataFontSize' );
      end;

      // Color coding:
      if ( ValueExists( 'SQLColKeyAttri' ) ) then
      begin
        SynSQLSyn1.KeyAttri.Foreground := StringToColor( ReadString( 'SQLColKeyAttri' ) );
      end;

      if ( ValueExists( 'SQLColFunctionAttri' ) ) then
      begin
        SynSQLSyn1.FunctionAttri.Foreground := StringToColor( ReadString( 'SQLColFunctionAttri' ) );
      end;

      if ( ValueExists( 'SQLColDataTypeAttri' ) ) then
      begin
        SynSQLSyn1.DataTypeAttri.Foreground := StringToColor( ReadString( 'SQLColDataTypeAttri' ) );
      end;

      if ( ValueExists( 'SQLColNumberAttri' ) ) then
      begin
        SynSQLSyn1.NumberAttri.Foreground := StringToColor( ReadString( 'SQLColNumberAttri' ) );
      end;

      if ( ValueExists( 'SQLColStringAttri' ) ) then
      begin
        SynSQLSyn1.StringAttri.Foreground := StringToColor( ReadString( 'SQLColStringAttri' ) );
      end;

      if ( ValueExists( 'SQLColCommentAttri' ) ) then
      begin
        SynSQLSyn1.CommentAttri.Foreground := StringToColor( ReadString( 'SQLColCommentAttri' ) );
      end;

      if ( ValueExists( 'SQLColTablenameAttri' ) ) then
      begin
        SynSQLSyn1.TablenameAttri.Foreground := StringToColor( ReadString( 'SQLColTablenameAttri' ) );
      end;

      // SQLFiles-History
      FillPopupQueryLoad();

      // SQL-Filter-Files-History
      i := 1;
      popupFilterOpenFile.Items.Clear();
      while ( ValueExists( 'SQLWhereFile' + IntToStr(i) ) ) do
      begin
        menuitem := Tmenuitem.Create(Self);
        menuitem.Caption := IntToStr( popupFilterOpenFile.Items.count + 1) + ' ' + ReadString( 'SQLWhereFile' + IntToStr(i) );
        menuitem.OnClick := LoadSQLWhereFile;
        popupFilterOpenFile.Items.Add( menuitem );
        inc( i );
      end;

      // Synchronize internal variables with defaults from DFM.
      StopOnErrors := btnQueryStopOnErrors.Down;

      // Open server-specific registry-folder.
      // relative from already opened folder!
      OpenKey( 'Servers\' + FConn.Description, true );

      // Set last used database, select it later in Init
      lastUsedDB := ReadString( 'lastUsedDB' );
    end;
    CloseKey();
  end;
end;


procedure TMDIChild.FormClose(Sender: TObject; var Action: TCloseAction);
var
  ws    : String;
begin
  SetWindowConnected( false );
  SetWindowName( main.discname );
  Application.Title := APPNAME;

  // Closing connection and saving some vars into registry
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

  with ( TRegistry.Create() ) do
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

      // Open server-specific registry-folder.
      // relative from already opened folder!
      OpenKey( 'Servers\' + FConn.Description, true );
      WriteString( 'lastUsedDB', ActualDatabase );
    end;
  end;

  FormDeactivate( Sender );
  mainform.ToolBarData.Visible := false;
  Action := caFree;

  SetWindowConnected( false );
  SetWindowName( main.discname );
  Application.Title := APPNAME;
end;


procedure TMDIChild.LogSQL(msg: String = ''; comment: Boolean = true);
begin
  // Add a sql-command or info-line to history-memo
  while ( SynMemoSQLLog.Lines.Count > mainform.logsqlnum ) do
  begin
    SynMemoSQLLog.Lines.Delete(0);
  end;

  // Shorten very long messages
  if ( Length( msg ) > SQLLOG_CHAR_LIMIT ) then
  begin
    msg :=
      '/* Very large SQL query, showing first ' +
      FormatNumber( SQLLOG_CHAR_LIMIT ) +
      ' characters: */ ' +
      Copy( msg, 0, SQLLOG_CHAR_LIMIT ) +
      ' ...';
  end;
  msg := StringReplace( msg, #9, ' ', [rfReplaceAll] );
  msg := StringReplace( msg, #10, ' ', [rfReplaceAll] );
  msg := StringReplace( msg, #13, ' ', [rfReplaceAll] );
  msg := StringReplace( msg, '  ', ' ', [rfReplaceAll] );
  if ( comment ) then
  begin
    msg := '/* ' + msg + ' */';
  end;
  SynMemoSQLLog.Lines.Add( msg );
  SynMemoSQLLog.SetBookMark( 0, 0, SynMemoSQLLog.Lines.Count );
  SynMemoSQLLog.GotoBookMark( 0 );
  SynMemoSQLLog.ClearBookMark( 0 );
  SynMemoSQLLog.Repaint();
end;


procedure TMDIChild.ReadDatabasesAndTables(Sender: TObject);
var
  tnode          : TTreeNode;
  tmpSelected    : TTreeNode;
  i              : Integer;
  specialDbs     : TStringList;
  dbName         : String;
  ds             : TDataSet;
begin
  // Fill DBTree
  Screen.Cursor := crHourGlass;
  dataselected := false;
  DBTree.OnChange := nil;
  DBTree.items.Clear();
  ClearAllTableLists;

  tnodehost := DBtree.Items.Add( nil, FConn.MysqlParams.User + '@' + FConn.MysqlParams.Host );  // Host or Root
  tnodehost.ImageIndex := 41;
  tnodehost.SelectedIndex := 41;

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
      if ( dbName = DBNAME_INFORMATION_SCHEMA ) then
      begin
        specialDbs.Insert( 0, dbName )
      end
      (*
      else
      if ( dbName = DBNAME_MYSQL ) then
      begin
        specialDbs.Add( dbName );
      end
      *)
      else
      begin
        OnlyDBs2.Add( dbName );
      end;
      ds.Next();
    end;
    OnlyDBs2.Sort();
    // Prioritised position of system-databases
    for i := ( specialDbs.Count - 1 ) downto 0 do
    begin
      OnlyDBs2.Insert( 0, specialDbs[i] );
    end;
  end
  else
  begin
    OnlyDBs2 := OnlyDBs;
  end;

  // Avoids excessive InitializeKeywordLists() calls.
  SynSQLSyn1.TableNames.BeginUpdate();

  // Let synedit know all database names so that they can be highlighted
  // TODO: Is this right?  Adding "<db name>.<table name>" seems to make more sense..
  SynSQLSyn1.TableNames.AddStrings( OnlyDBs2 );
  SynSQLSyn1.TableNames.EndUpdate();

  // List Databases and Tables-Names
  tmpSelected := nil;
  for i := 0 to ( OnlyDBs2.Count - 1 ) do
  begin
    tnode := DBtree.Items.AddChild( tnodehost, OnlyDBs2[i] );
    tnode.ImageIndex := 37;
    tnode.SelectedIndex := 38;
    // Add dummy-node, will be replaced by real tables on expanding
    DBTree.Items.AddChild( tnode, DUMMY_NODE_TEXT );
    if ( ActualDatabase = OnlyDBs2[i] ) then
    begin
      tmpSelected := tnode;
    end;
  end;

  mainform.showstatus( IntToStr( OnlyDBs2.Count ) + ' Databases' );
  tnodehost.Expand( false );
  DBTree.OnChange := DBtreeChange;
  if ( tmpSelected <> nil ) then
  begin
    DBTree.Selected := tmpSelected;
  end
  else
  begin
    DBTree.Selected := tnodehost;
  end;
  DBtreeChange( Self, tnodehost );
  MainForm.ShowStatus( STATUS_MSG_READY, 2 );
  Screen.Cursor := crDefault;
end;


procedure TMDIChild.SelectHost;
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
  ActualDatabase := '';
  ActualTable := '';
end;


procedure TMDIChild.SelectDatabase(db: String);
begin
  if (not DBTree.Dragging) and (
   (PageControlMain.ActivePage = tabTable) or
   (PageControlMain.ActivePage = tabData)
  ) then PageControlMain.ActivePage := tabDatabase;

  tabDatabase.TabVisible := true;
  tabTable.TabVisible := false;
  tabData.TabVisible := false;

  pnlTableTop.Caption := 'Table-Properties';
  Caption := Description + ' - /' + ActualDatabase;
  ActualDatabase := db;
  ActualTable := '';
  try
    ShowDBProperties( Self );
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


procedure TMDIChild.SelectTable(db: String; table: String; switchView: boolean = true);
begin
  dataselected := false;
  ActualDatabase := db;
  ActualTable := table;
  if switchView then ShowTableProperties( Self );
  Caption := Description + ' - /' + ActualDatabase + '/' + ActualTable;
end;

// React on dbtree-clicks
procedure TMDIChild.DBtreeChange(Sender: TObject; Node: TTreeNode);
begin
  if ( Node = nil ) then
  begin
    raise Exception.Create( 'Internal badness: No host node in object tree.' );
  end;
  Screen.Cursor := crHourGlass;

  case ( Node.Level ) of
    0 :                                    // Root / Host chosen
    begin
      SelectHost();
    end;
    1 :                                    // DB chosen
    begin
      SelectDatabase( Node.Text );
    end;
    2 :                                    // Table chosen
    begin
      SelectTable( Node.Parent.Text, Node.Text );
    end;
  end;

  pcChange( Self );
  Screen.Cursor := crDefault;
end;


{***
  A database-node is about to be expanded:
  Drop the dummy-node and add all tables
}
procedure TMDIChild.DBtreeExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  i             : Integer;
  TableNames    : TStringList;
begin
  if ( ( Node.getFirstChild <> nil ) and ( Node.getFirstChild.Text = DUMMY_NODE_TEXT ) ) then
  begin
    // Drop dummynode
    for i := ( Node.Count - 1 ) downto 0 do
    begin
      Node.Item[i].Delete();
    end;

    // Get all tables into dbtree
    TableNames := GetCol( 'SHOW TABLES FROM ' + mask( Node.Text ) );
    for i := 0 to ( TableNames.Count - 1 ) do
    begin
      with ( DBTree.Items.AddChild( Node, TableNames[i] ) ) do
      begin
        ImageIndex := 39;
        SelectedIndex := 40;
      end;
    end;

    // Add tables to syntax-highlighter
    AddUniqueItemsToList( TableNames, SynSQLSyn1.TableNames );
  end;
end;


procedure TMDIChild.viewdata(Sender: TObject);
var
  sorting              : String;
  DropDown             : TStringList;
  i                    : Integer;
  j                    : Integer;
  ValidColumns         : TStringList;
  PrimaryKeyColumns    : TStringList;
  reg                  : TRegistry;
  reg_value            : String;
  orderclauses         : TStringList;
  columnname           : String;
  columnexists         : Boolean;
  select_base          : String;
  limit                : Int64;
  ds                   : TDataSet;
  conn                 : TOpenConnProf;
  sl_query             : TStringList;
  manualLimit          : boolean;
  manualLimitEnd       : integer;
  DisplayedColumnsList : TStringList;
  tmp                  : TDataSet;
  RewriteOrderClause   : Boolean;
begin
  viewingdata := true;
  try
    sl_query := TStringList.Create();

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
      end;

      // limit number of rows fetched according to preferences
      if manualLimit then begin
        // manual limit set in preferences
        limit := manualLimitEnd;
      end else begin
        // no tick in preferences check box - auto-limit:
        // limit number of rows fetched if more than ~ 5 MB of data
        limit := GetCalculatedLimit( ActualTable );
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

    reg := TRegistry.Create();
    reg.OpenKey( REGPATH + '\Servers\' + FConn.Description, true );

    if ( not dataselected ) then
    begin
      SynMemoFilter.Text := '';
      // Read cached WHERE-clause and set filter
      reg_value := 'WHERECLAUSE_' + ActualDatabase + '.' + ActualTable;
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
    reg_value := 'ORDERCLAUSE_' + ActualDatabase + '.' + ActualTable;
    sorting := '';
    gridData.SortColumns.Clear();
    if ( reg.ValueExists( reg_value ) ) then
    begin
      orderclauses := explode( ',', reg.ReadString( reg_value ) );
      RewriteOrderClause := False;
      ValidColumns := GetVTCaptions( ListColumns );
      for i := 0 to ( orderclauses.Count - 1 ) do
      begin
        columnname := Trim( Copy( orderclauses[i], 0, LastPos( ' ', orderclauses[i] ) ) );
        columnname := trimc( columnname, '`' );
        columnexists := ValidColumns.IndexOf(columnname) > -1;

        if ( not columnexists ) then
        begin
          LogSQL( 'Notice: A stored ORDER-BY clause could not be applied, '+
            'because the column "' + columnname + '" does not exist!');
          RewriteOrderClause := True;
          Continue;
        end;

        // Add column to order clause
        if sorting <> '' then
          sorting := sorting + ', ';
        sorting := sorting + orderclauses[i];

        with ( gridData.SortColumns.Add() ) do
        begin
          Fieldname := columnname;
          if ( Copy( orderclauses[i], ( Length( orderclauses[i] ) - 3 ), 4 ) = 'DESC' ) then
          begin
            SortType := stAscending;
          end
          else
          begin
            SortType := stDescending;
          end;
        end;
      end;
      // Old orderclause contained a no longer existing column, so overwrite it
      if RewriteOrderClause then
      begin
        reg.WriteString( reg_value, sorting );
      end;
    end;

    if ( sorting <> '' ) then
    begin
      sorting := 'ORDER BY ' + sorting;
      // Signal for the user that we applied an ORDER-clause
      btnDataSorting.Font.Color := clRed;
    end
    else
    begin
      btnDataSorting.Font.Color := clWindowText;
    end;

    MenuLimit.Checked := Mainform.CheckBoxLimit.Checked;
    PrimaryKeyColumns := TStringList.Create();

    if ( ( ActualTable <> '' ) and ( ActualDatabase <> '' ) ) then
    begin
      // Ensure <Table> and <Data> are visible
      tabTable.TabVisible := true;
      tabData.TabVisible := true;
      // Switch to <Data>
      PageControlMain.ActivePage := tabData;

      MainForm.ShowStatus( 'Retrieving data...', 2, true );

      // Read columns to display from registry
      with( TRegistry.Create ) do
      begin
        OpenKey( REGPATH + '\Servers\' + FConn.Description, true );
        DisplayedColumnsList := explode( '`', ReadString(REGNAME_DISPLAYEDCOLUMNS + '_' + ActualDatabase + '.' + ActualTable));
      end;

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
      select_base := select_base + ' FROM ' + mask( ActualTable );
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
        conn := FConn;
        conn.MysqlParams.Database := ActualDatabase;

        // Avoid excessive GUI updates.
        if ( DataSource1.DataSet <> nil ) then
        begin
          DataSource1.DataSet.DisableControls();
        end;

        // free previous resultset
        tmp := DataSource1.DataSet;
        DataSource1.DataSet := nil;
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
          // Put the user with his nose onto the wrong filter he specified
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

      for i := 0 to Length(VTRowDataListColumns) - 1 do
      begin

        // give all enum-fields a PickList with its Items
        if ( StrCmpBegin( 'enum', VTRowDataListColumns[i].Captions[1]) ) then
        begin
          DropDown := explode( ''',''', getEnumValues( VTRowDataListColumns[i].Captions[1] ) );
          for j := 0 to ( DropDown.Count - 1 ) do
          begin
            DropDown[j] := trimc( DropDown[j], '''' );
          end;

          for j := 0 to ( gridData.Columns.Count - 1 ) do
          begin
            if ( gridData.Columns[j].FieldName = VTRowDataListColumns[i].Captions[0] ) then
            begin
              gridData.Columns[j].PickList := DropDown;
            end;
          end;
        end;

        // make PK-columns = fsBold
        for j := 0 to ( gridData.Columns.Count - 1 ) do
        begin
          if (
            ( gridData.Columns[j].FieldName = VTRowDataListColumns[i].Captions[0] ) and
            ( VTRowDataListColumns[i].ImageIndex = 26 )
          ) then
          begin
            PrimaryKeyColumns.Add( VTRowDataListColumns[i].Captions[0] );
          end;
        end;
      end;

      for j := 0 to ( gridData.Columns.Count - 1 ) do
      begin
        // for letting NULLs being inserted into "NOT NULL" fields
        // in mysql5+, the server rejects inserts with NULLs in NOT NULL-fields,
        // so the Required-check on client-side is not needed at any time
        ds.Fields[j].Required := false;

        // set column-width
        if (
          (Mainform.DefaultColWidth <> 0) and
          (gridData.Columns[j].Width > Mainform.DefaultColWidth)
        ) then
        begin
          gridData.Columns[j].Width := Mainform.DefaultColWidth;
        end;

        // make PK-columns = fsBold
        for i := 0 to ( PrimaryKeyColumns.Count - 1 ) do
        begin
          if (PrimaryKeyColumns[i] = gridData.Columns[j].Fieldname) then
          begin
            gridData.Columns[j].Font.Style := gridData.Columns[j].Font.Style + [fsBold];
            gridData.Columns[j].Color := $02EEEEEE;
          end;
        end;
      end;

      DisplayRowCountStats(ds);
      dataselected := true;
      viewingdata := false;
      ds.EnableControls();
      pcChange(self);
    end;

    Screen.Cursor := crDefault;
    FreeAndNil (sl_query);
  finally
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
    mask( ActualTable ), 0 ) );

  lblDataTop.Caption := ActualDatabase + '.' + ActualTable + ': ' +
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


procedure TMDIChild.WaitForQueryCompletion(WaitForm: TForm);
begin
  debug( 'Waiting for query to complete.' );
  WaitForm.ShowModal();
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
    if ActualDatabase <> '' then
      pnlQueryTop.Caption := 'SQL-Query on Database ' + ActualDatabase + ':'
    else
      pnlQueryTop.Caption := 'SQL-Query on Host ' + FConn.MysqlParams.Host + ':';
    // Manually invoke OnChange event of tabset to fill helper list with data
    tabsetQueryHelpers.OnChange( Sender, tabsetQueryHelpers.TabIndex, dummy);
  end;

  // Move focus to relevant controls in order for them to receive keyboard events.
  if PageControlMain.ActivePage = tabDatabase then ListTables.SetFocus;
  if PageControlMain.ActivePage = tabTable then ListColumns.SetFocus;
  if PageControlMain.ActivePage = tabData then gridData.SetFocus;

  // Ensure controls are in a valid state
  ValidateControls;

  // Show processlist if it's visible now but empty yet
  if ListProcesses.RootNodeCount = 0 then
    ShowProcessList( self );
end;


{***
  Ensures that we're connected to the currently selected database.
}
procedure TMDIChild.EnsureActiveDatabase;
begin
  // Blank = current database undefined
  if ActualDatabase = '' then Exit;
  if (FMysqlConn.Connection.Database <> ActualDatabase) or UserQueryFired then begin
    ExecUseQuery(ActualDatabase, false, false);
    UserQueryFired := false;
    FMysqlConn.Connection.Database := ActualDatabase;
  end;
end;


{***
  Look for list of tables for current database in cache.
  Retrieve from server if necessary.
  @return TDataSet The cached list of tables for the active database.
}
function TMDIChild.FetchActiveDbTableList: TDataSet;
var
  ds: TDataSet;
  OldCursor: TCursor;
begin
  if CachedTableLists.IndexOf(ActualDatabase) = -1 then begin
    // Not in cache, load table list.
    OldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    MainForm.ShowStatus('Displaying tables from ' + ActualDatabase + '...', 2, true);
    if mysql_version >= 32300 then begin
      ds := GetResults('SHOW TABLE STATUS', false, false);
    end else begin
      // contains table names, nothing else.
      ds := GetResults('SHOW TABLES', false, false);
      // could clean up data (rename first column to 'Name') and
      // and add row counters to data set as a new field by using
      // SELECT COUNT(*), but that would potentially be rather slow.
    end;
    CachedTableLists.AddObject(ActualDatabase, ds);
    Screen.Cursor := OldCursor;
  end;
  Result := TDataSet(CachedTableLists.Objects[CachedTableLists.IndexOf(ActualDatabase)]);
  Result.First;
end;


{***
  Nukes cached table list for active database, then refreshes it.
  @return TDataSet The newly cached list of tables for the active database.
}
function TMDIChild.RefreshActiveDbTableList: TDataSet;
var
  idx: Integer;
  o: TObject;
begin
  idx := CachedTableLists.IndexOf(ActualDatabase);
  if idx > -1 then begin
    o := CachedTableLists.Objects[idx];
    FreeAndNil(o);
    CachedTableLists.Delete(idx);
  end;
  Result := FetchActiveDbTableList;
end;


{***
  Nukes the table list cache.
}
procedure TMDIChild.ClearAllTableLists;
var
  idx: Integer;
  o: TObject;
begin
  for idx := 0 to CachedTableLists.Count - 1 do begin
    o := CachedTableLists.Objects[idx];
    FreeAndNil(o);
  end;
  CachedTableLists.Clear;
end;


{***
  Updates tree with table list for currently selected database (or table).
}
procedure TMDIChild.UpdateTreeTableList;
var
  tndb : TTreenode;
  ds: TDataSet;
  t, u: Integer;
begin
  if DBTree.Selected.Level = 1 then tndb := DBTree.Selected
  else if DBTree.Selected.Level = 2 then tndb := DBTree.Selected.Parent
  else exit;

  // get all tables back into dbtree
  for u:=tndb.Count-1 downto 0 do tndb.Item[u].delete;
  ds := FetchActiveDbTableList;
  for t:=0 to ds.RecordCount-1 do
  begin
    with DBtree.Items.AddChild(tndb, ds.Fields[0].AsString) do
    begin
      ImageIndex := 39;
      selectedIndex := 40;
    end;
    ds.Next;
  end;
end;


{ Show tables and their properties on the tabsheet "Database" }
procedure TMDIChild.ShowDBProperties(Sender: TObject);
var
  i,j,k           : Integer;
  bytes           : Extended;
  menuitem        : TMenuItem;
  TablelistColumns: TStringList;
  column          : TVirtualTreeColumn;
  ds              : TDataSet;
  OldSortColumn   : Integer;
  OldSortDirection: TSortDirection;
begin
  // DB-Properties
  Screen.Cursor := crHourGlass;
  MainForm.ShowStatus( 'Reading from database ' + ActualDatabase + '...', 2, true );
  Mainform.ButtonDropDatabase.Hint := 'Drop Database...|Drop Database ' + ActualDatabase + '...';

  try
    // Refresh chosen in table list?
    if sender = menurefresh then RefreshActiveDbTableList;

    // Populate database subitems (= tables) in tree.
    UpdateTreeTableList;

    ds := FetchActiveDbTableList;

    // Generate items for popupDbGridHeader
    for i:=popupDbGridHeader.Items.Count-1 downto 2 do
      popupDbGridHeader.Items.Delete( i );
    with TRegistry.Create do
    begin
      openkey( REGPATH + '\Servers\' + FConn.Description, true );
      if ValueExists( 'TablelistDefaultColumns' ) then
        popupDbGridHeader.Items[0].Checked := ReadBool( 'TablelistDefaultColumns' );
      if ValueExists( 'TablelistColumns' ) then
        TablelistColumns := Explode( ',', ReadString( 'TablelistColumns' ) )
      else
        TablelistColumns := TStringList.Create;
      free;
    end;
    for i:=0 to ds.FieldCount-1 do
    begin
      menuitem := TMenuItem.Create( self );
      menuitem.Caption := ds.Fields[i].Fieldname;
      menuitem.Tag := 2;
      menuitem.OnClick := MenuTablelistColumnsClick;
      if i=0 then
      begin
        menuitem.Enabled := false; // tablename should always be kept
        menuitem.Checked := true;
      end
      else if TablelistColumns.Count > 0 then
      begin
        menuitem.Checked := TablelistColumns.IndexOf( menuitem.Caption ) > -1;
      end;
      popupDbGridHeader.Items.Add( menuitem );
    end;

    // Remember sorting options and restore them later
    OldSortColumn := ListTables.Header.SortColumn;
    OldSortDirection := ListTables.Header.SortDirection;

    ListTables.BeginUpdate;
    ListTables.Clear;
    ListTables.Header.Columns.BeginUpdate;
    ListTables.Header.Columns.Clear;
    column := ListTables.Header.Columns.Add;
    column.Text := 'Table';
    column.Width := 120;
    if popupDbGridHeader.Items[0].Checked then
    begin // Default columns - initialize column headers
      column := ListTables.Header.Columns.Add;
      column.Text := 'Records';
      column.Alignment := taRightJustify;
      column.Width := 80;

      column := ListTables.Header.Columns.Add;
      column.Text := 'Size';
      column.Alignment := taRightJustify;
      column.Width := 70;

      column := ListTables.Header.Columns.Add;
      column.Text := 'Created';
      column.Width := 120;

      column := ListTables.Header.Columns.Add;
      column.Text := 'Updated';
      column.Width := 120;

      column := ListTables.Header.Columns.Add;
      column.Text := 'Engine';
      column.Width := 70;

      column := ListTables.Header.Columns.Add;
      column.Text := 'Comment';
      column.Width := 50;
    end;
    for i:=0 to TablelistColumns.Count-1 do
    begin
      column := ListTables.Header.Columns.Add;
      column.Text := TablelistColumns[i];
      column.Width := 50;
      column.MinWidth := 50;
    end;
    // Ensure AutoResize does not cause an AV
    ListTables.Header.SortColumn := OldSortColumn;
    ListTables.Header.SortDirection := OldSortDirection;
    ListTables.Header.Columns.EndUpdate;

    SetLength(VTRowDataListTables, ds.RecordCount);
    for i := 1 to ds.RecordCount do
    begin
      VTRowDataListTables[i-1].Captions := TStringList.Create;
      VTRowDataListTables[i-1].ImageIndex := 39;
      // Table
      VTRowDataListTables[i-1].Captions.Add( ds.Fields[0].AsString );
      if (mysql_version >= 32300) and popupDbGridHeader.Items[0].Checked then
      begin // Default columns
        // Records
        VTRowDataListTables[i-1].Captions.Add( FormatNumber( ds.FieldByName('Rows').AsFloat ) );
        // Size: Data_length + Index_length
        bytes := ds.FieldByName('Data_length').AsFloat + ds.FieldByName('Index_length').AsFloat;
        VTRowDataListTables[i-1].Captions.Add( FormatNumber( bytes / 1024 + 1 ) + ' KB');
        // Created:
        if not ds.FieldByName('Create_time').IsNull then
          VTRowDataListTables[i-1].Captions.Add( ds.FieldByName('Create_time').AsString )
        else
          VTRowDataListTables[i-1].Captions.Add('N/A');

        // Updated:
        if not ds.FieldByName('Update_time').IsNull then
          VTRowDataListTables[i-1].Captions.Add( ds.FieldByName('Update_time').AsString )
        else
          VTRowDataListTables[i-1].Captions.Add('N/A');

        // Type
        if ds.FindField('Type')<>nil then
          VTRowDataListTables[i-1].Captions.Add( ds.FieldByName('Type').AsString )
        else if ds.FindField('Engine')<>nil then
          VTRowDataListTables[i-1].Captions.Add( ds.FieldByName('Engine').AsString )
        else
          VTRowDataListTables[i-1].Captions.Add('');

        // Comment
        VTRowDataListTables[i-1].Captions.Add( ds.FieldByName('Comment').AsString );
      end;
      for j:=0 to TablelistColumns.Count-1 do
      begin
        for k:=0 to ds.FieldCount-1 do
        begin
          if TablelistColumns[j] = ds.Fields[k].FieldName then
          begin
            if ds.Fields[k].DataType in [ftInteger, ftSmallint, ftWord, ftFloat, ftWord ] then
            begin
              // Number
              // TODO: doesn't match any column
              ListTables.Header.Columns[ListTables.Header.Columns.Count-1].Alignment := taRightJustify;
              VTRowDataListTables[i-1].Captions.Add( FormatNumber( ds.Fields[k].AsFloat ) );
            end
            else
              // String
              VTRowDataListTables[i-1].Captions.Add( ds.Fields[k].AsString );
            break;
          end;
        end;
      end;
      ds.Next;
    end;
    mainform.showstatus(ActualDatabase + ': ' + IntToStr(ds.RecordCount) +' table(s)');
  finally
    ListTables.RootNodeCount := Length(VTRowDataListTables);
    ListTables.EndUpdate;
    Screen.Cursor := crDefault;
  end;
  Screen.Cursor := crHourglass;

  pnlDatabaseTop.Caption := 'Database ' + ActualDatabase + ': ' + IntToStr(ListTables.RootNodeCount) + ' table(s)';
  MainForm.ShowStatus( STATUS_MSG_READY, 2 );
  Screen.Cursor := crDefault;
end;



{ Show columns of selected table, indicate indexed columns by certain icons }
procedure TMDIChild.ShowTableProperties(Sender: TObject);
var
  i,j : Integer;
  tn, tndb : TTreeNode;
  isFulltext : Boolean;
  ds : TDataSet;
  dummy: Boolean;
begin
  // Table-Properties

  Screen.Cursor := crHourGlass;

  if (not DBTree.Dragging) and (
   (PageControlMain.ActivePage = tabHost) or
   (PageControlMain.ActivePage = tabDatabase)
  ) then PageControlMain.ActivePage := tabTable;

  tabDatabase.TabVisible := true;
  tabTable.TabVisible := true;
  tabData.TabVisible := true;

  pnlTableTop.Caption := 'Table-Properties for ' + ActualDatabase + ': ' + ActualTable;

  // set current node in DBTree to ActualTable:
  with DBTree do begin
    if Selected.Level = 1 then tndb := Selected
    else if Selected.Level = 2 then tndb := Selected.Parent
    else if Selected.Level = 3 then tndb := Selected.Parent.Parent
    else exit;
  end;
  tn := tndb.getFirstChild;
  for i:=0 to tndb.Count -1 do
  begin
    if ActualTable = tn.Text then
    begin
      DBTree.Selected := tn; // select table
      break;
    end;
    tn := tndb.GetNextChild(tn);
  end;

  MainForm.ShowStatus( 'Reading table properties...', 2, true );
  ListColumns.BeginUpdate;
  ListColumns.Clear;
  Try
    ds := GetResults( 'SHOW COLUMNS FROM ' + mask(ActualTable), false );

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
      ds.Next;
    end;

    ListColumns.RootNodeCount := Length(VTRowDataListColumns);

    // Manually invoke OnChange event of tabset to fill helper list with data
    if tabsetQueryHelpers.TabIndex = 0 then
      tabsetQueryHelpers.OnChange( Sender, tabsetQueryHelpers.TabIndex, dummy);

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

    Screen.Cursor := crHourglass;
    ds := GetResults( 'SHOW KEYS FROM ' + mask(ActualTable) );
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

  MainForm.ShowStatus( STATUS_MSG_READY, 2, false );
  MainForm.showstatus(ActualDatabase + ': '+ ActualTable + ': ' + IntToStr(ListColumns.RootNodeCount) +' field(s)');
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
  Used to change the iconindex of selected items and to trigger ValidateControls
}
procedure TMDIChild.ListTablesClick(Sender: TObject);
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
  btnDbInsertRecord.Enabled := tableSelected;
  menuinsert.Enabled := tableSelected;
  btnDbViewData.Enabled := tableSelected;
  menuviewdata.Enabled := tableSelected;
  btnDbEmptyTable.Enabled := tableSelected;
  menuemptytable.Enabled := tableSelected;
  MenuAdvancedProperties.Enabled := tableSelected;
  MenuRenameTable.Enabled := tableSelected;
  MenuChangeType1.Enabled := tableSelected;
  MenuChangeType2.Enabled := tableSelected;
  MenuChangeType3.Enabled := tableSelected;
  MenuChangeType4.Enabled := tableSelected;
  MenuChangeType5.Enabled := tableSelected;
  MenuChangeTypeOther.Enabled := tableSelected;
  Mainform.CopyTable.Enabled := tableSelected;
  MenuTableComment.Enabled := tableSelected;
  MenuOptimize.Enabled := tableSelected;
  MenuCheck.Enabled := tableSelected;
  MenuAnalyze.Enabled := tableSelected;
  MenuRepair.Enabled := tableSelected;

  MainForm.ButtonDropDatabase.Enabled := (ActualDatabase <> '') and FrmIsFocussed;
  MainForm.DropTable.Enabled := tableSelected or ((PageControlMain.ActivePage <> tabDatabase) and (ActualTable <> '') and FrmIsFocussed);
  MainForm.ButtonCreateTable.Enabled := (ActualDatabase <> '') and FrmIsFocussed;
  MainForm.ButtonImportTextFile.Enabled := (mysql_version >= 32206) and FrmIsFocussed;
  MainForm.MenuImportTextFile.Enabled := MainForm.ButtonImportTextFile.Enabled;

  with MainForm do
  begin
    ButtonRefresh.Enabled := FrmIsFocussed;
    ButtonReload.Enabled := FrmIsFocussed;
    ExportTables.Enabled := FrmIsFocussed;
    ButtonImportTextfile.Enabled := FrmIsFocussed;
    ButtonCreateTable.Enabled := FrmIsFocussed;
    ButtonCreateDatabase.Enabled := FrmIsFocussed;
    ButtonDropDatabase.Enabled := false;
    MenuRefresh.Enabled := FrmIsFocussed;
    MenuExport.Enabled := FrmIsFocussed;
    MenuImportTextFile.Enabled := FrmIsFocussed;
    MenuCreateTable.Enabled := FrmIsFocussed;
    MenuCreateDatabase.Enabled := FrmIsFocussed;
    MenuDropDatabase.Enabled := FrmIsFocussed;
    DropTable.Enabled := FrmIsFocussed;
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
    {***
      @note ansgarbecker, 2007-31-03
        1. For data-tab-queries (threaded queries) the TDatasource does *not* have
        an linked TDataset/TZquery in case of a SQL-error. To avoid an AV with
        NIL-reference we have to first check if the Dataset is existant - if not,
        assume the datagrid is empty.
        2. For query-tab-queries (not threaded) the TZQuery is linked
        to the TDatasource and we can safely ask for TDataset.IsEmpty without
        causing a NIL-reference.
      @todo use the same threaded process for both datagrid and query-grid
    }
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
  SelectTable(ActualDatabase, table, false);
  PageControlMain.ActivePage := tabData;
  viewdata(self);
end;


procedure TMDIChild.ShowTable(Sender: TObject);
var
  i : Integer;
  tn, tndb : TTreeNode;
  NodeData: PVTreeData;
begin
  if DBTree.Selected.Level = 1 then tndb := DBTree.Selected
  else if DBTree.Selected.Level = 2 then tndb := DBTree.Selected.Parent
  else exit;

  tn := tndb.getFirstChild;
  for i:=0 to tndb.Count -1 do begin
    NodeData := ListTables.GetNodeData(ListTables.FocusedNode);
    if NodeData.Captions[0] = tn.Text then
    begin
      DBTree.Selected := tn;
      ShowTableData(tn.Text);
      break;
    end;
    tn := tndb.GetNextChild(tn);
  end;
end;


procedure TMDIChild.EmptyTable(Sender: TObject);
var
  t : TStringList;
  i : Integer;
begin
  // Empty Table(s)
  if ListTables.SelectedCount = 0 then
    exit;

  // Add selected items/tables to helper list
  t := GetVTCaptions(ListTables, True);

  if MessageDlg('Empty ' + IntToStr(t.count) + ' Table(s) ?' + crlf + '(' + implodestr(', ', t) + ')', mtConfirmation, [mbok,mbcancel], 0) <> mrok then
    exit;

  Screen.Cursor := crSQLWait;
  for i:=0 to t.count-1 do
    ExecUpdateQuery( 'DELETE FROM ' + mask(t[i]) );
  t.Free;
  ShowDBProperties(self);
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
    3 : tndb_ := DBTree.Selected.Parent.Parent;
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
        end;
      end;
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
      if CompareText( ds.Fields[4].AsString, 'Killed') = 0 then
        VTRowDataListProcesses[i-1].ImageIndex := 83  // killed
      else
        VTRowDataListProcesses[i-1].ImageIndex := 82; // running
      for j := 1 to 7 do
        VTRowDataListProcesses[i-1].Captions.Add(ds.Fields[j].AsString);
      ds.Next;
    end;
    ds.Close;
    tabProcessList.Caption := 'Process-List (' + IntToStr(Length(VTRowDataListProcesses)) + ')';
  except
    on E: Exception do begin
      LogSQL('Error loading process list (automatic refresh disabled): ' + e.Message);
      TimerProcesslist.Enabled := false;
    end;
  end;
  ListProcesses.RootNodeCount := Length(VTRowDataListProcesses);
  ListProcesses.EndUpdate;
  Screen.Cursor := crDefault;
end;

procedure TMDIChild.KillProcess(Sender: TObject);
var t : Boolean;
  NodeData : PVTreeData;
begin
  NodeData := ListProcesses.GetNodeData(ListProcesses.FocusedNode);
  if NodeData.Captions[0] = IntToStr( MySQLConn.Connection.GetThreadId ) then
    MessageDlg('Fatal: Better not kill my own Process...', mtError, [mbok], 0)
  else begin
    t := TimerProcessList.Enabled;
    TimerProcessList.Enabled := false; // prevent av (ListProcesses.selected...)
    if MessageDlg('Kill Process '+NodeData.Captions[0]+'?', mtConfirmation, [mbok,mbcancel], 0) = mrok then
    begin
      ExecUpdateQuery( 'KILL '+NodeData.Captions[0] );
      ShowVariablesAndProcesses(self);
    end;
    TimerProcessList.Enabled := t; // re-enable autorefresh timer
  end;
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
  term              : String;
  prevDb            : String;
begin
  if ( btnAltTerminator.Down ) then
  begin
    term := '//';
  end
  else
  begin
    term := ';';
  end;

  if ( CurrentLine ) then
  begin
    // Run current line
    SQL := parseSQL( SynMemoQuery.LineText, term );
  end
  else
  if ( Selection ) then
  begin
    // Run selection
    SQL := parseSQL( SynMemoQuery.SelText, term );
  end
  else
  begin
    // Run all
    SQL := parseSQL( SynMemoQuery.Text, term );
  end;

  if ( SQL.Count = 0 ) then
  begin
    LabelResultinfo.Caption := '(nothing to do)';
    Exit;
  end;

  // Let EnsureActiveDatabase know that we're firing user queries.
  prevDb := ActualDatabase;
  ActualDatabase := EmptyStr;
  UserQueryFired := true;

  // Destroy old data set.
  ds := DataSource2.DataSet;
  DataSource2.DataSet := nil;
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

    if ( ActualDatabase <> EmptyStr ) then
    begin
      FMysqlConn.Connection.Database := ActualDatabase;
    end;

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
        if ( ExpectResultSet( SQL[i] ) ) then
        begin
          ds := GetResults( SQL[i], false, false );
          gridQuery.DataSource.DataSet := ds;
          if ( ds <> nil ) then
          begin
            fieldcount := ds.Fieldcount;
            recordcount := ds.Recordcount;
          end
          else
          begin
            fieldcount := 0;
            recordcount := 0;
          end;
          rowsaffected := rowsaffected + TZQuery(ds).RowsAffected;
        end
        else
        begin
          rowsaffected := rowsaffected + ExecUpdateQuery( SQL[i], false, false );
          fieldcount := 0;
          recordcount := 0;
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
      if ( Mainform.DefaultColWidth <> 0 ) then
      begin
        for i:=0 to gridQuery.Columns.count-1 do
        begin
          if ( gridQuery.Columns[i].Width > Mainform.DefaultColWidth ) then
          begin
            gridQuery.Columns[i].Width := Mainform.DefaultColWidth;
          end;
        end;
      end;
    end
    else
    begin
      // Avoid AV for mousewheel-scrolling in empty datagrid
      DataSource2.DataSet := nil;
    end;
    // Ensure controls are in a valid state
    ValidateControls();
    viewingdata := false;
    ActualDatabase := prevDb;
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
    if MessageDlg('Can''t drop all or the last Field - drop Table '+ActualTable+'?', mtConfirmation, [mbok,mbcancel], 0) = mrok then
    begin
      Screen.Cursor := crSQLWait;
      ExecUpdateQuery( 'DROP TABLE '+mask(ActualTable) );
      tn := DBTree.Selected;
      DBTree.Selected := DBTree.Selected.Parent;
      tn.Destroy;
      try
        RefreshActiveDbTableList;
      except
      end;
      ShowDBProperties(self);
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
    ExecUpdateQuery( 'ALTER TABLE '+mask(ActualTable)+' ' + dropCmd );

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
  var Value: String; Shift: TShiftState; Index: Integer; EndToken: Char);
begin
  SynCompletionProposal1.Editor.UndoList.AddGroupBreak;
end;


procedure TMDIChild.SynCompletionProposal1AfterCodeCompletion(Sender: TObject;
  const Value: String; Shift: TShiftState; Index: Integer; EndToken: Char);
begin
  SynCompletionProposal1.Editor.UndoList.AddGroupBreak;
end;


{ Proposal-Combobox pops up }
procedure TMDIChild.SynCompletionProposal1Execute(Kind: TSynCompletionType;
  Sender: TObject; var CurrentInput: String; var x, y: Integer;
  var CanExecute: Boolean);
var
  i,j,c,t          : Integer;
  tn, child        : TTreeNode;
  sql, tmpsql, kw  : String;
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
    dbname := ActualDatabase;
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
          child := tn.getFirstChild;
          for j:=0 to tn.Count-1 do
          begin
            addTable(child.Text);
            child := tn.getNextChild(child);
          end;
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

    if ActualDatabase <> '' then
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
begin
  if Sender = PopupMenuCreateTable then begin
    if DBTree.Selected.Level = 2 then CreateTableWindow(Self, DBtree.Selected.Parent.Text);
    if DBTree.Selected.Level = 1 then CreateTableWindow(Self, DBtree.Selected.Text);
  end else CreateTableWindow(Self);
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
var dbname : String;
begin
  // Create new Database:
  if InputQuery('Create new Database...', 'Database Name:', dbname) then
  begin
    Screen.Cursor := crSQLWait;
    Try
      ExecUpdateQuery( 'CREATE DATABASE ' + mask( dbname ) );
      // Add DB to OnlyDBs-regkey if this is not empty
      if OnlyDBs.Count > 0 then
      begin
        OnlyDBs.Add( dbname );
        with TRegistry.Create do
        begin
          if OpenKey(REGPATH + '\Servers\' + FConn.Description, false) then
          begin
            WriteString( 'OnlyDBs', ImplodeStr( ';', OnlyDBs ) );
            CloseKey;
          end;
        end;
      end;
      ActualDatabase := dbname;
      ReadDatabasesAndTables(self);
    except
      MessageDLG('Creation failed.'+crlf+'Maybe '''+dbname+''' is not a valid database-name.', mtError, [mbOK], 0)
    end;
    Screen.Cursor := crDefault;
  end;
end;


procedure TMDIChild.MenuAdvancedPropertiesClick(Sender: TObject);
begin
  tbl_properties_Window(self);
end;


{***
  Rename table after checking the new name for invalid characters
}
procedure TMDIChild.ListTablesNewText(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; NewText: WideString);
var
  i : Integer;
  NodeData : PVTreeData;
begin
  // Fetch data from node
  NodeData := Sender.GetNodeData(Node);

  // Try to rename, on any error abort and don't rename ListItem
  try
    ensureValidIdentifier( NewText );
    // rename table
    ExecUpdateQuery( 'ALTER TABLE ' + mask(NodeData.Captions[0]) + ' RENAME ' + mask(NewText), False, False );

    i := SynSQLSyn1.TableNames.IndexOf( NodeData.Captions[0] );
    if i > -1 then
      SynSQLSyn1.TableNames[i] := NewText;
    // Update nodedata
    NodeData.Captions[0] := NewText;
    ActualTable := NewText;
    RefreshActiveDbTableList;
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


procedure TMDIChild.Clear1Click(Sender: TObject);
begin
  // clear
  if SynMemoFilter.Focused then
    SynMemoFilter.Lines.Clear
  else
    SynMemoQuery.Lines.Clear;
end;


procedure TMDIChild.MenuTableCommentClick(Sender: TObject);
begin
  // table-comment
  tablecommentWindow(self);
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
    SelectTable(ActualDatabase, NodeData.Captions[0]);
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


procedure TMDIChild.gridDataTitleClick(Column: TColumn);
var
  Grid : TSMDBGrid;
  i  : Integer;
  existed : Boolean;
  sortcol : TSMSortColumn;
  sorting, reg_value : String;
  reg : TRegistry;
begin
  // column-title clicked -> generate "ORDER BY"

  Grid := Column.Grid as TSMDBGrid;
  Grid.DataSource.DataSet.DisableControls;
  existed := false;

  for i:=Grid.SortColumns.Count-1 downto 0 do
  begin
    sortcol := Grid.SortColumns[i];
    if sortcol.FieldName <> Column.FieldName then
      continue;
    existed := true;
    case sortcol.SortType of
      stDescending : sortcol.SortType := stAscending;
      stAscending : Grid.SortColumns.Delete(i);
      stNone : sortcol.SortType := stDescending;
    end;
  end;

  {add a new sorted column in list - ascending order}
  if not existed then
  begin
    sortcol := Grid.SortColumns.Add;
    sortcol.FieldName := column.FieldName;
    sortcol.SortType := stDescending;
  end;

  Grid.DataSource.DataSet.EnableControls;

  // Concat orderclause
  sorting := '';
  for i := 0 to ( gridData.SortColumns.Count - 1 ) do
  begin
    sortcol := gridData.SortColumns[i];

    if ( sortcol.SortType <> stNone ) then
    begin
      if ( sorting <> '' ) then
      begin
        sorting := sorting + ', ';
      end;
      sorting := sorting + mask( sortcol.FieldName );
    end;

    case sortcol.SortType of
      // stNone: ;
      stAscending :
      begin
        sorting := sorting + ' DESC';
      end;
      stDescending:
      begin
        sorting := sorting + ' ASC';
      end;
    end;
  end;

  // Write orderclause to registry
  reg := TRegistry.Create();
  reg.OpenKey( REGPATH + '\Servers\' + FConn.Description, true );
  reg_value := 'ORDERCLAUSE_' + ActualDatabase + '.' + ActualTable;
  if ( sorting <> '' ) then
  begin
    reg.WriteString( reg_value, sorting );
  end
  else
  begin
    if ( reg.ValueExists( reg_value ) ) then
      reg.DeleteValue( reg_value );
  end;
  reg.CloseKey;

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
  value := gridData.SelectedField.AsString;
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


procedure TMDIChild.btnDbPropertiesClick(Sender: TObject);
var
  NodeData : PVTreeData;
begin
  if Assigned(ListTables.FocusedNode) then
  begin
    NodeData := ListTables.GetNodeData(ListTables.FocusedNode);
    SelectTable(ActualDatabase, NodeData.Captions[0]);
  end;
end;

procedure TMDIChild.btnDbViewDataClick(Sender: TObject);
var
  NodeData : PVTreeData;
begin
  NodeData := ListTables.GetNodeData(ListTables.FocusedNode);
  ShowTableData(NodeData.Captions[0]);
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
  grid: TSMDBGrid;
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
    FileName := ActualTable;
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
    reg.openkey( REGPATH + '\Servers\' + FConn.Description, false );
    reg_value := 'WHERECLAUSE_' + ActualDatabase + '.' + ActualTable;
    if where <> '' then
      reg.WriteString( reg_value, where )
    else
    if reg.ValueExists( reg_value ) then
      reg.DeleteValue( reg_value );
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

procedure TMDIChild.MenuChangeTypeClick(Sender: TObject);
var
  i : Integer;
  tabletype : String;
  Selected : TStringList;
begin
  tabletype := (Sender as TMenuItem).Caption;
  tabletype := StringReplace( tabletype, '&', '', [rfReplaceAll] ); // Remove Auto-Hotkey
  Selected := GetVTCaptions(ListTables, True);
  for i:=0 to Selected.Count - 1 do
    ExecUpdateQuery( 'ALTER TABLE ' + mask(Selected[i]) + ' TYPE = ' + tabletype);
  Selected.Free;
  ShowDBProperties(self);
end;

procedure TMDIChild.MenuChangeTypeOtherClick(Sender: TObject);
var
  i : Integer;
  strtype : String;
  Selected : TStringList;
begin
  // change table-type:
  if inputquery('Change table-type...','New table-type:', strtype) then begin
    Selected := GetVTCaptions(ListTables, True);
    for i:=0 to Selected.Count - 1 do
      ExecUpdateQuery( 'ALTER TABLE ' + mask(Selected[i]) + ' TYPE = ' + strtype );
    Selected.Free;
    ShowDBProperties(self);
  end;
end;

procedure TMDIChild.InsertRecord(Sender: TObject);
begin
  viewdata(self);
  GetVisualDataset().Insert;
end;


// select all tables
procedure TMDIChild.selectall1Click(Sender: TObject);
begin
  ListTables.SelectAll(False);
end;

procedure TMDIChild.popupQueryPopup(Sender: TObject);
var
  NotInFilterMemo,
  somechars         : Boolean;
begin
  // Depending which SynMemo is focused, (de-)activate some menuitems
  // The popupQuery is used in both Filter- and Query-Memo
  NotInFilterMemo := Not SynMemoFilter.Focused;

  MenuSetFilter.Visible := Not NotInFilterMemo;

  MenuRun.Visible := NotInFilterMemo;
  MenuRunSelection.Visible := NotInFilterMemo;
  MenuRunLine.Visible := NotInFilterMemo;

  MenuCopy.Visible := NotInFilterMemo;
  MenuPaste.Visible := NotInFilterMemo;

  MenuFind.Visible := NotInFilterMemo;
  MenuReplace.Visible := NotInFilterMemo;

  MenuLoad.Visible := NotInFilterMemo;
  MenuInsertFileAtCursor.Visible := NotInFilterMemo;
  MenuSave.Visible := NotInFilterMemo;
  MenuSaveSelectionToFile.Visible := NotInFilterMemo;
  MenuSaveAsSnippet.Visible := NotInFilterMemo;
  MenuSaveSelectionAsSnippet.Visible := NotInFilterMemo;

  if NotInFilterMemo then
  begin
    somechars := SynMemoQuery.GetTextLen > 0;
    MenuRun.ShortCut := TextToShortCut('F9');  // Exec SQL with F9
    MenuSetFilter.ShortCut := TextToShortCut('');
    // Inserting file at cursor only makes sense with content
    MenuInsertFileAtCursor.Enabled := somechars;
    Menusave.Enabled := somechars;
    MenuSaveSelectionToFile.Enabled := SynMemoQuery.SelAvail;
    MenuSaveAsSnippet.Enabled := somechars;
    MenuSaveSelectionAsSnippet.Enabled := SynMemoQuery.SelAvail;
  end
  else
  begin
    somechars := SynMemoFilter.GetTextLen > 0;
    MenuRun.ShortCut := TextToShortCut('');
    MenuSetFilter.ShortCut := TextToShortCut('F9'); // set Filter with F9
  end;
  MenuClear.Enabled := somechars;
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
  end;
end;

procedure TMDIChild.Autoupdate1Click(Sender: TObject);
var
  seconds : String;
  secondsInt : Integer;
begin
  // set interval for autorefresh-timer
  seconds := IntToStr(TimerProcesslist.interval div 1000);
  if inputquery('Auto-refresh processlist','Update list every ... seconds:', seconds) then begin
    secondsInt := StrToIntDef(seconds, 0);
    if secondsInt > 0 then begin
      TimerProcesslist.Interval := secondsInt * 1000;
      TimerProcesslist.Enabled := true;
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
  TimerProcesslist.Enabled := true;
  EnableAutoRefresh.Checked := true;
  DisableAutoRefresh.Checked := false;
end;

procedure TMDIChild.DisableAutoRefreshClick(Sender: TObject);
begin
  // enable autorefresh-timer
  TimerProcesslist.Enabled := false;
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
  else if src = lboxQueryHelpers then
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
  AFiles: TStrings);
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
  MenuAutoupdate.Enabled := PageControlHost.ActivePage = tabProcessList;
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
  MainForm.DropTable.Enabled := DBtree.Selected.Level = 2;
  DBRightClickSelectedItem := DBtree.Selected;
end;


procedure TMDIChild.btnQuerySaveSnippetClick(Sender: TObject);
var
  snippetname : String;
  sfile       : Textfile;
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
    AssignFile( sfile, snippetname );
    Rewrite( sfile );
    // Save complete content or just the selected text,
    // depending on the tag of calling control
    case (Sender as TComponent).Tag of
      0: Write( sfile, SynMemoQuery.Text );
      1: Write( sfile, SynMemoQuery.SelText );
    end;
    CloseFile( sfile );
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
    keyword := SynMemoQuery.WordAtCursor
  // Data-Tab
  else if (PageControlMain.ActivePage = tabData)
    and (-1 < gridData.Col)
    and (gridData.Col <= Length(VTRowDataListColumns)) then
  begin
    keyword := VTRowDataListColumns[gridData.Col-1].Captions[1];
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

procedure TMDIChild.ToolButton4Click(Sender: TObject);
begin
  SaveBlob;
  mainform.HTMLviewExecute(Sender);
end;

procedure TMDIChild.btnQuerySaveClick(Sender: TObject);
var
  f : TextFile;
begin
  // Save SQL
  if SaveDialogSQLFile.Execute then
  begin
    Screen.Cursor := crHourGlass;
    AssignFile(f, SaveDialogSQLFile.FileName);
    Rewrite(f);
    // Save complete content or just the selected text,
    // depending on the tag of calling control
    case (Sender as TComponent).Tag of
      0: Write( f, SynMemoQuery.Text );
      1: Write( f, SynMemoQuery.SelText );
    end;
    CloseFile(f);
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
  tmpstr, filecontent      : String;
  f                        : TextFile;
  msgtext                  : String;
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
    AssignFile( f, filename );
    Reset( f );
    while not eof( f ) do
    begin
      Readln( f, tmpstr );
      filecontent := filecontent + tmpstr + CRLF;
    end;
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
  CloseFile( f );

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
  ShowTableData(ActualTable);
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
  if (Sender as TDBGrid).SelectedField <> nil then begin
    // Set focus on DBMemo when user doubleclicks a (MEMO)-cell
    if (sender as TSMDBGrid).SelectedField.IsBlob and (PageControlBlobEditors.ActivePage = tabBlobEditorText) then begin
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

procedure TMDIChild.DBGridGetCellParams(Sender: TObject; Field: TField;
  AFont: TFont; var Background: TColor; Highlight: Boolean);
begin
  if (Sender as TDBGrid).SelectedRows.CurrentRowSelected then begin
    background := clInfoBK;
    afont.Color := clInfoText;
  end;
  if (field <> nil) and field.IsNull then background := mainform.DataNullBackground;
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
    value := sstr(gridData.SelectedField.AsString, 100);
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
begin
  Result := -1; // Silence compiler warning.
  MysqlQuery := nil;
  try
    try
      // Start query execution
      MysqlQuery := RunThreadedQuery(sql);
      Result := TZQuery(MysqlQuery.MysqlDataset).RowsAffected;
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
    if MysqlQuery <> nil then FreeAndNil (MysqlQuery);
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
      if res <> nil then FreeAndNil(res);
      if not HandleErrors then raise THandledSQLError.Create(E.Message);
      Result := nil;
    end;
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
end;


function TMDIChild.GetNamedVar( SQLQuery: String; x: String; HandleErrors: Boolean = false; DisplayErrors: Boolean = false) : String;
var
  ds: TDataSet;
begin
  ds := GetResults( SQLQuery, HandleErrors, DisplayErrors );
  if ds = nil then exit;
  Result := ds.Fields.FieldByName(x).AsString;
  ds.Close;
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
    ExecPostAsync(FConn,nil,FProgressForm.Handle,ds);
    WaitForQueryCompletion(FProgressForm);
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
  if (Copy(AQuery, 1, 3) <> 'USE') then EnsureActiveDatabase;
  // Indicate a querythread is active (only one thread allow at this moment)
  FQueryRunning := true;
  try
    // Check if the connection of the current window is still alive
    // Otherwise reconnect
    try
      CheckConnection;
    except
      on E: Exception do begin
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
    WaitForQueryCompletion(FProgressForm);
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
  grid: TSMDBGrid;
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


procedure TMDIChild.ZQuery1EditError(DataSet: TDataSet; E: EDatabaseError;
  var Action: TDataAction);
begin
  LogSQL( E.Message, true );
end;


// Search with searchbox
procedure TMDIChild.ButtonDataSearchClick(Sender: TObject);
var
  i : Integer;
  where : String;
begin
  if not gridData.DataSource.DataSet.Active then
    exit;
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



// Click on first menu-item in context-menu of tablelist-columns
procedure TMDIChild.MenuTablelistColumnsClick(Sender: TObject);
var
  menuitem : TMenuItem;
  TablelistColumnsList : TStringList;
begin
  menuitem := (Sender as TMenuItem);
  with TRegistry.Create do
  try
    openkey( REGPATH + '\Servers\' + FConn.Description, true );
    case menuitem.Tag of
      1 : // Toggle default-columns
        WriteBool( 'TablelistDefaultColumns', not menuitem.Checked );
      2 :
        begin
          if ValueExists( 'TablelistColumns' ) then
            TablelistColumnsList := Explode( ',', ReadString( 'TablelistColumns' ) )
          else
            TablelistColumnsList := TStringList.Create;
          if TablelistColumnsList.IndexOf( menuitem.Caption ) > -1 then
            TablelistColumnsList.Delete( TablelistColumnsList.IndexOf( menuitem.Caption ) )
          else
            TablelistColumnsList.Add( menuitem.Caption );
          WriteString( 'TablelistColumns', implodestr( ',', TablelistColumnsList ) );
        end;
     end;
    free;
  except
    free;
    MessageDlg( 'Error while writing to registry.', mtError, [mbOK], 0 );
    exit;
  end;
  menuitem.Checked := not menuitem.Checked;
  ShowDBProperties( self );
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



function TMDIChild.GetActiveGrid: TSMDBGrid;
begin
  Result := nil;
  if PageControlMain.ActivePage = tabData then Result := gridData;
  if PageControlMain.ActivePage = tabQuery then Result := gridQuery;
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
    3: // Load snippet file �nto query-memo
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
    def := GetResults( 'SHOW COLUMNS FROM ' + mask(ActualTable) + ' LIKE ' + esc(NodeData.Captions[0]), False, False );

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
    sql_update := 'ALTER TABLE ' + mask(ActualTable) +
      ' CHANGE ' + mask(NodeData.Captions[0]) +
      ' ' + mask(NewText) + ' ' +
      def.FieldByName('Type').AsString + ' ' +
      sql_null +
      sql_default +
      sql_extra;

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
begin
  // Get the pointer to the node data
  NodeData := Sender.GetNodeData(Node);
  // Bind data to node
  if Sender = ListVariables then
  begin
    NodeData.Captions := VTRowDataListVariables[Node.Index].Captions;
    NodeData.ImageIndex := VTRowDataListVariables[Node.Index].ImageIndex;
  end
  else if Sender = ListCommandStats then
  begin
    NodeData.Captions := VTRowDataListCommandStats[Node.Index].Captions;
    NodeData.ImageIndex := VTRowDataListCommandStats[Node.Index].ImageIndex;
  end
  else if Sender = ListProcesses then
  begin
    NodeData.Captions := VTRowDataListProcesses[Node.Index].Captions;
    NodeData.ImageIndex := VTRowDataListProcesses[Node.Index].ImageIndex;
  end
  else if Sender = ListTables then
  begin
    NodeData.Captions := VTRowDataListTables[Node.Index].Captions;
    NodeData.ImageIndex := VTRowDataListTables[Node.Index].ImageIndex;
  end
  else if Sender = ListColumns then
  begin
    NodeData.Captions := VTRowDataListColumns[Node.Index].Captions;
    NodeData.ImageIndex := VTRowDataListColumns[Node.Index].ImageIndex;
  end;
end;


{**
  Free data of a node
}
procedure TMDIChild.vstFreeNode(Sender: TBaseVirtualTree; Node:
    PVirtualNode);
var
  a : TVTreeDataArray;
begin
  // Detect which global array should be processed
  if Sender = ListVariables then
    a := VTRowDataListVariables
  else if Sender = ListCommandStats then
    a := VTRowDataListCommandStats
  else if Sender = ListProcesses then
    a := VTRowDataListProcesses
  else if Sender = ListTables then
    a := VTRowDataListTables
  else if Sender = ListColumns then
    a := VTRowDataListColumns;

  if Node.Index < Cardinal(High(a)-1) then
  begin
    // Delete node somewhere in the middle of the array
    // Taken from http://delphi.about.com/cs/adptips2004/a/bltip0204_2.htm
    System.Move( a[Node.Index +1],
      a[Node.Index],
      (Cardinal(Length(a)) - Node.Index -1) * SizeOf(TVTreeData) + 1
      );
  end;
  SetLength(a, Length(a) - 1)
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
  Int1, Int2 : Integer;
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
    Int1 := MakeInt( CellText1 );
    Int2 := MakeInt( CellText2 );
    if Int1 > Int2 then
      Result := 1
    else if Int1 = Int2 then
      Result := 0
    else if Int1 < Int2 then
      Result := -1;
  end
  else begin
    // Compare Strings
    Result := CompareText( CellText1, CellText2 );
  end;
end;



end.



