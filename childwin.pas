UNIT Childwin;


// -------------------------------------
// MDI-Child-Window
// -------------------------------------


INTERFACE

uses
  Synchronization,
  Windows, Classes, Graphics, Forms, Controls, StdCtrls,
  ExtCtrls, ComCtrls, ImgList, SysUtils, Dialogs, Menus,
  SynEdit, SynMemo, SynEditHighlighter, SynHighlighterSQL, SynEditSearch,
  SynEditTypes, Registry, Spin, Clipbrd, Shellapi,
  Buttons, CheckLst, ToolWin, Db, DBGrids,
  DBCtrls, helpers,
  Grids, messages, Mask, ZDataset,
  ZAbstractRODataset, ZConnection,
  ZSqlMonitor, ZPlainMySqlDriver, EDBImage, ZAbstractDataset, ZDbcLogging,
  SynCompletionProposal, HeidiComp, SynEditMiscClasses, MysqlQuery, MysqlQueryThread,
  queryprogress, communication, MysqlConn, smdbgrid;


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
    Panel2: TPanel;
    tabTable: TTabSheet;
    Panel3: TPanel;
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
    ListVariables: TSortListView;
    ListProcesses: TSortListView;
    popupHost: TPopupMenu;
    Kill1: TMenuItem;
    NewDatabase1: TMenuItem;
    ListTables: TSortListView;
    Refresh1: TMenuItem;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    ToolBar1: TToolBar;
    btnDbViewData: TToolButton;
    btnDbProperties: TToolButton;
    btnDbInsertRecord: TToolButton;
    btnDbEmptyTable: TToolButton;
    ToolBar2: TToolBar;
    menurefresh: TMenuItem;
    N2: TMenuItem;
    Panel7: TPanel;
    btnTableDropField: TToolButton;
    btnTableViewData: TToolButton;
    SynSQLSyn1: TSynSQLSyn;
    SynMemoQuery: TSynMemo;
    Splitter3: TSplitter;
    menucreatetable: TMenuItem;
    OpenDialog1: TOpenDialog;
    Timer1: TTimer;
    popupTableGrid: TPopupMenu;
    Refresh2: TMenuItem;
    DropField1: TMenuItem;
    N3: TMenuItem;
    N5: TMenuItem;
    PopupmenuDropDatabase: TMenuItem;
    btnTableInsertRecord: TToolButton;
    popupDataGrid: TPopupMenu;
    Refresh3: TMenuItem;
    Insertrecord2: TMenuItem;
    btnTableAddField: TToolButton;
    MenuAddField: TMenuItem;
    btnTableEditField: TToolButton;
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
    Timer5: TTimer;
    PopupMenuDropTable: TMenuItem;
    N17: TMenuItem;
    Panel9: TPanel;
    ListColumns: TSortListView;
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
    PageControl4: TPageControl;
    TabSheet3: TTabSheet;
    DBMemo1: TDBMemo;
    TabSheet4: TTabSheet;
    SynMemoFilter: TSynMemo;
    btnDbCopyTable: TToolButton;
    btnDbDropTable: TToolButton;
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
    Panel8: TPanel;
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
    ZQuery1: TZQuery;
    ZQuery3: TZReadOnlyQuery;
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
    btnTableManageIndexes: TToolButton;
    tabCommandStats: TTabSheet;
    ListCommandStats: TSortListView;
    CheckBoxDataSearch: TCheckBox;
    QF13: TMenuItem;
    QF14: TMenuItem;
    QF15: TMenuItem;
    QF16: TMenuItem;
    QF17: TMenuItem;
    N21: TMenuItem;
    procedure gridQueryMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure gridDataMouseDown(Sender: TObject; Button: TMouseButton;
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
      const Value: string; Shift: TShiftState; Index: Integer; EndToken: Char);
    procedure btnDbPropertiesClick(Sender: TObject);
    procedure popupDbGridPopup(Sender: TObject);
    procedure SynCompletionProposal1CodeCompletion(Sender: TObject;
      var Value: string; Shift: TShiftState; Index: Integer; EndToken: Char);
    procedure SynCompletionProposal1Execute(Kind: TSynCompletionType;
      Sender: TObject; var CurrentInput: string; var x, y: Integer;
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
    procedure ListTablesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ValidateDbActions;
    procedure SelectHost;
    procedure SelectDatabase(db: string);
    procedure SelectTable(db: string; table: string);
    procedure ShowTable(Sender: TObject);
    procedure EmptyTable(Sender: TObject);
    procedure DropDB(Sender: TObject);
    procedure LogSQL(msg: string = ''; comment: Boolean = true );
    procedure ShowVariablesAndProcesses(Sender: TObject);
    procedure ListProcessesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure CreateDatabase(Sender: TObject);
    procedure KillProcess(Sender: TObject);
    procedure PageControlHostChange(Sender: TObject);
    procedure ExecSQLClick(Sender: TObject; Selection: Boolean = false; CurrentLine: Boolean=false);
    procedure ListColumnsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure DropField(Sender: TObject);
    procedure SynMemoQueryChange(Sender: TObject);
    procedure CreateTable(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UpdateField(Sender: TObject);
    procedure MenuAdvancedPropertiesClick(Sender: TObject);
    procedure ListTablesEdited(Sender: TObject; Item: TListItem;
      var S: String);
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
    procedure Timer5Timer(Sender: TObject);
    procedure gridDataTitleClick(Column: TColumn);
    procedure Filter1Click(Sender: TObject);
    procedure MenuLimitClick(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure QuickFilterClick(Sender: TObject);
    procedure btnBlobWordWrapClick(Sender: TObject);
    procedure PageControl4Change(Sender: TObject);
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
    procedure ListTablesEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
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
    function GetVar( SQLQuery: String; x: Integer = 0 ) : String;
    procedure GetResults( SQLQuery: String; ZQuery: TZReadOnlyQuery; QuietOnError: Boolean = false );
    function GetCol( SQLQuery: String; x: Integer = 0 ) : TStringList;
    procedure ZSQLMonitor1LogTrace(Sender: TObject; Event: TZLoggingEvent);
    procedure ResizeImageToFit;
    procedure Splitter2Moved(Sender: TObject);
    procedure DataSourceDataChange(Sender: TObject; Field: TField);
    procedure DBGridColEnter(Sender: TObject);
    procedure ZQuery1EditError(DataSet: TDataSet; E: EDatabaseError;
      var Action: TDataAction);
    procedure FormResize(Sender: TObject);
    procedure ButtonDataSearchClick(Sender: TObject);
    procedure EditDataSearchEnter(Sender: TObject);
    procedure EditDataSearchExit(Sender: TObject);
    procedure ListTablesColumnRightClick(Sender: TObject;
      Column: TListColumn; Point: TPoint);
    procedure MenuTablelistColumnsClick(Sender: TObject);
    procedure ListTablesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    function mask(str: String) : String;
    procedure CheckConnection();
    procedure ZQueryBeforeSendingSQL(DataSet: TDataSet);
    procedure QueryLoad( filename: String; ReplaceContent: Boolean = true );
    procedure AddOrRemoveFromQueryLoadHistory( filename: String; AddIt: Boolean = true; CheckIfFileExists: Boolean = true );
    procedure popupQueryLoadClick( sender: TObject );
    procedure FillPopupQueryLoad;
    procedure PopupQueryLoadRemoveAbsentFiles( sender: TObject );
    function ExecuteQuery(query: string): TDataSet;
    function CreateOrGetRemoteQueryTab(sender: THandle): THandle;
    function GetCalculatedLimit( Table: String ): Int64;

    private
      strHostRunning             : String;
      uptime, time_connected     : Integer;
      OnlyDBs                    : TStringList;  // used on connecting
      viewingdata                : Boolean;
      WhereFilters               : TStringList;
      WhereFiltersIndex          : Integer;
      StopOnErrors, WordWrap     : Boolean;
      CanAcessMysqlFlag          : Boolean;
      FCurDataset                : TDataSet;
      FQueryRunning              : Boolean;
      FMysqlConn                 : TMysqlConn;
      FConnParams                : TConnParams;
      lastUsedDB                 : String;

      function HasAccessToDB(ADBName: String): Boolean;      // used to flag if the current account can access mysql database
      procedure GridHighlightChanged(Sender: TObject);
      procedure SaveBlob;
      function GetActiveGrid: TSMDBGrid;
      function CanAcessMysql: Boolean;
      procedure WaitForQueryCompletion(WaitForm: TForm);
      function RunThreadedQuery(AQuery : String) : TMysqlQuery;
      procedure DisplayRowCountStats;

    public
      ActualDatabase, ActualTable: string;
      dataselected, editing      : Boolean;
      mysql_version              : Integer;
      tnodehost                  : TTreeNode;
      OnlyDBs2                   : TStringList;
      Description                : String;
      DBRightClickSelectedItem   : TTreeNode;    // TreeNode for dropping with right-click

      FProgressForm              : TFrmQueryProgress;
      procedure Init(AConnParams : PConnParams; AMysqlConn : TMysqlConn);
      procedure SetQueryRunningFlag(AValue : Boolean);
      //procedure HandleQueryNotification(ASender : TMysqlQuery; AEvent : Integer);
      function GetVisualDataset() : TDataSet;

      function ExecUpdateQuery (ASQLQuery: String ) : Boolean;
      function ExecSelectQuery (AQuery : String; out AMysqlQuery : TMysqlQuery) : Boolean; 
      function ExecUseQuery (ADatabase : String) : Boolean;

      property ActiveGrid: TSMDBGrid read GetActiveGrid;
      property MysqlConn : TMysqlConn read FMysqlConn;
      property ConnParams : TConnParams read FConnParams;
  end;

const
  sqllog_char_limit = 2000;

// --------------------------------------------------------------------------------------
IMPLEMENTATION

uses
  Main, createtable, fieldeditor, tbl_properties, tblcomment, selectsomedatabases, optimizetables, copytable, sqlhelp,
  printlist;

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
begin

  try
    time_connected := 0;
    TimerConnected.Enabled := true;
    LogSQL( 'Connection-ID: ' + IntToStr(MySQLConn.Connection.GetThreadId) );
    // On Re-Connection, try to restore lost properties
    if FMysqlConn.Connection.Database <> '' then
    begin
      ExecUseQuery( FMysqlConn.Connection.Database );
    end;
  except
    on E: Exception do
    begin
      LogSQL(E.Message, true);
      Screen.Cursor := crDefault;
      MessageDlg(E.Message, mtError, [mbOK], 0);
      raise;
    end;
  end;

end;


// Check the tabletype of the selected table in the Popupmenu of ListTables
procedure TMDIChild.popupDbGridPopup(Sender: TObject);
var
  i               : byte;
  SelectedEngine  : String;
begin
  if ListTables.SelCount <> 1 then
    exit;
  for i:=0 to ListTables.Columns.count-1 do
  begin
    if ListTables.Columns[i].Caption = 'Engine' then
    begin
      SelectedEngine := ListTables.Selected.SubItems[i-1];
      break;
    end;
  end;
  for i:=0 to MenuChangeType.count-1 do
  begin
    MenuChangeType.Items[i].Checked := MenuChangeType.Items[i].Caption = SelectedEngine;
  end;
end;


procedure TMDIChild.Init(AConnParams : PConnParams; AMysqlConn : TMysqlConn);
var
  AutoReconnect    : Boolean;
  menuitem         : TMenuItem;
  i                : Byte;
  winName          : string;
  j                : Integer;
  treenode         : TTreeNode;
begin
  FConnParams := AConnParams^;
  FMysqlConn := AMysqlConn; // we're now responsible to free it

  FConnParams.MysqlConn := FMysqlConn.Connection; // use this connection (instead of zconn)

  // replace default connections
  ZQuery1.Connection := FConnParams.MysqlConn;
  ZQuery3.Connection := FConnParams.MysqlConn;

  // initialization: establish connection and read some vars from registry
  MainForm.Showstatus('Creating window...', 2, true);

  // temporarily disable AutoReconnect in Registry
  // in case of unexpected application-termination
  AutoReconnect := false;
  with TRegistry.Create do
  begin
    openkey(REGPATH, true);
    if Valueexists('Autoreconnect') then
    if ReadBool('AutoReconnect') then
    begin
      AutoReconnect := true;
      WriteBool('AutoReconnect', false);
    end;
    closekey();
  end;

  ReadWindowOptions;

  MainForm.Showstatus('Connecting to '+FConnParams.MysqlParams.Host+'...', 2, true);

  try
    PerformConnect;
  except
    timer5.Enabled := true;
    Exit;
  end;

  Description := FMysqlConn.Description;;
  Caption := Description;
  OnlyDBs := explode(';', FConnParams.DatabaseList);
  if FConnParams.DatabaseListSort then
    OnlyDBs.Sort;

  // Versions and Statistics
  LogSQL( 'Connection established with host "' + FMysqlConn.Connection.hostname + '" on port ' + inttostr(FMysqlConn.Connection.Port) );

  ShowVariablesAndProcesses(self);
  ReadDatabasesAndTables(self);

  // re-enable AutoReconnect in Registry!
  if AutoReconnect then
  with TRegistry.Create do
  begin
    openkey(REGPATH, true);
    WriteBool('AutoReconnect', true);
    closekey();
  end;

  // set some defaults
  ActualDatabase := '';
  ActualTable := '';

  // read engine-types for popupmenu in database tab
  if mysql_version >= 40102 then
  begin
    for i := MenuChangeType.Count-1 downto 0 do
      MenuChangeType.Delete(i);
    GetResults( 'SHOW ENGINES', ZQuery3 );
    for i := 0 to ZQuery3.RecordCount  -1 do
    begin
      menuitem := TMenuItem.Create(self);
      menuitem.Caption := ZQuery3.FieldByName('Engine').AsString ;
      menuitem.Hint := ZQuery3.FieldByName('Comment').AsString ;
      if Uppercase(ZQuery3.FieldByName('Support').AsString) = 'NO' then
      begin
        menuitem.Enabled := false;
        menuitem.Hint := menuitem.Hint + ' (Not supported on this server)';
      end;
      menuitem.OnClick := MenuChangeTypeClick;
      MenuChangeType.Add(menuitem);
      ZQuery3.Next;
    end;
  end;
  CanAcessMysqlFlag := CanAcessMysql;
  SetWindowConnected(true);
  i := SetWindowName(Description);
  winName := Description;
  if i <> 0 then winName := winName + Format(' (%d)', [i]);
  Application.Title := winName + ' - ' + APPNAME;

  // Reselect last used database
  if lastUsedDB <> '' then
  begin
    for j:=0 to DBTree.Items.Count-1 do
    begin
      treenode := DBTree.Items[j];
      if (treenode.Level = 1) and (treenode.Text = lastUsedDB ) then
      begin
        DBTree.Selected := treenode;
        break;
      end;
    end;
  end;

end;


procedure TMDIChild.ReadWindowOptions;
var
  ws : String;
  i : Integer;
  menuitem : Tmenuitem;
begin
  with TRegistry.Create do
  begin
    if OpenKey(REGPATH, true) then
    begin
      ws := ReadString('childwinstate');
      if mainform.MDIChildCount > 1 then
        if mainform.MDIChildren[0].WindowState = wsNormal then
          ws := '';
      if ws = 'Normal' then
      begin
        windowstate := wsNormal;
        if valueexists('childwinleft') then
        begin
          left := ReadInteger('childwinleft');
          top := ReadInteger('childwintop');
          width := ReadInteger('childwinwidth');
          height := ReadInteger('childwinheight');
        end;
      end else
      if ws = 'Minimized'
        then windowstate := wsMinimized else
      if ws = 'Maximized'
        then windowstate := wsMaximized;

      // other values:
      if valueExists('querymemoheight') then
        panel7.Height := ReadInteger('querymemoheight');
      if valueExists('dbtreewidth') then
        dbtree.Width := ReadInteger('dbtreewidth');
      if valueExists('sqloutheight') then
        PageControlBottom.Height := ReadInteger('sqloutheight');
      if valueExists('DefaultColWidth') then
        Mainform.DefaultColWidth := ReadInteger('DefaultColWidth')
      else
        Mainform.DefaultColWidth := 100;

      // SQL-Font:
      if (ValueExists('FontName')) and (ValueExists('FontSize')) then begin
        SynMemoQuery.Font.Name := ReadString('FontName');
        SynMemoSQLLog.Font.Name := ReadString('FontName');
        SynMemoQuery.Font.Size := ReadInteger('FontSize');
        SynMemoSQLLog.Font.Size := ReadInteger('FontSize');
      end;

      // Data-Font:
      if (ValueExists('DataFontName')) and (ValueExists('DataFontSize')) then begin
        gridData.Font.Name := ReadString('DataFontName');
        gridQuery.Font.Name := ReadString('DataFontName');
        DBMemo1.Font.Name := ReadString('DataFontName');
        gridData.Font.Size := ReadInteger('DataFontSize');
        gridQuery.Font.Size := ReadInteger('DataFontSize');
        DBMemo1.Font.Size := ReadInteger('DataFontSize');
      end;

      // color coding:
      if ValueExists('SQLColKeyAttri') then
        SynSQLSyn1.KeyAttri.Foreground := StringToColor(ReadString('SQLColKeyAttri'));
      if ValueExists('SQLColFunctionAttri') then
        SynSQLSyn1.FunctionAttri.Foreground := StringToColor(ReadString('SQLColFunctionAttri'));
      if ValueExists('SQLColDataTypeAttri') then
        SynSQLSyn1.DataTypeAttri.Foreground := StringToColor(ReadString('SQLColDataTypeAttri'));
      if ValueExists('SQLColNumberAttri') then
        SynSQLSyn1.NumberAttri.Foreground := StringToColor(ReadString('SQLColNumberAttri'));
      if ValueExists('SQLColStringAttri') then
        SynSQLSyn1.StringAttri.Foreground := StringToColor(ReadString('SQLColStringAttri'));
      if ValueExists('SQLColCommentAttri') then
        SynSQLSyn1.CommentAttri.Foreground := StringToColor(ReadString('SQLColCommentAttri'));
      if ValueExists('SQLColTablenameAttri') then
        SynSQLSyn1.TablenameAttri.Foreground := StringToColor(ReadString('SQLColTablenameAttri'));

      // SQLFiles-History
      FillPopupQueryLoad;

      // SQL-Filter-Files-History
      i := 1;
      popupFilterOpenFile.Items.Clear;
      while ValueExists('SQLWhereFile'+inttostr(i)) do begin
        menuitem := Tmenuitem.Create(self);
        menuitem.Caption := inttostr(popupFilterOpenFile.Items.count+1) + ' ' + ReadString('SQLWhereFile'+inttostr(i));
        menuitem.OnClick := LoadSQLWhereFile;
        popupFilterOpenFile.Items.Add(menuitem);
        inc(i);
      end;

      // Synchronize internal variables with defaults from DFM.
      StopOnErrors := btnQueryStopOnErrors.Down;

      // Open server-specific registry-folder.
      // relative from already opened folder!
      OpenKey( 'Servers\' + FConnParams.Description, true );

      // Set last used database, select it later in Init
      lastUsedDB := ReadString( 'lastUsedDB' );

    end;
    CloseKey;
  end;
end;


procedure TMDIChild.FormClose(Sender: TObject; var Action: TCloseAction);
var
  ws : String;
begin
  SetWindowConnected(false);
  SetWindowName(main.discname);
  Application.Title := APPNAME;
  // closing connection and saving some vars into registry
  if windowstate = wsNormal then
    ws := 'Normal' else
  if windowstate = wsMinimized
    then ws := 'Minimized' else
  if windowstate = wsMaximized
    then ws := 'Maximized';

  with TRegistry.Create do
  begin
    if OpenKey(REGPATH, true) then
    begin
      WriteString('childwinstate', ws);
      WriteInteger('childwinleft', left);
      WriteInteger('childwintop', top);
      WriteInteger('childwinwidth', width);
      WriteInteger('childwinheight', height);

      WriteInteger('querymemoheight', panel7.Height);
      WriteInteger('dbtreewidth', dbtree.width);
      WriteInteger('sqloutheight', PageControlBottom.Height);

      // Open server-specific registry-folder.
      // relative from already opened folder!
      OpenKey( 'Servers\' + FConnParams.Description, true );
      WriteString('lastUsedDB', ActualDatabase);
    end;
  end;
  mainform.ToolBarData.visible := false;
  FormDeactivate(sender);
  Action := caFree;
  SetWindowConnected(false);
  SetWindowName(main.discname);
  Application.Title := APPNAME;
end;


procedure TMDIChild.LogSQL(msg: string = ''; comment: Boolean = true);
begin
  // add a sql-command or info-line to history-memo
  while SynMemoSQLLog.Lines.Count > mainform.logsqlnum do
  begin
    SynMemoSQLLog.Lines.Delete(0);
  end;
  // Shorten very long messages
  if Length( msg ) > sqllog_char_limit then
  begin
    msg :=
      '/* Very large SQL query, showing first ' +
      IntToStr(sqllog_char_limit) +
      ' characters: */ ' +
      Copy(msg, 0, sqllog_char_limit) +
      ' ...';
  end;
  msg := StringReplace( msg, #9, ' ', [rfReplaceAll] );
  msg := StringReplace( msg, #10, ' ', [rfReplaceAll] );
  msg := StringReplace( msg, #13, ' ', [rfReplaceAll] );
  msg := StringReplace( msg, '  ', ' ', [rfReplaceAll] );
  if comment then
    msg := '/* ' + msg + ' */';
  SynMemoSQLLog.Lines.Add(msg);
  SynMemoSQLLog.SetBookMark(0,0,SynMemoSQLLog.Lines.Count);
  SynMemoSQLLog.GotoBookMark(0);
  SynMemoSQLLog.ClearBookMark(0);
  SynMemoSQLLog.Repaint;
end;


procedure TMDIChild.ReadDatabasesAndTables(Sender: TObject);
var
  tnode, tchild, tmpSelected: TTreeNode;
  i, j : Integer;
  specialDbs: TStringList;
  dbName : string;
  tableName : string;
  tableCount : integer;
begin
  // Fill DBTree
  Screen.Cursor := crHourGlass;
  dataselected := false;
  DBTree.OnChange := nil;
  DBTree.items.Clear;

  tnodehost := DBtree.Items.Add(nil, FConnParams.MysqlParams.User + '@' + FConnParams.MysqlParams.Host);  // Host or Root
  tnodehost.ImageIndex := 41;
  tnodehost.SelectedIndex := 41;

  Screen.Cursor := crSQLWait;
  mainform.Showstatus('Reading Databases...', 2, true);
  if OnlyDBs.Count = 0 then
  begin
    OnlyDBs2 := TStringList.Create;
    specialDbs := TStringList.Create;
    GetResults( 'SHOW DATABASES', ZQuery3 );
    for i:=1 to ZQuery3.RecordCount do
    begin
      dbName := ZQuery3.FieldByName('Database').AsString;
      if dbName = DBNAME_INFORMATION_SCHEMA then specialDbs.Insert(0, dbName)
      //else if dbName = DBNAME_MYSQL then specialDbs.Add(dbName)
      else OnlyDBs2.Add(dbName);
      ZQuery3.Next;
    end;
    OnlyDBs2.sort;
    // Prioritised position of system-databases
    for i := specialDbs.Count -1 downto 0 do OnlyDBs2.Insert(0, specialDbs[i]);
  end else OnlyDBs2 := OnlyDBs;

  // Let synedit know all tablenames so that they can be highlighted
  SynSQLSyn1.TableNames.Clear;
  SynSQLSyn1.TableNames.Capacity := OnlyDBs2.Count;
  SynSQLSyn1.TableNames.AddStrings( OnlyDBs2 );

  if (OnlyDBs.Count = 0) and (OnlyDBs2.Count > 50) then
    SelectFromManyDatabasesWindow (Self,OnlyDBs2);

  // List Databases and Tables-Names
  tmpSelected := nil;
  tableCount := 0;
  for i:=0 to OnlyDBs2.Count-1 do
  begin
    GetResults( 'SHOW TABLES FROM ' + mask(OnlyDBs2[i]), ZQuery3, true );
    if not ZQuery3.Active then
      continue;
    tnode := DBtree.Items.AddChild(tnodehost, OnlyDBs2[i]);
    tnode.ImageIndex := 37;
    tnode.SelectedIndex := 38;
    if ActualDatabase = OnlyDBs2[i] then tmpSelected := tnode;
    for j:=1 to ZQuery3.RecordCount do begin
      tableCount := tableCount + 1;
      tchild := DBtree.Items.AddChild( tnode, ZQuery3.Fields[0].AsString );
      tchild.ImageIndex := 39;
      tchild.SelectedIndex := 40;
      if
        (tmpSelected <> nil) and
        (tmpSelected.Text = OnlyDBs2[i]) and
        (ActualTable = ZQuery3.Fields[0].AsString) then
        tmpSelected := tchild;
      ZQuery3.Next;
    end;
  end;

  SynSQLSyn1.TableNames.Capacity := OnlyDBs2.Count + tableCount;
  ZQuery3.First;
  for i := 0 to OnlyDBs2.Count - 1 do
  begin
    if not ZQuery3.Active then continue;
    tableName := ZQuery3.Fields[0].AsString;
    SynSQLSyn1.TableNames.Add(tableName);
    ZQuery3.Next;
  end;

  mainform.showstatus(inttostr(OnlyDBs2.count) + ' Databases');
  tnodehost.Expand(false);
  DBTree.OnChange := DBtreeChange;
  if tmpSelected <> nil then
    DBTree.Selected := tmpSelected
  else
    DBTree.Selected := tnodehost;
  DBtreeChange(self, tnodehost);
  MainForm.ShowStatus( STATUS_MSG_READY, 2 );
  Screen.Cursor := crDefault;
end;


procedure TMDIChild.SelectHost;
begin
  tabDatabase.TabVisible := false;
  tabTable.TabVisible := false;
  tabData.TabVisible := false;
  if
   (not DBTree.Dragging) or
   (PageControlMain.ActivePage = tabDatabase) or
   (PageControlMain.ActivePage = tabTable) or
   (PageControlMain.ActivePage = tabData) then
  begin
    PageControlMain.ActivePage := tabHost;
  end;
  Caption := Description;
  ActualDatabase := '';
  ActualTable := '';
end;

procedure TMDIChild.SelectDatabase(db: string);
begin
  tabDatabase.TabVisible := true;
  tabTable.TabVisible := false;
  tabData.TabVisible := false;
  if
   (not DBTree.Dragging) or
   (PageControlMain.ActivePage = tabTable) or
   (PageControlMain.ActivePage = tabData) then
  begin
    PageControlMain.ActivePage := tabDatabase;
  end;
  ListTables.Items.Clear;
  ListColumns.Items.Clear;
  Panel3.Caption := 'Table-Properties';
  Caption := Description + ' - /' + ActualDatabase;
  ActualDatabase := db;
  ShowDBProperties(self);
  ActualTable := '';
end;

procedure TMDIChild.SelectTable(db: string; table: string);
begin
  if ActualDatabase <> db then SelectDatabase(db);
  tabDatabase.TabVisible := true;
  tabTable.TabVisible := true;
  tabData.TabVisible := true;
  dataselected := false;
  ActualTable := table;
  ShowTableProperties(self);
  Caption := Description + ' - /' + ActualDatabase + '/' + ActualTable;
end;

// react on dbtree-clicks
procedure TMDIChild.DBtreeChange(Sender: TObject; Node: TTreeNode);
begin
  Screen.Cursor := crHourGlass;

  case Node.Level of
    0 : begin                                   // Root / Host chosen
      SelectHost;
    end;
    1 : begin                                   // DB chosen
      SelectDatabase(Node.Text);
    end;
    2 : begin                                   // Table chosen
      SelectTable(Node.Parent.Text, Node.Text);
    end;
  end;

  pcChange(self);
  Screen.Cursor := crDefault;
end;


procedure TMDIChild.viewdata(Sender: TObject);
var
  sorting                  : String;
  DropDown                 : TStringList;
  i, j                     : Integer;
  Columns,
  PrimaryKeyColumns        : TStringList;
  reg                      : TRegistry;
  reg_value                : String;
  orderclauses             : TStringList;
  columnname               : String;
  columnexists             : Boolean;
  select_base              : String;
  limit                    : Int64;
  mq : TMysqlQuery;
  conn_params : TConnParams;
  sl_query : TStringList;
begin
  // view table-data with zeos
  if viewingdata then
    abort;
  viewingdata := true;

  sl_query := TStringList.Create();

  // limit number of rows automatically if first time this table is shown
  if not dataselected then
  begin
    // limit number of rows fetched if more than ~ 5 MB of data
    limit := GetCalculatedLimit( ActualTable );

    // adjust limit in GUI
    mainform.ToolBarData.Visible := true;
    if limit = -1 then begin
      mainform.CheckBoxLimit.Checked := false;
    end else begin
      mainform.CheckBoxLimit.Checked := true;
      mainform.EditLimitStart.Text := '0';
      mainform.EditLimitEnd.Text := IntToStr(limit);
    end;
    mainform.Repaint;
  end;

  // set db-aware-component's properties...
  DBMemo1.DataField := '';
  DBMemo1.DataSource := DataSource1;
  EDBImage1.DataField := '';
  EDBImage1.DataSource := DataSource1;

  reg := TRegistry.Create;
  reg.openkey( REGPATH + '\Servers\' + FConnParams.Description, true );

  if not dataselected then
  begin
    SynMemoFilter.Text := '';
    gridData.SortColumns.Clear;
    // Read cached WHERE-clause and set filter
    reg_value := 'WHERECLAUSE_' + ActualDatabase + '.' + ActualTable;
    if reg.ValueExists( reg_value ) then
    begin
      SynMemoFilter.Text := reg.ReadString( reg_value );
      // Ensure the user can see its previous specified filter
      // in case of an SQL-error, it's important that he can delete it
      tabFilter.tabVisible := true;
      PageControlBottom.ActivePage := tabFilter;
    end;
    // Read cached ORDER-clause and set Grid.Sortcolumns
    reg_value := 'ORDERCLAUSE_' + ActualDatabase + '.' + ActualTable;
    if reg.ValueExists( reg_value ) then
    begin
      orderclauses := explode( ',', reg.ReadString( reg_value ) );
      for i:=0 to orderclauses.Count-1 do
      begin
        columnname := trim( copy( orderclauses[i], 0, pos( ' ', orderclauses[i] ) ) );
        columnexists := false;
        for j:=0 to ListColumns.Items.Count-1 do
        begin
          if ListColumns.Items[j].Caption = columnname then
          begin
            columnexists := true;
            break;
          end;
        end;
        if not columnexists then
        begin
          logsql('Notice: A stored ORDER-BY clause could not be applied, because the column "' + columnname + '" does not exist!');
          continue;
        end;
        with gridData.SortColumns.Add do
        begin
          Fieldname := columnname;
          if copy( orderclauses[i], length(orderclauses[i])-3, 4 ) = 'DESC' then
            SortType := stAscending
          else
            SortType := stDescending;
        end;
      end;
    end;
  end;

  sorting := '';
  for i:=0 to gridData.SortColumns.Count-1 do
  begin
    with gridData.SortColumns[i] do
    begin
      if SortType <> stNone then begin
        if sorting <> '' then
          sorting := sorting + ', ';
        sorting := sorting + mask(FieldName);
      end;
      if SortType = stAscending then
        sorting := sorting + ' DESC'
      else if SortType = stDescending then
        sorting := sorting + ' ASC';
    end;
  end;
  reg_value := 'ORDERCLAUSE_' + ActualDatabase + '.' + ActualTable;
  if sorting <> '' then
  begin
    reg.WriteString( reg_value, sorting );
    sorting := 'ORDER BY ' + sorting;
  end
  else if reg.ValueExists( reg_value ) then
    reg.DeleteValue( reg_value );

  MenuLimit.Checked := Mainform.CheckBoxLimit.Checked;
  Columns := TStringList.Create;
  PrimaryKeyColumns := TStringList.Create;

  if (ActualTable <> '') and (ActualDatabase <> '') then
  begin
    // Ensure <Table> and <Data> are visible
    tabTable.TabVisible := true;
    tabData.TabVisible := true;
    // Set the grid-cells to always-edit-mode if set in preferences
    if Mainform.DataAlwaysEditMode then
    begin
      gridData.Options := gridData.Options + [dgAlwaysShowEditor];
      gridQuery.Options := gridQuery.Options + [dgAlwaysShowEditor];
    end
    else
    begin
      gridData.Options := gridData.Options - [dgAlwaysShowEditor];
      gridQuery.Options := gridQuery.Options - [dgAlwaysShowEditor];
    end;
    // Switch to <Data>
    PageControlMain.ActivePage := tabData;

		MainForm.ShowStatus( 'Retrieving data...', 2, true );

    if FCurDataset<>nil then
      FreeAndNil (FCurDataset);

    // Prepare SELECT statement
    select_base := 'SELECT ';
    // Try to calc the rowcount regardless of a given LIMIT
    if mysql_version >= 40000 then
      select_base := select_base + ' SQL_CALC_FOUND_ROWS';
    select_base := select_base + ' * FROM ' + mask(ActualTable);
    sl_query.Add( select_base );
    // Apply custom WHERE filter
    if trim(self.SynMemoFilter.Text) <> '' then
      sl_query.Add( 'WHERE ' + trim(self.SynMemoFilter.Text) );
    // Apply custom ORDER BY if detected in registry
    if sorting <> '' then
      sl_query.Add( sorting );
    // Apply LIMIT
    if mainform.CheckBoxLimit.Checked then
      sl_query.Add('LIMIT ' + mainform.EditLimitStart.Text + ', ' + mainform.EditLimitEnd.Text );
    try
      FQueryRunning := True;

      conn_params := FConnParams;
      conn_params.MysqlParams.Database := ActualDatabase;

      // free previous resultset
      try
        DataSource1.DataSet.Free;
        DataSource1.DataSet := nil;
      except
      end;

      // start query (with wait dialog)
      FProgressForm := TFrmQueryProgress.Create(Self);
      debug('viewdata(): Launching asynchronous query.');
      mq := ExecMysqlStatementAsync(sl_query.Text,FConnParams,nil,FProgressForm.Handle);
      WaitForQueryCompletion(FProgressForm);

      MainForm.ShowStatus( 'Filling grid with record-data...', 2, true );
      DataSource1.DataSet := mq.MysqlDataset;
      FCurDataset := mq.MysqlDataset;

      // Attach After- and Before-Events to the new dataset
      with mq.MysqlDataset do
      begin
        AfterPost := ZQueryGridAfterPost;
        AfterDelete := ZQueryGridAfterPost;
        BeforeClose := ZQueryGridBeforeClose;
        BeforeOpen := ZQueryBeforeSendingSQL;
        BeforePost := ZQueryBeforeSendingSQL;
      end;

    except
      on E:Exception do
      begin
        // Most likely we have a wrong filter-clause when this happens
        LogSQL( E.Message, true );
        MessageDlg( E.Message, mtError, [mbOK], 0 );
        viewingdata := false;
        MainForm.ShowStatus( STATUS_MSG_READY, 2 );
        Screen.Cursor := crDefault;
        exit;
      end;
    end;



 		MainForm.ShowStatus( STATUS_MSG_READY, 2 );

    for i:=0 to ListColumns.Items.Count-1 do
    begin
      Columns.Add( ListColumns.Items[i].Caption );

      // give all enum-fields a PickList with its Items
      if StrCmpBegin('enum', ListColumns.Items[i].SubItems[0]) then
      begin
        DropDown := explode(''',''', getEnumValues(ListColumns.Items[i].SubItems[0]));
        for j:=0 to DropDown.count-1 do
        begin
          DropDown[j] := trimc(DropDown[j], '''');
        end;
        for j:=0 to gridData.Columns.count-1 do
        begin
          if gridData.Columns[j].FieldName = ListColumns.Items[i].Caption then
            gridData.Columns[j].PickList := DropDown;
        end;
      end;

      // make PK-columns = fsBold
      for j:=0 to gridData.Columns.count-1 do
      begin
        if (gridData.Columns[j].FieldName = ListColumns.Items[i].Caption) and
          (ListColumns.Items[i].ImageIndex = 26) then
        begin
          PrimaryKeyColumns.Add( ListColumns.Items[i].Caption );
        end;
      end;

    end;

    for j:=0 to gridData.Columns.count-1 do
    begin
      // for letting NULLs being inserted into "NOT NULL" fields
      // in mysql5+, the server rejects inserts with NULLs in NOT NULL-fields,
      // so the Required-check on client-side is not needed at any time
      FCurDataset.Fields[j].Required := false;

      // set column-width
      if (Mainform.DefaultColWidth <> 0) and (gridData.Columns[j].Width > Mainform.DefaultColWidth) then
      begin
        gridData.Columns[j].Width := Mainform.DefaultColWidth;
      end;

      // make PK-columns = fsBold
      for i:=0 to PrimaryKeyColumns.Count-1 do
      begin
        if (PrimaryKeyColumns[i] = gridData.Columns[j].Fieldname) then
        begin
          gridData.Columns[j].Font.Style := gridData.Columns[j].Font.Style + [fsBold];
          gridData.Columns[j].Color := $02EEEEEE;
        end;
      end;
    end;

    DisplayRowCountStats;
    dataselected := true;
    pcChange(self);
  end;

  viewingdata := false;
  Screen.Cursor := crDefault;
  FreeAndNil (sl_query);
end;



{***
  Calculate + display total rowcount and found rows matching to filter
  in data-tab
}
procedure TMDIChild.DisplayRowCountStats;
var
  rows_matching  : Int64; // rows matching to where-filter
  rows_total     : Int64; // total rowcount
begin
  // Get rowcount
  rows_total := StrToInt64( GetVar( 'SELECT COUNT(*) FROM ' + mask(ActualTable), 0 ) );

  Panel5.Caption := ActualDatabase + '.' + ActualTable + ': ' + FormatNumber(rows_total) + ' records total';

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
  if mysql_version >= 40000 then
  begin
    rows_matching := StrToInt64Def(GetVar('SELECT @found_rows'), 0);
  end
  else
  begin
    rows_matching := rows_total;
  end;

  if (rows_matching <> rows_total)
    and (trim(SynMemoFilter.Text) <> '')
    and (FCurDataset.RecordCount = rows_matching) // Avoids displaying wrong numbers after INSERTs, DELETEs and UPDATEs
    then
    Panel5.Caption := Panel5.Caption + ', ' + FormatNumber(rows_matching) + ' matching to filter';
  if (mysql_version >= 40000) and (rows_matching = rows_total) and (trim(SynMemoFilter.Text) <> '') then
    Panel5.Caption := Panel5.Caption + ', filter matches all records';
  if mainform.CheckBoxLimit.Checked and ( rows_matching > StrToIntDef(mainform.EditLimitEnd.Text,0) ) then
    Panel5.Caption := Panel5.Caption + ', limited to ' + FormatNumber(FCurDataset.RecordCount);
end;



procedure TMDIChild.WaitForQueryCompletion(WaitForm: TForm);
begin
  debug('Waiting for query to complete.');
  WaitForm.ShowModal;
  debug('Query complete.');
end;

{***
  Occurs when active tab has changed.
}
procedure TMDIChild.pcChange(Sender: TObject);
var DataOrQueryTab : Boolean;
begin
  tabFilter.tabVisible := (PageControlMain.ActivePage = tabData);

  Mainform.ExecuteQuery.Enabled := PageControlMain.ActivePage = tabQuery;
  Mainform.ExecuteSelection.Enabled := PageControlMain.ActivePage = tabQuery;
  Mainform.ExecuteLine.Enabled := PageControlMain.ActivePage = tabQuery;
  if (PageControlMain.ActivePage = tabData) and (not dataselected) then
    viewdata(self);
  if PageControlMain.ActivePage = tabQuery then
    if ActualDatabase <> '' then
      Panel6.Caption := 'SQL-Query on Database ' + ActualDatabase + ':'
    else
      Panel6.Caption := 'SQL-Query on Host ' + FConnParams.MysqlParams.Host + ':';

  // copy and save csv-buttons
  DataOrQueryTab := (PageControlMain.ActivePage = tabQuery) or (PageControlMain.ActivePage = tabData);
  with mainform do begin
    Copy2CSV.Enabled := DataOrQueryTab;
    CopyHTMLtable.Enabled := DataOrQueryTab;
    Copy2XML.Enabled := DataOrQueryTab;
    ExportData.Enabled := DataOrQueryTab;
    PrintList.Enabled := not DataOrQueryTab;
    ToolBarData.Visible:= (PageControlMain.ActivePage = tabData);
    DBNavigator1.DataSource := DataSource1;
  end;
  tabBlobEditor.tabVisible := DataOrQueryTab;

  // Move focus to relevant controls in order for them to receive keyboard events.
  if PageControlMain.ActivePage = tabDatabase then ListTables.SetFocus;
  if PageControlMain.ActivePage = tabTable then ListColumns.SetFocus;
  if PageControlMain.ActivePage = tabData then gridData.SetFocus;

  ValidateDbActions;
end;


{ Show tables and their properties on the tabsheet "Database" }
procedure TMDIChild.ShowDBProperties(Sender: TObject);
var
  n               : TListItem;
  i,j,k,t,u       : Integer;
  bytes           : Extended;
  tndb            : TTreenode;
  menuitem        : TMenuItem;
  TablelistColumns: TStringList;
  column          : TListColumn;
begin
  // DB-Properties
  Screen.Cursor := crHourGlass;
  MainForm.ShowStatus( 'Reading from database ' + ActualDatabase + '...', 2, true );
  Mainform.ButtonDropDatabase.Hint := 'Drop Database...|Drop Database ' + ActualDatabase + '...';

  FMysqlConn.Connection.Database := ActualDatabase;
  ExecUseQuery( ActualDatabase );

  Try
    if mysql_version >= 32300 then
    begin
      // get quick results with versions 3.23.xx and newer
      GetResults( 'SHOW TABLE STATUS', ZQuery3 );
      MainForm.ShowStatus( 'Displaying tables from ' + ActualDatabase + '...', 2, true );

      // Generate items for popupDbGridHeader
      for i:=popupDbGridHeader.Items.Count-1 downto 2 do
        popupDbGridHeader.Items.Delete( i );
      with TRegistry.Create do
      begin
        openkey( REGPATH + '\Servers\' + FConnParams.Description, true );
        if ValueExists( 'TablelistDefaultColumns' ) then
          popupDbGridHeader.Items[0].Checked := ReadBool( 'TablelistDefaultColumns' );
        if ValueExists( 'TablelistColumns' ) then
          TablelistColumns := Explode( ',', ReadString( 'TablelistColumns' ) )
        else
          TablelistColumns := TStringList.Create;
        free;
      end;
      for i:=0 to ZQuery3.FieldCount-1 do
      begin
        menuitem := TMenuItem.Create( self );
        menuitem.Caption := ZQuery3.Fields[i].Fieldname;
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

      ListTables.Items.BeginUpdate;
      ListTables.Items.Clear;
      ListTables.Columns.BeginUpdate;
      ListTables.Columns.Clear;
      column := ListTables.Columns.Add;
      column.Caption := 'Table';
      column.Width := -1;
      if popupDbGridHeader.Items[0].Checked then
      begin // Default columns - initialize column headers
        column := ListTables.Columns.Add;
        column.Caption := 'Records';
        column.Alignment := taRightJustify;
        column.Width := 80;

        column := ListTables.Columns.Add;
        column.Caption := 'Size';
        column.Alignment := taRightJustify;
        column.Width := -1;

        column := ListTables.Columns.Add;
        column.Caption := 'Created';
        column.Width := -1;

        column := ListTables.Columns.Add;
        column.Caption := 'Updated';
        column.Width := -1;

        column := ListTables.Columns.Add;
        column.Caption := 'Engine';
        column.Width := -1;

        column := ListTables.Columns.Add;
        column.Caption := 'Comment';
        column.Width := -1;
      end;
      for i:=0 to TablelistColumns.Count-1 do
      begin
        column := ListTables.Columns.Add;
        column.Caption := TablelistColumns[i];
        column.Width := -1;
        column.MinWidth := 50;
        column.Autosize := true;
      end;

      for i := 1 to ZQuery3.RecordCount do
      begin
        n := ListTables.Items.Add;
        n.ImageIndex := 39;
        // Table
        n.Caption := ZQuery3.FieldByName('Name').AsString;
        if popupDbGridHeader.Items[0].Checked then
        begin // Default columns
          // Records
          n.SubItems.Add( FormatNumber( ZQuery3.FieldByName('Rows').AsFloat ) );
          // Size: Data_length + Index_length
          bytes := ZQuery3.FieldByName('Data_length').AsFloat + ZQuery3.FieldByName('Index_length').AsFloat;
          n.SubItems.Add( FormatNumber( bytes / 1024 + 1 ) + ' KB');
          // Created:
          n.SubItems.Add( DateTimeToStr(ZQuery3.FieldByName('Create_time').AsDateTime) );

          // Updated:
          if not ZQuery3.FieldByName('Update_time').IsNull then
            n.SubItems.Add( DateTimeToStr(ZQuery3.FieldByName('Update_time').AsDateTime) )
          else
            n.SubItems.Add('N/A');
            
          // Type
          if ZQuery3.FindField('Type')<>nil then
            n.SubItems.Add( ZQuery3.FieldByName('Type').AsString )
          else if ZQuery3.FindField('Engine')<>nil then
            n.SubItems.Add( ZQuery3.FieldByName('Engine').AsString )
          else
            n.SubItems.Add('');

          // Comment
          n.SubItems.Add( ZQuery3.FieldByName('Comment').AsString );
        end;
        for j:=0 to TablelistColumns.Count-1 do
        begin
          for k:=0 to ZQuery3.FieldCount-1 do
          begin
            if TablelistColumns[j] = ZQuery3.Fields[k].FieldName then
            begin
              if ZQuery3.Fields[k].DataType in [ftInteger, ftSmallint, ftWord, ftFloat, ftWord ] then
              begin
                // Number
                // TODO: doesn't match any column 
                ListTables.Columns[n.SubItems.Count].Alignment := taRightJustify;
                n.SubItems.Add( FormatNumber( ZQuery3.Fields[k].AsFloat ) );
              end
              else
                // String
                n.SubItems.Add( ZQuery3.Fields[k].AsString );
            end;
          end;
        end;
        ZQuery3.Next;
      end;
    end
    else begin
      // get slower results with versions 3.22.xx and older
      ZQuery3.SQL.Clear;
      ZQuery3.SQL.Add('SHOW TABLES');
      ZQuery3.Open;
      ZQuery3.First;
      for i := 1 to ZQuery3.RecordCount do
      begin
        n := ListTables.Items.Add;
        n.Caption := ZQuery3.Fields[0].AsString;
        n.ImageIndex := 39;
        n.SubItems.Add( GetVar( 'SELECT COUNT(*) FROM '+ZQuery3.Fields[0].AsString ) );
        ZQuery3.Next;
      end;
    end;
    mainform.showstatus(ActualDatabase + ': ' + inttostr(ZQuery3.RecordCount) +' table(s)');
  Finally
    ListTables.Columns.EndUpdate;
    ListTables.Items.EndUpdate;
    // Remove existing column-sort-images
    // (TODO: auomatically invoke this method in TSortListView itself)
    ListTables.ClearSortColumnImages;
    Screen.Cursor := crDefault;
  End;
  Screen.Cursor := crHourglass;

  // update dbtree with new/deleted tables
  if DBTree.Selected.Level = 1 then tndb := DBTree.Selected
  else if DBTree.Selected.Level = 2 then tndb := DBTree.Selected.Parent
  else exit;

  // get all tables back into dbtree
  for u:=tndb.Count-1 downto 0 do
    tndb.Item[u].delete;
  for t:=0 to ListTables.Items.Count-1 do
  begin
    with DBtree.Items.AddChild(tndb, ListTables.Items[t].Caption) do begin
      ImageIndex := 39;
      selectedIndex := 40;
    end;
  end;

  Panel2.Caption := 'Database ' + ActualDatabase + ': ' + inttostr(ListTables.Items.Count) + ' table(s)';
  MainForm.ShowStatus( STATUS_MSG_READY, 2 );
  Screen.Cursor := crDefault;
end;



{ Show columns of selected table, indicate indexed columns by certain icons }
procedure TMDIChild.ShowTableProperties(Sender: TObject);
var
  i,j : Integer;
  n : TListItem;
  tn, tndb : TTreeNode;
  isFulltext : Boolean;
begin
  // Table-Properties

  Screen.Cursor := crHourGlass;

  if (PageControlMain.ActivePage <> tabData) and (not DBTree.Dragging) then
    PageControlMain.ActivePage := tabTable;
  Panel3.Caption := 'Table-Properties for ' + ActualDatabase + ': ' + ActualTable;

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

  // and the other way around: set current listitem in tableslist to ActualTable:
  for i:=0 to ListTables.items.Count -1 do
  begin
    // ListTables.Items[i].Selected := (ActualTable = ListTables.Items[i].Caption); // ListTablesOnChange will be called by this line
  end;

  MainForm.ShowStatus( 'Reading table properties...', 2, true );
  ListColumns.Items.BeginUpdate;
  ListColumns.Items.Clear;
  Try
    GetResults( 'SHOW COLUMNS FROM ' + mask(ActualTable), ZQuery3 );
    for i:=1 to ZQuery3.RecordCount do
    begin
      n := ListColumns.Items.Add;
      n.ImageIndex := 62;

      n.Caption := ZQuery3.FieldByName('Field').AsString;
      n.Subitems.Add( ZQuery3.FieldByName('Type').AsString );
      if lowercase( ZQuery3.FieldByName('Null').AsString ) = 'yes' then
        n.Subitems.Add('Yes')
        else n.Subitems.Add('No');
      n.Subitems.Add( ZQuery3.FieldByName('Default').AsString );
      n.Subitems.Add( ZQuery3.FieldByName('Extra').AsString );
      ZQuery3.Next;
    end;

    // add fields to dbtree for drag'n dropping purpose
    if not DBTree.Selected.HasChildren then
    begin
      ZQuery3.First;
      for i:=1 to ZQuery3.RecordCount do begin
        tn := DBtree.Items.AddChild(Dbtree.Selected, ZQuery3.FieldByName('Field').AsString );
        if ZQuery3.FieldByName('Key').AsString = 'PRI' then
          tn.ImageIndex := 26
        else
          tn.ImageIndex := 62;
        tn.SelectedIndex := tn.ImageIndex;
        ZQuery3.Next;
      end;
    end;
  finally
    ListColumns.Items.EndUpdate;
    // Remove existing column-sort-images
    // (TODO: auomatically invoke this method in TSortListView itself)
    ListColumns.ClearSortColumnImages;
    Screen.Cursor := crDefault;
  end;


  Screen.Cursor := crHourglass;
  GetResults( 'SHOW KEYS FROM ' + mask(ActualTable), ZQuery3 );
  for i:=1 to ZQuery3.RecordCount do
  begin
    // primary key
    if ZQuery3.FieldByName('Key_name').AsString = 'PRIMARY' then
    begin
      for j:=0 to ListColumns.Items.Count-1 do
      begin
        if ZQuery3.FieldByName('Column_name').AsString = ListColumns.Items[j].Caption then
        begin
          ListColumns.Items[j].ImageIndex := 26;
          break;
        end;
      end;
    end;

    // index
    if (ZQuery3.FieldByName('Key_name').AsString <> 'PRIMARY')
    	and (ZQuery3.FieldByName('Non_unique').AsString = '1') then
    begin
      for j:=0 to ListColumns.Items.Count-1 do
      begin
        if ZQuery3.FieldByName('Column_name').AsString = ListColumns.Items[j].Caption then
        begin
          if ListColumns.Items[j].ImageIndex = 62 then // Only apply if it's the default image
            ListColumns.Items[j].ImageIndex := 63;
          break;
        end;
      end;
    end;

    // unique
    if (ZQuery3.FieldByName('Key_name').AsString <> 'PRIMARY') and (ZQuery3.FieldByName('Non_unique').AsString = '0') then
    begin
      for j:=0 to ListColumns.Items.Count-1 do
      begin
        if ZQuery3.FieldByName('Column_name').AsString = ListColumns.Items[j].Caption then
        begin
          if ListColumns.Items[j].ImageIndex = 62 then // Only apply if it's the default image
            ListColumns.Items[j].ImageIndex := 64;
          break;
        end;
      end;
    end;

    // column is part of a fulltext key, available since 3.23.xx
    if mysql_version < 40002 then
      isFulltext := (ZQuery3.FieldByName('Comment').AsString = 'FULLTEXT')
    else
      isFulltext := (ZQuery3.FieldByName('Index_type').AsString = 'FULLTEXT');
    if (ZQuery3.FieldByName('Key_name').AsString <> 'PRIMARY') and isFulltext then
    begin
      for j:=0 to ListColumns.Items.Count-1 do
      begin
        if ZQuery3.FieldByName('Column_name').AsString = ListColumns.Items[j].Caption then
        begin
          if ListColumns.Items[j].ImageIndex = 62 then // Only apply if it's the default image
            ListColumns.Items[j].ImageIndex := 65;
          break;
        end;
      end;
    end;
    ZQuery3.Next;
  end;

  MainForm.ShowStatus( STATUS_MSG_READY, 2, false );
  MainForm.showstatus(ActualDatabase + ': '+ ActualTable + ': ' + inttostr(ListColumns.Items.count) +' field(s)');
  Screen.Cursor := crDefault;
end;


function TMDIChild.ExecuteQuery(query: string): TDataSet;
var
  ds: TZReadOnlyQuery;
begin
  ds := TZReadOnlyQuery.Create(nil);
  ds.Connection := MysqlConn.Connection;
  GetResults(query, ds);
  result := ds;
end;


procedure TMDIChild.ListTablesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  i: integer;
begin
  for i := 0 to ListTables.Items.Count - 1 do
    ListTables.Items[i].ImageIndex := 39;
  if (ListTables.Selected <> nil) then ListTables.Selected.ImageIndex := 40;
  ValidateDbActions;
end;

// Enable/disable various buttons and menu items.
// Invoked when active sheet changes or highlighted database changes.
procedure TMDIChild.ValidateDbActions;
var
  tableSelected : Boolean;
begin
  // Make sure that main menu "drop table" affects table selected in tree view,
  // not table (now invisibly) selected on the database grid.
  if (PageControlMain.ActivePage <> tabDatabase) then ListTables.Selected := nil;

  tableSelected := (ListTables.Selected <> nil);
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

  MainForm.ButtonDropDatabase.Enabled := ActualDatabase <> '';
  MainForm.DropTable.Enabled := tableSelected or ((PageControlMain.ActivePage <> tabDatabase) and (ActualTable <> ''));
  MainForm.ButtonCreateTable.Enabled := ActualDatabase <> '';
end;


procedure TMDIChild.ShowTable(Sender: TObject);
var
  i : Integer;
  tn, tndb : TTreeNode;
begin
  // vor viewdata...
  if DBTree.Selected.Level = 1 then tndb := DBTree.Selected
  else if DBTree.Selected.Level = 2 then tndb := DBTree.Selected.Parent
  else exit;

  tn := tndb.getFirstChild;
  for i:=0 to tndb.Count -1 do begin
    if ListTables.Selected.Caption = tn.Text then
    begin
      DBTree.Selected := tn;
      PageControlMain.ActivePage := tabData;
      viewdata(self);
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
  if ListTables.SelCount = 0 then
    exit;
  t := TStringlist.Create;
  with ListTables do
  for i:=0 to Items.count-1 do
    if Items[i].Selected then
      t.add(Items[i].Caption);

  if MessageDlg('Empty ' + inttostr(t.count) + ' Table(s) ?' + crlf + '(' + implodestr(', ', t) + ')', mtConfirmation, [mbok,mbcancel], 0) <> mrok then
    exit;

  Screen.Cursor := crSQLWait;
  for i:=0 to t.count-1 do
    ExecUpdateQuery( 'DELETE FROM ' + mask(t[i]) );
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
          if OpenKey(REGPATH + '\Servers\' + FConnParams.Description, false) then
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
    n : TListItem;
    tmpval : Double;
  begin
    n := ListCommandStats.Items.Add;
    n.ImageIndex := 86;
    caption := copy( caption, 5, length(caption) );
    caption := StringReplace( caption, '_', ' ', [rfReplaceAll] );
    n.Caption := caption;
    // Total Frequency
    n.Subitems.Add( FormatNumber( commandCount ) );
    // Average per hour
    uptime := max(uptime, 1);
    tmpval := commandCount / ( uptime / 60 / 60 );
    n.Subitems.Add( FormatNumber( tmpval, 1 ) );
    // Average per second
    tmpval := commandCount / uptime;
    n.Subitems.Add( FormatNumber( tmpval, 1 ) );
    // Percentage. Take care of division by zero errors and Int64's
    if commandCount < 1 then
      commandCount := 1;
    if totalCount < 1 then
      totalCount := 1;
    tmpval := 100 / totalCount * commandCount;
    n.Subitems.Add( FormatNumber( tmpval, 1 ) + ' %' );
  end;

var
  v : String[10];
  i : Integer;
  n : TListItem;
  versions : TStringList;
  questions : Int64;
begin
  // Refresh variables and process-list
  Screen.Cursor := crSQLWait;

  // VERSION
  v := GetVar( 'SELECT VERSION()' );
  versions := explode( '.', v );
  mysql_version := MakeInt(versions[0]) * 10000 + MakeInt(versions[1]) * 100 + MakeInt(versions[2]);
  strHostRunning := FConnParams.MysqlParams.Host + ' running MySQL-Version ' + v + ' / Uptime: ';


  // VARIABLES
  ListVariables.Items.BeginUpdate;
  ListVariables.Items.Clear;

  GetResults( 'SHOW VARIABLES', ZQuery3 );
  for i:=1 to ZQuery3.RecordCount do
  begin
    n := ListVariables.Items.Add;
    n.ImageIndex := 87;
    n.Caption := ZQuery3.Fields[0].AsString;
    n.Subitems.Add( ZQuery3.Fields[1].AsString );
    ZQuery3.Next;
  end;

  // STATUS
  uptime := 1; // avoids division by zero :)
  questions := 1;
  GetResults( 'SHOW /*!50002 GLOBAL */ STATUS', ZQuery3 );
  for i:=1 to ZQuery3.RecordCount do
  begin
    if LowerCase( copy( ZQuery3.Fields[0].AsString, 1, 4 ) ) <> 'com_' then
    begin
      n := ListVariables.Items.Add;
      n.ImageIndex := 87;
      n.Caption := ZQuery3.Fields[0].AsString;
      n.Subitems.Add( ZQuery3.Fields[1].AsString );
      if lowercase( ZQuery3.Fields[0].AsString ) = 'uptime' then
        uptime := MakeInt(ZQuery3.Fields[1].AsString);
      if lowercase( ZQuery3.Fields[0].AsString ) = 'questions' then
        questions := MakeInt(ZQuery3.Fields[1].AsString);
    end;
    ZQuery3.Next;
  end;
  // Remove existing column-sort-images
  // (TODO: auomatically invoke this method in TSortListView itself)
  ListVariables.ClearSortColumnImages;

  // Command-Statistics
  ListCommandStats.Items.BeginUpdate;
  ListCommandStats.Items.Clear;
  addLVitem( '    All commands', questions, questions );
  ZQuery3.First;
  for i:=1 to ZQuery3.RecordCount do
  begin
    if LowerCase( copy( ZQuery3.Fields[0].AsString, 1, 4 ) ) = 'com_' then
    begin
      addLVitem( ZQuery3.Fields[0].AsString, MakeInt(ZQuery3.Fields[1].AsString), questions );
    end;
    ZQuery3.Next;
  end;
  ListCommandStats.Items.EndUpdate;
  // Sort 2nd column descending
  ListCommandStats.ColClick( ListCommandStats.Columns[1] );
  ListCommandStats.ColClick( ListCommandStats.Columns[1] );

  Timer1Timer(self);
  Timer1.OnTimer := Timer1Timer;

  ListVariables.Items.EndUpdate;
  tabVariables.Caption := 'Variables (' + inttostr(ListVariables.Items.Count) + ')';
  Screen.Cursor := crDefault;

  ShowProcesslist(self); // look at next procedure
end;



procedure TMDIChild.ShowProcessList(sender: TObject);
var
  i,j : Integer;
  n   : TListItem;
begin
  // PROCESSLIST
  Screen.Cursor := crSQLWait;
  try
    ListProcesses.Items.BeginUpdate;
    ListProcesses.Items.Clear;
    ZQuery3.Close;
    ZQuery3.SQL.Clear;
    ZQuery3.SQL.Add( 'SHOW FULL PROCESSLIST' );
    ZQuery3.Open;
    ZQuery3.First;
    for i:=1 to ZQuery3.RecordCount do
    begin
      n := ListProcesses.Items.Add;
      n.Caption := ZQuery3.Fields[0].AsString;
      if CompareText( ZQuery3.Fields[4].AsString, 'Killed') = 0 then
        n.ImageIndex := 83  // killed
      else
        n.ImageIndex := 82; // running
      for j := 1 to 7 do
        n.Subitems.Add(ZQuery3.Fields[j].AsString);
      ZQuery3.Next;
    end;
    ZQuery3.Close;
    ListProcesses.Items.EndUpdate;
    // Remove existing column-sort-images
    // (TODO: auomatically invoke this method in TSortListView itself)
    ListProcesses.ClearSortColumnImages;
    tabProcessList.Caption := 'Process-List (' + inttostr(ListProcesses.Items.Count) + ')';
  except
    LogSQL( 'Error on loading process-list!' );
  end;
  ListProcesses.Items.EndUpdate;
  Screen.Cursor := crDefault;
end;


procedure TMDIChild.ListProcessesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  Kill1.Enabled := (ListProcesses.Selected <> nil) and (PageControlHost.ActivePage = tabProcessList);
end;


procedure TMDIChild.KillProcess(Sender: TObject);
var t : boolean;
begin
  if ListProcesses.Selected.Caption = IntToStr( MySQLConn.Connection.GetThreadId ) then
    MessageDlg('Fatal: Better not kill my own Process...', mtError, [mbok], 0)
  else begin
    t := TimerProcessList.Enabled;
    TimerProcessList.Enabled := false; // prevent av (ListProcesses.selected...)
    if MessageDlg('Kill Process '+ListProcesses.Selected.Caption+'?', mtConfirmation, [mbok,mbcancel], 0) = mrok then
    begin
      ExecUpdateQuery( 'KILL '+ListProcesses.Selected.Caption );
      ShowVariablesAndProcesses(self);
    end;
    TimerProcessList.Enabled := t; // re-enable autorefresh timer
  end;
end;


procedure TMDIChild.PageControlHostChange(Sender: TObject);
begin
  ListProcessesChange(self, nil, TItemChange(self));
end;





procedure TMDIChild.ExecSQLClick(Sender: TObject; Selection: Boolean=false; CurrentLine: Boolean=false);
var
  SQL                     : TStringList;
  i, rowsaffected         : Integer;
  SQLstart, SQLend, SQLscriptstart,
  SQLscriptend            : Integer;
  SQLTime                 : Double;
  fieldcount, recordcount : Integer;
begin
  if CurrentLine then begin
    // Run current line
    SQL := parseSQL(SynMemoQuery.LineText);
  end else if Selection then begin
    // Run selection
    SQL := parsesql(SynMemoQuery.SelText);
  end else begin
    // Run all
    SQL := parsesql(SynMemoQuery.Text);
  end;

  if SQL.Count = 0 then begin
    LabelResultinfo.Caption := '(nothing to do)';
    exit;
  end;

  SQLscriptstart := GetTickCount;
  LabelResultinfo.Caption := '';

  try
    CheckConnection;
  except
    exit;
  end;

  TRY
    MainForm.showstatus('Initializing SQL...', 2, true);
    Mainform.ExecuteQuery.Enabled := false;
    Mainform.ExecuteSelection.Enabled := false;

    if ActualDatabase <> '' then
      FMysqlConn.Connection.Database := ActualDatabase;
    ZQuery1.Active := false;
    ZQuery1.DisableControls;

    rowsaffected := 0;
    fieldcount := 0;
    recordcount := 0;
    ProgressBarQuery.Max := SQL.Count;
    ProgressBarQuery.Position := 0;
    ProgressBarQuery.show;

    MainForm.showstatus('Executing SQL...', 2, true);
    for i:=0 to SQL.Count-1 do
    begin
      ProgressBarQuery.Stepit;
      ProgressBarQuery.Repaint;
      if sql[i] = '' then
        continue;
      // open last query with data-aware:
      LabelResultinfo.Caption := '';
      ZQuery1.Close;
      ZQuery1.SQL.Clear;
      ZQuery1.SQL.Add(SQL[i]);
      // set db-aware-component's properties..
      DBMemo1.DataField := '';
      DBMemo1.DataSource := DataSource2;
      EDBImage1.DataField := '';
      EDBImage1.DataSource := DataSource2;
      // ok, let's rock
      SQLstart := GetTickCount;

      try
        if ExpectResultSet( copy( SQL[i], 0, 20 ) ) then
        begin
          ZQuery1.Open;
          fieldcount := ZQuery1.Fieldcount;
          recordcount := ZQuery1.Recordcount;
        end
        else
        begin
          ZQuery1.ExecSql;
          fieldcount := 0;
          recordcount := 0;
        end;
      except
        on E:Exception do
        begin
          if btnQueryStopOnErrors.Down or (i=SQL.Count-1) then begin
            Screen.Cursor := crdefault;
            LogSQL(E.Message, true);
            MessageDLG(E.Message, mtError, [mbOK], 0);
            ProgressBarQuery.hide;
            Mainform.ExecuteQuery.Enabled := true;
            Mainform.ExecuteSelection.Enabled := true;
            break;
          end
          else LogSQL(E.Message, true);
        end;
      end;
      rowsaffected := rowsaffected + ZQuery1.RowsAffected;
      SQLend := GetTickCount;
      SQLTime := (SQLend - SQLstart) / 1000;

      LabelResultinfo.Caption :=
        FormatNumber( rowsaffected ) + ' row(s) affected, ' +
        FormatNumber( fieldcount ) + ' field(s) / ' +
        FormatNumber( recordcount ) + ' record(s) in last result set.';
      if SQL.Count = 1 then begin
        LabelResultinfo.Caption := LabelResultinfo.Caption +
          ' Query time: '+FormatNumber( SQLTime, 3)+' sec.';
      end;
    end;
    ProgressBarQuery.hide;
    Mainform.ExecuteQuery.Enabled := true;
    Mainform.ExecuteSelection.Enabled := true;
    // count chars:
    SynMemoQuery.OnChange(self);

    if SQL.Count > 1 then begin
      SQLscriptend := GetTickCount;
      SQLTime := (SQLscriptend - SQLscriptstart) / 1000;
      LabelResultinfo.Caption := LabelResultinfo.Caption + ' Batch time: ' + FormatNumber(SQLTime, 3)+' sec.';
    end;



  FINALLY
    ZQuery1.EnableControls;
    // resize all columns, if they are more wide than Mainform.DefaultColWidth
    if Mainform.DefaultColWidth <> 0 then
      for i:=0 to gridQuery.Columns.count-1 do
        if gridQuery.Columns[i].Width > Mainform.DefaultColWidth then
          gridQuery.Columns[i].Width := Mainform.DefaultColWidth;
    Screen.Cursor := crdefault;
	  MainForm.ShowStatus( STATUS_MSG_READY, 2 );
  END;
end;




procedure TMDIChild.ListColumnsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  // Clicked somewhere in the field-list of the "Table"-tabsheet

  // some columns selected ?
  if ListColumns.Selected <> nil then with ListColumns.Selected do
  begin
    btnTableDropField.Enabled := True;
    DropField1.Enabled := True;
    MenuEditField.Enabled := true;
    btnTableEditField.enabled := true;
  end
  else
  begin
    btnTableDropField.Enabled := False;
    DropField1.Enabled := false;
    MenuEditField.Enabled := false;
    btnTableEditField.enabled := false;
  end;

end;


procedure TMDIChild.DropField(Sender: TObject);
var
  tn : TTreeNode;
begin
  // Drop Column
  if ListColumns.Items.Count = 1 then
  begin
    if MessageDlg('Can''t drop the last Field - drop Table '+ActualTable+'?', mtConfirmation, [mbok,mbcancel], 0) = mrok then
    begin
      Screen.Cursor := crSQLWait;
      ExecUpdateQuery( 'DROP TABLE '+mask(ActualTable) );
      tn := DBTree.Selected;
      DBTree.Selected := DBTree.Selected.Parent;
      tn.Destroy;
      ShowDBProperties(self);
      Screen.Cursor := crDefault;
    end;
  end else
  if MessageDlg('Drop field ' + ListColumns.Selected.Caption + ' ?', mtConfirmation, [mbok,mbcancel], 0) = mrok then
  begin
    ExecUpdateQuery( 'ALTER TABLE '+mask(ActualTable)+' DROP '+mask(ListColumns.Selected.Caption) );
    ShowTableProperties(self);
  end;
end;


{ Proposal about to insert a string into synmemo }
procedure TMDIChild.SynCompletionProposal1CodeCompletion(Sender: TObject;
  var Value: string; Shift: TShiftState; Index: Integer; EndToken: Char);
begin
  SynCompletionProposal1.Editor.UndoList.AddGroupBreak;
end;


procedure TMDIChild.SynCompletionProposal1AfterCodeCompletion(Sender: TObject;
  const Value: string; Shift: TShiftState; Index: Integer; EndToken: Char);
begin
  SynCompletionProposal1.Editor.UndoList.AddGroupBreak;
end;


{ Proposal-Combobox pops up }
procedure TMDIChild.SynCompletionProposal1Execute(Kind: TSynCompletionType;
  Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);
var
  i,j,c,t          : Integer;
  functionname     : String;
  functiondecl     : String;
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
  begin
    dbname := ActualDatabase;
    if pos( '.', tablename ) > -1 then
    begin
      dbname := copy( tablename, 0, pos( '.', tablename )-1 );
      tablename := copy( tablename, pos( '.', tablename )+1, length(tablename) );
    end;
    tablename := mask( tablename );
    if dbname <> '' then
      tablename := mask( dbname ) + '.' + tablename;
    getResults( 'SHOW COLUMNS FROM '+tablename, ZQuery3, true );
    if not ZQuery3.Active then
      exit;
    for i:=0 to ZQuery3.RecordCount-1 do
    begin
      SynCompletionProposal1.InsertList.Add( ZQuery3.FieldByName( 'Field' ).AsString );
      SynCompletionProposal1.ItemList.Add( '\hspace{2}\color{'+ColorToString(clTeal)+'}column\color{clWindowText}\column{}' + ZQuery3.FieldByName( 'Field' ).AsString + '\style{-B} ' + ZQuery3.FieldByName( 'Type' ).AsString );
      ZQuery3.Next;
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
    kw := copy( sql, i, pos( ' ', copy( sql, i, Length(sql) ) )-1 );
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
      kw := copy( sql, i, pos( ' ', copy( sql, i, Length(sql) ) )-1 );
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
    tables.CommaText := StringReplace( copy( sql, t, length(sql)), ' ', ',', [rfReplaceall] );
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


  if length(CurrentInput) = 0 then // makes only sense if the user has typed "database."
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

  if (SynCompletionProposal1.ItemList.count = 0) and (length(CurrentInput)>0) then
  begin
    // Add databases
    SynCompletionProposal1.InsertList.AddStrings( OnlyDBs2 );
    SynCompletionProposal1.ItemList.AddStrings( OnlyDBs2 );
    for i:=0 to SynCompletionProposal1.ItemList.count-1 do
      SynCompletionProposal1.ItemList[i] := '\hspace{2}\color{'+ColorToString(SynSQLSyn1.TableNameAttri.Foreground)+'}database\color{clWindowText}\column{}' + SynCompletionProposal1.ItemList[i];

    if ActualDatabase <> '' then
    begin
      // Add tables
      for i:=0 to ListTables.Items.Count-1 do
      begin
        addTable( ListTables.Items[i].Caption );
      end;
      if length(CurrentInput) = 0 then // assume that we have already a dbname in memo
        SynCompletionProposal1.Position := OnlyDBs2.Count;
    end;

    // Add functions
    for i := 0 to MainForm.sqlfunctionlist.Count - 1 do
    begin
      functionname := copy(MainForm.sqlfunctionlist[i], 0, pos('(', MainForm.sqlfunctionlist[i])-1);
      if pos( '|', MainForm.sqlfunctionlist[i] ) > 0 then
        functiondecl := copy(MainForm.sqlfunctionlist[i], length(functionname)+1, pos( '|', MainForm.sqlfunctionlist[i] )-length(functionname)-1)
      else
        functiondecl := copy(MainForm.sqlfunctionlist[i], length(functionname)+1, length(MainForm.sqlfunctionlist[i]) );
      SynCompletionProposal1.InsertList.Add( functionname + functiondecl );
      SynCompletionProposal1.ItemList.Add( '\hspace{2}\color{'+ColorToString(SynSQLSyn1.FunctionAttri.Foreground)+'}function\color{clWindowText}\column{}' + functionname + '\style{-B}' + functiondecl );
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
  PanelCharsInQueryWindow.Caption := FormatNumber( length(SynMemoQuery.Text) ) + ' Characters';
  somechars := length(SynMemoQuery.Text) > 0;
  Mainform.ExecuteQuery.Enabled := somechars;
  Mainform.ExecuteSelection.Enabled := length(SynMemoQuery.SelText) > 0;
  Mainform.ExecuteLine.Enabled := SynMemoQuery.LineText <> '';
  btnQuerySave.Enabled := somechars;
  btnQuerySaveSnippet.Enabled := somechars;
end;



procedure TMDIChild.CreateTable(Sender: TObject);
begin
  CreateTableForm.showmodal;
end;


procedure TMDIChild.Timer1Timer(Sender: TObject);
var
  tage, stunden, minuten, sekunden : Integer;
begin
  // Host-Uptime
  tage:= uptime div (60*60*24);
  sekunden := uptime mod (60*60*24);
  stunden := sekunden div (60*60);
  sekunden := sekunden mod (60*60);
  minuten  := sekunden div 60;
  sekunden := sekunden mod 60;

  inc(uptime);
  Panel4.Caption := format(strHostRunning + '%d days, %.2d:%.2d:%.2d', [tage,stunden,minuten,sekunden])
end;


procedure TMDIChild.FormActivate(Sender: TObject);
begin
  if FMysqlConn.IsConnected then
  begin
    with MainForm do
    begin
      ButtonRefresh.Enabled := true;
      ButtonReload.Enabled := true;
      ExportTables.Enabled := true;
      ButtonImportTextfile.Enabled := true;
      ButtonCreateTable.Enabled := true;
      ButtonCreateDatabase.Enabled := true;
      ButtonDropDatabase.Enabled := false;
      MenuRefresh.Enabled := true;
      MenuExport.Enabled := true;
      MenuImportTextFile.Enabled := true;
      MenuCreateTable.Enabled := true;
      MenuCreateDatabase.Enabled := true;
      MenuDropDatabase.Enabled := true;
      DropTable.Enabled := true;
      LoadSQL.Enabled := true;
      MenuFlushHosts.Enabled := true;
      MenuFlushLogs.Enabled := true;
      FlushUserPrivileges1.Enabled := true;
      MenuFlushTables.Enabled := true;
      MenuFlushTableswithreadlock.Enabled := true;
      MenuFlushStatus.Enabled := true;
      UserManager.Enabled := true;
      Diagnostics.Enabled := true;
      InsertFiles.Enabled := true;
      PrintList.Enabled := true;
      if (PageControlMain.ActivePage = tabData) or
        (PageControlMain.ActivePage = tabQuery) then begin
        Copy2CSV.Enabled := true;
        CopyHTMLtable.Enabled := true;
        Copy2XML.Enabled := true;
        ExportData.Enabled := true;
      end;
      ToolBarData.visible := (PageControlMain.ActivePage = tabData);
      DBNavigator1.DataSource := DataSource1;
      //DBtreeChange( self, DBTree.Selected );
      btnSQLHelp.Enabled := (mysql_version >= 40100);
      menuSQLHelp.Enabled := btnSQLHelp.Enabled;
      menuLoad.OnClick := self.btnQueryLoadClick;
      menuSave.OnClick := self.btnQuerySaveClick;
    end;
  end;
  TimerConnected.OnTimer(self);

  mainform.MenuUserManager.Enabled := CanAcessMysqlFlag;
  mainform.ButtonUserManager.Enabled := CanAcessMysqlFlag;
  if not CanAcessMysqlFlag then
  begin
    mainform.MenuUserManager.Hint := 'you have no access to the privilege-tables';
    mainform.ButtonUserManager.Hint := 'you have no access to the privilege-tables';
  end;
  // Otherwise leave the default hint
end;

procedure TMDIChild.FormDeactivate(Sender: TObject);
begin
  with MainForm do
  begin
    ButtonRefresh.Enabled := false;
    ButtonReload.Enabled := false;
    ExportTables.Enabled := false;
    ButtonImportTextfile.Enabled := false;
    ButtonCreateTable.Enabled := false;
    ButtonCreateDatabase.Enabled := false;
    ButtonDropDatabase.Enabled := false;
    MenuRefresh.Enabled := false;
    MenuExport.Enabled := false;
    MenuImportTextFile.Enabled := false;
    MenuCreateTable.Enabled := false;
    MenuCreateDatabase.Enabled := false;
    MenuDropDatabase.Enabled := false;
    DropTable.Enabled := false;
    MenuFlushHosts.Enabled := false;
    MenuFlushLogs.Enabled := false;
    FlushUserPrivileges1.Enabled := false;
    MenuFlushTables.Enabled := false;
    MenuFlushTableswithreadlock.Enabled := false;
    MenuFlushStatus.Enabled := false;
    UserManager.Enabled := false;
    Diagnostics.Enabled := false;
    InsertFiles.Enabled := false;
    PrintList.Enabled := false;
    Copy2CSV.Enabled := false;
    CopyHTMLtable.Enabled := false;
    Copy2XML.Enabled := false;
    ExportData.Enabled := false;
    LoadSQL.Enabled := false;
    btnSQLHelp.Enabled := false;
    menuSQLHelp.Enabled := false;
    menuLoad.OnClick := nil;
    menuSave.OnClick := nil;
  end;
  MainForm.showstatus('', 1); // empty connected_time
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

  ZQuery3.DisableControls;
  FormResize( self );
end;


{ Edit field }
procedure TMDIChild.UpdateField(Sender: TObject);
var
  fn : String;
  fem : TFieldEditorMode;
begin
  fn := '';
  fem := femFieldAdd;

  if ListColumns.Selected<>nil then
    fn := ListColumns.Selected.Caption;

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
          if OpenKey(REGPATH + '\Servers\' + FConnParams.Description, false) then
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
  tbl_properties_form.showmodal;
end;


{***
  Rename table after checking the new name for invalid characters
}
procedure TMDIChild.ListTablesEdited(Sender: TObject; Item: TListItem;
  var S: String);
var
  i : Integer;
begin
  try
    ensureValidIdentifier( S );
  except
    On E : Exception do
    begin
      MessageDlg( E.Message, mtError, [mbOK], 0 );
      abort;
    end;
  end;
  
  // rename table
  ExecUpdateQuery( 'ALTER TABLE ' + mask(Item.Caption) + ' RENAME ' + mask(S) );
  i := SynSQLSyn1.TableNames.IndexOf( Item.Caption );
  if i > -1 then
    SynSQLSyn1.TableNames[i] := S;
  ActualTable := S;
  ShowDBProperties(self);
  // re-select same item
  for i:=0 to ListTables.Items.Count-1 do
    if ListTables.Items[i].Caption = S then
      break;
  ListTables.Selected := ListTables.Items[i];
  ListTables.Items[i].Focused := true;
  // Important! Otherwise OnEdited refreshes list too:
  abort;
end;


procedure TMDIChild.MenuRenameTableClick(Sender: TObject);
begin
  // menuitem for edit table-name
  ListTables.Selected.EditCaption;
end;


procedure TMDIChild.MenuViewBlobClick(Sender: TObject);
begin
  PageControlBottom.ActivePageIndex := 1;
end;


procedure TMDIChild.TimerConnectedTimer(Sender: TObject);
var
  hours, minutes, seconds : Integer;
begin
  inc(time_connected);

  if Mainform.ActiveMDIChild = self then
  begin
    // calculate and display connection-time
    seconds := time_connected mod (60*60*24);
    hours := seconds div (60*60);
    seconds := seconds mod (60*60);
    minutes := seconds div 60;
    seconds := seconds mod 60;
    MainForm.showstatus( format('Connected: %.2d:%.2d:%.2d', [hours, minutes, seconds]), 1 );
  end;
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
  tablecomment.showmodal;
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
begin
  // Optimize tables
  Screen.Cursor := crHourGlass;
  try
    for i:=0 to ListTables.Items.Count - 1 do
    begin
      if ListTables.Items[i].Selected then
        ExecUpdateQuery( 'OPTIMIZE TABLE ' + mask(ListTables.Items[i].Caption) );
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;


procedure TMDIChild.MenuCheckClick(Sender: TObject);
var
  i : Integer;
  tables : String;
begin
  // Check tables
  Screen.Cursor := crHourGlass;
  try
    tables := '';
    for i:=0 to ListTables.Items.Count - 1 do
      if ListTables.Items[i].Selected then begin
        if tables <> '' then
          tables := tables + ', ';
        tables := tables + mask(ListTables.Items[i].Caption);
      end;
    ExecUpdateQuery( 'CHECK TABLE ' + tables + ' QUICK' );
  finally
    Screen.Cursor := crDefault;
  end;
end;


procedure TMDIChild.MenuAnalyzeClick(Sender: TObject);
var
  i : Integer;
  tables : String;
begin
  // Analyze tables
  Screen.Cursor := crHourGlass;
  try
    tables := '';
    for i:=0 to ListTables.Items.Count - 1 do
      if ListTables.Items[i].Selected then begin
        if tables <> '' then
          tables := tables + ', ';
        tables := tables + mask(ListTables.Items[i].Caption);
      end;
    ExecUpdateQuery( 'ANALYZE TABLE ' + tables );
  finally
    Screen.Cursor := crDefault;
  end;
end;


procedure TMDIChild.MenuRepairClick(Sender: TObject);
var
  i : Integer;
  tables : String;
begin
  // Repair tables
  Screen.Cursor := crHourGlass;
  try
    tables := '';
    for i:=0 to ListTables.Items.Count - 1 do
      if ListTables.Items[i].Selected then begin
        if tables <> '' then
          tables := tables + ', ';
        tables := tables + mask(ListTables.Items[i].Caption);
      end;
    ExecUpdateQuery( 'REPAIR TABLE ' + tables + ' QUICK' );
  finally
    Screen.Cursor := crDefault;
  end;
end;


procedure TMDIChild.ListTablesDblClick(Sender: TObject);
begin
  // table-doubleclick
  if ListTables.Selected <> nil then begin
    SelectTable(ActualDatabase, ListTables.Selected.Caption);
  end;
end;


procedure TMDIChild.Timer5Timer(Sender: TObject);
begin
  // can't connect -> close MDI-Child
  timer5.Enabled := false;
  Mainform.Showstatus('', 1);
  MainForm.ShowStatus( STATUS_MSG_READY, 2 );
  close;
end;


procedure TMDIChild.gridDataTitleClick(Column: TColumn);
var
  Grid : TSMDBGrid;
  i  : Integer;
  existed : Boolean;
begin
  // column-title clicked -> generate "ORDER BY"

  Grid := Column.Grid as TSMDBGrid;
  Grid.DataSource.DataSet.DisableControls;
  existed := false;

  for i:=Grid.SortColumns.Count-1 downto 0 do
  begin
    with Grid.SortColumns[i] do
    begin
      if FieldName <> column.FieldName then
        continue;
      existed := true;
      case SortType of
        stDescending : SortType := stAscending;
        stAscending : Grid.SortColumns.Delete(i);
        stNone : SortType := stDescending;
      end;
    end;
  end;

  {add a new sorted column in list - ascending order}
  if not existed then with Grid.SortColumns.Add do
  begin
    FieldName := column.FieldName;
    SortType := stDescending
  end;

  Grid.DataSource.DataSet.EnableControls;
  viewdata(self);
end;



procedure TMDIChild.Filter1Click(Sender: TObject);
begin
  // Set WHERE-Filter
  PageControlBottom.ActivePageIndex := 2;
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
  Search: string;
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
  Search: string;
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
  if MessageDLG('Delete '+inttostr(gridData.SelectedRows.count)+' Record(s)?', mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
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
  PageControlBottom.ActivePageIndex := 2;
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
begin
  if ListTables.Selected <> nil then
  begin
    SelectTable(ActualDatabase, ListTables.Selected.Caption);
  end;
end;

procedure TMDIChild.PageControl4Change(Sender: TObject);
begin
  btnBlobCopy.Enabled := true;
  btnBlobLoad.Enabled := not DBMemo1.ReadOnly;
  btnBlobSave.Enabled := true;
  if PageControl4.ActivePage = Tabsheet3 then
  begin
    // MEMO tab activated.
    btnBlobWordWrap.Enabled := true;
  end;
  if PageControl4.ActivePage = Tabsheet4 then
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
  if length(DBMemo1.DataField) = 0 then exit;
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
    case PageControl4.ActivePageIndex of
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
      case PageControl4.ActivePageIndex of
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
    case PageControl4.ActivePageIndex of
      0 : Filter := 'Textfiles (*.txt)|*.txt|All files (*.*)|*.*';
      1 : Filter := 'All Images (*.jpg, *.jpeg, *.bmp)|*.jpg;*.jpeg;*.bmp|JPEG (*.jpg, *.jpeg)|*.jpg;*.jpeg|Bitmap (*.bmp)|*.bmp|All files (*.*)|*.*';
    end;

    if execute then
    case PageControl4.ActivePageIndex of
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
          inc(i);
        _filename := copy(_filename, i+1, length(_filename));
        _filename := stringreplace(_filename, '&', '', [rfReplaceAll]);
        if _filename = FileName then
          dontadd := true;
      end;

      if not dontadd then begin
        with TRegistry.Create do begin
          openkey(REGPATH, true);
          for i:=1 to 10 do begin
            if not ValueExists('SQLWhereFile'+inttostr(i)) then
              break;
          end;
          while i > 1 do begin
            WriteString('SQLWhereFile'+inttostr(i), ReadString('SQLWhereFile'+inttostr(i-1)));
            dec(i);
          end;
          WriteString('SQLWhereFile1', FileName);

          i := 1;
          popupTreeView.Items.Clear;
          while ValueExists('SQLWhereFile'+inttostr(i)) do begin
            menuitem := Tmenuitem.Create(self);
            menuitem.Caption := inttostr(popupFilterOpenFile.Items.count+1) + ' ' + ReadString('SQLWhereFile'+inttostr(i));
            menuitem.OnClick := LoadSQLWhereFile;
            popupFilterOpenFile.Items.Add(menuitem);
            inc(i);
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
    reg.openkey( REGPATH + '\Servers\' + FConnParams.Description, false );
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
    inc(i);
  filename := copy(filename, i+1, length(filename));
  filename := stringreplace(filename, '&', '', [rfReplaceAll]);

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
begin
  tabletype := (Sender as TMenuItem).Caption;
  tabletype := StringReplace( tabletype, '&', '', [rfReplaceAll] ); // Remove Auto-Hotkey
  for i:=0 to ListTables.Items.Count - 1 do
    if ListTables.Items[i].Selected then
      ExecUpdateQuery( 'ALTER TABLE ' + mask(ListTables.Items[i].Caption) + ' TYPE = ' + tabletype);
  ShowDBProperties(self);
end;

procedure TMDIChild.MenuChangeTypeOtherClick(Sender: TObject);
var
  i : Integer;
  strtype : String;
begin
  // change table-type:
  if inputquery('Change table-type...','New table-type:', strtype) then begin
    for i:=0 to ListTables.Items.Count - 1 do
      if ListTables.Items[i].Selected then
        ExecUpdateQuery( 'ALTER TABLE ' + mask(ListTables.Items[i].Caption) + ' TYPE = ' + strtype );
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
var i : Integer;
begin
  for i:=0 to ListTables.Items.count-1 do
    ListTables.Items[i].Selected := true;
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

procedure TMDIChild.gridDataMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ed: TCustomEdit;
begin
  if Button = mbRight then begin
    // Select text in in-place editor to make popup menu copy and paste work.
    (Sender as TDBGrid).EditorMode := true;
    ed := TCustomEdit(FindControl(GetFocus()));
    ed.SelectAll;
    // Popup menu manually, mucking about with the grid causes it to stop doing it by itself.
    popupDataGrid.Popup(X + 178, Y + 142);
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
      MessageDLG('Seconds must be between 1 and ' + inttostr(maxint) + '.', mtError, [mbOK], 0);
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
begin
  // dragging an object over the query-memo
  if (Source as TControl).Parent = DBTree then
    accept := true;
  // set x-position of cursor
  SynMemoQuery.CaretX := (x - SynMemoQuery.Gutter.Width) div SynMemoQuery.CharWidth - 1 + SynMemoQuery.LeftChar;
  // set y-position of cursor
  SynMemoQuery.CaretY := y div SynMemoQuery.LineHeight + SynMemoQuery.TopLine;
  if not SynMemoQuery.Focused then
    SynMemoQuery.SetFocus;
end;


procedure TMDIChild.SynMemoQueryDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  // dropping a TTreeNode into the query-memo
  SynMemoQuery.UndoList.AddGroupBreak;
  SynMemoQuery.SelText := DBTree.Selected.Text;
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
  MenuAutoupdate.Enabled := PageControlHost.ActivePageIndex=1;
end;

procedure TMDIChild.ListTablesEditing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  // so that one can press DEL when editing
  menudroptable.ShortCut := TextToShortCut('');
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
  MainForm.DropTable.Enabled := DBtree.Selected.Level = 2;
  DBRightClickSelectedItem := DBtree.Selected;
end;


procedure TMDIChild.btnQuerySaveSnippetClick(Sender: TObject);
var
  snippetname : String;
  sfile       : Textfile;
begin
  // Save snippet
  if InputQuery( 'Save snippet', 'Snippet name:', snippetname) then
  begin
    if copy( snippetname, length(snippetname)-4, 4 ) <> '.sql' then
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
    Write( sfile, SynMemoQuery.Text );
    CloseFile( sfile );
    FillPopupQueryLoad;
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
    and (gridData.Col <= ListColumns.Items.Count) then
  begin
    keyword := ListColumns.Items[gridData.Col-1].SubItems[0];
  end
  // Table-Tab
  else if ListColumns.Focused and (ListColumns.Selected <> nil) then
  begin
    keyword := ListColumns.Selected.SubItems[0];
  end;
  // Clean existing paranthesis, fx: char(64)
  if pos( '(', keyword ) > 0 then
  begin
    keyword := copy( keyword, 1, pos( '(', keyword )-1 );
  end;

  // Show the window
  CallSQLHelpWithKeyword( keyword );
end;



{***
  Show SQL Help window directly using a keyword
  @param string SQL-keyword
  @see FieldeditForm.btnDatatypeHelp
}
procedure TMDIChild.CallSQLHelpWithKeyword( keyword: String );
begin
  // Set help-keyword and show window
  frmSQLhelp.keyword := keyword;
  frmSQLhelp.Show;
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
    Write(f, SynMemoQuery.Text);
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
  if pos( '\', filename ) = 0 then
  begin // assuming we load a snippet
    filename := DIRNAME_SNIPPETS + filename;
  end
  else
  begin // assuming we load a file from the recent-list
    p := pos( ' ', filename ) + 1;
    filename := copy(filename, p, length(filename));
  end;
  filename := stringreplace(filename, '&', '', [rfReplaceAll]);
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
    if pos( 'SQLFile', Values[i] ) <> 1 then
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
    reg.WriteString( 'SQLFile'+inttostr(i), newfilelist[i] );
  end;

  reg.Free;
end;


procedure TMDIChild.QueryLoad( filename: String; ReplaceContent: Boolean = true );

var
  tmpstr, filecontent      : String;
  f                        : TextFile;
begin
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

  if pos( DIRNAME_SNIPPETS, filename ) = 0 then
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
  snippets := getFilesFromDir( DIRNAME_SNIPPETS, '*.sql' );
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
      if not ValueExists('SQLFile'+inttostr(i)) then
        continue;
      sqlFilename := ReadString( 'SQLFile'+inttostr(i) );
      inc(j);
      menuitem := TMenuItem.Create( popupQueryLoad );
      menuitem.Caption := inttostr(j) + ' ' + sqlFilename;
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

procedure TMDIChild.DBGridDblClick(Sender: TObject);
begin
  // If grid is not empty...
  if (Sender as TDBGrid).SelectedField <> nil then begin
    // Set focus on DBMemo when user doubleclicks a (MEMO)-cell
    if (sender as TSMDBGrid).SelectedField.IsBlob and (PageControl4.ActivePageIndex = 0) then begin
      PageControlBottom.ActivePageIndex := 1;
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
  if field.IsNull then background := mainform.DataNullBackground;
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
  delete(d, pos('&', d), 1);
  DataSource1.Edit;
  gridData.SelectedField.AsString := d;
end;

procedure TMDIChild.btnBlobCopyClick(Sender: TObject);
begin
  if dbmemo1.DataField = '' then exit;
  SaveBlob;
  case PageControl4.ActivePageIndex of
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
  // Display row count and filter-matchings above dbgrid
  DisplayRowCountStats;

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
end;

procedure TMDIChild.ZQueryGridBeforeClose(DataSet: TDataSet);
begin
  // unassign data-aware controls
  DBMemo1.DataField := '';
  EDBImage1.DataField := '';
end;


function TMDIChild.ExecUseQuery (ADatabase : String) : Boolean;
begin
  FConnParams.MysqlParams.Database := ADatabase;
  Result := ExecUpdateQuery('USE ' + mask(ADatabase));
end;


{***
  Execute a query without returning a resultset
  The currently active connection is used

  @param String The single SQL-query to be executed on the server
  @return Boolean Return True on success, False otherwise
}
function TMDIChild.ExecUpdateQuery(ASQLQuery: String ) : Boolean;
var
  MysqlQuery : TMysqlQuery;
begin
  // Start query execution
  MysqlQuery := RunThreadedQuery(ASQLQuery);

  // Inspect query result code and log / notify user on failure
  if MysqlQuery.Result in [MQR_CONNECT_FAIL,MQR_QUERY_FAIL] then
  begin
    MessageDlg( MysqlQuery.Comment, mtError, [mbOK], 0 );
    LogSql( MysqlQuery.Comment, True );
  end;

  // Get thread result code and convert into function return value
  Result := (MysqlQuery.Result = MQR_SUCCESS);

  // Cleanup the MysqlQuery object, we won't need it anymore
  FreeAndNil (MysqlQuery);

end;


{***
  Execute a query which may return a resultset. The caller is responsible for
  freeing the MysqlQuery object and its Dataset member, only on returnvalue True.
  The currently active connection is used

  @param String The single SQL-query to be executed on the server
  @param TMysqlQuery Containing the dataset and info data availability
  @return Boolean Return True on success, False otherwise
}
function TMDIChild.ExecSelectQuery(AQuery: String;
  out AMysqlQuery : TMysqlQuery): Boolean;
var
  MysqlQuery : TMysqlQuery;
begin
  Result := False;
  AMysqlQuery := nil;

  // Start query execution
  MysqlQuery := RunThreadedQuery(AQuery);

  // Inspect query result code and log / notify user on failure
  if MysqlQuery.Result in [MQR_CONNECT_FAIL,MQR_QUERY_FAIL] then
  begin
    MessageDlg( MysqlQuery.Comment, mtError, [mbOK], 0 );
    LogSql( MysqlQuery.Comment, True );
  end;

  if MysqlQuery.Result = MQR_SUCCESS then
    begin
      Result := MysqlQuery.HasResultset; // Report success
      AMysqlQuery := MysqlQuery;
    end
  else
    begin
      FreeAndNil (MysqlQuery)
    end
end;







{***
  Executes a query with an existing ZQuery-object

  @note This freezes the user interface
}
procedure TMDIChild.GetResults( SQLQuery: String; ZQuery: TZReadOnlyQuery; QuietOnError: Boolean = false );
begin
  // todo: convert it to a function to get info on result
  // todo: move to separate unit

  try
    CheckConnection;
  except
    exit;
  end;
  ZQuery.SQL.Text := SQLQuery;
  try
    ZQuery.Open;
  except
    on E:Exception do
    begin
      if not QuietOnError then
        MessageDlg( E.Message, mtError, [mbOK], 0 );
      LogSQL( E.Message );
      exit;
    end;
  end;
  ZQuery.DisableControls;
  ZQuery.First;
end;


{***
  Execute a query and return string from column x

  @note This freezes the user interface
}
function TMDIChild.GetVar( SQLQuery: String; x: Integer = 0 ) : String;
begin
  GetResults( SQLQuery, ZQuery3 );
  Result := ZQuery3.Fields[x].AsString;
  ZQuery3.Close;
end;


{***
  This returns the dataset object that is currently visible to the user,
  depending on with tabsheet is active.

  @return TDataset if data/query tab is active, nil otherwise.
}
function TMDIChild.GetVisualDataset: TDataSet;
begin

  case PageControlMain.ActivePageIndex of
    3: Result := FCurDataset;
    4: Result := ZQuery1;
  else
    Result := nil;
  end;
end;


{***
  Execute a query and return column x as Stringlist

  @param  String SQL query string
  @param  Integer 0-based column index in the resultset to return
  @return TStringList
  @note   This freezes the user interface
}
function TMDIChild.GetCol( SQLQuery: String; x: Integer = 0 ) : TStringList;
var
  i: Integer;
begin
  GetResults( SQLQuery, ZQuery3 );
  Result := TStringList.create();
  for i := 0 to ZQuery3.RecordCount - 1 do
  begin
    Result.Add( ZQuery3.Fields[x].AsString );
    ZQuery3.Next;
  end;
  ZQuery3.Close;
end;


{***
  Event procedure handler for the ZSQLMonitor1 object
}
procedure TMDIChild.ZSQLMonitor1LogTrace(Sender: TObject;
  Event: TZLoggingEvent);
begin
  LogSQL( Trim( Event.Message ), (Event.Category <> lcExecute) );
end;

{***
  Property-set procedure for QueryRunningFlag
  Allows it to be set from other windows
}
procedure TMDIChild.SetQueryRunningFlag (AValue : Boolean);
begin
  FQueryRunning := AValue;
end;


// Resize image to fit
procedure TMDIChild.ResizeImageToFit;
begin
  if EDBImage1.Picture.Width < 1 then exit;
  EDBImage1.Width := MulDiv(EDBImage1.Height, EDBImage1.Picture.Width, EDBImage1.Picture.Height);
  if EDBImage1.Picture.Height < 1 then exit;
  if EDBImage1.Height < 1 then exit;
  MainForm.showstatus('Image: ' + inttostr( EDBImage1.Picture.width)
    + ' x ' + inttostr( EDBImage1.Picture.Height ) + ' pixel, '
    + 'zoomed to ' + IntToStr(round( 100 / EDBImage1.Picture.Height * EDBImage1.Height )) + '%'
  );
end;

{***
  Run a query in a separate thread of execution on the current connection.

}
function TMDIChild.RunThreadedQuery(AQuery: String): TMysqlQuery;
begin
  Result := nil;

  // Check if the connection of the current window is still alive
  // Otherwise reconnect
  try
    CheckConnection;
  except
    exit;
  end;

  // Create instance of the progress form (but don't show it yet)
  FProgressForm := TFrmQueryProgress.Create(Self);


  // Indicate a querythread is active (only one thread allow at this moment)
  FQueryRunning := True;


  {
    Launch a thread of execution that passes the query to the server

    The progressform serves as receiver of the status
    messages (WM_MYSQL_THREAD_NOTIFY) of the thread:

    * After the thread starts it notifies the progressform (MQE_INITED)
      (which calls ShowModal on itself)
    * Waits for a completion message from the thread (MQE_FINISHED) to remove itself
    * Set FQueryRunning to false
  }
  debug('RunThreadedQuery(): Launching asynchronous query.');
  Result := ExecMysqlStatementAsync (AQuery,FConnParams,nil,FProgressForm.Handle);

  {
    Repeatedly check if the query has finished by inspecting FQueryRunning
    Allow repainting of user interface
  }
  WaitForQueryCompletion(FProgressForm);
end;

procedure TMDIChild.Splitter2Moved(Sender: TObject);
begin
  ResizeImageToFit;
end;


procedure TMDIChild.DataSourceDataChange(Sender: TObject; Field: TField);
begin
  GridHighlightChanged(Sender);
end;

procedure TMDIChild.DBGridColEnter(Sender: TObject);
begin
  GridHighlightChanged(Sender);
end;

procedure TMDIChild.GridHighlightChanged(Sender: TObject);
var
  grid: TSMDBGrid;
  ds: TDataSource;
begin
  // Current highlighted row and/or column in grid has changed.

  // (This probably only happens when something is clicked
  //  in the DBGrid, but if we really wanted to be sure, we
  //  could hook DBGrid.GridEnter and set a bool variable
  //  to true (meaning "grid has focus, row change events
  //  probably comes from grid"), and vice versa in GridExit..)

  grid := ActiveGrid;
  ds := grid.DataSource;
  if grid.SelectedField = nil then exit;

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
    EDBImage1.DataField := grid.SelectedField.FieldName;

    // Disable text editor if there's binary data or odd newlines in the field,
    // since the text editor may/will silently corrupt it if used.
    DBMemo1.ReadOnly :=
      hasIrregularChars(DBMemo1.Field.AsString) or
      hasIrregularNewlines(DBMemo1.Field.AsString);

    // Ensure visibility of the Blob-Editor
    PageControlBottom.ActivePageIndex := 1;
    MenuViewBlob.Enabled := true;

    // Detect if we have picture-data in this BLOB and
    // if yes, bring the viewer in the BLOB-editor to the front
    if EDBImage1.Picture.Height > 0 then
    begin
      PageControl4.ActivePageIndex := 1;
    end
    else
    begin
      PageControl4.ActivePageIndex := 0;
    end;
    ResizeImageToFit;

  end
  else
  begin
    // No BLOB selected, so disconnect the Blob-components
    // from any field
    DBMemo1.DataField := '';
    EDBImage1.DataField := '';
    MenuViewBlob.Enabled := false;
  end;

  if (DBMemo1.ReadOnly or (Length(DBMemo1.DataField) = 0)) then
  begin
    // Indicate the ReadOnly-state of the BLOB to the user
    DBMemo1.Color := clInactiveCaptionText;
  end
  else
  begin
    DBMemo1.Color := clWindow;
  end;

  PageControl4Change(self);
end;


procedure TMDIChild.gridQueryMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ed: TCustomEdit;
begin
  if Button = mbRight then begin
    // Select text in in-place editor to make popup menu copy and paste work.
    (Sender as TDBGrid).EditorMode := true;
    ed := TCustomEdit(FindControl(GetFocus()));
    ed.SelectAll;
    // Popup menu manually, mucking about with the grid causes it to stop doing it by itself.
    popupResultGrid.Popup(X + 178, Y + 248);
  end else begin
    inherited MouseDown(Button, Shift, X, Y);
  end;
end;

procedure TMDIChild.ZQuery1EditError(DataSet: TDataSet; E: EDatabaseError;
  var Action: TDataAction);
begin
  LogSQL( E.Message, true );
end;


procedure TMDIChild.FormResize(Sender: TObject);
begin
  ListTables.Width := tabDatabase.Width - Toolbar1.Width - Toolbar1.Left;
  ListTables.Height := tabDatabase.Height - Panel2.Height;
  Panel9.Width := tabTable.Width - Toolbar2.Width - Toolbar2.Left;
  Panel9.Height := tabTable.Height - Panel3.Height;
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
    openkey( REGPATH + '\Servers\' + FConnParams.Description, true );
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


// Rightclick on header of table grid
procedure TMDIChild.ListTablesColumnRightClick(Sender: TObject;
  Column: TListColumn; Point: TPoint);
begin
  popupDbGridHeader.Popup( Mouse.CursorPos.X, Mouse.CursorPos.Y );
end;


// Rightclick on database grid
procedure TMDIChild.ListTablesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if button = mbright then
    popupDbGrid.Popup( Mouse.CursorPos.X, Mouse.CursorPos.Y );
end;



function TMDIChild.mask(str: String) : String;
begin
  result := maskSql(mysql_version, str);
end;


function TMDIChild.CanAcessMysql: Boolean;
begin
  Result := HasAccessToDB (DBNAME_MYSQL);
end;

function TMDIChild.HasAccessToDB (ADBName : String) : Boolean;
var
  ds: TZReadOnlyQuery;
  dbName: string;
begin
  Result := False;

  ds := TZReadOnlyQuery.Create(Self);
  ds.Connection := FMysqlConn.Connection;
  GetResults( 'SHOW DATABASES', ds );
  
  while not ds.Eof do
    begin
      dbName := ds.FieldByName('Database').AsString;
      if LowerCase(dbName) = LowerCase(ADBName) then
        begin
          Result := True;
          Break;
        end;
      ds.Next;
    end;

  FreeAndNil (ds);
end;

procedure TMDIChild.CheckConnection;
begin
  if not FMysqlConn.IsAlive then begin
    LogSQL('Connection failure detected. Trying to reconnect.', true);
    FMysqlConn.Connection.Reconnect;
    PerformConnect;
  end;
end;



function TMDIChild.GetActiveGrid: TSMDBGrid;
begin
  Result := nil;
  if PageControlMain.ActivePage = tabData then Result := gridData;
  if PageControlMain.ActivePage = tabQuery then Result := gridQuery;
end;



procedure TMDIChild.ZQueryBeforeSendingSQL(DataSet: TDataSet);
begin
  try
    CheckConnection;
  except
    exit;
  end;
end;


{***
  Detect average row size and limit the number of rows fetched at
  once if more than ~ 5 MB of data
}
function TMDIChild.GetCalculatedLimit( Table: String ): Int64;
var
  AvgRowSize, RecordCount : Int64;
const
  // how much memory we're aiming to use for the
  // data grid and it's automatic limit function
  // this value should probably be user configurable
  LOAD_SIZE: Integer = 5*1024*1024;
  // how much overhead this application has per row
  ROW_SIZE_OVERHEAD : Integer = 1150;
  // average row size guess for mysql server < 5.0
  ROW_SIZE_GUESS: Integer = 2048;
  // round to nearest value when deciding limit
  ROUNDING: Integer = 1000;
begin
  result := -1;
  GetResults( 'SHOW TABLE STATUS LIKE ' + esc(Table), ZQuery3 );
  AvgRowSize := MakeInt( ZQuery3.FieldByName( 'Avg_row_length' ).AsString ) + ROW_SIZE_OVERHEAD;
  RecordCount := MakeInt( ZQuery3.FieldByName( 'Rows' ).AsString );
  if AvgRowSize * RecordCount > LOAD_SIZE then
  begin
    result := Trunc( LOAD_SIZE / AvgRowSize );
    result := (Trunc(result / ROUNDING) + 1) * ROUNDING;
    if result >= RecordCount then
      result := -1;
  end;
  debug( 'GetCalculatedLimit: ' + formatnumber(result) );
end;



end.




