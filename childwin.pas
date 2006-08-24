UNIT Childwin;


// -------------------------------------
// HeidiSQL
// MDI-Child-Window
// -------------------------------------


INTERFACE

uses Windows, Classes, Graphics, Forms, Controls, StdCtrls,
  ExtCtrls, ComCtrls, ImgList, SysUtils, Dialogs, Menus,
  SynEdit, SynMemo, SynEditHighlighter, SynHighlighterSQL,
  Registry, Spin, Clipbrd, Shellapi,
  Buttons, CheckLst, ToolWin, Db, DBGrids,
  DBCtrls, helpers,
  Grids, messages, smdbgrid, Mask, ZDataset,
  ZAbstractRODataset, ZConnection,
  ZSqlMonitor, ZPlainMySqlDriver, EDBImage, ZAbstractDataset, ZDbcLogging,
  SynCompletionProposal, HeidiComp;


type
  TMDIChild = class(TForm)
    Panel1: TPanel;
    DBtree: TTreeView;
    Splitter1: TSplitter;
    TableShow: TPanel;
    PageControl1: TPageControl;
    SheetData: TTabSheet;
    SheetDatabase: TTabSheet;
    Splitter2: TSplitter;
    SheetQuery: TTabSheet;
    popupTreeView: TPopupMenu;
    Drop1: TMenuItem;
    Panel2: TPanel;
    SheetTable: TTabSheet;
    Panel3: TPanel;
    popupDbGrid: TPopupMenu;
    menuviewdata: TMenuItem;
    menuproperties: TMenuItem;
    menuinsert: TMenuItem;
    menudroptable: TMenuItem;
    menuemptytable: TMenuItem;
    SheetHost: TTabSheet;
    PageControl2: TPageControl;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
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
    Timer2: TTimer;
    popupResultGrid: TPopupMenu;
    Copyrecords1: TMenuItem;
    CopyasCSVData1: TMenuItem;
    N9: TMenuItem;
    Label4: TLabel;
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
    PageControl3: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet8: TTabSheet;
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
    SaveDialogExportData: TSaveDialog;
    N11: TMenuItem;
    ProgressBarQuery: TProgressBar;
    btnQueryReplace: TToolButton;
    Copy2: TMenuItem;
    Copy4: TMenuItem;
    N14: TMenuItem;
    Copyfieldcontents1: TMenuItem;
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
    ZConn: TZConnection;
    ZQuery1: TZQuery;
    ZQuery2: TZQuery;
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
    procedure TabelleAnzeigen(Sender: TObject);
    procedure TabelleLeeren(Sender: TObject);
    procedure DBLoeschen(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LogSQL(msg: string = ''; comment: Boolean = true );
    procedure ShowVariablesAndProcesses(Sender: TObject);
    procedure ListProcessesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure CreateDatabase(Sender: TObject);
    procedure KillProcess(Sender: TObject);
    procedure PageControl2Change(Sender: TObject);
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
    procedure Timer2Timer(Sender: TObject);
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
    procedure LoadSQLClick(Sender: TObject);
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
    procedure DBGridEnter(Sender: TObject);
    procedure DBGridExit(Sender: TObject);
    procedure popupDataGridPopup(Sender: TObject);
    procedure InsertDate(Sender: TObject);
    procedure btnBlobCopyClick(Sender: TObject);
    procedure setNULL1Click(Sender: TObject);
    procedure MenuAddFieldClick(Sender: TObject);
    procedure ZQuery2BeforeClose(DataSet: TDataSet);
    procedure ExecQuery( SQLQuery: String );
    procedure ExecUseQuery( DbName: String );
    function GetVar( SQLQuery: String; x: Integer = 0 ) : String;
    procedure GetResults( SQLQuery: String; ZQuery: TZReadOnlyQuery );
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
    procedure DBMemo1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    function mask(str: String) : String;
    procedure CheckConnection();
    procedure ZQueryBeforeSendingSQL(DataSet: TDataSet);

    private
      { Private declarations }
      strHostRunning             : String;
      uptime, time_connected     : Integer;
      OnlyDBs                    : TStringList;  // used on connecting
      rowcount                   : Integer;      // rowcount of ActualTable
      viewingdata                : Boolean;
      WhereFilters               : TStringList;
      WhereFiltersIndex          : Integer;
      StopOnErrors, WordWrap     : Boolean;
      procedure GridHighlightChanged(Sender: TObject);
      procedure SaveBlob;
      function GetActiveGrid: TSMDBGrid;

    public
      { Public declarations }
      ActualDatabase, ActualTable: string;
      dataselected, editing      : Boolean;
      mysql_version              : Integer;
      tnodehost                  : TTreeNode;
      OnlyDBs2                   : TStringList;
      Description                : String;
      DBRightClickSelectedItem   : TTreeNode;    // TreeNode for dropping with right-click
      property ActiveGrid: TSMDBGrid read GetActiveGrid;
  end;


// --------------------------------------------------------------------------------------
IMPLEMENTATION

uses
  connections, Main, createtable, fieldeditor, tbl_properties,
  tblcomment, selectsomedatabases, optimizetables, copytable,
  mysqlerror;



const
	CRLF = #13#10;


{$R *.DFM}



procedure TMDIChild.PerformConnect;
begin
  try
    ZConn.Connect;
    TimerConnected.Enabled := true;
    // On Re-Connection, try to restore lost properties
    if ZConn.Database <> '' then
    begin
      ExecUseQuery( ZConn.Database );
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


procedure TMDIChild.FormCreate(Sender: TObject);
var
  AutoReconnect    : Boolean;
  menuitem         : TMenuItem;
  i                : Byte;
begin
  // initialization: establish connection and read some vars from registry
  Screen.Cursor := crHourGlass;
  MainForm.Showstatus('Creating window...', 2, true);

  // temporarily disable AutoReconnect in Registry
  // in case of unexpected application-termination
  AutoReconnect := false;
  with TRegistry.Create do
  begin
    openkey(regpath, true);
    if Valueexists('Autoreconnect') then
    if ReadBool('AutoReconnect') then
    begin
      AutoReconnect := true;
      WriteBool('AutoReconnect', false);
    end;
    closekey();
  end;

  ReadWindowOptions;

  MainForm.Showstatus('Connecting to '+connform.EditHost.Text+'...', 2, true);
  ZConn.Hostname := connform.EditHost.Text;
  ZConn.User := connform.EditBenutzer.Text;
  ZConn.Password := connform.EditPasswort.Text;
  ZConn.Port := strToIntDef(connform.EditPort.Text, MYSQL_PORT);
  if connform.CheckBoxCompressed.Checked then
    ZConn.Properties.Values['compress'] := 'true';
  ZConn.Properties.Values['timeout'] := connform.EditTimeout.Text;
  ZConn.Properties.Values['dbless'] := 'true';
  ZConn.Properties.Values['CLIENT_LOCAL_FILES'] := 'true';
  ZConn.Properties.Values['CLIENT_INTERACTIVE'] := 'true';
  // ZConn.Properties.Values['USE_RESULT'] := 'true'; // doesn't work

  //  ZConn.Properties.Values['CLIENT_SSL'] := 'true'; // from an mdaems's example

  try
    PerformConnect;
  except
    MainForm.Showstatus( STATUS_MSG_READY, 2);
    timer5.Enabled := true;
    Exit;
  end;

  Description := connform.ComboBoxDescription.Text;
  Caption := Description;
  OnlyDBs := explode(';', connform.EditOnlyDBs.Text);

  // Versions and Statistics
  LogSQL( 'Connection established with host "' + ZConn.hostname + '" on port ' + inttostr(ZConn.Port) );

  ShowVariablesAndProcesses(self);
  ReadDatabasesAndTables(self);

  // re-enable AutoReconnect in Registry!
  if AutoReconnect then
  with TRegistry.Create do
  begin
    openkey(regpath, true);
    WriteBool('AutoReconnect', true);
    closekey();
  end;

  // set some defaults
  ActualDatabase := '';
  ActualTable := '';
  Screen.Cursor := crDefault;

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
end;


procedure TMDIChild.ReadWindowOptions;
var
  ws : String;
  i : Integer;
  menuitem : Tmenuitem;
begin
  with TRegistry.Create do
  begin
    if OpenKey(regpath, true) then
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
        PageControl3.Height := ReadInteger('sqloutheight');
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

    end;
    CloseKey;
  end;
end;


procedure TMDIChild.FormClose(Sender: TObject; var Action: TCloseAction);
var
  ws : String;
begin
  // closing connection and saving some vars into registry
  if windowstate = wsNormal then
    ws := 'Normal' else
  if windowstate = wsMinimized
    then ws := 'Minimized' else
  if windowstate = wsMaximized
    then ws := 'Maximized';

  with TRegistry.Create do
  begin
    if OpenKey(regpath, true) then
    begin
      WriteString('childwinstate', ws);
      WriteInteger('childwinleft', left);
      WriteInteger('childwintop', top);
      WriteInteger('childwinwidth', width);
      WriteInteger('childwinheight', height);

      WriteInteger('querymemoheight', panel7.Height);
      WriteInteger('dbtreewidth', dbtree.width);
      WriteInteger('sqloutheight', PageControl3.Height);
    end;
  end;
  mainform.ToolBarData.visible := false;
  FormDeactivate(sender);
  Action := caFree;
end;



procedure TMDIChild.LogSQL(msg: string = ''; comment: Boolean = true);
begin
  // add a sql-command or info-line to history-memo
  while SynMemoSQLLog.Lines.Count > mainform.logsqlnum do
  begin
    SynMemoSQLLog.Lines.Delete(0);
  end;
  msg := Copy(msg, 0, 2000);
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
begin
  // Fill DBTree
  Screen.Cursor := crHourGlass;
  dataselected := false;
  DBTree.OnChange := nil;
  DBTree.items.Clear;

  tnodehost := DBtree.Items.Add(nil, ZConn.User + '@' + ZConn.Hostname);  // Host or Root
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
  SynSQLSyn1.TableNames.AddStrings( OnlyDBs2 );
  if (OnlyDBs.Count = 0) and (OnlyDBs2.Count > 50) then with SelectFromManyDatabases do begin
    CheckListBoxDBs.Items.Clear;
    CheckListBoxDBs.Items := OnlyDBs2;
    ShowModal;
  end;

  // List Databases and Tables-Names
  for i:=0 to OnlyDBs2.Count-1 do
  try
    GetResults( 'SHOW TABLES FROM ' + mask(OnlyDBs2[i]), ZQuery3 );
    tnode := DBtree.Items.AddChild(tnodehost, OnlyDBs2[i]);
    tnode.ImageIndex := 37;
    tnode.SelectedIndex := 38;
    if ActualDatabase = OnlyDBs2[i] then
      tmpSelected := tnode;
    for j:=1 to ZQuery3.RecordCount do
    begin
      tchild := DBtree.Items.AddChild( tnode, ZQuery3.Fields[0].AsString );
      tchild.ImageIndex := 39;
      tchild.SelectedIndex := 40;
      if (ActualTable = ZQuery3.Fields[0].AsString) and (tmpSelected.Text = OnlyDBs2[i]) then
        tmpSelected := tchild;
      SynSQLSyn1.TableNames.Add( ZQuery3.Fields[0].AsString );
      ZQuery3.Next;
    end;
  except
    on E : Exception do
    begin
      LogSQL( 'Could not open database ''' + OnlyDBs2[i] + ''' - ignoring: ' + e.Message );
      continue;
    end;
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
  SheetDatabase.TabVisible := false;
  SheetTable.TabVisible := false;
  SheetData.TabVisible := false;
  if
   (not DBTree.Dragging) or
   (PageControl1.ActivePage = SheetDatabase) or
   (PageControl1.ActivePage = SheetTable) or
   (PageControl1.ActivePage = SheetData) then
  begin
    PageControl1.ActivePage := SheetHost;
  end;
  Caption := Description;
  ActualDatabase := '';
  ActualTable := '';
end;

procedure TMDIChild.SelectDatabase(db: string);
begin
  SheetDatabase.TabVisible := true;
  SheetTable.TabVisible := false;
  SheetData.TabVisible := false;
  if
   (not DBTree.Dragging) or
   (PageControl1.ActivePage = SheetTable) or
   (PageControl1.ActivePage = SheetData) then
  begin
    PageControl1.ActivePage := SheetDatabase;
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
  SheetDatabase.TabVisible := true;
  SheetTable.TabVisible := true;
  SheetData.TabVisible := true;
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
  columnname                : String;
  columnexists              : Boolean;
begin
  // view table-data with zeos
  if viewingdata then
    abort;
  viewingdata := true;

  // rowcount:
  try
    rowcount := StrToIntDef( GetVar( 'SELECT COUNT(*) FROM ' + mask(ActualTable), 0 ), 0 );
  except
    rowcount := 0;
  end;

  // set db-aware-component's properties...
  DBMemo1.DataField := '';
  DBMemo1.DataSource := DataSource1;
  EDBImage1.DataField := '';
  EDBImage1.DataSource := DataSource1;

  reg := TRegistry.Create;
  reg.openkey( regpath + '\Servers\' + description, true );

  if not dataselected then
  begin
    SynMemoFilter.Text := '';
    gridData.SortColumns.Clear;
    // Read cached WHERE-clause and set filter
    reg_value := 'WHERECLAUSE_' + ActualDatabase + '.' + ActualTable;
    if reg.ValueExists( reg_value ) then
      SynMemoFilter.Text := reg.ReadString( reg_value );
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
    SheetTable.TabVisible := true;
    SheetData.TabVisible := true;
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
    PageControl1.ActivePage := SheetData;

		MainForm.ShowStatus( 'Retrieving data...', 2, true );
    ZConn.Database := ActualDatabase;
    ZQuery2.Close;
    ZQuery2.SQL.Clear;
    ZQuery2.SQL.Add( 'SELECT * FROM ' + mask(ActualTable) );
    if trim(self.SynMemoFilter.Text) <> '' then
      ZQuery2.SQL.Add( 'WHERE ' + trim(self.SynMemoFilter.Text) );
    if sorting <> '' then
      ZQuery2.SQL.Add( sorting );
    if mainform.CheckBoxLimit.Checked then
      ZQuery2.SQL.Add('LIMIT ' + intToStr(mainform.UpDownLimitStart.Position) + ', ' + intToStr(mainform.UpDownLimitEnd.position) );
    try
      ZQuery2.Open;
    except
      on E:Exception do
      begin
        LogSQL( E.Message, true );
        MessageDlg(E.Message , mtError, [mbOK], 0);
        ZQuery2.Active := false;
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
      if StrCmpBegin('enum', ListColumns.Items[i].SubItems[0]) then begin
        DropDown := explode(''',''', getklammervalues(ListColumns.Items[i].SubItems[0]));
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
      ZQuery2.Fields[j].Required := false;

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

    Panel5.Caption := ActualDatabase + ' / ' + ActualTable + ': ' +
      IntToStr(rowcount) + ' Records (' +
      IntToStr(ZQuery2.RecordCount) + ' selected)';

    dataselected := true;
    pcChange(self);
  end;
  viewingdata := false;
  Screen.Cursor := crDefault;
end;



procedure TMDIChild.pcChange(Sender: TObject);
var DataOrQueryTab : Boolean;
begin
  // PageControl changes
  Mainform.ExecuteQuery.Enabled := PageControl1.ActivePage = SheetQuery;
  Mainform.ExecuteSelection.Enabled := PageControl1.ActivePage = SheetQuery;
  Mainform.ExecuteLine.Enabled := PageControl1.ActivePage = SheetQuery;
  if (PageControl1.ActivePage = SheetData) and (not dataselected) then
    viewdata(self);
  if PageControl1.ActivePage = SheetQuery then
    if ActualDatabase <> '' then
      Panel6.Caption := 'SQL-Query on Database ' + ActualDatabase + ':'
    else
      Panel6.Caption := 'SQL-Query on Host ' + ZConn.HostName + ':';

  // copy and save csv-buttons
  DataOrQueryTab := (PageControl1.ActivePage = SheetQuery) or (PageControl1.ActivePage = SheetData);
  with mainform do begin
    Copy2CSV.Enabled := DataOrQueryTab;
    CopyHTMLtable.Enabled := DataOrQueryTab;
    Copy2XML.Enabled := DataOrQueryTab;
    ExportData.Enabled := DataOrQueryTab;
    PrintList.Enabled := not DataOrQueryTab;
  end;

  mainform.ToolBarData.Visible:= (PageControl1.ActivePage = SheetData);
  mainform.DBNavigator1.DataSource := DataSource1;

  Tabsheet2.TabVisible := DataOrQueryTab;
  Tabsheet8.TabVisible := (PageControl1.ActivePage = SheetData);

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

  ZConn.Database := ActualDatabase;
  ExecUseQuery( ActualDatabase );

  Try
    if mysql_version >= 32300 then
    begin
      // get quick results with versions 3.23.xx and newer
      GetResults( 'SHOW TABLE STATUS', ZQuery3 );

      // Generate items for popupDbGridHeader
      for i:=popupDbGridHeader.Items.Count-1 downto 2 do
        popupDbGridHeader.Items.Delete( i );
      with TRegistry.Create do
      begin
        openkey( regpath + '\Servers\' + description, true );
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
          n.SubItems.Add( format('%10.0n', [ZQuery3.FieldByName('Rows').AsFloat] ) );
          // Size: Data_length + Index_length
          bytes := ZQuery3.FieldByName('Data_length').AsFloat + ZQuery3.FieldByName('Index_length').AsFloat;
          n.SubItems.Add(format('%10.0n KB', [bytes / 1024 + 1]));
          // Created:
          n.SubItems.Add( DateTimeToStr(ZQuery3.FieldByName('Create_time').AsDateTime) );
          // Updated:
          n.SubItems.Add( DateTimeToStr(ZQuery3.FieldByName('Update_time').AsDateTime) );
          // Type
          Try // Until 4.x
            n.SubItems.Add( ZQuery3.FieldByName('Type').AsString );
          Except // Since 5.x
            on EDatabaseError do
              n.SubItems.Add( ZQuery3.FieldByName('Engine').AsString );
          End;
          // Comment
          n.SubItems.Add( ZQuery3.FieldByName('Comment').AsString );
        end;
        for j:=0 to TablelistColumns.Count-1 do
        begin
          for k:=0 to ZQuery3.FieldCount-1 do
          begin
            if TablelistColumns[j] = ZQuery3.Fields[k].FieldName then
            begin
              n.SubItems.Add( ZQuery3.Fields[k].AsString );
              if IntToStr(StrToIntDef(ZQuery3.Fields[k].AsString,-1)) =  ZQuery3.Fields[k].AsString then
                ListTables.Columns[n.SubItems.Count].Alignment := taRightJustify
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
  MainForm.ShowStatus( 'Reading table properties...', 2, true );

  if (PageControl1.ActivePage <> SheetData) and (not DBTree.Dragging) then
    PageControl1.ActivePage := SheetTable;
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

  ListColumns.Items.BeginUpdate;
  ListColumns.Items.Clear;
  Try
    GetResults( 'SHOW FIELDS FROM ' + mask(ActualTable), ZQuery3 );
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

  MainForm.showstatus(ActualDatabase + ': '+ ActualTable + ': ' + inttostr(ListColumns.Items.count) +' field(s)');
  Screen.Cursor := crDefault;
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
  if (PageControl1.ActivePage <> SheetDatabase) then ListTables.Selected := nil;

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
  MainForm.DropTable.Enabled := tableSelected or ((PageControl1.ActivePage <> SheetDatabase) and (ActualTable <> ''));
  MainForm.ButtonCreateTable.Enabled := ActualDatabase <> '';
end;


procedure TMDIChild.TabelleAnzeigen(Sender: TObject);
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
      PageControl1.ActivePage := SheetData;
      viewdata(self);
      break;
    end;
    tn := tndb.GetNextChild(tn);
  end;
end;


procedure TMDIChild.TabelleLeeren(Sender: TObject);
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
    ExecQuery( 'DELETE FROM ' + mask(t[i]) );
  ShowDBProperties(self);
  Screen.Cursor := crDefault;
end;


procedure TMDIChild.DBLoeschen(Sender: TObject);
var
  tndb_ : TTreeNode;
begin
  // Drop DB?
  if (Sender as TComponent).Name = 'PopupmenuDropDatabase' then // drop cmd from popupmenu
    tndb_ := DBRightClickSelectedItem
  else case DBTree.Selected.Level of  // drop cmd from toolbar
    1 : tndb_ := DBTree.Selected;
    2 : tndb_ := DBTree.Selected.Parent;
    3 : tndb_ := DBTree.Selected.Parent.Parent;
  end;

  if MessageDlg('Drop Database "'+tndb_.Text+'"?' + crlf + crlf + 'WARNING: You will lose all tables in database '+tndb_.Text+'!', mtConfirmation, [mbok,mbcancel], 0) <> mrok then
    abort;

  Screen.Cursor := crSQLWait;
  try
    ExecQuery( 'DROP DATABASE ' + mask(tndb_.Text) );
    if OnlyDBs.Count > 0 then
    begin
      if OnlyDBs.IndexOf(tndb_.Text) > -1 then
      begin
        OnlyDBs.Delete( OnlyDBs.IndexOf(tndb_.Text) );
        with TRegistry.Create do
        begin
          if OpenKey(regpath + '\Servers\' + Description, false) then
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
var
  v : String[10];
  i : Integer;
  n : TListItem;
  versions : TStringList;
begin
// Variables und Process-List aktualisieren
  Screen.Cursor := crSQLWait;

  ListVariables.Items.BeginUpdate;
  ListVariables.Items.Clear;

  // VERSION
  v := GetVar( 'SELECT VERSION()' );
  versions := explode( '.', v );
  mysql_version := MakeInt(versions[0]) * 10000 + MakeInt(versions[1]) * 100 + MakeInt(versions[2]);
  strHostRunning := ZConn.HostName + ' running MySQL-Version ' + v + ' / Uptime: ';

  // VARIABLES
  GetResults( 'SHOW VARIABLES', ZQuery3 );
  for i:=1 to ZQuery3.RecordCount do
  begin
    n := ListVariables.Items.Add;
    n.Caption := ZQuery3.Fields[0].AsString;
    n.Subitems.Add( ZQuery3.Fields[1].AsString );
    ZQuery3.Next;
  end;

  uptime := 0;

  // STATUS
  GetResults( 'SHOW STATUS', ZQuery3 );
  for i:=1 to ZQuery3.RecordCount do
  begin
    n := ListVariables.Items.Add;
    n.Caption := ZQuery3.Fields[0].AsString;
    n.Subitems.Add( ZQuery3.Fields[1].AsString );
    if lowercase( ZQuery3.Fields[0].AsString ) = 'uptime' then
      uptime := strToIntDef(ZQuery3.Fields[1].AsString, 0);
    ZQuery3.Next;
  end;

  Timer1Timer(self);
  Timer1.OnTimer := Timer1Timer;

  ListVariables.Items.EndUpdate;
  TabSheet6.Caption := 'Variables (' + inttostr(ListVariables.Items.Count) + ')';
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
    ZQuery3.SQL.Add( 'SHOW PROCESSLIST' );
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
    TabSheet7.Caption := 'Process-List (' + inttostr(ListProcesses.Items.Count) + ')';
  except
    LogSQL( 'Error on loading process-list!' );
  end;
  ListProcesses.Items.EndUpdate;
  Screen.Cursor := crDefault;
end;


procedure TMDIChild.ListProcessesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  Kill1.Enabled := (ListProcesses.Selected <> nil) and (PageControl2.ActivePage = TabSheet7);
end;


procedure TMDIChild.KillProcess(Sender: TObject);
var t : boolean;
begin
  if ListProcesses.Selected.Caption = GetVar( 'SELECT CONNECTION_ID()' ) then
    MessageDlg('Fatal: Better not kill my own Process...', mtError, [mbok], 0)
  else begin
    t := TimerProcessList.Enabled;
    TimerProcessList.Enabled := false; // prevent av (ListProcesses.selected...)
    if MessageDlg('Kill Process '+ListProcesses.Selected.Caption+'?', mtConfirmation, [mbok,mbcancel], 0) = mrok then
    begin
      ExecQuery( 'KILL '+ListProcesses.Selected.Caption );
      ShowVariablesAndProcesses(self);
    end;
    TimerProcessList.Enabled := t; // re-enable autorefresh timer
  end;
end;


procedure TMDIChild.PageControl2Change(Sender: TObject);
begin
  ListProcessesChange(self, nil, TItemChange(self));
end;



procedure TMDIChild.ExecSQLClick(Sender: TObject; Selection: Boolean=false; CurrentLine: Boolean=false);
var
  SQL                     : TStringList;
  i, rowsaffected         : Integer;
  SQLstart, SQLend, SQLscriptstart,
  SQLscriptend            : Integer;
  SQLTime                 : Real;
  fieldcount, recordcount : Integer;
  sql_keyword             : String;
begin
  // Execute user-defined SQL
//  if SynMemo3.Focused then
//    exit;
  if length(trim(SynMemoQuery.Text)) = 0 then
    exit;

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
      zconn.Database := ActualDatabase;
    ZQuery1.Active := false;
    ZQuery1.DisableControls;

    if CurrentLine then
      SQL := parseSQL(SynMemoQuery.LineText)         // Run current line
    else begin
      if Selection then
        SQL := parsesql(SynMemoQuery.SelText) else   // Run selection
        SQL := parsesql(SynMemoQuery.Text);          // Run all
    end;
    if SQL.Count > 1 then
      SQLscriptstart := GetTickCount
    else
      Label4.Caption := '';

    rowsaffected := 0;
    ProgressBarQuery.Max := SQL.Count;
    ProgressBarQuery.Position := 0;
    ProgressBarQuery.show;

    MainForm.showstatus('Executing SQL...', 2, true);
    for i:=0 to SQL.Count-1 do begin
      ProgressBarQuery.Stepit;
      Application.ProcessMessages;
      if sql[i] = '' then
        continue;
      // open last query with data-aware:
      Label4.Caption := '';
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
        sql_keyword := lowercase( copy( SQL[i], 0, 20 ) );
        if (
          StrCmpBegin( 'analyze', sql_keyword ) or
          StrCmpBegin( 'check', sql_keyword ) or
          StrCmpBegin( 'desc', sql_keyword ) or
          StrCmpBegin( 'describe', sql_keyword ) or
          StrCmpBegin( 'explain', sql_keyword ) or
          StrCmpBegin( 'help', sql_keyword ) or
          StrCmpBegin( 'optimize', sql_keyword ) or
          StrCmpBegin( 'repair', sql_keyword ) or
        	StrCmpBegin( 'select', sql_keyword ) or
          StrCmpBegin( 'show', sql_keyword )
          ) then
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

      Label4.Caption :=
        IntToStr(rowsaffected) + ' row(s) affected. ' +
        inttostr(fieldcount) + ' field(s), ' +
        inttostr(recordcount) + ' record(s) in last resultset. ' +
        'Time: '+floattostrf(SQLTime, ffFixed, 18, 2)+' sec.';

    end;
    ProgressBarQuery.hide;
    Mainform.ExecuteQuery.Enabled := true;
    Mainform.ExecuteSelection.Enabled := true;
    // count chars:
    SynMemoQuery.OnChange(self);

    if SQL.Count > 1 then begin
      SQLscriptend := GetTickCount;
      SQLTime := (SQLscriptend - SQLscriptstart) / 1000;
      Label4.Caption := Label4.Caption + ' Script-Time: ' + floattostrf(SQLTime, ffFixed, 18, 2)+' sec.';
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
  // Feldeigenschaften anzeigen

  if ListColumns.Selected <> nil then
  with ListColumns.Selected do begin
    btnTableDropField.Enabled := True;
    DropField1.Enabled := True; //drop field
    MenuEditField.Enabled := true;
    btnTableEditField.enabled := true;
  end else begin
    btnTableDropField.Enabled := False;
    DropField1.Enabled := false; //drop field
    MenuEditField.Enabled := false;
    btnTableEditField.enabled := false;
  end;

end;

procedure TMDIChild.DropField(Sender: TObject);
var
  tn : TTreeNode;
begin
  // Feld löschen
  if ListColumns.Items.Count = 1 then
  begin
    if MessageDlg('Can''t drop the last Field - drop Table '+ActualTable+'?', mtConfirmation, [mbok,mbcancel], 0) = mrok then
    begin
      Screen.Cursor := crSQLWait;
      ExecQuery( 'DROP TABLE '+mask(ActualTable) );
      tn := DBTree.Selected;
      DBTree.Selected := DBTree.Selected.Parent;
      tn.Destroy;
      ShowDBProperties(self);
      Screen.Cursor := crDefault;
    end;
  end else
  if MessageDlg('Drop field ' + ListColumns.Selected.Caption + ' ?', mtConfirmation, [mbok,mbcancel], 0) = mrok then
  begin
    ExecQuery( 'ALTER TABLE '+mask(ActualTable)+' DROP '+mask(ListColumns.Selected.Caption) );
    ShowTableProperties(self);
  end;
end;


{ Proposal about to insert a string into synmemo }
procedure TMDIChild.SynCompletionProposal1CodeCompletion(Sender: TObject;
  var Value: string; Shift: TShiftState; Index: Integer; EndToken: Char);
begin
  // don't mask function-names
  if not StrCmpBegin( '\image{86}', SynCompletionProposal1.ItemList[Index] )
    then Value := mask( Value );
end;


{ Proposal-Combobox pops up }
procedure TMDIChild.SynCompletionProposal1Execute(Kind: TSynCompletionType;
  Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);
var
  i,j : Integer;
  functionname : String;
  functiondecl : String;
  tn, child : TTreeNode;

  procedure addTable( name: String );
  begin
    SynCompletionProposal1.InsertList.Add( name );
    SynCompletionProposal1.ItemList.Add( '\image{40}\hspace{2}\color{clBlue}table\color{clWindowText}\column{}' + name );
  end;

begin
  SynCompletionProposal1.InsertList.Clear;
  SynCompletionProposal1.ItemList.Clear;

  if length(CurrentInput) = 0 then
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

  with SynCompletionProposal1 do
  begin

    if SynCompletionProposal1.ItemList.count = 0 then
    begin
      // Add databases to proposal list
      InsertList.AddStrings( OnlyDBs2 );
      ItemList.AddStrings( OnlyDBs2 );
      for i:=0 to ItemList.count-1 do
        ItemList[i] := '\image{38}\hspace{2}\color{clMaroon}database\color{clWindowText}\column{}' + ItemList[i];

      if ActualDatabase <> '' then
      begin
        // Add tables to proposal list
        for i:=0 to ListTables.Items.Count-1 do
        begin
          addTable( ListTables.Items[i].Caption );
        end;
        if length(CurrentInput) = 0 then // assume that we have already a dbname in memo
          Position := OnlyDBs2.Count;
      end;
    end;

    // Add functions to proposal list
    for i := 0 to MainForm.sqlfunctionlist.Count - 1 do
    begin
      functionname := copy(MainForm.sqlfunctionlist[i], 0, pos('(', MainForm.sqlfunctionlist[i])-1);
      if pos( '|', MainForm.sqlfunctionlist[i] ) > 0 then
        functiondecl := copy(MainForm.sqlfunctionlist[i], length(functionname)+1, pos( '|', MainForm.sqlfunctionlist[i] )-length(functionname)-1)
      else
        functiondecl := copy(MainForm.sqlfunctionlist[i], length(functionname)+1, length(MainForm.sqlfunctionlist[i]) );
      InsertList.Add( functionname + functiondecl );
      ItemList.Add( '\image{86}\hspace{2}\color{clTeal}function\color{clWindowText}\column{}' + functionname + '\style{-B}' + functiondecl );
    end;
  end;
end;


procedure TMDIChild.SynMemoQueryChange(Sender: TObject);
var somechars : Boolean;
begin
  PanelCharsInQueryWindow.Caption :=
    Format('%g', [length(SynMemoQuery.Text) + 0.0]) + ' Characters';
  somechars := length(SynMemoQuery.Text) > 0;
  Mainform.ExecuteQuery.Enabled := somechars;
  Mainform.ExecuteSelection.Enabled := length(SynMemoQuery.SelText) > 0;
  Mainform.ExecuteLine.Enabled := SynMemoQuery.LineText <> '';
  btnQuerySave.Enabled := somechars;
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
  if ZConn.Connected then
  begin
    Application.Title := Description + ' - ' + main.appname;
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
      if (PageControl1.ActivePage = SheetData) or
        (PageControl1.ActivePage = SheetQuery) then begin
        Copy2CSV.Enabled := true;
        CopyHTMLtable.Enabled := true;
        Copy2XML.Enabled := true;
        ExportData.Enabled := true;
      end;
      mainform.ToolBarData.visible := (PageControl1.ActivePage = SheetData);
      mainform.DBNavigator1.DataSource := DataSource1;
      //DBtreeChange( self, DBTree.Selected );
    end;
  end;
  TimerConnected.OnTimer(self);
  ZSQLMonitor1.Active := true;
end;

procedure TMDIChild.FormDeactivate(Sender: TObject);
begin
  Application.Title := main.appname;
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
  end;
  MainForm.showstatus('', 1); // empty connected_time

  ZSQLMonitor1.Active := false;
end;



// Primary key abfragen - delete from .. where
procedure TMDIChild.FormShow(Sender: TObject);
begin
  // initialize some values and components:
  timer2.Enabled := true;

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
begin
  FieldEditForm.UpdateField := ListColumns.Selected <> nil;
  FieldEditForm.showmodal;
end;

{ Add new field }
procedure TMDIChild.MenuAddFieldClick(Sender: TObject);
begin
  FieldEditForm.UpdateField := false;
  FieldEditForm.showmodal;
end;


procedure TMDIChild.CreateDatabase(Sender: TObject);
var dbname : String;
begin
  // Create new Database:
  if InputQuery('Create new Database...', 'Database Name:', dbname) then
  begin
    Screen.Cursor := crSQLWait;
    Try
      ExecQuery( 'CREATE DATABASE ' + mask( dbname ) );
      // Add DB to OnlyDBs-regkey if this is not empty
      if OnlyDBs.Count > 0 then
      begin
        OnlyDBs.Add( dbname );
        with TRegistry.Create do
        begin
          if OpenKey(regpath + '\Servers\' + Description, false) then
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


procedure TMDIChild.Timer2Timer(Sender: TObject);
begin
  // show mdi-form (krücke...)
  show;
  Application.ProcessMessages;
  Timer2.Enabled := false;
end;


procedure TMDIChild.MenuAdvancedPropertiesClick(Sender: TObject);
begin
  tbl_properties_form.showmodal;
end;


procedure TMDIChild.ListTablesEdited(Sender: TObject; Item: TListItem;
  var S: String);
var i : Integer;
begin
  // edit table-name
  menudroptable.ShortCut := TextToShortCut('Del');

  ExecQuery( 'ALTER TABLE ' + mask(Item.Caption) + ' RENAME ' + mask(S) );
  ActualTable := S;
  ShowDBProperties(self);
  // Re-Select Entry
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
  PageControl3.ActivePageIndex := 1;
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
  PageControl1.ActivePage := SheetQuery;
  pcChange(self);
end;


procedure TMDIChild.Markall3Click(Sender: TObject);
begin
  // select all in history
  SynMemoSQLLog.SelectAll;
end;


procedure TMDIChild.More1Click(Sender: TObject);
begin
  optimize.showmodal;
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
        ExecQuery( 'OPTIMIZE TABLE ' + mask(ListTables.Items[i].Caption) );
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
    ExecQuery( 'CHECK TABLE ' + tables + ' QUICK' );
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
    ExecQuery( 'ANALYZE TABLE ' + tables );
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
    ExecQuery( 'REPAIR TABLE ' + tables + ' QUICK' );
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
  mainform.Showstatus('', 1);
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
  PageControl3.ActivePageIndex := 2;
  SynMemoFilter.SetFocus;
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
      ZQuery2.Delete
  end else
  if MessageDLG('Delete '+inttostr(gridData.SelectedRows.count)+' Record(s)?', mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
    gridData.SelectedRows.Delete;
end;


procedure TMDIChild.QuickFilterClick(Sender: TObject);
var
  filter,value,menuitem : String;
begin
  // Set filter for "where..."-clause
  value := escape_string(gridData.SelectedField.AsString);
  menuitem := (Sender as TMenuItem).Name;
  if menuitem = 'QF1' then
    filter := '=' + ' ''' + value + ''''
  else if menuitem = 'QF2' then
    filter := '!=' + ' ''' + value + ''''
  else if menuitem = 'QF3' then
    filter := '>' + ' ''' + value + ''''
  else if menuitem = 'QF4' then
    filter := '<' + ' ''' + value + ''''
  else if menuitem = 'QF5' then
    filter := 'LIKE' + ' ''' + value + '%'''
  else if menuitem = 'QF6' then
    filter := 'LIKE' + ' ''%' + value + ''''
  else if menuitem = 'QF7' then
    filter := 'LIKE' + ' ''%' + value + '%'''
  else if menuitem = 'QF8' then begin
    filter := InputBox('Specify filter-value...', mask(gridData.SelectedField.FieldName)+' = ', 'Value');
    if filter = 'Value' then
      abort;
    filter := '= ''' + filter + '''';
  end
  else if menuitem = 'QF9' then begin
    filter := InputBox('Specify filter-value...', mask(gridData.SelectedField.FieldName)+' != ', 'Value');
    if filter = 'Value' then
      abort;
    filter := '!= ''' + filter + '''';
  end
  else if menuitem = 'QF10' then begin
    filter := InputBox('Specify filter-value...', mask(gridData.SelectedField.FieldName)+' > ', 'Value');
    if filter = 'Value' then
      abort;
    filter := '> ''' + filter + '''';
  end
  else if menuitem = 'QF11' then begin
    filter := InputBox('Specify filter-value...', mask(gridData.SelectedField.FieldName)+' < ', 'Value');
    if filter = 'Value' then
      abort;
    filter := '< ''' + filter + '''';
  end
  else if menuitem = 'QF12' then begin
    filter := InputBox('Specify filter-value...', mask(gridData.SelectedField.FieldName)+' like ', 'Value');
    if filter = 'Value' then
      abort;
    filter := 'LIKE ''' + filter + '''';
  end;

  SynMemoFilter.Text := gridData.SelectedField.FieldName + ' ' + filter;
  PageControl3.ActivePageIndex := 2;
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
            Write(bf, ZQuery2.FieldByName(DBMemo1.DataField).AsString);
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
          openkey(regpath, true);
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
    reg.openkey( regpath + '\Servers\' + description, false );
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
  SynMemoFilter.Lines.Clear;
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
      ExecQuery( 'ALTER TABLE ' + mask(ListTables.Items[i].Caption) + ' TYPE = ' + tabletype);
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
        ExecQuery( 'ALTER TABLE ' + mask(ListTables.Items[i].Caption) + ' TYPE = ' + strtype );
    ShowDBProperties(self);
  end;
end;

procedure TMDIChild.InsertRecord(Sender: TObject);
begin
  viewdata(self);
  ZQuery2.Insert;
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
  SynMemoQuery.SelText := DBTree.Selected.Text;
end;



procedure TMDIChild.SynMemoQueryDropFiles(Sender: TObject; X, Y: Integer;
  AFiles: TStrings);
var
  i        : Integer;
  s        : TStringList;
begin
  // one or more files from explorer or somewhere else was
  // dropped onto the query-memo - let's load their contents:
  s := TStringList.Create;
  for i:=0 to AFiles.Count-1 do begin
    if fileExists(AFiles[i]) then begin
      s.LoadFromFile(AFiles[i]);
      SynMemoQuery.Lines.AddStrings(s);
    end;
  end;
  SynMemoQuery.OnChange(self);
end;

procedure TMDIChild.SynMemoQueryKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  SynMemoQuery.OnChange(self);
end;

procedure TMDIChild.SynMemoQueryMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SynMemoQuery.OnChange(Self);
end;

procedure TMDIChild.popupHostPopup(Sender: TObject);
begin
  MenuAutoupdate.Enabled := PageControl2.ActivePageIndex=1;
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

procedure TMDIChild.btnQueryFindClick(Sender: TObject);
begin
  mainform.FindDialog1.execute;
end;

procedure TMDIChild.ToolButton4Click(Sender: TObject);
begin
  SaveBlob;
  mainform.HTMLviewExecute(Sender);
end;

procedure TMDIChild.LoadSQLClick(Sender: TObject);
var
  filename : String;
  i : Integer;
begin
  // Load SQL File
  Screen.Cursor := crHourGlass;
  filename := (sender as TMenuItem).Caption;
  i := 0;
  while filename[i] <> ' ' do
    inc(i);
  filename := copy(filename, i+1, length(filename));
  filename := stringreplace(filename, '&', '', [rfReplaceAll]);

  try
    SynMemoQuery.Lines.LoadFromFile(filename);
  except
    MessageDLG('Error while reading file ''' + filename + '''', mtError, [mbOK], 0);
  end;
  Screen.Cursor := crDefault;
  SynMemoQueryChange(self);
end;

procedure TMDIChild.btnQuerySaveClick(Sender: TObject);
begin
  mainform.ButtonSaveSQLClick(self);
end;

procedure TMDIChild.btnQueryLoadClick(Sender: TObject);
begin
  Mainform.ButtonLoadSQLFile(self);
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
  // Set focus on DBMemo when user doubleclicks a (MEMO)-cell
//  showmessage((sender as TControl).classname);
  if (sender as TSMDBGrid).SelectedField.IsBlob and (PageControl4.ActivePageIndex = 0) then begin
    PageControl3.ActivePageIndex := 1;
    DBMemo1.SetFocus;
  end;
end;

procedure TMDIChild.SaveDialogExportDataTypeChange(Sender: TObject);
begin
  with SaveDialogExportData do begin
    Case FilterIndex of
      1 : DefaultExt := 'csv';
      2 : DefaultExt := 'html';
      3 : DefaultExt := 'xml';
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

procedure TMDIChild.DBGridEnter(Sender: TObject);
begin
  Mainform.ManualCopy.Enabled := true;
end;

procedure TMDIChild.DBGridExit(Sender: TObject);
begin
  Mainform.ManualCopy.Enabled := false;
end;

procedure TMDIChild.popupDataGridPopup(Sender: TObject);
var y,m,d,h,i,s,ms : Word;
begin
  DataInsertDateTime.Enabled := gridData.SelectedField.DataType in [ftString, ftDatetime, ftDate, ftTime];
  if not DataInsertDateTime.Enabled then exit;
  decodedate(now, y, m, d);
  decodetime(now, h, i, s, ms);
  DataDateTime.Caption := Format('%.4d-%.2d-%.2d %.2d:%.2d:%.2d', [y,m,d,h,i,s]);
  DataDate.Caption := Format('%.4d-%.2d-%.2d', [y,m,d]);
  DataTime.Caption := Format('%.2d:%.2d:%.2d', [h,i,s]);
  DataTimestamp.caption := Format('%.4d%.2d%.2d%.2d%.2d%.2d', [y,m,d,h,i,s]);
  DataYear.Caption := Format('%.4d', [y]);
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
    0 : clipboard.astext := ZQuery2.FieldByName(DBMemo1.DataField).AsString;
    1 : EDBImage1.CopyToClipboard;
  end;
end;

procedure TMDIChild.setNULL1Click(Sender: TObject);
begin
  if not (DataSource1.State in [dsEdit, dsInsert]) then
    DataSource1.Edit;
  gridData.SelectedField.Clear;
end;


procedure TMDIChild.ZQuery2BeforeClose(DataSet: TDataSet);
begin
  // unassign data-aware controls
  DBMemo1.DataField := '';
  EDBImage1.DataField := '';
end;


procedure TMDIChild.ExecUseQuery( DbName: String );
begin
  ExecQuery('USE ' + mask(DbName));
end;

// Execute a query without returning a resultset
procedure TMDIChild.ExecQuery( SQLQuery: String );
begin
  try
    CheckConnection;
  except
    exit;
  end;
  With TZReadOnlyQuery.Create( self ) do
  begin
    Connection := ZConn;
    SQL.Text := SQLQuery;
    ExecSQL;
    Free;
  end;
end;


// Executes a query with an existing ZQuery-object
procedure TMDIChild.GetResults( SQLQuery: String; ZQuery: TZReadOnlyQuery );
begin
  try
    CheckConnection;
  except
    exit;
  end;
  ZQuery.SQL.Text := SQLQuery;
  ZQuery.Open;
  ZQuery.DisableControls;
  ZQuery.First;
end;


// Execute a query and return string from column x
function TMDIChild.GetVar( SQLQuery: String; x: Integer = 0 ) : String;
begin
  GetResults( SQLQuery, ZQuery3 );
  Result := ZQuery3.Fields[x].AsString;
  ZQuery3.Close;
end;


// Execute a query and return column x as Stringlist
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


// Monitor SQL
procedure TMDIChild.ZSQLMonitor1LogTrace(Sender: TObject;
  Event: TZLoggingEvent);
begin
  LogSQL( Trim( Event.Message ), (Event.Category <> lcExecute) );
  if Event.Category = lcDisconnect then
  begin
    time_connected := 0;
    TimerConnected.Enabled := false;
    Mainform.ShowStatus( 'Disconnected since ' + TimeToStr(Time), 2, true );
  end;
end;



procedure TMDIChild.ResizeImageToFit;
begin
  // Resize image to fit
  if EDBImage1.Picture.Width = 0 then
    exit;
  EDBImage1.Width := MulDiv(EDBImage1.Height, EDBImage1.Picture.Width, EDBImage1.Picture.Height);
  MainForm.showstatus('Image: ' + inttostr( EDBImage1.Picture.width)
    + ' x ' + inttostr( EDBImage1.Picture.Height ) + ' pixel, '
    + 'zoomed to ' + IntToStr(round( 100 / EDBImage1.Picture.Height * EDBImage1.Height )) + '%'
    );

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
    DBMemo1.DataField := grid.SelectedField.FieldName;
    EDBImage1.DataField := grid.SelectedField.FieldName;

    // Disable text editor if there's binary data in the field,
    // since the text editor may silently corrupt it if used.
    DBMemo1.ReadOnly := hasNonLatin1Chars( DBMemo1.Field.AsString );

    PageControl3.ActivePageIndex := 1;
    MenuViewBlob.Enabled := true;
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
    DBMemo1.DataField := '';
    EDBImage1.DataField := '';
    MenuViewBlob.Enabled := false;
  end;
  if (DBMemo1.ReadOnly or (Length(DBMemo1.DataField) = 0)) then DBMemo1.Color := clInactiveCaptionText
  else DBMemo1.Color := clWindow;
  PageControl4Change(self);
end;


procedure TMDIChild.ZQuery1EditError(DataSet: TDataSet; E: EDatabaseError;
  var Action: TDataAction);
begin
  LogSQL( E.Message, true );
end;


procedure TMDIChild.FormResize(Sender: TObject);
begin
  ListTables.Width := SheetDatabase.Width - Toolbar1.Width - Toolbar1.Left;
  ListTables.Height := SheetDatabase.Height - Panel2.Height;
  Panel9.Width := SheetTable.Width - Toolbar2.Width - Toolbar2.Left;
  Panel9.Height := SheetTable.Height - Panel3.Height;
end;


// Search with searchbox
procedure TMDIChild.ButtonDataSearchClick(Sender: TObject);
var
  i : Integer;
  where : String;
begin
  if not ZQuery2.Active then
    exit;
  where := '';
  if EditDataSearch.text <> '' then for i:=0 to gridData.FieldCount-1 do
  begin
    if where <> '' then
      where := where + CRLF + ' OR ';
    where := where + gridData.Fields[i].FieldName + ' LIKE ''%' + EditDataSearch.text + '%''';
  end;
  SynMemoFilter.Text := where;

  SetFilter(self);
end;

// Searchbox focused
procedure TMDIChild.EditDataSearchEnter(Sender: TObject);
begin
  ButtonDataSearch.Default := true;
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
    openkey( regpath + '\Servers\' + description, true );
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
    MessageDlg( 'Error when writing to registry.', mtError, [mbOK], 0 );
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


// Simulate Ctrl+A-behaviour of common editors
procedure TMDIChild.DBMemo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ( Shift = [ssCtrl] ) and ( Key = Ord('A') ) then
    DBMemo1.SelectAll;
end;

function TMDIChild.mask(str: String) : String;
var
  i, o                    : byte;
  hasbadchar, iskeyword   : Boolean;
  keywords                : String;
begin
  if mysql_version >= 32300 then
  begin
    // only mask if needed
    hasbadchar := false;
    for i:=1 to length(str) do
    begin
      o := ord( str[i] );
      // digits, upper chars, lower chars and _ are allowed
      hasbadchar := not (o in [48..57, 65..90, 97..122, 95]);
      // see bug 1500753
      if (i = 1) then
        hasbadchar := o in [48..57];
      if hasbadchar then
        break;
    end;

    // found a list with keywords:
    // http://dev.mysql.com/doc/refman/5.1/en/reserved-words.html
    keywords := ' ADD ALL ALTER ANALYZE AND AS ASC BEFORE BETWEEN BIGINT BINARY '+
      'BLOB BOTH BY CASCADE CASE CHANGE CHAR CHARACTER CHECK COLLATE '+
      'COLUMN COLUMNS CONSTRAINT CONVERT CREATE CROSS CURRENT_DATE '+
      'CURRENT_TIME CURRENT_TIMESTAMP CURRENT_USER DATABASE DATABASES '+
      'DAY_HOUR DAY_MICROSECOND DAY_MINUTE DAY_SECOND DEC DECIMAL '+
      'DEFAULT DELAYED DELETE DESC DESCRIBE DISTINCT DISTINCTROW DIV '+
      'DOUBLE DROP DUAL ELSE ENCLOSED ESCAPED EXISTS EXPLAIN FALSE '+
      'FIELDS FLOAT FLOAT4 FLOAT8 FOR FORCE FOREIGN FROM FULLTEXT '+
      'GRANT GROUP HAVING HIGH_PRIORITY HOUR_MICROSECOND HOUR_MINUTE '+
      'HOUR_SECOND IF IGNORE IN INDEX INFILE INNER INSERT INT INT1 '+
      'INT2 INT3 INT4 INT8 INTEGER INTERVAL INTO IS JOIN KEY KEYS '+
      'KILL LEADING LEFT LIKE LIMIT LINES LOAD LOCALTIME LOCALTIMESTAMP '+
      'LOCK LONG LONGBLOB LONGTEXT LOW_PRIORITY MATCH MEDIUMBLOB '+
      'MEDIUMINT MEDIUMTEXT MIDDLEINT MINUTE_MICROSECOND MINUTE_SECOND MOD '+
      'NATURAL NOT NO_WRITE_TO_BINLOG NULL NUMERIC ON OPTIMIZE OPTION '+
      'OPTIONALLY OR ORDER OUTER OUTFILE PRECISION PRIMARY PRIVILEGES '+
      'PROCEDURE PURGE RAID0 READ REAL REFERENCES REGEXP RENAME '+
      'REPLACE REQUIRE RESTRICT REVOKE RIGHT RLIKE SECOND_MICROSECOND '+
      'SELECT SEPARATOR SET SHOW SMALLINT SONAME SPATIAL SQL_BIG_RESULT '+
      'SQL_CALC_FOUND_ROWS SQL_SMALL_RESULT SSL STARTING STRAIGHT_JOIN TABLE '+
      'TABLES TERMINATED THEN TINYBLOB TINYINT TINYTEXT TO TRAILING '+
      'TRUE UNION UNIQUE UNLOCK UNSIGNED UPDATE USAGE USE USING '+
      'UTC_DATE UTC_TIME UTC_TIMESTAMP VALUES VARBINARY VARCHAR VARCHARACTER '+
      'VARYING WHEN WHERE WITH WRITE X509 XOR YEAR_MONTH ZEROFILL '+
      'ASENSITIVE CALL CONDITION CONNECTION CONTINUE CURSOR DECLARE '+
      'DETERMINISTIC EACH ELSEIF EXIT FETCH GOTO INOUT INSENSITIVE ITERATE '+
      'LABEL LEAVE LOOP MODIFIES OUT READS RELEASE REPEAT RETURN SCHEMA SCHEMAS '+
      'SENSITIVE SPECIFIC SQL SQLEXCEPTION SQLSTATE SQLWARNING TRIGGER UNDO UPGRADE '+
      'WHILE ACCESSIBLE LINEAR RANGE READ_ONLY READ_WRITE ';
    iskeyword := ( pos( ' '+UpperCase(str)+' ', keywords ) > 0 );

    if hasbadchar or iskeyword then
    begin
      result := StringReplace(str, '`', '``', [rfReplaceAll]);
      result := '`' + result + '`';
    end
    else
      result := str;
  end
  else
    result := str;
end;





procedure TMDIChild.CheckConnection;
var
  status: boolean;
begin
  status := ZConn.Ping;
  if not status then begin
    LogSQL('Connection failure detected. Trying to reconnect.', true);
    ZConn.Reconnect;
    PerformConnect;
  end;
end;

function TMDIChild.GetActiveGrid: TSMDBGrid;
begin
  Result := nil;
  if PageControl1.ActivePage = SheetData then Result := gridData;
  if PageControl1.ActivePage = SheetQuery then Result := gridQuery;
end;

procedure TMDIChild.ZQueryBeforeSendingSQL(DataSet: TDataSet);
begin
  try
    CheckConnection;
  except
    exit;
  end;
end;

end.

