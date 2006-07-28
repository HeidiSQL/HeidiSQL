UNIT Childwin;


// -------------------------------------
// HeidiSQL
// MDI-Child-Window
// -------------------------------------


INTERFACE

uses Windows, Classes, Graphics, Forms, Controls, StdCtrls,
  ExtCtrls, ComCtrls, ImgList, SysUtils, Dialogs, Menus, SortListView,
  SynEdit, SynMemo, SynEditHighlighter, SynHighlighterSQL,
  Registry, Spin, Clipbrd, Shellapi,
  Buttons, CheckLst, ToolWin, Db, DBGrids,
  DBCtrls, helpers,
  Grids, messages, smdbgrid, Mask, ZDataset,
  ZAbstractRODataset, ZConnection,
  ZSqlMonitor, ZPlainMySqlDriver, EDBImage, ZAbstractDataset, ZDbcLogging;


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
    PopupMenu1: TPopupMenu;
    Drop1: TMenuItem;
    Panel2: TPanel;
    SheetTable: TTabSheet;
    Panel3: TPanel;
    pmenu2: TPopupMenu;
    menuviewdata: TMenuItem;
    menuproperties: TMenuItem;
    menuinsert: TMenuItem;
    menudroptable: TMenuItem;
    menuemptytable: TMenuItem;
    SheetHost: TTabSheet;
    PageControl2: TPageControl;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    VariabelListe: TSortListView;
    ProcessListe: TSortListView;
    PopupMenu2: TPopupMenu;
    Kill1: TMenuItem;
    NewDatabase1: TMenuItem;
    Tabellenliste: TSortListView;
    Refresh1: TMenuItem;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton13: TToolButton;
    ToolBar2: TToolBar;
    menurefresh: TMenuItem;
    N2: TMenuItem;
    Panel7: TPanel;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    SynSQLSyn1: TSynSQLSyn;
    SynMemo1: TSynMemo;
    Splitter3: TSplitter;
    menucreatetable: TMenuItem;
    OpenDialog1: TOpenDialog;
    Timer1: TTimer;
    PopupMenu3: TPopupMenu;
    Refresh2: TMenuItem;
    DropField1: TMenuItem;
    N3: TMenuItem;
    N5: TMenuItem;
    PopupmenuDropDatabase: TMenuItem;
    ButtonInsert: TToolButton;
    PopupMenuData: TPopupMenu;
    Refresh3: TMenuItem;
    Insertrecord2: TMenuItem;
    ButtonAddField: TToolButton;
    MenuAddField: TMenuItem;
    ButtonEditField: TToolButton;
    MenuEditField: TMenuItem;
    Timer2: TTimer;
    Result: TPopupMenu;
    Copyrecords1: TMenuItem;
    CopyasCSVData1: TMenuItem;
    N9: TMenuItem;
    Label4: TLabel;
    MenuAdvancedProperties: TMenuItem;
    N10: TMenuItem;
    MenuRenameTable: TMenuItem;
    MenuViewBlob: TMenuItem;
    Timer3: TTimer;
    Timer4: TTimer;
    N12: TMenuItem;
    MenuTableComment: TMenuItem;
    PopupMenu4: TPopupMenu;
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
    N16: TMenuItem;
    More1: TMenuItem;
    Timer5: TTimer;
    PopupMenuDropTable: TMenuItem;
    N17: TMenuItem;
    Panel9: TPanel;
    FeldListe: TSortListView;
    CopycontentsasHTML1: TMenuItem;
    CopycontentsasHTML2: TMenuItem;
    Copy3: TMenuItem;
    Paste2: TMenuItem;
    N4: TMenuItem;
    DBGrid1: TSMDBGrid;
    DataSource1: TDataSource;
    DBGrid2: TSMDBGrid;
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
    PopupMenu7: TPopupMenu;
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
    SynMemo2: TSynMemo;
    ToolBar3: TToolBar;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    PageControl4: TPageControl;
    TabSheet3: TTabSheet;
    DBMemo1: TDBMemo;
    TabSheet4: TTabSheet;
    SynMemo3: TSynMemo;
    ToolButton14: TToolButton;
    MenuTabelleLoeschen: TToolButton;
    N18: TMenuItem;
    MenuChangeType5: TMenuItem;
    MenuChangeType6: TMenuItem;
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
    ButtonRunQuery: TToolButton;
    ButtonRunSelection: TToolButton;
    ButtonLoadSQL: TToolButton;
    ButtonSaveSQL: TToolButton;
    ToolButton15: TToolButton;
    PanelCharsInQueryWindow: TPanel;
    ToolButtonStopOnErrors: TToolButton;
    Panel10: TPanel;
    ComboBoxWhereFilters: TComboBox;
    ToolBar4: TToolBar;
    ToolButton18: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton17: TToolButton;
    ToolButton16: TToolButton;
    BtnPreviousFilter: TToolButton;
    BtnNextFilter: TToolButton;
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
    ToolButton19: TToolButton;
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
    ToolButton4: TToolButton;
    HTMLview1: TMenuItem;
    ToolButton5: TToolButton;
    ScrollBox1: TScrollBox;
    InsertfilesintoBLOBfields1: TMenuItem;
    InsertfilesintoBLOBfields2: TMenuItem;
    InsertfilesintoBLOBfields3: TMenuItem;
    N19: TMenuItem;
    Gemini1: TMenuItem;
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
    PopupMenuTablelistColumns: TPopupMenu;
    DefaultColumnLayout1: TMenuItem;
    N20: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ReadDatabasesAndTables(Sender: TObject);
    procedure DBtreeChange(Sender: TObject; Node: TTreeNode);
    procedure pcChange(Sender: TObject);
    procedure viewdata(Sender: TObject);
    procedure ShowDBProperties(Sender: TObject);
    procedure ShowTableProperties(Sender: TObject);
    procedure TabellenlisteChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure TabelleAnzeigen(Sender: TObject);
    procedure TabelleLeeren(Sender: TObject);
    procedure TabelleLoeschen(Sender: TObject);
    procedure DBLoeschen(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure showstatus(msg: string='';  panel : Integer=0;  Icon: Integer=50);
    procedure LogSQL(msg: string = ''; comment: Boolean = true );
    procedure ShowVariablesAndProcesses(Sender: TObject);
    procedure ProcessListeChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure CreateDatabase(Sender: TObject);
    procedure KillProcess(Sender: TObject);
    procedure PageControl2Change(Sender: TObject);
    procedure ExecSQLClick(Sender: TObject; Selection: Boolean = false; CurrentLine: Boolean=false);
    procedure FeldListeChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure DropField(Sender: TObject);
    procedure SynMemo1Change(Sender: TObject);
    procedure CreateTable(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UpdateField(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure MenuAdvancedPropertiesClick(Sender: TObject);
    procedure TabellenlisteEdited(Sender: TObject; Item: TListItem;
      var S: String);
    procedure MenuRenameTableClick(Sender: TObject);
    procedure MenuViewBlobClick(Sender: TObject);
    procedure Timer3Timer(Sender: TObject);
    procedure Timer4Timer(Sender: TObject);
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
    procedure TabellenlisteDblClick(Sender: TObject);
    procedure Timer5Timer(Sender: TObject);
    procedure DBGrid1TitleClick(Column: TColumn);
    procedure Filter1Click(Sender: TObject);
    procedure MenuLimitClick(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure QuickFilterClick(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure PageControl4Change(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure ToolButton11Click(Sender: TObject);
    procedure ToolButton12Click(Sender: TObject);
    procedure setFilter(Sender: TObject);
    procedure ClearFilter(Sender: TObject);
    procedure LoadSQLWhereFile(Sender: TObject);
    procedure DropFilter1Click(Sender: TObject);
    procedure MenuChangeTypeClick(Sender: TObject);
    procedure MenuChangeTypeOtherClick(Sender: TObject);
    procedure InsertRecord(Sender: TObject);
    procedure ZQuery2AfterOpen(DataSet: TDataSet);
    procedure selectall1Click(Sender: TObject);
    procedure ResultPopup(Sender: TObject);
    procedure DBGrid1ColumnMoved(Sender: TObject; FromIndex,
      ToIndex: Integer);
    procedure Autoupdate1Click(Sender: TObject);
    procedure EnableAutoRefreshClick(Sender: TObject);
    procedure ShowProcessList(sender: TObject);
    procedure DisableAutoRefreshClick(Sender: TObject);
    procedure SynMemo1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SynMemo1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SynMemo1DropFiles(Sender: TObject; X, Y: Integer;
      AFiles: TStrings);
    procedure SynMemo1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure TabellenlisteEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure Saveastextfile1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure ToolButton15Click(Sender: TObject);
    procedure LoadSQLClick(Sender: TObject);
    procedure ButtonSaveSQLClick(Sender: TObject);
    procedure ButtonLoadSQLClick(Sender: TObject);
    procedure BtnPreviousFilterClick(Sender: TObject);
    procedure BtnNextFilterClick(Sender: TObject);
    procedure ComboBoxWhereFiltersChange(Sender: TObject);
    procedure ToolButtonStopOnErrorsClick(Sender: TObject);
    procedure DBGridDblClick(Sender: TObject);
    procedure SaveDialogExportDataTypeChange(Sender: TObject);
    procedure DBGridGetCellParams(Sender: TObject; Field: TField;
      AFont: TFont; var Background: TColor; Highlight: Boolean);
    procedure DBGridEnter(Sender: TObject);
    procedure DBGridExit(Sender: TObject);
    procedure PopupMenuDataPopup(Sender: TObject);
    procedure InsertDate(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure setNULL1Click(Sender: TObject);
    procedure MenuAddFieldClick(Sender: TObject);
    procedure ZQuery2BeforeClose(DataSet: TDataSet);
    procedure ExecQuery( SQLQuery: String );
    procedure ExecUseQuery( DbName: String );
    function GetVar( SQLQuery: String; x: Integer = 0 ) : String;
    procedure GetResults( SQLQuery: String; ZQuery: TZReadOnlyQuery );
    procedure ZSQLMonitor1LogTrace(Sender: TObject; Event: TZLoggingEvent);
    procedure ResizeImageToFit;
    procedure Splitter2Moved(Sender: TObject);
    procedure DBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure ZSQLMonitor1Trace(Sender: TObject; Event: TZLoggingEvent;
      var LogTrace: Boolean);
    procedure ZQuery1EditError(DataSet: TDataSet; E: EDatabaseError;
      var Action: TDataAction);
    procedure FormResize(Sender: TObject);
    procedure ButtonDataSearchClick(Sender: TObject);
    procedure EditDataSearchEnter(Sender: TObject);
    procedure EditDataSearchExit(Sender: TObject);
    procedure TabellenlisteColumnRightClick(Sender: TObject;
      Column: TListColumn; Point: TPoint);
    procedure MenuTablelistColumnsClick(Sender: TObject);
    procedure TabellenlisteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DBMemo1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    function mask(str: String) : String;

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

    public
      { Public declarations }
      ActualDatabase, ActualTable: string;
      dataselected, editing      : Boolean;
      mysql_version              : Integer;
      tnodehost                  : TTreeNode;
      OnlyDBs2                   : TStringList;
      Description                : String;
      DBRightClickSelectedItem   : TTreeNode;    // TreeNode for dropping with right-click
  end;


// --------------------------------------------------------------------------------------
IMPLEMENTATION

uses
  connections, Main, createtable, fieldeditor, tbl_properties,
  tblcomment, selectsomedatabases, optimizetables, copytable,
  mysqlerror;



const
	CRLF = #13#10;
  SQL_PING = 'SELECT 1';

{$R *.DFM}



procedure TMDIChild.FormCreate(Sender: TObject);
var
  AutoReconnect    : Boolean;
begin
  // initialization: establish connection and read some vars from registry
  Screen.Cursor := crHourGlass;
  mainform.Showstatus('Creating window...', 2, 51);

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
    ZConn.Connect;
  except
    on E: Exception do
    begin
      MessageDlg( E.Message, mtError, [mbOK], 0 );
      Screen.Cursor := crDefault;
      timer5.Enabled := true;
      Exit;
    end;
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
  with TRegistry.Create do begin
    openkey(regpath, true);
    WriteBool('AutoReconnect', true);
    closekey();
  end;

  ActualDatabase := '';
  ActualTable := '';
  Screen.Cursor := crDefault;

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
        SynMemo1.Font.Name := ReadString('FontName');
        SynMemo2.Font.Name := ReadString('FontName');
        SynMemo1.Font.Size := ReadInteger('FontSize');
        SynMemo2.Font.Size := ReadInteger('FontSize');
      end;

      // Data-Font:
      if (ValueExists('DataFontName')) and (ValueExists('DataFontSize')) then begin
        DBGrid1.Font.Name := ReadString('DataFontName');
        DBGrid2.Font.Name := ReadString('DataFontName');
        DBMemo1.Font.Name := ReadString('DataFontName');
        DBGrid1.Font.Size := ReadInteger('DataFontSize');
        DBGrid2.Font.Size := ReadInteger('DataFontSize');
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
      PopUpMenu7.Items.Clear;
      while ValueExists('SQLWhereFile'+inttostr(i)) do begin
        menuitem := Tmenuitem.Create(self);
        menuitem.Caption := inttostr(PopUpMenu7.Items.count+1) + ' ' + ReadString('SQLWhereFile'+inttostr(i));
        menuitem.OnClick := LoadSQLWhereFile;
        PopUpMenu7.Items.Add(menuitem);
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


procedure TMDIChild.showstatus(msg: string='';  panel : Integer=0;  Icon: Integer=50);
begin
  if mainform.ActiveMDIChild <> self then
    exit;
  mainform.showstatus(msg, panel, Icon);
end;


procedure TMDIChild.LogSQL(msg: string = ''; comment: Boolean = true);
begin
  // add a sql-command or info-line to history-memo
  while SynMemo2.Lines.Count > mainform.logsqlnum do
  begin
    SynMemo2.Lines.Delete(0);
  end;
  msg := Copy(msg, 0, 2000);
  msg := StringReplace( msg, #9, ' ', [rfReplaceAll] );
  msg := StringReplace( msg, #10, ' ', [rfReplaceAll] );
  msg := StringReplace( msg, #13, ' ', [rfReplaceAll] );
  msg := StringReplace( msg, '  ', ' ', [rfReplaceAll] );
  if comment then
    msg := '/* ' + msg + ' */';
  SynMemo2.Lines.Add(msg);
  SynMemo2.SetBookMark(0,0,SynMemo2.Lines.Count);
  SynMemo2.GotoBookMark(0);
  SynMemo2.ClearBookMark(0);
  SynMemo2.Repaint;
end;


procedure TMDIChild.ReadDatabasesAndTables(Sender: TObject);
var
  tnode, tchild, tmpSelected: TTreeNode;
  i, j : Integer;
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
  mainform.Showstatus('Reading Databases...', 2, 51);
  if OnlyDBs.Count = 0 then
  begin
    OnlyDBs2 := TStringList.Create;
    GetResults( 'SHOW DATABASES', ZQuery3 );
    for i:=1 to ZQuery3.RecordCount do
    begin
      OnlyDBs2.Add(ZQuery3.FieldByName('Database').AsString);
      ZQuery3.Next;
    end;
    zconn.Database := ZQuery3.FieldByName('Database').AsString;
  end else
    OnlyDBs2 := OnlyDBs;
  if OnlyDBs2.Count > 50 then with SelectFromManyDatabases do begin
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
      ZQuery3.Next;
    end;
  except
    on E : Exception do
    begin
      LogSQL( 'Could not open database ''' + OnlyDBs2[i] + ''' - ignoring: ' + e.Message );
      continue;
    end;
  end;

  showstatus(inttostr(OnlyDBs2.count) + ' Databases');
  tnodehost.Expand(false);
  DBTree.OnChange := DBtreeChange;
  if tmpSelected <> nil then
    DBTree.Selected := tmpSelected
  else
    DBTree.Selected := tnodehost;
  DBtreeChange(self, tnodehost);
  mainform.Showstatus('Ready', 2);
  Screen.Cursor := crDefault;
end;



procedure TMDIChild.DBtreeChange(Sender: TObject; Node: TTreeNode);
var
  strdb, strtable : String;
begin
  // react on dbtree-clicks
  Screen.Cursor := crHourGlass;
  case Node.Level of
    0 : begin                                   // Root / Host chosen
      if not DBTree.Dragging then
        PageControl1.ActivePage := SheetHost;
      SheetDatabase.TabVisible := false;
      SheetTable.TabVisible := false;
      SheetData.TabVisible := false;
      ActualDatabase := '';
      ActualTable := '';
      Caption := Description;
    end;
    1 : begin                                   // DB chosen
      SheetDatabase.TabVisible := true;
      SheetTable.TabVisible := false;
      SheetData.TabVisible := false;
      if not DBTree.Dragging then
        PageControl1.ActivePage := SheetDatabase;
      strdb := Node.Text;
      showstatus(strdb +': ' + inttostr(Node.count) +' table(s)');
      if ActualDatabase <> strdb then
      begin
        ActualDatabase := strdb;
        ActualTable := '';
        ToolButton9.Enabled := False;
        FeldListe.Items.Clear;
        Panel3.Caption := 'Table-Properties';
        ShowDBProperties(self);
      end;
      Caption := Description + ' - /' + ActualDatabase;
    end;
    2 : begin                                   // Table chosen
      SheetDatabase.TabVisible := true;
      SheetTable.TabVisible := true;
      SheetData.TabVisible := true;
      strdb := Node.Parent.Text;
      strtable := Node.Text;
      if (ActualDatabase <> strdb) or (ActualTable <> strtable) then
        dataselected := false;
      if ActualDatabase <> strdb then begin
        ActualDatabase := strdb;
        ShowDBProperties(self);
      end;
      if (ActualTable <> strtable) or (ActualDatabase <> strdb) then begin
        ActualTable := strtable;
        ShowTableProperties(self);
      end;
      showstatus(strdb + ': '+strtable +': ' + inttostr(FeldListe.Items.count) +' field(s)');
//      if not dataselected then pcChange(self);
      Caption := Description + ' - /' + ActualDatabase + '/' + ActualTable;
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
    SynMemo3.Text := '';
    DBGrid1.SortColumns.Clear;
    // Read cached WHERE-clause and set filter
    reg_value := 'WHERECLAUSE_' + ActualDatabase + '.' + ActualTable;
    if reg.ValueExists( reg_value ) then
      SynMemo3.Text := reg.ReadString( reg_value );
    // Read cached ORDER-clause and set Grid.Sortcolumns
    reg_value := 'ORDERCLAUSE_' + ActualDatabase + '.' + ActualTable;
    if reg.ValueExists( reg_value ) then
    begin
      orderclauses := explode( ',', reg.ReadString( reg_value ) );
      for i:=0 to orderclauses.Count-1 do
      begin
        columnname := trim( copy( orderclauses[i], 0, pos( ' ', orderclauses[i] ) ) );
        columnexists := false;
        for j:=0 to FeldListe.Items.Count-1 do
        begin
          if FeldListe.Items[j].Caption = columnname then
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
        with DBGrid1.SortColumns.Add do
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
  for i:=0 to dbgrid1.SortColumns.Count-1 do
  begin
    with dbgrid1.SortColumns[i] do
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
      DBGrid1.Options := DBGrid1.Options + [dgAlwaysShowEditor];
      DBGrid2.Options := DBGrid2.Options + [dgAlwaysShowEditor];
    end
    else
    begin
      DBGrid1.Options := DBGrid1.Options - [dgAlwaysShowEditor];
      DBGrid2.Options := DBGrid2.Options - [dgAlwaysShowEditor];
    end;
    PageControl1.ActivePage := SheetData;

    ZConn.Database := ActualDatabase;
    ZQuery2.Close;
    ZQuery2.SQL.Clear;
    ZQuery2.SQL.Add( 'SELECT * FROM ' + mask(ActualTable) );
    if trim(self.SynMemo3.Text) <> '' then
      ZQuery2.SQL.Add( 'WHERE ' + trim(self.SynMemo3.Text) );
    if sorting <> '' then
      ZQuery2.SQL.Add( sorting );
    if mainform.CheckBoxLimit.Checked then
      ZQuery2.SQL.Add('LIMIT ' + intToStr(mainform.UpDownLimitStart.Position) + ', ' + intToStr(mainform.UpDownLimitEnd.position) );
    try
      ZQuery2.Open;
    except
      on E:Exception do
      begin
        MessageDlg(E.Message , mtError, [mbOK], 0);
        LogSQL( E.Message );
        ZQuery2.Active := false;
        viewingdata := false;
        Screen.Cursor := crDefault;
        exit;
      end;
    end;

    for i:=0 to FeldListe.Items.Count-1 do
    begin
      Columns.Add( FeldListe.Items[i].Caption );

      // give all enum-fields a PickList with its Items
      if StrCmpBegin('enum', FeldListe.Items[i].SubItems[0]) then begin
        DropDown := explode(''',''', getklammervalues(FeldListe.Items[i].SubItems[0]));
        for j:=0 to DropDown.count-1 do
        begin
          DropDown[j] := trimc(DropDown[j], '''');
        end;
        for j:=0 to DBGrid1.Columns.count-1 do
        begin
          if DBGrid1.Columns[j].FieldName = FeldListe.Items[i].Caption then
            DBGrid1.Columns[j].PickList := DropDown;
        end;
      end;

      // make PK-columns = fsBold
      for j:=0 to DBGrid1.Columns.count-1 do
      begin
        if (DBGrid1.Columns[j].FieldName = FeldListe.Items[i].Caption) and
          (FeldListe.Items[i].ImageIndex = 26) then
        begin
          PrimaryKeyColumns.Add( FeldListe.Items[i].Caption );
        end;
      end;

    end;

    for j:=0 to DBGrid1.Columns.count-1 do
    begin
      // for letting NULLs being inserted into "NOT NULL" fields
      // in mysql5+, the server rejects inserts with NULLs in NOT NULL-fields,
      // so the Required-check on client-side is not needed at any time 
      ZQuery2.Fields[j].Required := false;

      // set column-width
      if (Mainform.DefaultColWidth <> 0) and (DBGrid1.Columns[j].Width > Mainform.DefaultColWidth) then
      begin
        DBGrid1.Columns[j].Width := Mainform.DefaultColWidth;
      end;

      // make PK-columns = fsBold
      for i:=0 to PrimaryKeyColumns.Count-1 do
      begin
        if (PrimaryKeyColumns[i] = DBGrid1.Columns[j].Fieldname) then
        begin
          DBGrid1.Columns[j].Font.Style := DBGrid1.Columns[j].Font.Style + [fsBold];
          DBGrid1.Columns[j].Color := $02EEEEEE;
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
  //  showmessage('pcchange!!!');
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
end;


procedure TMDIChild.ShowDBProperties(Sender: TObject);
var
  n               : TListItem;
  i,j,k,t,u,bytes : Integer;
  tndb            : TTreenode;
  menuitem        : TMenuItem;
  TablelistColumns: TStringList;
  column          : TListColumn;
begin
  // DB-Properties
  Screen.Cursor := crHourGlass;
  Mainform.ButtonDropDatabase.Hint := 'Drop Database...|Drop Database ' + ActualDatabase + '...';

  ZConn.Database := ActualDatabase;
  ExecUseQuery( ActualDatabase );

  Try
    if mysql_version >= 32300 then begin
      // get quick results with versions 3.23.xx and newer
      GetResults( 'SHOW TABLE STATUS', ZQuery3 );

      // Generate items for PopupMenuTablelistColumns
      for i:=PopupMenuTablelistColumns.Items.Count-1 downto 2 do
        PopupMenuTablelistColumns.Items.Delete( i );
      with TRegistry.Create do
      begin
        openkey( regpath + '\Servers\' + description, true );
        if ValueExists( 'TablelistDefaultColumns' ) then
          PopupMenuTablelistColumns.Items[0].Checked := ReadBool( 'TablelistDefaultColumns' );
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
        PopupMenuTablelistColumns.Items.Add( menuitem );
      end;

      Tabellenliste.Columns.BeginUpdate;
      Tabellenliste.Items.BeginUpdate;
      Tabellenliste.Columns.Clear;
      column := Tabellenliste.Columns.Add;
      column.Caption := 'Table';
      column.Width := -1;
      if PopupMenuTablelistColumns.Items[0].Checked then
      begin // Default columns - initialize column headers
        column := Tabellenliste.Columns.Add;
        column.Caption := 'Records';
        column.Alignment := taRightJustify;
        column.Width := 80;

        column := Tabellenliste.Columns.Add;
        column.Caption := 'Size';
        column.Alignment := taRightJustify;
        column.Width := -1;

        column := Tabellenliste.Columns.Add;
        column.Caption := 'Created';
        column.Width := -1;

        column := Tabellenliste.Columns.Add;
        column.Caption := 'Updated';
        column.Width := -1;

        column := Tabellenliste.Columns.Add;
        column.Caption := 'Type';
        column.Width := -1;

        column := Tabellenliste.Columns.Add;
        column.Caption := 'Comment';
        column.Width := -1;
      end;
      for i:=0 to TablelistColumns.Count-1 do
      begin
        column := Tabellenliste.Columns.Add;
        column.Caption := TablelistColumns[i];
        column.Width := -1;
        column.MinWidth := 50;
        column.Autosize := true;
      end;

      Tabellenliste.Items.Clear;
      for i := 1 to ZQuery3.RecordCount do
      begin
        n := Tabellenliste.Items.Add;
        n.ImageIndex := 39;
        // Table
        n.Caption := ZQuery3.FieldByName('Name').AsString;
        if PopupMenuTablelistColumns.Items[0].Checked then
        begin // Default columns
          // Records
          n.SubItems.Add( ZQuery3.FieldByName('Rows').AsString );
          // Size: Data_length + Index_length
          bytes := ZQuery3.FieldByName('Data_length').AsInteger + ZQuery3.FieldByName('Index_length').AsInteger;
          n.SubItems.Add(format('%d KB', [bytes div 1024 + 1]));
          // Created:
          n.SubItems.Add( DateTimeToStr(ZQuery3.FieldByName('Create_time').AsDateTime) );
          // Updated:
          n.SubItems.Add( DateTimeToStr(ZQuery3.FieldByName('Update_time').AsDateTime) );
          // Type
          Try // Until 4.x
            n.SubItems.Add( ZQuery3.FieldByName('Type').AsString );
          Except // Since 5.x
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
                Tabellenliste.Columns[n.SubItems.Count].Alignment := taRightJustify
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
        n := Tabellenliste.Items.Add;
        n.Caption := ZQuery3.Fields[0].AsString;
        n.ImageIndex := 39;
        n.SubItems.Add( GetVar( 'SELECT COUNT(*) FROM '+ZQuery3.Fields[0].AsString ) );
        ZQuery3.Next;
      end;
    end;
  Finally
    Tabellenliste.Columns.EndUpdate;
    Tabellenliste.Items.EndUpdate;
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
  for t:=0 to TabellenListe.Items.Count-1 do
  begin
    with DBtree.Items.AddChild(tndb, TabellenListe.Items[t].Caption) do begin
      ImageIndex := 39;
      selectedIndex := 40;
    end;
  end;

  Panel2.Caption := 'Database ' + ActualDatabase + ': ' + inttostr(TabellenListe.Items.Count) + ' table(s)';
  Screen.Cursor := crDefault;
end;



procedure TMDIChild.ShowTableProperties(Sender: TObject);
var
  i,j : Integer;
  n : TListItem;
  tn, tndb : TTreeNode;
  isFulltext : Boolean;
begin
  // Table-Properties
  Screen.Cursor := crHourGlass;
  if (PageControl1.ActivePage <> SheetData) and (not DBTree.Dragging) then
    PageControl1.ActivePage := SheetTable;
  Panel3.Caption := 'Table-Properties for ' + ActualDatabase + ': ' + ActualTable;

  // Tabelle auch im TreeView selektieren:
  with DBTree do begin
    if Selected.Level = 1 then tndb := Selected
    else if Selected.Level = 2 then tndb := Selected.Parent
    else if Selected.Level = 3 then tndb := Selected.Parent.Parent
    else exit;
  end;
  tn := tndb.getFirstChild;
  for i:=0 to tndb.Count -1 do begin
    if ActualTable = tn.Text then begin
      DBTree.Selected := tn; // select table
      break;
    end;
    tn := tndb.GetNextChild(tn);
  end;

  Feldliste.Items.BeginUpdate;
  FeldListe.Items.Clear;
  Try
    GetResults( 'SHOW FIELDS FROM ' + mask(ActualTable), ZQuery3 );
    for i:=1 to ZQuery3.RecordCount do
    begin
      n := FeldListe.Items.Add;
      if ZQuery3.FieldByName('Key').AsString = 'PRI' then
        n.ImageIndex := 26
      else if ZQuery3.FieldByName('Key').AsString = 'UNI' then
        n.ImageIndex := 64
      else if ZQuery3.FieldByName('Key').AsString = 'MUL' then
        n.ImageIndex := 63
      else
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
    Feldliste.Items.EndUpdate;
    Screen.Cursor := crDefault;
  end;


  Screen.Cursor := crHourglass;
  GetResults( 'SHOW KEYS FROM ' + mask(ActualTable), ZQuery3 );
  for i:=1 to ZQuery3.RecordCount do
  begin
    // primary key
    if ZQuery3.FieldByName('Key_name').AsString = 'PRIMARY' then
    begin
      for j:=0 to FeldListe.Items.Count-1 do
      begin
        if ZQuery3.FieldByName('Column_name').AsString = FeldListe.Items[j].Caption then
        begin
          FeldListe.Items[j].ImageIndex := 26;
          break;
        end;
      end;
    end;

    // index
    if (ZQuery3.FieldByName('Key_name').AsString <> 'PRIMARY') and (ZQuery3.FieldByName('Non_unique').AsString = '1') then
    begin
      for j:=0 to FeldListe.Items.Count-1 do
      begin
        if ZQuery3.FieldByName('Column_name').AsString = FeldListe.Items[j].Caption then
        begin
          FeldListe.Items[j].ImageIndex := 63;
          break;
        end;
      end;
    end;

    // unique
    if (ZQuery3.FieldByName('Key_name').AsString <> 'PRIMARY') and (ZQuery3.FieldByName('Non_unique').AsString = '0') then
    begin
      for j:=0 to FeldListe.Items.Count-1 do
      begin
        if ZQuery3.FieldByName('Column_name').AsString = FeldListe.Items[j].Caption then
        begin
          FeldListe.Items[j].ImageIndex := 64;
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
      for j:=0 to FeldListe.Items.Count-1 do
      begin
        if ZQuery3.FieldByName('Column_name').AsString = FeldListe.Items[j].Caption then
        begin
          FeldListe.Items[j].ImageIndex := 65;
          break;
        end;
      end;
    end;
    ZQuery3.Next;
  end;

  Screen.Cursor := crDefault;
end;


procedure TMDIChild.TabellenlisteChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var someselected : Boolean;
begin
  someselected := (Tabellenliste.Selected <> nil);
  // Tabelle aus der DB ausgewählt
  with Toolbar1 do
  begin
    Toolbutton2.Enabled := someselected; // eigenschaften
    menuproperties.Enabled := someselected;
    Toolbutton3.Enabled := someselected; //einfügen
    menuinsert.Enabled := someselected;
    Toolbutton1.Enabled := someselected; //anzeigen
    menuviewdata.Enabled := someselected;
    Toolbutton13.Enabled := someselected; // leeren
    menuemptytable.Enabled := someselected;
    MenuAdvancedProperties.Enabled := someselected;
    MenuRenameTable.Enabled := someselected;
    MenuDropTable.Enabled := someselected;
    MenuChangeType1.Enabled := someselected;
    MenuChangeType2.Enabled := someselected;
    MenuChangeType3.Enabled := someselected;
    MenuChangeType4.Enabled := someselected;
    MenuChangeType5.Enabled := someselected;
    MenuChangeType6.Enabled := someselected;
    MenuChangeTypeOther.Enabled := someselected;
    Mainform.CopyTable.Enabled := someselected;
    MenuTabelleLoeschen.Enabled := someselected;
    if someselected then
      ActualTable := Tabellenliste.Selected.Caption
    else
      ActualTable := '';
    MenuTableComment.Enabled := someselected;
    MenuOptimize.Enabled := someselected;
    MenuCheck.Enabled := someselected;
    MenuAnalyze.Enabled := someselected;
    MenuRepair.Enabled := someselected;
  end;

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
    if Tabellenliste.Selected.Caption = tn.Text then
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
  if Tabellenliste.SelCount = 0 then
    exit;
  t := TStringlist.Create;
  with Tabellenliste do
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


procedure TMDIChild.TabelleLoeschen(Sender: TObject);
var
  i,j : Integer;
  tn, tndb : TTreeNode;
  t : TStringList;
begin
  // Drop Table(s)
  t := TStringlist.Create;

  if (Sender as TComponent).Name = 'PopupMenuDropTable' then begin
    // delete-command was sended by dbtree-popupmenu:
    t.add(mask(DBRightClickSelectedItem.Parent.text) + '.' + mask(DBRightClickSelectedItem.text));
  end
  else with Tabellenliste do begin
    // delete-command was sended by tabellenliste-popupmenu:
    for i:=0 to Items.count-1 do
      if Items[i].Selected then
        t.add(mask(Items[i].Caption));
    if t.count = 0 then
      exit;
  end;

  if MessageDlg('Drop ' + inttostr(t.count) + ' Table(s) ?' + crlf + '(' + implodestr(', ', t) + ')', mtConfirmation, [mbok,mbcancel], 0) <> mrok then
    exit;

  Screen.Cursor := crSQLWait;
  ExecQuery( 'DROP TABLE ' + implodestr(', ', t) );


  if DBTree.Selected.Level = 1 then tndb := DBTree.Selected
  else if DBTree.Selected.Level = 2 then tndb := DBTree.Selected.Parent
  else exit;

  for i:=0 to t.count-1 do begin
    // delete it in dbtree too...
    tn := tndb.getFirstChild;
    for j:=0 to tndb.Count -1 do begin
      if t[i] = tn.Text then begin
        tn.Delete;
        break;
      end;
      tn := tndb.GetNextChild(tn);
    end;
  end;

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

  if MessageDlg('Drop Database '+tndb_.Text+'?' + crlf + crlf + 'WARNING: You will lose all tables in database '+tndb_.Text+'!', mtConfirmation, [mbok,mbcancel], 0) <> mrok then
    abort;

  Screen.Cursor := crSQLWait;
  try
    ExecQuery( 'DROP DATABASE ' + mask(tndb_.Text) );
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

  Variabelliste.Items.BeginUpdate;
  Variabelliste.Items.Clear;

  // VERSION
  v := GetVar( 'SELECT VERSION()' );
  versions := explode( '.', v );
  mysql_version := MakeInt(versions[0]) * 10000 + MakeInt(versions[1]) * 100 + MakeInt(versions[2]);
  strHostRunning := ZConn.HostName + ' running MySQL-Version ' + v + ' / Uptime: ';

  // VARIABLES
  GetResults( 'SHOW VARIABLES', ZQuery3 );
  for i:=1 to ZQuery3.RecordCount do
  begin
    n := VariabelListe.Items.Add;
    n.Caption := ZQuery3.Fields[0].AsString;
    n.Subitems.Add( ZQuery3.Fields[1].AsString );
    ZQuery3.Next;
  end;

  uptime := 0;

  // STATUS
  GetResults( 'SHOW STATUS', ZQuery3 );
  for i:=1 to ZQuery3.RecordCount do
  begin
    n := VariabelListe.Items.Add;
    n.Caption := ZQuery3.Fields[0].AsString;
    n.Subitems.Add( ZQuery3.Fields[1].AsString );
    if lowercase( ZQuery3.Fields[0].AsString ) = 'uptime' then
      uptime := strToIntDef(ZQuery3.Fields[1].AsString, 0);
    ZQuery3.Next;
  end;

  Timer1Timer(self);
  Timer1.OnTimer := Timer1Timer;

  VariabelListe.Items.EndUpdate;
  TabSheet6.Caption := 'Variables (' + inttostr(VariabelListe.Items.Count) + ')';
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
    ProcessListe.Items.BeginUpdate;
    ProcessListe.Items.Clear;
    ZQuery3.Close;
    ZQuery3.SQL.Clear;
    ZQuery3.SQL.Add( 'SHOW PROCESSLIST' );
    ZQuery3.Open;
    ZQuery3.First;
    for i:=1 to ZQuery3.RecordCount do
    begin
      n := ProcessListe.Items.Add;
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
    ProcessListe.Items.EndUpdate;
    TabSheet7.Caption := 'Process-List (' + inttostr(ProcessListe.Items.Count) + ')';
  except
    LogSQL( 'Error on loading process-list!' );
  end;
  ProcessListe.Items.EndUpdate;
  Screen.Cursor := crDefault;
end;


procedure TMDIChild.ProcessListeChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  Kill1.Enabled := (ProcessListe.Selected <> nil) and (PageControl2.ActivePage = TabSheet7);
end;


procedure TMDIChild.KillProcess(Sender: TObject);
var t : boolean;
begin
  if ProcessListe.Selected.Caption = GetVar( 'SELECT CONNECTION_ID()' ) then
    MessageDlg('Fatal: Better not kill my own Process...', mtError, [mbok], 0)
  else begin
    t := TimerProcessList.Enabled;
    TimerProcessList.Enabled := false; // prevent av (processliste.selected...)
    if MessageDlg('Kill Process '+ProcessListe.Selected.Caption+'?', mtConfirmation, [mbok,mbcancel], 0) = mrok then
    begin
      ExecQuery( 'KILL '+ProcessListe.Selected.Caption );
      ShowVariablesAndProcesses(self);
    end;
    TimerProcessList.Enabled := t; // re-enable autorefresh timer
  end;
end;


procedure TMDIChild.PageControl2Change(Sender: TObject);
begin
  ProcessListeChange(self, nil, TItemChange(self));
end;



procedure TMDIChild.ExecSQLClick(Sender: TObject; Selection: Boolean=false; CurrentLine: Boolean=false);
var
  SQL              : TStringList;
  i, rowsaffected  : Integer;
  SQLstart, SQLend, SQLscriptstart,
  SQLscriptend     : Integer;
  SQLTime          : Real;
  fieldcount, recordcount : Integer;
begin
  // Execute user-defined SQL
//  if SynMemo3.Focused then
//    exit;
  if length(trim(SynMemo1.Text)) = 0 then
    exit;

  TRY
    showstatus('Initializing SQL...', 2, 51);
    Mainform.ExecuteQuery.Enabled := false;
    Mainform.ExecuteSelection.Enabled := false;

    if ActualDatabase <> '' then
      zconn.Database := ActualDatabase;
    ZQuery1.Active := false;
    ZQuery1.DisableControls;

    if CurrentLine then
      SQL := parseSQL(SynMemo1.LineText)         // Run current line
    else begin
      if Selection then
        SQL := parsesql(SynMemo1.SelText) else   // Run selection
        SQL := parsesql(SynMemo1.Text);          // Run all
    end;
    if SQL.Count > 1 then
      SQLscriptstart := GetTickCount;

    rowsaffected := 0;
    ProgressBarQuery.Max := SQL.Count;
    ProgressBarQuery.Position := 0;
    ProgressBarQuery.show;

    showstatus('Executing SQL...', 2, 51);
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
        if (StrCmpBegin('select', lowercase(SQL[i])) or
           StrCmpBegin('show', lowercase(SQL[i])) or
           StrCmpBegin('desc', lowercase(SQL[i])) or
           StrCmpBegin('explain', lowercase(SQL[i])) or
           StrCmpBegin('describe', lowercase(SQL[i]))) then
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
          if ToolButtonStopOnErrors.Down or (i=SQL.Count-1) then begin
            Screen.Cursor := crdefault;
            MessageDLG(E.Message, mtError, [mbOK], 0);
            ProgressBarQuery.hide;
            Mainform.ExecuteQuery.Enabled := true;
            Mainform.ExecuteSelection.Enabled := true;
            break;
          end;
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
    SynMemo1.OnChange(self);

    if SQL.Count > 1 then begin
      SQLscriptend := GetTickCount;
      SQLTime := (SQLscriptend - SQLscriptstart) / 1000;
      Label4.Caption := Label4.Caption + ' Script-Time: ' + floattostrf(SQLTime, ffFixed, 18, 2)+' sec.';
    end;



  FINALLY
    ZQuery1.EnableControls;
    // resize all columns, if they are more wide than Mainform.DefaultColWidth
    if Mainform.DefaultColWidth <> 0 then
      for i:=0 to DBGrid2.Columns.count-1 do
        if DBGrid2.Columns[i].Width > Mainform.DefaultColWidth then
          DBGrid2.Columns[i].Width := Mainform.DefaultColWidth;
    Screen.Cursor := crdefault;
    showstatus('Ready', 2);
  END;
end;




procedure TMDIChild.FeldListeChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  // Feldeigenschaften anzeigen

  if FeldListe.Selected <> nil then
  with Feldliste.Selected do begin
    Toolbutton9.Enabled := True;
    DropField1.Enabled := True; //drop field
    MenuEditField.Enabled := true;
    ButtonEditField.enabled := true;
  end else begin
    Toolbutton9.Enabled := False;
    DropField1.Enabled := false; //drop field
    MenuEditField.Enabled := false;
    ButtonEditField.enabled := false;
  end;

end;

procedure TMDIChild.DropField(Sender: TObject);
var
  tn : TTreeNode;
begin
  // Feld löschen
  if Feldliste.Items.Count = 1 then
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
  if MessageDlg('Drop field ' + FeldListe.Selected.Caption + ' ?', mtConfirmation, [mbok,mbcancel], 0) = mrok then
  begin
    ExecQuery( 'ALTER TABLE '+mask(ActualTable)+' DROP '+mask(FeldListe.Selected.Caption) );
    ShowTableProperties(self);
  end;
end;


procedure TMDIChild.SynMemo1Change(Sender: TObject);
var somechars : Boolean;
begin
  PanelCharsInQueryWindow.Caption :=
    Format('%g', [length(SynMemo1.Text) + 0.0]) + ' Characters';
  somechars := length(SynMemo1.Text) > 0;
  Mainform.ExecuteQuery.Enabled := somechars;
  Mainform.ExecuteSelection.Enabled := length(SynMemo1.SelText) > 0;
  Mainform.ExecuteLine.Enabled := SynMemo1.LineText <> '';
  ButtonSaveSQL.Enabled := somechars;
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
      ButtonDropDatabase.Enabled := true;
      ButtonDropTable.Enabled := true;
      MenuRefresh.Enabled := true;
      MenuExport.Enabled := true;
      MenuImportTextFile.Enabled := true;
      MenuCreateTable.Enabled := true;
      MenuCreateDatabase.Enabled := true;
      MenuDropDatabase.Enabled := true;
      MenuDropTable.Enabled := true;
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
    end;
  end;
  timer4.OnTimer(self);
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
    ButtonDropTable.Enabled := false;
    MenuRefresh.Enabled := false;
    MenuExport.Enabled := false;
    MenuImportTextFile.Enabled := false;
    MenuCreateTable.Enabled := false;
    MenuCreateDatabase.Enabled := false;
    MenuDropDatabase.Enabled := false;
    MenuDropTable.Enabled := false;
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
  end;
  showstatus('', 1); // empty connected_time

  ZSQLMonitor1.Active := false;
end;



// Primary key abfragen - delete from .. where
procedure TMDIChild.FormShow(Sender: TObject);
begin
  // initialize some values and components:
  timer2.Enabled := true;
  timer3.Enabled := true;
  timer4.Enabled := true;

  { TODO : nur bei autoconnected file laden }
  if (paramstr(1) <> '') and Main.loadsqlfile then
  try
    // load sql-file from paramstr
    SynMemo1.Lines.LoadFromFile(paramstr(1));
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
  FieldEditForm.UpdateField := FeldListe.Selected <> nil;
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
  if InputQuery('Create new Database...', 'Database Name:', dbname) then begin
    Screen.Cursor := crSQLWait;
    ZQuery3.SQL.Clear();
    ZQuery3.SQL.Add('CREATE DATABASE ' + dbname);
    Try
      ZQuery3.ExecSQL;
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


procedure TMDIChild.TabellenlisteEdited(Sender: TObject; Item: TListItem;
  var S: String);
var i : Integer;
begin
  // edit table-name
  menudroptable.ShortCut := TextToShortCut('Del');

  ExecQuery( 'ALTER TABLE ' + mask(Item.Caption) + ' RENAME ' + mask(S) );
  ActualTable := S;
  ShowDBProperties(self);
  // Re-Select Entry
  for i:=0 to Tabellenliste.Items.Count-1 do
    if TabellenListe.Items[i].Caption = S then
      break;
  TabellenListe.Selected := TabellenListe.Items[i];
  TabellenListe.Items[i].Focused := true;
  // Important! Otherwise OnEdited refreshes list too:
  abort;
end;


procedure TMDIChild.MenuRenameTableClick(Sender: TObject);
begin
  // menuitem for edit table-name
  Tabellenliste.Selected.EditCaption;
end;


procedure TMDIChild.MenuViewBlobClick(Sender: TObject);
begin
  PageControl3.ActivePageIndex := 1;
end;


procedure TMDIChild.Timer3Timer(Sender: TObject);
begin
  try
    showstatus('Pinging host...', 2, 51);
    ExecQuery( SQL_PING );
    showstatus('Ready', 2);
  except
    showstatus('Connection to Host terminated abnormally!');
    Timer3.Enabled := false;
  end;
end;


procedure TMDIChild.Timer4Timer(Sender: TObject);
var
  stunden, minuten, sekunden : Integer;
begin
  // calculate and display connection-time
  sekunden := time_connected mod (60*60*24);
  stunden := sekunden div (60*60);
  sekunden := sekunden mod (60*60);
  minuten  := sekunden div 60;
  sekunden := sekunden mod 60;

  inc(time_connected);
  showstatus(format('Connected: %.2d:%.2d:%.2d', [stunden,minuten,sekunden]), 1);
end;


procedure TMDIChild.Clear1Click(Sender: TObject);
begin
  // clear
  if SynMemo3.Focused then
    SynMemo3.Lines.Clear
  else
    synmemo1.Lines.Clear;
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
  SynMemo2.Lines.Clear;
  Screen.Cursor := crDefault;
end;


procedure TMDIChild.EditQuery1Click(Sender: TObject);
begin
  // take query from history to query-tab
  SynMemo1.Text := SynMemo2.SelText;
  PageControl1.ActivePage := SheetQuery;
  pcChange(self);
end;


procedure TMDIChild.Markall3Click(Sender: TObject);
begin
  // select all in history
  SynMemo2.SelectAll;
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
    for i:=0 to Tabellenliste.Items.Count - 1 do
    begin
      if Tabellenliste.Items[i].Selected then
        ExecQuery( 'OPTIMIZE TABLE ' + mask(Tabellenliste.Items[i].Caption) );
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
    for i:=0 to Tabellenliste.Items.Count - 1 do
      if Tabellenliste.Items[i].Selected then begin
        if tables <> '' then
          tables := tables + ', ';
        tables := tables + mask(Tabellenliste.Items[i].Caption);
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
    for i:=0 to Tabellenliste.Items.Count - 1 do
      if Tabellenliste.Items[i].Selected then begin
        if tables <> '' then
          tables := tables + ', ';
        tables := tables + mask(Tabellenliste.Items[i].Caption);
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
    for i:=0 to Tabellenliste.Items.Count - 1 do
      if Tabellenliste.Items[i].Selected then begin
        if tables <> '' then
          tables := tables + ', ';
        tables := tables + mask(Tabellenliste.Items[i].Caption);
      end;
    ExecQuery( 'REPAIR TABLE ' + tables + ' QUICK' );
  finally
    Screen.Cursor := crDefault;
  end;
end;


procedure TMDIChild.TabellenlisteDblClick(Sender: TObject);
begin
  // table-doubleclick
  if Tabellenliste.Selected <> nil then begin
    SheetTable.TabVisible := Tabellenliste.Selected <> nil;
    SheetData.TabVisible := Tabellenliste.Selected <> nil;
    ActualTable := Tabellenliste.Selected.Caption;
    ShowTableProperties(self);
  end;
end;


procedure TMDIChild.Timer5Timer(Sender: TObject);
begin
  // can't connect -> close MDI-Child
  timer5.Enabled := false;
  timer4.Enabled := false;
  mainform.Showstatus('', 1);
  mainform.Showstatus('Ready', 2);
  close;
end;


procedure TMDIChild.DBGrid1TitleClick(Column: TColumn);
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
  SynMemo3.SetFocus;
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
  if DBGrid1.SelectedRows.Count = 0 then begin
    if MessageDLG('Delete 1 Record(s)?', mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
      ZQuery2.Delete
  end else
  if MessageDLG('Delete '+inttostr(DBGrid1.SelectedRows.count)+' Record(s)?', mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
    DBGrid1.SelectedRows.Delete;
end;


procedure TMDIChild.QuickFilterClick(Sender: TObject);
var
  filter,value,menuitem : String;
begin
  // Set filter for "where..."-clause
  value := escape_string(DBGrid1.SelectedField.AsString);
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
    filter := InputBox('Specify filter-value...', mask(DBGrid1.SelectedField.FieldName)+' = ', 'Value');
    if filter = 'Value' then
      abort;
    filter := '= ''' + filter + '''';
  end
  else if menuitem = 'QF9' then begin
    filter := InputBox('Specify filter-value...', mask(DBGrid1.SelectedField.FieldName)+' != ', 'Value');
    if filter = 'Value' then
      abort;
    filter := '!= ''' + filter + '''';
  end
  else if menuitem = 'QF10' then begin
    filter := InputBox('Specify filter-value...', mask(DBGrid1.SelectedField.FieldName)+' > ', 'Value');
    if filter = 'Value' then
      abort;
    filter := '> ''' + filter + '''';
  end
  else if menuitem = 'QF11' then begin
    filter := InputBox('Specify filter-value...', mask(DBGrid1.SelectedField.FieldName)+' < ', 'Value');
    if filter = 'Value' then
      abort;
    filter := '< ''' + filter + '''';
  end
  else if menuitem = 'QF12' then begin
    filter := InputBox('Specify filter-value...', mask(DBGrid1.SelectedField.FieldName)+' like ', 'Value');
    if filter = 'Value' then
      abort;
    filter := 'LIKE ''' + filter + '''';
  end;

  SynMemo3.Text := DBGrid1.SelectedField.FieldName + ' ' + filter;
  PageControl3.ActivePageIndex := 2;
  SynMemo3.SetFocus;
  SetFilter(self);
end;


procedure TMDIChild.ToolButton6Click(Sender: TObject);
begin
  // linebreaks
  WordWrap := not WordWrap;
  ToolButton6.Down := WordWrap;
  if Toolbutton6.Down then
    DBMemo1.Scrollbars := ssVertical
  else
    DBMemo1.Scrollbars := ssBoth;
end;


procedure TMDIChild.PageControl4Change(Sender: TObject);
begin
  // BLOB-Type change
  if PageControl4.ActivePage = Tabsheet3 then begin      // MEMO
    ToolButton5.Enabled := true;
    ToolButton6.Enabled := true;
    ToolButton7.Enabled := true;
    ToolButton8.Enabled := true;
  end
  else if PageControl4.ActivePage = Tabsheet4 then begin // ImageBLOB
    ToolButton5.Enabled := true;
    ToolButton6.Enabled := false;
    ToolButton7.Enabled := true;
    ToolButton8.Enabled := true;
  end
end;

procedure TMDIChild.ToolButton8Click(Sender: TObject);
begin
  // Save BLOB
  if not (DataSource1.State in [dsEdit, dsInsert]) then
  try
    DataSource1.Edit;
  except
    exit;
  end;
  with TSaveDialog.Create(self) do begin
    case PageControl4.ActivePageIndex of
      0 : begin
            Filter := 'Textfiles (*.txt)|*.txt|All files (*.*)|*.*';
            DefaultExt := 'txt';
          end;
      1 : begin
            Filter := 'Bitmaps(*.bmp)|*.bmp|All files (*.*)|*.*';
            DefaultExt := 'bmp';
          end;
    end;
    FileName := DBGrid1.SelectedField.FieldName;
    Options := [ofOverwritePrompt,ofEnableSizing];
    if execute then try
      Screen.Cursor := crHourGlass;
      case PageControl4.ActivePageIndex of
        0 : DBMemo1.Lines.SaveToFile(filename);
        1 : EDBImage1.Picture.SaveToFile(filename);
      end;
      Screen.Cursor := crDefault;
    except
      Screen.Cursor := crDefault;
      messagedlg('File could not be saved', mterror, [mbok], 0);
    end;
  end;
end;

procedure TMDIChild.ToolButton7Click(Sender: TObject);
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


procedure TMDIChild.ToolButton11Click(Sender: TObject);
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
        SynMemo3.Lines.LoadFromFile(FileName);
      except
        MessageDLG('Error while reading file ''' + filename + '''', mtError, [mbOK], 0);
      end;
      Screen.Cursor := crDefault;

      // don't get one filename more than one time
      dontadd := false;
      for m:=0 to PopUpMenu7.Items.Count-1 do begin
        _filename := PopUpMenu7.Items[m].Caption;
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
          PopUpMenu1.Items.Clear;
          while ValueExists('SQLWhereFile'+inttostr(i)) do begin
            menuitem := Tmenuitem.Create(self);
            menuitem.Caption := inttostr(PopUpMenu7.Items.count+1) + ' ' + ReadString('SQLWhereFile'+inttostr(i));
            menuitem.OnClick := LoadSQLWhereFile;
            PopUpMenu7.Items.Add(menuitem);
            inc(i);
          end;
        end;
      end;

    end;
  end;
end;

procedure TMDIChild.ToolButton12Click(Sender: TObject);
begin
  // save where-filter
  With TSaveDialog.Create(self) do begin
    Filter := 'Textfiles (*.txt)|*.txt|SQL-Files (*.sql)|*.sql|All files (*.*)|*.*';
    FileName := ActualTable;
    Options := [ofOverwritePrompt,ofEnableSizing];
    DefaultExt := 'txt';
    if Execute and (Filename <> '') then
      try
        SynMemo3.Lines.SaveToFile(FileName);
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
  where := trim(self.SynMemo3.Text);

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
      BtnPreviousFilter.Enabled := true;
      BtnNextFilter.Enabled := true;
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
  SynMemo3.Lines.Clear;
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
    SynMemo3.Lines.LoadFromFile(filename);
  except
    MessageDLG('Error while reading file ''' + filename + '''', mtError, [mbOK], 0);
  end;
  Screen.Cursor := crDefault;
end;

procedure TMDIChild.DropFilter1Click(Sender: TObject);
begin
  // Drop Filter
  SynMemo3.Lines.Clear;
  viewdata(self);
end;

procedure TMDIChild.MenuChangeTypeClick(Sender: TObject);
var
  i : Integer;
begin
  for i:=0 to Tabellenliste.Items.Count - 1 do
    if Tabellenliste.Items[i].Selected then
      ExecQuery( 'ALTER TABLE ' + mask(Tabellenliste.Items[i].Caption) + ' TYPE = ' + (Sender as TMenuItem).Hint);
  ShowDBProperties(self);
end;

procedure TMDIChild.MenuChangeTypeOtherClick(Sender: TObject);
var
  i : Integer;
  strtype : String;
begin
  // change table-type:
  if inputquery('Change table-type...','New table-type:', strtype) then begin
    for i:=0 to Tabellenliste.Items.Count - 1 do
      if Tabellenliste.Items[i].Selected then
        ExecQuery( 'ALTER TABLE ' + mask(Tabellenliste.Items[i].Caption) + ' TYPE = ' + strtype );
    ShowDBProperties(self);
  end;
end;

procedure TMDIChild.InsertRecord(Sender: TObject);
begin
  viewdata(self);
  ZQuery2.Insert;
end;

procedure TMDIChild.ZQuery2AfterOpen(DataSet: TDataSet);
begin
  mainform.Showstatus('Ready', 2);
end;

// select all tables
procedure TMDIChild.selectall1Click(Sender: TObject);
var i : Integer;
begin
  for i:=0 to Tabellenliste.Items.count-1 do
    Tabellenliste.Items[i].Selected := true;
end;

procedure TMDIChild.ResultPopup(Sender: TObject);
begin
  // data available?
  // MainForm.Save2CSV.enabled :=
end;

procedure TMDIChild.DBGrid1ColumnMoved(Sender: TObject; FromIndex,
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



procedure TMDIChild.SynMemo1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  // dragging an object over the query-memo
  if (Source as TControl).Parent = DBTree then
    accept := true;
  // set x-position of cursor
  SynMemo1.CaretX := (x - SynMemo1.Gutter.Width) div SynMemo1.CharWidth - 1 + SynMemo1.LeftChar;
  // set y-position of cursor
  SynMemo1.CaretY := y div SynMemo1.LineHeight + SynMemo1.TopLine;
  if not SynMemo1.Focused then
    SynMemo1.SetFocus;
end;


procedure TMDIChild.SynMemo1DragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  // dropping a TTreeNode into the query-memo
  SynMemo1.SelText := DBTree.Selected.Text;
end;



procedure TMDIChild.SynMemo1DropFiles(Sender: TObject; X, Y: Integer;
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
      SynMemo1.Lines.AddStrings(s);
    end;
  end;
  SynMemo1.OnChange(self);
end;

procedure TMDIChild.SynMemo1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  SynMemo1.OnChange(self);
end;

procedure TMDIChild.PopupMenu2Popup(Sender: TObject);
begin
  MenuAutoupdate.Enabled := PageControl2.ActivePageIndex=1;
end;

procedure TMDIChild.TabellenlisteEditing(Sender: TObject; Item: TListItem;
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
      SynMemo2.Lines.SaveToFile(Filename);
      Screen.Cursor := crdefault;
    end;
  end;
end;



procedure TMDIChild.PopupMenu1Popup(Sender: TObject);
begin
  // toggle drop-items and remember right-clicked item
  PopupMenuDropDatabase.Enabled := DBtree.Selected.Level = 1;
  PopupMenuDropTable.Enabled := DBtree.Selected.Level = 2;
  DBRightClickSelectedItem := DBtree.Selected;
end;

procedure TMDIChild.ToolButton15Click(Sender: TObject);
begin
  mainform.FindDialog1.execute;
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
    SynMemo1.Lines.LoadFromFile(filename);
  except
    MessageDLG('Error while reading file ''' + filename + '''', mtError, [mbOK], 0);
  end;
  Screen.Cursor := crDefault;
  SynMemo1Change(self);
end;

procedure TMDIChild.ButtonSaveSQLClick(Sender: TObject);
begin
  mainform.ButtonSaveSQLClick(self);
end;

procedure TMDIChild.ButtonLoadSQLClick(Sender: TObject);
begin
  Mainform.ButtonLoadSQLFile(self);
end;

procedure TMDIChild.BtnPreviousFilterClick(Sender: TObject);
begin
  // Go to previous filter
  if WhereFiltersIndex > 0 then begin
    dec(WhereFiltersIndex);
    ComboBoxWhereFilters.ItemIndex := WhereFiltersIndex;
    SynMemo3.Text := WhereFilters[WhereFiltersIndex];
  end;
end;

procedure TMDIChild.BtnNextFilterClick(Sender: TObject);
begin
  // Go to next filter
  if WhereFiltersIndex < WhereFilters.count-1 then begin
    inc(WhereFiltersIndex);
    ComboBoxWhereFilters.ItemIndex := WhereFiltersIndex;
    SynMemo3.Text := WhereFilters[WhereFiltersIndex];
  end;
end;

procedure TMDIChild.ComboBoxWhereFiltersChange(Sender: TObject);
begin
  WhereFiltersIndex := ComboBoxWhereFilters.ItemIndex;
  SynMemo3.Text := ComboBoxWhereFilters.Items[ComboBoxWhereFilters.ItemIndex];
end;

procedure TMDIChild.ToolButtonStopOnErrorsClick(Sender: TObject);
begin
  StopOnErrors := not StopOnErrors;
  ToolButtonStopOnerrors.Down := StopOnErrors;
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

procedure TMDIChild.PopupMenuDataPopup(Sender: TObject);
var y,m,d,h,i,s,ms : Word;
begin
  DataInsertDateTime.Enabled := DBGrid1.SelectedField.DataType in [ftString, ftDatetime, ftDate, ftTime];
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
  DBgrid1.SelectedField.AsString := d;
end;

procedure TMDIChild.ToolButton5Click(Sender: TObject);
begin
  if dbmemo1.DataField = '' then exit;
  case PageControl4.ActivePageIndex of
    0 : clipboard.astext := DBMemo1.lines.Text;
// TODO    1 : 
  end;
end;

procedure TMDIChild.setNULL1Click(Sender: TObject);
begin
  if not (DataSource1.State in [dsEdit, dsInsert]) then
    DataSource1.Edit;
  DBgrid1.SelectedField.Clear;
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
  With TZReadOnlyQuery.Create( self ) do
  begin
    Connection := ZConn;
    SQL.Text := SQLQuery;
    ExecSQL;
    Free;
  end;
end;


// Execute a query and return data from a single cell
function TMDIChild.GetVar( SQLQuery: String; x: Integer = 0 ) : String;
begin
  With TZReadOnlyQuery.Create( self ) do
  begin
    Connection := ZConn;
    SQL.Text := SQLQuery;
    Open;
    try
      First;
      Result := Fields[x].AsString;
      Close;
    finally
      Free;
    end;
  end;
end;


// Executes a query with an existing ZQuery-object
procedure TMDIChild.GetResults( SQLQuery: String; ZQuery: TZReadOnlyQuery );
begin
  ZQuery.SQL.Text := SQLQuery;
  ZQuery.Open;
  ZQuery.DisableControls;
  ZQuery.First;
end;


// Monitor SQL
procedure TMDIChild.ZSQLMonitor1LogTrace(Sender: TObject;
  Event: TZLoggingEvent);
begin
  LogSQL( Trim( Event.Message ), (Event.Category <> lcExecute) );
end;



procedure TMDIChild.ResizeImageToFit;
begin
  // Resize image to fit
  if EDBImage1.Picture.Width = 0 then
    exit;
  EDBImage1.Width := MulDiv(EDBImage1.Height, EDBImage1.Picture.Width, EDBImage1.Picture.Height);
  showstatus('Image: ' + inttostr( EDBImage1.Picture.width)
    + ' x ' + inttostr( EDBImage1.Picture.Height ) + ' pixel, '
    + 'zoomed to ' + IntToStr(round( 100 / EDBImage1.Picture.Height * EDBImage1.Height )) + '%'
    );

end;


procedure TMDIChild.Splitter2Moved(Sender: TObject);
begin
  ResizeImageToFit;
end;


procedure TMDIChild.DBGridDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);
var
  Grid : TSMDBGrid;
  ds : Tdatasource;
begin
  // view blob or memo while Data-Tabsheet is active!
  grid := (sender as TSMDBGrid);
  ds := grid.DataSource;
  if grid.SelectedField = nil then exit;


  if DBMemo1.DataSource <> ds then begin
    DBMemo1.DataField := '';
    DBMemo1.DataSource := ds;
    EDBImage1.DataField := '';
    EDBImage1.DataSource := ds;
  end;
  if grid.SelectedField.IsBlob then begin
    DBMemo1.DataField := grid.SelectedField.FieldName;
    EDBImage1.DataField := grid.SelectedField.FieldName;
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
  end else
  begin
    DBMemo1.DataField := '';
    EDBImage1.DataField := '';
    MenuViewBlob.Enabled := false;
  end;
  PageControl4Change(self);
end;


procedure TMDIChild.ZSQLMonitor1Trace(Sender: TObject;
  Event: TZLoggingEvent; var LogTrace: Boolean);
begin
  if Trim( Event.Message ) = SQL_PING then
    LogTrace := false;
end;


procedure TMDIChild.ZQuery1EditError(DataSet: TDataSet; E: EDatabaseError;
  var Action: TDataAction);
begin
  LogSQL( E.Message, true );
end;


procedure TMDIChild.FormResize(Sender: TObject);
begin
  Tabellenliste.Width := SheetDatabase.Width - Toolbar1.Width - Toolbar1.Left;
  Tabellenliste.Height := SheetDatabase.Height - Panel2.Height;
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
  if EditDataSearch.text <> '' then for i:=0 to DBGrid1.FieldCount-1 do
  begin
    if where <> '' then
      where := where + CRLF + ' OR ';
    where := where + DBGrid1.Fields[i].FieldName + ' LIKE ''%' + EditDataSearch.text + '%''';
  end;
  SynMemo3.Text := where;

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


// Rightclick on tablelist-columns
procedure TMDIChild.TabellenlisteColumnRightClick(Sender: TObject;
  Column: TListColumn; Point: TPoint);
begin
  PopupMenuTablelistColumns.Popup( Mouse.CursorPos.X, Mouse.CursorPos.Y );
end;


// Rightclick in tablelist-area, not on columns!
procedure TMDIChild.TabellenlisteMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if button = mbright then
    pmenu2.Popup( Mouse.CursorPos.X, Mouse.CursorPos.Y );
end;


// Simulate Ctrl+A-behaviour of common editors
procedure TMDIChild.DBMemo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ( Shift = [ssCtrl] ) and ( Key = Ord('A') ) then
    DBMemo1.SelectAll;
end;

function TMDIChild.mask(str: String) : String;
begin
  if mysql_version >= 32300 then
  begin
    // TODO: For better readability, it would be neat if we only escaped when necessary.
    result := StringReplace(str, '`', '``', [rfReplaceAll]);
    result := '`' + result + '`';
  end
  else
    result := str;
end;

end.



