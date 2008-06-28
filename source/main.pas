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
  ActnList, ImgList, Registry, ShellApi, ToolWin, Clipbrd, db, DBCtrls,
  SynMemo, synedit, SynEditTypes, ZDataSet, ZSqlProcessor,
  HeidiComp, sqlhelp, MysqlQueryThread, Childwin, VirtualTrees, TntDBGrids,
  DateUtils, PngImageList, OptimizeTables, View, Usermanager,
  SelectDBObject, DBActns;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    FileNewItem: TMenuItem;
    FileCloseItem: TMenuItem;
    Help1: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    menuAbout: TMenuItem;
    Edit1: TMenuItem;
    CopyItem: TMenuItem;
    PasteItem: TMenuItem;
    StatusBar: TStatusBar;
    ActionList1: TActionList;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    FileNew1: TAction;
    FileExit1: TAction;
    FileClose1: TWindowClose;
    Extra1: TMenuItem;
    FlushUserPrivileges1: TMenuItem;
    MenuCopyCSV: TMenuItem;
    N3: TMenuItem;
    MenuRefresh: TMenuItem;
    MenuExport: TMenuItem;
    MenuCreateDatabase: TMenuItem;
    MenuCreateTable: TMenuItem;
    N4: TMenuItem;
    MenuDropDatabase: TMenuItem;
    MenuDropTable: TMenuItem;
    ResetWindowOptions1: TMenuItem;
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
    N7: TMenuItem;
    menuReadme: TMenuItem;
    UserManager: TAction;
    ShowAboutBox: TAction;
    actMaintenance: TAction;
    menuMaintenance: TMenuItem;
    ImExport1: TMenuItem;
    CopyContentsasHTMLTable1: TMenuItem;
    CopyHTMLtable: TAction;
    Copy2CSV: TAction;
    menuWebsite: TMenuItem;
    N9: TMenuItem;
    N11: TMenuItem;
    PrintList: TAction;
    actCopyTable: TAction;
    ControlBar1: TControlBar;
    ToolBarStandard: TToolBar;
    ToolButton9: TToolButton;
    FileCloseItem2: TToolButton;
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
    PanelLimit: TPanel;
    CheckBoxLimit: TCheckBox;
    EditLimitStart: TEdit;
    EditLimitEnd: TEdit;
    ButtonOK: TButton;
    UpDownLimitStart: TUpDown;
    UpDownLimitEnd: TUpDown;
    EditUndo1: TEditUndo;
    ToolButton14: TToolButton;
    ExecuteQuery: TAction;
    ExecuteSelection: TAction;
    SaveDialog2: TSaveDialog;
    ExportSettings1: TMenuItem;
    Importsettings1: TMenuItem;
    OpenDialog2: TOpenDialog;
    menuSupportForum: TMenuItem;
    Copy2XML: TAction;
    ExportData: TAction;
    Exportdata1: TMenuItem;
    CopyasXMLdata1: TMenuItem;
    ExecuteLine: TAction;
    HTMLview: TAction;
    InsertFiles: TAction;
    InsertfilesintoBLOBfields1: TMenuItem;
    ExportTables: TAction;
    DataSearch: TAction;
    actDropTablesAndViews: TAction;
    LoadSQL: TAction;
    ImportSQL1: TMenuItem;
    menuConnections: TPopupMenu;
    miNewConnection: TMenuItem;
    menuWindow: TMenuItem;
    miFake: TMenuItem;
    menuBugtracker: TMenuItem;
    menuFeaturetracker: TMenuItem;
    menuDownload: TMenuItem;
    btnSQLHelp: TToolButton;
    menuSQLHelp: TMenuItem;
    N8: TMenuItem;
    Import1: TMenuItem;
    tlbSep6: TToolButton;
    menuUpdateCheck: TMenuItem;
    PngImageListMain: TPngImageList;
    actEditView: TAction;
    actCreateView: TAction;
    ToolButton2: TToolButton;
    Createview1: TMenuItem;
    ToolButton3: TToolButton;
    actDataSetFirst: TDataSetFirst;
    actDataSetLast: TDataSetLast;
    actDataSetInsert: TDataSetInsert;
    actDataSetDelete: TDataSetDelete;
    actDataSetPost: TDataSetPost;
    ToolButton4: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton10: TToolButton;
    ToolBarDatabase: TToolBar;
    btnDBEmptyTable: TToolButton;
    btnDBDropTable: TToolButton;
    btnDBCopyTable: TToolButton;
    btnCreateTable: TToolButton;
    ToolButton11: TToolButton;
    ToolbarTable: TToolBar;
    btnTableEditField: TToolButton;
    btnTableAddField: TToolButton;
    btnTableDropField: TToolButton;
    btnTableManageIndexes: TToolButton;
    actCreateTable: TAction;
    actEmptyTables: TAction;
    procedure actCreateTableExecute(Sender: TObject);
    procedure actCreateViewExecute(Sender: TObject);
    procedure btnSQLHelpClick(Sender: TObject);
    procedure menuWindowClick(Sender: TObject);
    procedure focusWindow(Sender: TObject);
    procedure menuConnectionsPopup(Sender: TObject);
    procedure ShowConnections(Sender: TObject);
    procedure FileExit1Execute(Sender: TObject);
    procedure FlushClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonRefreshClick(Sender: TObject);
    procedure ButtonCreateDatabaseClick(Sender: TObject);
    procedure ButtonDropDatabaseClick(Sender: TObject);
    procedure ResetWindowOptions1Click(Sender: TObject);
    procedure ButtonImportTextfileClick(Sender: TObject);
    procedure MenuPreferencesClick(Sender: TObject);
    procedure menuReadmeClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure UserManagerExecute(Sender: TObject);
    procedure ShowAboutBoxExecute(Sender: TObject);
    procedure actMaintenanceExecute(Sender: TObject);
    procedure actEditViewExecute(Sender: TObject);
    procedure CopyHTMLtableExecute(Sender: TObject);
    procedure Copy2CSVExecute(Sender: TObject);
    procedure PrintListExecute(Sender: TObject);
    procedure actCopyTableExecute(Sender: TObject);
    procedure showstatus(msg: string=''; panel: Integer=4);
    procedure ButtonOKClick(Sender: TObject);
    procedure LimitPanelEnter(Sender: TObject);
    procedure LimitPanelExit(Sender: TObject);
    procedure OpenURL(Sender: TObject);
    function mask(str: String) : String;
    procedure ExportSettings1Click(Sender: TObject);
    procedure Importsettings1Click(Sender: TObject);
    procedure ExecuteQueryExecute(Sender: TObject);
    procedure ExecuteSelectionExecute(Sender: TObject);
    procedure Copy2XMLExecute(Sender: TObject);
    procedure ExportDataExecute(Sender: TObject);
    procedure ExecuteLineExecute(Sender: TObject);
    procedure HTMLviewExecute(Sender: TObject);
    procedure InsertFilesExecute(Sender: TObject);
    procedure ExportTablesExecute(Sender: TObject);
    procedure DataSearchExecute(Sender: TObject);
    procedure actDataSetDeleteExecute(Sender: TObject);
    procedure btnTableAddFieldClick(Sender: TObject);
    procedure btnTableEditFieldClick(Sender: TObject);
    procedure actDropTablesAndViewsExecute(Sender: TObject);
    procedure actEmptyTablesExecute(Sender: TObject);
    procedure LoadSQLExecute(Sender: TObject);
    procedure EnsureConnected;
    function ExecuteRemoteQuery(sender: THandle; query: string): TDataSet;
    procedure ExecuteRemoteNonQuery(sender: THandle; query: string);
    procedure HandleWMComplete(var msg: TMessage); message WM_COMPLETED;
    procedure HandleWMCopyData(var msg: TWMCopyData); message WM_COPYDATA;
    procedure HandleWMProcessLog(var msg: TMessage); message WM_PROCESSLOG;
    procedure menuUpdateCheckClick(Sender: TObject);
    procedure btnTableDropFieldClick(Sender: TObject);
    procedure btnTableManageIndexesClick(Sender: TObject);
  private
    regMain : TRegistry;
    function GetChildwin: TMDIChild;
    function GetParamValue(const paramChar: Char; const paramName:
      string; var curIdx: Byte; out paramValue: string): Boolean;
  public
    MaintenanceForm: TOptimize;
    ViewForm: TfrmView;
    UserManagerForm: TUserManagerForm;
    SelectDBObjectForm: TfrmSelectDBObject;
    procedure OpenRegistry(Session: String = '');
    function GetRegValue( valueName: String; defaultValue: Integer; Session: String = '' ) : Integer; Overload;
    function GetRegValue( valueName: String; defaultValue: Boolean; Session: String = '' ) : Boolean; Overload;
    function GetRegValue( valueName: String; defaultValue: String; Session: String = '' ) : String; Overload;
    function CreateMDIChild(parHost, parPort, parUser, parPass, parDatabase, parTimeout, parCompress, parSortDatabases, parDescription: String): Boolean;

    // Reference to currently active childwindow:
    property Childwin: TMDIChild read GetChildwin;
end;

var
  MainForm            : TMainForm;
  appstarted          : Boolean = false;               // see connections.pas
  loadsqlfile         : boolean = true;               // load sql-file into query-memo at startup?
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
  Columns  : TStringList;
  SubParts : TStringList;
end;

implementation

uses
  About,
  connections,
  exportsql,
  tbl_properties,
  loaddata,
  options,
  printlist,
  copytable,
  insertfiles,
  Helpers,
  Threading,
  mysql_structures,
  MysqlConn,
  UpdateCheck,
  fieldeditor,
  createtable;

{$R *.DFM}

procedure TMainForm.ShowConnections(Sender: TObject);
begin
  if ActiveMDIChild = nil then
    ConnectionWindow(Self)
  else begin
    debug('perf: new connection clicked.');
    ShellExec( ExtractFileName(paramstr(0)), ExtractFilePath(paramstr(0)) );
  end;
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
  ChildWin.ProcessSqlLog;
end;

procedure TMainForm.EnsureConnected;
begin
  if ActiveMDIChild = nil then raise Exception.Create('Not connected.');
end;

function TMainForm.ExecuteRemoteQuery(sender: THandle; query: string): TDataSet;
//var
  //tab: THandle;
begin
  EnsureConnected;
  // tab := TMDIChild(ActiveMDIChild).CreateOrGetRemoteQueryTab(sender);
  // TQueryTab(tab).AddText(query);
  // tab.ExecOrQueueQuery(query);
  result := ChildWin.ExecuteQuery(query);
end;

procedure TMainForm.ExecuteRemoteNonQuery(sender: THandle; query: string);
//var
  //tab: THandle;
begin
  EnsureConnected;
  // tab := TMDIChild(ActiveMDIChild).CreateOrGetRemoteQueryTab(sender);
  // TQueryTab(tab).AddText(query);
  // tab.ExecOrQueueQuery(query);
  ChildWin.ExecuteNonQuery(query);
end;

procedure TMainForm.showstatus(msg: string=''; panel: Integer=4);
begin
  // show Message in statusbar
  StatusBar.Panels[panel].Text := msg;
  StatusBar.Repaint;
end;

procedure TMainForm.FileExit1Execute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FlushClick(Sender: TObject);
var
  flushwhat : String;
begin
  if sender is TMenuItem then
    flushwhat := UpperCase((sender as TMenuItem).Caption)
  else if sender is TToolButton then
    flushwhat := 'PRIVILEGES';
  delete(flushwhat, pos('&', flushwhat), 1);
  ChildWin.ExecUpdateQuery('FLUSH ' + flushwhat);
  if sender = MenuFlushTableswithreadlock then begin
    MessageDlg(
      'Tables have been flushed and read lock acquired.'#10 +
      'Perform backup or snapshot of table data files now.'#10 +
      'Press OK to unlock when done...',
      mtInformation, [mbOk], 0
    );
    ChildWin.ExecUpdateQuery('UNLOCK TABLES');
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  ws : String;
  filename : String;
  buffer   : array[0..MAX_PATH] of char;
begin
  if ActiveMDIChild <> nil then
    ActiveMDIChild.Close;

  if windowstate = wsNormal
    then ws := 'Normal' else
  if windowstate = wsMinimized
    then ws := 'Minimized' else
  if windowstate = wsMaximized
    then ws := 'Maximized';

  with TRegistry.Create do
  begin
    if OpenKey(REGPATH, true) then
    begin
      WriteString(REGNAME_WINDOWSTATE, ws);
      WriteInteger(REGNAME_WINDOWLEFT, left);
      WriteInteger(REGNAME_WINDOWTOP, top);
      WriteInteger(REGNAME_WINDOWWIDTH, width);
      WriteInteger(REGNAME_WINDOWHEIGHT, height);
      // Position of Toolbars
      WriteInteger(REGNAME_TOOLBAR2LEFT, ToolBarStandard.Left);
      WriteInteger(REGNAME_TOOLBAR2TOP, ToolBarStandard.Top);
      WriteInteger(REGNAME_TOOLBARDBLEFT, ToolBarDatabase.Left);
      WriteInteger(REGNAME_TOOLBARDBTOP, ToolBarDatabase.Top);
      WriteInteger(REGNAME_TOOLBARTABLELEFT, ToolBarTable.Left);
      WriteInteger(REGNAME_TOOLBARTABLETOP, ToolBarTable.Top);
      WriteInteger(REGNAME_TOOLBARDATALEFT, ToolBarData.Left);
      WriteInteger(REGNAME_TOOLBARDATATOP, ToolBarData.Top);
    end;
    CloseKey;
    Free;
  end;
  GetTempPath(MAX_PATH, buffer);
  filename := buffer;
  filename := '\'+filename+APPNAME+'-preview.';
  if FileExists(filename+'html') then
    deletefile(filename+'html');
  if FileExists(filename+'jpg') then
    deletefile(filename+'jpg');
  if FileExists(filename+'gif') then
    deletefile(filename+'gif');
  if FileExists(filename+'bmp') then
    deletefile(filename+'bmp');
  if regMain <> nil then begin
    regMain.CloseKey;
    regMain.Free;
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
  ws : String;
  Monitor: TMonitor;
const
  MoveWinThreshold: Byte = 80;
begin
  caption := APPNAME;
  setLocales;

  ws := GetRegValue('windowstate', 'Normal');
  if ws = 'Minimized'
    then windowstate := wsMinimized else
  if ws = 'Normal' then begin
    windowstate := wsNormal;
    Left := GetRegValue(REGNAME_WINDOWLEFT, Left);
    Top := GetRegValue(REGNAME_WINDOWTOP, Top);
    Width := GetRegValue(REGNAME_WINDOWWIDTH, Width);
    Height := GetRegValue(REGNAME_WINDOWHEIGHT, Height);
    // Ensure main window (that means: the upper left corner) is placed inside
    // current monitor resolution, important e.g. after having plugged off a second monitor
    // Ensures a minimum visible area of 80x80 pixels, so the user can manually resize and move it
    Monitor := Screen.MonitorFromWindow(Self.Handle);
    if Left > Monitor.Left + Monitor.Width - MoveWinThreshold then
      Left := Monitor.Left + Monitor.Width - MoveWinThreshold; // Set minimal visible width
    if Top > Monitor.Top + Monitor.Height - MoveWinThreshold then
      Top := Monitor.Top + Monitor.Height - MoveWinThreshold; // Set minimal visible height
  end else
  if ws = 'Maximized'
    then windowstate := wsMaximized;

  // Position of Toolbars
  ToolBarStandard.Left := GetRegValue(REGNAME_TOOLBAR2LEFT, ToolBarStandard.Left);
  ToolBarStandard.Top := GetRegValue(REGNAME_TOOLBAR2TOP, ToolBarStandard.Top);
  ToolBarDatabase.Left := GetRegValue(REGNAME_TOOLBARDBLEFT, ToolBarDatabase.Left);
  ToolBarDatabase.Top := GetRegValue(REGNAME_TOOLBARDBTOP, ToolBarDatabase.Top);
  ToolBarTable.Left := GetRegValue(REGNAME_TOOLBARTABLELEFT, ToolBarTable.Left);
  ToolBarTable.Top := GetRegValue(REGNAME_TOOLBARTABLETOP, ToolBarTable.Top);
  ToolBarData.Left := GetRegValue(REGNAME_TOOLBARDATALEFT, ToolBarData.Left);
  ToolBarData.Top := GetRegValue(REGNAME_TOOLBARDATATOP, ToolBarData.Top);

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
  reg : TRegistry;
  LastUpdatecheck : TDateTime;
  UpdatecheckInterval : Integer;
  DefaultLastrunDate : String;
  frm : TfrmUpdateCheck;
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
      frm.ShowModal;
      FreeAndNil(frm);
    end;
  end;

  // Check commandline if parameters were passed. Otherwise show connections windows
  curParam := 1;
  while curParam <= ParamCount do
  begin
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

  // Minimal parameter is hostname. If given, user commandline parameters.
  if parHost <> '' then
  begin
    // Parameters belong to connection, not to a SQL file which should get opened
    loadsqlfile := False;
    // Take care for empty description - it gets used to read/write session settings to registry!
    if parDescription = '' then
      parDescription := parHost;
    if CreateMDIChild(
      parHost,
      parPort,
      parUser,
      parPass,
      parDatabase,
      parTimeout,
      parCompress,
      parSortDatabases,
      parDescription) then
    begin
      // Save session parameters to registry
      reg := TRegistry.Create;
      if reg.OpenKey(REGPATH + REGKEY_SESSIONS + parDescription, true) then
      begin
        reg.WriteString(REGNAME_HOST, parHost);
        reg.WriteString(REGNAME_USER, parUser);
        reg.WriteString(REGNAME_PASSWORD, encrypt(parPass));
        reg.WriteString(REGNAME_PORT, parPort);
        reg.WriteString(REGNAME_TIMEOUT, parTimeout);
        reg.WriteBool(REGNAME_COMPRESSED, Boolean(StrToIntDef(parCompress, 0)) );
        reg.WriteString(REGNAME_ONLYDBS, parDatabase);
        reg.WriteBool(REGNAME_ONLYDBSSORTED, Boolean(StrToIntDef(parSortDatabases, 0)) );
        reg.CloseKey;
        reg.Free;
      end;
    end;
  end else
    // Cannot be done in OnCreate because we need ready forms here:
    ShowConnections(self);
end;


procedure TMainForm.ButtonRefreshClick(Sender: TObject);
begin
  // Refresh
  // Force data tab update when appropriate.
  Childwin.dataselected := false;
  if ChildWin.PageControlMain.ActivePage = ChildWin.tabHost then
    ChildWin.ShowVariablesAndProcesses(self)
  else if ChildWin.PageControlMain.ActivePage = ChildWin.tabDatabase then
    ChildWin.MenuRefreshClick(self)
  else if ChildWin.PageControlMain.ActivePage = ChildWin.tabTable then
    ChildWin.ShowTableProperties(ChildWin.SelectedTable)
  else if ChildWin.PageControlMain.ActivePage = ChildWin.tabData then
    ChildWin.viewdata(Sender)
  else
    ChildWin.RefreshTree(True);
end;

procedure TMainForm.ButtonCreateDatabaseClick(Sender: TObject);
begin
  // create database
  ChildWin.CreateDatabase(self);
end;

procedure TMainForm.ButtonDropDatabaseClick(Sender: TObject);
begin
  // drop db
  if ChildWin.ActiveDatabase <> '' then
    ChildWin.DropDB(self);
end;

procedure TMainForm.btnSQLHelpClick(Sender: TObject);
begin
  // SQL help
  ChildWin.CallSQLHelp( Sender );
end;


procedure TMainForm.ResetWindowOptions1Click(Sender: TObject);
var
  reg : TRegistry;
begin
  // reset all options for window-size, height ...

  if ActiveMDIChild <> nil then
  begin
    MessageDlg('Close all open windows before you do this.', mtError, [mbok], 0);
    exit;
  end;

  reg := TRegistry.Create;
  with reg do
  begin
    Access := KEY_ALL_ACCESS;
    if OpenKey(REGPATH, false) then
    begin
      DeleteValue(REGNAME_CHILDWINSTATE);
      DeleteValue(REGNAME_CHILDWINLEFT);
      DeleteValue(REGNAME_CHILDWINTOP);
      DeleteValue(REGNAME_CHILDWINWIDTH);
      DeleteValue(REGNAME_CHILDWINHEIGHT);
      DeleteValue(REGNAME_QUERYMEMOHEIGHT);
      DeleteValue(REGNAME_DBTREEWIDTH);
      DeleteValue(REGNAME_SQLOUTHEIGHT);

      CloseKey;
      MessageDlg('All Window-Settings were reset to default values.', mtInformation, [mbok], 0);
    end;
    Free;
  end;

end;

procedure TMainForm.ButtonImportTextfileClick(Sender: TObject);
begin
  // Import Textfile
  loaddataWindow(self);
end;

procedure TMainForm.MenuPreferencesClick(Sender: TObject);
begin
  // Preferences
  OptionsWindow (Self);
end;

procedure TMainForm.menuReadmeClick(Sender: TObject);
begin
  // show readme.txt
  ShellExec( 'readme.txt', ExtractFilePath(paramstr(0)) );
end;

procedure TMainForm.FormResize(Sender: TObject);
var
  i, room: Integer;
begin
  room := 0;
  for i := 1 to Statusbar.Panels.Count - 1 do
    inc(room, Statusbar.Panels[i].Width);
  StatusBar.Panels[0].Width := Statusbar.Width - room;
end;

procedure TMainForm.UserManagerExecute(Sender: TObject);
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

procedure TMainForm.ShowAboutBoxExecute(Sender: TObject);
begin
  // Info-Box
  AboutWindow (Self);
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
    if Assigned(Childwin.ListTables.FocusedNode) then begin
      // "Edit view" was clicked in ListTables' context menu
      NodeData := Childwin.ListTables.GetNodeData(Childwin.ListTables.FocusedNode);
      ViewForm.EditViewName := NodeData.Captions[0];
    end else if Childwin.DBtree.GetFirstSelected <> nil then begin
      // "Edit view" was clicked in DBTree's context menu
      ds := Childwin.FetchDbTableList(Childwin.ActiveDatabase);
      ds.RecNo := Childwin.DBtree.GetFirstSelected.Index+1;
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


procedure TMainForm.Copy2CSVExecute(Sender: TObject);
begin
  // Copy data in actual dataset as CSV
  if ChildWin.PageControlMain.ActivePage = ChildWin.tabData then
    dataset2csv(ChildWin.GetVisualDataset(), ChildWin.prefCSVSeparator, ChildWin.prefCSVEncloser, ChildWin.prefCSVTerminator)
  else if ChildWin.PageControlMain.ActivePage = ChildWin.tabQuery then
    dataset2csv(ChildWin.GetVisualDataset(), ChildWin.prefCSVSeparator, ChildWin.prefCSVEncloser, ChildWin.prefCSVTerminator);
end;


procedure TMainForm.CopyHTMLtableExecute(Sender: TObject);
begin
  // Copy data in actual dataset as HTML
  if ChildWin.PageControlMain.ActivePage = ChildWin.tabData then
    dataset2html(ChildWin.GetVisualDataset(), TZQuery(ChildWin.GetVisualDataset()).Sql.Text, '', ChildWin.prefConvertHTMLEntities, APPNAME + ' ' + FullAppVersion )
  else if ChildWin.PageControlMain.ActivePage = ChildWin.tabQuery then
    dataset2html(ChildWin.GetVisualDataset(), TZReadOnlyQuery(ChildWin.GetVisualDataset()).Sql.Text, '', ChildWin.prefConvertHTMLEntities, APPNAME + ' ' + FullAppVersion);
end;


procedure TMainForm.PrintListExecute(Sender: TObject);
var
  page : TTabSheet;
begin
  // print
  page := ChildWin.PageControlMain.ActivePage;
  if page.Name = 'SheetData' then
  begin
    // TODO: Print data
  end
  else if (page.Name = 'SheetQuery') then
  begin
    // TODO: Print data
  end
  else
    printlistWindow(self);
end;


procedure TMainForm.actCopyTableExecute(Sender: TObject);
begin
  // copy table
  CopyTableWindow(self);
end;


procedure TMainForm.ButtonOKClick(Sender: TObject);
begin
  ChildWin.DBTree.SetFocus;
  ChildWin.viewdata(Sender);
end;

procedure TMainForm.LimitPanelEnter(Sender: TObject);
begin
  // Entering Data-Toolbar
  ButtonOK.Default := true;
end;

procedure TMainForm.LimitPanelExit(Sender: TObject);
begin
  // Exiting Data-Toolbar
  ButtonOK.Default := false;
end;

procedure TMainForm.focusWindow(Sender: TObject);
begin
  ActivateWindow((Sender as TMenuItem).Tag);
end;


procedure TMainForm.menuConnectionsPopup(Sender: TObject);
var
  i: integer;
  s: string;
  keep: boolean;
  list: TWindowDataArray;
  item: TMenuItem;
begin
  // Delete dynamically added connection menu items.
  for i := menuConnections.Items.Count - 1 downto 0 do begin
    s := menuConnections.Items[i].Name;
    SetLength(s, 2);
    keep := false;
    if s = 'mi' then keep := true;
    if s = 'se' then keep := true;
    if not keep then menuConnections.Items.Delete(i);
  end;

  // Check if all the heidisql windows are still alive.
  CheckForCrashedWindows;

  // Fetch list of heidisql windows.
  list := GetWindowList;

  // Add separator before 'open heidisql windows' section.
  item := TMenuItem.Create(self);
  item.Caption := '-';
  menuConnections.Items.Add(item);

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
    menuConnections.Items.Add(item);
  end;
end;


procedure TMainForm.OpenURL(Sender: TObject);
begin
  // open url (hint)
  if sender is TMenuItem then
    ShellExec( TMenuItem(Sender).Hint )
  else
    ShellExec( TControl(Sender).Hint );
end;


// Escape database, table, field, index or key name.
function TMainform.mask(str: String) : String;
begin
  if ActiveMDIChild = nil then
    raise Exception.Create('Cannot mask SQL without active MDI');
  result := ChildWin.mask(str);
end;


procedure TMainForm.ExportSettings1Click(Sender: TObject);
begin
  // Export settings to .reg-file
  if SaveDialog2.Execute then begin
    if winexec(pchar('regedit.exe /e "'+SaveDialog2.FileName+'" HKEY_CURRENT_USER'+REGPATH), SW_SHOW) = ERROR_FILE_NOT_FOUND then
      MessageDlg('File not found: regedit.exe', mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.Importsettings1Click(Sender: TObject);
begin
  // Import settings from .reg-file
  if OpenDialog2.Execute then begin
    if winexec(pchar('regedit.exe "'+OpenDialog2.FileName+'"'), SW_SHOW) = ERROR_FILE_NOT_FOUND then
      MessageDlg('File not found: regedit.exe', mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.ExecuteQueryExecute(Sender: TObject);
begin
  ChildWin.ExecSqlClick(sender, false);
end;

procedure TMainForm.ExecuteSelectionExecute(Sender: TObject);
begin
  ChildWin.ExecSqlClick(sender, true);
end;

procedure TMainForm.ExecuteLineExecute(Sender: TObject);
begin
  ChildWin.ExecSqlClick(sender, false, true);
end;

procedure TMainForm.Copy2XMLExecute(Sender: TObject);
begin
  // Copy data in actual dataset as XML
  if ChildWin.PageControlMain.ActivePage = ChildWin.tabData then
    dataset2xml(ChildWin.GetVisualDataset(), ChildWin.SelectedTable)
  else if ChildWin.PageControlMain.ActivePage = ChildWin.tabQuery then
    dataset2xml(ChildWin.GetVisualDataset(), 'SQL-query');
end;

procedure TMainForm.ExportDataExecute(Sender: TObject);
var
  query : TDataSet;
begin
  // Save data in current dataset as CSV, HTML or XML

  case ChildWin.PageControlMain.ActivePageIndex of
    3 : begin query := ChildWin.GetVisualDataset(){ZQuery2}; ChildWin.SaveDialogExportData.Filename := ChildWin.SelectedTable; end;
    4 : begin query := ChildWin.GetVisualDataset() {ZQuery1}; ChildWin.SaveDialogExportData.Filename := 'SQL-query'; end;
    else
      raise Exception.Create('Internal error: Cannot fetch query with no related active tab.');
  end;

  with ChildWin.SaveDialogExportData do
  begin
    Title := 'Export result-set from '+Filename+'...';
    FieldSep := ChildWin.prefCSVSeparator;
    LineSep := ChildWin.prefCSVTerminator;
    FieldEncl := ChildWin.prefCSVEncloser;
    ConvertHTMLSpecialChars := ChildWin.prefConvertHTMLEntities;


    if Execute and (FileName <> '') then
    begin
      Screen.Cursor := crHourGlass;
      case FilterIndex of
        1 : dataset2csv(query, FieldSep, FieldEncl, LineSep, Filename);
        2 : dataset2html(query, FileName, FileName, ConvertHTMLSpecialChars, APPNAME+' '+FullAppVersion);
        3 : dataset2xml(query, FileName, FileName);
      end;
      ChildWin.prefCSVSeparator := FieldSep;
      ChildWin.prefCSVTerminator := LineSep;
      ChildWin.prefCSVEncloser := FieldEncl;
      ChildWin.prefConvertHTMLEntities := ConvertHTMLSpecialChars;
      with TRegistry.Create do
      begin
        openkey(REGPATH, true);
        WriteBool(REGNAME_CONVERTHTMLENTITIES, ConvertHTMLSpecialChars);
        WriteString(REGNAME_CSV_SEPARATOR, FieldSep);
        WriteString(REGNAME_CSV_ENCLOSER, FieldEncl);
        WriteString(REGNAME_CSV_TERMINATOR, LineSep);
        closekey();
        Free;
      end;
      Screen.Cursor := crDefault;
    end;
  end;

end;



// view HTML
procedure TMainForm.HTMLviewExecute(Sender: TObject);
var
  g              : TTntDBGrid;
  filename,extension   : String;
  f              : Textfile;
  buffer         : array[0..MAX_PATH] of char;
begin
  g := ChildWin.ActiveGrid;
  if g = nil then begin messagebeep(MB_ICONASTERISK); exit; end;
  if g.datasource.State = dsInactive then begin
    messagebeep(MB_ICONASTERISK);
    exit;
  end;
  Screen.Cursor := crHourGlass;
  showstatus('Saving contents to file...');
  GetTempPath(MAX_PATH, buffer);
  if g.SelectedField.IsBlob and (pos('JFIF', copy(g.SelectedField.AsString, 0, 20)) <> 0) then
    extension := 'jpg'
  else if g.SelectedField.IsBlob and StrCmpBegin('GIF', g.SelectedField.AsString) then
    extension := 'gif'
  else if g.SelectedField.IsBlob and StrCmpBegin('BM', g.SelectedField.AsString) then
    extension := 'bmp'
  else
    extension := 'html';
  filename := buffer;
  filename := filename+'\'+APPNAME+'-preview.'+extension;
  AssignFile(f, filename);
  Rewrite(f);
  Write(f, g.SelectedField.AsString);
  CloseFile(f);
  ShowStatus( STATUS_MSG_READY );
  Screen.Cursor := crDefault;
  ShellExec( filename );
end;

procedure TMainForm.InsertFilesExecute(Sender: TObject);
begin
  InsertFilesWindow(Self);
end;

procedure TMainForm.ExportTablesExecute(Sender: TObject);
begin
  // Export SQL
  ExportTablesWindow (Self);
end;

procedure TMainForm.DataSearchExecute(Sender: TObject);
begin
  with ChildWin.EditDataSearch do
  begin
    SetFocus;
    SelectAll;
  end;
end;

// Drop Table(s)
procedure TMainForm.actDropTablesAndViewsExecute(Sender: TObject);
var
  i : Integer;
  Tables, Views : TStringList;
  msg, sql, activeDB : String;
begin
  debug('drop table activated');
  // Set default database name to to ActiveDatabase.
  // Can be overwritten when someone selects a table in dbtree from different database
  activeDB := Childwin.ActiveDatabase;

  Tables := TStringlist.Create;
  Views := TStringlist.Create;
  if Childwin.PageControlMain.ActivePage = Childwin.tabDatabase then begin
    // Invoked from one of the various buttons, SheetDatabase is the active page, drop highlighted table(s).
    Tables := GetVTCaptions(Childwin.ListTables, True, 0, NODETYPE_TABLE);
    Views := GetVTCaptions(Childwin.ListTables, True, 0, NODETYPE_VIEW);
  end else begin
    // Invoked from one of the various buttons, drop table selected in tree view.
    case Childwin.GetSelectedNodeType of
      NODETYPE_TABLE: Tables.Add(Childwin.SelectedTable);
      NODETYPE_VIEW: Views.Add(Childwin.SelectedTable)
    end;
  end;

  // Fix actions temporarily enabled for popup menu.
  Childwin.ValidateControls;

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
      sql := sql + Childwin.mask(Tables[i]);
    end;
    Childwin.ExecUpdateQuery( sql );
  end;
  FreeAndNil(Tables);

  // Compose and run DROP VIEW query
  if Views.Count > 0 then begin
    sql := 'DROP VIEW ';
    for i := 0 to Views.Count - 1 do
    begin
      if i > 0 then
        sql := sql + ', ';
      sql := sql + Childwin.mask(Views[i]);
    end;
    Childwin.ExecUpdateQuery( sql );
  end;
  FreeAndNil(Views);

  // Refresh ListTables + dbtree so the dropped tables are gone:
  Childwin.MenuRefreshClick(Sender)
end;


// Load SQL-file, make sure that SheetQuery is activated
procedure TMainForm.LoadSQLExecute(Sender: TObject);
begin
  ChildWin.PageControlMain.ActivePage := ChildWin.tabQuery;
  ChildWin.btnQueryLoadClick( sender );
end;


{**
  Init main registry object and open desired key
  Outsoureced from GetRegValue() to avoid redundant code
  in these 3 overloaded methods.
}
procedure TMainForm.OpenRegistry(Session: String = '');
var
  folder : String;
begin
  if regMain = nil then
    regMain := TRegistry.Create;
  folder := REGPATH;
  if Session <> '' then
    folder := folder + REGKEY_SESSIONS + Session;
  if regMain.CurrentPath <> folder then
    regMain.OpenKey(folder, false);
end;


{**
  Read a numeric preference value from registry
}
function TMainForm.GetRegValue( valueName: String; defaultValue: Integer; Session: String = '' ) : Integer;
begin
  result := defaultValue;
  OpenRegistry(Session);
  if regMain.ValueExists( valueName ) then
    result := regMain.ReadInteger( valueName );
end;


{***
  Read a boolean preference value from registry
  @param string Name of the value
  @param boolean Default-value to return if valueName was not found
  @param string Subkey of REGPATH where to search for the value
}
function TMainForm.GetRegValue( valueName: String; defaultValue: Boolean; Session: String = '' ) : Boolean;
begin
  result := defaultValue;
  OpenRegistry(Session);
  if regMain.ValueExists( valueName ) then
    result := regMain.ReadBool( valueName );
end;



{***
  Read a text preference value from registry
}
function TMainForm.GetRegValue( valueName: String; defaultValue: String; Session: String = '' ) : String;
begin
  result := defaultValue;
  OpenRegistry(Session);
  if regMain.ValueExists( valueName ) then
    result := regMain.ReadString( valueName );
end;



{***
  Return active childwin if any
  @return TMDIChild
}
function TMainForm.GetChildwin: TMDIChild;
begin
  result := TMDIChild( ActiveMDIChild );
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


{**
  Receive connection parameters and create the mdi-window
  Paremeters are either sent by connection-form or by commandline.
}
function TMainform.CreateMDIChild(parHost, parPort, parUser, parPass, parDatabase, parTimeout, parCompress, parSortDatabases, parDescription: String): Boolean;
var
  cp : TOpenConnProf;
  mysqlconn : TMysqlConn;
  f : TMDIChild;
begin
  // fill structure
  ZeroMemory (@cp,SizeOf(cp));

  with cp do
  begin
    MysqlParams.Protocol := 'mysql';
    MysqlParams.Host := Trim( parHost );
    MysqlParams.Port := StrToIntDef(parPort, DEFAULT_PORT);
    MysqlParams.Database := '';
    MysqlParams.User := parUser;
    MysqlParams.Pass := parPass;

    // additional
    if Integer(parCompress) > 0 then
      MysqlParams.PrpCompress := 'true'
    else
      MysqlParams.PrpCompress := 'false';

    MysqlParams.PrpTimeout := parTimeout;
    MysqlParams.PrpDbless := 'true';
    MysqlParams.PrpClientLocalFiles := 'true';
    MysqlParams.PrpClientInteractive := 'true';

    DatabaseList := parDatabase;
    DatabaseListSort := Boolean(StrToIntDef(parSortDatabases, 0));
    Description := parDescription;
  end;

  mysqlconn := TMysqlConn.Create(@cp);

  // attempt to establish connection
  case mysqlconn.Connect of
    MCR_SUCCESS:
    begin
      // create child window and pass it the conn params and the opened connection
      f := TMDIChild.Create(Application);
      f.Init(@cp,mysqlconn); // childwin responsible to free mysqlconn
      Result := True;
    end;
  else
    // attempt failed -- show error
    MessageDlg ( 'Could not establish connection! Details:'+CRLF+CRLF+mysqlconn.LastError, mtError, [mbOK], 0);
    Result := False;
    FreeAndNil (mysqlconn);
  end;

  ShowStatus( STATUS_MSG_READY );
end;

procedure TMainForm.actDataSetDeleteExecute(Sender: TObject);
begin
  ChildWin.Delete1Click(sender);
  abort;
end;


procedure TMainForm.menuUpdateCheckClick(Sender: TObject);
var
  frm : TfrmUpdateCheck;
begin
  frm := TfrmUpdateCheck.Create(Self);
  frm.ShowModal;
  FreeAndNil(frm);
end;

procedure TMainForm.btnTableAddFieldClick(Sender: TObject);
begin
  FieldEditorWindow(Childwin, femFieldAdd);
end;

procedure TMainForm.btnTableEditFieldClick(Sender: TObject);
begin
  Childwin.UpdateField(Sender);
end;

procedure TMainForm.btnTableDropFieldClick(Sender: TObject);
begin
  Childwin.DropField(Sender);
end;

procedure TMainForm.btnTableManageIndexesClick(Sender: TObject);
begin
  FieldEditorWindow(Childwin, femIndexEditor);
end;

procedure TMainForm.actCreateTableExecute(Sender: TObject);
begin
  if Childwin.CreateTableForm = nil then
    Childwin.CreateTableForm := TCreateTableForm.Create(Childwin);
  Childwin.CreateTableForm.ShowModal;
end;

procedure TMainForm.actEmptyTablesExecute(Sender: TObject);
var
  t: TStringList;
  i: Integer;
  sql_pattern: String;
begin
  if Childwin.ListTables.SelectedCount = 0 then
    exit;
  // Add selected items/tables to helper list
  t := GetVTCaptions(Childwin.ListTables, True);
  if MessageDlg('Empty ' + IntToStr(t.count) + ' table(s) ?' + CRLF + '(' + implodestr(', ', t) + ')',
    mtConfirmation, [mbOk, mbCancel], 0) <> mrOk then
    exit;

  Screen.Cursor := crHourglass;
  {**
    @note ansgarbecker: Empty table using faster TRUNCATE statement on newer servers
    @see http://dev.mysql.com/doc/refman/5.0/en/truncate.html
    @see https://sourceforge.net/tracker/index.php?func=detail&aid=1644143&group_id=164593&atid=832350
  }
  if Childwin.mysql_version < 50003 then
    sql_pattern := 'DELETE FROM '
  else
    sql_pattern := 'TRUNCATE ';

  for i:=0 to t.count-1 do
    Childwin.ExecUpdateQuery( sql_pattern + mask(t[i]) );
  t.Free;
  Childwin.MenuRefreshClick(Sender);
  Screen.Cursor := crDefault;
end;

end.
