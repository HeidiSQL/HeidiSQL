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
  ActnList, ImgList, Registry, ShellApi, ToolWin, Clipbrd, db,
  SynMemo, synedit, SynEditTypes, ZDataSet, ZSqlProcessor,
  HeidiComp, sqlhelp, MysqlQueryThread, Childwin, VirtualTrees,
  DateUtils, PngImageList, OptimizeTables, View, Usermanager,
  SelectDBObject, Widestrings, ShlObj;

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
    actCopy: TEditCopy;
    actPaste: TEditPaste;
    actOpenSession: TAction;
    actExitApplication: TAction;
    actCloseSession: TWindowClose;
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
    actCut: TEditCut;
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
    procedure showstatus(msg: string=''; panel: Integer=5);
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
    procedure EnsureConnected;
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
  private
    FDelimiter: String;
    function GetChildwin: TMDIChild;
    function GetParamValue(const paramChar: Char; const paramName:
      string; var curIdx: Byte; out paramValue: string): Boolean;
    procedure SetDelimiter(Value: String);
  public
    virtualDesktopName: string;
    regMain : TRegistry;
    MaintenanceForm: TOptimize;
    ViewForm: TfrmView;
    UserManagerForm: TUserManagerForm;
    SelectDBObjectForm: TfrmSelectDBObject;
    SQLHelpForm: TfrmSQLhelp;
    property Delimiter: String read FDelimiter write SetDelimiter;
    procedure OpenRegistry(Session: String = '');
    procedure CallSQLHelpWithKeyword( keyword: String );
    procedure AddOrRemoveFromQueryLoadHistory( filename: String;
      AddIt: Boolean = true; CheckIfFileExists: Boolean = true );
    procedure popupQueryLoadClick( sender: TObject );
    procedure FillPopupQueryLoad;
    procedure PopupQueryLoadRemoveAbsentFiles( sender: TObject );
    function GetRegValue( valueName: String; defaultValue: Integer; Session: String = '' ) : Integer; Overload;
    function GetRegValue( valueName: String; defaultValue: Boolean; Session: String = '' ) : Boolean; Overload;
    function GetRegValue( valueName: String; defaultValue: String; Session: String = '' ) : String; Overload;
    function CreateMDIChild(parHost, parPort, parUser, parPass, parDatabase, parTimeout, parCompress, parSortDatabases, parDescription: WideString): Boolean;

    // Reference to currently active childwindow:
    property Childwin: TMDIChild read GetChildwin;
end;


procedure InheritFont(AFont: TFont);


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
  Columns  : TWideStringList;
  SubParts : TWideStringList;
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
  createdatabase,
  createtable,
  uVistaFuncs;

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

procedure TMainForm.showstatus(msg: string=''; panel: Integer=5);
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
  with TRegistry.Create do begin
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
    CloseKey;
    Free;
  end;
end;

procedure TMainForm.loadWindowConfig;
var
  ws: String;
begin
  // Called on application start or when monitor configuration has changed.
  with TRegistry.Create do begin
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
  ChildWin.ExecUpdateQuery('FLUSH ' + flushwhat);
  if Sender = actFlushTableswithreadlock then begin
    MessageDlg(
      'Tables have been flushed and read lock acquired.'#10 +
      'Perform backup or snapshot of table data files now.'#10 +
      'Press OK to unlock when done...',
      mtInformation, [mbOk], 0
    );
    ChildWin.ExecUpdateQuery('UNLOCK TABLES');
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
  buffer   : array[0..MAX_PATH] of char;
begin
  if ActiveMDIChild <> nil then
    ActiveMDIChild.Close;

  saveWindowConfig;

  with TRegistry.Create do
  begin
    if OpenKey(REGPATH, true) then
    begin
      // Position of Toolbars
      WriteInteger(REGNAME_TOOLBAR2LEFT, ToolBarStandard.Left);
      WriteInteger(REGNAME_TOOLBAR2TOP, ToolBarStandard.Top);
      WriteInteger(REGNAME_TOOLBARDBLEFT, ToolBarDatabase.Left);
      WriteInteger(REGNAME_TOOLBARDBTOP, ToolBarDatabase.Top);
      WriteInteger(REGNAME_TOOLBARDATALEFT, ToolBarData.Left);
      WriteInteger(REGNAME_TOOLBARDATATOP, ToolBarData.Top);
      WriteInteger(REGNAME_TOOLBARQUERYLEFT, ToolBarQuery.Left);
      WriteInteger(REGNAME_TOOLBARQUERYTOP, ToolBarQuery.Top);

      // Save delimiter
      WriteString( REGNAME_DELIMITER, Delimiter );
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
begin
  caption := APPNAME;
  setLocales;

  // Make Vista miniature window work.
  //Application.MainFormOnTaskBar := True;

  // Use new Vista dialogs per default.
  //UseLatestCommonDialogs := True;

  // Delphi work around to force usage of Vista's default font (other OSes will be unaffected)
  SetVistaFonts(Font);

  SpareBufRefill;
  ErrorProc := HandleRuntimeError;

  refreshMonitorConfig;
  loadWindowConfig;

  // Position of Toolbars
  ToolBarStandard.Left := GetRegValue(REGNAME_TOOLBAR2LEFT, ToolBarStandard.Left);
  ToolBarStandard.Top := GetRegValue(REGNAME_TOOLBAR2TOP, ToolBarStandard.Top);
  ToolBarDatabase.Left := GetRegValue(REGNAME_TOOLBARDBLEFT, ToolBarDatabase.Left);
  ToolBarDatabase.Top := GetRegValue(REGNAME_TOOLBARDBTOP, ToolBarDatabase.Top);
  ToolBarData.Left := GetRegValue(REGNAME_TOOLBARDATALEFT, ToolBarData.Left);
  ToolBarData.Top := GetRegValue(REGNAME_TOOLBARDATATOP, ToolBarData.Top);
  ToolBarQuery.Left := GetRegValue(REGNAME_TOOLBARQUERYLEFT, ToolBarQuery.Left);
  ToolBarQuery.Top := GetRegValue(REGNAME_TOOLBARQUERYTOP, ToolBarQuery.Top);

  // Delimiter
  Delimiter := GetRegValue(REGNAME_DELIMITER, DEFAULT_DELIMITER);

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

  // SQLFiles-History
  FillPopupQueryLoad;
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
    actOpenSession.Execute;
end;



procedure TMainForm.actCreateDatabaseExecute(Sender: TObject);
var
  newdb: String;
begin
  // Create database:
  // Create modal form once on demand
  if Childwin.CreateDatabaseForm = nil then
    Childwin.CreateDatabaseForm := TCreateDatabaseForm.Create(Self);

  // Rely on the modalresult being set correctly
  if Childwin.CreateDatabaseForm.ShowModal = mrOK then
  begin
    newdb := Childwin.CreateDatabaseForm.editDBName.Text;
    // Add DB to OnlyDBs-regkey if this is not empty
    if Childwin.DatabasesWanted.Count > 0 then
    begin
      Childwin.DatabasesWanted.Add( newdb );
      with TRegistry.Create do
      begin
        if OpenKey(REGPATH + REGKEY_SESSIONS + Childwin.Conn.Description, false) then
        begin
          WriteString( 'OnlyDBs', ImplodeStr( ';', Childwin.DatabasesWanted ) );
          CloseKey;
        end;
        Free;
      end;
    end;
    // reload db nodes and switch to new one
    Childwin.RefreshTree(False, newdb);
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
begin
  room := 0;
  for i := 1 to Statusbar.Panels.Count - 1 do
    inc(room, Statusbar.Panels[i].Width);
  StatusBar.Panels[0].Width := Statusbar.Width - room;
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
    m := Childwin.SynMemoQuery
  else begin
    m := Childwin.SynMemoFilter;
    Childwin.editFilterSearch.Clear;
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
    if actEditView.ActionComponent = Childwin.menuEditView then begin
      // "Edit view" was clicked in ListTables' context menu
      NodeData := Childwin.ListTables.GetNodeData(Childwin.ListTables.FocusedNode);
      ViewForm.EditViewName := NodeData.Captions[0];
    end else if actEditView.ActionComponent = Childwin.menuTreeEditView then begin
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



procedure TMainForm.actWebbrowse(Sender: TObject);
begin
  // Browse to URL (hint)
  ShellExec( TAction(Sender).Hint );
end;


// Escape database, table, field, index or key name.
function TMainform.mask(str: WideString) : WideString;
begin
  if ActiveMDIChild = nil then
    raise Exception.Create('Cannot mask SQL without active MDI');
  result := ChildWin.mask(str);
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
  ChildWin.ExecSqlClick(sender, false);
end;

procedure TMainForm.actExecuteSelectionExecute(Sender: TObject);
begin
  ChildWin.ExecSqlClick(sender, true);
end;

procedure TMainForm.actExecuteLineExecute(Sender: TObject);
begin
  ChildWin.ExecSqlClick(sender, false, true);
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
    GridData := ChildWin.ActiveData;
    GridToCsv(ChildWin.ActiveGrid, GridData, ChildWin.prefCSVSeparator, ChildWin.prefCSVEncloser, ChildWin.prefCSVTerminator, S);
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
  if ChildWin.ActiveGrid = Childwin.DataGrid then Title := Childwin.SelectedTable
  else Title := 'SQL query';
  try
    GridData := ChildWin.ActiveData;
    GridToHtml(ChildWin.ActiveGrid, GridData, Title, S);
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
  if ChildWin.ActiveGrid = Childwin.DataGrid then Root := Childwin.SelectedTable
  else Root := 'SQL query';
  try
    GridData := ChildWin.ActiveData;
    GridToXml(ChildWin.ActiveGrid, GridData, Root, S);
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
  if ChildWin.ActiveGrid = Childwin.DataGrid then Tablename := Childwin.SelectedTable
  else Tablename := 'unknown';
  try
    GridData := ChildWin.ActiveData;
    GridToSql(ChildWin.ActiveGrid, GridData, Tablename, S);
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
  Dialog := ChildWin.SaveDialogExportData;

  Grid := Childwin.ActiveGrid;
  GridData := Childwin.ActiveData;
  if Grid = Childwin.DataGrid then
    Title := ChildWin.SelectedTable
  else
    Title := 'SQL query';

  Dialog.FileName := Title;
  Dialog.Title := 'Export result set from '+Dialog.Filename+'...';

  if Dialog.Execute and (Dialog.FileName <> '') then try
    Screen.Cursor := crHourGlass;
    FS := openfs(Dialog.FileName);
    case Dialog.FilterIndex of
      1: GridToCsv(Grid, GridData, ChildWin.prefCSVSeparator, ChildWin.prefCSVEncloser, ChildWin.prefCSVTerminator, FS);
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
  tmppath        : array[0..MAX_PATH] of char;
  Content        : WideString;
  IsBinary       : Boolean;
begin
  g := ChildWin.ActiveGrid;
  if g = nil then begin messagebeep(MB_ICONASTERISK); exit; end;
  Screen.Cursor := crHourGlass;
  showstatus('Saving contents to file...');
  if g = Childwin.DataGrid then begin
    Childwin.EnsureFullWidth(g, g.FocusedColumn, g.FocusedNode);
    IsBinary := Childwin.FDataGridResult.Columns[Childwin.DataGrid.FocusedColumn].IsBinary;
  end else begin
    IsBinary := Childwin.FQueryGridResult.Columns[Childwin.QueryGrid.FocusedColumn].IsBinary;
  end;
  Content := g.Text[g.FocusedNode, g.FocusedColumn];
  childwin.logsql(g.Name);

  GetTempPath(MAX_PATH, tmppath);
  filename := tmppath;
  filename := filename+'\'+APPNAME+'-preview.';
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
  activeDB := Childwin.ActiveDatabase;

  Tables := TWideStringlist.Create;
  Views := TWideStringlist.Create;
  if (Childwin.PageControlMain.ActivePage = Childwin.tabDatabase) and
    ((Sender as TAction).ActionComponent <> Childwin.PopupMenuDropTable) then begin
    // Invoked from one of the various buttons, SheetDatabase is the active page, drop highlighted table(s).
    Tables := GetVTCaptions(Childwin.ListTables, True, 0, NODETYPE_TABLE);
    Tables.AddStrings(GetVTCaptions(Childwin.ListTables, True, 0, NODETYPE_CRASHED_TABLE));
    Views := GetVTCaptions(Childwin.ListTables, True, 0, NODETYPE_VIEW);
  end else begin
    // Invoked from one of the various buttons, drop table selected in tree view.
    case Childwin.GetSelectedNodeType of
      NODETYPE_TABLE: Tables.Add(Childwin.SelectedTable);
      NODETYPE_CRASHED_TABLE: Tables.Add(Childwin.SelectedTable);
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
procedure TMainForm.actLoadSQLExecute(Sender: TObject);
begin
  ChildWin.PageControlMain.ActivePage := ChildWin.tabQuery;
  if ChildWin.OpenDialogSQLfile.Execute then
    ChildWin.QueryLoad( ChildWin.OpenDialogSQLfile.FileName );
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
function TMainform.CreateMDIChild(parHost, parPort, parUser, parPass, parDatabase, parTimeout, parCompress, parSortDatabases, parDescription: WideString): Boolean;
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


procedure TMainForm.actDataDeleteExecute(Sender: TObject);
begin
  // Delete row(s)
  if (Childwin.DataGrid.SelectedCount = 1) and
    (Childwin.FDataGridResult.Rows[Childwin.DataGrid.GetFirstSelected.Index].State = grsInserted)
    then begin
    // Deleting the virtual row which is only in memory by stopping edit mode
    actDataCancelChanges.Execute;
  end else begin
    // The "normal" case: Delete existing rows
    if not Childwin.CheckUniqueKeyClause then
      Exit;
    if Childwin.DataGrid.SelectedCount = 0 then
      MessageDLG('Please select one or more rows to delete them.', mtError, [mbOK], 0)
    else if MessageDLG('Delete '+inttostr(Childwin.DataGrid.SelectedCount)+' row(s)?',
        mtConfirmation, [mbOK, mbCancel], 0) = mrOK then begin
      Childwin.GridPostDelete(Childwin.DataGrid);
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
  FieldEditorWindow(Childwin, femFieldAdd);
end;


procedure TMainForm.actEditFieldExecute(Sender: TObject);
var
  fieldname: WideString;
  fem: TFieldEditorMode;
begin
  fieldname := '';
  fem := femFieldAdd;
  if Assigned(Childwin.ListColumns.FocusedNode) and (vsSelected in Childwin.ListColumns.FocusedNode.States) then
    fieldname := Childwin.ListColumns.Text[Childwin.ListColumns.FocusedNode, 0];
  if fieldname <> '' then
    fem := femFieldUpdate;
  FieldEditorWindow(Childwin, fem, fieldname);
end;


procedure TMainForm.actDropFieldsExecute(Sender: TObject);
var
  i: Integer;
  dropCmd: String;
  dropList: TWideStringList;
begin
  // We allow the user to select and delete multiple listItems
  dropList := GetVTCaptions( Childwin.ListColumns, True );
  // User confirmation
  if MessageDlg('Delete ' + IntToStr(dropList.Count) + ' column(s): ' + ImplodeStr( ', ', dropList ) + ' ?', mtConfirmation, [mbok,mbcancel], 0) = mrok then
  try
    // Concat fields for ALTER query
    for i := 0 to dropList.Count - 1 do
      dropCmd := dropCmd + 'DROP ' + mask(dropList[i]) + ', ';
    // Remove trailing comma
    delete(dropCmd, Length(dropCmd)-1, 2);
    // Execute field dropping
    Childwin.ExecUpdateQuery( 'ALTER TABLE '+mask(Childwin.SelectedTable)+' ' + dropCmd );
    // Rely on the server respective ExecUpdateQuery has raised an exception so the
    // following code will be skipped on any error
    Childwin.ListColumns.BeginUpdate;
    Childwin.ListColumns.DeleteSelectedNodes;
    Childwin.ListColumns.EndUpdate;
    Childwin.FSelectedTableColumns := nil;
    Childwin.FSelectedTableKeys := nil;
    // Set focus on first item
    Childwin.ListColumns.FocusedNode := Childwin.ListColumns.GetFirstVisible;
  except
    On E : Exception do begin
      MessageDlg( E.Message, mtError, [mbOK], 0 );
    end;
  end;
end;


procedure TMainForm.actEditIndexesExecute(Sender: TObject);
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
  t: TWideStringList;
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


procedure TMainForm.actEditTableFieldsExecute(Sender: TObject);
var
  NodeData: PVTreeData;
begin
  // table-doubleclick
  if Assigned(Childwin.ListTables.FocusedNode) then begin
    NodeData := Childwin.ListTables.GetNodeData(Childwin.ListTables.FocusedNode);
    Childwin.SelectedTable := NodeData.Captions[0];
    Childwin.PageControlMain.ActivePage := Childwin.tabTable;
  end;
end;


procedure TMainForm.actEditTablePropertiesExecute(Sender: TObject);
var
  NodeData: PVTreeData;
  caller: TComponent;
begin
  if Childwin.TablePropertiesForm = nil then
    Childwin.TablePropertiesForm := Ttbl_properties_form.Create(Self);

  caller := TAction(Sender).ActionComponent;
  if caller = Childwin.menuTreeAlterTable then
    Childwin.TablePropertiesForm.TableName := Childwin.SelectedTable
  else begin
    NodeData := Childwin.ListTables.GetNodeData( Childwin.ListTables.FocusedNode );
    Childwin.TablePropertiesForm.TableName := NodeData.Captions[0];
  end;

  Childwin.TablePropertiesForm.ShowModal;
end;


procedure TMainForm.actDropDatabaseExecute(Sender: TObject);
var
  tndb: PVirtualNode;
  db: String;
begin
  // Drop DB.
  case Childwin.DBtree.GetNodeLevel(Childwin.DBtree.GetFirstSelected) of
    1: tndb := Childwin.DBtree.GetFirstSelected;
    2: tndb := Childwin.DBtree.GetFirstSelected.Parent;
    else Exit;
  end;
  if not Assigned(tndb) then raise Exception.Create('Internal error: Cannot drop NIL database.');
  db := Childwin.Databases[tndb.Index];

  if MessageDlg('Drop Database "'+db+'"?' + crlf + crlf + 'WARNING: You will lose all tables in database '+db+'!', mtConfirmation, [mbok,mbcancel], 0) <> mrok then
    Abort;

  Screen.Cursor := crHourglass;
  try
    Childwin.ExecUpdateQuery( 'DROP DATABASE ' + mask(db) );
    Childwin.ClearDbTableList(db);
    if Childwin.DatabasesWanted.IndexOf(db) > -1 then begin
      Childwin.DatabasesWanted.Delete( Childwin.DatabasesWanted.IndexOf(db) );
      with TRegistry.Create do begin
        if OpenKey(REGPATH + REGKEY_SESSIONS + Childwin.Conn.Description, false) then begin
          WriteString( 'OnlyDBs', ImplodeStr( ';', Childwin.DatabasesWanted ) );
          CloseKey;
        end;
        Free;
      end;
    end;
    Childwin.DBtree.Selected[Childwin.DBtree.GetFirst] := true;
    Childwin.RefreshTree(False);
  except
    MessageDLG('Dropping failed.'+crlf+'Maybe '''+db+''' is not a valid database-name.', mtError, [mbOK], 0)
  end;
  Screen.Cursor := crDefault;
end;


procedure TMainForm.actEditDatabaseExecute(Sender: TObject);
begin
  if Childwin.CreateDatabaseForm = nil then
    Childwin.CreateDatabaseForm := TCreateDatabaseForm.Create(Self);
  Childwin.CreateDatabaseForm.modifyDB := Childwin.ActiveDatabase;
  Childwin.CreateDatabaseForm.ShowModal;
end;


procedure TMainForm.actOpenSessionExecute(Sender: TObject);
var
  dlgResult: Integer;
begin
  if ActiveMDIChild = nil then begin
    dlgResult := ConnectionWindow(Self);
    if dlgResult = mrCancel then Close;
  end else begin
    debug('perf: new connection clicked.');
    ShellExec( ExtractFileName(paramstr(0)), ExtractFilePath(paramstr(0)) );
  end;
end;


procedure TMainForm.actQueryFindExecute(Sender: TObject);
var
  m: TSynMemo;
begin
  FindDialogQuery.execute;
  m := Childwin.SynMemoQuery;
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
  m := Childwin.SynMemoQuery;
  // if something is selected search for that text
  if m.SelAvail and (m.BlockBegin.Line = m.BlockEnd.Line)
  then
    ReplaceDialogQuery.FindText := m.SelText
  else
    ReplaceDialogQuery.FindText := m.GetWordAtRowCol(m.CaretXY);
end;


procedure TMainForm.actRefreshExecute(Sender: TObject);
begin
  // Refresh
  // Force data tab update when appropriate.
  Childwin.dataselected := false;
  if ChildWin.PageControlMain.ActivePage = ChildWin.tabHost then
    ChildWin.ShowVariablesAndProcesses(self)
  else if ChildWin.PageControlMain.ActivePage = ChildWin.tabDatabase then
    ChildWin.MenuRefreshClick(self)
  else if ChildWin.PageControlMain.ActivePage = ChildWin.tabTable then
    ChildWin.ShowTableProperties
  else if ChildWin.PageControlMain.ActivePage = ChildWin.tabData then
    ChildWin.viewdata(Sender)
  else
    ChildWin.RefreshTree(True);
end;


procedure TMainForm.actSQLhelpExecute(Sender: TObject);
var
  keyword : String;
begin
  // Call SQL Help from various places
  if Childwin.mysql_version < 40100 then
    exit;

  keyword := '';
  // Query-Tab
  if Childwin.ActiveControl is TSynMemo then
    keyword := TSynMemo(Childwin.ActiveControl).WordAtCursor
  // Data-Tab
  else if (Childwin.PageControlMain.ActivePage = Childwin.tabData)
    and Assigned(Childwin.DataGrid.FocusedNode) then begin
    keyword := Childwin.VTRowDataListColumns[Childwin.DataGrid.FocusedColumn].Captions[1];
  end
  // Table-Tab
  else if Childwin.ListColumns.Focused and Assigned(Childwin.ListColumns.FocusedNode) then
  begin
    keyword := Childwin.ListColumns.Text[Childwin.ListColumns.FocusedNode, 1];
  end
  else if Childwin.lboxQueryHelpers.Focused then
  begin
    // Makes only sense if one of the tabs "SQL fn" or "SQL kw" was selected
    if Childwin.tabsetQueryHelpers.TabIndex in [1,2] then
    begin
      keyword := Childwin.lboxQueryHelpers.Items[Childwin.lboxQueryHelpers.ItemIndex];
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
  if Childwin.SaveDialogSQLFile.Execute then
  begin
    Screen.Cursor := crHourGlass;
    // Save complete content or just the selected text,
    // depending on the tag of calling control
    case (Sender as TAction).Tag of
      0: SaveUnicodeFile( Childwin.SaveDialogSQLFile.FileName, Childwin.SynMemoQuery.Text );
      1: SaveUnicodeFile( Childwin.SaveDialogSQLFile.FileName, Childwin.SynMemoQuery.SelText );
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
      0: SaveUnicodeFile(snippetname, Childwin.SynMemoQuery.Text);
      1: SaveUnicodeFile(snippetname, Childwin.SynMemoQuery.SelText);
    end;
    FillPopupQueryLoad;
    if Childwin.tabsetQueryHelpers.TabIndex = 3 then begin
      // SQL Snippets selected in query helper, refresh list
      mayChange := True; // Unused; satisfies callee parameter collection which is probably dictated by tabset.
      Childwin.tabsetQueryHelpersChange(Sender, 3, mayChange);
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
  Childwin.SynMemoQuery.WordWrap := TAction(Sender).Checked;
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
  if Childwin.SynMemoQuery.SearchReplace(Search, '', Options) = 0 then
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
  if Childwin.SynMemoQuery.SearchReplace( Search, ReplaceDialogQuery.ReplaceText, Options) = 0 then
  begin
    MessageBeep(MB_ICONASTERISK);
    ShowStatus( 'SearchText ''' + Search + ''' not found!', 0);
    if ssoBackwards in Options then
      Childwin.SynMemoQuery.BlockEnd := Childwin.SynMemoQuery.BlockBegin
    else
      Childwin.SynMemoQuery.BlockBegin := Childwin.SynMemoQuery.BlockEnd;
    Childwin.SynMemoQuery.CaretXY := Childwin.SynMemoQuery.BlockBegin;
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
  Childwin.QueryLoad( filename );
end;


procedure TMainform.AddOrRemoveFromQueryLoadHistory( filename: String; AddIt: Boolean = true; CheckIfFileExists: Boolean = true );
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
    newfilelist.Add( filename );

  // Add all other filenames
  for i:=0 to Values.Count-1 do begin
    if Pos( 'SQLFile', Values[i] ) <> 1 then
      continue;
    savedfilename := Mainform.GetRegValue( Values[i], '' );
    reg.DeleteValue( Values[i] );
    if CheckIfFileExists and (not FileExists( savedfilename )) then
      continue;
    if (savedfilename <> filename) and (newfilelist.IndexOf(savedfilename)=-1) then
      newfilelist.add( savedfilename );
  end;

  // Save new list
  for i := 0 to newfilelist.Count-1 do begin
    if i >= 20 then
      break;
    reg.WriteString( 'SQLFile'+IntToStr(i), newfilelist[i] );
  end;

  reg.Free;
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
  if Value = FDelimiter then
    Exit;
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
    if Assigned(Childwin) then Childwin.LogSQL(ErrMsg);
    Raise Exception.Create(ErrMsg);
  end else begin
    FDelimiter := Value;
    if Assigned(Childwin) then Childwin.LogSQL(Format('Delimiter changed to %s.', [Delimiter]));
    actSetDelimiter.Hint := actSetDelimiter.Caption + ' (current value: '+Delimiter+')';
  end;
end;


procedure TMainForm.actApplyFilterExecute(Sender: TObject);
begin
  Childwin.viewdata(Sender);
end;


procedure TMainForm.actDataFirstExecute(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := Childwin.DataGrid.GetFirst;
  if Assigned(Node) then begin
    Childwin.DataGrid.ClearSelection;
    Childwin.DataGrid.FocusedNode := Node;
    Childwin.DataGrid.Selected[Node] := True;
  end;
end;

procedure TMainForm.actDataInsertExecute(Sender: TObject);
begin
  Childwin.DataGridInsertRow;
end;

procedure TMainForm.actDataLastExecute(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := Childwin.DataGrid.GetLast;
  if Assigned(Node) then begin
    Childwin.DataGrid.ClearSelection;
    Childwin.DataGrid.FocusedNode := Node;
    Childwin.DataGrid.Selected[Node] := True;
  end;
end;

procedure TMainForm.actDataPostChangesExecute(Sender: TObject);
begin
  Childwin.DataGridPostUpdateOrInsert(Childwin.Datagrid.FocusedNode);
end;

procedure TMainForm.actRemoveFilterExecute(Sender: TObject);
begin
  actClearFilterEditor.Execute;
  Childwin.viewdata(Sender);
end;


procedure TMainForm.actDataCancelChangesExecute(Sender: TObject);
begin
  Childwin.DataGridCancel(Sender);
end;


procedure TMainForm.actSelectTreeBackgroundExecute(Sender: TObject);
var
  cs: TColorSelect;
begin
  // Select database tree background color
  cs := TColorSelect.Create(Self);
  cs.Dialog.Color := Childwin.DBtree.Color;
  if cs.Execute then begin
    Childwin.DBtree.Color := cs.Dialog.Color;
    OpenRegistry(Childwin.SessionName);
    regmain.WriteInteger(REGNAME_TREEBACKGROUND, cs.Dialog.Color);
  end;
end;


end.
