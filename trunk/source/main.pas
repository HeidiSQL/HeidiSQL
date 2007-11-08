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
  SynMemo, synedit, SynEditTypes, smdbgrid, ZDataSet, ZSqlProcessor,
  HeidiComp, sqlhelp, MysqlQueryThread, Childwin, VirtualTrees;

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
    ImageList1: TImageList;
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
    Diagnostics: TAction;
    OptimizeTables1: TMenuItem;
    ImExport1: TMenuItem;
    CopyContentsasHTMLTable1: TMenuItem;
    CopyHTMLtable: TAction;
    Copy2CSV: TAction;
    menuWebsite: TMenuItem;
    N9: TMenuItem;
    N11: TMenuItem;
    PrintList: TAction;
    CopyTable: TAction;
    ControlBar1: TControlBar;
    ToolBarStandard: TToolBar;
    ToolButton9: TToolButton;
    FileCloseItem2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton12: TToolButton;
    ToolButton7: TToolButton;
    ButtonCreateDatabase: TToolButton;
    ButtonCreateTable: TToolButton;
    ButtonDropDatabase: TToolButton;
    ButtonDropTable: TToolButton;
    ToolButton4: TToolButton;
    ButtonRefresh: TToolButton;
    ButtonReload: TToolButton;
    ToolButton13: TToolButton;
    ButtonImportTextfile: TToolButton;
    ButtonExport: TToolButton;
    ButtonUserManager: TToolButton;
    ToolBarData: TToolBar;
    DBNavigator1: TDBNavigator;
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
    DropTable: TAction;
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
    ToolButton1: TToolButton;
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
    procedure ButtonCreateTableClick(Sender: TObject);
    procedure ButtonDropDatabaseClick(Sender: TObject);
    procedure ResetWindowOptions1Click(Sender: TObject);
    procedure ButtonImportTextfileClick(Sender: TObject);
    procedure MenuPreferencesClick(Sender: TObject);
    procedure menuReadmeClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure UserManagerExecute(Sender: TObject);
    procedure ShowAboutBoxExecute(Sender: TObject);
    procedure DiagnosticsExecute(Sender: TObject);
    procedure CopyHTMLtableExecute(Sender: TObject);
    procedure Copy2CSVExecute(Sender: TObject);
    procedure PrintListExecute(Sender: TObject);
    procedure CopyTableExecute(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);
    procedure showstatus(msg: string='';  panel : Integer=0; busy: Boolean=false);
    procedure ButtonOKClick(Sender: TObject);
    procedure CheckBoxLimitClick(Sender: TObject);
    procedure LimitPanelEnter(Sender: TObject);
    procedure LimitPanelExit(Sender: TObject);
    procedure OpenURL(Sender: TObject);
    function mask(str: String) : String;
    procedure ExportSettings1Click(Sender: TObject);
    procedure Importsettings1Click(Sender: TObject);
    procedure ExecuteQueryExecute(Sender: TObject);
    procedure ExecuteSelectionExecute(Sender: TObject);
    procedure Copy2XMLExecute(Sender: TObject);
    procedure DBNavigator1BeforeAction(Sender: TObject;
      Button: TNavigateBtn);
    procedure ExportDataExecute(Sender: TObject);
    procedure ExecuteLineExecute(Sender: TObject);
    procedure HTMLviewExecute(Sender: TObject);
    procedure InsertFilesExecute(Sender: TObject);
    procedure ExportTablesExecute(Sender: TObject);
    procedure DataSearchExecute(Sender: TObject);
    procedure DropTableExecute(Sender: TObject);
    procedure LoadSQLExecute(Sender: TObject);
    procedure EnsureConnected;
    function ExecuteRemoteQuery(sender: THandle; query: string): TDataSet;
    procedure ExecuteRemoteNonQuery(sender: THandle; query: string);
    procedure HandleWMComplete(var msg: TMessage); message WM_COMPLETED;
    procedure HandleWMCopyData(var msg: TWMCopyData); message WM_COPYDATA;
    procedure HandleWMProcessLog(var msg: TMessage); message WM_PROCESSLOG;
    procedure HandleWMClearRightClickPointer(var msg: TMessage); message WM_CLEAR_RIGHTCLICK_POINTER;
  private
    function GetChildwin: TMDIChild;
  public
    function GetRegValue( valueName: String; defaultValue: Integer; key: String = '' ) : Integer; Overload;
    function GetRegValue( valueName: String; defaultValue: Boolean; key: String = '' ) : Boolean; Overload;
    procedure SaveRegValue( valueName: String; value: Integer; key: String = '' ); Overload;
    procedure SaveRegValue( valueName: String; value: Boolean; key: String = '' ); Overload;

    // Reference to currently active childwindow:
    property Childwin: TMDIChild read GetChildwin;
end;

var
  MainForm            : TMainForm;
  highestcon          : Integer;                       // Remember last connection
  appstarted          : Boolean = false;               // see connections.pas
  StatusText          : String = 'Initializing...';
  StatusIconIndex     : Integer = 43;
  loadsqlfile         : boolean = true;               // load sql-file into query-memo at startup?
  appversion          : String = 'x.y $Rev$';
  DirnameCommonAppData,
  DirnameUserAppData,
  DIRNAME_SNIPPETS,
  DirnameSessionLogs  : String;

const
  discname = 'not connected';
  ICON_MYSELF_CONNECTED = 89;
  ICON_MYSELF_DISCONNECTED = 92;
  ICON_OTHER_CONNECTED = 91;
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
  usermanager,
  options,
  optimizetables,
  printlist,
  copytable,
  insertfiles,
  Helpers,
  Threading,
  mysql_structures;

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

procedure TMainForm.HandleWMClearRightClickPointer(var msg: TMessage);
begin
  debug('clearing stored right click item');
  ChildWin.DBRightClickSelectedItem := nil;
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

procedure TMainForm.showstatus(msg: string='';  panel : Integer=0; busy: Boolean=false);
begin
  // show Message in statusbar
  if panel = 2 then begin
    StatusText := msg;
    if busy then
      StatusIconIndex := 43
    else
      StatusIconIndex := 42;
  end
  else
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
      WriteString('windowstate', ws);
      WriteInteger('windowleft', left);
      WriteInteger('windowtop', top);
      WriteInteger('windowwidth', width);
      WriteInteger('windowheight', height);
      // Position of Toolbars
      WriteInteger('ToolBar2Left', ToolBarStandard.Left);
      WriteInteger('ToolBarDataLeft', ToolBarData.Left);
      WriteInteger('ToolBar2Top', ToolBarStandard.Top);
      WriteInteger('ToolBarDataTop', ToolBarData.Top);
    end;
    CloseKey;
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
begin
  caption := APPNAME;
  setLocales;

  with TRegistry.Create do
  begin
    if OpenKey(REGPATH, true) then
    begin
      ws := ReadString('windowstate');
      if ws = 'Minimized'
        then windowstate := wsMinimized else
      if ws = 'Normal' then begin
        windowstate := wsNormal;
        left := ReadInteger('windowleft');
        top := ReadInteger('windowtop');
        width := ReadInteger('windowwidth');
        height := ReadInteger('windowheight');
      end else
      if ws = 'Maximized'
        then windowstate := wsMaximized;

      // Position of Toolbars
      if valueExists('ToolBar2Left') then
        ToolBarStandard.Left := ReadInteger('ToolBar2Left');
      if valueExists('ToolBarDataLeft') then
        ToolBarData.Left := ReadInteger('ToolBarDataLeft');
      if valueExists('ToolBar2Top') then
        ToolBarStandard.Top := ReadInteger('ToolBar2Top');
      if valueExists('ToolBarDataTop') then
        ToolBarData.Top := ReadInteger('ToolBarDataTop');
    end;
    CloseKey;
  end;

  // Beautify appversion
  if Pos('$Rev: WC', appversion) < 1 then begin
    appversion := Copy(appversion, 1, Pos('$Rev', appversion) + 4);
    appversion := appversion + ' unknown';
  end else appversion := StringReplace(appversion, 'WC ', '', []);
  appversion := StringReplace( appversion, '$Rev', 'Revision', [rfIgnoreCase] );
  appversion := StringReplace( appversion, '$', '', [] );
  appversion := Trim( appversion );

  // "All users" folder for HeidiSQL's data (All Users\Application Data)
  DirnameCommonAppData := GetShellFolder(CSIDL_COMMON_APPDATA) + '\' + APPNAME + '\';

  // User folder for HeidiSQL's data (<user name>\Application Data)
  DirnameUserAppData := GetShellFolder(CSIDL_APPDATA) + '\' + APPNAME + '\';

  // Folder which contains snippet-files
  DIRNAME_SNIPPETS := DirnameCommonAppData + 'Snippets\';

  // Folder for session logfiles
  DirnameSessionLogs := DirnameUserAppData + 'Sessionlogs\';
end;


procedure TMainForm.FormShow(Sender: TObject);
begin
  // Cannot be done in OnCreate because we need ready forms here:
  ShowConnections(self);
end;


procedure TMainForm.ButtonRefreshClick(Sender: TObject);
begin
  // Refresh
  if ChildWin.PageControlMain.ActivePage = ChildWin.tabHost then
    ChildWin.ShowVariablesAndProcesses(self)
  else if ChildWin.PageControlMain.ActivePage = ChildWin.tabDatabase then
    ChildWin.MenuRefreshClick(self)
  else if ChildWin.PageControlMain.ActivePage = ChildWin.tabTable then
    ChildWin.ShowTableProperties(ChildWin.SelectedTable)
  else if ChildWin.PageControlMain.ActivePage = ChildWin.tabData then
    ChildWin.viewdata(self)
  else ChildWin.ReadDatabasesAndTables(self);
end;

procedure TMainForm.ButtonCreateDatabaseClick(Sender: TObject);
begin
  // create database
  ChildWin.CreateDatabase(self);
end;

procedure TMainForm.ButtonCreateTableClick(Sender: TObject);
begin
  // create table
  ChildWin.CreateTable(self);
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
      DeleteValue('childwinstate');
      DeleteValue('childwinleft');
      DeleteValue('childwintop');
      DeleteValue('childwinwidth');
      DeleteValue('childwinheight');
      DeleteValue('querymemoheight');
      DeleteValue('dbtreewidth');
      DeleteValue('sqloutheight');

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
begin
  StatusBar.Panels[0].Width := (width - StatusBar.Panels[1].Width) - StatusBar.Panels[2].Width;
end;

procedure TMainForm.UserManagerExecute(Sender: TObject);
begin
  UserManagerWindow (Self);
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

procedure TMainForm.DiagnosticsExecute(Sender: TObject);
begin
  // optimize / repair... tables
  TableDiagnosticsWindow (Self);
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
    dataset2html(ChildWin.GetVisualDataset(), TZQuery(ChildWin.GetVisualDataset()).Sql.Text, '', ChildWin.prefConvertHTMLEntities, APPNAME + ' ' + appversion )
  else if ChildWin.PageControlMain.ActivePage = ChildWin.tabQuery then
    dataset2html(ChildWin.GetVisualDataset(), TZReadOnlyQuery(ChildWin.GetVisualDataset()).Sql.Text, '', ChildWin.prefConvertHTMLEntities, APPNAME + ' ' + appversion);
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


procedure TMainForm.CopyTableExecute(Sender: TObject);
begin
  // copy table
  CopyTableWindow(self);
end;


procedure TMainForm.StatusBarDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
begin
  // clear panel
  StatusBar.Canvas.Pen.Color := StatusBar.Canvas.Brush.Color;
  StatusBar.Canvas.Rectangle(rect);
  StatusBar.Canvas.Pen.Color := clWindowText;
  // draw icon and message
  ImageList1.Draw(StatusBar.Canvas, Rect.Left, Rect.Top, StatusIconIndex);
  StatusBar.Canvas.TextOut(Rect.left + 17, Rect.top+1, StatusText);
end;


procedure TMainForm.ButtonOKClick(Sender: TObject);
begin
  // Set Filter
  ChildWin.DBTree.SetFocus;
  ChildWin.viewdata(self);
end;


procedure TMainForm.CheckBoxLimitClick(Sender: TObject);
begin
  // Check/Uncheck popupmenu-item
  if ChildWin <> nil then
    ChildWin.MenuLimit.Checked := CheckBoxLimit.Checked;
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
    if winexec(pchar('regedit.exe /e "'+SaveDialog2.FileName+'" HKEY_CURRENT_USER\'+REGPATH), SW_SHOW) = ERROR_FILE_NOT_FOUND then
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

procedure TMainForm.DBNavigator1BeforeAction(Sender: TObject;
  Button: TNavigateBtn);
begin
  if Button = nbdelete then begin
    ChildWin.Delete1Click(sender);
    abort;
  end;
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
        2 : dataset2html(query, FileName, FileName, ConvertHTMLSpecialChars, APPNAME+' '+appversion);
        3 : dataset2xml(query, FileName, FileName);
      end;
      ChildWin.prefCSVSeparator := FieldSep;
      ChildWin.prefCSVTerminator := LineSep;
      ChildWin.prefCSVEncloser := FieldEncl;
      ChildWin.prefConvertHTMLEntities := ConvertHTMLSpecialChars;
      with TRegistry.Create do
      begin
        openkey(REGPATH, true);
        WriteBool('ConvertHTMLEntities', ConvertHTMLSpecialChars);
        WriteString('CSVSeparator', FieldSep);
        WriteString('CSVEncloser', FieldEncl);
        WriteString('CSVTerminator', LineSep);
        closekey();
      end;
      Screen.Cursor := crDefault;
    end;
  end;

end;



// view HTML
procedure TMainForm.HTMLviewExecute(Sender: TObject);
var
  g              : TSMDBGrid;
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
  showstatus('Saving contents to file...', 2, true);
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
  ShowStatus( STATUS_MSG_READY, 2 );
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
procedure TMainForm.DropTableExecute(Sender: TObject);
var
  i : Integer;
  tndb : TTreeNode;
  t : TStringList;
  db, msg, sql, activeDB : String;
begin
  debug('drop table activated');
  t := TStringlist.Create;
  // Set default database name to to ActiveDatabase.
  // Can be overwritten when someone selects a table in dbtree from different database
  activeDB := Childwin.ActiveDatabase;
  db := activeDB;
  tndb := nil;

  if (Sender as TBasicAction).ActionComponent = Childwin.PopupMenuDropTable then begin
    // Invoked by tree menu popup.
    tndb := Childwin.DBRightClickSelectedItem.Parent;
    db := tndb.Text;
    t.add( Childwin.DBRightClickSelectedItem.Text );
  end else if Childwin.PageControlMain.ActivePage = Childwin.tabDatabase then begin
    // Invoked from one of the various buttons, SheetDatabase is the active page, drop highlighted table(s).
    t := GetVTCaptions(Childwin.ListTables, True);
  end else begin
    // Invoked from one of the various buttons, drop table selected in tree view.
    t.add( Childwin.SelectedTable );
  end;

  // Fix actions temporarily enabled for popup menu.
  Childwin.ValidateControls;

  // Safety stop to avoid firing DROP TABLE without tablenames
  if t.Count = 0 then
    Exit;

  // Ask user for confirmation to drop selected tables
  msg := 'Drop ' + inttostr(t.count) + ' table(s) in database "'+db+'"?' + crlf + crlf + implodestr(', ', t);
  if MessageDlg(msg, mtConfirmation, [mbok,mbcancel], 0) <> mrok then
    Exit;

  // Form and execute SQL
  sql := 'DROP TABLE ';
  for i := 0 to t.Count - 1 do
  begin
    if i > 0 then
      sql := sql + ', ';
    if db <> activeDB then
      sql := sql + Childwin.mask(db) + '.';
    sql := sql + Childwin.mask(t[i]);
  end;
  Childwin.ExecUpdateQuery( sql );

  // Refresh ListTables + dbtree so the dropped tables are gone:
  if db = activeDB then
    Childwin.MenuRefreshClick(Sender)
  else
    Childwin.PopulateTreeTableList( tndb, '', True );
end;


// Load SQL-file, make sure that SheetQuery is activated
procedure TMainForm.LoadSQLExecute(Sender: TObject);
begin
  ChildWin.PageControlMain.ActivePage := ChildWin.tabQuery;
  ChildWin.btnQueryLoadClick( sender );
end;


function TMainForm.GetRegValue( valueName: String; defaultValue: Integer; key: String = '' ) : Integer;
var
  reg : TRegistry;
  folder : String;
begin
  result := defaultValue;
  reg := TRegistry.Create;
  folder := REGPATH;
  if key <> '' then
    folder := folder + '\' + key;
  try
    if reg.OpenKey(folder, false) then
    begin
      if reg.ValueExists( valueName ) then
        result := reg.ReadInteger( valueName );
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;



{***
  Safely read a given valueName from the registry default-folder REGPATH 
  @param string Name of the value
  @param boolean Default-value to return if valueName was not found
  @param string Subkey of REGPATH where to search for the value
}
function TMainForm.GetRegValue( valueName: String; defaultValue: Boolean; key: String = '' ) : Boolean;
var
  reg : TRegistry;
  folder : String;
begin
  result := defaultValue;
  reg := TRegistry.Create;
  folder := REGPATH;
  if key <> '' then
    folder := folder + '\' + key;
  try
    if reg.OpenKey(folder, false) then
    begin
      if reg.ValueExists( valueName ) then
        result := reg.ReadBool( valueName );
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;



{***
  Saves a number-value into the predefined folder REGPATH
  @param string Name for the value
  @param integer Value-number
  @param string Subkey of REGPATH where to store the value
}
procedure TMainForm.SaveRegValue( valueName: String; value: Integer; key: String = '' );
var
  reg : TRegistry;
  folder : String;
begin
  reg := TRegistry.Create;
  folder := REGPATH;
  if key <> '' then
    folder := folder + '\' + key;
  try
    if reg.OpenKey(folder, false) then
    begin
      reg.WriteInteger( valueName, value );
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;



{***
  Saves a boolean-value into the predefined folder REGPATH
  @param string Name for the value
  @param boolean Value
  @param string Subkey of REGPATH where to store the value
}
procedure TMainForm.SaveRegValue( valueName: String; value: Boolean; key: String = '' );
var
  reg : TRegistry;
  folder : String;
begin
  reg := TRegistry.Create;
  folder := REGPATH;
  if key <> '' then
    folder := folder + '\' + key;
  try
    if reg.OpenKey(folder, false) then
    begin
      reg.WriteBool( valueName, value );
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;



{***
  Return active childwin if any
  @return TMDIChild
}
function TMainForm.GetChildwin: TMDIChild;
begin
  result := TMDIChild( ActiveMDIChild );
end;


end.
