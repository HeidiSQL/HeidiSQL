unit Main;


// -------------------------------------
// Main-window
// -------------------------------------

{$I ../../components/compilerdetection/compilers.inc}

interface

uses
  Synchronization,
  Communication,
  Windows, SysUtils, Classes, Graphics, Forms, Controls, Menus,
  StdCtrls, Dialogs, Buttons, Messages, ExtCtrls, ComCtrls, StdActns,
  ActnList, ImgList, Registry, ShellApi, ToolWin, Clipbrd, db, DBCtrls,
  SynMemo, synedit, SynEditTypes, smdbgrid, ZDataSet, ZSqlProcessor,
  HeidiComp, sqlhelp, MysqlQueryThread, Childwin;

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
    SQLFunctions: TPopupMenu;
    MenuRun: TMenuItem;
    MenuRunSelection: TMenuItem;
    N10: TMenuItem;
    menuclear: TMenuItem;
    EditUndo1: TEditUndo;
    ToolButton14: TToolButton;
    ExecuteQuery: TAction;
    ExecuteSelection: TAction;
    MenuSetFilter: TMenuItem;
    menucopy: TMenuItem;
    N12: TMenuItem;
    menupaste: TMenuItem;
    menuload: TMenuItem;
    menusave: TMenuItem;
    MenuFind: TMenuItem;
    SaveDialog2: TSaveDialog;
    ExportSettings1: TMenuItem;
    Importsettings1: TMenuItem;
    OpenDialog2: TOpenDialog;
    menuSupportForum: TMenuItem;
    Copy2XML: TAction;
    ExportData: TAction;
    Exportdata1: TMenuItem;
    CopyasXMLdata1: TMenuItem;
    MenuReplace: TMenuItem;
    ExecuteLine: TAction;
    MenuRunLine: TMenuItem;
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
    procedure MenuReplaceClick(Sender: TObject);
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
    procedure ButtonAdvancedPropertiesClick(Sender: TObject);
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
    procedure insertFunction(sender: TObject);
    procedure menuclearClick(Sender: TObject);
    procedure SQLFunctionsPopup(Sender: TObject);
    procedure MenuSetFilterClick(Sender: TObject);
    procedure OpenURL(Sender: TObject);
    function mask(str: String) : String;
    procedure ExportSettings1Click(Sender: TObject);
    procedure Importsettings1Click(Sender: TObject);
    procedure ExecuteQueryExecute(Sender: TObject);
    procedure ExecuteSelectionExecute(Sender: TObject);
    procedure MenuFindClick(Sender: TObject);
    procedure Save2XMLExecute(Sender: TObject);
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
  private
    function GetChildwin: TMDIChild;
  public
    logsqlnum                  : Integer;
    CSVSeparator               : String[10];
    CSVEncloser                : String[10];
    CSVTerminator              : String[10];
    ConvertHTMLEntities        : Boolean;
    DefaultColWidth            : Integer;
    NativeFieldTypes           : Boolean;
    LanguageOffset             : Integer;
    DataNullBackground         : TColor;
    SQLFunctionNames,
    SQLFunctionDeclarations,
    SQLFunctionDescriptions    : TStringList;
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
  DIRNAME_SNIPPETS    : String;

const
  discname = 'not connected';
  // TODO: Wait for Sven's better icons to represent server connections here.
  ICON_MYSELF_CONNECTED = 89;
  ICON_MYSELF_DISCONNECTED = 90;
  ICON_OTHER_CONNECTED = 91;
  ICON_OTHER_DISCONNECTED = 92;

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
  Threading;

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
  mi : TMenuItem;
  f : TextFile;
  FunctionLine,
  FunctionName,
  FunctionDeclaration,
  FunctionDescription : String;
  i, pipeposition : Integer;
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
      CSVSeparator := ',';
      CSVEncloser := '';
      CSVTerminator := '\r\n';
      ConvertHTMLEntities := true;
      logsqlnum := 300;
      if Valueexists('CSVSeparator') then
        CSVSeparator := ReadString('CSVSeparator');
      if Valueexists('CSVEncloser') then
        CSVEncloser := ReadString('CSVEncloser');
      if Valueexists('CSVTerminator') then
        CSVTerminator := ReadString('CSVTerminator');
      if Valueexists('ConvertHTMLEntities') then
        ConvertHTMLEntities := ReadBool('ConvertHTMLEntities');
      if valueExists('logsqlnum') then
        logsqlnum := ReadInteger('logsqlnum');
      if valueExists('NativeFieldTypes') then
        NativeFieldTypes := ReadBool('NativeFieldTypes')
      else
        NativeFieldTypes := false;

      // Position of Toolbars
      if valueExists('ToolBar2Left') then
        ToolBarStandard.Left := ReadInteger('ToolBar2Left');
      if valueExists('ToolBarDataLeft') then
        ToolBarData.Left := ReadInteger('ToolBarDataLeft');
      if valueExists('ToolBar2Top') then
        ToolBarStandard.Top := ReadInteger('ToolBar2Top');
      if valueExists('ToolBarDataTop') then
        ToolBarData.Top := ReadInteger('ToolBarDataTop');

      // Other values
      if valueExists('DataNullBackground') then
        DataNullBackground := StringToColor(ReadString('DataNullBackground'))
      else
        DataNullBackground := clAqua;
    end;
    CloseKey;
  end;

  // read function-list from function.txt:
  if fileexists(ExtractFilePath(paramstr(0)) + 'function.txt') then
  try
    AssignFile(f, ExtractFilePath(paramstr(0)) + 'function.txt');
    Reset(f);
    i := 1;
    SQLFunctionNames := TStringList.Create;
    SQLFunctionDeclarations := TStringList.Create;
    SQLFunctionDescriptions := TStringList.Create;

    while not eof(f) do
    begin
      FunctionName := '';
      FunctionDeclaration := '';
      FunctionDescription := '';
      Readln(f, FunctionLine);

      if (length(FunctionLine) > 0) and (FunctionLine[1] <> '#') then
      begin
        FunctionName := FunctionLine;
        if pos('(', FunctionName) > 0 then
          FunctionName := copy(FunctionName, 0, pos('(', FunctionName)-1);
        FunctionName := trim(FunctionName);

        FunctionDeclaration := Copy(FunctionLine, 0, Pos( ')', FunctionLine ) );
        FunctionDeclaration := Copy(FunctionDeclaration, Pos( '(', FunctionDeclaration ), Length(FunctionDeclaration) );

        pipeposition := LastPos('|', FunctionLine);
        if pipeposition > 0 then // read hint
        begin
          FunctionDescription := copy(FunctionLine, pipeposition+1, length(FunctionLine)-1);
          FunctionDescription := trim(FunctionDescription);
        end;

        mi := TMenuItem.Create(self);
        mi.Caption := FunctionName;
        mi.Hint := FunctionName + FunctionDeclaration;
        if FunctionDescription <> '' then
          mi.Hint := mi.Hint + ' - ' + FunctionDescription;
        mi.Hint := Trim(mi.Hint);
        // Replace pipes as they're used to seperate ShortHint and LongHint 
        mi.Hint := StringReplace( mi.Hint, '|', '�', [rfReplaceAll] );

        if FunctionLine[1] <> ' ' then // build submenu
        begin
          SQLfunctions.Items.add(mi);
          inc(i);
        end else
        begin
          mi.OnClick := insertFunction;
          SQLfunctions.Items[i+11].Add(mi);
          SQLFunctionNames.Add(FunctionName);
          SQLFunctionDeclarations.Add(FunctionDeclaration);
          SQLFunctionDescriptions.Add(FunctionDescription);
        end;
      end;
    end;
  finally
    CloseFile(f);
  end;

  // Beautify appversion
  appversion := StringReplace( appversion, '$Rev', 'Revision', [rfIgnoreCase] );
  appversion := StringReplace( appversion, '$', '', [] );
  appversion := Trim( appversion );

  // Folder which contains snippet-files
  DIRNAME_SNIPPETS := GetShellFolder($0023) + '\' + APPNAME + '\Snippets\'; // CSIDL_COMMON_APPDATA
end;


procedure TMainForm.FormShow(Sender: TObject);
begin
  // Cannot be done in OnCreate because we need ready forms here:
  ShowConnections(self);
end;


procedure TMainForm.insertFunction(sender: TObject);
var
  f : String;
  sm : TSynMemo;
begin
  // insert function from function.txt
  if ChildWin.SynMemoFilter.Focused then
    sm := ChildWin.SynMemoFilter
  else
    sm := ChildWin.SynMemoQuery;
  f := TMenuItem(Sender).Hint;
  f := stringreplace(f, '&', '', [rfReplaceAll]);
  // Restore pipes as they're used to seperate ShortHint and LongHint
  f := StringReplace( f, '�', '|', [rfReplaceAll] );
  f := copy(f, 0, pos(')', f));
  sm.UndoList.AddGroupBreak;
  sm.SelText := f;
  sm.UndoList.AddGroupBreak;
  if not ChildWin.SynMemoFilter.Focused then
    ChildWin.SynMemoQueryChange(self);
end;



procedure TMainForm.ButtonRefreshClick(Sender: TObject);
begin
  // Refresh
  if ChildWin.PageControlMain.ActivePage = ChildWin.tabHost then
    ChildWin.ShowVariablesAndProcesses(self)
  else if ChildWin.PageControlMain.ActivePage = ChildWin.tabDatabase then begin
    ChildWin.RefreshActiveDbTableList;
    ChildWin.ShowDBProperties(self)
  end else if ChildWin.PageControlMain.ActivePage = ChildWin.tabTable then
    ChildWin.ShowTableProperties(self)
  else if ChildWin.PageControlMain.ActivePage = ChildWin.tabData then
    ChildWin.viewdata(self)
  else
    ChildWin.ReadDatabasesAndTables(self);
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
  if ChildWin.ActualDatabase <> '' then
    ChildWin.DropDB(self);
end;

procedure TMainForm.btnSQLHelpClick(Sender: TObject);
begin
  // SQL help
  ChildWin.CallSQLHelp( Sender );
end;

procedure TMainForm.ButtonAdvancedPropertiesClick(Sender: TObject);
begin
  // Show advanced table-properties
  tbl_properties_form.showmodal;
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
  loaddataform.showmodal;
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
    dataset2csv(ChildWin.GetVisualDataset(), CSVSeparator, CSVEncloser, CSVTerminator)
  else if ChildWin.PageControlMain.ActivePage = ChildWin.tabQuery then
    dataset2csv(ChildWin.GetVisualDataset(), CSVSeparator, CSVEncloser, CSVTerminator);
end;


procedure TMainForm.CopyHTMLtableExecute(Sender: TObject);
begin
  // Copy data in actual dataset as HTML
  if ChildWin.PageControlMain.ActivePage = ChildWin.tabData then
    dataset2html(ChildWin.GetVisualDataset(), TZQuery(ChildWin.GetVisualDataset()).Sql.Text, '', ConvertHTMLEntities, APPNAME + ' ' + appversion )
  else if ChildWin.PageControlMain.ActivePage = ChildWin.tabQuery then
    dataset2html(ChildWin.GetVisualDataset(), TZReadOnlyQuery(ChildWin.GetVisualDataset()).Sql.Text, '', ConvertHTMLEntities, APPNAME + ' ' + appversion);
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
    printlistform.showmodal;
end;


procedure TMainForm.CopyTableExecute(Sender: TObject);
begin
  // copy table
  CopyTableForm.ShowModal;
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

procedure TMainForm.menuclearClick(Sender: TObject);
var
  memo : TSynMemo;
begin
  // Clear SynMemo
  if ChildWin.SynMemoFilter.Focused then
    memo := ChildWin.SynMemoFilter
  else
    memo := ChildWin.SynMemoQuery;
  // Make sure to add this step to SynMemo's undo history
  memo.SelectAll;
  memo.SelText := '';
  memo.SelStart := 0;
  memo.SelEnd := 0;
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

procedure TMainForm.SQLFunctionsPopup(Sender: TObject);
begin
  // Depending which SynMemo is focused, (de-)activate some menuitems
  // The popupmenu SQLFunctions is used in both Filter- and Query-Memo
  if ChildWin.SynMemoFilter.focused then
  begin
    MenuRun.ShortCut := TextToShortCut('');
    MenuSetFilter.ShortCut := TextToShortCut('F9'); // set Filter with F9
    MenuSetFilter.Visible := true;
    MenuRun.Visible := false;
    MenuRunSelection.Visible := false;
    MenuRunLine.Visible := false;
    MenuCopy.Visible := false;
    MenuPaste.Visible := false;
    MenuLoad.Visible := false;
    MenuSave.Visible := false;
    MenuFind.Visible := false;
    MenuReplace.Visible := false;
  end
  else begin
    MenuRun.ShortCut := TextToShortCut('F9');  // Exec SQL with F9
    MenuSetFilter.ShortCut := TextToShortCut('');
    MenuSetFilter.Visible := false;
    MenuRun.Visible := true;
    MenuRunSelection.Visible := true;
    MenuRunLine.Visible := true;
    MenuCopy.Visible := true;
    MenuPaste.Visible := true;
    MenuLoad.Visible := true;
    MenuSave.Visible := true;
    MenuFind.Visible := true;
    MenuReplace.Visible := true;
  end;

end;

procedure TMainForm.MenuSetFilterClick(Sender: TObject);
begin
  ChildWin.SetFilter(self);
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

procedure TMainForm.MenuFindClick(Sender: TObject);
begin
  ChildWin.btnQueryFindClick(sender);
end;

procedure TMainForm.MenuReplaceClick(Sender: TObject);
begin
  ChildWin.btnQueryReplaceClick(sender);
end;

procedure TMainForm.ExecuteLineExecute(Sender: TObject);
begin
  ChildWin.ExecSqlClick(sender, false, true);
end;

procedure TMainForm.Save2XMLExecute(Sender: TObject);
begin
  // Save data in actual dataset as HTML
  with TSaveDialog.Create(self) do begin
    Filter := 'XML-Files (*.xml)|*.xml|All files (*.*)|*.*';
    DefaultExt := 'xml';
    if ChildWin.PageControlMain.ActivePage = ChildWin.tabData then
      FileName := ChildWin.ActualTable
    else
      FileName := 'SQL query';
    Options := [ofOverwritePrompt,ofEnableSizing];

    if Execute and (FileName <> '') then begin
      if ChildWin.PageControlMain.ActivePage = ChildWin.tabData then
        dataset2xml(ChildWin.GetVisualDataset(), ChildWin.ActualTable, FileName)
      else
        dataset2xml(ChildWin.GetVisualDataset(), 'SQL query', FileName);
    end;
  end;
end;

procedure TMainForm.Copy2XMLExecute(Sender: TObject);
begin
  // Copy data in actual dataset as XML
  if ChildWin.PageControlMain.ActivePage = ChildWin.tabData then
    dataset2xml(ChildWin.GetVisualDataset(), ChildWin.ActualTable)
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
    3 : begin query := ChildWin.GetVisualDataset(){ZQuery2}; ChildWin.SaveDialogExportData.Filename := ChildWin.ActualTable; end;
    4 : begin query := ChildWin.GetVisualDataset() {ZQuery1}; ChildWin.SaveDialogExportData.Filename := 'SQL-query'; end;
    else
      raise Exception.Create('Internal error: Cannot fetch query with no related active tab.');
  end;

  with ChildWin.SaveDialogExportData do
  begin
    Title := 'Export result-set from '+Filename+'...';
    FieldSep := CSVSeparator;
    LineSep := CSVTerminator;
    FieldEncl := CSVEncloser;
    ConvertHTMLSpecialChars := ConvertHTMLEntities;


    if Execute and (FileName <> '') then
    begin
      Screen.Cursor := crHourGlass;
      case FilterIndex of
        1 : dataset2csv(query, FieldSep, FieldEncl, LineSep, Filename);
        2 : dataset2html(query, FileName, FileName, ConvertHTMLSpecialChars, APPNAME+' '+appversion);
        3 : dataset2xml(query, FileName, FileName);
      end;
      CSVSeparator := FieldSep;
      CSVTerminator := LineSep;
      CSVEncloser := FieldEncl;
      ConvertHTMLEntities := ConvertHTMLSpecialChars;
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
  i,j : Integer;
  tn, tndb : TTreeNode;
  t : TStringList;
begin
  with ChildWin do
  begin
    t := TStringlist.Create;

    if (Sender as TBasicAction).ActionComponent = PopupMenuDropTable then begin
      // Invoked by tree menu popup.
      t.add(mask(DBRightClickSelectedItem.Parent.text) + '.' + mask(DBRightClickSelectedItem.text));
    end else if PageControlMain.ActivePage = tabDatabase then with ListTables do begin
      // Invoked from one of the various buttons, SheetDatabase is the active page, drop highlighted table(s).
      for i:=0 to Items.count-1 do if Items[i].Selected then t.add(mask(Items[i].Caption));
    end else begin
      // Invoked from one of the various buttons, drop table selected in tree view.
      t.add(mask(ActualDatabase) + '.' + mask(ActualTable));
    end;

    // Fix actions temporarily enabled for popup menu.
    ValidateControls;

    if MessageDlg('Drop ' + inttostr(t.count) + ' Table(s) ?' + crlf + '(' + implodestr(', ', t) + ')', mtConfirmation, [mbok,mbcancel], 0) <> mrok then
      exit;

    Screen.Cursor := crSQLWait;
    ExecUpdateQuery( 'DROP TABLE ' + implodestr(', ', t) );

    if DBtree.Selected.Level in [1, 2] then
    begin
      tndb := nil;
      if DBTree.Selected.Level = 1 then
        tndb := DBTree.Selected
      else if DBTree.Selected.Level = 2 then
        tndb := DBTree.Selected.Parent;

      for i:=0 to t.count-1 do
      begin
        // delete table in dbtree
        tn := tndb.getFirstChild;
        for j:=0 to tndb.Count -1 do
        begin
          if t[i] = tn.Text then
          begin
            tn.Delete;
            break;
          end;
          tn := tndb.GetNextChild(tn);
        end;
        // delete in SynEditHighlighter's tablenames
        j := SynSQLSyn1.TableNames.IndexOf( t[i] );
        if j > -1 then
          SynSQLSyn1.TableNames.Delete( j );
      end;
    end;

    ShowDBProperties(self);
    Screen.Cursor := crDefault;
  end;
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
