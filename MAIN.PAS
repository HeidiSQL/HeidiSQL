unit Main;


// -------------------------------------
// HeidiSQL
// Main-window
// -------------------------------------

{$I ../../components/compilerdetection/compilers.inc}

interface

uses
{$ifndef COMPILER_7_UP}
  ThemeMgr, ThemeMgrDB,
{$endif}
  Windows, SysUtils, Classes, Graphics, Forms, Controls, Menus,
  StdCtrls, Dialogs, Buttons, Messages, ExtCtrls, ComCtrls, StdActns,
  ActnList, ImgList, Registry, ShellApi, ToolWin, Clipbrd, db, DBCtrls,
  SynMemo, synedit, smdbgrid, ZDataSet;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    FileNewItem: TMenuItem;
    FileCloseItem: TMenuItem;
    Window1: TMenuItem;
    Help1: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    WindowCascadeItem: TMenuItem;
    WindowTileItem: TMenuItem;
    WindowArrangeItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    Edit1: TMenuItem;
    CopyItem: TMenuItem;
    PasteItem: TMenuItem;
    WindowMinimizeItem: TMenuItem;
    StatusBar: TStatusBar;
    ActionList1: TActionList;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    FileNew1: TAction;
    FileExit1: TAction;
    WindowCascade1: TWindowCascade;
    WindowTileHorizontal1: TWindowTileHorizontal;
    WindowArrangeAll1: TWindowArrange;
    WindowMinimizeAll1: TWindowMinimizeAll;
    FileClose1: TWindowClose;
    WindowTileVertical1: TWindowTileVertical;
    WindowTileItem2: TMenuItem;
    ImageList1: TImageList;
    Extra1: TMenuItem;
    FlushUserPrivileges1: TMenuItem;
    N2: TMenuItem;
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
    Timer1: TTimer;
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
    ImageList2: TImageList;
    Readme1: TMenuItem;
    N8: TMenuItem;
    UserManager: TAction;
    ShowAboutBox: TAction;
    Diagnostics: TAction;
    OptimizeTables1: TMenuItem;
    ImExport1: TMenuItem;
    CopyContentsasHTMLTable1: TMenuItem;
    CopyHTMLtable: TAction;
    Copy2CSV: TAction;
    menuOnTheWeb: TMenuItem;
    N9: TMenuItem;
    N11: TMenuItem;
    PrintList: TAction;
    CopyTable: TAction;
    ControlBar1: TControlBar;
    ToolBar2: TToolBar;
    ToolButton9: TToolButton;
    FileCloseItem2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton12: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
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
    ToolButton15: TToolButton;
    ToolBarData: TToolBar;
    DBNavigator1: TDBNavigator;
    PanelLimit: TPanel;
    CheckBoxLimit: TCheckBox;
    EditLimitStart: TEdit;
    EditLimitEnd: TEdit;
    ButtonOK: TButton;
    UpDownLimitStart: TUpDown;
    UpDownLimitEnd: TUpDown;
    SaveDialog1: TSaveDialog;
    SQLFunctions: TPopupMenu;
    MenuRun: TMenuItem;
    MenuRunSelection: TMenuItem;
    N10: TMenuItem;
    menuclear: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupMenu6: TPopupMenu;
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
    FindDialog1: TFindDialog;
    SaveDialog2: TSaveDialog;
    ExportSettings1: TMenuItem;
    Importsettings1: TMenuItem;
    OpenDialog2: TOpenDialog;
    menuForum: TMenuItem;
    Copy2XML: TAction;
    ExportData: TAction;
    Exportdata1: TMenuItem;
    CopyasXMLdata1: TMenuItem;
    SearchReplace: TAction;
    ReplaceDialog1: TReplaceDialog;
    Searchandreplace1: TMenuItem;
    ManualCopy: TAction;
    ExecuteLine: TAction;
    ExecuteLine1: TMenuItem;
    HTMLview: TAction;
    InsertFiles: TAction;
    InsertfilesintoBLOBfields1: TMenuItem;
    ExportTables: TAction;
{$ifndef COMPILER_7_UP}
    ThemeManager1: TThemeManager;
{$endif}
    DataSearch: TAction;
    procedure ShowConnections(Sender: TObject);
    procedure FileExit1Execute(Sender: TObject);
    procedure connect(Sender: TObject);
    procedure FlushClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ButtonRefreshClick(Sender: TObject);
    procedure ButtonCreateDatabaseClick(Sender: TObject);
    procedure ButtonCreateTableClick(Sender: TObject);
    procedure ButtonDropDatabaseClick(Sender: TObject);
    procedure ButtonDropTableClick(Sender: TObject);
    procedure ButtonAdvancedPropertiesClick(Sender: TObject);
    procedure ResetWindowOptions1Click(Sender: TObject);
    procedure ButtonImportTextfileClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure MenuPreferencesClick(Sender: TObject);
    procedure Readme1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure UserManagerExecute(Sender: TObject);
    procedure ShowAboutBoxExecute(Sender: TObject);
    procedure DiagnosticsExecute(Sender: TObject);
    procedure CopyHTMLtableExecute(Sender: TObject);
    procedure Copy2CSVExecute(Sender: TObject);
    procedure Save2CSVExecute(Sender: TObject);
    procedure SaveHTMLTableExecute(Sender: TObject);
    procedure PrintListExecute(Sender: TObject);
    procedure CopyTableExecute(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);
    procedure showstatus(msg: string='';  panel : Integer=0;  Icon: Integer=50);
    procedure ButtonOKClick(Sender: TObject);
    procedure CheckBoxLimitClick(Sender: TObject);
    procedure LimitPanelEnter(Sender: TObject);
    procedure LimitPanelExit(Sender: TObject);
    procedure ButtonSaveSQLClick(Sender: TObject);
    procedure ButtonLoadSQLFile(Sender: TObject);
    procedure insertFunction(sender: TObject);
    procedure menuclearClick(Sender: TObject);
    procedure SQLFunctionsPopup(Sender: TObject);
    procedure MenuSetFilterClick(Sender: TObject);
    procedure OpenURL(Sender: TObject);
    function mask(str: String) : String;
    procedure FindDialog1Find(Sender: TObject);
    procedure ExportSettings1Click(Sender: TObject);
    procedure Importsettings1Click(Sender: TObject);
    procedure LoadSQLFile(sender: TObject);
    procedure ExecuteQueryExecute(Sender: TObject);
    procedure ExecuteSelectionExecute(Sender: TObject);
    procedure MenuFindClick(Sender: TObject);
    procedure Save2XMLExecute(Sender: TObject);
    procedure Copy2XMLExecute(Sender: TObject);
    procedure DBNavigator1BeforeAction(Sender: TObject;
      Button: TNavigateBtn);
    procedure ExportDataExecute(Sender: TObject);
    procedure SearchReplaceExecute(Sender: TObject);
    procedure ReplaceDialog1Replace(Sender: TObject);
    procedure ReplaceDialog1Find(Sender: TObject);
    procedure ImportWizardExecute(Sender: TObject);
    procedure ManualCopyExecute(Sender: TObject);
    procedure ExecuteLineExecute(Sender: TObject);
    procedure HTMLviewExecute(Sender: TObject);
    procedure InsertFilesExecute(Sender: TObject);
    procedure debug( msg : String = '' );
    procedure ExportTablesExecute(Sender: TObject);
    procedure DataSearchExecute(Sender: TObject);

  private
    { Private declarations }
  public
    logsqlnum                  : Integer;
    CSVSeparator, CSVEncloser,
    CSVTerminator              : String[10];
    ConvertHTMLEntities        : Boolean;
    DefaultColWidth            : Integer;
    NativeFieldTypes           : Boolean;
    LanguageOffset             : Integer;
    DataAlwaysEditMode         : Boolean;
    DataNullBackground         : TColor;
    debugfile                  : TextFile;

end;

var
  MainForm: TMainForm;
  highestcon : Integer;                       // Remember last connection
  appstarted : Boolean = false;               // see connections.pas
  StatusText : String = 'Initializing...';
  StatusIconIndex : Integer = 51;
  loadsqlfile : boolean = true;               // load sql-file into query-memo at startup?
  appversion : String = '3.0 RC2 $Rev$';

const
  appname = 'HeidiSQL';
  regpath = 'Software\' + appname;

type TMyKey = record
  Name    : String;
  _type   : String;
  Columns : TStringList;
end;

implementation

uses ChildWin, About, connections, exportsql, tbl_properties, loaddata,
  usermanager, options, optimizetables, helpers,
  printlist, copytable, insertfiles;

{ TODO: Using two copies of the DFM is ugly,
  but it's the only way I can think of.. }
{$ifndef COMPILER_7_UP}
  {$R MAIN.DFM}
{$else}
  {$R MAIN_NOTHEMEMGR.DFM}
{$endif}

procedure TMainForm.ShowConnections(Sender: TObject);
begin
  connform.showmodal;
end;


procedure TMainForm.showstatus(msg: string='';  panel : Integer=0;  Icon: Integer=50);
begin
  // show Message in statusbar
  if panel = 2 then begin
    StatusText := msg;
    StatusIconIndex := Icon;
  end
  else
    StatusBar.Panels[panel].Text := msg;
  StatusBar.Repaint;
end;

procedure TMainForm.FileExit1Execute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.connect(Sender: TObject);
begin
  // MDI-Child erzeugen und Verbindung aufbauen...
  Screen.Cursor := crSQLWait;
  { create a new MDI child window }
  TMDIChild.Create(Application);
  Screen.Cursor := crDefault;
end;

procedure TMainForm.FlushClick(Sender: TObject);
var
  flushwhat : String;
begin
  if sender is TMenuItem then
    flushwhat := (sender as TMenuItem).Caption
  else if sender is TToolButton then
    flushwhat := 'PRIVILEGES';
  delete(flushwhat, pos('&', flushwhat), 1);
  TMDIChild(Application.Mainform.ActiveMDIChild).ExecQuery('FLUSH ' + flushwhat);
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
    if OpenKey(regpath, true) then
    begin
      WriteString('windowstate', ws);
      WriteInteger('windowleft', left);
      WriteInteger('windowtop', top);
      WriteInteger('windowwidth', width);
      WriteInteger('windowheight', height);
      // Position of Toolbars
      WriteInteger('ToolBar2Left', ToolBar2.Left);
      WriteInteger('ToolBarDataLeft', ToolBarData.Left);
      WriteInteger('ToolBar2Top', ToolBar2.Top);
      WriteInteger('ToolBarDataTop', ToolBarData.Top);
      // Limit-options
      WriteBool('DataLimit', CheckBoxLimit.Checked);
      WriteInteger('DataLimitStart', UpDownLimitStart.Position);
      WriteInteger('DataLimitEnd', UpDownLimitEnd.Position);
    end;
    CloseKey;
  end;
  GetTempPath(MAX_PATH, buffer);
  filename := buffer+'\heidisql-preview.';
  if FileExists(filename+'html') then
    deletefile(filename+'html');
  if FileExists(filename+'jpg') then
    deletefile(filename+'jpg');
  if FileExists(filename+'gif') then
    deletefile(filename+'gif');
  if FileExists(filename+'bmp') then
    deletefile(filename+'bmp');
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  ws : String;
  mi : TMenuItem;
  f : TextFile;
  functionname, functionhint : String;
  i, pipeposition : Integer;
begin
  caption := appname;
  DateSeparator := '-';
  TimeSeparator := ':';
  ShortDateFormat := 'yyyy/mm/dd';
  LongTimeFormat := 'hh:nn:ss';
  DecimalSeparator := '.';

  with TRegistry.Create do
  begin
    if OpenKey(regpath, true) then
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
        ToolBar2.Left := ReadInteger('ToolBar2Left');
      if valueExists('ToolBarDataLeft') then
        ToolBarData.Left := ReadInteger('ToolBarDataLeft');
      if valueExists('ToolBar2Top') then
        ToolBar2.Top := ReadInteger('ToolBar2Top');
      if valueExists('ToolBarDataTop') then
        ToolBarData.Top := ReadInteger('ToolBarDataTop');

      // SQLFiles-History
      i := 1;
      PopUpMenu6.Items.Clear;
      while ValueExists('SQLFile'+inttostr(i)) do begin
        mi := Tmenuitem.Create(self);
        mi.Caption := inttostr(PopUpMenu6.Items.count+1) + ' ' + ReadString('SQLFile'+inttostr(i));
        mi.OnClick := LoadSQLFile;
        PopUpMenu6.Items.Add(mi);
        inc(i);
      end;

      // Limit-options
      if valueExists('DataLimit') then
        CheckBoxLimit.Checked := ReadBool('DataLimit');
      if valueExists('DataLimitStart') then
        UpDownLimitStart.Position := ReadInteger('DataLimitStart');
      if valueExists('DataLimitEnd') then
        UpDownLimitEnd.Position := ReadInteger('DataLimitEnd');

      // Other values
      DataAlwaysEditMode := true;
      if valueExists('DataAlwaysEditMode') then
        DataAlwaysEditMode := ReadBool('DataAlwaysEditMode');
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

    while not eof(f) do
    begin
      functionname := '';
      Readln(f, functionname);
      pipeposition := pos('|', functionname);
      if pipeposition > 0 then // read hint
      begin
        functionhint := copy(functionname, 0, pipeposition-1) + ' - ' + copy(functionname, pipeposition+1, length(functionname)-1);
        functionname := copy(functionname, 0, pos('(', functionname)-1)
      end else
        functionhint := '';
      if (functionname[1] <> '#') and (length(trim(functionname)) > 0) then
      begin
        mi := TMenuItem.Create(self);
        mi.Caption := trim(functionname);
        mi.Hint := trim(functionhint);
        mi.OnClick := insertFunction;
        if functionname[1] <> ' ' then // build submenu
        begin
          SQLfunctions.Items.add(mi);
          inc(i);
        end else
        begin
          SQLfunctions.Items[i+11].OnClick := nil; // deactivate parent Menuitem
          SQLfunctions.Items[i+11].Add(mi);
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

  timer1.Enabled := true;
end;

procedure TMainform.LoadSQLFile(sender: TObject);
begin
  TMDIChild(Mainform.ActiveMDIChild).LoadSQLClick(sender);
end;

procedure TMainForm.insertFunction(sender: TObject);
var
  t1,t2,f : String;
  oldSelStart : Integer;
  sm : TSynMemo;
begin
  // insert function from function.txt
  if TMDIChild(Mainform.ActiveMDIChild).SynMemo3.Focused then
    sm := TMDIChild(Mainform.ActiveMDIChild).SynMemo3
  else
    sm := TMDIChild(Mainform.ActiveMDIChild).SynMemo1;
  f := TMenuItem(Sender).Hint;
  f := stringreplace(f, '&', '', [rfReplaceAll]);
  f := copy(f, 0, pos(')', f));
  oldSelStart := sm.SelStart;
  t1 := copy(sm.Text, 0, sm.SelStart-1);
  t2 := copy(sm.Text, sm.SelStart, Length(sm.Text) - sm.SelStart);
  sm.Text := t1 + f + t2;
  sm.SelStart := oldSelStart + length(f);
  if not TMDIChild(Mainform.ActiveMDIChild).SynMemo3.Focused then
    TMDIChild(Mainform.ActiveMDIChild).SynMemo1Change(self);
end;



procedure TMainForm.ButtonRefreshClick(Sender: TObject);
begin
  // Refresh
  with TMDIChild(Application.Mainform.ActiveMDIChild) do
  begin
    if PageControl1.ActivePage = SheetHost then
      ShowVariablesAndProcesses(self)
    else if PageControl1.ActivePage = SheetDatabase then
      ShowDBProperties(self)
    else if PageControl1.ActivePage = SheetTable then
      ShowTableProperties(self)
    else if PageControl1.ActivePage = SheetData then
      viewdata(self)
    else
      ReadDatabasesAndTables(self);
  end;
end;

procedure TMainForm.ButtonCreateDatabaseClick(Sender: TObject);
begin
  // create database
  TMDIChild(Application.Mainform.ActiveMDIChild).CreateDatabase(self);
end;

procedure TMainForm.ButtonCreateTableClick(Sender: TObject);
begin
  // create table
  TMDIChild(Application.Mainform.ActiveMDIChild).CreateTable(self);
end;

procedure TMainForm.ButtonDropDatabaseClick(Sender: TObject);
begin
  // drop db
  with TMDIChild(Application.Mainform.ActiveMDIChild) do
    if ActualDatabase <> '' then
      DBLoeschen(self);
end;

procedure TMainForm.ButtonDropTableClick(Sender: TObject);
begin
  // delete table
  with TMDIChild(Application.Mainform.ActiveMDIChild) do
    if ActualTable <> '' then
      TabelleLoeschen(self);
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
    if OpenKey(regpath, false) then
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

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  // Startup!
  timer1.Enabled := false;
  ShowConnections(self);
end;

procedure TMainForm.MenuPreferencesClick(Sender: TObject);
begin
  // Preferences
  optionsform.showmodal;
end;

procedure TMainForm.Readme1Click(Sender: TObject);
begin
  // show readme.txt
  shellexecute(0, 'open', pchar('readme.txt'), Nil, pchar(ExtractFilePath(paramstr(0))), 5);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  StatusBar.Panels[0].Width := width - StatusBar.Panels[1].Width - StatusBar.Panels[2].Width;
end;

procedure TMainForm.UserManagerExecute(Sender: TObject);
begin
  // Usermanager
  usermanagerform.showmodal;
end;

procedure TMainForm.ShowAboutBoxExecute(Sender: TObject);
begin
  // Info-Box
  AboutBox.showmodal;
end;

procedure TMainForm.DiagnosticsExecute(Sender: TObject);
begin
  // optimize / repair... tables
  optimize.showmodal;
end;


procedure TMainForm.Copy2CSVExecute(Sender: TObject);
begin
  // Copy data in actual dataset as CSV
  with TMDIChild(Application.Mainform.ActiveMDIChild) do begin
    if PageControl1.ActivePage = SheetData then
      dataset2csv(ZQuery2, CSVSeparator, CSVEncloser, CSVTerminator)
    else if PageControl1.ActivePage = SheetQuery then
      dataset2csv((ZQuery1 as TZQuery), CSVSeparator, CSVEncloser, CSVTerminator);
  end;
end;


procedure TMainForm.Save2CSVExecute(Sender: TObject);
begin
  // Save data in actual dataset as CSV
  with TMDIChild(Application.Mainform.ActiveMDIChild) do
  with TSaveDialog.Create(self) do begin
//    Separator := CSVSeparator;
//    Encloser := CSVEncloser;
//    Terminator := CSVTerminator;
    Filter := 'CSV-Files (*.csv)|*.csv|Textfiles (*.txt)|*.txt|All files (*.*)|*.*';
    DefaultExt := 'csv';
    if PageControl1.ActivePage = SheetData then
      FileName := ActualTable
    else
      FileName := sstr(ZQuery1.Sql[0], 20);
    Options := [ofOverwritePrompt,ofEnableSizing];

    if Execute and (FileName <> '') then begin
      if PageControl1.ActivePage = SheetData then
        dataset2csv(ZQuery2, CSVSeparator, CSVEncloser, CSVTerminator, Filename)
      else if PageControl1.ActivePage = SheetQuery then
        dataset2csv(ZQuery1, CSVSeparator, CSVEncloser, CSVTerminator, Filename);
    end;
  end;
end;


procedure TMainForm.CopyHTMLtableExecute(Sender: TObject);
begin
  // Copy data in actual dataset as HTML
  with TMDIChild(Application.Mainform.ActiveMDIChild) do begin
    if PageControl1.ActivePage = SheetData then
      dataset2html(ZQuery2, ZConn.HostName + ': ' + ActualDatabase + '.' + ActualTable)
    else if PageControl1.ActivePage = SheetQuery then
      dataset2html(ZQuery1, ZQuery1.Sql[0]);
  end;
end;


procedure TMainForm.SaveHTMLTableExecute(Sender: TObject);
begin
  // Save data in actual dataset as HTML
  with TMDIChild(Application.Mainform.ActiveMDIChild) do begin

    with TSaveDialog.Create(self) do begin
      Filter := 'Hypertext-Files (*.html, *.htm)|*.html;*.htm|All files (*.*)|*.*';
      DefaultExt := 'html';
      if PageControl1.ActivePage = SheetData then
        FileName := ActualTable
      else
        FileName := sstr(ZQuery1.Sql[0], 20);
      Options := [ofOverwritePrompt,ofEnableSizing];

      if Execute and (FileName <> '') then
      begin
        if PageControl1.ActivePage = SheetData then
          dataset2html(ZQuery2, ZConn.HostName + ' / ' + ActualDatabase + ' / ' + ActualTable, FileName)
        else                       
          dataset2html(ZQuery1, ZQuery1.Sql[0], FileName);
      end;
    end;

  end;

end;

procedure TMainForm.PrintListExecute(Sender: TObject);
var
  page : TTabSheet;
begin
  // print
  page := TMDIChild(Mainform.ActiveMDIChild).PageControl1.ActivePage;
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
  TMDIChild(Mainform.ActiveMDIChild).DBTree.SetFocus;
  TMDIChild(Mainform.ActiveMDIChild).viewdata(self);
end;


procedure TMainForm.CheckBoxLimitClick(Sender: TObject);
begin
  // Check/Uncheck popupmenu-item
  if TMDIChild(Mainform.ActiveMDIChild) <> nil then
    TMDIChild(Mainform.ActiveMDIChild).MenuLimit.Checked := CheckBoxLimit.Checked;
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
begin
  // Clear SynMemo
  with TMDIChild(Mainform.ActiveMDIChild) do begin
    if SynMemo3.Focused then
      SynMemo3.Lines.Clear
    else
      SynMemo1.Lines.Clear;
  end;
end;


procedure TMainForm.ButtonSaveSQLClick(Sender: TObject);
var f : TextFile;
begin
  // Save SQL
  if SaveDialog1.Execute then begin
    Screen.Cursor := crHourGlass;
    AssignFile(f, SaveDialog1.FileName);
    Rewrite(f);
    Write(f, TMDIChild(Mainform.ActiveMDIChild).SynMemo1.Text);
    CloseFile(f);
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.ButtonLoadSQLFile(Sender: TObject);
var
  menuitem        : Tmenuitem;
  m,i             : Integer;
  filename        : String;
  dontadd         : Boolean;
begin
  // Load file
  if OpenDialog1.Execute then
  begin
    Screen.Cursor := crHourGlass;
    TMDIChild(ActiveMDIChild).SynMemo1.Lines.LoadFromFile(OpenDialog1.FileName);
    Screen.Cursor := crDefault;
    TMDIChild(ActiveMDIChild).SynMemo1Change(self);

    // don't get one filename more than one time
    dontadd := false;
    for m:=0 to PopUpMenu6.Items.Count-1 do begin
      filename := PopUpMenu6.Items[m].Caption;
      i := 0;
      while filename[i] <> ' ' do
        inc(i);
      filename := copy(filename, i+1, length(filename));
      filename := stringreplace(filename, '&', '', [rfReplaceAll]);
      if filename = OpenDialog1.FileName then
        dontadd := true;
    end;

    if not dontadd then begin
      with TRegistry.Create do begin
        openkey(regpath, true);
        for i:=1 to 20 do
        begin
          if not ValueExists('SQLFile'+inttostr(i)) then
            break;
        end;
        while i > 1 do
        begin
          WriteString('SQLFile'+inttostr(i), ReadString('SQLFile'+inttostr(i-1)));
          dec(i);
        end;
        WriteString('SQLFile1', OpenDialog1.FileName);

        i := 1;
        PopUpMenu6.Items.Clear;
        while ValueExists('SQLFile'+inttostr(i)) do
        begin
          menuitem := Tmenuitem.Create(self);
          menuitem.Caption := inttostr(PopUpMenu6.Items.count+1) + ' ' + ReadString('SQLFile'+inttostr(i));
          menuitem.OnClick := LoadSQLFile;
          PopUpMenu6.Items.Add(menuitem);
          inc(i);
        end;
      end;
    end;

  end;
end;



procedure TMainForm.SQLFunctionsPopup(Sender: TObject);
begin

  if TMDIChild(ActiveMDIChild).SynMemo3.focused then begin // set Filter with F9
    MenuRun.ShortCut := TextToShortCut('');
    MenuSetFilter.ShortCut := TextToShortCut('F9');
    MenuSetFilter.Visible := true;
    MenuRun.Visible := false;
    MenuRunSelection.Visible := false;
    MenuCopy.Visible := false;
    MenuPaste.Visible := false;
    MenuLoad.Visible := false;
    MenuSave.Visible := false;
    MenuFind.Visible := false;
    searchreplace.Visible := false;
  end
  else begin // Exec SQL with F9
    MenuRun.ShortCut := TextToShortCut('F9');
    MenuSetFilter.ShortCut := TextToShortCut('');
    MenuSetFilter.Visible := false;
    MenuRun.Visible := true;
    MenuRunSelection.Visible := true;
    MenuCopy.Visible := true;
    MenuPaste.Visible := true;
    MenuLoad.Visible := true;
    MenuSave.Visible := true;
    MenuFind.Visible := true;
    searchreplace.Visible := true;
  end;

end;

procedure TMainForm.MenuSetFilterClick(Sender: TObject);
begin
  TMDIChild(ActiveMDIChild).SetFilter(self);
end;


procedure TMainForm.OpenURL(Sender: TObject);
var url :  Pchar;
begin
  // open url (hint)
  if sender is TMenuItem then
    url := pchar(TMenuItem(Sender).Hint)
  else
    url := pchar(TControl(Sender).Hint);
  shellexecute(0, 'open', url, Nil, Nil, sw_shownormal);
end;




// mask tablenames, dbs and so on with backtick
function TMainform.mask(str: String) : String;
begin
  if TMDIChild(ActiveMDIChild).mysql_version >= 32300 then
    result := '`' + str + '`'
  else
    result := str;
end;


procedure TMainForm.FindDialog1Find(Sender: TObject);
var foundat: Longint;
begin
  with TMDIChild(ActiveMDIChild).SynMemo1 do begin
    FoundAt := pos(FindDialog1.FindText, copy(Text, SelEnd, Length(Text)));
    if FoundAt > 0 then begin
      SetFocus;
      SelStart := FoundAt + SelEnd -1;
      SelEnd := SelStart + Length(FindDialog1.FindText);
    end else
      messagebeep(0);
  end;
end;

procedure TMainForm.ExportSettings1Click(Sender: TObject);
begin
  // Export settings to .reg-file
  if SaveDialog2.Execute then begin
    if winexec(pchar('regedit.exe /e "'+SaveDialog2.FileName+'" HKEY_CURRENT_USER\'+regpath), SW_SHOW) = ERROR_FILE_NOT_FOUND then
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
  TMDIChild(ActiveMDIChild).ExecSqlClick(sender, false);
end;

procedure TMainForm.ExecuteSelectionExecute(Sender: TObject);
begin
  TMDIChild(ActiveMDIChild).ExecSqlClick(sender, true);
end;

procedure TMainForm.MenuFindClick(Sender: TObject);
begin
  TMDIChild(ActiveMDIChild).ToolButton15Click(sender);
end;

procedure TMainForm.ExecuteLineExecute(Sender: TObject);
begin
  TMDIChild(ActiveMDIChild).ExecSqlClick(sender, false, true);
end;

procedure TMainForm.Save2XMLExecute(Sender: TObject);
begin
  // Save data in actual dataset as HTML
  with TMDIChild(Application.Mainform.ActiveMDIChild) do begin

    with TSaveDialog.Create(self) do begin
      Filter := 'XML-Files (*.xml)|*.xml|All files (*.*)|*.*';
      DefaultExt := 'xml';
      if PageControl1.ActivePage = SheetData then
        FileName := ActualTable
      else
        FileName := sstr(ZQuery1.Sql[0], 20);
      Options := [ofOverwritePrompt,ofEnableSizing];

      if Execute and (FileName <> '') then begin
        if PageControl1.ActivePage = SheetData then
          dataset2xml(ZQuery2, ActualTable, FileName)
        else
          dataset2xml(ZQuery1, 'SQL-query', FileName);
      end;
    end;

  end;
end;

procedure TMainForm.Copy2XMLExecute(Sender: TObject);
begin
  // Copy data in actual dataset as XML
  with TMDIChild(Application.Mainform.ActiveMDIChild) do begin
    if PageControl1.ActivePage = SheetData then
      dataset2xml(ZQuery2, ActualTable)
    else if PageControl1.ActivePage = SheetQuery then
      dataset2xml(ZQuery1, 'SQL-query');
  end;
end;

procedure TMainForm.DBNavigator1BeforeAction(Sender: TObject;
  Button: TNavigateBtn);
begin
  if Button = nbdelete then begin
    TMDIChild(Application.Mainform.ActiveMDIChild).Delete1Click(sender);
    abort;
  end;
end;

procedure TMainForm.ExportDataExecute(Sender: TObject);
var
  query : TZQuery;
  titel : String;
begin
  // Save data in actual dataset as CSV, HTML or XML
  with TMDIChild(Application.Mainform.ActiveMDIChild) do begin
    case PageControl1.ActivePageIndex of
      3 : begin query := ZQuery2; titel := ActualTable; end;
      4 : begin query := ZQuery1; titel := 'SQL-query'; end;
    end;

    with SaveDialogExportData do begin
      Title := 'Export result-set from '+titel+'...';
      FileName := titel;

      if Execute and (FileName <> '') then
      begin
        Screen.Cursor := crHourGlass;
        case FilterIndex of
          1 : dataset2csv(query, CSVSeparator, CSVEncloser, CSVTerminator, Filename);
          2 : dataset2html(query, titel, FileName);
          3 : dataset2xml(query, titel, FileName);
        end;
        Screen.Cursor := crDefault;
      end;
    end;

  end;
end;

procedure TMainForm.SearchReplaceExecute(Sender: TObject);
begin
  ReplaceDialog1.Execute;
end;


{ Replace Text with replace-dialog }
procedure TMainForm.ReplaceDialog1Replace(Sender: TObject);
var
  SelPos: Integer;
  m : TSynMemo;
begin
  m := TMDIChild(Mainform.ActiveMDIChild).synmemo1;
  with TReplaceDialog(Sender) do begin
    if not (frReplaceAll in options) then begin
      { Perform a global case-sensitive search for FindText in Memo1 }
      SelPos := Pos(FindText, m.Lines.Text);
      if SelPos > 0 then
      begin
        m.SetFocus;
        m.SelStart := SelPos;
        m.SelEnd := m.SelStart + Length(FindText);
        { Replace selected text with ReplaceText }
        m.SelText := ReplaceText;
      end
      else
        messagebeep(0);
    end
    else
      m.SearchReplace(FindText, ReplaceText, [ssoReplaceAll, ssoEntireScope]);
  end;
end;

{ Find Text with replace-dialog }
procedure TMainForm.ReplaceDialog1Find(Sender: TObject);
begin
  FindDialog1.FindText := ReplaceDialog1.FindText;
  FindDialog1Find (ReplaceDialog1);
end;

procedure TMainForm.ImportWizardExecute(Sender: TObject);
begin
end;


// copy field-contents to clipboard
procedure TMainForm.ManualCopyExecute(Sender: TObject);
var g : TSMDBGrid;
begin
  with TMDIChild(Mainform.ActiveMDIChild) do begin
    case PageControl1.ActivePageIndex of
    3: g := DBGrid1;
    4: g := DBGrid2;
    else begin messagebeep(MB_ICONASTERISK); exit; end;
    end;
  end;
  if g.datasource.State <> dsInactive then
    clipboard.AsText := g.SelectedField.AsString
  else
    messagebeep(MB_ICONASTERISK);
end;


// view HTML
procedure TMainForm.HTMLviewExecute(Sender: TObject);
var
  g              : TSMDBGrid;
  filename,extension   : String;
  f              : Textfile;
  buffer         : array[0..MAX_PATH] of char;
begin
  with TMDIChild(Mainform.ActiveMDIChild) do begin
    case PageControl1.ActivePageIndex of
    3: g := DBGrid1;
    4: g := DBGrid2;
    else begin messagebeep(MB_ICONASTERISK); exit; end;
    end;
  end;
  if g.datasource.State = dsInactive then begin
    messagebeep(MB_ICONASTERISK);
    exit;
  end;
  Screen.Cursor := crHourGlass;
  showstatus('Saving contents to file...', 2, 51);
  GetTempPath(MAX_PATH, buffer);
  if g.SelectedField.IsBlob and (pos('JFIF', copy(g.SelectedField.AsString, 0, 20)) <> 0) then
    extension := 'jpg'
  else if g.SelectedField.IsBlob and StrCmpBegin('GIF', g.SelectedField.AsString) then
    extension := 'gif'
  else if g.SelectedField.IsBlob and StrCmpBegin('BM', g.SelectedField.AsString) then
    extension := 'bmp'
  else
    extension := 'html';
  filename := buffer+'\heidisql-preview.'+extension;
  AssignFile(f, filename);
  Rewrite(f);
  Write(f, g.SelectedField.AsString);
  CloseFile(f);
  showstatus('Ready', 2);
  Screen.Cursor := crDefault;
  ShellExecute(0, 'open', pchar(filename), nil, nil, SW_SHOWNORMAL);
end;

procedure TMainForm.InsertFilesExecute(Sender: TObject);
begin
  FrmInsertFiles.showmodal;
end;

procedure TMainForm.debug( msg : String = '' );
var
  debugfilename : String;
begin
  debugfilename := ExtractFilePath(paramstr(0)) + 'debug.txt';
  if fileexists(debugfilename) then
  begin
    try
      // MessageDlg(msg, mtInformation, [mbok], 0);
      AssignFile(debugfile, debugfilename);
      Append(debugfile);
      //Reset(debugfile);
      Writeln( debugfile, datetimetostr(date()) + ' ' + timetostr(time()) + ': ' + msg );
      closefile( debugfile );
    except
      raise exception.Create( 'Problem with debug-file: ' + debugfilename );
    end;
  end;
end;




procedure TMainForm.ExportTablesExecute(Sender: TObject);
begin
  // Export SQL
  exportsqlform.showmodal;
end;

procedure TMainForm.DataSearchExecute(Sender: TObject);
begin
  with TMDIChild(Mainform.ActiveMDIChild).EditDataSearch do
  begin
    SetFocus;
    SelectAll;
  end;
end;

end.
