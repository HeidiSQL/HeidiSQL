program heidisql;

uses
  madExcept,
  Forms,
  SysUtils,
  Dialogs,
  Windows,
  main in '..\..\source\main.pas' {MainForm},
  about in '..\..\source\about.pas' {AboutBox},
  connections in '..\..\source\connections.pas' {connform},
  loaddata in '..\..\source\loaddata.pas' {loaddataform},
  usermanager in '..\..\source\usermanager.pas' {UserManagerForm},
  options in '..\..\source\options.pas' {optionsform},
  tabletools in '..\..\source\tabletools.pas' {frmTableTools},
  printlist in '..\..\source\printlist.pas' {printlistForm},
  copytable in '..\..\source\copytable.pas' {CopyTableForm},
  insertfiles in '..\..\source\insertfiles.pas' {frmInsertFiles},
  helpers in '..\..\source\helpers.pas',
  sqlhelp in '..\..\source\sqlhelp.pas' {frmSQLhelp},
  mysql_structures in '..\..\source\mysql_structures.pas',
  column_selection in '..\..\source\column_selection.pas' {ColumnSelectionForm},
  data_sorting in '..\..\source\data_sorting.pas' {DataSortingForm},
  runsqlfile in '..\..\source\runsqlfile.pas' {RunSQLFileForm},
  createdatabase in '..\..\source\createdatabase.pas' {CreateDatabaseForm},
  updatecheck in '..\..\source\updatecheck.pas' {frmUpdateCheck},
  editvar in '..\..\source\editvar.pas' {frmEditVariable},
  view in '..\..\source\view.pas' {frmView},
  selectdbobject in '..\..\source\selectdbobject.pas' {frmSelectDBObject},
  texteditor in '..\..\source\texteditor.pas' {frmTextEditor},
  bineditor in '..\..\source\bineditor.pas' {frmBinEditor},
  grideditlinks in '..\..\source\grideditlinks.pas',
  uVistaFuncs in '..\..\source\uVistaFuncs.pas',
  routine_editor in '..\..\source\routine_editor.pas' {frmRoutineEditor},
  table_editor in '..\..\source\table_editor.pas' {frmTableEditor},
  mysql_api in '..\..\source\mysql_api.pas',
  mysql_connection in '..\..\source\mysql_connection.pas',
  trigger_editor in '..\..\source\trigger_editor.pas' {frmTriggerEditor: TFrame},
  searchreplace in '..\..\source\searchreplace.pas' {frmSearchReplace},
  event_editor in '..\..\source\event_editor.pas' {frmEventEditor: TFrame},
  loginform in '..\..\source\loginform.pas' {frmLogin},
  Cromis.DirectoryWatch in '..\..\source\Cromis.DirectoryWatch.pas';

{$R ..\..\res\icon.RES}
{$R ..\..\res\version.RES}
{$R ..\..\res\manifest.RES}
{$R ..\..\res\updater.RES}

var
  DoStop, prefAllowMultipleInstances: Boolean;
begin
  debug('perf: All modules loaded.');

  prefAllowMultipleInstances := GetRegValue(REGNAME_MULTI_INSTANCES, DEFAULT_MULTI_INSTANCES);
  SecondInstMsgId := RegisterWindowMessage(APPNAME);
  DoStop := False;
  if not prefAllowMultipleInstances then
    DoStop := CheckForSecondInstance;
  if DoStop then
    Application.Terminate
  else begin
    Application.Initialize;
    Application.Title := APPNAME;
    Application.UpdateFormatSettings := False;
    Application.CreateForm(TMainForm, MainForm);
    Application.OnMessage := Mainform.OnMessageHandler;
    debug('perf: Main created.');
    MainForm.Startup;
    Application.Run;
  end;
 end.
