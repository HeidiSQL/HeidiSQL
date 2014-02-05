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
  createdatabase in '..\..\source\createdatabase.pas' {CreateDatabaseForm},
  updatecheck in '..\..\source\updatecheck.pas' {frmUpdateCheck},
  editvar in '..\..\source\editvar.pas' {frmEditVariable},
  view in '..\..\source\view.pas' {frmView},
  selectdbobject in '..\..\source\selectdbobject.pas' {frmSelectDBObject},
  texteditor in '..\..\source\texteditor.pas' {frmTextEditor},
  bineditor in '..\..\source\bineditor.pas' {frmBinEditor},
  grideditlinks in '..\..\source\grideditlinks.pas',
  routine_editor in '..\..\source\routine_editor.pas' {frmRoutineEditor},
  table_editor in '..\..\source\table_editor.pas' {frmTableEditor},
  dbconnection in '..\..\source\dbconnection.pas',
  trigger_editor in '..\..\source\trigger_editor.pas' {frmTriggerEditor: TFrame},
  searchreplace in '..\..\source\searchreplace.pas' {frmSearchReplace},
  event_editor in '..\..\source\event_editor.pas' {frmEventEditor: TFrame},
  loginform in '..\..\source\loginform.pas' {frmLogin},
  Cromis.DirectoryWatch in '..\..\source\Cromis.DirectoryWatch.pas',
  exportgrid in '..\..\source\exportgrid.pas' {frmExportGrid},
  syncdb in '..\..\source\syncdb.pas' {frmSyncDB},
  gnugettext in '..\..\source\gnugettext.pas',
  JumpList in '..\..\source\JumpList.pas',
	extra_controls in '..\..\source\extra_controls.pas';

{$R ..\..\res\icon.RES}
{$R ..\..\res\version.RES}
{$R ..\..\res\manifest.RES}
{$R ..\..\res\updater.RES}

var
  AppLanguage: String;
begin
  // Use MySQL standard format for date/time variables: YYYY-MM-DD HH:MM:SS
  // Be aware that Delphi internally converts the slashes in ShortDateFormat to the DateSeparator
  FormatSettings.DateSeparator := '-';
  FormatSettings.TimeSeparator := ':';
  FormatSettings.ShortDateFormat := 'yyyy/mm/dd';
  FormatSettings.LongTimeFormat := 'hh:nn:ss';

  AppSettings := TAppSettings.Create;
  SecondInstMsgId := RegisterWindowMessage(APPNAME);
  if (not AppSettings.ReadBool(asAllowMultipleInstances)) and CheckForSecondInstance then begin
    AppSettings.Free;
    Application.Terminate;
  end else begin
    AppLanguage := AppSettings.ReadString(asAppLanguage);
    UseLanguage(AppLanguage);
    Application.Initialize;
    Application.Title := APPNAME;
    Application.UpdateFormatSettings := False;
    Application.CreateForm(TMainForm, MainForm);
    MainForm.AfterFormCreate;
    Application.OnDeactivate := MainForm.ApplicationDeActivate;
    Application.MainFormOnTaskBar := True;
    Application.Run;
  end;
 end.
