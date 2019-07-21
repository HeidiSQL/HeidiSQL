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
  apphelpers in '..\..\source\apphelpers.pas',
  sqlhelp in '..\..\source\sqlhelp.pas' {frmSQLhelp},
  dbstructures in '..\..\source\dbstructures.pas',
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
  extra_controls in '..\..\source\extra_controls.pas',
  change_password in '..\..\source\change_password.pas' {frmPasswordChange},
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Graphics,
  theme_preview in '..\..\source\theme_preview.pas' {frmThemePreview};

{.$R *.RES}
{$R ..\..\res\icon.RES}
{$R ..\..\res\icon-question.RES}
{$R ..\..\res\version.RES}
{$R ..\..\res\manifest.RES}
{$R ..\..\res\updater.RES}
{$R ..\..\res\styles.RES}

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
    // SysLanguage may be zh_CN, while we don't offer such a language, but anyway, this is just the current system language:
    SysLanguage := DefaultInstance.GetCurrentLocaleName;
    UseLanguage(AppLanguage);
    // First time translation via dxgettext.
    // Issue #3064: Ignore TFont, so "Default" on mainform for WinXP users does not get broken.
    TP_GlobalIgnoreClass(TFont);

    Application.Initialize;
    Application.Title := APPNAME;
    Application.UpdateFormatSettings := False;
    TStyleManager.TrySetStyle(AppSettings.ReadString(asTheme));
    Application.CreateForm(TMainForm, MainForm);
    MainForm.AfterFormCreate;
    Application.OnDeactivate := MainForm.ApplicationDeActivate;
    Application.OnShowHint := MainForm.ApplicationShowHint;
    Application.MainFormOnTaskBar := True;
    Application.Run;
  end;
 end.
