program heidisql;

uses
  Forms,
  SysUtils,
  Dialogs,
  main in '..\..\source\main.pas' {MainForm},
  about in '..\..\source\about.pas' {AboutBox},
  connections in '..\..\source\connections.pas' {connform},
  exportsql in '..\..\source\exportsql.pas' {ExportSQLForm},
  loaddata in '..\..\source\loaddata.pas' {loaddataform},
  usermanager in '..\..\source\usermanager.pas' {UserManagerForm},
  options in '..\..\source\options.pas' {optionsform},
  optimizetables in '..\..\source\optimizetables.pas' {optimize},
  printlist in '..\..\source\printlist.pas' {printlistForm},
  copytable in '..\..\source\copytable.pas' {CopyTableForm},
  insertfiles in '..\..\source\insertfiles.pas' {frmInsertFiles},
  insertfiles_progress in '..\..\source\insertfiles_progress.pas' {frmInsertFilesProgress},
  helpers in '..\..\source\helpers.pas',
  synchronization in '..\..\source\synchronization.pas',
  communication in '..\..\source\communication.pas',
  threading in '..\..\source\threading.pas',
  sqlhelp in '..\..\source\sqlhelp.pas' {frmSQLhelp},
  queryprogress in '..\..\source\queryprogress.pas' {frmQueryProgress},
  mysqlquery in '..\..\source\mysqlquery.pas',
  mysqlquerythread in '..\..\source\mysqlquerythread.pas',
  mysqlconn in '..\..\source\mysqlconn.pas',
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
  dataviewsave in '..\..\source\dataviewsave.pas' {FrmDataViewSave},
  routine_editor in '..\..\source\routine_editor.pas' {frmRoutineEditor},
  table_editor in '..\..\source\table_editor.pas' {frmTableEditor};

{$R ..\..\res\icon.RES}
{$R ..\..\res\version.RES}
{$R ..\..\res\manifest.RES}

begin
  debug('perf: All modules loaded.');
  Application.Initialize;
  Application.Title := APPNAME;
  Application.UpdateFormatSettings := False;
  Application.CreateForm(TMainForm, MainForm);
  debug('perf: Main created.');

  try
    try
      InitializeSync(MainForm.Handle);
      SetWindowName(main.discname);
      InitializeThreading(MainForm.Handle);
      InitializeComm(
        MainForm.Handle,
        MainForm.ExecuteRemoteNonQuery,
        MainForm.ExecuteRemoteQuery
      );
      debug('perf: Running.');
      MainForm.Startup;
      Application.Run;
    finally
      DeInitializeSync;
    end;
  except
    on e: Exception do begin
      ShowMessage(e.ClassName + ': ' + e.Message);
    end;
  end;
 end.
