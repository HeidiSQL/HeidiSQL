program heidisql;

{%ToDo 'heidisql.todo'}
{%File '..\..\source\const.inc'}

uses
  Forms,
  SysUtils,
  Dialogs,
  MAIN in '..\..\source\MAIN.PAS' {MainForm},
  childwin in '..\..\source\childwin.pas' {MDIChild},
  about in '..\..\source\about.pas' {AboutBox},
  connections in '..\..\source\connections.pas' {connform},
  createtable in '..\..\source\createtable.pas' {CreateTableForm},
  fieldeditor in '..\..\source\fieldeditor.pas' {FieldEditForm},
  exportsql in '..\..\source\exportsql.pas' {ExportSQLForm},
  tbl_properties in '..\..\source\tbl_properties.pas' {tbl_properties_form},
  tblcomment in '..\..\source\tblcomment.pas' {tablecomment},
  loaddata in '..\..\source\loaddata.pas' {loaddataform},
  usermanager in '..\..\source\usermanager.pas' {UserManagerForm},
  options in '..\..\source\options.pas' {optionsform},
  optimizetables in '..\..\source\optimizetables.pas' {optimize},
  printlist in '..\..\source\printlist.pas' {printlistForm},
  copytable in '..\..\source\copytable.pas' {CopyTableForm},
  edituser in '..\..\source\edituser.pas' {FormEditUser},
  insertfiles in '..\..\source\insertfiles.pas' {frmInsertFiles},
  insertfiles_progress in '..\..\source\insertfiles_progress.pas' {frmInsertFilesProgress},
  helpers in '..\..\source\helpers.pas',
  synchronization in '..\..\source\synchronization.pas',
  communication in '..\..\source\communication.pas',
  threading in '..\..\source\threading.pas',
  sqlhelp in '..\..\source\sqlhelp.pas' {frmSQLhelp},
  queryprogress in '..\..\source\queryprogress.pas' {frmQueryProgress},
  MysqlQuery in '..\..\source\MysqlQuery.pas',
  MysqlQueryThread in '..\..\source\MysqlQueryThread.pas',
  MysqlConn in '..\..\source\MysqlConn.pas',
  mysql in '..\..\source\mysql.pas',
  column_selection in '..\..\source\column_selection.pas' {ColumnSelectionForm};

{$R *.RES}

begin
  debug('perf: All modules loaded.');
  Application.Initialize;
  Application.Title := APPNAME;
  Application.CreateForm(TMainForm, MainForm); debug('perf: Main created.');

  Application.CreateForm(TCopyTableForm, CopyTableForm); debug('perf: CopyTable created.');
  Application.CreateForm(TFormEditUser, FormEditUser); debug('perf: EditUser created.');
  Application.CreateForm(TfrmSQLhelp, frmSQLhelp); debug('perf: frmSQLhelp created.');


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
