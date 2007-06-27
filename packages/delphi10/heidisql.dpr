program heidisql;

{%ToDo 'heidisql.todo'}

uses
  Forms,
  SysUtils,
  Dialogs,
  MAIN in '..\..\MAIN.PAS' {MainForm},
  childwin in '..\..\childwin.pas' {MDIChild},
  about in '..\..\about.pas' {AboutBox},
  connections in '..\..\connections.pas' {connform},
  createtable in '..\..\createtable.pas' {CreateTableForm},
  fieldeditor in '..\..\fieldeditor.pas' {FieldEditForm},
  exportsql in '..\..\exportsql.pas' {ExportSQLForm},
  tbl_properties in '..\..\tbl_properties.pas' {tbl_properties_form},
  tblcomment in '..\..\tblcomment.pas' {tablecomment},
  loaddata in '..\..\loaddata.pas' {loaddataform},
  usermanager in '..\..\usermanager.pas' {UserManagerForm},
  options in '..\..\options.pas' {optionsform},
  selectsomedatabases in '..\..\selectsomedatabases.pas' {SelectFromManyDatabases},
  optimizetables in '..\..\optimizetables.pas' {optimize},
  printlist in '..\..\printlist.pas' {printlistForm},
  copytable in '..\..\copytable.pas' {CopyTableForm},
  edituser in '..\..\edituser.pas' {FormEditUser},
  insertfiles in '..\..\insertfiles.pas' {frmInsertFiles},
  insertfiles_progress in '..\..\insertfiles_progress.pas' {frmInsertFilesProgress},
  helpers in '..\..\helpers.pas',
  synchronization in '..\..\synchronization.pas',
  communication in '..\..\communication.pas',
  threading in '..\..\threading.pas',
  sqlhelp in '..\..\sqlhelp.pas' {frmSQLhelp},
  queryprogress in '..\..\queryprogress.pas' {frmQueryProgress},
  MysqlQuery in '..\..\MysqlQuery.pas',
  MysqlQueryThread in '..\..\MysqlQueryThread.pas',
  MysqlConn in '..\..\MysqlConn.pas';

{$R *.RES}

begin
  debug('perf: All modules loaded.');
  Application.Initialize;
  Application.Title := main.appname;
  Application.CreateForm(TMainForm, MainForm); debug('perf: Main created.');

  Application.CreateForm(TCreateTableForm, CreateTableForm); debug('perf: CreateTable created.');
  Application.CreateForm(Ttbl_properties_form, tbl_properties_form); debug('perf: tbl_properties created.');
  Application.CreateForm(Ttablecomment, tablecomment); debug('perf: tablecomment created.');
  Application.CreateForm(Tloaddataform, loaddataform); debug('perf: loaddata created.');
  Application.CreateForm(TprintlistForm, printlistForm); debug('perf: printlist created.');
  Application.CreateForm(TCopyTableForm, CopyTableForm); debug('perf: CopyTable created.');
  Application.CreateForm(TFormEditUser, FormEditUser); debug('perf: EditUser created.');


  try
    try
      InitializeSync(Application.Handle);
      SetWindowName(main.discname);
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
