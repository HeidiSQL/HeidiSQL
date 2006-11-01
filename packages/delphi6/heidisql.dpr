program heidisql;

{%ToDo 'heidisql.todo'}

uses
  Forms,
  Main in '..\..\MAIN.PAS' {MainForm},
  childwin in '..\..\childwin.pas' {MDIChild},
  About in '..\..\about.pas' {AboutBox},
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
  insertfiles_progress in '..\..\insertfiles_progress.pas' {frmInsertFilesProgress};

{$R *.RES}

begin
//  AboutBox := TAboutBox.Create(Application);
//  AboutBox.show;
//  AboutBox.Update;

  Application.Initialize;
  Application.Title := 'HeidiSQL';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(Tconnform, connform);
  Application.CreateForm(TCreateTableForm, CreateTableForm);
  Application.CreateForm(TFieldEditForm, FieldEditForm);
  Application.CreateForm(Ttbl_properties_form, tbl_properties_form);
  Application.CreateForm(Ttablecomment, tablecomment);
  Application.CreateForm(Tloaddataform, loaddataform);
  Application.CreateForm(TUserManagerForm, UserManagerForm);
  Application.CreateForm(TprintlistForm, printlistForm);
  Application.CreateForm(TCopyTableForm, CopyTableForm);
  Application.CreateForm(TFormEditUser, FormEditUser);
  Application.Run;
 end.
