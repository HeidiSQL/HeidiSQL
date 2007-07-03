program heidisql;

{%ToDo 'heidisql.todo'}

uses
  Forms,
  Main in '..\..\source\MAIN.PAS' {MainForm},
  childwin in '..\..\source\childwin.pas' {MDIChild},
  About in '..\..\source\about.pas' {AboutBox},
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
  insertfiles_progress in '..\..\source\insertfiles_progress.pas' {frmInsertFilesProgress};

{$R *.RES}

begin
//  AboutBox := TAboutBox.Create(Application);
//  AboutBox.show;
//  AboutBox.Update;

  Application.Initialize;
  Application.Title := 'HeidiSQL';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(Tconnform, connform);
  Application.CreateForm(TFieldEditForm, FieldEditForm);
  Application.CreateForm(Tloaddataform, loaddataform);
  Application.CreateForm(TUserManagerForm, UserManagerForm);
  Application.CreateForm(TprintlistForm, printlistForm);
  Application.CreateForm(TCopyTableForm, CopyTableForm);
  Application.CreateForm(TFormEditUser, FormEditUser);
  Application.Run;
 end.
