program QueryThreadExample;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  MysqlQueryThread in 'MysqlQueryThread.pas',
  MysqlQuery in 'MysqlQuery.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
