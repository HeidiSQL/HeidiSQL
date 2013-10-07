program Project14Client;

uses
  Forms,
  Project14ClientMain in 'Project14ClientMain.pas' {Form1},
  Project14Interface in 'Project14Interface.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
