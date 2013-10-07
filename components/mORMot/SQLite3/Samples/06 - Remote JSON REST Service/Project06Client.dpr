program Project06Client;

uses
  Forms,
  Project06ClientMain in 'Project06ClientMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
