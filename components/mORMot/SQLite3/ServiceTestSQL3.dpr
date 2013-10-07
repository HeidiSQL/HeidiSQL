/// test the service remote control of the SQLite3Service unit
program ServiceTestSQL3;

uses
  Forms,
  ServiceTestForm in 'ServiceTestForm.pas' {MainServiceTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainServiceTest, MainServiceTest);
  Application.Run;
end.
