program IpcExample;

uses
  Forms,
  IpcMain in 'IpcMain.pas' {Form1},
  IpcSession in 'IpcSession.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
