program URLDemoLinux;

uses
  QForms,
  Main in 'Main.pas' {Form1},
  LibcExec in 'LibcExec.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
