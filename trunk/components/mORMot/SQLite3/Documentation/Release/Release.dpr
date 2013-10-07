program Release;

uses
  {$I SynDprUses.inc}
  Forms,
  ReleaseForm in 'ReleaseForm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
