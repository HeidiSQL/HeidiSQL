program SynAutoCorrectDemo;

uses
  Forms,
  uDemo in 'uDemo.pas' {frmDemo};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Aerodynamica TAsSynAutoCorrect Demo';
  Application.CreateForm(TfrmDemo, frmDemo);
  Application.Run;
end.
