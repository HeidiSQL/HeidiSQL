/// sample program able to visualize .log files as created by TSynLog
program LogView;

//   first line of uses clause must be   {$I SynDprUses.inc}
uses
  {$I SynDprUses.inc}
  Forms,
  LogViewMain in 'LogViewMain.pas' {MainLogView};

{$R *.res}
{$R Vista.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainLogView, MainLogView);
  Application.Run;
end.
