program ThemeExplorerDB;

uses
  Forms,
  DBMain in 'DBMain.pas' {DBMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Windows XP theme explorer';
  Application.CreateForm(TDBMainForm, DBMainForm);
  Application.Run;
end.
