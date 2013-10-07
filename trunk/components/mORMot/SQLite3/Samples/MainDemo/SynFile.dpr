program SynFile;

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

//   first line of uses clause must be   {$I SynDprUses.inc}
uses
  {$I SynDprUses.inc}
  Forms,
  FileTables in 'FileTables.pas',
  FileMain in 'FileMain.pas' {MainForm},
  FileClient in 'FileClient.pas',
  FileEdit in 'FileEdit.pas' {EditForm};

{$R *.res}
{$R Vista.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TEditForm, EditForm);
  Application.Run;
end.
