program ThemeExplorer;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  SubForm1 in 'SubForm1.pas' {Form1},
  SubForm2 in 'SubForm2.pas' {Form2},
  SubForm3 in 'SubForm3.pas' {Form3},
  TestFrame in 'TestFrame.pas' {Frame1: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Windows XP theme explorer';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
