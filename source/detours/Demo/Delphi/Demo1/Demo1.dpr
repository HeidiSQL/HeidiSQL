program Demo1;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Main},
  CPUID in '..\..\..\Source\CPUID.pas',
  DDetours in '..\..\..\Source\DDetours.pas',
  InstDecode in '..\..\..\Source\InstDecode.pas',
  LegacyTypes in '..\..\..\Source\LegacyTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
