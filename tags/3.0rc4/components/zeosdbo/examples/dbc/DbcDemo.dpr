program DbcDemo;

uses
  Forms,
  DbcDemoMain in 'DbcDemoMain.pas' {frmDBCDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDBCDemo, frmDBCDemo);
  Application.Run;
end.
