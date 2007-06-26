program CompletionProposalDemo;

uses
  Forms,
  FormMain_ctCode in 'FormMain_ctCode.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
