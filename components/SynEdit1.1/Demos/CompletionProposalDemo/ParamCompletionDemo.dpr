program ParamCompletionDemo;

uses
  Forms,
  FormMain_ctParams in 'FormMain_ctParams.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
