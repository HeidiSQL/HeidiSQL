unit Project06ClientMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  SynCommons, SQLite3Commons;

type
  TForm1 = class(TForm)
    edtA: TEdit;
    edtB: TEdit;
    lblA: TLabel;
    lblB: TLabel;
    btnCall: TButton;
    btnCancel: TButton;
    lblResult: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnCallClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Model: TSQLModel;
    Client: TSQLRestClientURINamedPipe;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{$R Vista.res}

procedure TForm1.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.btnCallClick(Sender: TObject);
var a,b: double;
    err: integer;
begin
  val(edtA.Text,a,err);
  if err<>0 then begin
    edtA.SetFocus;
    exit;
  end;
  val(edtB.Text,b,err);
  if err<>0 then begin
    edtB.SetFocus;
    exit;
  end;
  if Client=nil then begin
    if Model=nil then
      Model := TSQLModel.Create([],'service');
    Client := TSQLRestClientURINamedPipe.Create(Model,'RestService');
  end;
  lblResult.Caption := UTF8ToString(Client.CallBackGetResult('sum',['a',a,'b',b]));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Client.Free;
  Model.Free;
end;

end.
