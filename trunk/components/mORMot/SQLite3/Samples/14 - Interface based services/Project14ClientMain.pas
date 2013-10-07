unit Project14ClientMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  SynCommons, SQLite3Commons, SQLite3HttpClient,
  Project14Interface;

type
  TForm1 = class(TForm)
    edtA: TEdit;
    edtB: TEdit;
    lblA: TLabel;
    lblB: TLabel;
    btnCall: TButton;
    btnCancel: TButton;
    lblResult: TLabel;
    ComboProtocol: TComboBox;
    procedure btnCancelClick(Sender: TObject);
    procedure btnCallClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboProtocolChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Model: TSQLModel;
    Client: TSQLRestClientURI;
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
var a,b: integer;
    err: integer;
    I: ICalculator;
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
      Model := TSQLModel.Create([],ROOT_NAME);
    case ComboProtocol.ItemIndex of
    0: Client := TSQLite3HttpClient.Create('localhost','888',Model);
    1: Client := TSQLRestClientURINamedPipe.Create(Model,APPLICATION_NAME);
    else exit;
    end;
    Client.SetUser('User','synopse');
    Client.ServiceRegister([TypeInfo(ICalculator)],sicShared);
  end;
  if Client.Services['Calculator'].Get(I) then
    lblResult.Caption := IntToStr(I.Add(a,b));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Client.Free;
  Model.Free;
end;

procedure TForm1.ComboProtocolChange(Sender: TObject);
begin
  FreeAndNil(Client);
end;

end.
