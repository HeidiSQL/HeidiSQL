unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MysqlQueryThread;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    Label1: TLabel;
    edHost: TEdit;
    edPort: TEdit;
    edUser: TEdit;
    edPass: TEdit;
    edDatabase: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Label5: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure LogMsg (AMsg : String);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  t : TMysqlQueryThread;
  cp : TConnParams;
begin
  cp.Host := edHost.Text;
  cp.Database := edDatabase.Text;
  cp.Protocol := 'mysql';
  cp.User := edUser.Text;
  cp.Pass := edPass.Text;
  cp.Port := StrToIntDef(edPort.Text,3306);
  cp.Form := Self;

  t := TMysqlQueryThread.Create(cp,Edit1.Text);
end;

procedure TForm1.LogMsg(AMsg: String);
begin
  Memo1.Lines.Add (AMsg);
end;

end.
