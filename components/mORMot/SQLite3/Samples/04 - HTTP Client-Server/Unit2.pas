unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  SynCommons, SQLite3Commons, SQLite3, SQLite3HttpServer, SampleData;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
    Model: TSQLModel;
    DB: TSQLRestServerDB;
    Server: TSQLite3HttpServer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Model := CreateSampleModel;
  DB := TSQLRestServerDB.Create(Model,ChangeFileExt(paramstr(0),'.db3'),true);
  DB.CreateMissingTables(0);
  Server := TSQLite3HttpServer.Create('8080',[DB]);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Server.Free;
  DB.Free;
  Model.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Label1.Caption := Caption;
end;

end.
