unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SQLite3Commons, SQLite3, StdCtrls, SampleData;

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
    Server: TSQLRestServerDB;
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
  Server := TSQLRestServerDB.Create(Model,ChangeFileExt(paramstr(0),'.db3'));
  Server.CreateMissingTables;
  Server.ExportServerNamedPipe('03');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Server.Free;
  Model.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Label1.Caption := Caption;
end;

end.
