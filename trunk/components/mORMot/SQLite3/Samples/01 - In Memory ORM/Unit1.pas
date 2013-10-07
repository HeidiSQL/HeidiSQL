unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  SynCommons, SQLite3Commons,
  SampleData;

type
  TForm1 = class(TForm)
    QuestionMemo: TMemo;
    NameEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    AddButton: TButton;
    QuitButton: TButton;
    FindButton: TButton;
    procedure AddButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure QuitButtonClick(Sender: TObject);
    procedure FindButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    Database: TSQLRest;
    Model: TSQLModel;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.AddButtonClick(Sender: TObject);
var Rec: TSQLSampleRecord;
begin
  Rec := TSQLSampleRecord.Create;
  try
    // we use explicit StringToUTF8() for conversion below
    // a real application should use TLanguageFile.StringToUTF8() in SQLite3i18n
    Rec.Name := StringToUTF8(NameEdit.Text);
    Rec.Question := StringToUTF8(QuestionMemo.Text);
    if Database.Add(Rec,true)=0 then
      ShowMessage('Error adding the data') else begin
      NameEdit.Text := '';
      QuestionMemo.Text := '';
      NameEdit.SetFocus;
    end;
  finally
    Rec.Free;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Database.Free;
  Model.Free;
end;

procedure TForm1.QuitButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FindButtonClick(Sender: TObject);
var Rec: TSQLSampleRecord;
begin
  Rec := TSQLSampleRecord.Create(Database,'Name=?',[StringToUTF8(NameEdit.Text)]);
  try
    if Rec.ID=0 then
      QuestionMemo.Text := 'Not found' else
      QuestionMemo.Text := UTF8ToString(Rec.Question);
  finally
    Rec.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Model := CreateSampleModel; // from SampleData unit
end;

end.
