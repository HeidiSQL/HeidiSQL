unit mysqlerror;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ShellApi, clipbrd;

type
  TFormError = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Button3: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    errormsg : String;
    errornr  : Integer;
  end;

//var
  //FormError: TFormError;

implementation

uses helpers;

{$R *.DFM}

procedure TFormError.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TFormError.Button2Click(Sender: TObject);
var url :  Pchar;
begin
  if errormsg='' then
    abort;
  url := pchar('http://www.heidisql.com/ohelp.php?errornr='+inttostr(errornr)+'&errormsg=' + urlencode(errormsg));
  shellexecute(0, 'open', url, Nil, Nil, sw_shownormal);
end;

procedure TFormError.Button3Click(Sender: TObject);
begin
  ClipBoard.AsText := Memo1.Text;
end;

end.
