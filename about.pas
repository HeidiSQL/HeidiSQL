unit About;


// -------------------------------------
// HeidiSQL
// About-box
// -------------------------------------


interface

uses Windows, Menus, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ShellApi, SysUtils, dialogs, ComCtrls;

type
  TAboutBox = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    Button1: TButton;
    Panel2: TPanel;
    ProductName: TLabel;
    LabelVersion: TLabel;
    LabelCompiled: TLabel;
    Label1: TLabel;
    LabelWebpage: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Button2: TButton;
    Button3: TButton;
    Image2: TImage;
    Memo1: TMemo;
    Label2: TLabel;
    procedure OKButtonClick(Sender: TObject);
    procedure OpenURL(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure MouseOver(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation

uses main;
{$R *.DFM}



procedure TAboutBox.OKButtonClick(Sender: TObject);
begin
  close;
end;


procedure TAboutBox.OpenURL(Sender: TObject);
var url :  Pchar;
begin
  url := pchar(TControl(Sender).Hint);
  shellexecute(0, 'open', url, Nil, Nil, sw_shownormal);
end;



procedure TAboutBox.Button1Click(Sender: TObject);
begin
  close;
end;


procedure TAboutBox.MouseOver(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  LabelWebpage.Font.color := clBlue;
  if sender is TLabel then
    TLabel(Sender).Font.color := clRed;
end;


procedure TAboutBox.FormShow(Sender: TObject);
var
  OSVersion   : OSVERSIONINFO;
  os, ver     : String;
  CompiledInt : Integer;
  Compiled    : TDateTime;
begin
  Screen.Cursor := crHourGlass;

  // App-Version
  LabelVersion.Caption := 'Version ' + appversion;

  // Compile-date
  CompiledInt := Fileage(paramstr(0));
  Compiled := FileDateToDateTime(CompiledInt);
  LabelCompiled.Caption := 'Compiled on: ' + DateTimeToStr(Compiled);

  Screen.Cursor := crDefault;
end;

end.

