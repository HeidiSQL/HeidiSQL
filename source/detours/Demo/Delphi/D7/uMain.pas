unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DDetours;

type
  TMain = class(TForm)
    BtnHook: TButton;
    BtnMsgBox: TButton;
    BtnUnhook: TButton;
    procedure BtnHookClick(Sender: TObject);
    procedure BtnMsgBoxClick(Sender: TObject);
    procedure BtnUnhookClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

type TMessageBox = function (hWnd: HWND; lpText, lpCaption: PChar; uType: UINT): Integer; stdcall;
var  TrampolineMessageBox : TMessageBox;

function InterceptMessageBox(hWnd: HWND; lpText, lpCaption: PChar; uType: UINT): Integer; stdcall;
var
  Self: TMain;
begin
  Self := GetTrampolineParam(TrampolineMessageBox);
  Self.Caption := 'MessageBox hooked !';
  Result := TrampolineMessageBox(hWnd, 'this text was hooked', 'this title was hooked', MB_ICONWARNING);
end;


procedure TMain.FormCreate(Sender: TObject);
begin
  BtnUnHook.Enabled := False;
end;

procedure TMain.BtnHookClick(Sender: TObject);
begin
  TrampolineMessageBox := InterceptCreate(@MessageBox, @InterceptMessageBox, Self);
  BtnUnHook.Enabled := True;
  BtnHook.Enabled := False;
end;

procedure TMain.BtnMsgBoxClick(Sender: TObject);
begin
  MessageBox(0, 'text', 'caption', 0);
end;

procedure TMain.BtnUnHookClick(Sender: TObject);
begin
  if Assigned(TrampolineMessageBox) then
  begin
    InterceptRemove(@TrampolineMessageBox);
    TrampolineMessageBox := nil;
    BtnHook.Enabled := True;
    BtnUnHook.Enabled := False;
  end;
end;

initialization

finalization

if Assigned(TrampolineMessageBox) then
  InterceptRemove(@TrampolineMessageBox);

end.

