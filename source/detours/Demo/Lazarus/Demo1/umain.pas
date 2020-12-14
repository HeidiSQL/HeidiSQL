unit uMain;

{$mode Delphi}{$H+}


interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Windows, DDetours;

type

  { TMain }

  TMain = class(TForm)
    BtnHook: TButton;
    BtnMsgBox: TButton;
    BtnUnhook: TButton;
    procedure BtnHookClick(Sender: TObject);
    procedure BtnMsgBoxClick(Sender: TObject);
    procedure BtnUnhookClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Main: TMain;

implementation

{$R *.lfm}

type
  TMessageBox = function(hWnd: HWND; lpText: LPCSTR; lpCaption: LPCSTR;
    uType: UINT): longint; stdcall;

var
  TrampolineMessageBox: TMessageBox = nil;

function InterceptMessageBox(hWnd: HWND; lpText: LPCSTR; lpCaption: LPCSTR;
  uType: UINT): longint; stdcall;
var
  Form: TMain;
begin
  Form := GetTrampolineParam(TrampolineMessageBox);
  Form.Caption := 'MessageBox Hooked!';
  Result := TrampolineMessageBox(hWnd, 'this text was hooked.',
    'this caption was hooked.', MB_ICONEXCLAMATION);
end;

{ TMain }

procedure TMain.BtnHookClick(Sender: TObject);
begin
  BtnUnhook.Enabled := True;
  BtnHook.Enabled := False;
  @TrampolineMessageBox := InterceptCreate(@MessageBox, @InterceptMessageBox, Self);
end;

procedure TMain.BtnMsgBoxClick(Sender: TObject);
begin
  MessageBox(0, 'text', 'caption', 0);
end;

procedure TMain.BtnUnhookClick(Sender: TObject);
begin
  BtnHook.Enabled := True;
  BtnUnHook.Enabled := False;
  if Assigned(TrampolineMessageBox) then
  begin
    InterceptRemove(@TrampolineMessageBox);
    TrampolineMessageBox := nil;
  end;
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  BtnUnhook.Enabled := False;
end;

initialization

finalization
  if Assigned(TrampolineMessageBox) then
    InterceptRemove(@TrampolineMessageBox);
end.
