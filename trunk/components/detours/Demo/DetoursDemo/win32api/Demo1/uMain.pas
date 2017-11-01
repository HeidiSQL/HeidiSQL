unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DDetours;

type
  TMain = class(TForm)
    BtnHook: TButton;
    BtnUnHook: TButton;
    BtnTestMsgBox: TButton;
    procedure BtnHookClick(Sender: TObject);
    procedure BtnUnHookClick(Sender: TObject);
    procedure BtnTestMsgBoxClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

var
  TrampolineMessageBoxW: function(hWnd: hWnd; lpText, lpCaption: LPCWSTR; uType: UINT): Integer;
stdcall = nil;

function InterceptMessageBoxW(hWnd: hWnd; lpText, lpCaption: LPCWSTR; uType: UINT): Integer; stdcall;
begin
  Result := TrampolineMessageBoxW(hWnd, 'Hooked', 'Hooked', MB_OK or MB_ICONEXCLAMATION);
end;

procedure TMain.BtnHookClick(Sender: TObject);
begin
  if not Assigned(TrampolineMessageBoxW) then
  begin
    @TrampolineMessageBoxW := InterceptCreate(@MessageBoxW, @InterceptMessageBoxW);
  end;
end;

procedure TMain.BtnTestMsgBoxClick(Sender: TObject);
begin
  MessageBoxW(Handle, 'Text', 'Caption', MB_OK);
end;

procedure TMain.BtnUnHookClick(Sender: TObject);
begin
  if Assigned(TrampolineMessageBoxW) then
  begin
    InterceptRemove(@TrampolineMessageBoxW);
    TrampolineMessageBoxW := nil;
  end;
end;

end.
