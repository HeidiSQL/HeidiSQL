unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DDetours;

type
  TMain = class(TForm)
    MemLog: TMemo;
    BtnEnableHook1: TButton;
    BtnEnableHook2: TButton;
    BtnEnableHook3: TButton;
    BtnRemoveHook1: TButton;
    BtnRemoveHook2: TButton;
    BtnRemoveHook3: TButton;
    BtnTest: TButton;
    procedure BtnEnableHook1Click(Sender: TObject);
    procedure BtnEnableHook2Click(Sender: TObject);
    procedure BtnEnableHook3Click(Sender: TObject);
    procedure BtnRemoveHook1Click(Sender: TObject);
    procedure BtnRemoveHook2Click(Sender: TObject);
    procedure BtnRemoveHook3Click(Sender: TObject);
    procedure BtnTestClick(Sender: TObject);
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

type
  TMsgBoxNextHookProc = function(hWnd: hWnd; lpText, lpCaption: LPCWSTR; uType: UINT): Integer; stdcall;

var
  NextHook1: TMsgBoxNextHookProc = nil;
  NextHook2: TMsgBoxNextHookProc = nil;
  NextHook3: TMsgBoxNextHookProc = nil;

function MessageBox_Hook1(hWnd: hWnd; lpText, lpCaption: LPCWSTR; uType: UINT): Integer; stdcall;
begin
  Main.MemLog.Lines.Add('Hook 1 executed successfully.');
  Result := NextHook1(hWnd, 'Hook1', lpCaption, uType);
end;

function MessageBox_Hook2(hWnd: hWnd; lpText, lpCaption: LPCWSTR; uType: UINT): Integer; stdcall;
begin
  Main.MemLog.Lines.Add('Hook 2 executed successfully.');
  Result := NextHook2(hWnd, lpText, 'Hook2', uType);
end;

function MessageBox_Hook3(hWnd: hWnd; lpText, lpCaption: LPCWSTR; uType: UINT): Integer; stdcall;
begin
  Main.MemLog.Lines.Add('Hook 3 executed successfully.');
  Result := NextHook3(hWnd, lpText, lpText, uType);
end;

procedure TMain.BtnEnableHook1Click(Sender: TObject);
begin
  if not Assigned(NextHook1) then
    @NextHook1 := InterceptCreate(@MessageBox, @MessageBox_Hook1);
end;

procedure TMain.BtnEnableHook2Click(Sender: TObject);
begin
  if not Assigned(NextHook2) then
    @NextHook2 := InterceptCreate(@MessageBox, @MessageBox_Hook2);
end;

procedure TMain.BtnEnableHook3Click(Sender: TObject);
begin
  if not Assigned(NextHook3) then
    @NextHook3 := InterceptCreate(@MessageBox, @MessageBox_Hook3);
end;

procedure TMain.BtnRemoveHook1Click(Sender: TObject);
begin
  if Assigned(NextHook1) then
  begin
    InterceptRemove(@NextHook1);
    NextHook1 := nil;
  end;
end;

procedure TMain.BtnRemoveHook2Click(Sender: TObject);
begin
  if Assigned(NextHook2) then
  begin
    InterceptRemove(@NextHook2);
    NextHook2 := nil;
  end;
end;

procedure TMain.BtnRemoveHook3Click(Sender: TObject);
begin
  if Assigned(NextHook3) then
  begin
    InterceptRemove(@NextHook3);
    NextHook3 := nil;
  end;
end;

procedure TMain.BtnTestClick(Sender: TObject);
begin
  MemLog.Lines.Add('----------------------------');
  MessageBox(Handle, 'Msg Text', 'Msg Caption', MB_OK);
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  MemLog.Clear;
end;

end.
