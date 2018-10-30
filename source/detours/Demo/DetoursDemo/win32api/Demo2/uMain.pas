unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DDetours;

type
  TMain = class(TForm)
    Memo1: TMemo;
    Edit1: TEdit;
    BtnEnableHook: TButton;
    BtnDisableHook: TButton;
    Label1: TLabel;
    procedure BtnEnableHookClick(Sender: TObject);
    procedure BtnDisableHookClick(Sender: TObject);
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

var
  TrampolineGetSysColor: function(nIndex: Integer): DWORD;
stdcall = nil;

function InterceptGetSysColor(nIndex: Integer): DWORD; stdcall;
begin
  if nIndex = COLOR_HIGHLIGHT then
    Result := clWebOrange
  else
    Result := TrampolineGetSysColor(nIndex);
end;

procedure TMain.BtnEnableHookClick(Sender: TObject);
begin
  @TrampolineGetSysColor := InterceptCreate(@GetSysColor, @InterceptGetSysColor);
  Label1.Caption := 'GetSysColor is hooked.';
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  Label1.Caption := 'GetSysColor not hooked.';
end;

procedure TMain.BtnDisableHookClick(Sender: TObject);
begin
  if Assigned(TrampolineGetSysColor) then
  begin
    InterceptRemove(@TrampolineGetSysColor);
    TrampolineGetSysColor := nil;
    Label1.Caption := 'GetSysColor not hooked.';
  end;
end;

end.
