unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DDetours;

type
  TMain = class(TForm)
    BtnEnableHook: TButton;
    BtnDisableHook: TButton;
    Edit1: TEdit;
    BtnClickMe: TButton;
    procedure BtnEnableHookClick(Sender: TObject);
    procedure BtnDisableHookClick(Sender: TObject);
    procedure BtnClickMeClick(Sender: TObject);
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
  TrampolineSetTextBuf: procedure(const Self; Buffer: PChar) = nil;

procedure SetTextBufHooked(const Self; Buffer: PChar);
var
  S: String;
begin
  S := 'Hooked _' + String(Buffer);
  TrampolineSetTextBuf(Self, PChar(S)); // Call the original function .
end;

procedure TMain.BtnDisableHookClick(Sender: TObject);
begin
  if Assigned(TrampolineSetTextBuf) then
  begin
    InterceptRemove(@TrampolineSetTextBuf);
    TrampolineSetTextBuf := nil;
  end;
end;

procedure TMain.BtnEnableHookClick(Sender: TObject);
begin
  Edit1.Text := 'Enter new text ..';
  @TrampolineSetTextBuf := InterceptCreate(@TControl.SetTextBuf, @SetTextBufHooked);
end;

procedure TMain.BtnClickMeClick(Sender: TObject);
begin
  BtnClickMe.Caption := Edit1.Text;
end;

end.
