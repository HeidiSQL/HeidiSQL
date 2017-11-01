unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DDetours;

type
  TMain = class(TForm)
    BtnCallShowMsg: TButton;
    BtnHook: TButton;
    BtnUnHook: TButton;
    procedure BtnCallShowMsgClick(Sender: TObject);
    procedure BtnHookClick(Sender: TObject);
    procedure BtnUnHookClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type
  IMyInterface = Interface
    procedure ShowMsg(const Msg: String);
  End;

  TMyObject = class(TInterfacedObject, IMyInterface)
  public
    procedure ShowMsg(const Msg: String);
  end;

var
  Main: TMain;
  FMyInterface: IMyInterface;

implementation

{$R *.dfm}

var
  TrampolineShowMsg: procedure(const Self; const Msg: String) = nil;

procedure TMain.BtnUnHookClick(Sender: TObject);
begin
  if Assigned(TrampolineShowMsg) then
  begin
    InterceptRemove(@TrampolineShowMsg);
    TrampolineShowMsg := nil;
  end;
end;

procedure TMain.BtnCallShowMsgClick(Sender: TObject);
begin
  FMyInterface.ShowMsg('This is a test !');
end;

procedure InterceptShowMsg(const Self; const Msg: String);
begin
  TrampolineShowMsg(Self, 'Hooked');
end;
{ TMyObject }

procedure TMyObject.ShowMsg(const Msg: String);
var
  S: String;
begin
  S := 'Your Message : ' + Msg;
  ShowMessage(S);
end;

procedure TMain.BtnHookClick(Sender: TObject);
begin
  @TrampolineShowMsg := InterceptCreate(FMyInterface, 3, @InterceptShowMsg);
end;

initialization

FMyInterface := TMyObject.Create;

end.
