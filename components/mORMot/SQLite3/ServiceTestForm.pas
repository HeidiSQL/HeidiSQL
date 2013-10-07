/// form to test the service remote control
unit ServiceTestForm;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Forms,
  Controls,
  StdCtrls,
  Dialogs,
  SynCrtSock,
  SQLite3Commons,
  WinSvc,
  SQLite3Service;
  
const
  SERVICENAME = 'KOL_ServiceA';
  SERVICEEXE = 'd:\temp\debug\TestKOLService.exe';

type
  /// form to test the service remote control
  TMainServiceTest = class(TForm)
    BtnStart: TButton;
    BtnStop: TButton;
    BtnCreate: TButton;
    BtnShutDown: TButton;
    Label1: TLabel;
    BtnPause: TButton;
    BtnResume: TButton;
    procedure BtnCreateClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    /// refresh the display
    procedure WMTimer(var Msg: TMessage); message WM_TIMER;
  public
    /// helper to get the service to be controlled
    function GetService: TServiceController;
  end;

var
  MainServiceTest: TMainServiceTest;


implementation

{$R *.dfm}

procedure TMainServiceTest.BtnCreateClick(Sender: TObject);
begin
  TServiceController.CreateNewService('','',SERVICENAME,SERVICENAME,SERVICEEXE).
    Free;
  SetTimer(Handle,0,100,nil)
end;

procedure TMainServiceTest.FormShow(Sender: TObject);
begin
  PostMessage(Handle,WM_TIMER,0,0);
end;

function TMainServiceTest.GetService: TServiceController;
begin
  result := TServiceController.CreateOpenService('','',SERVICENAME);
end;

procedure TMainServiceTest.BtnClick(Sender: TObject);
var OK: boolean;
    i: integer;
begin
  with GetService do
  try
    if (SCHandle=0) or (Handle=0) then
      exit;
    OK := true;
    if Sender=BtnStart then
      OK := Start(['param1', 'param2', 'param3']) else
    if Sender=BtnStop then
      OK := Stop else
    if Sender=BtnPause then
      OK := Pause else
    if Sender=BtnResume then
      OK := Resume else
    if Sender=BtnShutDown then begin
      Stop;
      for i := 1 to 30 do
        if Status.dwCurrentState<>SERVICE_STOP_PENDING then
          break else
          Sleep(100); // wait 3 seconds
      OK := Delete;
    end;
    if not OK then
      ShowMessage(SysErrorMessage(GetLastError));
  finally
    Free;
  end;
end;

procedure TMainServiceTest.FormDestroy(Sender: TObject);
begin
  BtnClick(BtnShutDown);
  KillTimer(self.Handle,0);
end;

procedure TMainServiceTest.WMTimer(var Msg: TMessage);
var aState: TServiceState;
begin
  with GetService do
  try
    aState := State;
  finally
    Free;
  end;
  Label1.Caption := ServiceStateText(aState);
  BtnCreate.Enabled := aState in [ssNotInstalled,ssErrorRetrievingState];
  BtnStart.Enabled := aState in [ssStopped];
  BtnPause.Enabled := aState in [ssRunning];
  BtnResume.Enabled := aState in [ssPaused];
  BtnStop.Enabled := aState in [ssStarting,ssRunning,ssResuming,ssPaused];
  BtnShutDown.Enabled := aState in [ssStopped..ssPaused];
end;

end.
