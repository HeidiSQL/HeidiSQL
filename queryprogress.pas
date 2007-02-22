unit queryprogress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,MysqlQuery,MysqlQueryThread, ExtCtrls, communication;

type
  TfrmQueryProgress = class(TForm)
    btnAbort: TButton;
    lblStatusMsg: TLabel;
    timAntiFreeze: TTimer;
    procedure timAntiFreezeTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnAbortClick(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  private
    procedure HandleQueryNotificationMsg(var AMessage : TMessage); message WM_MYSQL_THREAD_NOTIFY;
  public

  end;

var
  frmQueryProgress: TfrmQueryProgress;

implementation

uses ChildWin; 

{$R *.dfm}

procedure TfrmQueryProgress.btnAbortClick(Sender: TObject);
begin
  Close();
  // todo: implement connection killing !!
end;

procedure TfrmQueryProgress.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST;
end;

procedure TfrmQueryProgress.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

{***
  Handles the TMysqlQueryThread notification messages.

  @param TMessage Message structure containing
                  * LParam: Event type
                  * WParam: MysqlQuery object containing status + resultset
}

procedure TfrmQueryProgress.HandleQueryNotificationMsg(var AMessage: TMessage);
var
  ChildWin : TMDIChild;
begin
  case AMessage.LParam of
    MQE_INITED:
      begin
        TMDIChild(Owner).SetQueryRunningFlag(True); // Todo: check if this is still needed
      end;
    MQE_STARTED:
      begin
        ShowModal();
      end;
    MQE_FINISHED:
      begin
        TMysqlQuery(AMessage.WParam);
        ChildWin := TMDIChild(Owner);
        ChildWin.SetQueryRunningFlag(False);
        Close();
      end;
    MQE_FREED:;
  end;
end;


procedure TfrmQueryProgress.timAntiFreezeTimer(Sender: TObject);
begin
  Application.ProcessMessages();
end;

end.
