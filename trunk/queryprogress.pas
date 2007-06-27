unit queryprogress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,MysqlQuery,MysqlQueryThread, ExtCtrls, communication;

type
  TfrmQueryProgress = class(TForm)
    btnAbort: TButton;
    lblStatusMsg: TLabel;
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

uses
  ChildWin,
  helpers;

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
begin
  debug(Format('qry: Progress form received WM_MYSQL_THREAD_NOTIFY message with status %d', [AMessage.LParam]));
  case AMessage.LParam of
    MQE_INITED:
      begin
        debug('qry: Setting running flag to ''true''.');
      end;
    MQE_STARTED:
      begin
      end;
    MQE_FINISHED:
      begin
        debug('qry: Setting running flag to ''false'' and closing dialog.');
        Close();
      end;
    MQE_FREED:;
  end;
end;



end.
