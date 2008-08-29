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
    procedure FormCreate(Sender: TObject);
  private
    procedure HandleQueryNotificationMsg(var AMessage : TMessage); message WM_MYSQL_THREAD_NOTIFY;
  public

  end;

var
  frmQueryProgress: TfrmQueryProgress;

implementation

uses
  ChildWin,
  helpers,
  main;

{$R *.dfm}

procedure TfrmQueryProgress.btnAbortClick(Sender: TObject);
begin
  Close();
  // todo: implement connection killing !!
end;


procedure TfrmQueryProgress.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmQueryProgress.FormCreate(Sender: TObject);
begin
  InheritFont(Font);
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
    MQE_FINISHED:
      begin
        debug('qry: Setting running flag to ''false'' and closing dialog.');
        Close();
      end;
  end;
end;



end.
