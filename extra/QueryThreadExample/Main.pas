unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MysqlQueryThread, ComCtrls, ExtCtrls, MysqlQuery, Grids,
  DBGrids, DB;

type
  TForm1 = class(TForm)
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Button1: TButton;
    Edit1: TEdit;
    edHost: TEdit;
    edPort: TEdit;
    edUser: TEdit;
    edPass: TEdit;
    edDatabase: TEdit;
    Label4: TLabel;
    Panel3: TPanel;
    bnKillThread: TButton;
    Label7: TLabel;
    Panel1: TPanel;
    Panel4: TPanel;
    lvResult: TListView;
    Panel5: TPanel;
    Splitter1: TSplitter;
    pnResultset: TPanel;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    Button2: TButton;
    Button3: TButton;
    procedure Button3Click(Sender: TObject);
    procedure lvResultSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure bnKillThreadClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    function FindListItemByMysqlQueryObject(AObject: Pointer): Integer;
    function AssembleConnParams () : TConnParams;
    function AddListItem(AMysqlQuery : Pointer; ACaption : String) : TListItem;
  public
    procedure LogMsg (AMsg : String);
    procedure HandleQueryNotification(ASender : TMysqlQuery; AEvent : Integer);
    procedure HandleQueryNotificationMsg(var AMessage : TMessage); message WM_MYSQL_THREAD_NOTIFY;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  cp : TConnParams;
  mq : TMysqlQuery;
begin
  cp := AssembleConnParams();
  mq := ExecMysqlStatementAsync(Edit1.Text,cp,HandleQueryNotification);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  cp : TConnParams;
  mq : TMysqlQuery;
begin
  cp := AssembleConnParams();
  mq := ExecMysqlStatementBlocking(Edit1.Text,cp);
end;

function TForm1.AddListItem(AMysqlQuery: Pointer; ACaption: String): TListItem;
begin
  Result := lvResult.Items.Add();
  Result.Data := AMysqlQuery;
  Result.Caption := ACaption;
  Result.SubItems.Add('');
  Result.SubItems.Add('');
  Result.SubItems.Add('');
  Result.SubItems.Add('');
  Result.SubItems.Add('');
end;

function TForm1.AssembleConnParams: TConnParams;
begin
  Result.Host := edHost.Text;
  Result.Database := edDatabase.Text;
  Result.Protocol := 'mysql';
  Result.User := edUser.Text;
  Result.Pass := edPass.Text;
  Result.Port := StrToIntDef(edPort.Text,3306);
  Result.WndHandle := Handle;
end;

procedure TForm1.bnKillThreadClick(Sender: TObject);
var
  cp : TConnParams;
  mq : TMysqlQuery;
  li : TListItem;
begin
  li := lvResult.Selected;

  if li<>nil then
    if li.Data <> nil then
      begin
        mq := li.Data;

        if mq.ConnectionID<>0 then
          begin
            cp := AssembleConnParams();
            ExecMysqlStatementAsync (Format('KILL %d;',[mq.ConnectionID]),cp,HandleQueryNotification);
          end;
      end;

end;

function TForm1.FindListItemByMysqlQueryObject(AObject : Pointer): Integer;
var
  i : Integer;
begin
  Result := -1;

  if lvResult.Items.Count > 0 then
    for i  := 0 to lvResult.Items.Count - 1 do
      if lvResult.Items[i].Data = AObject then
        begin
          Result := i;
          Break;
        end;

end;

procedure TForm1.HandleQueryNotification(ASender : TMysqlQuery; AEvent : Integer);
var
  li : TListItem;
  idx : Integer;
begin
  li := nil;
  idx := FindListItemByMysqlQueryObject (ASender);

  if (idx<>-1) or (AEvent=MQE_INITED) then
    begin
      if idx=-1 then
        li := AddListItem(ASender,'?')
      else
        li := lvResult.Items[idx];

      case AEvent of
        MQE_INITED:
          begin
            li.Caption := IntToStr(ASender.ThreadID);
            li.SubItems[0] := '?';
            li.SubItems[1] := 'INIT';
            li.SubItems[2] := ASender.Sql;
          end;
        MQE_STARTED:
          begin
            li.SubItems[0] := IntToStr(ASender.ConnectionID);
            li.SubItems[1] := 'STARTED';
          end;
        MQE_FINISHED:
          begin
            li.SubItems[1] := 'FINISHED';
          end;
        MQE_FREED:
          begin
            li.SubItems[1] := 'IDLE';
          end;

      end;

      li.SubItems[3] := Format('[%d] %s',[ASender.Result,ASender.Comment]);
    end;

end;

procedure TForm1.HandleQueryNotificationMsg(var AMessage: TMessage);
var
  idx : Integer;
  li : TListItem;
  mq : TMysqlQuery;
begin
  //ShowMessageFmt('thread notification winmessage: wp=%d  lp=%d    qry=%s',[AMessage.WParam,AMessage.LParam,TMysqlQuery(Pointer(AMessage.WParam)).Sql]);

  idx := FindListItemByMysqlQueryObject (Pointer(AMessage.WParam));
  if (idx<>-1) or (AMessage.LParam=MQE_INITED) then
    begin
      mq := TMysqlQuery(Pointer(AMessage.WParam));

      if idx=-1 then
        li := AddListItem(mq,'?')
      else
        li := lvResult.Items[idx];

      li.SubItems[1] := IntToStr(AMessage.LParam);


      case AMessage.LParam of
        MQE_INITED:
          li.Caption := IntToStr(mq.ThreadID);
        MQE_STARTED:;
        MQE_FINISHED:;
        MQE_FREED:;
      end;


    end;
end;

procedure TForm1.LogMsg(AMsg: String);
begin
end;

procedure TForm1.lvResultSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  mq : TMysqlQuery;
begin
  bnKillThread.Enabled := Selected;
  Datasource1.DataSet := nil;

  if lvResult.Selected<>nil then
    if lvResult.Selected.Data <> nil then
      begin
        mq := TMysqlQuery(lvResult.Selected.Data);
        if mq.HasResultset then
          begin
            DataSource1.Dataset := mq.MysqlDataset;
            // Enable free button
          end;
        
      end;
      

end;


end.
