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
    procedure lvResultSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure bnKillThreadClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    function FindListItemByMysqlQueryObject(AObject: Pointer): Integer;
    function FindListItemByThreadID(AThreadID : Integer) : Integer;
  public
    procedure LogMsg (AMsg : String);
    procedure HandleQueryNotification(ASender : TMysqlQuery; AEvent : Integer);
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

  cp.Host := edHost.Text;
  cp.Database := edDatabase.Text;
  cp.Protocol := 'mysql';
  cp.User := edUser.Text;
  cp.Pass := edPass.Text;
  cp.Port := StrToIntDef(edPort.Text,3306);
  cp.Form := Self;

  mq := TMysqlQuery.Create(nil,@cp);
  mq.OnNotify := HandleQueryNotification;
  mq.Query(Edit1.Text,MQM_ASYNC);
end;

procedure TForm1.bnKillThreadClick(Sender: TObject);
var
  cp : TConnParams;
begin

  if lvResult.Selected<>nil then
    begin


      cp.Host := edHost.Text;
      cp.Database := edDatabase.Text;
      cp.Protocol := 'mysql';
      cp.User := edUser.Text;
      cp.Pass := edPass.Text;
      cp.Port := StrToIntDef(edPort.Text,3306);
      cp.Form := Self;

      TMysqlQueryThread.Create(nil,cp,Format('KILL %s;',[lvResult.Selected.SubItems[0]]));

    end;

end;

function TForm1.FindListItemByThreadID(AThreadID: Integer): Integer;
var
  i : Integer;
begin
  Result := -1;

  if lvResult.Items.Count > 0 then
    for i  := 0 to lvResult.Items.Count - 1 do
      if lvResult.Items[i].Data = Pointer(AThreadID) then
        begin
          Result := i;
          Break;
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

  case AEvent of
    MQE_INITED:
      begin
        li := lvResult.Items.Add();
        li.Caption := IntToStr(ASender.ThreadID);
        li.Data := ASender;
        li.SubItems.Add (''); // connection id
        li.SubItems.Add (ASender.Sql);
        li.SubItems.Add (ASender.Comment);        
      end;
    MQE_STARTED:
      begin
        idx := FindListItemByMysqlQueryObject (ASender);
        if idx<>-1 then
          begin
            li := lvResult.Items[idx];
            li.SubItems[0] := IntToStr(ASender.ConnectionID);
            li.SubItems[1] := ASender.Sql;
            li.SubItems[2] := ASender.Comment;
          end;
      end;
    MQE_FINISHED:
      begin
        idx := FindListItemByMysqlQueryObject (ASender);
        if idx<>-1 then
          begin
            li := lvResult.Items[idx];
            li.SubItems[1] := ASender.Sql;
            li.SubItems[2] := ASender.Comment;
          end;
      end;
    // MQE_FREED ...

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
