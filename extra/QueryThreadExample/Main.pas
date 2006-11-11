unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MysqlQueryThread, ComCtrls, ExtCtrls;

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
    procedure lvResultSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure bnKillThreadClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    function FindListItemByThreadID(AThreadID : Integer) : Integer;
  public
    procedure UpdateThreadStatus(AQueryResult: TThreadResult);
    procedure LogMsg (AMsg : String);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  t : TMysqlQueryThread;
  cp : TConnParams;
begin
  cp.Host := edHost.Text;
  cp.Database := edDatabase.Text;
  cp.Protocol := 'mysql';
  cp.User := edUser.Text;
  cp.Pass := edPass.Text;
  cp.Port := StrToIntDef(edPort.Text,3306);
  cp.Form := Self;

  t := TMysqlQueryThread.Create(cp,Edit1.Text);
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

      TMysqlQueryThread.Create(cp,Format('KILL %s;',[lvResult.Selected.SubItems[0]]));

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

procedure TForm1.LogMsg(AMsg: String);
begin
end;

procedure TForm1.lvResultSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  bnKillThread.Enabled := Selected;
end;

procedure TForm1.UpdateThreadStatus (AQueryResult : TThreadResult);
var
  li : TListItem;
  idx : Integer;
begin

  case AQueryResult.Action of
    1:
      begin
        li := lvResult.Items.Add();
        li.Data := Pointer (AQueryResult.ThreadID);
        li.Caption := IntToStr(AQueryResult.ThreadID);
        li.SubItems.Add (IntToStr(AQueryResult.ConnectionID));
        li.SubItems.Add (AQueryResult.Sql);
        li.SubItems.Add (AQueryResult.Comment);
      end;
    2:
      begin
        idx := FindListItemByThreadID (AQueryResult.ThreadID);
        if idx<>-1 then
          begin
            lvResult.Items[idx].SubItems[2] := AQueryResult.Comment;
          end;
      end;
    3:
      begin
        idx := FindListItemByThreadID (AQueryResult.ThreadID);
        if idx<>-1 then
          lvResult.Items.Delete(idx); 

      end;
  end;
end;

end.
