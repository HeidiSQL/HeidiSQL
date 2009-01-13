unit IpcMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, MysqlQueryThread, MysqlQuery, IpcSession;

type
  TForm1 = class(TForm)
    Panel2: TPanel;
    StatusBar1: TStatusBar;
    pnMain: TPanel;
    pnWindowList: TPanel;
    Panel3: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    lvWindows: TListView;
    Label2: TLabel;
    Button2: TButton;
    Splitter1: TSplitter;
    Panel4: TPanel;
    Memo1: TMemo;
    Button3: TButton;
    Label3: TLabel;
    Button4: TButton;
    Button5: TButton;
    pnSessionSettings: TPanel;
    Label1: TLabel;
    edConnName: TEdit;
    Button1: TButton;
    Label5: TLabel;
    CheckBox1: TCheckBox;
    procedure CheckBox1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvWindowsDblClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SessOpened(Sender: TObject);
    procedure SessClosed(Sender: TObject);
  private
    FSess : TIpcSession;
    function AssembleConnParams: TOpenConnProf;
  public
    procedure HandleWMCopyData(var msg: TWMCopyData); message WM_COPYDATA;
    procedure DumpWindowList();
    procedure DebugWrite(AMsg : String);
  end;

var
  Form1: TForm1;

implementation

uses communication, synchronization, threading, helpers;

{$R *.dfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  FSess.Name := edConnName.Text;
  DumpWindowList();
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  DumpWindowList();
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  CheckForCrashedWindows();
  DumpWindowList();
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if FSess.Open(True,edConnName.Text) then
    begin
    end;

  DumpWindowList();
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  FSess.Close();
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  FSess.ConnectedFlag := CheckBox1.Checked;
end;

procedure TForm1.DebugWrite(AMsg: String);
begin
  Memo1.Lines.Add(AMsg);
end;

procedure TForm1.DumpWindowList;
var
  wda : TWindowDataArray;
  i : Integer;
  li : TListItem;
begin
  lvWindows.Items.BeginUpdate();
  lvWindows.Items.Clear();

  wda := GetWindowList();
  for i := Low(wda) to High(wda) do
    begin
      li := lvWindows.Items.Add();
      li.Caption := Format('%d',[i]);
      li.SubItems.Add(Format('%d',[wda[i].AppHandle]));
      li.SubItems.Add(wda[i].Name);
      li.SubItems.Add(Format('%d',[Ord(wda[i].Connected)]));
      //DebugWrite(Format('%d -- Handle: %d  Name: %s',[i,wda[i].AppHandle,wda[i].Name]));
    end;


  lvWindows.Items.EndUpdate();
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FSess := TIpcSession.Create(Self,Handle);
  FSess.OnOpened := SessOpened;
  FSess.OnClosed := SessClosed;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil (FSess);
end;

procedure TForm1.HandleWMCopyData(var msg: TWMCopyData);
var
  method: DWORD;
begin
  // extract info from message
  method := msg.CopyDataStruct^.dwData;

  // display
  DebugWrite(Format('WM_COPYDATA received; method=%d',[method]));

  if method in [CMD_EXECUTEQUERY_NORESULTS,CMD_EXECUTEQUERY_RESULTS] then
    DebugWrite(Format('Request: %d  -- Query: %s',[GetRequestIdFromMsg(msg),GetQueryFromMsg(msg)]));


  // release
  ReleaseRemoteCaller(ERR_UNSPECIFIED);
end;

procedure TForm1.lvWindowsDblClick(Sender: TObject);
var
  h : THandle;
begin
  if lvWindows.Selected<>nil then
    begin
      h := StrToIntDef(lvWindows.Selected.SubItems[0],0);

      if h<>0 then
        ActivateWindow(h);
    end;
end;

procedure TForm1.SessClosed(Sender: TObject);
begin
  DebugWrite('Ipc session closed');
  pnWindowList.Enabled := False;
end;

procedure TForm1.SessOpened(Sender: TObject);
begin
  DebugWrite('Ipc session opened');
  pnWindowList.Enabled := True;
end;

function TForm1.AssembleConnParams: TOpenConnProf;
begin
  ZeroMemory(@Result,SizeOf(Result));
  Result.MysqlParams.Host := 'localhost';
  Result.MysqlParams.Database := 'test';
  Result.MysqlParams.Protocol := 'mysql';
  Result.MysqlParams.User := 'root';
  Result.MysqlParams.Pass := '';
  Result.MysqlParams.Port := StrToIntDef('',3306);
end;


end.
