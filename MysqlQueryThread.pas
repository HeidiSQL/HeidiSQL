unit MysqlQueryThread;

interface

uses
  Windows, Messages, Forms, Db, Classes, ZConnection, ZDataSet, StdCtrls;

{$IFDEF EXAMPLE_APP}
const
  WM_MYSQL_THREAD_NOTIFY = WM_USER+100;
{$ENDIF}

type
  // Mysql connection parameter structure
  TMysqlConnParams = record
    Host,
    Database,
    Protocol,
    User,
    Pass : String;
    Port : Integer;
    PrpCompress,
    PrpTimeout,
    PrpDbless,
    PrpClientLocalFiles,
    PrpClientInteractive : String;
  end;
  PMysqlConnParams = ^TMysqlConnParams;

  // HeidiSQL internal param structure (as entered in connection form)
  TConnParams = record
    MysqlParams : TMysqlConnParams; // actual params sent to database server
    DatabaseList : String;
    DatabaseListSort : Boolean;
    Description : String;
    MysqlConn : TZConnection;
  end;
  PConnParams = ^TConnParams;

  TThreadResult = record
    ThreadID : Integer;
    ConnectionID : Integer;
    Action : Integer;
    Sql : String;
    Result : Integer;
    Comment : String;
  end;

  TMysqlConnectThread = class(TThread)
    private
    protected
    public
  end;

  TMysqlQueryThread = class(TThread)
    private
      FMysqlConnectionID : Integer;
      FMysqlConn : TZConnection;
      FConnParams : TConnParams;
      FOwner : TObject; // TMysqlQuery object
      FSql : String;
      FResult : Integer;
      FComment : String;
      FSyncMode : Integer;
      FNotifyWndHandle : THandle;
      procedure ReportInit;
      procedure ReportStart;
      procedure ReportFinished;
      procedure ReportFreed;
    protected
      procedure Execute; override;
      procedure SetState (AResult : Integer; AComment : String);
      procedure SetNotifyWndHandle (Value : THandle);
      procedure NotifyStatus (AEvent : Integer);
      procedure NotifyStatusViaEventProc (AEvent : Integer);
      procedure NotifyStatusViaWinMessage (AEvent : Integer);
      function AssembleResult () : TThreadResult;
      function RunDataQuery (ASql : String; var ADataset : TDataset) : Boolean;
      function RunUpdateQuery (ASql : String) : Boolean;
      function QuerySingleCellAsInteger (ASql : String) : Integer;
    public
      constructor Create (AOwner : TObject; APrm : TConnParams; ASql : String; ASyncMode : Integer);
      destructor Destroy; override;
      property NotifyWndHandle : THandle read FNotifyWndHandle write SetNotifyWndHandle;
  end;

implementation

uses
  MysqlQuery, SysUtils, Main, Dialogs, helpers
{$IFNDEF EXAMPLE_APP}
,communication
{$ENDIF}
  ;

function TMysqlQueryThread.AssembleResult: TThreadResult;
begin
  ZeroMemory (@Result,SizeOf(Result));

  Result.ThreadID := ThreadID;
  Result.ConnectionID := FMysqlConnectionID;
  Result.Action := 1;
  Result.Sql := FSql;
  Result.Result := FResult;
  Result.Comment := FComment;
end;

constructor TMysqlQueryThread.Create(AOwner : TObject; APrm : TConnParams; ASql : String; ASyncMode : Integer);
var
  mc : TZConnection;
begin
  Inherited Create(True);

  FOwner := AOwner;
  FConnParams := APrm;
  FSyncMode := ASyncMode;
  mc := TMysqlQuery(FOwner).MysqlConnection;
  FMysqlConn := mc;
  FResult := 0;
  FSql := ASql;

  mc.HostName := APrm.MysqlParams.Host;
  mc.Database := APrm.MysqlParams.Database;
  mc.User := APrm.MysqlParams.User;
  mc.Password := APrm.MysqlParams.Pass;
  mc.Protocol := APrm.MysqlParams.Protocol;
  mc.Port := APrm.MysqlParams.Port;

  FreeOnTerminate := True;

end;

destructor TMysqlQueryThread.Destroy;
begin
  inherited;
end;





procedure TMysqlQueryThread.NotifyStatus(AEvent: Integer);
var
  h : THandle;
begin

  // trigger query finished event
  if (FSyncMode=MQM_SYNC) and (AEvent=MQE_FREED) then
    begin
      h := OpenEvent (EVENT_ALL_ACCESS,False,PChar(TMysqlQuery(FOwner).EventName));

      if h<>0 then
        SetEvent (h);
    end;

  case TMysqlQuery(FOwner).NotificationMode of
    MQN_EVENTPROC:  NotifyStatusViaEventProc(AEvent);
    MQN_WINMESSAGE: NotifyStatusViaWinMessage(AEvent);
  end;

end;

procedure TMysqlQueryThread.NotifyStatusViaEventProc(AEvent: Integer);
begin
  if FSyncMode=MQM_ASYNC then
    begin
      case AEvent of
        MQE_INITED:     Synchronize(ReportInit);
        MQE_STARTED:    Synchronize(ReportStart);
        MQE_FINISHED:   Synchronize(ReportFinished);
        MQE_FREED:      Synchronize(ReportFreed);
      end;

    end;
end;

procedure TMysqlQueryThread.NotifyStatusViaWinMessage(AEvent: Integer);
begin

  PostMessage(FNotifyWndHandle,WM_MYSQL_THREAD_NOTIFY,Integer(FOwner),AEvent);
end;

procedure TMysqlQueryThread.Execute;
var
  q : TZQuery;
  r : Boolean;
begin
  NotifyStatus(MQE_INITED);

  try
    if not FMysqlConn.Connected then
      FMysqlConn.Connect();
  except
    SetState (MQR_CONNECT_FAIL,'Connect error');
  end;

  if FMysqlConn.Connected then
    begin
      FMysqlConnectionID := QuerySingleCellAsInteger('SELECT CONNECTION_ID()');

      NotifyStatus (MQE_STARTED);

      if ExpectResultSet(FSql) then
        begin
          q := nil;
          r := RunDataQuery (FSql,TDataSet(q));

          if r then
            begin
              if q.State=dsBrowse then
                begin
                  // WTF?
                end;
            end;

          TMysqlQuery(FOwner).SetMysqlDataset(q);
        end
      else
        r := RunUpdateQuery (FSql);

      if r then
        SetState (MQR_SUCCESS,'SUCCESS')
      else
        SetState (MQR_QUERY_FAIL,'ERROR');

    end;

  NotifyStatus (MQE_FINISHED);    
  NotifyStatus (MQE_FREED);
end;


function TMysqlQueryThread.QuerySingleCellAsInteger(ASql: String): Integer;
var
  ds : TDataSet;
begin
  Result := 0;

  if RunDataQuery(ASql,ds) then
    begin
      if ds.Fields.Count > 0 then
        Result := ds.Fields[0].AsInteger;
      FreeAndNil (ds);
    end;
end;

procedure TMysqlQueryThread.ReportStart();
var
  qr : TThreadResult;
begin
  qr := AssembleResult();

  if FOwner <> nil then
    TMysqlQuery (FOwner).PostNotification(qr,MQE_STARTED);
end;

procedure TMysqlQueryThread.ReportFinished();
var
  qr : TThreadResult;
begin
  qr := AssembleResult();

  if FOwner <> nil then
    TMysqlQuery (FOwner).PostNotification(qr,MQE_FINISHED);
end;

procedure TMysqlQueryThread.ReportInit();
var
  qr : TThreadResult;
begin
  qr := AssembleResult();

  if FOwner <> nil then
    TMysqlQuery (FOwner).PostNotification(qr,MQE_INITED);
end;

procedure TMysqlQueryThread.ReportFreed;
var
  qr : TThreadResult;
begin
  qr := AssembleResult();

  if FOwner <> nil then
    TMysqlQuery (FOwner).PostNotification(qr,MQE_FREED);
end;

function TMysqlQueryThread.RunDataQuery(ASql: String;
  var ADataset: TDataset): Boolean;
var
  q : TZQuery;
begin
  Result := False;
  q := TZQuery.Create(nil);
  q.Connection := FMysqlConn;
  q.SQL.Text := ASql;

  try
    q.Active := True;
    ADataset := q;
    Result := True;
  except
    // EZSQLException
  end;
end;

function TMysqlQueryThread.RunUpdateQuery(ASql: String): Boolean;
var
  q : TZQuery;
begin
  Result := False;
  q := TZQuery.Create(nil);
  q.Connection := FMysqlConn;
  q.SQL.Text := ASql;

  try
    q.ExecSQL();
    Result := True;
  except
    On E: Exception do
      MessageDlg( 'SQL Error: '+ CRLF + E.Message, mtError, [mbOK], 0 );
  end;

  FreeAndNil (q);
end;

procedure TMysqlQueryThread.SetNotifyWndHandle(Value: THandle);
begin
  FNotifyWndHandle := Value;
end;

procedure TMysqlQueryThread.SetState(AResult: Integer; AComment: String);
begin
  FResult := AResult;
  FComment := AComment;
end;

end.
