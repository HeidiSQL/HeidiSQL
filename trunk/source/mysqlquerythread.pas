unit MysqlQueryThread;

interface

uses
  Windows, Messages, Forms, Db, Classes, ZConnection, ZDataSet, StdCtrls, SysUtils,
  ZMessages,
  HeidiComp;

{$IFDEF EXAMPLE_APP}
const
  WM_MYSQL_THREAD_NOTIFY = WM_USER+100;
{$ENDIF}

type
  // Exception information
  TExceptionData = record
    Msg : String[200];
    HelpContext : Integer;
  end;

  // Mysql protocol-relevant connection parameter structure
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

  // Established connection and it's corresponding connection profile.
  // (The actual connection need not necessarily be open, of course, it could in theory be closed or nil,
  //  but to keep the name short; this beats "TConnectionProfileDataAndConnectionObject", which I guess would be the proper name.
  TOpenConnProf = record
    MysqlParams : TMysqlConnParams; // stuff that needs to be shipped over to the mysql driver.
    DatabaseList : String;
    DatabaseListSort : Boolean;
    Description : String;
    MysqlConn : TZConnection;
  end;
  POpenConnProf = ^TOpenConnProf;

  TThreadResult = record
    ThreadID : Integer;
    ConnectionID : Cardinal;
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
      FMysqlConn : TZConnection;
      FConn : TOpenConnProf;
      FOwner : TObject; // TMysqlQuery object
      FSql : String;
      FCallback: TAsyncPostRunner;
      FPostDataSet: TDeferDataSet;
      FResult : Integer;
      FComment : String;
      FSyncMode : Integer;
      FNotifyWndHandle : THandle;
      procedure ReportInit;
      procedure ReportStart;
      procedure ReportFinished;
      procedure ReportFreed;
      function  GetExceptionData(AException : Exception) : TExceptionData;
    protected
      procedure Execute; override;
      procedure SetState (AResult : Integer; AComment : String);
      procedure SetNotifyWndHandle (Value : THandle);
      procedure NotifyStatus (AEvent : Integer);
      procedure NotifyStatusViaEventProc (AEvent : Integer);
      procedure NotifyStatusViaWinMessage (AEvent : Integer);
      function AssembleResult () : TThreadResult;
      function RunDataQuery (ASql : String; var ADataset : TDataset; out AExceptionData : TExceptionData; callback: TAsyncPostRunner) : Boolean;
      function RunUpdateQuery (ASql : String; var ADataset : TDataset; out AExceptionData : TExceptionData; callback: TAsyncPostRunner) : Boolean;
      function QuerySingleCellAsInteger (ASql : String) : Integer;
    public
      constructor Create (AOwner : TObject; AConn : TOpenConnProf; ASql : String; ASyncMode : Integer; Callback: TAsyncPostRunner; APostDataSet: TDeferDataSet);
      destructor Destroy; override;
      property NotifyWndHandle : THandle read FNotifyWndHandle write SetNotifyWndHandle;
  end;

implementation

uses
  MysqlQuery, Dialogs, helpers, communication
{$IFNDEF EXAMPLE_APP}
, Main
{$ENDIF}
  ;

function TMysqlQueryThread.AssembleResult: TThreadResult;
begin
  ZeroMemory (@Result,SizeOf(Result));

  Result.ThreadID := ThreadID;
  try
    Result.ConnectionID := FMysqlConn.GetThreadId;
  except
  end;
  Result.Action := 1;
  Result.Sql := FSql;
  Result.Result := FResult;
  Result.Comment := FComment;
end;

constructor TMysqlQueryThread.Create (AOwner : TObject; AConn : TOpenConnProf; ASql : String; ASyncMode : Integer; Callback: TAsyncPostRunner; APostDataSet: TDeferDataSet);
var
  mc : TZConnection;
begin
  Inherited Create(True);

  FOwner := AOwner;
  FConn := AConn;
  FSyncMode := ASyncMode;
  FCallback := Callback;
  FPostDataSet := APostDataSet;
  mc := TMysqlQuery(FOwner).MysqlConnection;
  FMysqlConn := mc;
  FResult := 0;
  FSql := ASql;

  mc.HostName := AConn.MysqlParams.Host;
  mc.Database := AConn.MysqlParams.Database;
  mc.User := AConn.MysqlParams.User;
  mc.Password := AConn.MysqlParams.Pass;
  mc.Protocol := AConn.MysqlParams.Protocol;
  mc.Port := AConn.MysqlParams.Port;

  FreeOnTerminate := True;

end;

destructor TMysqlQueryThread.Destroy;
begin
  inherited;
end;





procedure TMysqlQueryThread.NotifyStatus(AEvent: Integer);
var
  h : THandle;
  qr : TThreadResult;
begin

  if AEvent = MQE_FINISHED then begin
    debug(Format('qry: Setting result', [AEvent]));
    qr := AssembleResult();
    TMysqlQuery(FOwner).SetThreadResult(qr);
    // trigger query finished event
    h := OpenEvent (EVENT_MODIFY_STATE,False,PChar(TMysqlQuery(FOwner).EventName));
    debug('qry: Signalling completion via event.');
    if not SetEvent (h) then debug(Format('qry: Assertion failed: Error %d signaling event', [GetLastError]));
    CloseHandle(h);
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
  debug('qry: Signalling completion via message.');
  debug(Format('qry: Posting status %d via WM_MYSQL_THREAD_NOTIFY message', [AEvent]));
  PostMessage(FNotifyWndHandle,WM_MYSQL_THREAD_NOTIFY,Integer(FOwner),AEvent);
end;

procedure TMysqlQueryThread.Execute;
var
  q : TDeferDataSet;
  r : Boolean;
  ex : TExceptionData;
begin
  debug(Format('qry: Thread %d running...', [ThreadID]));
  NotifyStatus(MQE_INITED);

  try
    if not FMysqlConn.Connected then FMysqlConn.Connect();
  except
    on E: Exception do begin
      SetState (MQR_CONNECT_FAIL,Format('%s',[E.Message]));
    end;
  end;

  if FMysqlConn.Connected then begin
    NotifyStatus (MQE_STARTED);

    q := nil;
    if FPostDataSet <> nil then begin
      try
        FPostDataSet.DoAsync;
        SetState (MQR_SUCCESS,'SUCCESS')
      except
        on E: Exception do begin
          SetState (MQR_QUERY_FAIL,Format('%s', [E.Message]));
        end;
      end;
    end else begin
      try
        r := RunDataQuery (FSql,TDataSet(q),ex,FCallback);
        //if r then begin
          //if q.State=dsBrowse then begin
            // WTF?
          //end;
        //end;
        TMysqlQuery(FOwner).SetMysqlDataset(q);

        if r then SetState (MQR_SUCCESS,'SUCCESS')
        else SetState (MQR_QUERY_FAIL,ex.Msg);
      except
        on E: Exception do begin
          SetState (MQR_QUERY_FAIL,Format('%s', [E.Message]));
        end;
      end;
    end;
  end;

  NotifyStatus (MQE_FINISHED);
  NotifyStatus (MQE_FREED);
  debug(Format('qry: Thread %d suspending.', [ThreadID]));
end;


function TMysqlQueryThread.GetExceptionData(
  AException: Exception): TExceptionData;
begin
  ZeroMemory (@Result,SizeOf(Result));
  Result.Msg := AException.Message;
  Result.HelpContext := AException.HelpContext;
end;

function TMysqlQueryThread.QuerySingleCellAsInteger(ASql: String): Integer;
var
  ds : TDataSet;
  e : TExceptionData;
begin
  Result := 0;

  if RunDataQuery(ASql,ds,e, FCallback) then
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
  var ADataset: TDataset; out AExceptionData : TExceptionData; callback: TAsyncPostRunner): Boolean;
var
  q : TDeferDataSet;
begin
  Result := False;
  q := TDeferDataSet.Create(nil, callback);
  q.Connection := FMysqlConn;
  q.SQL.Text := ASql;
  ADataset := q;

  try
    q.Active := True;
    Result := True;
  except
    on E: Exception do begin
      if E.Message = SCanNotOpenResultSet then begin
        Result := true;
        FreeAndNil(ADataset);
      end else AExceptionData := GetExceptionData(E);
    end;
  end;
end;

function TMysqlQueryThread.RunUpdateQuery(ASql: String; var ADataset: TDataset; out AExceptionData : TExceptionData; callback: TAsyncPostRunner): Boolean;
var
  q : TDeferDataSet;
begin
  Result := False;
  q := TDeferDataSet.Create(nil, callback);
  q.Connection := FMysqlConn;
  q.SQL.Text := ASql;
  ADataSet := q;

  try
    q.DoAsyncExecSql();
    Result := True;
  except
    On E: Exception do
      AExceptionData := GetExceptionData(E);
      //MessageDlg( 'SQL Error: '+ CRLF + E.Message, mtError, [mbOK], 0 );
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
