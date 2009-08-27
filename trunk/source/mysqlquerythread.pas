unit MysqlQueryThread;

interface

uses
  Windows, Messages, Forms, Db, Classes, ZConnection, ZDataSet, StdCtrls, SysUtils,
  ZMessages,
  HeidiComp, SynRegExpr, mysql_structures;

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
    NetType: Integer;
    Host: String;
    Database: WideString;
    Protocol,
    User,
    Pass : String;
    Port : Integer;
    PrpCompress,
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
    MysqlConn : TZConnection;
  end;
  POpenConnProf = ^TOpenConnProf;

  TThreadResult = record
    ThreadID : Integer;
    Action : Integer;
    Sql : WideString;
    Result : Integer;
    Comment : String;
  end;

  TMysqlQueryThread = class(TThread)
    private
      FMysqlConn : TZConnection;
      FConn : TOpenConnProf;
      FOwner : TObject; // TMysqlQuery object
      FSql : WideString;
      FCallback: TAsyncPostRunner;
      FPostDataSet: TDeferDataSet;
      FResult : Integer;
      FComment : String;
      FNotifyWndHandle : THandle;
      function  GetExceptionData(AException : Exception) : TExceptionData;
    protected
      procedure Execute; override;
      procedure SetState (AResult : Integer; AComment : String);
      procedure SetNotifyWndHandle (Value : THandle);
      procedure NotifyStatus (AEvent : Integer);
      procedure NotifyStatusViaWinMessage (AEvent : Integer);
      function AssembleResult () : TThreadResult;
      function RunDataQuery (ASql : WideString; var ADataset : TDataset; out AExceptionData : TExceptionData; callback: TAsyncPostRunner) : Boolean;
      function RunUpdateQuery (ASql : WideString; var ADataset : TDataset; out AExceptionData : TExceptionData; callback: TAsyncPostRunner) : Boolean;
      function QuerySingleCellAsInteger (ASql : WideString) : Integer;
    public
      constructor Create (AOwner : TObject; AConn : TOpenConnProf; ASql : WideString; Callback: TAsyncPostRunner; APostDataSet: TDeferDataSet);
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
  Result.Action := 1;
  Result.Sql := FSql;
  Result.Result := FResult;
  Result.Comment := FComment;
end;

constructor TMysqlQueryThread.Create (AOwner : TObject; AConn : TOpenConnProf; ASql : WideString; Callback: TAsyncPostRunner; APostDataSet: TDeferDataSet);
var
  mc : TZConnection;
begin
  Inherited Create(True);

  FOwner := AOwner;
  FConn := AConn;
  FCallback := Callback;
  FPostDataSet := APostDataSet;
  mc := TMysqlQuery(FOwner).MysqlConnection;
  FMysqlConn := mc;
  FResult := 0;
  FSql := ASql;

  if AConn.MysqlParams.NetType = NETTYPE_TCPIP then begin
    mc.HostName := AConn.MysqlParams.Host;
    mc.SocketName := '';
  end else begin
    mc.HostName := '.';
    mc.SocketName := AConn.MysqlParams.Host;
  end;
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
    if not SetEvent (h) then raise Exception.Create(Format('Assertion failed: Error %d signaling event', [GetLastError]));
    CloseHandle(h);
  end;
  NotifyStatusViaWinMessage(AEvent);
end;

procedure TMysqlQueryThread.NotifyStatusViaWinMessage(AEvent: Integer);
begin
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

function TMysqlQueryThread.QuerySingleCellAsInteger(ASql: WideString): Integer;
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

function TMysqlQueryThread.RunDataQuery(ASql: WideString;
  var ADataset: TDataset; out AExceptionData : TExceptionData; callback: TAsyncPostRunner): Boolean;
var
  q : TDeferDataSet;
begin
  Result := False;
  q := TDeferDataSet.Create(nil, callback);
  q.Connection := FMysqlConn;
  // Parameter checking is used only in Insert Files, which has it's own TZQuery.
  q.ParamCheck := false;
  q.SQL.Clear;
  q.SQL.Add(ASql);
  ADataset := q;

  try
    q.Active := True;
    Result := True;
  except
    on E: Exception do begin
      if E.Message = SCanNotOpenResultSet then begin
        Result := true;
        FreeAndNil(ADataset);
      end else if MainForm.cancelling then begin
        AExceptionData := GetExceptionData(Exception.Create('Cancelled by user.'));
        try
          FMysqlConn.Reconnect;
        except
        end;
        MainForm.cancelling := false;
      end else begin
        AExceptionData := GetExceptionData(E);
      end;
    end;
  end;
end;

function TMysqlQueryThread.RunUpdateQuery(ASql: WideString; var ADataset: TDataset; out AExceptionData : TExceptionData; callback: TAsyncPostRunner): Boolean;
var
  q : TDeferDataSet;
begin
  Result := False;
  q := TDeferDataSet.Create(nil, callback);
  q.Connection := FMysqlConn;
  // Parameter checking is used only in Insert Files, which has it's own TZQuery.
  q.ParamCheck := false;
  q.SQL.Text := ASql;
  ADataSet := q;

  try
    q.DoAsyncExecSql();
    Result := True;
  except
    On E: Exception do begin
      if MainForm.cancelling then begin
        AExceptionData := GetExceptionData(Exception.Create('Cancelled by user.'));
        try
          FMysqlConn.Reconnect;
        except
        end;
        MainForm.cancelling := false;
      end else begin
        AExceptionData := GetExceptionData(E);
      end;
    end;
  end;

  FreeAndNil (q);
end;

procedure TMysqlQueryThread.SetNotifyWndHandle(Value: THandle);
begin
  FNotifyWndHandle := Value;
end;

procedure TMysqlQueryThread.SetState(AResult: Integer; AComment: String);
var
  rx: TRegExpr;
  msg: String;
begin
  debug(Format('qry: Setting status %d with comment %s', [AResult, AComment]));
  FResult := AResult;
  FComment := AComment;
  if FResult <> MQR_SUCCESS then begin
    // Find "(errno: 123)" in message and add more meaningful message from perror.exe
    rx := TRegExpr.Create;
    rx.Expression := '.+\(errno\:\s+(\d+)\)';
    if rx.Exec(FComment) then begin
      msg := MySQLErrorCodes.Values[rx.Match[1]];
      if msg <> '' then
        FComment := FComment + CRLF + CRLF + msg;
    end;
    rx.Free;
  end;
end;

end.
