unit MysqlQueryThread;

interface

uses
  Windows, Forms, Db, Classes, ZConnection, ZDataSet, StdCtrls;

type

  TConnParams = record
    Host,
    Database,
    Protocol,
    User,
    Pass : String;
    Port : Integer;
    Form : TForm;
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

  TMysqlQueryThread = class(TThread)
    private
      FMysqlConnectionID : Integer;
      FMysqlConn : TZConnection;
      FConnParams : TConnParams;
      FOwner : TObject; // TMysqlQuery object
      FSql : String;
      FForm : TForm;
      FResult : Integer; // result code
      FComment : String;
      procedure ReportInit; // result text
      procedure ReportStart;
      procedure ReportFinish;
      procedure ReportKill;
    protected
      procedure Execute; override;
      procedure SetState (AResult : Integer; AComment : String);
      function AssembleResult () : TThreadResult;
      function RunDataQuery (ASql : String; var ADataset : TDataset) : Boolean;
      function RunUpdateQuery (ASql : String) : Boolean;
      function QuerySingleCellAsInteger (ASql : String) : Integer;
      function GetExpectResultSet (ASql : String) : Boolean;
    public
      constructor Create (AOwner : TObject; APrm : TConnParams; ASql : String);
      destructor Destroy; override;
  end;

var
  TResultsetKeywords : array[0..1] of string[10] =
  (
    'SELECT',
    'SHOW'
  );

implementation

uses
  MysqlQuery, SysUtils, Main, Dialogs, Messages;

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

constructor TMysqlQueryThread.Create(AOwner : TObject; APrm : TConnParams; ASql : String);
var
  mc : TZConnection;
begin
  Inherited Create(True);

  FOwner := AOwner;
  FConnParams := APrm;
  mc := TMysqlQuery(FOwner).MysqlConnection;
  FMysqlConn := mc;
  FResult := 0;
  FSql := ASql;

  mc.HostName := APrm.Host;
  mc.Database := APrm.Database;
  mc.User := APrm.User;
  mc.Password := APrm.Pass;
  mc.Protocol := APrm.Protocol;
  mc.Port := APrm.Port;

  FreeOnTerminate := True;

end;

destructor TMysqlQueryThread.Destroy;
begin
  inherited;
end;





procedure TMysqlQueryThread.Execute;
var
  q : TZQuery;
  r : Boolean;
  h : THandle;
begin
  Synchronize(ReportInit);

  try
    FMysqlConn.Connect();
  except
    SetState (MQR_CONNECT_FAIL,'Connect error');
  end;

  r := False;
  q := nil;

  if FMysqlConn.Connected then
    begin
      FMysqlConnectionID := QuerySingleCellAsInteger('SELECT CONNECTION_ID()');

      Synchronize(ReportStart);

      if GetExpectResultSet(FSql) then
        begin
          r := RunDataQuery (FSql,TDataSet(q));

          if r then
            begin
              if q.State=dsBrowse then
                begin
                  //if q.Fields.Count > 0 then
                    //FValue := q.RecordCount;
                end;
            end;

          TMysqlQuery(FOwner).SetMysqlDataset(q);
          //FreeAndNil (q);
        end
      else
        r := RunUpdateQuery (FSql);

      if r then
        SetState (MQR_SUCCESS,'SUCCESS')
      else
        SetState (MQR_QUERY_FAIL,'ERROR');

    end;

  h := OpenEvent (EVENT_ALL_ACCESS,False,PChar(TMysqlQuery(FOwner).EventName));
  if h<>0 then
    SetEvent (h);

  Synchronize(ReportFinish);
  Sleep (5000);
  Synchronize(ReportKill);
end;

function TMysqlQueryThread.GetExpectResultSet(ASql: String): Boolean;
var
  p : Integer;
  kw : String;
  i : Integer;
begin
  Result := False;

  if ASql<>'' then
    begin
      p := Pos (' ',ASql);
      if p > 0 then
        begin
          kw := Copy (ASql,0,p-1);

          for i := Low(TResultsetKeywords) to High(TResultsetKeywords) do
            if UpperCase(kw) = TResultsetKeywords[i] then
              Result := True;

        end;

    end;
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

procedure TMysqlQueryThread.ReportFinish();
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

procedure TMysqlQueryThread.ReportKill;
var
  qr : TThreadResult;
begin
  qr := AssembleResult();

  //if FOwner <> nil then
    //TMysqlQuery (FOwner).PostNotification(qr,MQE_KILLED);
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
  end;

  FreeAndNil (q);
end;

procedure TMysqlQueryThread.SetState(AResult: Integer; AComment: String);
begin
  FResult := AResult;
  FComment := AComment;
end;

end.
