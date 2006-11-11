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
      FSql : String;
      FForm : TForm;
      FValue : Integer;
      FComment : String;
      procedure ReportStart;
      procedure ReportFinish;
      procedure ReportKill;      
    protected
      procedure Execute; override;
      function RunDataQuery (ASql : String; var ADataset : TDataset) : Boolean;
      function RunUpdateQuery (ASql : String) : Boolean;
      function QuerySingleCellAsInteger (ASql : String) : Integer;
      function GetExpectResultSet (ASql : String) : Boolean;
    public
      constructor Create (APrm : TConnParams; ASql : String);
      destructor Destroy; override;
  end;

var
  TResultsetKeywords : array[0..1] of string[10] =
  (
    'SELECT',
    'SHOW'
  );

implementation

uses SysUtils, Main, Dialogs;

{ Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TMysqlQueryThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TMysqlQueryThread }

constructor TMysqlQueryThread.Create(APrm : TConnParams; ASql : String);
begin
  Inherited Create(True);

  FConnParams := APrm;
  FValue := 0;
  FSql := ASql;
  FMysqlConn := TZConnection.Create (nil);
  FMysqlConn.HostName := APrm.Host;
  FMysqlConn.Database := APrm.Database;
  FMysqlConn.User := APrm.User;
  FMysqlConn.Password := APrm.Pass;
  FMysqlConn.Protocol := APrm.Protocol;
  FMysqlConn.Port := APrm.Port;

  FreeOnTerminate := True;
  Resume;

end;

destructor TMysqlQueryThread.Destroy;
begin
  FreeAndNil (FMysqlConn);
  inherited;
end;





procedure TMysqlQueryThread.Execute;
var
  q : TZQuery;
  r : Boolean;
begin

  try
    FMysqlConn.Connect();
  except
    FComment := 'Connect error';
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
                  if q.Fields.Count > 0 then
                    FValue := q.RecordCount;
                end;
            end;

          FreeAndNil (q);
        end
      else
        r := RunUpdateQuery (FSql);

      if r then
        begin
          FComment := 'Query OK';
        end;

    end;

  FMysqlConn.Disconnect();

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
  ZeroMemory (@qr,SizeOf(qr));

  qr.ThreadID := ThreadID;
  qr.ConnectionID := FMysqlConnectionID;
  qr.Action := 1;
  qr.Sql := FSql;
  qr.Result := FValue;
  qr.Comment := 'Executing';

  TForm1(FConnParams.Form).UpdateThreadStatus(qr);
end;

procedure TMysqlQueryThread.ReportFinish();
var
  qr : TThreadResult;
begin
  ZeroMemory (@qr,SizeOf(qr));

  qr.ThreadID := ThreadID;
  qr.ConnectionID := FMysqlConnectionID;
  qr.Action := 2;
  qr.Sql := FSql;
  qr.Result := FValue;
  qr.Comment := FComment;

  TForm1(FConnParams.Form).UpdateThreadStatus(qr);
end;


procedure TMysqlQueryThread.ReportKill;
var
  qr : TThreadResult;
begin
  ZeroMemory (@qr,SizeOf(qr));

  qr.ThreadID := ThreadID;
  qr.ConnectionID := FMysqlConnectionID;
  qr.Action := 3;
  qr.Sql := FSql;
  qr.Result := FValue;
  qr.Comment := 'Killed';

  TForm1(FConnParams.Form).UpdateThreadStatus(qr);
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
    FComment := 'Query error';
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
    FComment := 'Query error';
  end;

  FreeAndNil (q);
end;

end.
