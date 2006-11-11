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

  TMysqlQueryThread = class(TThread)
    private
      FMysqlProcessID : Integer;
      FMysqlConn : TZConnection;
      FConnParams : TConnParams;
      FSql : String;
      FForm : TForm;
      FValue : Integer;
    protected
      procedure Execute; override;
      function ExecSelectQuery (ASql : String; var ADataset : TDataset) : Boolean;
    public
      procedure Report ();
      constructor Create (APrm : TConnParams; ASql : String);
      destructor Destroy; override;
  end;

implementation

uses SysUtils, Main;

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
  FMysqlConn.Connect();
  

  FreeOnTerminate := True;
  Resume;

  FMysqlConn.Disconnect();
end;

destructor TMysqlQueryThread.Destroy;
begin
  FreeAndNil (FMysqlConn);
  inherited;
end;


function TMysqlQueryThread.ExecSelectQuery(ASql: String;
  var ADataset: TDataset): Boolean;
var
  q : TZQuery;
begin
  q := TZQuery.Create(nil);
  q.Connection := FMysqlConn;
  q.SQL.Text := FSql;

  try
    q.Active := True;

    ADataset := q;

    Result := True;
  except

  end;
end;


procedure TMysqlQueryThread.Execute;
var
  q : TDataset;
begin
  // todo: get connection id
  // FMysqlConn := SELECT CONNECTION_ID();


  if ExecSelectQuery (FSql,q) then
    if q.Fields.Count > 0 then
      FValue := q.Fields[0].Value;
  
  FreeAndNil (q);
  Synchronize(Report);
end;

procedure TMysqlQueryThread.Report();
begin
  TForm1(FConnParams.Form).LogMsg(Format ('Thread %d finished; Result: %d; Sql: %s',[Self.ThreadID,FValue,FSql]));
end;


end.
