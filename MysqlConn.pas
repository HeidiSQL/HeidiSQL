unit MysqlConn;

interface

uses ZConnection, ExtCtrls, MysqlQueryThread;

const
  // connection attemp result codes
  MCR_SUCCESS = 0;
  MCR_FAILED = 1;

type


  TMysqlConn = class
    private
      FConn : TZConnection;
      FMysqlConnParams : TMysqlConnParams;
      FLastError : String;
    function GetDescription: String;
    function GetIsAlive: Boolean;
    function GetIsConnected: Boolean;
      //FTimer : TTimer;
    public
      constructor Create(AParams : PMysqlConnParams);
      destructor Destroy(); override;
      function Connect() : Integer;
      property IsConnected : Boolean read GetIsConnected;
      property IsAlive : Boolean read GetIsAlive;
      property Connection : TZConnection read FConn;
      property LastError : String read FLastError;
      property Description : String read GetDescription;
  end;

implementation

uses SysUtils;

{ TMysqlConn }

constructor TMysqlConn.Create;
begin
  FConn := TZConnection.Create(nil);
  FMysqlConnParams := AParams^;
  FLastError := '';
end;

function TMysqlConn.Connect(): Integer;
begin
  FLastError := '';

  with FMysqlConnParams do
    begin
      FConn.Protocol := 'mysql';
      FConn.Hostname := Host;
      FConn.User := User;
      FConn.Password := Pass;
      FConn.Port := Port;

      FConn.Properties.Values['compress'] := PrpCompress;
      FConn.Properties.Values['timeout'] := PrpTimeout;
      FConn.Properties.Values['dbless'] := PrpDbless;
      FConn.Properties.Values['CLIENT_LOCAL_FILES'] := PrpClientLocalFiles;
      FConn.Properties.Values['CLIENT_INTERACTIVE'] := PrpClientInteractive;
      // ZConn.Properties.Values['USE_RESULT'] := 'true'; // doesn't work
      // ZConn.Properties.Values['CLIENT_SSL'] := 'true'; // from an mdaems's example
    end;

  try
    FConn.Connect();
    Result := MCR_SUCCESS;
  except
    // todo: handle exception
    on E : Exception do
      begin
        FLastError := E.Message;
        Result := MCR_FAILED;
      end;
  end;
end;



destructor TMysqlConn.Destroy;
begin
  FreeAndNil (FConn);
  inherited;
end;

function TMysqlConn.GetDescription: String;
begin
  with FMysqlConnParams do
    Result := Format ('%s@%s:%d',[User,Host,Port]);
end;

function TMysqlConn.GetIsAlive: Boolean;
begin
  Result := False;

  if IsConnected then try
    Result := FConn.Ping();
  except
    Result := False;
  end;
end;

function TMysqlConn.GetIsConnected: Boolean;
begin
  Result := FConn.Connected;
end;



end.
