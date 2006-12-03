unit MysqlQuery;

interface

uses Windows, Messages, Classes, Db, ZConnection, ZDataSet, MysqlQueryThread;

const

  // execution mode
  MQM_SYNC = 0;
  MQM_ASYNC = 1;

  // notification mode
  MQN_EVENTPROC = 0;
  MQN_WINMESSAGE = 1;

  // query result
  MQR_NOTHING = 0; // no result yet
  MQR_SUCCESS = 1; // success
  MQR_CONNECT_FAIL = 2; // done with error
  MQR_QUERY_FAIL = 3; // done with error

  // notification events
  MQE_INITED = 0; // initialized
  MQE_STARTED = 1; // query started
  MQE_FINISHED = 2; // query finished
  MQE_FREED = 3; // object removed from memory

type
  TMysqlQuery = class;

  TMysqlQueryNotificationEvent = procedure (ASender : TMysqlQuery; AEvent : Integer) of object;

  TMysqlQuery = class
    private
      FConnParams : TConnParams;
      FQueryResult : TThreadResult;
      FMysqlConnection : TZConnection;
      FMysqlDataset : TDataset;
      FThreadID : Integer;
      FSyncMode : Integer;
      FQueryThread : TMysqlQueryThread;
      FEventName : String;
      FSql : String;
      FOnNotify : TMysqlQueryNotificationEvent;
      function GetNotificationMode: Integer;
      function GetConnectionID: Integer;
      function GetComment: String;
      function GetResult: Integer;
      function GetHasresultSet: Boolean;
    protected

    public
      FProgressForm : Pointer;
      constructor Create (AOwner : TComponent; AParams : PConnParams);
      destructor Destroy (); override;
      function Query (ASql : String; AMode : Integer = MQM_SYNC; ANotifyWndHandle : THandle = 0) : Integer;
      procedure SetMysqlDataset(ADataset : TDataset);
      procedure PostNotification (AQueryResult : TThreadResult; AEvent : Integer);
      property Result : Integer read GetResult;
      property Comment : String read GetComment;
      property ConnectionID : Integer read GetConnectionID;
      property MysqlConnection : TZConnection read FMysqlConnection;
      property MysqlDataset : TDataset read FMysqlDataset;
      property HasResultset : Boolean read GetHasresultSet;
      property ThreadID : Integer read FThreadID;
      property Sql : String read FSql;
      property EventName : String read FEventName;
      property NotificationMode : Integer read GetNotificationMode;
      property OnNotify : TMysqlQueryNotificationEvent read FOnNotify write FOnNotify;
  end;

  function ExecMysqlStatementAsync(ASql : String; AConnParams : TConnParams; ANotifyProc : TMysqlQueryNotificationEvent = nil; AWndHandle : THandle = 0) : TMysqlQuery;
  function ExecMysqlStatementBlocking(ASql : String; AConnParams : TConnParams; AWndHandle : THandle) : TMysqlQuery;

implementation

uses SysUtils, Dialogs;


function ExecMysqlStatementAsync(ASql : String; AConnParams : TConnParams; ANotifyProc : TMysqlQueryNotificationEvent; AWndHandle : THandle) : TMysqlQuery;
begin
  Result := TMysqlQuery.Create(nil,@AConnParams);
  Result.OnNotify := ANotifyProc;
  Result.Query(ASql,MQM_ASYNC,AWndHandle);
end;

function ExecMysqlStatementBlocking(ASql : String; AConnParams : TConnParams; AWndHandle : THandle) : TMysqlQuery;
begin
  Result := TMysqlQuery.Create(nil,@AConnParams);
  Result.Query(ASql,MQM_SYNC,AWndHandle);
end;



{ TMysqlQuery }

constructor TMysqlQuery.Create(AOwner: TComponent; AParams: PConnParams);
begin
  FConnParams := AParams^;
  ZeroMemory (@FQueryResult,SizeOf(FQueryResult));
  FSql := '';

  if AParams.MysqlConn<>nil then
    FMysqlConnection := AParams.MysqlConn
  else
    FMysqlConnection := TZConnection.Create(nil);

  FMysqlDataset := nil;
end;

destructor TMysqlQuery.Destroy;
begin
  //FreeAndNil (FMysqlDataset);
  FreeAndNil (FMysqlConnection);
  inherited;
end;

function TMysqlQuery.GetComment: String;
begin
  Result := FQueryResult.Comment;  
end;

function TMysqlQuery.GetConnectionID: Integer;
begin
  Result := FQueryResult.ConnectionID;
end;

function TMysqlQuery.GetHasresultSet: Boolean;
begin
  Result := FMysqlDataset <> nil;
end;

function TMysqlQuery.GetNotificationMode: Integer;
begin
  if Assigned(FOnNotify) then
    Result := MQN_EVENTPROC
  else
    Result := MQN_WINMESSAGE;
end;

function TMysqlQuery.GetResult: Integer;
begin
  Result := FQueryResult.Result;
end;

procedure TMysqlQuery.PostNotification(AQueryResult: TThreadResult; AEvent : Integer);
begin
  FQueryResult := AQueryResult;

  if FSyncMode=MQM_ASYNC then
    if AEvent in [MQE_INITED,MQE_STARTED,MQE_FINISHED,MQE_FREED] then  
      if Assigned(FOnNotify) then
        FOnNotify(Self,AEvent);
end;

function TMysqlQuery.Query(ASql: String; AMode: Integer; ANotifyWndHandle : THandle): Integer;
var
  EventHandle : THandle;
begin

  // create thread object
  FQueryThread := TMysqlQueryThread.Create(Self,FConnParams,ASql,AMode);
  FQueryThread.NotifyWndHandle := ANotifyWndHandle;
  FThreadID := FQueryThread.ThreadID;
  FEventName := 'HEIDISQL_'+IntToStr(FThreadID);
  FSyncMode := AMode;
  FSql := ASql;

  case AMode of
    MQM_SYNC:
      begin
        // create mutex
        EventHandle := CreateEvent (nil,False,False,PChar(FEventName));
      end;
    MQM_ASYNC:;
  end;

  // exec query
  FQueryThread.Resume();

  case AMode of
    MQM_SYNC:
      begin        
        WaitForSingleObject (EventHandle, INFINITE);
        CloseHandle (EventHandle);
        // read status
        // free thread
      end;
    MQM_ASYNC:;
  end;
end;

procedure TMysqlQuery.SetMysqlDataset(ADataset: TDataset);
begin
  FMysqlDataset := ADataset;
end;

end.
