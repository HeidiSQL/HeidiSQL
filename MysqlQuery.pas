unit MysqlQuery;

interface

uses Windows, Messages, Classes, Db, ZConnection, ZDataSet, MysqlQueryThread;

const

  // Query execution mode
  MQM_SYNC = 0;
  MQM_ASYNC = 1;

  // Query status notification mode
  MQN_EVENTPROC = 0;  // via event procedure with Synchronize
  MQN_WINMESSAGE = 1; // via window message WM_MYSQL_THREAD_NOTIFY

  // Thread notification events
  MQE_INITED = 0;       // initialized
  MQE_STARTED = 1;      // query started
  MQE_FINISHED = 2;     // query finished
  MQE_FREED = 3;        // object removed from memory

  // Query result codes
  MQR_NOTHING = 0;      // no result yet
  MQR_SUCCESS = 1;      // success
  MQR_CONNECT_FAIL = 2; // done with error
  MQR_QUERY_FAIL = 3;   // done with error

{$I const.inc}

type
  TMysqlQuery = class;

  TMysqlQueryNotificationEvent = procedure (ASender : TMysqlQuery; AEvent : Integer) of object;

  TMysqlQuery = class
    private
      FConnParams : TConnParams;
      FQueryResult : TThreadResult;
      FMysqlConnection : TZConnection;
      FMysqlConnectionIsOwned : Boolean;
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
      constructor Create (AOwner : TComponent; AParams : PConnParams);
      destructor Destroy (); override;
      procedure Query (ASql : String; AMode : Integer = MQM_SYNC; ANotifyWndHandle : THandle = 0);
      procedure SetMysqlDataset(ADataset : TDataset);
      procedure PostNotification (AQueryResult : TThreadResult; AEvent : Integer);
      procedure SetThreadResult(AResult : TThreadResult);

      property Result : Integer read GetResult;                 // Query result code
      property Comment : String read GetComment;                // Textual information about the query result, includes error description
      property ConnectionID : Integer read GetConnectionID;     // Mysql connection ID
      property MysqlConnection : TZConnection read FMysqlConnection;
      property MysqlDataset : TDataset read FMysqlDataset;      // Resultset
      property HasResultset : Boolean read GetHasresultSet;     // Indicator of resultset availability
      property ThreadID : Integer read FThreadID;               // Mysql query thread ID (on the clients os)
      property Sql : String read FSql;                          // Query string
      property EventName : String read FEventName;              // Operating system event name used for blocking mode
      property NotificationMode : Integer read GetNotificationMode;
      property OnNotify : TMysqlQueryNotificationEvent read FOnNotify write FOnNotify; // Event procedure used in MQN_EVENTPROC notification mode
  end;

  function ExecMysqlStatementAsync(ASql : String; AConnParams : TConnParams; ANotifyProc : TMysqlQueryNotificationEvent = nil; AWndHandle : THandle = 0) : TMysqlQuery;
  function ExecMysqlStatementBlocking(ASql : String; AConnParams : TConnParams; AWndHandle : THandle) : TMysqlQuery;


implementation

uses SysUtils, Dialogs;


{***
  Wrapper function to simplify running a query in asynchronous mode
  This function will end right after the thread is created.

  If ANotifyProc<>nil then status notifications will be send using this eventproc;
  * use the ASender param to inspect the result

  Otherwise status notifications are sent by the WM_MYSQL_THREAD_NOTIFY message;
  * use the WParam member of the AMessage parameter (a TMysqlQuery object)

  @param string SQL-statement
  @param TConnParams Connection credentials structure
  @param TMysqlQueryNotificationEvent Notify procedure
  @param THandle Window handle to post thread status messages to
}
function ExecMysqlStatementAsync(ASql : String; AConnParams : TConnParams; ANotifyProc : TMysqlQueryNotificationEvent; AWndHandle : THandle) : TMysqlQuery;
begin
  Result := TMysqlQuery.Create(nil,@AConnParams);
  Result.OnNotify := ANotifyProc;
  Result.Query(ASql,MQM_ASYNC,AWndHandle);
end;


{***
  Wrapper function to simplify running a query in blocking mode
  Status notifications are sent by the WM_MYSQL_THREAD_NOTIFY message;
  Use the WParam member of the AMessage parameter (a TMysqlQuery object)

  @param string The single SQL-statement to be executed
  @param TConnParams Connection credentials structure
  @param THandle Window handle to post thread status messages to
  @return TMysqlQuery Query object returned to resolve status info and the data
}

function ExecMysqlStatementBlocking(ASql : String; AConnParams : TConnParams; AWndHandle : THandle) : TMysqlQuery;
begin
  Result := TMysqlQuery.Create(nil,@AConnParams);
  Result.Query(ASql,MQM_SYNC,AWndHandle);
end;



{ TMysqlQuery }

{***
  Constructor

  @param TComponent Owner
  @param PConnParams Used to pass connection credentials. If the MysqlConn member
          (a TZConnection object) <>nil, the query will be run on this connection.
          Otherwise a new connection object is created with the credentials
          in the MysqlParams member
}

constructor TMysqlQuery.Create(AOwner: TComponent; AParams: PConnParams);
begin
  FConnParams := AParams^;
  FMysqlConnectionIsOwned := False;

  ZeroMemory (@FQueryResult,SizeOf(FQueryResult));
  FSql := '';

  if AParams.MysqlConn<>nil then
    FMysqlConnection := AParams.MysqlConn
  else
    begin
      FMysqlConnectionIsOwned := True;
      FMysqlConnection := TZConnection.Create(nil);
    end;

  FMysqlDataset := nil;
end;


{***
  Destructor:
    remove created objects from memory

  @result
}

destructor TMysqlQuery.Destroy;
begin
  //FreeAndNil (FMysqlDataset);

  // Only free the connection object if we first created it
  if FMysqlConnectionIsOwned then
    FreeAndNil (FMysqlConnection);

  inherited;
end;


{***

  @param
  @param

  @result
}

function TMysqlQuery.GetComment: String;
begin
  Result := FQueryResult.Comment;  
end;


{***

  @param
  @param

  @result
}

function TMysqlQuery.GetConnectionID: Integer;
begin
  Result := FQueryResult.ConnectionID;
end;


{***

  @param
  @param

  @result
}

function TMysqlQuery.GetHasresultSet: Boolean;
begin
  Result := FMysqlDataset <> nil;
end;



{***

  @param
  @param

  @result
}

function TMysqlQuery.GetNotificationMode: Integer;
begin
  if Assigned(FOnNotify) then
    Result := MQN_EVENTPROC
  else
    Result := MQN_WINMESSAGE;
end;


{***

  @param
  @param

  @result
}

function TMysqlQuery.GetResult: Integer;
begin
  Result := FQueryResult.Result;
end;


{***

  @param
  @param

  @result
}

procedure TMysqlQuery.PostNotification(AQueryResult: TThreadResult; AEvent : Integer);
begin
  SetThreadResult(AQueryResult);

  if FSyncMode=MQM_ASYNC then
    if AEvent in [MQE_INITED,MQE_STARTED,MQE_FINISHED,MQE_FREED] then  
      if Assigned(FOnNotify) then
        FOnNotify(Self,AEvent);
end;


{***

  @param
  @param

  @result
}

procedure TMysqlQuery.Query(ASql: String; AMode: Integer; ANotifyWndHandle : THandle);
var
  EventHandle : THandle;
begin

  // create thread object
  FQueryThread := TMysqlQueryThread.Create(Self,FConnParams,ASql,AMode);
  FQueryThread.NotifyWndHandle := ANotifyWndHandle;
  FThreadID := FQueryThread.ThreadID;
  FEventName := APPNAME+'_'+IntToStr(FThreadID);
  FSyncMode := AMode;
  FSql := ASql;

  case AMode of
    MQM_SYNC:
      begin
        // create mutex
        EventHandle := CreateEvent (nil,False,False,PChar(FEventName));
        // exec query
        FQueryThread.Resume();
        WaitForSingleObject (EventHandle, INFINITE);
        CloseHandle (EventHandle);
        // read status
        // free thread
      end;
    MQM_ASYNC:
      begin
        // exec query
        FQueryThread.Resume();
      end;
  end;
end;


{***

  @param
  @param

  @result
}

procedure TMysqlQuery.SetMysqlDataset(ADataset: TDataset);
begin
  FMysqlDataset := ADataset;
end;

procedure TMysqlQuery.SetThreadResult(AResult: TThreadResult);
begin
  FQueryResult := AResult;
end;

end.
