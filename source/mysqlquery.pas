unit MysqlQuery;

interface

uses Windows, Messages, Classes, Db, ZConnection, ZDataSet, MysqlQueryThread, helpers;

const
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
      FConn : TOpenConnProf;
      FQueryResult : TThreadResult;
      FMysqlConnection : TZConnection;
      FMysqlConnectionIsOwned : Boolean;
      FMysqlDataset : TDataset;
      FThreadID : Integer;
      FQueryThread : TMysqlQueryThread;
      FEventName : String;
      FEventHandle : THandle;
      FSql : WideString;
      function GetComment: String;
      function GetResult: Integer;
      function GetHasresultSet: Boolean;
    protected

    public
      constructor Create (AOwner : TComponent; AConn : POpenConnProf); overload;
      destructor Destroy (); override;
      procedure Query(ASql: WideString; ANotifyWndHandle : THandle; Callback: TAsyncPostRunner; ds: TDeferDataSet);
      procedure SetMysqlDataset(ADataset : TDataset);
      procedure PostNotification (AQueryResult : TThreadResult; AEvent : Integer);
      procedure SetThreadResult(AResult : TThreadResult);

      property Result : Integer read GetResult;                 // Query result code
      property Comment : String read GetComment;                // Textual information about the query result, includes error description
      property MysqlConnection : TZConnection read FMysqlConnection;
      property MysqlDataset : TDataset read FMysqlDataset;      // Resultset
      property HasResultset : Boolean read GetHasresultSet;     // Indicator of resultset availability
      property ThreadID : Integer read FThreadID;               // Mysql query thread ID (on the clients os)
      property Sql : WideString read FSql;                          // Query string
      property EventName : String read FEventName;              // Operating system event name used for blocking mode
      property EventHandle : THandle read FEventHandle;
  end;

  function ExecMysqlStatementAsync(ASql : WideString; AConn : TOpenConnProf; AWndHandle : THandle; Callback: TAsyncPostRunner) : TMysqlQuery;
  function ExecPostAsync(AConn : TOpenConnProf; AWndHandle : THandle; ds: TDeferDataSet): TMysqlQuery;


implementation

uses
  SysUtils,
  Dialogs;


{***
  Wrapper function to simplify running a query in asynchronous mode
  This function will end right after the thread is created.

  Otherwise status notifications are sent by the WM_MYSQL_THREAD_NOTIFY message;
  * use the WParam member of the AMessage parameter (a TMysqlQuery object)

  @param string SQL-statement
  @param TConnParams Connection credentials structure
  @param THandle Window handle to post thread status messages to
}
function ExecMysqlStatementAsync(ASql : WideString; AConn : TOpenConnProf; AWndHandle : THandle; Callback: TAsyncPostRunner) : TMysqlQuery;
begin
  Result := TMysqlQuery.Create(nil,@AConn);
  Result.Query(ASql,AWndHandle,Callback,nil);
end;


function ExecPostAsync(AConn : TOpenConnProf; AWndHandle : THandle; ds: TDeferDataSet): TMysqlQuery;
begin
  Result := TMysqlQuery.Create(nil,@AConn);
  Result.Query('',AWndHandle,nil,ds);
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

constructor TMysqlQuery.Create(AOwner: TComponent; AConn: POpenConnProf);
begin
  FConn := AConn^;
  FMysqlConnectionIsOwned := False;

  ZeroMemory (@FQueryResult,SizeOf(FQueryResult));
  FSql := '';

  if AConn.MysqlConn<>nil then
    FMysqlConnection := AConn.MysqlConn
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

function TMysqlQuery.GetComment: String;
begin
  Result := FQueryResult.Comment;
end;

function TMysqlQuery.GetHasresultSet: Boolean;
begin
  Result := FMysqlDataset <> nil;
end;

function TMysqlQuery.GetResult: Integer;
begin
  Result := FQueryResult.Result;
end;

procedure TMysqlQuery.PostNotification(AQueryResult: TThreadResult; AEvent : Integer);
begin
  SetThreadResult(AQueryResult);
  debug(Format('qry: Not calling notify function, event type %d occurred.', [AEvent]));
end;

procedure TMysqlQuery.Query(ASql: WideString; ANotifyWndHandle : THandle; Callback: TAsyncPostRunner; ds: TDeferDataSet);
begin
  // create thread object
  FQueryThread := TMysqlQueryThread.Create(Self,FConn,ASql,Callback,ds);
  FQueryThread.NotifyWndHandle := ANotifyWndHandle;
  FThreadID := FQueryThread.ThreadID;
  FEventName := APPNAME+'_'+IntToStr(FThreadID);
  FSql := ASql;

  FEventHandle := CreateEvent ({*EVENT_MODIFY_STATE + SYNCHRONIZE*}nil, False, False, PChar(FEventName));

  // exec query
  debug(Format('qry: Starting query thread %d', [FQueryThread.ThreadID]));
  FQueryThread.Resume();
end;


procedure TMysqlQuery.SetMysqlDataset(ADataset: TDataset);
begin
  FMysqlDataset := ADataset;
end;

procedure TMysqlQuery.SetThreadResult(AResult: TThreadResult);
begin
  try
    FQueryResult := AResult;
  except
    raise Exception.Create('Assertion failed: Internal error in SetThreadResult().');
  end;
end;

end.
