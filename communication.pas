unit communication;

(*
 Functions for Inter-Process Communication.
*)


interface

uses
  Windows,
  Threading,
  Classes,
  Messages;

const
  CMD_EXECUTEQUERY    = 1;
  CMD_GETDBLIST       = 2;
  RES_DBLIST          = 257;
  ERR_NOERROR         = 0;
  ERR_UNSPECIFIED     = 1;

(*
 Execute a query on a window.
*)
procedure RemoteExecQuery(window: THandle; query: string);

(*
 Execute a USE query on a window,
 given the version of the mysql server that window is connected to.
*)
procedure RemoteExecUseQuery(window: THandle; mysqlVersion: integer; dbName: string);

(*
 Get a list of databases from another window.
 Requestor identifies the local HeidiSQL window which wants the data.
*)
function RemoteGetDatabases(requestor: THandle; window: THandle): TStringList;

(*
 Fill in resulting data and return waiting thread to caller.
 Call from ProcessMessages() when a DB list message has been received.
*)
procedure FinishRemoteGetDbCommand(msg: TWMCopyData);

(*
 Slightly lame wrapper: Call to release a RemoteXXX SendMessage call on the "other side".
*)
procedure ReleaseRemoteCaller(errCode: integer);

(*
 Send a database list to a window.
*)
procedure SendDbListToRemote(window: THandle; requestor: THandle; sl: TStringList);

(*
 Extract a SQL query from a WM_COPYDATA message.
*)
function GetQueryFromMsg(msg: TWMCopyData): string;

(*
 Extract a THandle from a WM_COPYDATA message.
*)
function GetRequestorFromMsg(msg: TWMCopyData): THandle;


implementation

uses
  main,
  Helpers,
  SysUtils;

type
  TCopyDataStruct = packed record
    dwData: LongWord; // up to 32 bits of data to be passed to the receiving application.
    cbData: LongWord; // the size, in bytes, of the data pointed to by the lpData member.
    lpData: Pointer;  // points to data to be passed to the receiving application. This member can be nil.
  end;
  TResult = class
    private
      req: THandle;
      res: TObject;
    public
      property GetRequestor: THandle read req;
      property GetResult: TObject read res;
      constructor Create(const Requestor: THandle; const Result: TObject);
  end;

var
  results: TList;


constructor TResult.Create(const Requestor: THandle; const Result: TObject);
begin
  self.req := Requestor;
  self.res := Result;
end;


procedure SendStringListToRemote(window: THandle; requestor: THandle; resType: integer; sl: TStringList);
var
  data: TCopyDataStruct;
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    debug(Format('ipc: Sending string list to window %d, requestor %d', [window, requestor]));
    ms.Write(requestor, sizeof(THandle));
    sl.SaveToStream(ms);
    data.dwData := resType;
    data.cbData := ms.Size;
    data.lpData := ms.Memory;
    SendMessage(window, WM_COPYDATA, MainForm.Handle, integer(@data));
  finally
    ms.free;
  end;
end;


function GetQueryFromMsg(msg: TWMCopyData): string;
begin
  result := PChar(msg.CopyDataStruct^.lpData);
end;


function GetRequestorFromMsg(msg: TWMCopyData): THandle;
var
  req: THandle;
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    with msg.CopyDataStruct^ do begin
      ms.Write(lpData^, cbData);
      ms.Position := 0;
      ms.Read(req, sizeof(THandle));
      debug(Format('ipc: Received query from window %d, requestor %d', [msg.From, req]));
      result := req;
    end;
  finally
    ms.free;
  end;
end;


function GetDbListFromMsg(msg: TWMCopyData): TResult;
var
  sl: TStringList;
  req: THandle;
  ms: TMemoryStream;
begin
  sl := TStringlist.Create;
  ms := TMemoryStream.Create;
  try
    with msg.CopyDataStruct^ do begin
      ms.Write(lpData^, cbData);
      ms.Position := 0;
      ms.Read(req, sizeof(THandle));
      sl.LoadFromStream(ms);
      result := TResult.Create(req, sl);
    end;
  finally
    ms.free;
  end;
end;


procedure RemoteExecQuery(window: THandle; query: string);
var
  data: TCopyDataStruct;
  err: integer;
begin
  data.dwData := CMD_EXECUTEQUERY;
  // +1 since we need to send the terminating #0 as well
  data.cbData := Length(query) + 1;
  data.lpData := PChar(query);
  SendMessage(window, WM_COPYDATA, MainForm.Handle, integer(@data));
  if err <> 0 then Exception.CreateFmt('Remote returned error %d when asked to execute query', [err]);
end;


procedure RemoteExecUseQuery(window: THandle; mysqlVersion: integer; dbName: string);
begin
  RemoteExecQuery(window, 'USE ' + maskSql(mysqlVersion, dbName));
end;


function RemoteGetDatabases(requestor: THandle; window: THandle): TStringList;
var
  data: TCopyDataStruct;
  err: integer;
  i: integer;
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(requestor, sizeof(THandle));
    data.cbData := ms.Size;
    data.lpData := ms.Memory;
    data.dwData := CMD_GETDBLIST;
    err := SendMessage(window, WM_COPYDATA, MainForm.Handle, integer(@data));
    if err <> 0 then begin
      raise Exception.CreateFmt('Remote returned error %d when asked for list of databases.', [err]);
    end else begin
      debug(Format('ipc: Requestor %d waiting for completion of remote get db list.', [requestor]));
      WaitForCompletion(requestor);
      for i := 0 to Results.Count - 1 do begin
        if TResult(results[i]).GetRequestor = requestor then begin
          result := TStringList(TResult(results[i]).GetResult);
          results.Delete(i);
        end;
      end;
    end;
  finally
    ms.free;
  end;
end;


procedure FinishRemoteGetDbCommand(msg: TWMCopyData);
var
  res: TResult;
begin
  res := GetDbListFromMsg(msg);
  results.Add(res);
  debug(Format('ipc: Remote db call finished, releasing requestor %d.', [res.GetRequestor]));
  NotifyComplete(res.GetRequestor);
end;


procedure ReleaseRemoteCaller(errCode: integer);
begin
  // reply to the message so the clients thread is unblocked
  ReplyMessage(errCode);
end;


procedure SendDbListToRemote(window: THandle; requestor: THandle; sl: TStringList);
begin
  SendStringListToRemote(window, requestor, RES_DBLIST, sl);
end;



initialization
  results := TList.Create;

end.

