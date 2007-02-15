unit communication;

(*
 Functions for Inter-Process Communication.

 Note about WM_COPYDATA:
 This message must be sent synchronously with SendMessage,
 cannot be sent asynchronously using PostMessage.  I think
 the reason is related to the idea behind WM_COPYDATA causing
 Windows to do a context switch to the receiving application..
*)


interface

uses
  Db,
  Windows,
  Threading,
  Classes,
  Messages;

const
  // Our custom message types.
  WM_COMPLETED        = WM_APP + 1;
  WM_GETDBLIST        = WM_APP + 2;
  WM_GETDBMSVERSION   = WM_APP + 3;
  // Our message subtypes for WM_COPYDATA messages.
  CMD_EXECUTEQUERY    = 1;
  RES_DBLIST          = 257;
  // Our SendMessage return codes.
  ERR_NOERROR         = 0;
  ERR_UNSPECIFIED     = 1;
  ERROR_NOT_CONNECTED = 0;

  // Sent by TMysqlQueryThread to notify status
  WM_MYSQL_THREAD_NOTIFY = WM_USER+100;

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
 Gets the MySQL version from another window.
*)
function RemoteGetVersion(window: THandle): integer;

(*
 Get a list of databases from another window.
*)
function RemoteGetDatabases(handler: TCompletionHandler; timeout: Cardinal; window: THandle): Cardinal;

(*
 Fill in resulting data and return waiting thread to caller.
 Call from message handler when a DB list message has been received.
*)
procedure FinishRemoteGetDbCommand(msg: TWMCopyData);

(*
 Slightly lame wrapper: Call to release a RemoteXXX SendMessage call on the "other side".
*)
procedure ReleaseRemoteCaller(errCode: integer);

(*
 Send a database list to a window.
*)
procedure SendDbListToRemote(window: THandle; request: Cardinal; ds: TDataSet);

(*
 Extract a SQL query from a WM_COPYDATA message.
*)
function GetQueryFromMsg(msg: TWMCopyData): string;

(*
 Extract a THandle from a WM_COPYDATA message.
*)
function GetRequestIdFromMsg(msg: TWMCopyData): Cardinal;

(*
 Helper which will handle WM_COMPLETED messages received.
*)
procedure HandleWMCompleteMessage(var msg: TMessage);

(*
 Helper which will handle WM_GETDBMSVERSION messages received.
*)
procedure HandleWMGetDbmsVersionMessage(var msg: TMessage);

(*
 Helper which will handle WM_GETDBLIST messages received.
*)
procedure HandleWMGetDbListMessage(var msg: TMessage);

(*
 Helper which will handle WM_COPYDATA messages received.
*)
procedure HandleWMCopyDataMessage(var msg: TWMCopyData);


implementation

uses
  AdoDb,
  AdoInt,
  ActiveX,
  main,
  childwin,
  Helpers,
  SysUtils;

type
  TCopyDataStruct = packed record
    dwData: LongWord; // up to 32 bits of data to be passed to the receiving application.
    cbData: LongWord; // the size, in bytes, of the data pointed to by the lpData member.
    lpData: Pointer;  // points to data to be passed to the receiving application. This member can be nil.
  end;


function CopyDataSetToAdoDataSet(src: TDataSet): TAdoDataSet;
var
  dst: TAdoDataSet;
  i: Integer;
begin
  dst := TAdoDataSet.Create(nil);
  dst.FieldDefs.Assign(src.FieldDefs);
  dst.CreateDataSet;
  src.First;
  while not src.Eof do begin
    dst.Append;
    for i := 0 to dst.FieldCount - 1 do begin
      dst.Fields[i].Assign(src.Fields[i]);
    end;
    dst.Post;
    src.Next;
  end;
  result := dst;
end;


procedure SendDatasetToRemote(window: THandle; request: Cardinal; resType: integer; ds: TDataSet);
var
  adods: TAdoDataSet;
  data: TCopyDataStruct;
  ms: TMemoryStream;
  sa: TStreamAdapter;
  olevar: OleVariant;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(request, sizeof(THandle));
    adods := CopyDataSetToAdoDataSet(ds);
    sa := TStreamAdapter.Create(ms);
    olevar := adods.Recordset;
    olevar.Save(sa as IStream, 0);
    debug(Format('ipc: Sending data set to window %d, request id %d, size %d', [window, request, ms.Size]));
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


function GetRequestIdFromMsg(msg: TWMCopyData): Cardinal;
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
      result := req;
    end;
  finally
    ms.free;
  end;
end;


function GetDataSetFromMsg(msg: TWMCopyData): TDataSet;
var
  adods: TAdoDataSet;
  ms: TMemoryStream;
  sa: TStreamAdapter;
  olevar: OleVariant;
begin
  ms := TMemoryStream.Create;
  try
    with msg.CopyDataStruct^ do begin
      ms.Write(lpData^, cbData);
      ms.Position := sizeof(THandle);
      sa := TStreamAdapter.Create(ms);
      olevar := CoRecordset.Create;
      olevar.Open(sa as IStream);
      adods := TAdoDataSet.Create(nil);
      adods.Recordset := IUnknown(olevar) as _Recordset;
      result := adods;
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
  data.cbData := Length(query) + 1;
  data.lpData := PChar(query);
  err := SendMessage(window, WM_COPYDATA, MainForm.Handle, integer(@data));
  if err <> 0 then Exception.CreateFmt('Remote returned error %d when asked to execute query', [err]);
end;


procedure RemoteExecUseQuery(window: THandle; mysqlVersion: integer; dbName: string);
begin
  RemoteExecQuery(window, 'USE ' + maskSql(mysqlVersion, dbName));
end;


function RemoteGetVersion(window: THandle): integer;
var
  err: integer;
begin
  err := SendMessage(window, WM_GETDBMSVERSION, MainForm.Handle, 0);
  if err = ERROR_NOT_CONNECTED then Exception.Create('Remote is not connected to a server');
  // Result piggy-backed on error code.  Lazy.
  result := err;
end;


function RemoteGetDatabases(handler: TCompletionHandler; timeout: Cardinal; window: THandle): Cardinal;
var
  ms: TMemoryStream;
  req: Cardinal;
begin
  ms := TMemoryStream.Create;
  try
    req := SetCompletionHandler(handler, timeout);
    result := req;
    PostMessage(window, WM_GETDBLIST, MainForm.Handle, req);
    debug(Format('ipc: Remote db call started for request id %d.', [req]));
  finally
    ms.free;
  end;
end;


procedure FinishRemoteGetDbCommand(msg: TWMCopyData);
var
  res: TDataSet;
  req: THandle;
begin
  res := GetDataSetFromMsg(msg);
  req := GetRequestIdFromMsg(msg);
  debug(Format('ipc: Remote db call finished for request id %d.', [req]));
  NotifyComplete(req, res);
end;


procedure ReleaseRemoteCaller(errCode: integer);
begin
  // reply to the message so the clients thread is unblocked
  ReplyMessage(errCode);
end;


procedure SendDbListToRemote(window: THandle; request: Cardinal; ds: TDataSet);
begin
  SendDataSetToRemote(window, request, RES_DBLIST, ds);
end;


procedure HandleWMCompleteMessage(var msg: TMessage);
var
  req: Cardinal;
  res: TNotifyStructure;
  callback: TCompletionHandler;
begin
  debug('ipc: Handling WM_COMPLETED.');
  try
    // Extract results.
    req := msg.LParam;
    res := ExtractResults(req);
    // Call completion handler.
    callback := res.GetHandler;
    callback(res);
  finally
    ReleaseRemoteCaller(ERR_NOERROR);
  end;
end;


procedure HandleWMGetDbmsVersionMessage(var msg: TMessage);
var
  cwin: TMDIChild;
  err: integer;
begin
  debug('ipc: Handling WM_GETDBMSVERSION.');
  cwin := TMDIChild(Mainform.ActiveMDIChild);
  err := ERROR_NOT_CONNECTED;
  // Assumes mysql_version is set the instant the
  // ActiveMDIChild variable is set, or is at the
  // least initialized to ERROR_NOT_CONNECTED...
  if cwin <> nil then err := cwin.mysql_version;
  // Piggy-back result on error code.  Lazy.
  ReleaseRemoteCaller(err);
end;


procedure HandleWMGetDbListMessage(var msg: TMessage);
var
  ds: TDataSet;
  req: Cardinal;
  from: THandle;
begin
  debug('ipc: Handling WM_GETDBLIST.');
  try
    ds := MainForm.GetDBNames;
    try
      req := msg.LParam;
      from := msg.WParam;
      SendDbListToRemote(from, req, ds);
    finally
      ds.free;
    end;
  finally
    ReleaseRemoteCaller(ERR_NOERROR);
  end;
end;

procedure HandleWMCopyDataMessage(var msg: TWMCopyData);
var
  err: integer;
  query: string;
  //tab: THandle;
begin
  debug('ipc: Handling WM_COPYDATA.');
  err := ERR_UNSPECIFIED;
  case msg.CopyDataStruct^.dwData of
    CMD_EXECUTEQUERY: begin
      try
        query := GetQueryFromMsg(msg);
        MainForm.ExecuteRemoteQuery(msg.From, query);
        err := ERR_NOERROR;
      finally
        ReleaseRemoteCaller(err);
      end;
    end;
    RES_DBLIST: begin
      ReleaseRemoteCaller(ERR_NOERROR);
      FinishRemoteGetDbCommand(msg);
    end;
  end;
end;


end.

