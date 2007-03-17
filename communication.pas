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
  // Our message subtypes for WM_COPYDATA messages.
  CMD_EXECUTEQUERY_NORESULTS     = 1; { Slightly faster  - Fire-and-forget, no results }
  CMD_EXECUTEQUERY_RESULTS       = 2; { Normal           - Wait for completion, fetch results }
  RES_QUERYDATA                  = 257;
  RES_EXCEPTION                  = 258;
  RES_NONQUERY                   = 259;
  // Our custom return codes.
  ERR_NOERROR         = 0;
  ERR_UNSPECIFIED     = 1;

  // Sent by TMysqlQueryThread to notify status
  WM_MYSQL_THREAD_NOTIFY = WM_USER+100;

(*
 Execute a query on a window.
*)
procedure RemoteExecNonQuery(window: THandle; query: string);

(*
 Execute a USE query on a window,
 given the version of the mysql server that window is connected to.
*)
procedure RemoteExecUseNonQuery(window: THandle; mysqlVersion: integer; dbName: string);

(*
 Execute a query on another window, request results but don't wait for them.
*)
function RemoteExecQueryAsync(handler: TCompletionHandler; timeout: Cardinal; window: THandle; query: String; waitControl: TObject = nil): Cardinal;

(*
 Execute a query on another window, showing a wait dialog while processing
 and calling a completion handler via messaging when done.
*)
function RemoteExecQuery(handler: TCompletionHandler; timeout: Cardinal; window: THandle; query: String; info: String): Cardinal;

(*
 Execute a query on another window, showing a wait dialog while processing
 and returning results or raising an exception when done.
*)
function RemoteExecQuerySimple(window: THandle; query: String; info: String): TDataSet;

(*
 Fill in resulting data and return waiting thread to caller.
 Call from message handler when a query has finished executing and results are ready.
*)
procedure FinishRemoteExecution(msg: TWMCopyData);

(*
 Slightly lame wrapper: Call to release a RemoteXXX SendMessage call on the "other side".
*)
procedure ReleaseRemoteCaller(errCode: integer);

(*
 Extract a SQL query from a WM_COPYDATA message.
*)
function GetQueryFromMsg(msg: TWMCopyData): string;

(*
 Extract a request id from a WM_COPYDATA message.
*)
function GetRequestIdFromMsg(msg: TWMCopyData): Cardinal;

(*
 Helper which will handle WM_COMPLETED messages received.
*)
procedure HandleWMCompleteMessage(var msg: TMessage);

(*
 Helper which will handle WM_COPYDATA messages received.
*)
procedure HandleWMCopyDataMessage(var msg: TWMCopyData);


implementation

uses
  Forms,
  Dialogs,
  AdoDb,
  AdoInt,
  ActiveX,
  main,
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
    ms.Write(request, sizeof(Cardinal));
    if resType <> RES_NONQUERY then begin
      adods := CopyDataSetToAdoDataSet(ds);
      sa := TStreamAdapter.Create(ms);
      olevar := adods.Recordset;
      olevar.Save(sa as IStream, 0);
    end;
    debug(Format('ipc: Sending data set to window %d, request id %d, size %d', [window, request, ms.Size]));
    data.dwData := resType;
    data.cbData := ms.Size;
    data.lpData := ms.Memory;
    SendMessage(window, WM_COPYDATA, MainForm.Handle, integer(@data));
  finally
    ms.free;
  end;
end;


procedure SendErrorStringToRemote(window: THandle; request: Cardinal; resType: integer; error: string);
var
  data: TCopyDataStruct;
  ms: TMemoryStream;
  pcerr: PChar;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(request, sizeof(Cardinal));
    pcerr := PChar(error);
    ms.Write(pcerr^, StrLen(pcerr) + 1);
    debug(Format('ipc: Sending error message to window %d, request id %d, size %d', [window, request, ms.Size]));
    data.dwData := resType;
    data.cbData := ms.Size;
    data.lpData := ms.Memory;
    SendMessage(window, WM_COPYDATA, MainForm.Handle, integer(@data));
  finally
    ms.free;
  end;
end;


function GetStringFromMsgInternal(msg: TWMCopyData): string;
var
  pcsql: PChar;
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    with msg.CopyDataStruct^ do begin
      ms.Write(lpData^, cbData);
      ms.Position := sizeof(Cardinal);
      pcsql := StrAlloc(ms.Size - sizeof(Cardinal));
      ms.Read(pcsql^, ms.Size - sizeof(Cardinal));
      result := pcsql;
    end;
  finally
    ms.free;
  end;
end;


function GetQueryFromMsg(msg: TWMCopyData): string;
begin
  result := GetStringFromMsgInternal(msg);
end;


function GetExceptionTextFromMsg(msg: TWMCopyData): string;
begin
  result := GetStringFromMsgInternal(msg);
end;


function GetRequestIdFromMsg(msg: TWMCopyData): Cardinal;
var
  req: Cardinal;
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    with msg.CopyDataStruct^ do begin
      ms.Write(lpData^, cbData);
      ms.Position := 0;
      ms.Read(req, sizeof(Cardinal));
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
      ms.Position := sizeof(Cardinal);
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


procedure RemoteExecQueryInternal(method: DWORD; req: Cardinal; window: THandle; query: String);
var
  ms: TMemoryStream;
  pcsql: PChar;
  data: TCopyDataStruct;
  err: integer;
begin
  ms := TMemoryStream.Create;
  try
    debug(Format('ipc: Remote query being requested, id %d.', [req]));
    ms.Write(req, sizeof(Cardinal));
    pcsql := PChar(query);
    ms.Write(pcsql^, StrLen(pcsql) + 1);
    data.dwData := method;
    data.cbData := ms.Size;
    data.lpData := ms.Memory;
    err := SendMessage(window, WM_COPYDATA, MainForm.Handle, integer(@data));
    if err <> 0 then Exception.CreateFmt('Remote returned error %d when asked to execute query', [err]);
  finally
    ms.free;
  end;
end;


procedure RemoteExecNonQuery(window: THandle; query: string);
begin
  RemoteExecQueryInternal(CMD_EXECUTEQUERY_NORESULTS, REQUEST_ID_INVALID, window, query);
end;


procedure RemoteExecUseNonQuery(window: THandle; mysqlVersion: integer; dbName: string);
begin
  RemoteExecNonQuery(window, 'USE ' + maskSql(mysqlVersion, dbName));
end;


function RemoteExecQueryAsync(handler: TCompletionHandler; timeout: Cardinal; window: THandle; query: String; waitControl: TObject = nil): Cardinal;
var
  req: Cardinal;
begin
  req := SetCompletionHandler(handler, timeout, waitControl);
  RemoteExecQueryInternal(CMD_EXECUTEQUERY_RESULTS, req, window, query);
  result := req;
end;


function RemoteExecQuery(handler: TCompletionHandler; timeout: Cardinal; window: THandle; query: String; info: String): Cardinal;
var
  cancelDialog: TForm;
  requestId: Cardinal;
begin
  if Length(info) = 0 then info := 'Waiting for remote session to execute query...';
  cancelDialog := CreateMessageDialog(info, mtCustom, [mbCancel]);
  requestId := RemoteExecQueryAsync(handler, timeout, window, query, cancelDialog);
  // The callback method shouldn't be activated before messages has been processed,
  // so we can safely touch the wait control (a cancel dialog) here.
  cancelDialog.ShowModal;
  // We just cancel in any case.
  // If the query was completed before the cancel dialog closed,
  // the notification code won't accept the cancel, so it's OK.
  NotifyInterrupted(requestId, Exception.Create('User cancelled.'));
  result := RequestId;
end;


function RemoteExecQuerySimple(window: THandle; query: String; info: String): TDataSet;
var
  requestId: Cardinal;
begin
  // Call with no handler (= no completion message) and no timeout.
  requestId := RemoteExecQuery(nil, INFINITE_TIMEOUT, window, query, info);
  // Take care of results since there's no handler.
  result := TDataSet(ExtractResultObject(requestId));
end;


procedure SwitchWaitControlInternal(waitControl: TObject);
var
  cancelDialog: TForm;
begin
  // Hide the cancel dialog if it's still showing.
  cancelDialog := TForm(waitControl);
  if (cancelDialog <> nil) and cancelDialog.Visible then cancelDialog.Close;
end;


procedure FinishRemoteExecution(msg: TWMCopyData);
var
  res: TDataSet;
  req: Cardinal;
  s: string;
begin
  req := GetRequestIdFromMsg(msg);
  debug(Format('ipc: Remote execute query call finished for request id %d.', [req]));
  case msg.CopyDataStruct^.dwData of
    RES_QUERYDATA: begin
      res := GetDataSetFromMsg(msg);
      NotifyComplete(req, res);
    end;
    RES_EXCEPTION: begin
      s := GetExceptionTextFromMsg(msg);
      NotifyInterrupted(req, Exception.Create(s));
    end;
  end;
end;


procedure ReleaseRemoteCaller(errCode: integer);
begin
  // reply to the message so the clients thread is unblocked
  ReplyMessage(errCode);
end;


procedure ReportFinishedQuery(method: DWORD; window: THandle; request: Cardinal; ds: TDataSet);
var
  resType: DWORD;
begin
  if method = CMD_EXECUTEQUERY_NORESULTS then resType := RES_NONQUERY
  else resType := RES_QUERYDATA;
  SendDataSetToRemote(window, request, resType, ds);
end;


procedure ReportFailedQuery(method: DWORD; window: THandle; request: Cardinal; error: string); overload;
begin
  SendErrorStringToRemote(window, request, RES_EXCEPTION, error);
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
    res := ExtractResults(req, true);
    // Switch wait control to non-waiting state.
    SwitchWaitControlInternal(res.GetWaitControl);
    // Perform rest of completion via callback, if any.
    callback := res.GetHandler;
    if @callback <> nil then begin
      // Clear results.
      ExtractResults(req);
      // Perform callback.
      callback(res);
    end;
    // Otherwise just assume that completion will be handled
    // by some thread which were waiting for the wait control.
    //
    // In the future, we could explicitly sound an event for
    // this purpose, in case it's not possible to wait on the
    // wait control..
  finally
    ReleaseRemoteCaller(ERR_NOERROR);
  end;
end;


procedure HandleWMCopyDataMessage(var msg: TWMCopyData);
var
  method: DWORD;
  query: string;
  remoteReqId: integer;
  data: TDataSet;
  //tab: THandle;
begin
  debug('ipc: Handling WM_COPYDATA.');
  method := msg.CopyDataStruct^.dwData;
  if
    (method = CMD_EXECUTEQUERY_NORESULTS) or
    (method = CMD_EXECUTEQUERY_RESULTS)
  then begin
    try
      remoteReqId := GetRequestIdFromMsg(msg);
      query := GetQueryFromMsg(msg);
    finally
      ReleaseRemoteCaller(ERR_NOERROR);
    end;
    data := nil;
    try
      if method = CMD_EXECUTEQUERY_NORESULTS then begin
        MainForm.ExecuteRemoteNonQuery(msg.From, query);
      end else begin
        data := MainForm.ExecuteRemoteQuery(msg.From, query);
      end;
    except
      on e: Exception do begin
        ReportFailedQuery(method, msg.From, remoteReqId, e.Message);
      end;
    end;
    if data <> nil then ReportFinishedQuery(method, msg.From, remoteReqId, data);
  end;
  if
    (method = RES_QUERYDATA) or
    (method = RES_EXCEPTION)
  then begin
    ReleaseRemoteCaller(ERR_NOERROR);
    FinishRemoteExecution(msg);
  end;
end;


end.

