unit threading;

interface

uses
  SysUtils;

type
  // This declaration is needed for the kind of circular typw
  // declaration we make, with Delphi's one-pass compiler.
  TNotifyStructure = class;

  // "of object" means that we expect an instance method,
  // not a procedure which isn't contained in a class.
  TCompletionHandler = procedure(result: TNotifyStructure) of object;

  TNotifyStructure = class
    private
      req: Cardinal;
      res: TObject;
      ex: Exception;
      handler: TCompletionHandler;
      ctl: TObject;
    public
      property GetRequestId: Cardinal read req;
      property GetResult: TObject read res write res;
      property GetException: Exception read ex write ex;
      property GetHandler: TCompletionHandler read handler;
      property GetWaitControl: TObject read ctl;
      constructor Create(const RequestId: Cardinal; const CompletionHandler: TCompletionHandler; const WaitControl: TObject);
  end;

function SetCompletionHandler(handler: TCompletionHandler; timeout: integer; waitControl: TObject = nil): Cardinal;
procedure NotifyComplete(RequestId: Cardinal; Results: TObject);
procedure NotifyInterrupted(RequestId: Cardinal; AnException: Exception);
function ExtractResults(RequestId: Cardinal; PeekOnly: Boolean = false): TNotifyStructure;
function ExtractResultObject(RequestId: Cardinal): TObject;

const
  INFINITE_TIMEOUT = $7fffffff;
  REQUEST_ID_INVALID = 0;

implementation

uses
  Main,
  Windows,
  Communication,
  Helpers,
  Forms,
  Classes;


var
  working: TThreadList;

constructor TNotifyStructure.Create(const RequestId: Cardinal; const CompletionHandler: TCompletionHandler; const WaitControl: TObject);
begin
  self.req := RequestId;
  self.handler := CompletionHandler;
  self.ctl := WaitControl;
  self.res := nil;
  self.ex := nil;
end;

function IndexOf(list: TList; RequestId: Cardinal): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to list.Count - 1 do begin
    if TNotifyStructure(list[i]).GetRequestId = RequestId then
    begin
      result := i;
      exit;
    end;
  end;
end;

function SetCompletionHandler(handler: TCompletionHandler; timeout: integer; waitControl: TObject = nil): Cardinal;
var
  lockedList: TList;
  ns: TNotifyStructure;
  txt: string;
  RequestId: Cardinal;
begin
  debug('thr: Setting completion handler.');
  lockedList := working.LockList;
  try
    // Make a unique request id.
    repeat
      RequestId := Random($7fffffff);
    until (RequestId <> REQUEST_ID_INVALID) and (IndexOf(lockedList, RequestId) = -1);
    debug(Format('thr: Assigned request id %d.', [RequestId]));
    result := RequestId;
    // Raise an exception if a handler already exists.
    if IndexOf(lockedList, RequestId) > -1 then begin
      txt := Format('Internal error: Request %d is already in a waiting state?', [RequestId]);
      debug(txt);
      raise Exception.Create(txt);
    end;
    // Create + add notify structure.
    ns := TNotifyStructure.Create(RequestId, handler, waitControl);
    lockedList.Add(ns);
    // Optionally start timer.
    if timeout <> INFINITE_TIMEOUT then begin
      // todo: create a timer that activates NotifyInterupted on timeout.
    end;
  finally
    working.UnlockList;
  end;
end;

procedure InternalNotify(RequestId: Cardinal; Results: TObject; AnException: Exception);
var
  lockedList: TList;
  idx: integer;
  item: TNotifyStructure;
begin
  debug(Format('thr: Notifying completion of %d.', [RequestId]));
  lockedList := working.LockList;
  try
    idx := IndexOf(lockedList, RequestId);
    // Abort if we can't find notify structure.
    if idx = -1 then begin
      debug(Format('thr: Received completion notice for %d, but handler does not exist.  Perhaps already completed.', [RequestId]));
      exit;
    end;
    item := TNotifyStructure(lockedList[idx]);
    // Abort if result or exception has already been set.
    if (item.res <> nil) or (item.ex <> nil) then begin
      debug(Format('thr: Received completion notice for %d, but handler already has result or exception.  Perhaps already completed, but handler hasn''t been activated yet.', [RequestId]));
      exit;
    end;
    // Set result.
    item.res := Results;
    // Stop timer.
    // todo: stop timeout timer.
  finally
    working.UnlockList;
  end;
  // Send a message to the active message loop indicating
  // that processing has completed.
  PostMessage(MainForm.Handle, WM_COMPLETED, MainForm.Handle, item.GetRequestId);
end;

procedure NotifyComplete(RequestId: Cardinal; Results: TObject); overload;
begin
  InternalNotify(RequestId, Results, nil);
end;

procedure NotifyInterrupted(RequestId: Cardinal; AnException: Exception); overload;
begin
  InternalNotify(RequestId, nil, AnException);
end;

function ExtractResults(RequestId: Cardinal; PeekOnly: Boolean = false): TNotifyStructure;
var
  lockedList: TList;
  idx: integer;
  txt: string;
begin
  result := nil;
  lockedList := working.LockList;
  try
    idx := IndexOf(lockedList, RequestId);
    // Raise an exception if we can't find notify structure.
    if idx = -1 then begin
      txt := Format('Request %d does not exist, cannot extract results.', [RequestId]);
      debug(txt);
      raise Exception.Create(txt);
    end;
    // Remove notify structure from list and return it to caller.
    result := TNotifyStructure(lockedList[idx]);
    if not PeekOnly then lockedList.Delete(idx);
  finally
    working.UnlockList;
  end;
end;

function ExtractResultObject(RequestId: Cardinal): TObject;
var
  res: TNotifyStructure;
  ex: Exception;
  o: TObject;
begin
  res := ExtractResults(RequestId);
  o := res.GetResult;
  ex := res.GetException;
  res.Free;
  if o <> nil then begin
    result := o;
    exit
  end else if ex <> nil then begin
    raise ex;
  end else begin
    raise Exception.Create('Internal error in caller: no results available yet.');
  end;
end;


initialization
  working := TThreadList.Create;

end.

