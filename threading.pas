unit threading;

interface

procedure WaitForCompletion(TabOrWindowId: THandle);
procedure NotifyComplete(TabOrWindowId: THandle);

implementation

uses
  Helpers,
  Forms,
  SysUtils,
  Classes;

var
  working: TList;

procedure WaitForCompletion(TabOrWindowId: THandle);
begin
  // Allow the GUI to continue to function, but wait for an event
  // (db query, remote call) to finish before returning control to caller.
  if working.IndexOf(Pointer(TabOrWindowId)) > -1 then begin
    raise Exception.Create('Internal Error: already working?');
  end;
  working.Add(Pointer(TabOrWindowId));
  while working.IndexOf(Pointer(TabOrWindowId)) > -1 do begin
    debug(Format('thr: Caller %d awaiting completion, list index is %d', [TabOrWindowId, working.IndexOf(Pointer(TabOrWindowId))]));
    Application.ProcessMessages;
    Sleep(50);
  end;
  debug(Format('thr: Returning control to caller %d, list index is %d', [TabOrWindowId, working.IndexOf(Pointer(TabOrWindowId))]));
end;

procedure NotifyComplete(TabOrWindowId: THandle);
begin
  // Someone told us that we're done working on something.
  // Eg. a db query or remote call has finished executing.
  //
  // Pick the handle given out of the 'working' list.
  //
  // In case no call to WaitForCompletion has occurred yet,
  // sleep/yield until that has happened first, thereby allowing
  // starting a query / remote call before actually calling WaitForCompletion().
  debug(Format('thr: Notifying completion to %d, list index is %d', [TabOrWindowId, working.IndexOf(Pointer(TabOrWindowId))]));
  while working.IndexOf(Pointer(TabOrWindowId)) = -1 do Sleep(50);
  working.Remove(Pointer(TabOrWindowId));
end;

initialization
  working := TList.Create;

end.
