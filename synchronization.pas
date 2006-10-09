unit synchronization;

(*
 Inter-Process Synchronization.

 Current limitations:
  * Hard limit on number of windows.
  * Reuses various Delphi exceptions instead of defining own.
 (Both could be fixed if necessary.)
*)



interface

const
  appName = 'HeidiSQL';
  (*
   If you alter the structure of the shared data (TWindowData)
   or the window limit (maxWindows), you must alter the string
   below so the two incompatible versions of your app can coxist
   peacefully.
  *)
  compatibilityLevel = 'v2';
  maxWindows = 99;

type
  TWindowData = packed record
    appHandle: THandle;
    (*connectionUid: integer;*)
    name: ShortString;
    namePostFix: integer;
    (*ready: boolean;*)
    connected: boolean;
  end;
  TWindowDataArray = packed array of TWindowData;

(*
 Run this procedure at application startup.
 catch any exceptions and abort after showing
 an error message if one occurred.
*)
procedure InitializeSync(myWindow: THandle);

(*
 Run this procedure at application shutdown.
 Catch any exceptions and show an error message if one occurred.
*)
procedure DeInitializeSync;

(*
 Run this procedure when (dis-)connecting.
 Set uid to a connection profile's uid,
 or 0 if it's a custom connection or disconnection.

 Commented out:
 We currently do not have any uniqueness handling nor versioning of connection profiles.
*)
(*procedure SetConnectionUid(uid: integer);*)

(*
 Run this procedure when starting and ending database queries.

 Commented out:
 I'm not sure we want 1 connection per window.
 Perhaps we want multiple tabs, each with their own connection,
 in which case a per-window flag is useless.
*)
(*procedure SetWindowReady(ready: boolean);*)

(*
 Run this procedure when opening/closing connection to a host.
*)
procedure SetWindowConnected(connected: boolean);

(*
 Run this procedure when opening/closing connection.
*)
procedure SetWindowName(name: string);

(*
 Run this procedure to get a list of application windows.
 If you run this function from a timer, consider:
   1) Disable the timer at once when it's fired, reenable when event handler is all done.
   2) Before reenabling the timer, adjust it's interval value by adding 100msec * nr. of windows.
*)
function GetWindowList: TWindowDataArray;

(*
 Run this procedure to find and remove disappeared application instances' data.
 If you run this function from a timer, consider:
   1) Disable the timer at once when it's fired, reenable when event handler is all done.
   2) Before reenabling the timer, adjust it's interval value by adding 100msec * nr. of windows.
*)
procedure CheckForCrashedWindows;



implementation

uses
  Windows,
  SysUtils;

const
  FILE_BACKING_PAGEFILE = $ffffffff;

type
  PSharedData = ^TSharedData;
  TSharedData = record
    windows: array[0..maxWindows] of TWindowData;
  end;

var
  sharedData: PSharedData = nil;
  mySlot: integer = -1;
  myDataArea: THandle = 0;
  myInternalLock: TRTLCriticalSection;
  mutexName: PAnsiChar;
  dataAreaName: PAnsiChar;

procedure InitializeSync(myWindow: THandle);
var
  mutex: THandle;
  errorCode: integer;
  i: integer;
begin
  EnterCriticalSection(myInternalLock);
    try
    // Take ownership of a mutually exclusive lock shared
    // between multiple instances of this application.
    // Create the lock if it doesn't exist.
    mutex := CreateMutex(nil, true, mutexName);
    if GetLastError = ERROR_ALREADY_EXISTS then begin
      mutex := OpenMutex(MUTEX_ALL_ACCESS, false, mutexName);
    end;

    // Die on failure to open or create lock.
    if mutex = 0 then begin
      raise EAssertionFailed.CreateFmt('Failed to create or open mutex ''%s'': Win32 error %d.', [mutexName, GetLastError]);
    end;

    // Create or open shared data area.
    myDataArea := CreateFileMapping(FILE_BACKING_PAGEFILE, nil, PAGE_READWRITE, 0, SizeOf(TSharedData), dataAreaName);
    errorCode := GetLastError;

    // Die on failure to open or create shared data area.
    if myDataArea = 0 then begin
      raise EAssertionFailed.CreateFmt('Failed to create or open file mapping ''%s'': Win32 error %d.', [dataAreaName, errorCode]);
    end;

    // Map shared data area into our process memory.
    sharedData := MapViewOfFile(myDataArea, FILE_MAP_ALL_ACCESS, 0, 0, 0);

    // Die on failure to map data area.
    if sharedData = nil then begin
      raise EAssertionFailed.CreateFmt('Failed to map data area into memory: Win32 error %d.', [GetLastError]);
    end;

    // If we've just created the data area, initialize it.
    // (Not particularly needed on Windows, since it clears
    //  memory in a background thread before giving it to apps.
    //  But that's a security feature so who knows if it will
    //  be around as a feature later / on different platforms.)
    if errorCode <> ERROR_ALREADY_EXISTS then begin
      FillChar(sharedData^, SizeOf(TSharedData), 0);
    end;

    // Find an empty slot and grab it.
    for i := 0 to maxWindows do begin
      if sharedData^.windows[i].appHandle = 0 then begin
        sharedData^.windows[i].appHandle := myWindow;
        (*sharedData^.windows[i].connectionUid := 0;*)
        (*sharedData^.windows[i].ready := true;*)
        sharedData^.windows[i].name := '';
        sharedData^.windows[i].namePostFix := 0;
        sharedData^.windows[i].connected := false;
        mySlot := i;
        break;
      end;
    end;

    // Die if no empty slot was found.
    if mySlot = -1 then begin
      raise EAssertionFailed.CreateFmt('Failed to allocate window slot. No more than %d instances is allowed.', [maxWindows + 1]);
    end;

    // Disown lock.
    ReleaseMutex(mutex);

  finally
    LeaveCriticalSection(myInternalLock);
  end;
end;


// Internal helper procedure to grab the mutex and abort on error.
procedure GrabLock(var mutex: THandle);
begin
  mutex := OpenMutex(MUTEX_ALL_ACCESS, false, mutexName);

  // Die on failure to open lock.
  if mutex = 0 then begin
    raise EAssertionFailed.CreateFmt('Failed to open mutex ''%s'': Win32 error %d.', [mutexName, GetLastError]);
  end;
end;


function FindWindowsWithName(searchName: string; excludeSlot: integer; addPostFix: boolean; clearPostFix: boolean): integer;
var
  max: integer;
  i: integer;
begin
  // Find out if other windows has the same name.
  max := 0;
  for i := 0 to maxWindows do begin
    if i <> excludeSlot then with sharedData^.windows[i] do begin
      if appHandle <> 0 then begin
        if name = searchName then begin
          if addPostFix or clearPostFix then begin
            if addPostFix and (namePostFix = 0) then namePostFix := 1;
            if clearPostFix then namePostFix := 0;
          end else if max = 0 then max := max + 1;
          if namePostFix > max then max := namePostFix;
        end;
      end;
    end;
  end;
  result := max;
end;


procedure DeInitializeSync;
var
  mutex: THandle;
begin
  EnterCriticalSection(myInternalLock);
  try
    // Take ownership of lock.
    GrabLock(mutex);

    // Clear uniqueness postfix if only one window with this name is left.
    if FindWindowsWithName(sharedData^.windows[mySlot].name, mySlot, false, false) = 1 then begin
      FindWindowsWithName(sharedData^.windows[mySlot].name, -1, false, true);
    end;

    // Remove ourselves from our slot.
    if mySlot > -1 then begin
      sharedData^.windows[mySlot].appHandle := 0;
      mySlot := -1;
    end;

    // Unmap shared data from memory.
    if sharedData <> nil then begin
      UnmapViewOfFile(sharedData);
      sharedData := nil;
    end;
    if myDataArea <> 0 then begin
      CloseHandle(myDataArea);
      myDataArea := 0;
    end;

    // We could potentially clean up the mutex if we're
    // the last instance, but we really need not bother
    // since Windows does it for us when we terminate.

    // Release lock.
    ReleaseMutex(mutex);

  finally
    LeaveCriticalSection(myInternalLock);
  end;
end;


(*
procedure SetConnectionUid(uid: integer);
var
  mutex: THandle;
begin
  EnterCriticalSection(myInternalLock);
    try
    // Take ownership of lock.
    GrabLock(mutex);

    // Set UID value.
    sharedData^.windows[mySlot].connectionUid := uid;

    // Release lock.
    ReleaseMutex(mutex);

  finally
    LeaveCriticalSection(myInternalLock);
  end;
end;
*)


(*
procedure SetWindowReady(ready: boolean);
var
  mutex: THandle;
begin
  EnterCriticalSection(myInternalLock);
    try
    // Take ownership of lock.
    GrabLock(mutex);

    // Set UID value.
    sharedData^.windows[mySlot].ready := ready;

    // Release lock.
    ReleaseMutex(mutex);

  finally
    LeaveCriticalSection(myInternalLock);
  end;
end;
*)


procedure SetWindowConnected(connected: boolean);
var
  mutex: THandle;
begin
  EnterCriticalSection(myInternalLock);
    try
    // Take ownership of lock.
    GrabLock(mutex);

    // Set UID value.
    sharedData^.windows[mySlot].connected := connected;

    // Release lock.
    ReleaseMutex(mutex);

  finally
    LeaveCriticalSection(myInternalLock);
  end;
end;


procedure SetWindowName(name: string);
var
  mutex: THandle;
  count: integer;
begin
  EnterCriticalSection(myInternalLock);
  try
    // Take ownership of lock.
    GrabLock(mutex);

    // Find out if other windows has the same name, in which case set namePostFix > 0.
    count := FindWindowsWithName(name, mySlot, true, false);
    if count > 0 then count := count + 1;
    sharedData^.windows[mySlot].namePostFix := count;

    // Set name by copying string value into array.
    sharedData^.windows[mySlot].name := name;

    // Release lock.
    ReleaseMutex(mutex);

  finally
    LeaveCriticalSection(myInternalLock);
  end;
end;


function GetWindowList: TWindowDataArray;
var
  mutex: THandle;
  i: integer;
  count: integer;
begin
  EnterCriticalSection(myInternalLock);
  try
    // Take ownership of lock.
    GrabLock(mutex);

    // Find how many slots are used.
    count := 0;
    if sharedData <> nil then begin
      for i := 0 to maxWindows do begin
        if sharedData^.windows[i].appHandle <> 0 then begin
          count := count + 1;
        end;
      end;
    end;

    // Create an array with a clone of the slot data.
    SetLength(result, count);
    if sharedData <> nil then begin
      count := 0;
      for i := 0 to maxWindows do begin
        if sharedData^.windows[i].appHandle <> 0 then begin
          result[count] := sharedData^.windows[i];
          count := count + 1;
          if count = High(result) + 1 then break;
        end;
      end;
    end;

    // Release lock.
    ReleaseMutex(mutex);

    // Hint the programmer that he might or might not have an issue.
    // This exception can be safely ignored, it's ok to hit it on purpose.
    if count = 0 then begin
      raise ERangeError.Create('GetWindowList() called, but there is 0 instances left.');
    end;

  finally
    LeaveCriticalSection(myInternalLock);
  end;
end;


procedure CheckForCrashedWindows;
var
  mutex: THandle;
  count: integer;
  i: integer;
begin
  EnterCriticalSection(myInternalLock);
  try
    // Take ownership of lock.
    GrabLock(mutex);

    // Remove application instance shared data if they've disappeared.
    count := 0;
    if sharedData <> nil then begin
      for i := 0 to maxWindows do with sharedData^.windows[i] do begin
        if appHandle <> 0 then begin
          count := count + 1;
          if not IsWindow(appHandle) then begin
            // Clear uniqueness postfix if only one window with this name is left.
            if FindWindowsWithName(name, i, false, false) = 0 then FindWindowsWithName(name, -1, false, true);
            appHandle := 0;
          end;
        end;
      end;
    end;

    // Release lock.
    ReleaseMutex(mutex);

    // Hint the programmer that he might or might not have an issue.
    // This exception can be safely ignored, it's ok to hit it on purpose.
    if count = 0 then begin
      raise ERangeError.Create('CheckForCrashedWindows() called, but there is 0 instances left.');
    end;

  finally
    LeaveCriticalSection(myInternalLock);
  end;
end;


initialization
  // The critical section is technically not absolutely necessary,
  // but it makes this unit thread-safe, thereby making it easier
  // to use the functions herein.
  InitializeCriticalSection(myInternalLock);
  mutexName := appName + '_mutex_' + compatibilityLevel;
  dataAreaName := appName + '_data_' + compatibilityLevel;

end.

