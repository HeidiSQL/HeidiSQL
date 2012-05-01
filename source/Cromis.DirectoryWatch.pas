(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2009 Iztok Kacin, Cromis (iztok.kacin@gmail.com).
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 * - Neither the name of the Iztok Kacin nor the names of its contributors may be
 *   used to endorse or promote products derived from this software without specific
 *   prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * NOTICE OF CODE ORIGIN
 *
 * This code was derived from the original code of author "Gleb Yourchenko"
 * The original code "FnugryDirWatch" can still be found at Torry Components
 * The URL is: http://www.torry.net/pages.php?id=252
 *
 * The code was taken as a starting point and then mainly written from scratch
 * keeping some of the healthy code parts. So I am not in any way an author of
 * the original idea. But I am the author of all the changes and new code parts.
 *
 * ============================================================================
 * 12/10/2009 (1.0.0)
 *  - Initial code rewrite from "FnugryDirWatch"
 * 16/01/2010 (1.0.1)
 *  - Refactored the main watch loop
 * ============================================================================
*)

unit Cromis.DirectoryWatch;

interface

uses
   Windows, SysUtils, Classes, Messages;

const
  FILE_NOTIFY_CHANGE_FILE_NAME   = $00000001;
  FILE_NOTIFY_CHANGE_DIR_NAME    = $00000002;
  FILE_NOTIFY_CHANGE_ATTRIBUTES  = $00000004;
  FILE_NOTIFY_CHANGE_SIZE        = $00000008;
  FILE_NOTIFY_CHANGE_LAST_WRITE  = $00000010;
  FILE_NOTIFY_CHANGE_LAST_ACCESS = $00000020;
  FILE_NOTIFY_CHANGE_CREATION    = $00000040;
  FILE_NOTIFY_CHANGE_SECURITY    = $00000100;

const
  cShutdownTimeout = 3000;
  
type
  // the filters that control when the watch is triggered
  TWatchOption = (woFileName, woDirName, woAttributes, woSize, woLastWrite,
                  woLastAccess, woCreation, woSecurity);
  TWatchOptions = set of TWatchOption;

  // the actions that are the result of the watch being triggered
  TWatchAction = (waAdded, waRemoved, waModified, waRenamedOld, waRenamedNew);
  TWatchActions = set of TWatchAction;

  TFileChangeNotifyEvent = procedure(const Sender: TObject;
                                     const Action: TWatchAction;
                                     const FileName: string
                                     ) of object;

  TDirectoryWatch = class
  private
    FWatchOptions : TWatchOptions;
    FWatchActions : TWatchActions;
    FWatchSubTree : Boolean;
    FWatchThread  : TThread;
    FWndHandle    : HWND;
    FDirectory    : string;
    FAbortEvent   : Cardinal;
    FOnChange     : TNotifyEvent;
    FOnNotify     : TFileChangeNotifyEvent;
    procedure WatchWndProc(var Msg: TMessage);
    procedure SetDirectory(const Value: string);
    procedure SetWatchOptions(const Value: TWatchOptions);
    procedure SetWatchActions(const Value: TWatchActions);
    procedure SetWatchSubTree(const Value: Boolean);
    procedure DeallocateHWnd(Wnd: HWND);
    function MakeFilter: Integer;
  protected
    procedure Change; virtual;
    procedure AllocWatchThread;
    procedure ReleaseWatchThread;
    procedure RestartWatchThread;
    procedure Notify(const Action: Integer;
                     const FileName: string
                     ); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    function Running: Boolean;
    property WatchSubTree: Boolean read FWatchSubTree write SetWatchSubTree;
    property WatchOptions: TWatchOptions read FWatchOptions write SetWatchOptions;
    property WatchActions: TWatchActions read FWatchActions write SetWatchActions;
    property Directory: string read FDirectory write SetDirectory;
    // notification properties. Notify about internal and exernal changes
    property OnNotify: TFileChangeNotifyEvent read FOnNotify write FOnNotify;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

type
  PFILE_NOTIFY_INFORMATION = ^TFILE_NOTIFY_INFORMATION;
  TFILE_NOTIFY_INFORMATION = record
    NextEntryOffset : Cardinal;
    Action          : Cardinal;
    FileNameLength  : Cardinal;
    FileName        : array[0..MAX_PATH - 1] of WideChar;
  end;

const
  WM_DIRWATCH_ERROR    = WM_USER + 137;
  WM_DIRWATCH_NOTIFY   = WM_USER + 138;

  FILE_LIST_DIRECTORY  = $0001;

const
  // error messages
  cErrorInWatchThread = 'Error "%s" in watch thread. Error code: %d';
  cErrorCreateWatchError = 'Error trying to create file handle for "%s". Error code: %d';

const
  IO_BUFFER_LEN = 32 * SizeOf(TFILE_NOTIFY_INFORMATION);
  
type
  TDirWatchThread = class(TThread)
  private
     FWatchSubTree : Boolean;
     FAbortEvent   : Cardinal;
     FChangeEvent  : Cardinal;
     FWndHandle    : Cardinal;
     FDirHandle    : Cardinal;
     FDirectory    : string;
     FIOResult     : Pointer;
     FFilter       : Integer;
  protected
     procedure Execute; override;
  public
     constructor Create(const Directory: string;
                        const WndHandle: Cardinal;
                        const AbortEvent: Cardinal;
                        const TypeFilter: Cardinal;
                        const aWatchSubTree: Boolean);
     destructor Destroy; override;
  end;

procedure TDirWatchThread.Execute;
var
  NotifyData: PFILE_NOTIFY_INFORMATION;
  Events: array[0..1] of THandle;
  WaitResult: DWORD;
  NextEntry: Integer;
  ErrorMsg: PWideChar;
  FileName: PWideChar;
  Overlap: TOverlapped;
  ResSize: Cardinal;
begin
  FillChar(Overlap, SizeOf(TOverlapped), 0);
  Overlap.hEvent := FChangeEvent;

  // set the array of events
  Events[0] := FChangeEvent;
  Events[1] := FAbortEvent;

  while not Terminated do
  try
    if ReadDirectoryChangesW(FDirHandle, FIOResult, IO_BUFFER_LEN, FWatchSubtree, FFilter, @ResSize, @Overlap, nil) then
    begin
      WaitResult := WaitForMultipleObjects(2, @Events[0], FALSE, INFINITE);

      // check if we have terminated the thread
      if WaitResult <> WAIT_OBJECT_0 then
      begin
        Terminate;
        Exit;
      end;
      
      if WaitResult = WAIT_OBJECT_0 then
      begin
        NotifyData := FIOResult;

        repeat
          NextEntry := NotifyData^.NextEntryOffset;

          // get memory for filename and fill it with data
          GetMem(FileName, NotifyData^.FileNameLength + 2);
          Move(NotifyData^.FileName, Pointer(FileName)^, NotifyData^.FileNameLength);
          PWord(Cardinal(FileName) + NotifyData^.FileNameLength)^ := 0;

          // send the message about the filename information and advance to the next entry
          PostMessage(FWndHandle, WM_DIRWATCH_NOTIFY, NotifyData^.Action, LParam(FileName));
          Inc(DWORD(NotifyData), NextEntry);
        until (NextEntry = 0);
      end;
    end;
  except
    on E :Exception do
    begin
      GetMem(ErrorMsg, Length(E.Message) + 2);
      Move(E.Message, Pointer(ErrorMsg)^, Length(E.Message));
      PWord(Cardinal(ErrorMsg) + Cardinal(Length(E.Message)))^ := 0;
      PostMessage(FWndHandle, WM_DIRWATCH_ERROR, GetLastError, LPARAM(ErrorMsg));
    end;
  end;
end;

constructor TDirWatchThread.Create(const Directory: string;
                                   const WndHandle: Cardinal;
                                   const AbortEvent: Cardinal;
                                   const TypeFilter: Cardinal;
                                   const aWatchSubTree: Boolean);
begin
   //
   // Retrieve proc pointer, open directory to
   // watch and allocate buffer for notification data.
   // (note, it is done before calling inherited
   // create (that calls BeginThread) so any exception
   // will be still raised in caller's thread)
   //
   FDirHandle := CreateFile(PChar(Directory),
                            FILE_LIST_DIRECTORY,
                            FILE_SHARE_READ OR
                            FILE_SHARE_DELETE OR
                            FILE_SHARE_WRITE,
                            nil, OPEN_EXISTING,
                            FILE_FLAG_BACKUP_SEMANTICS OR
                            FILE_FLAG_OVERLAPPED,
                            0);

   if FDirHandle = INVALID_HANDLE_VALUE then
     raise Exception.CreateFmt(cErrorCreateWatchError, [Directory, GetLastError]);

   FChangeEvent := CreateEvent(nil, FALSE, FALSE, nil);
   FAbortEvent := AbortEvent;

   // allocate the buffer memory
   GetMem(FIOResult, IO_BUFFER_LEN);

   FWatchSubTree := aWatchSubtree;
   FWndHandle := WndHandle;
   FDirectory := Directory;
   FFilter := TypeFilter;

   // make sure we free the thread
   FreeOnTerminate := True;

   inherited Create(False);
end;


destructor TDirWatchThread.Destroy;
begin
   if FDirHandle <> INVALID_HANDLE_VALUE  then
     CloseHandle(FDirHandle);
   if Assigned(FIOResult) then
     FreeMem(FIOResult);

   inherited Destroy;
end;

{ TFnugryDirWatch }

procedure TDirectoryWatch.AllocWatchThread;
begin
  if FWatchThread = nil then
  begin
    FAbortEvent := CreateEvent(nil, FALSE, FALSE, nil);
    FWatchThread := TDirWatchThread.Create(Directory,
                                           FWndHandle,
                                           FAbortEvent,
                                           MakeFilter,
                                           WatchSubtree);
  end;
end;

procedure TDirectoryWatch.ReleaseWatchThread;
var
  AResult: Cardinal;
begin
  if FWatchThread <> nil then
  begin
    // set and close event
    SetEvent(FAbortEvent);
    CloseHandle(FAbortEvent);

    // wait and block until thread is finished
    AResult := WaitForSingleObject(FWatchThread.Handle, cShutdownTimeout);

    // check if we timed out
    if AResult = WAIT_TIMEOUT then
      TerminateThread(FWatchThread.Handle, 0);

    FWatchThread := nil;
  end;

end;

procedure TDirectoryWatch.RestartWatchThread;
begin
  Stop;
  Start;
end;

function TDirectoryWatch.Running: Boolean;
begin
  Result := FWatchThread <> nil;
end;

procedure TDirectoryWatch.DeallocateHWnd(Wnd: HWND);
var
  Instance: Pointer;
begin
  Instance := Pointer(GetWindowLong(Wnd, GWL_WNDPROC));

  if Instance <> @DefWindowProc then
  begin
    { make sure we restore the default
      windows procedure before freeing memory }
    SetWindowLong(Wnd, GWL_WNDPROC, Longint(@DefWindowProc));
    FreeObjectInstance(Instance);
  end;

  DestroyWindow(Wnd);
end;

destructor TDirectoryWatch.Destroy;
begin
  Stop;
  DeallocateHWnd(FWndHandle);

  inherited Destroy;
end;

constructor TDirectoryWatch.Create;
begin
   FWndHandle := AllocateHWnd(WatchWndProc);
   FWatchSubtree := True;

   // construct the default watch actions and options
   FWatchActions := [waAdded, waRemoved, waModified, waRenamedOld, waRenamedNew];
   FWatchOptions := [woFileName, woDirName, woAttributes, woSize, woLastWrite,
                     woLastAccess, woCreation, woSecurity];
end;



procedure TDirectoryWatch.SetWatchActions(const Value: TWatchActions);
begin
  if FWatchActions <> Value then
  begin
    FWatchActions := Value;

    if Running then
      RestartWatchThread;

    Change;
  end;
end;

procedure TDirectoryWatch.SetWatchOptions(const Value: TWatchOptions);
begin
  if FWatchOptions <> Value then
  begin
    FWatchOptions := Value;

    if Running then
      RestartWatchThread;

    Change;
  end;
end;

procedure TDirectoryWatch.WatchWndProc(var Msg :TMessage);
var
  ErrorCode: Cardinal;
  ErrorMessage: string;
begin
   case Msg.msg of
     WM_DIRWATCH_NOTIFY:
     //
     // Retrieve notify data and forward
     // the event to TDirectoryWatch's notify
     // handler. Free filename string (allocated
     // in WatchThread's notify handler.)
     //
     begin
        try
           Notify(Msg.wParam, WideCharToString(PWideChar(Msg.lParam)));
        finally
          if Msg.lParam <> 0 then
            FreeMem(Pointer(Msg.lParam));
        end;
     end;

     WM_DIRWATCH_ERROR:
     //
     // Disable dir watch and re-raise
     // exception on error
     //
     begin
        try
          ErrorMessage := WideCharToString(PWideChar(Msg.lParam));
          ErrorCode := Msg.WParam;
          Stop;

          raise Exception.CreateFmt(cErrorInWatchThread, [ErrorMessage, ErrorCode]);
        finally
          if Msg.lParam <> 0 then
            FreeMem(Pointer(Msg.lParam));
        end;
     end;
     //
     // pass all other messages down the line
     //
     else
     begin
       Msg.Result := DefWindowProc(FWndHandle, Msg.Msg, Msg.wParam, Msg.lParam);
       Exit;
     end;
   end;
end;

function TDirectoryWatch.MakeFilter: Integer;
const
  FilterFlags: array [TWatchOption] of Integer = (FILE_NOTIFY_CHANGE_FILE_NAME,
                                                  FILE_NOTIFY_CHANGE_DIR_NAME,
                                                  FILE_NOTIFY_CHANGE_ATTRIBUTES,
                                                  FILE_NOTIFY_CHANGE_SIZE,
                                                  FILE_NOTIFY_CHANGE_LAST_WRITE,
                                                  FILE_NOTIFY_CHANGE_LAST_ACCESS,
                                                  FILE_NOTIFY_CHANGE_CREATION,
                                                  FILE_NOTIFY_CHANGE_SECURITY);
var
  Flag: TWatchOption;
begin
  Result := 0;

  for Flag in FWatchOptions do
    Result := Result or FilterFlags[Flag];
end;

procedure TDirectoryWatch.SetWatchSubTree(const Value :Boolean);
begin
  if Value <> FWatchSubtree then
  begin
    FWatchSubtree := Value;

    if Running then
      RestartWatchThread;

    Change;
  end;
end;


procedure TDirectoryWatch.Start;
begin
  if FDirectory = '' then
    raise Exception.Create('Please specify a directory to watch');

  if not Running then
  begin
    AllocWatchThread;
    Change;
  end;
end;

procedure TDirectoryWatch.Stop;
begin
  if Running then
  begin
    ReleaseWatchThread;
    Change;
  end;
end;

procedure TDirectoryWatch.SetDirectory(const Value: string);
begin
  if StrIComp(PChar(Trim(Value)), PChar(FDirectory)) <> 0 then
  begin
    FDirectory := Trim(Value);

    if Running then
      RestartWatchThread;

    Change;
  end;
end;

procedure TDirectoryWatch.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDirectoryWatch.Notify(const Action: Integer; const FileName: string);
begin
  if Assigned(FOnNotify) then
    if TWatchAction(Action - 1) in FWatchActions then
      FOnNotify(Self, TWatchAction(Action - 1), FileName);
end;

end.

