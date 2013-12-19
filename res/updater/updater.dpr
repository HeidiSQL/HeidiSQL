program updater;

{ A window which terminates running HeidiSQL instances and moves the downloaded update file to
  its desired directory. Avoids to use any VCL unit, to keep the executable small. }

{$R manifest.RES}

uses
  Windows, Messages, Tlhelp32, psapi;

var
  WClass: TWndClass;
  BackupPath, AppPath, DownloadPath: String;
  hAppHandle, HLabel: HWND;
  AppMsg: TMsg;

const
  AppName = 'HeidiSQL';
  WindowPadding = 10;
  WindowWidth = 600;
  WindowHeight = 80;
  QuitTimeout = 20000; // We long we're gracefully waiting for a window to be gone, in milliseconds
  TerminatedCheck = 200; // Interval between checks if host application is gone
  PathDelim = '\';



{ We don't include SysUtils unit, so we need to implement our own versions of some basic functions here }

function FileExists(Filename: String): Boolean;
var
  Find: THandle;
  Data: TWin32FindData;
begin
  Find := FindFirstFile(PChar(Filename), Data);
  Result := Find <> INVALID_HANDLE_VALUE;
end;

function IntToStr(Value: Int64): String;
var
  Minus : Boolean;
begin
  Result := '';
  if Value = 0 then
    Result := '0';
  Minus := Value < 0;
  if Minus then
    Value := -Value;
  while Value > 0 do begin
    Result := Char((Value mod 10) + Integer('0')) + Result;
    Value := Value div 10;
  end;
  if Minus then
    Result := '-' + Result;
end;

function ExtractFileName(const FileName: string): string;
var
  i: Integer;
begin
  for i:=Length(Filename) downto 0 do
    if Filename[i] = PathDelim then
      break;
  Result := Copy(FileName, i+1, MaxInt);
end;

function ExtractFilePath(const FileName: string): string;
var
  i: Integer;
begin
  for i:=Length(Filename) downto 0 do
    if Filename[i] = PathDelim then
      break;
  Result := Copy(FileName, 1, i);
end;


function Trim(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  if (L > 0) and (S[I] > ' ') and (S[L] > ' ') then Exit(S);
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then Exit('');
  while S[L] <= ' ' do Dec(L);
  Result := Copy(S, I, L - I + 1);
end;


function GetEXEFromHandle(const wnd: HWND) : string;
var
  pid: dword;
  wv: TOSVersionInfo;
  hProcess, hp: THandle;
  ContinueLoop: Boolean;
  aProcessEntry32: TProcessEntry32;
  buf: array[0..MAX_PATH] of char;
begin
  Result := '';

  // R U kiddin', man?
  if wnd = 0 then
    Exit;

  // Get running OS
  ZeroMemory(@wv, SizeOf(TOSVersionInfo));
  wv.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(wv);

  // Get process ID
  GetWindowThreadProcessID(wnd, @pid);

  hProcess := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if hProcess <> INVALID_HANDLE_VALUE then try
    aProcessEntry32.dwSize := SizeOf(aProcessEntry32);
    ContinueLoop := Process32First(hProcess, aProcessEntry32);

    while ContinueLoop do begin
      if aProcessEntry32.th32ProcessID = pid then begin
        ZeroMemory(@buf, SizeOf(buf));
        LStrCpy(buf, aProcessEntry32.szExeFile);
        if wv.dwPlatformId = VER_PLATFORM_WIN32_NT then begin
          hp := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, pid);
          if hProcess <> INVALID_HANDLE_VALUE then try
            ZeroMemory(@buf, SizeOf(buf));
            GetModuleFileNameEx(hp, 0, buf, SizeOf(buf));
          finally
            CloseHandle(hp);
          end;
        end;
        break;
      end;
      ContinueLoop := Process32Next(hProcess,aProcessEntry32);
    end;
  finally
    CloseHandle(hProcess);
  end;

  Result := String(buf);
end;


procedure Status(Text: String; IsError: Boolean=False);
begin
  // Display status message on label
  SendMessage(HLabel, WM_SETTEXT, 1, Integer(PChar(Text)) );
  UpdateWindow(hLabel);
  if IsError then begin
    Sleep(4000);
    Halt(1);
  end;
end;


function EnumAllInstances(Wnd: HWND; LParam: LPARAM): Bool; stdcall;
var
  WndTitle, WndPath, Hint: String;
  i, WaitTime: Integer;
begin
  // Callback function which passes one window handle
  // EnumWindows will stop processing if we return false
  Result := True;
  WndPath := GetEXEFromHandle(Wnd);
  if (WndPath <> AppPath) and (WndPath <> ExtractFilename(AppPath)) then
    Exit;

  SetLength(WndTitle, 256);
  for i:=1 to 256 do
    WndTitle[i] := ' ';
  GetWindowText(Wnd, PChar(WndTitle), 256);
  WndTitle := Trim(WndTitle);

  Hint := 'Closing "'+WndTitle+'"';
  Status(Hint);
  WaitTime := 0;
  PostMessage(Wnd, WM_CLOSE, 0, 0);
  while WaitTime < QuitTimeout do begin
    Sleep(TerminatedCheck);
    Inc(WaitTime, TerminatedCheck);
    Status(Hint + ', wait time left: '+IntToStr((QuitTimeout - WaitTime) div 1000)+' seconds.');
    // IsWindow() returns true on dialogs we have just hidden
    if not IsWindowVisible(Wnd) then
      break;
  end;
  if IsWindowVisible(Wnd) then begin
    Status('Error: Could not terminate session '+WndTitle, true);
    Result := False;
  end;
end;


// Callback function for Timer
procedure FormShow(wnd: HWND; uMsg: UINT; idEvent: UINT; dwTime: DWORD); stdcall;
var
  SUInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
begin
  KillTimer(hAppHandle, 0);
  AppPath := Paramstr(1);
  DownloadPath := ParamStr(2);

  // Paremeter syntax check
  if (AppPath = '') or (DownloadPath = '') then begin
    Status('Syntax: '+ExtractFilename(Paramstr(0))+' OldFile.exe NewFile.exe'+#13#10+
      'Please don''t execute this file directly.', True);
  end;
  if (not FileExists(AppPath)) or (not FileExists(DownloadPath)) then
    Status('Error: Either target file "'+AppPath+'" or download file "'+DownloadPath+'" does not exist.', True);

  // Terminate running instances
  Status('Close running '+AppName+' instances ...');
  EnumWindows(@EnumAllInstances, 0);

  // Backup old .exe to working directory
  Status('Creating backup of old file ...');
  BackupPath := ExtractFilepath(Paramstr(0))+ExtractFilename(AppPath)+'.backup.exe';
  if FileExists(BackupPath) then
    DeleteFile(PChar(BackupPath));
  if not MoveFile(PChar(AppPath), PChar(BackupPath)) then
    Status('Failed to create backup file "'+BackupPath+'" from "'+AppPath+'"', True)
  else
    Status('Success.');

  // Move update file to final path
  Status('Moving downloaded file to desired directory ...');
  if not MoveFile(PChar(DownloadPath), PChar(AppPath)) then
    Status('Failed to move file "'+DownloadPath+'" to "'+AppPath+'"', True)
  else begin
    Status('Success. Restarting '+AppName+' now ...');
    FillChar(SUInfo, SizeOf(SUInfo), #0);
    SUInfo.cb := SizeOf(SUInfo);
    SUInfo.dwFlags := STARTF_USESHOWWINDOW;
    SUInfo.wShowWindow := SW_SHOWNORMAL;
    CreateProcess(
      nil,
      PChar(AppPath),
      nil,
      nil,
      False,
      CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
      nil,
      PChar(ExtractFilePath(AppPath)),
      SUInfo,
      ProcInfo
      );
  end;
  PostQuitMessage(0);
end;


function WindowProc(hWnd, msg, wpr, lpr: Longint): Longint; stdcall;
var
  x, y: integer;
  Font: HFont;
begin
  // Custom window procedure
  case msg of

    WM_CREATE: begin
      // Center window
      x := GetSystemMetrics(SM_CXSCREEN);
      y := GetSystemMetrics(SM_CYSCREEN);
      MoveWindow(hWnd,
        (x div 2) - (WindowWidth div 2),
        (y div 2) - (WindowHeight div 2),
        WindowWidth,
        WindowHeight,
        true);
      // Create status label
      HLabel := CreateWindow(
        'STATIC',                           // Class name
        'Status:',                          // Label's text
        WS_VISIBLE or WS_CHILD or SS_LEFT,  // Styles
        WindowPadding,                      // X pos
        WindowPadding,                      // Y pos
        WindowWidth - 2*WindowPadding,      // Width
        WindowHeight - 2*WindowPadding,     // Height
        hWnd,                               // Parent hwnd
        0,                                  // ID
        hAppHandle,                         // HInstance of program
        nil                                 // Params for main window
        );
      // Cosmetics
      Font := Createfont(-11, 0, 0, 0, 0, 0, 0, 0, ANSI_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH, 'Tahoma');
      SendMessage(HLabel, WM_SETFONT, Font, 1);
      SetBkColor(hwnd, COLOR_BTNFACE+1);
    end;

    WM_SHOWWINDOW: SetTimer(hAppHandle, 0, 200, @FormShow);

    WM_DESTROY: PostQuitMessage(0);

  end;

  Result := DefWindowProc(hWnd, msg, wpr, lpr);
end;



// Main program goes here
begin
  // Define window class
  WClass.hInstance := hInstance;
  WClass.style := CS_HREDRAW or CS_VREDRAW;
  WClass.hIcon := LoadIcon(hInstance, IDI_WINLOGO);
  WClass.lpfnWndProc := @WindowProc;
  WClass.hbrBackground := COLOR_BTNFACE+1;
  WClass.lpszClassName := 'WndClass';
  WClass.hCursor := LoadCursor(0, IDC_ARROW);
  WClass.cbClsExtra := 0;
  WClass.cbWndExtra := 0;
  WClass.lpszMenuName := '';
  RegisterClass(WClass);

  // Create form
  hAppHandle := CreateWindow(
    WClass.lpszClassName,
    AppName+' Updater',
    WS_POPUPWINDOW or WS_CAPTION or WS_VISIBLE,
    100, // Default x + y coordinates, will be centered in WM_CREATE
    100,
    WindowWidth,
    WindowHeight,
    0,
    0,
    hInstance,
    nil
    );

  // Message loop
  while GetMessage(AppMsg, 0, 0, 0) do begin
    TranslateMessage(AppMsg);
    DispatchMessage(AppMsg);
  end;
  ExitCode := AppMsg.wParam;
end.
