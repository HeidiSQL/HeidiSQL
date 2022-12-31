program updater;

{ A console window which terminates running HeidiSQL instances and moves the downloaded update file to
  its desired directory. }

{$APPTYPE CONSOLE}

{$R manifest.RES}
// (un)comment the following resource inclusion to vary the binary size. Update checker trusts the same file size before overwriting the old one.
{$R ..\icon.RES}

uses
  Winapi.Windows, Winapi.Messages, Winapi.TlHelp32, Winapi.PsAPI, Winapi.ShellAPI, System.SysUtils;

var
  BackupPath, AppPath, DownloadPath: String;
  RestartTaskName, RestartCmd, RestartParams: String;

const
  AppName = 'HeidiSQL';
  QuitTimeout = 20000; // We long we're gracefully waiting for a window to be gone, in milliseconds
  TerminatedCheck = 500; // Interval between checks if host application is gone

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
const
  SleepSecondsOnError: Integer=10;
begin
  // Display status message
  WriteLn(Text);
  if IsError then begin
    Writeln('Exiting in '+SleepSecondsOnError.ToString+' seconds...');
    Sleep(SleepSecondsOnError * 1000);
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
  if (LowerCase(WndPath) <> LowerCase(AppPath)) and (LowerCase(WndPath) <> LowerCase(ExtractFilename(AppPath))) then
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

// Main program code
begin
  try
    AppPath := Paramstr(1);
    DownloadPath := ParamStr(2);
    RestartTaskName := ParamStr(3);

    // Parameter syntax check
    if (AppPath = '') or (DownloadPath = '') or (RestartTaskName = '') then begin
      Status('Syntax: '+ExtractFilename(Paramstr(0))+' OldFile.exe NewFile.exe RestartTaskName'+#13#10+
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
    if not CopyFile(PChar(AppPath), PChar(BackupPath), False) then
      Status('Failed to create backup file "'+BackupPath+'" from "'+AppPath+'"', True)
    else begin
      DeleteFile(PChar(AppPath));
      Status('Success.');
    end;

    // Move update file to final path
    Status('Moving downloaded file to desired directory ...');
    // Issue #1616: MoveFile() does not work when target directory is a symlink, so we prefer CopyFile + DeleteFile
    if not CopyFile(PChar(DownloadPath), PChar(AppPath), False) then
      Status('Failed to copy file "'+DownloadPath+'" to "'+AppPath+'"', True)
    else begin
      DeleteFile(PChar(DownloadPath));
      Status('Success. Restarting '+AppName+' through task "'+RestartTaskName+'" now ...');
      RestartCmd := 'schtasks';
      RestartParams := '/Run /TN ' + RestartTaskName;
      ShellExecute(0, 'open', PChar(RestartCmd), PChar(RestartParams), '', SW_HIDE);
    end;

    Status('Exiting in 10 seconds...');
    Sleep(10000);
  except
    on E: Exception do
      Status(E.ClassName + ': ' + E.Message, True);
  end;
end.
