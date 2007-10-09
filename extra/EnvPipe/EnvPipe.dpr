program EnvPipe;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Windows,
  TlHelp32;

function GetParentProcessId: Cardinal;
var
  Snapshot: THandle;
  lppe: TProcessEntry32;
  CurrentID: Cardinal;
begin
  Result := 0;
  CurrentID := GetCurrentProcessId;
  Snapshot := CreateToolhelp32Snapshot(th32cs_SnapProcess, 0);
  try
    lppe.dwSize := SizeOf(TProcessEntry32);
    if Process32First(Snapshot, lppe) then repeat
      // loop to find the current process
      if lppe.th32ProcessID = CurrentID then begin
        // store ID of current process's parent
        Result := lppe.th32ParentProcessID;
        // found it; exit the loop
        Break;
      end;
    // terminate loop when there are no more processes.
    until not Process32Next(Snapshot, lppe);
    // couldn't find CurrentID.
    if GetLastError = ERROR_NO_MORE_FILES then exit;
    // find the parent process
    if Process32First(Snapshot, lppe) then repeat
      if lppe.th32ProcessID = Result then begin
        Result := lppe.th32ProcessID;
        // found it; exit the loop.
        break;
      end;
    until not Process32Next(Snapshot, lppe);
  finally
    CloseHandle(Snapshot);
  end;
end;

function GetCommandOutput: string;
var
  executable: String;
  parameters: String;
  i: Integer;
var
  secinfo: TSecurityAttributes;
  startinfo: TStartupInfo;
  processinfo: TProcessInformation;
  stdoutread, stdoutwrite: THandle;
  buffer: array[0..255] of Char;
  bytes: Cardinal;
begin
  ExitCode := 1;
  Result := '';

  executable := ParamStr(2);
  parameters := '';
  if ParamCount > 2 then for i := 3 to ParamCount do begin
    parameters := parameters + ParamStr(i) + ' ';
  end;

  try
    FillChar(secinfo, sizeof(TSecurityAttributes), 0);
    secinfo.nLength := SizeOf(secinfo);
    secinfo.bInheritHandle := true;
    secinfo.lpSecurityDescriptor := nil;

    if not CreatePipe(stdoutread, stdoutwrite, @secinfo, 0) then begin
      WriteLn(Format('%d: Failed to create pipe for stdout substitute.', [GetLastError]));
      Exit;
    end;

//    SetNamedPipeHandleState(stdoutread, PIPE_NOWAIT, nil, nil)

    SetHandleInformation(stdoutread, HANDLE_FLAG_INHERIT, 0);
    
    FillChar(startinfo, sizeof(TStartupInfo), 0);
    startinfo.cb := sizeof(TStartupInfo);
    startinfo.dwFlags := STARTF_USESTDHANDLES;
    startinfo.wShowWindow := SW_HIDE;
    startinfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
    startinfo.hStdOutput := stdoutwrite;
    startinfo.hStdError := stdoutwrite;

    FillChar(processinfo, sizeof(TProcessInformation), 0);

    if not CreateProcess(PChar(executable), PChar(parameters), nil, nil, true, 0, nil, nil, startinfo, processinfo) then begin
      WriteLn(Format('%d: CreateProcess failed.', [GetLastError]));
      Exit;
    end;

    CloseHandle(stdoutwrite);

    repeat
      if not ReadFile(stdoutread, buffer, 255, bytes, nil) then begin
        OutputDebugString(PChar(Format('%d: ReadFile failed after %d bytes consumed.', [GetLastError, Length(Result)])));
        // Non-fatal?  MSDN example just stops reading when any ReadFile error occurs.
      end;

      if bytes > 0 then begin
        buffer[bytes] := #0;
        Result := Trim(Result) + buffer;
      end;
    until (bytes = 0) or (WaitForSingleObject(processinfo.hProcess, 0) = 0);

    if not GetExitCodeProcess(processinfo.hProcess, Cardinal(ExitCode)) then begin
      ExitCode := 1;
      WriteLn(Format('%d: GetExitCodeProcess failed.', [GetLastError]));
      Exit;
    end;

  finally
    CloseHandle(processinfo.hThread);
    CloseHandle(processinfo.hProcess);
    CloseHandle(stdoutread);
  end;
end;

{ NtQueryInformation constants } 

const 
  ProcessBasicInformation = 0; 

{ NtQueryInformation types } 

type 
  TProcessBasicInformation = packed record
    ExitStatus: Integer; 
    PebBaseAddress: Pointer;
    AffinityMask: Integer;
    BasePriority: Integer; 
    UniqueProcessID: Integer; 
    InheritedFromUniqueProcessID: Integer;
  end; 

  TNtQueryInformationProcess = 
    function(hProcess: THandle; ProcessInformationClass: Integer;
      var ProcessInformation; ProcessInformationLength: Integer;
      var ReturnLength: Integer): Integer; stdcall;

type
  PNtUnicodeString = ^TNtUnicodeString;
  TNtUnicodeString = packed record
    Length: Word;
    MaximumLength: Word;
    Buffer: PWideChar;
  end;

type
  PCurDir = ^TCurDir;
  TCurDir = packed record
    DosPath: TNtUnicodeString;
    Handle : THandle;
  end;

type
  PNtAnsiString = ^TNtAnsiString;
  TNtAnsiString = packed record
    Length: Word;
    MaximumLength: Word;
    Buffer: PAnsiChar;
  end;

type
  PRtlDriveLetterCurDir = ^TRtlDriveLetterCurDir;
  TRtlDriveLetterCurDir = packed record
    Flags    : Word;
    Length   : Word;
    TimeStamp: Cardinal;
    DosPath  : TNtAnsiString;
  end;

type
  PRtlUserProcessParameters = ^TRtlUserProcessParameters;
  TRtlUserProcessParameters = record
    MaximumLength    : Cardinal;
    Length           : Cardinal;
    Flags            : Cardinal;
    DebugFlags       : Cardinal;
    ConsoleHandle    : THandle;
    ConsoleFlags     : Cardinal;
    StandardInput    : THandle;
    StandardOutput   : THandle;
    StandardError    : THandle;
    CurrentDirectory : TCurDir;
    DllPath          : TNtUnicodeString;
    ImagePathName    : TNtUnicodeString;
    CommandLine      : TNtUnicodeString;
    Environment      : Pointer;
    StartingX        : Cardinal;
    StartingY        : Cardinal;
    CountX           : Cardinal;
    CountY           : Cardinal;
    CountCharsX      : Cardinal;
    CountCharsY      : Cardinal;
    FillAttribute    : Cardinal;
    WindowFlags      : Cardinal;
    ShowWindowFlags  : Cardinal;
    WindowTitle      : TNtUnicodeString;
    DesktopInfo      : TNtUnicodeString;
    ShellInfo        : TNtUnicodeString;
    RuntimeData      : TNtUnicodeString;
    CurrentDirectores: Array [0..31] of TRtlDriveLetterCurDir;
  end;

type
  PPebLdrData = ^TPebLdrData;
  TPebLdrData = packed record
    Length                         : Cardinal;        // 0h
    Initialized                    : LongBool;        // 4h
    SsHandle                       : THandle;         // 8h
    InLoadOrderModuleList          : TListEntry;      // 0Ch
    InMemoryOrderModuleList        : TListEntry;      // 14h
    InInitializationOrderModuleList: TListEntry;      // 1Ch
  end;

type
  PPebFreeBlock = ^TPebFreeBlock;
  TPebFreeBlock = record
    Next: PPebFreeBlock;
    Size: Cardinal;
  end;
  
type
  TProcessEnvironmentBlock = packed record
    InheritedAddressSpace         : Boolean;
    ReadImageFileExecOptions      : Boolean;
    BeingDebugged                 : Boolean;
    SpareBool                     : Boolean;
    Mutant                        : Pointer;
    ImageBaseAddress              : Pointer;
    Ldr                           : PPebLdrData;
    ProcessParameters             : PRtlUserProcessParameters;
    SubSystemData                 : Pointer;
    ProcessHeap                   : Pointer;
    FastPebLock                   : Pointer;
    FastPebLockRoutine            : Pointer;
    FastPebUnlockRoutine          : Pointer;
    EnvironmentUpdateCount        : Cardinal;
    KernelCallbackTable           : Pointer;
    case Integer of
      4: (
        EventLogSection           : Pointer;
        EventLog                  : Pointer);
      5: (
        SystemReserved            : Array [0..1] of Cardinal;
  { end; }
    FreeList                      : PPebFreeBlock;
    TlsExpansionCounter           : Cardinal;
    TlsBitmap                     : Pointer;
    TlsBitmapBits                 : Array [0..1] of Cardinal;
    ReadOnlySharedMemoryBase      : Pointer;
    ReadOnlySharedMemoryHeap      : Pointer;
    ReadOnlyStaticServerData      : ^Pointer;
    AnsiCodePageData              : Pointer;
    OemCodePageData               : Pointer;
    UnicodeCaseTableData          : Pointer;
    NumberOfProcessors            : Cardinal;
    NtGlobalFlag                  : Cardinal;
    Unknown                       : Cardinal;
    CriticalSectionTimeout        : TLargeInteger;
    HeapSegmentReserve            : Cardinal;
    HeapSegmentCommit             : Cardinal;
    HeapDeCommitTotalFreeThreshold: Cardinal;
    HeapDeCommitFreeBlockThreshold: Cardinal;
    NumberOfHeaps                 : Cardinal;
    MaximumNumberOfHeaps          : Cardinal;
    ProcessHeaps                  : ^Pointer;
    GdiSharedHandleTable          : Pointer;
    ProcessStarterHelper          : Pointer;
    GdiDCAttributeList            : Cardinal;
    LoaderLock                    : Pointer;
    OSMajorVersion                : Cardinal;
    OSMinorVersion                : Cardinal;
    OSBuildNumber                 : Word;
    OSCSDVersion                  : Word;
    OSPlatformId                  : Cardinal;
    ImageSubsystem                : Cardinal;
    ImageSubsystemMajorVersion    : Cardinal;
    ImageSubsystemMinorVersion    : Cardinal;
    ImageProcessAffinityMask      : Cardinal;
    GdiHandleBuffer               : Array [0..33] of Cardinal;
    PostProcessInitRoutine        : ^Pointer;
    TlsExpansionBitmap            : Pointer;
    TlsExpansionBitmapBits        : Array [0..31] of Cardinal;
    SessionId                     : Cardinal;
    AppCompatInfo                 : Pointer;
    CSDVersion                    : TNtUnicodeString);
  end;

var
  ppid: Cardinal;
  pwnd: THandle;
  hNTDLL: Integer;
  ntQip: TNtQueryInformationProcess;
  pbi: TProcessBasicInformation;
  peb: TProcessEnvironmentBlock;
  upp: TRtlUserProcessParameters;
  env: array[0..16383] of WideChar;
  envstr: PWideChar;
  size, last: Cardinal;
  p, len: Cardinal;
  retLen: Integer;
  retLenU: Cardinal;
  err: Cardinal;
  res: Boolean;
  line: String;
  variable: String;

begin
  // Being slightly annoyed with:
  //   for /f "usebackq" %f in (`svnversion .`) do set wcver=%f
  // failing to set %errorlevel% to something sensible when svnversion
  // is not available, I thought I'd try and roll my own utility to
  // set an environment variable to the output of a program.  Turned
  // out to be a bit more complex than I had initially envisioned..

  // All data structures has been leeched off from MSDN and
  // various anonymous usenet and web postings found via Google.

  if ParamCount < 2 then begin
    WriteLn('Usage:');
    WriteLn('   1) SET <env var>=');
    WriteLn('   2) ' + ParamStr(0) + ' <env var> <command...>');
    Exit;
  end;

  variable := ParamStr(1);
  ppid := GetParentProcessId;
  WriteLn(Format('Parent process id is %d', [ppid]));
  line := Trim(GetCommandOutput);
  WriteLn(Format('Output is %s', [line])); 

  pwnd := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ or PROCESS_VM_WRITE or PROCESS_VM_OPERATION, false, ppid);
  if pwnd = 0 then begin
    WriteLn(Format('%d: OpenProcess failed.', [GetLastError]));
  end;
  hNTDLL := LoadLibrary('NTDLL.DLL');
  if hNTDLL = 0 then begin
    WriteLn(Format('%d: LoadLibrary("ntdll.dll") failed.', [GetLastError]));
  end else begin
    ntQip := GetProcAddress(hNTDLL, 'NtQueryInformationProcess');
    if not Assigned(ntQip) then begin
      WriteLn(Format('%d: GetProcAddress("NtQueryInformationProcess") failed.', [GetLastError]));
    end else begin
      ntQip(pwnd, ProcessBasicInformation, pbi, sizeof(pbi), retLen);
      OutputDebugString(PChar(Format('PEB base address: %x', [Cardinal(pbi.PebBaseAddress)])));
      if pbi.PebBaseAddress = nil then begin
        WriteLn(Format('Failed to get correct PEB base address: %d', [GetLastError]));
      end else begin
        if not ReadProcessMemory(pwnd, pbi.PebBaseAddress, @peb, sizeof(peb), retLenU) then begin
          WriteLn(Format('%d: ReadProcessMemory failed.', [GetLastError]));
        end else begin
          OutputDebugString(PChar(Format('UPP base address: %x', [Cardinal(peb.ProcessParameters)])));
          if not ReadProcessMemory(pwnd, peb.ProcessParameters, @upp, sizeof(upp), retLenU) then begin
            WriteLn(Format('%d: ReadProcessMemory failed.', [GetLastError]));
          end else begin
            OutputDebugString(PChar(Format('Environment base address: %x', [Cardinal(upp.Environment)])));
            size := sizeof(env);
            last := size;
            SetLastError(0);
            repeat
              // Quite probably always a multiple of the heap manager's block size, which is usually 4kB all year around.
              res := ReadProcessMemory(pwnd, upp.Environment, @env, size, retLenU);
              err := GetLastError;
              OutputDebugString(PChar(Format('ReadProcessMemory read %d out of %d attempted bytes of environment.', [retLenU, size])));
              last := last shr 1;
              if last = 0 then last := 1;
              if res then size := size + last;
              if (not res) and (err = 299) then size := size - last;
            until ((not res) and (err <> 299)) or ((last = 1) and res);
            if err <> 299 then begin
              WriteLn(Format('%d: ReadProcessMemory failed.', [err]));
            end else begin
              len := Succ(Length(variable + '=' + line));
              GetMem(envstr, SizeOf(WideChar) * len);
              StringToWideChar(variable + '=' + line, envstr, len);
              for p := 1 to size do begin
                if env[p - 1] + env[p] = #0#0 then begin
                  if p + (SizeOf(WideChar) * len) > size then begin
                    WriteLn('Failed: not enough room in environment.');
                  end else begin
                    // No effort is done to remove existing values.
                    // cmd.exe seems to cope fine with duplicate values,
                    // but to be on the safe side it's probably a good
                    // idea to clear them beforehand..
                    Move(envstr[0], env[p], SizeOf(WideChar) * len);
                    if not WriteProcessMemory(pwnd, upp.Environment, @env, size, retLenU) then begin
                      WriteLn(Format('%d: WriteProcessMemory failed; wrote %d out of %d attempted bytes of environment.', [GetLastError, retLenU, size]));
                    end else begin
                      OutputDebugString(PChar(Format('WriteProcessMemory successfully wrote %d out of %d attempted bytes of environment.', [retLenU, size])));
                    end;
                    Break;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end.

