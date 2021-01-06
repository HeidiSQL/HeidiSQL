{ TlHelp32 types for fpc }

const
  TH32CS_SNAPHEAPLIST = $00000001;
  TH32CS_SNAPPROCESS  = $00000002;
  TH32CS_SNAPTHREAD   = $00000004;
  TH32CS_SNAPMODULE   = $00000008;
  TH32CS_SNAPALL      = TH32CS_SNAPHEAPLIST or TH32CS_SNAPPROCESS or
    TH32CS_SNAPTHREAD or TH32CS_SNAPMODULE;
  TH32CS_INHERIT      = $80000000;

  TLS_OUT_OF_INDEXES = DWORD($FFFFFFFF);  // FPC does not declare TLS_OUT_OF_INDEXES.

type 
 tagTHREADENTRY32 = record
    dwSize: DWORD;
    cntUsage: DWORD;
    th32ThreadID: DWORD;       
    th32OwnerProcessID: DWORD; 
    tpBasePri: Longint;
    tpDeltaPri: Longint;
    dwFlags: DWORD;
  end;
  THREADENTRY32 = tagTHREADENTRY32;
  PTHREADENTRY32 = ^tagTHREADENTRY32;
  LPTHREADENTRY32 = ^tagTHREADENTRY32;
  TThreadEntry32 = tagTHREADENTRY32;

  TThread32First = function (hSnapshot: THandle; var lpte: TThreadEntry32): BOOL stdcall;
  TThread32Next = function (hSnapshot: THandle; var lpte: TThreadENtry32): BOOL stdcall;
  TCreateToolhelp32Snapshot = function (dwFlags, th32ProcessID: DWORD): THandle; stdcall;  
