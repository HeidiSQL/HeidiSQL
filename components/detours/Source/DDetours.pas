// **************************************************************************************************
// Delphi Detours Library.
// Unit DDetours
// https://github.com/MahdiSafsafi/delphi-detours-library

// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is DDetours.pas.
//
// Contributor(s):
// David Millington
// RRUZ
//
// The Initial Developer of the Original Code is Mahdi Safsafi [SMP3].
// Portions created by Mahdi Safsafi . are Copyright (C) 2013-2017 Mahdi Safsafi .
// All Rights Reserved.
//
// **************************************************************************************************

{ ===============================> CHANGE LOG <======================================================

  Jan 24,2015:
  +Added support for vtable patching.
  +Added GetNHook/IsHooked support for Interface.

  Jan 20,2015:
  +Added support to hook Delphi Interface by name.

  Version2 , Mahdi Safsafi:
  +Many bug fix.
  +Added new hooking model architecture.
  +Added multi hook support.
  +Added COM hook support.
  +Added instruction maping feature.
  +Added hook detecting feature.
  +Added BeginHooks/EndHooks.
  +Added BeginUnHooks/EndUnHooks.
  +Added IDetours interface.
  +Added MultiNop instructions support.
  +Generate better opcodes.
  +Improved support for x64.
  +Improved AllocMemAt function.
  ====================================================================================================== }

unit DDetours;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF FPC}

interface

{$I Defs.inc}

uses
  InstDecode,
  CPUID,
  SysUtils,
{$IFDEF DXE2UP}
  WinApi.Windows,
{$ELSE !DXE2UP}
  Windows,
{$ENDIF DXE2UP}
  Classes
{$IFDEF MustUseGenerics}
    , Generics.Collections //
    , Typinfo, RTTI
{$ENDIF MustUseGenerics}
    ;

type
  InterceptException = Exception;
{$IFNDEF DXE2UP}
  SIZE_T = NativeUInt;
{$ENDIF !DXE2UP}

const
  { Maximum allowed number of hooks. }
  MAX_HOOKS = 7;

  { Options }
  Root = 1; // Future use.
  ST = 2; // Suspend Threads.

{$IFDEF BuildThreadSafe}
  {
    Make the new version compatible with the old one !
    ===================================================
    I don't guarantee that i will continue
    supporting old release .
    ==> You sould update your code
    when you have some time !
  }
  v1compatibility = ST;
{$ELSE !BuildThreadSafe}
  v1compatibility = 0;
{$ENDIF BuildThreadSafe}
  { ======================================================================================================================================================= }
function InterceptCreate(const TargetProc, InterceptProc: Pointer; Options: Byte = v1compatibility): Pointer; overload;
function InterceptCreate(const TargetInterface; MethodIndex: Integer; const InterceptProc: Pointer; Options: Byte = v1compatibility): Pointer; overload;
function InterceptCreate(const Module, MethodName: String; const InterceptProc: Pointer; ForceLoadModule: Boolean = True; Options: Byte = v1compatibility)
  : Pointer; overload;
procedure InterceptCreate(const TargetProc, InterceptProc: Pointer; var TrampoLine: Pointer; Options: Byte = v1compatibility); overload;
function InterceptRemove(const Trampo: Pointer; Options: Byte = v1compatibility): Integer;
function GetNHook(const TargetProc: Pointer): ShortInt; overload;
function GetNHook(const TargetInterface; MethodIndex: Integer): ShortInt; overload;
function IsHooked(const TargetProc: Pointer): Boolean; overload;
function IsHooked(const TargetInterface; MethodIndex: Integer): Boolean; overload;
function PatchVt(const TargetInterface; MethodIndex: Integer; InterceptProc: Pointer): Pointer;
function UnPatchVt(const Trampo: Pointer): Boolean;

{$IFDEF MustUseGenerics }
function InterceptCreate(const TargetInterface; const MethodName: String; const InterceptProc: Pointer; Options: Byte = v1compatibility): Pointer; overload;
function GetNHook(const TargetInterface; const MethodName: String): ShortInt; overload;
function IsHooked(const TargetInterface; const MethodName: String): Boolean; overload;
function BeginHooks(): Boolean;
function EndHooks(): Boolean;
function BeginUnHooks(): Boolean;
function EndUnHooks(): Boolean;

type
  DetourException = Exception;

  IGenericCast<T> = interface(IInterface)
    ['{B19D793C-3225-439C-A2F3-04A72D41879E}']
    function TToPointer(const _T: T): Pointer;
    function PointerToT(const _P: Pointer): T;
  end;

  IDetours<T> = interface(IInterface)
    ['{04552E96-C716-4378-BE9A-CD383D20AB91}']
    function NextHook: T;
    function GetInstalled: Boolean;
    function GetHookCount: ShortInt;
    procedure SetHook(const TargetProc, InterceptProc: T);
    procedure Enable;
    procedure Disable;
    property TrampoLine: T read NextHook; // Call the original
  end;

  TDetours<T> = class(TInterfacedObject, IGenericCast<T>, IDetours<T>)
  private
    FTargetProc: PByte;
    FInterceptProc: PByte;
    FNextHook: T;
    function __TToPointer(const _T): Pointer;
    function __PointerToT(const _P): T;
    function TToPointer(const _T: T): Pointer;
    function PointerToT(const _P: Pointer): T;
    function NextHook: T;
    function GetInstalled: Boolean;
    function GetHookCount: ShortInt;
  protected
    function CheckTType: Boolean;
    procedure SetHook(const TargetProc, InterceptProc: T);
  public
    constructor Create(const TargetProc, InterceptProc: T);
    destructor Destroy; override;
    procedure Enable;
    procedure Disable;
    property TrampoLine: T read NextHook; // Call the original
    property Installed: Boolean read GetInstalled;
    property nHook: ShortInt read GetHookCount;
  end;

const
  SInvalidTType = '%s must be procedure.';
  SDetoursNotInstalled = 'Detour is not installed; trampoline pointer is nil';
{$ENDIF MustUseGenerics }
{$IFDEF FPC}

var
  Critical: TRTLCriticalSection;
{$ENDIF FPC}

implementation

{$OVERFLOWCHECKS OFF}
{$IFNDEF FPC}

{ Delphi }
uses
{$IFDEF DXE2UP}
  WinApi.TLHelp32;
{$ELSE !DXE2UP}
  TLHelp32;
{$ENDIF DXE2UP}
{$ELSE FPC}

type
  tagTHREADENTRY32 = record
    dwSize: DWORD;
    cntUsage: DWORD;
    th32ThreadID: DWORD; // this thread
    th32OwnerProcessID: DWORD; // Process this thread is associated with
    tpBasePri: Longint;
    tpDeltaPri: Longint;
    dwFlags: DWORD;
  end;

  THREADENTRY32 = tagTHREADENTRY32;
  PTHREADENTRY32 = ^tagTHREADENTRY32;
  LPTHREADENTRY32 = ^tagTHREADENTRY32;
  TThreadEntry32 = tagTHREADENTRY32;

  TCreateToolhelp32Snapshot = function(dwFlags, th32ProcessID: DWORD): THandle stdcall;
  TThread32First = function(hSnapshot: THandle; var lpte: TThreadEntry32): BOOL stdcall;
  TThread32Next = function(hSnapshot: THandle; var lpte: TThreadEntry32): BOOL stdcall;

var
  CreateToolhelp32Snapshot: TCreateToolhelp32Snapshot;
  Thread32First: TThread32First;
  Thread32Next: TThread32Next;

const
  TH32CS_SNAPTHREAD = $00000004;
{$ENDIF !FPC}

type
  TOpenThread = function(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwThreadId: DWORD): THandle; stdcall;

var
  OpenThread: TOpenThread;
  hKernel: THandle;
  OpenThreadExist: Boolean = False;
  FreeKernel: Boolean = False;
  SizeOfAlloc: DWORD = 0; // See initialization !

const
  { Instructions OpCodes }
  opJmpRelz = $E9;
  opJmpRelb = $EB;
  opJmpMem = $25FF;
  opTestb = $85;
  opPrfOpSize = $66;
  opPrfAddrSize = $67;
  opNop = $90;

  fDscrHasTmp = $01;
  DscrSigSize = $08;
  TrampoSize = 64;
  TmpSize = 32;

type
  TDscrSig = array [0 .. DscrSigSize - 1] of Byte;
  TTrampoData = array [0 .. TrampoSize - 1] of Byte;

  TVirtualProtect = function(lpAddress: Pointer; dwSize: SIZE_T; flNewProtect: DWORD; var OldProtect: DWORD): BOOL; stdcall;
  TVirtualAlloc = function(lpvAddress: Pointer; dwSize: SIZE_T; flAllocationType, flProtect: DWORD): Pointer; stdcall;
  TVirtualQuery = function(lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: SIZE_T): SIZE_T; stdcall;
  TFlushInstructionCache = function(hProcess: THandle; const lpBaseAddress: Pointer; dwSize: SIZE_T): BOOL; stdcall;
  TGetCurrentProcess = function: THandle; stdcall;
  TVirtualFree = function(lpAddress: Pointer; dwSize: SIZE_T; dwFreeType: DWORD): BOOL; stdcall;

  TInternalFuncs = record
    VirtualAlloc: TVirtualAlloc;
    VirtualFree: TVirtualFree;
    VirtualProtect: TVirtualProtect;
    VirtualQuery: TVirtualQuery;
    FlushInstructionCache: TFlushInstructionCache;
    GetCurrentProcess: TGetCurrentProcess;
  end;

var
  InternalFuncs: TInternalFuncs;

const
  { Descriptor Signature }
{$IFDEF CPUX64}
  DscrSig: TDscrSig = ( //
    $90, { NOP }
    $40, { REX }
    $40, { REX }
    $40, { REX }
    $0F, { ESCAPE TWO BYTE }
    $1F, { HINT_NOP }
    $F3, { PRF }
    $F3 { PRF }
    );
{$ELSE !CPUX64}
  DscrSig: TDscrSig = ( //
    $90, { NOP }
    $40, { INC EAX }
    $48, { DEC EAX }
    $90, { NOP }
    $0F, { ESCAPE TWO BYTE }
    $1F, { HINT_NOP }
    $F3, { PRF }
    $F3 { PRF }
    );
{$ENDIF CPUX64}

type
  TTrampoInfo = record
    Addr: PByte; // Pointer to first trampoline instruction .
    Size: Byte; // Stolen bytes size .
    PData: PByte; // Original Stolen bytes.
  end;

  PTrampoInfo = ^TTrampoInfo;

  TJmpMem = packed record
    OpCode: WORD; // $0F$25
    Disp32: Integer;
  end;

  PJmpMem = ^TJmpMem;

  TDescriptor = packed record
    Sig: TDscrSig; { Table signature. }
    DscrAddr: PByte; { Pointer that hold jmp address (if Used)! }
    nHook: Byte; { Number of hooks . }
    Flags: Byte; { Reserved for future use! }
    ExMem: PByte; { Reserved for jmp (if used) & for Trampoline ! }
    OrgPtr: PByte; { Original Target Proc address. }
    Trampo: PTrampoInfo; { Pointer to TrampoInfo struct. }
    { Array that hold jmp destination address. }
    JmpAddrs: array [0 .. MAX_HOOKS] of PByte;
    {
      Mark the beginning of descriptor code executing .
      ==> Must be NOP .
    }
    CodeEntry: Byte;
    { Jmp Instruction for NextHook call and Trampoline call ! }
    JmpMems: array [0 .. MAX_HOOKS] of TJmpMem;
  end;

  PDescriptor = ^TDescriptor;

  TNextHook = packed record
    ID: Byte; { Hook ID . }
    PDscr: PDescriptor;
  end;

  PNextHook = ^TNextHook;

  TThreadsIDList = class(TList);

  TInterceptMonitor = class(TObject)
    class procedure InternalCreate;
    class procedure InternalDestroy;
    class var FLock: TObject;
    class procedure Enter;
    class procedure Leave;
  end;

  TIntercept = class(TObject)
  private
    FOptions: Byte;
    FList: TThreadsIDList;
    class function GetRoot(P: PByte): PByte;
  public
    constructor Create(Options: Byte); virtual;
    destructor Destroy; override;

  protected
    function GetDescriptor(P: PByte): PDescriptor;
    function IsValidDescriptor(P: PByte): Boolean;
    function CreateNewDescriptor: PDescriptor;
    procedure InsertDescriptor(PAt: PByte; PDscr: PDescriptor);
    procedure RemoveDescriptor(PDscr: PDescriptor);
    function InstallHook(TargetProc, InterceptProc: PByte; const Options: Byte = $00): PByte;
    function AddHook(PDscr: PDescriptor; InterceptProc: PByte; const Options: Byte = $00): PByte;
    function RemoveHook(Trampo: PByte): Integer;
  end;

const
  { Error Str }
  ErrFuncSize = 'Size of function is too small, risk to override others adjacent functions.';
  ErrJmpInvalid = 'Invalid JMP Type.';
  ErrJmpInvalid64 = 'Invalid JMP Type for x64.';
  ErrJmpInvalid32 = 'Invalid JMP Type for x32.';
  ErrJmpInvalidDstSave = 'Invalid DstSave Address pointer.';
  ErrMultiNopNotSup = 'Multi Bytes Nop Instructions not supported by your CPU.';
  ErrRipDisp = 'Failed to correcr RIP Displacement.';
  ErrTrampoSize = 'Exceed maximum TrampoSize.';
  ErrMaxHook = 'Exceed maximum allowed of hooks.';
  ErrTargetProc = 'Invalid TargetProc Pointer.';
  ErrInterceptProc = 'Invalid InterceptProc Pointer.';
  ErrInvalidDscr = 'Invalid Descriptor.';
  ErrInvalidTrampo = 'Invalid TrampoLine Pointer.';
  ErrBgnUnHooks = 'BeginUnHooks must be called outside BeginHooks/EndHooks.';

  { JMP Type }
  tJmpNone = 0;
  tJmpRel8 = 1;
  tJmpRel16 = 2;
  tJmpRel32 = 3;
  tJmpMem16 = 4;
  tJmpMem32 = 5;
  tJmpMem64 = 6;
  tJmpRipZ = 7;

{$IFDEF CPUX64}
  tJmpMemN = tJmpMem64;
{$ELSE !CPUX64}
  tJmpMemN = tJmpMem32;
{$ENDIF CPUX64}
  { Jmp Type To Size }
  JmpTypeToSize: array [0 .. 7] of Byte = ( //
    0, { None }
    2, { tJmpRel8  = $EB + Rel8 }
    4, { tJmpRel16 = OpSizePrf + $E9 + Rel16 }
    5, { tJmpRel32 = $E9 + Rel32 }
    7, { tJmpMem16 = OpSizePrf + $FF /4 + Disp32 }
    6, { tJmpMem32 = $FF /4 + Disp32 }
    6, { tJmpMem64 = $FF /4 + Disp32 }
    14 { tJmpRipZ  = $FF /4 + Disp32 + DQ }
    );

  SizeToJmpType: array [0 .. 4] of Byte = ( //
{$IFDEF CPUX86}
    tJmpRel8, { db }
    tJmpRel16, { dw }
    tJmpRel32, { dd }
    tJmpMem32, { dd }
    tJmpMem32 { dd }
{$ELSE !CPUX86}
    tJmpRel8, { db }
    tJmpRel32, { dw }
    tJmpRel32, { dd }
    tJmpMem64, { dq }
    tJmpMem64 { dq }
{$ENDIF CPUX86}
    );

  {
    // Useful function when debugging !
    procedure DbgPrint(const msg: string; Value: Int64); overload;
    var
    s: string;
    begin
    s := msg;
    if s <> EmptyStr then
    s := s + ' = ' + IntToStr(Value)
    else
    s := IntToStr(Value);
    OutputDebugStringW(PChar(s));
    end;

    procedure DbgPrint(const msg: string); overload;
    begin
    OutputDebugStringW(PChar(msg));
    end;

    procedure ShowMsg(const msg: string);
    begin
    MessageBoxW(0, PChar(msg), nil, MB_OK);
    end;
  }
const
  THREAD_SUSPEND_RESUME = $0002;

function SuspendAllThreads(RTID: TThreadsIDList): Boolean;
var
  hSnap: THandle;
  PID: DWORD;
  te: TThreadEntry32;
  nCount: DWORD;
  hThread: THandle;
  Next: Boolean;
  CurrentThreadId: Cardinal;
begin
  PID := GetCurrentProcessId;
  CurrentThreadId := GetCurrentThreadId;
  hSnap := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, PID);
  Result := hSnap <> INVALID_HANDLE_VALUE;
  if Result then
  begin
    te.dwSize := SizeOf(TThreadEntry32);
    Next := Thread32First(hSnap, te);
    while Next do
    begin
      if (te.th32OwnerProcessID = PID) and (te.th32ThreadID <> CurrentThreadId) then
      begin
        { Allow the caller thread to access the Detours .
          => Suspend all threads, except the current thread . }
        hThread := OpenThread(THREAD_SUSPEND_RESUME, False, te.th32ThreadID);
        if hThread <> INVALID_HANDLE_VALUE then
        begin
          nCount := SuspendThread(hThread);
          if nCount <> DWORD(-1) then // thread's previously was running  .
            { Only add threads that was running before suspending them ! }
            RTID.Add(Pointer(NativeUInt(te.th32ThreadID)));
          CloseHandle(hThread);
        end;
      end;
      Next := Thread32Next(hSnap, te);
    end;
    CloseHandle(hSnap);
  end;
end;

function ResumeSuspendedThreads(RTID: TThreadsIDList): Boolean;
var
  i: Integer;
  TID: DWORD;
  hThread: THandle;
begin
  Result := False;
  if Assigned(RTID) then
    for i := 0 to RTID.Count - 1 do
    begin
      TID := DWORD(RTID.Items[i]);
      if TID <> DWORD(-1) then
      begin
        Result := True;
        hThread := OpenThread(THREAD_SUSPEND_RESUME, False, TID);
        if hThread <> INVALID_HANDLE_VALUE then
        begin
          ResumeThread(hThread);
          CloseHandle(hThread);
        end;
      end;
    end;
end;

function SetMemPermission(const P: Pointer; const Size: NativeUInt; const NewProtect: DWORD): DWORD;
const
  PAGE_EXECUTE_FLAGS = PAGE_EXECUTE or PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY;
begin
  Result := 0;
  if Assigned(P) and (Size > 0) and (NewProtect > 0) then
  begin
    if InternalFuncs.VirtualProtect(P, Size, NewProtect, Result) then
      if (NewProtect and PAGE_EXECUTE_FLAGS <> 0) then
        {
          If the protected region will be executed
          => We need to update the cpu cache !
        }
        InternalFuncs.FlushInstructionCache(InternalFuncs.GetCurrentProcess(), P, Size);
  end;
end;

function GetDispDataSize(PInst: PInstruction): ShortInt;
begin
  Result := 0;
  if PInst^.Disp.Flags and dfUsed <> 0 then
  begin
    if PInst^.Archi = CPUX32 then
    begin
      if PInst^.Prefixes and Prf_OpSize <> 0 then
        Exit(ops16bits)
      else
        Exit(ops32bits);
    end else begin
      case PInst^.OperandFlags of
        opdD64:
          begin
            {
              Defaults to O64 in PM64.
              PrfOpSize results in O16.
            }
            if PInst^.Prefixes and Prf_OpSize <> 0 then
              Exit(ops16bits)
            else
              Exit(ops64bits);
          end;
        opdF64, opdDv64:
          begin
            { The operand size is forced to a 64-bit operand size in PM64 ! }
            Exit(ops64bits);
          end;
        opdDf64:
          begin
            {
              Defaults to O64 in PM64.
              PrfOpSize results in O16 in AMD64.
              PrfOpSize is ignored in EM64T.
            }
            if (CPUVendor = vAMD) and (PInst^.Prefixes and Prf_OpSize <> 0) then
              Exit(ops16bits)
            else
              Exit(ops64bits);
          end;
      else
        begin
          if PInst^.Rex.W then
            Exit(ops64bits)
          else if (PInst^.Prefixes and Prf_OpSize <> 0) then
            Exit(ops16bits)
          else
            Exit(ops32bits);
        end;
      end;
    end;
  end;
end;

function fDecodeInst(PInst: PInstruction): ShortInt;
var
  IsNxtInstData: Boolean;
begin
  { Include VEX decoding if the cpu support it! }
  if (VEX in CPUEncoding) then
    PInst.Options := DecodeVex;

  Result := DecodeInst(PInst);

{$IFDEF CPUX64}
  IsNxtInstData := ((PInst^.Disp.Flags and (dfUsed or dfRip) = (dfUsed or dfRip)) and (PInst^.Disp.Value = 0));
{$ELSE !CPUX64}
  IsNxtInstData := (PInst^.Disp.Value = UInt64(PInst^.NextInst));
{$ENDIF CPUX64}
  if IsNxtInstData then
  begin
    {
      Check if the Next Instruction is data !
      If so , That's mean it's not a valid instruction .
      We must skip this data ..
      otherwise , disassembling next instructions will fail !
    }
    Inc(Result, GetDispDataSize(PInst));
    PInst^.InstSize := Result;
  end;
end;

function RoundMultipleOf(const Value, n: NativeUInt): NativeUInt; {$IFDEF MustInline}inline; {$ENDIF}
begin
  if Value = 0 then
    Exit(n);
  Result := ((Value + (n - 1)) and not(n - 1));
end;

function AllocMemAt(const Addr: Pointer; const MemSize, flProtect: DWORD): Pointer;
var
  mbi: TMemoryBasicInformation;
  SysInfo: TSystemInfo;
  pBase: PByte;
  P: PByte;
  Q: PByte;
  pMax, pMin: PByte;
  dwAllocGran: DWORD;
begin
  { Alloc memory on the specific nearest address from the Addr . }

  Result := nil;
  P := PByte(Addr);
  if not Assigned(P) then
  begin
    Result := InternalFuncs.VirtualAlloc(nil, MemSize, MEM_RESERVE or MEM_COMMIT, flProtect);
    Exit;
  end;

  GetSystemInfo(SysInfo);
  pMin := SysInfo.lpMinimumApplicationAddress;
  pMax := SysInfo.lpMaximumApplicationAddress;
  dwAllocGran := SysInfo.dwAllocationGranularity;

  if (P < pMin) or (P > pMax) then
    Exit;
  if InternalFuncs.VirtualQuery(P, mbi, SizeOf(mbi)) = 0 then
    Exit;

  pBase := mbi.BaseAddress;
  Q := pBase;
  while Q < pMax do
  begin
    if InternalFuncs.VirtualQuery(Q, mbi, SizeOf(mbi)) = 0 then
      Exit;
    if (mbi.State = MEM_FREE) and (mbi.RegionSize >= dwAllocGran) and (mbi.RegionSize >= MemSize) then
    begin
      { The address (P) must be multiple of the allocation granularity (dwAllocationGranularity) . }
      P := PByte(RoundMultipleOf(NativeUInt(Q), dwAllocGran));
      Result := InternalFuncs.VirtualAlloc(P, MemSize, MEM_RESERVE or MEM_COMMIT, flProtect);
      if Assigned(Result) then
        Exit;
    end;
    Inc(Q, mbi.RegionSize); // Next Region .
  end;
  {
    If thre is no memory available in the range [Addr - pMax]
    try to allocate at the range [pMin - Addr]
  }
  Q := pBase;
  while Q > pMin do
  begin
    if InternalFuncs.VirtualQuery(Q, mbi, SizeOf(mbi)) = 0 then
      Exit;
    if (mbi.State = MEM_FREE) and (mbi.RegionSize >= dwAllocGran) and (mbi.RegionSize >= MemSize) then
    begin
      P := PByte(RoundMultipleOf(NativeUInt(Q), dwAllocGran));
      Result := InternalFuncs.VirtualAlloc(P, MemSize, MEM_RESERVE or MEM_COMMIT, flProtect);
      if Assigned(Result) then
        Exit;
    end;
    Dec(Q, mbi.RegionSize); // Previous Region.
  end;
end;

function TryAllocMemAt(const Addr: Pointer; const MemSize, flProtect: DWORD): Pointer;
var
  MEM_64: DWORD;
begin
  MEM_64 := 0;
  Result := AllocMemAt(Addr, MemSize, flProtect);
  if not Assigned(Result) then
  begin
{$IFDEF CPUX64}
    { Allocates memory at the highest possible address }
    if (UInt64(Addr) and $FFFFFFFF00000000 <> 0) then
      MEM_64 := MEM_TOP_DOWN;
{$ENDIF CPUX64}
    Result := InternalFuncs.VirtualAlloc(nil, MemSize, MEM_RESERVE or MEM_COMMIT or MEM_64, flProtect);
  end;
end;

function InsertJmp(Src, Dst: PByte; JmpType: Byte; const DstSave: PByte = nil): ShortInt;
var
  Offset32: Int32;
  Offset64: Int64;
  JmpSize: Byte;
begin
  Result := 1;
  JmpSize := JmpTypeToSize[JmpType];
  Offset32 := Int32(UInt64(Dst) - UInt64(Src)) - JmpSize;
  case JmpType of
    tJmpNone:
      begin
        raise InterceptException.Create(ErrJmpInvalid);
      end;
    tJmpRel8:
      begin
        PByte(Src)^ := opJmpRelb;
        Inc(Src);
        PInt8(Src)^ := Int8(Offset32);
      end;
    tJmpRel16:
      begin
{$IFDEF CPUX64}
        {
          JMP Rel16
          ==> Not supported on x64!
        }
        raise InterceptException.Create(ErrJmpInvalid64);
{$ENDIF CPUX64}
        PByte(Src)^ := opPrfOpSize;
        Inc(Src);
        PByte(Src)^ := opJmpRelz;
        Inc(Src);
        PInt16(Src)^ := Int16(Offset32);
      end;
    tJmpRel32:
      begin
        PByte(Src)^ := opJmpRelz;
        Inc(Src);
        PInt32(Src)^ := Offset32;
      end;
    tJmpMem16:
      begin
{$IFDEF CPUX64}
        {
          JMP WORD [012345]
          ==> Not supported on x64!
        }
        raise InterceptException.Create(ErrJmpInvalid64);
{$ENDIF CPUX64}
        if not Assigned(DstSave) then
          raise InterceptException.Create(ErrJmpInvalidDstSave);
        PByte(Src)^ := opPrfOpSize;
        Inc(Src);
        PWord(Src)^ := opJmpMem;
        Inc(Src, 2);
        PUInt32(Src)^ := UInt32(DstSave);
        PUInt16(DstSave)^ := UInt16(Dst);
      end;
    tJmpMem32:
      begin
{$IFDEF CPUX64}
        {
          JMP DWORD [012345]
          ==> Not supported on x64!
        }
        raise InterceptException.Create(ErrJmpInvalid64);
{$ENDIF CPUX64}
        if not Assigned(DstSave) then
          raise InterceptException.Create(ErrJmpInvalidDstSave);
        PWord(Src)^ := opJmpMem;
        Inc(Src, 2);
        PUInt32(Src)^ := UInt32(DstSave);
        PUInt32(DstSave)^ := UInt32(Dst);
      end;
    tJmpMem64:
      begin
{$IFDEF CPUX86}
        {
          JMP QWORD [0123456789]
          ==> Not supported on x32!
        }
        raise InterceptException.Create(ErrJmpInvalid32);
{$ENDIF CPUX86}
        if not Assigned(DstSave) then
          raise InterceptException.Create(ErrJmpInvalidDstSave);
        { RIP Disp ! }
        PUInt64(DstSave)^ := UInt64(Dst);
        Offset64 := Int64(UInt64(DstSave) - UInt64(Src)) - JmpSize;
        Offset32 := Integer(Offset64);
        { If the distance between DispAddr and Src exceed 32-bits then
          the only way to insert a jump
          is to use tJmpRipZ method ! }
        if Offset32 <> Offset64 then
          Exit(-1);
        PWord(Src)^ := opJmpMem;
        Inc(Src, 2);
        PInt32(Src)^ := Offset32;
      end;
    tJmpRipZ:
      begin
{$IFDEF CPUX86}
        raise InterceptException.Create(ErrJmpInvalid32);
{$ENDIF CPUX86}
        {
          This is the most harder way to insert a jump !
          Why ?
          because we are going to mix code & data !
          Thats mean when disassembling instructions after
          this branch .. you will have a corrupted dissambled
          structure !

          The only way to detect this kind of jmp is:
          to use fDecodeInst rather than DecodeInst routine .

          ==> We should avoid using this kind of jmp
          in the original target proc .

          ==> It's Ok to use in others situation .
        }

        PWord(Src)^ := opJmpMem;
        Inc(Src, 2);
        PInt32(Src)^ := $00;
        Inc(Src, 4);
        PUInt64(Src)^ := UInt64(Dst);
      end;
  end;
end;

function GetUInt64Size(const Value: UInt64): Byte;
begin
  if UInt8(Value) = Value then
    Exit(1)
  else if UInt16(Value) = Value then
    Exit(2)
  else if UInt32(Value) = Value then
    Exit(4)
  else
    Exit(8);
end;

function GetInt64Size(const Value: Int64): Byte;
begin
  if Int8(Value) = Value then
    Exit(1)
  else if Int16(Value) = Value then
    Exit(2)
  else if Int32(Value) = Value then
    Exit(4)
  else
    Exit(8);
end;

function GetJmpType(Src, Dst, DstSave: PByte): Byte;
var
  Offset: Int64;
  OffsetSize: Byte;
begin
  Offset := Int64(UInt64(Src) - UInt64(Dst));
  OffsetSize := GetInt64Size(Offset);
  Result := SizeToJmpType[OffsetSize shr 1];
{$IFDEF CPUX64}
  if Result = tJmpMem64 then
  begin
    if not Assigned(DstSave) then
      raise InterceptException.Create(ErrJmpInvalidDstSave);
    Offset := Int64(UInt64(DstSave) - UInt64(Src)) - 7;
    if Int32(Offset) <> Offset then
      Exit(tJmpRipZ);
  end;
{$ENDIF CPUX64}
end;

{$IFDEF UseMultiBytesNop}

const
  Nop9: array [0 .. 8] of Byte = ($66, $0F, $1F, $84, $00, $00, $00, $00, $00);
  Nop8: array [0 .. 7] of Byte = ($0F, $1F, $84, $00, $00, $00, $00, $00);
  Nop7: array [0 .. 6] of Byte = ($0F, $1F, $80, $00, $00, $00, $00);
  Nop6: array [0 .. 5] of Byte = ($66, $0F, $1F, $44, $00, $00);
  Nop5: array [0 .. 4] of Byte = ($0F, $1F, $44, $00, $00);
  Nop4: array [0 .. 3] of Byte = ($0F, $1F, $40, $00);
  Nop3: array [0 .. 2] of Byte = ($0F, $1F, $00);
  Nop2: array [0 .. 1] of Byte = ($66, $90);
  Nop1: array [0 .. 0] of Byte = ($90);
  MultiNops: array [0 .. 8] of PByte = ( //
    @Nop1, { Standard Nop }
    @Nop2, { 2 Bytes Nop }
    @Nop3, { 3 Bytes Nop }
    @Nop4, { 4 Bytes Nop }
    @Nop5, { 5 Bytes Nop }
    @Nop6, { 6 Bytes Nop }
    @Nop7, { 7 Bytes Nop }
    @Nop8, { 8 Bytes Nop }
    @Nop9 { 9 Bytes Nop }

    );

function IsMultiBytesNop(const P: PByte; Len: ShortInt = 0): Boolean;
var
  i: Integer;
  nL: Integer;
begin
  for i := Length(MultiNops) downto 1 do
  begin
    nL := i;
    Result := CompareMem(MultiNops[i - 1], P, nL);
    if Result then
    begin
      if Len < 0 then
        Exit;
      Result := (nL = Len);
      if Result then
        Exit;
    end;
  end;
end;

procedure FillMultiNop(var Buff; Size: Integer);
var
  i: Integer;
  nL: Byte;
  P: PByte;
begin
  { Multi Bytes Nop Instructions seems to be
    faster to execute rather than
    the traditional (NOP x n) instructions.

    However it's not supported by all CPU !
    ==> Use FillNop(P,Size,True)!

    ==> CPUID implement a routine to detect
    if the CPU support Multi Bytes Nop .
  }
  if not(iMultiNop in CPUInsts) then
    raise InterceptException.Create(ErrMultiNopNotSup);

  P := PByte(@Buff);
  for i := Length(MultiNops) downto 1 do
  begin
    nL := i;
    if Size = 0 then
      Break;
    while Size >= nL do
    begin
      Move(MultiNops[i - 1]^, P^, nL);
      Dec(Size, nL);
      Inc(P, nL);
    end;
  end;
end;
{$ENDIF UseMultiBytesNop}

function IsNop(const P: PByte; Len: ShortInt; MultiBytesNop: Boolean = False): Boolean;
var
  i: Integer;
  Q: PByte;
begin
  { Return True if the first instructions are nop/multi nop ! }
  Result := False;
  Q := P;
{$IFDEF UseMultiBytesNop}
  if (MultiBytesNop and (iMultiNop in CPUInsts)) then
    Result := IsMultiBytesNop(P, Len)
  else
{$ENDIF UseMultiBytesNop}
    for i := 0 to Len - 1 do
    begin
      Result := (Q^ = opNop);
      if not Result then
        Exit;
      Inc(Q); // Next Byte.
    end;
end;

procedure FillNop(var P; const Size: Integer; const MultiBytesNop: Boolean = False); {$IFDEF MustInline}inline; {$ENDIF}
begin
{$IFDEF UseMultiBytesNop}
  if (MultiBytesNop and (iMultiNop in CPUInsts)) then
    FillMultiNop(P, Size)
  else
{$ENDIF UseMultiBytesNop}
    FillChar(PByte(@P)^, Size, opNop);
end;

function GetPrefixesCount(Prefixes: WORD): Byte;
var
  Prf: WORD;
  i: Byte;
begin
  { Get prefixes count used by the instruction. }
  Result := 0;
  if Prefixes = 0 then
    Exit;

  Prf := 0;
  i := 0;
  Prefixes := Prefixes and not Prf_VEX;
  while Prf < $8000 do
  begin
    Prf := (1 shl i);
    if (Prf and Prefixes = Prf) then
      Inc(Result);
    Inc(i);
  end;
end;

function GetInstOpCodes(PInst: PInstruction; P: PByte): ShortInt;
var
  nPrfs: Byte;
begin
  {
    Return opcodes length
    Instruction OpCodes in arg P  .
  }
  Result := 0;
  FillChar(P^, MAX_INST_LENGTH_N, $90);
  nPrfs := GetPrefixesCount(PInst^.Prefixes);
  Inc(Result, nPrfs);
  case PInst^.OpTable of
    tbTwoByte:
      if PInst^.Prefixes and Prf_VEX3 = 0 then
        Inc(Result); // $0F
    tbThreeByte:
      begin
        if PInst^.Prefixes and Prf_VEX3 = 0 then
          Inc(Result, 2); // 0F + 38|3A !
      end;
    tbFPU: Inc(Result, 2); // [$D8..$D9] + ModRm !
  end;
  if PInst^.Prefixes and Prf_Vex2 <> 0 then
    Inc(Result); // VEX.P0
  if PInst^.Prefixes and Prf_VEX3 <> 0 then
    Inc(Result, 2); // VEX.P0 + VEX.P1
  if PInst^.OpKind = kGrp then
    Inc(Result, 2) // Group + ModRm
  else
    Inc(Result); // OpCode
  if Assigned(P) then
    Move(PInst^.Addr^, P^, Result);
end;

function CorrectJ(PInst: PInstruction; NewAddr: PByte): Integer;
const
  { Convert LOOP instruction to relative word jcc ! }
  LOOP_To_JccZ: array [0 .. 3] of WORD = ($850F, $840F, $840F, $9090);
  { Convert LOOP instruction to relative byte jcc ! }
  LOOP_To_JccB: array [0 .. 3] of Byte = ($75, $74, $75, $90);
var
  Offset: Int64;
  POpc: PByte;
  // Opcsz: Integer;
  NOpc: DWORD;
  PQ: PByte;
  Relsz: Byte;
  JmpType: Byte;
  JmpSize: Byte;
  function GetJccOpCode(RelSize: Byte): DWORD;
  var
    LOp: Byte;
    Opc: array [0 .. 3] of Byte;
  begin
    FillChar(PByte(@Opc[0])^, 4, #0);
    LOp := PInst^.OpCode and $F;
    case RelSize of
      ops8bits:
        begin
          Opc[0] := $70 or LOp;
        end;
      ops16bits:
        begin
          Opc[0] := opPrfOpSize;
          Opc[1] := $0F;
          Opc[2] := $80 or LOp;
        end;
      ops32bits:
        begin
          Opc[0] := $0F;
          Opc[1] := $80 or LOp;
        end;
    end;
    Result := PDWORD(@Opc[0])^;
  end;

begin
  PQ := NewAddr;
  JmpSize := 0;
  GetMem(POpc, MAX_INST_LENGTH_N + 1);
  try
    // Opcsz := GetInstOpCodes(PInst, POpc);
    Offset := Int64(UInt64(PInst^.Branch.Target) - UInt64(PQ) - 6);
    Relsz := GetInt64Size(Offset);
{$IFDEF CPUX64}
    if Relsz = ops16bits then
      Relsz := ops32bits;
{$ENDIF CPUX64}
    if PInst^.OpType and otJcc = 0 then
    begin
      { Not Jcc ! }
      if PInst^.OpCode in [$E0 .. $E2] then
      begin
        { LOOPNE/LOOPZ/LOOP }
        if Relsz = ops8bits then
        begin
          if PInst^.Prefixes and Prf_AddrSize <> 0 then
          begin
            PQ^ := opPrfAddrSize;
            Inc(PQ);
          end;
          PQ^ := PInst^.OpCode;
          Inc(PQ);
          PQ^ := Int8(Offset);
          Inc(PQ);
        end
        else
          case PInst^.AddrMode of
            am16:
              begin
                { Dec CX ! }
{$IFDEF CPUX64}
                { . $49 result in REX
                  ==> Use $FF group !
                }
                PQ^ := opPrfOpSize;
                Inc(PQ);
                PQ^ := $FF;
                Inc(PQ);
                PQ^ := $C9;
                Inc(PQ);
{$ELSE !CPUX64}
                PQ^ := opPrfOpSize;
                Inc(PQ);
                PQ^ := $49;
                Inc(PQ);
{$ENDIF CPUX64}
              end;
            am32:
              begin
                { Dec ECX ! }
{$IFDEF CPUX64}
                PQ^ := $FF;
                Inc(PQ);
                PQ^ := $C9;
                Inc(PQ);
{$ELSE !CPUX64}
                PQ^ := $49;
                Inc(PQ);
{$ENDIF CPUX64}
              end;
            am64:
              begin
                { Dec RCX ! }
                PQ^ := $48; // REX.W = True !
                Inc(PQ);
                PQ^ := $FF;
                Inc(PQ);
                PQ^ := $C9;
                Inc(PQ);
              end;
          end;
        case Relsz of
          ops16bits:
            begin
              Offset := Int64(UInt64(PInst^.Branch.Target) - UInt64(PQ) - 5);
              PQ^ := opPrfOpSize;
              Inc(PQ);
              PWord(PQ)^ := LOOP_To_JccZ[PInst^.OpCode and 3];
              Inc(PQ, 2);
              PInt16(PQ)^ := Int16(Offset);
              Inc(PQ, 2);
            end;
          ops32bits:
            begin
              Offset := Int64(UInt64(PInst^.Branch.Target) - UInt64(PQ) - 6);
              PWord(PQ)^ := LOOP_To_JccZ[PInst^.OpCode and 3];
              Inc(PQ, 2);
              PInt32(PQ)^ := Int32(Offset);
              Inc(PQ, 4);
            end;
          ops64bits:
            begin
              {
                Dec RCX
                Jcc @Tmp
                Jmp @NextInst
                @Tmp:
                Jmp @LoopDst
              }
              { Insert Jcc ! }
              PQ^ := LOOP_To_JccB[PInst^.OpCode and 3];
              Inc(PQ);
              PQ^ := 2;
              Inc(PQ);
              { Insert Jmp NextInst }
              PQ^ := opJmpRelb;
              Inc(PQ);
              PQ^ := 14;
              Inc(PQ);
              { Insert Jmp @LoopDst }
              InsertJmp(PQ, PInst.Branch.Target, tJmpRipZ);
              Inc(PQ, 14);
            end;
        end;
      end
      else if PInst^.OpCode = $E3 then
      begin
        { JCXZ/JECX/JRCX }
        if Relsz = ops8bits then
        begin
          if PInst^.Prefixes and Prf_AddrSize <> 0 then
          begin
            PQ^ := opPrfAddrSize;
            Inc(PQ);
          end;
          PQ^ := PInst^.OpCode;
          Inc(PQ);
          PQ^ := Int8(Offset);
          Inc(PQ);
        end
        else
          case PInst^.AddrMode of
            am16:
              begin
                { TEST CX,CX }
                PQ^ := opPrfOpSize;
                Inc(PQ);
                PQ^ := opTestb;
                Inc(PQ);
                PQ^ := $C9; // ModRm [Mod = 3; Reg = Rm = CX = 1]
                Inc(PQ);
              end;
            am32:
              begin
                { TEST ECX,ECX }
                PQ^ := opTestb;
                Inc(PQ);
                PQ^ := $C9;
                Inc(PQ);
              end;
            am64:
              begin
                { TEST RCX,RCX }
                PQ^ := $48; // REX.W = True !
                Inc(PQ);
                PQ^ := opTestb;
                Inc(PQ);
                PQ^ := $C9;
                Inc(PQ);
              end;
          end;
        case Relsz of
          ops16bits:
            begin
              {
                TEST CX,CX
                JZ @Dst
              }
              Offset := Int64(UInt64(PInst^.Branch.Target) - UInt64(PQ) - 5);
              PQ^ := opPrfOpSize;
              Inc(PQ);
              PQ^ := $0F;
              Inc(PQ);
              PQ^ := $84; // JZ !
              Inc(PQ);
              PInt16(PQ)^ := Int16(Offset);
              Inc(PQ, 2);
            end;
          ops32bits:
            begin
              {
                TEST ECX,ECX
                JZ @Dst
              }
              Offset := Int64(UInt64(PInst^.Branch.Target) - UInt64(PQ) - 6);
              PQ^ := $0F;
              Inc(PQ);
              PQ^ := $84; // JZ !
              Inc(PQ);
              PInt32(PQ)^ := Int32(Offset);
              Inc(PQ, 4);
            end;
          ops64bits:
            begin
              {
                TEST RCX,RCX
                JZ @Tmp
                Jmp @NextInst
                @Tmp:
                Jmp @Dst
              }
              { Insert JZ ! }
              PQ^ := $74;
              Inc(PQ);
              PQ^ := 2;
              Inc(PQ);
              { Insert Jmp NextInst }
              PQ^ := opJmpRelb;
              Inc(PQ);
              PQ^ := 14;
              Inc(PQ);
              { Insert Jmp @Dst }
              InsertJmp(PQ, PInst.Branch.Target, tJmpRipZ);
              Inc(PQ, 14);
            end;
        end;
      end;
    end else begin
      { Jcc ! }
      NOpc := GetJccOpCode(Relsz);
      case Relsz of
        ops8bits:
          begin
            Offset := Int64(UInt64(PInst^.Branch.Target) - UInt64(PQ) - 2);
            PInt8(PQ)^ := UInt8(NOpc);
            Inc(PQ);
            PInt8(PQ)^ := Int8(Offset);
            Inc(PQ);
          end;
        ops16bits:
          begin
            Offset := Int64(UInt64(PInst^.Branch.Target) - UInt64(PQ) - 5);
            PUInt32(PQ)^ := UInt32(NOpc);
            Inc(PQ, 3);
            PInt16(PQ)^ := Int16(Offset);
            Inc(PQ, 2);
          end;
        ops32bits:
          begin
            Offset := Int64(UInt64(PInst^.Branch.Target) - UInt64(PQ) - 6);
            PUInt16(PQ)^ := UInt16(NOpc);
            Inc(PQ, 2);
            PInt32(PQ)^ := Int32(Offset);
            Inc(PQ, 4);
          end;
        ops64bits:
          begin
            {
              Unfortunately there is no Jcc Rel 64bits !

              ===>Original implementation<===
              test eax,eax
              jz @DstAddr

              ===>New implementation<===
              test eax,eax
              Q:      jz @tmp
              Q+2:    jmp @NextInst
              Q+4:    @tmp:
              Q+4:    jmp @DstAddr
              Q+4+jmpsize:        [DstAddr]
              @NextInstruction:
            }

            { jz @tmp is guaranteed to be 2 Bytes in length ! }
            { Trampo.NextInstruction = Q + 4 + jmp @DstAddr Size }
            PQ^ := PInst^.OpCode;
            Inc(PQ);
            PQ^ := 2;
            Inc(PQ);
            JmpType := GetJmpType(NewAddr + 4, PInst^.Branch.Target, NewAddr + 4 + 6);
            JmpSize := JmpTypeToSize[JmpSize];
            if JmpType > tJmpRel32 then
              Inc(JmpSize, SizeOf(Pointer));

            { Jmp To Next Valid Instruction ! }
            PQ^ := opJmpRelb;
            Inc(PQ);
            PQ^ := JmpSize;
            Inc(PQ);

            InsertJmp(NewAddr + 4, PInst^.Branch.Target, JmpType, NewAddr + 4 + 6);
            Inc(PQ, JmpSize);
          end;
      end;
    end;
  finally
    FreeMem(POpc);
  end;
  Result := PQ - NewAddr;
  if Result = 00 then
  begin
    Move(PInst^.Addr^, NewAddr^, PInst^.InstSize);
    Result := PInst^.InstSize;
  end;
end;

function MakeModRm(iMod, Reg, Rm: Byte): Byte; {$IFDEF MustInline}inline; {$ENDIF}
begin
  Result := (iMod shl 6) or (Reg shl 3) or (Rm);
end;

function CorrectRipDisp(PInst: PInstruction; NewAddr: PByte): Integer;
type
  PNativeUInt = ^NativeUInt;
var
  Offset: Int64;
  P: PByte;
  rReg: Byte;
  POpc: PByte;
  pMR: PByte;
  pFrst: PByte;
  L: ShortInt;
begin
  pFrst := NewAddr;
  P := PInst^.NextInst;
  {
    If AddressMode is 32-bits :
    ===> EIP + Disp32 !
    else
    If AddressMode is 64-bits:
    ===> RIP + Disp32 !
  }
  if PInst^.AddrMode = am32 then
    P := PByte(UInt64(P) and $FFFFFFFF);

  P := P + Int64(PInst^.Disp.Value);

  Offset := Int64(UInt64(P) - UInt64(NewAddr) - PInst^.InstSize);
  if Int32(Offset) <> Offset then
  begin
    rReg := rEAX;
    if PInst^.ModRm.Flags and mfUsed <> 0 then
    begin
      Assert(PInst^.Disp.Flags and dfRip <> 0);
      if PInst^.ModRm.Reg = rReg then
        rReg := rECX;

      { PUSH UsedReg }
      PByte(NewAddr)^ := $50 + (rReg and $7);
      Inc(NewAddr);

{$IFDEF CPUX64}
      PByte(NewAddr)^ := $48; // REX.W!
      Inc(NewAddr);
{$ENDIF CPUX64}
      { MOV REG,Imm(NativeUInt) }
      PByte(NewAddr)^ := $B8 + (rReg and $7);
      Inc(NewAddr);
      PNativeUInt(NewAddr)^ := NativeUInt(P);
      Inc(NewAddr, SizeOf(NativeUInt));

      { Set the original instruction opcodes }
      POpc := GetMemory(MAX_INST_LENGTH_N);
      L := GetInstOpCodes(PInst, POpc);

      Move(PByte(@POpc[0])^, NewAddr^, L);
      Inc(NewAddr, L);
      pMR := NewAddr;
      if (PInst^.OpKind and kGrp <> 0) or (PInst^.OpTable = tbFPU) then
        Dec(pMR);

      PByte(pMR)^ := MakeModRm($00, PInst^.ModRm.Reg, rReg);
      Inc(pMR);
      NewAddr := pMR;

      { POP UsedReg }
      PByte(NewAddr)^ := $58 + (rReg and $7);
      Inc(NewAddr);

      FreeMemory(POpc);
      Exit(NewAddr - pFrst);
    end
    else
      raise InterceptException.Create(ErrRipDisp);
  end;
  Move(PInst^.Addr^, NewAddr^, PInst^.InstSize);
  Inc(NewAddr, PInst^.InstSize);
  PInt32(NewAddr - SizeOf(Int32))^ := Int32(Offset);

  Result := PInst^.InstSize;
end;

function CorrectJmpRel(PInst: PInstruction; NewAddr: PByte): Integer;
var
  JmpType: Byte;
begin
  JmpType := GetJmpType(NewAddr, PInst^.Branch.Target, NewAddr + 6);
  InsertJmp(NewAddr, PInst^.Branch.Target, JmpType, NewAddr + 6);
  Result := JmpTypeToSize[JmpType];
end;

function CorrectCallRel(PInst: PInstruction; NewAddr: PByte): Integer;
var
  Offset: Int64;
  Relsz: Byte;
  P: PByte;
begin
  P := NewAddr;
  Offset := Int64(UInt64(PInst^.Branch.Target) - UInt64(P) - 6);
  Relsz := GetInt64Size(Offset);
{$IFDEF CPUX64}
  { Only 32-bits relative offset is supported on x64! }
  if Relsz < ops32bits then
    Relsz := ops32bits;
{$ELSE !CPUX64}
  { Only 16/32-bits relative offset is supported on x32! }
  if Relsz < ops16bits then
    Relsz := ops32bits;
{$ENDIF CPUX64}
  case Relsz of
    ops16bits:
      begin
        Offset := Int64(UInt64(PInst^.Branch.Target) - UInt64(P) - 4);
        P^ := opPrfOpSize;
        Inc(P);
        P^ := $E8;
        Inc(P);
        PInt16(P)^ := Int16(Offset);
        Inc(P, 2);
      end;
    ops32bits:
      begin
        Offset := Int64(UInt64(PInst^.Branch.Target) - UInt64(P) - 5);
        P^ := $E8;
        Inc(P);
        PInt32(P)^ := Int32(Offset);
        Inc(P, 4);
      end;
    ops64bits:
      begin
        {
          64-bits Relative offset is not supported
          ==> Map to a new opcode !
        }
        {
          CALL [02]
          Jmp @NextValidInstruction
          dq : Call dst address !
          @NextValidInstruction:

        }
        P^ := $FF; // Group 5 !
        Inc(P);
        {
          ModRm.Mod = 00
          ModRm.Reg = 02
          ModRm.Rm = 05
          ==> ModRm = $15 !
        }
        P^ := MakeModRm($00, $02, $05);
        Inc(P);
        P^ := 2;
        Inc(P, 4);

        { Jmp Next Instruction ! }
        P^ := opJmpRelb;
        Inc(P);
        P^ := $08;
        Inc(P);
        PUInt64(P)^ := UInt64(PInst^.Branch.Target);
        Inc(P, SizeOf(UInt64));
      end;
  end;
  Result := P - NewAddr;
  if Result = 0 then
  begin
    Move(PInst^.Addr^, P^, PInst^.InstSize);
    Result := PInst^.InstSize;
  end;
end;

function MapInsts(Addr, NewAddr: PByte; Size: Integer): Integer;
var
  P, Q: PByte;
  PInst: PInstruction;
  sz, iz, nz: Integer;
begin
  { Map Data from Addr to NewAddr ! }
  { This function will fix Relative offset & RIP displacement . }
  Result := 0;
  sz := 0;
  P := Addr;
  Q := NewAddr;
  PInst := GetMemory(SizeOf(TInstruction));
  FillChar(PInst^, SizeOf(TInstruction), #0);

  PInst^.Archi := CPUX;
  PInst^.NextInst := P;
  PInst^.VirtualAddr := nil;

  while sz < Size do
  begin
    PInst^.Addr := PInst^.NextInst;
    iz := fDecodeInst(PInst);
    nz := iz;
    if PInst^.Disp.Flags and (dfUsed or dfRip) = (dfUsed or dfRip) then
      nz := CorrectRipDisp(PInst, Q)
    else if (PInst^.Branch.Falgs and bfRel = bfRel) then
    begin
      { Instruction use relative offset }
      if (PInst^.OpType = otJMP) then
        nz := CorrectJmpRel(PInst, Q)
      else if (PInst^.OpType = otCALL) then
        nz := CorrectCallRel(PInst, Q)
      else
        nz := CorrectJ(PInst, Q)
    end
    else
      Move(PInst^.Addr^, Q^, nz);
    Inc(Q, nz);
    Inc(Result, nz);
    Inc(sz, iz);
  end;
  FreeMemory(PInst);
end;

const
  arNone = $00;
  arPlus = $08;
  arMin = $10;
  arAdd = arPlus or $01;
  arSub = arMin or $01;
  arInc = arPlus or $02;
  arDec = arMin or $02;

{$WARN COMPARISON_TRUE OFF}

function GetInstArithmeticType(PInst: PInstruction): Byte;
  function IsInstAdd(PInst: PInstruction): Boolean;
  begin
    Result := False;
    if PInst^.OpTable = tbOneByte then
    begin
      if (PInst^.OpCode >= $00) and (PInst^.OpCode < $06) then
        Exit(True);
    end;
    if (PInst^.OpKind = kGrp) and (PInst^.ModRm.Reg = $00) then
    begin
      if (PInst^.OpCode > $7F) and (PInst^.OpCode < $84) then
        Exit(True);
    end;
  end;
  function IsInstSub(PInst: PInstruction): Boolean;
  begin
    Result := False;
    if PInst^.OpTable = tbOneByte then
    begin
      if (PInst^.OpCode > $27) and (PInst^.OpCode < $2E) then
        Exit(True);
    end;
    if (PInst^.OpKind = kGrp) and (PInst^.ModRm.Reg = $05) then
    begin
      if (PInst^.OpCode > $7F) and (PInst^.OpCode < $84) then
        Exit(True);
    end;
  end;
  function IsInstInc(PInst: PInstruction): Boolean;
  begin
    Result := False;
    if (PInst^.Archi = CPUX32) and (PInst^.OpTable = tbOneByte) then
    begin
      if (PInst^.OpCode >= $40) and (PInst^.OpCode <= $47) then
        Exit(True);
    end;
    if (PInst^.OpKind = kGrp) and (PInst^.ModRm.Reg = $00) then
    begin
      if (PInst^.OpCode = $FE) or (PInst^.OpCode = $FF) then
        Exit(True);
    end;
  end;
  function IsInstDec(PInst: PInstruction): Boolean;
  begin
    Result := False;
    if (PInst^.Archi = CPUX32) and (PInst^.OpTable = tbOneByte) then
    begin
      if (PInst^.OpCode >= $48) and (PInst^.OpCode <= $4F) then
        Exit(True);
    end;
    if (PInst^.OpKind = kGrp) and (PInst^.ModRm.Reg = $01) then
    begin
      if (PInst^.OpCode = $FE) or (PInst^.OpCode = $FF) then
        Exit(True);
    end;
  end;

begin
  { Return Instruction Arithmetic (+ or - or ..) }
  Result := arNone;
  if IsInstAdd(PInst) then
    Exit(arAdd);
  if IsInstInc(PInst) then
    Exit(arAdd);
  if IsInstSub(PInst) then
    Exit(arSub);
  if IsInstDec(PInst) then
    Exit(arSub);
end;
{$WARN COMPARISON_TRUE ON}

function EvalArithU(Arith: Byte; Value: NativeUInt; Offset: NativeInt): NativeUInt;
begin
  Result := Value;
  case Arith of
    arAdd: Inc(Result, Offset);
    arInc: Inc(Result);
    arSub: Dec(Result, Offset);
    arDec: Dec(Result);
  end;
end;

{$HINTS OFF}

function InterfaceToObj(const AIntf): TObject;
const
  {
    Delphi insert QueryInterface,_AddRef,_Release methods
    as the last functions in the code entry.
    => We must skip them to point to the first function declared in the interface.
  }
  Offset = SizeOf(Pointer) * 3;
{$IFDEF CPUX64}
  ObjReg = rECX;
{$ELSE !CPUX64}
  ObjReg = rEAX;
{$ENDIF CPUX64}
var
  Pvt, PCode: PByte;
  Inst: TInstruction;
  PObj: PByte;
  imm: Int64;
  Arith: Byte;
  Skip: Boolean;
  sReg: ShortInt;
begin

  if not Assigned(@AIntf) then
    Exit(nil);

  sReg := -1;
  PObj := PByte(AIntf);
{$IFNDEF FPC}
  Inst := default (TInstruction);
{$ENDIF !FPC}
  Inst.Archi := CPUX;
  Pvt := PPointer(AIntf)^; // vTable !
  PCode := PPointer(Pvt + Offset)^; // Code Entry !
  Inst.NextInst := PCode;
  {
    At the top of code entry delphi will generate :
    int 3
    add/sub eax/rcx,offset <===
    jmp FirstFunction
  }

  while True do
  begin
    Inst.imm.Value := 0;
    Inst.Addr := Inst.NextInst;
    fDecodeInst(@Inst);
    { Keep looping until JMP/RET ! }
    if (Inst.Branch.Falgs and bfUsed <> 0) or (Inst.OpType = otRET) then
      Break;

    Arith := GetInstArithmeticType(@Inst);
    Skip := (Arith = arNone);

    if not Skip then
    begin
{$IFDEF CPUX86}
      if Inst.ModRm.iMod <> $03 then
      begin
        {
          ====> stdcall ! <====
          If the method (declared in interface)
          calling convention is stdcall,
          Delphi will generate :
          add/sub [esp+offset],imm !
        }
        if Inst.Sib.Flags and sfUsed <> 0 then
          sReg := Inst.Sib.Index
        else
          sReg := Inst.ModRm.Rm;
        Skip := not(sReg = rESP);
      end
      else
{$ENDIF CPUX86}
      begin
        if (Inst.ModRm.Flags and mfUsed <> 0) then
          Skip := not((Inst.ModRm.iMod = $03) and (Inst.ModRm.Rm = ObjReg))
        else if Arith in [arInc, arDec] then
          { Is Inc/Dec EAX/RCX ? }
          Skip := (Inst.OpCode and $07 <> ObjReg);
      end;
    end;

    if not Skip then
    begin
      imm := Inst.imm.Value;
      PObj := PByte(EvalArithU(Arith, NativeUInt(PObj), imm));
    end;
  end;

  Result := TObject(PObj);
end;

{$HINTS ON}

function GetInterfaceMethodPtrByIndex(const PInterface; MethodIndex: Integer): PByte;
var
  Pvt: PPointer;
  P: PPointer;
  PDst: PByte;
  Inst: TInstruction;
  i: Integer;
begin
  {
    Return original method ptr
    => Return first instruction that was
    implemented on Interface object !
  }
{$IFNDEF FPC}
  Inst := default (TInstruction);
{$ENDIF !FPC}
  Inst.Archi := CPUX;
  Pvt := PPointer(PInterface)^; // Virtual Table !
  P := Pvt;
  // Inc(PByte(P), MethodIndex * SizeOf(NativeUInt));
  Inc(P, MethodIndex);
  P := PPointer(P)^;
  PDst := PByte(P);
  Inst.NextInst := PByte(P);
  for i := 0 to 3 do
  begin
    Inst.Addr := Inst.NextInst;
    fDecodeInst(@Inst);
    if Assigned(Inst.Branch.Target) then
    begin
      PDst := Inst.Branch.Target;
      Break;
    end;
  end;
  Result := PDst;
end;

{$IFDEF MustUseGenerics}

function GetMethodPtrFromObjByName(Obj: TObject; const MethodName: String): Pointer;
var
  LCtx: TRttiContext;
  LType: TRttiType;
  LMethods: TArray<TRttiMethod>;
  LMethod: TRttiMethod;
begin
  Result := nil;
  if (not Assigned(Obj)) or (MethodName = EmptyStr) then
    Exit;

  LCtx := TRttiContext.Create;
  LType := LCtx.GetType(Obj.ClassType);
  LMethods := LType.GetMethods;
  for LMethod in LMethods do
  begin
    if SameText(LMethod.Name, MethodName) then
      Exit(LMethod.CodeAddress);
  end;
end;

function GetInterfaceMethodPtrByName(const PInterface; const MethodName: String): PByte;
var
  Obj: TObject;
begin
  Result := nil;
  if (not Assigned(@PInterface)) or (MethodName = EmptyStr) then
    Exit;
  Obj := InterfaceToObj(PInterface);
  if Assigned(Obj) then
  begin
    Result := GetMethodPtrFromObjByName(Obj, MethodName);
  end;
end;

var
  GlobalThreadList: TDictionary<THandle, TThreadsIDList>;

{$ENDIF MustUseGenerics}
  { TIntercept }

constructor TIntercept.Create(Options: Byte);
begin
  FOptions := Options;
  FList := nil;

  if (FOptions and ST = ST)
{$IFDEF MustUseGenerics}
    and (not GlobalThreadList.ContainsKey(GetCurrentThread))
{$ENDIF MustUseGenerics}
  then
  begin
    { Suspend All threads ! }
    if OpenThreadExist then
    begin
      FList := TThreadsIDList.Create;
      SuspendAllThreads(FList);
    end;
  end;

  if not Assigned(FList) then
  begin
    TInterceptMonitor.Enter();
  end;
end;

destructor TIntercept.Destroy;
begin
  if Assigned(FList) then
  begin
    ResumeSuspendedThreads(FList);
    FreeAndNil(FList);
  end else begin
    TInterceptMonitor.Leave();
  end;
  inherited;
end;

function TIntercept.GetDescriptor(P: PByte): PDescriptor;
var
  Inst: TInstruction;
  function IsDscrpInst(PInst: PInstruction): Boolean;
  begin
    Result :=
{$IFDEF UseMultiBytesNop}
      (IsNop(PInst.Addr, 6, True)) or
{$ELSE !UseMultiBytesNop}
      (IsNop(PInst.Addr, 6)) or
{$ENDIF UseMultiBytesNop}
      (Assigned(PInst.Branch.Target));
  end;

begin
  Result := nil;
{$IFNDEF FPC}
  Inst := default (TInstruction);
{$ENDIF !FPC}
  Inst.Archi := CPUX;
  Inst.VirtualAddr := nil;
  { Find last JMP ! }
  P := GetRoot(P);
  Inst.Addr := P;
  fDecodeInst(@Inst);

  { The first instruction must be NOP ! }
  if Inst.OpCode = opNop then
  begin
    Inst.Addr := Inst.NextInst;
    fDecodeInst(@Inst);
    if IsDscrpInst(@Inst) then
    begin
      Inc(P); // Skip CodeEntry !
      Inc(P, SizeOf(TJmpMem) * (MAX_HOOKS + 1)); // Skip JmpMems !
      { Go to the Top ! }
      Dec(P, SizeOf(TDescriptor));
      if IsValidDescriptor(P) then
        Result := PDescriptor(P);
    end;
  end;
end;

class function TIntercept.GetRoot(P: PByte): PByte;
var
  Inst: TInstruction;
begin
  Result := P;
{$IFNDEF FPC}
  Inst := default (TInstruction);
{$ENDIF !FPC}
  Inst.Addr := P;
  Inst.Archi := CPUX;
  Inst.VirtualAddr := nil;
  {
    While the opcode is jmp and the jmp destination
    address is known get the next jmp .
  }
  fDecodeInst(@Inst);
  if (Inst.OpType = otJMP) and (Assigned(Inst.Branch.Target)) then
    Result := GetRoot(Inst.Branch.Target);
end;

function TIntercept.CreateNewDescriptor: PDescriptor;
begin
  { Create a new descriptor tables ! }
  Result := AllocMem(SizeOf(TDescriptor));
  {
    Hahaha .. this stupid code (between commenet)
    had take me 2h to figure why my libray does not works !
    I didn't know what i was thinking in when i wrote this code !
    :)
  }
  // SetMemPermission(Result, SizeOf(TDescriptor), PAGE_READWRITE);
  // SetMemPermission(@Result^.JmpMems, SizeOf(TJmpMem) * (MAX_HOOKS + 1), PAGE_EXECUTE_READWRITE);
  FillNop(Result^, SizeOf(TDescriptor));
  FillNop(Result^.JmpMems[0], SizeOf(TJmpMem) * (MAX_HOOKS + 1), True);

  { A valid descriptor have a valid signature . }
  CopyMemory(Result, PByte(@DscrSig[0]), DscrSigSize);
  Result^.nHook := 0;
  Result^.Flags := 0;
  Result^.ExMem := nil;
end;

procedure TIntercept.InsertDescriptor(PAt: PByte; PDscr: PDescriptor);
const
  { JMP from Target to Code Entry }
  kJmpCE = 1;
  { JMP from Target to Temporal address than JMP to Code Entry }
  kJmpTmpJmpCE = 2;
  { JMP from Target to Temporal address than JMP (Rip Zero) to Code Entry }
  kJmpTmpJmpRipZCE = 3;
  { JMP (Rip Zero) from Target to Code Entry }
  kJmpRipZCE = 4;

var
  fJmpType: Byte; { First JMP }
{$IFDEF CPUX64}
  sJmpType: Byte; { Second JMP (if used !) }
  Tmp: PByte;
{$ENDIF CPUX64}
  JmpKind: Byte;
  P, T: PByte;
  JmpSize: Byte;
  Inst: TInstruction;
  Sb: Byte;
  OrgAccess: DWORD;
  Tsz: Integer;
  PExMem: PByte;
  LPExMem: PByte;
begin
  JmpKind := kJmpCE;
  Sb := 0;
  P := PAt;
  PDscr^.OrgPtr := P;
  fJmpType := GetJmpType(P, @PDscr^.CodeEntry, @PDscr^.DscrAddr);
{$IFDEF CPUX64}
  Tmp := nil;
  PExMem := TryAllocMemAt(P, SizeOfAlloc, PAGE_EXECUTE_READWRITE);
{$ELSE !CPUX64}
  PExMem := TryAllocMemAt(nil, SizeOfAlloc, PAGE_EXECUTE_READWRITE);
{$ENDIF CPUX64}
  LPExMem := PExMem;
{$IFDEF CPUX64}
  sJmpType := tJmpNone;
  JmpKind := kJmpRipZCE;
  { Try to find the perfect jump instruction ! }
  {
    That's mean that we try to avoid using tJmpRelN on TargetProc .
    ==> Because it use more than 6 bytes in length .
  }
  if JmpTypeToSize[fJmpType] > 6 then
  begin
    Tmp := PExMem;
    Inc(PExMem, TmpSize);
    if Assigned(Tmp) then
    begin
      JmpKind := kJmpRipZCE;
      fJmpType := GetJmpType(P, Tmp, Tmp + 6);
      if JmpTypeToSize[fJmpType] < 7 then
      begin
        JmpKind := kJmpTmpJmpRipZCE;
        sJmpType := GetJmpType(Tmp, @PDscr^.CodeEntry, Tmp + 6 + 8);
        if JmpTypeToSize[sJmpType] < 7 then
          JmpKind := kJmpTmpJmpCE;
      end;
    end;
  end else begin
    JmpKind := kJmpCE;
  end;
{$ENDIF CPUX64}
{$IFNDEF FPC}
  Inst := default (TInstruction);
{$ENDIF !FPC}
  Inst.Archi := CPUX;
  Inst.NextInst := P;
  Inst.VirtualAddr := nil;

  JmpSize := JmpTypeToSize[fJmpType];

  while Sb < JmpSize do
  begin
    if Inst.OpType = otRET then
      raise InterceptException.Create(ErrFuncSize);
    Inst.Addr := Inst.NextInst;
    Inc(Sb, fDecodeInst(@Inst));
  end;

  if Sb > TrampoSize then
    raise InterceptException.Create(ErrTrampoSize);

  { Trampoline momory }
  T := PExMem;
  FillNop(T^, TrampoSize);

  PDscr^.Trampo := AllocMem(SizeOf(TTrampoInfo));
  PDscr^.Trampo^.PData := AllocMem(Sb + 6);
  FillNop(PDscr^.Trampo^.PData^, Sb + 6);
  { Save original target routine instruction . }
  Move(P^, PDscr^.Trampo^.PData^, Sb);
  PDscr^.Trampo^.Addr := T; // Pointer to the first trampoline instruction.
  PDscr^.Trampo^.Size := Sb; // Size of stolen instructions .

  Tsz := MapInsts(P, T, Sb);
  OrgAccess := SetMemPermission(P, Sb, PAGE_EXECUTE_READWRITE);
  try
    FillNop(P^, Sb);
    case JmpKind of
      kJmpCE:
        begin
          { A very good jump ! }
          {
            TargetProc :
            JMP @PDscr^.CodeEntry
          }
          InsertJmp(P, @PDscr^.CodeEntry, fJmpType, @PDscr^.DscrAddr);
        end;
{$IFDEF CPUX64}
      kJmpTmpJmpCE:
        begin
          {
            TargetProc :
            JMP @Tmp ==> Tmp is allocated nearly from TargetProc !

            Tmp:
            JMP @PDscr^.CodeEntry
          }
          InsertJmp(P, Tmp, fJmpType, Tmp + 6);
          InsertJmp(Tmp, @PDscr^.CodeEntry, sJmpType, Tmp + 6 + 8);
        end;
      kJmpTmpJmpRipZCE:
        begin
          {
            TargetProc :
            JMP @Tmp ==> Tmp is allocated nearly from TargetProc !

            Tmp:
            JMP @PDscr^.CodeEntry  ==> tJmpRipZ
          }
          InsertJmp(P, Tmp, fJmpType, Tmp + 6);
          InsertJmp(Tmp, @PDscr^.CodeEntry, tJmpRipZ, nil);
        end;
      kJmpRipZCE:
        begin
          {
            Not a good jump !

            TargetProc :
            JMP @PDscr^.CodeEntry  ==> tJmpRipZ
          }
          InsertJmp(P, @PDscr^.CodeEntry, tJmpRipZ, nil);
        end;
{$ENDIF CPUX64}
    end;

    {
      Insert a JMP instruction after the stolen instructions
      on the trampoline.
      ==> This JMP will return to TargetProc to allow
      executing originals instructions.
    }
{$IFDEF CPUX64}
    // InsertJmp(T + Tsz, P + JmpTypeToSize[fJmpType], tJmpRipZ);
    InsertJmp(T + Tsz, P + Sb, tJmpRipZ);
{$ELSE !CPUX64}
    InsertJmp(T + Tsz, P + Sb, tJmpMem32, T + Tsz + 6);
{$ENDIF CPUX64}
    { Save LPExMem ==> we need it when deleting descriptor }
    PDscr^.ExMem := LPExMem;

    SetMemPermission(LPExMem, SizeOfAlloc, PAGE_EXECUTE_READWRITE);
    SetMemPermission(PDscr, SizeOf(TDescriptor), PAGE_EXECUTE_READWRITE);
  finally
    SetMemPermission(P, Sb, OrgAccess);
  end;
end;

function TIntercept.IsValidDescriptor(P: PByte): Boolean;
begin
  Result := CompareMem(P, PByte(@DscrSig[0]), SizeOf(DscrSig));
end;

function TIntercept.AddHook(PDscr: PDescriptor; InterceptProc: PByte; const Options: Byte): PByte;
var
  n: ShortInt;
  NxHook: PByte;
begin
  {
    Return a pointer to a function that can
    call next installed Hooks.
  }
  n := PDscr^.nHook;
  if n + 1 > MAX_HOOKS then
    raise InterceptException.Create(ErrMaxHook);

  { Alloc memory for the NextHook ! }
  NxHook := AllocMem(TrampoSize);
  Result := NxHook;

  FillNop(Result^, TrampoSize);

  PNextHook(Result)^.PDscr := PDscr;
  PNextHook(Result)^.ID := n + 1;
  Inc(Result, SizeOf(TNextHook));

  { Redirect code to InterceptProc ! }
  InsertJmp(@PDscr^.JmpMems[n], InterceptProc, tJmpMemN, @PDscr^.JmpAddrs[n]);
  { Redirect code to TrampoLine ! }
  InsertJmp(@PDscr^.JmpMems[n + 1], PDscr^.Trampo^.Addr, tJmpMemN, @PDscr^.JmpAddrs[n + 1]);
  { Redirect code to next hook ! }
  InsertJmp(Result, @PDscr^.JmpMems[n + 1], tJmpMemN, Result + 6);
  Inc(PDscr^.nHook);

  SetMemPermission(Result, JmpTypeToSize[tJmpRipZ], PAGE_EXECUTE_READWRITE);
end;

function TIntercept.InstallHook(TargetProc, InterceptProc: PByte; const Options: Byte = $00): PByte;
var
  P: PByte;
  PDscr: PDescriptor;
begin
  if not Assigned(TargetProc) then
    raise InterceptException.Create(ErrTargetProc);

  if not Assigned(InterceptProc) then
    raise InterceptException.Create(ErrInterceptProc);

  PDscr := GetDescriptor(TargetProc);
  if not Assigned(PDscr) then
  begin
    P := GetRoot(TargetProc);
    PDscr := CreateNewDescriptor;
    try
      InsertDescriptor(P, PDscr);
    except
      FreeMem(PDscr);
      raise;
    end;
  end;
  Result := AddHook(PDscr, InterceptProc);
end;

procedure TIntercept.RemoveDescriptor(PDscr: PDescriptor);
var
  OrgAccess: DWORD;
  P: PByte;
  sz: Integer;
  vr: Boolean;
begin
  P := PDscr^.OrgPtr;
  sz := PDscr^.Trampo^.Size;

  OrgAccess := SetMemPermission(P, sz, PAGE_EXECUTE_READWRITE);
  try
    SetMemPermission(PDscr^.ExMem, TrampoSize, PAGE_EXECUTE_READWRITE);

    { Restore the old stolen instructions ! }
    Move(PDscr^.Trampo^.PData^, PDscr^.OrgPtr^, PDscr^.Trampo^.Size);

    FillNop(PDscr^.ExMem^, SizeOfAlloc);
    FreeMem(PDscr^.Trampo^.PData);
    FreeMem(PDscr^.Trampo);

    if Assigned(PDscr^.ExMem) then
    begin
      vr := InternalFuncs.VirtualFree(PDscr^.ExMem, 0, MEM_RELEASE);
      if not vr then
        RaiseLastOSError;
    end;

    FillNop(PDscr^, SizeOf(TDescriptor));
    FreeMem(PDscr);
  finally
    SetMemPermission(P, sz, OrgAccess);
  end;
end;

function TIntercept.RemoveHook(Trampo: PByte): Integer;
var
  PNxtHook: PNextHook;
  PDscr: PDescriptor;
  n: Byte;
begin
  if not Assigned(Trampo) then
    raise InterceptException.Create(ErrInvalidTrampo);

  PNxtHook := PNextHook(Trampo - SizeOf(TNextHook));
  if not Assigned(PNxtHook) then
    raise InterceptException.Create(ErrInvalidTrampo);

  PDscr := PNxtHook^.PDscr;
  if not IsValidDescriptor(PByte(PDscr)) then
    raise InterceptException.Create(ErrInvalidDscr);

  n := PNxtHook^.ID;
  Dec(PDscr^.nHook);

  PDscr^.JmpAddrs[n - 1] := nil;
  { Remove JMP from descriptor table }
  FillNop(PByte(@PDscr^.JmpMems[n - 1])^, SizeOf(TJmpMem), True);

  {
    Return the number of hooks
    that are still alive !
  }
  Result := PDscr^.nHook;

  if Result = 0 then
    RemoveDescriptor(PDscr);

  FreeMem(PNxtHook);
end;

function InterceptCreate(const TargetProc, InterceptProc: Pointer; Options: Byte = v1compatibility): Pointer;
var
  Intercept: TIntercept;
begin
  Intercept := TIntercept.Create(Options);
  try
    Result := Intercept.InstallHook(TargetProc, InterceptProc, Options);
  finally
    Intercept.Free;
  end;
end;

{ =====> Support for Interface <===== }

function InterceptCreate(const TargetInterface; MethodIndex: Integer; const InterceptProc: Pointer; Options: Byte = v1compatibility): Pointer;
var
  P: PByte;
begin
  Result := nil;
  if not Assigned(@TargetInterface) then
    Exit;
  P := GetInterfaceMethodPtrByIndex(TargetInterface, MethodIndex);
  if Assigned(P) then
  begin
    Result := InterceptCreate(P, InterceptProc, Options);
  end;
end;

{$IFDEF MustUseGenerics}

function InterceptCreate(const TargetInterface; const MethodName: String; const InterceptProc: Pointer; Options: Byte = v1compatibility): Pointer; overload;
var
  P: PByte;
begin
  Result := nil;
  if (not Assigned(@TargetInterface)) or (MethodName = EmptyStr) then
    Exit;

  P := GetInterfaceMethodPtrByName(TargetInterface, MethodName);
  if Assigned(P) then
    Result := InterceptCreate(P, InterceptProc, Options);
end;
{$ENDIF MustUseGenerics}

function InterceptCreate(const Module, MethodName: string; const InterceptProc: Pointer; ForceLoadModule: Boolean = True;
  Options: Byte = v1compatibility): Pointer;
var
  pOrgPointer: Pointer;
  LModule: THandle;
begin
  { RRUZ's idea ==> Looks great ! }
  Result := nil;
  LModule := GetModuleHandle(PChar(Module));
  if (LModule = 0) and ForceLoadModule then
    LModule := LoadLibrary(PChar(Module));

  if LModule <> 0 then
  begin
    pOrgPointer := GetProcAddress(LModule, PChar(MethodName));
    if Assigned(pOrgPointer) then
      Result := InterceptCreate(pOrgPointer, InterceptProc, Options);
  end;
end;

procedure InterceptCreate(const TargetProc, InterceptProc: Pointer; var TrampoLine: Pointer; Options: Byte = v1compatibility);
var
  Intercept: TIntercept;
begin
  Intercept := TIntercept.Create(Options);
  try
    TrampoLine := Intercept.InstallHook(TargetProc, InterceptProc, Options);
  finally
    Intercept.Free;
  end;
end;

function InterceptRemove(const Trampo: Pointer; Options: Byte = v1compatibility): Integer;
var
  Intercept: TIntercept;
begin
  if not Assigned(Trampo) then
    Exit(-1);
  Intercept := TIntercept.Create(Options);
  try
    Result := Intercept.RemoveHook(Trampo);
  finally
    Intercept.Free;
  end;
end;

function GetNHook(const TargetProc: Pointer): ShortInt;
var
  Intercept: TIntercept;
  PDscr: PDescriptor;
begin
  {
    Return the number of installed hooks !
    Return -1 if no hooks installed !
  }
  Result := -1;
  if not Assigned(TargetProc) then
    raise InterceptException.Create(ErrTargetProc);

  Intercept := TIntercept.Create(0);
  try
    PDscr := Intercept.GetDescriptor(TargetProc);
    if Assigned(PDscr) then
      Result := PDscr^.nHook;
  finally
    Intercept.Free;
  end;
end;

function GetNHook(const TargetInterface; MethodIndex: Integer): ShortInt; overload;
var
  P: PByte;
begin
  P := GetInterfaceMethodPtrByIndex(TargetInterface, MethodIndex);
  Result := GetNHook(P);
end;

{$IFDEF MustUseGenerics }

function GetNHook(const TargetInterface; const MethodName: String): ShortInt; overload;
var
  P: PByte;
begin
  P := GetInterfaceMethodPtrByName(TargetInterface, MethodName);
  Result := GetNHook(P);
end;
{$ENDIF MustUseGenerics }

function IsHooked(const TargetProc: Pointer): Boolean;
begin
  Result := GetNHook(TargetProc) > 0;
end;

function IsHooked(const TargetInterface; MethodIndex: Integer): Boolean; overload;
var
  P: PByte;
begin
  P := GetInterfaceMethodPtrByIndex(TargetInterface, MethodIndex);
  Result := IsHooked(P);
end;

{$IFDEF MustUseGenerics }

function IsHooked(const TargetInterface; const MethodName: String): Boolean; overload;
var
  P: PByte;
begin
  P := GetInterfaceMethodPtrByName(TargetInterface, MethodName);
  Result := IsHooked(P);
end;
{$ENDIF MustUseGenerics }

type
  TTrampoDataVt = record
    vAddr: Pointer;
    Addr: Pointer;
  end;

  PTrampoDataVt = ^TTrampoDataVt;

function PatchVt(const TargetInterface; MethodIndex: Integer; InterceptProc: Pointer): Pointer;
var
  vt: PPointer;
  P, DstAddr: PPointer;
  Q: PByte;
  OrgAccess: DWORD;
  PInfo: PTrampoDataVt;
begin
  {
    NB: PatchVt does not support multi hook !!
    PatchVt will patch only vtable !!
  }
  Result := nil;
  if not Assigned(@TargetInterface) then
    Exit;
  if not Assigned(InterceptProc) then
    Exit;

  TInterceptMonitor.Enter;
  try
    vt := PPointer(TargetInterface)^;
    P := vt;
    Inc(P, MethodIndex);
    DstAddr := P^; // address !

    OrgAccess := SetMemPermission(P, 32, PAGE_EXECUTE_READWRITE);
    try
      P^ := InterceptProc;
    finally
      SetMemPermission(P, 32, OrgAccess);
    end;

    Result := InternalFuncs.VirtualAlloc(nil, 32, MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE);
    SetMemPermission(Result, 32, PAGE_EXECUTE_READWRITE);
    PInfo := Result;
    PInfo^.vAddr := P;
    PInfo^.Addr := DstAddr;
    Inc(PByte(Result), SizeOf(TTrampoDataVt));

    Q := Result;
{$IFDEF CPUX64}
    { Use JMP RipZero ! }
    PWord(Q)^ := opJmpMem;
    Inc(Q, 2);
    PInt32(Q)^ := $00;
    Inc(Q, 4);
    PNativeUInt(Q)^ := NativeUInt(DstAddr);
{$ELSE !CPUX64}
    PWord(Q)^ := opJmpMem;
    Inc(Q, 2);
    PUInt32(Q)^ := UInt32(Q + 4);
    PUInt32(Q + 4)^ := UInt32(DstAddr);
{$ENDIF CPUX64}
  finally
    TInterceptMonitor.Leave;
  end;
end;

function UnPatchVt(const Trampo: Pointer): Boolean;
var
  OrgAccess: DWORD;
  PInfo: PTrampoDataVt;
begin
  if not Assigned(Trampo) then
    Exit(False);

  TInterceptMonitor.Enter;
  try
    PInfo := PTrampoDataVt(PByte(Trampo) - SizeOf(TTrampoDataVt));
    OrgAccess := SetMemPermission(PInfo^.vAddr, 32, PAGE_EXECUTE_READWRITE);
    try
      PPointer(PInfo^.vAddr)^ := PInfo^.Addr;
    finally
      SetMemPermission(PInfo^.vAddr, 32, OrgAccess);
    end;
    Result := InternalFuncs.VirtualFree(Trampo, 0, MEM_RELEASE);
  finally
    TInterceptMonitor.Leave;
  end;
end;

{ TInterceptMonitor }

class procedure TInterceptMonitor.InternalCreate;
begin
  FLock := TObject.Create;
end;

class procedure TInterceptMonitor.InternalDestroy;
begin
  FreeAndNil(FLock);
end;

class procedure TInterceptMonitor.Enter;
begin
  {
    If the current thread is working with DDL (Insert/Remove hook)..
    others threads must wait before accessing
    detours (Insert/Remove/...).
  }
{$IFNDEF FPC}
  TMonitor.Enter(FLock);
{$ELSE FPC}
  EnterCriticalSection(Critical);
{$ENDIF !FPC}
end;

class procedure TInterceptMonitor.Leave;
begin
  {
    After the current thread finish it's
    job with TIntercept .. others thread
    can now access TIntercept .
  }
{$IFNDEF FPC}
  TMonitor.Exit(FLock);
{$ELSE}
  LeaveCriticalSection(Critical);
{$ENDIF}
end;

{$IFDEF MustUseGenerics }
{ ***** BEGIN LICENSE BLOCK *****
  *
  * The initial developer of the original TDetour
  * class is David Millington .
  *
  * ***** END LICENSE BLOCK ***** }

{ TDetours<T> }

function TDetours<T>.CheckTType: Boolean;
var
  LPInfo: PTypeInfo;
begin
  LPInfo := TypeInfo(T);
  Result := SizeOf(T) = SizeOf(Pointer);
  if Result then
    Result := LPInfo.Kind = tkProcedure;
  if not Result then
    raise DetourException.CreateFmt(SInvalidTType, [LPInfo.Name]);
end;

constructor TDetours<T>.Create(const TargetProc, InterceptProc: T);
begin
  inherited Create();
  CheckTType;
  SetHook(TargetProc, InterceptProc);
end;

destructor TDetours<T>.Destroy;
begin
  Disable;
  inherited;
end;

procedure TDetours<T>.SetHook(const TargetProc, InterceptProc: T);
begin
  FTargetProc := TToPointer(TargetProc);
  FInterceptProc := TToPointer(InterceptProc);
  FNextHook := T(nil);
  Assert(Assigned(FTargetProc) and Assigned(FInterceptProc), 'Target or replacement methods are not assigned');
end;

procedure TDetours<T>.Disable;
var
  PTrampoline: Pointer;
begin
  if Installed then
  begin
    PTrampoline := TToPointer(FNextHook);
    DDetours.InterceptRemove(PTrampoline);
    FNextHook := T(nil);
  end;
end;

procedure TDetours<T>.Enable;
begin
  if not Installed then
    FNextHook := PointerToT(InterceptCreate(FTargetProc, FInterceptProc));
end;

function TDetours<T>.GetHookCount: ShortInt;
begin
  Result := GetNHook(FTargetProc);
end;

function TDetours<T>.GetInstalled: Boolean;
begin
  Result := Assigned(TToPointer(FNextHook));
end;

function TDetours<T>.NextHook: T;
begin
  Assert(Installed, SDetoursNotInstalled);
  Result := FNextHook;
end;

function TDetours<T>.PointerToT(const _P: Pointer): T;
begin
  Result := __PointerToT(_P);
end;

function TDetours<T>.TToPointer(const _T: T): Pointer;
begin
  Result := __TToPointer(_T);
end;

function TDetours<T>.__PointerToT(const _P): T;
begin
  Result := T(_P);
end;

function TDetours<T>.__TToPointer(const _T): Pointer;
begin
  Result := Pointer(_T);
end;

{ --------------------------------------------------------------------------- }
function BeginHooks(): Boolean;
var
  List: TThreadsIDList;
begin
  List := TThreadsIDList.Create;
  GlobalThreadList.Add(GetCurrentThread, List);
  Result := SuspendAllThreads(List);
end;

function EndHooks(): Boolean;
var
  List: TThreadsIDList;
  currThread: THandle;
begin
  currThread := GetCurrentThread;
  List := GlobalThreadList[currThread];
  Assert(Assigned(List));
  Result := ResumeSuspendedThreads(List);
  GlobalThreadList.Remove(currThread);
  FreeAndNil(List);
end;

function BeginUnHooks(): Boolean;
begin
  if GlobalThreadList.ContainsKey(GetCurrentThread) then
    raise InterceptException.Create(ErrBgnUnHooks);
  Result := BeginHooks;
end;

function EndUnHooks(): Boolean;
begin
  Result := EndHooks;
end;
{$ENDIF MustUseGenerics}

var
  SysInfo: TSystemInfo;

procedure InitInternalFuncs();

  function CloneFunc(Func: PByte): PByte;
  var
    mb, ns, Sb, fn: Byte;
    P: PByte;
    Inst: TInstruction;
  begin
    Sb := 0;
    Func := TIntercept.GetRoot(Func);
    Result := VirtualAlloc(nil, 64, MEM_RESERVE or MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    P := Result;
    mb := JmpTypeToSize[tJmpRipZ];
{$IFNDEF FPC}
    Inst := default (TInstruction);
{$ENDIF !FPC}
    Inst.Archi := CPUX;
    Inst.NextInst := Func;
    while Sb <= mb do
    begin
      Inst.Addr := Inst.NextInst;
      ns := fDecodeInst(@Inst);
      Inc(Sb, ns);
    end;
    fn := MapInsts(Func, P, Sb);
    Inc(P, fn);
{$IFDEF CPUX64}
    InsertJmp(P, Func + Sb, tJmpRipZ);
{$ELSE !CPUX64}
    InsertJmp(P, Func + Sb, tJmpRel32);
{$ENDIF CPUX64}
  end;

begin
{$IFDEF HookInternalFuncs}
  @InternalFuncs.VirtualAlloc := CloneFunc(@VirtualAlloc);
  @InternalFuncs.VirtualFree := CloneFunc(@VirtualFree);
  @InternalFuncs.VirtualProtect := CloneFunc(@VirtualProtect);
  @InternalFuncs.VirtualQuery := CloneFunc(@VirtualQuery);
  @InternalFuncs.FlushInstructionCache := CloneFunc(@FlushInstructionCache);
  @InternalFuncs.GetCurrentProcess := CloneFunc(@GetCurrentProcess);
{$ELSE !HookInternalFuncs}
  @InternalFuncs.VirtualAlloc := @VirtualAlloc;
  @InternalFuncs.VirtualFree := @VirtualFree;
  @InternalFuncs.VirtualProtect := @VirtualProtect;
  @InternalFuncs.VirtualQuery := @VirtualQuery;
  @InternalFuncs.FlushInstructionCache := @FlushInstructionCache;
  @InternalFuncs.GetCurrentProcess := @GetCurrentProcess;
{$ENDIF HookInternalFuncs}
end;

procedure FreeInternalFuncs;
begin
{$IFDEF HookInternalFuncs}
  InternalFuncs.VirtualFree(@InternalFuncs.VirtualAlloc, 0, MEM_RELEASE);
  InternalFuncs.VirtualFree(@InternalFuncs.VirtualProtect, 0, MEM_RELEASE);
  InternalFuncs.VirtualFree(@InternalFuncs.VirtualQuery, 0, MEM_RELEASE);
  InternalFuncs.VirtualFree(@InternalFuncs.FlushInstructionCache, 0, MEM_RELEASE);
  InternalFuncs.VirtualFree(@InternalFuncs.GetCurrentProcess, 0, MEM_RELEASE);
  // VirtualFree must be the last one !
  InternalFuncs.VirtualFree(@InternalFuncs.VirtualFree, 0, MEM_RELEASE);
{$ENDIF HookInternalFuncs}
end;

initialization

TInterceptMonitor.InternalCreate;
{$IFDEF MustUseGenerics}
GlobalThreadList := TDictionary<THandle, TThreadsIDList>.Create;
{$ENDIF MustUseGenerics}
GetSystemInfo(SysInfo);
SizeOfAlloc := SysInfo.dwPageSize;
if SizeOfAlloc < (TmpSize + TrampoSize + 64) then
  SizeOfAlloc := (TmpSize + TrampoSize + 64);
{$IFDEF FPC}
OpenThread := nil;
InitializeCriticalSection(Critical);
{$ELSE !FPC}
@OpenThread := nil;
{$ENDIF !FPC}
FreeKernel := False;

hKernel := GetModuleHandle(kernel32);
if hKernel <= 0 then
begin
  hKernel := LoadLibrary(kernel32);
  FreeKernel := (hKernel > 0);
end;

if hKernel > 0 then
begin
{$IFDEF FPC}
  @OpenThread := GetProcAddress(hKernel, 'OpenThread');
  @CreateToolhelp32Snapshot := GetProcAddress(hKernel, 'CreateToolhelp32Snapshot');
  @Thread32First := GetProcAddress(hKernel, 'Thread32First');
  @Thread32Next := GetProcAddress(hKernel, 'Thread32Next');
{$ELSE !FPC}
  @OpenThread := GetProcAddress(hKernel, 'OpenThread');
{$ENDIF !FPC}
end;
{ The OpenThread function does not exist on OS version < Win XP }
OpenThreadExist := (@OpenThread <> nil);
InitInternalFuncs;

finalization

{$IFDEF MustUseGenerics}
  GlobalThreadList.Free;
{$ENDIF MustUseGenerics}
if (FreeKernel) and (hKernel > 0) then
  FreeLibrary(hKernel);
FreeInternalFuncs;
{$IFDEF FPC}
DeleteCriticalSection(Critical);
{$ENDIF FPC}
TInterceptMonitor.InternalDestroy;

end.
