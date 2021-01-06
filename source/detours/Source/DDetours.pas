// **************************************************************************************************
// Delphi Detours Library.
// Unit DDetours
// https://github.com/MahdiSafsafi/DDetours
//
// This Source Code Form is subject to the terms of the Mozilla
// Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at
// https://mozilla.org/MPL/2.0/.
// **************************************************************************************************
//
// Contributors:
// - David Millington : Added TDetours<T> class.
// **************************************************************************************************

unit DDetours;

{define FIX_MADEXCEPT if you are using crash on buffer overrun/underrun feature from MadExcept }
{.$DEFINE FIX_MADEXCEPT}

{$IFDEF FPC}
{$MODE DELPHI}
{$HINTS OFF}
{$WARN 4045 OFF}
{$WARN 4055 OFF}
{$WARN 4056 OFF}
{$WARN 4082 OFF}
{$WARN 5024 OFF}
{$WARN 5028 OFF}
{$WARN 5057 OFF}
{$WARN 5058 OFF}
{$ENDIF FPC}

interface

{$I DDetoursDefs.inc}

uses

{$IFDEF RENAMED_NAMESPACE}
  System.SysUtils,
  System.Classes,
  WinApi.Windows,
  WinApi.TLHelp32,
{$IFNDEF SUPPORTS_MONITOR}
  System.SyncObjs,
{$ENDIF SUPPORTS_MONITOR}
{$ELSE !RENAMED_NAMESPACE}
  SysUtils,
  Windows,
  Classes,
{$IFNDEF SUPPORTS_MONITOR}
  SyncObjs,
{$ENDIF SUPPORTS_MONITOR}
{$IFNDEF FPC}
  TLHelp32,
{$ENDIF FPC}
{$ENDIF RENAMED_NAMESPACE}
{$IFDEF SUPPORTS_RTTI}
  System.Generics.Collections,
  System.Typinfo, System.RTTI,
{$ENDIF SUPPORTS_RTTI}
  LegacyTypes,
  CPUID,
  InstDecode;

type
  InterceptException = Exception;
  TTransactionOption = (toSuspendThread);
  TTransactionOptions = set of TTransactionOption;

  TInterceptOption = (ioForceLoad, ioRecursive);
  TInterceptOptions = set of TInterceptOption;

const
  { Maximum allowed number of hooks. }
  MAX_HOOKS = 7;

  DefaultInterceptOptions = [];
  SErrorInvalidTType = '<T> must be a method';

  { ========================================= DDetours Interface ========================================= }
function InterceptCreate(const TargetProc, InterceptProc: Pointer; const Param: Pointer = nil; const Options: TInterceptOptions = DefaultInterceptOptions)
  : Pointer; overload;
function InterceptCreate(const TargetInterface; MethodIndex: Integer; const InterceptProc: Pointer; const Param: Pointer = nil;
  const Options: TInterceptOptions = DefaultInterceptOptions): Pointer; overload;
function InterceptCreate(const Module, MethodName: String; const InterceptProc: Pointer; const Param: Pointer = nil;
  const Options: TInterceptOptions = DefaultInterceptOptions): Pointer; overload;
procedure InterceptCreate(const TargetProc, InterceptProc: Pointer; var TrampoLine: Pointer; const Param: Pointer = nil;
  const Options: TInterceptOptions = DefaultInterceptOptions); overload;

{$IFDEF SUPPORTS_RTTI}
function InterceptCreate(const TargetInterface; const MethodName: String; const InterceptProc: Pointer; const Param: Pointer = nil;
  const Options: TInterceptOptions = DefaultInterceptOptions): Pointer; overload;
{$ENDIF SUPPORTS_RTTI}
function InterceptRemove(const TrampoLine: Pointer): Integer; overload;

function GetHookCount(const TargetProc: Pointer): Integer; overload;
function GetHookCount(const TargetInterface; MethodIndex: Integer): Integer; overload;
{$IFDEF SUPPORTS_RTTI}
function GetHookCount(const TargetInterface; const MethodName: String): Integer; overload;
{$ENDIF SUPPORTS_RTTI}
function IsHooked(const TargetProc: Pointer): Boolean; overload;
function IsHooked(const TargetInterface; MethodIndex: Integer): Boolean; overload;
{$IFDEF SUPPORTS_RTTI}
function IsHooked(const TargetInterface; const MethodName: String): Boolean; overload;
{$ENDIF SUPPORTS_RTTI}
function PatchVt(const TargetInterface; MethodIndex: Integer; InterceptProc: Pointer): Pointer;
function UnPatchVt(const TrampoLine: Pointer): Boolean;

function BeginTransaction(Options: TTransactionOptions = [toSuspendThread]): THandle;
function EndTransaction(Handle: THandle): Boolean;

function EnterRecursiveSection(var TrampoLine; MaxRecursionLevel: NativeInt = 0): Boolean;
function ExitRecursiveSection(var TrampoLine): Boolean;

function GetCreatorThreadIdFromTrampoline(var TrampoLine): TThreadId;
function GetTrampolineParam(var TrampoLine): Pointer;

{$IFDEF SUPPORTS_GENERICS}

type
  IIntercept<T, U> = interface(IInterface)
    ['{EECBF3C2-3938-4923-835A-B0A6AD27744D}']
    function GetTrampoline(): T;
    function GetParam(): U;
    function GetCreatorThreadId(): TThreadId;
    function GetInterceptOptions(): TInterceptOptions;
    function EnterRecursive(MaxRecursionLevel: NativeInt = 0): Boolean;
    function ExitRecursive(): Boolean;

    property NextHook: T read GetTrampoline;
    property TrampoLine: T read GetTrampoline; // alias to NextHook
    property Param: U read GetParam;
    property CreatorThreadId: TThreadId read GetCreatorThreadId;
    property InterceptOptions: TInterceptOptions read GetInterceptOptions;
  end;

  {
    Based on David Millington's original implementation TDetours<T>.
  }
  TIntercept<T, U> = class(TInterfacedObject, IIntercept<T, U>)
  private
    FNextHook: T;
    FTrampolinePtr: Pointer;
    FParam: U;
    FCreatorThreadId: TThreadId;
    FInterceptOptions: TInterceptOptions;
    function TToPointer(const A): Pointer;
    function PointerToT(const P): T;
    function EnsureTIsMethod(): Boolean;
  public
    function GetTrampoline(): T;
    function GetParam(): U;
    function GetCreatorThreadId(): TThreadId;
    function GetInterceptOptions(): TInterceptOptions;
    function EnterRecursive(MaxRecursionLevel: NativeInt = 0): Boolean;
    function ExitRecursive(): Boolean;
    constructor Create(const TargetProc, InterceptProc: T; const AParam: U; const AInterceptOptions: TInterceptOptions = DefaultInterceptOptions); virtual;
    destructor Destroy(); override;
    property Param: U read FParam;
    property NextHook: T read FNextHook;
    property TrampoLine: T read FNextHook; // alias to NextHook
    property CreatorThreadId: TThreadId read FCreatorThreadId;
    property InterceptOptions: TInterceptOptions read FInterceptOptions;
  end;

  TIntercept<T> = class(TIntercept<T, Pointer>)
  public
    constructor Create(const TargetProc, InterceptProc: T; const AParam: Pointer = nil;
      const AInterceptOptions: TInterceptOptions = DefaultInterceptOptions); override;
  end;
{$ENDIF SUPPORTS_GENERICS}

type
  DetourException = Exception;

implementation

const
  { Nops }
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

  { Arithmetic operands }
  arNone = $00;
  arPlus = $08;
  arMin = $10;
  arAdd = arPlus or $01;
  arSub = arMin or $01;
  arInc = arPlus or $02;
  arDec = arMin or $02;

  { Instructions OpCodes }
  opJmpRelz = $E9;
  opJmpRelb = $EB;
  opJmpMem = $25FF;
  opTestb = $85;
  opPrfOpSize = $66;
  opPrfAddrSize = $67;
  opNop = $90;

  { thread constants }
  THREAD_SUSPEND_RESUME = $0002;

  { Error messages }
  SErrorSmallFunctionSize = 'Size of function is too small, risk to override others adjacent functions.';
  SErrorInvalidJmp = 'Invalid JMP Type.';
  SErrorInvalidJmp64 = 'Invalid JMP Type for x64.';
  SErrorInvalidJmp32 = 'Invalid JMP Type for x32.';
  SErrorInvalidDstSave = 'Invalid DstSave Address pointer.';
  SErrorUnsupportedMultiNop = 'Multi Bytes Nop Instructions not supported by your CPU.';
  SErrorRipDisp = 'Failed to correcr RIP Displacement.';
  SErrorBigTrampoSize = 'Exceed maximum TrampoSize.';
  SErrorMaxHook = 'Exceed maximum allowed of hooks.';
  SErrorInvalidTargetProc = 'Invalid TargetProc Pointer.';
  SErrorInvalidInterceptProc = 'Invalid InterceptProc Pointer.';
  SErrorInvalidDescriptor = 'Invalid Descriptor.';
  SErrorInvalidTrampoline = 'Invalid TrampoLine Pointer.';
  SErrorBeginUnHook = 'BeginUnHooks must be called outside BeginHooks/EndHooks.';
  SErrorRecursiveSectionUnsupported = 'Trampoline was not marked to use recursive section.';
  SErrorTlsOutOfIndexes = 'Tls out of indexes.';
  { JMP Type }
  JT_NONE = 0;
  JT_REL8 = 1;
  JT_REL16 = 2;
  JT_REL32 = 3;
  JT_MEM16 = 4;
  JT_MEM32 = 5;
  JT_MEM64 = 6;
  JT_RIPZ = 7;

{$IFDEF CPUX64}
  JT_MEMN = JT_MEM64;
{$ELSE !CPUX64}
  JT_MEMN = JT_MEM32;
{$ENDIF CPUX64}
  { Jmp Type To Size }
  JmpTypeToSize: array [0 .. 7] of Byte = ( //
    0, { None }
    2, { JT_REL8  = $EB + Rel8 }
    4, { JT_REL16 = OpSizePrf + $E9 + Rel16 }
    5, { JT_REL32 = $E9 + Rel32 }
    7, { JT_MEM16 = OpSizePrf + $FF /4 + Disp32 }
    6, { JT_MEM32 = $FF /4 + Disp32 }
    6, { JT_MEM64 = $FF /4 + Disp32 }
    14 { JT_RIPZ  = $FF /4 + Disp32 + DQ }
    );

  SizeToJmpType: array [0 .. 4] of Byte = ( //
{$IFDEF CPUX86}
    JT_REL8, { db }
    JT_REL16, { dw }
    JT_REL32, { dd }
    JT_MEM32, { dd }
    JT_MEM32 { dd }
{$ELSE !CPUX86}
    JT_REL8, { db }
    JT_REL32, { dw }
    JT_REL32, { dd }
    JT_MEM64, { dq }
    JT_MEM64 { dq }
{$ENDIF CPUX86}
    );

  DscrSigSize = $08;
  TmpSize = 32;

  TrampolineSignature = $544C544C;

type
  TArrayOfThreadId = array [0 .. HIGH(SmallInt) - 1] of DWORD;
  PArrayOfThreadId = ^TArrayOfThreadId;

  TTransactionStruct = record
    Options: TTransactionOptions;
    TID: DWORD;
    PID: DWORD;
    ThreadPriority: Integer;
    SuspendedThreadCount: Integer;
    SuspendedThreads: PArrayOfThreadId;
  end;

  PTransactionStruct = ^TTransactionStruct;

  TOpenThread = function(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwThreadId: DWORD): THandle; stdcall;

  TDscrSig = array [0 .. DscrSigSize - 1] of Byte;

  TVirtualProtect = function(lpAddress: Pointer; dwSize: SIZE_T; flNewProtect: DWORD; var OldProtect: DWORD): BOOL; stdcall;
  TVirtualAlloc = function(lpvAddress: Pointer; dwSize: SIZE_T; flAllocationType, flProtect: DWORD): Pointer; stdcall;
  TVirtualQuery = function(lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: SIZE_T): SIZE_T; stdcall;
  TFlushInstructionCache = function(hProcess: THandle; const lpBaseAddress: Pointer; dwSize: SIZE_T): BOOL; stdcall;
  TGetCurrentProcess = function: THandle; stdcall;
  TVirtualFree = function(lpAddress: Pointer; dwSize: SIZE_T; dwFreeType: DWORD): BOOL; stdcall;

  { TEnumThreadCallBack for EnumProcessThreads }
  TEnumThreadCallBack = function(ID: DWORD; Param: Pointer): Boolean;

  TInternalFuncs = record
    VirtualAlloc: TVirtualAlloc;
    VirtualFree: TVirtualFree;
    VirtualProtect: TVirtualProtect;
    VirtualQuery: TVirtualQuery;
    FlushInstructionCache: TFlushInstructionCache;
    GetCurrentProcess: TGetCurrentProcess;
  end;

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
    Signature: Cardinal;
    threadid: TThreadId;
    Param: Pointer;
    TlsRecursionLevelIndex: DWORD;
    InterceptOptions: TInterceptOptions;
  end;

  PNextHook = ^TNextHook;

  TTrampoDataVt = record
    vAddr: Pointer;
    Addr: Pointer;
  end;

  PTrampoDataVt = ^TTrampoDataVt;

const
  TrampoSize = SizeOf(TNextHook) + 64;

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
{$IFDEF FPC}
{$I 'TlHelp32.inc'}
{$ENDIF FPC}

var
  OpenThread: TOpenThread = nil;
{$IFDEF FPC}
  CreateToolhelp32Snapshot: TCreateToolhelp32Snapshot = nil;
  Thread32First: TThread32First = nil;
  Thread32Next: TThread32Next = nil;
{$ENDIF FPC}
  hKernel: THandle;
  OpenThreadExist: Boolean = False;
  FreeKernel: Boolean = False;
  SizeOfAlloc: DWORD = 0; // See initialization !
  SysInfo: TSystemInfo;
  InternalFuncs: TInternalFuncs;
{$IFDEF SUPPORTS_MONITOR}
  FLock: TObject = nil;
{$ELSE !SUPPORTS_MONITOR}
  FLock: TCriticalSection = nil;
{$ENDIF SUPPORTS_MONITOR }
  { ================================== Utils ================================== }

function GetUInt64Size(const Value: UInt64): Integer; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF SUPPORTS_INLINE}
begin
  if UInt8(Value) = Value then
    Result := 1
  else if UInt16(Value) = Value then
    Result := 2
  else if UInt32(Value) = Value then
    Result := 4
  else
    Result := 8;
end;

function GetInt64Size(const Value: Int64): Integer; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF SUPPORTS_INLINE}
begin
  if Int8(Value) = Value then
    Result := 1
  else if Int16(Value) = Value then
    Result := 2
  else if Int32(Value) = Value then
    Result := 4
  else
    Result := 8;
end;

procedure EnterLook(LockedObject: TObject); {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF SUPPORTS_INLINE}
begin
{$IFDEF SUPPORTS_MONITOR}
  TMonitor.Enter(LockedObject);
{$ELSE !SUPPORTS_MONITOR}
  TCriticalSection(LockedObject).Enter();
{$ENDIF SUPPORTS_MONITOR}
end;

procedure LeaveLook(LockedObject: TObject); {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF SUPPORTS_INLINE}
begin
{$IFDEF SUPPORTS_MONITOR}
  TMonitor.Exit(LockedObject);
{$ELSE !SUPPORTS_MONITOR}
  TCriticalSection(LockedObject).Leave();
{$ENDIF SUPPORTS_MONITOR}
end;

function EnumProcessThreads(PID: DWORD; CallBack: TEnumThreadCallBack; Param: Pointer): BOOL;
var
  hSnap: THandle;
  te: TThreadEntry32;
  Next: Boolean;
begin
  hSnap := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, PID);
  Result := hSnap <> INVALID_HANDLE_VALUE;
  if Result then
  begin
    te.dwSize := SizeOf(TThreadEntry32);
    Next := Thread32First(hSnap, te);
    while Next do
    begin
      if (te.th32OwnerProcessID = PID) then
      begin
        try
          if not CallBack(te.th32ThreadID, Param) then
            break;
        except
        end;
      end;
      Next := Thread32Next(hSnap, te);
    end;
    Result := CloseHandle(hSnap);
  end;
end;

function SetMemPermission(const P: Pointer; const Size: SIZE_T; const NewProtect: DWORD): DWORD;
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

function GetDispDataSize(PInst: PInstruction): Integer;
begin
  Result := 0;
  if PInst^.Disp.Flags and dfUsed <> 0 then
  begin
    if PInst^.Archi = CPUX32 then
    begin
      if PInst^.Prefixes and Prf_OpSize <> 0 then
        Result := ops16bits
      else
        Result := ops32bits;
      Exit;
    end
    else
    begin
      case PInst^.OperandFlags of
        opdD64:
          begin
            {
              Defaults to O64 in PM64.
              PrfOpSize results in O16.
            }
            if PInst^.Prefixes and Prf_OpSize <> 0 then
              Result := ops16bits
            else
              Result := ops64bits;
          end;
        opdF64, opdDv64:
          begin
            { The operand size is forced to a 64-bit operand size in PM64 ! }
            Result := (ops64bits);
            Exit;
          end;
        opdDf64:
          begin
            {
              Defaults to O64 in PM64.
              PrfOpSize results in O16 in AMD64.
              PrfOpSize is ignored in EM64T.
            }
            if (CPUVendor = vAMD) and (PInst^.Prefixes and Prf_OpSize <> 0) then
              Result := (ops16bits)
            else
              Result := (ops64bits);
            Exit;
          end;
      else
        begin
          if PInst^.Rex.W then
            Result := (ops64bits)
          else if (PInst^.Prefixes and Prf_OpSize <> 0) then
            Result := (ops16bits)
          else
            Result := (ops32bits);
          Exit;
        end;
      end;
    end;
  end;
end;

function fDecodeInst(PInst: PInstruction): Integer;
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

function RoundMultipleOf(const Value, MultipleOf: NativeInt): NativeInt; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF SUPPORTS_INLINE}
begin
  if Value = 0 then
  begin
    Result := (MultipleOf);
    Exit;
  end;
  Result := ((Value + (MultipleOf - 1)) and not(MultipleOf - 1));
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

  if (NativeUInt(P) < NativeUInt(pMin)) or (NativeUInt(P) > NativeUInt(pMax)) then
    Exit;
  if InternalFuncs.VirtualQuery(P, mbi, SizeOf(mbi)) = 0 then
    Exit;

  pBase := mbi.BaseAddress;
  Q := pBase;
  while NativeUInt(Q) < NativeUInt(pMax) do
  begin
    if InternalFuncs.VirtualQuery(Q, mbi, SizeOf(mbi)) = 0 then
      Exit;
    if (mbi.State = MEM_FREE) and (mbi.RegionSize >= dwAllocGran) and (mbi.RegionSize >= MemSize) then
    begin
      { The address (P) must be multiple of the allocation granularity (dwAllocationGranularity) . }
      P := PByte(RoundMultipleOf(NativeInt(Q), dwAllocGran));
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
  while NativeUInt(Q) > NativeUInt(pMin) do
  begin
    if InternalFuncs.VirtualQuery(Q, mbi, SizeOf(mbi)) = 0 then
      Exit;
    if (mbi.State = MEM_FREE) and (mbi.RegionSize >= dwAllocGran) and (mbi.RegionSize >= MemSize) then
    begin
      P := PByte(RoundMultipleOf(NativeInt(Q), dwAllocGran));
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

function InsertJmp(Src, Dst: PByte; JmpType: Integer; const DstSave: PByte = nil): Integer;
var
  Offset: NativeInt;
  JmpSize: Integer;
begin
  Result := 1;
  JmpSize := JmpTypeToSize[JmpType];
  Offset := NativeInt(NativeInt(Dst) - NativeInt(Src)) - JmpSize;
  case JmpType of
    JT_NONE:
      begin
        raise InterceptException.Create(SErrorInvalidJmp);
      end;
    JT_REL8:
      begin
        PByte(Src)^ := opJmpRelb;
        Inc(Src);
        PInt8(Src)^ := Int8(Offset);
      end;
    JT_REL16:
      begin
{$IFDEF CPUX64}
        {
          JMP Rel16
          ==> Not supported on x64!
        }
        raise InterceptException.Create(SErrorInvalidJmp64);
{$ENDIF CPUX64}
        PByte(Src)^ := opPrfOpSize;
        Inc(Src);
        PByte(Src)^ := opJmpRelz;
        Inc(Src);
        PInt16(Src)^ := Int16(Offset);
      end;
    JT_REL32:
      begin
        PByte(Src)^ := opJmpRelz;
        Inc(Src);
        PInt32(Src)^ := Offset;
      end;
    JT_MEM16:
      begin
{$IFDEF CPUX64}
        {
          JMP WORD [012345]
          ==> Not supported on x64!
        }
        raise InterceptException.Create(SErrorInvalidJmp64);
{$ENDIF CPUX64}
        if not Assigned(DstSave) then
          raise InterceptException.Create(SErrorInvalidDstSave);
        PByte(Src)^ := opPrfOpSize;
        Inc(Src);
        PWord(Src)^ := opJmpMem;
        Inc(Src, 2);
        PUInt32(Src)^ := UInt32(DstSave);
        PUInt16(DstSave)^ := UInt16(Dst);
      end;
    JT_MEM32:
      begin
{$IFDEF CPUX64}
        {
          JMP DWORD [012345]
          ==> Not supported on x64!
        }
        raise InterceptException.Create(SErrorInvalidJmp64);
{$ENDIF CPUX64}
        if not Assigned(DstSave) then
          raise InterceptException.Create(SErrorInvalidDstSave);
        PWord(Src)^ := opJmpMem;
        Inc(Src, 2);
        PUInt32(Src)^ := UInt32(DstSave);
        PUInt32(DstSave)^ := UInt32(Dst);
      end;
    JT_MEM64:
      begin
{$IFDEF CPUX86}
        {
          JMP QWORD [0123456789]
          ==> Not supported on x32!
        }
        raise InterceptException.Create(SErrorInvalidJmp32);
{$ENDIF CPUX86}
        if not Assigned(DstSave) then
          raise InterceptException.Create(SErrorInvalidDstSave);
        { RIP Disp ! }
        PUInt64(DstSave)^ := UInt64(Dst);
        Offset := NativeInt(NativeInt(DstSave) - NativeInt(Src)) - JmpSize;

        PWord(Src)^ := opJmpMem;
        Inc(Src, 2);
        PInt32(Src)^ := Offset;
      end;
    JT_RIPZ:
      begin
{$IFDEF CPUX86}
        raise InterceptException.Create(SErrorInvalidJmp32);
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

function GetJmpType(Src, Dst, DstSave: PByte): Integer;
var
  Offset: NativeInt;
  OffsetSize: Integer;
begin
  Offset := NativeInt(NativeInt(Src) - NativeInt(Dst));
  OffsetSize := GetInt64Size(Offset);
  Result := SizeToJmpType[OffsetSize shr 1];
{$IFDEF CPUX64}
  if Result = JT_MEM64 then
  begin
    if not Assigned(DstSave) then
      raise InterceptException.Create(SErrorInvalidDstSave);
    Offset := NativeInt(NativeInt(DstSave) - NativeInt(Src)) - 7;
    if Integer(Offset) <> Offset then
    begin
      Result := (JT_RIPZ);
      Exit;
    end;
  end;
{$ENDIF CPUX64}
end;

function IsMultiBytesNop(P: Pointer; Size: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Size > 0 then
  begin
    while (Size > 0) do
    begin
      for i := Length(MultiNops) downto 1 do
      begin
        if Size >= i then
        begin
          Result := CompareMem(MultiNops[i - 1], P, i);
          if Result then
          begin
            Inc(PByte(P), i);
            Dec(Size, i);
            break;
          end;
        end;
      end;
      if not Result then
        Exit;
    end;
    Result := True;
  end;
end;

procedure FillMultiNop(var Buffer; Size: Integer);
var
  i: Integer;
  P: PByte;
begin
  { Multi Bytes Nop Instruction is fast to execute compared to
    the traditional NOP instruction.

    However it's not supported by all CPU !
    ==> Use FillNop(P,Size,True).

    ==> CPUID implements a routine to detect
    if the CPU supports Multi Bytes Nop .
  }
  if not(iMultiNop in CPUInsts) then
    raise InterceptException.Create(SErrorUnsupportedMultiNop);

  P := PByte(@Buffer);
  for i := Length(MultiNops) downto 1 do
  begin
    while Size >= i do
    begin
      Move(MultiNops[i - 1]^, P^, i);
      Dec(Size, i);
      Inc(P, i);
    end;
    if Size = 0 then
      Exit;
  end;
end;

function IsNop(P: PByte; Size: Integer): Boolean;
var
  i: Integer;
begin
  { Return True if the first instructions are nop/multi nop. }
  Result := False;
  if iMultiNop in CPUInsts then
    Result := IsMultiBytesNop(P, Size)
  else
    for i := 0 to Size - 1 do
    begin
      Result := (P^ = opNop);
      if not Result then
        Exit;
      Inc(P); // Next Byte.
    end;
end;

procedure FillNop(var P; Size: Integer; MultipleNop: Boolean);
begin
  if MultipleNop and (iMultiNop in CPUInsts) then
    FillMultiNop(P, Size)
  else
    FillChar(P, Size, opNop);
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
    tbFPU:
      Inc(Result, 2); // [$D8..$D9] + ModRm !
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

function GetJccOpCode(PInst: PInstruction; RelSize: Integer): DWORD;
var
  OpCode: Byte;
  Opcodes: array [0 .. 3] of Byte;
begin
  FillChar(PByte(@Opcodes[0])^, 4, #00);
  OpCode := PInst^.OpCode and $F;
  case RelSize of
    ops8bits:
      begin
        Opcodes[0] := $70 or OpCode;
      end;
    ops16bits:
      begin
        Opcodes[0] := opPrfOpSize;
        Opcodes[1] := $0F;
        Opcodes[2] := $80 or OpCode;
      end;
    ops32bits:
      begin
        Opcodes[0] := $0F;
        Opcodes[1] := $80 or OpCode;
      end;
  end;
  Result := PDWORD(@Opcodes[0])^;
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
  NOpc: DWORD;
  PQ: PByte;
  Relsz: Integer;
  JmpType: Integer;
  JmpSize: Integer;
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
              InsertJmp(PQ, PInst.Branch.Target, JT_RIPZ);
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
              InsertJmp(PQ, PInst.Branch.Target, JT_RIPZ);
              Inc(PQ, 14);
            end;
        end;
      end;
    end
    else
    begin
      { Jcc ! }
      NOpc := GetJccOpCode(PInst, Relsz);
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
            JmpType := GetJmpType(PByte(NativeInt(NewAddr) + 4), PInst^.Branch.Target, PByte(NativeInt(NewAddr) + 4 + 6));
            JmpSize := JmpTypeToSize[JmpSize];
            if JmpType > JT_REL32 then
              Inc(JmpSize, SizeOf(Pointer));

            { Jmp To Next Valid Instruction ! }
            PQ^ := opJmpRelb;
            Inc(PQ);
            PQ^ := JmpSize;
            Inc(PQ);
            InsertJmp(PByte(NativeInt(NewAddr) + 4), PInst^.Branch.Target, JmpType, PByte(NativeInt(NewAddr) + 4 + 6));
            Inc(PQ, JmpSize);
          end;
      end;
    end;
  finally
    FreeMem(POpc);
  end;
  Result := Integer(NativeInt(PQ) - NativeInt(NewAddr));
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

  P := PByte(Int64(P) + Int64(PInst^.Disp.Value));

  Offset := Int64(Int64(P) - Int64(NewAddr) - PInst^.InstSize);
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
      { MOV REG,Imm(NativeInt) }
      PByte(NewAddr)^ := $B8 + (rReg and $7);
      Inc(NewAddr);
      PNativeInt(NewAddr)^ := NativeInt(P);
      Inc(NewAddr, SizeOf(NativeInt));

      { Set the original instruction opcodes }
      POpc := GetMemory(MAX_INST_LENGTH_N);
      L := GetInstOpCodes(PInst, POpc);

      Move(POpc^, NewAddr^, L);
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
      Result := (NativeInt(NewAddr) - NativeInt(pFrst));
      Exit;
    end
    else
      raise InterceptException.Create(SErrorRipDisp);
  end;
  Move(PInst^.Addr^, NewAddr^, PInst^.InstSize);
  Inc(NewAddr, PInst^.InstSize);
  PInt32(NativeInt(NewAddr) - SizeOf(Int32))^ := Int32(Offset);

  Result := PInst^.InstSize;
end;

function CorrectJmpRel(PInst: PInstruction; NewAddr: PByte): Integer;
var
  JmpType: Byte;
begin
  JmpType := GetJmpType(NewAddr, PInst^.Branch.Target, PByte(NativeInt(NewAddr) + 6));
  InsertJmp(NewAddr, PInst^.Branch.Target, JmpType, PByte(NativeInt(NewAddr) + 6));
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
  Result := NativeInt(P) - NativeInt(NewAddr);
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

{$IFNDEF FPC}
{$WARN COMPARISON_TRUE OFF}
{$ENDIF FPC}

function GetInstArithmeticType(PInst: PInstruction): Integer;
  function IsInstAdd(PInst: PInstruction): Boolean;
  begin
    Result := False;
    if PInst^.OpTable = tbOneByte then
    begin
      if (PInst^.OpCode >= $00) and (PInst^.OpCode < $06) then
      begin
        Result := (True);
        Exit;
      end;
    end;
    if (PInst^.OpKind = kGrp) and (PInst^.ModRm.Reg = $00) then
    begin
      if (PInst^.OpCode > $7F) and (PInst^.OpCode < $84) then
      begin
        Result := (True);
        Exit;
      end;
    end;
  end;
  function IsInstSub(PInst: PInstruction): Boolean;
  begin
    Result := False;
    if PInst^.OpTable = tbOneByte then
    begin
      if (PInst^.OpCode > $27) and (PInst^.OpCode < $2E) then
      begin
        Result := (True);
        Exit;
      end;
    end;
    if (PInst^.OpKind = kGrp) and (PInst^.ModRm.Reg = $05) then
    begin
      if (PInst^.OpCode > $7F) and (PInst^.OpCode < $84) then
      begin
        Result := (True);
        Exit;
      end;
    end;
  end;
  function IsInstInc(PInst: PInstruction): Boolean;
  begin
    Result := False;
    if (PInst^.Archi = CPUX32) and (PInst^.OpTable = tbOneByte) then
    begin
      if (PInst^.OpCode >= $40) and (PInst^.OpCode <= $47) then
      begin
        Result := (True);
        Exit;
      end;
    end;
    if (PInst^.OpKind = kGrp) and (PInst^.ModRm.Reg = $00) then
    begin
      if (PInst^.OpCode = $FE) or (PInst^.OpCode = $FF) then
      begin
        Result := (True);
        Exit;
      end;
    end;
  end;
  function IsInstDec(PInst: PInstruction): Boolean;
  begin
    Result := False;
    if (PInst^.Archi = CPUX32) and (PInst^.OpTable = tbOneByte) then
    begin
      if (PInst^.OpCode >= $48) and (PInst^.OpCode <= $4F) then
      begin
        Result := (True);
        Exit;
      end;
    end;
    if (PInst^.OpKind = kGrp) and (PInst^.ModRm.Reg = $01) then
    begin
      if (PInst^.OpCode = $FE) or (PInst^.OpCode = $FF) then
      begin
        Result := (True);
        Exit;
      end;
    end;
  end;

begin
  { Return Instruction Arithmetic (+ or - or ..) }
  Result := arNone;
  if IsInstAdd(PInst) or IsInstInc(PInst) then
    Result := (arAdd)
  else if IsInstSub(PInst) or IsInstDec(PInst) then
    Result := (arSub);
end;
{$IFNDEF FPC}
{$WARN COMPARISON_TRUE ON}
{$ENDIF FPC}

function EvalArith(Arith: Integer; Value: NativeInt; Offset: NativeInt): NativeInt;
begin
  Result := Value;
  case Arith of
    arAdd:
      Inc(Result, Offset);
    arInc:
      Inc(Result);
    arSub:
      Dec(Result, Offset);
    arDec:
      Dec(Result);
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
  Arith: Integer;
  Skip: Boolean;
  sReg: ShortInt;
begin

  if not Assigned(@AIntf) then
  begin
    Result := nil;
    Exit;
  end;

  sReg := -1;
  PObj := PByte(AIntf);
  FillChar(Inst, SizeOf(TInstruction), #00);
  Inst.Archi := CPUX;
  Pvt := PPointer(AIntf)^; // vTable !
  PCode := PPointer(NativeInt(Pvt) + Offset)^; // Code Entry !
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
      break;

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
      PObj := PByte(EvalArith(Arith, NativeInt(PObj), imm));
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
  FillChar(Inst, SizeOf(TInstruction), #00);
  Inst.Archi := CPUX;
  Pvt := PPointer(PInterface)^; // Virtual Table !
  P := Pvt;
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
      break;
    end;
  end;
  Result := PDst;
end;

{$IFDEF SUPPORTS_RTTI}

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
    begin
      Result := LMethod.CodeAddress;
      Exit;
    end;
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

{$ENDIF  SUPPORTS_RTTI}

function GetRoot(P: PByte): PByte;
var
  Inst: TInstruction;
begin
  Result := P;
  FillChar(Inst, SizeOf(TInstruction), #00);
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

function IsValidDescriptor(P: PByte): Boolean;
begin
  Result := CompareMem(P, PByte(@DscrSig[0]), SizeOf(DscrSig));
end;

function GetDescriptor(P: PByte): PDescriptor;
var
  Inst: TInstruction;
  function IsDscrpInst(PInst: PInstruction): Boolean;
  begin
    Result := Assigned(PInst.Branch.Target) or IsNop(PInst.Addr, 6);
  end;

begin
  Result := nil;
  FillChar(Inst, SizeOf(TInstruction), #00);
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

function CreateNewDescriptor(): PDescriptor;
begin
  { Create a new descriptor tables ! }
  Result := AllocMem(SizeOf(TDescriptor));
  FillNop(Result^, SizeOf(TDescriptor), False);
  FillNop(Result^.JmpMems[0], SizeOf(TJmpMem) * (MAX_HOOKS + 1), True);

  { A valid descriptor have a valid signature . }
  CopyMemory(Result, PByte(@DscrSig[0]), DscrSigSize);
  Result^.nHook := 0;
  Result^.Flags := 0;
  Result^.ExMem := nil;
end;

procedure InsertDescriptor(PAt: PByte; PDscr: PDescriptor);
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
  Sb := 0;
  P := PAt;
  PDscr^.OrgPtr := P;
  fJmpType := GetJmpType(P, @PDscr^.CodeEntry, @PDscr^.DscrAddr);
{$IFDEF CPUX64}
  Tmp := nil;
  PExMem := TryAllocMemAt(P, SizeOfAlloc, PAGE_EXECUTE_READWRITE);
  LPExMem := PExMem;
  sJmpType := JT_NONE;
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
  end
  else
  begin
    JmpKind := kJmpCE;
  end;
{$ELSE !CPUX64}
  PExMem := TryAllocMemAt(nil, SizeOfAlloc, PAGE_EXECUTE_READWRITE);
  JmpKind := kJmpCE;
  LPExMem := PExMem;
{$ENDIF CPUX64}
  FillChar(Inst, SizeOf(TInstruction), #00);
  Inst.Archi := CPUX;
  Inst.NextInst := P;
  Inst.VirtualAddr := nil;

  JmpSize := JmpTypeToSize[fJmpType];

  while Sb < JmpSize do
  begin
    if Inst.OpType = otRET then
      raise InterceptException.Create(SErrorSmallFunctionSize);
    Inst.Addr := Inst.NextInst;
    Inc(Sb, fDecodeInst(@Inst));
  end;

  if Sb > TrampoSize then
    raise InterceptException.Create(SErrorBigTrampoSize);

  { Trampoline momory }
  T := PExMem;
  FillNop(T^, TrampoSize, False);

  PDscr^.Trampo := AllocMem(SizeOf(TTrampoInfo));
  PDscr^.Trampo^.PData := AllocMem(Sb + 6);
  FillNop(PDscr^.Trampo^.PData^, Sb + 6, False);
  { Save original target routine instruction . }
  Move(P^, PDscr^.Trampo^.PData^, Sb);
  PDscr^.Trampo^.Addr := T; // Pointer to the first trampoline instruction.
  PDscr^.Trampo^.Size := Sb; // Size of stolen instructions .

  Tsz := MapInsts(P, T, Sb);
  OrgAccess := SetMemPermission(P, Sb, PAGE_EXECUTE_READWRITE);
  try
    FillNop(P^, Sb, False);
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
            JMP @PDscr^.CodeEntry  ==> JT_RIPZ
          }
          InsertJmp(P, Tmp, fJmpType, Tmp + 6);
          InsertJmp(Tmp, @PDscr^.CodeEntry, JT_RIPZ, nil);
        end;
      kJmpRipZCE:
        begin
          {
            Not a good jump !

            TargetProc :
            JMP @PDscr^.CodeEntry  ==> JT_RIPZ
          }
          InsertJmp(P, @PDscr^.CodeEntry, JT_RIPZ, nil);
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
    InsertJmp(T + Tsz, P + Sb, JT_RIPZ);
{$ELSE !CPUX64}
    InsertJmp(PByte(NativeInt(T) + Tsz), PByte(NativeInt(P) + Sb), JT_MEM32, PByte(NativeInt(T) + Tsz + 6));
{$ENDIF CPUX64}
    { Save LPExMem ==> we need it when deleting descriptor }
    PDscr^.ExMem := LPExMem;

    SetMemPermission(LPExMem, SizeOfAlloc, PAGE_EXECUTE_READWRITE);
    SetMemPermission(PDscr, SizeOf(TDescriptor), PAGE_EXECUTE_READWRITE);
  finally
    SetMemPermission(P, Sb, OrgAccess);
  end;
end;

procedure MadExceptFreeMem(P: Pointer);
var
  Page: Pointer;
  mbi: TMemoryBasicInformation;
  Permission: DWORD;
begin
  if InternalFuncs.VirtualQuery(P, mbi, SizeOf(mbi)) <> 0 then
  begin
    Page := mbi.BaseAddress;
    Permission := SetMemPermission(Page, SysInfo.dwPageSize, PAGE_READWRITE);
    FreeMem(P);
    SetMemPermission(Page, SysInfo.dwPageSize, Permission);
  end
  else
    FreeMem(P);
end;

function GetNextHookPtrFromTrampoline(TrampoLine: Pointer): PNextHook;
begin
  if Assigned(TrampoLine) then
  begin
    Result := PNextHook(NativeInt(TrampoLine) - SizeOf(TNextHook));
    if Result^.Signature = TrampolineSignature then
      Exit;
  end;
  raise DetourException.Create(SErrorInvalidTrampoline);
end;

function AddHook(PDscr: PDescriptor; InterceptProc: PByte; Param: Pointer; Options: TInterceptOptions): PByte;
var
  n: ShortInt;
  NxHook: PByte;
  LTlsRecursionLevelIndex: DWORD;
begin
  {
    Return a pointer to a function that can
    call next installed Hooks.
  }
  n := PDscr^.nHook;
  if n + 1 > MAX_HOOKS then
    raise InterceptException.Create(SErrorMaxHook);

  { Alloc memory for the NextHook ! }
  NxHook := AllocMem(TrampoSize);
  Result := NxHook;

  FillNop(Result^, TrampoSize, False);

  PNextHook(Result)^.PDscr := PDscr;
  PNextHook(Result)^.ID := n + 1;
  PNextHook(Result)^.threadid := GetCurrentThreadId();
  PNextHook(Result)^.Param := Param;
  PNextHook(Result)^.Signature := TrampolineSignature;
  PNextHook(Result)^.InterceptOptions := Options;
  if ioRecursive in Options then
  begin
    LTlsRecursionLevelIndex := TlsAlloc();
    if LTlsRecursionLevelIndex <> TLS_OUT_OF_INDEXES then
      PNextHook(Result)^.TlsRecursionLevelIndex := LTlsRecursionLevelIndex
    else
      raise DetourException.Create(SErrorTlsOutOfIndexes);
  end;
  Inc(Result, SizeOf(TNextHook));

  { Redirect code to InterceptProc ! }
  InsertJmp(@PDscr^.JmpMems[n], InterceptProc, JT_MEMN, @PDscr^.JmpAddrs[n]);
  { Redirect code to TrampoLine ! }
  InsertJmp(@PDscr^.JmpMems[n + 1], PDscr^.Trampo^.Addr, JT_MEMN, @PDscr^.JmpAddrs[n + 1]);
  { Redirect code to next hook ! }
  InsertJmp(Result, @PDscr^.JmpMems[n + 1], JT_MEMN, PByte(NativeInt(Result) + 6));
  Inc(PDscr^.nHook);

  SetMemPermission(Result, JmpTypeToSize[JT_RIPZ], PAGE_EXECUTE_READWRITE);
end;

function InstallHook(TargetProc, InterceptProc: PByte; Param: Pointer; Options: TInterceptOptions): PByte;
var
  P: PByte;
  PDscr: PDescriptor;
begin
  if not Assigned(TargetProc) then
    raise InterceptException.Create(SErrorInvalidTargetProc);

  if not Assigned(InterceptProc) then
    raise InterceptException.Create(SErrorInvalidInterceptProc);

  PDscr := GetDescriptor(TargetProc);
  if not Assigned(PDscr) then
  begin
    P := GetRoot(TargetProc);
    PDscr := CreateNewDescriptor();
    try
      InsertDescriptor(P, PDscr);
    except
      FreeMem(PDscr);
      raise;
    end;
  end;
  Result := AddHook(PDscr, InterceptProc, Param, Options);
end;

procedure RemoveDescriptor(PDscr: PDescriptor);
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

    FillNop(PDscr^.ExMem^, SizeOfAlloc, False);
    FreeMem(PDscr^.Trampo^.PData);
    FreeMem(PDscr^.Trampo);

    if Assigned(PDscr^.ExMem) then
    begin
      vr := InternalFuncs.VirtualFree(PDscr^.ExMem, 0, MEM_RELEASE);
      if not vr then
        RaiseLastOSError;
    end;

    FillNop(PDscr^, SizeOf(TDescriptor), False);
{$IFDEF FIX_MADEXCEPT}
    MadExceptFreeMem(PDscr);
{$ELSE !FIX_MADEXCEPT}
    FreeMem(PDscr);
{$ENDIF FIX_MADEXCEPT}
  finally
    SetMemPermission(P, sz, OrgAccess);
  end;
end;

function RemoveHook(TrampoLine: PByte): Integer;
var
  PNxtHook: PNextHook;
  PDscr: PDescriptor;
  n: Byte;
begin
  if not Assigned(TrampoLine) then
    raise InterceptException.Create(SErrorInvalidTrampoline);

  PNxtHook := GetNextHookPtrFromTrampoline(TrampoLine);
  if not Assigned(PNxtHook) then
    raise InterceptException.Create(SErrorInvalidTrampoline);

  PDscr := PNxtHook^.PDscr;
  if not IsValidDescriptor(PByte(PDscr)) then
    raise InterceptException.Create(SErrorInvalidDescriptor);

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

  if ioRecursive in PNxtHook^.InterceptOptions then
    TlsFree(PNxtHook^.TlsRecursionLevelIndex);

{$IFDEF FIX_MADEXCEPT}
    MadExceptFreeMem(PNxtHook);
{$ELSE !FIX_MADEXCEPT}
    FreeMem(PNxtHook);
{$ENDIF FIX_MADEXCEPT}

end;

{ ======================================= InterceptCreate ======================================= }

function InterceptCreate(const TargetProc, InterceptProc: Pointer; const Param: Pointer = nil;
  const Options: TInterceptOptions = DefaultInterceptOptions): Pointer;
begin
  Result := InstallHook(TargetProc, InterceptProc, Param, Options);
end;

function InterceptCreate(const TargetInterface; MethodIndex: Integer; const InterceptProc: Pointer; const Param: Pointer = nil;
  const Options: TInterceptOptions = DefaultInterceptOptions): Pointer;
var
  P: PByte;
begin
  Result := nil;
  if not Assigned(@TargetInterface) then
    Exit;
  P := GetInterfaceMethodPtrByIndex(TargetInterface, MethodIndex);
  if Assigned(P) then
  begin
    Result := InterceptCreate(P, InterceptProc, Param, Options);
  end;
end;

function InterceptCreate(const Module, MethodName: string; const InterceptProc: Pointer; const Param: Pointer = nil;
  const Options: TInterceptOptions = DefaultInterceptOptions): Pointer;
var
  pOrgPointer: Pointer;
  LModule: THandle;
begin
  { RRUZ's idea ==> Looks great ! }
  Result := nil;
  LModule := GetModuleHandle(PChar(Module));
  if (LModule = 0) and (ioForceLoad in Options) then
    LModule := LoadLibrary(PChar(Module));

  if LModule <> 0 then
  begin
    pOrgPointer := GetProcAddress(LModule, PChar(MethodName));
    if Assigned(pOrgPointer) then
      Result := InterceptCreate(pOrgPointer, InterceptProc, Param, Options);
  end;
end;

procedure InterceptCreate(const TargetProc, InterceptProc: Pointer; var TrampoLine: Pointer; const Param: Pointer = nil;
  const Options: TInterceptOptions = DefaultInterceptOptions);
begin
  TrampoLine := InstallHook(TargetProc, InterceptProc, Param, Options);
end;

{$IFDEF SUPPORTS_RTTI}

function InterceptCreate(const TargetInterface; const MethodName: String; const InterceptProc: Pointer; const Param: Pointer = nil;
  const Options: TInterceptOptions = DefaultInterceptOptions): Pointer; overload;
var
  P: PByte;
begin
  { Interface support }
  Result := nil;
  if (not Assigned(@TargetInterface)) or (MethodName = EmptyStr) then
    Exit;

  P := GetInterfaceMethodPtrByName(TargetInterface, MethodName);
  if Assigned(P) then
    Result := InterceptCreate(P, InterceptProc);
end;

{$ENDIF SUPPORTS_RTTI}
{ ======================================= InterceptRemove ======================================= }

function InterceptRemove(const TrampoLine: Pointer): Integer;
begin
  if Assigned(TrampoLine) then
    Result := RemoveHook(TrampoLine)
  else
    Result := -1;
end;

{ ======================================= GetHookCount ======================================= }

function GetHookCount(const TargetProc: Pointer): Integer;
var
  PDscr: PDescriptor;
begin
  { Return the number of installed hooks. }
  if Assigned(TargetProc) then
  begin
    PDscr := GetDescriptor(TargetProc);
    if Assigned(PDscr) then
    begin
      Result := PDscr^.nHook;
      Exit;
    end;
  end
  else
    raise InterceptException.Create(SErrorInvalidTargetProc);
  Result := 0;
end;

function GetHookCount(const TargetInterface; MethodIndex: Integer): Integer; overload;
var
  P: PByte;
begin
  P := GetInterfaceMethodPtrByIndex(TargetInterface, MethodIndex);
  Result := GetHookCount(P);
end;
{$IFDEF SUPPORTS_RTTI}

function GetHookCount(const TargetInterface; const MethodName: String): Integer; overload;
var
  P: PByte;
begin
  { Interface support }
  P := GetInterfaceMethodPtrByName(TargetInterface, MethodName);
  Result := GetHookCount(P);
end;
{$ENDIF  SUPPORTS_RTTI}
{ ======================================= IsHooked ======================================= }

function IsHooked(const TargetProc: Pointer): Boolean;
begin
  Result := GetHookCount(TargetProc) > 0;
end;

function IsHooked(const TargetInterface; MethodIndex: Integer): Boolean; overload;
var
  P: PByte;
begin
  P := GetInterfaceMethodPtrByIndex(TargetInterface, MethodIndex);
  Result := IsHooked(P);
end;

{$IFDEF SUPPORTS_RTTI}

function IsHooked(const TargetInterface; const MethodName: String): Boolean; overload;
var
  P: PByte;
begin
  { Interface support }
  P := GetInterfaceMethodPtrByName(TargetInterface, MethodName);
  Result := IsHooked(P);
end;
{$ENDIF  SUPPORTS_RTTI}
{ ======================================= Patch ======================================= }

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
    PNativeInt(Q)^ := NativeInt(DstAddr);
{$ELSE !CPUX64}
    PWord(Q)^ := opJmpMem;
    Inc(Q, 2);
    PUInt32(Q)^ := UInt32(NativeInt(Q) + 4);
    PUInt32(NativeInt(Q) + 4)^ := UInt32(DstAddr);
{$ENDIF CPUX64}
  finally
  end;
end;

function UnPatchVt(const TrampoLine: Pointer): Boolean;
var
  OrgAccess: DWORD;
  PInfo: PTrampoDataVt;
begin
  if not Assigned(TrampoLine) then
  begin
    Result := False;
    Exit;
  end;

  try
    PInfo := PTrampoDataVt(NativeInt(TrampoLine) - SizeOf(TTrampoDataVt));
    OrgAccess := SetMemPermission(PInfo^.vAddr, 32, PAGE_EXECUTE_READWRITE);
    try
      PPointer(PInfo^.vAddr)^ := PInfo^.Addr;
    finally
      SetMemPermission(PInfo^.vAddr, 32, OrgAccess);
    end;
    Result := InternalFuncs.VirtualFree(TrampoLine, 0, MEM_RELEASE);
  finally
  end;
end;

{ ======================================= Trampoline misc =================================== }

function GetCreatorThreadIdFromTrampoline(var TrampoLine): TThreadId;
var
  PNxtHook: PNextHook;
begin
  PNxtHook := GetNextHookPtrFromTrampoline(PPointer(@TrampoLine)^);
  Result := PNxtHook^.threadid;
end;

function GetTrampolineParam(var TrampoLine): Pointer;
var
  PNxtHook: PNextHook;
begin
  PNxtHook := GetNextHookPtrFromTrampoline(PPointer(@TrampoLine)^);
  Result := PNxtHook^.Param;
end;

{ ======================================= Recursive Section ======================================= }

function EnterRecursiveSection(var TrampoLine; MaxRecursionLevel: NativeInt = 0): Boolean;
var
  PNxtHook: PNextHook;
  RecursionLevel: NativeInt;
begin
  PNxtHook := GetNextHookPtrFromTrampoline(PPointer(@TrampoLine)^);
  if ioRecursive in PNxtHook^.InterceptOptions then
  begin
    RecursionLevel := NativeInt(TlsGetValue(PNxtHook^.TlsRecursionLevelIndex));
    Result := RecursionLevel <= MaxRecursionLevel;
    if Result then
    begin
      Inc(RecursionLevel);
      TlsSetValue(PNxtHook^.TlsRecursionLevelIndex, Pointer(RecursionLevel));
    end;
  end
  else
    raise DetourException.Create(SErrorRecursiveSectionUnsupported);
end;

function ExitRecursiveSection(var TrampoLine): Boolean;
var
  PNxtHook: PNextHook;
  RecursionLevel: NativeInt;
begin
  PNxtHook := GetNextHookPtrFromTrampoline(PPointer(@TrampoLine)^);
  if ioRecursive in PNxtHook^.InterceptOptions then
  begin
    RecursionLevel := NativeInt(TlsGetValue(PNxtHook^.TlsRecursionLevelIndex));
    Result := RecursionLevel >= 0;
    if Result then
    begin
      Dec(RecursionLevel);
      TlsSetValue(PNxtHook^.TlsRecursionLevelIndex, Pointer(RecursionLevel));
    end;
  end
  else
    raise DetourException.Create(SErrorRecursiveSectionUnsupported);
end;

{ ======================================= Transaction ======================================= }
function CountThreadCallBack(ID: DWORD; Param: Pointer): BOOL;
begin
  Assert(Assigned(Param));
  Inc(PInteger(Param)^);
  Result := True;
end;

function SuspendOrResumeThread(threadid: DWORD; Suspend: Boolean): DWORD;
var
  hThread: THandle;
begin
  hThread := OpenThread(THREAD_SUSPEND_RESUME, False, threadid);
  if hThread <> THandle(0) then
  begin
    if Suspend then
      Result := SuspendThread(hThread)
    else
      Result := ResumeThread(hThread);
    CloseHandle(hThread);
  end
  else
    Result := DWORD(-1);
end;

function SuspendThreadCallBack(ID: DWORD; Param: Pointer): BOOL;
var
  PStruct: PTransactionStruct;
  SuspendCount: DWORD;
begin
  Assert(Assigned(Param));
  PStruct := PTransactionStruct(Param);
  if ID <> PStruct^.TID then
  begin
    SuspendCount := SuspendOrResumeThread(ID, True);
    if SuspendCount <> DWORD(-1) then
    // thread's previously was running  .
    begin
      { Only add threads that was running before suspending them ! }
      PStruct^.SuspendedThreads^[PStruct^.SuspendedThreadCount] := ID;
      Inc(PStruct^.SuspendedThreadCount);
    end;
  end;
  Result := True;
end;

function BeginTransaction(Options: TTransactionOptions = [toSuspendThread]): THandle;
var
  PStruct: PTransactionStruct;
  ThreadCount: Integer;
  P: Pointer;
  ThreadHandle: THandle;
begin
  EnterLook(FLock);
  try
    ThreadHandle := GetCurrentThread();
    PStruct := GetMemory(SizeOf(TTransactionStruct));
    FillChar(PStruct^, SizeOf(TTransactionStruct), #00);
    PStruct^.Options := Options;
    PStruct^.PID := GetCurrentProcessId();
    PStruct^.TID := GetCurrentThreadId();
    PStruct^.ThreadPriority := GetThreadPriority(ThreadHandle);
    SetThreadPriority(ThreadHandle, THREAD_PRIORITY_TIME_CRITICAL);
    Result := THandle(PStruct);
    if toSuspendThread in Options then
    begin
      ThreadCount := 0;
      EnumProcessThreads(PStruct^.PID, @CountThreadCallBack, @ThreadCount);
      if ThreadCount > 1 then
      begin
        P := GetMemory(ThreadCount * 2 * SizeOf(DWORD));
        PStruct^.SuspendedThreads := P;
        EnumProcessThreads(PStruct^.PID, @SuspendThreadCallBack, PStruct);
      end;
    end;
  finally
    LeaveLook(FLock);
  end;
end;

function EndTransaction(Handle: THandle): Boolean;
var
  PStruct: PTransactionStruct;
  i: Integer;
begin
  EnterLook(FLock);
  Result := True;
  PStruct := PTransactionStruct(Handle);
  try
    if PStruct^.SuspendedThreadCount > 0 then
    begin
      for i := 0 to PStruct^.SuspendedThreadCount - 1 do
      begin
        SuspendOrResumeThread(PStruct^.SuspendedThreads^[i], False);
      end;
      FreeMemory(PStruct^.SuspendedThreads);
    end;
    SetThreadPriority(GetCurrentThread(), PStruct^.ThreadPriority);
    FreeMemory(PTransactionStruct(Handle));
  finally
    LeaveLook(FLock);
  end;
end;

{$IFDEF SUPPORTS_GENERICS}
{ TIntercept<T,U> }

function TIntercept<T, U>.TToPointer(const A): Pointer;
begin
  Result := Pointer(A);
end;

function TIntercept<T, U>.PointerToT(const P): T;
begin
  Result := T(P);
end;

function TIntercept<T, U>.EnsureTIsMethod(): Boolean;
var
  LPInfo: PTypeInfo;
begin
  Result := SizeOf(T) = SizeOf(Pointer);
  if Result then
  begin
    LPInfo := TypeInfo(T);
    if LPInfo.Kind = tkProcedure then
      Exit
    else
      raise DetourException.Create(SErrorInvalidTType);
  end;
end;

constructor TIntercept<T, U>.Create(const TargetProc, InterceptProc: T; const AParam: U; const AInterceptOptions: TInterceptOptions = DefaultInterceptOptions);
begin
  EnsureTIsMethod();
  FCreatorThreadId := GetCurrentThreadId();
  FInterceptOptions := AInterceptOptions;
  FParam := AParam;
  FTrampolinePtr := InterceptCreate(TToPointer(TargetProc), TToPointer(InterceptProc), @FParam, AInterceptOptions);
  FNextHook := PointerToT(FTrampolinePtr);
end;

function TIntercept<T, U>.GetTrampoline(): T;
begin
  Result := FNextHook;
end;

function TIntercept<T, U>.GetParam(): U;
begin
  Result := FParam;
end;

function TIntercept<T, U>.GetCreatorThreadId(): TThreadId;
begin
  Result := FCreatorThreadId;
end;

function TIntercept<T, U>.GetInterceptOptions(): TInterceptOptions;
begin
  Result := FInterceptOptions;
end;

function TIntercept<T, U>.EnterRecursive(MaxRecursionLevel: NativeInt = 0): Boolean;
begin
  Result := EnterRecursiveSection(FTrampolinePtr, MaxRecursionLevel);
end;

function TIntercept<T, U>.ExitRecursive(): Boolean;
begin
  Result := ExitRecursiveSection(FTrampolinePtr);
end;

destructor TIntercept<T, U>.Destroy();
begin
  InterceptRemove(TToPointer(FNextHook));
  inherited;
end;

{ TIntercept<T> }
constructor TIntercept<T>.Create(const TargetProc, InterceptProc: T; const AParam: Pointer = nil;
  const AInterceptOptions: TInterceptOptions = DefaultInterceptOptions);
begin
  inherited Create(TargetProc, InterceptProc, AParam, InterceptOptions);
end;

{$ENDIF SUPPORTS_GENERICS}
{ ======================================= Initialization ======================================= }

procedure InitInternalFuncs();

  function CloneFunc(Func: PByte): PByte;
  var
    mb, ns, Sb, fn: Byte;
    P: PByte;
    Inst: TInstruction;
  begin
    Sb := 0;
    Func := GetRoot(Func);
    Result := VirtualAlloc(nil, 64, MEM_RESERVE or MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    P := Result;
    mb := JmpTypeToSize[JT_RIPZ];
    FillChar(Inst, SizeOf(TInstruction), #00);
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
    InsertJmp(P, PByte(NativeInt(Func) + Sb), JT_RIPZ);
{$ELSE !CPUX64}
    InsertJmp(P, PByte(NativeInt(Func) + Sb), JT_REL32);
{$ENDIF CPUX64}
  end;

begin
{$IFDEF HOOK_INTERNAL_FUNCTIONS}
  @InternalFuncs.VirtualAlloc := CloneFunc(@VirtualAlloc);
  @InternalFuncs.VirtualFree := CloneFunc(@VirtualFree);
  @InternalFuncs.VirtualProtect := CloneFunc(@VirtualProtect);
  @InternalFuncs.VirtualQuery := CloneFunc(@VirtualQuery);
  @InternalFuncs.FlushInstructionCache := CloneFunc(@FlushInstructionCache);
  @InternalFuncs.GetCurrentProcess := CloneFunc(@GetCurrentProcess);
{$ELSE !HOOK_INTERNAL_FUNCTIONS}
  @InternalFuncs.VirtualAlloc := @VirtualAlloc;
  @InternalFuncs.VirtualFree := @VirtualFree;
  @InternalFuncs.VirtualProtect := @VirtualProtect;
  @InternalFuncs.VirtualQuery := @VirtualQuery;
  @InternalFuncs.FlushInstructionCache := @FlushInstructionCache;
  @InternalFuncs.GetCurrentProcess := @GetCurrentProcess;
{$ENDIF HOOK_INTERNAL_FUNCTIONS}
end;

procedure FreeInternalFuncs;
begin
{$IFDEF HOOK_INTERNAL_FUNCTIONS}
  InternalFuncs.VirtualFree(@InternalFuncs.VirtualAlloc, 0, MEM_RELEASE);
  InternalFuncs.VirtualFree(@InternalFuncs.VirtualProtect, 0, MEM_RELEASE);
  InternalFuncs.VirtualFree(@InternalFuncs.VirtualQuery, 0, MEM_RELEASE);
  InternalFuncs.VirtualFree(@InternalFuncs.FlushInstructionCache, 0, MEM_RELEASE);
  InternalFuncs.VirtualFree(@InternalFuncs.GetCurrentProcess, 0, MEM_RELEASE);
  // VirtualFree must be the last one !
  InternalFuncs.VirtualFree(@InternalFuncs.VirtualFree, 0, MEM_RELEASE);
{$ENDIF HOOK_INTERNAL_FUNCTIONS}
end;

initialization

{$IFDEF SUPPORTS_MONITOR}
  FLock := TObject.Create();
{$ELSE SUPPORTS_MONITOR}
  FLock := TCriticalSection.Create();
{$ENDIF SUPPORTS_MONITOR}
GetSystemInfo(SysInfo);
SizeOfAlloc := SysInfo.dwPageSize;
if SizeOfAlloc < (TmpSize + TrampoSize + 64) then
  SizeOfAlloc := (TmpSize + TrampoSize + 64);
{$IFDEF FPC}
OpenThread := nil;
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
InitInternalFuncs();

finalization

if (FreeKernel) and (hKernel > 0) then
  FreeLibrary(hKernel);
FreeInternalFuncs();
if Assigned(FLock) then
  FreeAndNil(FLock);

end.
