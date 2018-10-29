// **************************************************************************************************
// Delphi Detours Library
// Unit DDetours
// http://code.google.com/p/delphi-detours-library/

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
// Contributor(s): David Millington
//
// The Initial Developer of the Original Code is Mahdi Safsafi [SMP3].
// Portions created by Mahdi Safsafi . are Copyright (C) 2013-2014 Mahdi Safsafi .
// All Rights Reserved.
//
// **************************************************************************************************
{
  RECENT CHANGES:

  ==>  July 25,2014 , Mahdi Safsafi :
  - Added GenericsCast unit to performe casting between generics types .

  ==>  July 24,2014 , Mahdi Safsafi :
  - Added _PointerToT,_TToPointer functions to fix bug in delphi
  versions that have bug when using 'absolute' in a generic method .
  - Added CheckTType function to check if T is procedure .
}

unit DDetours;

interface

{$I dDefs.inc}

uses
  Windows,
  InstDecode
{$IFDEF MustUseGenerics}
    ,
  SysUtils,
  Typinfo,
  GenericsCast
{$ENDIF MustUseGenerics}
    ;

{$IFNDEF BuildThreadSafe}
{$MESSAGE WARN 'BuildThreadSafe not defined , the library is not a thread safe .'}
{$MESSAGE HINT 'Define BuildThreadSafe to make the library thread safe .'}
{$ENDIF}
{$IFNDEF DEBUG}
{$WARN COMPARISON_TRUE OFF}
{$ENDIF}
// ---------------------------------------------------------------------------------
function InterceptCreate(const TargetProc, InterceptProc: Pointer): Pointer; stdcall; overload;
function InterceptCreate(const Module, Method : string; const InterceptProc: Pointer; ForceLoadModule : Boolean = False): Pointer; stdcall;  overload;

function InterceptRemove(var Trampoline: Pointer): Boolean; stdcall;
function MakeProcObj(var Proc; const Obj: Pointer): Boolean; stdcall;

{$IFDEF MustUseGenerics}

type
  DetourException = class(Exception);

const
  SInvalidTType = '%s must be procedure.';

type
  TDetour<T> = class
    function CheckTType: Boolean;
  private
    FTargetProc: Pointer;
    FInterceptProc: Pointer;
    FTrampoline: T;
    function GetInstalled: Boolean;
    function PointerToT(const P: Pointer): T;
    function TToPointer(const AT: T): Pointer;
    function GetTrampoline: T;
  public
    constructor Create(const TargetProc, InterceptProc: T);
    destructor Destroy; override;
    procedure Enable;
    procedure Disable;
    property Trampoline: T read GetTrampoline; // Call the original
    property Installed: Boolean read GetInstalled;
  end;
{$ENDIF MustUseGenerics}

implementation

{$IFDEF BuildThreadSafe}
{$IFNDEF FPC}

// Delphi
uses
  TLHelp32;
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

  TCreateToolhelp32Snapshot = function(dwFlags, th32ProcessID: DWORD)
    : THandle stdcall;
  TThread32First = function(hSnapshot: THandle; var lpte: TThreadEntry32)
    : BOOL stdcall;
  TThread32Next = function(hSnapshot: THandle; var lpte: TThreadEntry32)
    : BOOL stdcall;

var
  CreateToolhelp32Snapshot: TCreateToolhelp32Snapshot;
  Thread32First: TThread32First;
  Thread32Next: TThread32Next;

const
  TH32CS_SNAPTHREAD = $00000004;
{$ENDIF FPC}

type
  TThreadsListID = class
  private
    FCount: UINT;
    FPointer: Pointer;
    FSize: UINT;
  public
    procedure Add(const Value: DWORD);
    function GetID(const Index: UINT): DWORD;
    constructor Create; virtual;
    destructor Destroy; override;
    property Count: UINT read FCount;
    property ThreadIDs[const index: UINT]: DWORD read GetID;
  end;

type
  TOpenThread = function(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
    dwThreadId: DWORD): THandle; stdcall;

var
  OpenThread: TOpenThread;
  hKernel: THandle;
  OpenThreadExist: Boolean = False;
  FreeKernel: Boolean = False;
{$ENDIF BuildThreadSafe}

type
{$IFDEF CPUX86}
  PRelativeJump = ^TRelativeJump;

  TRelativeJump = Packed Record
    OpCode: Byte;
    Offset: Integer;
  end;
{$ELSE !CPUX86}

  PIndirectJump = ^TIndirectJump;

  TIndirectJump = Packed Record
    OpCode: Word;
    Offset: Integer;
  end;
{$ENDIF !CPUX86}
{$IFDEF CPUX86}

  TJumpInst = TRelativeJump;
{$ELSE !CPUX86}
  TJumpInst = TIndirectJump;
{$ENDIF !CPUX86}
  PJumpInst = ^TJumpInst;

  PSaveData = ^TSaveData;

  TSaveData = packed record
    D1: Pointer; // Target Proc Address .
    D2: Pointer; // InterceptProc Proc Address .
    Sb: Byte; // Stolen Bytes .
    Size32: Boolean;
  end;

const
  SizeOfJmp = SizeOf(TJumpInst);
  opNop = $90;
  opJmpIndirect = $25FF; // jmp dword ptr[<Address>]
  opJmpRelative = $E9; // jmp <Displacement>
  opInt3 = $CC; // int 3 (debug breakpoint)
{$IFDEF CPUX86}
  CPUX: TCPUX = CPUX32;
  opJmp = opJmpRelative;
{$ELSE !CPUX86}
  CPUX: TCPUX = CPUX64;
  opJmp = opJmpIndirect;
{$ENDIF !CPUX86}
  TrampolineSize = (SizeOf(Pointer) shl 3) + SizeOf(TSaveData);

{$IFDEF DEBUG}
{$DEFINE SkipInt3}
{$ENDIF}

function HiQword(const Value: UINT64): DWORD; overload;
{$IFDEF MustInline}inline; {$ENDIF}
begin
  Result := (Value shr 32);
end;

function HiQword(const Value: Int64): DWORD; overload;
{$IFDEF MustInline}inline; {$ENDIF}
begin
  Result := (Value shr 32);
end;

procedure FillNop(const Address: Pointer; const Count: Integer);
{$IFDEF MustInline}inline; {$ENDIF}
begin
  FillChar(Address^, Count, opNop);
end;

function IsOpUnCondJmpWithNoReg(const Addr: PByte): Boolean;
var
  Inst: TInstruction;
begin
  {
    Return true if instruction is Unconditional Jump and does not use Registers  .
    e.g : JMP DWORD PTR DS:[Address] . => Result = True .
  }
  Result := False;
  FillChar(Inst, SizeOf(TInstruction), #0);
  DecodeInstruction(Addr, @Inst, CPUX);
  if Inst.ModRM.Used then
  begin
    if Inst.ModRM.rMod <> 0 then
      if Inst.ModRM.rRM <> 5 then
        Exit;
    {
      e.g :
      JMP EAX  .
      JMP DWORD PTR DS:[EAX+10] .
      => Result = False .
    }
  end;
  if Inst.nOpCode = 1 then
  begin
    Result := Inst.OpCode in [$E9, $EB];
    if not Result then
      if Inst.OpCode = $FF then
        Result := (Inst.ModRM.rReg in [4, 5]);
    // rReg = extension for opCode !
  end
  else if Inst.nOpCode = 2 then
  begin
    Result := (HiByte(Inst.OpCode) = 0) and (Inst.ModRM.rReg = 6);
    // rReg = extension for opCode ! => JMPE .
  end
  else
    Result := False;
end;

{$HINTS OFF}

function IsOpRel(const Addr: Pointer): Boolean;
var
  P: PByte;
  Op: Word;
  Inst: TInstruction;
begin
  { Check if instruction use relative offset .
    eg : JE -5 .
  }
  Result := False;
  P := PByte(Addr);
  Op := PWORD(P)^;
  FillChar(Inst, SizeOf(TInstruction), #0);
  DecodeInstruction(P, @Inst, CPUX);
  Result := (Inst.Displacement.Used) and (Inst.Displacement.Relative);
  if Result then
    Exit;
  if Byte(Op) = $F then
  begin
    Result := HiByte(Op) in [$80 .. $8F]; // Jump.
  end
  else if Byte(Op) <> $F then
  begin
    Result := P^ in [$70 .. $7F, $E0 .. $E3, $E8, $E9, $EB];
  end
  else
    Result := False;
end;
{$HINTS ON}

function GetRoot(const P: Pointer; Inst: TInstruction): Pointer;
var
  Q: PByte;
begin
  Result := P;
  FillChar(Inst, SizeOf(TInstruction), Char(0));
  DecodeInstruction(P, @Inst, CPUX);
  if (Inst.JumpCall.JumpUsed) and IsOpUnCondJmpWithNoReg(P) then
  begin
    Q := Pointer(Inst.JumpCall.Address);
    Result := GetRoot(Q, Inst);
  end;
end;

function SetMemPermission(const Code: Pointer; const Size: Integer;
  const Permission: DWORD): DWORD;
begin
  Result := 0;
  if Assigned(Code) and (Size > 0) and (Permission > 0) then
    if FlushInstructionCache(GetCurrentProcess, Code, Size) then
      VirtualProtect(Code, Size, Permission, Result);
end;

procedure CorrectRelOffset(const Src, Dst: Pointer; const Inst: TInstruction);
var
  NewOffset: Integer;
  Addr: Int64;
  Q: PByte;
  OffsetAddr: Pointer;
begin
  Q := PByte(Dst);
  if (CPUX = CPUX64) and (Inst.JumpCall.Used) and
    (Inst.JumpCall.IndirectDispOnly) then
  begin
    { e.g: jmp qword ptr [rel $0000ad9c] }
    // Addr := Inst.JumpCall.Address;
    { OffsetAddr = EIP + Offset }
    OffsetAddr := Pointer(UINT64(Src) + Inst.JumpCall.Offset + Inst.InstSize);
    { Offset = OffsetAddr - EIP }
    NewOffset := Integer(UINT64(OffsetAddr) - UINT64(Q) - Inst.InstSize);
    { Set the new Offset . }
    Inc(Q, Inst.InstSize - Inst.JumpCall.OffsetSize);
    PInteger(Q)^ := NewOffset;
  end
  else if (CPUX = CPUX64) and (Inst.Displacement.Used) and
    (Inst.Displacement.Relative) then
  begin
    {
      mov rax,[rel $00000011]
      We can not copy this instruction directly .
      We need to correct the offset $00000011 .
    }
    OffsetAddr := Pointer(UINT64(Src) + Inst.Displacement.Value +
      Inst.InstSize);
    NewOffset := UINT64(OffsetAddr) - UINT64(Q) - Inst.InstSize;
    if Inst.Displacement.i32 then
    begin
      { Four Bytes Displacement }
      Inc(Q, Inst.InstSize - 4);
      PInteger(Q)^ := NewOffset;
    end
    else
    begin
      { One Byte Displacement }
      Inc(Q, Inst.InstSize - 1);
      PByte(Q)^ := Byte(NewOffset);
    end;
  end
  else if (Inst.JumpCall.Relative) and (Inst.JumpCall.Used) then
  begin
    { JMP Relative }
    Addr := Inst.JumpCall.Address;
    NewOffset := UINT64(Addr) - UINT64(Dst) - Inst.InstSize;
    Inc(Q, Inst.InstSize - Inst.JumpCall.OffsetSize);
    case Inst.JumpCall.OffsetSize of
      1:
        PShortInt(Q)^ := ShortInt(NewOffset);
      2:
        PShort(Q)^ := Short(NewOffset);
      4:
        PInteger(Q)^ := NewOffset;
    end;
  end;
end;

procedure CopyInstruction(const Src; var Dst; const Size: ShortInt);
var
  PSrc, PDst: PByte;
  S: ShortInt;
  Inst: TInstruction;
  i: Integer;
begin
  i := 0;
  PSrc := PByte(@Src);
  PDst := PByte(@Dst);
  while i < Size do
  begin
    FillChar(Inst, SizeOf(TInstruction), #0);
    S := DecodeInstruction(PSrc, @Inst, CPUX);
{$IFDEF SkipInt3}
    if PSrc^ <> opInt3 then
{$ENDIF SkipInt3}
    begin
      Move(PSrc^, PDst^, S);
      if IsOpRel(PSrc) then
        CorrectRelOffset(PSrc, PDst, Inst);
      Inc(PDst, S);
    end;
    Inc(PSrc, S);
    Inc(i, S);
  end;
end;

function AddrAllocMem(const Addr: Pointer;
  const Size, flProtect: DWORD): Pointer;
var
  mbi: TMemoryBasicInformation;
  Info: TSystemInfo;
  P, Q: UINT64;
  PP: Pointer;
begin
  { Alloc memory on the specific nearest address from the Addr . }
  Result := nil;
  if Addr = nil then
  begin
    Result := VirtualAlloc(nil, Size, MEM_COMMIT, flProtect);
    Exit;
  end;
  P := UINT64(Addr);
  Q := UINT64(Addr);
  GetSystemInfo(Info);
  { Interval = [2GB ..P.. 2GB] = 4GB }
  if Int64(P - (High(DWORD) div 2)) < 0 then
    P := 1
  else
    P := UINT64(P - (High(DWORD) div 2)); // -2GB .
  if UINT64(Q + (High(DWORD) div 2)) > High( {$IFDEF CPUX64}UINT64
{$ELSE}UINT
{$ENDIF} ) then
    Q := High( {$IFDEF CPUX64}UINT64 {$ELSE}UINT {$ENDIF} )
  else
    Q := Q + (High(DWORD) div 2); // + 2GB .

  while P < Q do
  begin
    PP := Pointer(P);
    if VirtualQuery(PP, mbi, Size) = 0 then
      Break;
    if (mbi.State and MEM_FREE = MEM_FREE) and (mbi.RegionSize > Size) then
      { Yes there is a memory that we can use ! }
      if (mbi.RegionSize >= Info.dwAllocationGranularity) then
      begin
        { The RegionSize must be greater than the dwAllocationGranularity . }
        { The address (PP) must be multiple of the allocation granularity (dwAllocationGranularity) . }
        PP := Pointer(Info.dwAllocationGranularity *
          (UINT64(PP) div Info.dwAllocationGranularity) +
          Info.dwAllocationGranularity);
        {
          If PP is multiple of dwAllocationGranularity then alloc memory .
          If PP is not multiple of dwAllocationGranularity ,the VirtualAlloc will fails .
        }
        if UINT64(PP) mod Info.dwAllocationGranularity = 0 then
          Result := VirtualAlloc(PP, Size, MEM_COMMIT or MEM_RESERVE,
            flProtect);
        if Result <> nil then
          Exit;
      end;
    P := UINT64(mbi.BaseAddress) + mbi.RegionSize; // Next region .
  end;
end;

{$IFOPT Q+}
{$DEFINE Q_On}
{$ENDIF}
{$Q-}

function DoInterceptCreate(const TargetProc, InterceptProc: Pointer): Pointer;
var
  P, Q: PByte;
  PJmp: PJumpInst;
  Sb, nb: Byte;
  OrgProcAccess: DWORD;
  Inst: TInstruction;
  PSave: PSaveData;
{$IFDEF CPUX64}
  Offset: Int64;
  J: UINT64;
{$ELSE !CPUX64}
  Offset: Integer;
{$ENDIF !CPUX64}
  Size32: Boolean;
begin
  P := PByte(TargetProc);
  { Alloc memory for the trampoline routine . }
{$IFDEF CPUX64}
  Result := AddrAllocMem(TargetProc, TrampolineSize, PAGE_EXECUTE_READWRITE);
{$ELSE !CPUX64}
  Result := VirtualAlloc(nil, TrampolineSize, MEM_COMMIT,
    PAGE_EXECUTE_READWRITE);
{$ENDIF !CPUX64}
  Q := Result;
  if not Assigned(Result) then
    Exit; // Failed !

  Sb := 0;
  Size32 := True;
  Inc(Q, SizeOf(TSaveData));
  // Reserved for the extra bytes that hold information about address .

  { Offset between the trampoline and the target proc address . }
{$IFDEF CPUX64}
  PSave := PSaveData(Q);
  Offset := Int64(UINT64(PSave) - UINT64(P) - SizeOfJmp);
  // Sign Extended ! .
  // {$ELSE}
  // Offset := Integer(UINT(PSave) - UINT(P) - SizeOfJmp); // Sign Extended ! .
{$ENDIF CPUX64}
  nb := SizeOfJmp; // Numbers of bytes needed .

{$IFDEF CPUX64}
  if Offset < 0 then
    J := UINT64(-Offset)
  else
    J := UINT64(Offset);

  if HiQword(J) <> 0 then
  begin
    { The size of offset is too big than the size of dword . }
    Size32 := False;
    Inc(nb, SizeOf(Pointer));
  end;
{$ENDIF CPUX64}
  { Calculate the Stolens instructions . }
  while Sb < nb do
  begin
    { Get information about the instruction . }
    FillChar(Inst, SizeOf(TInstruction), Char(0));
    DecodeInstruction(P, @Inst, CPUX);
{$IFDEF SkipInt3}
    if Inst.OpCode <> opInt3 then
{$ENDIF SkipInt3}
      Inc(Sb, Inst.InstSize);
    Inc(P, Inst.InstSize); // Next Instruction .
  end;
  P := PByte(TargetProc); // Restore the old value .

  { The size is not enough to insert the Jump instruction }
  if Sb < nb then
    Exit

    { Don't copy beyond the trampoline }
  else if Sb > (TrampolineSize - SizeOf(TSaveData) - SizeOfJmp) then
    Exit;

  { Allow writing to the target proc ==> So we can insert the jump instruction . }
  OrgProcAccess := SetMemPermission(P, nb, PAGE_EXECUTE_READWRITE);
  { Copy the stolens instructions from the target proc to the trampoline proc . }
  CopyInstruction(P^, Q^, Sb);

  if Sb > nb then
    FillNop(Pointer(NativeUInt(P) + nb), Sb - nb);
  // Fill the rest bytes with NOP instruction .

  if not Size32 then
  begin
    { The variable that hold the destination will be inserted
      on the target proc (after the jump instruction)
      => We are going to use JMP instuction with RIP without offset .
      => JMP [RIP] .
    }
    PSave := PSaveData(P);
    Inc(PByte(PSave), SizeOfJmp);
  end
  else
    { The variable that hold the destination will be inserted
      on the trampoline proc (before the stolens instructions)
    }
    PSave := PSaveData(Result);

  PSave^.D1 := InterceptProc;

  { Calculate the offset between the InterceptProc variable and the jmp instruction (target proc) . }
{$IFDEF CPUX64}
  Offset := Int64(UINT64(PSave) - UINT64(P) - SizeOfJmp);
  // Sign Extended ! .
{$ELSE !CPUX64}
  {$IFOPT Q+}
    {$DEFINE OVERFLOW_CHECK_ENABLED}
    {$Q-}
  {$ELSE}
    {$UNDEF OVERFLOW_CHECK_ENABLED}
  {$ENDIF}
      Offset := Integer(UINT(InterceptProc) - UINT(P) - SizeOfJmp); // Sign Extended ! .
  {$IFDEF OVERFLOW_CHECK_ENABLED}
    {$Q+}
    {$UNDEF OVERFLOW_CHECK_ENABLED}
  {$ENDIF}
  // Sign Extended ! .
{$ENDIF !CPUX64}
  { Insert JMP instruction . }
  PJmp := PJumpInst(P);
  PJmp^.OpCode := opJmp;
  PJmp^.Offset := Integer(Offset);

  { Save the InterceptProc and the TargetProc address on the trampoline routine . }
  PSave := PSaveData(Result);
  PSave^.D1 := InterceptProc;
  PSave^.D2 := Pointer(UINT64(P) + nb);
  PSave^.Sb := Sb;
  PSave^.Size32 := Size32;

  Q := Result;
  Inc(Q, Sb + SizeOf(TSaveData)); // Address of JMP instruction .

  { Calculate the offset between the TargetProc variable and the jmp instruction (Trampoline proc) . }
{$IFDEF CPUX64}
  Offset := Int64((UINT64(PSave) + SizeOf(Pointer)) - UINT64(Q) - SizeOfJmp);
  // Sign Extended ! .
{$ELSE !CPUX64}
  Offset := Integer((UINT(PSave^.D2) - UINT(Q) - SizeOfJmp));
  // Sign Extended ! .
{$ENDIF !CPUX64}
  { Insert JMP instruction . }
  PJmp := PJumpInst(Q);
  PJmp^.OpCode := opJmp;
  PJmp^.Offset := Integer(Offset);

  { Skip the Saved data on the Trampoline .
    => So it will never be executed .
  }
  Inc(PByte(Result), SizeOf(TSaveData));

  { Restore TargetProc old permission . }
  SetMemPermission(P, Sb, OrgProcAccess);
end;
{$IFDEF Q_On}
{$Q+ }
{$ENDIF Q_On}
// ------------------------------------------------------------------------------
{$IFDEF BuildThreadSafe }

const
  THREAD_SUSPEND_RESUME = $0002;

function SuspendAllThreads(RTID: TThreadsListID): Boolean;
var
  hSnap: THandle;
  PID: DWORD;
  te: TThreadEntry32;
  nCount: DWORD;
  hThread: THandle;
begin
  PID := GetCurrentProcessId;

  hSnap := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, PID);

  Result := hSnap <> INVALID_HANDLE_VALUE;

  if Result then
  begin
    te.dwSize := SizeOf(TThreadEntry32);
    if Thread32First(hSnap, te) then
    begin
      while True do
      begin
        if te.th32OwnerProcessID = PID then
          { Allow the caller thread to access the Detours .
            => Suspend all threads ,except the current thread .
          }
          if te.th32ThreadID <> GetCurrentThreadId then
          begin
            hThread := OpenThread(THREAD_SUSPEND_RESUME, False,
              te.th32ThreadID);
            if hThread <> INVALID_HANDLE_VALUE then
            begin
              nCount := SuspendThread(hThread);
              if nCount <> DWORD(-1) then // thread's previously was running  .
                RTID.Add(te.th32ThreadID);
              // Only add threads that was running before suspending them !
              CloseHandle(hThread);
            end;
          end;
        if not Thread32Next(hSnap, te) then
          Break;
      end;
    end;
    CloseHandle(hSnap);
  end;
end;

function ResumeSuspendedThreads(RTID: TThreadsListID): Boolean;
var
  i: Integer;
  TID: DWORD;
  hThread: THandle;
begin
  Result := False;
  if Assigned(RTID) then
    for i := 0 to RTID.Count do
    begin
      TID := RTID.ThreadIDs[i];
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

{$ENDIF BuildThreadSafe}

function MakeProcObj(var Proc; const Obj: Pointer): Boolean;
{$IFNDEF FPC}
asm
  {
  Default calling convention is :
  x86 : register .
  x64 : fastcall.
  ==>  Do not change ! <==
  ============================================
  In order to access TObject variables
  the compiler need to know the Object
  that hold this variable .

  When calling TObject.MethodX ,
  the compiler does not know the TObject
  So the delphi compiler will use a useful
  trick .. it will insert the pointer to
  the object on the EAX/RCX registers .

  eg:Self.DoX(A):
  MOV EAX,Self
  MOV EDX,A
  CALL DoX

  That's mean : the compiler use a specific
  calling convention to call ObjectMethod
  => EAX/RCX are always reserved and contains
  the object pointer value .

  So in order to use Trampoline function
  we need to make sure that EAX/RCX registers
  hold the Object pointer value.
  ============================================
   }

  {$IFDEF CPUX86}
  { ->EAX     Proc  . }
  { ->EDX     Obj   . }
  { <-AL     Result . }
  TEST EAX,EAX
  JE @FAIL
  TEST EDX,EDX
  JE @FAIL
  MOV [EAX+4],EDX
  {$ELSE !CPUX86}
  { ->RCX     Proc  . }
  { ->RDX     Obj   . }
  { <-AL     Result . }
  TEST RCX,RCX
  JE @FAIL
  TEST RDX,RDX
  JE @FAIL
  MOV [RCX+8],RDX
  {$ENDIF !CPUX86}
  MOV AL,1   // Result = True
  JMP @END
@FAIL:
  XOR AL,AL // Result = False
@END:
end;
{$ELSE FPC}
begin
  Result := False;
  if Assigned(Pointer(Proc)) and Assigned(Pointer(Obj)) then
  begin
    TMethod(Proc).Data := Pointer(Obj);
    TMethod(Proc).Code := Pointer(Proc);
    Result := True;
  end;
end;
{$ENDIF FPC}



function InterceptCreate(const Module, Method : string; const InterceptProc: Pointer; ForceLoadModule : Boolean = False): Pointer; stdcall;
var
 pOrgPointer : Pointer;
 LModule     : THandle;
begin
  Result:=nil;
  LModule := GetModuleHandle(PChar(Module));
  if (LModule=0) and ForceLoadModule then
    LModule:=LoadLibrary(PChar(Module));

  if LModule<>0 then
  begin
   pOrgPointer   := GetProcAddress(LModule, PChar(Method));
   if Assigned(pOrgPointer) then
    Result  := InterceptCreate(pOrgPointer, InterceptProc);
  end;
end;




function InterceptCreate(const TargetProc, InterceptProc: Pointer): Pointer;
var
  P: PByte;
  Inst: TInstruction;
{$IFDEF BuildThreadSafe }
  { List that hold the running threads
    that we had make them suspended ! }
  RTID: TThreadsListID;
{$ENDIF BuildThreadSafe}
begin
  Result := nil;
  if Assigned(TargetProc) and Assigned(InterceptProc) then
  begin
{$IFDEF BuildThreadSafe }
    RTID := nil;
    if OpenThreadExist then
    begin
      RTID := TThreadsListID.Create;
      SuspendAllThreads(RTID);
    end;
{$ENDIF BuildThreadSafe}
    FillChar(Inst, SizeOf(TInstruction), Char(0));
    P := PByte(TargetProc);
    P := GetRoot(P, Inst);
    Result := DoInterceptCreate(P, InterceptProc);
{$IFDEF BuildThreadSafe }
    if OpenThreadExist then
    begin
      ResumeSuspendedThreads(RTID);
      RTID.Free;
    end;
{$ENDIF BuildThreadSafe}
  end;
end;

function InterceptRemove(var Trampoline: Pointer): Boolean;
var
  P, Q: PByte;
  PSave: PSaveData;
  OrgProcAccess: DWORD;
  Sb: Byte;
{$IFDEF BuildThreadSafe }
  RTID: TThreadsListID;
{$ENDIF BuildThreadSafe}
begin
  Result := False;
  if Assigned(Trampoline) then
  begin
{$IFDEF BuildThreadSafe }
    RTID := nil;
    if OpenThreadExist then
    begin
      RTID := TThreadsListID.Create;
      SuspendAllThreads(RTID);
    end;
{$ENDIF BuildThreadSafe}
    Q := Trampoline;
    PSave := PSaveData(Q);
    Dec(PByte(PSave), SizeOf(TSaveData));
    P := PSave^.D2;
    Dec(P, SizeOfJmp + (Byte(Byte(not PSave^.Size32) and Byte(CPUX = CPUX64)) *
      SizeOf(Pointer)));
    Sb := PSave^.Sb;
    OrgProcAccess := SetMemPermission(P, Sb, PAGE_EXECUTE_READWRITE);
    CopyInstruction(Q^, P^, Sb);
    SetMemPermission(P, Sb, OrgProcAccess);
    Result := VirtualFree(PSave, TrampolineSize, MEM_RELEASE);
{$IFDEF BuildThreadSafe }
    if OpenThreadExist then
    begin
      ResumeSuspendedThreads(RTID);
      RTID.Free;
    end;
{$ENDIF BuildThreadSafe}
  end;
end;
{ ===================================================== }

{$IFDEF MustUseGenerics }

{ ***** BEGIN LICENSE BLOCK *****
  *
  * The initial developer of the original code (TDetour) is David Millington .
  *
  * ***** END LICENSE BLOCK ***** }
{ ----------------------------------------------------- }
{ TDetour }
{ ----------------------------------------------------- }
function TDetour<T>.CheckTType: Boolean;
var
  LPInfo: PTypeInfo;
begin
  LPInfo := TypeInfo(T);
  Result := SizeOf(T) = SizeOf(Pointer);
  if Result then
    Result := LPInfo.Kind = tkProcedure;
  if not Result then
    raise DetourException.Create(Format(SInvalidTType, [LPInfo.Name]));
end;

constructor TDetour<T>.Create(const TargetProc, InterceptProc: T);
begin
  inherited Create();
  CheckTType;
  FTargetProc := TToPointer(TargetProc);
  FInterceptProc := TToPointer(InterceptProc);
  Assert(Assigned(FTargetProc) and Assigned(FInterceptProc),
    'Target or replacement methods are not assigned');
  FTrampoline := T(nil);
  Enable;
end;

destructor TDetour<T>.Destroy;
begin
  Disable;
  inherited;
end;

procedure TDetour<T>.Enable;
begin
  if not Installed then
    FTrampoline := PointerToT(DDetours.InterceptCreate(FTargetProc,
      FInterceptProc));
end;

procedure TDetour<T>.Disable;
var
  PTrampoline: Pointer;
begin
  if Installed then
  begin
    PTrampoline := TToPointer(FTrampoline);
    DDetours.InterceptRemove(PTrampoline);
    FTrampoline := T(nil);
  end;
end;

function TDetour<T>.GetInstalled: Boolean;
begin
  Result := Assigned(TToPointer(FTrampoline));
end;

function TDetour<T>.GetTrampoline: T;
begin
  Assert(Installed, 'Detour is not installed; trampoline pointer is nil');
  Result := FTrampoline;
end;

function TDetour<T>.PointerToT(const P: Pointer): T;
{ var
  Trampoline: Pointer;
  Method: T absolute Trampoline; }
begin
  // Cannot cast from Pointer to T (even though intended use of this class is for T to be a method
  // type - there is no constraint for this though) so hack it via absolute
  // Trampoline := P;
  // Result := Method;
  Result := TGenericsCast<Pointer, T>.Cast(P);
end;

function TDetour<T>.TToPointer(const AT: T): Pointer;
{ var
  Method: T;
  Trampoline: Pointer absolute Method; }
begin
  // Cannot cast from Pointer to T (even though intended use of this class is for T to be a method
  // type - there is no constraint for this though) so hack it via absolute
  // Method := AT;
  // Result := Trampoline;
  Result := TGenericsCast<T, Pointer>.Cast(AT);
end;

{$ENDIF MustUseGenerics}
{ ===================================================== }

{$IFDEF BuildThreadSafe }
{ ----------------------------------------------------- }
{ TThreadsListID }
{ ----------------------------------------------------- }

constructor TThreadsListID.Create;
begin
  FPointer := nil;
  FSize := 0;
end;

destructor TThreadsListID.Destroy;
begin
  if Assigned(FPointer) then
    FreeMem(FPointer);
  inherited;
end;

procedure TThreadsListID.Add(const Value: DWORD);
var
  Delta: UINT;
begin
  if not Assigned(FPointer) then
  begin
    { First Item ! }
    GetMem(FPointer, SizeOf(DWORD));
    FCount := 0;
    FSize := 0;
  end
  else
    Inc(FCount); // Not first item !

  { Calculate the delta position between the memory that will
    hold the value and the pointer value (FPointer). }
  Delta := FCount shl 2; // FCount * SizeOf(DWORD);

  if Delta <> 0 then
    ReallocMem(FPointer, Delta + SizeOf(DWORD));

  Inc(PByte(FPointer), Delta);
  PDWORD(FPointer)^ := Value;

  { Always restore the previous pointer value }
  Dec(PByte(FPointer), Delta);
  Inc(FSize, SizeOf(DWORD)); // Size in bytes that is being used .
end;

function TThreadsListID.GetID(const Index: UINT): DWORD;
var
  Delta: UINT;
begin
  Result := DWORD(-1); // Default result when fail .

  Delta := Index shl 2; // Index * SizeOf(DWORD) ;

  // if (Delta >= 0) and (Delta < FSize) and (Index >= 0) and (Index <= FCount) then
  if (Delta < FSize) and (Index <= FCount) then
  begin
    { Not out of range ! }
    Inc(PByte(FPointer), Delta);
    Result := PDWORD(FPointer)^;

    { Always restore the previous pointer value }
    Dec(PByte(FPointer), Delta);
  end;
end;

initialization

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
  Pointer(OpenThread) := GetProcAddress(hKernel, 'OpenThread');
  Pointer(CreateToolhelp32Snapshot) := GetProcAddress(hKernel,
    'CreateToolhelp32Snapshot');
  Pointer(Thread32First) := GetProcAddress(hKernel, 'Thread32First');
  Pointer(Thread32Next) := GetProcAddress(hKernel, 'Thread32Next');
{$ELSE !FPC}
  @OpenThread := GetProcAddress(hKernel, 'OpenThread');
{$ENDIF !FPC}
end;
{ The OpenThread function does not exist on OS version < Win XP }
OpenThreadExist := (@OpenThread <> nil);

finalization

if (FreeKernel) and (hKernel > 0) then
  FreeLibrary(hKernel);
{$ENDIF BuildThreadSafe}

end.
