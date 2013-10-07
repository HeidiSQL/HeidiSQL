/// fast scaling memory manager for Delphi
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 0.4
unit SynScaleMM;

{
  Original code is ScaleMM - Fast scaling memory manager for Delphi
  by André Mussche - Released under Mozilla Public License 1.1
  http://code.google.com/p/scalemm

  Simple, small and compact MM, built on top of the main Memory Manager
  (FastMM4 is a good candidate, standard since Delphi 2007), architectured
  in order to scale on multi core CPU's (which is what FastMM4 is lacking).

  Usage:
   - Delphi 6 up to Delphi 2005 with FastMM4:
     Place FastMM4 as the very first unit under the "uses" clause of your
     project's .dpr file THEN add SynScaleMM to the "uses" clause
   - Delphi 6 up to Delphi 2005 with no FastMM4 or Delphi 2006 up to Delphi XE:
     Place SynScaleMM as the very first unit under the "uses" clause of your
     project's .dpr file.



   SynScaleMM - fast scaling memory manager for Delphi
  -----------------------------------------------------

  Modifications/fork to SynScaleMM by A.Bouchez - http://synopse.info:
  - Synchronized with r19 revision, from Dec 6, 2010;
  - Compiles from Delphi 6 up to Delphi XE;
  - Some pascal code converted to faster asm;
  - Some code refactoring, a lot of comments added;
  - Added medium block handling from 2048 bytes up to 16384;
  - Released under MPL 1.1/GPL 2.0/LGPL 2.1 tri-license.

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is ScaleMM - Fast scaling memory manager for Delphi.

  The Initial Developer of the Original Code is André Mussche.

  Portions created by the Initial Developer are Copyright (C) 2012
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Arnaud Bouchez http://synopse.info
  Portions created by each contributor are Copyright (C) 2012
  each contributor. All Rights Reserved.

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  Version 0.4
  - Reallocation made a lot faster, in case of a growing size by some bytes
  
}

interface

{.$DEFINE DEBUG_SCALEMM}  // slower but better debugging (no inline functions etc)

/// internal GetSmallMemManager function is 2% faster with an injected offset
{$define SCALE_INJECT_OFFSET}

// inlined TLS access
// - injected offset + GetSmallMemManager call can be slower than offset loading
{$define INLINEGOWN}
{$ifdef INLINEGOWN}
  {$ifndef HASINLINE} // inlined Getmem/Freemem will call GetSmallMemManager
    {$undef SCALE_INJECT_OFFSET}
  {$endif}
{$endif}

// enable Backing Off Locks with Spin-Wait Loops
// - see http://software.intel.com/en-us/articles/implementing-scalable-atomic-locks-for-multi-core-intel-em64t-and-ia32-architectures
{$define SPINWAITBACKOFF}

// other posible defines:
{.$define ALLOCBY64}      // allocated by 64 memory items (if undefined, by 32)
{.$define PURE_PASCAL}    // no assembly, pure delphi code
{.$define Align16Bytes}   // 16 byte aligned header, so some more overhead
{$define USEMEDIUM}      // handling of 2048..16384 bytes blocks
{.$define USEBITMAP}      // freed blocks per bit storage (experimental)
{.$define BACKOFFSLEEP1}  // could avoid race condition in some (rare) cases

{$ifdef DEBUG_SCALEMM}
  {$OPTIMIZATION   OFF}
  {$STACKFRAMES    ON}
  {$ASSERTIONS     ON}
  {$DEBUGINFO      ON}
  {$OVERFLOWCHECKS ON}
  {$RANGECHECKS    ON}
{$else}      // default "release" mode, much faster!
  {$OPTIMIZATION   ON}         // 235% faster!
  {$STACKFRAMES    OFF}        // 12% faster
  {$ASSERTIONS     OFF}
  {$OVERFLOWCHECKS OFF}
  {$RANGECHECKS    OFF}
  {$if CompilerVersion >= 17}
    {$define HASINLINE}        // Delphi 2005 or newer
  {$ifend}
  {$D-}
  {$L-}
{$endif}

{$ifdef USEBITMAP} // bitmap size must match NativeUInt bit count
  {$ifdef CPUX64}
    {$define ALLOCBY64}
  {$else}
    {$undef ALLOCBY64}
  {$endif}
{$endif}

const
  /// alloc memory blocks with 64 or 32 memory items each time
  // - 64 = 1 shl 6, 32 = 1 shl 5, therefore any multiplication compiles into
  // nice and fast shl opcode
  // - on a heavily multi-threaded application, with USEMEDIUM defined below,
  // a lower value (i.e. 32) could be used instead (maybe dedicated value for
  // medium blocks would be even better)
  // - if USEBITMAP is defined, this size will match the NativeUInt bit count
  C_ARRAYSIZE = {$ifdef ALLOCBY64}64{$else}32{$endif};
  /// keep 10 free blocks in cache
  C_GLOBAL_BLOCK_CACHE = 10;

{$if CompilerVersion < 19}
type // from Delphi 6 up to Delphi 2007
  NativeUInt = Cardinal;
  NativeInt = Integer;
{$ifend}

{$if CompilerVersion >= 17}
  {$define USEMEMMANAGEREX}
{$ifend}


const
{$ifdef USEMEDIUM}
  /// Maximum index of 2048 bytes granularity Medium blocks
  // - 63488 could have been the upper limit because 65536=63488+2048 won't fit in
  // a FItemSize: word, but it will allocate 63488*C_ARRAYSIZE=4 MB per thread!
  // - so we allocate here up to 16384 bytes, i.e. 1 MB, which sounds
  // reasonable
  // - a global VirtualAlloc() bigger block, splitted into several medium blocks,
  // via a double-linked list (see FastMM4 algorithm) could be implemented instead
  MAX_MEDIUMMEMBLOCK = 7;
  /// Maximum index of 256 bytes granularity Small blocks
  MAX_SMALLMEMBLOCK = 6;
{$else}
  /// Maximum index of 256 bytes granularity Small blocks
  // - Small blocks will include 2048 if Medium Blocks not handled
  MAX_SMALLMEMBLOCK = 7;
{$endif}

type
  PMemBlock = ^TMemBlock;
  PMemBlockList = ^TMemBlockList;
  PThreadMemManager = ^TThreadMemManager;
  PMemHeader = ^TMemHeader;

{$A-} { all object/record must be packed }
  /// Header appended to the beginning of every allocated memory block
  TMemHeader = object
    /// the memory block handler which owns this memory block
    Owner: PMemBlock;
{$ifdef USEBITMAP}
    /// the index in the array[0..C_ARRAYSIZE-1] of Owner memory items
    FIndexInMemBlockArray: NativeUInt;
{$else}
    /// linked to next single memory item (other thread freem mem)
    NextMem: PMemHeader;
{$endif}
    {$ifdef Align16Bytes}
      todo
    {$endif}
  end;

  /// memory block handler
  // - internal storage of the memory blocks will follow this structure, and
  // will contain array[0..C_ARRAYSIZE-1] of memory items,
  // i.e. (FItemSize + SizeOf(TMemHeader)) * C_ARRAYSIZE bytes
  TMemBlock = object
    /// the memory block list which owns this memory block handler
    Owner: PMemBlockList;
    /// link to the next list with free memory
    FNextMemBlock: PMemBlock;
    /// link to the previous list with free memory
    // - double linked to be able for fast removal of one block
    FPreviousMemBlock: PMemBlock;
    /// link to the next list with freed memory, in case this list has no more freed mem
    FNextFreedMemBlock: PMemBlock;
    /// link to the previous list with freed memory
    FPreviousFreedMemBlock: PMemBlock;
{$ifdef USEBITMAP}
    /// individual bit is set for any block which is to be freed from other thread
    FToBeFreedFromOtherThread: NativeUInt;
    /// link to the next TMemBlock containing blocks to be freed from other thread
    NextMem: PMemBlock;
    /// individual bit is set for any available block  in [0..C_ARRAYSIZE-1]
    FAvailable: NativeUInt;
{$else}
    /// how much free mem is used, max is C_ARRAYSIZE
    FUsageCount: NativeUInt;
    /// current index in FFreedArray
    FFreedIndex: NativeUInt;
    /// points to all freed PMemHeader
    FFreedArray: array[0..C_ARRAYSIZE-1] of Pointer;
{$endif}
    function GetUsedMemoryItem: PMemHeader; {$ifdef HASINLINE}inline;{$endif}
    procedure FreeMem(aMemoryItem: PMemHeader); {$ifdef HASINLINE}inline;{$endif}
    procedure FreeBlockMemoryToGlobal;
  end;

  /// memory block list
  // - current size if 16 bytes (this is a packed object)
  TMemBlockList = object
    /// the per-thread memory manager which created this block
    Owner: PThreadMemManager;
    /// list containing freed memory (which this block owns)
    // - used to implement a fast caching of memory blocks
    FFirstFreedMemBlock: PMemBlock;
    /// list containing all memory this block owns
    FFirstMemBlock: PMemBlock;
    /// size of memory items (32, 64 etc bytes)
    FItemSize : word;
    /// number of blocks inside FFirstFreedMemBlock
    FFreeMemCount: byte;
    /// recursive check when we alloc memory for this blocksize (new memory list)
    FRecursive: boolean;

    {$ifdef CPUX64}
    // for faster "array[0..7] of TMemBlockList" calc
    // (for 32 bits, the TMemBlockList instance size if 16 bytes)
    FFiller: array[1..sizeof(NativeInt)-sizeof(word)-sizeof(byte)-sizeof(boolean)] of byte;
    {$endif}

    procedure AddNewMemoryBlock;
    function GetMemFromNewBlock : Pointer;
  end;

  POtherThreadFreedMemory = {$ifdef USEBITMAP}PMemBlock{$else}PMemHeader{$endif};
  
  /// handles per-thread memory managment
  TThreadMemManager = object
  private
    /// link to the list of mem freed in other thread
    FOtherThreadFreedMemory: POtherThreadFreedMemory;
    /// array with memory per block size of 32 bytes (mini blocks)
    // - i.e. 32, 64, 96, 128, 160, 192, 224 bytes
    FMiniMemoryBlocks: array[0..6] of TMemBlockList;
    /// array with memory per block size of 256 bytes (small blocks)
    // - i.e. 256,512,768,1024,1280,1536,1792[,2048] bytes
    FSmallMemoryBlocks: array[0..MAX_SMALLMEMBLOCK] of TMemBlockList;
{$ifdef USEMEDIUM}
    /// array with memory per block size of 2048 bytes (medium blocks)
    // - i.e. 2048,4096,6144,8192,10240,12288,14336,16384 bytes
    FMediumMemoryBlocks: array[0..MAX_MEDIUMMEMBLOCK] of TMemBlockList;
{$endif}

    // link to list of items to reuse after thread terminated
    FNextThreadManager: PThreadMemManager;

    procedure ProcessFreedMemFromOtherThreads;
    procedure AddFreedMemFromOtherThread(aMemory: PMemHeader);
  public
    FThreadId: LongWord;
    /// is this thread memory available to new thread?
    FThreadTerminated: Boolean;

    procedure Init;
    procedure Reset;

    function GetMem(aSize: NativeUInt): Pointer; {$ifdef HASINLINE}inline;{$endif}
    function FreeMem(aMemory: Pointer): NativeInt; {$ifdef HASINLINE}inline;{$endif}
  end;

  /// Global memory manager
  // - a single instance is created for the whole process
  // - caches some memory (blocks + threadmem) for fast reuse
  // - also keeps allocated memory in case an old thread allocated some memory
  // for another thread
  TGlobalMemManager = object
  private
    /// all thread memory managers
    FFirstThreadMemory: PThreadMemManager;
    /// freed/used thread memory managers
    // - used to cache the per-thread managers in case of multiple threads creation 
    FFirstFreedThreadMemory: PThreadMemManager;
    /// main thread manager (owner of all global mem)
    FMainThreadMemory: PThreadMemManager;

    /// Freed/used memory: array with memory per 32 bytes block size
    // - i.e. 32, 64, 96, 128, 160, 192, 224 bytes
    FFreedMiniMemoryBlocks  : array[0..6] of TMemBlockList;
    /// Freed/used memory: array with memory per 256 bytes block size
    // - i.e. 256,512,768,1024,1280,1536,1792[,2048] bytes
    FFreedSmallMemoryBlocks : array[0..MAX_SMALLMEMBLOCK] of TMemBlockList;
{$ifdef USEMEDIUM}
    /// Freed/used memory: array with memory per block size of 2048 bytes
    // - i.e. 2048,4096,6144,8192,10240,12288,14336,16384 bytes
    FFreedMediumMemoryBlocks: array[0..MAX_MEDIUMMEMBLOCK] of TMemBlockList;
{$endif}

    procedure Init;
    procedure FreeBlocksFromThreadMemory(aThreadMem: PThreadMemManager);
  public
    procedure AddNewThreadManagerToList(aThreadMem: PThreadMemManager);
    procedure FreeThreadManager(aThreadMem: PThreadMemManager);
    function  GetNewThreadManager: PThreadMemManager;
    procedure FreeAllMemory;

    procedure FreeBlockMemory(aBlockMem: PMemBlock);
    function  GetBlockMemory(aItemSize: NativeUInt): PMemBlock;
  end;
{$A+}

function Scale_GetMem(aSize: Integer): Pointer;
function Scale_AllocMem(aSize: Cardinal): Pointer;
function Scale_FreeMem(aMemory: Pointer): Integer;
function Scale_ReallocMem(aMemory: Pointer; aSize: Integer): Pointer;

var
  GlobalManager: TGlobalMemManager;

/// Points to the Memory Manager on which ScaleMM is based
// - ScaleMM works on top of a main MM, which is FastMM4 since Delphi 2007
// - ScaleMM will handle blocks up to 2048 bytes (or 16384 is medium blocks
// are enabled)
// - but larger blocks are delegated to OldMM
// - you can explicitely use OldMM on purpose (but it doesn't seem to be a good idea)
// - note that also "root" block memory is allocated by OldMM if ScaleMM needs
// memory itself (to populate its internal buffers): there is not direct call
// to the VirtualAlloc() API, for instance
var
{$ifdef USEMEMMANAGEREX}
  OldMM: TMemoryManagerEx;
{$else}
  OldMM: TMemoryManager;
{$endif}


implementation

// Windows.pas unit dependency should be not used -> code inlined here

type
  DWORD = LongWord;
  BOOL  = LongBool;

const
  PAGE_EXECUTE_READWRITE = $40;
  kernel32  = 'kernel32.dll';

function  TlsAlloc: DWORD; stdcall; external kernel32 name 'TlsAlloc';
function  TlsGetValue(dwTlsIndex: DWORD): Pointer; stdcall; external kernel32 name 'TlsGetValue';
function  TlsSetValue(dwTlsIndex: DWORD; lpTlsValue: Pointer): BOOL; stdcall; external kernel32 name 'TlsSetValue';
function  TlsFree(dwTlsIndex: DWORD): BOOL; stdcall; external kernel32 name 'TlsFree';
procedure Sleep(dwMilliseconds: DWORD); stdcall; external kernel32 name 'Sleep';
{$ifdef SPINWAITBACKOFF}
function  SwitchToThread: BOOL; stdcall; external kernel32 name 'SwitchToThread';
{$else}
  {$undef BACKOFFSLEEP1} // this additional Sleep(1) is for spin wait backoff 
{$endif}
function  FlushInstructionCache(hProcess: THandle; const lpBaseAddress: Pointer; dwSize: DWORD): BOOL; stdcall; external kernel32 name 'FlushInstructionCache';
function  GetCurrentProcess: THandle; stdcall; external kernel32 name 'GetCurrentProcess';
function  GetCurrentThreadId: DWORD; stdcall; external kernel32 name 'GetCurrentThreadId';
function  Scale_VirtualProtect(lpAddress: Pointer; dwSize, flNewProtect: DWORD;
            var OldProtect: DWORD): BOOL; stdcall; overload; external kernel32 name 'VirtualProtect';
procedure ExitThread(dwExitCode: DWORD); stdcall; external kernel32 name 'ExitThread';


function SetPermission(Code: Pointer; Size, Permission: Cardinal): Cardinal;
begin
  Assert(Assigned(Code) and (Size > 0));
  { Flush the instruction cache so changes to the code page are effective immediately }
  if Permission <> 0 then
    if FlushInstructionCache(GetCurrentProcess, Code, Size) then
      Scale_VirtualProtect(Code, Size, Permission, Longword(Result));
end;

function CreateSmallMemManager: PThreadMemManager; forward;


{$ifdef PURE_PASCAL}

threadvar
  GCurrentThreadManager: PThreadMemManager;

function GetSmallMemManager: PThreadMemManager; {$ifdef HASINLINE}inline;{$endif}
begin
  Result := GCurrentThreadManager;
  if Result = nil then
    Result := CreateSmallMemManager;
end;

{$else}
var
  GOwnTlsIndex,
  GOwnTlsOffset: NativeUInt;

function GetSmallMemManager: PThreadMemManager;
asm
{$ifdef SCALE_INJECT_OFFSET}
  mov eax,123456789        // dummy value: calc once and inject at runtime
{$else}
  mov eax,GOwnTlsOffset    // 2% slower, so we default use injected offset
{$endif}
  mov ecx,fs:[$00000018]
  mov eax,[ecx+eax]      // fixed offset, calculated only once
  or eax,eax
  jz CreateSmallMemManager
end;

procedure _FixedOffset;
{$ifdef SCALE_INJECT_OFFSET}
var p: PAnsiChar;
{$endif}
begin
  GOwnTlsOffset := GOwnTlsIndex * 4 + $0e10;
  {$ifdef SCALE_INJECT_OFFSET}
  p  := @GetSmallMemManager;
  SetPermission(p, 5, PAGE_EXECUTE_READWRITE);
  PCardinal(p+1)^ := GOwnTlsOffset;  // write fixed offset 
  {$endif}
end;

{$endif PURE_PASCAL}

function CreateSmallMemManager: PThreadMemManager;
begin
  Result := GlobalManager.GetNewThreadManager;
  if Result = nil then
  begin
    Result := OldMM.GetMem( SizeOf(TThreadMemManager) );
    Result.Init;
  end
  else
  begin
    Result.FThreadId := GetCurrentThreadId;
    Result.FThreadTerminated := False;
  end;
  {$ifdef PURE_PASCAL}
  GCurrentThreadManager := Result;
  {$else}
  TlsSetValue(GOwnTLSIndex, Result);
  {$endif}
end;

// compare oldvalue with destination: if equal then newvalue is set
function CAS0(const oldValue: pointer; newValue: pointer; var destination): boolean;
// - if failed, try to Switch to next OS thread, or Sleep 0 ms if it no next thread 
asm // eax=oldValue, edx=newValue, ecx=Destination
  lock cmpxchg dword ptr [Destination],newValue
  // will compile as "lock cmpxchg dword ptr [ecx],edx" under Win32 e.g.
  setz al
{$ifdef SPINWAITBACKOFF}
  jz @ok
  call SwitchToThread
  test oldValue,oldValue // oldValue=eax under Win32 e.g.
  jnz @ok
  push 0
  call Sleep
  xor oldValue,oldValue // return false
{$else}
  jz @ok
  pause // let the CPU know this thread is in a Spin Wait loop
{$endif}
@ok:
end;

{$ifdef BACKOFFSLEEP1}
function CAS1(const oldValue: pointer; newValue: pointer; var destination): boolean;
// - if failed, try to Switch to next OS thread, or Sleep 1 ms if it no next thread
// (this 1 ms sleep is necessary to avoid race condition - see
//  http://synopse.info/forum/viewtopic.php?pid=914#p914 )
asm // eax=oldValue, edx=newValue, ecx=Destination
  lock cmpxchg dword ptr [Destination],newValue
  // will compile as "lock cmpxchg dword ptr [ecx],edx" under Win32 e.g.
  setz al
  jz @ok
  call SwitchToThread
  test oldValue,oldValue
  jnz @ok
  push 1
  call Sleep
  xor oldValue,oldValue
@ok:
end;
{$endif}

procedure InterlockedIncrement(var Value: Byte);
asm
  lock inc byte [Value]  // will compile as "lock inc byte [eax]" under Win32 e.g.
end;

procedure InterlockedDecrement(var Value: Byte);
asm
  lock dec byte [Value]  // will compile as "lock dec byte [eax]" under Win32 e.g.
end;

/// gets the first set bit and resets it, returning the bit index
function FindFirstSetBit(Value: NativeUInt): NativeUInt;
asm
  bsf Value,Value // will compile as "bsf eax,eax" under Win32 e.g.
end;

/// sets a specified bit
function SetBit(var Value: NativeUInt; BitIndex: NativeUInt): NativeUInt;
asm
  bts [Value],BitIndex // will compile as "bts [eax],edx" under Win32 e.g.
end;

{$ifdef DEBUG_SCALEMM}
procedure Assert(aCondition: boolean);
begin
  if not aCondition then
  begin
    asm
      int 3;
    end;
    Sleep(0);  // no exception, just dummy for breakpoint
  end;
end;
{$endif}

function GetOldMem(aSize: NativeUInt): Pointer; {$ifdef HASINLINE}inline;{$endif}
begin
  Result := OldMM.GetMem(aSize + SizeOf(TMemHeader));
  if Result<>nil then begin
    PMemHeader(Result)^.Owner := nil;  // not our memlist, so mark as such
    Result := Pointer(NativeUInt(Result) + SizeOf(TMemHeader) );
  end;
end;


{ TThreadMemManager }

procedure TThreadMemManager.Init;
var i, j: NativeUInt;
begin
  fillchar(self,sizeof(self),0);
  FThreadId := GetCurrentThreadId;
  j := 32; 
  for i := Low(FMiniMemoryBlocks) to High(FMiniMemoryBlocks) do
  begin // 32, 64, 96, 128, 160, 192, 224 bytes
    FMiniMemoryBlocks[i].Owner := @Self;
    FMiniMemoryBlocks[i].FItemSize := j;
    inc(j,32);
  end;
  assert(j=256);
  for i := Low(FSmallMemoryBlocks) to High(FSmallMemoryBlocks) do
  begin // 256,512,768,1024,1280,1536,1792 bytes
    FSmallMemoryBlocks[i].Owner := @Self;
    FSmallMemoryBlocks[i].FItemSize := j;
    inc(j,256);
  end;
{$ifdef USEMEDIUM}
  assert(j=2048);
  for i := Low(FMediumMemoryBlocks) to High(FMediumMemoryBlocks) do
  begin // 2048, 4096...16384 bytes
    FMediumMemoryBlocks[i].Owner := @Self;
    FMediumMemoryBlocks[i].FItemSize := j;
    inc(j,2048);
  end;
  assert(j=(MAX_MEDIUMMEMBLOCK+2)*2048);
{$else}
  assert(j=2304);
{$endif}
end;

procedure TThreadMemManager.ProcessFreedMemFromOtherThreads;
var
  pcurrentmem, ptempmem: POtherThreadFreedMemory;
begin
  // reset first item (to get all mem in linked list)
  repeat
    pcurrentmem := FOtherThreadFreedMemory;
    if CAS0(pcurrentmem, nil, FOtherThreadFreedMemory) then
      break;
{$ifdef BACKOFFSLEEP1}
    pcurrentmem := FOtherThreadFreedMemory;
    if CAS1(pcurrentmem, nil, FOtherThreadFreedMemory) then
      break;
{$endif}
  until false;
  // free all mem in linked list
  while pcurrentmem <> nil do
  begin
    ptempmem    := pcurrentmem;
    pcurrentmem := pcurrentmem.NextMem;
{$ifdef USEBITMAP}
    with ptempmem^ do
    while FToBeFreedFromOtherThread<>0 do
      FreeMem(Pointer( NativeUInt(ptempmem) + sizeof(ptempmem^) +
        FindFirstSetBit(FToBeFreedFromOtherThread) * (Owner^.FItemSize + SizeOf(TMemHeader)) ));
{$else}
    ptempmem.Owner.FreeMem(ptempmem);
{$endif}
  end;
end;    

procedure TThreadMemManager.Reset;
var
  i: NativeUInt;

  procedure __ResetBlocklist(aBlocklist: PMemBlockList);
  begin
    aBlocklist.FFirstFreedMemBlock := nil;
    aBlocklist.FFirstMemBlock := nil;
    aBlocklist.FRecursive := False;
  end;

begin
  FThreadId := 0;
  FThreadTerminated := True;
  FOtherThreadFreedMemory := nil;
  FNextThreadManager := nil;
  for i := Low(FMiniMemoryBlocks) to High(FMiniMemoryBlocks) do
    __ResetBlocklist(@FMiniMemoryBlocks[i]);
  for i := Low(FSmallMemoryBlocks) to High(FSmallMemoryBlocks) do
    __ResetBlocklist(@FSmallMemoryBlocks[i]);
{$ifdef USEMEDIUM}
  for i := Low(FMediumMemoryBlocks) to High(FMediumMemoryBlocks) do
    __ResetBlocklist(@FMediumMemoryBlocks[i]);
{$endif}
end;

procedure TThreadMemManager.AddFreedMemFromOtherThread(aMemory: PMemHeader);
var
  poldmem, currentmem: POtherThreadFreedMemory;
begin
{$ifdef USEBITMAP}
  currentmem := aMemory^.Owner;
  SetBit(currentmem^.FToBeFreedFromOtherThread,aMemory^.FIndexInMemBlockArray);
{$else}
  currentmem := aMemory;
{$endif}
  repeat
    poldmem := FOtherThreadFreedMemory;
    currentmem.NextMem  := poldmem;  // link to current next BEFORE the swap!
    // set new item as first (to created linked list)
    if CAS0(poldmem, currentmem, FOtherThreadFreedMemory) then
      break;
{$ifdef BACKOFFSLEEP1}
    poldmem := FOtherThreadFreedMemory;
    currentmem.NextMem  := poldmem;  
    if CAS1(poldmem, currentmem, FOtherThreadFreedMemory) then
      break;
{$endif}
  until false;               
end;

function TThreadMemManager.FreeMem(aMemory: Pointer): NativeInt;
var
  pm: PMemBlock;
  p: Pointer;
begin
  p  := Pointer(NativeUInt(aMemory) - SizeOf(TMemHeader));
  pm := PMemHeader(p).Owner;

  if FOtherThreadFreedMemory <> nil then
    ProcessFreedMemFromOtherThreads;

  if pm <> nil then
  with pm^ do
  begin
    // block obtained via Scale_GetMem()
    Assert(Owner <> nil);
    Assert(Owner.Owner <> nil);
    if Owner.Owner = @Self then
      // mem of own thread
      FreeMem(PMemHeader(p)) else
      // put mem in lockfree queue of owner thread
      Owner.Owner.AddFreedMemFromOtherThread(PMemHeader(p));
    Result := 0;
  end
  else
    Result := OldMM.FreeMem(p);
end;

function TThreadMemManager.GetMem(aSize: NativeUInt): Pointer;
var
  bm: PMemBlockList;
begin
  if aSize <= (length(FMiniMemoryBlocks)*32) then
    if aSize > 0 then
      // blocks of 32: 32, 64, 96, 128, 160, 192, 224
      bm := @FMiniMemoryBlocks[(aSize-1) shr 5] else
    begin
      Result := nil;
      Exit;
    end 
  else if aSize <= (length(FSmallMemoryBlocks)*256) then
    // blocks of 256: 256,512,768,1024,1280,1536,1792 bytes
    bm := @FSmallMemoryBlocks[(aSize-1) shr 8]
{$ifdef USEMEDIUM}
  else if aSize <= (length(FMediumMemoryBlocks)*2048) then
    // blocks of 2048: 2048, 4096... bytes
    bm := @FMediumMemoryBlocks[(aSize-1) shr 11]     
{$endif}
  else
  begin
    // larger blocks are allocated via the old Memory Manager
    Result := GetOldMem(aSize);
    Exit;
  end;

  if FOtherThreadFreedMemory <> nil then
    ProcessFreedMemFromOtherThreads;

  with bm^ do
  begin
{$ifndef USEBITMAP}
    if FFirstFreedMemBlock <> nil then
      // first get from freed mem (fastest because most chance?)
      Result := FFirstFreedMemBlock.GetUsedMemoryItem else
{$endif}
      // from normal list
      Result := GetMemFromNewBlock;
  end;

  Assert(NativeUInt(Result) > $10000);
  Result  := Pointer(NativeUInt(Result) + SizeOf(TMemHeader));
end;


{ TMemBlock }

procedure TMemBlock.FreeBlockMemoryToGlobal;
begin
  if Owner.FFirstMemBlock = @Self then
    Exit; //keep one block

  // remove ourselves from linked list
  if FPreviousMemBlock <> nil then
    FPreviousMemBlock.FNextMemBlock := Self.FNextMemBlock;
  if FPreviousFreedMemBlock <> nil then
    FPreviousFreedMemBlock.FNextFreedMemBlock := Self.FNextFreedMemBlock;
  if FNextMemBlock <> nil then
    FNextMemBlock.FPreviousMemBlock := Self.FPreviousMemBlock;
  if FNextFreedMemBlock <> nil then
    FNextFreedMemBlock.FPreviousFreedMemBlock := Self.FPreviousFreedMemBlock;

  if Owner.FFirstFreedMemBlock = @Self then
    Owner.FFirstFreedMemBlock := nil;
  if Owner.FFirstMemBlock = @Self then
    Owner.FFirstMemBlock := nil;

  GlobalManager.FreeBlockMemory(@Self);
end;

procedure TMemBlock.FreeMem(aMemoryItem: PMemHeader);
begin
  // first free item of block?
  // then we add this block to (linked) list with available mem
{$ifdef USEBITMAP}
  if FAvailable=NativeUInt(-1) then
{$else}
  if FFreedIndex = 0 then
{$endif}
    with Owner^ do  //faster
    begin
      {Self.}FNextFreedMemBlock := {Owner}FFirstFreedMemBlock;   //link to first list
      {Self.}FPreviousFreedMemBlock := nil;
      if {Self}FNextFreedMemBlock <> nil then
        {Self}FNextFreedMemBlock.FPreviousFreedMemBlock := @Self; //back link
      {Owner}FFirstFreedMemBlock := @Self; //replace first list
    end;
{$ifdef USEBITMAP}
  SetBit(FAvailable,aMemoryItem^.FIndexInMemBlockArray);
  if FAvailable=NativeUInt(-1) then
{$else}
  // free mem block
  FFreedArray[FFreedIndex] := aMemoryItem;
  inc(FFreedIndex);
  if FFreedIndex = C_ARRAYSIZE then
{$endif}
    // all memory available
    with Owner^ do
      if (FFreeMemCount >= C_GLOBAL_BLOCK_CACHE) and
         ({Owner.}FFirstMemBlock <> @Self) then // keep one block
        Self.FreeBlockMemoryToGlobal else
        inc(FFreeMemCount);
end;

function TMemBlock.GetUsedMemoryItem: PMemHeader;
begin
  Assert(Self.Owner <> nil);
{$ifdef USEBITMAP}
  Assert(FAvailable<>0);
  Result := Pointer( NativeUInt(@Self)+ sizeof(Self) +
    FindFirstSetBit(FAvailable) * (Owner.FItemSize + SizeOf(TMemHeader)) );
  if FAvailable=0 then
{$else}
  Assert(FFreedIndex > 0);
  dec(FFreedIndex);
  Result := FFreedArray[FFreedIndex];
  if FFreedIndex = 0 then
{$endif}
  begin // no free items left:
    // set next free memlist
    Owner.FFirstFreedMemBlock := FNextFreedMemBlock;
    // first one has no previous
    if FNextFreedMemBlock <> nil then
      FNextFreedMemBlock.FPreviousFreedMemBlock := nil;
    // remove from free list
    FPreviousFreedMemBlock := nil;
    FNextFreedMemBlock     := nil;
  end
  else
{$ifdef USEBITMAP}
  if FAvailable=NativeUInt(-1) then
{$else}
  if FFreedIndex = C_ARRAYSIZE-1 then
{$endif}
     // all memory is now available
     dec(Owner.FFreeMemCount);
end;


{ TMemBlockList }

procedure TMemBlockList.AddNewMemoryBlock;
var
  pm: PMemBlock;
begin
  FRecursive := True;
  // get block from cache
  pm := GlobalManager.GetBlockMemory(FItemSize);
  if pm = nil then
  begin
    // create own one
    pm :=
    {$ifdef USEMEDIUM}
      Owner.GetMem {$else}
      GetOldMem // (32+8)*64=2560 > 2048 -> use OldMM
    {$endif}
      ( SizeOf(pm^) + (FItemSize + SizeOf(TMemHeader)) * C_ARRAYSIZE );
    with pm^ do begin // put zero only to needed properties
{$ifdef USEBITMAP}
      fillchar(FNextFreedMemBlock,SizeOf(FNextFreedMemBlock)+
        SizeOf(FPreviousFreedMemBlock)+
        SizeOf(FToBeFreedFromOtherThread)+SizeOf(NextMem),0);
      FAvailable := NativeUInt(-1); // set all bits = mark all available
{$else}
      fillchar(FNextFreedMemBlock,SizeOf(FNextFreedMemBlock)+
        SizeOf(FPreviousFreedMemBlock)+SizeOf(FUsageCount)+SizeOf(FFreedIndex),0);
{$endif}
    end;
  end;
  // init
  with pm^ do
  begin
    {pm.}Owner := @Self;
    // set new memlist as first, add link to current item
    {pm.}FNextMemBlock := {self.}FFirstMemBlock;
    // back link to new first item
    if {self.}FFirstMemBlock <> nil then
      {self.}FFirstMemBlock.FPreviousMemBlock := pm;
    {self.}FFirstMemBlock := pm;
    {pm.}FPreviousMemBlock := nil;
{$ifdef USEBITMAP}
    if FAvailable<>NativeUInt(-1) then
{$else}
    if {pm.}FFreedIndex > 0 then
{$endif}
    begin
      // if block has already some freed memory (previous used block from cache)
      // then add to used list
      {pm.}FNextFreedMemBlock := {Self}FFirstFreedMemBlock;  // link to first list
      {pm.}FPreviousFreedMemBlock := nil;
      if {pm.}FNextFreedMemBlock <> nil then
        {pm.}FNextFreedMemBlock.FPreviousFreedMemBlock := pm; // back link
      {Self.}FFirstFreedMemBlock := pm;                     // replace first list
{$ifndef USEBITMAP}
      if {pm.}FFreedIndex = C_ARRAYSIZE then
        inc({pm.}Owner.FFreeMemCount);
{$endif}
    end;
  end;
  FRecursive := False;
end;           

function TMemBlockList.GetMemFromNewBlock: Pointer;
var
  pm: PMemBlock;
begin
  // store: first time init?
  if FFirstMemBlock = nil then
  begin
    if FRecursive then
    begin
      Result := GetOldMem(Self.FItemSize);
      Exit;
    end;
    AddNewMemoryBlock;
  end;

  pm := FFirstMemBlock;
  with pm^ do
{$ifdef USEBITMAP}
    if FAvailable=0 then
{$else}
    if FUsageCount >= C_ARRAYSIZE then
{$endif}
    begin
      // memlist full? make new memlist
      if FRecursive then
      begin
        Result := GetOldMem(Self.FItemSize);
        Exit;
      end;
      AddNewMemoryBlock;
      pm := FFirstMemBlock;
    end;

  // get mem from list
  with pm^ do
    // space left?
{$ifndef USEBITMAP}
    if FUsageCount < C_ARRAYSIZE then
    begin
      // calc next item
      Result := Pointer( NativeUInt(pm) + sizeof(pm^) +
        FUsageCount * (FItemSize + SizeOf(TMemHeader)) );
      inc(FUsageCount);
      // startheader = link to memlist
      TMemHeader(Result^).Owner := pm;
    end
    else
{$endif}
      Result := GetUsedMemoryItem;

  Assert(NativeUInt(Result) > $10000);
end;


{ TGlobalManager }

procedure TGlobalMemManager.AddNewThreadManagerToList(aThreadMem: PThreadMemManager);
var
  pprevthreadmem: PThreadMemManager;
begin
  repeat
    pprevthreadmem := FFirstThreadMemory;
    // try to set "result" in global var
    if CAS0(pprevthreadmem, aThreadMem, FFirstThreadMemory) then
      break;
{$ifdef BACKOFFSLEEP1}
    pprevthreadmem := FFirstThreadMemory;
    if CAS1(pprevthreadmem, aThreadMem, FFirstThreadMemory) then
      break;
{$endif}
  until false;
  // make linked list: new one is first item (global var), next item is previous item
  aThreadMem.FNextThreadManager := pprevthreadmem;
end;

procedure TGlobalMemManager.FreeAllMemory;

  procedure __ProcessBlockMem(aOldBlock: PMemBlockList);
  var
    allmem, oldmem: PMemBlock;
  begin
    if aOldBlock = nil then
      Exit;
    allmem := aOldBlock.FFirstFreedMemBlock;
    while allmem <> nil do
    begin
      // not in use
{$ifdef USEBITMAP}
{$else}
      if allmem.FUsageCount = allmem.FFreedIndex then
      begin
        oldmem := allmem;
        allmem := allmem.FNextFreedMemBlock;
        FMainThreadMemory.FreeMem(oldmem);
      end
      else
        allmem := allmem.FNextFreedMemBlock;
{$endif}
    end;
  end;

var
  oldthreadmem, tempthreadmem: PThreadMemManager;
  i: NativeUInt;
begin
  // free internal blocks
  for i := Low(Self.FFreedMiniMemoryBlocks) to High(Self.FFreedMiniMemoryBlocks) do
    __ProcessBlockMem(@Self.FFreedMiniMemoryBlocks[i]);
  for i := Low(Self.FFreedSmallMemoryBlocks) to High(Self.FFreedSmallMemoryBlocks) do
    __ProcessBlockMem(@Self.FFreedSmallMemoryBlocks[i]);
{$ifdef USEMEDIUM}
  for i := Low(Self.FFreedMediumMemoryBlocks) to High(Self.FFreedMediumMemoryBlocks) do
    __ProcessBlockMem(@Self.FFreedMediumMemoryBlocks[i]);
{$endif}

  // free current thread
  tempthreadmem := GetSmallMemManager;
  for i := Low(tempthreadmem.FMiniMemoryBlocks) to High(tempthreadmem.FMiniMemoryBlocks) do
    __ProcessBlockMem(@tempthreadmem.FMiniMemoryBlocks[i]);
  for i := Low(tempthreadmem.FSmallMemoryBlocks) to High(tempthreadmem.FSmallMemoryBlocks) do
    __ProcessBlockMem(@tempthreadmem.FSmallMemoryBlocks[i]);
{$ifdef USEMEDIUM}
  for i := Low(tempthreadmem.FMediumMemoryBlocks) to High(tempthreadmem.FMediumMemoryBlocks) do
    __ProcessBlockMem(@tempthreadmem.FMediumMemoryBlocks[i]);
{$endif}

  // free cached threads
  oldthreadmem := Self.FFirstFreedThreadMemory;
  while oldthreadmem <> nil do
  begin
    tempthreadmem := oldthreadmem;
    oldthreadmem  := oldthreadmem.FNextThreadManager;
    OldMM.FreeMem(tempthreadmem);
  end;
end;

procedure TGlobalMemManager.FreeBlockMemory(aBlockMem: PMemBlock);
var bl: PMemBlockList;
    prevmem: PMemBlock;
begin
{$ifndef USEBITMAP}
  Assert( aBlockMem.FFreedIndex = aBlockMem.FUsageCount );
{$endif}
  with aBlockMem.Owner^ do
    if FItemSize <= (length(Self.FFreedMiniMemoryBlocks)*32) then
      // blocks of 32: 32, 64, 96, 128, 160, 192, 224
      bl := @Self.FFreedMiniMemoryBlocks[(FItemSize-1) shr 5]
    else if FItemSize <= (length(Self.FFreedSmallMemoryBlocks)*256) then
      // blocks of 256: 256,512,768,1024,1280,1536,1792[,2048] bytes
      bl := @Self.FFreedSmallMemoryBlocks[(FItemSize-1) shr 8]
{$ifdef USEMEDIUM}
    else if FItemSize <= (length(Self.FFreedMediumMemoryBlocks)*2048) then
      // blocks of 2048: 2048,4096,6144,8192,10240,12288,14336,16384 bytes
      bl := @Self.FFreedMediumMemoryBlocks[(FItemSize-1) shr 11]
{$endif}
    else begin
      // large block
      FMainThreadMemory.FreeMem(aBlockMem);
      Exit;
    end;

  // too much cached?
  if bl.FFreeMemCount > C_GLOBAL_BLOCK_CACHE then
  begin
    // dispose
    FMainThreadMemory.FreeMem(aBlockMem);
    Exit;
  end;

  // add freemem block to front (replace first item, link previous to first items)
  repeat
    prevmem := bl.FFirstFreedMemBlock;
    aBlockMem.FNextFreedMemBlock := prevmem;
    if CAS0(prevmem, aBlockMem, bl.FFirstFreedMemBlock) then
      break;
{$ifdef BACKOFFSLEEP1}
    prevmem := bl.FFirstFreedMemBlock;
    aBlockMem.FNextFreedMemBlock := prevmem;
    if CAS1(prevmem, aBlockMem, bl.FFirstFreedMemBlock) then
      break;
{$endif}
  until False;

  // inc items cached
  InterlockedIncrement(bl.FFreeMemCount);

  // prepare block content
  aBlockMem.Owner := bl;
  aBlockMem.FNextMemBlock := nil;
  aBlockMem.FPreviousMemBlock := nil;
  aBlockMem.FPreviousFreedMemBlock := nil;
end;

procedure TGlobalMemManager.FreeBlocksFromThreadMemory(aThreadMem: PThreadMemManager);
var
  i: NativeUInt;

  procedure __ProcessBlockMem(aOldBlock, aGlobalBlock: PMemBlockList);
  var
    allmem, prevmem, tempmem,
    lastunusedmem, lastinusemem,
    unusedmem, inusemem: PMemBlock;
  begin
    allmem        := aOldBlock.FFirstMemBlock;
    unusedmem     := nil;
    lastunusedmem := nil;
    inusemem      := nil;
    lastinusemem  := nil;

    // scan all memoryblocks and filter unused blocks
    while allmem <> nil do
    begin
      if allmem.Owner = nil then
        Break; // loop?

      // fully free, no mem in use?
{$ifdef USEBITMAP}
{$else}
      if allmem.FFreedIndex = allmem.FUsageCount then
      begin

        if aGlobalBlock.FFreeMemCount > C_GLOBAL_BLOCK_CACHE then
        begin
          // next one
          tempmem := allmem;
          allmem  := allmem.FNextMemBlock;
          // dispose
          aThreadMem.FreeMem(tempmem);
          Continue;
        end;

        // first item of list?
        if unusedmem = nil then
          unusedmem := allmem
        else
          // else add to list (link to previous)
          lastunusedmem.FNextMemBlock := allmem;
        lastunusedmem  := allmem;

        // update number of items cached
        inc(aGlobalBlock.FFreeMemCount);
      end
      else
      // some items in use (in other thread? or mem leak?)
      begin
        // first item of list?
        if inusemem = nil then
          inusemem := allmem
        else
          // else add to list (link to previous)
          lastinusemem.FNextMemBlock := allmem;
        lastinusemem  := allmem;

        // update number of items cached
        inc(aGlobalBlock.FFreeMemCount);
      end;
{$endif}

      allmem.Owner                  := aGlobalBlock;
      allmem.FNextFreedMemBlock     := nil;
      allmem.FPreviousMemBlock      := nil;
      allmem.FPreviousFreedMemBlock := nil;

      // next one
      allmem := allmem.FNextMemBlock;
    end;

    if inusemem <> nil then
    begin
      assert(lastinusemem <> nil);
      // add freemem list to front (replace first item, link previous to last item)
      repeat
        prevmem := aGlobalBlock.FFirstFreedMemBlock;
        lastinusemem.FNextFreedMemBlock := prevmem;
        if CAS0(prevmem, inusemem, aGlobalBlock.FFirstFreedMemBlock) then
          break;
{$ifdef BACKOFFSLEEP1}
        prevmem := aGlobalBlock.FFirstFreedMemBlock;
        lastinusemem.FNextFreedMemBlock := prevmem;
        if CAS1(prevmem, inusemem, aGlobalBlock.FFirstFreedMemBlock) then
          break;
{$endif}
      until false;
    end;

    if unusedmem <> nil then
    begin
      assert(lastunusedmem <> nil);
      //add unusedmem list to front (replace first item, link previous to last item)
      repeat
        prevmem := aGlobalBlock.FFirstMemBlock;
        lastunusedmem.FNextMemBlock := prevmem;
        if CAS0(prevmem, unusedmem, aGlobalBlock.FFirstMemBlock) then
          break;
{$ifdef BACKOFFSLEEP1}
        prevmem := aGlobalBlock.FFirstMemBlock;
        lastunusedmem.FNextMemBlock := prevmem;
        if CAS1(prevmem, unusedmem, aGlobalBlock.FFirstMemBlock) then
          break;
{$endif}
      until false;
    end;
  end;

begin
  assert(GetSmallMemManager=aThreadMem);
  for i := Low(aThreadMem.FMiniMemoryBlocks) to High(aThreadMem.FMiniMemoryBlocks) do
    __ProcessBlockMem( @aThreadMem.FMiniMemoryBlocks[i],   @Self.FFreedMiniMemoryBlocks[i]);
  for i := Low(aThreadMem.FSmallMemoryBlocks) to High(aThreadMem.FSmallMemoryBlocks) do
    __ProcessBlockMem( @aThreadMem.FSmallMemoryBlocks[i],  @Self.FFreedSmallMemoryBlocks[i]);
{$ifdef USEMEDIUM}
  for i := Low(aThreadMem.FMediumMemoryBlocks) to High(aThreadMem.FMediumMemoryBlocks) do
    __ProcessBlockMem( @aThreadMem.FMediumMemoryBlocks[i], @Self.FFreedMediumMemoryBlocks[i]);
{$endif}
end;

procedure TGlobalMemManager.FreeThreadManager(aThreadMem: PThreadMemManager);
var
  pprevthreadmem: PThreadMemManager;
begin
  // clear mem (partial: add to reuse list, free = free)
  FreeBlocksFromThreadMemory(aThreadMem);
  aThreadMem.Reset;

  { TODO : keep max nr of threads }
  // add to available list
  repeat
    pprevthreadmem := FFirstFreedThreadMemory;
    // make linked list: new one is first item (global var), next item is previous item
    aThreadMem.FNextThreadManager := pprevthreadmem;
    // try to set "result" in global var
    if CAS0(pprevthreadmem, aThreadMem, FFirstFreedThreadMemory) then
      break;
{$ifdef BACKOFFSLEEP1}
    pprevthreadmem := FFirstFreedThreadMemory;
    aThreadMem.FNextThreadManager := pprevthreadmem;
    if CAS1(pprevthreadmem, aThreadMem, FFirstFreedThreadMemory) then
      break;
{$endif}
  until false;
end;

function TGlobalMemManager.GetBlockMemory(aItemSize: NativeUInt): PMemBlock;
var bl: PMemBlockList;
    prevmem, nextmem: PMemBlock;
begin
  Result := nil;

  dec(aItemSize);
  if aItemSize < (length(Self.FFreedMiniMemoryBlocks)*32) then
    // blocks of 32: 32, 64, 96, 128, 160, 192, 224
    bl := @Self.FFreedMiniMemoryBlocks[aItemSize shr 5]
  else if aItemSize < (length(Self.FFreedSmallMemoryBlocks)*256) then
    // blocks of 256: 256,512,768,1024,1280,1536,1792[,2048] bytes
    bl := @Self.FFreedSmallMemoryBlocks[aItemSize shr 8]
{$ifdef USEMEDIUM}
  else if aItemSize < (length(Self.FFreedMediumMemoryBlocks)*2048) then
    // blocks of 2048: 2048,4096,6144,8192,10240,12288,14336,16384 bytes
    bl := @Self.FFreedMediumMemoryBlocks[aItemSize shr 11]
{$endif}
  else begin
    // not allocated by this unit (should not happen)
    assert(false);
    Exit;
  end;

  // get freed mem from list from front (replace first item)
  repeat
    if bl.FFirstFreedMemBlock <> nil then
    begin
      prevmem := bl.FFirstFreedMemBlock;
      if prevmem = nil then
        Continue;
      nextmem := prevmem.FNextFreedMemBlock;
      if CAS0(prevmem, nextmem, bl.FFirstFreedMemBlock) then
      begin
        Result := prevmem;
        Break;
      end;
{$ifdef BACKOFFSLEEP1}
      prevmem := bl.FFirstFreedMemBlock;
      if prevmem = nil then
        Continue;
      nextmem := prevmem.FNextFreedMemBlock;
      if CAS1(prevmem, nextmem, bl.FFirstFreedMemBlock) then
      begin
        Result := prevmem;
        Break;
      end;
{$endif}
    end
    // get free mem from list from front (replace first item)
    else if bl.FFirstMemBlock <> nil then
    begin
      prevmem := bl.FFirstMemBlock;
      if prevmem = nil then
        Continue;
      nextmem := prevmem.FNextMemBlock;
      if CAS0(prevmem, nextmem, bl.FFirstMemBlock) then
      begin
        Result := prevmem;
        Break;
      end;
{$ifdef BACKOFFSLEEP1}
      prevmem := bl.FFirstMemBlock;
      if prevmem = nil then
        Continue;
      nextmem := prevmem.FNextMemBlock;
      if CAS1(prevmem, nextmem, bl.FFirstMemBlock) then
      begin
        Result := prevmem;
        Break;
      end;
{$endif}
    end
    else
      Break;
  until false;

  if Result <> nil then
  begin
    InterlockedDecrement(bl.FFreeMemCount);
    Result.Owner := bl;
    Result.FNextFreedMemBlock := nil;
    Result.FNextMemBlock := nil;
    Result.FPreviousMemBlock := nil;
    Result.FPreviousFreedMemBlock := nil;
  end;
end;

function TGlobalMemManager.GetNewThreadManager: PThreadMemManager;
var
  pprevthreadmem, newthreadmem: PThreadMemManager;
begin
  Result := nil;

  // get one cached instance from freed list
  while FFirstFreedThreadMemory <> nil do
  begin
    pprevthreadmem := FFirstFreedThreadMemory;
    if pprevthreadmem <> nil then
      newthreadmem := pprevthreadmem.FNextThreadManager else
      newthreadmem := nil;
    // try to set "result" in global var
    if CAS0(pprevthreadmem, newthreadmem, FFirstFreedThreadMemory) then
    begin
      Result := pprevthreadmem;
      Result.FNextThreadManager := nil;
      break;
    end;
{$ifdef BACKOFFSLEEP1}
    pprevthreadmem := FFirstFreedThreadMemory;
    if pprevthreadmem <> nil then
      newthreadmem := pprevthreadmem.FNextThreadManager else
      newthreadmem := nil;
    if CAS1(pprevthreadmem, newthreadmem, FFirstFreedThreadMemory) then
    begin
      Result := pprevthreadmem;
      Result.FNextThreadManager := nil;
      break;
    end;
{$endif}
  end;
end;

procedure TGlobalMemManager.Init;
var i, j: NativeUInt;
begin
  fillchar(self,SizeOf(self),0);
  j := 32;
  for i := Low(FFreedMiniMemoryBlocks) to High(FFreedMiniMemoryBlocks) do
  begin
    FFreedMiniMemoryBlocks[i].Owner := @Self;
    FFreedMiniMemoryBlocks[i].FItemSize := j;
    inc(j,32);
  end;
  Assert(j=256);
  for i := Low(FFreedSmallMemoryBlocks) to High(FFreedSmallMemoryBlocks) do
  begin
    FFreedSmallMemoryBlocks[i].Owner := @Self;
    FFreedSmallMemoryBlocks[i].FItemSize := j;
    inc(j,256);
  end;
{$ifdef USEMEDIUM}
  Assert(j=2048);
  for i := Low(FFreedMediumMemoryBlocks) to High(FFreedMediumMemoryBlocks) do
  begin
    FFreedMediumMemoryBlocks[i].Owner := @Self;
    FFreedMediumMemoryBlocks[i].FItemSize := j;
    inc(j,2048);
  end;
  assert(j=18432);
{$else}
  assert(j=2304);
{$endif}
  FMainThreadMemory := GetSmallMemManager;
end;

{$ifndef PURE_PASCAL}
{$if CompilerVersion < 19}
procedure Move(const Source; var Dest; Count: Integer);
asm // eax=source edx=dest ecx=count
  // original code by John O'Harrow - included since Delphi 2007
  cmp     ecx, 32
  ja      @@LargeMove {Count > 32 or Count < 0}
  sub     ecx, 8
  jg      @@SmallMove
  jmp     dword ptr [@@JumpTable+32+ecx*4] {0..8 Byte Move}
@@SmallMove: {9..32 Byte Move}
  fild    qword ptr [eax+ecx] {Load Last 8}
  fild    qword ptr [eax] {Load First 8}
  cmp     ecx, 8
  jle     @@Small16
  fild    qword ptr [eax+8] {Load Second 8}
  cmp     ecx, 16
  jle     @@Small24
  fild    qword ptr [eax+16] {Load Third 8}
  fistp   qword ptr [edx+16] {Save Third 8}
@@Small24:
  fistp   qword ptr [edx+8] {Save Second 8}
@@Small16:
  fistp   qword ptr [edx] {Save First 8}
  fistp   qword ptr [edx+ecx] {Save Last 8}
@@Exit:
  ret
  lea eax,eax+0 // for alignment of @@JumpTable
@@JumpTable: {4-Byte Aligned}
  dd      @@Exit, @@M01, @@M02, @@M03, @@M04, @@M05, @@M06, @@M07, @@M08
@@LargeForwardMove: {4-Byte Aligned}
  push    edx
  fild    qword ptr [eax] {First 8}
  lea     eax, [eax+ecx-8]
  lea     ecx, [ecx+edx-8]
  fild    qword ptr [eax] {Last 8}
  push    ecx
  neg     ecx
  and     edx, -8 {8-Byte Align Writes}
  lea     ecx, [ecx+edx+8]
  pop     edx
@FwdLoop:
  fild    qword ptr [eax+ecx]
  fistp   qword ptr [edx+ecx]
  add     ecx, 8
  jl      @FwdLoop
  fistp   qword ptr [edx] {Last 8}
  pop     edx
  fistp   qword ptr [edx] {First 8}
  ret
@@LargeMove:
  jng     @@LargeDone {Count < 0}
  cmp     eax, edx
  ja      @@LargeForwardMove
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     @@LargeForwardMove
  sub     ecx, 8 {Backward Move}
  push    ecx
  fild    qword ptr [eax+ecx] {Last 8}
  fild    qword ptr [eax] {First 8}
  add     ecx, edx
  and     ecx, -8 {8-Byte Align Writes}
  sub     ecx, edx
@BwdLoop:
  fild    qword ptr [eax+ecx]
  fistp   qword ptr [edx+ecx]
  sub     ecx, 8
  jg      @BwdLoop
  pop     ecx
  fistp   qword ptr [edx] {First 8}
  fistp   qword ptr [edx+ecx] {Last 8}
@@LargeDone:
  ret
@@M01:
  movzx   ecx, [eax]
  mov     [edx], cl
  ret
@@M02:
  movzx   ecx, word ptr [eax]
  mov     [edx], cx
  ret
@@M03:
  mov     cx, [eax]
  mov     al, [eax+2]
  mov     [edx], cx
  mov     [edx+2], al
  ret
@@M04:
  mov     ecx, [eax]
  mov     [edx], ecx
  ret
@@M05:
  mov     ecx, [eax]
  mov     al, [eax+4]
  mov     [edx], ecx
  mov     [edx+4], al
  ret
@@M06:
  mov     ecx, [eax]
  mov     ax, [eax+4]
  mov     [edx], ecx
  mov     [edx+4], ax
  ret
@@M07:
  mov     ecx, [eax]
  mov     eax, [eax+3]
  mov     [edx], ecx
  mov     [edx+3], eax
  ret
@@M08:
  fild    qword ptr [eax]
  fistp   qword ptr [edx]
end;
{$ifend}
{$endif PURE_PASCAL}

function Scale_ReallocMem(aMemory: Pointer; aSize: Integer): Pointer;
var
  pm: PMemBlock;
  p: Pointer;
begin
  // ReAlloc can be misued as GetMem or FreeMem (documented in delphi help) so check what the user wants
  Assert(NativeUInt(aMemory) > $10000);

  // Normal realloc of exisiting data?
  if (aMemory <> nil) and (aSize > 0) then
  begin
    p  := Pointer(NativeUInt(aMemory) - SizeOf(TMemHeader));
    pm := PMemHeader(p).Owner;

    if pm <> nil then
    with pm^ do
    begin
      if (NativeUInt(aSize) <= Owner.FItemSize) then
      begin
        // new size smaller than current size
        if NativeUInt(aSize) > (Owner.FItemSize shr 2) then
          Result := aMemory // no resize needed up to 1/4 the current item size
        else
        // too much downscaling: use move
        with GetSmallMemManager^ do
        begin
          Result := GetMem(aSize); // new mem
          if aMemory <> Result then
          begin
            Move(aMemory^, Result^, aSize); // copy (use smaller new size)
            FreeMem(aMemory); // free old mem
          end;
        end;
      end
      else
      with GetSmallMemManager^ do
      begin
        // new size bigger than current size: avoid moves with small granularity
        if aSize <= (length(FMiniMemoryBlocks)*32) then
          aSize := (length(FMiniMemoryBlocks)*32) else
        if aSize <= (length(FSmallMemoryBlocks)*256) then
          aSize := (length(FSmallMemoryBlocks)*256)
        {$ifdef USEMEDIUM}
        else if aSize <= (length(FMediumMemoryBlocks)*2048) then
          aSize := (length(FMediumMemoryBlocks)*2048)
        {$endif};
        Result := GetMem(aSize); // new mem
        if aMemory <> Result then
        begin
          Move(aMemory^, Result^, Owner.FItemSize); // copy (use smaller old size)
          FreeMem(aMemory); // free old mem
        end;
      end;
    end
    // was allocated via OldMM -> rely on OldMM for reallocation
    else
    begin
      Result := OldMM.ReallocMem(p, aSize + SizeOf(TMemHeader));
      if Result<>nil then
      begin
        PMemHeader(Result)^.Owner := nil; // mark not from our memlist
        Result := Pointer(NativeUInt(Result) + SizeOf(TMemHeader) );
      end;
    end;
  end
  else
  begin
    if (aMemory = nil) and (aSize > 0) then
      // GetMem disguised as ReAlloc
      Result := Scale_GetMem(aSize)
    else
    begin
      // FreeMem disguised as ReAlloc
      Result := nil;
      Scale_FreeMem(aMemory);
    end;
  end;
end;


function Scale_GetMem(aSize: Integer): Pointer;
{$ifdef HASINLINE}
begin
  Result := GetSmallMemManager.GetMem(aSize);
  Assert(NativeUInt(Result) > $10000);
end;
{$else}
  {$ifdef PURE_PASCAL}
  begin
    Result := GetSmallMemManager.GetMem(aSize);
    Assert(NativeUInt(Result) > $10000);
  end;
  {$else}
  asm
    {$ifdef INLINEGOWN}
    mov edx,eax
    mov eax,GOwnTlsOffset
    mov ecx,fs:[$00000018]
    mov eax,[ecx+eax]      // fixed offset, calculated only once
    or eax,eax
    jnz TThreadMemManager.GetMem
    push edx
    call CreateSmallMemManager
    pop edx
    jmp TThreadMemManager.GetMem
    {$else}
    push eax
    call GetSmallMemManager
    pop edx
    jmp TThreadMemManager.GetMem
    {$endif}
  end;
  {$endif}
{$endif}

function Scale_AllocMem(aSize: Cardinal): Pointer;
begin
  Result := GetSmallMemManager.GetMem(aSize);
  Assert(NativeUInt(Result) > $10000);
  fillchar(Result^, aSize, 0); // AllocMem() = GetMem()+ZeroMemory()
end;

function Scale_FreeMem(aMemory: Pointer): Integer;
{$ifdef HASINLINE}
begin
  Assert(NativeUInt(aMemory) > $10000);
  Result := GetSmallMemManager.FreeMem(aMemory);
end;
{$else}
  {$ifdef PURE_PASCAL}
  begin
    Assert(NativeUInt(aMemory) > $10000);
    Result := GetSmallMemManager.FreeMem(aMemory);
  end;
  {$else}
  asm
    {$ifdef INLINEGOWN}
    mov edx,eax
    mov eax,GOwnTlsOffset
    mov ecx,fs:[$00000018]
    mov eax,[ecx+eax]      // fixed offset, calculated only once
    or eax,eax
    jnz TThreadMemManager.FreeMem
    push edx
    call CreateSmallMemManager
    pop edx
    jmp TThreadMemManager.FreeMem
    {$else}
    push eax
    call GetSmallMemManager
    pop edx
    jmp TThreadMemManager.FreeMem
    {$endif}
  end;
  {$endif}
{$endif}

{$ifdef USEMEMMANAGEREX}
function Scale_RegisterMemoryLeak(P: Pointer): Boolean;
begin
  { TODO : implement memory leak checking }
  Result := OldMM.RegisterExpectedMemoryLeak(p);
end;

function Scale_UnregisterMemoryLeak(P: Pointer): Boolean;
begin
  Result := OldMM.UnregisterExpectedMemoryLeak(p);
end;
{$endif}

type
  TEndThread = procedure(ExitCode: Integer);
var
  OldEndThread: TEndThread;

procedure NewEndThread(ExitCode: Integer); //register; // ensure that calling convension matches EndThread
begin
  // free all thread mem
  GlobalManager.FreeThreadManager( GetSmallMemManager );
  // OldEndThread(ExitCode);  todo: make trampoline with original begin etc
  // code of original EndThread;
  ExitThread(ExitCode);
end;

type
  PJump = ^TJump;
  TJump = packed record
    OpCode: Byte;
    Distance: Integer;
  end;
var
  NewCode: TJump = (OpCode  : $E9;
                    Distance: 0);

// redirect calls to System.EndThread to NewEndThread
procedure PatchThread;
var
  pEndThreadAddr: PJump;
  iOldProtect: DWord;
begin
  pEndThreadAddr := Pointer(@EndThread);
  Scale_VirtualProtect(pEndThreadAddr, 5, PAGE_EXECUTE_READWRITE, iOldProtect);
  // calc jump to new function
  NewCode.Distance := Cardinal(@NewEndThread) - (Cardinal(@EndThread) + 5);
  // store old
  OldEndThread := TEndThread(pEndThreadAddr);
  // overwrite with jump to new function
  pEndThreadAddr^  := NewCode;
  // flush CPU
  FlushInstructionCache(GetCurrentProcess, pEndThreadAddr, 5);
end;

const
{$ifdef USEMEMMANAGEREX}
  ScaleMM_Ex: TMemoryManagerEx = (
    GetMem: Scale_GetMem;
    FreeMem: Scale_FreeMem;
    ReallocMem: Scale_ReallocMem;
    AllocMem: Scale_AllocMem;
    RegisterExpectedMemoryLeak: Scale_RegisterMemoryLeak;
    UnregisterExpectedMemoryLeak: Scale_UnregisterMemoryLeak );
{$else}
  ScaleMM_Ex: TMemoryManager = (
    GetMem: Scale_GetMem;
    FreeMem: Scale_FreeMem;
    ReallocMem: Scale_ReallocMem );
{$endif}

procedure ScaleMMInstall;
begin
  {$ifndef PURE_PASCAL}
  // get TLS slot
  GOwnTlsIndex  := TlsAlloc;
  // write fixed offset to TLS slot (instead calc via GOwnTlsIndex)
  _FixedOffset;
  {$endif}

  // Hook memory Manager
  GetMemoryManager(OldMM);
  if @OldMM <> @ScaleMM_Ex then
    SetMemoryManager(ScaleMM_Ex);

  // init main thread manager
  GlobalManager.Init;

  // we need to patch System.EndThread to properly mark memory to be freed
  PatchThread;
end;

initialization
  ScaleMMInstall;

finalization
  { TODO : check for memory leaks }
  GlobalManager.FreeAllMemory;

end.