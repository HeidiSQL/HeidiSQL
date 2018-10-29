// **************************************************************************************************
// Delphi Instruction Decode Library
// Unit InstDecode
// http://code.google.com/p/delphi-detours-library/

// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is InstDecode.pas.
//
// The Initial Developer of the Original Code is Mahdi Safsafi [SMP3].
// Portions created by Mahdi Safsafi . are Copyright (C) 2013-2014 Mahdi Safsafi .
// All Rights Reserved.
//
// **************************************************************************************************

unit InstDecode;

interface

uses Windows;

{$I dDefs.inc}

type
  PPrefixes = ^TPrefixes;

  TPrefixes = packed record
    Count: Byte; // Numbers of prefixes used by the instruction .
    case Integer of
      0: (Pref1, Pref2, Pref3, Pref4: Byte);
      1: (Prefixes: Array [0 .. 4 - 1] of Byte);
      2: (Value: DWORD);
  end;

  PModRM = ^TModRM;

  TModRM = record
    Used: Boolean; { True if instruction use ModRM . }
    Value: Byte; { Value of ModRM . }
    rMod: Byte; { Mod field value . }
    rReg: Byte; { Reg field value . }
    rRM: Byte; { RM field value . }
    OpExt: Boolean; { True if  ModRM store OpCode extension . }
  end;

  PSIB = ^TSIB;

  TSIB = packed record
    Used: Boolean;
    Scale: Byte;
    Index: Byte;
    Base: Byte;
    Value: Byte;
  end;

  PDisplacement = ^TDisplacement;

  TDisplacement = packed record
    Used: Boolean; { True if instruction use Displacement . }
    i32: Boolean; { True if size of Displacement = 32 bit ; otherwise False : size of Displacement = 8 bit }
    Value: Integer; { Displacement value . }
    Relative: Boolean; { True if Displacement is Relative => only for x64 bit . }
  end;

  PImmediate = ^TImmediate;

  TImmediate = packed record
    Used: Boolean; { True if instruction use Immediate . }
    SizeOfImmediate: Byte; { Size Of Immediate Value [1,2,4,8] bytes . }
    Value: Int64; { Value of Immediate . }
  end;

  PJumpCall = ^TJumpCall;

  TJumpCall = packed record
    Used: Boolean; { True if instruction is JUMP or CALL . }
    JumpUsed: Boolean; { True if instruction is JUMP . }
    CallUsed: Boolean; { True if instruction is CALL . }
    Relative: Boolean; { True if instruction use Relative offset . }
    IndirectDispOnly: Boolean; { True if JUMP/CALL Indirect Disp only => JMP [123456] }
    IndirectReg: Boolean; { True if JUMP/CALL Indirect  => JMP [EAX]; JMP [EAX + 123456] }
    OffsetSize: Byte; { Size of Offset . }
    Offset: Integer; { Offset value . }
    Address: UInt64; { Destination address . }
  end;

  TOpCodeParams = packed record
    nOp: Byte; // Numbers of operands .
    Op1: DWORD; // Operand 1 flags .
    Op2: DWORD; // Operand 2 flags .
    Op3: DWORD; // Operand 3 flags .
    Op4: DWORD; // Operand 4 flags .
  end;

  PInstruction = ^TInstruction;

  TInstruction = packed record
    Address: Pointer; { Current Address }
    Mod16: Boolean; { True if size of operand =  16 bit }
    Mod64: Boolean; { True if size of operand =  64 bit }
    Prefixes: TPrefixes; { Prefixes }
    OpCode: Word; { OpCode }
    nOpCode: Byte; { Numbers of OpCode ==> Max = 3 }
    SOpCode: Byte; { Secondary OpCode Value }
    SOpUsed: Boolean; { True if Secondary OpCode Used . }
    Params: TOpCodeParams; { Operand Params }
    ModRM: TModRM; { ModRM }
    SIB: TSIB; { SIB }
    Displacement: TDisplacement; { Displacement }
    Immediate: TImmediate; { Immediate }
    JumpCall: TJumpCall; { JumpCall }
    InstSize: ShortInt; { Instruction Size }
    NextInst: Pointer; { Pointer to the next instruction . }
  end;

const
  { preffixes codes }
  { Prefix group 1 }
  prefLock = $F0;
  prefRepn = $F2;
  prefRep = $F3;
  { Prefix group 2 }
  prefCS = $2E;
  prefSS = $36;
  prefDS = $3E;
  prefES = $26;
  prefFS = $64;
  prefGS = $65;
  pref2E = $2E; // Branch not taken
  pref3E = $3E; // Branch taken
  { Prefix group 3 }
  prefSize66 = $66; // Operand-size override prefix ==>Operand 16bit .
  { Prefix group 4 }
  prefAdd67 = $67; // Address-size override prefix
  prefSize48 = $48; // Operand-size override prefix ==>Operand 64bit .

  Max_Inst_Len32 = 17; // 4 Prefix + 2 Byte OpCode + 1 Second OpCode + 1 Byte ModRM + 1 SIB + 4 Byte Disp + 4 Byte Imm.
  Max_Inst_Len64 = 21; // 4 Prefix + 2 Byte OpCode + 1 Second OpCode + 1 Byte ModRM + 1 SIB + 4 Byte Disp + 8 Byte Imm.

type
  TCPUX = (CPUX32, CPUX64);

function DecodeInstruction(const Addr: Pointer; const Inst: PInstruction; const CPUX: TCPUX): ShortInt;
function GetMaxInstLen(const CPUX: TCPUX): ShortInt;

{$IFNDEF FPC}
{$IF CompilerVersion <25}

{ dcc < XE4 }
type
  PShort = ^SHORT;
  PUInt64 = ^UInt64;
{$IFEND}
{$ELSE FPC}

type
  // PShort = ^SHORT;
  PUInt64 = ^UInt64;

{$ENDIF FPC}

implementation

{$I OpCodesTables.inc}
{$I ModRMTable.inc}

type
  PSysEAJump = ^TSysEAJump;

  TSysEAJump = packed record
    D: DWORD;
    W: Word;
  end;

{$WARN NO_RETVAL OFF}

function GetMaxInstLen(const CPUX: TCPUX): ShortInt;
begin
  case CPUX of
    CPUX32: Result := Max_Inst_Len32;
    CPUX64: Result := Max_Inst_Len64;
  end;
end;

function GetBit(const Value: Cardinal; const nBit: Byte): Boolean;
begin
  Result := (Value and (1 shl nBit)) <> 0;
end;

function GetMod(const ModRegRM: Byte): Byte; {$IFDEF MustInline}inline; {$ENDIF}
begin
  Result := ModRegRM shr 6;
end;

function GetReg(const ModRegRM: Byte): Byte; {$IFDEF MustInline}inline; {$ENDIF}
begin
  Result := Byte(ModRegRM shl 2);
  Result := Result shr 5;
end;

function GetRM(const ModRegRM: Byte): Byte; {$IFDEF MustInline}inline; {$ENDIF}
begin
  { Result := (ModRegRM shl 5);
    Result := Result shr 5; }
  Result := ModRegRM and 7;
end;

function GetScale(const SIB: Byte): Byte; {$IFDEF MustInline}inline; {$ENDIF}
begin
  Result := SIB shr 6;
  case Result of
    { 00b } 0: Result := 1;
    { 01b } 1: Result := 2;
    { 10b } 2: Result := 4;
    { 11b } 3: Result := 8;
  end;
end;

function GetIndex(const SIB: Byte): Byte; {$IFDEF MustInline}inline; {$ENDIF}
begin
  Result := Byte(SIB shl 2);
  Result := Result shr 5;
end;

function GetBase(const SIB: Byte): Byte; {$IFDEF MustInline}inline; {$ENDIF}
begin
  { Result := (SIB shl 5);
    Result := Result shr 5; }
  Result := SIB and 7;
end;

{$HINTS OFF}

function DecodePreffixes(const Addr: Pointer; const Inst: PInstruction; const CPUX: TCPUX): Byte;
var
  i: Integer;
  P: PByte;
  PrefixUsed: Boolean;
begin
  Result := 0;
  P := PByte(Addr);
  Inst^.Prefixes.Count := 0;
  Inst^.Prefixes.Value := 0;
  Inst^.Mod16 := False;
  Inst^.Mod64 := False;
  PrefixUsed := False;

  for i := 0 to 3 do
  begin
    PrefixUsed := P^ in [prefLock, prefRepn, prefRep, prefCS, prefSS, prefDS, prefES, prefFS, prefGS {$IFNDEF FPC}, pref2E, pref3E{$ENDIF}, prefSize66, prefAdd67];
    if not PrefixUsed then
      if CPUX = CPUX64 then
      begin
        PrefixUsed := (P^ = prefSize48);
        if PrefixUsed then
          Inst^.Mod64 := True;
      end;
    if PrefixUsed then
    begin
      Inst^.Prefixes.Prefixes[i] := P^;
      Inc(Result);
      if P^ = prefSize66 then
        Inst^.Mod16 := True;
    end
    else
      Break;
    Inc(P);
  end;
  Inst^.Prefixes.Count := Result;
end;
{$HINTS ON}

function GetEntryOpCode(const Opw: Word): TOpCodeEntryInfo;
var
  Op: Byte;
begin
  FillChar(Result, SizeOf(TOpCodeEntryInfo), #0);
  Op := Byte(Opw);
  if Op = $0F then
    Result := TwoByteOpCodesEntry[HiByte(Opw)]
  else
    Result := OneByteOpCodesEntry[Op];
end;

function DecodeOpCode(const Addr: Pointer; const Inst: PInstruction; const CPUX: TCPUX): Byte;
var
  P: PByte;
  Entry: TOpCodeEntryInfo;
  n: Byte;
  Op, SOp: Byte;
  SOpUsed: BOOL;
begin
  P := PByte(Addr);
  Result := 1; // Default for one byte OpCode .
  Inst^.nOpCode := 1;
  if P^ = $0F then
  begin
    { Two bytes OpCode }
    Inst^.nOpCode := 2;
    Inst^.OpCode := PWORD(P)^;
    Inc(Result);
  end
  else
    Inst^.OpCode := PByte(P)^; // One byte OpCode .

  Entry := GetEntryOpCode(Inst^.OpCode);
  n := 0; // Number of operands .
  if Entry.Op1 <> Op_NONE then
    Inc(n);
  if Entry.Op2 <> Op_NONE then
    Inc(n);
  if Entry.Op3 <> Op_NONE then
    Inc(n);
  if Entry.Op4 <> Op_NONE then
    Inc(n);

  { Operands Flags }
  Inst^.Params.Op1 := Entry.Op1;
  Inst^.Params.Op2 := Entry.Op2;
  Inst^.Params.Op3 := Entry.Op3;
  Inst^.Params.Op4 := Entry.Op4;
  Inst^.Params.nOp := n;

  SOp := 0;
  SOpUsed := False;
  if P^ = $0F then
  begin
    Inc(P);
    Op := P^; // Primary OpCode .
    Inc(P);
    SOp := P^; // Secondary OpCode .
    case Op of
      $01: SOpUsed := SOp in [$C1 .. $C4, $C8 .. $C9, $D0 .. $D1, $F8 .. $F9];
      $38, $39: SOpUsed := SOp in [$00 .. $B, $10, $14 .. $15, $17, $1C, $1D, $1E, $20 .. $25, $28 .. $2B, $30 .. $35, $37 .. $41, $80 .. $81, $F0 .. $F1];
      $3A: SOpUsed := SOp in [$8 .. $F, $14 .. $17, $20 .. $22, $40 .. $42, $60 .. $63];
    else SOpUsed := False;
    end;
  end;

  { If Secondary OpCode is used . }
  if SOpUsed then
  begin
    Inst^.SOpCode := SOp;
    Inc(Inst^.nOpCode);
    Inc(Result);
  end
  else
    Inst^.SOpCode := 0;
  Inst^.SOpUsed := SOpUsed;
end;

function DecodeSIB(const Addr: Pointer; const Inst: PInstruction; const CPUX: TCPUX): Byte;
var
  SIB: Byte;
begin
  Result := 0;
  // Inst^.SIB.Used := ((Inst^.ModRM.rMod < 3) and (Inst^.ModRM.rRM = 4));
  SIB := PByte(Addr)^;
  if Inst^.SIB.Used then
  begin
    Inst^.SIB.Value := SIB;
    Inst^.SIB.Scale := GetScale(SIB);
    Inst^.SIB.Index := GetIndex(SIB);
    Inst^.SIB.Base := GetBase(SIB);
    Inc(Result); // SIB Used .
  end;
end;

function DecodeModRM(const Addr: Pointer; const Inst: PInstruction; const CPUX: TCPUX): Byte;
var
  P: PByte;
  Flags: Byte;
  Entry: TOpCodeEntryInfo;
  ModRM, ModRMF: Byte;
begin
  Result := 0;
  P := PByte(Addr);
  Entry := GetEntryOpCode(Inst^.OpCode);
  Flags := Entry.Flags;
  Inst^.SIB.Used := False;

  { if Inst^.ModRM.OpExt => Reg field contain opcode extension . }
  Inst^.ModRM.OpExt := (Flags and Opf_ModRMExt = Opf_ModRMExt);

  if Flags and Opf_ModRM = Opf_ModRM then
  begin
    { ModRM Used . }
    ModRM := P^;
    Inst^.ModRM.Used := True;
    Inst^.ModRM.rMod := GetMod(ModRM);
    Inst^.ModRM.rRM := GetRM(ModRM);
    Inst^.ModRM.rReg := GetReg(ModRM);
    Inst^.ModRM.Value := ModRM;
    Inc(Result);
    Inc(P);
    ModRMF := ModRMFlags[ModRM]; // ModRM Flags.
    if ModRMF in [8, $B, $D] then // SIB is used ! => See ModRMFlags .
    begin
      Inst^.SIB.Used := True;
      Inc(Result, DecodeSIB(P, Inst, CPUX));
    end;
  end;
end;

function DecodeJump(const Addr: Pointer; const Inst: PInstruction; const CPUX: TCPUX): Byte;
var
  P: PByte;
  Q: Pointer;
  Opf: Byte;
  Jbf: DWORD;
  Entry: TOpCodeEntryInfo;
  Value: UInt64;
  Size: Byte;
  DstAddr: UInt64;
  SysEAJump: PSysEAJump;
begin
  Result := 0;
  Size := 0;
  Entry := GetEntryOpCode(Inst^.OpCode);
  Jbf := 0;
  Opf := Entry.Flags;
  P := PByte(Addr);
  if (Inst^.OpCode = $FF) and (Inst^.ModRM.Used) and (Inst^.ModRM.rReg > 1) and (Inst^.ModRM.rReg < 6) then
  begin
    { Absolute Indirect JMP }
    Inst^.JumpCall.Used := True;
    Inst^.JumpCall.CallUsed := Inst^.ModRM.rReg in [2, 3]; // CALL instruction .
    Inst^.JumpCall.JumpUsed := Inst^.ModRM.rReg in [4, 5]; // JMP instruction .
    if (Inst^.ModRM.rMod = 00) then
    begin
      Inst^.JumpCall.IndirectDispOnly := False;
      Inst^.JumpCall.IndirectReg := True; // JMP [EAX] .
      Inst^.JumpCall.OffsetSize := 4;
      if (Inst^.ModRM.rRM = 5) and (Inst^.Displacement.Used) then
      begin
        Q := Pointer(Inst^.Displacement.Value);
        Inst^.JumpCall.IndirectReg := False;
        Inst^.JumpCall.IndirectDispOnly := True; // JMP DWORD Ptr [Address]; where Address = Destination .
        if CPUX = CPUX32 then
        begin
          { E.g :
            DW 401591  = 401792.
            JMP DWORD PTR DS:[401591] .
            Offset = 401591 .
            Address = Pointer to the address stored on the Offset value = 401792 .
          }
          Inst^.JumpCall.Address := UINT(Q^);
          Inst^.JumpCall.Offset := Integer(Inst^.Displacement.Value);
        end
        else
        begin
          { E.g :
            Distination = $76A6A418 .
            JMP QWORD  PTR [REL $002d079a]
            $002d079a = Difference between EIP and the variable (V) that hold the distination address pointer.
            @V:= EIP + 4 bytes (SizeOf Rel offset=> SizeOf(002d079a)= 4 Bytes) + offset value (002d079a).
            Distination := V^ => Value stored on the variable V .
          }
          Inst^.JumpCall.Address := UInt64(Pointer(UInt64(P) + 4 + UInt64(Q))^);
          Inst^.JumpCall.Offset := Integer(Inst^.Displacement.Value);
        end;
      end;
    end;
    Exit;
  end;

  if Opf and Opf_Jump = Opf_Jump then
  begin
    if (Entry.Op1 and Op_J = Op_J) or (Entry.Op1 and Op_Ap = Op_Ap) then
      Jbf := Entry.Op1
    else if (Entry.Op2 and Op_J = Op_J) or (Entry.Op2 and Op_Ap = Op_Ap) then
      Jbf := Entry.Op2;

    if Jbf and Op_J = Op_J then // Jump used .
    begin
      { Relative JMP }
      Inst^.JumpCall.Used := True;
      Inst^.JumpCall.Relative := True;
      if Jbf and Op_Jbs = Op_Jbs then
      begin
        Inst^.JumpCall.Offset := PShortInt(P)^;
        Size := 1;
      end
      else if Jbf and Op_Jvds = Op_Jvds then
      begin
        Size := 2;
        if Inst^.Mod16 then
          Inst^.JumpCall.Offset := PShort(P)^
        else
        begin
          Size := 4;
          Inst^.JumpCall.Offset := PInteger(P)^
        end;
      end;
      if Size > 0 then
      begin
        Inst^.JumpCall.OffsetSize := Size;
        DstAddr := UInt64(Inst^.Address);
        DstAddr := DstAddr + Inst^.JumpCall.Offset + Inst^.nOpCode { OpCodes } + Size;
        Inst^.JumpCall.Address := DstAddr;
        Result := Size;
      end;
    end
    else if Jbf and Op_Ap = Op_Ap then
    begin
      { Used only by the System ! }
      Inst^.JumpCall.Relative := False;
      Inst^.JumpCall.IndirectDispOnly := False;
      if Inst^.Mod16 then
      begin
        Result := 4;
        Inst^.JumpCall.OffsetSize := 4;
        Inst^.JumpCall.Address := PDWORD(P)^;
      end
      else
      begin
        Result := 6;
        Inst^.JumpCall.OffsetSize := 6;
        SysEAJump := PSysEAJump(P);
        Value := UInt64(SysEAJump^.W);
        Value := UInt64(Value shl 32);
        Value := UInt64(Value or SysEAJump^.D);
        Inst^.JumpCall.Address := Value;
      end;
    end;
  end;
end;

function DecodeDisplacement(const Addr: Pointer; const Inst: PInstruction; const CPUX: TCPUX): Byte;
var
  Size: Byte;
  ModRMF: Byte;
begin
  Size := 0;
  Result := 0;
  if Inst^.ModRM.Used then
  begin
    Inst^.Displacement.Relative := (CPUX = CPUX64) and (Inst^.ModRM.rMod = 00) and (Inst^.ModRM.rRM = 5);
    {
      Relative Displacement only for x64 !
      e.g : MOV RAX ,[REL 123456]
    }
    ModRMF := ModRMFlags[Inst^.ModRM.Value];
    case ModRMF of
      00, 01: Exit; // No Disp.
      03, $B: Size := 1; // 8 Bit Disp .
      05, 08, $D: Size := 4; // 32 Bit Disp .
    end;
  end;
  if Size = 1 then
    Inst^.Displacement.Value := PShortInt(Addr)^
  else if Size = 4 then
    Inst^.Displacement.Value := PInteger(Addr)^;

  Inst^.Displacement.Used := (Size > 0);
  Inst^.Displacement.i32 := (Size = 4);
  Inc(Result, Size);
  Inc(Result, DecodeJump(Addr, Inst, CPUX));
end;

function DecodeImmediate(const Addr: Pointer; const Inst: PInstruction; const CPUX: TCPUX): Byte;
var
  P: PByte;
  Entry: TOpCodeEntryInfo;
  immf: DWORD;
  Size: Byte;
begin
  P := PByte(Addr);
  Result := 0;
  Inst^.Immediate.Used := False;
  Entry := GetEntryOpCode(Inst^.OpCode);
  immf := 0;
  Size := 0;
  if Inst^.JumpCall.Used then
    Exit;
  try
    { Check if data is used ! }
    if (Entry.Op1 and Op_Imm = Op_Imm) then
      immf := Entry.Op1
    else if (Entry.Op2 and Op_Imm = Op_Imm) then
      immf := Entry.Op2
    else if (Entry.Op3 and Op_Imm = Op_Imm) then
      immf := Entry.Op3
    else if (Entry.Op4 and Op_Imm = Op_Imm) then
      immf := Entry.Op4;

    if immf > 0 then
    begin
      { Yes ,there is data (Immediate) }
      Inst^.Immediate.Used := True;
      if (immf and Op_Ivqp = Op_Ivqp) or (immf and Op_Ivs = Op_Ivs) then
      begin
        Size := 2; // Smaller size = SizeOf(WORD)= 2 Bytes .
        if not Inst^.Mod16 then
          Inc(Size, 2); // 32 bit data .
        if Inst^.Mod64 then
          Inc(Size, 4); // 64 bit data .
        Exit;
      end
      else if immf and Op_Iw = Op_Iw then
        Size := 2 // 16 bit data .
      else if immf and Op_Ib = Op_Ib then
        Size := 1 // 8 bit data .
    end;
  finally
    if Size > 0 then
    begin
      Inst^.Immediate.SizeOfImmediate := Size;
      { Get Immediate value }
      if (immf and Op_SignExt = Op_SignExt) or (Entry.Flags and Opf_S = Opf_S) then
      begin
        { Sign-extend .eg :PUSH -5 }
        if Size = 8 then
          Inst^.Immediate.Value := PInt64(P)^
        else if Size = 4 then
          Inst^.Immediate.Value := PInteger(P)^
        else if Size = 2 then
          Inst^.Immediate.Value := PShort(P)^
        else // Size = 1 .
          Inst^.Immediate.Value := PShortInt(P)^;
      end
      else
      begin
        if Size = 8 then
          Inst^.Immediate.Value := PUInt64(P)^
        else if Size = 4 then
          Inst^.Immediate.Value := PDWORD(P)^
        else if Size = 2 then
          Inst^.Immediate.Value := PWORD(P)^
        else // Size = 1 .
          Inst^.Immediate.Value := PByte(P)^;
      end;
      Inc(Result, Size);
    end;
  end;
end;

function DecodeInstruction(const Addr: Pointer; const Inst: PInstruction; const CPUX: TCPUX): ShortInt;
var
  P: PByte;
  Size: ShortInt;
begin
  Result := 0;
  P := PByte(Addr);
  Inst^.Address := Addr;
  Size := DecodePreffixes(P, Inst, CPUX);
  Inc(Result, Size);
  Inc(P, Size);
  Size := DecodeOpCode(P, Inst, CPUX);
  Inc(Result, Size);
  Inc(P, Size);
  Size := DecodeModRM(P, Inst, CPUX);
  Inc(Result, Size);
  Inc(P, Size);
  Size := DecodeDisplacement(P, Inst, CPUX);
  Inc(Result, Size);
  Inc(P, Size);
  Size := DecodeImmediate(P, Inst, CPUX);
  Inc(Result, Size);

  if Result > GetMaxInstLen(CPUX) then
  begin
    Result := -1;
    Exit;
  end;

  Inst^.InstSize := Result;
  Inc(P, Result);
  Inst^.NextInst := P;
end;

end.
