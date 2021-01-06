// **************************************************************************************************
// x86 Instruction Decode Library
// Unit InstDecode
// https://github.com/MahdiSafsafi/DDetours
//
// This Source Code Form is subject to the terms of the Mozilla 
// Public License, v. 2.0. If a copy of the MPL was not distributed 
// with this file, You can obtain one at
// https://mozilla.org/MPL/2.0/.
// **************************************************************************************************

{ ===============================> CHANGE LOG <======================================================
  ==>  Jun, 7, 2020:
  +Added support for older Delphi version (D7+).
  +Added support for FPC.
  +Fixed some bug related to displacement.
  
  ==>  Dec 27,2014 , Mahdi Safsafi :
  +BugFix : IN/INS/OUT/OUTS instructions decoding.
  +BugFix : MOV with offset instructions decoding.

  ==> Version 2:
  +Updated opcodes map .
  +Added support to three byte escape Table
  +Added support to vex decoding (vex three & two byte).
  +Added support to groups opcodes instructions.
  +Added support to decode invalid opcode .
  +Added support to 16-bits ModRm .
  +Added support to handling errors.
  +Added support for mandatory prefixes.
  +Improve Decoding Process .=> Very faster than the old one !
  +Reduce memory usage .
  +Removing inused fields.
  +Better support for REX prefix.
  +Reduce OpCodesTable data size (the old : 8670 bytes => the new one : 1020 bytes !)
  +BugFix : FPU instructions length.
  +BugFix : Instructions that use two immediat .
  +BugFix : Invalid instructions .
  +BugFix : Invalid instructions for some mandatory prefixes.
  +Many Bug Fix.

  ====================================================================================================== }

unit InstDecode;

{$IFDEF FPC}
{$MODE DELPHI}
{$HINTS OFF}
{$WARN 4056 OFF}
{$WARN 4082 OFF}
{$ENDIF FPC}

interface

{$I DDetoursDefs.inc}

uses
  SysUtils,
  LegacyTypes;

const
  { CPUX }
  CPUX32 = $00; { x86-32 }
  CPUX64 = $01; { x86-64 }
  CPUX = {$IFDEF CPUX64}CPUX64 {$ELSE}CPUX32 {$ENDIF};

  { Address Mode }
  am16 = $01; { 16-bit addressing mode }
  am32 = $02; { 32-bit addressing mode }
  am64 = $03; { 64-bit addressing mode }
  { Default Addressing Mode Depending on CPUX (32/64)bit }
  DefAddressMode: array [0 .. 1] of Byte = (am32, am64);
  { Used to select Addressing Mode when Address Mode Prefix is used ! }
  AddressMode: array [0 .. 1] of Byte = (am16, am32);

  { Tables }
  tbOneByte = $01; { One Byte OpCodes Table }
  tbTwoByte = $02; { Two Byte OpCodes Table }
  tbThreeByte = $03; { Three Byte OpCodes Table }
  tbFPU = $04; { FPU OpCodes Table }

  { Prefixs }
  Prf_Seg_CS = $01;
  Prf_Seg_DS = $02;
  Prf_Seg_ES = $04;
  Prf_Seg_GS = $08;
  Prf_Seg_FS = $10;
  Prf_Seg_SS = $20;
  Prf_OpSize = $40;
  Prf_AddrSize = $80;
  Prf_Lock = $100;
  Prf_Repe = $200;
  Prf_Repne = $400;
  Prf_Rex = $800;
  Prf_VEX = $1000;
  Prf_Vex2 = Prf_VEX or $2000;
  Prf_Vex3 = Prf_VEX or $4000;

  { Segment Registers }
  Seg_CS = $01;
  Seg_DS = $02;
  Seg_ES = $03;
  Seg_GS = $04;
  Seg_FS = $05;
  Seg_SS = $06;

  { OpSize }
  ops8bits = $01;
  ops16bits = $02;
  ops32bits = $04;
  ops48bits = $06;
  ops64bits = $08;
  ops128bits = $10;
  ops256bits = $20;
  ops512bits = $40;

  { OpType }
  otNone = $00;
  otRET = $01; { RET Instruction }
  otCALL = $02; { CALL Instruction }
  otJMP = $04; { JMP Instruction }
  otJ = $08;
  otJcc = $10; { Conditional JUMP Instruction }

  { OpKind }
  kGrp = $01;
  // kFPU = $02; Use OpTable !

  { Options }
  DecodeVex = $01;

  { ModRm Flags }
  mfUsed = $80; { ModRm Used }

  { Sib Flags }
  sfUsed = $01; { Sib Used }

  { Displacement Flags }
  dfUsed = $01; { Disp Used }
  dfRip = $02; { RIP Disp }
  dfSigned = $04; { Displacement can be signed ! }
  dfDispOnly = $08; { Displacement Only without registers ! }
  dfOffset = $10; { Offset coded after the opcode. }

  { Immediat Flags }
  imfUsed = $01; { Imm Used }

  { Branch Flags }
  bfUsed = $01; { JUMP/CALL Used }
  bfRel = $02; { Relative Branch }
  bfAbs = $04; { Absolute Branch }
  bfIndirect = $08; { Indirect Branch }
  bfReg = $10;
  bfFar = bfAbs or $20; { Far Branch }
  bfRip = $40;

  { Operand Flags }
  opdD64 = $01;
  opdF64 = $02;
  opdDf64 = $03;
  opdDv64 = $04;

  { Options }
  UseVA = $01;

  { General Purpose Registers }
  rEAX = $00;
  rECX = $01;
  rEDX = $02;
  rEBX = $03;
  rESP = $04;
  rEBP = $05;
  rESI = $06;
  rEDI = $07;

  { Error }
  NO_ERROR = $00;
  INVALID_CPUX = $01;
  INVALID_ADDRESS = $02;
  INVALID_INSTRUCTION_LENGTH = $04;
  ERROR_DISP_SIZE = $08;
  ERROR_IMM_SIZE = $10;
  ERROR_VEX_ESCAPE = $20;
  INVALID_GROUP_OPCODE = $40;

  UnknownErrorStr = 'Unknown Error';
  InstErrorsStr: array [0 .. 7] of String = (
    { NO_ERROR }
    'No error',
    { INVALID_CPUX }
    'Invalid cpux',
    { INVALID_ADDRESS }
    'Invalid address',
    { INVALID_INSTRUCTION_LENGTH }
    'Invalid instruction length',
    { ERROR_DISP_SIZE }
    'Invalid displacement size',
    { ERROR_IMM_SIZE }
    'Invalid immediat size',
    { ERROR_VEX_ESCAPE }
    'Invalid vex mmmmm field',
    { INVALID_GROUP_OPCODE }
    'Invalid group opcode');

  _vex3_ = $03;
  _opcode_ = $01;
  _modrm_ = $01;
  _sib_ = $01;
  _disp32_ = $04;
  _imm32_ = $04;
  _imm64_ = $08;
  { Intel define instruction length as a 15 bytes !
    However , it's possible to incode instructions
    that exceed the defined length !
  }
  MAX_INST_LENGTH_X32 = _vex3_ + _opcode_ + _modrm_ + _sib_ + _disp32_ + _imm32_;
  MAX_INST_LENGTH_X64 = _vex3_ + _opcode_ + _modrm_ + _sib_ + _disp32_ + _imm64_;
  CPUX_TO_INST_LENGTH: array [0 .. 1] of ShortInt = (MAX_INST_LENGTH_X32, MAX_INST_LENGTH_X64);
{$IFDEF CPUX64}
  MAX_INST_LENGTH_N = MAX_INST_LENGTH_X64;
{$ELSE !CPUX64}
  MAX_INST_LENGTH_N = MAX_INST_LENGTH_X32;
{$ENDIF CPUX64}

var
  { Raise Exception When Error Occurs ! }
  RaiseExceptionOnError: Boolean = True;

type
  InstException = class(Exception);

  TModRM = record
    iMod: Byte; { ModRm.Mod Field }
    Reg: Byte; { ModRm.Reg Field }
    Rm: Byte; { ModRm.Rm Field }
    Value: Byte; { ModRm Value }
    { ModRm.Flags => See ModRmFlagsTable.inc }
    Flags: Byte;
  end;

  PModRM = ^TModRM;
  LPModRM = PModRM;

  TSib = record
    Scale: Byte; { SIB.Scale Field }
    Index: Byte; { Register Index }
    Base: Byte; { Register Base }
    Value: Byte; { SIB Value }
    Flags: Byte; { SIB Flags }
  end;

  PSib = ^TSib;
  LPSib = PSib;

  TImmediat = record
    Size: Byte; { Size of Immediat => opsxxxbits }
    Value: Int64; { Immediat Value }
    Flags: Byte; { Sets of imfxxx }
  end;

  PImmediat = ^TImmediat;

  TDisplacement = record
    Size: Byte; { Size of Displacement => opsxxxbits }
    Value: Int64; { Displacement Value }
    Flags: Byte; { Sets of dfxxx }
  end;

  PDisplacement = ^TDisplacement;

  TBranch = record
    Size: Byte;
    Value: Int64;
    Target: PByte; { Destination Address }
    Falgs: Byte; { Sets of bfxxx }
  end;

  PBranch = ^TBranch;

  TRex = record
    R: Boolean; { REX.R Field }
    X: Boolean; { REX.X Field }
    B: Boolean; { REX.B Field }
    W: Boolean; { REX.W Field }
    Value: Byte; { REX Value = [$40..$4F] }
  end;

  PRex = ^TRex;

  TVex = record
    {
      ==================> N.B <==================
      1 => ALL FIELD ARE IN NO INVERTED FORM !
      2 => VEX.[R,X,B & W] ARE ACCESSIBLE THROUGH REX FIELD !

    }
    vvvv: Byte; { VEX.vvvv ==> Vector Register }
    L: Boolean; { VEX.L ==> You should use VL instead ! }
    PP: Byte; { VEX.PP ==> Implied Mandatory Prefixes }
    mmmmm: Byte; { VEX.mmmmm ==> Implied Escape }
    VL: Byte; { Vector Length }
  end;

  PVex = ^TVex;

  TInternalData = record
    MndPrf: Byte; { Mandatory Prefix }
    zOpSize: Byte; { word or dword depending on opsize prefix ! }
    vOpSize: Byte; { word or dword or qword depending on opsize & REX prefix ! }
  end;

  PInternalData = ^TInternalData;

  TInstruction = record
    Archi: Byte; { CPUX32 or CPUX64 ! }
    AddrMode: Byte; { Address Mode }
    Addr: PByte;
    VirtualAddr: PByte;
    NextInst: PByte; { Pointer to the Next Instruction }
    OpCode: Byte; { OpCode Value }
    OpType: Byte;
    OpKind: Byte;
    OpTable: Byte; { tbOneByte,tbTwoByte,... }
    OperandFlags: Byte;
    Prefixes: Word; { Sets of Prf_xxx }
    ModRm: TModRM;
    Sib: TSib;
    Disp: TDisplacement;
    Imm: TImmediat; { Primary Immediat }
    ImmEx: TImmediat; { Secondary Immediat if used ! }
    Branch: TBranch; { JMP & CALL }
    SegReg: Byte; { Segment Register }
    Rex: TRex;
    Vex: TVex;
    LID: TInternalData; { Internal Data }
    Errors: Byte;
    InstSize: Integer;
    Options: Byte;
    UserTag: NativeInt;
  end;

  PInstruction = ^TInstruction;

  TDecoderProc = procedure(PInst: PInstruction);

function DecodeInst(PInst: PInstruction): Integer;

{ Useful ModRm Routines }
function GetModRm_Mod(const Value: Byte): Byte; {$IFDEF MustInline}inline; {$ENDIF}
function GetModRm_Reg(const Value: Byte): Byte; {$IFDEF MustInline}inline; {$ENDIF}
function GetModRm_Rm(const Value: Byte): Byte; {$IFDEF MustInline}inline; {$ENDIF}
{ Useful Sib Routines }
function GetSib_Base(const Value: Byte): Byte; {$IFDEF MustInline}inline; {$ENDIF}
function GetSib_Index(const Value: Byte): Byte; {$IFDEF MustInline}inline; {$ENDIF}
function GetSib_Scale(const Value: Byte): Byte; {$IFDEF MustInline}inline; {$ENDIF}
function IsSibBaseRegValid(PInst: PInstruction): Boolean; {$IFDEF MustInline}inline; {$ENDIF}

implementation

{$I OpCodesTables.inc}
{$I ModRmFlagsTables.inc}
{ ================================== 00 ================================== }
procedure Decode_InvalidOpCode(PInst: PInstruction); forward;
{ ================================== 01 ================================== }
procedure Decode_NA_ModRm(PInst: PInstruction); forward;
{ ================================== 02 ================================== }
procedure Decode_NA_Ib(PInst: PInstruction); forward;
{ ================================== 03 ================================== }
procedure Decode_NA_Iz(PInst: PInstruction); forward;
{ ================================== 04 ================================== }
procedure Decode_NA_I64(PInst: PInstruction); forward;
{ ================================== 05 ================================== }
procedure Decode_Escape_2_Byte(PInst: PInstruction); forward;
{ ================================== 06 ================================== }
procedure Decode_ES_Prefix(PInst: PInstruction); forward;
{ ================================== 07 ================================== }
procedure Decode_CS_Prefix(PInst: PInstruction); forward;
{ ================================== 08 ================================== }
procedure Decode_SS_Prefix(PInst: PInstruction); forward;
{ ================================== 09 ================================== }
procedure Decode_DS_Prefix(PInst: PInstruction); forward;
{ ================================== 10 ================================== }
procedure Decode_REX_Prefix(PInst: PInstruction); forward;
{ ================================== 11 ================================== }
procedure Decode_NA_D64(PInst: PInstruction); forward;
{ ================================== 12 ================================== }
procedure Decode_NA_ModRm_I64(PInst: PInstruction); forward;
{ ================================== 13 ================================== }
procedure Decode_FS_Prefix(PInst: PInstruction); forward;
{ ================================== 14 ================================== }
procedure Decode_GS_Prefix(PInst: PInstruction); forward;
{ ================================== 15 ================================== }
procedure Decode_OPSIZE_Prefix(PInst: PInstruction); forward;
{ ================================== 16 ================================== }
procedure Decode_ADSIZE_Prefix(PInst: PInstruction); forward;
{ ================================== 17 ================================== }
procedure Decode_NA_Iz_D64(PInst: PInstruction); forward;
{ ================================== 18 ================================== }
procedure Decode_NA_ModRm_Iz(PInst: PInstruction); forward;
{ ================================== 19 ================================== }
procedure Decode_NA_Ib_D64(PInst: PInstruction); forward;
{ ================================== 20 ================================== }
procedure Decode_NA_ModRm_Ib(PInst: PInstruction); forward;
{ ================================== 21 ================================== }
procedure Decode_NA(PInst: PInstruction); forward;
{ ================================== 22 ================================== }
procedure Decode_NA_Jb_Df64(PInst: PInstruction); forward;
{ ================================== 23 ================================== }
procedure Decode_Group_1(PInst: PInstruction); forward;
{ ================================== 24 ================================== }
procedure Decode_Group_1A(PInst: PInstruction); forward;
{ ================================== 25 ================================== }
procedure Decode_NA_CALL_Ap_I64(PInst: PInstruction); forward;
{ ================================== 26 ================================== }
procedure Decode_NA_OfstV(PInst: PInstruction); forward;
{ ================================== 27 ================================== }
procedure Decode_NA_Iv(PInst: PInstruction); forward;
{ ================================== 28 ================================== }
procedure Decode_Group_2(PInst: PInstruction); forward;
{ ================================== 29 ================================== }
procedure Decode_NA_RET_Iw_Df64(PInst: PInstruction); forward;
{ ================================== 30 ================================== }
procedure Decode_NA_RET_Df64(PInst: PInstruction); forward;
{ ================================== 31 ================================== }
procedure Decode_VEX3_Prefix(PInst: PInstruction); forward;
{ ================================== 32 ================================== }
procedure Decode_VEX2_Prefix(PInst: PInstruction); forward;
{ ================================== 33 ================================== }
procedure Decode_Group_11(PInst: PInstruction); forward;
{ ================================== 34 ================================== }
procedure Decode_NA_Iw_Ib_D64(PInst: PInstruction); forward;
{ ================================== 35 ================================== }
procedure Decode_NA_RET_Iw(PInst: PInstruction); forward;
{ ================================== 36 ================================== }
procedure Decode_NA_RET(PInst: PInstruction); forward;
{ ================================== 37 ================================== }
procedure Decode_NA_Ib_I64(PInst: PInstruction); forward;
{ ================================== 38 ================================== }
procedure Decode_Escape_FPU_D8(PInst: PInstruction); forward;
{ ================================== 39 ================================== }
procedure Decode_Escape_FPU_D9(PInst: PInstruction); forward;
{ ================================== 40 ================================== }
procedure Decode_Escape_FPU_DA(PInst: PInstruction); forward;
{ ================================== 41 ================================== }
procedure Decode_Escape_FPU_DB(PInst: PInstruction); forward;
{ ================================== 42 ================================== }
procedure Decode_Escape_FPU_DC(PInst: PInstruction); forward;
{ ================================== 43 ================================== }
procedure Decode_Escape_FPU_DD(PInst: PInstruction); forward;
{ ================================== 44 ================================== }
procedure Decode_Escape_FPU_DE(PInst: PInstruction); forward;
{ ================================== 45 ================================== }
procedure Decode_Escape_FPU_DF(PInst: PInstruction); forward;
{ ================================== 46 ================================== }
procedure Decode_NA_CALL_Jz_Df64(PInst: PInstruction); forward;
{ ================================== 47 ================================== }
procedure Decode_NA_JMP_Jz_Df64(PInst: PInstruction); forward;
{ ================================== 48 ================================== }
procedure Decode_NA_JMP_Ap_I64(PInst: PInstruction); forward;
{ ================================== 49 ================================== }
procedure Decode_NA_JMP_Jb_Df64(PInst: PInstruction); forward;
{ ================================== 50 ================================== }
procedure Decode_LOCK_Prefix(PInst: PInstruction); forward;
{ ================================== 51 ================================== }
procedure Decode_REPNE_Prefix(PInst: PInstruction); forward;
{ ================================== 52 ================================== }
procedure Decode_REPE_Prefix(PInst: PInstruction); forward;
{ ================================== 53 ================================== }
procedure Decode_Group_3(PInst: PInstruction); forward;
{ ================================== 54 ================================== }
procedure Decode_Group_4_INC_DEC(PInst: PInstruction); forward;
{ ================================== 55 ================================== }
procedure Decode_Group_5_INC_DEC(PInst: PInstruction); forward;
{ ================================== 56 ================================== }
procedure Decode_Group_6(PInst: PInstruction); forward;
{ ================================== 57 ================================== }
procedure Decode_Group_7(PInst: PInstruction); forward;
{ ================================== 58 ================================== }
procedure Decode_NA_CALL(PInst: PInstruction); forward;
{ ================================== 59 ================================== }
procedure Decode_NA_66_F2_F3_ModRm(PInst: PInstruction); forward;
{ ================================== 60 ================================== }
procedure Decode_NA_66_ModRm(PInst: PInstruction); forward;
{ ================================== 61 ================================== }
procedure Decode_NA_66_F3_ModRm(PInst: PInstruction); forward;
{ ================================== 62 ================================== }
procedure Decode_Group_16(PInst: PInstruction); forward;
{ ================================== 63 ================================== }
procedure Decode_NA_ModRm_F64(PInst: PInstruction); forward;
{ ================================== 64 ================================== }
procedure Decode_Escape_3_Byte(PInst: PInstruction); forward;
{ ================================== 65 ================================== }
procedure Decode_NA_F3_ModRm(PInst: PInstruction); forward;
{ ================================== 66 ================================== }
procedure Decode_66_ModRm(PInst: PInstruction); forward;
{ ================================== 67 ================================== }
procedure Decode_NA_66_F2_F3_ModRm_Ib(PInst: PInstruction); forward;
{ ================================== 68 ================================== }
procedure Decode_Group_12(PInst: PInstruction); forward;
{ ================================== 69 ================================== }
procedure Decode_Group_13(PInst: PInstruction); forward;
{ ================================== 70 ================================== }
procedure Decode_Group_14(PInst: PInstruction); forward;
{ ================================== 71 ================================== }
procedure Decode_66_F2_ModRm(PInst: PInstruction); forward;
{ ================================== 72 ================================== }
procedure Decode_NA_Jz_Df64(PInst: PInstruction); forward;
{ ================================== 73 ================================== }
procedure Decode_Group_15(PInst: PInstruction); forward;
{ ================================== 74 ================================== }
procedure Decode_F3_ModRm(PInst: PInstruction); forward;
{ ================================== 75 ================================== }
procedure Decode_Group_10_UD2(PInst: PInstruction); forward;
{ ================================== 76 ================================== }
procedure Decode_Group_8(PInst: PInstruction); forward;
{ ================================== 77 ================================== }
procedure Decode_NA_66_ModRm_Ib(PInst: PInstruction); forward;
{ ================================== 78 ================================== }
procedure Decode_Group_9(PInst: PInstruction); forward;
{ ================================== 79 ================================== }
procedure Decode_66_F2_F3_ModRm(PInst: PInstruction); forward;
{ ================================== 80 ================================== }
procedure Decode_F2_ModRm(PInst: PInstruction); forward;
{ ================================== 81 ================================== }
procedure Decode_SP_T38_F0_F7(PInst: PInstruction); forward;
{ ================================== 82 ================================== }
procedure Decode_66_ModRm_Ib(PInst: PInstruction); forward;
{ ================================== 83 ================================== }
procedure Decode_F2_ModRm_Ib(PInst: PInstruction); forward;

procedure JumpError(PInst: PInstruction); forward;
procedure JumpToTableTwoByte(PInst: PInstruction); forward;
procedure JumpToTableThreeByte_38(PInst: PInstruction); forward;
procedure JumpToTableThreeByte_3A(PInst: PInstruction); forward;

procedure Decode_CALL_ModRm(PInst: PInstruction); forward;
procedure Decode_JMP_ModRm(PInst: PInstruction); forward;
procedure Decode_CALL_Mp(PInst: PInstruction); forward;
procedure Decode_JMP_Mp(PInst: PInstruction); forward;

const

  { Convert PP To Mandatory Prefixes ! }
  PPToMndPrf: array [0 .. 3] of Byte = ($00, $66, $F3, $F2);

  { Convert LL To OpSize ! }
  LLToOpSize: array [0 .. 3] of Word = (ops128bits, ops256bits, ops512bits, 0);

  { Call escaping procedure ! }
  mmmmmToEscProc: array [0 .. 4] of TDecoderProc = ( //
    JumpError, { }
    JumpToTableTwoByte, { 00001: implied 0F leading opcode byte }
    JumpToTableThreeByte_38, { 00010: implied 0F 38 leading opcode bytes }
    JumpToTableThreeByte_3A, { 00011: implied 0F 3A leading opcode bytes }
    JumpError { }
    );

  DecoderProcTable: array [0 .. $54 - 1] of TDecoderProc = ( //
    { 00 } Decode_InvalidOpCode,
    { 01 } Decode_NA_ModRm,
    { 02 } Decode_NA_Ib,
    { 03 } Decode_NA_Iz,
    { 04 } Decode_NA_I64,
    { 05 } Decode_Escape_2_Byte,
    { 06 } Decode_ES_Prefix,
    { 07 } Decode_CS_Prefix,
    { 08 } Decode_SS_Prefix,
    { 09 } Decode_DS_Prefix,
    { 10 } Decode_REX_Prefix,
    { 11 } Decode_NA_D64,
    { 12 } Decode_NA_ModRm_I64,
    { 13 } Decode_FS_Prefix,
    { 14 } Decode_GS_Prefix,
    { 15 } Decode_OPSIZE_Prefix,
    { 16 } Decode_ADSIZE_Prefix,
    { 17 } Decode_NA_Iz_D64,
    { 18 } Decode_NA_ModRm_Iz,
    { 19 } Decode_NA_Ib_D64,
    { 20 } Decode_NA_ModRm_Ib,
    { 21 } Decode_NA,
    { 22 } Decode_NA_Jb_Df64,
    { 23 } Decode_Group_1,
    { 24 } Decode_Group_1A,
    { 25 } Decode_NA_CALL_Ap_I64,
    { 26 } Decode_NA_OfstV,
    { 27 } Decode_NA_Iv,
    { 28 } Decode_Group_2,
    { 29 } Decode_NA_RET_Iw_Df64,
    { 30 } Decode_NA_RET_Df64,
    { 31 } Decode_VEX3_Prefix,
    { 32 } Decode_VEX2_Prefix,
    { 33 } Decode_Group_11,
    { 34 } Decode_NA_Iw_Ib_D64,
    { 35 } Decode_NA_RET_Iw,
    { 36 } Decode_NA_RET,
    { 37 } Decode_NA_Ib_I64,
    { 38 } Decode_Escape_FPU_D8,
    { 39 } Decode_Escape_FPU_D9,
    { 40 } Decode_Escape_FPU_DA,
    { 41 } Decode_Escape_FPU_DB,
    { 42 } Decode_Escape_FPU_DC,
    { 43 } Decode_Escape_FPU_DD,
    { 44 } Decode_Escape_FPU_DE,
    { 45 } Decode_Escape_FPU_DF,
    { 46 } Decode_NA_CALL_Jz_Df64,
    { 47 } Decode_NA_JMP_Jz_Df64,
    { 48 } Decode_NA_JMP_Ap_I64,
    { 49 } Decode_NA_JMP_Jb_Df64,
    { 50 } Decode_LOCK_Prefix,
    { 51 } Decode_REPNE_Prefix,
    { 52 } Decode_REPE_Prefix,
    { 53 } Decode_Group_3,
    { 54 } Decode_Group_4_INC_DEC,
    { 55 } Decode_Group_5_INC_DEC,
    { 56 } Decode_Group_6,
    { 57 } Decode_Group_7,
    { 58 } Decode_NA_CALL,
    { 59 } Decode_NA_66_F2_F3_ModRm,
    { 60 } Decode_NA_66_ModRm,
    { 61 } Decode_NA_66_F3_ModRm,
    { 62 } Decode_Group_16,
    { 63 } Decode_NA_ModRm_F64,
    { 64 } Decode_Escape_3_Byte,
    { 65 } Decode_NA_F3_ModRm,
    { 66 } Decode_66_ModRm,
    { 67 } Decode_NA_66_F2_F3_ModRm_Ib,
    { 68 } Decode_Group_12,
    { 69 } Decode_Group_13,
    { 70 } Decode_Group_14,
    { 71 } Decode_66_F2_ModRm,
    { 72 } Decode_NA_Jz_Df64,
    { 73 } Decode_Group_15,
    { 74 } Decode_F3_ModRm,
    { 75 } Decode_Group_10_UD2,
    { 76 } Decode_Group_8,
    { 77 } Decode_NA_66_ModRm_Ib,
    { 78 } Decode_Group_9,
    { 79 } Decode_66_F2_F3_ModRm,
    { 80 } Decode_F2_ModRm,
    { 81 } Decode_SP_T38_F0_F7,
    { 82 } Decode_66_ModRm_Ib,
    { 83 } Decode_F2_ModRm_Ib);
  { .$REGION 'COMMON' }
  { ========================== COMMON =============================== }

procedure SetInstError(PInst: PInstruction; Error: Byte);
var
  ErrStr: String;
begin
  ErrStr := EmptyStr;
  if Error = NO_ERROR then
  begin
    { Clear Errors ! }
    PInst^.Errors := NO_ERROR;
    Exit;
  end;
  PInst^.Errors := PInst^.Errors or Error;

  if RaiseExceptionOnError then
  begin
    if (Error > 0) and (Error < Length(InstErrorsStr)) then
      ErrStr := InstErrorsStr[Error]
    else
      ErrStr := UnknownErrorStr;
    raise InstException.Create(Format('Error %d : %s.', [Error, ErrStr]));
  end;
end;

function GetModRm_Mod(const Value: Byte): Byte;
begin
  Result := Value shr 6;
end;

function GetModRm_Reg(const Value: Byte): Byte;
begin
  Result := (Value and $38) shr $03;
end;

function GetModRm_Rm(const Value: Byte): Byte;
begin
  Result := Value and 7;
end;

function GetSib_Base(const Value: Byte): Byte;
begin
  Result := Value and 7;
end;

function GetSib_Index(const Value: Byte): Byte;
begin
  Result := (Value and $38) shr $03;
end;

function GetSib_Scale(const Value: Byte): Byte;
begin
  Result := (1 shl (Value shr 6));
end;

function IsSibBaseRegValid(PInst: PInstruction): Boolean;
begin
  Result := True;
  if PInst^.Sib.Flags and sfUsed <> 0 then
    Result := not((PInst^.ModRm.iMod = 0) and (PInst^.Sib.Base = 5));
end;

procedure SetOpCode(PInst: PInstruction); {$IFDEF MustInline}inline; {$ENDIF}
begin
  PInst^.OpCode := PInst^.NextInst^;
  Inc(PInst^.NextInst);
end;

procedure SetGroup(PInst: PInstruction); {$IFDEF MustInline}inline; {$ENDIF}
begin
  PInst^.OpKind := kGrp;
end;

procedure ForceOpSize(PInst: PInstruction); {$IFDEF MustInline}inline; {$ENDIF}
begin
  if PInst^.Archi = CPUX32 then
    Exit;
  PInst^.LID.vOpSize := ops64bits;
end;

procedure DecodeSib(PInst: PInstruction); {$IFDEF MustInline}inline; {$ENDIF}
var
  PSib: LPSib;
begin
  PSib := @PInst^.Sib;
  PSib.Flags := sfUsed;
  PSib.Value := PInst^.NextInst^;
  PSib.Base := GetSib_Base(PSib.Value);
  PSib.Index := GetSib_Index(PSib.Value);
  PSib.Scale := GetSib_Scale(PSib.Value);
  Inc(PInst^.NextInst); // Skip SIB !
end;

procedure DecodeDisp(PInst: PInstruction);
var
  Disp: Int64;
  Size: Byte;
  DispOnly: Boolean;
begin
  Disp := $00;
  Size := PInst^.Disp.Size;
  PInst^.Disp.Flags := dfUsed;
  DispOnly := (PInst^.ModRm.iMod = $00) and (PInst^.ModRm.Rm = $05);

  case Size of
    ops8bits:
      Disp := (PInt8(PInst^.NextInst)^); // and $FF;
    ops16bits:
      Disp := (PInt16(PInst^.NextInst)^); // and $FFFF;
    ops32bits:
      begin
        Disp := (PInt32(PInst^.NextInst)^); // and $FFFFFFFF;
        if (PInst^.Archi = CPUX64) and DispOnly then
          { RIP disp ! }
          PInst^.Disp.Flags := PInst^.Disp.Flags or dfRip;
      end;
  else
    SetInstError(PInst, ERROR_DISP_SIZE);
  end;

  if DispOnly then
    PInst^.Disp.Flags := PInst^.Disp.Flags or dfDispOnly
  else
    PInst^.Disp.Flags := PInst^.Disp.Flags or dfSigned;

  PInst^.Disp.Value := Disp;
  Inc(PInst^.NextInst, Size) // Skip Disp !
end;

procedure Decode_ModRm(PInst: PInstruction);
var
  PModRM: LPModRM;
  SibUsed: Boolean;
const
  { Get Disp Size from ModRm . }
  ModRMFlagsToDispSize: array [0 .. 4] of Byte = (0, ops8bits, ops16bits, 0, ops32bits);
begin
  PModRM := @PInst^.ModRm;
  PModRM.Value := PInst^.NextInst^;
  PModRM.iMod := GetModRm_Mod(PModRM.Value);
  PModRM.Reg := GetModRm_Reg(PModRM.Value);
  PModRM.Rm := GetModRm_Rm(PModRM.Value);
  PModRM.Flags := ModRMFlags[PInst^.AddrMode][PModRM.Value];

  PInst^.Disp.Size := ModRMFlagsToDispSize[(PModRM.Flags shr 1) and 7];
  Inc(PInst^.NextInst); // Skip ModRM !

  SibUsed := (PModRM.Flags and $10 > 0); { SibUsed ! }

  if SibUsed then
  begin
    DecodeSib(PInst);
    { if the base is not valid ==> there is a disp32 .
      But the disp can be 8bit ==> we need to check first
      if the disp does not exist !
    }
    if (PInst^.Disp.Size = 0) and (not IsSibBaseRegValid(PInst)) then
      PInst^.Disp.Size := ops32bits;
  end;

  if PInst^.Disp.Size > 0 then
    DecodeDisp(PInst);

  { ModRm Exists ! }
  PModRM.Flags := PModRM.Flags or mfUsed;
end;

procedure Decode_Imm(PInst: PInstruction; immSize: Byte);
var
  Imm: Int64;
  PImm: PImmediat;
begin
  Imm := $00;
  case immSize of
    ops8bits:
      Imm := (PInt8(PInst^.NextInst)^);
    ops16bits:
      Imm := (PInt16(PInst^.NextInst)^);
    ops32bits:
      Imm := (PInt32(PInst^.NextInst)^);
    ops64bits:
      Imm := (PInt64(PInst^.NextInst)^);
  else
    SetInstError(PInst, ERROR_IMM_SIZE);
  end;

  {
    If Imm field already used => get the extra Imm
  }
  if PInst^.Imm.Flags and imfUsed <> $00 then
    PImm := @PInst^.ImmEx
  else
    PImm := @PInst^.Imm;

  PImm.Flags := imfUsed;
  PImm.Value := Imm;
  PImm.Size := immSize;
  Inc(PInst^.NextInst, immSize); // Skip Immediat !
end;

procedure Decode_J(PInst: PInstruction; Size: Byte);
var
  Value: Int64;
  VA: PByte;
begin
  Value := $00;
  case Size of
    ops8bits:
      Value := (PInt8(PInst^.NextInst)^);
    ops16bits:
      Value := (PInt16(PInst^.NextInst)^);
    ops32bits:
      Value := (PInt32(PInst^.NextInst)^);
    ops64bits:
      Value := (PInt64(PInst^.NextInst)^);
  end;
  Inc(PInst^.NextInst, Size);
  if PInst^.OpType = otNone then
    PInst^.OpType := otJ;
  if PInst^.OpCode in [$70 .. $8F] then
    PInst^.OpType := otJ or otJcc;
  if Assigned(PInst^.VirtualAddr) then
    VA := PByte(NativeInt(PInst^.VirtualAddr) + NativeInt(NativeInt(PInst^.NextInst) - NativeInt(PInst^.Addr)))
  else
    VA := PInst^.NextInst;
  PInst^.Branch.Size := Size;
  PInst^.Branch.Falgs := bfUsed or bfRel;
  PInst^.Branch.Value := Value;
  PInst^.Branch.Target := PByte(NativeInt(VA) + Value);
end;

procedure Decode_Branch_ModRm(PInst: PInstruction);
var
  P: PByte;
  VA: PByte;
begin
  SetOpCode(PInst);
  Decode_ModRm(PInst);
  PInst^.Branch.Value := PInst^.Disp.Value;
  PInst^.Branch.Size := PInst^.Disp.Size;
  PInst^.Branch.Falgs := bfUsed or bfIndirect or bfAbs;
  if Assigned(PInst^.VirtualAddr) then
    VA := PByte(NativeInt(PInst^.VirtualAddr) + (NativeInt(PInst^.NextInst) - NativeInt(PInst^.Addr)))
  else
    VA := PInst^.NextInst;
  if (PInst^.ModRm.iMod = $00) and (PInst^.ModRm.Rm = $05) then
  begin
    { Memory = Displacement }
    if PInst^.Archi = CPUX64 then
    begin
      if PInst^.Prefixes and Prf_AddrSize <> 0 then
        { Displacement = EIP + Offset }
        VA := PByte(UInt64(VA) and $FFFFFFFF);
      { Displacement = RIP + Offset }
      PInst^.Branch.Falgs := PInst^.Branch.Falgs or bfRip;
      P := PByte(NativeInt(VA) + NativeInt(PInst^.Disp.Value));
      { Memory 64-bits }
      PInst^.Branch.Target := PByte(PUInt64(P)^);
    end
    else
    begin
      { No RIP }
      P := PByte(UInt32(PInst^.Disp.Value));
      if PInst^.Prefixes and Prf_OpSize <> 0 then
        { Memory 16-bits }
        PInst^.Branch.Target := PByte(PUInt16(P)^)
      else
        { Memory 32-bits }
        PInst^.Branch.Target := PByte(PUInt32(P)^);
    end;
  end
  else
  begin
    { Memory = Displacement + Register }
    PInst^.Branch.Falgs := PInst^.Branch.Falgs or bfReg;
    PInst^.Branch.Target := nil;
  end;
end;

procedure Decode_Ap(PInst: PInstruction);
begin
  SetOpCode(PInst);
  PInst^.Branch.Falgs := bfUsed or bfFar;
  { We must clear the upper word ! }
  PInst^.Branch.Value := PUInt64(PInst^.NextInst)^ and $FFFFFFFFFFFF;
  PInst^.Branch.Size := ops48bits;
  PInst^.Branch.Target := nil;
  Inc(PInst^.NextInst, ops48bits);
end;

procedure Decode_Mp(PInst: PInstruction);
begin
  SetOpCode(PInst);
  PInst^.Branch.Falgs := bfUsed or bfFar;
  Decode_ModRm(PInst);
  PInst^.Branch.Value := PInst^.Disp.Value;
  PInst^.Branch.Size := PInst^.Disp.Size;
  PInst^.Branch.Target := nil;
end;

procedure Decode_InvalidOpCode(PInst: PInstruction); {$IFDEF MustInline}inline;
{$ENDIF}
begin
  SetOpCode(PInst);
end;

procedure Decode_Invalid_Group(PInst: PInstruction); {$IFDEF MustInline}inline;
{$ENDIF}
begin
  SetOpCode(PInst);
  Inc(PInst^.NextInst);
end;

procedure Decode_Invalid_FPU(PInst: PInstruction); {$IFDEF MustInline}inline;
{$ENDIF}
begin
  SetOpCode(PInst);
  Inc(PInst^.NextInst);
end;

{ .$ENDREGION }
{ .$REGION 'PREFIXES' }
{ ========================== PREFIXES =============================== }

procedure Decode_ES_Prefix(PInst: PInstruction);
begin
  { ES Segment Override Prefix }
  Inc(PInst^.NextInst);
  PInst^.Prefixes := PInst^.Prefixes or Prf_Seg_ES;
  PInst^.SegReg := Seg_ES;
  DecoderProcTable[OneByteTable[PInst^.NextInst^]](PInst);
end;

procedure Decode_CS_Prefix(PInst: PInstruction);
begin
  { CS Segment Override Prefix }
  Inc(PInst^.NextInst);
  PInst^.Prefixes := PInst^.Prefixes or Prf_Seg_CS;
  PInst^.SegReg := Seg_CS;
  DecoderProcTable[OneByteTable[PInst^.NextInst^]](PInst);
end;

procedure Decode_SS_Prefix(PInst: PInstruction);
begin
  { SS Segment Override Prefix }
  Inc(PInst^.NextInst);
  PInst^.Prefixes := PInst^.Prefixes or Prf_Seg_SS;
  PInst^.SegReg := Seg_SS;
  DecoderProcTable[OneByteTable[PInst^.NextInst^]](PInst);
end;

procedure Decode_DS_Prefix(PInst: PInstruction);
begin
  { DS Segment Override Prefix }
  Inc(PInst^.NextInst);
  PInst^.Prefixes := PInst^.Prefixes or Prf_Seg_DS;
  PInst^.SegReg := Seg_DS;
  DecoderProcTable[OneByteTable[PInst^.NextInst^]](PInst);
end;

procedure Decode_REX_Prefix(PInst: PInstruction);
begin
  { REX Prefix valid only on PM64! }
  if PInst^.Archi = CPUX32 then
  begin
    { INC/DEC REG }
    Decode_NA(PInst);
    Exit;
  end;
  PInst^.Prefixes := PInst^.Prefixes or Prf_Rex;
  PInst^.Rex.Value := PInst^.NextInst^;
  PInst^.Rex.B := (PInst^.Rex.Value and 1 <> 0);
  PInst^.Rex.X := (PInst^.Rex.Value and 2 <> 0);
  PInst^.Rex.R := (PInst^.Rex.Value and 4 <> 0);
  PInst^.Rex.W := (PInst^.Rex.Value and 8 <> 0);

  if PInst^.Rex.W then
    PInst^.LID.vOpSize := ops64bits;

  Inc(PInst^.NextInst); // Skip Rex .
  DecoderProcTable[OneByteTable[PInst^.NextInst^]](PInst);
end;

procedure Decode_FS_Prefix(PInst: PInstruction);
begin
  { FS Segment Override Prefix }
  Inc(PInst^.NextInst);
  PInst^.Prefixes := PInst^.Prefixes or Prf_Seg_FS;
  PInst^.SegReg := Seg_FS;
  DecoderProcTable[OneByteTable[PInst^.NextInst^]](PInst);
end;

procedure Decode_GS_Prefix(PInst: PInstruction);
begin
  { GS Segment Override Prefix }
  Inc(PInst^.NextInst);
  PInst^.Prefixes := PInst^.Prefixes or Prf_Seg_GS;
  PInst^.SegReg := Seg_GS;
  DecoderProcTable[OneByteTable[PInst^.NextInst^]](PInst);
end;

procedure Decode_OPSIZE_Prefix(PInst: PInstruction);
begin
  PInst^.Prefixes := PInst^.Prefixes or Prf_OpSize;
  PInst^.LID.vOpSize := ops16bits;
  PInst^.LID.zOpSize := ops16bits;
  PInst^.LID.MndPrf := $66;
  Inc(PInst^.NextInst);
  DecoderProcTable[OneByteTable[PInst^.NextInst^]](PInst);
end;

procedure Decode_ADSIZE_Prefix(PInst: PInstruction);
begin
  PInst^.Prefixes := PInst^.Prefixes or Prf_AddrSize;
  Inc(PInst^.NextInst);
  PInst^.AddrMode := AddressMode[PInst^.Archi];
  DecoderProcTable[OneByteTable[PInst^.NextInst^]](PInst);
end;

procedure Decode_VEX3_Prefix(PInst: PInstruction);
var
  P: Byte;
  Q: PByte;
  R, X: Boolean;
begin
  if PInst^.Options and DecodeVex = 0 then
  begin
    Decode_NA_ModRm(PInst);
    Exit;
  end;
  if PInst^.Archi = CPUX32 then
  begin
    Q := PInst^.NextInst;
    Inc(Q);
    R := (Q^ and $80 <> 0);
    X := (Q^ and $40 <> 0);
    {
      if R & X are set ==> Vex prefix is valid !
      otherwise the instruction is LES .
    }
    if not(R and X) then
    begin
      { LES instruction }
      Decode_NA_ModRm(PInst);
      Exit;
    end;
  end;

  Inc(PInst^.NextInst); // Skip $C4 !
  PInst^.Prefixes := PInst^.Prefixes or Prf_Vex3;
  P := PInst^.NextInst^; // P0
  PInst^.Rex.R := not(P and $80 <> 0);
  PInst^.Rex.X := not(P and $40 <> 0);
  PInst^.Rex.B := not(P and $20 <> 0);
  PInst^.Vex.mmmmm := (P and $1F);
  Inc(PInst^.NextInst); // Skip P0
  P := PInst^.NextInst^; // P1
  Inc(PInst^.NextInst); // Skip P1
  PInst^.Rex.W := (P and $80 <> 0);
  PInst^.Vex.vvvv := $0F - ((P and $78) shr 3);
  PInst^.Vex.L := (P and 4 <> 0);
  PInst^.Vex.PP := (P and 3);
  PInst^.Vex.VL := LLToOpSize[Byte(PInst^.Vex.L)];
  PInst^.LID.MndPrf := PPToMndPrf[PInst^.Vex.PP];
  mmmmmToEscProc[PInst^.Vex.mmmmm](PInst);
end;

procedure Decode_VEX2_Prefix(PInst: PInstruction);
var
  P: Byte;
  Q: PByte;
  R: Boolean;
begin
  if PInst^.Options and DecodeVex = 0 then
  begin
    Decode_NA_ModRm(PInst);
    Exit;
  end;
  if PInst^.Archi = CPUX32 then
  begin
    Q := PInst^.NextInst;
    Inc(Q);
    R := (Q^ and $80 <> 0);
    {
      if R is set ==> Vex prefix is valid !
      otherwise the instruction is LDS.
    }
    if not R then
    begin
      { LDS instruction }
      Decode_NA_ModRm(PInst);
      Exit;
    end;
  end;

  Inc(PInst^.NextInst); // Skip $C5 !
  PInst^.Prefixes := PInst^.Prefixes or Prf_Vex2;
  P := PInst^.NextInst^;
  PInst^.Rex.R := not(P and $80 <> 0);
  PInst^.Vex.vvvv := $0F - ((P and $78) shr 3);
  PInst^.Vex.L := (P and 4 <> 0);
  PInst^.Vex.PP := (P and 3);
  PInst^.Vex.VL := LLToOpSize[Byte(PInst^.Vex.L)];
  PInst^.LID.MndPrf := PPToMndPrf[PInst^.Vex.PP];
  Inc(PInst^.NextInst); // Skip P0 !
  DecoderProcTable[TwoByteTable[PInst^.NextInst^]](PInst);
end;

procedure Decode_LOCK_Prefix(PInst: PInstruction);
begin
  PInst^.Prefixes := PInst^.Prefixes or Prf_Lock;
  Inc(PInst^.NextInst);
  DecoderProcTable[OneByteTable[PInst^.NextInst^]](PInst);
end;

procedure Decode_REPNE_Prefix(PInst: PInstruction);
begin
  PInst^.Prefixes := PInst^.Prefixes or Prf_Repne;
  PInst^.LID.MndPrf := $F2;
  Inc(PInst^.NextInst);
  DecoderProcTable[OneByteTable[PInst^.NextInst^]](PInst);
end;

procedure Decode_REPE_Prefix(PInst: PInstruction);
begin
  PInst^.Prefixes := PInst^.Prefixes or Prf_Repe;
  PInst^.LID.MndPrf := $F3;
  Inc(PInst^.NextInst);
  DecoderProcTable[OneByteTable[PInst^.NextInst^]](PInst);
end;
{ .$ENDREGION }
{ .$REGION 'ESCAPE' }
{ ========================== ESCAPE =============================== }

procedure JumpError(PInst: PInstruction);
begin
  { Wrong Vex.mmmmm value ! }
  SetInstError(PInst, ERROR_VEX_ESCAPE);
end;

procedure JumpToTableTwoByte(PInst: PInstruction);
begin
  { Implied $0F OpCode => Jump to table TwoByteTable }
  PInst^.OpTable := tbTwoByte;
  DecoderProcTable[TwoByteTable[PInst^.NextInst^]](PInst);
end;

procedure JumpToTableThreeByte_38(PInst: PInstruction);
begin
  { Implied $0F$38 OpCodes => Jump to table ThreeByteTable.38 }
  PInst^.OpTable := tbThreeByte;
  DecoderProcTable[ThreeByteTable38[PInst^.NextInst^]](PInst);
end;

procedure JumpToTableThreeByte_3A(PInst: PInstruction);
begin
  { Implied $0F$3A OpCodes => Jump to table ThreeByteTable.3A }
  PInst^.OpTable := tbThreeByte;
  DecoderProcTable[ThreeByteTable3A[PInst^.NextInst^]](PInst);
end;

procedure Decode_Escape_2_Byte(PInst: PInstruction);
begin
  { Two Byte OpCode Escape ! }
  PInst^.OpTable := tbTwoByte;
  Inc(PInst^.NextInst);
  DecoderProcTable[TwoByteTable[PInst^.NextInst^]](PInst);
end;

procedure Decode_Escape_3_Byte(PInst: PInstruction);
var
  P: Byte;
begin
  { Three Byte OpCode Escape ! }
  PInst^.OpTable := tbThreeByte;
  P := PInst^.NextInst^;
  Inc(PInst^.NextInst);
  if P = $38 then
    DecoderProcTable[ThreeByteTable38[PInst^.NextInst^]](PInst)
  else
    DecoderProcTable[ThreeByteTable3A[PInst^.NextInst^]](PInst);
end;

{ .$ENDREGION }
{ .$REGION 'FPU' }
{ ========================== FPU =============================== }

procedure Decode_Escape_FPU_D8(PInst: PInstruction);
begin
  { All OpCode are valid for $D8 ! }
  PInst^.OpTable := tbFPU;
  Decode_NA_ModRm(PInst);
end;

procedure Decode_Escape_FPU_D9(PInst: PInstruction);
var
  P: PByte;
  ModRm: Byte;
  Reg: Byte;
begin
  PInst^.OpTable := tbFPU;
  P := PInst^.NextInst;
  Inc(P);
  ModRm := P^;
  Reg := GetModRm_Reg(ModRm);
  if ModRm < $C0 then
  begin
    if Reg = $01 then
    begin
      Decode_Invalid_FPU(PInst);
      Exit;
    end;
  end
  else
  begin
    if ModRm in [$D1 .. $DF, $E2, $E3, $EF] then
    begin
      Decode_Invalid_FPU(PInst);
      Exit;
    end;
  end;
  Decode_NA_ModRm(PInst);
end;

procedure Decode_Escape_FPU_DA(PInst: PInstruction);
var
  P: PByte;
  ModRm: Byte;
begin
  PInst^.OpTable := tbFPU;
  P := PInst^.NextInst;
  Inc(P);
  ModRm := P^;
  if ModRm > $C0 then
  begin
    if ModRm in [$E0 .. $E8, $EA .. $EF, $F0 .. $FF] then
    begin
      Decode_Invalid_FPU(PInst);
      Exit;
    end;
  end;
  Decode_NA_ModRm(PInst);
end;

procedure Decode_Escape_FPU_DB(PInst: PInstruction);
var
  P: PByte;
  ModRm: Byte;
  Reg: Byte;
begin
  PInst^.OpTable := tbFPU;
  P := PInst^.NextInst;
  Inc(P);
  ModRm := P^;
  Reg := GetModRm_Reg(ModRm);
  if ModRm < $C0 then
  begin
    if (Reg = $04) or (Reg = $06) then
    begin
      Decode_Invalid_FPU(PInst);
      Exit;
    end;
  end
  else
  begin
    if ModRm in [$E0, $E1, $E4 .. $E7, $F8 .. $FF] then
    begin
      Decode_Invalid_FPU(PInst);
      Exit;
    end;
  end;
  Decode_NA_ModRm(PInst);
end;

procedure Decode_Escape_FPU_DC(PInst: PInstruction);
var
  P: PByte;
  ModRm: Byte;
begin
  PInst^.OpTable := tbFPU;
  P := PInst^.NextInst;
  Inc(P);
  ModRm := P^;
  if ModRm > $C0 then
  begin
    if ModRm in [$D0 .. $DF] then
    begin
      Decode_Invalid_FPU(PInst);
      Exit;
    end;
  end;
  Decode_NA_ModRm(PInst);
end;

procedure Decode_Escape_FPU_DD(PInst: PInstruction);
var
  P: PByte;
  ModRm: Byte;
  Reg: Byte;
begin
  PInst^.OpTable := tbFPU;
  P := PInst^.NextInst;
  Inc(P);
  ModRm := P^;
  Reg := GetModRm_Reg(ModRm);
  if ModRm < $C0 then
  begin
    if (Reg = $05) then
    begin
      Decode_Invalid_FPU(PInst);
      Exit;
    end;
  end
  else
  begin
    if ModRm in [$C8 .. $CF, $F0 .. $FF] then
    begin
      Decode_Invalid_FPU(PInst);
      Exit;
    end;
  end;
  Decode_NA_ModRm(PInst);
end;

procedure Decode_Escape_FPU_DE(PInst: PInstruction);
var
  P: PByte;
  ModRm: Byte;
begin
  PInst^.OpTable := tbFPU;
  P := PInst^.NextInst;
  Inc(P);
  ModRm := P^;
  if ModRm > $C0 then
  begin
    if ModRm in [$D0 .. $D8, $DA .. $DF] then
    begin
      Decode_Invalid_FPU(PInst);
      Exit;
    end;
  end;
  Decode_NA_ModRm(PInst);
end;

procedure Decode_Escape_FPU_DF(PInst: PInstruction);
var
  P: PByte;
  ModRm: Byte;
begin
  PInst^.OpTable := tbFPU;
  P := PInst^.NextInst;
  Inc(P);
  ModRm := P^;
  if ModRm > $C0 then
  begin
    if ModRm in [$C0 .. $CF, $D0 .. $DF, $E1 .. $E7, $F8 .. $FF] then
    begin
      Decode_Invalid_FPU(PInst);
      Exit;
    end;
  end;
  Decode_NA_ModRm(PInst);
end;

{ .$ENDREGION }
{ .$REGION 'GROUPS' }
{ ========================== GROUPS =============================== }

procedure Decode_Group_1(PInst: PInstruction);
begin
  SetGroup(PInst);
  if not(PInst^.NextInst^ in [$80 .. $83]) then
    SetInstError(PInst, INVALID_GROUP_OPCODE);
  if PInst^.NextInst^ = $81 then
    Decode_NA_ModRm_Iz(PInst)
  else
    Decode_NA_ModRm_Ib(PInst);
end;

procedure Decode_Group_1A(PInst: PInstruction);
var
  P: PByte;
  Reg: Byte;
begin
  SetGroup(PInst);
  if (PInst^.NextInst^ <> $8F) then
    SetInstError(PInst, INVALID_GROUP_OPCODE);
  P := PInst^.NextInst;
  Inc(P); // ModRm !
  Reg := GetModRm_Reg(P^);
  if (Reg = $00) then
  begin
    Decode_NA_ModRm(PInst);
    Exit;
  end;
  Decode_Invalid_Group(PInst);
end;

procedure Decode_Group_2(PInst: PInstruction);
begin
  SetGroup(PInst);
  if not(PInst^.NextInst^ in [$C0 .. $C1, $D0 .. $D3]) then
    SetInstError(PInst, INVALID_GROUP_OPCODE);
  if (PInst^.NextInst^ in [$C0, $C1]) then
    Decode_NA_ModRm_Ib(PInst)
  else
    Decode_NA_ModRm(PInst);
end;

procedure Decode_Group_3(PInst: PInstruction);
var
  P: PByte;
  Reg: Byte;
begin
  SetGroup(PInst);
  if not(PInst^.NextInst^ in [$F6, $F7]) then
    SetInstError(PInst, INVALID_GROUP_OPCODE);
  P := PInst^.NextInst;
  Inc(P);
  Reg := GetModRm_Reg(P^);
  if (Reg < $02) then
  begin
    { [TEST Reg,Immb] & [TEST Reg,Immz] }
    if PInst^.NextInst^ = $F6 then
      Decode_NA_ModRm_Ib(PInst)
    else
      Decode_NA_ModRm_Iz(PInst);
    Exit;
  end;
  Decode_NA_ModRm(PInst);
end;

procedure Decode_Group_4_INC_DEC(PInst: PInstruction);
var
  P: PByte;
  Reg: Byte;
begin
  SetGroup(PInst);
  Assert(PInst^.NextInst^ = $FE);
  P := PInst^.NextInst;
  Inc(P); // ModRm
  Reg := GetModRm_Reg(P^);
  if (Reg < $02) then
  begin
    { INC/DEC REG }
    Decode_NA_ModRm(PInst);
    Exit;
  end;
  Decode_Invalid_Group(PInst);
end;

procedure Decode_Group_5_INC_DEC(PInst: PInstruction);
var
  Reg: Byte;
  P: PByte;
const
  GroupProc: array [0 .. 7] of TDecoderProc = ( //
    { 00 } Decode_NA_ModRm, { INC Ev }
    { 01 } Decode_NA_ModRm, { DEC Ev }
    { 02 } Decode_CALL_ModRm, { CALL Ev }
    { 03 } Decode_CALL_Mp, { CALL Mp }
    { 04 } Decode_JMP_ModRm, { JMP Ev }
    { 05 } Decode_JMP_Mp, { JMP Mp }
    { 06 } Decode_NA_ModRm, { PUSH Ev }
    { 07 } Decode_Invalid_Group { InvalidOpCode }
    );
begin
  SetGroup(PInst);
  if (PInst^.NextInst^ <> $FF) then
    SetInstError(PInst, INVALID_GROUP_OPCODE);
  P := PInst^.NextInst;
  Inc(P); // ModRm
  Reg := GetModRm_Reg(P^);
  GroupProc[Reg](PInst);
end;

procedure Decode_Group_6(PInst: PInstruction);
var
  P: PByte;
  Reg: Byte;
begin
  SetGroup(PInst);
  if (PInst^.OpTable <> tbTwoByte) and (PInst^.NextInst^ <> $00) then
    SetInstError(PInst, INVALID_GROUP_OPCODE);
  P := PInst^.NextInst;
  Inc(P);
  Reg := GetModRm_Reg(P^);
  if Reg = $07 then
  begin
    Decode_Invalid_Group(PInst);
    Exit;
  end;
  Decode_NA_ModRm(PInst);
end;

procedure Decode_Group_7(PInst: PInstruction);
var
  P: PByte;
  iMod, Reg: Byte;
begin
  SetGroup(PInst);
  if (PInst^.OpTable <> tbTwoByte) and (PInst^.NextInst^ <> $01) then
    SetInstError(PInst, INVALID_GROUP_OPCODE);
  P := PInst^.NextInst;
  Inc(P);
  iMod := GetModRm_Mod(P^);
  Reg := GetModRm_Reg(P^);
  if (Reg = $04) or (Reg = $06) then
  begin
    Decode_NA_ModRm(PInst);
    Exit;
  end
  else if Reg = $05 then
  begin
    Decode_Invalid_Group(PInst);
    Exit;
  end;
  if iMod <> $03 then
  begin
    Decode_NA_ModRm(PInst);
    Exit;
  end;
  Decode_Invalid_Group(PInst);
end;

procedure Decode_Group_8(PInst: PInstruction);
var
  P: PByte;
  Reg: Byte;
begin
  SetGroup(PInst);
  if (PInst^.OpTable <> tbTwoByte) and (PInst^.NextInst^ <> $BA) then
    SetInstError(PInst, INVALID_GROUP_OPCODE);
  P := PInst^.NextInst;
  Inc(P);
  Reg := GetModRm_Reg(P^);
  if Reg > $03 then
  begin
    Decode_NA_ModRm_Ib(PInst);
    Exit;
  end;
  Decode_Invalid_Group(PInst);
end;

procedure Decode_Group_9(PInst: PInstruction);
var
  P: PByte;
  iMod, Reg: Byte;
begin
  SetGroup(PInst);
  if (PInst^.OpTable <> tbTwoByte) and (PInst^.NextInst^ <> $C7) then
    SetInstError(PInst, INVALID_GROUP_OPCODE);
  P := PInst^.NextInst;
  Inc(P);
  iMod := GetModRm_Mod(P^);
  Reg := GetModRm_Reg(P^);
  if (iMod = $03) and (Reg > $05) then
  begin
    Decode_NA_ModRm(PInst);
    Exit;
  end;

  if (iMod <> $03) then
  begin
    { Mod = Mem }
    if (((PInst^.LID.MndPrf = $00) and (Reg = $01)) or //
      ((PInst^.LID.MndPrf = $66) and (Reg = $06)) or //
      ((PInst^.LID.MndPrf = $F3) and (Reg > $05))) then
    begin
      Decode_NA_ModRm(PInst);
      Exit;
    end;
  end;
  Decode_Invalid_Group(PInst);
end;

procedure Decode_Group_10_UD2(PInst: PInstruction);
begin
  SetGroup(PInst);
  Decode_InvalidOpCode(PInst);
end;

procedure Decode_Group_11(PInst: PInstruction);
var
  P: PByte;
  Reg: Byte;
begin
  SetGroup(PInst);
  if not(PInst^.NextInst^ in [$C6, $C7]) then
    SetInstError(PInst, INVALID_GROUP_OPCODE);
  P := PInst^.NextInst;
  Inc(P);
  Reg := GetModRm_Reg(P^);
  if PInst^.NextInst^ = $C6 then
  begin
    if (Reg = $00) then
    begin
      { XABORT Instruction }
      Decode_NA_ModRm_Ib(PInst);
      Exit;
    end
    else if (Reg = $07) then
    begin
      Decode_NA_Ib(PInst);
      Exit;
    end
  end
  else if PInst^.NextInst^ = $C7 then
  begin
    if Reg = $00 then
    begin
      Decode_NA_ModRm_Iz(PInst);
      Exit;
    end
    else if Reg = $07 then
    begin
      { XBEGIN Instruction }
      SetOpCode(PInst);
      Inc(PInst^.NextInst);
      Decode_J(PInst, PInst^.LID.zOpSize);
      PInst^.OpType := $00;
      Exit;
    end;
  end;
  Decode_Invalid_Group(PInst);
end;

procedure Decode_Group_12(PInst: PInstruction);
var
  P: PByte;
  iMod, Reg: Byte;
begin
  SetGroup(PInst);
  { Group 12 & 13 }
  if (PInst^.OpTable <> tbTwoByte) and not(PInst^.NextInst^ in [$71, $72]) then
    SetInstError(PInst, INVALID_GROUP_OPCODE);
  P := PInst^.NextInst;
  Inc(P);
  iMod := GetModRm_Mod(P^);
  Reg := GetModRm_Reg(P^);
  if (iMod = $03) and (Reg in [$02, $04, $06]) then
  begin
    Decode_NA_ModRm_Ib(PInst);
    Exit;
  end;
  Decode_Invalid_Group(PInst);
end;

procedure Decode_Group_13(PInst: PInstruction);
begin
  SetGroup(PInst);
  { Group 13 has the same instructions signature as Group 12 ! }
  Decode_Group_12(PInst);
end;

procedure Decode_Group_14(PInst: PInstruction);
var
  P: PByte;
  iMod, Reg: Byte;
begin
  SetGroup(PInst);
  if (PInst^.OpTable <> tbTwoByte) and (PInst^.NextInst^ <> $73) then
    SetInstError(PInst, INVALID_GROUP_OPCODE);
  P := PInst^.NextInst;
  Inc(P);
  iMod := GetModRm_Mod(P^);
  Reg := GetModRm_Reg(P^);
  if iMod = $03 then
  begin
    if (Reg = $02) or (Reg = $06) then
    begin
      Decode_NA_ModRm_Ib(PInst);
      Exit;
    end;
    if (PInst^.LID.MndPrf = $66) and ((Reg = $03) or (Reg = $07)) then
    begin
      Decode_NA_ModRm_Ib(PInst);
      Exit;
    end;
  end;
  Decode_Invalid_Group(PInst);
end;

procedure Decode_Group_15(PInst: PInstruction);
var
  P: PByte;
  iMod, Reg: Byte;
begin
  SetGroup(PInst);
  if (PInst^.OpTable <> tbTwoByte) and (PInst^.NextInst^ <> $AE) then
    SetInstError(PInst, INVALID_GROUP_OPCODE);
  P := PInst^.NextInst;
  Inc(P);
  iMod := GetModRm_Mod(P^);
  Reg := GetModRm_Reg(P^);
  if (iMod = $03) and (PInst^.LID.MndPrf = $F3) and (Reg < $04) then
  begin
    Decode_NA_ModRm(PInst);
    Exit;
  end;
  Decode_Invalid_Group(PInst);
end;

procedure Decode_Group_16(PInst: PInstruction);
var
  P: PByte;
  iMod, Reg: Byte;
begin
  SetGroup(PInst);
  if (PInst^.OpTable <> tbTwoByte) and (PInst^.NextInst^ <> $18) then
    SetInstError(PInst, INVALID_GROUP_OPCODE);
  P := PInst^.NextInst;
  Inc(P);
  iMod := GetModRm_Mod(P^);
  Reg := GetModRm_Reg(P^);
  if (iMod <> $03) and (Reg < $04) then
  begin
    { Prefetch group instructions. }
    Decode_NA_ModRm(PInst);
    Exit;
  end;
  Decode_Invalid_Group(PInst);
end;

procedure Decode_Group_17(PInst: PInstruction);
var
  P: PByte;
  Reg: Byte;
begin
  SetGroup(PInst);
  P := PInst^.NextInst;
  Inc(P);
  Reg := GetModRm_Reg(P^);
  if (Reg > $00) and (Reg < $04) then
  begin
    Decode_NA_ModRm(PInst);
    Exit;
  end;
  Decode_Invalid_Group(PInst);
end;

{ .$ENDREGION }
{ .$REGION 'DECODERS' }
{ ========================== DECODERS PROC =============================== }

procedure Decode_NA_CALL_Ap_I64(PInst: PInstruction);
begin
  { Instruction is only valid for x32 ! }
  if PInst^.Archi = CPUX64 then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  PInst^.OpType := otCALL;
  Decode_Ap(PInst);
end;

procedure Decode_NA_ModRm(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $00 }
  if ((PInst^.OpTable <> tbOneByte) and not(PInst^.LID.MndPrf = $00)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  Decode_ModRm(PInst);
end;

procedure Decode_NA_Ib(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $00 }
  if ((PInst^.OpTable <> tbOneByte) and not(PInst^.LID.MndPrf = $00)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  Decode_Imm(PInst, ops8bits);
end;

procedure Decode_NA_Iz(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $00 }
  if ((PInst^.OpTable <> tbOneByte) and not(PInst^.LID.MndPrf = $00)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  Decode_Imm(PInst, PInst^.LID.zOpSize);
end;

procedure Decode_NA_I64(PInst: PInstruction);
begin
  { Instruction is invalid on PM64 }
  { Only valid when mandatory prefix is : $00 }
  if (PInst^.Archi = CPUX64) or ((PInst^.OpTable <> tbOneByte) and not(PInst^.LID.MndPrf = $00)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
end;

procedure Decode_NA(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $00 }
  if ((PInst^.OpTable <> tbOneByte) and not(PInst^.LID.MndPrf = $00)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
end;

procedure Decode_NA_ModRm_I64(PInst: PInstruction);
begin
  { Instruction is invalid on PM64 }
  { Only valid when mandatory prefix is : $00 }
  if (PInst^.Archi = CPUX64) or ((PInst^.OpTable <> tbOneByte) and not(PInst^.LID.MndPrf = $00)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  Decode_ModRm(PInst);
end;

procedure Decode_NA_ModRm_Iz(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $00 }
  if ((PInst^.OpTable <> tbOneByte) and not(PInst^.LID.MndPrf = $00)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  Decode_ModRm(PInst);
  Decode_Imm(PInst, PInst^.LID.zOpSize);
end;

procedure Decode_NA_ModRm_Ib(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $00 }
  if ((PInst^.OpTable <> tbOneByte) and not(PInst^.LID.MndPrf = $00)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  Decode_ModRm(PInst);
  Decode_Imm(PInst, ops8bits);
end;

procedure Decode_NA_Jb_Df64(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $00 }
  if ((PInst^.OpTable <> tbOneByte) and (PInst^.LID.MndPrf <> $00)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  PInst^.OperandFlags := opdDf64;
  Decode_J(PInst, ops8bits);
end;

procedure Decode_NA_RET(PInst: PInstruction);
begin
  SetOpCode(PInst);
  PInst^.OpType := otRET;
  if PInst^.OpCode in [$C2, $CA] then
    Decode_Imm(PInst, ops16bits);
end;

procedure Decode_NA_Ib_I64(PInst: PInstruction);
begin
  { Instruction is invalid on PM64 }
  { Only valid when mandatory prefix is : $00 }
  if (PInst^.Archi = CPUX64) or ((PInst^.OpTable <> tbOneByte) and not(PInst^.LID.MndPrf = $00)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  Decode_Imm(PInst, ops8bits);
end;

procedure Decode_NA_CALL_Jz_Df64(PInst: PInstruction);
begin
  SetOpCode(PInst);
  PInst^.OpType := otCALL;
  PInst^.OperandFlags := opdDf64;
  Decode_J(PInst, PInst^.LID.zOpSize);
end;

procedure Decode_NA_JMP_Jz_Df64(PInst: PInstruction);
begin
  SetOpCode(PInst);
  PInst^.OpType := otJMP;
  PInst^.OperandFlags := opdDf64;
  Decode_J(PInst, PInst^.LID.zOpSize);
end;

procedure Decode_NA_JMP_Ap_I64(PInst: PInstruction);
begin
  if PInst^.Archi = CPUX64 then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  PInst^.OpType := otJMP;
  Decode_Ap(PInst);
end;

procedure Decode_NA_JMP_Jb_Df64(PInst: PInstruction);
begin
  SetOpCode(PInst);
  PInst^.OpType := otJMP;
  PInst^.OperandFlags := opdDf64;
  Decode_J(PInst, ops8bits);
end;

procedure Decode_CALL_ModRm(PInst: PInstruction);
begin
  PInst^.OpType := otCALL;
  Decode_Branch_ModRm(PInst);
end;

procedure Decode_CALL_Mp(PInst: PInstruction);
begin
  PInst^.OpType := otCALL;
  Decode_Mp(PInst);
end;

procedure Decode_JMP_ModRm(PInst: PInstruction);
begin
  PInst^.OpType := otJMP;
  Decode_Branch_ModRm(PInst);
end;

procedure Decode_JMP_Mp(PInst: PInstruction);
begin
  PInst^.OpType := otJMP;
  Decode_Mp(PInst);
end;

procedure Decode_NA_CALL(PInst: PInstruction);
begin
  { SYSCALL! }
  SetOpCode(PInst);
end;

procedure Decode_NA_66_F2_F3_ModRm(PInst: PInstruction);
begin
  SetOpCode(PInst);
  Decode_ModRm(PInst);
end;

procedure Decode_NA_66_ModRm(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $00 or $66 }
  if ((PInst^.OpTable <> tbOneByte) and not(PInst^.LID.MndPrf in [$00, $66])) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  Decode_ModRm(PInst);
end;

procedure Decode_NA_66_F3_ModRm(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $00 or $66 or $F3 }
  if ((PInst^.OpTable <> tbOneByte) and (PInst^.LID.MndPrf = $F2)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  Decode_ModRm(PInst);
end;

procedure Decode_NA_F3_ModRm(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $00 or $F3 }
  if ((PInst^.OpTable <> tbOneByte) and not(PInst^.LID.MndPrf in [$00, $F3])) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  Decode_ModRm(PInst);
end;

procedure Decode_66_ModRm(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $66 }
  if ((PInst^.OpTable <> tbOneByte) and not(PInst^.LID.MndPrf = $66)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  Decode_ModRm(PInst);
end;

procedure Decode_NA_66_F2_F3_ModRm_Ib(PInst: PInstruction);
begin
  SetOpCode(PInst);
  Decode_ModRm(PInst);
  Decode_Imm(PInst, ops8bits);
end;

procedure Decode_66_F2_ModRm(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $66 or $F2 }
  if ((PInst^.OpTable <> tbOneByte) and not(PInst^.LID.MndPrf in [$66, $F2])) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  Decode_ModRm(PInst);
end;

procedure Decode_F3_ModRm(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $F3 }
  if ((PInst^.OpTable <> tbOneByte) and not(PInst^.LID.MndPrf = $F3)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  Decode_ModRm(PInst);
end;

procedure Decode_NA_66_ModRm_Ib(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $00 or $66 }
  if ((PInst^.OpTable <> tbOneByte) and not(PInst^.LID.MndPrf in [$00, $66])) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  Decode_ModRm(PInst);
  Decode_Imm(PInst, ops8bits);
end;

procedure Decode_66_F2_F3_ModRm(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $66 or $F2 or $F3 }
  if ((PInst^.OpTable <> tbOneByte) and (PInst^.LID.MndPrf = $00)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  Decode_ModRm(PInst);
end;

procedure Decode_F2_ModRm(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $F2 }
  if ((PInst^.OpTable <> tbOneByte) and not(PInst^.LID.MndPrf = $F2)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  Decode_ModRm(PInst);
end;

procedure Decode_SP_T38_F0_F7(PInst: PInstruction);
var
  Prf66F2: Boolean;
begin
  if PInst^.NextInst^ = $F3 then
  begin
    Decode_Group_17(PInst);
    Exit;
  end;

  { 66 & F2 }
  Prf66F2 := PInst^.Prefixes and (Prf_OpSize or Prf_Repne) = (Prf_OpSize or Prf_Repne);

  if Prf66F2 then
  begin
    { Valid only for CRC32 instruction ! }
    if (PInst^.NextInst^ = $F0) or (PInst^.NextInst^ = $F1) then
    begin
      Decode_NA_ModRm(PInst);
      Exit;
    end
    else
    begin
      Decode_InvalidOpCode(PInst);
      Exit;
    end;
  end
  else if PInst^.LID.MndPrf = $00 then
  begin
    if (PInst^.NextInst^ = $F4) or (PInst^.NextInst^ = $F6) then
    begin
      Decode_InvalidOpCode(PInst);
      Exit;
    end;
  end
  else if PInst^.LID.MndPrf = $66 then
  begin
    if (PInst^.NextInst^ in [$F2 .. $F5]) then
    begin
      Decode_InvalidOpCode(PInst);
      Exit;
    end;
  end
  else if PInst^.LID.MndPrf = $F3 then
  begin
    if (PInst^.NextInst^ < $F5) then
    begin
      Decode_InvalidOpCode(PInst);
      Exit;
    end;
  end
  else if PInst^.LID.MndPrf = $F2 then
  begin
    if (PInst^.NextInst^ in [$F2 .. $F4]) then
    begin
      Decode_InvalidOpCode(PInst);
      Exit;
    end;
  end;
  Decode_NA_ModRm(PInst);
end;

procedure Decode_66_ModRm_Ib(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $66 }
  if ((PInst^.OpTable <> tbOneByte) and not(PInst^.LID.MndPrf = $66)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  Decode_ModRm(PInst);
  Decode_Imm(PInst, ops8bits);
end;

procedure Decode_F2_ModRm_Ib(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $F2 }
  if ((PInst^.OpTable <> tbOneByte) and not(PInst^.LID.MndPrf = $F2)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  Decode_ModRm(PInst);
  Decode_Imm(PInst, ops8bits);
end;

procedure Decode_NA_RET_Iw_Df64(PInst: PInstruction);
begin
  PInst^.OpType := otRET;
  SetOpCode(PInst);
  PInst^.OperandFlags := opdDf64;
  Decode_Imm(PInst, ops16bits);
end;

procedure Decode_NA_D64(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $00 }
  if ((PInst^.OpTable <> tbOneByte) and (PInst^.LID.MndPrf <> $00)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  PInst^.OperandFlags := opdD64;
end;

procedure Decode_NA_Iz_D64(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $00 }
  if ((PInst^.OpTable <> tbOneByte) and (PInst^.LID.MndPrf <> $00)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  PInst^.OperandFlags := opdD64;
  Decode_Imm(PInst, PInst^.LID.zOpSize);
end;

procedure Decode_NA_Ib_D64(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $00 }
  if ((PInst^.OpTable <> tbOneByte) and (PInst^.LID.MndPrf <> $00)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  PInst^.OperandFlags := opdD64;
  Decode_Imm(PInst, ops8bits);
end;

procedure Decode_NA_RET_Df64(PInst: PInstruction);
begin
  PInst^.OpType := otRET;
  PInst^.OperandFlags := opdDf64;
  SetOpCode(PInst);
end;

procedure Decode_NA_RET_Iw(PInst: PInstruction);
begin
  PInst^.OpType := otRET;
  SetOpCode(PInst);
  Decode_Imm(PInst, ops16bits);
end;

procedure Decode_NA_Iw_Ib_D64(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $00 }
  if ((PInst^.OpTable <> tbOneByte) and (PInst^.LID.MndPrf <> $00)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  PInst^.OperandFlags := opdD64;
  Decode_Imm(PInst, ops16bits);
  Decode_Imm(PInst, ops8bits);
end;

procedure Decode_NA_ModRm_F64(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $00 }
  if ((PInst^.OpTable <> tbOneByte) and (PInst^.LID.MndPrf <> $00)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  PInst^.OperandFlags := opdF64;
  Decode_ModRm(PInst);
end;

procedure Decode_NA_Jz_Df64(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $00 }
  if ((PInst^.OpTable <> tbOneByte) and (PInst^.LID.MndPrf <> $00)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  PInst^.OperandFlags := opdDf64;
  Decode_J(PInst, PInst^.LID.zOpSize);
end;

procedure Decode_NA_OfstV(PInst: PInstruction);
begin
  SetOpCode(PInst);
  Decode_Imm(PInst, PInst.LID.vOpSize);
  PInst.Disp.Value := PInst.Imm.Value;
  PInst.Disp.Size := PInst.Imm.Size;
  PInst.Disp.Flags := dfUsed or dfOffset;
  PInst.Imm.Size := $00;
  PInst.Imm.Value := $00;
  PInst.Imm.Flags := $00;
end;

procedure Decode_NA_Iv(PInst: PInstruction);
begin
  { Only valid when mandatory prefix is : $00 }
  if ((PInst^.OpTable <> tbOneByte) and not(PInst^.LID.MndPrf = $00)) then
  begin
    Decode_InvalidOpCode(PInst);
    Exit;
  end;
  SetOpCode(PInst);
  Decode_Imm(PInst, PInst^.LID.vOpSize);
end;
{ .$ENDREGION }

function DecodeInst(PInst: PInstruction): Integer;
var
  P: PByte;
  LArchi: Byte;
  LAddr: PByte;
  LErrors: Byte;
  LVA: PByte;
  LOptions: Byte;
begin
  { No Errors }
  SetInstError(PInst, NO_ERROR);

  if not(PInst^.Archi in [CPUX32, CPUX64]) then
    SetInstError(PInst, INVALID_CPUX);

  if not Assigned(PInst^.Addr) then
    SetInstError(PInst, INVALID_ADDRESS);

  { Init Instruction Structure }
  LArchi := PInst^.Archi;
  LAddr := PInst^.Addr;
  LVA := PInst^.VirtualAddr;
  LErrors := PInst^.Errors;
  LOptions := PInst^.Options;

  FillChar(PInst^, SizeOf(TInstruction), #0);

  PInst^.Archi := LArchi;
  PInst^.Addr := LAddr;
  PInst^.Errors := LErrors;
  PInst^.VirtualAddr := LVA;
  PInst.Options := LOptions;

  P := PInst^.Addr;
  PInst^.NextInst := P;

  PInst^.LID.zOpSize := ops32bits;
  PInst^.LID.vOpSize := ops32bits;
  PInst^.AddrMode := DefAddressMode[PInst^.Archi];

  { Default Op Table is One Byte ! }
  PInst^.OpTable := tbOneByte;

  DecoderProcTable[OneByteTable[P^]](PInst);
  Result := Integer(NativeInt(PInst^.NextInst) - NativeInt(P));
  PInst^.InstSize := Result;

  if Result > CPUX_TO_INST_LENGTH[PInst^.Archi] then
    SetInstError(PInst, INVALID_INSTRUCTION_LENGTH);
end;

end.
