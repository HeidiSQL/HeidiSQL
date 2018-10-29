// **************************************************************************************************
// This file (OpCodesTables.inc) is a part of the InstDecode Library .
// File OpCodesTables
// http://code.google.com/p/delphi-detours-library/

// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is OpCodesTables.inc .
//
// The Initial Developer of the Original Code is Mahdi Safsafi [SMP3].
// Portions created by Mahdi Safsafi . are Copyright (C) 2013-2014 Mahdi Safsafi .
// All Rights Reserved.
//
// **************************************************************************************************

{
  Reference : ref.x86asm.net

}
const
  { OpCode Flags }
  Opf_NONE = $0;
  Opf_W = $1; // bit w (bit index 0, operand size) is present.
  Opf_D = $2; // bit d (bit index 1, Direction) is present.
  Opf_S = $4; // bit s (bit index 1, Sign-extend) is present.
  Opf_L = $8; // Instruction can use lock prefix .
  Opf_ModRM = $10; // Instruction use ModRM .
  Opf_ModRMExt = Opf_ModRM or $20; // Instruction use ModRMExt.
  Opf_Jump = $40;
  { ------------------->Operand Flags <------------------ }
  {
    A  =  Direct address. The instruction has no ModR/M byte; the address of the operand is encoded in the instruction; no base register, index register, or scaling factor can be applied (for example, far JMP (EA)).
    E  =  ModR/M specifies the operand (general-purpose register or a memory address) .
    G  =  general-purpose register .
    J  = The instruction contains a relative offset to be added to the instruction pointer register (for example, JMP ).
    M  =  The ModR/M byte may refer only to memory: mod <> 11bin .
    O  =  The instruction has no ModR/M byte; the offset of the operand is coded as a word, double word or quad word (depending on address size attribute) in the instruction. No base register, index register, or scaling factor can be applied (only MOV  (A0, A1, A2, A3)).
    S  =  The reg field of the ModR/M byte selects a segment register (only MOV (8C, 8E)).
    U  =  The R/M field of the ModR/M byte selects a 128-bit XMM register.
    D  =  The reg field of the ModR/M byte selects a debug register (only MOV (0F21, 0F23)).
    R  =  The mod field of the ModR/M byte may refer only to a general register .
    T  =  The reg field of the ModR/M byte selects a test register (only MOV (0F24, 0F26)).
    Z  =  The instruction has no ModR/M byte; the three least-significant bits of the opcode byte selects a general-purpose register .

    a  =  Two one-word operands in memory or two double-word operands in memory, depending on operand-size attribute (only BOUND).
    b  =  BYTE .
    bs =  Byte, sign-extended to the size of the destination operand.
    bss = Byte, sign-extended to the size of the stack pointer (for example, PUSH (6A)).
    w  =  WORD .
    d  =  DWORD .
    q  =  QUADWORD .
    v  =  Word or Dword or Quadword (64 bit).
    p  =  32-bit or 48-bit pointer, depending on operand-size attribute (for example, CALLF (9A).
    dr =  Double-real. Only x87 FPU instructions (for example, FADD).
    di =  DoubleWord-integer. Only x87 FPU instructions (for example, FIADD).
    wi =  Word-integer. Only x87 FPU instructions (for example, FIADD).
    ps =  128-bit packed single-precision floating-point data.
    psq =  64-bit packed single-precision floating-point data.

  }

  Op_NONE = 0; // Operand is not used.

  Op_b = $1;
  Op_w = $2;
  Op_d = $4;
  Op_q = $8;
  Op_v = Op_w or Op_d;
  Op_vqp = Op_v or Op_q;
  Op_SignExt = $10;
  Op_bs = Op_b or Op_SignExt;
  Op_Stack = $20;

  { One Byte Reg }
  Op_Regb = $1;
  Op_AH = Op_Regb;
  Op_AL = Op_Regb;
  Op_CL = Op_Regb;

  { 2 Bytes Reg }
  Op_Regw = $2;
  Op_AX = Op_Regw;
  Op_CX = Op_Regw;
  Op_DX = Op_Regw;
  Op_DI = Op_Regw;

  { DWORD Reg }
  Op_Regd = $4;
  Op_eAX = Op_Regd;
  Op_eCX = Op_Regd;
  Op_eBP = Op_Regd;
  Op_EBX = Op_Regd;

  { x64 Reg }
  Op_Regq = $8;
  Op_rAX = Op_Regq;
  Op_rCX = Op_Regq;

  { EFLAGS Reg }
  Op_Regf = $2;
  Op_Regef = $4;
  Op_Flags = Op_Regf; // 2 Bytes reg .
  Op_eFlags = Op_Regef; // 4 Bytes reg .

  { Segment Registers }
  Op_Segr = $40;
  Op_SS = Op_Segr; // stack segment .
  Op_ES = Op_Segr; // extra segment .
  Op_CS = Op_Segr; // code segment .
  Op_DS = Op_Segr; // data segment .
  Op_FS = Op_Segr;
  Op_GS = Op_Segr;

  Op_Sw = $80 or Op_w; // ModRM select Segment Registers + Word .e.g : MOV WORD PTR SS:[EBP+55],DS .
  Op_ESsr = $80; // ModR/M byte selects a segment register
  Op_SS_rSP = $80; // SS:[rSP]
  Op_ES_DI = $80; // ES:[DI]
  Op_DS_SI = $80; // DS:[SI]
  Op_DS_rSI = $80; // (DS):[rSI]
  Op_ES_rDI = $80; // (ES:)[rDI]
  Op_DS_rBX_AL = $80; // (DS:)[rBX+AL]
  Op_DS_rDI = $80;

  Op_E = $100;
  Op_G = $200; // general-purpose register .
  Op_Eb = Op_E or Op_b; // general-purpose register (AL,BL,..) or memory address .
  Op_Ew = Op_E or Op_w; // general-purpose register (AX,BX,..) or memory address .
  Op_Ed = Op_E or Op_d;
  Op_Ev = Op_E or Op_v; // eneral-purpose register (EAX,AX,..) or memory address .
  Op_Evqp = Op_E or Op_vqp; // eneral-purpose register (RAX,EAX,AX,..) or memory address .
  Op_Gb = Op_G or Op_b; // eneral-purpose register (AL,BL,..).
  Op_Gw = Op_G or Op_w; // general-purpose register (AX,BX,..).
  Op_Gd = Op_G or Op_d;
  Op_Gv = Op_G or Op_v; // general-purpose register Word or Dword (EAX,EBX,AX,BX,..).
  Op_Gvqp = Op_G or Op_vqp; // eneral-purpose register (RAX,EAX,AX,..).
  Op_Z = $400; // no ModR/M ; Registers are used .
  Op_Zb = Op_Z or Op_b; // no ModR/M ; Registers are used => Byte Reg (AL,BL,..)
  Op_Zv = Op_Z or Op_b;
  Op_Zvqp = Op_Z or Op_vqp;

  { Immediate }
  Op_Imm = $800;
  Op_Ib = Op_Imm or Op_b; // size of immediate = Byte .
  Op_Ibs = Op_Ib or Op_bs; // Byte, sign-extended to the size of the destination operand.
  Op_Ibss = Op_Ibs or Op_Stack; // Byte, sign-extended to the size of the stack pointer (for example, PUSH (6A)). eg: PUSH -5 .
  Op_Iw = Op_Imm or Op_w; // size of immediate = Word .
  Op_Ivs = Op_Imm or Op_v or Op_SignExt; // Word or doubleword sign extended to the size of the stack pointer (for example, PUSH (68)).
  Op_Ivqp = Op_Imm or Op_vqp; // Word or doubleword, depending on operand-size attribute, or quadword, promoted by REX.W in 64-bit mode.
  Op_Ivds = Op_Imm or Op_v or Op_SignExt;

  Op_Ap = $1000; // Direct address ; No ModRM .

  Op_M = $2000; // The ModR/M byte may refer only to memory.
  Op_Ma = Op_M or 1; // Two one-word operands in memory or two double-word operands in memory  .
  Op_Mdr = Op_M or 2; // Mem=Double-real.
  Op_Mwi = Op_M or 4; // Mem=Word-integer.
  Op_Mp = Op_M or 8; // Mem=32-bit or 48-bit pointer.
  Op_Mdi = Op_M or $10; // Mem=DWORD-integer.
  Op_Mw = Op_M or $20; // Mem=Word Memory . e.g : MOV EAX,WORD PRT DS:[EBX].
  Op_Msr = Op_M or $40; // Mem = Segment Registers .

  op_vds = Op_v or $4000;
  Op_J = $8000;
  Op_Jbs = Op_J or Op_bs; // relative offset;sign-extended .e.g : JO -5 .

  Op_O = $10000;
  Op_Ob = Op_O or Op_b;
  Op_Ovqp = Op_O or Op_vqp;
  Op_Jvds = Op_J or op_vds;

  Op_1 = $20000;
  Op_3 = $40000;
  Op_ST = $80000;
  Op_TwoByte = $100000;

  Op_XMM = $200000;
  Op_U = Op_XMM;
  Op_2V = Op_XMM;
  Op_2W = Op_XMM or Op_E;
  Op_p = $400000;
  Op_ps = $800000;
  Op_2D = Op_G;
  Op_2ss = $1000000; // Scalar element of a 128-bit packed single-precision floating data.
  Op_psq = Op_ps or Op_q;
  Op_LDTR = 0;
  Op_GDTR = 0;
  Op_Ms = 0;
  Op_Mq = Op_M or Op_q;
  Op_Uq = Op_U or Op_q;
  Op_CR0 = 0;
  Op_Vps = Op_2V or Op_ps;
  Op_Wq = Op_2W or Op_q;
  Op_Wps = Op_2W or Op_ps;
  Op_Vq = Op_2V or Op_q;
  Op_Dd = Op_2D or Op_d;
  Op_Td = 0;
  Op_Rd = Op_G or Op_d;
  Op_Cd = 0;
  Op_Qpi = 0;
  Op_Mps = Op_M or Op_ps;
  Op_Ppi = 0;
  Op_Wpsq = Op_2W or Op_psq;
  Op_Vss = Op_2V or Op_2ss;
  Op_Wss = Op_2W or Op_2ss;
  Op_PMC = 0;
  Op_Nq = 0;
  Op_Pq = 0;
  Op_Qd = 0;
  Op_Qq = 0;
  Op_Gdqp = 0;
  Op_Ups = Op_U or Op_ps;
  Op_Vpd = Op_2V or Op_p or Op_d;
  Op_Wdq = Op_2W or Op_d or Op_q;
  Op_Mdq = Op_M or Op_d or Op_q;
  Op_Vdq = Op_2V or Op_d or Op_q;
  Op_Wpd = Op_2W or Op_p or Op_d;
  Op_Mstx = 0;
  Op_ST1 = 0;
  Op_Mptp = 0;
  Op_Mdqp = 0;
  Op_Rdqp = 0;
  Op_IA32_BIOS_SIGN_ID = 0;
  Op_IA32_SYSENTER_CS = 0;
  Op_IA32_SYSENTER_ESP = 0;
  Op_IA32_TIME_STAMP_COUNTER = 0;

type
  TOpCodeEntryInfo = packed record
    Flags: Byte;
    Op1: DWORD;
    Op2: DWORD;
    Op3: DWORD;
    Op4: DWORD;
  end;

const
  { 50-58;58-59;59-60;90-98;B80-B8;B8-C0 }
  OneByteOpCodesEntry: array [0 .. $FF] of TOpCodeEntryInfo = (
    { 00 } (Flags: Opf_D or Opf_W or Opf_ModRM or Opf_L; Op1: Op_Eb; Op2: Op_Gb; Op3: Op_NONE; Op4: Op_NONE) { ADD } ,
    { 01 } (Flags: Opf_D or Opf_W or Opf_ModRM or Opf_L; Op1: Op_Evqp; Op2: Op_Gvqp; Op3: Op_NONE; Op4: Op_NONE) { ADD } ,
    { 02 } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Gb; Op2: Op_Eb; Op3: Op_NONE; Op4: Op_NONE) { ADD } ,
    { 03 } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { ADD } ,
    { 04 } (Flags: Opf_W; Op1: Op_AL; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { ADD } ,
    { 05 } (Flags: Opf_W; Op1: Op_rAX; Op2: Op_Ivds; Op3: Op_NONE; Op4: Op_NONE) { ADD } ,
    { 06 } (Flags: Opf_NONE; Op1: Op_SS_rSP; Op2: Op_ES; Op3: Op_NONE; Op4: Op_NONE) { PUSH } ,
    { 07 } (Flags: Opf_NONE; Op1: Op_ES; Op2: Op_SS_rSP; Op3: Op_NONE; Op4: Op_NONE) { POP } ,
    { 08 } (Flags: Opf_D or Opf_W or Opf_ModRM or Opf_L; Op1: Op_Eb; Op2: Op_Gb; Op3: Op_NONE; Op4: Op_NONE) { OR } ,
    { 09 } (Flags: Opf_D or Opf_W or Opf_ModRM or Opf_L; Op1: Op_Evqp; Op2: Op_Gvqp; Op3: Op_NONE; Op4: Op_NONE) { OR } ,
    { 0A } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Gb; Op2: Op_Eb; Op3: Op_NONE; Op4: Op_NONE) { OR } ,
    { 0B } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { OR } ,
    { 0C } (Flags: Opf_W; Op1: Op_AL; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { OR } ,
    { 0D } (Flags: Opf_W; Op1: Op_rAX; Op2: Op_Ivds; Op3: Op_NONE; Op4: Op_NONE) { OR } ,
    { 0E } (Flags: Opf_NONE; Op1: Op_SS_rSP; Op2: Op_CS; Op3: Op_NONE; Op4: Op_NONE) { PUSH } ,
    { 0F } (Flags: Opf_NONE; Op1: Op_CS; Op2: Op_SS_rSP; Op3: Op_NONE; Op4: Op_NONE) { POP } ,
    { 10 } (Flags: Opf_D or Opf_W or Opf_ModRM or Opf_L; Op1: Op_Eb; Op2: Op_Gb; Op3: Op_NONE; Op4: Op_NONE) { ADC } ,
    { 11 } (Flags: Opf_D or Opf_W or Opf_ModRM or Opf_L; Op1: Op_Evqp; Op2: Op_Gvqp; Op3: Op_NONE; Op4: Op_NONE) { ADC } ,
    { 12 } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Gb; Op2: Op_Eb; Op3: Op_NONE; Op4: Op_NONE) { ADC } ,
    { 13 } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { ADC } ,
    { 14 } (Flags: Opf_W; Op1: Op_AL; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { ADC } ,
    { 15 } (Flags: Opf_W; Op1: Op_rAX; Op2: Op_Ivds; Op3: Op_NONE; Op4: Op_NONE) { ADC } ,
    { 16 } (Flags: Opf_NONE; Op1: Op_SS_rSP; Op2: Op_SS; Op3: Op_NONE; Op4: Op_NONE) { PUSH } ,
    { 17 } (Flags: Opf_NONE; Op1: Op_SS; Op2: Op_SS_rSP; Op3: Op_NONE; Op4: Op_NONE) { POP } ,
    { 18 } (Flags: Opf_D or Opf_W or Opf_ModRM or Opf_L; Op1: Op_Eb; Op2: Op_Gb; Op3: Op_NONE; Op4: Op_NONE) { SBB } ,
    { 19 } (Flags: Opf_D or Opf_W or Opf_ModRM or Opf_L; Op1: Op_Evqp; Op2: Op_Gvqp; Op3: Op_NONE; Op4: Op_NONE) { SBB } ,
    { 1A } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Gb; Op2: Op_Eb; Op3: Op_NONE; Op4: Op_NONE) { SBB } ,
    { 1B } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { SBB } ,
    { 1C } (Flags: Opf_W; Op1: Op_AL; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { SBB } ,
    { 1D } (Flags: Opf_W; Op1: Op_rAX; Op2: Op_Ivds; Op3: Op_NONE; Op4: Op_NONE) { SBB } ,
    { 1E } (Flags: Opf_NONE; Op1: Op_SS_rSP; Op2: Op_DS; Op3: Op_NONE; Op4: Op_NONE) { PUSH } ,
    { 1F } (Flags: Opf_NONE; Op1: Op_DS; Op2: Op_SS_rSP; Op3: Op_NONE; Op4: Op_NONE) { POP } ,
    { 20 } (Flags: Opf_D or Opf_W or Opf_ModRM or Opf_L; Op1: Op_Eb; Op2: Op_Gb; Op3: Op_NONE; Op4: Op_NONE) { AND } ,
    { 21 } (Flags: Opf_D or Opf_W or Opf_ModRM or Opf_L; Op1: Op_Evqp; Op2: Op_Gvqp; Op3: Op_NONE; Op4: Op_NONE) { AND } ,
    { 22 } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Gb; Op2: Op_Eb; Op3: Op_NONE; Op4: Op_NONE) { AND } ,
    { 23 } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { AND } ,
    { 24 } (Flags: Opf_W; Op1: Op_AL; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { AND } ,
    { 25 } (Flags: Opf_W; Op1: Op_rAX; Op2: Op_Ivds; Op3: Op_NONE; Op4: Op_NONE) { AND } ,
    { 26 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_ES; Op3: Op_NONE; Op4: Op_NONE) { ES } ,
    { 27 } (Flags: Opf_NONE; Op1: Op_AL; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { DAA } ,
    { 28 } (Flags: Opf_D or Opf_W or Opf_ModRM or Opf_L; Op1: Op_Eb; Op2: Op_Gb; Op3: Op_NONE; Op4: Op_NONE) { SUB } ,
    { 29 } (Flags: Opf_D or Opf_W or Opf_ModRM or Opf_L; Op1: Op_Evqp; Op2: Op_Gvqp; Op3: Op_NONE; Op4: Op_NONE) { SUB } ,
    { 2A } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Gb; Op2: Op_Eb; Op3: Op_NONE; Op4: Op_NONE) { SUB } ,
    { 2B } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { SUB } ,
    { 2C } (Flags: Opf_W; Op1: Op_AL; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { SUB } ,
    { 2D } (Flags: Opf_W; Op1: Op_rAX; Op2: Op_Ivds; Op3: Op_NONE; Op4: Op_NONE) { SUB } ,
    { 2E } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_CS; Op3: Op_NONE; Op4: Op_NONE) { CS } ,
    { 2F } (Flags: Opf_NONE; Op1: Op_AL; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { DAS } ,
    { 30 } (Flags: Opf_D or Opf_W or Opf_ModRM or Opf_L; Op1: Op_Eb; Op2: Op_Gb; Op3: Op_NONE; Op4: Op_NONE) { XOR } ,
    { 31 } (Flags: Opf_D or Opf_W or Opf_ModRM or Opf_L; Op1: Op_Evqp; Op2: Op_Gvqp; Op3: Op_NONE; Op4: Op_NONE) { XOR } ,
    { 32 } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Gb; Op2: Op_Eb; Op3: Op_NONE; Op4: Op_NONE) { XOR } ,
    { 33 } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { XOR } ,
    { 34 } (Flags: Opf_W; Op1: Op_AL; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { XOR } ,
    { 35 } (Flags: Opf_W; Op1: Op_rAX; Op2: Op_Ivds; Op3: Op_NONE; Op4: Op_NONE) { XOR } ,
    { 36 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_SS; Op3: Op_NONE; Op4: Op_NONE) { SS } ,
    { 37 } (Flags: Opf_NONE; Op1: Op_AL; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { AAA } ,
    { 38 } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_NONE; Op2: Op_Eb; Op3: Op_Gb; Op4: Op_NONE) { CMP } ,
    { 39 } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_NONE; Op2: Op_Evqp; Op3: Op_Gvqp; Op4: Op_NONE) { CMP } ,
    { 3A } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_NONE; Op2: Op_Gb; Op3: Op_Eb; Op4: Op_NONE) { CMP } ,
    { 3B } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_NONE; Op2: Op_Gvqp; Op3: Op_Evqp; Op4: Op_NONE) { CMP } ,
    { 3C } (Flags: Opf_W; Op1: Op_NONE; Op2: Op_AL; Op3: Op_Ib; Op4: Op_NONE) { CMP } ,
    { 3D } (Flags: Opf_W; Op1: Op_NONE; Op2: Op_rAX; Op3: Op_Ivds; Op4: Op_NONE) { CMP } ,
    { 3E } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_DS; Op3: Op_NONE; Op4: Op_NONE) { DS } ,
    { 3F } (Flags: Opf_NONE; Op1: Op_AL; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { AAS } ,
    { 40 } (Flags: Opf_NONE; Op1: Op_Zv; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { INC } ,
    { 41 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { REX.B } ,
    { 42 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { REX.X } ,
    { 43 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { REX.XB } ,
    { 44 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { REX.R } ,
    { 45 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { REX.RB } ,
    { 46 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { REX.RX } ,
    { 47 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { REX.RXB } ,
    { 48 } (Flags: Opf_NONE; Op1: Op_Zv; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { DEC } ,
    { 49 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { REX.WB } ,
    { 4A } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { REX.WX } ,
    { 4B } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { REX.WXB } ,
    { 4C } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { REX.WR } ,
    { 4D } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { REX.WRB } ,
    { 4E } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { REX.WRX } ,
    { 4F } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { REX.WRXB } ,
    { 50 } (Flags: Opf_NONE; Op1: Op_SS_rSP; Op2: Op_Zv; Op3: Op_NONE; Op4: Op_NONE) { PUSH } ,
    { 51 } (Flags: Opf_NONE; Op1: Op_SS_rSP; Op2: Op_Zv; Op3: Op_NONE; Op4: Op_NONE) { PUSH } ,
    { 52 } (Flags: Opf_NONE; Op1: Op_SS_rSP; Op2: Op_Zv; Op3: Op_NONE; Op4: Op_NONE) { PUSH } ,
    { 53 } (Flags: Opf_NONE; Op1: Op_SS_rSP; Op2: Op_Zv; Op3: Op_NONE; Op4: Op_NONE) { PUSH } ,
    { 54 } (Flags: Opf_NONE; Op1: Op_SS_rSP; Op2: Op_Zv; Op3: Op_NONE; Op4: Op_NONE) { PUSH } ,
    { 55 } (Flags: Opf_NONE; Op1: Op_SS_rSP; Op2: Op_Zv; Op3: Op_NONE; Op4: Op_NONE) { PUSH } ,
    { 56 } (Flags: Opf_NONE; Op1: Op_SS_rSP; Op2: Op_Zv; Op3: Op_NONE; Op4: Op_NONE) { PUSH } ,
    { 57 } (Flags: Opf_NONE; Op1: Op_SS_rSP; Op2: Op_Zv; Op3: Op_NONE; Op4: Op_NONE) { PUSH } ,
    { 58 } (Flags: Opf_NONE; Op1: Op_Zv; Op2: Op_SS_rSP; Op3: Op_NONE; Op4: Op_NONE) { POP } ,
    { 59 } (Flags: Opf_NONE; Op1: Op_Zv; Op2: Op_SS_rSP; Op3: Op_NONE; Op4: Op_NONE) { POP } ,
    { 5A } (Flags: Opf_NONE; Op1: Op_Zv; Op2: Op_SS_rSP; Op3: Op_NONE; Op4: Op_NONE) { POP } ,
    { 5B } (Flags: Opf_NONE; Op1: Op_Zv; Op2: Op_SS_rSP; Op3: Op_NONE; Op4: Op_NONE) { POP } ,
    { 5C } (Flags: Opf_NONE; Op1: Op_Zv; Op2: Op_SS_rSP; Op3: Op_NONE; Op4: Op_NONE) { POP } ,
    { 5D } (Flags: Opf_NONE; Op1: Op_Zv; Op2: Op_SS_rSP; Op3: Op_NONE; Op4: Op_NONE) { POP } ,
    { 5E } (Flags: Opf_NONE; Op1: Op_Zv; Op2: Op_SS_rSP; Op3: Op_NONE; Op4: Op_NONE) { POP } ,
    { 5F } (Flags: Opf_NONE; Op1: Op_Zv; Op2: Op_SS_rSP; Op3: Op_NONE; Op4: Op_NONE) { POP } ,
    { 60 } (Flags: Opf_NONE; Op1: Op_SS_rSP; Op2: Op_AX; Op3: Op_CX; Op4: Op_NONE) { PUSHA } ,
    { 61 } (Flags: Opf_NONE; Op1: Op_DI; Op2: Op_SS_rSP; Op3: Op_NONE; Op4: Op_NONE) { POPA } ,
    { 62 } (Flags: Opf_D or Opf_ModRM; Op1: Op_SS_rSP; Op2: Op_Gv; Op3: Op_Ma; Op4: Op_NONE) { BOUND } ,
    { 63 } (Flags: Opf_ModRM; Op1: Op_NONE; Op2: Op_Ew; Op3: Op_Gw; Op4: Op_NONE) { ARPL } ,
    { 64 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_FS; Op3: Op_NONE; Op4: Op_NONE) { FS } ,
    { 65 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_GS; Op3: Op_NONE; Op4: Op_NONE) { GS } ,
    { 66 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { } ,
    { 67 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { } ,
    { 68 } (Flags: Opf_NONE; Op1: Op_SS_rSP; Op2: Op_Ivs; Op3: Op_NONE; Op4: Op_NONE) { PUSH } ,
    { 69 } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_Ivds; Op4: Op_NONE) { IMUL } ,
    { 6A } (Flags: Opf_S; Op1: Op_SS_rSP; Op2: Op_Ibss; Op3: Op_NONE; Op4: Op_NONE) { PUSH } ,
    { 6B } (Flags: Opf_ModRM or Opf_S; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_Ibs; Op4: Op_NONE) { IMUL } ,
    { 6C } (Flags: Opf_W; Op1: Op_ES_rDI; Op2: Op_DX; Op3: Op_NONE; Op4: Op_NONE) { INS } ,
    { 6D } (Flags: Opf_W; Op1: Op_ES_DI; Op2: Op_DX; Op3: Op_NONE; Op4: Op_NONE) { INS } ,
    { 6E } (Flags: Opf_W; Op1: Op_DX; Op2: Op_DS_rSI; Op3: Op_NONE; Op4: Op_NONE) { OUTS } ,
    { 6F } (Flags: Opf_W; Op1: Op_DX; Op2: Op_DS_SI; Op3: Op_NONE; Op4: Op_NONE) { OUTS } ,
    { 70 } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jbs; Op3: Op_NONE; Op4: Op_NONE) { JO } ,
    { 71 } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jbs; Op3: Op_NONE; Op4: Op_NONE) { JNO } ,
    { 72 } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jbs; Op3: Op_NONE; Op4: Op_NONE) { JB } ,
    { 73 } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jbs; Op3: Op_NONE; Op4: Op_NONE) { JNB } ,
    { 74 } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jbs; Op3: Op_NONE; Op4: Op_NONE) { JZ } ,
    { 75 } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jbs; Op3: Op_NONE; Op4: Op_NONE) { JNZ } ,
    { 76 } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jbs; Op3: Op_NONE; Op4: Op_NONE) { JBE } ,
    { 77 } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jbs; Op3: Op_NONE; Op4: Op_NONE) { JNBE } ,
    { 78 } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jbs; Op3: Op_NONE; Op4: Op_NONE) { JS } ,
    { 79 } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jbs; Op3: Op_NONE; Op4: Op_NONE) { JNS } ,
    { 7A } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jbs; Op3: Op_NONE; Op4: Op_NONE) { JP } ,
    { 7B } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jbs; Op3: Op_NONE; Op4: Op_NONE) { JNP } ,
    { 7C } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jbs; Op3: Op_NONE; Op4: Op_NONE) { JL } ,
    { 7D } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jbs; Op3: Op_NONE; Op4: Op_NONE) { JNL } ,
    { 7E } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jbs; Op3: Op_NONE; Op4: Op_NONE) { JLE } ,
    { 7F } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jbs; Op3: Op_NONE; Op4: Op_NONE) { JNLE } ,
    { 80 } (Flags: Opf_W or Opf_L or Opf_ModRMExt; Op1: Op_Eb; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { ADD } ,
    { 81 } (Flags: Opf_W or Opf_L or Opf_ModRMExt; Op1: Op_Evqp; Op2: Op_Ivds; Op3: Op_NONE; Op4: Op_NONE) { ADD } ,
    { 82 } (Flags: Opf_W or Opf_L or Opf_ModRMExt; Op1: Op_Eb; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { ADD } ,
    { 83 } (Flags: Opf_W or Opf_L or Opf_S or Opf_ModRMExt; Op1: Op_Evqp; Op2: Op_Ibs; Op3: Op_NONE; Op4: Op_NONE) { ADD } ,
    { 84 } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_NONE; Op2: Op_Eb; Op3: Op_Gb; Op4: Op_NONE) { TEST } ,
    { 85 } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_NONE; Op2: Op_Evqp; Op3: Op_Gvqp; Op4: Op_NONE) { TEST } ,
    { 86 } (Flags: Opf_D or Opf_W or Opf_ModRM or Opf_L; Op1: Op_Gb; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { XCHG } ,
    { 87 } (Flags: Opf_D or Opf_W or Opf_ModRM or Opf_L; Op1: Op_Gvqp; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { XCHG } ,
    { 88 } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Eb; Op2: Op_Gb; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { 89 } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Evqp; Op2: Op_Gvqp; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { 8A } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Gb; Op2: Op_Eb; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { 8B } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { 8C } (Flags: Opf_D or Opf_ModRM; Op1: Op_Mw; Op2: Op_Sw; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { 8D } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_M; Op3: Op_NONE; Op4: Op_NONE) { LEA } ,
    { 8E } (Flags: Opf_D or Opf_ModRM; Op1: Op_Sw; Op2: Op_Ew; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { 8F } (Flags: Opf_W or Opf_ModRMExt; Op1: Op_Ev; Op2: Op_SS_rSP; Op3: Op_NONE; Op4: Op_NONE) { POP } ,
    { 90 } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { XCHG } ,
    { 91 } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { XCHG } ,
    { 92 } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { XCHG } ,
    { 93 } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { XCHG } ,
    { 94 } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { XCHG } ,
    { 95 } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { XCHG } ,
    { 96 } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { XCHG } ,
    { 97 } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { XCHG } ,
    { 98 } (Flags: Opf_NONE; Op1: Op_AX; Op2: Op_AL; Op3: Op_NONE; Op4: Op_NONE) { CBW } ,
    { 99 } (Flags: Opf_NONE; Op1: Op_DX; Op2: Op_AX; Op3: Op_NONE; Op4: Op_NONE) { CWD } ,
    { 9A } (Flags: Opf_NONE; Op1: Op_SS_rSP; Op2: Op_Ap; Op3: Op_NONE; Op4: Op_NONE) { CALLF } ,
    { 9B } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { FWAIT } ,
    { 9C } (Flags: Opf_NONE; Op1: Op_SS_rSP; Op2: Op_Flags; Op3: Op_NONE; Op4: Op_NONE) { PUSHF } ,
    { 9D } (Flags: Opf_NONE; Op1: Op_Flags; Op2: Op_SS_rSP; Op3: Op_NONE; Op4: Op_NONE) { POPF } ,
    { 9E } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_AH; Op3: Op_NONE; Op4: Op_NONE) { SAHF } ,
    { 9F } (Flags: Opf_NONE; Op1: Op_AH; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { LAHF } ,
    { A0 } (Flags: Opf_W; Op1: Op_AL; Op2: Op_Ob; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { A1 } (Flags: Opf_W; Op1: Op_rAX; Op2: Op_Ovqp; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { A2 } (Flags: Opf_W; Op1: Op_Ob; Op2: Op_AL; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { A3 } (Flags: Opf_W; Op1: Op_Ovqp; Op2: Op_rAX; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { A4 } (Flags: Opf_W; Op1: Op_ES_rDI; Op2: Op_DS_rSI; Op3: Op_NONE; Op4: Op_NONE) { MOVS } ,
    { A5 } (Flags: Opf_W; Op1: Op_ES_DI; Op2: Op_DS_SI; Op3: Op_NONE; Op4: Op_NONE) { MOVS } ,
    { A6 } (Flags: Opf_W; Op1: Op_NONE; Op2: Op_ES_rDI; Op3: Op_NONE; Op4: Op_NONE) { CMPS } ,
    { A7 } (Flags: Opf_W; Op1: Op_NONE; Op2: Op_ES_DI; Op3: Op_NONE; Op4: Op_NONE) { CMPS } ,
    { A8 } (Flags: Opf_W; Op1: Op_NONE; Op2: Op_AL; Op3: Op_Ib; Op4: Op_NONE) { TEST } ,
    { A9 } (Flags: Opf_W; Op1: Op_NONE; Op2: Op_rAX; Op3: Op_Ivds; Op4: Op_NONE) { TEST } ,
    { AA } (Flags: Opf_W; Op1: Op_ES_rDI; Op2: Op_AL; Op3: Op_NONE; Op4: Op_NONE) { STOS } ,
    { AB } (Flags: Opf_W; Op1: Op_ES_DI; Op2: Op_AX; Op3: Op_NONE; Op4: Op_NONE) { STOS } ,
    { AC } (Flags: Opf_W; Op1: Op_AL; Op2: Op_DS_rSI; Op3: Op_NONE; Op4: Op_NONE) { LODS } ,
    { AD } (Flags: Opf_W; Op1: Op_AX; Op2: Op_DS_SI; Op3: Op_NONE; Op4: Op_NONE) { LODS } ,
    { AE } (Flags: Opf_W; Op1: Op_NONE; Op2: Op_ES_rDI; Op3: Op_AL; Op4: Op_NONE) { SCAS } ,
    { AF } (Flags: Opf_W; Op1: Op_NONE; Op2: Op_ES_DI; Op3: Op_NONE; Op4: Op_NONE) { SCAS } ,
    { B0 } (Flags: Opf_NONE; Op1: Op_Zb; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { B1 } (Flags: Opf_NONE; Op1: Op_Zb; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { B2 } (Flags: Opf_NONE; Op1: Op_Zb; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { B3 } (Flags: Opf_NONE; Op1: Op_Zb; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { B4 } (Flags: Opf_NONE; Op1: Op_Zb; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { B5 } (Flags: Opf_NONE; Op1: Op_Zb; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { B6 } (Flags: Opf_NONE; Op1: Op_Zb; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { B7 } (Flags: Opf_NONE; Op1: Op_Zb; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { B8 } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_Ivqp; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { B9 } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_Ivqp; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { BA } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_Ivqp; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { BB } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_Ivqp; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { BC } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_Ivqp; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { BD } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_Ivqp; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { BE } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_Ivqp; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { BF } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_Ivqp; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { C0 } (Flags: Opf_W or Opf_ModRMExt; Op1: Op_Eb; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { ROL } ,
    { C1 } (Flags: Opf_W or Opf_ModRMExt; Op1: Op_Evqp; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { ROL } ,
    { C2 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_SS_rSP; Op3: Op_NONE; Op4: Op_NONE) { RETN } ,
    { C3 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_SS_rSP; Op3: Op_NONE; Op4: Op_NONE) { RETN } ,
    { C4 } (Flags: Opf_ModRM; Op1: Op_ES; Op2: Op_Mp; Op3: Op_NONE; Op4: Op_NONE) { LES } ,
    { C5 } (Flags: Opf_ModRM; Op1: Op_DS; Op2: Op_Mp; Op3: Op_NONE; Op4: Op_NONE) { LDS } ,
    { C6 } (Flags: Opf_W or Opf_ModRMExt; Op1: Op_Eb; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { C7 } (Flags: Opf_W or Opf_ModRMExt; Op1: Op_Evqp; Op2: Op_Ivds; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { C8 } (Flags: Opf_NONE; Op1: Op_SS_rSP; Op2: Op_Iw; Op3: Op_Ib; Op4: Op_NONE) { ENTER } ,
    { C9 } (Flags: Opf_NONE; Op1: Op_eBP; Op2: Op_SS_rSP; Op3: Op_NONE; Op4: Op_NONE) { LEAVE } ,
    { CA } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_Iw; Op3: Op_SS_rSP; Op4: Op_NONE) { RETF } ,
    { CB } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_SS_rSP; Op3: Op_NONE; Op4: Op_NONE) { RETF } ,
    { CC } (Flags: Opf_NONE; Op1: Op_SS_rSP; Op2: Op_3; Op3: Op_eFlags; Op4: Op_NONE) { INT } ,
    { CD } (Flags: Opf_NONE; Op1: Op_SS_rSP; Op2: Op_Ib; Op3: Op_eFlags; Op4: Op_NONE) { INT } ,
    { CE } (Flags: Opf_NONE; Op1: Op_SS_rSP; Op2: Op_eFlags; Op3: Op_NONE; Op4: Op_NONE) { INTO } ,
    { CF } (Flags: Opf_NONE; Op1: Op_Flags; Op2: Op_SS_rSP; Op3: Op_NONE; Op4: Op_NONE) { IRET } ,
    { D0 } (Flags: Opf_W or Opf_ModRMExt; Op1: Op_Eb; Op2: Op_1; Op3: Op_NONE; Op4: Op_NONE) { ROL } ,
    { D1 } (Flags: Opf_W or Opf_ModRMExt; Op1: Op_Evqp; Op2: Op_1; Op3: Op_NONE; Op4: Op_NONE) { ROL } ,
    { D2 } (Flags: Opf_W or Opf_ModRMExt; Op1: Op_Eb; Op2: Op_CL; Op3: Op_NONE; Op4: Op_NONE) { ROL } ,
    { D3 } (Flags: Opf_W or Opf_ModRMExt; Op1: Op_Evqp; Op2: Op_CL; Op3: Op_NONE; Op4: Op_NONE) { ROL } ,
    { D4 } (Flags: Opf_NONE; Op1: Op_AL; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { AAM } ,
    { D5 } (Flags: Opf_NONE; Op1: Op_AL; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { AAD } ,
    { D6 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { } ,
    { D7 } (Flags: Opf_NONE; Op1: Op_AL; Op2: Op_DS_rBX_AL; Op3: Op_NONE; Op4: Op_NONE) { XLAT } ,
    { D8 } (Flags: Opf_ModRMExt; Op1: Op_ST; Op2: Op_Msr; Op3: Op_NONE; Op4: Op_NONE) { FADD } ,
    { D9 } (Flags: Opf_ModRMExt; Op1: Op_ST; Op2: Op_ESsr; Op3: Op_NONE; Op4: Op_NONE) { FLD } ,
    { DA } (Flags: Opf_ModRMExt; Op1: Op_ST; Op2: Op_Mdi; Op3: Op_NONE; Op4: Op_NONE) { FIADD } ,
    { DB } (Flags: Opf_ModRMExt; Op1: Op_ST; Op2: Op_Mdi; Op3: Op_NONE; Op4: Op_NONE) { FILD } ,
    { DC } (Flags: Opf_ModRMExt; Op1: Op_ST; Op2: Op_Mdr; Op3: Op_NONE; Op4: Op_NONE) { FADD } ,
    { DD } (Flags: Opf_ModRMExt; Op1: Op_ST; Op2: Op_Mdr; Op3: Op_NONE; Op4: Op_NONE) { FLD } ,
    { DE } (Flags: Opf_ModRMExt; Op1: Op_ST; Op2: Op_Mwi; Op3: Op_NONE; Op4: Op_NONE) { FIADD } ,
    { DF } (Flags: Opf_ModRMExt; Op1: Op_ST; Op2: Op_Mwi; Op3: Op_NONE; Op4: Op_NONE) { FILD } ,
    { E0 } (Flags: Opf_Jump; Op1: Op_eCX; Op2: Op_Jbs; Op3: Op_NONE; Op4: Op_NONE) { LOOPNZ } ,
    { E1 } (Flags: Opf_Jump; Op1: Op_eCX; Op2: Op_Jbs; Op3: Op_NONE; Op4: Op_NONE) { LOOPZ } ,
    { E2 } (Flags: Opf_Jump; Op1: Op_eCX; Op2: Op_Jbs; Op3: Op_NONE; Op4: Op_NONE) { LOOP } ,
    { E3 } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jbs; Op3: Op_CX; Op4: Op_NONE) { JCXZ } ,
    { E4 } (Flags: Opf_W; Op1: Op_AL; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { IN } ,
    { E5 } (Flags: Opf_W; Op1: Op_eAX; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { IN } ,
    { E6 } (Flags: Opf_W; Op1: Op_Ib; Op2: Op_AL; Op3: Op_NONE; Op4: Op_NONE) { OUT } ,
    { E7 } (Flags: Opf_W; Op1: Op_Ib; Op2: Op_eAX; Op3: Op_NONE; Op4: Op_NONE) { OUT } ,
    { E8 } (Flags: Opf_Jump; Op1: Op_SS_rSP; Op2: Op_Jvds; Op3: Op_NONE; Op4: Op_NONE) { CALL } ,
    { E9 } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jvds; Op3: Op_NONE; Op4: Op_NONE) { JMP } ,
    { EA } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Ap; Op3: Op_NONE; Op4: Op_NONE) { JMPF } ,
    { EB } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jbs; Op3: Op_NONE; Op4: Op_NONE) { JMP } ,
    { EC } (Flags: Opf_W; Op1: Op_AL; Op2: Op_DX; Op3: Op_NONE; Op4: Op_NONE) { IN } ,
    { ED } (Flags: Opf_W; Op1: Op_eAX; Op2: Op_DX; Op3: Op_NONE; Op4: Op_NONE) { IN } ,
    { EE } (Flags: Opf_W; Op1: Op_DX; Op2: Op_AL; Op3: Op_NONE; Op4: Op_NONE) { OUT } ,
    { EF } (Flags: Opf_W; Op1: Op_DX; Op2: Op_eAX; Op3: Op_NONE; Op4: Op_NONE) { OUT } ,
    { F0 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { LOCK } ,
    { F1 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { } ,
    { F2 } (Flags: Opf_NONE; Op1: Op_eCX; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { REPNZ } ,
    { F3 } (Flags: Opf_NONE; Op1: Op_eCX; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { REPZ } ,
    { F4 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { HLT } ,
    { F5 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { CMC } ,
    { F6 } (Flags: Opf_W or Opf_ModRMExt; Op1: Op_NONE; Op2: Op_Eb; Op3: Op_Ib; Op4: Op_NONE) { TEST } ,
    { F7 } (Flags: Opf_W or Opf_ModRMExt; Op1: Op_NONE; Op2: Op_Evqp; Op3: Op_Ivqp; Op4: Op_NONE) { TEST } ,
    { F8 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { CLC } ,
    { F9 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { STC } ,
    { FA } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { CLI } ,
    { FB } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { STI } ,
    { FC } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { CLD } ,
    { FD } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { STD } ,
    { FE } (Flags: Opf_W or Opf_ModRMExt; Op1: Op_Eb; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { INC } ,
    { FF } (Flags: Opf_W or Opf_ModRMExt; Op1: Op_Evqp; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { INC }
    );

  { TwoByteOpCodesEntry }
  TwoByteOpCodesEntry: array [0 .. $FF] of TOpCodeEntryInfo = (
    { 00 } (Flags: Opf_ModRMExt; Op1: Op_Mw; Op2: Op_LDTR; Op3: Op_NONE; Op4: Op_NONE) { SLDT } ,
    { 01 } (Flags: Opf_ModRMExt; Op1: Op_Ms; Op2: Op_GDTR; Op3: Op_NONE; Op4: Op_NONE) { SGDT } ,
    { 02 } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Mw; Op3: Op_NONE; Op4: Op_NONE) { LAR } ,
    { 03 } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Mw; Op3: Op_NONE; Op4: Op_NONE) { LSL } ,
    { 04 } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Mw; Op3: Op_NONE; Op4: Op_NONE) { LSL } ,
    { 05 } (Flags: Opf_NONE; Op1: Op_AX; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { LOADALL } ,
    { 06 } (Flags: Opf_NONE; Op1: Op_CR0; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { CLTS } ,
    { 07 } (Flags: Opf_NONE; Op1: Op_eAX; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { LOADALL } ,
    { 08 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { INVD } ,
    { 09 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { WBINVD } ,
    { 0A } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { WBINVD } ,
    { 0B } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { UD2 } ,
    { 0C } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { UD2 } ,
    { 0D } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_Ev; Op3: Op_NONE; Op4: Op_NONE) { NOP } ,
    { 0E } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_Ev; Op3: Op_NONE; Op4: Op_NONE) { NOP } ,
    { 0F } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_Ev; Op3: Op_NONE; Op4: Op_NONE) { NOP } ,
    { 10 } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_NONE; Op4: Op_NONE) { MOVUPS } ,
    { 11 } (Flags: Opf_ModRM; Op1: Op_Wps; Op2: Op_Vps; Op3: Op_NONE; Op4: Op_NONE) { MOVUPS } ,
    { 12 } (Flags: Opf_ModRM; Op1: Op_Vq; Op2: Op_Uq; Op3: Op_NONE; Op4: Op_NONE) { MOVHLPS } ,
    { 13 } (Flags: Opf_ModRM; Op1: Op_Mq; Op2: Op_Vq; Op3: Op_NONE; Op4: Op_NONE) { MOVLPS } ,
    { 14 } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wq; Op3: Op_NONE; Op4: Op_NONE) { UNPCKLPS } ,
    { 15 } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wq; Op3: Op_NONE; Op4: Op_NONE) { UNPCKHPS } ,
    { 16 } (Flags: Opf_ModRM; Op1: Op_Vq; Op2: Op_Uq; Op3: Op_NONE; Op4: Op_NONE) { MOVLHPS } ,
    { 17 } (Flags: Opf_ModRM; Op1: Op_Mq; Op2: Op_Vq; Op3: Op_NONE; Op4: Op_NONE) { MOVHPS } ,
    { 18 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_Ev; Op3: Op_NONE; Op4: Op_NONE) { HINT_NOP } ,
    { 19 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_Ev; Op3: Op_NONE; Op4: Op_NONE) { HINT_NOP } ,
    { 1A } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_Ev; Op3: Op_NONE; Op4: Op_NONE) { HINT_NOP } ,
    { 1B } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_Ev; Op3: Op_NONE; Op4: Op_NONE) { HINT_NOP } ,
    { 1C } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_Ev; Op3: Op_NONE; Op4: Op_NONE) { HINT_NOP } ,
    { 1D } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_Ev; Op3: Op_NONE; Op4: Op_NONE) { HINT_NOP } ,
    { 1E } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_Ev; Op3: Op_NONE; Op4: Op_NONE) { HINT_NOP } ,
    { 1F } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_Ev; Op3: Op_NONE; Op4: Op_NONE) { HINT_NOP } ,
    { 20 } (Flags: Opf_ModRM; Op1: Op_Rd; Op2: Op_Cd; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { 21 } (Flags: Opf_ModRM; Op1: Op_Rd; Op2: Op_Dd; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { 22 } (Flags: Opf_ModRM; Op1: Op_Cd; Op2: Op_Rd; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { 23 } (Flags: Opf_ModRM; Op1: Op_Dd; Op2: Op_Rd; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { 24 } (Flags: Opf_ModRM; Op1: Op_Rd; Op2: Op_Td; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { 25 } (Flags: Opf_ModRM; Op1: Op_Rd; Op2: Op_Td; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { 26 } (Flags: Opf_ModRM; Op1: Op_Td; Op2: Op_Rd; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { 27 } (Flags: Opf_ModRM; Op1: Op_Td; Op2: Op_Rd; Op3: Op_NONE; Op4: Op_NONE) { MOV } ,
    { 28 } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_NONE; Op4: Op_NONE) { MOVAPS } ,
    { 29 } (Flags: Opf_ModRM; Op1: Op_Wps; Op2: Op_Vps; Op3: Op_NONE; Op4: Op_NONE) { MOVAPS } ,
    { 2A } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Qpi; Op3: Op_NONE; Op4: Op_NONE) { CVTPI2PS } ,
    { 2B } (Flags: Opf_ModRM; Op1: Op_Mps; Op2: Op_Vps; Op3: Op_NONE; Op4: Op_NONE) { MOVNTPS } ,
    { 2C } (Flags: Opf_ModRM; Op1: Op_Ppi; Op2: Op_Wpsq; Op3: Op_NONE; Op4: Op_NONE) { CVTTPS2PI } ,
    { 2D } (Flags: Opf_ModRM; Op1: Op_Ppi; Op2: Op_Wpsq; Op3: Op_NONE; Op4: Op_NONE) { CVTPS2PI } ,
    { 2E } (Flags: Opf_ModRM; Op1: Op_NONE; Op2: Op_Vss; Op3: Op_Wss; Op4: Op_NONE) { UCOMISS } ,
    { 2F } (Flags: Opf_ModRM; Op1: Op_NONE; Op2: Op_Vss; Op3: Op_Wss; Op4: Op_NONE) { COMISS } ,
    { 30 } (Flags: Opf_NONE; Op1: Op_Msr; Op2: Op_rCX; Op3: Op_rAX; Op4: Op_NONE) { WRMSR } ,
    { 31 } (Flags: Opf_NONE; Op1: Op_eAX; Op2: Op_IA32_TIME_STAMP_COUNTER; Op3: Op_NONE; Op4: Op_NONE) { RDTSC } ,
    { 32 } (Flags: Opf_NONE; Op1: Op_rAX; Op2: Op_rCX; Op3: Op_Msr; Op4: Op_NONE) { RDMSR } ,
    { 33 } (Flags: Opf_NONE; Op1: Op_eAX; Op2: Op_PMC; Op3: Op_NONE; Op4: Op_NONE) { RDPMC } ,
    { 34 } (Flags: Opf_NONE; Op1: Op_SS; Op2: Op_IA32_SYSENTER_CS; Op3: Op_IA32_SYSENTER_ESP; Op4: Op_NONE) { SYSENTER } ,
    { 35 } (Flags: Opf_NONE; Op1: Op_SS; Op2: Op_IA32_SYSENTER_CS; Op3: Op_rCX; Op4: Op_NONE) { SYSEXIT } ,
    { 36 } (Flags: Opf_NONE; Op1: Op_SS; Op2: Op_IA32_SYSENTER_CS; Op3: Op_rCX; Op4: Op_NONE) { SYSEXIT } ,
    { 37 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_eAX; Op3: Op_NONE; Op4: Op_NONE) { GETSEC } ,
    { 38 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PSHUFB } ,
    { 39 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PSHUFB } ,
    { 3A } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_Ib; Op4: Op_NONE) { ROUNDPS } ,
    { 3B } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_Ib; Op4: Op_NONE) { ROUNDPS } ,
    { 3C } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_Ib; Op4: Op_NONE) { ROUNDPS } ,
    { 3D } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_Ib; Op4: Op_NONE) { ROUNDPS } ,
    { 3E } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_Ib; Op4: Op_NONE) { ROUNDPS } ,
    { 3F } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_Ib; Op4: Op_NONE) { ROUNDPS } ,
    { 40 } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { CMOVO } ,
    { 41 } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { CMOVNO } ,
    { 42 } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { CMOVB } ,
    { 43 } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { CMOVNB } ,
    { 44 } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { CMOVZ } ,
    { 45 } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { CMOVNZ } ,
    { 46 } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { CMOVBE } ,
    { 47 } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { CMOVNBE } ,
    { 48 } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { CMOVS } ,
    { 49 } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { CMOVNS } ,
    { 4A } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { CMOVP } ,
    { 4B } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { CMOVNP } ,
    { 4C } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { CMOVL } ,
    { 4D } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { CMOVNL } ,
    { 4E } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { CMOVLE } ,
    { 4F } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { CMOVNLE } ,
    { 50 } (Flags: Opf_ModRM; Op1: Op_Gdqp; Op2: Op_Ups; Op3: Op_NONE; Op4: Op_NONE) { MOVMSKPS } ,
    { 51 } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_NONE; Op4: Op_NONE) { SQRTPS } ,
    { 52 } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_NONE; Op4: Op_NONE) { RSQRTPS } ,
    { 53 } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_NONE; Op4: Op_NONE) { RCPPS } ,
    { 54 } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_NONE; Op4: Op_NONE) { ANDPS } ,
    { 55 } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_NONE; Op4: Op_NONE) { ANDNPS } ,
    { 56 } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_NONE; Op4: Op_NONE) { ORPS } ,
    { 57 } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_NONE; Op4: Op_NONE) { XORPS } ,
    { 58 } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_NONE; Op4: Op_NONE) { ADDPS } ,
    { 59 } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_NONE; Op4: Op_NONE) { MULPS } ,
    { 5A } (Flags: Opf_ModRM; Op1: Op_Vpd; Op2: Op_Wps; Op3: Op_NONE; Op4: Op_NONE) { CVTPS2PD } ,
    { 5B } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wdq; Op3: Op_NONE; Op4: Op_NONE) { CVTDQ2PS } ,
    { 5C } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_NONE; Op4: Op_NONE) { SUBPS } ,
    { 5D } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_NONE; Op4: Op_NONE) { MINPS } ,
    { 5E } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_NONE; Op4: Op_NONE) { DIVPS } ,
    { 5F } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_NONE; Op4: Op_NONE) { MAXPS } ,
    { 60 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qd; Op3: Op_NONE; Op4: Op_NONE) { PUNPCKLBW } ,
    { 61 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qd; Op3: Op_NONE; Op4: Op_NONE) { PUNPCKLWD } ,
    { 62 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qd; Op3: Op_NONE; Op4: Op_NONE) { PUNPCKLDQ } ,
    { 63 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qd; Op3: Op_NONE; Op4: Op_NONE) { PACKSSWB } ,
    { 64 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qd; Op3: Op_NONE; Op4: Op_NONE) { PCMPGTB } ,
    { 65 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qd; Op3: Op_NONE; Op4: Op_NONE) { PCMPGTW } ,
    { 66 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qd; Op3: Op_NONE; Op4: Op_NONE) { PCMPGTD } ,
    { 67 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PACKUSWB } ,
    { 68 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PUNPCKHBW } ,
    { 69 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PUNPCKHWD } ,
    { 6A } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PUNPCKHDQ } ,
    { 6B } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PACKSSDW } ,
    { 6C } (Flags: Opf_ModRM; Op1: Op_Vdq; Op2: Op_Wdq; Op3: Op_NONE; Op4: Op_NONE) { PUNPCKLQDQ } ,
    { 6D } (Flags: Opf_ModRM; Op1: Op_Vdq; Op2: Op_Wdq; Op3: Op_NONE; Op4: Op_NONE) { PUNPCKHQDQ } ,
    { 6E } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Ed; Op3: Op_NONE; Op4: Op_NONE) { MOVD } ,
    { 6F } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { MOVQ } ,
    { 70 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_Ib; Op4: Op_NONE) { PSHUFW } ,
    { 71 } (Flags: Opf_ModRMExt; Op1: Op_Nq; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { PSRLW } ,
    { 72 } (Flags: Opf_ModRMExt; Op1: Op_Nq; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { PSRLD } ,
    { 73 } (Flags: Opf_ModRMExt; Op1: Op_Nq; Op2: Op_Ib; Op3: Op_NONE; Op4: Op_NONE) { PSRLQ } ,
    { 74 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PCMPEQB } ,
    { 75 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PCMPEQW } ,
    { 76 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PCMPEQD } ,
    { 77 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { EMMS } ,
    { 78 } (Flags: Opf_ModRM; Op1: Op_Ed; Op2: Op_Gd; Op3: Op_NONE; Op4: Op_NONE) { VMREAD } ,
    { 79 } (Flags: Opf_ModRM; Op1: Op_NONE; Op2: Op_Gd; Op3: Op_Ed; Op4: Op_NONE) { VMWRITE } ,
    { 7A } (Flags: Opf_ModRM; Op1: Op_NONE; Op2: Op_Gd; Op3: Op_Ed; Op4: Op_NONE) { VMWRITE } ,
    { 7B } (Flags: Opf_ModRM; Op1: Op_NONE; Op2: Op_Gd; Op3: Op_Ed; Op4: Op_NONE) { VMWRITE } ,
    { 7C } (Flags: Opf_ModRM; Op1: Op_Vpd; Op2: Op_Wpd; Op3: Op_NONE; Op4: Op_NONE) { HADDPD } ,
    { 7D } (Flags: Opf_ModRM; Op1: Op_Vpd; Op2: Op_Wpd; Op3: Op_NONE; Op4: Op_NONE) { HSUBPD } ,
    { 7E } (Flags: Opf_ModRM; Op1: Op_Ed; Op2: Op_Pq; Op3: Op_NONE; Op4: Op_NONE) { MOVD } ,
    { 7F } (Flags: Opf_ModRM; Op1: Op_Qq; Op2: Op_Pq; Op3: Op_NONE; Op4: Op_NONE) { MOVQ } ,
    { 80 } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jvds; Op3: Op_NONE; Op4: Op_NONE) { JO } ,
    { 81 } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jvds; Op3: Op_NONE; Op4: Op_NONE) { JNO } ,
    { 82 } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jvds; Op3: Op_NONE; Op4: Op_NONE) { JB } ,
    { 83 } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jvds; Op3: Op_NONE; Op4: Op_NONE) { JNB } ,
    { 84 } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jvds; Op3: Op_NONE; Op4: Op_NONE) { JZ } ,
    { 85 } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jvds; Op3: Op_NONE; Op4: Op_NONE) { JNZ } ,
    { 86 } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jvds; Op3: Op_NONE; Op4: Op_NONE) { JBE } ,
    { 87 } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jvds; Op3: Op_NONE; Op4: Op_NONE) { JNBE } ,
    { 88 } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jvds; Op3: Op_NONE; Op4: Op_NONE) { JS } ,
    { 89 } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jvds; Op3: Op_NONE; Op4: Op_NONE) { JNS } ,
    { 8A } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jvds; Op3: Op_NONE; Op4: Op_NONE) { JP } ,
    { 8B } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jvds; Op3: Op_NONE; Op4: Op_NONE) { JNP } ,
    { 8C } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jvds; Op3: Op_NONE; Op4: Op_NONE) { JL } ,
    { 8D } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jvds; Op3: Op_NONE; Op4: Op_NONE) { JNL } ,
    { 8E } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jvds; Op3: Op_NONE; Op4: Op_NONE) { JLE } ,
    { 8F } (Flags: Opf_Jump; Op1: Op_NONE; Op2: Op_Jvds; Op3: Op_NONE; Op4: Op_NONE) { JNLE } ,
    { 90 } (Flags: Opf_ModRMExt; Op1: Op_Eb; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { SETO } ,
    { 91 } (Flags: Opf_ModRMExt; Op1: Op_Eb; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { SETNO } ,
    { 92 } (Flags: Opf_ModRMExt; Op1: Op_Eb; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { SETB } ,
    { 93 } (Flags: Opf_ModRMExt; Op1: Op_Eb; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { SETNB } ,
    { 94 } (Flags: Opf_ModRMExt; Op1: Op_Eb; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { SETZ } ,
    { 95 } (Flags: Opf_ModRMExt; Op1: Op_Eb; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { SETNZ } ,
    { 96 } (Flags: Opf_ModRMExt; Op1: Op_Eb; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { SETBE } ,
    { 97 } (Flags: Opf_ModRMExt; Op1: Op_Eb; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { SETNBE } ,
    { 98 } (Flags: Opf_ModRMExt; Op1: Op_Eb; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { SETS } ,
    { 99 } (Flags: Opf_ModRMExt; Op1: Op_Eb; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { SETNS } ,
    { 9A } (Flags: Opf_ModRMExt; Op1: Op_Eb; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { SETP } ,
    { 9B } (Flags: Opf_ModRMExt; Op1: Op_Eb; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { SETNP } ,
    { 9C } (Flags: Opf_ModRMExt; Op1: Op_Eb; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { SETL } ,
    { 9D } (Flags: Opf_ModRMExt; Op1: Op_Eb; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { SETNL } ,
    { 9E } (Flags: Opf_ModRMExt; Op1: Op_Eb; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { SETLE } ,
    { 9F } (Flags: Opf_ModRMExt; Op1: Op_Eb; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { SETNLE } ,
    { A0 } (Flags: Opf_NONE; Op1: Op_SS_rSP; Op2: Op_FS; Op3: Op_NONE; Op4: Op_NONE) { PUSH } ,
    { A1 } (Flags: Opf_NONE; Op1: Op_FS; Op2: Op_SS_rSP; Op3: Op_NONE; Op4: Op_NONE) { POP } ,
    { A2 } (Flags: Opf_NONE; Op1: Op_IA32_BIOS_SIGN_ID; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { CPUID } ,
    { A3 } (Flags: Opf_ModRM; Op1: Op_NONE; Op2: Op_Evqp; Op3: Op_Gvqp; Op4: Op_NONE) { BT } ,
    { A4 } (Flags: Opf_D or Opf_ModRM; Op1: Op_Evqp; Op2: Op_Gvqp; Op3: Op_Ib; Op4: Op_NONE) { SHLD } ,
    { A5 } (Flags: Opf_D or Opf_ModRM; Op1: Op_Evqp; Op2: Op_Gvqp; Op3: Op_CL; Op4: Op_NONE) { SHLD } ,
    { A6 } (Flags: Opf_D or Opf_ModRM; Op1: Op_Evqp; Op2: Op_Gvqp; Op3: Op_CL; Op4: Op_NONE) { SHLD } ,
    { A7 } (Flags: Opf_D or Opf_ModRM; Op1: Op_Evqp; Op2: Op_Gvqp; Op3: Op_CL; Op4: Op_NONE) { SHLD } ,
    { A8 } (Flags: Opf_NONE; Op1: Op_SS_rSP; Op2: Op_GS; Op3: Op_NONE; Op4: Op_NONE) { PUSH } ,
    { A9 } (Flags: Opf_NONE; Op1: Op_GS; Op2: Op_SS_rSP; Op3: Op_NONE; Op4: Op_NONE) { POP } ,
    { AA } (Flags: Opf_NONE; Op1: Op_Flags; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { RSM } ,
    { AB } (Flags: Opf_ModRM or Opf_L; Op1: Op_Evqp; Op2: Op_Gvqp; Op3: Op_NONE; Op4: Op_NONE) { BTS } ,
    { AC } (Flags: Opf_D or Opf_ModRM; Op1: Op_Evqp; Op2: Op_Gvqp; Op3: Op_Ib; Op4: Op_NONE) { SHRD } ,
    { AD } (Flags: Opf_D or Opf_ModRM; Op1: Op_Evqp; Op2: Op_Gvqp; Op3: Op_CL; Op4: Op_NONE) { SHRD } ,
    { AE } (Flags: Opf_ModRMExt; Op1: Op_Mstx; Op2: Op_ST; Op3: Op_ST1; Op4: Op_NONE) { FXSAVE } ,
    { AF } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { IMUL } ,
    { B0 } (Flags: Opf_D or Opf_W or Opf_ModRM or Opf_L; Op1: Op_Eb; Op2: Op_Gb; Op3: Op_NONE; Op4: Op_NONE) { CMPXCHG } ,
    { B1 } (Flags: Opf_D or Opf_W or Opf_ModRM or Opf_L; Op1: Op_Evqp; Op2: Op_Gvqp; Op3: Op_NONE; Op4: Op_NONE) { CMPXCHG } ,
    { B2 } (Flags: Opf_ModRM; Op1: Op_SS; Op2: Op_Mptp; Op3: Op_NONE; Op4: Op_NONE) { LSS } ,
    { B3 } (Flags: Opf_ModRM or Opf_L; Op1: Op_Evqp; Op2: Op_Gvqp; Op3: Op_NONE; Op4: Op_NONE) { BTR } ,
    { B4 } (Flags: Opf_ModRM; Op1: Op_FS; Op2: Op_Mptp; Op3: Op_NONE; Op4: Op_NONE) { LFS } ,
    { B5 } (Flags: Opf_ModRM; Op1: Op_GS; Op2: Op_Mptp; Op3: Op_NONE; Op4: Op_NONE) { LGS } ,
    { B6 } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Eb; Op3: Op_NONE; Op4: Op_NONE) { MOVZX } ,
    { B7 } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Ew; Op3: Op_NONE; Op4: Op_NONE) { MOVZX } ,
    { B8 } (Flags: Opf_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { JMPE } ,
    { B9 } (Flags: Opf_ModRM; Op1: Op_NONE; Op2: Op_G; Op3: Op_E; Op4: Op_NONE) { UD } ,
    { BA } (Flags: Opf_ModRMExt; Op1: Op_NONE; Op2: Op_Evqp; Op3: Op_Ib; Op4: Op_NONE) { BT } ,
    { BB } (Flags: Opf_ModRM or Opf_L; Op1: Op_Evqp; Op2: Op_Gvqp; Op3: Op_NONE; Op4: Op_NONE) { BTC } ,
    { BC } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { BSF } ,
    { BD } (Flags: Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Evqp; Op3: Op_NONE; Op4: Op_NONE) { BSR } ,
    { BE } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Eb; Op3: Op_NONE; Op4: Op_NONE) { MOVSX } ,
    { BF } (Flags: Opf_D or Opf_W or Opf_ModRM; Op1: Op_Gvqp; Op2: Op_Ew; Op3: Op_NONE; Op4: Op_NONE) { MOVSX } ,
    { C0 } (Flags: Opf_D or Opf_W or Opf_ModRM or Opf_L; Op1: Op_Eb; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { XADD } ,
    { C1 } (Flags: Opf_D or Opf_W or Opf_ModRM or Opf_L; Op1: Op_Evqp; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { XADD } ,
    { C2 } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_Ib; Op4: Op_NONE) { CMPPS } ,
    { C3 } (Flags: Opf_ModRM; Op1: Op_Mdqp; Op2: Op_Gdqp; Op3: Op_NONE; Op4: Op_NONE) { MOVNTI } ,
    { C4 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Rdqp; Op3: Op_Ib; Op4: Op_NONE) { PINSRW } ,
    { C5 } (Flags: Opf_ModRM; Op1: Op_Gdqp; Op2: Op_Nq; Op3: Op_Ib; Op4: Op_NONE) { PEXTRW } ,
    { C6 } (Flags: Opf_ModRM; Op1: Op_Vps; Op2: Op_Wps; Op3: Op_Ib; Op4: Op_NONE) { SHUFPS } ,
    { C7 } (Flags: Opf_L or Opf_ModRMExt; Op1: Op_Mq; Op2: Op_EBX; Op3: Op_eCX; Op4: Op_NONE) { CMPXCHG8B } ,
    { C8 } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { BSWAP } ,
    { C9 } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { BSWAP } ,
    { CA } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { BSWAP } ,
    { CB } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { BSWAP } ,
    { CC } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { BSWAP } ,
    { CD } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { BSWAP } ,
    { CE } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { BSWAP } ,
    { CF } (Flags: Opf_NONE; Op1: Op_Zvqp; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { BSWAP } ,
    { D0 } (Flags: Opf_ModRM; Op1: Op_Vpd; Op2: Op_Wpd; Op3: Op_NONE; Op4: Op_NONE) { ADDSUBPD } ,
    { D1 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PSRLW } ,
    { D2 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PSRLD } ,
    { D3 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PSRLQ } ,
    { D4 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PADDQ } ,
    { D5 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PMULLW } ,
    { D6 } (Flags: Opf_ModRM; Op1: Op_Wq; Op2: Op_Vq; Op3: Op_NONE; Op4: Op_NONE) { MOVQ } ,
    { D7 } (Flags: Opf_ModRM; Op1: Op_Gdqp; Op2: Op_Nq; Op3: Op_NONE; Op4: Op_NONE) { PMOVMSKB } ,
    { D8 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PSUBUSB } ,
    { D9 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PSUBUSW } ,
    { DA } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PMINUB } ,
    { DB } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qd; Op3: Op_NONE; Op4: Op_NONE) { PAND } ,
    { DC } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PADDUSB } ,
    { DD } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PADDUSW } ,
    { DE } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PMAXUB } ,
    { DF } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PANDN } ,
    { E0 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PAVGB } ,
    { E1 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PSRAW } ,
    { E2 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PSRAD } ,
    { E3 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PAVGW } ,
    { E4 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PMULHUW } ,
    { E5 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PMULHW } ,
    { E6 } (Flags: Opf_ModRM; Op1: Op_Vdq; Op2: Op_Wpd; Op3: Op_NONE; Op4: Op_NONE) { CVTPD2DQ } ,
    { E7 } (Flags: Opf_ModRM; Op1: Op_Mq; Op2: Op_Pq; Op3: Op_NONE; Op4: Op_NONE) { MOVNTQ } ,
    { E8 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PSUBSB } ,
    { E9 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PSUBSW } ,
    { EA } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PMINSW } ,
    { EB } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { POR } ,
    { EC } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PADDSB } ,
    { ED } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PADDSW } ,
    { EE } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PMAXSW } ,
    { EF } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PXOR } ,
    { F0 } (Flags: Opf_ModRM; Op1: Op_Vdq; Op2: Op_Mdq; Op3: Op_NONE; Op4: Op_NONE) { LDDQU } ,
    { F1 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PSLLW } ,
    { F2 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PSLLD } ,
    { F3 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PSLLQ } ,
    { F4 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PMULUDQ } ,
    { F5 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qd; Op3: Op_NONE; Op4: Op_NONE) { PMADDWD } ,
    { F6 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PSADBW } ,
    { F7 } (Flags: Opf_ModRM; Op1: Op_DS_rDI; Op2: Op_Nq; Op3: Op_NONE; Op4: Op_NONE) { MASKMOVQ } ,
    { F8 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PSUBB } ,
    { F9 } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PSUBW } ,
    { FA } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PSUBD } ,
    { FB } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PSUBQ } ,
    { FC } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PADDB } ,
    { FD } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PADDW } ,
    { FE } (Flags: Opf_ModRM; Op1: Op_Pq; Op2: Op_Qq; Op3: Op_NONE; Op4: Op_NONE) { PADDD } ,
    { FF } (Flags: Op_NONE; Op1: Op_NONE; Op2: Op_NONE; Op3: Op_NONE; Op4: Op_NONE) { Invalid }
    );
