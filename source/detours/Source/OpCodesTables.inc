// **************************************************************************************************
// Part of x86 Instruction Decode Library [InstDecode]
// 
// https://github.com/MahdiSafsafi/DDetours
//
// This Source Code Form is subject to the terms of the Mozilla 
// Public License, v. 2.0. If a copy of the MPL was not distributed 
// with this file, You can obtain one at
// https://mozilla.org/MPL/2.0/.
// **************************************************************************************************

{ Reference :
	-sandpile.org
	-Intel® 64 and IA-32 Architectures Software Developer’s Manual Vol 2
}

{ ============================================
			Index -> DecoderProc
  ============================================ }
{ 00 = Decode_InvalidOpCode}
{ 01 = Decode_NA_ModRm}
{ 02 = Decode_NA_Ib}
{ 03 = Decode_NA_Iz}
{ 04 = Decode_NA_I64}
{ 05 = Decode_Escape_2_Byte}
{ 06 = Decode_ES_Prefix}
{ 07 = Decode_CS_Prefix}
{ 08 = Decode_SS_Prefix}
{ 09 = Decode_DS_Prefix}
{ 10 = Decode_REX_Prefix}
{ 11 = Decode_NA_D64}
{ 12 = Decode_NA_ModRm_I64}
{ 13 = Decode_FS_Prefix}
{ 14 = Decode_GS_Prefix}
{ 15 = Decode_OPSIZE_Prefix}
{ 16 = Decode_ADSIZE_Prefix}
{ 17 = Decode_NA_Iz_D64}
{ 18 = Decode_NA_ModRm_Iz}
{ 19 = Decode_NA_Ib_D64}
{ 20 = Decode_NA_ModRm_Ib}
{ 21 = Decode_NA}
{ 22 = Decode_NA_Jb_Df64}
{ 23 = Decode_Group_1}
{ 24 = Decode_Group_1A}
{ 25 = Decode_NA_CALL_Ap_I64}
{ 26 = Decode_NA_OfstV}
{ 27 = Decode_NA_Iv}
{ 28 = Decode_Group_2}
{ 29 = Decode_NA_RET_Iw_Df64}
{ 30 = Decode_NA_RET_Df64}
{ 31 = Decode_VEX3_Prefix}
{ 32 = Decode_VEX2_Prefix}
{ 33 = Decode_Group_11}
{ 34 = Decode_NA_Iw_Ib_D64}
{ 35 = Decode_NA_RET_Iw}
{ 36 = Decode_NA_RET}
{ 37 = Decode_NA_Ib_I64}
{ 38 = Decode_Escape_FPU_D8}
{ 39 = Decode_Escape_FPU_D9}
{ 40 = Decode_Escape_FPU_DA}
{ 41 = Decode_Escape_FPU_DB}
{ 42 = Decode_Escape_FPU_DC}
{ 43 = Decode_Escape_FPU_DD}
{ 44 = Decode_Escape_FPU_DE}
{ 45 = Decode_Escape_FPU_DF}
{ 46 = Decode_NA_CALL_Jz_Df64}
{ 47 = Decode_NA_JMP_Jz_Df64}
{ 48 = Decode_NA_JMP_Ap_I64}
{ 49 = Decode_NA_JMP_Jb_Df64}
{ 50 = Decode_LOCK_Prefix}
{ 51 = Decode_REPNE_Prefix}
{ 52 = Decode_REPE_Prefix}
{ 53 = Decode_Group_3}
{ 54 = Decode_Group_4_INC_DEC}
{ 55 = Decode_Group_5_INC_DEC}
{ 56 = Decode_Group_6}
{ 57 = Decode_Group_7}
{ 58 = Decode_NA_CALL}
{ 59 = Decode_NA_66_F2_F3_ModRm}
{ 60 = Decode_NA_66_ModRm}
{ 61 = Decode_NA_66_F3_ModRm}
{ 62 = Decode_Group_16}
{ 63 = Decode_NA_ModRm_F64}
{ 64 = Decode_Escape_3_Byte}
{ 65 = Decode_NA_F3_ModRm}
{ 66 = Decode_66_ModRm}
{ 67 = Decode_NA_66_F2_F3_ModRm_Ib}
{ 68 = Decode_Group_12}
{ 69 = Decode_Group_13}
{ 70 = Decode_Group_14}
{ 71 = Decode_66_F2_ModRm}
{ 72 = Decode_NA_Jz_Df64}
{ 73 = Decode_Group_15}
{ 74 = Decode_F3_ModRm}
{ 75 = Decode_Group_10_UD2}
{ 76 = Decode_Group_8}
{ 77 = Decode_NA_66_ModRm_Ib}
{ 78 = Decode_Group_9}
{ 79 = Decode_66_F2_F3_ModRm}
{ 80 = Decode_F2_ModRm}
{ 81 = Decode_SP_T38_F0_F7}
{ 82 = Decode_66_ModRm_Ib}
{ 83 = Decode_F2_ModRm_Ib}

{============================================
                  OneByteTable                    
============================================}
const OneByteTable : array[Byte] of Byte =(//
{0x00} 01, {ADD}
{0x01} 01, {ADD}
{0x02} 01, {ADD}
{0x03} 01, {ADD}
{0x04} 02, {ADD}
{0x05} 03, {ADD}
{0x06} 04, {PUSH}
{0x07} 04, {POP}
{0x08} 01, {OR}
{0x09} 01, {OR}
{0x0A} 01, {OR}
{0x0B} 01, {OR}
{0x0C} 02, {OR}
{0x0D} 03, {OR}
{0x0E} 04, {PUSH}
{0x0F} 05, {Escape_2_Byte}
{0x10} 01, {ADC}
{0x11} 01, {ADC}
{0x12} 01, {ADC}
{0x13} 01, {ADC}
{0x14} 02, {ADC}
{0x15} 03, {ADC}
{0x16} 04, {PUSH}
{0x17} 04, {POP}
{0x18} 01, {SBB}
{0x19} 01, {SBB}
{0x1A} 01, {SBB}
{0x1B} 01, {SBB}
{0x1C} 02, {SBB}
{0x1D} 03, {SBB}
{0x1E} 04, {PUSH}
{0x1F} 04, {POP}
{0x20} 01, {AND}
{0x21} 01, {AND}
{0x22} 01, {AND}
{0x23} 01, {AND}
{0x24} 02, {AND}
{0x25} 03, {AND}
{0x26} 06, {ES_Prefix}
{0x27} 04, {DAA}
{0x28} 01, {SUB}
{0x29} 01, {SUB}
{0x2A} 01, {SUB}
{0x2B} 01, {SUB}
{0x2C} 02, {SUB}
{0x2D} 03, {SUB}
{0x2E} 07, {CS_Prefix}
{0x2F} 04, {DAS}
{0x30} 01, {XOR}
{0x31} 01, {XOR}
{0x32} 01, {XOR}
{0x33} 01, {XOR}
{0x34} 02, {XOR}
{0x35} 03, {XOR}
{0x36} 08, {SS_Prefix}
{0x37} 04, {AAA}
{0x38} 01, {CMP}
{0x39} 01, {CMP}
{0x3A} 01, {CMP}
{0x3B} 01, {CMP}
{0x3C} 02, {CMP}
{0x3D} 03, {CMP}
{0x3E} 09, {DS_Prefix}
{0x3F} 04, {AAS}
{0x40} 10, {INC/REX_Prefix}
{0x41} 10, {INC/REX_Prefix}
{0x42} 10, {INC/REX_Prefix}
{0x43} 10, {INC/REX_Prefix}
{0x44} 10, {INC/REX_Prefix}
{0x45} 10, {INC/REX_Prefix}
{0x46} 10, {INC/REX_Prefix}
{0x47} 10, {INC/REX_Prefix}
{0x48} 10, {DEC/REX_Prefix}
{0x49} 10, {DEC/REX_Prefix}
{0x4A} 10, {DEC/REX_Prefix}
{0x4B} 10, {DEC/REX_Prefix}
{0x4C} 10, {DEC/REX_Prefix}
{0x4D} 10, {DEC/REX_Prefix}
{0x4E} 10, {DEC/REX_Prefix}
{0x4F} 10, {DEC/REX_Prefix}
{0x50} 11, {PUSH}
{0x51} 11, {PUSH}
{0x52} 11, {PUSH}
{0x53} 11, {PUSH}
{0x54} 11, {PUSH}
{0x55} 11, {PUSH}
{0x56} 11, {PUSH}
{0x57} 11, {PUSH}
{0x58} 11, {POP}
{0x59} 11, {POP}
{0x5A} 11, {POP}
{0x5B} 11, {POP}
{0x5C} 11, {POP}
{0x5D} 11, {POP}
{0x5E} 11, {POP}
{0x5F} 11, {POP}
{0x60} 04, {PUSHAD/PUSHA}
{0x61} 04, {POPAD/POPA}
{0x62} 12, {BOUND}
{0x63} 01, {ARPL/MOVSXD}
{0x64} 13, {FS_Prefix}
{0x65} 14, {GS_Prefix}
{0x66} 15, {OPSIZE_Prefix}
{0x67} 16, {ADSIZE_Prefix}
{0x68} 17, {PUSH}
{0x69} 18, {IMUL}
{0x6A} 19, {PUSH}
{0x6B} 20, {IMUL}
{0x6C} 21, {INS}
{0x6D} 21, {INS}
{0x6E} 21, {OUTS}
{0x6F} 21, {OUTS}
{0x70} 22, {JO}
{0x71} 22, {JNO}
{0x72} 22, {JB}
{0x73} 22, {JNB}
{0x74} 22, {JZ}
{0x75} 22, {JNZ}
{0x76} 22, {JBE}
{0x77} 22, {JNBE}
{0x78} 22, {JS}
{0x79} 22, {JNS}
{0x7A} 22, {JP}
{0x7B} 22, {JNP}
{0x7C} 22, {JL}
{0x7D} 22, {JNL}
{0x7E} 22, {JLE}
{0x7F} 22, {JNLE}
{0x80} 23, {group_1}
{0x81} 23, {group_1}
{0x82} 23, {group_1*}
{0x83} 23, {group_1}
{0x84} 01, {TEST}
{0x85} 01, {TEST}
{0x86} 01, {XCHG}
{0x87} 01, {XCHG}
{0x88} 01, {MOV}
{0x89} 01, {MOV}
{0x8A} 01, {MOV}
{0x8B} 01, {MOV}
{0x8C} 01, {MOV}
{0x8D} 01, {LEA}
{0x8E} 01, {MOV}
{0x8F} 24, {group_1A}
{0x90} 21, {PAUSE/NOP}
{0x91} 21, {XCHG}
{0x92} 21, {XCHG}
{0x93} 21, {XCHG}
{0x94} 21, {XCHG}
{0x95} 21, {XCHG}
{0x96} 21, {XCHG}
{0x97} 21, {XCHG}
{0x98} 21, {CWDE/CBW/CDQE}
{0x99} 21, {CDQ/CWD/CQO}
{0x9A} 25, {CALL}
{0x9B} 21, {WAIT}
{0x9C} 11, {PUSHF}
{0x9D} 11, {POPF}
{0x9E} 21, {SAHF}
{0x9F} 21, {LAHF}
{0xA0} 26, {MOV}
{0xA1} 26, {MOV}
{0xA2} 26, {MOV}
{0xA3} 26, {MOV}
{0xA4} 21, {MOVS}
{0xA5} 21, {MOVS}
{0xA6} 21, {CMPS}
{0xA7} 21, {CMPS}
{0xA8} 02, {TEST}
{0xA9} 03, {TEST}
{0xAA} 21, {STOS}
{0xAB} 21, {STOS}
{0xAC} 21, {LODS}
{0xAD} 21, {LODS}
{0xAE} 21, {SCAS}
{0xAF} 21, {SCAS}
{0xB0} 02, {MOV}
{0xB1} 02, {MOV}
{0xB2} 02, {MOV}
{0xB3} 02, {MOV}
{0xB4} 02, {MOV}
{0xB5} 02, {MOV}
{0xB6} 02, {MOV}
{0xB7} 02, {MOV}
{0xB8} 27, {MOV}
{0xB9} 27, {MOV}
{0xBA} 27, {MOV}
{0xBB} 27, {MOV}
{0xBC} 27, {MOV}
{0xBD} 27, {MOV}
{0xBE} 27, {MOV}
{0xBF} 27, {MOV}
{0xC0} 28, {group_2}
{0xC1} 28, {group_2}
{0xC2} 29, {RET}
{0xC3} 30, {RET}
{0xC4} 31, {LES/VEX3_Prefix}
{0xC5} 32, {LDS/VEX2_Prefix}
{0xC6} 33, {group_11}
{0xC7} 33, {group_11}
{0xC8} 34, {ENTER}
{0xC9} 11, {LEAVE}
{0xCA} 35, {RET}
{0xCB} 36, {RET}
{0xCC} 21, {INT3}
{0xCD} 02, {INT}
{0xCE} 04, {INTO}
{0xCF} 36, {IRET}
{0xD0} 28, {group_2}
{0xD1} 28, {group_2}
{0xD2} 28, {group_2}
{0xD3} 28, {group_2}
{0xD4} 37, {AAM}
{0xD5} 37, {AAD}
{0xD6} 04, {SETALC}
{0xD7} 21, {XLAT}
{0xD8} 38, {Escape_FPU_D8}
{0xD9} 39, {Escape_FPU_D9}
{0xDA} 40, {Escape_FPU_DA}
{0xDB} 41, {Escape_FPU_DB}
{0xDC} 42, {Escape_FPU_DC}
{0xDD} 43, {Escape_FPU_DD}
{0xDE} 44, {Escape_FPU_DE}
{0xDF} 45, {Escape_FPU_DF}
{0xE0} 22, {LOOPNZ/LOOPNE}
{0xE1} 22, {LOOPZ/LOOPE}
{0xE2} 22, {LOOP}
{0xE3} 22, {JRCX/JECX/JCXZ}
{0xE4} 02, {IN}
{0xE5} 02, {IN}
{0xE6} 02, {OUT}
{0xE7} 02, {OUT}
{0xE8} 46, {CALL}
{0xE9} 47, {JMP}
{0xEA} 48, {JMP}
{0xEB} 49, {JMP}
{0xEC} 21, {IN}
{0xED} 21, {IN}
{0xEE} 21, {OUT}
{0xEF} 21, {OUT}
{0xF0} 50, {LOCK_Prefix}
{0xF1} 21, {INT1}
{0xF2} 51, {REPNE_Prefix}
{0xF3} 52, {REPE_Prefix}
{0xF4} 21, {HLT}
{0xF5} 21, {CMC}
{0xF6} 53, {group_3}
{0xF7} 53, {group_3}
{0xF8} 21, {CLC}
{0xF9} 21, {STC}
{0xFA} 21, {CLI}
{0xFB} 21, {STI}
{0xFC} 21, {CLD}
{0xFD} 21, {STD}
{0xFE} 54, {group_4_INC_DEC}
{0xFF} 55 {group_5_INC_DEC}
);
{============================================
                  TwoByteTable                    
============================================}
const TwoByteTable : array[Byte] of Byte =(//
{0x00} 56, {group_6}
{0x01} 57, {group_7}
{0x02} 01, {LAR}
{0x03} 01, {LSL}
{0x04} 21, {LOADALL?/RESET?/HANG?}
{0x05} 58, {LOADALL/SYSCALL}
{0x06} 21, {CLTS}
{0x07} 36, {LOADALL/SYSRET}
{0x08} 21, {INVD}
{0x09} 21, {WBINVD}
{0x0A} 21, {CL1INVMB}
{0x0B} 21, {UD1}
{0x0C} 00, {InvalidOpCode}
{0x0D} 21, {3DNow}
{0x0E} 21, {3DNow}
{0x0F} 21, {3DNow}
{0x10} 59, {VMOVUPD/VMOVSD/VMOVSS/VMOVUPS}
{0x11} 59, {VMOVUPD/VMOVSD/VMOVSS/VMOVUPS}
{0x12} 59, {VMOVLPD/VMOVDDUP/VMOVSLDUP/VMOVLPS/VMOVHLPS}
{0x13} 60, {InvalidOpCode/VMOVLPD/VMOVLPS}
{0x14} 60, {InvalidOpCode/VUNPCKLPD/VUNPCKLPS}
{0x15} 60, {InvalidOpCode/VUNPCKHPD/VUNPCKHPS}
{0x16} 61, {InvalidOpCode/VMOVHPD/VMOVSHDUP/VMOVLHPS/VMOVHPS}
{0x17} 60, {InvalidOpCode/VMOVHPD/VMOVHPS}
{0x18} 62, {group_16}
{0x19} 62, {group_16}
{0x1A} 62, {group_16}
{0x1B} 62, {group_16}
{0x1C} 62, {group_16}
{0x1D} 62, {group_16}
{0x1E} 62, {group_16}
{0x1F} 62, {group_16}
{0x20} 63, {MOV}
{0x21} 63, {MOV}
{0x22} 63, {MOV}
{0x23} 63, {MOV}
{0x24} 63, {MOV}
{0x25} 00, {InvalidOpCode}
{0x26} 63, {MOV}
{0x27} 00, {InvalidOpCode}
{0x28} 60, {InvalidOpCode/VMOVAPD/VMOVAPS}
{0x29} 60, {InvalidOpCode/VMOVAPD/VMOVAPS}
{0x2A} 59, {VCVTSI2SD/CVTPI2PD/VCVTSI2SS/CVTPI2PS}
{0x2B} 59, {MOVNTSD/MOVNTSS/VMOVNTPD/VMOVNTPS}
{0x2C} 59, {CVTTPD2PI/VCVTTSD2SI/CVTTPS2PI/VCVTTSS2SI}
{0x2D} 59, {CVTPD2PI/VCVTSD2SI/CVTPS2PI/VCVTSS2SI}
{0x2E} 60, {InvalidOpCode/VUCOMISD/VUCOMISS}
{0x2F} 60, {InvalidOpCode/VCOMISD/VCOMISS}
{0x30} 21, {WRMSR}
{0x31} 21, {RDTSC}
{0x32} 21, {RDMSR}
{0x33} 21, {RDPMC}
{0x34} 21, {SYSENTER}
{0x35} 21, {SYSEXIT}
{0x36} 00, {InvalidOpCode}
{0x37} 21, {GETSEC}
{0x38} 64, {Escape_3_Byte}
{0x39} 00, {InvalidOpCode}
{0x3A} 64, {Escape_3_Byte}
{0x3B} 00, {InvalidOpCode}
{0x3C} 00, {InvalidOpCode}
{0x3D} 00, {InvalidOpCode}
{0x3E} 00, {InvalidOpCode}
{0x3F} 00, {InvalidOpCode}
{0x40} 01, {CMOVO}
{0x41} 01, {CMOVNO}
{0x42} 01, {CMOVB}
{0x43} 01, {CMOVNB}
{0x44} 01, {CMOVZ}
{0x45} 01, {CMOVNZ}
{0x46} 01, {CMOVBE}
{0x47} 01, {CMOVNBE}
{0x48} 01, {CMOVS}
{0x49} 01, {CMOVNS}
{0x4A} 01, {CMOVP}
{0x4B} 01, {CMOVNP}
{0x4C} 01, {CMOVL}
{0x4D} 01, {CMOVNL}
{0x4E} 01, {CMOVLE}
{0x4F} 01, {CMOVNLE}
{0x50} 60, {InvalidOpCode/VMOVMSKPD/VMOVMSKPS}
{0x51} 59, {VSQRTPD/VSQRTSD/VSQRTSS/VSQRTPS}
{0x52} 65, {InvalidOpCode/VRSQRTSS/VRSQRTPS}
{0x53} 65, {InvalidOpCode/VRCPSS/VRCPPS}
{0x54} 60, {InvalidOpCode/VANDPD/VANDPS}
{0x55} 60, {InvalidOpCode/VANDNPD/VANDNPS}
{0x56} 60, {InvalidOpCode/VORPD/VORPS}
{0x57} 60, {InvalidOpCode/VXORPD/VXORPS}
{0x58} 59, {VADDPD/VADDSD/VADDSS/VADDPS}
{0x59} 59, {VMULPD/VMULSD/VMULSS/VMULPS}
{0x5A} 59, {VCVTPD2PS/VCVTSD2SS/VCVTSS2SD/VCVTPS2PD}
{0x5B} 61, {InvalidOpCode/VCVTPS2DQ/VCVTTPS2DQ/VCVTDQ2PS}
{0x5C} 59, {VSUBPD/VSUBSD/VSUBSS/VSUBPS}
{0x5D} 59, {VMINPD/VMINSD/VMINSS/VMINPS}
{0x5E} 59, {VDIVPD/VDIVSD/VDIVSS/VDIVPS}
{0x5F} 59, {VMAXPD/VMAXSD/VMAXSS/VMAXPS}
{0x60} 60, {PUNPCKLBW/VPUNPCKLBW}
{0x61} 60, {PUNPCKLWD/VPUNPCKLWD}
{0x62} 60, {PUNPCKLDQ/VPUNPCKLDQ}
{0x63} 60, {PACKSSWB/VPACKSSWB}
{0x64} 60, {PCMPGTB/VPCMPGTB}
{0x65} 60, {PCMPGTW/VPCMPGTW}
{0x66} 60, {PCMPGTD/VPCMPGTD}
{0x67} 60, {PACKUSWB/VPACKUSWB}
{0x68} 60, {PUNPCKHBW/InvalidOpCode/VPUNPCKHBW}
{0x69} 60, {PUNPCKHWD/InvalidOpCode/VPUNPCKHWD}
{0x6A} 60, {PUNPCKHDQ/InvalidOpCode/VPUNPCKHDQ}
{0x6B} 60, {PACKSSDW/InvalidOpCode/VPACKSSDW}
{0x6C} 66, {InvalidOpCode/VPUNPCKLQDQ}
{0x6D} 66, {InvalidOpCode/VPUNPCKHQDQ}
{0x6E} 60, {MOVDQ/InvalidOpCode/VMOVDQ}
{0x6F} 61, {MOVQ/InvalidOpCode/VMOVDQA/VMOVDQU}
{0x70} 67, {VPSHUFD/VPSHUFLW/VPSHUFHW/PSHUFW}
{0x71} 68, {group_12}
{0x72} 69, {group_13}
{0x73} 70, {group_14}
{0x74} 60, {PCMPEQB/InvalidOpCode/VPCMPEQB}
{0x75} 60, {PCMPEQW/InvalidOpCode/VPCMPEQW}
{0x76} 60, {PCMPEQD/InvalidOpCode/VPCMPEQD}
{0x77} 21, {EMMS/VZEROUPPER/VZEROALL/InvalidOpCode}
{0x78} 01, {VMREAD/InvalidOpCode}
{0x79} 01, {VMWRITE/InvalidOpCode}
{0x7A} 21, {SSE5A/InvalidOpCode}
{0x7B} 21, {SSE5A/InvalidOpCode}
{0x7C} 71, {InvalidOpCode/VHADDPD/VHADDPS}
{0x7D} 71, {InvalidOpCode/VHSUBPD/VHSUBPS}
{0x7E} 61, {MOVDQ/InvalidOpCode/VMOVDQ/VMOVQ}
{0x7F} 61, {MOVQ/InvalidOpCode/VMOVDQA/VMOVDQU}
{0x80} 72, {JO}
{0x81} 72, {JNO}
{0x82} 72, {JB}
{0x83} 72, {JNB}
{0x84} 72, {JZ}
{0x85} 72, {JNZ}
{0x86} 72, {JBE}
{0x87} 72, {JNBE}
{0x88} 72, {JS}
{0x89} 72, {JNS}
{0x8A} 72, {JP}
{0x8B} 72, {JNP}
{0x8C} 72, {JL}
{0x8D} 72, {JNL}
{0x8E} 72, {JLE}
{0x8F} 72, {JNLE}
{0x90} 01, {SETO}
{0x91} 01, {SETNO}
{0x92} 01, {SETB}
{0x93} 01, {SETNB}
{0x94} 01, {SETZ}
{0x95} 01, {SETNZ}
{0x96} 01, {SETBE}
{0x97} 01, {SETNBE}
{0x98} 01, {SETS}
{0x99} 01, {SETNS}
{0x9A} 01, {SETP}
{0x9B} 01, {SETNP}
{0x9C} 01, {SETL}
{0x9D} 01, {SETNL}
{0x9E} 01, {SETLE}
{0x9F} 01, {SETNLE}
{0xA0} 11, {PUSH}
{0xA1} 11, {POP}
{0xA2} 21, {CPUID}
{0xA3} 01, {BT}
{0xA4} 20, {SHLD}
{0xA5} 01, {SHLD}
{0xA6} 00, {InvalidOpCode}
{0xA7} 00, {InvalidOpCode}
{0xA8} 11, {PUSH}
{0xA9} 11, {POP}
{0xAA} 21, {RSM}
{0xAB} 01, {BTS}
{0xAC} 20, {SHRD}
{0xAD} 01, {SHRD}
{0xAE} 73, {group_15}
{0xAF} 01, {IMUL}
{0xB0} 01, {CMPXCHG}
{0xB1} 01, {CMPXCHG}
{0xB2} 01, {LSS}
{0xB3} 01, {BTR}
{0xB4} 01, {LFS}
{0xB5} 01, {LGS}
{0xB6} 01, {MOVZX}
{0xB7} 01, {MOVZX}
{0xB8} 74, {POPCNT/InvalidOpCode}
{0xB9} 75, {group_10_UD2/InvalidOpCode}
{0xBA} 76, {group_8/InvalidOpCode}
{0xBB} 01, {BTC/InvalidOpCode}
{0xBC} 65, {BSF/TZCNT}
{0xBD} 65, {BSR/LZCNT}
{0xBE} 01, {MOVSX/InvalidOpCode}
{0xBF} 01, {MOVSX/InvalidOpCode}
{0xC0} 59, {XADD}
{0xC1} 59, {XADD}
{0xC2} 67, {VCMPccPD/VCMPccSD/VCMPccSS/VCMPccPS}
{0xC3} 01, {InvalidOpCode/MOVNTI}
{0xC4} 77, {InvalidOpCode/VPINSRW/PINSRW}
{0xC5} 77, {InvalidOpCode/VPEXTRW/PEXTRW}
{0xC6} 77, {InvalidOpCode/VSHUFPD/VSHUFPS}
{0xC7} 78, {group_9}
{0xC8} 21, {BSWAP}
{0xC9} 21, {BSWAP}
{0xCA} 21, {BSWAP}
{0xCB} 21, {BSWAP}
{0xCC} 21, {BSWAP}
{0xCD} 21, {BSWAP}
{0xCE} 21, {BSWAP}
{0xCF} 21, {BSWAP}
{0xD0} 71, {InvalidOpCode/VADDSUBPD/VADDSUBPS}
{0xD1} 60, {PSRLW/InvalidOpCode/VPSRLW}
{0xD2} 60, {PSRLD/InvalidOpCode/VPSRLD}
{0xD3} 60, {PSRLQ/InvalidOpCode/VPSRLQ}
{0xD4} 60, {InvalidOpCode/VPADDQ/PADDQ}
{0xD5} 60, {PMULLW/InvalidOpCode/VPMULLW}
{0xD6} 79, {InvalidOpCode/MOVDQ2Q/MOVQ2DQ/VMOVQ}
{0xD7} 60, {InvalidOpCode/VPMOVMSKB/PMOVMSKB}
{0xD8} 60, {PSUBUSB/VPSUBUSB}
{0xD9} 60, {PSUBUSW/VPSUBUSW}
{0xDA} 60, {VPMINUB/PMINUB}
{0xDB} 60, {PAND/VPAND}
{0xDC} 60, {PADDUSB/VPADDUSB}
{0xDD} 60, {PADDUSW/VPADDUSW}
{0xDE} 60, {VPMAXUB/PMAXUB}
{0xDF} 60, {PANDN/VPANDN}
{0xE0} 60, {InvalidOpCode/VPAVGB/PAVGB}
{0xE1} 60, {PSRAW/InvalidOpCode/VPSRAW}
{0xE2} 60, {PSRAD/InvalidOpCode/VPSRAD}
{0xE3} 60, {InvalidOpCode/VPAVGW/PAVGW}
{0xE4} 60, {InvalidOpCode/VPMULHUW/PMULHUW}
{0xE5} 60, {PMULHW/InvalidOpCode/VPMULHW}
{0xE6} 79, {InvalidOpCode/VCVTTPD2DQ/VCVTPD2DQ/VCVTDQ2PD}
{0xE7} 60, {InvalidOpCode/VMOVNTDQ/MOVNTQ}
{0xE8} 60, {PSUBSB/VPSUBSB}
{0xE9} 60, {PSUBSW/VPSUBSW}
{0xEA} 60, {VPMINSW/PMINSW}
{0xEB} 60, {POR/VPOR}
{0xEC} 60, {PADDSB/VPADDSB}
{0xED} 60, {PADDSW/VPADDSW}
{0xEE} 60, {VPMAXSW/PMAXSW}
{0xEF} 60, {PXOR/VPXOR}
{0xF0} 80, {InvalidOpCode/VLDDQU}
{0xF1} 60, {PSLLW/InvalidOpCode/VPSLLW}
{0xF2} 60, {PSLLD/InvalidOpCode/VPSLLD}
{0xF3} 60, {PSLLQ/InvalidOpCode/VPSLLQ}
{0xF4} 60, {InvalidOpCode/VPMULUDQ/PMULUDQ}
{0xF5} 60, {PMADDWD/InvalidOpCode/VPMADDWD}
{0xF6} 60, {InvalidOpCode/VPSADBW/PSADBW}
{0xF7} 60, {InvalidOpCode/VMASKMOVDQU/MASKMOVQ}
{0xF8} 60, {PSUBB/VPSUBB}
{0xF9} 60, {PSUBW/VPSUBW}
{0xFA} 60, {PSUBD/VPSUBD}
{0xFB} 60, {VPSUBQ/PSUBQ}
{0xFC} 60, {PADDB/VPADDB}
{0xFD} 60, {PADDW/VPADDW}
{0xFE} 60, {PADDD/VPADDD}
{0xFF} 00 {InvalidOpCode}
);
{============================================
                  ThreeByteTable38                    
============================================}
const ThreeByteTable38 : array[Byte] of Byte =(//
{0x00} 60, {VPSHUFB/PSHUFB}
{0x01} 60, {VPHADDW/PHADDW}
{0x02} 60, {VPHADDD/PHADDD}
{0x03} 60, {VPHADDSW/PHADDSW}
{0x04} 60, {VPMADDUBSW/PMADDUBSW}
{0x05} 60, {VPHSUBW/PHSUBW}
{0x06} 60, {VPHSUBD/PHSUBD}
{0x07} 60, {VPHSUBSW/PHSUBSW}
{0x08} 60, {VPSIGNB/PSIGNB}
{0x09} 60, {VPSIGNW/PSIGNW}
{0x0A} 60, {VPSIGND/PSIGND}
{0x0B} 60, {VPMULHRSW/PMULHRSW}
{0x0C} 66, {VPERMILPS/InvalidOpCode}
{0x0D} 66, {VPERMILPD/InvalidOpCode}
{0x0E} 66, {VTESTPS/InvalidOpCode}
{0x0F} 66, {VTESTPD/InvalidOpCode}
{0x10} 66, {InvalidOpCode/PBLENDVB}
{0x11} 00, {InvalidOpCode}
{0x12} 00, {InvalidOpCode}
{0x13} 66, {VCVTPH2PS/InvalidOpCode}
{0x14} 66, {InvalidOpCode/BLENDVPS}
{0x15} 66, {InvalidOpCode/BLENDVPD}
{0x16} 66, {VPERMPS/InvalidOpCode}
{0x17} 66, {InvalidOpCode/VPTEST}
{0x18} 66, {VBROADCASTSS/InvalidOpCode}
{0x19} 66, {VBROADCASTSD/InvalidOpCode}
{0x1A} 66, {VBROADCASTF128/InvalidOpCode}
{0x1B} 00, {InvalidOpCode}
{0x1C} 60, {VPABSB/PABSB}
{0x1D} 60, {VPABSW/PABSW}
{0x1E} 60, {VPABSD/PABSD}
{0x1F} 00, {InvalidOpCode}
{0x20} 66, {InvalidOpCode/VPMOVSXBW}
{0x21} 66, {InvalidOpCode/VPMOVSXBD}
{0x22} 66, {InvalidOpCode/VPMOVSXBQ}
{0x23} 66, {InvalidOpCode/VPMOVSXWD}
{0x24} 66, {InvalidOpCode/VPMOVSXWQ}
{0x25} 66, {InvalidOpCode/VPMOVSXDQ}
{0x26} 00, {InvalidOpCode}
{0x27} 00, {InvalidOpCode}
{0x28} 66, {InvalidOpCode/VPMULDQ}
{0x29} 66, {InvalidOpCode/VPCMPEQQ}
{0x2A} 66, {InvalidOpCode/VMOVNTDQA}
{0x2B} 66, {InvalidOpCode/VPACKUSDW}
{0x2C} 66, {VMASKMOVPS/InvalidOpCode}
{0x2D} 66, {VMASKMOVPD/InvalidOpCode}
{0x2E} 66, {VMASKMOVPS/InvalidOpCode}
{0x2F} 66, {VMASKMOVPD/InvalidOpCode}
{0x30} 66, {InvalidOpCode/VPMOVZXBW}
{0x31} 66, {InvalidOpCode/VPMOVZXBD}
{0x32} 66, {InvalidOpCode/VPMOVZXBQ}
{0x33} 66, {InvalidOpCode/VPMOVZXWD}
{0x34} 66, {InvalidOpCode/VPMOVZXWQ}
{0x35} 66, {InvalidOpCode/VPMOVZXDQ}
{0x36} 66, {VPERMD/InvalidOpCode}
{0x37} 66, {InvalidOpCode/VPCMPGTQ}
{0x38} 66, {InvalidOpCode/VPMINSB}
{0x39} 66, {InvalidOpCode/VPMINSD}
{0x3A} 66, {InvalidOpCode/VPMINUW}
{0x3B} 66, {InvalidOpCode/VPMINUD}
{0x3C} 66, {InvalidOpCode/VPMAXSB}
{0x3D} 66, {InvalidOpCode/VPMAXSD}
{0x3E} 66, {InvalidOpCode/VPMAXUW}
{0x3F} 66, {InvalidOpCode/VPMAXUD}
{0x40} 66, {InvalidOpCode/VPMULLD}
{0x41} 66, {InvalidOpCode/VPHMINPOSUW}
{0x42} 00, {InvalidOpCode}
{0x43} 00, {InvalidOpCode}
{0x44} 00, {InvalidOpCode}
{0x45} 66, {VPSRLVQ/InvalidOpCode}
{0x46} 66, {VPSRAVD/InvalidOpCode}
{0x47} 66, {VPSLLVQ/VPSLLVD/InvalidOpCode}
{0x48} 00, {InvalidOpCode}
{0x49} 00, {InvalidOpCode}
{0x4A} 00, {InvalidOpCode}
{0x4B} 00, {InvalidOpCode}
{0x4C} 00, {InvalidOpCode}
{0x4D} 00, {InvalidOpCode}
{0x4E} 00, {InvalidOpCode}
{0x4F} 00, {InvalidOpCode}
{0x50} 00, {InvalidOpCode}
{0x51} 00, {InvalidOpCode}
{0x52} 00, {InvalidOpCode}
{0x53} 00, {InvalidOpCode}
{0x54} 00, {InvalidOpCode}
{0x55} 00, {InvalidOpCode}
{0x56} 00, {InvalidOpCode}
{0x57} 00, {InvalidOpCode}
{0x58} 66, {VPBROADCASTD/InvalidOpCode}
{0x59} 66, {VPBROADCASTQ/InvalidOpCode}
{0x5A} 66, {VBROADCASTI128/InvalidOpCode}
{0x5B} 00, {InvalidOpCode}
{0x5C} 00, {InvalidOpCode}
{0x5D} 00, {InvalidOpCode}
{0x5E} 00, {InvalidOpCode}
{0x5F} 00, {InvalidOpCode}
{0x60} 00, {InvalidOpCode}
{0x61} 00, {InvalidOpCode}
{0x62} 00, {InvalidOpCode}
{0x63} 00, {InvalidOpCode}
{0x64} 00, {InvalidOpCode}
{0x65} 00, {InvalidOpCode}
{0x66} 00, {InvalidOpCode}
{0x67} 00, {InvalidOpCode}
{0x68} 00, {InvalidOpCode}
{0x69} 00, {InvalidOpCode}
{0x6A} 00, {InvalidOpCode}
{0x6B} 00, {InvalidOpCode}
{0x6C} 00, {InvalidOpCode}
{0x6D} 00, {InvalidOpCode}
{0x6E} 00, {InvalidOpCode}
{0x6F} 00, {InvalidOpCode}
{0x70} 00, {InvalidOpCode}
{0x71} 00, {InvalidOpCode}
{0x72} 00, {InvalidOpCode}
{0x73} 00, {InvalidOpCode}
{0x74} 00, {InvalidOpCode}
{0x75} 00, {InvalidOpCode}
{0x76} 00, {InvalidOpCode}
{0x77} 00, {InvalidOpCode}
{0x78} 66, {VPBROADCASTB/InvalidOpCode}
{0x79} 66, {VPBROADCASTW/InvalidOpCode}
{0x7A} 00, {InvalidOpCode}
{0x7B} 00, {InvalidOpCode}
{0x7C} 00, {InvalidOpCode}
{0x7D} 00, {InvalidOpCode}
{0x7E} 00, {InvalidOpCode}
{0x7F} 00, {InvalidOpCode}
{0x80} 66, {INVPCID}
{0x81} 66, {INVVPID}
{0x82} 66, {INVPCID}
{0x83} 00, {InvalidOpCode}
{0x84} 00, {InvalidOpCode}
{0x85} 00, {InvalidOpCode}
{0x86} 00, {InvalidOpCode}
{0x87} 00, {InvalidOpCode}
{0x88} 00, {InvalidOpCode}
{0x89} 00, {InvalidOpCode}
{0x8A} 00, {InvalidOpCode}
{0x8B} 00, {InvalidOpCode}
{0x8C} 66, {VPMASKMOVD/VPMASKMOVQ/InvalidOpCode}
{0x8D} 00, {InvalidOpCode}
{0x8E} 66, {VPMASKMOVD/VPMASKMOVQ/InvalidOpCode}
{0x8F} 00, {InvalidOpCode}
{0x90} 66, {VPGATHERDD/VPGATHERDQ}
{0x91} 66, {VPGATHERQD/VPGATHERQQ}
{0x92} 66, {VGATHERDPS/VGATHERDPD}
{0x93} 66, {VGATHERQPS/VGATHERQPD}
{0x94} 00, {InvalidOpCode}
{0x95} 00, {InvalidOpCode}
{0x96} 66, {VFMADDSUB132PS/VFMADDSUB132PD}
{0x97} 66, {VFMSUBADD132PS/VFMSUBADD132PD}
{0x98} 66, {VFMADD132PS/VFMADD132PD}
{0x99} 66, {VFMADD132SS/VFMADD132SD}
{0x9A} 66, {VFMSUB132PS/VFMSUB132PD}
{0x9B} 66, {VFMSUB132SS/VFMSUB132SD}
{0x9C} 66, {VFNMADD132PS/VFNMADD132PD}
{0x9D} 66, {VFNMADD132SS/VFNMADD132SD}
{0x9E} 66, {VFNMSUB132PS/VFNMSUB132PD}
{0x9F} 66, {VFNMSUB132SS/VFNMSUB132SD}
{0xA0} 00, {InvalidOpCode}
{0xA1} 00, {InvalidOpCode}
{0xA2} 00, {InvalidOpCode}
{0xA3} 00, {InvalidOpCode}
{0xA4} 00, {InvalidOpCode}
{0xA5} 00, {InvalidOpCode}
{0xA6} 66, {VFMADDSUB213PS/VFMADDSUB213PD}
{0xA7} 66, {VFMSUBADD213PS/VFMSUBADD213PD}
{0xA8} 66, {VFMADD213PS/VFMADD213PD}
{0xA9} 66, {VFMADD213SS/VFMADD213SD}
{0xAA} 66, {VFMSUB213PS/VFMSUB213PD}
{0xAB} 66, {VFMSUB213SS/VFMSUB213SD}
{0xAC} 66, {VFNMADD213PS/VFNMADD213PD}
{0xAD} 66, {VFNMADD213SS/VFNMADD213SD}
{0xAE} 66, {VFNMSUB213PS/VFNMSUB213PD}
{0xAF} 66, {VFNMSUB213SS/VFNMSUB213SD}
{0xB0} 00, {InvalidOpCode}
{0xB1} 00, {InvalidOpCode}
{0xB2} 00, {InvalidOpCode}
{0xB3} 00, {InvalidOpCode}
{0xB4} 00, {InvalidOpCode}
{0xB5} 00, {InvalidOpCode}
{0xB6} 66, {VFMADDSUB231PS/VFMADDSUB231PD}
{0xB7} 66, {VFMSUBADD231PS/VFMSUBADD231PD}
{0xB8} 66, {VFMADD231PS/VFMADD231PD}
{0xB9} 66, {VFMADD231SS/VFMADD231SD}
{0xBA} 66, {VFMSUB231PS/VFMSUB231PD}
{0xBB} 66, {VFMSUB231SS/VFMSUB231SD}
{0xBC} 66, {VFNMADD231PS/VFNMADD231PD}
{0xBD} 66, {VFNMADD231SS/VFNMADD231SD}
{0xBE} 66, {VFNMSUB231PS/VFNMSUB231PD}
{0xBF} 66, {VFNMSUB231SS/VFNMSUB231SD}
{0xC0} 00, {InvalidOpCode}
{0xC1} 00, {InvalidOpCode}
{0xC2} 00, {InvalidOpCode}
{0xC3} 00, {InvalidOpCode}
{0xC4} 00, {InvalidOpCode}
{0xC5} 00, {InvalidOpCode}
{0xC6} 00, {InvalidOpCode}
{0xC7} 00, {InvalidOpCode}
{0xC8} 01, {SHA1NEXTE}
{0xC9} 01, {SHA1MSG1}
{0xCA} 01, {SHA1MSG2}
{0xCB} 01, {SHA256RNDS2}
{0xCC} 01, {SHA256MSG1}
{0xCD} 01, {SHA256MSG2}
{0xCE} 00, {InvalidOpCode}
{0xCF} 00, {InvalidOpCode}
{0xD0} 00, {InvalidOpCode}
{0xD1} 00, {InvalidOpCode}
{0xD2} 00, {InvalidOpCode}
{0xD3} 00, {InvalidOpCode}
{0xD4} 00, {InvalidOpCode}
{0xD5} 00, {InvalidOpCode}
{0xD6} 00, {InvalidOpCode}
{0xD7} 00, {InvalidOpCode}
{0xD8} 00, {InvalidOpCode}
{0xD9} 00, {InvalidOpCode}
{0xDA} 00, {InvalidOpCode}
{0xDB} 66, {VAESIMC}
{0xDC} 66, {VAESENC}
{0xDD} 66, {VAESENCLAST}
{0xDE} 66, {VAESDEC}
{0xDF} 66, {VAESDECLAST}
{0xE0} 00, {InvalidOpCode}
{0xE1} 00, {InvalidOpCode}
{0xE2} 00, {InvalidOpCode}
{0xE3} 00, {InvalidOpCode}
{0xE4} 00, {InvalidOpCode}
{0xE5} 00, {InvalidOpCode}
{0xE6} 00, {InvalidOpCode}
{0xE7} 00, {InvalidOpCode}
{0xE8} 00, {InvalidOpCode}
{0xE9} 00, {InvalidOpCode}
{0xEA} 00, {InvalidOpCode}
{0xEB} 00, {InvalidOpCode}
{0xEC} 00, {InvalidOpCode}
{0xED} 00, {InvalidOpCode}
{0xEE} 00, {InvalidOpCode}
{0xEF} 00, {InvalidOpCode}
{0xF0} 81, {MOVBE/InvalidOpCode/CRC32}
{0xF1} 81, {MOVBE/InvalidOpCode/CRC32}
{0xF2} 81, {ANDNv/InvalidOpCode}
{0xF3} 81, {group_17}
{0xF4} 81, {InvalidOpCode}
{0xF5} 81, {PDEPv/PEXTv/BZHIv/InvalidOpCode}
{0xF6} 81, {MULXv/ADCX/ADOX/InvalidOpCode}
{0xF7} 81, {SHLXv/SHRXv/SARXv/BEXTRv/InvalidOpCode}
{0xF8} 00, {InvalidOpCode}
{0xF9} 00, {InvalidOpCode}
{0xFA} 00, {InvalidOpCode}
{0xFB} 00, {InvalidOpCode}
{0xFC} 00, {InvalidOpCode}
{0xFD} 00, {InvalidOpCode}
{0xFE} 00, {InvalidOpCode}
{0xFF} 00 {InvalidOpCode}
);
{============================================
                  ThreeByteTable3A                    
============================================}
const ThreeByteTable3A : array[Byte] of Byte =(//
{0x00} 82, {VPERMQ/InvalidOpCode}
{0x01} 82, {VPERMPD/InvalidOpCode}
{0x02} 82, {VPBLENDD/InvalidOpCode}
{0x03} 00, {InvalidOpCode}
{0x04} 82, {VPERMILPS/InvalidOpCode}
{0x05} 82, {VPERMILPD/InvalidOpCode}
{0x06} 82, {VPERM2F128/InvalidOpCode}
{0x07} 00, {InvalidOpCode}
{0x08} 82, {InvalidOpCode/VROUNDPS}
{0x09} 82, {InvalidOpCode/VROUNDPD}
{0x0A} 82, {InvalidOpCode/VROUNDSS}
{0x0B} 82, {InvalidOpCode/VROUNDSD}
{0x0C} 82, {InvalidOpCode/VBLENDPS}
{0x0D} 82, {InvalidOpCode/VBLENDPD}
{0x0E} 82, {InvalidOpCode/VPBLENDW}
{0x0F} 77, {VPALIGNR/PALIGNR}
{0x10} 00, {InvalidOpCode}
{0x11} 00, {InvalidOpCode}
{0x12} 00, {InvalidOpCode}
{0x13} 00, {InvalidOpCode}
{0x14} 82, {InvalidOpCode/VPEXTRB}
{0x15} 82, {InvalidOpCode/VPEXTRW}
{0x16} 82, {InvalidOpCode/VPEXTRD}
{0x17} 82, {InvalidOpCode/VEXTRACTPS}
{0x18} 82, {VINSERTF128/InvalidOpCode}
{0x19} 82, {VEXTRACTF128/InvalidOpCode}
{0x1A} 00, {InvalidOpCode}
{0x1B} 00, {InvalidOpCode}
{0x1C} 00, {InvalidOpCode}
{0x1D} 82, {VCVTPS2PH/InvalidOpCode}
{0x1E} 00, {InvalidOpCode}
{0x1F} 00, {InvalidOpCode}
{0x20} 82, {InvalidOpCode/VINSERTPS}
{0x21} 82, {InvalidOpCode/VPINSRD}
{0x22} 00, {InvalidOpCode}
{0x23} 00, {InvalidOpCode}
{0x24} 00, {InvalidOpCode}
{0x25} 00, {InvalidOpCode}
{0x26} 00, {InvalidOpCode}
{0x27} 00, {InvalidOpCode}
{0x28} 00, {InvalidOpCode}
{0x29} 00, {InvalidOpCode}
{0x2A} 00, {InvalidOpCode}
{0x2B} 00, {InvalidOpCode}
{0x2C} 00, {InvalidOpCode}
{0x2D} 00, {InvalidOpCode}
{0x2E} 00, {InvalidOpCode}
{0x2F} 00, {InvalidOpCode}
{0x30} 00, {InvalidOpCode}
{0x31} 00, {InvalidOpCode}
{0x32} 00, {InvalidOpCode}
{0x33} 00, {InvalidOpCode}
{0x34} 00, {InvalidOpCode}
{0x35} 00, {InvalidOpCode}
{0x36} 00, {InvalidOpCode}
{0x37} 00, {InvalidOpCode}
{0x38} 82, {VINSERTI128/InvalidOpCode}
{0x39} 82, {VEXTRACTI128/InvalidOpCode}
{0x3A} 00, {InvalidOpCode}
{0x3B} 00, {InvalidOpCode}
{0x3C} 00, {InvalidOpCode}
{0x3D} 00, {InvalidOpCode}
{0x3E} 00, {InvalidOpCode}
{0x3F} 00, {InvalidOpCode}
{0x40} 82, {InvalidOpCode/VDPPS}
{0x41} 82, {InvalidOpCode/VDPPD}
{0x42} 82, {InvalidOpCode/VMPSADBW}
{0x43} 00, {InvalidOpCode}
{0x44} 82, {InvalidOpCode/VPCLMULQDQ}
{0x45} 00, {InvalidOpCode}
{0x46} 82, {VPERM2I128/InvalidOpCode}
{0x47} 00, {InvalidOpCode}
{0x48} 82, {VPERMILzz2PS/InvalidOpCode}
{0x49} 82, {VPERMILzz2PD/InvalidOpCode}
{0x4A} 66, {VBLENDVPS/InvalidOpCode}
{0x4B} 66, {VBLENDVPD/InvalidOpCode}
{0x4C} 66, {VPBLENDVB/InvalidOpCode}
{0x4D} 00, {InvalidOpCode}
{0x4E} 00, {InvalidOpCode}
{0x4F} 00, {InvalidOpCode}
{0x50} 00, {InvalidOpCode}
{0x51} 00, {InvalidOpCode}
{0x52} 00, {InvalidOpCode}
{0x53} 00, {InvalidOpCode}
{0x54} 00, {InvalidOpCode}
{0x55} 00, {InvalidOpCode}
{0x56} 00, {InvalidOpCode}
{0x57} 00, {InvalidOpCode}
{0x58} 00, {InvalidOpCode}
{0x59} 00, {InvalidOpCode}
{0x5A} 00, {InvalidOpCode}
{0x5B} 00, {InvalidOpCode}
{0x5C} 66, {VFMADDSUBPS/InvalidOpCode}
{0x5D} 66, {VFMADDSUBPD/InvalidOpCode}
{0x5E} 66, {VFMSUBADDPS/InvalidOpCode}
{0x5F} 66, {VFMSUBADDPD/InvalidOpCode}
{0x60} 82, {InvalidOpCode/VPCMPESTRM}
{0x61} 82, {InvalidOpCode/VPCMPESTRI}
{0x62} 82, {InvalidOpCode/VPCMPISTRM}
{0x63} 82, {InvalidOpCode/VPCMPISTRI}
{0x64} 00, {InvalidOpCode}
{0x65} 00, {InvalidOpCode}
{0x66} 00, {InvalidOpCode}
{0x67} 00, {InvalidOpCode}
{0x68} 66, {VFMADDPS/InvalidOpCode}
{0x69} 66, {VFMADDPD/InvalidOpCode}
{0x6A} 66, {VFMADDSS/InvalidOpCode}
{0x6B} 66, {VFMADDSD/InvalidOpCode}
{0x6C} 66, {VFMSUBPS/InvalidOpCode}
{0x6D} 66, {VFMSUBPD/InvalidOpCode}
{0x6E} 66, {VFMSUBSS/InvalidOpCode}
{0x6F} 66, {VFMSUBSD/InvalidOpCode}
{0x70} 00, {InvalidOpCode}
{0x71} 00, {InvalidOpCode}
{0x72} 00, {InvalidOpCode}
{0x73} 00, {InvalidOpCode}
{0x74} 00, {InvalidOpCode}
{0x75} 00, {InvalidOpCode}
{0x76} 00, {InvalidOpCode}
{0x77} 00, {InvalidOpCode}
{0x78} 66, {VFNMADDPS/InvalidOpCode}
{0x79} 66, {VFNMADDPD/InvalidOpCode}
{0x7A} 66, {VFNMADDSS/InvalidOpCode}
{0x7B} 66, {VFNMADDSD/InvalidOpCode}
{0x7C} 66, {VFNMSUBPS/InvalidOpCode}
{0x7D} 66, {VFNMSUBPD/InvalidOpCode}
{0x7E} 66, {VFNMSUBSS/InvalidOpCode}
{0x7F} 66, {VFNMSUBSD/InvalidOpCode}
{0x80} 00, {InvalidOpCode}
{0x81} 00, {InvalidOpCode}
{0x82} 00, {InvalidOpCode}
{0x83} 00, {InvalidOpCode}
{0x84} 00, {InvalidOpCode}
{0x85} 00, {InvalidOpCode}
{0x86} 00, {InvalidOpCode}
{0x87} 00, {InvalidOpCode}
{0x88} 00, {InvalidOpCode}
{0x89} 00, {InvalidOpCode}
{0x8A} 00, {InvalidOpCode}
{0x8B} 00, {InvalidOpCode}
{0x8C} 00, {InvalidOpCode}
{0x8D} 00, {InvalidOpCode}
{0x8E} 00, {InvalidOpCode}
{0x8F} 00, {InvalidOpCode}
{0x90} 00, {InvalidOpCode}
{0x91} 00, {InvalidOpCode}
{0x92} 00, {InvalidOpCode}
{0x93} 00, {InvalidOpCode}
{0x94} 00, {InvalidOpCode}
{0x95} 00, {InvalidOpCode}
{0x96} 00, {InvalidOpCode}
{0x97} 00, {InvalidOpCode}
{0x98} 00, {InvalidOpCode}
{0x99} 00, {InvalidOpCode}
{0x9A} 00, {InvalidOpCode}
{0x9B} 00, {InvalidOpCode}
{0x9C} 00, {InvalidOpCode}
{0x9D} 00, {InvalidOpCode}
{0x9E} 00, {InvalidOpCode}
{0x9F} 00, {InvalidOpCode}
{0xA0} 00, {InvalidOpCode}
{0xA1} 00, {InvalidOpCode}
{0xA2} 00, {InvalidOpCode}
{0xA3} 00, {InvalidOpCode}
{0xA4} 00, {InvalidOpCode}
{0xA5} 00, {InvalidOpCode}
{0xA6} 00, {InvalidOpCode}
{0xA7} 00, {InvalidOpCode}
{0xA8} 00, {InvalidOpCode}
{0xA9} 00, {InvalidOpCode}
{0xAA} 00, {InvalidOpCode}
{0xAB} 00, {InvalidOpCode}
{0xAC} 00, {InvalidOpCode}
{0xAD} 00, {InvalidOpCode}
{0xAE} 00, {InvalidOpCode}
{0xAF} 00, {InvalidOpCode}
{0xB0} 00, {InvalidOpCode}
{0xB1} 00, {InvalidOpCode}
{0xB2} 00, {InvalidOpCode}
{0xB3} 00, {InvalidOpCode}
{0xB4} 00, {InvalidOpCode}
{0xB5} 00, {InvalidOpCode}
{0xB6} 00, {InvalidOpCode}
{0xB7} 00, {InvalidOpCode}
{0xB8} 00, {InvalidOpCode}
{0xB9} 00, {InvalidOpCode}
{0xBA} 00, {InvalidOpCode}
{0xBB} 00, {InvalidOpCode}
{0xBC} 00, {InvalidOpCode}
{0xBD} 00, {InvalidOpCode}
{0xBE} 00, {InvalidOpCode}
{0xBF} 00, {InvalidOpCode}
{0xC0} 00, {InvalidOpCode}
{0xC1} 00, {InvalidOpCode}
{0xC2} 00, {InvalidOpCode}
{0xC3} 00, {InvalidOpCode}
{0xC4} 00, {InvalidOpCode}
{0xC5} 00, {InvalidOpCode}
{0xC6} 00, {InvalidOpCode}
{0xC7} 00, {InvalidOpCode}
{0xC8} 00, {InvalidOpCode}
{0xC9} 00, {InvalidOpCode}
{0xCA} 00, {InvalidOpCode}
{0xCB} 00, {InvalidOpCode}
{0xCC} 20, {SHA1RNDS4}
{0xCD} 00, {InvalidOpCode}
{0xCE} 00, {InvalidOpCode}
{0xCF} 00, {InvalidOpCode}
{0xD0} 00, {InvalidOpCode}
{0xD1} 00, {InvalidOpCode}
{0xD2} 00, {InvalidOpCode}
{0xD3} 00, {InvalidOpCode}
{0xD4} 00, {InvalidOpCode}
{0xD5} 00, {InvalidOpCode}
{0xD6} 00, {InvalidOpCode}
{0xD7} 00, {InvalidOpCode}
{0xD8} 00, {InvalidOpCode}
{0xD9} 00, {InvalidOpCode}
{0xDA} 00, {InvalidOpCode}
{0xDB} 00, {InvalidOpCode}
{0xDC} 00, {InvalidOpCode}
{0xDD} 00, {InvalidOpCode}
{0xDE} 00, {InvalidOpCode}
{0xDF} 82, {VAESKEYGENASSIST}
{0xE0} 00, {InvalidOpCode}
{0xE1} 00, {InvalidOpCode}
{0xE2} 00, {InvalidOpCode}
{0xE3} 00, {InvalidOpCode}
{0xE4} 00, {InvalidOpCode}
{0xE5} 00, {InvalidOpCode}
{0xE6} 00, {InvalidOpCode}
{0xE7} 00, {InvalidOpCode}
{0xE8} 00, {InvalidOpCode}
{0xE9} 00, {InvalidOpCode}
{0xEA} 00, {InvalidOpCode}
{0xEB} 00, {InvalidOpCode}
{0xEC} 00, {InvalidOpCode}
{0xED} 00, {InvalidOpCode}
{0xEE} 00, {InvalidOpCode}
{0xEF} 00, {InvalidOpCode}
{0xF0} 83, {RORXv}
{0xF1} 00, {InvalidOpCode}
{0xF2} 00, {InvalidOpCode}
{0xF3} 00, {InvalidOpCode}
{0xF4} 00, {InvalidOpCode}
{0xF5} 00, {InvalidOpCode}
{0xF6} 00, {InvalidOpCode}
{0xF7} 00, {InvalidOpCode}
{0xF8} 00, {InvalidOpCode}
{0xF9} 00, {InvalidOpCode}
{0xFA} 00, {InvalidOpCode}
{0xFB} 00, {InvalidOpCode}
{0xFC} 00, {InvalidOpCode}
{0xFD} 00, {InvalidOpCode}
{0xFE} 00, {InvalidOpCode}
{0xFF} 00 {InvalidOpCode}
);
