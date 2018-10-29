// **************************************************************************************************
// Part of Delphi Instruction Decode Library [InstDecode]
// 
// http://code.google.com/p/delphi-detours-library/

// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is ModRmFlagsTables.inc.
//
// The Initial Developer of the Original Code is Mahdi Safsafi [SMP3].
// Portions created by Mahdi Safsafi . are Copyright (C) 2013-2015 Mahdi Safsafi .
// All Rights Reserved.
//
// **************************************************************************************************


{ Reference : Intel® 64 and IA-32 Architectures Software Developer’s Manual Vol 2 }

type
  TModRmFlagsArray = array [Byte] of Byte;
  PModRmFlagsArray = ^TModRmFlagsArray;
  {
    ModRMFlags :
    Bits:4 3 2 1 0 .

    Bit 0 : Set ==> Register Indirect Addressing Mode  .
    Bit 1 : Set ==> Displacement 8 bit .
    Bit 2 : Set ==> Displacement 16 bit .
    Bit 3 : Set ==> Displacement 32 bit.
    Bit 4 : Set ==> SIB Used  .


    Values:

    $00 ==> Register .
    $01 ==> Register Indirect Addressing Mode with No Displacement .
    $03 ==> Register Indirect Addressing Mode + 8 bit Displacement .
    $04 ==> 16 bit Displacement only without register .
    $05 ==> Register Indirect Addressing Mode + 16 bit Displacement .
    $08 ==> 32 bit Displacement only without register .
    $09 ==> Register Indirect Addressing Mode + 32 bit Displacement .
    $11 ==>  Register Indirect Addressing Mode + SIB .
    $13 ==> Register Indirect Addressing Mode + SIB + 8 bit Displacement .
    $19 ==> Register Indirect Addressing Mode + SIB + 32 bit Displacement .

  }

const

  ModRM16Flags: TModRmFlagsArray = (
    { =>        Mod=00b       <= }
    $01, $01, $01, $01, $01, $01, $04, $01, { 00 }
    $01, $01, $01, $01, $01, $01, $04, $01, { 00 }
    $01, $01, $01, $01, $01, $01, $04, $01, { 00 }
    $01, $01, $01, $01, $01, $01, $04, $01, { 00 }
    $01, $01, $01, $01, $01, $01, $04, $01, { 00 }
    $01, $01, $01, $01, $01, $01, $04, $01, { 00 }
    $01, $01, $01, $01, $01, $01, $04, $01, { 00 }
    $01, $01, $01, $01, $01, $01, $04, $01, { 00 }
    { =>        Mod=01b       <= }
    $03, $03, $03, $03, $03, $03, $03, $03, { 01 }
    $03, $03, $03, $03, $03, $03, $03, $03, { 01 }
    $03, $03, $03, $03, $03, $03, $03, $03, { 01 }
    $03, $03, $03, $03, $03, $03, $03, $03, { 01 }
    $03, $03, $03, $03, $03, $03, $03, $03, { 01 }
    $03, $03, $03, $03, $03, $03, $03, $03, { 01 }
    $03, $03, $03, $03, $03, $03, $03, $03, { 01 }
    $03, $03, $03, $03, $03, $03, $03, $03, { 01 }
    { =>        Mod=10b       <= }
    $05, $05, $05, $05, $05, $05, $05, $05, { 10 }
    $05, $05, $05, $05, $05, $05, $05, $05, { 10 }
    $05, $05, $05, $05, $05, $05, $05, $05, { 10 }
    $05, $05, $05, $05, $05, $05, $05, $05, { 10 }
    $05, $05, $05, $05, $05, $05, $05, $05, { 10 }
    $05, $05, $05, $05, $05, $05, $05, $05, { 10 }
    $05, $05, $05, $05, $05, $05, $05, $05, { 10 }
    $05, $05, $05, $05, $05, $05, $05, $05, { 10 }
    { =>        Mod=11b       <= }
    $00, $00, $00, $00, $00, $00, $00, $00, { 11 }
    $00, $00, $00, $00, $00, $00, $00, $00, { 11 }
    $00, $00, $00, $00, $00, $00, $00, $00, { 11 }
    $00, $00, $00, $00, $00, $00, $00, $00, { 11 }
    $00, $00, $00, $00, $00, $00, $00, $00, { 11 }
    $00, $00, $00, $00, $00, $00, $00, $00, { 11 }
    $00, $00, $00, $00, $00, $00, $00, $00, { 11 }
    $00, $00, $00, $00, $00, $00, $00, $00 { 11 }

    );
  ModRM32Flags: TModRmFlagsArray = (
    { =>        Mod=00b       <= }
    $01, $01, $01, $01, $11, $08, $01, $01, { 00 }
    $01, $01, $01, $01, $11, $08, $01, $01, { 00 }
    $01, $01, $01, $01, $11, $08, $01, $01, { 00 }
    $01, $01, $01, $01, $11, $08, $01, $01, { 00 }
    $01, $01, $01, $01, $11, $08, $01, $01, { 00 }
    $01, $01, $01, $01, $11, $08, $01, $01, { 00 }
    $01, $01, $01, $01, $11, $08, $01, $01, { 00 }
    $01, $01, $01, $01, $11, $08, $01, $01, { 00 }
    { =>        Mod=01b       <= }
    $03, $03, $03, $03, $13, $03, $03, $03, { 01 }
    $03, $03, $03, $03, $13, $03, $03, $03, { 01 }
    $03, $03, $03, $03, $13, $03, $03, $03, { 01 }
    $03, $03, $03, $03, $13, $03, $03, $03, { 01 }
    $03, $03, $03, $03, $13, $03, $03, $03, { 01 }
    $03, $03, $03, $03, $13, $03, $03, $03, { 01 }
    $03, $03, $03, $03, $13, $03, $03, $03, { 01 }
    $03, $03, $03, $03, $13, $03, $03, $03, { 01 }
    { =>        Mod=10b       <= }
    $09, $09, $09, $09, $19, $09, $09, $09, { 10 }
    $09, $09, $09, $09, $19, $09, $09, $09, { 10 }
    $09, $09, $09, $09, $19, $09, $09, $09, { 10 }
    $09, $09, $09, $09, $19, $09, $09, $09, { 10 }
    $09, $09, $09, $09, $19, $09, $09, $09, { 10 }
    $09, $09, $09, $09, $19, $09, $09, $09, { 10 }
    $09, $09, $09, $09, $19, $09, $09, $09, { 10 }
    $09, $09, $09, $09, $19, $09, $09, $09, { 10 }
    { =>        Mod=11b       <= }
    $00, $00, $00, $00, $00, $00, $00, $00, { 11 }
    $00, $00, $00, $00, $00, $00, $00, $00, { 11 }
    $00, $00, $00, $00, $00, $00, $00, $00, { 11 }
    $00, $00, $00, $00, $00, $00, $00, $00, { 11 }
    $00, $00, $00, $00, $00, $00, $00, $00, { 11 }
    $00, $00, $00, $00, $00, $00, $00, $00, { 11 }
    $00, $00, $00, $00, $00, $00, $00, $00, { 11 }
    $00, $00, $00, $00, $00, $00, $00, $00 { 11 }

    );

  ModRmFlags: array [0 .. 3] of PModRmFlagsArray = ( //
    nil,
    @ModRM16Flags, { AddrMode 16-bits }
    @ModRM32Flags, { AddrMode 32-bits }
    @ModRM32Flags { AddrMode 64-bits }
    );