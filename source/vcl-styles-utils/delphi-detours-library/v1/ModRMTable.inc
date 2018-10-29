// **************************************************************************************************
// This file (ModRMTable.inc) is a part of the InstDecode Library .
// File ModRMTable
// http://code.google.com/p/delphi-detours-library/

// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is ModRMTable.inc .
//
// The Initial Developer of the Original Code is Mahdi Safsafi [SMP3].
// Portions created by Mahdi Safsafi . are Copyright (C) 2013-2014 Mahdi Safsafi .
// All Rights Reserved.
//
// **************************************************************************************************

{ Reference : Intel® 64 and IA-32 Architectures Software Developer’s Manual Vol 2 }

{
  ModRMFlags :
  Bits: 3 2 1 0 .
  Bit 0 : Set ==> Register Indirect Addressing Mode  .
  Bit 1 : Set ==> Displacement 8 bit .
  Bit 2 : Set ==> Displacement 32 bit .
  Bit 3 : Set ==> SIB Used .

  Values:
  $0 => Register Addressing Mode .
  $1 => Register Indirect Addressing Mode with No Displacement .
  $3 => Register Indirect Addressing Mode + 8 bit Displacement .
  $5 => Register Indirect Addressing Mode + 32 bit Displacement .
  $8 => SIB is Used .
  $B => Register Indirect Addressing Mode + SIB + 8 bit Displacement .
  $D => Register Indirect Addressing Mode + SIB + 32 bit Displacement .
}
const
  ModRMFlags: array [0 .. $FF] of Byte = (
    { =>        Mod=00b       <= }
    1, 1, 1, 1, 8, 5, 1, 1, { 00 }
    1, 1, 1, 1, 8, 5, 1, 1, { 00 }
    1, 1, 1, 1, 8, 5, 1, 1, { 00 }
    1, 1, 1, 1, 8, 5, 1, 1, { 00 }
    1, 1, 1, 1, 8, 5, 1, 1, { 00 }
    1, 1, 1, 1, 8, 5, 1, 1, { 00 }
    1, 1, 1, 1, 8, 5, 1, 1, { 00 }
    1, 1, 1, 1, 8, 5, 1, 1, { 00 }
    { =>        Mod=01b       <= }
    3, 3, 3, 3, $B, 3, 3, 3, { 01 }
    3, 3, 3, 3, $B, 3, 3, 3, { 01 }
    3, 3, 3, 3, $B, 3, 3, 3, { 01 }
    3, 3, 3, 3, $B, 3, 3, 3, { 01 }
    3, 3, 3, 3, $B, 3, 3, 3, { 01 }
    3, 3, 3, 3, $B, 3, 3, 3, { 01 }
    3, 3, 3, 3, $B, 3, 3, 3, { 01 }
    3, 3, 3, 3, $B, 3, 3, 3, { 01 }
    { =>        Mod=10b       <= }
    5, 5, 5, 5, $D, 5, 5, 5, { 10 }
    5, 5, 5, 5, $D, 5, 5, 5, { 10 }
    5, 5, 5, 5, $D, 5, 5, 5, { 10 }
    5, 5, 5, 5, $D, 5, 5, 5, { 10 }
    5, 5, 5, 5, $D, 5, 5, 5, { 10 }
    5, 5, 5, 5, $D, 5, 5, 5, { 10 }
    5, 5, 5, 5, $D, 5, 5, 5, { 10 }
    5, 5, 5, 5, $D, 5, 5, 5, { 10 }
    { =>        Mod=11b       <= }
    0, 0, 0, 0, 0, 0, 0, 0, { 11 }
    0, 0, 0, 0, 0, 0, 0, 0, { 11 }
    0, 0, 0, 0, 0, 0, 0, 0, { 11 }
    0, 0, 0, 0, 0, 0, 0, 0, { 11 }
    0, 0, 0, 0, 0, 0, 0, 0, { 11 }
    0, 0, 0, 0, 0, 0, 0, 0, { 11 }
    0, 0, 0, 0, 0, 0, 0, 0, { 11 }
    0, 0, 0, 0, 0, 0, 0, 0 { 11 }

    );
