/// fast LZO Compression routines
// - licensed under a MPL/GPL/LGPL tri-license; version 1.13
unit SynLZO;

{
    This file is part of Synopse LZO Compression.

    Synopse LZO Compression. Copyright (C) 2012 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse LZO Compression.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2012
  the Initial Developer. All Rights Reserved.

  Contributor(s):
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


  
     Pascal SynLZO Compression / Decompression library
     =================================================
     by Arnaud Bouchez http://bouchez.info

    * SynLZO is a very FAST portable lossless data compression library
      written in optimized pascal code for Delphi 3 up to Delphi 2009
      with a tuned asm version available
    * offers *extremely* fast compression and decompression
      with good compression rate, in comparaison with its speed
    * original LZO written in ANSI C - pascal+asm conversion by A.Bouchez
    * simple but very fast direct file compression:
      SynLZO compressed files read/write is faster than copying plain files!

    SynLZO implements a number of algorithms with the following features:
    * hashing+dictionary compression in one pass, with no huffman table
    * in-memory compression (the dictionary is the input stream itself)
    * Decompression is simple and *very* fast
    * Algorithm is thread safe
    * Algorithm is lossless
    * supports overlapping compression and in-place decompression
    * direct file compression/decompression using memory mapped files

    LZO and the LZO algorithms and implementations are distributed under the terms
    of the GNU General Public License (GPL)
    (c)1996-2008 Markus F.X.J. Oberhumer http://www.oberhumer.com/opensource/lzo
    Delphi/Pascal/asm code conversion (c)2008 Arnaud Bouchez http://bouchez.info

    This unit is a full pascal conversion of the lzo algorigthm, by A. Bouchez.
    Speed was the goal here, the code is very optimized but not stylish.
    The porting was done from the original lzo files, rather than minilzo,
      with some enhancements and adaptation for the Delphi pascal compiler.
    A special version of this unit was optimized deep in asm code.

    Conversion notes:
    - this format is NOT stream compatible with any lzo* official format
       => use it internally in your application, not as exchange format
    - very small code size (less than 1KB for both compressor/decompressor)
    - the uncompressed data length is stored in the beginning of the stream
       and can be retrieved easily for proper out_p memory allocation
    - compression is 10 times faster than zip, decompression 4 times
    - same compression ratio as original C-lzo, ie somewhat decent vs its speed
    - this implementation is faster than the original lzo code
    - please give correct data to the decompressor (i.e. first CRC in_p data)
       => we recommend our very fast Adler32 procedures from SynCrypto
    - up to 3 bytes overflow may be written in the decompression out_p buffer
       => but the GetMem() allocation routine allows this directly
    - the code is heavily commented, in order to understand the algorithm
       => it's not obvious to follow, but much easier than the original C code!
       => the unused M1 case was removed from decompressor
    - tested and benchmarked with a lot of data types/sizes
       => use the asm code, which is very tuned  ($define USEASM)
    - the code may be compiled under 64 bits FPC (implemented but not tested)
    - tested under Delphi 7, Delphi 2007 and Delphi 2009
    - if you include it in your application, please give me some credits:
       "use SynLZO compression by http://bouchez.info"
    - use at your own risk!

 Some benchmark are available at the end of the file (on my Turion TL-65 1.8Ghz)

 The results show that our SynLZO implementation is often faster than original
   LZO C implementation, especially for compression.
 It's a must-have for compression speed, regarding ZIP (compression is
   10 times faster, decompression 4 times, with lower compression rate) or others.
 You can notice some speed enhancements in the compiler from Delphi 7 to 2009:
   the D2009 optimizer deals better with register assignment and 8bits-32bits
   conversion. Note the pascal code was optimized by looking at the generated
   asm (via Alt-F2), and by proper profiling. D2009 use fpu fast move(), and my
   D7 version use the FastCode SSE move() version (which may be blazzing fast
   on the upcoming CPUs, with true 128bits memory access - until now, the
   use of 128bits registers in the move() code were not faster than 64bits FPU).
 => for the first time, I consider leaving D7 for the new D2009 IDE... there's
   a lot to refactor (string->ansistring, PChar->PAnsiChar) in the code,
   but it may be worth it, since quite all my code is test-driven, so that I can
   be sure the conversion is ok. My only negative impact is that I can't use the
   language enhancements if I want to be compatible with CrossKylix, which I
   would like to, in order to be able to compile for Linux target. But the new
   D2009 IDE is great, even if it seems a bit slow compared to the old D7 one.
   And don't speak about the MSHelp system used in Delphi 2009: it's a bulky
   piece of software - such coders should be hang. :)
 => an optimized asm version, derivating from D2009 generated code, is provided
   and is to be used - this asm version is 100% compatible with the pascal code


  Revision history

  Version 1.6
  - first release, associated with the main Synopse SQLite3 framework

  Version 1.13
  - code modifications to compile with Delphi 5 compiler
  - comment refactoring (mostly for inclusion in SynProject documentation)
  - new CompressSynLZO function, for THttpSocket.RegisterCompress - those
    functions will return 'synlzo' as ACCEPT-ENCODING: HTTP header parameter

}

interface

{$I Synopse.inc}

uses
  Classes;

{.$define LZOFILE}
{ attempt to use in-place file compression using memory mapping mechanism
  -> still not fully functional -> so do not define }


/// get maximum possible (worse) compressed size for out_p
function lzopas_compressdestlen(in_len: integer): integer;

/// compress in_p(in_len) into out_p
// - out_p must be at least lzopas_compressdestlen(in_len) bytes long
// - returns compressed size in out_p
function lzopas_compress(in_p: PAnsiChar; in_len: integer; out_p: PAnsiChar): integer;

/// get uncompressed size from lzo-compressed buffer (to reserve memory, e.g.)
function lzopas_decompressdestlen(in_p: PAnsiChar): integer;

/// uncompress in_p(in_len) into out_p (must be allocated before call), returns out_len
// - may write up to out_len+3 bytes in out_p
// - the decompression mode is "fast-unsafe" -> CRC/Adler32 in_p data before call
function lzopas_decompress(in_p: PAnsiChar; in_len: integer; out_p: PAnsiChar): Integer;

/// (de)compress a data content using the SynLZO algorithm
// - as expected by THttpSocket.RegisterCompress
// - will return 'synlzo' as ACCEPT-ENCODING: header parameter
// - will store a hash of both compressed and uncompressed stream: if the
// data is corrupted during transmission, will instantly return ''
function CompressSynLZO(var Data: AnsiString; Compress: boolean): AnsiString;



{$ifdef LZOFILE}

{$ifdef WIN32}
// file compression functions using fast SynLZO library (up to 2GB file size)
// - if you are dealing directly with file compression, this is where to begin
// - SynLZO compressed files read/write is faster than copying plain files :)

type
  TCompressFunc = function(in_p: PAnsiChar; in_len: integer; out_p: PAnsiChar): integer;
  TCompressLen = function(in_len: integer): integer;

function lzopas_compressfile(const srcFile, dstFile: AnsiString; dstSize: PInteger=nil;
  methodLen: TCompressLen=nil; methodComp: TCompressFunc=nil): boolean;
// do the magic: srcFile is compressed into dstFile
// true if sucess; on error, return false and error code in GetLastError

function lzopas_decompressfile(const srcFile, dstFile: AnsiString): boolean;
// uncompress srcFile into dstFile (with checksum and file age restore)
// true if sucess; on error, return false and error code in GetLastError

function lzopas_decompressfilesize(const srcFile: AnsiString): integer;
// -1 if error, decompressed size otherwize
function lzopas_decompressfilecheck(const srcFile: AnsiString): boolean;
// true if file checksum is correct (no transmission fail, e.g.)
{$endif}

{$endif LZOFILE}

implementation

{$ifndef CPUX64}
{$ifndef CPU64}
  {$define USEASM}
  // if defined, a hand-tuned asm compression code (derivating from one generated
  //   by Delphi 2009) will be used instead of the slower Delphi3-2007 code
{$endif}
{$endif}

{$ifdef MSWINDOWS}
uses
  Windows, SysUtils;
{$endif}

{$ifndef FPC}
type
{$ifdef CPU64}
  PtrInt = NativeInt;
  PtrUInt = NativeUInt;
{$else}
  PtrInt = integer;
  PtrUInt = cardinal;
{$endif}
{$ifndef CONDITIONALEXPRESSIONS}
  // Delphi 5 doesn't have those base types defined :(
  PCardinal = ^Cardinal;
  IntegerArray  = array[0..$effffff] of Integer;
  PIntegerArray = ^IntegerArray;
{$endif}

{$else}
{$ifdef CPU64}
// CPU64 note: integer must be 4 bytes, word 2 bytes; use of PtrInt for common integer
{$define USEOFFSET}
// the PAnsiPChar hashing trick used in the original lzo algo is not 64bits enabled
// -> define this in order to use 64bits-compatible hashing code (same speed)
// -> it will store offset from in_p beginning, instead of direct adress
// it doesn't change the lzo stream format, only internal hashing dictionary
{$endif}
{$endif}

// for Delphi 2009 compatibility, we use PAnsiChar instead of PChar
// (which uses WideChar in D2009) - this code remain backward compatible with D3-2007

{.$define WITHM1}
// this was an attempt to use M1 (ip<#16) for storing offset $C000..$FFFF
// => some confusion on decompression, and only matters with huge files
// => leave this flag undefined

const
  // used for offset storage (M2..M4 patterns, M1 is deprecated)
  M2_MAX_LEN = 8;
  M3_MAX_LEN = 33;
  M4_MAX_LEN = 9;
  M4_MARKER = 16;
  M3_MARKER = M4_MARKER+16;
  M2_MARKER = M3_MARKER+32;
  M2_MAX_OFFSET = $0800; // = 1 shl 11 (M2 stores offs0..11)
  M3_MAX_OFFSET = $4000; // = 1 shl 14 (M3 stores offs0..14)
  M4_MAX_OFFSET = $C000;
{$ifdef WITHM1}
  MAX_OFFSET = $FFFF;
{$else}
  MAX_OFFSET = M4_MAX_OFFSET-1;
{$endif}
  M4_OFF_BITS = 11;
  M4_MASK = 7 shl M4_OFF_BITS;
  // used for hashing
  D_BITS = 14;
  D_MASK = (1 shl D_BITS) - 1;
  D_HIGH = (D_MASK shr 1)+1;
  D_MUL_SHIFT = 5;
  D_MUL = (1 shl D_MUL_SHIFT)+1;

{.$define WT}
// if defined, some more test are introduced during compression
//  i.e. in some rare cases, we should overlap bytes during decompression
// {$define WT} will check for t=m_off>* every time we test a m_pos[] value
// this is only necessary if move() is not byte-oriented
// if you get some errors (with FPC on CPU other than i386, e.g.), try enable it
//  => the compression ratio will be somewhat lower, and decompression slower
//  => with the present source code under Delphi, it's NOT necessary AT ALL
//  => the movechars() procedure is called only when overlap is possible
//  => another way for being 100% sure is to use the safe movechars() only
//  => never change the movechars() procedure below

procedure movechars(s,d: PAnsiChar; t: PtrInt);
// very fast code for unaligned and overlapping (see {$define WT}) small blocks
// this code is sometimes used rather than system.Move() by decompress()
var i: PtrInt;
begin
  for i := 1 to t do begin
    d^ := s^;
    inc(d);
    inc(s);
  end;
end;

function _lzo1x_1_do_compress(in_p: PAnsiChar; in_len: PtrInt;
  out_p: PAnsiChar; out out_len: integer): integer;
{$ifdef USEASM}
// code below was extracted from the pascal code generated by Delphi 2009
// after some manual further optimization of the asm, here's something fast...
// faster than the original lzo asm or the most optimized C code generated I got
asm
        push    eax
        mov     eax, 16
@@1759: add     esp, -4092
        push    eax
        dec     eax
        jnz     @@1759
        mov     eax, [ebp-4H]
        add     esp, -24
        push    ebx
        push    esi
        push    edi
        mov     [ebp-8H], ecx
        mov     [ebp-4H], edx
        mov     ebx, eax
        mov     eax, [ebp-4H]
        add     eax, ebx
        mov     [ebp-0CH], eax
        mov     eax, [ebp-0CH]
        sub     eax, 9 
        mov     [ebp-10H], eax
        mov     [ebp-14H], ebx
        add     ebx, 4
        mov     eax, [ebp-8H]
        mov     [ebp-18H], eax
        lea     eax, [ebp-1001CH]
        xor     ecx, ecx
        mov     edx, 65536
        call    System.@FillChar
        jmp     @@1760
        nop;nop;nop;nop;nop;nop;nop;nop;nop;nop;nop
@@1760: movzx   eax, byte ptr [ebx+3]
        movzx   edx, byte ptr [ebx+2]
        shl     eax, 6
        movzx   ecx, byte ptr [ebx+1]
        xor     eax, edx
        movzx   edx, byte ptr [ebx]
        shl     eax, 5
        xor     eax, ecx
        shl     eax, 5
        xor     eax, edx
        mov     edx, eax
        shl     eax, 5
        add     eax, edx
        shr     eax, 5
        and     eax, 3FFFH
        mov     edx,[ebp+eax*4-1001CH]
        test    edx, edx
        jnz     @@1762
@@1761: mov     [ebp+eax*4-1001CH], ebx
        inc     ebx
        cmp     ebx, [ebp-10H]
        jc      @@1760
        jmp     @@1788
        nop;nop;nop;nop;nop;nop
@@1762: mov     esi, ebx
        mov     edi, edx
        sub     esi, edx
        cmp     esi, 49151
        jg      @@1761
        cmp     esi, 2048
        jle     @@1763
        mov     cl, [ebx+3H]
        cmp     cl, [edi+3H]
        jz      @@1763
        and     eax, 7FFH
        xor     eax, 201FH
        mov     edx, [ebp+eax*4-1001CH]
        test    edx, edx
        mov     edi, edx
        mov     esi, ebx
        jz      @@1761
        sub     esi, edi
        cmp     esi, 49151
        jg      @@1761
        cmp     esi, 2048
        jle     @@1763
        cmp     cl, [edi+3H]
        jnz     @@1761
@@1763: mov     edx, [edi]
        cmp     dx, word ptr [ebx]
        jnz     @@1761
        shr     edx,16
        cmp     dl, [ebx+2H]
        jnz     @@1761
        mov     [ebp+eax*4-1001CH], ebx
        mov     eax, ebx
        sub     eax, [ebp-14H]
        je      @@1768
        cmp     eax, 3
        jg      @@1764
        mov     ecx, [ebp-8H]
        add     [ebp-8H], eax
        mov     edx, [ebp-14H]
        add     [ebp-14H], eax
        mov     edx, [edx]
        or      [ecx-2], al
        mov     [ecx], edx
        jmp     @@1768

@@1764: cmp     eax, 18
        jg      @@1765
        mov     ecx,[ebp-8H]
        lea     eax,[eax-3]
        mov     [ecx], al
        mov     edx,[ebp-14H]
        inc     ecx
        push    ebx
        mov     ebx,[edx]
        mov     [ecx],ebx
        dec     eax
        lea     edx,[edx+4]
        lea     ecx,[ecx+4]
        jz      @@0
@@1:    mov     bl,[edx]
        mov     [ecx],bl
        dec     eax
        lea     edx,[edx+1]
        lea     ecx,[ecx+1]
        jnz     @@1
@@0:    pop     ebx
        mov     [ebp-8H], ecx
        mov     [ebp-14H], edx
        jmp     @@1768

@@1765: mov     edx, eax                               
        sub     edx, 18                                
        mov     [ebp-1CH], edx               
        mov     edx, [ebp-8H]                
        mov     byte ptr [edx], 0                      
        inc     dword ptr [ebp-8H]
        cmp     dword ptr [ebp-1CH], 255
        jle     @@1767                                 
@@1766: sub     dword ptr [ebp-1CH], 255
        mov     edx, [ebp-8H]                
        mov     byte ptr [edx], 0                      
        inc     dword ptr [ebp-8H]
        cmp     dword ptr [ebp-1CH], 255
        jg      @@1766                                 
@@1767: mov     dl, [ebp-1CH]
        mov     ecx, [ebp-8H]
        mov     [ecx], dl
        inc     dword ptr [ebp-8H]
        mov     edx, [ebp-8H]
        mov     ecx, [ebp-14H]
        xchg    ecx, eax
        call    move
        mov     eax, ebx
        sub     eax, [ebp-14H]
        add     [ebp-8H], eax
        add     [ebp-14H], eax

@@1768: mov     eax,[edi+3H]
        mov     ecx,[ebx+3H]
        cmp     al,cl
        jne     @@1784
        cmp     ah,ch
        jne     @@1783
        shr     eax,16
        shr     ecx,16
        cmp     al,cl
        jne     @@1782
        cmp     ah,ch
        jne     @@1781
        mov     ax,[edi+7H]
        mov     cx,[ebx+7H]
        cmp     al,cl
        jne     @@1780
        cmp     ah,ch
        jne     @@1779
        add     ebx,9
        mov     eax, [ebp-0CH]
        add     edi,9
        cmp     eax, ebx
        jbe     @@1771
        mov     dl, [edi]
        cmp     dl, [ebx]
        jnz     @@1771
@@1769: inc     ebx
        inc     edi
        cmp     eax, ebx
        jbe     @@1771
        mov     dl, [edi]
        cmp     dl, [ebx]
        jz      @@1769
@@1771: mov     eax, ebx
        sub     eax, [ebp-14H]
        cmp     esi, 16384
        jg      @@1773
        dec     esi                                    
        cmp     eax, 33                                
        jg      @@1772
{$ifdef CONDITIONALEXPRESSIONS}
        lea     esi,esi*4
{$else} shl     esi,2      {$endif}
        sub     eax, 2
        mov     edx, [ebp-8H]
        or      eax, 20H
        mov     word ptr [edx+1], si
        mov     [edx], al
        add     edx, 3
        mov     [ebp-14H], ebx
        cmp     ebx, [ebp-10H]
        mov     [ebp-8H],edx
        jc      @@1760
        jmp     @@1788

@@1772: sub     eax, 33
        mov     edx, [ebp-8H]                
        mov     byte ptr [edx], 32                     
        jmp     @@1775                                 

@@1773: sub     esi, 16384
        cmp     eax, 9
        jg      @@1774                                 
        mov     edx, esi                               
        and     edx, 4000H                             
        shr     edx, 11                                
        or      dl, 10H                                
        sub     al, 2                                  
        or      dl, al                                 
        mov     eax, [ebp-8H]                
        mov     [eax], dl                     
        inc     dword ptr [ebp-8H]
        jmp     @@1778                                 

@@1774: sub     eax, 9                                 
        mov     edx, esi                               
        and     edx, 4000H                             
        shr     edx, 11                                
        or      dl, 10H                                
        mov     ecx, [ebp-8H]                
        mov     [ecx], dl
@@1775: inc     dword ptr [ebp-8H]
        cmp     eax, 255
        jle     @@1777
@@1776: sub     eax, 255
        mov     edx, [ebp-8H]
        mov     byte ptr [edx], 0
        inc     dword ptr [ebp-8H]
        cmp     eax, 255
        jg      @@1776
@@1777: mov     edx, [ebp-8H]
        mov     [edx], al
        inc     dword ptr [ebp-8H]
@@1778:
{$ifdef CONDITIONALEXPRESSIONS}
        lea     esi,esi*4
{$else} shl     esi,2      {$endif}
        mov     eax, [ebp-8H]
        add     dword ptr [ebp-8H], 2
        mov     word ptr [eax], si
        mov     [ebp-14H], ebx
        cmp     ebx, [ebp-10H]               
        jc      @@1760                                 
        jmp     @@1788
@@1779: inc     ebx
@@1780: inc     ebx
@@1781: inc     ebx
@@1782: inc     ebx
@@1783: inc     ebx
@@1784: add     ebx, 3
@@1785: cmp     esi, 2048
        jg      @@1786
        dec     esi
        mov     edx, esi
        shr     esi, 3
        mov     ecx, [ebp-14H]
        and     edx, 07H
        lea     eax, [ebx-1]
        shl     esi, 8
        sub     eax, ecx
{$ifdef CONDITIONALEXPRESSIONS}
        lea     edx,edx*4
{$else} shl     edx,2      {$endif}
        shl     eax, 5
        or      eax, edx
        mov     edx, [ebp-8H]
        or      eax, esi
        add     dword ptr [ebp-8H], 2
        mov     [edx], ax
        mov     [ebp-14H], ebx
        cmp     ebx, [ebp-10H]
        jc      @@1760
        jmp     @@1788

@@1786: cmp     esi, 16384
        jg      @@1787
        lea     eax, [ebx-2]
        dec     esi
        sub     eax, [ebp-14H]
        shl     esi, 10                                
        or      eax, 20H                               
        mov     edx, [ebp-8H]                
        or      eax, esi
        add     dword ptr [ebp-8H], 3
        mov     [edx], eax
        mov     [ebp-14H], ebx
        cmp     ebx, [ebp-10H]               
        jc      @@1760                                 
        jmp     @@1788                                 
@@1787: sub     esi, 16384
        lea     eax, [ebx-2]
        mov     edx, esi
        sub     eax, [ebp-14H]
        and     edx, 4000H
        or      eax, 10H
        shr     edx, 11
        or      eax, edx
        mov     edx, [ebp-8H]
        mov     [edx], al
        inc     dword ptr [ebp-8H]
{$ifdef CONDITIONALEXPRESSIONS}
        lea     esi,esi*4
{$else} shl     esi,2      {$endif}
        mov     eax, [ebp-8H]
        mov     word ptr [eax], si
        add     dword ptr [ebp-8H], 2
        mov     [ebp-14H], ebx
        cmp     ebx, [ebp-10H]
        jc      @@1760
@@1788: mov     eax, [ebp-8H]
        sub     eax, [ebp-18H]
        mov     edx, [ebp+8H]
        mov     [edx], eax
        mov     eax, [ebp-0CH]
        sub     eax, [ebp-14H]
        pop     edi
        pop     esi
        pop     ebx
        mov     esp, ebp
end;
{$else}
var in_end, ip_end, ii, end_p, m_pos, out_beg: PAnsiChar;
    m_off, m_len, dindex, t, tt: PtrInt; // CPU register (32 or 64 bits wide)
{$ifdef USEOFFSET}
    dict: array[0..D_MASK] of integer; // for 64bit CPU, store offset from in_p
    ip_beg: PAnsiChar;
{$else}
    dict: array[0..D_MASK] of PAnsiChar;
{$endif}
label lit, try_match, match, same4, m3_m4_len, m3_m4_offset, m1;
begin
  in_end := in_p+in_len;  // in_end is max source position
  ip_end := in_end-9;     // we read till in_p[8]
  // use 9 instead of (M2_MAX_LEN-5); // ip_end=in_end-3 is max hashing position
{$ifdef USEOFFSET}
  ip_beg := in_p;
{$endif}
  ii := in_p;
  inc(in_p,4);
  out_beg := out_p;
  FillChar(dict,sizeof(dict),0); // dict[] must be set to 0 before call
  repeat
    // 1. hash the p[0]..p[3] bytes
    dindex := ((D_MUL * ((((((ord(in_p[3]) shl 6) xor ord(in_p[2])) shl 5)
      xor ord(in_p[1])) shl 5) xor ord(in_p[0]))) shr D_MUL_SHIFT) and D_MASK;
    // 2. check if already hashed p[0]..p[3] sequence
    // 2.1 not hashed -> add hash position in dict[], and continue
{$ifdef USEOFFSET}
    if dict[dindex]=0 then begin
lit:  dict[dindex] := in_p-ip_beg;
{$else}
    if dict[dindex]=nil then begin
lit:  dict[dindex] := in_p;
{$endif}
      inc(in_p);
      if in_p<ip_end then
        continue else break;
    end else
    // 2.2 hashed -> values in m_pos, offset in m_off and test if of some interrest
{$ifdef USEOFFSET}
      m_pos := @ip_beg[dict[dindex]]; {$else}
      m_pos := dict[dindex];
{$endif}
    m_off := in_p-m_pos;
    if {$ifdef WT}(m_off<3)or{$endif} (m_off>MAX_OFFSET) then
      goto lit else
      if (m_off<=M2_MAX_OFFSET) or (m_pos[3]=in_p[3]) then
        goto try_match;
    // 3. the first hash was not interesting -> try 2nd hash
    dindex := (dindex and (D_MASK and $7ff)) xor (D_HIGH or $1f);
    // 3.1 not hashed -> add hash position in dict[], and continue
{$ifdef USEOFFSET}
    if dict[dindex]=0 then
      goto lit else
      m_pos := @ip_beg[dict[dindex]];
{$else}
    if dict[dindex]=nil then
      goto lit else
      m_pos := dict[dindex];
{$endif}
    // 3.2 hashed -> values in m_pos, offset in m_off and test if of some interrest
    m_off := in_p-m_pos;
    if {$ifdef WT}(m_off<3)or{$endif} (m_off>MAX_OFFSET) then
      goto lit else
    if (m_off<=M2_MAX_OFFSET) or (m_pos[3]=in_p[3]) then
       goto try_match else
       goto lit;
    // 4. if of some interrest -> try exact match of in_p[0..2]
try_match:
    if (pWord(m_pos)^<>pWord(in_p)^) or (m_pos[2]<>in_p[2]) then
      goto lit;
    // 5. we have a 3 chars match
match:
    // 5.1 update dict[]
{$ifdef USEOFFSET}
    dict[dindex] := in_p-ip_beg; {$else}
    dict[dindex] := in_p;
{$endif}
    // 5.2 store input stream till current position
    t := in_p-ii;
    if t<>0 then begin
      if t<=3 then begin
        PByte(out_p-2)^ := PByte(out_p-2)^ or t;
        pInteger(out_p)^ := pInteger(ii)^;
        inc(out_p,t);
        inc(ii,t);
      end else
      if t<=18 then begin
        out_p^ := ansichar(t-3);
        inc(out_p);
        movechars(ii,out_p,t);
        inc(out_p,in_p-ii);
        inc(ii,in_p-ii);
      end else begin
        tt := t-18;
        out_p^ := #0; inc(out_p);
        while tt>255 do begin // size > 255 are stored as #0
          dec(tt,255);
          out_p^ := #0;
          inc(out_p);
        end;
        out_p^ := ansichar(tt);
        inc(out_p);
       // we recommend using FastCode (with SSE2) (included in D2009)
        system.move(ii^,out_p^,t);
        inc(out_p,in_p-ii);
        inc(ii,in_p-ii);
      end;
    end;
    // 5.3 test longest common sequence from in_p[] into m_pos[]
    {$ifdef WT}
    t := m_off;
    {$endif}
//    if (pInteger(@m_pos[3])^=pInteger(@in_p[3])^){$ifdef WT}and (t>6){$endif} then goto same4 else
    if (m_pos[3]=in_p[3]) {$ifdef WT}and (t>3){$endif} then
      if (m_pos[4]=in_p[4]) {$ifdef WT}and (t>4){$endif} then
        if (m_pos[5]=in_p[5]) {$ifdef WT}and (t>5){$endif} then
          if (m_pos[6]=in_p[6]) {$ifdef WT}and (t>6){$endif} then
same4:      if (m_pos[7]=in_p[7]) {$ifdef WT}and (t>7){$endif} then
              if (m_pos[8]=in_p[8]) {$ifdef WT}and (t>8){$endif} then begin
                // 5.3.1 longest sequence is 9 chars or more
                inc(in_p,9);
                end_p := in_end;
//                if in_p>=end_p then // since in_p<ip_end=in_end-3 we have m_len>=3
//                  in_p := end_p else this is very slow => done in decompress()
                inc(m_pos,M2_MAX_LEN+1);
                // get sequence length in m_len
                {$ifdef WT}dec(t,9);{$endif}
                while (in_p<end_p) and (m_pos^=in_p^) {$ifdef WT}and (t>0){$endif} do begin
                  inc(in_p);
                  inc(m_pos);
                  {$ifdef WT}dec(t);{$endif}
                end;
                m_len := in_p-ii;
                // store m_off + m_len into out_p
                // M2 is not possible here, since len>9
                if m_off<=M3_MAX_OFFSET then begin
                  // B0: 0..4=len-2(<32) 5=M3_MARKER
                  // B1..n-1 #0 = inc(len,255), or len-33
                  // Wn: 0..1=new 2..15=offs0..13
                  // M3_MAX_OFFSET = $4000 = 1 shl 14 (offs0..13)
                  dec(m_off);
                  if m_len<=33 then begin // M2_MARKER=M3_MARKER+32 -> 1..31 ok
                    out_p^ := ansichar(integer(M3_MARKER or (m_len-2)));
                    inc(out_p);
                    pWord(out_p)^ := m_off shl 2; // Wn: 0..1=new 2..15=offs0..13
                    inc(out_p,2);
                    ii := in_p;
                    if in_p<ip_end then continue else break;
                  end else begin
                    dec(m_len,33);
                    out_p^ := ansichar(M3_MARKER); // 0 -> big len is #0-stored
                    goto m3_m4_len;
                  end;
                end else
{$ifdef WITHM1} if m_off<M4_MAX_OFFSET then {$endif}
                begin // M3_MARKER = M4_MARKER+16 -> 1..15 ok
                  // B0: 0..2=len-2 3=off14 4=M4_MARKER
                  // B1..Bn-1 #0 = inc(len,255), or len-9
                  // Wn: 0..1=new 2..15=offs0..13
                  // M3=0..$4000 M4=$4000..$bfff (2 pages of offs0..13)
                  dec(m_off,M3_MAX_OFFSET); // m_off=0..M4_MAX_OFFSET-M3_MAX_OFFSET=$7fff
                  if (m_len<=M4_MAX_LEN) then begin
                    out_p^ := ansichar(integer(M4_MARKER or
                      ((m_off and M3_MAX_OFFSET)shr M4_OFF_BITS) or // off14 bit
                      (m_len-2))); // 3 bits for m_len
                    inc(out_p);
                  end else begin
                    dec(m_len,M4_MAX_LEN);
                    out_p^ := ansichar(integer(M4_MARKER or
                      ((m_off and M3_MAX_OFFSET)shr M4_OFF_BITS))); // off14 bit
                      // we store m_len-1=0 -> len is #0-stored
{$ifdef WITHM1}     goto m3_m4_len;
                  end;
                end else begin
                  // B0: 0..2=len-2 (M1 is <16)
                  // B1..Bn-1 #0 = inc(len,255), or len-9
                  // Wn: 0..1=new 2..15=offs0..13
                  // M1=$C000..$FFFF
                  dec(m_off,M4_MAX_OFFSET);
                  if (m_len<=M4_MAX_LEN) then begin
                    out_p^ := ansichar(m_len-2);
                    inc(out_p);
                  end else begin
                    dec(m_len,M4_MAX_LEN);
                    out_p^ := #0; {$endif}
m3_m4_len:          inc(out_p);
                    while (m_len>255) do begin // size > 255 are stored as #0
                      dec(m_len,255);
                      out_p^ := #0;
                      inc(out_p);
                    end;
                    out_p^ := ansichar(m_len);
                    inc(out_p);
                  end;
                end;
                pWord(out_p)^ := m_off shl 2; // Wn: 0..1=new 2..15=offs0..14
                inc(out_p,2);
                ii := in_p;
                if in_p<ip_end then continue else break;
              end else inc(in_p,8) // if (m_pos[8]=in_p[8])
            else inc(in_p,7)
          else inc(in_p,6)
        else inc(in_p,5)
      else inc(in_p,4)
    else inc(in_p,3);
    // 5.3.2 one of the (m_pos[*]=in_p[0]) was false -> store m_off + m_len=in_p-ii
    // here, we have always m_len=in_p-ii >=3 and <9
    if m_off<=M2_MAX_OFFSET then begin // M2_MAX_OFFSET = 1 shl 11 (offs0..10)
      // M2 is for len <8, storing offset on 11 bits
      // B0: 0..1=new 2..4=offs0..2 5..7=len-1
      // B1: 0..7=offs3..10
      // len-1>=2 therefore t>=(2 shl 5)=64=M2_MARKER
      dec(m_off);
      pWord(out_p)^ := integer(((in_p-ii-1)shl 5) or ((m_off and 7)shl 2) or ((m_off shr 3) shl 8));
//      out_p[0] := ansichar(((in_p-ii-1)shl 5) or ((m_off and 7)shl 2));
//      out_p[1] := ansichar(m_off shr 3);
      inc(out_p,2);
      ii := in_p;
      if in_p<ip_end then continue else break;
    end else
    if m_off<=M3_MAX_OFFSET then begin // M3_MAX_OFFSET = 1 shl 14 (offs0..13)
      // M3 is for every len, storing offset on 14 bits
      // B: 0..4=len-2(<32) 5=M3_MARKER
      // W: 0..1=new 2..15=offs0..13
      dec(m_off);
      pInteger(out_p)^ := integer(M3_MARKER or (in_p-ii-2) or (m_off shl 10));
      inc(out_p,3);
      ii := in_p;
      if in_p<ip_end then continue else break;
    end else
{$ifdef WITHM1} if m_off<M4_MAX_OFFSET then {$endif}
    begin // M3_MARKER = M4_MARKER+16 -> 1..15 ok
      // M3 is for every len, storing (offset-M3_MAX_OFFSET) on 15 bits
     // B: 0..2=len-2 3=off14 4=M4_MARKER
     // W: 0..1=new 2..15=offs0..13
      dec(m_off,M3_MAX_OFFSET);
      out_p^ := ansichar(integer(M4_MARKER or (in_p-ii-2) or // len-2
        ((m_off and M3_MAX_OFFSET)shr M4_OFF_BITS))); // off14 bit
m1:   inc(out_p);
      pWord(out_p)^ := m_off shl 2; // off0..13 bits
      inc(out_p,2);
      ii := in_p;
      if in_p<ip_end then continue else break;
{$ifdef WITHM1}
    end else begin
      // B: 0..2=len-2 (M1 is <16)
      // W: 0..1=new 2..15=offs0..13
      dec(m_off,M4_MAX_OFFSET);
      out_p^ := ansichar(in_p-ii-2);
      goto m1;
{$endif}
    end;
  until false;
  // 6. finished -> store out_len and number of bytes left to store
  out_len := out_p-out_beg;
  result := in_end-ii; // returns source left bytes
end;
{$endif USEASM}

function lzopas_compressdestlen(in_len: integer): integer;
// get maximum possible (worse) compressed size for out_p
begin
  result := in_len+(in_Len shr 3)+(64+7);
  // an incompressed block is store by one #0 for each 255 bytes -> shr 3 is good
end;

function lzopas_compress(in_p: PAnsiChar; in_len: integer; out_p: PAnsiChar): integer;
// compress in_p(in_len) into out_p
// out_p must be at least lzopas_compressdestlen(in_len) bytes long
// returns compressed size in out_p
var out_beg: PAnsiChar;
    t, tt: PtrInt;
label mov;
begin
  out_beg := out_p;
  // 1. store in_len
  if in_len>=$8000 then begin
    pWord(out_p)^ := $8000 or (in_len and $7fff);
    pWord(out_p+2)^ := in_len shr 15;
    inc(out_p,4);
  end else begin
    pWord(out_p)^ := in_len; // in_len<32768 -> stored as word, otherwize as integer
    if in_len=0 then begin
      result := 2;
      exit;
    end;
    inc(out_p,2);
  end;
  // 2. compress
  if in_len<=M2_MAX_LEN+5 then begin  // M2_MAX_LEN+5=13
    // 2.1 source is not big enough to be hashed -> direct copy
    t := in_len;
    out_p^ := ansichar(t+17); // out_p=op_beg -> avoid PByte(out_p-2)^ access
    goto mov;
  end else begin
    // 2.2 compress using lzo hashing
    t := _lzo1x_1_do_compress(in_p, in_len, out_p, result);
    inc(out_p,result);
  end;
  // 3. store remaining t bytes
  if t>0 then begin
    if t<=3 then
      inc(out_p[-2],t) else
    if t<=18 then begin
      out_p^ := ansichar(t-3);
      inc(out_p);
    end else begin
      tt := t-18;
      out_p^ := #0;
      inc(out_p);
      while tt>255 do begin // size > 255 are stored as #0
        dec(tt,255);
        out_p^ := #0;
        inc(out_p);
      end;
      out_p^ := ansichar(tt);
mov:  inc(out_p);
    end;
    // we recommend using FastCode (with SSE2) (included in D2009)
    system.move((in_p+in_len-t)^,out_p^,t);
    inc(out_p,t);
  end;
  result := out_p-out_beg;
end;

function lzopas_decompressdestlen(in_p: PAnsiChar): integer;
// get uncompressed size from lzo-compressed buffer (to reserve memory, e.g.)
begin
  result := pWord(in_p)^;
  inc(in_p,2);
  if result and $8000<>0 then
    result := (result and $7fff) or (integer(pWord(in_p)^) shl 15);
end;

function lzopas_decompress(in_p: PAnsiChar; in_len: integer; out_p: PAnsiChar): Integer;
// uncompress in_p(in_len) into out_p, returns out_len
// may write up to out_len+3 bytes in out_p
// the decompression mode is "fast-unsafe" -> CRC/Adler32 in_p data before call
{$ifdef USEASM}
// code below was extracted from the pascal code generated by Delphi 2009
// after some manual further optimization of the asm, here's something fast...
asm
        push    ebx
        push    esi
        push    edi
        push    ebp
        add     esp, -16
        mov     edi, ecx
        mov     [esp], edx
        mov     esi, eax
        mov     eax, [esp]
        add     eax, esi
        mov     [esp+8H], eax
        movzx   eax, word ptr [esi]
        test    eax,eax
        mov     [esp+4H], eax
        je      @@1829
        add     esi, 2
        test    byte ptr [esp+5H], 80H
        jz      @@1806
        mov     eax, [esp+4H]
        and     eax, 7FFFH
        movzx   edx, word ptr [esi]
        shl     edx, 15
        or      eax, edx
        mov     [esp+4H], eax
        add     esi, 2
@@1806: mov     eax, [esp+4H]
        add     eax, edi
        mov     [esp+0CH], eax
        movzx   ebx, byte ptr [esi]
        cmp     ebx, 17
        jle     @@1807
        sub     ebx, 17
        inc     esi
        cmp     ebx, 4
        jl      @@1826
@@s:    mov     al,[esi]
        mov     [edi],al
        dec     ebx
        lea     esi,[esi+1]
        lea     edi,[edi+1]
        jnz     @@s
//        mov ecx, ebx; rep movsb  :( damn slow on Core Duo
        jmp     @@1812

        nop;nop
@@1807: cmp     esi, [esp+8H]
        jnc     @@1829
@@1808: movzx   ebx, byte ptr [esi]
        inc     esi
        cmp     ebx, 16
        jge     @@1813
        test    ebx, ebx
        jnz     @@1811
        cmp     byte ptr [esi], 0
        jnz     @@180a
@@1809: add     ebx, 255
        inc     esi
@@1810: cmp     byte ptr [esi], 0
        jz      @@1809
@@180a: movzx   eax, byte ptr [esi]
        add     eax,15
        add     ebx,eax
        inc     esi
@@1811: add     ebx,3
        mov     edx, edi
        mov     eax, esi
        mov     ecx, ebx
        call    move
        add     esi, ebx
        add     edi, ebx
@@1812: cmp     esi, [esp+8H]
        jnc     @@1829
        movzx   ebx, byte ptr [esi]
        inc     esi
@@1813: cmp     ebx, 64
        jl      @@1814
        lea     ebp, [edi-1]
        mov     eax, ebx
        shr     eax, 2
        and     eax, 07H
        sub     ebp, eax
        mov     al, [esi]
        inc     esi
        shl     eax, 3
        shr     ebx, 5
        sub     ebp, eax
        inc     ebx
        lea     eax, [ebx+edi]
        cmp     eax, [esp+0CH]
        jbe     @@1824
        mov     ebx, [esp+0CH]
        sub     ebx, edi
        jmp     @@1824

@@1814: cmp     ebx, 32
        jl      @@1818
        and     ebx, 1FH
        jnz     @@1817
        cmp     byte ptr [esi], 0
        jnz     @@181a
@@1815: add     ebx, 255
        inc     esi
@@1816: cmp     byte ptr [esi], 0
        jz      @@1815
@@181a: movzx   eax, byte ptr [esi]
        add     eax,31
        add     ebx,eax
        inc     esi
@@1817: lea     ebp, [edi-1]
        movzx   eax, word ptr [esi]
        shr     eax, 2
        sub     ebp, eax
        add     esi, 2
        jmp     @@1822

@@1818: cmp     ebx, 16
        jl      @@1822
        mov     eax, ebx
        and     eax, 08H
        shl     eax, 11
        lea     ebp,[edi-16384]
        sub     ebp, eax
        and     ebx, 07H
        jnz     @@1821
        cmp     byte ptr [esi], 0
        jnz     @@182a
@@1819: add     ebx, 255
        inc     esi
@@1820: cmp     byte ptr [esi], 0
        jz      @@1819
@@182a: movzx   eax, byte ptr [esi]
        add     eax, 7
        add     ebx, eax
        inc     esi
@@1821: movzx   eax, word ptr [esi]
        shr     eax, 2
        sub     ebp, eax
        add     esi, 2
@@1822: lea     eax, [ebx+edi+2]
        lea     ebx, [ebx+2]
        cmp     eax, [esp+0CH]
        jbe     @@1823
        mov     ebx, [esp+0CH]
        sub     ebx, edi
@@1823: cmp     ebx, 6
        mov     ecx, edi
        jl      @@1824
        sub     ecx, ebp
        cmp     ebx, ecx
        jg      @@1824
        mov     edx, edi
        mov     eax, ebp
        mov     ecx, ebx
        call    move
        add     edi, ebx
        jmp     @@1825
        nop;nop;nop
@@1824: mov     al,[ebp]
        mov     [edi],al
        dec     ebx
        lea     edi,[edi+1]
        jz      @@1825
        mov     al,[ebp+1]
        mov     [edi],al
        dec     ebx
        lea     ebp,[ebp+2]
        lea     edi,[edi+1]
        jnz     @@1824
@@1825: movzx   ecx, byte ptr [esi-2H]
        and     ecx, 3
        jz      @@1807
@@1826: dec     ecx
        mov     al,[esi]
        mov     [edi],al
        lea     esi,[esi+1]
        lea     edi,[edi+1]
        jz      @@1827
        dec     ecx
        mov     al,[esi]
        mov     [edi],al
        lea     esi,[esi+1]
        lea     edi,[edi+1]
        jz      @@1827
        mov     al,[esi]
        mov     [edi],al
        lea     esi,[esi+1]
        lea     edi,[edi+1]
@@1827: movzx   ebx, byte ptr [esi]
        lea     esi,[esi+1]
        cmp     esi,[esp+8H]
        jc      @@1813
@@1829: mov     eax, [esp+4H]
        add     esp, 16
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
end;
{$else}
var ip_end, m_pos, out_end: PAnsiChar;
    t: PtrInt;
label match_next, first_literal_run, match, match_done, copy_m, m1;
begin
  ip_end := in_p+in_len;
  // 1. get true uncompressed size (algo may add 3 bytes -> we need exact length)
  result := pWord(in_p)^;
  if result=0 then exit;
  inc(in_p,2);
  if result and $8000<>0 then begin
    result := (result and $7fff) or (integer(pWord(in_p)^) shl 15);
    inc(in_p,2);
  end;
  out_end := out_p+result;
  // 2. uncompress first block
  t := ord(in_p[0]);
  if t>17 then begin
    dec(t,17);
    inc(in_p);
    if t<4 then
      goto match_next;
    movechars(in_p,out_p,t);
    inc(out_p,t);
    inc(in_p,t);
    goto first_literal_run;
  end;
  // 3. two decompression loops for uncompressing
  while in_p<ip_end do begin
    // 3.1 outer loop is for common blocks
    t := ord(in_p[0]);
    inc(in_p);
    if t>=16 then
      goto match else
    if t=0 then begin
      while in_p[0]=#0 do  begin // size > 255 are stored as #0
        inc(t,255);
        inc(in_p);
      end;
      inc(t,15+ord(in_p[0]));
      inc(in_p);
    end;
    inc(t,3);
    // we recommend using FastCode (with SSE2) (included in D2009)
    system.Move(in_p^,out_p^,t);
    inc(in_p,t);
    inc(out_p,t);
first_literal_run:
    if in_p>=ip_end then // this was missing in the original files
      break;
    t := ord(in_p[0]);
    inc(in_p);
    repeat
      // 3.2 inside loop can deal with ord(in_p[-2]) and 3 copy paterns
match:if t>=M2_MARKER then begin // M2_MARKER=M3_MARKER+32
        // B0: 0..1=new 2..4=offs0..2 5..7=len-1
        // B1: 0..7=offs3..10
        // len-1>=2 => t>=(2 shl 5)=64=M2_MARKER
        // M2_MAX_OFFSET = $0800 = 1 shl 11 (offs0..10)
        m_pos := out_p-1-((t shr 2) and 7)-(ord(in_p[0])shl 3);
        inc(in_p);
        t := (t shr 5)+1;
        if out_p+t>out_end then // avoid out_p overflow
          t := out_end-out_p;
        goto copy_m;
      end else
      if t>=M3_MARKER then begin // M3_MARKER = M4_MARKER+16
        // B0: 0..4=len-2(<32) 5=M3_MARKER
        // B1..n-1 #0 = inc(len,255), or len-33
        // Wn: 0..1=new 2..15=offs0..13
        // M3_MAX_OFFSET = $4000 = 1 shl 14 (offs0..13)
        t := t and 31;
        if t=0 then begin // len-2=0 => len stored with #0
          while in_p[0]=#0 do  begin // size > 255 are stored as #0
            inc(t,255);
            inc(in_p);
          end;
          inc(t,31+ord(in_p[0]));
          inc(in_p);
        end;
        m_pos := out_p-1-(pWord(in_p)^ shr 2);
        inc(in_p,2);
      end else
      if t>=M4_MARKER then begin
        // B0: 0..2=len-2 3=off14 4=M4_MARKER
        // B1..Bn-1 #0 = inc(len,255), or len-9
        // Wn: 0..1=new 2..15=offs0..13
        m_pos := out_p-((t and 8)shl M4_OFF_BITS)-M3_MAX_OFFSET;
{$ifdef WITHM1}
        goto m1;
      end else begin
        // B0: 0..2=len-2 (M1 is <16)
        // B1..Bn-1 #0 = inc(len,255), or len-9
        // Wn: 0..1=new 2..15=offs0..14
        m_pos := out_p-M4_MAX_OFFSET;
{$endif}
m1:     t := t and 7;
        if t=0 then begin // size > 255 are stored as #0
          while in_p[0]=#0 do  begin
            inc(t,255);
            inc(in_p);
          end;
          inc(t,7+ord(in_p[0]));
          inc(in_p);
        end;
        dec(m_pos,pWord(in_p)^ shr 2);
        inc(in_p,2);
      end;
      inc(t,2);
      if out_p+t>out_end then // avoid out_p overflow
        t := out_end-out_p;
      if (t>=6) and (out_p-m_pos>=t) then // >=t avoid overlap cf {$define WT}
        system.Move(m_pos^,out_p^,t) else // FastCode move() is prefered here
copy_m: movechars(m_pos,out_p,t); // small or overlapping move proc
      inc(out_p,t);
match_done:
      t := ord(in_p[-2]) and 3;
      if t=0 then break;
match_next:
      out_p^ := in_p^; // copy t (=1,2 or 3) bytes
      inc(out_p);
      inc(in_p);
      if t<>1 then begin
        out_p^ := in_p^;
        inc(out_p);
        inc(in_p);
        if t=3 then begin
          out_p^ := in_p^;
          inc(out_p);
          inc(in_p);
        end;
      end;
      t := ord(in_p[0]);
      inc(in_p);
    until in_p>=ip_end;
  end;
end;
{$endif USEASM}


{$ifdef LZOFILE}
{$ifdef WIN32}

function adler32(adler: cardinal; buf: pointer; len: cardinal): cardinal;
{$ifdef USEASM}
// our optimized asm version
asm
	push      ebx
	push      esi
	push      edi
	mov       edi,eax
	shr       edi,16
	movzx     ebx,ax
	push      ebp
	mov       esi,edx
	test      esi,esi
	mov       ebp,ecx
	jne       @31
	mov       eax,1
	jmp       @32
@31:
	test      ebp,ebp
	jbe       @34
@33:
	cmp       ebp,5552
	jae        @35
	mov       eax,ebp
	jmp        @36
@35:
	mov       eax,5552
@36:
	sub       ebp,eax
	cmp       eax,16
	jl        @38
	xor       edx,edx
	xor       ecx,ecx
@39:
	sub       eax,16
	mov       dl,[esi]
	mov       cl,[esi+1]
	add       ebx,edx
	add       edi,ebx
	add       ebx,ecx
	mov       dl,[esi+2]
	add       edi,ebx
	add       ebx,edx
	mov       cl,[esi+3]
	add       edi,ebx
	add       ebx,ecx
	mov       dl,[esi+4]
	add       edi,ebx
	add       ebx,edx
	mov       cl,[esi+5]
	add       edi,ebx
	add       ebx,ecx
	mov       dl,[esi+6]
	add       edi,ebx
	add       ebx,edx
	mov       cl,[esi+7]
	add       edi,ebx
	add       ebx,ecx
	mov       dl,[esi+8]
	add       edi,ebx
	add       ebx,edx
	mov       cl,[esi+9]
	add       edi,ebx
	add       ebx,ecx
	mov       dl,[esi+10]
	add       edi,ebx
	add       ebx,edx
	mov       cl,[esi+11]
	add       edi,ebx
	add       ebx,ecx
	mov       dl,[esi+12]
	add       edi,ebx
	add       ebx,edx
	mov       cl,[esi+13]
	add       edi,ebx
	add       ebx,ecx
	mov       dl,[esi+14]
	add       edi,ebx
	add       ebx,edx
	mov       cl,[esi+15]
	add       edi,ebx
	add       ebx,ecx
	cmp       eax,16
	lea       esi,[esi+16]
	lea       edi,[edi+ebx]
	jge       @39
@38:
	test      eax,eax
	je         @42
@43:
	xor       edx,edx
	mov       dl,[esi]
	add       ebx,edx
	dec       eax
	lea       esi,[esi+1]
  lea       edi,[edi+ebx]
	jg        @43
@42:
	mov       ecx,65521
	mov       eax,ebx
	xor       edx,edx
	div       ecx
	mov       ebx,edx
	mov       ecx,65521
	mov       eax,edi
	xor       edx,edx
	div       ecx
	test      ebp,ebp
	mov       edi,edx
	ja        @33
@34:
	mov       eax,edi
	shl       eax,16
	or        eax,ebx
@45:
@32:
	pop       ebp
	pop       edi
	pop       esi
	pop       ebx
end;
{$else}
// standard pascal version, fast enough
var s1, s2: cardinal;
    i, n: integer;
begin
  s1 := LongRec(Adler).Lo;
  s2 := LongRec(Adler).Hi;
  while len>0 do begin
    if len<5552 then
      n := len else
      n := 5552;
    for i := 1 to n do begin
      inc(s1,pByte(buf)^);
      inc(cardinal(buf));
      inc(s2,s1);
    end;
    s1 := s1 mod 65521;
    s2 := s2 mod 65521;
    dec(len,n);
  end;
  result := word(s1)+cardinal(word(s2)) shl 16;
end;
{$endif}


// file compression functions using fast SynLZO library:
// - the file is cut into blocks of fixed size (128MB by default)
//    therefore, there is no size limit for the file to be compressed
// - Adler32 checksum of the compressed data and original file time are stored
//    for integrity check and proper decompression into same file as original
// - code below is very fast, using the OS+CPU capabilities to manage buffering
// - for huge files (several GB) the speed is only limited by the disk access,
//    not by the CPU efficiency: it's faster than ZIP for daily use
//    => SynLZO file compression is a must-have for fast SQLITE backup, e.g.

const
  SynLZOMagic: array[0..7] of char = 'SynLZO'#26#1; // SynLZO file version #1

function lzopas_compressfile2(const srcFile, dstFile: AnsiString; BlockSizeMB: integer=128): boolean;
// do the magic: srcFile is compressed into dstFile
// on error, return false and error code in GetLastError
var sHandle, dHandle: cardinal;  // file handle
    sMap, dMap: cardinal;  // file map handle
    sData, dData: pAnsiChar; // file mapped memory
    sSize, dSize: Int64;  // file length
    sSizeRec: Int64Rec absolute sSize;
    dSizeRec: Int64Rec absolute dSize;
    blockCount,
    blockSize,
    cSize: cardinal;
    cData: PAnsiChar;
    sPos, dPos: Int64; // file position
    sPosRec: Int64Rec absolute sPos;
    dPosRec: Int64Rec absolute dPos;
    sTime: TFileTime;  // file time
    err: cardinal;
    SystemInfo: TSystemInfo;
    mask: cardinal;
    i: integer;
begin
//  if BlockSizeMB<64 then BlockSizeMB := 64 else // minimum 64MB memory map windows size
  if BlockSizeMB>512 then
    BlockSizeMB := 512; // 521MB is big enough for memory map window size
  result := false;
  err := 0;
  sPos := 0;
  dPos := 0;
  try
    sHandle := CreateFileA(pointer(srcFile), GENERIC_READ,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    if sHandle <> INVALID_HANDLE_VALUE then begin
      dHandle := CreateFileA(pointer(dstFile), GENERIC_READ or GENERIC_WRITE,
        0, nil, CREATE_ALWAYS, 0, 0);
      if dHandle <> INVALID_HANDLE_VALUE then begin
        sMap := CreateFileMapping(sHandle, nil, PAGE_READONLY, 0, 0, nil);
        if sMap <> 0 then begin
          GetFileTime(sHandle, nil, nil, @sTime);
          sSizeRec.Lo := GetFileSize(sHandle, @sSizeRec.Hi); // get sSize
          BlockSize := BlockSizeMB*(1024*1024); // MB -> bytes
          BlockCount := sSize div BlockSize;
          if BlockCount>0 then begin
            GetSystemInfo(SystemInfo); // get memory mapped offset alignement mask
            dSize := Int64(BlockCount+1)*BlockSize+BlockSize shr 3+
              (64+36)+SystemInfo.dwAllocationGranularity;
            mask := not(SystemInfo.dwAllocationGranularity-1);
            // standard is 64KB granularity -> mask=$ffff0000
          end else begin
            dSize := sSize+sSize shr 3+(64+36);
            mask := 0; // one block -> dPos=0 -> mask=0
          end;
          dMap := CreateFileMapping(dHandle, nil, PAGE_READWRITE, dSizeRec.Hi, dSizeRec.Lo, nil);
          if dMap <> 0 then begin
            // here is the blocks loop to compress from sMap into dMap:
            for i := 0 to BlockCount do begin
              if i=integer(BlockCount) then // last block = map only remaining bytes
                BlockSize := sSize-sPos;
              sData := MapViewOfFile(sMap, FILE_MAP_READ, // map source block
                sPosRec.Hi, sPosRec.Lo, // source blocks are MB -> always aligned
                BlockSize);
              if sData <> nil then begin
                dData := MapViewOfFile(dMap, FILE_MAP_ALL_ACCESS,
                  dPosRec.Hi, dPosRec.Lo and mask,
                  dPosRec.Lo and not(mask)+BlockSize+BlockSize shr 3+(64+36));
                if dData <> nil then begin // do block compression
                  cData := dData+dPosRec.Lo and not(mask);
                  if i=0 then begin // first block begin = header
                    PInt64(cData)^ := PInt64(@SynLZOMagic[0])^; // magic
                    PInt64(cData+8)^ := sSize;        // decompressed size
                    PFileTime(cData+16)^ := sTime;    // file age
                    PCardinal(cData+28)^ := adler32(0,cData,28); // header checksum
                    inc(cData,32); // header is 32 bytes long
                  end;
                  cSize := lzopas_compress(sData,BlockSize,cData);
                  result := cSize > 0;
                  if result then begin
                    // add checksum of compressed data at the window end
                    PCardinal(cData+cSize)^ := adler32(0,cData,cSize);
                    if i=0 then // header size=32
                      inc(cSize,36) else
                      inc(cSize,4);
                    inc(dPos,cSize);
                  end else
                    err := ERROR_ACCESS_DENIED;
                  UnmapViewOfFile(dData);
                end else err := GetLastError;
                UnmapViewOfFile(sData);
              end else err := GetLastError;
              if result then
                inc(sPos,BlockSize) else
                break;
            end;
            CloseHandle(dMap);
          end else err := GetLastError;
          CloseHandle(sMap);
          if result then
            if sPos<>sSize then begin
              err := ERROR_INVALID_BLOCK;
              result := false;
            end else begin
              SetFilePointer(dHandle, dPosRec.Lo, @dPosRec.Hi, FILE_BEGIN);
              SetEndOfFile(dHandle); // truncate compressed file into used size
            end;
        end else err := GetLastError;
        CloseHandle(dHandle);
        if not result then
          windows.DeleteFile(pointer(dstFile));
      end else err := GetLastError;
      CloseHandle(sHandle);
    end else err := GetLastError;
  except
    SetFileAttributes(pointer(dstFile), 0);
    windows.DeleteFile(pointer(dstFile));
    err := ERROR_ACCESS_DENIED;
  end;
  if not result then
    SetLastError(err);
end;

function lzopas_compressfile(const srcFile, dstFile: AnsiString; dstSize: PInteger=nil;
  methodLen: TCompressLen=nil; methodComp: TCompressFunc=nil): boolean;
// do the magic: srcFile is compressed into dstFile
// on error, return false and error code in GetLastError
var sf, df: cardinal;  // file handle
    sm, dm: cardinal;  // file map handle
    sb, db: pAnsiChar; // file mapped memory
    sl, dl: integer;   // file length
    st: TFileTime;     // file time
    err: cardinal;
begin
  result := false;
  err := 0;
  try
    sf := CreateFileA(pointer(srcFile), GENERIC_READ,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    if sf <> INVALID_HANDLE_VALUE then begin
      df := CreateFileA(pointer(dstFile), GENERIC_READ or GENERIC_WRITE,
        0, nil, CREATE_ALWAYS, 0, 0);
      if df <> INVALID_HANDLE_VALUE then begin
        sm := CreateFileMapping(sf, nil, PAGE_READONLY, 0, 0, nil);
        if sm <> 0 then begin
          GetFileTime(sf, nil, nil, @st);
          sl := GetFileSize(sf, nil);
          if sl>2147480000 then // up to 2GB file size
            raise Exception.Create('');
          if @methodLen=nil then
            dl := lzopas_compressdestlen(sl) else
            dl := methodLen(sl);
          dm := CreateFileMapping(df, nil, PAGE_READWRITE, 0, dl, nil);
          if dm <> 0 then begin
            sb := MapViewOfFile(sm, FILE_MAP_READ, 0, 0, 0);
            if sb <> nil then begin
              db := MapViewOfFile(dm, FILE_MAP_ALL_ACCESS, 0, 0, 0);
              if db <> nil then begin
                if @methodComp=nil then
                  dl := lzopas_compress(sb,sl,db) else // compression
                  dl := methodComp(sb,sl,db); // compression
                result := dl > 0;
                if result then begin
                  PCardinal(db+dl)^ := adler32(0,db,dl); // integrity checksum
                  PFileTime(db+dl+sizeof(cardinal))^ := st; // store file age
                end;
                UnmapViewOfFile(db);
              end else err := GetLastError;
              UnmapViewOfFile(sb);
            end else err := GetLastError;
            CloseHandle(dm);
          end else err := GetLastError;
          CloseHandle(sm);
          if result then begin
            inc(dl,sizeof(cardinal)+sizeof(TFileTime));
            SetFilePointer(df, dl, nil, FILE_BEGIN);
            if dstSize<>nil then
              dstSize^ := dl;
            SetEndOfFile(df); // truncate compressed file into used size
          end;
        end else err := GetLastError;
        if result then
          SetFileTime(df, nil, nil, @st); // file age set
        CloseHandle(df);
        if not result then
          windows.DeleteFile(pointer(dstFile));
      end else err := GetLastError;
      CloseHandle(sf);
    end else err := GetLastError;
  except
    SetFileAttributes(pointer(dstFile), 0);
    windows.DeleteFile(pointer(dstFile));
    err := ERROR_ACCESS_DENIED;
  end;
  if not result then
    SetLastError(err);
end;

function lzopas_decompressfile(const srcFile, dstFile: AnsiString): boolean;
// uncompress srcFile into dstFile (with checksum and file age restore)
// on error, return false and error code in GetLastError
var sf, df: cardinal;   // file handle
    sm, dm: cardinal;   // file map handle
    sb, db: PAnsiChar;  // file mapped memory
    sl, dl: integer;    // file length
    st: TFileTime;      // file time
    err: cardinal;
begin
  result := false;
  err  := 0;
  try
    sf := CreateFileA(pointer(srcFile), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
      nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN, 0);
    if sf <> INVALID_HANDLE_VALUE then begin
      df := CreateFileA(pointer(dstFile), GENERIC_READ or GENERIC_WRITE, 0, nil,
        CREATE_ALWAYS, FILE_FLAG_SEQUENTIAL_SCAN, 0);
      if df <> INVALID_HANDLE_VALUE then begin
        sm := CreateFileMapping(sf, nil, PAGE_READONLY, 0, 0, nil);
        if sm <> 0 then begin
          sb := MapViewOfFile(sm, FILE_MAP_READ, 0, 0, 0);
          if sb <> nil then begin
            Int64(st) := 0;
            sl := GetFileSize(sf, nil);
            dl := lzopas_decompressdestlen(sb); // dl+4 (up to 3 bytes overflow)
            dm := CreateFileMapping(df, nil, PAGE_READWRITE, 0, dl+4, nil);
            if dm <> 0 then begin
              db := MapViewOfFile(dm, FILE_MAP_ALL_ACCESS, 0, 0, 0);
              if db <> nil then begin
                dec(sl,sizeof(cardinal)+sizeof(TFileTime));
                if adler32(0,sb,sl)=PCardinal(sb+sl)^ then begin // integrity 
                  result := lzopas_decompress(sb,sl,db)=dl; // decompression
                  if result then
                    st := PFileTime(sb+sl+sizeof(cardinal))^;
                end;
                UnmapViewOfFile(db);
              end else err := GetLastError;
              CloseHandle(dm);
            end else err := GetLastError;
            UnmapViewOfFile(sb);
            if result then begin
              SetFilePointer(df, dl, nil, FILE_BEGIN);
              SetEndOfFile(df); // file truncate to attempted size
              SetFileTime(df, nil, nil, @st); // file age set
            end;
          end else err := GetLastError;
          CloseHandle(sm);
        end else err := GetLastError;
        CloseHandle(df);
        if not result then
          windows.DeleteFile(pointer(dstFile));
      end else err := GetLastError;
      CloseHandle(sf);
    end else err := GetLastError;
  except
    SetFileAttributes(pointer(dstFile), 0);
    windows.DeleteFile(pointer(dstFile));
    err := ERROR_ACCESS_DENIED;
  end;
  if not result then
    SetLastError(err);
end;

function lzopas_decompressfilesize(const srcFile: AnsiString): integer;
// -1 if error, decompressed size otherwize
var sf: cardinal;
    W: word;
begin
  result := -1;
  sf := CreateFileA(pointer(srcFile), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  if sf <> INVALID_HANDLE_VALUE then
  try
    if FileRead(sf,W,2)<>2 then
      exit;
    result := W;
    if result and $8000<>0 then
      if FileRead(sf,W,2)<>2 then
        result := -1 else
        result := (result and $7fff) or (integer(W) shl 15);
  finally
    CloseHandle(sf);
  end;
end;

function lzopas_decompressfilecheck(const srcFile: AnsiString): boolean;
// true if file checksum is correct (no transmission fail, e.g.)
var sf : cardinal;  // file handle
    sm: cardinal;   // file map handle
    sb: PAnsiChar;  // file mapped memory
    sl: integer;    // file length
    err: cardinal;
begin
  result := false;
  err  := 0;
  try
    sf := CreateFileA(pointer(srcFile), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN, 0);
    if sf <> INVALID_HANDLE_VALUE then begin
      sm := CreateFileMapping(sf, nil, PAGE_READONLY, 0, 0, nil);
      if sm <> 0 then begin
        sb := MapViewOfFile(sm, FILE_MAP_READ, 0, 0, 0);
        if sb <> nil then begin
          sl := GetFileSize(sf, nil)-(sizeof(cardinal)+sizeof(TFileTime));
          if adler32(0,sb,sl)=PCardinal(sb+sl)^ then
            result := true;
          UnmapViewOfFile(sb);
        end else err := GetLastError;
        CloseHandle(sm);
      end else err := GetLastError;
      CloseHandle(sf);
    end else err := GetLastError;
  except
    err := ERROR_ACCESS_DENIED;
  end;
  if not result then
    SetLastError(err);
end;

{$endif WIN32}
{$endif LZOFILE}

function Hash32(P: PIntegerArray; L: integer): cardinal;
// faster than Adler32, even asm version, because read DWORD aligned data
var s1,s2: cardinal;
    i: integer;
begin
  if P<>nil then begin
    s1 := 0;
    s2 := 0;
    for i := 1 to L shr 4 do begin // 16 bytes (4 DWORD) by loop - aligned read
      inc(s1,P^[0]);
      inc(s2,s1);
      inc(s1,P^[1]);
      inc(s2,s1);
      inc(s1,P^[2]);
      inc(s2,s1);
      inc(s1,P^[3]);
      inc(s2,s1);
      inc(PByte(P),16);
    end;
    for i := 1 to (L shr 2)and 3 do begin // 4 bytes (DWORD) by loop
      inc(s1,P^[0]);
      inc(s2,s1);
      inc(PByte(P),4);
    end;
    case L and 3 of // remaining 0..3 bytes
    1: inc(s1,PByte(P)^);
    2: inc(s1,PWord(P)^);
    3: inc(s1,PWord(P)^ or (PByteArray(P)^[2] shl 16));
    end;
    inc(s2,s1);
    result := s1 xor (s2 shl 16);
  end else
    result := 0;
end;

function CompressSynLZO(var Data: AnsiString; Compress: boolean): AnsiString;
var DataLen, len: integer;
    P: PAnsiChar;
begin
  DataLen := length(Data);
  if DataLen<>0 then // '' is compressed and uncompressed to ''
  if Compress then begin
    len := lzopas_compressdestlen(DataLen)+8;
    SetString(result,nil,len);
    P := pointer(result);
    PCardinal(P)^ := Hash32(pointer(Data),DataLen);
    len := lzopas_compress(pointer(Data),DataLen,P+8);
    PCardinal(P+4)^ := Hash32(pointer(P+8),len);
    SetString(Data,P,len+8);
  end else begin
    result := '';
    P := pointer(Data);
    if (DataLen<=8) or (Hash32(pointer(P+8),DataLen-8)<>PCardinal(P+4)^) then
      exit;
    len := lzopas_decompressdestlen(P+8);
    SetLength(result,len);
    if (len<>0) and ((lzopas_decompress(P+8,DataLen-8,pointer(result))<>len) or
       (Hash32(pointer(result),len)<>PCardinal(P)^)) then begin
      result := '';
      exit;
    end else
      SetString(Data,PAnsiChar(pointer(result)),len);
  end;
  result := 'synlzo';
end;


initialization
  // in the above code, integer must be 4 bytes, word 2 bytes
  assert(sizeof(integer)=4);
  assert(sizeof(word)=2);
  {
   e:\copie cd\DamnSmallLinux43.iso (big already compressed data)
   Compress 2 times, Uncompress 8 times

  LZO Compress: 4539ms, 52049920->51651086, ratio=17, speed = 22 MB/s
  LZO Decompress: 515ms, speed = 789 MB/s
  LZO asm Compress: 2823ms, 52049920->51651087, ratio=17, speed = 36 MB/s
  Delphi 2009 LZO pas Compress: 2980ms, speed = 34 MB/s
  LZO pas Decompress: 468ms, speed = 868 MB/s
  Delphi 7 LZO pas Compress: 3823ms, speed = 26 MB/s
  LZO pas Decompress: 468ms, speed = 868 MB/s

   e:\copie cd\DamnSmallLinux43.iso
   Compress 4 times, Uncompress 16 times

  LZO Compress: 8955ms, 52049920->51651086, ratio=17, speed = 22 MB/s
  LZO Decompress: 951ms, speed = 855 MB/s
  LZO asm Compress: 5647ms, 52049920->51651087, ratio=17, speed = 36 MB/s
  Delphi 2009 LZO pas Compress: 5975ms, speed = 34 MB/s
  LZO pas Decompress: 905ms, speed = 898 MB/s
  Delphi 7 LZO pas Compress: 7675ms, speed = 26 MB/s
  LZO pas Decompress: 889ms, speed = 914 MB/s

   CATENA5.TXT
   Compress 20 times, Uncompress 80 times

  LZO Compress: 1778ms, 6358752->3289373, ratio=193, speed = 69 MB/s
  LZO Decompress: 2777ms, speed = 178 MB/s
  LZO asm Compress: 1420ms, 6358752->3289372, ratio=193, speed = 87 MB/s
  Delphi 2009 LZO pas Compress: 1498ms, speed = 82 MB/s
  LZO pas Decompress: 3089ms, speed = 160 MB/s
  Delphi 7 LZO pas Compress: 1794ms, speed = 69 MB/s
  LZO pas Decompress: 3136ms, speed = 158 MB/s

   CATENA5.TXT
   Compress 100 times, Uncompress 400 times

  LZO Compress: 8892ms, 6358752->3289373, ratio=193, speed = 69 MB/s
  LZO Decompress: 13791ms, speed = 180 MB/s
  LZO asm Compress: 7145ms, 6358752->3289372, ratio=193, speed = 86 MB/s
  Delphi 2009 LZO pas Compress: 7457ms, speed = 83 MB/s
  LZO pas Decompress: 15210ms, speed = 163 MB/s
  Delphi 7 LZO pas Compress: 8893ms, speed = 69 MB/s
  LZO pas Decompress: 15507ms, speed = 160 MB/s

   \Dev\Temp\lzo\Pas\TestLZO.exe
   Compress 2000 times, Uncompress 8000 times

  LZO Compress: 3557ms, 97280->57248, ratio=169, speed = 53 MB/s
  LZO Decompress: 2496ms, speed = 304 MB/s
  Delphi 2009 LZO pas Compress: 2745ms, speed = 69 MB/s
  LZO pas Decompress: 2621ms, speed = 289 MB/s
  Delphi 7 (smallest exe size)
  LZO Compress: 2543ms, 57856->39772, ratio=145, speed = 44 MB/s
  LZO Decompress: 1716ms, speed = 263 MB/s
  LZO asm Compress: 57856->39773, 1794ms, speed = 62 MB/s
  LZO pas Compress: 2277ms, speed = 49 MB/s
  LZO pas Decompress: 1935ms, speed = 233 MB/s

   \Dev\Temp\lzo\Pas\TestLZO.exe
   Compress 10000 times, Uncompress 40000 times

  LZO Compress: 17940ms, 97280->57248, ratio=169, speed = 52 MB/s
  LZO Decompress: 12496ms, speed = 304 MB/s
  Delphi 2009 LZO pas Compress: 13806ms, speed = 68 MB/s
  LZO pas Decompress: 13119ms, speed = 289 MB/s
  Delphi 7 (smallest exe size)
  LZO Compress: 12730ms, 57856->39772, ratio=145, speed = 44 MB/s
  LZO Decompress: 8580ms, speed = 263 MB/s
  LZO asm Compress: 9001ms, 57856->39773, ratio=145, speed = 62 MB/s
  LZO pas Compress: 11388ms, speed = 49 MB/s
  LZO pas Decompress: 9672ms, speed = 233 MB/s

   MiniLZO.cs
   Compress 10000 times, Uncompress 40000 times

  LZO Compress: 1529ms, 25252->5844, ratio=432, speed = 161 MB/s
  LZO Decompress: 2184ms, speed = 451 MB/s
  LZO asm Compress: 1201ms, 25252->5842, ratio=432, speed = 205 MB/s
  Delphi 2009 LZO pas Compress: 1404ms, speed = 175 MB/s
  LZO pas Decompress: 2246ms, speed = 439 MB/s
  Delphi 7 LZO pas Compress: 2091ms, speed = 117 MB/s
  LZO pas Decompress: 2309ms, speed = 427 MB/s

   MiniLZO.cs
   Compress 20000 times, Uncompress 80000 times

  LZO Compress: 3042ms, 25252->5844, ratio=432, speed = 162 MB/s
  LZO Decompress: 4368ms, speed = 451 MB/s
  LZO asm Compress: 2418ms, 25252->5842, ratio=432, speed = 203 MB/s
  Delphi 2009 LZO pas Compress: 2715ms, speed = 181 MB/s
  LZO pas Decompress: 4524ms, speed = 436 MB/s
  Delphi 7 LZO pas Compress: 4165ms, speed = 118 MB/s
  LZO pas Decompress: 4680ms, speed = 421 MB/s

   KLOG.xml
   Compress 20000 times, Uncompress 80000 times

  LZO Compress: 639ms, 6034->1347, ratio=447, speed = 184 MB/s
  LZO Decompress: 952ms, speed = 495 MB/s
  LZO asm Compress: 609ms, 6034->1342, ratio=449, speed = 193 MB/s
  Delphi 2009 LZO pas Compress: 749ms, speed = 157 MB/s
  LZO pas Decompress: 952ms, speed = 495 MB/s
  Delphi 7 LZO pas Compress: 1030ms, speed = 114 MB/s
  LZO pas Decompress: 4634ms, speed = 425 MB/s

   KLOG.xml
   Compress 50000 times, Uncompress 200000 times

  LZO Compress: 1591ms, 6034->1347, ratio=447, speed = 185 MB/s
  LZO Decompress: 2340ms, speed = 503 MB/s
  LZO asm Compress: 1513ms, 6034->1342, ratio=449, speed = 194 MB/s
  Delphi 2009 LZO pas Compress: 1825ms, speed = 177 MB/s
  LZO pas Decompress: 2387ms, speed = 493 MB/s
  Delphi 7 LZO pas Compress: 2543ms, speed = 115 MB/s
  LZO pas Decompress: 2402ms, speed = 490 MB/s

  About ZIP compression (optimized C compiled code, version 1.2.3):
  * zip compression level=6 (normal)
    MiniLZO.cs zip 25252->3707, ratio=681, 15 MB/s, unzip 162 MB/s
    KLOG.xml zip 6034->806, ratio=748, 14 MB/s, unzip 130 MB/s
  * zip compression level=1 (fastest)
    MiniLZO.cs zip 25252->4523, ratio=558, 34 MB/s, unzip 139 MB/s
    KLOG.xml zip 6034->994, ratio=607, 20 MB/s, unzip 115 MB/s

}
end.

