/// SynLZ Compression routines
// - licensed under a MPL/GPL/LGPL tri-license; version 1.17
unit SynLZ;

{
    This file is part of Synopse SynLZ Compression.

    Synopse SynLZ Compression. Copyright (C) 2012 Arnaud Bouchez
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

  The Original Code is Synopse SynLZ Compression.

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


     SynLZ Compression / Decompression library
     =========================================
      by Arnaud Bouchez http://bouchez.info

    * SynLZ is a very FAST lossless data compression library
      written in optimized pascal code for Delphi 3 up to Delphi 2009
      with a tuned asm version available
    * symetrical compression and decompression speed (which is
      very rare above all other compression algorithms in the wild)
    * good compression rate (usualy better than LZO)
    * fastest averrage compression speed (ideal for xml/text communication, e.g.)

    SynLZ implements a new compression algorithm with the following features:
    * hashing+dictionary compression in one pass, with no huffman table
    * optimized 32bits control word, embedded in the data stream
    * in-memory compression (the dictionary is the input stream itself)
    * compression and decompression have the same speed (both use hashing)
    * thread safe and lossless algorithm
    * supports overlapping compression and in-place decompression
    * code size for compression/decompression functions is smaller than LZO's

    The contents of this file are subject to the Mozilla Public License
    Version 1.1 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at
    http://www.mozilla.org/MPL
    Software distributed under the License is distributed on an "AS IS"
    basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
    License for the specific language governing rights and limitations
    under the License.

    The Initial Developer of the Original Code is Arnaud Bouchez.
    This work is Copyright (C)2008 Arnaud Bouchez - http://bouchez.info

    Conversion notes:
    - this format is NOT stream compatible with any lz* official format
       => use it internally in your application, not as exchange format
    - very small code size (less than 1KB for both compressor/decompressor)
    - the uncompressed data length is stored in the beginning of the stream
       and can be retrieved easily for proper out_p memory allocation
    - please give correct data to the decompressor (i.e. first CRC in_p data)
       => we recommend our very fast Adler32 procedure, or a zip-like container
    - a 2nd more tuned algorithm is included, but is somewhat slower in practice
       => use SynLZ[de]compres1*() functions in your applications
    - tested and benchmarked with a lot of data types/sizes
       => use the asm code, which is very tuned: SynLZ[de]compress1asm()
    - tested under Delphi 7, Delphi 2007 and Delphi 2009
    - a hashing limitation makes SynLZ sometimes unable to pack continuous
       blocks of same byte -> SynLZ is perfect for xml/text, but SynLZO
       is prefered for database files
    - if you include it in your application, please give me some credits:
       "use SynLZ compression by http://bouchez.info"
    - use at your own risk!

    Some benchmark on a Sempron computer:
    - compression is 20 times faster than zip, decompression 3 times
    - same compression ratio as lzo algo, but faster (up to 2x) on compression
    - the R and W intermediate speed are at the compressed stream level, i.e.
      the speed which used for disk usage -> you see that SynLZ behaves
      very well for real-time data compression, for backup purpose e.g.
      (a typical SATA disk drive has a speed of 50-70 MB/s)

 KLOG.xml 6034 bytes
 lz1 asm     1287 21.3% R  256 MB/s W   54 MB/s R   71 MB/s W 334 MB/s
 lz1 pas     1287 21.3% R  184 MB/s W   39 MB/s R   58 MB/s W 274 MB/s
 lz2 pas     1274 21.1% R  173 MB/s W   36 MB/s R   57 MB/s W 274 MB/s
   lzo C     1347 22.3% R  185 MB/s W   41 MB/s R  111 MB/s W 501 MB/s
     zip      806 13.4% R   14 MB/s W    1 MB/s R   14 MB/s W 110 MB/s

 MiniLZO.cs 25252 bytes
 lz1 asm     5775 22.9% R  246 MB/s W   56 MB/s R   70 MB/s W 306 MB/s
 lz1 pas     5775 22.9% R  178 MB/s W   40 MB/s R   58 MB/s W 253 MB/s
 lz2 pas     5762 22.8% R  166 MB/s W   37 MB/s R   57 MB/s W 250 MB/s
   lzo C     5846 23.2% R  164 MB/s W   38 MB/s R  103 MB/s W 448 MB/s
     zip     3707 14.7% R   15 MB/s W    2 MB/s R   22 MB/s W 154 MB/s

 TestLZO.exe 158720 bytes
 lz1 asm   110686 69.7% R  127 MB/s W   88 MB/s R   80 MB/s W 115 MB/s
 lz1 pas   110686 69.7% R   98 MB/s W   68 MB/s R   63 MB/s W  90 MB/s
 lz2 pas   109004 68.7% R   88 MB/s W   60 MB/s R   60 MB/s W  88 MB/s
   lzo C   108202 68.2% R   40 MB/s W   27 MB/s R  164 MB/s W 241 MB/s
     zip    88786 55.9% R    5 MB/s W    3 MB/s R   33 MB/s W  60 MB/s

 Browsing.sq3db 46047232 bytes (46MB)
 lz1 asm 19766884 42.9% R  171 MB/s W   73 MB/s R   73 MB/s W 171 MB/s
 lz1 pas 19766884 42.9% R  130 MB/s W   56 MB/s R   59 MB/s W 139 MB/s
 lz2 pas 19707346 42.8% R  123 MB/s W   52 MB/s R   59 MB/s W 139 MB/s
 lzo asm 20629084 44.8% R   89 MB/s W   40 MB/s R  135 MB/s W 302 MB/s
   lzo C 20629083 44.8% R   66 MB/s W   29 MB/s R  145 MB/s W 325 MB/s
     zip 15564126 33.8% R    6 MB/s W    2 MB/s R   30 MB/s W  91 MB/s

 TRKCHG.DBF 4572297 bytes (4MB)
 lz1 asm   265782 5.8% R  430 MB/s W   25 MB/s R   29 MB/s W 510 MB/s
 lz1 pas   265782 5.8% R  296 MB/s W   17 MB/s R   28 MB/s W 483 MB/s
 lz2 pas   274773 6.0% R  258 MB/s W   15 MB/s R   27 MB/s W 450 MB/s
   lzo C   266897 5.8% R  318 MB/s W   18 MB/s R   41 MB/s W 702 MB/s
     zip   158408 3.5% R   25 MB/s W    0 MB/s R   11 MB/s W 318 MB/s

 CATENA5.TXT 6358752 bytes
 lz1 asm  3275269 51.5% R  132 MB/s W   68 MB/s R   66 MB/s W 129 MB/s
 lz1 pas  3275269 51.5% R  103 MB/s W   53 MB/s R   57 MB/s W 112 MB/s
 lz2 pas  3277397 51.5% R   95 MB/s W   49 MB/s R   57 MB/s W 112 MB/s
   lzo C  3289373 51.7% R   63 MB/s W   33 MB/s R   90 MB/s W 175 MB/s
     zip  2029096 31.9% R    4 MB/s W    1 MB/s R   29 MB/s W  91 MB/s


  Benchmark update - introducing LZ4 at http://code.google.com/p/lz4

  190 MB file containing pascal sources, on a Core 2 duo PC:
   LZ4     compression = 1.25 sec, comp. size = 71 MB, decompression = 0.44 sec
   SynLZ   compression = 1.09 sec, comp. size = 63 MB, decompression = 0.99 sec
   zip (1) compression = 6.44 sec, comp. size = 52 MB, decompression = 1.49 sec
   zip (6) compression = 20.1 sec, comp. size = 42 MB, decompression = 1.35 sec

  Note: zip decompression here uses fast asm optimized version of SynZip.pas

  Decompression is slower in SynLZ, due to the algorithm used: it does recreate
   the hash table even at decompression, while it is not needed by LZ4.
  Having the hash table at hand allows more patterns to be available, so
   compression ratio is better, at the expand of a slower speed.

  Conclusion:
   SynLZ compresses better than LZ4,
   SynLZ is faster to compress than LZ4,
   but SynLZ is slower to decompress than LZ4,
   and SynLZ is still very competitive for our Client-Server mORMot purpose ;)


  Revision history

  Version 1.6
  - first release, associated with the main Synopse SQLite3 framework

  Version 1.13
  - code modifications to compile with Delphi 5 compiler
  - comment refactoring (mostly for inclusion in SynProject documentation)
  - new CompressSynLZ function, for THttpSocket.RegisterCompress - this
    function will return 'synlzo' as "ACCEPT-ENCODING:" HTTP header parameter

  Version 1.15
  - force ignore asm version of the code if PUREPASCAL conditional is defined

  Version 1.16
  - fixed potential GPF issue in Hash32() function

  Version 1.17
  - Use RawByteString type for CompressSynLZ() function prototype 

}

interface

{$I Synopse.inc}

/// get maximum possible (worse) compressed size for out_p
function SynLZcompressdestlen(in_len: integer): integer;

/// get uncompressed size from lz-compressed buffer (to reserve memory, e.g.)
function SynLZdecompressdestlen(in_p: PAnsiChar): integer;

/// 1st compression method uses hashing with a 32bits control word
function SynLZcompress1pas(src: PAnsiChar; size: integer; dst: PAnsiChar): integer;
/// 1st compression method uses hashing with a 32bits control word
function SynLZdecompress1pas(src: PAnsiChar; size: integer; dst: PAnsiChar): Integer;

{$ifndef PUREPASCAL}
/// optimized asm version of the 1st compression method
function SynLZcompress1asm(src: PAnsiChar; size: integer; dst: PAnsiChar): integer;
/// optimized asm version of the 1st compression method
function SynLZdecompress1asm(src: PAnsiChar; size: integer; dst: PAnsiChar): Integer;
{$endif PUREPASCAL}

/// 2nd compression method optimizes pattern copy -> a bit smaller, but slower
function SynLZcompress2(src: PAnsiChar; size: integer; dst: PAnsiChar): integer;
/// 2nd compression method optimizes pattern copy -> a bit smaller, but slower
function SynLZdecompress2(src: PAnsiChar; size: integer; dst: PAnsiChar): Integer;

{$ifndef UNICODE}
type
  /// define RawByteString, as it does exist in Delphi 2009 and up
  // - to be used for byte storage into an AnsiString
  RawByteString = AnsiString;
{$endif}

/// compress a data content using the SynLZ algorithm
// - as expected by THttpSocket.RegisterCompress
// - will return 'synlz' as ACCEPT-ENCODING: header parameter
// - will store a hash of both compressed and uncompressed stream: if the
// data is corrupted during transmission, will instantly return ''
function CompressSynLZ(var Data: RawByteString; Compress: boolean): RawByteString;


implementation

function SynLZcompressdestlen(in_len: integer): integer;
// get maximum possible (worse) compressed size for out_p
begin
  result := in_len+in_len shr 3+16; // worse case
end;

{$ifndef FPC}
type
  PtrUInt = {$ifdef CPUX64} NativeUInt {$else} cardinal {$endif};
{$endif}

{$ifndef CONDITIONALEXPRESSIONS}
type // Delphi 5 doesn't have those base types defined :(
  PByte = ^Byte;
  PWord = ^Word;
  PInteger = ^Integer;
  PCardinal = ^Cardinal;
  IntegerArray  = array[0..$effffff] of Integer;
  PIntegerArray = ^IntegerArray;
{$endif}

function SynLZdecompressdestlen(in_p: PAnsiChar): integer;
// get uncompressed size from lz-compressed buffer (to reserve memory, e.g.)
begin
  result := pWord(in_p)^;
  inc(in_p,2);
  if result and $8000<>0 then
    result := (result and $7fff) or (integer(pWord(in_p)^) shl 15);
end;

procedure movechars(s,d: PAnsiChar; t: integer);
// fast code for unaligned and overlapping (see {$define WT}) small blocks
// this code is sometimes used rather than system.Move() by decompress2()
var i: integer;
begin
  for i := 1 to t do begin
    d^ := s^;
    inc(d);
    inc(s);
  end;
end;

{$ifndef PUREPASCAL}
function SynLZcompress1asm(src: PAnsiChar; size: integer; dst: PAnsiChar): integer;
asm
        push    ebp
        push    ebx
        push    esi
        push    edi
        push    eax
        mov     eax, 8                                 
@@0906: add     esp, -4092
        push    eax                                    
        dec     eax
        jnz     @@0906                                 
        mov     eax, [esp+8000H]
        add     esp, -32
        mov     esi, ecx
        mov     [esp], edx
        mov     edi, eax
        mov     [esp+8H], esi
        mov     eax,[esp]
        cmp     eax, 32768
        jl      @@0889
        and     eax, 7FFFH
        or      eax, 8000H
        mov     [esi], ax
        mov     eax, [esp]
        shr     eax, 15
        mov     [esi+2], ax
        add     esi, 4
        jmp     @@0891
@@0890: mov     eax, 2
        jmp     @@0904
@@0889: mov     [esi], ax
        test    eax,eax
        jz     @@0890
        add     esi,2
@@0891: mov     eax, [esp]
        add     eax, edi
        mov     [esp+0CH], eax
        mov     eax, [esp+0CH]
        sub     eax, 11
        mov     [esp+10H], eax
        xor     ebx, ebx
        mov     eax, esi
        mov     [esp+18H], eax
        xor     edx, edx
        mov     [eax], edx                   
        add     esi, 4
        lea     eax, [esp+24H]                         
        xor     ecx, ecx                               
        mov     edx, 16384
        call    system.@fillchar
        // main loop:
        cmp     edi, [esp+10H]
        ja      @@0900
@@0892: mov     edx, [edi]
        mov     eax, edx
        shr     edx, 12                                
        xor     edx, eax                               
        and     edx, 0FFFH
        mov     ebp, [esp+edx*4+24H]
        mov     ecx, [esp+edx*4+4024H]
        mov     [esp+edx*4+24H], edi
        xor     ecx, eax
        mov     [esp+1CH], ecx
        test    ecx, 0FFFFFFH
        mov     [esp+edx*4+4024H], eax
        jnz     @@0897
        mov     eax, edi
        or      ebp,ebp
        jz      @@0897
        sub     eax, ebp
        cmp     eax, 2                                 
        mov     ecx, [esp+18H]
        jle     @@0897
        mov     eax,[ecx]
        lea     edi,[edi+2]
        bts     eax,ebx
        add     ebp, 2
        mov     [ecx],eax
        mov     ecx, [esp+0CH]
        mov     eax, 1
        sub     ecx, edi
        dec     ecx
        mov     [esp+20H], ecx
        cmp     ecx, 271
        jl      @@0894
        mov     dword ptr [esp+20H], 271
        jmp @@0894
@@0893: inc     eax
@@0894: mov     cl, [ebp+eax]
        cmp     cl, [edi+eax]
        jnz     @@0895
        cmp     eax, [esp+20H]
        jge     @@0895
@@1893: inc     eax
@@1894: mov     cl, [ebp+eax]
        cmp     cl, [edi+eax]
        jnz     @@0895
        cmp     eax, [esp+20H]
        jge     @@0895
@@2893: inc     eax
@@2894: mov     cl, [ebp+eax]
        cmp     cl, [edi+eax]
        jnz     @@0895
        cmp     eax, [esp+20H]
        jl      @@0893
@@0895: add     edi, eax
        shl     edx, 4
        cmp     eax, 15
        jg      @@0896
        or      eax, edx
        mov     word ptr [esi], ax
        add     esi, 2
        jmp     @@0898
@@0896: sub     eax, 16
        mov     [esi], dx
        mov     [esi+2H], al
        add     esi, 3
        jmp     @@0898
@@0897: mov     al, [edi]
        mov     [esi], al
        inc     edi
        inc     esi
@@0898: cmp     bl, 31
        jnc     @@0899
        cmp     edi, [esp+10H]
        lea     ebx,[ebx+1]
        jbe     @@0892
        jmp     @@0900
@@0899: mov     [esp+18H], esi
        xor     edx, edx
        mov     [esi], edx
        add     esi, 4
        xor     ebx, ebx
        cmp     edi, [esp+10H]
        jbe     @@0892
@@0900: cmp     edi, [esp+0CH]
        jnc     @@0903
@@0901: mov     al, [edi]
        mov     [esi], al
        inc     edi
        inc     esi
        cmp     bl, 31
        jnc     @@0902
        cmp     edi, [esp+0CH]
        lea     ebx,[ebx+1]
        jc      @@0901
        jmp     @@0903
@@0902: xor     ebx, ebx
        mov     [esi], ebx
        lea     esi,[esi+4]
        cmp     edi, [esp+0CH]
        jc      @@0901
@@0903: mov     eax, esi
        sub     eax, [esp+8H]
@@0904: add     esp, 32804
        pop     edi
        pop     esi
        pop     ebx
        pop     ebp
end;
{$endif PUREPASCAL}

type
  TByteArray = array[0..3] of byte;
  PByteArray = ^TByteArray;

function SynLZcompress1pas(src: PAnsiChar; size: integer; dst: PAnsiChar): integer;
var dst_beg, // initial dst value
    src_end,          // real last byte available in src
    src_endmatch,     // last byte to try for hashing
    o: pAnsiChar;
    CWbit: byte;
    CWpoint: PInteger;
    h, v, cached: integer;
    t, tmax: integer;
    offset: array[0..4095] of PAnsiChar; // 16KB+16KB=32KB hashing code
    cache: array[0..4095] of integer;
begin
  dst_beg := dst;
  // 1. store in_len
  if size>=$8000 then begin
    pWord(dst)^ := $8000 or (size and $7fff);
    pWord(dst+2)^ := size shr 15;
    inc(dst,4);
  end else begin
    pWord(dst)^ := size ; // src<32768 -> stored as word, otherwize as integer
    if size=0 then begin
      result := 2;
      exit;
    end;
    inc(dst,2);
  end;
  // 2. compress
  src_end := src+size;
  src_endmatch := src_end-(6+5);
  CWbit := 0;
  CWpoint := pointer(dst);
  pInteger(dst)^ := 0;
  inc(dst,sizeof(CWpoint));
  fillchar(offset,sizeof(offset),0); // fast 16KB reset to 0
  // 1. main loop to search using hash[]
  if src<=src_endmatch then
  repeat
    v := pInteger(src)^;
    h := ((v shr 12) xor v) and 4095;
    o := offset[h];
    offset[h] := src;
    cached := v xor cache[h];
    cache[h] := v;
    if (cached and $00ffffff=0) and (o<>nil) and (src-o>2) then begin
      CWpoint^ := CWpoint^ or (1 shl CWbit);
      inc(src,2);
      inc(o,2);
      t := 1;
      tmax := src_end-src-1;
      if tmax>=(255+16) then
        tmax := (255+16);
      while (o[t]=src[t]) and (t<tmax) do
        inc(t);
      inc(src,t);
      h := h shl 4;
      // here we have always t>0
      if t<=15 then begin // mark 2 to 17 bytes -> size=1..15
        pWord(dst)^ := integer(t or h);
        inc(dst,2);
      end else begin // mark 18 to (255+16) bytes -> size=0, next byte=t
        dec(t,16);
        pWord(dst)^ := h; // size=0
        dst[2] := ansichar(t);
        inc(dst,3);
      end;
    end else begin
      dst^ := src^;
      inc(src);
      inc(dst);
    end;
    if CWbit<31 then begin
      inc(CWbit);
      if src<=src_endmatch then continue else break;
    end else begin
      CWpoint := pointer(dst);
      pInteger(dst)^ := 0;
      inc(dst,sizeof(CWpoint));
      CWbit := 0;
      if src<=src_endmatch then continue else break;
    end;
  until false;
  // 2. store remaining bytes
  if src<src_end then
  repeat
    dst^ := src^;
    inc(src);
    inc(dst);
    if CWbit<31 then begin
      inc(CWbit);
      if src<src_end then continue else break;
    end else begin
      pInteger(dst)^ := 0;
      inc(dst,4);
      CWbit := 0;
      if src<src_end then continue else break;
    end;
  until false;
  result := dst-dst_beg;
end;

const
  bitlut: array[0..15] of integer =
    (4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0);

function SynLZdecompress1b(src: PAnsiChar; size: integer; dst: PAnsiChar): Integer;
// this routine was trying to improve speed, but was slower
var last_hashed: PAnsiChar; // initial src and dst value
    src_end: PAnsiChar;
    CWbit: integer;
    CW, v, t, h: integer;
    offset: array[0..4095] of PAnsiChar; // 16KB hashing code
label nextCW;
begin
//  src_beg := src;
//  dst_beg := dst;
  src_end := src+size;
  // 1. retrieve out_len
  result := pWord(src)^;
  if result=0 then exit;
  inc(src,2);
  if result and $8000<>0 then begin
    result := (result and $7fff) or (integer(pWord(src)^) shl 15);
    inc(src,2);
  end;
  // 2. decompress
  last_hashed := dst-1;
  CWbit := 32;
nextCW:
  CW := pInteger(src)^;
  inc(src,4);
  CWbit := CWbit-32;
  if src<src_end then
  repeat
    if CW and 1=0 then begin
      if CWbit<(32-4) then begin
        pInteger(dst)^ := pInteger(src)^;
        v := bitlut[CW and 15];
        inc(src,v);
        inc(dst,v);
        inc(CWbit,v);
        CW := CW shr v;
        if src>=src_end then break;
        while last_hashed<dst-3 do begin
          inc(last_hashed);
          v := pInteger(last_hashed)^;
          offset[((v shr 12) xor v) and 4095] := last_hashed;
        end;
      end else begin
        dst^ := src^;
        inc(src);
        inc(dst);
        if src>=src_end then break;
        if last_hashed<dst-3 then begin
          inc(last_hashed);
          v := pInteger(last_hashed)^;
          offset[((v shr 12) xor v) and 4095] := last_hashed;
        end;
        inc(CWbit);
        CW := CW shr 1;
        if CWbit<32 then
          continue else
          goto nextCW;
      end;
    end else begin
      h := pWord(src)^;
      inc(src,2);
      t := (h and 15)+2;
      h := h shr 4;
      if t=2 then begin
        t := ord(src^)+(16+2);
        inc(src);
      end;
      if dst-offset[h]<t then
        movechars(offset[h],dst,t) else
        move(offset[h]^,dst^,t);
      while last_hashed<dst do begin
        inc(last_hashed);
        v := pInteger(last_hashed)^;
        offset[((v shr 12) xor v) and 4095] := last_hashed;
      end;
      inc(dst,t);
      if src>=src_end then break;
      last_hashed := dst-1;
      inc(CWbit);
      CW := CW shr 1;
      if CWbit<32 then
        continue else
        goto nextCW;
    end;
  until false;
//  assert(result=dst-dst_beg);
end;

{$ifndef PUREPASCAL}
function SynLZdecompress1asm(src: PAnsiChar; size: integer; dst: PAnsiChar): Integer;
asm
        push    ebp
        push    ebx
        push    esi
        push    edi                                    
        push    eax
        mov     eax, 4                                 
@@0906: add     esp, -4092
        push    eax                                    
        dec     eax
        jnz     @@0906                                 
        mov     eax, [esp+4000H]             
        add     esp, -24                               
        mov     esi, ecx                               
        mov     ebx, eax
        mov     [esp+8H], esi                
        add     edx, ebx                               
        mov     [esp+10H], edx               
        movzx   eax, word ptr [ebx]                    
        mov     [esp], eax                   
        or      eax,eax                     
        je      @@0917                                 
        add     ebx, 2
        mov     eax, [esp]                   
        test    ah, 80H
        jz      @@0907
        and     eax, 7FFFH                             
        movzx   edx, word ptr [ebx]                    
        shl     edx, 15
        or      eax, edx                               
        mov     [esp], eax                   
        add     ebx, 2
@@0907: lea     ebp, [esi-1]
@@0908: mov     ecx, [ebx]
        add     ebx, 4
        mov     [esp+14H], ecx
        cmp     ebx, [esp+10H]
        mov     edi, 1
        jnc     @@0917
@@0909: mov     ecx, [esp+14H]
@@090A: test    ecx, edi
        jnz     @@0911
        mov     al, byte ptr [ebx]
        inc     ebx
        mov     byte ptr [esi], al
        inc     esi
        cmp     ebx, [esp+10H]
        lea     eax, [esi-3]
        jnc     @@0917
        cmp     eax, ebp
        jbe     @@0910
        inc     ebp
        mov     eax, [ebp]
        mov     edx, eax
        shr     eax, 12
        xor     eax, edx
        and     eax, 0FFFH
        mov     [esp+eax*4+1CH], ebp
@@0910: add     edi, edi
        jnz     @@090A
        jmp     @@0908

@@0911: movzx   edx, word ptr [ebx]
        add     ebx, 2
        mov     eax, edx
        and     edx, 0FH
        add     edx, 2
        shr     eax, 4
        cmp     edx,2
        jnz     @@0912
        movzx   edx, byte ptr [ebx]
        inc     ebx
        add     edx, 18
@@0912: mov     eax, [esp+eax*4+1CH]
        mov     ecx, esi
        mov     [esp+18H], edx
        sub     ecx, eax
        cmp     ecx, edx
        jl     @@0913
        mov     ecx, edx
        mov     edx, esi
        call    move
@@0914: cmp     esi, ebp
        jbe     @@0916
@@0915: inc     ebp
        mov     edx, [ebp]
        mov     eax, edx
        shr     edx, 12
        xor     eax, edx
        and     eax, 0FFFH
        cmp     esi, ebp
        mov     [esp+eax*4+1CH], ebp
        ja      @@0915
@@0916: add     esi, [esp+18H]
        cmp     ebx, [esp+10H]
        jnc     @@0917
        add     edi, edi
        lea     ebp, [esi-1]
        jz      @@0908
        jmp     @@0909
@@0913: mov     ecx, edx
        mov     edx, esi
        call    movechars
        jmp     @@0914

@@0917: mov     eax, [esp]
        add     esp, 16412
        pop     edi
        pop     esi
        pop     ebx
        pop     ebp
end;
{$endif PUREPASCAL}

function SynLZdecompress1pas(src: PAnsiChar; size: integer; dst: PAnsiChar): Integer;
var last_hashed: PAnsiChar; // initial src and dst value
    src_end: PAnsiChar;
    CWbit: integer;
    CW, v, t, h: integer;
    offset: array[0..4095] of PAnsiChar; // 16KB hashing code
label nextCW;
begin
  src_end := src+size;
  // 1. retrieve out_len
  result := pWord(src)^;
  if result=0 then exit;
  inc(src,2);
  if result and $8000<>0 then begin
    result := (result and $7fff) or (integer(pWord(src)^) shl 15);
    inc(src,2);
  end;
  // 2. decompress
  last_hashed := dst-1;
nextCW:
  CW := pInteger(src)^;
  inc(src,4);
  CWbit := 1;
  if src<src_end then
  repeat
    if CW and CWbit=0 then begin
      dst^ := src^;
      inc(src);
      inc(dst);
      if src>=src_end then break;
      if last_hashed<dst-3 then begin
        inc(last_hashed);
        v := pInteger(last_hashed)^;
        offset[((v shr 12) xor v) and 4095] := last_hashed;
      end;
      CWbit := CWbit shl 1;
      if CWbit<>0 then
        continue else
        goto nextCW;
    end else begin
      h := pWord(src)^;
      inc(src,2);
      t := (h and 15)+2;
      h := h shr 4;
      if t=2 then begin
        t := ord(src^)+(16+2);
        inc(src);
      end;
      if dst-offset[h]<t then // avoid overlaping move() bug
        movechars(offset[h],dst,t) else
        move(offset[h]^,dst^,t);
      while last_hashed<dst do begin
        inc(last_hashed);
        v := pInteger(last_hashed)^;
        offset[((v shr 12) xor v) and 4095] := last_hashed;
      end;
      inc(dst,t);
      if src>=src_end then break;
      last_hashed := dst-1;
      CWbit := CWbit shl 1;
      if CWbit<>0 then
        continue else
        goto nextCW;
    end;
  until false;
end;


function SynLZcompress2(src: PAnsiChar; size: integer; dst: PAnsiChar): integer;
var dst_beg,      // initial dst value
    src_end,      // real last byte available in src
    src_endmatch, // last byte to try for hashing
    o: pAnsiChar;
    CWbit: byte;
    CWpoint: PInteger;
    h, v, cached: integer;
    t, tmax, tdiff, i: integer;
    offset: array[0..4095] of PAnsiChar; // 16KB+16KB=32KB hashing code
    cache: array[0..4095] of integer;
    label dotdiff;
begin
  dst_beg := dst;
  // 1. store in_len
  if size>=$8000 then begin
    pWord(dst)^ := $8000 or (size and $7fff);
    pWord(dst+2)^ := size shr 15;
    inc(dst,4);
  end else begin
    pWord(dst)^ := size ; // src<32768 -> stored as word, otherwize as integer
    if size=0 then begin
      result := 2;
      exit;
    end;
    inc(dst,2);
  end;
  // 2. compress
  src_end := src+size;
  src_endmatch := src_end-(6+5);
  CWbit := 0;
  CWpoint := pointer(dst);
  pInteger(dst)^ := 0;
  inc(dst,sizeof(CWpoint));
  tdiff := 0;
  fillchar(offset,sizeof(offset),0); // fast 16KB reset to 0
  // 1. main loop to search using hash[]
  if src<=src_endmatch then
  repeat
    v := pInteger(src)^;
    h := ((v shr 12) xor v) and 4095;
    o := offset[h];
    offset[h] := src;
    cached := v xor cache[h];
    cache[h] := v;
    if (cached and $00ffffff=0) and (o<>nil) and (src-o>2) then begin
//      SetBit(CWpoint,CWbit);
//      asm movzx eax,byte ptr CWbit; bts [CWpoint],eax; end
      if tdiff<>0 then begin
        dec(src,tdiff);
dotdiff:v := tdiff;
        if v<=8 then begin
          if CWBit+v>31 then begin
            for i := CWBit to 31 do begin
              dst^ := src^;
              inc(dst);
              inc(src);
            end;
            CWpoint := pointer(dst);
            pInteger(dst)^ := 0;
            inc(dst,4);
            CWBit := (CWBit+v) and 31;
            for i := 1 to CWBit do begin
              dst^ := src^;
              inc(dst);
              inc(src);
            end;
          end else begin
            inc(CWBit,v);
            for i := 1 to v do begin
              dst^ := src^;
              inc(dst);
              inc(src);
            end;
          end;
        end else begin
          CWpoint^ := CWpoint^ or (1 shl CWbit);
          dec(v,9);
          if v>15 then begin
            v := 15; // v=9..24 -> h=0..15
            dst^ := #$ff; // size=15 -> tdiff
          end else
            dst^ := ansichar((v shl 4) or 15); // size=15 -> tdiff
          inc(dst);
          pInt64(dst)^ := pInt64(src)^;
          inc(dst,8);
          inc(src,8);
          for i := 1 to v+1 do begin
            dst^ := src^;
            inc(dst);
            inc(src);
          end;
          if CWBit<31 then
            inc(CWBit) else begin
            CWpoint := pointer(dst);
            pInteger(dst)^ := 0;
            inc(dst,4);
            CWbit := 0;
          end;
          dec(tdiff,24);
          if tdiff>0 then
            goto dotdiff;
        end;
      end;
//      assert(pWord(o)^=pWord(src)^);
      tdiff := 0;
      CWpoint^ := CWpoint^ or (1 shl CWbit);
      inc(src,2);
      inc(o,2);
      t := 0; // t=matchlen-2
      tmax := src_end-src;
      if tmax>=(255+15) then
        tmax := (255+15);
      while (o[t]=src[t]) and (t<tmax) do
        inc(t);
      inc(src,t);
      h := h shl 4;
//      assert(t>0);
      // here we have always t>0
      if t<15 then begin // store t=1..14 -> size=t=1..14
        pWord(dst)^ := integer(t or h);
        inc(dst,2);
      end else begin // store t=15..255+15 -> size=0, next byte=matchlen-15-2
        dst[2] := ansichar(t-15);
        pWord(dst)^ := h; // size=0
        inc(dst,3);
      end;
      if CWbit<31 then begin
        inc(CWbit);
        if src<=src_endmatch then continue else break;
      end else begin
        CWpoint := pointer(dst);
        pInteger(dst)^ := 0;
        inc(dst,4);
        CWbit := 0;
        if src<=src_endmatch then continue else break;
      end;
    end else begin
      inc(src);
      inc(tdiff);
      if src<=src_endmatch then continue else break;
    end;
  until false;
  // 2. store remaining bytes
  dec(src,tdiff); // force store trailing bytes
  if src<src_end then
  repeat
    dst^ := src^;
    inc(src);
    inc(dst);
    if CWbit<31 then begin
      inc(CWbit);
      if src<src_end then continue else break;
    end else begin
      pInteger(dst)^ := 0;
      inc(dst,4);
      CWbit := 0;
      if src<src_end then continue else break;
    end;
  until false;
  result := dst-dst_beg;
end;

function SynLZdecompress2(src: PAnsiChar; size: integer; dst: PAnsiChar): Integer;
var dst_beg, last_hashed: PAnsiChar; // initial src and dst value
    src_end: PAnsiChar;
    CWbit: integer;
    CW, v, t, h, i: integer;
    offset: array[0..4095] of PAnsiChar; // 16KB hashing code
label nextCW;
begin
  dst_beg := dst;
  src_end := src+size;
  {$ifndef CPU64}
  t := 0; // make compiler happy
  {$endif}
  // 1. retrieve out_len
  result := pWord(src)^;
  if result=0 then exit;
  inc(src,2);
  if result and $8000<>0 then begin
    result := (result and $7fff) or (integer(pWord(src)^) shl 15);
    inc(src,2);
  end;
  // 2. decompress
  last_hashed := dst-1;
nextCW:
  CW := pInteger(src)^;
  inc(src,4);
  CWbit := 1;
  if src<src_end then
  repeat
    if CW and CWbit=0 then begin
      dst^ := src^;
      inc(src);
      inc(dst);
      if src>=src_end then break;
      if last_hashed<dst-3 then begin
        inc(last_hashed);
        v := pInteger(last_hashed)^;
        offset[((v shr 12) xor v) and 4095] := last_hashed;
      end;
      CWbit := CWbit shl 1;
      if CWbit<>0 then
        continue else
        goto nextCW;
    end else begin
      case ord(src^) and 15 of // get size
      0: begin // size=0 -> next byte=matchlen-15-2
        h := pWord(src)^ shr 4;
        t := ord(src[2])+(15+2);
        inc(src,3);
        if dst-offset[h]<t then
          movechars(offset[h],dst,t) else
          move(offset[h]^,dst^,t);
      end;
      15: begin
        for i := 1 to ord(src^) shr 4+9 do begin // size=15 -> tdiff
          inc(src);
          dst^ := src^;
          inc(dst);
        end;
        inc(src);
        if src>=src_end then break;
        while last_hashed<dst-3 do begin
          inc(last_hashed);
          v := pInteger(last_hashed)^;
          offset[((v shr 12) xor v) and 4095] := last_hashed;
        end;
        CWbit := CWbit shl 1;
        if CWbit<>0 then
          continue else
          goto nextCW;
      end;
      else begin // size=1..14=matchlen-2
        h := pWord(src)^;
        inc(src,2);
        t := (h and 15)+2;
        h := h shr 4;
        if dst-offset[h]<t then
          movechars(offset[h],dst,t) else
          move(offset[h]^,dst^,t);
      end;
      end;
      while last_hashed<dst do begin
        inc(last_hashed);
        v := pInteger(last_hashed)^;
        offset[((v shr 12) xor v) and 4095] := last_hashed;
      end;
      inc(dst,t);
      if src>=src_end then break;
      last_hashed := dst-1;
      CWbit := CWbit shl 1;
      if CWbit<>0 then
        continue else
        goto nextCW;
    end;
  until false;
  assert(result=dst-dst_beg);
end;

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
      inc(PtrUInt(P),16);
    end;
    for i := 1 to (L shr 2)and 3 do begin // 4 bytes (DWORD) by loop
      inc(s1,P^[0]);
      inc(s2,s1);
      inc(PtrUInt(P),4);
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

function CompressSynLZ(var Data: RawByteString; Compress: boolean): RawByteString;
var DataLen, len: integer;
    P: PAnsiChar;
begin
  DataLen := length(Data);
  if DataLen<>0 then // '' is compressed and uncompressed to ''
  if Compress then begin
    len := SynLZcompressdestlen(DataLen)+8;
    SetString(result,nil,len);
    P := pointer(result);
    PCardinal(P)^ := Hash32(pointer(Data),DataLen);
{$ifdef PUREPASCAL}
    len := SynLZcompress1pas(pointer(Data),DataLen,P+8); {$else}
    len := SynLZcompress1asm(pointer(Data),DataLen,P+8);
{$endif}
    PCardinal(P+4)^ := Hash32(pointer(P+8),len);
    SetString(Data,P,len+8);
  end else begin
    result := '';
    P := pointer(Data);
    if (DataLen<=8) or (Hash32(pointer(P+8),DataLen-8)<>PCardinal(P+4)^) then
      exit;
    len := SynLZdecompressdestlen(P+8);
    SetLength(result,len);
    if (len<>0) and
{$ifdef PUREPASCAL}
        ((SynLZdecompress1pas(P+8,DataLen-8,pointer(result))<>len) or
{$else} ((SynLZdecompress1asm(P+8,DataLen-8,pointer(result))<>len) or
{$endif}
       (Hash32(pointer(result),len)<>PCardinal(P)^)) then begin
      result := '';
      exit;
    end else 
      SetString(Data,PAnsiChar(pointer(result)),len);
  end;
  result := 'synlz';
end;


end.
