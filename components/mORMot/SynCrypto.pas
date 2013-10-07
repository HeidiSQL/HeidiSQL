/// fast cryptographic routines (hashing and cypher)
// - implements AES, XOR, ADLER32, MD5, SHA1, SHA256 algorithms
// - optimized for speed (tuned assembler and VIA PADLOCK optional support)
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.17
unit SynCrypto;

(*
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2012 Arnaud Bouchez
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

  The Original Code is Synopse SQLite3/mORMot database framework.

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


      Synopse Cryptographic routines
      ==============================

    - fastest ever 100% Delphi (and asm ;) code
    - AES Crypto(128,192,256 bits key) with optimized asm version
    - XOR Crypto (32 bits key) - very fast with variable or fixed key
    - ADLER32 - 32 bits fast Hash with optimized asm version
    - MD5 - standard fast 128 bits Hash
    - SHA1 - 160 bits Secure Hash
    - SHA256 - 256 bits Secure Hash with optimized asm version
    - VIA PADLOCK optional support - native .o code on linux or .dll (Win32)
     (tested on a Dedibox C7 (rev1) linux server - need validation for Win32)
    - AES >512KB buffers: use multi-threaded code for multi-core CPU

    Source code licenced under the MPL:
      see http://www.mozilla.org/MPL/MPL-1.1.html


    Benchmark on my AMD-64 TL-56 dualcore-CPU:
    ==========================================
    Testing with blocks of 16KB each
           crc32  624 MB/s
     adler32 pas  571 MB/s              asm 1304 MB/s
             MD5  176 MB/s
            SHA1  101 MB/s
          SHA256   63 MB/s
   AES128 cypher   84 MB/s         uncypher   81 MB/s  asm version
   AES128 cypher   57 MB/s         uncypher   57 MB/s  pascal version
   AES192 cypher   72 MB/s         uncypher   70 MB/s  asm version
   AES192 cypher   48 MB/s         uncypher   48 MB/s  pascal version
   AES256 cypher   62 MB/s         uncypher   61 MB/s  asm version
   AES256 cypher   42 MB/s         uncypher   42 MB/s  pascal version
        XorBlock 3463 MB/s  (very fast, since with 16KB data remain in L2 cache)
       XorOffset 3425 MB/s
        XorConst 5940 MB/s  (even faster, since no table used -> all in L1 cache)

    Testing with blocks of 1024KB each (for AES: block >512KB -> uses dualcore)
           crc32  577 MB/s
     adler32 pas  529 MB/s              asm 1003 MB/s
             MD5  176 MB/s
            SHA1  100 MB/s
          SHA256   63 MB/s
   AES128 cypher  129 MB/s         uncypher  130 MB/s  asm version
   AES128 cypher   96 MB/s         uncypher   95 MB/s  pascal version
   AES192 cypher  107 MB/s         uncypher  114 MB/s  asm version
   AES192 cypher   83 MB/s         uncypher   85 MB/s  pascal version
   AES256 cypher   98 MB/s         uncypher  105 MB/s  asm version
   AES256 cypher   76 MB/s         uncypher   76 MB/s  pascal version
        XorBlock 1423 MB/s   (we reach the memory control bandwidth)
       XorOffset 1325 MB/s
        XorConst 1506 MB/s

    Testing with blocks of 4096KB each (for AES: block >512KB -> uses dualcore)
           crc32  578 MB/s
     adler32 pas  525 MB/s              asm  984 MB/s
             MD5  175 MB/s
            SHA1  100 MB/s
          SHA256   63 MB/s
   AES128 cypher  159 MB/s         uncypher  147 MB/s  asm version
   AES128 cypher  107 MB/s         uncypher  109 MB/s  pascal version
   AES192 cypher  134 MB/s         uncypher  128 MB/s  asm version
   AES192 cypher   90 MB/s         uncypher   92 MB/s  pascal version
   AES256 cypher  118 MB/s         uncypher  113 MB/s  asm version
   AES256 cypher   80 MB/s         uncypher   81 MB/s  pascal version
        XorBlock 1385 MB/s
       XorOffset 1292 MB/s
        XorConst 1479 MB/s

   Benchmark on a C7 Dedibox (USEPADLOCK version):
   ===============================================
   Testing with blocks of 16KB each
           crc32  402 MB/s
     adler32 pas  274 MB/s        asm  542 MB/s       libz.so 414 MB/s
             MD5  126 MB/s
            SHA1  480 MB/s
          SHA256  458 MB/s
   AES128 cypher 1566 MB/s         uncypher 1560 MB/s
   AES192 cypher 1421 MB/s         uncypher 1422 MB/s
   AES256 cypher 1237 MB/s         uncypher 1247 MB/s
        XorBlock 2336 MB/s
       XorOffset 1807 MB/s
        XorConst 3154 MB/s

    Testing with blocks of 1024KB each
           crc32  352 MB/s
     adler32 pas  256 MB/s         asm  395 MB/s      libz.so 361 MB/s
             MD5  123 MB/s
            SHA1  324 MB/s
          SHA256  324 MB/s
   AES128 cypher  552 MB/s         uncypher  552 MB/s
   AES192 cypher  552 MB/s         uncypher  552 MB/s
   AES256 cypher  552 MB/s         uncypher  552 MB/s
        XorBlock  354 MB/s
       XorOffset  373 MB/s
        XorConst  511 MB/s

    Testing with blocks of 4096KB each
           crc32  352 MB/s
     adler32 pas  255 MB/s         asm  395 MB/s      libz.so 361 MB/s
             MD5  124 MB/s
            SHA1  324 MB/s
          SHA256  326 MB/s
   AES128 cypher  552 MB/s         uncypher  552 MB/s
   AES192 cypher  552 MB/s         uncypher  552 MB/s
   AES256 cypher  552 MB/s         uncypher  552 MB/s
        XorBlock  352 MB/s
       XorOffset  368 MB/s
        XorConst  510 MB/s

   Conclusion:
   - USETHREADSFORBIGAESBLOCKS will help on modern multi-threaded CPU
   - AES speed: W.Ehrhardt's pascal is 55MB/s, A.Bouchez's asm is 84MB/s
   - AES-256 is faster than a simple XOR() on a dedibox with a C7 cpu ;)

   Initial version (C) 2008-2009 Arnaud Bouchez http://bouchez.info

   Revision History:

   Version 1.0
    - initial release on Internet, with MyCrypto unit name

   Version 1.1
    - updated release, with new optimized AES i386 assembler implementation
      and no FastCode dependency (CpuCount is taken from Windows API)

   Version 1.4 - February 8, 2010
   - whole Synopse SQLite3 database framework released under the GNU Lesser
     General Public License version 3, instead of generic "Public Domain"

   Version 1.8
   - mostly code review for Delphi 2009/2010 integration (unit uses now
     SynCommons string types definitions)

   Version 1.9
   - now use direct Windows threads, since we don't need any exception handling
     nor memory usage inside the AES encryption Thread handler
     -> avoid classes.TThread and system.BeginThread() use
     -> application is still "officialy" mono-threaded (i.e. IsMultiThread=false),
     for faster System.pas and FastMM4 (prevent CPU locking - see
     http://synopse.info/forum/viewtopic.php?id=57 about Delphi & multi-core)
   - some other minor fixes and enhancements

   Version 1.10
   - code modifications to compile with Delphi 6 compiler

   Version 1.13
   - code modifications to compile with Delphi 5 compiler

   Version 1.15
   - unit now tested with Delphi XE2 (32 Bit)

   Version 1.16
   - added TAESECB, TAESCBC, TAESCFB, TAESOFB and TAESCTR classes to handle AES
     encryption of memory buffers in ECB, CBC, CFB, OFB and CTR mode (including
     PKCS7 padding)
   - added pure pascal version (for XE2 64 compilation) of all algorithms

*)

interface

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

{.$define USEPADLOCK}

{$ifdef Linux} // padlock is dedibox linux tested only, but may be OK on Windows
  {$undef USETHREADSFORBIGAESBLOCKS}
  {$define USEPADLOCK}
{$else}
  // on Windows: will use Threads for very big blocks (>512KB) if multi-CPU
  {$define USETHREADSFORBIGAESBLOCKS}
{$endif}

{$ifdef USEPADLOCK}
{$ifdef MSWINDOWS}
  {$define USEPADLOCKDLL}   // Win32: we can use LibPadlock.dll
{$else}
  {.$define PADLOCKDEBUG}   // display message before using padlock
  {.$define USEPADLOCKDLL}  // Linux: use fast .o linked code
{$endif}
{$endif}

uses
{$ifdef MSWINDOWS}
  Windows,
{$endif}
  SysUtils,
{$ifndef LVCL}
  {$ifdef CONDITIONALEXPRESSIONS}
  RtlConsts,
  {$endif}
{$endif}
  Classes,
  SynCommons;


const
  /// hide all AES Context complex code
  AESContextSize = 278+sizeof(pointer);
  /// hide all SHA Context complex code
  SHAContextSize = 108;
  /// standard AES block size during cypher/uncypher
  AESBlockSize = 16;
  /// maximum AES key size
  AESKeySize = 256 div 8;

type
  PAESBlock = ^TAESBlock;

  /// 128 bits memory block for AES data cypher/uncypher
  TAESBlock = packed array[0..AESBlockSize-1] of byte;

  /// 256 bits memory block for maximum AES key storage
  TAESKey = packed array[0..AESKeySize-1] of byte;

  PAES = ^TAES;
  /// handle AES cypher/uncypher
  // - this is the default Electronic codebook (ECB) mode
  TAES = object
  private
    Context: packed array[1..AESContextSize] of byte;
{$ifdef USEPADLOCK}
    function DoPadlockInit(const Key; KeySize: cardinal): boolean;
{$endif}
  public
    /// true if the context was initialized
    Initialized: boolean;
    /// Initialize AES contexts for cypher
    // - first method to call before using this class
    // - KeySize is in bits, i.e. 128,192,256
    function EncryptInit(const Key; KeySize: cardinal): boolean;
    /// encrypt an AES data block
    procedure Encrypt(var B: TAESBlock); overload;
    /// encrypt an AES data block into another data block
    procedure Encrypt(const BI: TAESBlock; var BO: TAESBlock); overload;

    /// Initialize AES contexts for uncypher
    function DecryptInit(const Key; KeySize: cardinal): boolean;
    /// decrypt an AES data block
    procedure Decrypt(var B: TAESBlock); overload;
    /// decrypt an AES data block into another data block
    procedure Decrypt(const BI: TAESBlock; var BO: TAESBlock); overload;

    /// Finalize AES contexts for both cypher and uncypher
    // - only used with Padlock
    procedure Done;

    /// generic initialization method for AES contexts
    // - call either EncryptInit() either DecryptInit() method
    function DoInit(const Key; KeySize: cardinal; doEncrypt: boolean): boolean;
    /// perform the AES cypher or uncypher to continuous memory blocks
    // - call either Encrypt() either Decrypt() method
    procedure DoBlocks(pIn, pOut: PAESBlock; out oIn, oOut: PAESBLock; Count: integer; doEncrypt: boolean); overload;
    /// perform the AES cypher or uncypher to continuous memory blocks
    // - call either Encrypt() either Decrypt() method
    procedure DoBlocks(pIn, pOut: PAESBlock; Count: integer; doEncrypt: boolean); overload;
{$ifdef USETHREADSFORBIGAESBLOCKS}
    /// perform the AES cypher or uncypher to continuous memory blocks
    // - this special method will use Threads for bigs blocks (>512KB) if multi-CPU
    // - call either Encrypt() either Decrypt() method
    procedure DoBlocksThread(var bIn, bOut: PAESBlock; Count: integer; doEncrypt: boolean);
{$endif}
  end;

  TAESAbstractClass = class of TAESAbstract;
  
  /// handle AES cypher/uncypher with chaining
  // - use any of the inherited implementation, accorsponding to the chaining
  // mode required - TAESECB, TAESCBC, TAESCFB, TAESOFB and TAESCTR classes to
  // handle in ECB, CBC, CFB, OFB and CTR mode (including PKCS7 padding)
  TAESAbstract = class
  protected
    fIn, fOut: PAESBlock;
    CV: TAESBlock;
    AES: TAES;
    fCount: Cardinal;
    fKey: TAESKey;
    fKeySize: cardinal;
    fKeySizeBytes: cardinal;
    procedure EncryptInit;
    procedure DecryptInit;
    procedure EncryptTrailer;
  public
    /// Initialize AES contexts for cypher
    // - first method to call before using this class
    // - KeySize is in bits, i.e. 128,192,256
    // - IV is the Initialization Vector
    constructor Create(const aKey; aKeySize: cardinal; const aIV: TAESBlock); virtual;
    /// perform the AES cypher in the corresponding mode
    // - this abstract method will set CV from AES.Context, and fIn/fOut
    // from BufIn/BufOut 
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); virtual;
    /// perform the AES un-cypher in the corresponding mode
    // - this abstract method will set CV from AES.Context, and fIn/fOut
    // from BufIn/BufOut
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); virtual;
    /// encrypt a memory buffer using a PKCS7 padding pattern
    // - PKCS7 is described in RFC 5652 - it will add up to 16 bytes to
    // the input buffer
    function EncryptPKCS7(const Input: RawByteString): RawByteString;
    /// decrypt a memory buffer using a PKCS7 padding pattern
    // - PKCS7 is described in RFC 5652 - it will trim up to 16 bytes from
    // the input buffer
    function DecryptPKCS7(const Input: RawByteString): RawByteString;
    /// associated Key data
    property Key: TAESKey read fKey;
    /// associated Key Size, in bits (i.e. 128,192,256)
    property KeySize: cardinal read fKeySize;
  end;

  /// handle AES cypher/uncypher without chaining (ECB)
  // - this mode is known to be less secure than the others
  // - IV value set on constructor is used to code the trailing bytes
  // of the buffer (by a simple XOR)
  TAESECB = class(TAESAbstract)
  public
    /// perform the AES cypher in the ECB mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the ECB mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// handle AES cypher/uncypher with Cipher-block chaining (CBC)
  TAESCBC = class(TAESAbstract)
  public
    /// perform the AES cypher in the CBC mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the CBC mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// handle AES cypher/uncypher with Cipher feedback (CFB)
  TAESCFB = class(TAESAbstract)
  public
    /// perform the AES cypher in the CFB mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the CFB mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// handle AES cypher/uncypher with Output feedback (OFB)
  TAESOFB = class(TAESAbstract)
  public
    /// perform the AES cypher in the OFB mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the OFB mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// handle AES cypher/uncypher with Counter mode (CTR)
  TAESCTR = class(TAESAbstract)
  public
    /// perform the AES cypher in the CTR mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the CTR mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  PSHA1Digest = ^TSHA1Digest;
  /// 160 bits memory block for SHA1 hash digest storage
  TSHA1Digest   = packed array[0..19] of byte;

  PSHA1 = ^TSHA1;
  /// handle SHA1 hashing
  TSHA1 = object
  private
    Context: packed array[1..SHAContextSize div 4] of cardinal;
    procedure Compress; // used by Update and Final
  public
    /// initialize SHA1 context for hashing
    procedure Init;
    /// update the SHA1 context with some data
    procedure Update(Buffer: pointer; Len: integer);
    /// finalize and compute the resulting SHA1 hash Digest of all data
    // affected to Update() method
    procedure Final(out Digest: TSHA1Digest);
    /// one method to rule them all
    // - call Init, then Update(), then Final()
    // - only Full() is Padlock-implemented - use this rather than Update()
    procedure Full(Buffer: pointer; Len: integer; out Digest: TSHA1Digest);
  end;

  PSHA256Digest = ^TSHA256Digest;
  /// 256 bits memory block for SHA256 hash digest storage
  TSHA256Digest   = packed array[0..31] of byte;

  PSHA256 = ^TSHA256;
  /// handle SHA256 hashing
  TSHA256 = object
  private
    Context: packed array[1..SHAContextSize] of byte;
    procedure Compress; // used by Update and Final
  public
    /// initialize SHA256 context for hashing
    procedure Init;
    /// update the SHA256 context with some data
    procedure Update(Buffer: pointer; Len: integer);
    /// finalize and compute the resulting SHA256 hash Digest of all data
    // affected to Update() method
    procedure Final(out Digest: TSHA256Digest);
    /// one method to rule them all
    // - call Init, then Update(), then Final()
    // - only Full() is Padlock-implemented - use this rather than Update()
    procedure Full(Buffer: pointer; Len: integer; out Digest: TSHA256Digest);
  end;

  TMD5In = array[0..15] of cardinal;
  /// 128 bits memory block for MD5 hash digest storage
  TMD5Digest = array[0..15] of Byte;
  PMD5 = ^TMD5;
  TMD5Buf = array[0..3] of cardinal;
  /// handle MD5 hashing
  TMD5 = object
  private
    buf: TMD5Buf;
    bytes: array[0..1] of cardinal;
    in_: TMD5In;
  public
    /// initialize MD5 context for hashing
    procedure Init;
    /// update the MD5 context with some data
    procedure Update(const buffer; Len: cardinal);
    /// finalize and compute the resulting MD5 hash Digest of all data
    // affected to Update() method
    function Final: TMD5Digest;
    /// one method to rule them all
    // - call Init, then Update(), then Final()
    procedure Full(Buffer: pointer; Len: integer; out Digest: TMD5Digest);
  end;

{$A-} { packed memory structure }
  /// internal header for storing our AES data with salt and CRC
  TAESFullHeader = object
    /// Len before compression (if any)
    OriginalLen,
    /// Len before AES encoding
    SourceLen,
    /// Random Salt for better encryption
    SomeSalt,
    /// CRC from header
    HeaderCheck: cardinal;
    function Calc(const Key; KeySize: cardinal): cardinal;
  end;
{$A+}

  PAESFull = ^TAESFull;
  /// AES and XOR encryption object for easy direct memory or stream access
  // - calls internaly TAES objet methods, and handle memory and streams for best speed
  // - a TAESFullHeader is encrypted at the begining, allowing fast Key validation,
  // but the resulting stream is not compatible with raw TAES object
  TAESFull = object
  public
    /// header, stored at the beginning of struct -> 16-byte aligned
    Head: TAESFullHeader;
    /// this memory stream is used in case of EncodeDecode(outStream=bOut=nil)
    // method call
    outStreamCreated: TMemoryStream;
    /// main method of AES or XOR cypher/uncypher
    // - return out size, -1 if error on decoding (Key not correct)
    // - valid KeySize: 0=nothing, 32=xor, 128,192,256=AES
    // - if outStream is TMemoryStream -> auto-reserve space (no Realloc:)
    // - for normal usage, you just have to Assign one In and one Out
    // - if outStream AND bOut are both nil, an outStream is created via
    // THeapMemoryStream.Create
    // - if Padlock is used, 16-byte alignment is forced (via tmp buffer if necessary)
    // - if Encrypt -> OriginalLen can be used to store unCompressed Len
    function EncodeDecode(const Key; KeySize, inLen: cardinal; Encrypt: boolean;
      inStream, outStream: TStream; bIn, bOut: pointer; OriginalLen: cardinal=0): integer;
  end;

  /// AES encryption stream
  // - encrypt the Data on the fly, in a compatible way with AES() - last bytes
  // are coded with XOR (not compatible with TAESFull format)
  // - not optimized for small blocks -> ok if used AFTER TBZCompressor/TZipCompressor
  // - warning: Write() will crypt Buffer memory in place -> use AFTER T*Compressor
  TAESWriteStream = class(TStream)
  public
    Adler, // CRC from uncrypted compressed data - for Key check
    DestSize: cardinal;
  private
    Dest: TStream;
    Buf: TAESBlock; // very small buffer for remainging 0..15 bytes
    BufCount: integer; // number of pending bytes (0..15) in Buf
    AES: TAES;
    NoCrypt: boolean; // if KeySize=0
  public
    /// initialize the AES encryption stream for an output stream (e.g.
    // a TMemoryStream or a TFileStream)
    constructor Create(outStream: TStream; const Key; KeySize: cardinal);
    /// finalize the AES encryption stream
    // - internaly call the Finish method
    destructor Destroy; override;
    /// read some data is not allowed -> this method will raise an exception on call
    function Read(var Buffer; Count: Longint): Longint; override;
    /// append some data to the outStream, after encryption
    function Write(const Buffer; Count: Longint): Longint; override;
    /// read some data is not allowed -> this method will raise an exception on call
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    /// write pending data
    // - should always be called before closeing the outStream (some data may
    // still be in the internal buffers)
    procedure Finish;
  end;


/// direct MD5 hash calculation of some data
function MD5Buf(const Buffer; Len: Cardinal): TMD5Digest;

/// direct MD5 hash calculation of some data (string-encoded)
// - result is returned in hexadecimal format
function MD5(const s: RawByteString): RawUTF8;

/// direct SHA1 hash calculation of some data (string-encoded)
// - result is returned in hexadecimal format
function SHA1(const s: RawByteString): RawUTF8;

/// direct SHA256 hash calculation of some data (string-encoded)
// - result is returned in hexadecimal format
function SHA256(const s: RawByteString): RawUTF8;

/// direct SHA256 hash calculation of some data (string-encoded)
// - result is returned in hexadecimal format
// - this procedure has a weak password protection: small incoming data
// is append to some salt, in order to have at least a 256 bytes long hash:
// such a feature improve security for small passwords, e.g.
procedure SHA256Weak(const s: RawByteString; out Digest: TSHA256Digest); overload;


/// direct Encrypt/Decrypt of data using the TAES class
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
procedure AES(const Key; KeySize: cardinal; buffer: pointer; Len: Integer; Encrypt: boolean); overload;

/// direct Encrypt/Decrypt of data using the TAES class
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
procedure AES(const Key; KeySize: cardinal; bIn, bOut: pointer; Len: Integer; Encrypt: boolean); overload;

/// direct Encrypt/Decrypt of data using the TAES class
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
function  AES(const Key; KeySize: cardinal; const s: RawByteString; Encrypt: boolean): RawByteString; overload;

/// direct Encrypt/Decrypt of data using the TAES class
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
function  AES(const Key; KeySize: cardinal; buffer: pointer; Len: cardinal; Stream: TStream; Encrypt: boolean): boolean; overload;

/// AES and XOR encryption using the TAESFull format
// - outStream will be larger/smaller than Len (full AES encrypted)
// - returns true if OK
function AESFull(const Key; KeySize: cardinal; bIn: pointer; Len: Integer;
  outStream: TStream; Encrypt: boolean; OriginalLen: Cardinal=0): boolean; overload;

/// AES and XOR encryption using the TAESFull format
// - bOut must be at least bIn+32/Encrypt bIn-16/Decrypt
// - returns outLength, -1 if error
function AESFull(const Key; KeySize: cardinal; bIn, bOut: pointer; Len: Integer;
  Encrypt: boolean; OriginalLen: Cardinal=0): integer; overload;

/// AES and XOR decryption check using the TAESFull format
// - return true if begining of buff contains true AESFull encrypted data with this Key
// - if not KeySize in [128,192,256] -> use fast and efficient Xor Cypher
function AESFullKeyOK(const Key; KeySize: cardinal; buff: pointer): boolean;

/// AES encryption using the TAES format with a supplied SHA256 password
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
procedure AESSHA256(Buffer: pointer; Len: integer; const Password: RawByteString; Encrypt: boolean); overload;

/// AES encryption using the TAES format with a supplied SHA256 password
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
procedure AESSHA256(bIn, bOut: pointer; Len: integer; const Password: RawByteString; Encrypt: boolean); overload;

/// AES encryption using the TAES format with a supplied SHA256 password
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
function  AESSHA256(const s, Password: RawByteString; Encrypt: boolean): RawByteString; overload;

/// AES encryption using the TAESFull format with a supplied SHA256 password
// - outStream will be larger/smaller than Len: this is a full AES version with
// a triming TAESFullHeader at the beginning
procedure AESSHA256Full(bIn: pointer; Len: Integer; outStream: TStream; const Password: RawByteString; Encrypt: boolean); overload;

/// compute the hexadecimal representation of a SHA1 digest
function SHA1DigestToString(const D: TSHA1Digest): RawUTF8;

/// compute the hexadecimal representation of a SHA256 digest
function SHA256DigestToString(const D: TSHA256Digest): RawUTF8;

/// compare two supplied MD5 digests
function MD5DigestsEqual(const A, B: TMD5Digest): Boolean;

/// compute the hexadecimal representation of a MD5 digest
function MD5DigestToString(const D: TMD5Digest): RawUTF8;

/// compute the HTDigest for a user and a realm, according to a supplied password
// - apache-compatible: 'agent007:download area:8364d0044ef57b3defcfa141e8f77b65'
function htdigest(const user, realm, pass: RawByteString): RawUTF8;

/// self test of Adler32 routines
function Adler32SelfTest: boolean;

/// self test of MD5 routines
function MD5SelfTest: boolean;

/// self test of SHA1 routines
function SHA1SelfTest: boolean;

/// self test of SHA256 routines
function SHA256SelfTest: boolean;

/// self test of AES routines
function AESSelfTest: boolean;


// little endian fast conversion
// - 32 bits  = 1 integer
// - use fast bswap asm in x86/x64 mode
function bswap32(a: cardinal): cardinal;

// little endian fast conversion
// - 160 bits = 5 integers
// - use fast bswap asm in x86/x64 mode
procedure bswap160(s,d: PIntegerArray);

// little endian fast conversion
// - 256 bits = 8 integers
// - use fast bswap asm in x86/x64 mode
procedure bswap256(s,d: PIntegerArray);

/// simple Adler32 implementation
// - a bit slower than Adler32Asm() version below, but shorter code size
function Adler32Pas(Adler: cardinal; p: pointer; Count: Integer): cardinal;

/// fast Adler32 implementation
// - 16-bytes-chunck unrolled asm version
function Adler32Asm(Adler: cardinal; p: pointer; Count: Integer): cardinal;

// - very fast XOR according to Cod - not Compression or Stream compatible
// - used in AESFull() for KeySize=32
procedure XorBlock(p: PIntegerArray; Count, Cod: integer);

/// fast and simple XOR Cypher using Index (=Position in Dest Stream)
// - Compression not compatible with this function: should be applied after
// compress (e.g. as outStream for TAESWriteStream)
// - Stream compatible (with updated Index)
// - used in AES() and TAESWriteStream
procedure XorOffset(p: pByte; Index,Count: integer);

/// fast XOR Cypher changing by Count value
// - Compression compatible, since the XOR value is always the same, the
// compression rate will not change a lot
procedure XorConst(p: PIntegerArray; Count: integer);


{$ifdef USEPADLOCK}
var
  /// if dll/so and VIA padlock compatible CPU are present
  padlock_available: boolean = false;
{$endif}


implementation

{$ifdef USEPADLOCK}
{$ifdef Linux}
uses
  LibC;
{$endif}

const
  AES_SUCCEEDED = 0;
  KEY_128BITS = 0;
  KEY_192BITS = 1;
  KEY_256BITS = 2;
  ACE_AES_ECB = 0;
  ACE_AES_CBC = 1;

{$ifdef USEPADLOCKDLL}
type
  tpadlock_phe_available = function: boolean; cdecl;
  tpadlock_phe_sha = function(
    buffer: pointer; nbytes: integer; var Digest): integer; cdecl;

  tpadlock_ace_available = function: boolean; cdecl;
  tpadlock_aes_begin = function: pointer; cdecl;
  tpadlock_aes_setkey = function(
    ctx: pointer; const key; key_len: integer): integer; cdecl;
  tpadlock_aes_setmodeiv = function(
    ctx: pointer; mode: integer; var iv): integer; cdecl;
  tpadlock_aes_encrypt = function(
    ctx, bIn, bOut: pointer; nbytes: integer): integer; cdecl;
  tpadlock_aes_decrypt = function(
    ctx, bIn, bOut: pointer; nbytes: integer): integer; cdecl;
  tpadlock_aes_close = function(
    ctx: pointer): integer; cdecl;

var
  padlock_phe_available: tpadlock_phe_available = nil;
  padlock_phe_sha1: tpadlock_phe_sha = nil;
  padlock_phe_sha256: tpadlock_phe_sha = nil;

  padlock_ace_available: tpadlock_ace_available = nil;
  padlock_aes_begin: tpadlock_aes_begin = nil;
  padlock_aes_setkey: tpadlock_aes_setkey = nil;
  padlock_aes_setmodeiv: tpadlock_aes_setmodeiv = nil;
  padlock_aes_encrypt: tpadlock_aes_encrypt = nil;
  padlock_aes_decrypt: tpadlock_aes_decrypt = nil;
  padlock_aes_close: tpadlock_aes_close = nil;

{$ifdef MSWINDOWS}
  LibHandle: THandle = 0;
{$else} // Linux:
  LibHandle: HMODULE = 0;
{$endif}


procedure PadlockInit;
begin
{$ifdef MSWINDOWS}
  LibHandle := LoadLibrary('LibPadlock');
{$else} // Linux:
  LibHandle := LoadLibrary('libvia_padlock.so');
  if LibHandle=0 then
    LibHandle := LoadLibrary('libvia_padlock.so.1.0.0');
{$endif}
  if LibHandle=0 then
    exit;
  padlock_phe_available := GetProcAddress(LibHandle,'padlock_phe_available');
  padlock_phe_sha1 := GetProcAddress(LibHandle,'padlock_phe_sha1');
  padlock_phe_sha256 := GetProcAddress(LibHandle,'padlock_phe_sha256');
  padlock_ace_available := GetProcAddress(LibHandle,'padlock_ace_available');
  padlock_aes_begin := GetProcAddress(LibHandle,'padlock_aes_begin');
  padlock_aes_setkey := GetProcAddress(LibHandle,'padlock_aes_setkey');
  padlock_aes_setmodeiv := GetProcAddress(LibHandle,'padlock_aes_setmodeiv');
  padlock_aes_encrypt := GetProcAddress(LibHandle,'padlock_aes_encrypt');
  padlock_aes_decrypt := GetProcAddress(LibHandle,'padlock_aes_decrypt');
  padlock_aes_close := GetProcAddress(LibHandle,'padlock_aes_close');
  if @padlock_phe_available=nil then exit;
  if @padlock_phe_sha1=nil then exit;
  if @padlock_phe_sha256=nil then exit;
  if @padlock_ace_available=nil then exit;
  if @padlock_aes_begin=nil then exit;
  if @padlock_aes_setkey=nil then exit;
  if @padlock_aes_setmodeiv=nil then exit;
  if @padlock_aes_encrypt=nil then exit;
  if @padlock_aes_decrypt=nil then exit;
  if @padlock_aes_close=nil then exit;
  if padlock_phe_available and padlock_ace_available then
    padlock_available := true;
end;
{$else} // not USEPADLOCKDLL:

{$ifdef MSWINDOWS}
{$L padlock.obj}
{$L padlock_sha.obj}
{$L padlock_aes.obj}

function memcpy(dest, src: Pointer; count: integer): Pointer; cdecl;
begin
  Move(src^, dest^, count);
  Result := dest;
end;

function memset(dest: Pointer; val: Integer; count: integer): Pointer; cdecl;
begin
  FillChar(dest^, count, val);
  Result := dest;
end;

function malloc(size: integer): Pointer; cdecl;
begin
  GetMem(Result, size);
end;

procedure free(pBlock: Pointer); cdecl;
begin
  FreeMem(pBlock);
end;

function printf(format:PAnsiChar; args:array of const): PAnsiChar; cdecl;
begin
  result := format;
  // called on error -> do nothing
end;

{$else}
{$L padlock.o}
{$L padlock_sha.o}
{$L padlock_aes.o}
{$endif}

{ this .o files have been generated from the sdk sources with
    gcc-2.95 -c -O2 padlock*.c -I../include
}
  function padlock_phe_available: boolean;                                         cdecl; external;
  function padlock_phe_sha1(buf: pointer; nbytes: integer; var Digest): integer;   cdecl; external;
  function padlock_phe_sha256(buf: pointer; nbytes: integer; var Digest): integer; cdecl; external;

  function padlock_ace_available: boolean;                                         cdecl; external;
  function padlock_aes_begin: pointer;                                             cdecl; external;
  function padlock_aes_setkey(ctx: pointer; const key; key_len: integer): integer; cdecl; external;
  function padlock_aes_setmodeiv (ctx: pointer; mode: integer; var iv): integer;   cdecl; external;
  function padlock_aes_encrypt(ctx, bIn, bOut: pointer; nbytes: integer): integer; cdecl; external;
  function padlock_aes_decrypt(ctx, bIn, bOut: pointer; nbytes: integer): integer; cdecl; external;
  function padlock_aes_close(ctx: pointer): integer;                               cdecl; external;

procedure PadlockInit;
begin
  if padlock_phe_available and padlock_ace_available then
    padlock_available := true;
{$ifdef PADLOCKDEBUG}if padlock_available then writeln('PADLOCK available'); {$endif}
end;
{$endif USEPADLOCKDLL}
{$endif USEPADLOCK}

const
  AESMaxRounds = 14;

type
  TKeyArray   = packed array[0..AESMaxRounds] of TAESBlock;
  PAESContext = ^TAESContext;
{$A-} { packed memory structure }
  TAESContext = record
    // don't change the structure below: it is fixed in the asm code
    // -> use PUREPASCAL if you really have to change it
    RK: TKeyArray;   // Key (encr. or decr.)
    IV: TAESBlock;   // IV or CTR
    buf: TAESBlock;  // Work buffer
    bLen: word;      // Bytes used in buf
    Rounds: word;    // Number of rounds
    KeyBits: word;   // Number of bits in key
    ViaCtx: pointer; // padlock_*() context
  end;
{$A+}

const
  AESBLKSIZE  = AESBlockSize;


// helper types for better code generation
type
  TWA4  = packed array[0..3] of cardinal;     // AES block as array of cardinal
  TBA4  = packed array[0..3] of byte;         // AES "word" as array of byte  
  TAWk  = packed array[0..4*(AESMaxRounds+1)-1] of cardinal; // Key as array of cardinal
  PWA4  = ^TWA4;
  PAWk  = ^TAWk;
  PLong = ^cardinal;


const
  RCon: array[0..9] of cardinal= ($01,$02,$04,$08,$10,$20,$40,$80,$1b,$36);


// AES static tables 
const
  SBox: array[byte] of byte =
   ($63, $7c, $77, $7b, $f2, $6b, $6f, $c5, $30, $01, $67, $2b, $fe, $d7, $ab, $76,
    $ca, $82, $c9, $7d, $fa, $59, $47, $f0, $ad, $d4, $a2, $af, $9c, $a4, $72, $c0,
    $b7, $fd, $93, $26, $36, $3f, $f7, $cc, $34, $a5, $e5, $f1, $71, $d8, $31, $15,
    $04, $c7, $23, $c3, $18, $96, $05, $9a, $07, $12, $80, $e2, $eb, $27, $b2, $75,
    $09, $83, $2c, $1a, $1b, $6e, $5a, $a0, $52, $3b, $d6, $b3, $29, $e3, $2f, $84,
    $53, $d1, $00, $ed, $20, $fc, $b1, $5b, $6a, $cb, $be, $39, $4a, $4c, $58, $cf,
    $d0, $ef, $aa, $fb, $43, $4d, $33, $85, $45, $f9, $02, $7f, $50, $3c, $9f, $a8,
    $51, $a3, $40, $8f, $92, $9d, $38, $f5, $bc, $b6, $da, $21, $10, $ff, $f3, $d2,
    $cd, $0c, $13, $ec, $5f, $97, $44, $17, $c4, $a7, $7e, $3d, $64, $5d, $19, $73,
    $60, $81, $4f, $dc, $22, $2a, $90, $88, $46, $ee, $b8, $14, $de, $5e, $0b, $db,
    $e0, $32, $3a, $0a, $49, $06, $24, $5c, $c2, $d3, $ac, $62, $91, $95, $e4, $79,
    $e7, $c8, $37, $6d, $8d, $d5, $4e, $a9, $6c, $56, $f4, $ea, $65, $7a, $ae, $08,
    $ba, $78, $25, $2e, $1c, $a6, $b4, $c6, $e8, $dd, $74, $1f, $4b, $bd, $8b, $8a,
    $70, $3e, $b5, $66, $48, $03, $f6, $0e, $61, $35, $57, $b9, $86, $c1, $1d, $9e,
    $e1, $f8, $98, $11, $69, $d9, $8e, $94, $9b, $1e, $87, $e9, $ce, $55, $28, $df,
    $8c, $a1, $89, $0d, $bf, $e6, $42, $68, $41, $99, $2d, $0f, $b0, $54, $bb, $16);

const
  InvSBox: array[byte] of byte =
   ($52, $09, $6a, $d5, $30, $36, $a5, $38, $bf, $40, $a3, $9e, $81, $f3, $d7, $fb,
    $7c, $e3, $39, $82, $9b, $2f, $ff, $87, $34, $8e, $43, $44, $c4, $de, $e9, $cb,
    $54, $7b, $94, $32, $a6, $c2, $23, $3d, $ee, $4c, $95, $0b, $42, $fa, $c3, $4e,
    $08, $2e, $a1, $66, $28, $d9, $24, $b2, $76, $5b, $a2, $49, $6d, $8b, $d1, $25,
    $72, $f8, $f6, $64, $86, $68, $98, $16, $d4, $a4, $5c, $cc, $5d, $65, $b6, $92,
    $6c, $70, $48, $50, $fd, $ed, $b9, $da, $5e, $15, $46, $57, $a7, $8d, $9d, $84,
    $90, $d8, $ab, $00, $8c, $bc, $d3, $0a, $f7, $e4, $58, $05, $b8, $b3, $45, $06,
    $d0, $2c, $1e, $8f, $ca, $3f, $0f, $02, $c1, $af, $bd, $03, $01, $13, $8a, $6b,
    $3a, $91, $11, $41, $4f, $67, $dc, $ea, $97, $f2, $cf, $ce, $f0, $b4, $e6, $73,
    $96, $ac, $74, $22, $e7, $ad, $35, $85, $e2, $f9, $37, $e8, $1c, $75, $df, $6e,
    $47, $f1, $1a, $71, $1d, $29, $c5, $89, $6f, $b7, $62, $0e, $aa, $18, $be, $1b,
    $fc, $56, $3e, $4b, $c6, $d2, $79, $20, $9a, $db, $c0, $fe, $78, $cd, $5a, $f4,
    $1f, $dd, $a8, $33, $88, $07, $c7, $31, $b1, $12, $10, $59, $27, $80, $ec, $5f,
    $60, $51, $7f, $a9, $19, $b5, $4a, $0d, $2d, $e5, $7a, $9f, $93, $c9, $9c, $ef,
    $a0, $e0, $3b, $4d, $ae, $2a, $f5, $b0, $c8, $eb, $bb, $3c, $83, $53, $99, $61,
    $17, $2b, $04, $7e, $ba, $77, $d6, $26, $e1, $69, $14, $63, $55, $21, $0c, $7d);


const
 Td0: array[byte] of cardinal =
   ($50a7f451, $5365417e, $c3a4171a, $965e273a, $cb6bab3b, $f1459d1f, $ab58faac, $9303e34b,
    $55fa3020, $f66d76ad, $9176cc88, $254c02f5, $fcd7e54f, $d7cb2ac5, $80443526, $8fa362b5,
    $495ab1de, $671bba25, $980eea45, $e1c0fe5d, $02752fc3, $12f04c81, $a397468d, $c6f9d36b,
    $e75f8f03, $959c9215, $eb7a6dbf, $da595295, $2d83bed4, $d3217458, $2969e049, $44c8c98e,
    $6a89c275, $78798ef4, $6b3e5899, $dd71b927, $b64fe1be, $17ad88f0, $66ac20c9, $b43ace7d,
    $184adf63, $82311ae5, $60335197, $457f5362, $e07764b1, $84ae6bbb, $1ca081fe, $942b08f9,
    $58684870, $19fd458f, $876cde94, $b7f87b52, $23d373ab, $e2024b72, $578f1fe3, $2aab5566,
    $0728ebb2, $03c2b52f, $9a7bc586, $a50837d3, $f2872830, $b2a5bf23, $ba6a0302, $5c8216ed,
    $2b1ccf8a, $92b479a7, $f0f207f3, $a1e2694e, $cdf4da65, $d5be0506, $1f6234d1, $8afea6c4,
    $9d532e34, $a055f3a2, $32e18a05, $75ebf6a4, $39ec830b, $aaef6040, $069f715e, $51106ebd,
    $f98a213e, $3d06dd96, $ae053edd, $46bde64d, $b58d5491, $055dc471, $6fd40604, $ff155060,
    $24fb9819, $97e9bdd6, $cc434089, $779ed967, $bd42e8b0, $888b8907, $385b19e7, $dbeec879,
    $470a7ca1, $e90f427c, $c91e84f8, $00000000, $83868009, $48ed2b32, $ac70111e, $4e725a6c,
    $fbff0efd, $5638850f, $1ed5ae3d, $27392d36, $64d90f0a, $21a65c68, $d1545b9b, $3a2e3624,
    $b1670a0c, $0fe75793, $d296eeb4, $9e919b1b, $4fc5c080, $a220dc61, $694b775a, $161a121c,
    $0aba93e2, $e52aa0c0, $43e0223c, $1d171b12, $0b0d090e, $adc78bf2, $b9a8b62d, $c8a91e14,
    $8519f157, $4c0775af, $bbdd99ee, $fd607fa3, $9f2601f7, $bcf5725c, $c53b6644, $347efb5b,
    $7629438b, $dcc623cb, $68fcedb6, $63f1e4b8, $cadc31d7, $10856342, $40229713, $2011c684,
    $7d244a85, $f83dbbd2, $1132f9ae, $6da129c7, $4b2f9e1d, $f330b2dc, $ec52860d, $d0e3c177,
    $6c16b32b, $99b970a9, $fa489411, $2264e947, $c48cfca8, $1a3ff0a0, $d82c7d56, $ef903322,
    $c74e4987, $c1d138d9, $fea2ca8c, $360bd498, $cf81f5a6, $28de7aa5, $268eb7da, $a4bfad3f,
    $e49d3a2c, $0d927850, $9bcc5f6a, $62467e54, $c2138df6, $e8b8d890, $5ef7392e, $f5afc382,
    $be805d9f, $7c93d069, $a92dd56f, $b31225cf, $3b99acc8, $a77d1810, $6e639ce8, $7bbb3bdb,
    $097826cd, $f418596e, $01b79aec, $a89a4f83, $656e95e6, $7ee6ffaa, $08cfbc21, $e6e815ef,
    $d99be7ba, $ce366f4a, $d4099fea, $d67cb029, $afb2a431, $31233f2a, $3094a5c6, $c066a235,
    $37bc4e74, $a6ca82fc, $b0d090e0, $15d8a733, $4a9804f1, $f7daec41, $0e50cd7f, $2ff69117,
    $8dd64d76, $4db0ef43, $544daacc, $df0496e4, $e3b5d19e, $1b886a4c, $b81f2cc1, $7f516546,
    $04ea5e9d, $5d358c01, $737487fa, $2e410bfb, $5a1d67b3, $52d2db92, $335610e9, $1347d66d,
    $8c61d79a, $7a0ca137, $8e14f859, $893c13eb, $ee27a9ce, $35c961b7, $ede51ce1, $3cb1477a,
    $59dfd29c, $3f73f255, $79ce1418, $bf37c773, $eacdf753, $5baafd5f, $146f3ddf, $86db4478,
    $81f3afca, $3ec468b9, $2c342438, $5f40a3c2, $72c31d16, $0c25e2bc, $8b493c28, $41950dff,
    $7101a839, $deb30c08, $9ce4b4d8, $90c15664, $6184cb7b, $70b632d5, $745c6c48, $4257b8d0);

  Td1: array[byte] of cardinal =
   ($a7f45150, $65417e53, $a4171ac3, $5e273a96, $6bab3bcb, $459d1ff1, $58faacab, $03e34b93,
    $fa302055, $6d76adf6, $76cc8891, $4c02f525, $d7e54ffc, $cb2ac5d7, $44352680, $a362b58f,
    $5ab1de49, $1bba2567, $0eea4598, $c0fe5de1, $752fc302, $f04c8112, $97468da3, $f9d36bc6,
    $5f8f03e7, $9c921595, $7a6dbfeb, $595295da, $83bed42d, $217458d3, $69e04929, $c8c98e44,
    $89c2756a, $798ef478, $3e58996b, $71b927dd, $4fe1beb6, $ad88f017, $ac20c966, $3ace7db4,
    $4adf6318, $311ae582, $33519760, $7f536245, $7764b1e0, $ae6bbb84, $a081fe1c, $2b08f994,
    $68487058, $fd458f19, $6cde9487, $f87b52b7, $d373ab23, $024b72e2, $8f1fe357, $ab55662a,
    $28ebb207, $c2b52f03, $7bc5869a, $0837d3a5, $872830f2, $a5bf23b2, $6a0302ba, $8216ed5c,
    $1ccf8a2b, $b479a792, $f207f3f0, $e2694ea1, $f4da65cd, $be0506d5, $6234d11f, $fea6c48a,
    $532e349d, $55f3a2a0, $e18a0532, $ebf6a475, $ec830b39, $ef6040aa, $9f715e06, $106ebd51,
    $8a213ef9, $06dd963d, $053eddae, $bde64d46, $8d5491b5, $5dc47105, $d406046f, $155060ff,
    $fb981924, $e9bdd697, $434089cc, $9ed96777, $42e8b0bd, $8b890788, $5b19e738, $eec879db,
    $0a7ca147, $0f427ce9, $1e84f8c9, $00000000, $86800983, $ed2b3248, $70111eac, $725a6c4e,
    $ff0efdfb, $38850f56, $d5ae3d1e, $392d3627, $d90f0a64, $a65c6821, $545b9bd1, $2e36243a,
    $670a0cb1, $e757930f, $96eeb4d2, $919b1b9e, $c5c0804f, $20dc61a2, $4b775a69, $1a121c16,
    $ba93e20a, $2aa0c0e5, $e0223c43, $171b121d, $0d090e0b, $c78bf2ad, $a8b62db9, $a91e14c8,
    $19f15785, $0775af4c, $dd99eebb, $607fa3fd, $2601f79f, $f5725cbc, $3b6644c5, $7efb5b34,
    $29438b76, $c623cbdc, $fcedb668, $f1e4b863, $dc31d7ca, $85634210, $22971340, $11c68420,
    $244a857d, $3dbbd2f8, $32f9ae11, $a129c76d, $2f9e1d4b, $30b2dcf3, $52860dec, $e3c177d0,
    $16b32b6c, $b970a999, $489411fa, $64e94722, $8cfca8c4, $3ff0a01a, $2c7d56d8, $903322ef,
    $4e4987c7, $d138d9c1, $a2ca8cfe, $0bd49836, $81f5a6cf, $de7aa528, $8eb7da26, $bfad3fa4,
    $9d3a2ce4, $9278500d, $cc5f6a9b, $467e5462, $138df6c2, $b8d890e8, $f7392e5e, $afc382f5,
    $805d9fbe, $93d0697c, $2dd56fa9, $1225cfb3, $99acc83b, $7d1810a7, $639ce86e, $bb3bdb7b,
    $7826cd09, $18596ef4, $b79aec01, $9a4f83a8, $6e95e665, $e6ffaa7e, $cfbc2108, $e815efe6,
    $9be7bad9, $366f4ace, $099fead4, $7cb029d6, $b2a431af, $233f2a31, $94a5c630, $66a235c0,
    $bc4e7437, $ca82fca6, $d090e0b0, $d8a73315, $9804f14a, $daec41f7, $50cd7f0e, $f691172f,
    $d64d768d, $b0ef434d, $4daacc54, $0496e4df, $b5d19ee3, $886a4c1b, $1f2cc1b8, $5165467f,
    $ea5e9d04, $358c015d, $7487fa73, $410bfb2e, $1d67b35a, $d2db9252, $5610e933, $47d66d13,
    $61d79a8c, $0ca1377a, $14f8598e, $3c13eb89, $27a9ceee, $c961b735, $e51ce1ed, $b1477a3c,
    $dfd29c59, $73f2553f, $ce141879, $37c773bf, $cdf753ea, $aafd5f5b, $6f3ddf14, $db447886,
    $f3afca81, $c468b93e, $3424382c, $40a3c25f, $c31d1672, $25e2bc0c, $493c288b, $950dff41,
    $01a83971, $b30c08de, $e4b4d89c, $c1566490, $84cb7b61, $b632d570, $5c6c4874, $57b8d042);

  Td2: array[byte] of cardinal =
   ($f45150a7, $417e5365, $171ac3a4, $273a965e, $ab3bcb6b, $9d1ff145, $faacab58, $e34b9303,
    $302055fa, $76adf66d, $cc889176, $02f5254c, $e54ffcd7, $2ac5d7cb, $35268044, $62b58fa3,
    $b1de495a, $ba25671b, $ea45980e, $fe5de1c0, $2fc30275, $4c8112f0, $468da397, $d36bc6f9,
    $8f03e75f, $9215959c, $6dbfeb7a, $5295da59, $bed42d83, $7458d321, $e0492969, $c98e44c8,
    $c2756a89, $8ef47879, $58996b3e, $b927dd71, $e1beb64f, $88f017ad, $20c966ac, $ce7db43a,
    $df63184a, $1ae58231, $51976033, $5362457f, $64b1e077, $6bbb84ae, $81fe1ca0, $08f9942b,
    $48705868, $458f19fd, $de94876c, $7b52b7f8, $73ab23d3, $4b72e202, $1fe3578f, $55662aab,
    $ebb20728, $b52f03c2, $c5869a7b, $37d3a508, $2830f287, $bf23b2a5, $0302ba6a, $16ed5c82,
    $cf8a2b1c, $79a792b4, $07f3f0f2, $694ea1e2, $da65cdf4, $0506d5be, $34d11f62, $a6c48afe,
    $2e349d53, $f3a2a055, $8a0532e1, $f6a475eb, $830b39ec, $6040aaef, $715e069f, $6ebd5110,
    $213ef98a, $dd963d06, $3eddae05, $e64d46bd, $5491b58d, $c471055d, $06046fd4, $5060ff15,
    $981924fb, $bdd697e9, $4089cc43, $d967779e, $e8b0bd42, $8907888b, $19e7385b, $c879dbee,
    $7ca1470a, $427ce90f, $84f8c91e, $00000000, $80098386, $2b3248ed, $111eac70, $5a6c4e72,
    $0efdfbff, $850f5638, $ae3d1ed5, $2d362739, $0f0a64d9, $5c6821a6, $5b9bd154, $36243a2e,
    $0a0cb167, $57930fe7, $eeb4d296, $9b1b9e91, $c0804fc5, $dc61a220, $775a694b, $121c161a,
    $93e20aba, $a0c0e52a, $223c43e0, $1b121d17, $090e0b0d, $8bf2adc7, $b62db9a8, $1e14c8a9,
    $f1578519, $75af4c07, $99eebbdd, $7fa3fd60, $01f79f26, $725cbcf5, $6644c53b, $fb5b347e,
    $438b7629, $23cbdcc6, $edb668fc, $e4b863f1, $31d7cadc, $63421085, $97134022, $c6842011,
    $4a857d24, $bbd2f83d, $f9ae1132, $29c76da1, $9e1d4b2f, $b2dcf330, $860dec52, $c177d0e3,
    $b32b6c16, $70a999b9, $9411fa48, $e9472264, $fca8c48c, $f0a01a3f, $7d56d82c, $3322ef90,
    $4987c74e, $38d9c1d1, $ca8cfea2, $d498360b, $f5a6cf81, $7aa528de, $b7da268e, $ad3fa4bf,
    $3a2ce49d, $78500d92, $5f6a9bcc, $7e546246, $8df6c213, $d890e8b8, $392e5ef7, $c382f5af,
    $5d9fbe80, $d0697c93, $d56fa92d, $25cfb312, $acc83b99, $1810a77d, $9ce86e63, $3bdb7bbb,
    $26cd0978, $596ef418, $9aec01b7, $4f83a89a, $95e6656e, $ffaa7ee6, $bc2108cf, $15efe6e8,
    $e7bad99b, $6f4ace36, $9fead409, $b029d67c, $a431afb2, $3f2a3123, $a5c63094, $a235c066,
    $4e7437bc, $82fca6ca, $90e0b0d0, $a73315d8, $04f14a98, $ec41f7da, $cd7f0e50, $91172ff6,
    $4d768dd6, $ef434db0, $aacc544d, $96e4df04, $d19ee3b5, $6a4c1b88, $2cc1b81f, $65467f51,
    $5e9d04ea, $8c015d35, $87fa7374, $0bfb2e41, $67b35a1d, $db9252d2, $10e93356, $d66d1347,
    $d79a8c61, $a1377a0c, $f8598e14, $13eb893c, $a9ceee27, $61b735c9, $1ce1ede5, $477a3cb1,
    $d29c59df, $f2553f73, $141879ce, $c773bf37, $f753eacd, $fd5f5baa, $3ddf146f, $447886db,
    $afca81f3, $68b93ec4, $24382c34, $a3c25f40, $1d1672c3, $e2bc0c25, $3c288b49, $0dff4195,
    $a8397101, $0c08deb3, $b4d89ce4, $566490c1, $cb7b6184, $32d570b6, $6c48745c, $b8d04257);

  Td3: array[byte] of cardinal =
   ($5150a7f4, $7e536541, $1ac3a417, $3a965e27, $3bcb6bab, $1ff1459d, $acab58fa, $4b9303e3,
    $2055fa30, $adf66d76, $889176cc, $f5254c02, $4ffcd7e5, $c5d7cb2a, $26804435, $b58fa362,
    $de495ab1, $25671bba, $45980eea, $5de1c0fe, $c302752f, $8112f04c, $8da39746, $6bc6f9d3,
    $03e75f8f, $15959c92, $bfeb7a6d, $95da5952, $d42d83be, $58d32174, $492969e0, $8e44c8c9,
    $756a89c2, $f478798e, $996b3e58, $27dd71b9, $beb64fe1, $f017ad88, $c966ac20, $7db43ace,
    $63184adf, $e582311a, $97603351, $62457f53, $b1e07764, $bb84ae6b, $fe1ca081, $f9942b08,
    $70586848, $8f19fd45, $94876cde, $52b7f87b, $ab23d373, $72e2024b, $e3578f1f, $662aab55,
    $b20728eb, $2f03c2b5, $869a7bc5, $d3a50837, $30f28728, $23b2a5bf, $02ba6a03, $ed5c8216,
    $8a2b1ccf, $a792b479, $f3f0f207, $4ea1e269, $65cdf4da, $06d5be05, $d11f6234, $c48afea6,
    $349d532e, $a2a055f3, $0532e18a, $a475ebf6, $0b39ec83, $40aaef60, $5e069f71, $bd51106e,
    $3ef98a21, $963d06dd, $ddae053e, $4d46bde6, $91b58d54, $71055dc4, $046fd406, $60ff1550,
    $1924fb98, $d697e9bd, $89cc4340, $67779ed9, $b0bd42e8, $07888b89, $e7385b19, $79dbeec8,
    $a1470a7c, $7ce90f42, $f8c91e84, $00000000, $09838680, $3248ed2b, $1eac7011, $6c4e725a,
    $fdfbff0e, $0f563885, $3d1ed5ae, $3627392d, $0a64d90f, $6821a65c, $9bd1545b, $243a2e36,
    $0cb1670a, $930fe757, $b4d296ee, $1b9e919b, $804fc5c0, $61a220dc, $5a694b77, $1c161a12,
    $e20aba93, $c0e52aa0, $3c43e022, $121d171b, $0e0b0d09, $f2adc78b, $2db9a8b6, $14c8a91e,
    $578519f1, $af4c0775, $eebbdd99, $a3fd607f, $f79f2601, $5cbcf572, $44c53b66, $5b347efb,
    $8b762943, $cbdcc623, $b668fced, $b863f1e4, $d7cadc31, $42108563, $13402297, $842011c6,
    $857d244a, $d2f83dbb, $ae1132f9, $c76da129, $1d4b2f9e, $dcf330b2, $0dec5286, $77d0e3c1,
    $2b6c16b3, $a999b970, $11fa4894, $472264e9, $a8c48cfc, $a01a3ff0, $56d82c7d, $22ef9033,
    $87c74e49, $d9c1d138, $8cfea2ca, $98360bd4, $a6cf81f5, $a528de7a, $da268eb7, $3fa4bfad,
    $2ce49d3a, $500d9278, $6a9bcc5f, $5462467e, $f6c2138d, $90e8b8d8, $2e5ef739, $82f5afc3,
    $9fbe805d, $697c93d0, $6fa92dd5, $cfb31225, $c83b99ac, $10a77d18, $e86e639c, $db7bbb3b,
    $cd097826, $6ef41859, $ec01b79a, $83a89a4f, $e6656e95, $aa7ee6ff, $2108cfbc, $efe6e815,
    $bad99be7, $4ace366f, $ead4099f, $29d67cb0, $31afb2a4, $2a31233f, $c63094a5, $35c066a2,
    $7437bc4e, $fca6ca82, $e0b0d090, $3315d8a7, $f14a9804, $41f7daec, $7f0e50cd, $172ff691,
    $768dd64d, $434db0ef, $cc544daa, $e4df0496, $9ee3b5d1, $4c1b886a, $c1b81f2c, $467f5165,
    $9d04ea5e, $015d358c, $fa737487, $fb2e410b, $b35a1d67, $9252d2db, $e9335610, $6d1347d6,
    $9a8c61d7, $377a0ca1, $598e14f8, $eb893c13, $ceee27a9, $b735c961, $e1ede51c, $7a3cb147,
    $9c59dfd2, $553f73f2, $1879ce14, $73bf37c7, $53eacdf7, $5f5baafd, $df146f3d, $7886db44,
    $ca81f3af, $b93ec468, $382c3424, $c25f40a3, $1672c31d, $bc0c25e2, $288b493c, $ff41950d,
    $397101a8, $08deb30c, $d89ce4b4, $6490c156, $7b6184cb, $d570b632, $48745c6c, $d04257b8);

  Te0: array[byte] of cardinal =
   ($a56363c6, $847c7cf8, $997777ee, $8d7b7bf6, $0df2f2ff, $bd6b6bd6, $b16f6fde, $54c5c591,
    $50303060, $03010102, $a96767ce, $7d2b2b56, $19fefee7, $62d7d7b5, $e6abab4d, $9a7676ec,
    $45caca8f, $9d82821f, $40c9c989, $877d7dfa, $15fafaef, $eb5959b2, $c947478e, $0bf0f0fb,
    $ecadad41, $67d4d4b3, $fda2a25f, $eaafaf45, $bf9c9c23, $f7a4a453, $967272e4, $5bc0c09b,
    $c2b7b775, $1cfdfde1, $ae93933d, $6a26264c, $5a36366c, $413f3f7e, $02f7f7f5, $4fcccc83,
    $5c343468, $f4a5a551, $34e5e5d1, $08f1f1f9, $937171e2, $73d8d8ab, $53313162, $3f15152a,
    $0c040408, $52c7c795, $65232346, $5ec3c39d, $28181830, $a1969637, $0f05050a, $b59a9a2f,
    $0907070e, $36121224, $9b80801b, $3de2e2df, $26ebebcd, $6927274e, $cdb2b27f, $9f7575ea,
    $1b090912, $9e83831d, $742c2c58, $2e1a1a34, $2d1b1b36, $b26e6edc, $ee5a5ab4, $fba0a05b,
    $f65252a4, $4d3b3b76, $61d6d6b7, $ceb3b37d, $7b292952, $3ee3e3dd, $712f2f5e, $97848413,
    $f55353a6, $68d1d1b9, $00000000, $2cededc1, $60202040, $1ffcfce3, $c8b1b179, $ed5b5bb6,
    $be6a6ad4, $46cbcb8d, $d9bebe67, $4b393972, $de4a4a94, $d44c4c98, $e85858b0, $4acfcf85,
    $6bd0d0bb, $2aefefc5, $e5aaaa4f, $16fbfbed, $c5434386, $d74d4d9a, $55333366, $94858511,
    $cf45458a, $10f9f9e9, $06020204, $817f7ffe, $f05050a0, $443c3c78, $ba9f9f25, $e3a8a84b,
    $f35151a2, $fea3a35d, $c0404080, $8a8f8f05, $ad92923f, $bc9d9d21, $48383870, $04f5f5f1,
    $dfbcbc63, $c1b6b677, $75dadaaf, $63212142, $30101020, $1affffe5, $0ef3f3fd, $6dd2d2bf,
    $4ccdcd81, $140c0c18, $35131326, $2fececc3, $e15f5fbe, $a2979735, $cc444488, $3917172e,
    $57c4c493, $f2a7a755, $827e7efc, $473d3d7a, $ac6464c8, $e75d5dba, $2b191932, $957373e6,
    $a06060c0, $98818119, $d14f4f9e, $7fdcdca3, $66222244, $7e2a2a54, $ab90903b, $8388880b,
    $ca46468c, $29eeeec7, $d3b8b86b, $3c141428, $79dedea7, $e25e5ebc, $1d0b0b16, $76dbdbad,
    $3be0e0db, $56323264, $4e3a3a74, $1e0a0a14, $db494992, $0a06060c, $6c242448, $e45c5cb8,
    $5dc2c29f, $6ed3d3bd, $efacac43, $a66262c4, $a8919139, $a4959531, $37e4e4d3, $8b7979f2,
    $32e7e7d5, $43c8c88b, $5937376e, $b76d6dda, $8c8d8d01, $64d5d5b1, $d24e4e9c, $e0a9a949,
    $b46c6cd8, $fa5656ac, $07f4f4f3, $25eaeacf, $af6565ca, $8e7a7af4, $e9aeae47, $18080810,
    $d5baba6f, $887878f0, $6f25254a, $722e2e5c, $241c1c38, $f1a6a657, $c7b4b473, $51c6c697,
    $23e8e8cb, $7cdddda1, $9c7474e8, $211f1f3e, $dd4b4b96, $dcbdbd61, $868b8b0d, $858a8a0f,
    $907070e0, $423e3e7c, $c4b5b571, $aa6666cc, $d8484890, $05030306, $01f6f6f7, $120e0e1c,
    $a36161c2, $5f35356a, $f95757ae, $d0b9b969, $91868617, $58c1c199, $271d1d3a, $b99e9e27,
    $38e1e1d9, $13f8f8eb, $b398982b, $33111122, $bb6969d2, $70d9d9a9, $898e8e07, $a7949433,
    $b69b9b2d, $221e1e3c, $92878715, $20e9e9c9, $49cece87, $ff5555aa, $78282850, $7adfdfa5,
    $8f8c8c03, $f8a1a159, $80898909, $170d0d1a, $dabfbf65, $31e6e6d7, $c6424284, $b86868d0,
    $c3414182, $b0999929, $772d2d5a, $110f0f1e, $cbb0b07b, $fc5454a8, $d6bbbb6d, $3a16162c);

  Te1: array[byte] of cardinal =
   ($6363c6a5, $7c7cf884, $7777ee99, $7b7bf68d, $f2f2ff0d, $6b6bd6bd, $6f6fdeb1, $c5c59154,
    $30306050, $01010203, $6767cea9, $2b2b567d, $fefee719, $d7d7b562, $abab4de6, $7676ec9a,
    $caca8f45, $82821f9d, $c9c98940, $7d7dfa87, $fafaef15, $5959b2eb, $47478ec9, $f0f0fb0b,
    $adad41ec, $d4d4b367, $a2a25ffd, $afaf45ea, $9c9c23bf, $a4a453f7, $7272e496, $c0c09b5b,
    $b7b775c2, $fdfde11c, $93933dae, $26264c6a, $36366c5a, $3f3f7e41, $f7f7f502, $cccc834f,
    $3434685c, $a5a551f4, $e5e5d134, $f1f1f908, $7171e293, $d8d8ab73, $31316253, $15152a3f,
    $0404080c, $c7c79552, $23234665, $c3c39d5e, $18183028, $969637a1, $05050a0f, $9a9a2fb5,
    $07070e09, $12122436, $80801b9b, $e2e2df3d, $ebebcd26, $27274e69, $b2b27fcd, $7575ea9f,
    $0909121b, $83831d9e, $2c2c5874, $1a1a342e, $1b1b362d, $6e6edcb2, $5a5ab4ee, $a0a05bfb,
    $5252a4f6, $3b3b764d, $d6d6b761, $b3b37dce, $2929527b, $e3e3dd3e, $2f2f5e71, $84841397,
    $5353a6f5, $d1d1b968, $00000000, $ededc12c, $20204060, $fcfce31f, $b1b179c8, $5b5bb6ed,
    $6a6ad4be, $cbcb8d46, $bebe67d9, $3939724b, $4a4a94de, $4c4c98d4, $5858b0e8, $cfcf854a,
    $d0d0bb6b, $efefc52a, $aaaa4fe5, $fbfbed16, $434386c5, $4d4d9ad7, $33336655, $85851194,
    $45458acf, $f9f9e910, $02020406, $7f7ffe81, $5050a0f0, $3c3c7844, $9f9f25ba, $a8a84be3,
    $5151a2f3, $a3a35dfe, $404080c0, $8f8f058a, $92923fad, $9d9d21bc, $38387048, $f5f5f104,
    $bcbc63df, $b6b677c1, $dadaaf75, $21214263, $10102030, $ffffe51a, $f3f3fd0e, $d2d2bf6d,
    $cdcd814c, $0c0c1814, $13132635, $ececc32f, $5f5fbee1, $979735a2, $444488cc, $17172e39,
    $c4c49357, $a7a755f2, $7e7efc82, $3d3d7a47, $6464c8ac, $5d5dbae7, $1919322b, $7373e695,
    $6060c0a0, $81811998, $4f4f9ed1, $dcdca37f, $22224466, $2a2a547e, $90903bab, $88880b83,
    $46468cca, $eeeec729, $b8b86bd3, $1414283c, $dedea779, $5e5ebce2, $0b0b161d, $dbdbad76,
    $e0e0db3b, $32326456, $3a3a744e, $0a0a141e, $494992db, $06060c0a, $2424486c, $5c5cb8e4,
    $c2c29f5d, $d3d3bd6e, $acac43ef, $6262c4a6, $919139a8, $959531a4, $e4e4d337, $7979f28b,
    $e7e7d532, $c8c88b43, $37376e59, $6d6ddab7, $8d8d018c, $d5d5b164, $4e4e9cd2, $a9a949e0,
    $6c6cd8b4, $5656acfa, $f4f4f307, $eaeacf25, $6565caaf, $7a7af48e, $aeae47e9, $08081018,
    $baba6fd5, $7878f088, $25254a6f, $2e2e5c72, $1c1c3824, $a6a657f1, $b4b473c7, $c6c69751,
    $e8e8cb23, $dddda17c, $7474e89c, $1f1f3e21, $4b4b96dd, $bdbd61dc, $8b8b0d86, $8a8a0f85,
    $7070e090, $3e3e7c42, $b5b571c4, $6666ccaa, $484890d8, $03030605, $f6f6f701, $0e0e1c12,
    $6161c2a3, $35356a5f, $5757aef9, $b9b969d0, $86861791, $c1c19958, $1d1d3a27, $9e9e27b9,
    $e1e1d938, $f8f8eb13, $98982bb3, $11112233, $6969d2bb, $d9d9a970, $8e8e0789, $949433a7,
    $9b9b2db6, $1e1e3c22, $87871592, $e9e9c920, $cece8749, $5555aaff, $28285078, $dfdfa57a,
    $8c8c038f, $a1a159f8, $89890980, $0d0d1a17, $bfbf65da, $e6e6d731, $424284c6, $6868d0b8,
    $414182c3, $999929b0, $2d2d5a77, $0f0f1e11, $b0b07bcb, $5454a8fc, $bbbb6dd6, $16162c3a);

  Te2: array[byte] of cardinal =
   ($63c6a563, $7cf8847c, $77ee9977, $7bf68d7b, $f2ff0df2, $6bd6bd6b, $6fdeb16f, $c59154c5,
    $30605030, $01020301, $67cea967, $2b567d2b, $fee719fe, $d7b562d7, $ab4de6ab, $76ec9a76,
    $ca8f45ca, $821f9d82, $c98940c9, $7dfa877d, $faef15fa, $59b2eb59, $478ec947, $f0fb0bf0,
    $ad41ecad, $d4b367d4, $a25ffda2, $af45eaaf, $9c23bf9c, $a453f7a4, $72e49672, $c09b5bc0,
    $b775c2b7, $fde11cfd, $933dae93, $264c6a26, $366c5a36, $3f7e413f, $f7f502f7, $cc834fcc,
    $34685c34, $a551f4a5, $e5d134e5, $f1f908f1, $71e29371, $d8ab73d8, $31625331, $152a3f15,
    $04080c04, $c79552c7, $23466523, $c39d5ec3, $18302818, $9637a196, $050a0f05, $9a2fb59a,
    $070e0907, $12243612, $801b9b80, $e2df3de2, $ebcd26eb, $274e6927, $b27fcdb2, $75ea9f75,
    $09121b09, $831d9e83, $2c58742c, $1a342e1a, $1b362d1b, $6edcb26e, $5ab4ee5a, $a05bfba0,
    $52a4f652, $3b764d3b, $d6b761d6, $b37dceb3, $29527b29, $e3dd3ee3, $2f5e712f, $84139784,
    $53a6f553, $d1b968d1, $00000000, $edc12ced, $20406020, $fce31ffc, $b179c8b1, $5bb6ed5b,
    $6ad4be6a, $cb8d46cb, $be67d9be, $39724b39, $4a94de4a, $4c98d44c, $58b0e858, $cf854acf,
    $d0bb6bd0, $efc52aef, $aa4fe5aa, $fbed16fb, $4386c543, $4d9ad74d, $33665533, $85119485,
    $458acf45, $f9e910f9, $02040602, $7ffe817f, $50a0f050, $3c78443c, $9f25ba9f, $a84be3a8,
    $51a2f351, $a35dfea3, $4080c040, $8f058a8f, $923fad92, $9d21bc9d, $38704838, $f5f104f5,
    $bc63dfbc, $b677c1b6, $daaf75da, $21426321, $10203010, $ffe51aff, $f3fd0ef3, $d2bf6dd2,
    $cd814ccd, $0c18140c, $13263513, $ecc32fec, $5fbee15f, $9735a297, $4488cc44, $172e3917,
    $c49357c4, $a755f2a7, $7efc827e, $3d7a473d, $64c8ac64, $5dbae75d, $19322b19, $73e69573,
    $60c0a060, $81199881, $4f9ed14f, $dca37fdc, $22446622, $2a547e2a, $903bab90, $880b8388,
    $468cca46, $eec729ee, $b86bd3b8, $14283c14, $dea779de, $5ebce25e, $0b161d0b, $dbad76db,
    $e0db3be0, $32645632, $3a744e3a, $0a141e0a, $4992db49, $060c0a06, $24486c24, $5cb8e45c,
    $c29f5dc2, $d3bd6ed3, $ac43efac, $62c4a662, $9139a891, $9531a495, $e4d337e4, $79f28b79,
    $e7d532e7, $c88b43c8, $376e5937, $6ddab76d, $8d018c8d, $d5b164d5, $4e9cd24e, $a949e0a9,
    $6cd8b46c, $56acfa56, $f4f307f4, $eacf25ea, $65caaf65, $7af48e7a, $ae47e9ae, $08101808,
    $ba6fd5ba, $78f08878, $254a6f25, $2e5c722e, $1c38241c, $a657f1a6, $b473c7b4, $c69751c6,
    $e8cb23e8, $dda17cdd, $74e89c74, $1f3e211f, $4b96dd4b, $bd61dcbd, $8b0d868b, $8a0f858a,
    $70e09070, $3e7c423e, $b571c4b5, $66ccaa66, $4890d848, $03060503, $f6f701f6, $0e1c120e,
    $61c2a361, $356a5f35, $57aef957, $b969d0b9, $86179186, $c19958c1, $1d3a271d, $9e27b99e,
    $e1d938e1, $f8eb13f8, $982bb398, $11223311, $69d2bb69, $d9a970d9, $8e07898e, $9433a794,
    $9b2db69b, $1e3c221e, $87159287, $e9c920e9, $ce8749ce, $55aaff55, $28507828, $dfa57adf,
    $8c038f8c, $a159f8a1, $89098089, $0d1a170d, $bf65dabf, $e6d731e6, $4284c642, $68d0b868,
    $4182c341, $9929b099, $2d5a772d, $0f1e110f, $b07bcbb0, $54a8fc54, $bb6dd6bb, $162c3a16);

  Te3: array[byte] of cardinal =
   ($c6a56363, $f8847c7c, $ee997777, $f68d7b7b, $ff0df2f2, $d6bd6b6b, $deb16f6f, $9154c5c5,
    $60503030, $02030101, $cea96767, $567d2b2b, $e719fefe, $b562d7d7, $4de6abab, $ec9a7676,
    $8f45caca, $1f9d8282, $8940c9c9, $fa877d7d, $ef15fafa, $b2eb5959, $8ec94747, $fb0bf0f0,
    $41ecadad, $b367d4d4, $5ffda2a2, $45eaafaf, $23bf9c9c, $53f7a4a4, $e4967272, $9b5bc0c0,
    $75c2b7b7, $e11cfdfd, $3dae9393, $4c6a2626, $6c5a3636, $7e413f3f, $f502f7f7, $834fcccc,
    $685c3434, $51f4a5a5, $d134e5e5, $f908f1f1, $e2937171, $ab73d8d8, $62533131, $2a3f1515,
    $080c0404, $9552c7c7, $46652323, $9d5ec3c3, $30281818, $37a19696, $0a0f0505, $2fb59a9a,
    $0e090707, $24361212, $1b9b8080, $df3de2e2, $cd26ebeb, $4e692727, $7fcdb2b2, $ea9f7575,
    $121b0909, $1d9e8383, $58742c2c, $342e1a1a, $362d1b1b, $dcb26e6e, $b4ee5a5a, $5bfba0a0,
    $a4f65252, $764d3b3b, $b761d6d6, $7dceb3b3, $527b2929, $dd3ee3e3, $5e712f2f, $13978484,
    $a6f55353, $b968d1d1, $00000000, $c12ceded, $40602020, $e31ffcfc, $79c8b1b1, $b6ed5b5b,
    $d4be6a6a, $8d46cbcb, $67d9bebe, $724b3939, $94de4a4a, $98d44c4c, $b0e85858, $854acfcf,
    $bb6bd0d0, $c52aefef, $4fe5aaaa, $ed16fbfb, $86c54343, $9ad74d4d, $66553333, $11948585,
    $8acf4545, $e910f9f9, $04060202, $fe817f7f, $a0f05050, $78443c3c, $25ba9f9f, $4be3a8a8,
    $a2f35151, $5dfea3a3, $80c04040, $058a8f8f, $3fad9292, $21bc9d9d, $70483838, $f104f5f5,
    $63dfbcbc, $77c1b6b6, $af75dada, $42632121, $20301010, $e51affff, $fd0ef3f3, $bf6dd2d2,
    $814ccdcd, $18140c0c, $26351313, $c32fecec, $bee15f5f, $35a29797, $88cc4444, $2e391717,
    $9357c4c4, $55f2a7a7, $fc827e7e, $7a473d3d, $c8ac6464, $bae75d5d, $322b1919, $e6957373,
    $c0a06060, $19988181, $9ed14f4f, $a37fdcdc, $44662222, $547e2a2a, $3bab9090, $0b838888,
    $8cca4646, $c729eeee, $6bd3b8b8, $283c1414, $a779dede, $bce25e5e, $161d0b0b, $ad76dbdb,
    $db3be0e0, $64563232, $744e3a3a, $141e0a0a, $92db4949, $0c0a0606, $486c2424, $b8e45c5c,
    $9f5dc2c2, $bd6ed3d3, $43efacac, $c4a66262, $39a89191, $31a49595, $d337e4e4, $f28b7979,
    $d532e7e7, $8b43c8c8, $6e593737, $dab76d6d, $018c8d8d, $b164d5d5, $9cd24e4e, $49e0a9a9,
    $d8b46c6c, $acfa5656, $f307f4f4, $cf25eaea, $caaf6565, $f48e7a7a, $47e9aeae, $10180808,
    $6fd5baba, $f0887878, $4a6f2525, $5c722e2e, $38241c1c, $57f1a6a6, $73c7b4b4, $9751c6c6,
    $cb23e8e8, $a17cdddd, $e89c7474, $3e211f1f, $96dd4b4b, $61dcbdbd, $0d868b8b, $0f858a8a,
    $e0907070, $7c423e3e, $71c4b5b5, $ccaa6666, $90d84848, $06050303, $f701f6f6, $1c120e0e,
    $c2a36161, $6a5f3535, $aef95757, $69d0b9b9, $17918686, $9958c1c1, $3a271d1d, $27b99e9e,
    $d938e1e1, $eb13f8f8, $2bb39898, $22331111, $d2bb6969, $a970d9d9, $07898e8e, $33a79494,
    $2db69b9b, $3c221e1e, $15928787, $c920e9e9, $8749cece, $aaff5555, $50782828, $a57adfdf,
    $038f8c8c, $59f8a1a1, $09808989, $1a170d0d, $65dabfbf, $d731e6e6, $84c64242, $d0b86868,
    $82c34141, $29b09999, $5a772d2d, $1e110f0f, $7bcbb0b0, $a8fc5454, $6dd6bbbb, $2c3a1616);


 type
  TSHAHash  = packed record
    A,B,C,D,E,F,G,H: cardinal; // will use A..E with TSHA1, A..H with TSHA256
  end;

  TSHAContext = packed record
    // Working hash
    Hash  : TSHAHash;
    // 64bit msg length
    MLen  : Int64;
    // Block buffer
    Buffer: array[0..63] of byte;
    // Index in buffer
    Index : integer;
  end;

{$ifdef CPU64}
function bswap32(a: cardinal): cardinal;
asm
  {$ifdef CPUX64}
  .NOFRAME
  {$endif}
  mov rax,rcx
  bswap eax
end;
{$else}
{$ifdef PUREPASCAL}
function bswap32(a: cardinal): cardinal; {$ifdef HASINLINE}inline;{$endif}
begin
  result := ((a and $ff)shl 24)or((a and $ff00)shl 8)or
            ((a and $ff0000)shr 8)or((a and $ff000000)shr 24);
end;
{$else}
function bswap32(a: cardinal): cardinal;
asm
  bswap eax
end;
{$endif}
{$endif}

{$ifdef PUREPASCAL}
procedure bswap256(s,d: PIntegerArray);
begin
  d[0] := bswap32(s[0]);
  d[1] := bswap32(s[1]);
  d[2] := bswap32(s[2]);
  d[3] := bswap32(s[3]);
  d[4] := bswap32(s[4]);
  d[5] := bswap32(s[5]);
  d[6] := bswap32(s[6]);
  d[7] := bswap32(s[7]);
end;

procedure bswap160(s,d: PIntegerArray);
begin                
  d[0] := bswap32(s[0]);
  d[1] := bswap32(s[1]);
  d[2] := bswap32(s[2]);
  d[3] := bswap32(s[3]);
  d[4] := bswap32(s[4]);
end;

{$else}

procedure bswap256(s,d: PIntegerArray);
asm
  push ebx
  mov ecx,eax // ecx=s, edx=d
  mov eax,[ecx]; mov ebx,[ecx+4]; bswap eax; bswap ebx; mov [edx],eax; mov [edx+4],ebx
  mov eax,[ecx+8]; mov ebx,[ecx+12]; bswap eax; bswap ebx; mov [edx+8],eax; mov [edx+12],ebx
  mov eax,[ecx+16]; mov ebx,[ecx+20]; bswap eax; bswap ebx; mov [edx+16],eax; mov [edx+20],ebx
  mov eax,[ecx+24]; mov ebx,[ecx+28]; bswap eax; bswap ebx; mov [edx+24],eax; mov [edx+28],ebx
  pop ebx
end;

procedure bswap160(s,d: PIntegerArray);
asm
  push ebx
  mov ecx,eax // ecx=s, edx=d
  mov eax,[ecx]; mov ebx,[ecx+4]; bswap eax; bswap ebx; mov [edx],eax; mov [edx+4],ebx
  mov eax,[ecx+8]; mov ebx,[ecx+12]; bswap eax; bswap ebx; mov [edx+8],eax; mov [edx+12],ebx
  mov eax,[ecx+16]; bswap eax; mov [edx+16],eax
  pop ebx
end;

{$endif}

function SHA256SelfTest: boolean;
function SingleTest(const s: RawByteString; const TDig: TSHA256Digest): boolean;
var SHA: TSHA256;
  Digest: TSHA256Digest;
  i: integer;
begin
  // 1. Hash complete RawByteString
  SHA.Full(pointer(s),length(s),Digest);
  result := CompareMem(@Digest,@TDig,sizeof(Digest));
  if not result then exit;
  // 2. one update call for all chars
  SHA.Init;
  for i := 1 to length(s) do
    SHA.Update(@s[i],1);
  SHA.Final(Digest);
  result := CompareMem(@Digest,@TDig,sizeof(Digest));
  // 3. test consistency with Padlock engine down results 
{$ifdef USEPADLOCK}
  if not result or not padlock_available then exit;
  padlock_available := false;  // force PadLock engine down
  SHA.Full(pointer(s),length(s),Digest);
  result := CompareMem(@Digest,@TDig,sizeof(Digest));
{$ifdef PADLOCKDEBUG} write('=padlock '); {$endif}
  padlock_available := true;
{$endif}
end;
var Digest: TSHA256Digest;
const
  D1: TSHA256Digest = ($ba,$78,$16,$bf,$8f,$01,$cf,$ea,$41,$41,$40,$de,$5d,$ae,$22,$23,
     $b0,$03,$61,$a3,$96,$17,$7a,$9c,$b4,$10,$ff,$61,$f2,$00,$15,$ad);
  D2: TSHA256Digest = ($24,$8d,$6a,$61,$d2,$06,$38,$b8,$e5,$c0,$26,$93,$0c,$3e,$60,$39,
     $a3,$3c,$e4,$59,$64,$ff,$21,$67,$f6,$ec,$ed,$d4,$19,$db,$06,$c1);
  D3: TSHA256Digest =
    ($94,$E4,$A9,$D9,$05,$31,$23,$1D,$BE,$D8,$7E,$D2,$E4,$F3,$5E,$4A,
     $0B,$F4,$B3,$BC,$CE,$EB,$17,$16,$D5,$77,$B1,$E0,$8B,$A9,$BA,$A3);
begin
//  result := true; exit;
  result := SingleTest('abc', D1) and
     SingleTest('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq', D2);
  if not result then exit;
  SHA256Weak('lagrangehommage',Digest); // test with len=256>64
  result := Comparemem(@Digest,@D3,sizeof(Digest));
end;

function MD5(const s: RawByteString): RawUTF8;
begin
  result := MD5DigestToString(MD5Buf(s[1],length(s)));
end;

function SHA1(const s: RawByteString): RawUTF8;
var SHA: TSHA1;
    Digest: TSHA1Digest;
begin
  SHA.Full(pointer(s),length(s),Digest);
  result := SHA1DigestToString(Digest);
end;

function SHA256(const s: RawByteString): RawUTF8;
var SHA: TSHA256;
    Digest: TSHA256Digest;
begin
  SHA.Full(pointer(s),length(s),Digest);
  result := SHA256DigestToString(Digest);
end;

function SHA1SelfTest: boolean;
function SingleTest(const s: RawByteString; TDig: TSHA1Digest): boolean;
var SHA: TSHA1;
    Digest: TSHA1Digest;
    i: integer;
begin
  // 1. Hash complete RawByteString
  SHA.Full(pointer(s),length(s),Digest);
  result := CompareMem(@Digest,@TDig,sizeof(Digest));
  if not result then exit;
  // 2. one update call for all chars
  for i := 1 to length(s) do
    SHA.Update(@s[i],1);
  SHA.Final(Digest);
  result := CompareMem(@Digest,@TDig,sizeof(Digest));
  // 3. test consistency with Padlock engine down results 
{$ifdef USEPADLOCK}
  if not result or not padlock_available then exit;
  padlock_available := false;  // force PadLock engine down
  SHA.Full(pointer(s),length(s),Digest);
  result := CompareMem(@Digest,@TDig,sizeof(Digest));
{$ifdef PADLOCKDEBUG} write('=padlock '); {$endif}
  padlock_available := true;
{$endif}
end;
const
  Test1Out: TSHA1Digest=
    ($A9,$99,$3E,$36,$47,$06,$81,$6A,$BA,$3E,$25,$71,$78,$50,$C2,$6C,$9C,$D0,$D8,$9D);
  Test2Out: TSHA1Digest=
    ($84,$98,$3E,$44,$1C,$3B,$D2,$6E,$BA,$AE,$4A,$A1,$F9,$51,$29,$E5,$E5,$46,$70,$F1);
var
  s: RawByteString;
  SHA: TSHA1;
  Digest: TSHA1Digest;
begin
  result := SingleTest('abc',Test1Out) and
    SingleTest('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq',Test2Out);
  if not result then exit;
  s := 'Wikipedia, l''encyclopedie libre et gratuite';
  SHA.Full(pointer(s),length(s),Digest);
  result := SHA1DigestToString(Digest)='c18cc65028bbdc147288a2d136313287782b9c73';
end;


{ TAES }

{$define AES_ROLLED}
// if defined, use rolled version, which is faster (at least on my AMD CPU)

{$ifdef PUREPASCAL}
  {$define PURE_PASCAL} // AES128 unrolled pascal(Delphi7)=57MB/s rolled asm=84MB/s :)
{$endif}

function AESSelfTest: boolean;
var A: TAES;
    st: RawByteString;
    Key: TSHA256Digest;
    s,b,p: TAESBlock;
    i,k,ks: integer;
begin
  // ensure that we have $2000 bytes of contiguous XOR tables ;)
  result := (cardinal(@TD0)+$400=cardinal(@TD1))and(cardinal(@TD0)+$800=cardinal(@TD2))
    and(cardinal(@TD0)+$C00=cardinal(@TD3))and(cardinal(@TD0)+$1000=cardinal(@TE0))
    and(cardinal(@TD0)+$1400=cardinal(@TE1))and(cardinal(@TD0)+$1800=cardinal(@TE2))
    and(cardinal(@TD0)+$1C00=cardinal(@TE3));
  if not result then exit;
  // test
  result := false;
  Randomize;
  st := '1234essai';
  PInteger(@st[1])^ := Random(MaxInt); 
  for k := 0 to 2 do begin
    ks := 128+k*64; // test keysize of 128,192 and 256 bits
//    write('Test AES ',ks);
    for i := 1 to 100 do begin
      SHA256Weak(st,Key);
      move(Key,s,16);
      A.EncryptInit(Key,ks);
      A.Encrypt(s,b);
      A.Done;
      A.DecryptInit(Key,ks);
      A.Decrypt(b,p);
      A.Done;
      if not CompareMem(@p,@s,AESBLockSize) then begin
        write(' AESSelfTest compareError'); exit; end;
      st := st+AnsiChar(Random(255));
    end;
  end;
  result := true;
end;

procedure TAES.Encrypt(var B: TAESBlock);
begin
  Encrypt(B,B);
end;

{$ifndef CONDITIONALEXPRESSIONS}
  {$define PURE_PASCAL} // Delphi 5 internal asm is buggy :(
{$endif}

procedure TAES.Encrypt(const BI: TAESBlock; var BO: TAESBlock);
// encrypt one block: Context contains encryption key
{$ifdef PURE_PASCAL}
{ PURE_PASCAL version (c) Wolfgang Ehrhardt under zlib license:
 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:
 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in
    a product, an acknowledgment in the product documentation would be
    appreciated but is not required.
 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.
 3. This notice may not be removed or altered from any source distribution. }
var
  ctx: TAESContext absolute Context;
  s0,s1,s2,s3: cardinal; // TAESBlock s as separate variables
  t0,t1,t2: cardinal;    // TAESBlock t as separate variables
{$ifdef AES_ROLLED}
  i: integer;
  pK: PWA4;
{$else}
  t3: cardinal;
  pK: PAWk;              // pointer to loop rount key
{$endif}
begin
{$ifdef USEPADLOCK}
  if ctx.ViaCtx<>nil then begin
    padlock_aes_encrypt(ctx.ViaCtx,@BI,@BO,16);
    exit;
  end;
{$endif}
  // Setup key pointer
  pK := PWA4(@ctx.RK);
  // Initialize with input block
  s0 := TWA4(BI)[0] xor pK^[0];
  s1 := TWA4(BI)[1] xor pK^[1];
  s2 := TWA4(BI)[2] xor pK^[2];
  s3 := TWA4(BI)[3] xor pK^[3];
{$ifdef AES_ROLLED}
  // Wolfgang Ehrhardt rolled version - faster on modern CPU than unrolled one below
  Inc(PK);
  for I := 1 to ctx.Rounds-1 do begin
      t0 := Te0[s0 and $ff] xor Te1[s1 shr 8 and $ff] xor Te2[s2 shr 16 and $ff] xor Te3[s3 shr 24];
      t1 := Te0[s1 and $ff] xor Te1[s2 shr 8 and $ff] xor Te2[s3 shr 16 and $ff] xor Te3[s0 shr 24];
      t2 := Te0[s2 and $ff] xor Te1[s3 shr 8 and $ff] xor Te2[s0 shr 16 and $ff] xor Te3[s1 shr 24];
      s3 := Te0[s3 and $ff] xor Te1[s0 shr 8 and $ff] xor Te2[s1 shr 16 and $ff] xor Te3[s2 shr 24] xor PK[3];
      s0 := t0 xor PK[0];
      s1 := t1 xor PK[1];
      s2 := t2 xor PK[2];
      Inc(pK);
    end;
  TWA4(BO)[0] := ((SBox[s0        and $ff])        xor
                  (SBox[s1 shr  8 and $ff]) shl  8 xor
                  (SBox[s2 shr 16 and $ff]) shl 16 xor
                  (SBox[s3 shr 24])         shl 24    ) xor pK^[0];
  TWA4(BO)[1] := ((SBox[s1        and $ff])        xor
                  (SBox[s2 shr  8 and $ff]) shl  8 xor
                  (SBox[s3 shr 16 and $ff]) shl 16 xor
                  (SBox[s0 shr 24])         shl 24    ) xor pK^[1];
  TWA4(BO)[2] := ((SBox[s2        and $ff])        xor
                  (SBox[s3 shr  8 and $ff]) shl  8 xor
                  (SBox[s0 shr 16 and $ff]) shl 16 xor
                  (SBox[s1 shr 24])         shl 24    ) xor pK^[2];
  TWA4(BO)[3] := ((SBox[s3        and $ff])        xor
                  (SBox[s0 shr  8 and $ff]) shl  8 xor
                  (SBox[s1 shr 16 and $ff]) shl 16 xor
                  (SBox[s2 shr 24])         shl 24    ) xor pK^[3];
{$else} // unrolled version (WE6) from Wolfgang Ehrhardt - slower
  // Round 1
  t0 := Te0[s0 and $ff] xor Te1[s1 shr 8 and $ff] xor Te2[s2 shr 16 and $ff] xor Te3[s3 shr 24] xor pK^[4];
  t1 := Te0[s1 and $ff] xor Te1[s2 shr 8 and $ff] xor Te2[s3 shr 16 and $ff] xor Te3[s0 shr 24] xor pK^[5];
  t2 := Te0[s2 and $ff] xor Te1[s3 shr 8 and $ff] xor Te2[s0 shr 16 and $ff] xor Te3[s1 shr 24] xor pK^[6];
  t3 := Te0[s3 and $ff] xor Te1[s0 shr 8 and $ff] xor Te2[s1 shr 16 and $ff] xor Te3[s2 shr 24] xor pK^[7];
  // Round 2
  s0 := Te0[t0 and $ff] xor Te1[t1 shr 8 and $ff] xor Te2[t2 shr 16 and $ff] xor Te3[t3 shr 24] xor pK^[8];
  s1 := Te0[t1 and $ff] xor Te1[t2 shr 8 and $ff] xor Te2[t3 shr 16 and $ff] xor Te3[t0 shr 24] xor pK^[9];
  s2 := Te0[t2 and $ff] xor Te1[t3 shr 8 and $ff] xor Te2[t0 shr 16 and $ff] xor Te3[t1 shr 24] xor pK^[10];
  s3 := Te0[t3 and $ff] xor Te1[t0 shr 8 and $ff] xor Te2[t1 shr 16 and $ff] xor Te3[t2 shr 24] xor pK^[11];
  // Round 3
  t0 := Te0[s0 and $ff] xor Te1[s1 shr 8 and $ff] xor Te2[s2 shr 16 and $ff] xor Te3[s3 shr 24] xor pK^[12];
  t1 := Te0[s1 and $ff] xor Te1[s2 shr 8 and $ff] xor Te2[s3 shr 16 and $ff] xor Te3[s0 shr 24] xor pK^[13];
  t2 := Te0[s2 and $ff] xor Te1[s3 shr 8 and $ff] xor Te2[s0 shr 16 and $ff] xor Te3[s1 shr 24] xor pK^[14];
  t3 := Te0[s3 and $ff] xor Te1[s0 shr 8 and $ff] xor Te2[s1 shr 16 and $ff] xor Te3[s2 shr 24] xor pK^[15];
  // Round 4
  s0 := Te0[t0 and $ff] xor Te1[t1 shr 8 and $ff] xor Te2[t2 shr 16 and $ff] xor Te3[t3 shr 24] xor pK^[16];
  s1 := Te0[t1 and $ff] xor Te1[t2 shr 8 and $ff] xor Te2[t3 shr 16 and $ff] xor Te3[t0 shr 24] xor pK^[17];
  s2 := Te0[t2 and $ff] xor Te1[t3 shr 8 and $ff] xor Te2[t0 shr 16 and $ff] xor Te3[t1 shr 24] xor pK^[18];
  s3 := Te0[t3 and $ff] xor Te1[t0 shr 8 and $ff] xor Te2[t1 shr 16 and $ff] xor Te3[t2 shr 24] xor pK^[19];
  // Round 5
  t0 := Te0[s0 and $ff] xor Te1[s1 shr 8 and $ff] xor Te2[s2 shr 16 and $ff] xor Te3[s3 shr 24] xor pK^[20];
  t1 := Te0[s1 and $ff] xor Te1[s2 shr 8 and $ff] xor Te2[s3 shr 16 and $ff] xor Te3[s0 shr 24] xor pK^[21];
  t2 := Te0[s2 and $ff] xor Te1[s3 shr 8 and $ff] xor Te2[s0 shr 16 and $ff] xor Te3[s1 shr 24] xor pK^[22];
  t3 := Te0[s3 and $ff] xor Te1[s0 shr 8 and $ff] xor Te2[s1 shr 16 and $ff] xor Te3[s2 shr 24] xor pK^[23];
  // Round 6
  s0 := Te0[t0 and $ff] xor Te1[t1 shr 8 and $ff] xor Te2[t2 shr 16 and $ff] xor Te3[t3 shr 24] xor pK^[24];
  s1 := Te0[t1 and $ff] xor Te1[t2 shr 8 and $ff] xor Te2[t3 shr 16 and $ff] xor Te3[t0 shr 24] xor pK^[25];
  s2 := Te0[t2 and $ff] xor Te1[t3 shr 8 and $ff] xor Te2[t0 shr 16 and $ff] xor Te3[t1 shr 24] xor pK^[26];
  s3 := Te0[t3 and $ff] xor Te1[t0 shr 8 and $ff] xor Te2[t1 shr 16 and $ff] xor Te3[t2 shr 24] xor pK^[27];
  // Round 7
  t0 := Te0[s0 and $ff] xor Te1[s1 shr 8 and $ff] xor Te2[s2 shr 16 and $ff] xor Te3[s3 shr 24] xor pK^[28];
  t1 := Te0[s1 and $ff] xor Te1[s2 shr 8 and $ff] xor Te2[s3 shr 16 and $ff] xor Te3[s0 shr 24] xor pK^[29];
  t2 := Te0[s2 and $ff] xor Te1[s3 shr 8 and $ff] xor Te2[s0 shr 16 and $ff] xor Te3[s1 shr 24] xor pK^[30];
  t3 := Te0[s3 and $ff] xor Te1[s0 shr 8 and $ff] xor Te2[s1 shr 16 and $ff] xor Te3[s2 shr 24] xor pK^[31];
  // Round 8
  s0 := Te0[t0 and $ff] xor Te1[t1 shr 8 and $ff] xor Te2[t2 shr 16 and $ff] xor Te3[t3 shr 24] xor pK^[32];
  s1 := Te0[t1 and $ff] xor Te1[t2 shr 8 and $ff] xor Te2[t3 shr 16 and $ff] xor Te3[t0 shr 24] xor pK^[33];
  s2 := Te0[t2 and $ff] xor Te1[t3 shr 8 and $ff] xor Te2[t0 shr 16 and $ff] xor Te3[t1 shr 24] xor pK^[34];
  s3 := Te0[t3 and $ff] xor Te1[t0 shr 8 and $ff] xor Te2[t1 shr 16 and $ff] xor Te3[t2 shr 24] xor pK^[35];
  // Round 9
  t0 := Te0[s0 and $ff] xor Te1[s1 shr 8 and $ff] xor Te2[s2 shr 16 and $ff] xor Te3[s3 shr 24] xor pK^[36];
  t1 := Te0[s1 and $ff] xor Te1[s2 shr 8 and $ff] xor Te2[s3 shr 16 and $ff] xor Te3[s0 shr 24] xor pK^[37];
  t2 := Te0[s2 and $ff] xor Te1[s3 shr 8 and $ff] xor Te2[s0 shr 16 and $ff] xor Te3[s1 shr 24] xor pK^[38];
  t3 := Te0[s3 and $ff] xor Te1[s0 shr 8 and $ff] xor Te2[s1 shr 16 and $ff] xor Te3[s2 shr 24] xor pK^[39];
  if ctx.rounds>10 then begin
    // Round 10
    s0 := Te0[t0 and $ff] xor Te1[t1 shr 8 and $ff] xor Te2[t2 shr 16 and $ff] xor Te3[t3 shr 24] xor pK^[40];
    s1 := Te0[t1 and $ff] xor Te1[t2 shr 8 and $ff] xor Te2[t3 shr 16 and $ff] xor Te3[t0 shr 24] xor pK^[41];
    s2 := Te0[t2 and $ff] xor Te1[t3 shr 8 and $ff] xor Te2[t0 shr 16 and $ff] xor Te3[t1 shr 24] xor pK^[42];
    s3 := Te0[t3 and $ff] xor Te1[t0 shr 8 and $ff] xor Te2[t1 shr 16 and $ff] xor Te3[t2 shr 24] xor pK^[43];
    // Round 11
    t0 := Te0[s0 and $ff] xor Te1[s1 shr 8 and $ff] xor Te2[s2 shr 16 and $ff] xor Te3[s3 shr 24] xor pK^[44];
    t1 := Te0[s1 and $ff] xor Te1[s2 shr 8 and $ff] xor Te2[s3 shr 16 and $ff] xor Te3[s0 shr 24] xor pK^[45];
    t2 := Te0[s2 and $ff] xor Te1[s3 shr 8 and $ff] xor Te2[s0 shr 16 and $ff] xor Te3[s1 shr 24] xor pK^[46];
    t3 := Te0[s3 and $ff] xor Te1[s0 shr 8 and $ff] xor Te2[s1 shr 16 and $ff] xor Te3[s2 shr 24] xor pK^[47];
    if ctx.rounds>12 then begin
      // Round 12
      s0 := Te0[t0 and $ff] xor Te1[t1 shr 8 and $ff] xor Te2[t2 shr 16 and $ff] xor Te3[t3 shr 24] xor pK^[48];
      s1 := Te0[t1 and $ff] xor Te1[t2 shr 8 and $ff] xor Te2[t3 shr 16 and $ff] xor Te3[t0 shr 24] xor pK^[49];
      s2 := Te0[t2 and $ff] xor Te1[t3 shr 8 and $ff] xor Te2[t0 shr 16 and $ff] xor Te3[t1 shr 24] xor pK^[50];
      s3 := Te0[t3 and $ff] xor Te1[t0 shr 8 and $ff] xor Te2[t1 shr 16 and $ff] xor Te3[t2 shr 24] xor pK^[51];
      // Round 13
      t0 := Te0[s0 and $ff] xor Te1[s1 shr 8 and $ff] xor Te2[s2 shr 16 and $ff] xor Te3[s3 shr 24] xor pK^[52];
      t1 := Te0[s1 and $ff] xor Te1[s2 shr 8 and $ff] xor Te2[s3 shr 16 and $ff] xor Te3[s0 shr 24] xor pK^[53];
      t2 := Te0[s2 and $ff] xor Te1[s3 shr 8 and $ff] xor Te2[s0 shr 16 and $ff] xor Te3[s1 shr 24] xor pK^[54];
      t3 := Te0[s3 and $ff] xor Te1[s0 shr 8 and $ff] xor Te2[s1 shr 16 and $ff] xor Te3[s2 shr 24] xor pK^[55];
    end;
  end;
  inc(cardinal(pK), (ctx.rounds shl 4));

  TWA4(BO)[0] := ((SBox[t0        and $ff])        xor
                  (SBox[t1 shr  8 and $ff]) shl  8 xor
                  (SBox[t2 shr 16 and $ff]) shl 16 xor
                  (SBox[t3 shr 24])         shl 24    ) xor pK^[0];
  TWA4(BO)[1] := ((SBox[t1        and $ff])        xor
                  (SBox[t2 shr  8 and $ff]) shl  8 xor
                  (SBox[t3 shr 16 and $ff]) shl 16 xor
                  (SBox[t0 shr 24])         shl 24    ) xor pK^[1];
  TWA4(BO)[2] := ((SBox[t2        and $ff])        xor
                  (SBox[t3 shr  8 and $ff]) shl  8 xor
                  (SBox[t0 shr 16 and $ff]) shl 16 xor
                  (SBox[t1 shr 24])         shl 24    ) xor pK^[2];
  TWA4(BO)[3] := ((SBox[t3        and $ff])        xor
                  (SBox[t0 shr  8 and $ff]) shl  8 xor
                  (SBox[t1 shr 16 and $ff]) shl 16 xor
                  (SBox[t2 shr 24])         shl 24    ) xor pK^[3];
{$endif}
end;
{$else}
asm // rolled optimized encryption asm version by A. Bouchez
  // eax=TAES(self) edx=BI ecx=BO
{$ifdef USEPADLOCK}
  cmp dword [eax].TAESContext.ViaCtx,0
  jz @DoAsm
  mov eax,[eax].TAESContext.ViaCtx
  push 16
  push ecx
  push edx
  push eax           // padlock_aes_encrypt(ctx.ViaCtx,@BI,@BO,16);
{$ifdef USEPADLOCKDLL}
  call dword ptr [padlock_aes_encrypt] {$else}
  call padlock_aes_encrypt
{$endif}
  add esp,16 // cdecl -> caller must restore stack
  ret
@DoAsm: {$endif}
  push ebx
  push esi
  push edi
  push ebp
  add esp,-24
  mov [esp+4],ecx
  mov ecx,eax // ecx=pk
  movzx eax,byte ptr [eax+$112] // round count
  dec eax
  mov [esp+20],eax
  mov ebx,[edx]
  xor ebx,[ecx]
  mov esi,[edx+4]
  xor esi,[ecx+4]
  mov eax,[edx+8]
  xor eax,[ecx+8]
  mov edx,[edx+12]
  xor edx,[ecx+12]
  lea ecx,ecx+16
@1: // pK=ecx s0=ebx s1=esi s2=eax s3=edx
  movzx edi,bl
  mov edi,[4*edi+te0]
  movzx ebp,si
  shr ebp,$08
  xor edi,[4*ebp+te1]
  mov ebp,eax
  shr ebp,$10
  and ebp,$ff
  xor edi,[4*ebp+te2]
  mov ebp,edx
  shr ebp,$18
  xor edi,[4*ebp+te3]
  mov [esp+8],edi
  mov edi,esi
  and edi,255
  mov edi,[4*edi+te0]
  movzx ebp,ax
  shr ebp,$08
  xor edi,[4*ebp+te1]
  mov ebp,edx
  shr ebp,$10
  and ebp,255
  xor edi,[4*ebp+te2]
  mov ebp,ebx
  shr ebp,$18
  xor edi,[4*ebp+te3]
  mov [esp+12],edi
  movzx edi,al
  mov edi,[4*edi+te0]
  movzx ebp,dh
  xor edi,[4*ebp+te1]
  mov ebp,ebx
  shr ebp,$10
  and ebp,255
  xor edi,[4*ebp+te2]
  mov ebp,esi
  shr ebp,$18
  xor edi,[4*ebp+te3]
  mov [esp+16],edi
  and edx,255
  mov edx,[4*edx+te0]
  shr ebx,$08
  and ebx,255
  xor edx,[4*ebx+te1]
  shr esi,$10
  and esi,255
  xor edx,[4*esi+te2]
  shr eax,$18
  xor edx,[4*eax+te3]
  mov ebx,[ecx]
  xor ebx,[esp+8]
  mov esi,[ecx+4]
  xor esi,[esp+12]
  mov eax,[ecx+8]
  xor eax,[esp+16]
  xor edx,[ecx+12]
  lea ecx,ecx+16
  dec byte ptr [esp+20]
  jne @1

  mov ebp,ecx // ebp=pk
  movzx ecx,bl
  mov edi,esi
  movzx ecx,byte ptr [ecx+SBox]
  shr edi,$08
  and edi,255
  movzx edi,byte ptr [edi+SBox]
  shl edi,$08
  xor ecx,edi
  mov edi,eax
  shr edi,$10
  and edi,255
  movzx edi,byte ptr [edi+SBox]
  shl edi,$10
  xor ecx,edi
  mov edi,edx
  shr edi,$18
  movzx edi,byte ptr [edi+SBox]
  shl edi,$18
  xor ecx,edi
  xor ecx,[ebp]
  mov edi,[esp+4]
  mov [edi],ecx
  mov ecx,esi
  and ecx,255
  movzx ecx,byte ptr [ecx+SBox]
  movzx edi,ah
  movzx edi,byte ptr [edi+SBox]
  shl edi,$08
  xor ecx,edi
  mov edi,edx
  shr edi,$10
  and edi,255
  movzx edi,byte ptr [edi+SBox]
  shl edi,$10
  xor ecx,edi
  mov edi,ebx
  shr edi,$18
  movzx edi,byte ptr [edi+SBox]
  shl edi,$18
  xor ecx,edi
  xor ecx,[ebp+4]
  mov edi,[esp+4]
  mov [edi+4],ecx
  mov ecx,eax
  and ecx,255
  movzx ecx,byte ptr [ecx+SBox]
  movzx edi,dh
  movzx edi,byte ptr [edi+SBox]
  shl edi,$08
  xor ecx,edi
  mov edi,ebx
  shr edi,$10
  and edi,255
  movzx edi,byte ptr [edi+SBox]
  shl edi,$10
  xor ecx,edi
  mov edi,esi
  shr edi,$18
  movzx edi,byte ptr [edi+SBox]
  shl edi,$18
  xor ecx,edi
  xor ecx,[ebp+8]
  mov edi,[esp+4]
  mov [edi+8],ecx
  and edx,255
  movzx edx,byte ptr [edx+SBox]
  shr ebx,$08
  and ebx,255
  xor ecx,ecx
  mov cl,byte ptr [ebx+SBox]
  shl ecx,$08
  xor edx,ecx
  shr esi,$10
  and esi,255
  xor ecx,ecx
  mov cl,byte ptr [esi+SBox]
  shl ecx,$10
  xor edx,ecx
  shr eax,$18
  movzx eax,byte ptr [eax+SBox]
  shl eax,$18
  xor edx,eax
  xor edx,[ebp+12]
  mov eax,[esp+4]
  mov [eax+12],edx
  add esp,24
  pop ebp
  pop edi
  pop esi
  pop ebx
end;
{$endif}

{$ifdef USEPADLOCK}
function TAES.DoPadlockInit(const Key; KeySize: cardinal): boolean;
var ctx: TAESContext absolute Context;
  KL: integer;
begin
  result := false;
  ctx.ViaCtx := nil; // object has not set it to nil
  if not padlock_available then // dll/so and CPU are present ?
    exit;
  case KeySize of
    128: KL := KEY_128BITS;
    192: KL := KEY_192BITS;
    256: KL := KEY_256BITS;
    else exit;
  end;
  ctx.ViaCtx := padlock_aes_begin;
  if ctx.ViaCtx<>nil then
  if (padlock_aes_setkey(ctx.ViaCtx,Key,KL)=AES_SUCCEEDED) and
     (padlock_aes_setmodeiv(ctx.ViaCtx,ACE_AES_ECB,ctx.IV)=AES_SUCCEEDED) then
    result := true else begin
    padlock_aes_close(ctx.ViaCtx);
    ctx.ViaCtx := nil;
  end;
end;
{$endif}

function TAES.EncryptInit(const Key; KeySize: cardinal): boolean;
procedure Shift(KeySize: cardinal; pk: PAWK);
var i: integer;
    temp: cardinal;
begin
  // 32 bit use shift and mask
  case KeySize of
  128:
    for i := 0 to 9 do begin
      temp := pK^[3];
      // SubWord(RotWord(temp)) if "word" count mod 4 = 0
      pK^[4] := ((SBox[(temp shr  8) and $ff])       ) xor
                ((SBox[(temp shr 16) and $ff]) shl  8) xor
                ((SBox[(temp shr 24)        ]) shl 16) xor
                ((SBox[(temp       ) and $ff]) shl 24) xor
                pK^[0] xor RCon[i];
      pK^[5] := pK^[1] xor pK^[4];
      pK^[6] := pK^[2] xor pK^[5];
      pK^[7] := pK^[3] xor pK^[6];
      inc(PtrUInt(pK),4*4);
    end;
  192:
    for i := 0 to 7 do begin
      temp := pK^[5];
      // SubWord(RotWord(temp)) if "word" count mod 6 = 0
      pK^[ 6] := ((SBox[(temp shr  8) and $ff])       ) xor
                 ((SBox[(temp shr 16) and $ff]) shl  8) xor
                 ((SBox[(temp shr 24)        ]) shl 16) xor
                 ((SBox[(temp       ) and $ff]) shl 24) xor
                 pK^[0] xor RCon[i];
      pK^[ 7] := pK^[1] xor pK^[6];
      pK^[ 8] := pK^[2] xor pK^[7];
      pK^[ 9] := pK^[3] xor pK^[8];
      if i=7 then exit;
      pK^[10] := pK^[4] xor pK^[ 9];
      pK^[11] := pK^[5] xor pK^[10];
      inc(PtrUInt(pK),6*4);
    end;
  else
    for i := 0 to 6 do begin
      temp := pK^[7];
      // SubWord(RotWord(temp)) if "word" count mod 8 = 0
      pK^[ 8] := ((SBox[(temp shr  8) and $ff])       ) xor
                 ((SBox[(temp shr 16) and $ff]) shl  8) xor
                 ((SBox[(temp shr 24)        ]) shl 16) xor
                 ((SBox[(temp       ) and $ff]) shl 24) xor
                 pK^[0] xor RCon[i];
      pK^[ 9] := pK^[1] xor pK^[ 8];
      pK^[10] := pK^[2] xor pK^[ 9];
      pK^[11] := pK^[3] xor pK^[10];
      if i=6 then exit;
      temp := pK^[11];
      // SubWord(temp) if "word" count mod 8 = 4
      pK^[12] := ((SBox[(temp       ) and $ff])       ) xor
                 ((SBox[(temp shr  8) and $ff]) shl  8) xor
                 ((SBox[(temp shr 16) and $ff]) shl 16) xor
                 ((SBox[(temp shr 24)        ]) shl 24) xor
                 pK^[4];
      pK^[13] := pK^[5] xor pK^[12];
      pK^[14] := pK^[6] xor pK^[13];
      pK^[15] := pK^[7] xor pK^[14];
      inc(PtrUInt(pK),8*4);
    end;
  end;
end;
var
  Nk: word;
  ctx: TAESContext absolute Context;
begin
  result := true;
  Initialized := true;
{$ifdef USEPADLOCK}
  if DoPadlockInit(Key,KeySize) then
    exit; // Init OK
{$endif}
  with ctx do begin
    // Clear only the necessary context data at init. IV and buf
    // remain uninitialized, other fields are initialized below.
    bLen :=0;
{$ifdef USEPADLOCK}
    ctx.ViaCtx := nil;
{$endif}
  end;
  if (KeySize<>128) and (KeySize<>192) and (KeySize<>256) then begin
    result := false;
    Initialized := false;
    exit;
  end;
  Nk := KeySize div 32;
  Move(Key, ctx.RK, 4*Nk);
  ctx.KeyBits := KeySize;
  ctx.Rounds  := 6 + Nk;
  // Calculate encryption round keys
  Shift(KeySize,pointer(@ctx.RK));
end;

{$ifndef PURE_PASCAL}
{$define AES_ROLLED} // asm version is rolled 
{$endif}

function TAES.DecryptInit(const Key; KeySize: cardinal): boolean;
procedure MakeDecrKey(var ctx: TAESContext);
// Calculate decryption key from encryption key
var i: integer;
    x: cardinal;
{$ifndef AES_ROLLED}
    j: integer;
{$endif}
begin
{$ifndef AES_ROLLED} // inversion is needed only for fully unrolled version
  // invert the order of the round keys
  i := 0;
  j := 4*ctx.Rounds;
  while i<j do begin
    x:=TAWk(ctx.RK)[i  ];  TAWk(ctx.RK)[i  ]:=TAWk(ctx.RK)[j  ];  TAWk(ctx.RK)[j  ]:=x;
    x:=TAWk(ctx.RK)[i+1];  TAWk(ctx.RK)[i+1]:=TAWk(ctx.RK)[j+1];  TAWk(ctx.RK)[j+1]:=x;
    x:=TAWk(ctx.RK)[i+2];  TAWk(ctx.RK)[i+2]:=TAWk(ctx.RK)[j+2];  TAWk(ctx.RK)[j+2]:=x;
    x:=TAWk(ctx.RK)[i+3];  TAWk(ctx.RK)[i+3]:=TAWk(ctx.RK)[j+3];  TAWk(ctx.RK)[j+3]:=x;
    inc(i,4);
    dec(j,4);
  end;
{$endif}
  for i := 1 to ctx.Rounds-1 do begin
    x  := TAWk(ctx.RK)[i*4  ];
    TAWk(ctx.RK)[i*4  ] := Td3[SBox[x shr 24]] xor Td2[SBox[x shr 16 and $ff]]
      xor Td1[SBox[x shr 8 and $ff]] xor Td0[SBox[x and $ff]];
    x  := TAWk(ctx.RK)[i*4+1];
    TAWk(ctx.RK)[i*4+1] := Td3[SBox[x shr 24]] xor Td2[SBox[x shr 16 and $ff]]
      xor Td1[SBox[x shr 8 and $ff]] xor Td0[SBox[x and $ff]];
    x  := TAWk(ctx.RK)[i*4+2];
    TAWk(ctx.RK)[i*4+2] := Td3[SBox[x shr 24]] xor Td2[SBox[x shr 16 and $ff]]
      xor Td1[SBox[x shr 8 and $ff]] xor Td0[SBox[x and $ff]];
    x  := TAWk(ctx.RK)[i*4+3];
    TAWk(ctx.RK)[i*4+3] := Td3[SBox[x shr 24]] xor Td2[SBox[x shr 16 and $ff]]
      xor Td1[SBox[x shr 8 and $ff]] xor Td0[SBox[x and $ff]];
  end;
end;
var ctx: TAESContext absolute Context;
begin
{$ifdef USEPADLOCK}
  if DoPadlockInit(Key,KeySize) then begin
    result := true;
    Initialized := true;
    exit; // Init OK
  end;
{$endif}
  result := EncryptInit(Key, KeySize); // contains Initialized := true
  if not result then
    exit;
  MakeDecrKey(ctx);
end;

procedure TAES.Decrypt(var B: TAESBlock);
begin
  Decrypt(B,B);
end;

procedure TAES.Decrypt(const BI: TAESBlock; var BO: TAESBlock);
// decrypt one block (in ECB mode)
{$ifdef PURE_PASCAL}
var
  ctx: TAESContext absolute Context;
  s0,s1,s2,s3: cardinal;    {TAESBlock s as separate variables}
  t0,t1,t2: cardinal;    {TAESBlock t as separate variables}
{$ifdef AES_ROLLED}
  i: integer;
  pK: PWA4;
{$else}
  t3: cardinal;
  pK: PAWk;                 {pointer to loop rount key   }
{$endif}
begin
{$ifdef USEPADLOCK}
  if ctx.ViaCtx<>nil then begin
    padlock_aes_decrypt(ctx.ViaCtx,@BI,@BO,16);
    exit;
  end;
{$endif}
{$ifdef AES_ROLLED}
  // Wolfgang Ehrhardt rolled version - faster on modern CPU than unrolled one below
  // Setup key pointer
  pK := PWA4(@ctx.RK[ctx.Rounds]);
  // Initialize with input block
  s0 := TWA4(BI)[0] xor pK^[0];
  s1 := TWA4(BI)[1] xor pK^[1];
  s2 := TWA4(BI)[2] xor pK^[2];
  s3 := TWA4(BI)[3] xor pK^[3];
  dec(pK);
  for I := 1 to ctx.Rounds-1 do begin
      t0 := Td0[s0 and $ff] xor Td1[s3 shr 8 and $ff] xor Td2[s2 shr 16 and $ff] xor Td3[s1 shr 24];
      t1 := Td0[s1 and $ff] xor Td1[s0 shr 8 and $ff] xor Td2[s3 shr 16 and $ff] xor Td3[s2 shr 24];
      t2 := Td0[s2 and $ff] xor Td1[s1 shr 8 and $ff] xor Td2[s0 shr 16 and $ff] xor Td3[s3 shr 24];
      s3 := Td0[s3 and $ff] xor Td1[s2 shr 8 and $ff] xor Td2[s1 shr 16 and $ff] xor Td3[s0 shr 24] xor PK[3];
      s0 := t0 xor PK[0];
      s1 := t1 xor PK[1];
      s2 := t2 xor PK[2];
      dec(pK);
    end;
  TWA4(BO)[0] := ((InvSBox[s0        and $ff])        xor
                  (InvSBox[s3 shr  8 and $ff]) shl  8 xor
                  (InvSBox[s2 shr 16 and $ff]) shl 16 xor
                  (InvSBox[s1 shr 24])         shl 24    ) xor pK^[0];
  TWA4(BO)[1] := ((InvSBox[s1        and $ff])        xor
                  (InvSBox[s0 shr  8 and $ff]) shl  8 xor
                  (InvSBox[s3 shr 16 and $ff]) shl 16 xor
                  (InvSBox[s2 shr 24])         shl 24    ) xor pK^[1];
  TWA4(BO)[2] := ((InvSBox[s2        and $ff])        xor
                  (InvSBox[s1 shr  8 and $ff]) shl  8 xor
                  (InvSBox[s0 shr 16 and $ff]) shl 16 xor
                  (InvSBox[s3 shr 24])         shl 24    ) xor pK^[2];
  TWA4(BO)[3] := ((InvSBox[s3        and $ff])        xor
                  (InvSBox[s2 shr  8 and $ff]) shl  8 xor
                  (InvSBox[s1 shr 16 and $ff]) shl 16 xor
                  (InvSBox[s0 shr 24])         shl 24    ) xor pK^[3];
{$else} // unrolled version (WE6) from Wolfgang Ehrhardt - slower
  // Setup key pointer
  pK := PAWk(@ctx.RK);
  // Initialize with input block
  s0 := TWA4(BI)[0] xor pK^[0];
  s1 := TWA4(BI)[1] xor pK^[1];
  s2 := TWA4(BI)[2] xor pK^[2];
  s3 := TWA4(BI)[3] xor pK^[3];

  // Round 1
  t0 := Td0[s0 and $ff] xor Td1[s3 shr 8 and $ff] xor Td2[s2 shr 16 and $ff] xor Td3[s1 shr 24] xor pK^[4];
  t1 := Td0[s1 and $ff] xor Td1[s0 shr 8 and $ff] xor Td2[s3 shr 16 and $ff] xor Td3[s2 shr 24] xor pK^[5];
  t2 := Td0[s2 and $ff] xor Td1[s1 shr 8 and $ff] xor Td2[s0 shr 16 and $ff] xor Td3[s3 shr 24] xor pK^[6];
  t3 := Td0[s3 and $ff] xor Td1[s2 shr 8 and $ff] xor Td2[s1 shr 16 and $ff] xor Td3[s0 shr 24] xor pK^[7];
  // Round 2
  s0 := Td0[t0 and $ff] xor Td1[t3 shr 8 and $ff] xor Td2[t2 shr 16 and $ff] xor Td3[t1 shr 24] xor pK^[8];
  s1 := Td0[t1 and $ff] xor Td1[t0 shr 8 and $ff] xor Td2[t3 shr 16 and $ff] xor Td3[t2 shr 24] xor pK^[9];
  s2 := Td0[t2 and $ff] xor Td1[t1 shr 8 and $ff] xor Td2[t0 shr 16 and $ff] xor Td3[t3 shr 24] xor pK^[10];
  s3 := Td0[t3 and $ff] xor Td1[t2 shr 8 and $ff] xor Td2[t1 shr 16 and $ff] xor Td3[t0 shr 24] xor pK^[11];
  // Round 3
  t0 := Td0[s0 and $ff] xor Td1[s3 shr 8 and $ff] xor Td2[s2 shr 16 and $ff] xor Td3[s1 shr 24] xor pK^[12];
  t1 := Td0[s1 and $ff] xor Td1[s0 shr 8 and $ff] xor Td2[s3 shr 16 and $ff] xor Td3[s2 shr 24] xor pK^[13];
  t2 := Td0[s2 and $ff] xor Td1[s1 shr 8 and $ff] xor Td2[s0 shr 16 and $ff] xor Td3[s3 shr 24] xor pK^[14];
  t3 := Td0[s3 and $ff] xor Td1[s2 shr 8 and $ff] xor Td2[s1 shr 16 and $ff] xor Td3[s0 shr 24] xor pK^[15];
  // Round 4
  s0 := Td0[t0 and $ff] xor Td1[t3 shr 8 and $ff] xor Td2[t2 shr 16 and $ff] xor Td3[t1 shr 24] xor pK^[16];
  s1 := Td0[t1 and $ff] xor Td1[t0 shr 8 and $ff] xor Td2[t3 shr 16 and $ff] xor Td3[t2 shr 24] xor pK^[17];
  s2 := Td0[t2 and $ff] xor Td1[t1 shr 8 and $ff] xor Td2[t0 shr 16 and $ff] xor Td3[t3 shr 24] xor pK^[18];
  s3 := Td0[t3 and $ff] xor Td1[t2 shr 8 and $ff] xor Td2[t1 shr 16 and $ff] xor Td3[t0 shr 24] xor pK^[19];
  // Round 5
  t0 := Td0[s0 and $ff] xor Td1[s3 shr 8 and $ff] xor Td2[s2 shr 16 and $ff] xor Td3[s1 shr 24] xor pK^[20];
  t1 := Td0[s1 and $ff] xor Td1[s0 shr 8 and $ff] xor Td2[s3 shr 16 and $ff] xor Td3[s2 shr 24] xor pK^[21];
  t2 := Td0[s2 and $ff] xor Td1[s1 shr 8 and $ff] xor Td2[s0 shr 16 and $ff] xor Td3[s3 shr 24] xor pK^[22];
  t3 := Td0[s3 and $ff] xor Td1[s2 shr 8 and $ff] xor Td2[s1 shr 16 and $ff] xor Td3[s0 shr 24] xor pK^[23];
  // Round 6
  s0 := Td0[t0 and $ff] xor Td1[t3 shr 8 and $ff] xor Td2[t2 shr 16 and $ff] xor Td3[t1 shr 24] xor pK^[24];
  s1 := Td0[t1 and $ff] xor Td1[t0 shr 8 and $ff] xor Td2[t3 shr 16 and $ff] xor Td3[t2 shr 24] xor pK^[25];
  s2 := Td0[t2 and $ff] xor Td1[t1 shr 8 and $ff] xor Td2[t0 shr 16 and $ff] xor Td3[t3 shr 24] xor pK^[26];
  s3 := Td0[t3 and $ff] xor Td1[t2 shr 8 and $ff] xor Td2[t1 shr 16 and $ff] xor Td3[t0 shr 24] xor pK^[27];
  // Round 7
  t0 := Td0[s0 and $ff] xor Td1[s3 shr 8 and $ff] xor Td2[s2 shr 16 and $ff] xor Td3[s1 shr 24] xor pK^[28];
  t1 := Td0[s1 and $ff] xor Td1[s0 shr 8 and $ff] xor Td2[s3 shr 16 and $ff] xor Td3[s2 shr 24] xor pK^[29];
  t2 := Td0[s2 and $ff] xor Td1[s1 shr 8 and $ff] xor Td2[s0 shr 16 and $ff] xor Td3[s3 shr 24] xor pK^[30];
  t3 := Td0[s3 and $ff] xor Td1[s2 shr 8 and $ff] xor Td2[s1 shr 16 and $ff] xor Td3[s0 shr 24] xor pK^[31];
  // Round 8
  s0 := Td0[t0 and $ff] xor Td1[t3 shr 8 and $ff] xor Td2[t2 shr 16 and $ff] xor Td3[t1 shr 24] xor pK^[32];
  s1 := Td0[t1 and $ff] xor Td1[t0 shr 8 and $ff] xor Td2[t3 shr 16 and $ff] xor Td3[t2 shr 24] xor pK^[33];
  s2 := Td0[t2 and $ff] xor Td1[t1 shr 8 and $ff] xor Td2[t0 shr 16 and $ff] xor Td3[t3 shr 24] xor pK^[34];
  s3 := Td0[t3 and $ff] xor Td1[t2 shr 8 and $ff] xor Td2[t1 shr 16 and $ff] xor Td3[t0 shr 24] xor pK^[35];
  // Round 9
  t0 := Td0[s0 and $ff] xor Td1[s3 shr 8 and $ff] xor Td2[s2 shr 16 and $ff] xor Td3[s1 shr 24] xor pK^[36];
  t1 := Td0[s1 and $ff] xor Td1[s0 shr 8 and $ff] xor Td2[s3 shr 16 and $ff] xor Td3[s2 shr 24] xor pK^[37];
  t2 := Td0[s2 and $ff] xor Td1[s1 shr 8 and $ff] xor Td2[s0 shr 16 and $ff] xor Td3[s3 shr 24] xor pK^[38];
  t3 := Td0[s3 and $ff] xor Td1[s2 shr 8 and $ff] xor Td2[s1 shr 16 and $ff] xor Td3[s0 shr 24] xor pK^[39];
  if ctx.rounds>10 then begin
    // Round 10
    s0 := Td0[t0 and $ff] xor Td1[t3 shr 8 and $ff] xor Td2[t2 shr 16 and $ff] xor Td3[t1 shr 24] xor pK^[40];
    s1 := Td0[t1 and $ff] xor Td1[t0 shr 8 and $ff] xor Td2[t3 shr 16 and $ff] xor Td3[t2 shr 24] xor pK^[41];
    s2 := Td0[t2 and $ff] xor Td1[t1 shr 8 and $ff] xor Td2[t0 shr 16 and $ff] xor Td3[t3 shr 24] xor pK^[42];
    s3 := Td0[t3 and $ff] xor Td1[t2 shr 8 and $ff] xor Td2[t1 shr 16 and $ff] xor Td3[t0 shr 24] xor pK^[43];
    // Round 11
    t0 := Td0[s0 and $ff] xor Td1[s3 shr 8 and $ff] xor Td2[s2 shr 16 and $ff] xor Td3[s1 shr 24] xor pK^[44];
    t1 := Td0[s1 and $ff] xor Td1[s0 shr 8 and $ff] xor Td2[s3 shr 16 and $ff] xor Td3[s2 shr 24] xor pK^[45];
    t2 := Td0[s2 and $ff] xor Td1[s1 shr 8 and $ff] xor Td2[s0 shr 16 and $ff] xor Td3[s3 shr 24] xor pK^[46];
    t3 := Td0[s3 and $ff] xor Td1[s2 shr 8 and $ff] xor Td2[s1 shr 16 and $ff] xor Td3[s0 shr 24] xor pK^[47];
    if ctx.rounds>12 then begin
      // Round 12
      s0 := Td0[t0 and $ff] xor Td1[t3 shr 8 and $ff] xor Td2[t2 shr 16 and $ff] xor Td3[t1 shr 24] xor pK^[48];
      s1 := Td0[t1 and $ff] xor Td1[t0 shr 8 and $ff] xor Td2[t3 shr 16 and $ff] xor Td3[t2 shr 24] xor pK^[49];
      s2 := Td0[t2 and $ff] xor Td1[t1 shr 8 and $ff] xor Td2[t0 shr 16 and $ff] xor Td3[t3 shr 24] xor pK^[50];
      s3 := Td0[t3 and $ff] xor Td1[t2 shr 8 and $ff] xor Td2[t1 shr 16 and $ff] xor Td3[t0 shr 24] xor pK^[51];
      // Round 13
      t0 := Td0[s0 and $ff] xor Td1[s3 shr 8 and $ff] xor Td2[s2 shr 16 and $ff] xor Td3[s1 shr 24] xor pK^[52];
      t1 := Td0[s1 and $ff] xor Td1[s0 shr 8 and $ff] xor Td2[s3 shr 16 and $ff] xor Td3[s2 shr 24] xor pK^[53];
      t2 := Td0[s2 and $ff] xor Td1[s1 shr 8 and $ff] xor Td2[s0 shr 16 and $ff] xor Td3[s3 shr 24] xor pK^[54];
      t3 := Td0[s3 and $ff] xor Td1[s2 shr 8 and $ff] xor Td2[s1 shr 16 and $ff] xor Td3[s0 shr 24] xor pK^[55];
    end;
  end;
  inc(cardinal(pK), (ctx.rounds shl 4));

  // Uses InvSbox and shl, needs type cast cardinal() for   
  // 16 bit compilers: here InvSbox is byte, Td4 is cardinal
  TWA4(BO)[0] := ((InvSBox[t0 and $ff]) xor
    (InvSBox[t3 shr  8 and $ff]) shl  8 xor
    (InvSBox[t2 shr 16 and $ff]) shl 16 xor
    (InvSBox[t1 shr 24]) shl 24) xor pK^[0];
  TWA4(BO)[1] := ((InvSBox[t1 and $ff]) xor
    (InvSBox[t0 shr  8 and $ff]) shl  8 xor
    (InvSBox[t3 shr 16 and $ff]) shl 16 xor
    (InvSBox[t2 shr 24]) shl 24) xor pK^[1];
  TWA4(BO)[2] := ((InvSBox[t2 and $ff]) xor
    (InvSBox[t1 shr  8 and $ff]) shl  8 xor
    (InvSBox[t0 shr 16 and $ff]) shl 16 xor
    (InvSBox[t3 shr 24]) shl 24) xor pK^[2];
  TWA4(BO)[3] := ((InvSBox[t3 and $ff]) xor
    (InvSBox[t2 shr  8 and $ff]) shl  8 xor
    (InvSBox[t1 shr 16 and $ff]) shl 16 xor
    (InvSBox[t0 shr 24]) shl 24) xor pK^[3];
{$endif}
end;
{$else}
asm // rolled optimized decryption asm version by A. Bouchez
  // eax=TAES(self) edx=BI ecx=BO
{$ifdef USEPADLOCK}
  cmp dword [eax].TAESContext.ViaCtx,0
  jz @DoAsm
  mov eax,[eax].TAESContext.ViaCtx
  push 16
  push ecx
  push edx
  push eax           // padlock_aes_decrypt(ctx.ViaCtx,@BI,@BO,16);
{$ifdef USEPADLOCKDLL}
  call dword ptr [padlock_aes_decrypt] {$else}
  call padlock_aes_decrypt
{$endif}
  add esp,16 // cdecl -> caller must restore stack
  ret
@DoAsm: {$endif}
  push ebx
  push esi
  push edi
  push ebp
  add esp,-20
  mov [esp],ecx
  movzx ecx,word ptr [eax+274]
  lea esi,4*ecx
  lea ecx,ecx-1
  lea eax,[eax+4*esi] // eax=@ctx.rk[ctx.rounds]=pk
  mov [esp+16],ecx // [esp+16]=ctx.round
  mov ebx,[edx]
  xor ebx,[eax]
  mov esi,[edx+4]
  xor esi,[eax+4]
  mov ecx,[edx+8]
  xor ecx,[eax+8]
  mov edx,[edx+12]
  xor edx,[eax+12]
  lea eax,eax-16
@1: // pk=eax s0=ebx s1=esi s2=ecx s3=edx
  movzx edi,bl
  mov edi,[4*edi+td0]
  movzx ebp,dh
  xor edi,[4*ebp+td1]
  mov ebp,ecx
  shr ebp,$10
  and ebp,255
  xor edi,[4*ebp+td2]
  mov ebp,esi
  shr ebp,$18
  xor edi,[4*ebp+td3]
  mov [esp+4],edi
  mov edi,esi
  and edi,255
  mov edi,[4*edi+td0]
  movzx ebp,bh
  xor edi,[4*ebp+td1]
  mov ebp,edx
  shr ebp,$10
  and ebp,255
  xor edi,[4*ebp+td2]
  mov ebp,ecx
  shr ebp,$18
  xor edi,[4*ebp+td3]
  mov [esp+8],edi
  movzx edi,cl
  mov edi,[4*edi+td0]
  movzx ebp,si
  shr ebp,$08
  xor edi,[4*ebp+td1]
  mov ebp,ebx
  shr ebp,$10
  and ebp,255
  xor edi,[4*ebp+td2]
  mov ebp,edx
  shr ebp,$18
  xor edi,[4*ebp+td3]
  mov [esp+12],edi
  and edx,255
  mov edx,[4*edx+td0]
  movzx ecx,ch
  xor edx,[4*ecx+td1]
  shr esi,$10
  and esi,255
  xor edx,[4*esi+td2]
  shr ebx,$18
  xor edx,[4*ebx+td3]
  xor edx,[eax+12]
  mov ebx,[eax]
  xor ebx,[esp+4]
  mov esi,[eax+4]
  xor esi,[esp+8]
  mov ecx,[eax+8]
  xor ecx,[esp+12]
  lea eax,eax-16
  dec byte ptr [esp+16]
  jnz @1

  mov ebp,eax
  movzx eax,bl
  movzx eax,byte ptr [eax+InvSBox]
  movzx edi,dh
  movzx edi,byte ptr [edi+InvSBox]
  shl edi,$08
  xor eax,edi
  mov edi,ecx
  shr edi,$10
  and edi,255
  movzx edi,byte ptr [edi+InvSBox]
  shl edi,$10
  xor eax,edi
  mov edi,esi
  shr edi,$18
  movzx edi,byte ptr [edi+InvSBox]
  shl edi,$18
  xor eax,edi
  xor eax,[ebp]
  mov edi,[esp]
  mov [edi],eax
  mov eax,esi
  and eax,255
  movzx eax,byte ptr [eax+InvSBox]
  movzx edi,bh
  movzx edi,byte ptr [edi+InvSBox]
  shl edi,$08
  xor eax,edi
  mov edi,edx
  shr edi,$10
  and edi,255
  movzx edi,byte ptr [edi+InvSBox]
  shl edi,$10
  xor eax,edi
  mov edi,ecx
  shr edi,$18
  movzx edi,byte ptr [edi+InvSBox]
  shl edi,$18
  xor eax,edi
  xor eax,[ebp+4]
  mov edi,[esp]
  mov [edi+4],eax
  movzx eax,cl
  movzx eax,byte ptr [eax+InvSBox]
  movzx edi,si
  shr edi,$08
  movzx edi,byte ptr [edi+InvSBox]
  shl edi,$08
  xor eax,edi
  mov edi,ebx
  shr edi,$10
  and edi,255
  movzx edi,byte ptr [edi+InvSBox]
  shl edi,$10
  xor eax,edi
  mov edi,edx
  shr edi,$18
  movzx edi,byte ptr [edi+InvSBox]
  shl edi,$18
  xor eax,edi
  xor eax,[ebp+8]
  mov edi,[esp]
  mov [edi+8],eax
  and edx,255
  movzx eax,byte ptr [edx+InvSBox]
  shr ecx,$08
  and ecx,255
  movzx edx,byte ptr [ecx+InvSBox]
  shl edx,$08
  xor eax,edx
  shr esi,$10
  and esi,255
  movzx edx,byte ptr [esi+InvSBox]
  shl edx,$10
  xor eax,edx
  shr ebx,$18
  movzx edx,byte ptr [ebx+InvSBox]
  shl edx,$18
  xor eax,edx
  xor eax,[ebp+12]
  mov [edi+12],eax
  add esp,20
  pop ebp
  pop edi
  pop esi
  pop ebx
end;
{$endif}
procedure TAES.DoBlocks(pIn, pOut: PAESBlock;
  out oIn, oOut: PAESBLock; Count: integer; doEncrypt: Boolean);
var i: integer;
    ctx: TAESContext absolute Context;
begin
{$ifdef USEPADLOCK}
//  assert(cardinal(pIn) and $F=0); // must be 16 bytes aligned
  if ctx.ViaCtx<>nil then begin
    if Count<>0 then begin
      Count := Count*AESBlockSize;
      if doEncrypt then
        padlock_aes_encrypt(ctx.ViaCtx,pIn,pOut,Count) else
        padlock_aes_decrypt(ctx.ViaCtx,pIn,pOut,Count);
    end;
    oIn := pointer(integer(pIn)+Count);
    oOut := pointer(integer(pOut)+Count);
    exit;
  end;
{$endif}
  if doEncrypt then
  for i := 1 to Count do begin
    Encrypt(pIn^,pOut^);
    inc(pIn);
    inc(pOut);
  end else
  for i := 1 to Count do begin
    Decrypt(pIn^,pOut^);
    inc(pIn);
    inc(pOut);
  end;
  oIn := pIn;
  oOut := pOut;
end;

function TAES.DoInit(const Key; KeySize: cardinal; doEncrypt: boolean): boolean;
begin
  if doEncrypt then
    result := EncryptInit(Key, KeySize) else
    result := DecryptInit(Key,KeySize);
end;

procedure TAES.DoBlocks(pIn, pOut: PAESBlock; Count: integer; doEncrypt: boolean);
begin
  DoBlocks(pIn,pOut,pIn,pOut,Count,doEncrypt);
end;



{$ifdef USEPADLOCK}
procedure TAES.Done;
var ctx: TAESContext absolute Context;
begin
  if initialized and padlock_available and (ctx.ViaCtx<>nil) then begin
    padlock_aes_close(ctx.ViaCtx);
    initialized := false;
    ctx.ViaCtx := nil;
  end;
end;
{$else}
procedure TAES.Done;
begin
end;
{$endif}

{$ifdef USETHREADSFORBIGAESBLOCKS}
type
  TThreadParams = record
    bIn, bOut: pAESBlock;
    BlockCount: integer;
    Encrypt: boolean;
    ID: DWORD;
    AES: TAES;
  end;

{ we use direct Windows threads, since we don't need any exception handling
  nor memory usage inside the Thread handler
   -> avoid classes.TThread and system.BeginThread() use
   -> application is still "officialy" mono-threaded (i.e. IsMultiThread=false),
     for faster System.pas and FastMM4 (no locking)
   -> code is even shorter then original one using TThread }
function ThreadWrapper(var P: TThreadParams): Integer; stdcall;
begin
  with P do
    AES.DoBlocks(bIn,bOut,bIn,bOut,BlockCount,Encrypt);
  ExitThread(0);
  result := 0; // make the compiler happy, but won't never be called
end;

procedure TAES.DoBlocksThread(var bIn, bOut: PAESBlock; Count: integer; doEncrypt: boolean);
var Thread: array[0..3] of TThreadParams; // faster than dynamic array
    Handle: array[0..3] of THandle; // high(Thread) is not compiled by XE2
    nThread, i, nOne: integer;
    pIn, pOut: PAESBlock;
begin
  if Count=0 then exit;
  if {$ifdef USEPADLOCK} padlock_available or {$endif}
    (SystemInfo.dwNumberOfProcessors<=1) or // (DebugHook<>0) or
    (Count<((512*1024) div AESBlockSize)) then begin // not needed below 512 KB
    DoBlocks(bIn,bOut,bIn,bOut,Count,doEncrypt);
    exit;
  end;
  nThread := SystemInfo.dwNumberOfProcessors;
  if nThread>length(Thread) then // a quad-core is enough ;)
    nThread := length(Thread);
  nOne := Count div nThread;
  pIn := bIn;
  pOut := bOut;
  for i := 0 to nThread-1 do
  with Thread[i] do begin // create threads parameters
    bIn := pIn;
    bOut := pOut;
    BlockCount := nOne;
    Encrypt := doEncrypt;
    AES := self; // local copy of the AES context for every thread
    Handle[i] := CreateThread(nil,0,@ThreadWrapper,@Thread[i],0,ID);
    inc(pIn,nOne);
    inc(pOut,nOne);
    dec(Count,nOne);
  end;
  if Count>0 then
    DoBlocks(pIn,pOut,pIn,pOut,Count,doEncrypt); // remaining blocks
  inc(Count,nOne*nThread);
  assert(integer(pIn)-integer(bIn)=Count*AESBlockSize);
  assert(integer(pOut)-integer(bOut)=Count*AESBlockSize);
  bIn := pIn;
  bOut := pOut;
  WaitForMultipleObjects(nThread,@Handle[0],True,INFINITE); 
  for i := 0 to nThread-1 do
    CloseHandle(Handle[i]);
end;
{$endif}


{ TSHA256 }

procedure Sha256ExpandMessageBlocks(W, Buf: PIntegerArray);
// Calculate "expanded message blocks"
{$ifdef PUREPASCAL}
var i: integer;
begin
  for i := 0 to 15 do
    W[i]:= bswap32(Buf[i]);
  for i := 16 to 63 do
    W[i] := (((W[i-2]shr 17)or(W[i-2]shl 15))xor((W[i-2]shr 19)or(W[i-2]shl 13))
      xor (W[i-2]shr 10))+W[i-7]+(((W[i-15]shr 7)or(W[i-15]shl 25))
      xor ((W[i-15]shr 18)or(W[i-15]shl 14))xor(W[i-15]shr 3))+W[i-16];
end;
{$else}
asm // W=eax Buf=edx
     push  esi
     push  edi
     push  ebx
     mov   esi,eax
     // part 1: W[i]:= RB(TW32Buf(Buf)[i])
     mov eax,[edx]; mov ebx,[edx+4]; bswap eax; bswap ebx; mov [esi],eax; mov [esi+4],ebx
     mov eax,[edx+8]; mov ebx,[edx+12]; bswap eax; bswap ebx; mov [esi+8],eax; mov [esi+12],ebx
     mov eax,[edx+16]; mov ebx,[edx+20]; bswap eax; bswap ebx; mov [esi+16],eax; mov [esi+20],ebx
     mov eax,[edx+24]; mov ebx,[edx+28]; bswap eax; bswap ebx; mov [esi+24],eax; mov [esi+28],ebx
     mov eax,[edx+32]; mov ebx,[edx+36]; bswap eax; bswap ebx; mov [esi+32],eax; mov [esi+36],ebx
     mov eax,[edx+40]; mov ebx,[edx+44]; bswap eax; bswap ebx; mov [esi+40],eax; mov [esi+44],ebx
     mov eax,[edx+48]; mov ebx,[edx+52]; bswap eax; bswap ebx; mov [esi+48],eax; mov [esi+52],ebx
     mov eax,[edx+56]; mov ebx,[edx+60]; bswap eax; bswap ebx; mov [esi+56],eax; mov [esi+60],ebx
     lea esi,[esi+64]
     // part2: W[i]:= LRot_1(W[i-3] xor W[i-8] xor W[i-14] xor W[i-16]);
     mov   ecx,48
@@2: mov   eax,[esi-2*4]    // W[i-2]
     mov   edi,[esi-7*4]    // W[i-7]
     mov   edx,eax
     mov   ebx,eax          // Sig1: RR17 xor RR19 xor SRx,10
     ror   eax,17
     ror   edx,19
     shr   ebx,10
     xor   eax,edx
     xor   eax,ebx
     add   edi,eax
     mov   eax,[esi-15*4]   // W[i-15]
     mov   ebx,eax          // Sig0: RR7 xor RR18 xor SR3
     mov   edx,eax
     ror   eax,7
     ror   edx,18
     shr   ebx,3
     xor   eax,edx
     xor   eax,ebx
     add   eax,edi
     add   eax,[esi-16*4]   // W[i-16]
     mov   [esi],eax
     add   esi,4
     dec   ecx
     jnz   @@2
     pop   ebx
     pop   edi
     pop   esi
end;
{$endif}

procedure TSHA256.Compress;
// Actual hashing function
const
  K: array[0..63] of cardinal = (
   $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5, $3956c25b, $59f111f1,
   $923f82a4, $ab1c5ed5, $d807aa98, $12835b01, $243185be, $550c7dc3,
   $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174, $e49b69c1, $efbe4786,
   $0fc19dc6, $240ca1cc, $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da,
   $983e5152, $a831c66d, $b00327c8, $bf597fc7, $c6e00bf3, $d5a79147,
   $06ca6351, $14292967, $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13,
   $650a7354, $766a0abb, $81c2c92e, $92722c85, $a2bfe8a1, $a81a664b,
   $c24b8b70, $c76c51a3, $d192e819, $d6990624, $f40e3585, $106aa070,
   $19a4c116, $1e376c08, $2748774c, $34b0bcb5, $391c0cb3, $4ed8aa4a,
   $5b9cca4f, $682e6ff3, $748f82ee, $78a5636f, $84c87814, $8cc70208,
   $90befffa, $a4506ceb, $bef9a3f7, $c67178f2);
var
  H: TSHAHash;
  W: array[0..63] of cardinal;
{$ifdef PUREPASCAL}
  i: integer;
  t1, t2: cardinal;
{$endif}
begin
  // Calculate "expanded message blocks"
  Sha256ExpandMessageBlocks(@W,@TSHAContext(Context).Buffer);

  // Assign old working hash to variables A..H
  with TSHAContext(Context) do begin
    H.A := Hash.A;
    H.B := Hash.B;
    H.C := Hash.C;
    H.D := Hash.D;
    H.E := Hash.E;
    H.F := Hash.F;
    H.G := Hash.G;
    H.H := Hash.H;
  end;

{$ifdef PUREPASCAL}
  // SHA256 compression function
  for i := 0 to high(W) do begin
    t1 := H.H+(((H.E shr 6)or(H.E shl 26))xor((H.E shr 11)or(H.E shl 21))xor
      ((H.E shr 25)or(H.E shl 7)))+((H.E and H.F)xor(not H.E and H.G))+K[i]+W[i];
    t2 := (((H.A shr 2)or(H.A shl 30))xor((H.A shr 13)or(H.A shl 19))xor
      ((H.A shr 22)xor(H.A shl 10)))+((H.A and H.B)xor(H.A and H.C)xor(H.B and H.C));
    H.H := H.G; H.G := H.F; H.F := H.E; H.E := H.D+t1;
    H.D := H.C; H.C := H.B; H.B := H.A; H.A := t1+t2; 
  end;
{$else}
  // SHA256 compression function - optimized by A.B. for pipelined CPU
  asm
    push ebx
    push esi
    push edi
    xor  edi,edi // edi=i
    // rolled version faster than the unrolled one (good pipelining work :)
@s: mov  eax,[H].TSHAHash.E
    mov  ecx,eax
    mov  edx,eax
    mov  ebx,eax // ebx=E
    ror  eax,6
    ror  edx,11
    ror  ecx,25
    xor  eax,edx
    mov  edx,[H].TSHAHash.G
    xor  eax,ecx
    mov  ecx,[H].TSHAHash.H
    add  ecx,eax // T1=ecx
    mov  eax,[H].TSHAHash.F
    mov  [H].TSHAHash.H,edx
    mov  [H].TSHAHash.G,eax
    xor  eax,edx
    mov  [H].TSHAHash.F,ebx
    and  eax,ebx
    xor  eax,edx
    add  eax,dword ptr [K+edi*4]
    add  eax,ecx
    mov  ecx,[H].TSHAHash.D
    add  eax,dword ptr [W+edi*4]
    mov  ebx,[H].TSHAHash.A
    //  eax= T1 := H + Sum1(E) +(((F xor G) and E) xor G)+K[i]+W[i];
    add  ecx,eax
    mov  esi,eax  // esi = T1
    mov  [H].TSHAHash.E,ecx // E := D + T1;
    mov  eax,ebx // Sum0(A)
    mov  edx,ebx
    ror  eax,2
    mov  ecx,ebx
    ror  edx,13
    ror  ecx,22
    xor  eax,edx
    xor  eax,ecx // eax = Sum0(A)
    mov  ecx,[H].TSHAHash.B
    add  esi,eax
    mov  eax,ebx // ebx=A
    mov  edx,ebx // eax=edx=A
    or   eax,ecx
    and  eax,[H].TSHAHash.C   // eax = (A or B)and C
    and  edx,ecx
    or   eax,edx // eax = ((A or B)and C) or (A and B)
    inc  edi
    add  esi,eax  // esi= T1+T2
    mov  [H].TSHAHash.A,esi // all these instructions are pipelined -> roll OK
    mov  eax,[H].TSHAHash.C // eax=C ecx=B ebx=A
    mov  [H].TSHAHash.B,ebx
    mov  [H].TSHAHash.C,ecx
    mov  [H].TSHAHash.D,eax
    cmp  edi,64
    jnz  @s
    pop  edi
    pop  esi
    pop  ebx
  end;
{$endif}

  // Calculate new working hash
  with TSHAContext(Context) do begin
    inc(Hash.A,H.A);
    inc(Hash.B,H.B);
    inc(Hash.C,H.C);
    inc(Hash.D,H.D);
    inc(Hash.E,H.E);
    inc(Hash.F,H.F);
    inc(Hash.G,H.G);
    inc(Hash.H,H.H);
  end;
end;

procedure TSHA256.Final(out Digest: TSHA256Digest);
// finalize SHA256 calculation, clear context
var Data: TSHAContext absolute Context;
begin
  // Message padding
  // 1. append bit '1' after Buffer
  Data.Buffer[Data.Index]:= $80;
  fillchar(Data.Buffer[Data.Index+1],63-Data.Index,0);
  // 2. Compress if more than 448 bits, (no room for 64 bit length
  if Data.Index>=56 then begin
    Compress;
    fillchar(Data.Buffer,56,0);
  end;
  // Write 64 bit Buffer length into the last bits of the last block
  // (in big endian format) and do a final compress
  PInteger(@Data.Buffer[56])^ := bswap32(Int64Rec(Data.MLen).Hi);
  PInteger(@Data.Buffer[60])^ := bswap32(Int64Rec(Data.MLen).Lo);
  Compress;
  // Hash -> Digest to little endian format
  bswap256(@Data.Hash,@Digest);
  // Clear Data
  Init;
end;

procedure TSHA256.Full(Buffer: pointer; Len: integer; out Digest: TSHA256Digest);
begin
{$ifdef USEPADLOCK}
  // Padlock need all data once -> Full() is OK, not successive Update()
  if padlock_available then begin
    Init; // for later Update use
    {$ifdef PADLOCKDEBUG}write('padlock_phe_sha256 ');{$endif}
    if padlock_phe_sha256(buffer,Len,Digest)=0 then
      exit else
    {$ifdef PADLOCKDEBUG}write(':ERROR ');{$endif}
  end;
{$endif}
  Init;
  Update(Buffer,Len);
  Final(Digest);
end;

procedure TSHA256.Init;
// initialize context
var Data: TSHAContext absolute Context;
begin
  fillchar(Data,sizeof(Data),0);
  Data.Hash.A := $6a09e667;
  Data.Hash.B := $bb67ae85;
  Data.Hash.C := $3c6ef372;
  Data.Hash.D := $a54ff53a;
  Data.Hash.E := $510e527f;
  Data.Hash.F := $9b05688c;
  Data.Hash.G := $1f83d9ab;
  Data.Hash.H := $5be0cd19;
end;

procedure TSHA256.Update(Buffer: pointer; Len: integer);
var Data: TSHAContext absolute Context;
    aLen: integer;
begin
  if Buffer=nil then exit; // avoid GPF
  inc(Data.MLen, Int64(cardinal(Len)) shl 3);
  while Len > 0 do begin
    aLen := 64-Data.Index;
    if aLen<=Len then begin
      move(buffer^,Data.Buffer[Data.Index],aLen);
      dec(Len,aLen);
      inc(PtrInt(buffer),aLen);
      Compress;
      Data.Index := 0;
    end else begin
      move(buffer^,Data.Buffer[Data.Index],Len);
      inc(Data.Index,Len);
      break;
    end;
  end;
end;

procedure SHA256Weak(const s: RawByteString; out Digest: TSHA256Digest);
var L: integer;
    SHA: TSHA256;
    p: PAnsiChar;
    tmp: array[0..255] of byte;
begin
  L := length(s);
  p := pointer(s);
  if L<sizeof(tmp) then begin
    fillchar(tmp,sizeof(tmp),L); // add some salt to unweak password
    if L>0 then
      move(p^,tmp,L);
    SHA.Full(@tmp,sizeof(tmp),Digest);
  end else
    SHA.Full(p,L,Digest);
end;

procedure AES(const Key; KeySize: cardinal; buffer: pointer; Len: Integer; Encrypt: boolean);
begin
  AES(Key,KeySize,buffer,buffer,Len,Encrypt);
end;

procedure AES(const Key; KeySize: cardinal; bIn, bOut: pointer; Len: Integer; Encrypt: boolean);
var n: integer;
    pIn, pOut: PAESBlock;
    Crypt: TAES;
begin
  if (bIn=nil) or (bOut=nil) then exit;
  // 1. Init
  n := Len div AESBlockSize;
  if n<0 then exit else
  if n>0 then
    if (KeySize>4) and not Crypt.DoInit(Key,KeySize,Encrypt) then
      KeySize := 4; // if error in KeySize, use default fast XorOffset()
  if KeySize=0 then begin // KeySize=0 -> no encryption -> direct copy
    move(bIn^, bOut^, Len);
    exit;
  end;
  if n<1 then begin // too small for AES -> XorOffset() remaining 0..15 bytes
    move(bIn^, bOut^, Len);
    XorOffset(bOut,0,Len);
    exit;
  end;
  // 2. All full blocks, with AES
{$ifdef USETHREADSFORBIGAESBLOCKS}
  pIn := bIn;
  pOut := bOut;
  Crypt.DoBlocksThread(pIn,pOut,n,Encrypt);
{$else}
  Crypt.DoBlocks(bIn,bOut,pIn,pOut,n,Encrypt);
{$endif}
  // 3. Last block, just XORed from Key
//  assert(KeySize div 8>=AESBlockSize);
  n := cardinal(Len) mod AESBlockSize;
  move(pIn^,pOut^,n); // pIn=pOut is tested in move()
  XorOffset(pointer(pOut),Len-n,n);
{$ifdef USEPADLOCK}
  Crypt.Done; // use for Padlock support
{$endif}
end;

const TmpSize = 65536;
  // Tmp buffer for AESFull -> Xor Crypt is TmpSize-dependent / use XorBlock()
      TmpSizeBlock = TmpSize div AESBlockSize;
type
  TTmp = array[0..TmpSizeBlock-1] of TAESBlock;

function AES(const Key; KeySize: cardinal; const s: RawByteString; Encrypt: boolean): RawByteString;
begin
  SetLength(result,length(s));
  if s<>'' then
    AES(Key,KeySize,pointer(s),pointer(result),length(s),Encrypt);
end;

function AES(const Key; KeySize: cardinal; buffer: pointer; Len: cardinal; Stream: TStream; Encrypt: boolean): boolean; overload;
var buf: pointer;
    last, b, n, i: cardinal;
    Crypt: TAES;
begin
  result := false;
  if buffer=nil then exit;
  if (KeySize>4) and not Crypt.DoInit(Key,KeySize,Encrypt) then
    KeySize := 4; // if error in KeySize, use default fast XorOffset()
  if KeySize=0 then begin // no Crypt -> direct write to dest Stream
    Stream.Write(buffer^,Len);
    result := true;
    exit;
  end;
  getmem(buf,TmpSize);
  try
    Last := Len mod AESBlockSize;
    n := Len-Last;
    i := 0;
    while n>0 do begin // crypt/uncrypt all AESBlocks
      if n>TmpSize then
        b := TmpSize else
        b := n;
      assert(b mod AESBlockSize=0);
      if KeySize=4 then begin
        move(buffer^,buf^,b);
        XorOffset(pointer(buf),i,b);
        inc(i,b);
      end else
        Crypt.DoBlocks(buffer,buf,b div AESBlockSize,Encrypt);
      Stream.Write(buf^,b);
      inc(PtrUInt(buffer),b);
      dec(n,b);
    end;
    assert((KeySize>4)or(i=Len-Last));
    if last>0 then begin // crypt/uncrypt (Xor) last 0..15 bytes
      move(buffer^,buf^,Last);
      XorOffset(pointer(buf),Len-Last,Last);
      Stream.Write(buf^,Last);
    end;
    result := true;
  finally
    freemem(buf);
  end;
end;

function KeyFrom(const Key; KeySize: cardinal): cardinal;
begin
  case KeySize div 8 of
  0:   result := 0;
  1:   result := pByte(@Key)^;
  2,3: result := pWord(@Key)^;
  else result := PInteger(@Key)^;
  end;
end;

function TAESFullHeader.Calc(const Key; KeySize: cardinal): cardinal;
begin
  result := Adler32Asm(KeySize,@Key,KeySize shr 3) xor Te0[OriginalLen and $FF]
    xor Te1[SourceLen and $FF] xor Td0[SomeSalt and $7FF];
end;

function TAESFull.EncodeDecode(const Key; KeySize, inLen: cardinal; Encrypt: boolean;
  inStream, outStream: TStream; bIn, bOut: pointer; OriginalLen: Cardinal=0): integer;
var Tmp: ^TTmp;
    pIn, pOut: PAESBlock;
    Crypt: TAES;
    nBlock,
    XorCod: cardinal;
procedure Read(Tmp: pointer; ByteCount: cardinal);
begin
  if pIn=nil then
    InStream.Read(Tmp^,ByteCount) else begin
    move(pIn^,Tmp^,ByteCount);
    inc(PtrUInt(pIn),ByteCount);
  end;
end;
procedure Write(Tmp: pointer; ByteCount: cardinal);
begin
  if pOut=nil then
    OutStream.Write(Tmp^,ByteCount) else begin
    move(Tmp^,pOut^,ByteCount);
    inc(PtrUInt(pOut),ByteCount);
  end;
end;
procedure SetOutLen(Len: cardinal);
var P: cardinal;
begin
  result := Len; // global EncodeDecode() result
  if OutStream<>nil then begin
    if OutStream.InheritsFrom(TMemoryStream) then
      with TMemoryStream(OutStream) do begin
        P := Seek(0,soFromCurrent);
        Size := P+Len; // auto-reserve space (no Realloc:)
        Seek(P+Len,soFromBeginning);
        bOut := PAnsiChar(Memory)+P;
        pOut := bOut;
        OutStream := nil; //  OutStream is slower and use no thread
      end;
  end else
  if bOut=nil then begin
    outStreamCreated := THeapMemoryStream.Create; // faster than TMemoryStream
    outStreamCreated.Size := Len; // auto-reserve space (no Realloc:)
    bOut := outStreamCreated.Memory;
    pOut := bOut; // OutStream is slower and use no thread
  end;
  if KeySize=0 then exit; // no Tmp to be allocated on direct copy
{$ifdef USEPADLOCK} // PADLOCK prefers 16-bytes alignment
  if (KeySize=32) or (InStream<>nil) or (OutStream<>nil) or
     (cardinal(bIn) and $f<>0) or (cardinal(bOut) and $f<>0) then begin
    New(Tmp);
//    assert(cardinal(Tmp) and $F=0);
  end;
{$else}
  if (KeySize=32) or (InStream<>nil) or (OutStream<>nil) then 
    New(Tmp);
{$endif}
end;
procedure DoBlock(BlockCount: integer);
begin
  if BlockCount=0 then
    exit;
  Read(Tmp,BlockCount*AESBlockSize);
  Crypt.DoBlocks(PAESBLock(Tmp),PAESBLock(Tmp),BlockCount,Encrypt);
  Write(Tmp,BlockCount*AESBlockSize);
end;
var n, LastLen: cardinal;
    i: integer;
    Last: TAESBlock;
begin
  Tmp := nil;
  outStreamCreated := nil;
  Head.SourceLen := InLen;
  nBlock := Head.SourceLen div AESBlockSize;
  if Encrypt and (OriginalLen<>0) then
    Head.OriginalLen := OriginalLen else
    Head.OriginalLen := InLen;
  KeySize := KeySize div 8;
  if not (KeySize in [0,4,16,24,32]) then
    KeySize := 0 else  // valid KeySize: 0=nothing, 32=xor, 128,192,256=AES
    KeySize := KeySize*8;
  XorCod := inLen;
  if (inStream<>nil) and inStream.InheritsFrom(TMemoryStream) then begin
    bIn := TMemoryStream(inStream).Memory;
    inStream := nil;
   end;
  pIn := bIn;
  pOut := bOut;
  if (KeySize>=128) and not Crypt.DoInit(Key,KeySize,Encrypt) then
    KeySize := 32;
  if KeySize=32 then
     XorCod := KeyFrom(Key,KeySize) xor XorCod else
  if (KeySize=0) and (InStream=nil) then begin
    SetOutLen(inLen);
    Write(bIn,inLen);  // no encryption -> direct write
    exit;
  end;
  try
    // 0. KeySize = 0:direct copy 32:XorBlock 
    if KeySize<128 then begin
      SetOutLen(inLen);
      assert(Tmp<>nil);
      LastLen := inLen;
      while LastLen<>0 do begin
        if LastLen>TmpSize then
          n := TmpSize else
          n := LastLen;
        Read(Tmp,n);
        if KeySize>0 then
          XorBlock(pointer(Tmp),n,XorCod);
        Write(Tmp,n);
        dec(LastLen,n);
      end;
    end else begin // no we do AES encryption:
      // 1. Header process
      if Encrypt then begin
        // encrypt data
        if (pIn=pOut) and (pIn<>nil) then begin
          assert(false); // Head in pOut^ will overflow data in pIn^
          result := 0;
          exit;
        end;
        LastLen := inLen mod AESBlockSize;
        if LastLen=0 then
          SetOutLen(inLen+AESBlockSize) else
          SetOutLen((nBlock+2)*AESBlockSize);
        Head.SomeSalt := random(MaxInt);
        Head.HeaderCheck := Head.Calc(Key,KeySize);
        Crypt.Encrypt(TAESBlock(Head));
        Write(@Head,sizeof(Head));
      end else begin
        // uncrypt data
        dec(nBlock); // Header is already done
        Read(@Head,sizeof(Head));
        Crypt.Decrypt(TAESBlock(Head));
        with Head do begin
          if HeaderCheck<>Head.Calc(Key,KeySize) then begin
            result := -1;
            exit; // wrong key
          end;
          SetOutLen(SourceLen);
          LastLen := SourceLen mod AESBlockSize;
        end;
        if LastLen<>0 then
          dec(nBlock); // the very last block is for the very last bytes
      end;
      // 2. All full blocks, with AES
      if Tmp=nil then begin
      {$ifdef USETHREADSFORBIGAESBLOCKS} // Tmp is 64KB -> helpless Threads
        Crypt.DoBlocksThread(pIn,pOut,nBlock,Encrypt);
      {$else}
        Crypt.DoBlocks(pIn,pOut,pIn,pOut,nBlock,Encrypt);
      {$endif}
      end else begin
        for i := 1 to nBlock div TmpSizeBlock do
          DoBlock(TmpSizeBlock);
        DoBlock(nBlock mod TmpSizeBlock);
      end;
      // 3. Last block
      if LastLen<>0 then
      if Encrypt then begin
        fillchar(Last,AESBlockSize,0);
        Read(@Last,LastLen);
        Crypt.Encrypt(Last);
        Write(@Last,AESBlockSize);
      end else begin
        Read(@Last,AESBlockSize);
        Crypt.Decrypt(Last);
        Write(@Last,LastLen);
      end;
{$ifdef USEPADLOCK}
      Crypt.Done; // used for Padlock only
{$endif}
    end;
  finally
    if Tmp<>nil then
      Freemem(Tmp);
  end;
end;


function AESFullKeyOK(const Key; KeySize: cardinal; buff: pointer): boolean;
// true if begining of buff contains true AESFull encrypted data with this Key
var Crypt: TAES;
    Head: TAESFullHeader;
begin
  if KeySize<128 then
    result := true else
  if not Crypt.DecryptInit(Key,KeySize) then
    result := false else begin
    Crypt.Decrypt(PAESBlock(buff)^,TAESBlock(Head));
    result := Head.Calc(Key,KeySize)=Head.HeaderCheck;
{$ifdef USEPADLOCK}
    Crypt.Done; // for Padlock support
{$endif}
  end;
end;

function AESFull(const Key; KeySize: cardinal; bIn, bOut: pointer; Len: integer;
  Encrypt: boolean; OriginalLen: Cardinal=0): integer; overload;
// bOut must be at least bIn+32/Encrypt bIn-16/Decrypt -> returns outLength, <0 if error
var A: TAESFull;
begin
  result := A.EncodeDecode(Key,KeySize,Len,Encrypt,nil,nil,bIn,bOut,OriginalLen);
end;

function AESFull(const Key; KeySize: cardinal; bIn: pointer; Len: Integer;
   outStream: TStream; Encrypt: boolean; OriginalLen: Cardinal=0): boolean; // true is Key OK
// outStream will be larger/smaller than Len: this is a full AES version
// if not KeySize in [128,192,256] -> use very fast and Simple Xor Cypher
var A: TAESFull;
begin
  result := A.EncodeDecode(Key,KeySize,
    Len,Encrypt,nil,outStream,bIn,nil,OriginalLen)>=0;
end;

procedure AESSHA256(bIn, bOut: pointer; Len: integer; const Password: RawByteString; Encrypt: boolean);
var Digest: TSHA256Digest;
begin
  SHA256Weak(Password,Digest);
  AES(Digest,sizeof(Digest)*8,bIn,bOut,Len,Encrypt);
end;

function AESSHA256(const s, Password: RawByteString; Encrypt: boolean): RawByteString;
begin
  SetLength(result,length(s));
  AESSHA256(pointer(s),pointer(result),length(s),Password,Encrypt);
end;

procedure AESSHA256(Buffer: pointer; Len: integer; const Password: RawByteString; Encrypt: boolean);
// Encrypt/Decrypt Buffer with AES and SHA256 password
begin
  AESSHA256(Buffer,Buffer,Len,Password,Encrypt);
end;

procedure AESSHA256Full(bIn: pointer; Len: Integer; outStream: TStream; const Password: RawByteString; Encrypt: boolean);
// outStream will be larger/smaller than Len: this is a full AES version
var Digest: TSHA256Digest;
begin
  SHA256Weak(Password,Digest);
  AESFull(Digest,sizeof(Digest)*8,bIn,Len,outStream,Encrypt);
end;


function Adler32Pas(Adler: cardinal; p: pointer; Count: Integer): cardinal;
// simple Adler32 implementation (twice slower than Asm, but shorter code size)
var s1, s2: cardinal;
    i, n: integer;
begin
  s1 := LongRec(Adler).Lo;
  s2 := LongRec(Adler).Hi;
  while Count>0 do begin
    if Count<5552 then
      n := Count else
      n := 5552;
    for i := 1 to n do begin
      inc(s1,pByte(p)^);
      inc(PtrUInt(p));
      inc(s2,s1);
    end;
    s1 := s1 mod 65521;
    s2 := s2 mod 65521;
    dec(Count,n);
  end;
  result := word(s1)+cardinal(word(s2)) shl 16;
end;

function Adler32Asm(Adler: cardinal; p: pointer; Count: Integer): cardinal;
{$ifdef PUREPASCAL}
var s1, s2: cardinal;
    i, n: integer;
begin
  s1 := LongRec(Adler).Lo;
  s2 := LongRec(Adler).Hi;
  while Count>0 do begin
    if Count<5552 then
      n := Count else
      n := 5552;
    for i := 1 to n do begin
      inc(s1,pByte(p)^);
      inc(PtrUInt(p));
      inc(s2,s1);
    end;
    s1 := s1 mod 65521;
    s2 := s2 mod 65521;
    dec(Count,n);
  end;
  result := word(s1)+cardinal(word(s2)) shl 16;
end;
{$else}
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
	movzx     edx,byte ptr [esi]
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
{$endif}

function Adler32SelfTest: boolean;
begin
  result := (Adler32Asm(1,@Te0,sizeof(Te0))=Adler32Pas(1,@Te0,sizeof(Te0))) and
    (Adler32Asm(7,@Te1,sizeof(Te1)-3)=Adler32Pas(7,@Te1,sizeof(Te1)-3));
end;


{ TAESWriteStream }

constructor TAESWriteStream.Create(outStream: TStream; const Key; KeySize: cardinal);
begin
  inherited Create;
  if KeySize=0 then
    NoCrypt := true else
    AES.EncryptInit(Key,KeySize);
  Dest := outStream;
end;

destructor TAESWriteStream.Destroy;
begin
  Finish;
{$ifdef USEPADLOCK}
  AES.Done; // usefull for padlock only
{$endif}
  inherited;
end;

procedure TAESWriteStream.Finish;
begin
  if BufCount=0 then exit;
  assert((BufCount<AESBlockSize) and AES.Initialized and not NoCrypt);
  XorOffset(@Buf,DestSize,BufCount);
  Dest.Write(Buf,BufCount);
  BufCount := 0;
end;

function TAESWriteStream.Read(var Buffer; Count: Integer): Longint;
begin
  raise EStreamError.Create(ClassName);
end;

function TAESWriteStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  raise EStreamError.Create(ClassName);
end;

function TAESWriteStream.Write(const Buffer; Count: Integer): Longint;
// most of the time, a 64KB-buffered compressor have BufCount=0
// will crypt 'const Buffer' memory in place -> use AFTER T*Compressor
var B: TByteArray absolute Buffer;
    Len: integer;
begin
  result := Count;
  Adler := Adler32Asm(Adler,@Buffer,Count);
  if not NoCrypt then // KeySize=0 -> save as-is
  if not AES.Initialized then // if error in KeySize -> default fast XorOffset()
    XorOffset(@B,DestSize,Count) else begin
    if BufCount>0 then begin
      Len := AESBlockSize-BufCount;
      if Len>Count then
        Len := Count;
      move(Buffer,Buf[BufCount],Len);
      inc(BufCount,Len);
      if BufCount<AESBlockSize then
        exit;
      AES.Encrypt(Buf);
      Dest.Write(Buf,AESBlockSize);
      inc(DestSize,AESBlockSize);
      Dec(Count,Len);
      AES.DoBlocks(@B[Len],@B[Len],cardinal(Count) div AESBlockSize,true);
    end else
      AES.DoBlocks(@B,@B,cardinal(Count) div AESBlockSize,true);
    BufCount := cardinal(Count) mod AESBlockSize;
    if BufCount<>0 then begin
      dec(Count,BufCount);
      move(B[Count],Buf[0],BufCount);
    end;
  end;
  Dest.Write(Buffer,Count);
  inc(DestSize,Count);
end;


procedure XorBlock(p: PIntegerArray; Count, Cod: integer);
// very fast Xor() according to Cod - not Compression or Stream compatible
var i: integer;
begin
  for i := 1 to Count shr 4 do begin // proceed through 16 bytes blocs
    Cod := (Cod shl 11) xor integer(Td0[cod shr 21]); // shr 21 -> 8*[byte] of cardinal
    p^[0] := p^[0] xor Cod;
    p^[1] := p^[1] xor Cod;
    p^[2] := p^[2] xor Cod;
    p^[3] := p^[3] xor Cod;
    inc(PtrUInt(p),16);
  end;
  Cod := (Cod shl 11) xor integer(Td0[cod shr 21]);
  for i := 1 to (Count and 15)shr 2 do begin // last 4 bytes blocs
    p^[0] := p^[0] xor Cod;
    inc(PtrUInt(p),4);
  end;
  for i := 1 to Count and 3 do begin
    pByte(p)^ := pByte(p)^ xor byte(Cod);
    inc(PtrUInt(p));
  end;
end;  

var
  Xor32Byte: TByteArray absolute Td0;  // $2000=8192 bytes of XOR tables ;)

procedure XorOffset(p: pByte; Index,Count: integer);
// XorOffset: fast and simple Cypher using Index (=Position in Dest Stream):
// Compression not OK -> apply after compress (e.g. TBZCompressor.withXor=true)
{$ifndef PURE_PASCAL}
procedure Xor64(PI: PIntegerArray; P: pByte; Count: integer);
asm // eax=PI edx=P ecx=Count64
  push ebx
  push esi
  shr ecx,3
  jz @z
@1:
  mov ebx,[eax]
  mov esi,[eax+4]
  xor [edx],ebx
  xor [edx+4],esi
  dec ecx
  lea eax,eax+8
  lea edx,edx+8
  jnz @1
@z:
  pop esi
  pop ebx
end;
{$endif PURE_PASCAL}
var i, Len: integer;
begin
  if Count>0 then
  repeat
    Index := Index and $1FFF;
    Len := $2000-Index;
    if Len>Count then
      Len := Count;
{$ifdef PURE_PASCAL}
    for i := 1 to Len do begin
      p^ := p^ xor Xor32Byte[Index];
      inc(p); inc(Index);
    end;
{$else}
    Xor64(@Xor32Byte[Index],p,Len);
    inc(p,Len and -8); // -8=$FFFFFFF8
    inc(Index,Len and -8);
    for i := 1 to Len and 7 do begin
      p^ := p^ xor Xor32Byte[Index];
      inc(p); inc(Index);
    end;
{$endif}
    Dec(Count,Len);
  until Count=0;
end;


procedure XorConst(p: PIntegerArray; Count: integer);
// XorConst: fast Cypher changing by Count value
// (compression OK):
var i: integer;
    Code: integer;
begin // 1 to 3 bytes may stay unencrypted: not relevant
  Code := integer(Td0[Count and $3FF]);
  for i := 1 to (Count shr 4) do begin
     p^[0] := p^[0] xor Code;
     p^[1] := p^[1] xor Code;
     p^[2] := p^[2] xor Code;
     p^[3] := p^[3] xor Code;
     inc(PtrUInt(p),16);
  end;
  for i := 0 to ((Count and 15)shr 2)-1 do // last 4 bytes blocs
    p^[i] := p^[i] xor Code;
end;


{ TMD5 }

procedure MD5Transform(var buf: TMD5Buf; const in_: TMD5In);
var a,b,c,d: cardinal; // unrolled -> compiler will only use cpu registers :)
// the code below is very fast, and can be compared proudly against C or ASM
begin
  a := buf[0];
  b := buf[1];
  c := buf[2];
  d := buf[3];
  inc(a,in_[0]+$d76aa478+(d xor(b and(c xor d)))); a := ((a shl 7)or(a shr(32-7)))+b;
  inc(d,in_[1]+$e8c7b756+(c xor(a and(b xor c)))); d := ((d shl 12)or(d shr(32-12)))+a;
  inc(c,in_[2]+$242070db+(b xor(d and(a xor b)))); c := ((c shl 17)or(c shr(32-17)))+d;
  inc(b,in_[3]+$c1bdceee+(a xor(c and(d xor a)))); b := ((b shl 22)or(b shr(32-22)))+c;
  inc(a,in_[4]+$f57c0faf+(d xor(b and(c xor d)))); a := ((a shl 7)or(a shr(32-7)))+b;
  inc(d,in_[5]+$4787c62a+(c xor(a and(b xor c)))); d := ((d shl 12)or(d shr(32-12)))+a;
  inc(c,in_[6]+$a8304613+(b xor(d and(a xor b)))); c := ((c shl 17)or(c shr(32-17)))+d;
  inc(b,in_[7]+$fd469501+(a xor(c and(d xor a)))); b := ((b shl 22)or(b shr(32-22)))+c;
  inc(a,in_[8]+$698098d8+(d xor(b and(c xor d)))); a := ((a shl 7)or(a shr(32-7)))+b;
  inc(d,in_[9]+$8b44f7af+(c xor(a and(b xor c)))); d := ((d shl 12)or(d shr(32-12)))+a;
  inc(c,in_[10]+$ffff5bb1+(b xor(d and(a xor b)))); c := ((c shl 17)or(c shr(32-17)))+d;
  inc(b,in_[11]+$895cd7be+(a xor(c and(d xor a)))); b := ((b shl 22)or(b shr(32-22)))+c;
  inc(a,in_[12]+$6b901122+(d xor(b and(c xor d)))); a := ((a shl 7)or(a shr(32-7)))+b;
  inc(d,in_[13]+$fd987193+(c xor(a and(b xor c)))); d := ((d shl 12)or(d shr(32-12)))+a;
  inc(c,in_[14]+$a679438e+(b xor(d and(a xor b)))); c := ((c shl 17)or(c shr(32-17)))+d;
  inc(b,in_[15]+$49b40821+(a xor(c and(d xor a)))); b := ((b shl 22)or(b shr(32-22)))+c;
  inc(a,in_[1]+$f61e2562+(c xor(d and(b xor c))));  a := ((a shl 5)or(a shr(32-5)))+b;
  inc(d,in_[6]+$c040b340+(b xor(c and(a xor b))));  d := ((d shl 9)or(d shr(32-9)))+a;
  inc(c,in_[11]+$265e5a51+(a xor(b and(d xor a)))); c := ((c shl 14)or(c shr(32-14)))+d;
  inc(b,in_[0]+$e9b6c7aa+(d xor(a and(c xor d))));  b := ((b shl 20)or(b shr(32-20)))+c;
  inc(a,in_[5]+$d62f105d+(c xor(d and(b xor c))));  a := ((a shl 5)or(a shr(32-5)))+b;
  inc(d,in_[10]+$02441453+(b xor(c and(a xor b)))); d := ((d shl 9)or(d shr(32-9)))+a;
  inc(c,in_[15]+$d8a1e681+(a xor(b and(d xor a)))); c := ((c shl 14)or(c shr(32-14)))+d;
  inc(b,in_[4]+$e7d3fbc8+(d xor(a and(c xor d))));  b := ((b shl 20)or(b shr(32-20)))+c;
  inc(a,in_[9]+$21e1cde6+(c xor(d and(b xor c))));  a := ((a shl 5)or(a shr(32-5)))+b;
  inc(d,in_[14]+$c33707d6+(b xor(c and(a xor b)))); d := ((d shl 9)or(d shr(32-9)))+a;
  inc(c,in_[3]+$f4d50d87+(a xor(b and(d xor a))));  c := ((c shl 14)or(c shr(32-14)))+d;
  inc(b,in_[8]+$455a14ed+(d xor(a and(c xor d))));  b := ((b shl 20)or(b shr(32-20)))+c;
  inc(a,in_[13]+$a9e3e905+(c xor(d and(b xor c)))); a := ((a shl 5)or(a shr(32-5)))+b;
  inc(d,in_[2]+$fcefa3f8+(b xor(c and(a xor b))));  d := ((d shl 9)or(d shr(32-9)))+a;
  inc(c,in_[7]+$676f02d9+(a xor(b and(d xor a))));  c := ((c shl 14)or(c shr(32-14)))+d;
  inc(b,in_[12]+$8d2a4c8a+(d xor(a and(c xor d)))); b := ((b shl 20)or(b shr(32-20)))+c;
  inc(a,in_[5]+$fffa3942+(b xor c xor d));  a := ((a shl 4)or(a shr(32-4)))+b;
  inc(d,in_[8]+$8771f681+(a xor b xor c));  d := ((d shl 11)or(d shr(32-11)))+a;
  inc(c,in_[11]+$6d9d6122+(d xor a xor b)); c := ((c shl 16)or(c shr(32-16)))+d;
  inc(b,in_[14]+$fde5380c+(c xor d xor a)); b := ((b shl 23)or(b shr(32-23)))+c;
  inc(a,in_[1]+$a4beea44+(b xor c xor d));  a := ((a shl 4)or(a shr(32-4)))+b;
  inc(d,in_[4]+$4bdecfa9+(a xor b xor c));  d := ((d shl 11)or(d shr(32-11)))+a;
  inc(c,in_[7]+$f6bb4b60+(d xor a xor b));  c := ((c shl 16)or(c shr(32-16)))+d;
  inc(b,in_[10]+$bebfbc70+(c xor d xor a)); b := ((b shl 23)or(b shr(32-23)))+c;
  inc(a,in_[13]+$289b7ec6+(b xor c xor d)); a := ((a shl 4)or(a shr(32-4)))+b;
  inc(d,in_[0]+$eaa127fa+(a xor b xor c));  d := ((d shl 11)or(d shr(32-11)))+a;
  inc(c,in_[3]+$d4ef3085+(d xor a xor b));  c := ((c shl 16)or(c shr(32-16)))+d;
  inc(b,in_[6]+$04881d05+(c xor d xor a));  b := ((b shl 23)or(b shr(32-23)))+c;
  inc(a,in_[9]+$d9d4d039+(b xor c xor d));  a := ((a shl 4)or(a shr(32-4)))+b;
  inc(d,in_[12]+$e6db99e5+(a xor b xor c)); d := ((d shl 11)or(d shr(32-11)))+a;
  inc(c,in_[15]+$1fa27cf8+(d xor a xor b)); c := ((c shl 16)or(c shr(32-16)))+d;
  inc(b,in_[2]+$c4ac5665+(c xor d xor a));   b := ((b shl 23)or(b shr(32-23)))+c;
  inc(a,in_[0]+$f4292244+(c xor(b or(not d))));  a := ((a shl 6)or(a shr(32-6)))+b;
  inc(d,in_[7]+$432aff97+(b xor(a or(not c))));  d := ((d shl 10)or(d shr(32-10)))+a;
  inc(c,in_[14]+$ab9423a7+(a xor(d or(not b)))); c := ((c shl 15)or(c shr(32-15)))+d;
  inc(b,in_[5]+$fc93a039+(d xor(c or(not a))));  b := ((b shl 21)or(b shr(32-21)))+c;
  inc(a,in_[12]+$655b59c3+(c xor(b or(not d)))); a := ((a shl 6)or(a shr(32-6)))+b;
  inc(d,in_[3]+$8f0ccc92+(b xor(a or(not c))));  d := ((d shl 10)or(d shr(32-10)))+a;
  inc(c,in_[10]+$ffeff47d+(a xor(d or(not b)))); c := ((c shl 15)or(c shr(32-15)))+d;
  inc(b,in_[1]+$85845dd1+(d xor(c or(not a))));  b := ((b shl 21)or(b shr(32-21)))+c;
  inc(a,in_[8]+$6fa87e4f+(c xor(b or(not d))));  a := ((a shl 6)or(a shr(32-6)))+b;
  inc(d,in_[15]+$fe2ce6e0+(b xor(a or(not c)))); d := ((d shl 10)or(d shr(32-10)))+a;
  inc(c,in_[6]+$a3014314+(a xor(d or(not b))));  c := ((c shl 15)or(c shr(32-15)))+d;
  inc(b,in_[13]+$4e0811a1+(d xor(c or(not a)))); b := ((b shl 21)or(b shr(32-21)))+c;
  inc(a,in_[4]+$f7537e82+(c xor(b or(not d))));  a := ((a shl 6)or(a shr(32-6)))+b;
  inc(d,in_[11]+$bd3af235+(b xor(a or(not c)))); d := ((d shl 10)or(d shr(32-10)))+a;
  inc(c,in_[2]+$2ad7d2bb+(a xor(d or(not b))));  c := ((c shl 15)or(c shr(32-15)))+d;
  inc(b,in_[9]+$eb86d391+(d xor(c or(not a))));  b := ((b shl 21)or(b shr(32-21)))+c;
  inc(buf[0],a);
  inc(buf[1],b);
  inc(buf[2],c);
  inc(buf[3],d);
end;

function TMD5.Final: TMD5Digest;
var count: Integer;
    p: ^Byte;
begin
  count := bytes[0] and $3f;  // Number of bytes in
  p := @in_;
  Inc(p, count);
  // Set the first char of padding to 0x80.  There is always room
  p^ := $80;
  Inc(p);
  // Bytes of padding needed to make 56 bytes (-8..55) 
  count := 56 - 1 - count;
  if count < 0 then begin  //  Padding forces an extra block 
    FillChar(p^, count + 8, 0);
    MD5Transform(buf, in_);
    p := @in_;
    count := 56;
  end;
  FillChar(p^, count, 0);
  // Append length in bits and transform 
  in_[14] := bytes[0] shl 3;
  in_[15] := (bytes[1] shl 3) or (bytes[0] shr 29);
  MD5Transform(buf, in_);
  Move(buf, Result, 16);
end;

procedure TMD5.Full(Buffer: pointer; Len: integer; out Digest: TMD5Digest);
begin
  Init;
  Update(Buffer^,Len);
  Digest := Final;
end;

procedure TMD5.Init;
begin
  buf[0] := $67452301;
  buf[1] := $efcdab89;
  buf[2] := $98badcfe;
  buf[3] := $10325476;
  bytes[0] := 0;
  bytes[1] := 0;
end;

procedure TMD5.Update(const buffer; len: Cardinal);
var p: ^TMD5In;
    t: cardinal;
    i: integer;
begin
  p := @buffer;
  // Update byte count
  t := bytes[0];
  Inc(bytes[0], len);
  if bytes[0]<t then
    Inc(bytes[1]);  // Carry from low to high
  t := 64 - (t and $3f);  // Space available in in (at least 1)
  if t>len then begin
    Move(p^, Pointer(Cardinal(@in_) + 64 - t)^, len);
    Exit;
  end;
  // First chunk is an odd size
  Move(p^, Pointer(Cardinal(@in_) + 64 - t)^, t);
  MD5Transform(buf, in_);
  inc(PtrUInt(p), t);
  dec(len, t);
  // Process data in 64-byte chunks
  for i := 1 to len div 64 do begin
    MD5Transform(buf, p^);
    inc(p);
  end;
  // Handle any remaining bytes of data.
  Move(p^, in_, len mod 64);
end;

function MD5Buf(const Buffer; Len: Cardinal): TMD5Digest;
var MD5: TMD5;
begin
  MD5.Full(@Buffer,Len,result);
end;

function MD5DigestsEqual(const A, B: TMD5Digest): Boolean;
begin
  result := CompareMem(@A,@B,sizeof(TMD5Digest));
end;

const Digits: array[0..15] of AnsiChar = '0123456789abcdef';

function MD5DigestToString(const D: TMD5Digest): RawUTF8;
var P: PAnsiChar;
    I: Integer;
begin
  SetLength(result,sizeof(D)*2);
  P := pointer(result);
  for I := 0 to sizeof(D)-1 do begin
    P[0] := Digits[D[I] shr 4];
    P[1] := Digits[D[I] and 15];
    Inc(P,2);
  end;
end;

function SHA1DigestToString(const D: TSHA1Digest): RawUTF8;
var P: PAnsiChar;
    I: Integer;
begin
  SetLength(result,sizeof(D)*2);
  P := pointer(result);
  for I := 0 to sizeof(D)-1 do begin
    P[0] := Digits[D[I] shr 4];
    P[1] := Digits[D[I] and 15];
    Inc(P,2);
  end;
end;

function SHA256DigestToString(const D: TSHA256Digest): RawUTF8;
var P: PAnsiChar;
    I: Integer;
begin
  SetLength(result,sizeof(D)*2);
  P := pointer(result);
  for I := 0 to sizeof(D)-1 do begin
    P[0] := Digits[D[I] shr 4];
    P[1] := Digits[D[I] and 15];
    Inc(P,2);
  end;
end;

function htdigest(const user, realm, pass: RawByteString): RawUTF8;
// apache-compatible: agent007:download area:8364d0044ef57b3defcfa141e8f77b65
//    hash=`echo -n "$user:$realm:$pass" | md5sum | cut -b -32`
//    echo "$user:$realm:$hash"
var tmp: RawByteString;
begin
  tmp := user+':'+realm+':';
  result := tmp+MD5(tmp+pass);
end;

function MD5SelfTest: boolean;
begin
  result := htdigest('agent007','download area','secret')=
    'agent007:download area:8364d0044ef57b3defcfa141e8f77b65';
end;

{ TSHA1 }

// TSHAContext = Hash,MLen,Buffer,Index
procedure TSHA1.Compress;
var
  A, B, C, D, E: cardinal;
  X: cardinal;
  W: array[0..79] of cardinal;
  i: integer;
begin
  // init W[] + A..E
  bswap256(@TSHAContext(Context).Buffer[0],@W[0]);
  bswap256(@TSHAContext(Context).Buffer[32],@W[8]);
  for i := 16 to 79 do begin
    X  := W[i-3] xor W[i-8] xor W[i-14] xor W[i-16];
    W[i]:= (X shl 1) or (X shr 31);
  end;
  with TSHAContext(Context) do begin
    A := Hash.A;
    B := Hash.B;
    C := Hash.C;
    D := Hash.D;
    E := Hash.E;
  end;

  // unrolled loop -> all is computed in cpu registers
  Inc(E,((A shl 5) or (A shr 27)) + (D xor (B and (C xor D))) + $5A827999 + W[ 0]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (C xor (A and (B xor C))) + $5A827999 + W[ 1]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (B xor (E and (A xor B))) + $5A827999 + W[ 2]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (A xor (D and (E xor A))) + $5A827999 + W[ 3]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (E xor (C and (D xor E))) + $5A827999 + W[ 4]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (D xor (B and (C xor D))) + $5A827999 + W[ 5]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (C xor (A and (B xor C))) + $5A827999 + W[ 6]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (B xor (E and (A xor B))) + $5A827999 + W[ 7]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (A xor (D and (E xor A))) + $5A827999 + W[ 8]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (E xor (C and (D xor E))) + $5A827999 + W[ 9]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (D xor (B and (C xor D))) + $5A827999 + W[10]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (C xor (A and (B xor C))) + $5A827999 + W[11]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (B xor (E and (A xor B))) + $5A827999 + W[12]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (A xor (D and (E xor A))) + $5A827999 + W[13]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (E xor (C and (D xor E))) + $5A827999 + W[14]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (D xor (B and (C xor D))) + $5A827999 + W[15]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (C xor (A and (B xor C))) + $5A827999 + W[16]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (B xor (E and (A xor B))) + $5A827999 + W[17]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (A xor (D and (E xor A))) + $5A827999 + W[18]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (E xor (C and (D xor E))) + $5A827999 + W[19]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $6ED9EBA1 + W[20]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $6ED9EBA1 + W[21]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $6ED9EBA1 + W[22]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $6ED9EBA1 + W[23]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $6ED9EBA1 + W[24]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $6ED9EBA1 + W[25]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $6ED9EBA1 + W[26]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $6ED9EBA1 + W[27]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $6ED9EBA1 + W[28]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $6ED9EBA1 + W[29]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $6ED9EBA1 + W[30]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $6ED9EBA1 + W[31]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $6ED9EBA1 + W[32]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $6ED9EBA1 + W[33]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $6ED9EBA1 + W[34]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $6ED9EBA1 + W[35]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $6ED9EBA1 + W[36]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $6ED9EBA1 + W[37]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $6ED9EBA1 + W[38]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $6ED9EBA1 + W[39]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + ((B and C) or (D and (B or C))) + $8F1BBCDC + W[40]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + ((A and B) or (C and (A or B))) + $8F1BBCDC + W[41]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + ((E and A) or (B and (E or A))) + $8F1BBCDC + W[42]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + ((D and E) or (A and (D or E))) + $8F1BBCDC + W[43]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + ((C and D) or (E and (C or D))) + $8F1BBCDC + W[44]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + ((B and C) or (D and (B or C))) + $8F1BBCDC + W[45]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + ((A and B) or (C and (A or B))) + $8F1BBCDC + W[46]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + ((E and A) or (B and (E or A))) + $8F1BBCDC + W[47]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + ((D and E) or (A and (D or E))) + $8F1BBCDC + W[48]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + ((C and D) or (E and (C or D))) + $8F1BBCDC + W[49]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + ((B and C) or (D and (B or C))) + $8F1BBCDC + W[50]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + ((A and B) or (C and (A or B))) + $8F1BBCDC + W[51]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + ((E and A) or (B and (E or A))) + $8F1BBCDC + W[52]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + ((D and E) or (A and (D or E))) + $8F1BBCDC + W[53]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + ((C and D) or (E and (C or D))) + $8F1BBCDC + W[54]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + ((B and C) or (D and (B or C))) + $8F1BBCDC + W[55]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + ((A and B) or (C and (A or B))) + $8F1BBCDC + W[56]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + ((E and A) or (B and (E or A))) + $8F1BBCDC + W[57]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + ((D and E) or (A and (D or E))) + $8F1BBCDC + W[58]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + ((C and D) or (E and (C or D))) + $8F1BBCDC + W[59]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $CA62C1D6 + W[60]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $CA62C1D6 + W[61]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $CA62C1D6 + W[62]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $CA62C1D6 + W[63]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $CA62C1D6 + W[64]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $CA62C1D6 + W[65]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $CA62C1D6 + W[66]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $CA62C1D6 + W[67]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $CA62C1D6 + W[68]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $CA62C1D6 + W[69]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $CA62C1D6 + W[70]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $CA62C1D6 + W[71]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $CA62C1D6 + W[72]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $CA62C1D6 + W[73]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $CA62C1D6 + W[74]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $CA62C1D6 + W[75]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $CA62C1D6 + W[76]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $CA62C1D6 + W[77]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $CA62C1D6 + W[78]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $CA62C1D6 + W[79]); C:= (C shl 30) or (C shr 2);

  // Calculate new working hash
  with TSHAContext(Context) do begin
    inc(Hash.A,A);
    inc(Hash.B,B);
    inc(Hash.C,C);
    inc(Hash.D,D);
    inc(Hash.E,E);
  end;
end;

procedure TSHA1.Final(out Digest: TSHA1Digest);
var Data: TSHAContext absolute Context;
begin
  // 1. append bit '1' after Buffer
  Data.Buffer[Data.Index]:= $80;
  fillchar(Data.Buffer[Data.Index+1],63-Data.Index,0);
  // 2. Compress if more than 448 bits, (no room for 64 bit length
  if Data.Index>=56 then begin
    Compress;
    fillchar(Data.Buffer,56,0);
  end;
  // Write 64 bit Buffer length into the last bits of the last block
  // (in big endian format) and do a final compress
  PInteger(@Data.Buffer[56])^ := bswap32(Int64Rec(Data.MLen).Hi);
  PInteger(@Data.Buffer[60])^ := bswap32(Int64Rec(Data.MLen).Lo);
  Compress;
  // Hash -> Digest to little endian format
  bswap160(@Data.Hash,@Digest);
  // Clear Data
  Init;
end;

procedure TSHA1.Full(Buffer: pointer; Len: integer; out Digest: TSHA1Digest);
begin
{$ifdef USEPADLOCK}
  // Padlock need all data once -> Full() is OK, not successive Update()
  if padlock_available then begin
    Init; // for later Update use
    {$ifdef PADLOCKDEBUG}write('padlock_phe_sha1 ');{$endif}
    if padlock_phe_sha1(buffer,Len,Digest)=0 then
      exit else
    {$ifdef PADLOCKDEBUG}write(':ERROR ');{$endif}
  end;
{$endif}
  Init;
  Update(Buffer,Len);
  Final(Digest);
end;

procedure TSHA1.Init;
// initialize context
var Data: TSHAContext absolute Context;
begin
  fillchar(Data,sizeof(Data),0);
  Data.Hash.A := $67452301;
  Data.Hash.B := $EFCDAB89;
  Data.Hash.C := $98BADCFE;
  Data.Hash.D := $10325476;
  Data.Hash.E := $C3D2E1F0;
end;

procedure TSHA1.Update(Buffer: pointer; Len: integer);
var Data: TSHAContext absolute Context;
    aLen: integer;
begin
  if Buffer=nil then exit; // avoid GPF
  inc(Data.MLen, Int64(cardinal(Len)) shl 3);
  while Len > 0 do begin
    aLen := sizeof(Data.Buffer)-Data.Index;
    if aLen<=Len then begin
      move(buffer^,Data.Buffer[Data.Index],aLen);
      dec(Len,aLen);
      inc(PtrUInt(buffer),aLen);
      Compress;
      Data.Index := 0;
    end else begin
      move(buffer^,Data.Buffer[Data.Index],Len);
      inc(Data.Index,Len);
      break;
    end;
  end;
end;


{ TAESAbstract }

procedure XorBlock16(A,B: PCardinalArray); overload;
begin
  A[0] := A[0] xor B[0];
  A[1] := A[1] xor B[1];
  A[2] := A[2] xor B[2];
  A[3] := A[3] xor B[3];
end;

procedure XorBlock16(A,B,C: PCardinalArray); overload;
begin
  B[0] := A[0] xor C[0];
  B[1] := A[1] xor C[1];
  B[2] := A[2] xor C[2];
  B[3] := A[3] xor C[3];
end;

procedure XorBlockN(A,B,C: PByteArray; Count: integer);
var i: integer;
begin
  for i := 0 to Count-1 do
    B[i] := A[i] xor C[i];
end;

const
  sAESException = 'AES engine initialization failure';

constructor TAESAbstract.Create(const aKey; aKeySize: cardinal;
  const aIV: TAESBlock);
begin
   if (aKeySize<>128) and (aKeySize<>192) and (aKeySize<>256) then
    raise Exception.CreateFmt(
      '%s.Create key size = %d; should be either 128, 192 or 256',
      [ClassName,aKeySize]);
  fKeySize := aKeySize;
  fKeySizeBytes := fKeySize shr 3;
  move(aKey,fKey,fKeySizeBytes);
  TAESContext(AES.Context).IV := aIV;
end;

procedure TAESAbstract.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  fIn := BufIn;
  fOut := BufOut;
  fCount := Count;
  CV := TAESContext(AES.Context).IV;
end;

procedure TAESAbstract.DecryptInit;
begin
  if not AES.DecryptInit(fKey,fKeySize) then
    raise Exception.Create(sAESException);
end;

function TAESAbstract.DecryptPKCS7(const Input: RawByteString): RawByteString;
var len: integer;
begin
  // validate input
  len := length(Input);
  if (len<AESBlockSize) or (len and (AESBlockSize-1)<>0) then
    raise Exception.Create('Invalid content');
  // decrypt
  SetLength(result,len);
  Decrypt(pointer(Input),pointer(result),len);
  // delete right padding
  if ord(result[len])>AESBlockSize then
    raise Exception.Create('Invalid content');
  SetLength(result,len-ord(result[len]));
end;

procedure TAESAbstract.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  fIn := BufIn;
  fOut := BufOut;
  fCount := Count;
  CV := TAESContext(AES.Context).IV;
end;

procedure TAESAbstract.EncryptInit;
begin
  if not AES.EncryptInit(fKey,fKeySize) then
    raise Exception.Create(sAESException);
end;

function TAESAbstract.EncryptPKCS7(const Input: RawByteString): RawByteString;
var len, padding: cardinal;
begin
  // use PKCS7 padding, so expects 16 bytes blocks
  len := length(Input);
  padding := 16-(len and (AESBlockSize-1));
  SetLength(result,len+padding);
  move(Pointer(Input)^,pointer(result)^,len);
  FillChar(PByteArray(result)^[len],padding,padding);
  // encryption
  Encrypt(pointer(result),pointer(result),len+padding);
end;

procedure TAESAbstract.EncryptTrailer;
var len: Cardinal;
begin
  len := fCount and (AESBlockSize-1);
  if len<>0 then begin
    AES.Encrypt(CV,CV);
    XorBlockN(pointer(fIn),pointer(fOut),@CV,len);
  end;
end;


{ TAESECB }

procedure TAESECB.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
begin
  inherited; // CV := IV + set fIn,fOut,fCount
  DecryptInit;
  for i := 1 to Count shr 4 do begin
    AES.Decrypt(fIn^,fOut^);
    inc(fIn);
    inc(fOut);
  end;
  XorBlockN(pointer(fIn),pointer(fOut),@CV,Count and (AESBlockSize-1));
end;

procedure TAESECB.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
begin
  inherited; // CV := IV + set fIn,fOut,fCount
  EncryptInit;
  for i := 1 to Count shr 4 do begin
    AES.Encrypt(fIn^,fOut^);
    inc(fIn);
    inc(fOut);
  end;
  XorBlockN(pointer(fIn),pointer(fOut),@CV,Count and (AESBlockSize-1));
end;


{ TAESCBC }

procedure TAESCBC.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
    tmp: TAESBlock;
begin
  inherited; // CV := IV + set fIn,fOut,fCount
  if Count>=AESBlockSize then begin
    DecryptInit;
    for i := 1 to Count shr 4 do begin
      tmp := fIn^;
      AES.Decrypt(fIn^,fOut^);
      XorBlock16(pointer(fOut),pointer(@CV));
      CV := tmp;
      inc(fIn);
      inc(fOut);
    end;
  end;
  Count := Count and (AESBlockSize-1);
  if Count<>0 then begin
    EncryptInit; // not set in EncryptTrailer -> use custom code
    AES.Encrypt(CV,CV);
    XorBlockN(pointer(fIn),pointer(fOut),@CV,Count);
  end;
end;

procedure TAESCBC.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
begin
  inherited; // CV := IV + set fIn,fOut,fCount
  EncryptInit;
  for i := 1 to Count shr 4 do begin
    XorBlock16(pointer(fIn),pointer(fOut),pointer(@CV));
    AES.Encrypt(fOut^,fOut^);
    CV := fOut^;
    inc(fIn);
    inc(fOut);
  end;
  EncryptTrailer;
end;


{ TAESCFB }

procedure TAESCFB.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
    tmp: TAESBlock;
begin
  inherited; // CV := IV + set fIn,fOut,fCount
  EncryptInit;
  for i := 1 to Count shr 4 do begin
    tmp := fIn^;
    AES.Encrypt(CV,CV);
    XorBlock16(pointer(fIn),pointer(fOut),pointer(@CV));
    CV := tmp;
    inc(fIn);
    inc(fOut);
  end;
  EncryptTrailer;
end;

procedure TAESCFB.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
begin
  inherited; // CV := IV + set fIn,fOut,fCount
  EncryptInit;
  for i := 1 to Count shr 4 do begin
    AES.Encrypt(CV,CV);
    XorBlock16(pointer(fIn),pointer(fOut),pointer(@CV));
    CV := fOut^;
    inc(fIn);
    inc(fOut);
  end;
  EncryptTrailer;
end;


{ TAESOFB }

procedure TAESOFB.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  Encrypt(BufIn, BufOut, Count); // by definition
end;

procedure TAESOFB.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
begin
  inherited; // CV := IV + set fIn,fOut,fCount
  EncryptInit;
  for i := 1 to Count shr 4 do begin
    AES.Encrypt(CV,CV);
    XorBlock16(pointer(fIn),pointer(fOut),pointer(@CV));
    inc(fIn);
    inc(fOut);
  end;
  EncryptTrailer;
end;


{ TAESCTR }

procedure TAESCTR.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  Encrypt(BufIn, BufOut, Count); // by definition
end;

procedure TAESCTR.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var i,j: integer;
    tmp: TAESBlock;
begin
  inherited; // CV := IV + set fIn,fOut,fCount
  EncryptInit;
  for i := 1 to Count shr 4 do begin
    AES.Encrypt(CV,tmp);
    inc(CV[7]);
    j := 7;
    while (j>0) and (CV[j]=0) do begin
      dec(j);
      inc(CV[j]);
    end;
    XorBlock16(pointer(fIn),pointer(fOut),pointer(@tmp));
    inc(fIn);
    inc(fOut);
  end;
  Count := Count and (AESBlockSize-1);
  if Count<>0 then begin
    AES.Encrypt(CV,tmp);
    XorBlockN(pointer(fIn),pointer(fOut),@tmp,Count);
  end;
end;


initialization
{$ifdef USEPADLOCK}
  PadlockInit;
{$endif}
  assert(sizeof(TAESContext)=AESContextSize);
  assert(sizeof(TSHAContext)=SHAContextSize);
  assert(sizeof(TAESFullHeader)=AESBlockSize);

finalization
{$ifdef USEPADLOCKDLL}
  if LibHandle<>0 then
    FreeLibrary(LibHandle); // same on Win+Linux, thanks to SysUtils
{$endif}
end.

