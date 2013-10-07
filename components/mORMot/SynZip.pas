/// low-level access to ZLib compression (1.2.5 engine version)
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.17
unit SynZip;

{
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

  ORIGINAL LICENSE:

  zlib.h -- interface of the 'zlib' general purpose compression library
  version 1.2.5, April 19th, 2010

  Copyright (C) 1995-2010 Jean-loup Gailly and Mark Adler

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  Jean-loup Gailly
  Mark Adler



    Cross-platform ZLib 1.2.5 implementation
   ==========================================

    Link to original C-compiled ZLib library
    - Win32: use fast obj and inline asm
    - Linux: use available system library libz.so
    Also defines .zip file structure (TFileInfo TFileHeader TLastHeader)

   Version 1.3
    - Delphi 2009/2010 compatibility (Unicode)

   Version 1.3.1 - January 23, 2010
    - issue corrected in CompressStream()
    - compilation of TSynZipCompressor under Delphi 2009/2010, without any
      Internal Error DT5830 (trigerred with my Delphi 2009 Update 3)

   Version 1.3.2 - February 5, 2010
    - added .zip direct reading class

   Version 1.4 - February 8, 2010
    - whole Synopse SQLite3 database framework released under the GNU Lesser
      General Public License version 3, instead of generic "Public Domain"

   Version 1.5 - February 11, 2010
    - added .zip direct writing class

   Version 1.9
   - crc32 is now coded in inlined fast asm (crc32.obj is no longer necessary)
   - crc32 hashing is performed using 8 tables, for better CPU pipelining and
     faster execution
   - crc32 tables are created on the fly during unit initialization, therefore
     save 8 KB of code size from standard crc32.obj, with no speed penalty

   Version 1.9.2
   - both obj files (i.e. deflate.obj and trees.obj) updated to version 1.2.5

   Version 1.13
   - code modifications to compile with Delphi 5 compiler
   - new CompressGZip and CompressDeflate functions, for THttpSocket.RegisterCompress
   - now handle Unicode file names UTF-8 encoded inside .Zip archive
   - new TZipWrite.CreateFrom constructor, to add some new content to an
     existing .Zip archive
   - EventArchiveZip function can be used as a TSynLogArchiveEvent handler to
     compress old .log files into a .zip standard archive

   Version 1.15
   - unit now tested with Delphi XE2 (32 Bit)

   Version 1.16
   - unit now compiles with Delphi XE2 (64 Bit)
   - TZipWrite.AddDeflated(const aFileName) method will use streaming instead
     of in-memory compression (will handle huge files much efficiently, e.g.
     log files as for EventArchiveZip)

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64

{$ifdef FPC}
  {$define USEZLIB}
{$else}
  {$ifdef Win32}
    {$define USEINLINEASM}
    // if defined, we use a special inlined asm version for uncompress:
    // seems 50% faster than BC++ generated .obj, and is 3KB smaller in code size
  {$else}
    {$define USEZLIB}
  {$endif}
{$endif}

interface

uses
  SysUtils, Classes,
{$ifdef USEZLIB}
  ZLib,
{$endif}
{$ifdef MSWINDOWS}
  Windows;
{$else}
  LibC;
{$endif}

type
  /// the format used for storing data
  TSynZipCompressorFormat = (szcfRaw, szcfZip, szcfGZ);

{$ifndef CONDITIONALEXPRESSIONS}
type // Delphi 5 doesn't have those base types defined :(
  PInteger = ^Integer;
  PCardinal = ^Cardinal;
  IntegerArray  = array[0..$effffff] of Integer;
  PIntegerArray = ^IntegerArray;
{$endif}

{/ in-memory ZLib DEFLATE compression }
function CompressMem(src, dst: pointer; srcLen, dstLen: integer;
  CompressionLevel: integer=6; ZipFormat: Boolean=false) : integer;

/// in-memory ZLib INFLATE decompression
function UnCompressMem(src, dst: pointer; srcLen, dstLen: integer) : integer;

/// ZLib DEFLATE compression from memory into a stream
function CompressStream(src: pointer; srcLen: integer;
  aStream: TStream; CompressionLevel:integer=6; ZipFormat: Boolean=false): cardinal;

/// ZLib INFLATE decompression from memory into a stream
// - return the number of bytes written into the stream
// - if checkCRC if not nil, it will contain thecrc32  (if aStream is nil, it will
// fast calculate the crc of the the uncompressed memory block)
function UnCompressStream(src: pointer; srcLen: integer; aStream: TStream; checkCRC: PCardinal): cardinal;


{$ifndef UNICODE}
type
  /// define RawByteString, as it does exist in Delphi 2009 and up
  // - to be used for byte storage into an AnsiString
  RawByteString = AnsiString;
  /// as available in newer Delphi versions
  NativeUInt = cardinal;
{$endif}

/// compress some data, with a proprietary format (including CRC)
function CompressString(const data: RawByteString; failIfGrow: boolean = false;
  CompressionLevel: integer=6) : RawByteString;

/// uncompress some data, with a proprietary format (including CRC)
// - return '' in case of a decompression failure
function UncompressString(const data: RawByteString) : RawByteString;


/// (un)compress a data content using the gzip algorithm
// - as expected by THttpSocket.RegisterCompress
// - will use internaly a level compression of 1, i.e. fastest available (content
// of 4803 bytes is compressed into 700, and time is 440 us instead of 220 us)
function CompressGZip(var Data: RawByteString; Compress: boolean): RawByteString;

/// (un)compress a data content using the Deflate algorithm
// - as expected by THttpSocket.RegisterCompress
// - will use internaly a level compression of 1, i.e. fastest available (content
// of 4803 bytes is compressed into 700, and time is 440 us instead of 220 us)
function CompressDeflate(var Data: RawByteString; Compress: boolean): RawByteString;



/// low-level check of the code returned by the ZLib library
function Check(const Code: Integer; const ValidCodes: array of Integer): integer;


type
  PCardinalArray = ^TCardinalArray;
  TCardinalArray = array[0..(MaxLongint div SizeOf(cardinal))-1] of cardinal;


/// just hash aString with CRC32 algorithm
// - crc32 is better than adler32 for short strings
function CRC32string(const aString: RawByteString): cardinal;

// don't know why using objects below produce an Internal Error DT5830
// under Delphi 2009 Update 3 !!!!!
// -> see http://qc.embarcadero.com/wc/qcmain.aspx?d=79792
// it seems that this compiler doesn't like to compile packed objects,
// but all other versions (including Delphi 2009 Update 2) did
// -> do Codegear knows about regression tests?

type
{$A-} { force packed object (not allowed under Delphi 2009) }
  /// the internal memory structure as expected by the ZLib library
  {$ifndef UNICODE}
  TZStream = object
  {$else}
  TZStream =  record
  {$endif}
    next_in : PAnsiChar;
    avail_in : cardinal;
    total_in : cardinal;
    next_out : PAnsiChar;
    avail_out : cardinal;
    total_out : cardinal;
    msg : PAnsiChar;
    state : pointer;
    zalloc : pointer;
    zfree : pointer;
    opaque : pointer;
    data_type: integer;
    adler : cardinal;
    reserved : cardinal;
    procedure Init;
    function DeflateInit(CompressionLevel: integer; ZipFormat: Boolean): Boolean;
  end;
  PZStream = ^TZStream;

  PFileInfo = ^TFileInfo;
  /// generic file information structure, as used in .zip file format
  // - used in any header, contains info about following block
  {$ifndef UNICODE}
  TFileInfo = object
  {$else}
  TFileInfo = record
  {$endif}
    neededVersion : word;            // $14
    flags         : word;            // 0
    zzipMethod    : word;            // 0=stored 8=deflate 12=BZ2 14=LZMA
    zlastMod      : integer;         // time in dos format
    zcrc32        : dword;           // crc32 checksum of uncompressed data
    zzipSize      : dword;           // size of compressed data
    zfullSize     : dword;           // size of uncompressed data
    nameLen       : word;            // length(name)
    extraLen      : word;            // 0
    function SameAs(aInfo: PFileInfo): boolean;
    function AlgoID: integer; // 1..15  (1=SynLZ e.g.) from flags
    procedure SetAlgoID(Algorithm: integer);
    function GetUTF8FileName: boolean;
    procedure SetUTF8FileName;
    procedure UnSetUTF8FileName;
  end;

  /// directory file information structure, as used in .zip file format
  // - used at the end of the zip file to recap all entries
  TFileHeader = {$ifdef UNICODE}record{$else}object{$endif}
    signature     : dword;           // $02014b50 PK#1#2
    madeBy        : word;            // $14
    fileInfo      : TFileInfo;
    commentLen    : word;            // 0
    firstDiskNo   : word;            // 0
    intFileAttr   : word;            // 0 = binary; 1 = text
    extFileAttr   : dword;           // dos file attributes
    localHeadOff  : dword;           // @TLocalFileHeader
    function DataPosition: cardinal;
    procedure Init;
  end;
  PFileHeader = ^TFileHeader;

  /// internal file information structure, as used in .zip file format
  // - used locally inside the file stream, followed by the name and then the data
  TLocalFileHeader = record
    signature     : dword;           // $04034b50 PK#3#4
    fileInfo      : TFileInfo;
  end;

  /// last header structure, as used in .zip file format
  // - this header ends the file and is used to find the TFileHeader entries
  TLastHeader = record
    signature     : dword;           // $06054b50 PK#5#6
    thisDisk      : word;            // 0
    headerDisk    : word;            // 0
    thisFiles     : word;            // 1
    totalFiles    : word;            // 1
    headerSize    : dword;           // sizeOf(TFileHeaders + names)
    headerOffset  : dword;           // @TFileHeader
    commentLen    : word;            // 0
  end;
{$A+}

{$ifdef linux}
const
  ZLIB_VERSION = '1.2.5';
  libz='libz.so';

type
  gzFile = pointer;


const
  Z_NO_FLUSH = 0;

  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH = 2;
  Z_FULL_FLUSH = 3;
  Z_FINISH = 4;

  Z_OK = 0;
  Z_STREAM_END = 1;
  Z_NEED_DICT = 2;
  Z_ERRNO = -(1);
  Z_STREAM_ERROR = -(2);
  Z_DATA_ERROR = -(3);
  Z_MEM_ERROR = -(4);
  Z_BUF_ERROR = -(5);
  Z_VERSION_ERROR = -(6);

  Z_NO_COMPRESSION = 0;
  Z_BEST_SPEED = 1;
  Z_BEST_COMPRESSION = 9;
  Z_DEFAULT_COMPRESSION = -(1);

  Z_FILTERED = 1;
  Z_HUFFMAN_ONLY = 2;
  Z_DEFAULT_STRATEGY = 0;

  Z_BINARY = 0;
  Z_ASCII = 1;
  Z_UNKNOWN = 2;

  Z_DEFLATED = 8;
  MAX_WBITS   = 15; // 32K LZ77 window
  DEF_MEM_LEVEL = 8;

  Z_NULL = 0;

function zlibVersion: PAnsiChar; cdecl;
function deflate(var strm: TZStream; flush: integer): integer; cdecl;
function deflateEnd(var strm: TZStream): integer; cdecl;
function inflate(var strm: TZStream; flush: integer): integer; cdecl;
function inflateEnd(var strm: TZStream): integer; cdecl;
function deflateSetDictionary(var strm: TZStream;dictionary : PAnsiChar; dictLength: cardinal): integer; cdecl;
function deflateCopy(var dest,source: TZStream): integer; cdecl;
function deflateReset(var strm: TZStream): integer; cdecl;
function deflateParams(var strm: TZStream; level: integer; strategy: integer): integer; cdecl;
function inflateSetDictionary(var strm: TZStream;dictionary : PAnsiChar; dictLength: cardinal): integer; cdecl;
function inflateSync(var strm: TZStream): integer; cdecl;
function inflateReset(var strm: TZStream): integer; cdecl;
function compress(dest: PAnsiChar;var destLen:cardinal; source : PAnsiChar; sourceLen:cardinal):integer; cdecl;
function compress2(dest: PAnsiChar;destLen:pcardinal; source : PAnsiChar; sourceLen:cardinal; level:integer):integer; cdecl;
function uncompress(dest: PAnsiChar;destLen:pcardinal; source : PAnsiChar; sourceLen:cardinal):integer; cdecl;
{
function gzopen(path: PAnsiChar; mode: PAnsiChar): gzFile; cdecl;
function gzdopen(fd: integer; mode: PAnsiChar): gzFile; cdecl;
function gzsetparams(thefile: gzFile; level: integer; strategy: integer): integer; cdecl;
function gzread(thefile: gzFile; buf: pointer; len:cardinal): integer; cdecl;
function gzwrite(thefile: gzFile; buf: pointer; len:cardinal): integer; cdecl;
function gzprintf(thefile: gzFile; format: PAnsiChar; args:array of const): integer; cdecl;
function gzputs(thefile: gzFile; s: PAnsiChar): integer; cdecl;
function gzgets(thefile: gzFile; buf: PAnsiChar; len: integer): PAnsiChar; cdecl;
function gzputc(thefile: gzFile; c:AnsiChar):AnsiChar; cdecl;
function gzgetc(thefile: gzFile):AnsiChar; cdecl;
function gzflush(thefile: gzFile; flush: integer): integer; cdecl;
function gzseek(thefile: gzFile; offset:integer; whence: integer):integer; cdecl;
function gzrewind(thefile: gzFile): integer; cdecl;
function gztell(thefile: gzFile):integer; cdecl;
function gzeof(thefile: gzFile):longbool; cdecl;
function gzclose(thefile: gzFile): integer; cdecl;
function gzerror(thefile: gzFile; var errnum: integer): PAnsiChar; cdecl;
}
function adler32(adler:cardinal; buf: PAnsiChar; len: cardinal): cardinal; cdecl;
function crc32(crc:cardinal; buf: PAnsiChar; len: cardinal): cardinal; cdecl;
function deflateInit_(var strm: TZStream; level: integer; version: PAnsiChar; stream_size: integer): integer; cdecl;
function inflateInit_(var strm: TZStream; version: PAnsiChar; stream_size: integer): integer; cdecl;
function deflateInit2_(var strm: TZStream; level: integer; method: integer; windowBits: integer; memLevel: integer;strategy: integer; version: PAnsiChar; stream_size: integer): integer; cdecl;
function inflateInit2_(var strm: TZStream; windowBits: integer; version: PAnsiChar; stream_size: integer): integer; cdecl;
function get_crc_table: pointer; cdecl;

function zlibAllocMem(AppData: Pointer; Items, Size: cardinal): Pointer; cdecl;
procedure zlibFreeMem(AppData, Block: Pointer);  cdecl;

{$else}
{ our very own short implementation of ZLibH }

const
  ZLIB_VERSION = '1.2.5';

  Z_NO_FLUSH = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH = 2;
  Z_FULL_FLUSH = 3;
  Z_FINISH = 4;

  Z_OK = 0;
  Z_STREAM_END = 1;
  Z_MEM_ERROR = -(4);
  Z_BUF_ERROR = -(5);

  Z_DEFLATED = 8;
  MAX_WBITS   = 15; // 32K LZ77 window
  DEF_MEM_LEVEL = 8;
  Z_DEFAULT_STRATEGY = 0;
  Z_HUFFMAN_ONLY = 2;

function deflateInit2_(var strm: TZStream; level: integer; method: integer; windowBits: integer; memLevel: integer;strategy: integer; version: PAnsiChar; stream_size: integer): integer;
function deflate(var strm: TZStream; flush: integer): integer;
function deflateEnd(var strm: TZStream): integer;

function inflateInit2_(var strm: TZStream; windowBits: integer; version: PAnsiChar; stream_size: integer): integer;
  {$ifdef USEINLINEASM}stdcall;{$endif}
function inflate(var strm: TZStream; flush: integer): integer;
  {$ifdef USEINLINEASM}stdcall;{$endif}
function inflateEnd(var strm: TZStream): integer;
  {$ifdef USEINLINEASM}stdcall;{$endif}


function adler32(adler: cardinal; buf: PAnsiChar; len: cardinal): cardinal;
function crc32(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;
function get_crc_table: pointer;

{$endif Linux}


{/ uncompress a .gz file content
 - return '' if the .gz content is invalid (e.g. bad crc) }
function GZRead(gz: PAnsiChar; gzLen: integer): RawByteString;

type
  /// a simple TStream descendant for compressing data into a stream
  // - this simple version don't use any internal buffer, but rely
  // on Zip library buffering system
  // - the version in SynZipFiles is much more powerfull, but this one
  // is sufficient for most common cases (e.g. for on the fly .gz backup)
  TSynZipCompressor = class(TStream)
  private
    fInitialized: Boolean;
    fDestStream: TStream;
    fStrm: TZStream;
    fCRC: Cardinal;
    fGZFormat: boolean;
    fBufferOut: array[word] of byte; // a 64 KB buffer
    function FlushBufferOut: integer;
  public
    {/ create a compression stream, writting the compressed data into
      the specified stream (e.g. a file stream) }
    constructor Create(outStream: TStream; CompressionLevel: Integer;
      Format: TSynZipCompressorFormat = szcfRaw);
    /// release memory
    destructor Destroy; override;
    /// this method will raise an error: it's a compression-only stream
    function Read(var Buffer; Count: Longint): Longint; override;
    /// add some data to be compressed
    function Write(const Buffer; Count: Longint): Longint; override;
    /// used to return the current position, i.e. the real byte written count
    // - for real seek, this method will raise an error: it's a compression-only stream
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    /// the number of byte written, i.e. the current uncompressed size
    property SizeIn: cardinal read FStrm.total_in;
    /// write all pending compressed data into outStream 
    procedure Flush;
    /// the number of byte sent to the destination stream, i.e. the current
    // compressed size
    property SizeOut: cardinal read FStrm.total_out;
    /// the current CRC of the written data, i.e. the uncompressed data CRC
    property CRC: cardinal read fCRC;
  end;

  /// stores an entry of a file inside a .zip archive
  TZipEntry = record
    /// the information of this file, as stored in the .zip archive
    info: PFileInfo;
    /// points to the compressed data in the .zip archive, mapped in memory
    data: PAnsiChar;
    /// name of the file inside the .zip archive
    // - not ASCIIZ: length = info.nameLen
    storedName: PAnsiChar;
    /// name of the file inside the .zip archive
    // - converted from DOS/OEM or UTF-8 into generic (Unicode) string
    zipName: TFileName;
  end;

{$ifndef LINUX}
  /// read-only access to a .zip archive file
  // - can open directly a specified .zip file (will be memory mapped for fast access)
  // - can open a .zip archive file content from a resource (embedded in the executable)
  // - can open a .zip archive file content from memory
  TZipRead = class
  private
    file_, map: NativeUInt; // we use a memory mapped file to access the zip content
    buf: PByteArray;
    ReadOffset: cardinal;
    procedure UnMap;
  public
    /// the number of files inside a .zip archive
    Count: integer;
    /// the files inside the .zip archive
    Entry: array of TZipEntry;

    /// open a .zip archive file as Read Only
    constructor Create(const aFileName: TFileName; ZipStartOffset: cardinal=0;
      Size: cardinal=0); overload;
    /// open a .zip archive file directly from a resource
    constructor Create(Instance: THandle; const ResName: string; ResType: PChar); overload;
    /// open a .zip archive file from its File Handle
    constructor Create(aFile: THandle; ZipStartOffset: cardinal=0;
      Size: cardinal=0); overload;
    /// open a .zip archive file directly from memory
    constructor Create(BufZip: pByteArray; Size: cardinal); overload;
    /// release associated memory
    destructor  Destroy; override;

    /// get the index of a file inside the .zip archive
    function NameToIndex(const aName: TFileName): integer;
    /// uncompress a file stored inside the .zip archive into memory
    function UnZip(aIndex: integer): RawByteString; overload;
    /// uncompress a file stored inside the .zip archive into a destination directory
    function UnZip(aIndex: integer; const DestDir: TFileName): boolean; overload;
  end;
{$endif Linux}

  /// write-only access for creating a .zip archive file
  // - not to be used to update a .zip file, but to create a new one
  // - update can be done manualy by using a TZipRead instance and the
  // AddFromZip() method
  TZipWrite = class
  protected
    fAppendOffset: cardinal;
    fFileName: TFileName;
    fMagic: cardinal;
    function InternalAdd(const zipName: TFileName; Buf: pointer; Size: integer): cardinal;
  public
    /// the associated file handle
    Handle: integer;
    /// the total number of entries
    Count: integer;
    /// the resulting file entries, ready to be written as a .zip catalog
    // - those will be appended after the data blocks at the end of the .zip file
    Entry: array of record
      /// the file name, as stored in the .zip internal directory
      intName: RawByteString;
      /// the corresponding file header
      fhr: TFileHeader;
    end;
    /// initialize the .zip file
    // - a new .zip file content is created
    constructor Create(const aFileName: TFileName); overload;
    /// initialize an existing .zip file in order to add some content to it
    // - warning: AddStored/AddDeflated() won't check for duplicate zip entries
    // - this method is very fast, and will increase the .zip file in-place
    // (the old content is not copied, new data is appended at the file end)
    constructor CreateFrom(const aFileName: TFileName);
    /// compress (using the deflate method) a memory buffer, and add it to the zip file
    // - by default, the 1st of January, 2010 is used if not date is supplied
    procedure AddDeflated(const aZipName: TFileName; Buf: pointer; Size: integer;
      CompressLevel: integer=6; FileAge: integer=1+1 shl 5+30 shl 9); overload;
    /// compress (using the deflate method) a file, and add it to the zip file
    procedure AddDeflated(const aFileName: TFileName; RemovePath: boolean=true;
      CompressLevel: integer=6); overload;
    /// add a memory buffer to the zip file, without compression
    // - content is stored, not deflated
    // (in that case, no deflate code is added to the executable)
    // - by default, the 1st of January, 2010 is used if not date is supplied
    procedure AddStored(const aZipName: TFileName; Buf: pointer; Size: integer;
      FileAge: integer=1+1 shl 5+30 shl 9);
    /// add a file from an already compressed zip entry
    procedure AddFromZip(const ZipEntry: TZipEntry);
    /// append a file content into the destination file
    // - usefull to add the initial Setup.exe file, e.g.
    procedure Append(const Content: RawByteString);
    /// release associated memory, and close destination file
    destructor Destroy; override;
  end;

/// a TSynLogArchiveEvent handler which will compress older .log files
// into .zip archive files
// - resulting file will be named YYYYMM.zip and will be located in the
// aDestinationPath directory, i.e. TSynLogFamily.ArchivePath+'\log\YYYYMM.zip' 
function EventArchiveZip(const aOldLogFileName, aDestinationPath: TFileName): boolean;


implementation

{ TZipWrite }

var
  EventArchiveZipWrite: TZipWrite = nil;

function EventArchiveZip(const aOldLogFileName, aDestinationPath: TFileName): boolean;
var n: integer;
begin
  result := false;
  if aOldLogFileName='' then
    FreeAndNil(EventArchiveZipWrite) else begin
    if not FileExists(aOldLogFileName) then
      exit;
    if EventArchiveZipWrite=nil then
      EventArchiveZipWrite := TZipWrite.CreateFrom(
        copy(aDestinationPath,1,length(aDestinationPath)-1)+'.zip');
    n := EventArchiveZipWrite.Count;
    EventArchiveZipWrite.AddDeflated(aOldLogFileName,True);
    if (EventArchiveZipWrite.Count=n+1) and DeleteFile(pointer(aOldLogFileName)) then
      result := True;
  end;
end;

procedure TZipWrite.AddDeflated(const aFileName: TFileName; RemovePath: boolean=true;
      CompressLevel: integer=6);
var Time: TFileTime;
    ZipName: TFileName;
    FileTime: LongRec;
    Size: Int64;
    Size64: Int64Rec absolute Size;
    OffsHead, OffsEnd: cardinal;
    S: TFileStream;
    D: THandleStream;
    Z: TSynZipCompressor;
begin
  S := TFileStream.Create(aFileName,fmOpenRead or fmShareDenyNone);
  try
    if RemovePath then
      ZipName := ExtractFileName(aFileName) else
      ZipName := aFileName;
    GetFileTime(S.Handle,nil,nil,@Time);
    FileTimeToLocalFileTime(Time,Time);
    FileTimeToDosDateTime(Time,FileTime.Hi,FileTime.Lo);
    Size := S.Size;
    if Size64.Hi<>0 then
      raise Exception.CreateFmt('%s file too big for .zip',[aFileName]);
    if Count>=length(Entry) then
      SetLength(Entry,length(Entry)+20);
    OffsHead := InternalAdd(ZipName,nil,0);
    D := THandleStream.Create(Handle);
    Z := TSynZipCompressor.Create(D,CompressLevel);
    try
      Z.CopyFrom(S,Size64.Lo);
      Z.Flush;
      assert(Z.SizeIn=Size64.Lo);
      with Entry[Count] do begin
        with fhr.fileInfo do begin
          zcrc32 := Z.CRC;
          zfullSize := Z.SizeIn;
          zzipSize := Z.SizeOut;
          zzipMethod := Z_DEFLATED;
          zlastMod := integer(FileTime);
        end;
        OffsEnd := D.Position;
        D.Position := OffsHead+sizeof(fMagic);
        D.Write(fhr.fileInfo,sizeof(fhr.fileInfo));
        D.Position := OffsEnd;
      end;
      inc(Count);
    finally
      Z.Free;
      D.Free;
    end;
  finally
    S.Free;
  end;
end;

function Is7BitAnsi(P: PChar): boolean;
begin
  if P<>nil then
    while true do
      if ord(P^)=0 then
        break else
      if ord(P^)<=126 then
        inc(P) else begin
        result := false;
        exit;
      end;
  result := true;
end;

function TZipWrite.InternalAdd(const zipName: TFileName; Buf: pointer; Size: integer): cardinal;
begin
  with Entry[Count] do begin
    fHr.signature := $02014b51;
    dec(fHr.signature); // +1 to avoid finding it in the exe
    fHr.madeBy := $14;
    fHr.fileInfo.neededVersion := $14;
    result := SetFilePointer(Handle,0,nil,FILE_CURRENT);
    fHr.localHeadOff := result-fAppendOffset;
    {$ifdef CONDITIONALEXPRESSIONS}
    // Delphi 5 doesn't have UTF8Decode/UTF8Encode functions -> make 7 bit version
    if Is7BitAnsi(pointer(zipName)) then begin
    {$endif}
      {$ifdef UNICODE}
      intName := AnsiString(zipName);
      {$else}  // intName := zipName -> error reference count under Delphi 6
      SetString(intName,PAnsiChar(pointer(zipName)),length(zipName));
      {$endif}
      fHr.fileInfo.UnSetUTF8FileName;
    {$ifdef CONDITIONALEXPRESSIONS}
    end else begin
      intName := UTF8Encode(WideString(zipName));
      fHr.fileInfo.SetUTF8FileName;
    end;
    {$endif}
    fHr.fileInfo.nameLen := length(intName);
    FileWrite(Handle,fMagic,sizeof(fMagic));
    FileWrite(Handle,fhr.fileInfo,sizeof(fhr.fileInfo));
    FileWrite(Handle,pointer(intName)^,fhr.fileInfo.nameLen);
  end;
  if Buf<>nil then begin
    FileWrite(Handle,Buf^,Size); // write stored data
    inc(Count);
  end;
end;

procedure TZipWrite.AddDeflated(const aZipName: TFileName; Buf: pointer;
  Size, CompressLevel, FileAge: integer);
var tmp: pointer;
    tmpsize: integer;
begin
  if (self=nil) or (Handle<=0) then
    exit;
  if Count>=length(Entry) then
    SetLength(Entry,length(Entry)+20);
  with Entry[Count] do begin
    with fhr.fileInfo do begin
      zcrc32 := SynZip.crc32(0,Buf,Size);
      zfullSize := Size;
      zzipMethod := Z_DEFLATED;
      zlastMod := FileAge;
      tmpsize := (Size*11) div 10+12;
      Getmem(tmp,tmpSize);
      zzipSize := CompressMem(Buf,tmp,Size,tmpSize,CompressLevel);
      InternalAdd(aZipName,tmp,zzipSize); // write stored data
      Freemem(tmp);
    end;
  end;
end;

procedure TZipWrite.AddFromZip(const ZipEntry: TZipEntry);
begin
  if (self=nil) or (Handle<=0) then
    exit;
  if Count>=length(Entry) then
    SetLength(Entry,length(Entry)+20);
  with Entry[Count] do begin
    fhr.fileInfo := ZipEntry.info^;
    InternalAdd(ZipEntry.zipName,ZipEntry.data,fhr.fileInfo.zzipSize);
  end;
end;

procedure TZipWrite.AddStored(const aZipName: TFileName; Buf: pointer;
  Size, FileAge: integer);
begin
  if (self=nil) or (Handle<=0) then
    exit;
  if Count>=length(Entry) then
    SetLength(Entry,length(Entry)+20);
  with Entry[Count], fhr.fileInfo do begin
    zcrc32 := SynZip.crc32(0,Buf,Size);
    zfullSize := Size;
    zzipSize := Size;
    zlastMod := FileAge;
    InternalAdd(aZipName,Buf,Size);
  end;
end;

procedure TZipWrite.Append(const Content: RawByteString);
begin
  if (self=nil) or (Handle<=0) or (fAppendOffset<>0) then
    exit;
  fAppendOffset := length(Content);
  FileWrite(Handle,pointer(Content)^,fAppendOffset);
end;

constructor TZipWrite.Create(const aFileName: TFileName);
begin
  fFileName := aFileName;
  fMagic := $04034b50+1; // +1 to avoid finding it in the exe generated code
  dec(fMagic);
  if Handle=0 then
    Handle := FileCreate(aFileName);
end;

constructor TZipWrite.CreateFrom(const aFileName: TFileName);
var R: TZipRead;
    i: Integer;
begin
  Handle := FileOpen(aFileName,fmOpenReadWrite or fmShareDenyNone);
  if Handle<0 then begin
    R := nil;
    Handle := 0;
  end else
    R := TZipRead.Create(Handle);
  Create(aFileName);
  if R<>nil then
  try
    Count := R.Count;
    SetLength(Entry,Count+10);
    for i := 0 to Count-1 do
    with Entry[i], R.Entry[i] do begin
      fhr.Init;
      fhr.localHeadOff := NativeUInt(info)-NativeUInt(R.Entry[0].info);
      fhr.fileInfo := info^;
      SetString(intName,storedName,info^.nameLen);
    end;
    SetFilePointer(Handle,R.ReadOffset,nil,FILE_BEGIN);
  finally
    R.Free;
  end;
end;

destructor TZipWrite.Destroy;
var lhr: TLastHeader;
    i: integer;
begin
  fillchar(lhr,sizeof(lhr),0);
  lhr.signature := $06054b50+1; dec(lhr.signature); // +1 to avoid finding it in the exe
  lhr.thisFiles := Count;
  lhr.totalFiles := Count;
  lhr.headerOffset := SetFilePointer(Handle,0,nil,FILE_CURRENT)-fAppendOffset;
  for i := 0 to Count-1 do
  with Entry[i] do begin
    assert(fhr.fileInfo.nameLen=length(intName));
    inc(lhr.headerSize,sizeof(TFileHeader)+fhr.fileInfo.nameLen);
    FileWrite(Handle,fhr,sizeof(fhr));
    FileWrite(Handle,pointer(IntName)^,fhr.fileInfo.nameLen);
  end;
  FileWrite(Handle,lhr,sizeof(lhr));
  SetEndOfFile(Handle);
  FileClose(Handle);
  inherited;
end;

{$ifndef Linux}

procedure TZipRead.UnMap;
begin
  Count := 0;
  if map<>0 then begin
    UnmapViewOfFile(Buf);
    CloseHandle(map);
  end;
  if file_>0 then begin
    CloseHandle(file_);
    file_ := 0;
  end;
end;

{$ifdef UNICODE}
function UTF8Decode(const tmp: UTF8String): TFileName;
begin
  result := TFileName(tmp);
end;
{$endif}

constructor TZipRead.Create(BufZip: pByteArray; Size: cardinal);
var lhr: ^TLastHeader;
    h: ^TFileHeader;
    lfhr: ^TLocalFileHeader;
    i,j: integer;
    {$ifdef CONDITIONALEXPRESSIONS}
    tmp: UTF8String;
    {$else}
    tmp: RawByteString;
    {$endif}
begin
  for i := 0 to 31 do begin // resources size may be rounded up to alignment
    lhr := @BufZip[Size-sizeof(TLastHeader)];
    if lhr^.signature+1=$06054b51 then
      break;
    dec(Size);
    if Size<=sizeof(lhr^) then
      break;
  end;
  if lhr^.signature+1<>$06054b51 then begin
    UnMap;
    raise Exception.Create('ZIP format');
    exit;
  end;
  SetLength(Entry,lhr^.totalFiles); // fill Entry[] with the Zip headers
  H := @BufZip[lhr^.headerOffset];
  ReadOffset := lhr^.headerOffset;
  for i := 1 to lhr^.totalFiles do begin
    if H^.signature+1<>$02014b51 then begin
      UnMap;
      raise Exception.Create('ZIP format');
      exit;
    end;
    lfhr := @BufZip[H^.localHeadOff];
    with Entry[Count] do begin
      info := @lfhr^.fileInfo;
      storedName := PAnsiChar(lfhr)+sizeof(lfhr^);
      data := storedName+info^.NameLen+info^.extraLen; // data mapped in memory
      SetString(tmp,storedName,info^.nameLen);
      for j := 0 to info^.nameLen-1 do
        if storedName[j]='/' then // normalize path delimiter
          PAnsiChar(Pointer(tmp))[j] := '\';
      {$ifdef CONDITIONALEXPRESSIONS}
      // Delphi 5 doesn't have UTF8Decode/UTF8Encode functions -> make 7 bit version
      if info^.GetUTF8FileName then
        // decode UTF-8 file name into native string/TFileName type
        zipName := UTF8Decode(tmp) else
      {$endif}
      begin
        // decode OEM/DOS file name into native string/TFileName type
        SetLength(zipName,info^.nameLen);
        OemToChar(Pointer(tmp),Pointer(zipName)); // OemToCharW/OemToCharA
      end;
      inc(NativeUInt(H),sizeof(H^)+info^.NameLen+H^.fileInfo.extraLen+H^.commentLen);
      if (info^.zZipMethod in [0,Z_DEFLATED]) and
        (zipName<>'') and (zipName[length(zipName)]<>'\') then // ignore folder
        inc(Count); // known methods: stored + deflate
    end;
  end;
end;

constructor TZipRead.Create(Instance: THandle; const ResName: string; ResType: PChar);
// locked resources are memory map of the executable -> direct access is easy
var HResInfo: THandle;
    HGlobal: THandle;
begin
  HResInfo := FindResource(Instance,PChar(ResName),ResType);
  if HResInfo=0 then
    exit;
  HGlobal := LoadResource(HInstance, HResInfo);
  if HGlobal<>0 then
    // warning: resources size may be rounded up to alignment -> handled in Create()
    Create(LockResource(HGlobal),SizeofResource(HInstance, HResInfo));
end;

constructor TZipRead.Create(aFile: THandle; ZipStartOffset: cardinal=0;
      Size: cardinal=0);
var i, ExeOffset: integer;
begin
  if aFile<=0 then
    exit;
  if Size=0 then
    Size := GetFileSize(aFile, nil);
  map := CreateFileMapping(aFile, nil, PAGE_READONLY, 0, 0, nil);
  if map=0 then begin
    Unmap;
    raise Exception.Create('Missing File');
    exit;
  end;
  Buf := MapViewOfFile(map, FILE_MAP_READ, 0, 0, 0);
  ExeOffset := -1;
  for i := ZipStartOffset to Size-5 do
    if PCardinal(@buf[i])^+1=$04034b51 then begin // +1 to avoid finding it in the exe part
      ExeOffset := i;
      break;
    end;
  if ExeOffset<0 then begin
    Unmap;
    raise Exception.Create('No ZIP found');
    exit;
  end;
  Create(@Buf[ExeOffset],integer(Size)-ExeOffset);
end;

constructor TZipRead.Create(const aFileName: TFileName; ZipStartOffset, Size: cardinal);
begin
  file_ := CreateFile(pointer(aFileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  Create(file_,ZipStartOffset, Size);
end;

destructor TZipRead.Destroy;
begin
  UnMap;
  inherited;
end;

function TZipRead.NameToIndex(const aName: TFileName): integer;
begin
  if (self<>nil) and (aName<>'') then
    for result := 0 to Count-1 do
      if SameText(Entry[result].zipName,aName) then
        exit;
  result := -1;
end;

function TZipRead.UnZip(aIndex: integer): RawByteString;
var len: cardinal;
begin
  result := ''; // somewhat faster if memory is reallocated each time
  if (self=nil) or (cardinal(aIndex)>=cardinal(Count)) then
    exit;
  with Entry[aIndex] do begin
    SetLength(result,info^.zfullSize);
    if info^.zZipMethod=0 then begin // stored method
      len := info^.zfullsize;
      move(data^,pointer(result)^,len);
    end else // deflate method
      len := UnCompressMem(data,pointer(result),info^.zzipsize,info^.zfullsize);
    if (len<>info^.zfullsize) or
       (info^.zcrc32<>SynZip.crc32(0,pointer(result),info^.zfullSize)) then begin
      MessageBox(0,'CRC',pointer(zipName),0); // Name is PAnsiChar -> force MessageBoxA()
      result := '';
    end;
  end;
end;

{$ifndef CONDITIONALEXPRESSIONS}
/// DirectoryExists returns a boolean value that indicates whether the
//  specified directory exists (and is actually a directory)
function DirectoryExists(const Directory: string): boolean;
var Code: Integer;
begin
  Code := GetFileAttributes(PChar(Directory));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;
{$endif}

function EnsurePath(var Path: TFileName): boolean;
var Parent: TFileName;
begin
  result := false;
  if Path='' then exit;
  if Path[length(Path)]<>'\' then
    Path := Path+'\';
  if DirectoryExists(Path) then
    result := true else begin
    Parent := ExtractFilePath(copy(Path,1,length(Path)-1));
    if (Parent<>'') and not DirectoryExists(Parent) then
      if not EnsurePath(Parent) then exit;
    if CreateDirectory(pointer(path),nil) then
      result := true;
  end;
end;

function TZipRead.UnZip(aIndex: integer; const DestDir: TFileName): boolean;
var FS: TFileStream;
    Path: TFileName;
    CRC: Cardinal;
begin
  result := false;
  if (self=nil) or (cardinal(aIndex)>=cardinal(Count)) then
    exit;
  with Entry[aIndex] do begin
    Path := DestDir+ExtractFilePath(zipName); // include sub directories
    if not EnsurePath(Path) then
      exit;
    FS := TFileStream.Create(Path+ExtractFileName(zipName),fmCreate);
    try
      if info^.zZipMethod=0 then begin
        FS.Write(data^,info^.zfullsize);
        CRC := SynZip.crc32(0,data,info^.zfullSize);
      end else
        if UnCompressStream(data,info^.zzipsize,FS,@CRC)<>info^.zfullsize then
          exit;
      result := CRC=info^.zcrc32;
      if info^.zlastMod<>0 then
{$ifdef CONDITIONALEXPRESSIONS}
  {$WARN SYMBOL_PLATFORM OFF}
{$endif}
        FileSetDate(FS.Handle,info^.zlastMod);
{$ifdef CONDITIONALEXPRESSIONS}
  {$WARN SYMBOL_PLATFORM ON}
{$endif}
    finally
      FS.Free;
    end;
  end;
end;

{$endif Linux}

function GZRead(gz: PAnsiChar; gzLen: integer): RawByteString;
var Len: integer;
begin
  result := '';
  if (gz=nil) or (gzLen<=18) or (PCardinal(gz)^<>$88B1F) then
    exit; // it MUST be a true .gz file
  Len := pInteger(@gz[gzLen-4])^;
  assert(cardinal(Len)<cardinal(gzLen)*64); // basic content check
  if Len=0 then
    exit;
  SetLength(result,Len);
  if UnCompressMem(@gz[10],pointer(result),gzLen-18,Len)<>Len then
    result := '' else
    if SynZip.crc32(0,pointer(result),Len)<>pCardinal(@gz[gzLen-8])^ then
      result := ''; // invalid CRC
end;


{$ifdef Linux}
function zlibVersion: PAnsiChar; cdecl; external libz name 'zlibVersion';
function deflate; cdecl; external libz name 'deflate';
function deflateEnd(var strm: TZStream): integer; cdecl; external libz name 'deflateEnd';
function inflate(var strm: TZStream; flush: integer): integer; cdecl; external libz name 'inflate';
function inflateEnd(var strm: TZStream): integer; cdecl; external libz name 'inflateEnd';
function deflateSetDictionary(var strm: TZStream;dictionary : PAnsiChar; dictLength: cardinal): integer; cdecl; external libz name 'deflateSetDictionary';
function deflateCopy(var dest,source: TZStream): integer; cdecl; external libz name 'deflateCopy';
function deflateReset(var strm: TZStream): integer; cdecl; external libz name 'deflateReset';
function deflateParams(var strm: TZStream; level: integer; strategy: integer): integer; cdecl; external libz name 'deflateParams';
function inflateSetDictionary(var strm: TZStream;dictionary : PAnsiChar; dictLength: cardinal): integer; cdecl; external libz name 'inflateSetDictionary';
function inflateSync(var strm: TZStream): integer; cdecl; external libz name 'inflateSync';
function inflateReset(var strm: TZStream): integer; cdecl; external libz name 'inflateReset';
function compress(dest: PAnsiChar;var destLen:cardinal; source : PAnsiChar; sourceLen:cardinal):integer; cdecl; external libz name 'compress';
function compress2(dest: PAnsiChar;destLen:pcardinal; source : PAnsiChar; sourceLen:cardinal; level:integer):integer; cdecl; external libz name 'compress2';
function uncompress(dest: PAnsiChar;destLen:pcardinal; source : PAnsiChar; sourceLen:cardinal):integer; cdecl; external libz name 'uncompress';
{
function gzopen(path: PAnsiChar; mode: PAnsiChar): gzFile; cdecl; external libz name 'gzopen';
function gzdopen(fd: integer; mode: PAnsiChar): gzFile; cdecl; external libz name 'gzdopen';
function gzsetparams(thefile: gzFile; level: integer; strategy: integer): integer; cdecl; external libz name 'gzsetparams';
function gzread(thefile: gzFile; buf: pointer; len:cardinal): integer; cdecl; external libz name 'gzread';
function gzwrite(thefile: gzFile; buf: pointer; len:cardinal): integer; cdecl; external libz name 'gzwrite';
function gzprintf(thefile: gzFile; format: PAnsiChar; args:array of const): integer; cdecl; external libz name 'gzprintf';
function gzputs(thefile: gzFile; s: PAnsiChar): integer; cdecl; external libz name 'gzputs';
function gzgets(thefile: gzFile; buf: PAnsiChar; len: integer): PAnsiChar; cdecl; external libz name 'gzgets';
function gzputc(thefile: gzFile; c:AnsiChar):AnsiChar; cdecl; external libz name 'gzputc';
function gzgetc(thefile: gzFile):AnsiChar; cdecl; external libz name 'gzgetc';
function gzflush(thefile: gzFile; flush: integer): integer; cdecl; external libz name 'gzflush';
function gzseek(thefile: gzFile; offset:integer; whence: integer):integer; cdecl; external libz name 'gzseek';
function gzrewind(thefile: gzFile): integer; cdecl; external libz name 'gzrewind';
function gztell(thefile: gzFile):integer; cdecl; external libz name 'gztell';
function gzeof(thefile: gzFile):longbool; cdecl; external libz name 'gzeof';
function gzclose(thefile: gzFile): integer; cdecl; external libz name 'gzclose';
function gzerror(thefile: gzFile; var errnum: integer): PAnsiChar; cdecl; external libz name 'gzerror';
}
function adler32(adler:cardinal; buf: PAnsiChar; len: cardinal):cardinal; cdecl; external libz name 'adler32';
function crc32(crc:cardinal;buf : PAnsiChar; len: cardinal):cardinal; cdecl; external libz name 'crc32';
function deflateInit_(var strm: TZStream; level: integer; version: PAnsiChar; stream_size: integer): integer; cdecl; external libz name 'deflateInit_';
function inflateInit_(var strm: TZStream; version: PAnsiChar; stream_size: integer): integer; cdecl; external libz name 'inflateInit_';
function deflateInit2_(var strm: TZStream; level: integer; method: integer; windowBits: integer; memLevel: integer;strategy: integer; version: PAnsiChar; stream_size: integer): integer; cdecl; external libz name 'deflateInit2_';
function inflateInit2_(var strm: TZStream; windowBits: integer; version: PAnsiChar; stream_size: integer): integer; cdecl; external libz name 'inflateInit2_';
function get_crc_table: pointer; cdecl; external libz name 'get_crc_table';

function zlibAllocMem(AppData: Pointer; Items, Size: cardinal): Pointer; cdecl;
begin
  Getmem(Result,Items * Size);
end;

procedure zlibFreeMem(AppData, Block: Pointer);  cdecl;
begin
  FreeMem(Block);
end;

procedure TZStream.Init;
begin
  fillchar(Self,sizeof(Self),0);
  zalloc := @zlibAllocMem; // even under Linux, use program heap
  zfree := @zlibFreeMem;
end;

{$else} // Windows:

{$ifndef USEZLIB}
// our very own short implementation of ZLibH

// some small functions (adler32.obj, zutil.obj, compress.obj, uncompress.obj,
// crc32.obj) are coded directly in this unit, not as external .obj files

// this external obj code was compiled with pascal register call conventions
{$ifdef FPC} // FPC expects coff format
  {$LINK deflate.o}
  {$LINK trees.o}
{$else}      // Delphi works with omf32 format
  {$LINK deflate.obj}
  {$LINK trees.obj}
{$endif}

function deflateInit2_(var strm: TZStream; level: integer; method: integer; windowBits: integer; memLevel: integer;strategy: integer; version: PAnsiChar; stream_size: integer): integer; external;
function deflate(var strm: TZStream; flush: integer): integer; external;
function deflateEnd(var strm: TZStream): integer; external;

const
  _z_errmsg: array[0..9] of PAnsiChar = (
    'need dictionary',     // Z_NEED_DICT       2
    'stream end',          // Z_STREAM_END      1
    '',                    // Z_OK              0
    'file error',          // Z_ERRNO         (-1)
    'stream error',        // Z_STREAM_ERROR  (-2)
    'data error',          // Z_DATA_ERROR    (-3)
    'insufficient memory', // Z_MEM_ERROR     (-4)
    'buffer error',        // Z_BUF_ERROR     (-5)
    'incompatible version',// Z_VERSION_ERROR (-6)
    '');

{$ifdef USEINLINEASM}

const
  Err246: array[0..3] of AnsiChar = 'lit';
  Err247: array[0..3] of AnsiChar = 'dst';
  Err248: array[0..3] of AnsiChar = 'inv';

procedure Inline018;
asm
        sub     esp, 60
        push    ebx
        push    ebp
        push    esi
        push    edi
        mov     eax, dword ptr [esp + 50H]
        mov     edx, dword ptr [eax]
        mov     ecx, dword ptr [eax + 04H]
        mov     ebx, dword ptr [esp + 54H]
        dec     edx
        lea     ecx, [ecx + edx - 05H]
        mov     ebp, dword ptr [eax + 1CH]
        mov     dword ptr [esp + 30H], ecx
        mov     esi, dword ptr [eax + 0CH]
        mov     eax, dword ptr [eax + 10H]
        mov     ecx, eax
        sub     ecx, ebx
        dec     esi
        lea     eax, [eax + esi - 00000101H]
        add     ecx, esi
        mov     dword ptr [esp + 34H], eax
        mov     eax, dword ptr [ebp + 2CH]
        mov     dword ptr [esp + 44H], ecx
        mov     ecx, dword ptr [ebp + 28H]
        mov     dword ptr [esp + 48H], eax
        mov     eax, dword ptr [ebp + 34H]
        mov     ebx, dword ptr [ebp + 3CH]
        mov     dword ptr [esp + 28H], ecx
        mov     ecx, dword ptr [ebp + 30H]
        mov     dword ptr [esp + 2CH], eax
        mov     eax, dword ptr [ebp + 50H]
        mov     dword ptr [esp + 1CH], ecx
        mov     ecx, dword ptr [ebp + 4CH]
        mov     dword ptr [esp + 24H], eax
        mov     dword ptr [esp + 20H], ecx
        mov     ecx, dword ptr [ebp + 54H]
        mov     eax, 1
        shl     eax, cl
        mov     ecx, dword ptr [ebp + 58H]
        mov     edi, dword ptr [ebp + 38H]
        dec     eax
        mov     dword ptr [esp + 38H], eax
        mov     eax, 1
        shl     eax, cl
        mov     dword ptr [esp + 18H], ebp
        mov     dword ptr [esp + 14H], edx
        dec     eax
        mov     dword ptr [esp + 3CH], eax
@@019:  cmp     ebx, 15
        jnc     @@020
        movzx   eax, byte ptr [edx + 01H]
        inc     edx
        mov     ecx, ebx
        shl     eax, cl
        add     ebx, 8
        mov     ecx, ebx
        add     edi, eax
        movzx   eax, byte ptr [edx + 01H]
        inc     edx
        shl     eax, cl
        mov     dword ptr [esp + 14H], edx
        add     edi, eax
        add     ebx, 8
@@020:  mov     ecx, dword ptr [esp + 38H]
        mov     eax, dword ptr [esp + 20H]
        and     ecx, edi
        mov     eax, dword ptr [eax + ecx*4]
        movzx   ecx, ah
        mov     dword ptr [esp + 10H], eax
        movzx   eax, al
        shr     edi, cl
        sub     ebx, ecx
        test    eax, eax
        jz      @@022
@@021:  test    al, 10H
        jnz     @@023
        test    al, 40H
        jne     @@046
        mov     ecx, 1
        mov     dword ptr [esp + 54H], ecx
        mov     ecx, eax
        mov     eax, dword ptr [esp + 54H]
        shl     eax, cl
        mov     ecx, dword ptr [esp + 10H]
        shr     ecx, 16
        dec     eax
        and     eax, edi
        add     eax, ecx
        mov     ecx, dword ptr [esp + 20H]
        mov     eax, dword ptr [ecx + eax*4]
        movzx   ecx, ah
        mov     dword ptr [esp + 10H], eax
        movzx   eax, al
        shr     edi, cl
        sub     ebx, ecx
        test    eax, eax
        jnz     @@021
@@022:  mov     eax, dword ptr [esp + 10H]
        inc     esi
        shr     eax, 16
        mov     byte ptr [esi], al
        jmp     @@043

@@023:  mov     ecx, dword ptr [esp + 10H]
        shr     ecx, 16
        and     eax, 0000000FH
        mov     dword ptr [esp + 54H], ecx
        jz      @@025
        cmp     ebx, eax
        jnc     @@024
        movzx   ebp, byte ptr [edx + 01H]
        inc     edx
        mov     ecx, ebx
        shl     ebp, cl
        mov     dword ptr [esp + 14H], edx
        add     edi, ebp
        add     ebx, 8
@@024:  mov     ecx, eax
        mov     ebp, 1
        shl     ebp, cl
        mov     ecx, dword ptr [esp + 54H]
        dec     ebp
        and     ebp, edi
        add     ecx, ebp
        mov     ebp, dword ptr [esp + 18H]
        mov     dword ptr [esp + 54H], ecx
        mov     ecx, eax
        shr     edi, cl
        sub     ebx, eax
@@025:  cmp     ebx, 15
        jnc     @@026
        movzx   eax, byte ptr [edx + 01H]
        inc     edx
        mov     ecx, ebx
        shl     eax, cl
        add     ebx, 8
        mov     ecx, ebx
        add     edi, eax
        movzx   eax, byte ptr [edx + 01H]
        inc     edx
        shl     eax, cl
        mov     dword ptr [esp + 14H], edx
        add     edi, eax
        add     ebx, 8
@@026:  mov     ecx, dword ptr [esp + 3CH]
        mov     eax, dword ptr [esp + 24H]
        and     ecx, edi
        mov     eax, dword ptr [eax + ecx*4]
        movzx   ecx, ah
        mov     dword ptr [esp + 10H], eax
        movzx   eax, al
        shr     edi, cl
        sub     ebx, ecx
        test    al, 10H
        jnz     @@028
@@027:  test    al, 40H
        jne     @@045
        mov     ecx, 1
        mov     dword ptr [esp + 40H], ecx
        mov     ecx, eax
        mov     eax, dword ptr [esp + 40H]
        shl     eax, cl
        mov     ecx, dword ptr [esp + 10H]
        shr     ecx, 16
        dec     eax
        and     eax, edi
        add     eax, ecx
        mov     ecx, dword ptr [esp + 24H]
        mov     eax, dword ptr [ecx + eax*4]
        movzx   ecx, ah
        mov     dword ptr [esp + 10H], eax
        movzx   eax, al
        shr     edi, cl
        sub     ebx, ecx
        test    al, 10H
        jz      @@027
@@028:  mov     ecx, dword ptr [esp + 10H]
        shr     ecx, 16
        and     eax, 0000000FH
        cmp     ebx, eax
        mov     dword ptr [esp + 10H], ecx
        jnc     @@029
        movzx   ebp, byte ptr [edx + 01H]
        inc     edx
        mov     ecx, ebx
        shl     ebp, cl
        add     ebx, 8
        mov     dword ptr [esp + 14H], edx
        add     edi, ebp
        cmp     ebx, eax
        jnc     @@029
        movzx   ebp, byte ptr [edx + 01H]
        inc     edx
        mov     ecx, ebx
        shl     ebp, cl
        mov     dword ptr [esp + 14H], edx
        add     edi, ebp
        add     ebx, 8
@@029:  mov     ecx, 1
        mov     ebp, ecx
        mov     ecx, eax
        shl     ebp, cl
        sub     ebx, eax
        dec     ebp
        and     ebp, edi
        mov     ecx, ebp
        mov     ebp, dword ptr [esp + 10H]
        add     ebp, ecx
        mov     ecx, eax
        shr     edi, cl
        mov     ecx, dword ptr [esp + 44H]
        mov     eax, esi
        sub     eax, ecx
        cmp     ebp, eax
        mov     dword ptr [esp + 10H], ebp
        jbe     @@040
        sub     ebp, eax
        cmp     ebp, dword ptr [esp + 48H]
        ja      @@044
        mov     ecx, dword ptr [esp + 2CH]
        mov     eax, dword ptr [esp + 1CH]
        dec     ecx
        test    eax, eax
        jnz     @@031
        mov     eax, dword ptr [esp + 28H]
        sub     eax, ebp
        add     ecx, eax
        mov     eax, dword ptr [esp + 54H]
        cmp     ebp, eax
        jae     @@037
        sub     eax, ebp
        mov     dword ptr [esp + 54H], eax

@@030:  mov     al, byte ptr [ecx + 01H]
        inc     ecx
        inc     esi
        dec     ebp
        mov     byte ptr [esi], al
        jnz     @@030
        jmp     @@036

@@031:  cmp     eax, ebp
        jnc     @@034
        sub     eax, ebp
        add     eax, dword ptr [esp + 28H]
        add     ecx, eax
        sub     ebp, dword ptr [esp + 1CH]
        mov     eax, dword ptr [esp + 54H]
        cmp     ebp, eax
        jnc     @@037
        sub     eax, ebp
        mov     dword ptr [esp + 54H], eax

@@032:  mov     al, byte ptr [ecx + 01H]
        inc     ecx
        inc     esi
        dec     ebp
        mov     byte ptr [esi], al
        jnz     @@032
        mov     ecx, dword ptr [esp + 2CH]
        mov     eax, dword ptr [esp + 1CH]
        mov     ebp, dword ptr [esp + 54H]
        dec     ecx
        cmp     eax, ebp
        jnc     @@037
        sub     ebp, eax
        mov     dword ptr [esp + 54H], ebp
        mov     dword ptr [esp + 40H], eax
        mov     ebp, eax
@@033:  mov     al, byte ptr [ecx + 01H]
        inc     ecx
        inc     esi
        dec     ebp
        mov     byte ptr [esi], al
        jnz     @@033
        jmp     @@036

@@034:  sub     eax, ebp
        add     ecx, eax
        mov     eax, dword ptr [esp + 54H]
        cmp     ebp, eax
        jnc     @@037
        sub     eax, ebp
        mov     dword ptr [esp + 54H], eax
@@035:  mov     al, byte ptr [ecx + 01H]
        inc     ecx
        inc     esi
        dec     ebp
        mov     byte ptr [esi], al
        jnz     @@035
@@036:  mov     eax, dword ptr [esp + 10H]
        mov     ecx, esi
        sub     ecx, eax
@@037:  mov     eax, dword ptr [esp + 54H]
        cmp     eax, 2
        jbe     @@039
        lea     edx, [eax - 03H]
        mov     eax, -1431655765
        mul     edx
        mov     ebp, edx
        shr     ebp, 1
        inc     ebp
        nop
@@038:  mov     al, byte ptr [ecx + 01H]
        inc     ecx
        inc     esi
        mov     byte ptr [esi], al
        mov     dl, byte ptr [ecx + 01H]
        inc     ecx
        inc     esi
        mov     byte ptr [esi], dl
        mov     edx, dword ptr [esp + 54H]
        mov     al, byte ptr [ecx + 01H]
        inc     ecx
        inc     esi
        sub     edx, 3
        dec     ebp
        mov     byte ptr [esi], al
        mov     dword ptr [esp + 54H], edx
        jnz     @@038
        mov     edx, dword ptr [esp + 14H]
@@039:  mov     eax, dword ptr [esp + 54H]
        test    eax, eax
        jz      @@042
        mov     al, byte ptr [ecx + 01H]
        inc     ecx
        inc     esi
        mov     byte ptr [esi], al
        cmp     dword ptr [esp + 54H], 1
        jbe     @@042
        mov     cl, byte ptr [ecx + 01H]
        inc     esi
        mov     byte ptr [esi], cl
        jmp     @@042

@@040:  mov     eax, esi
        sub     eax, ebp

@@041:  mov     cl, byte ptr [eax + 01H]
        inc     eax
        inc     esi
        mov     byte ptr [esi], cl
        mov     cl, byte ptr [eax + 01H]
        inc     eax
        inc     esi
        mov     byte ptr [esi], cl
        mov     cl, byte ptr [eax + 01H]
        inc     eax
        inc     esi
        mov     byte ptr [esi], cl
        mov     ecx, dword ptr [esp + 54H]
        sub     ecx, 3
        cmp     ecx, 2
        mov     dword ptr [esp + 54H], ecx
        ja      @@041
        test    ecx, ecx
        jz      @@042
        mov     cl, byte ptr [eax + 01H]
        inc     eax
        inc     esi
        mov     byte ptr [esi], cl
        cmp     dword ptr [esp + 54H], 1
        jbe     @@042
        mov     al, byte ptr [eax + 01H]
        inc     esi
        mov     byte ptr [esi], al
@@042:  mov     ebp, dword ptr [esp + 18H]
@@043:  cmp     edx, dword ptr [esp + 30H]
        jnc     @@049
        cmp     esi, dword ptr [esp + 34H]
        jb      @@019
        jmp     @@049

@@044:  mov     ecx, dword ptr [esp + 50H]
        mov     eax, dword ptr [esp + 18H]
        mov     dword ptr [ecx + 18H],offset Err248
        mov     dword ptr [eax], 27
        mov     ebp, eax
        jmp     @@049

@@045:  mov     ecx, dword ptr [esp + 50H]
        mov     dword ptr [ecx + 18H], offset Err247
        jmp     @@048

@@046:  test    al, 20H
        jz      @@047
        mov     dword ptr [ebp], 11
        jmp     @@049

@@047:  mov     eax, dword ptr [esp + 50H]
        mov     dword ptr [eax + 18H], offset Err246
@@048:  mov     dword ptr [ebp], 27
@@049:  mov     eax, ebx
        shr     eax, 3
        lea     ecx, [eax*8]
        sub     edx, eax
        sub     ebx, ecx
        mov     ecx, ebx
        mov     eax, 1
        shl     eax, cl
        mov     ecx, dword ptr [esp + 50H]
        dec     eax
        and     edi, eax
        lea     eax, [edx + 01H]
        mov     dword ptr [ecx], eax
        lea     eax, [esi + 01H]
        mov     dword ptr [ecx + 0CH], eax
        mov     eax, dword ptr [esp + 30H]
        sub     eax, edx
        add     eax, 5
        mov     dword ptr [ecx + 04H], eax
        mov     eax, dword ptr [esp + 34H]
        sub     eax, esi
        add     eax, 257
        mov     dword ptr [ecx + 10H], eax
        mov     dword ptr [ebp + 38H], edi
        pop     edi
        pop     esi
        mov     dword ptr [ebp + 3CH], ebx
        pop     ebp
        pop     ebx
        add     esp, 60
        ret     8
end;

procedure inflateReset;
asm
        mov     edx, dword ptr [esp + 04H]
        xor     ecx, ecx
        cmp     edx, ecx
        jz      @@050
        mov     eax, dword ptr [edx + 1CH]
        cmp     eax, ecx
        jz      @@050
        mov     dword ptr [eax + 1CH], ecx
        mov     dword ptr [edx + 14H], ecx
        mov     dword ptr [edx + 08H], ecx
        mov     dword ptr [edx + 18H], ecx
        mov     dword ptr [edx + 30H], 1
        mov     dword ptr [eax], ecx
        mov     dword ptr [eax + 04H], ecx
        mov     dword ptr [eax + 0CH], ecx
        mov     dword ptr [eax + 20H], ecx
        mov     dword ptr [eax + 28H], ecx
        mov     dword ptr [eax + 2CH], ecx
        mov     dword ptr [eax + 30H], ecx
        mov     dword ptr [eax + 38H], ecx
        mov     dword ptr [eax + 3CH], ecx
        lea     ecx, [eax + 00000530H]
        mov     dword ptr [eax + 14H], 32768
        mov     dword ptr [eax + 6CH], ecx
        mov     dword ptr [eax + 50H], ecx
        mov     dword ptr [eax + 4CH], ecx
        xor     eax, eax
        ret     4

@@050:  mov     eax, -2
        ret     4
end;

function zsalloc(AppData: Pointer; Items, Size: cardinal): Pointer; stdcall;
begin // direct use of the (FastMM4) delphi heap for all inflate memory allocation
  Getmem(Result,Items * Size);
end;

procedure zsfree(AppData, Block: Pointer); stdcall;
begin // direct use of the (FastMM4) delphi heap for all inflate memory allocation
  FreeMem(Block);
end;

function inflateInit2_;
asm  pop ebp  // auto-generated push ebp; mov ebp,esp
        mov     eax, dword ptr [esp + 0CH]
        push    edi
        xor     edi, edi
        cmp     eax, edi
        je      @@058
        cmp     byte ptr [eax], 49
        jne     @@058
        cmp     dword ptr [esp + 14H], 56
        jnz     @@058
        push    esi
        mov     esi, dword ptr [esp + 0CH]
        cmp     esi, edi
        jz      @@057
        mov [esi].TZStream.zFree,offset zsfree
        mov [esi].TZStream.zAlloc,offset zsalloc
        mov     dword ptr [esi + 18H], edi
        mov eax,9520
        call System.@GetMem
        mov [esi].TZStream.State,eax
        mov     ecx, dword ptr [esp + 10H]
        cmp     ecx, edi
        jge     @@054
        mov     dword ptr [eax + 08H], edi
        neg     ecx
        jmp     @@055

@@054:  mov     edx, ecx
        sar     edx, 4
        inc     edx
        mov     dword ptr [eax + 08H], edx
@@055:  cmp     ecx, 8
        jl      @@056
        cmp     ecx, 15
        jg      @@056
        push    esi
        mov     dword ptr [eax + 24H], ecx
        mov     dword ptr [eax + 34H], edi
        call    inflateReset
        pop     esi
        pop     edi
        ret     16

@@056:  push    eax
        mov     eax, dword ptr [esi + 28H]
        push    eax
        call    dword ptr [esi + 24H]
        mov     dword ptr [esi + 1CH], edi
@@057:  pop     esi
        mov     eax, -2
        pop     edi
        ret     16

@@058:  mov     eax, -6
        pop     edi
        ret     16
end;


procedure Inline059;
asm
        push    ebx
        push    ebp
        mov     ebp, dword ptr [esp + 0CH]
        mov     ebx, dword ptr [ebp + 1CH]
        push    esi
        push    edi
        mov     esi, eax
        mov     eax, dword ptr [ebx + 34H]
        xor     edi, edi
        cmp     eax, edi
        jnz     @@060
        mov     ecx, dword ptr [ebx + 24H]
        mov     eax, 1
        shl     eax, cl
        mov     ecx, dword ptr [ebp + 28H]
        push    1
        push    eax
        push    ecx
        call    dword ptr [ebp + 20H]
        cmp     eax, edi
        mov     dword ptr [ebx + 34H], eax
        jnz     @@060
        pop     edi
        pop     esi
        pop     ebp
        mov     eax, 1
        pop     ebx
        ret     4

@@060:  cmp     dword ptr [ebx + 28H], edi
        jnz     @@061
        mov     ecx, dword ptr [ebx + 24H]
        mov     edx, 1
        shl     edx, cl
        mov     dword ptr [ebx + 30H], edi
        mov     dword ptr [ebx + 2CH], edi
        mov     dword ptr [ebx + 28H], edx
@@061:  mov     edi, dword ptr [ebp + 10H]
        mov     ecx, dword ptr [ebx + 28H]
        sub     esi, edi
        mov     eax, esi
        cmp     eax, ecx
        jc      @@062
        mov     esi, dword ptr [ebp + 0CH]
        mov     edi, dword ptr [ebx + 34H]
        sub     esi, ecx
        mov     eax, ecx
        shr     ecx, 2
        rep movsd
        mov     ecx, eax
        and     ecx, 00000003H
        rep movsb
        mov     ecx, dword ptr [ebx + 28H]
        pop     edi
        pop     esi
        pop     ebp
        mov     dword ptr [ebx + 30H], 0
        mov     dword ptr [ebx + 2CH], ecx
        xor     eax, eax
        pop     ebx
        ret     4

@@062:  sub     ecx, dword ptr [ebx + 30H]
        cmp     ecx, eax
        mov     dword ptr [esp + 14H], ecx
        jbe     @@063
        mov     ecx, eax
        mov     dword ptr [esp + 14H], ecx
@@063:  mov     edx, dword ptr [ebx + 30H]
        mov     edi, dword ptr [ebx + 34H]
        mov     esi, dword ptr [ebp + 0CH]
        add     edi, edx
        mov     edx, ecx
        shr     ecx, 2
        sub     esi, eax
        rep movsd
        mov     ecx, edx
        and     ecx, 00000003H
        rep movsb
        mov     ecx, dword ptr [esp + 14H]
        sub     eax, ecx
        jz      @@064
        mov     esi, dword ptr [ebp + 0CH]
        mov     edi, dword ptr [ebx + 34H]
        mov     ecx, eax
        mov     edx, ecx
        sub     esi, eax
        shr     ecx, 2
        rep movsd
        mov     ecx, edx
        and     ecx, 00000003H
        rep movsb
        pop     edi
        mov     dword ptr [ebx + 30H], eax
        mov     eax, dword ptr [ebx + 28H]
        pop     esi
        mov     dword ptr [ebx + 2CH], eax
        pop     ebp
        xor     eax, eax
        pop     ebx
        ret     4

@@064:  mov     edi, dword ptr [ebx + 30H]
        mov     eax, dword ptr [ebx + 28H]
        add     edi, ecx
        mov     edx, edi
        cmp     edx, eax
        mov     dword ptr [ebx + 30H], edi
        jnz     @@065
        mov     dword ptr [ebx + 30H], 0
@@065:  mov     edx, dword ptr [ebx + 2CH]
        cmp     edx, eax
        jnc     @@066
        add     edx, ecx
        mov     dword ptr [ebx + 2CH], edx
@@066:  pop     edi
        pop     esi
        pop     ebp
        xor     eax, eax
        pop     ebx
        ret     4
end;

function inflate;
asm  pop ebp  // auto-generated push ebp; mov ebp,esp
        mov     eax, dword ptr [esp + 04H]
        sub     esp, 52
        test    eax, eax
        push    ebx
        je      @@191
        mov     ebx, dword ptr [eax + 1CH]
        test    ebx, ebx
        je      @@191
        mov     ecx, dword ptr [eax + 0CH]
        test    ecx, ecx
        je      @@191
        cmp     dword ptr [eax], 0
        jnz     @@067
        mov     ecx, dword ptr [eax + 04H]
        test    ecx, ecx
        jne     @@191
@@067:  cmp     dword ptr [ebx], 11
        jnz     @@068
        mov     dword ptr [ebx], 12
@@068:  mov     ecx, dword ptr [eax + 0CH]
        mov     edx, dword ptr [ebx + 38H]
        push    ebp
        mov     ebp, dword ptr [ebx + 3CH]
        push    esi
        mov     esi, dword ptr [eax]
        push    edi
        mov     edi, dword ptr [eax + 04H]
        mov     dword ptr [esp + 24H], ecx
        mov     ecx, dword ptr [eax + 10H]
        mov     eax, dword ptr [ebx]
        cmp     eax, 28
        mov     dword ptr [esp + 1CH], ecx
        mov     dword ptr [esp + 14H], esi
        mov     dword ptr [esp + 18H], edi
        mov     dword ptr [esp + 10H], edx
        mov     dword ptr [esp + 38H], edi
        mov     dword ptr [esp + 28H], ecx
        mov     dword ptr [esp + 2CH], 0
        ja      @@176
@@069:  jmp     dword ptr [@@192 + eax*4]

@@070:  mov     eax, dword ptr [ebx + 08H]
        test    eax, eax
        jnz     @@071
        mov     dword ptr [ebx], 12
        jmp     @@175

@@071:  cmp     ebp, 16
        jnc     @@073
@@072:  test    edi, edi
        je      @@183
        movzx   eax, byte ptr [esi]
        mov     ecx, ebp
        shl     eax, cl
        dec     edi
        add     ebp, 8
        mov     dword ptr [esp + 18H], edi
        add     edx, eax
        inc     esi
        cmp     ebp, 16
        mov     dword ptr [esp + 10H], edx
        mov     dword ptr [esp + 14H], esi
        jc      @@072
@@073:  mov     eax, edx
        and     eax, 000000FFH
        shr     edx, 8
        shl     eax, 8
        add     eax, edx
        xor     edx, edx
        mov     ecx, 31
        div     ecx
        test    edx, edx
        jz      @@074
        mov     edx, dword ptr [esp + 48H]
        mov     dword ptr [edx + 18H], offset Err248
        mov     edx, dword ptr [esp + 10H]
        jmp     @@174

@@074:  mov     ecx, dword ptr [esp + 10H]
        mov     eax, ecx
        and     al, 0FH
        cmp     al, 8
        jz      @@075
        mov     ecx, dword ptr [esp + 48H]
        mov     edx, dword ptr [esp + 10H]
        mov     dword ptr [ecx + 18H], offset Err248
        jmp     @@174

@@075:  mov     eax, dword ptr [ebx + 24H]
        shr     ecx, 4
        mov     dword ptr [esp + 10H], ecx
        and     ecx, 0000000FH
        add     ecx, 8
        sub     ebp, 4
        cmp     ecx, eax
        jbe     @@076
        mov     edx, dword ptr [esp + 48H]
        mov     dword ptr [edx + 18H], offset Err248
        mov     edx, dword ptr [esp + 10H]
        jmp     @@174

@@076:  mov     eax, 1
        shl     eax, cl
        mov     dword ptr [ebx + 14H], eax
        xor eax,eax
        xor ecx,ecx
        xor edx,edx
        xor ebp,ebp
        call    Adler32
        mov     edx, dword ptr [esp + 10H]
        mov     ecx, dword ptr [esp + 48H]
        shr     edx, 8
        not     edx
        and     edx, 00000002H
        or      edx, 00000009H
        mov     dword ptr [ebx + 18H], eax
        mov     dword ptr [ecx + 30H], eax
        mov     dword ptr [ebx], edx
        mov     dword ptr [esp + 10H], ebp
        mov     edx, ebp
        jmp     @@175

@@077:  cmp     ebp, 32
        jnc     @@079
@@078:  test    edi, edi
        je      @@183
        movzx   eax, byte ptr [esi]
        mov     ecx, ebp
        shl     eax, cl
        dec     edi
        add     ebp, 8
        mov     dword ptr [esp + 18H], edi
        add     edx, eax
        inc     esi
        cmp     ebp, 32
        mov     dword ptr [esp + 10H], edx
        mov     dword ptr [esp + 14H], esi
        jc      @@078
@@079:  mov     ecx, edx
        and     ecx, 0000FF00H
        mov     eax, edx
        shl     eax, 16
        add     ecx, eax
        xor     eax, eax
        mov     ah, byte ptr [esp + 12H]
        shl     ecx, 8
        shr     edx, 24
        add     ecx, eax
        lea     eax, [ecx + edx]
        mov     ecx, dword ptr [esp + 48H]
        xor     ebp, ebp
        mov     dword ptr [ebx + 18H], eax
        mov     dword ptr [ecx + 30H], eax
        mov     dword ptr [esp + 10H], ebp
        mov     dword ptr [ebx], 10
        mov     edx, ebp

@@080:  mov     eax, dword ptr [ebx + 0CH]
        test    eax, eax
        je      @@178
        xor eax,eax
        xor ecx,ecx
        xor edx,edx
        call    Adler32
        mov     edx, dword ptr [esp + 48H]
        mov     dword ptr [ebx + 18H], eax
        mov     dword ptr [edx + 30H], eax
        mov     edx, dword ptr [esp + 10H]
        mov     dword ptr [ebx], 11

@@081:  cmp     dword ptr [esp + 4CH], 5
        je      @@183

@@082:  mov     eax, dword ptr [ebx + 04H]
        test    eax, eax
        jz      @@083
        mov     ecx, ebp
        and     ecx, 00000007H
        shr     edx, cl
        sub     ebp, ecx
        mov     dword ptr [ebx], 24
        mov     dword ptr [esp + 10H], edx
        jmp     @@175

@@083:  cmp     ebp, 3
        jnc     @@085

@@084:  test    edi, edi
        je      @@183
        movzx   eax, byte ptr [esi]
        mov     ecx, ebp
        shl     eax, cl
        dec     edi
        add     ebp, 8
        mov     dword ptr [esp + 18H], edi
        add     edx, eax
        inc     esi
        cmp     ebp, 3
        mov     dword ptr [esp + 14H], esi
        jc      @@084
@@085:  mov     ecx, edx
        shr     edx, 1
        and     ecx, 00000001H
        mov     eax, edx
        and     eax, 00000003H
        dec     ebp
        cmp     eax, 3
        mov     dword ptr [ebx + 04H], ecx
        ja      @@090
        jmp     dword ptr [@@193 + eax*4]

@@086:  shr     edx, 2
        mov     dword ptr [ebx], 13
        mov     dword ptr [esp + 10H], edx
        sub     ebp, 2
        jmp     @@175

@@087:  shr     edx, 2
        mov     dword ptr [ebx + 4CH], offset @@249
        mov     dword ptr [ebx + 54H], 9
        mov     dword ptr [ebx + 50H], offset @@250
        mov     dword ptr [ebx + 58H], 5
        mov     dword ptr [ebx], 18
        mov     dword ptr [esp + 10H], edx
        sub     ebp, 2
        jmp     @@175

@@088:  shr     edx, 2
        mov     dword ptr [ebx], 15
        mov     dword ptr [esp + 10H], edx
        sub     ebp, 2
        jmp     @@175

@@089:  mov     eax, dword ptr [esp + 48H]
        mov     dword ptr [eax + 18H], offset Err248
        mov     dword ptr [ebx], 27
@@090:  shr     edx, 2
        mov     dword ptr [esp + 10H], edx
        sub     ebp, 2
        jmp     @@175

@@091:  mov     ecx, ebp
        and     ecx, 00000007H
        shr     edx, cl
        sub     ebp, ecx
        cmp     ebp, 32
        mov     dword ptr [esp + 10H], edx
        jnc     @@093

@@092:  test    edi, edi
        je      @@183
        movzx   eax, byte ptr [esi]
        mov     ecx, ebp
        shl     eax, cl
        dec     edi
        add     ebp, 8
        mov     dword ptr [esp + 18H], edi
        add     edx, eax
        inc     esi
        cmp     ebp, 32
        mov     dword ptr [esp + 10H], edx
        mov     dword ptr [esp + 14H], esi
        jc      @@092
@@093:  mov     ecx, edx
        mov     eax, edx
        not     ecx
        and     eax, 0000FFFFH
        shr     ecx, 16
        cmp     eax, ecx
        jz      @@094
        mov     eax, dword ptr [esp + 48H]
        mov     dword ptr [eax + 18H], offset Err248
        jmp     @@174

@@094:  xor     ebp, ebp
        mov     dword ptr [ebx + 40H], eax
        mov     dword ptr [esp + 10H], ebp
        mov     dword ptr [ebx], 14
        mov     edx, ebp

@@095:  mov     ecx, dword ptr [ebx + 40H]
        test    ecx, ecx
        mov     dword ptr [esp + 20H], ecx
        je      @@142
        cmp     ecx, edi
        jbe     @@096
        mov     ecx, edi
        mov     dword ptr [esp + 20H], ecx
@@096:  mov     eax, dword ptr [esp + 1CH]
        cmp     ecx, eax
        jbe     @@097
        mov     ecx, eax
        mov     dword ptr [esp + 20H], ecx
@@097:  test    ecx, ecx
        je      @@183
        mov     esi, dword ptr [esp + 14H]
        mov     edi, dword ptr [esp + 24H]
        mov     eax, ecx
        shr     ecx, 2
        rep movsd
        mov     ecx, eax
        mov     eax, dword ptr [esp + 20H]
        and     ecx, 00000003H
        rep movsb
        mov     esi, dword ptr [esp + 18H]
        mov     ecx, dword ptr [esp + 14H]
        mov     edi, dword ptr [esp + 1CH]
        sub     esi, eax
        mov     dword ptr [esp + 18H], esi
        mov     esi, dword ptr [esp + 24H]
        add     ecx, eax
        mov     dword ptr [esp + 14H], ecx
        mov     ecx, dword ptr [ebx + 40H]
        sub     edi, eax
        add     esi, eax
        sub     ecx, eax
        mov     dword ptr [esp + 1CH], edi
        mov     edi, dword ptr [esp + 18H]
        mov     dword ptr [esp + 24H], esi
        mov     esi, dword ptr [esp + 14H]
        mov     dword ptr [ebx + 40H], ecx
        jmp     @@175

@@098:  cmp     ebp, 14
        jnc     @@100
@@099:  test    edi, edi
        je      @@183
        movzx   eax, byte ptr [esi]
        mov     ecx, ebp
        shl     eax, cl
        dec     edi
        add     ebp, 8
        mov     dword ptr [esp + 18H], edi
        add     edx, eax
        inc     esi
        cmp     ebp, 14
        mov     dword ptr [esp + 14H], esi
        jc      @@099
@@100:  mov     ecx, edx
        and     ecx, 0000001FH
        shr     edx, 5
        add     ecx, 257
        mov     eax, edx
        mov     dword ptr [ebx + 60H], ecx
        and     eax, 0000001FH
        shr     edx, 5
        inc     eax
        mov     ecx, edx
        and     ecx, 0000000FH
        mov     dword ptr [ebx + 64H], eax
        mov     eax, dword ptr [ebx + 60H]
        add     ecx, 4
        shr     edx, 4
        sub     ebp, 14
        cmp     eax, 286
        mov     dword ptr [ebx + 5CH], ecx
        mov     dword ptr [esp + 10H], edx
        ja      @@108
        cmp     dword ptr [ebx + 64H], 30
        ja      @@108
        mov     dword ptr [ebx + 68H], 0
        mov     dword ptr [ebx], 16

@@101:  mov     ecx, dword ptr [ebx + 68H]
        cmp     ecx, dword ptr [ebx + 5CH]
        jnc     @@105
@@102:  cmp     ebp, 3
        jnc     @@104

@@103:  test    edi, edi
        je      @@183
        movzx   eax, byte ptr [esi]
        mov     ecx, ebp
        shl     eax, cl
        dec     edi
        add     ebp, 8
        mov     dword ptr [esp + 18H], edi
        add     edx, eax
        inc     esi
        cmp     ebp, 3
        mov     dword ptr [esp + 14H], esi
        jc      @@103
@@104:  mov     eax, dword ptr [ebx + 68H]
        movzx   eax, word ptr [@@251 + eax*2]
        xor     ecx, ecx
        mov     cl, dl
        shr     edx, 3
        sub     ebp, 3
        mov     dword ptr [esp + 10H], edx
        and     ecx, 00000007H
        mov     word ptr [ebx + eax*2 + 70H], cx
        mov     ecx, dword ptr [ebx + 68H]
        inc     ecx
        mov     dword ptr [ebx + 68H], ecx
        cmp     ecx, dword ptr [ebx + 5CH]
        jc      @@102
@@105:  mov     ecx, dword ptr [ebx + 68H]
        mov     eax, 19
        cmp     ecx, eax
        jnc     @@107
        xor     ecx, ecx

@@106:  mov     edx, dword ptr [ebx + 68H]
        movzx   edx, word ptr [@@251 + edx*2]
        mov     word ptr [ebx + edx*2 + 70H], cx
        mov     edx, dword ptr [ebx + 68H]
        inc     edx
        cmp     edx, eax
        mov     dword ptr [ebx + 68H], edx
        jc      @@106
@@107:  lea     eax, [ebx + 00000530H]
        lea     ecx, [ebx + 6CH]
        mov     dword ptr [ecx], eax
        mov     dword ptr [ebx + 4CH], eax
        lea     edx, [ebx + 000002F0H]
        push    edx
        lea     eax, [ebx + 54H]
        push    eax
        push    ecx
        mov     dword ptr [eax], 7
        push    19
        lea     eax, [ebx + 70H]
        push    eax
        push    0
        call    @@196
        test    eax, eax
        mov     edx, dword ptr [esp + 10H]
        mov     dword ptr [esp + 2CH], eax
        jz      @@109
        mov     ecx, dword ptr [esp + 48H]
        mov     dword ptr [ecx + 18H], offset Err248
        jmp     @@174

@@108:  mov     eax, dword ptr [esp + 48H]
        mov     dword ptr [eax + 18H], offset Err248
        jmp     @@174

@@109:  mov     dword ptr [ebx + 68H], 0
        mov     dword ptr [ebx], 17

@@110:  mov     ecx, dword ptr [ebx + 60H]
        mov     eax, dword ptr [ebx + 64H]
        add     eax, ecx
        cmp     dword ptr [ebx + 68H], eax
        jae     @@129
@@111:  mov     ecx, dword ptr [ebx + 54H]
        mov     eax, 1
        shl     eax, cl
        mov     ecx, dword ptr [ebx + 4CH]
        dec     eax
        and     eax, edx
        mov     eax, dword ptr [ecx + eax*4]
        movzx   ecx, ah
        cmp     ecx, ebp
        mov     dword ptr [esp + 3CH], eax
        jbe     @@113
@@112:  test    edi, edi
        je      @@183
        movzx   eax, byte ptr [esi]
        mov     ecx, ebp
        shl     eax, cl
        mov     ecx, dword ptr [ebx + 54H]
        dec     edi
        add     ebp, 8
        add     edx, eax
        mov     eax, 1
        shl     eax, cl
        mov     ecx, dword ptr [ebx + 4CH]
        inc     esi
        mov     dword ptr [esp + 18H], edi
        dec     eax
        and     eax, edx
        mov     eax, dword ptr [ecx + eax*4]
        movzx   ecx, ah
        cmp     ecx, ebp
        mov     dword ptr [esp + 14H], esi
        mov     dword ptr [esp + 3CH], eax
        ja      @@112
@@113:  mov     ecx, dword ptr [esp + 3CH]
        shr     ecx, 16
        cmp     cx, 16
        jnc     @@116
        movzx   ecx, ah
        cmp     ebp, ecx
        mov     dword ptr [esp + 20H], ecx
        jnc     @@115
@@114:  test    edi, edi
        je      @@183
        movzx   eax, byte ptr [esi]
        mov     ecx, ebp
        shl     eax, cl
        mov     ecx, dword ptr [esp + 20H]
        dec     edi
        add     ebp, 8
        add     edx, eax
        inc     esi
        cmp     ebp, ecx
        mov     dword ptr [esp + 18H], edi
        mov     dword ptr [esp + 14H], esi
        jc      @@114
@@115:  mov     ax, word ptr [esp + 3EH]
        shr     edx, cl
        sub     ebp, ecx
        mov     ecx, dword ptr [ebx + 68H]
        mov     word ptr [ebx + ecx*2 + 70H], ax
        mov     eax, dword ptr [ebx + 68H]
        inc     eax
        mov     dword ptr [esp + 10H], edx
        mov     dword ptr [ebx + 68H], eax
        jmp     @@128

@@116:  jnz     @@119
        movzx   ecx, ah
        lea     eax, [ecx + 02H]
        cmp     ebp, eax
        mov     dword ptr [esp + 20H], ecx
        jnc     @@118

@@117:  test    edi, edi
        je      @@183
        movzx   eax, byte ptr [esi]
        mov     ecx, ebp
        shl     eax, cl
        mov     ecx, dword ptr [esp + 20H]
        dec     edi
        add     ebp, 8
        add     edx, eax
        inc     esi
        lea     eax, [ecx + 02H]
        cmp     ebp, eax
        mov     dword ptr [esp + 18H], edi
        mov     dword ptr [esp + 14H], esi
        jc      @@117
@@118:  mov     eax, dword ptr [ebx + 68H]
        shr     edx, cl
        sub     ebp, ecx
        test    eax, eax
        mov     dword ptr [esp + 10H], edx
        je      @@130
        movzx   ecx, word ptr [ebx + eax*2 + 6EH]
        mov     eax, edx
        and     eax, 00000003H
        add     eax, 3
        shr     edx, 2
        mov     dword ptr [esp + 30H], ecx
        mov     dword ptr [esp + 20H], eax
        sub     ebp, 2
        jmp     @@126

@@119:  cmp     cx, 17
        movzx   ecx, ah
        mov     dword ptr [esp + 20H], ecx
        jnz     @@122
        lea     eax, [ecx + 03H]
        cmp     ebp, eax
        jnc     @@121
@@120:  test    edi, edi
        je      @@183
        movzx   eax, byte ptr [esi]
        mov     ecx, ebp
        shl     eax, cl
        mov     ecx, dword ptr [esp + 20H]
        dec     edi
        add     ebp, 8
        add     edx, eax
        inc     esi
        lea     eax, [ecx + 03H]
        cmp     ebp, eax
        mov     dword ptr [esp + 18H], edi
        mov     dword ptr [esp + 14H], esi
        jc      @@120
@@121:  shr     edx, cl
        mov     eax, edx
        and     eax, 00000007H
        add     eax, 3
        mov     dword ptr [esp + 20H], eax
        shr     edx, 3
        mov     eax, -3
        jmp     @@125

@@122:  lea     eax, [ecx + 07H]
        cmp     ebp, eax
        jnc     @@124
@@123:  test    edi, edi
        je      @@183
        movzx   eax, byte ptr [esi]
        mov     ecx, ebp
        shl     eax, cl
        mov     ecx, dword ptr [esp + 20H]
        dec     edi
        add     ebp, 8
        add     edx, eax
        inc     esi
        lea     eax, [ecx + 07H]
        cmp     ebp, eax
        mov     dword ptr [esp + 18H], edi
        mov     dword ptr [esp + 14H], esi
        jc      @@123
@@124:  shr     edx, cl
        mov     eax, edx
        and     eax, 0000007FH
        add     eax, 11
        mov     dword ptr [esp + 20H], eax
        shr     edx, 7
        mov     eax, -7
@@125:  sub     eax, ecx
        mov     dword ptr [esp + 30H], 0
        add     ebp, eax
@@126:  mov     eax, dword ptr [ebx + 60H]
        mov     ecx, dword ptr [ebx + 64H]
        add     ecx, eax
        mov     eax, dword ptr [ebx + 68H]
        add     eax, dword ptr [esp + 20H]
        mov     dword ptr [esp + 10H], edx
        cmp     eax, ecx
        ja      @@131
        mov     eax, dword ptr [esp + 20H]
        test    eax, eax
        jz      @@128
        mov     dword ptr [esp + 20H], eax
        mov     eax, dword ptr [esp + 30H]

@@127:  mov     ecx, dword ptr [ebx + 68H]
        mov     word ptr [ebx + ecx*2 + 70H], ax
        inc     dword ptr [ebx + 68H]
        dec     dword ptr [esp + 20H]
        jnz     @@127
@@128:  mov     ecx, dword ptr [ebx + 60H]
        mov     eax, dword ptr [ebx + 64H]
        add     eax, ecx
        cmp     dword ptr [ebx + 68H], eax
        jb      @@111
@@129:  cmp     dword ptr [ebx], 27
        je      @@175
        lea     eax, [ebx + 00000530H]
        lea     ecx, [ebx + 6CH]
        mov     dword ptr [ecx], eax
        lea     edx, [ebx + 000002F0H]
        push    edx
        mov     dword ptr [ebx + 4CH], eax
        lea     eax, [ebx + 54H]
        push    eax
        push    ecx
        mov     ecx, dword ptr [ebx + 60H]
        push    ecx
        lea     edx, [ebx + 70H]
        push    edx
        push    1
        mov     dword ptr [eax], 9
        call    @@196
        test    eax, eax
        mov     dword ptr [esp + 2CH], eax
        jz      @@132
        mov     eax, dword ptr [esp + 48H]
        mov     edx, dword ptr [esp + 10H]
        mov     dword ptr [eax + 18H], offset Err248
        jmp     @@174

@@130:  mov     ecx, dword ptr [esp + 48H]
        mov     dword ptr [ecx + 18H], offset Err248
        jmp     @@174

@@131:  mov     eax, dword ptr [esp + 48H]
        mov     dword ptr [eax + 18H], offset Err248
        jmp     @@174

@@132:  mov     edx, dword ptr [ebx + 6CH]
        lea     ecx, [ebx + 6CH]
        mov     dword ptr [ebx + 50H], edx
        lea     edx, [ebx + 000002F0H]
        push    edx
        lea     eax, [ebx + 58H]
        push    eax
        push    ecx
        mov     ecx, dword ptr [ebx + 60H]
        mov     dword ptr [eax], 6
        mov     eax, dword ptr [ebx + 64H]
        push    eax
        lea     edx, [ebx + ecx*2 + 70H]
        push    edx
        push    2
        call    @@196
        test    eax, eax
        mov     edx, dword ptr [esp + 10H]
        mov     dword ptr [esp + 2CH], eax
        jz      @@133
        mov     eax, dword ptr [esp + 48H]
        mov     dword ptr [eax + 18H], offset Err248
        jmp     @@174

@@133:  mov     dword ptr [ebx], 18

@@134:  cmp     edi, 6
        jc      @@135
        cmp     dword ptr [esp + 1CH], 258
        jc      @@135
        mov     eax, dword ptr [esp + 48H]
        mov     ecx, dword ptr [esp + 24H]
        mov     dword ptr [eax + 0CH], ecx
        mov     ecx, dword ptr [esp + 1CH]
        mov     dword ptr [eax + 10H], ecx
        mov     dword ptr [eax], esi
        mov     dword ptr [eax + 04H], edi
        mov     dword ptr [ebx + 38H], edx
        mov     edx, dword ptr [esp + 28H]
        push    edx
        push    eax
        mov     dword ptr [ebx + 3CH], ebp
        call    Inline018
        mov     eax, dword ptr [esp + 48H]
        mov     edx, dword ptr [eax + 10H]
        mov     ecx, dword ptr [eax + 0CH]
        mov     esi, dword ptr [eax]
        mov     edi, dword ptr [eax + 04H]
        mov     eax, dword ptr [ebx + 38H]
        mov     ebp, dword ptr [ebx + 3CH]
        mov     dword ptr [esp + 1CH], edx
        mov     dword ptr [esp + 24H], ecx
        mov     dword ptr [esp + 14H], esi
        mov     dword ptr [esp + 18H], edi
        mov     dword ptr [esp + 10H], eax
        mov     edx, eax
        jmp     @@175

@@135:  mov     ecx, dword ptr [ebx + 54H]
        mov     eax, 1
        shl     eax, cl
        dec     eax
        and     eax, edx
        mov     ecx, eax
        mov     eax, dword ptr [ebx + 4CH]
        mov     eax, dword ptr [eax + ecx*4]
        movzx   ecx, ah
        cmp     ecx, ebp
        mov     dword ptr [esp + 3CH], eax
        jbe     @@137

@@136:  test    edi, edi
        je      @@183
        movzx   eax, byte ptr [esi]
        mov     ecx, ebp
        shl     eax, cl
        mov     ecx, dword ptr [ebx + 54H]
        dec     edi
        add     ebp, 8
        add     edx, eax
        mov     eax, 1
        shl     eax, cl
        mov     ecx, dword ptr [ebx + 4CH]
        inc     esi
        mov     dword ptr [esp + 18H], edi
        dec     eax
        and     eax, edx
        mov     eax, dword ptr [ecx + eax*4]
        movzx   ecx, ah
        cmp     ecx, ebp
        mov     dword ptr [esp + 14H], esi
        mov     dword ptr [esp + 3CH], eax
        ja      @@136
@@137:  test    al, al
        je      @@140
        test    al, 0F0H
        jne     @@140
        movzx   ecx, ah
        mov     dword ptr [esp + 20H], ecx
        xor     ecx, ecx
        mov     cl, al
        mov     dword ptr [esp + 10H], eax
        add     ecx, dword ptr [esp + 20H]
        mov     eax, 1
        shl     eax, cl
        mov     ecx, dword ptr [esp + 20H]
        dec     eax
        and     eax, edx
        shr     eax, cl
        mov     ecx, dword ptr [esp + 3CH]
        shr     ecx, 16
        add     eax, ecx
        mov     ecx, eax
        mov     eax, dword ptr [ebx + 4CH]
        mov     eax, dword ptr [eax + ecx*4]
        mov     ecx, dword ptr [esp + 10H]
        shr     ecx, 8
        mov     dword ptr [esp + 3CH], eax
        movzx   ecx, cl
        movzx   eax, ah
        add     eax, ecx
        cmp     eax, ebp
        mov     dword ptr [esp + 20H], ecx
        jbe     @@139

@@138:  test    edi, edi
        je      @@183
        movzx   eax, byte ptr [esi]
        mov     ecx, ebp
        shl     eax, cl
        xor     ecx, ecx
        mov     cl, byte ptr [esp + 10H]
        dec     edi
        add     edx, eax
        mov     eax, dword ptr [esp + 20H]
        inc     esi
        add     ebp, 8
        mov     dword ptr [esp + 18H], edi
        add     ecx, eax
        mov     eax, 1
        shl     eax, cl
        mov     ecx, dword ptr [esp + 20H]
        mov     dword ptr [esp + 14H], esi
        dec     eax
        and     eax, edx
        shr     eax, cl
        movzx   ecx, word ptr [esp + 12H]
        add     eax, ecx
        mov     ecx, dword ptr [ebx + 4CH]
        mov     eax, dword ptr [ecx + eax*4]
        mov     ecx, dword ptr [esp + 20H]
        mov     dword ptr [esp + 3CH], eax
        movzx   eax, ah
        add     eax, ecx
        cmp     eax, ebp
        ja      @@138
@@139:  mov     eax, dword ptr [esp + 3CH]
        shr     edx, cl
        sub     ebp, ecx
@@140:  movzx   ecx, ah
        shr     edx, cl
        movzx   ecx, ah
        sub     ebp, ecx
        mov     ecx, dword ptr [esp + 3CH]
        shr     ecx, 16
        test    al, al
        mov     dword ptr [esp + 10H], edx
        mov     dword ptr [ebx + 40H], ecx
        jnz     @@141
        mov     dword ptr [ebx], 23
        jmp     @@175

@@141:  test    al, 20H
        jz      @@143
@@142:  mov     dword ptr [ebx], 11
        jmp     @@175

@@143:  test    al, 40H
        jz      @@144
        mov     eax, dword ptr [esp + 48H]
        mov     dword ptr [eax + 18H], offset Err248
        jmp     @@174

@@144:  and     eax, 0000000FH
        mov     dword ptr [ebx + 48H], eax
        mov     dword ptr [ebx], 19

@@145:  mov     eax, dword ptr [ebx + 48H]
        test    eax, eax
        jz      @@148
        cmp     ebp, eax
        jnc     @@147
@@146:  test    edi, edi
        je      @@183
        movzx   eax, byte ptr [esi]
        mov     ecx, ebp
        shl     eax, cl
        dec     edi
        add     ebp, 8
        mov     dword ptr [esp + 18H], edi
        add     edx, eax
        mov     eax, dword ptr [ebx + 48H]
        inc     esi
        cmp     ebp, eax
        mov     dword ptr [esp + 14H], esi
        jc      @@146
@@147:  mov     ecx, dword ptr [ebx + 48H]
        mov     eax, 1
        shl     eax, cl
        mov     ecx, dword ptr [ebx + 40H]
        dec     eax
        and     eax, edx
        add     ecx, eax
        mov     dword ptr [ebx + 40H], ecx
        mov     ecx, dword ptr [ebx + 48H]
        shr     edx, cl
        sub     ebp, ecx
@@148:  mov     dword ptr [ebx], 20

@@149:  mov     ecx, dword ptr [ebx + 58H]
        mov     eax, 1
        shl     eax, cl
        dec     eax
        and     eax, edx
        mov     ecx, eax
        mov     eax, dword ptr [ebx + 50H]
        mov     eax, dword ptr [eax + ecx*4]
        movzx   ecx, ah
        cmp     ecx, ebp
        mov     dword ptr [esp + 3CH], eax
        jbe     @@151
@@150:  test    edi, edi
        je      @@183
        movzx   eax, byte ptr [esi]
        mov     ecx, ebp
        shl     eax, cl
        mov     ecx, dword ptr [ebx + 58H]
        dec     edi
        add     ebp, 8
        add     edx, eax
        mov     eax, 1
        shl     eax, cl
        mov     ecx, dword ptr [ebx + 50H]
        inc     esi
        mov     dword ptr [esp + 18H], edi
        dec     eax
        and     eax, edx
        mov     eax, dword ptr [ecx + eax*4]
        movzx   ecx, ah
        cmp     ecx, ebp
        mov     dword ptr [esp + 14H], esi
        mov     dword ptr [esp + 3CH], eax
        ja      @@150
@@151:  test    al, 0F0H
        jne     @@154
        movzx   ecx, ah
        mov     dword ptr [esp + 20H], ecx
        xor     ecx, ecx
        mov     cl, al
        mov     dword ptr [esp + 10H], eax
        add     ecx, dword ptr [esp + 20H]
        mov     eax, 1
        shl     eax, cl
        mov     ecx, dword ptr [esp + 20H]
        dec     eax
        and     eax, edx
        shr     eax, cl
        mov     ecx, dword ptr [esp + 3CH]
        shr     ecx, 16
        add     eax, ecx
        mov     ecx, eax
        mov     eax, dword ptr [ebx + 50H]
        mov     eax, dword ptr [eax + ecx*4]
        mov     ecx, dword ptr [esp + 10H]
        shr     ecx, 8
        mov     dword ptr [esp + 3CH], eax
        movzx   ecx, cl
        movzx   eax, ah
        add     eax, ecx
        cmp     eax, ebp
        mov     dword ptr [esp + 20H], ecx
        jbe     @@153

@@152:  test    edi, edi
        je      @@183
        movzx   eax, byte ptr [esi]
        mov     ecx, ebp
        shl     eax, cl
        xor     ecx, ecx
        mov     cl, byte ptr [esp + 10H]
        dec     edi
        add     edx, eax
        mov     eax, dword ptr [esp + 20H]
        inc     esi
        add     ebp, 8
        mov     dword ptr [esp + 18H], edi
        add     ecx, eax
        mov     eax, 1
        shl     eax, cl
        mov     ecx, dword ptr [esp + 20H]
        mov     dword ptr [esp + 14H], esi
        dec     eax
        and     eax, edx
        shr     eax, cl
        movzx   ecx, word ptr [esp + 12H]
        add     eax, ecx
        mov     ecx, dword ptr [ebx + 50H]
        mov     eax, dword ptr [ecx + eax*4]
        mov     ecx, dword ptr [esp + 20H]
        mov     dword ptr [esp + 3CH], eax
        movzx   eax, ah
        add     eax, ecx
        cmp     eax, ebp
        ja      @@152
@@153:  mov     eax, dword ptr [esp + 3CH]
        shr     edx, cl
        sub     ebp, ecx
@@154:  movzx   ecx, ah
        shr     edx, cl
        movzx   ecx, ah
        sub     ebp, ecx
        test    al, 40H
        mov     dword ptr [esp + 10H], edx
        jz      @@155
        mov     ecx, dword ptr [esp + 48H]
        mov     dword ptr [ecx + 18H], offset Err248
        jmp     @@174

@@155:  mov     ecx, dword ptr [esp + 3CH]
        shr     ecx, 16
        and     eax, 0000000FH
        mov     dword ptr [ebx + 44H], ecx
        mov     dword ptr [ebx + 48H], eax
        mov     dword ptr [ebx], 21

@@156:  mov     eax, dword ptr [ebx + 48H]
        test    eax, eax
        jz      @@159
        cmp     ebp, eax
        jnc     @@158
@@157:  test    edi, edi
        je      @@183
        movzx   eax, byte ptr [esi]
        mov     ecx, ebp
        shl     eax, cl
        dec     edi
        add     ebp, 8
        mov     dword ptr [esp + 18H], edi
        add     edx, eax
        mov     eax, dword ptr [ebx + 48H]
        inc     esi
        cmp     ebp, eax
        mov     dword ptr [esp + 14H], esi
        jc      @@157
@@158:  mov     ecx, dword ptr [ebx + 48H]
        mov     eax, 1
        shl     eax, cl
        mov     ecx, dword ptr [ebx + 44H]
        dec     eax
        and     eax, edx
        add     ecx, eax
        mov     dword ptr [ebx + 44H], ecx
        mov     ecx, dword ptr [ebx + 48H]
        shr     edx, cl
        sub     ebp, ecx
        mov     dword ptr [esp + 10H], edx
@@159:  mov     eax, dword ptr [esp + 1CH]
        mov     ecx, dword ptr [ebx + 2CH]
        sub     ecx, eax
        add     ecx, dword ptr [esp + 28H]
        cmp     dword ptr [ebx + 44H], ecx
        jbe     @@160
        mov     eax, dword ptr [esp + 48H]
        mov     dword ptr [eax + 18H], offset Err248
        jmp     @@174

@@160:  mov     dword ptr [ebx], 22

@@161:  mov     eax, dword ptr [esp + 1CH]
        test    eax, eax
        je      @@183
        mov     ecx, dword ptr [esp + 28H]
        sub     ecx, eax
        mov     eax, dword ptr [ebx + 44H]
        cmp     eax, ecx
        jbe     @@164
        sub     eax, ecx
        mov     ecx, dword ptr [ebx + 30H]
        cmp     eax, ecx
        mov     dword ptr [esp + 20H], eax
        jbe     @@162
        sub     eax, ecx
        mov     ecx, dword ptr [ebx + 34H]
        add     ecx, dword ptr [ebx + 28H]
        mov     dword ptr [esp + 20H], eax
        sub     ecx, eax
        jmp     @@163

@@162:  mov     ecx, dword ptr [ebx + 34H]
        sub     ecx, eax
        add     ecx, dword ptr [ebx + 30H]
        mov     eax, dword ptr [esp + 20H]
@@163:  mov     dword ptr [esp + 30H], ecx
        mov     ecx, dword ptr [ebx + 40H]
        cmp     eax, ecx
        mov     dword ptr [esp + 34H], ecx
        jbe     @@166
        mov     eax, ecx
        jmp     @@165

@@164:  mov     ecx, dword ptr [esp + 24H]
        sub     ecx, eax
        mov     eax, dword ptr [ebx + 40H]
        mov     dword ptr [esp + 30H], ecx
        mov     dword ptr [esp + 34H], eax
@@165:  mov     dword ptr [esp + 20H], eax
@@166:  mov     ecx, dword ptr [esp + 1CH]
        cmp     eax, ecx
        jbe     @@167
        mov     eax, ecx
        mov     dword ptr [esp + 20H], eax
@@167:  sub     ecx, eax
        mov     dword ptr [esp + 1CH], ecx
        mov     ecx, dword ptr [esp + 34H]
        sub     ecx, eax
        mov     eax, dword ptr [esp + 24H]
        mov     dword ptr [ebx + 40H], ecx

@@168:  mov     ecx, dword ptr [esp + 30H]
        mov     cl, byte ptr [ecx]
        mov     byte ptr [eax], cl
        mov     ecx, dword ptr [esp + 30H]
        inc     eax
        inc     ecx
        mov     dword ptr [esp + 30H], ecx
        dec     dword ptr [esp + 20H]
        jnz     @@168
        mov     dword ptr [esp + 24H], eax
        mov     eax, dword ptr [ebx + 40H]
        test    eax, eax
        jne     @@175
        mov     dword ptr [ebx], 18
        jmp     @@175

@@169:  mov     eax, dword ptr [esp + 1CH]
        test    eax, eax
        je      @@183
        mov     eax, dword ptr [esp + 24H]
        mov     cl, byte ptr [ebx + 40H]
        mov     byte ptr [eax], cl
        inc     eax
        mov     dword ptr [esp + 24H], eax
        dec     dword ptr [esp + 1CH]
        mov     dword ptr [ebx], 18
        jmp     @@175

@@170:  mov     eax, dword ptr [ebx + 08H]
        test    eax, eax
        je      @@180
        cmp     ebp, 32
        jnc     @@172

@@171:  test    edi, edi
        je      @@183
        movzx   eax, byte ptr [esi]
        mov     ecx, ebp
        shl     eax, cl
        dec     edi
        add     ebp, 8
        mov     dword ptr [esp + 18H], edi
        add     edx, eax
        inc     esi
        cmp     ebp, 32
        mov     dword ptr [esp + 10H], edx
        mov     dword ptr [esp + 14H], esi
        jc      @@171
@@172:  mov     eax, dword ptr [esp + 28H]
        sub     eax, dword ptr [esp + 1CH]
        mov     ecx, dword ptr [esp + 48H]
        add     dword ptr [ecx + 14H], eax
        mov     ecx, dword ptr [ebx + 1CH]
        add     ecx, eax
        test    eax, eax
        mov     dword ptr [esp + 28H], eax
        mov     dword ptr [ebx + 1CH], ecx
        jz      @@173
        mov     ecx, dword ptr [esp + 24H]
        mov     edx, dword ptr [ebx + 18H]
        push    eax
        sub     ecx, eax
        push    ecx
        push    edx // parameters on stack
        pop eax
        pop edx
        pop ecx // register calling convention
        call    Adler32
        mov     ecx, dword ptr [esp + 48H]
        mov     edx, dword ptr [esp + 10H]
        mov     dword ptr [ebx + 18H], eax
        mov     dword ptr [ecx + 30H], eax
@@173:  mov     eax, dword ptr [esp + 1CH]
        mov     dword ptr [esp + 28H], eax
        mov     ecx, edx
        and     ecx, 0000FF00H
        mov     eax, edx
        shl     eax, 16
        add     ecx, eax
        xor     eax, eax
        mov     ah, byte ptr [esp + 12H]
        shl     ecx, 8
        add     ecx, eax
        mov     eax, edx
        shr     eax, 24
        add     ecx, eax
        cmp     ecx, dword ptr [ebx + 18H]
        jz      @@179
        mov     ecx, dword ptr [esp + 48H]
        mov     dword ptr [ecx + 18H], offset Err248
@@174:  mov     dword ptr [ebx], 27
@@175:  mov     eax, dword ptr [ebx]
        cmp     eax, 28
        jbe     @@069

@@176:  mov     eax, -2
@@177:  pop     edi
        pop     esi
        pop     ebp
        pop     ebx
        add     esp, 52
        ret     8

@@178:  mov     eax, dword ptr [esp + 48H]
        mov     ecx, dword ptr [esp + 24H]
        mov     dword ptr [eax + 0CH], ecx
        mov     ecx, dword ptr [esp + 1CH]
        mov     dword ptr [eax + 04H], edi
        mov     dword ptr [eax], esi
        pop     edi
        mov     dword ptr [eax + 10H], ecx
        pop     esi
        mov     dword ptr [ebx + 3CH], ebp
        pop     ebp
        mov     dword ptr [ebx + 38H], edx
        mov     eax, 2
        pop     ebx
        add     esp, 52
        ret     8

@@179:  xor     ebp, ebp
        mov     dword ptr [esp + 10H], ebp
        mov     edx, ebp
@@180:  mov     dword ptr [ebx], 26

@@181:  mov     dword ptr [esp + 2CH], 1
        jmp     @@183

@@182:  mov     dword ptr [esp + 2CH], -3
@@183:  mov     eax, dword ptr [esp + 48H]
        mov     ecx, dword ptr [esp + 24H]
        mov     dword ptr [eax + 0CH], ecx
        mov     ecx, dword ptr [esp + 1CH]
        mov     dword ptr [eax + 10H], ecx
        mov     dword ptr [eax], esi
        mov     dword ptr [eax + 04H], edi
        mov     eax, dword ptr [ebx + 28H]
        test    eax, eax
        mov     dword ptr [ebx + 38H], edx
        mov     dword ptr [ebx + 3CH], ebp
        jnz     @@184
        cmp     dword ptr [ebx], 24
        jge     @@186
        mov     eax, dword ptr [esp + 48H]
        mov     edx, dword ptr [esp + 28H]
        cmp     edx, dword ptr [eax + 10H]
        jz      @@186
@@184:  mov     ecx, dword ptr [esp + 48H]
        mov     eax, dword ptr [esp + 28H]
        push    ecx
        call    Inline059;
        test    eax, eax
        jz      @@186
        mov     dword ptr [ebx], 28

@@185:  pop     edi
        pop     esi
        pop     ebp
        mov     eax, -4
        pop     ebx
        add     esp, 52
        ret     8

@@186:  mov     esi, dword ptr [esp + 48H]
        mov     edx, dword ptr [esi + 04H]
        mov     ecx, dword ptr [esi + 10H]
        mov     ebp, dword ptr [esp + 38H]
        mov     edi, dword ptr [esp + 28H]
        mov     eax, dword ptr [esi + 08H]
        sub     ebp, edx
        mov     edx, dword ptr [esi + 14H]
        sub     edi, ecx
        add     eax, ebp
        add     edx, edi
        mov     dword ptr [esi + 08H], eax
        mov     dword ptr [esi + 14H], edx
        mov     ecx, dword ptr [ebx + 1CH]
        mov     eax, dword ptr [ebx + 08H]
        add     ecx, edi
        test    eax, eax
        mov     dword ptr [ebx + 1CH], ecx
        jz      @@187
        test    edi, edi
        jz      @@187
        mov     edx, dword ptr [esi + 0CH]
        mov     eax, dword ptr [ebx + 18H]
        mov ecx,edi
        sub     edx, edi
        call    Adler32 // register calling convention
        mov     dword ptr [ebx + 18H], eax
        mov     dword ptr [esi + 30H], eax
@@187:  mov     ecx, dword ptr [ebx + 04H]
        mov     eax, dword ptr [ebx]
        neg     ecx
        sbb     ecx, ecx
        xor     edx, edx
        and     ecx, 00000040H
        cmp     eax, 11
        setne   dl
        dec     edx
        and     edx, 00000080H
        add     ecx, edx
        add     ecx, dword ptr [ebx + 3CH]
        test    ebp, ebp
        mov     dword ptr [esi + 2CH], ecx
        jnz     @@188
        test    edi, edi
        jz      @@189
@@188:  cmp     dword ptr [esp + 4CH], 4
        jnz     @@190
@@189:  mov     eax, dword ptr [esp + 2CH]
        test    eax, eax
        jne     @@177
        pop     edi
        pop     esi
        pop     ebp
        mov     eax, -5
        pop     ebx
        add     esp, 52
        ret     8

@@190:  mov     eax, dword ptr [esp + 2CH]
        pop     edi
        pop     esi
        pop     ebp
        pop     ebx
        add     esp, 52
        ret     8

@@191:  mov     eax, -2
        pop     ebx
        add     esp, 52
        ret     8

@@196:  sub     esp, 124
        mov     edx, dword ptr [esp + 00000088H]
        xor     eax, eax
        test    edx, edx
        mov     dword ptr [esp + 3CH], eax
        mov     dword ptr [esp + 40H], eax
        mov     dword ptr [esp + 44H], eax
        mov     dword ptr [esp + 48H], eax
        mov     dword ptr [esp + 4CH], eax
        push    ebx
        mov     dword ptr [esp + 54H], eax
        push    ebp
        mov     ebp, dword ptr [esp + 0000008CH]
        mov     dword ptr [esp + 5CH], eax
        push    esi
        mov     dword ptr [esp + 64H], eax
        jbe     @@198

@@197:  movzx   ecx, word ptr [ebp + eax*2]
        inc     word ptr [esp + ecx*2 + 48H]
        lea     ecx, [esp + ecx*2 + 48H]
        inc     eax
        cmp     eax, edx
        jc      @@197
@@198:  mov     esi, dword ptr [esp + 0000009CH]
        mov     eax, dword ptr [esi]
        mov     ebx, 15
        mov     dword ptr [esp + 10H], eax
        mov     dword ptr [esp + 18H], ebx

@@199:  cmp     word ptr [esp + ebx*2 + 48H], 0
        jnz     @@200
        dec     ebx
        cmp     ebx, 1
        jnc     @@199
@@200:  cmp     eax, ebx
        mov     dword ptr [esp + 18H], ebx
        jbe     @@201
        mov     dword ptr [esp + 10H], ebx
@@201:  test    ebx, ebx
        jnz     @@202
        mov     eax, dword ptr [esp + 00000098H]
        mov     edx, dword ptr [eax]
        mov     word ptr [esp + 0EH], bx
        mov     byte ptr [esp + 0CH], 64
        mov     byte ptr [esp + 0DH], 1
        mov     ecx, dword ptr [esp + 0CH]
        mov     dword ptr [edx], ecx
        mov     edx, dword ptr [eax]
        add     edx, 4
        mov     dword ptr [eax], edx
        mov     dword ptr [edx], ecx
        add     dword ptr [eax], 4
        mov     dword ptr [esi], 1
        pop     esi
        pop     ebp
        xor     eax, eax
        pop     ebx
        add     esp, 124
        ret     24

@@202:  mov     esi, 1

@@203:  cmp     word ptr [esp + esi*2 + 48H], 0
        jnz     @@208
        cmp     word ptr [esp + esi*2 + 4AH], 0
        jnz     @@204
        cmp     word ptr [esp + esi*2 + 4CH], 0
        jnz     @@205
        cmp     word ptr [esp + esi*2 + 4EH], 0
        jnz     @@206
        cmp     word ptr [esp + esi*2 + 50H], 0
        jnz     @@207
        add     esi, 5
        cmp     esi, 15
        jbe     @@203
        jmp     @@208

@@204:  inc     esi
        jmp     @@208

@@205:  add     esi, 2
        jmp     @@208

@@206:  add     esi, 3
        jmp     @@208

@@207:  add     esi, 4
@@208:  cmp     dword ptr [esp + 10H], esi
        jnc     @@209
        mov     dword ptr [esp + 10H], esi
@@209:  mov     edx, 1
        mov     eax, edx
@@210:  movzx   ecx, word ptr [esp + eax*2 + 48H]
        add     edx, edx
        sub     edx, ecx
        js      @@212
        inc     eax
        cmp     eax, 15
        jbe     @@210
        test    edx, edx
        push    edi
        mov     edi, dword ptr [esp + 00000090H]
        jle     @@213
        test    edi, edi
        jz      @@211
        cmp     ebx, 1
        jz      @@213
@@211:  pop     edi
        pop     esi
        pop     ebp
        or      eax, 0FFFFFFFFH
        pop     ebx
        add     esp, 124
        ret     24

@@212:  pop     esi
        pop     ebp
        or      eax, 0FFFFFFFFH
        pop     ebx
        add     esp, 124
        ret     24

@@213:
        mov     word ptr [esp + 6EH], 0
        mov     eax, 2

@@214:  mov     dx, word ptr [esp + eax + 6CH]
        add     dx, word ptr [esp + eax + 4CH]
        mov     cx, word ptr [esp + eax + 4EH]
        add     cx, dx
        mov     word ptr [esp + eax + 6EH], dx
        mov     word ptr [esp + eax + 70H], cx
        add     eax, 4
        cmp     eax, 30
        jc      @@214
        mov     ebx, dword ptr [esp + 00000098H]
        xor     eax, eax
        test    ebx, ebx
        jbe     @@217
@@215:  cmp     word ptr [ebp + eax*2], 0
        jz      @@216
        movzx   edx, word ptr [ebp + eax*2]
        movzx   ecx, word ptr [esp + edx*2 + 6CH]
        mov     edx, dword ptr [esp + 000000A4H]
        mov     word ptr [edx + ecx*2], ax
        movzx   edx, word ptr [ebp + eax*2]
        inc     word ptr [esp + edx*2 + 6CH]
        lea     edx, [esp + edx*2 + 6CH]
@@216:  inc     eax
        cmp     eax, ebx
        jc      @@215
@@217:  mov     eax, edi
        sub     eax, 0
        mov     edx, -1
        jz      @@219
        dec     eax
        jz      @@218
        mov     dword ptr [esp + 34H], offset @@265
        mov     dword ptr [esp + 30H], offset @@266
        mov     dword ptr [esp + 2CH], edx
        jmp     @@221

@@218:  mov     eax, offset @@263
        sub     eax, 514
        mov     dword ptr [esp + 34H], eax
        mov     eax, offset @@264
        sub     eax, 514
        mov     dword ptr [esp + 2CH], 256
        jmp     @@220

@@219:  mov     eax, dword ptr [esp + 000000A4H]
        mov     dword ptr [esp + 34H], eax
        mov     dword ptr [esp + 2CH], 19
@@220:  mov     dword ptr [esp + 30H], eax
@@221:  mov     eax, dword ptr [esp + 0000009CH]
        mov     ecx, dword ptr [eax]
        mov     dword ptr [esp + 20H], ecx
        mov     ecx, dword ptr [esp + 14H]
        mov     eax, 1
        shl     eax, cl
        mov     dword ptr [esp + 38H], edx
        xor     ebp, ebp
        xor     ebx, ebx
        cmp     edi, 1
        lea     edx, [eax - 01H]
        mov     dword ptr [esp + 18H], esi
        mov     dword ptr [esp + 3CH], eax
        mov     dword ptr [esp + 28H], eax
        mov     dword ptr [esp + 40H], edx
        jnz     @@222
        cmp     eax, 1456
        jae     @@244
@@222:  mov     eax, dword ptr [esp + 000000A4H]
        mov     dword ptr [esp + 24H], eax

@@223:  mov     cl, byte ptr [esp + 18H]
        mov     esi, dword ptr [esp + 24H]
        mov     ax, word ptr [esi]
        mov     edx, dword ptr [esp + 2CH]
        sub     cl, bl
        mov     byte ptr [esp + 11H], cl
        movzx   ecx, ax
        cmp     ecx, edx
        jge     @@224
        mov     byte ptr [esp + 10H], 0
        mov     word ptr [esp + 12H], ax
        jmp     @@226

@@224:  jle     @@225
        movzx   eax, word ptr [esi]
        mov     edx, dword ptr [esp + 30H]
        shl     eax, 1
        mov     cl, byte ptr [eax + edx]
        mov     edx, dword ptr [esp + 34H]
        mov     ax, word ptr [eax + edx]
        mov     byte ptr [esp + 10H], cl
        mov     word ptr [esp + 12H], ax
        jmp     @@226

@@225:  mov     byte ptr [esp + 10H], 96
        mov     word ptr [esp + 12H], 0
@@226:  mov     ecx, dword ptr [esp + 18H]
        mov     eax, dword ptr [esp + 3CH]
        sub     ecx, ebx
        mov     edx, 1
        shl     edx, cl
        mov     ecx, ebx
        mov     edi, ebp
        shr     edi, cl
        mov     ecx, dword ptr [esp + 20H]
        mov     dword ptr [esp + 44H], eax
        lea     esi, [edx*4]
        add     edi, eax
        lea     ecx, [ecx + edi*4]
        mov     edi, dword ptr [esp + 10H]
@@227:  sub     eax, edx
        sub     ecx, esi
        test    eax, eax
        mov     dword ptr [ecx], edi
        jnz     @@227
        mov     edx, dword ptr [esp + 18H]
        lea     ecx, [edx - 01H]
        mov     eax, 1
        shl     eax, cl
        test    ebp, eax
        jz      @@229
@@228:  shr     eax, 1
        test    ebp, eax
        jnz     @@228
@@229:  test    eax, eax
        jz      @@230
        lea     ecx, [eax - 01H]
        and     ecx, ebp
        add     ecx, eax
        mov     ebp, ecx
        jmp     @@231

@@230:  xor     ebp, ebp
@@231:  mov     esi, dword ptr [esp + 24H]
        add     esi, 2
        dec     word ptr [esp + edx*2 + 4CH]
        cmp     word ptr [esp + edx*2 + 4CH], 0
        mov     dword ptr [esp + 24H], esi
        jnz     @@232
        cmp     edx, dword ptr [esp + 1CH]
        je      @@238
        mov     ecx, dword ptr [esp + 00000094H]
        mov     edx, esi
        movzx   eax, word ptr [edx]
        movzx   edx, word ptr [ecx + eax*2]
        mov     dword ptr [esp + 18H], edx
@@232:  cmp     edx, dword ptr [esp + 14H]
        jbe     @@223
        mov     esi, dword ptr [esp + 40H]
        mov     eax, dword ptr [esp + 38H]
        and     esi, ebp
        cmp     esi, eax
        mov     dword ptr [esp + 48H], esi
        je      @@223
        test    ebx, ebx
        jnz     @@233
        mov     ebx, dword ptr [esp + 14H]
@@233:  mov     eax, dword ptr [esp + 20H]
        mov     ecx, dword ptr [esp + 44H]
        mov     edi, dword ptr [esp + 1CH]
        lea     edx, [eax + ecx*4]
        mov     ecx, dword ptr [esp + 18H]
        sub     ecx, ebx
        mov     dword ptr [esp + 20H], edx
        mov     eax, 1
        lea     edx, [ebx + ecx]
        shl     eax, cl
        cmp     edx, edi
        jnc     @@236
        lea     esi, [esp + edx*2 + 4CH]
@@234:  movzx   edi, word ptr [esi]
        sub     eax, edi
        test    eax, eax
        jle     @@235
        mov     edi, dword ptr [esp + 1CH]
        inc     ecx
        inc     edx
        add     esi, 2
        shl     eax, 1
        cmp     edx, edi
        jc      @@234
@@235:  mov     esi, dword ptr [esp + 48H]
@@236:  mov     edx, dword ptr [esp + 28H]
        mov     eax, 1
        shl     eax, cl
        add     edx, eax
        mov     dword ptr [esp + 3CH], eax
        cmp     dword ptr [esp + 00000090H], 1
        mov     dword ptr [esp + 28H], edx
        jnz     @@237
        mov     eax, edx
        cmp     eax, 1456
        jae     @@244
@@237:  mov     edx, esi
        mov     esi, dword ptr [esp + 0000009CH]
        mov     eax, dword ptr [esi]
        mov     byte ptr [eax + edx*4], cl
        mov     ecx, dword ptr [esi]
        mov     al, byte ptr [esp + 14H]
        mov     byte ptr [ecx + edx*4 + 01H], al
        mov     eax, dword ptr [esi]
        mov     ecx, dword ptr [esp + 20H]
        sub     ecx, eax
        sar     ecx, 2
        mov     dword ptr [esp + 38H], edx
        mov     word ptr [eax + edx*4 + 02H], cx
        jmp     @@223

@@238:  mov     edi, dword ptr [esp + 0000009CH]
        mov     al, dl
        sub     al, bl
        test    ebp, ebp
        mov     byte ptr [esp + 10H], 64
        mov     byte ptr [esp + 11H], al
        mov     word ptr [esp + 12H], 0
        jz      @@243
        mov     esi, dword ptr [esp + 20H]

@@239:  test    ebx, ebx
        jz      @@240
        mov     ecx, dword ptr [esp + 40H]
        mov     eax, dword ptr [esp + 38H]
        and     ecx, ebp
        cmp     ecx, eax
        jz      @@240
        mov     eax, dword ptr [esp + 14H]
        mov     esi, dword ptr [edi]
        xor     ebx, ebx
        mov     dword ptr [esp + 18H], eax
        mov     byte ptr [esp + 11H], al
        mov     edx, eax
@@240:  mov     ecx, ebx
        mov     eax, ebp
        shr     eax, cl
        mov     ecx, dword ptr [esp + 10H]
        mov     dword ptr [esi + eax*4], ecx
        lea     ecx, [edx - 01H]
        mov     eax, 1
        shl     eax, cl
        test    ebp, eax
        jz      @@242

@@241:  shr     eax, 1
        test    ebp, eax
        jnz     @@241
@@242:  test    eax, eax
        jz      @@243
        lea     ecx, [eax - 01H]
        and     ecx, ebp
        add     ecx, eax
        mov     ebp, ecx
        jnz     @@239
@@243:  mov     edx, dword ptr [esp + 28H]
        mov     ecx, dword ptr [edi]
        lea     eax, [edx*4]
        mov     edx, dword ptr [esp + 14H]
        add     ecx, eax
        mov     dword ptr [edi], ecx
        mov     ecx, dword ptr [esp + 000000A0H]
        pop     edi
        pop     esi
        pop     ebp
        mov     dword ptr [ecx], edx
        xor     eax, eax
        pop     ebx
        add     esp, 124
        ret     24

@@244:  pop     edi
        pop     esi
        pop     ebp
        mov     eax, 1
        pop     ebx
        add     esp, 124
        ret     24

        nop; nop

@@192:  dd offset @@070 // buffer is 8 bytes aligned
        dd offset @@176
        dd offset @@176
        dd offset @@176
        dd offset @@176
        dd offset @@176
        dd offset @@176
        dd offset @@176
        dd offset @@176
        dd offset @@077
        dd offset @@080
        dd offset @@081
        dd offset @@082
        dd offset @@091
        dd offset @@095
        dd offset @@098
        dd offset @@101
        dd offset @@110
        dd offset @@134
        dd offset @@145
        dd offset @@149
        dd offset @@156
        dd offset @@161
        dd offset @@169
        dd offset @@170
        dd offset @@176
        dd offset @@181
        dd offset @@182
        dd offset @@185

@@193:  dd offset @@086
        dd offset @@087
        dd offset @@088
        dd offset @@089

@@251:  dw 0010H, 0011H, 0012H, 0000H
        dw 0008H, 0007H, 0009H, 0006H
        dw 000AH, 0005H, 000BH, 0004H
        dw 000CH, 0003H, 000DH, 0002H
        dw 000EH, 0001H, 000FH, 0000H

@@249:  db 60H, 07H, 00H, 00H, 00H, 08H, 50H, 00H
        db 00H, 08H, 10H, 00H, 14H, 08H, 73H, 00H
        db 12H, 07H, 1FH, 00H, 00H, 08H, 70H, 00H
        db 00H, 08H, 30H, 00H, 00H, 09H, 0C0H, 00H
        db 10H, 07H, 0AH, 00H, 00H, 08H, 60H, 00H
        db 00H, 08H, 20H, 00H, 00H, 09H, 0A0H, 00H
        db 00H, 08H, 00H, 00H, 00H, 08H, 80H, 00H
        db 00H, 08H, 40H, 00H, 00H, 09H, 0E0H, 00H
        db 10H, 07H, 06H, 00H, 00H, 08H, 58H, 00H
        db 00H, 08H, 18H, 00H, 00H, 09H, 90H, 00H
        db 13H, 07H, 3BH, 00H, 00H, 08H, 78H, 00H
        db 00H, 08H, 38H, 00H, 00H, 09H, 0D0H, 00H
        db 11H, 07H, 11H, 00H, 00H, 08H, 68H, 00H
        db 00H, 08H, 28H, 00H, 00H, 09H, 0B0H, 00H
        db 00H, 08H, 08H, 00H, 00H, 08H, 88H, 00H
        db 00H, 08H, 48H, 00H, 00H, 09H, 0F0H, 00H
        db 10H, 07H, 04H, 00H, 00H, 08H, 54H, 00H
        db 00H, 08H, 14H, 00H, 15H, 08H, 0E3H, 00H
        db 13H, 07H, 2BH, 00H, 00H, 08H, 74H, 00H
        db 00H, 08H, 34H, 00H, 00H, 09H, 0C8H, 00H
        db 11H, 07H, 0DH, 00H, 00H, 08H, 64H, 00H
        db 00H, 08H, 24H, 00H, 00H, 09H, 0A8H, 00H
        db 00H, 08H, 04H, 00H, 00H, 08H, 84H, 00H
        db 00H, 08H, 44H, 00H, 00H, 09H, 0E8H, 00H
        db 10H, 07H, 08H, 00H, 00H, 08H, 5CH, 00H
        db 00H, 08H, 1CH, 00H, 00H, 09H, 98H, 00H
        db 14H, 07H, 53H, 00H, 00H, 08H, 7CH, 00H
        db 00H, 08H, 3CH, 00H, 00H, 09H, 0D8H, 00H
        db 12H, 07H, 17H, 00H, 00H, 08H, 6CH, 00H
        db 00H, 08H, 2CH, 00H, 00H, 09H, 0B8H, 00H
        db 00H, 08H, 0CH, 00H, 00H, 08H, 8CH, 00H
        db 00H, 08H, 4CH, 00H, 00H, 09H, 0F8H, 00H
        db 10H, 07H, 03H, 00H, 00H, 08H, 52H, 00H
        db 00H, 08H, 12H, 00H, 15H, 08H, 0A3H, 00H
        db 13H, 07H, 23H, 00H, 00H, 08H, 72H, 00H
        db 00H, 08H, 32H, 00H, 00H, 09H, 0C4H, 00H
        db 11H, 07H, 0BH, 00H, 00H, 08H, 62H, 00H
        db 00H, 08H, 22H, 00H, 00H, 09H, 0A4H, 00H
        db 00H, 08H, 02H, 00H, 00H, 08H, 82H, 00H
        db 00H, 08H, 42H, 00H, 00H, 09H, 0E4H, 00H
        db 10H, 07H, 07H, 00H, 00H, 08H, 5AH, 00H
        db 00H, 08H, 1AH, 00H, 00H, 09H, 94H, 00H
        db 14H, 07H, 43H, 00H, 00H, 08H, 7AH, 00H
        db 00H, 08H, 3AH, 00H, 00H, 09H, 0D4H, 00H
        db 12H, 07H, 13H, 00H, 00H, 08H, 6AH, 00H
        db 00H, 08H, 2AH, 00H, 00H, 09H, 0B4H, 00H
        db 00H, 08H, 0AH, 00H, 00H, 08H, 8AH, 00H
        db 00H, 08H, 4AH, 00H, 00H, 09H, 0F4H, 00H
        db 10H, 07H, 05H, 00H, 00H, 08H, 56H, 00H
        db 00H, 08H, 16H, 00H, 40H, 08H, 00H, 00H
        db 13H, 07H, 33H, 00H, 00H, 08H, 76H, 00H
        db 00H, 08H, 36H, 00H, 00H, 09H, 0CCH, 00H
        db 11H, 07H, 0FH, 00H, 00H, 08H, 66H, 00H
        db 00H, 08H, 26H, 00H, 00H, 09H, 0ACH, 00H
        db 00H, 08H, 06H, 00H, 00H, 08H, 86H, 00H
        db 00H, 08H, 46H, 00H, 00H, 09H, 0ECH, 00H
        db 10H, 07H, 09H, 00H, 00H, 08H, 5EH, 00H
        db 00H, 08H, 1EH, 00H, 00H, 09H, 9CH, 00H
        db 14H, 07H, 63H, 00H, 00H, 08H, 7EH, 00H
        db 00H, 08H, 3EH, 00H, 00H, 09H, 0DCH, 00H
        db 12H, 07H, 1BH, 00H, 00H, 08H, 6EH, 00H
        db 00H, 08H, 2EH, 00H, 00H, 09H, 0BCH, 00H
        db 00H, 08H, 0EH, 00H, 00H, 08H, 8EH, 00H
        db 00H, 08H, 4EH, 00H, 00H, 09H, 0FCH, 00H
        db 60H, 07H, 00H, 00H, 00H, 08H, 51H, 00H
        db 00H, 08H, 11H, 00H, 15H, 08H, 83H, 00H
        db 12H, 07H, 1FH, 00H, 00H, 08H, 71H, 00H
        db 00H, 08H, 31H, 00H, 00H, 09H, 0C2H, 00H
        db 10H, 07H, 0AH, 00H, 00H, 08H, 61H, 00H
        db 00H, 08H, 21H, 00H, 00H, 09H, 0A2H, 00H
        db 00H, 08H, 01H, 00H, 00H, 08H, 81H, 00H
        db 00H, 08H, 41H, 00H, 00H, 09H, 0E2H, 00H
        db 10H, 07H, 06H, 00H, 00H, 08H, 59H, 00H
        db 00H, 08H, 19H, 00H, 00H, 09H, 92H, 00H
        db 13H, 07H, 3BH, 00H, 00H, 08H, 79H, 00H
        db 00H, 08H, 39H, 00H, 00H, 09H, 0D2H, 00H
        db 11H, 07H, 11H, 00H, 00H, 08H, 69H, 00H
        db 00H, 08H, 29H, 00H, 00H, 09H, 0B2H, 00H
        db 00H, 08H, 09H, 00H, 00H, 08H, 89H, 00H
        db 00H, 08H, 49H, 00H, 00H, 09H, 0F2H, 00H
        db 10H, 07H, 04H, 00H, 00H, 08H, 55H, 00H
        db 00H, 08H, 15H, 00H, 10H, 08H, 02H, 01H
        db 13H, 07H, 2BH, 00H, 00H, 08H, 75H, 00H
        db 00H, 08H, 35H, 00H, 00H, 09H, 0CAH, 00H
        db 11H, 07H, 0DH, 00H, 00H, 08H, 65H, 00H
        db 00H, 08H, 25H, 00H, 00H, 09H, 0AAH, 00H
        db 00H, 08H, 05H, 00H, 00H, 08H, 85H, 00H
        db 00H, 08H, 45H, 00H, 00H, 09H, 0EAH, 00H
        db 10H, 07H, 08H, 00H, 00H, 08H, 5DH, 00H
        db 00H, 08H, 1DH, 00H, 00H, 09H, 9AH, 00H
        db 14H, 07H, 53H, 00H, 00H, 08H, 7DH, 00H
        db 00H, 08H, 3DH, 00H, 00H, 09H, 0DAH, 00H
        db 12H, 07H, 17H, 00H, 00H, 08H, 6DH, 00H
        db 00H, 08H, 2DH, 00H, 00H, 09H, 0BAH, 00H
        db 00H, 08H, 0DH, 00H, 00H, 08H, 8DH, 00H
        db 00H, 08H, 4DH, 00H, 00H, 09H, 0FAH, 00H
        db 10H, 07H, 03H, 00H, 00H, 08H, 53H, 00H
        db 00H, 08H, 13H, 00H, 15H, 08H, 0C3H, 00H
        db 13H, 07H, 23H, 00H, 00H, 08H, 73H, 00H
        db 00H, 08H, 33H, 00H, 00H, 09H, 0C6H, 00H
        db 11H, 07H, 0BH, 00H, 00H, 08H, 63H, 00H
        db 00H, 08H, 23H, 00H, 00H, 09H, 0A6H, 00H
        db 00H, 08H, 03H, 00H, 00H, 08H, 83H, 00H
        db 00H, 08H, 43H, 00H, 00H, 09H, 0E6H, 00H
        db 10H, 07H, 07H, 00H, 00H, 08H, 5BH, 00H
        db 00H, 08H, 1BH, 00H, 00H, 09H, 96H, 00H
        db 14H, 07H, 43H, 00H, 00H, 08H, 7BH, 00H
        db 00H, 08H, 3BH, 00H, 00H, 09H, 0D6H, 00H
        db 12H, 07H, 13H, 00H, 00H, 08H, 6BH, 00H
        db 00H, 08H, 2BH, 00H, 00H, 09H, 0B6H, 00H
        db 00H, 08H, 0BH, 00H, 00H, 08H, 8BH, 00H
        db 00H, 08H, 4BH, 00H, 00H, 09H, 0F6H, 00H
        db 10H, 07H, 05H, 00H, 00H, 08H, 57H, 00H
        db 00H, 08H, 17H, 00H, 40H, 08H, 00H, 00H
        db 13H, 07H, 33H, 00H, 00H, 08H, 77H, 00H
        db 00H, 08H, 37H, 00H, 00H, 09H, 0CEH, 00H
        db 11H, 07H, 0FH, 00H, 00H, 08H, 67H, 00H
        db 00H, 08H, 27H, 00H, 00H, 09H, 0AEH, 00H
        db 00H, 08H, 07H, 00H, 00H, 08H, 87H, 00H
        db 00H, 08H, 47H, 00H, 00H, 09H, 0EEH, 00H
        db 10H, 07H, 09H, 00H, 00H, 08H, 5FH, 00H
        db 00H, 08H, 1FH, 00H, 00H, 09H, 9EH, 00H
        db 14H, 07H, 63H, 00H, 00H, 08H, 7FH, 00H
        db 00H, 08H, 3FH, 00H, 00H, 09H, 0DEH, 00H
        db 12H, 07H, 1BH, 00H, 00H, 08H, 6FH, 00H
        db 00H, 08H, 2FH, 00H, 00H, 09H, 0BEH, 00H
        db 00H, 08H, 0FH, 00H, 00H, 08H, 8FH, 00H
        db 00H, 08H, 4FH, 00H, 00H, 09H, 0FEH, 00H
        db 60H, 07H, 00H, 00H, 00H, 08H, 50H, 00H
        db 00H, 08H, 10H, 00H, 14H, 08H, 73H, 00H
        db 12H, 07H, 1FH, 00H, 00H, 08H, 70H, 00H
        db 00H, 08H, 30H, 00H, 00H, 09H, 0C1H, 00H
        db 10H, 07H, 0AH, 00H, 00H, 08H, 60H, 00H
        db 00H, 08H, 20H, 00H, 00H, 09H, 0A1H, 00H
        db 00H, 08H, 00H, 00H, 00H, 08H, 80H, 00H
        db 00H, 08H, 40H, 00H, 00H, 09H, 0E1H, 00H
        db 10H, 07H, 06H, 00H, 00H, 08H, 58H, 00H
        db 00H, 08H, 18H, 00H, 00H, 09H, 91H, 00H
        db 13H, 07H, 3BH, 00H, 00H, 08H, 78H, 00H
        db 00H, 08H, 38H, 00H, 00H, 09H, 0D1H, 00H
        db 11H, 07H, 11H, 00H, 00H, 08H, 68H, 00H
        db 00H, 08H, 28H, 00H, 00H, 09H, 0B1H, 00H
        db 00H, 08H, 08H, 00H, 00H, 08H, 88H, 00H
        db 00H, 08H, 48H, 00H, 00H, 09H, 0F1H, 00H
        db 10H, 07H, 04H, 00H, 00H, 08H, 54H, 00H
        db 00H, 08H, 14H, 00H, 15H, 08H, 0E3H, 00H
        db 13H, 07H, 2BH, 00H, 00H, 08H, 74H, 00H
        db 00H, 08H, 34H, 00H, 00H, 09H, 0C9H, 00H
        db 11H, 07H, 0DH, 00H, 00H, 08H, 64H, 00H
        db 00H, 08H, 24H, 00H, 00H, 09H, 0A9H, 00H
        db 00H, 08H, 04H, 00H, 00H, 08H, 84H, 00H
        db 00H, 08H, 44H, 00H, 00H, 09H, 0E9H, 00H
        db 10H, 07H, 08H, 00H, 00H, 08H, 5CH, 00H
        db 00H, 08H, 1CH, 00H, 00H, 09H, 99H, 00H
        db 14H, 07H, 53H, 00H, 00H, 08H, 7CH, 00H
        db 00H, 08H, 3CH, 00H, 00H, 09H, 0D9H, 00H
        db 12H, 07H, 17H, 00H, 00H, 08H, 6CH, 00H
        db 00H, 08H, 2CH, 00H, 00H, 09H, 0B9H, 00H
        db 00H, 08H, 0CH, 00H, 00H, 08H, 8CH, 00H
        db 00H, 08H, 4CH, 00H, 00H, 09H, 0F9H, 00H
        db 10H, 07H, 03H, 00H, 00H, 08H, 52H, 00H
        db 00H, 08H, 12H, 00H, 15H, 08H, 0A3H, 00H
        db 13H, 07H, 23H, 00H, 00H, 08H, 72H, 00H
        db 00H, 08H, 32H, 00H, 00H, 09H, 0C5H, 00H
        db 11H, 07H, 0BH, 00H, 00H, 08H, 62H, 00H
        db 00H, 08H, 22H, 00H, 00H, 09H, 0A5H, 00H
        db 00H, 08H, 02H, 00H, 00H, 08H, 82H, 00H
        db 00H, 08H, 42H, 00H, 00H, 09H, 0E5H, 00H
        db 10H, 07H, 07H, 00H, 00H, 08H, 5AH, 00H
        db 00H, 08H, 1AH, 00H, 00H, 09H, 95H, 00H
        db 14H, 07H, 43H, 00H, 00H, 08H, 7AH, 00H
        db 00H, 08H, 3AH, 00H, 00H, 09H, 0D5H, 00H
        db 12H, 07H, 13H, 00H, 00H, 08H, 6AH, 00H
        db 00H, 08H, 2AH, 00H, 00H, 09H, 0B5H, 00H
        db 00H, 08H, 0AH, 00H, 00H, 08H, 8AH, 00H
        db 00H, 08H, 4AH, 00H, 00H, 09H, 0F5H, 00H
        db 10H, 07H, 05H, 00H, 00H, 08H, 56H, 00H
        db 00H, 08H, 16H, 00H, 40H, 08H, 00H, 00H
        db 13H, 07H, 33H, 00H, 00H, 08H, 76H, 00H
        db 00H, 08H, 36H, 00H, 00H, 09H, 0CDH, 00H
        db 11H, 07H, 0FH, 00H, 00H, 08H, 66H, 00H
        db 00H, 08H, 26H, 00H, 00H, 09H, 0ADH, 00H
        db 00H, 08H, 06H, 00H, 00H, 08H, 86H, 00H
        db 00H, 08H, 46H, 00H, 00H, 09H, 0EDH, 00H
        db 10H, 07H, 09H, 00H, 00H, 08H, 5EH, 00H
        db 00H, 08H, 1EH, 00H, 00H, 09H, 9DH, 00H
        db 14H, 07H, 63H, 00H, 00H, 08H, 7EH, 00H
        db 00H, 08H, 3EH, 00H, 00H, 09H, 0DDH, 00H
        db 12H, 07H, 1BH, 00H, 00H, 08H, 6EH, 00H
        db 00H, 08H, 2EH, 00H, 00H, 09H, 0BDH, 00H
        db 00H, 08H, 0EH, 00H, 00H, 08H, 8EH, 00H
        db 00H, 08H, 4EH, 00H, 00H, 09H, 0FDH, 00H
        db 60H, 07H, 00H, 00H, 00H, 08H, 51H, 00H
        db 00H, 08H, 11H, 00H, 15H, 08H, 83H, 00H
        db 12H, 07H, 1FH, 00H, 00H, 08H, 71H, 00H
        db 00H, 08H, 31H, 00H, 00H, 09H, 0C3H, 00H
        db 10H, 07H, 0AH, 00H, 00H, 08H, 61H, 00H
        db 00H, 08H, 21H, 00H, 00H, 09H, 0A3H, 00H
        db 00H, 08H, 01H, 00H, 00H, 08H, 81H, 00H
        db 00H, 08H, 41H, 00H, 00H, 09H, 0E3H, 00H
        db 10H, 07H, 06H, 00H, 00H, 08H, 59H, 00H
        db 00H, 08H, 19H, 00H, 00H, 09H, 93H, 00H
        db 13H, 07H, 3BH, 00H, 00H, 08H, 79H, 00H
        db 00H, 08H, 39H, 00H, 00H, 09H, 0D3H, 00H
        db 11H, 07H, 11H, 00H, 00H, 08H, 69H, 00H
        db 00H, 08H, 29H, 00H, 00H, 09H, 0B3H, 00H
        db 00H, 08H, 09H, 00H, 00H, 08H, 89H, 00H
        db 00H, 08H, 49H, 00H, 00H, 09H, 0F3H, 00H
        db 10H, 07H, 04H, 00H, 00H, 08H, 55H, 00H
        db 00H, 08H, 15H, 00H, 10H, 08H, 02H, 01H
        db 13H, 07H, 2BH, 00H, 00H, 08H, 75H, 00H
        db 00H, 08H, 35H, 00H, 00H, 09H, 0CBH, 00H
        db 11H, 07H, 0DH, 00H, 00H, 08H, 65H, 00H
        db 00H, 08H, 25H, 00H, 00H, 09H, 0ABH, 00H
        db 00H, 08H, 05H, 00H, 00H, 08H, 85H, 00H
        db 00H, 08H, 45H, 00H, 00H, 09H, 0EBH, 00H
        db 10H, 07H, 08H, 00H, 00H, 08H, 5DH, 00H
        db 00H, 08H, 1DH, 00H, 00H, 09H, 9BH, 00H
        db 14H, 07H, 53H, 00H, 00H, 08H, 7DH, 00H
        db 00H, 08H, 3DH, 00H, 00H, 09H, 0DBH, 00H
        db 12H, 07H, 17H, 00H, 00H, 08H, 6DH, 00H
        db 00H, 08H, 2DH, 00H, 00H, 09H, 0BBH, 00H
        db 00H, 08H, 0DH, 00H, 00H, 08H, 8DH, 00H
        db 00H, 08H, 4DH, 00H, 00H, 09H, 0FBH, 00H
        db 10H, 07H, 03H, 00H, 00H, 08H, 53H, 00H
        db 00H, 08H, 13H, 00H, 15H, 08H, 0C3H, 00H
        db 13H, 07H, 23H, 00H, 00H, 08H, 73H, 00H
        db 00H, 08H, 33H, 00H, 00H, 09H, 0C7H, 00H
        db 11H, 07H, 0BH, 00H, 00H, 08H, 63H, 00H
        db 00H, 08H, 23H, 00H, 00H, 09H, 0A7H, 00H
        db 00H, 08H, 03H, 00H, 00H, 08H, 83H, 00H
        db 00H, 08H, 43H, 00H, 00H, 09H, 0E7H, 00H
        db 10H, 07H, 07H, 00H, 00H, 08H, 5BH, 00H
        db 00H, 08H, 1BH, 00H, 00H, 09H, 97H, 00H
        db 14H, 07H, 43H, 00H, 00H, 08H, 7BH, 00H
        db 00H, 08H, 3BH, 00H, 00H, 09H, 0D7H, 00H
        db 12H, 07H, 13H, 00H, 00H, 08H, 6BH, 00H
        db 00H, 08H, 2BH, 00H, 00H, 09H, 0B7H, 00H
        db 00H, 08H, 0BH, 00H, 00H, 08H, 8BH, 00H
        db 00H, 08H, 4BH, 00H, 00H, 09H, 0F7H, 00H
        db 10H, 07H, 05H, 00H, 00H, 08H, 57H, 00H
        db 00H, 08H, 17H, 00H, 40H, 08H, 00H, 00H
        db 13H, 07H, 33H, 00H, 00H, 08H, 77H, 00H
        db 00H, 08H, 37H, 00H, 00H, 09H, 0CFH, 00H
        db 11H, 07H, 0FH, 00H, 00H, 08H, 67H, 00H
        db 00H, 08H, 27H, 00H, 00H, 09H, 0AFH, 00H
        db 00H, 08H, 07H, 00H, 00H, 08H, 87H, 00H
        db 00H, 08H, 47H, 00H, 00H, 09H, 0EFH, 00H
        db 10H, 07H, 09H, 00H, 00H, 08H, 5FH, 00H
        db 00H, 08H, 1FH, 00H, 00H, 09H, 9FH, 00H
        db 14H, 07H, 63H, 00H, 00H, 08H, 7FH, 00H
        db 00H, 08H, 3FH, 00H, 00H, 09H, 0DFH, 00H
        db 12H, 07H, 1BH, 00H, 00H, 08H, 6FH, 00H
        db 00H, 08H, 2FH, 00H, 00H, 09H, 0BFH, 00H
        db 00H, 08H, 0FH, 00H, 00H, 08H, 8FH, 00H
        db 00H, 08H, 4FH, 00H, 00H, 09H, 0FFH, 00H

@@250:  db 10H, 05H, 01H, 00H, 17H, 05H, 01H, 01H
        db 13H, 05H, 11H, 00H, 1BH, 05H, 01H, 10H
        db 11H, 05H, 05H, 00H, 19H, 05H, 01H, 04H
        db 15H, 05H, 41H, 00H, 1DH, 05H, 01H, 40H
        db 10H, 05H, 03H, 00H, 18H, 05H, 01H, 02H
        db 14H, 05H, 21H, 00H, 1CH, 05H, 01H, 20H
        db 12H, 05H, 09H, 00H, 1AH, 05H, 01H, 08H
        db 16H, 05H, 81H, 00H, 40H, 05H, 00H, 00H
        db 10H, 05H, 02H, 00H, 17H, 05H, 81H, 01H
        db 13H, 05H, 19H, 00H, 1BH, 05H, 01H, 18H
        db 11H, 05H, 07H, 00H, 19H, 05H, 01H, 06H
        db 15H, 05H, 61H, 00H, 1DH, 05H, 01H, 60H
        db 10H, 05H, 04H, 00H, 18H, 05H, 01H, 03H
        db 14H, 05H, 31H, 00H, 1CH, 05H, 01H, 30H
        db 12H, 05H, 0DH, 00H, 1AH, 05H, 01H, 0CH
        db 16H, 05H, 0C1H, 00H, 40H, 05H, 00H, 00H

@@263:  db 03H, 00H, 04H, 00H, 05H, 00H, 06H, 00H
        db 07H, 00H, 08H, 00H, 09H, 00H, 0AH, 00H
        db 0BH, 00H, 0DH, 00H, 0FH, 00H, 11H, 00H
        db 13H, 00H, 17H, 00H, 1BH, 00H, 1FH, 00H
        db 23H, 00H, 2BH, 00H, 33H, 00H, 3BH, 00H
        db 43H, 00H, 53H, 00H, 63H, 00H, 73H, 00H
        db 83H, 00H, 0A3H, 00H, 0C3H, 00H, 0E3H, 00H
        db 02H, 01H, 00H, 00H, 00H, 00H, 00H, 00H

@@264:  db 10H, 00H, 10H, 00H, 10H, 00H, 10H, 00H
        db 10H, 00H, 10H, 00H, 10H, 00H, 10H, 00H
        db 11H, 00H, 11H, 00H, 11H, 00H, 11H, 00H
        db 12H, 00H, 12H, 00H, 12H, 00H, 12H, 00H
        db 13H, 00H, 13H, 00H, 13H, 00H, 13H, 00H
        db 14H, 00H, 14H, 00H, 14H, 00H, 14H, 00H
        db 15H, 00H, 15H, 00H, 15H, 00H, 15H, 00H
        db 10H, 00H, 0C9H, 00H, 0C4H, 00H, 00H, 00H

@@265:  db 01H, 00H, 02H, 00H, 03H, 00H, 04H, 00H
        db 05H, 00H, 07H, 00H, 09H, 00H, 0DH, 00H
        db 11H, 00H, 19H, 00H, 21H, 00H, 31H, 00H
        db 41H, 00H, 61H, 00H, 81H, 00H, 0C1H, 00H
        db 01H, 01H, 81H, 01H, 01H, 02H, 01H, 03H
        db 01H, 04H, 01H, 06H, 01H, 08H, 01H, 0CH
        db 01H, 10H, 01H, 18H, 01H, 20H, 01H, 30H
        db 01H, 40H, 01H, 60H, 00H, 00H, 00H, 00H

@@266:  db 10H, 00H, 10H, 00H, 10H, 00H, 10H, 00H
        db 11H, 00H, 11H, 00H, 12H, 00H, 12H, 00H
        db 13H, 00H, 13H, 00H, 14H, 00H, 14H, 00H
        db 15H, 00H, 15H, 00H, 16H, 00H, 16H, 00H
        db 17H, 00H, 17H, 00H, 18H, 00H, 18H, 00H
        db 19H, 00H, 19H, 00H, 1AH, 00H, 1AH, 00H
        db 1BH, 00H, 1BH, 00H, 1CH, 00H, 1CH, 00H
        db 1DH, 00H, 1DH, 00H, 40H, 00H, 40H, 00H
end;

function inflateEnd;
asm  pop ebp  // auto-generated push ebp; mov ebp,esp
        push    esi
        mov     esi, dword ptr [esp + 08H]
        test    esi, esi
        jz      @@195
        mov     eax, dword ptr [esi + 1CH]
        test    eax, eax
        jz      @@195
        mov     ecx, dword ptr [esi + 24H]
        test    ecx, ecx
        jz      @@195
        mov     eax, dword ptr [eax + 34H]
        test    eax, eax
        jz      @@194
        push    eax
        mov     eax, dword ptr [esi + 28H]
        push    eax
        call    ecx
@@194:  mov     ecx, dword ptr [esi + 1CH]
        mov     edx, dword ptr [esi + 28H]
        push    ecx
        push    edx
        call    dword ptr [esi + 24H]
        mov     dword ptr [esi + 1CH], 0
        xor     eax, eax
        pop     esi
        ret     4

@@195:  mov     eax, -2
        pop     esi
        ret     4
end;

{$else}

{$LINK infback.obj}
{$LINK inffast.obj}
{$LINK inflate.obj}
{$LINK inftrees.obj}

function inflateInit_(var strm: TZStream; version: PAnsiChar; stream_size: integer): integer; external;
function inflateInit2_; external;
function inflate; external;
function inflateEnd; external;

{$endif USEINLINEASM}

{$endif USEZLIB}

{$ifdef USEZLIB}

function adler32(adler: cardinal; buf: PAnsiChar; len: cardinal): cardinal;
begin
  result := ZLib.adler32(adler,pointer(buf),len);
end;

function crc32(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;
begin
  result := ZLib.crc32(crc,pointer(buf),len);
end;

function deflateInit2_(var strm: TZStream; level: integer; method: integer; windowBits: integer; memLevel: integer;strategy: integer; version: PAnsiChar; stream_size: integer): integer;
begin
  result := ZLib.deflateInit2_(z_streamp(@strm)^,level,method,windowBits,memLevel,strategy,version,stream_size);
end;

function deflate(var strm: TZStream; flush: integer): integer;
begin
  result := ZLib.deflate(z_streamp(@strm)^,flush);
end;

function deflateEnd(var strm: TZStream): integer;
begin
  result := ZLib.deflateEnd(z_streamp(@strm)^);
end;

function inflateInit2_(var strm: TZStream; windowBits: integer; version: PAnsiChar; stream_size: integer): integer;
begin
  result := ZLib.inflateInit2_(z_streamp(@strm)^,windowBits,version,stream_size);
end;

function inflate(var strm: TZStream; flush: integer): integer;
begin
  result := ZLib.inflate(z_streamp(@strm)^,flush);
end;

function inflateEnd(var strm: TZStream): integer;
begin
  result := ZLib.inflateEnd(z_streamp(@strm)^);
end;

function get_crc_table: pointer;
begin
  result := ZLib.get_crc_table;
end;

{$else}

function adler32;
// our optimized asm version, faster than the original zlib C implementation
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
  nop; nop
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

{$define BYFOUR}
// if defined, the crc32 hashing is performed using 8 tables, for better
// CPU pipelining and faster execution

var
  // tables content is created from code in initialization section below
  // (save 8 KB of code size from standard crc32.obj, with no speed penalty)
  crc32tab: array[0..{$ifdef BYFOUR}7{$else}0{$endif},byte] of cardinal;

function crc32(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;
// adapted from fast Aleksandr Sharahov version
asm
{$ifdef BYFOUR}
  test edx, edx
  jz   @ret
  neg  ecx
  jz   @ret
  not eax
  push ebx
@head:
  test dl, 3
  jz   @bodyinit
  movzx ebx, byte [edx]
  inc  edx
  xor  bl, al
  shr  eax, 8
  xor  eax,dword ptr [ebx*4 + crc32tab]
  inc  ecx
  jnz  @head
  pop  ebx
  not eax
@ret:
  ret
@bodyinit:
  sub  edx, ecx
  add  ecx, 8
  jg   @bodydone
  push esi
  push edi
  mov  edi, edx
  mov  edx, eax
@bodyloop:
  mov ebx, [edi + ecx - 4]
  xor edx, [edi + ecx - 8]
  movzx esi, bl
  mov eax,dword ptr [esi*4 + crc32tab + 1024*3]
  movzx esi, bh
  xor eax,dword ptr [esi*4 + crc32tab + 1024*2]
  shr ebx, 16
  movzx esi, bl
  xor eax,dword ptr [esi*4 + crc32tab + 1024*1]
  movzx esi, bh
  xor eax,dword ptr [esi*4 + crc32tab + 1024*0]
  movzx esi, dl
  xor eax,dword ptr [esi*4 + crc32tab + 1024*7]
  movzx esi, dh
  xor eax,dword ptr [esi*4 + crc32tab + 1024*6]
  shr edx, 16
  movzx esi, dl
  xor eax,dword ptr [esi*4 + crc32tab + 1024*5]
  movzx esi, dh
  xor eax,dword ptr [esi*4 + crc32tab + 1024*4]
  add ecx, 8
  jg  @done
  mov ebx, [edi + ecx - 4]
  xor eax, [edi + ecx - 8]
  movzx esi, bl
  mov edx,dword ptr [esi*4 + crc32tab + 1024*3]
  movzx esi, bh
  xor edx,dword ptr [esi*4 + crc32tab + 1024*2]
  shr ebx, 16
  movzx esi, bl
  xor edx,dword ptr [esi*4 + crc32tab + 1024*1]
  movzx esi, bh
  xor edx,dword ptr [esi*4 + crc32tab + 1024*0]
  movzx esi, al
  xor edx,dword ptr [esi*4 + crc32tab + 1024*7]
  movzx esi, ah
  xor edx,dword ptr [esi*4 + crc32tab + 1024*6]
  shr eax, 16
  movzx esi, al
  xor edx,dword ptr [esi*4 + crc32tab + 1024*5]
  movzx esi, ah
  xor edx,dword ptr [esi*4 + crc32tab + 1024*4]
  add ecx, 8
  jle @bodyloop
  mov eax, edx
@done:
  mov edx, edi
  pop edi
  pop esi
@bodydone:
  sub ecx, 8
  jl @tail
  pop ebx
  not eax
  ret
@tail:
  movzx ebx, byte [edx + ecx]
  xor bl,al
  shr eax,8
  xor eax,dword ptr [ebx*4 + crc32tab]
  inc ecx
  jnz @tail
  pop ebx
  not eax
{$else}
  test edx, edx
  jz @ret
  neg ecx
  jz @ret
  not eax
  sub edx,ecx
  push ebx
@next:
  movzx ebx, byte [edx + ecx]
  xor bl, al
  shr eax, 8
  xor eax, [ebx*4 + crc32tab]
  add ecx, 1
  jnz @next
  pop ebx
  not eax
@ret:
{$endif BYFOUR}
end;

function get_crc_table: pointer;
begin
  result := @crc32tab;
end;

{$endif USEZLIB}

{$endif LINUX}
 
function compressBound(sourceLen: cardinal): cardinal;
begin
  result := sourceLen + (sourceLen shr 12) + (sourceLen shr 14) + 11;
end;

procedure TZStream.Init;
begin
  fillchar(Self,sizeof(Self),0);
end;

function zcalloc(AppData: Pointer; Items, Size: cardinal): Pointer;
begin // direct use of the (FastMM4) delphi heap for all zip memory allocation
  Getmem(Result,Items * Size);
end;

procedure zcfree(AppData, Block: Pointer);
begin // direct use of the (FastMM4) delphi heap for all zip memory allocation
  FreeMem(Block);
end;

procedure _memcpy(dest, src: Pointer; count: integer); cdecl;
begin // will use fastcode if compiled within
  Move(src^, dest^, count);
end;

procedure _memset(dest: Pointer; val: Integer; count: integer); cdecl;
begin // will use fastcode if compiled within
  FillChar(dest^, count, val);
end;

function TZStream.DeflateInit(CompressionLevel: integer; ZipFormat: Boolean): Boolean;
var Bits: integer;
begin
  if ZipFormat then
    Bits := MAX_WBITS else
    Bits := -MAX_WBITS;
  result := deflateInit2_(self, CompressionLevel, Z_DEFLATED, Bits, DEF_MEM_LEVEL,
    Z_DEFAULT_STRATEGY, ZLIB_VERSION, sizeof(self))>=0
end;


procedure MessageError(const Text, Caption: string);
// simple Error MessageBox with translation (if any available)
{$ifdef MSWINDOWS}
begin
  MessageBox(0,pointer(Text),pointer(Caption),MB_ICONERROR);
{$else}
const EOL: word = $0A0D;          // #13#10
      Point: integer = $200A0D3A; // ':'#13#10' '
begin
  __write(stderr, EOL, 2);
  __write(stderr, Caption, Length(Caption)); __write(stderr, Point, 4);
  __write(stderr, Text, Length(Text)); __write(stderr, EOL, 2);
{$endif}
end;

function Check(const Code: Integer; const ValidCodes: array of Integer): integer;
var i: Integer;
begin
  if Code = Z_MEM_ERROR then
    OutOfMemoryError;
  result := code;
  for i := Low(ValidCodes) to High(ValidCodes) do
    if ValidCodes[i] = Code then
      Exit;
  MessageError(IntToStr(Code),'SynZip');
end;

(*function UpdateCrc32(acrc32: cardinal; inBuf: pointer; inLen: integer) : cardinal;
{  zlib crc32 : 46.63 ms  ict4zip updatecrc32 : 128.94 ms -> no comment ! }
type b=pByte;
var i: integer;
begin
  result := aCRC32;
  for i := 0 to (inLen shr 3)-1 do begin
    result := crc32tab[byte(result xor b(inBuf)^)] xor (result shr 8); inc(b(inBuf));
    result := crc32tab[byte(result xor b(inBuf)^)] xor (result shr 8); inc(b(inBuf));
    result := crc32tab[byte(result xor b(inBuf)^)] xor (result shr 8); inc(b(inBuf));
    result := crc32tab[byte(result xor b(inBuf)^)] xor (result shr 8); inc(b(inBuf));
    result := crc32tab[byte(result xor b(inBuf)^)] xor (result shr 8); inc(b(inBuf));
    result := crc32tab[byte(result xor b(inBuf)^)] xor (result shr 8); inc(b(inBuf));
    result := crc32tab[byte(result xor b(inBuf)^)] xor (result shr 8); inc(b(inBuf));
    result := crc32tab[byte(result xor b(inBuf)^)] xor (result shr 8); inc(b(inBuf));
  end;
  for i := 0 to (inLen and 7)-1 do begin
    result := crc32tab[byte(result xor b(inBuf)^)] xor (result shr 8); inc(b(inBuf)); end;
end;*)

function CompressString(const data: RawByteString; failIfGrow: boolean = false;
  CompressionLevel: integer=6) : RawByteString;
var i1 : integer;
begin
  result := '';
  SetLength(result, 12 + (length(data) * 11) div 10 + 12);
  pInt64(result)^ := length(data);
  PCardinalArray(result)^[2] := adler32(0, pointer(data), length(data));
  i1 := CompressMem(pointer(data), @PByteArray(result)[12], length(data),
    length(result) - 12, CompressionLevel);
  if (i1 > 0) and ( (12 + i1 < length(data)) or (not failIfGrow) ) then
       SetLength(result, 12 + i1)
  else result := '';
end;

function UncompressString(const data: RawByteString) : RawByteString;
begin
  result := '';
  if Length(data) > 12 then begin
    SetLength(result, PCardinal(data)^);
    SetLength(result, UncompressMem(@PByteArray(data)[12], pointer(result),
      length(data) - 12, length(result)));
    if (result <> '') and
     ((Adler32(0, pointer(result), length(result))) <> PCardinalArray(data)^[2]) then
      result := '';
  end;
end;

function CompressMem(src, dst: pointer; srcLen, dstLen: integer;
  CompressionLevel: integer=6; ZipFormat: Boolean=false) : integer;
var strm: TZStream;
begin
  strm.Init;
  strm.next_in := src;
  strm.avail_in := srcLen;
  strm.next_out := dst;
  strm.avail_out := dstLen; // -MAX_WBITS -> no zLib header => .zip compatible !
  if strm.DeflateInit(CompressionLevel,ZipFormat) then
  try
    Check(deflate(strm,Z_FINISH),[Z_STREAM_END,Z_OK]);
  finally
    deflateEnd(strm);
  end;
  result := strm.total_out;
end;

function CompressStream(src: pointer; srcLen: integer;
  aStream: TStream; CompressionLevel: integer=6; ZipFormat: Boolean=false): cardinal;
var strm: TZStream;
    code: integer;
    buf: array[word] of byte;
procedure FlushBuf;
var Count: integer;
begin
  Count := SizeOf(buf) - strm.avail_out;
  if Count=0 then exit;
  aStream.Write(buf,Count);
  strm.next_out := @buf;
  strm.avail_out := sizeof(buf);
end;
begin
  strm.Init;
  strm.next_in := src;
  strm.avail_in := srcLen;
  strm.next_out := @buf;
  strm.avail_out := sizeof(buf);
  if strm.DeflateInit(CompressionLevel,ZipFormat) then
  try                 // -MAX_WBITS -> no zLib header => .zip compatible !
    repeat
      code := Check(deflate(strm, Z_FINISH),[Z_OK,Z_STREAM_END,Z_BUF_ERROR]);
      FlushBuf;
    until code=Z_STREAM_END;
    FlushBuf;
  finally
    deflateEnd(strm);
  end;
  result := strm.total_out;
end;

function UnCompressMem(src, dst: pointer; srcLen, dstLen: integer) : integer;
var strm: TZStream;
//    Z: TMemoryStream; R: Int64Rec;
begin
  strm.Init;
  strm.next_in := src;
  strm.avail_in := srcLen;
  strm.next_out := dst;
  strm.avail_out := dstLen;
  if inflateInit2_(strm, -MAX_WBITS, ZLIB_VERSION, sizeof(strm))>=0 then
  try                 // -MAX_WBITS -> no zLib header => .zip compatible !
    Check(inflate(strm, Z_FINISH),[Z_OK,Z_STREAM_END]);
  finally
    inflateEnd(strm);
  end;
  result := strm.total_out;
{  Z := TMemoryStream.Create; R := UnCompressStream(src,srcLen,Z,nil);
  assert(CompareMem(Z.Memory,dst,R.Lo)); assert(R.Lo=strm.total_out);
  assert(crc32(0,dst,result)=R.Hi); Z.Free; }
end;

function UnCompressStream(
  src: pointer; srcLen: integer; aStream: TStream; checkCRC: PCardinal): cardinal;
// result:=dstLen  checkCRC(<>nil)^:=crc32  (if aStream=nil -> fast crc calc)
var strm: TZStream;
    code: integer;
    buf: array[word] of byte;
procedure FlushBuf;
var Count: integer;
begin
  Count := SizeOf(buf) - strm.avail_out;
  if Count=0 then exit;
  if CheckCRC<>nil then
    CheckCRC^ := crc32(CheckCRC^,@buf,Count);
  if aStream<>nil then
    aStream.Write(buf,Count);
  strm.next_out := @buf;
  strm.avail_out := sizeof(buf);
end;
begin
  strm.Init;
  strm.next_in := src;
  strm.avail_in := srcLen;
  strm.next_out := @buf;
  strm.avail_out := sizeof(buf);
  if checkCRC<>nil then
    CheckCRC^ := 0;
  if inflateInit2_(strm, -MAX_WBITS, ZLIB_VERSION, sizeof(strm))>=0 then
  try                 // -MAX_WBITS -> no zLib header => .zip compatible !
    repeat
      code := Check(inflate(strm, Z_FINISH),[Z_OK,Z_STREAM_END,Z_BUF_ERROR]);
      FlushBuf;
    until code=Z_STREAM_END;
    FlushBuf;
  finally
    inflateEnd(strm);
  end;
  result := strm.total_out;
end;

const
  gzheader : array [0..2] of cardinal = ($88B1F,0,0);

  HTTP_LEVEL = 1; // 6 is standard, but 1 is enough and faster

function CompressGZip(var Data: RawByteString; Compress: boolean): RawByteString;
var L: integer;
    P: PAnsiChar;
begin
  L := length(Data);
  if Compress then begin
    SetString(result,nil,L+128+L shr 3); // maximum possible memory required
    P := pointer(result);
    move(gzheader,P^,10);
    inc(P,10);
    inc(P,CompressMem(pointer(Data),P,L,length(result)-20,HTTP_LEVEL));
    PCardinal(P)^ := crc32(0,pointer(Data),L);
    inc(P,4);
    PCardinal(P)^ := L;
    inc(P,4);
    SetString(Data,PAnsiChar(pointer(result)),P-pointer(result));
  end else
    Data := gzread(pointer(Data),length(Data));
  result := 'gzip';
end;

function CompressDeflate(var Data: RawByteString; Compress: boolean): RawByteString;
var strm: TZStream;
    code, len: integer;
begin
  strm.Init;
  strm.next_in := pointer(Data);
  strm.avail_in := length(Data);
  if Compress then begin
    SetString(result,nil,strm.avail_in+256+strm.avail_in shr 3); // max mem required
    strm.next_out := pointer(result);
    strm.avail_out := length(result);
    // +MAX_WBITS below = encode in deflate format
    // Z_HUFFMAN_ONLY instead of Z_DEFAULT_STRATEGY is slowest and bad
    if deflateInit2_(strm, HTTP_LEVEL, Z_DEFLATED, +MAX_WBITS, DEF_MEM_LEVEL,
      Z_DEFAULT_STRATEGY, ZLIB_VERSION, sizeof(strm))>=0 then
    try
      Check(deflate(strm,Z_FINISH),[Z_STREAM_END]);
    finally
      deflateEnd(strm);
    end;
  end else begin
    len := (strm.avail_in*20)shr 3; // initial chunk size = comp. ratio of 60%
    SetString(result,nil,len);
    strm.next_out := pointer(result);
    strm.avail_out := len;
    if inflateInit2_(strm, +MAX_WBITS, ZLIB_VERSION, sizeof(strm))>=0 then
    try                 // +MAX_WBITS below = decode from deflate format
      repeat
        code := Check(inflate(strm, Z_FINISH),[Z_OK,Z_STREAM_END,Z_BUF_ERROR]);
        if strm.avail_out=0 then begin
          // need to increase buffer by chunk
          SetLength(result,length(result)+len);
          strm.next_out := PAnsiChar(pointer(result))+length(result)-len;
          strm.avail_out := len;
        end;
      until code=Z_STREAM_END;
    finally
      inflateEnd(strm);
    end;
  end;
  SetString(Data,PAnsiChar(pointer(result)),strm.total_out);
  result := 'deflate';
end;


{ TFileHeader }

function TFileHeader.DataPosition: cardinal;
begin
  result := localHeadOff+sizeof(TLocalFileHeader)+fileInfo.extraLen+fileInfo.nameLen;
end;

procedure TFileHeader.Init;
begin
  fillchar(self,sizeof(TFileHeader),0);
  signature   := $02014b51;
  dec(signature); // avoid finding the signature in the generated code
  madeBy      := $14;
  extFileAttr := $A0; // archive, normal
  fileInfo.neededVersion := $14;
end;

{ TFileInfo }

function TFileInfo.AlgoID: integer;
// 1..15  (1=SynLZ e.g.) from flags bits 7..10
begin
  // in PKware appnote, bits 7..10 of general purpose bit flag are not used
  result := (flags shr 7) and 15; // proprietary flag for SynZipFiles.pas
end;

function TFileInfo.SameAs(aInfo: PFileInfo): boolean;
begin // tolerate a time change through a network: zcrc32 is accurate enough
  result := (zzipMethod=aInfo.zzipMethod) and (zcrc32=aInfo.zcrc32) and
    (zzipSize=aInfo.zzipSize) and (zzipMethod=aInfo.zzipMethod) and
    (zfullSize=aInfo.zfullSize) and (flags=aInfo.flags);
end;

procedure TFileInfo.SetAlgoID(Algorithm: integer);
begin
  zzipMethod := 0; // file is stored, accorging to .ZIP standard
  // in PKware appnote, bits 7..10 of general purpose bit flag are not used
  flags := (Algorithm and 15) shl 7; // proprietary flag for SynZipFiles.pas
end;

function TFileInfo.GetUTF8FileName: boolean;
begin // from PKware appnote, Bit 11: Language encoding flag (EFS)
  result := (flags and (1 shl 11))<>0;
end;

procedure TFileInfo.SetUTF8FileName;
begin
  flags := flags or (1 shl 11);
end;

procedure TFileInfo.UnSetUTF8FileName;
begin
  flags := flags and not(1 shl 11);
end;

function CRC32string(const aString: RawByteString): cardinal;
// crc32 is better than adler32 for short strings, and fast enough in zlib 1.2.5
begin
  result := length(aString);
  if result<>0 then
    result := crc32(0,pointer(aString),result);
end;


{ TSynZipCompressor }

constructor TSynZipCompressor.Create(outStream: TStream; CompressionLevel: Integer;
  Format: TSynZipCompressorFormat = szcfRaw);
begin
  fDestStream := outStream;
  fGZFormat := (Format=szcfGZ);
  if fGZFormat then
    fDestStream.Write(gzHeader,10);
  with FStrm do begin
    Init;
    next_out := @FBufferOut;
    avail_out := SizeOf(FBufferOut);
    fInitialized := DeflateInit(CompressionLevel,Format=szcfZip);
  end;
end;

procedure TSynZipCompressor.Flush;
begin
  if FInitialized then begin
    while (Check(deflate(FStrm, Z_FINISH),[Z_OK, Z_STREAM_END])<>Z_STREAM_END) and
          (FStrm.avail_out=0) do
      FlushBufferOut;
    FlushBufferOut;
  end;
end;

destructor TSynZipCompressor.Destroy;
begin
  if FInitialized then begin
    Flush;
    FStrm.next_out := nil;
    FStrm.avail_out := 0;
    deflateEnd(FStrm);
  end;
  if fGZFormat then begin
    fDestStream.Write(fCRC,4);
    fDestStream.Write(SizeIn,4);
  end;
  inherited;
end;

function TSynZipCompressor.FlushBufferOut: integer;
begin
  Result := 0;
  if not FInitialized then
    exit;
  if FStrm.avail_out < SizeOf(FBufferOut) then begin
    Result := SizeOf(FBufferOut) - FStrm.avail_out;
    FDestStream.Write(FBufferOut, Result);
    FStrm.next_out := @FBufferOut;
    FStrm.avail_out := SizeOf(FBufferOut);
  end;
end;

function TSynZipCompressor.Read(var Buffer; Count: Integer): Longint;
begin
  assert(false);
  result := 0;
end;

function TSynZipCompressor.Seek(Offset: Integer; Origin: Word): Longint;
begin
  if not FInitialized then 
    result := 0 else
  if (Offset = 0) and (Origin = soFromCurrent) then // for TStream.Position
      Result := FStrm.total_in else begin
    Result := 0;
    assert((Offset = 0) and (Origin = soFromBeginning) and (FStrm.total_in = 0));
  end;
end;

function TSynZipCompressor.Write(const Buffer; Count: Integer): Longint;
begin
  if (self=nil) or not FInitialized or (Count<=0) then begin
    result := 0;
    exit;
  end;
  result := Count;
  fCRC := crc32(fCRC,@Buffer,Count);
  FStrm.next_in := pointer(@Buffer);
  FStrm.avail_in := Count;
  while FStrm.avail_in > 0 do begin // compress pending data
    if Check(deflate(FStrm, Z_NO_FLUSH), [Z_OK])<>Z_OK then
      raise Exception.Create('ZCompress');
    if FStrm.avail_out = 0 then
      FlushBufferOut;
  end;
  FStrm.next_in := nil;
  FStrm.avail_in := 0;
end;

{$ifndef USEZLIB}

{
  Generate a table for a byte-wise 32-bit CRC calculation on the polynomial:
  x^32+x^26+x^23+x^22+x^16+x^12+x^11+x^10+x^8+x^7+x^5+x^4+x^2+x+1.

  Polynomials over GF(2) are represented in binary, one bit per coefficient,
  with the lowest powers in the most significant bit.  Then adding polynomials
  is just exclusive-or, and multiplying a polynomial by x is a right shift by
  one.  If we call the above polynomial p, and represent a byte as the
  polynomial q, also with the lowest power in the most significant bit (so the
  byte 0xb1 is the polynomial x^7+x^3+x+1), then the CRC is (q*x^32) mod p,
  where a mod b means the remainder after dividing a by b.

  This calculation is done using the shift-register method of multiplying and
  taking the remainder.  The register is initialized to zero, and for each
  incoming bit, x^32 is added mod p to the register if the bit is a one (where
  x^32 mod p is p+x^32 = x^26+...+1), and the register is multiplied mod p by
  x (which is shifting right by one and adding x^32 mod p if the bit shifted
  out is a one).  We start with the highest power (least significant bit) of
  q and repeat for all eight bits of q.

  The table is simply the CRC of all possible eight bit values.  This is all
  the information needed to generate CRC's on data a byte at a time for all
  combinations of CRC register values and incoming bytes.
}
procedure InitCrc32Tab;
var i,n: integer;
    crc: cardinal;
begin // this code size is only 105 bytes, generating 8 KB table content
  for i := 0 to 255 do begin
    crc := i;
    for n := 1 to 8 do
      if (crc and 1)<>0 then
        // $edb88320 from polynomial p=(0,1,2,4,5,7,8,10,11,12,16,22,23,26)
        crc := (crc shr 1) xor $edb88320 else
        crc := crc shr 1;
    crc32tab[0,i] := crc;
  end;
{$ifdef BYFOUR}
  for i := 0 to 255 do begin
    crc := crc32tab[0,i];
    for n := 1 to 7 do begin
      crc := (crc shr 8) xor crc32tab[0,byte(crc)];
      crc32tab[n,i] := crc;
    end;
  end;
{$endif}
end;

initialization
  // crc32 tables content is created from code in initialization section below
  // (save 8 KB of code size from standard crc32.obj, with no speed penalty)
  InitCrc32Tab;
{$endif}
end.



