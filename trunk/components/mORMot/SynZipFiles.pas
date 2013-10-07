/// high-level access to .zip archive file compression
// - this unit is a part of the freeware Synopse SQLite3 database framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.13
unit SynZipFiles;

(*
    This file is part of Synopse SQLite3 database framework.

    Synopse SQLite3 database framework. Copyright (C) 2012 Arnaud Bouchez
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

  The Original Code is Synopse SQLite3 database framework.

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

  Version 1.10
  - code modifications to compile with Delphi 6 compiler

  Version 1.13
  - handle Unicode versions of Delphi (Delphi 2009/2010/XE)
  - now officialy handle UTF-8 encoded file names inside .Zip archive

*)

interface

uses
{$ifdef Win32}
  Windows,
  // USEPDF: under Win32: get ZLib 1.2.3 functions from libpdf.dll
  {$ifdef USEPDF}pdf,{$endif}
{$else}
  Libc, Types,
{$endif}
  SynCrypto, SynCommons,
  Classes, SysUtils, SynZip;

{
  Proprietary compression/encryption adding to standard zip files
  - the .zip file structure is 100% compatible with standard
  - data is stored uncompressed, but passed via TSynCompressionAlgo class
  - algorithms are ID-identified (from 1 to 15)
  - algorithm registration is made with SynCompressionAlgos.AlgoRegister
  - ID is stored in unused 7..10 bits of "flags" zip entry (cf. PKware appnote)
  - TSynCompressionAlgo is called by 64KB chunks or once for whole data
  - inherit from TSynCompressionAlgoBuf to simply handle 64KB chunks
  - Synopse has registered several TSynCompressionAlgo IDs:
     1=SynLz-chunked 2=SynLz-whole
     3=LzoAsm-chunked 4=LzoAsm-whole
     5=Bz2-chunked 
     6=AES-chunked 7=AES+Zip-chunked 8=AES+SynLz-chunked
    so you can use 9..15 for your own purpose 
  - most of this unit functions are TSynCompressionAlgo aware
}

type
  TZipException = class(Exception);

  TSynCompressionAlgo = class
  protected
    fDestStream: TStream;
  public
    /// initialize compression into OutStream
    procedure CompressInit(OutStream: TStream); virtual;
    /// compress InP[InLen] into OutStream + update CRC, return compressed length
    function Compress(InP: pointer; InLen: cardinal; CRC: PCardinal): cardinal; virtual; abstract;
    /// called once at the end for compression flush, return compressed length
    // (default implementation: just do nothing)
    function CompressFinish: cardinal; virtual;
    /// return uncompressed length of InP[InLen] for proper mem allocation
    function UnCompressedLength(InP: pointer; InLen: cardinal): cardinal; virtual; abstract;
    /// uncompress InP[InLen] into OutP, return uncompressed length (called once for decompression)
    function UnCompress(InP: pointer; InLen: cardinal; OutP: pointer): cardinal; virtual; abstract;
  end;

  TSynCompressionAlgoClass = class of TSynCompressionAlgo;

  TSynCompressionAlgos = object
    Values: array of record
      ID, WholeID: integer;
      func: TSynCompressionAlgoClass;
    end;
    procedure AlgoRegister(aAlgo: TSynCompressionAlgoClass; aID,aWholeID: integer);
    function Algo(aID: integer): TSynCompressionAlgoClass;
    function WholeAlgoID(aID: integer): integer;
  end;

  /// template class for 64KB chunked (not whole) algorithm (SynLZ, LZO...)
  // which forces storing as  uncompressed if compression ratio has no gain
  TSynCompressionAlgoBuf = class(TSynCompressionAlgo)
  protected
    // fast tmp buffer (size=worse case with 64KB chunk)
    fCompressBuf: PAnsiChar;
    function AlgoCompress(src: PAnsiChar; size: integer; dst: PAnsiChar): integer; virtual; abstract;
    function AlgoCompressLength(size: integer): integer; virtual; abstract;
    function AlgoUnCompress(src: PAnsiChar; size: integer; dst: PAnsiChar): integer; virtual; abstract;
    function AlgoUnCompressLength(src: PAnsiChar; size: integer): integer; virtual; abstract;
  public
    /// initialize compression into OutStream
    procedure CompressInit(OutStream: TStream); override;
    /// free fCompressBuf memory if allocated
    destructor Destroy; override;
    /// compress InP[InLen] into OutStream + update CRC, return compressed length
    function Compress(InP: pointer; InLen: cardinal; CRC: PCardinal): cardinal; override;
    /// return uncompressed length of InP[InLen] for proper mem allocation
    function UnCompressedLength(InP: pointer; InLen: cardinal): cardinal; override;
    /// uncompress InP[InLen] into OutP, return uncompressed length
    function UnCompress(InP: pointer; InLen: cardinal; OutP: pointer): cardinal; override;
  end;

  /// template class for whole algorithm (SynLZ, LZO...)
  // which forces storing as  uncompressed if compression ratio has no gain
  TSynCompressionAlgoWhole = class(TSynCompressionAlgo)
  protected
    function AlgoCompress(src: PAnsiChar; size: integer; dst: PAnsiChar): integer; virtual; abstract;
    function AlgoCompressLength(size: integer): integer; virtual; abstract;
    function AlgoUnCompress(src: PAnsiChar; size: integer; dst: PAnsiChar): integer; virtual; abstract;
    function AlgoUnCompressLength(src: PAnsiChar; size: integer): integer; virtual; abstract;
  public
    /// compress InP[InLen] into OutStream + update CRC, return compressed length
    function Compress(InP: pointer; InLen: cardinal; CRC: PCardinal): cardinal; override;
    /// return uncompressed length of InP[InLen] for proper mem allocation
    function UnCompressedLength(InP: pointer; InLen: cardinal): cardinal; override;
    /// uncompress InP[InLen] into OutP, return uncompressed length
    function UnCompress(InP: pointer; InLen: cardinal; OutP: pointer): cardinal; override;
  end;

  TZipCompressor = class(TStream)
  private
    fInitialized: Boolean;
    fDestStream: TStream;
    fStrm: TZStream;
    fAlgorithm: TSynCompressionAlgo;
    fAlgorithmStream: THeapMemoryStream;
    fAlgorithmID: integer; // =0 if not Assigned(fAlgorithm)
    fCRC: Cardinal;
    fBlobDataHeaderPosition: integer;
    fBufferIn, fBufferOut: array[word] of byte; // two 64kb buffers
    procedure Finish;
    function FlushBufferOut: integer;
    function InFlateDeflate: boolean; // return true if error
  public             
    constructor Create(outStream: TStream; CompressionLevel: Integer;
      Algorithm: integer=0);
    constructor CreateAsBlobData(outStream: TStream; CompressionLevel: Integer;
      Algorithm: integer=0);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function WriteOnce(const Buffer; Count: Longint): Longint;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property SizeIn: cardinal read FStrm.total_in;
    property SizeOut: cardinal read FStrm.total_out;
    property CRC: cardinal read fCRC;
  end;

  TGzWriter = class(TZipCompressor)
  private
    outFile: TStream;
    outFileToBeFree: boolean;
  public
    constructor Create(const aFileName: TFileName);  overload;
    constructor Create(const aDestStream: TStream);  overload;
    // use Write() to add some data
    destructor Destroy; override;
  end;

  PZipEntry = ^TZipEntry;
  TZipEntry = {$ifdef UNICODE} record {$else} object {$endif}
    ZipName: RawUTF8;
    Header: TFileHeader; // as stored in standard .zip file
    function SameAs(const aEntry: TZipEntry): boolean;
    // test if algo is registered, perform crc32 check and create one instance
    function AlgoCreate(data: pointer; const FileName: TFileName): TSynCompressionAlgo;
  end;

  TZipCommon = class
  private
    fCount: integer;
    fFileName: TFileName;
  public
    Entry: array of TZipEntry;
    constructor Create(const aFileName: TFileName);
    destructor Destroy; override;
    function ZipNameIndexOf(const aZipName: RawUTF8): integer;
    property Count: integer read fCount;
    property FileName: TFileName read fFileName;
  end;

  // used to transfert Blob Data from/to Client without compress/uncompress:
{$A-}
  PBlobData = ^TBlobData;
  TBlobData = object
  private
    // test if algo is registered, perform crc32 check and create one instance
    function AlgoCreate(data: pointer): TSynCompressionAlgo;
  public
    dataSize,  // always used
    dataFullSize, dataCRC: cardinal; // used only if AlreadyCompressed
    dataMethod: byte; // 0=stored 8=inflate >=16: AlgoID=dataMethod shr 4
    databuf: char;
    function AlgoID: cardinal;
    procedure SetFrom(const FileInfo: TFileInfo);
    // uncompress if necessary:
    function Expand: RawByteString;
    // do freemem() if dataMethod<>0; no direct AES since may be mapped
    function ExpandBuf(out destSize: cardinal): pointer;
    procedure ExpandStream(Stream: TStream);
    function Next: PAnsiChar; // points to next bloc
  end;
{$A8}

const
  BLOBDATA_HEADSIZE = sizeof(TBlobData)-1; // -1 for databuf: char

type
  TMemoryMap = object
  // use mapped files: very fast + avoid unnecessary disk access
  private
    _map: cardinal;
  public
    buf: PByteArray;
    _file,
    _size: integer;
    function DoMap(const aFileName: TFileName): boolean; // buf -> mapped file
    procedure UnMap;
  end;

  TZipReader = class(TZipCommon)
  private
    fMap: TMemoryMap;
  public
    constructor Create(const aFileName: TFileName);
    destructor Destroy; override;
    procedure Clear; // force Count=0
    function GetData(aIndex: integer; aStream: TStream=nil; CheckCRC: boolean=false;
      asBlobDataStored: boolean=false; withAlgoDataLen: boolean=false): PAnsiChar;
    function GetString(aIndex: integer): RawByteString;
    function GetBuffer(aIndex: integer; out Dest: PAnsiChar): integer;
    function GetBlobData(aIndex: integer): RawByteString; overload; // PBlobData(result)
    procedure GetBlobData(aIndex: integer; aStream: TStream); overload; // TBlobData->aStream
    procedure DeleteLastEntry; // don't use inside TZipValues: already done in Create
    procedure SaveToStream(aStream: TStream); // save uncompressed to stream
    function SameAs(aReader: TZipReader): boolean;
    property Map: TMemoryMap read fMap;
  end;

  TZipWriter = class(TZipCommon)
  private
    outFile: TFileStream;
    fNow: integer;
    fDestFileName: TFileName;
    function AddEntry(const aZipName: RawUTF8; FileAge: integer = 0): PZipEntry;
  public
    Zip: TZipCompressor;
    forceFileAge: integer; // <>0 -> will be used in Destroy to dest file
    constructor Create(const aFileName: TFileName);  overload;
    constructor Create(AppendTo: TZipReader; ReCreate: boolean=false); overload;
    constructor Create(fromStream: TStream; const DestFilename: TFileName=''); overload;
    // restore after TZipReader.SaveToStream()
    destructor Destroy; override;

    /// this Creates a TZipCompressor -> user Zip.Write() to send data:
    // if CompressionLevel<0: direct copy
    procedure ZipCreate(const aZipName: RawUTF8; CompressionLevel: integer;
      FileAge: integer = 0; Algorithm: integer=0);
    /// compression finish, fileInfo update+save, Zip.Free;
    // after ZipCreate: let aIndex=-1 will update Entry[Count]+inc(fCount)
    procedure ZipClose(aIndex: integer=-1);

    procedure Add(const aZipName: RawUTF8; data: PAnsiChar; dataSize: cardinal;
      CompressionLevel: integer; dataCRC: pCardinal=nil;
      FileAge: integer = 0; Algorithm: integer=0); overload;
    procedure Add(const aZipName: RawUTF8; p: PBlobData); overload;
    procedure Add(aReader: TZipReader; aReaderIndex: integer); overload;
    procedure AddFile(const aFileName: TFileName; const aZipName: RawUTF8;
      CompressionLevel: integer; Algorithm: integer=0);
    function LastCRC32Added: cardinal;
  end;

  // TZip handles ZIP standard files on disk
  TZip = class
  private
    FileQueue: TStringList;
    SomeDeleted: boolean;
  public
    Reader: TZipReader;
    Writer: TZipWriter;
    constructor Create(const aFileName: TFileName);
    destructor Destroy; override;
    function FileName: TFileName;
    function MarkDeleted(aReaderIndex: integer): boolean; virtual; // before any ZipCreate
    function MarkDeletedBefore(aDate: TDateTime; aBackup: TZip=nil): boolean;
    procedure BeginWriter; virtual;
    function  ZipCreate(const aZipName: RawUTF8; CompressionLevel: integer;
      Algorithm: integer=0): TZipCompressor;
    // use Zip.Write() to send data before ZipClose
    procedure ZipClose;
    function AddBuf(const aZipName: RawUTF8; CompressionLevel: integer;
      data: pointer; dataSize: cardinal; Algorithm: integer=0): boolean;
    function AddToFileQueue(const aFileName: TFileName;
      const aZipName: RawUTF8): boolean;  // flushed at Destroy
    function SameAs(aZip: TZip): boolean;
    function FileQueueCount: integer;
  end;

  TZipValues = class(TZip)
  // store some Values[] in a .zip file (TBlob, TBlobDiff, TDC4...)
  protected // all this must be overrided according to Values[]:
    procedure LoadValues(data: PAnsiChar); virtual; abstract;
    // SetLength(Values,Count+10); move(data,Values[0],Count*sizeof(Values[0]));
    procedure SaveValues(Zip: TZipCompressor); virtual; abstract;
  public
    Count: integer;
    modified: boolean;
    constructor Create(const aFileName: TFileName); // make Reader.Create
    destructor Destroy; override;
    function GetValue(aReaderIndex: integer; aStream: TStream=nil): PAnsiChar;
    procedure CopyValue(source, dest: integer); virtual; abstract;
    // Values[dest] := Values[source];
    procedure BeginWriter; override;
    function MarkDeleted(aReaderIndex: integer): boolean; override; // before any AddValue
  end;

var
  BlobDataNull: TBlobData;

  SynCompressionAlgos: TSynCompressionAlgos;


procedure CompressAsBlobData(const data; size: integer; aStream: TStream;
  CompressionLevel: integer=6; Algorithm: integer=0); 
// create a TBlobData in aStream - can use encryption with algo
// 7=AES+Zip-chunked and 8=AES+SynLz-chunked

function GZRead(const aFileName: TFileName): RawByteString; overload;
function GZRead(gz: PAnsiChar; gzLen: integer): RawByteString; overload;
procedure GZRead(const aFileName: TFileName; aStream: TStream; StoreLen: boolean); overload;
// direct uncompress .gz file into string or TStream

procedure GetDate(out Y,M,D: cardinal);


implementation

const // TZipException messages:
  sZlibInternalError = 'zlib: Internal error';
  sIncorrectZipFormatN = 'Incorrect zip format in file %s';
  sZipAlgoIDNUnknownN = 'Algo ID %d unknown for %s';
  sZipCrcErrorNN = 'crc32 checksum error for %s in %s';


{ TZipCommon }

constructor TZipCommon.Create(const aFileName: TFileName);
begin
  fFileName := aFileName;
end;

destructor TZipCommon.Destroy;
begin
  Finalize(Entry);
  inherited;
end;

function TZipCommon.ZipNameIndexOf(const aZipName: RawUTF8): integer;
begin
  for result := 0 to Count-1 do
    if SameTextU(Entry[result].ZipName,aZipName) then
      exit;
  result := -1;
end;

{ TZipReader }

procedure TZipReader.Clear;
begin
  fCount := 0;
  Map.UnMap;
end;

constructor TZipReader.Create(const aFileName: TFileName);
var i: integer;
    lhr: ^TLastHeader;
    H: ^TFileHeader;
    tmp: WinAnsiString;
    LastHeaderPosition: integer;
procedure Error;
begin
//  MessageBox(0,pointer(fFileName),'Incorrect format',MB_ICONERROR);
  Map.UnMap;
  fCount := 0;
  raise TZipException.CreateFmt(sIncorrectZipFormatN,[fFileName]);
end;
begin
  // 1. open aFileName
  inherited; // fFileName := aFileName;
  Map.DoMap(fFileName);
  if Map.buf=nil then exit;
  // 2. find last header, in order to reach the TFileHeader entries
  if Map._size<sizeof(lhr^) then begin
    Error;
    exit;
  end;
  lhr := @Map.Buf[Map._Size-sizeof(lhr^)];
  with lhr^ do begin
    if signature<>$06054b50 then begin
      Error;
      exit;
    end;
    fCount := thisFiles;
    SetLength(Entry,Count);
    LastHeaderPosition := headerOffset;
  end;
  // 3. read all TFileHeader entries and fill Entry[] with its values
  H := @Map.Buf[LastHeaderPosition];
  for i := 0 to Count-1 do
  with H^ do begin
    if signature<>$02014b50 then begin
      Error; 
      break; 
    end;
    with Entry[i] do begin
      if FileInfo.GetUTF8FileName then
        SetString(ZipName,PAnsiChar(H)+sizeof(H^),fileInfo.nameLen) else begin
        SetLength(tmp,fileInfo.nameLen); // convert from DOS/OEM into WinAnsi
        OemToCharBuffA(PAnsiChar(H)+sizeof(H^),pointer(tmp),fileInfo.nameLen);
        ZipName := WinAnsiToUtf8(tmp);
      end;
      ZipName := StringReplaceChars(ZipName,'/','\');
      Header := H^;
    end; // next entry is after the ZipNname and some extra/comment
    inc(PtrUInt(H),sizeof(H^)+fileInfo.nameLen+fileInfo.extraLen+commentLen);
  end;
end;

procedure TZipReader.DeleteLastEntry;
// don't use inside TZipValues: already done in TZipValues.Create
begin
  if Count<=0 then
    exit;
  dec(fCount);
end;

destructor TZipReader.Destroy;
begin
  Map.UnMap;
  inherited;
end;

function TZipReader.GetBlobData(aIndex: integer): RawByteString;
begin
  result := '';
  if (self=nil) or (cardinal(aIndex)>=cardinal(Count)) or (Map.buf=nil) then
    exit;
  with Entry[aIndex].Header do begin
    SetLength(result,fileInfo.zzipSize+BLOBDATA_HEADSIZE);
    with PBlobData(result)^ do begin
      SetFrom(fileInfo);
      move(Map.buf^[DataPosition],databuf,datasize);
    end;
  end;
end;

procedure TZipReader.GetBlobData(aIndex: integer; aStream: TStream);
// put TBlobData in aStream
var blob: TBlobData;
begin
  if (self=nil) or (aIndex<0) or (aIndex>=Count) or (Map.buf=nil) then
    aStream.Write(BlobDataNull,BLOBDATA_HEADSIZE) else
    with Entry[aIndex].Header do begin
      blob.SetFrom(fileInfo);
      aStream.Write(blob,BLOBDATA_HEADSIZE);
      aStream.Write(Map.buf^[DataPosition],blob.dataSize);
    end;
end;

function TZipReader.GetBuffer(aIndex: integer; out Dest: PAnsiChar): integer;
var data: PAnsiChar;
begin
  result := 0;
  Dest := nil;
  if (self=nil) or (aIndex<0) or (aIndex>=Count) or (Map.buf=nil) then
    exit;
  with Entry[aIndex], Header.fileInfo do begin
    data := @Map.buf^[Header.DataPosition];
    result := zfullSize;
    if AlgoID<>0 then begin
      with Entry[aIndex].AlgoCreate(data,FileName) do // crc32+algo object create
      try // algo registered (no TZipException raised) -> uncompress
        result := UnCompressedLength(data,zfullsize);
        Getmem(Dest,result);
        UnCompress(data,zfullsize,Dest); // direct uncompress into Dest
      finally
        Free;
      end;
      exit;
    end;
    Getmem(Dest,zfullSize);
    case zzipMethod of
    0: move(data^,Dest^,result); // stored = direct copy
    8: if (UnCompressMem(data,Dest,zzipSize,zfullSize)<>integer(zfullSize)) or
          (crc32(0,Dest,zfullSize)<>zcrc32) then begin
          Freemem(Dest);
          Dest := nil;
          raise TZipException.CreateFmt(sZipCrcErrorNN,[Entry[aIndex].ZipName,FileName]);
        end;
    end;
  end;
end;

function TZipReader.GetData(aIndex: integer; aStream: TStream=nil;
  CheckCRC: boolean=false; asBlobDataStored: boolean=false; withAlgoDataLen: boolean=false): PAnsiChar;
// aStream=nil  -> return bulk memory data position in mapped file
// aStream<>nil -> uncompress and un-algo into aStream; CheckCRC=true -> force check CRC
// asBlobDataStored=true -> PBlobData stored format into aStream
// withAlgoDataLen=true  -> unCompressed algo length stored into aStream
var CRC: cardinal;
    CRCP: PCardinal;
    Blob: TBlobData;
    tmp: PAnsiChar;
    L: cardinal;
begin
  if (self=nil) or (aIndex<0) or (aIndex>=Count) or (Map.buf=nil) then
    result := nil else
    with Entry[aIndex].Header do begin
      result := @Map.buf^[DataPosition];
      if aStream=nil then
        exit; // no decompress to stream: only get result PAnsiChar [and DataLen^]
      if fileInfo.AlgoID<>0 then begin // bits 7..10 are used for algo
        // un-algo specific uncompression
        L := GetBuffer(aIndex,tmp); // uncompress with algo (always check crc)
        if tmp<>nil then
        try
          if asBlobDataStored then begin // algo: add uncompressed data header
            Blob.dataSize := L;
            Blob.dataFullSize := L;
            Blob.dataCRC := crc32(0,tmp,L);
            Blob.dataMethod := fileInfo.AlgoID shl 4; // 0=stored + AlgoID
            aStream.Write(Blob,BLOBDATA_HEADSIZE);
          end else
          if withAlgoDataLen then
            aStream.Write(L,4);
          aStream.Write(tmp^,L); // write uncompressed
        finally
          freemem(tmp);
        end;
      end else begin // standard zip format
        if asBlobDataStored then begin
          Blob.dataSize := FileInfo.zfullSize;
          Blob.dataFullSize := FileInfo.zfullSize;
          Blob.dataCRC := FileInfo.zcrc32;
          Blob.dataMethod := 0; // stored, since will be uncompressed below
          aStream.Write(Blob,BLOBDATA_HEADSIZE);
        end;
        case fileInfo.zzipMethod of
        0: begin
          aStream.Write(result^,fileInfo.zfullSize); // stored = direct copy
          if CheckCRC then
            CRC := crc32(0,result,fileInfo.zfullSize);
        end;
        8: begin // deflate
          if CheckCRC then
            CRCP := @CRC else CRCP := nil;
          if UnCompressStream(result,fileInfo.zzipSize,aStream,CRCP)
               <>fileInfo.zfullSize then result := nil;
        end;
        end; // case fileInfo.zzipMethod of
        if CheckCRC and (CRC<>fileInfo.zcrc32) then
          result := nil;
      end;
    end;
end;

function TZipReader.GetString(aIndex: integer): RawByteString;
var data: PAnsiChar;
begin
  result := '';
  if (self=nil) or (aIndex<0) or (aIndex>=Count) or (Map.buf=nil) then
    exit;
  with Entry[aIndex].Header, fileInfo do begin
    data := @Map.buf^[DataPosition];
    if AlgoID<>0 then begin // special algo
      with Entry[aIndex].AlgoCreate(data,FileName) do // crc32+algo object create
      try // algo registered (no TZipException raised) -> uncompress
        SetLength(result,UnCompressedLength(data,zfullsize));
        if UnCompress(data,zfullsize,pointer(result))<> // direct uncompress into string
          cardinal(length(result)) then
          raise TZipException.CreateFmt(sZipCrcErrorNN,[Entry[aIndex].ZipName,FileName]);
      finally
        Free;
      end;
    end else // no algo: normal .zip file
    case zzipMethod of
      0: SetString(result,data,zfullSize); // stored = direct copy
      8: begin // deflate:
        SetLength(result,zfullSize);
        if (UnCompressMem(data,pointer(result),zzipSize,zfullSize)<>integer(zfullSize)) or
           (crc32(0,pointer(result),zfullSize)<>zcrc32) then
          raise TZipException.CreateFmt(sZipCrcErrorNN,[Entry[aIndex].ZipName,FileName]);
       end;
    end;
  end;
end;

function TZipReader.SameAs(aReader: TZipReader): boolean;
var i: integer;
begin
  result := self=aReader; if result then exit;
  if (self=nil) or (aReader=nil) or (Count<>aReader.Count) then
    exit;
  for i := 0 to Count-1 do
    if not Entry[i].SameAs(aReader.Entry[i]) then
      exit;
  for i := 0 to Count-1 do
    with Entry[i].Header do
    if not Comparemem(@Map.buf^[DataPosition],
      @aReader.Map.buf^[aReader.Entry[i].Header.DataPosition],fileInfo.zzipSize) then
      exit;
  result := true;
end;

procedure TZipReader.SaveToStream(aStream: TStream);
var i: integer;
    L: cardinal;
    aName: RawUTF8;
begin
  aName := StringToUTF8(ExtractFileName(fFileName));
  // 1. write global params
  L := length(aName);
  aStream.Write(L,1);
  aStream.Write(aName[1],L); // UTF-8 encoded file name 
  aStream.Write(fCount,4);
  // 2. write Entry[].ZipName
  for i := 0 to Count-1 do
  with Entry[i] do begin
    aStream.Write(Header.fileInfo,sizeof(Header.fileInfo));
    aStream.Write(pointer(ZipName)^,Header.fileInfo.NameLen);
  end;
  // 3. write all uncompressed data
  for i := 0 to Count-1 do
  with Entry[i] do // withAlgoDataLen=true: algo -> uncompressed length stored
    GetData(i,aStream,false,false,true); // deflate and un-algo if necessary
end;


{ TZip }

function TZip.AddBuf(const aZipName: RawUTF8; CompressionLevel: integer;
  data: pointer; dataSize: cardinal; Algorithm: integer=0): boolean;
var Z: TZipCompressor;
begin
  if self=nil then begin
    result := false;
    exit;
  end;
  if dataSize<512 then
    CompressionLevel := -1; // force store if too small
  Z := ZipCreate(aZipName,CompressionLevel,Algorithm); // Z=Writer.Zip
  result := Z<>nil;
  if not result then exit;
  Z.WriteOnce(data^,dataSize);
  ZipClose;
end;

function TZip.AddToFileQueue(const aFileName: TFileName; const aZipName: RawUTF8): boolean;
// flushed at Destroy
var i: integer;
begin
  result := false;
  if self=nil then exit;
  if Writer=nil then begin
    i := Reader.ZipNameIndexOf(aZipName);
    if i>=0 then
      MarkDeleted(i);
  end else
    if Writer.ZipNameIndexOf(aZipName)>=0 then
      exit;
  if FileQueue=nil then
    FileQueue := TStringList.Create;
  FileQueue.Values[aFileName] := UTF8ToString(aZipName);
  result := true;
end;

procedure TZip.BeginWriter;
begin
  if (self<>nil) and (Writer=nil) then
    Writer := TZipWriter.Create(Reader); // append Reader, recreate=false
end;

constructor TZip.Create(const aFileName: TFileName);
begin
  Reader := TZipReader.Create(aFileName);
  // Writer will be created as necessary
end;

function GetValueFromIndex(List: TStrings; Index: Integer): string;
begin // not defined before Delphi 7
  if Index >= 0 then
    Result := Copy(List[Index],Length(List.Names[Index])+2,MaxInt) else
    Result := '';
end;

destructor TZip.Destroy;
var i, method: integer;
    zipName: TFileName;
begin
  ZipClose; // close pending Writer.Zip if any
  if SomeDeleted and (Writer=nil) and
    ((FileQueue=nil) or (FileQueue.Count=0)) then
    BeginWriter else
  if FileQueue<>nil then
  for i := 0 to FileQueue.Count-1 do begin
    zipName := GetValueFromIndex(FileQueue,i);
    if zipName='' then
      continue;
    if GetFileNameExtIndex(zipName,'zip,jpg,jpeg,gz,bz2,bZ,7z,gif,bj,bjt')>=0 then
      method := -1 else // store already compressed file
      method := 6;      // normal deflate compression
    if Writer=nil then
      BeginWriter;
    Writer.AddFile(FileQueue.Names[i],StringToUTF8(zipName),method);
  end;
  FreeAndNil(FileQueue);
  FreeAndNil(Reader);
  FreeAndNil(Writer);
  inherited;
end;

function TZip.FileName: TFileName;
begin
  if (self=nil) or (Reader=nil) then
    result := '' else
    result := Reader.fFileName;
end;

function TZip.FileQueueCount: integer;
begin
  if FileQueue=nil then
    result := 0 else
    result := FileQueue.Count;
end;

function TZip.MarkDeleted(aReaderIndex: integer): boolean;
begin
  result := (self<>nil)and(Writer=nil)and(aReaderIndex>=0)and(aReaderIndex<Reader.Count);
  if not result then
    exit;
  SomeDeleted := true;
//if aReaderIndex=Reader.Count-1 then Reader.DeleteLastEntry else !TZipValues use signature!
  Reader.Entry[aReaderIndex].Header.signature := 0; // just signature = 0 to delete
end;

{$ifndef Win32}
function DateTimeToFileDateWindows(DateTime: TDateTime): Integer;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  DecodeDate(DateTime, Year, Month, Day);
  if (Year < 1980) or (Year > 2107) then
    Result := 0 else begin
    DecodeTime(DateTime, Hour, Min, Sec, MSec);
    LongRec(Result).Lo := (Sec shr 1) or (Min shl 5) or (Hour shl 11);
    LongRec(Result).Hi := Day or (Month shl 5) or ((Year - 1980) shl 9);
  end;
end;

function NowToFileDateWindows: Integer;
begin
  result := DateTimeToFileDateWindows(Now);
end;
{$endif}

function TZip.MarkDeletedBefore(aDate: TDateTime; aBackup: TZip=nil): boolean;
var dt, i: integer;
begin
  result := false;
  if (self=nil) or (Writer<>nil) then
    exit;
  dt := {$ifdef Win32}DateTimeToFileDate{$else}DateTimeToFileDateWindows{$endif}(aDate);
  for i := 0 to Reader.Count-1 do
  with Reader.Entry[i].Header do
  if (signature<>0) and (fileInfo.zlastMod<dt) then begin
    if aBackup<>nil then begin
      aBackup.BeginWriter;
      aBackup.Writer.Add(Reader,i);
    end;
    MarkDeleted(i);
    result := true;
  end;
end;

function TZip.SameAs(aZip: TZip): boolean;
begin
  result := self=aZip;
  if result then
    exit;
  if (self=nil) or (aZip=nil) then
    exit;
  assert(Writer=nil);
  if Writer<>nil then
    exit;
  result := Reader.SameAs(aZip.Reader);
end;

procedure TZip.ZipClose;
begin
  if (self=nil) or (Writer=nil) then exit;
  Writer.ZipClose;
end;

function TZip.ZipCreate(const aZipName: RawUTF8; CompressionLevel: integer;
  Algorithm: integer=0): TZipCompressor;
// use TZipCompressor.Write() to send data - CompressionLevel<0 -> force store
var i: integer;
begin
  result := nil;
  if self=nil then exit;
  if Writer=nil then begin
    i := Reader.ZipNameIndexOf(aZipName);
    if (i>=0) then
      MarkDeleted(i);
  end else
    if Writer.ZipNameIndexOf(aZipName)>=0 then
      exit;
  if FileQueue<>nil then
  for i := 0 to FileQueue.Count-1 do
    if GetValueFromIndex(FileQueue,i)=aZipName then begin
      FileQueue.Delete(i);
      break;
    end;
  BeginWriter;
  if Writer.Zip<>nil then
    exit;
  Writer.ZipCreate(aZipName,CompressionLevel,0,Algorithm);
  result := Writer.Zip;
end;


{ TCompressorDecompressor }

constructor TZipCompressor.Create(outStream: TStream; CompressionLevel, Algorithm: Integer);
var Algo: TSynCompressionAlgoClass;
begin
  fDestStream := outStream;
  fBlobDataHeaderPosition := -1; // not AsBlobData
  with FStrm do begin
    Init;
    next_out := @FBufferOut;
    avail_out := SizeOf(FBufferOut);
    next_in := @FBufferIn;
  end;
  if Algorithm<>0 then begin
    Algo := SynCompressionAlgos.Algo(Algorithm);
    if not Assigned(Algo) then // unknown algo -> error
      raise TZipException.CreateFmt(sZipAlgoIDNUnknownN,[Algorithm,ClassName]);
    fAlgorithm := Algo.Create;
    fAlgorithmID := Algorithm;
    fAlgorithm.CompressInit(fDestStream);
    if SynCompressionAlgos.WholeAlgoID(Algorithm)=Algorithm then
      // whole algo = not a 64KB chunked algo
      fAlgorithmStream := THeapMemoryStream.Create; // create temp buffer
  end else begin
    if CompressionLevel>=0 then // FInitialized=false -> direct copy to FDestStream
      fInitialized := Check(deflateInit2_(FStrm, CompressionLevel, Z_DEFLATED,
      -MAX_WBITS, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY,ZLIB_VERSION, sizeof(FStrm)),
      [Z_OK])=Z_OK;  // -MAX_WBITS -> no zLib header => .zip compatible !
  end;
end;

constructor TZipCompressor.CreateAsBlobData(outStream: TStream;
  CompressionLevel, Algorithm: Integer);
begin
  Create(outStream,CompressionLevel,Algorithm);
  fBlobDataHeaderPosition := outStream.Seek(0,soFromCurrent);
  outStream.Write(FBufferOut,BLOBDATA_HEADSIZE); // save Bulk header
end;

destructor TZipCompressor.Destroy;
var p: integer;
    blob: TBlobData;
begin
  if FInitialized then begin
    FStrm.next_out := nil;
    FStrm.avail_out := 0;
    deflateEnd(FStrm);
  end else begin
    FreeAndNil(fAlgorithmStream);
    FreeAndNil(fAlgorithm);
  end;
  if fBlobDataHeaderPosition>=0 then begin // CreateAsBlobData() -> update header
    p := fDestStream.Seek(0,soFromCurrent);
    with blob do begin
      dataFullSize := SizeIn;
      dataSize := p-fBlobDataHeaderPosition-BLOBDATA_HEADSIZE;
      assert(dataSize=SizeOut);
      dataCRC := CRC;
      // dataMethod: 0=stored 8=inflate >16: AlgoID=dataMethod shr 4
      if FInitialized then
        dataMethod := 8 else
        dataMethod := fAlgorithmID shl 4; // stored + AlgoID
    end;
    fDestStream.Seek(fBlobDataHeaderPosition,soFromBeginning);
    fDestStream.Write(blob,BLOBDATA_HEADSIZE);
    fDestStream.Seek(p,soFromBeginning);
  end;
  inherited;
end;

procedure TZipCompressor.Finish;
begin
  if (self=nil) then exit;
  if assigned(fAlgorithm) then begin
    if assigned(fAlgorithmStream) then begin
      fAlgorithmStream.Write(fBufferIn,fStrm.avail_in); // write pending data
      fStrm.total_in := fAlgorithm.Compress( // compress whole data at once
        fAlgorithmStream.Memory,fAlgorithmStream.Seek(0,soFromCurrent),@fCRC);
    end else
    if FStrm.avail_in>0 then
      inc(fStrm.total_in,fAlgorithm.Compress(@fBufferIn,FStrm.avail_in,@fCRC));
    inc(fStrm.total_in,fAlgorithm.CompressFinish); // finish compression
    fStrm.total_out := fStrm.total_in; // .zip file compression mode = stored
    exit;
  end;
  if not FInitialized then
    exit;
  while FStrm.avail_in > 0 do begin // compress pending data
    if InFlateDeflate then
      raise TZipException.Create(SZlibInternalError);
    if FStrm.avail_out = 0 then
      FlushBufferOut;
  end;
  FStrm.next_in := nil;
  FStrm.avail_in := 0;
  while (Check(deflate(FStrm, Z_FINISH), [Z_OK, Z_STREAM_END]) <> Z_STREAM_END) and
      (FStrm.avail_out = 0) do
    FlushBufferOut;
  FlushBufferOut;
end;

function TZipCompressor.FlushBufferOut: integer;
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

function TZipCompressor.InFlateDeflate: boolean;
begin
  Result := Check(deflate(FStrm, Z_NO_FLUSH), [Z_OK])<>Z_OK;
end;

function TZipCompressor.Read(var Buffer; Count: Longint): Longint;
begin
  assert(false);
  result := 0;
end;

function TZipCompressor.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if not FInitialized then // CompressionLevel<0: direct copy to
    result := 0 else
  if (Offset = 0) and (Origin = soFromCurrent) then // for TStream.Position
    if assigned(fAlgorithmStream) then
      Result := fAlgorithmStream.Seek(0,soFromCurrent) else
      Result := FStrm.total_in else begin
    Result := 0;
    assert((Offset = 0) and (Origin = soFromBeginning) and (FStrm.total_in = 0));
  end;
end;

function TZipCompressor.Write(const Buffer; Count: integer): integer;
begin
  if self<>nil then begin
    result := Count;
    if FInitialized then begin
      if Count=0 then exit;
      fCRC := crc32(fCRC,@Buffer,Count);
      if cardinal(Count)+FStrm.avail_in>sizeof(fBufferIn)-1 then begin
        while FStrm.avail_in > 0 do begin
          if InflateDeflate then
            raise TZipException.Create(SZlibInternalError);
          if FStrm.avail_out = 0 then
            FlushBufferOut;
        end;
        FStrm.avail_in := 0;
        FStrm.next_in := @fBufferIn;
      end;
      if Count<sizeof(fBufferIn) then begin
        move(Buffer,fBufferIn[FStrm.avail_in],Count);
        inc(FStrm.avail_in,Count);
      end else begin
        FStrm.avail_in := Count;
        FStrm.next_in := @Buffer;
        while FStrm.avail_in > 0 do begin
          if InFlateDeflate then
            raise TZipException.Create(SZlibInternalError);
          if FStrm.avail_out = 0 then
            FlushBufferOut;
        end;
        FStrm.avail_in := 0;
        FStrm.next_in := @fBufferIn;
      end;
    end else begin // if not FIinitialized: CompressionLevel<0: direct copy to
      if Count=0 then exit;
      if Assigned(fAlgorithmStream) then begin
        // algo -> copy into fAlgorithmStream by fBufferIn[] chunks
        if cardinal(Count)+FStrm.avail_in>sizeof(fBufferIn)-1 then begin
          fAlgorithmStream.Write(fBufferIn,fStrm.avail_in); // flush buffer
          FStrm.avail_in := 0;
          FStrm.next_in := @fBufferIn;
        end;
        if Count<sizeof(fBufferIn) then begin // small block -> in fBufferIn[]
          move(Buffer,fBufferIn[FStrm.avail_in],Count);
          inc(FStrm.avail_in,Count);
        end else
          fAlgorithmStream.Write(Buffer,Count); // big block -> direct store
      end else
      if Assigned(fAlgorithm) then begin
        // algo without fAlgorithmStream -> direct compress in fBufferIn[] chunks
        FStrm.next_out := @Buffer;
        repeat
          FStrm.avail_out := sizeof(fBufferIn)-FStrm.avail_in;
          if cardinal(Count)<=FStrm.avail_out then begin // count fit in fBufferIn[]
            move(FStrm.next_out^,fBufferIn[FStrm.avail_in],Count); // -> copy bytes
            inc(FStrm.avail_in,Count);
            break;
          end else begin // Count too big for fBufferIn[] -> compress chunk
            if FStrm.avail_in=0 then begin // direct compress from buffer
              inc(fStrm.total_in,fAlgorithm.Compress(FStrm.next_out,sizeof(fBufferIn),@fCRC));
              inc(FStrm.next_out,sizeof(fBufferIn));
              dec(Count,sizeof(fBufferIn));
            end else begin // compress with data already in fBufferIn[]
              move(FStrm.next_out,fBufferIn[FStrm.avail_in],FStrm.avail_out);
              inc(fStrm.total_in,fAlgorithm.Compress(@fBufferIn,sizeof(fBufferIn),@fCRC));
              FStrm.avail_in := 0;
              FStrm.next_in := @fBufferIn;
              inc(FStrm.next_out,FStrm.avail_out);
              dec(Count,FStrm.avail_out);
            end;
          end;
        until Count=0;
      end else begin
        // normal store -> direct copy to fDestStream
        inc(FStrm.total_in,Count);
        inc(FStrm.total_out,Count);
        fCRC := crc32(fCRC,@Buffer,Count);
        fDestStream.Write(Buffer,Count);
      end;
    end;
  end else
    result := 0; // self=nil
end;

function TZipCompressor.WriteOnce(const Buffer; Count: Integer): Longint;
// same as Write, but optimized for one call of Write()
begin
  if Count=0 then
    result := Count else
  if Assigned(fAlgorithmStream) then begin // whole: avoid fAlgorithmStream use
    FreeAndNil(fAlgorithmStream); // very fast, since memory already allocated=0
    result := fAlgorithm.Compress(@Buffer,Count,@fCRC)+fAlgorithm.CompressFinish;
    fStrm.total_in := result;
  end else
    result := Write(Buffer,Count);
end;

{ TGzWriter }

constructor TGzWriter.Create(const aFileName: TFileName);
begin
  if FileExists(aFilename) then begin
    Create(TFileStream.Create(aFileName,fmOpenWrite));
    outFile.Size := outFile.Position;
  end else
    Create(TFileStream.Create(aFileName,fmCreate));
  outFileToBeFree := true;
end;

constructor TGzWriter.Create(const aDestStream: TStream);
const gzheader : array [0..2] of cardinal = ($88B1F,0,0);
begin
  outFile := aDestStream;
  outFile.Write(gzHeader,10);
  inherited Create(outFile, 6);
end;

destructor TGzWriter.Destroy;
begin
  Finish;
  outFile.Write(CRC,4);
  outFile.Write(SizeIn,4);
  if outFileToBeFree then
    FreeAndNil(outFile);
  inherited;
end;

function GZRead(const aFileName: TFileName): RawByteString; overload;
var Map: TMemoryMap;
begin
  if not Map.DoMap(aFileName) then
    result := '' else
    try
      result := SynZipFiles.GzRead(pointer(Map.buf),Map._size);
    finally
      Map.UnMap;
    end;
end;

function GZRead(gz: PAnsiChar; gzLen: integer): RawByteString; overload;
var Len: integer;
begin
  if PCardinal(gz)^<>$88B1F then
    SetString(result,gz,gzLen) else begin
    Len := pInteger(@gz[gzLen-4])^;
    assert(Len>=0);
    SetString(result,nil,Len);
    UnCompressMem(@gz[10],pointer(result),gzLen-18,Len);
  end;
end;

procedure GZRead(const aFileName: TFileName; aStream: TStream; StoreLen: boolean); overload;
// just add an ungz file contents, storing len:Integer first if StoreLen=true
var Map: TMemoryMap;
    Len: integer;
begin
  if not Map.DoMap(aFileName) then begin
    if StoreLen then
      aStream.Write(Map.Buf,4); // no file -> store len=0 
  end else
    try
      if PCardinal(Map.buf)^<>$88B1F then begin
        if StoreLen then
          aStream.Write(Map._Size,4);
        aStream.Write(Map.buf^,Map._size); // not a .gz -> store as is
      end else begin
        Len := pInteger(@Map.buf[Map._size-4])^; // .gz -> uncompress
        assert(Len>=0);
        if StoreLen then
          aStream.Write(Len,4);
        UnCompressStream(@Map.buf[10],Map._size-18,aStream,nil);
      end;
    finally
      Map.UnMap;
    end;
end;

{ TZipWriter }

constructor TZipWriter.Create(AppendTo: TZipReader; ReCreate: boolean=false);
  procedure InitTmp;     
  begin
    fDestFileName := AppendTo.fFileName;
    fFileName := ChangeFileExt(fDestFileName,'.tmp');
    outFile := TFileStream.Create(fFileName,fmCreate);
  end;
var i, firstDeleted: integer;
begin
  fNow := {$ifdef Win32}DateTimeToFileDate{$else}DateTimeToFileDateWindows{$endif}(Now);
  if AppendTo=nil then
    exit;
  SetLength(Entry,AppendTo.Count+32);
  if ReCreate then begin // force full file recreate from AppendTo data
    InitTmp;
    // don't call AppendTo.Map.UnMap since we will need to read the data!
    exit; // all the data will be copied from AppendTo.Map manually by caller
  end;
  firstDeleted := -1;
  for i := 0 to AppendTo.Count-1 do
    if AppendTo.Entry[i].Header.signature<>0 then begin // file not deleted
      Entry[Count] := AppendTo.Entry[i]; // -> add Entry[]
      inc(fCount);
    end else // file deleted
      if firstDeleted<0 then // -> update first deleted index
        firstDeleted := i;
  if (Count=0) then begin
  // nothing to read from old file -> just reopen
    AppendTo.Map.UnMap;
    fFileName := AppendTo.fFileName;
    DeleteFile(fFileName); // avoid win32 bug if filesize=0 
    outFile := TFileStream.Create(fFileName,fmCreate);
  end else
  if (Count=AppendTo.Count) or (firstDeleted=AppendTo.Count-1) then begin
  // no delete or only the last one: append to end of file
    fFileName := AppendTo.fFileName;
    if AppendTo.Map.buf=nil then // AppendTo file doesn't exists
      outFile := TFileStream.Create(fFileName,fmCreate) // new void file
    else begin // AppendTo file exists
      AppendTo.Map.UnMap; // outFile seek to end of AppendTo file data
      outFile := TFileStream.Create(fFileName,fmOpenReadWrite);
      with AppendTo.Entry[Count-1].Header do
        outFile.Position := DataPosition+fileInfo.zzipSize;
    end;
  end else begin
  // some deleted: copy entries from mapped file to .tmp file
    InitTmp;
    fCount := 0; // recreate Entry[] in Add(AppendTo,i) below
    for i := 0 to AppendTo.Count-1 do
      if AppendTo.Entry[i].Header.signature<>0 then
        Add(AppendTo,i); // add not deleted entries
    AppendTo.Map.UnMap; // we won't use AppendTo any more
  end;
end;

constructor TZipWriter.Create(fromStream: TStream; const DestFileName: TFileName='');
// used to restore data uncompressed+bz-compressed with TZipReader.SaveToStream()
var i, sign: integer;
    L, srcLen: cardinal;
    src: pointer; // temporary buffer for CompressMem
    aAlgo, wAlgo: integer;
    fromMemory: PAnsiChar;
begin
  fNow := {$ifdef Win32}DateTimeToFileDate{$else}DateTimeToFileDateWindows{$endif}(Now);
  // 1. read global params
  L := 0;
  fromStream.Read(L,1);
  if DestFileName<>'' then begin
    fFileName := DestFileName;
    fromStream.Seek(L,soFromCurrent); // ignore file name stored in fromStream
  end else begin
    SetLength(fFileName,L);
    fromStream.Read(fFileName[1],L); // fromStream -> dest file name 
  end;
  fromStream.Read(fCount,4);
  // 2. read Entry[]
  SetLength(Entry,fCount);
  for i := 0 to Count-1 do
  with Entry[i] do begin
    Header.Init; // signature, madeBy, extFileAttr init
    fromStream.Read(Header.FileInfo,sizeof(Header.fileInfo));
    SetLength(ZipName,Header.FileInfo.nameLen);
    fromStream.Read(ZipName[1],Header.FileInfo.nameLen);
  end;
  // 3. read and recompress all data
  outFile := TFileStream.Create(fFileName,fmCreate);
  if fromStream.InheritsFrom(TMemoryStream) then
    fromMemory := PAnsiChar(TMemoryStream(fromStream).Memory)+
      fromStream.Seek(0,soFromCurrent) else
    fromMemory := nil;
  srcLen := 0;
  src := nil;
  for i := 0 to Count-1 do
  with Entry[i],Header.fileInfo do begin
    Header.localHeadOff := outFile.Position; // position can change, as we recompress
    sign := $04034b50; outFile.Write(sign,4); // write .zip fileinfo signature
    aAlgo := AlgoID;
    if (aAlgo<>0) or (zzipMethod=8) then begin // reuse same compression/algo
      // special ZipCreate(), without AddEntry():
      if aAlgo<>0 then begin // reuse same algo
        wAlgo := SynCompressionAlgos.WholeAlgoID(aAlgo); // whole algo is prefered here
        if wAlgo<>0 then begin
          aAlgo := wAlgo;
          Header.fileInfo.SetAlgoID(aAlgo); // update Header.fileInfo.flags
        end;
      end;
      Zip := TZipCompressor.Create(outFile, 6, aAlgo);
      outFile.Write(neededVersion,sizeof(Header.fileInfo)); // save bulk fileInfo
      outFile.Write(ZipName[1],nameLen);
      if fromMemory<>nil then begin
        Zip.WriteOnce(fromMemory[4],pInteger(fromMemory)^); // direct recompress using algo
        L := pInteger(fromMemory)^+4;
        inc(fromMemory,L); // jump uncompressed data
        fromStream.Seek(L,soFromCurrent); // synchronize fromStream position
      end else begin
        fromStream.Read(L,4); // SaveStream(..,withAlgoDataLen=true)
        if L>srcLen then begin
          if srcLen<>0 then
            Freemem(src); // Freemem+Getmem is better than Reallocmem (no move)
          srcLen := succ(L shr 12) shl 12; // 4KB size boundary
          Getmem(src,srcLen);
        end;
        fromStream.Read(src^,L); // read uncompressed data
        Zip.WriteOnce(src^,L); // recompress using algo
      end;
      ZipClose(i); // fileInfo update+Zip.Free; aIndex>=0 -> no inc(fCount)
    end else begin
      assert(zzipMethod=0);
      zzipSize := zfullSize;
      outFile.Write(neededVersion,sizeof(Header.fileInfo)); // save new fileInfo
      outFile.Write(ZipName[1],nameLen);
      outFile.CopyFrom(fromStream,zzipSize);
    end;
  end;
end;

constructor TZipWriter.Create(const aFileName: TFileName);
begin
  fNow := {$ifdef Win32}DateTimeToFileDate{$else}DateTimeToFileDateWindows{$endif}(Now);
  SetLength(Entry,100);
  fFileName := aFileName;
  outFile := TFileStream.Create(fFileName,fmCreate);
end;

destructor TZipWriter.Destroy;
var i: integer;
    lhr: TLastHeader;
begin
  if not Assigned(outFile) then begin // an error occured during outfile creation
    inherited; // -> just free memory and leave
    exit;
  end;
  // 1. prepare last header
  with lhr do begin
    signature     := $06054b50;
    thisDisk      := 0;
    headerDisk    := 0;
    thisFiles     := Count;
    totalFiles    := Count;
    headerSize    := 0;
    headerOffset  := outFile.seek(0,soFromCurrent); // position of file entries
    commentLen    := 0;
  end;
  // 2. write file entries from Entry[]
  for i := 0 to Count-1 do
  with Entry[i] do begin
    inc(lhr.headerSize, sizeOf(TFileHeader)+length(ZipName));
    outFile.Write(Header,sizeof(TFileHeader));
    outFile.Write(ZipName[1],length(ZipName));
  end;
  // 3. write last header
  outFile.Write(lhr,sizeof(lhr));
  // 4. truncate and close file
{$ifdef Win32}
  SetEndOfFile(outFile.Handle);
{$else}
  ftruncate(outFile.Handle, outFile.seek(0,soFromCurrent));
{$endif}
  if forceFileAge<>0 then
    FileSetDate(outFile.Handle,forceFileAge);
  outFile.Free;
  // 5. if we worked on a .tmp file (recreated from a TZipReader) -> make it new
  if fDestFileName<>'' then begin
    if not DeleteFile(fDestFileName) then begin
      Sleep(100);
      if not DeleteFile(fDestFileName) then assert(false); end;
    RenameFile(fFileName,fDestFileName); // '.tmp' -> '.bjt' ou '.zip'
  end;
  // 6. free memory: Finalize(Entry)
  inherited;
end;

procedure TZipWriter.AddFile(const aFileName: TFileName; const aZipName: RawUTF8;
  CompressionLevel: integer; Algorithm: integer=0);
// direct compress or store of a file content, using memory mapped file
var Map: TMemoryMap;
begin
  if not Map.doMap(aFileName) then exit;
  if Map._size<64 then
    CompressionLevel := -1; // store if too small
  ZipCreate(aZipName,CompressionLevel,0,Algorithm); // initialize Zip object
  Entry[Count].Header.fileInfo.zlastMod :=
    {$ifdef Win32}FileGetDate(Map._file){$else}
    DateTimeToFileDateWindows(FileDateToDateTime(FileGetDate(Map._file)){$endif};
  Zip.WriteOnce(Map.buf^,Map._size);
  Map.UnMap;
  ZipClose;
end;

procedure TZipWriter.Add(const aZipName: RawUTF8;
  data: PAnsiChar; dataSize: cardinal; CompressionLevel: integer;
  dataCRC: pCardinal=nil; FileAge: integer = 0; Algorithm: integer=0);
begin
  if (self<>nil) and (aZipName<>'') then
  if (CompressionLevel<0) and (Algorithm=0) then
  with AddEntry(aZipName,FileAge)^.Header,FileInfo do begin
    zzipSize := dataSize;
    zfullSize := dataSize;
    if dataCRC<>nil then
      zcrc32 := dataCRC^ else
      zcrc32 := crc32(0,data,dataSize);
    outFile.Write(fileInfo,sizeof(fileInfo));
    outFile.Write(aZipName[1],length(aZipName));
    outFile.Write(data^,dataSize);
    inc(fCount);
  end else begin
    ZipCreate(aZipName,CompressionLevel,FileAge,Algorithm);
    Zip.WriteOnce(data^,dataSize);
    ZipClose; // fileInfo update+save, Zip.Free, inc(Count)
  end;
end;

procedure TZipWriter.Add(const aZipName: RawUTF8; p: PBlobData);
begin
  with AddEntry(aZipName)^.Header,FileInfo do begin
    if p^.AlgoID=0 then begin
      zzipMethod := p^.dataMethod;
      zzipSize := p^.dataSize;
      zfullSize := p^.dataFullSize;
      zcrc32 := p^.dataCRC;
    end else
      if Assigned(SynCompressionAlgos.Algo(p^.AlgoID)) then
        SetAlgoID(p^.AlgoID) else
        raise TZipException.CreateFmt(sZipAlgoIDNUnknownN,[p^.AlgoID,aZipName]);
    outFile.Write(fileInfo,sizeof(fileInfo));
    outFile.Write(aZipName[1],length(aZipName));
    outFile.Write(p^.databuf,p^.dataSize);
    inc(fCount);
  end;
end;

procedure TZipWriter.Add(aReader: TZipReader; aReaderIndex: integer);
// this copy directly from a TZipReader
var sign: integer;
    data: PAnsiChar;
    E: PZipEntry;
begin
  if (aReader=nil) or (aReaderIndex<0) or (aReaderIndex>=aReader.Count) then
    exit;
  if Count=length(Entry) then
    SetLength(Entry,Count+100);
  with Entry[Count] do begin
    E := @aReader.Entry[aReaderIndex];
    ZipName := E^.ZipName; // may be different after delete or new
    Header := E^.Header; // direct whole header copy
    with Header do begin // update Entry[Count]:
      data := @aReader.Map.buf[DataPosition]; // calc with old fileinfo.namelen 
      fileInfo.nameLen := length(ZipName); // recalc length, as may be updated
      localHeadOff  := outFile.Position; // only changed data = position in file
      sign := $04034b50; outFile.Write(sign,4);
      outFile.Write(fileInfo,sizeof(fileInfo)); // save new fileInfo
      outFile.Write(ZipName[1],fileInfo.nameLen);
      outFile.Write(data^,fileInfo.zzipSize); // direct data copy
    end;
  end;
  inc(fCount);
end;

procedure TZipWriter.ZipClose(aIndex: integer=-1);
// compression finish, fileInfo update+save, Zip.Free
// after ZipCreate: aIndex=-1 -> update Entry[Count] + inc(Count)
// in TZipWriter.Create(fromStream..): aIndex>=0 -> update Entry[aIndex] only
var p: cardinal;
    i: integer;
begin
  if Zip=nil then exit;
  Zip.Finish;
  if aIndex<0 then
    i := Count else
    i := aIndex;
  with Entry[i] do begin
    Header.fileInfo.zcrc32 := Zip.CRC;
    Header.fileInfo.zfullSize := Zip.SizeIn; // if algo -> SizeIn=SizeOut
    if Zip.FInitialized then
      Header.fileInfo.zzipSize := Zip.SizeOut else
      Header.fileInfo.zzipSize := Header.fileInfo.zfullSize;
    p := outFile.Seek(0,soFromCurrent);
    outFile.Seek(Header.localHeadOff+sizeof(dword),soFromBeginning);
    outFile.Write(Header.fileInfo,sizeof(Header.fileInfo)); // save updated fileInfo
    outFile.Seek(p,soFromBeginning);
  end;
  FreeAndNil(Zip);
  if aIndex<0 then
    inc(fCount);
end;

procedure TZipWriter.ZipCreate(const aZipName: RawUTF8;
  CompressionLevel: integer; FileAge: integer = 0; Algorithm: integer=0);
begin
  assert(Zip=nil);
  with AddEntry(aZipName,FileAge)^.Header do begin
    if Algorithm>0 then
      fileInfo.SetAlgoID(Algorithm) else
    if CompressionLevel>=0 then
      fileInfo.zzipMethod := 8;
    Zip := TZipCompressor.Create(outFile, CompressionLevel,Algorithm);
    outFile.Write(fileInfo,sizeof(fileInfo)); // save bulk fileInfo
    outFile.Write(aZipName[1],length(aZipName));
    // now the caller will use Zip.Write to compress data into outFile
    // and will end compression with ZipClose
  end;
end;

function TZipWriter.AddEntry(const aZipName: RawUTF8; FileAge: integer = 0): PZipEntry;
var sign: integer;
    tmp: WinAnsiString;
begin
  if Count=length(Entry) then
    SetLength(Entry,Count+100);
  result := @Entry[Count];
  with result^ do begin
    Header.Init; // signature, madeBy, extFileAttr, fileInfo.neededVersion init
{$ifdef Win32}
    if IsWinAnsiU(pointer(aZipName)) then begin
      // Win-Ansi code page -> encode as DOS/OEM charset (old format)
      tmp := Utf8ToWinAnsi(aZipName);
      SetLength(ZipName,length(tmp));
      CharToOemBuffA(pointer(tmp),pointer(ZipName),length(tmp));
    end else
{$endif} // Linux will use only UTF-8 encoding
    begin
      ZipName := aZipName;
      Header.fileInfo.SetUTF8FileName; // mark file name is UTF-8 encoded
    end;
    with Header do begin
      localHeadOff  := outFile.Position;
      with fileInfo do begin
        if FileAge=0 then
          zlastMod := fNow else
          zlastMod := FileAge;
        nameLen := length(ZipName);
      end;
    end;
  end;
  sign := $04034b50; outFile.Write(sign,sizeof(dword));
end;

function TZipWriter.LastCRC32Added: cardinal;
begin
  if Count>0 then
    result := Entry[Count-1].Header.fileInfo.zcrc32 else
    result := 0;
end;


{ TZipValues }

procedure TZipValues.BeginWriter;
var i: integer;
begin
  if Writer<>nil then exit;
  Modified := true;
  Count := 0;
  for i := 0 to Reader.Count-1 do // update Values[] with MarkDeleted
    if Reader.Entry[i].Header.signature<>0 then begin
      CopyValue(i,Count);
      inc(Count);
    end;
  inherited BeginWriter; // Writer := TZipWriter.Create(Reader) = calc MarkDeleted
  assert(Writer.Count=Count);
end;

constructor TZipValues.Create(const aFileName: TFileName);
var n: integer;
begin
  inherited Create(aFileName); // Reader := TZipReader.Create
  n := Reader.Count-1; // '-index-' must be last Entry[n] -> otherwise gap in Values[]
  if n=0 then
    Reader.Clear; // must contains at least: Values[0] + '-index-'
  if n<1 then exit;
  with Reader.Entry[n] do // read Values[] from last Entry[]:
  if (ZipName='-index-') and (Header.fileInfo.zzipMethod=0) then begin
    Count := n;
    LoadValues(@Reader.Map.buf^[Header.DataPosition]);
    Reader.DeleteLastEntry; // ignore '-index-' from now
  end else begin
    Count := 0;
    Assert(false,'wrong file format for '+FileName);
  end;
end;

destructor TZipValues.Destroy;
begin
  if Modified and (Count>0) then begin
    BeginWriter; // will truncate to the last block
    with Writer do begin
      ZipCreate('-index-',-1);
      SaveValues(Zip);
      ZipClose;
    end;
  end;
  inherited;
end;

function TZipValues.GetValue(aReaderIndex: integer; aStream: TStream): PAnsiChar;
begin
  if Writer<>nil then
    result := nil else
    result := Reader.GetData(aReaderIndex,aStream);
end;

function TZipValues.MarkDeleted(aReaderIndex: integer): boolean;
begin
  if aReaderIndex<0 then begin
    result := false;
    exit;
  end;
  Modified := true;
  result := inherited MarkDeleted(aReaderIndex);
end;

{ TBlobData }

procedure CompressAsBlobData(const data; size: integer; aStream: TStream;
  CompressionLevel: integer=6; Algorithm: integer=0);
// create a TBlobData in aStream (encryption algo: 6=AES 7=AES+Zip 8=AES+SynLz)
begin
  with TZipCompressor.CreateAsBlobData(aStream,CompressionLevel,Algorithm) do
  try
    Write(data,size);
    Finish;
  finally
    Free;
  end;
end;

{ use algo 6=AES 7=AES+Zip-chunked 8=AES+SynLz-chunked
function CompressAsBlobData(data: PAnsiChar; size: integer;
  AESKey: pointer=nil; AESKeySize: integer=0): string;
// optional AES-encrypt AFTER compression -> 0% ZIP compatible but security safe
begin
  SetLength(result,sizeof(TBlobData)+(size*11)div 10+12);
  with PBlobData(pointer(result))^ do begin
    dataFullSize := size;
    dataCRC := crc32(0,data,size);
    dataSize := CompressMem(data,@dataBuf,size,length(result)-sizeof(TBlobData));
    if dataSize>=dataFullSize then begin // compress only if efficient
      dataMethod := 0; // store
      dataSize := dataFullSize;
      if (AESKey<>nil) and (AESKeySize>0) then
        AES(AESKey^,AESKeySize,data,@databuf,dataSize,true) else
        move(data^,databuf,dataSize);
    end else begin
      dataMethod := 8;
      if (AESKey<>nil) and (AESKeySize>0) then
        AES(AESKey^,AESKeySize,@databuf,@databuf,dataSize,true);
    end;
    SetLength(result,dataSize+BLOBDATA_HEADSIZE);
  end;
end;}

function TBlobData.AlgoCreate(data: pointer): TSynCompressionAlgo;
// test if algo is registered, perform crc32 check and create one instance
var Algo: TSynCompressionAlgoClass;
begin
  if DataMethod<15 then
    result := nil else begin
    Algo := SynCompressionAlgos.Algo(AlgoID); // registered?
    if not Assigned(Algo) then // error: unregistered algo
      raise TZipException.CreateFmt(sZipAlgoIDNUnknownN,[
        AlgoID,'TBlobData']);
    if crc32(0,data,dataFullSize)<>dataCRC then // always check integrity
      raise TZipException.CreateFmt(sZipCrcErrorNN,[IntToStr(AlgoID),'TBlobData']);
    result := Algo.Create; // create algo instance
  end;
end;

function TBlobData.AlgoID: cardinal;
begin
  // 0=stored 8=inflate >=16: AlgoID=dataMethod shr 4
  result := (dataMethod shr 4) and 15;
end;

function TBlobData.Expand: RawByteString;
begin
  case DataMethod of // 0=stored 8=inflate >16: AlgoID=dataMethod shr 4
  16..31:
    with AlgoCreate(@dataBuf) do // crc32+algo object create
    try
      SetString(result,nil,UnCompressedLength(@dataBuf,dataFullSize));
      UnCompress(@dataBuf,dataFullSize,pointer(result));
    finally
      Free;
    end;
  8: begin
    SetString(result,nil,dataFullSize);
    if (UnCompressMem(
      @dataBuf,pointer(result),dataSize,dataFullSize)<>integer(dataFullSize)) or
      (crc32(0,pointer(result),dataFullSize)<>dataCRC) then begin
      assert(false);
      result := '';
    end;
  end;
  0: if dataSize=0 then
      result := '' else
      SetString(result,PAnsiChar(@dataBuf),dataSize);
  else begin
    assert(false); // impossible dataMethod -> probably bad PBlobData
    result := '';
  end;
  end;
end;

function TBlobData.ExpandBuf(out destSize: cardinal): pointer;
// uncompress and alloc memory if necessary (i.e. DataMethod<>0);
// no direct AES since may be mapped and DataMethod=0
begin
  case DataMethod of // 0=stored 8=inflate >16: AlgoID=dataMethod shr 4
  16..31:
    with AlgoCreate(@dataBuf) do // crc32+algo object create
    try
      destsize := UnCompressedLength(@dataBuf,dataFullSize);
      Getmem(result,destSize);
      UnCompress(@dataBuf,dataFullSize,result);
    finally
      Free;
    end;
  0: result := @dataBuf;
  8: begin
    GetMem(result,dataFullSize);
    if (UnCompressMem(@dataBuf,result,dataSize,dataFullSize)<>integer(dataFullSize)) or
      (crc32(0,result,dataFullSize)<>dataCRC) then begin
      Freemem(result);
      assert(false);
      result := nil;
    end;
  end;
  else begin
    assert(false); // impossible dataMethod -> probably bad PBlobData
    result := nil;
  end;
  end;
end;

procedure TBlobData.ExpandStream(Stream: TStream);
var tmp: RawByteString;
begin
  case DataMethod of
  16..31: begin
    tmp := Expand;
    Stream.Write(pointer(tmp)^,length(tmp));
  end;
  0: Stream.Write(dataBuf,dataFullsize);
  8: UnCompressStream(@dataBuf,dataSize,Stream,nil);
  else assert(false);
  end;
end;

function TBlobData.Next: PAnsiChar;
asm
  lea ecx,eax+TBlobData.databuf
  mov eax,[eax].TBlobData.datasize
  add eax,ecx
end;

procedure TBlobData.SetFrom(const FileInfo: TFileInfo);
begin
  dataSize := FileInfo.zzipsize;
  dataFullSize := FileInfo.zfullSize;
  dataCRC := FileInfo.zcrc32;
  // dataMethod: 0=stored 8=inflate >16: AlgoID=dataMethod shr 4
  if FileInfo.AlgoID<>0 then
    dataMethod := FileInfo.AlgoID shl 4 else // stored + AlgoID
    dataMethod := FileInfo.zzipMethod;
end;


{ TZipEntry }

function TZipEntry.AlgoCreate(data: pointer; const FileName: TFileName): TSynCompressionAlgo;
// test if algo is registered, perform crc32 check and create one instance
var Algo: TSynCompressionAlgoClass;
begin
  if Header.fileInfo.AlgoID=0 then
    result := nil else begin
    Algo := SynCompressionAlgos.Algo(Header.fileInfo.AlgoID); // registered?
    if not Assigned(Algo) then // error: unregistered algo
      raise TZipException.CreateFmt(sZipAlgoIDNUnknownN,[
        Header.fileInfo.AlgoID,ZipName]);
    if crc32(0,data,Header.fileInfo.zfullSize)<>
      Header.fileInfo.zcrc32 then // always check integrity
      raise TZipException.CreateFmt(sZipCrcErrorNN,[ZipName,FileName]);
    result := Algo.Create; // create algo instance
  end;
end;

function TZipEntry.SameAs(const aEntry: TZipEntry): boolean;
begin
  result := (ZipName=aEntry.ZipName) and
     Header.fileInfo.SameAs(@aEntry.Header.fileInfo);
end;


procedure GetDate(out Y,M,D: cardinal);
{$IFDEF MSWINDOWS}
var SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  with SystemTime do begin
    Y := wYear;
    M := wMonth;
    D := wDay;
  end;
{$ELSE} // LINUX:
var T: TTime_T;
    UT: TUnixTime;
begin
  __time(@T);
  localtime_r(@T, UT);
  Y := UT.tm_year + 1900;
  M := UT.tm_mon + 1;
  D := UT.tm_mday;
{$ENDIF}
end;


{ TSynCompressionAlgos }

function TSynCompressionAlgos.Algo(aID: integer): TSynCompressionAlgoClass;
var i: integer;
begin
  for i := 0 to length(Values)-1 do
  with Values[i] do
  if ID=aID then begin
      result := func;
      exit;
    end;
  result := nil;
end;

procedure TSynCompressionAlgos.AlgoRegister(aAlgo: TSynCompressionAlgoClass;
  aID, aWholeID: integer);
var L: integer;
begin
  if not Assigned(aAlgo) then
    exit;
  aID := aID and 15;
  if (aID=0) or Assigned(Algo(aID)) then exit;
  L := length(Values);
  SetLength(Values,L+1);
  with Values[L] do begin
    ID := aID;
    WholeID := aWholeID;
    func := aAlgo;
  end;
end;

function TSynCompressionAlgos.WholeAlgoID(aID: integer): integer;
var i: integer;
begin
  for i := 0 to length(Values)-1 do
  with Values[i] do
  if ID=aID then begin
      result := WholeID;
      exit;
    end;
  result := 0;
end;


{ TSynCompressionAlgo }

function TSynCompressionAlgo.CompressFinish: cardinal;
begin
  result := 0; // default implementation: just do nothing
end;

procedure TSynCompressionAlgo.CompressInit(OutStream: TStream);
begin
  fDestStream := OutStream;
end;


{ TSynCompressionAlgoBuf }

function TSynCompressionAlgoBuf.Compress(InP: pointer; InLen: cardinal;
  CRC: PCardinal): cardinal;
begin
  if InLen=0 then begin
    result := 0;
    exit;
  end;
  assert(InLen<=65536); // // fCompressBuf[] size if fixed
  result := AlgoCompress(InP,InLen,fCompressBuf+3); // compress InP[InLen]
  if result+128>InLen then begin // compression was not effective -> store
    pWord(fCompressBuf)^ := 0; // fCompressBuf[0..2] = 0 = no compression
    pInteger(@fCompressBuf[2])^ := (InLen-1)shl 8; // [3..4]=uncompressed len-1
    move(InP^,fCompressBuf[5],InLen); // store after uncompressed len
    result := InLen+5;
  end else begin // compression was effective -> store compressed chunk len
    pWord(fCompressBuf)^ := result; // fCompressBuf[0..2] = chunk len
    fCompressBuf[2] := AnsiChar(result shr 16);
    inc(result,3);
  end;
  fDestStream.Write(fCompressBuf^,result);
  if CRC<>nil then
    CRC^ := crc32(CRC^,fCompressBuf,result);
end;

procedure TSynCompressionAlgoBuf.CompressInit(OutStream: TStream);
begin
  inherited; // fDestSteram := OutStream
  // size = worse case with 64KB chunk
  Getmem(fCompressBuf,AlgoCompressLength(65536)); // = 73744 for SynLZ, e.g.
end;

destructor TSynCompressionAlgoBuf.Destroy;
begin
  Freemem(fCompressBuf);
  inherited;
end;

function TSynCompressionAlgoBuf.UnCompress(InP: pointer; InLen: cardinal;
  OutP: pointer): cardinal;
var sP,sEnd, dP: PAnsiChar;
    L: integer;
begin
  sP := InP;
  sEnd := sP+InLen;
  dP := OutP;
  while sP<sEnd do begin  // -> uncompress InP[InLen] into PAnsiChar(OutStream)
    L := PInteger(sP)^ and $ffffff;
    if L=0 then begin // no compression
      inc(sP,3);
      L := pWord(sP)^+1;
      inc(sP,2);
      move(sP^,dP^,L);
      inc(sP,L);
      inc(dp,L);
    end else begin // SynLZ compression
      inc(dP,AlgoUnCompress(sP+3,L,dP));
      inc(sP,L+3);
    end;
  end;
  result := dp-PAnsiChar(OutP);
end;

function TSynCompressionAlgoBuf.UnCompressedLength(InP: pointer;
  InLen: cardinal): cardinal;
var sP,sEnd: PAnsiChar;
    L: integer;
begin
  sP := InP;
  sEnd := sP+InLen;
  result := 0;
  while sP<sEnd do begin // return uncompressed len
    L := PInteger(sP)^ and $ffffff;
    if L=0 then begin // no compression
      L := pWord(sP+3)^+1;
      inc(result,L);
      inc(sP,L+5);
    end else begin
      inc(result,AlgoUnCompressLength(sP+3,InLen)); // very fast length calc
      inc(sP,L+3);
    end;
  end
end;

{ TSynCompressionAlgoWhole }

function TSynCompressionAlgoWhole.Compress(InP: pointer; InLen: cardinal;
  CRC: PCardinal): cardinal;
var tmp: PAnsiChar;
begin
  getmem(tmp,AlgoCompressLength(InLen));
  try
    result := AlgoCompress(InP,InLen,tmp+1);
    if result+128>InLen then begin // compression not effective
      tmp[0] := #0; // mark stored
      move(InP^,tmp[1],InLen);
      result := InLen;
    end else // compression was effective
      tmp[0] := #1; // mark compressed
    inc(result);
    fDestStream.Write(tmp^,result);
    if CRC<>nil then
      CRC^ := crc32(CRC^,tmp,result);
  finally
    freemem(tmp);
  end;
end;

function TSynCompressionAlgoWhole.UnCompress(InP: pointer; InLen: cardinal;
  OutP: pointer): cardinal;
var tmp: PAnsiChar absolute InP;
begin
  case tmp[0] of
  #0: begin
    result := InLen-1;
    move(tmp[1],OutP^,result);
  end;
  #1:  result := AlgoUnCompress(tmp+1,InLen-1,OutP);
  else result := 0;
  end;
end;

function TSynCompressionAlgoWhole.UnCompressedLength(InP: pointer;
  InLen: cardinal): cardinal;
var tmp: PAnsiChar absolute InP;
begin
  case tmp[0] of
  #0:  result := InLen-1;
  #1:  result := AlgoUnCompressLength(tmp+1,InLen-1);
  else result := 0;
  end;
end;


{ TMemoryMap }

function TMemoryMap.DoMap(const aFileName: TFileName): boolean;
begin
  _file := FileOpen(aFileName,fmOpenRead or fmShareDenyWrite);
  buf := nil;
  result := _file>=0;
  if not result then begin
    _size := 0;
    exit;
  end;
  _size := FileSeek(_file,0,2); // from end -> return File Size
  if _size=0 then begin
    FileClose(_file);
    _file := 0;
  end else begin
    FileSeek(_file,0,0); // from beginning
    _map := CreateFileMapping(_file, nil, PAGE_READONLY, 0, 0, nil);
    if _map<>0 then
      buf := MapViewOfFile(_map, FILE_MAP_READ, 0, 0, 0) else begin
      result := false;
      _size := 0;
    end;
  end;
end;

procedure TMemoryMap.UnMap;
begin
  if buf<>nil then begin
    UnmapViewOfFile(buf);
    CloseHandle(_map);
    buf := nil; // mark unmapped
  end;
  if _file<>0 then begin
    FileClose(_file);
    _file := 0;
  end;
end;

initialization
  fillchar(BlobDataNull,sizeof(TBlobData),0);

end.

