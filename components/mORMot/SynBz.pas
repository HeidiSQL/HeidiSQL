/// fast BZ2 compression/decompression
// - licensed under a MPL/GPL/LGPL tri-license; version 1.6
unit SynBz;

{
    This file is part of Synopse BZ2 Compression.

    Synopse Synopse BZ2 Compression. Copyright (C) 2012 Arnaud Bouchez
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

  The Original Code is Synopse BZ2 Compression.

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


  Cross-platform BZLib implementation
  ===================================

   decompression:
      - can use inline assembler version (bunzipasm.inc)
      - more than 2 times faster than standard C version
      - no external .obj necessary
      - 100% API compatible
      - does CRC Bz2Check
      - fully tested under Delphi, should work with (Cross)Kylix
    asm translation extracted from isbunzip.dll and optimized for Delphi 7

    compression:
      - use fast bzip.dll (faster than standard C version)

    Cross-Platform:
      - under Win32, use fast bzip.dll (provided in bzasm.zip) + assembler
      - under Linux, use default libbz2.so
       (Linux can use faster assembler version - must be checked if this 
        is really usefull)

  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
  License for the specific language governing rights and limitations
  under the License.

  The Initial Developer of the Original Code is Arnaud Bouchez.
  This work is Copyright (C) 2008 Arnaud Bouchez - http://bouchez.info

  All Rights Reserved.

}


interface

{$define USEBZAASM}
// use ASM version of bunzip.dll (ok as far as tested)

uses
  SysUtils, Types, Classes,
{$ifdef WIN32}
  Windows;
{$else}
  LibC;
{$endif}

type
  TBZAlloc = function(AppData: Pointer; Items, Size: Cardinal): Pointer; stdcall;
  TBZFree = procedure(AppData, Block: Pointer); stdcall;
  TBZStreamRec = object
    next_in: PAnsiChar;
    avail_in: Integer;
    total_in: Int64;
    next_out: PAnsiChar;
    avail_out: Integer;
    total_out: Int64;
    State: Pointer;
    zalloc: TBZAlloc;
    zfree: TBZFree;
    AppData: Pointer;
    procedure Init;
  end;

  TBZBuffer = array[word] of byte;
  TBZCompressor = class(TStream)
  private
    FInitialized: Boolean;
    FStrm: TBZStreamRec;
    FDestStream: TStream;
    SrcLen, DestLen: Cardinal;
    FBufferIn: TBZBuffer;
    BufferOut: ^TBZBuffer; // via Getmem() for AES 16-bytes alignment
    procedure FlushBufferOut;
  protected
    // override for crc or encryption, e.g.
    procedure WriteDest(const Buffer; Count: Longint); virtual;
  public
    constructor Create(outStream: TStream; CompressionLevel: Integer=6);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure Finish;
    property FullSize: cardinal read SrcLen;
    property CompressedSize: cardinal read DestLen;
  end;

  TBZDecompressor = class(TStream)
  private
    FReachedEnd,
    FInitialized: Boolean;
    FStrm: TBZStreamRec;
    FSrcStream: TStream;
    DestLen: Cardinal;
    FBufferIn: array[word] of byte; // one 64kb buffers
  public
    constructor Create(inStream: TStream);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property FullSize: cardinal read DestLen;
  end;


/// result=nil if error decompressing
function UnCompressBzDll(Source: PAnsiChar;
  SourceSize, DestSize: integer): TMemoryStream; overload;

/// Dest must already be allocated with enough place (DestSize); return true if ok
function UnCompressBzDll(Source: PAnsiChar; SourceSize: integer;
  Dest: PAnsiChar; Destsize: integer): boolean; overload;

// raw BZ2 functions:

{$ifdef USEBZAASM}
function BZ2_bzDecompressInit(var strm: TBZStreamRec;
    verbosity, small: Integer): Integer; cdecl;
function BZ2_bzDecompress(var strm: TBZStreamRec): Integer; cdecl;
function BZ2_bzDecompressEnd(var strm: TBZStreamRec): Integer; cdecl;
{$endif}

{$ifdef Win32}
function BZCompressorInit: boolean;
{$ifdef USEBZAASM}
const BZDeCompressorInit = true;
{$else}
function BZDeCompressorInit: boolean;
{$endif}
{$endif}

var
{$ifdef Win32}
  BZ2_bzCompressInit: function(var strm: TBZStreamRec;
    blockSize100k, verbosity, workFactor: Integer): Integer; stdcall = nil;
  BZ2_bzCompress: function(var strm: TBZStreamRec;
    action: Integer): Integer; stdcall = nil;
  BZ2_bzCompressEnd: function(var strm: TBZStreamRec): Integer; stdcall = nil;

{$ifndef USEBZAASM}
  BZ2_bzDecompressInit: function(var strm: TBZStreamRec;
    verbosity, small: Integer): Integer; stdcall = nil;
  BZ2_bzDecompress: function(var strm: TBZStreamRec): Integer; stdcall = nil;
  BZ2_bzDecompressEnd: function(var strm: TBZStreamRec): Integer; stdcall = nil;
{$endif}

{$else}
const
  bzlib='libbz2.so';

function BZ2_bzCompressInit(var strm: TBZStreamRec;
    blockSize100k, verbosity, workFactor: Integer): Integer; cdecl;
    external bzlib name 'BZ2_bzCompressInit';
function BZ2_bzCompress(var strm: TBZStreamRec;
    action: Integer): Integer; cdecl;
    external bzlib name 'BZ2_bzCompress';
function BZ2_bzCompressEnd(var strm: TBZStreamRec): Integer; cdecl;
    external bzlib name 'BZ2_bzCompressEnd';

{$ifndef USEBZAASM}
function BZ2_bzDecompressInit(var strm: TBZStreamRec;
    verbosity, small: Integer): Integer; cdecl;
    external bzlib name 'BZ2_bzDecompressInit';
function BZ2_bzDecompress(var strm: TBZStreamRec): Integer; cdecl;
    external bzlib name 'BZ2_bzDecompress';
function BZ2_bzDecompressEnd(var strm: TBZStreamRec): Integer; cdecl;
    external bzlib name 'BZ2_bzDecompressEnd';
{$endif}
{$endif}

const
  BZ_RUN              = 0;
  BZ_FLUSH            = 1;
  BZ_FINISH           = 2;

  BZ_OK               = 0;
  BZ_RUN_OK           = 1;
  BZ_FLUSH_OK         = 2;
  BZ_FINISH_OK        = 3;
  BZ_STREAM_END       = 4;
  BZ_SEQUENCE_ERROR   = (-1);
  BZ_PARAM_ERROR      = (-2);
  BZ_MEM_ERROR        = (-3);
  BZ_DATA_ERROR       = (-4);
  BZ_DATA_ERROR_MAGIC = (-5);
  BZ_IO_ERROR         = (-6);
  BZ_UNEXPECTED_EOF   = (-7);
  BZ_OUTBUFF_FULL     = (-8);
  BZ_CONFIG_ERROR     = (-9);

function Bz2Check(const Code: Integer; const ValidCodes: array of Integer): integer;


implementation

const
  SBzlibDataError = 'bzlib: Compressed data is corrupted';
  SBzlibInternalErrorN = 'bzlib: Internal error. Code %d';

{$ifdef USEBZAASM}
{$I bunzipasm.inc} // all asm code extracted from bunzip.dll :)
{$endif}

{$ifdef Win32}
var BZCompressModule:   HMODULE = 0;
    BZDecompressModule: HMODULE = 0;

function GetLastErrorText(aerror: integer): string;
begin
 setlength(result,1024);
 setlength(result,FormatMessage(format_message_from_system,
   nil,aerror,0,PChar(result),1024,nil));
end;

procedure ShowLastError;
begin
  MessageBox(0,PAnsiChar(GetLastErrorText(getLastError)),nil,MB_OK or MB_ICONERROR);
end;

function BZInitCompressFunctions(Module: HMODULE): Boolean;
begin
  BZCompressModule := Module;
  if Module=0 then begin
    ShowLastError;
    Result := false;
  end else begin
    BZ2_bzCompressInit := GetProcAddress(Module, 'BZ2_bzCompressInit');
    BZ2_bzCompress := GetProcAddress(Module, 'BZ2_bzCompress');
    BZ2_bzCompressEnd := GetProcAddress(Module, 'BZ2_bzCompressEnd');
    Result := Assigned(BZ2_bzCompressInit) and Assigned(BZ2_bzCompress) and
      Assigned(BZ2_bzCompressEnd);
  end;
  if not Result then begin
    BZ2_bzCompressInit := nil;
    BZ2_bzCompress := nil;
    BZ2_bzCompressEnd := nil;
  end;
end;

{$ifndef USEBZAASM}
function BZInitDecompressFunctions: Boolean;
var F: string;
begin
  result := false;
  F := extractfilepath(paramstr(0))+'bunzip.dll';
  if FileExists(F) then
     BZDeCompressModule:= LoadLibrary(PChar(F)) else
     exit;
  if BZDeCompressModule=0 then
    ShowLastError else begin
    BZ2_bzDecompressInit := GetProcAddress(BZDeCompressModule, 'BZ2_bzDecompressInit');
    BZ2_bzDecompress := GetProcAddress(BZDeCompressModule, 'BZ2_bzDecompress');
    BZ2_bzDecompressEnd := GetProcAddress(BZDeCompressModule, 'BZ2_bzDecompressEnd');
    Result := Assigned(BZ2_bzDecompressInit) and Assigned(BZ2_bzDecompress) and
      Assigned(BZ2_bzDecompressEnd);
  end;
  if not Result then begin
    BZ2_bzDecompressInit := nil;
    BZ2_bzDecompress := nil;
    BZ2_bzDecompressEnd := nil;
  end;
end;
{$endif}
{$endif}

function BZAllocMem(AppData: Pointer; Items, Size: Cardinal): Pointer; stdcall;
begin
  GetMem(Result, Items * Size);
end;

procedure BZFreeMem(AppData, Block: Pointer); stdcall;
begin
  FreeMem(Block);
end;

function Bz2Check(const Code: Integer; const ValidCodes: array of Integer): integer;
var I: Integer;
begin
  if Code = BZ_MEM_ERROR then
    OutOfMemoryError;
  Result := Code;
  for I := Low(ValidCodes) to High(ValidCodes) do
    if ValidCodes[I] = Code then
      Exit;
  raise Exception.CreateFmt(SBzlibInternalErrorN, [Code]);
end;

{$ifdef Win32}
function BZCompressorInit: boolean;
var F: string;
begin
  result := true;
  if @BZ2_bzCompressInit=nil then begin
    F := extractfilepath(paramstr(0))+'bzip.dll';
    if FileExists(F) then
      result := BZInitCompressFunctions(LoadLibrary(PChar(F))) else
      result := false;
  end;
end;

{$ifndef USEBZAASM}
function BZDeCompressorInit: boolean;
begin
  if (@BZ2_bzDecompressInit=nil) then
    result := BZInitDecompressFunctions else
    result := true;
end;
{$endif}
{$endif}


{ TBZCompressor }

constructor TBZCompressor.Create(outStream: TStream; CompressionLevel: Integer);
begin
{$ifdef Win32}
  BZCompressorInit;
  if @BZ2_bzCompressInit=nil then
    CompressionLevel := -1; // <0 -> direct copy (no compression)
{$endif}
  FDestStream := outStream;
  FStrm.Init;
  New(BufferOut);
  with FStrm do begin
    next_out := PAnsiChar(BufferOut);
    avail_out := SizeOf(BufferOut^);
    next_in := @FBufferIn;
  end;
  if CompressionLevel>=0 then // FInitialized=false -> direct copy
    FInitialized := Bz2Check(BZ2_bzCompressInit(FStrm, CompressionLevel, 0, 0),
      [BZ_OK])=BZ_OK;
end;

destructor TBZCompressor.Destroy;
begin
  if FInitialized then begin
    FStrm.next_out := nil;
    FStrm.avail_out := 0;
    BZ2_bzCompressEnd(FStrm);
    FreeMem(BufferOut);
  end;
  inherited;
end;

procedure TBZCompressor.Finish;
begin
  if self=nil then
    exit;
  if FInitialized then begin
    while FStrm.avail_in > 0 do begin
      Bz2Check(BZ2_bzCompress(FStrm, BZ_RUN), [BZ_RUN_OK]);
      if FStrm.avail_out = 0 then FlushBufferOut;
    end;
    FStrm.next_in := nil;
    FStrm.avail_in := 0;
    while Bz2Check(BZ2_bzCompress(FStrm, BZ_FINISH), [BZ_FINISH_OK, BZ_STREAM_END])<>BZ_STREAM_END do
      FlushBufferOut;
  end;
  FlushBufferOut;
end;

procedure TBZCompressor.FlushBufferOut;
var Count: integer;
begin
//  if not FInitialized then exit;
  Count := SizeOf(BufferOut^) - FStrm.avail_out;
  if Count=0 then exit;
  WriteDest(BufferOut^, Count);
  FStrm.next_out := PAnsiChar(BufferOut);
  FStrm.avail_out := SizeOf(BufferOut^);
end;

function TBZCompressor.Read(var Buffer; Count: Integer): Longint;
begin
  assert(false);
  result := 0;
end;

function TBZCompressor.Seek(Offset: Integer; Origin: Word): Longint;
begin
  if not FInitialized then // CompressionLevel<0: direct copy to
    result := 0 else
  if (Offset = 0) and (Origin = soFromCurrent) then // for TStream.Position
    Result := FStrm.total_out else begin
    Result := 0;
    assert((Offset = 0) and (Origin = soFromBeginning) and (FStrm.total_out = 0));
  end;
end;

function TBZCompressor.Write(const Buffer; Count: Integer): Longint;
var i: integer;
    p: PAnsiChar;
begin
  if (self<>nil) and (Count>0) then begin
    result := Count;
    inc(SrcLen,Count);
    if FInitialized then begin
      if Count+FStrm.avail_in>sizeof(fBufferIn)-1 then begin
        while FStrm.avail_in > 0 do begin
          Bz2Check(BZ2_bzCompress(FStrm, BZ_RUN), [BZ_RUN_OK]);
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
          Bz2Check(BZ2_bzCompress(FStrm, BZ_RUN), [BZ_RUN_OK]);
          if FStrm.avail_out = 0 then
            FlushBufferOut;
        end;
        FStrm.avail_in := 0;
        FStrm.next_in := @fBufferIn;
      end;
    end else begin // if not FIinitialized: CompressionLevel<0: direct copy to
      p := @Buffer;
      while Count>=FStrm.avail_out do begin
        FlushBufferOut;
        if Count<sizeof(BufferOut^) then
          i := Count else
          i := sizeof(BufferOut^);
        move(p^,BufferOut^,i);
        inc(p,i);
        dec(Count,i);
        dec(FStrm.avail_out,i);
      end;
      if Count>0 then begin
        move(p^,BufferOut^[sizeof(BufferOut^)-FStrm.avail_out],Count);
        dec(FStrm.avail_out,Count);
      end;
    end;
  end else
    result := 0; // self=nil
end;

procedure TBZCompressor.WriteDest(const Buffer; Count: Integer);
begin
  inc(DestLen,Count);
  FDestStream.Write(Buffer, Count);
end;

{ TBZDecompressor }

constructor TBZDecompressor.Create(inStream: TStream);
begin
  FSrcStream := inStream;
{$ifndef USEBZAASM}
  if (@BZ2_bzDecompressInit=nil) and not BZInitDecompressFunctions then exit;
{$endif}
  FStrm.Init;
  FStrm.next_in := @FBufferIn;
  FInitialized := Bz2Check(BZ2_bzDecompressInit(FStrm, 0, 0), [BZ_OK])=BZ_OK;
end;

destructor TBZDecompressor.Destroy;
begin
  if FInitialized then begin
    FStrm.next_out := nil;
    FStrm.avail_out := 0;
    BZ2_bzDecompressEnd(FStrm);
  end;
  inherited;
end;

function TBZDecompressor.Read(var Buffer; Count: Integer): Longint;
begin
  if not FInitialized then begin // no decompression: direct copy from source
    Result := FSrcStream.Read(Buffer,Count);
    inc(DestLen,Result);
    exit;
  end;
  Result := 0;
  if FReachedEnd then { unexpected EOF }
    exit;
  FStrm.next_out := @Buffer;
  FStrm.avail_out := Count;
  while FStrm.avail_out > 0 do begin
    if FStrm.avail_in = 0 then begin
      FStrm.next_in := @FBufferIn;
      FStrm.avail_in := FSrcStream.Read(FBufferIn, SizeOf(FBufferIn));
      { Unlike zlib, bzlib does not return an error when avail_in is zero and
        it still needs input. To avoid an infinite loop, check for this. }
      if FStrm.avail_in=0 then
        exit; // leave result=0 to mark error
    end;
    case Bz2Check(BZ2_bzDecompress(FStrm),                        
      [BZ_OK, BZ_STREAM_END, BZ_DATA_ERROR, BZ_DATA_ERROR_MAGIC]) of
      BZ_STREAM_END: begin
        FReachedEnd := True;
        break;
      end;
      BZ_DATA_ERROR, BZ_DATA_ERROR_MAGIC:
        raise Exception.Create(SBzlibDataError);
    end;
  end;
  Result := Count-FStrm.avail_out;
  inc(DestLen,Result);
end;

function TBZDecompressor.Seek(Offset: Integer; Origin: Word): Longint;
begin
  if not FInitialized then // CompressionLevel<0: direct copy to
    result := DestLen else
    result := FStrm.total_out;
  if (Offset<>0) or (Origin<>soFromCurrent) then begin // for TStream.Position
    assert((Offset = 0) and (Origin = soFromBeginning) and (result = 0));
    Result := 0;
  end;
end;

function TBZDecompressor.Write(const Buffer; Count: Integer): Longint;
begin
  assert(false);
  result := 0;
end;

function UnCompressBzDll(Source: PAnsiChar; SourceSize, DestSize: integer): TMemoryStream; overload;
// result=nil if error decompressing
var strm: TBZStreamRec;
    res: integer;
begin
  result := nil;
{$ifndef USEBZAASM}
  if (@BZ2_bzDecompressInit=nil) and not BZInitDecompressFunctions then exit;
{$endif}
  strm.Init;
  if BZ2_bzDecompressInit(strm, 0,0)<>BZ_OK then
    exit;
  strm.next_in := Source;
  strm.avail_in := SourceSize;
  result := TMemoryStream.Create;
  result.Size := DestSize;
  strm.next_out := result.Memory;
  strm.avail_out := DestSize;
{$ifdef USEBZAASM}
  try
    res := BZ2_bzDecompress(strm);
  except
    on E: Exception do
      res := BZ_DATA_ERROR;
  end;
{$else}
  res := BZ2_bzDecompress(strm);
{$endif}
  if res<>BZ_STREAM_END then
    FreeAndNil(result);
  BZ2_bzDecompressEnd(strm);
end;

function UnCompressBzDll(Source: PAnsiChar; SourceSize: integer;
  Dest: PAnsiChar; Destsize: integer): boolean; overload;
// Dest must already be allocated with DestSize = enough place
var strm: TBZStreamRec;
    res: integer;
begin
  result := false;
{$ifndef USEBZAASM}
  if (@BZ2_bzDecompressInit=nil) and not BZInitDecompressFunctions then exit;
{$endif}
  strm.Init;
  if BZ2_bzDecompressInit(strm, 0,0)<>BZ_OK then
    exit;
  strm.next_in := Source;
  strm.avail_in := SourceSize;
  strm.next_out := Dest;
  strm.avail_out := DestSize;
{$ifdef USEBZAASM}
  try
    res := BZ2_bzDecompress(strm);
  except
    on E: Exception do
      res := BZ_DATA_ERROR;
  end;
{$else}
  res := BZ2_bzDecompress(strm);
{$endif}
  result := (res=BZ_STREAM_END);
  BZ2_bzDecompressEnd(strm);
end;

{ TBZStreamRec }

procedure TBZStreamRec.Init;
begin
  FillChar(self, SizeOf(self), 0);
{$ifdef Win32}  // use LibC Alloc/Free on Linux
  zalloc := BZAllocMem;
  zfree := BZFreeMem;
{$endif}
end;

{$ifdef Win32}
initialization
finalization
  if BZCompressModule<>0 then
    FreeLibrary(BZCompressModule);
  if BZDeCompressModule<>0 then
    FreeLibrary(BZDeCompressModule);
{$endif}
end.

