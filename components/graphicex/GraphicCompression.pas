unit GraphicCompression;

// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The original code is GraphicColor.pas, released November 1, 1999.
//
// The initial developer of the original code is Dipl. Ing. Mike Lischke (Pleißa, Germany, www.delphi-gems.com),
//
// Portions created by Dipl. Ing. Mike Lischke are Copyright
// (C) 1999-2003 Dipl. Ing. Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
// This file is part of the image library GraphicEx.
//
// GraphicCompression contains various encoder/decoder classes used to handle compressed
// data in the various image classes.
//
// Currently supported methods are:
// - LZW (Lempel-Ziff-Welch)
//   + TIF
//   + GIF
// - RLE (run length encoding)
//   + TGA,
//   + PCX,
//   + packbits
//   + SGI
//   + CUT
//   + RLA
//   + PSP
// - CCITT
//   + raw G3 (fax T.4)
//   + modified G3 (CCITT RLE)
//   + modified G3 w/ word alignment (CCITT RLEW)
// - LZ77
// - Thunderscan
// - JPEG
// - PCD Huffmann encoding (photo CD)
//
//----------------------------------------------------------------------------------------------------------------------

interface

{$I GraphicConfiguration.inc}

uses                                                
  Windows, Classes, SysUtils, Graphics,  
  JPG,   // JPEG compression support
  MZLib;  // general inflate/deflate and LZ77 compression support
     
type
  // abstract decoder class to define the base functionality of an encoder/decoder
  TDecoder = class
  public
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); virtual; abstract;
    procedure DecodeEnd; virtual;
    procedure DecodeInit; virtual;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); virtual; abstract;
    procedure EncodeInit; virtual;
    procedure EncodeEnd; virtual;
  end;

  // generally, there should be no need to cover the decoder classes by conditional symbols
  // because the image classes which use the decoder classes are already covered and if they
  // aren't compiled then the decoders are also not compiled (more precisely: not linked)
  TTargaRLEDecoder = class(TDecoder)
  private
    FColorDepth: Cardinal;
  public
    constructor Create(ColorDepth: Cardinal); 
    
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  {$ifdef UseLZW}
  // Lempel-Ziff-Welch encoder/decoder class
  // TIFF LZW compression / decompression is a bit different to the common LZW code
  TTIFFLZWDecoder = class(TDecoder)
  public
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;
  {$endif} // UseLZW

  TPackbitsRLEDecoder = class(TDecoder)
  public
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  TPCXRLEDecoder = class(TDecoder)
  public
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  TSGIRLEDecoder = class(TDecoder)
  private
    FSampleSize: Byte; // 8 or 16 bits
  public
    constructor Create(SampleSize: Byte);

    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  TCUTRLEDecoder = class(TDecoder)
  public
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  TPSPRLEDecoder = class(TDecoder)
  public
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  {$ifdef UseLZW}
  // Note: We need a different LZW decoder class for GIF because the bit order is reversed compared to that
  //       of TIFF and the code size increment is handled slightly different.
  TGIFLZWDecoder = class(TDecoder)
  private
    FInitialCodeSize: Byte;
  public
    constructor Create(InitialCodeSize: Byte);

    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;
  {$endif} // UseLZW

  TRLADecoder = class(TDecoder)
  public
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  TStateEntry = record
    NewState: array[Boolean] of Cardinal;
    RunLength: Integer;
  end;
  TStateArray = array of TStateEntry;

  TCCITTDecoder = class(TDecoder)
  private
    FOptions: Integer; // determines some options how to proceed
                       // Bit 0: if set then two-dimensional encoding was used, otherwise one-dimensional
                       // Bit 1: if set then data is uncompressed
                       // Bit 2: if set then fill bits are used before EOL codes so that EOL codes always end at
                       //        at a byte boundary (not used in this context)
    FIsWhite,          // alternating flag used while coding
    FSwapBits: Boolean; // True if the order of all bits in a byte must be swapped
    FWhiteStates,
    FBlackStates: TStateArray;
    FWidth: Cardinal; // need to know how line length for modified huffman encoding

    // coding/encoding variables
    FBitsLeft,
    FMask,
    FBits: Byte;
    FPackedSize,
    FRestWidth: Cardinal;
    FSource,
    FTarget: PByte;
    FFreeTargetBits: Byte;
    FWordAligned: Boolean;
    procedure MakeStates;
  protected
    function FillRun(RunLength: Cardinal): Boolean;
    function FindBlackCode: Integer;
    function FindWhiteCode: Integer;
    function NextBit: Boolean;
  public
    constructor Create(Options: Integer; SwapBits, WordAligned: Boolean; Width: Cardinal);
  end;

  TCCITTFax3Decoder = class(TCCITTDecoder)
  public
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  TCCITTMHDecoder = class(TCCITTDecoder) // modified Huffman RLE
  public
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  TLZ77Decoder = class(TDecoder)
  private
    FStream: TZState;
    FZLibResult,         // contains the return code of the last ZLib operation
    FFlushMode: Integer; // one of flush constants declard in ZLib.pas
                         // this is usually Z_FINISH for PSP and Z_PARTIAL_FLUSH for PNG
    FAutoReset: Boolean; // TIF, PSP and PNG share this decoder, TIF needs a reset for each
                         // decoder run
    function GetAvailableInput: Integer;
    function GetAvailableOutput: Integer;

  public
    constructor Create(FlushMode: Integer; AutoReset: Boolean);

    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure DecodeEnd; override;
    procedure DecodeInit; override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;

    property AvailableInput: Integer read GetAvailableInput;
    property AvailableOutput: Integer read GetAvailableOutput;
    property ZLibResult: Integer read FZLibResult;
  end;

  TTIFFJPEGDecoder = class;
        
  TJPEGGeneral = packed record
    case byte of
      0: (common: jpeg_common_struct);
      1: (d: jpeg_decompress_struct);
      2: (c: jpeg_compress_struct);
  end;

  PJPEGState = ^TJPEGState;
  TJPEGState = record
    General: TJPEGGeneral;                    // must be the first member here because we pass TJPEGState as
                                              // compress, decompress or common struct around to be able
                                              // to access our internal data
    Error: jpeg_error_mgr;                    // libjpeg error manager
    DestinationManager: jpeg_destination_mgr; // data dest for compression
    SourceManager: jpeg_source_mgr;           // data source for decompression
    HSampling,	                              // luminance sampling factors
    VSampling: Word;
    BytesPerLine: Cardinal;                   // decompressed bytes per scanline
    RawBuffer: Pointer;                       // source data
    RawBufferSize: Cardinal;
    // pointers to intermediate buffers when processing downsampled data
    DownSampleBuffer: array[0..MAX_COMPONENTS - 1] of JSAMPARRAY;
    ScanCount,	                              // number of 'scanlines' accumulated
    SamplesPerClump: Integer;
    JPEGTables: Pointer;                      // JPEGTables tag value, or nil
    JTLength: Cardinal;                       // number of bytes JPEGTables
    JPEGQuality,                              // compression quality level
    JPEGTablesMode: Integer;                  // what to put in JPEGTables
  end;

  TTIFFJPEGDecoder = class(TDecoder)
  private
		FState: TJPEGState;
    FImageProperties: Pointer; // anonymously declared because I cannot take GraphicEx.pas in the uses clause above
  public
    constructor Create(Properties: Pointer);

    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure DecodeInit; override;
    procedure DecodeEnd; override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  TThunderDecoder = class(TDecoder)
  private
    FWidth: Cardinal; // width of a scanline in pixels
  public
    constructor Create(Width: Cardinal);

    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  TPCDDecoder = class(TDecoder)
  private
    FStream: TStream;  // decoder must read some data
  public
    constructor Create(Stream: TStream);

    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Math,
  GraphicEx,
  GraphicStrings,
  GraphicColor;

const // LZW encoding and decoding support
  NoLZWCode = 4096;

type
  EGraphicCompression = class(Exception);

//----------------------------------------------------------------------------------------------------------------------

procedure CompressionError(ErrorString: String); overload;

begin
  raise EGraphicCompression.Create(ErrorString);
end;

//----------------- TDecoder (generic decoder class) -------------------------------------------------------------------

procedure TDecoder.DecodeEnd;

// called after all decompression has been done

begin
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDecoder.DecodeInit;

// called before any decompression can start

begin
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDecoder.EncodeEnd;

// called after all compression has been done

begin
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDecoder.EncodeInit;

// called before any compression can start

begin
end;

//----------------- TTargaRLEDecoder -----------------------------------------------------------------------------------

constructor TTargaRLEDecoder.Create(ColorDepth: Cardinal);

begin
  FColorDepth := ColorDepth;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTargaRLEDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

type
  PCardinalArray = ^TCardinalArray;
  TCardinalArray = array[0..MaxInt div 4 - 1] of Cardinal;

var
  I: Integer;
  SourcePtr,
  TargetPtr: PByte;
  RunLength: Cardinal;
  SourceCardinal: Cardinal;

begin
  TargetPtr := Dest;
  SourcePtr := Source;
  // unrolled decoder loop to speed up process
  case FColorDepth of
    8:
      while UnpackedSize > 0 do
      begin
        RunLength := 1 + (SourcePtr^ and $7F);
        if SourcePtr^ > $7F then
        begin
          Inc(SourcePtr);
          FillChar(TargetPtr^, RunLength, SourcePtr^);
          Inc(TargetPtr, RunLength);
          Inc(SourcePtr);
        end
        else
        begin
          Inc(SourcePtr);
          Move(SourcePtr^, TargetPtr^, RunLength);
          Inc(SourcePtr, RunLength);
          Inc(TargetPtr, RunLength);
        end;
        Dec(UnpackedSize, RunLength);
      end;
    15,
    16:
      while UnpackedSize > 0 do
      begin
        RunLength := 1 + (SourcePtr^ and $7F);
        if SourcePtr^ > $7F then
        begin
          Inc(SourcePtr);
          for I := 0 to RunLength - 1 do
          begin
            TargetPtr^ := SourcePtr^;
            Inc(SourcePtr);
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^;
            Dec(SourcePtr);
            Inc(TargetPtr);
          end;
          Inc(SourcePtr, 2);
        end
        else
        begin
          Inc(SourcePtr);
          Move(SourcePtr^, TargetPtr^, 2 * RunLength);
          Inc(SourcePtr, 2 * RunLength);
          Inc(TargetPtr, 2 * RunLength);
        end;
        Dec(UnpackedSize, RunLength);
      end;
    24:
      while UnpackedSize > 0 do
      begin
        RunLength := 1 + (SourcePtr^ and $7F);
        if SourcePtr^ > $7F then
        begin
          Inc(SourcePtr);
          for I := 0 to RunLength - 1 do
          begin
            TargetPtr^ := SourcePtr^;
            Inc(SourcePtr);
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^;
            Inc(SourcePtr);
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^;
            Dec(SourcePtr, 2);
            Inc(TargetPtr);
          end;
          Inc(SourcePtr, 3);
        end
        else
        begin
          Inc(SourcePtr);
          Move(SourcePtr^, TargetPtr^, 3 * RunLength);
          Inc(SourcePtr, 3 * RunLength);
          Inc(TargetPtr, 3 * RunLength);
        end;
        Dec(UnpackedSize, RunLength);
      end;
    32:
      while UnpackedSize > 0 do
      begin
        RunLength := 1 + (SourcePtr^ and $7F);
        if SourcePtr^ > $7F then
        begin
          Inc(SourcePtr);
          SourceCardinal := PCardinalArray(SourcePtr)[0];
          for I := 0 to RunLength - 1 do
            PCardinalArray(TargetPtr)[I] := SourceCardinal;

          Inc(TargetPtr, 4 * RunLength);
          Inc(SourcePtr, 4);
        end
        else
        begin
          Inc(SourcePtr);
          Move(SourcePtr^, TargetPtr^, 4 * RunLength);
          Inc(SourcePtr, 4 * RunLength);
          Inc(TargetPtr, 4 * RunLength);
        end;
        Dec(UnpackedSize, RunLength);
      end;
  end;
    
  Source := SourcePtr;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetPixel(P: PByte; BPP: Byte): Cardinal;

// Retrieves a pixel value from a Buffer. The actual size and order of the bytes is not important
// since we are only using the value for comparisons with other pixels.

begin
  Result := P^;
  Inc(P);
  Dec(BPP);
  while BPP > 0 do
  begin
    Result := Result shl 8;
    Result := Result or P^;
    Inc(P);
    Dec(BPP);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function CountDiffPixels(P: PByte; BPP: Byte; Count: Integer): Integer;

// counts pixels in Buffer until two identical adjacent ones found

var
  N: Integer;
  Pixel,
  NextPixel: Cardinal;

begin
  N := 0;
  NextPixel := 0; // shut up compiler
  if Count = 1 then Result := Count
               else
  begin
    Pixel := GetPixel(P, BPP);
    while Count > 1 do
    begin
      Inc(P, BPP);
      NextPixel := GetPixel(P, BPP);
      if NextPixel = Pixel then Break;
      Pixel := NextPixel;
      Inc(N);
      Dec(Count);
    end;
    if NextPixel = Pixel then Result := N
                         else Result := N + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function CountSamePixels(P: PByte; BPP: Byte; Count: Integer): Integer;

var
  Pixel,
  NextPixel: Cardinal;

begin
  Result := 1;
  Pixel := GetPixel(P, BPP);
  Dec(Count);
  while Count > 0 do
  begin
    Inc(P, BPP);
    NextPixel := GetPixel(P, BPP);
    if NextPixel <> Pixel then Break;
    Inc(Result);
    Dec(Count);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTargaRLEDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

// Encodes "Count" bytes pointed to by Source into the Buffer supplied with Target and returns the
// number of bytes stored in Target. BPP denotes bytes per pixel color depth.
// Note: The target Buffer must provide enough space to hold the compressed data. Using a size of
//       twice the size of the input Buffer is sufficent.

var
  DiffCount, // pixel count until two identical
  SameCount: Integer; // number of identical adjacent pixels
  SourcePtr,
  TargetPtr: PByte;
  BPP: Integer;

begin
  BytesStored := 0;
  SourcePtr := Source;
  TargetPtr := Dest;
  BytesStored := 0;
  // +1 for 15 bits to get the correct 2 bytes per pixel
  BPP := (FColorDepth + 1) div 8;
  while Count > 0 do
  begin
    DiffCount := CountDiffPixels(SourcePtr, BPP, Count);
    SameCount := CountSamePixels(SourcePtr, BPP, Count);
    if DiffCount > 128 then DiffCount := 128;
    if SameCount > 128  then SameCount := 128;

    if DiffCount > 0 then
    begin
      // create a raw packet
      TargetPtr^ := DiffCount - 1; Inc(TargetPtr);
      Dec(Count, DiffCount);
      Inc(BytesStored, (DiffCount * BPP) + 1);
      while DiffCount > 0 do
      begin
        TargetPtr^ := SourcePtr^; Inc(SourcePtr); Inc(TargetPtr);
        if BPP > 1 then begin TargetPtr^ := SourcePtr^; Inc(SourcePtr); Inc(TargetPtr); end;
        if BPP > 2 then begin TargetPtr^ := SourcePtr^; Inc(SourcePtr); Inc(TargetPtr); end;
        if BPP > 3 then begin TargetPtr^ := SourcePtr^; Inc(SourcePtr); Inc(TargetPtr); end;
        Dec(DiffCount);
      end;
    end;

    if SameCount > 1 then
    begin
      // create a RLE packet
      TargetPtr^ := (SameCount - 1) or $80; Inc(TargetPtr);
      Dec(Count, SameCount);
      Inc(BytesStored, BPP + 1);
      Inc(SourcePtr, (SameCount - 1) * BPP);
      TargetPtr^ := SourcePtr^; Inc(SourcePtr); Inc(TargetPtr);
      if BPP > 1 then begin TargetPtr^ := SourcePtr^; Inc(SourcePtr); Inc(TargetPtr); end;
      if BPP > 2 then begin TargetPtr^ := SourcePtr^; Inc(SourcePtr); Inc(TargetPtr); end;
      if BPP > 3 then begin TargetPtr^ := SourcePtr^; Inc(SourcePtr); Inc(TargetPtr); end;
    end;
  end;
end;

//----------------- TTIFFLZWDecoder ------------------------------------------------------------------------------------

{$ifdef UseLZW}

procedure TTIFFLZWDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

var
  I: Integer;
  Data,           // current data
  Bits,           // counter for bit management
  Code: Cardinal; // current code value
  SourcePtr: PByte;
  InCode: Cardinal; // Buffer for passed code

  CodeSize: Cardinal;
  CodeMask: Cardinal;
  FreeCode: Cardinal;
  OldCode: Cardinal;
  Prefix: array[0..4095] of Cardinal; // LZW prefix
  Suffix,                             // LZW suffix
  Stack: array [0..4095] of Byte;     // stack
  StackPointer: PByte;
  Target: PByte;
  FirstChar: Byte;  // Buffer for decoded byte
  ClearCode,
  EOICode: Word;

begin
  Target := Dest;
  SourcePtr := Source;

  // initialize parameter
  ClearCode := 1 shl 8;
  EOICode := ClearCode + 1;
  FreeCode := ClearCode + 2;
  OldCode := NoLZWCode;
  CodeSize := 9;
  CodeMask := (1 shl CodeSize) - 1; 

  // init code table
  for I := 0 to ClearCode - 1 do
  begin
    Prefix[I] := NoLZWCode;
    Suffix[I] := I;
  end;

  // initialize stack
  StackPointer := @Stack;
  FirstChar := 0;

  Data := 0;
  Bits := 0;  
  while (PackedSize > 0) and (UnpackedSize > 0) do
  begin
    // read code from bit stream
    Inc(Data, Cardinal(SourcePtr^) shl (24 - Bits));
    Inc(Bits, 8);
    while Bits >= CodeSize do
    begin
      // current code
      Code := (Data and ($FFFFFFFF - CodeMask)) shr (32 - CodeSize);
      // mask it
      Data := Data shl CodeSize;
      Dec(Bits, CodeSize);

      if Code = EOICode then Exit;

      // handling of clear codes
      if Code = ClearCode then
      begin
        // reset of all variables
        CodeSize := 9;
        CodeMask := (1 shl CodeSize) - 1;
        FreeCode := ClearCode + 2;
        OldCode := NoLZWCode;
        Continue;
      end;

      // check whether it is a valid, already registered code
      if Code > FreeCode then Break;

      // handling for the first LZW code: print and keep it
      if OldCode = NoLZWCode then
      begin
        FirstChar := Suffix[Code];
        Target^ := FirstChar;
        Inc(Target);
        Dec(UnpackedSize);
        OldCode := Code;
        Continue;
      end;

      // keep the passed LZW code
      InCode := Code;  

      // the first LZW code is always smaller than FFirstCode
      if Code = FreeCode then
      begin
        StackPointer^ := FirstChar;
        Inc(StackPointer);
        Code := OldCode;
      end;

      // loop to put decoded bytes onto the stack
      while Code > ClearCode do
      begin
        StackPointer^ := Suffix[Code];
        Inc(StackPointer);
        Code := Prefix[Code];
      end;

      // place new code into code table
      FirstChar := Suffix[Code];
      StackPointer^ := FirstChar;
      Inc(StackPointer);
      Prefix[FreeCode] := OldCode;
      Suffix[FreeCode] := FirstChar;
      if FreeCode < 4096 then Inc(FreeCode);

      // increase code size if necessary
      if (FreeCode = CodeMask) and
         (CodeSize < 12) then
      begin
        Inc(CodeSize);
        CodeMask := (1 shl CodeSize) - 1;
      end;

      // put decoded bytes (from the stack) into the target Buffer
      OldCode := InCode;
      repeat
        Dec(StackPointer);
        Target^ := StackPointer^;
        Inc(Target);
        Dec(UnpackedSize);
      until Cardinal(StackPointer) <= Cardinal(@Stack);
    end;
    Inc(SourcePtr);
    Dec(PackedSize);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTIFFLZWDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin

end;

{$endif} // UseLZW

//----------------- TPackbitsRLEDecoder --------------------------------------------------------------------------------

procedure TPackbitsRLEDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

// decodes a simple run-length encoded strip of size PackedSize

var
  SourcePtr,
  TargetPtr: PByte;
  N: Integer;

begin
  TargetPtr := Dest;
  SourcePtr := Source;
  while (UnpackedSize > 0) and
        (PackedSize > 0) do
  begin
    N := ShortInt(SourcePtr^);
    Inc(SourcePtr);
    Dec(PackedSize);
    if N < 0 then // replicate next Byte -N + 1 times
    begin
      if N = -128 then Continue; // nop
      N := -N + 1;
      if N > UnpackedSize then N := UnpackedSize;
      FillChar(TargetPtr^, N, SourcePtr^);
      Inc(SourcePtr);
      Dec(PackedSize);
      Inc(TargetPtr, N);
      Dec(UnpackedSize, N);
    end
    else
    begin // copy next N + 1 bytes literally
      Inc(N);
      if N > UnpackedSize then N := UnpackedSize;
      if N > PackedSize then N := PackedSize;
      Move(SourcePtr^, TargetPtr^, N);
      Inc(TargetPtr, N);
      Inc(SourcePtr, N);
      Dec(PackedSize, N);
      Dec(UnpackedSize, N);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPackbitsRLEDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin

end;

//----------------- TPCXRLEDecoder -------------------------------------------------------------------------------------

procedure TPCXRLEDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

var
  Count: Integer;
  SourcePtr,
  TargetPtr: PByte;

begin
  SourcePtr := Source;
  TargetPtr := Dest;
  while UnpackedSize > 0 do
  begin
    if (SourcePtr^ and $C0) = $C0 then
    begin
      // RLE-Code
      Count := SourcePtr^ and $3F;
      Inc(SourcePtr);
      if UnpackedSize < Count then Count := UnpackedSize;
      FillChar(TargetPtr^, Count, SourcePtr^);
      Inc(SourcePtr);
      Inc(TargetPtr, Count);
      Dec(UnpackedSize, Count);
    end
    else
    begin
      // not compressed
      TargetPtr^ := SourcePtr^;
      Inc(SourcePtr);
      Inc(TargetPtr);
      Dec(UnpackedSize);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPCXRLEDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin

end;

//----------------- TSGIRLEDecoder -------------------------------------------------------------------------------------

constructor TSGIRLEDecoder.Create(SampleSize: Byte);

begin
  FSampleSize := SampleSize;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSGIRLEDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

var
  Source8,
  Target8: PByte;
  Source16,
  Target16: PWord;
  Pixel: Byte;
  Pixel16: Word;
  RunLength: Cardinal;

begin
  if FSampleSize = 8 then
  begin
    Source8 := Source;
    Target8 := Dest;
    while True do
    begin
      Pixel := Source8^;
      Inc(Source8);
      RunLength := Pixel and $7F;
      if RunLength = 0 then Break;

      if (Pixel and $80) <> 0 then
      begin
        Move(Source8^, Target8^, RunLength);
        Inc(Target8, RunLength);
        Inc(Source8, RunLength);
      end
      else
      begin
        Pixel := Source8^;
        Inc(Source8);
        FillChar(Target8^, RunLength, Pixel);
        Inc(Target8, RunLength);
      end;
    end;
  end
  else
  begin
    // 16 bits per sample
    Source16 := Source;
    Target16 := Dest;
    while True do
    begin
      // SGI images are stored in big endian style, swap this one repeater value for it
      Pixel16 := Swap(Source16^);
      Inc(Source16);
      RunLength := Pixel16 and $7F;
      if RunLength = 0 then Break;

      if (Pixel16 and $80) <> 0 then
      begin
        Move(Source16^, Target16^, 2 * RunLength);
        Inc(Source16^, RunLength);
        Inc(Target16^, RunLength);
      end
      else
      begin
        Pixel16 := Source16^;
        Inc(Source16);
        while RunLength > 0 do
        begin
          Target16^ := Pixel16;
          Inc(Target16);
          Dec(RunLength);
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSGIRLEDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin

end;

//----------------- TCUTRLE --------------------------------------------------------------------------------------------

procedure TCUTRLEDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

var
  TargetPtr: PByte;
  Pixel: Byte;
  RunLength: Cardinal;

begin
  TargetPtr := Dest;
  // skip first two bytes per row (I don't know their meaning)
  Inc(PByte(Source), 2);
  while True do
  begin
    Pixel := PByte(Source)^;
    Inc(PByte(Source));
    if Pixel = 0 then Break;

    RunLength := Pixel and $7F;
    if (Pixel and $80) = 0 then
    begin
      Move(Source^, TargetPtr^, RunLength);
      Inc(TargetPtr, RunLength);
      Inc(PByte(Source), RunLength);
    end
    else
    begin
      Pixel := PByte(Source)^;
      Inc(PByte(Source));
      FillChar(TargetPtr^, RunLength, Pixel);
      Inc(TargetPtr, RunLength);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCUTRLEDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin

end;

//----------------- TPSPRLEDecoder -------------------------------------------------------------------------------------

procedure TPSPRLEDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

var
  SourcePtr,
  TargetPtr: PByte;
  RunLength: Cardinal;

begin
  SourcePtr := Source;
  TargetPtr := Dest;
  while PackedSize > 0 do
  begin
    RunLength := SourcePtr^;
    Inc(SourcePtr);
    Dec(PackedSize);
    if RunLength < 128 then
    begin
      Move(SourcePtr^, TargetPtr^, RunLength);
      Inc(TargetPtr, RunLength);
      Inc(SourcePtr, RunLength);
      Dec(PackedSize, RunLength);
    end
    else
    begin
      Dec(RunLength, 128);
      FillChar(TargetPtr^, RunLength, SourcePtr^);
      Inc(SourcePtr);
      Inc(TargetPtr, RunLength);
      Dec(PackedSize);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPSPRLEDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin

end;

//----------------- TGIFLZWDecoder -------------------------------------------------------------------------------------

{$ifdef UseLZW}

constructor TGIFLZWDecoder.Create(InitialCodeSize: Byte);

begin
  FInitialCodeSize := InitialCodeSize;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGIFLZWDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

var
  I: Integer;
  Data,           // current data
  Bits,           // counter for bit management
  Code: Cardinal; // current code value
  SourcePtr: PByte;
  InCode: Cardinal; // Buffer for passed code

  CodeSize: Cardinal;
  CodeMask: Cardinal;
  FreeCode: Cardinal;
  OldCode: Cardinal;
  Prefix: array[0..4095] of Cardinal; // LZW prefix
  Suffix,                             // LZW suffix
  Stack: array [0..4095] of Byte;     // stack
  StackPointer: PByte;
  Target: PByte;
  FirstChar: Byte;  // Buffer for decoded byte
  ClearCode,
  EOICode: Word;

begin
  Target := Dest;
  SourcePtr := Source;

  // initialize parameter
  CodeSize := FInitialCodeSize + 1;
  ClearCode := 1 shl FInitialCodeSize;
  EOICode := ClearCode + 1;
  FreeCode := ClearCode + 2;
  OldCode := NoLZWCode;
  CodeMask := (1 shl CodeSize) - 1;

  // init code table
  for I := 0 to ClearCode - 1 do
  begin
    Prefix[I] := NoLZWCode;
    Suffix[I] := I;
  end;

  // initialize stack
  StackPointer := @Stack;
  FirstChar := 0;

  Data := 0;
  Bits := 0;
  while (UnpackedSize > 0) and (PackedSize > 0) do
  begin
    // read code from bit stream
    Inc(Data, SourcePtr^ shl Bits);
    Inc(Bits, 8);
    while Bits >= CodeSize do
    begin
      // current code
      Code := Data and CodeMask;
      // prepare next run
      Data := Data shr CodeSize;
      Dec(Bits, CodeSize);

      // decoding finished?
      if Code = EOICode then Break;

      // handling of clear codes
      if Code = ClearCode then
      begin
        // reset of all variables
        CodeSize := FInitialCodeSize + 1;
        CodeMask := (1 shl CodeSize) - 1;
        FreeCode := ClearCode + 2;
        OldCode := NoLZWCode;
        Continue;
      end;

      // check whether it is a valid, already registered code
      if Code > FreeCode then Break;

      // handling for the first LZW code: print and keep it
      if OldCode = NoLZWCode then
      begin
        FirstChar := Suffix[Code];
        Target^ := FirstChar;
        Inc(Target);
        Dec(UnpackedSize);
        OldCode := Code;
        Continue;
      end;

      // keep the passed LZW code
      InCode := Code;  

      // the first LZW code is always smaller than FFirstCode
      if Code = FreeCode then
      begin
        StackPointer^ := FirstChar;
        Inc(StackPointer);
        Code := OldCode;
      end;

      // loop to put decoded bytes onto the stack
      while Code > ClearCode do
      begin
        StackPointer^ := Suffix[Code];
        Inc(StackPointer);
        Code := Prefix[Code];
      end;

      // place new code into code table
      FirstChar := Suffix[Code];
      StackPointer^ := FirstChar;
      Inc(StackPointer);
      Prefix[FreeCode] := OldCode;
      Suffix[FreeCode] := FirstChar;

      // increase code size if necessary
      if (FreeCode = CodeMask) and
         (CodeSize < 12) then
      begin
        Inc(CodeSize);
        CodeMask := (1 shl CodeSize) - 1;
      end;
      if FreeCode < 4095 then Inc(FreeCode);

      // put decoded bytes (from the stack) into the target Buffer
      OldCode := InCode;
      repeat
        Dec(StackPointer);
        Target^ := StackPointer^;
        Inc(Target);
        Dec(UnpackedSize);
      until StackPointer = @Stack;
    end;
    Inc(SourcePtr);
    Dec(PackedSize);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGIFLZWDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin

end;

{$endif}

//----------------- TRLADecoder ----------------------------------------------------------------------------------------

procedure TRLADecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

// decodes a simple run-length encoded strip of size PackedSize
// this is very similar to TPackbitsRLEDecoder

var
  SourcePtr,
  TargetPtr: PByte;
  N: SmallInt;

begin
  TargetPtr := Dest;
  SourcePtr := Source;
  while PackedSize > 0 do
  begin
    N := ShortInt(SourcePtr^);
    Inc(SourcePtr);
    Dec(PackedSize);
    if N >= 0 then // replicate next Byte N + 1 times
    begin
      FillChar(TargetPtr^, N + 1, SourcePtr^);
      Inc(TargetPtr, N + 1);
      Inc(SourcePtr);
      Dec(PackedSize);
    end
    else
    begin // copy next -N bytes literally
      Move(SourcePtr^, TargetPtr^, -N);
      Inc(TargetPtr, -N);
      Inc(SourcePtr, -N);
      Inc(PackedSize, N);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRLADecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin

end;

//----------------- TCCITTDecoder --------------------------------------------------------------------------------------

constructor TCCITTDecoder.Create(Options: Integer; SwapBits, WordAligned: Boolean; Width: Cardinal);

begin
  FOptions := Options;
  FSwapBits := SwapBits;
  FWidth := Width;
  FWordAligned := WordAligned;
  MakeStates;
end;

//----------------------------------------------------------------------------------------------------------------------

const
  // 256 bytes to make bit reversing easy,
  // this is actually not much more than writing bit manipulation code, but much faster
  ReverseTable: array[0..255] of Byte = (
    $00, $80, $40, $C0, $20, $A0, $60, $E0,
    $10, $90, $50, $D0, $30, $B0, $70, $F0,
    $08, $88, $48, $C8, $28, $A8, $68, $E8,
    $18, $98, $58, $D8, $38, $B8, $78, $F8,
    $04, $84, $44, $C4, $24, $A4, $64, $E4,
    $14, $94, $54, $D4, $34, $B4, $74, $F4,
    $0C, $8C, $4C, $CC, $2C, $AC, $6C, $EC,
    $1C, $9C, $5C, $DC, $3C, $BC, $7C, $FC,
    $02, $82, $42, $C2, $22, $A2, $62, $E2,
    $12, $92, $52, $D2, $32, $B2, $72, $F2,
    $0A, $8A, $4A, $CA, $2A, $AA, $6A, $EA,
    $1A, $9A, $5A, $DA, $3A, $BA, $7A, $FA,
    $06, $86, $46, $C6, $26, $A6, $66, $E6,
    $16, $96, $56, $D6, $36, $B6, $76, $F6,
    $0E, $8E, $4E, $CE, $2E, $AE, $6E, $EE,
    $1E, $9E, $5E, $DE, $3E, $BE, $7E, $FE,
    $01, $81, $41, $C1, $21, $A1, $61, $E1,
    $11, $91, $51, $D1, $31, $B1, $71, $F1,
    $09, $89, $49, $C9, $29, $A9, $69, $E9,
    $19, $99, $59, $D9, $39, $B9, $79, $F9,
    $05, $85, $45, $C5, $25, $A5, $65, $E5,
    $15, $95, $55, $D5, $35, $B5, $75, $F5,
    $0D, $8D, $4D, $CD, $2D, $AD, $6D, $ED,
    $1D, $9D, $5D, $DD, $3D, $BD, $7D, $FD,
    $03, $83, $43, $C3, $23, $A3, $63, $E3,
    $13, $93, $53, $D3, $33, $B3, $73, $F3,
    $0B, $8B, $4B, $CB, $2B, $AB, $6B, $EB,
    $1B, $9B, $5B, $DB, $3B, $BB, $7B, $FB,
    $07, $87, $47, $C7, $27, $A7, $67, $E7,
    $17, $97, $57, $D7, $37, $B7, $77, $F7,
    $0F, $8F, $4F, $CF, $2F, $AF, $6F, $EF,
    $1F, $9F, $5F, $DF, $3F, $BF, $7F, $FF
  );

  G3_EOL = -1;
  G3_INVALID = -2;

//----------------------------------------------------------------------------------------------------------------------

function TCCITTDecoder.FillRun(RunLength: Cardinal): Boolean;

// fills a number of bits with 1s (for black, white only increments pointers),
// returns True if the line has been filled entirely, otherwise False

var
  Run: Cardinal;

begin
  Run := Min(FFreeTargetBits, RunLength);
  // fill remaining bits in the current byte
  if Run in [1..7] then
  begin
    Dec(FFreeTargetBits, Run);
    if not FIsWhite then FTarget^ := FTarget^ or (((1 shl Run) - 1) shl FFreeTargetBits);
    if FFreeTargetBits = 0 then
    begin
      Inc(FTarget);
      FFreeTargetBits := 8;
    end;
    Run := RunLength - Run;
  end
  else Run := RunLength;

  // fill entire bytes whenever possible
  if Run > 0 then
  begin
    if not FIsWhite then FillChar(FTarget^, Run div 8, $FF);
    Inc(FTarget, Run div 8);
    Run := Run mod 8;
  end;

  // finally fill remaining bits
  if Run > 0 then
  begin
    FFreeTargetBits := 8 - Run;
    if not FIsWhite then FTarget^ := ((1 shl Run) - 1) shl FFreeTargetBits;
  end;

  // this will throw an exception if the sum of the run lengths for a row is not
  // exactly the row size (the documentation speaks of an unrecoverable error)
  if Cardinal(RunLength) > FRestWidth then RunLength := FRestWidth;
  Dec(FRestWidth, RunLength);
  Result := FRestWidth = 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCCITTDecoder.FindBlackCode: Integer;

// Executes the state machine to find the run length for the next bit combination.
// Returns the run length of the found code.

var
  State,
  NewState: Cardinal;
  Bit: Boolean;
  
begin
  State := 0;
  Result := 0;
  repeat
    // advance to next byte in the input Buffer if necessary
    if FBitsLeft = 0 then
    begin
      if FPackedSize = 0 then Break;
      FBits := FSource^;
      Inc(FSource);
      Dec(FPackedSize);
      FMask := $80;
      FBitsLeft := 8;
    end;
    Bit := (FBits and FMask) <> 0;

    // advance the state machine
    NewState := FBlackStates[State].NewState[Bit];
    if NewState = 0 then
    begin
      Inc(Result, FBlackStates[State].RunLength);
      if FBlackStates[State].RunLength < 64 then Break
                                            else
      begin
        NewState := FBlackStates[0].NewState[Bit];
      end;
    end;
    State := NewState;

    // address next bit
    FMask := FMask shr 1;
    if FBitsLeft > 0 then Dec(FBitsLeft);
  until False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCCITTDecoder.FindWhiteCode: Integer;

// Executes the state machine to find the run length for the next bit combination.
// Returns the run length of the found code.

var
  State,
  NewState: Cardinal;
  Bit: Boolean;

begin
  State := 0;
  Result := 0;
  repeat
    // advance to next byte in the input Buffer if necessary
    if FBitsLeft = 0 then
    begin
      if FPackedSize = 0 then Break;
      FBits := FSource^;
      Inc(FSource);
      Dec(FPackedSize);
      FMask := $80;
      FBitsLeft := 8;
    end;
    Bit := (FBits and FMask) <> 0;

    // advance the state machine
    NewState := FWhiteStates[State].NewState[Bit];
    if NewState = 0 then
    begin
      // a code has been found
      Inc(Result, FWhiteStates[State].RunLength);
      // if we found a terminating code then exit loop, otherwise continue
      if FWhiteStates[State].RunLength < 64 then Break
                                            else
      begin
        // found a make up code, continue state machine with current bit (rather than reading the next one)
        NewState := FWhiteStates[0].NewState[Bit];
      end;
    end;
    State := NewState;

    // address next bit
    FMask := FMask shr 1;
    if FBitsLeft > 0 then Dec(FBitsLeft);
  until False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCCITTDecoder.NextBit: Boolean;

// Reads the current bit and returns True if it is set, otherwise False.
// This method is only used in the process to synchronize the bit stream in descentants.

begin
  // advance to next byte in the input Buffer if necessary
  if (FBitsLeft = 0) and (FPackedSize > 0) then
  begin
    FBits := FSource^;
    Inc(FSource);
    Dec(FPackedSize);
    FMask := $80;
    FBitsLeft := 8;
  end;
  Result := (FBits and FMask) <> 0;

  // address next bit
  FMask := FMask shr 1;
  if FBitsLeft > 0 then Dec(FBitsLeft);
end;

//----------------------------------------------------------------------------------------------------------------------

type
  TCodeEntry = record
    Code, Len: Cardinal;
  end;

const // CCITT code tables
  WhiteCodes: array[0..103] of TCodeEntry = (
    (Code : $0035; Len : 8),
    (Code : $0007; Len : 6),
    (Code : $0007; Len : 4),
    (Code : $0008; Len : 4),
    (Code : $000B; Len : 4),
    (Code : $000C; Len : 4),
    (Code : $000E; Len : 4),
    (Code : $000F; Len : 4),
    (Code : $0013; Len : 5),
    (Code : $0014; Len : 5),
    (Code : $0007; Len : 5),
    (Code : $0008; Len : 5),
    (Code : $0008; Len : 6),
    (Code : $0003; Len : 6),
    (Code : $0034; Len : 6),
    (Code : $0035; Len : 6),
    (Code : $002A; Len : 6),
    (Code : $002B; Len : 6),
    (Code : $0027; Len : 7),
    (Code : $000C; Len : 7),
    (Code : $0008; Len : 7),
    (Code : $0017; Len : 7),
    (Code : $0003; Len : 7),
    (Code : $0004; Len : 7),
    (Code : $0028; Len : 7),
    (Code : $002B; Len : 7),
    (Code : $0013; Len : 7),
    (Code : $0024; Len : 7),
    (Code : $0018; Len : 7),
    (Code : $0002; Len : 8),
    (Code : $0003; Len : 8),
    (Code : $001A; Len : 8),
    (Code : $001B; Len : 8),
    (Code : $0012; Len : 8),
    (Code : $0013; Len : 8),
    (Code : $0014; Len : 8),
    (Code : $0015; Len : 8),
    (Code : $0016; Len : 8),
    (Code : $0017; Len : 8),
    (Code : $0028; Len : 8),
    (Code : $0029; Len : 8),
    (Code : $002A; Len : 8),
    (Code : $002B; Len : 8),
    (Code : $002C; Len : 8),
    (Code : $002D; Len : 8),
    (Code : $0004; Len : 8),
    (Code : $0005; Len : 8),
    (Code : $000A; Len : 8),
    (Code : $000B; Len : 8),
    (Code : $0052; Len : 8),
    (Code : $0053; Len : 8),
    (Code : $0054; Len : 8),
    (Code : $0055; Len : 8),
    (Code : $0024; Len : 8),
    (Code : $0025; Len : 8),
    (Code : $0058; Len : 8),
    (Code : $0059; Len : 8),
    (Code : $005A; Len : 8),
    (Code : $005B; Len : 8),
    (Code : $004A; Len : 8),
    (Code : $004B; Len : 8),
    (Code : $0032; Len : 8),
    (Code : $0033; Len : 8),
    (Code : $0034; Len : 8),
    (Code : $001B; Len : 5),
    (Code : $0012; Len : 5),
    (Code : $0017; Len : 6),
    (Code : $0037; Len : 7),
    (Code : $0036; Len : 8),
    (Code : $0037; Len : 8),
    (Code : $0064; Len : 8),
    (Code : $0065; Len : 8),
    (Code : $0068; Len : 8),
    (Code : $0067; Len : 8),
    (Code : $00CC; Len : 9),
    (Code : $00CD; Len : 9),
    (Code : $00D2; Len : 9),
    (Code : $00D3; Len : 9),
    (Code : $00D4; Len : 9),
    (Code : $00D5; Len : 9),
    (Code : $00D6; Len : 9),
    (Code : $00D7; Len : 9),
    (Code : $00D8; Len : 9),
    (Code : $00D9; Len : 9),
    (Code : $00DA; Len : 9),
    (Code : $00DB; Len : 9),
    (Code : $0098; Len : 9),
    (Code : $0099; Len : 9),
    (Code : $009A; Len : 9),
    (Code : $0018; Len : 6),
    (Code : $009B; Len : 9),
    (Code : $0008; Len : 11),
    (Code : $000C; Len : 11),
    (Code : $000D; Len : 11),
    (Code : $0012; Len : 12),
    (Code : $0013; Len : 12),
    (Code : $0014; Len : 12),
    (Code : $0015; Len : 12),
    (Code : $0016; Len : 12),
    (Code : $0017; Len : 12),
    (Code : $001C; Len : 12),
    (Code : $001D; Len : 12),
    (Code : $001E; Len : 12),
    (Code : $001F; Len : 12)
    // EOL codes are added "manually"
  );

  BlackCodes: array[0..103] of TCodeEntry = (
    (Code : $0037; Len : 10),
    (Code : $0002; Len : 3),
    (Code : $0003; Len : 2),
    (Code : $0002; Len : 2),
    (Code : $0003; Len : 3),
    (Code : $0003; Len : 4),
    (Code : $0002; Len : 4),
    (Code : $0003; Len : 5),
    (Code : $0005; Len : 6),
    (Code : $0004; Len : 6),
    (Code : $0004; Len : 7),
    (Code : $0005; Len : 7),
    (Code : $0007; Len : 7),
    (Code : $0004; Len : 8),
    (Code : $0007; Len : 8),
    (Code : $0018; Len : 9),
    (Code : $0017; Len : 10),
    (Code : $0018; Len : 10),
    (Code : $0008; Len : 10),
    (Code : $0067; Len : 11),
    (Code : $0068; Len : 11),
    (Code : $006C; Len : 11),
    (Code : $0037; Len : 11),
    (Code : $0028; Len : 11),
    (Code : $0017; Len : 11),
    (Code : $0018; Len : 11),
    (Code : $00CA; Len : 12),
    (Code : $00CB; Len : 12),
    (Code : $00CC; Len : 12),
    (Code : $00CD; Len : 12),
    (Code : $0068; Len : 12),
    (Code : $0069; Len : 12),
    (Code : $006A; Len : 12),
    (Code : $006B; Len : 12),
    (Code : $00D2; Len : 12),
    (Code : $00D3; Len : 12),
    (Code : $00D4; Len : 12),
    (Code : $00D5; Len : 12),
    (Code : $00D6; Len : 12),
    (Code : $00D7; Len : 12),
    (Code : $006C; Len : 12),
    (Code : $006D; Len : 12),
    (Code : $00DA; Len : 12),
    (Code : $00DB; Len : 12),
    (Code : $0054; Len : 12),
    (Code : $0055; Len : 12),
    (Code : $0056; Len : 12),
    (Code : $0057; Len : 12),
    (Code : $0064; Len : 12),
    (Code : $0065; Len : 12),
    (Code : $0052; Len : 12),
    (Code : $0053; Len : 12),
    (Code : $0024; Len : 12),
    (Code : $0037; Len : 12),
    (Code : $0038; Len : 12),
    (Code : $0027; Len : 12),
    (Code : $0028; Len : 12),
    (Code : $0058; Len : 12),
    (Code : $0059; Len : 12),
    (Code : $002B; Len : 12),
    (Code : $002C; Len : 12),
    (Code : $005A; Len : 12),
    (Code : $0066; Len : 12),
    (Code : $0067; Len : 12),
    (Code : $000F; Len : 10),
    (Code : $00C8; Len : 12),
    (Code : $00C9; Len : 12),
    (Code : $005B; Len : 12),
    (Code : $0033; Len : 12),
    (Code : $0034; Len : 12),
    (Code : $0035; Len : 12),
    (Code : $006C; Len : 13),
    (Code : $006D; Len : 13),
    (Code : $004A; Len : 13),
    (Code : $004B; Len : 13),
    (Code : $004C; Len : 13),
    (Code : $004D; Len : 13),
    (Code : $0072; Len : 13),
    (Code : $0073; Len : 13),
    (Code : $0074; Len : 13),
    (Code : $0075; Len : 13),
    (Code : $0076; Len : 13),
    (Code : $0077; Len : 13),
    (Code : $0052; Len : 13),
    (Code : $0053; Len : 13),
    (Code : $0054; Len : 13),
    (Code : $0055; Len : 13),
    (Code : $005A; Len : 13),
    (Code : $005B; Len : 13),
    (Code : $0064; Len : 13),
    (Code : $0065; Len : 13),
    (Code : $0008; Len : 11),
    (Code : $000C; Len : 11),
    (Code : $000D; Len : 11),
    (Code : $0012; Len : 12),
    (Code : $0013; Len : 12),
    (Code : $0014; Len : 12),
    (Code : $0015; Len : 12),
    (Code : $0016; Len : 12),
    (Code : $0017; Len : 12),
    (Code : $001C; Len : 12),
    (Code : $001D; Len : 12),
    (Code : $001E; Len : 12),
    (Code : $001F; Len : 12)
    // EOL codes are added "manually"
  );

procedure TCCITTDecoder.MakeStates;

// creates state arrays for white and black codes
// These state arrays are so designed that they have at each state (starting with state 0) a new state index
// into the same array according to the bit for which the state is current.

  //--------------- local functions -------------------------------------------

  procedure AddCode(var Target: TStateArray; Bits: Cardinal; BitLen, RL: Integer);

  // interprets the given string as a sequence of bits and makes a state chain from it

  var
    State,
    NewState: Integer;
    Bit: Boolean;

  begin
    // start state
    State := 0;
    // prepare bit combination (bits are given right align, but must be scanned from left)
    Bits := Bits shl (32 - BitLen);
    while BitLen > 0 do
    begin
      // determine next state according to the bit string
      asm
        SHL [Bits], 1
        SETC [Bit]
      end;
      NewState := Target[State].NewState[Bit];
      // Is it a not yet assigned state?
      if NewState = 0 then
      begin
        // if yes then create a new state at the end of the array
        NewState := Length(Target);
        Target[State].NewState[Bit] := NewState;
        SetLength(Target, Length(Target) + 1);
      end;
      State := NewState;

      Dec(BitLen);
    end;
    // at this point State indicates the final state where we must store the run length for the
    // particular bit combination
    Target[State].RunLength := RL;
  end;

  //--------------- end local functions ---------------------------------------

var
  I: Integer;

begin
  // set an initial entry in each state array
  SetLength(FWhiteStates, 1);
  SetLength(FBlackStates, 1);

  // with codes
  for I := 0 to 63 do
    with WhiteCodes[I] do AddCode(FWhiteStates, Code, Len, I);
  for I := 64 to 103 do
    with WhiteCodes[I] do AddCode(FWhiteStates, Code, Len, (I - 63) * 64);

  AddCode(FWhiteStates, 1, 12, G3_EOL);
  AddCode(FWhiteStates, 1, 9, G3_INVALID);
  AddCode(FWhiteStates, 1, 10, G3_INVALID);
  AddCode(FWhiteStates, 1, 11, G3_INVALID);
  AddCode(FWhiteStates, 0, 12, G3_INVALID);

  // black codes
  for I := 0 to 63 do
    with BlackCodes[I] do AddCode(FBlackStates, Code, Len, I);
  for I := 64 to 103 do
    with BlackCodes[I] do AddCode(FBlackStates, Code, Len, (I - 63) * 64);

  AddCode(FBlackStates, 1, 12, G3_EOL);
  AddCode(FBlackStates, 1, 9, G3_INVALID);
  AddCode(FBlackStates, 1, 10, G3_INVALID);
  AddCode(FBlackStates, 1, 11, G3_INVALID);
  AddCode(FBlackStates, 0, 12, G3_INVALID);
end;

//----------------- TCCITTFax3Decoder ----------------------------------------------------------------------------------

procedure TCCITTFax3Decoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

var
  RunLength: Integer;
  EOLCount: Integer;

  //--------------- local functions -------------------------------------------

  procedure SynchBOL;

  // synch bit stream to next line start

  var
    Count: Integer;

  begin
    // if no EOL codes have been read so far then do it now
    if EOLCount = 0 then
    begin
      // advance until 11 consecutive 0 bits have been found
      Count := 0;
      while (Count < 11) and (FPackedSize > 0) do
      begin
        if NextBit then Count := 0
                   else Inc(Count);
      end;
    end;

    // read 8 bit until at least one set bit is found
    repeat
      Count := 0;
      while (Count < 8) and (FPackedSize > 0) do
      begin
        if NextBit then Count := 9
                   else Inc(Count);
      end;
    until (Count > 8) or (FPackedSize = 0);

    // here we are already beyond the set bit and can restart scanning
    EOLCount := 0;
  end;

  //---------------------------------------------------------------------------

  procedure AdjustEOL;

  begin
    FIsWhite := False;
    if FFreeTargetBits in [1..7] then Inc(FTarget);
    FFreeTargetBits := 8;
    FRestWidth := FWidth;
  end;

  //--------------- end local functions ---------------------------------------

begin  
  // make all bits white
  FillChar(Dest^, UnpackedSize, 0);

  // swap all bits here, in order to avoid frequent tests in the main loop
  if FSwapBits then
  asm
         PUSH EBX
         LEA EBX, ReverseTable
         MOV ECX, [PackedSize]
         MOV EDX, [Source]
         MOV EDX, [EDX]
  @@1:
         MOV AL, [EDX]
         XLAT                     // (Only) Delphi 6 needs XLATB here.
         MOV [EDX], AL
         INC EDX
         DEC ECX
         JNZ @@1
         POP EBX
  end;

  // setup initial states
  // a row always starts with a (possibly zero-length) white run
  FSource := Source;
  FBitsLeft := 0;
  FPackedSize := PackedSize;

  // target preparation
  FTarget := Dest;
  FRestWidth := FWidth;
  FFreeTargetBits := 8;
  EOLCount := 0;

  // main loop
  repeat
    // synchronize to start of next line
    SynchBOL;
    // a line always starts with a white run
    FIsWhite := True;
    // decode one line
    repeat
      if FIsWhite then RunLength := FindWhiteCode
                  else RunLength := FindBlackCode;
      if RunLength >= 0 then
      begin
        if FillRun(RunLength) then Break;
        FIsWhite := not FIsWhite;
      end    
      else
        if RunLength = G3_EOL then Inc(EOLCount)
                              else Break;
    until (RunLength = G3_EOL) or (FPackedSize = 0);
    AdjustEOL;
  until (FPackedSize = 0) or (FTarget - PChar(Dest) >= UnpackedSize);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCCITTFax3Decoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin

end;

//----------------- TCCITTMHDecoder ------------------------------------------------------------------------------------

procedure TCCITTMHDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

var
  RunLength: Integer;

  //--------------- local functions -------------------------------------------

  procedure AdjustEOL;

  begin
    FIsWhite := False;
    if FFreeTargetBits in [1..7] then Inc(FTarget);
    FFreeTargetBits := 8;
    FRestWidth := FWidth;
    if FBitsLeft < 8 then FBitsLeft := 0; // discard remaining bits
    if FWordAligned and Odd(Cardinal(FTarget)) then Inc(FTarget);
  end;

  //--------------- end local functions ---------------------------------------

begin
  // make all bits white
  FillChar(Dest^, UnpackedSize, 0);

  // swap all bits here, in order to avoid frequent tests in the main loop
  if FSwapBits then 
  asm
         PUSH EBX
         LEA EBX, ReverseTable
         MOV ECX, [PackedSize]
         MOV EDX, [Source]
         MOV EDX, [EDX]
  @@1:
         MOV AL, [EDX]
         XLAT                     // (Only) Delphi 6 needs XLATB here.
         MOV [EDX], AL
         INC EDX
         DEC ECX
         JNZ @@1
         POP EBX
  end;

  // setup initial states
  // a row always starts with a (possibly zero-length) white run
  FIsWhite := True;
  FSource := Source;
  FBitsLeft := 0;
  FPackedSize := PackedSize;

  // target preparation
  FTarget := Dest;
  FRestWidth := FWidth;
  FFreeTargetBits := 8;

  // main loop
  repeat
    if FIsWhite then RunLength := FindWhiteCode
                else RunLength := FindBlackCode;
    if RunLength > 0 then
      if FillRun(RunLength) then AdjustEOL;
    FIsWhite := not FIsWhite;
  until FPackedSize = 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCCITTMHDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin

end;

//----------------- TLZ77Decoder ---------------------------------------------------------------------------------------

constructor TLZ77Decoder.Create(FlushMode: Integer; AutoReset: Boolean);

begin
  FillChar(FStream, SizeOf(FStream), 0);
  FFlushMode := FlushMode;
  FAutoReset := AutoReset;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TLZ77Decoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

begin
  FStream.NextInput := Source;
  FStream.AvailableInput := PackedSize;
  if FAutoReset then FZLibResult := InflateReset(FStream);
  if FZLibResult = Z_OK then
  begin
    FStream.NextOutput := Dest;
    FStream.AvailableOutput := UnpackedSize;
    FZLibResult := Inflate(FStream, FFlushMode);
    // advance pointers so used input can be calculated
    Source := FStream.NextInput;
    Dest := FStream.NextOutput;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TLZ77Decoder.DecodeEnd;

begin
  if InflateEnd(FStream) < 0 then CompressionError(gesLZ77Error);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TLZ77Decoder.DecodeInit;

begin
  if InflateInit(FStream) < 0 then CompressionError(gesLZ77Error);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TLZ77Decoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin

end;

//----------------------------------------------------------------------------------------------------------------------

function TLZ77Decoder.GetAvailableInput: Integer;

begin
  Result := FStream.AvailableInput;
end;

//----------------------------------------------------------------------------------------------------------------------

function TLZ77Decoder.GetAvailableOutput: Integer;

begin
  Result := FStream.AvailableOutput;
end;

//----------------- TTIFFJPEGDecoder ---------------------------------------------------------------------------------------


// Libjpeg interface layer needed to provide access from the JPEG coder class.

// This routine is invoked only for warning messages, since error_exit does its own thing
// and trace_level is never set > 0.

procedure Internaljpeg_output_message(cinfo: j_common_ptr);

var
  Buffer: array[0..JMSG_LENGTH_MAX] of Char;
  State: PJPEGState;

begin
  State := Pointer(cinfo);
	State.Error.format_message(@State.General.common, Buffer);
  MessageBox(0, Buffer, PChar(gesWarning), MB_OK or MB_ICONWARNING);
end;

//----------------------------------------------------------------------------------------------------------------------
{
procedure Internaljpeg_create_compress(var State: TJPEGState);

begin
	// initialize JPEG error handling
  State.General.Common.err := @State.Error;
	State.Error.output_message := Internaljpeg_output_message;

	jpeg_createCompress(@State.General.c, JPEG_LIB_VERSION, SizeOf(State.General.c));
end;
}
//----------------------------------------------------------------------------------------------------------------------

// JPEG library source data manager. These routines supply compressed data to libjpeg.

procedure std_init_source(cinfo: j_decompress_ptr); 

var
  State: PJPEGState;

begin
  State := Pointer(cinfo);

	State.SourceManager.next_input_byte := State.RawBuffer;
	State.SourceManager.bytes_in_buffer := State.RawBufferSize;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure std_fill_input_buffer(cinfo: j_decompress_ptr); 

const
  Dummy_EOI: array[0..1] of JOCTET = ($FF, JPEG_EOI);

var
  State: PJPEGState;

begin
  State := Pointer(cinfo);

	// Should never get here since entire strip/tile is read into memory before the
  // decompressor is called, and thus was supplied by init_source.
	MessageBox(0, PChar(gesJPEGEOI), PChar(gesWarning), MB_OK or MB_ICONWARNING);

	// insert a fake EOI marker
	State.SourceManager.next_input_byte := @Dummy_EOI;
	State.SourceManager.bytes_in_buffer := 2;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure std_skip_input_data(cinfo: j_decompress_ptr; num_bytes: Integer); 

var
  State: PJPEGState;

begin
  State := Pointer(cinfo);

	if num_bytes > 0 then
  begin
		if num_bytes > State.SourceManager.bytes_in_buffer then
    begin
			// oops, buffer overrun
			std_fill_input_buffer(cinfo);
		end
    else
    begin
			Inc(State.SourceManager.next_input_byte, num_bytes);
			Dec(State.SourceManager.bytes_in_buffer, num_bytes);
		end;
	end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure std_term_source(cinfo: j_decompress_ptr); 

// No work necessary here.

begin
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Internaljpeg_data_src(var State: TJPEGState);

begin
  with State do
  begin
    // set data source manager
    General.d.src := @SourceManager;
    
    // fill in fields in our data source manager
    SourceManager.init_source := @std_init_source;
    SourceManager.fill_input_buffer := @std_fill_input_buffer;
    SourceManager.skip_input_data := @std_skip_input_data;
    SourceManager.resync_to_restart := @jpeg_resync_to_restart;
    SourceManager.term_source := @std_term_source;
    SourceManager.bytes_in_buffer := 0;		// for safety
    SourceManager.next_input_byte := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

// Alternate source manager for reading from JPEGTables.
// We can share all the code except for the init routine.

procedure tables_init_source(cinfo: j_decompress_ptr);

var
  State: PJPEGState;

begin
  State := Pointer(cinfo);

	State.SourceManager.next_input_byte := State.JPEGTables;
	State.SourceManager.bytes_in_buffer := State.JTLength;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Internaljpeg_tables_src(var State: TJPEGState);

begin
	Internaljpeg_data_src(State);
	State.SourceManager.init_source := @tables_init_source;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TTIFFJPEGDecoder.Create(Properties: Pointer);

begin
  FImageProperties := Properties;
  with PImageProperties(Properties)^ do
  begin
    if Assigned(JPEGTables) then
    begin
      FState.JPEGTables := @JPEGTables[0];
      FState.JTLength := Length(JPEGTables);
    end;
    // no else branch, rely on class initialization
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTIFFJPEGDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

type
  PCompInfoArray = ^TCompInfoArray;
  TCompInfoArray = array[0..MAX_COMPONENTS - 1] of jpeg_component_info;

const
  // also defined in GraphicEx, but not publicitly
  PLANARCONFIG_CONTIG = 1;
  PLANARCONFIG_SEPARATE = 2;

var
	I, J: Integer;
  SegmentWidth,
  SegmentHeight: Cardinal;
  Temp: Integer;
  Target: PByte;
  
begin
	// Reset decoder state from any previous strip/tile, in case application didn't read the whole strip.
	jpeg_abort(@FState.General.Common);

  FState.RawBuffer := Source;
  FState.RawBufferSize := PackedSize;
	// Read the header for this strip/tile.
	jpeg_read_header(@FState.General, True);

  with PImageProperties(FImageProperties)^ do
  begin
    // Check image parameters and set decompression parameters.
    if ioTiled in Options then
    begin
      // tiled images currently not supported
      SegmentWidth := TileWidth;
      SegmentHeight := Height;
      BytesPerLine := 0; //TIFFTileRowSize(tif);
    end
    else
    begin
      SegmentWidth := Width;
      SegmentHeight := Height - CurrentRow;
      // I assume here that all strips are equally sized
      if SegmentHeight > RowsPerStrip[0] then SegmentHeight := RowsPerStrip[0];
    end;

    FState.BytesPerLine := BytesPerLine;
    
    if (PlanarConfig = PLANARCONFIG_SEPARATE) and (CurrentStrip = StripCount) then
    begin
      // For PC 2, scale down the expected strip/tile size to match a downsampled component
      SegmentWidth := (SegmentWidth + Cardinal(FState.HSampling - 1)) div FState.HSampling;
      SegmentHeight := (SegmentHeight + Cardinal(FState.VSampling - 1)) div FState.VSampling;
    end;

    if (FState.General.d.image_width <> SegmentWidth) or
       (FState.General.d.image_height <> SegmentHeight) then CompressionError(gesJPEGStripSize);

    Temp := 1;
    if PlanarConfig = PLANARCONFIG_CONTIG then Temp := SamplesPerPixel;
    if FState.General.d.num_components <> Temp then CompressionError(gesJPEGComponentCount);

    if FState.General.d.data_precision <> BitsPerSample then CompressionError(gesJPEGDataPrecision);

    if PlanarConfig = PLANARCONFIG_CONTIG then
    begin
      // component 0 should have expected sampling factors 
      if (FState.General.d.comp_info.h_samp_factor <> FState.HSampling) or
         (FState.General.d.comp_info.v_samp_factor <> FState.VSampling) then
        CompressionError(gesJPEGSamplingFactors);

      // rest should have sampling factors 1,1
      for I := 1 to FState.General.d.num_components - 1 do
        with PCompInfoArray(FState.General.d.comp_info)[I] do
        begin
          if (h_samp_factor <> 1) or (v_samp_factor <> 1) then
            CompressionError(gesJPEGSamplingFactors);
        end;
    end
    else
    begin
      // PC 2's single component should have sampling factors 1,1
      if (FState.General.d.comp_info.h_samp_factor <> 1) or
         (FState.General.d.comp_info.v_samp_factor <> 1) then
        CompressionError(gesJPEGSamplingFactors);
    end;
  
    // Since libjpeg can convert YCbCr data to RGB (actually BGR) on the fly I let do
    // it this conversion instead handling it by the color manager.
    if ColorScheme = csYCbCr then FState.General.d.jpeg_color_space := JCS_YCbCr
                             else FState.General.d.jpeg_color_space := JCS_UNKNOWN;
    FState.General.d.out_color_space := JCS_RGB;

    FState.General.d.raw_data_out := False;

    // Start JPEG decompressor
    jpeg_start_decompress(@FState.General);

    try
      Target := Dest;
      // data is expected to be read in multiples of a scanline
      J := Cardinal(UnpackedSize) div FState.BytesPerLine;
      if (Cardinal(UnpackedSize) mod FState.BytesPerLine) <> 0 then
        CompressionError(gesJPEGFractionalLine);

      while J > 0 do
      begin
        // jpeg_read_scanlines needs as target an array of pointers, but since we read only one lin
        // at a time we can simply pass the address of the pointer to the data
        if jpeg_read_scanlines(@FState.General.d, @Target, 1) <> 1 then Exit;
        Inc(Target, FState.BytesPerLine);
        Dec(J);
      end;
    finally
      jpeg_finish_decompress(@FState.General.d);
    end
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTIFFJPEGDecoder.DecodeEnd;

begin
  // release libjpeg resources
  jpeg_destroy(@FState.General.Common);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTIFFJPEGDecoder.DecodeInit;

begin
	// initialize JPEG error handling
  FState.Error := jpeg_std_error;
	FState.General.d.common.err := @FState.Error;
	FState.Error.output_message := Internaljpeg_output_message;

  // let JPEG library init the core structure before setting our own stuff
	jpeg_createDecompress(@FState.General.d, JPEG_LIB_VERSION, SizeOf(FState.General.d));

  with PImageProperties(FImageProperties)^ do
  begin
    if {(ColorScheme = csYCbCr) and} Assigned(YCbCrSubsampling) then
    begin
		  FState.HSampling := YCbCrSubsampling[0];
		  FState.VSampling := YCbCrSubsampling[1];
    end
    else
    begin
		  // TIFF 6.0 forbids subsampling of all other color spaces
		  FState.HSampling := 1;
		  FState.VSampling := 1;
    end;
  end;

  // default values for codec-specific fields
  with FState do
  begin
    // Default IJG quality
    JPEGQuality := 75;
  end;

  if Assigned(FState.JPEGTables) then
  begin
    Internaljpeg_tables_src(FState);
    if jpeg_read_header(@FState.General, False) <> JPEG_HEADER_TABLES_ONLY then
      CompressionError(gesJPEGBogusTableField);
  end;
  
  Internaljpeg_data_src(FState);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTIFFJPEGDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin
end;

//----------------- TThunderDecoder ------------------------------------------------------------------------------------

// ThunderScan uses an encoding scheme designed for 4-bit pixel values.  Data is encoded in bytes, with
// each byte split into a 2-bit code word and a 6-bit data value.  The encoding gives raw data, runs of
// pixels, or pixel values encoded as a delta from the previous pixel value.  For the latter, either 2-bit
// or 3-bit delta values are used, with the deltas packed into a single byte.

const
  THUNDER_DATA = $3F;       // mask for 6-bit data
  THUNDER_CODE = $C0;       // mask for 2-bit code word
    // code values
  THUNDER_RUN = 0;          // run of pixels w/ encoded count
  THUNDER_2BITDELTAS = $40;	// 3 pixels w/ encoded 2-bit deltas
    DELTA2_SKIP = 2;        // skip code for 2-bit deltas
  THUNDER_3BITDELTAS = $80; // 2 pixels w/ encoded 3-bit deltas
    DELTA3_SKIP = 4;        // skip code for 3-bit deltas
  THUNDER_RAW = $C0;        // raw data encoded

  TwoBitDeltas: array[0..3] of Integer = (0, 1, 0, -1);
  ThreeBitDeltas: array[0..7] of Integer = (0, 1, 2, 3, 0, -3, -2, -1);

constructor TThunderDecoder.Create(Width: Cardinal);

begin
  FWidth := Width;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThunderDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

var
  SourcePtr,
  TargetPtr: PByte;
  N, Delta: Integer;
  NPixels: Cardinal;
  LastPixel: Integer;

  //--------------- local function --------------------------------------------

  procedure SetPixel(Delta: Integer);

  begin
    Lastpixel := Delta and $0F;
    if Odd(NPixels) then
    begin
      TargetPtr^ := TargetPtr^ or LastPixel;
      Inc(TargetPtr);
    end
    else TargetPtr^ := LastPixel shl 4;

    Inc(NPixels);
  end;

  //--------------- end local function ----------------------------------------

begin
  SourcePtr := Source;
  TargetPtr := Dest;
  while UnpackedSize > 0 do
  begin
    LastPixel := 0;
    NPixels := 0;
    // Usually Width represents the byte number of a strip, but the thunder
    // algo is only defined for 4 bits per pixel formats where 2 pixels take up
    // one byte.
    while (PackedSize > 0) and (NPixels < 2 * FWidth) do
    begin
      N := SourcePtr^;
      Inc(SourcePtr);
      Dec(PackedSize);
      case N and THUNDER_CODE of
        THUNDER_RUN:
          // pixel run, replicate the last pixel n times, where n is the lower-order 6 bits
          begin
            if Odd(NPixels) then
            begin
              TargetPtr^ := TargetPtr^ or Lastpixel;
              Lastpixel := TargetPtr^;
              Inc(TargetPtr);
              Inc(NPixels);
              Dec(N);
            end
            else LastPixel := LastPixel or LastPixel shl 4;

            Inc(NPixels, N);
            while N > 0 do
            begin
              TargetPtr^ := LastPixel;
              Inc(TargetPtr);
              Dec(N, 2);
            end;

            if N = -1 then
            begin
              Dec(TargetPtr);
              TargetPtr^ := TargetPtr^ and $F0;
            end;

            LastPixel := LastPixel and $0F;
          end;
        THUNDER_2BITDELTAS: // 2-bit deltas
          begin
            Delta := (N shr 4) and 3;
            if Delta <> DELTA2_SKIP then SetPixel(LastPixel + TwoBitDeltas[Delta]);
            Delta := (N shr 2) and 3;
            if Delta <> DELTA2_SKIP then SetPixel(LastPixel + TwoBitDeltas[Delta]);
            Delta := N and 3;
            if Delta <> DELTA2_SKIP then SetPixel(LastPixel + TwoBitDeltas[Delta]);
          end;
        THUNDER_3BITDELTAS: // 3-bit deltas
          begin
            Delta := (N shr 3) and 7;
            if Delta <> DELTA3_SKIP then SetPixel(LastPixel + ThreeBitDeltas[Delta]);
            Delta := N and 7;
            if Delta <> DELTA3_SKIP then SetPixel(LastPixel + ThreeBitDeltas[Delta]);
          end;
        THUNDER_RAW: // raw data
          SetPixel(N);
      end;
    end;

    Dec(UnpackedSize, FWidth);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThunderDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin
end;

//----------------- TPCDDecoder ----------------------------------------------------------------------------------------

constructor TPCDDecoder.Create(Stream: TStream);

begin
  FStream := Stream;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPCDDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

// recovers the Huffman encoded luminance and chrominance deltas
// Note: This decoder leaves a bit the way like the other decoders work.
//       Source points to an array of 3 pointers, one for luminance (Y, Luma), one for blue
//       chrominance (Cb, Chroma1) and one for red chrominance (Cr, Chroma2). These pointers
//       point to source and target at the same time (in place decoding).
//       PackedSize contains the width of the current subimage and UnpackedSize its height.
//       Dest is not used and can be nil.

type
  PPointerArray = ^TPointerArray;
  TPointerArray = array[0..2] of Pointer;

  PPCDTable = ^TPCDTable;
  TPCDTable = record
    Length: Byte;
    Sequence: Cardinal;
    Key: Byte;
    Mask: Integer;
  end;

  PQuantumArray = ^TQuantumArray;
  TQuantumArray = array[0..3 * 256 - 1] of Byte;

var
  Luma,
  Chroma1,
  Chroma2: PChar; // hold the actual pointers, PChar to easy pointer maths
  Width,
  Height: Cardinal;

  PCDTable: array[0..2] of PPCDTable;
  I, J, K: Cardinal;
  R: PPCDTable;
  RangeLimit: PQuantumArray;
  P, Q,
  Buffer: PChar;
  Accumulator,
  Bits,
  Length,
  Plane,
  Row: Cardinal;
  PCDLength: array[0..2] of Cardinal;

  //--------------- local function --------------------------------------------

  procedure PCDGetBits(N: Cardinal);

  begin
    Accumulator := Accumulator shl N;
    Dec(Bits, N);
    while Bits <= 24 do
    begin
      if P >= (Buffer + $800) then
      begin
        FStream.ReadBuffer(Buffer^, $800);
        P := Buffer;
      end;
      Accumulator := Accumulator or (Cardinal(P^) shl (24 - Bits));
      Inc(Bits, 8);
      Inc(P);
    end;
  end;

  //--------------- end local function ----------------------------------------

var
  Limit: Cardinal;
  
begin
  // place the used source values into local variables with proper names to make
  // their usage clearer
  Luma := PPointerArray(Source)[0];
  Chroma1 := PPointerArray(Source)[1];
  Chroma2 := PPointerArray(Source)[2];
  Width := PackedSize;
  Height := UnpackedSize;
  
  // initialize Huffman tables
  ZeroMemory(@PCDTable, SizeOf(PCDTable));
  GetMem(Buffer, $800);
  try
    Accumulator := 0;
    Bits := 32;
    P := Buffer + $800;
    Limit := 1;
    if Width > 1536 then Limit := 3;
    for I := 0 to Limit - 1 do
    begin
      PCDGetBits(8);
      Length := (Accumulator and $FF) + 1;
      GetMem(PCDTable[I], Length * SizeOf(TPCDTable));

      R := PCDTable[I];
      for J := 0 to Length - 1 do
      begin
        PCDGetBits(8);
        R.Length := (Accumulator and $FF) + 1;
        if R.Length > 16 then
        begin
          if Assigned(Buffer) then FreeMem(Buffer);
          for K := 0 to 2 do
            if Assigned(PCDTable[K]) then FreeMem(PCDTable[K]);
          Exit;
        end;
        PCDGetBits(16);
        R.Sequence := (Accumulator and $FFFF) shl 16;
        PCDGetBits(8);
        R.Key := Accumulator and $FF;
        asm
          // R.Mask := not ((1 shl (32 - R.Length)) - 1);
          // asm implementation to avoid overflow errors and for faster execution
          MOV EDX, [R]
          MOV CL, 32
          SUB CL, [EDX].TPCDTable.Length
          MOV EAX, 1
          SHL EAX, CL
          DEC EAX
          NOT EAX
          MOV [EDX].TPCDTable.Mask, EAX
        end;
        Inc(R);
      end;
      PCDLength[I] := Length;
    end;

    // initialize range limits
    GetMem(RangeLimit, 3 * 256);
    try
      for I := 0 to 255 do
      begin
        RangeLimit[I] := 0;
        RangeLimit[I + 256] := I;
        RangeLimit[I + 2 * 256] := 255;
      end;
      Inc(PByte(RangeLimit), 255);

      // search for sync byte
      PCDGetBits(16);
      PCDGetBits(16);
      while (Accumulator and $00FFF000) <> $00FFF000 do PCDGetBits(8);
      while (Accumulator and $FFFFFF00) <> $FFFFFE00 do PCDGetBits(1);

      // recover the Huffman encoded luminance and chrominance deltas
      Length := 0;
      Plane := 0;
      Q := Luma;
      repeat
        if (Accumulator and $FFFFFF00) = $FFFFFE00 then
        begin
          // determine plane and row number
          PCDGetBits(16);
          Row := (Accumulator shr 9) and $1FFF;
          if Row = Height then Break;
          PCDGetBits(8);
          Plane := Accumulator shr 30;
          PCDGetBits(16);
          case Plane of
            0:
              Q := Luma + Row * Width;
            2:
              begin
                Q := Chroma1 + (Row shr 1) * Width;
                Dec(Plane);
              end;
            3:
              begin
                Q := Chroma2 + (Row shr 1) * Width;
                Dec(Plane);
              end;
          else
            Abort; // invalid/corrupt image
          end;

          Length := PCDLength[Plane];
          Continue;
        end;

        // decode luminance or chrominance deltas
        R := PCDTable[Plane];
        I := 0;
        while (I < Length) and ((Accumulator and R.Mask) <> R.Sequence) do
        begin
          Inc(I);
          Inc(R);
        end;
      
        if R = nil then
        begin
          // corrupt PCD image, skipping to sync byte
          while (Accumulator and $00FFF000) <> $00FFF000 do PCDGetBits(8);
          while (Accumulator and $FFFFFF00) <> $FFFFFE00 do PCDGetBits(1);
          Continue;
        end;

        if R.Key < 128 then Q^ := Char(RangeLimit[ClampByte(Byte(Q^) + R.Key)])
                       else Q^ := Char(RangeLimit[ClampByte(Byte(Q^) + R.Key - 256)]);
        Inc(Q);
        PCDGetBits(R.Length);
      until False;                                     
    finally
      for I := 0 to 2 do
        if Assigned(PCDTable[I]) then FreeMem(PCDTable[I]);
      Dec(PByte(RangeLimit), 255);
      if Assigned(RangeLimit) then FreeMem(RangeLimit);
    end;
  finally
    if Assigned(Buffer) then FreeMem(Buffer);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPCDDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin
end;

//----------------------------------------------------------------------------------------------------------------------

end.

