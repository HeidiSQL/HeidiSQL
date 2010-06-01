unit GraphicEx;

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
//
// GraphicEx -
//   This unit is an addendum to Graphics.pas, in order to enable your application
//   to import many common graphic files.
//
// See help file for a description of supported image types. Additionally, there is a resample routine
// (Stretch) based on code from Anders Melander (http://www.melander.dk/delphi/resampler/index.html)
// which has been optimized quite a lot to work faster and bug fixed.
//
// version - 9.9
//
// 03-SEP-2000 ml:
//   EPS with alpha channel, workaround for TIFs with wrong alpha channel indication,
//   workaround for bad packbits compressed (TIF) images
// 28-AUG-2000 ml:
//   small bugfixes
// 27-AUG-2000 ml:
//   changed all FreeMemory(P) calls back to ... if Assigned(P) then FreeMem(P); ...
// 24-AUG-2000 ml:
//   small bug in LZ77 decoder removed
// 18-AUG-2000 ml:
//   TIF deflate decoding scheme
// 15-AUG-2000 ml:
//   workaround for TIF images without compression, but prediction scheme set (which is not really used in this case)
// 12-AUG-2000 ml:
//   small changes 
//
// For older history please look into the help file.
//
// Note: The library provides usually only load support for the listed image formats but will perhaps be enhanced
//       in the future to save those types too. It can be compiled with Delphi 4 or newer versions.
//
//----------------------------------------------------------------------------------------------------------------------

interface

{$I GraphicConfiguration.inc}

uses
  Windows, Classes, ExtCtrls, Graphics, SysUtils, JPEG,
  GraphicCompression, GraphicStrings, GraphicColor;

type
  TCardinalArray = array of Cardinal;
  TByteArray = array of Byte;
  TFloatArray = array of Single;

  TImageOptions = set of (
    ioTiled,       // image consists of tiles not strips (TIF)
    ioBigEndian,   // byte order in values >= words is reversed (TIF, RLA, SGI)
    ioMinIsWhite,  // minimum value in grayscale palette is white not black (TIF)
    ioReversed,    // bit order in bytes is reveresed (TIF)
    ioUseGamma     // gamma correction is used
  );

  // describes the compression used in the image file
  TCompressionType = (
    ctUnknown,     // compression type is unknown
    ctNone,        // no compression at all
    ctRLE,         // run length encoding
    ctPackedBits,  // Macintosh packed bits
    ctLZW,         // Lempel-Zif-Welch
    ctFax3,        // CCITT T.4 (1d), also known as fax group 3
    ctFaxRLE,      // modified Huffman (CCITT T.4 derivative)
    ctFax4,        // CCITT T.6, also known as fax group 4
    ctFaxRLEW,     // CCITT T.4 with word alignment
    ctLZ77,        // Hufman inflate/deflate
    ctJPEG,        // TIF JPEG compression (new version)
    ctOJPEG,       // TIF JPEG compression (old version)
    ctThunderscan, // TIF thunderscan compression
    ctNext,
    ctIT8CTPAD,
    ctIT8LW,
    ctIT8MP,
    ctIT8BL,
    ctPixarFilm,
    ctPixarLog,
    ctDCS,
    ctJBIG,
    ctPCDHuffmann  // PhotoCD Hufman compression
  );

  // properties of a particular image which are set while loading an image or when
  // they are explicitly requested via ReadImageProperties
  PImageProperties = ^TImageProperties;
  TImageProperties = record
    Version: Cardinal;                 // TIF, PSP, GIF
    Options: TImageOptions;            // all images
    Width,                             // all images
    Height: Cardinal;                  // all images
    ColorScheme: TColorScheme;         // all images
    BitsPerSample,                     // all Images
    SamplesPerPixel,                   // all images
    BitsPerPixel: Byte;                // all images
    Compression: TCompressionType;     // all images
    FileGamma: Single;                 // RLA, PNG
    XResolution,
    YResolution: Single;               // given in dpi (TIF, PCX, PSP)
    Interlaced,                        // GIF, PNG
    HasAlpha: Boolean;                 // TIF, PNG

    // informational data, used internally and/or by decoders
    // TIF
    FirstIFD,
    PlanarConfig,                      // most of this data is needed in the JPG decoder
    CurrentRow,
    TileWidth,
    TileLength,
    BytesPerLine: Cardinal;
    RowsPerStrip: TCardinalArray;
    YCbCrSubSampling,
    JPEGTables: TByteArray;
    JPEGColorMode,
    JPEGTablesMode: Cardinal;
    CurrentStrip,
    StripCount,
    Predictor: Integer;

    // PCD
    Overview: Boolean;                 // true if image is an overview image
    Rotate: Byte;                      // describes how the image is rotated (aka landscape vs. portrait image)
    ImageCount: Word;                  // number of subimages if this is an overview image

    // GIF
    LocalColorTable: Boolean;          // image uses an own color palette instead of the global one

    // RLA
    BottomUp: Boolean;                 // images is bottom to top

    // PSD
    Channels: Byte;                    // up to 24 channels per image

    // PNG
    FilterMode: Byte;                 
  end;

  // This is the general base class for all image types implemented in GraphicEx.
  // It contains some generally used class/data.
  TGraphicExGraphic = class(TBitmap)
  private
    FColorManager: TColorManager;
    FImageProperties: TImageProperties;
    FBasePosition: Cardinal;  // stream start position
    FStream: TStream;         // used for local references of the stream the class is currently loading from
    FProgressRect: TRect;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    class function CanLoad(const FileName: String): Boolean; overload; virtual;
    class function CanLoad(Stream: TStream): Boolean; overload; virtual; 
    procedure LoadFromResourceName(Instance: THandle; const ResName: String);
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer);
    function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; virtual;

    property ColorManager: TColorManager read FColorManager;
    property ImageProperties: TImageProperties read FImageProperties write FImageProperties;
  end;

  TGraphicExGraphicClass = class of TGraphicExGraphic;
   
  {$ifdef SGIGraphic}
  // *.bw, *.rgb, *.rgba, *.sgi images
  TSGIGraphic = class(TGraphicExGraphic)
  private
    FRowStart,
    FRowSize: TCardinalArray;    // start and length of a line (if compressed)
    FDecoder: TDecoder;          // ...same applies here
    procedure ReadAndDecode(Red, Green, Blue, Alpha: Pointer; Row, BPC: Cardinal);
  public
    class function CanLoad(Stream: TStream): Boolean; override;
    procedure LoadFromStream(Stream: TStream); override;
    function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif}

  {$ifdef AutodeskGraphic}
  // *.cel, *.pic images
  TAutodeskGraphic = class(TGraphicExGraphic)
  public
    class function CanLoad(Stream: TStream): Boolean; override;
    procedure LoadFromStream(Stream: TStream); override;
    function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif}

  {$ifdef TIFFGraphic}
  // *.tif, *.tiff images
  // one entry in a an IFD (image file directory)
  TIFDEntry = packed record
    Tag: Word;
    DataType: Word;
    DataLength: Cardinal;
    Offset: Cardinal;
  end;

  TTIFFPalette = array[0..787] of Word;

  TTIFFGraphic = class(TGraphicExGraphic)
  private
    FIFD: array of TIFDEntry; // the tags of one image file directory
    FPalette: TTIFFPalette;
    FYCbCrPositioning: Cardinal;
    FYCbCrCoefficients: TFloatArray;
    function FindTag(Tag: Cardinal; var Index: Cardinal): Boolean;
    procedure GetValueList(Stream: TStream; Tag: Cardinal; var Values: TByteArray); overload;
    procedure GetValueList(Stream: TStream; Tag: Cardinal; var Values: TCardinalArray); overload;
    procedure GetValueList(Stream: TStream; Tag: Cardinal; var Values: TFloatArray); overload;
    function GetValue(Stream: TStream; Tag: Cardinal; Default: Single = 0): Single; overload;
    function GetValue(Tag: Cardinal; Default: Cardinal = 0): Cardinal; overload;
    function GetValue(Tag: Cardinal; var Size: Cardinal; Default: Cardinal = 0): Cardinal; overload;
    procedure SortIFD;
    procedure SwapIFD;
  public
    class function CanLoad(Stream: TStream): Boolean; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; override;
  end;

    {$ifdef EPSGraphic}
    TEPSGraphic = class(TTIFFGraphic)
    public
      class function CanLoad(Stream: TStream): Boolean; override;
      procedure LoadFromStream(Stream: TStream); override;
      function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; override;
    end;
    {$endif} // EPSGraphic
  {$endif} // TIFFGraphic

  {$ifdef TargaGraphic}
  // *.tga; *.vst; *.icb; *.vda; *.win images
  TTargaGraphic = class(TGraphicExGraphic)
   public
    class function CanLoad(Stream: TStream): Boolean; override;
    procedure LoadFromStream(Stream: TStream); override;
    function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; override;
    procedure SaveToStream(Stream: TStream); overload; override;
    procedure SaveToStream(Stream: TStream; Compressed: Boolean); reintroduce; overload;
  end;
  {$endif}

  {$ifdef PCXGraphic}
  // *.pcx; *.pcc; *.scr images
  // Note: Due to the badly designed format a PCX/SCR file cannot be part in a larger stream because the position of the
  //       color palette as well as the decoding size can only be determined by the size of the image.
  //       Hence the image must be the only one in the stream or the last one.
  TPCXGraphic = class(TGraphicExGraphic)
  public
    class function CanLoad(Stream: TStream): Boolean; override;
    procedure LoadFromStream(Stream: TStream); override;
    function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif}

  {$ifdef PCDGraphic}
  // *.pcd images
  // Note: By default the BASE resolution of a PCD image is loaded with LoadFromStream. 
  TPCDGraphic = class(TGraphicExGraphic)
  public
    class function CanLoad(Stream: TStream): Boolean; override;
    procedure LoadFromStream(Stream: TStream); override;
    function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif}

  {$ifdef PortableMapGraphic}
  // *.ppm, *.pgm, *.pbm images
  TPPMGraphic = class(TGraphicExGraphic)
  private
    FBuffer: array[0..4095] of Char;
    FIndex: Integer;
    function CurrentChar: Char;
    function GetChar: Char;
    function GetNumber: Cardinal;
    function ReadLine: String;
  public
    class function CanLoad(Stream: TStream): Boolean; override;
    procedure LoadFromStream(Stream: TStream); override;
    function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif}

  {$ifdef CUTGraphic}
  // *.cut (+ *.pal) images
  // Note: Also this format should not be used in a stream unless it is the only image or the last one!
  TCUTGraphic = class(TGraphicExGraphic)
  private
    FPaletteFile: String;
  protected
    procedure LoadPalette;
  public
    class function CanLoad(Stream: TStream): Boolean; override;
    procedure LoadFromFile(const FileName: String); override;
    procedure LoadFromStream(Stream: TStream); override;
    function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; override;

    property PaletteFile: String read FPaletteFile write FPaletteFile;
  end;
  {$endif}

  {$ifdef GIFGraphic}
  // *.gif images
  TGIFGraphic = class(TGraphicExGraphic)
  private
    function SkipExtensions: Byte;
  public
    class function CanLoad(Stream: TStream): Boolean; override;
    procedure LoadFromStream(Stream: TStream); override;
    function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif}

  {$ifdef RLAGraphic}
  // *.rla, *.rpf images
  // implementation based on code from Dipl. Ing. Ingo Neumann (ingo@upstart.de, ingo_n@dialup.nacamar.de)
  TRLAGraphic = class(TGraphicExGraphic)
  private
    procedure SwapHeader(var Header); // start position of the image header in the stream
  public
    class function CanLoad(Stream: TStream): Boolean; override;
    procedure LoadFromStream(Stream: TStream); override;
    function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif}

  {$ifdef PhotoshopGraphic}
  // *.psd, *.pdd images
  TPSDGraphic = class(TGraphicExGraphic)
  public
    class function CanLoad(Stream: TStream): Boolean; override;
    procedure LoadFromStream(Stream: TStream); override;
    function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif}

  {$ifdef PaintshopProGraphic}
  // *.psp images (file version 3 and 4)
  TPSPGraphic = class(TGraphicExGraphic)
  public
    class function CanLoad(Stream: TStream): Boolean; override;
    procedure LoadFromStream(Stream: TStream); override;
    function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif}

  {$ifdef PortableNetworkGraphic}
  // *.png images
  TChunkType = array[0..3] of Char;

  // This header is followed by a variable number of data bytes, which are followed by the CRC for this data.
  // The actual size of this data is given by field length in the chunk header.
  // CRC is Cardinal (4 byte unsigned integer).
  TPNGChunkHeader = packed record
    Length: Cardinal;  // size of data (entire chunk excluding itself, CRC and type)
    ChunkType: TChunkType;
  end;

  TPNGGraphic = class(TGraphicExGraphic)
  private
    FDecoder: TLZ77Decoder;
    FIDATSize: Integer;        // remaining bytes in the current IDAT chunk
    FRawBuffer,                // buffer to load raw chunk data and to check CRC
    FCurrentSource: Pointer;   // points into FRawBuffer for current position of decoding
    FHeader: TPNGChunkHeader;  // header of the current chunk
    FCurrentCRC: Cardinal;     // running CRC for the current chunk
    FSourceBPP: Integer;       // bits per pixel used in the file
    FPalette: HPALETTE;        // used to hold the palette handle until we can set it finally after the pixel format
                               // has been set too (as this destroys the current palette)
    FTransparency: TByteArray; // If the image is indexed then this array might contain alpha values (depends on file)
                               // each entry corresponding to the same palette index as the index in this array.
                               // For grayscale and RGB images FTransparentColor contains the (only) transparent
                               // color.
    FTransparentColor: TColor; // transparent color for gray and RGB
    FBackgroundColor: TColor;  // index or color ref
    procedure ApplyFilter(Filter: Byte; Line, PrevLine, Target: PByte; BPP, BytesPerRow: Integer);
    function IsChunk(ChunkType: TChunkType): Boolean;
    function LoadAndSwapHeader: Cardinal;
    procedure LoadBackgroundColor(const Description);
    procedure LoadIDAT(const Description);
    procedure LoadTransparency(const Description);
    procedure ReadDataAndCheckCRC;
    procedure ReadRow(RowBuffer: Pointer; BytesPerRow: Integer);
    function SetupColorDepth(ColorType, BitDepth: Integer): Integer;
  public
    class function CanLoad(Stream: TStream): Boolean; override;
    procedure LoadFromStream(Stream: TStream); override;
    function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; override;

    property BackgroundColor: TColor read FBackgroundColor;
    property Transparency: TByteArray read FTransparency;
  end;
  {$endif} // PortableNetworkGraphic

  // ---------- file format management stuff
  TFormatType = (
    ftAnimation,   // format contains an animation (like GIF or AVI)
    ftLayered,     // format supports multiple layers (like PSP, PSD)
    ftMultiImage,  // format can contain more than one image (like TIF or GIF)
    ftRaster,      // format is contains raster data (this is mainly used)
    ftVector       // format contains vector data (like DXF or PSP file version 4)
  );
  TFormatTypes = set of TFormatType;
  TFilterSortType = (
    fstNone,        // do not sort entries, list them as they are registered
    fstBoth,        // sort entries first by description then by extension
    fstDescription, // sort entries by description only
    fstExtension    // sort entries by extension only
  );

  TFilterOption = (
    foCompact,          // use the compact form in filter strings instead listing each extension on a separate line
    foIncludeAll,       // include the 'All image files' filter string
    foIncludeExtension  // add the extension to the description
  );
  TFilterOptions = set of TFilterOption;

  // The file format list is an alternative to Delphi's own poor implementation which does neither allow to filter
  // graphic formats nor to build common entries in filter strings nor does it care for duplicate entries or
  // alphabetic ordering. Additionally, some properties are maintained for each format to do searches, filter partiuclar
  // formats for a certain case etc.
  TFileFormatList = class
  private
    FClassList,
    FExtensionList: TList;
  protected
    function FindExtension(const Extension: String): Integer;
    function FindGraphicClass(GraphicClass: TGraphicClass): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function GetDescription(Graphic: TGraphicClass): String;
    procedure GetExtensionList(List: TStrings);
    function GetGraphicFilter(Formats: TFormatTypes; SortType: TFilterSortType; Options: TFilterOptions;
      GraphicClass: TGraphicClass): String;
    function GraphicFromExtension(S: String): TGraphicClass;
    function GraphicFromContent(const FileName: String): TGraphicExGraphicClass; overload;
    function GraphicFromContent(Stream: TStream): TGraphicExGraphicClass; overload;
    procedure RegisterFileFormat(const Extension, Common, Individual: String; FormatTypes: TFormatTypes;
      Replace, RegisterDefault: Boolean; GraphicClass: TGraphicClass);
    procedure UnregisterFileFormat(const Extension: String; GraphicClass: TGraphicClass);
  end;

  // resampling support types
  TResamplingFilter = (sfBox, sfTriangle, sfHermite, sfBell, sfSpline, sfLanczos3, sfMitchell);

// Resampling support routines
procedure Stretch(NewWidth, NewHeight: Cardinal; Filter: TResamplingFilter; Radius: Single; Source, Target: TBitmap); overload;
procedure Stretch(NewWidth, NewHeight: Cardinal; Filter: TResamplingFilter; Radius: Single; Source: TBitmap); overload;

var
  FileFormatList: TFileFormatList;
  
//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Consts, Math, MZLib;

type
  // resampling support types
  TRGBInt = record
   R, G, B: Integer;
  end;

  PRGBWord = ^TRGBWord;
  TRGBWord = record
   R, G, B: Word;
  end;

  PRGBAWord = ^TRGBAWord;
  TRGBAWord = record
   R, G, B, A: Word;
  end;

  PBGR = ^TBGR;
  TBGR = packed record
   B, G, R: Byte;
  end;

  PBGRA = ^TBGRA;
  TBGRA = packed record
   B, G, R, A: Byte;
  end;

  PRGB = ^TRGB;
  TRGB = packed record
   R, G, B: Byte;
  end;

  PRGBA = ^TRGBA;
  TRGBA = packed record
   R, G, B, A: Byte;
  end;

  PPixelArray = ^TPixelArray;
  TPixelArray = array[0..0] of TBGR;

  TFilterFunction = function(Value: Single): Single;

  // contributor for a Pixel
  PContributor = ^TContributor;
  TContributor = record
   Weight: Integer; // Pixel Weight
   Pixel: Integer; // Source Pixel
  end;

  TContributors = array of TContributor;

  // list of source pixels contributing to a destination pixel
  TContributorEntry = record
   N: Integer;
   Contributors: TContributors;
  end;

  TContributorList = array of TContributorEntry;

const
  DefaultFilterRadius: array[TResamplingFilter] of Single = (0.5, 1, 1, 1.5, 2, 3, 2);

threadvar // globally used cache for current image (speeds up resampling about 10%)
  CurrentLineR: array of Integer;
  CurrentLineG: array of Integer;
  CurrentLineB: array of Integer;

//----------------------------------------------------------------------------------------------------------------------

procedure GraphicExError(ErrorString: String); overload;

begin
  raise EInvalidGraphic.Create(ErrorString);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure GraphicExError(ErrorString: String; Args: array of const); overload;

begin
  raise EInvalidGraphic.CreateFmt(ErrorString, Args);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Upsample(Width, Height, ScaledWidth: Cardinal; Pixels: PChar);

// Creates a new image that is a integral size greater than an existing one.

var
  X, Y: Cardinal;
  P, Q, R: PChar;

begin
  for Y := 0 to Height - 1 do
  begin
    P := Pixels + (Height - 1 - Y) * ScaledWidth + (Width - 1);
    Q := Pixels + ((Height - 1 - Y) shl 1) * ScaledWidth + ((Width - 1) shl 1);
    Q^ := P^;
    (Q + 1)^ := P^;
    for X := 1 to Width - 1 do
    begin
      Dec(P);
      Dec(Q, 2);
      Q^ := P^;
      (Q + 1)^ := Char((Word(P^) + Word((P + 1)^) + 1) shr 1);
    end;
  end;            

  for Y := 0 to Height - 2 do
  begin
    P := Pixels + (Y shl 1) * ScaledWidth;
    Q := P + ScaledWidth;
    R := Q + ScaledWidth;
    for X := 0 to Width - 2 do
    begin
      Q^ := Char((Word(P^) + Word(R^) + 1) shr 1);
      (Q + 1)^ := Char((Word(P^) + Word((P + 2)^) + Word(R^) + Word((R + 2)^) + 2) shr 2);
      Inc(Q, 2);
      Inc(P, 2);
      Inc(R, 2);
    end;
    Q^ := Char((Word(P^) + Word(R^) + 1) shr 1);
    Inc(P);
    Inc(Q);
    Q^ := Char((Word(P^) + Word(R^) + 1) shr 1);
  end;
  P := Pixels + (2 * Height - 2) * ScaledWidth;
  Q := Pixels + (2 * Height - 1) * ScaledWidth;
  Move(P^, Q^, 2 * Width);
end;

//----------------- filter functions for stretching --------------------------------------------------------------------

function HermiteFilter(Value: Single): Single;

// f(t) = 2|t|^3 - 3|t|^2 + 1, -1 <= t <= 1

begin
  if Value < 0 then Value := -Value;
  if Value < 1 then Result := (2 * Value - 3) * Sqr(Value) + 1
               else Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function BoxFilter(Value: Single): Single;

// This filter is also known as 'nearest neighbour' Filter.

begin
  if (Value > -0.5) and (Value <= 0.5) then Result := 1
                                       else Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TriangleFilter(Value: Single): Single;

// aka 'linear' or 'bilinear' filter

begin
  if Value < 0 then Value := -Value;
  if Value < 1 then Result := 1 - Value
               else Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function BellFilter(Value: Single): Single;

begin
  if Value < 0 then Value := -Value;
  if Value < 0.5 then Result := 0.75 - Sqr(Value)
                 else
    if Value < 1.5 then
    begin
      Value := Value - 1.5;
      Result := 0.5 * Sqr(Value);
    end
    else Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function SplineFilter(Value: Single): Single;

// B-spline filter

var
  Temp: Single;

begin
  if Value < 0 then Value := -Value;
  if Value < 1 then
  begin
    Temp := Sqr(Value);
    Result := 0.5 * Temp * Value - Temp + 2 / 3;
  end
  else
    if Value < 2 then
    begin
      Value := 2 - Value;
      Result := Sqr(Value) * Value / 6;
    end
    else Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function Lanczos3Filter(Value: Single): Single;

  //--------------- local function --------------------------------------------

  function SinC(Value: Single): Single;

  begin
    if Value <> 0 then
    begin
      Value := Value * Pi;
      Result := Sin(Value) / Value;
    end
    else Result := 1;
  end;

  //---------------------------------------------------------------------------

begin
  if Value < 0 then Value := -Value;
  if Value < 3 then Result := SinC(Value) * SinC(Value / 3)
               else Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function MitchellFilter(Value: Single): Single;

const
  B = 1 / 3;
  C = 1 / 3;

var Temp: Single;

begin
  if Value < 0 then Value := -Value;
  Temp := Sqr(Value);
  if Value < 1 then
  begin
    Value := (((12 - 9 * B - 6 * C) * (Value * Temp))
             + ((-18 + 12 * B + 6 * C) * Temp)
             + (6 - 2 * B));
    Result := Value / 6;
  end
  else
    if Value < 2 then
    begin
      Value := (((-B - 6 * C) * (Value * Temp))
               + ((6 * B + 30 * C) * Temp)
               + ((-12 * B - 48 * C) * Value)
               + (8 * B + 24 * C));
      Result := Value / 6;
    end
    else Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

const
  FilterList: array[TResamplingFilter] of TFilterFunction = (
    BoxFilter,
    TriangleFilter,
    HermiteFilter,
    BellFilter,
    SplineFilter,
    Lanczos3Filter,
    MitchellFilter
  );

//----------------------------------------------------------------------------------------------------------------------

procedure FillLineChache(N, Delta: Integer; Line: Pointer);

var
  I: Integer;
  Run: PBGR;

begin
  Run := Line;
  for I := 0 to N - 1 do
  begin
    CurrentLineR[I] := Run.R;
    CurrentLineG[I] := Run.G;
    CurrentLineB[I] := Run.B;
    Inc(PByte(Run), Delta);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function ApplyContributors(N: Integer; Contributors: TContributors): TBGR;

var
  J: Integer;
  RGB: TRGBInt;
  Total,
  Weight: Integer;
  Pixel: Cardinal;
  Contr: ^TContributor;
    
begin
  RGB.R := 0;
  RGB.G := 0;
  RGB.B := 0;
  Total := 0;
  Contr := @Contributors[0];
  for J := 0 to N - 1 do
  begin
    Weight := Contr.Weight;
    Inc(Total, Weight);
    Pixel := Contr.Pixel;
    Inc(RGB.r, CurrentLineR[Pixel] * Weight);
    Inc(RGB.g, CurrentLineG[Pixel] * Weight);
    Inc(RGB.b, CurrentLineB[Pixel] * Weight);

    Inc(Contr);
  end;

  if Total = 0 then
  begin
    Result.R := ClampByte(RGB.R shr 8);
    Result.G := ClampByte(RGB.G shr 8);
    Result.B := ClampByte(RGB.B shr 8);
  end
  else
  begin
    Result.R := ClampByte(RGB.R div Total);
    Result.G := ClampByte(RGB.G div Total);
    Result.B := ClampByte(RGB.B div Total);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure DoStretch(Filter: TFilterFunction; Radius: Single; Source, Target: TBitmap);

// This is the actual scaling routine. Target must be allocated already with sufficient size. Source must
// contain valid data, Radius must not be 0 and Filter must not be nil.

var
  ScaleX,
  ScaleY: Single;  // Zoom scale factors
  I, J,
  K, N: Integer; // Loop variables
  Center: Single; // Filter calculation variables
  Width: Single;
  Weight: Integer;  // Filter calculation variables
  Left,
  Right: Integer; // Filter calculation variables
  Work: TBitmap;
  ContributorList: TContributorList;
  SourceLine,
  DestLine: PPixelArray;
  DestPixel: PBGR;
  Delta,
  DestDelta: Integer;
  SourceHeight,
  SourceWidth,
  TargetHeight,
  TargetWidth: Integer;

begin
  // shortcut variables
  SourceHeight := Source.Height;
  SourceWidth := Source.Width;
  TargetHeight := Target.Height;
  TargetWidth := Target.Width;

  if (SourceHeight = 0) or (SourceWidth = 0) or
     (TargetHeight = 0) or (TargetWidth = 0) then Exit;
     
  // create intermediate image to hold horizontal zoom
  Work := TBitmap.Create;
  try
    Work.PixelFormat := pf24Bit;
    Work.Height := SourceHeight;
    Work.Width := TargetWidth;
    if SourceWidth = 1 then ScaleX :=  TargetWidth / SourceWidth
                       else ScaleX :=  (TargetWidth - 1) / (SourceWidth - 1);
    if (SourceHeight = 1) or (TargetHeight = 1) then ScaleY :=  TargetHeight / SourceHeight
                                                else ScaleY :=  (TargetHeight - 1) / (SourceHeight - 1);

    // pre-calculate filter contributions for a row
    SetLength(ContributorList, TargetWidth);
    // horizontal sub-sampling
    if ScaleX < 1 then
    begin
      // scales from bigger to smaller Width
      Width := Radius / ScaleX;
      for I := 0 to TargetWidth - 1 do
      begin
        ContributorList[I].N := 0;
        SetLength(ContributorList[I].Contributors, Trunc(2 * Width + 1));
        Center := I / ScaleX;
        Left := Floor(Center - Width);
        Right := Ceil(Center + Width);
        for J := Left to Right do
        begin
          Weight := Round(Filter((Center - J) * ScaleX) * ScaleX * 256);
          if Weight <> 0 then
          begin
            if J < 0 then N := -J
                     else
              if J >= SourceWidth then N := SourceWidth - J + SourceWidth - 1
                                  else N := J;
            K := ContributorList[I].N;
            Inc(ContributorList[I].N);
            ContributorList[I].Contributors[K].Pixel := N;
            ContributorList[I].Contributors[K].Weight := Weight;
          end;
        end;
      end;
    end
    else
    begin
      // horizontal super-sampling
      // scales from smaller to bigger Width
      for I := 0 to TargetWidth - 1 do
      begin
        ContributorList[I].N := 0;
        SetLength(ContributorList[I].Contributors, Trunc(2 * Radius + 1));
        Center := I / ScaleX;
        Left := Floor(Center - Radius);
        Right := Ceil(Center + Radius);
        for J := Left to Right do
        begin
          Weight := Round(Filter(Center - J) * 256);
          if Weight <> 0 then
          begin
            if J < 0 then N := -J
                     else
             if J >= SourceWidth then N := SourceWidth - J + SourceWidth - 1
                                 else N := J;
            K := ContributorList[I].N;
            Inc(ContributorList[I].N);
            ContributorList[I].Contributors[K].Pixel := N;
            ContributorList[I].Contributors[K].Weight := Weight;
          end;
        end;
      end;
    end;

    // now apply filter to sample horizontally from Src to Work
    SetLength(CurrentLineR, SourceWidth);
    SetLength(CurrentLineG, SourceWidth);
    SetLength(CurrentLineB, SourceWidth);
    for K := 0 to SourceHeight - 1 do
    begin
      SourceLine := Source.ScanLine[K];
      FillLineChache(SourceWidth, 3, SourceLine);
      DestPixel := Work.ScanLine[K];
      for I := 0 to TargetWidth - 1 do
        with ContributorList[I] do
        begin
          DestPixel^ := ApplyContributors(N, ContributorList[I].Contributors);
          // move on to next column
          Inc(DestPixel);
        end;
    end;

    // free the memory allocated for horizontal filter weights, since we need the stucture again
    for I := 0 to TargetWidth - 1 do ContributorList[I].Contributors := nil;
    ContributorList := nil;

    // pre-calculate filter contributions for a column
    SetLength(ContributorList, TargetHeight);
    // vertical sub-sampling
    if ScaleY < 1 then
    begin
      // scales from bigger to smaller height
      Width := Radius / ScaleY;
      for I := 0 to TargetHeight - 1 do
      begin
        ContributorList[I].N := 0;
        SetLength(ContributorList[I].Contributors, Trunc(2 * Width + 1));
        Center := I / ScaleY;
        Left := Floor(Center - Width);
        Right := Ceil(Center + Width);
        for J := Left to Right do
        begin
          Weight := Round(Filter((Center - J) * ScaleY) * ScaleY * 256);
          if Weight <> 0 then
          begin
            if J < 0 then N := -J
                     else
              if J >= SourceHeight then N := SourceHeight - J + SourceHeight - 1
                                   else N := J;
            K := ContributorList[I].N;
            Inc(ContributorList[I].N);
            ContributorList[I].Contributors[K].Pixel := N;
            ContributorList[I].Contributors[K].Weight := Weight;
          end;
        end;
      end
    end
    else
    begin
      // vertical super-sampling
      // scales from smaller to bigger height
      for I := 0 to TargetHeight - 1 do
      begin
        ContributorList[I].N := 0;
        SetLength(ContributorList[I].Contributors, Trunc(2 * Radius + 1));
        Center := I / ScaleY;
        Left := Floor(Center - Radius);
        Right := Ceil(Center + Radius);
        for J := Left to Right do
        begin
          Weight := Round(Filter(Center - J) * 256);
          if Weight <> 0 then
          begin
            if J < 0 then N := -J
                     else
              if J >= SourceHeight then N := SourceHeight - J + SourceHeight - 1
                                   else N := J;
            K := ContributorList[I].N;
            Inc(ContributorList[I].N);
            ContributorList[I].Contributors[K].Pixel := N;
            ContributorList[I].Contributors[K].Weight := Weight;
          end;
        end;
      end;
    end;

    // apply filter to sample vertically from Work to Target
    SetLength(CurrentLineR, SourceHeight);
    SetLength(CurrentLineG, SourceHeight);
    SetLength(CurrentLineB, SourceHeight);


    SourceLine := Work.ScanLine[0];
    Delta := Integer(Work.ScanLine[1]) - Integer(SourceLine);
    DestLine := Target.ScanLine[0];
    DestDelta := Integer(Target.ScanLine[1]) - Integer(DestLine);
    for K := 0 to TargetWidth - 1 do
    begin
      DestPixel := Pointer(DestLine);
      FillLineChache(SourceHeight, Delta, SourceLine);
      for I := 0 to TargetHeight - 1 do
        with ContributorList[I] do
        begin
          DestPixel^ := ApplyContributors(N, ContributorList[I].Contributors);
          Inc(Integer(DestPixel), DestDelta);
        end;
      Inc(SourceLine);
      Inc(DestLine);
    end;

    // free the memory allocated for vertical filter weights
    for I := 0 to TargetHeight - 1 do ContributorList[I].Contributors := nil;
    // this one is done automatically on exit, but is here for completeness
    ContributorList := nil;

  finally
    Work.Free;
    CurrentLineR := nil;
    CurrentLineG := nil;
    CurrentLineB := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Stretch(NewWidth, NewHeight: Cardinal; Filter: TResamplingFilter; Radius: Single; Source, Target: TBitmap);

// Scales the source bitmap to the given size (NewWidth, NewHeight) and stores the Result in Target.
// Filter describes the filter function to be applied and Radius the size of the filter area.
// Is Radius = 0 then the recommended filter area will be used (see DefaultFilterRadius).

begin
  if Radius = 0 then Radius := DefaultFilterRadius[Filter];
  Target.Handle := 0;
  Target.PixelFormat := pf24Bit;
  Target.Width := NewWidth;
  Target.Height := NewHeight;
  Source.PixelFormat := pf24Bit;
  DoStretch(FilterList[Filter], Radius, Source, Target);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Stretch(NewWidth, NewHeight: Cardinal; Filter: TResamplingFilter; Radius: Single; Source: TBitmap);

var
  Target: TBitmap;

begin
  if Radius = 0 then Radius := DefaultFilterRadius[Filter];
  Target := TBitmap.Create;
  try
    Target.PixelFormat := pf24Bit;
    Target.Width := NewWidth;
    Target.Height := NewHeight;
    Source.PixelFormat := pf24Bit;
    DoStretch(FilterList[Filter], Radius, Source, Target);
    Source.Assign(Target);
  finally
    Target.Free;
  end;
end;

//----------------- support functions for image loading ----------------------------------------------------------------

procedure SwapShort(P: PWord; Count: Cardinal); 

// swaps high and low byte of 16 bit values
// EAX contains P, EDX contains Count

asm
@@Loop:
              MOV CX, [EAX]
              XCHG CH, CL
              MOV [EAX], CX
              ADD EAX, 2
              DEC EDX
              JNZ @@Loop
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SwapLong(P: PInteger; Count: Cardinal); overload;

// swaps high and low bytes of 32 bit values
// EAX contains P, EDX contains Count

asm
@@Loop:
              MOV ECX, [EAX]
              BSWAP ECX
              MOV [EAX], ECX
              ADD EAX, 4
              DEC EDX
              JNZ @@Loop
end;

//----------------------------------------------------------------------------------------------------------------------

function SwapLong(Value: Cardinal): Cardinal; overload;

// swaps high and low bytes of the given 32 bit value

asm
              BSWAP EAX
end;

//----------------- various conversion routines ------------------------------------------------------------------------

procedure Depredict1(P: Pointer; Count: Cardinal);

// EAX contains P and EDX Count

asm
@@1:
              MOV CL, [EAX]
              ADD [EAX + 1], CL
              INC EAX
              DEC EDX
              JNZ @@1
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Depredict3(P: Pointer; Count: Cardinal); 

// EAX contains P and EDX Count

asm
              MOV ECX, EDX
              SHL ECX, 1
              ADD ECX, EDX         // 3 * Count
@@1:
              MOV DL, [EAX]
              ADD [EAX + 3], DL
              INC EAX
              DEC ECX
              JNZ @@1
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Depredict4(P: Pointer; Count: Cardinal);

// EAX contains P and EDX Count

asm
              SHL EDX, 2          // 4 * Count
@@1:
              MOV CL, [EAX]
              ADD [EAX + 4], CL
              INC EAX
              DEC EDX
              JNZ @@1
end;

//----------------- TGraphicExGraphic ----------------------------------------------------------------------------------

constructor TGraphicExGraphic.Create;

begin
  inherited;
  FColorManager := TColorManager.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TGraphicExGraphic.Destroy;

begin
  FColorManager.Free;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.Assign(Source: TPersistent);

begin
  if Source is TGraphicExGraphic then FImageProperties := TGraphicExGraphic(Source).FImageProperties;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TGraphicExGraphic.CanLoad(const FileName: String): Boolean;

var
  Stream: TFileStream;

begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := CanLoad(Stream);
  finally
    Stream.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TGraphicExGraphic.CanLoad(Stream: TStream): Boolean;

// Descentants have to override this method and return True if they consider the data in Stream
// as loadable by the particular class.
// Note: Make sure the stream position is the same on exit as it was on enter!

begin
  Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.LoadFromResourceID(Instance: THandle; ResID: Integer);

var
  Stream: TResourceStream;

begin
  Stream := TResourceStream.CreateFromID(Instance, ResID, RT_RCDATA);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.LoadFromResourceName(Instance: THandle; const ResName: String);

var
  Stream: TResourceStream;

begin
  Stream := TResourceStream.Create(Instance, ResName, RT_RCDATA);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGraphicExGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

// Initializes the internal image properties structure.
// Descentants must override this method to fill in the actual values.
// Result is always False to show there is no image to load.

begin
  Finalize(FImageProperties);
  ZeroMemory(@FImageProperties, SizeOf(FImageProperties));
  FImageProperties.FileGamma := 1;
  Result := False;
end;

//----------------- TAutodeskGraphic -----------------------------------------------------------------------------------

{$ifdef AutodeskGraphic}

type
  TAutodeskHeader = packed record
    Width,
    Height,
    XCoord,
    YCoord: Word;
    Depth,
    Compression: Byte;
    DataSize: Cardinal;
    Reserved: array[0..15] of Byte;
  end;

//----------------------------------------------------------------------------------------------------------------------

class function TAutodeskGraphic.CanLoad(Stream: TStream): Boolean;

var
  FileID: Word;
  Header: TAutodeskHeader;
  LastPosition: Cardinal;

begin
  with Stream do
  begin
    Result := (Size - Position) > (SizeOf(FileID) + SizeOf(Header));
    if Result then
    begin
      LastPosition := Position;
      Read(FileID, SizeOf(FileID));
      Result := FileID = $9119;
      if Result then
      begin
        // read image dimensions
        Read(Header, SizeOf(Header));
        Result := (Header.Depth = 8) and (Header.Compression = 0);
      end;
      Position := LastPosition;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAutodeskGraphic.LoadFromStream(Stream: TStream);

var
  FileID: Word;
  FileHeader: TAutodeskHeader;
  LogPalette: TMaxLogPalette;
  I: Integer;

begin
  Handle := 0;
  FBasePosition := Stream.Position;

  if ReadImageProperties(Stream, 0) then
  begin
    with Stream do
    begin
      Position := FBasePosition;

      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
      Read(FileID, 2);

      // read image dimensions
      Read(FileHeader, SizeOf(FileHeader));

      // read palette entries and create a palette
      ZeroMemory(@LogPalette, SizeOf(LogPalette));
      LogPalette.palVersion := $300;
      LogPalette.palNumEntries := 256;
      for I := 0 to 255 do
      begin
        Read(LogPalette.palPalEntry[I], 3);
        LogPalette.palPalEntry[I].peBlue := LogPalette.palPalEntry[I].peBlue shl 2;
        LogPalette.palPalEntry[I].peGreen := LogPalette.palPalEntry[I].peGreen shl 2;
        LogPalette.palPalEntry[I].peRed := LogPalette.palPalEntry[I].peRed shl 2;
      end;

      // setup bitmap properties
      PixelFormat := pf8Bit;
      Palette := CreatePalette(PLogPalette(@LogPalette)^);
      Width := FileHeader.Width;
      Height := FileHeader.Height;
      // finally read image data
      for I := 0 to Height - 1 do
      begin
        Read(Scanline[I]^, FileHeader.Width);
                                                        
        Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
        OffsetRect(FProgressRect, 0, 1);
      end;

      Progress(Self, psEnding, 0, False, FProgressRect, '');
    end;
  end
  else GraphicExError(gesInvalidImage, ['Autodesk']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TAutodeskGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

var
  FileID: Word;
  Header: TAutodeskHeader;

begin
  Result := inherited ReadImageProperties(Stream, ImageIndex);
  with Stream, FImageProperties do
  begin
    Read(FileID, 2);
    if FileID = $9119 then
    begin
      // read image dimensions
      Read(Header, SizeOf(Header));

      ColorScheme := csIndexed;
      Width := Header.Width;
      Height := Header.Height;
      BitsPerSample := 8;
      SamplesPerPixel := 1;
      BitsPerPixel := 8;
      Compression := ctNone;

      Result := True;
    end;
  end;
end;

{$endif} // AutodeskGraphic

//----------------- TSGIGraphic ----------------------------------------------------------------------------------------

{$ifdef SGIGraphic}

const
  SGIMagic = 474;

  SGI_COMPRESSION_VERBATIM = 0;
  SGI_COMPRESSION_RLE = 1;

type
  TSGIHeader = packed record
    Magic: SmallInt;         // IRIS image file magic number
    Storage,                 // Storage format
    BPC: Byte;               // Number of bytes per pixel channel (1 or 2)
    Dimension: Word;         // Number of dimensions
                             //   1 - one single scanline (and one channel) of length XSize
                             //   2 - two dimensional (one channel) of size XSize x YSize
                             //   3 - three dimensional (ZSize channels) of size XSize x YSize
    XSize,                   // width of image
    YSize,                   // height of image
    ZSize: Word;             // number of channels/planes in image (3 for RGB, 4 for RGBA etc.)
    PixMin,                  // Minimum pixel value
    PixMax: Cardinal;        // Maximum pixel value
    Dummy: Cardinal;         // ignored
    ImageName: array[0..79] of Char;
    ColorMap: Integer;       // Colormap ID
                             //  0 - default, almost all images are stored with this flag
                             //  1 - dithered, only one channel of data (pixels are packed), obsolete
                             //  2 - screen (palette) image, obsolete
                             //  3 - no image data, palette only, not displayable
    Dummy2: array[0..403] of Byte; // ignored
  end;

//----------------------------------------------------------------------------------------------------------------------

class function TSGIGraphic.CanLoad(Stream: TStream): Boolean;

// returns True if the data in Stream represents a graphic which can be loaded by this class

var
  Header: TSGIHeader;
  LastPosition: Cardinal;

begin
  with Stream do
  begin
    Result := (Size - Position) > SizeOf(TSGIHeader);
    if Result then
    begin
      LastPosition := Position;
      ReadBuffer(Header, SizeOf(Header));
      // one number as check is too unreliable, hence we take some more fields into the check
      Result := (Swap(Header.Magic) = SGIMagic) and
                (Header.BPC in [1, 2]) and
                (Swap(Header.Dimension) in [1..3]);
      Position := LastPosition;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSGIGraphic.ReadAndDecode(Red, Green, Blue, Alpha: Pointer; Row, BPC: Cardinal);

var
  Count: Cardinal;
  RawBuffer: Pointer;

begin
  with FStream, FImageProperties do
    // compressed image?
    if Assigned(FDecoder) then
    begin
      if Assigned(Red) then
      begin
        Position := FBasePosition + FRowStart[Row + 0 * Height];
        Count := BPC * FRowSize[Row + 0 * Height];
        GetMem(RawBuffer, Count);
        try
          Read(RawBuffer^, Count);
          FDecoder.Decode(RawBuffer, Red, Count, Width);
        finally
          if Assigned(RawBuffer) then FreeMem(RawBuffer);
        end;
      end;

      if Assigned(Green) then
      begin
        Position := FBasePosition + FRowStart[Row + 1 * Height];
        Count := BPC * FRowSize[Row + 1 * Height];
        GetMem(RawBuffer, Count);
        try
          Read(RawBuffer^, Count);
          FDecoder.Decode(RawBuffer, Green, Count, Width);
        finally
          if Assigned(RawBuffer) then FreeMem(RawBuffer);
        end;
      end;

      if Assigned(Blue) then
      begin
        Position := FBasePosition + FRowStart[Row + 2 * Height];
        Count := BPC * FRowSize[Row + 2 * Height];
        GetMem(RawBuffer, Count);
        try
          Read(RawBuffer^, Count);
          FDecoder.Decode(RawBuffer, Blue, Count, Width);
        finally
          if Assigned(RawBuffer) then FreeMem(RawBuffer);
        end;
      end;

      if Assigned(Alpha) then
      begin
        Position := FBasePosition + FRowStart[Row + 3 * Height];
        Count := BPC * FRowSize[Row + 3 * Height];
        GetMem(RawBuffer, Count);
        try
          Read(RawBuffer^, Count);
          FDecoder.Decode(RawBuffer, Alpha, Count, Width);
        finally
          if Assigned(RawBuffer) then FreeMem(RawBuffer);
        end;
      end;
    end
    else
    begin
      if Assigned(Red) then
      begin
        Position := FBasePosition + 512 + (Row * Width);
        Read(Red^, BPC * Width);
      end;

      if Assigned(Green) then
      begin
        Position := FBasePosition + 512 + (Row * Width) + (Width * Height);
        Read(Green^, BPC * Width);
      end;

      if Assigned(Blue) then
      begin
        Position := FBasePosition + 512 + (Row * Width) + (2 * Width * Height);
        Read(Blue^, BPC * Width);
      end;

      if Assigned(Alpha) then
      begin
        Position := FBasePosition + 512 + (Row * Width) + (3 * Width * Height);
        Read(Alpha^, BPC * Width);
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSGIGraphic.LoadFromStream(Stream: TStream);

var
  Y: Cardinal;
  RedBuffer,
  GreenBuffer,
  BlueBuffer,
  AlphaBuffer: Pointer;
  Header: TSGIHeader;
  Count: Cardinal;

begin
  // free previous image
  Handle := 0;

  // keep stream reference and start position for seek operations
  FStream := Stream;
  FBasePosition := Stream.Position;

  if ReadImageProperties(Stream, 0) then
  begin
    with FImageProperties, Stream do
    begin
      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      Stream.Position := FBasePosition;
      // read header again, we need some additional information
      ReadBuffer(Header, SizeOf(Header));

      // SGI images are always stored in big endian style
      ColorManager.SourceOptions := [coNeedByteSwap];
      with Header do ColorMap := SwapLong(ColorMap);

      if Compression = ctRLE then
      begin
        Count := Height * SamplesPerPixel;
        SetLength(FRowStart, Count);
        SetLength(FRowSize, Count);
        // read line starts and sizes from stream
        Read(FRowStart[0], Count * SizeOf(Cardinal));
        SwapLong(@FRowStart[0], Count);
        Read(FRowSize[0], Count * SizeOf(Cardinal));
        SwapLong(@FRowSize[0], Count);
        FDecoder := TSGIRLEDecoder.Create(BitsPerSample);
      end
      else
      begin
        FDecoder := nil;
      end;

      // set pixel format before size to avoid possibly large conversion operation
      with ColorManager do
      begin
        SourceBitsPerSample := BitsPerSample;
        TargetBitsPerSample := 8;
        SourceSamplesPerPixel := SamplesPerPixel;
        TargetSamplesPerPixel := SamplesPerPixel;
        SourceColorScheme := ColorScheme;
        case ColorScheme of
          csRGBA:
            TargetColorScheme := csBGRA;
          csRGB:
            TargetColorScheme := csBGR;
        else
          TargetColorScheme := csIndexed;
        end;
        PixelFormat := TargetPixelFormat;
      end;
      Self.Width := Width;
      Self.Height := Height;

      RedBuffer := nil;
      GreenBuffer := nil;
      BlueBuffer := nil;
      AlphaBuffer := nil;
      Progress(Self, psEnding, 100, True, FProgressRect, '');

      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
      try
        Count := (BitsPerPixel div 8) * Width;
        // read lines and put them into the bitmap
        case ColorScheme of
          csRGBA:
            begin
              GetMem(RedBuffer, Count);
              GetMem(GreenBuffer, Count);
              GetMem(BlueBuffer, Count);
              GetMem(AlphaBuffer, Count);
              for  Y := 0 to Height - 1 do
              begin
                ReadAndDecode(RedBuffer, GreenBuffer, BlueBuffer, AlphaBuffer, Y, Header.BPC);
                ColorManager.ConvertRow([RedBuffer, GreenBuffer, BlueBuffer, AlphaBuffer],
                                        ScanLine[Height - Y - 1], Width, $FF);
                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            end;
          csRGB:
            begin
              GetMem(RedBuffer, Count);
              GetMem(GreenBuffer, Count);
              GetMem(BlueBuffer, Count);
              for  Y := 0 to Height - 1 do
              begin
                ReadAndDecode(RedBuffer, GreenBuffer, BlueBuffer, nil, Y, Header.BPC);
                ColorManager.ConvertRow([RedBuffer, GreenBuffer, BlueBuffer],
                                        ScanLine[Height - Y - 1], Width, $FF);
                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            end;
        else
          // any other format is interpreted as being 256 gray scales
          Palette := ColorManager.CreateGrayscalePalette(False);
          for  Y := 0 to Height - 1 do
          begin
            ReadAndDecode(ScanLine[Height - Y - 1], nil, nil, nil, Y, Header.BPC);
            Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
            OffsetRect(FProgressRect, 0, 1);
          end;
        end;

      finally
        Progress(Self, psEnding, 100, True, FProgressRect, '');

        if Assigned(RedBuffer) then FreeMem(RedBuffer);
        if Assigned(GreenBuffer) then FreeMem(GreenBuffer);
        if Assigned(BlueBuffer) then FreeMem(BlueBuffer);
        if Assigned(AlphaBuffer) then FreeMem(AlphaBuffer);
        FDecoder.Free;
      end;
    end;
  end
  else GraphicExError(gesInvalidImage, ['sgi, bw or rgb(a)']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSGIGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

var
  Header: TSGIHeader;
 
begin
  Result := inherited ReadImageProperties(Stream, ImageIndex);
  with FImageProperties do
  begin
    Stream.ReadBuffer(Header, SizeOf(Header));
    if Swap(Header.Magic) = SGIMagic then
    begin 
      Options := [ioBigEndian];
      BitsPerSample := Header.BPC * 8;
      Width := Swap(Header.XSize);
      Height := Swap(Header.YSize);
      SamplesPerPixel := Swap(Header.ZSize);
      case SamplesPerPixel of
        4:
          ColorScheme := csRGBA;
        3:
          ColorScheme := csRGB;
      else
        // all other is considered as being 8 bit gray scale
        ColorScheme := csIndexed;
      end;

      BitsPerPixel := BitsPerSample * SamplesPerPixel;
      if Header.Storage = SGI_COMPRESSION_RLE then Compression := ctRLE
                                              else Compression := ctNone;
      Result := True;
    end;
  end;
end;

{$endif} // SGIGraphic

//----------------- TTIFFGraphic ---------------------------------------------------------------------------------------

{$ifdef TIFFGraphic}

const // TIFF tags
  TIFFTAG_SUBFILETYPE = 254;                     // subfile data descriptor
    FILETYPE_REDUCEDIMAGE = $1;                  // reduced resolution version
    FILETYPE_PAGE = $2;                          // one page of many
    FILETYPE_MASK = $4;                          // transparency mask
  TIFFTAG_OSUBFILETYPE = 255;                    // kind of data in subfile (obsolete by revision 5.0)
    OFILETYPE_IMAGE = 1;                         // full resolution image data
    OFILETYPE_REDUCEDIMAGE = 2;                  // reduced size image data
    OFILETYPE_PAGE = 3;                          // one page of many
  TIFFTAG_IMAGEWIDTH = 256;                      // image width in pixels
  TIFFTAG_IMAGELENGTH = 257;                     // image height in pixels
  TIFFTAG_BITSPERSAMPLE = 258;                   // bits per channel (sample)
  TIFFTAG_COMPRESSION = 259;                     // data compression technique
    COMPRESSION_NONE = 1;                        // dump mode
    COMPRESSION_CCITTRLE = 2;                    // CCITT modified Huffman RLE
    COMPRESSION_CCITTFAX3 = 3;                   // CCITT Group 3 fax encoding
    COMPRESSION_CCITTFAX4 = 4;                   // CCITT Group 4 fax encoding
    COMPRESSION_LZW = 5;                         // Lempel-Ziv & Welch
    COMPRESSION_OJPEG = 6;                       // 6.0 JPEG (old version)
    COMPRESSION_JPEG = 7;                        // JPEG DCT compression (new version)
    COMPRESSION_ADOBE_DEFLATE = 8;               // new id but same as COMPRESSION_DEFLATE
    COMPRESSION_NEXT = 32766;                    // next 2-bit RLE
    COMPRESSION_CCITTRLEW = 32771;               // modified Huffman with word alignment
    COMPRESSION_PACKBITS = 32773;                // Macintosh RLE
    COMPRESSION_THUNDERSCAN = 32809;             // ThunderScan RLE
    // codes 32895-32898 are reserved for ANSI IT8 TIFF/IT <dkelly@etsinc.com)
    COMPRESSION_IT8CTPAD = 32895;                // IT8 CT w/padding
    COMPRESSION_IT8LW = 32896;                   // IT8 Linework RLE
    COMPRESSION_IT8MP = 32897;                   // IT8 Monochrome picture
    COMPRESSION_IT8BL = 32898;                   // IT8 Binary line art
    // compression codes 32908-32911 are reserved for Pixar
    COMPRESSION_PIXARFILM = 32908;               // Pixar companded 10bit LZW
    COMPRESSION_PIXARLOG = 32909;                // Pixar companded 11bit ZIP
    COMPRESSION_DEFLATE = 32946;                 // Deflate compression (LZ77)
    // compression code 32947 is reserved for Oceana Matrix <dev@oceana.com>
    COMPRESSION_DCS = 32947;                     // Kodak DCS encoding
    COMPRESSION_JBIG = 34661;                    // ISO JBIG
  TIFFTAG_PHOTOMETRIC = 262;                     // photometric interpretation
    PHOTOMETRIC_MINISWHITE = 0;                  // min value is white
    PHOTOMETRIC_MINISBLACK = 1;                  // min value is black
    PHOTOMETRIC_RGB = 2;                         // RGB color model
    PHOTOMETRIC_PALETTE = 3;                     // color map indexed
    PHOTOMETRIC_MASK = 4;                        // holdout mask
    PHOTOMETRIC_SEPARATED = 5;                   // color separations
    PHOTOMETRIC_YCBCR = 6;                       // CCIR 601
    PHOTOMETRIC_CIELAB = 8;                      // 1976 CIE L*a*b*
  TIFFTAG_THRESHHOLDING = 263;                   // thresholding used on data (obsolete by revision 5.0)
    THRESHHOLD_BILEVEL = 1;                      // b&w art scan
    THRESHHOLD_HALFTONE = 2;                     // or dithered scan
    THRESHHOLD_ERRORDIFFUSE = 3;                 // usually floyd-steinberg
  TIFFTAG_CELLWIDTH = 264;                       // dithering matrix width (obsolete by revision 5.0)
  TIFFTAG_CELLLENGTH = 265;                      // dithering matrix height (obsolete by revision 5.0)
  TIFFTAG_FILLORDER = 266;                       // data order within a Byte
    FILLORDER_MSB2LSB = 1;                       // most significant -> least
    FILLORDER_LSB2MSB = 2;                       // least significant -> most
  TIFFTAG_DOCUMENTNAME = 269;                    // name of doc. image is from
  TIFFTAG_IMAGEDESCRIPTION = 270;                // info about image
  TIFFTAG_MAKE = 271;                            // scanner manufacturer name
  TIFFTAG_MODEL = 272;                           // scanner model name/number
  TIFFTAG_STRIPOFFSETS = 273;                    // Offsets to data strips
  TIFFTAG_ORIENTATION = 274;                     // image FOrientation (obsolete by revision 5.0)
    ORIENTATION_TOPLEFT = 1;                     // row 0 top, col 0 lhs
    ORIENTATION_TOPRIGHT = 2;                    // row 0 top, col 0 rhs
    ORIENTATION_BOTRIGHT = 3;                    // row 0 bottom, col 0 rhs
    ORIENTATION_BOTLEFT = 4;                     // row 0 bottom, col 0 lhs
    ORIENTATION_LEFTTOP = 5;                     // row 0 lhs, col 0 top
    ORIENTATION_RIGHTTOP = 6;                    // row 0 rhs, col 0 top
    ORIENTATION_RIGHTBOT = 7;                    // row 0 rhs, col 0 bottom
    ORIENTATION_LEFTBOT = 8;                     // row 0 lhs, col 0 bottom
  TIFFTAG_SAMPLESPERPIXEL = 277;                 // samples per pixel
  TIFFTAG_ROWSPERSTRIP = 278;                    // rows per strip of data
  TIFFTAG_STRIPBYTECOUNTS = 279;                 // bytes counts for strips
  TIFFTAG_MINSAMPLEVALUE = 280;                  // minimum sample value (obsolete by revision 5.0)
  TIFFTAG_MAXSAMPLEVALUE = 281;                  // maximum sample value (obsolete by revision 5.0)
  TIFFTAG_XRESOLUTION = 282;                     // pixels/resolution in x
  TIFFTAG_YRESOLUTION = 283;                     // pixels/resolution in y
  TIFFTAG_PLANARCONFIG = 284;                    // storage organization
    PLANARCONFIG_CONTIG = 1;                     // single image plane
    PLANARCONFIG_SEPARATE = 2;                   // separate planes of data
  TIFFTAG_PAGENAME = 285;                        // page name image is from
  TIFFTAG_XPOSITION = 286;                       // x page offset of image lhs
  TIFFTAG_YPOSITION = 287;                       // y page offset of image lhs
  TIFFTAG_FREEOFFSETS = 288;                     // byte offset to free block (obsolete by revision 5.0)
  TIFFTAG_FREEBYTECOUNTS = 289;                  // sizes of free blocks (obsolete by revision 5.0)
  TIFFTAG_GRAYRESPONSEUNIT = 290;                // gray scale curve accuracy
    GRAYRESPONSEUNIT_10S = 1;                    // tenths of a unit
    GRAYRESPONSEUNIT_100S = 2;                   // hundredths of a unit
    GRAYRESPONSEUNIT_1000S = 3;                  // thousandths of a unit
    GRAYRESPONSEUNIT_10000S = 4;                 // ten-thousandths of a unit
    GRAYRESPONSEUNIT_100000S = 5;                // hundred-thousandths
  TIFFTAG_GRAYRESPONSECURVE = 291;               // gray scale response curve
  TIFFTAG_GROUP3OPTIONS = 292;                   // 32 flag bits
    GROUP3OPT_2DENCODING = $1;                   // 2-dimensional coding
    GROUP3OPT_UNCOMPRESSED = $2;                 // data not compressed
    GROUP3OPT_FILLBITS = $4;                     // fill to byte boundary
  TIFFTAG_GROUP4OPTIONS = 293;                   // 32 flag bits
    GROUP4OPT_UNCOMPRESSED = $2;                 // data not compressed
  TIFFTAG_RESOLUTIONUNIT = 296;                  // units of resolutions
    RESUNIT_NONE = 1;                            // no meaningful units
    RESUNIT_INCH = 2;                            // english
    RESUNIT_CENTIMETER = 3;                      // metric
  TIFFTAG_PAGENUMBER = 297;                      // page numbers of multi-page
  TIFFTAG_COLORRESPONSEUNIT = 300;               // color curve accuracy
    COLORRESPONSEUNIT_10S = 1;                   // tenths of a unit
    COLORRESPONSEUNIT_100S = 2;                  // hundredths of a unit
    COLORRESPONSEUNIT_1000S = 3;                 // thousandths of a unit
    COLORRESPONSEUNIT_10000S = 4;                // ten-thousandths of a unit
    COLORRESPONSEUNIT_100000S = 5;               // hundred-thousandths
  TIFFTAG_TRANSFERFUNCTION = 301;                // colorimetry info
  TIFFTAG_SOFTWARE = 305;                        // name & release
  TIFFTAG_DATETIME = 306;                        // creation date and time
  TIFFTAG_ARTIST = 315;                          // creator of image
  TIFFTAG_HOSTCOMPUTER = 316;                    // machine where created
  TIFFTAG_PREDICTOR = 317;                       // prediction scheme w/ LZW
    PREDICTION_NONE = 1;                         // no prediction scheme used before coding
    PREDICTION_HORZ_DIFFERENCING = 2;            // horizontal differencing prediction scheme used
  TIFFTAG_WHITEPOINT = 318;                      // image white point
  TIFFTAG_PRIMARYCHROMATICITIES = 319;           // primary chromaticities
  TIFFTAG_COLORMAP = 320;                        // RGB map for pallette image
  TIFFTAG_HALFTONEHINTS = 321;                   // highlight+shadow info
  TIFFTAG_TILEWIDTH = 322;                       // rows/data tile
  TIFFTAG_TILELENGTH = 323;                      // cols/data tile
  TIFFTAG_TILEOFFSETS = 324;                     // offsets to data tiles
  TIFFTAG_TILEBYTECOUNTS = 325;                  // Byte counts for tiles
  TIFFTAG_BADFAXLINES = 326;                     // lines w/ wrong pixel count
  TIFFTAG_CLEANFAXDATA = 327;                    // regenerated line info
    CLEANFAXDATA_CLEAN = 0;                      // no errors detected
    CLEANFAXDATA_REGENERATED = 1;                // receiver regenerated lines
    CLEANFAXDATA_UNCLEAN = 2;                    // uncorrected errors exist
  TIFFTAG_CONSECUTIVEBADFAXLINES = 328;          // max consecutive bad lines
  TIFFTAG_SUBIFD = 330;                          // subimage descriptors
  TIFFTAG_INKSET = 332;                          // inks in separated image
    INKSET_CMYK = 1;                             // cyan-magenta-yellow-black
  TIFFTAG_INKNAMES = 333;                        // ascii names of inks
  TIFFTAG_DOTRANGE = 336;                        // 0% and 100% dot codes
  TIFFTAG_TARGETPRINTER = 337;                   // separation target
  TIFFTAG_EXTRASAMPLES = 338;                    // info about extra samples
    EXTRASAMPLE_UNSPECIFIED = 0;                 // unspecified data
    EXTRASAMPLE_ASSOCALPHA = 1;                  // associated alpha data
    EXTRASAMPLE_UNASSALPHA = 2;                  // unassociated alpha data
  TIFFTAG_SAMPLEFORMAT = 339;                    // data sample format
    SAMPLEFORMAT_UINT = 1;                       // unsigned integer data
    SAMPLEFORMAT_INT = 2;                        // signed integer data
    SAMPLEFORMAT_IEEEFP = 3;                     // IEEE floating point data
    SAMPLEFORMAT_VOID = 4;                       // untyped data
  TIFFTAG_SMINSAMPLEVALUE = 340;                 // variable MinSampleValue
  TIFFTAG_SMAXSAMPLEVALUE = 341;                 // variable MaxSampleValue
  TIFFTAG_JPEGTABLES = 347;                      // JPEG table stream

  // Tags 512-521 are obsoleted by Technical Note #2 which specifies a revised JPEG-in-TIFF scheme.

  TIFFTAG_JPEGPROC = 512;                        // JPEG processing algorithm
    JPEGPROC_BASELINE = 1;                       // baseline sequential
    JPEGPROC_LOSSLESS = 14;                      // Huffman coded lossless
  TIFFTAG_JPEGIFOFFSET = 513;                    // Pointer to SOI marker
  TIFFTAG_JPEGIFBYTECOUNT = 514;                 // JFIF stream length
  TIFFTAG_JPEGRESTARTINTERVAL = 515;             // restart interval length
  TIFFTAG_JPEGLOSSLESSPREDICTORS = 517;          // lossless proc predictor
  TIFFTAG_JPEGPOINTTRANSFORM = 518;              // lossless point transform
  TIFFTAG_JPEGQTABLES = 519;                     // Q matrice offsets
  TIFFTAG_JPEGDCTABLES = 520;                    // DCT table offsets
  TIFFTAG_JPEGACTABLES = 521;                    // AC coefficient offsets
  TIFFTAG_YCBCRCOEFFICIENTS = 529;               // RGB -> YCbCr transform
  TIFFTAG_YCBCRSUBSAMPLING = 530;                // YCbCr subsampling factors
  TIFFTAG_YCBCRPOSITIONING = 531;                // subsample positioning
    YCBCRPOSITION_CENTERED = 1;                  // as in PostScript Level 2
    YCBCRPOSITION_COSITED = 2;                   // as in CCIR 601-1
  TIFFTAG_REFERENCEBLACKWHITE = 532;             // colorimetry info
  // tags 32952-32956 are private tags registered to Island Graphics
  TIFFTAG_REFPTS = 32953;                        // image reference points
  TIFFTAG_REGIONTACKPOINT = 32954;               // region-xform tack point
  TIFFTAG_REGIONWARPCORNERS = 32955;             // warp quadrilateral
  TIFFTAG_REGIONAFFINE = 32956;                  // affine transformation mat
  // tags 32995-32999 are private tags registered to SGI
  TIFFTAG_MATTEING = 32995;                      // use ExtraSamples
  TIFFTAG_DATATYPE = 32996;                      // use SampleFormat
  TIFFTAG_IMAGEDEPTH = 32997;                    // z depth of image
  TIFFTAG_TILEDEPTH = 32998;                     // z depth/data tile

  // tags 33300-33309 are private tags registered to Pixar
  //
  // TIFFTAG_PIXAR_IMAGEFULLWIDTH and TIFFTAG_PIXAR_IMAGEFULLLENGTH
  // are set when an image has been cropped out of a larger image.
  // They reflect the size of the original uncropped image.
  // The TIFFTAG_XPOSITION and TIFFTAG_YPOSITION can be used
  // to determine the position of the smaller image in the larger one.

  TIFFTAG_PIXAR_IMAGEFULLWIDTH = 33300;          // full image size in x
  TIFFTAG_PIXAR_IMAGEFULLLENGTH = 33301;         // full image size in y
  // tag 33405 is a private tag registered to Eastman Kodak
  TIFFTAG_WRITERSERIALNUMBER = 33405;            // device serial number
  // tag 33432 is listed in the 6.0 spec w/ unknown ownership
  TIFFTAG_COPYRIGHT = 33432;                     // copyright string
  // 34016-34029 are reserved for ANSI IT8 TIFF/IT <dkelly@etsinc.com)
  TIFFTAG_IT8SITE = 34016;                       // site name
  TIFFTAG_IT8COLORSEQUENCE = 34017;              // color seq. [RGB,CMYK,etc]
  TIFFTAG_IT8HEADER = 34018;                     // DDES Header
  TIFFTAG_IT8RASTERPADDING = 34019;              // raster scanline padding
  TIFFTAG_IT8BITSPERRUNLENGTH = 34020;           // # of bits in short run
  TIFFTAG_IT8BITSPEREXTENDEDRUNLENGTH = 34021;   // # of bits in long run
  TIFFTAG_IT8COLORTABLE = 34022;                 // LW colortable
  TIFFTAG_IT8IMAGECOLORINDICATOR = 34023;        // BP/BL image color switch
  TIFFTAG_IT8BKGCOLORINDICATOR = 34024;          // BP/BL bg color switch
  TIFFTAG_IT8IMAGECOLORVALUE = 34025;            // BP/BL image color value
  TIFFTAG_IT8BKGCOLORVALUE = 34026;              // BP/BL bg color value
  TIFFTAG_IT8PIXELINTENSITYRANGE = 34027;        // MP pixel intensity value
  TIFFTAG_IT8TRANSPARENCYINDICATOR = 34028;      // HC transparency switch
  TIFFTAG_IT8COLORCHARACTERIZATION = 34029;      // color character. table
  // tags 34232-34236 are private tags registered to Texas Instruments
  TIFFTAG_FRAMECOUNT = 34232;                    // Sequence Frame Count
  // tag 34750 is a private tag registered to Pixel Magic
  TIFFTAG_JBIGOPTIONS = 34750;                   // JBIG options
  // tags 34908-34914 are private tags registered to SGI
  TIFFTAG_FAXRECVPARAMS = 34908;                 // encoded class 2 ses. parms
  TIFFTAG_FAXSUBADDRESS = 34909;                 // received SubAddr string
  TIFFTAG_FAXRECVTIME = 34910;                   // receive time (secs)
  // tag 65535 is an undefined tag used by Eastman Kodak
  TIFFTAG_DCSHUESHIFTVALUES = 65535;             // hue shift correction data

  // The following are 'pseudo tags' that can be used to control codec-specific functionality.
  // These tags are not written to file.  Note that these values start at $FFFF + 1 so that they'll
  // never collide with Aldus-assigned tags.

  TIFFTAG_FAXMODE = 65536;                       // Group 3/4 format control
    FAXMODE_CLASSIC = $0000;                     // default, include RTC
    FAXMODE_NORTC = $0001;                       // no RTC at end of data
    FAXMODE_NOEOL = $0002;                       // no EOL code at end of row
    FAXMODE_BYTEALIGN = $0004;                   // Byte align row
    FAXMODE_WORDALIGN = $0008;                   // Word align row
    FAXMODE_CLASSF = FAXMODE_NORTC;              // TIFF class F
  TIFFTAG_JPEGQUALITY = 65537;                   // compression quality level
  // Note: quality level is on the IJG 0-100 scale.  Default value is 75
  TIFFTAG_JPEGCOLORMODE = 65538;                 // Auto RGB<=>YCbCr convert?
    JPEGCOLORMODE_RAW = $0000;                   // no conversion (default)
    JPEGCOLORMODE_RGB = $0001;                   // do auto conversion
  TIFFTAG_JPEGTABLESMODE = 65539;                // What to put in JPEGTables
    JPEGTABLESMODE_QUANT = $0001;                // include quantization tbls
    JPEGTABLESMODE_HUFF = $0002;                 // include Huffman tbls
  // Note: default is JPEGTABLESMODE_QUANT or JPEGTABLESMODE_HUFF
  TIFFTAG_FAXFILLFUNC = 65540;                   // G3/G4 fill function
  TIFFTAG_PIXARLOGDATAFMT = 65549;               // PixarLogCodec I/O data sz
    PIXARLOGDATAFMT_8BIT = 0;                    // regular u_char samples
    PIXARLOGDATAFMT_8BITABGR = 1;                // ABGR-order u_chars
    PIXARLOGDATAFMT_11BITLOG = 2;                // 11-bit log-encoded (raw)
    PIXARLOGDATAFMT_12BITPICIO = 3;              // as per PICIO (1.0==2048)
    PIXARLOGDATAFMT_16BIT = 4;                   // signed short samples
    PIXARLOGDATAFMT_FLOAT = 5;                   // IEEE float samples
  // 65550-65556 are allocated to Oceana Matrix <dev@oceana.com>
  TIFFTAG_DCSIMAGERTYPE = 65550;                 // imager model & filter
    DCSIMAGERMODEL_M3 = 0;                       // M3 chip (1280 x 1024)
    DCSIMAGERMODEL_M5 = 1;                       // M5 chip (1536 x 1024)
    DCSIMAGERMODEL_M6 = 2;                       // M6 chip (3072 x 2048)
    DCSIMAGERFILTER_IR = 0;                      // infrared filter
    DCSIMAGERFILTER_MONO = 1;                    // monochrome filter
    DCSIMAGERFILTER_CFA = 2;                     // color filter array
    DCSIMAGERFILTER_OTHER = 3;                   // other filter
  TIFFTAG_DCSINTERPMODE = 65551;                 // interpolation mode
    DCSINTERPMODE_NORMAL = $0;                   // whole image, default
    DCSINTERPMODE_PREVIEW = $1;                  // preview of image (384x256)
  TIFFTAG_DCSBALANCEARRAY = 65552;               // color balance values
  TIFFTAG_DCSCORRECTMATRIX = 65553;              // color correction values
  TIFFTAG_DCSGAMMA = 65554;                      // gamma value
  TIFFTAG_DCSTOESHOULDERPTS = 65555;             // toe & shoulder points
  TIFFTAG_DCSCALIBRATIONFD = 65556;              // calibration file desc
  // Note: quality level is on the ZLIB 1-9 scale. Default value is -1
  TIFFTAG_ZIPQUALITY = 65557;                    // compression quality level
  TIFFTAG_PIXARLOGQUALITY = 65558;               // PixarLog uses same scale

  // TIFF data types
  TIFF_NOTYPE = 0;                               // placeholder
  TIFF_BYTE = 1;                                 // 8-bit unsigned integer
  TIFF_ASCII = 2;                                // 8-bit bytes w/ last byte null
  TIFF_SHORT = 3;                                // 16-bit unsigned integer
  TIFF_LONG = 4;                                 // 32-bit unsigned integer
  TIFF_RATIONAL = 5;                             // 64-bit unsigned fraction
  TIFF_SBYTE = 6;                                // 8-bit signed integer
  TIFF_UNDEFINED = 7;                            // 8-bit untyped data
  TIFF_SSHORT = 8;                               // 16-bit signed integer
  TIFF_SLONG = 9;                                // 32-bit signed integer
  TIFF_SRATIONAL = 10;                           // 64-bit signed fraction
  TIFF_FLOAT = 11;                               // 32-bit IEEE floating point
  TIFF_DOUBLE = 12;                              // 64-bit IEEE floating point

  TIFF_BIGENDIAN = $4D4D;
  TIFF_LITTLEENDIAN = $4949;

  TIFF_VERSION = 42;

type
  TTIFFHeader = packed record
    ByteOrder: Word;
    Version: Word;
    FirstIFD: Cardinal;
  end;

//----------------------------------------------------------------------------------------------------------------------

class function TTIFFGraphic.CanLoad(Stream: TStream): Boolean;

var
  Header: TTIFFHeader;
  LastPosition: Cardinal;

begin
  with Stream do
  begin
    Result := (Size - Position) > SizeOf(Header);
    if Result then
    begin
      LastPosition := Position;

      Stream.ReadBuffer(Header, SizeOf(Header));
      Result := (Header.ByteOrder = TIFF_BIGENDIAN) or
                (Header.ByteOrder = TIFF_LITTLEENDIAN);
      if Result then
      begin
        if Header.ByteOrder = TIFF_BIGENDIAN then
        begin
          Header.Version := Swap(Header.Version);
          Header.FirstIFD := SwapLong(Header.FirstIFD);
        end;

        Result := (Header.Version = TIFF_VERSION) and (Integer(Header.FirstIFD) < (Size - Integer(LastPosition)));
      end;
      Position := LastPosition;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TTIFFGraphic.FindTag(Tag: Cardinal; var Index: Cardinal): Boolean;

// looks through the currently loaded IFD for the entry indicated by Tag;
// returns True and the index of the entry in Index if the entry is there
// otherwise the result is False and Index undefined
// Note: The IFD is sorted so we can use a binary search here.

var
  L, H, I, C: Integer;

begin
  Result := False;
  L := 0;
  H := High(FIFD);
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Integer(FIFD[I].Tag) - Integer(Tag);
    if C < 0 then L := I + 1
             else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

//----------------------------------------------------------------------------------------------------------------------

const
  DataTypeToSize: array[TIFF_NOTYPE..TIFF_SLONG] of Byte = (0, 1, 1, 2, 4, 8, 1, 1, 2, 4);

procedure TTIFFGraphic.GetValueList(Stream: TStream; Tag: Cardinal; var Values: TByteArray);

// returns the values of the IFD entry indicated by Tag

var
  Index,
  Value,
  Shift: Cardinal;
  I: Integer;

begin
  Values := nil;
  if FindTag(Tag, Index) and
     (FIFD[Index].DataLength > 0) then
  begin
    // prepare value list
    SetLength(Values, FIFD[Index].DataLength);

    // determine whether the data fits into 4 bytes
    Value := DataTypeToSize[FIFD[Index].DataType] * FIFD[Index].DataLength;

    // data fits into one cardinal -> extract it
    if Value <= 4 then
    begin
      Shift := DataTypeToSize[FIFD[Index].DataType] * 8;
      Value := FIFD[Index].Offset;
      for I := 0 to FIFD[Index].DataLength - 1 do
      begin
        case FIFD[Index].DataType of
          TIFF_BYTE:
            Values[I] := Byte(Value);
          TIFF_SHORT,
          TIFF_SSHORT:
            // no byte swap needed here because values in the IFD are already swapped
            // (if necessary at all)
            Values[I] := Word(Value);
          TIFF_LONG,
          TIFF_SLONG:
            Values[I] := Value;
        end;
        Value := Value shr Shift;
      end;
    end
    else
    begin
      // data of this tag does not fit into one 32 bits value
      Stream.Position := FBasePosition + FIFD[Index].Offset;
      // bytes sized data can be read directly instead of looping through the array
      if FIFD[Index].DataType in [TIFF_BYTE, TIFF_ASCII, TIFF_SBYTE, TIFF_UNDEFINED]
        then Stream.Read(Values[0], Value)
        else
      begin
        for I := 0 to High(Values) do
        begin
          Stream.Read(Value, DataTypeToSize[FIFD[Index].DataType]);
          case FIFD[Index].DataType of
            TIFF_BYTE:
              Value := Byte(Value);
            TIFF_SHORT,
            TIFF_SSHORT:
              begin
                if ioBigEndian in FImageProperties.Options then Value := Swap(Word(Value))
                                                           else Value := Word(Value);
              end;
            TIFF_LONG,
            TIFF_SLONG:
              if ioBigEndian in FImageProperties.Options then Value := SwapLong(Value);
          end;
          Values[I] := Value;
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTIFFGraphic.GetValueList(Stream: TStream; Tag: Cardinal; var Values: TCardinalArray);

// returns the values of the IFD entry indicated by Tag

var
  Index,
  Value,
  Shift: Cardinal;
  I: Integer;

begin
  Values := nil;
  if FindTag(Tag, Index) and
     (FIFD[Index].DataLength > 0) then
  begin
    // prepare value list
    SetLength(Values, FIFD[Index].DataLength);

    // determine whether the data fits into 4 bytes
    Value := DataTypeToSize[FIFD[Index].DataType] * FIFD[Index].DataLength;

    // data fits into one cardinal -> extract it
    if Value <= 4 then
    begin
      Shift := DataTypeToSize[FIFD[Index].DataType] * 8;
      Value := FIFD[Index].Offset;
      for I := 0 to FIFD[Index].DataLength - 1 do
      begin
        case FIFD[Index].DataType of
          TIFF_BYTE,
          TIFF_ASCII,
          TIFF_SBYTE,
          TIFF_UNDEFINED:
            Values[I] := Byte(Value);
          TIFF_SHORT,
          TIFF_SSHORT:
            // no byte swap needed here because values in the IFD are already swapped
            // (if necessary at all)
            Values[I] := Word(Value);
          TIFF_LONG,
          TIFF_SLONG:
            Values[I] := Value;
        end;
        Value := Value shr Shift;
      end;
    end
    else
    begin
      // data of this tag does not fit into one 32 bits value
      Stream.Position := FBasePosition + FIFD[Index].Offset;
      // even bytes sized data must be read by the loop as it is expanded to cardinals
      for I := 0 to High(Values) do
      begin
        Stream.Read(Value, DataTypeToSize[FIFD[Index].DataType]);
        case FIFD[Index].DataType of
          TIFF_BYTE:
            Value := Byte(Value);
          TIFF_SHORT,
          TIFF_SSHORT:
            begin
              if ioBigEndian in FImageProperties.Options then Value := Swap(Word(Value))
                                                         else Value := Word(Value);
            end;
          TIFF_LONG,
          TIFF_SLONG:
            if ioBigEndian in FImageProperties.Options then Value := SwapLong(Value);
        end;
        Values[I] := Value;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTIFFGraphic.GetValueList(Stream: TStream; Tag: Cardinal; var Values: TFloatArray);

// returns the values of the IFD entry indicated by Tag

var
  Index,
  Shift,
  IntValue: Cardinal;
  Value: Single;
  I: Integer;
  IntNominator,
  IntDenominator: Cardinal;
  FloatNominator,
  FloatDenominator: Cardinal;

begin
  Values := nil;
  if FindTag(Tag, Index) and
     (FIFD[Index].DataLength > 0) then
  begin
    // prepare value list
    SetLength(Values, FIFD[Index].DataLength);

    // determine whether the data fits into 4 bytes
    Value := DataTypeToSize[FIFD[Index].DataType] * FIFD[Index].DataLength;

    // data fits into one cardinal -> extract it
    if Value <= 4 then
    begin
      Shift := DataTypeToSize[FIFD[Index].DataType] * 8;
      IntValue := FIFD[Index].Offset;
      for I := 0 to FIFD[Index].DataLength - 1 do
      begin
        case FIFD[Index].DataType of
          TIFF_BYTE,
          TIFF_ASCII,
          TIFF_SBYTE,
          TIFF_UNDEFINED:
            Values[I] := Byte(IntValue);
          TIFF_SHORT,
          TIFF_SSHORT:
            // no byte swap needed here because values in the IFD are already swapped
            // (if necessary at all)
            Values[I] := Word(IntValue);
          TIFF_LONG,
          TIFF_SLONG:
            Values[I] := IntValue;
        end;
        IntValue := IntValue shr Shift;
      end;
    end
    else
    begin
      // data of this tag does not fit into one 32 bits value
      Stream.Position := FBasePosition + FIFD[Index].Offset;
      // even bytes sized data must be read by the loop as it is expanded to Single
      for I := 0 to High(Values) do
      begin
        case FIFD[Index].DataType of
          TIFF_BYTE:
            begin
              Stream.Read(IntValue, DataTypeToSize[FIFD[Index].DataType]);
              Value := Byte(IntValue);
            end;
          TIFF_SHORT,
          TIFF_SSHORT:
            begin
              Stream.Read(IntValue, DataTypeToSize[FIFD[Index].DataType]);
              if ioBigEndian in FImageProperties.Options then Value := Swap(Word(IntValue))
                                                         else Value := Word(IntValue);
            end;
          TIFF_LONG,
          TIFF_SLONG:
            begin
              Stream.Read(IntValue, DataTypeToSize[FIFD[Index].DataType]);
              if ioBigEndian in FImageProperties.Options then Value := SwapLong(IntValue);
            end;
          TIFF_RATIONAL,
          TIFF_SRATIONAL:
            begin
              Stream.ReadBuffer(FloatNominator, SizeOf(FloatNominator));
              Stream.ReadBuffer(FloatDenominator, SizeOf(FloatDenominator));
              if ioBigEndian in FImageProperties.Options then
              begin
                FloatNominator := SwapLong(Cardinal(FloatNominator));
                FloatDenominator := SwapLong(Cardinal(FloatDenominator));
              end;
              Value := FloatNominator / FloatDenominator;
            end;
          TIFF_FLOAT:
            begin
              Stream.ReadBuffer(IntNominator, SizeOf(IntNominator));
              Stream.ReadBuffer(IntDenominator, SizeOf(IntDenominator));
              if ioBigEndian in FImageProperties.Options then
              begin
                IntNominator := SwapLong(IntNominator);
                IntDenominator := SwapLong(IntDenominator);
              end;
              Value := IntNominator / IntDenominator;
            end;
          end;
        Values[I] := Value;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TTIFFGraphic.GetValue(Stream: TStream; Tag: Cardinal; Default: Single = 0): Single;

// returns the value of the IFD entry indicated by Tag or the default value if the entry is not there

var
  Index: Cardinal;
  IntNominator,
  IntDenominator: Cardinal;
  FloatNominator,
  FloatDenominator: Cardinal;

begin
  Result := Default;
  if FindTag(Tag, Index) then
  begin
    // if the data length is > 1 then Offset is a real offset into the stream,
    // otherwise it is the value itself and must be shortend depending on the data type
    if FIFD[Index].DataLength = 1 then
    begin
      case FIFD[Index].DataType of
        TIFF_BYTE:
          Result := Byte(FIFD[Index].Offset);
        TIFF_SHORT,
        TIFF_SSHORT:
          Result := Word(FIFD[Index].Offset);
        TIFF_LONG,
        TIFF_SLONG: // nothing to do
          Result := FIFD[Index].Offset;
        TIFF_RATIONAL,
        TIFF_SRATIONAL:
          begin
            Stream.Position := FBasePosition + FIFD[Index].Offset;
            Stream.ReadBuffer(FloatNominator, SizeOf(FloatNominator));
            Stream.ReadBuffer(FloatDenominator, SizeOf(FloatDenominator));
            if ioBigEndian in FImageProperties.Options then
            begin
              FloatNominator := SwapLong(Cardinal(FloatNominator));
              FloatDenominator := SwapLong(Cardinal(FloatDenominator));
            end;
            Result := FloatNominator / FloatDenominator;
          end;
        TIFF_FLOAT:
          begin
            Stream.Position := FBasePosition + FIFD[Index].Offset;
            Stream.ReadBuffer(IntNominator, SizeOf(IntNominator));
            Stream.ReadBuffer(IntDenominator, SizeOf(IntDenominator));
            if ioBigEndian in FImageProperties.Options then
            begin
              IntNominator := SwapLong(IntNominator);
              IntDenominator := SwapLong(IntDenominator);
            end;
            Result := IntNominator / IntDenominator;
          end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TTIFFGraphic.GetValue(Tag: Cardinal; Default: Cardinal = 0): Cardinal;

// returns the value of the IFD entry indicated by Tag or the default value if the entry is not there

var
  Index: Cardinal;

begin
  if not FindTag(Tag, Index) then Result := Default
                             else
  begin
    Result := FIFD[Index].Offset;
    // if the data length is > 1 then Offset is a real offset into the stream,
    // otherwise it is the value itself and must be shortend depending on the data type
    if FIFD[Index].DataLength = 1 then
    begin
      case FIFD[Index].DataType of
        TIFF_BYTE:
          Result := Byte(Result);
        TIFF_SHORT,
        TIFF_SSHORT:
          Result := Word(Result);
        TIFF_LONG,
        TIFF_SLONG: // nothing to do
          ;
      else
        Result := Default;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TTIFFGraphic.GetValue(Tag: Cardinal; var Size: Cardinal; Default: Cardinal): Cardinal;

// Returns the value of the IFD entry indicated by Tag or the default value if the entry is not there.
// If the tag exists then also the data size is returned.

var
  Index: Cardinal;

begin
  if not FindTag(Tag, Index) then
  begin
    Result := Default;
    Size := 0;
  end
  else
  begin
    Result := FIFD[Index].Offset;
    Size := FIFD[Index].DataLength;
    // if the data length is > 1 then Offset is a real offset into the stream,
    // otherwise it is the value itself and must be shortend depending on the data type
    if FIFD[Index].DataLength = 1 then
    begin
      case FIFD[Index].DataType of
        TIFF_BYTE:
          Result := Byte(Result);
        TIFF_SHORT,
        TIFF_SSHORT:
          Result := Word(Result);
        TIFF_LONG,
        TIFF_SLONG: // nothing to do
          ;
      else
        Result := Default;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTIFFGraphic.SortIFD;

// Although all entries in the IFD should be sorted there are still files where this is not the case.
// Because the lookup for certain tags in the IFD uses binary search it must be made sure the IFD is
// sorted (what we do here).

  //--------------- local function --------------------------------------------

  procedure QuickSort(L, R: Integer);

  var
    I, J, M: Integer;
    T: TIFDEntry;

  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      repeat
        while FIFD[I].Tag < FIFD[M].Tag do Inc(I);
        while FIFD[J].Tag > FIFD[M].Tag do Dec(J);
        if I <= J then
        begin
          T := FIFD[I];
          FIFD[I] := FIFD[J];
          FIFD[J] := T;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then QuickSort(L, J);
      L := I;
    until I >= R;
  end;

  //--------------- end local functions ---------------------------------------

begin
  QuickSort(0, High(FIFD));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTIFFGraphic.SwapIFD;

// swap the member fields of all entries of the currently loaded IFD from big endian to little endian

var
  I: Integer;
  Size: Cardinal;

begin
  for I := 0 to High(FIFD) do
    with FIFD[I] do
    begin
      Tag := Swap(Tag);
      DataType := Swap(DataType);
      DataLength := SwapLong(DataLength);
      
      // determine whether the data fits into 4 bytes
      Size := DataTypeToSize[FIFD[I].DataType] * FIFD[I].DataLength;
      if Size >= 4 then Offset := SwapLong(Offset)
                   else
        case DataType of
          TIFF_SHORT,
          TIFF_SSHORT:
            if DataLength > 1 then Offset := SwapLong(Offset)
                              else Offset := Swap(Word(Offset));
        end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTIFFGraphic.LoadFromStream(Stream: TStream);

var
  IFDCount: Word;
  Buffer: Pointer;
  Run: PChar;
  Pixels,
  EncodedData,
  DataPointerCopy: Pointer;
  Offsets,
  ByteCounts: TCardinalArray;
  ColorMap: Cardinal;

  StripSize: Cardinal;
  Decoder: TDecoder;

  // dynamically assigned handler
  Deprediction: procedure(P: Pointer; Count: Cardinal);

begin
  Handle := 0;
  Deprediction := nil;
  Decoder := nil;

  // we need to keep the current stream position because all position information
  // are relative to this one
  FBasePosition := Stream.Position;
  if ReadImageProperties(Stream, 0) then
  begin
    with FImageProperties do
    try
      // tiled images aren't supported
      if ioTiled in Options then Exit;
      
      FProgressRect := Rect(0, 0, 0, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      // read data of the first image file directory (IFD)
      Stream.Position := FBasePosition + FirstIFD;
      Stream.ReadBuffer(IFDCount, SizeOf(IFDCount));
      if ioBigEndian in Options then IFDCount := Swap(IFDCount);
      SetLength(FIFD, IFDCount);
      Stream.ReadBuffer(FIFD[0], IFDCount * SizeOf(TIFDEntry));
      if ioBigEndian in Options then SwapIFD;
      SortIFD;

      // --- read the data of the directory which are needed to actually load the image:

      // data organization
      GetValueList(Stream, TIFFTAG_STRIPOFFSETS, Offsets);
      GetValueList(Stream, TIFFTAG_STRIPBYTECOUNTS, ByteCounts);

      // retrive additional tile data if necessary
      if ioTiled in Options then
      begin
        GetValueList(Stream, TIFFTAG_TILEOFFSETS, Offsets);
        GetValueList(Stream, TIFFTAG_TILEBYTECOUNTS, ByteCounts);
      end;

      // determine pixelformat and setup color conversion
      with ColorManager do
      begin
        if ioBigEndian in Options then SourceOptions := [coNeedByteSwap]
                                  else SourceOptions := [];
        SourceBitsPerSample := BitsPerSample;
        if SourceBitsPerSample = 16 then TargetBitsPerSample := 8
                                    else TargetBitsPerSample := SourceBitsPerSample;

        // the JPEG lib does internally a conversion to RGB
        if Compression in [ctOJPEG, ctJPEG] then SourceColorScheme := csBGR
                                            else SourceColorScheme := ColorScheme;

        case SourceColorScheme of
          csRGBA:
            TargetColorScheme := csBGRA;
          csRGB:
            TargetColorScheme := csBGR;
          csCMY,
          csCMYK,
          csCIELab,
          csYCbCr:
            TargetColorScheme := csBGR;
          csIndexed:
            begin
              if HasAlpha then SourceColorScheme := csGA; // fake indexed images with alpha (used in EPS)
                                                          // as being grayscale with alpha
              TargetColorScheme := csIndexed;
            end;
        else
          TargetColorScheme := SourceColorScheme;
        end;

        SourceSamplesPerPixel := SamplesPerPixel;
        if SourceColorScheme = csCMYK then TargetSamplesPerPixel := 3
                                      else TargetSamplesPerPixel := SamplesPerPixel;
        if SourceColorScheme = csCIELab then SourceOptions := SourceOptions + [coLabByteRange];

        if SourceColorScheme = csGA then PixelFormat := pf8Bit
                                    else PixelFormat := TargetPixelFormat;
      end;
      
      // now that the pixel format is set we can also set the (possibly large) image dimensions
      Self.Width := Width;
      Self.Height := Height;
      if (Width = 0) or (Height = 0) then GraphicExError(gesInvalidImage, ['TIF/TIFF']);

      FProgressRect.Right := Width;
      if ColorManager.TargetColorScheme in [csIndexed, csG, csGA] then
      begin
        // load palette data and build palette
        if ColorManager.TargetColorScheme = csIndexed then
        begin
          ColorMap := GetValue(TIFFTAG_COLORMAP, StripSize, 0);
          if StripSize > 0 then
          begin
            Stream.Position := FBasePosition + ColorMap;
            // number of palette entries is also given by the color map tag
            // (3 components each (r,g,b) and two bytes per component)
            Stream.ReadBuffer(FPalette[0] , 2 * StripSize);
            Palette := ColorManager.CreateColorPalette([@FPalette[0], @FPalette[StripSize div 3],
                                                       @FPalette[2 * StripSize div 3]], pfPlane16Triple, StripSize, False);
          end;
        end
        else Palette := ColorManager.CreateGrayScalePalette(ioMinIsWhite in Options);
      end
      else
        if ColorManager.SourceColorScheme = csYCbCr then
          ColorManager.SetYCbCrParameters(FYCbCrCoefficients, YCbCrSubSampling[0], YCbCrSubSampling[1]);

      // intermediate buffer for data
      BytesPerLine := (BitsPerPixel * Width + 7) div 8;

      // determine prediction scheme
      if Compression <> ctNone then
      begin
        // Prediction without compression makes no sense at all (as it is to improve
        // compression ratios). Appearently there are image which are uncompressed but still
        // have a prediction scheme set. Hence we must check for it.
        case Predictor of
          PREDICTION_HORZ_DIFFERENCING: // currently only one prediction scheme is defined
            case SamplesPerPixel of
              4:
                Deprediction := Depredict4;
              3:
                Deprediction := Depredict3;
            else
              Deprediction := Depredict1;
            end;
        end;
      end;
      
      // create decompressor for the image
      case Compression of
        ctNone:
          ;
        {$ifdef UseLZW}
        ctLZW:
          Decoder := TTIFFLZWDecoder.Create;
        {$endif}
        ctPackedBits:
          Decoder := TPackbitsRLEDecoder.Create;
        ctFaxRLE,
        ctFaxRLEW:
          Decoder := TCCITTMHDecoder.Create(GetValue(TIFFTAG_GROUP3OPTIONS),
                                            ioReversed in Options,
                                            Compression = ctFaxRLEW,
                                            Width);
        ctFax3:
          Decoder := TCCITTFax3Decoder.Create(GetValue(TIFFTAG_GROUP3OPTIONS), ioReversed in Options, False, Width);
        ctJPEG:
          begin
            // some extra work is needed for JPEG
            GetValueList(Stream, TIFFTAG_JPEGTABLES, JPEGTables);

            Decoder := TTIFFJPEGDecoder.Create(@FImageProperties);
          end;
        ctThunderscan:
          Decoder := TThunderDecoder.Create(Width);
        ctLZ77:
          Decoder := TLZ77Decoder.Create(Z_PARTIAL_FLUSH, True);
      else
        {
        COMPRESSION_OJPEG,
        COMPRESSION_CCITTFAX4
        COMPRESSION_NEXT
        COMPRESSION_IT8CTPAD
        COMPRESSION_IT8LW
        COMPRESSION_IT8MP
        COMPRESSION_IT8BL
        COMPRESSION_PIXARFILM
        COMPRESSION_PIXARLOG
        COMPRESSION_DCS
        COMPRESSION_JBIG}
        GraphicExError(gesUnsupportedFeature, [gesCompressionScheme, 'TIF/TIFF']);
      end;

      if Assigned(Decoder) then Decoder.DecodeInit;
      Progress(Self, psEnding, 0, False, FProgressRect, '');
 
      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
      // go for each strip in the image (which might contain more than one line)
      CurrentRow := 0;
      CurrentStrip := 0;
      StripCount := Length(Offsets);
      while CurrentStrip < StripCount do
      begin
        Stream.Position := FBasePosition + Offsets[CurrentStrip];
        if CurrentStrip < Length(RowsPerStrip) then StripSize := BytesPerLine * RowsPerStrip[CurrentStrip]
                                               else StripSize := BytesPerLine * RowsPerStrip[High(RowsPerStrip)];

        GetMem(Buffer, StripSize);
        Run := Buffer;
        try
          // decompress strip if necessary
          if Assigned(Decoder) then
          begin
            GetMem(EncodedData, ByteCounts[CurrentStrip]);
            try
              DataPointerCopy := EncodedData;
              Stream.Read(EncodedData^, ByteCounts[CurrentStrip]);
              // need pointer copies here because they could get modified
              // while decoding
              Decoder.Decode(DataPointerCopy, Pointer(Run), ByteCounts[CurrentStrip], StripSize);
            finally
              if Assigned(EncodedData) then FreeMem(EncodedData);
            end;
          end
          else
          begin
            Stream.Read(Buffer^, StripSize);
          end;

          Run := Buffer;
          // go for each line (row) in the strip
          while (CurrentRow < Height) and ((Run - Buffer) < Integer(StripSize)) do
          begin
            Pixels := ScanLine[CurrentRow];
            // depredict strip if necessary
            if Assigned(Deprediction) then Deprediction(Run, Width - 1);
            // any color conversion comes last
            ColorManager.ConvertRow([Run], Pixels, Width, $FF);
            Inc(PChar(Run), BytesPerLine);
            Inc(CurrentRow);

            Progress(Self, psRunning, MulDiv(CurrentRow, 100, Height), True, FProgressRect, '');
            OffsetRect(FProgressRect, 0, 1);
          end;

        finally
          if Assigned(Buffer) then FreeMem(Buffer);
        end;

        Inc(CurrentStrip);
      end;
    finally
      Progress(Self, psEnding, 0, False, FProgressRect, '');

      if Assigned(Decoder) then Decoder.DecodeEnd;
      Decoder.Free;
    end;
  end
  else GraphicExError(gesInvalidImage, ['TIF/TIFF']);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTIFFGraphic.SaveToStream(Stream: TStream);

begin
end;

//----------------------------------------------------------------------------------------------------------------------

function TTIFFGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

// Reads all relevant TIF properties of the image of index ImageIndex (zero based).
// Returns True if the image ImageIndex could be read, otherwise False.

const
  PhotometricToColorScheme: array[PHOTOMETRIC_MINISWHITE..PHOTOMETRIC_CIELAB] of TColorScheme = (
    csG,
    csG,
    csRGBA,
    csIndexed,
    csUnknown,
    csCMYK,
    csYCbCr,
    csUnknown,
    csCIELab
  );

var
  IFDCount: Word;
  ExtraSamples: TCardinalArray;
  PhotometricInterpretation: Byte;
  TIFCompression: Word;
  Index: Cardinal;
  
  IFDOffset: Cardinal;
  Header: TTIFFHeader;
  LocalBitsPerSample: TCardinalArray;
  
begin
  // clear image properties
  Result := inherited ReadImageProperties(Stream, ImageIndex);

  with FImageProperties do
  begin
    // rewind stream to header position
    Stream.Position := FBasePosition;

    Stream.ReadBuffer(Header, SizeOf(Header));
    if Header.ByteOrder = TIFF_BIGENDIAN then
    begin
      Options := Options + [ioBigEndian];
      Header.Version := Swap(Header.Version);
      Header.FirstIFD := SwapLong(Header.FirstIFD);
    end;

    Version := Header.Version;
    FirstIFD := Header.FirstIFD;
    if Version = TIFF_VERSION then
    begin
      IFDOffset := Header.FirstIFD;
      // advance to next IFD until we have the desired one
      repeat
        Stream.Position := FBasePosition + IFDOffset;
        // number of entries in this directory
        Stream.ReadBuffer(IFDCount, SizeOf(IFDCount));
        if Header.ByteOrder = TIFF_BIGENDIAN then IFDCount := Swap(IFDCount);

        // if we already have the desired image then get out of here
        if ImageIndex = 0 then Break;

        Dec(ImageIndex);
        // advance to offset for next IFD
        Stream.Seek(IFDCount * SizeOf(TIFDEntry), soFromCurrent);
        Stream.ReadBuffer(IFDOffset, SizeOf(IFDOffset));
        // no further image available, but the required index is still not found
        if IFDOffset = 0 then Exit;
      until False;

      SetLength(FIFD, IFDCount);
      Stream.ReadBuffer(FIFD[0], IFDCount * SizeOf(TIFDEntry));
      if Header.ByteOrder = TIFF_BIGENDIAN then SwapIFD;
      SortIFD;

      Width := GetValue(TIFFTAG_IMAGEWIDTH);
      Height := GetValue(TIFFTAG_IMAGELENGTH);
      if (Width = 0) or (Height = 0) then Exit;

      // data organization
      GetValueList(Stream, TIFFTAG_ROWSPERSTRIP, RowsPerStrip);
      // some images rely on the default size ($FFFFFFFF) if only one stripe is in the image,
      // make sure there's a valid value also in this case
      if (Length(RowsPerStrip) = 0) or (RowsPerStrip[0] = $FFFFFFFF) then
      begin
        SetLength(RowsPerStrip, 1);
        RowsPerStrip[0] := Height;
      end;

      // number of color components per pixel (1 for b&w, 16 and 256 colors, 3 for RGB, 4 for CMYK etc.)
      SamplesPerPixel := GetValue(TIFFTAG_SAMPLESPERPIXEL, 1);

      // number of bits per color component
      GetValueList(Stream, TIFFTAG_BITSPERSAMPLE, LocalBitsPerSample);
      if Length(LocalBitsPerSample) = 0 then BitsPerSample := 1
                                        else BitsPerSample := LocalBitsPerSample[0];

      // determine whether image is tiled and retrive tile data if necessary
      TileWidth := GetValue(TIFFTAG_TILEWIDTH, 0);
      TileLength := GetValue(TIFFTAG_TILELENGTH, 0);
      if  (TileWidth > 0) and (TileLength > 0) then Include(Options, ioTiled);

      // photometric interpretation determines the color space
      PhotometricInterpretation := GetValue(TIFFTAG_PHOTOMETRIC);
      // type of extra information for additional samples per pixel
      GetValueList(Stream, TIFFTAG_EXTRASAMPLES, ExtraSamples);

      // determine whether extra samples must be considered
      HasAlpha := Length(ExtraSamples) > 0;
      // if any of the extra sample contains an invalid value then consider
      // it as being not existant to avoid wrong interpretation for badly
      // written images
      if HasAlpha then
      begin
        for Index := 0 to High(ExtraSamples) do
          if ExtraSamples[Index] > EXTRASAMPLE_UNASSALPHA then
          begin
            HasAlpha := False;
            Break;
          end;
      end;

      // currently all bits per sample values are equal
      BitsPerPixel := BitsPerSample * SamplesPerPixel;

      // create decompressor for the image
      TIFCompression := GetValue(TIFFTAG_COMPRESSION);
      case TIFCompression of
        COMPRESSION_NONE:
          Compression := ctNone;
        COMPRESSION_LZW:
          Compression := ctLZW;
        COMPRESSION_PACKBITS:
          Compression := ctPackedBits;
        COMPRESSION_CCITTRLE:
          Compression := ctFaxRLE;
        COMPRESSION_CCITTRLEW:
          Compression := ctFaxRLEW;
        COMPRESSION_CCITTFAX3:
          Compression := ctFax3;
        COMPRESSION_OJPEG:
          Compression := ctOJPEG;
        COMPRESSION_JPEG:
          Compression := ctJPEG;
        COMPRESSION_CCITTFAX4:
          Compression := ctFax4;
        COMPRESSION_NEXT:
          Compression := ctNext;
        COMPRESSION_THUNDERSCAN:
          Compression := ctThunderscan;
        COMPRESSION_IT8CTPAD:
          Compression := ctIT8CTPAD;
        COMPRESSION_IT8LW:
          Compression := ctIT8LW;
        COMPRESSION_IT8MP:
          Compression := ctIT8MP;
        COMPRESSION_IT8BL:
          Compression := ctIT8BL;
        COMPRESSION_PIXARFILM:
          Compression := ctPixarFilm;
        COMPRESSION_PIXARLOG: // also a LZ77 clone
          Compression := ctPixarLog;
        COMPRESSION_ADOBE_DEFLATE,
        COMPRESSION_DEFLATE: 
          Compression := ctLZ77;
        COMPRESSION_DCS:
          Compression := ctDCS;
        COMPRESSION_JBIG:
          Compression := ctJBIG;
      else
        Compression := ctUnknown;
      end; 

      if PhotometricInterpretation in [PHOTOMETRIC_MINISWHITE..PHOTOMETRIC_CIELAB] then
      begin
        ColorScheme := PhotometricToColorScheme[PhotometricInterpretation];
        if (PhotometricInterpretation = PHOTOMETRIC_RGB) and (SamplesPerPixel < 4) then ColorScheme := csRGB;
        if PhotometricInterpretation = PHOTOMETRIC_MINISWHITE then Include(Options, ioMinIsWhite);

        // extra work necessary for YCbCr
        if PhotometricInterpretation = PHOTOMETRIC_YCBCR then
        begin
          if FindTag(TIFFTAG_YCBCRSUBSAMPLING, Index)
            then GetValueList(Stream, TIFFTAG_YCBCRSUBSAMPLING, YCbCrSubSampling)
            else
            begin
              // initialize default values if nothing is given in the file
              SetLength(YCbCrSubSampling, 2);
              YCbCrSubSampling[0] := 2;
              YCbCrSubSampling[1] := 2;
            end;
          if FindTag(TIFFTAG_YCBCRPOSITIONING, Index) then FYCbCrPositioning := GetValue(TIFFTAG_YCBCRPOSITIONING)
                                                      else FYCbCrPositioning := YCBCRPOSITION_CENTERED;

          if FindTag(TIFFTAG_YCBCRCOEFFICIENTS, Index)
            then GetValueList(Stream, TIFFTAG_YCBCRCOEFFICIENTS, FYCbCrCoefficients)
            else
            begin
              // defaults are from CCIR recommendation 601-1
              SetLength(FYCbCrCoefficients, 3);
              FYCbCrCoefficients[0] := 0.299;
              FYCbCrCoefficients[1] := 0.587;
              FYCbCrCoefficients[2] := 0.114;
            end;
        end;
      end
      else ColorScheme := csUnknown;

      JPEGColorMode := GetValue(TIFFTAG_JPEGCOLORMODE, JPEGCOLORMODE_RAW);
      JPEGTablesMode := GetValue(TIFFTAG_JPEGTABLESMODE, JPEGTABLESMODE_QUANT or JPEGTABLESMODE_HUFF);

      PlanarConfig := GetValue(TIFFTAG_PLANARCONFIG);
      // other image properties
      XResolution := GetValue(Stream, TIFFTAG_XRESOLUTION);
      YResolution := GetValue(Stream, TIFFTAG_YRESOLUTION);
      if GetValue(TIFFTAG_RESOLUTIONUNIT, RESUNIT_INCH) = RESUNIT_CENTIMETER then
      begin
        // Resolution is given in centimeters.
        // Although I personally prefer the metric system over the old english one :-)
        // I still convert to inches because it is an unwritten rule to give image resolutions in dpi.
        XResolution := XResolution * 2.54;
        YResolution := YResolution * 2.54;
      end;

      // determine prediction scheme
      Predictor := GetValue(TIFFTAG_PREDICTOR);

      // determine fill order in bytes
      if GetValue(TIFFTAG_FILLORDER, FILLORDER_MSB2LSB) = FILLORDER_LSB2MSB then Include(Options, ioReversed);

      // finally show that we found and read an image
      Result := True;
    end;
  end;
end;

//----------------- TEPSGraphic ----------------------------------------------------------------------------------------

{$ifdef EPSGraphic}

// Note: This EPS implementation does only read embedded pixel graphics in TIF format (preview).
// Credits to:
//   Olaf Stieleke
//   Torsten Pohlmeyer
//   CPS Krohn GmbH
// for providing the base information about how to read the preview image.

type
  TEPSHeader = packed record
    Code: Cardinal;   // alway $C6D3D0C5, if not there then this is not an EPS or it is not a binary EPS
    PSStart,          // Offset PostScript-Code
    PSLen,	          // length of PostScript-Code
    MetaPos,          // position of a WMF
    MetaLen,          // length of a WMF 
    TiffPos,          // position of TIFF (preview images should be either WMF or TIF but not both)
    TiffLen: Integer; // length of the TIFF
    Checksum: SmallInt;
  end;

//----------------------------------------------------------------------------------------------------------------------

class function TEPSGraphic.CanLoad(Stream: TStream): Boolean;

var
  Header: TEPSHeader;
  LastPosition: Cardinal;

begin
  with Stream do
  begin
    Result := (Size - Position) > SizeOf(Header);
    if Result then
    begin
      LastPosition := Position;

      Stream.ReadBuffer(Header, SizeOf(Header));
      Result := (Header.Code = $C6D3D0C5) and
                (Header.TiffPos > Integer(LastPosition) + SizeOf(Header)) and
                (Header.TiffLen > 0);
      Position := LastPosition;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEPSGraphic.LoadFromStream(Stream: TStream);

var
  Header: TEPSHeader;

begin
  Stream.ReadBuffer(Header, SizeOf(Header));
  if Header.Code <> $C6D3D0C5 then GraphicExError(gesInvalidImage, ['EPS']);
  Stream.Seek(Header.TiffPos - SizeOf(Header), soFromCurrent);
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TEPSGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

begin
  Result := inherited ReadImageProperties(Stream, ImageIndex);
end;

{$endif} // EPSGraphic

{$endif} // TIFFGraphic

//----------------- TTargaGraphic --------------------------------------------------------------------------------------

{$ifdef TargaGraphic}

//  FILE STRUCTURE FOR THE ORIGINAL TRUEVISION TGA FILE
//    FIELD 1: NUMBER OF CHARACTERS IN ID FIELD (1 BYTES)
//    FIELD 2: COLOR MAP TYPE (1 BYTES)
//    FIELD 3: IMAGE TYPE CODE (1 BYTES)
//      = 0  NO IMAGE DATA INCLUDED
//      = 1  UNCOMPRESSED, COLOR-MAPPED IMAGE
//      = 2  UNCOMPRESSED, TRUE-COLOR IMAGE
//      = 3  UNCOMPRESSED, BLACK AND WHITE IMAGE (black and white is actually grayscale)
//      = 9  RUN-LENGTH ENCODED COLOR-MAPPED IMAGE
//      = 10 RUN-LENGTH ENCODED TRUE-COLOR IMAGE
//      = 11 RUN-LENGTH ENCODED BLACK AND WHITE IMAGE
//    FIELD 4: COLOR MAP SPECIFICATION (5 BYTES)
//      4.1: COLOR MAP ORIGIN (2 BYTES)
//      4.2: COLOR MAP LENGTH (2 BYTES)
//      4.3: COLOR MAP ENTRY SIZE (1 BYTES)
//    FIELD 5:IMAGE SPECIFICATION (10 BYTES)
//      5.1: X-ORIGIN OF IMAGE (2 BYTES)
//      5.2: Y-ORIGIN OF IMAGE (2 BYTES)
//      5.3: WIDTH OF IMAGE (2 BYTES)
//      5.4: HEIGHT OF IMAGE (2 BYTES)
//      5.5: IMAGE PIXEL SIZE (1 BYTE)
//      5.6: IMAGE DESCRIPTOR BYTE (1 BYTE)
//        bit 0..3: attribute bits per pixel
//        bit 4..5: image orientation:
//          0: bottom left
//          1: bottom right
//          2: top left
//          3: top right
//        bit 6..7: interleaved flag
//          0: two way (even-odd) interleave (e.g. IBM Graphics Card Adapter), obsolete
//          1: four way interleave (e.g. AT&T 6300 High Resolution), obsolete
//    FIELD 6: IMAGE ID FIELD (LENGTH SPECIFIED BY FIELD 1)
//    FIELD 7: COLOR MAP DATA (BIT WIDTH SPECIFIED BY FIELD 4.3 AND
//             NUMBER OF COLOR MAP ENTRIES SPECIFIED IN FIELD 4.2)
//    FIELD 8: IMAGE DATA FIELD (WIDTH AND HEIGHT SPECIFIED IN FIELD 5.3 AND 5.4)

const
  TARGA_NO_COLORMAP = 0;
  TARGA_COLORMAP = 1;

  TARGA_EMPTY_IMAGE = 0;
  TARGA_INDEXED_IMAGE = 1;
  TARGA_TRUECOLOR_IMAGE = 2;
  TARGA_BW_IMAGE = 3;
  TARGA_INDEXED_RLE_IMAGE = 9;
  TARGA_TRUECOLOR_RLE_IMAGE = 10;
  TARGA_BW_RLE_IMAGE = 11;

type
  TTargaHeader = packed record
    IDLength,
    ColorMapType,
    ImageType: Byte;
    ColorMapOrigin,
    ColorMapSize: Word;
    ColorMapEntrySize: Byte;
    XOrigin,
    YOrigin,
    Width,
    Height: Word;
    PixelSize: Byte;
    ImageDescriptor: Byte;
  end;


//----------------------------------------------------------------------------------------------------------------------

class function TTargaGraphic.CanLoad(Stream: TStream): Boolean;

var
  Header: TTargaHeader;
  LastPosition: Cardinal;
  
begin
  with Stream do
  begin
    LastPosition := Position;
    Result := (Size - Position) > SizeOf(Header);
    if Result then
    begin
      ReadBuffer(Header, SizeOf(Header));
      // Targa images are hard to determine because there is no magic id or something like that.
      // Hence all we can do is to check if all values from the header are within correct limits.
      Result := (Header.ImageType in [TARGA_EMPTY_IMAGE, TARGA_INDEXED_IMAGE, TARGA_TRUECOLOR_IMAGE, TARGA_BW_IMAGE,
                 TARGA_INDEXED_RLE_IMAGE, TARGA_TRUECOLOR_RLE_IMAGE, TARGA_BW_RLE_IMAGE]) and
                 (Header.ColorMapType in [TARGA_NO_COLORMAP, TARGA_COLORMAP]) and
                 (Header.ColorMapEntrySize in [15, 16, 24, 32]) and
                 (Header.PixelSize in [8, 15, 16, 24, 32]);
    end;
    Position := LastPosition;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTargaGraphic.LoadFromStream(Stream: TStream);

var
  Run,
  RLEBuffer: PChar;
  I: Integer;
  LineSize: Integer;
  LineBuffer: Pointer;
  ReadLength: Integer;
  LogPalette: TMaxLogPalette;
  Color16: Word;
  Header: TTargaHeader;
  FlipV: Boolean;
  Decoder: TTargaRLEDecoder;

begin
  Handle := 0;
  FBasePosition := Stream.Position;
  if ReadImageProperties(Stream, 0) then
    with Stream, FImageProperties do
    begin
      Stream.Position := FBasePosition;

      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      Stream.Read(Header, SizeOf(Header));
      FlipV := (Header.ImageDescriptor and $20) <> 0;
      Header.ImageDescriptor := Header.ImageDescriptor and $F;

      // skip image ID
      Seek(Header.IDLength, soFromCurrent);

      with ColorManager do
      begin
        SourceSamplesPerPixel := SamplesPerPixel;
        TargetSamplesPerPixel := SamplesPerPixel;
        SourceColorScheme := ColorScheme;
        SourceOptions := [];
        TargetColorScheme := csBGR;
        SourceBitsPerSample := BitsPerSample;
        TargetBitsPerSample := BitsPerSample;
        PixelFormat := TargetPixelFormat;
      end;
      
      if (Header.ColorMapType = TARGA_COLORMAP) or
         (Header.ImageType in [TARGA_BW_IMAGE, TARGA_BW_RLE_IMAGE]) then
      begin
        if Header.ImageType in [TARGA_BW_IMAGE, TARGA_BW_RLE_IMAGE] then
          Palette := ColorManager.CreateGrayscalePalette(False)
        else
        begin
          LineSize := (Header.ColorMapEntrySize div 8) * Header.ColorMapSize;
          GetMem(LineBuffer, LineSize);
          try
            ReadBuffer(LineBuffer^, LineSize);
            case Header.ColorMapEntrySize of
              32:
                Palette := ColorManager.CreateColorPalette([LineBuffer], pfInterlaced8Quad, Header.ColorMapSize, True);
              24:
                Palette := ColorManager.CreateColorPalette([LineBuffer], pfInterlaced8Triple, Header.ColorMapSize, True);
            else
              with LogPalette do
              begin
                // read palette entries and create a palette
                ZeroMemory(@LogPalette, SizeOf(LogPalette));
                palVersion := $300;
                palNumEntries := Header.ColorMapSize;

                // 15 and 16 bits per color map entry (handle both like 555 color format
                // but make 8 bit from 5 bit per color component)
                for I := 0 to Header.ColorMapSize - 1 do
                begin
                  Stream.Read(Color16, 2);
                  palPalEntry[I].peBlue := (Color16 and $1F) shl 3;
                  palPalEntry[I].peGreen := (Color16 and $3E0) shr 2;
                  palPalEntry[I].peRed := (Color16 and $7C00) shr 7;
                end;
                Palette := CreatePalette(PLogPalette(@LogPalette)^);
              end;
            end;
          finally
            if Assigned(LineBuffer) then FreeMem(LineBuffer);
          end;
        end;
      end;

      Self.Width := Header.Width;
      Self.Height := Header.Height;

      LineSize := Width * (Header.PixelSize div 8);
      Progress(Self, psEnding, 0, False, FProgressRect, '');

      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
      case Header.ImageType of
        TARGA_EMPTY_IMAGE: // nothing to do here
          ;
        TARGA_BW_IMAGE,
        TARGA_INDEXED_IMAGE,
        TARGA_TRUECOLOR_IMAGE:
          begin
            for I := 0 to Height - 1 do
            begin
              if FlipV then LineBuffer := ScanLine[I]
                       else LineBuffer := ScanLine[Header.Height - (I + 1)];
              ReadBuffer(LineBuffer^, LineSize);
              Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            end;
          end;
        TARGA_BW_RLE_IMAGE,
        TARGA_INDEXED_RLE_IMAGE,
        TARGA_TRUECOLOR_RLE_IMAGE:
          begin
            RLEBuffer := nil;
            Decoder := TTargaRLEDecoder.Create(Header.PixelSize);
            try
              GetMem(RLEBuffer, 2 * LineSize);
              for I := 0 to Height - 1 do
              begin
                if FlipV then LineBuffer := ScanLine[I]
                         else LineBuffer := ScanLine[Header.Height - (I + 1)];
                ReadLength := Stream.Read(RLEBuffer^, 2 * LineSize);
                Run := RLEBuffer;
                Decoder.Decode(Pointer(Run), LineBuffer, 2 * LineSize, Width);
                Stream.Position := Stream.Position - ReadLength + (Run - RLEBuffer);
                Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            finally
              if Assigned(RLEBuffer) then FreeMem(RLEBuffer);
              Decoder.Free;
            end;
          end;
      end;
      Progress(Self, psEnding, 0, False, FProgressRect, '');
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TTargaGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

var
  Header: TTargaHeader;

begin
  inherited ReadImageProperties(Stream, ImageIndex);
  with Stream, FImageProperties do
  begin
    ReadBuffer(Header, SizeOf(Header));
    Header.ImageDescriptor := Header.ImageDescriptor and $F;

    Width := Header.Width;
    Height := Header.Height;
    BitsPerSample := 8;

    case Header.PixelSize of
      8:
        begin
          if Header.ImageType in [TARGA_BW_IMAGE, TARGA_BW_RLE_IMAGE] then ColorScheme := csG
                                                                      else ColorScheme := csIndexed;
          SamplesPerPixel := 1;
        end;
      15,
      16: // actually, 16 bit are meant being 15 bit
        begin
          ColorScheme := csRGB;
          BitsPerSample := 5;
          SamplesPerPixel := 3;
        end;
      24:
        begin
          ColorScheme := csRGB;
          SamplesPerPixel := 3;
        end;
      32:
        begin
          ColorScheme := csRGBA;
          SamplesPerPixel := 4;
        end;
    end;

    BitsPerPixel := SamplesPerPixel * BitsPerSample;
    if Header.ImageType in [TARGA_BW_RLE_IMAGE, TARGA_INDEXED_RLE_IMAGE, TARGA_TRUECOLOR_RLE_IMAGE]
      then Compression := ctRLE
      else Compression := ctNone;

    Width := Header.Width;
    Height := Header.Height;
    Result := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTargaGraphic.SaveToStream(Stream: TStream);

begin
  SaveToStream(Stream, True);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTargaGraphic.SaveToStream(Stream: TStream; Compressed: Boolean);

// The format of the image to be saved depends on the current properties of the bitmap not
// on the values which may be set in the header during a former load.

var
  RLEBuffer: Pointer;
  I: Integer;
  LineSize: Integer;
  WriteLength: Cardinal;
  LogPalette: TMaxLogPalette;
  BPP: Byte;
  Header: TTargaHeader;
  Encoder: TTargaRLEDecoder;
  
begin
  FProgressRect := Rect(0, 0, Width, 1);
  Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);
  // prepare color depth
  case PixelFormat of
    pf1Bit,
    pf4Bit: // Note: 1 bit and 4 bits per pixel are not supported in the Targa format, an image
            //       with one of these pixel formats is implicitly converted to 256 colors.
      begin
        PixelFormat := pf8Bit;
        BPP := 1;
      end;
    pf8Bit:
      BPP := 1;
    pf15Bit,
    pf16Bit:
      BPP := 2;
    pf24Bit:
      BPP := 3;
    pf32Bit:
      BPP := 4;
  else
    BPP := GetDeviceCaps(Canvas.Handle, BITSPIXEL) div 8;
  end;

  if not Empty then
  begin
    with Header do
    begin
      IDLength := 0;
      if BPP = 1 then ColorMapType := 1
                 else ColorMapType := 0;
      if not Compressed then
        // can't distinct between a B&W and an color indexed image here, so I use always the latter
        if BPP = 1 then ImageType := TARGA_INDEXED_IMAGE
                   else ImageType := TARGA_TRUECOLOR_IMAGE
                        else
        if BPP = 1 then ImageType := TARGA_INDEXED_RLE_IMAGE
                   else ImageType := TARGA_TRUECOLOR_RLE_IMAGE;

      ColorMapOrigin := 0;
      // always save entire palette
      ColorMapSize := 256;
      // always save complete color information
      ColorMapEntrySize := 24;
      XOrigin := 0;
      YOrigin := 0;
      Width := Self.Width;
      Height := Self.Height;
      PixelSize := 8 * BPP;
      // if the image is a bottom-up DIB then indicate this in the image descriptor
      if Cardinal(Scanline[0]) > Cardinal(Scanline[1]) then ImageDescriptor := $20
                                                       else ImageDescriptor := 0;
    end;
  
    Stream.Write(Header, SizeOf(Header));

    // store color palette if necessary
    if Header.ColorMapType = 1 then
      with LogPalette do
      begin
        // read palette entries
        GetPaletteEntries(Palette, 0, 256, palPalEntry);
        for I := 0 to 255 do
        begin
          Stream.Write(palPalEntry[I].peBlue, 1);
          Stream.Write(palPalEntry[I].peGreen, 1);
          Stream.Write(palPalEntry[I].peRed, 1);
        end;
      end;

    LineSize := Width * (Header.PixelSize div 8);
    Progress(Self, psEnding, 0, False, FProgressRect, '');

    Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
    // finally write image data
    if Compressed then
    begin
      RLEBuffer := nil;
      Encoder := TTargaRLEDecoder.Create(Header.PixelSize);
      try
        GetMem(RLEBuffer, 2 * LineSize);
        for I := 0 to Height - 1 do
        begin
          Encoder.Encode(ScanLine[I], RLEBuffer, Width, WriteLength);
          Stream.WriteBuffer(RLEBuffer^, WriteLength);

          Progress(Self, psRunning, 0, False, FProgressRect, '');
          OffsetRect(FProgressRect, 0, 1);
        end;
      finally
        if Assigned(RLEBuffer) then FreeMem(RLEBuffer);
        Encoder.Free;
      end;
    end
    else
    begin
      for I := 0 to Height - 1 do
      begin
        Stream.WriteBuffer(ScanLine[I]^, LineSize);

        Progress(Self, psRunning, 0, False, FProgressRect, '');
        OffsetRect(FProgressRect, 0, 1);
      end;
    end;

    Progress(Self, psEnding, 0, False, FProgressRect, '');
  end;
end;

{$endif} // TargaGraphic

//----------------- TPCXGraphic ----------------------------------------------------------------------------------------

{$ifdef PCXGraphic}

type
  TPCXHeader = record
    FileID: Byte;                      // $0A for PCX files, $CD for SCR files
    Version: Byte;                     // 0: version 2.5; 2: 2.8 with palette; 3: 2.8 w/o palette; 5: version 3
    Encoding: Byte;                    // 0: uncompressed; 1: RLE encoded
    BitsPerPixel: Byte;
    XMin,
    YMin,
    XMax,
    YMax,                              // coordinates of the corners of the image
    HRes,                              // horizontal resolution in dpi
    VRes: Word;                        // vertical resolution in dpi
    ColorMap: array[0..15] of TRGB;    // color table
    Reserved,
    ColorPlanes: Byte;                 // color planes (at most 4)
    BytesPerLine,                      // number of bytes of one line of one plane
    PaletteType: Word;                 // 1: color or b&w; 2: gray scale
    Fill: array[0..57] of Byte;
  end;

//----------------------------------------------------------------------------------------------------------------------

class function TPCXGraphic.CanLoad(Stream: TStream): Boolean;

var
  Header: TPCXHeader;
  LastPosition: Cardinal;

begin
  with Stream do
  begin
    LastPosition := Position;
    Result := (Size - Position) > SizeOf(Header);
    if Result then
    begin
      Result := (Header.FileID in [$0A, $0C]) and
                (Header.Version in [0, 2, 3, 5]) and
                (Header.Encoding in [0, 1]);
      ReadBuffer(Header, SizeOf(Header));
    end;
    Position := LastPosition;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPCXGraphic.LoadFromStream(Stream: TStream);

var
  Header: TPCXHeader;

  //--------------- local functions -------------------------------------------

  procedure MakePalette;

  var
    PCXPalette: array[0..255] of TRGB;
    OldPos: Integer;
    Marker: Byte;

  begin
    if (Header.Version <> 3) or (PixelFormat = pf1Bit) then
    begin
      case PixelFormat of
        pf1Bit:
          Palette := ColorManager.CreateGrayScalePalette(False);
        pf4Bit:
          with Header do
          begin
            if paletteType = 2 then Palette := ColorManager.CreateGrayScalePalette(False)
                               else Palette := ColorManager.CreateColorPalette([@ColorMap], pfInterlaced8Triple, 16, False);
          end;
        pf8Bit:
          begin
            OldPos := Stream.Position;
            // 256 colors with 3 components plus one marker byte
            Stream.Position := Stream.Size - 769;
            Stream.Read(Marker, 1);
            if Marker <> $0C then
            begin
              // palette ID is wrong, perhaps gray scale?
              if Header.PaletteType = 2 then Palette := ColorManager.CreateGrayScalePalette(False)
                                        else ; // ignore palette
            end
            else
            begin
              Stream.Read(PCXPalette[0], 768);
              Palette := ColorManager.CreateColorPalette([@PCXPalette], pfInterlaced8Triple, 256, False);
            end;
            Stream.Position := OldPos;
          end;
      end;
    end
    else
    begin
      // version 2.8 without palette information, just use the system palette
      // 256 colors will not be correct with this assignment...
      Palette := SystemPalette16;
    end;
  end;

  //--------------- end local functions ---------------------------------------

var
  PCXSize,
  Size: Cardinal;
  RawBuffer,
  DecodeBuffer: Pointer;
  Run: PByte;
  Plane1,
  Plane2,
  Plane3,
  Plane4: PByte;
  Value,
  Mask: Byte;
  I, J: Integer;
  Line: PByte;
  Increment: Cardinal;
  NewPixelFormat: TPixelFormat;

begin
  Handle := 0;
  FBasePosition := Stream.Position;
  if ReadImageProperties(Stream, 0) then
  begin
    Stream.Position := FBasePosition;

    FProgressRect := Rect(0, 0, Width, 1);
    Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);
    Stream.Read(Header, SizeOf(Header));
    PCXSize := Stream.Size - Stream.Position;
    with Header, FImageProperties do
    begin
      if not (FileID in [$0A, $CD]) then GraphicExError(gesInvalidImage, ['PCX or SCR']);

      with ColorManager do
      begin
        SourceColorScheme := ColorScheme;
        SourceBitsPerSample := BitsPerSample;
        SourceSamplesPerPixel := SamplesPerPixel;
        if ColorScheme = csIndexed then TargetColorScheme := csIndexed
                                   else TargetColorScheme := csBGR;
        if BitsPerPixel = 2 then TargetBitsPerSample := 4
                            else TargetBitsPerSample := BitsPerSample;
        // Note: pixel depths of 2 and 4 bits may not be used with more than one plane
        //       otherwise the image will not show up correctly
        TargetSamplesPerPixel := SamplesPerPixel;
      end;
    
      NewPixelFormat := ColorManager.TargetPixelFormat;
      if NewPixelFormat = pfCustom then
      begin
        // there can be a special case comprising 4 planes each with 1 bit
        if (SamplesPerPixel = 4) and (BitsPerPixel = 4) then NewPixelFormat := pf4Bit
                                                        else GraphicExError(gesInvalidColorFormat, ['PCX']);
      end;

      PixelFormat := NewPixelFormat;
      // 256 colors palette is appended to the actual PCX data
      if PixelFormat = pf8Bit then Dec(PCXSize, 769);
      if PixelFormat <> pf24Bit then MakePalette;

      Self.Width := Width;
      Self.Height := Height;
                                                  
      // adjust alignment of line
      Increment := SamplesPerPixel * Header.BytesPerLine;

      // allocate pixel data buffer and decode data if necessary
      if Compression = ctRLE then
      begin
        Size := Increment * Height;
        GetMem(DecodeBuffer, Size);

        GetMem(RawBuffer, PCXSize);
        try
          Stream.ReadBuffer(RawBuffer^, PCXSize);
          with TPCXRLEDecoder.Create do
          try
            Decode(RawBuffer, DecodeBuffer, PCXSize, Size);
          finally
            Free;
          end;
        finally
          if Assigned(RawBuffer) then FreeMem(RawBuffer);
        end;
      end
      else
      begin
        GetMem(DecodeBuffer, PCXSize);
        Stream.ReadBuffer(DecodeBuffer^, PCXSize);
      end;
      Progress(Self, psEnding, 0, False, FProgressRect, '');

      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
      try
        Run := DecodeBuffer;

        if (SamplesPerPixel = 4) and (BitsPerPixel = 4) then
        begin
          // 4 planes with one bit

          for I := 0 to Height - 1 do
          begin
            Plane1 := Run;
            PChar(Plane2) := PChar(Run) + Increment div 4;
            PChar(Plane3) := PChar(Run) + 2 * (Increment div 4);
            PChar(Plane4) := PChar(Run) + 3 * (Increment div 4);

            Line := ScanLine[I];
            // number of bytes to write
            Size := (Width * BitsPerPixel + 7) div 8;
            Mask := 0;
            while Size > 0 do
            begin
              Value := 0;
              for J := 0 to 1 do
              asm
                MOV AL, [Value]

                MOV EDX, [Plane4]             // take the 4 MSBs from the 4 runs and build a nibble
                SHL BYTE PTR [EDX], 1         // read MSB and prepare next run at the same time
                RCL AL, 1                     // MSB from previous shift is in CF -> move it to AL

                MOV EDX, [Plane3]             // now do the same with the other three runs
                SHL BYTE PTR [EDX], 1
                RCL AL, 1

                MOV EDX, [Plane2]
                SHL BYTE PTR [EDX], 1
                RCL AL, 1

                MOV EDX, [Plane1]
                SHL BYTE PTR [EDX], 1
                RCL AL, 1

                MOV [Value], AL
              end;
              Line^ := Value;
              Inc(Line);
              Dec(Size);

              // two runs above (to construct two nibbles -> one byte), now update marker
              // to know when to switch to next byte in the planes
              Mask := (Mask + 2) mod 8;
              if Mask = 0 then
              begin
                Inc(Plane1);
                Inc(Plane2);
                Inc(Plane3);
                Inc(Plane4);
              end;
            end;
            Inc(Run, Increment);
            
            Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
            OffsetRect(FProgressRect, 0, 1);
          end;
        end
        else
          if PixelFormat = pf24Bit then
          begin
            // true color
            for I := 0 to Height - 1 do
            begin
              Line := ScanLine[I];
              Plane1 := Run;
              PChar(Plane2) := PChar(Run) + Increment div 3;
              PChar(Plane3) := PChar(Run) + 2 * (Increment div 3);
              ColorManager.ConvertRow([Plane1, Plane2, Plane3], Line, Width, $FF);
              Inc(Run, Increment);

              Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            end
          end
          else
          begin
            // other indexed formats
            for I := 0 to Height - 1 do
            begin
              Line := ScanLine[I];
              ColorManager.ConvertRow([Run], Line, Width, $FF);
              Inc(Run, Increment);

              Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            end;
          end;
      finally
        if Assigned(DecodeBuffer) then FreeMem(DecodeBuffer);
      end;
      Progress(Self, psEnding, 0, False, FProgressRect, '');
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPCXGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

var
  Header: TPCXHeader;

begin
  Result := inherited ReadImageProperties(Stream, 0);
  with Stream do
  begin
    ReadBuffer(Header, SizeOf(Header));
    with FImageProperties do
    begin
      if Header.FileID in [$0A, $CD] then
      begin
        Width := Header.XMax - Header.XMin + 1;
        Height := Header.YMax - Header.YMin + 1;

        SamplesPerPixel := Header.ColorPlanes;
        BitsPerSample := Header.BitsPerPixel;
        BitsPerPixel := BitsPerSample * SamplesPerPixel;
        if BitsPerPixel <= 8 then ColorScheme := csIndexed
                             else ColorScheme := csRGB;
        if Header.Encoding = 1 then Compression := ctRLE
                               else Compression := ctNone;
        XResolution := Header.HRes;
        YResolution := Header.VRes;

        Result := True;
      end;
    end;
  end;
end;

{$endif} // PCXGraphic

//----------------- TPCDGraphic ----------------------------------------------------------------------------------------

{$ifdef PCDGraphic}

const
  PCD_BEGIN_BASE16 = 8192;
  PCD_BEGIN_BASE4 = 47104;
  PCD_BEGIN_BASE = 196608;
  PCD_BEGIN_ORIENTATION = 194635;
  PCD_BEGIN = 2048;

  PCD_MAGIC = 'PCD_IPI';

//----------------------------------------------------------------------------------------------------------------------

class function TPCDGraphic.CanLoad(Stream: TStream): Boolean;

var
  Header: array of Byte;
  LastPosition: Cardinal;

begin
  with Stream do
  begin
    LastPosition := Position;
    Result := (Size - Position) > 3 * $800;
    if Result then
    begin
      SetLength(Header, $803);
      ReadBuffer(Header[0], Length(Header));
      Result := (StrLComp(@Header[0], 'PCD_OPA', 7) = 0) or
                (StrLComp(@Header[$800], 'PCD', 3) = 0);
    end;
    Position := LastPosition;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPCDGraphic.LoadFromStream(Stream: TStream);

var
  C1, C2, YY: PChar;
  YCbCrData: array[0..2] of PChar;
  SourceDummy,
  DestDummy: Pointer;

  Offset, I,
  X, Y,
  ImageIndex,
  Rows,
  Columns: Cardinal;
  ScanLines: array of Pointer;

  LineBuffer: Pointer;
  Line,
  Run: PBGR;
  Decoder: TPCDDecoder;

begin
  Handle := 0;
  FBasePosition := Stream.Position;
  ImageIndex := 2; // third image is Base resolution

  if ReadImageProperties(Stream, ImageIndex) then
  begin
    with Stream, FImageProperties do
    begin
      Position := FBasePosition;

      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing); 
      Columns := 192 shl Min(ImageIndex, 2);
      Rows := 128 shl Min(ImageIndex, 2);

      // since row and columns might be swapped because of rotated images
      // we determine the final dimensions once more
      Width := 192 shl ImageIndex;
      Height := 128 shl ImageIndex;

      ZeroMemory(@YCbCrData, SizeOf(YCbCrData));
      try
        GetMem(YCbCrData[0], Width * Height);
        GetMem(YCbCrData[1], Width * Height);
        GetMem(YCbCrData[2], Width * Height);

        // advance to image data 
        Offset := 96;
        if Overview then Offset := 5
                    else
          if ImageIndex = 1 then Offset := 23
                            else
            if ImageIndex = 0 then Offset := 4;
        Seek(Offset * $800 , soFromCurrent);

        // color conversion setup
        with ColorManager do
        begin
          SourceColorScheme := csPhotoYCC;
          SourceBitsPerSample := 8;
          SourceSamplesPerPixel := 3;
          TargetColorScheme := csBGR;
          TargetBitsPerSample := 8;
          TargetSamplesPerPixel := 3;
        end;
        PixelFormat := pf24Bit;
        // PhotoYCC format uses CCIR Recommendation 709 coefficients and is subsampled
        // by factor 2 vertically and horizontally
        ColorManager.SetYCbCrParameters([0.2125, 0.7154, 0.0721], 2, 2);

        Progress(Self, psEnding, 0, False, FProgressRect, '');

        if False then
        begin
          // if Overview then ... no info yet about overview image structure
        end
        else
        begin
          YY := YCbCrData[0];
          C1 := YCbCrData[1];
          C2 := YCbCrData[2];
          I := 0;
          Progress(Self, psStarting, 0, False, FProgressRect, gesLoadingData);
          while I < Rows do
          begin
            Progress(Self, psRunning, MulDiv(I, 100, Rows), False, FProgressRect, '');
            ReadBuffer(YY^, Columns);
            Inc(YY, Width);
            ReadBuffer(YY^, Columns);
            Inc(YY, Width);
            ReadBuffer(C1^, Columns shr 1);
            Inc(C1, Width);
            ReadBuffer(C2^, Columns shr 1);
            Inc(C2, Width);
            Inc(I, 2);
          end;
          Progress(Self, psEnding, 0, False, FProgressRect, '');

          Progress(Self, psStarting, 0, False, FProgressRect, gesUpsampling);
          // Y stands here for maximum number of upsample calls
          Y := 5;
          if ImageIndex >= 3 then
          begin
            Inc(Y, 3 * (ImageIndex - 3));

            Decoder := TPCDDecoder.Create(Stream);
            SourceDummy := @YCbCrData;
            DestDummy := nil;
            try
              // recover luminance deltas for 1536 x 1024 image
              Progress(Self, psRunning, MulDiv(0, 100, Y), False, FProgressRect, '');
              Upsample(768, 512, Width, YCbCrData[0]);
              Progress(Self, psRunning, MulDiv(1, 100, Y), False, FProgressRect, '');
              Upsample(384, 256, Width, YCbCrData[1]);
              Progress(Self, psRunning, MulDiv(2, 100, Y), False, FProgressRect, '');
              Upsample(384, 256, Width, YCbCrData[2]);
              Seek(4 * $800, soFromCurrent);

              Decoder.Decode(SourceDummy, DestDummy, Width, 1024);
              if ImageIndex >= 4 then
              begin
                // recover luminance deltas for 3072 x 2048 image
                Progress(Self, psRunning, MulDiv(3, 100, Y), False, FProgressRect, '');
                Upsample(1536, 1024, Width, YCbCrData[0]);
                Progress(Self, psRunning, MulDiv(4, 100, Y), False, FProgressRect, '');
                Upsample(768, 512, Width, YCbCrData[1]);
                Progress(Self, psRunning, MulDiv(5, 100, Y), False, FProgressRect, '');
                Upsample(768, 512, Width, YCbCrData[2]);
                Offset := (Position - Integer(FBasePosition)) div $800 + 12;
                Seek(FBasePosition + Offset * $800, soFromBeginning);

                Decoder.Decode(SourceDummy, DestDummy, Width, 2048);
                if ImageIndex = 5 then
                begin
                  // recover luminance deltas for 6144 x 4096 image (vaporware)
                  Progress(Self, psRunning, MulDiv(6, 100, Y), False, FProgressRect, '');
                  Upsample(3072, 2048, Width, YCbCrData[1]);
                  Progress(Self, psRunning, MulDiv(7, 100, Y), False, FProgressRect, '');
                  Upsample(1536, 1024, Width, YCbCrData[1]);
                  Progress(Self, psRunning, MulDiv(8, 100, Y), False, FProgressRect, '');
                  Upsample(1536, 1024, Width, YCbCrData[2]);
                end;
              end;
            finally
              Decoder.Free;
            end;
          end;

          Progress(Self, psRunning, MulDiv(Y - 1, 100, Y), False, FProgressRect, '');
          Upsample(Width shr 1, Height shr 1, Width, YCbCrData[1]);
          Progress(Self, psRunning, MulDiv(Y, 100, Y), False, FProgressRect, '');
          Upsample(Width shr 1, Height shr 1, Width, YCbCrData[2]);

          Progress(Self, psEnding, 0, False, FProgressRect, '');

          Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
          // transfer luminance and chrominance channels
          YY := YCbCrData[0];
          C1 := YCbCrData[1];
          C2 := YCbCrData[2];

          // For the rotated mode where we need to turn the image by 90°. We can speed up loading
          // the image by factor 2 by using a local copy of the Scanline pointers.
          if Rotate in [1, 3] then
          begin
            Self.Width := Height;
            Self.Height := Width;
            FProgressRect.Right := Height;
            
            SetLength(ScanLines, Width);
            for Y := 0 to Width - 1 do ScanLines[Y] := ScanLine[Y];
            GetMem(LineBuffer, 3 * Width);
          end
          else
          begin
            ScanLines := nil;
            Self.Width := Width;
            Self.Height := Height;
            LineBuffer := nil;
          end;

          try
            case Rotate of
              1: // rotate -90° 
                begin
                  for Y := 0 to Height - 1 do
                  begin
                    ColorManager.ConvertRow([YY, C1, C2], LineBuffer, Width, $FF);
                    Inc(YY, Width);
                    Inc(C1, Width);
                    Inc(C2, Width);

                    Run := LineBuffer;
                    for X := 0 to Width - 1 do
                    begin
                      PChar(Line) := PChar(ScanLines[Width - X - 1]) + Y * 3;
                      Line^ := Run^;
                      Inc(Run);
                    end;

                    Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                    OffsetRect(FProgressRect, 0, 1);
                  end;
                end;
              3: // rotate 90°
                begin
                  for Y := 0 to Height - 1 do
                  begin
                    ColorManager.ConvertRow([YY, C1, C2], LineBuffer, Width, $FF);
                    Inc(YY, Width);
                    Inc(C1, Width);
                    Inc(C2, Width);

                    Run := LineBuffer;
                    for X := 0 to Width - 1 do
                    begin
                      PChar(Line) := PChar(ScanLines[X]) + (Height - Y - 1) * 3;
                      Line^ := Run^;
                      Inc(Run);
                    end;

                    Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                    OffsetRect(FProgressRect, 0, 1);
                  end;
                end;
            else
              for Y := 0 to Height - 1 do
              begin
                ColorManager.ConvertRow([YY, C1, C2], ScanLine[Y], Width, $FF);
                Inc(YY, Width);
                Inc(C1, Width);
                Inc(C2, Width);

                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            end;
            Progress(Self, psEnding, 0, False, FProgressRect, '');
          finally
            ScanLines := nil;
            if Assigned(LineBuffer) then FreeMem(LineBuffer);
          end;
        end;

      finally
        if Assigned(YCbCrData[2]) then FreeMem(YCbCrData[2]);
        if Assigned(YCbCrData[1]) then FreeMem(YCbCrData[1]);
        if Assigned(YCbCrData[0]) then FreeMem(YCbCrData[0]);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPCDGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

var
  Header: array of Byte;
  Temp: Cardinal;
  
begin
  if ImageIndex > 5 then ImageIndex := 5;
  Result := inherited ReadImageProperties(Stream, ImageIndex) and
            ((Stream.Size - Integer(FBasePosition)) > 3 * $800);
  with Stream, FImageProperties do
  begin
    SetLength(Header, 3 * $800);
    ReadBuffer(Header[0], Length(Header));
    try
      Overview := StrLComp(@Header[0], 'PCD_OPA', 7) = 0;
      // determine if image is a PhotoCD image
      if Overview or (StrLComp(@Header[$800], 'PCD', 3) = 0) then
      begin
        Rotate := Header[$0E02] and 3;

        // image sizes are fixed, depending on the given image index
        if Overview then ImageIndex := 0;
        Width := 192 shl ImageIndex;
        Height := 128 shl ImageIndex;
        if (Rotate = 1) or (Rotate = 3) then
        begin
          Temp := Width;
          Width := Height;
          Height := Temp;
        end;
        ColorScheme := csPhotoYCC;
        BitsPerSample := 8;
        SamplesPerPixel := 3;
        BitsPerPixel := BitsPerSample * SamplesPerPixel;
        if ImageIndex > 2 then Compression := ctPCDHuffmann
                          else Compression := ctNone;
        ImageCount := (Header[10] shl 8) or Header[11];

        Result := True;
      end;
    finally
      Header := nil;
    end;
  end;
end;

{$endif} // PCDGraphic

//----------------- TPPMGraphic ----------------------------------------------------------------------------------------

{$ifdef PortableMapGraphic}

class function TPPMGraphic.CanLoad(Stream: TStream): Boolean;

var
  Buffer: array[0..9] of Char;
  LastPosition: Cardinal;

begin
  with Stream do
  begin
    LastPosition := Position;
    Result := (Size - Position) > 10;
    if Result then
    begin
      ReadBuffer(Buffer, SizeOf(Buffer));
      Result := (Buffer[0] = 'P') and (Buffer[1] in ['1'..'6']);
    end;
    Position := LastPosition;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPPMGraphic.CurrentChar: Char;

begin
  if FIndex = SizeOf(FBuffer) then Result := #0
                              else Result := FBuffer[FIndex];
end;

//----------------------------------------------------------------------------------------------------------------------

function TPPMGraphic.GetChar: Char;

// buffered I/O

begin
  if FIndex = SizeOf(FBuffer) then
  begin
    if FStream.Position = FStream.Size then GraphicExError(gesStreamReadError, ['PPM']);
    FIndex := 0;
    FStream.Read(FBuffer, SizeOf(FBuffer));
  end;
  Result := FBuffer[FIndex];
  Inc(FIndex);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPPMGraphic.GetNumber: Cardinal;

// reads the next number from the stream (and skips all characters which are not in 0..9)

var
  Ch: Char;

begin
  // skip all non-numbers
  repeat
    Ch := GetChar;
    // skip comments
    if Ch = '#' then
    begin
      ReadLine;
      Ch := GetChar;
    end;
  until Ch in ['0'..'9'];

  // read the number characters and convert meanwhile
  Result := 0;
  repeat
    Result := 10 * Result + Ord(Ch) - $30;
    Ch := GetChar;
  until not (Ch in ['0'..'9']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPPMGraphic.ReadLine: String;

// reads one text line from stream and skips comments

var
  Ch: Char;
  I: Integer;

begin
  Result := '';
  repeat
    Ch := GetChar;
    if Ch in [#13, #10] then Break
                        else Result := Result + Ch;
  until False;
  // eat #13#10 combination
  if (Ch = #13) and (CurrentChar = #10) then GetChar;

  // delete comments
  I := Pos('#', Result);
  if I > 0 then Delete(Result, I, MaxInt);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPPMGraphic.LoadFromStream(Stream: TStream);

var
  Buffer: String;
  Line24: PBGR;
  Line8: PByte;
  X, Y: Integer;
  Pixel: Byte;

begin
  Handle := 0;
  FBasePosition := Stream.Position;
  // copy reference for buffered access
  FStream := Stream;
  if ReadImageProperties(Stream, 0) then
  begin
    with FImageProperties do
    begin
      Stream.Position := FBasePosition;

      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
      // set index pointer to end of buffer to cause reload
      FIndex := SizeOf(FBuffer);
      with Stream do
      begin
        Buffer := ReadLine;
        case StrToInt(Buffer[2]) of
          1: // PBM ASCII format (black & white)
            begin
              PixelFormat := pf1Bit;
              Self.Width := GetNumber;
              Self.Height := GetNumber;
              ColorManager.TargetSamplesPerPixel := 1;
              ColorManager.TargetBitsPerSample := 1;
              Palette := ColorManager.CreateGrayScalePalette(True);

              // read image data
              for Y := 0 to Height - 1 do
              begin
                Line8 := ScanLine[Y];
                Pixel := 0;
                for X := 1 to Width do
                begin
                  Pixel := (Pixel shl 1) or (GetNumber and 1);
                  if (X mod 8) = 0 then
                  begin
                    Line8^ := Pixel;
                    Inc(Line8);
                    Pixel := 0;
                  end;
                end;
                if (Width mod 8) <> 0 then Line8^ := Pixel shl (8 - (Width mod 8));

                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            end;
          2: // PGM ASCII form (gray scale)
            begin
              PixelFormat := pf8Bit;
              Self.Width := GetNumber;
              Self.Height := GetNumber;
              // skip maximum color value
              GetNumber;
              ColorManager.TargetSamplesPerPixel := 1;
              ColorManager.TargetBitsPerSample := 8;
              Palette := ColorManager.CreateGrayScalePalette(False);

              // read image data
              for Y := 0 to Height - 1 do
              begin
                Line8 := ScanLine[Y];
                for X := 0 to Width - 1 do
                begin
                  Line8^ := GetNumber;
                  Inc(Line8);
                end;

                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            end;
          3: // PPM ASCII form (true color)
            begin
              PixelFormat := pf24Bit;
              Self.Width := GetNumber;
              Self.Height := GetNumber;
              // skip maximum color value
              GetNumber;

              for Y := 0 to Height - 1 do
              begin
                Line24 := ScanLine[Y];
                for X := 0 to Width - 1 do
                begin
                  Line24.R := GetNumber;
                  Line24.G := GetNumber;
                  Line24.B := GetNumber;
                  Inc(Line24);
                end;

                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            end;
          4: // PBM binary format (black & white)
            begin
              PixelFormat := pf1Bit;
              Self.Width := GetNumber;
              Self.Height := GetNumber;
              ColorManager.TargetSamplesPerPixel := 1;
              ColorManager.TargetBitsPerSample := 1;
              Palette := ColorManager.CreateGrayScalePalette(True);

              // read image data
              for Y := 0 to Height - 1 do
              begin
                Line8 := ScanLine[Y];
                for X := 0 to (Width div 8) - 1 do
                begin
                  Line8^ := Byte(GetChar);
                  Inc(Line8);
                end;
                if (Width mod 8) <> 0 then Line8^ := Byte(GetChar);

                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            end;
          5: // PGM binary form (gray scale)
            begin
              PixelFormat := pf8Bit;
              Self.Width := GetNumber;
              Self.Height := GetNumber;
              // skip maximum color value
              GetNumber;
              ColorManager.TargetSamplesPerPixel := 1;
              ColorManager.TargetBitsPerSample := 8;
              Palette := ColorManager.CreateGrayScalePalette(False);

              // read image data
              for Y := 0 to Height - 1 do
              begin
                Line8 := ScanLine[Y];
                for X := 0 to Width - 1 do
                begin
                  Line8^ := Byte(GetChar);
                  Inc(Line8);
                end;

                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            end;
          6: // PPM binary form (true color)
            begin
              PixelFormat := pf24Bit;
              Self.Width := GetNumber;
              Self.Height := GetNumber;
              // skip maximum color value
              GetNumber;

              // Pixel values are store linearly (but RGB instead BGR).
              // There's one allowed white space which will automatically be skipped by the first
              // GetChar call below
              // now read the pixels
              for Y := 0 to Height - 1 do
              begin
                Line24 := ScanLine[Y];
                for X := 0 to Width - 1 do
                begin
                  Line24.R := Byte(GetChar);
                  Line24.G := Byte(GetChar);
                  Line24.B := Byte(GetChar);
                  Inc(Line24);
                end;

                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            end;
        end;
      end;
      Progress(Self, psEnding, 0, False, FProgressRect, '');
    end;
  end
  else GraphicExError(gesInvalidImage, ['PBM, PGM or PPM']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPPMGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

var
  Buffer: String;

begin
  Result := inherited ReadImageProperties(Stream, ImageIndex);
  with Stream, FImageProperties do
  begin
    // set index pointer to end of buffer to cause reload
    FIndex := SizeOf(FBuffer);
    Buffer := ReadLine;

    Compression := ctNone;

    if Buffer[1] = 'P' then
    begin
      case StrToInt(Buffer[2]) of
        1: // PBM ASCII format (black & white)
          begin
            Width := GetNumber;
            Height := GetNumber;

            SamplesPerPixel := 1;
            BitsPerSample := 1;
            ColorScheme := csIndexed;
            BitsPerPixel := SamplesPerPixel * BitsPerSample;
          end;
        2: // PGM ASCII form (gray scale)
          begin
            Width := GetNumber;
            Height := GetNumber;
            // skip maximum color value
            GetNumber;

            SamplesPerPixel := 1;
            BitsPerSample := 8;
            ColorScheme := csIndexed;
            BitsPerPixel := SamplesPerPixel * BitsPerSample;
          end;
        3: // PPM ASCII form (true color)
          begin
            Width := GetNumber;
            Height := GetNumber;
            // skip maximum color value
            GetNumber;

            SamplesPerPixel := 3;
            BitsPerSample := 8;
            ColorScheme := csRGB;
            BitsPerPixel := SamplesPerPixel * BitsPerSample;
          end;
        4: // PBM binary format (black & white)
          begin
            Width := GetNumber;
            Height := GetNumber;

            SamplesPerPixel := 1;
            BitsPerSample := 1;
            ColorScheme := csIndexed;
            BitsPerPixel := SamplesPerPixel * BitsPerSample;
          end;
        5: // PGM binary form (gray scale)
          begin
            Width := GetNumber;
            Height := GetNumber;
            // skip maximum color value
            GetNumber;

            SamplesPerPixel := 1;
            BitsPerSample := 8;
            ColorScheme := csIndexed;
            BitsPerPixel := SamplesPerPixel * BitsPerSample;
          end;
        6: // PPM binary form (true color)
          begin
            Width := GetNumber;
            Height := GetNumber;
            // skip maximum color value
            GetNumber;

            SamplesPerPixel := 3;
            BitsPerSample := 8;
            ColorScheme := csRGB;
            BitsPerPixel := SamplesPerPixel * BitsPerSample;
          end;
      end;
      Result := True;
    end;
  end;
end;

{$endif} // PortableMapGraphic

//----------------- TCUTGraphic ----------------------------------------------------------------------------------------

{$ifdef CUTGraphic}

class function TCUTGraphic.CanLoad(Stream: TStream): Boolean;

// Note: cut files cannot be determined from stream because the only information
//       is width and height of the image at stream/image start which is by no means
//       enough to identify a cut (or any other) image.

begin
  Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCUTGraphic.LoadFromFile(const FileName: String);

// overridden to extract an implicit palette file name

begin
  FPaletteFile := ChangeFileExt(FileName, '.pal');
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCUTGraphic.LoadFromStream(Stream: TStream);

var
  Buffer: PByte;
  Run,
  Line: Pointer;
  Decoder: TCUTRLEDecoder;
  CUTSize: Cardinal;
  Y: Integer;

begin
  Handle := 0;
  FBasePosition := Stream.Position;
  if ReadImageProperties(Stream, 0) then
  begin
    with Stream, FImageProperties do
    begin
      Position := FBasePosition + 6;

      FProgressRect := Rect(0, 0, Width, 0);
      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
      PixelFormat := pf8Bit;
      Self.Width := Width;
      Self.Height := Height;
      LoadPalette;

      CutSize := Stream.Size - Stream.Position;
      Decoder := TCUTRLEDecoder.Create;
      Buffer := nil;
      try
        GetMem(Buffer, CutSize);
        Stream.ReadBuffer(Buffer^, CUTSize);

        Run := Buffer;
        for Y := 0 to Height - 1 do
        begin
          Line := ScanLine[Y];
          Decoder.Decode(Run, Line, 0, Width);

          Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
          OffsetRect(FProgressRect, 0, 1);
        end;

      finally
        Decoder.Free;
        if Assigned(Buffer) then FreeMem(Buffer);
      end;

      Progress(Self, psEnding, 0, False, FProgressRect, '');
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCUTGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

var
  Dummy: Word;
  
begin
  inherited ReadImageProperties(Stream, ImageIndex);
  with Stream, FImageProperties do
  begin
    PixelFormat := pf8Bit;
    ReadBuffer(Dummy, SizeOf(Dummy));
    Width := Dummy;
    ReadBuffer(Dummy, SizeOf(Dummy));
    Height := Dummy;

    ColorScheme := csIndexed;
    BitsPerSample := 8;
    SamplesPerPixel := 1;
    BitsPerPixel := BitsPerSample * SamplesPerPixel;

    Compression := ctRLE;
    
    Result := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  // the palette file header is actually more complex than the
  // image file's header, funny...
  PHaloPaletteHeader = ^THaloPaletteHeader;
  THaloPaletteHeader = packed record
    ID: array[0..1] of Char;  // should be 'AH'
    Version,
    Size: Word;
    FileType,
    SubType: Byte;
    BrdID,
    GrMode: Word;
    MaxIndex,
    MaxRed,
    MaxGreen,
    MaxBlue: Word; // colors = MaxIndex + 1
    Signature: array[0..7] of Char; // 'Dr. Halo'
    Filler: array[0..11] of Byte;
  end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCUTGraphic.LoadPalette;

var
  Header: PHaloPaletteHeader;
  LogPalette: TMaxLogPalette;
  I: Integer;
  Buffer: array[0..511] of Byte;
  Run: PWord;

begin
  LogPalette.palVersion := $300;
  if FileExists(FPaletteFile) then
  begin
    with TFileStream.Create(FPaletteFile, fmOpenRead or fmShareDenyNone) do
    try
      // quite strange file organization here, we need always to load 512 bytes blocks
      // and skip occasionally some bytes
      ReadBuffer(Buffer, SizeOf(Buffer));
      Header := @Buffer;
      LogPalette.palNumEntries := Header.MaxIndex + 1;
      Run := @Buffer;
      Inc(PByte(Run), SizeOf(Header^));
      for I := 0 to LogPalette.palNumEntries - 1 do
      begin
        // load next 512 bytes buffer if necessary
        if (Integer(Run) - Integer(@Buffer)) > 506 then
        begin
          ReadBuffer(Buffer, SizeOf(Buffer));
          Run := @Buffer;
        end;
        LogPalette.palPalEntry[I].peRed := Run^;
        Inc(Run);
        LogPalette.palPalEntry[I].peGreen := Run^;
        Inc(Run);
        LogPalette.palPalEntry[I].peBlue := Run^;
        Inc(Run);
      end;
    finally
      Free;
    end;
  end
  else
  begin
    LogPalette.palNumEntries := 256;
    // no external palette so use gray scale
    for I := 0 to 255 do
    begin
      LogPalette.palPalEntry[I].peBlue := I;
      LogPalette.palPalEntry[I].peGreen := I;
      LogPalette.palPalEntry[I].peRed := I;
    end;
  end;

  // finally create palette
  Palette := CreatePalette(PLogPalette(@LogPalette)^);
end;

{$endif} // CUTGraphic

//----------------- TGIFGraphic ----------------------------------------------------------------------------------------

{$ifdef GIFGraphic}

const
  // logical screen descriptor packed field masks
  GIF_GLOBALCOLORTABLE = $80;
  GIF_COLORRESOLUTION = $70;
  GIF_GLOBALCOLORTABLESORTED = $08; 
  GIF_COLORTABLESIZE = $07;

  // image flags
  GIF_LOCALCOLORTABLE = $80;
  GIF_INTERLACED = $40;
  GIF_LOCALCOLORTABLESORTED= $20;

  // block identifiers
  GIF_PLAINTEXT = $01;
  GIF_GRAPHICCONTROLEXTENSION = $F9;
  GIF_COMMENTEXTENSION = $FE;
  GIF_APPLICATIONEXTENSION = $FF;
  GIF_IMAGEDESCRIPTOR = Ord(',');
  GIF_EXTENSIONINTRODUCER = Ord('!');
  GIF_TRAILER = Ord(';');
  
type
  TGIFHeader = packed record
    Signature: array[0..2] of Char; // magic ID 'GIF'
    Version: array[0..2] of Char;   // '87a' or '89a' 
  end;

  TLogicalScreenDescriptor = packed record
    ScreenWidth: Word;
    ScreenHeight: Word;
    PackedFields,
    BackgroundColorIndex, // index into global color table
    AspectRatio: Byte;    // actual ratio = (AspectRatio + 15) / 64
  end;

  TImageDescriptor = packed record
    //Separator: Byte; // leave that out since we always read one bye ahead
    Left: Word;		 // X position of image with respect to logical screen
    Top: Word;		 // Y position
    Width: Word;
    Height: Word;
    PackedFields: Byte;
  end;

//----------------------------------------------------------------------------------------------------------------------

class function TGIFGraphic.CanLoad(Stream: TStream): Boolean;

var
  Header: TGIFHeader;
  LastPosition: Cardinal;

begin
  with Stream do
  begin
    LastPosition := Position;
    Result := (Size - Position) > (SizeOf(TGIFHeader) + SizeOf(TLogicalScreenDescriptor) + SizeOf(TImageDescriptor));
    if Result then
    begin
      ReadBuffer(Header, SizeOf(Header));
      Result := UpperCase(Header.Signature) = 'GIF';
    end;
    Position := LastPosition;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGIFGraphic.SkipExtensions: Byte;

// Skips all blocks until an image block has been found in the data stream.
// Result is the image block ID if an image block could be found.

var
  Increment: Byte;

begin
  with FStream do
  begin
    // iterate through the blocks until first image is found
    repeat
      ReadBuffer(Result, 1);
      if Result = GIF_EXTENSIONINTRODUCER then
      begin
        // skip any extension
        ReadBuffer(Result, 1);
        case Result of
          GIF_PLAINTEXT:
            begin
              // block size of text grid data
              ReadBuffer(Increment, 1);
              Seek(Increment, soFromCurrent);
              // skip variable lengthed text block
              repeat
                // block size
                ReadBuffer(Increment, 1);
                if Increment = 0 then Break;
                Seek(Increment, soFromCurrent);
              until False;
            end;
          GIF_GRAPHICCONTROLEXTENSION:
            begin
              // block size
              ReadBuffer(Increment, 1);
              // skip block and its terminator
              Seek(Increment + 1, soFromCurrent);
            end;
          GIF_COMMENTEXTENSION:
            repeat
              // block size
              ReadBuffer(Increment, 1);
              if Increment = 0 then Break;
              Seek(Increment, soFromCurrent);
            until False;
          GIF_APPLICATIONEXTENSION:
            begin
              // application id and authentication code plus potential application data
              repeat
                ReadBuffer(Increment, 1);
                if Increment = 0 then Break;
                Seek(Increment, soFromCurrent);
              until False;
            end;
        end;
      end;
    until (Result = GIF_IMAGEDESCRIPTOR) or (Result = GIF_TRAILER);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGIFGraphic.LoadFromStream(Stream: TStream);

var
  Header: TGIFHeader;
  ScreenDescriptor: TLogicalScreenDescriptor;
  ImageDescriptor: TImageDescriptor;
  LogPalette: TMaxLogPalette;
  I: Cardinal;
  BlockID: Byte;
  InitCodeSize: Byte;
  RawData,
  Run: PByte;
  TargetBuffer,
  TargetRun,
  Line: Pointer;
  Pass,
  Increment,
  Marker: Integer;
  Decoder: TDecoder;

begin
  // release old image
  Handle := 0;
  FBasePosition := Stream.Position;
  FStream := Stream;
  if ReadImageProperties(Stream, 0) then
  begin
    with Stream, FImageProperties do
    begin
      Position := FBasePosition;

      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      ReadBuffer(Header, SizeOf(Header));

      PixelFormat := pf8Bit;
      
      // general information
      ReadBuffer(ScreenDescriptor, SizeOf(ScreenDescriptor));

      ZeroMemory(@LogPalette, SizeOf(LogPalette));
      LogPalette.palVersion := $300;
      // read global color table if given
      if (ScreenDescriptor.PackedFields and GIF_GLOBALCOLORTABLE) <> 0 then
      begin
        // the global color table immediately follows the screen descriptor
        LogPalette.palNumEntries := 2 shl (ScreenDescriptor.PackedFields and GIF_COLORTABLESIZE);
        for I := 0 to LogPalette.palNumEntries - 1 do
        begin
          ReadBuffer(LogPalette.palPalEntry[I].peRed, 1);
          ReadBuffer(LogPalette.palPalEntry[I].peGreen, 1);
          ReadBuffer(LogPalette.palPalEntry[I].peBlue, 1);
        end;
        // finally create palette
        Palette := CreatePalette(PLogPalette(@LogPalette)^);
      end;

      BlockID := SkipExtensions;
      Progress(Self, psEnding, 0, False, FProgressRect, '');

      // image found?
      if BlockID = GIF_IMAGEDESCRIPTOR then
      begin
        Progress(Self, psStarting, 0, False, FProgressRect, gesLoadingData);
        ReadBuffer(ImageDescriptor, SizeOf(TImageDescriptor));
        Self.Width := Width;
        Self.Height := Height;

        // if there is a local color table then override the already set one
        if (ImageDescriptor.PackedFields and GIF_LOCALCOLORTABLE) <> 0 then
        begin
          // the global color table immediately follows the image descriptor
          LogPalette.palNumEntries := 2 shl (ImageDescriptor.PackedFields and GIF_COLORTABLESIZE);
          for I := 0 to LogPalette.palNumEntries - 1 do
          begin
            ReadBuffer(LogPalette.palPalEntry[I].peRed, 1);
            ReadBuffer(LogPalette.palPalEntry[I].peGreen, 1);
            ReadBuffer(LogPalette.palPalEntry[I].peBlue, 1);
          end;
          Palette := CreatePalette(PLogPalette(@LogPalette)^);
        end;

        ReadBuffer(InitCodeSize, 1);
        // decompress data in one step
        // 1) count data
        Marker := Position;
        Pass := 0;
        Increment := 0;
        repeat
          if Read(Increment, 1) = 0 then Break;
          Inc(Pass, Increment);
          Seek(Increment, soFromCurrent);
        until Increment = 0;

        // 2) allocate enough memory
        GetMem(RawData, Pass);
        // add one extra line of extra memory for badly coded images
        GetMem(TargetBuffer, Width * (Height + 1));

        try
          // 3) read and decode data
          Position := Marker;
          Increment := 0;
          Run := RawData;
          repeat
            if Read(Increment, 1) = 0 then Break;
            Read(Run^, Increment);
            Inc(Run, Increment);
          until Increment = 0;

          Decoder := TGIFLZWDecoder.Create(InitCodeSize);
          try
            Run := RawData;
            Decoder.Decode(Pointer(Run), TargetBuffer, Pass, Width * Height);
          finally
            Decoder.Free;
          end;
          Progress(Self, psEnding, 0, False, FProgressRect, '');

          // finally transfer image data
          Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
          if (ImageDescriptor.PackedFields and GIF_INTERLACED) = 0 then
          begin
            TargetRun := TargetBuffer;
            for I := 0 to Height - 1 do
            begin
              Line := Scanline[I];
              Move(TargetRun^, Line^, Width);
              Inc(PByte(TargetRun), Width);

              Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            end;
          end
          else
          begin
            TargetRun := TargetBuffer;
            // interlaced image, need to move in four passes
            for Pass := 0 to 3 do
            begin
              // determine start line and increment of the pass
              case Pass of
                0:
                  begin
                    I := 0;
                    Increment := 8;
                  end;
                1:
                  begin
                    I := 4;
                    Increment := 8;
                  end;
                2:
                  begin
                    I := 2;
                    Increment := 4;
                  end;
              else
                I := 1;
                Increment := 2;
              end;

              while I < Height do
              begin
                Line := Scanline[I];
                Move(TargetRun^, Line^, Width);
                Inc(PByte(TargetRun), Width);
                Inc(I, Increment);

                if Pass = 3 then
                begin
                  // progress events only for last (and most expensive) run
                  Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
                  OffsetRect(FProgressRect, 0, 1);
                end;
              end;
            end;
          end;
          Progress(Self, psEnding, 0, False, FProgressRect, '');
        finally
          if Assigned(TargetBuffer) then FreeMem(TargetBuffer);
          if Assigned(RawData) then FreeMem(RawData);
        end;
      end;
    end;
  end
  else GraphicExError(gesInvalidImage, ['GIF']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGIFGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

var
  Header: TGIFHeader;
  ScreenDescriptor: TLogicalScreenDescriptor;
  ImageDescriptor: TImageDescriptor;
  BlockID: Integer;
  
begin
  Result := inherited ReadImageProperties(Stream, ImageIndex);
  with Stream, FImageProperties do
  begin
    ReadBuffer(Header, SizeOf(Header));
    if UpperCase(Header.Signature) = 'GIF' then
    begin
      Version := StrToInt(Copy(Header.Version, 1, 2));
      ColorScheme := csIndexed;
      SamplesPerPixel := 1;
      // might be overwritten
      BitsPerSample := 8;
      Compression := ctLZW;

      // general information
      ReadBuffer(ScreenDescriptor, SizeOf(ScreenDescriptor));

      // skip global color table if given
      if (ScreenDescriptor.PackedFields and GIF_GLOBALCOLORTABLE) <> 0 then
      begin
        BitsPerSample := (ScreenDescriptor.PackedFields and GIF_COLORTABLESIZE) + 1;
        // the global color table immediately follows the screen descriptor
        Seek(3 * (1 shl BitsPerSample), soFromCurrent);
      end;

      BlockID := SkipExtensions;

      // image found?
      if BlockID = GIF_IMAGEDESCRIPTOR then
      begin
        ReadBuffer(ImageDescriptor, SizeOf(TImageDescriptor));
        Width := ImageDescriptor.Width;
        if Width = 0 then Width := ScreenDescriptor.ScreenWidth;
        Height := ImageDescriptor.Height;
        if Height = 0 then Height := ScreenDescriptor.ScreenHeight;

        // if there is a local color table then override the already set one
        LocalColorTable := (ImageDescriptor.PackedFields and GIF_LOCALCOLORTABLE) <> 0;
        if LocalColorTable then
          BitsPerSample := (ImageDescriptor.PackedFields and GIF_LOCALCOLORTABLE) + 1;
        Interlaced := (ImageDescriptor.PackedFields and GIF_INTERLACED) <> 0;
      end;

      BitsPerPixel := SamplesPerPixel * BitsPerSample;

      Result := True;
    end;
  end;
end;

{$endif} // GIFGraphic

//----------------- TRLAGraphic ----------------------------------------------------------------------------------------

{$ifdef RLAGraphic}

// This implementation is based on code from Dipl. Ing. Ingo Neumann (ingo@upstart.de, ingo_n@dialup.nacamar.de).

type
  TRLAWindow = packed record
    Left,
    Right,
    Bottom,
    Top: SmallInt;
  end;

  TRLAHeader = packed record
    Window,                            // overall image size
    Active_window: TRLAWindow;         // size of non-zero portion of image (we use this as actual image size)
    Frame,                             // frame number if part of a sequence
    Storage_type,                      // type of image channels (0 - integer data, 1 - float data)
    Num_chan,                          // samples per pixel (usually 3: r, g, b)
    Num_matte,                         // number of matte channels (usually only 1)
    Num_aux,                           // number of auxiliary channels, usually 0
    Revision: SmallInt;                // always $FFFE
    Gamma: array[0..15] of Char;       // gamma single value used when writing the image
    Red_pri: array[0..23] of Char;     // used chromaticity for red channel (typical format: "%7.4f %7.4f")
    Green_pri: array[0..23] of Char;   // used chromaticity for green channel
    Blue_pri: array[0..23] of Char;    // used chromaticity for blue channel
    White_pt: array[0..23] of Char;    // used chromaticity for white point
    Job_num: Integer;                  // rendering speciifc
    Name: array[0..127] of Char;       // original file name
    Desc: array[0..127] of Char;       // a file description
    ProgramName: array[0..63] of Char; // name of program which created the image
    Machine: array[0..31] of Char;     // name of computer on which the image was rendered
    User: array[0..31] of Char;        // user who ran the creation program of the image
    Date: array[0..19] of Char;        // creation data of image (ex: Sep 30 12:29 1993)
    Aspect: array[0..23] of Char;      // aspect format of the file (external resource)
    Aspect_ratio: array[0..7] of Char; // float number Width /Height
    Chan: array[0..31] of Char;        // color space (can be: rgb, xyz, sampled or raw)
    Field: SmallInt;                   // 0 - non-field rendered data, 1 - field rendered data
    Time: array[0..11] of Char;        // time needed to create the image (used when rendering)
    Filter: array[0..31] of Char;      // filter name to post-process image data
    Chan_bits,                         // bits per sample
    Matte_type,                        // type of matte channel (see aux_type)
    Matte_bits,                        // precision of a pixel's matte channel (1..32)
    Aux_type,                          // type of aux channel (0 - integer data; 4 - single (float) data
    Aux_bits: SmallInt;                // bits precision of the pixel's aux channel (1..32 bits)
    Aux: array[0..31] of Char;         // auxiliary channel as either range or depth
    Space: array[0..35] of Char;       // unused
    Next: Integer;                     // offset for next header if multi-frame image
  end;
  
//----------------------------------------------------------------------------------------------------------------------

class function TRLAGraphic.CanLoad(Stream: TStream): Boolean;

var
  Header: TRLAHeader;
  LastPosition: Cardinal;

begin
  with Stream do
  begin
    LastPosition := Position;
    Result := (Size - Position) > SizeOf(Header);
    if Result then
    begin
      ReadBuffer(Header, SizeOf(Header));
      Result := (Swap(Word(Header.Revision)) = $FFFE) and
                ((LowerCase(Header.Chan) = 'rgb') or
                 (LowerCase(Header.Chan) = 'xyz'));
    end;
    Position := LastPosition;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRLAGraphic.LoadFromStream(Stream: TStream);

var
  Offsets: TCardinalArray;
  RLELength: Word;
  Line: Pointer;
  Y: Integer;

  // RLE buffers
  RawBuffer,
  RedBuffer,
  GreenBuffer,
  BlueBuffer,
  AlphaBuffer: Pointer;
  Decoder: TRLADecoder;

begin
  // free previous image data
  Handle := 0;
  FBasePosition := Stream.Position;
  if ReadImageProperties(Stream, 0) then
  begin
    with Stream, FImageProperties do
    begin
      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);

      with ColorManager do
      begin
        SourceSamplesPerPixel := SamplesPerPixel;
        TargetSamplesPerPixel := SamplesPerPixel;

        SourceBitsPerSample := BitsPerSample;
        if BitsPerSample > 8 then TargetBitsPerSample := 8
                             else TargetBitsPerSample := BitsPerSample;
        SourceColorScheme := ColorScheme;
        if ColorScheme = csRGBA then TargetColorScheme := csBGRA
                                else TargetColorScheme := csBGR;

        PixelFormat := TargetPixelFormat;

        if FileGamma <> 1 then
        begin
          SetGamma(FileGamma);
          TargetOptions := TargetOptions + [coApplyGamma];
          Include(Options, ioUseGamma);
        end;
      end;

      // dimension of image, top might be larger than bottom denoting a bottom up image
      Self.Width := Width;
      Self.Height := Height;

      // each scanline is organized in RLE compressed strips whose location in the stream
      // is determined by the offsets table
      SetLength(Offsets, Height);
      ReadBuffer(Offsets[0], Height * SizeOf(Cardinal));
      SwapLong(@Offsets[0], Height);

      // setup intermediate storage
      Decoder := TRLADecoder.Create;
      RawBuffer := nil;
      RedBuffer := nil;
      GreenBuffer := nil;
      BlueBuffer := nil;
      AlphaBuffer := nil;
      try
        GetMem(RedBuffer, Width);
        GetMem(GreenBuffer, Width);
        GetMem(BlueBuffer, Width);
        GetMem(AlphaBuffer, Width);

        // no go for each scanline
        for Y := 0 to Height - 1 do
        begin
          Stream.Position := FBasePosition + Offsets[Y];
          if BottomUp then Line := ScanLine[Integer(Height) - Y - 1]
                      else Line := ScanLine[Y];
          // read channel data to decode
          // red
          ReadBuffer(RLELength, SizeOf(RLELength));
          RLELength := Swap(RLELength);
          ReallocMem(RawBuffer, RLELength);
          ReadBuffer(RawBuffer^, RLELength);
          Decoder.Decode(RawBuffer, RedBuffer, RLELength, Width);
          // green
          ReadBuffer(RLELength, SizeOf(RLELength));
          RLELength := Swap(RLELength);
          ReallocMem(RawBuffer, RLELength);
          ReadBuffer(RawBuffer^, RLELength);
          Decoder.Decode(RawBuffer, GreenBuffer, RLELength, Width);
          // blue
          ReadBuffer(RLELength, SizeOf(RLELength));
          RLELength := Swap(RLELength);
          ReallocMem(RawBuffer, RLELength);
          ReadBuffer(RawBuffer^, RLELength);
          Decoder.Decode(RawBuffer, BlueBuffer, RLELength, Width);

          if ColorManager.TargetColorScheme = csBGR then
          begin
            ColorManager.ConvertRow([RedBuffer, GreenBuffer, BlueBuffer], Line, Width, $FF);
          end
          else
          begin
            // alpha
            ReadBuffer(RLELength, SizeOf(RLELength));
            RLELength := Swap(RLELength);
            ReallocMem(RawBuffer, RLELength);
            ReadBuffer(RawBuffer^, RLELength);
            Decoder.Decode(RawBuffer, AlphaBuffer, RLELength, Width);

            ColorManager.ConvertRow([RedBuffer, GreenBuffer, BlueBuffer, AlphaBuffer], Line, Width, $FF);
          end;

          Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
          OffsetRect(FProgressRect, 0, 1);
        end;
      finally
        if Assigned(RawBuffer) then FreeMem(RawBuffer);
        if Assigned(RedBuffer) then FreeMem(RedBuffer);
        if Assigned(GreenBuffer) then FreeMem(GreenBuffer);
        if Assigned(BlueBuffer) then FreeMem(BlueBuffer);
        if Assigned(AlphaBuffer) then FreeMem(AlphaBuffer);
        Decoder.Free;
      end;
      Progress(Self, psEnding, 0, False, FProgressRect, '');
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TRLAGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

var
  Header: TRLAHeader;
  
begin
  Result := inherited ReadImageProperties(Stream, ImageIndex);
  with Stream, FImageProperties do
  begin
    ReadBuffer(Header, SizeOf(Header));
    // data is always given in big endian order, so swap data which needs this
    SwapHeader(Header);
    Options := [ioBigEndian];

    SamplesPerPixel := Header.num_chan;
    if Header.num_matte = 1 then Inc(SamplesPerPixel);
    BitsPerSample := Header.Chan_bits;
    BitsPerPixel := SamplesPerPixel * BitsPerSample;

    if LowerCase(Header.Chan) = 'rgb' then
    begin
      if Header.num_matte > 0 then ColorScheme := csRGBA
                              else ColorScheme := csRGB;
    end
    else
      if LowerCase(Header.Chan) = 'xyz' then Exit;

    try
      FileGamma := StrToFloat(Header.Gamma);
    except
    end;

    Compression := ctRLE;

    // dimension of image, top might be larger than bottom denoting a bottom up image
    Width := Header.Active_window.Right - Header.Active_window.Left + 1;
    Height := Abs(Header.Active_window.Bottom - Header.Active_window.Top) + 1;
    BottomUp := (Header.Active_window.Bottom - Header.Active_window.Top) < 0;

    Result := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRLAGraphic.SwapHeader(var Header);

// separate swap method to ease reading the main flow of the LoadFromStream method

begin
  with TRLAHeader(Header) do
  begin
    SwapShort(@Window, 4);
    SwapShort(@Active_window, 4);
    Frame := Swap(Frame);
    Storage_type := Swap(Storage_type);
    Num_chan := Swap(Num_chan);
    Num_matte := Swap(Num_matte);
    Num_aux := Swap(Num_aux);
    Revision := Swap(Revision);
    Job_num  := SwapLong(Job_num);
    Field := Swap(Field);
    Chan_bits := Swap(Chan_bits);
    Matte_type := Swap(Matte_type);
    Matte_bits := Swap(Matte_bits);
    Aux_type := Swap(Aux_type);
    Aux_bits := Swap(Aux_bits);
    Next := SwapLong(Next);
  end;
end;

{$endif} // RLAGraphic

//----------------- TPSDGraphic ----------------------------------------------------------------------------------------

{$ifdef PhotoshopGraphic}

const
  // color modes
  PSD_BITMAP = 0;
  PSD_GRAYSCALE = 1;
  PSD_INDEXED = 2;
  PSD_RGB = 3;
  PSD_CMYK = 4;
  PSD_MULTICHANNEL = 7;
  PSD_DUOTONE = 8;
  PSD_LAB = 9;

  PSD_COMPRESSION_NONE = 0;
  PSD_COMPRESSION_RLE = 1; // RLE compression (same as TIFF packed bits)

type
  TPSDHeader = packed record
    Signature: array[0..3] of Char; // always '8BPS'
    Version: Word;                  // always 1
    Reserved: array[0..5] of Byte;  // reserved, always 0
    Channels: Word;                 // 1..24, number of channels in the image (including alpha)
    Rows,
    Columns: Cardinal;              // 1..30000, size of image
    Depth: Word;                    // 1, 8, 16 bits per channel
    Mode: Word;                     // color mode (see constants above)
  end;

//----------------------------------------------------------------------------------------------------------------------

class function TPSDGraphic.CanLoad(Stream: TStream): Boolean;

var
  Header: TPSDHeader;
  LastPosition: Cardinal;

begin
  with Stream do
  begin
    LastPosition := Position;
    Result := (Size - Position) > SizeOf(Header);
    if Result then
    begin
      ReadBuffer(Header, SizeOf(Header));
      Result := (UpperCase(Header.Signature) = '8BPS') and
                (Swap(Header.Version) = 1);
    end;
    Position := LastPosition;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPSDGraphic.LoadFromStream(Stream: TStream);

var
  Header: TPSDHeader;
  Count: Cardinal;
  Decoder: TDecoder;
  RLELength: array of Word;

  Y: Integer;
  BPS: Cardinal;        // bytes per sample either 1 or 2 for 8 bits per channel and 16 bits per channel respectively 
  ChannelSize: Integer; // size of one channel (taking BPS into account)
  Increment: Integer;   // pointer increment from one line to next

  // RLE buffers
  Line,
  RawBuffer,           // all image data compressed
  Buffer: Pointer;     // all iamge data uncompressed
  Run1,                // running pointer in Buffer 1
  Run2,                // etc.
  Run3,
  Run4: PByte;
  RawPalette: array[0..767] of Byte;

begin
  // free previous image data
  Handle := 0;
  FBasePosition := Stream.Position;
  if ReadImageProperties(Stream, 0) then
  begin
    with Stream, FImageProperties do
    begin
      Position := FBasePosition;

      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      ReadBuffer(Header, SizeOf(Header));

      // initialize color manager
      with ColorManager do
      begin
        SourceOptions := [coNeedByteSwap];
        SourceBitsPerSample := BitsPerSample;
        if BitsPerSample = 16 then TargetBitsPerSample := 8
                              else TargetBitsPerSample := BitsPerSample;
        SourceSamplesPerPixel := SamplesPerPixel;
        TargetSamplesPerPixel := SamplesPerPixel;

        // color space
        SourceColorScheme := ColorScheme;
        case ColorScheme of
          csG,
          csIndexed:
            TargetColorScheme := ColorScheme;
          csRGB:
            TargetColorScheme := csBGR;
          csRGBA:
            TargetColorScheme := csBGRA;
          csCMYK:
            begin
              TargetColorScheme := csBGR;
              TargetSamplesPerPixel := 3;
            end;
          csCIELab:
            begin
              // PSD uses 0..255 for a and b so we need to convert them to -128..127
              SourceOptions := SourceOptions + [coLabByteRange, coLabChromaOffset];
              TargetColorScheme := csBGR;
            end;
        end;
      end;
    
      PixelFormat := ColorManager.TargetPixelFormat;
      Self.Width := Width;
      Self.Height := Height;

      // size of palette
      ReadBuffer(Count, SizeOf(Count));
      Count := SwapLong(Count);
      // setup the palette if necessary, color data immediately follows header
      case ColorScheme of
        csG:
          Palette := ColorManager.CreateGrayscalePalette(ioMinIsWhite in Options);
        csIndexed:
          begin
            ReadBuffer(RawPalette, Count);
            Count := Count div 3;
            Palette := ColorManager.CreateColorPalette([@RawPalette, @RawPalette[Count], @RawPalette[2 * Count]],
                                                       pfPlane8Triple, Count, False);
          end;
      end;

      // skip resource and layers section
      ReadBuffer(Count, SizeOf(Count));
      Count := SwapLong(Count);
      Seek(Count, soFromCurrent);
      ReadBuffer(Count, SizeOf(Count));
      Count := SwapLong(Count);
      // +2 in order to skip the following compression value
      Seek(Count + 2, soFromCurrent);

      // now read out image data
      RawBuffer := nil;

      if Compression = ctPackedBits then
      begin
        Decoder := TPackbitsRLEDecoder.Create;
        SetLength(RLELength, Height * Channels);
        ReadBuffer(RLELength[0], 2 * Length(RLELength));
        SwapShort(@RLELength[0], Height * Channels);
      end
      else Decoder := nil;

      Progress(Self, psEnding, 0, False, FProgressRect, '');

      try
        case ColorScheme of
          csG,
          csIndexed:
            begin
              Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
              // very simple format here, we don't need the color conversion manager
              if Assigned(Decoder) then
              begin
                // determine whole compressed size
                Count := 0;
                for Y := 0 to Height - 1 do Inc(Count, RLELength[Y]);
                GetMem(RawBuffer, Count);
                try
                  ReadBuffer(RawBuffer^, Count);
                  Run1 := RawBuffer;
                  for Y := 0 to Height - 1 do
                  begin
                    Count := RLELength[Y];
                    Line := ScanLine[Y];
                    Decoder.Decode(Pointer(Run1), Line, Count, Width);
                    Inc(Run1, Count);

                    Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                    OffsetRect(FProgressRect, 0, 1);
                  end;
                finally
                  if Assigned(RawBuffer) then FreeMem(RawBuffer);
                end;
              end
              else // uncompressed data 
                for Y := 0 to Height - 1 do
                begin
                  ReadBuffer(ScanLine[Y]^, Width);

                  Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                  OffsetRect(FProgressRect, 0, 1);
                end;
            end;
          csRGB,
          csRGBA,
          csCMYK,
          csCIELab:
            begin
              Progress(Self, psStarting, 0, False, FProgressRect, gesLoadingData);
              // Data is organized in planes. This means first all red rows, then
              // all green and finally all blue rows. 
              BPS := BitsPerSample div 8;
              ChannelSize := BPS * Width * Height;

              GetMem(Buffer, Channels * ChannelSize);
              try
                // first run: load image data and decompress it if necessary
                if Assigned(Decoder) then
                begin
                  // determine whole compressed size
                  Count := 0;
                  for Y := 0 to High(RLELength) do Inc(Count, RLELength[Y]);
                  Count := Count * Cardinal(BPS);
                  GetMem(RawBuffer, Count);
                  try
                    ReadBuffer(RawBuffer^, Count);
                    Decoder.Decode(RawBuffer, Buffer, Count, Channels * ChannelSize);
                  finally
                    if Assigned(RawBuffer) then FreeMem(RawBuffer);
                  end;
                end
                else
                  ReadBuffer(Buffer^, Channels * ChannelSize);

                Progress(Self, psEnding, 0, False, FProgressRect, '');

                Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
                Increment := BPS * Width;
                // second run: put data into image (convert color space if necessary)
                case ColorScheme of
                  csRGB:
                    begin
                      Run1 := Buffer;
                      Run2 := Run1; Inc(Run2, ChannelSize);
                      Run3 := Run2; Inc(Run3, ChannelSize);
                      for Y := 0 to Height - 1 do
                      begin
                        ColorManager.ConvertRow([Run1, Run2, Run3], ScanLine[Y], Width, $FF);
                        Inc(Run1, Increment);
                        Inc(Run2, Increment);
                        Inc(Run3, Increment);

                        Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                        OffsetRect(FProgressRect, 0, 1);
                      end;
                    end;
                  csRGBA:
                    begin
                      Run1 := Buffer;
                      Run2 := Run1; Inc(Run2, ChannelSize);
                      Run3 := Run2; Inc(Run3, ChannelSize);
                      Run4 := Run3; Inc(Run4, ChannelSize);
                      for Y := 0 to Height - 1 do
                      begin
                        ColorManager.ConvertRow([Run1, Run2, Run3, Run4], ScanLine[Y], Width, $FF);
                        Inc(Run1, Increment);
                        Inc(Run2, Increment);
                        Inc(Run3, Increment);
                        Inc(Run4, Increment);

                        Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                        OffsetRect(FProgressRect, 0, 1);
                      end;
                    end;
                  csCMYK:
                    begin
                      // Photoshop CMYK values are given with 0 for maximum values, but the
                      // (general) CMYK conversion works with 255 as maxium value. Hence we must reverse
                      // all entries in the buffer.
                      Run1 := Buffer;
                      for Y := 1 to 4 * ChannelSize do
                      begin
                        Run1^ := 255 - Run1^;
                        Inc(Run1);
                      end;

                      Run1 := Buffer;
                      Run2 := Run1; Inc(Run2, ChannelSize);
                      Run3 := Run2; Inc(Run3, ChannelSize);
                      Run4 := Run3; Inc(Run4, ChannelSize);
                      for Y := 0 to Height - 1 do
                      begin
                        ColorManager.ConvertRow([Run1, Run2, Run3, Run4], ScanLine[Y], Width, $FF);
                        Inc(Run1, Increment);
                        Inc(Run2, Increment);
                        Inc(Run3, Increment);
                        Inc(Run4, Increment);

                        Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                        OffsetRect(FProgressRect, 0, 1);
                      end;
                    end;
                  csCIELab:
                    begin
                      Run1 := Buffer;
                      Run2 := Run1; Inc(Run2, ChannelSize);
                      Run3 := Run2; Inc(Run3, ChannelSize);
                      for Y := 0 to Height - 1 do
                      begin
                        ColorManager.ConvertRow([Run1, Run2, Run3], ScanLine[Y], Width, $FF);
                        Inc(Run1, Increment);
                        Inc(Run2, Increment);
                        Inc(Run3, Increment);

                        Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                        OffsetRect(FProgressRect, 0, 1);
                      end;
                    end;
                end;
              finally
                if Assigned(Buffer) then FreeMem(Buffer);
              end;
            end;
        end;

      finally
        Decoder.Free;
        Progress(Self, psEnding, 0, False, FProgressRect, '');
      end;
    end;
  end                                   
  else GraphicExError(gesInvalidImage, ['PSD or PDD']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPSDGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

var
  Header: TPSDHeader;
  Dummy: Word;
  Count: Cardinal;

begin
  Result := inherited ReadImageProperties(Stream, ImageIndex);
  with Stream, FImageProperties do
  begin
    ReadBuffer(Header, SizeOf(Header));
    if Header.Signature = '8BPS' then
    begin
      with Header do
      begin
        // PSD files are big endian only
        Channels := Swap(Channels);
        Rows := SwapLong(Rows);
        Columns := SwapLong(Columns);
        Depth := Swap(Depth);
        Mode := Swap(Mode);
      end;

      Options := [ioBigEndian];
      // initialize color manager
      BitsPerSample := Header.Depth;
      Channels := Header.Channels;
      // 1..24 channels are supported in PSD files, we can only use 4.
      // The documentation states that main image data (rgb(a), cmyk etc.) is always
      // written as first channels in their component order.
      if Channels > 4 then SamplesPerPixel := 4
                      else SamplesPerPixel := Channels;

      BitsPerPixel := SamplesPerPixel * BitsPerSample;

      // color space
      case Header.Mode of
        PSD_DUOTONE, // duo tone should be handled as grayscale
        PSD_GRAYSCALE:
          ColorScheme := csG;
        PSD_BITMAP:  // B&W
          begin
            ColorScheme := csG;
            Include(Options, ioMinIsWhite);
          end;
        PSD_INDEXED: // 8 bits only are assumed because 16 bit wouldn't make sense here
          ColorScheme := csIndexed;
        PSD_MULTICHANNEL,
        PSD_RGB:
          if Header.Channels = 3 then ColorScheme := csRGB
                                 else ColorScheme := csRGBA;
        PSD_CMYK:
          ColorScheme := csCMYK;
        PSD_LAB:
          ColorScheme := csCIELab;
      end;

      Width := Header.Columns;
      Height := Header.Rows;

      // size of palette
      ReadBuffer(Count, SizeOf(Count));
      Count := SwapLong(Count);
      // skip palette (count is always given, might be 0 however, e.g. for RGB)
      Seek(Count, soFromCurrent);

      // skip resource and layers section
      ReadBuffer(Count, SizeOf(Count));
      Count := SwapLong(Count);
      Seek(Count, soFromCurrent);
      ReadBuffer(Count, SizeOf(Count));
      Count := SwapLong(Count);
      Seek(Count, soFromCurrent);

      ReadBuffer(Dummy, SizeOf(Dummy));
      if Swap(Dummy) = 1 then Compression := ctPackedBits
                         else Compression := ctNone;
      Result := True;
    end;
  end;
end;

{$endif} // PhotoshopGraphic

//----------------- TPSPGraphic ----------------------------------------------------------------------------------------

{$ifdef PaintshopProGraphic}

const
  // block identifiers
  PSP_IMAGE_BLOCK = 0;          // General Image Attributes Block (main)
  PSP_CREATOR_BLOCK = 1;        // Creator Data Block (main)
  PSP_COLOR_BLOCK = 2;          // Color Palette Block (main and sub)
  PSP_LAYER_START_BLOCK = 3;    // Layer Bank Block (main)
    PSP_LAYER_BLOCK = 4;          // Layer Block (sub)
    PSP_CHANNEL_BLOCK = 5;        // Channel Block (sub)
  PSP_SELECTION_BLOCK = 6;      // Selection Block (main)
  PSP_ALPHA_BANK_BLOCK = 7;     // Alpha Bank Block (main)
    PSP_ALPHA_CHANNEL_BLOCK = 8;  // Alpha Channel Block (sub)
  PSP_THUMBNAIL_BLOCK = 9;      // Thumbnail Block (main)
  PSP_EXTENDED_DATA_BLOCK = 10; // Extended Data Block (main)
  PSP_TUBE_BLOCK = 11;          // Picture Tube Data Block (main)
    PSP_ADJUSTMENT_EXTENSION_BLOCK = 12; // Adjustment Layer Extension Block (sub)
    PSP_VECTOR_EXTENSION_BLOCK = 13;     // Vector Layer Extension Block (sub)
    PSP_SHAPE_BLOCK = 14;                // Vector Shape Block (sub)
    PSP_PAINTSTYLE_BLOCK = 15;           // Paint Style Block (sub)
  PSP_COMPOSITE_IMAGE_BANK_BLOCK = 16; // Composite Image Bank (main)
    PSP_COMPOSITE_ATTRIBUTES_BLOCK = 17; // Composite Image Attributes (sub)
    PSP_JPEG_BLOCK = 18;                 // JPEG Image Block (sub)

  // bitmap types
	PSP_DIB_IMAGE = 0;            // Layer color bitmap
	PSP_DIB_TRANS_MASK = 1;       // Layer transparency mask bitmap
	PSP_DIB_USER_MASK = 2;        // Layer user mask bitmap
	PSP_DIB_SELECTION= 3;         // Selection mask bitmap
	PSP_DIB_ALPHA_MASK = 4;       // Alpha channel mask bitmap
	PSP_DIB_THUMBNAIL = 5;        // Thumbnail bitmap
  PSP_DIB_THUMBNAIL_TRANS_MASK = 6; // Thumbnail transparency mask
  PSP_DIB_ADJUSTMENT_LAYER = 7; // Adjustment layer bitmap
  PSP_DIB_COMPOSITE = 8;        // Composite image bitmap
  PSP_DIB_COMPOSITE_TRANS_MASK = 9; // Composite image transparency

  // composite image type
  PSP_IMAGE_COMPOSITE = 0;      // Composite Image
  PSP_IMAGE_THUMBNAIL = 1;      // Thumbnail Image

  // graphic contents flags
  PSP_GC_RASTERLAYERS = 1;      // At least one raster layer
  PSP_GC_VectorLayers = 2;      // At least one vector layer
  PSP_GC_ADJUSTMENTLAYERS = 4;  // At least one adjustment layer
  // Additional attributes
  PSP_GC_THUMBNAIL = $01000000;              // Has a thumbnail
  PSP_GC_THUMBNAILTRANSPARENCY = $02000000;  // Thumbnail transp.
  PSP_GC_COMPOSITE = $04000000;              // Has a composite image
  PSP_GC_COMPOSITETRANSPARENCY = $08000000;  // Composite transp.
  PSP_GC_FLATIMAGE = $10000000;              // Just a background
  PSP_GC_SELECTION = $20000000;              // Has a selection
  PSP_GC_FLOATINGSELECTIONLAYER = $40000000; // Has float. selection
  PSP_GC_ALPHACHANNELS = $80000000;          // Has alpha channel(s)

  // character style flags
  PSP_STYLE_ITALIC = 1;         // Italic property bit
  PSP_STYLE_STRUCK = 2;         // Strike-out property bit
  PSP_STYLE_UNDERLINED = 4;     // Underlined property bit

  // layer flags
	PSP_LAYER_VISIBLEFLAG = 1;    // Layer is visible
	PSP_LAYER_MASKPRESENCEFLAG = 2; // Layer has a mask

  // Shape property flags
  PSP_SHAPE_ANTIALIASED = 1;    // Shape is anti-aliased
  PSP_SHAPE_Selected = 2;       // Shape is selected
  PSP_SHAPE_Visible = 4;        // Shape is visible

  // Polyline node type flags
  PSP_NODE_UNCONSTRAINED = 0;   // Default node type
  PSP_NODE_SMOOTH = 1;          // Node is smooth
  PSP_NODE_SYMMETRIC = 2;       // Node is symmetric
  PSP_NODE_ALIGNED = 4;         // Node is aligned
  PSP_NODE_ACTIVE = 8;          // Node is active
  PSP_NODE_LOCKED = 16;         // Node is locked (PSP doc says 0x16 here, but this seems to be a typo)
  PSP_NODE_SELECTED = 32;       // Node is selected (PSP doc says 0x32 here)
  PSP_NODE_VISIBLE = 64;        // Node is visible (PSP doc says 0x64 here)
  PSP_NODE_CLOSED = 128;        // Node is closed (PSP doc says 0x128 here)

  // Blend modes
	LAYER_BLEND_NORMAL = 0;
  LAYER_BLEND_DARKEN = 1;
  LAYER_BLEND_LIGHTEN = 2;
  LAYER_BLEND_HUE = 3;
  LAYER_BLEND_SATURATION = 4;
  LAYER_BLEND_COLOR = 5;
  LAYER_BLEND_LUMINOSITY = 6;
  LAYER_BLEND_MULTIPLY = 7;
  LAYER_BLEND_SCREEN = 8;
  LAYER_BLEND_DISSOLVE = 9;
  LAYER_BLEND_OVERLAY = 10;
  LAYER_BLEND_HARD_LIGHT = 11;
  LAYER_BLEND_SOFT_LIGHT = 12;
  LAYER_BLEND_DIFFERENCE = 130;
  LAYER_BLEND_DODGE = 14;
  LAYER_BLEND_BURN = 15;
  LAYER_BLEND_EXCLUSION = 16;
  LAYER_BLEND_ADJUST = 255;

  // Adjustment layer types
  PSP_ADJUSTMENT_NONE = 0;      // Undefined adjustment layer type
  PSP_ADJUSTMENT_LEVEL = 1;     // Level adjustment
  PSP_ADJUSTMENT_CURVE = 2;     // Curve adjustment
  PSP_ADJUSTMENT_BRIGHTCONTRAST = 3; // Brightness-contrast adjustment
  PSP_ADJUSTMENT_COLORBAL = 4;  // Color balance adjustment
  PSP_ADJUSTMENT_HSL = 5;       // HSL adjustment
  PSP_ADJUSTMENT_CHANNELMIXER = 6; // Channel mixer adjustment
  PSP_ADJUSTMENT_INVERT = 7;    // Invert adjustment
  PSP_ADJUSTMENT_THRESHOLD = 8; // Threshold adjustment
  PSP_ADJUSTMENT_POSTER = 9;    // Posterize adjustment

  // Vector shape types
  PSP_VST_Unknown = 0;          // Undefined vector type
  PSP_VST_TEXT = 1;             // Shape represents lines of text
  PSP_VST_POLYLINE = 2;         // Shape represents a multiple segment line
  PSP_VST_ELLIPSE = 3;          // Shape represents an ellipse (or circle)
  PSP_VST_POLYGON = 4;          // Shape represents a closed polygon

  // Text element types
  PSP_TET_UNKNOWN = 0;          // Undefined text element type
  PSP_TET_CHAR = 1;             // A single character code
  PSP_TET_CHARSTYLE = 2;        // A character style change
  PSP_TET_LINESTYLE = 3;        // A line style change

  // Text alignment types
  PSP_TAT_LEFT = 0;             // Left text alignment
  PSP_TAT_CENTER = 1;           // Center text alignment
  PSP_TAT_RIGHT = 2;            // Right text alignment

  // Paint style types
  PSP_STYLE_NONE = 0;           // Undefined paint style
  PSP_STYLE_COLOR = 1;          // Paint using color (RGB or palette index)
  PSP_STYLE_GRADIENT = 2;       // Paint using gradient

  // Channel types
	PSP_CHANNEL_COMPOSITE = 0;    // Channel of single channel bitmap
	PSP_CHANNEL_RED = 1;          // Red channel of 24 bit bitmap
	PSP_CHANNEL_GREEN = 2;        // Green channel of 24 bit bitmap
	PSP_CHANNEL_BLUE = 3;         // Blue channel of 24 bit bitmap

  // Resolution metrics
  PSP_METRIC_UNDEFINED = 0;	    // Metric unknown
  PSP_METRIC_INCH = 1;          // Resolution is in inches
  PSP_METRIC_CM = 2;            // Resolution is in centimeters

  // Compression types
	PSP_COMP_NONE = 0;            // No compression
	PSP_COMP_RLE = 1;             // RLE compression
	PSP_COMP_LZ77 = 2;            // LZ77 compression
  PSP_COMP_JPEG = 3;            // JPEG compression (only used by thumbnail and composite image)

  // Picture tube placement mode
	PSP_TPM_Random = 0;           // Place tube images in random intervals
	PSPS_TPM_Constant = 1;        // Place tube images in constant intervals

  // Tube selection mode
	PSP_TSM_RANDOM =0;            // Randomly select the next image in tube to display
	PSP_TSM_INCREMENTAL = 1;     // Select each tube image in turn
	PSP_TSM_ANGULAR = 2;          // Select image based on cursor direction
	PSP_TSM_PRESSURE = 3;         // Select image based on pressure (from pressure-sensitive pad)
	PSP_TSM_VELOCITY = 4;         // Select image based on cursor speed

  // Extended data field types
  PSP_XDATA_TRNS_INDEX = 0;     // Transparency index field

  // Creator field types
	PSP_CRTR_FLD_TITLE = 0;       // Image document title field
	PSP_CRTR_FLD_CRT_DATE = 1;    // Creation date field
	PSP_CRTR_FLD_MOD_DATE = 2;    // Modification date field
	PSP_CRTR_FLD_ARTIST = 3;      // Artist name field
	PSP_CRTR_FLD_CPYRGHT = 4;     // Copyright holder name field
	PSP_CRTR_FLD_DESC = 5;        // Image document description field
	PSP_CRTR_FLD_APP_ID = 6;      // Creating app id field
	PSP_CRTR_FLD_APP_VER = 7;     // Creating app version field

  // Creator application identifier
	PSP_CREATOR_APP_UNKNOWN = 0;  // Creator application unknown
	PSP_CREATOR_APP_PAINT_SHOP_PRO = 1; // Creator is Paint Shop Pro

  // Layer types (file version 3)
  PSP_LAYER_NORMAL = 0;         // Normal layer
  PSP_LAYER_FLOATING_SELECTION = 1; // Floating selection layer

  // Layer types (file version 4)
  PSP_LAYER_UNDEFINED = 0;      // Undefined layer type
  PSP_LAYER_RASTER = 1;         // Standard raster layer
  PSP_LAYER_FLOATINGRASTERSELECTION = 2; // Floating selection (raster layer)
  PSP_LAYER_Vector = 3;         // Vector layer
  PSP_LAYER_ADJUSTMENT = 4;     // Adjustment layer

  MagicID = 'Paint Shop Pro Image File';

type
  // These block header structures are here for informational purposes only because the data of those
  // headers is read member by member to generalize code for the different file versions
  TPSPBlockHeader3 = packed record          // block header file version 3
    HeaderIdentifier: array[0..3] of Char;  // i.e. "~BK" followed by a zero byte
    BlockIdentifier: Word;                  // one of the block identifiers
    InitialChunkLength,                     // length of the first sub chunk header or similar
    TotalBlockLength: Cardinal;             // length of this block excluding this header
  end;

  TPSPBlockHeader4 = packed record          // block header file version 4
    HeaderIdentifier: array[0..3] of Char;  // i.e. "~BK" followed by a zero byte
    BlockIdentifier: Word;                  // one of the block identifiers
    TotalBlockLength: Cardinal;             // length of this block excluding this header
  end;

  TPSPColorPaletteInfoChunk = packed record
    EntryCount: Cardinal;                   // number of entries in the palette
  end;

  TPSPColorPaletteChunk = array[0..255] of TRGBQuad; // might actually be shorter 

  TPSPChannelInfoChunk = packed record
    CompressedSize,
    UncompressedSize: Cardinal;
    BitmapType,                             // one of the bitmap types
    ChannelType: Word;                      // one of the channel types
  end;

  // PSP defines a channel content chunk which is just a bunch of bytes (size is CompressedSize).
  // There is no sense to define this record type here.

  TPSPFileHeader = packed record
    Signature: array[0..31] of Char;        // the string "Paint Shop Pro Image File\n\x1a", padded with zeroes
    MajorVersion,
    MinorVersion: Word;                
  end;

  TPSPImageAttributes = packed record
    Width,
    Height: Integer;
    Resolution: Double;                     // Number of pixels per metric
    ResolutionMetric: Byte;                 // Metric used for resolution (one of the metric constants)
    Compression,                            // compression type of image (not thumbnail, it has its own compression)
    BitDepth,                               // The bit depth of the color bitmap in each Layer of the image document
                                            // (must be 1, 4, 8 or 24).
    PlaneCount: Word;                       // Number of planes in each layer of the image document (usually 1)
    ColorCount: Cardinal;                   // number of colors in each layer (2^bit depth)
    GreyscaleFlag: Boolean;                 // Indicates whether the color bitmap in each layer of image document is a
                                            // greyscale (False = not greyscale, True = greyscale).
    TotalImageSize: Cardinal;               // Sum of the sizes of all layer color bitmaps.
    ActiveLayer: Integer;                   // Identifies the layer that was active when the image document was saved.
    LayerCount: Word;                       // Number of layers in the document.
    GraphicContents: Cardinal;              // A series of flags that helps define the image's graphic contents.
  end;

  TPSPLayerInfoChunk = packed record
    //LayerName: array[0..255] of Char;     // Name of layer (in ASCII text). Has been replaced in version 4
                                            // by a Delphi like short string (length word and variable length string)
    LayerType: Byte;                        // Type of layer.
    ImageRectangle,                         // Rectangle defining image border.
    SavedImageRectangle: TRect;             // Rectangle within image rectangle that contains "significant" data
                                            // (only the contents of this rectangle are saved to the file).
    LayerOpacity: Byte;                     // Overall layer opacity.
    BlendingMode: Byte;                     // Mode to use when blending layer.
    Visible: Boolean;                       // TRUE if layer was visible at time of save, FALSE otherwise.
    TransparencyProtected: Boolean;         // TRUE if transparency is protected.
    LinkGroupIdentifier: Byte;              // Identifies group to which this layer belongs.
    MaskRectangle,                          // Rectangle defining user mask border.
    SavedMaskRectangle: TRect;              // Rectangle within mask rectangle that contains "significant" data
                                            // (only the contents of this rectangle are saved to the file).
    MaskLinked: Boolean;                    // TRUE if mask linked to layer (i.e., mask moves relative to layer)
    MaskDisabled: Boolean;                  // TRUE if mask is disabled, FALSE otherwise.
    InvertMask: Boolean;                    // TRUE if mask should be inverted when the layer is merged, FALSE otherwise.
    BlendRangeCount: Word;                  // Number of valid source-destination field pairs to follow (note, there are
                                            // currently always 5 such pairs, but they are not necessarily all valid).
    SourceBlendRange1,                      // First source blend range value.
    DestinationBlendRange1,                 // First destination blend range value.
    SourceBlendRange2,
    DestinationBlendRange2,
    SourceBlendRange3,
    DestinationBlendRange3,
    SourceBlendRange4,
    DestinationBlendRange4,
    SourceBlendRange5,
    DestinationBlendRange5: array[0..3] of Byte;
    // these fields are obsolete since file version 4 because there's an own chunk for them
    // BitmapCount: Word;                      // Number of bitmaps to follow.
    // ChannelCount: Word;                     // Number of channels to follow.
  end;

//----------------------------------------------------------------------------------------------------------------------

class function TPSPGraphic.CanLoad(Stream: TStream): Boolean;

var
  Header: TPSPFileHeader;
  LastPosition: Cardinal;

begin
  with Stream do
  begin
    LastPosition := Position;
    Result := (Size - Position) > SizeOf(Header);
    if Result then
    begin
      ReadBuffer(Header, SizeOf(Header));
      Result := (StrLIComp(Header.Signature, MagicID, Length(MagicID)) = 0) and
                (Header.MajorVersion >= 3);
    end;
    Position := LastPosition;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPSPGraphic.LoadFromStream(Stream: TStream);

var
  Header: TPSPFileHeader;
  Image: TPSPImageAttributes;
  // to use the code below for file 3 and 4 I read the parts of the block header
  // separately instead as a structure
  HeaderIdentifier: array[0..3] of Char;  // i.e. "~BK" followed by a zero byte
  BlockIdentifier: Word;                  // one of the block identifiers
  InitialChunkLength,                     // length of the first sub chunk header or similar
  TotalBlockLength: Cardinal;             // length of this block excluding this header

  LastPosition,
  ChunkSize: Cardinal;
  LayerInfo: TPSPLayerInfoChunk;
  ChannelInfo: TPSPChannelInfoChunk;
  LayerName: String;
  NameLength: Word;
  NextLayerPosition,
  NextMainBlock: Integer;

  // file version 4 specific data
  BitmapCount,
  ChannelCount: Word;

  // load and decoding of image data
  R, G, B, C: PByte;
  RedBuffer,
  GreenBuffer,
  BlueBuffer,
  CompBuffer: Pointer;
  X, Y,
  Index,
  RowSize: Integer; // size in bytes of one scanline 

  // other data
  RawPalette: array[0..4 * 256 - 1] of Byte;

  //--------------- local functions -------------------------------------------

  function ReadBlockHeader: Boolean;

  // Fills in the block header variables according to the file version.
  // Returns True if a block header could be read otherwise False (stream end). 

  begin
    with Stream do
    begin
      Result := Position < Size;
      if Result then
      begin
        ReadBuffer(HeaderIdentifier, SizeOf(HeaderIdentifier));
        ReadBuffer(BlockIdentifier, SizeOf(BlockIdentifier));
        if Header.MajorVersion = 3 then ReadBuffer(InitialChunkLength, SizeOf(InitialChunkLength));
        ReadBuffer(TotalBlockLength, SizeOf(TotalBlockLength));
      end;
    end;
  end;

  //---------------------------------------------------------------------------

  procedure ReadAndDecompress(Target: Pointer);

  // reads a stream of data from file stream and decompresses it into Target

  var
    RawBuffer: Pointer;
    Decoder: TDecoder;
    Source: Pointer;

  begin
    Decoder := nil;
    GetMem(RawBuffer, ChannelInfo.CompressedSize);
    try
      Stream.ReadBuffer(RawBuffer^, ChannelInfo.CompressedSize);
      // pointer might be advanced while decoding, so use a copy
      Source := RawBuffer;
      case Image.Compression of
        PSP_COMP_RLE:
          begin
            Decoder := TPSPRLEDecoder.Create;
            Decoder.Decode(Source, Target, ChannelInfo.CompressedSize, ChannelInfo.UncompressedSize);
          end;
        PSP_COMP_LZ77:
          begin
            Decoder := TLZ77Decoder.Create(Z_FINISH, False);
            Decoder.DecodeInit;
            Decoder.Decode(Source, Target, ChannelInfo.CompressedSize, ChannelInfo.UncompressedSize);
          end;
        PSP_COMP_JPEG: // here just for completeness, used only in thumbnails and composite images
          ;
      end;
      Decoder.DecodeEnd;
    finally
      if Assigned(RawBuffer) then FreeMem(RawBuffer);
      Decoder.Free;
    end;
  end;

  //---------------------------------------------------------------------------

  procedure ReadChannelData;

  // Reads the actual data of one channel from the current stream position.
  // Decompression is done by the way.

  begin
    with Stream do
    begin
      ReadBlockHeader;
      if Header.MajorVersion > 3 then ReadBuffer(ChunkSize, SizeOf(ChunkSize));
      ReadBuffer(ChannelInfo, SizeOf(ChannelInfo));
      case ChannelInfo.ChannelType of
        PSP_CHANNEL_COMPOSITE: // single channel bitmap (indexed or transparency mask)
          begin
            GetMem(CompBuffer, ChannelInfo.UncompressedSize);
            if Image.Compression <> PSP_COMP_NONE then ReadAndDecompress(CompBuffer)
                                                  else ReadBuffer(CompBuffer^, ChannelInfo.CompressedSize);
          end;
        PSP_CHANNEL_RED:  // red channel of 24 bit bitmap
          begin
            GetMem(RedBuffer, ChannelInfo.UncompressedSize);
            if Image.Compression <> PSP_COMP_NONE then ReadAndDecompress(RedBuffer)
                                                  else ReadBuffer(RedBuffer^, ChannelInfo.CompressedSize);
          end;
        PSP_CHANNEL_GREEN:
          begin
            GetMem(GreenBuffer, ChannelInfo.UncompressedSize);
            if Image.Compression <> PSP_COMP_NONE then ReadAndDecompress(GreenBuffer)
                                                  else ReadBuffer(GreenBuffer^, ChannelInfo.CompressedSize);
          end;
        PSP_CHANNEL_BLUE:
          begin
            GetMem(BlueBuffer, ChannelInfo.UncompressedSize);
            if Image.Compression <> PSP_COMP_NONE then ReadAndDecompress(BlueBuffer)
                                                  else ReadBuffer(BlueBuffer^, ChannelInfo.CompressedSize);
          end;
      end;
    end;
  end;

  //--------------- end local functions ---------------------------------------

begin
  // free previous image data
  Handle := 0;
  FBasePosition := Stream.Position;
  if ReadImageProperties(Stream, 0) then
  begin
    Stream.Position := FBasePosition;
    RedBuffer := nil;
    GreenBuffer := nil;
    BlueBuffer := nil;
    with Stream, FImageProperties do
    try
      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      // Note: To be robust with future PSP images any reader must be able to skip data
      //       which it doesn't know instead of relying on the size of known structures.
      //       Hence there's some extra work needed with the stream (mainly to keep the
      //       current position before a chunk is read and advancing the stream using the
      //       chunk size field).
      ReadBuffer(Header, SizeOf(Header));

      // read general image attribute block
      ReadBlockHeader;
      LastPosition := Position;
      if Version > 3 then ReadBuffer(ChunkSize, SizeOf(ChunkSize));
      ReadBuffer(Image, SizeOf(Image));
      Position := LastPosition + TotalBlockLength;

      with ColorManager, Image do
      begin
        SourceOptions := [];
        SourceBitsPerSample := BitsPerSample;
        TargetBitsPerSample := BitsPerSample;
        SourceSamplesPerPixel := SamplesPerPixel;
        TargetSamplesPerPixel := SamplesPerPixel;
        SourceColorScheme := ColorScheme;
        if ColorScheme = csRGB then TargetColorScheme := csBGR
                               else TargetColorScheme := ColorScheme;

        PixelFormat := TargetPixelFormat;
      end;

      // set bitmap properties
      RowSize := 0; // make compiler quiet
      case BitsPerSample of
        1:
          RowSize := (Image.Width + 7) div 8;
        4:
          RowSize := Image.Width div 2 + 1;
        8:
          RowSize := Image.Width;
      else
        GraphicExError(gesInvalidColorFormat, ['PSP']);
      end;

      Self.Width := Width;
      Self.Height := Height;
      Progress(Self, psEnding, 0, False, FProgressRect, '');

      // go through main blocks and read what is needed
      repeat
        if not ReadBlockHeader then Break;
        NextMainBlock := Position + Integer(TotalBlockLength);
        // no more blocks?
        if HeaderIdentifier[0] <> '~' then Break;

        case BlockIdentifier of
          PSP_COMPOSITE_IMAGE_BANK_BLOCK:
            begin
              // composite image block, if present then it must appear before the layer start block
              // and represents a composition of several layers

              // do not need to read anything further
              //Break;
            end;
          PSP_LAYER_START_BLOCK:
            repeat
              if not ReadBlockHeader then Break;

              Progress(Self, psStarting, 0, False, FProgressRect, gesLoadingData);

              // calculate start of next (layer) block in case we need to skip this one
              NextLayerPosition := Position + Integer(TotalBlockLength);
              // if all layers have been considered the break loop to continue with other blocks if necessary
              if BlockIdentifier <> PSP_LAYER_BLOCK then Break;

              // layer information chunk
              if Version > 3 then
              begin
                LastPosition := Position;
                ReadBuffer(ChunkSize, SizeOf(ChunkSize));
                ReadBuffer(NameLength, SizeOf(NameLength));
                SetLength(LayerName, NameLength);
                if NameLength > 0 then ReadBuffer(LayerName[1], NameLength);
                ReadBuffer(LayerInfo, SizeOf(LayerInfo));
                Position := LastPosition + ChunkSize;

                // continue only with undefined or raster chunks
                if not (LayerInfo.LayerType in [PSP_LAYER_UNDEFINED, PSP_LAYER_RASTER]) then
                begin
                  Position := NextLayerPosition;
                  Continue;
                end;

                // in file version 4 there's also an additional bitmap chunk which replaces
                // two fields formerly located in the LayerInfo chunk
                LastPosition := Position;
                ReadBuffer(ChunkSize, SizeOf(ChunkSize));
              end
              else
              begin
                SetLength(LayerName, 256);
                ReadBuffer(LayerName[1], 256);
                ReadBuffer(LayerInfo, SizeOf(LayerInfo));

                // continue only with normal (raster) chunks
                if LayerInfo.LayerType <> PSP_LAYER_NORMAL then
                begin
                  Position := NextLayerPosition;
                  Continue;
                end;
              end;

              ReadBuffer(BitmapCount, SizeOf(BitmapCount));
              ReadBuffer(ChannelCount, SizeOf(ChannelCount));

              // But now we can reliably say whether we have an alpha channel or not.
              // This kind of information can only be read very late and causes us to
              // possibly reallocate the entire image (because it is copied by the VCL
              // when changing the pixel format).
              // I don't know another way (preferably before the size of the image is set).
              if ChannelCount > 3 then
              begin
                ColorManager.TargetColorScheme := csBGRA;
                PixelFormat := pf32Bit;
              end;

              if Version > 3 then Position := LastPosition + ChunkSize;

              // allocate memory for all channels and read raw data
              for X := 0 to ChannelCount - 1 do ReadChannelData;
              Progress(Self, psEnding, 0, False, FProgressRect, '');

              Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
              R := RedBuffer;
              G := GreenBuffer;
              B := BlueBuffer;
              C := CompBuffer;
              with ColorManager do
              begin
                if TargetColorScheme in [csIndexed, csG] then
                begin
                  for Y := 0 to Height - 1 do
                  begin
                    ColorManager.ConvertRow([C], ScanLine[Y], Width, $FF);
                    Inc(C, RowSize);

                    Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                    OffsetRect(FProgressRect, 0, 1);
                  end;
                end
                else
                begin
                  for Y := 0 to Height - 1 do
                  begin
                    ColorManager.ConvertRow([R, G, B, C], ScanLine[Y], Width, $FF);
                    Inc(R, RowSize);
                    Inc(G, RowSize);
                    Inc(B, RowSize);
                    Inc(C, RowSize);

                    Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                    OffsetRect(FProgressRect, 0, 1);
                  end;
                end;
              end;
              Progress(Self, psEnding, 0, False, FProgressRect, '');
              // after the raster layer has been read there's no need to loop further
              Break;
            until False; // layer loop
          PSP_COLOR_BLOCK:  // color palette block (this is also present for gray scale and b&w images)
            begin
              if Version > 3 then ReadBuffer(ChunkSize, SizeOf(ChunkSize));
              ReadBuffer(Index, SizeOf(Index));
              ReadBuffer(RawPalette, Index * SizeOf(TRGBQuad));
              Palette := ColorManager.CreateColorPalette([@RawPalette], pfInterlaced8Quad, Index, True);
            end;
        end;

        // explicitly set stream position to next main block as we might have read a block only partially
        Position := NextMainBlock;
      until False; // main block loop
    finally
      if Assigned(RedBuffer) then FreeMem(RedBuffer);
      if Assigned(GreenBuffer) then FreeMem(GreenBuffer);
      if Assigned(BlueBuffer) then FreeMem(BlueBuffer);
    end;
  end
  else GraphicExError(gesInvalidImage, ['PSP']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPSPGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

var
  Header: TPSPFileHeader;
  Image: TPSPImageAttributes;
  // to use the code below for file 3 and 4 I read the parts of the block header
  // separately instead as a structure
  HeaderIdentifier: array[0..3] of Char;  // i.e. "~BK" followed by a zero byte
  BlockIdentifier: Word;                  // one of the block identifiers
  InitialChunkLength,                     // length of the first sub chunk header or similar
  TotalBlockLength: Cardinal;             // length of this block excluding this header

  LastPosition,
  ChunkSize: Cardinal;

  //--------------- local functions -------------------------------------------

  function ReadBlockHeader: Boolean;

  // Fills in the block header variables according to the file version.
  // Returns True if a block header could be read otherwise False (stream end). 

  begin
    with Stream do
    begin
      Result := Position < Size;
      if Result then
      begin
        ReadBuffer(HeaderIdentifier, SizeOf(HeaderIdentifier));
        ReadBuffer(BlockIdentifier, SizeOf(BlockIdentifier));
        if Header.MajorVersion = 3 then ReadBuffer(InitialChunkLength, SizeOf(InitialChunkLength));
        ReadBuffer(TotalBlockLength, SizeOf(TotalBlockLength));
      end;
    end;
  end;

  //--------------- end local functions ---------------------------------------

begin
  Result := inherited ReadImageProperties(Stream, ImageIndex);
  with Stream, FImageProperties do
  begin
    ReadBuffer(Header, SizeOf(Header));
    if (StrLIComp(Header.Signature, MagicID, Length(MagicID)) = 0) and
       (Header.MajorVersion >= 3) then
    begin
      Version := Header.MajorVersion;

      // read general image attribute block
      ReadBlockHeader;
      LastPosition := Position;
      if Header.MajorVersion > 3 then ReadBuffer(ChunkSize, SizeOf(ChunkSize));
      ReadBuffer(Image, SizeOf(Image));
      Position := LastPosition + TotalBlockLength;

      if Image.BitDepth = 24 then
      begin
        BitsPerSample := 8;
        SamplesPerPixel := 3;
        ColorScheme := csRGB; // an alpha channel might exist, this is determined by the layer's channel count 
      end
      else
      begin
        BitsPerSample := Image.BitDepth;
        SamplesPerPixel := 1;
        if Image.GreyscaleFlag then ColorScheme := csG
                               else ColorScheme := csIndexed;
      end;
      BitsPerPixel := BitsPerSample * SamplesPerPixel;

      Width := Image.Width;
      Height := Image.Height;

      case Image.Compression of
        PSP_COMP_NONE:
          Compression := ctNone;
        PSP_COMP_RLE:
          Compression := ctRLE;
        PSP_COMP_LZ77:
          Compression := ctLZ77;
        PSP_COMP_JPEG:
          Compression := ctJPEG;
      else
        Compression := ctUnknown;
      end;
      XResolution := Image.Resolution;
      if Image.ResolutionMetric = PSP_METRIC_CM then XResolution := XResolution * 2.54;
      YResolution := XResolution;
      Result := True;
    end;
  end;
end;

{$endif} // PaintshopProGraphic

//----------------- TPNGGraphic ----------------------------------------------------------------------------------------

{$ifdef PortableNetworkGraphic}

const
  PNGMagic: array[0..7] of Byte = (137, 80, 78, 71, 13, 10, 26, 10);

  // recognized and handled chunk types
  IHDR = 'IHDR';
  IDAT = 'IDAT';
  IEND = 'IEND';
  PLTE = 'PLTE';
  gAMA = 'gAMA';
  tRNS = 'tRNS';
  bKGD = 'bKGD';

  CHUNKMASK = $20; // used to check bit 5 in chunk types

type
  // The following chunks structures are those which appear in the data field of the general chunk structure
  // given above.

  // chunk type: 'IHDR'
  PIHDRChunk = ^TIHDRChunk;
  TIHDRChunk = packed record
    Width,
    Height: Cardinal;
    BitDepth,          // bits per sample (allowed are 1, 2, 4, 8 and 16)
    ColorType,         // combination of:
                       //   1 - palette used
                       //   2 - colors used
                       //   4 - alpha channel used
                       // allowed values are:
                       //   0 - gray scale (allowed bit depths are: 1, 2, 4, 8, 16)
                       //   2 - RGB (8, 16)
                       //   3 - palette (1, 2, 4, 8)
                       //   4 - gray scale with alpha (8, 16)
                       //   6 - RGB with alpha (8, 16)
    Compression,       // 0 - LZ77, others are not yet defined
    Filter,            // filter mode 0 is the only one currently defined
    Interlaced: Byte;  // 0 - not interlaced, 1 - Adam7 interlaced
  end;

//----------------------------------------------------------------------------------------------------------------------

class function TPNGGraphic.CanLoad(Stream: TStream): Boolean;

var
  Magic: array[0..7] of Byte;
  LastPosition: Cardinal;

begin
  with Stream do
  begin
    LastPosition := Position;
    Result := (Size - Position) > SizeOf(Magic);
    if Result then
    begin
      ReadBuffer(Magic, SizeOf(Magic));
      Result := CompareMem(@Magic, @PNGMagic, 8);
    end;
    Position := LastPosition;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPNGGraphic.IsChunk(ChunkType: TChunkType): Boolean;

// determines, independant of the cruxial 5ths bits in each "letter", whether the
// current chunk type in the header is the same as the given chunk type

const
  Mask = not $20202020;

begin
  Result := (Cardinal(FHeader.ChunkType) and Mask) = (Cardinal(ChunkType) and Mask);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPNGGraphic.LoadAndSwapHeader: Cardinal;

// read next chunk header and swap fields to little endian,
// returns the intial CRC value for following checks

begin
  FStream.ReadBuffer(FHeader, SizeOf(FHeader));
  Result := CRC32(0, @FHeader.ChunkType, 4);
  FHeader.Length := SwapLong(FHeader.Length);
end;

//----------------------------------------------------------------------------------------------------------------------

function PaethPredictor(a, b, c: Byte): Byte;

var
  p, pa, pb, pc: Integer;

begin
  // a = left, b = above, c = upper left
  p := a + b - c;        // initial estimate
  pa := Abs(p - a);      // distances to a, b, c
  pb := Abs(p - b);
  pc := Abs(p - c);
  // return nearest of a, b, c, breaking ties in order a, b, c
  if (pa <= pb) and (pa <= pc) then Result := a
                               else
    if pb <= pc then Result := b
                else Result := c;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.ApplyFilter(Filter: Byte; Line, PrevLine, Target: PByte; BPP, BytesPerRow: Integer);

// Applies the filter given in Filter to all bytes in Line (eventually using PrevLine).
// Note: The filter type is assumed to be of filter mode 0, as this is the only one currently
//       defined in PNG.
//       In opposition to the PNG documentation different identifiers are used here.
//       Raw refers to the current, not yet decoded value. Decoded refers to the current, already
//       decoded value (this one is called "raw" in the docs) and Prior is the current value in the
//       previous line. For the Paeth prediction scheme a fourth pointer is used (PriorDecoded) to describe
//       the value in the previous line but less the BPP value (Prior[x - BPP]).      

var
  I: Integer;
  Raw,
  Decoded,
  Prior,
  PriorDecoded,
  TargetRun: PByte;

begin
  case Filter of
    0: // no filter, just copy data
      Move(Line^, Target^, BytesPerRow);
    1: // subtraction filter
      begin
        Raw := Line;
        TargetRun := Target;
        // Transfer BPP bytes without filtering. This mimics the effect of bytes left to the
        // scanline being zero.
        Move(Raw^, TargetRun^, BPP);

        // now do rest of the line
        Decoded := TargetRun;
        Inc(Raw, BPP);
        Inc(TargetRun, BPP);
        Dec(BytesPerRow, BPP);
        while BytesPerRow > 0 do
        begin
          TargetRun^ := Byte(Raw^ + Decoded^);
          Inc(Raw);
          Inc(Decoded);
          Inc(TargetRun);
          Dec(BytesPerRow);
        end;
      end;
    2: // Up filter
      begin
        Raw := Line;
        Prior := PrevLine;
        TargetRun := Target;
        while BytesPerRow > 0 do
        begin
          TargetRun^ := Byte(Raw^ + Prior^);
          Inc(Raw);
          Inc(Prior);
          Inc(TargetRun);
          Dec(BytesPerRow);
        end;
      end;
    3: // average filter
      begin
        // first handle BPP virtual pixels to the left
        Raw := Line;
        Decoded := Line;
        Prior := PrevLine;
        TargetRun := Target;
        for I := 0 to BPP - 1 do
        begin
          TargetRun^ := Byte(Raw^ + Floor(Prior^ / 2));
          Inc(Raw);
          Inc(Prior);
          Inc(TargetRun);
        end;
        Dec(BytesPerRow, BPP);

        // now do rest of line
        while BytesPerRow > 0 do
        begin
          TargetRun^ := Byte(Raw^ + Floor((Decoded^ + Prior^) / 2));
          Inc(Raw);
          Inc(Decoded);
          Inc(Prior);
          Inc(TargetRun);
          Dec(BytesPerRow);
        end;
      end;
   4: // paeth prediction
     begin
       // again, start with first BPP pixel which would refer to non-existing pixels to the left
       Raw := Line;
       Decoded := Target;
       Prior := PrevLine;
       PriorDecoded := PrevLine;
       TargetRun := Target;
       for I := 0 to BPP - 1 do
       begin
         TargetRun^ := Byte(Raw^ + PaethPredictor(0, Prior^, 0));
         Inc(Raw);
         Inc(Prior);
         Inc(TargetRun);
       end;
       Dec(BytesPerRow, BPP);

       // finally do rest of line
       while BytesPerRow > 0 do
       begin
         TargetRun^ := Byte(Raw^ + PaethPredictor(Decoded^, Prior^, PriorDecoded^));
          Inc(Raw);
          Inc(Decoded);
          Inc(Prior);
          Inc(PriorDecoded);
          Inc(TargetRun);
          Dec(BytesPerRow);
       end;
     end;
   end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.LoadFromStream(Stream: TStream);

var
  Description: TIHDRChunk;

begin
  // free previous image data
  Handle := 0;

  FBasePosition := Stream.Position;
  FDecoder := nil;
  FStream := Stream;
  if ReadImageProperties(Stream, 0) then
  begin
    with Stream, FImageProperties do
    begin
      Position := FBasePosition + 8; // skip magic

      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      FPalette := 0;
      FTransparency := nil;
      FBackgroundColor := clWhite;
      FTransparentColor := clNone;

      // first chunk must be an IHDR chunk
      FCurrentCRC := LoadAndSwapHeader;

      FRawBuffer := nil;
      ColorManager.SourceOptions := [coNeedByteSwap];
      try
        // read IHDR chunk
        ReadDataAndCheckCRC;
        Move(FRawBuffer^, Description, SizeOf(Description));
        SwapLong(@Description, 2);

        // currently only one compression type is supported by PNG (LZ77)
        if Compression = ctLZ77 then
        begin
          FDecoder := TLZ77Decoder.Create(Z_PARTIAL_FLUSH, False);
          FDecoder.DecodeInit;
        end
        else
          GraphicExError(gesUnsupportedFeature, [gesCompressionScheme, 'PNG']);

        // setup is done, now go for the chunks
        repeat
          FCurrentCRC := LoadAndSwapHeader;
          if IsChunk(IDAT) then
          begin
            Progress(Self, psEnding, 0, False, FProgressRect, '');
            LoadIDAT(Description);
            // After reading the image data the next chunk header has already been loaded
            // so continue with code below instead trying to load a new chunk header.
          end
          else
            if IsChunk(PLTE) then
            begin
              // palette chunk
              if (FHeader.Length mod 3) <> 0 then GraphicExError(gesInvalidPalette, ['PNG']);
              ReadDataAndCheckCRC;
              // load palette only if the image is indexed colors
              if Description.ColorType = 3 then
              begin
                // first setup pixel format before actually creating a palette
                FSourceBPP := SetupColorDepth(Description.ColorType, Description.BitDepth);
                FPalette := ColorManager.CreateColorPalette([FRawBuffer], pfInterlaced8Triple, FHeader.Length div 3, False);
              end;
              Continue;
            end
            else                             
              if IsChunk(gAMA) then
              begin
                ReadDataAndCheckCRC;
                // the file gamme given here is a scaled cardinal (e.g. 0.45 is expressed as 45000)
                ColorManager.SetGamma(SwapLong(PCardinal(FRawBuffer)^) / 100000);
                ColorManager.TargetOptions := ColorManager.TargetOptions + [coApplyGamma];
                Include(Options, ioUseGamma);
                Continue;
              end
              else
                if IsChunk(bKGD) then
                begin
                  LoadBackgroundColor(Description);
                  Continue;
                end
                else
                  if IsChunk(tRNS) then
                  begin
                    LoadTransparency(Description);
                    Continue;
                  end;

          // Skip unknown or unsupported chunks (+4 because of always present CRC).
          // IEND will be skipped as well, but this chunk is empty, so the stream will correctly
          // end on the first byte after the IEND chunk.
          Seek(FHeader.Length + 4, soFromCurrent);
          if IsChunk(IEND) then Break;

          // Note: According to the specs an unknown, but as critical marked chunk is a fatal error.
          if (Byte(FHeader.ChunkType[0]) and CHUNKMASK) = 0 then GraphicExError(gesUnknownCriticalChunk);
        until False;
      finally
        if Assigned(FDecoder) then
        begin
          FDecoder.DecodeEnd;
          FDecoder.Free;
        end;
        if Assigned(FRawBuffer) then FreeMem(FRawBuffer);
        Progress(Self, psEnding, 0, False, FProgressRect, '');
      end;
    end;
  end
  else GraphicExError(gesInvalidImage, ['PNG']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPNGGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

var
  Magic: array[0..7] of Byte;
  Description: TIHDRChunk;

begin
  Result := inherited ReadImageProperties(Stream, ImageIndex);
  FStream := Stream;
  with Stream, FImageProperties do
  begin
    ReadBuffer(Magic, 8);
    if CompareMem(@Magic, @PNGMagic, 8) then
    begin
      // first chunk must be an IHDR chunk
      FCurrentCRC := LoadAndSwapHeader;
      if IsChunk(IHDR) then
      begin
        Include(Options, ioBigEndian);
        // read IHDR chunk
        ReadDataAndCheckCRC;
        Move(FRawBuffer^, Description, SizeOf(Description));
        SwapLong(@Description, 2);

        if (Description.Width = 0) or (Description.Height = 0) then Exit;

        Width := Description.Width;
        Height := Description.Height;

        if Description.Compression = 0 then Compression := ctLZ77
                                       else Compression := ctUnknown;

        BitsPerSample := Description.BitDepth;
        SamplesPerPixel := 1;
        case Description.ColorType of
          0:
            ColorScheme := csG;
          2:
            begin
              ColorScheme := csRGB;
              SamplesPerPixel := 3;
            end;
          3:
            ColorScheme := csIndexed;
          4:
            ColorScheme := csGA;
          6:
            begin
              ColorScheme := csRGBA;
              SamplesPerPixel := 4;
            end;
        else
          ColorScheme := csUnknown;
        end;

        BitsPerPixel := SamplesPerPixel * BitsPerSample;
        FilterMode := Description.Filter;
        Interlaced := Description.Interlaced <> 0;
        HasAlpha := ColorScheme in [csGA, csRGBA, csBGRA];

        // find gamma 
        repeat
          FCurrentCRC := LoadAndSwapHeader;
          if IsChunk(gAMA) then
          begin
            ReadDataAndCheckCRC;
            // the file gamme given here is a scaled cardinal (e.g. 0.45 is expressed as 45000)
            FileGamma := SwapLong(PCardinal(FRawBuffer)^) / 100000;
            Break;
          end;

          Seek(FHeader.Length + 4, soFromCurrent);
          if IsChunk(IEND) then Break;
        until False;

        Result := True;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.LoadBackgroundColor(const Description);

// loads the data from the current chunk (must be a bKGD chunk) and fills the bitmpap with that color

var
  Run: PWord;
  R, G, B: Byte;

begin
  ReadDataAndCheckCRC;
  with TIHDRChunk(Description) do
  begin
    case ColorType of
      0, 4: // G(A)
        begin
          case BitDepth of
            2:
              FBackgroundColor := MulDiv16(Swap(PWord(FRawBuffer)^), 15, 3);
            16:
              FBackgroundColor := MulDiv16(Swap(PWord(FRawBuffer)^), 255, 65535);
          else // 1, 4, 8 bits gray scale
            FBackgroundColor := Byte(Swap(PWord(FRawBuffer)^));
          end;
        end;
      2, 6:  // RGB(A)
        begin
          Run := FRawBuffer;
          if BitDepth = 16 then
          begin
            R := MulDiv16(Swap(Run^), 255, 65535); Inc(Run);
            G := MulDiv16(Swap(Run^), 255, 65535); Inc(Run);
            B := MulDiv16(Swap(Run^), 255, 65535); 
          end
          else
          begin
            R := Byte(Swap(Run^)); Inc(Run);
            G := Byte(Swap(Run^)); Inc(Run);
            B := Byte(Swap(Run^));
          end;
          FBackgroundColor := RGB(R, G, B);
        end;
    else // indexed color scheme (3)
      FBackgroundColor := PByte(FRawBuffer)^;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.LoadIDAT(const Description);

// loads image data from the current position of the stream

const
  // interlace start and offsets
  RowStart: array[0..6] of Integer = (0, 0, 4, 0, 2, 0, 1);
  ColumnStart: array[0..6] of Integer = (0, 4, 0, 2, 0, 1, 0);
  RowIncrement: array[0..6] of Integer = (8, 8, 8, 4, 4, 2, 2);
  ColumnIncrement: array[0..6] of Integer = (8, 8, 4, 4, 2, 2, 1);
  PassMask: array[0..6] of Byte = ($80, $08, $88, $22, $AA, $55, $FF);

var
  Row: Integer;
  TargetBPP: Integer;
  RowBuffer: array[Boolean] of PChar; // I use PChar here instead of simple pointer to ease pointer math below
  EvenRow: Boolean; // distincts between the two rows we need to hold for filtering
  Pass: Integer;
  BytesPerRow,
  InterlaceRowBytes,
  InterlaceWidth: Integer;

begin
  Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
  RowBuffer[False] := nil;
  RowBuffer[True] := nil;
  try
    // adjust pixel format etc. if not yet done
    if PixelFormat = pfDevice then
      FSourceBPP := SetupColorDepth(TIHDRChunk(Description).ColorType, TIHDRChunk(Description).BitDepth);

    if TIHDRChunk(Description).BitDepth = 16 then TargetBPP := FSourceBPP div 2
                                             else TargetBPP := FSourceBPP;

    if FPalette <> 0 then Palette := FPalette;
    // after setting the pixel format we can set the dimensions too without
    // initiating color conversions
    Width := TIHDRChunk(Description).Width;
    Height := TIHDRChunk(Description).Height;

    // set background and transparency color, these values must be set after the
    // bitmap is actually valid (although, not filled)
    Canvas.Lock;
    try
      Canvas.Brush.Color := FBackgroundColor;
      Canvas.FillRect(Rect(0, 0, Width, Height));
    finally
      Canvas.Unlock;
    end;
    if FTransparentColor <> clNone then
    begin
      TransparentColor := FTransparentColor;
      Transparent := True;
    end;

    // determine maximum number of bytes per row and consider there's one filter byte at the start of each row
    BytesPerRow := TargetBPP * ((Width * TIHDRChunk(Description).BitDepth + 7) div 8) + 1;

    RowBuffer[True] := AllocMem(BytesPerRow);
    RowBuffer[False] := AllocMem(BytesPerRow);

    // there can be more than one IDAT chunk in the file but then they must directly
    // follow each other (handled in ReadRow)
    EvenRow := True;

    // prepare interlaced images
    if TIHDRChunk(Description).Interlaced = 1 then
    begin
      for Pass := 0 to 6 do
      begin
        // prepare next interlace run
        if Width <= ColumnStart[Pass] then Continue;
        InterlaceWidth := (Width + ColumnIncrement[Pass] - 1 - ColumnStart[Pass]) div ColumnIncrement[Pass];
        InterlaceRowBytes := TargetBPP * ((InterlaceWidth * TIHDRChunk(Description).BitDepth + 7) div 8) + 1;

        Row := RowStart[Pass];
        while Row < Height do
        begin
          ReadRow(RowBuffer[EvenRow], InterlaceRowBytes);
          ApplyFilter(Byte(RowBuffer[EvenRow]^),
                      Pointer(RowBuffer[EvenRow] + 1),
                      Pointer(RowBuffer[not EvenRow] + 1),
                      Pointer(RowBuffer[EvenRow] + 1),
                      FSourceBPP,
                      InterlaceRowBytes - 1);

          ColorManager.ConvertRow([Pointer(RowBuffer[EvenRow] + 1)], ScanLine[Row], Width, PassMask[Pass]);
          EvenRow := not EvenRow;
          // continue with next row in interlaced order
          Inc(Row, RowIncrement[Pass]);

          if Pass = 6 then
          begin
            // progress event only for last (and most expensive) pass
            Progress(Self, psRunning, MulDiv(Row, 100, Height), True, FProgressRect, '');
            OffsetRect(FProgressRect, 0, 1);
          end;
        end;
      end;
    end
    else
    begin
      for Row := 0 to Height - 1 do
      begin
        ReadRow(RowBuffer[EvenRow], BytesPerRow);
        ApplyFilter(Byte(RowBuffer[EvenRow]^),
                    Pointer(RowBuffer[EvenRow] + 1),
                    Pointer(RowBuffer[not EvenRow] + 1),
                    Pointer(RowBuffer[EvenRow] + 1),
                    FSourceBPP,
                    BytesPerRow - 1);

        ColorManager.ConvertRow([Pointer(RowBuffer[EvenRow] + 1)], ScanLine[Row], Width, $FF);
        EvenRow := not EvenRow;

        Progress(Self, psRunning, MulDiv(Row, 100, Height), True, FProgressRect, '');
        OffsetRect(FProgressRect, 0, 1);
      end;
    end;

    // in order to improve safe failness we read all remaining but not read IDAT chunks here
    while IsChunk(IDAT) do
    begin
      ReadDataAndCheckCRC;   
      FCurrentCRC := LoadAndSwapHeader;
    end;
  finally
    if Assigned(RowBuffer[True]) then FreeMem(RowBuffer[True]);
    if Assigned(RowBuffer[False]) then FreeMem(RowBuffer[False]);
  end;
  // ending progress event is issued in main method
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.LoadTransparency(const Description);

// reads the data of the current transparency chunk

var
  Run: PWord;
  R, G, B: Byte;
  
begin
  ReadDataAndCheckCRC;
  with TIHDRChunk(Description) do
  begin
    case ColorType of
      0: // gray
        begin
          case BitDepth of
            2:
              R := MulDiv16(Swap(PWord(FRawBuffer)^), 15, 3);
            16:
              R := MulDiv16(Swap(PWord(FRawBuffer)^), 255, 65535);
          else // 1, 4, 8 bits gray scale
            R := Byte(Swap(PWord(FRawBuffer)^));
          end;
          FTransparentColor := RGB(R, R, R);
        end;
      2:  // RGB
        begin
          Run := FRawBuffer;
          if BitDepth = 16 then
          begin
            R := MulDiv16(Swap(Run^), 255, 65535); Inc(Run);
            G := MulDiv16(Swap(Run^), 255, 65535); Inc(Run);
            B := MulDiv16(Swap(Run^), 255, 65535); 
          end
          else
          begin
            R := Byte(Swap(Run^)); Inc(Run);
            G := Byte(Swap(Run^)); Inc(Run);
            B := Byte(Swap(Run^));
          end;
          FTransparentColor := RGB(R, G, B);
        end;
      4, 6:
        // formats with full alpha channel, they shouldn't have a transparent color 
    else
      // Indexed color scheme (3), with at most 256 alpha values (for each palette entry).
      SetLength(FTransparency, 255);
      // read the values (at most 256)...
      Move(FRawBuffer^,  FTransparency[0], Max(FHeader.Length, 256));
      // ...and set default values (255, fully opaque) for non-supplied values
      if FHeader.Length < 256 then FillChar(FTransparency[FHeader.Length], 256 - FHeader.Length, $FF);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.ReadDataAndCheckCRC;

// Allocates memory in FRawBuffer and reads the next Header.Length bytes from Stream.
// Furthermore, the CRC value following the data is read as well and compared with
// the CRC value which is calculated here.

var
  FileCRC: Cardinal;

begin
  ReallocMem(FRawBuffer, FHeader.Length);
  FStream.ReadBuffer(FRawBuffer^, FHeader.Length);
  FStream.ReadBuffer(FileCRC, SizeOf(FileCRC));
  FileCRC := SwapLong(FileCRC);
  // The type field of a chunk is included in the CRC, this serves as initial value
  // for the calculation here and is determined in LoadAndSwapHeader.
  FCurrentCRC := CRC32(FCurrentCRC, FRawBuffer, FHeader.Length);
  if FCurrentCRC <> FileCRC then GraphicExError(gesInvalidCRC, ['PNG']);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.ReadRow(RowBuffer: Pointer; BytesPerRow: Integer);

// reads and decodes one scanline

var
  LocalBuffer: Pointer;
  PendingOutput: Integer;

begin
  LocalBuffer := RowBuffer;
  PendingOutput := BytesPerRow;
  repeat
    // read pending chunk data if available input has dropped to zero
    if FDecoder.AvailableInput = 0 then
    begin
      FIDATSize := 0;
      // read all following chunks until enough data is available or there is no further IDAT chunk
      while FIDATSize = 0 do
      begin
        // finish if the current chunk is not an IDAT chunk
        if not IsChunk(IDAT) then Exit;

        ReadDataAndCheckCRC;
        FCurrentSource := FRawBuffer;
        FIDATSize := FHeader.Length;

        // prepare next chunk (plus CRC)
        FCurrentCRC := LoadAndSwapHeader;
      end;
    end;

    // this decode call will advance Source and Target accordingly
    FDecoder.Decode(FCurrentSource,
                    LocalBuffer,
                    FIDATSize - (Integer(FCurrentSource) - Integer(FRawBuffer)),
                    PendingOutput);

    if FDecoder.ZLibResult = Z_STREAM_END then
    begin
       if (FDecoder.AvailableOutput <> 0) or
          (FDecoder.AvailableInput <> 0) then GraphicExError(gesExtraCompressedData, ['PNG']);
      Break;
    end;

    if FDecoder.ZLibResult <> Z_OK then GraphicExError(gesCompression, ['PNG']);

    PendingOutput := BytesPerRow - (Integer(LocalBuffer) - Integer(RowBuffer));
  until PendingOutput = 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPNGGraphic.SetupColorDepth(ColorType, BitDepth: Integer): Integer;

begin
  Result := 0;
  // determine color scheme and setup related stuff,
  // Note: The calculated BPP value is always at least 1 even for 1 bits per pixel etc. formats
  //       and used in filter calculation.
  case ColorType of
    0: // gray scale (allowed bit depths are: 1, 2, 4, 8, 16 bits)
      if BitDepth in [1, 2, 4, 8, 16] then
      with ColorManager do
      begin
        SourceColorScheme := csG;
        TargetColorScheme := csG;

        SourceSamplesPerPixel := 1;
        TargetSamplesPerPixel := 1;
        SourceBitsPerSample := BitDepth;
        // 2 bits values are converted to 4 bits values because DIBs don't know the former variant
        case BitDepth of
          2:
            TargetBitsPerSample := 4;
          16:
            TargetBitsPerSample := 8;
        else
          TargetBitsPerSample := BitDepth;
        end;

        PixelFormat := TargetPixelFormat;
        FPalette := CreateGrayscalePalette(False);
        Result := (BitDepth + 7) div 8;
      end
      else GraphicExError(gesInvalidColorFormat, ['PNG']);
    2: // RGB
      if BitDepth in [8, 16] then
      with ColorManager do
      begin
        SourceSamplesPerPixel := 3;
        TargetSamplesPerPixel := 3;
        SourceColorScheme := csRGB;
        TargetColorScheme := csBGR;
        SourceBitsPerSample := BitDepth;
        TargetBitsPerSample := 8;
        PixelFormat := pf24Bit;
        Result := BitDepth * 3 div 8;
      end
      else GraphicExError(gesInvalidColorFormat, ['PNG']);
    3: // palette
      if BitDepth in [1, 2, 4, 8] then
      with ColorManager do
      begin
        SourceColorScheme := csIndexed;
        TargetColorScheme := csIndexed;
        SourceSamplesPerPixel := 1;
        TargetSamplesPerPixel := 1;
        SourceBitsPerSample := BitDepth;
        // 2 bits values are converted to 4 bits values because DIBs don't know the former variant
        if BitDepth = 2 then TargetBitsPerSample := 4
                        else TargetBitsPerSample := BitDepth;

        PixelFormat := TargetPixelFormat;
        Result := 1;
      end
      else GraphicExError(gesInvalidColorFormat, ['PNG']);
    4: // gray scale with alpha,
       // For the moment this format is handled without alpha, but might later be converted
       // to RGBA with gray pixels or use a totally different approach.
      if BitDepth in [8, 16] then
      with ColorManager do
      begin
        SourceSamplesPerPixel := 1;
        TargetSamplesPerPixel := 1;
        SourceBitsPerSample := BitDepth;
        TargetBitsPerSample := 8;
        SourceColorScheme := csGA; 
        TargetColorScheme := csIndexed;
        PixelFormat := pf8Bit;
        FPalette := CreateGrayScalePalette(False);
        Result := 2 * BitDepth div 8;
      end
      else GraphicExError(gesInvalidColorFormat, ['PNG']);
    6: // RGB with alpha (8, 16)
      if BitDepth in [8, 16] then
      with ColorManager do
      begin
        SourceSamplesPerPixel := 4;
        TargetSamplesPerPixel := 4;
        SourceColorScheme := csRGBA;
        TargetColorScheme := csBGRA;
        SourceBitsPerSample := BitDepth;
        TargetBitsPerSample := 8;
        PixelFormat := pf32Bit;

        Result := BitDepth * 4 div 8;
      end
      else GraphicExError(gesInvalidColorFormat, ['PNG']);
  else
    GraphicExError(gesInvalidColorFormat, ['PNG']);
  end;
end;

{$endif} // PortableNetworkGraphic

//----------------- TFileFormatList ------------------------------------------------------------------------------------

type
  PClassEntry = ^TClassEntry;
  TClassEntry = record
    GraphicClass: TGraphicClass;
    Description: String;
    Count: Cardinal;
  end;

  PExtensionEntry = ^TExtensionEntry;
  TExtensionEntry = record
    Extension,
    Description: String;
    FormatTypes: TFormatTypes;
    ClassReference: PClassEntry;
  end;

constructor TFileFormatList.Create;

begin
  FClassList := TList.Create;
  FExtensionList := TList.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TFileFormatList.Destroy;

begin
  Clear;
  FClassList.Free;
  FExtensionList.Free;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFileFormatList.Clear;

var
  I: Integer;                         

begin
  for I := 0 to FClassList.Count - 1 do
  begin
    TPicture.UnregisterGraphicClass(PClassEntry(FClassList[I]).GraphicClass);
    Dispose(PClassEntry(FClassList[I])); // need Dispose with type casting to free strings too
  end;
  FClassList.Clear;

  for I := 0 to FExtensionList.Count - 1 do
    Dispose(PExtensionEntry(FExtensionList[I])); 
  FExtensionList.Clear;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.FindExtension(const Extension: String): Integer;

// Returns the entry which belongs to the given extension string or -1 if there's nothing in the list for this ext.

var
  I: Integer;

begin
  Result := -1;
  if Extension <> '' then
    for I := 0 to FExtensionList.Count - 1 do
      if CompareText(PExtensionEntry(FExtensionList[I]).Extension, Extension) = 0 then
      begin
        Result := I;
        Break;
      end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.FindGraphicClass(GraphicClass: TGraphicClass): Integer;

// returns the entry index which belongs to the given graphic class or -1

var
  I: Integer;

begin
  Result := -1;
  for I := 0 to FClassList.Count - 1 do
    if PClassEntry(FClassList[I]).GraphicClass = GraphicClass then
    begin
      Result := I;
      Break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.GetDescription(Graphic: TGraphicClass): String;

// returns the registered description string for the given class

var
  I: Integer;

begin
  Result := '';
  I := FindGraphicClass(Graphic);
  if I > -1 then Result := PClassEntry(FClassList[I]).Description;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFileFormatList.GetExtensionList(List: TStrings);

// returns a list of registered extensions (letters only, no *. part)

var
  I: Integer;
  ExtEntry: PExtensionEntry;

begin
  List.Clear;
  for I := 0 to FExtensionList.Count - 1 do
  begin
    ExtEntry := FExtensionList[I];
    List.Add(ExtEntry.Extension);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.GetGraphicFilter(Formats: TFormatTypes; SortType: TFilterSortType;
  Options: TFilterOptions; GraphicClass: TGraphicClass): String;

// Creates a string which can directly be used in an open or save dialog's filter property.
// Formats may be used to limit the number of formats to return.
// SortType determines how to sort the entries.
// Compact determines whether to group extensions (= True) or to put every extension on a separate line.
// AllImages finally determines whether to include the 'All image file' entry which includes all allowed extensions
// which qualify by the other properties.
// Usually all these options determine quite nicely which formats are well suited for a particular task
// but sometimes you may find it better to specify a graphic class to limit returned formats further.
// In this case set GraphicClass to the particular class otherwise set it nil.

var
  I, J: Integer;
  DL, EL, All: TStringList;
  ExtEntry: PExtensionEntry;
  ClassEntry: PClassEntry;
  S,
  DescriptionFormat: String;

begin
  Result := '';
  if Formats = [] then Formats := [ftAnimation..ftVector];
  DL := TStringList.Create;
  DL.Sorted := SortType in [fstDescription, fstBoth];
  EL := TStringList.Create;
  EL.Sorted := SortType in [fstExtension, fstBoth];

  // this string list is used to hold the (possibly sorted) list of all allowed extensions
  All := TStringList.Create;
  All.Sorted := SortType in [fstExtension, fstBoth];

  // using an adjusted format string makes the code below easier for different options
  DescriptionFormat := '%s';
  if foIncludeExtension in Options then DescriptionFormat := DescriptionFormat + '%s';

  if foCompact in Options then
  begin
    // all extension for a particular image class on one line
    for I := 0 to FClassList.Count - 1 do
    begin
      ClassEntry := FClassList[I];
      if (GraphicClass = nil) or (GraphicClass = ClassEntry.GraphicClass) then
      begin
        EL.Clear;
        // collect allowed extensions for the current graphic class,
        // this will automatically sort the entries if wanted
        for J := 0 to FExtensionList.Count - 1 do
        begin
          ExtEntry := FExtensionList[J];
          if (ExtEntry.ClassReference = ClassEntry) and ((ExtEntry.FormatTypes * Formats) <> []) then
            EL.Add(ExtEntry.Extension);
        end;

        // build the extension list and an description entry
        if foIncludeAll in Options then All.AddStrings(EL);
        S := '';
        for J := 0 to EL.Count - 1 do S := S + '*.' + EL[J] + '; ';
        // remove last semicolon and space
        SetLength(S, Length(S) - 2);
        if S <> '' then DL.AddObject(ClassEntry.Description, Pointer(StrNew(PChar(S))));
      end;
    end;
  end
  else
  begin
    // list each extension separately
    for I := 0 to FExtensionList.Count - 1 do
    begin
      ExtEntry := FExtensionList[I];
      if ((GraphicClass = nil) or (ExtEntry.ClassReference.GraphicClass = GraphicClass)) and
         ((ExtEntry.FormatTypes * Formats) <> []) then
      begin
        S := ExtEntry.Description;
        if S = '' then S := ExtEntry.ClassReference.Description;
        DL.AddObject(S, Pointer(StrNew(PChar('*.' + ExtEntry.Extension))));
        if foIncludeAll in Options then All.Add(ExtEntry.Extension);
      end;
    end;
  end;

  // build final filter string out of the collected sub strings
  if (foIncludeAll in Options) and (All.Count > 0) then
  begin
    // first include the general entry if wanted (this entry is never taken into sort order
    S := '';
    for J := 0 to All.Count - 1 do S := S + '*.' + All[J] + '; ';
    SetLength(S, Length(S) - 2);
    Result := gesAllImages + '|' + S + '|';
  end;

  for I := 0 to DL.Count - 1 do
  begin
    S := PChar(DL.Objects[I]);
    StrDispose(PChar(DL.Objects[I]));
    Result := Result + Format(DescriptionFormat, [DL[I], ' (' + S + ')']) + '|' + S + '|';
  end;
  // remove last separator in string
  if Length(Result) > 0 then SetLength(Result, Length(Result) - 1);
  All.Free;
  EL.Free;
  DL.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.GraphicFromExtension(S: String): TGraphicClass;

// Returns the class which belongs to the extension given in S or nil if there's non registered.
// S may contain a regular file name (also UNC is allowed), a string returned from ExtractFileExt (with period) or just
// an extension string.

var
  Index: Integer;

begin
  Result := nil;
  Index := Pos('.', S);
  if Index > 0 then Delete(S, 1, Index);
  Index := FindExtension(S);
  if Index > -1 then Result := PExtensionEntry(FExtensionList[Index]).ClassReference.GraphicClass;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.GraphicFromContent(const FileName: String): TGraphicExGraphicClass;

// description see other overloaded version

var
  Stream: TFileStream;

begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := GraphicFromContent(Stream);
  finally
    Stream.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.GraphicFromContent(Stream: TStream): TGraphicExGraphicClass;

// Determines the type of image in the stream. This test is only available for TGraphicExGraphic
// classes (this excludes TBitmap, TIcon, TMetaFile etc.).
// Note: Not all image types can be found using this code because they are not
//       uniquely identifyable (e.g. Dr. Halo *.cut images).

var
  I: Integer;
  T: TGraphicExGraphicClass;

begin
  Result := nil;
  for I := 0 to FClassList.Count - 1 do
  begin
    if PClassEntry(FClassList[I]).GraphicClass.InheritsFrom(TGraphicExGraphic) then
    begin
      T := TGraphicExGraphicClass(PClassEntry(FClassList[I]).GraphicClass);
      if T.CanLoad(Stream) then
      begin
        Result := T;
        Break;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFileFormatList.RegisterFileFormat(const Extension, Common, Individual: String; FormatTypes: TFormatTypes;
  Replace, RegisterDefault: Boolean; GraphicClass: TGraphicClass);

// Registers the given graphic class with the passed extension string. If there's already a class registered with this
// extension then either the registration of the older entry is replaced by the new one (Replace = True) or an exception
// is raised.
// This method takes also care to register the new extension with TPicture to make the default handling work too
// if RegisterDefault is True.
// Further parameters are:
// - Extension: the new extension to be registered (not necessarily with only 3 characters, but without a period).
// - Common: a description string for all extensions registered with the same class used when several extensions are
//   listed on one filter line. Pass '' to avoid changing a previously set value if there's one.
// - Individual: a description string used when each extension is listed separately.
// - FormatTypes: classifies the given file type as being a raster or vector file, with single or multiple images etc.
// - GraphicClass: the TGraphic descentant to be used to load and save the particular file.

var
  ExtIndex,
  ClassIndex: Integer;
  ExtEntry: PExtensionEntry;
  ClassEntry,
  OldReference: PClassEntry;

  //--------------- local functions -------------------------------------------

  procedure UpdateClassEntry;

  // updates a class entry (creates one if necessary)

  begin
    if ClassIndex = -1 then
    begin
      New(ClassEntry);
      ClassEntry.GraphicClass := GraphicClass;
      ClassEntry.Count := 0;
      FClassList.Add(ClassEntry);
    end
    else
      ClassEntry := FClassList[ClassIndex];

    if Common <> '' then ClassEntry.Description := Common;
    Inc(ClassEntry.Count);
    ExtEntry.ClassReference := ClassEntry;
  end;

  //--------------- end local functions ---------------------------------------

var
  S: String;

begin
  if Extension <> '' then
  begin
    ExtIndex := FindExtension(Extension);
    ClassIndex := FindGraphicClass(GraphicClass);
    if ExtIndex = -1 then
    begin
      // extension not yet registered
      New(ExtEntry);
      ExtEntry.Extension := Extension;
      ExtEntry.Description := Individual;
      ExtEntry.FormatTypes := FormatTypes;
      FExtensionList.Add(ExtEntry);
      UpdateClassEntry;
    end
    else
      if Replace then
      begin
        // replace current extension entry with new one
        ExtEntry := FExtensionList[ExtIndex];
        if ExtEntry.ClassReference.GraphicClass <> GraphicClass then
        begin
          // assign existing extension to new graphic class
          OldReference := ExtEntry.ClassReference;
          UpdateClassEntry;
          Dec(OldReference.Count);
          // remove the graphic class entry if no longer used
          if OldReference.Count = 0 then FClassList.Remove(OldReference);
        end;
          // otherwise do nothing
      end
      else
        GraphicExError(gesRegistration, [Extension]);

    // finally make TPicture work
    S := Individual;
    if S = '' then S := ClassEntry.Description;
    TPicture.RegisterFileFormat(Extension, S, GraphicClass);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFileFormatList.UnregisterFileFormat(const Extension: String; GraphicClass: TGraphicClass);

// Removes the entry for the given extension from the internal list.
// If Extension is '' then all associations for the given GraphicClass are removed otherwise the class is ignored and
// only the one particular extension is removed.
// Unregistration from TPicture is done here too, if necessary.

var
  ExtIndex,
  ClassIndex: Integer;
  ExtEntry: PExtensionEntry;
  ClassEntry: PClassEntry;

begin
  ExtIndex := FindExtension(Extension);
  // make sure we don't try to remove a non-registered extension
  if (Extension = '') or (ExtIndex > -1) then
  begin
    if ExtIndex > -1 then
    begin
      // there's an entry for the extension
      ExtEntry := FExtensionList[ExtIndex];
      Dec(ExtEntry.ClassReference.Count);
      // unregister graphic class too if necessary
      if ExtEntry.ClassReference.Count = 0 then
      begin
        TPicture.UnregisterGraphicClass(ExtEntry.ClassReference.GraphicClass);
        Dispose(ExtEntry.ClassReference);
        FClassList.Remove(ExtEntry.ClassReference);
      end;

      // finally delete extension entry
      Dispose(ExtEntry);
      FExtensionList.Delete(ExtIndex);
    end
    else
    begin
      // all entries for the given graphic class must be removed
      ClassIndex := FindGraphicClass(GraphicClass);
      ClassEntry := FClassList[ClassIndex];
      for ExtIndex := FExtensionList.Count - 1 downto 0 do
      begin
        if PExtensionEntry(FExtensionList[ExtIndex]).ClassReference.GraphicClass = GraphicClass then
        begin
          Dec(ClassEntry.Count);
          Dispose(PExtensionEntry(FExtensionList[ExtIndex]));
          FExtensionList.Delete(ExtIndex);
          // no need to run through further entries if all references are done
          if ClassEntry.Count = 0 then Break;
        end;
      end;
      Dispose(ClassEntry);
      FClassList.Delete(ClassIndex);
      TPicture.UnregisterGraphicClass(GraphicClass);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  FileFormatList := TFileFormatList.Create;
  with FileFormatList do
  begin
    // internally register Delphi's "built in" formats, these will not be unregistered on exit and
    // also not registered with TPicture (because they are already or will soon be)
    RegisterFileFormat('bmp', gesBitmaps, '', [ftRaster], False, False, TBitmap);
    RegisterFileFormat('ico', gesIcons, '', [ftRaster], False, False, TIcon);
    RegisterFileFormat('wmf', gesMetaFiles, '', [ftVector], False, False, TMetafile);
    RegisterFileFormat('emf', gesMetaFiles, gesEnhancedMetaFiles, [ftVector], False, False, TMetafile);
    RegisterFileFormat('jfif', gesJPGImages, gesJFIFImages, [ftRaster], False, False, TJPEGImage);
    RegisterFileFormat('jpg', '', gesJPGImages, [ftRaster], False, False, TJPEGImage);
    RegisterFileFormat('jpe', '', gesJPEImages, [ftRaster], False, False, TJPEGImage);
    RegisterFileFormat('jpeg', '', gesJPEGImages, [ftRaster], False, False, TJPEGImage);

    // register our own formats
    RegisterFileFormat('rle', gesBitmaps, gesRLEBitmaps, [ftRaster], False, True, TBitmap);
    RegisterFileFormat('dib', '', gesDIBs, [ftRaster], False, True, TBitmap);

    {$ifdef TargaGraphic}
    RegisterFileFormat('win', gesTruevision, '', [ftRaster], False, True, TTargaGraphic);
    RegisterFileFormat('vst', '', '', [ftRaster], False, True, TTargaGraphic);
    RegisterFileFormat('vda', '', '', [ftRaster], False, True, TTargaGraphic);
    RegisterFileFormat('tga', '', '', [ftRaster], False, True, TTargaGraphic);
    RegisterFileFormat('icb', '', '', [ftRaster], False, True, TTargaGraphic);
    {$endif}

    {$ifdef TIFFGraphic}
    RegisterFileFormat('tiff', gesTIFF, gesMacTIFF, [ftRaster, ftMultiImage], False,
      True, TTIFFGraphic);
    RegisterFileFormat('tif', '', gesPCTIF, [ftRaster, ftMultiImage], False, True, TTIFFGraphic);
    RegisterFileFormat('fax', '', gesGFIFax, [ftRaster, ftMultiImage], False, True, TTIFFGraphic);
      {$ifdef EPSGraphic}
      RegisterFileFormat('eps', gesEPS, '', [ftRaster], False, True, TEPSGraphic);
      {$endif}
    {$endif}

    {$ifdef PCXGraphic}
    RegisterFileFormat('pcx', gesZSoft, '', [ftRaster], False, True, TPCXGraphic);
    RegisterFileFormat('pcc', '', '', [ftRaster], False, True, TPCXGraphic);
    RegisterFileFormat('scr', '', gesZSoftWord, [ftRaster], False, True, TPCXGraphic);
    {$endif}

    {$ifdef RLAGraphic}
    RegisterFileFormat('rpf', gesAliasWaveFront, '', [ftRaster], False, True, TRLAGraphic);
    RegisterFileFormat('rla', '', '', [ftRaster], False, True, TRLAGraphic);
    {$endif}

    {$ifdef SGIGraphic}
    RegisterFileFormat('sgi', gesSGI, gesSGITrueColor, [ftRaster], False, True, TSGIGraphic);
    RegisterFileFormat('rgba', '', gesSGITrueColorAlpha, [ftRaster], False, True, TSGIGraphic);
    RegisterFileFormat('rgb', '', gesSGITrueColor, [ftRaster], False, True, TSGIGraphic);
    RegisterFileFormat('bw', '', gesSGIMono, [ftRaster], False, True, TSGIGraphic);
    {$endif}

    {$ifdef PhotoshopGraphic}
    RegisterFileFormat('psd', gesPhotoshop, '', [ftRaster, ftLayered], False, True, TPSDGraphic);
    RegisterFileFormat('pdd', '', '', [ftRaster, ftLayered], False, True, TPSDGraphic);
    {$endif}

    {$ifdef PortableMapGraphic}
    RegisterFileFormat('ppm', gesPortable, gesPortablePixel, [ftRaster], False, True, TPPMGraphic);
    RegisterFileFormat('pgm', '', gesPortableGray, [ftRaster], False, True, TPPMGraphic);
    RegisterFileFormat('pbm', '', gesPortableMono, [ftRaster], False, True, TPPMGraphic);
    {$endif}

    {$ifdef AutodeskGraphic}
    RegisterFileFormat('cel', gesAutodesk, '', [ftRaster], False, True, TAutodeskGraphic);
    RegisterFileFormat('pic', gesAutodesk, '', [ftRaster], False, True, TAutodeskGraphic);
    {$endif}

    {$ifdef PCDGraphic}
    RegisterFileFormat('pcd', gesKodakPhotoCD, '', [ftRaster], False, True, TPCDGraphic);
    {$endif}

    {$ifdef GIFGraphic}
    RegisterFileFormat('gif', gesCompuserve, '', [ftRaster, ftMultiImage, ftAnimation], False, True, TGIFGraphic);
    {$endif}

    {$ifdef CUTGraphic}
    RegisterFileFormat('cut', gesHalo, '', [ftRaster], False, True, TCUTGraphic);
    {$endif}

    {$ifdef PaintshopProGraphic}
    RegisterFileFormat('psp', gesPaintshopPro, '', [ftRaster, ftVector], False, True, TPSPGraphic);
    {$endif}

    {$ifdef PortableNetworkGraphic}
    RegisterFileFormat('png', gesPortableNetworkGraphic, '', [ftRaster], False, True, TPNGGraphic);
    {$endif}
  end;
finalization
  with FileFormatList do
  begin
    {$ifdef PaintshopProGraphic} UnregisterFileFormat('', TPSPGraphic); {$endif}
    {$ifdef PhotoshopGraphic} UnregisterFileFormat('', TPSDGraphic); {$endif}
    {$ifdef TargaGraphic} UnregisterFileFormat('', TTargaGraphic); {$endif}
    {$ifdef TIFFGraphic} UnregisterFileFormat('', TTIFFGraphic); {$endif}
    {$ifdef SGIGraphic} UnregisterFileFormat('', TSGIGraphic); {$endif}
    {$ifdef PCXGraphic} UnregisterFileFormat('', TPCXGraphic); {$endif}
    {$ifdef AutodeskGraphic} UnregisterFileFormat('', TAutodeskGraphic); {$endif}
    {$ifdef PCDGraphic} UnregisterFileFormat('', TPCDGraphic); {$endif}
    {$ifdef PortableMapGraphic} UnregisterFileFormat('', TPPMGraphic); {$endif}
    {$ifdef CUTGraphic} UnregisterFileFormat('', TCUTGraphic); {$endif}
    {$ifdef GIFGraphic} UnregisterFileFormat('', TGIFGraphic); {$endif}
    {$ifdef RLAGraphic} UnregisterFileFormat('', TRLAGraphic); {$endif}
    UnregisterFileFormat('rle', TBitmap);
    UnregisterFileFormat('dib', TBitmap);
    {$ifdef PortableNetworkGraphic} UnregisterFileFormat('', TPNGGraphic); {$endif}

    Free;
  end;
end.

