{Portable Network Graphics Delphi 1.4361   (8 March 2003)     }

{This is the latest implementation for TPngImage component    }
{It's meant to be a full replacement for the previous one.    }
{There are lots of new improvements, including cleaner code,  }
{full partial transparency support, speed improvements,       }
{saving using ADAM 7 interlacing, better error handling, also }
{the best compression for the final image ever. And now it's  }
{truly able to read about any png image.                      }

{
  Version 1.4361
  2003-03-04 - Fixed important bug for simple transparency when using
               RGB, Grayscale color modes

  Version 1.436
  2003-03-04 - * NEW * Property Pixels for direct access to pixels
               * IMPROVED * Palette property (TPngObject) (read only)
               Slovenian traslation for the component (Miha Petelin)
               Help file update (scanline article/png->jpg example)

  Version 1.435
  2003-11-03 - * NEW * New chunk implementation zTXt (method AddzTXt)
               * NEW * New compiler flags to store the extra 8 bits
               from 16 bits samples (when saving it is ignored), the
               extra data may be acessed using ExtraScanline property
               * Fixed * a bug on tIMe chunk
               French translation included (Thanks to IBE Software)
               Bugs fixed

  Version 1.432
  2002-08-24 - * NEW *  A new method, CreateAlpha will transform the
               current image into partial transparency.
               Help file updated with a new article on how to handle
               partial transparency.

  Version 1.431
  2002-08-14 - Fixed and tested to work on:
               C++ Builder 3
               C++ Builder 5
               Delphi 3
               There was an error when setting TransparentColor, fixed
               New method, RemoveTransparency to remove image
               BIT TRANSPARENCY

  Version 1.43
  2002-08-01 - * NEW * Support for Delphi 3 and C++ Builder 3
               Implements mostly some things that were missing,
               a few tweaks and fixes.

  Version 1.428
  2002-07-24 - More minor fixes (thanks to Ian Boyd)
               Bit transparency fixes
               * NEW * Finally support to bit transparency
               (palette / rgb / grayscale -> all)

  Version 1.427
  2002-07-19 - Lots of bugs and leaks fixed
               * NEW * method to easy adding text comments, AddtEXt
               * NEW * property for setting bit transparency,
                       TransparentColor

  Version 1.426
  2002-07-18 - Clipboard finally fixed (hope)
               Changed UseDelphi trigger to UseDelphi
               * NEW * Support for bit transparency bitmaps
                       when assigning from/to TBitmap objects
               Altough it does not support drawing transparent
               parts of bit transparency pngs (only partial)
               it is closer than ever

  Version 1.425
  2002-07-01 - Clipboard methods implemented
               Lots of bugs fixed

  Version 1.424
  2002-05-16 - Scanline and AlphaScanline are now working correctly.
               New methods for handling the clipboard

  Version 1.423
  2002-05-16 - * NEW * Partial transparency for 1, 2, 4 and 8 bits is
               also supported using the tRNS chunk (for palette and
               grayscaling).
               New bug fixes (Peter Haas).

  Version 1.422
  2002-05-14 - Fixed some critical leaks, thanks to Peter Haas tips.
               New translation for German (Peter Haas).

  Version 1.421
  2002-05-06 - Now uses new ZLIB version, 1.1.4 with some security
               fixes.
               LoadFromResourceID and LoadFromResourceName added and
               help file updated for that.
               The resources strings are now located in pnglang.pas.
               New translation for Brazilian Portuguese.
               Bugs fixed.

 IMPORTANT: I'm currently looking for bugs on the library. If
            anyone has found one, please send me an email and
            I will fix right away. Thanks for all the help and
            ideias I'm receiving so far.}

{My new email is: gubadaud@terra.com.br}
{Website link   : pngdelphi.sourceforge.net}
{Gustavo Huffenbacher Daud}

unit pngimage;

interface

{Triggers avaliable (edit the fields bellow)}
{$DEFINE UseDelphi}              //Disable fat vcl units (perfect to small apps)
{$DEFINE ErrorOnUnknownCritical} //Error when finds an unknown critical chunk
{$DEFINE CheckCRC}               //Enables CRC checking
{$DEFINE RegisterGraphic}        //Registers TPNGObject to use with TPicture
{$DEFINE PartialTransparentDraw} //Draws partial transparent images
{.$DEFINE Store16bits}            //Stores the extra 8 bits from 16bits/sample
{.$DEFINE Debug}                 //For programming purposes
{$RANGECHECKS OFF} {$J+}



uses
 Windows {$IFDEF UseDelphi}, Classes, Graphics, SysUtils{$ENDIF} {$IFDEF Debug},
 dialogs{$ENDIF}, pngzlib, pnglang;

{$IFNDEF UseDelphi}
  const
    soFromBeginning = 0;
    soFromCurrent = 1;
    soFromEnd = 2;
{$ENDIF}

const
  {ZLIB constants}
  ZLIBErrors: Array[-6..2] of string = ('incompatible version (-6)',
    'buffer error (-5)', 'insufficient memory (-4)', 'data error (-3)',
    'stream error (-2)', 'file error (-1)', '(0)', 'stream end (1)',
    'need dictionary (2)');
  Z_NO_FLUSH      = 0;
  Z_FINISH        = 4;
  Z_STREAM_END    = 1;

  {Avaliable PNG filters for mode 0}
  FILTER_NONE    = 0;
  FILTER_SUB     = 1;
  FILTER_UP      = 2;
  FILTER_AVERAGE = 3;
  FILTER_PAETH   = 4;

  {Avaliable color modes for PNG}
  COLOR_GRAYSCALE      = 0;
  COLOR_RGB            = 2;
  COLOR_PALETTE        = 3;
  COLOR_GRAYSCALEALPHA = 4;
  COLOR_RGBALPHA       = 6;


type
  {$IFNDEF UseDelphi}
    {Custom exception handler}
    Exception = class(TObject)
      constructor Create(Msg: String);
    end;
    ExceptClass = class of Exception;
    TColor = ColorRef;
  {$ENDIF}

  {Error types}
  EPNGOutMemory = class(Exception);
  EPngError = class(Exception);
  EPngUnexpectedEnd = class(Exception);
  EPngInvalidCRC = class(Exception);
  EPngInvalidIHDR = class(Exception);
  EPNGMissingMultipleIDAT = class(Exception);
  EPNGZLIBError = class(Exception);
  EPNGInvalidPalette = class(Exception);
  EPNGInvalidFileHeader = class(Exception);
  EPNGIHDRNotFirst = class(Exception);
  EPNGNotExists = class(Exception);
  EPNGSizeExceeds = class(Exception);
  EPNGMissingPalette = class(Exception);
  EPNGUnknownCriticalChunk = class(Exception);
  EPNGUnknownCompression = class(Exception);
  EPNGUnknownInterlace = class(Exception);
  EPNGNoImageData = class(Exception);
  EPNGCouldNotLoadResource = class(Exception);
  EPNGCannotChangeTransparent = class(Exception);
  EPNGHeaderNotPresent = class(Exception);

type
  {Direct access to pixels using R,G,B}
  TRGBLine = array[word] of TRGBTriple;
  pRGBLine = ^TRGBLine;

  {Same as TBitmapInfo but with allocated space for}
  {palette entries}
  TMAXBITMAPINFO = packed record
    bmiHeader: TBitmapInfoHeader;
    bmiColors: packed array[0..255] of TRGBQuad;
  end;

  {Transparency mode for pngs}
  TPNGTransparencyMode = (ptmNone, ptmBit, ptmPartial);
  {Pointer to a cardinal type}
  pCardinal = ^Cardinal;
  {Access to a rgb pixel}
  pRGBPixel = ^TRGBPixel;
  TRGBPixel = packed record
    B, G, R: Byte;
  end;

  {Pointer to an array of bytes type}
  TByteArray = Array[Word] of Byte;
  pByteArray = ^TByteArray;

  {Forward}
  TPNGObject = class;
  pPointerArray = ^TPointerArray;
  TPointerArray = Array[Word] of Pointer;

  {Contains a list of objects}
  TPNGPointerList = class
  private
    fOwner: TPNGObject;
    fCount : Cardinal;
    fMemory: pPointerArray;
    function GetItem(Index: Cardinal): Pointer;
    procedure SetItem(Index: Cardinal; const Value: Pointer);
  protected
    {Removes an item}
    function Remove(Value: Pointer): Pointer; virtual;
    {Inserts an item}
    procedure Insert(Value: Pointer; Position: Cardinal);
    {Add a new item}
    procedure Add(Value: Pointer);
    {Returns an item}
    property Item[Index: Cardinal]: Pointer read GetItem write SetItem;
    {Set the size of the list}
    procedure SetSize(const Size: Cardinal);
    {Returns owner}
    property Owner: TPNGObject read fOwner;
  public
    {Returns number of items}
    property Count: Cardinal read fCount write SetSize;
    {Object being either created or destroyed}
    constructor Create(AOwner: TPNGObject);
    destructor Destroy; override;
  end;

  {Forward declaration}
  TChunk = class;
  TChunkClass = class of TChunk;

  {Same as TPNGPointerList but providing typecasted values}
  TPNGList = class(TPNGPointerList)
  private
    {Used with property Item}
    function GetItem(Index: Cardinal): TChunk;
  public
    {Removes an item}
    procedure RemoveChunk(Chunk: TChunk); overload;
    {Add a new chunk using the class from the parameter}
    function Add(ChunkClass: TChunkClass): TChunk;
    {Returns pointer to the first chunk of class}
    function ItemFromClass(ChunkClass: TChunkClass): TChunk;
    {Returns a chunk item from the list}
    property Item[Index: Cardinal]: TChunk read GetItem;
  end;

  {$IFNDEF UseDelphi}
    {The STREAMs bellow are only needed in case delphi provided ones is not}
    {avaliable (UseDelphi trigger not set)}
    {Object becomes handles}
    TCanvas = THandle;
    TBitmap = HBitmap;
    {Trick to work}
    TPersistent = TObject;

    {Base class for all streams}
    TStream = class
    protected
      {Returning/setting size}
      function GetSize: Longint; virtual;
      procedure SetSize(const Value: Longint); virtual; abstract;
      {Returns/set position}
      function GetPosition: Longint; virtual;
      procedure SetPosition(const Value: Longint); virtual;
    public
      {Returns/sets current position}
      property Position: Longint read GetPosition write SetPosition;
      {Property returns/sets size}
      property Size: Longint read GetSize write SetSize;
      {Allows reading/writing data}
      function Read(var Buffer; Count: Longint): Cardinal; virtual; abstract;
      function Write(const Buffer; Count: Longint): Cardinal; virtual; abstract;
      {Copies from another Stream}
      function CopyFrom(Source: TStream;
        Count: Cardinal): Cardinal; virtual;
      {Seeks a stream position}
      function Seek(Offset: Longint; Origin: Word): Longint; virtual; abstract;
    end;

    {File stream modes}
    TFileStreamMode = (fsmRead, fsmWrite, fsmCreate);
    TFileStreamModeSet = set of TFileStreamMode;

    {File stream for reading from files}
    TFileStream = class(TStream)
    private
      {Opened mode}
      Filemode: TFileStreamModeSet;
      {Handle}
      fHandle: THandle;
    protected
      {Set the size of the file}
      procedure SetSize(const Value: Longint); override;
    public
      {Seeks a file position}
      function Seek(Offset: Longint; Origin: Word): Longint; override;
      {Reads/writes data from/to the file}
      function Read(var Buffer; Count: Longint): Cardinal; override;
      function Write(const Buffer; Count: Longint): Cardinal; override;
      {Stream being created and destroy}
      constructor Create(Filename: String; Mode: TFileStreamModeSet);
      destructor Destroy; override;
    end;

    {Stream for reading from resources}
    TResourceStream = class(TStream)
      constructor Create(Instance: HInst; const ResName: String; ResType:PChar);
    private
      {Variables for reading}
      Size: Integer;
      Memory: Pointer;
      Position: Integer;
    protected
      {Set the size of the file}
      procedure SetSize(const Value: Longint); override;
    public
      {Stream processing}
      function Read(var Buffer; Count: Integer): Cardinal; override;
      function Seek(Offset: Integer; Origin: Word): Longint; override;
      function Write(const Buffer; Count: Longint): Cardinal; override;
    end;
  {$ENDIF}

  {Forward}
  TChunkIHDR = class;
  {Interlace method}
  TInterlaceMethod = (imNone, imAdam7);
  {Compression level type}
  TCompressionLevel = 0..9;
  {Filters type}
  TFilter = (pfNone, pfSub, pfUp, pfAverage, pfPaeth);
  TFilters = set of TFilter;

  {Png implementation object}
  TPngObject = class{$IFDEF UseDelphi}(TGraphic){$ENDIF}
  protected
    {Gamma table values}
    GammaTable, InverseGamma: Array[Byte] of Byte;
    procedure InitializeGamma;
  private
    {Temporary palette}
    TempPalette: HPalette;
    {Filters to test to encode}
    fFilters: TFilters;
    {Compression level for ZLIB}
    fCompressionLevel: TCompressionLevel;
    {Maximum size for IDAT chunks}
    fMaxIdatSize: Cardinal;
    {Returns if image is interlaced}
    fInterlaceMethod: TInterlaceMethod;
    {Chunks object}
    fChunkList: TPngList;
    {Clear all chunks in the list}
    procedure ClearChunks;
    {Returns if header is present}
    function HeaderPresent: Boolean;
    {Returns linesize and byte offset for pixels}
    procedure GetPixelInfo(var LineSize, Offset: Cardinal);
    procedure SetMaxIdatSize(const Value: Cardinal);
    function GetAlphaScanline(const LineIndex: Integer): pByteArray;
    function GetScanline(const LineIndex: Integer): Pointer;
    {$IFDEF Store16bits}
    function GetExtraScanline(const LineIndex: Integer): Pointer;
    {$ENDIF}
    function GetTransparencyMode: TPNGTransparencyMode;
    function GetTransparentColor: TColor;
    procedure SetTransparentColor(const Value: TColor);
  protected
    {Returns the image palette}
    function GetPalette: HPALETTE; {$IFDEF UseDelphi}override;{$ENDIF}
    {THANY: Sets a new palette}
    procedure SetPalette(Value: HPALETTE); {$IFDEF UseDelphi}override;{$ENDIF}
    {Returns/sets image width and height}
    function GetWidth: Integer; {$IFDEF UseDelphi}override;{$ENDIF}
    function GetHeight: Integer; {$IFDEF UseDelphi}override; {$ENDIF}
    procedure SetWidth(Value: Integer);  {$IFDEF UseDelphi}override; {$ENDIF}
    procedure SetHeight(Value: Integer);  {$IFDEF UseDelphi}override;{$ENDIF}
    {Assigns from another TPNGObject}
    procedure AssignPNG(Source: TPNGObject);
    {Returns if the image is empty}
    function GetEmpty: Boolean; {$IFDEF UseDelphi}override; {$ENDIF}
    {Used with property Header}
    function GetHeader: TChunkIHDR;
    {Draws using partial transparency}
    procedure DrawPartialTrans(DC: HDC; Rect: TRect);
    {$IFDEF UseDelphi}
    {Returns if the image is transparent}
    function GetTransparent: Boolean; override;
    {$ENDIF}
    {Returns a pixel}
    function GetPixels(const X, Y: Integer): TColor; virtual;
    procedure SetPixels(const X, Y: Integer; const Value: TColor); virtual;
  public
    {Generates alpha information}
    procedure CreateAlpha;
    {Removes the image transparency}
    procedure RemoveTransparency;
    {Transparent color}
    property TransparentColor: TColor read GetTransparentColor write
      SetTransparentColor;
    {Add text chunk, TChunkTEXT, TChunkzTXT}
    procedure AddtEXt(const Keyword, Text: String);
    procedure AddzTXt(const Keyword, Text: String);
    {$IFDEF UseDelphi}
    {Saves to clipboard format (thanks to Antoine Pottern)}
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPalette); override;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPalette); override;
    {$ENDIF}
    {Calling errors}
    procedure RaiseError(ExceptionClass: ExceptClass; Text: String);
    {Returns a scanline from png}
    property Scanline[const Index: Integer]: Pointer read GetScanline;
    {$IFDEF Store16bits}
    property ExtraScanline[const Index: Integer]: Pointer read GetExtraScanline;
    {$ENDIF}
    property AlphaScanline[const Index: Integer]: pByteArray read GetAlphaScanline;
    {Returns pointer to the header}
    property Header: TChunkIHDR read GetHeader;
    {Returns the transparency mode used by this png}
    property TransparencyMode: TPNGTransparencyMode read GetTransparencyMode;
    {Assigns from another object}
    procedure Assign(Source: TPersistent);{$IFDEF UseDelphi}override;{$ENDIF}
    {Assigns to another object}
    procedure AssignTo(Dest: TPersistent);{$IFDEF UseDelphi}override;{$ENDIF}
    {Assigns from a windows bitmap handle}
    procedure AssignHandle(Handle: HBitmap; Transparent: Boolean;
      TransparentColor: ColorRef);
    {Draws the image into a canvas}
    procedure Draw(ACanvas: TCanvas; const Rect: TRect);
      {$IFDEF UseDelphi}override;{$ENDIF}
    {Width and height properties}
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    {Returns if the image is interlaced}
    property InterlaceMethod: TInterlaceMethod read fInterlaceMethod
      write fInterlaceMethod;
    {Filters to test to encode}
    property Filters: TFilters read fFilters write fFilters;
    {Maximum size for IDAT chunks, default and minimum is 65536}
    property MaxIdatSize: Cardinal read fMaxIdatSize write SetMaxIdatSize;
    {Property to return if the image is empty or not}
    property Empty: Boolean read GetEmpty;
    {Compression level}
    property CompressionLevel: TCompressionLevel read fCompressionLevel
      write fCompressionLevel;
    {Access to the chunk list}
    property Chunks: TPngList read fChunkList;
    {Object being created and destroyed}
    constructor Create; {$IFDEF UseDelphi}override;{$ENDIF}
    destructor Destroy; override;
    {$IFNDEF UseDelphi}procedure LoadFromFile(const Filename: String);{$ENDIF}
    {$IFNDEF UseDelphi}procedure SaveToFile(const Filename: String);{$ENDIF}
    procedure LoadFromStream(Stream: TStream); {$IFDEF UseDelphi}override;{$ENDIF}
    procedure SaveToStream(Stream: TStream); {$IFDEF UseDelphi}override;{$ENDIF}
    {Loading the image from resources}
    procedure LoadFromResourceName(Instance: HInst; const Name: String);
    procedure LoadFromResourceID(Instance: HInst; ResID: Integer);
    {Access to the png pixels}
    property Pixels[const X, Y: Integer]: TColor read GetPixels write SetPixels;
    {Palette property}
    {THANY: Palette is now writeable}
    {$IFNDEF UseDelphi}property Palette: HPalette read GetPalette write SetPalette;{$ENDIF}
  end;

  {Chunk name object}
  TChunkName = Array[0..3] of Char;

  {Global chunk object}
  TChunk = class
  private
    {Contains data}
    fData: Pointer;
    fDataSize: Cardinal;
    {Stores owner}
    fOwner: TPngObject;
    {Stores the chunk name}
    fName: TChunkName;
    {Returns pointer to the TChunkIHDR}
    function GetHeader: TChunkIHDR;
    {Used with property index}
    function GetIndex: Integer;
    {Should return chunk class/name}
    class function GetName: String; virtual;
    {Returns the chunk name}
    function GetChunkName: String;
  public
    {Returns index from list}
    property Index: Integer read GetIndex;
    {Returns pointer to the TChunkIHDR}
    property Header: TChunkIHDR read GetHeader;
    {Resize the data}
    procedure ResizeData(const NewSize: Cardinal);
    {Returns data and size}
    property Data: Pointer read fData;
    property DataSize: Cardinal read fDataSize;
    {Assigns from another TChunk}
    procedure Assign(Source: TChunk); virtual;
    {Returns owner}
    property Owner: TPngObject read fOwner;
    {Being destroyed/created}
    constructor Create(Owner: TPngObject); virtual;
    destructor Destroy; override;
    {Returns chunk class/name}
    property Name: String read GetChunkName;
    {Loads the chunk from a stream}
    function LoadFromStream(Stream: TStream; const ChunkName: TChunkName;
      Size: Integer): Boolean; virtual;
    {Saves the chunk to a stream}
    function SaveData(Stream: TStream): Boolean;
    function SaveToStream(Stream: TStream): Boolean; virtual;
  end;

  {Chunk classes}
  TChunkIEND = class(TChunk);     {End chunk}

  {IHDR data}
  pIHDRData = ^TIHDRData;
  TIHDRData = packed record
    Width, Height: Cardinal;
    BitDepth,
    ColorType,
    CompressionMethod,
    FilterMethod,
    InterlaceMethod: Byte;
  end;

  {Information header chunk}
  TChunkIHDR = class(TChunk)
  private
    {Current image}
    ImageHandle: HBitmap;
    ImageDC: HDC;

    {Output windows bitmap}
    HasPalette: Boolean;
    BitmapInfo: TMaxBitmapInfo;
    BytesPerRow: Integer;
    {Stores the image bytes}
    {$IFDEF Store16bits}ExtraImageData: Pointer;{$ENDIF}
    ImageData: pointer;
    ImageAlpha: Pointer;

    {Contains all the ihdr data}
    IHDRData: TIHDRData;
  protected
    {Resizes the image data to fill the color type, bit depth, }
    {width and height parameters}
    procedure PrepareImageData;
    {Release allocated ImageData memory}
    procedure FreeImageData;
  public
    {Properties}
    property Width: Cardinal read IHDRData.Width write IHDRData.Width;
    property Height: Cardinal read IHDRData.Height write IHDRData.Height;
    property BitDepth: Byte read IHDRData.BitDepth write IHDRData.BitDepth;
    property ColorType: Byte read IHDRData.ColorType write IHDRData.ColorType;
    property CompressionMethod: Byte read IHDRData.CompressionMethod
      write IHDRData.CompressionMethod;
    property FilterMethod: Byte read IHDRData.FilterMethod
      write IHDRData.FilterMethod;
    property InterlaceMethod: Byte read IHDRData.InterlaceMethod
      write IHDRData.InterlaceMethod;
    {Loads the chunk from a stream}
    function LoadFromStream(Stream: TStream; const ChunkName: TChunkName;
      Size: Integer): Boolean; override;
    {Saves the chunk to a stream}
    function SaveToStream(Stream: TStream): Boolean; override;
    {Destructor/constructor}
    constructor Create(Owner: TPngObject); override;
    destructor Destroy; override;
    {Assigns from another TChunk}
    procedure Assign(Source: TChunk); override;
  end;

  {Gamma chunk}
  TChunkgAMA = class(TChunk)
  private
    {Returns/sets the value for the gamma chunk}
    function GetValue: Cardinal;
    procedure SetValue(const Value: Cardinal);
  public
    {Returns/sets gamma value}
    property Gamma: Cardinal read GetValue write SetValue;
    {Loading the chunk from a stream}
    function LoadFromStream(Stream: TStream; const ChunkName: TChunkName;
      Size: Integer): Boolean; override;
    {Being created}
    constructor Create(Owner: TPngObject); override;
    {Assigns from another TChunk}
    procedure Assign(Source: TChunk); override;
  end;

  {ZLIB Decompression extra information}
  TZStreamRec2 = packed record
    {From ZLIB}
    ZLIB: TZStreamRec;
    {Additional info}
    Data: Pointer;
    fStream   : TStream;
  end;

  {Palette chunk}
  TChunkPLTE = class(TChunk)
  private
    {Number of items in the palette}
    fCount: Integer;
    {Contains the palette handle}
    function GetPaletteItem(Index: Byte): TRGBQuad;
  public
    {Returns the color for each item in the palette}
    property Item[Index: Byte]: TRGBQuad read GetPaletteItem;
    {Returns the number of items in the palette}
    property Count: Integer read fCount;
    {Loads the chunk from a stream}
    function LoadFromStream(Stream: TStream; const ChunkName: TChunkName;
      Size: Integer): Boolean; override;
    {Saves the chunk to a stream}
    function SaveToStream(Stream: TStream): Boolean; override;
    {Assigns from another TChunk}
    procedure Assign(Source: TChunk); override;
  end;

  {Transparency information}
  TChunktRNS = class(TChunk)
  private
    fBitTransparency: Boolean;
    function GetTransparentColor: ColorRef;
    {Returns the transparent color}
    procedure SetTransparentColor(const Value: ColorRef);
  public
    {Palette values for transparency}
    PaletteValues: Array[Byte] of Byte;
    {Returns if it uses bit transparency}
    property BitTransparency: Boolean read fBitTransparency;
    {Returns the transparent color}
    property TransparentColor: ColorRef read GetTransparentColor write
      SetTransparentColor;
    {Loads/saves the chunk from/to a stream}
    function LoadFromStream(Stream: TStream; const ChunkName: TChunkName;
      Size: Integer): Boolean; override;
    function SaveToStream(Stream: TStream): Boolean; override;
    {Assigns from another TChunk}
    procedure Assign(Source: TChunk); override;
  end;

  {Actual image information}
  TChunkIDAT = class(TChunk)
  private
    {Holds another pointer to the TChunkIHDR}
    Header: TChunkIHDR;
    {Stores temporary image width and height}
    ImageWidth, ImageHeight: Integer;
    {Size in bytes of each line and offset}
    Row_Bytes, Offset : Cardinal;
    {Contains data for the lines}
    Encode_Buffer: Array[0..5] of pByteArray;
    Row_Buffer: Array[Boolean] of pByteArray;
    {Variable to invert the Row_Buffer used}
    RowUsed: Boolean;
    {Ending position for the current IDAT chunk}
    EndPos: Integer;
    {Filter the current line}
    procedure FilterRow;
    {Filter to encode and returns the best filter}
    function FilterToEncode: Byte;
    {Reads ZLIB compressed data}
    function IDATZlibRead(var ZLIBStream: TZStreamRec2; Buffer: Pointer;
      Count: Integer; var EndPos: Integer; var crcfile: Cardinal): Integer;
    {Compress and writes IDAT data}
    procedure IDATZlibWrite(var ZLIBStream: TZStreamRec2; Buffer: Pointer;
      const Length: Cardinal);
    procedure FinishIDATZlib(var ZLIBStream: TZStreamRec2);
    {Prepares the palette}
    procedure PreparePalette;
  protected
    {Decode interlaced image}
    procedure DecodeInterlacedAdam7(Stream: TStream;
      var ZLIBStream: TZStreamRec2; const Size: Integer; var crcfile: Cardinal);
    {Decode non interlaced imaged}
    procedure DecodeNonInterlaced(Stream: TStream;
      var ZLIBStream: TZStreamRec2; const Size: Integer;
      var crcfile: Cardinal);
  protected
    {Encode non interlaced images}
    procedure EncodeNonInterlaced(Stream: TStream;
      var ZLIBStream: TZStreamRec2);
    {Encode interlaced images}
    procedure EncodeInterlacedAdam7(Stream: TStream;
      var ZLIBStream: TZStreamRec2);
  protected
    {Memory copy methods to decode}
    procedure CopyNonInterlacedRGB8(
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
    procedure CopyNonInterlacedRGB16(
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
    procedure CopyNonInterlacedPalette148(
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
    procedure CopyNonInterlacedPalette2(
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
    procedure CopyNonInterlacedGray2(
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
    procedure CopyNonInterlacedGrayscale16(
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
    procedure CopyNonInterlacedRGBAlpha8(
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
    procedure CopyNonInterlacedRGBAlpha16(
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
    procedure CopyNonInterlacedGrayscaleAlpha8(
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
    procedure CopyNonInterlacedGrayscaleAlpha16(
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
    procedure CopyInterlacedRGB8(const Pass: Byte;
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
    procedure CopyInterlacedRGB16(const Pass: Byte;
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
    procedure CopyInterlacedPalette148(const Pass: Byte;
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
    procedure CopyInterlacedPalette2(const Pass: Byte;
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
    procedure CopyInterlacedGray2(const Pass: Byte;
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
    procedure CopyInterlacedGrayscale16(const Pass: Byte;
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
    procedure CopyInterlacedRGBAlpha8(const Pass: Byte;
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
    procedure CopyInterlacedRGBAlpha16(const Pass: Byte;
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
    procedure CopyInterlacedGrayscaleAlpha8(const Pass: Byte;
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
    procedure CopyInterlacedGrayscaleAlpha16(const Pass: Byte;
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
  protected
    {Memory copy methods to encode}
    procedure EncodeNonInterlacedRGB8(Src, Dest, Trans: pChar);
    procedure EncodeNonInterlacedRGB16(Src, Dest, Trans: pChar);
    procedure EncodeNonInterlacedGrayscale16(Src, Dest, Trans: pChar);
    procedure EncodeNonInterlacedPalette148(Src, Dest, Trans: pChar);
    procedure EncodeNonInterlacedRGBAlpha8(Src, Dest, Trans: pChar);
    procedure EncodeNonInterlacedRGBAlpha16(Src, Dest, Trans: pChar);
    procedure EncodeNonInterlacedGrayscaleAlpha8(Src, Dest, Trans: pChar);
    procedure EncodeNonInterlacedGrayscaleAlpha16(Src, Dest, Trans: pChar);
    procedure EncodeInterlacedRGB8(const Pass: Byte; Src, Dest, Trans: pChar);
    procedure EncodeInterlacedRGB16(const Pass: Byte; Src, Dest, Trans: pChar);
    procedure EncodeInterlacedPalette148(const Pass: Byte;
      Src, Dest, Trans: pChar);
    procedure EncodeInterlacedGrayscale16(const Pass: Byte;
      Src, Dest, Trans: pChar);
    procedure EncodeInterlacedRGBAlpha8(const Pass: Byte;
      Src, Dest, Trans: pChar);
    procedure EncodeInterlacedRGBAlpha16(const Pass: Byte;
      Src, Dest, Trans: pChar);
    procedure EncodeInterlacedGrayscaleAlpha8(const Pass: Byte;
      Src, Dest, Trans: pChar);
    procedure EncodeInterlacedGrayscaleAlpha16(const Pass: Byte;
      Src, Dest, Trans: pChar);
  public
    {Loads the chunk from a stream}
    function LoadFromStream(Stream: TStream; const ChunkName: TChunkName;
      Size: Integer): Boolean; override;
    {Saves the chunk to a stream}
    function SaveToStream(Stream: TStream): Boolean; override;
  end;

  {Image last modification chunk}
  TChunktIME = class(TChunk)
  private
    {Holds the variables}
    fYear: Word;
    fMonth, fDay, fHour, fMinute, fSecond: Byte;
  public
    {Returns/sets variables}
    property Year: Word read fYear write fYear;
    property Month: Byte read fMonth write fMonth;
    property Day: Byte read fDay write fDay;
    property Hour: Byte read fHour write fHour;
    property Minute: Byte read fMinute write fMinute;
    property Second: Byte read fSecond write fSecond;
    {Loads the chunk from a stream}
    function LoadFromStream(Stream: TStream; const ChunkName: TChunkName;
      Size: Integer): Boolean; override;
    {Saves the chunk to a stream}
    function SaveToStream(Stream: TStream): Boolean; override;
  end;

  {Textual data}
  TChunktEXt = class(TChunk)
  private
    fKeyword, fText: String;
  public
    {Keyword and text}
    property Keyword: String read fKeyword write fKeyword;
    property Text: String read fText write fText;
    {Loads the chunk from a stream}
    function LoadFromStream(Stream: TStream; const ChunkName: TChunkName;
      Size: Integer): Boolean; override;
    {Saves the chunk to a stream}
    function SaveToStream(Stream: TStream): Boolean; override;
    {Assigns from another TChunk}
    procedure Assign(Source: TChunk); override;
  end;

  {zTXT chunk}
  TChunkzTXt = class(TChunktEXt)
    {Loads the chunk from a stream}
    function LoadFromStream(Stream: TStream; const ChunkName: TChunkName;
      Size: Integer): Boolean; override;
    {Saves the chunk to a stream}
    function SaveToStream(Stream: TStream): Boolean; override;
  end;

{Here we test if it's c++ builder or delphi version 3 or less}
{$IFDEF VER110}{$DEFINE DelphiBuilder3Less}{$ENDIF}
{$IFDEF VER100}{$DEFINE DelphiBuilder3Less}{$ENDIF}
{$IFDEF VER93}{$DEFINE DelphiBuilder3Less}{$ENDIF}
{$IFDEF VER90}{$DEFINE DelphiBuilder3Less}{$ENDIF}
{$IFDEF VER80}{$DEFINE DelphiBuilder3Less}{$ENDIF}


{Registers a new chunk class}
procedure RegisterChunk(ChunkClass: TChunkClass);
{Calculates crc}
function update_crc(crc: {$IFNDEF DelphiBuilder3Less}Cardinal{$ELSE}Integer
  {$ENDIF}; buf: pByteArray; len: Integer): Cardinal;
{Invert bytes using assembly}
function ByteSwap(const a: integer): integer;

implementation

var
  ChunkClasses: TPngPointerList;
  {Table of CRCs of all 8-bit messages}
  crc_table: Array[0..255] of Cardinal;
  {Flag: has the table been computed? Initially false}
  crc_table_computed: Boolean;

{Draw transparent image using transparent color}
procedure DrawTransparentBitmap(dc: HDC; srcBits: Pointer;
  var srcHeader: TBitmapInfoHeader;
  srcBitmapInfo: pBitmapInfo; Rect: TRect; cTransparentColor: COLORREF);
var
  cColor:   COLORREF;
  bmAndBack, bmAndObject, bmAndMem: HBITMAP;
  bmBackOld, bmObjectOld, bmMemOld: HBITMAP;
  hdcMem, hdcBack, hdcObject, hdcTemp: HDC;
  ptSize, orgSize: TPOINT;
  OldBitmap, DrawBitmap: HBITMAP;
begin
  hdcTemp := CreateCompatibleDC(dc);
  // Select the bitmap
  DrawBitmap := CreateDIBitmap(dc, srcHeader, CBM_INIT, srcBits, srcBitmapInfo^,
    DIB_RGB_COLORS);
  OldBitmap := SelectObject(hdcTemp, DrawBitmap);

  // Sizes
  OrgSize.x := abs(srcHeader.biWidth);
  OrgSize.y := abs(srcHeader.biHeight);
  ptSize.x := Rect.Right - Rect.Left;        // Get width of bitmap
  ptSize.y := Rect.Bottom - Rect.Top;        // Get height of bitmap

  // Create some DCs to hold temporary data.
  hdcBack  := CreateCompatibleDC(dc);
  hdcObject := CreateCompatibleDC(dc);
  hdcMem   := CreateCompatibleDC(dc);

  // Create a bitmap for each DC. DCs are required for a number of
  // GDI functions.

  // Monochrome DCs
  bmAndBack  := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);
  bmAndObject := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);

  bmAndMem   := CreateCompatibleBitmap(dc, ptSize.x, ptSize.y);

  // Each DC must select a bitmap object to store pixel data.
  bmBackOld  := SelectObject(hdcBack, bmAndBack);
  bmObjectOld := SelectObject(hdcObject, bmAndObject);
  bmMemOld   := SelectObject(hdcMem, bmAndMem);

  // Set the background color of the source DC to the color.
  // contained in the parts of the bitmap that should be transparent
  cColor := SetBkColor(hdcTemp, cTransparentColor);

  // Create the object mask for the bitmap by performing a BitBlt
  // from the source bitmap to a monochrome bitmap.
  StretchBlt(hdcObject, 0, 0, ptSize.x, ptSize.y, hdcTemp, 0, 0,
    orgSize.x, orgSize.y, SRCCOPY);

  // Set the background color of the source DC back to the original
  // color.
  SetBkColor(hdcTemp, cColor);

  // Create the inverse of the object mask.
  BitBlt(hdcBack, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0,
       NOTSRCCOPY);

  // Copy the background of the main DC to the destination.
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, dc, Rect.Left, Rect.Top,
       SRCCOPY);

  // Mask out the places where the bitmap will be placed.
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, SRCAND);

  // Mask out the transparent colored pixels on the bitmap.
//  BitBlt(hdcTemp, 0, 0, ptSize.x, ptSize.y, hdcBack, 0, 0, SRCAND);
  StretchBlt(hdcTemp, 0, 0, OrgSize.x, OrgSize.y, hdcBack, 0, 0,
    PtSize.x, PtSize.y, SRCAND);

  // XOR the bitmap with the background on the destination DC.
  StretchBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcTemp, 0, 0,
    OrgSize.x, OrgSize.y, SRCPAINT);

  // Copy the destination to the screen.
  BitBlt(dc, Rect.Left, Rect.Top, ptSize.x, ptSize.y, hdcMem, 0, 0,
       SRCCOPY);

  // Delete the memory bitmaps.
  DeleteObject(SelectObject(hdcBack, bmBackOld));
  DeleteObject(SelectObject(hdcObject, bmObjectOld));
  DeleteObject(SelectObject(hdcMem, bmMemOld));
  DeleteObject(SelectObject(hdcTemp, OldBitmap));

  // Delete the memory DCs.
  DeleteDC(hdcMem);
  DeleteDC(hdcBack);
  DeleteDC(hdcObject);
  DeleteDC(hdcTemp);
end;

{Make the table for a fast CRC.}
procedure make_crc_table;
var
  c: Cardinal;
  n, k: Integer;
begin

  {fill the crc table}
  for n := 0 to 255 do
  begin
    c := Cardinal(n);
    for k := 0 to 7 do
    begin
      if Boolean(c and 1) then
        c := $edb88320 xor (c shr 1)
      else
        c := c shr 1;
    end;
    crc_table[n] := c;
  end;

  {The table has already being computated}
  crc_table_computed := true;
end;

{Update a running CRC with the bytes buf[0..len-1]--the CRC
 should be initialized to all 1's, and the transmitted value
 is the 1's complement of the final running CRC (see the
 crc() routine below)).}
function update_crc(crc: {$IFNDEF DelphiBuilder3Less}Cardinal{$ELSE}Integer
  {$ENDIF}; buf: pByteArray; len: Integer): Cardinal;
var
  c: Cardinal;
  n: Integer;
begin
  c := crc;

  {Create the crc table in case it has not being computed yet}
  if not crc_table_computed then make_crc_table;

  {Update}
  for n := 0 to len - 1 do
    c := crc_table[(c XOR buf^[n]) and $FF] XOR (c shr 8);

  {Returns}
  Result := c;
end;

{$IFNDEF UseDelphi}
  function FileExists(Filename: String): Boolean;
  var
    FindFile: THandle;
    FindData: TWin32FindData;
  begin
    FindFile := FindFirstFile(PChar(Filename), FindData);
    Result := FindFile <> INVALID_HANDLE_VALUE;
    if Result then Windows.FindClose(FindFile);
  end;


{$ENDIF}

{$IFNDEF UseDelphi}
  {Exception implementation}
  constructor Exception.Create(Msg: String);
  begin
  end;
{$ENDIF}

{Calculates the paeth predictor}
function PaethPredictor(a, b, c: Byte): Byte;
var
  pa, pb, pc: Integer;
begin
  { a = left, b = above, c = upper left }
  pa := abs(b - c);      { distances to a, b, c }
  pb := abs(a - c);
  pc := abs(a + b - c * 2);

  { return nearest of a, b, c, breaking ties in order a, b, c }
  if (pa <= pb) and (pa <= pc) then
    Result := a
  else
    if pb <= pc then
      Result := b
    else
      Result := c;
end;

{Invert bytes using assembly}
function ByteSwap(const a: integer): integer;
asm
  bswap eax
end;
function ByteSwap16(inp:word): word;
asm
  bswap eax
  shr   eax, 16
end;

{Calculates number of bytes for the number of pixels using the}
{color mode in the paramenter}
function BytesForPixels(const Pixels: Integer; const ColorType,
  BitDepth: Byte): Integer;
begin
  case ColorType of
    {Palette and grayscale contains a single value, for palette}
    {an value of size 2^bitdepth pointing to the palette index}
    {and grayscale the value from 0 to 2^bitdepth with color intesity}
    COLOR_GRAYSCALE, COLOR_PALETTE:
      Result := (Pixels * BitDepth + 7) div 8;
    {RGB contains 3 values R, G, B with size 2^bitdepth each}
    COLOR_RGB:
      Result := (Pixels * BitDepth * 3) div 8;
    {Contains one value followed by alpha value booth size 2^bitdepth}
    COLOR_GRAYSCALEALPHA:
      Result := (Pixels * BitDepth * 2) div 8;
    {Contains four values size 2^bitdepth, Red, Green, Blue and alpha}
    COLOR_RGBALPHA:
      Result := (Pixels * BitDepth * 4) div 8;
    else
      Result := 0;
  end {case ColorType}
end;

type
  pChunkClassInfo = ^TChunkClassInfo;
  TChunkClassInfo = record
    ClassName: TChunkClass;
  end;

{Register a chunk type}
procedure RegisterChunk(ChunkClass: TChunkClass);
var
  NewClass: pChunkClassInfo;
begin
  {In case the list object has not being created yet}
  if ChunkClasses = nil then ChunkClasses := TPngPointerList.Create(nil);

  {Add this new class}
  new(NewClass);
  NewClass^.ClassName := ChunkClass;
  ChunkClasses.Add(NewClass);
end;

{Free chunk class list}
procedure FreeChunkClassList;
var
  i: Integer;
begin
  if (ChunkClasses <> nil) then
  begin
    FOR i := 0 TO ChunkClasses.Count - 1 do
      Dispose(pChunkClassInfo(ChunkClasses.Item[i]));
    ChunkClasses.Free;
  end;
end;

{Registering of common chunk classes}
procedure RegisterCommonChunks;
begin
  {Important chunks}
  RegisterChunk(TChunkIEND);
  RegisterChunk(TChunkIHDR);
  RegisterChunk(TChunkIDAT);
  RegisterChunk(TChunkPLTE);
  RegisterChunk(TChunkgAMA);
  RegisterChunk(TChunktRNS);

  {Not so important chunks}
  RegisterChunk(TChunktIME);
  RegisterChunk(TChunktEXt);
  RegisterChunk(TChunkzTXt);
end;

{Creates a new chunk of this class}
function CreateClassChunk(Owner: TPngObject; Name: TChunkName): TChunk;
var
  i       : Integer;
  NewChunk: TChunkClass;
begin
  {Looks for this chunk}
  NewChunk := TChunk;  {In case there is no registered class for this}

  {Looks for this class in all registered chunks}
  if Assigned(ChunkClasses) then
    FOR i := 0 TO ChunkClasses.Count - 1 DO
    begin
      if pChunkClassInfo(ChunkClasses.Item[i])^.ClassName.GetName = Name then
      begin
        NewChunk := pChunkClassInfo(ChunkClasses.Item[i])^.ClassName;
        break;
      end;
    end;

  {Returns chunk class}
  Result := NewChunk.Create(Owner);
  Result.fName := Name;
end;

{ZLIB support}

const
  ZLIBAllocate = High(Word);

{Initializes ZLIB for decompression}
function ZLIBInitInflate(Stream: TStream): TZStreamRec2;
begin
  {Fill record}
  Fillchar(Result, SIZEOF(TZStreamRec2), #0);

  {Set internal record information}
  with Result do
  begin
    GetMem(Data, ZLIBAllocate);
    fStream := Stream;
  end;

  {Init decompression}
  InflateInit_(Result.zlib, zlib_version, SIZEOF(TZStreamRec));
end;

{Initializes ZLIB for compression}
function ZLIBInitDeflate(Stream: TStream;
  Level: TCompressionlevel; Size: Cardinal): TZStreamRec2;
begin
  {Fill record}
  Fillchar(Result, SIZEOF(TZStreamRec2), #0);

  {Set internal record information}
  with Result, ZLIB do
  begin
    GetMem(Data, Size);
    fStream := Stream;
    next_out := Data;
    avail_out := Size;
  end;

  {Inits compression}
  deflateInit_(Result.zlib, Level, zlib_version, sizeof(TZStreamRec));
end;

{Terminates ZLIB for compression}
procedure ZLIBTerminateDeflate(var ZLIBStream: TZStreamRec2);
begin
  {Terminates decompression}
  DeflateEnd(ZLIBStream.zlib);
  {Free internal record}
  FreeMem(ZLIBStream.Data, ZLIBAllocate);
end;

{Terminates ZLIB for decompression}
procedure ZLIBTerminateInflate(var ZLIBStream: TZStreamRec2);
begin
  {Terminates decompression}
  InflateEnd(ZLIBStream.zlib);
  {Free internal record}
  FreeMem(ZLIBStream.Data, ZLIBAllocate);
end;

{Decompresses ZLIB into a memory address}
function DecompressZLIB(const Input: Pointer; InputSize: Integer;
  var Output: Pointer; var OutputSize: Integer;
  var ErrorOutput: String): Boolean;
var
  StreamRec : TZStreamRec;
  Buffer    : Array[Byte] of Byte;
  InflateRet: Integer;
begin
  with StreamRec do
  begin
    {Initializes}
    Result := True;
    OutputSize := 0;

    {Prepares the data to decompress}
    FillChar(StreamRec, SizeOf(TZStreamRec), #0);
    InflateInit_(StreamRec, zlib_version, SIZEOF(TZStreamRec));
    next_in := Input;
    avail_in := InputSize;

    {Decodes data}
    repeat
      {In case it needs an output buffer}
      if (avail_out = 0) then
      begin
        next_out := @Buffer;
        avail_out := SizeOf(Buffer);
      end {if (avail_out = 0)};

      {Decompress and put in output}
      InflateRet := inflate(StreamRec, 0);
      if (InflateRet = Z_STREAM_END) or (InflateRet = 0) then
      begin
        {Reallocates output buffer}
        inc(OutputSize, total_out);
        if Output = nil then
          GetMem(Output, OutputSize) else ReallocMem(Output, OutputSize);
        {Copies the new data}
        CopyMemory(Ptr(Longint(Output) + OutputSize - total_out),
          @Buffer, total_out);
      end {if (InflateRet = Z_STREAM_END) or (InflateRet = 0)}
      {Now tests for errors}
      else if InflateRet < 0 then
      begin
        Result := False;
        ErrorOutput := StreamRec.msg;
        InflateEnd(StreamRec);
        Exit;
      end {if InflateRet < 0}
    until InflateRet = Z_STREAM_END;

    {Terminates decompression}
    InflateEnd(StreamRec);
  end {with StreamRec}

end;

{Compresses ZLIB into a memory address}
function CompressZLIB(Input: Pointer; InputSize, CompressionLevel: Integer;
  var Output: Pointer; var OutputSize: Integer;
  var ErrorOutput: String): Boolean;
var
  StreamRec : TZStreamRec;
  Buffer    : Array[Byte] of Byte;
  DeflateRet: Integer;
begin
  with StreamRec do
  begin
    Result := True; {By default returns TRUE as everything might have gone ok}
    OutputSize := 0; {Initialize}
    {Prepares the data to compress}
    FillChar(StreamRec, SizeOf(TZStreamRec), #0);
    DeflateInit_(StreamRec, CompressionLevel,zlib_version, SIZEOF(TZStreamRec));

    next_in := Input;
    avail_in := InputSize;

    while avail_in > 0 do
    begin
      {When it needs new buffer to stores the compressed data}
      if avail_out = 0 then
      begin
        {Restore buffer}
        next_out := @Buffer;
        avail_out := SizeOf(Buffer);
      end {if avail_out = 0};

      {Compresses}
      DeflateRet := deflate(StreamRec, Z_FINISH);

      if (DeflateRet = Z_STREAM_END) or (DeflateRet = 0) then
      begin
        {Updates the output memory}
        inc(OutputSize, total_out);
        if Output = nil then
          GetMem(Output, OutputSize) else ReallocMem(Output, OutputSize);

        {Copies the new data}
        CopyMemory(Ptr(Longint(Output) + OutputSize - total_out),
          @Buffer, total_out);
      end {if (InflateRet = Z_STREAM_END) or (InflateRet = 0)}
      {Now tests for errors}
      else if DeflateRet < 0 then
      begin
        Result := False;
        ErrorOutput := StreamRec.msg;
        DeflateEnd(StreamRec);
        Exit;
      end {if InflateRet < 0}

    end {while avail_in > 0};

    {Finishes compressing}
    DeflateEnd(StreamRec);
  end {with StreamRec}

end;

{TPngPointerList implementation}

{Object being created}
constructor TPngPointerList.Create(AOwner: TPNGObject);
begin
  inherited Create; {Let ancestor work}
  {Holds owner}
  fOwner := AOwner;
  {Memory pointer not being used yet}
  fMemory := nil;
  {No items yet}
  fCount := 0;
end;

{Removes value from the list}
function TPngPointerList.Remove(Value: Pointer): Pointer;
var
  I, Position: Integer;
begin
  {Gets item position}
  Position := -1;
  FOR I := 0 TO Count - 1 DO
    if Value = Item[I] then Position := I;
  {In case a match was found}
  if Position >= 0 then
  begin
    Result := Item[Position]; {Returns pointer}
    {Remove item and move memory}
    Dec(fCount);
    if Position < Integer(FCount) then
      System.Move(fMemory^[Position + 1], fMemory^[Position],
      (Integer(fCount) - Position) * SizeOf(Pointer));
  end {if Position >= 0} else Result := nil
end;

{Add a new value in the list}
procedure TPngPointerList.Add(Value: Pointer);
begin
  Count := Count + 1;
  Item[Count - 1] := Value;
end;


{Object being destroyed}
destructor TPngPointerList.Destroy;
begin
  {Release memory if needed}
  if fMemory <> nil then
    FreeMem(fMemory, fCount * sizeof(Pointer));

  {Free things}
  inherited Destroy;
end;

{Returns one item from the list}
function TPngPointerList.GetItem(Index: Cardinal): Pointer;
begin
  if (Index <= Count - 1) then
    Result := fMemory[Index]
  else
    {In case it's out of bounds}
    Result := nil;
end;

{Inserts a new item in the list}
procedure TPngPointerList.Insert(Value: Pointer; Position: Cardinal);
begin
  if (Position < Count) then
  begin
    {Increase item count}
    SetSize(Count + 1);
    {Move other pointers}
    if Position < Count then
      System.Move(fMemory^[Position], fMemory^[Position + 1],
        (Count - Position - 1) * SizeOf(Pointer));
    {Sets item}
    Item[Position] := Value;
  end;
end;

{Sets one item from the list}
procedure TPngPointerList.SetItem(Index: Cardinal; const Value: Pointer);
begin
  {If index is in bounds, set value}
  if (Index <= Count - 1) then
    fMemory[Index] := Value
end;

{This method resizes the list}
procedure TPngPointerList.SetSize(const Size: Cardinal);
begin
  {Sets the size}
  if (fMemory = nil) and (Size > 0) then
    GetMem(fMemory, Size * SIZEOF(Pointer))
  else
    if Size > 0 then  {Only realloc if the new size is greater than 0}
      ReallocMem(fMemory, Size * SIZEOF(Pointer))
    else
    {In case user is resize to 0 items}
    begin
      FreeMem(fMemory);
      fMemory := nil;
    end;
  {Update count}
  fCount := Size;
end;

{TPNGList implementation}

{Removes an item}
procedure TPNGList.RemoveChunk(Chunk: TChunk);
begin
  Remove(Chunk);
  Chunk.Free
end;

{Add a new item}
function TPNGList.Add(ChunkClass: TChunkClass): TChunk;
var
  IHDR: TChunkIHDR;
  IEND: TChunkIEND;

  IDAT: TChunkIDAT;
  PLTE: TChunkPLTE;
begin
  Result := nil; {Default result}
  {Adding these is not allowed}
  if (ChunkClass = TChunkIHDR) or (ChunkClass = TChunkIDAT) or
    (ChunkClass = TChunkPLTE) or (ChunkClass = TChunkIEND) then
    fOwner.RaiseError(EPngError, EPNGCannotAddChunkText)
  {Two of these is not allowed}
  else if ((ChunkClass = TChunkgAMA) and (ItemFromClass(TChunkgAMA) <> nil)) or
     ((ChunkClass = TChunktRNS) and (ItemFromClass(TChunktRNS) <> nil)) then
    fOwner.RaiseError(EPngError, EPNGCannotAddChunkText)
  {There must have an IEND and IHDR chunk}
  else if (ItemFromClass(TChunkIEND) = nil) or
    (ItemFromClass(TChunkIHDR) = nil) then
    fOwner.RaiseError(EPngError, EPNGCannotAddInvalidImageText)
  else
  begin
    {Get common chunks}
    IHDR := ItemFromClass(TChunkIHDR) as TChunkIHDR;
    IEND := ItemFromClass(TChunkIEND) as TChunkIEND;
    {Create new chunk}
    Result := ChunkClass.Create(Owner);
    {Add to the list}
    if (ChunkClass = TChunkgAMA) then
      Insert(Result, IHDR.Index + 1)
    {Transparency chunk (fix by Ian Boyd)}
    else if (ChunkClass = TChunktRNS) then
    begin
      {Transparecy chunk must be after PLTE; before IDAT}
      IDAT := ItemFromClass(TChunkIDAT) as TChunkIDAT;
      PLTE := ItemFromClass(TChunkPLTE) as TChunkPLTE;

      if Assigned(PLTE) then
        Insert(Result, PLTE.Index + 1)
      else if Assigned(IDAT) then
        Insert(Result, IDAT.Index)
      else
        Insert(Result, IHDR.Index + 1)
    end
    else {All other chunks}
      Insert(Result, IEND.Index);
  end {if}
end;

{Returns item from the list}
function TPNGList.GetItem(Index: Cardinal): TChunk;
begin
  Result := inherited GetItem(Index);
end;

{Returns first item from the list using the class from parameter}
function TPNGList.ItemFromClass(ChunkClass: TChunkClass): TChunk;
var
  i: Integer;
begin
  Result := nil; {Initial result}
  FOR i := 0 TO Count - 1 DO
    {Test if this item has the same class}
    if Item[i] is ChunkClass then
    begin
      {Returns this item and exit}
      Result := Item[i];
      break;
    end {if}
end;

{$IFNDEF UseDelphi}

  {TStream implementation}

  {Copies all from another stream}
  function TStream.CopyFrom(Source: TStream; Count: Cardinal): Cardinal;
  const
    MaxBytes = $f000;
  var
    Buffer:  PChar;
    BufSize, N: Cardinal;
  begin
    {If count is zero, copy everything from Source}
    if Count = 0 then
    begin
      Source.Seek(0, soFromBeginning);
      Count := Source.Size;
    end;

    Result := Count; {Returns the number of bytes readed}
    {Allocates memory}
    if Count > MaxBytes then BufSize := MaxBytes else BufSize := Count;
    GetMem(Buffer, BufSize);

    {Copy memory}
    while Count > 0 do
    begin
      if Count > BufSize then N := BufSize else N := Count;
      Source.Read(Buffer^, N);
      Write(Buffer^, N);
      dec(Count, N);
    end;

    {Deallocates memory}
    FreeMem(Buffer, BufSize);
  end;

{Set current stream position}
procedure TStream.SetPosition(const Value: Longint);
begin
  Seek(Value, soFromBeginning);
end;

{Returns position}
function TStream.GetPosition: Longint;
begin
  Result := Seek(0, soFromCurrent);
end;

  {Returns stream size}
function TStream.GetSize: Longint;
  var
    Pos: Cardinal;
  begin
    Pos := Seek(0, soFromCurrent);
    Result := Seek(0, soFromEnd);
    Seek(Pos, soFromCurrent);
  end;

  {TFileStream implementation}

  {Filestream object being created}
  constructor TFileStream.Create(Filename: String; Mode: TFileStreamModeSet);
    {Makes file mode}
    function OpenMode: DWORD;
    begin
      Result := 0;
      if fsmRead in Mode then Result := GENERIC_READ;
      if (fsmWrite in Mode) or (fsmCreate in Mode) then
        Result := Result OR GENERIC_WRITE;
    end;
  const
    IsCreate: Array[Boolean] of Integer = (OPEN_ALWAYS, CREATE_ALWAYS);
  begin
    {Call ancestor}
    inherited Create;

    {Create handle}
    fHandle := CreateFile(PChar(Filename), OpenMode, FILE_SHARE_READ or
      FILE_SHARE_WRITE, nil, IsCreate[fsmCreate in Mode], 0, 0);
    {Store mode}
    FileMode := Mode;
  end;

  {Filestream object being destroyed}
  destructor TFileStream.Destroy;
  begin
    {Terminates file and close}
    if FileMode = [fsmWrite] then
      SetEndOfFile(fHandle);
    CloseHandle(fHandle);

    {Call ancestor}
    inherited Destroy;
  end;

  {Writes data to the file}
  function TFileStream.Write(const Buffer; Count: Longint): Cardinal;
  begin
    if not WriteFile(fHandle, Buffer, Count, Result, nil) then
      Result := 0;
  end;

  {Reads data from the file}
  function TFileStream.Read(var Buffer; Count: Longint): Cardinal;
  begin
    if not ReadFile(fHandle, Buffer, Count, Result, nil) then
      Result := 0;
  end;

  {Seeks the file position}
  function TFileStream.Seek(Offset: Integer; Origin: Word): Longint;
  begin
    Result := SetFilePointer(fHandle, Offset, nil, Origin);
  end;

  {Sets the size of the file}
  procedure TFileStream.SetSize(const Value: Longint);
  begin
    Seek(Value, soFromBeginning);
    SetEndOfFile(fHandle);
  end;

  {TResourceStream implementation}

  {Creates the resource stream}
  constructor TResourceStream.Create(Instance: HInst; const ResName: String;
    ResType: PChar);
  var
    ResID: HRSRC;
    ResGlobal: HGlobal;
  begin
    {Obtains the resource ID}
    ResID := FindResource(hInstance, PChar(ResName), RT_RCDATA);
    if ResID = 0 then raise EPNGError.Create('');
    {Obtains memory and size}
    ResGlobal := LoadResource(hInstance, ResID);
    Size := SizeOfResource(hInstance, ResID);
    Memory := LockResource(ResGlobal);
    if (ResGlobal = 0) or (Memory = nil) then EPNGError.Create('');
  end;


  {Setting resource stream size is not supported}
  procedure TResourceStream.SetSize(const Value: Integer);
  begin
  end;

  {Writing into a resource stream is not supported}
  function TResourceStream.Write(const Buffer; Count: Integer): Cardinal;
  begin
    Result := 0;
  end;

  {Reads data from the stream}
  function TResourceStream.Read(var Buffer; Count: Integer): Cardinal;
  begin
    //Returns data
    CopyMemory(@Buffer, Ptr(Longint(Memory) + Position), Count);
    //Update position
    inc(Position, Count);
    //Returns
    Result := Count;
  end;

  {Seeks data}
  function TResourceStream.Seek(Offset: Integer; Origin: Word): Longint;
  begin
    {Move depending on the origin}
    case Origin of
      soFromBeginning: Position := Offset;
      soFromCurrent: inc(Position, Offset);
      soFromEnd: Position := Size + Offset;
    end;

    {Returns the current position}
    Result := Position;
  end;

{$ENDIF}

{TChunk implementation}

{Resizes the data}
procedure TChunk.ResizeData(const NewSize: Cardinal);
begin
  fDataSize := NewSize;
  ReallocMem(fData, NewSize + 1);
end;

{Returns index from list}
function TChunk.GetIndex: Integer;
var
  i: Integer;
begin
  Result := -1; {Avoiding warnings}
  {Searches in the list}
  FOR i := 0 TO Owner.Chunks.Count - 1 DO
    if Owner.Chunks.Item[i] = Self then
    begin
      {Found match}
      Result := i;
      exit;
    end {for i}
end;

{Returns pointer to the TChunkIHDR}
function TChunk.GetHeader: TChunkIHDR;
begin
  Result := Owner.Chunks.Item[0] as TChunkIHDR;
end;

{Assigns from another TChunk}
procedure TChunk.Assign(Source: TChunk);
begin
  {Copy properties}
  fName := Source.fName;
  {Set data size and realloc}
  ResizeData(Source.fDataSize);

  {Copy data (if there's any)}
  if fDataSize > 0 then CopyMemory(fData, Source.fData, fDataSize);
end;

{Chunk being created}
constructor TChunk.Create(Owner: TPngObject);
var
  ChunkName: String;
begin
  {Ancestor create}
  inherited Create;

  {If it's a registered class, set the chunk name based on the class}
  {name. For instance, if the class name is TChunkgAMA, the GAMA part}
  {will become the chunk name}
  ChunkName := Copy(ClassName, Length('TChunk') + 1, Length(ClassName));
  if Length(ChunkName) = 4 then CopyMemory(@fName[0], @ChunkName[1], 4);

  {Initialize data holder}
  GetMem(fData, 1);
  fDataSize := 0;
  {Record owner}
  fOwner := Owner;
end;

{Chunk being destroyed}
destructor TChunk.Destroy;
begin
  {Free data holder}
  FreeMem(fData, fDataSize + 1);
  {Let ancestor destroy}
  inherited Destroy;
end;

{Returns the chunk name 1}
function TChunk.GetChunkName: String;
begin
  Result := fName
end;

{Returns the chunk name 2}
class function TChunk.GetName: String;
begin
  {For avoid writing GetName for each TChunk descendent, by default for}
  {classes which don't declare GetName, it will look for the class name}
  {to extract the chunk kind. Example, if the class name is TChunkIEND }
  {this method extracts and returns IEND}
  Result := Copy(ClassName, Length('TChunk') + 1, Length(ClassName));
end;

{Saves the data to the stream}
function TChunk.SaveData(Stream: TStream): Boolean;
var
  ChunkSize, ChunkCRC: Cardinal;
begin
  {First, write the size for the following data in the chunk}
  ChunkSize := ByteSwap(DataSize);
  Stream.Write(ChunkSize, 4);
  {The chunk name}
  Stream.Write(fName, 4);
  {If there is data for the chunk, write it}
  if DataSize > 0 then Stream.Write(Data^, DataSize);
  {Calculates and write CRC}
  ChunkCRC := update_crc($ffffffff, @fName[0], 4);
  ChunkCRC := Byteswap(update_crc(ChunkCRC, Data, DataSize) xor $ffffffff);
  Stream.Write(ChunkCRC, 4);

  {Returns that everything went ok}
  Result := TRUE;
end;

{Saves the chunk to the stream}
function TChunk.SaveToStream(Stream: TStream): Boolean;
begin
  Result := SaveData(Stream)
end;


{Loads the chunk from a stream}
function TChunk.LoadFromStream(Stream: TStream; const ChunkName: TChunkName;
  Size: Integer): Boolean;
var
  CheckCRC: Cardinal;
  {$IFDEF CheckCRC}RightCRC: Cardinal;{$ENDIF}
begin
  {Copies data from source}
  ResizeData(Size);
  if Size > 0 then Stream.Read(fData^, Size);
  {Reads CRC}
  Stream.Read(CheckCRC, 4);
  CheckCrc := ByteSwap(CheckCRC);

  {Check if crc readed is valid}
  {$IFDEF CheckCRC}
    RightCRC := update_crc($ffffffff, @ChunkName[0], 4);
    RightCRC := update_crc(RightCRC, fData, Size) xor $ffffffff;
    Result := RightCRC = CheckCrc;

    {Handle CRC error}
    if not Result then
    begin
      {In case it coult not load chunk}
      Owner.RaiseError(EPngInvalidCRC, EPngInvalidCRCText);
      exit;
    end
  {$ELSE}Result := TRUE; {$ENDIF}

end;

{TChunktIME implementation}

{Chunk being loaded from a stream}
function TChunktIME.LoadFromStream(Stream: TStream;
  const ChunkName: TChunkName; Size: Integer): Boolean;
begin
  {Let ancestor load the data}
  Result := inherited LoadFromStream(Stream, ChunkName, Size);
  if not Result or (Size <> 7) then exit; {Size must be 7}

  {Reads data}
  fYear := ((pByte(Longint(Data) )^) * 256)+ (pByte(Longint(Data) + 1)^);
  fMonth := pByte(Longint(Data) + 2)^;
  fDay := pByte(Longint(Data) + 3)^;
  fHour := pByte(Longint(Data) + 4)^;
  fMinute := pByte(Longint(Data) + 5)^;
  fSecond := pByte(Longint(Data) + 6)^;
end;

{Saving the chunk to a stream}
function TChunktIME.SaveToStream(Stream: TStream): Boolean;
begin
  {Update data}
  ResizeData(7);  {Make sure the size is 7}
  pWord(Data)^ := Year;
  pByte(Longint(Data) + 2)^ := Month;
  pByte(Longint(Data) + 3)^ := Day;
  pByte(Longint(Data) + 4)^ := Hour;
  pByte(Longint(Data) + 5)^ := Minute;
  pByte(Longint(Data) + 6)^ := Second;

  {Let inherited save data}
  Result := inherited SaveToStream(Stream);
end;

{TChunkztXt implementation}

{Loading the chunk from a stream}
function TChunkzTXt.LoadFromStream(Stream: TStream;
  const ChunkName: TChunkName; Size: Integer): Boolean;
var
  ErrorOutput: String;
  CompressionMethod: Byte;
  Output: Pointer;
  OutputSize: Integer;
begin
  {Load data from stream and validate}
  Result := inherited LoadFromStream(Stream, ChunkName, Size);
  if not Result or (Size < 4) then exit;
  fKeyword := PChar(Data);  {Get keyword and compression method bellow}
  CompressionMethod := pByte(Longint(fKeyword) + Length(fKeyword))^;
  fText := '';

  {In case the compression is 0 (only one accepted by specs), reads it}
  if CompressionMethod = 0 then
  begin
    Output := nil;
    if DecompressZLIB(PChar(Longint(Data) + Length(fKeyword) + 2),
      Size - Length(fKeyword) - 2, Output, OutputSize, ErrorOutput) then
    begin
      SetLength(fText, OutputSize);
      CopyMemory(@fText[1], Output, OutputSize);
    end {if DecompressZLIB(...};
    FreeMem(Output);
  end {if CompressionMethod = 0}

end;

{Saving the chunk to a stream}
function TChunkztXt.SaveToStream(Stream: TStream): Boolean;
var
  Output: Pointer;
  OutputSize: Integer;
  ErrorOutput: String;
begin
  Output := nil; {Initializes output}
  if fText = '' then fText := ' ';

  {Compresses the data}
  if CompressZLIB(@fText[1], Length(fText), Owner.CompressionLevel, Output,
    OutputSize, ErrorOutput) then
  begin
    {Size is length from keyword, plus a null character to divide}
    {plus the compression method, plus the length of the text (zlib compressed)}
    ResizeData(Length(fKeyword) + 2 + OutputSize);

    Fillchar(Data^, DataSize, #0);
    {Copies the keyword data}
    if Keyword <> '' then
      CopyMemory(Data, @fKeyword[1], Length(Keyword));
    {Compression method 0 (inflate/deflate)}
    pByte(Ptr(Longint(Data) + Length(Keyword) + 1))^ := 0;
    if OutputSize > 0 then
      CopyMemory(Ptr(Longint(Data) + Length(Keyword) + 2), Output, OutputSize);

    {Let ancestor calculate crc and save}
    Result := SaveData(Stream);
  end {if CompressZLIB(...} else Result := False;

  {Frees output}
  if Output <> nil then FreeMem(Output)
end;

{TChunktEXt implementation}

{Assigns from another text chunk}
procedure TChunktEXt.Assign(Source: TChunk);
begin
  fKeyword := TChunktEXt(Source).fKeyword;
  fText := TChunktEXt(Source).fText;
end;

{Loading the chunk from a stream}
function TChunktEXt.LoadFromStream(Stream: TStream;
  const ChunkName: TChunkName; Size: Integer): Boolean;
begin
  {Load data from stream and validate}
  Result := inherited LoadFromStream(Stream, ChunkName, Size);
  if not Result or (Size < 3) then exit;
  {Get text}
  fKeyword := PChar(Data);
  SetLength(fText, Size - Length(fKeyword) - 1);
  CopyMemory(@fText[1], Ptr(Longint(Data) + Length(fKeyword) + 1),
    Length(fText));
end;

{Saving the chunk to a stream}
function TChunktEXt.SaveToStream(Stream: TStream): Boolean;
begin
  {Size is length from keyword, plus a null character to divide}
  {plus the length of the text}
  ResizeData(Length(fKeyword) + 1 + Length(fText));
  Fillchar(Data^, DataSize, #0);
  {Copy data}
  if Keyword <> '' then
    CopyMemory(Data, @fKeyword[1], Length(Keyword));
  if Text <> '' then
    CopyMemory(Ptr(Longint(Data) + Length(Keyword) + 1), @fText[1],
      Length(Text));
  {Let ancestor calculate crc and save}
  Result := inherited SaveToStream(Stream);
end;


{TChunkIHDR implementation}

{Chunk being created}
constructor TChunkIHDR.Create(Owner: TPngObject);
begin
  {Call inherited}
  inherited Create(Owner);
  {Prepare pointers}
  ImageHandle := 0;
  ImageDC := 0;
end;

{Chunk being destroyed}
destructor TChunkIHDR.Destroy;
begin
  {Free memory}
  FreeImageData();

  {Calls TChunk destroy}
  inherited Destroy;
end;

{Assigns from another IHDR chunk}
procedure TChunkIHDR.Assign(Source: TChunk);
begin
  {Copy the IHDR data}
  if Source is TChunkIHDR then
  begin
    {Copy IHDR values}
    IHDRData := TChunkIHDR(Source).IHDRData;

    {Prepare to hold data by filling BitmapInfo structure and}
    {resizing ImageData and ImageAlpha memory allocations}
    PrepareImageData();

    {Copy image data}
    CopyMemory(ImageData, TChunkIHDR(Source).ImageData,
      BytesPerRow * Integer(Height));
    CopyMemory(ImageAlpha, TChunkIHDR(Source).ImageAlpha,
      Integer(Width) * Integer(Height));

    {Copy palette colors}
    BitmapInfo.bmiColors := TChunkIHDR(Source).BitmapInfo.bmiColors;
  end
  else
    Owner.RaiseError(EPNGError, EPNGCannotAssignChunkText);
end;

{Release allocated image data}
procedure TChunkIHDR.FreeImageData;
begin
  {Free old image data}
  if ImageHandle <> 0  then DeleteObject(ImageHandle);
  if ImageDC     <> 0  then DeleteDC(ImageDC);
  if ImageAlpha <> nil then FreeMem(ImageAlpha);
  {$IFDEF Store16bits}
  if ExtraImageData <> nil then FreeMem(ExtraImageData);
  {$ENDIF}
  ImageHandle := 0; ImageDC := 0; ImageAlpha := nil; ImageData := nil;
end;

{Chunk being loaded from a stream}
function TChunkIHDR.LoadFromStream(Stream: TStream; const ChunkName: TChunkName;
  Size: Integer): Boolean;
begin
  {Let TChunk load it}
  Result := inherited LoadFromStream(Stream, ChunkName, Size);
  if not Result then Exit;

  {Now check values}
  {Note: It's recommended by png specification to make sure that the size}
  {must be 13 bytes to be valid, but some images with 14 bytes were found}
  {which could be loaded by internet explorer and other tools}
  if (fDataSize < SIZEOF(TIHdrData)) then
  begin
    {Ihdr must always have at least 13 bytes}
    Result := False;
    Owner.RaiseError(EPNGInvalidIHDR, EPNGInvalidIHDRText);
    exit;
  end;

  {Everything ok, reads IHDR}
  IHDRData := pIHDRData(fData)^;
  IHDRData.Width := ByteSwap(IHDRData.Width);
  IHDRData.Height := ByteSwap(IHDRData.Height);

  {The width and height must not be larger than 65535 pixels}
  if (IHDRData.Width > High(Word)) or (IHDRData.Height > High(Word)) then
  begin
    Result := False;
    Owner.RaiseError(EPNGSizeExceeds, EPNGSizeExceedsText);
    exit;
  end {if IHDRData.Width > High(Word)};
  {Compression method must be 0 (inflate/deflate)}
  if (IHDRData.CompressionMethod <> 0) then
  begin
    Result := False;
    Owner.RaiseError(EPNGUnknownCompression, EPNGUnknownCompressionText);
    exit;
  end;
  {Interlace must be either 0 (none) or 7 (adam7)}
  if (IHDRData.InterlaceMethod <> 0) and (IHDRData.InterlaceMethod <> 1) then
  begin
    Result := False;
    Owner.RaiseError(EPNGUnknownInterlace, EPNGUnknownInterlaceText);
    exit;
  end;

  {Updates owner properties}
  Owner.InterlaceMethod := TInterlaceMethod(IHDRData.InterlaceMethod);

  {Prepares data to hold image}
  PrepareImageData();
end;

{Saving the IHDR chunk to a stream}
function TChunkIHDR.SaveToStream(Stream: TStream): Boolean;
begin
  {Ignore 2 bits images}
  if BitDepth = 2 then BitDepth := 4;

  {It needs to do is update the data with the IHDR data}
  {structure containing the write values}
  ResizeData(SizeOf(TIHDRData));
  pIHDRData(fData)^ := IHDRData;
  {..byteswap 4 byte types}
  pIHDRData(fData)^.Width := ByteSwap(pIHDRData(fData)^.Width);
  pIHDRData(fData)^.Height := ByteSwap(pIHDRData(fData)^.Height);
  {..update interlace method}
  pIHDRData(fData)^.InterlaceMethod := Byte(Owner.InterlaceMethod);
  {..and then let the ancestor SaveToStream do the hard work}
  Result := inherited SaveToStream(Stream);
end;

{Resizes the image data to fill the color type, bit depth, }
{width and height parameters}
procedure TChunkIHDR.PrepareImageData();

  {Set the bitmap info}
  procedure SetInfo(const Bitdepth: Integer; const Palette: Boolean);
  begin

    {Copy if the bitmap contain palette entries}
    HasPalette := Palette;
    {Initialize the structure with zeros}
    fillchar(BitmapInfo, sizeof(BitmapInfo), #0);
    {Fill the strucutre}
    with BitmapInfo.bmiHeader do
    begin
      biSize := sizeof(TBitmapInfoHeader);
      biHeight := Height;
      biWidth := Width;
      biPlanes := 1;
      biBitCount := BitDepth;
      biCompression := BI_RGB;
    end {with BitmapInfo.bmiHeader}
  end;
begin
  {Prepare bitmap info header}
  Fillchar(BitmapInfo, sizeof(TMaxBitmapInfo), #0);
  {Release old image data}
  FreeImageData();

  {Obtain number of bits for each pixel}
  case ColorType of
    COLOR_GRAYSCALE, COLOR_PALETTE, COLOR_GRAYSCALEALPHA:
      case BitDepth of
        {These are supported by windows}
        1, 4, 8: SetInfo(BitDepth, TRUE);
        {2 bits for each pixel is not supported by windows bitmap}
        2      : SetInfo(4, TRUE);
        {Also 16 bits (2 bytes) for each pixel is not supported}
        {and should be transormed into a 8 bit grayscale}
        16     : SetInfo(8, TRUE);
      end;
    {Only 1 byte (8 bits) is supported}
    COLOR_RGB, COLOR_RGBALPHA:  SetInfo(24, FALSE);
  end {case ColorType};
  {Number of bytes for each scanline}
  BytesPerRow := (((BitmapInfo.bmiHeader.biBitCount * Width) + 31)
    and not 31) div 8;

  {Build array for alpha information, if necessary}
  if (ColorType = COLOR_RGBALPHA) or (ColorType = COLOR_GRAYSCALEALPHA) then
  begin
    GetMem(ImageAlpha, Integer(Width) * Integer(Height));
    FillChar(ImageAlpha^, Integer(Width) * Integer(Height), #0);
  end;

  {Build array for extra byte information}
  {$IFDEF Store16bits}
  if (BitDepth = 16) then
  begin
    GetMem(ExtraImageData, BytesPerRow * Integer(Height));
    FillChar(ExtraImageData^, BytesPerRow * Integer(Height), #0);
  end;
  {$ENDIF}

  {Creates the image to hold the data, CreateDIBSection does a better}
  {work in allocating necessary memory}
  ImageDC := CreateCompatibleDC(0);
  ImageHandle := CreateDIBSection(ImageDC, pBitmapInfo(@BitmapInfo)^,
    DIB_RGB_COLORS, ImageData, 0, 0);

  {Clears the old palette (if any)}
  with Owner do
    if  TempPalette <> 0 then
    begin
      DeleteObject(TempPalette);
      TempPalette := 0;
    end {with Owner, if TempPalette <> 0};

  {Build array and allocate bytes for each row}
  zeromemory(ImageData, BytesPerRow * Integer(Height));
end;

{TChunktRNS implementation}

{$IFNDEF UseDelphi}
function CompareMem(P1, P2: pByte; const Size: Integer): Boolean;
var i: Integer;
begin
  Result := True;
  for i := 1 to Size do
  begin
    if P1^ <> P2^ then Result := False;
    inc(P1); inc(P2);
  end {for i}
end;
{$ENDIF}

{Sets the transpararent color}
procedure TChunktRNS.SetTransparentColor(const Value: ColorRef);
var
  i: Byte;
  LookColor: TRGBQuad;
begin
  {Clears the palette values}
  Fillchar(PaletteValues, SizeOf(PaletteValues), #0);
  {Sets that it uses bit transparency}
  fBitTransparency := True;


  {Depends on the color type}
  with Header do
    case ColorType of
      COLOR_GRAYSCALE:
      begin
        Self.ResizeData(2);
        pWord(@PaletteValues[0])^ := ByteSwap16(GetRValue(Value));
      end;
      COLOR_RGB:
      begin
        Self.ResizeData(6);
        pWord(@PaletteValues[0])^ := ByteSwap16(GetRValue(Value));
        pWord(@PaletteValues[2])^ := ByteSwap16(GetGValue(Value));
        pWord(@PaletteValues[4])^ := ByteSwap16(GetBValue(Value));
      end;
      COLOR_PALETTE:
      begin
        {Creates a RGBQuad to search for the color}
        LookColor.rgbRed := GetRValue(Value);
        LookColor.rgbGreen := GetGValue(Value);
        LookColor.rgbBlue := GetBValue(Value);
        {Look in the table for the entry}
        for i := 0 to 255 do
          if CompareMem(@BitmapInfo.bmiColors[i], @LookColor, 3) then
            Break;
        {Fill the transparency table}
        Fillchar(PaletteValues, i, 255);
        Self.ResizeData(i + 1)

      end
    end {case / with};

end;

{Returns the transparent color for the image}
function TChunktRNS.GetTransparentColor: ColorRef;
var
  PaletteChunk: TChunkPLTE;
  i: Integer;
begin
  Result := 0; {Default: Unknown transparent color}

  {Depends on the color type}
  with Header do
    case ColorType of
      COLOR_GRAYSCALE:
          Result := RGB(PaletteValues[0], PaletteValues[0],
        PaletteValues[0]);
      COLOR_RGB:
          Result := RGB(PaletteValues[1], PaletteValues[3], PaletteValues[5]);
      COLOR_PALETTE:
      begin
        {Obtains the palette chunk}
        PaletteChunk := Owner.Chunks.ItemFromClass(TChunkPLTE) as TChunkPLTE;

        {Looks for an entry with 0 transparency meaning that it is the}
        {full transparent entry}
        for i := 0 to Self.DataSize - 1 do
          if PaletteValues[i] = 0 then
            with PaletteChunk.GetPaletteItem(i) do
            begin
              Result := RGB(rgbRed, rgbGreen, rgbBlue);
              break
            end
      end {COLOR_PALETTE}
    end {case Header.ColorType};
end;

{Saving the chunk to a stream}
function TChunktRNS.SaveToStream(Stream: TStream): Boolean;
begin
  {Copy palette into data buffer}
  if DataSize <= 256 then
    CopyMemory(fData, @PaletteValues[0], DataSize);

  Result := inherited SaveToStream(Stream);
end;

{Assigns from another chunk}
procedure TChunktRNS.Assign(Source: TChunk);
begin
  CopyMemory(@PaletteValues[0], @TChunkTrns(Source).PaletteValues[0], 256);
  fBitTransparency := TChunkTrns(Source).fBitTransparency;
  inherited Assign(Source);
end;

{Loads the chunk from a stream}
function TChunktRNS.LoadFromStream(Stream: TStream; const ChunkName: TChunkName;
  Size: Integer): Boolean;
var
  i, Differ255: Integer;
begin
  {Let inherited load}
  Result := inherited LoadFromStream(Stream, ChunkName, Size);

  if not Result then Exit;

  {Make sure size is correct}
  if Size > 256 then Owner.RaiseError(EPNGInvalidPalette,
    EPNGInvalidPaletteText);

  {The unset items should have value 255}
  Fillchar(PaletteValues[0], 256, 255);
  {Copy the other values}
  CopyMemory(@PaletteValues[0], fData, Size);

  {Create the mask if needed}
  case Header.ColorType of
    {Mask for grayscale and RGB}
    COLOR_RGB, COLOR_GRAYSCALE: fBitTransparency := True;
    COLOR_PALETTE:
    begin
      Differ255 := 0; {Count the entries with a value different from 255}
      {Tests if it uses bit transparency}
      for i := 0 to Size - 1 do
        if PaletteValues[i] <> 255 then inc(Differ255);

      {If it has one value different from 255 it is a bit transparency}
      fBitTransparency := (Differ255 = 1);
    end {COLOR_PALETTE}
  end {case Header.ColorType};

end;

{Prepares the image palette}
procedure TChunkIDAT.PreparePalette;
var
  Entries: Word;
  j      : Integer;
begin
  {In case the image uses grayscale, build a grayscale palette}
  with Header do
    if (ColorType = COLOR_GRAYSCALE) or (ColorType = COLOR_GRAYSCALEALPHA) then
    begin
      {Calculate total number of palette entries}
      Entries := (1 shl Byte(BitmapInfo.bmiHeader.biBitCount));

      FOR j := 0 TO Entries - 1 DO
        with BitmapInfo.bmiColors[j] do
        begin

          {Calculate each palette entry}
          rgbRed := fOwner.GammaTable[MulDiv(j, 255, Entries - 1)];
          rgbGreen := rgbRed;
          rgbBlue := rgbRed;
        end {with BitmapInfo.bmiColors[j]}
    end {if ColorType = COLOR_GRAYSCALE..., with Header}
end;

{Reads from ZLIB}
function TChunkIDAT.IDATZlibRead(var ZLIBStream: TZStreamRec2;
  Buffer: Pointer; Count: Integer; var EndPos: Integer;
  var crcfile: Cardinal): Integer;
var
  ProcResult : Integer;
  IDATHeader : Array[0..3] of char;
  IDATCRC    : Cardinal;
begin
  {Uses internal record pointed by ZLIBStream to gather information}
  with ZLIBStream, ZLIBStream.zlib do
  begin
    {Set the buffer the zlib will read into}
    next_out := Buffer;
    avail_out := Count;

    {Decode until it reach the Count variable}
    while avail_out > 0 do
    begin
      {In case it needs more data and it's in the end of a IDAT chunk,}
      {it means that there are more IDAT chunks}
      if (fStream.Position = EndPos) and (avail_out > 0) and
        (avail_in = 0) then
      begin
        {End this chunk by reading and testing the crc value}
        fStream.Read(IDATCRC, 4);

        {$IFDEF CheckCRC}
          if crcfile xor $ffffffff <> Cardinal(ByteSwap(IDATCRC)) then
          begin
            Result := -1;
            Owner.RaiseError(EPNGInvalidCRC, EPNGInvalidCRCText);
            exit;
          end;
        {$ENDIF}

        {Start reading the next chunk}
        fStream.Read(EndPos, 4);        {Reads next chunk size}
        fStream.Read(IDATHeader[0], 4); {Next chunk header}
        {It must be a IDAT chunk since image data is required and PNG}
        {specification says that multiple IDAT chunks must be consecutive}
        if IDATHeader <> 'IDAT' then
        begin
          Owner.RaiseError(EPNGMissingMultipleIDAT, EPNGMissingMultipleIDATText);
          result := -1;
          exit;
        end;

        {Calculate chunk name part of the crc}
        {$IFDEF CheckCRC}
          crcfile := update_crc($ffffffff, @IDATHeader[0], 4);
        {$ENDIF}
        EndPos := fStream.Position + ByteSwap(EndPos);
      end;


      {In case it needs compressed data to read from}
      if avail_in = 0 then
      begin
        {In case it's trying to read more than it is avaliable}
        if fStream.Position + ZLIBAllocate > EndPos then
          avail_in := fStream.Read(Data^, EndPos - fStream.Position)
         else
          avail_in := fStream.Read(Data^, ZLIBAllocate);
        {Update crc}
        {$IFDEF CheckCRC}
          crcfile := update_crc(crcfile, Data, avail_in);
        {$ENDIF}

        {In case there is no more compressed data to read from}
        if avail_in = 0 then
        begin
          Result := Count - avail_out;
          Exit;
        end;

        {Set next buffer to read and record current position}
        next_in := Data;

      end {if avail_in = 0};

      ProcResult := inflate(zlib, 0);

      {In case the result was not sucessfull}
      if (ProcResult < 0) then
      begin
        Result := -1;
        Owner.RaiseError(EPNGZLIBError,
          EPNGZLIBErrorText + zliberrors[procresult]);
        exit;
      end;

    end {while avail_out > 0};

  end {with};

  {If everything gone ok, it returns the count bytes}
  Result := Count;
end;

{TChunkIDAT implementation}

const
  {Adam 7 interlacing values}
  RowStart: array[0..6] of Integer = (0, 0, 4, 0, 2, 0, 1);
  ColumnStart: array[0..6] of Integer = (0, 4, 0, 2, 0, 1, 0);
  RowIncrement: array[0..6] of Integer = (8, 8, 8, 4, 4, 2, 2);
  ColumnIncrement: array[0..6] of Integer = (8, 8, 4, 4, 2, 2, 1);

{Copy interlaced images with 1 byte for R, G, B}
procedure TChunkIDAT.CopyInterlacedRGB8(const Pass: Byte;
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Dest := pChar(Longint(Dest) + Col * 3);
  repeat
    {Copy this row}
    Byte(Dest^) := fOwner.GammaTable[pByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^) := fOwner.GammaTable[pByte(Longint(Src) + 1)^]; inc(Dest);
    Byte(Dest^) := fOwner.GammaTable[pByte(Longint(Src)    )^]; inc(Dest);

    {Move to next column}
    inc(Src, 3);
    inc(Dest, ColumnIncrement[Pass] * 3 - 3);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Copy interlaced images with 2 bytes for R, G, B}
procedure TChunkIDAT.CopyInterlacedRGB16(const Pass: Byte;
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Dest := pChar(Longint(Dest) + Col * 3);
  repeat
    {Copy this row}
    Byte(Dest^) := Owner.GammaTable[pByte(Longint(Src) + 4)^]; inc(Dest);
    Byte(Dest^) := Owner.GammaTable[pByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^) := Owner.GammaTable[pByte(Longint(Src)    )^]; inc(Dest);
    {$IFDEF Store16bits}
    {Copy extra pixel values}
    Byte(Extra^) := fOwner.GammaTable[pByte(Longint(Src) + 5)^]; inc(Extra);
    Byte(Extra^) := fOwner.GammaTable[pByte(Longint(Src) + 3)^]; inc(Extra);
    Byte(Extra^) := fOwner.GammaTable[pByte(Longint(Src) + 1)^]; inc(Extra);
    {$ENDIF}

    {Move to next column}
    inc(Src, 6);
    inc(Dest, ColumnIncrement[Pass] * 3 - 3);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Copy ímages with palette using bit depths 1, 4 or 8}
procedure TChunkIDAT.CopyInterlacedPalette148(const Pass: Byte;
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
const
  BitTable: Array[1..8] of Integer = ($1, $3, 0, $F, 0, 0, 0, $FF);
  StartBit: Array[1..8] of Integer = (7 , 0 , 0, 4,  0, 0, 0, 0);
var
  CurBit, Col: Integer;
  Dest2: PChar;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  repeat
    {Copy data}
    CurBit := StartBit[Header.BitDepth];
    repeat
      {Adjust pointer to pixel byte bounds}
      Dest2 := pChar(Longint(Dest) + (Header.BitDepth * Col) div 8);
      {Copy data}
      Byte(Dest2^) := Byte(Dest2^) or
        ( ((Byte(Src^) shr CurBit) and BitTable[Header.BitDepth])
          shl (StartBit[Header.BitDepth] - (Col * Header.BitDepth mod 8)));

      {Move to next column}
      inc(Col, ColumnIncrement[Pass]);
      {Will read next bits}
      dec(CurBit, Header.BitDepth);
    until CurBit < 0;

    {Move to next byte in source}
    inc(Src);
  until Col >= ImageWidth;
end;

{Copy ímages with palette using bit depth 2}
procedure TChunkIDAT.CopyInterlacedPalette2(const Pass: Byte; Src, Dest,
  Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
var
  CurBit, Col: Integer;
  Dest2: PChar;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  repeat
    {Copy data}
    CurBit := 6;
    repeat
      {Adjust pointer to pixel byte bounds}
      Dest2 := pChar(Longint(Dest) + Col div 2);
      {Copy data}
      Byte(Dest2^) := Byte(Dest2^) or (((Byte(Src^) shr CurBit) and $3)
         shl (4 - (4 * Col) mod 8));
      {Move to next column}
      inc(Col, ColumnIncrement[Pass]);
      {Will read next bits}
      dec(CurBit, 2);
    until CurBit < 0;

    {Move to next byte in source}
    inc(Src);
  until Col >= ImageWidth;
end;

{Copy ímages with grayscale using bit depth 2}
procedure TChunkIDAT.CopyInterlacedGray2(const Pass: Byte;
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
var
  CurBit, Col: Integer;
  Dest2: PChar;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  repeat
    {Copy data}
    CurBit := 6;
    repeat
      {Adjust pointer to pixel byte bounds}
      Dest2 := pChar(Longint(Dest) + Col div 2);
      {Copy data}
      Byte(Dest2^) := Byte(Dest2^) or ((((Byte(Src^) shr CurBit) shl 2) and $F)
         shl (4 - (Col*4) mod 8));
      {Move to next column}
      inc(Col, ColumnIncrement[Pass]);
      {Will read next bits}
      dec(CurBit, 2);
    until CurBit < 0;

    {Move to next byte in source}
    inc(Src);
  until Col >= ImageWidth;
end;

{Copy ímages with palette using 2 bytes for each pixel}
procedure TChunkIDAT.CopyInterlacedGrayscale16(const Pass: Byte;
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Dest := pChar(Longint(Dest) + Col);
  repeat
    {Copy this row}
    Dest^ := Src^; inc(Dest);
    {$IFDEF Store16bits}
    Extra^ := pChar(Longint(Src) + 1)^; inc(Extra);
    {$ENDIF}

    {Move to next column}
    inc(Src, 2);
    inc(Dest, ColumnIncrement[Pass] - 1);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Decodes interlaced RGB alpha with 1 byte for each sample}
procedure TChunkIDAT.CopyInterlacedRGBAlpha8(const Pass: Byte;
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Dest := pChar(Longint(Dest) + Col * 3);
  Trans := pChar(Longint(Trans) + Col);
  repeat
    {Copy this row and alpha value}
    Trans^ := pChar(Longint(Src) + 3)^;
    Byte(Dest^)  := fOwner.GammaTable[pByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^)  := fOwner.GammaTable[pByte(Longint(Src) + 1)^]; inc(Dest);
    Byte(Dest^)  := fOwner.GammaTable[pByte(Longint(Src)    )^]; inc(Dest);

    {Move to next column}
    inc(Src, 4);
    inc(Dest, ColumnIncrement[Pass] * 3 - 3);
    inc(Trans, ColumnIncrement[Pass]);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Decodes interlaced RGB alpha with 2 bytes for each sample}
procedure TChunkIDAT.CopyInterlacedRGBAlpha16(const Pass: Byte;
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Dest := pChar(Longint(Dest) + Col * 3);
  Trans := pChar(Longint(Trans) + Col);
  repeat
    {Copy this row and alpha value}
    Trans^ := pChar(Longint(Src) + 6)^;
    Byte(Dest^)  := fOwner.GammaTable[pByte(Longint(Src) + 4)^]; inc(Dest);
    Byte(Dest^)  := fOwner.GammaTable[pByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^)  := fOwner.GammaTable[pByte(Longint(Src)    )^]; inc(Dest);
    {$IFDEF Store16bits}
    {Copy extra pixel values}
    Byte(Extra^) := fOwner.GammaTable[pByte(Longint(Src) + 5)^]; inc(Extra);
    Byte(Extra^) := fOwner.GammaTable[pByte(Longint(Src) + 3)^]; inc(Extra);
    Byte(Extra^) := fOwner.GammaTable[pByte(Longint(Src) + 1)^]; inc(Extra);
    {$ENDIF}

    {Move to next column}
    inc(Src, 8);
    inc(Dest, ColumnIncrement[Pass] * 3 - 3);
    inc(Trans, ColumnIncrement[Pass]);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Decodes 8 bit grayscale image followed by an alpha sample}
procedure TChunkIDAT.CopyInterlacedGrayscaleAlpha8(const Pass: Byte;
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
var
  Col: Integer;
begin
  {Get first column, pointers to the data and enter in loop}
  Col := ColumnStart[Pass];
  Dest := pChar(Longint(Dest) + Col);
  Trans := pChar(Longint(Trans) + Col);
  repeat
    {Copy this grayscale value and alpha}
    Dest^ := Src^;  inc(Src);
    Trans^ := Src^; inc(Src);

    {Move to next column}
    inc(Dest, ColumnIncrement[Pass]);
    inc(Trans, ColumnIncrement[Pass]);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Decodes 16 bit grayscale image followed by an alpha sample}
procedure TChunkIDAT.CopyInterlacedGrayscaleAlpha16(const Pass: Byte;
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
var
  Col: Integer;
begin
  {Get first column, pointers to the data and enter in loop}
  Col := ColumnStart[Pass];
  Dest := pChar(Longint(Dest) + Col);
  Trans := pChar(Longint(Trans) + Col);
  repeat
    {$IFDEF Store16bits}
    Extra^ := pChar(Longint(Src) + 1)^; inc(Extra);
    {$ENDIF}
    {Copy this grayscale value and alpha, transforming 16 bits into 8}
    Dest^ := Src^;  inc(Src, 2);
    Trans^ := Src^; inc(Src, 2);

    {Move to next column}
    inc(Dest, ColumnIncrement[Pass]);
    inc(Trans, ColumnIncrement[Pass]);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Decodes an interlaced image}
procedure TChunkIDAT.DecodeInterlacedAdam7(Stream: TStream;
  var ZLIBStream: TZStreamRec2; const Size: Integer; var crcfile: Cardinal);
var
  CurrentPass: Byte;
  PixelsThisRow: Integer;
  CurrentRow: Integer;
  Trans, Data{$IFDEF Store16bits}, Extra{$ENDIF}: pChar;
  CopyProc: procedure(const Pass: Byte; Src, Dest,
    Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar) of object;
begin

  CopyProc := nil; {Initialize}
  {Determine method to copy the image data}
  case Header.ColorType of
    {R, G, B values for each pixel}
    COLOR_RGB:
      case Header.BitDepth of
        8:  CopyProc := CopyInterlacedRGB8;
       16:  CopyProc := CopyInterlacedRGB16;
      end {case Header.BitDepth};
    {Palette}
    COLOR_PALETTE, COLOR_GRAYSCALE:
      case Header.BitDepth of
        1, 4, 8: CopyProc := CopyInterlacedPalette148;
        2      : if Header.ColorType = COLOR_PALETTE then
                   CopyProc := CopyInterlacedPalette2
                 else
                   CopyProc := CopyInterlacedGray2;
        16     : CopyProc := CopyInterlacedGrayscale16;
      end;
    {RGB followed by alpha}
    COLOR_RGBALPHA:
      case Header.BitDepth of
        8:  CopyProc := CopyInterlacedRGBAlpha8;
       16:  CopyProc := CopyInterlacedRGBAlpha16;
      end;
    {Grayscale followed by alpha}
    COLOR_GRAYSCALEALPHA:
      case Header.BitDepth of
        8:  CopyProc := CopyInterlacedGrayscaleAlpha8;
       16:  CopyProc := CopyInterlacedGrayscaleAlpha16;
      end;
  end {case Header.ColorType};

  {Adam7 method has 7 passes to make the final image}
  FOR CurrentPass := 0 TO 6 DO
  begin
    {Calculates the number of pixels and bytes for this pass row}
    PixelsThisRow := (ImageWidth - ColumnStart[CurrentPass] +
      ColumnIncrement[CurrentPass] - 1) div ColumnIncrement[CurrentPass];
    Row_Bytes := BytesForPixels(PixelsThisRow, Header.ColorType,
      Header.BitDepth);
    {Clear buffer for this pass}
    ZeroMemory(Row_Buffer[not RowUsed], Row_Bytes);

    {Get current row index}
    CurrentRow := RowStart[CurrentPass];
    {Get a pointer to the current row image data}
    Data := Ptr(Longint(Header.ImageData) + Header.BytesPerRow *
      (ImageHeight - 1 - CurrentRow));
    Trans := Ptr(Longint(Header.ImageAlpha) + ImageWidth * CurrentRow);
    {$IFDEF Store16bits}
    Extra := Ptr(Longint(Header.ExtraImageData) + Header.BytesPerRow *
      (ImageHeight - 1 - CurrentRow));
    {$ENDIF}

    if Row_Bytes > 0 then {There must have bytes for this interlaced pass}
      while CurrentRow < ImageHeight do
      begin
        {Reads this line and filter}
        if IDATZlibRead(ZLIBStream, @Row_Buffer[RowUsed][0], Row_Bytes + 1,
          EndPos, CRCFile) = 0 then break;

        FilterRow;
        {Copy image data}

        CopyProc(CurrentPass, @Row_Buffer[RowUsed][1], Data, Trans
          {$IFDEF Store16bits}, Extra{$ENDIF});

        {Use the other RowBuffer item}
        RowUsed := not RowUsed;

        {Move to the next row}
        inc(CurrentRow, RowIncrement[CurrentPass]);
        {Move pointer to the next line}
        dec(Data, RowIncrement[CurrentPass] * Header.BytesPerRow);
        inc(Trans, RowIncrement[CurrentPass] * ImageWidth);
        {$IFDEF Store16bits}
        dec(Extra, RowIncrement[CurrentPass] * Header.BytesPerRow);
        {$ENDIF}
      end {while CurrentRow < ImageHeight};

  end {FOR CurrentPass};

end;

{Copy 8 bits RGB image}
procedure TChunkIDAT.CopyNonInterlacedRGB8(
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
var
  I: Integer;
begin
  FOR I := 1 TO ImageWidth DO
  begin
    {Copy pixel values}
    Byte(Dest^) := fOwner.GammaTable[pByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^) := fOwner.GammaTable[pByte(Longint(Src) + 1)^]; inc(Dest);
    Byte(Dest^) := fOwner.GammaTable[pByte(Longint(Src)    )^]; inc(Dest);
    {Move to next pixel}
    inc(Src, 3);
  end {for I}
end;

{Copy 16 bits RGB image}
procedure TChunkIDAT.CopyNonInterlacedRGB16(
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
var
  I: Integer;
begin
  FOR I := 1 TO ImageWidth DO
  begin
    //Since windows does not supports 2 bytes for
    //each R, G, B value, the method will read only 1 byte from it
    {Copy pixel values}
    Byte(Dest^) := fOwner.GammaTable[pByte(Longint(Src) + 4)^]; inc(Dest);
    Byte(Dest^) := fOwner.GammaTable[pByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^) := fOwner.GammaTable[pByte(Longint(Src)    )^]; inc(Dest);
    {$IFDEF Store16bits}
    {Copy extra pixel values}
    Byte(Extra^) := fOwner.GammaTable[pByte(Longint(Src) + 5)^]; inc(Extra);
    Byte(Extra^) := fOwner.GammaTable[pByte(Longint(Src) + 3)^]; inc(Extra);
    Byte(Extra^) := fOwner.GammaTable[pByte(Longint(Src) + 1)^]; inc(Extra);
    {$ENDIF}

    {Move to next pixel}
    inc(Src, 6);
  end {for I}
end;

{Copy types using palettes (1, 4 or 8 bits per pixel)}
procedure TChunkIDAT.CopyNonInterlacedPalette148(
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
begin
  {It's simple as copying the data}
  CopyMemory(Dest, Src, Row_Bytes);
end;

{Copy grayscale types using 2 bits for each pixel}
procedure TChunkIDAT.CopyNonInterlacedGray2(
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
var
  i: Integer;
begin
  {2 bits is not supported, this routine will converted into 4 bits}
  FOR i := 1 TO Row_Bytes do
  begin
    Byte(Dest^) := ((Byte(Src^) shr 2) and $F) or ((Byte(Src^)) and $F0); inc(Dest);
    Byte(Dest^) := ((Byte(Src^) shl 2) and $F) or ((Byte(Src^) shl 4) and $F0); inc(Dest);
    inc(Src);
  end {FOR i}
end;

{Copy types using palette with 2 bits for each pixel}
procedure TChunkIDAT.CopyNonInterlacedPalette2(
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
var
  i: Integer;
begin
  {2 bits is not supported, this routine will converted into 4 bits}
  FOR i := 1 TO Row_Bytes do
  begin
    Byte(Dest^) := ((Byte(Src^) shr 4) and $3) or ((Byte(Src^) shr 2) and $30); inc(Dest);
    Byte(Dest^) := (Byte(Src^) and $3) or ((Byte(Src^) shl 2) and $30); inc(Dest);
    inc(Src);
  end {FOR i}
end;

{Copy grayscale images with 16 bits}
procedure TChunkIDAT.CopyNonInterlacedGrayscale16(
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
var
  I: Integer;
begin
  FOR I := 1 TO ImageWidth DO
  begin
    {Windows does not supports 16 bits for each pixel in grayscale}
    {mode, so reduce to 8}
    Dest^ := Src^; inc(Dest);
    {$IFDEF Store16bits}
    Extra^ := pChar(Longint(Src) + 1)^; inc(Extra);
    {$ENDIF}

    {Move to next pixel}
    inc(Src, 2);
  end {for I}
end;

{Copy 8 bits per sample RGB images followed by an alpha byte}
procedure TChunkIDAT.CopyNonInterlacedRGBAlpha8(
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
var
  i: Integer;
begin
  FOR I := 1 TO ImageWidth DO
  begin
    {Copy pixel values and transparency}
    Trans^ := pChar(Longint(Src) + 3)^;
    Byte(Dest^)  := fOwner.GammaTable[pByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^)  := fOwner.GammaTable[pByte(Longint(Src) + 1)^]; inc(Dest);
    Byte(Dest^)  := fOwner.GammaTable[pByte(Longint(Src)    )^]; inc(Dest);
    {Move to next pixel}
    inc(Src, 4); inc(Trans);
  end {for I}
end;

{Copy 16 bits RGB image with alpha using 2 bytes for each sample}
procedure TChunkIDAT.CopyNonInterlacedRGBAlpha16(
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
var
  I: Integer;
begin
  FOR I := 1 TO ImageWidth DO
  begin
    //Copy rgb and alpha values (transforming from 16 bits to 8 bits)
    {Copy pixel values}
    Trans^ := pChar(Longint(Src) + 6)^;
    Byte(Dest^)  := fOwner.GammaTable[pByte(Longint(Src) + 4)^]; inc(Dest);
    Byte(Dest^)  := fOwner.GammaTable[pByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^)  := fOwner.GammaTable[pByte(Longint(Src)    )^]; inc(Dest);
    {$IFDEF Store16bits}
    {Copy extra pixel values}
    Byte(Extra^) := fOwner.GammaTable[pByte(Longint(Src) + 5)^]; inc(Extra);
    Byte(Extra^) := fOwner.GammaTable[pByte(Longint(Src) + 3)^]; inc(Extra);
    Byte(Extra^) := fOwner.GammaTable[pByte(Longint(Src) + 1)^]; inc(Extra);
    {$ENDIF}
    {Move to next pixel}
    inc(Src, 8); inc(Trans);
  end {for I}
end;

{Copy 8 bits per sample grayscale followed by alpha}
procedure TChunkIDAT.CopyNonInterlacedGrayscaleAlpha8(
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
var
  I: Integer;
begin
  FOR I := 1 TO ImageWidth DO
  begin
    {Copy alpha value and then gray value}
    Dest^  := Src^;  inc(Src);
    Trans^ := Src^;  inc(Src);
    inc(Dest); inc(Trans);
  end;
end;

{Copy 16 bits per sample grayscale followed by alpha}
procedure TChunkIDAT.CopyNonInterlacedGrayscaleAlpha16(
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar);
var
  I: Integer;
begin
  FOR I := 1 TO ImageWidth DO
  begin
    {Copy alpha value and then gray value}
    {$IFDEF Store16bits}
    Extra^ := pChar(Longint(Src) + 1)^; inc(Extra);
    {$ENDIF}
    Dest^  := Src^;  inc(Src, 2);
    Trans^ := Src^;  inc(Src, 2);
    inc(Dest); inc(Trans);
  end;
end;

{Decode non interlaced image}
procedure TChunkIDAT.DecodeNonInterlaced(Stream: TStream;
  var ZLIBStream: TZStreamRec2; const Size: Integer; var crcfile: Cardinal);
var
  j: Cardinal;
  Trans, Data{$IFDEF Store16bits}, Extra{$ENDIF}: pChar;
  CopyProc: procedure(
    Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: pChar) of object;
begin
  CopyProc := nil; {Initialize}
  {Determines the method to copy the image data}
  case Header.ColorType of
    {R, G, B values}
    COLOR_RGB:
      case Header.BitDepth of
        8: CopyProc := CopyNonInterlacedRGB8;
       16: CopyProc := CopyNonInterlacedRGB16;
      end;
    {Types using palettes}
    COLOR_PALETTE, COLOR_GRAYSCALE:
      case Header.BitDepth of
        1, 4, 8: CopyProc := CopyNonInterlacedPalette148;
        2      : if Header.ColorType = COLOR_PALETTE then
                   CopyProc := CopyNonInterlacedPalette2
                 else
                   CopyProc := CopyNonInterlacedGray2;
        16     : CopyProc := CopyNonInterlacedGrayscale16;
      end;
    {R, G, B followed by alpha}
    COLOR_RGBALPHA:
      case Header.BitDepth of
        8  : CopyProc := CopyNonInterlacedRGBAlpha8;
       16  : CopyProc := CopyNonInterlacedRGBAlpha16;
      end;
    {Grayscale followed by alpha}
    COLOR_GRAYSCALEALPHA:
      case Header.BitDepth of
        8  : CopyProc := CopyNonInterlacedGrayscaleAlpha8;
       16  : CopyProc := CopyNonInterlacedGrayscaleAlpha16;
      end;
  end;

  {Get the image data pointer}
  Longint(Data) := Longint(Header.ImageData) +
    Header.BytesPerRow * (ImageHeight - 1);
  Trans := Header.ImageAlpha;
  {$IFDEF Store16bits}
  Longint(Extra) := Longint(Header.ExtraImageData) +
    Header.BytesPerRow * (ImageHeight - 1);
  {$ENDIF}
  {Reads each line}
  FOR j := 0 to ImageHeight - 1 do
  begin
    {Read this line Row_Buffer[RowUsed][0] if the filter type for this line}
    if IDATZlibRead(ZLIBStream, @Row_Buffer[RowUsed][0], Row_Bytes + 1, EndPos,
      CRCFile) = 0 then break;

    {Filter the current row}
    FilterRow;
    {Copies non interlaced row to image}
    CopyProc(@Row_Buffer[RowUsed][1], Data, Trans{$IFDEF Store16bits}, Extra
      {$ENDIF});

    {Invert line used}
    RowUsed := not RowUsed;
    dec(Data, Header.BytesPerRow);
    {$IFDEF Store16bits}dec(Extra, Header.BytesPerRow);{$ENDIF}
    inc(Trans, ImageWidth);
  end {for I};


end;

{Filter the current line}
procedure TChunkIDAT.FilterRow;
var
  pp: Byte;
  vv, left, above, aboveleft: Integer;
  Col: Cardinal;
begin
  {Test the filter}
  case Row_Buffer[RowUsed]^[0] of
    {No filtering for this line}
    FILTER_NONE: begin end;
    {AND 255 serves only to never let the result be larger than one byte}
    {Sub filter}
    FILTER_SUB:
      FOR Col := Offset + 1 to Row_Bytes DO
        Row_Buffer[RowUsed][Col] := (Row_Buffer[RowUsed][Col] +
          Row_Buffer[RowUsed][Col - Offset]) and 255;
    {Up filter}
    FILTER_UP:
      FOR Col := 1 to Row_Bytes DO
        Row_Buffer[RowUsed][Col] := (Row_Buffer[RowUsed][Col] +
          Row_Buffer[not RowUsed][Col]) and 255;
    {Average filter}
    FILTER_AVERAGE:
      FOR Col := 1 to Row_Bytes DO
      begin
        {Obtains up and left pixels}
        above := Row_Buffer[not RowUsed][Col];
        if col - 1 < Offset then
          left := 0
        else
          Left := Row_Buffer[RowUsed][Col - Offset];

        {Calculates}
        Row_Buffer[RowUsed][Col] := (Row_Buffer[RowUsed][Col] +
          (left + above) div 2) and 255;
      end;
    {Paeth filter}
    FILTER_PAETH:
    begin
      {Initialize}
      left := 0;
      aboveleft := 0;
      {Test each byte}
      FOR Col := 1 to Row_Bytes DO
      begin
        {Obtains above pixel}
        above := Row_Buffer[not RowUsed][Col];
        {Obtains left and top-left pixels}
        if (col - 1 >= offset) Then
        begin
          left := row_buffer[RowUsed][col - offset];
          aboveleft := row_buffer[not RowUsed][col - offset];
        end;

        {Obtains current pixel and paeth predictor}
        vv := row_buffer[RowUsed][Col];
        pp := PaethPredictor(left, above, aboveleft);

        {Calculates}
        Row_Buffer[RowUsed][Col] := (pp + vv) and $FF;
      end {for};
    end;
      
  end {case};
end;

{Reads the image data from the stream}
function TChunkIDAT.LoadFromStream(Stream: TStream; const ChunkName: TChunkName;
  Size: Integer): Boolean;
var
  ZLIBStream: TZStreamRec2;
  CRCCheck,
  CRCFile  : Cardinal;
begin
  {Get pointer to the header chunk}
  Header := Owner.Chunks.Item[0] as TChunkIHDR;
  {Build palette if necessary}
  if Header.HasPalette then PreparePalette();

  {Copy image width and height}
  ImageWidth := Header.Width;
  ImageHeight := Header.Height;

  {Initialize to calculate CRC}
  {$IFDEF CheckCRC}
    CRCFile := update_crc($ffffffff, @ChunkName[0], 4);
  {$ENDIF}

  Owner.GetPixelInfo(Row_Bytes, Offset); {Obtain line information}
  ZLIBStream := ZLIBInitInflate(Stream);  {Initializes decompression}

  {Calculate ending position for the current IDAT chunk}
  EndPos := Stream.Position + Size;

  {Allocate memory}
  GetMem(Row_Buffer[false], Row_Bytes + 1);
  GetMem(Row_Buffer[true], Row_Bytes + 1);
  ZeroMemory(Row_Buffer[false], Row_bytes + 1);
  {Set the variable to alternate the Row_Buffer item to use}
  RowUsed := TRUE;

  {Call special methods for the different interlace methods}
  case Owner.InterlaceMethod of
    imNone:  DecodeNonInterlaced(stream, ZLIBStream, Size, crcfile);
    imAdam7: DecodeInterlacedAdam7(stream, ZLIBStream, size, crcfile);
  end;

  {Free memory}
  ZLIBTerminateInflate(ZLIBStream); {Terminates decompression}
  FreeMem(Row_Buffer[False], Row_Bytes + 1);
  FreeMem(Row_Buffer[True], Row_Bytes + 1);

  {Now checks CRC}
  Stream.Read(CRCCheck, 4);
  {$IFDEF CheckCRC}
    CRCFile := CRCFile xor $ffffffff;
    CRCCheck := ByteSwap(CRCCheck);
    Result := CRCCheck = CRCFile;

    {Handle CRC error}
    if not Result then
    begin
      {In case it coult not load chunk}
      Owner.RaiseError(EPngInvalidCRC, EPngInvalidCRCText);
      exit;
    end;
  {$ELSE}Result := TRUE; {$ENDIF}
end;

const
  IDATHeader: Array[0..3] of char = ('I', 'D', 'A', 'T');
  BUFFER = 5;

{Saves the IDAT chunk to a stream}
function TChunkIDAT.SaveToStream(Stream: TStream): Boolean;
var
  ZLIBStream : TZStreamRec2;
begin
  {Get pointer to the header chunk}
  Header := Owner.Chunks.Item[0] as TChunkIHDR;
  {Copy image width and height}
  ImageWidth := Header.Width;
  ImageHeight := Header.Height;
  Owner.GetPixelInfo(Row_Bytes, Offset); {Obtain line information}

  {Allocate memory}
  GetMem(Encode_Buffer[BUFFER], Row_Bytes);
  ZeroMemory(Encode_Buffer[BUFFER], Row_Bytes);
  {Allocate buffers for the filters selected}
  {Filter none will always be calculated to the other filters to work}
  GetMem(Encode_Buffer[FILTER_NONE], Row_Bytes);
  ZeroMemory(Encode_Buffer[FILTER_NONE], Row_Bytes);
  if pfSub in Owner.Filters then
    GetMem(Encode_Buffer[FILTER_SUB], Row_Bytes);
  if pfUp in Owner.Filters then
    GetMem(Encode_Buffer[FILTER_UP], Row_Bytes);
  if pfAverage in Owner.Filters then
    GetMem(Encode_Buffer[FILTER_AVERAGE], Row_Bytes);
  if pfPaeth in Owner.Filters then
    GetMem(Encode_Buffer[FILTER_PAETH], Row_Bytes);

  {Initialize ZLIB}
  ZLIBStream := ZLIBInitDeflate(Stream, Owner.fCompressionLevel,
    Owner.MaxIdatSize);
  {Write data depending on the interlace method}
  case Owner.InterlaceMethod of
    imNone: EncodeNonInterlaced(stream, ZLIBStream);
    imAdam7: EncodeInterlacedAdam7(stream, ZLIBStream);
  end;
  {Terminates ZLIB}
  ZLIBTerminateDeflate(ZLIBStream);

  {Release allocated memory}
  FreeMem(Encode_Buffer[BUFFER], Row_Bytes);
  FreeMem(Encode_Buffer[FILTER_NONE], Row_Bytes);
  if pfSub in Owner.Filters then
    FreeMem(Encode_Buffer[FILTER_SUB], Row_Bytes);
  if pfUp in Owner.Filters then
    FreeMem(Encode_Buffer[FILTER_UP], Row_Bytes);
  if pfAverage in Owner.Filters then
    FreeMem(Encode_Buffer[FILTER_AVERAGE], Row_Bytes);
  if pfPaeth in Owner.Filters then
    FreeMem(Encode_Buffer[FILTER_PAETH], Row_Bytes);

  {Everything went ok}
  Result := True;
end;

{Writes the IDAT using the settings}
procedure WriteIDAT(Stream: TStream; Data: Pointer; const Length: Cardinal);
var
  ChunkLen, CRC: Cardinal;
begin
  {Writes IDAT header}
  ChunkLen := ByteSwap(Length);
  Stream.Write(ChunkLen, 4);                      {Chunk length}
  Stream.Write(IDATHeader[0], 4);                 {Idat header}
  CRC := update_crc($ffffffff, @IDATHeader[0], 4); {Crc part for header}

  {Writes IDAT data and calculates CRC for data}
  Stream.Write(Data^, Length);
  CRC := Byteswap(update_crc(CRC, Data, Length) xor $ffffffff);
  {Writes final CRC}
  Stream.Write(CRC, 4);
end;

{Compress and writes IDAT chunk data}
procedure TChunkIDAT.IDATZlibWrite(var ZLIBStream: TZStreamRec2;
  Buffer: Pointer; const Length: Cardinal);
begin
  with ZLIBStream, ZLIBStream.ZLIB do
  begin
    {Set data to be compressed}
    next_in := Buffer;
    avail_in := Length;

    {Compress all the data avaliable to compress}
    while avail_in > 0 do
    begin
      deflate(ZLIB, Z_NO_FLUSH);

      {The whole buffer was used, save data to stream and restore buffer}
      if avail_out = 0 then
      begin
        {Writes this IDAT chunk}
        WriteIDAT(fStream, Data, ZLIBAllocate);

        {Restore buffer}
        next_out := Data;
        avail_out := ZLIBAllocate;
      end {if avail_out = 0};

    end {while avail_in};

  end {with ZLIBStream, ZLIBStream.ZLIB}
end;

{Finishes compressing data to write IDAT chunk}
procedure TChunkIDAT.FinishIDATZlib(var ZLIBStream: TZStreamRec2);
begin
  with ZLIBStream, ZLIBStream.ZLIB do
  begin
    {Set data to be compressed}
    next_in := nil;
    avail_in := 0;

    while deflate(ZLIB,Z_FINISH) <> Z_STREAM_END do
    begin
      {Writes this IDAT chunk}
      WriteIDAT(fStream, Data, ZLIBAllocate - avail_out);
      {Re-update buffer}
      next_out := Data;
      avail_out := ZLIBAllocate;
    end;

    if avail_out < ZLIBAllocate then
      {Writes final IDAT}
      WriteIDAT(fStream, Data, ZLIBAllocate - avail_out);

  end {with ZLIBStream, ZLIBStream.ZLIB};
end;

{Copy memory to encode RGB image with 1 byte for each color sample}
procedure TChunkIDAT.EncodeNonInterlacedRGB8(Src, Dest, Trans: pChar);
var
  I: Integer;
begin
  FOR I := 1 TO ImageWidth DO
  begin
    {Copy pixel values}
    Byte(Dest^) := fOwner.InverseGamma[pByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^) := fOwner.InverseGamma[pByte(Longint(Src) + 1)^]; inc(Dest);
    Byte(Dest^) := fOwner.InverseGamma[pByte(Longint(Src)    )^]; inc(Dest);
    {Move to next pixel}
    inc(Src, 3);
  end {for I}
end;

{Copy memory to encode RGB images with 16 bits for each color sample}
procedure TChunkIDAT.EncodeNonInterlacedRGB16(Src, Dest, Trans: pChar);
var
  I: Integer;
begin
  FOR I := 1 TO ImageWidth DO
  begin
    //Now we copy from 1 byte for each sample stored to a 2 bytes (or 1 word)
    //for sample
    {Copy pixel values}
    pWORD(Dest)^ := fOwner.InverseGamma[pByte(Longint(Src) + 2)^]; inc(Dest, 2);
    pWORD(Dest)^ := fOwner.InverseGamma[pByte(Longint(Src) + 1)^]; inc(Dest, 2);
    pWORD(Dest)^ := fOwner.InverseGamma[pByte(Longint(Src)    )^]; inc(Dest, 2);
    {Move to next pixel}
    inc(Src, 3);
  end {for I}

end;

{Copy memory to encode types using palettes (1, 4 or 8 bits per pixel)}
procedure TChunkIDAT.EncodeNonInterlacedPalette148(Src, Dest, Trans: pChar);
begin
  {It's simple as copying the data}
  CopyMemory(Dest, Src, Row_Bytes);
end;

{Copy memory to encode grayscale images with 2 bytes for each sample}
procedure TChunkIDAT.EncodeNonInterlacedGrayscale16(Src, Dest, Trans: pChar);
var
  I: Integer;
begin
  FOR I := 1 TO ImageWidth DO
  begin
    //Now we copy from 1 byte for each sample stored to a 2 bytes (or 1 word)
    //for sample
    pWORD(Dest)^ := pByte(Longint(Src))^; inc(Dest, 2);
    {Move to next pixel}
    inc(Src);
  end {for I}
end;

{Encode images using RGB followed by an alpha value using 1 byte for each}
procedure TChunkIDAT.EncodeNonInterlacedRGBAlpha8(Src, Dest, Trans: pChar);
var
  i: Integer;
begin
  {Copy the data to the destination, including data from Trans pointer}
  FOR i := 1 TO ImageWidth do
  begin
    Byte(Dest^) := Owner.InverseGamma[PByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^) := Owner.InverseGamma[PByte(Longint(Src) + 1)^]; inc(Dest);
    Byte(Dest^) := Owner.InverseGamma[PByte(Longint(Src)    )^]; inc(Dest);
    Dest^ := Trans^; inc(Dest);
    inc(Src, 3); inc(Trans);
  end {for i};
end;

{Encode images using RGB followed by an alpha value using 2 byte for each}
procedure TChunkIDAT.EncodeNonInterlacedRGBAlpha16(Src, Dest, Trans: pChar);
var
  i: Integer;
begin
  {Copy the data to the destination, including data from Trans pointer}
  FOR i := 1 TO ImageWidth do
  begin
    pWord(Dest)^ := Owner.InverseGamma[PByte(Longint(Src) + 2)^]; inc(Dest, 2);
    pWord(Dest)^ := Owner.InverseGamma[PByte(Longint(Src) + 1)^]; inc(Dest, 2);
    pWord(Dest)^ := Owner.InverseGamma[PByte(Longint(Src)    )^]; inc(Dest, 2);
    pWord(Dest)^ := PByte(Longint(Trans)  )^; inc(Dest, 2);
    inc(Src, 3); inc(Trans);
  end {for i};
end;

{Encode grayscale images followed by an alpha value using 1 byte for each}
procedure TChunkIDAT.EncodeNonInterlacedGrayscaleAlpha8(
  Src, Dest, Trans: pChar);
var
  i: Integer;
begin
  {Copy the data to the destination, including data from Trans pointer}
  FOR i := 1 TO ImageWidth do
  begin
    Dest^ := Src^; inc(Dest);
    Dest^ := Trans^; inc(Dest);
    inc(Src); inc(Trans);
  end {for i};
end;

{Encode grayscale images followed by an alpha value using 2 byte for each}
procedure TChunkIDAT.EncodeNonInterlacedGrayscaleAlpha16(
  Src, Dest, Trans: pChar);
var
  i: Integer;
begin
  {Copy the data to the destination, including data from Trans pointer}
  FOR i := 1 TO ImageWidth do
  begin
    pWord(Dest)^ := pByte(Src)^;    inc(Dest, 2);
    pWord(Dest)^ := pByte(Trans)^;  inc(Dest, 2);
    inc(Src); inc(Trans);
  end {for i};
end;

{Encode non interlaced images}
procedure TChunkIDAT.EncodeNonInterlaced(Stream: TStream;
  var ZLIBStream: TZStreamRec2);
var
  {Current line}
  j: Cardinal;
  {Pointers to image data}
  Data, Trans: PChar;
  {Filter used for this line}
  Filter: Byte;
  {Method which will copy the data into the buffer}
  CopyProc: procedure(Src, Dest, Trans: pChar) of object;
begin
  CopyProc := nil;  {Initialize to avoid warnings}
  {Defines the method to copy the data to the buffer depending on}
  {the image parameters}
  case Header.ColorType of
    {R, G, B values}
    COLOR_RGB:
      case Header.BitDepth of
        8: CopyProc := EncodeNonInterlacedRGB8;
       16: CopyProc := EncodeNonInterlacedRGB16;
      end;
    {Palette and grayscale values}
    COLOR_GRAYSCALE, COLOR_PALETTE:
      case Header.BitDepth of
        1, 4, 8: CopyProc := EncodeNonInterlacedPalette148;
             16: CopyProc := EncodeNonInterlacedGrayscale16;
      end;
    {RGB with a following alpha value}
    COLOR_RGBALPHA:
      case Header.BitDepth of
          8: CopyProc := EncodeNonInterlacedRGBAlpha8;
         16: CopyProc := EncodeNonInterlacedRGBAlpha16;
      end;
    {Grayscale images followed by an alpha}
    COLOR_GRAYSCALEALPHA:
      case Header.BitDepth of
        8:  CopyProc := EncodeNonInterlacedGrayscaleAlpha8;
       16:  CopyProc := EncodeNonInterlacedGrayscaleAlpha16;
      end;
  end {case Header.ColorType};

  {Get the image data pointer}
  Longint(Data) := Longint(Header.ImageData) +
    Header.BytesPerRow * (ImageHeight - 1);
  Trans := Header.ImageAlpha;

  {Writes each line}
  FOR j := 0 to ImageHeight - 1 do
  begin
    {Copy data into buffer}
    CopyProc(Data, @Encode_Buffer[BUFFER][0], Trans);
    {Filter data}
    Filter := FilterToEncode;

    {Compress data}
    IDATZlibWrite(ZLIBStream, @Filter, 1);
    IDATZlibWrite(ZLIBStream, @Encode_Buffer[Filter][0], Row_Bytes);

    {Adjust pointers to the actual image data}
    dec(Data, Header.BytesPerRow);
    inc(Trans, ImageWidth);
  end;

  {Compress and finishes copying the remaining data}
  FinishIDATZlib(ZLIBStream);
end;

{Copy memory to encode interlaced images using RGB value with 1 byte for}
{each color sample}
procedure TChunkIDAT.EncodeInterlacedRGB8(const Pass: Byte;
  Src, Dest, Trans: pChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Src := pChar(Longint(Src) + Col * 3);
  repeat
    {Copy this row}
    Byte(Dest^) := fOwner.InverseGamma[pByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^) := fOwner.InverseGamma[pByte(Longint(Src) + 1)^]; inc(Dest);
    Byte(Dest^) := fOwner.InverseGamma[pByte(Longint(Src)    )^]; inc(Dest);

    {Move to next column}
    inc(Src, ColumnIncrement[Pass] * 3);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Copy memory to encode interlaced RGB images with 2 bytes each color sample}
procedure TChunkIDAT.EncodeInterlacedRGB16(const Pass: Byte;
  Src, Dest, Trans: pChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Src := pChar(Longint(Src) + Col * 3);
  repeat
    {Copy this row}
    pWord(Dest)^ := Owner.InverseGamma[pByte(Longint(Src) + 2)^]; inc(Dest, 2);
    pWord(Dest)^ := Owner.InverseGamma[pByte(Longint(Src) + 1)^]; inc(Dest, 2);
    pWord(Dest)^ := Owner.InverseGamma[pByte(Longint(Src)    )^]; inc(Dest, 2);

    {Move to next column}
    inc(Src, ColumnIncrement[Pass] * 3);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Copy memory to encode interlaced images using palettes using bit depths}
{1, 4, 8 (each pixel in the image)}
procedure TChunkIDAT.EncodeInterlacedPalette148(const Pass: Byte;
  Src, Dest, Trans: pChar);
const
  BitTable: Array[1..8] of Integer = ($1, $3, 0, $F, 0, 0, 0, $FF);
  StartBit: Array[1..8] of Integer = (7 , 0 , 0, 4,  0, 0, 0, 0);
var
  CurBit, Col: Integer;
  Src2: PChar;
begin
  {Clean the line}
  fillchar(Dest^, Row_Bytes, #0);
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  with Header.BitmapInfo.bmiHeader do
    repeat
      {Copy data}
      CurBit := StartBit[biBitCount];
      repeat
        {Adjust pointer to pixel byte bounds}
        Src2 := pChar(Longint(Src) + (biBitCount * Col) div 8);
        {Copy data}
        Byte(Dest^) := Byte(Dest^) or
          (((Byte(Src2^) shr (StartBit[Header.BitDepth] - (biBitCount * Col)
            mod 8))) and (BitTable[biBitCount])) shl CurBit;

        {Move to next column}
        inc(Col, ColumnIncrement[Pass]);
        {Will read next bits}
        dec(CurBit, biBitCount);
      until CurBit < 0;

      {Move to next byte in source}
      inc(Dest);
    until Col >= ImageWidth;
end;

{Copy to encode interlaced grayscale images using 16 bits for each sample}
procedure TChunkIDAT.EncodeInterlacedGrayscale16(const Pass: Byte;
  Src, Dest, Trans: pChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Src := pChar(Longint(Src) + Col);
  repeat
    {Copy this row}
    pWord(Dest)^ := Byte(Src^); inc(Dest, 2);

    {Move to next column}
    inc(Src, ColumnIncrement[Pass]);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Copy to encode interlaced rgb images followed by an alpha value, all using}
{one byte for each sample}
procedure TChunkIDAT.EncodeInterlacedRGBAlpha8(const Pass: Byte;
  Src, Dest, Trans: pChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Src := pChar(Longint(Src) + Col * 3);
  Trans := pChar(Longint(Trans) + Col);
  repeat
    {Copy this row}
    Byte(Dest^) := Owner.InverseGamma[pByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^) := Owner.InverseGamma[pByte(Longint(Src) + 1)^]; inc(Dest);
    Byte(Dest^) := Owner.InverseGamma[pByte(Longint(Src)    )^]; inc(Dest);
    Dest^ := Trans^; inc(Dest);

    {Move to next column}
    inc(Src, ColumnIncrement[Pass] * 3);
    inc(Trans, ColumnIncrement[Pass]);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Copy to encode interlaced rgb images followed by an alpha value, all using}
{two byte for each sample}
procedure TChunkIDAT.EncodeInterlacedRGBAlpha16(const Pass: Byte;
  Src, Dest, Trans: pChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Src := pChar(Longint(Src) + Col * 3);
  Trans := pChar(Longint(Trans) + Col);
  repeat
    {Copy this row}
    pWord(Dest)^ := pByte(Longint(Src) + 2)^; inc(Dest, 2);
    pWord(Dest)^ := pByte(Longint(Src) + 1)^; inc(Dest, 2);
    pWord(Dest)^ := pByte(Longint(Src)    )^; inc(Dest, 2);
    pWord(Dest)^ := pByte(Trans)^; inc(Dest, 2);

    {Move to next column}
    inc(Src, ColumnIncrement[Pass] * 3);
    inc(Trans, ColumnIncrement[Pass]);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Copy to encode grayscale interlaced images followed by an alpha value, all}
{using 1 byte for each sample}
procedure TChunkIDAT.EncodeInterlacedGrayscaleAlpha8(const Pass: Byte;
  Src, Dest, Trans: pChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Src := pChar(Longint(Src) + Col);
  Trans := pChar(Longint(Trans) + Col);
  repeat
    {Copy this row}
    Dest^ := Src^;   inc(Dest);
    Dest^ := Trans^; inc(Dest);

    {Move to next column}
    inc(Src, ColumnIncrement[Pass]);
    inc(Trans, ColumnIncrement[Pass]);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Copy to encode grayscale interlaced images followed by an alpha value, all}
{using 2 bytes for each sample}
procedure TChunkIDAT.EncodeInterlacedGrayscaleAlpha16(const Pass: Byte;
  Src, Dest, Trans: pChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Src := pChar(Longint(Src) + Col);
  Trans := pChar(Longint(Trans) + Col);
  repeat
    {Copy this row}
    pWord(Dest)^ := pByte(Src)^; inc(Dest, 2);
    pWord(Dest)^ := pByte(Trans)^; inc(Dest, 2);

    {Move to next column}
    inc(Src, ColumnIncrement[Pass]);
    inc(Trans, ColumnIncrement[Pass]);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Encode interlaced images}
procedure TChunkIDAT.EncodeInterlacedAdam7(Stream: TStream;
  var ZLIBStream: TZStreamRec2);
var
  CurrentPass, Filter: Byte;
  PixelsThisRow: Integer;
  CurrentRow : Integer;
  Trans, Data: pChar;
  CopyProc: procedure(const Pass: Byte;
    Src, Dest, Trans: pChar) of object;
begin
  CopyProc := nil;  {Initialize to avoid warnings}
  {Defines the method to copy the data to the buffer depending on}
  {the image parameters}
  case Header.ColorType of
    {R, G, B values}
    COLOR_RGB:
      case Header.BitDepth of
        8: CopyProc := EncodeInterlacedRGB8;
       16: CopyProc := EncodeInterlacedRGB16;
      end;
    {Grayscale and palette}
    COLOR_PALETTE, COLOR_GRAYSCALE:
      case Header.BitDepth of
        1, 4, 8: CopyProc := EncodeInterlacedPalette148;
             16: CopyProc := EncodeInterlacedGrayscale16;
      end;
    {RGB followed by alpha}
    COLOR_RGBALPHA:
      case Header.BitDepth of
          8: CopyProc := EncodeInterlacedRGBAlpha8;
         16: CopyProc := EncodeInterlacedRGBAlpha16;
      end;
    COLOR_GRAYSCALEALPHA:
    {Grayscale followed by alpha}
      case Header.BitDepth of
          8: CopyProc := EncodeInterlacedGrayscaleAlpha8;
         16: CopyProc := EncodeInterlacedGrayscaleAlpha16;
      end;
  end {case Header.ColorType};

  {Compress the image using the seven passes for ADAM 7}
  FOR CurrentPass := 0 TO 6 DO
  begin
    {Calculates the number of pixels and bytes for this pass row}
    PixelsThisRow := (ImageWidth - ColumnStart[CurrentPass] +
      ColumnIncrement[CurrentPass] - 1) div ColumnIncrement[CurrentPass];
    Row_Bytes := BytesForPixels(PixelsThisRow, Header.ColorType,
      Header.BitDepth);
    ZeroMemory(Encode_Buffer[FILTER_NONE], Row_Bytes);

    {Get current row index}
    CurrentRow := RowStart[CurrentPass];
    {Get a pointer to the current row image data}
    Data := Ptr(Longint(Header.ImageData) + Header.BytesPerRow *
      (ImageHeight - 1 - CurrentRow));
    Trans := Ptr(Longint(Header.ImageAlpha) + ImageWidth * CurrentRow);

    {Process all the image rows}
    if Row_Bytes > 0 then
      while CurrentRow < ImageHeight do
      begin
        {Copy data into buffer}
        CopyProc(CurrentPass, Data, @Encode_Buffer[BUFFER][0], Trans);
        {Filter data}
        Filter := FilterToEncode;

        {Compress data}
        IDATZlibWrite(ZLIBStream, @Filter, 1);
        IDATZlibWrite(ZLIBStream, @Encode_Buffer[Filter][0], Row_Bytes);

        {Move to the next row}
        inc(CurrentRow, RowIncrement[CurrentPass]);
        {Move pointer to the next line}
        dec(Data, RowIncrement[CurrentPass] * Header.BytesPerRow);
        inc(Trans, RowIncrement[CurrentPass] * ImageWidth);
      end {while CurrentRow < ImageHeight}

  end {CurrentPass};

  {Compress and finishes copying the remaining data}
  FinishIDATZlib(ZLIBStream);
end;

{Filters the row to be encoded and returns the best filter}
function TChunkIDAT.FilterToEncode: Byte;
var
  Run, LongestRun, ii, jj: Cardinal;
  Last, Above, LastAbove: Byte;
begin
  {Selecting more filters using the Filters property from TPngObject}
  {increases the chances to the file be much smaller, but decreases}
  {the performace}

  {This method will creates the same line data using the different}
  {filter methods and select the best}

  {Sub-filter}
  if pfSub in Owner.Filters then
    for ii := 0 to Row_Bytes - 1 do
    begin
      {There is no previous pixel when it's on the first pixel, so}
      {set last as zero when in the first}
      if (ii >= Offset) then
        last := Encode_Buffer[BUFFER]^[ii - Offset]
      else
        last := 0;
      Encode_Buffer[FILTER_SUB]^[ii] := Encode_Buffer[BUFFER]^[ii] - last;
    end;

  {Up filter}
  if pfUp in Owner.Filters then
    for ii := 0 to Row_Bytes - 1 do
      Encode_Buffer[FILTER_UP]^[ii] := Encode_Buffer[BUFFER]^[ii] -
        Encode_Buffer[FILTER_NONE]^[ii];

  {Average filter}
  if pfAverage in Owner.Filters then
    for ii := 0 to Row_Bytes - 1 do
    begin
      {Get the previous pixel, if the current pixel is the first, the}
      {previous is considered to be 0}
      if (ii >= Offset) then
        last := Encode_Buffer[BUFFER]^[ii - Offset]
      else
        last := 0;
      {Get the pixel above}
      above := Encode_Buffer[FILTER_NONE]^[ii];

      {Calculates formula to the average pixel}
      Encode_Buffer[FILTER_AVERAGE]^[ii] := Encode_Buffer[BUFFER]^[ii] -
        (above + last) div 2 ;
    end;

  {Paeth filter (the slower)}
  if pfPaeth in Owner.Filters then
  begin
    {Initialize}
    last := 0;
    lastabove := 0;
    for ii := 0 to Row_Bytes - 1 do
    begin
      {In case this pixel is not the first in the line obtains the}
      {previous one and the one above the previous}
      if (ii >= Offset) then
      begin
        last := Encode_Buffer[BUFFER]^[ii - Offset];
        lastabove := Encode_Buffer[FILTER_NONE]^[ii - Offset];
      end;
      {Obtains the pixel above}
      above := Encode_Buffer[FILTER_NONE]^[ii];
      {Calculate paeth filter for this byte}
      Encode_Buffer[FILTER_PAETH]^[ii] := Encode_Buffer[BUFFER]^[ii] -
        PaethPredictor(last, above, lastabove);
    end;
  end;

  {Now calculates the same line using no filter, which is necessary}
  {in order to have data to the filters when the next line comes}
  CopyMemory(@Encode_Buffer[FILTER_NONE]^[0],
    @Encode_Buffer[BUFFER]^[0], Row_Bytes);

  {If only filter none is selected in the filter list, we don't need}
  {to proceed and further}
  if (Owner.Filters = [pfNone]) or (Owner.Filters = []) then
  begin
    Result := FILTER_NONE;
    exit;
  end {if (Owner.Filters = [pfNone...};

  {Check which filter is the best by checking which has the larger}
  {sequence of the same byte, since they are best compressed}
  LongestRun := 0; Result := FILTER_NONE;
  for ii := FILTER_NONE TO FILTER_PAETH do
    {Check if this filter was selected}
    if TFilter(ii) in Owner.Filters then
    begin
      Run := 0;
      {Check if it's the only filter}
      if Owner.Filters = [TFilter(ii)] then
      begin
        Result := ii;
        exit;
      end;

      {Check using a sequence of four bytes}
      for jj := 2 to Row_Bytes - 1 do
        if (Encode_Buffer[ii]^[jj] = Encode_Buffer [ii]^[jj-1]) or
            (Encode_Buffer[ii]^[jj] = Encode_Buffer [ii]^[jj-2]) then
          inc(Run);  {Count the number of sequences}

      {Check if this one is the best so far}
      if (Run > LongestRun) then
      begin
        Result := ii;
        LongestRun := Run;
      end {if (Run > LongestRun)};

    end {if TFilter(ii) in Owner.Filters};
end;

{TChunkPLTE implementation}

{Returns an item in the palette}
function TChunkPLTE.GetPaletteItem(Index: Byte): TRGBQuad;
begin
  {Test if item is valid, if not raise error}
  if Index > Count - 1 then
    Owner.RaiseError(EPNGError, EPNGUnknownPalEntryText)
  else
    {Returns the item}
    Result := Header.BitmapInfo.bmiColors[Index];
end;

{Loads the palette chunk from a stream}
function TChunkPLTE.LoadFromStream(Stream: TStream;
  const ChunkName: TChunkName; Size: Integer): Boolean;
type
  pPalEntry = ^PalEntry;
  PalEntry = record r, g, b: Byte end;
var
  j        : Integer;          {For the FOR}
  PalColor : pPalEntry;
begin
  {Let ancestor load data and check CRC}
  Result := inherited LoadFromStream(Stream, ChunkName, Size);
  if not Result then exit;

  {This chunk must be divisible by 3 in order to be valid}
  if (Size mod 3 <> 0) or (Size div 3 > 256) then
  begin
    {Raise error}
    Result := FALSE;
    Owner.RaiseError(EPNGInvalidPalette, EPNGInvalidPaletteText);
    exit;
  end {if Size mod 3 <> 0};

  {Fill array with the palette entries}
  fCount := Size div 3;
  PalColor := Data;
  FOR j := 0 TO fCount - 1 DO
    with Header.BitmapInfo.bmiColors[j] do
    begin
      rgbRed  :=  Owner.GammaTable[PalColor.r];
      rgbGreen := Owner.GammaTable[PalColor.g];
      rgbBlue :=  Owner.GammaTable[PalColor.b];
      rgbReserved := 0;
      inc(PalColor); {Move to next palette entry}
    end;
end;

{Saves the PLTE chunk to a stream}
function TChunkPLTE.SaveToStream(Stream: TStream): Boolean;
var
  J: Integer;
  DataPtr: pByte;
begin
  {Adjust size to hold all the palette items}
  ResizeData(fCount * 3);
  {Copy pointer to data}
  DataPtr := fData;

  {Copy palette items}
  with Header do
    FOR j := 0 TO fCount - 1 DO
      with BitmapInfo.bmiColors[j] do
      begin
        DataPtr^ := Owner.InverseGamma[rgbRed]; inc(DataPtr);
        DataPtr^ := Owner.InverseGamma[rgbGreen]; inc(DataPtr);
        DataPtr^ := Owner.InverseGamma[rgbBlue]; inc(DataPtr);
      end {with BitmapInfo};

  {Let ancestor do the rest of the work}
  Result := inherited SaveToStream(Stream);
end;

{Assigns from another PLTE chunk}
procedure TChunkPLTE.Assign(Source: TChunk);
begin
  {Copy the number of palette items}
  if Source is TChunkPLTE then
    fCount := TChunkPLTE(Source).fCount
  else
    Owner.RaiseError(EPNGError, EPNGCannotAssignChunkText);
end;

{TChunkgAMA implementation}

{Assigns from another chunk}
procedure TChunkgAMA.Assign(Source: TChunk);
begin
  {Copy the gamma value}
  if Source is TChunkgAMA then
    Gamma := TChunkgAMA(Source).Gamma
  else
    Owner.RaiseError(EPNGError, EPNGCannotAssignChunkText);
end;

{Gamma chunk being created}
constructor TChunkgAMA.Create(Owner: TPngObject);
begin
  {Call ancestor}
  inherited Create(Owner);
  Gamma := 1;  {Initial value}
end;

{Returns gamma value}
function TChunkgAMA.GetValue: Cardinal;
begin
  {Make sure that the size is four bytes}
  if DataSize <> 4 then
  begin
    {Adjust size and returns 1}
    ResizeData(4);
    Result := 1;
  end
  {If it's right, read the value}
  else Result := Cardinal(ByteSwap(pCardinal(Data)^))
end;

function Power(Base, Exponent: Extended): Extended;
begin
  if Exponent = 0.0 then
    Result := 1.0               {Math rule}
  else if (Base = 0) or (Exponent = 0) then Result := 0
  else
    Result := Exp(Exponent * Ln(Base));
end;


{Loading the chunk from a stream}
function TChunkgAMA.LoadFromStream(Stream: TStream;
  const ChunkName: TChunkName; Size: Integer): Boolean;
var
  i: Integer;
  Value: Cardinal;
begin
  {Call ancestor and test if it went ok}
  Result := inherited LoadFromStream(Stream, ChunkName, Size);
  if not Result then exit;
  Value := Gamma;
  {Build gamma table and inverse table for saving}
  if Value <> 0 then
    with Owner do
      FOR i := 0 TO 255 DO
      begin
        GammaTable[I] := Round(Power((I / 255), 1 /
          (Value / 100000 * 2.2)) * 255);
        InverseGamma[Round(Power((I / 255), 1 /
          (Value / 100000 * 2.2)) * 255)] := I;
      end
end;

{Sets the gamma value}
procedure TChunkgAMA.SetValue(const Value: Cardinal);
begin
  {Make sure that the size is four bytes}
  if DataSize <> 4 then ResizeData(4);
  {If it's right, set the value}
  pCardinal(Data)^ := ByteSwap(Value);
end;

{TPngObject implementation}

{Assigns from another object}
procedure TPngObject.Assign(Source: TPersistent);
begin
  {Assigns contents from another TPNGObject}
  if Source is TPNGObject then
    AssignPNG(Source as TPNGObject)
  {Copy contents from a TBitmap}
  {$IFDEF UseDelphi}else if Source is TBitmap then
    with Source as TBitmap do
      AssignHandle(Handle, Transparent,
        ColorToRGB(TransparentColor)){$ENDIF}
  {Unknown source, let ancestor deal with it}
  else
    inherited;
end;

{Clear all the chunks in the list}
procedure TPngObject.ClearChunks;
var
  i: Integer;
begin
  {Initialize gamma}
  InitializeGamma();
  {Free all the objects and memory (0 chunks Bug fixed by Noel Sharpe)}
  for i := 0 TO Integer(Chunks.Count) - 1 do
    TChunk(Chunks.Item[i]).Free;
  Chunks.Count := 0;
end;

{Portable Network Graphics object being created}
constructor TPngObject.Create;
begin
  {Let it be created}
  inherited Create;

  {Initial properties}
  TempPalette := 0;
  fFilters := [pfSub];
  fCompressionLevel := 7;
  fInterlaceMethod := imNone;
  fMaxIdatSize := High(Word);
  {Create chunklist object}
  fChunkList := TPngList.Create(Self);
end;

{Portable Network Graphics object being destroyed}
destructor TPngObject.Destroy;
begin
  {Free object list}
  ClearChunks;
  fChunkList.Free;
  {Free the temporary palette}
  if TempPalette <> 0 then DeleteObject(TempPalette);

  {Call ancestor destroy}
  inherited Destroy;
end;

{Returns linesize and byte offset for pixels}
procedure TPngObject.GetPixelInfo(var LineSize, Offset: Cardinal);
begin
  {There must be an Header chunk to calculate size}
  if HeaderPresent then
  begin
    {Calculate number of bytes for each line}
    LineSize := BytesForPixels(Header.Width, Header.ColorType, Header.BitDepth);

    {Calculates byte offset}
    Case Header.ColorType of
      {Grayscale}
      COLOR_GRAYSCALE:
        If Header.BitDepth = 16 Then
          Offset := 2
        Else
          Offset := 1 ;
      {It always smaller or equal one byte, so it occupes one byte}
      COLOR_PALETTE:
        offset := 1;
      {It might be 3 or 6 bytes}
      COLOR_RGB:
        offset := 3 * Header.BitDepth Div 8;
      {It might be 2 or 4 bytes}
      COLOR_GRAYSCALEALPHA:
        offset := 2 * Header.BitDepth Div 8;
      {4 or 8 bytes}
      COLOR_RGBALPHA:
        offset := 4 * Header.BitDepth Div 8;
      else
        Offset := 0;
      End ;

  end
  else
  begin
    {In case if there isn't any Header chunk}
    Offset := 0;
    LineSize := 0;
  end;

end;

{Returns image height}
function TPngObject.GetHeight: Integer;
begin
  {There must be a Header chunk to get the size, otherwise returns 0}
  if HeaderPresent then
    Result := TChunkIHDR(Chunks.Item[0]).Height
  else Result := 0;
end;

{Returns image width}
function TPngObject.GetWidth: Integer;
begin
  {There must be a Header chunk to get the size, otherwise returns 0}
  if HeaderPresent then
    Result := Header.Width
  else Result := 0;
end;

{Returns if the image is empty}
function TPngObject.GetEmpty: Boolean;
begin
  Result := (Chunks.Count = 0);
end;

{Raises an error}
procedure TPngObject.RaiseError(ExceptionClass: ExceptClass; Text: String);
begin
  raise ExceptionClass.Create(Text);
end;

{Set the maximum size for IDAT chunk}
procedure TPngObject.SetMaxIdatSize(const Value: Cardinal);
begin
  {Make sure the size is at least 65535}
  if Value < High(Word) then
    fMaxIdatSize := High(Word) else fMaxIdatSize := Value;
end;

{$IFNDEF UseDelphi}
  {Creates a file stream reading from the filename in the parameter and load}
  procedure TPngObject.LoadFromFile(const Filename: String);
  var
    FileStream: TFileStream;
  begin
    {Test if the file exists}
    if not FileExists(Filename) then
    begin
      {In case it does not exists, raise error}
      RaiseError(EPNGNotExists, EPNGNotExistsText);
      exit;
    end;

    {Creates the file stream to read}
    FileStream := TFileStream.Create(Filename, [fsmRead]);
    LoadFromStream(FileStream);  {Loads the data}
    FileStream.Free;             {Free file stream}
  end;

  {Saves the current png image to a file}
  procedure TPngObject.SaveToFile(const Filename: String);
  var
    FileStream: TFileStream;
  begin
    {Creates the file stream to write}
    FileStream := TFileStream.Create(Filename, [fsmWrite]);
    SaveToStream(FileStream);    {Saves the data}
    FileStream.Free;             {Free file stream}
  end;

{$ENDIF}

{Returns pointer to the chunk TChunkIHDR which should be the first}
function TPngObject.GetHeader: TChunkIHDR;
begin
  {If there is a TChunkIHDR returns it, otherwise returns nil}
  if (Chunks.Count <> 0) and (Chunks.Item[0] is TChunkIHDR) then
    Result := Chunks.Item[0] as TChunkIHDR
  else
  begin
    {No header, throw error message}
    RaiseError(EPNGHeaderNotPresent, EPNGHeaderNotPresentText);
    Result := nil
  end
end;

{Draws using partial transparency}
{THANY: -1*Header.Height changed to -Integer(Header.Height) to prevent range errors}
procedure TPngObject.DrawPartialTrans(DC: HDC; Rect: TRect);
type
  {Access to pixels}
  TPixelLine = Array[Word] of TRGBQuad;
  pPixelLine = ^TPixelLine;
const
  {Structure used to create the bitmap}
  BitmapInfoHeader: TBitmapInfoHeader =
    (biSize: sizeof(TBitmapInfoHeader);
     biWidth: 100;
     biHeight: 100;
     biPlanes: 1;
     biBitCount: 32;
     biCompression: BI_RGB;
     biSizeImage: 0;
     biXPelsPerMeter: 0;
     biYPelsPerMeter: 0;
     biClrUsed: 0;
     biClrImportant: 0);
var
  {Buffer bitmap creation}
  BitmapInfo  : TBitmapInfo;
  BufferDC    : HDC;
  BufferBits  : Pointer;
  OldBitmap,
  BufferBitmap: HBitmap;

  {Transparency/palette chunks}
  TransparencyChunk: TChunktRNS;
  PaletteChunk: TChunkPLTE;
  TransValue, PaletteIndex: Byte;
  CurBit: Integer;
  Data: PByte;

  {Buffer bitmap modification}
  BytesPerRowDest,
  BytesPerRowSrc,
  BytesPerRowAlpha: Integer;
  ImageSource,
  AlphaSource     : pByteArray;
  ImageData       : pPixelLine;
  i, j            : Integer;
begin
  {Prepare to create the bitmap}
  Fillchar(BitmapInfo, sizeof(BitmapInfo), #0);
  BitmapInfoHeader.biWidth := Header.Width;
  BitmapInfoHeader.biHeight := -Integer(Header.Height);
  BitmapInfo.bmiHeader := BitmapInfoHeader;

  {Create the bitmap which will receive the background, the applied}
  {alpha blending and then will be painted on the background}
  BufferDC := CreateCompatibleDC(0);
  {In case BufferDC could not be created}
  if (BufferDC = 0) then RaiseError(EPNGOutMemory, EPNGOutMemoryText);
  BufferBitmap := CreateDIBSection(BufferDC, BitmapInfo, DIB_RGB_COLORS,
    BufferBits, 0, 0);
  {In case buffer bitmap could not be created}
  if (BufferBitmap = 0) or (BufferBits = Nil) then
  begin
    if BufferBitmap <> 0 then DeleteObject(BufferBitmap);
    DeleteDC(BufferDC);
    RaiseError(EPNGOutMemory, EPNGOutMemoryText);
  end;

  {Selects new bitmap and release old bitmap}
  OldBitmap := SelectObject(BufferDC, BufferBitmap);

  {Draws the background on the buffer image}
  StretchBlt(BufferDC, 0, 0, Header.Width, Header.height, DC, Rect.Left,
    Rect.Top, Header.Width, Header.Height, SRCCOPY);

  {Obtain number of bytes for each row}
  BytesPerRowAlpha := Header.Width;
  BytesPerRowDest := (((BitmapInfo.bmiHeader.biBitCount * Width) + 31)
    and not 31) div 8; {Number of bytes for each image row in destination}
  BytesPerRowSrc := (((Header.BitmapInfo.bmiHeader.biBitCount * Header.Width) +
    31) and not 31) div 8; {Number of bytes for each image row in source}

  {Obtains image pointers}
  ImageData := BufferBits;
  AlphaSource := Header.ImageAlpha;
  Longint(ImageSource) := Longint(Header.ImageData) +
    Header.BytesPerRow * Longint(Header.Height - 1);

  case Header.BitmapInfo.bmiHeader.biBitCount of
    {R, G, B images}
    24:
      FOR j := 1 TO Header.Height DO
      begin
        {Process all the pixels in this line}
        FOR i := 0 TO Header.Width - 1 DO
          with ImageData[i] do
          begin
            rgbRed := (255+ImageSource[2+i*3] * AlphaSource[i] + rgbRed * (255 -
              AlphaSource[i])) shr 8;
            rgbGreen := (255+ImageSource[1+i*3] * AlphaSource[i] + rgbGreen *
              (255 - AlphaSource[i])) shr 8;
            rgbBlue := (255+ImageSource[i*3] * AlphaSource[i] + rgbBlue *
             (255 - AlphaSource[i])) shr 8;
          end;

        {Move pointers}
        Longint(ImageData) := Longint(ImageData) + BytesPerRowDest;
        Longint(ImageSource) := Longint(ImageSource) - BytesPerRowSrc;
        Longint(AlphaSource) := Longint(AlphaSource) + BytesPerRowAlpha;
      end;
    {Palette images with 1 byte for each pixel}
    1,4,8: if Header.ColorType = COLOR_GRAYSCALEALPHA then
      FOR j := 1 TO Header.Height DO
      begin
        {Process all the pixels in this line}
        FOR i := 0 TO Header.Width - 1 DO
          with ImageData[i], Header.BitmapInfo do begin
            rgbRed := (255 + ImageSource[i] * AlphaSource[i] +
              rgbRed * (255 - AlphaSource[i])) shr 8;
            rgbGreen := (255 + ImageSource[i] * AlphaSource[i] +
              rgbGreen * (255 - AlphaSource[i])) shr 8;
            rgbBlue := (255 + ImageSource[i] * AlphaSource[i] +
              rgbBlue * (255 - AlphaSource[i])) shr 8;
          end;

        {Move pointers}
        Longint(ImageData) := Longint(ImageData) + BytesPerRowDest;
        Longint(ImageSource) := Longint(ImageSource) - BytesPerRowSrc;
        Longint(AlphaSource) := Longint(AlphaSource) + BytesPerRowAlpha;
      end
    else {Palette images}
    begin
      {Obtain pointer to the transparency chunk}
      TransparencyChunk := TChunktRNS(Chunks.ItemFromClass(TChunktRNS));
      PaletteChunk := TChunkPLTE(Chunks.ItemFromClass(TChunkPLTE));

      FOR j := 1 TO Header.Height DO
      begin
        {Process all the pixels in this line}
        i := 0; Data := @ImageSource[0];
        repeat
          CurBit := 0;

          repeat
            {Obtains the palette index}
            case Header.BitDepth of
              1: PaletteIndex := (Data^ shr (7-(I Mod 8))) and 1;
            2,4: PaletteIndex := (Data^ shr ((1-(I Mod 2))*4)) and $0F;
             else PaletteIndex := Data^;
            end;

            {Updates the image with the new pixel}
            with ImageData[i] do
            begin
              TransValue := TransparencyChunk.PaletteValues[PaletteIndex];
              rgbRed := (255 + PaletteChunk.Item[PaletteIndex].rgbRed *
                 TransValue + rgbRed * (255 - TransValue)) shr 8;
              rgbGreen := (255 + PaletteChunk.Item[PaletteIndex].rgbGreen *
                 TransValue + rgbGreen * (255 - TransValue)) shr 8;
              rgbBlue := (255 + PaletteChunk.Item[PaletteIndex].rgbBlue *
                 TransValue + rgbBlue * (255 - TransValue)) shr 8;
            end;

            {Move to next data}
            inc(i); inc(CurBit, Header.BitmapInfo.bmiHeader.biBitCount);
          until CurBit >= 8;
          {Move to next source data}
          inc(Data);
        until i >= Integer(Header.Width);

        {Move pointers}
        Longint(ImageData) := Longint(ImageData) + BytesPerRowDest;
        Longint(ImageSource) := Longint(ImageSource) - BytesPerRowSrc;
      end
    end {Palette images}
  end {case Header.BitmapInfo.bmiHeader.biBitCount};

  {Draws the new bitmap on the foreground}
  StretchBlt(DC, Rect.Left, Rect.Top, Header.Width, Header.Height, BufferDC,
    0, 0, Header.Width, Header.Height, SRCCOPY);

  {Free bitmap}
  SelectObject(BufferDC, OldBitmap);
  DeleteObject(BufferBitmap);
  DeleteDC(BufferDC);
end;

{Draws the image into a canvas}
procedure TPngObject.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  Header: TChunkIHDR;
begin
  {Quit in case there is no header, otherwise obtain it}
  if (Chunks.Count = 0) or not (Chunks.GetItem(0) is TChunkIHDR) then Exit;
  Header := Chunks.GetItem(0) as TChunkIHDR;

  {Copy the data to the canvas}
  case Self.TransparencyMode of
  {$IFDEF PartialTransparentDraw}
    ptmPartial:
      DrawPartialTrans(ACanvas{$IFDEF UseDelphi}.Handle{$ENDIF}, Rect);
  {$ENDIF}
    ptmBit: DrawTransparentBitmap(ACanvas{$IFDEF UseDelphi}.Handle{$ENDIF},
      Header.ImageData, Header.BitmapInfo.bmiHeader,
      pBitmapInfo(@Header.BitmapInfo), Rect,
      {$IFDEF UseDelphi}ColorToRGB({$ENDIF}TransparentColor)
      {$IFDEF UseDelphi}){$ENDIF}
    else
      StretchDiBits(ACanvas{$IFDEF UseDelphi}.Handle{$ENDIF}, Rect.Left,
        Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top, 0, 0,
        Header.Width, Header.Height, Header.ImageData,
        pBitmapInfo(@Header.BitmapInfo)^, DIB_RGB_COLORS, SRCCOPY)
  end {case}
end;

{Characters for the header}
const
  PngHeader: Array[0..7] of Char = (#137, #80, #78, #71, #13, #10, #26, #10);

{Loads the image from a stream of data}
procedure TPngObject.LoadFromStream(Stream: TStream);
var
  Header    : Array[0..7] of Char;
  HasIDAT   : Boolean;

  {Chunks reading}
  ChunkCount : Cardinal;
  ChunkLength: Cardinal;
  ChunkName  : TChunkName;
begin
  {Initialize before start loading chunks}
  ChunkCount := 0;
  ClearChunks();
  {Reads the header}
  Stream.Read(Header[0], 8);

  {Test if the header matches}
  if Header <> PngHeader then
  begin
    RaiseError(EPNGInvalidFileHeader, EPNGInvalidFileHeaderText);
    Exit;
  end;


  HasIDAT := FALSE;
  Chunks.Count := 10;

  {Load chunks}
  repeat
    inc(ChunkCount);  {Increment number of chunks}
    if Chunks.Count < ChunkCount then  {Resize the chunks list if needed}
      Chunks.Count := Chunks.Count + 10;

    {Reads chunk length and invert since it is in network order}
    {also checks the Read method return, if it returns 0, it}
    {means that no bytes was readed, probably because it reached}
    {the end of the file}
    if Stream.Read(ChunkLength, 4) = 0 then
    begin
      {In case it found the end of the file here}
      Chunks.Count := ChunkCount - 1;
      RaiseError(EPNGUnexpectedEnd, EPNGUnexpectedEndText);
    end;

    ChunkLength := ByteSwap(ChunkLength);
    {Reads chunk name}
    Stream.Read(Chunkname, 4);

    {Here we check if the first chunk is the Header which is necessary}
    {to the file in order to be a valid Portable Network Graphics image}
    if (ChunkCount = 1) and (ChunkName <> 'IHDR') then
    begin
      Chunks.Count := ChunkCount - 1;
      RaiseError(EPNGIHDRNotFirst, EPNGIHDRNotFirstText);
      exit;
    end;

    {Has a previous IDAT}
    if (HasIDAT and (ChunkName = 'IDAT')) or (ChunkName = 'cHRM') then
    begin
      dec(ChunkCount);
      Stream.Seek(ChunkLength + 4, soFromCurrent);
      Continue;
    end;
    {Tell it has an IDAT chunk}
    if ChunkName = 'IDAT' then HasIDAT := TRUE;

    {Creates object for this chunk}
    Chunks.SetItem(ChunkCount - 1, CreateClassChunk(Self, ChunkName));

    {Check if the chunk is critical and unknown}
    {$IFDEF ErrorOnUnknownCritical}
      if (TChunk(Chunks.Item[ChunkCount - 1]).ClassType = TChunk) and
        ((Byte(ChunkName[0]) AND $20) = 0) and (ChunkName <> '') then
      begin
        Chunks.Count := ChunkCount;
        RaiseError(EPNGUnknownCriticalChunk, EPNGUnknownCriticalChunkText);
      end;
    {$ENDIF}

    {Loads it}
    try if not TChunk(Chunks.Item[ChunkCount - 1]).LoadFromStream(Stream,
       ChunkName, ChunkLength) then break;
    except
      Chunks.Count := ChunkCount;
      raise;
    end;

  {Terminates when it reaches the IEND chunk}
  until (ChunkName = 'IEND');

  {Resize the list to the appropriate size}
  Chunks.Count := ChunkCount;

  {Check if there is data}
  if not HasIDAT then
    RaiseError(EPNGNoImageData, EPNGNoImageDataText);
end;

{Changing height is not supported}
procedure TPngObject.SetHeight(Value: Integer);
begin
  RaiseError(EPNGError, EPNGCannotChangeSizeText);
end;

{Changing width is not supported}
procedure TPngObject.SetWidth(Value: Integer);
begin
  RaiseError(EPNGError, EPNGCannotChangeSizeText);
end;

{$IFDEF UseDelphi}
{Saves to clipboard format (thanks to Antoine Pottern)}
procedure TPNGObject.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPalette);
begin
  with TBitmap.Create do
    try
      Width := Self.Width;
      Height := Self.Height;
      Self.Draw(Canvas, Rect(0, 0, Width, Height));
      SaveToClipboardFormat(AFormat, AData, APalette);
    finally
      Free;
    end {try}
end;

{Loads data from clipboard}
procedure TPngObject.LoadFromClipboardFormat(AFormat: Word;
  AData: THandle; APalette: HPalette);
begin
  with TBitmap.Create do
    try
      LoadFromClipboardFormat(AFormat, AData, APalette);
      Self.AssignHandle(Handle, False, 0);
    finally
      Free;
    end {try}
end;

{Returns if the image is transparent}
function TPngObject.GetTransparent: Boolean;
begin
  Result := (TransparencyMode <> ptmNone);
end;

{$ENDIF}

{Saving the PNG image to a stream of data}
procedure TPngObject.SaveToStream(Stream: TStream);
var
  j: Integer;
begin
  {Reads the header}
  Stream.Write(PNGHeader[0], 8);
  {Write each chunk}
  FOR j := 0 TO Chunks.Count - 1 DO
    Chunks.Item[j].SaveToStream(Stream)
end;

{Prepares the Header chunk}
procedure BuildHeader(Header: TChunkIHDR; Handle: HBitmap; Info: pBitmap;
  HasPalette: Boolean);
var
  DC: HDC;
begin
  {Set width and height}
  Header.Width := Info.bmWidth;
  Header.Height := abs(Info.bmHeight);
  {Set bit depth}
  if Info.bmBitsPixel >= 16 then
    Header.BitDepth := 8 else Header.BitDepth := Info.bmBitsPixel;
  {Set color type}
  if Info.bmBitsPixel >= 16 then
    Header.ColorType := COLOR_RGB else Header.ColorType := COLOR_PALETTE;
  {Set other info}
  Header.CompressionMethod := 0;  {deflate/inflate}
  Header.InterlaceMethod := 0;    {no interlace}

  {Prepares bitmap headers to hold data}
  Header.PrepareImageData();
  {Copy image data}
  DC := CreateCompatibleDC(0);
  GetDIBits(DC, Handle, 0, Header.Height, Header.ImageData,
    pBitmapInfo(@Header.BitmapInfo)^, DIB_RGB_COLORS);
  DeleteDC(DC);
end;

{Loads the image from a resource}
procedure TPngObject.LoadFromResourceName(Instance: HInst;
  const Name: String);
var
  ResStream: TResourceStream;
begin
  {Creates an especial stream to load from the resource}
  try ResStream := TResourceStream.Create(Instance, Name, RT_RCDATA);
  except RaiseError(EPNGCouldNotLoadResource, EPNGCouldNotLoadResourceText);
  exit; end;

  {Loads the png image from the resource}
  try
    LoadFromStream(ResStream);
  finally
    ResStream.Free;
  end;
end;

{Loads the png from a resource ID}
procedure TPngObject.LoadFromResourceID(Instance: HInst; ResID: Integer);
begin
  LoadFromResourceName(Instance, String(ResID));
end;

{Assigns this tpngobject to another object}
procedure TPngObject.AssignTo(Dest: TPersistent);
{$IFDEF UseDelphi}
var
  DeskDC: HDC;
  TRNS: TChunkTRNS;
{$ENDIF}
begin
  {If the destination is also a TPNGObject make it assign}
  {this one}
  if Dest is TPNGObject then
    TPNGObject(Dest).AssignPNG(Self)
  {$IFDEF UseDelphi}
  {In case the destination is a bitmap}
  else if (Dest is TBitmap) and HeaderPresent then
  begin
    {Device context}
    DeskDC := GetDC(0);
    {Copy the data}
    TBitmap(Dest).Handle := CreateDIBitmap(DeskDC,
      Header.BitmapInfo.bmiHeader, CBM_INIT, Header.ImageData,
      pBitmapInfo(@Header.BitmapInfo)^, DIB_RGB_COLORS);
    ReleaseDC(0, DeskDC);
    {Tests for the best pixelformat}
    case Header.BitmapInfo.bmiHeader.biBitCount of
      1: TBitmap(Dest).PixelFormat := pf1Bit;
      4: TBitmap(Dest).PixelFormat := pf4Bit;
      8: TBitmap(Dest).PixelFormat := pf8Bit;
     24: TBitmap(Dest).PixelFormat := pf24Bit;
     32: TBitmap(Dest).PixelFormat := pf32Bit;
    end {case Header.BitmapInfo.bmiHeader.biBitCount};

    {Copy transparency mode}
    if (TransparencyMode = ptmBit) then
    begin
      TRNS := Chunks.ItemFromClass(TChunkTRNS) as TChunkTRNS;
      TBitmap(Dest).TransparentColor := TRNS.TransparentColor;
      TBitmap(Dest).Transparent := True
    end {if (TransparencyMode = ptmBit)}

  end
  else
    {Unknown destination kind, }
    inherited AssignTo(Dest);
  {$ENDIF}
end;

{Assigns from a bitmap object}
procedure TPngObject.AssignHandle(Handle: HBitmap; Transparent: Boolean;
  TransparentColor: ColorRef);
var
  BitmapInfo: Windows.TBitmap;
  HasPalette: Boolean;

  {Chunks}
  Header: TChunkIHDR;
  PLTE: TChunkPLTE;
  IDAT: TChunkIDAT;
  IEND: TChunkIEND;
  TRNS: TChunkTRNS;
begin
  {Obtain bitmap info}
  GetObject(Handle, SizeOf(BitmapInfo), @BitmapInfo);

  {Only bit depths 1, 4 and 8 needs a palette}
  HasPalette := (BitmapInfo.bmBitsPixel < 16);

  {Clear old chunks and prepare}
  ClearChunks();

  {Create the chunks}
  Header := TChunkIHDR.Create(Self);
  if HasPalette then PLTE := TChunkPLTE.Create(Self) else PLTE := nil;
  if Transparent then TRNS := TChunkTRNS.Create(Self) else TRNS := nil;
  IDAT := TChunkIDAT.Create(Self);
  IEND := TChunkIEND.Create(Self);

  {Add chunks}
  TPNGPointerList(Chunks).Add(Header);
  if HasPalette then TPNGPointerList(Chunks).Add(PLTE);
  if Transparent then TPNGPointerList(Chunks).Add(TRNS);
  TPNGPointerList(Chunks).Add(IDAT);
  TPNGPointerList(Chunks).Add(IEND);

  {This method will fill the Header chunk with bitmap information}
  {and copy the image data}
  BuildHeader(Header, Handle, @BitmapInfo, HasPalette);
  {In case there is a image data, set the PLTE chunk fCount variable}
  {to the actual number of palette colors which is 2^(Bits for each pixel)}
  if HasPalette then PLTE.fCount := 1 shl BitmapInfo.bmBitsPixel;

  {In case it is a transparent bitmap, prepares it}
  if Transparent then TRNS.TransparentColor := TransparentColor;

end;

{Assigns from another PNG}
procedure TPngObject.AssignPNG(Source: TPNGObject);
var
  J: Integer;
begin
  {Copy properties}
  InterlaceMethod := Source.InterlaceMethod;
  MaxIdatSize := Source.MaxIdatSize;
  CompressionLevel := Source.CompressionLevel;
  Filters := Source.Filters;

  {Clear old chunks and prepare}
  ClearChunks();
  Chunks.Count := Source.Chunks.Count;
  {Create chunks and makes a copy from the source}
  FOR J := 0 TO Chunks.Count - 1 DO
    with Source.Chunks do
    begin
      Chunks.SetItem(J, TChunkClass(TChunk(Item[J]).ClassType).Create(Self));
      TChunk(Chunks.Item[J]).Assign(TChunk(Item[J]));
    end {with};
end;

{Returns a alpha data scanline}
function TPngObject.GetAlphaScanline(const LineIndex: Integer): pByteArray;
begin
  with Header do
    if (ColorType = COLOR_RGBALPHA) or (ColorType = COLOR_GRAYSCALEALPHA) then
      Longint(Result) := Longint(ImageAlpha) + (LineIndex * Longint(Width))
    else Result := nil;  {In case the image does not use alpha information}
end;

{$IFDEF Store16bits}
{Returns a png data extra scanline}
function TPngObject.GetExtraScanline(const LineIndex: Integer): Pointer;
begin
  with Header do
    Longint(Result) := (Longint(ExtraImageData) + ((Longint(Height) - 1) *
      BytesPerRow)) - (LineIndex * BytesPerRow);
end;
{$ENDIF}

{Returns a png data scanline}
function TPngObject.GetScanline(const LineIndex: Integer): Pointer;
begin
  with Header do
    Longint(Result) := (Longint(ImageData) + ((Longint(Height) - 1) *
      BytesPerRow)) - (LineIndex * BytesPerRow);
end;

{Initialize gamma table}
procedure TPngObject.InitializeGamma;
var
  i: Integer;
begin
  {Build gamma table as if there was no gamma}
  FOR i := 0 to 255 do
  begin
    GammaTable[i] := i;
    InverseGamma[i] := i;
  end {for i}
end;

{Returns the transparency mode used by this png}
function TPngObject.GetTransparencyMode: TPNGTransparencyMode;
var
  TRNS: TChunkTRNS;
begin
  with Header do
  begin
    Result := ptmNone; {Default result}
    {Gets the TRNS chunk pointer}
    TRNS := Chunks.ItemFromClass(TChunkTRNS) as TChunkTRNS;

    {Test depending on the color type}
    case ColorType of
      {This modes are always partial}
      COLOR_RGBALPHA, COLOR_GRAYSCALEALPHA: Result := ptmPartial;
      {This modes support bit transparency}
      COLOR_RGB, COLOR_GRAYSCALE: if TRNS <> nil then Result := ptmBit;
      {Supports booth translucid and bit}
      COLOR_PALETTE:
        {A TRNS chunk must be present, otherwise it won't support transparency}
        if TRNS <> nil then
          if TRNS.BitTransparency then
            Result := ptmBit else Result := ptmPartial
    end {case}

  end {with Header}
end;

{Add a text chunk}
procedure TPngObject.AddtEXt(const Keyword, Text: String);
var
  TextChunk: TChunkTEXT;
begin
  TextChunk := Chunks.Add(TChunkText) as TChunkTEXT;
  TextChunk.Keyword := Keyword;
  TextChunk.Text := Text;
end;

{Add a text chunk}
procedure TPngObject.AddzTXt(const Keyword, Text: String);
var
  TextChunk: TChunkzTXt;
begin
  TextChunk := Chunks.Add(TChunkText) as TChunkzTXt;
  TextChunk.Keyword := Keyword;
  TextChunk.Text := Text;
end;

{Removes the image transparency}
procedure TPngObject.RemoveTransparency;
var
  TRNS: TChunkTRNS;
begin
  TRNS := Chunks.ItemFromClass(TChunkTRNS) as TChunkTRNS;
  if TRNS <> nil then Chunks.RemoveChunk(TRNS)
end;

{Generates alpha information}
procedure TPngObject.CreateAlpha;
var
  TRNS: TChunkTRNS;
begin
  {Generates depending on the color type}
  with Header do
    case ColorType of
      {Png allocates different memory space to hold alpha information}
      {for these types}
      COLOR_GRAYSCALE, COLOR_RGB:
      begin
        {Transform into the appropriate color type}
        if ColorType = COLOR_GRAYSCALE then
          ColorType := COLOR_GRAYSCALEALPHA
        else ColorType := COLOR_RGBALPHA;
        {Allocates memory to hold alpha information}
        GetMem(ImageAlpha, Integer(Width) * Integer(Height));
        FillChar(ImageAlpha^, Integer(Width) * Integer(Height), #255);
      end;
      {Palette uses the TChunktRNS to store alpha}
      COLOR_PALETTE:
      begin
        {Gets/creates TRNS chunk}
        if Chunks.ItemFromClass(TChunkTRNS) = nil then
          TRNS := Chunks.Add(TChunkTRNS) as TChunkTRNS
        else
          TRNS := Chunks.ItemFromClass(TChunkTRNS) as TChunkTRNS;

          {Prepares the TRNS chunk}
          with TRNS do
          begin
            Fillchar(PaletteValues[0], 256, 255);
            fDataSize := 1 shl Header.BitDepth;
            fBitTransparency := False
          end {with Chunks.Add};
        end;
    end {case Header.ColorType}

end;

{Returns transparent color}
function TPngObject.GetTransparentColor: TColor;
var
  TRNS: TChunkTRNS;
begin
  TRNS := Chunks.ItemFromClass(TChunkTRNS) as TChunkTRNS;
  {Reads the transparency chunk to get this info}
  if Assigned(TRNS) then Result := TRNS.TransparentColor
    else Result := 0
end;

{$OPTIMIZATION OFF}
procedure TPngObject.SetTransparentColor(const Value: TColor);
var
  TRNS: TChunkTRNS;
begin
  if HeaderPresent then
    {Tests the ColorType}
    case Header.ColorType of
    {Not allowed for this modes}
    COLOR_RGBALPHA, COLOR_GRAYSCALEALPHA: Self.RaiseError(
      EPNGCannotChangeTransparent, EPNGCannotChangeTransparentText);
    {Allowed}
    COLOR_PALETTE, COLOR_RGB, COLOR_GRAYSCALE:
      begin
        TRNS := Chunks.ItemFromClass(TChunkTRNS) as TChunkTRNS;
        if not Assigned(TRNS) then TRNS := Chunks.Add(TChunkTRNS) as TChunkTRNS;

        {Sets the transparency value from TRNS chunk}
        TRNS.TransparentColor := {$IFDEF UseDelphi}ColorToRGB({$ENDIF}Value{$IFDEF UseDelphi}){$ENDIF}
      end {COLOR_PALETTE, COLOR_RGB, COLOR_GRAYSCALE)}
    end {case}
end;

{Returns if header is present}
function TPngObject.HeaderPresent: Boolean;
begin
  Result := ((Chunks.Count <> 0) and (Chunks.Item[0] is TChunkIHDR))
end;

{Returns pixel for png using palette and grayscale}
{THANY: Added correct support for BitDepth=1 and ColorType=COLOR_GRAYSCALE}
{THANY: Cast GammaTable index value to Byte to prevent access violation} 
function GetByteArrayPixel(const png: TPngObject; const X, Y: Integer): TColor;
var
  ByteData: Byte;
  DataDepth: Byte;
begin
  with png, Header do
  begin
    {Make sure the bitdepth is not greater than 8}
    DataDepth := BitDepth;
    if DataDepth > 8 then DataDepth := 8;
    {Obtains the byte containing this pixel}
    ByteData := pByteArray(png.Scanline[Y])^[X div (8 div DataDepth)];
    {Moves the bits we need to the right}
    ByteData := (ByteData shr ((8 - DataDepth) -
      (X mod (8 div DataDepth)) * DataDepth));
    {Discard the unwanted pixels}
    ByteData:= ByteData and ($FF shr (8 - DataDepth));

    {For palette mode map the palette entry and for grayscale convert and
    returns the intensity}
    case ColorType of
      COLOR_PALETTE:
        with TChunkPLTE(png.Chunks.ItemFromClass(TChunkPLTE)).Item[ByteData] do
          Result := rgb(GammaTable[rgbRed], GammaTable[rgbGreen],
            GammaTable[rgbBlue]);
      COLOR_GRAYSCALE:
      begin
        if BitDepth = 1
        then ByteData := GammaTable[Byte(ByteData * 255)]
        else ByteData := GammaTable[Byte(ByteData * ((1 shl DataDepth) + 1))];
        Result := rgb(ByteData, ByteData, ByteData);
      end;
      else Result := 0;
    end {case};
  end {with}
end;

{In case vcl units are not being used}
{$IFNDEF UseDelphi}
function ColorToRGB(const Color: TColor): COLORREF;
begin
  Result := Color
end;
{$ENDIF}

{Sets a pixel for grayscale and palette pngs}
procedure SetByteArrayPixel(const png: TPngObject; const X, Y: Integer;
  const Value: TColor);
const
  ClearFlag: Array[1..8] of Integer = (1, 3, 0, 15, 0, 0, 0, $FF);
var
  ByteData: pByte;
  DataDepth: Byte;
  ValEntry: Byte;
begin
  with png.Header do
  begin
    {Map into a palette entry}
    ValEntry := GetNearestPaletteIndex(Png.Palette, ColorToRGB(Value));

    {16 bits grayscale extra bits are discarted}
    DataDepth := BitDepth;
    if DataDepth > 8 then DataDepth := 8;
    {Gets a pointer to the byte we intend to change}
    ByteData := @pByteArray(png.Scanline[Y])^[X div (8 div DataDepth)];
    {Clears the old pixel data}
    ByteData^ := ByteData^ and not (ClearFlag[DataDepth] shl ((8 - DataDepth) -
      (X mod (8 div DataDepth)) * DataDepth));

    {Setting the new pixel}
    ByteData^ := ByteData^ or (ValEntry shl ((8 - DataDepth) -
      (X mod (8 div DataDepth)) * DataDepth));
  end {with png.Header}
end;

{Returns pixel when png uses RGB}
function GetRGBLinePixel(const png: TPngObject;
  const X, Y: Integer): TColor;
begin
  with pRGBLine(png.Scanline[Y])^[X] do
    Result := RGB(rgbtRed, rgbtGreen, rgbtBlue)
end;

{Sets pixel when png uses RGB}
procedure SetRGBLinePixel(const png: TPngObject;
 const X, Y: Integer; Value: TColor);
begin
  with pRGBLine(png.Scanline[Y])^[X] do
  begin
    rgbtRed := GetRValue(Value);
    rgbtGreen := GetGValue(Value);
    rgbtBlue := GetBValue(Value)
  end
end;

{Returns pixel when png uses grayscale}
{THANY: added GetGrayLinePixel function}
function GetGrayLinePixel(const png: TPngObject;
  const X, Y: Integer): TColor;
var
  B: Byte;
begin
  B := PByteArray(png.Scanline[Y])^[X];
  Result := RGB(B, B, B);
end;

{Sets pixel when png uses grayscale}
{THANY: added SetGrayLinePixel function}
procedure SetGrayLinePixel(const png: TPngObject;
 const X, Y: Integer; Value: TColor);
begin
  PByteArray(png.Scanline[Y])^[X] := GetRValue(Value);
end;

{Sets a pixel}
{THANY: Added support for COLOR_GRAYSCALEALPHA}
procedure TPngObject.SetPixels(const X, Y: Integer; const Value: TColor);
begin
  if (X in [0..Width - 1]) and (Y in [0..Height - 1]) then
    with Header do
    begin
      case ColorType of
        COLOR_GRAYSCALE, COLOR_PALETTE:
          SetByteArrayPixel(Self, X, Y, Value);
        COLOR_GRAYSCALEALPHA:
          SetGrayLinePixel(Self, X, Y, Value);
        else
          SetRGBLinePixel(Self, X, Y, Value)
      end; {case}
    end {with}
end;

{Returns a pixel}
{THANY: Added support for COLOR_GRAYSCALEALPHA}
function TPngObject.GetPixels(const X, Y: Integer): TColor;
begin
  if (X in [0..Width - 1]) and (Y in [0..Height - 1]) then
    with Header do
    begin
      case ColorType of
        COLOR_GRAYSCALE, COLOR_PALETTE:
          Result := GetByteArrayPixel(Self, X, Y);
        COLOR_GRAYSCALEALPHA:
          Result := GetGrayLinePixel(Self, X, Y);
        else
          Result := GetRGBLinePixel(Self, X, Y)
      end; {case}
    end {with}
  else Result := 0
end;

{Returns the image palette}
function TPngObject.GetPalette: HPALETTE;
var
  LogPalette: TMaxLogPalette;
  i: Integer;
begin
  {Palette is avaliable for COLOR_PALETTE and COLOR_GRAYSCALE modes}
  if (Header.ColorType in [COLOR_PALETTE, COLOR_GRAYSCALE])  then
  begin
    {In case the pal}
    if TempPalette = 0 then
      with LogPalette do
      begin
        {Prepares the new palette}
        palVersion := $300;
        palNumEntries := 256;
        {Copy entries}
        for i := 0 to LogPalette.palNumEntries - 1 do
        begin
          palPalEntry[i].peRed := Header.BitmapInfo.bmiColors[i].rgbRed;
          palPalEntry[i].peGreen := Header.BitmapInfo.bmiColors[i].rgbGreen;
          palPalEntry[i].peBlue := Header.BitmapInfo.bmiColors[i].rgbBlue;
          palPalEntry[i].peFlags := 0;
        end {for i};
        {Creates the palette}
        TempPalette := CreatePalette(pLogPalette(@LogPalette)^);
      end {with LogPalette, if Temppalette = 0}
  end {if Header.ColorType in ...};
  Result := TempPalette;
end;

{THANY: Added SetPalette to support writeable Palette property}
procedure TPngObject.SetPalette(Value: HPALETTE);
var
  Count, i: Integer;
  Entries: array[Byte] of TPaletteEntry;
begin
  {Palette is avaliable for COLOR_PALETTE and COLOR_GRAYSCALE modes}
  if (Header.ColorType in [COLOR_PALETTE, COLOR_GRAYSCALE])  then
  begin
    Count := GetPaletteEntries(Value, 0, 256, Entries);
    for i := 0 to Count - 1
    do begin
       Header.BitmapInfo.bmiColors[i].rgbBlue := Entries[i].peBlue;
       Header.BitmapInfo.bmiColors[i].rgbGreen := Entries[i].peGreen;
       Header.BitmapInfo.bmiColors[i].rgbRed := Entries[i].peRed;
       end; {for i}
    DeleteObject(TempPalette);
  end {if Header.ColorType in ...};
end;

initialization
  {Initialize}
  ChunkClasses := nil;
  {crc table has not being computed yet}
  crc_table_computed := FALSE;
  {Register the necessary chunks for png}
  RegisterCommonChunks;
  {Registers TPNGObject to use with TPicture}
  {$IFDEF UseDelphi}{$IFDEF RegisterGraphic}
    TPicture.RegisterFileFormat('PNG', 'Portable Network Graphics', TPNGObject);
  {$ENDIF}{$ENDIF}
finalization
  {$IFDEF UseDelphi}{$IFDEF RegisterGraphic}
    TPicture.UnregisterGraphicClass(TPNGObject);
  {$ENDIF}{$ENDIF}
  {Free chunk classes}
  FreeChunkClassList;
end.


