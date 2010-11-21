unit GraphicColor;

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
// GraphicColor contains the implementation of the color conversion manager.
// This class is responsible for converting between these color schemes/formats:
//   - RGB(A)
//   - BGR(A)
//   - CMY(K)
//   - CIE L*a*b*
//   - PhotoYCC, standard YCbCr
//   - indexed
//   - grayscale (with alpha, which is ignored currently)
//
// Additional tasks are:
//   - coversions between bit depths (1, 2, 4, 8, 16 bits)
//   - palette creation
//   - gamma tables creation and application
//   - masked pixel transfer for interlaced images
//   - big endian swap
//   - plane (planar) -> interleaved (interlaced) conversion
//
// Notes:
//   - Throughout the entire unit I used the terms BPS and SPP for "bits per sample" and
//     "samples per pixel", respectively. A sample is one component per pixel. For indexed color schemes
//     there's only 1 sample per pixel, for RGB there are 3 (red, green and blue) and so on.
//   - The bit depth of multi sample formats like RGB must be equal for each color component.
//   - Because of the large amount of possible combinations (color schemes, sample depth, gamma, byte swap)
//     I limited the accepted combinations to pratical ones. This leaves currently out:
//       + gamma correction for 16 bit values
//       + conversion to 16 bit (target) grayscale with alpha
//       + samples sizes less than 8 bits for multi-sample schemes (RGB etc.)
//       + indexed schemes with planes (e.g. 16 colors indexed as 4 planes each with one bit per sample)
//   - For now there is no conversion between indexed and non-indexed formats. Also between grayscale
//     and any other scheme is no conversion possible.
//
//----------------------------------------------------------------------------------------------------------------------

interface

{$I GraphicConfiguration.inc}

uses
  Windows, Graphics, GraphicStrings;

const
  // this is the value for average CRT monitors, adjust it if your monitor differs
  DefaultDisplayGamma = 2.2;

type
  TColorScheme = (
    csUnknown,   // not (yet) defined color scheme
    csIndexed,   // any palette format
    csG,         // gray scale
    csGA,        // gray scale with alpha channel
    csRGB,       // red, green, blue
    csRGBA,      // RGB with alpha channel
    csBGR,       // RGB in reversed order (used under Windows)
    csBGRA,      // BGR with alpha channel (alpha is always the last component)
    csCMY,       // cyan, magenta, yellow (used mainly for printing processes)
    csCMYK,      // CMY with black
    csCIELab,    // CIE color format using luminance and chromaticities
    csYCbCr,     // another format using luminance and chromaticities
    csPhotoYCC   // a modified YCbCr version used for photo CDs
  );
  
  TConvertOptions = set of (
    coAlpha,          // alpha channel is to be considered (this value is usually automatically set depending on
                      // the color scheme)
    coApplyGamma,     // target only, gamma correction must take place
    coNeedByteSwap,   // endian switch needed
    coLabByteRange,   // CIE L*a*b* only, luminance range is from 0..255 instead 0..100
    coLabChromaOffset // CIE L*a*b* only, chrominance values a and b are given in 0..255 instead -128..127
  );

  // format of the raw data to create a color palette from
  TRawPaletteFormat = (
    pfInterlaced8Triple, // rgb triple with 8 bits per component
    pfInterlaced8Quad,   // rgb quad with 8 bits per component (fourth entry is reserved as in Windows' logical palette)
    pfPlane8Triple,      // 3 separate planes of data with 8 bits per component
    pfPlane8Quad,
    pfInterlaced16Triple,// rgb triple with 16 bits per component
    pfInterlaced16Quad,
    pfPlane16Triple,     // 3 separate planes of data with 16 bits per component
    pfPlane16Quad
  );

  // TConversionMethod describes the general parameter list to which each implemented conversion method conforms.
  // Note: Source is defined as open array parameter to allow plane and interlaced source data.
  TConversionMethod = procedure(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte) of object;
  
  TColorManager = class
  private
    FChanged: Boolean;                 // set if any of the parameters changed
    FSourceBPS,                        // bits per sample of source data (allowed values are 1, 2, 4, 8, 16)
    FTargetBPS,                        // bits per sample of target data (allowed values are 1, 2, 4, 8, 16)
    FSourceSPP,                        // samples per source pixel (allowed values are 1, 3, 4)
    FTargetSPP: Byte;                  // samples per target pixel (allowed values are 1, 3, 4)
    FMainGamma,                        // primary gamma value which is usually read from a file (default is 1)
    FDisplayGamma: Single;             // (constant) gamma value of the current monitor (default is 2.2)
    FGammaTable: array[Byte] of Byte;  // contains precalculated gamma values for each possible component value
                                       // (range is 0..255)
    FYCbCrCoefficients: array[0..2] of Single;
    FHSubsampling,
    FVSubSampling: Byte;               // additional parameters used for YCbCr conversion
    FCrToRedTable,                     // lookup tables used for YCbCr conversion
    FCbToBlueTable,
    FCrToGreenTable,                                       
    FCbToGreenTable: array of Integer;

    FSourceScheme,
    FTargetScheme: TColorScheme;
    FRowConversion: TConversionMethod; // procedure variable for the actual conversion method used
    FSourceOptions,
    FTargetOptions: TConvertOptions;   // options to control conversion
  protected
    // Low level conversion helper used to convert one pixel component.
    function ComponentGammaConvert(Value: Byte): Byte;
    function ComponentNoConvert16(Value: Word): Word;
    function ComponentNoConvert8(Value: Byte): Byte;
    function ComponentScaleConvert(Value: Word): Byte;
    function ComponentScaleGammaConvert(Value: Word): Byte;
    function ComponentSwapScaleGammaConvert(Value: Word): Byte;
    function ComponentSwapScaleConvert(Value: Word): Byte;
    function ComponentSwapConvert(Value: Word): Word;

    // row conversion routines
    procedure RowConvertBGR2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertBGR2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertCIELAB2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertCIELAB2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertCMYK2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertCMYK2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertGray(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertIndexed8(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertIndexedBoth16(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertIndexedSource16(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertIndexedTarget16(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertRGB2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertRGB2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertPhotoYCC2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertPhotoYCC2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertYCbCr2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertYCbCr2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

    // other general routines
    procedure CreateYCbCrLookup;
    function GetPixelFormat(Index: Integer): TPixelFormat;
    procedure PrepareConversion;
    procedure SetSourceBitsPerSample(const Value: Byte);
    procedure SetSourceColorScheme(const Value: TColorScheme);
    procedure SetSourceSamplesPerPixel(const Value: Byte);
    procedure SetTargetBitsPerSample(const Value: Byte);
    procedure SetTargetColorScheme(const Value: TColorScheme);
    procedure SetTargetSamplesPerPixel(const Value: Byte);
  public
    constructor Create;

    procedure ConvertRow(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    function CreateColorPalette(Data: array of Pointer; DataFormat: TRawPaletteFormat; ColorCount: Cardinal;
      RGB: Boolean): HPALETTE;
    function CreateGrayscalePalette(MinimumIsWhite: Boolean): HPALETTE;
    procedure Error(const Msg: String);
    procedure SetGamma(MainGamma: Single; DisplayGamma: Single = DefaultDisplayGamma);
    procedure SetYCbCrParameters(Values: array of Single; HSubSampling, VSubSampling: Byte);

    property SourceBitsPerSample: Byte read FSourceBPS write SetSourceBitsPerSample;
    property SourceColorScheme: TColorScheme read FSourceScheme write SetSourceColorScheme;
    property SourceOptions: TConvertOptions read FSourceOptions write FSourceOptions;
    property SourcePixelFormat: TPixelFormat index 0 read GetPixelFormat;
    property SourceSamplesPerPixel: Byte read FSourceSPP write SetSourceSamplesPerPixel;
    property TargetBitsPerSample: Byte read FTargetBPS write SetTargetBitsPerSample;
    property TargetColorScheme: TColorScheme read FTargetScheme write SetTargetColorScheme;
    property TargetOptions: TConvertOptions read FTargetOptions write FTargetOptions;
    property TargetPixelFormat: TPixelFormat index 1 read GetPixelFormat;
    property TargetSamplesPerPixel: Byte read FTargetSPP write SetTargetSamplesPerPixel;
  end;

function ClampByte(Value: Integer): Byte;
function MulDiv16(Number, Numerator, Denominator: Word): Word;
  
//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Math, SysUtils;

type
  EColorConversionError = class(Exception);

  PCMYK = ^TCMYK;
  TCMYK = packed record
    C, M, Y, K: Byte;
  end;

  PCMYK16 = ^TCMYK16;
  TCMYK16 = packed record
    C, M, Y, K: Word;
  end;

  PCMY = ^TCMY;
  TCMY = packed record
    C, M, Y: Byte;
  end;

  PCMY16 = ^TCMY16;
  TCMY16 = packed record
    C, M, Y: Word;
  end;
  
  PRGB = ^TRGB;
  TRGB = packed record
    R, G, B: Byte;
  end;

  PRGB16 = ^TRGB16;
  TRGB16 = packed record
    R, G, B: Word;
  end;

  PRGBA = ^TRGBA;
  TRGBA = packed record
    R, G, B, A: Byte;
  end;

  PRGBA16 = ^TRGBA16;
  TRGBA16 = packed record
    R, G, B, A: Word;
  end;

  PBGR = ^TBGR;
  TBGR = packed record
    B, G, R: Byte;
  end;

  PBGR16 = ^TBGR16;
  TBGR16 = packed record
    B, G, R: Word;
  end;

  PBGRA = ^TBGRA;
  TBGRA = packed record
    B, G, R, A: Byte;
  end;

  PBGRA16 = ^TBGRA16;
  TBGRA16 = packed record
    B, G, R, A: Word;
  end;

//----------------- helper functions -----------------------------------------------------------------------------------

function ClampByte(Value: Integer): Byte;

// ensures Value is in the range 0..255, values < 0 are clamped to 0 and values > 255 are clamped to 255

asm
         OR EAX, EAX
         JNS @@positive
         XOR EAX, EAX
         RET

@@positive:
         CMP EAX, 255
         JBE @@OK
         MOV EAX, 255
@@OK:
end;

//----------------------------------------------------------------------------------------------------------------------

function MulDiv16(Number, Numerator, Denominator: Word): Word;

// faster equivalent to Windows' MulDiv function
// Number is passed via AX
// Numerator is passed via DX
// Denominator is passed via CX
// Result is passed via AX
// Note: No error checking takes place. Denominator must be > 0!

asm
         MUL DX
         DIV CX
end;

//----------------- TColorManager --------------------------------------------------------------------------------------

constructor TColorManager.Create;

// set some default values

begin
  FSourceBPS := 8;
  FTargetBPS := 8;                             
  FSourceSPP := 3; // 24 bit format
  FTargetSPP := 3; // 24 bit format
  SetGamma(1, DefaultDisplayGamma);
  FSourceScheme := csRGB;
  FTargetScheme := csBGR;

  // defaults are from CCIR Recommendation 601-1
  FYCbCrCoefficients[0] := 0.299;
  FYCbCrCoefficients[1] := 0.587;
  FYCbCrCoefficients[2] := 0.114;

  FHSubSampling := 1;
  FVSubSampling := 1;

  FChanged := True;
end;

//----------------- low level conversion routines ----------------------------------------------------------------------

// These routines are used for conversions from 16 to 8 bit values, either with gamma correction or byte swap (or both).

function TColorManager.ComponentNoConvert8(Value: Byte): Byte;

begin
  Result := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorManager.ComponentNoConvert16(Value: Word): Word;

begin
  Result := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorManager.ComponentGammaConvert(Value: Byte): Byte;

begin
  Result := FGammaTable[Value];
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorManager.ComponentScaleConvert(Value: Word): Byte;

begin
  Result := MulDiv16(Value, 255, 65535);
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorManager.ComponentScaleGammaConvert(Value: Word): Byte;

begin
  Result := FGammaTable[MulDiv16(Value, 255, 65535)];
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorManager.ComponentSwapScaleGammaConvert(Value: Word): Byte;

begin
  Result := FGammaTable[MulDiv16(Swap(Value), 255, 65535)];
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorManager.ComponentSwapScaleConvert(Value: Word): Byte;

begin
  Result := MulDiv16(Swap(Value), 255, 65535);
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorManager.ComponentSwapConvert(Value: Word): Word;

begin
  Result := Swap(Value);
end;

//----------------- row conversion routines ----------------------------------------------------------------------------

// Notes: Each method takes parameters for source and target data as well as the count of pixels to work on. This count
//        determines the number of pixels in the target buffer. The actual source count may differ for special color
//        schemes (like YCbCr) or interlaced lines.
//        Mask is a parameter which determines (in a repeative manner) which source pixel should actually be transferred
//        to the target buffer. A 1 in the corresponding bit (MSB is leftmost pixel) causes the transfer to happen.
//        Usually, this parameter is $FF to transfer all pixels, but for interlaced images (e.g. as in PNG format)
//        this will differ to limit pixel transfers. The bit mask only describes which target pixel is to skip. Source
//        pixel must be packed.
//        Windows DIBs are always byte aligned, so we don't need checks for byte alignments (in target).

procedure TColorManager.RowConvertBGR2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// same as ConvertBGR2RGB but for BGR target schemes

var
  SourceR16,
  SourceG16,
  SourceB16,
  SourceA16: PWord;

  SourceR8,
  SourceG8,
  SourceB8,
  SourceA8: PByte;

  TargetRun16: PBGR16;
  TargetRunA16: PBGRA16;
  TargetRun8: PBGR;
  TargetRunA8: PBGRA;
  BitRun: Byte;

  Convert8_8: function(Value: Byte): Byte of object;
  Convert16_8: function(Value: Word): Byte of object;
  Convert16_8Alpha: function(Value: Word): Byte of object;
  Convert16_16: function(Value: Word): Word of object;

  SourceIncrement,
  TargetIncrement: Cardinal;
  CopyAlpha: Boolean;

begin
  BitRun := $80;
  // determine alpha handling once
  CopyAlpha := False;
  if coAlpha in FSourceOptions then
  begin
    SourceIncrement := SizeOf(TRGBA);
    TargetIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then CopyAlpha := True;
  end
  else
  begin
    SourceIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then TargetIncrement := SizeOf(TRGBA)
                                 else TargetIncrement := SizeOf(TRGB);
  end;
  // in planar mode source increment is always 1
  if Length(Source) > 1 then SourceIncrement := 1;

  case FSourceBPS of
    8:
      begin
        if Length(Source) = 1 then
        begin
          // interleaved mode
          SourceB8 := Source[0];
          SourceG8 := SourceB8; Inc(SourceG8);
          SourceR8 := SourceG8; Inc(SourceR8);
          SourceA8 := SourceR8; Inc(SourceA8);
        end
        else
        begin
          SourceB8 := Source[0];
          SourceG8 := Source[1];
          SourceR8 := Source[2];
          if coAlpha in FSourceOptions then SourceA8 := Source[3]
                                       else SourceA8 := nil;
        end;

        case FTargetBPS of
          8: // 888 to 888
            begin
              if coApplyGamma in FTargetOptions then Convert8_8 := ComponentGammaConvert
                                                else Convert8_8 := ComponentNoConvert8;
              if CopyAlpha then
              begin
                TargetRunA8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA8.R := Convert8_8(SourceR8^);
                    TargetRunA8.G := Convert8_8(SourceG8^);
                    TargetRunA8.B := Convert8_8(SourceB8^);
                    // alpha values are never gamma corrected
                    TargetRunA8.A := SourceA8^;
                  
                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                    Inc(SourceA8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA8);
                end;
              end
              else
              begin
                TargetRun8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun8.R := Convert8_8(SourceR8^);
                    TargetRun8.G := Convert8_8(SourceG8^);
                    TargetRun8.B := Convert8_8(SourceB8^);

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PByte(TargetRun8), TargetIncrement);
                end;
              end;
            end;
          16: // 888 to 161616
            begin
              if coApplyGamma in FTargetOptions then Convert8_8 := ComponentGammaConvert
                                                else Convert8_8 := ComponentNoConvert8;
              if coNeedByteSwap in FSourceOptions then Convert16_16 := ComponentSwapConvert
                                                  else Convert16_16 := ComponentNoConvert16;
              if Length(Source) = 1 then
              begin
                SourceB8 := Source[0];
                SourceG8 := SourceB8; Inc(SourceG8);
                SourceR8 := SourceG8; Inc(SourceR8);
                SourceA8 := SourceR8; Inc(SourceA8);
              end
              else
              begin
                SourceB8 := Source[0];
                SourceG8 := Source[1];
                SourceR8 := Source[2];
                if coAlpha in FSourceOptions then SourceA8 := Source[3]
                                             else SourceA8 := nil;
              end;

              if CopyAlpha then
              begin
                TargetRunA16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), 65535, 255));
                    TargetRunA16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), 65535, 255));
                    TargetRunA16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), 65535, 255));
                    TargetRunA16.A := Convert16_16(MulDiv16(SourceA8^, 65535, 255));

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                    Inc(SourceA8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA16);
                end;
              end
              else
              begin
                TargetRun16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), 65535, 255));
                    TargetRun16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), 65535, 255));
                    TargetRun16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), 65535, 255));

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PWord(TargetRun16), TargetIncrement);
                end;
              end;
            end;
        end;
      end;
    16:
      begin
        if Length(Source) = 1 then
        begin
          SourceB16 := Source[0];
          SourceG16 := SourceB16; Inc(SourceG16);
          SourceR16 := SourceG16; Inc(SourceR16);
          SourceA16 := SourceR16; Inc(SourceA16);
        end
        else
        begin
          SourceB16 := Source[0];
          SourceG16 := Source[1];
          SourceR16 := Source[2];
          if coAlpha in FSourceOptions then SourceA16 := Source[3]
                                       else SourceA16 := nil;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              if coApplyGamma in FTargetOptions then
              begin
                if coNeedByteSwap in FSourceOptions then Convert16_8 := ComponentSwapScaleGammaConvert
                                                    else Convert16_8 := ComponentScaleGammaConvert;
              end
              else
              begin
                if coNeedByteSwap in FSourceOptions then Convert16_8 := ComponentSwapScaleConvert
                                                    else Convert16_8 := ComponentScaleConvert;
              end;
              // since alpha channels are never gamma corrected we need a separate conversion routine
              if coNeedByteSwap in FSourceOptions then Convert16_8Alpha := ComponentSwapScaleConvert
                                                  else Convert16_8Alpha := ComponentScaleConvert;

              if CopyAlpha then
              begin
                TargetRunA8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA8.R := Convert16_8(SourceR16^);
                    TargetRunA8.G := Convert16_8(SourceG16^);
                    TargetRunA8.B := Convert16_8(SourceB16^);
                    TargetRunA8.A := Convert16_8Alpha(SourceA16^);
                  
                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                    Inc(SourceA16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA8);
                end;
              end
              else
              begin
                TargetRun8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun8.R := Convert16_8(SourceR16^);
                    TargetRun8.G := Convert16_8(SourceG16^);
                    TargetRun8.B := Convert16_8(SourceB16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PByte(TargetRun8), TargetIncrement);
                end;
              end;
            end;
          16: // 161616 to 161616
            begin
              // no gamma correction for 16 bit samples yet
              if coNeedByteSwap in FSourceOptions then Convert16_16 := ComponentSwapConvert
                                                  else Convert16_16 := ComponentNoConvert16;

              if Length(Source) = 1 then
              begin
                SourceB16 := Source[0];
                SourceG16 := SourceB16; Inc(SourceG16);
                SourceR16 := SourceG16; Inc(SourceR16);
                SourceA16 := SourceR16; Inc(SourceA16);
              end
              else
              begin
                SourceB16 := Source[0];
                SourceG16 := Source[1];
                SourceR16 := Source[2];
                if coAlpha in FSourceOptions then SourceA16 := Source[3]
                                             else SourceA16 := nil;
              end;

              if CopyAlpha then
              begin
                TargetRunA16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA16.R := Convert16_16(SourceR16^);
                    TargetRunA16.G := Convert16_16(SourceG16^);
                    TargetRunA16.B := Convert16_16(SourceB16^);
                    TargetRunA16.A := Convert16_16(SourceA16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                    Inc(SourceA16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA16);
                end;
              end
              else
              begin
                TargetRun16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun16.R := Convert16_16(SourceR16^);
                    TargetRun16.G := Convert16_16(SourceG16^);
                    TargetRun16.B := Convert16_16(SourceB16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PWord(TargetRun16), TargetIncrement);
                end;
              end;
            end;
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.RowConvertBGR2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// Converts BGR source schemes to RGB target schemes and takes care for byte swapping, alpha copy/skip and
// gamma correction. 

var
  SourceR16,
  SourceG16,
  SourceB16,
  SourceA16: PWord;

  SourceR8,
  SourceG8,
  SourceB8,
  SourceA8: PByte;

  TargetRun16: PRGB16;
  TargetRunA16: PRGBA16;
  TargetRun8: PRGB;
  TargetRunA8: PRGBA;
  BitRun: Byte;

  Convert8_8: function(Value: Byte): Byte of object;
  Convert16_8: function(Value: Word): Byte of object;
  Convert16_8Alpha: function(Value: Word): Byte of object;
  Convert16_16: function(Value: Word): Word of object;

  SourceIncrement,
  TargetIncrement: Cardinal;
  CopyAlpha: Boolean;

begin
  BitRun := $80;
  // determine alpha handling once
  CopyAlpha := False;
  if coAlpha in FSourceOptions then
  begin
    SourceIncrement := SizeOf(TRGBA);
    TargetIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then CopyAlpha := True;
  end
  else
  begin
    SourceIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then TargetIncrement := SizeOf(TRGBA)
                                 else TargetIncrement := SizeOf(TRGB);
  end;
  // in planar mode source increment is always 1
  if Length(Source) > 1 then SourceIncrement := 1;

  case FSourceBPS of
    8:
      begin
        if Length(Source) = 1 then
        begin
          // interleaved mode
          SourceB8 := Source[0];
          SourceG8 := SourceB8; Inc(SourceG8);
          SourceR8 := SourceG8; Inc(SourceR8);
          SourceA8 := SourceR8; Inc(SourceA8);
        end
        else
        begin
          SourceB8 := Source[0];
          SourceG8 := Source[1];
          SourceR8 := Source[2];
          if coAlpha in FSourceOptions then SourceA8 := Source[3]
                                       else SourceA8 := nil;
        end;

        case FTargetBPS of
          8: // 888 to 888
            begin
              if coApplyGamma in FTargetOptions then Convert8_8 := ComponentGammaConvert
                                                else Convert8_8 := ComponentNoConvert8;
              if CopyAlpha then
              begin
                TargetRunA8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA8.R := Convert8_8(SourceR8^);
                    TargetRunA8.G := Convert8_8(SourceG8^);
                    TargetRunA8.B := Convert8_8(SourceB8^);
                    // alpha values are never gamma corrected
                    TargetRunA8.A := SourceA8^;
                  
                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                    Inc(SourceA8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA8);
                end;
              end
              else
              begin
                TargetRun8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun8.R := Convert8_8(SourceR8^);
                    TargetRun8.G := Convert8_8(SourceG8^);
                    TargetRun8.B := Convert8_8(SourceB8^);

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PByte(TargetRun8), TargetIncrement);
                end;
              end;
            end;
          16: // 888 to 161616
            begin
              if coApplyGamma in FTargetOptions then Convert8_8 := ComponentGammaConvert
                                                else Convert8_8 := ComponentNoConvert8;
              if coNeedByteSwap in FSourceOptions then Convert16_16 := ComponentSwapConvert
                                                  else Convert16_16 := ComponentNoConvert16;
              if Length(Source) = 1 then
              begin
                SourceB8 := Source[0];
                SourceG8 := SourceB8; Inc(SourceG8);
                SourceR8 := SourceG8; Inc(SourceR8);
                SourceA8 := SourceR8; Inc(SourceA8);
              end
              else
              begin
                SourceB8 := Source[0];
                SourceG8 := Source[1];
                SourceR8 := Source[2];
                if coAlpha in FSourceOptions then SourceA8 := Source[3]
                                             else SourceA8 := nil;
              end;

              if CopyAlpha then
              begin
                TargetRunA16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), 65535, 255));
                    TargetRunA16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), 65535, 255));
                    TargetRunA16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), 65535, 255));
                    TargetRunA16.A := Convert16_16(MulDiv16(SourceA8^, 65535, 255));

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                    Inc(SourceA8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA16);
                end;
              end
              else
              begin
                TargetRun16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), 65535, 255));
                    TargetRun16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), 65535, 255));
                    TargetRun16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), 65535, 255));

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PWord(TargetRun16), TargetIncrement);
                end;
              end;
            end;
        end;
      end;
    16:
      begin
        if Length(Source) = 1 then
        begin
          SourceB16 := Source[0];
          SourceG16 := SourceB16; Inc(SourceG16);
          SourceR16 := SourceG16; Inc(SourceR16);
          SourceA16 := SourceR16; Inc(SourceA16);
        end
        else
        begin
          SourceB16 := Source[0];
          SourceG16 := Source[1];
          SourceR16 := Source[2];
          if coAlpha in FSourceOptions then SourceA16 := Source[3]
                                       else SourceA16 := nil;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              if coApplyGamma in FTargetOptions then
              begin
                if coNeedByteSwap in FSourceOptions then Convert16_8 := ComponentSwapScaleGammaConvert
                                                    else Convert16_8 := ComponentScaleGammaConvert;
              end
              else
              begin
                if coNeedByteSwap in FSourceOptions then Convert16_8 := ComponentSwapScaleConvert
                                                    else Convert16_8 := ComponentScaleConvert;
              end;
              // since alpha channels are never gamma corrected we need a separate conversion routine
              if coNeedByteSwap in FSourceOptions then Convert16_8Alpha := ComponentSwapScaleConvert
                                                  else Convert16_8Alpha := ComponentScaleConvert;

              if CopyAlpha then
              begin
                TargetRunA8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA8.R := Convert16_8(SourceR16^);
                    TargetRunA8.G := Convert16_8(SourceG16^);
                    TargetRunA8.B := Convert16_8(SourceB16^);
                    TargetRunA8.A := Convert16_8Alpha(SourceA16^);
                  
                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                    Inc(SourceA16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA8);
                end;
              end
              else
              begin
                TargetRun8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun8.R := Convert16_8(SourceR16^);
                    TargetRun8.G := Convert16_8(SourceG16^);
                    TargetRun8.B := Convert16_8(SourceB16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PByte(TargetRun8), TargetIncrement);
                end;
              end;
            end;
          16: // 161616 to 161616
            begin
              // no gamma correction for 16 bit samples yet
              if coNeedByteSwap in FSourceOptions then Convert16_16 := ComponentSwapConvert
                                                  else Convert16_16 := ComponentNoConvert16;

              if Length(Source) = 1 then
              begin
                SourceB16 := Source[0];
                SourceG16 := SourceB16; Inc(SourceG16);
                SourceR16 := SourceG16; Inc(SourceR16);
                SourceA16 := SourceR16; Inc(SourceA16);
              end
              else
              begin
                SourceB16 := Source[0];
                SourceG16 := Source[1];
                SourceR16 := Source[2];
                if coAlpha in FSourceOptions then SourceA16 := Source[3]
                                             else SourceA16 := nil;
              end;

              if CopyAlpha then
              begin
                TargetRunA16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA16.R := Convert16_16(SourceR16^);
                    TargetRunA16.G := Convert16_16(SourceG16^);
                    TargetRunA16.B := Convert16_16(SourceB16^);
                    TargetRunA16.A := Convert16_16(SourceA16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                    Inc(SourceA16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA16);
                end;
              end
              else
              begin
                TargetRun16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun16.R := Convert16_16(SourceR16^);
                    TargetRun16.G := Convert16_16(SourceG16^);
                    TargetRun16.B := Convert16_16(SourceB16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PWord(TargetRun16), TargetIncrement);
                end;
              end;
            end;
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.RowConvertCIELAB2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// conversion of the CIE L*a*b color space to BGR using a two way approach assuming a D65 white point,
// first a conversion to CIE XYZ is performed and then from there to RGB

var
  LRun8,
  aRun8,
  bRun8: PByte;
  LRun16,
  aRun16,
  bRun16: PWord;
  L, a, b,
  X, Y, Z, // color values in float format
  T,
  YYn3: Extended;  // intermediate results
  Target8: PByte;
  Target16: PWord;
  Increment: Integer;
  AlphaSkip: Integer;
  BitRun: Byte;

begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions); // 0 if no alpha must be skipped, otherwise 1

  case FSourceBPS of
    8:
      begin
        if Length(Source) = 1 then
        begin
          LRun8 := Source[0];
          aRun8 := LRun8; Inc(aRun8);
          bRun8 := aRun8; Inc(bRun8);
          Increment := 3;
        end
        else
        begin
          LRun8 := Source[0];
          aRun8 := Source[1];
          bRun8 := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: /// 888 to 888
            begin
              Target8 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  if coLabByteRange in FSourceOptions then L := LRun8^ / 2.55
                                                      else L := LRun8^;
                  Inc(LRun8, Increment);
                  if coLabChromaOffset in FSourceOptions then
                  begin
                    a := aRun8^ - 128;
                    Inc(aRun8, Increment);
                    b := bRun8^ - 128;
                    Inc(bRun8, Increment);
                  end
                  else
                  begin
                    a := ShortInt(aRun8^);
                    Inc(aRun8, Increment);
                    b := ShortInt(bRun8^);
                    Inc(bRun8, Increment);
                  end;

                  YYn3 := (L + 16) / 116; // this corresponds to (Y/Yn)^1/3
                  if L < 7.9996 then
                  begin
                    Y := L / 903.3;
                    X := a / 3893.5 + Y;
                    Z := Y - b / 1557.4;
                  end
                  else
                  begin
                    T := YYn3 + a / 500;
                    X := T * T * T;
                    Y := YYn3 * YYn3 * YYn3;
                    T := YYn3 - b / 200;
                    Z := T * T * T;
                  end;

                  // once we have CIE XYZ it is easy (yet quite expensive) to calculate RGB values from this
                  // blue
                  Target8^ := ClampByte(Round(255 * ( 0.099 * X - 0.198 * Y + 1.099 * Z)));
                  Inc(Target8);
                  // green
                  Target8^ := ClampByte(Round(255 * (-0.952 * X + 1.893 * Y + 0.059 * Z)));
                  Inc(Target8);
                  // red
                  Target8^ := ClampByte(Round(255 * ( 2.998 * X - 1.458 * Y - 0.541 * Z)));
                  Inc(Target8, 1 + AlphaSkip);
                end
                else Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 888 to 161616
            begin
              Target16 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  if coLabByteRange in FSourceOptions then L := LRun8^ / 2.55
                                                      else L := LRun8^;
                  Inc(LRun8, Increment);
                  if coLabChromaOffset in FSourceOptions then
                  begin
                    a := aRun8^ - 128;
                    Inc(aRun8, Increment);
                    b := bRun8^ - 128;
                    Inc(bRun8, Increment);
                  end
                  else
                  begin
                    a := ShortInt(aRun8^);
                    Inc(aRun8, Increment);
                    b := ShortInt(bRun8^);
                    Inc(bRun8, Increment);
                  end;

                  YYn3 := (L + 16) / 116; // this corresponds to (Y/Yn)^1/3
                  if L < 7.9996 then
                  begin
                    Y := L / 903.3;
                    X := a / 3893.5 + Y;
                    Z := Y - b / 1557.4;
                  end
                  else
                  begin
                    T := YYn3 + a / 500;
                    X := T * T * T;
                    Y := YYn3 * YYn3 * YYn3;
                    T := YYn3 - b / 200;
                    Z := T * T * T;
                  end;

                  // blue
                  Target16^ := MulDiv16(ClampByte(Round(255 * ( 0.099 * X - 0.198 * Y + 1.099 * Z))), 65535, 255);
                  Inc(Target16);
                  // green
                  Target16^ := MulDiv16(ClampByte(Round(255 * (-0.952 * X + 1.893 * Y + 0.059 * Z))), 65535, 255);
                  Inc(Target16);
                  // red
                  Target16^ := MulDiv16(ClampByte(Round(255 * ( 2.998 * X - 1.458 * Y - 0.541 * Z))), 65535, 255);
                  Inc(Target16, 1 + AlphaSkip);
                end
                else Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end
            end;
        end;
      end;
    16: 
      begin
        if Length(Source) = 1 then
        begin
          LRun16 := Source[0];
          aRun16 := LRun16; Inc(aRun16);
          bRun16 := aRun16; Inc(bRun16);
          Increment := 3;
        end
        else
        begin
          LRun16 := Source[0];
          aRun16 := Source[1];
          bRun16 := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              Target8 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  if coLabByteRange in FSourceOptions then L := LRun16^ / 2.55
                                                      else L := LRun16^;
                  Inc(LRun16, Increment);
                  
                  if coLabChromaOffset in FSourceOptions then
                  begin
                    a := aRun16^ - 128;
                    Inc(aRun16, Increment);
                    b := bRun16^ - 128;
                    Inc(bRun16, Increment);
                  end
                  else
                  begin
                    a := ShortInt(aRun16^);
                    Inc(aRun16, Increment);
                    b := ShortInt(bRun16^);
                    Inc(bRun16, Increment);
                  end;

                  YYn3 := (L + 16) / 116; // this corresponds to (Y/Yn)^1/3
                  if L < 7.9996 then
                  begin
                    Y := L / 903.3;
                    X := a / 3893.5 + Y;
                    Z := Y - b / 1557.4;
                  end
                  else
                  begin
                    T := YYn3 + a / 500;
                    X := T * T * T;
                    Y := YYn3 * YYn3 * YYn3;
                    T := YYn3 - b / 200;
                    Z := T * T * T;
                  end;

                  // blue
                  Target8^ := ClampByte(Round(255 * ( 0.099 * X - 0.198 * Y + 1.099 * Z)));
                  Inc(Target8);
                  // green
                  Target8^ := ClampByte(Round(255 * (-0.952 * X + 1.893 * Y + 0.059 * Z)));
                  Inc(Target8);
                  // red
                  Target8^ := ClampByte(Round(255 * ( 2.998 * X - 1.458 * Y - 0.541 * Z)));
                  Inc(Target8, 1 + AlphaSkip);
                end
                else Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 161616 to 161616
            begin
              Target16 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  if coLabByteRange in FSourceOptions then L := LRun16^ / 2.55
                                                      else L := LRun16^;
                  Inc(LRun16, Increment);
                  if coLabChromaOffset in FSourceOptions then
                  begin
                    a := aRun16^ - 128;
                    Inc(aRun16, Increment);
                    b := bRun16^ - 128;
                    Inc(bRun16, Increment);
                  end
                  else
                  begin
                    a := ShortInt(aRun16^);
                    Inc(aRun16, Increment);
                    b := ShortInt(bRun16^);
                    Inc(bRun16, Increment);
                  end;

                  YYn3 := (L + 16) / 116; // this corresponds to (Y/Yn)^1/3
                  if L < 7.9996 then
                  begin
                    Y := L / 903.3;
                    X := a / 3893.5 + Y;
                    Z := Y - b / 1557.4;
                  end
                  else
                  begin
                    T := YYn3 + a / 500;
                    X := T * T * T;
                    Y := YYn3 * YYn3 * YYn3;
                    T := YYn3 - b / 200;
                    Z := T * T * T;
                  end;

                  // blue
                  Target16^ := ClampByte(Round(255 * ( 0.099 * X - 0.198 * Y + 1.099 * Z)));
                  Inc(Target16);
                  // green
                  Target16^ := ClampByte(Round(255 * (-0.952 * X + 1.893 * Y + 0.059 * Z)));
                  Inc(Target16);
                  // red
                  Target16^ := ClampByte(Round(255 * ( 2.998 * X - 1.458 * Y - 0.541 * Z)));
                  Inc(Target16, 1 + AlphaSkip);
                end
                else Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.RowConvertCIELAB2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// just like RowConvertCIELAB2BGR but for RGB target schemes

var
  LRun8,
  aRun8,
  bRun8: PByte;
  LRun16,
  aRun16,
  bRun16: PWord;
  L, a, b,
  X, Y, Z, // color values in float format
  T,
  YYn3: Extended;  // intermediate results
  Target8: PByte;
  Target16: PWord;
  Increment: Integer;
  AlphaSkip: Integer;
  BitRun: Byte;

begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions); // 0 if no alpha must be skipped, otherwise 1

  case FSourceBPS of
    8:
      begin
        if Length(Source) = 1 then
        begin
          LRun8 := Source[0];
          aRun8 := LRun8; Inc(aRun8);
          bRun8 := aRun8; Inc(bRun8);
          Increment := 3;
        end
        else
        begin
          LRun8 := Source[0];
          aRun8 := Source[1];
          bRun8 := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: /// 888 to 888
            begin
              Target8 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  if coLabByteRange in FSourceOptions then L := LRun8^ / 2.55
                                                      else L := LRun8^;
                  Inc(LRun8, Increment);
                  if coLabChromaOffset in FSourceOptions then
                  begin
                    a := aRun8^ - 128;
                    Inc(aRun8, Increment);
                    b := bRun8^ - 128;
                    Inc(bRun8, Increment);
                  end
                  else
                  begin
                    a := ShortInt(aRun8^);
                    Inc(aRun8, Increment);
                    b := ShortInt(bRun8^);
                    Inc(bRun8, Increment);
                  end;

                  YYn3 := (L + 16) / 116; // this corresponds to (Y/Yn)^1/3
                  if L < 7.9996 then
                  begin
                    Y := L / 903.3;
                    X := a / 3893.5 + Y;
                    Z := Y - b / 1557.4;
                  end
                  else
                  begin
                    T := YYn3 + a / 500;
                    X := T * T * T;
                    Y := YYn3 * YYn3 * YYn3;
                    T := YYn3 - b / 200;
                    Z := T * T * T;
                  end;

                  // once we have CIE XYZ it is easy (yet quite expensive) to calculate RGB values from this
                  // red
                  Target8^ := ClampByte(Round(255 * ( 2.998 * X - 1.458 * Y - 0.541 * Z)));
                  Inc(Target8);
                  // green
                  Target8^ := ClampByte(Round(255 * (-0.952 * X + 1.893 * Y + 0.059 * Z)));
                  Inc(Target8);
                  // blue
                  Target8^ := ClampByte(Round(255 * ( 0.099 * X - 0.198 * Y + 1.099 * Z)));
                  Inc(Target8, 1 + AlphaSkip);
                end
                else Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 888 to 161616
            begin
              Target16 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  if coLabByteRange in FSourceOptions then L := LRun8^ / 2.55
                                                      else L := LRun8^;
                  Inc(LRun8, Increment);
                  if coLabChromaOffset in FSourceOptions then
                  begin
                    a := aRun8^ - 128;
                    Inc(aRun8, Increment);
                    b := bRun8^ - 128;
                    Inc(bRun8, Increment);
                  end
                  else
                  begin
                    a := ShortInt(aRun8^);
                    Inc(aRun8, Increment);
                    b := ShortInt(bRun8^);
                    Inc(bRun8, Increment);
                  end;

                  YYn3 := (L + 16) / 116; // this corresponds to (Y/Yn)^1/3
                  if L < 7.9996 then
                  begin
                    Y := L / 903.3;
                    X := a / 3893.5 + Y;
                    Z := Y - b / 1557.4;
                  end
                  else
                  begin
                    T := YYn3 + a / 500;
                    X := T * T * T;
                    Y := YYn3 * YYn3 * YYn3;
                    T := YYn3 - b / 200;
                    Z := T * T * T;
                  end;

                  // red
                  Target16^ := MulDiv16(ClampByte(Round(255 * ( 2.998 * X - 1.458 * Y - 0.541 * Z))), 65535, 255);
                  Inc(Target16);
                  // green
                  Target16^ := MulDiv16(ClampByte(Round(255 * (-0.952 * X + 1.893 * Y + 0.059 * Z))), 65535, 255);
                  Inc(Target16);
                  // blue
                  Target16^ := MulDiv16(ClampByte(Round(255 * ( 0.099 * X - 0.198 * Y + 1.099 * Z))), 65535, 255);
                  Inc(Target16, 1 + AlphaSkip);
                end
                else Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end
            end;
        end;
      end;
    16: 
      begin
        if Length(Source) = 1 then
        begin
          LRun16 := Source[0];
          aRun16 := LRun16; Inc(aRun16);
          bRun16 := aRun16; Inc(bRun16);
          Increment := 3;
        end
        else
        begin
          LRun16 := Source[0];
          aRun16 := Source[1];
          bRun16 := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              Target8 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  if coLabByteRange in FSourceOptions then L := LRun16^ / 2.55
                                                      else L := LRun16^;
                  Inc(LRun16, Increment);
                  if coLabChromaOffset in FSourceOptions then
                  begin
                    a := aRun16^ - 128;
                    Inc(aRun16, Increment);
                    b := bRun16^ - 128;
                    Inc(bRun16, Increment);
                  end
                  else
                  begin
                    a := ShortInt(aRun16^);
                    Inc(aRun16, Increment);
                    b := ShortInt(bRun16^);
                    Inc(bRun16, Increment);
                  end;

                  YYn3 := (L + 16) / 116; // this corresponds to (Y/Yn)^1/3
                  if L < 7.9996 then
                  begin
                    Y := L / 903.3;
                    X := a / 3893.5 + Y;
                    Z := Y - b / 1557.4;
                  end
                  else
                  begin
                    T := YYn3 + a / 500;
                    X := T * T * T;
                    Y := YYn3 * YYn3 * YYn3;
                    T := YYn3 - b / 200;
                    Z := T * T * T;
                  end;

                  // red
                  Target8^ := ClampByte(Round(255 * ( 2.998 * X - 1.458 * Y - 0.541 * Z)));
                  Inc(Target8);
                  // green
                  Target8^ := ClampByte(Round(255 * (-0.952 * X + 1.893 * Y + 0.059 * Z)));
                  Inc(Target8);
                  // blue
                  Target8^ := ClampByte(Round(255 * ( 0.099 * X - 0.198 * Y + 1.099 * Z)));
                  Inc(Target8, 1 + AlphaSkip);
                end
                else Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 161616 to 161616
            begin
              Target16 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  if coLabByteRange in FSourceOptions then L := LRun16^ / 2.55
                                                      else L := LRun16^;
                  Inc(LRun16, Increment);
                  if coLabChromaOffset in FSourceOptions then
                  begin
                    a := aRun16^ - 128;
                    Inc(aRun16, Increment);
                    b := bRun16^ - 128;
                    Inc(bRun16, Increment);
                  end
                  else
                  begin
                    a := ShortInt(aRun16^);
                    Inc(aRun16, Increment);
                    b := ShortInt(bRun16^);
                    Inc(bRun16, Increment);
                  end;

                  YYn3 := (L + 16) / 116; // this corresponds to (Y/Yn)^1/3
                  if L < 7.9996 then
                  begin
                    Y := L / 903.3;
                    X := a / 3893.5 + Y;
                    Z := Y - b / 1557.4;
                  end
                  else
                  begin
                    T := YYn3 + a / 500;
                    X := T * T * T;
                    Y := YYn3 * YYn3 * YYn3;
                    T := YYn3 - b / 200;
                    Z := T * T * T;
                  end;

                  // red
                  Target16^ := ClampByte(Round(255 * ( 2.998 * X - 1.458 * Y - 0.541 * Z)));
                  Inc(Target16);
                  // green
                  Target16^ := ClampByte(Round(255 * (-0.952 * X + 1.893 * Y + 0.059 * Z)));
                  Inc(Target16);
                  // blue
                  Target16^ := ClampByte(Round(255 * ( 0.099 * X - 0.198 * Y + 1.099 * Z)));
                  Inc(Target16, 1 + AlphaSkip);
                end
                else Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.RowConvertCMYK2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// converts a stream of Count CMYK values to BGR

var
  C8, M8, Y8, K8: PByte;
  C16, M16, Y16, K16: PWord;
  Target8: PByte;
  Target16: PWord;
  Increment: Integer;
  AlphaSkip: Integer;
  BitRun: Byte;

begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions); // 0 if no alpha must be skipped, otherwise 1

  case FSourceBPS of
    8:
      begin
        if Length(Source) = 4 then
        begin
          // plane mode
          C8 := Source[0];
          M8 := Source[1];
          Y8 := Source[2];
          K8 := Source[3];
          Increment := 1;
        end
        else
        begin
          // interleaved mode
          C8 := Source[0];
          M8 := C8; Inc(M8);
          Y8 := M8; Inc(Y8);
          K8 := Y8; Inc(K8);
          Increment := 4;
        end;

        case FTargetBPS of
          8: // 888 to 888
            begin
              Target8 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  // blue
                  Target8^ := ClampByte(255 - (Y8^ - MulDiv16(Y8^, K8^, 255) + K8^));
                  Inc(Target8);
                  // green
                  Target8^ := ClampByte(255 - (M8^ - MulDiv16(M8^, K8^, 255) + K8^));
                  Inc(Target8);
                  // blue
                  Target8^ := ClampByte(255 - (C8^ - MulDiv16(C8^, K8^, 255) + K8^));
                  Inc(Target8, 1 + AlphaSkip);
                  
                  Inc(C8, Increment);
                  Inc(M8, Increment);
                  Inc(Y8, Increment);
                  Inc(K8, Increment);
                end
                else Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 888 to 161616
            begin
              Target16 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  // blue
                  Target16^ := MulDiv16(ClampByte(255 - (Y8^ - MulDiv16(Y8^, K8^, 255) + K8^)), 65535, 255);
                  Inc(Target16);
                  // green
                  Target16^ := MulDiv16(ClampByte(255 - (M8^ - MulDiv16(M8^, K8^, 255) + K8^)), 65535, 255);
                  Inc(Target16);
                  // blue
                  Target16^ := MulDiv16(ClampByte(255 - (C8^ - MulDiv16(C8^, K8^, 255) + K8^)), 65535, 255);
                  Inc(Target16, 1 + AlphaSkip);

                  Inc(C8, Increment);
                  Inc(M8, Increment);
                  Inc(Y8, Increment);
                  Inc(K8, Increment);
                end
                else Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
    16:
      begin
        if Length(Source) = 4 then
        begin
          // plane mode
          C16 := Source[0];
          M16 := Source[1];
          Y16 := Source[2];
          K16 := Source[3];
          Increment := 1;
        end
        else
        begin
          // interleaved mode
          C16 := Source[0];
          M16 := C16; Inc(M16);
          Y16 := M16; Inc(Y16);
          K16 := Y16; Inc(K16);
          Increment := 4;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              Target8 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  // blue
                  Target8^ := ClampByte(255 - MulDiv16((Y16^ - MulDiv16(Y16^, K16^, 65535) + K16^), 255, 65535));
                  Inc(Target8);
                  // green
                  Target8^ := ClampByte(255 - MulDiv16((M16^ - MulDiv16(M16^, K16^, 65535) + K16^), 255, 65535));
                  Inc(Target8);
                  // blue
                  Target8^ := ClampByte(255 - MulDiv16((C16^ - MulDiv16(C16^, K16^, 65535) + K16^), 255, 65535));
                  Inc(Target8, 1 + AlphaSkip);

                  Inc(C16, Increment);
                  Inc(M16, Increment);
                  Inc(Y16, Increment);
                  Inc(K16, Increment);
                end
                else Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 161616 to 161616
            begin
              Target16 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  // blue
                  Target16^ := 65535 - (Y16^ - MulDiv16(Y16^, K16^, 65535) + K16^);
                  Inc(Target16);
                  // green
                  Target16^ := 65535 - (M16^ - MulDiv16(M16^, K16^, 65535) + K16^);
                  Inc(Target16);
                  // blue
                  Target16^ := 65535 - (C16^ - MulDiv16(C16^, K16^, 65535) + K16^);
                  Inc(Target16, 1 + AlphaSkip);

                  Inc(C16, Increment);
                  Inc(M16, Increment);
                  Inc(Y16, Increment);
                  Inc(K16, Increment);
                end
                else Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.RowConvertCMYK2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// converts a stream of Count CMYK values to RGB,

var
  C8, M8, Y8, K8: PByte;
  C16, M16, Y16, K16: PWord;
  Target8: PByte;
  Target16: PWord;
  Increment: Integer;
  AlphaSkip: Integer;
  BitRun: Byte;

begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions); // 0 if no alpha must be skipped, otherwise 1

  case FSourceBPS of
    8:
      begin
        if Length(Source) = 4 then
        begin
          // plane mode
          C8 := Source[0];
          M8 := Source[1];
          Y8 := Source[2];
          K8 := Source[3];
          Increment := 1;
        end
        else
        begin
          // interleaved mode
          C8 := Source[0];
          M8 := C8; Inc(M8);
          Y8 := M8; Inc(Y8);
          K8 := Y8; Inc(K8);
          Increment := 4;
        end;

        case FTargetBPS of
          8: // 888 to 888
            begin
              Target8 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  // red
                  Target8^ := ClampByte(255 - (C8^ - MulDiv16(C8^, K8^, 255) + K8^));
                  Inc(Target8);
                  // green
                  Target8^ := ClampByte(255 - (M8^ - MulDiv16(M8^, K8^, 255) + K8^));
                  Inc(Target8);
                  // blue
                  Target8^ := ClampByte(255 - (Y8^ - MulDiv16(Y8^, K8^, 255) + K8^));
                  Inc(Target8, 1 + AlphaSkip);
                  
                  Inc(C8, Increment);
                  Inc(M8, Increment);
                  Inc(Y8, Increment);
                  Inc(K8, Increment);
                end
                else Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 888 to 161616
            begin
              Target16 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  // red
                  Target16^ := MulDiv16(ClampByte(255 - (C8^ - MulDiv16(C8^, K8^, 255) + K8^)), 65535, 255);
                  Inc(Target16);
                  // green
                  Target16^ := MulDiv16(ClampByte(255 - (M8^ - MulDiv16(M8^, K8^, 255) + K8^)), 65535, 255);
                  Inc(Target16);
                  // blue
                  Target16^ := MulDiv16(ClampByte(255 - (Y8^ - MulDiv16(Y8^, K8^, 255) + K8^)), 65535, 255);
                  Inc(Target16, 1 + AlphaSkip);

                  Inc(C8, Increment);
                  Inc(M8, Increment);
                  Inc(Y8, Increment);
                  Inc(K8, Increment);
                end
                else Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
    16:
      begin
        if Length(Source) = 4 then
        begin
          // plane mode
          C16 := Source[0];
          M16 := Source[1];
          Y16 := Source[2];
          K16 := Source[3];
          Increment := 1;
        end
        else
        begin
          // interleaved mode
          C16 := Source[0];
          M16 := C16; Inc(M16);
          Y16 := M16; Inc(Y16);
          K16 := Y16; Inc(K16);
          Increment := 4;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              Target8 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  // red
                  Target8^ := ClampByte(255 - MulDiv16((C16^ - MulDiv16(C16^, K16^, 65535) + K16^), 255, 65535));
                  Inc(Target8);
                  // green
                  Target8^ := ClampByte(255 - MulDiv16((M16^ - MulDiv16(M16^, K16^, 65535) + K16^), 255, 65535));
                  Inc(Target8);
                  // blue
                  Target8^ := ClampByte(255 - MulDiv16((Y16^ - MulDiv16(Y16^, K16^, 65535) + K16^), 255, 65535));
                  Inc(Target8, 1 + AlphaSkip);

                  Inc(C16, Increment);
                  Inc(M16, Increment);
                  Inc(Y16, Increment);
                  Inc(K16, Increment);
                end
                else Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 161616 to 161616
            begin
              Target16 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  // red
                  Target16^ := 65535 - (C16^ - MulDiv16(C16^, K16^, 65535) + K16^);
                  Inc(Target16);
                  // green
                  Target16^ := 65535 - (M16^ - MulDiv16(M16^, K16^, 65535) + K16^);
                  Inc(Target16);
                  // blue
                  Target16^ := 65535 - (Y16^ - MulDiv16(Y16^, K16^, 65535) + K16^);
                  Inc(Target16, 1 + AlphaSkip);

                  Inc(C16, Increment);
                  Inc(M16, Increment);
                  Inc(Y16, Increment);
                  Inc(K16, Increment);
                end
                else Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.RowConvertGray(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// conversion from source grayscale (possibly with alpha) to target grayscale
// Note: Since grayscale is basically handled like indexed mode (palette), there is no need to
//       handle gamma correction here as this happend already during palette creation.

var
  Target8: PByte;
  Target16: PWord;
  Source8: PByte;
  Source16: PWord;
  BitRun: Byte;
  AlphaSkip: Integer;
  Convert16: function(Value: Word): Byte of object;

begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FSourceOptions); // 0 if no alpha must be skipped, otherwise 1

  case FSourceBPS of
    8:
      case FTargetBPS of
        8: // 888 to 888
          begin
            Source8 := Source[0];
            Target8 := Target;

            while Count > 0 do
            begin
              if Boolean(Mask and BitRun) then
              begin
                Target8^ := Source8^;
                Inc(Source8, 1 + AlphaSkip);
              end;
              asm ROR BYTE PTR [BitRun], 1 end;
              Dec(Count);
              Inc(Target8);
            end;
          end;
        16: // 888 to 161616
          begin
            Source8 := Source[0];
            Target16 := Target;
            while Count > 0 do
            begin
              if Boolean(Mask and BitRun) then
              begin
                Target16^ := MulDiv16(Source8^, 65535, 255);
                Inc(Source8, 1 + AlphaSkip);
              end;
              asm ROR BYTE PTR [BitRun], 1 end;
              Dec(Count);
              Inc(Target16);
            end;
          end;
      end;
    16:
      case FTargetBPS of
        8: // 161616 to 888
          begin
            Source16 := Source[0];
            Target8 := Target;
            if coNeedByteSwap in FSourceOptions then Convert16 := ComponentSwapScaleConvert
                                                else Convert16 := ComponentScaleConvert;

            while Count > 0 do
            begin
              if Boolean(Mask and BitRun) then
              begin
                Target8^ := Convert16(Source16^);
                Inc(Source16, 1 + AlphaSkip);
              end;
              asm ROR BYTE PTR [BitRun], 1 end;
              Dec(Count);
              Inc(Target8);
            end;
          end;
        16: // 161616 to 161616
          begin
            Source16 := Source[0];
            Target16 := Target;

            if coNeedByteSwap in FSourceOptions then
            begin
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Target16^ := Swap(Source16^);
                  Inc(Source16, 1 + AlphaSkip);
                end;
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
                Inc(Target16);
              end;
            end
            else
            begin
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Target16^ := Source16^;
                  Inc(Source16, 1 + AlphaSkip);
                end;
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
                Inc(Target16);
              end;
            end;
          end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.RowConvertIndexed8(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// This is the core conversion routine for indexed pixel formats.
// This routine takes care about sample scaling and interlacing.
// Note: 16 bit indexed mode is a bit different (words instead bytes and byte swap) and handled separately.

var
  SourceRun,
  TargetRun: PByte;
  Value,
  BitRun,
  TargetMask,
  SourceMask,
  SourceShift,
  TargetShift,
  MaxInSample,
  MaxOutSample,
  SourceBPS,          // local copies to ease assembler access
  TargetBPS: Byte;
  Done: Cardinal;

begin
  SourceRun := Source[0];
  TargetRun := Target;

  if (FSourceBPS = FTargetBPS) and (Mask = $FF) then Move(SourceRun^, TargetRun^, (Count * FSourceBPS + 7) div 8)
                                                else
  begin
    BitRun := $80;
    // make a copy of these both values from private variables to local variables
    // to ease access during assembler parts in the code
    SourceBPS := FSourceBPS;
    TargetBPS := FTargetBPS;
    SourceMask := Byte(not ((1 shl (8 - SourceBPS)) - 1));
    MaxInSample := (1 shl SourceBPS) - 1;
    TargetMask := (1 shl (8 - TargetBPS)) - 1;
    MaxOutSample := (1 shl TargetBPS) - 1;
    SourceShift := 8;
    TargetShift := 8 - TargetBPS;
    Done := 0;
    while Done < Count do
    begin
      if Boolean(Mask and BitRun) then
      begin
        // adjust shift value by source bit depth
        Dec(SourceShift, SourceBPS);
        Value := (SourceRun^ and SourceMask) shr SourceShift;
        Value := MulDiv16(Value, MaxOutSample, MaxInSample);
        TargetRun^ := (TargetRun^ and TargetMask) or (Value shl TargetShift);
        if SourceShift = 0 then
        begin
          SourceShift := 8;
          Inc(SourceRun);
        end;
        asm
          MOV CL, [SourceBPS]
          ROR BYTE PTR [SourceMask], CL // roll source bit mask with source bit count
        end;
      end;

      asm
        ROR BYTE PTR [BitRun], 1      // adjust test bit mask
        MOV CL, [TargetBPS]
        ROR BYTE PTR [TargetMask], CL // roll target mask with target bit count
      end;
      if TargetShift = 0 then TargetShift := 8 - TargetBPS
                         else Dec(TargetShift, TargetBPS);
      Inc(Done);
      // advance target pointer every (8 div target bit count)
      if (Done mod (8 div TargetBPS)) = 0 then Inc(TargetRun);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.RowConvertIndexedBoth16(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// This is the core conversion routine for indexed pixel formats with 16 bits per sample values involved
// (special version for source and target resolution being both 16 BPS).

var
  TargetRun,
  SourceRun: PWord;
  BitRun: Byte;

begin
  SourceRun := Source[0];
  TargetRun := Target;
  BitRun := $80;

  if coNeedByteSwap in FSourceOptions then
  begin
    while Count > 0 do
    begin
      if Boolean(Mask and BitRun) then
      begin
        TargetRun^ := Swap(SourceRun^);
        Inc(SourceRun);
      end;
      asm ROR BYTE PTR [BitRun], 1 end;
      Dec(Count);
      Inc(TargetRun);
    end;
  end
  else
  begin
    if Mask = $FF then Move(SourceRun^, TargetRun^, 2 * Count)
                  else
      while Count > 0 do
      begin
        if Boolean(Mask and BitRun) then
        begin
          TargetRun^ := SourceRun^;
          Inc(SourceRun);
        end;
        asm ROR BYTE PTR [BitRun], 1 end;
        Dec(Count);
        Inc(TargetRun);
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.RowConvertIndexedSource16(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// This is the core conversion routine for indexed pixel formats with 16 bits per source sample values involved.

var
  TargetRun8: PByte;
  SourceRun16: PWord;
  Value,
  BitRun,
  TargetMask,
  TargetShift,
  MaxOutSample,
  TargetBPS: Byte;    // local copies to ease assembler access

begin
  SourceRun16 := Source[0];
  TargetRun8 := Target;
  BitRun := $80;
  // make a copy of these both values from private variables to local variables
  // to ease access during assembler parts in the code
  TargetBPS := FTargetBPS;
  TargetMask := (1 shl (8 - TargetBPS)) - 1;
  MaxOutSample := (1 shl TargetBPS) - 1;
  TargetShift := 8 - TargetBPS;
  while Count > 0 do
  begin
    if Boolean(Mask and BitRun) then
    begin
      if coNeedByteSwap in FSourceOptions then Value := MulDiv16(Swap(SourceRun16^), MaxOutSample, 65535)
                                          else Value := MulDiv16(SourceRun16^, MaxOutSample, 65535);
      TargetRun8^ := (TargetRun8^ and TargetMask) or (Value shl TargetShift);
      Inc(SourceRun16);
    end;

    asm
      ROR BYTE PTR [BitRun], 1      // adjust test bit mask
      MOV CL, [TargetBPS]
      ROR BYTE PTR [TargetMask], CL // roll target mask with target bit count
    end;
    if TargetShift = 0 then TargetShift := 8 - TargetBPS
                       else Dec(TargetShift, TargetBPS);
    Dec(Count);
    // advance target pointer every (8 div target bit count)
    if (Count mod (8 div TargetBPS)) = 0 then Inc(TargetRun8);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.RowConvertIndexedTarget16(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// This is the core conversion routine for indexed pixel formats with 16 bits per target sample values involved.

var
  SourceRun8: PByte;
  TargetRun16: PWord;
  Value: Word;
  BitRun,
  SourceMask,
  SourceShift,
  MaxInSample,
  SourceBPS: Byte;

begin
  SourceRun8 := Source[0];
  TargetRun16 := Target;
  BitRun := $80;
  SourceBPS := FSourceBPS;
  SourceMask := Byte(not ((1 shl (8 - SourceBPS)) - 1));
  MaxInSample := (1 shl SourceBPS) - 1;
  SourceShift := 8;
  while Count > 0 do
  begin
    if Boolean(Mask and BitRun) then
    begin
      // adjust shift value by source bit depth
      Dec(SourceShift, SourceBPS);
      Value := (SourceRun8^ and SourceMask) shr SourceShift;
      Value := MulDiv16(Value, 65535, MaxInSample);
      if coNeedByteSwap in FSourceOptions then TargetRun16^ := Swap(Value)
                                          else TargetRun16^ := Value;
      if SourceShift = 0 then
      begin
        SourceShift := 8;
        Inc(SourceRun8);
      end;
      asm
        MOV CL, [SourceBPS]
        ROR BYTE PTR [SourceMask], CL // roll source bit mask with source bit count
      end;
    end;

    asm
      ROR BYTE PTR [BitRun], 1      // adjust test bit mask
    end;

    Dec(Count);
    // advance target pointer every (8 div target bit count)
    Inc(TargetRun16);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.RowConvertRGB2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// Converts RGB source schemes to BGR target schemes and takes care for byte swapping, alpha copy/skip and
// gamma correction. 

var
  SourceR16,
  SourceG16,
  SourceB16,
  SourceA16: PWord;

  SourceR8,
  SourceG8,
  SourceB8,
  SourceA8: PByte;

  TargetRun16: PBGR16;
  TargetRunA16: PBGRA16;
  TargetRun8: PBGR;
  TargetRunA8: PBGRA;
  BitRun: Byte;

  Convert8_8: function(Value: Byte): Byte of object;
  Convert16_8: function(Value: Word): Byte of object;
  Convert16_8Alpha: function(Value: Word): Byte of object;
  Convert16_16: function(Value: Word): Word of object;

  SourceIncrement,
  TargetIncrement: Cardinal;
  CopyAlpha: Boolean;

begin
  BitRun := $80;
  // determine alpha handling once
  CopyAlpha := False;
  if coAlpha in FSourceOptions then
  begin
    // byte size of components doesn't matter as the increments are applied to
    // pointers whose data types determine the final increment
    SourceIncrement := SizeOf(TRGBA);
    TargetIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then CopyAlpha := True;
  end
  else
  begin
    SourceIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then TargetIncrement := SizeOf(TRGBA)
                                 else TargetIncrement := SizeOf(TRGB);
  end;
  // in planar mode source increment is always 1
  if Length(Source) > 1 then SourceIncrement := 1;

  case FSourceBPS of
    8:
      begin
        if Length(Source) = 1 then
        begin
          // interleaved mode
          SourceR8 := Source[0];
          SourceG8 := SourceR8; Inc(SourceG8);
          SourceB8 := SourceG8; Inc(SourceB8);
          SourceA8 := SourceB8; Inc(SourceA8);
        end
        else
        begin
          SourceR8 := Source[0];
          SourceG8 := Source[1];
          SourceB8 := Source[2];
          if coAlpha in FSourceOptions then SourceA8 := Source[3]
                                       else SourceA8 := nil;
        end;

        case FTargetBPS of
          8: // 888 to 888
            begin
              if coApplyGamma in FTargetOptions then Convert8_8 := ComponentGammaConvert
                                                else Convert8_8 := ComponentNoConvert8;
              if CopyAlpha then
              begin
                TargetRunA8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA8.R := Convert8_8(SourceR8^);
                    TargetRunA8.G := Convert8_8(SourceG8^);
                    TargetRunA8.B := Convert8_8(SourceB8^);
                    // alpha values are never gamma corrected
                    TargetRunA8.A := SourceA8^;
                  
                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                    Inc(SourceA8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA8);
                end;
              end
              else
              begin
                TargetRun8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun8.R := Convert8_8(SourceR8^);
                    TargetRun8.G := Convert8_8(SourceG8^);
                    TargetRun8.B := Convert8_8(SourceB8^);

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PByte(TargetRun8), TargetIncrement);
                end;
              end;
            end;
          16: // 888 to 161616
            begin
              if coApplyGamma in FTargetOptions then Convert8_8 := ComponentGammaConvert
                                                else Convert8_8 := ComponentNoConvert8;
              if coNeedByteSwap in FSourceOptions then Convert16_16 := ComponentSwapConvert
                                                  else Convert16_16 := ComponentNoConvert16;
              if Length(Source) = 1 then
              begin
                SourceB8 := Source[0];
                SourceG8 := SourceB8; Inc(SourceG8);
                SourceR8 := SourceG8; Inc(SourceR8);
                SourceA8 := SourceR8; Inc(SourceA8);
              end
              else
              begin
                SourceB8 := Source[0];
                SourceG8 := Source[1];
                SourceR8 := Source[2];
                if coAlpha in FSourceOptions then SourceA8 := Source[3]
                                             else SourceA8 := nil;
              end;

              if CopyAlpha then
              begin
                TargetRunA16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), 65535, 255));
                    TargetRunA16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), 65535, 255));
                    TargetRunA16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), 65535, 255));
                    TargetRunA16.A := Convert16_16(MulDiv16(SourceA8^, 65535, 255));

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                    Inc(SourceA8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA16);
                end;
              end
              else
              begin
                TargetRun16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), 65535, 255));
                    TargetRun16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), 65535, 255));
                    TargetRun16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), 65535, 255));

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PWord(TargetRun16), TargetIncrement);
                end;
              end;
            end;
        end;
      end;
    16:
      begin
        if Length(Source) = 1 then
        begin
          SourceR16 := Source[0];
          SourceG16 := SourceR16; Inc(SourceG16);
          SourceB16 := SourceG16; Inc(SourceB16);
          SourceA16 := SourceB16; Inc(SourceA16);
        end
        else
        begin
          SourceR16 := Source[0];
          SourceG16 := Source[1];
          SourceB16 := Source[2];
          if coAlpha in FSourceOptions then SourceA16 := Source[3]
                                       else SourceA16 := nil;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              if coApplyGamma in FTargetOptions then
              begin
                if coNeedByteSwap in FSourceOptions then Convert16_8 := ComponentSwapScaleGammaConvert
                                                    else Convert16_8 := ComponentScaleGammaConvert;
              end
              else
              begin
                if coNeedByteSwap in FSourceOptions then Convert16_8 := ComponentSwapScaleConvert
                                                    else Convert16_8 := ComponentScaleConvert;
              end;
              // since alpha channels are never gamma corrected we need a separate conversion routine
              if coNeedByteSwap in FSourceOptions then Convert16_8Alpha := ComponentSwapScaleConvert
                                                  else Convert16_8Alpha := ComponentScaleConvert;

              if CopyAlpha then
              begin
                TargetRunA8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA8.R := Convert16_8(SourceR16^);
                    TargetRunA8.G := Convert16_8(SourceG16^);
                    TargetRunA8.B := Convert16_8(SourceB16^);
                    TargetRunA8.A := Convert16_8Alpha(SourceA16^);
                  
                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                    Inc(SourceA16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA8);
                end;
              end
              else
              begin
                TargetRun8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun8.R := Convert16_8(SourceR16^);
                    TargetRun8.G := Convert16_8(SourceG16^);
                    TargetRun8.B := Convert16_8(SourceB16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PByte(TargetRun8), TargetIncrement);
                end;
              end;
            end;
          16: // 161616 to 161616
            begin
              // no gamma correction for 16 bit samples yet
              if coNeedByteSwap in FSourceOptions then Convert16_16 := ComponentSwapConvert
                                                  else Convert16_16 := ComponentNoConvert16;

              if Length(Source) = 1 then
              begin
                SourceB16 := Source[0];
                SourceG16 := SourceB16; Inc(SourceG16);
                SourceR16 := SourceG16; Inc(SourceR16);
                SourceA16 := SourceR16; Inc(SourceA16);
              end
              else
              begin
                SourceB16 := Source[0];
                SourceG16 := Source[1];
                SourceR16 := Source[2];
                if coAlpha in FSourceOptions then SourceA16 := Source[3]
                                             else SourceA16 := nil;
              end;

              if CopyAlpha then
              begin
                TargetRunA16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA16.R := Convert16_16(SourceR16^);
                    TargetRunA16.G := Convert16_16(SourceG16^);
                    TargetRunA16.B := Convert16_16(SourceB16^);
                    TargetRunA16.A := Convert16_16(SourceA16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                    Inc(SourceA16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA16);
                end;
              end
              else
              begin
                TargetRun16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun16.R := Convert16_16(SourceR16^);
                    TargetRun16.G := Convert16_16(SourceG16^);
                    TargetRun16.B := Convert16_16(SourceB16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PWord(TargetRun16), TargetIncrement);
                end;
              end;
            end;
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.RowConvertRGB2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// same as ConvertRGB2BGR but for RGB target schemes

var
  SourceR16,
  SourceG16,
  SourceB16,
  SourceA16: PWord;

  SourceR8,
  SourceG8,
  SourceB8,
  SourceA8: PByte;

  TargetRun16: PRGB16;
  TargetRunA16: PRGBA16;
  TargetRun8: PRGB;
  TargetRunA8: PRGBA;
  BitRun: Byte;

  Convert8_8: function(Value: Byte): Byte of object;
  Convert16_8: function(Value: Word): Byte of object;
  Convert16_8Alpha: function(Value: Word): Byte of object;
  Convert16_16: function(Value: Word): Word of object;

  SourceIncrement,
  TargetIncrement: Cardinal;
  CopyAlpha: Boolean;

begin
  BitRun := $80;
  // determine alpha handling once
  CopyAlpha := False;
  if coAlpha in FSourceOptions then
  begin
    SourceIncrement := SizeOf(TRGBA);
    TargetIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then CopyAlpha := True;
  end
  else
  begin
    SourceIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then TargetIncrement := SizeOf(TRGBA)
                                 else TargetIncrement := SizeOf(TRGB);
  end;
  // in planar mode source increment is always 1
  if Length(Source) > 1 then SourceIncrement := 1;

  case FSourceBPS of
    8:
      begin
        if Length(Source) = 1 then
        begin
          // interleaved mode
          SourceR8 := Source[0];
          SourceG8 := SourceR8; Inc(SourceG8);
          SourceB8 := SourceG8; Inc(SourceB8);
          SourceA8 := SourceB8; Inc(SourceA8);
        end
        else
        begin
          SourceR8 := Source[0];
          SourceG8 := Source[1];
          SourceB8 := Source[2];
          if coAlpha in FSourceOptions then SourceA8 := Source[3]
                                       else SourceA8 := nil;
        end;

        case FTargetBPS of
          8: // 888 to 888
            begin
              if coApplyGamma in FTargetOptions then Convert8_8 := ComponentGammaConvert
                                                else Convert8_8 := ComponentNoConvert8;
              if CopyAlpha then
              begin
                TargetRunA8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA8.R := Convert8_8(SourceR8^);
                    TargetRunA8.G := Convert8_8(SourceG8^);
                    TargetRunA8.B := Convert8_8(SourceB8^);
                    // alpha values are never gamma corrected
                    TargetRunA8.A := SourceA8^;
                  
                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                    Inc(SourceA8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA8);
                end;
              end
              else
              begin
                TargetRun8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun8.R := Convert8_8(SourceR8^);
                    TargetRun8.G := Convert8_8(SourceG8^);
                    TargetRun8.B := Convert8_8(SourceB8^);

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PByte(TargetRun8), TargetIncrement);
                end;
              end;
            end;
          16: // 888 to 161616
            begin
              if coApplyGamma in FTargetOptions then Convert8_8 := ComponentGammaConvert
                                                else Convert8_8 := ComponentNoConvert8;
              if coNeedByteSwap in FSourceOptions then Convert16_16 := ComponentSwapConvert
                                                  else Convert16_16 := ComponentNoConvert16;
              if Length(Source) = 1 then
              begin
                SourceB8 := Source[0];
                SourceG8 := SourceB8; Inc(SourceG8);
                SourceR8 := SourceG8; Inc(SourceR8);
                SourceA8 := SourceR8; Inc(SourceA8);
              end
              else
              begin
                SourceB8 := Source[0];
                SourceG8 := Source[1];
                SourceR8 := Source[2];
                if coAlpha in FSourceOptions then SourceA8 := Source[3]
                                             else SourceA8 := nil;
              end;

              if CopyAlpha then
              begin
                TargetRunA16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), 65535, 255));
                    TargetRunA16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), 65535, 255));
                    TargetRunA16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), 65535, 255));
                    TargetRunA16.A := Convert16_16(MulDiv16(SourceA8^, 65535, 255));

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                    Inc(SourceA8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA16);
                end;
              end
              else
              begin
                TargetRun16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), 65535, 255));
                    TargetRun16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), 65535, 255));
                    TargetRun16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), 65535, 255));

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PWord(TargetRun16), TargetIncrement);
                end;
              end;
            end;
        end;
      end;
    16:
      begin
        if Length(Source) = 1 then
        begin
          SourceR16 := Source[0];
          SourceG16 := SourceR16; Inc(SourceG16);
          SourceB16 := SourceG16; Inc(SourceB16);
          SourceA16 := SourceB16; Inc(SourceA16);
        end
        else
        begin
          SourceR16 := Source[0];
          SourceG16 := Source[1];
          SourceB16 := Source[2];
          if coAlpha in FSourceOptions then SourceA16 := Source[3]
                                       else SourceA16 := nil;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              if coApplyGamma in FTargetOptions then
              begin
                if coNeedByteSwap in FSourceOptions then Convert16_8 := ComponentSwapScaleGammaConvert
                                                    else Convert16_8 := ComponentScaleGammaConvert;
              end
              else
              begin
                if coNeedByteSwap in FSourceOptions then Convert16_8 := ComponentSwapScaleConvert
                                                    else Convert16_8 := ComponentScaleConvert;
              end;
              // since alpha channels are never gamma corrected we need a separate conversion routine
              if coNeedByteSwap in FSourceOptions then Convert16_8Alpha := ComponentSwapScaleConvert
                                                  else Convert16_8Alpha := ComponentScaleConvert;

              if CopyAlpha then
              begin
                TargetRunA8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA8.R := Convert16_8(SourceR16^);
                    TargetRunA8.G := Convert16_8(SourceG16^);
                    TargetRunA8.B := Convert16_8(SourceB16^);
                    TargetRunA8.A := Convert16_8Alpha(SourceA16^);
                  
                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                    Inc(SourceA16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA8);
                end;
              end
              else
              begin
                TargetRun8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun8.R := Convert16_8(SourceR16^);
                    TargetRun8.G := Convert16_8(SourceG16^);
                    TargetRun8.B := Convert16_8(SourceB16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PByte(TargetRun8), TargetIncrement);
                end;
              end;
            end;
          16: // 161616 to 161616
            begin
              // no gamma correction for 16 bit samples yet
              if coNeedByteSwap in FSourceOptions then Convert16_16 := ComponentSwapConvert
                                                  else Convert16_16 := ComponentNoConvert16;

              if Length(Source) = 1 then
              begin
                SourceB16 := Source[0];
                SourceG16 := SourceB16; Inc(SourceG16);
                SourceR16 := SourceG16; Inc(SourceR16);
                SourceA16 := SourceR16; Inc(SourceA16);
              end
              else
              begin
                SourceB16 := Source[0];
                SourceG16 := Source[1];
                SourceR16 := Source[2];
                if coAlpha in FSourceOptions then SourceA16 := Source[3]
                                             else SourceA16 := nil;
              end;

              if CopyAlpha then
              begin
                TargetRunA16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA16.R := Convert16_16(SourceR16^);
                    TargetRunA16.G := Convert16_16(SourceG16^);
                    TargetRunA16.B := Convert16_16(SourceB16^);
                    TargetRunA16.A := Convert16_16(SourceA16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                    Inc(SourceA16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA16);
                end;
              end
              else
              begin
                TargetRun16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun16.R := Convert16_16(SourceR16^);
                    TargetRun16.G := Convert16_16(SourceG16^);
                    TargetRun16.B := Convert16_16(SourceB16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PWord(TargetRun16), TargetIncrement);
                end;
              end;
            end;
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.RowConvertPhotoYCC2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// converts from PhotoYCC format to BGR(A)

var
  Y, Cb, Cr: Integer;
  Yf, Cbf, Crf: Single;
  Y8Run, Cb8Run, Cr8Run: PByte;
  Y16Run, Cb16Run, Cr16Run: PWord;
  Target8: PByte;
  Target16: PWord;
  AlphaSkip: Integer;
  BitRun: Byte;
  Increment: Integer;

begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions); // 0 if no alpha must be skipped, otherwise 1

  case FSourceBPS of
    8:
      begin
        if Length(Source) = 1 then
        begin
          Y8Run := Source[0];
          Cb8Run := Y8Run; Inc(Cb8Run);
          Cr8Run := Cb8Run; Inc(Cr8Run);
          Increment := 3;
        end
        else
        begin
          Y8Run := Source[0];
          Cb8Run := Source[1];
          Cr8Run := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: // 888 to 888
            begin
              Target8 := Target;
                                             
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := Y8Run^;
                  Inc(Y8Run, Increment);
                  Cb := Cb8Run^;
                  Inc(Cb8Run, Increment);
                  Cr := Cr8Run^;
                  Inc(Cr8Run, Increment);

                  // blue
                  Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
                  Inc(Target8);
                  // green
                  Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
                  Inc(Target8);
                  // red
                  Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
                  Inc(Target8, 1 + AlphaSkip);
                end
                else Inc(Target8, 3 + AlphaSkip);

                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 888 to 161616
            begin
              Target16 := Target;

              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := Y8Run^;
                  Inc(Y8Run, Increment);
                  Cb := Cb8Run^;
                  Inc(Cb8Run, Increment);
                  Cr := Cr8Run^;
                  Inc(Cr8Run, Increment);

                  // blue
                  Target16^ := MulDiv16(ClampByte(Y + FCbToBlueTable[Cb]), 65535, 255);
                  Inc(Target16);
                  // green
                  Target16^ := MulDiv16(ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]), 65535, 255);
                  Inc(Target16);
                  // red
                  Target16^ := MulDiv16(ClampByte(Y + FCrToRedTable[Cr]), 65535, 255);
                  Inc(Target16, 1 + AlphaSkip);
                end
                else Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
    16:
      begin
        if Length(Source) = 1 then
        begin
          Y16Run := Source[0];
          Cb16Run := Y16Run; Inc(Cb16Run);
          Cr16Run := Cb16Run; Inc(Cr16Run);
          Increment := 3;
        end
        else
        begin
          Y16Run := Source[0];
          Cb16Run := Source[1];
          Cr16Run := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              Target8 := Target;

              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := MulDiv16(Y16Run^, 255, 65535);
                  Inc(Y16Run, Increment);
                  Cb := MulDiv16(Cb16Run^, 255, 65535);
                  Inc(Cb16Run, Increment);
                  Cr := MulDiv16(Cr16Run^, 255, 65535);
                  Inc(Cr16Run, Increment);

                  // blue
                  Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
                  Inc(Target8);
                  // green
                  Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
                  Inc(Target8);
                  // red
                  Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
                  Inc(Target8, 1 + AlphaSkip);
                end
                else Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 161616 to 161616
            begin
              Target16 := Target;

              // conversion from 16 to 16 is done with full precision, so there is no
              // loss of information, but the code is slower because the lookup tables
              // cannot be used
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Yf := 1.3584 * Y16Run^;
                  Inc(Y16Run, Increment);
                  Cbf := Cb16Run^ - 40092; // (156 * 65535) div 255
                  Inc(Cb16Run, Increment);
                  Crf := Cr16Run^ - 35209; // (137 * 65535) div 255
                  Inc(Cr16Run, Increment);

                  // blue
                  Target16^ := Round(Yf + 2.2179 * Cbf);
                  Inc(Target16);
                  // green
                  Target16^ := Round(Yf - 0.9271435 * Crf - 0.4302726 * Cbf);
                  Inc(Target16);
                  // red
                  Target16^ := Round(Yf + 1.8215 * Crf);
                  Inc(Target16, 1 + AlphaSkip);
                end
                else Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.RowConvertPhotoYCC2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// converts from PhotoYCC format to RGB(A)

var
  Y, Cb, Cr: Integer;
  Yf, Cbf, Crf: Single;
  Y8Run, Cb8Run, Cr8Run: PByte;
  Y16Run, Cb16Run, Cr16Run: PWord;
  Target8: PByte;
  Target16: PWord;
  AlphaSkip: Integer;
  BitRun: Byte;
  Increment: Integer;

begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions); // 0 if no alpha must be skipped, otherwise 1

  case FSourceBPS of
    8:
      begin
        if Length(Source) = 1 then
        begin
          Y8Run := Source[0];
          Cb8Run := Y8Run; Inc(Cb8Run);
          Cr8Run := Cb8Run; Inc(Cr8Run);
          Increment := 3;
        end
        else
        begin
          Y8Run := Source[0];
          Cb8Run := Source[1];
          Cr8Run := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: // 888 to 888
            begin
              Target8 := Target;
                                             
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := Y8Run^;
                  Inc(Y8Run, Increment);
                  Cb := Cb8Run^;
                  Inc(Cb8Run, Increment);
                  Cr := Cr8Run^;
                  Inc(Cr8Run, Increment);

                  // red
                  Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
                  Inc(Target8, 1 + AlphaSkip);
                  // green
                  Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
                  Inc(Target8);
                  // blue
                  Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
                  Inc(Target8);
                end
                else Inc(Target8, 3 + AlphaSkip);

                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 888 to 161616
            begin
              Target16 := Target;

              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := Y8Run^;
                  Inc(Y8Run, Increment);
                  Cb := Cb8Run^;
                  Inc(Cb8Run, Increment);
                  Cr := Cr8Run^;
                  Inc(Cr8Run, Increment);

                  // red
                  Target16^ := MulDiv16(ClampByte(Y + FCrToRedTable[Cr]), 65535, 255);
                  Inc(Target16, 1 + AlphaSkip);
                  // green
                  Target16^ := MulDiv16(ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]), 65535, 255);
                  Inc(Target16);
                  // blue
                  Target16^ := MulDiv16(ClampByte(Y + FCbToBlueTable[Cb]), 65535, 255);
                  Inc(Target16);
                end
                else Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
    16:
      begin
        if Length(Source) = 1 then
        begin
          Y16Run := Source[0];
          Cb16Run := Y16Run; Inc(Cb16Run);
          Cr16Run := Cb16Run; Inc(Cr16Run);
          Increment := 3;
        end
        else
        begin
          Y16Run := Source[0];
          Cb16Run := Source[1];
          Cr16Run := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              Target8 := Target;

              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := MulDiv16(Y16Run^, 255, 65535);
                  Inc(Y16Run, Increment);
                  Cb := MulDiv16(Cb16Run^, 255, 65535);
                  Inc(Cb16Run, Increment);
                  Cr := MulDiv16(Cr16Run^, 255, 65535);
                  Inc(Cr16Run, Increment);

                  // red
                  Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
                  Inc(Target8, 1 + AlphaSkip);
                  // green
                  Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
                  Inc(Target8);
                  // blue
                  Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
                  Inc(Target8);
                end
                else Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 161616 to 161616
            begin
              Target16 := Target;

              // conversion from 16 to 16 is done with full precision, so there is no
              // loss of information, but the code is slower because the lookup tables
              // cannot be used
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Yf := 1.3584 * Y16Run^;
                  Inc(Y16Run, Increment);
                  Cbf := Cb16Run^ - 40092; // (156 * 65535) div 255
                  Inc(Cb16Run, Increment);
                  Crf := Cr16Run^ - 35209; // (137 * 65535) div 255
                  Inc(Cr16Run, Increment);

                  // red
                  Target16^ := Round(Yf + 1.8215 * Crf);
                  Inc(Target16, 1 + AlphaSkip);
                  // green
                  Target16^ := Round(Yf - 0.9271435 * Crf - 0.4302726 * Cbf);
                  Inc(Target16);
                  // blue
                  Target16^ := Round(Yf + 2.2179 * Cbf);
                  Inc(Target16);
                end
                else Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.RowConvertYCbCr2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// converts from standard YCbCr to BGR(A)

var
  Y, Cb, Cr: Integer;
  Yf, Cbf, Crf: Single;
  Y8Run, Cb8Run, Cr8Run: PByte;
  Y16Run, Cb16Run, Cr16Run: PWord;
  Target8: PByte;
  Target16: PWord;
  AlphaSkip: Integer;
  BitRun: Byte;
  Increment: Integer;

begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions); // 0 if no alpha must be skipped, otherwise 1

  case FSourceBPS of
    8:
      begin
        if Length(Source) = 1 then
        begin
          Y8Run := Source[0];
          Cb8Run := Y8Run; Inc(Cb8Run);
          Cr8Run := Cb8Run; Inc(Cr8Run);
          Increment := 3;
        end
        else
        begin
          Y8Run := Source[0];
          Cb8Run := Source[1];
          Cr8Run := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: // 888 to 888
            begin
              Target8 := Target;
                                             
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := Y8Run^;
                  Inc(Y8Run, Increment);
                  Cb := Cb8Run^;
                  Inc(Cb8Run, Increment);
                  Cr := Cr8Run^;
                  Inc(Cr8Run, Increment);

                  // blue
                  Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
                  Inc(Target8);
                  // green
                  Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
                  Inc(Target8);
                  // red
                  Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
                  Inc(Target8, 1 + AlphaSkip);
                end
                else Inc(Target8, 3 + AlphaSkip);

                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 888 to 161616
            begin
              Target16 := Target;

              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := Y8Run^;
                  Inc(Y8Run, Increment);
                  Cb := Cb8Run^;
                  Inc(Cb8Run, Increment);
                  Cr := Cr8Run^;
                  Inc(Cr8Run, Increment);

                  // blue
                  Target16^ := MulDiv16(ClampByte(Y + FCbToBlueTable[Cb]), 65535, 255);
                  Inc(Target16);
                  // green
                  Target16^ := MulDiv16(ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]), 65535, 255);
                  Inc(Target16);
                  // red
                  Target16^ := MulDiv16(ClampByte(Y + FCrToRedTable[Cr]), 65535, 255);
                  Inc(Target16, 1 + AlphaSkip);
                end
                else Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
    16:
      begin
        if Length(Source) = 1 then
        begin
          Y16Run := Source[0];
          Cb16Run := Y16Run; Inc(Cb16Run);
          Cr16Run := Cb16Run; Inc(Cr16Run);
          Increment := 3;
        end
        else
        begin
          Y16Run := Source[0];
          Cb16Run := Source[1];
          Cr16Run := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              Target8 := Target;

              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := MulDiv16(Y16Run^, 255, 65535);
                  Inc(Y16Run, Increment);
                  Cb := MulDiv16(Cb16Run^, 255, 65535);
                  Inc(Cb16Run, Increment);
                  Cr := MulDiv16(Cr16Run^, 255, 65535);
                  Inc(Cr16Run, Increment);

                  // blue
                  Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
                  Inc(Target8);
                  // green
                  Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
                  Inc(Target8);
                  // red
                  Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
                  Inc(Target8, 1 + AlphaSkip);
                end
                else Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 161616 to 161616
            begin
              Target16 := Target;

              // conversion from 16 to 16 is done with full precision, so there is no
              // loss of information, but the code is slower because the lookup tables
              // cannot be used
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Yf := 1.3584 * Y16Run^;
                  Inc(Y16Run, Increment);
                  Cbf := Cb16Run^ - 40092; // (156 * 65535) div 255
                  Inc(Cb16Run, Increment);
                  Crf := Cr16Run^ - 35209; // (137 * 65535) div 255
                  Inc(Cr16Run, Increment);

                  // blue
                  Target16^ := Round(Yf + 2.2179 * Cbf);
                  Inc(Target16);
                  // green
                  Target16^ := Round(Yf - 0.9271435 * Crf - 0.4302726 * Cbf);
                  Inc(Target16);
                  // red
                  Target16^ := Round(Yf + 1.8215 * Crf);
                  Inc(Target16, 1 + AlphaSkip);
                end
                else Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.RowConvertYCbCr2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// converts from standard YCbCr to RGB(A)

var
  Y, Cb, Cr: Integer;
  Yf, Cbf, Crf: Single;
  Y8Run, Cb8Run, Cr8Run: PByte;
  Y16Run, Cb16Run, Cr16Run: PWord;
  Target8: PByte;
  Target16: PWord;
  AlphaSkip: Integer;
  BitRun: Byte;
  Increment: Integer;

begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions); // 0 if no alpha must be skipped, otherwise 1

  case FSourceBPS of
    8:
      begin
        if Length(Source) = 1 then
        begin
          Y8Run := Source[0];
          Cb8Run := Y8Run; Inc(Cb8Run);
          Cr8Run := Cb8Run; Inc(Cr8Run);
          Increment := 3;
        end
        else
        begin
          Y8Run := Source[0];
          Cb8Run := Source[1];
          Cr8Run := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: // 888 to 888
            begin
              Target8 := Target;
                                             
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := Y8Run^;
                  Inc(Y8Run, Increment);
                  Cb := Cb8Run^;
                  Inc(Cb8Run, Increment);
                  Cr := Cr8Run^;
                  Inc(Cr8Run, Increment);

                  // red
                  Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
                  Inc(Target8, 1 + AlphaSkip);
                  // green
                  Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
                  Inc(Target8);
                  // blue
                  Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
                  Inc(Target8);
                end
                else Inc(Target8, 3 + AlphaSkip);

                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 888 to 161616
            begin
              Target16 := Target;

              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := Y8Run^;
                  Inc(Y8Run, Increment);
                  Cb := Cb8Run^;
                  Inc(Cb8Run, Increment);
                  Cr := Cr8Run^;
                  Inc(Cr8Run, Increment);

                  // red
                  Target16^ := MulDiv16(ClampByte(Y + FCrToRedTable[Cr]), 65535, 255);
                  Inc(Target16, 1 + AlphaSkip);
                  // green
                  Target16^ := MulDiv16(ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]), 65535, 255);
                  Inc(Target16);
                  // blue
                  Target16^ := MulDiv16(ClampByte(Y + FCbToBlueTable[Cb]), 65535, 255);
                  Inc(Target16);
                end
                else Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
    16:
      begin
        if Length(Source) = 1 then
        begin
          Y16Run := Source[0];
          Cb16Run := Y16Run; Inc(Cb16Run);
          Cr16Run := Cb16Run; Inc(Cr16Run);
          Increment := 3;
        end
        else
        begin
          Y16Run := Source[0];
          Cb16Run := Source[1];
          Cr16Run := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              Target8 := Target;

              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := MulDiv16(Y16Run^, 255, 65535);
                  Inc(Y16Run, Increment);
                  Cb := MulDiv16(Cb16Run^, 255, 65535);
                  Inc(Cb16Run, Increment);
                  Cr := MulDiv16(Cr16Run^, 255, 65535);
                  Inc(Cr16Run, Increment);

                  // red
                  Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
                  Inc(Target8, 1 + AlphaSkip);
                  // green
                  Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
                  Inc(Target8);
                  // blue
                  Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
                  Inc(Target8);
                end
                else Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 161616 to 161616
            begin
              Target16 := Target;

              // conversion from 16 to 16 is done with full precision, so there is no
              // loss of information, but the code is slower because the lookup tables
              // cannot be used
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Yf := 1.3584 * Y16Run^;
                  Inc(Y16Run, Increment);
                  Cbf := Cb16Run^ - 40092; // (156 * 65535) div 255
                  Inc(Cb16Run, Increment);
                  Crf := Cr16Run^ - 35209; // (137 * 65535) div 255
                  Inc(Cr16Run, Increment);

                  // red
                  Target16^ := Round(Yf + 1.8215 * Crf);
                  Inc(Target16, 1 + AlphaSkip);
                  // green
                  Target16^ := Round(Yf - 0.9271435 * Crf - 0.4302726 * Cbf);
                  Inc(Target16);
                  // blue
                  Target16^ := Round(Yf + 2.2179 * Cbf);
                  Inc(Target16);
                end
                else Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.CreateYCbCrLookup;

// In order to speedup YCbCr conversion lookup tables are used, this methods creates them.
//    R := Y + Cr * (2 - 2 * LumaRed);
//    B := Y + Cb * (2 - 2 * LumaBlue);
//    G := Y - LumaBlue * Cb * (2 - 2 * LumaBlue) / LumaGreen
//           - LumaRed * Cr * (2 - 2 * LumaRed) / LumaGreen;
//
// To avoid floating point arithmetic the fractional constants that come out of the equations are represented
// as fixed point values in the range 0...2^16.  We also eliminate multiplications by // pre-calculating possible
// values indexed by Cb and Cr (this code assumes conversion is being done for 8-bit samples).
//
// Note: the color manager uses dynamic arrays so the memory used here is automatically freed.
//
// Needed settings:
// - YCbCr parameters must be set or default values are used

var
  F1, F2, F3, F4: Single;
  LumaRed,
  LumaGreen,
  LumaBlue: Single;
  I: Integer;
  Offset1, Offset2: Integer;

begin
  LumaRed := FYCbCrCoefficients[0];
  LumaGreen := FYCbCrCoefficients[1];
  LumaBlue := FYCbCrCoefficients[2];

  F1 := 2 - 2 * LumaRed;
  F2 := LumaRed * F1 / LumaGreen;
  F3 := 2 - 2 * LumaBlue;
  F4 := LumaBlue * F3 / LumaGreen;

  SetLength(FCrToRedTable, 256);
  SetLength(FCbToBlueTable, 256);
  SetLength(FCrToGreenTable, 256);
  SetLength(FCbToGreenTable, 256);

  if FSourceScheme = csYCbCr then              
  begin
    // I is the actual input pixel value in the range 0..255, Cb and Cr values are in the range -128..127.
    // (for TIFF files they are in a range defined by the ReferenceBlackWhite tag).
    Offset1 := -128;
    for I := 0 to 255 do
    begin
      FCrToRedTable[I] := Round(F1 * Offset1);
      FCbToBlueTable[I] := Round(F3 * Offset1);
      FCrToGreenTable[I] := -Round(F2 * Offset1);
      FCbToGreenTable[I] := -Round(F4 * Offset1);
      Inc(Offset1);
    end;
  end
  else
  begin
    // PhotoYCC
    // I is the actual input pixel value in the range 0..255, Cb values are in the range -156..99,
    // Cr values are in the range -137..118.
    // (for TIFF files they are in a range defined by the ReferenceBlackWhite tag).
    Offset1 := -156;
    Offset2 := -137;
    for I := 0 to 255 do
    begin
      FCrToRedTable[I] := Round(F1 * Offset2);
      FCbToBlueTable[I] := Round(F3 * Offset1);
      FCrToGreenTable[I] := -Round(F2 * Offset2);
      FCbToGreenTable[I] := -Round(F4 * Offset1);
      Inc(Offset1);
      Inc(Offset2);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorManager.GetPixelFormat(Index: Integer): TPixelFormat;

// determines the pixel format from the current sample and pixel sizes
// Note: setting pfCustom as pixel format will raise an exception so check the result from this method first
//       before you actually assign it to a bitmap.
//
// Needed settings:
// - source samples per pixel and bits per sample for Index = 0
// - target samples per pixel and bits per sample for Index = 1

var
  SamplesPerPixel,
  BitsPerSample: Byte;

begin
  case Index of
    0:
      begin
        SamplesPerPixel := FSourceSPP;
        BitsPerSample := FSourceBPS;
      end;
  else
    SamplesPerPixel := FTargetSPP;
    BitsPerSample := FTargetBPS;
  end;

  case SamplesPerPixel of
    1: // one sample per pixel, this is usually a palette format
      case BitsPerSample of
        1:
          Result := pf1Bit;
        2..4: // values < 4 should be upscaled
          Result := pf4bit;
        8..16:
          // values > 8 bits must be downscaled to 8 bits
          Result := pf8bit;
      else
        Result := pfCustom;
      end;
    3: // Typical case is RGB or CIE L*a*b* (565 and 555 16 bit color formats would also be possible, but aren't handled
       // by the manager).
      case BitsPerSample of
        1..5: // values < 5 should be upscaled
          Result := pf15Bit;
      else
        // values > 8 bits should be downscaled
        Result := pf24bit;
      end;
    4: // Typical cases: RGBA and CMYK (with 8 bps, other formats like PCX's
       // 4 planes with 1 bit must be handled elsewhere)
      if BitsPerSample >= 8 then Result := pf32Bit
                            else Result := pfCustom;
  else
    Result := pfCustom;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.PrepareConversion;

// depending on the source and target pixel and color formats a conversion function must be
// determined which actually carries out the conversion
//
// Needed settings:
// - source and target samples per pixel and bits per sample
// - source and target color scheme

begin
  FRowConversion := nil;

  // Conversion between indexed and non-indexed formats is not supported as well as
  // between source BPS < 8 and target BPS > 8.
  // csGA and csG (grayscale w and w/o alpha) are considered being indexed modes
  if (FSourceScheme in [csIndexed, csG, csGA]) xor
     (FTargetScheme  in [csIndexed, csG]) then Error(gesIndexedNotSupported);

  // set up special conversion options
  if FSourceScheme in [csGA, csRGBA, csBGRA] then Include(FSourceOptions, coAlpha)
                                             else Exclude(FSourceOptions, coAlpha);

  if FTargetScheme in [csGA, csRGBA, csBGRA] then Include(FTargetOptions, coAlpha)
                                             else Exclude(FTargetOptions, coAlpha);

  case FSourceScheme of
    csG:
      if (FSourceBPS = 16) or (FTargetBPS = 16) then
      begin
        if (FSourceBPS >= 8) and (FTargetBPS >= 8) then FRowConversion := RowConvertGray;
      end
      else
        FRowConversion := RowConvertIndexed8;
    csGA:
      if (FSourceBPS in [8, 16]) and (FTargetBPS in [8, 16]) then FRowConversion := RowConvertGray;
    csIndexed:
      begin
        // Grayscale is handled like indexed mode.
        // Generally use indexed conversions (with various possible bit operations),
        // assign special methods for source only, target only or source and target being 16 bits per sample
        if (FSourceBPS = 16) and
           (FTargetBPS = 16) then FRowConversion := RowConvertIndexedBoth16
                             else
          if FSourceBPS = 16 then FRowConversion := RowConvertIndexedSource16
                             else
            if FTargetBPS = 16 then FRowConversion := RowConvertIndexedTarget16
                               else FRowConversion := RowConvertIndexed8;
      end;
    csRGB,
    csRGBA:
      case FTargetScheme of
        csRGB: FRowConversion := RowConvertRGB2RGB;
        csRGBA: FRowConversion := RowConvertRGB2RGB;
        csBGR: FRowConversion := RowConvertRGB2BGR;
        csBGRA: FRowConversion := RowConvertRGB2BGR;
        csCMY: ;
        csCMYK: ;
        csCIELab: ;
        csYCbCr: ;
      end;
    csBGRA,
    csBGR:
      case FTargetScheme of
        csRGB,
        csRGBA: FRowConversion := RowConvertBGR2RGB;
        csBGR,
        csBGRA: FRowConversion := RowConvertBGR2BGR;
        csCMY: ;
        csCMYK: ;
        csCIELab: ;
        csYCbCr: ;
      end;
    csCMY:
      case FTargetScheme of
        csRGB: ;
        csRGBA: ;
        csBGR: ;
        csBGRA: ;
        csCMY: ;
        csCMYK: ;
        csCIELab: ;
        csYCbCr: ;
      end;
    csCMYK:
      case FTargetScheme of
        csRGB,
        csRGBA: FRowConversion := RowConvertCMYK2RGB;
        csBGR,
        csBGRA: FRowConversion := RowConvertCMYK2BGR;
        csCMY: ;
        csCMYK: ;
        csCIELab: ;
        csYCbCr: ;
      end;
    csCIELab:
      case FTargetScheme of
        csRGB,
        csRGBA: FRowConversion := RowConvertCIELab2RGB;
        csBGR,
        csBGRA: FRowConversion := RowConvertCIELab2BGR;
        csCMY: ;
        csCMYK: ;
        csCIELab: ;
        csYCbCr: ;
      end;
    csYCbCr:
      begin
        // create lookup tables to speed up conversion
        CreateYCbCrLookup;
        case FTargetScheme of
          csRGB,
          csRGBA: FRowConversion := RowConvertYCbCr2RGB;
          csBGR,
          csBGRA: FRowConversion := RowConvertYCbCr2BGR;
          csCMY: ;
          csCMYK: ;
          csCIELab: ;
          csYCbCr: ;
        end;
      end;
    csPhotoYCC:
      begin
        // create lookup tables to speed up conversion
        CreateYCbCrLookup;
        case FTargetScheme of
          csRGB,
          csRGBA: FRowConversion := RowConvertPhotoYCC2RGB;
          csBGR,
          csBGRA: FRowConversion := RowConvertPhotoYCC2BGR;
          csCMY: ;
          csCMYK: ;
          csCIELab: ;
          csYCbCr: ;
        end;
      end;
  end;
  FChanged := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.SetSourceBitsPerSample(const Value: Byte);

begin
  if not (Value in [1..16]) then Error(gesInvalidSampleDepth);
  if FSourceBPS <> Value then
  begin
    FSourceBPS := Value;
    FChanged := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.SetSourceColorScheme(const Value: TColorScheme);

begin
  if FSourceScheme <> Value then
  begin
    FSourceScheme := Value;
    FChanged := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.SetSourceSamplesPerPixel(const Value: Byte);

begin
  if not (Value in [1..4]) then Error(gesInvalidPixelDepth);
  if FSourceSPP <> Value then
  begin
    FSourceSPP := Value;
    FChanged := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.SetTargetBitsPerSample(const Value: Byte);

begin
  if not (Value in [1..16]) then Error(gesInvalidSampleDepth);
  if FTargetBPS <> Value then
  begin
    FTargetBPS := Value;
    FChanged := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.SetTargetColorScheme(const Value: TColorScheme);

begin
  if FTargetScheme <> Value then
  begin
    FTargetScheme := Value;
    FChanged := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.SetTargetSamplesPerPixel(const Value: Byte);

begin
  if not (Value in [1..4]) then Error(gesInvalidPixelDepth);
  if FTargetSPP <> Value then
  begin
    FTargetSPP := Value;
    FChanged := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.ConvertRow(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// initializes the color conversion method if necessary and calls it to do the actual conversion
//
// Needed settings:
// - source and target BPS and SPP
// - main and display gamma, if gamma correction is wanted
// - conversion options
// - YCbCr parameters if any of the color schemes is csYCbCr

begin
  // if there are pending changes then apply them 
  if FChanged then PrepareConversion;
  // check if there's now a conversion method
  if @FRowConversion = nil then Error(gesConversionUnsupported)
                           else FRowConversion(Source, Target, Count, Mask);
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorManager.CreateColorPalette(Data: array of Pointer; DataFormat: TRawPaletteFormat;
  ColorCount: Cardinal; RGB: Boolean): HPALETTE;

// Creates a color palette from the provided data which can be in various raw formats:
// - either interlaced or plane
// - 8 bits or 16 bits per component
// - in RGB or BGR order
// - with 3 or 4 components per entry (fourth is ignored)
// ColorCount determines the number of color entries to create. If this number does not equal the
// number of palette entries which would result from the given target bits per sample resolution
// then the palette is adjusted accordingly to allow conversion between resolutions.
//
// Notes: For interlaced formats only one pointer needs to be passed in Data (only the first one is used)
//        while for plane data 3 pointers are necessary (for each plane one pointer).
//        The data order is assumed rgb or bgr in interlaced order (depending on RGB). In plane mode the three needed
//        pointers must also be given such that the pointer to red components is in Data[0], the green pointer in
//        Data[1] and the blue one in Data[2]. In this case BGR is not needed.
//
// Needed settings:
// - main and display gamma, if gamma correction is wanted
// - Options set as needed (gamma, byte swap)
// - source and target bits per sample resolution

var
  I,
  MaxIn, MaxOut: Integer;
  LogPalette: TMaxLogPalette;
  RunR8,
  RunG8,
  RunB8: PByte;
  RunR16,
  RunG16,
  RunB16: PWord;
  Convert8: function(Value: Byte): Byte of object;
  Convert16: function(Value: Word): Byte of object;
  
begin
  FillChar(LogPalette, SizeOf(LogPalette), 0);
  LogPalette.palVersion := $300;
  if ColorCount > 256 then LogPalette.palNumEntries := 256
                      else LogPalette.palNumEntries := ColorCount;

  case DataFormat of
    pfInterlaced8Triple,
    pfInterlaced8Quad:
      begin
        RunR8 := Data[0];
        if coApplyGamma in FTargetOptions then Convert8 := ComponentGammaConvert
                                          else Convert8 := ComponentNoConvert8;

        if RGB then
        begin
          for I := 0 to LogPalette.palNumEntries - 1 do
          begin
            LogPalette.palPalEntry[I].peBlue := Convert8(RunR8^); Inc(RunR8);
            LogPalette.palPalEntry[I].peGreen := Convert8(RunR8^); Inc(RunR8);
            LogPalette.palPalEntry[I].peRed := Convert8(RunR8^); Inc(RunR8);
            if DataFormat = pfInterlaced8Quad then Inc(RunR8);
          end;
        end
        else
        begin
          for I := 0 to LogPalette.palNumEntries - 1 do
          begin
            LogPalette.palPalEntry[I].peRed := Convert8(RunR8^); Inc(RunR8);
            LogPalette.palPalEntry[I].peGreen := Convert8(RunR8^); Inc(RunR8);
            LogPalette.palPalEntry[I].peBlue := Convert8(RunR8^); Inc(RunR8);
            if DataFormat = pfInterlaced8Quad then Inc(RunR8);
          end;
        end;
      end;
    pfPlane8Triple,
    pfPlane8Quad:
      begin
        RunR8 := Data[0];
        RunG8 := Data[1];
        RunB8 := Data[2];
        if coApplyGamma in FTargetOptions then Convert8 := ComponentGammaConvert
                                          else Convert8 := ComponentNoConvert8;
        for I := 0 to LogPalette.palNumEntries - 1 do
        begin
          LogPalette.palPalEntry[I].peRed := Convert8(RunR8^); Inc(RunR8);
          LogPalette.palPalEntry[I].peGreen := Convert8(RunG8^); Inc(RunG8);
          LogPalette.palPalEntry[I].peBlue := Convert8(RunB8^); Inc(RunB8);
        end;
      end;
    pfInterlaced16Triple,
    pfInterlaced16Quad:
      begin
        RunR16 := Data[0];
        if coApplyGamma in FTargetOptions then
        begin
          if coNeedByteSwap in FSourceOptions then Convert16 := ComponentSwapScaleGammaConvert
                                              else Convert16 := ComponentScaleGammaConvert;
        end
        else
        begin
          if coNeedByteSwap in FSourceOptions then Convert16 := ComponentSwapScaleConvert
                                              else Convert16 := ComponentScaleConvert;
        end;
        
        if RGB then
        begin
          for I := 0 to LogPalette.palNumEntries - 1 do
          begin
            LogPalette.palPalEntry[I].peRed := Convert16(RunR16^); Inc(RunR16);
            LogPalette.palPalEntry[I].peGreen := Convert16(RunR16^); Inc(RunR16);
            LogPalette.palPalEntry[I].peBlue := Convert16(RunR16^); Inc(RunR16);
            if DataFormat = pfInterlaced16Quad then Inc(RunR16);
          end;
        end
        else
        begin
          for I := 0 to LogPalette.palNumEntries - 1 do
          begin
            LogPalette.palPalEntry[I].peBlue := Convert16(RunR16^); Inc(RunR16);
            LogPalette.palPalEntry[I].peGreen := Convert16(RunR16^); Inc(RunR16);
            LogPalette.palPalEntry[I].peRed := Convert16(RunR16^); Inc(RunR16);
            if DataFormat = pfInterlaced16Quad then Inc(RunR16);
          end;
        end;
      end;
    pfPlane16Triple,
    pfPlane16Quad:
      begin
        RunR16 := Data[0];
        RunG16 := Data[1];
        RunB16 := Data[2];
        if coApplyGamma in FTargetOptions then
        begin
          if coNeedByteSwap in FSourceOptions then Convert16 := ComponentSwapScaleGammaConvert
                                              else Convert16 := ComponentScaleGammaConvert;
        end
        else
        begin
          if coNeedByteSwap in FSourceOptions then Convert16 := ComponentSwapScaleConvert
                                              else Convert16 := ComponentScaleConvert;
        end;

        for I := 0 to LogPalette.palNumEntries - 1 do
        begin
          LogPalette.palPalEntry[I].peRed := Convert16(RunR16^); Inc(RunR16);
          LogPalette.palPalEntry[I].peGreen := Convert16(RunG16^); Inc(RunG16);
          LogPalette.palPalEntry[I].peBlue := Convert16(RunB16^); Inc(RunB16);
        end;
      end;
  end;

  MaxIn := (1 shl FSourceBPS) - 1;
  MaxOut := (1 shl FTargetBPS) - 1;
  if (FTargetBPS <= 8) and (MaxIn <> MaxOut) then
  begin
    // If target resolution and given color depth differ then the palette needs to be adjusted.
    // Consider the case for 2 bit to 4 bit conversion. Only 4 colors will be given to create
    // the palette but after scaling all values will be up to 15 for which no color is in the palette.
    // This and the reverse case need to be accounted for.
    MaxIn := (1 shl FSourceBPS) - 1;
    MaxOut := (1 shl FTargetBPS) - 1;
    if MaxIn < MaxOut then
    begin
      // palette is too small, enhance it
      for I := MaxOut downto 0 do
      begin
        LogPalette.palPalEntry[I].peRed := LogPalette.palPalEntry[MulDiv16(I, MaxIn, MaxOut)].peRed;
        LogPalette.palPalEntry[I].peGreen := LogPalette.palPalEntry[MulDiv16(I, MaxIn, MaxOut)].peGreen;
        LogPalette.palPalEntry[I].peBlue := LogPalette.palPalEntry[MulDiv16(I, MaxIn, MaxOut)].peBlue;
      end;
    end
    else
    begin
      // palette contains too many entries, shorten it
      for I := 0 to MaxOut do
      begin
        LogPalette.palPalEntry[I].peRed := LogPalette.palPalEntry[MulDiv16(I, MaxIn, MaxOut)].peRed;
        LogPalette.palPalEntry[I].peGreen := LogPalette.palPalEntry[MulDiv16(I, MaxIn, MaxOut)].peGreen;
        LogPalette.palPalEntry[I].peBlue := LogPalette.palPalEntry[MulDiv16(I, MaxIn, MaxOut)].peBlue;
      end;
    end;
    LogPalette.palNumEntries := MaxOut + 1;
  end;
                      
  // finally create palette
  Result := CreatePalette(PLogPalette(@LogPalette)^);
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorManager.CreateGrayscalePalette(MinimumIsWhite: Boolean): HPALETTE;

// Creates a grayscale palette depending on the target bit depth and returns the handle of it,
// optionally applies gamma to each entry. MinimumIsWhite is True if the palette starts
// with white in index 0 decreasing to black while the index grows otherwise (and this is the usual case)
// a lower palette index has a lower brightness.
//
// Needed settings:
// - target BPS, 16 bits are handled as 8 bits
// - target SPP should be set to 1 (indicating an indexed image), but is not evaluated here
// - main and display gamma, if gamma correction is wanted
// Note: Target bps must not be changed between setting gamma and creating the palette.

var
  LogPalette: TMaxLogPalette;
  I: Integer;
  BPS,
  Upper,
  Factor: Byte;

begin
  FillChar(LogPalette, SizeOf(LogPalette), 0);
  LogPalette.palVersion := $300;
  // the product of BPS and SPP considers planar organizatons correctly
  // (e.g. PCX has a format 4 planes with 1 bit resulting to 16 color image)
  BPS := FTargetBPS * FTargetSPP;
  if BPS > 8 then BPS := 8;
  LogPalette.palNumEntries := 1 shl BPS;
  Upper := LogPalette.palNumEntries - 1;
  Factor := 255 div Upper;
  if MinimumIsWhite then
  begin
    if not (coApplyGamma in FTargetOptions) then
    begin
      for I := 0 to Upper do
      begin
        LogPalette.palPalEntry[Upper - I].peBlue := I * Factor;
        LogPalette.palPalEntry[Upper - I].peGreen := I * Factor;
        LogPalette.palPalEntry[Upper - I].peRed := I * Factor;
      end;
    end
    else
    begin
      for I := 0 to Upper do
      begin
        LogPalette.palPalEntry[Upper - I].peBlue := FGammaTable[I * Factor];
        LogPalette.palPalEntry[Upper - I].peGreen := FGammaTable[I * Factor];
        LogPalette.palPalEntry[Upper - I].peRed := FGammaTable[I * Factor];
      end;
    end;
  end
  else
  begin
    if not (coApplyGamma in FTargetOptions) then
    begin
      for I := 0 to Upper do
      begin
        LogPalette.palPalEntry[I].peBlue := I * Factor;
        LogPalette.palPalEntry[I].peGreen := I * Factor;
        LogPalette.palPalEntry[I].peRed := I * Factor;
      end;
    end
    else
    begin
      for I := 0 to Upper do
      begin
        LogPalette.palPalEntry[I].peBlue := FGammaTable[I * Factor];
        LogPalette.palPalEntry[I].peGreen := FGammaTable[I * Factor];
        LogPalette.palPalEntry[I].peRed := FGammaTable[I * Factor];
      end;
    end;
  end;
  // finally create palette
  Result := CreatePalette(PLogPalette(@LogPalette)^);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.Error(const Msg: String);

begin
  raise EColorConversionError.Create(Msg);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.SetGamma(MainGamma, DisplayGamma: Single);

// sets the current gamma values and creates the gamma lookup table
//
// Needed settings:
// - source bits per samples must be set
// - target bits per samples must be set

var
  I,
  SourceHighBound,
  TargetHighBound: Integer;
  Gamma: Single;

begin
  if MainGamma <= 0 then FMainGamma := 1
                    else FMainGamma := MainGamma;
  if DisplayGamma <= 0 then FDisplayGamma := 2.2 // default value for a usual CRT
                       else FDisplayGamma := DisplayGamma;

  Gamma := 1 / (FMainGamma * FDisplayGamma);

  // source high bound is the maximum possible source value which can appear (0..255)
  if FSourceBPS >= 8 then SourceHighBound := 255
                     else SourceHighBound := (1 shl FTargetBPS) - 1;
  // target high bound is the target value which corresponds to a target sample value of 1 (0..255)
  if FTargetBPS >= 8 then TargetHighBound := 255
                     else TargetHighBound := (1 shl FTargetBPS) - 1;
  for I := 0 to SourceHighBound  do
    FGammaTable[I] := Round(Power((I / SourceHighBound), Gamma) * TargetHighBound);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.SetYCbCrParameters(Values: array of Single; HSubSampling, VSubSampling: Byte);

// sets coefficients needed to convert from YCbCr color scheme

begin
  // there must always be at least one value in an open array
  FYCbCrCoefficients[0] := Values[0];
  if High(Values) > 0 then
  begin
    FYCbCrCoefficients[1] := Values[1];
    if High(Values) > 1 then FYCbCrCoefficients[2] := Values[2];
  end;

  // subsampling can be 1, 2 or 4 and vertical subsampling must always be <= horizontal subsampling
  if not (HSubSampling in [1, 2, 4]) then Error(gesInvalidSubSampling);
  if not (VSubSampling in [1, 2, 4]) then Error(gesInvalidSubSampling);
  if VSubSampling > HSubSampling then Error(gesVerticalSubSamplingError);
  FHSubSampling := HSubSampling;
  FVSubSampling := VSubSampling;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
