{==============================================================================
  Content:  TheTextDrawer, a helper class for drawing of
            fixed-pitched font characters
 ==============================================================================
  The contents of this file are subject to the Mozilla Public License Ver. 1.0
  (the "License"); you may not use this file except in compliance with the
  License. You may obtain a copy of the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.
 ==============================================================================
  The Original Code is HANAI Tohru's private delphi library.
 ==============================================================================
  The Initial Developer of the Original Code is HANAI Tohru (Japan)
  Portions created by HANAI Tohru are Copyright (C) 1999.
  All Rights Reserved.
 ==============================================================================
  Contributor(s):   HANAI Tohru
  Unicode translation by Maël Hörz.
 ==============================================================================
  History:  01/19/1999  HANAI Tohru
                        Initial Version
            02/13/1999  HANAI Tohru
                        Changed default intercharacter spacing
            09/09/1999  HANAI Tohru
                        Redesigned all. Simplified interfaces.
                        When drawing text now it uses TextOut + SetTextCharacter-
                        Extra insted ExtTextOut since ExtTextOut has a little
                        heavy behavior.
            09/10/1999  HANAI Tohru
                        Added code to call ExtTextOut because there is a problem
                        when TextOut called with italicized raster type font.
                        After this changing, ExtTextOut is called without the
                        last parameter `lpDx' and be with SetTextCharacterExtra.
                        This pair performs faster than with `lpDx'.
            09/14/1999  HANAI Tohru
                        Changed code for saving/restoring DC
            09/15/1999  HANAI Tohru
                        Added X/Y parameters to ExtTextOut.
            09/16/1999  HANAI Tohru
                        Redesigned for multi-bytes character drawing.
            09/19/1999  HANAI Tohru
                        Since TheTextDrawer grew fat it was split into three
                        classes - TheFontStock, TheTextDrawer and TheTextDrawerEx.
                        Currently it should avoid TheTextDrawer because it is
                        slower than TheTextDrawer.
            09/25/1999  HANAI Tohru
                        Added internally definition of LeadBytes for Delphi 2
            10/01/1999  HANAI Tohru
                        To save font resources, now all fonts data are shared
                        among all of TheFontStock instances. With this changing,
                        there added a new class `TheFontsInfoManager' to manage
                        those shared data.
            10/09/1999  HANAI Tohru
                        Added BaseStyle property to TheFontFont class.
 ==============================================================================}

// $Id: SynTextDrawer.pas,v 1.6.2.17 2008/09/17 13:59:12 maelh Exp $

// SynEdit note: The name had to be changed to get SynEdit to install 
//   together with mwEdit into the same Delphi installation

unit SynTextDrawer;

{$I SynEdit.inc}

interface

uses
  SynUnicode,
  SysUtils,
  Classes,
  Windows,
  Graphics,
  Math;

const
  FontStyleCount = Ord(High(TFontStyle)) +1;
  FontStyleCombineCount = (1 shl FontStyleCount);
  
type
  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array[0..MaxInt div SizeOf(Integer) - 1] of Integer;

  TheStockFontPatterns = 0..FontStyleCombineCount -1;

  PheFontData = ^TheFontData;
  TheFontData = record
    Style: TFontStyles;
    Handle: HFont;
    CharAdv: Integer;
    CharHeight: Integer;
  end;

  PheFontsData = ^TheFontsData;
  TheFontsData = array[TheStockFontPatterns] of TheFontData;

  PheSharedFontsInfo = ^TheSharedFontsInfo;
  TheSharedFontsInfo = record
    // reference counters
    RefCount: Integer;
    LockCount: Integer;
    // font information
    BaseFont: TFont;
    BaseLF: TLogFont;
    IsTrueType: Boolean;
    FontsData: TheFontsData;
  end;

  { TheStockFontManager }

  TheFontsInfoManager = class
  private
    FFontsInfo: TList;
    function FindFontsInfo(const LF: TLogFont): PheSharedFontsInfo;
    function CreateFontsInfo(ABaseFont: TFont;
      const LF: TLogFont): PheSharedFontsInfo;
    procedure DestroyFontHandles(pFontsInfo: PheSharedFontsInfo);
    procedure RetrieveLogFontForComparison(ABaseFont: TFont; var LF: TLogFont);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LockFontsInfo(pFontsInfo: PheSharedFontsInfo);
    procedure UnLockFontsInfo(pFontsInfo: PheSharedFontsInfo);
    function GetFontsInfo(ABaseFont: TFont): PheSharedFontsInfo;
    procedure ReleaseFontsInfo(pFontsInfo: PheSharedFontsInfo);
  end;

  { TheFontStock }

  TTextOutOptions = set of (tooOpaque, tooClipped);

  TheExtTextOutProc = procedure (X, Y: Integer; fuOptions: TTextOutOptions;
    const ARect: TRect; const Text: UnicodeString; Length: Integer) of object;

  EheFontStockException = class(Exception);

  TheFontStock = class
  private
    // private DC
    FDC: HDC;
    FDCRefCount: Integer;

    // Shared fonts
    FpInfo: PheSharedFontsInfo;
    FUsingFontHandles: Boolean;

    // Current font
    FCrntFont: HFONT;
    FCrntStyle: TFontStyles;
    FpCrntFontData: PheFontData;
    // local font info
    FBaseLF: TLogFont;
    function GetBaseFont: TFont;
    function GetIsTrueType: Boolean;
  protected
    function InternalGetDC: HDC; virtual;
    procedure InternalReleaseDC(Value: HDC); virtual;
    function InternalCreateFont(Style: TFontStyles): HFONT; virtual;
    function CalcFontAdvance(DC: HDC; pCharHeight: PInteger): Integer; virtual;
    function GetCharAdvance: Integer; virtual;
    function GetCharHeight: Integer; virtual;
    function GetFontData(idx: Integer): PheFontData; virtual;
    procedure UseFontHandles;
    procedure ReleaseFontsInfo;
    procedure SetBaseFont(Value: TFont); virtual;
    procedure SetStyle(Value: TFontStyles); virtual;
    property FontData[idx: Integer]: PheFontData read GetFontData;
    property FontsInfo: PheSharedFontsInfo read FpInfo;
  public
    constructor Create(InitialFont: TFont); virtual;
    destructor Destroy; override;
    procedure ReleaseFontHandles; virtual;
    property BaseFont: TFont read GetBaseFont;
    property Style: TFontStyles read FCrntStyle write SetStyle;
    property FontHandle: HFONT read FCrntFont;
    property CharAdvance: Integer read GetCharAdvance;
    property CharHeight: Integer read GetCharHeight;
    property IsTrueType: Boolean read GetIsTrueType;
  end;

  { TheTextDrawer }
  EheTextDrawerException = class(Exception);

  TheTextDrawer = class(TObject)
  private
    FDC: HDC;
    FSaveDC: Integer;

    // Font information
    FFontStock: TheFontStock;
    FStockBitmap: TBitmap;
    FCalcExtentBaseStyle: TFontStyles;
    FBaseCharWidth: Integer;
    FBaseCharHeight: Integer;

    // current font and properties
    FCrntFont: HFONT;
    FETODist: PIntegerArray;

    // current font attributes
    FColor: TColor;
    FBkColor: TColor;
    FCharExtra: Integer;

    // Begin/EndDrawing calling count
    FDrawingCount: Integer;
  protected
    procedure ReleaseETODist; virtual;
    procedure AfterStyleSet; virtual;
    procedure DoSetCharExtra(Value: Integer); virtual;
    property StockDC: HDC read FDC;
    property DrawingCount: Integer read FDrawingCount;
    property FontStock: TheFontStock read FFontStock;
    property BaseCharWidth: Integer read FBaseCharWidth;
    property BaseCharHeight: Integer read FBaseCharHeight;
  public
    constructor Create(CalcExtentBaseStyle: TFontStyles; BaseFont: TFont); virtual;
    destructor Destroy; override;
    function GetCharWidth: Integer; virtual;
    function GetCharHeight: Integer; virtual;
    procedure BeginDrawing(DC: HDC); virtual;
    procedure EndDrawing; virtual;
    procedure TextOut(X, Y: Integer; Text: PWideChar; Length: Integer); virtual;
    procedure ExtTextOut(X, Y: Integer; Options: TTextOutOptions; ARect: TRect;
      Text: PWideChar; Length: Integer); virtual;
    function TextExtent(const Text: UnicodeString): TSize; overload;
    function TextExtent(Text: PWideChar; Count: Integer): TSize; overload;
    function TextWidth(const Text: UnicodeString): Integer; overload;
    function TextWidth(Text: PWideChar; Count: Integer): Integer; overload;
    procedure SetBaseFont(Value: TFont); virtual;
    procedure SetBaseStyle(const Value: TFontStyles); virtual;
    procedure SetStyle(Value: TFontStyles); virtual;
    procedure SetForeColor(Value: TColor); virtual;
    procedure SetBackColor(Value: TColor); virtual;
    procedure SetCharExtra(Value: Integer); virtual;
    procedure ReleaseTemporaryResources; virtual;
    property CharWidth: Integer read GetCharWidth;
    property CharHeight: Integer read GetCharHeight;
    property BaseFont: TFont write SetBaseFont;
    property BaseStyle: TFontStyles write SetBaseStyle;
    property ForeColor: TColor write SetForeColor;
    property BackColor: TColor write SetBackColor;
    property Style: TFontStyles write SetStyle;
    property CharExtra: Integer read FCharExtra write SetCharExtra;
  end;

function GetFontsInfoManager: TheFontsInfoManager;

function UniversalExtTextOut(DC: HDC; X, Y: Integer; Options: TTextOutOptions;
  Rect: TRect; Str: PWideChar; Count: Integer; ETODist: PIntegerArray): Boolean;

implementation

{$IFDEF SYN_UNISCRIBE}
uses
  SynUsp10;
{$ENDIF}

var
  gFontsInfoManager: TheFontsInfoManager;

{ utility routines }

function GetFontsInfoManager: TheFontsInfoManager;
begin
  if not Assigned(gFontsInfoManager) then
    gFontsInfoManager := TheFontsInfoManager.Create;
  Result := gFontsInfoManager;
end;

function Min(x, y: integer): integer;
begin
  if x < y then Result := x else Result := y;
end;

// UniversalExtTextOut uses UniScribe where available for the best possible
// output quality. This also avoids a bug in (Ext)TextOut that surfaces when
// displaying a combination of Chinese and Korean text.
//
// See here for details: http://groups.google.com/group/microsoft.public.win32.programmer.international/browse_thread/thread/77cd596f2b96dc76/146300208098285c?lnk=st&q=font+substitution+problem#146300208098285c
function UniversalExtTextOut(DC: HDC; X, Y: Integer; Options: TTextOutOptions;
  Rect: TRect; Str: PWideChar; Count: Integer; ETODist: PIntegerArray): Boolean;
{$IFDEF SYN_UNISCRIBE}
const
  SSAnalyseFlags = SSA_GLYPHS or SSA_FALLBACK or SSA_LINK;
  SpaceString: UnicodeString = ' ';
{$ENDIF}
var
  TextOutFlags: DWORD;
{$IFDEF SYN_UNISCRIBE}
  GlyphBufferSize: Integer;
  saa: TScriptStringAnalysis;
{$ENDIF}
begin
  TextOutFlags := 0;
  if tooOpaque in Options then
    TextOutFlags := TextOutFlags or ETO_OPAQUE;
  if tooClipped in Options then
    TextOutFlags := TextOutFlags or ETO_CLIPPED;

{$IFDEF SYN_UNISCRIBE}
  if Usp10IsInstalled then
  begin
    // UniScribe requires that the string contains at least one character.
    // If UniversalExtTextOut should be used to fill the background we can just
    // pass a string made of a space.
    if Count <= 0 then
      if tooOpaque in Options then
      begin
        // Clipping is necessary, since depending on X, Y the space will be
        // printed outside Rect and potentially fill more than we want.
        TextOutFlags := TextOutFlags or ETO_CLIPPED;
        Str := PWideChar(SpaceString);
        Count := 1;
      end
      else
      begin
        Result := False;
        Exit;
      end;

    // According to the MS Windows SDK (1.5 * Count + 16) is the recommended
    // value for GlyphBufferSize (see documentation of cGlyphs parameter of
    // ScriptStringAnalyse function)
    GlyphBufferSize := (3 * Count) div 2 + 16;
    
    Result := Succeeded(ScriptStringAnalyse(DC, Str, Count, GlyphBufferSize, -1,
      SSAnalyseFlags, 0, nil, nil, Pointer(ETODist), nil, nil, @saa));
    Result := Result and Succeeded(ScriptStringOut(saa, X, Y, TextOutFlags,
      @Rect, 0, 0, False));
    Result := Result and Succeeded(ScriptStringFree(@saa));
  end
  else
{$ENDIF}
  begin
    Result := ExtTextOutW(DC, X, Y, TextOutFlags, @Rect, Str, Count,
      Pointer(ETODist));
  end;
end;

{ TheFontsInfoManager }

procedure TheFontsInfoManager.LockFontsInfo(
  pFontsInfo: PheSharedFontsInfo);
begin
  Inc(pFontsInfo^.LockCount);
end;

constructor TheFontsInfoManager.Create;
begin
  inherited;

  FFontsInfo := TList.Create;
end;

function TheFontsInfoManager.CreateFontsInfo(ABaseFont: TFont;
  const LF: TLogFont): PheSharedFontsInfo;
begin
  New(Result);
  FillChar(Result^, SizeOf(TheSharedFontsInfo), 0);
  with Result^ do
    try
      BaseFont := TFont.Create;
      BaseFont.Assign(ABaseFont);
      BaseLF := LF;
      IsTrueType := (0 <> (TRUETYPE_FONTTYPE and LF.lfPitchAndFamily));
  except
    Result^.BaseFont.Free;
    Dispose(Result);
    raise;
  end;
end;

procedure TheFontsInfoManager.UnlockFontsInfo(
  pFontsInfo: PheSharedFontsInfo);
begin
  with pFontsInfo^ do
  begin
    Dec(LockCount);
    if 0 = LockCount then
      DestroyFontHandles(pFontsInfo);
  end;
end;

destructor TheFontsInfoManager.Destroy;
begin
  gFontsInfoManager := nil;
  
  if Assigned(FFontsInfo) then
  begin
    while FFontsInfo.Count > 0 do
    begin
      ASSERT(1 = PheSharedFontsInfo(FFontsInfo[FFontsInfo.Count - 1])^.RefCount);
      ReleaseFontsInfo(PheSharedFontsInfo(FFontsInfo[FFontsInfo.Count - 1]));
    end;
    FFontsInfo.Free;
  end;

  inherited;
end;

procedure TheFontsInfoManager.DestroyFontHandles(
  pFontsInfo: PheSharedFontsInfo);
var
  i: Integer;
begin
  with pFontsInfo^ do
    for i := Low(TheStockFontPatterns) to High(TheStockFontPatterns) do
      with FontsData[i] do
        if Handle <> 0 then
        begin
          DeleteObject(Handle);
          Handle := 0;
        end;
end;

function TheFontsInfoManager.FindFontsInfo(
  const LF: TLogFont): PheSharedFontsInfo;
var
  i: Integer;
begin
  for i := 0 to FFontsInfo.Count - 1 do
  begin
    Result := PheSharedFontsInfo(FFontsInfo[i]);
    if CompareMem(@(Result^.BaseLF), @LF, SizeOf(TLogFont)) then
      Exit;
  end;
  Result := nil;
end;

function TheFontsInfoManager.GetFontsInfo(ABaseFont: TFont): PheSharedFontsInfo;
var
  LF: TLogFont;
begin
  ASSERT(Assigned(ABaseFont));

  RetrieveLogFontForComparison(ABaseFont, LF);
  Result := FindFontsInfo(LF);
  if not Assigned(Result) then
  begin
    Result := CreateFontsInfo(ABaseFont, LF);
    FFontsInfo.Add(Result);
  end;

  if Assigned(Result) then
    Inc(Result^.RefCount);
end;

procedure TheFontsInfoManager.ReleaseFontsInfo(pFontsInfo: PheSharedFontsInfo);
begin
  ASSERT(Assigned(pFontsInfo));

  with pFontsInfo^ do
  begin
{$IFDEF HE_ASSERT}
    ASSERT(LockCount < RefCount,
      'Call DeactivateFontsInfo before calling this.');
{$ELSE}
    ASSERT(LockCount < RefCount);
{$ENDIF}
    if RefCount > 1 then
      Dec(RefCount)
    else
    begin
      FFontsInfo.Remove(pFontsInfo);
      // free all objects
      BaseFont.Free;
      Dispose(pFontsInfo);
    end;
  end;
end;

procedure TheFontsInfoManager.RetrieveLogFontForComparison(ABaseFont: TFont;
  var LF: TLogFont);
var
  pEnd: PChar;
begin
  GetObject(ABaseFont.Handle, SizeOf(TLogFont), @LF);
  with LF do
  begin
    lfItalic := 0;
    lfUnderline := 0;
    lfStrikeOut := 0;
    pEnd := StrEnd(lfFaceName);
    FillChar(pEnd[1], @lfFaceName[High(lfFaceName)] - pEnd, 0);
  end;
end;

{ TheFontStock }

// CalcFontAdvance : Calculation a advance of a character of a font.
//  [*]hCalcFont will be selected as FDC's font if FDC wouldn't be zero.
function TheFontStock.CalcFontAdvance(DC: HDC; pCharHeight: PInteger): Integer;
var
  TM: TTextMetric;
  ABC: TABC;
  HasABC: Boolean;
begin
  // Calculate advance of a character.
  // The following code uses ABC widths instead TextMetric.tmAveCharWidth
  // because ABC widths always tells truth but tmAveCharWidth does not.
  // A true-type font will have ABC widths but others like raster type will not
  // so if the function fails then use TextMetric.tmAveCharWidth.
  GetTextMetrics(DC, TM);
  HasABC := GetCharABCWidths(DC, Ord('M'), Ord('M'), ABC);
  if not HasABC then
  begin
    with ABC do
    begin
      abcA := 0;
      abcB := TM.tmAveCharWidth;
      abcC := 0;
    end;
    TM.tmOverhang := 0;
  end;

  // Result(CharWidth)
  with ABC do
    Result := abcA + Integer(abcB) + abcC + TM.tmOverhang;
  // pCharHeight
  if Assigned(pCharHeight) then
    pCharHeight^ := Abs(TM.tmHeight) {+ TM.tmInternalLeading};
end;

constructor TheFontStock.Create(InitialFont: TFont);
begin
  inherited Create;

  SetBaseFont(InitialFont);
end;

destructor TheFontStock.Destroy;
begin
  ReleaseFontsInfo;
  ASSERT(FDCRefCount = 0);

  inherited;
end;

function TheFontStock.GetBaseFont: TFont;
begin
  Result := FpInfo^.BaseFont;
end;

function TheFontStock.GetCharAdvance: Integer;
begin
  Result := FpCrntFontData^.CharAdv;
end;

function TheFontStock.GetCharHeight: Integer;
begin
  Result := FpCrntFontData^.CharHeight;
end;

function TheFontStock.GetFontData(idx: Integer): PheFontData;
begin
  Result := @FpInfo^.FontsData[idx];
end;

function TheFontStock.GetIsTrueType: Boolean;
begin
  Result := FpInfo^.IsTrueType
end;

function TheFontStock.InternalCreateFont(Style: TFontStyles): HFONT;
const
  Bolds: array[Boolean] of Integer = (400, 700);
begin
  with FBaseLF do
  begin
    lfWeight := Bolds[fsBold in Style];
    lfItalic := Ord(BOOL(fsItalic in Style));
    lfUnderline := Ord(BOOL(fsUnderline in Style));
    lfStrikeOut := Ord(BOOL(fsStrikeOut in Style));
  end;
  Result := CreateFontIndirect(FBaseLF);
end;

function TheFontStock.InternalGetDC: HDC;
begin
  if FDCRefCount = 0 then
  begin
    ASSERT(FDC = 0);
    FDC := GetDC(0);
  end;
  Inc(FDCRefCount);
  Result := FDC;
end;

procedure TheFontStock.InternalReleaseDC(Value: HDC);
begin
  Dec(FDCRefCount);
  if FDCRefCount <= 0 then
  begin
    ASSERT((FDC <> 0) and (FDC = Value));
    ReleaseDC(0, FDC);
    FDC := 0;
    ASSERT(FDCRefCount = 0);
  end;
end;

procedure TheFontStock.ReleaseFontHandles;
begin
  if FUsingFontHandles then
    with GetFontsInfoManager do
    begin
      UnlockFontsInfo(FpInfo);
      FUsingFontHandles := False;
    end;
end;

procedure TheFontStock.ReleaseFontsInfo;
begin
  if Assigned(FpInfo) then
    with GetFontsInfoManager do
    begin
      if FUsingFontHandles then
      begin
        UnlockFontsInfo(FpInfo);
        FUsingFontHandles := False;
      end;
      ReleaseFontsInfo(FpInfo);
      FpInfo := nil;
    end;
end;

procedure TheFontStock.SetBaseFont(Value: TFont);
var
  pInfo: PheSharedFontsInfo;
begin
  if Assigned(Value) then
  begin
    pInfo := GetFontsInfoManager.GetFontsInfo(Value);
    if pInfo = FpInfo then
      GetFontsInfoManager.ReleaseFontsInfo(pInfo)
    else
    begin
      ReleaseFontsInfo;
      FpInfo := pInfo;
      FBaseLF := FpInfo^.BaseLF;
      SetStyle(Value.Style);
    end;
  end
  else
    raise EheFontStockException.Create('SetBaseFont: ''Value'' must be specified.');
end;

procedure TheFontStock.SetStyle(Value: TFontStyles);
var
  idx: Integer;
  DC: HDC;
  hOldFont: HFONT;
  p: PheFontData;
begin
{$IFDEF HE_ASSERT}
  ASSERT(SizeOf(TFontStyles) = 1,
    'TheTextDrawer.SetStyle: There''s more than four font styles but the current '+
    'code expects only four styles.');
{$ELSE}
  ASSERT(SizeOf(TFontStyles) = 1);
{$ENDIF}

  idx := Byte(Value);
  ASSERT(idx <= High(TheStockFontPatterns));

  UseFontHandles;
  p := FontData[idx];
  if FpCrntFontData = p then
    Exit;

  FpCrntFontData := p;
  with p^ do
    if Handle <> 0 then
    begin
      FCrntFont := Handle;
      FCrntStyle := Style;
      Exit;
    end;

  // create font
  FCrntFont := InternalCreateFont(Value);
  DC := InternalGetDC;
  hOldFont := SelectObject(DC, FCrntFont);

  // retrieve height and advances of new font
  with FpCrntFontData^ do
  begin
    Handle := FCrntFont;
    CharAdv := CalcFontAdvance(DC, @CharHeight);
  end;

  SelectObject(DC, hOldFont);
  InternalReleaseDC(DC);
end;

procedure TheFontStock.UseFontHandles;
begin
  if not FUsingFontHandles then
    with GetFontsInfoManager do
    begin
      LockFontsInfo(FpInfo);
      FUsingFontHandles := True;
    end;
end;

{ TheTextDrawer }

constructor TheTextDrawer.Create(CalcExtentBaseStyle: TFontStyles; BaseFont: TFont);
begin
  inherited Create;

  FFontStock := TheFontStock.Create(BaseFont);
  FStockBitmap := TBitmap.Create;
  FCalcExtentBaseStyle := CalcExtentBaseStyle;
  SetBaseFont(BaseFont);
  FColor := clWindowText;
  FBkColor := clWindow;
end;

destructor TheTextDrawer.Destroy;
begin
  FStockBitmap.Free;
  FFontStock.Free;
  ReleaseETODist;
  
  inherited;
end;

procedure TheTextDrawer.ReleaseETODist;
begin
  if Assigned(FETODist) then
  begin
    FreeMem(FETODist);
    FETODist := nil;
  end;
end;

procedure TheTextDrawer.BeginDrawing(DC: HDC);
begin
  if (FDC = DC) then
    ASSERT(FDC <> 0)
  else
  begin
    ASSERT((FDC = 0) and (DC <> 0) and (FDrawingCount = 0));
    FDC := DC;
    FSaveDC := SaveDC(DC);
    SelectObject(DC, FCrntFont);
    Windows.SetTextColor(DC, ColorToRGB(FColor));
    Windows.SetBkColor(DC, ColorToRGB(FBkColor));
    DoSetCharExtra(FCharExtra);
  end;
  Inc(FDrawingCount);
end;

procedure TheTextDrawer.EndDrawing;
begin
  ASSERT(FDrawingCount >= 1);
  Dec(FDrawingCount);
  if FDrawingCount <= 0 then
  begin
    if FDC <> 0 then
      RestoreDC(FDC, FSaveDC);
    FSaveDC := 0;
    FDC := 0;
    FDrawingCount := 0;
  end;
end;

function TheTextDrawer.GetCharWidth: Integer;
begin
  Result := FBaseCharWidth + FCharExtra;
end;

function TheTextDrawer.GetCharHeight: Integer;
begin
  Result := FBaseCharHeight;
end;

procedure TheTextDrawer.SetBaseFont(Value: TFont);
begin
  if Assigned(Value) then
  begin
    ReleaseETODist;
    FStockBitmap.Canvas.Font.Assign(Value);
    FStockBitmap.Canvas.Font.Style := [];
    with FFontStock do
    begin
      SetBaseFont(Value);
      Style := FCalcExtentBaseStyle;
      FBaseCharWidth := CharAdvance;
      FBaseCharHeight := CharHeight;
    end;
    SetStyle(Value.Style);
  end
  else
    raise EheTextDrawerException.Create('SetBaseFont: ''Value'' must be specified.');
end;

procedure TheTextDrawer.SetBaseStyle(const Value: TFontStyles);
begin
  if FCalcExtentBaseStyle <> Value then
  begin
    FCalcExtentBaseStyle := Value;
    ReleaseETODist;
    with FFontStock do
    begin
      Style := Value;
      FBaseCharWidth := CharAdvance;
      FBaseCharHeight := CharHeight;
    end;
  end;
end;

procedure TheTextDrawer.SetStyle(Value: TFontStyles);
begin
  with FFontStock do
  begin
    SetStyle(Value);
    Self.FCrntFont := FontHandle;
  end;
  AfterStyleSet;
end;

procedure TheTextDrawer.AfterStyleSet;
begin
  if FDC <> 0 then
    SelectObject(FDC, FCrntFont);
end;

procedure TheTextDrawer.SetForeColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    if FDC <> 0 then
      SetTextColor(FDC, ColorToRGB(Value));
  end;
end;

procedure TheTextDrawer.SetBackColor(Value: TColor);
begin
  if FBkColor <> Value then
  begin
    FBkColor := Value;
    if FDC <> 0 then
      Windows.SetBkColor(FDC, ColorToRGB(Value));
  end;
end;

procedure TheTextDrawer.SetCharExtra(Value: Integer);
begin
  if FCharExtra <> Value then
  begin
    FCharExtra := Value;
    DoSetCharExtra(FCharExtra);
  end;
end;

procedure TheTextDrawer.DoSetCharExtra(Value: Integer);
begin
  if FDC <> 0 then
    SetTextCharacterExtra(FDC, Value);
end;

procedure TheTextDrawer.TextOut(X, Y: Integer; Text: PWideChar;
  Length: Integer);
var
  r: TRect;
begin
  r := Rect(X, Y, X, Y);
  UniversalExtTextOut(FDC, X, Y, [], r, Text, Length, nil);
end;

procedure TheTextDrawer.ExtTextOut(X, Y: Integer; Options: TTextOutOptions;
  ARect: TRect; Text: PWideChar; Length: Integer);

  procedure InitETODist(CharWidth: Integer);
  var
    Size: TSize;
    i: Integer;
  begin
    ReallocMem(FETODist, Length * SizeOf(Integer));
    for i := 0 to Length - 1 do
    begin
      Size := TextExtent(PWideChar(@Text[i]), 1);
      FETODist[i] := Ceil(Size.cx / CharWidth) * CharWidth;
    end;
  end;

  procedure AdjustLastCharWidthAndRect;
  var
    LastChar: Cardinal;
    RealCharWidth, CharWidth: Integer;
    CharInfo: TABC;
    tm: TTextMetricA;
  begin
    if Length <= 0 then Exit;
    
    LastChar := Ord(Text[Length - 1]);
    CharWidth := FETODist[Length - 1];
    RealCharWidth := CharWidth;
    if Win32PlatformIsUnicode then
    begin
      if GetCharABCWidthsW(FDC, LastChar, LastChar, CharInfo) then
      begin
        RealCharWidth := CharInfo.abcA + Integer(CharInfo.abcB);
        if CharInfo.abcC >= 0 then
          Inc(RealCharWidth, CharInfo.abcC);
      end
      else if LastChar < Ord(High(AnsiChar)) then
      begin
        GetTextMetricsA(FDC, tm);
        RealCharWidth := tm.tmAveCharWidth + tm.tmOverhang;
      end;
    end
    else if WideChar(LastChar) <= High(AnsiChar) then
    begin
      if GetCharABCWidthsA(FDC, LastChar, LastChar, CharInfo) then
      begin
        RealCharWidth := CharInfo.abcA + Integer(CharInfo.abcB);
        if CharInfo.abcC >= 0 then
          Inc(RealCharWidth, CharInfo.abcC);
      end
      else if LastChar < Ord(High(AnsiChar)) then
      begin
        GetTextMetricsA(FDC, tm);
        RealCharWidth := tm.tmAveCharWidth + tm.tmOverhang;
      end;
    end;
    if RealCharWidth > CharWidth then
      Inc(ARect.Right, RealCharWidth - CharWidth);
    FETODist[Length - 1] := Max(RealCharWidth, CharWidth);
  end;

begin
  InitETODist(GetCharWidth);
  AdjustLastCharWidthAndRect;
  UniversalExtTextOut(FDC, X, Y, Options, ARect, Text, Length, FETODist);
end;

procedure TheTextDrawer.ReleaseTemporaryResources;
begin
  FFontStock.ReleaseFontHandles;
end;

function TheTextDrawer.TextExtent(const Text: UnicodeString): TSize;
begin
  Result := SynUnicode.TextExtent(FStockBitmap.Canvas, Text);
end;

function TheTextDrawer.TextExtent(Text: PWideChar; Count: Integer): TSize;
begin
  Result := SynUnicode.GetTextSize(FStockBitmap.Canvas.Handle, Text, Count);
end;

function TheTextDrawer.TextWidth(const Text: UnicodeString): Integer;
begin
  Result := SynUnicode.TextExtent(FStockBitmap.Canvas, Text).cX;
end;

function TheTextDrawer.TextWidth(Text: PWideChar; Count: Integer): Integer;
begin
  Result := SynUnicode.GetTextSize(FStockBitmap.Canvas.Handle, Text, Count).cX;
end;

initialization

finalization
  gFontsInfoManager.Free;

end.
