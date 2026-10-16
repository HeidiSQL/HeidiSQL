{==============================================================================
  Content:  TSynTextDrawer, a helper class for drawing of
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
                        Since TSynTextDrawer grew fat it was split into three
                        classes - TSynFontStock, TSynTextDrawer and TheTextDrawerEx.
                        Currently it should avoid TSynTextDrawer because it is
                        slower than TSynTextDrawer.
            09/25/1999  HANAI Tohru
                        Added internally definition of LeadBytes for Delphi 2
            10/01/1999  HANAI Tohru
                        To save font resources, now all fonts data are shared
                        among all of TSynFontStock instances. With this changing,
                        there added a new class `TSynFontsInfoManager' to manage
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
  {$IFDEF SYN_COMPILER_17_UP}
  Types, UITypes,
  {$ENDIF}
  SynUnicode,
  SynEditTypes,
  SysUtils,
  Classes,
  Windows,
  Graphics;

const
  FontStyleCount = Ord(High(TFontStyle)) + 1;
  FontStyleCombineCount = (1 shl FontStyleCount);

type
  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array[0..MaxInt div SizeOf(Integer) - 1] of Integer;

  TSynStockFontPatterns = 0..FontStyleCombineCount - 1;

  PSynFontData = ^TSynFontData;
  TSynFontData = record
    Style: TFontStyles;
    Handle: HFont;
    CharAdv: Integer;
    CharHeight: Integer;
  end;

  PSynFontsData = ^TSynFontsData;
  TSynFontsData = array[TSynStockFontPatterns] of TSynFontData;

  PSynSharedFontsInfo = ^TSynSharedFontsInfo;
  TSynSharedFontsInfo = record
    // reference counters
    RefCount: Integer;
    LockCount: Integer;
    // font information
    BaseFont: TFont;
    BaseLF: TLogFont;
    IsTrueType: Boolean;
    FontsData: TSynFontsData;
  end;

  { TSynStockFontManager }

  TSynFontsInfoManager = class
  private
    FFontsInfo: TList;
    function FindFontsInfo(const LF: TLogFont): PSynSharedFontsInfo;
    function CreateFontsInfo(ABaseFont: TFont;
      const LF: TLogFont): PSynSharedFontsInfo;
    procedure DestroyFontHandles(pFontsInfo: PSynSharedFontsInfo);
    procedure RetrieveLogFontForComparison(ABaseFont: TFont; var LF: TLogFont);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LockFontsInfo(pFontsInfo: PSynSharedFontsInfo);
    procedure UnLockFontsInfo(pFontsInfo: PSynSharedFontsInfo);
    function GetFontsInfo(ABaseFont: TFont): PSynSharedFontsInfo;
    procedure ReleaseFontsInfo(pFontsInfo: PSynSharedFontsInfo);
  end;

  { TSynFontStock }

  TTextOutOptions = set of (tooOpaque, tooClipped);

  TSynExtTextOutProc = procedure (X, Y: Integer; fuOptions: TTextOutOptions;
    const ARect: TRect; const Text: UnicodeString; Length: Integer) of object;

  ESynFontStockException = class(ESynError);

  TSynFontStock = class
  private
    // Private DC
    FDC: HDC;
    FDCRefCount: Integer;

    // Shared fonts
    FpInfo: PSynSharedFontsInfo;
    FUsingFontHandles: Boolean;

    // Current font
    FCrntFont: HFONT;
    FCrntStyle: TFontStyles;
    FpCrntFontData: PSynFontData;

    // Local font info
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
    function GetFontData(idx: Integer): PSynFontData; virtual;
    procedure UseFontHandles;
    procedure ReleaseFontsInfo;
    procedure SetBaseFont(Value: TFont); virtual;
    procedure SetStyle(Value: TFontStyles); virtual;

    property FontData[idx: Integer]: PSynFontData read GetFontData;
    property FontsInfo: PSynSharedFontsInfo read FpInfo;
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

  { TSynTextDrawer }
  ESynTextDrawerException = class(ESynError);

  TSynTextDrawer = class(TObject)
  private
    FDC: HDC;
    FSaveDC: Integer;

    // Font information
    FFontStock: TSynFontStock;
    FStockBitmap: TBitmap;
    FCalcExtentBaseStyle: TFontStyles;
    FBaseCharWidth: Integer;
    FBaseCharHeight: Integer;

    // Current font and properties
    FCrntFont: HFONT;
    FETODist: PIntegerArray;

    // Current font attributes
    FColor: TColor;
    FBkColor: TColor;
    FCharExtra: Integer;

    // Begin/EndDrawing calling count
    FDrawingCount: Integer;

    // GetCharABCWidthsW cache
    FCharABCWidthCache: array [0..127] of TABC;
    FCharWidthCache: array [0..127] of Integer;
  protected
    procedure ReleaseETODist; virtual;
    procedure AfterStyleSet; virtual;
    procedure DoSetCharExtra(Value: Integer); virtual;
    procedure FlushCharABCWidthCache;
    function GetCachedABCWidth(c : Cardinal; var abc : TABC) : Boolean;

    property StockDC: HDC read FDC;
    property DrawingCount: Integer read FDrawingCount;
    property FontStock: TSynFontStock read FFontStock;
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
    function TextWidth(const Char: WideChar): Integer; overload;
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

function GetFontsInfoManager: TSynFontsInfoManager;

function UniversalExtTextOut(DC: HDC; X, Y: Integer; Options: TTextOutOptions;
  Rect: TRect; Str: PWideChar; Count: Integer; ETODist: PIntegerArray): Boolean;

implementation

uses
  Math
{$IFDEF SYN_UNISCRIBE}
  , SynUsp10
{$ENDIF}
  ;

var
  GFontsInfoManager: TSynFontsInfoManager;

{ utility routines }

function GetFontsInfoManager: TSynFontsInfoManager;
begin
  if not Assigned(GFontsInfoManager) then
    GFontsInfoManager := TSynFontsInfoManager.Create;
  Result := GFontsInfoManager;
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

{ TSynFontsInfoManager }

procedure TSynFontsInfoManager.LockFontsInfo(
  pFontsInfo: PSynSharedFontsInfo);
begin
  Inc(pFontsInfo^.LockCount);
end;

constructor TSynFontsInfoManager.Create;
begin
  inherited;

  FFontsInfo := TList.Create;
end;

function TSynFontsInfoManager.CreateFontsInfo(ABaseFont: TFont;
  const LF: TLogFont): PSynSharedFontsInfo;
begin
  New(Result);
  FillChar(Result^, SizeOf(TSynSharedFontsInfo), 0);
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

procedure TSynFontsInfoManager.UnlockFontsInfo(
  pFontsInfo: PSynSharedFontsInfo);
begin
  with pFontsInfo^ do
  begin
    Dec(LockCount);
    if 0 = LockCount then
      DestroyFontHandles(pFontsInfo);
  end;
end;

destructor TSynFontsInfoManager.Destroy;
begin
  GFontsInfoManager := nil;
  
  if Assigned(FFontsInfo) then
  begin
    while FFontsInfo.Count > 0 do
    begin
      Assert(1 = PSynSharedFontsInfo(FFontsInfo[FFontsInfo.Count - 1])^.RefCount);
      ReleaseFontsInfo(PSynSharedFontsInfo(FFontsInfo[FFontsInfo.Count - 1]));
    end;
    FFontsInfo.Free;
  end;

  inherited;
end;

procedure TSynFontsInfoManager.DestroyFontHandles(
  pFontsInfo: PSynSharedFontsInfo);
var
  i: Integer;
begin
  with pFontsInfo^ do
    for i := Low(TSynStockFontPatterns) to High(TSynStockFontPatterns) do
      with FontsData[i] do
        if Handle <> 0 then
        begin
          DeleteObject(Handle);
          Handle := 0;
        end;
end;

function TSynFontsInfoManager.FindFontsInfo(
  const LF: TLogFont): PSynSharedFontsInfo;
var
  i: Integer;
begin
  for i := 0 to FFontsInfo.Count - 1 do
  begin
    Result := PSynSharedFontsInfo(FFontsInfo[i]);
    if CompareMem(@(Result^.BaseLF), @LF, SizeOf(TLogFont)) then
      Exit;
  end;
  Result := nil;
end;

function TSynFontsInfoManager.GetFontsInfo(ABaseFont: TFont): PSynSharedFontsInfo;
var
  LF: TLogFont;
begin
  Assert(Assigned(ABaseFont));

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

procedure TSynFontsInfoManager.ReleaseFontsInfo(pFontsInfo: PSynSharedFontsInfo);
begin
  Assert(Assigned(pFontsInfo));

  with pFontsInfo^ do
  begin
    Assert(LockCount < RefCount, 'Call DeactivateFontsInfo before calling this.');
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

procedure TSynFontsInfoManager.RetrieveLogFontForComparison(ABaseFont: TFont;
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

{ TSynFontStock }

// CalcFontAdvance : Calculation a advance of a character of a font.
//  [*]hCalcFont will be selected as FDC's font if FDC wouldn't be zero.
function TSynFontStock.CalcFontAdvance(DC: HDC; pCharHeight: PInteger): Integer;
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

constructor TSynFontStock.Create(InitialFont: TFont);
begin
  inherited Create;

  SetBaseFont(InitialFont);
end;

destructor TSynFontStock.Destroy;
begin
  ReleaseFontsInfo;
  Assert(FDCRefCount = 0);

  inherited;
end;

function TSynFontStock.GetBaseFont: TFont;
begin
  Result := FpInfo^.BaseFont;
end;

function TSynFontStock.GetCharAdvance: Integer;
begin
  Result := FpCrntFontData^.CharAdv;
end;

function TSynFontStock.GetCharHeight: Integer;
begin
  Result := FpCrntFontData^.CharHeight;
end;

function TSynFontStock.GetFontData(idx: Integer): PSynFontData;
begin
  Result := @FpInfo^.FontsData[idx];
end;

function TSynFontStock.GetIsTrueType: Boolean;
begin
  Result := FpInfo^.IsTrueType
end;

function TSynFontStock.InternalCreateFont(Style: TFontStyles): HFONT;
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

function TSynFontStock.InternalGetDC: HDC;
begin
  if FDCRefCount = 0 then
  begin
    Assert(FDC = 0);
    FDC := GetDC(0);
  end;
  Inc(FDCRefCount);
  Result := FDC;
end;

procedure TSynFontStock.InternalReleaseDC(Value: HDC);
begin
  Dec(FDCRefCount);
  if FDCRefCount <= 0 then
  begin
    Assert((FDC <> 0) and (FDC = Value));
    ReleaseDC(0, FDC);
    FDC := 0;
    Assert(FDCRefCount = 0);
  end;
end;

procedure TSynFontStock.ReleaseFontHandles;
begin
  if FUsingFontHandles then
    with GetFontsInfoManager do
    begin
      UnlockFontsInfo(FpInfo);
      FUsingFontHandles := False;
    end;
end;

procedure TSynFontStock.ReleaseFontsInfo;
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

procedure TSynFontStock.SetBaseFont(Value: TFont);
var
  pInfo: PSynSharedFontsInfo;
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
    raise ESynFontStockException.Create('SetBaseFont: ''Value'' must be specified.');
end;

procedure TSynFontStock.SetStyle(Value: TFontStyles);
var
  idx: Integer;
  DC: HDC;
  hOldFont: HFONT;
  p: PSynFontData;
begin
  Assert(SizeOf(TFontStyles) = 1,
    'TheTextDrawer.SetStyle: There''s more than four font styles but the current '+
    'code expects only four styles.');

  idx := Byte(Value);
  Assert(idx <= High(TSynStockFontPatterns));

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

procedure TSynFontStock.UseFontHandles;
begin
  if not FUsingFontHandles then
    with GetFontsInfoManager do
    begin
      LockFontsInfo(FpInfo);
      FUsingFontHandles := True;
    end;
end;

{ TSynTextDrawer }

constructor TSynTextDrawer.Create(CalcExtentBaseStyle: TFontStyles; BaseFont: TFont);
begin
  inherited Create;

  FFontStock := TSynFontStock.Create(BaseFont);
  FStockBitmap := TBitmap.Create;
  FCalcExtentBaseStyle := CalcExtentBaseStyle;
  SetBaseFont(BaseFont);
  FColor := clWindowText;
  FBkColor := clWindow;
end;

destructor TSynTextDrawer.Destroy;
begin
  FStockBitmap.Free;
  FFontStock.Free;
  ReleaseETODist;
  
  inherited;
end;

procedure TSynTextDrawer.ReleaseETODist;
begin
  if Assigned(FETODist) then
  begin
    FreeMem(FETODist);
    FETODist := nil;
  end;
end;

procedure TSynTextDrawer.BeginDrawing(DC: HDC);
begin
  if (FDC = DC) then
    Assert(FDC <> 0)
  else
  begin
    Assert((FDC = 0) and (DC <> 0) and (FDrawingCount = 0));
    FDC := DC;
    FSaveDC := SaveDC(DC);
    SelectObject(DC, FCrntFont);
    Windows.SetTextColor(DC, ColorToRGB(FColor));
    Windows.SetBkColor(DC, ColorToRGB(FBkColor));
    DoSetCharExtra(FCharExtra);
  end;
  Inc(FDrawingCount);
end;

procedure TSynTextDrawer.EndDrawing;
begin
  Assert(FDrawingCount >= 1);
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

function TSynTextDrawer.GetCharWidth: Integer;
begin
  Result := FBaseCharWidth + FCharExtra;
end;

function TSynTextDrawer.GetCharHeight: Integer;
begin
  Result := FBaseCharHeight;
end;

procedure TSynTextDrawer.SetBaseFont(Value: TFont);
begin
  if Assigned(Value) then
  begin
    FlushCharABCWidthCache;
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
    raise ESynTextDrawerException.Create('SetBaseFont: ''Value'' must be specified.');
end;

procedure TSynTextDrawer.SetBaseStyle(const Value: TFontStyles);
begin
  if FCalcExtentBaseStyle <> Value then
  begin
    FCalcExtentBaseStyle := Value;
    FlushCharABCWidthCache;
    ReleaseETODist;
    with FFontStock do
    begin
      Style := Value;
      FBaseCharWidth := CharAdvance;
      FBaseCharHeight := CharHeight;
    end;
  end;
end;

procedure TSynTextDrawer.SetStyle(Value: TFontStyles);
begin
  with FFontStock do
  begin
    SetStyle(Value);
    Self.FCrntFont := FontHandle;
  end;
  AfterStyleSet;
end;

procedure TSynTextDrawer.AfterStyleSet;
begin
  if FDC <> 0 then
    SelectObject(FDC, FCrntFont);
end;

procedure TSynTextDrawer.SetForeColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    if FDC <> 0 then
      SetTextColor(FDC, ColorToRGB(Value));
  end;
end;

procedure TSynTextDrawer.SetBackColor(Value: TColor);
begin
  if FBkColor <> Value then
  begin
    FBkColor := Value;
    if FDC <> 0 then
      Windows.SetBkColor(FDC, ColorToRGB(Value));
  end;
end;

procedure TSynTextDrawer.SetCharExtra(Value: Integer);
begin
  if FCharExtra <> Value then
  begin
    FCharExtra := Value;
    DoSetCharExtra(FCharExtra);
  end;
end;

procedure TSynTextDrawer.DoSetCharExtra(Value: Integer);
begin
  if FDC <> 0 then
    SetTextCharacterExtra(FDC, Value);
end;

procedure TSynTextDrawer.FlushCharABCWidthCache;
begin
  FillChar(FCharABCWidthCache, SizeOf(TABC) * Length(FCharABCWidthCache), 0);
  FillChar(FCharWidthCache, SizeOf(Integer) * Length(FCharWidthCache), 0);
end;

function TSynTextDrawer.GetCachedABCWidth(c: Cardinal; var abc: TABC) : Boolean;
begin
  if c > High(FCharABCWidthCache) then
  begin
    Result := GetCharABCWidthsW(FDC, c, c, abc);
    Exit;
  end;
  abc := FCharABCWidthCache[c];
  if (abc.abcA or Integer(abc.abcB) or abc.abcC) = 0 then
  begin
    Result := GetCharABCWidthsW(FDC, c, c, abc);
    if Result then
      FCharABCWidthCache[c] := abc;
  end
  else
    Result := True;
end;

procedure TSynTextDrawer.TextOut(X, Y: Integer; Text: PWideChar;
  Length: Integer);
var
  r: TRect;
begin
  r := Rect(X, Y, X, Y);
  UniversalExtTextOut(FDC, X, Y, [], r, Text, Length, nil);
end;

procedure TSynTextDrawer.ExtTextOut(X, Y: Integer; Options: TTextOutOptions;
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
      if Size.cx <> CharWidth then
         FETODist[i] := Ceil(Size.cx / CharWidth) * CharWidth
      else FETODist[i] := CharWidth;
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
      if GetCachedABCWidth(LastChar, CharInfo) then
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

procedure TSynTextDrawer.ReleaseTemporaryResources;
begin
  FFontStock.ReleaseFontHandles;
end;

function TSynTextDrawer.TextExtent(const Text: UnicodeString): TSize;
begin
  Result := SynUnicode.TextExtent(FStockBitmap.Canvas, Text);
end;

function TSynTextDrawer.TextExtent(Text: PWideChar; Count: Integer): TSize;
begin
  Result := SynUnicode.GetTextSize(FStockBitmap.Canvas.Handle, Text, Count);
end;

function TSynTextDrawer.TextWidth(const Char: WideChar): Integer;
var
  c: Cardinal;
begin
  c := Ord(Char);
  if c <= High(FCharWidthCache) then
  begin
    Result := FCharWidthCache[c];
    if Result = 0 then
    begin
      Result := SynUnicode.TextExtent(FStockBitmap.Canvas, Char).cX;
      FCharWidthCache[c] := Result;
    end;
  end
  else
    Result := SynUnicode.TextExtent(FStockBitmap.Canvas, Char).cX;
end;

function TSynTextDrawer.TextWidth(const Text: UnicodeString): Integer;
var
  c: Cardinal;
begin
  if Length(Text) = 1 then
  begin
    c := Ord(Text[1]);
    if c <= High(FCharWidthCache) then
    begin
      Result := FCharWidthCache[c];
      if Result = 0 then
      begin
        Result := SynUnicode.TextExtent(FStockBitmap.Canvas, Text).cX;
        FCharWidthCache[c] := Result;
      end;
      Exit;
    end;
  end;
  Result := SynUnicode.TextExtent(FStockBitmap.Canvas, Text).cX;
end;

function TSynTextDrawer.TextWidth(Text: PWideChar; Count: Integer): Integer;
begin
  Result := SynUnicode.GetTextSize(FStockBitmap.Canvas.Handle, Text, Count).cX;
end;

initialization

finalization
  GFontsInfoManager.Free;

end.
