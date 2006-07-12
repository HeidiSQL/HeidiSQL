{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditPrint.pas, released 2000-04-14.

The Original Code is partly based on the mwCustomEdit.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Hieke.
Portions created by Michael Hieke are Copyright 2000 Michael Hieke.
Portions created by Bradley S. Stowers are Copyright 1999 Bradley S. Stowers.
All Rights Reserved.

Contributors to SynEdit project are listed in Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynEditPrint_Old.pas,v 1.5 2000/11/26 11:52:09 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:  - to be implemented: limit font selection for text body to
                 fixed width fonts
               - to be implemented: print ranges (column pos of start and stop
                 lines are ignored)
               - !!! muPixels should be removed altogether !!!
-------------------------------------------------------------------------------}

{ Implements syntax highlighted printing of text. }
unit SynEditPrint_Old;

{$I SynEdit.inc}

interface

uses
  Classes, Windows, Graphics, SynEditMiscProcs, SynEditHighlighter;

type
  { Parameter type for status event }
  TSynPrintStatus = (psBegin, psNewPage, psEnd);
  { Print status event allows to cancel the print job }
  TPrintStatusEvent = procedure(Sender: TObject; Status: TSynPrintStatus;
    PageNumber: integer; var Abort: boolean) of object;
  { Page margins unit types }
  TSynMarginUnits = (muPixels, muThousandthsOfInches, muMillimeters);
  { Parser result types describe the type of header or footer line }
  TSynHFParseResult = (prEmpty, prTextLine, prHorzRulerTop, prHorzRulerCenter,
    prHorzRulerBottom);
  { Filter property to select the pages to print }
  TSynPrintFilter = (pfAll, pfEven, pfOdd);                                 
  { Event to customize the format of line number printing }
  TPrintGetLineNumberFormatEvent = procedure(Sender: TObject;
    MaxLineNumber: integer; var Format: string);

  { Printing base class that stores the settings for the printout and renders
    the text to the printer.  Customizable header and footer lines with
    different font.  Highlighter settings are responsible for printer output
    format.  Pagination allows for choosing page ranges in the print dialog. }
  TSynPrintout = class(TObject)
  protected
    { Printable area (depending on print margins) in printer device units }
    fPrintable: TRect;
    { Print rect for current line in printer device units }
    fLineRect: TRect;
    { Default background color of printout }
    fDefaultBG: TColor;
    { Array of maximum character width integers for ExtTextOut() call }
    fETOWidths: PIntArray;
    { Format for converting the current line number into the printed string }
    fLineNumberFmt: string;
    { Number of currently printed page }
    fPageNum: integer;
    { Visible chars per line for these fonts, headers, footers and margins }
    fCharsPerTextLine: integer;
    { Visible lines per page for these fonts, headers, footers and margins }
    fTextLinesPerPage: integer;
    { Width and height of the chars in header and footer lines }
    fHFFontSize: TPoint;
    { Width and height of the chars in the text body of the page }
    fTextFontSize: TPoint;
    { Wrapper for ExtTextOut() WINAPI function with less parameters }
    procedure ExtTextOut(ACanvas: TCanvas; X: integer; ARect: TRect;
      const Token: string; TokenLen: integer);
    { Computes the printable area in printer device units depending on the
      page margins and the printer capabilities }
    function InternalPrintableArea(PrinterDPI: TPoint): TRect;
    { Returns the formatted print date }
    function InternalPrintDate: string;
    { Returns the formatted print time }
    function InternalPrintTime: string;
    { Returns whether a given page should be printed }
    function ShouldPrint(APageNum: integer): boolean;                           //rs 2000-07-14
  protected
    fBlackAndWhite: boolean;
    fFilter: TSynPrintFilter;                                                   //rs 2000-07-14
    fFooterLines: TStrings;
    fHeaderLines: TStrings;
    fHFTextFont: TFont;
    fHighlighter: TSynCustomHighlighter;
    fHighlighterRangesOK: boolean;
    fLines: TStrings;
    fMargins: TRect;
    fMarginUnits: TSynMarginUnits;
    fNumCopies: integer;
    // this holds the line numbers for the first line of each page (pagination)
    fPageStartLineNums: TList;
    fPaginationDone: boolean;
    fPrintDate: string;
    fPrintDateTime: TDateTime;
    fPrintLineNumbers: boolean;
    fPrintRangeStart: TPoint;
    fPrintRangeEnd: TPoint;
    fPrintTime: string;
    fTextFont: TFont;
    fTitle: string;
    fWrapLongLines: boolean;
    fOnGetLineNumberFormat: TPrintGetLineNumberFormatEvent;
    fOnPrintStatus: TPrintStatusEvent;
    function DoCancelPrint(Status: TSynPrintStatus;
      PageNumber: integer): boolean; virtual;
    procedure DoGetLineNumberFormat(MaxLineNumber: integer); virtual;
    procedure EnsurePagination; virtual;
    function GetMargin(Index: integer): integer;                                //mh 2000-10-31
    function GetPageCount: integer;
    procedure LinesOrFontsChanged(Sender: TObject);
    procedure NeedsPagination;
    procedure Paginate; virtual;
    function ParseHeaderFooterLine(Line: string;
      var LineLeft, LineCenter, LineRight: string): TSynHFParseResult; virtual;
    procedure PrintHeaderFooterLines(ACanvas: TCanvas; AFooter: boolean);
    procedure PrintPage(PageNum: integer); virtual;
    procedure PrintTextLines(ACanvas: TCanvas; First, Last: integer);
    procedure SetFooterLines(Value: TStrings); virtual;
    procedure SetHeaderLines(Value: TStrings); virtual;
    procedure SetHFTextFont(Value: TFont); virtual;
    procedure SetHighlighter(Value: TSynCustomHighlighter); virtual;
    procedure SetLines(Value: TStrings); virtual;
    procedure SetMargin(Index: integer; Value: integer);                        //mh 2000-10-31
    procedure SetMarginUnits(Value: TSynMarginUnits);                           //mh 2000-10-31
    procedure SetPrintDateTime(Value: TDateTime);
    procedure SetPrintRangeEnd(Value: TPoint);
    procedure SetPrintRangeStart(Value: TPoint);
    procedure SetTextFont(Value: TFont); virtual;
    procedure SetWrapLongLines(Value: boolean); virtual;
    function WrapIndex(const Line: string; Start, MaxLength: integer;
      ForceWrap: boolean): integer; virtual;
    function WrapLineCount(Line: string; MaxLength: integer): integer; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    { Converts all tabs in Lines property to spaces.  Called only once to speed
      up pagination and rendering. }
    procedure LinesConvertTabsToSpaces(TabWidth: integer);
    { Print will print everything between PrintRangeStart and PrintRangeEnd }
    procedure Print;
    { PrintRange will print only the selected pages between PrintRangeStart
      and PrintRangeEnd }
    procedure PrintRange(FirstPage, LastPage: integer);
  public
    { Number of print copies }
    property Copies: integer read fNumCopies write fNumCopies;
    { Select the pages to print }
    property Filter: TSynPrintFilter read fFilter write fFilter;
    { Text to be printed in the footer area of each page }
    property FooterLines: TStrings read fFooterLines
      write SetFooterLines;
    { Font to print header and footer lines in.  Does not need to be a
      fixed width font. }
    property HeaderFooterFont: TFont read fHFTextFont write SetHFTextFont;
    { Text to be printed in the header area of each page }
    property HeaderLines: TStrings read fHeaderLines
      write SetHeaderLines;
    { Highlighter to scan tokens and retrieve font styles and foreground /
      background colors for all tokens }
    property Highlighter: TSynCustomHighlighter read fHighlighter
      write SetHighlighter;
    { Force printout to black and white }
    property IgnoreHighlighterColors: boolean read fBlackAndWhite
      write fBlackAndWhite;
    { Text to print }
    property Lines: TStrings read fLines write SetLines;
{begin}                                                                         //mh 2000-10-31
    { Bottom margin of the printable area }
    property MarginBottom: integer index 3 read GetMargin write SetMargin;
    { Left margin of the printable area }
    property MarginLeft: integer index 0 read GetMargin write SetMargin;
    { Right margin of the printable area }
    property MarginRight: integer index 2 read GetMargin write SetMargin;
    { Top margin of the printable area }
    property MarginTop: integer index 1 read GetMargin write SetMargin;
    { Unit of the margin values (like millimeters or thousands of inches) }
    property MarginUnits: TSynMarginUnits read fMarginUnits
      write SetMarginUnits;
{end}                                                                           //mh 2000-10-31
    { Number of pages the Lines will occupy under the current settings }
    property PageCount: integer read GetPageCount;
    { Date and time of printing, is initialized with current value in the
      constructor, but could be changed }
    property PrintDate: TDateTime read fPrintDateTime
      write SetPrintDateTime;
    { Enable the output of line numbers (only in the first line when word-wrap
      is on) }
    property PrintLineNumbers: boolean read fPrintLineNumbers
      write fPrintLineNumbers;
    { End line and column for printout.  Can be used to print only selected
      area of an editor control }
    property PrintRangeEnd: TPoint read fPrintRangeEnd
      write SetPrintRangeEnd;
    { Start line and column for printout.  Can be used to print only selected
      area of an editor control }
    property PrintRangeStart: TPoint read fPrintRangeStart
      write SetPrintRangeStart;
    { Font to print the page body in, has to be a fixed width font }
    property TextFont: TFont read fTextFont write SetTextFont;
    { Title of printout for use in the header line and for the print job
      identification in the print spooler }
    property Title: string read fTitle write fTitle;
    { Wrap long lines at word boundaries }
    property WrapLongLines: boolean read fWrapLongLines write SetWrapLongLines;
    { Get a customized format for line numbering }
    property OnGetLineNumberFormat: TPrintGetLineNumberFormatEvent
      read fOnGetLineNumberFormat write fOnGetLineNumberFormat;
    { Event to cancel the printing process }
    property OnPrintStatus: TPrintStatusEvent read fOnPrintStatus
      write fOnPrintStatus;
  end;

{ Helper function to return a formatted line the parser will understand }
function MakeHeaderFooterLine(LineLeft, LineCenter, LineRight: string): string;

implementation

uses
  SysUtils, Forms, Controls, Printers;

{ TSynPrintout }

constructor TSynPrintout.Create;
begin
  inherited Create;
  fLines := TStringList.Create;
  TStringList(fLines).OnChange := LinesOrFontsChanged;
  fFooterLines := TStringList.Create;
  TStringList(fFooterLines).OnChange := LinesOrFontsChanged;
  fHeaderLines := TStringList.Create;
  TStringList(fHeaderLines).OnChange := LinesOrFontsChanged;
  fPageStartLineNums := TList.Create;
  fFilter := pfAll;                                                    
  fMarginUnits := muMillimeters;
  fMargins := Rect(25, 20, 20, 20);
  fNumCopies := 1;
  fPrintDateTime := Now;
  fTextFont := TFont.Create;
  SetTextFont(nil);
  fTextFont.OnChange := LinesOrFontsChanged;
  fHFTextFont := TFont.Create;
  SetHFTextFont(nil);
  fHFTextFont.OnChange := LinesOrFontsChanged;
end;

destructor TSynPrintout.Destroy;
begin
  fFooterLines.Free;
  fHeaderLines.Free;
  fPageStartLineNums.Free;
  inherited Destroy;
end;

function TSynPrintout.DoCancelPrint(Status: TSynPrintStatus;
  PageNumber: integer): boolean;
begin
  Result := FALSE;
  if Assigned(fOnPrintStatus) then
    fOnPrintStatus(Self, Status, PageNumber, Result);
end;

procedure TSynPrintout.DoGetLineNumberFormat(MaxLineNumber: integer);
const
  FmtLimits: array[1..9] of integer = (10, 100, 1000, 10000, 100000, 1000000,
    10000000, 100000000, 100000000);
var
  i: integer;
begin
  if Assigned(fOnGetLineNumberFormat) then begin
    fLineNumberFmt := '';
    fOnGetLineNumberFormat(Self, MaxLineNumber, fLineNumberFmt);
    if fLineNumberFmt <> '' then
      exit;
  end;

  for i := Low(FmtLimits) to High(FmtLimits) do
    if MaxLineNumber < FmtLimits[i] then begin
      fLineNumberFmt := Format('%%%dd: ', [i]); // will become "%[i]d: "
      break;
    end;
end;

procedure TSynPrintout.EnsurePagination;
begin
  if not fPaginationDone then
    Paginate;
end;

procedure TSynPrintout.ExtTextOut(ACanvas: TCanvas; X: integer; ARect: TRect;
  const Token: string; TokenLen: integer);
var
  CharsToPrint: integer;
begin
  CharsToPrint := Max(0, Min(TokenLen, 1 + (ARect.Right - X) div
    fTextFontSize.x));
  if CharsToPrint > 0 then
    Windows.ExtTextOut(ACanvas.Handle, X, ARect.Top, ETO_OPAQUE or ETO_CLIPPED,
      @ARect, PChar(Token), CharsToPrint, @fETOWidths[0]);
end;

{begin}                                                                         //mh 2000-10-31
function TSynPrintout.GetMargin(Index: integer): integer;
begin
  case Index of
    0: Result := fMargins.Left;
    1: Result := fMargins.Top;
    2: Result := fMargins.Right;
    3: Result := fMargins.Bottom;
  else
    Result := 0;
  end
end;
{end}                                                                           //mh 2000-10-31

function TSynPrintout.GetPageCount: integer;
begin
  EnsurePagination;
  Result := fPageStartLineNums.Count;
end;

function TSynPrintout.InternalPrintableArea(PrinterDPI: TPoint): TRect;
var
  XOffset, YOffset: integer;
begin
  case fMarginUnits of
    muThousandthsOfInches:
      with Result do begin
        Left := MulDiv(fMargins.Left, PrinterDPI.x, 1000);
        Right := MulDiv(fMargins.Right, PrinterDPI.x, 1000);
        Top := MulDiv(fMargins.Top, PrinterDPI.y, 1000);
        Bottom := MulDiv(fMargins.Bottom, PrinterDPI.y, 1000);
      end;
    muMillimeters:
      with Result do begin
        // Same as inches, but divide by 0.0254 (25.4 mm per inch)
        Left := MulDiv(MulDiv(fMargins.Left, PrinterDPI.x, 1), 10, 254);
        Right := MulDiv(MulDiv(fMargins.Right, PrinterDPI.x, 1), 10, 254);
        Top := MulDiv(MulDiv(fMargins.Top, PrinterDPI.y, 1), 10, 254);
        Bottom := MulDiv(MulDiv(fMargins.Bottom, PrinterDPI.y, 1), 10, 254);
      end;
    else // muPixels
      Result := fMargins;
  end;
  XOffset := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);
  YOffset := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY);
  with Result do begin
    Left := Max(Left - XOffset, 0);
    Top := Max(Top - YOffset, 0);
    Right := Min(GetDeviceCaps(Printer.Handle, HORZRES),
      GetDeviceCaps(Printer.Handle, PHYSICALWIDTH) - XOffset - Right);
    Bottom := Min(GetDeviceCaps(Printer.Handle, VERTRES),
      GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT) - YOffset - Bottom);
  end;
end;

function TSynPrintout.InternalPrintDate: string;
begin
  if fPrintDate = '' then
    fPrintDate := DateToStr(fPrintDateTime);
  Result := fPrintDate;
end;

function TSynPrintout.InternalPrintTime: string;
begin
  if fPrintTime = '' then
    fPrintTime := TimeToStr(fPrintDateTime);
  Result := fPrintTime;
end;

procedure TSynPrintout.LinesConvertTabsToSpaces(TabWidth: integer);
var
  i: integer;
  pConvert: TConvertTabsProc;
begin
  pConvert := GetBestConvertTabsProc(TabWidth);

  fLines.BeginUpdate;
  try
    for i := 0 to fLines.Count - 1 do
      fLines[i] := pConvert(fLines[i], TabWidth);
  finally
    fLines.EndUpdate;
  end;
end;

procedure TSynPrintout.LinesOrFontsChanged(Sender: TObject);
begin
  NeedsPagination;
end;

procedure TSynPrintout.NeedsPagination;
begin
  if fPaginationDone then begin
    fPaginationDone := FALSE;
    fPageStartLineNums.Clear;
  end;
end;

procedure TSynPrintout.Paginate;
var
  TestFont: TFont;
  i, iLine: integer;
  Attr: TSynHighlighterAttributes;
  AttrStyles: array[0..15] of boolean;
  WH: TPoint;
  ScreenDPI: integer;
  PrinterDPI: TPoint;
  TextBody, Wrapped: integer;

  function GetPrinterFontDimensions(AFont: TFont): TPoint;
  var
    oldFont: HFont;
    tm: TTextMetric;
  begin
    Result := Point(0, 0);
    oldFont := SelectObject(Printer.Handle, AFont.Handle);
    try
      if GetTextMetrics(Printer.Handle, tm) then
        Result := Point(MulDiv(tm.tmAveCharWidth, PrinterDPI.x, ScreenDPI),
          MulDiv(tm.tmHeight + tm.tmExternalLeading, PrinterDPI.y, ScreenDPI))
    finally
      SelectObject(Printer.Handle, oldFont);
    end;
  end;

begin
  // determine the line range states if necessary
  if not fHighlighterRangesOK and Assigned(fHighlighter) and (Lines.Count > 0)
  then begin
    fHighlighter.ResetRange;
    fLines.Objects[0] := fHighlighter.GetRange;
    i := 1;
    while (i < Lines.Count) do begin
      fHighlighter.SetLine(fLines[i - 1], i - 1);
      fHighlighter.NextToEol;
      fLines.Objects[i] := fHighlighter.GetRange;
      Inc(i);
    end;
    fHighlighterRangesOK := TRUE;
  end;
  // determine the font dimensions
  TestFont := TFont.Create;
  try
    ScreenDPI := Screen.PixelsPerInch;
    PrinterDPI := Point(GetDeviceCaps(Printer.Handle, LOGPIXELSX),
      GetDeviceCaps(Printer.Handle, LOGPIXELSY));
    // determine the font dimensions for header and footer lines
    TestFont.Assign(fHFTextFont);
    fHFFontSize := GetPrinterFontDimensions(TestFont);
    // text body dimensions consist of maximum values that are searched using
    // all the possible font styles
    TestFont.Assign(fTextFont);
    fTextFontSize := GetPrinterFontDimensions(TestFont);
    if Assigned(fHighlighter) then begin
      fTextFontSize := Point(0, 0);
      FillChar(AttrStyles, SizeOf(AttrStyles), 0);
      for i := 0 to fHighlighter.AttrCount - 1 do begin
        Attr := fHighlighter.Attribute[i];
        if not Assigned(Attr) then
          continue;
        if AttrStyles[Attr.IntegerStyle] then
          continue;
        AttrStyles[Attr.IntegerStyle] := TRUE;
        TestFont.Style := Attr.Style;
        WH := GetPrinterFontDimensions(TestFont);
        if WH.x > fTextFontSize.x then
          fTextFontSize.x := WH.x;
        if WH.y > fTextFontSize.y then
          fTextFontSize.y := WH.y;
      end;
    end;
  finally
    TestFont.Free;
  end;
  // compute the printable rect
  fPrintable := InternalPrintableArea(PrinterDPI);
  TextBody := fPrintable.Bottom - fPrintable.Top;
  Dec(TextBody, fHFFontSize.y * (fHeaderLines.Count + fFooterLines.Count));
  fTextLinesPerPage := Max(0, TextBody) div fTextFontSize.y;
  // map this to the lines that are to be printed
  fPrintRangeStart.y := Max(1, fPrintRangeStart.y);
  if fPrintRangeEnd.y < 1 then
    fPrintRangeEnd.y := fLines.Count
  else
    fPrintRangeEnd.y := Min(fPrintRangeEnd.y, fLines.Count);
  // determine the number of printable chars per line
  with fPrintable do
    fCharsPerTextLine := (Right - Left + fTextFontSize.x - 1)
      div fTextFontSize.x;
  if fPrintLineNumbers then begin
    DoGetLineNumberFormat(fPrintRangeEnd.y);
    Dec(fCharsPerTextLine, Length(Format(fLineNumberFmt, [fPrintRangeEnd.y])));
  end;
  // compute the line number of the first line for each page and store it
  iLine := fPrintRangeStart.y - 1;
  while iLine < fPrintRangeEnd.y do begin
    fPageStartLineNums.Add(pointer(iLine));
    if fWrapLongLines then begin
      i := 0;
      while (iLine < fPrintRangeEnd.y) and (i < fTextLinesPerPage) do begin
        Wrapped := WrapLineCount(TrimRight(fLines[iLine]), fCharsPerTextLine);
        if i + Wrapped > fTextLinesPerPage then
          break;
        Inc(i, Wrapped);
        Inc(iLine);
      end;
    end else
      Inc(iLine, fTextLinesPerPage);
  end;
  fPaginationDone := TRUE;
end;

function MakeHeaderFooterLine(LineLeft, LineCenter, LineRight: string): string;
begin
  Result := LineLeft;
  if LineCenter <> '' then
    Result := Result + '$CENTER$' + LineCenter;
  if LineRight <> '' then
    Result := Result + '$RIGHT$' + LineRight;
end;

function TSynPrintout.ParseHeaderFooterLine(Line: string;
  var LineLeft, LineCenter, LineRight: string): TSynHFParseResult;
var
  TrimmedLine: string;
  Align: TAlignment;
  Len, Start, Run: integer;

  procedure DoAppend(AText: string);
  begin
    case Align of
      taCenter: LineCenter := LineCenter + AText;
      taRightJustify: LineRight := LineRight + AText;
      else LineLeft := LineLeft + AText;
    end;
  end;

  procedure TryAppend(var First: integer; After: integer);
  begin
    if After > First then begin
      DoAppend(Copy(Line, First, After - First));
      First := After;
    end;
  end;

  function TryExecuteMacro: boolean;
  var
    Macro: string;
  begin
    Result := TRUE;
    Macro := UpperCase(Copy(Line, Start, Run - Start + 1));
    if Macro = '$RIGHT$' then
      Align := taRightJustify
    else if Macro = '$CENTER$' then
      Align := taCenter
    else if Macro = '$PAGENUM$' then
      DoAppend(IntToStr(fPageNum))
    else if Macro = '$PAGECOUNT$' then
      DoAppend(IntToStr(fPageStartLineNums.Count))
    else if Macro = '$TITLE$' then
      DoAppend(fTitle)
    else if Macro = '$DATE$' then
      DoAppend(InternalPrintDate)
    else if Macro = '$TIME$' then
      DoAppend(InternalPrintTime)
    else if Macro = '$DATETIME$' then
      DoAppend(InternalPrintDate + ' ' + InternalPrintTime)
    else if Macro = '$TIMEDATE$' then
      DoAppend(InternalPrintTime + ' ' + InternalPrintDate)
    else
      Result := FALSE;
  end;

begin
  // try some special macros for lines and such
  TrimmedLine := Trim(Line);
  if TrimmedLine = '' then begin
    Result := prEmpty;
    exit;
  end;
  if (TrimmedLine[1] = '$') and (TrimmedLine[Length(TrimmedLine)] = '$') then
  begin
    if CompareText(TrimmedLine, '$LINE$') = 0 then begin
      Result := prHorzRulerCenter;
      exit;
    end;
    if CompareText(TrimmedLine, '$LINETOP$') = 0 then begin
      Result := prHorzRulerTop;
      exit;
    end;
    if CompareText(TrimmedLine, '$LINEBOTTOM$') = 0 then begin
      Result := prHorzRulerBottom;
      exit;
    end;
  end;

  Result := prTextLine;
  LineLeft := '';
  LineCenter := '';
  LineRight := '';
  // parse the line
  Len := Length(Line);
  if Len > 0 then begin
    // start with left-aligned text
    Align := taLeftJustify;
    Start := 1;
    Run := 1;
    while Run <= Len do begin
      // test for embedded macro
      if Line[Run] = '$' then begin
        TryAppend(Start, Run);
        Inc(Run);
        // search for next '$' which could mark the end of a macro
        while Run <= Len do begin
          if Line[Run] = '$' then begin
            // if this is a macro execute it and skip the chars from output
            if TryExecuteMacro then begin
              Inc(Run); // also the '$'
              Start := Run;
              break;
            end else
              // this '$' might again be the start of a macro
              TryAppend(Start, Run);
          end else
            Inc(Run);
        end;
      end else
        Inc(Run);
    end;
    TryAppend(Start, Run);
  end;
end;

procedure TSynPrintout.Print;
begin
  PrintRange(1, MaxInt);
end;

procedure TSynPrintout.PrintHeaderFooterLines(ACanvas: TCanvas;
  AFooter: boolean);
var
  HFLines: TStrings;
  i, Count: integer;
  Parse: TSynHFParseResult;
  LineLeft, LineCenter, LineRight: string;
  X, Y: integer;
begin
  // select the header / footer font into the printer canvas
  ACanvas.Font.Assign(fHFTextFont);
  ACanvas.Brush.Style := bsClear;
  // print the lines
  if AFooter then begin
    HFLines := fFooterLines;
    fLineRect.Top := fPrintable.Bottom - fFooterLines.Count * fHFFontSize.y;
  end else begin
    HFLines := fHeaderLines;
    fLineRect.Top := fPrintable.Top;
  end;
  // loop through all the lines and print them
  Count := HFLines.Count - 1;
  for i := 0 to Count do begin
    fLineRect.Bottom := fLineRect.Top + fHFFontSize.y;
    Parse := ParseHeaderFooterLine(HFLines[i], LineLeft, LineCenter, LineRight);
    case Parse of
      prEmpty: {};
      prTextLine:
        begin
          // no point in forcing a non-proportional font here...
          if LineRight <> '' then begin
            SetTextAlign(ACanvas.Handle, TA_RIGHT or TA_TOP);
            ACanvas.TextOut(fLineRect.Right, fLineRect.Top, LineRight);
          end;
          if LineCenter <> '' then begin
            SetTextAlign(ACanvas.Handle, TA_CENTER or TA_TOP);
            X := fLineRect.Left + (fLineRect.Right - fLineRect.Left) div 2;
            ACanvas.TextOut(X, fLineRect.Top, LineCenter);
          end;
          if LineLeft <> '' then begin
            SetTextAlign(ACanvas.Handle, TA_LEFT or TA_TOP);
            ACanvas.TextOut(fLineRect.Left, fLineRect.Top, LineLeft);
          end;
        end;
      prHorzRulerTop, prHorzRulerCenter, prHorzRulerBottom:
        with ACanvas do begin
          Pen.Width := 1;
          case Parse of
            prHorzRulerTop:
              Y := fLineRect.Top;
            prHorzRulerBottom:
              Y := fLineRect.Bottom;
            else
              Y := fLineRect.Top + (fLineRect.Bottom - fLineRect.Top) div 2;
          end;
          MoveTo(fLineRect.Left, Y);
          LineTo(fLineRect.Right, Y);
        end;
    end;
    fLineRect.Top := fLineRect.Bottom;
  end;
end;

procedure TSynPrintout.PrintPage(PageNum: integer);
var
  First, Last: integer;
begin
  if not DoCancelPrint(psNewPage, PageNum) then begin
    fPageNum := PageNum;
    // setup print rect for line
    fLineRect := fPrintable;
    // header
    PrintHeaderFooterLines(Printer.Canvas, FALSE);
    // text body
    First := integer(fPageStartLineNums[PageNum - 1]);
    if PageNum < fPageStartLineNums.Count then
      Last := integer(fPageStartLineNums[PageNum]) - 1
    else
      Last := fPrintRangeEnd.y - 1;
    PrintTextLines(Printer.Canvas, First, Last);
    // footer
    PrintHeaderFooterLines(Printer.Canvas, TRUE);
  end;
end;

procedure TSynPrintout.PrintRange(FirstPage, LastPage: integer);
var
  PagePrinted: boolean;
begin
  EnsurePagination;
  FirstPage := Max(1, FirstPage);
  LastPage := Min(LastPage, fPageStartLineNums.Count);
  if (FirstPage <= LastPage) and not DoCancelPrint(psBegin, 0) then begin
    fETOWidths := GetIntArray(Max(256, fCharsPerTextLine), fTextFontSize.x);
    try
      // set properties of print job
      Printer.Copies := Max(1, fNumCopies);
      Printer.Title := fTitle;
      Printer.BeginDoc;
      fDefaultBG := Printer.Canvas.Brush.Color;
      // print the lines in the range
      PagePrinted := FALSE;
      while TRUE do begin
        if DoCancelPrint(psNewPage, FirstPage) then
          break;
        try
          PagePrinted := ShouldPrint(FirstPage);
          if PagePrinted then
            PrintPage(FirstPage);
        except
          break;
        end;
        Inc(FirstPage);
        if FirstPage > LastPage then
          break;
        // end of page, but there's more to come...
        if PagePrinted then
          Printer.NewPage;
      end;
      // set the event handler parameter to -1 when the printout was cancelled
      if FirstPage <= LastPage then begin
        Printer.Abort;
        DoCancelPrint(psEnd, -1);
      end else begin
        Printer.EndDoc;
        DoCancelPrint(psEnd, 0);
      end;
      Printer.Canvas.Brush.Color := fDefaultBG;
    finally
      FreeMem(fETOWidths);
      fETOWidths := nil;
    end;
  end;
end;

procedure TSynPrintout.PrintTextLines(ACanvas: TCanvas; First, Last: integer);
var
  i, XStartOffset, XOffset, XEnd: integer;
  TokenRect: TRect;
  Line, Token: string;
  CharsDone, TokenLen, Wrap: integer;
  Attr: TSynHighlighterAttributes;
  AColor: TColor;
  WrapLen: integer;                                                             //mh 2000-10-31
begin
  ACanvas.Font.Assign(fTextFont);
  ACanvas.Brush.Color := fDefaultBG;
  SetTextAlign(ACanvas.Handle, TA_LEFT or TA_TOP);
  // print the lines
  for i := First to Last do begin
    fLineRect.Bottom := fLineRect.Top + fTextFontSize.y;
    TokenRect := fLineRect;
    XStartOffset := fLineRect.Left;
    // print line number?
    if fPrintLineNumbers then begin
      Token := Format(fLineNumberFmt, [i + 1]);
      if Token <> '' then begin
        // select the text body font into the (printer) canvas
        ACanvas.Font.Assign(fTextFont);
        ACanvas.Brush.Color := fDefaultBG;
        // print the line number
        TokenLen := Length(Token);
        XEnd := XStartOffset + fTextFontSize.x * TokenLen;
        TokenRect.Left := XStartOffset;
        TokenRect.Right := XEnd;
        ExtTextOut(ACanvas, XStartOffset, TokenRect, Token, TokenLen);
        XStartOffset := XEnd;
      end;
    end;
    // if fWordWrap is True we have to loop until the whole line is printed
    Line := TrimRight(fLines[i]);
    if Assigned(fHighlighter) then begin
      // set range and line text to highlighter
      fHighlighter.SetRange(fLines.Objects[i]);
      fHighlighter.SetLine(Line, i + 1);
    end;
    // always start after the line number text (with empty space instead
    // for the word-wrapped lines)
    XOffset := XStartOffset;
    CharsDone := 0;
    // print the highlighter tokens
    if Assigned(fHighlighter) then begin
      // loop through all the tokens of the line
      while not fHighlighter.GetEol do begin
        // get information about the token
        Token := fHighlighter.GetToken;
        TokenLen := Length(Token);
        // prepare font style and colors
        Attr := fHighlighter.GetTokenAttribute;
        if Assigned(Attr) then begin
          // setup the (printer) canvas with the token attributes
          ACanvas.Font.Style := Attr.Style;
          if not fBlackAndWhite then begin
            AColor := Attr.Foreground;
            if AColor = clNone then
              AColor := fTextFont.Color;
            ACanvas.Font.Color := AColor;
            AColor := Attr.Background;
            if AColor = clNone then
              AColor := fDefaultBG;
            ACanvas.Brush.Color := AColor;
          end;
        end else begin
          // select the text body font into the (printer) canvas
          ACanvas.Font.Assign(fTextFont);
          ACanvas.Brush.Color := fDefaultBG;
        end;
        // print the token, do word-wrap if necessary
        while fWrapLongLines and (CharsDone + TokenLen > fCharsPerTextLine) do
        begin
          // word-wrap the token and output the first part (but only if the
          // token could be wrapped)
          Wrap := WrapIndex(Token, 1, fCharsPerTextLine - CharsDone, FALSE);
          // if it is the only token on the line and still too long we will
          // force a word-wrap anyway
          if (Wrap = 0) and (CharsDone = 0) then                                //mh 2000-11-25
            Wrap := WrapIndex(Token, 1, fCharsPerTextLine - CharsDone, TRUE);   //mh 2000-11-25
          if (Wrap > 0) and (Wrap < TokenLen) then begin
            // output the first token part, then remove this part from Token
            TokenRect.Left := XOffset;
            TokenRect.Right := Min(fLineRect.Right,
              XOffset + Wrap * fTextFontSize.x);
            ExtTextOut(ACanvas, XOffset, TokenRect, Token, Wrap);
            Token := TrimLeft(Copy(Token, Wrap + 1, TokenLen));
            TokenLen := Length(Token);
          end;
          // start with next line
          CharsDone := 0;
          XOffset := XStartOffset;
          fLineRect.Top := fLineRect.Bottom;
          Inc(fLineRect.Bottom, fTextFontSize.y);
          TokenRect := fLineRect;
        end;
        TokenRect.Left := XOffset;
        TokenRect.Right := Min(fLineRect.Right,
          XOffset + TokenLen * fTextFontSize.x);
        ExtTextOut(ACanvas, XOffset, TokenRect, Token, TokenLen);
        Inc(CharsDone, TokenLen);
        // get out if no word-wrap but past end of printable area
        if not fWrapLongLines and (CharsDone >= fCharsPerTextLine) then
          break;
        XOffset := TokenRect.Right;
        // next token
        fHighlighter.Next;
      end;
    end else begin
      // select the text body font into the (printer) canvas
      ACanvas.Font.Assign(fTextFont);
      ACanvas.Brush.Color := fDefaultBG;
      // print the line
      TokenRect.Left := XOffset;
      TokenRect.Right := fLineRect.Right;
{begin}                                                                         //mh 2000-10-31
      while fWrapLongLines and (Length(Line) > fCharsPerTextLine) do begin
        WrapLen := WrapIndex(Line, 1, fCharsPerTextLine, TRUE);
        while (WrapLen <= Length(Line)) and (Line[WrapLen] <= ' ') do
          Inc(WrapLen);
        ExtTextOut(ACanvas, XOffset, TokenRect, Copy(Line, 1, WrapLen),
          WrapLen);
        Delete(Line, 1, WrapLen);
        fLineRect.Top := fLineRect.Bottom;
        Inc(fLineRect.Bottom, fTextFontSize.y);
        TokenRect := fLineRect;
      end;
{end}                                                                           //mh 2000-10-31
      ExtTextOut(ACanvas, XOffset, TokenRect, Line, Length(Line));
    end;
    fLineRect.Top := fLineRect.Bottom;
  end;
end;

procedure TSynPrintout.SetFooterLines(Value: TStrings);
begin
  fFooterLines.Assign(Value);
  NeedsPagination;
end;

procedure TSynPrintout.SetHeaderLines(Value: TStrings);
begin
  fHeaderLines.Assign(Value);
  NeedsPagination;
end;

procedure TSynPrintout.SetHFTextFont(Value: TFont);
begin
  if Assigned(Value) then
    fHFTextFont.Assign(Value)
  else with fHFTextFont do begin
    Name := 'Courier New';
    Size := 8;
    Color := clBlack;
    Style := [];
  end;
end;

procedure TSynPrintout.SetHighlighter(Value: TSynCustomHighlighter);
begin
  if Value <> fHighlighter then begin
    fHighlighterRangesOK := FALSE;
    fHighlighter := Value;
    NeedsPagination;
  end;
end;

procedure TSynPrintout.SetLines(Value: TStrings);
begin
  // Note: Assign does not copy the Objects property
  fHighlighterRangesOK := FALSE;
  fLines.Assign(Value);
  // this will scan the ranges if necessary
  NeedsPagination;
end;

{begin}                                                                         //mh 2000-10-31
procedure TSynPrintout.SetMargin(Index: integer; Value: integer);
begin
  if Value < 0 then
    Value := 0;
  case Index of
    0: if Value <> fMargins.Left then begin
         fMargins.Left := Value;
         NeedsPagination;
       end;
    1: if Value <> fMargins.Top then begin
         fMargins.Top := Value;
         NeedsPagination;
       end;
    2: if Value <> fMargins.Right then begin
         fMargins.Right := Value;
         NeedsPagination;
       end;
    3: if Value <> fMargins.Bottom then begin
         fMargins.Bottom := Value;
         NeedsPagination;
       end;
  end;
end;

procedure TSynPrintout.SetMarginUnits(Value: TSynMarginUnits);
begin
  if Value <> fMarginUnits then begin
    fMarginUnits := Value;
    NeedsPagination;
  end;
end;
{end}                                                                           //mh 2000-10-31

procedure TSynPrintout.SetPrintDateTime(Value: TDateTime);
begin
  fPrintDateTime := Value;
  fPrintDate := DateToStr(Value);
  fPrintTime := TimeToStr(Value);
end;

procedure TSynPrintout.SetPrintRangeEnd(Value: TPoint);
begin
  if (Value.x <> fPrintRangeEnd.x) or (Value.y <> fPrintRangeEnd.y) then
  begin
    fPrintRangeEnd := Value;
    NeedsPagination;
  end;
end;

procedure TSynPrintout.SetPrintRangeStart(Value: TPoint);
begin
  if (Value.x <> fPrintRangeStart.x) or (Value.y <> fPrintRangeStart.y) then
  begin
    fPrintRangeStart := Value;
    NeedsPagination;
  end;
end;

procedure TSynPrintout.SetTextFont(Value: TFont);
begin
  if Assigned(Value) then
// TODO: limit to fixed width fonts
    fTextFont.Assign(Value)
  else with fTextFont do begin
    Name := 'Courier New';
    Size := 10;
    Color := clBlack;
    Style := [];
  end;
end;

procedure TSynPrintout.SetWrapLongLines(Value: boolean);
begin
  if fWrapLongLines <> Value then begin
    fWrapLongLines := Value;
    NeedsPagination;
  end;
end;

function TSynPrintout.ShouldPrint(APageNum: integer): boolean;
begin
  case fFilter of
    pfEven: Result := not Odd(APageNum);
    pfOdd: Result := Odd(APageNum);
  else
    Result := TRUE;
  end;
end;

function TSynPrintout.WrapIndex(const Line: string;
  Start, MaxLength: integer; ForceWrap: boolean): integer;
var
  PWrap: PChar;
  Len, Wrap: integer;
begin
  PWrap := @Line[Start];
  Len := 0;
  if ForceWrap then
    Wrap := MaxLength
  else
    Wrap := 0;
  // this implementation wraps at whitespace chars only, if there is none
  // it will just return MaxLength
  // The real thing would be to word-wrap with the highlighter information...
  while Len < MaxLength do begin
    if PWrap[0] = #0 then
      break;
    Inc(Len);
//    if (PWrap[0] > #32) and (PWrap[1] <= #32) then
    if PWrap[0] <= #32 then                                                     //mh 2000-10-31
      Wrap := Len;
    Inc(PWrap);
  end;
  Result := Wrap;
end;

function TSynPrintout.WrapLineCount(Line: string; MaxLength: integer): integer;
var
  Len, Start, Wrap: integer;
begin
  Result := 1;
  Len := Length(Line);
  Start := 1;
  while Len > MaxLength do begin
    Wrap := WrapIndex(Line, Start, MaxLength, TRUE);
    Dec(Len, Wrap);
    Inc(Start, Wrap);
    Inc(Result);
  end;
end;

end.

