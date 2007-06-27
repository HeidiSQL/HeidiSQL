{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditPrint.pas, released 2000-06-01.

The Initial Author of the Original Code is Morten J. Skovrup.
Portions written by Morten J. Skovrup are copyright 2000 Morten J. Skovrup.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynEditPrint.pas,v 1.16 2002/02/11 18:41:09 harmeister Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
  Wrapping across page boundaries is not supported
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
CONTENTS:
  Print controller component.
    Allows setting margins, headers and footers.

  Design time properties:
    Header        : Class property to set properties for headers -
                    see CSynEditHeaderFooter.pas
    Footer        : Class property to set properties for footers -
                    see CSynEditHeaderFooter.pas
    Margins       : Class property to set properties for margins -
                    see CSynEditPrintMargins.pas
    Lines         : The lines that should be printed (see also SynEdit the
                    property below)
    Font          : The font the lines should be printed in (see also SynEdit
                    the property below)
    Title         : A title - can be referenced in headers/footers by using the
                    $TITLE$ macro
    Wrap          : Wrap text to margins
    Highlight     : Highlight text
    Colors        : Print in colors
    LineNumbers   : Print line numbers
    LineOffset    : Value added to linenumbers when printing
    PageOffset    : Value added to pagenumbers when printing
    OnPrintLine   : Fired when a line is printed
    OnPrintStatus : Fired at Beginning, End and when a new page is started
    Highlighter   : The highlighter used for highlighting the text (see also the
                    SynEdit property below)
    LineNumbersInMargin : If true line numbers are printed in the left margin,
                          else left margin is increased by width of line
                          number text.
    SelectedOnly  : Print only the selected Area
  Run-time properties:
    DocTitle    : Used to display the document name in the print queue monitor  //JJV 2000-10-13
    PrinterInfo : Read only. Returns info on printer (used internally)
    PageCount   : Returns the total number of pages;
    SynEdit     : By setting SynEdit to a specific TSynEdit component, the
                  properties Lines, Font and Highlighter are automatically
                  set to the corresponding values of the TSynEdit component
  Run-time methods:
    UpdatePages   : Used internally by the TSynEditPrintPreview component
    PrintToCanvas : Used internally by the TSynEditPrintPreview component
    Print         : Prints the contents of the Lines property
    PrintRange(StartPage,EndPage) : Prints the specified page-range (both inclusive)
-------------------------------------------------------------------------------}

unit SynEditPrint;

{$M+}
{$I SynEdit.inc}

interface

uses
  SysUtils,
  Classes,
{$IFDEF SYN_CLX}
  Qt,
  QGraphics,
  QPrinters,
  Types,
{$ELSE}
  Windows,
  Graphics,
  Printers,
{$ENDIF}
  SynEdit,
  SynEditTypes,
  SynEditPrintTypes,
  SynEditPrintHeaderFooter,
  SynEditPrinterInfo,
  SynEditPrintMargins,
  SynEditMiscProcs,
  SynEditHighlighter;

type
  TPageLine =
    class
  public
    FirstLine: Integer;
  end;
  //The actual print controller object
  TSynEditPrint =
    class(TComponent)
  private
    FCopies: integer;                                                           //EK 10/16/01
    FFooter: TFooter;
    FHeader: THeader;
    FLines: TStrings;
    FMargins: TSynEditPrintMargins;
    FPageCount: Integer;
    FFont: TFont;
    FTitle: string;
    FDocTitle: string;                                                          //JJV 2000-10-13
    FPrinterInfo: TSynEditPrinterInfo;
    FPages: TList;
    FCanvas: TCanvas;
    {************}
  {$IFNDEF SYN_CLX}
    FTextMetrics: TTextMetric;
  {$ENDIF}
    FCharWidth: Integer;
    FMaxLeftChar: Integer;
    FETODist: PIntArray;
    FWrap: Boolean;
    FOnPrintLine: TPrintLineEvent;
    FOnPrintStatus: TPrintStatusEvent;
    FYPos: Integer;
    FLineHeight: Integer;
    FHighlight: Boolean;
    FColors: Boolean;
    FHighlighter: TSynCustomHighlighter;
    FOldFont: TFont;
    FSynOK: Boolean;
    FLineNumbers: Boolean;
    FLineNumber: Integer;
    FLineOffset: Integer;
    FAbort: Boolean;
    FPrinting: Boolean;
    FDefaultBG: TColor;
    FPageOffset: Integer;
    FRangesOK: Boolean;
    FMaxWidth: integer;
    FMaxCol: Integer;
    FPagesCounted: Boolean;
    FLineNumbersInMargin: Boolean;
    FTabWidth: integer;
    fFontColor: TColor;                                                         // djlp 2000-09-20
    fSelectedOnly: Boolean;                                                     // jj 2001-07-23
    fSelAvail: Boolean;
    fSelMode: TSynSelectionMode;
    fBlockBegin: TPoint;
    fBlockEnd: TPoint;
    procedure CalcPages;
    procedure SetLines(const Value: TStrings);
    procedure SetFont(const Value: TFont);
    procedure SetCharWidth(const Value: Integer);
    procedure SetMaxLeftChar(const Value: Integer);
    procedure PrintPage(Num: Integer);
    procedure WriteLine(Text: string);
    procedure WriteLineNumber;
    procedure HandleWrap(Text: string; MaxWidth: Integer);
    procedure TextOut(Text: string; AList: TList);
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure RestoreCurrentFont;
    procedure SaveCurrentFont;
    procedure SetPixelsPrInch;
    procedure InitPrint;
    procedure InitRanges;
    function GetPageCount: Integer;
    procedure SetSynEdit(const Value: TSynEdit);
  protected
    property MaxLeftChar: Integer read FMaxLeftChar write SetMaxLeftChar;
    property CharWidth: Integer read FCharWidth write SetCharWidth;
    procedure PrintStatus(Status: TSynPrintStatus; PageNumber: integer;
      var Abort: boolean); virtual;
    procedure PrintLine(LineNumber, PageNumber: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdatePages(ACanvas: TCanvas);
    procedure PrintToCanvas(ACanvas: TCanvas; PageNumber: Integer);
    procedure Print;
    procedure PrintRange(StartPage, EndPage: Integer);
    property PrinterInfo: TSynEditPrinterInfo read FPrinterInfo;
    property PageCount: Integer read GetPageCount;
    property SynEdit: TSynEdit write SetSynEdit;

    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  published
    property Copies: integer read FCopies write FCopies;
    property Header: THeader read FHeader write FHeader;
    property Footer: TFooter read FFooter write FFooter;
    property Margins: TSynEditPrintMargins read FMargins write FMargins;
    property Lines: TStrings read FLines write SetLines;
    property Font: TFont read FFont write SetFont;
    property Title: string read FTitle write FTitle;
    property DocTitle: string read FDocTitle write FDocTitle;                   //JJV  2000-10-13
    property Wrap: Boolean read FWrap write FWrap default True;
    property Highlight: Boolean read FHighlight write FHighlight default True;
    property SelectedOnly: Boolean read FSelectedOnly write FSelectedOnly       // jj 2001-07-23
      default False;
    property Colors: Boolean read FColors write FColors default False;
    property LineNumbers: Boolean read FLineNumbers write FLineNumbers
      default False;
    property LineOffset: Integer read FLineOffset write FLineOffset default 0;
    property PageOffset: Integer read FPageOffset write FPageOffset default 0;
    property OnPrintLine: TPrintLineEvent read FOnPrintLine write FOnPrintLine;
    property OnPrintStatus: TPrintStatusEvent read FOnPrintStatus
      write FOnPrintStatus;
    property Highlighter: TSynCustomHighlighter read FHighlighter
      write SetHighlighter;
    property LineNumbersInMargin: Boolean read FLineNumbersInMargin
      write FLineNumbersInMargin default False;
    property TabWidth: integer read fTabWidth write fTabWidth;                  // djlp 2000-09-19
    property Color: TColor read fDefaultBG write fDefaultBG;                    // djlp 2000-09-19
  end;

implementation

{ TSynEditPrint }

constructor TSynEditPrint.Create(AOwner: TComponent);
begin
  inherited;
  FCopies := 1;
  FFooter := TFooter.Create;
  FHeader := THeader.Create;
  FLines := TStringList.Create;
  FMargins := TSynEditPrintMargins.Create;
  FPrinterInfo := TSynEditPrinterInfo.Create;
  FFont := TFont.Create;
  FOldFont := TFont.Create;
  FETODist := AllocMem(1);
  MaxLeftChar := 1024;
  FWrap := True;
  FHighlight := True;
  FColors := False;
  FLineNumbers := False;
  FLineOffset := 0;
  FPageOffset := 0;
  FLineNumbersInMargin := False;
  FPages := TList.Create;
  FTabWidth := 8;                                                     
  FDefaultBG := clWhite;                                                        // djlp 2000-09-19
end;

destructor TSynEditPrint.Destroy;
var
  i: Integer;
begin
  FFooter.Free;
  FHeader.Free;
  FLines.Free;
  FMargins.Free;
  FPrinterInfo.Free;
  FFont.Free;
  FOldFont.Free;
  for i := 0 to FPages.Count - 1 do
    TPageLine(FPages[i]).Free;
  FPages.Free;
  FreeMem(FETODist);
  inherited;
end;

procedure TSynEditPrint.SetLines(const Value: TStrings);
var
  i,j: integer;
  ConvertTabsProc: TConvertTabsProc;
  TmpString: String;
begin
  ConvertTabsProc := GetBestConvertTabsProc(FTabWidth);
  with FLines do begin
    BeginUpdate;
    try
      Clear;
      for i := 0 to Value.Count - 1 do
      begin
        TmpString := ConvertTabsProc(Value[i], FTabWidth);
        j := pos(TSynTabChar, TmpString);
        While j > 0 do
        begin
          TmpString[j] := ' ';
          j := pos(TSynTabChar, TmpString);
        end;
        Add(TmpString);
      end;
    finally
      EndUpdate;
    end;
  end;
  FRangesOK := False;
  FPagesCounted := False;
end;

procedure TSynEditPrint.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  FPagesCounted := False;
end;

procedure TSynEditPrint.SetCharWidth(const Value: Integer);
var
  i: Integer;
begin
  if FCharWidth <> Value then begin
    FCharWidth := Value;
      // Must have range checking turned off here!
    for i := 0 to FMaxLeftChar - 1 do
{$IFOPT R+}{$DEFINE SYN_RESET_RANGE_CHECK}{$R-}{$ENDIF}
      FETODist[i] := FCharWidth;
{$IFDEF SYN_RESET_RANGE_CHECK}{$R+}{$UNDEF SYN_RESET_RANGE_CHECK}{$ENDIF}
  end;
end;

procedure TSynEditPrint.SetMaxLeftChar(const Value: Integer);
var
  i: Integer;
begin
  if FMaxLeftChar <> Value then begin
    FMaxLeftChar := Value;
    ReallocMem(FETODist, FMaxLeftChar * SizeOf(Integer));
    for i := 0 to FMaxLeftChar - 1 do
{$IFOPT R+}{$DEFINE SYN_RESET_RANGE_CHECK}{$R-}{$ENDIF}
      FETODist[i] := FCharWidth;
{$IFDEF SYN_RESET_RANGE_CHECK}{$R+}{$UNDEF SYN_RESET_RANGE_CHECK}{$ENDIF}
  end;
end;

procedure TSynEditPrint.SetHighlighter(const Value: TSynCustomHighlighter);
begin
  FHighlighter := Value;
  FRangesOK := False;
  FPagesCounted := False;
end;

procedure TSynEditPrint.InitPrint;
{ Initialize Font.PixelsPerInch, Character widths, Margins, Total Page count,
  headers and footers}
var
  TmpSize: Integer;
begin
//  FDefaultBG := FCanvas.Brush.Color;                                          // djlp 2000-09-20
  fFontColor := FFont.Color;                                                    // djlp 2000-09-20
  FCanvas.Font.Assign(FFont);
  if not FPrinting then
  begin
    SetPixelsPrInch;
    TmpSize := FCanvas.Font.Size;
    FCanvas.Font.PixelsPerInch := FFont.PixelsPerInch;
    FCanvas.Font.Size := TmpSize;
  end;
  {************}
{$IFNDEF SYN_CLX}
  GetTextMetrics(FCanvas.Handle, FTextMetrics);
  CharWidth := FTextMetrics.tmAveCharWidth;
{$ENDIF}
  FMargins.InitPage(FCanvas, 1, FPrinterInfo, FLineNumbers, FLineNumbersInMargin,
    FLines.Count - 1 + FLineOffset);
  CalcPages;
  FHeader.InitPrint(FCanvas, FPageCount, FTitle, FMargins);
  FFooter.InitPrint(FCanvas, FPageCount, FTitle, FMargins);
  FSynOK := Highlight and Assigned(FHighLighter) and (FLines.Count > 0);
end;

procedure TSynEditPrint.SetPixelsPrInch;
var
  TmpSize: Integer;
begin
  FHeader.SetPixPrInch(FPrinterInfo.YPixPrInch);
  FFooter.SetPixPrInch(FPrinterInfo.YPixPrInch);
  //This should be necessary - else size would be changed...
  TmpSize := FFont.Size;
  FFont.PixelsPerInch := FPrinterInfo.YPixPrInch;
  FFont.Size := TmpSize;
end;

procedure TSynEditPrint.InitRanges;
//Initialize ranges in Highlighter
var
  i: Integer;
begin
  if (not FRangesOK) and Assigned(FHighlighter) and (Lines.Count > 0) then begin
    FHighlighter.ResetRange;
    FLines.Objects[0] := fHighlighter.GetRange;
    i := 1;
    while (i < Lines.Count) do begin
      FHighlighter.SetLine(FLines[i - 1], i - 1);
      FHighlighter.NextToEol;
      FLines.Objects[i] := FHighlighter.GetRange;
      Inc(i);
    end;
    FRangesOK := True;
  end;
end;

procedure TSynEditPrint.CalcPages;
{Calculates the total number of pages.}
var
  AStr, Text: string;
  StrWidth, i: Integer;
  j: Integer;
  AList: TList;
  YPos: Integer;
  PageLine: TPageLine;

  procedure CountWrapped;
  //Counts the number of lines a line is wrapped to
  var
    j: Integer;
  begin
    for j := 0 to AList.Count - 1 do
      YPos := YPos + FLineHeight;
  end;

begin
  InitRanges;
  for i := 0 to FPages.Count - 1 do
    TPageLine(FPages[i]).Free;
  FPages.Clear;
  FMaxWidth := FMargins.PRight - FMargins.PLeft;
  AStr := '';
  FMaxCol := 0;
  while FCanvas.TextWidth(AStr) < FMaxWidth do begin
    AStr := AStr + 'W';
    FMaxCol := FMaxCol + 1;
  end;
  FMaxCol := FMaxCol - 1;
  {FTestString is used to Calculate MaxWidth when prewiewing and printing -
   else the length is not calculated correctly when prewiewing and the
   zoom is different from 0.25,0.5,1,2,4 (as for example 1.20) - WHY???}
//  fTestString := StringOfChar('W', FMaxCol);
  AStr := StringOfChar('W', FMaxCol);
  FMaxWidth := FCanvas.TextWidth(AStr);
  {************}
{$IFNDEF SYN_CLX}
  FLineHeight := FTextMetrics.tmHeight + FTextMetrics.tmExternalLeading;
{$ENDIF}
  FPageCount := 1;
  PageLine := TPageLine.Create;
  PageLine.FirstLine := 0;
  FPages.Add(PageLine);
  YPos := FMargins.PTop;
  for i := 0 to Lines.Count - 1 do
  begin
    Text := Lines[i];
      {if new page then increase FPageCount and save the top-line number in
       FPages}
    if (YPos + FLineHeight > FMargins.PBottom) then
    begin
      YPos := FMargins.PTop;
      FPageCount := FPageCount + 1;
      PageLine := TPageLine.Create;
      PageLine.FirstLine := i;
      FPages.Add(PageLine);
    end;
    StrWidth := FCanvas.TextWidth(Text);
    {Check for wrap}
    if Wrap and (StrWidth > FMaxWidth) then begin                          
      AList := TList.Create;
      if WrapText(Text, [' ', '-', #9, ','], FMaxCol, AList) then
        CountWrapped
      else begin
              {If WrapTextToList didn't succed with the first set of breakchars
               then try this one:}
        if WrapText(Text, [';', ')', '.'], FMaxCol, AList) then
          CountWrapped
        else begin
                  {If WrapTextToList didn't succed at all, then do it the
                   primitive way}
          while Length(Text) > 0 do begin
            AStr := Copy(Text, 1, FMaxCol);
            Delete(Text, 1, FMaxCol);
            if Length(Text) > 0 then
              YPos := YPos + FLineHeight;
          end;
        end;
      end;
      for j := 0 to AList.Count - 1 do
        TWrapPos(AList[j]).Free;
      AList.Free;
    end;
    YPos := YPos + FLineHeight;
  end;
  FPagesCounted := True;
end;

procedure TSynEditPrint.WriteLineNumber;
{Writes the line number. FMargins. PLeft is the position of the left margin
 (which is automatically incremented by the length of the linenumber text, if
  the linenumbers should not be placed in the margin)}
var
  AStr: string;
begin
  SaveCurrentFont;
  AStr := IntToStr(FLineNumber + FLineOffset) + ': ';
  FCanvas.Font.Style := [];
  FCanvas.Font.Color := clBlack;
  FCanvas.TextOut(FMargins.PLeft - FCanvas.TextWidth(AStr), FYPos, AStr);
  RestoreCurrentFont;
end;

procedure TSynEditPrint.HandleWrap(Text: string; MaxWidth: Integer);
//Handles wrapping when printing
var
  AStr: string;
  AList: TList;
  j: Integer;
  procedure WrapPrimitive;
  var
    i: Integer;
    WrapPos: TWrapPos;
  begin
    i := 1;
    while i <= Length(Text) do begin
      AStr := '';
      while (Length(AStr) < FMaxCol) and (i <= Length(Text)) do begin
        AStr := AStr + Text[i];
        i := i + 1;
      end;
      WrapPos := TWrapPos.Create;
      WrapPos.Index := i - 1;
      AList.Add(WrapPos);
      if (Length(AStr) - i) <= FMaxCol then
        Break;
    end;
  end;
begin
  AStr := '';
  //First try to break the string at the following chars:
  AList := TList.Create;
  if WrapText(Text, [' ', '-', #9, ','], FMaxCol, AList) then
    TextOut(Text, AList)
  else begin
      //Then try to break the string at the following chars:
    if WrapText(Text, [';', ')', '.'], FMaxCol, AList) then
      TextOut(Text, AList)
    else begin
      WrapPrimitive;
      TextOut(Text, AList)
    end;
  end;
  for j := 0 to AList.Count - 1 do
    TWrapPos(Alist[j]).Free;
  AList.Free;
end;

procedure TSynEditPrint.SaveCurrentFont;
//Used to temporarely save the font of the canvas
begin
  FOldFont.Assign(FCanvas.Font);
end;

procedure TSynEditPrint.RestoreCurrentFont;
//Used to restore the font of the canvas
begin
  FCanvas.Font.Assign(FOldFont);
end;

procedure TSynEditPrint.TextOut(Text: string; AList: TList);
//Does the actual printing
var
  Token: string;
  TokenPos: Integer;
  Attr: TSynHighlighterAttributes;
  AColor: TColor;
  TokenStart: Integer;
  LCount: Integer;
  Handled: Boolean;
  aStr : String;

  procedure SplitToken;
  var
    AStr: string;
    Last: Integer;
    FirstPos: Integer;
    TokenEnd: Integer;
  begin
    Last := TokenPos;
    FirstPos := TokenPos;
    TokenEnd := TokenPos + Length(Token);
    while (LCount < AList.Count) and (TokenEnd > TWrapPos(AList[LCount]).Index) do begin
      AStr := Copy(Text, Last + 1, TWrapPos(AList[LCount]).Index - Last);       //DDH 10/16/01 added fix from Oliver Grahl
      Last := TWrapPos(AList[LCount]).Index;                                    //DDH 10/16/01 added fix from Oliver Grahl
      {************}
    {$IFNDEF SYN_CLX}
      ExtTextOut(FCanvas.Handle, FMargins.PLeft + FirstPos * FCharWidth, FYPos, 0, nil, PChar(AStr), Length(AStr), @FETODist[0]);
    {$ENDIF}
      FirstPos := 0;
      LCount := LCount + 1;
      FYPos := FYPos + FLineHeight;
    end;
    AStr := Copy(Text, Last + 1, TokenEnd - Last);                              //DDH 10/16/01 added fix from Oliver Grahl
    {************}
  {$IFNDEF SYN_CLX}
    ExtTextOut(FCanvas.Handle, FMargins.PLeft + FirstPos * FCharWidth, FYPos, 0, nil, PChar(AStr), Length(AStr), @FETODist[0]);
  {$ENDIF}
    //Ready for next token:
    TokenStart := TokenPos + Length(Token) - Length(AStr);
  end;
begin
  if FSynOK then begin
    SaveCurrentFont;
    FHighlighter.SetRange(FLines.Objects[FLineNumber - 1]);
    FHighlighter.SetLine(Text, FLineNumber);
    Token := '';
    TokenStart := 0;
    LCount := 0;
    while not FHighLighter.GetEol do begin
      Token := FHighLighter.GetToken;
      TokenPos := FHighLighter.GetTokenPos;
      Attr := FHighLighter.GetTokenAttribute;
      if Assigned(Attr) then begin
        FCanvas.Font.Style := Attr.Style;
        if FColors then begin
          AColor := Attr.Foreground;
          if AColor = clNone then
            AColor := FFont.Color;
          FCanvas.Font.Color := AColor;
          AColor := Attr.Background;
          if AColor = clNone then
            AColor := FDefaultBG;
          FCanvas.Brush.Color := AColor;
        end;
      end
      else begin
//        FCanvas.Font.Color := clBlack;                                        // djlp 2000-09-20
        FCanvas.Font.Color := fFontColor;                                       // djlp 2000-09-20
        FCanvas.Brush.Color := FDefaultBG;
      end;
      Handled := False;
      if (AList <> nil) then begin
        if (LCount < AList.Count) then begin
                  //Split between tokens:
          if (TokenPos >= TWrapPos(AList[LCount]).Index) then begin
            LCount := LCount + 1;
            TokenStart := TokenPos;
            FYPos := FYPos + FLineHeight;
          end
          else begin
                      //Split in the middle of a token:
            if (TokenPos + Length(Token) > TWrapPos(AList[LCount]).Index) then begin
              Handled := True;
              SplitToken;
            end;
          end;
        end;
      end;
      if not Handled then
        {************}
      {$IFNDEF SYN_CLX}
        ExtTextOut(FCanvas.Handle, FMargins.PLeft + (TokenPos - TokenStart) * FCharWidth, FYPos, 0, nil, PChar(Token), Length(Token), @FETODist[0]);
      {$ELSE}
        ;
        {$ENDIF}
      FHighLighter.Next;
    end;
    RestoreCurrentFont;
  end else
// same procedure at lines 408-413
    while Length(Text) > 0 do begin
      AStr := Copy(Text, 1, FMaxCol);
      FCanvas.TextOut(FMargins.PLeft, FYPos, aStr);
      if not Wrap then
        break;
      Delete(Text, 1, FMaxCol);
      if Length(Text) > 0 then
        FYPos := FYPos + FLineHeight;
    end;
end;

procedure TSynEditPrint.WriteLine(Text: string);
//Prints a line of text
var
  StrWidth: integer;
begin
  if FLineNumbers then WriteLineNumber;
  StrWidth := FCanvas.TextWidth(Text);
  {Note that MaxWidth is calculated, using FTestString found in CalcPages -
   else the length is not calculated correctly when prewiewing and the
   zoom is different from 0.25,0.5,1,2,4 (as for example 1.20) - WHY???
  }
  if Wrap and (StrWidth > FMaxWidth) then
    HandleWrap(Text, FMaxWidth)
  else
    TextOut(Text, nil);
  FYPos := FYPos + FLineHeight;
end;

procedure TSynEditPrint.PrintPage(Num: Integer);
//Prints a page
var
  i, iEnd: Integer;
begin
  PrintStatus(psNewPage, Num, FAbort);
  if not FAbort then begin
{begin}                                                                         // djlp 2000-09-20
    FCanvas.Brush.Color := Color;
    with FMargins do
      FCanvas.FillRect(Rect(PLeft, PTop, PRight, PBottom));
{end}                                                                           // djlp 2000-09-20
    FMargins.InitPage(FCanvas, Num, FPrinterInfo, FLineNumbers,
      FLineNumbersInMargin, FLines.Count - 1 + FLineOffset);
    FHeader.Print(FCanvas, Num + FPageOffset);
    if FPages.Count > 0 then begin
      FYPos := FMargins.PTop;
      if Num = FPageCount then
        iEnd := FLines.Count - 1
      else
        iEnd := TPageLine(FPages[Num]).FirstLine - 1;
      for i := TPageLine(FPages[Num - 1]).FirstLine to iEnd do begin
        FLineNumber := i + 1;
        if (not fSelectedOnly or ((i >= fBlockBegin.Y - 1) and (i <= fBlockEnd.Y - 1))) then begin
          if (not fSelectedOnly or (fSelMode = smLine)) then WriteLine(Lines[i])
            else WriteLine (Copy (Lines[i],fBlockBegin.X,fBlockEnd.X - fBlockBegin.X));
          PrintLine(i + 1, Num);
        end;
      end;
    end;
    FFooter.Print(FCanvas, Num + FPageOffset);
  end;
end;

procedure TSynEditPrint.UpdatePages(ACanvas: TCanvas);
//Update pages (called explicitly by preview component)
begin
  FCanvas := ACanvas;
  FPrinterInfo.UpdatePrinter;
  InitPrint;
end;

procedure TSynEditPrint.PrintToCanvas(ACanvas: TCanvas; PageNumber: Integer);
//Print to specified canvas. Used by preview component
begin
  FAbort := False;
  FPrinting := False;
  FCanvas := ACanvas;
  PrintPage(PageNumber);
end;

procedure TSynEditPrint.Print;
begin
  PrintRange(1, -1);
end;

procedure TSynEditPrint.PrintRange(StartPage, EndPage: Integer);
//Prints the pages in the specified range
var
  i, ii: Integer;
begin
  if fSelectedOnly and not fSelAvail then                                       // jj 2001-07-23
    exit;

  FPrinting := True;
  FAbort := False;
  // The next part sets the document title that is used by the printer queue.
  if FDocTitle <> '' then                                                       //JJV 2000-10-13
    Printer.Title := FDocTitle
  else
    Printer.Title := FTitle;
  Printer.BeginDoc;
  PrintStatus(psBegin, StartPage, FAbort);
  UpdatePages(Printer.Canvas);

  for ii:=1 to Copies do begin                                                  //EK 10/16/01
    i := StartPage;
    if EndPage < 0 then
      EndPage := FPageCount;
    while (i <= EndPage) and (not FAbort) do begin
      PrintPage(i);
      if ((i < EndPage) or (ii<Copies)) and not(FAbort) then                    //DDH 2001-10-25
        Printer.NewPage;
      i := i + 1;
    end;
  end;
  if not(FAbort) then                                                           //DDH 2001-10-25 
    PrintStatus(psEnd, EndPage, FAbort);
  Printer.EndDoc;
  FPrinting := False;
end;

procedure TSynEditPrint.PrintLine(LineNumber, PageNumber: Integer);
//Fires the OnPrintLine event
begin
  if Assigned(FOnPrintLine) then
    FOnPrintLine(Self, LineNumber, PageNumber);
end;

procedure TSynEditPrint.PrintStatus(Status: TSynPrintStatus;
  PageNumber: integer; var Abort: boolean);
//Fires the OnPrintStatus event
begin
  Abort := False;
  if Assigned(FOnPrintStatus) then
    FOnPrintStatus(Self, Status, PageNumber, Abort);
  if Abort then begin
    if FPrinting then
      Printer.Abort;
  end;
end;

function TSynEditPrint.GetPageCount: Integer;
{Returns total page count. If pages hasn't been counted before,
 then a UpdatePages is called with a temporary canvas}
var
  TmpCanvas: TCanvas;
  DC: HDC;
begin
  Result := 0;
  if FPagesCounted then
    Result := FPageCount
  else begin
    TmpCanvas := TCanvas.Create;
    try
      {************}
      {$IFNDEF SYN_CLX}
      DC := GetDC(0);
      try
        if DC <> 0 then
        begin
          TmpCanvas.Handle := DC;
          UpdatePages(TmpCanvas);
          TmpCanvas.Handle := 0;
          Result := FPageCount;
          FPagesCounted := True;
        end;
      finally
        ReleaseDC(0, DC);
      end;
      {$ENDIF}
    finally
      TmpCanvas.Free;
    end;
  end;
end;

procedure TSynEditPrint.SetSynEdit(const Value: TSynEdit);
begin
//  Lines := Value.Lines;                                                       // sb 2001-11-09
  HighLighter := Value.Highlighter;
  Font := Value.Font;
  FTabWidth := Value.TabWidth;
  Lines := Value.Lines;                                                         // sb 2001-11-09
  fSelAvail := Value.SelAvail;                                                  // jj 2001-07-23
  fBlockBegin := Value.BlockBegin;
  fBlockEnd := Value.BlockEnd;
  fSelMode := Value.SelectionMode;
end;

procedure TSynEditPrint.LoadFromStream(AStream: TStream);
var
  bufSize : integer;
  buffer : PChar;
begin
  FHeader.LoadFromStream(AStream);
  FFooter.LoadFromStream(AStream);
  FMargins.LoadFromStream(AStream);
  with AStream do begin
    Read(bufSize, SizeOf(bufSize));
    GetMem(buffer, bufSize+1);
    try
      Read(buffer^, bufSize);
      buffer[bufSize] := #0;
      FTitle := buffer;
    finally
      FreeMem(buffer);
    end;
    Read(bufSize, SizeOf(bufSize));
    GetMem(buffer, bufSize+1);
    try
      Read(buffer^, bufSize);
      buffer[bufSize] := #0;
      FDocTitle := buffer;
    finally
      FreeMem(buffer);
    end;
    Read(FWrap, SizeOf(FWrap));
    Read(FHighlight, SizeOf(FHighlight));
    Read(FColors, SizeOf(FColors));
    Read(FLineNumbers, SizeOf(FLineNumbers));
    Read(FLineOffset, SizeOf(FLineOffset));
    Read(FPageOffset, SizeOf(FPageOffset));
  end;
end;

procedure TSynEditPrint.SaveToStream(AStream: TStream);
var
  aLen : integer;
begin
  FHeader.SaveToStream(AStream);
  FFooter.SaveToStream(AStream);
  FMargins.SaveToStream(AStream);
  with AStream do begin
    aLen := Length(FTitle);
    Write(aLen, SizeOf(aLen));
    Write(PChar(FTitle)^, aLen);
    aLen := Length(FDocTitle);
    Write(aLen, SizeOf(aLen));
    Write(PChar(FDocTitle)^, aLen);
    Write(FWrap, SizeOf(FWrap));
    Write(FHighlight, SizeOf(FHighlight));
    Write(FColors, SizeOf(FColors));
    Write(FLineNumbers, SizeOf(FLineNumbers));
    Write(FLineOffset, SizeOf(FLineOffset));
    Write(FPageOffset, SizeOf(FPageOffset));
  end;
end;

end.

