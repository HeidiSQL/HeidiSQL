{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditPrintHeaderFooter.pas, released 2000-06-01.

The Initial Author of the Original Code is Morten J. Skovrup.
Portions written by Morten J. Skovrup are copyright 2000 Morten J. Skovrup.
Portions written by Michael Hieke are copyright 2000 Michael Hieke.
Unicode translation by Maël Hörz.
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

$Id: SynEditPrintHeaderFooter.pas,v 1.10.2.7 2008/09/23 14:02:08 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------
CONTENTS:
  Classes handling info about headers and footers.

  THeaderFooterItem:
    Class handling an item in a header or footer. An item has a text,Font,
    LineNumber and Alignment (i.e. two items can be on the same line but have
    different fonts). Used internally.

  THeaderFooter:
    Collection of THeaderFooterItem's
    Design-time properties:
      FrameTypes : Frame around header or footer - can be any combination of:
                   ftLine   : Line under header or line above footer
                   ftBox    : Box around header or footer
                   ftShaded : Filled box (without frame) around header or footer.
      ShadedColor : Fill color if ftShaded is in FrameTypes
      LineColor   : Color of line or box if ftLine or ftBox is in FrameTypes
      DefaultFont : Default font for THeaderFooterItem's. This can be used to
                    set the header/footer font once for all items.
      RomanNumbers : Print page numbers as Roman numbers.
      MirrorPosition : Mirror position of left/right aligned THeaderFooterItem's
                       Can be used when printing 2-sided.
    Run-time methods:
      function Add(Text: UnicodeString; Font: TFont;
                   Alignment: TAlignment;
                   LineNumber: Integer) : Integer;
        Add a THeaderFooterItem. If Font is nil or not specified then DefaultFont
        is used. Returned value is the index of the added item.
        The Text parameter can contain the following macros:
          $PAGECOUNT$  : Print total number of pages
          $PAGENUM$    : Print current page number
          $TITLE$      : Print the title
          $DATE$       : Print the date
          $TIME$       : Print the time
          $DATETIME$   : Print the date and then the time
          $TIMEDATE$   : Print the time and then the date
      procedure Delete(Index : Integer);
        Delete THeaderFooterItem with index Index.
      procedure Clear;
        Clear all THeaderFooterItems.
      function Count : Integer;
        Returns number of THeaderFooterItems.
      function Get(Index : Integer) : THeaderFooterItem;
        Returns THeaderFooterItem with Index.
      procedure SetPixPrInch(Value : Integer);
        Corrects the PixPerInch property of fonts. Used internally by
        TSynEditPrint.
      procedure InitPrint(ACanvas : TCanvas;NumPages : Integer; Title : UnicodeString;
                          Margins : TSynEditPrintMargins);
        Prepares the header or footer for printing. Used internally by
        TSynEditPrint.
      procedure Print(ACanvas : TCanvas; PageNum : Integer = 0);
        Prints the header or footer. Used internally by TSynEditPrint.

-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDITPRINTHEADERFOOTER}
unit SynEditPrintHeaderFooter;
{$ENDIF}
{$M+}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  Qt,
  QSynEditPrintTypes,
  QSynEditPrintMargins,
  QSynUnicode,
  QGraphics,
{$ELSE}
  Windows,
  SynEditPrintTypes,
  SynEditPrintMargins,
  SynUnicode,
  Graphics,
{$ENDIF}
  Classes,
  SysUtils;

type
  //An item in a header or footer. An item has a text,Font,LineNumber and
  //Alignment (i.e. two items can be on the same line but have different
  //fonts).
  THeaderFooterItem = class
  private
    FText: UnicodeString;
    FFont: TFont;
    FLineNumber: Integer;
    FAlignment: TAlignment;
        {Used to store the original Index when the item was added - the index
         might change when the list is sorted}
    FIndex: Integer;
    function GetAsString: UnicodeString;
    procedure SetAsString(const Value: UnicodeString);
    procedure SetFont(const Value: TFont);
  public
    constructor Create;
    destructor Destroy; override;
    function GetText(NumPages, PageNum: Integer; Roman: Boolean;
      Title, ATime, ADate: UnicodeString): UnicodeString;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  public
    property Alignment: TAlignment read FAlignment write FAlignment;
    property AsString: UnicodeString read GetAsString write SetAsString;
    property Font: TFont read FFont write SetFont;
    property LineNumber: Integer read FLineNumber write FLineNumber;
    property Text: UnicodeString read FText write FText;
  end;

  THeaderFooterType = (hftHeader, hftFooter);

  //Used internally to calculate line height and font-base-line for header and
  //footer
  TLineInfo = class
  public
    LineHeight: Integer;
    MaxBaseDist: Integer;
  end;

  //The header/footer class
  THeaderFooter = class(TPersistent)
  private
    FType: THeaderFooterType; // Indicates if header or footer
    FFrameTypes: TFrameTypes;
    FShadedColor: TColor;
    FLineColor: TColor;
    FItems: TList;
    FDefaultFont: TFont;
    FDate, FTime: UnicodeString;
    FNumPages: Integer;
    FTitle: UnicodeString;
    FMargins: TSynEditPrintMargins;
    FFrameHeight: Integer;
    FOldPen: TPen;
    FOldBrush: TBrush;
    FOldFont: TFont;
    FRomanNumbers: Boolean;
    FLineInfo: TList;
    FLineCount: Integer;
    FMirrorPosition: Boolean;
    procedure SetDefaultFont(const Value: TFont);
    procedure DrawFrame(ACanvas: TCanvas);
    procedure CalcHeight(ACanvas: TCanvas);
    procedure SaveFontPenBrush(ACanvas: TCanvas);
    procedure RestoreFontPenBrush(ACanvas: TCanvas);
    function GetAsString: UnicodeString;
    procedure SetAsString(const Value: UnicodeString);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Text: UnicodeString; Font: TFont; Alignment: TAlignment;
      LineNumber: Integer): Integer;
    procedure Delete(Index: Integer);
    procedure Clear;
    function Count: Integer;
    function Get(Index: Integer): THeaderFooterItem;
    procedure SetPixPrInch(Value: Integer);
    procedure InitPrint(ACanvas: TCanvas; NumPages: Integer; Title: UnicodeString;
      Margins: TSynEditPrintMargins);
    procedure Print(ACanvas: TCanvas; PageNum: Integer);
    procedure Assign(Source: TPersistent); override;
    procedure FixLines;
    property AsString: UnicodeString read GetAsString write SetAsString;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  published
    property FrameTypes: TFrameTypes read FFrameTypes write FFrameTypes
    default [ftLine];
    property ShadedColor: TColor read FShadedColor write FShadedColor
    default clSilver;
    property LineColor: TColor read FLineColor write FLineColor default clBlack;
    property DefaultFont: TFont read FDefaultFont write SetDefaultFont;
    property RomanNumbers: Boolean read FRomanNumbers write FRomanNumbers
    default False;
    property MirrorPosition: Boolean read FMirrorPosition write FMirrorPosition
    default False;
  end;

  //The header and footer - does nothing but set the value of FType in
  //THeaderFooter
  THeader = class(THeaderFooter)
  public
    constructor Create;
  end;

  TFooter = class(THeaderFooter)
  public
    constructor Create;
  end;

  {$IFNDEF SYN_COMPILER_3_UP}
  TFontCharSet = 0..255;
  {$ENDIF}

implementation

uses
{$IFDEF SYN_COMPILER_4_UP}
  Math,
{$ENDIF}
{$IFDEF SYN_CLX}
  QSynEditMiscProcs;
{$ELSE}
  SynEditMiscProcs;
{$ENDIF}

// Helper routine for AsString processing.
function GetFirstEl(var Value: UnicodeString; Delim: WideChar): UnicodeString;
var
  p: Integer;
begin
  p := Pos(Delim, Value);
  if p = 0 then
    p := Length(Value) + 1;
  Result := Copy(Value, 1, p - 1);
  Delete(Value, 1, p);
end;


{ THeaderFooterItem }

constructor THeaderFooterItem.Create;
begin
  inherited;
  FFont := TFont.Create;
end;

destructor THeaderFooterItem.Destroy;
begin
  inherited;
  FFont.Free;
end;

// Returns string representation of THeaderFooterItem to alleviate storing
// items into external storage (registry, ini file).
function THeaderFooterItem.GetAsString: UnicodeString;
begin
  Result :=
    EncodeString(FText) + '/' +
{$IFDEF SYN_COMPILER_3_UP}
{$IFDEF SYN_CLX}
    IntToStr(Ord(FFont.Charset)) + '/' +
{$ELSE}
    IntToStr(FFont.Charset) + '/' +
{$ENDIF}
{$ELSE}
    IntToStr(DEFAULT_CHARSET)+'/' +                             
{$ENDIF}
    IntToStr(FFont.Color) + '/' +
    IntToStr(FFont.Height) + '/' +
    EncodeString(FFont.Name) + '/' +
    IntToStr(Ord(FFont.Pitch)) + '/' +
    IntToStr(FFont.PixelsPerInch) + '/' +
    IntToStr(FFont.Size) + '/' +
    IntToStr(byte(FFont.Style)) + '/' +
    IntToStr(FLineNumber) + '/' +
    IntToStr(Ord(FAlignment));
end;


{ This is basically copied from original SynEditPrint.pas. Returns the
  header/footer text with macros expanded }
function THeaderFooterItem.GetText(NumPages, PageNum: Integer;
  Roman: Boolean; Title, ATime, ADate: UnicodeString): UnicodeString;
var
  Len, Start, Run: Integer;
  AStr: UnicodeString;

  procedure DoAppend(AText: UnicodeString);
  begin
    Result := Result + AText;
  end;
  procedure TryAppend(var First: Integer; After: Integer);
  begin
    if After > First then
    begin
      DoAppend(Copy(AStr, First, After - First));
      First := After;
    end;
  end;
  function TryExecuteMacro: Boolean;
  var
    Macro: UnicodeString;
  begin
    Result := True;
    Macro := SynWideUpperCase(Copy(FText, Start, Run - Start + 1));
    if Macro = '$PAGENUM$' then
    begin
      if Roman then
        DoAppend(IntToRoman(PageNum))
      else
        DoAppend(IntToStr(PageNum));
      Exit;
    end;
    if Macro = '$PAGECOUNT$' then
    begin
      if Roman then
        DoAppend(IntToRoman(NumPages))
      else
        DoAppend(IntToStr(NumPages));
      Exit;
    end;
    if Macro = '$TITLE$' then
    begin
      DoAppend(Title);
      Exit;
    end;
    if Macro = '$DATE$' then
    begin
      DoAppend(ADate);
      Exit;
    end;
    if Macro = '$TIME$' then
    begin
      DoAppend(ATime);
      Exit;
    end;
    if Macro = '$DATETIME$' then
    begin
      DoAppend(ADate + ' ' + ATime);
      Exit;
    end;
    if Macro = '$TIMEDATE$' then
    begin
      DoAppend(ATime + ' ' + ADate);
      Exit;
    end;
    Result := False;
  end;

begin
  Result := '';
  AStr := FText;
  if WideTrim(AStr) = '' then
    Exit;
  // parse the line
  Len := Length(AStr);
  if Len > 0 then
  begin
      // start with left-aligned text
    Start := 1;
    Run := 1;
    while Run <= Len do
    begin
          // test for embedded macro
      if AStr[Run] = '$' then
      begin
        TryAppend(Start, Run);
        Inc(Run);
          // search for next '$' which could mark the end of a macro
        while Run <= Len do begin
          if AStr[Run] = '$' then
          begin
            // if this is a macro execute it and skip the chars from output
            if TryExecuteMacro then
            begin
              Inc(Run); // also the '$'
              Start := Run;
              break;
            end
            else
            begin
                // this '$' might again be the start of a macro
              TryAppend(Start, Run);
              Inc(Run);                                                         //ek 2001-08-02
            end;
          end
          else
            Inc(Run);
        end;
      end
      else
        Inc(Run);
    end;
    TryAppend(Start, Run);
  end;
end;

procedure THeaderFooterItem.LoadFromStream(AStream: TStream);
var
  aCharset: TFontCharset;
  aColor: TColor;
  aHeight: Integer;
  aName: TFontName;
  aPitch: TFontPitch;
  aSize: Integer;
  aStyle: TFontStyles;
  Len, BufferSize: Integer;
  Buffer: Pointer;
begin
  with AStream do
  begin
    Read(Len, sizeof(Len));
    BufferSize := Len * sizeof(WideChar);
    GetMem(Buffer, BufferSize + sizeof(WideChar));
    try
      Read(Buffer^, BufferSize);
      PWideChar(Buffer)[BufferSize div sizeof(WideChar)] := #0;
      FText := PWideChar(Buffer);
    finally
      FreeMem(Buffer);
    end;
    Read(FLineNumber, sizeof(FLineNumber));
    // font
    Read(aCharset, sizeof(aCharset));
    Read(aColor, sizeof(aColor));
    Read(aHeight, sizeof(aHeight));
    Read(BufferSize, sizeof(BufferSize));
    GetMem(Buffer, BufferSize + 1);
    try
      Read(Buffer^, BufferSize);
      PAnsiChar(Buffer)[BufferSize div sizeof(AnsiChar)] := #0;
      aName := string(PAnsiChar(Buffer));
    finally
      FreeMem(Buffer);
    end;
    Read(aPitch, sizeof(aPitch));
    Read(aSize, sizeof(aSize));
    Read(aStyle, sizeof(aStyle));
    {$IFDEF SYN_COMPILER_3_UP}
    FFont.Charset := aCharset;
    {$ENDIF}
    FFont.Color := aColor;
    FFont.Height := aHeight;
    FFont.Name := aName;
    FFont.Pitch := aPitch;
    FFont.Size := aSize;
    FFont.Style := aStyle;
    Read(FAlignment, sizeof(FAlignment));
  end;
end;

procedure THeaderFooterItem.SaveToStream(AStream: TStream);
var
  aCharset: TFontCharset;
  aColor: TColor;
  aHeight: Integer;
  aName: TFontName;
  aPitch: TFontPitch;
  aSize: Integer;
  aStyle: TFontStyles;
  aLen: Integer;
begin
  with AStream do
  begin
    aLen := Length(FText);
    Write(aLen, sizeof(aLen));
    Write(PWideChar(FText)^, aLen * sizeof(WideChar));
    Write(FLineNumber, sizeof(FLineNumber));
    // font
    {$IFDEF SYN_COMPILER_3_UP}
    aCharset := FFont.Charset;
    {$ELSE}
    aCharset := DEFAULT_CHARSET;
    {$ENDIF}
    aColor   := FFont.Color;
    aHeight  := FFont.Height;
    aName    := FFont.Name;
    aPitch   := FFont.Pitch;
    aSize    := FFont.Size;
    aStyle   := FFont.Style;
    Write(aCharset, SizeOf(aCharset));
    Write(aColor, SizeOf(aColor));
    Write(aHeight, SizeOf(aHeight));
    aLen := Length(aName);
    Write(aLen, SizeOf(aLen));
    {$IFDEF SYN_COMPILER_2}           // In D2 TFontName is a ShortString
    Write(PAnsiChar(@aName[1])^, aLen);   // D2 cannot convert ShortStrings to PAnsiChar
    {$ELSE}
    Write(PAnsiChar(AnsiString(aName))^, aLen);
    {$ENDIF}
    Write(aPitch, SizeOf(aPitch));
    Write(aSize, SizeOf(aSize));
    Write(aStyle, SizeOf(aStyle));
    Write(FAlignment, SizeOf(FAlignment));
  end;
end;

procedure THeaderFooterItem.SetAsString(const Value: UnicodeString);
var
  s: UnicodeString;
  sty: TFontStyles;
begin
  s := Value;
  FText := DecodeString(GetFirstEl(s, '/'));
{$IFDEF SYN_COMPILER_3_UP}
{$IFDEF SYN_CLX}
  GetFirstEl(s, '/');
{$ELSE}
  FFont.Charset := StrToIntDef(GetFirstEl(s, '/'), 0);
{$ENDIF}
{$ELSE}
  GetFirstEl(s, '/');
{$ENDIF}
  FFont.Color := StrToIntDef(GetFirstEl(s, '/'), 0);
  FFont.Height := StrToIntDef(GetFirstEl(s, '/'), 0);
  FFont.Name := DecodeString(GetFirstEl(s, '/'));
  FFont.Pitch := TFontPitch(StrToIntDef(GetFirstEl(s, '/'), 0));
  FFont.PixelsPerInch := StrToIntDef(GetFirstEl(s, '/'), 0);
  FFont.Size := StrToIntDef(GetFirstEl(s, '/'), 0);
  byte(sty) := StrToIntDef(GetFirstEl(s, '/'), 0);
  FFont.Style := sty;
  FLineNumber := StrToIntDef(GetFirstEl(s, '/'), 0);
  FAlignment := TAlignment(StrToIntDef(GetFirstEl(s, '/'), 0));
end;

procedure THeaderFooterItem.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

{ THeaderFooter }

constructor THeaderFooter.Create;
begin
  inherited;
  FFrameTypes := [ftLine];
  FShadedColor := clSilver;
  FLineColor := clBlack;
  FItems := TList.Create;
  FDefaultFont := TFont.Create;
  FOldPen := TPen.Create;
  FOldBrush := TBrush.Create;
  FOldFont := TFont.Create;
  FRomanNumbers := False;
  FMirrorPosition := False;
  FLineInfo := TList.Create;
  with FDefaultFont do
  begin
    Name := 'Arial';
    Size := 10;
    Color := clBlack;
  end;
end;

destructor THeaderFooter.Destroy;
var
  i: Integer;
begin
  Clear;
  FItems.Free;
  FDefaultFont.Free;
  FOldPen.Free;
  FOldBrush.Free;
  FOldFont.Free;
  for i := 0 to FLineInfo.Count - 1 do
    TLineInfo(FLineInfo[i]).Free;
  FLineInfo.Free;
  inherited;
end;

function THeaderFooter.Add(Text: UnicodeString; Font: TFont;
  Alignment: TAlignment; LineNumber: Integer): Integer;
var
  AItem: THeaderFooterItem;
begin
  AItem := THeaderFooterItem.Create;
  if Font = nil then
    AItem.Font := FDefaultFont
  else
    AItem.Font := Font;
  AItem.Alignment := Alignment;
  AItem.LineNumber := LineNumber;
  AItem.FIndex := FItems.Add(AItem);
  AItem.Text := Text;
  Result := AItem.FIndex;
end;

procedure THeaderFooter.Delete(Index: Integer);
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
  begin
    if THeaderFooterItem(FItems[i]).FIndex = Index then
    begin
      FItems.Delete(i);
      Break;
    end;
  end;
end;

procedure THeaderFooter.Clear;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    THeaderFooterItem(FItems[i]).Free;
  FItems.Clear;
end;

procedure THeaderFooter.SetDefaultFont(const Value: TFont);
begin
  FDefaultFont.Assign(Value);
end;

{ Counts number of lines in header/footer and changes the line-number so they
  start with 1 (the user might add header/footer items starting at line 2) }
procedure THeaderFooter.FixLines;
var
  i, CurLine: Integer;
  LineInfo: TLineInfo;
begin
  for i := 0 to FLineInfo.Count - 1 do
    TLineInfo(FLineInfo[i]).Free;
  FLineInfo.Clear;
  CurLine := 0;
  FLineCount := 0;
  for i := 0 to FItems.Count - 1 do
  begin
    if THeaderFooterItem(FItems[i]).LineNumber <> CurLine then
    begin
      CurLine := THeaderFooterItem(FItems[i]).LineNumber;
      FLineCount := FLineCount + 1;
      LineInfo := TLineInfo.Create;
      FLineInfo.Add(LineInfo);
    end;
    THeaderFooterItem(FItems[i]).LineNumber := FLineCount;
  end;
end;

{ Calculates the hight of the header/footer, finds the line height for each line
  and calculates the font baseline where text is to be written }
procedure THeaderFooter.CalcHeight(ACanvas: TCanvas);
var
  i, CurLine: Integer;
  AItem: THeaderFooterItem;
  FOrgHeight: Integer;
{$IFNDEF SYN_CLX}
  TextMetric: TTextMetric;
{$ENDIF}
begin
  FFrameHeight := -1;
  if FItems.Count <= 0 then Exit;

  CurLine := 1;
  FFrameHeight := 0;
  FOrgHeight := FFrameHeight;
  for i := 0 to FItems.Count - 1 do
  begin
    AItem := THeaderFooterItem(FItems[i]);
    if AItem.LineNumber <> CurLine then
    begin
      CurLine := AItem.LineNumber;
      FOrgHeight := FFrameHeight;
    end;
    ACanvas.Font.Assign(AItem.Font);
  {$IFNDEF SYN_CLX}
    GetTextMetrics(ACanvas.Handle, TextMetric);
    with TLineInfo(FLineInfo[CurLine - 1]), TextMetric do
    begin
      LineHeight := Max(LineHeight, TextHeight(ACanvas, 'W'));
      MaxBaseDist := Max(MaxBaseDist, tmHeight - tmDescent);
    end;
  {$ENDIF}
    FFrameHeight := Max(FFrameHeight, FOrgHeight + TextHeight(ACanvas, 'W'));
  end;
  FFrameHeight := FFrameHeight + 2 * FMargins.PHFInternalMargin;
end;

function CompareItems(Item1, Item2: Pointer): Integer;
//Used to sort header/footer items
begin
  Result := THeaderFooterItem(Item1).LineNumber - THeaderFooterItem(Item2).LineNumber;
  if Result = 0 then
    Result := Integer(Item1) - Integer(Item2);
end;

procedure THeaderFooter.SetPixPrInch(Value: Integer);
var
  i, TmpSize: Integer;
  AFont: TFont;
begin
  for i := 0 to FItems.Count - 1 do
  begin
    AFont := THeaderFooterItem(FItems[i]).Font;
    TmpSize := AFont.Size;
    AFont.PixelsPerInch := Value;
    AFont.Size := TmpSize;
  end;
end;

procedure THeaderFooter.InitPrint(ACanvas: TCanvas; NumPages: Integer; Title: UnicodeString;
  Margins: TSynEditPrintMargins);
begin
  SaveFontPenBrush(ACanvas);
  FDate := DateToStr(Now);
  FTime := TimeToStr(Now);
  FNumPages := NumPages;
  FMargins := Margins;
  FTitle := Title;
  FItems.Sort(CompareItems);
  FixLines;
  CalcHeight(ACanvas);
  RestoreFontPenBrush(ACanvas);
end;

procedure THeaderFooter.SaveFontPenBrush(ACanvas: TCanvas);
begin
  FOldFont.Assign(ACanvas.Font);
  FOldPen.Assign(ACanvas.Pen);
  FOldBrush.Assign(ACanvas.Brush);
end;

procedure THeaderFooter.RestoreFontPenBrush(ACanvas: TCanvas);
begin
  ACanvas.Font.Assign(FOldFont);
  ACanvas.Pen.Assign(FOldPen);
  ACanvas.Brush.Assign(FOldBrush);
end;

procedure THeaderFooter.DrawFrame(ACanvas: TCanvas);
//Draws frame around header/footer
begin
  if (FrameTypes = []) then Exit;
  with ACanvas, FMargins do begin
    Pen.Color := LineColor;
    Brush.Color := ShadedColor;
    if ftShaded in FrameTypes then
      Brush.Style := bsSolid
    else
      Brush.Style := bsClear;
    if ftBox in FrameTypes then
      Pen.Style := psSolid
    else
      Pen.Style := psClear;
    if FrameTypes * [ftBox, ftShaded] <> [] then begin
      if FType = hftHeader then
        Rectangle(PLeft, PHeader - FFrameHeight, PRight, PHeader)
      else
        Rectangle(PLeft, PFooter, PRight, PFooter + FFrameHeight);
    end;
    if ftLine in FrameTypes then begin
      Pen.Style := psSolid;
      if FType = hftHeader then begin
        MoveTo(PLeft, PHeader);
        LineTo(PRight, PHeader);
      end
      else begin
        MoveTo(PLeft, PFooter);
        LineTo(PRight, PFooter);
      end
    end;
  end;
end;

procedure THeaderFooter.Print(ACanvas: TCanvas; PageNum: Integer);
var
  i, X, Y, CurLine: Integer;
  AStr: UnicodeString;
  AItem: THeaderFooterItem;
{$IFNDEF SYN_CLX}
  OldAlign: UINT;
{$ENDIF}
  TheAlignment: TAlignment;
begin
  if (FFrameHeight <= 0) then Exit; // No header/footer
  SaveFontPenBrush(ACanvas);
  DrawFrame(ACanvas);
  ACanvas.Brush.Style := bsClear;
  if FType = hftHeader then
    Y := FMargins.PHeader - FFrameHeight
  else
    Y := FMargins.PFooter;
  Y := Y + FMargins.PHFInternalMargin; // Add the specified internal margin

  CurLine := 1;
  for i := 0 to FItems.Count - 1 do
  begin
    AItem := THeaderFooterItem(FItems[i]);
    ACanvas.Font := AItem.Font;
    if AItem.LineNumber <> CurLine then
    begin
      Y := Y + TLineInfo(FLineInfo[CurLine - 1]).LineHeight;
      CurLine := AItem.LineNumber;
    end;
    AStr := AItem.GetText(FNumPages, PageNum, FRomanNumbers, FTitle, FTime, FDate);
      //Find the alignment of the header/footer item - check for MirrorPosition
    TheAlignment := AItem.Alignment;
    if MirrorPosition and ((PageNum mod 2) = 0) then
    begin
      case AItem.Alignment of
        taRightJustify: TheAlignment := taLeftJustify;
        taLeftJustify: TheAlignment := taRightJustify;
      end;
    end;
      //Find X-position of text
    with FMargins do begin
      X := PLeftHFTextIndent;
      case TheAlignment of
        taRightJustify: X := PRightHFTextIndent - TextWidth(ACanvas, AStr);
        taCenter: X := (PLeftHFTextIndent + PRightHFTextIndent - TextWidth(ACanvas, AStr)) div 2;
      end;
    end;
      {Aligning at base line - Fonts can have different size in headers and footers}
  {$IFNDEF SYN_CLX}
    OldAlign := SetTextAlign(ACanvas.Handle, TA_BASELINE);
    ExtTextOutW(ACanvas.Handle, X, Y + TLineInfo(FLineInfo[CurLine - 1]).MaxBaseDist,
      0, nil, PWideChar(AStr), Length(AStr), nil);
    SetTextAlign(ACanvas.Handle, OldAlign);
  {$ELSE}
    TextOut(ACanvas, X, Y + TLineInfo(FLineInfo[CurLine - 1]).MaxBaseDist, AStr);
  {$ENDIF}
  end;
  RestoreFontPenBrush(ACanvas);
end;

procedure THeaderFooter.Assign(Source: TPersistent);
var
  Src: THeaderFooter;
  i: Integer;
begin
  if (Source <> nil) and (Source is THeaderFooter) then begin
    Src := THeaderFooter(Source);
    Clear;
    FType := Src.FType;
    FFrameTypes := Src.FFrameTypes;
    FShadedColor := Src.FShadedColor;
    FLineColor := Src.FLineColor;
    for i := 0 to Src.FItems.Count - 1 do begin
      with THeaderFooterItem(Src.FItems[i]) do
        Add(Text, Font, Alignment, LineNumber);
    end;
    FDefaultFont.Assign(Src.FDefaultFont);
    FRomanNumbers := Src.FRomanNumbers;
    FMirrorPosition := Src.FMirrorPosition;
  end else
    inherited Assign(Source);
end;

function THeaderFooter.Count: Integer;
begin
  Result := FItems.Count;
end;

function THeaderFooter.Get(Index: Integer): THeaderFooterItem;
begin
  Result := THeaderFooterItem(FItems[Index]);
end;

function THeaderFooter.GetAsString: UnicodeString;
var
  i: integer;
begin
  FixLines;
  Result := '';
  for i := 0 to FItems.Count - 1 do begin
    if Result <> '' then Result := Result + '/';
    Result := Result + EncodeString(THeaderFooterItem(FItems[i]).AsString);
  end; //for
end;

procedure THeaderFooter.SetAsString(const Value: UnicodeString);
var
  item: THeaderFooterItem;
  s: UnicodeString;
begin
  Clear;
  item := THeaderFooterItem.Create;
  try
    s := Value;
    while s <> '' do
    begin
      item.AsString := DecodeString(GetFirstEl(s, '/'));
      Add(item.Text, item.Font, item.Alignment, item.LineNumber);
    end; 
  finally
    item.Free;
  end;
end;

procedure THeaderFooter.LoadFromStream(AStream: TStream);
var
  Num, i: Integer;
  aCharset: TFontCharset;
  aColor: TColor;
  aHeight: Integer;
  aName: TFontName;
  aPitch: TFontPitch;
  aSize: Integer;
  aStyle: TFontStyles;
  bufSize: Integer;
  buffer: PAnsiChar;
begin
  with AStream do begin
    // read header/footer properties first
    Read(FFrameTypes, SizeOf(FFrameTypes));
    Read(FShadedColor, SizeOf(FShadedColor));
    Read(FLineColor, SizeOf(FLineColor));
    Read(FRomanNumbers, SizeOf(FRomanNumbers));
    Read(FMirrorPosition, SizeOf(FMirrorPosition));
    // font
    Read(aCharset, SizeOf(aCharset));
    Read(aColor, SizeOf(aColor));
    Read(aHeight, SizeOf(aHeight));
    Read(bufSize, SizeOf(bufSize));
    GetMem(buffer, bufSize+1);
    try
      Read(buffer^, bufSize);
      buffer[bufSize] := #0;
      aName := string(buffer);
    finally
      FreeMem(buffer);
    end;
    Read(aPitch, SizeOf(aPitch));
    Read(aSize, SizeOf(aSize));
    Read(aStyle, SizeOf(aStyle));
    {$IFDEF SYN_COMPILER_3_UP}
    FDefaultFont.Charset := aCharset;
    {$ENDIF}
    FDefaultFont.Color   := aColor;
    FDefaultFont.Height  := aHeight;
    FDefaultFont.Name    := aName;
    FDefaultFont.Pitch   := aPitch;
    FDefaultFont.Size    := aSize;
    FDefaultFont.Style   := aStyle;
    // now read in the items
    Read(Num, SizeOf(Num));
    while Num > 0 do
    begin
      // load headerfooter items from stream
      i := Add('', nil, taLeftJustify, 1);
      Get(i).LoadFromStream(AStream);
      Dec(Num);
    end;
  end;
end;

procedure THeaderFooter.SaveToStream(AStream: TStream);
var
  i, Num: integer;
  aCharset: TFontCharset;
  aColor: TColor;
  aHeight: Integer;
  aName: TFontName;
  aPitch: TFontPitch;
  aSize: Integer;
  aStyle: TFontStyles;
  aLen : integer;
begin
  with AStream do begin
    // write the header/footer properties first
    Write(FFrameTypes, SizeOf(FFrameTypes));
    Write(FShadedColor, SizeOf(FShadedColor));
    Write(FLineColor, SizeOf(FLineColor));
    Write(FRomanNumbers, SizeOf(FRomanNumbers));
    Write(FMirrorPosition, SizeOf(FMirrorPosition));
    // font
    {$IFDEF SYN_COMPILER_3_UP}
    aCharset := FDefaultFont.Charset;
    {$ELSE}
    aCharSet := DEFAULT_CHARSET;
    {$ENDIF}
    aColor   := FDefaultFont.Color;
    aHeight  := FDefaultFont.Height;
    aName    := FDefaultFont.Name;
    aPitch   := FDefaultFont.Pitch;
    aSize    := FDefaultFont.Size;
    aStyle   := FDefaultFont.Style;
    Write(aCharset, SizeOf(aCharset));
    Write(aColor, SizeOf(aColor));
    Write(aHeight, SizeOf(aHeight));
    aLen := Length(aName);
    Write(aLen, SizeOf(aLen));
    {$IFDEF SYN_COMPILER_2}                    // In D2 TFontName is a ShortString
    Write(PAnsiChar(@aName[1])^, Length(aName));   // D2 cannot convert ShortStrings to PAnsiChar
    {$ELSE}
    Write(PAnsiChar(AnsiString(aName))^, Length(aName));
    {$ENDIF}
    Write(aPitch, SizeOf(aPitch));
    Write(aSize, SizeOf(aSize));
    Write(aStyle, SizeOf(aStyle));

    // now write the items
    Num := Count;
    Write(Num, SizeOf(Num));
    for i := 0 to Num - 1 do
      Get(i).SaveToStream(AStream);
  end;
end;

{ THeader }

constructor THeader.Create;
begin
  inherited;
  FType := hftHeader;
end;

{ TFooter }

constructor TFooter.Create;
begin
  inherited;
  FType := hftFooter;
end;

end.

