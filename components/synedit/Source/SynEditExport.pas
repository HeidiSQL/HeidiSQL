{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditExport.pas, released 2000-04-16.

The Original Code is partly based on the mwExport.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Hieke.
Portions created by Michael Hieke are Copyright 2000 Michael Hieke.
Portions created by James D. Jacobson are Copyright 1999 Martin Waldenburg.
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

$Id: SynEditExport.pas,v 1.7 2002/04/08 08:38:14 plpolak Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{ Base class for exporting a programming language source file or part of it to
  a formatted output like HTML or RTF and copying this to the Windows clipboard
  or saving it to a file. }
unit SynEditExport;

{$I SynEdit.inc}

interface

uses
  Classes,
  SysUtils,
  SynEditHighlighter,
{$IFDEF SYN_KYLIX}
  Libc,  //js 07-04-2002 "," was missing here 
{$ENDIF}
{$IFDEF SYN_CLX}
  Qt,
  QGraphics,
  Types,
  QClipbrd
{$ELSE}
  Windows,
  Graphics,
  Clipbrd
{$ENDIF}
  ;

type
  PSynReplaceCharsArray = ^TSynReplaceCharsArray;
  { Array to hold the replacements strings for chars that are invalid for the
    output format, occurences of the chars that have a corresponding entry in
    this array are replaced with the string the entry points to.  Descendant
    classes have to fill it accordingly. }
  TSynReplaceCharsArray = array[char] of PChar;

  { Base exporter class, implements the buffering and the common functionality
    to track the changes of token attributes, to export to the clipboard or to
    save the output to a file. Descendant classes have to implement only the
    actual formatting of tokens. }
  TSynCustomExporter = class(TComponent)
  private
    fBuffer: TMemoryStream;
    fFirstAttribute: boolean;
    procedure AssignFont(Value: TFont);
    procedure SetExportAsText(Value: boolean);
    procedure SetFont(Value: TFont);
    procedure SetHighlighter(Value: TSynCustomHighlighter);
    procedure SetTitle(const Value: string);
  protected
    fBackgroundColor: TColor;
    fClipboardFormat: UINT;
    fDefaultFilter: string;
    fExportAsText: boolean;
    fFont: TFont;
    fHighlighter: TSynCustomHighlighter;
    fLastBG: TColor;
    fLastFG: TColor;
    fLastStyle: TFontStyles;
    fReplaceReserved: TSynReplaceCharsArray;
    fTitle: string;
    fUseBackground: boolean;
    { Adds a string to the output buffer. }
    procedure AddData(const AText: string);
    { Adds a string and a trailing newline to the output buffer. }
    procedure AddDataNewLine(const AText: string);
    { Adds a newline to the output buffer. }
    procedure AddNewLine;
    { Copies the data under this format to the clipboard. The clipboard has to
      be opened explicitly when more than one format is to be set. }
    procedure CopyToClipboardFormat(AFormat: UINT);
    { Has to be overridden in descendant classes to add the closing format
      strings to the output buffer.  The parameters can be used to track what
      changes are made for the next token. }
    procedure FormatAttributeDone(BackgroundChanged, ForegroundChanged: boolean;
      FontStylesChanged: TFontStyles); virtual; abstract;
    { Has to be overridden in descendant classes to add the opening format
      strings to the output buffer.  The parameters can be used to track what
      changes have been made in respect to the previous token. }
    procedure FormatAttributeInit(BackgroundChanged, ForegroundChanged: boolean;
      FontStylesChanged: TFontStyles); virtual; abstract;
    { Has to be overridden in descendant classes to add the closing format
      strings to the output buffer after the last token has been written. }
    procedure FormatAfterLastAttribute; virtual; abstract;
{begin}                                                                         //mh 2000-10-10
    { Has to be overridden in descendant classes to add the opening format
      strings to the output buffer when the first token is about to be written. }
    procedure FormatBeforeFirstAttribute(BackgroundChanged,
      ForegroundChanged: boolean; FontStylesChanged: TFontStyles);
      virtual; abstract;
{end}                                                                           //mh 2000-10-10
    { Has to be overridden in descendant classes to add the formatted text of
      the actual token text to the output buffer. }
    procedure FormatToken(Token: string); virtual;
    { Has to be overridden in descendant classes to add a newline in the output
      format to the output buffer. }
    procedure FormatNewLine; virtual; abstract;
    { Returns the size of the formatted text in the output buffer, to be used
      in the format header or footer. }
    function GetBufferSize: integer;
    { The clipboard format the exporter creates as native format. }
    function GetClipboardFormat: UINT; virtual;
    { Has to be overridden in descendant classes to return the correct output
      format footer. }
    function GetFooter: string; virtual; abstract;
    { Has to be overridden in descendant classes to return the name of the
      output format. }
    function GetFormatName: string; virtual;
    { Has to be overridden in descendant classes to return the correct output
      format header. }
    function GetHeader: string; virtual; abstract;
    { Inserts a data block at the given position into the output buffer.  Is
      used to insert the format header after the exporting, since some header
      data may be known only after the conversion is done. }
    procedure InsertData(APos: integer; const AText: string);
{$IFDEF SYN_MBCSSUPPORT}
    { Replaces multibyte chars with the equivalent in the output format. }
    function ReplaceMBCS(Char1, Char2: char): string; virtual;
{$ENDIF}
    { Returns a string that has all the invalid chars of the output format
      replaced with the entries in the replacement array. }
    function ReplaceReservedChars(AToken: string; var IsSpace: boolean): string;
    { Sets the token attribute of the next token to determine the changes
      of colors and font styles so the properties of the next token can be
      added to the output buffer. }
    procedure SetTokenAttribute(IsSpace: boolean;
      Attri: TSynHighlighterAttributes); virtual;
  public
    { Creates an instance of the exporter. }
    constructor Create(AOwner: TComponent); override;
    { Destroys an instance of the exporter. }
    destructor Destroy; override;
    { Clears the output buffer and any internal data that relates to the last
      exported text. }
    procedure Clear; virtual;
    { Copies the output buffer contents to the clipboard, as the native format
      or as text depending on the ExportAsText property. }
    procedure CopyToClipboard;
    { Exports everything in the strings parameter to the output buffer. }
    procedure ExportAll(ALines: TStrings);
    { Exports the given range of the strings parameter to the output buffer. }
    procedure ExportRange(ALines: TStrings; Start, Stop: TPoint);
    { Saves the contents of the output buffer to a file. }
    procedure SaveToFile(const AFileName: string);
    { Saves the contents of the output buffer to a stream. }
    procedure SaveToStream(AStream: TStream);
  public
    { Default background color for text that has no token attribute assigned or
      for token attributes that have the background set to default. }
    property Color: TColor read fBackgroundColor write fBackgroundColor;
    { Filter string for the output format for SaveAs file dialogs. }
    property DefaultFilter: string read fDefaultFilter write fDefaultFilter;
    property ExportAsText: boolean read fExportAsText write SetExportAsText;
    { The font to be used for the output format. The font color is used for text
      that has no token attribute assigned or for token attributes that have
      the background set to default. }
    property Font: TFont read fFont write SetFont;
    { The output format of the exporter. } 
    property FormatName: string read GetFormatName;
    { The highlighter to use for exporting. }
    property Highlighter: TSynCustomHighlighter
      read fHighlighter write SetHighlighter;
    { The title to embedd into the output header. }
    property Title: string read fTitle write SetTitle;
    { Use the token attribute background for the exporting. }
    property UseBackground: boolean read fUseBackground write fUseBackground;
  end;

implementation

uses
  SynEditMiscProcs, SynEditStrConst;

{ TSynCustomExporter }

constructor TSynCustomExporter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBuffer := TMemoryStream.Create;
  {*****************}
{$IFNDEF SYN_CLX}
  fClipboardFormat := CF_TEXT;
{$ENDIF}
  fFont := TFont.Create;
  fBackgroundColor := clWindow;
  AssignFont(nil);
  Clear;
  fTitle := SYNS_Untitled;
end;

destructor TSynCustomExporter.Destroy;
begin
  fFont.Free;
  fBuffer.Free;
  inherited Destroy;
end;

procedure TSynCustomExporter.AddData(const AText: string);
begin
  if AText <> '' then
    fBuffer.Write(AText[1], Length(AText));
end;

procedure TSynCustomExporter.AddDataNewLine(const AText: string);
begin
  AddData(AText);
  AddNewLine;
end;

procedure TSynCustomExporter.AddNewLine;
const
  NL: array[0..1] of char = #13#10;
begin
  fBuffer.Write(NL[0], Length(NL));
end;

procedure TSynCustomExporter.AssignFont(Value: TFont);
begin
  if Value <> nil then
    fFont.Assign(Value)
  else begin
    fFont.Name := 'Courier New';
    fFont.Size := 10;
    fFont.Color := clBlack;
    fFont.Style := [];
  end;
end;

procedure TSynCustomExporter.Clear;
begin
  fBuffer.Position := 0;
  // Size is ReadOnly in Delphi 2
  fBuffer.SetSize(0);
  fLastStyle := [];
  fLastBG := clWindow;
  fLastFG := clWindowText;
end;

procedure TSynCustomExporter.CopyToClipboard;
begin
  if fExportAsText then
  {$IFDEF SYN_CLX}
    CopyToClipboardFormat(0)
  {$ELSE}
    CopyToClipboardFormat(CF_TEXT)
  {$ENDIF}
  else
    CopyToClipboardFormat(GetClipboardFormat);
end;

procedure TSynCustomExporter.CopyToClipboardFormat(AFormat: UINT);
var
  hData: THandle;
  hDataSize: UINT;
  PtrData: PChar;
begin
{$IFNDEF SYN_CLX}
  hDataSize := GetBufferSize + 1;
  hData := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT or GMEM_SHARE, hDataSize);
  if hData <> 0 then try
    PtrData := GlobalLock(hData);
    if Assigned(PtrData) then begin
      try
        fBuffer.Position := 0;
        fBuffer.Read(PtrData^, hDataSize - 1); // trailing #0
      finally
        GlobalUnlock(hData);
      end;
      Clipboard.SetAsHandle(AFormat, hData);
    end else
      Abort;
  except
    GlobalFree(hData);
    OutOfMemoryError;
  end;
  {$ENDIF}
end;

procedure TSynCustomExporter.ExportAll(ALines: TStrings);
begin
  ExportRange(ALines, Point(1, 1), Point(MaxInt, MaxInt));
end;

procedure TSynCustomExporter.ExportRange(ALines: TStrings; Start, Stop: TPoint);
var
  i: integer;
  Line, Token: string;
  IsSpace: boolean;
  Attri: TSynHighlighterAttributes;
begin
  // abort if not all necessary conditions are met
  if not Assigned(ALines) or not Assigned(Highlighter) or (ALines.Count = 0)
    or (Start.Y > ALines.Count) or (Start.Y > Stop.Y)
  then
  {$IFDEF SYN_CLX}
    exit;
  {$ELSE}
    Abort;
  {$ENDIF}
  Stop.Y := Max(1, Min(Stop.Y, ALines.Count));
  Stop.X := Max(1, Min(Stop.X, Length(ALines[Stop.Y - 1]) + 1));
  Start.X := Max(1, Min(Start.X, Length(ALines[Start.Y - 1]) + 1));
  if (Start.Y = Stop.Y) and (Start.X >= Stop.X) then
  {$IFDEF SYN_CLX}
    exit;
  {$ELSE}
    Abort;
  {$ENDIF}
  // initialization
  fBuffer.Position := 0;
  // Size is ReadOnly in Delphi 2
  fBuffer.SetSize(Max($1000, (Stop.Y - Start.Y) * 128));
  Highlighter.ResetRange;
  // export all the lines into fBuffer
  fFirstAttribute := TRUE;
  for i := Start.Y to Stop.Y do begin
    Line := ALines[i - 1];
    // order is important, since Start.Y might be equal to Stop.Y
    if i = Stop.Y then
      Delete(Line, Stop.X, MaxInt);
    if (i = Start.Y) and (Start.X > 1) then
      Delete(Line, 1, Start.X - 1);
    // export the line
    Highlighter.SetLine(Line, i);
    while not Highlighter.GetEOL do begin
      Attri := Highlighter.GetTokenAttribute;
      Token := ReplaceReservedChars(Highlighter.GetToken, IsSpace);
      SetTokenAttribute(IsSpace, Attri);
      FormatToken(Token);
      Highlighter.Next;
    end;
    FormatNewLine;
  end;
  if not fFirstAttribute then
    FormatAfterLastAttribute;
  // insert header
  fBuffer.SetSize(fBuffer.Position);
  InsertData(0, GetHeader);
  // add footer
  AddData(GetFooter);
  // Size is ReadOnly in Delphi 2
  fBuffer.SetSize(fBuffer.Position);
end;

procedure TSynCustomExporter.FormatToken(Token: string);
begin
  AddData(Token);
end;

function TSynCustomExporter.GetBufferSize: integer;
begin
  Result := fBuffer.Size;
end;

function TSynCustomExporter.GetClipboardFormat: UINT;
begin
  Result := fClipboardFormat;
end;

function TSynCustomExporter.GetFormatName: string;
begin
  Result := '';
end;

procedure TSynCustomExporter.InsertData(APos: integer; const AText: string);
var
  Len, ToMove, SizeNeeded: integer;
  Dest: PChar;
begin
  Len := Length(AText);
  if Len > 0 then begin
    ToMove := fBuffer.Position;
    SizeNeeded := ToMove + Len;
    if fBuffer.Size < SizeNeeded then
      // Size is ReadOnly in Delphi 2
      fBuffer.SetSize((SizeNeeded + $1800) and not $FFF); // increment in pages
    Dest := fBuffer.Memory;
    Inc(Dest, Len);
    Move(fBuffer.Memory^, Dest^, ToMove);
    fBuffer.Position := 0;
    fBuffer.Write(AText[1], Len);
    fBuffer.Position := ToMove + Len;
  end;
end;

{$IFDEF SYN_MBCSSUPPORT}
function TSynCustomExporter.ReplaceMBCS(Char1, Char2: char): string;
begin
  SetLength(Result, 2);
  Result[1] := Char1;
  Result[2] := Char2;
end;
{$ENDIF}

function TSynCustomExporter.ReplaceReservedChars(AToken: string;
  var IsSpace: boolean): string;
var
  I, ISrc, IDest, SrcLen, DestLen: integer;
  Replace: string;
  c: char;                                                                      //mh 2000-10-10
begin
  IsSpace := TRUE;
  if AToken <> '' then begin
    SrcLen := Length(AToken);
    ISrc := 1;
    DestLen := SrcLen;
    IDest := 1;
    SetLength(Result, DestLen);
    while ISrc <= SrcLen do begin
      c := AToken[ISrc];
      IsSpace := IsSpace and (c = ' ');
      if fReplaceReserved[c] <> nil then begin
        Replace := StrPas(fReplaceReserved[c]);
        Inc(ISrc);
{$IFDEF SYN_MBCSSUPPORT}
//      end else if ByteType(AToken, ISrc) <> mbSingleByte then begin
      end else if (AToken[ISrc] in LeadBytes) and (AToken[ISrc + 1] <> #0) then //mh 2000-10-10
      begin
        Replace := ReplaceMBCS(AToken[ISrc], AToken[ISrc + 1]);
        Inc(ISrc, 2);
{$ENDIF}
      end else begin
        if IDest > DestLen then begin
          Inc(DestLen, 32);
          SetLength(Result, DestLen);
        end;
        Result[IDest] := c;
        Inc(ISrc);
        Inc(IDest);
        continue;
      end;
      if IDest + Length(Replace) - 1 > DestLen then begin
        Inc(DestLen, Max(32, IDest + Length(Replace) - DestLen));
        SetLength(Result, DestLen);
      end;
      for I := 1 to Length(Replace) do begin
        Result[IDest] := Replace[I];
        Inc(IDest);
      end;
    end;
    SetLength(Result, IDest - 1);
  end else
    Result := '';
end;

procedure TSynCustomExporter.SaveToFile(const AFileName: string);
begin
  fBuffer.Position := 0;
  fBuffer.SaveToFile(AFileName);
end;

procedure TSynCustomExporter.SaveToStream(AStream: TStream);
begin
  fBuffer.Position := 0;
  fBuffer.SaveToStream(AStream);
end;

procedure TSynCustomExporter.SetExportAsText(Value: boolean);
begin
  if fExportAsText <> Value then begin
    fExportAsText := Value;
    Clear;
  end;
end;

procedure TSynCustomExporter.SetFont(Value: TFont);
begin
  AssignFont(Value);
end;

procedure TSynCustomExporter.SetHighlighter(Value: TSynCustomHighlighter);
begin
  if fHighlighter <> Value then begin
    fHighlighter := Value;
    if fHighlighter <> nil then
      fHighlighter.FreeNotification(Self);
    Clear;
  end;
end;

procedure TSynCustomExporter.SetTitle(const Value: string);
begin
  if fTitle <> Value then begin
    if Value <> '' then
      fTitle := Value
    else
      fTitle := SYNS_Untitled;
  end;
end;

procedure TSynCustomExporter.SetTokenAttribute(IsSpace: boolean;
  Attri: TSynHighlighterAttributes);
var
  ChangedBG: boolean;
  ChangedFG: boolean;
  ChangedStyles: TFontStyles;

  function ValidatedColor(AColor, ADefColor: TColor): TColor;
  begin
    if AColor = clNone then
      Result := ADefColor
    else
      Result := AColor;
  end;

begin
  if fFirstAttribute then begin
    fFirstAttribute := FALSE;
    fLastBG := ValidatedColor(Attri.Background, fBackgroundColor);
    fLastFG := ValidatedColor(Attri.Foreground, fFont.Color);
    fLastStyle := Attri.Style;
{begin}                                                                         //mh 2000-10-10
    FormatBeforeFirstAttribute(UseBackground and (fLastBG <> fBackgroundColor),
      fLastFG <> fFont.Color, Attri.Style);
(*
    FormatAttributeInit(UseBackground and (fLastBG <> fBackgroundColor),
      fLastFG <> fFont.Color, Attri.Style);
*)
{end}                                                                           //mh 2000-10-10
  end else begin
    ChangedBG := UseBackground and
      (fLastBG <> ValidatedColor(Attri.Background, fBackgroundColor));
    ChangedFG := not IsSpace and
      (fLastFG <> ValidatedColor(Attri.Foreground, fFont.Color));
    // which font style bits are to reset?
    if not IsSpace then
      ChangedStyles := fLastStyle - Attri.Style
    else
      ChangedStyles := [];
    if ChangedBG or ChangedFG or (fLastStyle <> Attri.Style) then begin
      FormatAttributeDone(ChangedBG, ChangedFG, ChangedStyles);
      // which font style bits are to set?
      if not IsSpace then
        ChangedStyles := Attri.Style - fLastStyle
      else
        ChangedStyles := [];
      fLastBG := ValidatedColor(Attri.Background, fBackgroundColor);
      if not IsSpace then begin
        fLastFG := ValidatedColor(Attri.Foreground, fFont.Color);
        fLastStyle := Attri.Style;
      end;
      FormatAttributeInit(ChangedBG, ChangedFG, ChangedStyles);
    end;
  end;
end;

end.

