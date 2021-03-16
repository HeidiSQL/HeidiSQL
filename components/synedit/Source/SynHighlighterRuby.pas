{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterRuby.pas, released 2001-11-13.
The Initial Author of this file is Stefan Ascher.
Portions by Jan Verhoeven (http://jansfreeware.com/jfdelphi.htm)
"Heredoc" syntax highlighting implementation by Marko Njezic.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynHighlighterRuby.pas,v 1.10.2.9 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Ruby highlighter for SynEdit)
@author(Stefan Ascher <stievie2002@yahoo.com>)
@created(21 May 2001)
@lastmod(2001-11-13)
The SynHighlighterVisualLisp unit provides SynEdit with a Ruby highlighter.
}

unit SynHighlighterRuby;

{$I SynEdit.inc}

interface

uses
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSecondKey,
    tkSpace, tkString, tkSymbol, tkUnknown);

{$IFDEF SYN_HEREDOC}
  TRangeState = (rsUnknown, rsHeredoc, rsIndentedHeredoc);

  TRangePointer = packed record
    case Boolean of
      True: (Ptr: Pointer);
      False: (Range: Byte; Length: Byte; Checksum: Word);
    end;
{$ELSE}
  TRangeState = (rsUnknown);
{$ENDIF}

type
  TSynRubySyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
{$IFDEF SYN_HEREDOC}
    FHeredocLength: Byte;
    FHeredocChecksum: Word;
{$ENDIF}
    FTokenID: TtkTokenKind;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FSecondKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FCommentAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyWords: TUnicodeStrings;
    FSecondKeys: TUnicodeStrings;
    procedure BraceOpenProc;
    procedure PointCommaProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RoundOpenProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
{$IFDEF SYN_HEREDOC}
    procedure HeredocProc;
{$ENDIF}
    procedure SetSecondKeys(const Value: TUnicodeStrings);
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
    procedure NextProcedure;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function IsKeyword(const AKeyword: UnicodeString): boolean; override;
    function IsSecondKeyWord(aToken: UnicodeString): Boolean;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property SecondKeyAttri: TSynHighlighterAttributes read FSecondKeyAttri
      write FSecondKeyAttri;
    property SecondKeyWords: TUnicodeStrings read FSecondKeys write SetSecondKeys;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri
      write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
  end;

implementation

uses
  SynEditMiscProcs,
  SynEditStrConst;

const
  RubyKeysCount = 43;
  RubyKeys: array[1..RubyKeysCount] of UnicodeString = (
    'alias', 'attr', 'begin', 'break', 'case', 'class', 'def', 'do', 'else',
    'elsif', 'end', 'ensure', 'exit', 'extend', 'false', 'for', 'gets', 'if',
    'in', 'include', 'load', 'loop', 'module', 'next', 'nil', 'not', 'print',
    'private', 'public', 'puts', 'raise', 'redo', 'require', 'rescue', 'retry',
    'return', 'self', 'then', 'true', 'unless', 'when', 'while', 'yield');

function TSynRubySyn.IsKeyword(const AKeyword: UnicodeString): Boolean;
var
  First, Last, I, Compare: Integer;
  Token: UnicodeString;
begin
  First := 0;
  Last := FKeyWords.Count - 1;
  Result := False;
  Token := SynWideUpperCase(AKeyword);

  while First <= Last do
  begin
    I := (First + Last) shr 1;
    Compare := WideCompareStr(FKeyWords[I], Token);
    if Compare = 0 then
    begin
      Result := True;
      Break;
    end
    else if Compare < 0 then
      First := I + 1
    else
      Last := I - 1;
  end;
end; { IsKeyWord }

function TSynRubySyn.IsSecondKeyWord(aToken: UnicodeString): Boolean;
var
  First, Last, I, Compare: Integer;
  Token: UnicodeString;
begin
  First := 0;
  Last := FSecondKeys.Count - 1;
  Result := False;
  Token := SynWideUpperCase(aToken);
  while First <= Last do
  begin
    I := (First + Last) shr 1;
    Compare := WideCompareStr(FSecondKeys[i], Token);
    if Compare = 0 then
    begin
      Result := True;
      Break;
    end
    else if Compare < 0 then
      First := I + 1
    else
      Last := I - 1;
  end;
end; { IsSecondKeyWord }

constructor TSynRubySyn.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FKeyWords := TUnicodeStringList.Create;
  TUnicodeStringList(FKeyWords).Sorted := True;
  TUnicodeStringList(FKeyWords).Duplicates := dupIgnore;
  FSecondKeys := TUnicodeStringList.Create;
  TUnicodeStringList(FSecondKeys).Sorted := True;
  TUnicodeStringList(FSecondKeys).Duplicates := dupIgnore;
  if not (csDesigning in ComponentState) then
    for i := 1 to RubyKeysCount do
      FKeyWords.Add(RubyKeys[i]);

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clMaroon;
  AddAttribute(FCommentAttri);
  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Foreground := clBlue;
  AddAttribute(FKeyAttri);
  FSecondKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrSecondReservedWord, SYNS_FriendlyAttrSecondReservedWord);
  AddAttribute(FSecondKeyAttri);
  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clGreen;
  AddAttribute(FNumberAttri);
  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clPurple;
  AddAttribute(FStringAttri);
  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  FSymbolAttri.Foreground := clBlue;
  AddAttribute(FSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);

  FRange := rsUnknown;
  FDefaultFilter := SYNS_FilterRuby;
end; { Create }

destructor TSynRubySyn.Destroy;
begin
  FKeyWords.Free;
  FSecondKeys.Free;
  inherited Destroy;
end; { Destroy }

procedure TSynRubySyn.BraceOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynRubySyn.PointCommaProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynRubySyn.CRProc;
begin
  FTokenID := tkSpace;
  case FLine[Run + 1] of
    #10:
      Inc(Run, 2);
    else
      Inc(Run);
  end;
end;

procedure TSynRubySyn.IdentProc;
begin
  while IsIdentChar(FLine[Run]) do Inc(Run);
  if IsKeyWord(GetToken) then
  begin
    FTokenID := tkKey;
    Exit;
  end
  else FTokenID := tkIdentifier;
  if IsSecondKeyWord(GetToken) then
    FTokenID := tkSecondKey
  else
    FTokenID := tkIdentifier;
end;

procedure TSynRubySyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynRubySyn.LowerProc;
{$IFDEF SYN_HEREDOC}
var
  i, Len, SkipRun: Integer;
  IndentedHeredoc: Boolean;
  QuoteChar: WideChar;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  if FLine[Run + 1] = '<' then
  begin
    FTokenID := tkSymbol;

    SkipRun := 0;
    QuoteChar := #0;
    if (FLine[Run + 2] = '-') and (FLine[Run + 3] in
      [WideChar('"'), WideChar(''''), WideChar('`')]) then
    begin
      SkipRun := 2;
      QuoteChar := FLine[Run + 3];
    end
    else
    if (FLine[Run + 2] in [WideChar('-'), WideChar('"'), WideChar(''''), WideChar('`')]) then
    begin
      SkipRun := 1;
      if FLine[Run + 2] <> '-' then
        QuoteChar := FLine[Run + 2];
    end;
    IndentedHeredoc := (SkipRun > 0) and (FLine[Run + 2] = '-');

    if IsIdentChar(FLine[Run + SkipRun + 2]) then
    begin
      Inc(Run, 2);

      i := Run;
      while IsIdentChar(FLine[SkipRun + i]) do Inc(i);
      Len := i - Run;

      if Len > 255 then
      begin
        FTokenID := tkUnknown;
        Exit;
      end;

      if (QuoteChar <> #0) and (FLine[Run + SkipRun + Len] <> QuoteChar) then
      begin
        FTokenID := tkUnknown;
        Exit;
      end;

      if IndentedHeredoc then
        FRange := rsIndentedHeredoc
      else
        FRange := rsHeredoc;
      FHeredocLength := Len;
      FHeredocChecksum := CalcFCS(FLine[Run + SkipRun], Len);

      Inc(Run, SkipRun + Len);
      FTokenID := tkString;
    end
    else
      Inc(Run, 2);
  end
  else
{$ENDIF}
  begin
    Inc(Run);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynRubySyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynRubySyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', 'e', 'E':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(Run);
  FTokenID := tkNumber;
  while IsNumberChar do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then
          Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynRubySyn.RoundOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynRubySyn.SlashProc;
begin
  case FLine[Run] of
    '/':
      begin
        Inc(Run);
        FTokenID := tkSymbol;
      end;
    '*':
      begin
        Inc(Run);
        FTokenID := tkSymbol;
      end;
  else
    begin
      FTokenID := tkComment;
      while FLine[Run] <> #0 do
      begin
        case FLine[Run] of
          #10, #13:
            Break;
        end;
        Inc(Run);
      end;
    end;
  end;
end;

procedure TSynRubySyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynRubySyn.StringProc;
var
  QuoteChar: WideChar;
begin
// Ha, ha, Strings in Ruby (could be anything)!!!!

//There are three more ways to construct string literals: %q, %Q, and ``here
//documents.''
//
//%q and %Q start delimited single- and double-quoted strings.
//
//%q/general single-quoted string/ » general single-quoted string
//%Q!general double-quoted string! » general double-quoted string
//%Q{Seconds/day: #{24*60*60}}     » Seconds/day: 86400
//
//The character following the ``q'' or ``Q'' is the delimiter. If it is an
//opening bracket, brace, parenthesis, or less-than sign, the string is read
//until the matching close symbol is found. Otherwise the string is read until
//the next occurrence of the same delimiter.

  FTokenID := tkString;
  QuoteChar := FLine[Run];      // either " or '
  if (FLine[Run + 1] = QuoteChar) and (FLine[Run + 2] = QuoteChar)
    then Inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13:
        Break;
    end;
    Inc(Run);
  until FLine[Run] = QuoteChar;
  if FLine[Run] <> #0 then Inc(Run);
end;

procedure TSynRubySyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

{$IFDEF SYN_HEREDOC}
procedure TSynRubySyn.HeredocProc;

  procedure SkipToEOL;
  begin
    case FLine[Run] of
       #0: NullProc;
      #10: LFProc;
      #13: CRProc;
    else
      repeat
        Inc(Run);
      until IsLineEnd(Run);
    end;
  end;

var
  i : Integer;
begin
  if IsLineEnd(Run) and (FTokenPos = Run) then
  begin
    NextProcedure;
    Exit;
  end;
  FTokenID := tkString;

  if FRange = rsIndentedHeredoc then
    while FLine[Run] in [WideChar(#9), WideChar(#32)] do Inc(Run);

  if ((Run = 0) and (FRange = rsHeredoc)) or (FRange = rsIndentedHeredoc) then
  begin
    i := 0;

    while not IsLineEnd(FLine[Run + i]) do
    begin
      if i > FHeredocLength then
      begin
        SkipToEOL;
        Exit;
      end;
      Inc(i);
    end;

    if i <> FHeredocLength then
    begin
      SkipToEOL;
      Exit;
    end;

    if (CalcFCS(FLine[Run], i) = FHeredocChecksum) then
    begin
      FRange := rsUnknown;
      Run := Run + i;
      Exit;
    end;
  end;

  SkipToEOL;
end;
{$ENDIF}

procedure TSynRubySyn.Next;
begin
  FTokenPos := Run;
{$IFDEF SYN_HEREDOC}
  if FRange in [rsHeredoc, rsIndentedHeredoc] then
    HeredocProc
  else
{$ENDIF}
    NextProcedure;
  inherited;
end;

procedure TSynRubySyn.NextProcedure;
begin
  case fLine[Run] of
    '<': LowerProc;
    '#': SlashProc;
    '{': BraceOpenProc;
    ';': PointCommaProc;
    #13: CRProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    #10: LFProc;
    #0: NullProc;
    '0'..'9': NumberProc;
    '(': RoundOpenProc;
    '/': SlashProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    #34, #39: StringProc;
    else UnknownProc;
  end;
end;

function TSynRubySyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynRubySyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynRubySyn.GetRange: Pointer;
{$IFDEF SYN_HEREDOC}
var
  RangePointer: TRangePointer;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  RangePointer.Range := Ord(FRange);
  RangePointer.Length := 0;
  RangePointer.Checksum := 0;
  if FRange in [rsHeredoc, rsIndentedHeredoc] then
  begin
    RangePointer.Length := FHeredocLength;
    RangePointer.Checksum := FHeredocChecksum;
  end;
  Result := RangePointer.Ptr;
{$ELSE}
  Result := Pointer(FRange);
{$ENDIF}
end;

function TSynRubySyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynRubySyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkSecondKey: Result := FSecondKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynRubySyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynRubySyn.ResetRange;
begin
  FRange := rsUnknown;
{$IFDEF SYN_HEREDOC}
  FHeredocLength := 0;
  FHeredocChecksum := 0;
{$ENDIF}
end;

procedure TSynRubySyn.SetRange(Value: Pointer);
{$IFDEF SYN_HEREDOC}
var
  RangePointer: TRangePointer;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  RangePointer := TRangePointer(Value);
  FRange := TRangeState(RangePointer.Range);
  FHeredocLength := 0;
  FHeredocChecksum := 0;
  if FRange in [rsHeredoc, rsIndentedHeredoc] then
  begin
    FHeredocLength := RangePointer.Length;
    FHeredocChecksum := RangePointer.Checksum;
  end;
{$ELSE}
  FRange := TRangeState(Value);
{$ENDIF}
end;

procedure TSynRubySyn.SetSecondKeys(const Value: TUnicodeStrings);
var
  i: Integer;
begin
  if Value <> nil then
    begin
      Value.BeginUpdate;
      for i := 0 to Value.Count - 1 do
        Value[i] := SynWideUpperCase(Value[i]);
      Value.EndUpdate;
    end;
  FSecondKeys.Assign(Value);
  DefHighLightChange(nil);
end;

function TSynRubySyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterRuby;
end;

class function TSynRubySyn.GetLanguageName: string;
begin
  Result := SYNS_LangRuby;
end;

function TSynRubySyn.GetSampleSource: UnicodeString;
begin
  Result :=
    '# Factorial'+#13#10+
    'def fact(n)'+#13#10+
    '  if n == 0'+#13#10+
    '    1'+#13#10+
    '  else'+#13#10+
    '    n * fact(n-1)'+#13#10+
    '  end'+#13#10+
    'end'+#13#10+
    'print fact(ARGV[0].to_i), "\n"';
end;

class function TSynRubySyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangRuby;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynRubySyn);
{$ENDIF}
end.
