{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterM3.pas, released 2000-11-23.
Unicode translation by Maël Hörz.

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

$Id: SynHighlighterM3.pas,v 1.11.2.4 2005/11/27 22:22:45 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Modula-3 syntax highlighter for SynEdit)
@author(Martin Pley <synedit@pley.de>)
@created(January 2000, converted to SynEdit November 23, 2000)
@lastmod(2000-11-23)
The SynHighlighterM3 unit provides SynEdit with a Modula-3 (.m3) highlighter.
}

{$IFNDEF QSYNHIGHLIGHTERM3}
unit SynHighlighterM3;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
  QSynHighlighterHashEntries,
{$ELSE}
  Graphics,
  Registry,
  SynEditTypes,
  SynEditHighlighter,
  SynHighlighterHashEntries,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkPragma,
    tkReserved, tkSpace, tkString, tkSymbol, tkUnknown, tkSyntaxError);

  TTokenRange = (trNone, trComment, trPragma);

  TRangeState = packed record
    case Boolean of
      False: (p: Pointer);
      True: (TokenRange: Word; Level: Word);
    end;

  TSynM3Syn = class(TSynCustomHighLighter)
  private
    fRange: TRangeState;
    FTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fPragmaAttri: TSynHighlighterAttributes;
    fReservedAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fSyntaxErrorAttri: TSynHighlighterAttributes;
    fKeywords: TSynHashEntryList;
    procedure DoAddKeyword(AKeyword: WideString; AKind: integer);
    function HashKey(Str: PWideChar): integer;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure SymAsciiCharProc;
    procedure SymCommentHelpProc;
    procedure SymCRProc;
    procedure SymIdentProc;
    procedure SymLFProc;
    procedure SymNestedHelperProc(AOpenChar, ACloseChar: WideChar);
    procedure SymNullProc;
    procedure SymNumberProc;
    procedure SymPragmaProc;
    procedure SymPragmaHelpProc;
    procedure SymRoundOpenProc;
    procedure SymSpaceProc;
    procedure SymStringProc;
    procedure SymSymbolProc;
    procedure SymUnknownProc;
  protected
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: WideString; override;
{$IFDEF SYN_DEVELOPMENT_CHECKS}
  public
    property _Keywords: TSynHashEntryList read fKeywords;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property PragmaAttri: TSynHighlighterAttributes read fPragmaAttri
      write fPragmaAttri;
    property ReservedAttri: TSynHighlighterAttributes read fReservedAttri
      write fReservedAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property SyntaxErrorAttri: TSynHighlighterAttributes read fSyntaxErrorAttri
      write fSyntaxErrorAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  Keywords: WideString =
    'AS,AND,ANY,ARRAY,BEGIN,BITS,BRANDED,BY,CASE,CONST,DIV,DO,ELSE,ELSIF,END,' +
    'EVAL,EXCEPT,EXCEPTION,EXIT,EXPORTS,FINALLY,FOR,FROM,GENERIC,IF,IMPORT,' +
    'IN,INTERFACE,LOCK,LOOP,METHODS,MOD,MODULE,NOT,OBJECT,OF,OR,OVERRIDES,' +
    'PROCEDURE,RAISE,RAISES,READONLY,RECORD,REF,REPEAT,RETURN,REVEAL,ROOT,' +
    'SET,THEN,TO,TRY,TYPE,TYPECASE,UNSAFE,UNTIL,UNTRACED,VALUE,VAR,WHILE,WITH';

  ReservedWords: WideString =
    'ABS,ADDRESS,ADR,ADRSIZE,BITSIZE,BOOLEAN,BYTESIZE,CARDINAL,CEILING,CHAR,' +
    'DEC,DISPOSE,FALSE,FIRST,FLOAT,FLOOR,INC,INTEGER,ISTYPE,LAST,LONGFLOAT,' +
    'LONGREAL,LOOPHOLE,MAX,MIN,MUTEX,NARROW,NEW,NIL,NULL,NUMBER,ORD,REAL,' +
    'REFANY,ROUND,SUBARRAY,TEXT,TRUE,TRUNC,TYPECODE,VAL';

procedure TSynM3Syn.DoAddKeyword(AKeyword: WideString; AKind: integer);
var
  HashValue: integer;
begin
  HashValue := HashKey(PWideChar(AKeyword));
  fKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

function TSynM3Syn.HashKey(Str: PWideChar): Integer;

  function GetOrd: Integer;
  begin
    case Str^ of
      'a'..'z': Result := 1 + Ord(Str^) - Ord('a');
      'A'..'Z': Result := 1 + Ord(Str^) - Ord('A');
      '0'..'9': Result := 28 + Ord(Str^) - Ord('0');
      '_': Result := 27;
      else Result := 0;
    end
  end;

begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
{$IFOPT Q-}
    Result := 7 * Result + GetOrd;
{$ELSE}
    Result := (7 * Result + GetOrd) and $FFFFFF;
{$ENDIF}
    Inc(Str);
  end;
  Result := Result and $FF; // 255
  fStringLen := Str - fToIdent;
end;

function TSynM3Syn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
begin
  fToIdent := MayBe;
  Entry := fKeywords[HashKey(MayBe)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > fStringLen then
      break
    else if Entry.KeywordLen = fStringLen then
      if IsCurrentToken(Entry.Keyword) then
      begin
        Result := TtkTokenKind(Entry.Kind);
        exit;
      end;
    Entry := Entry.Next;
  end;
  Result := tkIdentifier;
end;

constructor TSynM3Syn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := True;

  fKeywords := TSynHashEntryList.Create;
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey, SYNS_FriendlyAttrKey);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fPragmaAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  fPragmaAttri.Style:= [fsBold];
  AddAttribute(fPragmaAttri);
  fReservedAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  AddAttribute(fReservedAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  fSyntaxErrorAttri := TSynHighlighterAttributes.Create(SYNS_AttrSyntaxError, SYNS_FriendlyAttrSyntaxError);
  fSyntaxErrorAttri.Foreground := clRed;
  AddAttribute(fSyntaxErrorAttri);
  SetAttributesOnChange(DefHighlightChange);

  EnumerateKeywords(Ord(tkKey), Keywords, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkReserved), ReservedWords, IsIdentChar, DoAddKeyword);
  fDefaultFilter := SYNS_FilterModula3;
end;

destructor TSynM3Syn.Destroy;
begin
  fKeywords.Free;
  inherited Destroy;
end;

procedure TSynM3Syn.SymAsciiCharProc;
begin
  fTokenID := tkString;
  Inc(Run);
  while not IsLineEnd(Run) do
  begin
    case fLine[Run] of
      '\': if fLine[Run + 1] = #39 then
             Inc(Run);
      #39: begin
             Inc(Run);
             if fLine[Run] <> #39 then
               break;
           end;
    end;
    Inc(Run);
  end;
end;

procedure TSynM3Syn.SymCommentHelpProc;
begin
  fTokenID := tkComment;
  SymNestedHelperProc('(', ')');
end;

procedure TSynM3Syn.SymCRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynM3Syn.SymIdentProc;
begin
  fTokenID := IdentKind(fLine + Run);
  Inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do
    Inc(Run);
end;

procedure TSynM3Syn.SymLFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynM3Syn.SymNestedHelperProc(AOpenChar, ACloseChar: WideChar);
begin
  case fLine[Run] of
     #0: SymNullProc;
    #10: SymLFProc;
    #13: SymCRProc;
  else
    repeat
      if fLine[Run]= AOpenChar then
      begin
        Inc(Run);
        if fLine[Run] = '*' then
        begin
          Inc(Run);
          Inc(fRange.Level);
        end;
      end
      else if fLine[Run] = '*' then
      begin
        Inc(Run);
        if fLine[Run] = ACloseChar then
        begin
          Inc(Run);
          if fRange.Level > 0 then
            Dec(fRange.Level);
          if fRange.Level = 0 then
          begin
            fRange.TokenRange := Ord(trNone);
            break
          end;
        end;
      end
      else
        Inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynM3Syn.SymNullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynM3Syn.SymNumberProc;
var
  BasedNumber: Boolean;
  MaxDigit: Integer;

  function IsValidDigit(AChar: WideChar): Boolean;
  var
    Digit: Integer;
  begin
    case AChar of
      '0'..'9': Digit := Ord(AChar) - Ord('0');
      'a'..'f': Digit := Ord(AChar) - Ord('a');
      'A'..'F': Digit := Ord(AChar) - Ord('A');
      else Digit := -1;
    end;
    Result := (Digit >= 0) and (Digit <= MaxDigit);
  end;

  function IsExponentChar: Boolean;
  begin
    case fLine[Run] of
      'd', 'D', 'e', 'E', 'x', 'X':
        Result := True;
      else
        Result := False;
    end;
  end;


begin
  fTokenID := tkNumber;
  BasedNumber := False;
  MaxDigit := 9;
  // skip leading zeros, but they can be numbers too
  while fLine[Run] = '0' do
    Inc(Run);
  if not IsIdentChar(fLine[Run]) then
    exit;
  // check for numbers with a base prefix
  if (fLine[Run] in [WideChar('2')..WideChar('9')]) and (fLine[Run + 1] = '_') then
  begin
    BasedNumber := True;
    MaxDigit := Ord(fLine[Run]) - Ord('0') - 1;
    Inc(Run, 2);
  end
  else if (fLine[Run] = '1') and (fLine[Run + 1] in [WideChar('0')..WideChar('6')])
    and (fLine[Run + 2] = '_') then
  begin
    BasedNumber := True;
    MaxDigit := 10 + Ord(fLine[Run + 1]) - Ord('0') - 1;
    Inc(Run, 3);
  end;
  if BasedNumber then
  begin
    // advance over all valid digits, but at least one has to be there
    if IsValidDigit(fLine[Run]) then
    begin
      repeat
        Inc(Run);
      until not IsValidDigit(fLine[Run]);
    end
    else
      fTokenID := tkSyntaxError;
  end
  else
  begin
    // "normal" numbers
    repeat
      Inc(Run);
    until not (fLine[Run] in [WideChar('0')..WideChar('9')]);
    // can include a decimal point and an exponent
    if fLine[Run] = '.' then
    begin
      Inc(Run);
      if fLine[Run] in [WideChar('0')..WideChar('9')] then
      begin
        repeat
          Inc(Run);
        until not (fLine[Run] in [WideChar('0')..WideChar('9')]);
      end
      else
        fTokenID := tkSyntaxError; // must be a number after the '.'
    end;
    // can include an exponent
    if IsExponentChar then
    begin
      Inc(Run);
      if fLine[Run] in [WideChar('+'), WideChar('-')] then
        Inc(Run);
      if fLine[Run] in [WideChar('0')..WideChar('9')] then
      begin
        repeat
          Inc(Run);
        until not (fLine[Run] in [WideChar('0')..WideChar('9')]);
      end
      else // exponent must include a number
        fTokenID := tkSyntaxError;
    end;
  end;
  // it's a syntax error if there are any Identifier chars left
  if IsIdentChar(fLine[Run]) then
  begin
    fTokenID := tkSyntaxError;
    repeat
      Inc(Run);
    until not IsIdentChar(fLine[Run]);
  end;
end;

procedure TSynM3Syn.SymPragmaProc;
begin
  Inc(Run);
  if fLine[Run] = '*' then
  begin
    Inc(Run);
    fRange.TokenRange := Ord(trPragma);
    Inc(fRange.Level);
    if IsLineEnd(Run) then
      fTokenID := tkPragma
    else
      SymPragmaHelpProc;
  end else
    fTokenID := tkSymbol;
end;

procedure TSynM3Syn.SymPragmaHelpProc;
begin
  fTokenID := tkPragma;
  SymNestedHelperProc('<', '>');
end;

procedure TSynM3Syn.SymRoundOpenProc;
begin
  Inc(Run);
  if fLine[Run] = '*' then
  begin
    Inc(Run);
    fRange.TokenRange := Ord(trComment);
    Inc(fRange.Level);
    if IsLineEnd(Run) then
      fTokenID := tkComment
    else
      SymCommentHelpProc;
  end
  else
  begin
    fTokenID := tkSymbol;
    if fLine[Run] = '.' then
      Inc(Run);
  end;
end;

procedure TSynM3Syn.SymSpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynM3Syn.SymStringProc;
begin
  fTokenID := tkString;
  Inc(Run);
  while not IsLineEnd(Run) do
  begin
    case fLine[Run] of
      #34: begin
             Inc(Run);
             break;
           end;
      '\': if fLine[Run + 1] in [WideChar(#34), WideChar('\')] then
             Inc(Run);
    end;
    Inc(Run);
  end;
end;

procedure TSynM3Syn.SymSymbolProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynM3Syn.SymUnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynM3Syn.Next;
begin
  fTokenPos := Run;
  case TTokenRange(fRange.TokenRange) of
    trComment: SymCommentHelpProc;
    trPragma: SymPragmaHelpProc;
  else
    case fLine[Run] of
      #39: SymAsciiCharProc;
      #13: SymCRProc;
      'A'..'Z', 'a'..'z', '_': SymIdentProc;
      #10: SymLFProc;
       #0: SymNullProc;
      '0'..'9': SymNumberProc;
      '(': SymRoundOpenProc;
      #1..#9, #11, #12, #14..#32: SymSpaceProc;
      '{','}','|','!', #35..#38, #42..#47, #58, #59, #61..#64, #91..#94, ')': SymSymbolProc;
      '<': SymPragmaProc;
      #34: SymStringProc;
      else SymUnknownProc;
    end;
  end;
  inherited;
end;

function TSynM3Syn.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynM3Syn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynM3Syn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterModula3;
end;

class function TSynM3Syn.GetLanguageName: string;
begin
  Result := SYNS_LangModula3;
end;

function TSynM3Syn.GetRange: pointer;
begin
  result := fRange.p;
end;

function TSynM3Syn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkPragma: Result:= fPragmaAttri;
    tkReserved: Result := fReservedAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkSyntaxError: Result := fSyntaxErrorAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynM3Syn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynM3Syn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

procedure TSynM3Syn.ResetRange;
begin
  fRange.p := nil;
end;

procedure TSynM3Syn.SetRange(Value: pointer);
begin
  fRange.p := Value;
end;

class function TSynM3Syn.GetFriendlyLanguageName: WideString;
begin
  Result := SYNS_FriendlyLangModula3;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynM3Syn);
{$ENDIF}
end.
