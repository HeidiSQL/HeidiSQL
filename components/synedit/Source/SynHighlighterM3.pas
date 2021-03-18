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

$Id: SynHighlighterM3.pas,v 1.11.2.5 2008/09/14 16:25:00 maelh Exp $

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

unit SynHighlighterM3;

{$I SynEdit.inc}

interface

uses
  Graphics,
  Registry,
  SynEditTypes,
  SynEditHighlighter,
  SynHighlighterHashEntries,
  SynUnicode,
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
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FPragmaAttri: TSynHighlighterAttributes;
    FReservedAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FSyntaxErrorAttri: TSynHighlighterAttributes;
    FKeywords: TSynHashEntryList;
    procedure DoAddKeyword(AKeyword: UnicodeString; AKind: Integer);
    function HashKey(Str: PWideChar): Integer;
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
    class function GetFriendlyLanguageName: UnicodeString; override;
{$IFDEF SYN_DEVELOPMENT_CHECKS}
  public
    property _Keywords: TSynHashEntryList read FKeywords;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property PragmaAttri: TSynHighlighterAttributes read FPragmaAttri
      write FPragmaAttri;
    property ReservedAttri: TSynHighlighterAttributes read FReservedAttri
      write FReservedAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri
      write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
    property SyntaxErrorAttri: TSynHighlighterAttributes read FSyntaxErrorAttri
      write FSyntaxErrorAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  Keywords: UnicodeString =
    'AS,AND,ANY,ARRAY,BEGIN,BITS,BRANDED,BY,CASE,CONST,DIV,DO,ELSE,ELSIF,END,' +
    'EVAL,EXCEPT,EXCEPTION,EXIT,EXPORTS,FINALLY,FOR,FROM,GENERIC,IF,IMPORT,' +
    'IN,INTERFACE,LOCK,LOOP,METHODS,MOD,MODULE,NOT,OBJECT,OF,OR,OVERRIDES,' +
    'PROCEDURE,RAISE,RAISES,READONLY,RECORD,REF,REPEAT,RETURN,REVEAL,ROOT,' +
    'SET,THEN,TO,TRY,TYPE,TYPECASE,UNSAFE,UNTIL,UNTRACED,VALUE,VAR,WHILE,WITH';

  ReservedWords: UnicodeString =
    'ABS,ADDRESS,ADR,ADRSIZE,BITSIZE,BOOLEAN,BYTESIZE,CARDINAL,CEILING,CHAR,' +
    'DEC,DISPOSE,FALSE,FIRST,FLOAT,FLOOR,INC,INTEGER,ISTYPE,LAST,LONGFLOAT,' +
    'LONGREAL,LOOPHOLE,MAX,MIN,MUTEX,NARROW,NEW,NIL,NULL,NUMBER,ORD,REAL,' +
    'REFANY,ROUND,SUBARRAY,TEXT,TRUE,TRUNC,TYPECODE,VAL';

procedure TSynM3Syn.DoAddKeyword(AKeyword: UnicodeString; AKind: Integer);
var
  HashValue: Integer;
begin
  HashValue := HashKey(PWideChar(AKeyword));
  FKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
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
  FStringLen := Str - FToIdent;
end;

function TSynM3Syn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
begin
  FToIdent := MayBe;
  Entry := FKeywords[HashKey(MayBe)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > FStringLen then
      Break
    else if Entry.KeywordLen = FStringLen then
      if IsCurrentToken(Entry.Keyword) then
      begin
        Result := TtkTokenKind(Entry.Kind);
        Exit;
      end;
    Entry := Entry.Next;
  end;
  Result := tkIdentifier;
end;

constructor TSynM3Syn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

  FKeywords := TSynHashEntryList.Create;
  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style:= [fsItalic];
  AddAttribute(FCommentAttri);
  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey, SYNS_FriendlyAttrKey);
  FKeyAttri.Style:= [fsBold];
  AddAttribute(FKeyAttri);
  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);
  FPragmaAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  FPragmaAttri.Style:= [fsBold];
  AddAttribute(FPragmaAttri);
  FReservedAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  AddAttribute(FReservedAttri);
  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);
  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  FSyntaxErrorAttri := TSynHighlighterAttributes.Create(SYNS_AttrSyntaxError, SYNS_FriendlyAttrSyntaxError);
  FSyntaxErrorAttri.Foreground := clRed;
  AddAttribute(FSyntaxErrorAttri);
  SetAttributesOnChange(DefHighlightChange);

  EnumerateKeywords(Ord(tkKey), Keywords, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkReserved), ReservedWords, IsIdentChar, DoAddKeyword);
  FDefaultFilter := SYNS_FilterModula3;
end;

destructor TSynM3Syn.Destroy;
begin
  FKeywords.Free;
  inherited Destroy;
end;

procedure TSynM3Syn.SymAsciiCharProc;
begin
  FTokenID := tkString;
  Inc(Run);
  while not IsLineEnd(Run) do
  begin
    case FLine[Run] of
      '\': if FLine[Run + 1] = #39 then
             Inc(Run);
      #39: begin
             Inc(Run);
             if FLine[Run] <> #39 then
               Break;
           end;
    end;
    Inc(Run);
  end;
end;

procedure TSynM3Syn.SymCommentHelpProc;
begin
  FTokenID := tkComment;
  SymNestedHelperProc('(', ')');
end;

procedure TSynM3Syn.SymCRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynM3Syn.SymIdentProc;
begin
  FTokenID := IdentKind(FLine + Run);
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
end;

procedure TSynM3Syn.SymLFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynM3Syn.SymNestedHelperProc(AOpenChar, ACloseChar: WideChar);
begin
  case FLine[Run] of
     #0: SymNullProc;
    #10: SymLFProc;
    #13: SymCRProc;
  else
    repeat
      if FLine[Run]= AOpenChar then
      begin
        Inc(Run);
        if FLine[Run] = '*' then
        begin
          Inc(Run);
          Inc(FRange.Level);
        end;
      end
      else if FLine[Run] = '*' then
      begin
        Inc(Run);
        if FLine[Run] = ACloseChar then
        begin
          Inc(Run);
          if FRange.Level > 0 then
            Dec(FRange.Level);
          if FRange.Level = 0 then
          begin
            FRange.TokenRange := Ord(trNone);
            Break
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
  FTokenID := tkNull;
  Inc(Run);
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
    case FLine[Run] of
      'd', 'D', 'e', 'E', 'x', 'X':
        Result := True;
      else
        Result := False;
    end;
  end;


begin
  FTokenID := tkNumber;
  BasedNumber := False;
  MaxDigit := 9;
  // skip leading zeros, but they can be numbers too
  while FLine[Run] = '0' do
    Inc(Run);
  if not IsIdentChar(FLine[Run]) then
    Exit;
  // check for numbers with a base prefix
  if CharInSet(FLine[Run], ['2'..'9']) and (FLine[Run + 1] = '_') then
  begin
    BasedNumber := True;
    MaxDigit := Ord(FLine[Run]) - Ord('0') - 1;
    Inc(Run, 2);
  end
  else if (FLine[Run] = '1') and CharInSet(FLine[Run + 1], ['0'..'6'])
    and (FLine[Run + 2] = '_') then
  begin
    BasedNumber := True;
    MaxDigit := 10 + Ord(FLine[Run + 1]) - Ord('0') - 1;
    Inc(Run, 3);
  end;
  if BasedNumber then
  begin
    // advance over all valid digits, but at least one has to be there
    if IsValidDigit(FLine[Run]) then
    begin
      repeat
        Inc(Run);
      until not IsValidDigit(FLine[Run]);
    end
    else
      FTokenID := tkSyntaxError;
  end
  else
  begin
    // "normal" numbers
    repeat
      Inc(Run);
    until not CharInSet(FLine[Run], ['0'..'9']);
    // can include a decimal point and an exponent
    if FLine[Run] = '.' then
    begin
      Inc(Run);
      if CharInSet(FLine[Run], ['0'..'9']) then
      begin
        repeat
          Inc(Run);
        until not CharInSet(FLine[Run], ['0'..'9']);
      end
      else
        FTokenID := tkSyntaxError; // must be a number after the '.'
    end;
    // can include an exponent
    if IsExponentChar then
    begin
      Inc(Run);
      if CharInSet(FLine[Run], ['+', '-']) then
        Inc(Run);
      if CharInSet(FLine[Run], ['0'..'9']) then
      begin
        repeat
          Inc(Run);
        until not CharInSet(FLine[Run], ['0'..'9']);
      end
      else // exponent must include a number
        FTokenID := tkSyntaxError;
    end;
  end;
  // it's a syntax error if there are any Identifier chars left
  if IsIdentChar(FLine[Run]) then
  begin
    FTokenID := tkSyntaxError;
    repeat
      Inc(Run);
    until not IsIdentChar(FLine[Run]);
  end;
end;

procedure TSynM3Syn.SymPragmaProc;
begin
  Inc(Run);
  if FLine[Run] = '*' then
  begin
    Inc(Run);
    FRange.TokenRange := Ord(trPragma);
    Inc(FRange.Level);
    if IsLineEnd(Run) then
      FTokenID := tkPragma
    else
      SymPragmaHelpProc;
  end else
    FTokenID := tkSymbol;
end;

procedure TSynM3Syn.SymPragmaHelpProc;
begin
  FTokenID := tkPragma;
  SymNestedHelperProc('<', '>');
end;

procedure TSynM3Syn.SymRoundOpenProc;
begin
  Inc(Run);
  if FLine[Run] = '*' then
  begin
    Inc(Run);
    FRange.TokenRange := Ord(trComment);
    Inc(FRange.Level);
    if IsLineEnd(Run) then
      FTokenID := tkComment
    else
      SymCommentHelpProc;
  end
  else
  begin
    FTokenID := tkSymbol;
    if FLine[Run] = '.' then
      Inc(Run);
  end;
end;

procedure TSynM3Syn.SymSpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynM3Syn.SymStringProc;
begin
  FTokenID := tkString;
  Inc(Run);
  while not IsLineEnd(Run) do
  begin
    case FLine[Run] of
      #34: begin
             Inc(Run);
             Break;
           end;
      '\': if CharInSet(FLine[Run + 1], [#34, '\']) then
             Inc(Run);
    end;
    Inc(Run);
  end;
end;

procedure TSynM3Syn.SymSymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynM3Syn.SymUnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynM3Syn.Next;
begin
  FTokenPos := Run;
  case TTokenRange(FRange.TokenRange) of
    trComment: SymCommentHelpProc;
    trPragma: SymPragmaHelpProc;
  else
    case FLine[Run] of
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

function TSynM3Syn.GetDefaultAttribute(Index: Integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynM3Syn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynM3Syn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterModula3;
end;

class function TSynM3Syn.GetLanguageName: string;
begin
  Result := SYNS_LangModula3;
end;

function TSynM3Syn.GetRange: Pointer;
begin
  Result := FRange.p;
end;

function TSynM3Syn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkPragma: Result:= FPragmaAttri;
    tkReserved: Result := FReservedAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkSyntaxError: Result := FSyntaxErrorAttri;
    tkUnknown: Result := FIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynM3Syn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynM3Syn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynM3Syn.ResetRange;
begin
  FRange.p := nil;
end;

procedure TSynM3Syn.SetRange(Value: Pointer);
begin
  FRange.p := Value;
end;

class function TSynM3Syn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangModula3;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynM3Syn);
{$ENDIF}
end.
