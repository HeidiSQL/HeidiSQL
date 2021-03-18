{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: C:\Users\Public\Code\SynEdit\SynGen\Test ECMAScript\SynHighlighterECMAScript.pas, released 2020-06-21.
Description: ECMA Script Syntax Highlighter
The initial author of this file is Christian-W. Budde.
Copyright (c) 2020, all rights reserved.

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

$Id: $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

unit SynHighlighterECMAScript;

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
  TtkTokenKind = (
    tkComment,
    tkIdentifier,
    tkKey,
    tkReserved,
    tkStrict,
    tkNull,
    tkNumber,
    tkSpace,
    tkString,
    tkSymbol,
    tkUnknown);

  TRangeState = (rsUnknown, rsMultiLineComment, rsSingleLineComment,
    rsDoubleQuotedString, rsSingleQuotedString);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynECMAScriptSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FTokenId: TtkTokenKind;
    FIdentFuncTable: array[0..108] of TIdentFuncTableFunc;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function FuncKeyWord(Index: Integer): TtkTokenKind;
    function FuncReservedWord(Index: Integer): TtkTokenKind;
    function FuncStrictMode(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure AndSymbolProc;
    procedure BackslashProc;
    procedure CRProc;
    procedure CoalesceProc;
    procedure EqualsProc;
    procedure GreaterProc;
    procedure Hex4DigitProc;
    procedure InitIdent;
    procedure IdentProc;
    procedure LessProc;
    procedure LFProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure MultiLineCommentProc;
    procedure NotProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure DotProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StarProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    procedure XorSymbolProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetFriendlyLanguageName: UnicodeString; override;
    class function GetLanguageName: string; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetKeyWords(TokenKind: Integer): UnicodeString; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
  end;

implementation

uses
  SynEditStrConst;

resourcestring
  SYNS_FilterECMAScript = 'JavaScript files (*.js)|*.js';
  SYNS_LangECMAScript = 'ECMA Script';
  SYNS_FriendlyLangECMAScript = 'ECMA Script';

const
  // as this language is case-insensitive keywords *must* be in lowercase
  KeyWords: array[0..52] of UnicodeString = (
    'as', 'async', 'await', 'break', 'case', 'catch', 'class', 'const',
    'continue', 'debugger', 'default', 'delete', 'do', 'else', 'enum', 'export',
    'extends', 'false', 'finally', 'for', 'from', 'function', 'get', 'if',
    'implements', 'import', 'in', 'instance', 'interface', 'let', 'new', 'null',
    'of', 'package', 'private', 'protected', 'public', 'return', 'set',
    'static', 'super', 'switch', 'target', 'this', 'throw', 'true', 'try',
    'typeof', 'var', 'void', 'while', 'with', 'yield'
  );

  KeyIndices: array[0..108] of Integer = (
    -1, 25, -1, 42, -1, 52, 48, 10, -1, -1, -1, -1, 2, -1, -1, -1, -1, -1, -1,
    4, 46, -1, 5, 30, -1, -1, 27, 20, -1, -1, 32, 51, -1, 38, 13, -1, 28, 12,
    -1, -1, 19, 41, -1, 3, -1, -1, -1, 9, 6, -1, 24, 34, -1, -1, 36, 16, 49, -1,
    22, 17, -1, -1, 35, -1, 21, 0, -1, -1, -1, -1, 50, 11, -1, 40, 18, -1, 7,
    -1, -1, 39, 44, -1, -1, 47, 31, 43, 26, -1, -1, -1, -1, 33, 23, 29, 14, -1,
    45, 37, -1, 8, -1, -1, -1, -1, -1, -1, 1, -1, 15
  );

constructor TSynECMAScriptSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaseSensitive := False;

  FCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  FCommentAttri.Foreground := clNavy;
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);

  FSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  FDefaultFilter := SYNS_FilterECMAScript;
  FRange := rsUnknown;
end;

procedure TSynECMAScriptSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[12] := FuncReservedWord;
  FIdentFuncTable[65] := FuncKeyWord;
  FIdentFuncTable[106] := FuncKeyWord;
  FIdentFuncTable[43] := FuncReservedWord;
  FIdentFuncTable[19] := FuncReservedWord;
  FIdentFuncTable[22] := FuncReservedWord;
  FIdentFuncTable[48] := FuncReservedWord;
  FIdentFuncTable[76] := FuncReservedWord;
  FIdentFuncTable[99] := FuncReservedWord;
  FIdentFuncTable[47] := FuncReservedWord;
  FIdentFuncTable[7] := FuncReservedWord;
  FIdentFuncTable[71] := FuncReservedWord;
  FIdentFuncTable[37] := FuncReservedWord;
  FIdentFuncTable[34] := FuncReservedWord;
  FIdentFuncTable[94] := FuncReservedWord;
  FIdentFuncTable[108] := FuncReservedWord;
  FIdentFuncTable[55] := FuncReservedWord;
  FIdentFuncTable[59] := FuncReservedWord;
  FIdentFuncTable[74] := FuncReservedWord;
  FIdentFuncTable[40] := FuncReservedWord;
  FIdentFuncTable[27] := FuncKeyWord;
  FIdentFuncTable[64] := FuncReservedWord;
  FIdentFuncTable[58] := FuncKeyWord;
  FIdentFuncTable[92] := FuncReservedWord;
  FIdentFuncTable[50] := FuncStrictMode;
  FIdentFuncTable[1] := FuncReservedWord;
  FIdentFuncTable[86] := FuncReservedWord;
  FIdentFuncTable[26] := FuncReservedWord;
  FIdentFuncTable[36] := FuncStrictMode;
  FIdentFuncTable[93] := FuncStrictMode;
  FIdentFuncTable[23] := FuncReservedWord;
  FIdentFuncTable[84] := FuncReservedWord;
  FIdentFuncTable[30] := FuncKeyWord;
  FIdentFuncTable[91] := FuncStrictMode;
  FIdentFuncTable[51] := FuncStrictMode;
  FIdentFuncTable[62] := FuncStrictMode;
  FIdentFuncTable[54] := FuncStrictMode;
  FIdentFuncTable[97] := FuncReservedWord;
  FIdentFuncTable[33] := FuncKeyWord;
  FIdentFuncTable[79] := FuncStrictMode;
  FIdentFuncTable[73] := FuncReservedWord;
  FIdentFuncTable[41] := FuncReservedWord;
  FIdentFuncTable[3] := FuncKeyWord;
  FIdentFuncTable[85] := FuncReservedWord;
  FIdentFuncTable[80] := FuncReservedWord;
  FIdentFuncTable[96] := FuncReservedWord;
  FIdentFuncTable[20] := FuncReservedWord;
  FIdentFuncTable[83] := FuncReservedWord;
  FIdentFuncTable[6] := FuncReservedWord;
  FIdentFuncTable[56] := FuncReservedWord;
  FIdentFuncTable[70] := FuncReservedWord;
  FIdentFuncTable[31] := FuncReservedWord;
  FIdentFuncTable[5] := FuncReservedWord;
end;

{$Q-}
function TSynECMAScriptSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 474 + Ord(Str^) * 408;
    Inc(Str);
  end;
  Result := Result mod 109;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynECMAScriptSyn.FuncReservedWord(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkReserved
  else
    Result := tkIdentifier;
end;

function TSynECMAScriptSyn.FuncStrictMode(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStrict
  else
    Result := tkIdentifier;
end;

function TSynECMAScriptSyn.FuncKeyWord(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynECMAScriptSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

procedure TSynECMAScriptSyn.AndSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(FLine[Run], ['=', '&']) then
    Inc(Run);
end;

function TSynECMAScriptSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Key: Cardinal;
begin
  FToIdent := MayBe;
  Key := HashKey(MayBe);
  if Key <= High(FIdentFuncTable) then
    Result := FIdentFuncTable[Key](KeyIndices[Key])
  else
    Result := tkIdentifier;
end;

procedure TSynECMAScriptSyn.SpaceProc;
begin
  Inc(Run);
  FTokenId := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do
    Inc(Run);
end;

procedure TSynECMAScriptSyn.CoalesceProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] = '?' then
    Inc(Run);
end;

procedure TSynECMAScriptSyn.NotProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] = '=' then
  begin
    Inc(Run);
    if FLine[Run] = '=' then
      Inc(Run);
  end;
end;

procedure TSynECMAScriptSyn.NullProc;
begin
  FTokenId := tkNull;
  Inc(Run);
end;

procedure TSynECMAScriptSyn.CRProc;
begin
  FTokenId := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynECMAScriptSyn.LFProc;
begin
  FTokenId := tkSpace;
  Inc(Run);
end;

procedure TSynECMAScriptSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', '.', 'a'..'f', 'A'..'F', 'x', 'X':
        Result := True;
      else
        Result := False;
    end;
  end;

  function IsHexChar(Run: Integer): Boolean;
  begin
    case FLine[Run] of
      '0'..'9', 'a'..'f', 'A'..'F':
        Result := True;
      else
        Result := False;
    end;
  end;

var
  idx1: Integer; // token[1]
  isHex: Boolean;
begin
  FTokenID := tkNumber;
  isHex := False;
  idx1 := Run;
  Inc(Run);
  while IsNumberChar do
  begin
    case FLine[Run] of
      '.':
        if FLine[Succ(Run)] = '.' then
          Break;
      'a'..'f', 'A'..'F':
        if not isHex then
          Break;
      'x', 'X':
        begin
          if (FLine[idx1] <> '0') or (Run > Succ(idx1)) then
            Break;
          if not IsHexChar(Succ(Run)) then
            Break;
          isHex := True;
        end;
    end;
    Inc(Run);
  end;
end;

procedure TSynECMAScriptSyn.OrSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(FLine[Run], ['=', '|']) then Inc(Run);
end;

procedure TSynECMAScriptSyn.EqualsProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  case FLine[Run] of
    '=':
      begin
        Inc(Run);
        if FLine[Run] = '=' then
          Inc(Run);
      end;
    '>':
      Inc(Run);
  end;
end;

procedure TSynECMAScriptSyn.GreaterProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  case FLine[Run] of
    '=':
      Inc(Run);
    '>':
      begin
        Inc(Run);
        case FLine[Run] of
          '=':
            Inc(Run);
          '>':
            begin
              Inc(Run);
              if FLine[Run] = '=' then
                Inc(Run);
            end;
        end;
      end;
  end;
end;

procedure TSynECMAScriptSyn.LessProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  case FLine[Run] of
    '=':
      Inc(Run);
    '<':
      begin
        Inc(Run);
        if FLine[Run] = '=' then
          Inc(Run);
      end;
  end;
end;

procedure TSynECMAScriptSyn.PlusProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(FLine[Run], ['=', '+']) then
    Inc(Run);
end;

procedure TSynECMAScriptSyn.DotProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if (FLine[Run] = '.') and (FLine[Run + 1] = '.') then Inc(Run, 2);
end;

procedure TSynECMAScriptSyn.SlashProc;
begin
  Inc(Run);
  case FLine[Run] of
    '/':
      begin
        FTokenId := tkComment;
        FRange := rsSingleLineComment;
        repeat
          Inc(Run);
        until IsLineEnd(Run);
        FTokenId := tkComment;
      end;
    '*':
      begin
        Inc(Run, 1);
        FRange := rsMultiLineComment;
        FTokenId := tkComment;
        repeat
          Inc(Run);
           if (FLine[Run] = '*') and (FLine[Run + 1] = '/') then
           begin
             FRange := rsUnknown;
             Inc(Run, 2);
             Break;
           end;
         until IsLineEnd(Run);
      end;
    '=':
      begin
        Inc(Run);
        FTokenID := tkSymbol;
      end;
  end;
end;

procedure TSynECMAScriptSyn.MultiLineCommentProc;
begin
  case FLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      FTokenId := tkComment;
      repeat
        if (FLine[Run] = '*') and
           (FLine[Run + 1] = '/') then
        begin
          Inc(Run, 2);
          FRange := rsUnknown;
          Break;
        end;
        if not IsLineEnd(Run) then
          Inc(Run);
      until IsLineEnd(Run);
    end;
  end;
end;

procedure TSynECMAScriptSyn.StarProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  case FLine[Run] of
    '=':
      Inc(Run);
    '*':
      begin
        Inc(Run);
        if FLine[Run] = '=' then
          Inc(Run);
      end;
  end;
end;

procedure TSynECMAScriptSyn.StringProc;
var
  QuoteChar: UnicodeString;
begin
  FTokenID := tkString;
  QuoteChar := FLine[Run];   // We could have '"' or #39
  if (FLine[Run + 1] = QuoteChar) and (FLine[Run + 2] = QuoteChar) then Inc(Run, 2);
  repeat
    if IsLineEnd(Run) then
      Break;
    Inc(Run);
  until (FLine[Run] = QuoteChar) and (FLine[Pred(Run)] <> '\');
  if not IsLineEnd(Run) then
    Inc(Run);
end;

procedure TSynECMAScriptSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + Run));
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
end;

procedure TSynECMAScriptSyn.Hex4DigitProc;
var
  i: Integer;

  function IsHexDigit(AChar: WideChar): Boolean;
  begin
    case FLine[Run] of
      '0'..'9', 'a'..'f', 'A'..'F':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  for i := 0 to 3 do
  begin
    if not IsHexDigit(FLine[Run]) then
    begin
      FTokenId := tkUnknown;
      Exit;
    end;
    Inc(Run);
  end;
end;

procedure TSynECMAScriptSyn.BackslashProc;
begin
  Inc(Run);
  if FLine[Run] = 'u' then
  begin
    Inc(Run);
    Hex4DigitProc;
  end
  else
    FTokenId := tkUnknown;
end;

procedure TSynECMAScriptSyn.MinusProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(FLine[Run], ['=', '-', '>']) then Inc(Run);
end;

procedure TSynECMAScriptSyn.ModSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] = '=' then Inc(Run);
end;

procedure TSynECMAScriptSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynECMAScriptSyn.UnknownProc;
begin
  Inc(Run);
  FTokenId := tkUnknown;
end;

procedure TSynECMAScriptSyn.XorSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] = '=' then
    Inc(Run);
end;

procedure TSynECMAScriptSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsMultiLineComment: MultiLineCommentProc;
  else
    case FLine[Run] of
      #0:
        NullProc;
      #10:
        LFProc;
      #13:
        CRProc;
      #1..#9, #11, #12, #14..#32:
        SpaceProc;
      '"', #39:
        StringProc;
      '%':
        ModSymbolProc;
      '&':
        AndSymbolProc;
      '.':
        DotProc;
      '/':
        SlashProc;
      '-':
        MinusProc;
      '\':
        BackslashProc;
      '|':
        OrSymbolProc;
      '(', ')':
        SymbolProc;
      '*':
        StarProc;
      '+':
        PlusProc;
      '=':
        EqualsProc;
      '>':
        GreaterProc;
      '<':
        LessProc;
      '!':
        NotProc;
      '?':
        CoalesceProc;
      '^':
        XorSymbolProc;
      '~', ',', '[', ']', ':', ';', '{', '}':
        SymbolProc;
      '0'..'9':
        NumberProc;
      'A'..'Z', 'a'..'z', '_', '$', #$AA, #$B5, #$BA, #$C0..#$D6, #$D8..#$F6,
      #$0F8..#$2C1:
        IdentProc;
    else
      UnknownProc;
    end;
  end;
  inherited;
end;

function TSynECMAScriptSyn.GetDefaultAttribute(Index: Integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT:
      Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER:
      Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD:
      Result := FKeyAttri;
    SYN_ATTR_STRING:
      Result := FStringAttri;
    SYN_ATTR_WHITESPACE:
      Result := FSpaceAttri;
    SYN_ATTR_SYMBOL:
      Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynECMAScriptSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynECMAScriptSyn.GetKeyWords(TokenKind: Integer): UnicodeString;
begin
  Result :=
    'as,async,await,break,case,catch,class,const,continue,debugger,default,' +
    'delete,do,else,enum,export,extends,false,finally,for,from,function,' +
    'get,if,implements,import,in,instance,interface,let,new,null,of,package,' +
    'private,protected,public,return,set,static,super,switch,target,this,' +
    'throw,true,try,typeof,var,void,while,with,yield';
end;

function TSynECMAScriptSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenId;
end;

function TSynECMAScriptSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment:
      Result := FCommentAttri;
    tkIdentifier:
      Result := FIdentifierAttri;
    tkKey:
      Result := FKeyAttri;
    tkReserved:
      Result := FKeyAttri;
    tkNumber:
      Result := FNumberAttri;
    tkStrict:
      Result := FKeyAttri;
    tkSpace:
      Result := FSpaceAttri;
    tkString:
      Result := FStringAttri;
    tkSymbol:
      Result := FSymbolAttri;
    tkUnknown:
      Result := FIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynECMAScriptSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenId);
end;

function TSynECMAScriptSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    'A'..'Z', 'a'..'z', '_', '$', #$AA, #$B5, #$BA, #$C0..#$D6, #$D8..#$F6,
    #$0F8..#$2C1:
      Result := True;
    else
      Result := False;
  end;
end;

function TSynECMAScriptSyn.GetSampleSource: UnicodeString;
begin
  Result :=
    '// Syntax highlighting'#13#10 +
    'function printNumber()'#13#10 +
    '{'#13#10 +
    '  var number = 1234;'#13#10 +
    '  var x;'#13#10 +
    '  document.write("The number is " + number);'#13#10 +
    '  for (var i = 0; i <= number; i++)'#13#10 +
    '  {'#13#10 +
    '    x++;'#13#10 +
    '    x--;'#13#10 +
    '    x += 1.0;'#13#10 +
    '  }'#13#10 +
    '  i += @; // illegal character'#13#10 +
    '}'#13#10 +
    'body.onLoad = printNumber;';
end;

function TSynECMAScriptSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterECMAScript;
end;

class function TSynECMAScriptSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangECMAScript;
end;

class function TSynECMAScriptSyn.GetLanguageName: string;
begin
  Result := SYNS_LangECMAScript;
end;

procedure TSynECMAScriptSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynECMAScriptSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynECMAScriptSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

initialization
  RegisterPlaceableHighlighter(TSynECMAScriptSyn);

end.
