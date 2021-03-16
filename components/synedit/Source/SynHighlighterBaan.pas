{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterBaan.pas, released 2000-04-21.
The Original Code is based on the mwBaanSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is "riceball".
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

$Id: SynHighlighterBaan.pas,v 1.13.2.6 2008/09/14 16:24:59 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Baan syntax highlighter for SynEdit)
@author(riceball <teditor@mailroom.com>, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(2000, converted to SynEdit 2000-04-21)
@lastmod(2000-04-21)
The SynHighlighterBaan unit provides SynEdit with a Baan syntax highlighter.
Thanks to Martin Waldenburg.
}

unit SynHighlighterBaan;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, Controls, Graphics, Registry,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
  SysUtils, Classes;

type
  TtkTokenKind = (tkComment, tkDirective, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown, tkVariable);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynBaanSyn = class(TSynCustomHighlighter)
  private
    FTokenID: TtkTokenKind;
    FIdentFuncTable: array[0..460] of TIdentFuncTableFunc;
    FCommentAttri: TSynHighlighterAttributes;
    FDirectiveAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FVariableAttri: TSynHighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function FuncBrp46open(Index: Integer): TtkTokenKind;
    function FuncDate46num(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure AtSymbolProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure DirectiveProc;
    procedure EqualProc;
    procedure ErectProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PlusProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StarProc;
    procedure StringProc;
    procedure TildeProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
  protected
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property DirectiveAttri: TSynHighlighterAttributes read FDirectiveAttri
      write FDirectiveAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri
      write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
    property VariableAttri: TSynHighlighterAttributes read FVariableAttri
      write FVariableAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  KeyWords: array[0..112] of UnicodeString = (
    '__based', '__cdecl', '__declspe', '__except', '__export', '__far', 
    '__fastcal', '__fortran', '__import', '__int16', '__int32', '__int64', 
    '__int8', '__interrup', '__loadds', '__near', '__pascal', '__rtti', 
    '__segment', '__segname', '__self', '__stdcall', '__thread', '__try', 
    '_cdecl', '_export', '_fastcall', '_import', '_pascal', '_stdcall', 'auto', 
    'bool', 'break', 'brp.open', 'case', 'catch', 'cdecl', 'char', 'class', 
    'const', 'continue', 'date.num', 'default', 'defined', 'delete', 'do', 
    'domain', 'double', 'else', 'endif', 'endselect', 'enum', 'explicit', 
    'export', 'extern', 'false', 'fastcall', 'finally', 'float', 'for', 
    'friend', 'from', 'function', 'goto', 'if', 'import', 'inline', 'int', 
    'interrupt', 'long', 'mutable', 'namespace', 'new', 'null', 'operator', 
    'pascal', 'private', 'protected', 'public', 'register', 'reinterpr', 
    'return', 'select', 'selectdo', 'short', 'signed', 'sizeof', 'sql.close', 
    'static', 'static_ca', 'stdcall', 'string', 'strip$', 'struct', 'switch', 
    'table', 'template', 'this', 'throw', 'true', 'try', 'typedef', 'typeid', 
    'typename', 'union', 'unsigned', 'using', 'virtual', 'void', 'volatile', 
    'wchar_t', 'where', 'while' 
  );

  KeyIndices: array[0..460] of Integer = (
    -1, -1, -1, -1, -1, -1, 83, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 3, 33, 26, -1, 78, -1, -1, -1, -1, -1, 5, -1, 14, -1, 27, -1, 92, -1, 
    -1, -1, -1, 42, -1, 77, -1, -1, -1, -1, -1, -1, -1, -1, -1, 61, -1, -1, -1, 
    93, 2, -1, -1, -1, 50, -1, -1, -1, -1, -1, 40, -1, -1, -1, -1, 63, -1, 94, 
    -1, -1, 69, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 25, -1, -1, 44, -1, -1, 
    -1, 110, -1, -1, 51, -1, -1, -1, -1, 56, -1, 32, -1, -1, 109, -1, -1, -1, 
    -1, 16, -1, -1, -1, -1, 23, 88, -1, -1, 10, -1, -1, -1, -1, 67, -1, -1, -1, 
    72, 81, -1, -1, -1, 82, 24, -1, -1, -1, -1, -1, -1, -1, -1, 79, -1, -1, 64, 
    21, 80, -1, -1, 59, 0, -1, -1, -1, 12, -1, -1, 107, -1, 36, -1, -1, -1, -1, 
    31, -1, -1, -1, 62, -1, -1, 112, -1, -1, -1, -1, -1, -1, 7, -1, 106, -1, -1, 
    -1, -1, -1, -1, -1, -1, 52, 104, -1, 18, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 65, -1, -1, -1, 13, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 29, 28, 43, -1, 20, -1, -1, -1, 38, -1, -1, -1, -1, 
    -1, 103, -1, 70, 87, -1, -1, -1, 85, -1, 74, -1, -1, -1, -1, -1, 35, 39, -1, 
    -1, 97, 53, -1, -1, -1, -1, -1, -1, -1, 84, -1, 95, -1, -1, -1, -1, -1, -1, 
    -1, 100, 98, -1, -1, -1, -1, -1, -1, -1, -1, 111, 73, -1, 47, -1, -1, -1, 
    -1, -1, -1, -1, 105, -1, -1, -1, -1, -1, 66, 86, -1, -1, -1, -1, -1, -1, -1, 
    -1, 34, -1, -1, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 37, 55, -1, 
    -1, -1, 89, -1, 11, -1, -1, -1, 19, -1, -1, -1, -1, 90, -1, 102, 54, -1, -1, 
    45, -1, -1, 6, 30, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 46, 8, 22, -1, 
    -1, -1, -1, 99, -1, -1, -1, -1, -1, -1, -1, -1, -1, 101, -1, -1, -1, -1, -1, 
    -1, -1, 71, -1, -1, -1, -1, -1, 96, 48, -1, -1, -1, -1, -1, 75, -1, 60, -1, 
    -1, 58, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, 17, 4, -1, -1, -1, -1, 
    49, -1, -1, -1, -1, 57, -1, -1, -1, -1, 15, 91, -1, -1, 41, -1, -1, -1, 76, 
    68, -1, -1, -1, 108, -1, -1 
  );

{$Q-}
function TSynBaanSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 838 + Ord(Str^) * 296;
    Inc(Str);
  end;
  Result := Result mod 461;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynBaanSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynBaanSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[21] := FuncBrp46open;
  FIdentFuncTable[449] := FuncDate46num;

  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if @FIdentFuncTable[i] = nil then
      FIdentFuncTable[i] := KeyWordFunc;
end;

function TSynBaanSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynBaanSyn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

function TSynBaanSyn.FuncBrp46open(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynBaanSyn.FuncDate46num(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

constructor TSynBaanSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);
  FDirectiveAttri := TSynHighlighterAttributes.Create(SYNS_AttrDirective, SYNS_FriendlyAttrDirective);
  AddAttribute(FDirectiveAttri);
  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);
  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);
  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);
  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  FVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  AddAttribute(FVariableAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  FDefaultFilter := SYNS_FilterBaan;
end;

procedure TSynBaanSyn.AndSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {and assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
    '&':                               {logical and}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {and}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.AsciiCharProc;
begin
  FTokenID := tkString;
  repeat
    case FLine[Run] of
      #0, #10, #13:
        Break;
    end;
    Inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then Inc(Run);
end;

procedure TSynBaanSyn.AtSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
end;

procedure TSynBaanSyn.BraceCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynBaanSyn.BraceOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynBaanSyn.CRProc;
begin
  FTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: Inc(Run, 2);
  else
    Inc(Run);
  end;
end;

procedure TSynBaanSyn.ColonProc;
begin
  case FLine[Run + 1] of
    ':':                               {scope resolution operator}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {colon}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.CommaProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynBaanSyn.DirectiveProc;
begin
  FTokenID := tkDirective;
  repeat
    case FLine[Run] of
      #0, #10, #13:
        Break;
    end;
    Inc(Run);
  until FLine[Run] = #0;
end;

procedure TSynBaanSyn.EqualProc;
begin
  case FLine[Run + 1] of
    '=':                               {logical equal}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {assign}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.ErectProc;
begin
  Inc(Run, 1);                        {Bann Comments}
  FTokenID := tkComment;
  while FLine[Run] <> #0 do
  begin
    case FLine[Run] of
      #10, #13:
        Break;
    end; //case
    Inc(Run);
  end; //while
end;

procedure TSynBaanSyn.GreaterProc;
begin
  case FLine[Run + 1] of
    '=':                               {greater than or equal to}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
    '>':
      begin
        if FLine[Run + 2] = '=' then   {shift right assign}
          Inc(Run, 3)
        else                           {shift right}
          Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {greater than}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.IdentProc;
begin
  FTokenID := IdentKind(FLine + Run);
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do Inc(Run);
end;

procedure TSynBaanSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynBaanSyn.LowerProc;
begin
  case FLine[Run + 1] of
    '=':                               {less than or equal to}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
    '<':
      begin
        if FLine[Run + 2] = '=' then   {shift left assign}
          Inc(Run, 3)
        else                           {shift left}
          Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {less than}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.MinusProc;
begin
  case FLine[Run + 1] of
    '=':                               {subtract assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
    '-':                               {decrement}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
    '>':                               {arrow}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {subtract}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.ModSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {mod assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {mod}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.NotSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {not equal}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {not}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynBaanSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', '.', 'u', 'U', 'l', 'L', 'x', 'X', 'e', 'E', 'f', 'F':
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

procedure TSynBaanSyn.PlusProc;
begin
  case FLine[Run + 1] of
    '=':                               {add assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
    '+':                               {increment}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {subtract}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.RoundCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynBaanSyn.RoundOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynBaanSyn.SemiColonProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynBaanSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '=':                               {division assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {division}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynBaanSyn.SquareCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynBaanSyn.SquareOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynBaanSyn.StarProc;
begin
  case FLine[Run + 1] of
    '=':                               {multiply assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {star}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.StringProc;
begin
  FTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then Inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13:
        Break;
      #92:
        if FLine[Run + 1] = #10 then Inc(Run);
    end;
    Inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then Inc(Run);
end;

procedure TSynBaanSyn.TildeProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynBaanSyn.XOrSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {xor assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {xor}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynBaanSyn.Next;
begin
  FTokenPos := Run;
  case FLine[Run] of
    '&': AndSymbolProc;
    #39: AsciiCharProc;
    '@': AtSymbolProc;
    '}': BraceCloseProc;
    '{': BraceOpenProc;
    #13: CRProc;
    ':': ColonProc;
    ',': CommaProc;
    '#': DirectiveProc;
    '=': EqualProc;
    '|': ErectProc;
    '>': GreaterProc;
    'A'..'Z', 'a'..'z', '_', '.', '$': IdentProc;
    #10: LFProc;
    '<': LowerProc;
    '-': MinusProc;
    '%': ModSymbolProc;
    '!': NotSymbolProc;
    #0: NullProc;
    '0'..'9': NumberProc;
    '+': PlusProc;
    ')': RoundCloseProc;
    '(': RoundOpenProc;
    ';': SemiColonProc;
    '/': SlashProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    ']': SquareCloseProc;
    '[': SquareOpenProc;
    '*': StarProc;
    #34: StringProc;
    '~': TildeProc;
    '^': XOrSymbolProc;
    else UnknownProc;
  end;
  inherited;
end;

function TSynBaanSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynBaanSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynBaanSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynBaanSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := FCommentAttri;
    tkDirective: Result := FDirectiveAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkVariable: Result := FVariableAttri;
    tkUnknown: Result := FIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynBaanSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

function TSynBaanSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterBaan;
end;

function TSynBaanSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '.', '$', '_', '0'..'9', 'a'..'z', 'A'..'Z':
      Result := True;
    else
      Result := False;
  end;
end;

class function TSynBaanSyn.GetLanguageName: string;
begin
  Result := SYNS_LangBaan;
end;

class function TSynBaanSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangBaan;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynBaanSyn);
{$ENDIF}
end.
