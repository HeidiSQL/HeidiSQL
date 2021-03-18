{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterVBScript.pas, released 2000-04-18.
The Original Code is based on the lbVBSSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Luiz C. Vaz de Brito.
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

$Id: SynHighlighterVBScript.pas,v 1.14.2.7 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a VBScript highlighter for SynEdit)
@author(Luiz C. Vaz de Brito, converted to SynEdit by David Muir <david@loanhead45.freeserve.co.uk>)
@created(20 January 1999, converted to SynEdit April 18, 2000)
@lastmod(2000-06-23)
The SynHighlighterVBScript unit provides SynEdit with a VisualBasic Script (.vbs) highlighter.
Thanks to Primoz Gabrijelcic and Martin Waldenburg.
}

unit SynHighlighterVBScript;

{$I SynEdit.inc}

interface

uses
  Graphics,
  Registry,
  SynEditHighlighter,
  SynEditTypes,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkUnknown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynVBScriptSyn = class(TSynCustomHighLighter)
  private
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FIdentFuncTable: array[0..268] of TIdentFuncTableFunc;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function FuncRem(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure ApostropheProc;
    procedure CRProc;
    procedure DateProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
  protected
    function GetSampleSource: UnicodeString; override;
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
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri
      write FCommentAttri;
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
  end;

implementation

uses
  SynEditStrConst;

const
  KeyWords: array[0..83] of UnicodeString = (
    'and', 'as', 'boolean', 'byref', 'byte', 'byval', 'call', 'case', 'class', 
    'const', 'currency', 'debug', 'dim', 'do', 'double', 'each', 'else', 
    'elseif', 'empty', 'end', 'endif', 'enum', 'eqv', 'erase', 'error', 'event', 
    'exit', 'explicit', 'false', 'for', 'function', 'get', 'goto', 'if', 'imp', 
    'implements', 'in', 'integer', 'is', 'let', 'like', 'long', 'loop', 'lset',
    'me', 'mod', 'new', 'next', 'not', 'nothing', 'null', 'on', 'option', 
    'optional', 'or', 'paramarray', 'preserve', 'private', 'property', 'public', 
    'raiseevent', 'randomize', 'redim', 'rem', 'resume', 'rset', 'select', 
    'set', 'shared', 'single', 'static', 'stop', 'sub', 'then', 'to', 'true', 
    'type', 'typeof', 'until', 'variant', 'wend', 'while', 'with', 'xor' 
  );

  KeyIndices: array[0..268] of Integer = (
    -1, -1, -1, -1, -1, -1, -1, -1, 56, -1, 77, -1, 78, -1, 37, 19, 75, -1, -1, 
    -1, -1, -1, -1, -1, 12, -1, 66, -1, -1, -1, -1, -1, 35, -1, -1, -1, 46, 41, 
    36, -1, -1, 83, 33, 40, 34, -1, -1, -1, -1, 54, 24, 51, -1, -1, -1, -1, -1, 
    -1, -1, 76, -1, 68, -1, 1, -1, 7, -1, -1, 8, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 57, -1, 79, -1, -1, -1, 5, -1, -1, -1, -1, 4, -1, -1, -1, 43, 72, -1, 
    44, -1, -1, -1, -1, -1, -1, -1, 48, -1, -1, 69, -1, -1, 16, 70, 80, -1, 53, 
    47, 58, -1, -1, -1, -1, -1, -1, 63, -1, -1, -1, -1, 59, -1, 65, 39, -1, -1, 
    -1, -1, 6, -1, 55, -1, 67, -1, -1, -1, -1, -1, -1, 22, -1, -1, -1, 74, 50, 
    -1, -1, -1, 64, -1, -1, -1, -1, -1, -1, 31, 20, 23, -1, -1, 61, 27, 38, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 10, -1, 
    -1, -1, 3, -1, -1, 71, -1, -1, -1, -1, 11, 0, -1, -1, 82, 13, 15, 2, 30, 29, 
    14, -1, -1, 42, 49, 81, -1, 9, -1, -1, 62, 25, 60, -1, -1, 45, -1, -1, -1, 
    -1, -1, -1, -1, -1, 26, 28, -1, -1, -1, -1, 21, -1, -1, -1, -1, -1, -1, -1, 
    17, -1, -1, -1, -1, -1, 32, -1, -1, -1, -1, -1, -1, -1, -1, 52, 73, -1, -1, 
    18 
  );

{$Q-}
function TSynVBScriptSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 713 + Ord(Str^) * 134;
    Inc(Str);
  end;
  Result := Result mod 269;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynVBScriptSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynVBScriptSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[123] := FuncRem;

  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if @FIdentFuncTable[i] = nil then
      FIdentFuncTable[i] := KeyWordFunc;
end;

function TSynVBScriptSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynVBScriptSyn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

function TSynVBScriptSyn.FuncRem(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    ApostropheProc;
    FStringLen := 0;
    Result := tkComment;
  end
  else
    Result := tkIdentifier;
end;

constructor TSynVBScriptSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);
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
  SetAttributesOnChange(DefHighlightChange);
  FDefaultFilter := SYNS_FilterVBScript;
  InitIdent;
end;

procedure TSynVBScriptSyn.ApostropheProc;
begin
  FTokenID := tkComment;
  repeat
    Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynVBScriptSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then Inc(Run);
end;

procedure TSynVBScriptSyn.DateProc;
begin
  FTokenID := tkString;
  repeat
    if IsLineEnd(Run) then
      Break;
    Inc(Run);
  until FLine[Run] = '#';
  if not IsLineEnd(Run) then Inc(Run);
end;

procedure TSynVBScriptSyn.GreaterProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] = '=' then Inc(Run);
end;

procedure TSynVBScriptSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + Run));
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do Inc(Run);
end;

procedure TSynVBScriptSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynVBScriptSyn.LowerProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(FLine[Run], ['=', '>']) then Inc(Run);
end;

procedure TSynVBScriptSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynVBScriptSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', '.', 'e', 'E':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(Run);
  FTokenID := tkNumber;
  while IsNumberChar do Inc(Run);
end;

procedure TSynVBScriptSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynVBScriptSyn.StringProc;
begin
  FTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then Inc(Run, 2);
  repeat
    if IsLineEnd(Run) then
      Break;
    Inc(Run);
  until FLine[Run] = #34;
  if not IsLineEnd(Run) then Inc(Run);
end;

procedure TSynVBScriptSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynVBScriptSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkIdentifier;
end;

procedure TSynVBScriptSyn.Next;
begin
  FTokenPos := Run;
  case FLine[Run] of
    #39: ApostropheProc;
    #13: CRProc;
    '#': DateProc;
    '>': GreaterProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    #10: LFProc;
    '<': LowerProc;
    #0: NullProc;
    '0'..'9': NumberProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    #34: StringProc;
    '&', '{', '}', ':', ',', '=', '^', '-',
    '+', '.', '(', ')', ';', '/', '*': SymbolProc;
    else UnknownProc;
  end;
  inherited;
end;

function TSynVBScriptSyn.GetDefaultAttribute(Index: Integer):
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

function TSynVBScriptSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynVBScriptSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynVBScriptSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynVBScriptSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

function TSynVBScriptSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterVBScript;
end;

class function TSynVBScriptSyn.GetLanguageName: string;
begin
  Result := SYNS_LangVBSScript;
end;

function TSynVBScriptSyn.GetSampleSource: UnicodeString;
begin
  Result := ''' Syntax highlighting'#13#10 +
            'function printNumber()'#13#10 +
            '  number = 12345'#13#10 +
            '  document.write("The number is " + number)'#13#10 +
            '  for i = 0 to 10'#13#10 +
            '    x = x + 1.0'#13#10 +
            '  next'#13#10 +
            'end function';
end;

class function TSynVBScriptSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangVBSScript;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynVBScriptSyn);
{$ENDIF}
end.
