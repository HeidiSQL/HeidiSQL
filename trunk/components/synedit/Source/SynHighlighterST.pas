{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterST.pas, released 2002-07.
ST stands for Structured Text, and it is part of IEC1131 standard for
programming PLCs.
Author of this file is Ruggero Bandera.
Portions created by Ruggero Bandera are Copyright (C) 2002 Ruggero Bandera.
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

$Id: SynHighlighterST.pas,v 1.9.2.5 2005/11/27 22:22:45 maelh Exp $ by Ruggero Bandera

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNHIGHLIGHTERST}
unit SynHighlighterST;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
{$ELSE}
  Windows,
  Controls,
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkAsm, tkComment, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown);

  TRangeState = (rsANil, rsAnsi, rsAnsiAsm, rsAsm, rsBor, rsBorAsm, rsProperty,
    rsUnKnown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TSynSTSyn = class(TSynCustomHighlighter)
  private
    fAsmStart: Boolean;
    fRange: TRangeState;
    fIdentFuncTable: array[0..210] of TIdentFuncTableFunc;
    FTokenID: TtkTokenKind;
    fStringAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fAsmAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure AddressOpProc;
    procedure AsciiCharProc;
    procedure AnsiProc;
    procedure BorProc;
    procedure BraceOpenProc;
    procedure ColonOrGreaterProc;
    procedure CRProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PointProc;
    procedure RoundOpenProc;
    procedure SemicolonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
  protected
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: WideString; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: integer; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
  published
    property AsmAttri: TSynHighlighterAttributes read fAsmAttri write fAsmAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  KeyWords: array[0..74] of WideString = (
    'action', 'and', 'any', 'any_num', 'array', 'at', 'bool', 'by', 'byte', 
    'case', 'configuration', 'constant', 'dint', 'do', 'dword', 'else', 'elsif', 
    'end_action', 'end_case', 'end_configuration', 'end_for', 'end_if', 
    'end_repeat', 'end_resource', 'end_step', 'end_struct', 'end_transition', 
    'end_type', 'end_var', 'end_while', 'exit', 'external', 'finally', 'for', 
    'from', 'function', 'goto', 'if', 'index', 'initial_step', 'initialization', 
    'int', 'label', 'not', 'of', 'on', 'or', 'program', 'real', 'repeat', 
    'resource', 'retain', 'return', 'sint', 'step', 'string', 'struct', 'then', 
    'time', 'to', 'transition', 'type', 'udint', 'uint', 'until', 'usint', 
    'var', 'var_external', 'var_global', 'var_in_out', 'var_input', 
    'var_output', 'while', 'word', 'xor' 
  );

  KeyIndices: array[0..210] of Integer = (
    -1, -1, -1, -1, -1, 55, 39, -1, -1, -1, -1, 51, -1, -1, -1, -1, 57, 49, 4, 
    -1, 17, -1, -1, -1, -1, -1, -1, 24, -1, -1, -1, -1, -1, -1, 61, -1, -1, -1, 
    47, -1, -1, -1, 58, 70, 38, -1, -1, 35, -1, -1, -1, 28, 12, -1, -1, -1, -1, 
    -1, -1, 64, -1, -1, 1, -1, -1, 69, 27, 45, -1, 2, -1, -1, -1, 3, 9, -1, 37, 
    13, 63, -1, -1, 8, -1, -1, -1, -1, -1, 60, -1, -1, -1, -1, -1, -1, -1, -1, 
    10, -1, -1, -1, -1, -1, -1, -1, -1, -1, 18, 25, 20, -1, 53, 14, -1, -1, -1, 
    0, -1, -1, 26, 41, 42, 62, -1, -1, -1, 66, 21, 36, -1, -1, 30, -1, 73, 22, 
    -1, 16, -1, -1, -1, -1, 74, -1, -1, 23, -1, 29, 50, -1, -1, -1, -1, -1, 68, 
    -1, -1, 19, -1, 15, 11, -1, 48, -1, 72, -1, 43, -1, -1, -1, -1, 67, 31, -1, 
    32, -1, -1, 6, -1, -1, 7, 65, -1, -1, 33, -1, -1, -1, -1, -1, -1, -1, 5, -1, 
    40, 52, 34, -1, -1, -1, -1, -1, -1, -1, 56, -1, -1, 44, 54, -1, 71, 46, 59 
  );

{$Q-}
function TSynSTSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 381 + Ord(Str^) * 141;
    inc(Str);
  end;
  Result := Result mod 211;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynSTSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Key: Cardinal;
begin
  fToIdent := MayBe;
  Key := HashKey(MayBe);
  if Key <= High(fIdentFuncTable) then
    Result := fIdentFuncTable[Key](KeyIndices[Key])
  else
    Result := tkIdentifier;
end;

procedure TSynSTSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if @fIdentFuncTable[i] = nil then
      fIdentFuncTable[i] := KeyWordFunc;
end;

function TSynSTSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier
end;

function TSynSTSyn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

constructor TSynSTSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  fAsmAttri := TSynHighlighterAttributes.Create(SYNS_AttrAssembler, SYNS_FriendlyAttrAssembler);
  AddAttribute(fAsmAttri);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);

  InitIdent;
  fRange := rsUnknown;
  fAsmStart := False;
  fDefaultFilter := SYNS_FilterST;
end; { Create }

procedure TSynSTSyn.AddressOpProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '@' then inc(Run);
end;

procedure TSynSTSyn.AsciiCharProc;
begin
  fTokenID := tkString;
  inc(Run);
  while FLine[Run] in [WideChar('0')..WideChar('9')] do inc(Run);
end;

procedure TSynSTSyn.BorProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    else begin
      fTokenID := tkComment;
      repeat
        if fLine[Run] = '}' then begin
          Inc(Run);
          if fRange = rsBorAsm then
            fRange := rsAsm
          else
            fRange := rsUnKnown;
          break;
        end;
        Inc(Run);
      until IsLineEnd(Run);
    end;
  end;
end;

procedure TSynSTSyn.BraceOpenProc;
begin
  if fRange = rsAsm then
    fRange := rsBorAsm
  else
    fRange := rsBor;
  BorProc;
end;

procedure TSynSTSyn.ColonOrGreaterProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '=' then inc(Run);
end;

procedure TSynSTSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then inc(Run);
end;

procedure TSynSTSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do inc(Run);
end;

procedure TSynSTSyn.IntegerProc;

  function IsIntegerChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  inc(Run);
  fTokenID := tkNumber;
  while IsIntegerChar do inc(Run);
end;

procedure TSynSTSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynSTSyn.LowerProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in [WideChar('='), WideChar('>')] then inc(Run);
end;

procedure TSynSTSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynSTSyn.NumberProc;

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
  inc(Run);
  fTokenID := tkNumber;
  while IsNumberChar do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSynSTSyn.PointProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in [WideChar('.'), WideChar(')')] then inc(Run);
end;

procedure TSynSTSyn.AnsiProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = ')') then begin
        Inc(Run, 2);
        if fRange = rsAnsiAsm then
          fRange := rsAsm
        else
          fRange := rsUnKnown;
        break;
      end;
      Inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynSTSyn.RoundOpenProc;
begin
  Inc(Run);
  case fLine[Run] of
    '*':
      begin
        Inc(Run);
        if fRange = rsAsm then
          fRange := rsAnsiAsm
        else
          fRange := rsAnsi;
        fTokenID := tkComment;
        if not IsLineEnd(Run) then
          AnsiProc;
      end;
    '.':
      begin
        inc(Run);
        fTokenID := tkSymbol;
      end;
  else
    fTokenID := tkSymbol;
  end;
end;

procedure TSynSTSyn.SemicolonProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fRange = rsProperty then
    fRange := rsUnknown;
end;

procedure TSynSTSyn.SlashProc;
begin
  Inc(Run);
  if fLine[Run] = '/' then begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end else
    fTokenID := tkSymbol;
end;

procedure TSynSTSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynSTSyn.StringProc;
begin
  fTokenID := tkString;
  Inc(Run);
  while not IsLineEnd(Run) do
  begin
    if fLine[Run] = #39 then
    begin
      Inc(Run);
      if fLine[Run] <> #39 then
        break;
    end;
    Inc(Run);
  end;
end;

procedure TSynSTSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSTSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynSTSyn.Next;
begin
  fAsmStart := False;
  fTokenPos := Run;
  case fRange of
    rsAnsi, rsAnsiAsm:
      AnsiProc;
    rsBor, rsBorAsm:
      BorProc;
  else
    case fLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      '#': AsciiCharProc;
      '$': IntegerProc;
      #39: StringProc;
      '0'..'9': NumberProc;
      'A'..'Z', 'a'..'z', '_': IdentProc;
      '{': BraceOpenProc;
      '}', '!', '"', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~':
        begin
          case fLine[Run] of
            '(': RoundOpenProc;
            '.': PointProc;
            ';': SemicolonProc;
            '/': SlashProc;
            ':', '>': ColonOrGreaterProc;
            '<': LowerProc;
            '@': AddressOpProc;
          else
            SymbolProc;
          end;
        end;
      else
        UnknownProc;
    end;
  end;
  inherited;
end;

function TSynSTSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynSTSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynSTSyn.GetTokenID: TtkTokenKind;
begin
  if not fAsmStart and (fRange = rsAsm)
    and not (fTokenId in [tkNull, tkComment, tkSpace])
  then
    Result := tkAsm
  else
    Result := fTokenId;
end;

function TSynSTSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkAsm: Result := fAsmAttri;
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynSTSyn.GetTokenKind: integer;
begin
  Result := Ord(GetTokenID);
end;

function TSynSTSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

procedure TSynSTSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynSTSyn.ResetRange;
begin
  fRange:= rsUnknown;
end;

class function TSynSTSyn.GetLanguageName: string;
begin
  Result := SYNS_LangST;
end;

function TSynSTSyn.IsFilterStored: boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterST;
end;

class function TSynSTSyn.GetFriendlyLanguageName: WideString;
begin
  Result := SYNS_FriendlyLangST;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynSTSyn);
{$ENDIF}
end.
