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

$Id: SynHighlighterST.pas,v 1.9.2.6 2008/09/14 16:25:03 maelh Exp $ by Ruggero Bandera

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynHighlighterST;

{$I SynEdit.inc}

interface

uses
  Windows,
  Controls,
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkAsm, tkComment, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown);

  TRangeState = (rsANil, rsAnsi, rsAnsiAsm, rsAsm, rsBor, rsBorAsm, rsProperty,
    rsUnknown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TSynSTSyn = class(TSynCustomHighlighter)
  private
    FAsmStart: Boolean;
    FRange: TRangeState;
    FIdentFuncTable: array[0..210] of TIdentFuncTableFunc;
    FTokenID: TtkTokenKind;
    FStringAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FAsmAttri: TSynHighlighterAttributes;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
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
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
  published
    property AsmAttri: TSynHighlighterAttributes read FAsmAttri write FAsmAttri;
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
  KeyWords: array[0..74] of UnicodeString = (
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
    Inc(Str);
  end;
  Result := Result mod 211;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynSTSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynSTSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if @FIdentFuncTable[i] = nil then
      FIdentFuncTable[i] := KeyWordFunc;
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

  FCaseSensitive := False;

  FAsmAttri := TSynHighlighterAttributes.Create(SYNS_AttrAssembler, SYNS_FriendlyAttrAssembler);
  AddAttribute(FAsmAttri);
  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style:= [fsItalic];
  AddAttribute(FCommentAttri);
  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style:= [fsBold];
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

  InitIdent;
  FRange := rsUnknown;
  FAsmStart := False;
  FDefaultFilter := SYNS_FilterST;
end; { Create }

procedure TSynSTSyn.AddressOpProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] = '@' then Inc(Run);
end;

procedure TSynSTSyn.AsciiCharProc;
begin
  FTokenID := tkString;
  Inc(Run);
  while CharInSet(FLine[Run], ['0'..'9']) do Inc(Run);
end;

procedure TSynSTSyn.BorProc;
begin
  case FLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    else begin
      FTokenID := tkComment;
      repeat
        if FLine[Run] = '}' then begin
          Inc(Run);
          if FRange = rsBorAsm then
            FRange := rsAsm
          else
            FRange := rsUnknown;
          Break;
        end;
        Inc(Run);
      until IsLineEnd(Run);
    end;
  end;
end;

procedure TSynSTSyn.BraceOpenProc;
begin
  if FRange = rsAsm then
    FRange := rsBorAsm
  else
    FRange := rsBor;
  BorProc;
end;

procedure TSynSTSyn.ColonOrGreaterProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] = '=' then Inc(Run);
end;

procedure TSynSTSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then Inc(Run);
end;

procedure TSynSTSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + Run));
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do Inc(Run);
end;

procedure TSynSTSyn.IntegerProc;

  function IsIntegerChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(Run);
  FTokenID := tkNumber;
  while IsIntegerChar do Inc(Run);
end;

procedure TSynSTSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynSTSyn.LowerProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(FLine[Run], ['=', '>']) then Inc(Run);
end;

procedure TSynSTSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynSTSyn.NumberProc;

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
  while IsNumberChar do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynSTSyn.PointProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(FLine[Run], ['.', ')']) then Inc(Run);
end;

procedure TSynSTSyn.AnsiProc;
begin
  case FLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    FTokenID := tkComment;
    repeat
      if (FLine[Run] = '*') and (FLine[Run + 1] = ')') then begin
        Inc(Run, 2);
        if FRange = rsAnsiAsm then
          FRange := rsAsm
        else
          FRange := rsUnknown;
        Break;
      end;
      Inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynSTSyn.RoundOpenProc;
begin
  Inc(Run);
  case FLine[Run] of
    '*':
      begin
        Inc(Run);
        if FRange = rsAsm then
          FRange := rsAnsiAsm
        else
          FRange := rsAnsi;
        FTokenID := tkComment;
        if not IsLineEnd(Run) then
          AnsiProc;
      end;
    '.':
      begin
        Inc(Run);
        FTokenID := tkSymbol;
      end;
  else
    FTokenID := tkSymbol;
  end;
end;

procedure TSynSTSyn.SemicolonProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  if FRange = rsProperty then
    FRange := rsUnknown;
end;

procedure TSynSTSyn.SlashProc;
begin
  Inc(Run);
  if FLine[Run] = '/' then begin
    FTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end else
    FTokenID := tkSymbol;
end;

procedure TSynSTSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynSTSyn.StringProc;
begin
  FTokenID := tkString;
  Inc(Run);
  while not IsLineEnd(Run) do
  begin
    if FLine[Run] = #39 then
    begin
      Inc(Run);
      if FLine[Run] <> #39 then
        Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynSTSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynSTSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynSTSyn.Next;
begin
  FAsmStart := False;
  FTokenPos := Run;
  case FRange of
    rsAnsi, rsAnsiAsm:
      AnsiProc;
    rsBor, rsBorAsm:
      BorProc;
  else
    case FLine[Run] of
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
          case FLine[Run] of
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
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynSTSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynSTSyn.GetTokenID: TtkTokenKind;
begin
  if not FAsmStart and (FRange = rsAsm)
    and not (FTokenID in [tkNull, tkComment, tkSpace])
  then
    Result := tkAsm
  else
    Result := FTokenID;
end;

function TSynSTSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkAsm: Result := FAsmAttri;
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynSTSyn.GetTokenKind: Integer;
begin
  Result := Ord(GetTokenID);
end;

function TSynSTSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

procedure TSynSTSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynSTSyn.ResetRange;
begin
  FRange:= rsUnknown;
end;

class function TSynSTSyn.GetLanguageName: string;
begin
  Result := SYNS_LangST;
end;

function TSynSTSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterST;
end;

class function TSynSTSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangST;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynSTSyn);
{$ENDIF}
end.
