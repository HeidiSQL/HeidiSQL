{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: SynHighlighterGo.pas, released 2017-06-01.
Description: Syntax Parser/Highlighter
The initial author of this file is Christian-W. Budde.
Copyright (c) 2017, all rights reserved.

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

unit SynHighlighterGo;

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
    tkFloat,
    tkKey,
    tkNull,
    tkNumber,
    tkSpace,
    tkString,
    tkSymbol,
    tkUnknown);

  TRangeState = (rsUnknown, rsSingleString, rsDoubleString, rsExtraString);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynGoSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..88] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function HashKey(Str: PWideChar): Cardinal;
    function FuncBool(Index: Integer): TtkTokenKind;
    function FuncBreak(Index: Integer): TtkTokenKind;
    function FuncByte(Index: Integer): TtkTokenKind;
    function FuncCase(Index: Integer): TtkTokenKind;
    function FuncChan(Index: Integer): TtkTokenKind;
    function FuncComplex128(Index: Integer): TtkTokenKind;
    function FuncComplex64(Index: Integer): TtkTokenKind;
    function FuncConst(Index: Integer): TtkTokenKind;
    function FuncContinue(Index: Integer): TtkTokenKind;
    function FuncDefault(Index: Integer): TtkTokenKind;
    function FuncDefer(Index: Integer): TtkTokenKind;
    function FuncElse(Index: Integer): TtkTokenKind;
    function FuncFallthrough(Index: Integer): TtkTokenKind;
    function FuncFloat32(Index: Integer): TtkTokenKind;
    function FuncFloat64(Index: Integer): TtkTokenKind;
    function FuncFor(Index: Integer): TtkTokenKind;
    function FuncFunc(Index: Integer): TtkTokenKind;
    function FuncGo(Index: Integer): TtkTokenKind;
    function FuncGoto(Index: Integer): TtkTokenKind;
    function FuncIf(Index: Integer): TtkTokenKind;
    function FuncImport(Index: Integer): TtkTokenKind;
    function FuncInt(Index: Integer): TtkTokenKind;
    function FuncInt16(Index: Integer): TtkTokenKind;
    function FuncInt32(Index: Integer): TtkTokenKind;
    function FuncInt64(Index: Integer): TtkTokenKind;
    function FuncInt8(Index: Integer): TtkTokenKind;
    function FuncInterface(Index: Integer): TtkTokenKind;
    function FuncMap(Index: Integer): TtkTokenKind;
    function FuncPackage(Index: Integer): TtkTokenKind;
    function FuncRange(Index: Integer): TtkTokenKind;
    function FuncReturn(Index: Integer): TtkTokenKind;
    function FuncRune(Index: Integer): TtkTokenKind;
    function FuncSelect(Index: Integer): TtkTokenKind;
    function FuncString(Index: Integer): TtkTokenKind;
    function FuncStruct(Index: Integer): TtkTokenKind;
    function FuncSwitch(Index: Integer): TtkTokenKind;
    function FuncType(Index: Integer): TtkTokenKind;
    function FuncUint(Index: Integer): TtkTokenKind;
    function FuncUint16(Index: Integer): TtkTokenKind;
    function FuncUint32(Index: Integer): TtkTokenKind;
    function FuncUint64(Index: Integer): TtkTokenKind;
    function FuncUint8(Index: Integer): TtkTokenKind;
    function FuncUintptr(Index: Integer): TtkTokenKind;
    function FuncVar(Index: Integer): TtkTokenKind;
    procedure IdentProc;
    procedure UnknownProc;
    function AltFunc(Index: Integer): TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure CRProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
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
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  // as this language is case-insensitive keywords *must* be in lowercase
  KeyWords: array[0..43] of UnicodeString = (
    'bool', 'break', 'byte', 'case', 'chan', 'complex128', 'complex64', 'const', 
    'continue', 'default', 'defer', 'else', 'fallthrough', 'float32', 'float64', 
    'for', 'func', 'go', 'goto', 'if', 'import', 'int', 'int16', 'int32', 
    'int64', 'int8', 'interface', 'map', 'package', 'range', 'return', 'rune', 
    'select', 'string', 'struct', 'switch', 'type', 'uint', 'uint16', 'uint32', 
    'uint64', 'uint8', 'uintptr', 'var' 
  );

  KeyIndices: array[0..88] of Integer = (
    29, 24, -1, -1, -1, 7, -1, 37, 11, -1, 22, 14, -1, 21, 1, -1, -1, -1, 39, 
    31, -1, -1, -1, 28, -1, -1, 17, -1, -1, -1, 32, 30, 26, 41, 23, -1, 34, -1, 
    -1, -1, -1, -1, -1, 9, 13, 20, -1, 35, -1, 3, -1, -1, 15, -1, -1, 25, -1, 
    27, 4, 6, -1, 5, -1, -1, 10, 2, 16, -1, -1, -1, 43, -1, -1, 8, 40, 36, 33, 
    -1, -1, -1, -1, 18, 19, 38, -1, 42, 12, -1, 0 
  );

procedure TSynGoSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[88] := FuncBool;
  fIdentFuncTable[14] := FuncBreak;
  fIdentFuncTable[65] := FuncByte;
  fIdentFuncTable[49] := FuncCase;
  fIdentFuncTable[58] := FuncChan;
  fIdentFuncTable[61] := FuncComplex128;
  fIdentFuncTable[59] := FuncComplex64;
  fIdentFuncTable[5] := FuncConst;
  fIdentFuncTable[73] := FuncContinue;
  fIdentFuncTable[43] := FuncDefault;
  fIdentFuncTable[64] := FuncDefer;
  fIdentFuncTable[8] := FuncElse;
  fIdentFuncTable[86] := FuncFallthrough;
  fIdentFuncTable[44] := FuncFloat32;
  fIdentFuncTable[11] := FuncFloat64;
  fIdentFuncTable[52] := FuncFor;
  fIdentFuncTable[66] := FuncFunc;
  fIdentFuncTable[26] := FuncGo;
  fIdentFuncTable[81] := FuncGoto;
  fIdentFuncTable[82] := FuncIf;
  fIdentFuncTable[45] := FuncImport;
  fIdentFuncTable[13] := FuncInt;
  fIdentFuncTable[10] := FuncInt16;
  fIdentFuncTable[34] := FuncInt32;
  fIdentFuncTable[1] := FuncInt64;
  fIdentFuncTable[55] := FuncInt8;
  fIdentFuncTable[32] := FuncInterface;
  fIdentFuncTable[57] := FuncMap;
  fIdentFuncTable[23] := FuncPackage;
  fIdentFuncTable[0] := FuncRange;
  fIdentFuncTable[31] := FuncReturn;
  fIdentFuncTable[19] := FuncRune;
  fIdentFuncTable[30] := FuncSelect;
  fIdentFuncTable[76] := FuncString;
  fIdentFuncTable[36] := FuncStruct;
  fIdentFuncTable[47] := FuncSwitch;
  fIdentFuncTable[75] := FuncType;
  fIdentFuncTable[7] := FuncUint;
  fIdentFuncTable[83] := FuncUint16;
  fIdentFuncTable[18] := FuncUint32;
  fIdentFuncTable[74] := FuncUint64;
  fIdentFuncTable[33] := FuncUint8;
  fIdentFuncTable[85] := FuncUintptr;
  fIdentFuncTable[70] := FuncVar;
end;

{$Q-}
function TSynGoSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 345 + Ord(Str^) * 670;
    inc(Str);
  end;
  Result := Result mod 89;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynGoSyn.FuncBool(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncBreak(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncByte(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncCase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncChan(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncComplex128(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncComplex64(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncConst(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncContinue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncDefault(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncDefer(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncElse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncFallthrough(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncFloat32(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncFloat64(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncFor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncGo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncGoto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncIf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncImport(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncInt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncInt16(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncInt32(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncInt64(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncInt8(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncInterface(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncMap(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncPackage(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncRange(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncReturn(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncRune(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncSelect(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncString(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncStruct(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncSwitch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncType(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncUint(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncUint16(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncUint32(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncUint64(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncUint8(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncUintptr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.FuncVar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynGoSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynGoSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynGoSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynGoSyn.StringProc;
var
  StringChar: WideChar;
begin
  fTokenID := tkString;
  StringChar := fLine[Run];
  repeat
    inc(Run);
  until IsLineEnd(Run) or (fLine[Run] = StringChar);

  if FLine[Run] = #34 then
    inc(Run);
end;

procedure TSynGoSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynGoSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
    inc(Run);
end;

procedure TSynGoSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynGoSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '/':
      begin
        fTokenID := tkComment;
        inc(Run, 2);
        while not IsLineEnd(Run) do Inc(Run);
      end;
    '*':
      begin
        fTokenID := tkComment;
        inc(Run, 2);
        while fLine[Run] <> #0 do
          case fLine[Run] of
            '*':
              if fLine[Run + 1] = '/' then
              begin
                inc(Run, 2);
                break;
              end else inc(Run);
            #10, #13:
                break;
          else inc(Run);
          end;
      end;
    '=':
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

constructor TSynGoSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCaseSensitive := False;

  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clNavy;
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fNumberAttri := TSynHighLighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  fNumberAttri.Foreground := $666666;
  AddAttribute(fNumberAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Foreground := $214195;
  AddAttribute(fKeyAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Foreground := $619121;
  AddAttribute(fStringAttri);

  fSymbolAttri := TSynHighLighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  fSymbolAttri.Foreground := $666666;
  AddAttribute(fSymbolAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterGo;
  fRange := rsUnknown;
end;

procedure TSynGoSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', 'e', 'E', '-', '+':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(Run);
  fTokenID := tkNumber;
  while IsNumberChar do
  begin
    case fLine[Run] of
      '.':
        if fLine[Run + 1] = '.' then
          Break
        else
          fTokenID := tkFloat;
      'e', 'E': fTokenID := tkFloat;
      '-', '+':
        begin
          if fTokenID <> tkFloat then // arithmetic
            Break;
          if (FLine[Run - 1] <> 'e') and (FLine[Run - 1] <> 'E') then
            Break; //float, but it ends here
        end;
    end;
    Inc(Run);
  end;
end;

procedure TSynGoSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do inc(Run);
end;

procedure TSynGoSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynGoSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynGoSyn.Next;
begin
  fTokenPos := Run;
  case fLine[Run] of
    #0:
      NullProc;
    #10:
      LFProc;
    #13:
      CRProc;
    '/':
      SlashProc;
    #34:
      StringProc;
    #39:
      StringProc;
    #180:
      StringProc;
    #1..#9, #11, #12, #14..#32:
      SpaceProc;
    '0'..'9':
      NumberProc;
    'A'..'Z', 'a'..'z', '_':
      IdentProc;
    ':', '=', '+', '-', '.', ',':
      SymbolProc;
    '(', ')', '[', ']', '{', '}':
      UnknownProc;
  else
    UnknownProc;
  end;

  inherited;
end;

function TSynGoSyn.GetDefaultAttribute(Index: Integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynGoSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynGoSyn.GetKeyWords(TokenKind: Integer): UnicodeString;
begin
  Result := 
    'bool,break,byte,case,chan,complex128,complex64,const,continue,default' +
    ',defer,else,fallthrough,float32,float64,for,func,go,goto,if,import,int' +
    ',int16,int32,int64,int8,interface,map,package,range,return,rune,select' +
    ',string,struct,switch,type,uint,uint16,uint32,uint64,uint8,uintptr,var';
end;

function TSynGoSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynGoSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment:
      Result := fCommentAttri;
    tkIdentifier:
      Result := fIdentifierAttri;
    tkNumber, tkFloat:
      Result := fNumberAttri;
    tkKey:
      Result := fKeyAttri;
    tkSpace:
      Result := fSpaceAttri;
    tkString:
      Result := fStringAttri;
    tkSymbol:
      Result := fSymbolAttri;
    tkUnknown:
      Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynGoSyn.GetTokenKind: Integer;
begin
  Result := Ord(fTokenId);
end;

function TSynGoSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', '0'..'9', 'a'..'z', 'A'..'Z':
      Result := True;
    else
      Result := False;
  end;
end;

function TSynGoSyn.GetSampleSource: UnicodeString;
begin
  Result := 
    #13#10 +
    '/* Sample source for the go highlighter */'#13#10 +
    #13#10 +
    'package main'#13#10 +
    #13#10 +
    'import "fmt"'#13#10 +
    #13#10 +
    'func main() {'#13#10 +
    '  fmt.Println("hello world")'#13#10+'}';
end;

function TSynGoSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterGo;
end;

class function TSynGoSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangGo;
end;

class function TSynGoSyn.GetLanguageName: string;
begin
  Result := SYNS_LangGo;
end;

procedure TSynGoSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynGoSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynGoSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynGoSyn);
{$ENDIF}
end.
