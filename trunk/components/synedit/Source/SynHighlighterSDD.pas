{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterSDD.pas, released 2001-08-20.
The Initial Author of this file is Pieter Polak.
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

$Id: SynHighlighterSDD.pas,v 1.13.2.5 2005/11/27 22:22:45 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

{$IFNDEF QSYNHIGHLIGHTERSDD}
unit SynHighlighterSDD;
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
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (
    tkComment,
    tkIdentifier,
    tkKey,
    tkDatatype,
    tkNumber,
    tkNull,
    tkSpace,
    tkSymbol,
    tkUnknown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TRangeState = (rsComment, rsUnKnown);

type
  TSynSDDSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..36] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fDatatypeAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function FuncArray(Index: Integer): TtkTokenKind;
    function FuncBinarydata(Index: Integer): TtkTokenKind;
    function FuncBlock(Index: Integer): TtkTokenKind;
    function FuncByte(Index: Integer): TtkTokenKind;
    function FuncDatabase(Index: Integer): TtkTokenKind;
    function FuncDate(Index: Integer): TtkTokenKind;
    function FuncEnd(Index: Integer): TtkTokenKind;
    function FuncEndblock(Index: Integer): TtkTokenKind;
    function FuncInteger(Index: Integer): TtkTokenKind;
    function FuncKeys(Index: Integer): TtkTokenKind;
    function FuncLongint(Index: Integer): TtkTokenKind;
    function FuncMemotext(Index: Integer): TtkTokenKind;
    function FuncObject(Index: Integer): TtkTokenKind;
    function FuncObjects(Index: Integer): TtkTokenKind;
    function FuncOf(Index: Integer): TtkTokenKind;
    function FuncOwner(Index: Integer): TtkTokenKind;
    function FuncPartition(Index: Integer): TtkTokenKind;
    function FuncPartitions(Index: Integer): TtkTokenKind;
    function FuncPrimary(Index: Integer): TtkTokenKind;
    function FuncReal(Index: Integer): TtkTokenKind;
    function FuncSecondary(Index: Integer): TtkTokenKind;
    function FuncSpec(Index: Integer): TtkTokenKind;
    function FuncString(Index: Integer): TtkTokenKind;
    function FuncSuperblock(Index: Integer): TtkTokenKind;
    function FuncSuperspec(Index: Integer): TtkTokenKind;
    function FuncTime(Index: Integer): TtkTokenKind;
    function FuncVar(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure BraceOpenProc;
    procedure BraceCommentProc;
    procedure NumberProc;                                                    
    procedure CRProc;
    procedure LFProc;
    procedure IdentProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure UnknownProc;
    procedure SymbolProc;
  protected
    function GetSampleSource: WideString; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: WideString; override;   
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property DatatypeAttri: TSynHighlighterAttributes read fDatatypeAttri write fDatatypeAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  KeyWords: array[0..26] of WideString = (
    'array', 'binarydata', 'block', 'byte', 'database', 'date', 'end',
    'endblock', 'integer', 'keys', 'longint', 'memotext', 'object', 'objects',
    'of', 'owner', 'partition', 'partitions', 'primary', 'real', 'secondary',
    'spec', 'string', 'superblock', 'superspec', 'time', 'var'
  );

  KeyIndices: array[0..36] of Integer = (
    8, 3, 18, 0, 25, 14, 16, 22, 5, 19, 10, 20, -1, -1, 2, 26, -1, 21, -1, 12,
    1, 17, 15, -1, 9, -1, 11, 7, -1, 4, 6, -1, 13, -1, -1, 24, 23
  );

{$Q-}
function TSynSDDSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 813 + Ord(Str^) * 168;
    inc(Str);
  end;
  Result := Result mod 37;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynSDDSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynSDDSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[3] := FuncArray;
  fIdentFuncTable[20] := FuncBinarydata;
  fIdentFuncTable[14] := FuncBlock;
  fIdentFuncTable[1] := FuncByte;
  fIdentFuncTable[29] := FuncDatabase;
  fIdentFuncTable[8] := FuncDate;
  fIdentFuncTable[30] := FuncEnd;
  fIdentFuncTable[27] := FuncEndblock;
  fIdentFuncTable[0] := FuncInteger;
  fIdentFuncTable[24] := FuncKeys;
  fIdentFuncTable[10] := FuncLongint;
  fIdentFuncTable[26] := FuncMemotext;
  fIdentFuncTable[19] := FuncObject;
  fIdentFuncTable[32] := FuncObjects;
  fIdentFuncTable[5] := FuncOf;
  fIdentFuncTable[22] := FuncOwner;
  fIdentFuncTable[6] := FuncPartition;
  fIdentFuncTable[21] := FuncPartitions;
  fIdentFuncTable[2] := FuncPrimary;
  fIdentFuncTable[9] := FuncReal;
  fIdentFuncTable[11] := FuncSecondary;
  fIdentFuncTable[17] := FuncSpec;
  fIdentFuncTable[7] := FuncString;
  fIdentFuncTable[36] := FuncSuperblock;
  fIdentFuncTable[35] := FuncSuperspec;
  fIdentFuncTable[4] := FuncTime;
  fIdentFuncTable[15] := FuncVar;
end;

function TSynSDDSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynSDDSyn.FuncArray(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncBinarydata(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncBlock(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncByte(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncDatabase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncDate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncEnd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncEndblock(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncInteger(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncKeys(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncLongint(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncMemotext(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncObject(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncObjects(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncOf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncOwner(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncPartition(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncPartitions(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncPrimary(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncReal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncSecondary(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncSpec(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncString(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncSuperblock(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncSuperspec(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncTime(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynSDDSyn.FuncVar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

constructor TSynSDDSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Foreground := clNavy;
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := clGreen;
  AddAttribute(fKeyAttri);

  fDatatypeAttri := TSynHighlighterAttributes.Create(SYNS_AttrDataType, SYNS_FriendlyAttrDataType);
  fDatatypeAttri.Style := [fsBold];
  fDatatypeAttri.Foreground := clTeal;
  AddAttribute(fDatatypeAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fNumberAttri := TSynHighLighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  fNumberAttri.Foreground := clBlue;
  AddAttribute(fNumberAttri);

  fSymbolAttri := TSynHighLighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterSDD;
  fRange := rsUnknown;
end; { Create }

procedure TSynSDDSyn.BraceOpenProc;
begin
  fRange := rsComment;
  BraceCommentProc;
  fTokenID := tkComment;
end; { BraceOpenProc }

procedure TSynSDDSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do
    Inc(Run);
end; { IdentProc }

procedure TSynSDDSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end; { NullProc }

procedure TSynSDDSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    inc(Run);
  until not (fLine[Run] in [WideChar(#1)..WideChar(#32)]);
end; { SpaceProc }

procedure TSynSDDSyn.BraceCommentProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      fTokenID := tkComment;
      repeat
        if fLine[Run] = '}' then
        begin
          Inc(Run);
          fRange := rsUnKnown;
          Break;
        end;
        Inc(Run);
      until IsLineEnd(Run);
    end;
  end;
end; { BraceCommentProc }

procedure TSynSDDSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end; { UnknownProc }

procedure TSynSDDSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsComment: BraceCommentProc;
  else
    case fLine[Run] of
      '{': BraceOpenProc;
      '}', '!', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~': SymbolProc;
      'A'..'Z', 'a'..'z', '_': IdentProc;
      '0'..'9' : NumberProc;
      #0: NullProc;
      #1..#32: SpaceProc;
      else UnknownProc;
    end;
  end;
  inherited;
end; { Next }

procedure TSynSDDSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then
    inc(Run);
end; { CRProc }

procedure TSynSDDSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end; { LFProc }

function TSynSDDSyn.GetSampleSource: WideString;
begin
  Result := '{ Semanta data dictionary }'#13#10 +
            'database Sample.001;'#13#10 +
            'owner = COAS;'#13#10 +
            #13#10 +
            'objects'#13#10 +
            '  Test = object'#13#10 +
            '    Code : string[4];'#13#10 +
            '    Name : string[80];'#13#10 +
            '  end;'#13#10 +
            'keys'#13#10 +
            '  primary Test.Index = [Code];'#13#10 +
            'end.';
end; { GetSampleSource }

function TSynSDDSyn.GetDefaultAttribute(Index: Integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end; { GetDefaultAttribute }

function TSynSDDSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end; { GetEol }

function TSynSDDSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end; { GetTokenId }

function TSynSDDSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkDatatype: Result := fDatatypeAttri;
    tkSpace: Result := fSpaceAttri;
    tkNumber: Result := fNumberAttri;
    tkUnknown: Result := fIdentifierAttri;
    tkSymbol: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end; { GetTokenAttribute }

function TSynSDDSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end; { GetTokenKind }

procedure TSynSDDSyn.ResetRange;
begin
  inherited;
  fRange := rsUnknown;
end; { ResetRange }

procedure TSynSDDSyn.SetRange(Value: Pointer);
begin
  inherited;
  fRange := TRangeState(Value);
end; { SetRange }

function TSynSDDSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end; { GetRange }

class function TSynSDDSyn.GetLanguageName: string;
begin
  Result := SYNS_LangSDD;
end; { GetLanguageName }

procedure TSynSDDSyn.NumberProc;

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
      '.': if FLine[Run + 1] = '.' then
             Break;
    end;
    inc(Run);
  end;
end; { NumberProc }

function TSynSDDSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterSDD;
end; { IsFilterStored }

procedure TSynSDDSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

class function TSynSDDSyn.GetFriendlyLanguageName: WideString;
begin
  Result := SYNS_FriendlyLangSDD;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynSDDSyn);
{$ENDIF}
end.
