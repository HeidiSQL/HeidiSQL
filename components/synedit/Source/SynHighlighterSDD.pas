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

$Id: SynHighlighterSDD.pas,v 1.13.2.6 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

unit SynHighlighterSDD;

{$I SynEdit.inc}

interface

uses
  Windows,
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
    tkDatatype,
    tkNumber,
    tkNull,
    tkSpace,
    tkSymbol,
    tkUnknown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TRangeState = (rsComment, rsUnknown);

type
  TSynSDDSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FIdentFuncTable: array[0..36] of TIdentFuncTableFunc;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FDatatypeAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
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
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;   
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
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
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property DatatypeAttri: TSynHighlighterAttributes read FDatatypeAttri write FDatatypeAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  KeyWords: array[0..26] of UnicodeString = (
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
    Inc(Str);
  end;
  Result := Result mod 37;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynSDDSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynSDDSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[3] := FuncArray;
  FIdentFuncTable[20] := FuncBinarydata;
  FIdentFuncTable[14] := FuncBlock;
  FIdentFuncTable[1] := FuncByte;
  FIdentFuncTable[29] := FuncDatabase;
  FIdentFuncTable[8] := FuncDate;
  FIdentFuncTable[30] := FuncEnd;
  FIdentFuncTable[27] := FuncEndblock;
  FIdentFuncTable[0] := FuncInteger;
  FIdentFuncTable[24] := FuncKeys;
  FIdentFuncTable[10] := FuncLongint;
  FIdentFuncTable[26] := FuncMemotext;
  FIdentFuncTable[19] := FuncObject;
  FIdentFuncTable[32] := FuncObjects;
  FIdentFuncTable[5] := FuncOf;
  FIdentFuncTable[22] := FuncOwner;
  FIdentFuncTable[6] := FuncPartition;
  FIdentFuncTable[21] := FuncPartitions;
  FIdentFuncTable[2] := FuncPrimary;
  FIdentFuncTable[9] := FuncReal;
  FIdentFuncTable[11] := FuncSecondary;
  FIdentFuncTable[17] := FuncSpec;
  FIdentFuncTable[7] := FuncString;
  FIdentFuncTable[36] := FuncSuperblock;
  FIdentFuncTable[35] := FuncSuperspec;
  FIdentFuncTable[4] := FuncTime;
  FIdentFuncTable[15] := FuncVar;
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

  FCaseSensitive := False;

  FCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clNavy;
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  FKeyAttri.Foreground := clGreen;
  AddAttribute(FKeyAttri);

  FDatatypeAttri := TSynHighlighterAttributes.Create(SYNS_AttrDataType, SYNS_FriendlyAttrDataType);
  FDatatypeAttri.Style := [fsBold];
  FDatatypeAttri.Foreground := clTeal;
  AddAttribute(FDatatypeAttri);

  FSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FNumberAttri := TSynHighLighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clBlue;
  AddAttribute(FNumberAttri);

  FSymbolAttri := TSynHighLighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  FDefaultFilter := SYNS_FilterSDD;
  FRange := rsUnknown;
end; { Create }

procedure TSynSDDSyn.BraceOpenProc;
begin
  FRange := rsComment;
  BraceCommentProc;
  FTokenID := tkComment;
end; { BraceOpenProc }

procedure TSynSDDSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + Run));
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
end; { IdentProc }

procedure TSynSDDSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end; { NullProc }

procedure TSynSDDSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until not CharInSet(FLine[Run], [#1..#32]);
end; { SpaceProc }

procedure TSynSDDSyn.BraceCommentProc;
begin
  case FLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      FTokenID := tkComment;
      repeat
        if FLine[Run] = '}' then
        begin
          Inc(Run);
          FRange := rsUnknown;
          Break;
        end;
        Inc(Run);
      until IsLineEnd(Run);
    end;
  end;
end; { BraceCommentProc }

procedure TSynSDDSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end; { UnknownProc }

procedure TSynSDDSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsComment: BraceCommentProc;
  else
    case FLine[Run] of
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
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end; { CRProc }

procedure TSynSDDSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end; { LFProc }

function TSynSDDSyn.GetSampleSource: UnicodeString;
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
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end; { GetDefaultAttribute }

function TSynSDDSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end; { GetEol }

function TSynSDDSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end; { GetTokenId }

function TSynSDDSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkDatatype: Result := FDatatypeAttri;
    tkSpace: Result := FSpaceAttri;
    tkNumber: Result := FNumberAttri;
    tkUnknown: Result := FIdentifierAttri;
    tkSymbol: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end; { GetTokenAttribute }

function TSynSDDSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end; { GetTokenKind }

procedure TSynSDDSyn.ResetRange;
begin
  inherited;
  FRange := rsUnknown;
end; { ResetRange }

procedure TSynSDDSyn.SetRange(Value: Pointer);
begin
  inherited;
  FRange := TRangeState(Value);
end; { SetRange }

function TSynSDDSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end; { GetRange }

class function TSynSDDSyn.GetLanguageName: string;
begin
  Result := SYNS_LangSDD;
end; { GetLanguageName }

procedure TSynSDDSyn.NumberProc;

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
      '.': if FLine[Run + 1] = '.' then
             Break;
    end;
    Inc(Run);
  end;
end; { NumberProc }

function TSynSDDSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterSDD;
end; { IsFilterStored }

procedure TSynSDDSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

class function TSynSDDSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangSDD;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynSDDSyn);
{$ENDIF}
end.
