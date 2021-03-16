{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterFortran.pas, released 2000-04-21.
The Original Code is based on the mwFortranSyn.pas file from the
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

$Id: SynHighlighterFortran.pas,v 1.15.2.9 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Fortran syntax highlighter for SynEdit)
@author(riceball <teditor@mailroom.com>, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(2000, converted to SynEdit 2000-04-21)
@lastmod(2000-06-23)
The SynHighlighterFortran unit provides SynEdit with a Fortran syntax highlighter.
Thanks to Martin Waldenburg.
}

unit SynHighlighterFortran;

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
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkUnknown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynFortranSyn = class(TSynCustomHighlighter)
  private
    FTokenID: TtkTokenKind;
    FIdentFuncTable: array[0..192] of TIdentFuncTableFunc;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure CommaProc;
    procedure EqualProc;
    procedure ExclamationProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PlusProc;
    procedure PointProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StarProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure CommentProc;
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
  KeyWords: array[0..69] of UnicodeString = (
    'allocatable', 'allocate', 'allocated', 'associated', 'call', 'case', 
    'character', 'close', 'common', 'complex', 'contains', 'continue', 'cycle', 
    'data', 'deallocate', 'default', 'define', 'dimension', 'do', 'else', 
    'elseif', 'elsewhere', 'end', 'enddo', 'endif', 'entry', 'equivalence', 
    'exit', 'external', 'forall', 'format', 'function', 'if', 'implicit', 
    'include', 'integer', 'interface', 'logical', 'map', 'module', 'namelist', 
    'nullify', 'open', 'optional', 'parameter', 'pause', 'pointer', 'print', 
    'private', 'program', 'public', 'pure', 'read', 'real', 'record', 'return', 
    'save', 'select', 'stop', 'subroutine', 'target', 'then', 'type', 'union', 
    'use', 'value', 'volatile', 'where', 'while', 'write' 
  );

  KeyIndices: array[0..192] of Integer = (
    8, -1, -1, -1, -1, 11, -1, -1, -1, 31, 2, -1, -1, 59, -1, -1, -1, -1, -1, 
    13, 55, -1, -1, -1, 65, -1, 38, 54, 40, 10, 37, -1, -1, 25, -1, -1, 5, -1, 
    -1, -1, -1, -1, -1, 4, -1, -1, 21, -1, -1, 49, -1, -1, -1, -1, 9, -1, -1, 
    27, -1, 22, -1, 6, -1, -1, -1, -1, -1, -1, -1, -1, 64, -1, -1, 53, 68, -1, 
    34, -1, -1, 69, 30, -1, -1, -1, 32, -1, -1, -1, 19, 16, -1, -1, -1, -1, -1, 
    -1, -1, 62, -1, -1, -1, -1, -1, -1, 36, 60, 14, -1, -1, 66, 29, -1, -1, -1, 
    -1, 24, -1, 67, -1, 15, -1, -1, -1, -1, -1, -1, 44, 35, -1, -1, 46, -1, 17, 
    -1, -1, 28, -1, 56, 61, -1, -1, 63, 45, 18, -1, 0, 20, -1, -1, -1, -1, -1, 
    -1, 42, -1, 50, 3, 58, 52, -1, -1, -1, 51, -1, 48, -1, -1, -1, -1, -1, -1, 
    -1, -1, 12, 23, -1, 26, 1, -1, 41, 43, -1, -1, -1, 33, 7, -1, -1, -1, 47, 
    39, 57, -1 
  );

{$Q-}
function TSynFortranSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 294 + Ord(Str^) * 110;
    Inc(Str);
  end;
  Result := Result mod 193;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynFortranSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynFortranSyn.InitIdent;
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

function TSynFortranSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynFortranSyn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

constructor TSynFortranSyn.Create(AOwner: TComponent);
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
  InitIdent;
  FDefaultFilter := SYNS_FilterFortran;
end;

procedure TSynFortranSyn.AsciiCharProc;
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

procedure TSynFortranSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then Inc(Run);
end;

procedure TSynFortranSyn.CommaProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynFortranSyn.EqualProc;
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

procedure TSynFortranSyn.ExclamationProc;
begin
  Inc(Run, 1);                        {Fortran Comments}
  FTokenID := tkComment;
  while FLine[Run] <> #0 do
  begin
    case FLine[Run] of
      #10, #13:
        Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynFortranSyn.GreaterProc;
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

procedure TSynFortranSyn.IdentProc;
begin
  if CharInSet(FLine[Run], ['C', 'c']) and (Run = 0) then
  begin   //Fortran comments
    Inc(Run, 1);
    CommentProc;
  end
  else begin
    FTokenID := IdentKind(FLine + Run);
    Inc(Run, FStringLen);
    while IsIdentChar(FLine[Run]) do Inc(Run);
  end;
end;

procedure TSynFortranSyn.LFProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
end;

procedure TSynFortranSyn.LowerProc;
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

procedure TSynFortranSyn.MinusProc;
begin
  {subtract}
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynFortranSyn.ModSymbolProc;
begin
  {mod}
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynFortranSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynFortranSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', '.', 'x', 'X', 'e', 'E', 'f', 'F':
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

procedure TSynFortranSyn.PlusProc;
begin
  {subtract}
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynFortranSyn.PointProc;
begin
  if (((SynWideUpperCase(FLine[Run + 1]) = 'G') and CharInSet(SynWideUpperCase(FLine[Run + 2])[1], ['E', 'T'])) {.ge. .gt.}
       or ((SynWideUpperCase(FLine[Run + 1]) = 'L') and CharInSet(SynWideUpperCase(FLine[Run + 2])[1], ['E', 'T'])) {.le. .lt.}
       or ((SynWideUpperCase(FLine[Run + 1]) = 'N') and (SynWideUpperCase(FLine[Run + 2]) = 'E')) {.ne.}
       or ((SynWideUpperCase(FLine[Run + 1]) = 'E') and (SynWideUpperCase(FLine[Run + 2]) = 'Q')) {.eq.}
       or ((SynWideUpperCase(FLine[Run + 1]) = 'O') and (SynWideUpperCase(FLine[Run + 2]) = 'R'))){.or.}
     and (FLine[Run + 3] = '.') then
    begin
      Inc(Run, 4);
      FTokenID := tkSymbol;
    end
  else if (((SynWideUpperCase(FLine[Run + 1]) = 'A')
              and (SynWideUpperCase(FLine[Run + 2]) = 'N')
              and (SynWideUpperCase(FLine[Run + 3]) = 'D'))    {.and.}
           or ((SynWideUpperCase(FLine[Run + 1]) = 'N')
              and (SynWideUpperCase(FLine[Run + 2]) = 'O')
              and (SynWideUpperCase(FLine[Run + 3]) = 'T')))    {.not.}
          and (FLine[Run + 4] = '.') then
    begin
      Inc(Run, 5);
      FTokenID := tkSymbol;
    end
  else if (SynWideUpperCase(FLine[Run + 1]) = 'T')
          and (SynWideUpperCase(FLine[Run + 2]) = 'R')
          and (SynWideUpperCase(FLine[Run + 3]) = 'U')
          and (SynWideUpperCase(FLine[Run + 4]) = 'E')
          and (FLine[Run + 5] = '.') then  {.true.}
    begin
      Inc(Run, 6);
      FTokenID := tkSymbol;
    end
  else if (SynWideUpperCase(FLine[Run + 1]) = 'F')
          and (SynWideUpperCase(FLine[Run + 2]) = 'A')
          and (SynWideUpperCase(FLine[Run + 3]) = 'L')
          and (SynWideUpperCase(FLine[Run + 4]) = 'S')
          and (SynWideUpperCase(FLine[Run + 5]) = 'E')
          and (FLine[Run + 6] = '.') then  {.false.}
    begin
      Inc(Run, 7);
      FTokenID := tkSymbol;
    end
  else                                 {point}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
end;

procedure TSynFortranSyn.RoundCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynFortranSyn.RoundOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynFortranSyn.SemiColonProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynFortranSyn.SlashProc;
begin
  {division}
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynFortranSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynFortranSyn.StarProc;
begin
  if (Run = 0) then begin   //Fortran comments
    Inc(Run);
    CommentProc;
  end
  else begin
    {star}
    Inc(Run);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynFortranSyn.CommentProc;
begin
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

procedure TSynFortranSyn.StringProc;
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

procedure TSynFortranSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynFortranSyn.Next;
begin
  FTokenPos := Run;
  case FLine[Run] of
    #39: AsciiCharProc;
    #13: CRProc;
    ',': CommaProc;
    '=': EqualProc;
    '!': ExclamationProc;
    '>': GreaterProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    #10: LFProc;
    '<': LowerProc;
    '-': MinusProc;
    '%': ModSymbolProc;
    #0: NullProc;
    '0'..'9': NumberProc;
    '+': PlusProc;
    '.': PointProc;
    ')': RoundCloseProc;
    '(': RoundOpenProc;
    ';': SemiColonProc;
    '/': SlashProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    '*': StarProc;
    #34: StringProc;
    else UnknownProc;
  end;
  inherited;
end;

function TSynFortranSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynFortranSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynFortranSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynFortranSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
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

function TSynFortranSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

function TSynFortranSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterFortran;
end;

class function TSynFortranSyn.GetLanguageName: string;
begin
  Result := SYNS_LangFortran;
end;

class function TSynFortranSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangFortran;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynFortranSyn);
{$ENDIF}
end.
