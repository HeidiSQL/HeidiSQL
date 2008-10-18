{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: SynHighlighterLDraw.pas, released 2003-04-12.
Description: LDraw Parser/Highlighter
The initial author of this file is Orion Pobursky.
Copyright (c) 2003, all rights reserved.
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

$Id: SynHighlighterLDraw.pas,v 1.7.2.7 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}
{
@abstract(Provides an LDraw syntax highlighter for SynEdit)
@author(Orion Pobursky)
@created(03/01/2003)
@lastmod(07/05/2003)
The SynHighlighterLDraw unit provides SynEdit with a LEGO LDraw (.ldr / .dat) highlighter.
}

{$IFNDEF QSYNHIGHLIGHTERLDRAW}
unit SynHighlighterLDraw;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  Qt, QControls, QGraphics,
  QSynEditHighlighter,
  QSynEditTypes,
  QSynUnicode,
{$ELSE}
  Windows, Controls, Graphics,
  SynEditHighlighter, SynEditTypes,
  SynUnicode,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (
    tkColor,
    tkComment,
    tkFirstTri,
    tkFourthTri,
    tkIdentifier,
    tkKey,
    tkLine,
    tkNull,
    tkOpLine,
    tkQuad,
    tkSecondTri,
    tkThirdTri,
    tkTriangle,
    tkUnknown);

  TRangeState = (rsUnKnown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynLDRSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..1] of TIdentFuncTableFunc;
    fColorAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fFirstTriAttri: TSynHighlighterAttributes;
    fFourthTriAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fLineAttri: TSynHighlighterAttributes;
    fOpLineAttri: TSynHighlighterAttributes;
    fQuadAttri: TSynHighlighterAttributes;
    fSecondTriAttri: TSynHighlighterAttributes;
    fThirdTriAttri: TSynHighlighterAttributes;
    fTriangleAttri: TSynHighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function FuncAuthor(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure IdentProc;
    procedure Number1Proc;
    procedure UnknownProc;
    procedure NullProc;
    procedure CRProc;
    procedure LFProc;
    function FirstChar(DatLine: PWideChar): WideChar;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetKeyWords(TokenKind: Integer): UnicodeString; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property ColorAttri: TSynHighlighterAttributes read fColorAttri write fColorAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property FirstTriAttri: TSynHighlighterAttributes read fFirstTriAttri write fFirstTriAttri;
    property FourthTriAttri: TSynHighlighterAttributes read fFourthTriAttri write fFourthTriAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property LineAttri: TSynHighlighterAttributes read fLineAttri write fLineAttri;
    property OpLineAttri: TSynHighlighterAttributes read fOpLineAttri write fOpLineAttri;
    property QuadAttri: TSynHighlighterAttributes read fQuadAttri write fQuadAttri;
    property SecondTriAttri: TSynHighlighterAttributes read fSecondTriAttri write fSecondTriAttri;
    property ThirdTriAttri: TSynHighlighterAttributes read fThirdTriAttri write fThirdTriAttri;
    property TriangleAttri: TSynHighlighterAttributes read fTriangleAttri write fTriangleAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  KeyWords: array[0..0] of UnicodeString = (
    'author' 
  );

  KeyIndices: array[0..1] of Integer = (
    -1, 0 
  );

{$Q-}
function TSynLDRSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result + Ord(Str^);
    inc(Str);
  end;
  Result := Result mod 2;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynLDRSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynLDRSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[1] := FuncAuthor;
end;

{$IFDEF SYN_CLX}
function RGB(const R, G, B: Byte):  TColor;
begin
  Result := R or (G shl 8) or (B shl 16)
end;
{$ENDIF}

function TSynLDRSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynLDRSyn.FuncAuthor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

procedure TSynLDRSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynLDRSyn.CRProc;
begin
  fTokenID := tkUnknown;
  inc(Run);
  if fLine[Run] = #10 then
    inc(Run);
end;

procedure TSynLDRSyn.LFProc;
begin
  fTokenID := tkUnknown;
  inc(Run);
end;

constructor TSynLDRSyn.Create(AOwner: TComponent);

  {$IFDEF SYN_KYLIX}
  function RGB(r, g, b: Byte): LongWord;
  begin
    Result := (r or (g shl 8) or (b shl 16));
  end;
  {$ENDIF}

begin
  inherited Create(AOwner);
  fColorAttri := TSynHighLighterAttributes.Create(SYNS_AttrColor, SYNS_FriendlyAttrColor);
  fColorAttri.Foreground := clNavy;
  AddAttribute(fColorAttri);

  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Foreground := clBlue;
  AddAttribute(fCommentAttri);

  fFirstTriAttri := TSynHighLighterAttributes.Create(SYNS_AttrFirstTri, SYNS_FriendlyAttrFirstTri);
  fFirstTriAttri.Foreground := RGB(206,111,73);
  AddAttribute(fFirstTriAttri);

  fFourthTriAttri := TSynHighLighterAttributes.Create(SYNS_AttrFourthTri, SYNS_FriendlyAttrFourthTri);
  fFourthTriAttri.Foreground := RGB(54,99,12);
  AddAttribute(fFourthTriAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);

  fLineAttri := TSynHighLighterAttributes.Create(SYNS_AttrLine, SYNS_FriendlyAttrLine);
  fLineAttri.Foreground := clBlack;
  AddAttribute(fLineAttri);

  fOpLineAttri := TSynHighLighterAttributes.Create(SYNS_AttrOpLine, SYNS_FriendlyAttrOpLine);
  fOpLineAttri.Foreground := clBlack;
  AddAttribute(fOpLineAttri);

  fQuadAttri := TSynHighLighterAttributes.Create(SYNS_AttrQuad, SYNS_FriendlyAttrQuad);
  fQuadAttri.Foreground := clRed;
  AddAttribute(fQuadAttri);

  fSecondTriAttri := TSynHighLighterAttributes.Create(SYNS_AttrSecondTri, SYNS_FriendlyAttrSecondTri);
  fSecondTriAttri.Foreground := RGB(54,99,12);
  AddAttribute(fSecondTriAttri);

  fThirdTriAttri := TSynHighLighterAttributes.Create(SYNS_AttrThirdTri, SYNS_FriendlyAttrThirdTri);
  fThirdTriAttri.Foreground := RGB(206,111,73);
  AddAttribute(fThirdTriAttri);

  fTriangleAttri := TSynHighLighterAttributes.Create(SYNS_AttrTriangle, SYNS_FriendlyAttrTriangle);
  fTriangleAttri.Foreground := clBlack;
  AddAttribute(fTriangleAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterLDraw;
  fRange := rsUnknown;
end;

function TSynLDRSyn.FirstChar(DatLine: PWideChar): WideChar;
var
  i: Integer;
begin
  i := 0;
  while DatLine[i] = ' ' do inc(i);
  Result := DatLine[i];
end;

procedure TSynLDRSyn.IdentProc;
begin
  if FirstChar(fLine) = '0' then
  begin
    fTokenID := tkComment;
    while (fLine[Run] <> #10) and (fLine[Run] <> #13)
          and (fLine[Run] <> #0) do inc(Run);
  end
  else
  begin
    fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do
    Inc(Run);
  end;
end;

procedure TSynLDRSyn.Number1Proc;

  function ArgNumber(DatLine: PWideChar): Byte;
  var
   i: Integer;
   b: Boolean;
  begin
    i := 0;
    Result := 0;
    b := False;
    while i <= Run do
    begin
      if DatLine[i] = ' ' then
      begin
        inc(i);
        b := False;
      end
      else
      begin
        if not b then inc(Result);
        b := True;
        inc(i)
      end;
    end;
  end;

begin
  case ArgNumber(fLine) of
    1: begin
         case fLine[Run] of
           '0': fTokenID := tkComment;
           '1': fTokenID := tkIdentifier;
           '2': fTokenID := tkLine;
           '3': fTokenID := tkTriangle;
           '4': fTokenID := tkQuad;
           '5': fTokenID := tkOpLine;
         end;
       end; 
    2: if FirstChar(fLine) <> '0' then fTokenID := tkColor 
         else fTokenID := tkComment; 
    3..5: if FirstChar(fLine) <> '0' then fTokenID := tkFirstTri
            else fTokenID := tkComment; 
    6..8: if FirstChar(fLine) <> '0' then fTokenID := tkSecondTri
            else fTokenID := tkComment; 
    9..11: if FirstChar(fLine) <> '0' then fTokenID := tkThirdTri
             else fTokenID := tkComment; 
    12..14: if FirstChar(fLine) <> '0' then fTokenID := tkFourthTri
             else fTokenID := tkComment; 
    else
      fTokenID := tkIdentifier;
  end;
  while CharInSet(FLine[Run], ['0'..'9', '.']) do inc(Run);
end;

procedure TSynLDRSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynLDRSyn.Next;
begin
  fTokenPos := Run;
  case fLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    '0'..'9': Number1Proc;
    else UnknownProc;
  end;
  inherited;
end;

function TSynLDRSyn.GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
  else
    Result := nil;
  end;
end;

function TSynLDRSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynLDRSyn.GetKeyWords(TokenKind: Integer): UnicodeString;
begin
  Result := 'Author';
end;

function TSynLDRSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynLDRSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkColor: Result := fColorAttri;
    tkComment: Result := fCommentAttri;
    tkFirstTri: Result := fFirstTriAttri;
    tkFourthTri: Result := fFourthTriAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkLine: Result := fLineAttri;
    tkOpLine: Result := fOpLineAttri;
    tkQuad: Result := fQuadAttri;
    tkSecondTri: Result := fSecondTriAttri;
    tkThirdTri: Result := fThirdTriAttri;
    tkTriangle: Result := fTriangleAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynLDRSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynLDRSyn.GetSampleSource: UnicodeString;
begin
  Result := #13#10 +
            'Sample source for: '#13#10 +
            'Ldraw Parser/Highlighter'#13#10 +
            '0 Comment'#13#10 +
            '1 16 0 0 0 1 0 0 0 1 0 0 0 1 stud.dat'#13#10 +
            '2 16 0 0 0 1 1 1'#13#10 +
            '3 16 0 0 0 1 1 1 2 2 2'#13#10 +
            '4 16 0 0 0 1 1 1 2 2 2 3 3 3'#13#10 +
            '5 16 0 0 0 1 1 1 2 2 2 3 3 3';
end;

function TSynLDRSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterLDraw;
end;

function TSynLDRSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', 'A'..'Z', 'a'..'z':
      Result := True;
    else
      Result := False;
  end;
end;

class function TSynLDRSyn.GetLanguageName: string;
begin
  Result := SYNS_LangLDraw;
end;

procedure TSynLDRSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynLDRSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynLDRSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

class function TSynLDRSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangLDraw;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynLDRSyn);
{$ENDIF}
end.
