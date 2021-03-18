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

unit SynHighlighterLDraw;

{$I SynEdit.inc}

interface

uses
  Windows, Controls, Graphics,
  SynEditHighlighter, SynEditTypes,
  SynUnicode,
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

  TRangeState = (rsUnknown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynLDRSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FIdentFuncTable: array[0..1] of TIdentFuncTableFunc;
    FColorAttri: TSynHighlighterAttributes;
    FCommentAttri: TSynHighlighterAttributes;
    FFirstTriAttri: TSynHighlighterAttributes;
    FFourthTriAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FLineAttri: TSynHighlighterAttributes;
    FOpLineAttri: TSynHighlighterAttributes;
    FQuadAttri: TSynHighlighterAttributes;
    FSecondTriAttri: TSynHighlighterAttributes;
    FThirdTriAttri: TSynHighlighterAttributes;
    FTriangleAttri: TSynHighlighterAttributes;
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
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetKeyWords(TokenKind: Integer): UnicodeString; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property ColorAttri: TSynHighlighterAttributes read FColorAttri write FColorAttri;
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
    property FirstTriAttri: TSynHighlighterAttributes read FFirstTriAttri write FFirstTriAttri;
    property FourthTriAttri: TSynHighlighterAttributes read FFourthTriAttri write FFourthTriAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property LineAttri: TSynHighlighterAttributes read FLineAttri write FLineAttri;
    property OpLineAttri: TSynHighlighterAttributes read FOpLineAttri write FOpLineAttri;
    property QuadAttri: TSynHighlighterAttributes read FQuadAttri write FQuadAttri;
    property SecondTriAttri: TSynHighlighterAttributes read FSecondTriAttri write FSecondTriAttri;
    property ThirdTriAttri: TSynHighlighterAttributes read FThirdTriAttri write FThirdTriAttri;
    property TriangleAttri: TSynHighlighterAttributes read FTriangleAttri write FTriangleAttri;
  end;

implementation

uses
  SynEditStrConst;

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
    Inc(Str);
  end;
  Result := Result mod 2;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynLDRSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynLDRSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[1] := FuncAuthor;
end;

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
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynLDRSyn.CRProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynLDRSyn.LFProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

constructor TSynLDRSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorAttri := TSynHighLighterAttributes.Create(SYNS_AttrColor, SYNS_FriendlyAttrColor);
  FColorAttri.Foreground := clNavy;
  AddAttribute(FColorAttri);

  FCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clBlue;
  AddAttribute(FCommentAttri);

  FFirstTriAttri := TSynHighLighterAttributes.Create(SYNS_AttrFirstTri, SYNS_FriendlyAttrFirstTri);
  FFirstTriAttri.Foreground := RGB(206,111,73);
  AddAttribute(FFirstTriAttri);

  FFourthTriAttri := TSynHighLighterAttributes.Create(SYNS_AttrFourthTri, SYNS_FriendlyAttrFourthTri);
  FFourthTriAttri.Foreground := RGB(54,99,12);
  AddAttribute(FFourthTriAttri);

  FIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FLineAttri := TSynHighLighterAttributes.Create(SYNS_AttrLine, SYNS_FriendlyAttrLine);
  FLineAttri.Foreground := clBlack;
  AddAttribute(FLineAttri);

  FOpLineAttri := TSynHighLighterAttributes.Create(SYNS_AttrOpLine, SYNS_FriendlyAttrOpLine);
  FOpLineAttri.Foreground := clBlack;
  AddAttribute(FOpLineAttri);

  FQuadAttri := TSynHighLighterAttributes.Create(SYNS_AttrQuad, SYNS_FriendlyAttrQuad);
  FQuadAttri.Foreground := clRed;
  AddAttribute(FQuadAttri);

  FSecondTriAttri := TSynHighLighterAttributes.Create(SYNS_AttrSecondTri, SYNS_FriendlyAttrSecondTri);
  FSecondTriAttri.Foreground := RGB(54,99,12);
  AddAttribute(FSecondTriAttri);

  FThirdTriAttri := TSynHighLighterAttributes.Create(SYNS_AttrThirdTri, SYNS_FriendlyAttrThirdTri);
  FThirdTriAttri.Foreground := RGB(206,111,73);
  AddAttribute(FThirdTriAttri);

  FTriangleAttri := TSynHighLighterAttributes.Create(SYNS_AttrTriangle, SYNS_FriendlyAttrTriangle);
  FTriangleAttri.Foreground := clBlack;
  AddAttribute(FTriangleAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  FDefaultFilter := SYNS_FilterLDraw;
  FRange := rsUnknown;
end;

function TSynLDRSyn.FirstChar(DatLine: PWideChar): WideChar;
var
  i: Integer;
begin
  i := 0;
  while DatLine[i] = ' ' do Inc(i);
  Result := DatLine[i];
end;

procedure TSynLDRSyn.IdentProc;
begin
  if FirstChar(FLine) = '0' then
  begin
    FTokenID := tkComment;
    while (FLine[Run] <> #10) and (FLine[Run] <> #13)
          and (FLine[Run] <> #0) do Inc(Run);
  end
  else
  begin
    FTokenID := IdentKind((FLine + Run));
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do
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
        Inc(i);
        b := False;
      end
      else
      begin
        if not b then Inc(Result);
        b := True;
        Inc(i)
      end;
    end;
  end;

begin
  case ArgNumber(FLine) of
    1: begin
         case FLine[Run] of
           '0': FTokenID := tkComment;
           '1': FTokenID := tkIdentifier;
           '2': FTokenID := tkLine;
           '3': FTokenID := tkTriangle;
           '4': FTokenID := tkQuad;
           '5': FTokenID := tkOpLine;
         end;
       end; 
    2: if FirstChar(FLine) <> '0' then FTokenID := tkColor
         else FTokenID := tkComment;
    3..5: if FirstChar(FLine) <> '0' then FTokenID := tkFirstTri
            else FTokenID := tkComment;
    6..8: if FirstChar(FLine) <> '0' then FTokenID := tkSecondTri
            else FTokenID := tkComment;
    9..11: if FirstChar(FLine) <> '0' then FTokenID := tkThirdTri
             else FTokenID := tkComment;
    12..14: if FirstChar(FLine) <> '0' then FTokenID := tkFourthTri
             else FTokenID := tkComment;
    else
      FTokenID := tkIdentifier;
  end;
  while CharInSet(FLine[Run], ['0'..'9', '.']) do Inc(Run);
end;

procedure TSynLDRSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynLDRSyn.Next;
begin
  FTokenPos := Run;
  case FLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    '0'..'9': Number1Proc;
    else UnknownProc;
  end;
  inherited;
end;

function TSynLDRSyn.GetDefaultAttribute(Index: Integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
  else
    Result := nil;
  end;
end;

function TSynLDRSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynLDRSyn.GetKeyWords(TokenKind: Integer): UnicodeString;
begin
  Result := 'Author';
end;

function TSynLDRSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynLDRSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkColor: Result := FColorAttri;
    tkComment: Result := FCommentAttri;
    tkFirstTri: Result := FFirstTriAttri;
    tkFourthTri: Result := FFourthTriAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkLine: Result := FLineAttri;
    tkOpLine: Result := FOpLineAttri;
    tkQuad: Result := FQuadAttri;
    tkSecondTri: Result := FSecondTriAttri;
    tkThirdTri: Result := FThirdTriAttri;
    tkTriangle: Result := FTriangleAttri;
    tkUnknown: Result := FIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynLDRSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
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
  Result := FDefaultFilter <> SYNS_FilterLDraw;
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
  FRange := rsUnknown;
end;

procedure TSynLDRSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynLDRSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
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
