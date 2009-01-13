{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterBat.pas, released 2000-04-18.
The Original Code is based on the dmBatSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is David H. Muir.
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

$Id: SynHighlighterBat.pas,v 1.14.2.6 2008/09/14 16:24:59 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a MS-DOS Batch file highlighter for SynEdit)
@author(David Muir <dhm@dmsoftware.co.uk>)
@created(Late 1999)
@lastmod(May 19, 2000)
The SynHighlighterBat unit provides SynEdit with a MS-DOS Batch file (.bat) highlighter.
The highlighter supports the formatting of keywords and parameters (batch file arguments).
}

{$IFNDEF QSYNHIGHLIGHTERBAT}
unit SynHighlighterBat;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
  QSynUnicode,
{$ELSE}
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkUnknown, tkVariable);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynBatSyn = class(TSynCustomHighlighter)
  private
    fIdentFuncTable: array[0..24] of TIdentFuncTableFunc;
    FTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function FuncCall(Index: Integer): TtkTokenKind;
    function FuncCd(Index: Integer): TtkTokenKind;
    function FuncCls(Index: Integer): TtkTokenKind;
    function FuncCopy(Index: Integer): TtkTokenKind;
    function FuncDel(Index: Integer): TtkTokenKind;
    function FuncDo(Index: Integer): TtkTokenKind;
    function FuncEcho(Index: Integer): TtkTokenKind;
    function FuncErrorlevel(Index: Integer): TtkTokenKind;
    function FuncExist(Index: Integer): TtkTokenKind;
    function FuncFor(Index: Integer): TtkTokenKind;
    function FuncGoto(Index: Integer): TtkTokenKind;
    function FuncIf(Index: Integer): TtkTokenKind;
    function FuncIn(Index: Integer): TtkTokenKind;
    function FuncNot(Index: Integer): TtkTokenKind;
    function FuncOff(Index: Integer): TtkTokenKind;
    function FuncOn(Index: Integer): TtkTokenKind;
    function FuncPause(Index: Integer): TtkTokenKind;
    function FuncSet(Index: Integer): TtkTokenKind;
    function FuncShift(Index: Integer): TtkTokenKind;
    function FuncStart(Index: Integer): TtkTokenKind;
    function FuncTitle(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure VariableProc;
    procedure CRProc;
    procedure CommentProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure REMCommentProc;
    procedure SpaceProc;
    procedure UnknownProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
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
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property VariableAttri: TSynHighlighterAttributes read fVariableAttri
      write fVariableAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  KeyWords: array[0..20] of UnicodeString = (
    'call', 'cd', 'cls', 'copy', 'del', 'do', 'echo', 'errorlevel', 'exist', 
    'for', 'goto', 'if', 'in', 'not', 'off', 'on', 'pause', 'set', 'shift', 
    'start', 'title' 
  );

  KeyIndices: array[0..24] of Integer = (
    14, 4, -1, 6, 17, 12, 8, 18, 19, 15, -1, -1, 10, 3, 13, 0, 1, 11, 20, 7, 2, 
    5, -1, 16, 9 
  );

{$Q-}
function TSynBatSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 869 + Ord(Str^) * 61;
    inc(Str);
  end;
  Result := Result mod 25;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynBatSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynBatSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;
      
  fIdentFuncTable[15] := FuncCall;
  fIdentFuncTable[16] := FuncCd;
  fIdentFuncTable[20] := FuncCls;
  fIdentFuncTable[13] := FuncCopy;
  fIdentFuncTable[1] := FuncDel;
  fIdentFuncTable[21] := FuncDo;
  fIdentFuncTable[3] := FuncEcho;
  fIdentFuncTable[19] := FuncErrorlevel;
  fIdentFuncTable[6] := FuncExist;
  fIdentFuncTable[24] := FuncFor;
  fIdentFuncTable[12] := FuncGoto;
  fIdentFuncTable[17] := FuncIf;
  fIdentFuncTable[5] := FuncIn;
  fIdentFuncTable[14] := FuncNot;
  fIdentFuncTable[0] := FuncOff;
  fIdentFuncTable[9] := FuncOn;
  fIdentFuncTable[23] := FuncPause;
  fIdentFuncTable[4] := FuncSet;
  fIdentFuncTable[7] := FuncShift;
  fIdentFuncTable[8] := FuncStart;
  fIdentFuncTable[18] := FuncTitle;
end;

function TSynBatSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier
end;

function TSynBatSyn.FuncCall(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynBatSyn.FuncCd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynBatSyn.FuncCls(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynBatSyn.FuncCopy(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynBatSyn.FuncDel(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynBatSyn.FuncDo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynBatSyn.FuncEcho(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynBatSyn.FuncErrorlevel(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynBatSyn.FuncExist(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynBatSyn.FuncFor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynBatSyn.FuncGoto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynBatSyn.FuncIf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynBatSyn.FuncIn(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynBatSyn.FuncNot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynBatSyn.FuncOff(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynBatSyn.FuncOn(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynBatSyn.FuncPause(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynBatSyn.FuncSet(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynBatSyn.FuncShift(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynBatSyn.FuncStart(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynBatSyn.FuncTitle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

constructor TSynBatSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clNavy;
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey, SYNS_FriendlyAttrKey);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  fNumberAttri.Foreground := clBlue;
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  fVariableAttri.Foreground := clGreen;
  AddAttribute(fVariableAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterBatch;
end;

procedure TSynBatSyn.VariableProc;

  function IsVarChar: Boolean;
  begin
    case fLine[Run] of
      '_', '0'..'9', 'A'..'Z', 'a'..'z':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  fTokenID := tkVariable;
  repeat
    Inc(Run);
  until not IsVarChar;
  if fLine[Run] = '%' then
    Inc(Run);
end;

procedure TSynBatSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if (fLine[Run] = #10) then Inc(Run);
end;

procedure TSynBatSyn.CommentProc;
begin
  fTokenID := tkIdentifier;
  Inc(Run);
  if fLine[Run] = ':' then begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynBatSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  Inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do inc(Run);
end;

procedure TSynBatSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynBatSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynBatSyn.NumberProc;
begin
  fTokenID := tkNumber;
  repeat
    Inc(Run);
  until not CharInSet(fLine[Run], ['0'..'9', '.']);
end;

procedure TSynBatSyn.REMCommentProc;
begin
  if CharInSet(FLine[Run + 1], ['E', 'e']) and
    CharInSet(FLine[Run + 2], ['M', 'm']) and
    (FLine[Run + 3] < #33) then
  begin
    fTokenID := tkComment;
    Inc(Run, 3);
    while (FLine[Run] <> #0) do begin
      case FLine[Run] of
        #10, #13: break;
      end; { case }
      Inc(Run);
    end; { while }
  end
  else
  begin
    fTokenID := tkIdentifier;
    IdentProc;
  end;
end;

procedure TSynBatSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or IsLineEnd(Run);
end;

procedure TSynBatSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynBatSyn.Next;
begin
  fTokenPos := Run;

  case fLine[Run] of
    '%': VariableProc;
    #13: CRProc;
    ':': CommentProc;
    'A'..'Q', 'S'..'Z', 'a'..'q', 's'..'z', '_': IdentProc;
    #10: LFProc;
    #0: NullProc;
    '0'..'9': NumberProc;
    'R', 'r': REMCommentProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    else
      UnknownProc;
  end;
  inherited;
end;

function TSynBatSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynBatSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynBatSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkUnknown: Result := fIdentifierAttri;
    tkVariable: Result := fVariableAttri;
    else Result := nil;
  end;
end;

function TSynBatSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynBatSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynBatSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterBatch;
end;

class function TSynBatSyn.GetLanguageName: string;
begin
  Result := SYNS_LangBatch;
end;

function TSynBatSyn.GetSampleSource: UnicodeString;
begin
  Result := 'rem MS-DOS batch file'#13#10 +
            'rem'#13#10 +
            '@echo off'#13#10 +
            'cls'#13#10 +
            'echo The command line is: %1 %2 %3 %4 %5'#13#10 +
            'rem'#13#10 +
            'rem now wait for the user ...'#13#10 +
            'pause'#13#10 +
            'copy c:\*.pas d:\'#13#10 +
            'if errorlevel 1 echo Error in copy action!';
end;

class function TSynBatSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangBatch;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynBatSyn);
{$ENDIF}
end.
