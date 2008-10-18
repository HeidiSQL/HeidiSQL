{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterModelica.pas, released 2000-11-09.
The Initial Author of this file is Falko Jens Wagner.
Portions created by Falko Jens Wagner are Copyright 2000 Falko Jens Wagner.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynHighlighterModelica.pas,v 1.12.2.6 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNHIGHLIGHTERMODELICA}
unit SynHighlighterModelica;
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
  Registry,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkDirective, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown);

  TRangeState = (rsUnknown, rsString39, rsString34, rsComment);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynModelicaSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..96] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fDirectiveAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure ColonProc;
    procedure DirectiveProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure SymbolProcWithEqual;
    procedure UnknownProc;
    procedure AnsiCProc;
    procedure String34Proc;
    procedure String39Proc;
  protected
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property DirectiveAttri: TSynHighlighterAttributes read fDirectiveAttri
      write fDirectiveAttri;
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
  KeyWords: array[0..47] of UnicodeString = (
    'algorithm', 'and', 'annotation', 'assert', 'block', 'Boolean', 'class', 
    'connect', 'connector', 'constant', 'der', 'discrete', 'else', 'elseif', 
    'end', 'equation', 'extends', 'external', 'false', 'final', 'flow', 'for', 
    'function', 'if', 'in', 'input', 'Integer', 'loop', 'model', 'nondiscrete', 
    'not', 'or', 'output', 'package', 'parameter', 'partial', 'protected', 
    'public', 'Real', 'record', 'redeclare', 'replaceable', 'terminate', 'then', 
    'true', 'type', 'when', 'while' 
  );

  KeyIndices: array[0..96] of Integer = (
    -1, 8, 41, 46, -1, 21, -1, 30, 5, -1, 45, -1, -1, 23, 7, -1, -1, 17, 15, -1, 
    -1, 10, -1, -1, -1, 3, -1, 18, -1, 28, -1, -1, 47, -1, -1, -1, -1, -1, 39, 
    16, 27, 25, -1, 4, 22, -1, 43, -1, 37, 40, -1, -1, 31, -1, 42, -1, -1, 26, 
    14, 24, 44, -1, -1, -1, -1, 11, 33, 0, -1, -1, -1, -1, 36, 19, -1, 38, -1, 
    32, -1, -1, 29, -1, -1, -1, 6, 35, 12, 1, -1, -1, -1, 20, 34, -1, 13, 9, 2 
  );

{$Q-}
function TSynModelicaSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 598 + Ord(Str^) * 127;
    inc(Str);
  end;
  Result := Result mod 97;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynModelicaSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynModelicaSyn.InitIdent;
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

function TSynModelicaSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynModelicaSyn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

constructor TSynModelicaSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := True;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fDirectiveAttri := TSynHighlighterAttributes.Create(SYNS_AttrDirective, SYNS_FriendlyAttrDirective);
  AddAttribute(fDirectiveAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
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
  fDefaultFilter := SYNS_FilterModelica;
  fRange := rsUnknown;
end;

procedure TSynModelicaSyn.AndSymbolProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if CharInSet(fLine[Run], ['=', '&']) then
    Inc(Run);
end;

procedure TSynModelicaSyn.AsciiCharProc;
begin
  fRange := rsString39;
  fTokenID := tkString;
  repeat
    Inc(Run);
  until IsLineEnd(Run) or (fLine[Run] = #39);
  if fLine[Run] = #39 then
  begin
    fRange := rsUnknown;
    Inc(Run);
  end;
end;

procedure TSynModelicaSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynModelicaSyn.ColonProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fLine[Run] = ':' then
    Inc(Run);
end;

procedure TSynModelicaSyn.DirectiveProc;
begin
  fTokenID := tkDirective;
  repeat
    Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynModelicaSyn.GreaterProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  case fLine[Run] of
    '=': Inc(Run);
    '>': begin
           Inc(Run);
           if fLine[Run] = '=' then
             Inc(Run);
         end;
  end;
end;

procedure TSynModelicaSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do inc(Run);
end;

procedure TSynModelicaSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynModelicaSyn.LowerProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  case fLine[Run] of
    '=': Inc(Run);
    '<': begin
           Inc(Run);
           if fLine[Run] = '=' then
             Inc(Run);
         end;
  end;
end;

procedure TSynModelicaSyn.MinusProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if CharInSet(fLine[Run], ['=', '-', '>']) then
    Inc(Run);
end;

procedure TSynModelicaSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynModelicaSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', 'u', 'U', 'l', 'L', 'x', 'X', 'e', 'E', 'f', 'F':
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

procedure TSynModelicaSyn.OrSymbolProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if CharInSet(fLine[Run], ['=', '|']) then
    Inc(Run);
end;

procedure TSynModelicaSyn.PlusProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if CharInSet(fLine[Run], ['=', '+']) then
    Inc(Run);
end;

procedure TSynModelicaSyn.PointProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if (fLine[Run] = '.') and (fLine[Run + 1] = '.') then
    Inc(Run, 2);
end;

procedure TSynModelicaSyn.SlashProc;
begin
  Inc(Run);
  case fLine[Run] of
    '/':
      begin
        fTokenID := tkComment;
        repeat
          Inc(Run);
        until IsLineEnd(Run);
      end;
    '*':
      begin
        fRange := rsComment;
        inc(Run);
        if IsLineEnd(Run) then
          fTokenID := tkComment
        else
          AnsiCProc;
      end;
  else
    fTokenID := tkSymbol;
    if fLine[Run] = '=' then
      Inc(Run);
  end;
end;

procedure TSynModelicaSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or IsLineEnd(Run);
end;

procedure TSynModelicaSyn.StringProc;
begin
  fRange := rsString34;
  Inc(Run);
  if IsLineEnd(Run) then
    fTokenID := tkString
  else
    String34Proc;
end;

procedure TSynModelicaSyn.SymbolProc;
begin
  Inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynModelicaSyn.SymbolProcWithEqual;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fLine[Run] = '=' then
    Inc(Run);
end;

procedure TSynModelicaSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynModelicaSyn.AnsiCProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then begin
        inc(Run, 2);
        fRange := rsUnknown;
        break;
      end;
      Inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynModelicaSyn.String39Proc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkString;
    repeat
      if fLine[Run] = #39 then begin
        inc(Run);
        fRange := rsUnknown;
        break;
      end;
      Inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynModelicaSyn.String34Proc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkString;
    repeat
      case fLine[Run] of
        #34:
          begin
            Inc(Run);
            fRange := rsUnknown;
            break;
          end;
        #92:
          begin
            Inc(Run);
            if fLine[Run] = #34 then
              Inc(Run);
          end;
      else
        Inc(Run);
      end;
    until IsLineEnd(Run);
  end;
end;

procedure TSynModelicaSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsComment: AnsiCProc;
    rsString39: String39Proc;
    rsString34: String34Proc;
  else
    fRange := rsUnknown;
    case fLine[Run] of
      '&': AndSymbolProc;
      #39: AsciiCharProc;
      #13: CRProc;
      ':': ColonProc;
      '#': DirectiveProc;
      '>': GreaterProc;
      'A'..'Z', 'a'..'z', '_': IdentProc;
      #10: LFProc;
      '<': LowerProc;
      '-': MinusProc;
      #0: NullProc;
      '0'..'9': NumberProc;
      '|': OrSymbolProc;
      '+': PlusProc;
      '.': PointProc;
      '/': SlashProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      #34: StringProc;
      '~', '[', ']', '@', '{', '}', '(', ')', ';', ',': SymbolProc;
      '*', '^', '=', '%', '!': SymbolProcWithEqual;
      else UnknownProc;
    end;
  end;
  inherited;
end;

function TSynModelicaSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynModelicaSyn.GetEol: boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynModelicaSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynModelicaSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynModelicaSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkDirective: Result := fDirectiveAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynModelicaSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

procedure TSynModelicaSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynModelicaSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynModelicaSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterModelica;
end;

class function TSynModelicaSyn.GetLanguageName: string;
begin
  Result := SYNS_LangModelica;
end;

class function TSynModelicaSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangModelica;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynModelicaSyn);
{$ENDIF}
end.
