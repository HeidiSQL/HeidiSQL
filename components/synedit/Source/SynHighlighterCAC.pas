{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterCAC.pas, released 2000-04-21.
The Original Code is based on the cwCACSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Carlos Wijders.
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

$Id: SynHighlighterCAC.pas,v 1.10.2.7 2005/11/27 22:43:32 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a CA-Clipper syntax highlighter for SynEdit)
@author(Carlos Wijders <ctfbs@sr.net>, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(1998-12-27, converted to SynEdit 2000-04-21)
@lastmod(2000-06-23)
The SynHighlighterCAC unit provides SynEdit with a CA-Clipper syntax highlighter.
Thanks to Primoz Gabrijelcic, Andy Jeffries.
}

{$IFNDEF QSYNHIGHLIGHTERCAC}
unit SynHighlighterCAC;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
{$ELSE}
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkDirective, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkOperator, tkUnknown);

  TRangeState = (rsANil, rsCStyle, rsUnknown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TSynCACSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    FTokenID: TtkTokenKind;
    fStringAttri: TSynHighlighterAttributes;
    fOperatorAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fDirecAttri: TSynHighlighterAttributes;
    fIdentFuncTable: array[0..708] of TIdentFuncTableFunc;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure StarProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SymbolProc;
    procedure StringProc;
    procedure DirectiveProc;
    procedure UnknownProc;
    procedure CStyleProc;
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
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
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
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property OperatorAttri: TSynHighlighterAttributes read fOperatorAttri
      write fOperatorAttri;
    property DirecAttri: TSynHighlighterAttributes read fDirecAttri
      write fDirecAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  KeyWords: array[0..142] of WideString = (
    'aadd', 'abs', 'and', 'announce', 'asc', 'at', 'average', 'begin', 'bof', 
    'break', 'call', 'cancel', 'cdow', 'chr', 'clear', 'close', 'cmonth', 'col', 
    'commit', 'continue', 'copy', 'count', 'create', 'ctod', 'date', 'day', 
    'declare', 'delete', 'deleted', 'devpos', 'dir', 'display', 'dow', 'dtoc', 
    'dtos', 'eject', 'else', 'elseif', 'empty', 'endcase', 'enddo', 'endif', 
    'eof', 'erase', 'exit', 'exp', 'external', 'fcount', 'field', 'fieldname', 
    'file', 'find', 'flock', 'for', 'found', 'function', 'get', 'go', 'if', 
    'iif', 'index', 'init', 'inkey', 'input', 'int', 'join', 'keyboard', 
    'lastrec', 'len', 'list', 'local', 'locate', 'lock', 'log', 'lower', 
    'ltrim', 'max', 'memvar', 'min', 'month', 'not', 'note', 'or', 'pack', 
    'parameters', 'pcol', 'pcount', 'private', 'procedure', 'prompt', 'prow', 
    'public', 'quit', 'read', 'recall', 'reccount', 'recno', 'reindex', 
    'release', 'rename', 'replace', 'replicate', 'request', 'restore', 'return', 
    'rlock', 'round', 'row', 'rtrim', 'run', 'save', 'say', 'seconds', 'seek', 
    'select', 'sequence', 'setpos', 'skip', 'sort', 'space', 'sqrt', 'static', 
    'store', 'str', 'substr', 'sum', 'text', 'time', 'total', 'transform', 
    'trim', 'type', 'unlock', 'update', 'upper', 'use', 'val', 'valtype', 
    'wait', 'while', 'word', 'year', 'zap' 
  );

  KeyIndices: array[0..708] of Integer = (
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 138, 87, 41, 140, 88, -1, -1, -1, 11, 
    -1, -1, -1, 53, -1, -1, -1, -1, 54, -1, 111, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 110, -1, -1, -1, 106, -1, -1, -1, -1, -1, -1, 24, -1, 86, -1, 
    -1, -1, 81, -1, -1, -1, -1, -1, 119, -1, -1, 14, -1, -1, -1, 92, -1, -1, -1, 
    -1, -1, 77, 89, 10, 23, -1, -1, 91, 65, -1, 122, -1, -1, -1, 36, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 124, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 46, -1, 27, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 120, -1, 100, 2, -1, -1, -1, -1, 75, 7, -1, -1, 
    -1, -1, -1, -1, -1, 108, 99, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 137, -1, -1, -1, -1, -1, -1, -1, -1, 50, 30, -1, 
    -1, -1, -1, 83, 116, -1, -1, 134, -1, -1, 69, -1, -1, -1, 109, -1, 76, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 142, -1, -1, -1, -1, -1, 
    -1, -1, -1, 85, -1, -1, -1, 127, -1, -1, 102, 48, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 90, -1, -1, -1, -1, -1, -1, -1, -1, 74, -1, -1, -1, -1, 133, 
    -1, 57, 113, -1, -1, -1, -1, -1, -1, 43, -1, 33, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 126, -1, 132, -1, -1, -1, -1, -1, -1, -1, 80, -1, -1, -1, 
    58, -1, -1, -1, -1, -1, -1, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 78, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 125, -1, -1, -1, -1, -1, 98, -1, 49, 
    123, -1, -1, -1, -1, -1, -1, -1, -1, 38, -1, -1, -1, -1, -1, 15, -1, -1, -1, 
    -1, -1, -1, -1, -1, 103, -1, -1, -1, -1, -1, 5, 82, -1, -1, -1, -1, -1, 35, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 29, -1, -1, -1, -1, -1, -1, 72, 
    -1, -1, -1, -1, -1, -1, -1, 19, 63, -1, 52, -1, -1, -1, -1, -1, 34, -1, -1, 
    -1, -1, -1, -1, -1, 13, -1, -1, -1, 105, -1, -1, -1, -1, -1, -1, 39, -1, -1, 
    -1, 118, -1, -1, -1, -1, -1, 121, 3, 115, -1, -1, 64, -1, -1, 60, -1, 114, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 47, -1, -1, -1, -1, -1, 20, -1, -1, 
    62, -1, -1, -1, -1, -1, -1, -1, -1, -1, 135, -1, -1, -1, -1, 22, -1, -1, -1, 
    -1, -1, 55, -1, 68, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 17, 94, 112, -1, 
    -1, -1, -1, 59, -1, -1, 21, -1, -1, 66, -1, -1, -1, -1, -1, 107, 28, -1, -1, 
    -1, -1, -1, -1, -1, 96, -1, -1, -1, 56, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 93, -1, -1, 
    -1, -1, 9, -1, -1, -1, -1, 104, -1, -1, -1, 42, -1, -1, -1, -1, 79, 18, 70, 
    -1, 26, 25, 32, -1, -1, 0, 37, -1, 40, -1, -1, -1, -1, 73, -1, 97, -1, -1, 
    -1, 67, 128, -1, -1, -1, -1, -1, -1, 136, 16, 12, -1, -1, -1, -1, -1, -1, 
    131, 117, -1, -1, -1, -1, -1, -1, 45, -1, -1, -1, -1, -1, -1, 51, -1, 1, -1, 
    -1, -1, -1, -1, 141, -1, 129, -1, 44, -1, -1, 71, -1, 61, -1, -1, -1, -1, 
    -1, -1, -1, 101, -1, -1, -1, -1, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, 130, 
    139, -1, -1, -1, -1, -1, 95, -1, -1, -1, 31, -1, -1, 84, 8 
  );

{$Q-}
function TSynCACSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 123 + Ord(Str^) * 763;
    inc(Str);
  end;
  Result := Result mod 709;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynCACSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynCACSyn.InitIdent;
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

function TSynCACSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynCACSyn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

constructor TSynCACSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
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
  fOperatorAttri := TSynHighlighterAttributes.Create(SYNS_AttrOperator, SYNS_FriendlyAttrOperator);
  AddAttribute(fOperatorAttri);
  fDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  AddAttribute(fDirecAttri);
  InitIdent;
  SetAttributesOnChange(DefHighlightChange);
  fRange := rsUnknown;
  fDefaultFilter := SYNS_FilterCAClipper;
end;

procedure TSynCACSyn.CStyleProc;
begin
  fTokenID := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        exit;
      end;
    #10:
      begin
        LFProc;
        exit;
      end;

    #13:
      begin
        CRProc;
        exit;
      end;
  end;

  while fLine[Run] <> #0 do
    case fLine[Run] of
      '*':
        if fLine[Run + 1] = '/' then
        begin
          fRange := rsUnknown;
          inc(Run, 2);
          break;
        end else inc(Run);
      #10: break;
      #13: break;
    else inc(Run);
    end;
end;

procedure TSynCACSyn.CRProc;
begin
  fTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TSynCACSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do inc(Run);
end;

procedure TSynCACSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynCACSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynCACSyn.NumberProc;

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

procedure TSynCACSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '/':
      begin
        inc(Run, 2);
        fTokenID := tkComment;
        while FLine[Run] <> #0 do
        begin
          case FLine[Run] of
            #10, #13: break;
          end;
          inc(Run);
        end;
      end;
    '*':
      begin
        fTokenID := tkComment;
        fRange := rsCStyle;
        inc(Run, 2);
        while fLine[Run] <> #0 do
          case fLine[Run] of
            '*':
              if fLine[Run + 1] = '/' then
              begin
                fRange := rsUnknown;
                inc(Run, 2);
                break;
              end else inc(Run);
            #10: break;
            #13: break;
          else inc(Run);
          end;
      end;
  else
    begin
      inc(Run);
      fTokenID := tkOperator;
    end;
  end;
end;

procedure TSynCACSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynCACSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkOperator;
end;

procedure TSynCACSyn.StringProc;
var
  ActiveStr: WideChar;
begin
  fTokenID := tkString;
  ActiveStr := FLine[Run];
  if ((FLine[Run + 1] = #39) and (FLine[Run + 2] = #39)) or
    ((FLine[Run + 1] = #34) and (FLine[Run + 2] = #34)) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until (FLine[Run] = ActiveStr);
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynCACSyn.DirectiveProc;
begin
  fTokenID := tkDirective;
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
      '/': if FLine[Run + 1] = '/' then break;
      #34, #39: break;
    end;
    inc(Run);
  until FLine[Run] = #0;
end;

procedure TSynCACSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynCACSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsCStyle: CStyleProc;
    else
      case fLine[Run] of
        '@': SymbolProc;
        '&': SymbolProc;
        '{': SymbolProc;
        '}': SymbolProc;
        #13: CRProc;
        ':': SymbolProc;
        ',': SymbolProc;
        '#': DirectiveProc;
        '=': SymbolProc;
        '>': SymbolProc;
        'A'..'Z', 'a'..'z': IdentProc;
        '$': SymbolProc;
        #10: LFProc;
        '<': SymbolProc;
        '-': SymbolProc;
        '!': SymbolProc;
        #0: NullProc;
        '0'..'9': NumberProc;
        '+': SymbolProc;
        '.': SymbolProc;
        '?': SymbolProc;
        ')': SymbolProc;
        '(': SymbolProc;
        ';': SymbolProc;
        '/': SlashProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        ']': SymbolProc;
        '[': SymbolProc;
        '*': StarProc;
        #39, #34: StringProc;
        else UnknownProc;
      end;
  end;
  inherited;
end;

function TSynCACSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TSynCACSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynCACSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynCACSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynCACSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkDirective: Result := fDirecAttri;
    tkOperator: Result := fOperatorAttri;
    tkUnknown: Result := fOperatorAttri;
    else Result := nil;
  end;
end;

function TSynCACSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

procedure TSynCACSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynCACSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynCACSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCAClipper;
end;

class function TSynCACSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCAClipper;
end;

procedure TSynCACSyn.StarProc;
begin
// if Run is 0 there could be an access violation
  if (Run = 0) or IsLineEnd(Run - 1) then
  begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end
  else
  begin
    inc(Run);
    fTokenID := tkOperator;
  end;
end;

class function TSynCACSyn.GetFriendlyLanguageName: WideString;
begin
  Result := SYNS_FriendlyLangCAClipper;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynCACSyn);
{$ENDIF}
end.
