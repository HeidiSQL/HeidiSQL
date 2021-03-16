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

$Id: SynHighlighterCAC.pas,v 1.10.2.8 2008/09/14 16:24:59 maelh Exp $

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

unit SynHighlighterCAC;

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
  TtkTokenKind = (tkComment, tkDirective, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkOperator, tkUnknown);

  TRangeState = (rsANil, rsCStyle, rsUnknown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TSynCACSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FStringAttri: TSynHighlighterAttributes;
    FOperatorAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FCommentAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FDirecAttri: TSynHighlighterAttributes;
    FIdentFuncTable: array[0..708] of TIdentFuncTableFunc;
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
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
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
    property OperatorAttri: TSynHighlighterAttributes read FOperatorAttri
      write FOperatorAttri;
    property DirecAttri: TSynHighlighterAttributes read FDirecAttri
      write FDirecAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  KeyWords: array[0..142] of UnicodeString = (
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
    Inc(Str);
  end;
  Result := Result mod 709;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynCACSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynCACSyn.InitIdent;
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
  FOperatorAttri := TSynHighlighterAttributes.Create(SYNS_AttrOperator, SYNS_FriendlyAttrOperator);
  AddAttribute(FOperatorAttri);
  FDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  AddAttribute(FDirecAttri);
  InitIdent;
  SetAttributesOnChange(DefHighlightChange);
  FRange := rsUnknown;
  FDefaultFilter := SYNS_FilterCAClipper;
end;

procedure TSynCACSyn.CStyleProc;
begin
  FTokenID := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        Exit;
      end;
    #10:
      begin
        LFProc;
        Exit;
      end;

    #13:
      begin
        CRProc;
        Exit;
      end;
  end;

  while FLine[Run] <> #0 do
    case FLine[Run] of
      '*':
        if FLine[Run + 1] = '/' then
        begin
          FRange := rsUnknown;
          Inc(Run, 2);
          Break;
        end else Inc(Run);
      #10: Break;
      #13: Break;
    else Inc(Run);
    end;
end;

procedure TSynCACSyn.CRProc;
begin
  FTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: Inc(Run, 2);
  else Inc(Run);
  end;
end;

procedure TSynCACSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + Run));
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do Inc(Run);
end;

procedure TSynCACSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynCACSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynCACSyn.NumberProc;

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

procedure TSynCACSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '/':
      begin
        Inc(Run, 2);
        FTokenID := tkComment;
        while FLine[Run] <> #0 do
        begin
          case FLine[Run] of
            #10, #13: Break;
          end;
          Inc(Run);
        end;
      end;
    '*':
      begin
        FTokenID := tkComment;
        FRange := rsCStyle;
        Inc(Run, 2);
        while FLine[Run] <> #0 do
          case FLine[Run] of
            '*':
              if FLine[Run + 1] = '/' then
              begin
                FRange := rsUnknown;
                Inc(Run, 2);
                Break;
              end else Inc(Run);
            #10: Break;
            #13: Break;
          else Inc(Run);
          end;
      end;
  else
    begin
      Inc(Run);
      FTokenID := tkOperator;
    end;
  end;
end;

procedure TSynCACSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynCACSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkOperator;
end;

procedure TSynCACSyn.StringProc;
var
  ActiveStr: WideChar;
begin
  FTokenID := tkString;
  ActiveStr := FLine[Run];
  if ((FLine[Run + 1] = #39) and (FLine[Run + 2] = #39)) or
    ((FLine[Run + 1] = #34) and (FLine[Run + 2] = #34)) then Inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
    end;
    Inc(Run);
  until (FLine[Run] = ActiveStr);
  if FLine[Run] <> #0 then Inc(Run);
end;

procedure TSynCACSyn.DirectiveProc;
begin
  FTokenID := tkDirective;
  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
      '/': if FLine[Run + 1] = '/' then Break;
      #34, #39: Break;
    end;
    Inc(Run);
  until FLine[Run] = #0;
end;

procedure TSynCACSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynCACSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsCStyle: CStyleProc;
    else
      case FLine[Run] of
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

function TSynCACSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynCACSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynCACSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynCACSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynCACSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkDirective: Result := FDirecAttri;
    tkOperator: Result := FOperatorAttri;
    tkUnknown: Result := FOperatorAttri;
    else Result := nil;
  end;
end;

function TSynCACSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynCACSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynCACSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynCACSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterCAClipper;
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
    FTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end
  else
  begin
    Inc(Run);
    FTokenID := tkOperator;
  end;
end;

class function TSynCACSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangCAClipper;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynCACSyn);
{$ENDIF}
end.
