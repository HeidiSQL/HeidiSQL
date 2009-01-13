{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterSML.pas, released 2000-04-17.
The Original Code is based on the dmMLSyn.pas file from the
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

$Id: SynHighlighterSml.pas,v 1.14.2.6 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides SynEdit with a Standard ML syntax highlighter, with extra options for the standard Basis library.)
@author(David H Muir <dhm@dmsoftware.co.uk>)
@created(1999)
@lastmod(2000-06-23)
The SynHighlighterSML.pas unit provides SynEdit text control with a Standard ML highlighter.  Many formatting attributes can
be specified, and there is an option to include extra keywords and operators only found in the Basis library, this option can
be disabled for backwards compatibility with older ML compilers that do not have support for the Basis Library.
}

{$IFNDEF QSYNHIGHLIGHTERSML}
unit SynHighlighterSml;
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

Type
  TtkTokenKind = (tkCharacter, tkComment, tkIdentifier, tkKey, tkNull, tkNumber,
    tkOperator, tkSpace, tkString, tkSymbol, tkSyntaxError, tkUnknown);

  TRangeState = (rsUnknown, rsComment, rsMultilineString);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynSMLSyn = class(TSynCustomHighlighter)
  private
    fBasis: Boolean;
    fRange: TRangeState;
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..70] of TIdentFuncTableFunc;
    fCharacterAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fOperatorAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fSyntaxErrorAttri: TSynHighlighterAttributes;
    function IsValidMLCharacter: Boolean;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure CRProc;
    procedure CharacterProc;
    procedure ColonProc;
    procedure CommentProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OperatorProc;
    procedure RoundBracketOpenProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    procedure BasisOpProc;
    procedure StringEndProc;
    procedure PoundProc;
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
    function GetTokenKind: integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property CharacterAttri: TSynHighlighterAttributes read fCharacterAttri
      write fCharacterAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property OperatorAttri: TSynHighlighterAttributes read fOperatorAttri
      write fOperatorAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property SyntaxErrorAttri: TSynHighlighterAttributes read fSyntaxErrorAttri
      write fSyntaxErrorAttri;
    property Basis: Boolean read FBasis write FBasis default True;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  KeyWords: array[0..40] of UnicodeString = (
    'abstype', 'and', 'andalso', 'as', 'case', 'datatype', 'do', 'else', 'end', 
    'eqtype', 'exception', 'fn', 'fun', 'functor', 'handle', 'if', 'in', 
    'include', 'infix', 'infixr', 'let', 'local', 'nonfix', 'of', 'op', 'open', 
    'orelse', 'raise', 'rec', 'sharing', 'sig', 'signature', 'struct', 
    'structure', 'then', 'type', 'val', 'where', 'while', 'with', 'withtype' 
  );

  KeyIndices: array[0..70] of Integer = (
    28, -1, -1, -1, 23, 4, 19, -1, -1, 32, 8, 6, -1, 33, 0, -1, 14, -1, 2, -1, 
    -1, 29, 35, -1, -1, -1, -1, 13, -1, -1, 9, -1, 11, 30, 1, -1, 25, 36, -1, 
    -1, -1, 40, -1, 7, -1, 16, 26, 37, -1, 15, 21, -1, 18, 12, 5, -1, -1, 10, 
    22, 27, 34, 17, -1, 20, -1, 39, -1, 3, 38, 31, 24 
  );

{$Q-}
function TSynSMLSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 157 + Ord(Str^) * 35;
    inc(Str);
  end;
  Result := Result mod 71;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynSMLSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynSMLSyn.InitIdent;
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

function TSynSMLSyn.IsValidMLCharacter: Boolean;

 function IsABNRTChar(Run: Integer): Boolean;
  begin
    case fLine[Run] of
      'a', 'b', 'n', 'r', 't':
        Result := True;
      else
        Result := False;
    end;
  end;

var
  ASCIIStr: UnicodeString;
  ASCIICode, Error: Integer;
begin
  Result := False;
  if (fLine[Run] = '"') then
    if (Run > 2) and (fLine[Run - 1] <> '\') and (fLine[Run - 2] = '"') then
      Result := True
    else if (Run > 3) and (fLine[Run - 1] = '\') and (fLine[Run - 2] = '\')
      and (fLine[Run - 3] = '"') then
      Result := True
    else if (Run > 3) and IsABNRTChar(Run - 1) and
      (fLine[Run - 2] = '\') and (fLine[Run - 3] = '"') then
      Result := True
    else if (Run > 5) and (fLine[Run - 4] = '\') and (fLine[Run - 5] = '"') then
    begin
      ASCIIStr := copy(fLine, Run - 2, 3);
      Val(ASCIIStr, ASCIICode, Error);
      if (Error = 0) and (ASCIICode >= 0) and (ASCIICode <= 255) then
        Result := True
    end
end;

function TSynSMLSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynSMLSyn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

constructor TSynSMLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := True;

  fCharacterAttri := TSynHighlighterAttributes.Create(SYNS_AttrCharacter, SYNS_FriendlyAttrCharacter);
  fCharacterAttri.Foreground := clBlue;
  AddAttribute(fCharacterAttri);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clNavy;
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := clGreen;
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  fNumberAttri.Foreground := clRed;
  AddAttribute(fNumberAttri);
  fOperatorAttri := TSynHighlighterAttributes.Create(SYNS_AttrOperator, SYNS_FriendlyAttrOperator);
  fOperatorAttri.Foreground := clMaroon;
  AddAttribute(fOperatorAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Foreground := clBlue;
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  fSyntaxErrorAttri := TSynHighlighterAttributes.Create(SYNS_AttrSyntaxError, SYNS_FriendlyAttrSyntaxError);
  fSyntaxErrorAttri.Foreground := clRed;
  fSyntaxErrorAttri.Style := [fsBold];
  AddAttribute(fSyntaxErrorAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;        
  fDefaultFilter := SYNS_FilterSML;
  Basis := True;
end;

procedure TSynSMLSyn.CRProc;
begin
  fTokenID := tkSpace;
  Case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TSynSMLSyn.ColonProc;
begin
  inc(Run);
  if Basis and (fLine[Run] = ':') then
  begin
    fTokenID := tkOperator;
    inc(Run);
  end
  else fTokenID := tkSymbol;
end;

procedure TSynSMLSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do inc(Run);
end;

procedure TSynSMLSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynSMLSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynSMLSyn.NumberProc;

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
      '.':  if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSynSMLSyn.OperatorProc;
begin
  inc(Run);
  fTokenID := tkOperator;
end;

procedure TSynSMLSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynSMLSyn.StringProc;
begin
  fTokenID := tkString;
  repeat
    if fLine[Run] = '\' then
    begin
      case fLine[Run + 1] of
        '"', '\':
          Inc(Run);
        #00:
          begin
            Inc(Run);
            fRange := rsMultilineString;
            Exit;
          end;
      end;
    end;
    inc(Run);
  until IsLineEnd(Run) or (fLine[Run] = '"');
  if FLine[Run] = '"' then
    inc(Run);
end;

procedure TSynSMLSyn.StringEndProc;
begin
  fTokenID := tkString;

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

  fRange := rsUnknown;

  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
      '\':
        begin
          case fLine[Run + 1] of
            '"', '\':
              Inc(Run);
            #00:
              begin
                Inc(Run);
                fRange := rsMultilineString;
                Exit;
              end;
          end;
        end;
      '"': Break;
    end;
    inc(Run);
  until IsLineEnd(Run) or (fLine[Run] = '"');
  if FLine[Run] = '"' then
    inc(Run);
end;

procedure TSynSMLSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSMLSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynSMLSyn.BasisOpProc;
begin
  inc(Run);
  if Basis then fTokenID := tkOperator else fTokenID := tkIdentifier;
end;

procedure TSynSMLSyn.PoundProc;
begin
  Inc(Run);
  if (fLine[Run] = '"') then
    CharacterProc
  else
    fTokenID := tkIdentifier;
end;

procedure TSynSMLSyn.CharacterProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      repeat
        Inc(Run);
      until IsLineEnd(Run) or (fLine[Run] = '"');

      if IsValidMLCharacter then
        fTokenID := tkCharacter
      else
      begin
        if fLine[Run] = '"' then Inc(Run);
        fTokenID := tkSyntaxError;
      end;
    end
  end
end;

procedure TSynSMLSyn.RoundBracketOpenProc;
begin
  Inc(Run);
  if (fLine[Run] = '*') then
  begin
    fRange := rsComment;
    CommentProc;
    fTokenID := tkComment;
  end
  else
    fTokenID := tkIdentifier;
end;

procedure TSynSMLSyn.CommentProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      fTokenID := tkComment;
      repeat
        if (fLine[Run] = '*') and
           (fLine[Run + 1] = ')') then
        begin
          Inc(Run, 2);
          fRange := rsUnknown;
          Break;
        end;
        if not IsLineEnd(Run) then
          Inc(Run);
      until IsLineEnd(Run);
    end;
  end;
end;

procedure TSynSMLSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsComment: CommentProc;
    rsMultilineString: StringEndProc; 
  else
    begin
      fRange := rsUnknown;

      case fLine[Run] of
        #13: CRProc;
        '#': PoundProc;
        ':': ColonProc;
        'A'..'Z', 'a'..'z', '_': IdentProc;
        #10: LFProc;
        #0: NullProc;
        '0'..'9': NumberProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        '"': StringProc;
        '@', '^': BasisOpProc;
        '(': RoundBracketOpenProc;
        '+', '-', '~', '*', '/', '=', '<', '>': OperatorProc;
        ',', '.',  ';': SymbolProc;
        else UnknownProc;
      end;
    end;
  end;
  inherited;
end;

function TSynSMLSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynSMLSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynSMLSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynSMLSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkCharacter: Result := fCharacterAttri;
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkOperator: Result := fOperatorAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkSyntaxError: Result := fSyntaxErrorAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynSMLSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynSMLSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterSML;
end;

function TSynSMLSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    #39, '_', '0'..'9', 'a'..'z', 'A'..'Z':
      Result := True;
    else
      Result := False;
  end;
end;

class function TSynSMLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangSML;
end;

function TSynSMLSyn.GetSampleSource: UnicodeString;
begin
  Result := '(* Syntax highlighting *)'#13#10 +
            'load "Real";'#13#10 +
            'fun PrintNumber(x: int) ='#13#10 +
            '  let'#13#10 +
            '    val Number = real(x) / 10.0;'#13#10 +
            '    val Text = "The Number is " ^ Real.toString(~Number) ^ "\n";'#13#10 +
            '  in'#13#10 +
            '    print Text;'#13#10 +
            '    if x = 0 then () else PrintNumber(x-1)'#13#10+
            '  end;' 
end;

procedure TSynSMLSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynSMLSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynSMLSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

class function TSynSMLSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangSML;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynSMLSyn);
{$ENDIF}
end.
