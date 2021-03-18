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

unit SynHighlighterSml;

{$I SynEdit.inc}

interface

uses
  Graphics,
  Registry,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
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
    FBasis: Boolean;
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FIdentFuncTable: array[0..70] of TIdentFuncTableFunc;
    FCharacterAttri: TSynHighlighterAttributes;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FOperatorAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FSyntaxErrorAttri: TSynHighlighterAttributes;
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
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property CharacterAttri: TSynHighlighterAttributes read FCharacterAttri
      write FCharacterAttri;
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property OperatorAttri: TSynHighlighterAttributes read FOperatorAttri
      write FOperatorAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri
      write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
    property SyntaxErrorAttri: TSynHighlighterAttributes read FSyntaxErrorAttri
      write FSyntaxErrorAttri;
    property Basis: Boolean read FBasis write FBasis default True;
  end;

implementation

uses
  SynEditStrConst;

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
    Inc(Str);
  end;
  Result := Result mod 71;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynSMLSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynSMLSyn.InitIdent;
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

function TSynSMLSyn.IsValidMLCharacter: Boolean;

 function IsABNRTChar(Run: Integer): Boolean;
  begin
    case FLine[Run] of
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
  if (FLine[Run] = '"') then
    if (Run > 2) and (FLine[Run - 1] <> '\') and (FLine[Run - 2] = '"') then
      Result := True
    else if (Run > 3) and (FLine[Run - 1] = '\') and (FLine[Run - 2] = '\')
      and (FLine[Run - 3] = '"') then
      Result := True
    else if (Run > 3) and IsABNRTChar(Run - 1) and
      (FLine[Run - 2] = '\') and (FLine[Run - 3] = '"') then
      Result := True
    else if (Run > 5) and (FLine[Run - 4] = '\') and (FLine[Run - 5] = '"') then
    begin
      ASCIIStr := copy(FLine, Run - 2, 3);
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

  FCaseSensitive := True;

  FCharacterAttri := TSynHighlighterAttributes.Create(SYNS_AttrCharacter, SYNS_FriendlyAttrCharacter);
  FCharacterAttri.Foreground := clBlue;
  AddAttribute(FCharacterAttri);
  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  FCommentAttri.Foreground := clNavy;
  AddAttribute(FCommentAttri);
  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  FKeyAttri.Foreground := clGreen;
  AddAttribute(FKeyAttri);
  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clRed;
  AddAttribute(FNumberAttri);
  FOperatorAttri := TSynHighlighterAttributes.Create(SYNS_AttrOperator, SYNS_FriendlyAttrOperator);
  FOperatorAttri.Foreground := clMaroon;
  AddAttribute(FOperatorAttri);
  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clBlue;
  AddAttribute(FStringAttri);
  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  FSyntaxErrorAttri := TSynHighlighterAttributes.Create(SYNS_AttrSyntaxError, SYNS_FriendlyAttrSyntaxError);
  FSyntaxErrorAttri.Foreground := clRed;
  FSyntaxErrorAttri.Style := [fsBold];
  AddAttribute(FSyntaxErrorAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;        
  FDefaultFilter := SYNS_FilterSML;
  Basis := True;
end;

procedure TSynSMLSyn.CRProc;
begin
  FTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: Inc(Run, 2);
  else
    Inc(Run);
  end;
end;

procedure TSynSMLSyn.ColonProc;
begin
  Inc(Run);
  if Basis and (FLine[Run] = ':') then
  begin
    FTokenID := tkOperator;
    Inc(Run);
  end
  else FTokenID := tkSymbol;
end;

procedure TSynSMLSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + Run));
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do Inc(Run);
end;

procedure TSynSMLSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynSMLSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynSMLSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', '.', 'u', 'U', 'l', 'L', 'x', 'X', 'e', 'E', 'f', 'F':
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
      '.':  if FLine[Run + 1] = '.' then
        Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynSMLSyn.OperatorProc;
begin
  Inc(Run);
  FTokenID := tkOperator;
end;

procedure TSynSMLSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynSMLSyn.StringProc;
begin
  FTokenID := tkString;
  repeat
    if FLine[Run] = '\' then
    begin
      case FLine[Run + 1] of
        '"', '\':
          Inc(Run);
        #00:
          begin
            Inc(Run);
            FRange := rsMultilineString;
            Exit;
          end;
      end;
    end;
    Inc(Run);
  until IsLineEnd(Run) or (FLine[Run] = '"');
  if FLine[Run] = '"' then
    Inc(Run);
end;

procedure TSynSMLSyn.StringEndProc;
begin
  FTokenID := tkString;

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

  FRange := rsUnknown;

  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
      '\':
        begin
          case FLine[Run + 1] of
            '"', '\':
              Inc(Run);
            #00:
              begin
                Inc(Run);
                FRange := rsMultilineString;
                Exit;
              end;
          end;
        end;
      '"': Break;
    end;
    Inc(Run);
  until IsLineEnd(Run) or (FLine[Run] = '"');
  if FLine[Run] = '"' then
    Inc(Run);
end;

procedure TSynSMLSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynSMLSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynSMLSyn.BasisOpProc;
begin
  Inc(Run);
  if Basis then FTokenID := tkOperator else FTokenID := tkIdentifier;
end;

procedure TSynSMLSyn.PoundProc;
begin
  Inc(Run);
  if (FLine[Run] = '"') then
    CharacterProc
  else
    FTokenID := tkIdentifier;
end;

procedure TSynSMLSyn.CharacterProc;
begin
  case FLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      repeat
        Inc(Run);
      until IsLineEnd(Run) or (FLine[Run] = '"');

      if IsValidMLCharacter then
        FTokenID := tkCharacter
      else
      begin
        if FLine[Run] = '"' then Inc(Run);
        FTokenID := tkSyntaxError;
      end;
    end
  end
end;

procedure TSynSMLSyn.RoundBracketOpenProc;
begin
  Inc(Run);
  if (FLine[Run] = '*') then
  begin
    FRange := rsComment;
    CommentProc;
    FTokenID := tkComment;
  end
  else
    FTokenID := tkIdentifier;
end;

procedure TSynSMLSyn.CommentProc;
begin
  case FLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      FTokenID := tkComment;
      repeat
        if (FLine[Run] = '*') and
           (FLine[Run + 1] = ')') then
        begin
          Inc(Run, 2);
          FRange := rsUnknown;
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
  FTokenPos := Run;
  case FRange of
    rsComment: CommentProc;
    rsMultilineString: StringEndProc; 
  else
    begin
      FRange := rsUnknown;

      case FLine[Run] of
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

function TSynSMLSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynSMLSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynSMLSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkCharacter: Result := FCharacterAttri;
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkOperator: Result := FOperatorAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkSyntaxError: Result := FSyntaxErrorAttri;
    tkUnknown: Result := FIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynSMLSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

function TSynSMLSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterSML;
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
  FRange := rsUnknown;
end;

procedure TSynSMLSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynSMLSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
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
