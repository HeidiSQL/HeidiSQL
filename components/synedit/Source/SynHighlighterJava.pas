{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterJava.pas, released 2000-04-10.
The Original Code is based on the DcjSynJava.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Trier.
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

$Id: SynHighlighterJava.pas,v 1.18.2.10 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Java highlighter for SynEdit)
@author(Michael Trier)
@created(December 1998, converted to SynEdit 2000-04-10 by Michael Hieke)
@lastmod(2000-06-23)
The SynHighlighterJava unit provides SynEdit with a Java source (.java) highlighter.
}

unit SynHighlighterJava;

{$I SynEdit.inc}

interface

uses
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
  SysUtils, Classes;

type
  TtkTokenKind = (tkComment, tkDocument, tkIdentifier, tkInvalid, tkKey,
    tkNull, tkNumber, tkSpace, tkString, tkSymbol, tkUnknown);

  TxtkTokenKind = (
    xtkAdd, xtkAddAssign, xtkAnd, xtkAndAssign, xtkAssign, xtkBitComplement,
    xtkBraceClose, xtkBraceOpen, xtkColon, xtkCondAnd, xtkCondOr, xtkDecrement,
    xtkDivide, xtkDivideAssign, xtkGreaterThan, xtkGreaterThanEqual, xtkIncOr,
    xtkIncOrAssign, xtkIncrement, xtkLessThan, xtkLessThanEqual,
    xtkLogComplement, xtkLogEqual, xtkMultiply, xtkMultiplyAssign, xtkNotEqual,
    xtkPoint, xtkQuestion, xtkRemainder, xtkRemainderAssign, xtkRoundClose,
    xtkRoundOpen, xtkSemiColon, xtkShiftLeft, xtkShiftLeftAssign, xtkShiftRight,
    xtkShiftRightAssign, xtkSquareClose, xtkSquareOpen, xtkSubtract,
    xtkSubtractAssign, xtkUnsignShiftRight, xtkUnsignShiftRightAssign, xtkXor,
    xtkXorAssign, xtkComma);

  TRangeState = (rsANil, rsComment, rsDocument, rsUnknown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TSynJavaSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FRoundCount: Integer;
    FSquareCount: Integer;
    FTokenID: TtkTokenKind;
    FExtTokenID: TxtkTokenKind;
    FIdentFuncTable: array[0..112] of TIdentFuncTableFunc;
    FCommentAttri: TSynHighlighterAttributes;
    FDocumentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FInvalidAttri: TSynHighlighterAttributes;
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
    procedure CommentProc;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure AtSymbolProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure MultiplyProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure PoundProc;
    procedure QuestionProc;
    procedure RemainderSymbolProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StringProc;
    procedure TildeProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function GetExtTokenID: TxtkTokenKind;
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
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    property ExtTokenID: TxtkTokenKind read GetExtTokenID;
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property DocumentAttri: TSynHighlighterAttributes read FDocumentAttri
      write FDocumentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property InvalidAttri: TSynHighlighterAttributes read FInvalidAttri
      write FInvalidAttri;
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
  KeyWords: array[0..51] of UnicodeString = (
    'abstract', 'assert', 'boolean', 'break', 'byte', 'case', 'catch', 'char', 
    'class', 'const', 'continue', 'default', 'do', 'double', 'else', 'extends', 
    'false', 'final', 'finally', 'float', 'for', 'goto', 'if', 'implements', 
    'import', 'instanceof', 'int', 'interface', 'long', 'native', 'new', 'null', 
    'package', 'private', 'protected', 'public', 'return', 'short', 'static', 
    'strictfp', 'super', 'switch', 'synchronized', 'this', 'throw', 'throws', 
    'transient', 'true', 'try', 'void', 'volatile', 'while' 
  );

  KeyIndices: array[0..112] of Integer = (
    1, -1, -1, 45, -1, -1, 39, -1, -1, -1, 9, 36, 26, -1, -1, 4, 27, 5, 50, 25, 
    33, -1, 18, -1, 17, 6, 28, -1, -1, -1, 51, -1, -1, -1, -1, 21, 48, -1, 7, 3, 
    -1, -1, -1, 49, 41, -1, 35, -1, 46, 40, -1, -1, -1, 42, -1, -1, -1, -1, -1, 
    -1, 43, -1, -1, -1, -1, -1, 13, 24, -1, 37, -1, -1, 31, 11, -1, 22, -1, -1, 
    -1, 44, -1, 10, 19, 8, -1, -1, 38, 15, -1, -1, 34, -1, 14, -1, -1, -1, 0, 
    12, -1, 20, -1, 23, -1, 47, -1, -1, 29, 30, -1, -1, 16, 32, 2 
  );

{$Q-}
function TSynJavaSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 598 + Ord(Str^) * 349;
    Inc(Str);
  end;
  Result := Result mod 113;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynJavaSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynJavaSyn.InitIdent;
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

function TSynJavaSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynJavaSyn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

constructor TSynJavaSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);
  FDocumentAttri := TSynHighlighterAttributes.Create(SYNS_AttrDocumentation, SYNS_FriendlyAttrDocumentation);
  FDocumentAttri.Style := [fsItalic];
  AddAttribute(FDocumentAttri);
  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FInvalidAttri := TSynHighlighterAttributes.Create(SYNS_AttrInvalidSymbol, SYNS_FriendlyAttrInvalidSymbol);
  AddAttribute(FInvalidAttri);
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
  FRange := rsUnknown;
  SetAttributesOnChange(DefHighlightChange);

  InitIdent;
  FDefaultFilter := SYNS_FilterJava;
end; { Create }

procedure TSynJavaSyn.CommentProc;
begin
  if FRange = rsComment then
    FTokenID := tkComment
  else
    FTokenID := tkDocument;
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

  while not IsLineEnd(Run) do
    case FLine[Run] of
      '*':
        if FLine[Run + 1] = '/' then
        begin
          Inc(Run, 2);
          FRange := rsUnknown;
          Break;
        end
        else Inc(Run);
    else Inc(Run);
    end;
end;

procedure TSynJavaSyn.AndSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {and assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkAndAssign;
      end;
    '&':                               {conditional and}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkCondAnd;
      end;
  else                                 {and}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkAnd;
    end;
  end;
end;

procedure TSynJavaSyn.AsciiCharProc;
begin
  FTokenID := tkString;
  repeat
    if IsLineEnd(Run) then Break;
    if FLine[Run] = #92 then
      Inc(Run); // backslash, if we have an escaped single character, skip to the next
    if not IsLineEnd(Run) then Inc(Run); //Add check here to prevent overrun from backslash being last char
  until FLine[Run] = #39;
  if not IsLineEnd(Run) then Inc(Run);
end;

procedure TSynJavaSyn.AtSymbolProc;
begin
  FTokenID := tkInvalid;
  Inc(Run);
end;

procedure TSynJavaSyn.BraceCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkBraceClose;
end;

procedure TSynJavaSyn.BraceOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkBraceOpen;
end;

procedure TSynJavaSyn.CRProc;
begin
  FTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: Inc(Run, 2);
  else
    Inc(Run);
  end;
end;

procedure TSynJavaSyn.ColonProc;
begin
  Inc(Run);                            {colon - conditional}
  FTokenID := tkSymbol;
  FExtTokenID := xtkColon;
end;

procedure TSynJavaSyn.CommaProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkComma;
end;

procedure TSynJavaSyn.EqualProc;
begin
  case FLine[Run + 1] of
    '=':                               {logical equal}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkLogEqual;
      end;
  else                                 {assign}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkAssign;
    end;
  end;
end;

procedure TSynJavaSyn.GreaterProc;
begin
  case FLine[Run + 1] of
    '=':                               {greater than or equal to}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkGreaterThanEqual;
      end;
    '>':
      begin
        case FLine[Run + 2] of
          '=':                         {shift right assign}
            begin
            Inc(Run, 3);
            FExtTokenID := xtkShiftRightAssign;
            end;
          '>':
            if FLine[Run + 3] = '=' then
            begin
              Inc(Run, 4);             {unsigned shift right assign}
              FExtTokenID := xtkUnsignShiftRightAssign;
            end
            else
            begin
              Inc(Run, 3);             {unsigned shift right}
              FExtTokenID := xtkUnsignShiftRight;
            end;
        else                           {shift right}
          begin
            Inc(Run, 2);
            FExtTokenID := xtkShiftRight;
          end;
        end;
        FTokenID := tkSymbol;
      end;
  else                                 {greater than}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkGreaterThan;
    end;
  end;
end;

procedure TSynJavaSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + Run));
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do Inc(Run);
end;

procedure TSynJavaSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynJavaSyn.LowerProc;
begin
  case FLine[Run + 1] of
    '=':                               {less than or equal to}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkLessThanEqual;
      end;
    '<':
      begin
        if FLine[Run + 2] = '=' then   {shift left assign}
        begin
          Inc(Run, 3);
          FExtTokenID := xtkShiftLeftAssign;
        end
        else                           {shift left}
        begin
          Inc(Run, 2);
          FExtTokenID := xtkShiftLeft;
        end;
        FTokenID := tkSymbol;
      end;
  else                                 {less than}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkLessThan;
    end;
  end;
end;

procedure TSynJavaSyn.MinusProc;
begin
  case FLine[Run + 1] of
    '=':                               {subtract assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkSubtractAssign;
      end;
    '-':                               {decrement}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkDecrement;
      end;
  else                                 {subtract}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkSubtract;
    end;
  end;
end;

procedure TSynJavaSyn.MultiplyProc;
begin
  case FLine[Run + 1] of
    '=':                               {multiply assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkMultiplyAssign;
      end;
  else                                 {multiply}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkMultiply;
    end;
  end;
end;

procedure TSynJavaSyn.NotSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {not equal}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkNotEqual;
      end;
  else                                 {logical complement}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkLogComplement;
    end;
  end;
end;

procedure TSynJavaSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynJavaSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', '.', '-', 'l', 'L', 'x', 'X', 'A'..'F', 'a'..'f':
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

procedure TSynJavaSyn.OrSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {inclusive or assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkIncOrAssign;
      end;
    '|':                               {conditional or}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkCondOr;
      end;
  else                                 {inclusive or}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkIncOr;
    end;
  end;
end;

procedure TSynJavaSyn.PlusProc;
begin
  case FLine[Run + 1] of
    '=':                               {add assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkAddAssign;
      end;
    '+':                               {increment}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkIncrement;
      end;
  else                                 {add}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkAdd;
    end;
  end;
end;

procedure TSynJavaSyn.PointProc;
begin
  Inc(Run);                            {point}
  if CharInSet(FLine[Run], ['0'..'9']) then
  begin
    NumberProc;
    Exit;
  end;
  FTokenID := tkSymbol;
  FExtTokenID := xtkPoint;
end;

procedure TSynJavaSyn.PoundProc;
begin
  Inc(Run);
  FTokenID := tkInvalid;
end;

procedure TSynJavaSyn.QuestionProc;
begin
  FTokenID := tkSymbol;                {question mark - conditional}
  FExtTokenID := xtkQuestion;
  Inc(Run);
end;

procedure TSynJavaSyn.RemainderSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {remainder assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkRemainderAssign;
      end;
  else                                 {remainder}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkRemainder;
    end;
  end;
end;

procedure TSynJavaSyn.RoundCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundClose;
  Dec(FRoundCount);
end;

procedure TSynJavaSyn.RoundOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundOpen;
  Inc(FRoundCount);
end;

procedure TSynJavaSyn.SemiColonProc;
begin
  Inc(Run);                            {semicolon}
  FTokenID := tkSymbol;
  FExtTokenID := xtkSemiColon;
end;

procedure TSynJavaSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '/':                               {c++ style comments}
      begin
        Inc(Run, 2);
        FTokenID := tkComment;
        while not IsLineEnd(Run) do
        begin
          Inc(Run);
        end;
      end;
    '*':
      begin
        if (FLine[Run+2] = '*') and (FLine[Run+3] <> '/')then     {documentation comment}
        begin
          FRange := rsDocument;
          FTokenID := tkDocument;
          Inc(Run);
        end
        else                           {c style comment}
        begin
          FRange := rsComment;
          FTokenID := tkComment;
        end;

        Inc(Run, 2);
        while not IsLineEnd(Run) do
          case FLine[Run] of
            '*':
              if FLine[Run + 1] = '/' then
              begin
                Inc(Run, 2);
                FRange := rsUnknown;
                Break;
              end else Inc(Run);
          else
            Inc(Run);
          end;
      end;
    '=':                               {division assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkDivideAssign;
      end;
  else                                 {division}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkDivide;
    end;
  end;
end;

procedure TSynJavaSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynJavaSyn.SquareCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSquareClose;
  Dec(FSquareCount);
end;

procedure TSynJavaSyn.SquareOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSquareOpen;
  Inc(FSquareCount);
end;

procedure TSynJavaSyn.StringProc;
begin
  FTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then Inc(Run, 2);
  repeat
    if IsLineEnd(Run) then Break;
    case FLine[Run] of
      #92: Inc(Run);  // Backslash, if we have an escaped charcter it can be skipped
    end;
    if not IsLineEnd(Run) then Inc(Run); //Add check here to prevent overrun from backslash being last char
  until FLine[Run] = #34;
  if not IsLineEnd(Run) then Inc(Run);
end;

procedure TSynJavaSyn.TildeProc;
begin
  Inc(Run);                            {bitwise complement}
  FTokenID := tkSymbol;
  FExtTokenID := xtkBitComplement;
end;

procedure TSynJavaSyn.XOrSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {xor assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkXorAssign;
      end;
  else                                 {xor}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkXor;
    end;
  end;
end;

procedure TSynJavaSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynJavaSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsComment: CommentProc;
    rsDocument: CommentProc;
    else
    begin
      FRange := rsUnknown;
      case FLine[Run] of
        '&': AndSymbolProc;
        #39: AsciiCharProc;
        '@': AtSymbolProc;
        '}': BraceCloseProc;
        '{': BraceOpenProc;
        #13: CRProc;
        ':': ColonProc;
        ',': CommaProc;
        '=': EqualProc;
        '>': GreaterProc;
        'A'..'Z', 'a'..'z', '_', '$',
        WideChar(#$00C0)..WideChar(#$00D6),
        WideChar(#$00D8)..WideChar(#$00F6),
        WideChar(#$00F8)..WideChar(#$00FF): IdentProc;
        #10: LFProc;
        '<': LowerProc;
        '-': MinusProc;
        '*': MultiplyProc;
        '!': NotSymbolProc;
        #0: NullProc;
        '0'..'9': NumberProc;
        '|': OrSymbolProc;
        '+': PlusProc;
        '.': PointProc;
        '#': PoundProc;
        '?': QuestionProc;
        '%': RemainderSymbolProc;
        ')': RoundCloseProc;
        '(': RoundOpenProc;
        ';': SemiColonProc;
        '/': SlashProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        ']': SquareCloseProc;
        '[': SquareOpenProc;
        #34: StringProc;
        '~': TildeProc;
        '^': XOrSymbolProc;
        else UnknownProc;
      end;
    end;
  end;

  inherited;
end;

function TSynJavaSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
    else Result := nil;
  end;
end;

function TSynJavaSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynJavaSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

procedure TSynJavaSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynJavaSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynJavaSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynJavaSyn.GetExtTokenID: TxtkTokenKind;
begin
  Result := FExtTokenID;
end;

function TSynJavaSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkDocument: Result := FDocumentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkInvalid: Result := FInvalidAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FInvalidAttri;
    else Result := nil;
  end;
end;

function TSynJavaSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

function TSynJavaSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterJava;
end;

function TSynJavaSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', '$', '0'..'9', 'a'..'z', 'A'..'Z', WideChar(#$00C0)..WideChar(#$00D6),
      WideChar(#$00D8)..WideChar(#$00F6), WideChar(#$00F8)..WideChar(#$00FF):
      Result := True;
    else
      Result := False;
  end;
end;

class function TSynJavaSyn.GetLanguageName: string;
begin
  Result := SYNS_LangJava;
end;

function TSynJavaSyn.GetSampleSource: UnicodeString;
begin
  Result :=
    '/* Java syntax highlighting */'#13#10 +
    'import java.util.*;'#13#10 +
    #13#10 +
    '/** Example class */'#13#10 +
    'public class Sample {'#13#10 +
    '  public static void main(String[] args) {'#13#10 +
    '    int i = 0;'#13#10 +
    '    for(i = 0; i < 10; i++)'#13#10 +
    '      System.out.println("Hello world");'#13#10 +
    '  }'#13#10 +
    '}';
end;

class function TSynJavaSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangJava;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynJavaSyn);
{$ENDIF}
end.
