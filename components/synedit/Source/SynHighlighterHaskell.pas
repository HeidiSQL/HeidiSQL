{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterHaskell.pas, released 2001-10-28
The Original Code is based on the SynHighlighterCpp.pas, released 2000-04-10
which in turn was based on the dcjCppSyn.pas file from the mwEdit component
suite by Martin Waldenburg and other developers, the Initial Author of this file
is Michael Trier.
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

You may retrieve the latest version of SynEdit from the SynEdit home page,
located at http://SynEdit.SourceForge.net

You may retrieve the latest version of this file from
http://www.ashleybrown.co.uk/synedit/

-------------------------------------------------------------------------------}
{
@abstract(Provides a Haskell syntax highlighter for SynEdit)
@author(Ashley Brown)
@created(2001)
@lastmod(2000-10-26)
The SynHighlighterHaskell unit provides SynEdit with a Haskell syntax highlighter.
Based on SynHighlighterCpp.

http://haskell.org/
http://www.ashleybrown.co.uk/
ashley@ashleybrown.co.uk
}

unit SynHighlighterHaskell;

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
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull,
    tkNumber, tkSpace, tkString, tkSymbol, tkUnknown);

  TxtkTokenKind = (
    xtkAdd, xtkAddAssign, xtkAnd, xtkAndAssign, xtkArrow, xtkAssign,
    xtkBitComplement, xtkBraceClose, xtkBraceOpen, xtkColon, xtkComma,
    xtkDecrement, xtkDivide, xtkDivideAssign, xtkEllipse, xtkGreaterThan,
    xtkGreaterThanEqual, xtkIncOr, xtkIncOrAssign, xtkIncrement, xtkLessThan,
    xtkLessThanEqual, xtkLogAnd, xtkLogComplement, xtkLogEqual, xtkLogOr,
    xtkMod, xtkModAssign, xtkMultiplyAssign, xtkNotEqual, xtkPoint, xtkQuestion,
    xtkRoundClose, xtkRoundOpen, xtkScopeResolution, xtkSemiColon, xtkShiftLeft,
    xtkShiftLeftAssign, xtkShiftRight, xtkShiftRightAssign, xtkSquareClose,
    xtkSquareOpen, xtkStar, xtkSubtract, xtkSubtractAssign, xtkXor,
    xtkXorAssign);

  TRangeState = (rsUnknown, rsAnsiC, rsAnsiCAsm, rsAnsiCAsmBlock, rsAsm,
    rsAsmBlock, rsDirective, rsDirectiveComment, rsString34, rsString39);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TSynHaskellSyn = class(TSynCustomHighlighter)
  private
    FAsmStart: Boolean;
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FExtTokenID: TxtkTokenKind;
    FIdentFuncTable: array[0..28] of TIdentFuncTableFunc;
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
    procedure AnsiCProc;
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
    procedure ModSymbolProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure QuestionProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StarProc;
    procedure StringProc;
    procedure TildeProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function GetExtTokenID: TxtkTokenKind;
    function IsFilterStored: Boolean; override;
  public
    class function GetCapabilities: TSynHighlighterCapabilities; override;
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
    procedure EnumUserSettings(settings: TStrings); override;
    property ExtTokenID: TxtkTokenKind read GetExtTokenID;
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
  Windows,
  SynEditStrConst;

const
  KeyWords: array[0..23] of UnicodeString = (
    'Bool', 'Char', 'class', 'data', 'deriving', 'Double', 'else', 'False', 
    'Float', 'if', 'import', 'in', 'instance', 'Int', 'Integer', 'IO', 'let', 
    'module', 'otherwise', 'String', 'then', 'True', 'type', 'where' 
  );

  KeyIndices: array[0..28] of Integer = (
    2, 23, 10, 16, 7, -1, 22, 8, 14, 17, 5, 4, 11, -1, 1, 9, 12, 0, -1, 6, -1, 
    3, 15, 18, 20, -1, 13, 19, 21 
  );

{$Q-}
function TSynHaskellSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 904 + Ord(Str^) * 779;
    Inc(Str);
  end;
  Result := Result mod 29;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynHaskellSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynHaskellSyn.InitIdent;
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

function TSynHaskellSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynHaskellSyn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

constructor TSynHaskellSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);
  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style:= [fsBold];
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
  FRange := rsUnknown;
  FAsmStart := False;
  FDefaultFilter := SYNS_FilterHaskell;
end; { Create }

procedure TSynHaskellSyn.AnsiCProc;
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
          Inc(Run, 2);
          if FRange = rsAnsiCAsm then
            FRange := rsAsm
          else if FRange = rsAnsiCAsmBlock then
            FRange := rsAsmBlock
          else if FRange = rsDirectiveComment then
            FRange := rsDirective
          else
            FRange := rsUnknown;
          Break;
        end else
          Inc(Run);
      #10, #13:
        Break;
    else Inc(Run);
    end;
end;

procedure TSynHaskellSyn.AndSymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {and assign}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkAndAssign;
      end;
    '&':                               {logical and}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkLogAnd;
      end;
  else                                 {and}
    begin
      Inc(Run);
      FExtTokenID := xtkAnd;
    end;
  end;
end;

procedure TSynHaskellSyn.AsciiCharProc;
begin
  FTokenID := tkString;
  repeat
    if FLine[Run] = '\' then begin
      if CharInSet(FLine[Run + 1], [#39, '\']) then
        Inc(Run);
    end;
    Inc(Run);
  until IsLineEnd(Run) or (FLine[Run] = #39);
  if FLine[Run] = #39 then
    Inc(Run);
end;

procedure TSynHaskellSyn.AtSymbolProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

procedure TSynHaskellSyn.BraceCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkBraceClose;
  if FRange = rsAsmBlock then FRange := rsUnknown;
end;

procedure TSynHaskellSyn.BraceOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkBraceOpen;
  if FRange = rsAsm then
  begin
    FRange := rsAsmBlock;
    FAsmStart := True;
  end;
end;

procedure TSynHaskellSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run + 1] = #10 then Inc(Run);
end;

procedure TSynHaskellSyn.ColonProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run + 1] of
    ':':                               {scope resolution operator}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkScopeResolution;
      end;
  else                                 {colon}
    begin
      Inc(Run);
      FExtTokenID := xtkColon;
    end;
  end;
end;

procedure TSynHaskellSyn.CommaProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkComma;
end;

procedure TSynHaskellSyn.EqualProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {logical equal}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkLogEqual;
      end;
  else                                 {assign}
    begin
      Inc(Run);
      FExtTokenID := xtkAssign;
    end;
  end;
end;

procedure TSynHaskellSyn.GreaterProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {greater than or equal to}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkGreaterThanEqual;
      end;
    '>':
      begin
        if FLine[Run + 2] = '=' then   {shift right assign}
        begin
          Inc(Run, 3);
          FExtTokenID := xtkShiftRightAssign;
        end
        else                           {shift right}
        begin
          Inc(Run, 2);
          FExtTokenID := xtkShiftRight;
        end;
      end;
  else                                 {greater than}
    begin
      Inc(Run);
      FExtTokenID := xtkGreaterThan;
    end;
  end;
end;

procedure TSynHaskellSyn.QuestionProc;
begin
  FTokenID := tkSymbol;                {conditional}
  FExtTokenID := xtkQuestion;
  Inc(Run);
end;

procedure TSynHaskellSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + Run));
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do Inc(Run);
end;

procedure TSynHaskellSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynHaskellSyn.LowerProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {less than or equal to}
      begin
        Inc(Run, 2);
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
      end;
  else                                 {less than}
    begin
      Inc(Run);
      FExtTokenID := xtkLessThan;
    end;
  end;
end;

procedure TSynHaskellSyn.MinusProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {subtract assign}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkSubtractAssign;
      end;
    '-':                               {decrement}
      begin
        FTokenID := tkComment;
        Inc(Run, 2);
        while not IsLineEnd(Run) do Inc(Run);
      end;
    '>':                               {arrow}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkArrow;
      end;
  else                                 {subtract}
    begin
      Inc(Run);
      FExtTokenID := xtkSubtract;
    end;
  end;
end;

procedure TSynHaskellSyn.ModSymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {mod assign}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkModAssign;
      end;
  else                                 {mod}
    begin
      Inc(Run);
      FExtTokenID := xtkMod;
    end;
  end;
end;

procedure TSynHaskellSyn.NotSymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {not equal}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkNotEqual;
      end;
  else                                 {not}
    begin
      Inc(Run);
      FExtTokenID := xtkLogComplement;
    end;
  end;
end;

procedure TSynHaskellSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynHaskellSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f', '.', 'u', 'U', 'l', 'L', 'x', 'X':
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

procedure TSynHaskellSyn.OrSymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {or assign}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkIncOrAssign;
      end;
    '|':                               {logical or}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkLogOr;
      end;
  else                                 {or}
    begin
      Inc(Run);
      FExtTokenID := xtkIncOr;
    end;
  end;
end;

procedure TSynHaskellSyn.PlusProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {add assign}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkAddAssign;
      end;
    '+':                               {increment}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkIncrement;
      end;
  else                                 {add}
    begin
      Inc(Run);
      FExtTokenID := xtkAdd;
    end;
  end;
end;

procedure TSynHaskellSyn.PointProc;
begin
  FTokenID := tkSymbol;
  if (FLine[Run + 1] = '.') and (FLine[Run + 2] = '.') then
    begin                              {ellipse}
      Inc(Run, 3);
      FExtTokenID := xtkEllipse;
    end
  else                                 {point}
    begin
      Inc(Run);
      FExtTokenID := xtkPoint;
    end;
end;

procedure TSynHaskellSyn.RoundCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundClose;
end;

procedure TSynHaskellSyn.RoundOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundOpen;
end;

procedure TSynHaskellSyn.SemiColonProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSemiColon;
  if FRange = rsAsm then FRange := rsUnknown;
end;

procedure TSynHaskellSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '=':                               {divide assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkDivideAssign;
      end;
  else                                 {divide}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkDivide;
    end;
  end;
end;

procedure TSynHaskellSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynHaskellSyn.SquareCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSquareClose;
end;

procedure TSynHaskellSyn.SquareOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSquareOpen;
end;

procedure TSynHaskellSyn.StarProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {multiply assign}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkMultiplyAssign;
      end;
  else                                 {star}
    begin
      Inc(Run);
      FExtTokenID := xtkStar;
    end;
  end;
end;

procedure TSynHaskellSyn.StringProc;
begin
  FTokenID := tkString;
  repeat
    if FLine[Run] = '\' then begin
      if CharInSet(FLine[Run + 1], [#34, '\']) then
        Inc(Run);
    end;
    Inc(Run);
  until IsLineEnd(Run) or (FLine[Run] = #34);
  if FLine[Run] = #34 then
    Inc(Run);
end;

procedure TSynHaskellSyn.TildeProc;
begin
  Inc(Run);                            {bitwise complement}
  FTokenID := tkSymbol;
  FExtTokenID := xtkBitComplement;
end;

procedure TSynHaskellSyn.XOrSymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {xor assign}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkXorAssign;
      end;
  else                                 {xor}
    begin
      Inc(Run);
      FExtTokenID := xtkXor;
    end;
  end;
end;

procedure TSynHaskellSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynHaskellSyn.Next;
begin
  FAsmStart := False;
  FTokenPos := Run;
  case FRange of
    rsAnsiC, rsAnsiCAsm,
    rsAnsiCAsmBlock: AnsiCProc;
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
        '?': QuestionProc;
        'A'..'Z', 'a'..'z', '_': IdentProc;
        #10: LFProc;
        '<': LowerProc;
        '-': MinusProc;
        '%': ModSymbolProc;
        '!': NotSymbolProc;
        #0: NullProc;
        '0'..'9': NumberProc;
        '|': OrSymbolProc;
        '+': PlusProc;
        '.': PointProc;
        ')': RoundCloseProc;
        '(': RoundOpenProc;
        ';': SemiColonProc;
        '/': SlashProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        ']': SquareCloseProc;
        '[': SquareOpenProc;
        '*': StarProc;
        #34: StringProc;
        '~': TildeProc;
        '^': XOrSymbolProc;
        else UnknownProc;
      end;
    end;
  end;
  inherited;
end;

function TSynHaskellSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    else Result := nil;
  end;
end;

function TSynHaskellSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynHaskellSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynHaskellSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynHaskellSyn.GetExtTokenID: TxtkTokenKind;
begin
  Result := FExtTokenID;
end;

function TSynHaskellSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    else Result := nil;
  end;
end;

function TSynHaskellSyn.GetTokenKind: Integer;
begin
  Result := Ord(GetTokenID);
end;

procedure TSynHaskellSyn.ResetRange;
begin
  FRange:= rsUnknown;
end;

procedure TSynHaskellSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynHaskellSyn.EnumUserSettings(settings: TStrings);
begin
  { returns the user settings that exist in the registry }
  with TBetterRegistry.Create do
  begin
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly('\SOFTWARE\Borland\C++Builder') then
      begin
        try
          GetKeyNames(settings);
        finally
          CloseKey;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

function TSynHaskellSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterHaskell;
end;

function TSynHaskellSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', '0'..'9', 'a'..'z', 'A'..'Z', #39:
      Result := True;
    else
      Result := False;
  end;
end;

class function TSynHaskellSyn.GetLanguageName: string;
begin
  Result := SYNS_LangHaskell;
end;

class function TSynHaskellSyn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

function TSynHaskellSyn.GetSampleSource: UnicodeString;
begin
  Result :=
    '-- Haskell Sample Source'#13#10 +
    'tail :: [a] -> [a]'#13#10 +
    'tail (x:xs) = xs'#13#10 +
    '';
end;

class function TSynHaskellSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangHaskell;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynHaskellSyn);
{$ENDIF}
end.
