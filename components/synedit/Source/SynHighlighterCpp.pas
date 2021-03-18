{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterCpp.pas, released 2000-04-10.
The Original Code is based on the dcjCppSyn.pas file from the
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

$Id: SynHighlighterCpp.pas,v 1.22.2.9 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a C++ syntax highlighter for SynEdit)
@author(Michael Trier)
@created(1998)
@lastmod(2001-11-21)
The SynHighlighterCpp unit provides SynEdit with a C++ syntax highlighter.
Thanks to Martin Waldenburg.
}

unit SynHighlighterCpp;

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
  TtkTokenKind = (tkAsm, tkComment, tkDirective, tkIdentifier, tkKey, tkNull,
    tkNumber, tkSpace, tkString, tkSymbol, tkUnknown,
    tkChar, tkFloat, tkHex, tkOctal);

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
    rsAsmBlock, rsDirective, rsDirectiveComment, rsMultiLineString,
    rsMultiLineDirective);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TSynCppSyn = class(TSynCustomHighlighter)
  private
    FAsmStart: Boolean;
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FExtTokenID: TxtkTokenKind;
    FIdentFuncTable: array[0..342] of TIdentFuncTableFunc;
    FAsmAttri: TSynHighlighterAttributes;
    FCommentAttri: TSynHighlighterAttributes;
    FDirecAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FInvalidAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FFloatAttri: TSynHighlighterAttributes;
    FHexAttri: TSynHighlighterAttributes;
    FOctalAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FCharAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function FuncAsm(Index: Integer): TtkTokenKind;
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
    procedure DirectiveProc;
    procedure DirectiveEndProc;
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
    procedure StringEndProc;
  protected
    function GetExtTokenID: TxtkTokenKind;
    function GetSampleSource: UnicodeString; override;
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
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function UseUserSettings(settingIndex: Integer): Boolean; override;
    procedure EnumUserSettings(settings: TStrings); override;

    property ExtTokenID: TxtkTokenKind read GetExtTokenID;
  published
    property AsmAttri: TSynHighlighterAttributes read FAsmAttri write FAsmAttri;
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property DirecAttri: TSynHighlighterAttributes read FDirecAttri
      write FDirecAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property InvalidAttri: TSynHighlighterAttributes read FInvalidAttri
      write FInvalidAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property FloatAttri: TSynHighlighterAttributes read FFloatAttri
      write FFloatAttri;
    property HexAttri: TSynHighlighterAttributes read FHexAttri
      write FHexAttri;
    property OctalAttri: TSynHighlighterAttributes read FOctalAttri
      write FOctalAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri
      write FStringAttri;
    property CharAttri: TSynHighlighterAttributes read FCharAttri
      write FCharAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
  end;

implementation

uses
  Windows,
  SynEditStrConst;

const
  KeyWords: array[0..94] of UnicodeString = (
    '__asm', '__automated', '__cdecl', '__classid', '__closure', '__declspec', 
    '__dispid', '__except', '__export', '__fastcall', '__finally', '__import', 
    '__int16', '__int32', '__int64', '__int8', '__pascal', '__property', 
    '__published', '__rtti', '__stdcall', '__thread', '__try', '_asm', '_cdecl', 
    '_export', '_fastcall', '_import', '_pascal', '_stdcall', 'asm', 'auto', 
    'bool', 'break', 'case', 'catch', 'cdecl', 'char', 'class', 'const', 
    'const_cast', 'continue', 'default', 'delete', 'do', 'double', 
    'dynamic_cast', 'else', 'enum', 'explicit', 'extern', 'false', 'float', 
    'for', 'friend', 'goto', 'if', 'inline', 'int', 'interface', 'long', 
    'mutable', 'namespace', 'new', 'operator', 'pascal', 'private', 'protected', 
    'public', 'register', 'reinterpret_cast', 'return', 'short', 'signed', 
    'sizeof', 'static', 'static_cast', 'struct', 'switch', 'template', 'this', 
    'throw', 'true', 'try', 'typedef', 'typeid', 'typename', 'union', 
    'unsigned', 'using', 'virtual', 'void', 'volatile', 'wchar_t', 'while' 
  );

  KeyIndices: array[0..342] of Integer = (
    -1, 34, -1, -1, 57, 72, -1, 39, -1, 9, -1, 86, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 88, -1, 12, 66, -1, -1, -1, -1, -1, 42, -1, -1, -1, -1, -1, 56, 51, 
    40, 87, 77, -1, -1, -1, -1, 64, -1, -1, -1, -1, -1, -1, -1, -1, -1, 28, 41, 
    -1, 63, 6, -1, -1, -1, -1, -1, -1, -1, -1, 55, 65, 0, -1, -1, -1, -1, -1, 
    -1, 26, 83, -1, 38, 92, -1, -1, 93, 33, -1, -1, -1, -1, -1, -1, -1, 35, -1, 
    -1, -1, -1, -1, -1, -1, 79, 27, -1, -1, -1, 43, -1, -1, 20, -1, -1, 31, -1, 
    -1, -1, -1, -1, 89, -1, -1, -1, -1, 59, -1, 58, -1, -1, 46, -1, -1, 3, -1, 
    -1, 17, -1, 54, -1, 45, -1, -1, -1, -1, -1, -1, 53, -1, -1, -1, 1, -1, -1, 
    -1, -1, 44, 90, 32, -1, -1, -1, -1, -1, -1, 91, 13, -1, -1, -1, 60, -1, -1, 
    -1, -1, -1, 49, -1, -1, -1, -1, -1, -1, 75, -1, -1, 76, -1, -1, -1, -1, 30, 
    68, 23, 82, -1, 15, -1, -1, 2, -1, 70, -1, -1, -1, 73, 18, -1, -1, -1, -1, 
    -1, 47, 24, 52, 14, 84, -1, -1, -1, -1, -1, 25, -1, -1, -1, 80, 69, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 21, -1, 19, -1, -1, -1, 
    -1, -1, -1, 74, -1, -1, -1, 29, -1, -1, -1, 67, -1, 7, -1, -1, -1, 50, 61, 
    -1, -1, -1, 4, -1, 94, 85, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    81, -1, -1, -1, -1, -1, 10, 16, -1, -1, 36, 37, -1, -1, -1, 8, -1, 22, -1, 
    -1, -1, -1, 78, 62, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 71, -1, -1, -1, 5, -1, -1, -1, -1, -1, -1, -1, 11, -1, 48, 
    -1 
  );

{$Q-}
function TSynCppSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 179 + Ord(Str^) * 44;
    Inc(Str);
  end;
  Result := Result mod 343;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynCppSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynCppSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[70] := FuncAsm;
  FIdentFuncTable[191] := FuncAsm;
  FIdentFuncTable[189] := FuncAsm;

  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if @FIdentFuncTable[i] = nil then
      FIdentFuncTable[i] := KeyWordFunc;
end;

function TSynCppSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynCppSyn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

function TSynCppSyn.FuncAsm(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    Result := tkKey;
    FRange := rsAsm;
    FAsmStart := True;
  end
  else
    Result := tkIdentifier
end;

constructor TSynCppSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

  FAsmAttri := TSynHighlighterAttributes.Create(SYNS_AttrAssembler, SYNS_FriendlyAttrAssembler);
  AddAttribute(FAsmAttri);
  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style:= [fsItalic];
  AddAttribute(FCommentAttri);
  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FInvalidAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(FInvalidAttri);
  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style:= [fsBold];
  AddAttribute(FKeyAttri);
  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);
  FCharAttri := TSynHighlighterAttributes.Create(SYNS_AttrCharacter, SYNS_FriendlyAttrCharacter);
  AddAttribute(FCharAttri);
  FFloatAttri := TSynHighlighterAttributes.Create(SYNS_AttrFloat, SYNS_FriendlyAttrFloat);
  AddAttribute(FFloatAttri);
  FHexAttri := TSynHighlighterAttributes.Create(SYNS_AttrHexadecimal, SYNS_FriendlyAttrHexadecimal);
  AddAttribute(FHexAttri);
  FOctalAttri := TSynHighlighterAttributes.Create(SYNS_AttrOctal, SYNS_FriendlyAttrOctal);
  AddAttribute(FOctalAttri);
  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);
  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  FDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  AddAttribute(FDirecAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  FRange := rsUnknown;
  FAsmStart := False;
  FDefaultFilter := SYNS_FilterCPP;
end;

procedure TSynCppSyn.AnsiCProc;
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
          else if (FRange = rsDirectiveComment) and
            not IsLineEnd(Run) then
              FRange := rsMultiLineDirective
          else
            FRange := rsUnknown;
          Break;
        end else
          Inc(Run);
      #10, #13:
        Break;
      else
        Inc(Run);
    end;
end;

procedure TSynCppSyn.AndSymbolProc;
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

procedure TSynCppSyn.AsciiCharProc;
begin
  FTokenID := tkChar;
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

procedure TSynCppSyn.AtSymbolProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

procedure TSynCppSyn.BraceCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkBraceClose;
  if FRange = rsAsmBlock then FRange := rsUnknown;
end;

procedure TSynCppSyn.BraceOpenProc;
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

procedure TSynCppSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run + 1] = #10 then Inc(Run);
end;

procedure TSynCppSyn.ColonProc;
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

procedure TSynCppSyn.CommaProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkComma;
end;

procedure TSynCppSyn.DirectiveProc;
begin
  if WideTrim(FLine)[1] <> '#' then // '#' is not first char on the line, treat it as an invalid char
  begin
    FTokenID := tkUnknown;
    Inc(Run);
    Exit;
  end;
  FTokenID := tkDirective;
  repeat
    if FLine[Run] = '/' then // comment?
    begin
      if FLine[Run + 1] = '/' then // is end of directive as well
      begin
        FRange := rsUnknown;
        Exit;
      end
      else
        if FLine[Run + 1] = '*' then // might be embedded only
        begin
          FRange := rsDirectiveComment;
          Exit;
        end;
    end;
    if (FLine[Run] = '\') and (FLine[Run +1 ] = #0) then // a multiline directive
    begin
      Inc(Run);
      FRange := rsMultiLineDirective;
      Exit;
    end;
    Inc(Run);
  until IsLineEnd(Run)
end;

procedure TSynCppSyn.DirectiveEndProc;
begin
  FTokenID := tkDirective;
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
      '/': // comment?
        begin
          case FLine[Run + 1] of
            '/': // is end of directive as well
              begin
                FRange := rsUnknown;
                Exit;
              end;
            '*': // might be embedded only
              begin
                FRange := rsDirectiveComment;
                Exit;
              end;
          end;
        end;
      '\': // yet another line?
        begin
          if FLine[Run + 1] = #0 then
          begin
            Inc(Run);
            FRange := rsMultiLineDirective;
            Exit;
          end;
        end;
    end;
    Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynCppSyn.EqualProc;
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

procedure TSynCppSyn.GreaterProc;
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

procedure TSynCppSyn.QuestionProc;
begin
  FTokenID := tkSymbol;                {conditional}
  FExtTokenID := xtkQuestion;
  Inc(Run);
end;

procedure TSynCppSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + Run));
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do Inc(Run);
end;

procedure TSynCppSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynCppSyn.LowerProc;
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

procedure TSynCppSyn.MinusProc;
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
        Inc(Run, 2);
        FExtTokenID := xtkDecrement;
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

procedure TSynCppSyn.ModSymbolProc;
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

procedure TSynCppSyn.NotSymbolProc;
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

procedure TSynCppSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynCppSyn.NumberProc;

  function IsNumberChar(Run: Integer): Boolean;
  begin
    case FLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f', '.', 'u', 'U', 'l', 'L', 'x', 'X', '-', '+':
        Result := True;
      else
        Result := False;
    end;
  end;

  function IsDigitPlusMinusChar(Run: Integer): Boolean;
  begin
    case FLine[Run] of
      '0'..'9', '+', '-':
        Result := True;
      else
        Result := False;
    end;
  end;

  function IsHexDigit(Run: Integer): Boolean;
  begin
    case FLine[Run] of
      '0'..'9', 'a'..'f', 'A'..'F':
        Result := True;
      else
        Result := False;
    end;
  end;

  function IsAlphaUncerscore(Run: Integer): Boolean;
  begin
    case FLine[Run] of
      'A'..'Z', 'a'..'z', '_':
        Result := True;
      else
        Result := False;
    end;
  end;

var
  idx1: Integer; // token[1]
  i: Integer;
begin
  idx1 := Run;
  Inc(Run);
  FTokenID := tkNumber;
  while IsNumberChar(Run) do
  begin
    case FLine[Run] of
      '.':
        if FLine[Succ(Run)] = '.' then
          Break
        else
          if (FTokenID <> tkHex) then
            FTokenID := tkFloat
          else // invalid
          begin
            FTokenID := tkUnknown;
            Exit;
          end;
      '-', '+':
        begin
          if FTokenID <> tkFloat then // number <> float. an arithmetic operator
            Exit;
          if not CharInSet(FLine[Pred(Run)], ['e', 'E']) then
            Exit; // number = float, but no exponent. an arithmetic operator
          if not IsDigitPlusMinusChar(Succ(Run)) then // invalid
          begin
            Inc(Run);
            FTokenID := tkUnknown;
            Exit;
          end
        end;
      '0'..'7':
        if (Run = Succ(idx1)) and (FLine[idx1] = '0') then // octal number
          FTokenID := tkOctal;
      '8', '9':
        if (FLine[idx1] = '0') and
           ((FTokenID <> tkHex) and (FTokenID <> tkFloat)) then // invalid octal char
             FTokenID := tkUnknown;
      'a'..'d', 'A'..'D':
        if FTokenID <> tkHex then // invalid char
          Break;
      'e', 'E':
        if (FTokenID <> tkHex) then
          if CharInSet(FLine[Pred(Run)], ['0'..'9']) then // exponent
          begin
            for i := idx1 to Pred(Run) do
              if CharInSet(FLine[i], ['e', 'E']) then // too many exponents
              begin
                FTokenID := tkUnknown;
                Exit;
              end;
            if not IsDigitPlusMinusChar(Succ(Run)) then
              Break
            else
              FTokenID := tkFloat
          end
          else // invalid char
            Break;
      'f', 'F':
        if FTokenID <> tkHex then
        begin
          for i := idx1 to Pred(Run) do
            if CharInSet(FLine[i], ['f', 'F']) then // declaration syntax error
            begin
              FTokenID := tkUnknown;
              Exit;
            end;
          if FTokenID = tkFloat then
          begin
            if CharInSet(FLine[Pred(Run)], ['l', 'L']) then // can't mix
              Break;
          end
          else
            FTokenID := tkFloat;
        end;
      'l', 'L':
        begin
          for i := idx1 to Run - 2 do
            if CharInSet(FLine[i], ['l', 'L']) then // declaration syntax error
            begin
              FTokenID := tkUnknown;
              Exit;
            end;
          if FTokenID = tkFloat then
            if CharInSet(FLine[Pred(Run)], ['f', 'F']) then // can't mix
              Break;
        end;
      'u', 'U':
        if FTokenID = tkFloat then // not allowed
          Break
        else
          for i := idx1 to Pred(Run) do
            if CharInSet(FLine[i], ['u', 'U']) then // declaration syntax error
            begin
              FTokenID := tkUnknown;
              Exit;
            end;
      'x', 'X':
        if (Run = Succ(idx1)) and   // 0x... 'x' must be second char
           (FLine[idx1] = '0') and  // 0x...
           IsHexDigit(Succ(Run)) then // 0x... must be continued with a number
             FTokenID := tkHex
           else // invalid char
           begin
             if not IsIdentChar(FLine[Succ(Run)]) and
                CharInSet(FLine[Succ(idx1)], ['x', 'X']) then
             begin
               Inc(Run); // highlight 'x' too
               FTokenID := tkUnknown;
             end;
             Break;
           end;
    end; // case
    Inc(Run);
  end; // while
  if IsAlphaUncerscore(Run) then
    FTokenID := tkUnknown;
end;

procedure TSynCppSyn.OrSymbolProc;
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

procedure TSynCppSyn.PlusProc;
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

procedure TSynCppSyn.PointProc;
begin
  FTokenID := tkSymbol;
  if (FLine[Run + 1] = '.') and (FLine[Run + 2] = '.') then
    begin                              {ellipse}
      Inc(Run, 3);
      FExtTokenID := xtkEllipse;
    end
  else
    if CharInSet(FLine[Run + 1], ['0'..'9']) then // float
    begin
      Dec(Run); // numberproc must see the point
      NumberProc;
    end
  else                                 {point}
    begin
      Inc(Run);
      FExtTokenID := xtkPoint;
    end;
end;

procedure TSynCppSyn.RoundCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundClose;
end;

procedure TSynCppSyn.RoundOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundOpen;
end;

procedure TSynCppSyn.SemiColonProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSemiColon;
  if FRange = rsAsm then FRange := rsUnknown;
end;

procedure TSynCppSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '/':                               {c++ style comments}
      begin
        FTokenID := tkComment;
        Inc(Run, 2);
        while not IsLineEnd(Run) do Inc(Run);
      end;
    '*':                               {c style comments}
      begin
        FTokenID := tkComment;
        if FRange = rsAsm then
          FRange := rsAnsiCAsm
        else if FRange = rsAsmBlock then
          FRange := rsAnsiCAsmBlock
        else if FRange <> rsDirectiveComment then
          FRange := rsAnsiC;
        Inc(Run, 2);
        while FLine[Run] <> #0 do
          case FLine[Run] of
            '*':
              if FLine[Run + 1] = '/' then
              begin
                Inc(Run, 2);
                if FRange = rsDirectiveComment then
                  FRange := rsMultiLineDirective
                else if FRange = rsAnsiCAsm then
                  FRange := rsAsm
                else
                  begin
                  if FRange = rsAnsiCAsmBlock then
                    FRange := rsAsmBlock
                  else
                    FRange := rsUnknown;
                  end;
                Break;
              end else Inc(Run);
            #10, #13:
              begin
                if FRange = rsDirectiveComment then
                  FRange := rsAnsiC;
                Break;
              end;
          else Inc(Run);
          end;
      end;
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

procedure TSynCppSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynCppSyn.SquareCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSquareClose;
end;

procedure TSynCppSyn.SquareOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSquareOpen;
end;

procedure TSynCppSyn.StarProc;
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

procedure TSynCppSyn.StringProc;
begin
  FTokenID := tkString;
  repeat
    if FLine[Run] = '\' then begin
      case FLine[Run + 1] of
        #34, '\':
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
  until IsLineEnd(Run) or (FLine[Run] = #34);
  if FLine[Run] = #34 then
    Inc(Run);
end;

procedure TSynCppSyn.StringEndProc;
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
            #34, '\':
              Inc(Run);
            #00:
              begin
                Inc(Run);
                FRange := rsMultilineString;
                Exit;
              end;
          end;
        end;
      #34: Break;
    end;
    Inc(Run);
  until IsLineEnd(Run) or (FLine[Run] = #34);
  if FLine[Run] = #34 then
    Inc(Run);
end;

procedure TSynCppSyn.TildeProc;
begin
  Inc(Run);                            {bitwise complement}
  FTokenID := tkSymbol;
  FExtTokenID := xtkBitComplement;
end;

procedure TSynCppSyn.XOrSymbolProc;
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

procedure TSynCppSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynCppSyn.Next;
begin
  FAsmStart := False;
  FTokenPos := Run;
  case FRange of
    rsAnsiC, rsAnsiCAsm,
    rsAnsiCAsmBlock, rsDirectiveComment: AnsiCProc;
    rsMultiLineDirective: DirectiveEndProc;
    rsMultilineString: StringEndProc;
  else
    begin
      case FLine[Run] of
        '&': AndSymbolProc;
        #39: AsciiCharProc;
        '@': AtSymbolProc;
        '}': BraceCloseProc;
        '{': BraceOpenProc;
        #13: CRProc;
        ':': ColonProc;
        ',': CommaProc;
        '#': DirectiveProc;
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

function TSynCppSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynCppSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynCppSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynCppSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
  if ((FRange = rsAsm) or (FRange = rsAsmBlock)) and not FAsmStart
    and not (FTokenID in [tkComment, tkSpace, tkNull])
  then
    Result := tkAsm;
end;

function TSynCppSyn.GetExtTokenID: TxtkTokenKind;
begin
  Result := FExtTokenID;
end;

function TSynCppSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  FTokenID := GetTokenID;
  case FTokenID of
    tkAsm: Result := FAsmAttri;
    tkComment: Result := FCommentAttri;
    tkDirective: Result := FDirecAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkFloat: Result := FFloatAttri;
    tkHex: Result := FHexAttri;
    tkOctal: Result := FOctalAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkChar: Result := FCharAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FInvalidAttri;
    else Result := nil;
  end;
end;

function TSynCppSyn.GetTokenKind: Integer;
begin
  Result := Ord(GetTokenID);
end;

procedure TSynCppSyn.ResetRange;
begin
  FRange:= rsUnknown;
end;

procedure TSynCppSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynCppSyn.EnumUserSettings(settings: TStrings);
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

function TSynCppSyn.UseUserSettings(settingIndex: Integer): Boolean;
// Possible parameter values:
//   index into TStrings returned by EnumUserSettings
// Possible return values:
//   True : settings were read and used
//   False: problem reading settings or invalid version specified - old settings
//          were preserved

  function ReadCPPBSettings(settingIndex: Integer): Boolean;

    function ReadCPPBSetting(settingTag: string; attri: TSynHighlighterAttributes; key: string): Boolean;

      function ReadCPPB1(settingTag: string; attri: TSynHighlighterAttributes; name: string): Boolean;
      var
        i: Integer;
      begin
        for i := 1 to Length(name) do
          if name[i] = ' ' then name[i] := '_';
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
             '\SOFTWARE\Borland\C++Builder\' + settingTag + '\Highlight', name, True);
      end; { ReadCPPB1 }

      function ReadCPPB3OrMore(settingTag: string; attri: TSynHighlighterAttributes; key: string): Boolean;
      begin
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
          '\Software\Borland\C++Builder\' + settingTag + '\Editor\Highlight',
          key, False);
      end; { ReadCPPB3OrMore }

    begin { ReadCPPBSetting }
      try
        if (settingTag[1] = '1') then
          Result := ReadCPPB1(settingTag,attri,key)
        else
          Result := ReadCPPB3OrMore(settingTag,attri,key);
      except
        Result := False;
      end;
    end; { ReadCPPBSetting }

  var
    tmpStringAttri    : TSynHighlighterAttributes;
    tmpCharAttri      : TSynHighlighterAttributes;
    tmpNumberAttri    : TSynHighlighterAttributes;
    tmpFloatAttri     : TSynHighlighterAttributes;
    tmpHexAttri       : TSynHighlighterAttributes;
    tmpOctalAttri     : TSynHighlighterAttributes;
    tmpKeyAttri       : TSynHighlighterAttributes;
    tmpSymbolAttri    : TSynHighlighterAttributes;
    tmpAsmAttri       : TSynHighlighterAttributes;
    tmpCommentAttri   : TSynHighlighterAttributes;
    tmpIdentifierAttri: TSynHighlighterAttributes;
    tmpInvalidAttri   : TSynHighlighterAttributes;
    tmpSpaceAttri     : TSynHighlighterAttributes;
    tmpDirecAttri     : TSynHighlighterAttributes;
    s                 : TStringList;

  begin { ReadCPPBSettings }
    s := TStringList.Create;
    try
      EnumUserSettings(s);
      if settingIndex >= s.Count then
        Result := False
      else
      begin
        tmpStringAttri    := TSynHighlighterAttributes.Create('', '');
        tmpCharAttri      := TSynHighlighterAttributes.Create('', '');
        tmpNumberAttri    := TSynHighlighterAttributes.Create('', '');
        tmpFloatAttri     := TSynHighlighterAttributes.Create('', '');
        tmpHexAttri       := TSynHighlighterAttributes.Create('', '');
        tmpOctalAttri     := TSynHighlighterAttributes.Create('', '');
        tmpKeyAttri       := TSynHighlighterAttributes.Create('', '');
        tmpSymbolAttri    := TSynHighlighterAttributes.Create('', '');
        tmpAsmAttri       := TSynHighlighterAttributes.Create('', '');
        tmpCommentAttri   := TSynHighlighterAttributes.Create('', '');
        tmpIdentifierAttri:= TSynHighlighterAttributes.Create('', '');
        tmpInvalidAttri   := TSynHighlighterAttributes.Create('', '');
        tmpSpaceAttri     := TSynHighlighterAttributes.Create('', '');
        tmpDirecAttri     := TSynHighlighterAttributes.Create('', '');
        tmpStringAttri    .Assign(FStringAttri);
        tmpCharAttri      .Assign(FCharAttri);
        tmpNumberAttri    .Assign(FNumberAttri);
        tmpFloatAttri     .Assign(FFloatAttri);
        tmpHexAttri       .Assign(FHexAttri);
        tmpOctalAttri     .Assign(FOctalAttri);
        tmpKeyAttri       .Assign(FKeyAttri);
        tmpSymbolAttri    .Assign(FSymbolAttri);
        tmpAsmAttri       .Assign(FAsmAttri);
        tmpCommentAttri   .Assign(FCommentAttri);
        tmpIdentifierAttri.Assign(FIdentifierAttri);
        tmpInvalidAttri   .Assign(FInvalidAttri);
        tmpSpaceAttri     .Assign(FSpaceAttri);
        tmpDirecAttri     .Assign(FDirecAttri);
        if s[settingIndex][1] = '1' then
          Result := ReadCPPBSetting(s[settingIndex],FAsmAttri,'Plain text')
        else
          Result := ReadCPPBSetting(s[settingIndex],FAsmAttri,'Assembler');
        Result := Result                                                         and
                  ReadCPPBSetting(s[settingIndex],FCommentAttri,'Comment')       and
                  ReadCPPBSetting(s[settingIndex],FIdentifierAttri,'Identifier') and
                  ReadCPPBSetting(s[settingIndex],FInvalidAttri,'Illegal Char')  and
                  ReadCPPBSetting(s[settingIndex],FKeyAttri,'Reserved word')     and
                  ReadCPPBSetting(s[settingIndex],FNumberAttri,'Integer')        and
                  ReadCPPBSetting(s[settingIndex],FFloatAttri,'Float')           and
                  ReadCPPBSetting(s[settingIndex],FHexAttri,'Hex')               and
                  ReadCPPBSetting(s[settingIndex],FOctalAttri,'Octal')           and
                  ReadCPPBSetting(s[settingIndex],FSpaceAttri,'Whitespace')      and
                  ReadCPPBSetting(s[settingIndex],FStringAttri,'String')         and
                  ReadCPPBSetting(s[settingIndex],FCharAttri,'Character')             and
                  ReadCPPBSetting(s[settingIndex],FSymbolAttri,'Symbol')         and
                  ReadCPPBSetting(s[settingIndex],FDirecAttri,'Preprocessor');
        if not Result then
        begin
          FStringAttri    .Assign(tmpStringAttri);
          FCharAttri      .Assign(tmpCharAttri);
          FNumberAttri    .Assign(tmpNumberAttri);
          FFloatAttri     .Assign(tmpFloatAttri);
          FHexAttri       .Assign(tmpHexAttri);
          FOctalAttri     .Assign(tmpOctalAttri);
          FKeyAttri       .Assign(tmpKeyAttri);
          FSymbolAttri    .Assign(tmpSymbolAttri);
          FAsmAttri       .Assign(tmpAsmAttri);
          FCommentAttri   .Assign(tmpCommentAttri);
          FIdentifierAttri.Assign(tmpIdentifierAttri);
          FInvalidAttri   .Assign(tmpInvalidAttri);
          FSpaceAttri     .Assign(tmpSpaceAttri);
          FDirecAttri     .Assign(tmpDirecAttri);
        end;
        tmpStringAttri    .Free;
        tmpCharAttri      .Free;
        tmpNumberAttri    .Free;
        tmpFloatAttri     .Free;
        tmpHexAttri       .Free;
        tmpOctalAttri     .Free;
        tmpKeyAttri       .Free;
        tmpSymbolAttri    .Free;
        tmpAsmAttri       .Free;
        tmpCommentAttri   .Free;
        tmpIdentifierAttri.Free;
        tmpInvalidAttri   .Free;
        tmpSpaceAttri     .Free;
        tmpDirecAttri     .Free;
      end;
    finally
      s.Free;
    end;
  end; { ReadCPPBSettings }

begin
  Result := ReadCPPBSettings(settingIndex);
end; { TSynCppSyn.UseUserSettings }

function TSynCppSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterCPP;
end;

class function TSynCppSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCPP;
end;

class function TSynCppSyn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

function TSynCppSyn.GetSampleSource: UnicodeString;
begin
  Result :=
    '// Syntax Highlighting'#13#10+
    'void __fastcall TForm1::Button1Click(TObject *Sender)'#13#10+
    '{'#13#10+
    '  int number = 123456;'#13#10+
    '  char c = ''a'';'#13#10+
    '  Caption = "The number is " + IntToStr(i);'#13#10+
    '  for (int i = 0; i <= number; i++)'#13#10+
    '  {'#13#10+
    '    x -= 0xff;'#13#10+
    '    x -= 023;'#13#10+
    '    x += 1.0;'#13#10+
    '    x += @; /* illegal character */'#13#10+
    '  }'#13#10+
    '  #ifdef USE_ASM'#13#10+
    '    asm'#13#10+
    '    {'#13#10+
    '      ASM MOV AX, 0x1234'#13#10+
    '      ASM MOV i, AX'#13#10+
    '    }'#13#10+
    '  #endif'#13#10+
    '}';
end;

class function TSynCppSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangCPP;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynCppSyn);
{$ENDIF}
end.
