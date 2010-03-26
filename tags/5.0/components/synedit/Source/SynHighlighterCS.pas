{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterCS.pas, released 2001-10-28.
The Original Code is based on SynHighlighterCpp.pas, released 2000-04-10,
which in turn is based on the dcjCppSyn.pas file from the mwEdit component
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

$Id: SynHighlighterCS.pas,v 1.8.2.7 2008/09/14 16:24:59 maelh Exp $

You may retrieve the latest version of SynEdit from the SynEdit home page,
located at http://SynEdit.SourceForge.net

You may retrieve the latest version of this file from
http://www.ashleybrown.co.uk/synedit/

Known Issues:
  - strings on multiple lines are not supported 
-------------------------------------------------------------------------------}
{
@abstract(Provides a C# syntax highlighter for SynEdit)
@author(Ashley Brown)
@created(2001)
@lastmod(2001-10-20)
The SynHighlighterCS unit provides SynEdit with a C# syntax highlighter.
Based on SynHighlighterCpp.pas
}

{$IFNDEF QSYNHIGHLIGHTERCS}
unit SynHighlighterCS;
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
  SynEditMiscClasses,
  SynUnicode,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkAsm, tkComment, tkDirective, tkIdentifier, tkKey, tkNull,
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
    rsAsmBlock, rsDirective, rsDirectiveComment, rsString34, rsString39,
    rsMultiLineString);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TSynCSSyn = class(TSynCustomHighlighter)
  private
    fAsmStart: Boolean;
    fRange: TRangeState;
    FTokenID: TtkTokenKind;
    FExtTokenID: TxtkTokenKind;
    fIdentFuncTable: array[0..210] of TIdentFuncTableFunc;
    fAsmAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fDirecAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fInvalidAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function FuncAbstract(Index: Integer): TtkTokenKind;
    function FuncAs(Index: Integer): TtkTokenKind;
    function FuncBase(Index: Integer): TtkTokenKind;
    function FuncBool(Index: Integer): TtkTokenKind;
    function FuncBreak(Index: Integer): TtkTokenKind;
    function FuncByte(Index: Integer): TtkTokenKind;
    function FuncCase(Index: Integer): TtkTokenKind;
    function FuncCatch(Index: Integer): TtkTokenKind;
    function FuncChar(Index: Integer): TtkTokenKind;
    function FuncChecked(Index: Integer): TtkTokenKind;
    function FuncClass(Index: Integer): TtkTokenKind;
    function FuncConst(Index: Integer): TtkTokenKind;
    function FuncContinue(Index: Integer): TtkTokenKind;
    function FuncDecimal(Index: Integer): TtkTokenKind;
    function FuncDefault(Index: Integer): TtkTokenKind;
    function FuncDelegate(Index: Integer): TtkTokenKind;
    function FuncDo(Index: Integer): TtkTokenKind;
    function FuncDouble(Index: Integer): TtkTokenKind;
    function FuncElse(Index: Integer): TtkTokenKind;
    function FuncEnum(Index: Integer): TtkTokenKind;
    function FuncEvent(Index: Integer): TtkTokenKind;
    function FuncExplicit(Index: Integer): TtkTokenKind;
    function FuncExtern(Index: Integer): TtkTokenKind;
    function FuncFalse(Index: Integer): TtkTokenKind;
    function FuncFinally(Index: Integer): TtkTokenKind;
    function FuncFixed(Index: Integer): TtkTokenKind;
    function FuncFloat(Index: Integer): TtkTokenKind;
    function FuncFor(Index: Integer): TtkTokenKind;
    function FuncForeach(Index: Integer): TtkTokenKind;
    function FuncGoto(Index: Integer): TtkTokenKind;
    function FuncIf(Index: Integer): TtkTokenKind;
    function FuncImplicit(Index: Integer): TtkTokenKind;
    function FuncIn(Index: Integer): TtkTokenKind;
    function FuncInt(Index: Integer): TtkTokenKind;
    function FuncInterface(Index: Integer): TtkTokenKind;
    function FuncInternal(Index: Integer): TtkTokenKind;
    function FuncIs(Index: Integer): TtkTokenKind;
    function FuncLock(Index: Integer): TtkTokenKind;
    function FuncLong(Index: Integer): TtkTokenKind;
    function FuncNamespace(Index: Integer): TtkTokenKind;
    function FuncNew(Index: Integer): TtkTokenKind;
    function FuncNull(Index: Integer): TtkTokenKind;
    function FuncObject(Index: Integer): TtkTokenKind;
    function FuncOperator(Index: Integer): TtkTokenKind;
    function FuncOut(Index: Integer): TtkTokenKind;
    function FuncOverride(Index: Integer): TtkTokenKind;
    function FuncParams(Index: Integer): TtkTokenKind;
    function FuncPrivate(Index: Integer): TtkTokenKind;
    function FuncProtected(Index: Integer): TtkTokenKind;
    function FuncPublic(Index: Integer): TtkTokenKind;
    function FuncReadonly(Index: Integer): TtkTokenKind;
    function FuncRef(Index: Integer): TtkTokenKind;
    function FuncReturn(Index: Integer): TtkTokenKind;
    function FuncSbyte(Index: Integer): TtkTokenKind;
    function FuncSealed(Index: Integer): TtkTokenKind;
    function FuncSizeof(Index: Integer): TtkTokenKind;
    function FuncStackalloc(Index: Integer): TtkTokenKind;
    function FuncStatic(Index: Integer): TtkTokenKind;
    function FuncString(Index: Integer): TtkTokenKind;
    function FuncStruct(Index: Integer): TtkTokenKind;
    function FuncSwitch(Index: Integer): TtkTokenKind;
    function FuncThis(Index: Integer): TtkTokenKind;
    function FuncThrow(Index: Integer): TtkTokenKind;
    function FuncTrue(Index: Integer): TtkTokenKind;
    function FuncTry(Index: Integer): TtkTokenKind;
    function FuncTypeof(Index: Integer): TtkTokenKind;
    function FuncUint(Index: Integer): TtkTokenKind;
    function FuncUlong(Index: Integer): TtkTokenKind;
    function FuncUnchecked(Index: Integer): TtkTokenKind;
    function FuncUnsafe(Index: Integer): TtkTokenKind;
    function FuncUshort(Index: Integer): TtkTokenKind;
    function FuncUsing(Index: Integer): TtkTokenKind;
    function FuncVirtual(Index: Integer): TtkTokenKind;
    function FuncVoid(Index: Integer): TtkTokenKind;
    function FuncWhile(Index: Integer): TtkTokenKind;
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
    function IsFilterStored: Boolean; override;
    function GetSampleSource: UnicodeString; override;
    procedure NextProcedure;
  public
    class function GetCapabilities: TSynHighlighterCapabilities; override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
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
    function UseUserSettings(settingIndex: integer): boolean; override;
    procedure EnumUserSettings(settings: TStrings); override;
    property ExtTokenID: TxtkTokenKind read GetExtTokenID;
  published
    property AsmAttri: TSynHighlighterAttributes read fAsmAttri write fAsmAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property DirecAttri: TSynHighlighterAttributes read fDirecAttri
      write fDirecAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property InvalidAttri: TSynHighlighterAttributes read fInvalidAttri
      write fInvalidAttri;
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
  Windows,
  SynEditStrConst;
{$ENDIF}

const
  KeyWords: array[0..74] of UnicodeString = (
    'abstract', 'as', 'base', 'bool', 'break', 'byte', 'case', 'catch', 'char', 
    'checked', 'class', 'const', 'continue', 'decimal', 'default', 'delegate', 
    'do', 'double', 'else', 'enum', 'event', 'explicit', 'extern', 'false', 
    'finally', 'fixed', 'float', 'for', 'foreach', 'goto', 'if', 'implicit', 
    'in', 'int', 'interface', 'internal', 'is', 'lock', 'long', 'namespace', 
    'new', 'null', 'object', 'operator', 'out', 'override', 'params', 'private', 
    'protected', 'public', 'readonly', 'ref', 'return', 'sbyte', 'sealed', 
    'sizeof', 'stackalloc', 'static', 'string', 'struct', 'switch', 'this', 
    'throw', 'true', 'try', 'typeof', 'uint', 'ulong', 'unchecked', 'unsafe', 
    'ushort', 'using', 'virtual', 'void', 'while' 
  );

  KeyIndices: array[0..210] of Integer = (
    71, -1, -1, -1, -1, -1, -1, -1, -1, 69, -1, -1, -1, -1, 1, 46, -1, -1, 62, 
    -1, 53, -1, -1, -1, -1, 3, -1, -1, 18, -1, 8, -1, -1, -1, -1, -1, 19, -1, 
    -1, -1, -1, -1, 45, -1, -1, 28, 44, -1, 47, 21, -1, -1, -1, -1, -1, 73, -1, 
    -1, 9, -1, -1, -1, 26, 49, 63, 65, -1, -1, 16, 67, -1, 59, -1, -1, -1, 66, 
    -1, 50, -1, -1, -1, 29, -1, 32, 37, -1, -1, 48, -1, -1, 55, -1, 14, 40, -1, 
    -1, 13, -1, 12, -1, -1, 15, 30, -1, -1, -1, 41, -1, -1, -1, -1, 4, 56, -1, 
    58, -1, 38, -1, -1, -1, -1, 74, -1, -1, -1, 17, 33, -1, -1, 20, -1, -1, 27, 
    31, -1, 6, -1, -1, -1, -1, 7, -1, -1, 10, -1, -1, 2, -1, -1, -1, 64, -1, -1, 
    43, -1, -1, -1, 0, -1, 34, -1, 25, -1, -1, 5, 61, 60, -1, 42, -1, -1, -1, 
    51, -1, -1, -1, -1, 22, -1, -1, 72, -1, -1, 57, -1, 70, -1, 11, -1, -1, -1, 
    24, -1, 35, -1, -1, 23, -1, 39, -1, -1, 68, 52, 36, -1, -1, -1, -1, 54, -1, 
    -1 
  );

{$Q-}
function TSynCSSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 723 + Ord(Str^) * 24;
    inc(Str);
  end;
  Result := Result mod 211;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynCSSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynCSSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[157] := FuncAbstract;
  fIdentFuncTable[14] := FuncAs;
  fIdentFuncTable[146] := FuncBase;
  fIdentFuncTable[25] := FuncBool;
  fIdentFuncTable[111] := FuncBreak;
  fIdentFuncTable[164] := FuncByte;
  fIdentFuncTable[135] := FuncCase;
  fIdentFuncTable[140] := FuncCatch;
  fIdentFuncTable[30] := FuncChar;
  fIdentFuncTable[58] := FuncChecked;
  fIdentFuncTable[143] := FuncClass;
  fIdentFuncTable[187] := FuncConst;
  fIdentFuncTable[98] := FuncContinue;
  fIdentFuncTable[96] := FuncDecimal;
  fIdentFuncTable[92] := FuncDefault;
  fIdentFuncTable[101] := FuncDelegate;
  fIdentFuncTable[68] := FuncDo;
  fIdentFuncTable[125] := FuncDouble;
  fIdentFuncTable[28] := FuncElse;
  fIdentFuncTable[36] := FuncEnum;
  fIdentFuncTable[129] := FuncEvent;
  fIdentFuncTable[49] := FuncExplicit;
  fIdentFuncTable[177] := FuncExtern;
  fIdentFuncTable[196] := FuncFalse;
  fIdentFuncTable[191] := FuncFinally;
  fIdentFuncTable[161] := FuncFixed;
  fIdentFuncTable[62] := FuncFloat;
  fIdentFuncTable[132] := FuncFor;
  fIdentFuncTable[45] := FuncForeach;
  fIdentFuncTable[81] := FuncGoto;
  fIdentFuncTable[102] := FuncIf;
  fIdentFuncTable[133] := FuncImplicit;
  fIdentFuncTable[83] := FuncIn;
  fIdentFuncTable[126] := FuncInt;
  fIdentFuncTable[159] := FuncInterface;
  fIdentFuncTable[193] := FuncInternal;
  fIdentFuncTable[203] := FuncIs;
  fIdentFuncTable[84] := FuncLock;
  fIdentFuncTable[116] := FuncLong;
  fIdentFuncTable[198] := FuncNamespace;
  fIdentFuncTable[93] := FuncNew;
  fIdentFuncTable[106] := FuncNull;
  fIdentFuncTable[168] := FuncObject;
  fIdentFuncTable[153] := FuncOperator;
  fIdentFuncTable[46] := FuncOut;
  fIdentFuncTable[42] := FuncOverride;
  fIdentFuncTable[15] := FuncParams;
  fIdentFuncTable[48] := FuncPrivate;
  fIdentFuncTable[87] := FuncProtected;
  fIdentFuncTable[63] := FuncPublic;
  fIdentFuncTable[77] := FuncReadonly;
  fIdentFuncTable[172] := FuncRef;
  fIdentFuncTable[202] := FuncReturn;
  fIdentFuncTable[20] := FuncSbyte;
  fIdentFuncTable[208] := FuncSealed;
  fIdentFuncTable[90] := FuncSizeof;
  fIdentFuncTable[112] := FuncStackalloc;
  fIdentFuncTable[183] := FuncStatic;
  fIdentFuncTable[114] := FuncString;
  fIdentFuncTable[71] := FuncStruct;
  fIdentFuncTable[166] := FuncSwitch;
  fIdentFuncTable[165] := FuncThis;
  fIdentFuncTable[18] := FuncThrow;
  fIdentFuncTable[64] := FuncTrue;
  fIdentFuncTable[150] := FuncTry;
  fIdentFuncTable[65] := FuncTypeof;
  fIdentFuncTable[75] := FuncUint;
  fIdentFuncTable[69] := FuncUlong;
  fIdentFuncTable[201] := FuncUnchecked;
  fIdentFuncTable[9] := FuncUnsafe;
  fIdentFuncTable[185] := FuncUshort;
  fIdentFuncTable[0] := FuncUsing;
  fIdentFuncTable[180] := FuncVirtual;
  fIdentFuncTable[55] := FuncVoid;
  fIdentFuncTable[121] := FuncWhile;
end;



function TSynCSSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynCSSyn.FuncAbstract(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncAs(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncBase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncBool(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncBreak(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncByte(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncCase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncCatch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncChar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncChecked(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncClass(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncConst(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncContinue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncDecimal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncDefault(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncDelegate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncDo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncDouble(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncElse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncEnum(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncEvent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncExplicit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncExtern(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncFalse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncFinally(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncFixed(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncFloat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncFor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncForeach(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncGoto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncIf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncImplicit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncIn(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncInt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncInterface(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncInternal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncIs(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncLock(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncLong(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncNamespace(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncNew(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncNull(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncObject(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncOperator(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncOut(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncOverride(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncParams(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncPrivate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncProtected(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncPublic(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncReadonly(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncRef(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncReturn(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncSbyte(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncSealed(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncSizeof(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncStackalloc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncStatic(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncString(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncStruct(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncSwitch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncThis(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncThrow(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncTrue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncTry(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncTypeof(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncUint(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncUlong(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncUnchecked(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncUnsafe(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncUshort(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncUsing(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncVirtual(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncVoid(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCSSyn.FuncWhile(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

constructor TSynCSSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := True;

  fAsmAttri := TSynHighlighterAttributes.Create(SYNS_AttrAssembler, SYNS_FriendlyAttrAssembler);
  AddAttribute(fAsmAttri);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fInvalidAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(fInvalidAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  fDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  AddAttribute(fDirecAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fRange := rsUnknown;
  fAsmStart := False;
  fDefaultFilter := SYNS_FilterCS;
end; { Create }

procedure TSynCSSyn.AnsiCProc;
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

  while FLine[Run] <> #0 do
    case FLine[Run] of
      '*':
        if fLine[Run + 1] = '/' then
        begin
          inc(Run, 2);
          if fRange = rsAnsiCAsm then
            fRange := rsAsm
          else if fRange = rsAnsiCAsmBlock then
            fRange := rsAsmBlock
          else if fRange = rsDirectiveComment then
            fRange := rsDirective
          else
            fRange := rsUnKnown;
          break;
        end else
          inc(Run);
      #10: break;
      #13: break;
    else inc(Run);
    end;
end;

procedure TSynCSSyn.AndSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {and assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkAndAssign;
      end;
    '&':                               {logical and}
      begin
        inc(Run, 2);
        FExtTokenID := xtkLogAnd;
      end;
  else                                 {and}
    begin
      inc(Run);
      FExtTokenID := xtkAnd;
    end;
  end;
end;

procedure TSynCSSyn.AsciiCharProc;
begin
  fTokenID := tkString;
  repeat
    if fLine[Run] = '\' then begin
      if CharInSet(fLine[Run + 1], [#39, '\']) then
        inc(Run);
    end;
    inc(Run);
  until IsLineEnd(Run) or (fLine[Run] = #39);
  if fLine[Run] = #39 then
    inc(Run);
end;

procedure TSynCSSyn.AtSymbolProc;
begin
  fTokenID := tkUnknown;
  inc(Run);
end;

procedure TSynCSSyn.BraceCloseProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
  FExtTokenID := xtkBraceClose;
  if fRange = rsAsmBlock then fRange := rsUnknown;
end;

procedure TSynCSSyn.BraceOpenProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
  FExtTokenID := xtkBraceOpen;
  if fRange = rsAsm then
  begin
    fRange := rsAsmBlock;
    fAsmStart := True;
  end;
end;

procedure TSynCSSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run + 1] = #10 then Inc(Run);
end;

procedure TSynCSSyn.ColonProc;
begin
  fTokenID := tkSymbol;
  Case FLine[Run + 1] of
    ':':                               {scope resolution operator}
      begin
        inc(Run, 2);
        FExtTokenID := xtkScopeResolution;
      end;
  else                                 {colon}
    begin
      inc(Run);
      FExtTokenID := xtkColon;
    end;
  end;
end;

procedure TSynCSSyn.CommaProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkComma;
end;

procedure TSynCSSyn.DirectiveProc;
begin
  if IsLineEnd(Run) then
  begin
    if (Run <= 0) or (fLine[Run - 1] <> '\') then
      fRange := rsUnknown;
    NextProcedure;
  end
  else
  begin
    fTokenID := tkDirective;
    while True do
      case fLine[Run] of
        '/': // comment?
          begin
            if fLine[Run + 1] = '/' then
            begin // is end of directive as well
              fRange := rsUnknown;
              break;
            end else if fLine[Run + 1] = '*' then
            begin // might be embedded only
              fRange := rsDirectiveComment;
              break;
            end else
              Inc(Run);
          end;
        '\': // directive continued on next line?
          begin
            Inc(Run);
            if IsLineEnd(Run) then
            begin
              fRange := rsDirective;
              break;
            end;
          end;
        #0, #10, #13:
          begin
            fRange := rsUnknown;
            break;
          end;
        else
          Inc(Run);
      end;
  end;
end;

procedure TSynCSSyn.EqualProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {logical equal}
      begin
        inc(Run, 2);
        FExtTokenID := xtkLogEqual;
      end;
  else                                 {assign}
    begin
      inc(Run);
      FExtTokenID := xtkAssign;
    end;
  end;
end;

procedure TSynCSSyn.GreaterProc;
begin
  fTokenID := tkSymbol;
  Case FLine[Run + 1] of
    '=':                               {greater than or equal to}
      begin
        inc(Run, 2);
        FExtTokenID := xtkGreaterThanEqual;
      end;
    '>':
      begin
        if FLine[Run + 2] = '=' then   {shift right assign}
        begin
          inc(Run, 3);
          FExtTokenID := xtkShiftRightAssign;
        end
        else                           {shift right}
        begin
          inc(Run, 2);
          FExtTokenID := xtkShiftRight;
        end;
      end;
  else                                 {greater than}
    begin
      inc(Run);
      FExtTokenID := xtkGreaterThan;
    end;
  end;
end;

procedure TSynCSSyn.QuestionProc;
begin
  fTokenID := tkSymbol;                {conditional}
  FExtTokenID := xtkQuestion;
  inc(Run);
end;

procedure TSynCSSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do inc(Run);
end;

procedure TSynCSSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynCSSyn.LowerProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {less than or equal to}
      begin
        inc(Run, 2);
        FExtTokenID := xtkLessThanEqual;
      end;
    '<':
      begin
        if FLine[Run + 2] = '=' then   {shift left assign}
        begin
          inc(Run, 3);
          FExtTokenID := xtkShiftLeftAssign;
        end
        else                           {shift left}
        begin
          inc(Run, 2);
          FExtTokenID := xtkShiftLeft;
        end;
      end;
  else                                 {less than}
    begin
      inc(Run);
      FExtTokenID := xtkLessThan;
    end;
  end;
end;

procedure TSynCSSyn.MinusProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {subtract assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkSubtractAssign;
      end;
    '-':                               {decrement}
      begin
        inc(Run, 2);
        FExtTokenID := xtkDecrement;
      end;
    '>':                               {arrow}
      begin
        inc(Run, 2);
        FExtTokenID := xtkArrow;
      end;
  else                                 {subtract}
    begin
      inc(Run);
      FExtTokenID := xtkSubtract;
    end;
  end;
end;

procedure TSynCSSyn.ModSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {mod assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkModAssign;
      end;
  else                                 {mod}
    begin
      inc(Run);
      FExtTokenID := xtkMod;
    end;
  end;
end;

procedure TSynCSSyn.NotSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {not equal}
      begin
        inc(Run, 2);
        FExtTokenID := xtkNotEqual;
      end;
  else                                 {not}
    begin
      inc(Run);
      FExtTokenID := xtkLogComplement;
    end;
  end;
end;

procedure TSynCSSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynCSSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f', '.', 'u', 'U', 'l', 'L', 'x', 'X':
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

procedure TSynCSSyn.OrSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {or assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkIncOrAssign;
      end;
    '|':                               {logical or}
      begin
        inc(Run, 2);
        FExtTokenID := xtkLogOr;
      end;
  else                                 {or}
    begin
      inc(Run);
      FExtTokenID := xtkIncOr;
    end;
  end;
end;

procedure TSynCSSyn.PlusProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {add assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkAddAssign;
      end;
    '+':                               {increment}
      begin
        inc(Run, 2);
        FExtTokenID := xtkIncrement;
      end;
  else                                 {add}
    begin
      inc(Run);
      FExtTokenID := xtkAdd;
    end;
  end;
end;

procedure TSynCSSyn.PointProc;
begin
  fTokenID := tkSymbol;
  if (FLine[Run + 1] = '.') and (FLine[Run + 2] = '.') then
    begin                              {ellipse}
      inc(Run, 3);
      FExtTokenID := xtkEllipse;
    end
  else                                 {point}
    begin
      inc(Run);
      FExtTokenID := xtkPoint;
    end;
end;

procedure TSynCSSyn.RoundCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkRoundClose;
end;

procedure TSynCSSyn.RoundOpenProc;
begin
  inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundOpen;
end;

procedure TSynCSSyn.SemiColonProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSemiColon;
  if fRange = rsAsm then fRange := rsUnknown;
end;

procedure TSynCSSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '/':                               {c++ style comments}
      begin
        fTokenID := tkComment;
        inc(Run, 2);
        while not IsLineEnd(Run) do Inc(Run);
      end;
    '*':                               {c style comments}
      begin
        fTokenID := tkComment;
        if fRange = rsAsm then
          fRange := rsAnsiCAsm
        else if fRange = rsAsmBlock then
          fRange := rsAnsiCAsmBlock
        else if fRange <> rsDirectiveComment then                          
          fRange := rsAnsiC;
        inc(Run, 2);
        while fLine[Run] <> #0 do
          case fLine[Run] of
            '*':
              if fLine[Run + 1] = '/' then
              begin
                inc(Run, 2);
                if fRange = rsDirectiveComment then
                  fRange := rsDirective
                else if fRange = rsAnsiCAsm then
                  fRange := rsAsm
                else
                  begin
                  if fRange = rsAnsiCAsmBlock then
                    fRange := rsAsmBlock
                  else
                    fRange := rsUnKnown;
                  end;
                break;
              end else inc(Run);
            #10, #13:
              begin
                if fRange = rsDirectiveComment then
                  fRange := rsAnsiC;
                break;
              end;
          else inc(Run);
          end;
      end;
    '=':                               {divide assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
        FExtTokenID := xtkDivideAssign;
      end;
  else                                 {divide}
    begin
      inc(Run);
      fTokenID := tkSymbol;
      FExtTokenID := xtkDivide;
    end;
  end;
end;

procedure TSynCSSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynCSSyn.SquareCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSquareClose;
end;

procedure TSynCSSyn.SquareOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSquareOpen;
end;

procedure TSynCSSyn.StarProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {multiply assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkMultiplyAssign;
      end;
  else                                 {star}
    begin
      inc(Run);
      FExtTokenID := xtkStar;
    end;
  end;
end;

procedure TSynCSSyn.StringProc;
begin
  fTokenID := tkString;
  repeat
    if fLine[Run] = '\' then begin
      case fLine[Run + 1] of
        #34, '\':
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
  until IsLineEnd(Run) or (fLine[Run] = #34);
  if FLine[Run] = #34 then
    inc(Run);
end;

procedure TSynCSSyn.StringEndProc;
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
            #34, '\':
              Inc(Run);
            #00:
              begin
                Inc(Run);
                fRange := rsMultilineString;
                Exit;
              end;
          end;
        end;
      #34: Break;
    end;
    inc(Run);
  until IsLineEnd(Run) or (fLine[Run] = #34);
  if FLine[Run] = #34 then
    inc(Run);
end;

procedure TSynCSSyn.TildeProc;
begin
  inc(Run);                            {bitwise complement}
  fTokenId := tkSymbol;
  FExtTokenID := xtkBitComplement;
end;

procedure TSynCSSyn.XOrSymbolProc;
begin
  fTokenID := tkSymbol;
  Case FLine[Run + 1] of
  	'=':                               {xor assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkXorAssign;
      end;
  else                                 {xor}
    begin
      inc(Run);
      FExtTokenID := xtkXor;
    end;
  end;
end;

procedure TSynCSSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynCSSyn.Next;
begin
  fAsmStart := False;
  fTokenPos := Run;
  case fRange of
    rsAnsiC, rsAnsiCAsm,
    rsAnsiCAsmBlock, rsDirectiveComment: AnsiCProc;
    rsDirective: DirectiveProc;
    rsMultilineString: StringEndProc;
  else
    begin
      fRange := rsUnknown;
      NextProcedure;
    end;
  end;
  inherited;
end;

procedure TSynCSSyn.NextProcedure;
begin
  case fLine[Run] of
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

function TSynCSSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    else Result := nil;
  end;
end;

function TSynCSSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynCSSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynCSSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
  if ((fRange = rsAsm) or (fRange = rsAsmBlock)) and not fAsmStart
    and not (fTokenId in [tkComment, tkSpace, tkNull])
  then
    Result := tkAsm;
end;

function TSynCSSyn.GetExtTokenID: TxtkTokenKind;
begin
  Result := FExtTokenID;
end;

function TSynCSSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkAsm: Result := fAsmAttri;
    tkComment: Result := fCommentAttri;
    tkDirective: Result := fDirecAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fInvalidAttri;
    else Result := nil;
  end;
end;

function TSynCSSyn.GetTokenKind: integer;
begin
  Result := Ord(GetTokenID);
end;

procedure TSynCSSyn.ResetRange;
begin
  fRange:= rsUnknown;
end;

procedure TSynCSSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynCSSyn.EnumUserSettings(settings: TStrings);
begin
  { returns the user settings that exist in the registry }
  {$IFNDEF SYN_CLX}
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
  {$ENDIF}
end;

function TSynCSSyn.UseUserSettings(settingIndex: integer): boolean;
// Possible parameter values:
//   index into TStrings returned by EnumUserSettings
// Possible return values:
//   true : settings were read and used
//   false: problem reading settings or invalid version specified - old settings
//          were preserved

  {$IFNDEF SYN_CLX}
  function ReadCPPBSettings(settingIndex: integer): boolean;

    function ReadCPPBSetting(settingTag: string; attri: TSynHighlighterAttributes; key: string): boolean;

      function ReadCPPB1(settingTag: string; attri: TSynHighlighterAttributes; name: string): boolean;
      var
        i: integer;
      begin
        for i := 1 to Length(name) do
          if name[i] = ' ' then name[i] := '_';
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
             '\SOFTWARE\Borland\C++Builder\'+settingTag+'\Highlight',name,true);
      end; { ReadCPPB1 }

      function ReadCPPB3OrMore(settingTag: string; attri: TSynHighlighterAttributes; key: string): boolean;
      begin
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
                 '\Software\Borland\C++Builder\'+settingTag+'\Editor\Highlight',
                 key,false);
      end; { ReadCPPB3OrMore }

    begin { ReadCPPBSetting }
      try
        if (settingTag[1] = '1')
          then Result := ReadCPPB1(settingTag,attri,key)
          else Result := ReadCPPB3OrMore(settingTag,attri,key);
      except Result := false; end;
    end; { ReadCPPBSetting }

  var
    tmpStringAttri    : TSynHighlighterAttributes;
    tmpNumberAttri    : TSynHighlighterAttributes;
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
      if settingIndex >= s.Count then Result := false
      else begin
        tmpStringAttri    := TSynHighlighterAttributes.Create('', '');
        tmpNumberAttri    := TSynHighlighterAttributes.Create('', '');
        tmpKeyAttri       := TSynHighlighterAttributes.Create('', '');
        tmpSymbolAttri    := TSynHighlighterAttributes.Create('', '');
        tmpAsmAttri       := TSynHighlighterAttributes.Create('', '');
        tmpCommentAttri   := TSynHighlighterAttributes.Create('', '');
        tmpIdentifierAttri:= TSynHighlighterAttributes.Create('', '');
        tmpInvalidAttri   := TSynHighlighterAttributes.Create('', '');
        tmpSpaceAttri     := TSynHighlighterAttributes.Create('', '');
        tmpDirecAttri     := TSynHighlighterAttributes.Create('', '');
        tmpStringAttri    .Assign(fStringAttri);
        tmpNumberAttri    .Assign(fNumberAttri);
        tmpKeyAttri       .Assign(fKeyAttri);
        tmpSymbolAttri    .Assign(fSymbolAttri);
        tmpAsmAttri       .Assign(fAsmAttri);
        tmpCommentAttri   .Assign(fCommentAttri);
        tmpIdentifierAttri.Assign(fIdentifierAttri);
        tmpInvalidAttri   .Assign(fInvalidAttri);
        tmpSpaceAttri     .Assign(fSpaceAttri);
        tmpDirecAttri     .Assign(fDirecAttri);
        if s[settingIndex][1] = '1'
          then Result := ReadCPPBSetting(s[settingIndex],fAsmAttri,'Plain text')
          else Result := ReadCPPBSetting(s[settingIndex],fAsmAttri,'Assembler');
        Result := Result                                                         and
                  ReadCPPBSetting(s[settingIndex],fCommentAttri,'Comment')       and
                  ReadCPPBSetting(s[settingIndex],fIdentifierAttri,'Identifier') and
                  ReadCPPBSetting(s[settingIndex],fInvalidAttri,'Illegal Char')  and 
                  ReadCPPBSetting(s[settingIndex],fKeyAttri,'Reserved word')     and
                  ReadCPPBSetting(s[settingIndex],fNumberAttri,'Integer')        and
                  ReadCPPBSetting(s[settingIndex],fSpaceAttri,'Whitespace')      and
                  ReadCPPBSetting(s[settingIndex],fStringAttri,'String')         and
                  ReadCPPBSetting(s[settingIndex],fSymbolAttri,'Symbol')         and
                  ReadCPPBSetting(s[settingIndex],fDirecAttri,'Preprocessor');
        if not Result then begin
          fStringAttri    .Assign(tmpStringAttri);
          fNumberAttri    .Assign(tmpNumberAttri);
          fKeyAttri       .Assign(tmpKeyAttri);
          fSymbolAttri    .Assign(tmpSymbolAttri);
          fAsmAttri       .Assign(tmpAsmAttri);
          fCommentAttri   .Assign(tmpCommentAttri);
          fIdentifierAttri.Assign(tmpIdentifierAttri);
          fInvalidAttri.Assign(tmpInvalidAttri);
          fSpaceAttri     .Assign(tmpSpaceAttri);
          fDirecAttri     .Assign(tmpDirecAttri);
        end;
        tmpStringAttri    .Free;
        tmpNumberAttri    .Free;
        tmpKeyAttri       .Free;
        tmpSymbolAttri    .Free;
        tmpAsmAttri       .Free;
        tmpCommentAttri   .Free;
        tmpIdentifierAttri.Free;
        tmpInvalidAttri   .Free;
        tmpSpaceAttri     .Free;
        tmpDirecAttri     .Free;
      end;
    finally s.Free; end;
  end; { ReadCPPBSettings }
  {$ENDIF}

begin
  {$IFNDEF SYN_CLX}
  Result := ReadCPPBSettings(settingIndex);
  {$ELSE}
  Result := False;
  {$ENDIF}
end; { TSynCSSyn.UseUserSettings }

function TSynCSSyn.GetSampleSource: UnicodeString;
begin
  Result := '/* Syntax Highlighting */'#13#10 +
				'int num = 12345;'#13#10 +
				'string str = "Hello World";'#13#10;

end; { GetSampleSource }

class function TSynCSSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCS;
end;

function TSynCSSyn.IsFilterStored: boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCS;
end;

class function TSynCSSyn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

class function TSynCSSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangCS;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynCSSyn);
{$ENDIF}
end.
