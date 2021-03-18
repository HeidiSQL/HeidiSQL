{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.
                                          PP - 2001/10/24:
The Original Code is based on the UnrealSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Dean Harmon.
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

$Id: SynHighlighterUnreal.pas,v 1.17.2.8 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Unreal syntax highlighter for SynEdit)
@author(Dean Harmon)
@created(2000)
@lastmod(2001-06-29)
}

unit SynHighlighterUnreal;

{$I SynEdit.inc}

interface

uses
  Graphics,
  Registry,
  Windows, // registry constants
  SynEditHighlighter,
  SynEditTypes,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (
    tkComment,
    tkDirective,
    tkIdentifier,
    tkKey,
    tkKey2,
    tkNull,
    tkNumber,
    tkSpace,
    tkString,
    tkString2,
    tkSymbol,
    tkUnknown);

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

  TRangeState = (rsANil, rsAnsiC, rsDirective, rsDirectiveComment, rsUnknown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TSynUnrealSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FRoundCount: Integer;
    FSquareCount: Integer;
    FTokenID: TtkTokenKind;
    FExtTokenID: TxtkTokenKind;
    FIdentFuncTable: array[0..732] of TIdentFuncTableFunc;
    FCommentAttri: TSynHighlighterAttributes;
    FDirecAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FInvalidAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FKey2Attri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FString2Attri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function FuncAbstract(Index: Integer): TtkTokenKind;
    function FuncAlways(Index: Integer): TtkTokenKind;
    function FuncArray(Index: Integer): TtkTokenKind;
    function FuncArraycount(Index: Integer): TtkTokenKind;
    function FuncAssert(Index: Integer): TtkTokenKind;
    function FuncAuto(Index: Integer): TtkTokenKind;
    function FuncAutomated(Index: Integer): TtkTokenKind;
    function FuncBool(Index: Integer): TtkTokenKind;
    function FuncBoundingbox(Index: Integer): TtkTokenKind;
    function FuncBoundingvolume(Index: Integer): TtkTokenKind;
    function FuncBreak(Index: Integer): TtkTokenKind;
    function FuncButton(Index: Integer): TtkTokenKind;
    function FuncByte(Index: Integer): TtkTokenKind;
    function FuncCache(Index: Integer): TtkTokenKind;
    function FuncCacheexempt(Index: Integer): TtkTokenKind;
    function FuncCase(Index: Integer): TtkTokenKind;
    function FuncCatch(Index: Integer): TtkTokenKind;
    function FuncClass(Index: Integer): TtkTokenKind;
    function FuncCoerce(Index: Integer): TtkTokenKind;
    function FuncCollapsecategories(Index: Integer): TtkTokenKind;
    function FuncColor(Index: Integer): TtkTokenKind;
    function FuncConfig(Index: Integer): TtkTokenKind;
    function FuncConst(Index: Integer): TtkTokenKind;
    function FuncContinue(Index: Integer): TtkTokenKind;
    function FuncCoords(Index: Integer): TtkTokenKind;
    function FuncCpptext(Index: Integer): TtkTokenKind;
    function FuncCross(Index: Integer): TtkTokenKind;
    function FuncDefault(Index: Integer): TtkTokenKind;
    function FuncDefaultproperties(Index: Integer): TtkTokenKind;
    function FuncDelegate(Index: Integer): TtkTokenKind;
    function FuncDelete(Index: Integer): TtkTokenKind;
    function FuncDependson(Index: Integer): TtkTokenKind;
    function FuncDeprecated(Index: Integer): TtkTokenKind;
    function FuncDo(Index: Integer): TtkTokenKind;
    function FuncDontcollapsecategories(Index: Integer): TtkTokenKind;
    function FuncDot(Index: Integer): TtkTokenKind;
    function FuncEach(Index: Integer): TtkTokenKind;
    function FuncEdfindable(Index: Integer): TtkTokenKind;
    function FuncEditconst(Index: Integer): TtkTokenKind;
    function FuncEditconstarray(Index: Integer): TtkTokenKind;
    function FuncEditinline(Index: Integer): TtkTokenKind;
    function FuncEditinlinenew(Index: Integer): TtkTokenKind;
    function FuncEditinlinenotify(Index: Integer): TtkTokenKind;
    function FuncEditinlineuse(Index: Integer): TtkTokenKind;
    function FuncElse(Index: Integer): TtkTokenKind;
    function FuncEnum(Index: Integer): TtkTokenKind;
    function FuncEnumcount(Index: Integer): TtkTokenKind;
    function FuncEvent(Index: Integer): TtkTokenKind;
    function FuncExec(Index: Integer): TtkTokenKind;
    function FuncExpands(Index: Integer): TtkTokenKind;
    function FuncExplicit(Index: Integer): TtkTokenKind;
    function FuncExport(Index: Integer): TtkTokenKind;
    function FuncExportstructs(Index: Integer): TtkTokenKind;
    function FuncExtends(Index: Integer): TtkTokenKind;
    function FuncFalse(Index: Integer): TtkTokenKind;
    function FuncFinal(Index: Integer): TtkTokenKind;
    function FuncFloat(Index: Integer): TtkTokenKind;
    function FuncFor(Index: Integer): TtkTokenKind;
    function FuncForeach(Index: Integer): TtkTokenKind;
    function FuncFunction(Index: Integer): TtkTokenKind;
    function FuncGlobal(Index: Integer): TtkTokenKind;
    function FuncGlobalconfig(Index: Integer): TtkTokenKind;
    function FuncGoto(Index: Integer): TtkTokenKind;
    function FuncGuid(Index: Integer): TtkTokenKind;
    function FuncHidecategories(Index: Integer): TtkTokenKind;
    function FuncHidedropdown(Index: Integer): TtkTokenKind;
    function FuncHideparent(Index: Integer): TtkTokenKind;
    function FuncIf(Index: Integer): TtkTokenKind;
    function FuncIgnores(Index: Integer): TtkTokenKind;
    function FuncImport(Index: Integer): TtkTokenKind;
    function FuncInit(Index: Integer): TtkTokenKind;
    function FuncInput(Index: Integer): TtkTokenKind;
    function FuncInsert(Index: Integer): TtkTokenKind;
    function FuncInstanced(Index: Integer): TtkTokenKind;
    function FuncInt(Index: Integer): TtkTokenKind;
    function FuncIntrinsic(Index: Integer): TtkTokenKind;
    function FuncInvariant(Index: Integer): TtkTokenKind;
    function FuncIterator(Index: Integer): TtkTokenKind;
    function FuncLatent(Index: Integer): TtkTokenKind;
    function FuncLength(Index: Integer): TtkTokenKind;
    function FuncLocal(Index: Integer): TtkTokenKind;
    function FuncLocalized(Index: Integer): TtkTokenKind;
    function FuncLong(Index: Integer): TtkTokenKind;
    function FuncMesh(Index: Integer): TtkTokenKind;
    function FuncModel(Index: Integer): TtkTokenKind;
    function FuncMutable(Index: Integer): TtkTokenKind;
    function FuncName(Index: Integer): TtkTokenKind;
    function FuncNative(Index: Integer): TtkTokenKind;
    function FuncNativereplication(Index: Integer): TtkTokenKind;
    function FuncNew(Index: Integer): TtkTokenKind;
    function FuncNoexport(Index: Integer): TtkTokenKind;
    function FuncNone(Index: Integer): TtkTokenKind;
    function FuncNoteditinlinenew(Index: Integer): TtkTokenKind;
    function FuncNotplaceable(Index: Integer): TtkTokenKind;
    function FuncNousercreate(Index: Integer): TtkTokenKind;
    function FuncOperator(Index: Integer): TtkTokenKind;
    function FuncOptional(Index: Integer): TtkTokenKind;
    function FuncOut(Index: Integer): TtkTokenKind;
    function FuncParseconfig(Index: Integer): TtkTokenKind;
    function FuncPerobjectconfig(Index: Integer): TtkTokenKind;
    function FuncPlaceable(Index: Integer): TtkTokenKind;
    function FuncPlane(Index: Integer): TtkTokenKind;
    function FuncPointer(Index: Integer): TtkTokenKind;
    function FuncPostoperator(Index: Integer): TtkTokenKind;
    function FuncPreoperator(Index: Integer): TtkTokenKind;
    function FuncPrivate(Index: Integer): TtkTokenKind;
    function FuncProtected(Index: Integer): TtkTokenKind;
    function FuncRegister(Index: Integer): TtkTokenKind;
    function FuncReliable(Index: Integer): TtkTokenKind;
    function FuncRemove(Index: Integer): TtkTokenKind;
    function FuncReplication(Index: Integer): TtkTokenKind;
    function FuncReturn(Index: Integer): TtkTokenKind;
    function FuncRng(Index: Integer): TtkTokenKind;
    function FuncRot(Index: Integer): TtkTokenKind;
    function FuncRotator(Index: Integer): TtkTokenKind;
    function FuncSafereplace(Index: Integer): TtkTokenKind;
    function FuncScale(Index: Integer): TtkTokenKind;
    function FuncScriptconst(Index: Integer): TtkTokenKind;
    function FuncSelf(Index: Integer): TtkTokenKind;
    function FuncShowcategories(Index: Integer): TtkTokenKind;
    function FuncSimulated(Index: Integer): TtkTokenKind;
    function FuncSingular(Index: Integer): TtkTokenKind;
    function FuncSkip(Index: Integer): TtkTokenKind;
    function FuncSound(Index: Integer): TtkTokenKind;
    function FuncState(Index: Integer): TtkTokenKind;
    function FuncStatic(Index: Integer): TtkTokenKind;
    function FuncStop(Index: Integer): TtkTokenKind;
    function FuncString(Index: Integer): TtkTokenKind;
    function FuncStruct(Index: Integer): TtkTokenKind;
    function FuncSuper(Index: Integer): TtkTokenKind;
    function FuncSwitch(Index: Integer): TtkTokenKind;
    function FuncTexture(Index: Integer): TtkTokenKind;
    function FuncTransient(Index: Integer): TtkTokenKind;
    function FuncTravel(Index: Integer): TtkTokenKind;
    function FuncTrue(Index: Integer): TtkTokenKind;
    function FuncUnreliable(Index: Integer): TtkTokenKind;
    function FuncUntil(Index: Integer): TtkTokenKind;
    function FuncVar(Index: Integer): TtkTokenKind;
    function FuncVect(Index: Integer): TtkTokenKind;
    function FuncVector(Index: Integer): TtkTokenKind;
    function FuncVoid(Index: Integer): TtkTokenKind;
    function FuncWhile(Index: Integer): TtkTokenKind;
    function FuncWithin(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure AnsiCProc;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
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
    procedure DollarSignProc;
    procedure TildeProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
  protected
    function GetExtTokenID: TxtkTokenKind;
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
    procedure NextProcedure;
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
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property DirecAttri: TSynHighlighterAttributes read FDirecAttri
      write FDirecAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property InvalidAttri: TSynHighlighterAttributes read FInvalidAttri
      write FInvalidAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property Key2Attri: TSynHighlighterAttributes read FKey2Attri write FKey2Attri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri
      write FStringAttri;
    property SingleStringAttri: TSynHighlighterAttributes read FString2Attri
      write FString2Attri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  KeyWords: array[0..142] of UnicodeString = (
    'abstract', 'always', 'array', 'arraycount', 'assert', 'auto', 'automated', 
    'bool', 'boundingbox', 'boundingvolume', 'break', 'button', 'byte', 'cache', 
    'cacheexempt', 'case', 'catch', 'class', 'coerce', 'collapsecategories', 
    'color', 'config', 'const', 'continue', 'coords', 'cpptext', 'cross', 
    'default', 'defaultproperties', 'delegate', 'delete', 'dependson', 
    'deprecated', 'do', 'dontcollapsecategories', 'dot', 'each', 'edfindable', 
    'editconst', 'editconstarray', 'editinline', 'editinlinenew', 
    'editinlinenotify', 'editinlineuse', 'else', 'enum', 'enumcount', 'event', 
    'exec', 'expands', 'explicit', 'export', 'exportstructs', 'extends', 
    'false', 'final', 'float', 'for', 'foreach', 'function', 'global', 
    'globalconfig', 'goto', 'guid', 'hidecategories', 'hidedropdown', 
    'hideparent', 'if', 'ignores', 'import', 'init', 'input', 'insert', 
    'instanced', 'int', 'intrinsic', 'invariant', 'iterator', 'latent', 
    'length', 'local', 'localized', 'long', 'mesh', 'model', 'mutable', 'name', 
    'native', 'nativereplication', 'new', 'noexport', 'none', 
    'noteditinlinenew', 'notplaceable', 'nousercreate', 'operator', 'optional', 
    'out', 'parseconfig', 'perobjectconfig', 'placeable', 'plane', 'pointer', 
    'postoperator', 'preoperator', 'private', 'protected', 'register', 
    'reliable', 'remove', 'replication', 'return', 'rng', 'rot', 'rotator', 
    'safereplace', 'scale', 'scriptconst', 'self', 'showcategories', 
    'simulated', 'singular', 'skip', 'sound', 'state', 'static', 'stop', 
    'string', 'struct', 'super', 'switch', 'texture', 'transient', 'travel', 
    'true', 'unreliable', 'until', 'var', 'vect', 'vector', 'void', 'while', 
    'within' 
  );

  KeyIndices: array[0..732] of Integer = (
    -1, -1, -1, -1, -1, -1, 78, -1, -1, -1, -1, 25, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 79, -1, -1, -1, -1, -1, -1, -1, -1, 104, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 36, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 34, -1, -1, -1, 18, -1, -1, -1, -1, -1, 30, 1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 63, -1, -1, -1, -1, 114, 
    -1, -1, 121, -1, -1, -1, -1, -1, 105, -1, -1, 108, -1, 135, 9, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 117, 33, 109, -1, -1, -1, -1, -1, -1, 90, -1, -1, 
    -1, -1, -1, 106, -1, -1, -1, -1, -1, -1, -1, 124, -1, -1, -1, -1, 19, -1, 
    -1, -1, -1, 81, -1, 82, -1, -1, -1, -1, 40, 15, -1, -1, -1, 52, -1, 80, -1, 
    -1, -1, -1, -1, -1, 136, -1, -1, 61, -1, 113, -1, -1, -1, 83, -1, -1, -1, 
    -1, -1, -1, 27, -1, -1, 133, -1, -1, -1, -1, 62, -1, -1, -1, -1, -1, -1, -1, 
    76, -1, -1, -1, -1, -1, -1, -1, 126, -1, -1, -1, -1, -1, 2, -1, -1, -1, -1, 
    51, -1, -1, -1, -1, 44, -1, 22, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 20, -1, -1, -1, 8, -1, -1, -1, 110, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 96, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 65, -1, -1, 
    -1, -1, -1, -1, -1, 39, 24, -1, -1, -1, -1, 54, -1, 4, 123, -1, -1, -1, -1, 
    -1, -1, 50, 141, -1, -1, -1, -1, -1, -1, -1, 87, -1, -1, 21, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 60, -1, -1, -1, -1, -1, 85, -1, 
    -1, -1, -1, -1, 70, -1, 68, 131, -1, -1, 69, -1, -1, -1, -1, -1, 128, 26, 
    -1, -1, -1, -1, -1, -1, -1, -1, 7, -1, -1, 142, -1, -1, 122, -1, 74, -1, -1, 
    -1, -1, -1, -1, -1, 13, -1, -1, -1, -1, 101, 119, -1, -1, 94, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 100, -1, -1, -1, -1, -1, 89, -1, -1, 0, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 29, -1, -1, 92, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 112, -1, -1, -1, -1, 67, -1, -1, 45, -1, 
    116, -1, -1, 132, 28, -1, -1, -1, 31, -1, -1, -1, 77, -1, -1, -1, -1, -1, 
    91, -1, 37, -1, -1, -1, -1, 35, -1, 6, -1, -1, -1, -1, -1, -1, -1, 97, -1, 
    -1, -1, -1, -1, 53, -1, 84, -1, -1, -1, -1, 56, 14, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 23, -1, 107, -1, -1, -1, -1, 98, -1, -1, 75, -1, -1, -1, -1, 
    -1, 88, -1, -1, 103, -1, -1, 93, -1, -1, -1, -1, -1, -1, -1, -1, -1, 59, 
    139, 11, 42, -1, -1, 95, -1, -1, -1, -1, -1, 3, -1, -1, -1, 38, -1, -1, -1, 
    -1, -1, -1, -1, -1, 16, -1, 46, -1, -1, -1, -1, -1, 102, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 111, -1, -1, 41, -1, -1, -1, 
    -1, -1, -1, -1, -1, 48, 64, -1, -1, -1, -1, 86, -1, 58, 43, 72, -1, -1, 66, 
    137, 71, -1, -1, -1, -1, -1, 129, -1, -1, -1, -1, -1, -1, -1, -1, 17, 130, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 120, -1, 73, -1, -1, 118, -1, -1, -1, 
    -1, -1, -1, 138, -1, -1, -1, 55, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 10, -1, -1, -1, -1, -1, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, 115, -1, 
    -1, -1, -1, 32, 47, 49, -1, -1, -1, -1, -1, -1, -1, 57, -1, -1, -1, -1, -1, 
    -1, 125, 134, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 99, 12, -1, 127, 
    140, -1, -1 
  );

{$Q-}
function TSynUnrealSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 41 + Ord(Str^) * 701;
    Inc(Str);
  end;
  Result := Result mod 733;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynUnrealSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynUnrealSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[410] := FuncAbstract;
  FIdentFuncTable[71] := FuncAlways;
  FIdentFuncTable[219] := FuncArray;
  FIdentFuncTable[554] := FuncArraycount;
  FIdentFuncTable[294] := FuncAssert;
  FIdentFuncTable[681] := FuncAuto;
  FIdentFuncTable[477] := FuncAutomated;
  FIdentFuncTable[364] := FuncBool;
  FIdentFuncTable[249] := FuncBoundingbox;
  FIdentFuncTable[109] := FuncBoundingvolume;
  FIdentFuncTable[675] := FuncBreak;
  FIdentFuncTable[544] := FuncButton;
  FIdentFuncTable[727] := FuncByte;
  FIdentFuncTable[380] := FuncCache;
  FIdentFuncTable[499] := FuncCacheexempt;
  FIdentFuncTable[160] := FuncCase;
  FIdentFuncTable[567] := FuncCatch;
  FIdentFuncTable[635] := FuncClass;
  FIdentFuncTable[64] := FuncCoerce;
  FIdentFuncTable[147] := FuncCollapsecategories;
  FIdentFuncTable[245] := FuncColor;
  FIdentFuncTable[314] := FuncConfig;
  FIdentFuncTable[231] := FuncConst;
  FIdentFuncTable[510] := FuncContinue;
  FIdentFuncTable[287] := FuncCoords;
  FIdentFuncTable[11] := FuncCpptext;
  FIdentFuncTable[355] := FuncCross;
  FIdentFuncTable[189] := FuncDefault;
  FIdentFuncTable[454] := FuncDefaultproperties;
  FIdentFuncTable[425] := FuncDelegate;
  FIdentFuncTable[70] := FuncDelete;
  FIdentFuncTable[458] := FuncDependson;
  FIdentFuncTable[696] := FuncDeprecated;
  FIdentFuncTable[120] := FuncDo;
  FIdentFuncTable[60] := FuncDontcollapsecategories;
  FIdentFuncTable[475] := FuncDot;
  FIdentFuncTable[49] := FuncEach;
  FIdentFuncTable[470] := FuncEdfindable;
  FIdentFuncTable[558] := FuncEditconst;
  FIdentFuncTable[286] := FuncEditconstarray;
  FIdentFuncTable[159] := FuncEditinline;
  FIdentFuncTable[596] := FuncEditinlinenew;
  FIdentFuncTable[545] := FuncEditinlinenotify;
  FIdentFuncTable[614] := FuncEditinlineuse;
  FIdentFuncTable[229] := FuncElse;
  FIdentFuncTable[448] := FuncEnum;
  FIdentFuncTable[569] := FuncEnumcount;
  FIdentFuncTable[697] := FuncEvent;
  FIdentFuncTable[605] := FuncExec;
  FIdentFuncTable[698] := FuncExpands;
  FIdentFuncTable[302] := FuncExplicit;
  FIdentFuncTable[224] := FuncExport;
  FIdentFuncTable[164] := FuncExportstructs;
  FIdentFuncTable[491] := FuncExtends;
  FIdentFuncTable[292] := FuncFalse;
  FIdentFuncTable[662] := FuncFinal;
  FIdentFuncTable[498] := FuncFloat;
  FIdentFuncTable[706] := FuncFor;
  FIdentFuncTable[613] := FuncForeach;
  FIdentFuncTable[542] := FuncFunction;
  FIdentFuncTable[330] := FuncGlobal;
  FIdentFuncTable[176] := FuncGlobalconfig;
  FIdentFuncTable[197] := FuncGoto;
  FIdentFuncTable[89] := FuncGuid;
  FIdentFuncTable[606] := FuncHidecategories;
  FIdentFuncTable[278] := FuncHidedropdown;
  FIdentFuncTable[618] := FuncHideparent;
  FIdentFuncTable[445] := FuncIf;
  FIdentFuncTable[344] := FuncIgnores;
  FIdentFuncTable[348] := FuncImport;
  FIdentFuncTable[342] := FuncInit;
  FIdentFuncTable[620] := FuncInput;
  FIdentFuncTable[615] := FuncInsert;
  FIdentFuncTable[648] := FuncInstanced;
  FIdentFuncTable[372] := FuncInt;
  FIdentFuncTable[520] := FuncIntrinsic;
  FIdentFuncTable[205] := FuncInvariant;
  FIdentFuncTable[462] := FuncIterator;
  FIdentFuncTable[6] := FuncLatent;
  FIdentFuncTable[24] := FuncLength;
  FIdentFuncTable[166] := FuncLocal;
  FIdentFuncTable[152] := FuncLocalized;
  FIdentFuncTable[154] := FuncLong;
  FIdentFuncTable[182] := FuncMesh;
  FIdentFuncTable[493] := FuncModel;
  FIdentFuncTable[336] := FuncMutable;
  FIdentFuncTable[611] := FuncName;
  FIdentFuncTable[311] := FuncNative;
  FIdentFuncTable[526] := FuncNativereplication;
  FIdentFuncTable[407] := FuncNew;
  FIdentFuncTable[128] := FuncNoexport;
  FIdentFuncTable[468] := FuncNone;
  FIdentFuncTable[428] := FuncNoteditinlinenew;
  FIdentFuncTable[532] := FuncNotplaceable;
  FIdentFuncTable[389] := FuncNousercreate;
  FIdentFuncTable[548] := FuncOperator;
  FIdentFuncTable[265] := FuncOptional;
  FIdentFuncTable[485] := FuncOut;
  FIdentFuncTable[517] := FuncParseconfig;
  FIdentFuncTable[726] := FuncPerobjectconfig;
  FIdentFuncTable[401] := FuncPlaceable;
  FIdentFuncTable[385] := FuncPlane;
  FIdentFuncTable[575] := FuncPointer;
  FIdentFuncTable[529] := FuncPostoperator;
  FIdentFuncTable[33] := FuncPreoperator;
  FIdentFuncTable[103] := FuncPrivate;
  FIdentFuncTable[134] := FuncProtected;
  FIdentFuncTable[512] := FuncRegister;
  FIdentFuncTable[106] := FuncReliable;
  FIdentFuncTable[121] := FuncRemove;
  FIdentFuncTable[253] := FuncReplication;
  FIdentFuncTable[593] := FuncReturn;
  FIdentFuncTable[440] := FuncRng;
  FIdentFuncTable[178] := FuncRot;
  FIdentFuncTable[94] := FuncRotator;
  FIdentFuncTable[691] := FuncSafereplace;
  FIdentFuncTable[450] := FuncScale;
  FIdentFuncTable[119] := FuncScriptconst;
  FIdentFuncTable[651] := FuncSelf;
  FIdentFuncTable[386] := FuncShowcategories;
  FIdentFuncTable[646] := FuncSimulated;
  FIdentFuncTable[97] := FuncSingular;
  FIdentFuncTable[370] := FuncSkip;
  FIdentFuncTable[295] := FuncSound;
  FIdentFuncTable[142] := FuncState;
  FIdentFuncTable[713] := FuncStatic;
  FIdentFuncTable[213] := FuncStop;
  FIdentFuncTable[729] := FuncString;
  FIdentFuncTable[354] := FuncStruct;
  FIdentFuncTable[626] := FuncSuper;
  FIdentFuncTable[636] := FuncSwitch;
  FIdentFuncTable[345] := FuncTexture;
  FIdentFuncTable[453] := FuncTransient;
  FIdentFuncTable[192] := FuncTravel;
  FIdentFuncTable[714] := FuncTrue;
  FIdentFuncTable[108] := FuncUnreliable;
  FIdentFuncTable[173] := FuncUntil;
  FIdentFuncTable[619] := FuncVar;
  FIdentFuncTable[658] := FuncVect;
  FIdentFuncTable[543] := FuncVector;
  FIdentFuncTable[730] := FuncVoid;
  FIdentFuncTable[303] := FuncWhile;
  FIdentFuncTable[367] := FuncWithin;
end;

function TSynUnrealSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncAbstract(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncAlways(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncArray(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncArraycount(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncAssert(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncAuto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncAutomated(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncBool(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncBoundingbox(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncBoundingvolume(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncBreak(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncButton(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncByte(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncCache(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncCacheexempt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncCase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncCatch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncClass(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncCoerce(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncCollapsecategories(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncColor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncConfig(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncConst(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncContinue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncCoords(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncCpptext(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;

end;

function TSynUnrealSyn.FuncCross(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSymbol
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncDefault(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncDefaultproperties(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncDelegate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncDelete(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncDependson(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncDeprecated(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncDo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncDontcollapsecategories(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncDot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSymbol
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncEach(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncEdfindable(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncEditconst(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncEditconstarray(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncEditinline(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncEditinlinenew(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncEditinlinenotify(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncEditinlineuse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncElse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncEnum(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncEnumcount(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncEvent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncExec(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncExpands(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncExplicit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncExport(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncExportstructs(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncExtends(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncFalse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncFinal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncFloat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncFor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncForeach(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncFunction(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncGlobal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncGlobalconfig(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncGoto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncGuid(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncHidecategories(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncHidedropdown(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncHideparent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncIf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncIgnores(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncImport(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncInit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncInput(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncInsert(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncInstanced(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncInt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncIntrinsic(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncInvariant(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncIterator(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncLatent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncLength(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncLocal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncLocalized(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncLong(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncMesh(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncModel(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncMutable(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncName(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncNative(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncNativereplication(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncNew(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncNoexport(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncNone(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncNoteditinlinenew(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncNotplaceable(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncNousercreate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncOperator(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncOptional(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncOut(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncParseconfig(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncPerobjectconfig(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncPlaceable(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncPlane(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncPointer(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncPostoperator(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncPreoperator(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncPrivate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncProtected(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncRegister(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncReliable(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncRemove(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncReplication(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncReturn(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncRng(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncRot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncRotator(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncSafereplace(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncScale(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncScriptconst(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncSelf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncShowcategories(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncSimulated(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;

end;

function TSynUnrealSyn.FuncSingular(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncSkip(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncSound(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncState(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncStatic(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncStop(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncString(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncStruct(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncSuper(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncSwitch(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncTexture(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncTransient(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncTravel(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncTrue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncUnreliable(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncUntil(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncVar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncVect(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncVector(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncVoid(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncWhile(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncWithin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

constructor TSynUnrealSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

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
  FKey2Attri := TSynHighlighterAttributes.Create(SYNS_AttrSecondReservedWord, SYNS_FriendlyAttrSecondReservedWord);
  FKey2Attri.Style:= [fsBold];
  AddAttribute(FKey2Attri);
  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);
  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);
  FString2Attri := TSynHighlighterAttributes.Create(SYNS_AttrSingleString, SYNS_FriendlyAttrSingleString);
  AddAttribute(FString2Attri);
  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  FDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrDirective, SYNS_FriendlyAttrDirective);
  AddAttribute(FDirecAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  FRange := rsUnknown;
  FDefaultFilter := SYNS_FilterCPP;
end; { Create }

procedure TSynUnrealSyn.AnsiCProc;
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

  while not IsLineEnd(Run) do
    case FLine[Run] of
      '*':
        if FLine[Run + 1] = '/' then
        begin
          Inc(Run, 2);
          if FRange = rsDirectiveComment then
            FRange := rsDirective
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

procedure TSynUnrealSyn.AndSymbolProc;
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

procedure TSynUnrealSyn.AsciiCharProc;
begin
  FTokenID := tkString2;
  repeat
    if IsLineEnd(Run) then Break;
    if FLine[Run] = #92 then                             {backslash}
        {if we have an escaped single quote it doesn't count}
      if FLine[Run + 1] = #39 then Inc(Run);
    Inc(Run);
  until FLine[Run] = #39;
  if not IsLineEnd(Run) then Inc(Run);
end;

procedure TSynUnrealSyn.BraceCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkBraceClose;
end;

procedure TSynUnrealSyn.BraceOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkBraceOpen;
end;

procedure TSynUnrealSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run + 1] = #10 then Inc(Run);
end;

procedure TSynUnrealSyn.ColonProc;
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

procedure TSynUnrealSyn.CommaProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkComma;
end;

procedure TSynUnrealSyn.DirectiveProc;
begin
  if IsLineEnd(Run) then
  begin
    if (Run <= 0) then
      FRange := rsUnknown;
    NextProcedure;
  end
  else
  begin
    FTokenID := tkDirective;
    while TRUE do
      case FLine[Run] of
        '/': // comment?
          begin
            if FLine[Run + 1] = '/' then // is end of directive as well
              Break
            else if FLine[Run + 1] = '*' then
            begin // might be embedded only
              FRange := rsDirectiveComment;
              Break;
            end else
              Inc(Run);
          end;
        #0, #10, #13:
          begin
            FRange := rsUnknown;
            Break;
          end;
        else Inc(Run);
      end;
  end;
end;

procedure TSynUnrealSyn.EqualProc;
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

procedure TSynUnrealSyn.GreaterProc;
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

procedure TSynUnrealSyn.QuestionProc;
begin
  FTokenID := tkSymbol;                {conditional}
  FExtTokenID := xtkQuestion;
  Inc(Run);
end;

procedure TSynUnrealSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + Run));
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do Inc(Run);
end;

procedure TSynUnrealSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynUnrealSyn.LowerProc;
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

procedure TSynUnrealSyn.MinusProc;
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

procedure TSynUnrealSyn.ModSymbolProc;
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

procedure TSynUnrealSyn.NotSymbolProc;
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

procedure TSynUnrealSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynUnrealSyn.NumberProc;

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

procedure TSynUnrealSyn.OrSymbolProc;
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

procedure TSynUnrealSyn.PlusProc;
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

procedure TSynUnrealSyn.PointProc;
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

procedure TSynUnrealSyn.RoundCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundClose;
  Dec(FRoundCount);
end;

procedure TSynUnrealSyn.RoundOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundOpen;
  Inc(FRoundCount);
end;

procedure TSynUnrealSyn.SemiColonProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSemiColon;
end;

procedure TSynUnrealSyn.SlashProc;
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
        if FRange <> rsDirectiveComment then
          FRange := rsAnsiC;
        Inc(Run, 2);
        while not IsLineEnd(Run) do
          case FLine[Run] of
            '*':
              if FLine[Run + 1] = '/' then
              begin
                Inc(Run, 2);
                if FRange = rsDirectiveComment then
                  FRange := rsDirective
                else
                  FRange := rsUnknown;
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

procedure TSynUnrealSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynUnrealSyn.SquareCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSquareClose;
  Dec(FSquareCount);
end;

procedure TSynUnrealSyn.SquareOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSquareOpen;
  Inc(FSquareCount);
end;

procedure TSynUnrealSyn.StarProc;
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

procedure TSynUnrealSyn.StringProc;
begin
  FTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then Inc(Run, 2);
  repeat
    if IsLineEnd(Run) then
      Break;
    if FLine[Run] = #92 then                             {backslash}
        case FLine[Run + 1] of
          #10: Inc(Run);               {line continuation character}
          #34: Inc(Run);               {escaped quote doesn't count}
          #92: Inc(Run);
        end;
    Inc(Run);
  until FLine[Run] = #34;
  if not IsLineEnd(Run) then Inc(Run);
end;

procedure TSynUnrealSyn.DollarSignProc;
begin
  FTokenID := tkSymbol;
  Inc(run);
end;


procedure TSynUnrealSyn.TildeProc;
begin
  Inc(Run);                            {bitwise complement}
  FTokenID := tkSymbol;
  FExtTokenID := xtkBitComplement;
end;

procedure TSynUnrealSyn.XOrSymbolProc;
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

procedure TSynUnrealSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynUnrealSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsAnsiC, rsDirectiveComment: AnsiCProc;
    rsDirective: DirectiveProc;
  else
    begin
      FRange := rsUnknown;
      NextProcedure
    end;
  end;
  inherited;
end;

procedure TSynUnrealSyn.NextProcedure;
begin
  case FLine[Run] of
    '&': AndSymbolProc;
    #39: AsciiCharProc;
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
    '$', '@': DollarSignProc;
    '~': TildeProc;
    '^': XOrSymbolProc;
    else UnknownProc;
  end;
end;

function TSynUnrealSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynUnrealSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynUnrealSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynUnrealSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynUnrealSyn.GetExtTokenID: TxtkTokenKind;
begin
  Result := FExtTokenID;
end;


function TSynUnrealSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterCPP;
end; { IsFilterStored }


function TSynUnrealSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkDirective: Result := FDirecAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkKey2: Result := FKey2Attri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkString2: Result := FString2Attri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FInvalidAttri;
    else Result := nil;
  end;
end;

function TSynUnrealSyn.GetTokenKind: Integer;
begin
  Result := Ord(GetTokenID);
end;

procedure TSynUnrealSyn.ResetRange;
begin
  FRange:= rsUnknown;
end;

procedure TSynUnrealSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynUnrealSyn.EnumUserSettings(settings: TStrings);
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

function TSynUnrealSyn.UseUserSettings(settingIndex: Integer): Boolean;
// Possible parameter values:
//   index into TStrings returned by EnumUserSettings
// Possible return values:
//   true : settings were read and used
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
             '\SOFTWARE\Borland\C++Builder\'+settingTag+'\Highlight',name,true);
      end; { ReadCPPB1 }

      function ReadCPPB3OrMore(settingTag: string; attri: TSynHighlighterAttributes; key: string): Boolean;
      begin
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
                 '\Software\Borland\C++Builder\'+settingTag+'\Editor\Highlight',
                 key, False);
      end; { ReadCPPB3OrMore }

    begin { ReadCPPBSetting }
      try
        if (settingTag[1] = '1')
          then Result := ReadCPPB1(settingTag,attri,key)
          else Result := ReadCPPB3OrMore(settingTag,attri,key);
      except Result := False; end;
    end; { ReadCPPBSetting }

  var
    tmpStringAttri    : TSynHighlighterAttributes;
    tmpNumberAttri    : TSynHighlighterAttributes;
    tmpKeyAttri       : TSynHighlighterAttributes;
    tmpSymbolAttri    : TSynHighlighterAttributes;
    tmpCommentAttri   : TSynHighlighterAttributes;
    tmpIdentifierAttri: TSynHighlighterAttributes;
    tmpInvalidAttri   : TSynHighlighterAttributes;
    tmpSpaceAttri     : TSynHighlighterAttributes;
    tmpDirecAttri     : TSynHighlighterAttributes;
    sl                 : TStringList;

  begin { ReadCPPBSettings }
    sl := TStringList.Create;
    try
      EnumUserSettings(sl);
      if settingIndex >= sl.Count then Result := False
      else begin
        tmpStringAttri    := TSynHighlighterAttributes.Create('', '');
        tmpNumberAttri    := TSynHighlighterAttributes.Create('', '');
        tmpKeyAttri       := TSynHighlighterAttributes.Create('', '');
        tmpSymbolAttri    := TSynHighlighterAttributes.Create('', '');
        tmpCommentAttri   := TSynHighlighterAttributes.Create('', '');
        tmpIdentifierAttri:= TSynHighlighterAttributes.Create('', '');
        tmpInvalidAttri   := TSynHighlighterAttributes.Create('', '');
        tmpSpaceAttri     := TSynHighlighterAttributes.Create('', '');
        tmpDirecAttri     := TSynHighlighterAttributes.Create('', '');
        tmpStringAttri    .Assign(FStringAttri);
        tmpNumberAttri    .Assign(FNumberAttri);
        tmpKeyAttri       .Assign(FKeyAttri);
        tmpSymbolAttri    .Assign(FSymbolAttri);
        tmpCommentAttri   .Assign(FCommentAttri);
        tmpIdentifierAttri.Assign(FIdentifierAttri);
        tmpInvalidAttri   .Assign(FInvalidAttri);
        tmpSpaceAttri     .Assign(FSpaceAttri);
        tmpDirecAttri     .Assign(FDirecAttri);
        Result := ReadCPPBSetting(sl[settingIndex],FCommentAttri,'Comment')       and
                  ReadCPPBSetting(sl[settingIndex],FIdentifierAttri,'Identifier') and
                  ReadCPPBSetting(sl[settingIndex],FInvalidAttri,'Illegal Char')  and
                  ReadCPPBSetting(sl[settingIndex],FKeyAttri,'Reserved word')     and
                  ReadCPPBSetting(sl[settingIndex],FNumberAttri,'Integer')        and
                  ReadCPPBSetting(sl[settingIndex],FSpaceAttri,'Whitespace')      and
                  ReadCPPBSetting(sl[settingIndex],FStringAttri,'String')         and
                  ReadCPPBSetting(sl[settingIndex],FSymbolAttri,'Symbol')         and
                  ReadCPPBSetting(sl[settingIndex],FDirecAttri,'Preprocessor');
        if not Result then begin
          FStringAttri    .Assign(tmpStringAttri);
          FString2Attri   .Assign(tmpStringAttri);
          FNumberAttri    .Assign(tmpNumberAttri);
          FKeyAttri       .Assign(tmpKeyAttri);
          FKey2Attri      .Assign(tmpKeyAttri);
          FSymbolAttri    .Assign(tmpSymbolAttri);
          FCommentAttri   .Assign(tmpCommentAttri);
          FIdentifierAttri.Assign(tmpIdentifierAttri);
          FInvalidAttri.Assign(tmpInvalidAttri);
          FSpaceAttri     .Assign(tmpSpaceAttri);
          FDirecAttri     .Assign(tmpDirecAttri);
        end;
        tmpStringAttri    .Free;
        tmpNumberAttri    .Free;
        tmpKeyAttri       .Free;
        tmpSymbolAttri    .Free;
        tmpCommentAttri   .Free;
        tmpIdentifierAttri.Free;
        tmpInvalidAttri   .Free;
        tmpSpaceAttri     .Free;
        tmpDirecAttri     .Free;
      end;
    finally
      sl.Free;
    end;
  end; { ReadCPPBSettings }

begin
  Result := ReadCPPBSettings(settingIndex);
end; { TSynUnrealSyn.UseUserSettings }

class function TSynUnrealSyn.GetLanguageName: string;
begin
  Result := SYNS_LangUnreal;
end;

class function TSynUnrealSyn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

function TSynUnrealSyn.GetSampleSource: UnicodeString;
begin
  Result := '//----Comment-----------------------------------------------------------'#13#10+
            'class TestObject expands Object native;'#13#10+
            #13#10+
            '#exec MESH    IMPORT     MESH=Something ANIVFILE=MODELS\Something.3D DATAFILE=MODELS\Something.3D X=0 Y=0 Z=0 MLOD=0'#13#10+
            #13#10+
            'var() Sound HitSound;'#13#10+
            'function Cast()'#13#10+
            '{'#13#10+
            '  Super.Cast();'#13#10+
            '  CastTime = 50;'#13#10+
            '  GatherEffect = Spawn( class''SomethingCorona'',,, GetStartLoc(), Pawn(Owner).ViewRotation );'#13#10+
            '  GatherEffect.SetFollowPawn( Pawn(Owner) );'#13#10+
            '}'#13#10+
            #13#10+
            'defaultproperties'#13#10+
            '{'#13#10+
            '  PickupMessage="You have picked up a thing."'#13#10+
            '}';
end;

class function TSynUnrealSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangUnreal;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynUnrealSyn);
{$ENDIF}
end.
