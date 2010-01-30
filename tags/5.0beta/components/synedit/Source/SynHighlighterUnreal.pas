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

{$IFNDEF QSYNHIGHLIGHTERUNREAL}
unit SynHighlighterUnreal;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditHighlighter,
  QSynEditTypes,
  QSynUnicode,
{$ELSE}
  Graphics,
  Registry,
  Windows, // registry constants
  SynEditHighlighter,
  SynEditTypes,
  SynUnicode,
{$ENDIF}
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

  TRangeState = (rsANil, rsAnsiC, rsDirective, rsDirectiveComment, rsUnKnown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TSynUnrealSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    FRoundCount: Integer;
    FSquareCount: Integer;
    FTokenID: TtkTokenKind;
    FExtTokenID: TxtkTokenKind;
    fIdentFuncTable: array[0..732] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fDirecAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fInvalidAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fKey2Attri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fString2Attri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
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
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property DirecAttri: TSynHighlighterAttributes read fDirecAttri
      write fDirecAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property InvalidAttri: TSynHighlighterAttributes read fInvalidAttri
      write fInvalidAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property Key2Attri: TSynHighlighterAttributes read fKey2Attri write fKey2Attri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SingleStringAttri: TSynHighlighterAttributes read fString2Attri
      write fString2Attri;
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
    inc(Str);
  end;
  Result := Result mod 733;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynUnrealSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynUnrealSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[410] := FuncAbstract;
  fIdentFuncTable[71] := FuncAlways;
  fIdentFuncTable[219] := FuncArray;
  fIdentFuncTable[554] := FuncArraycount;
  fIdentFuncTable[294] := FuncAssert;
  fIdentFuncTable[681] := FuncAuto;
  fIdentFuncTable[477] := FuncAutomated;
  fIdentFuncTable[364] := FuncBool;
  fIdentFuncTable[249] := FuncBoundingbox;
  fIdentFuncTable[109] := FuncBoundingvolume;
  fIdentFuncTable[675] := FuncBreak;
  fIdentFuncTable[544] := FuncButton;
  fIdentFuncTable[727] := FuncByte;
  fIdentFuncTable[380] := FuncCache;
  fIdentFuncTable[499] := FuncCacheexempt;
  fIdentFuncTable[160] := FuncCase;
  fIdentFuncTable[567] := FuncCatch;
  fIdentFuncTable[635] := FuncClass;
  fIdentFuncTable[64] := FuncCoerce;
  fIdentFuncTable[147] := FuncCollapsecategories;
  fIdentFuncTable[245] := FuncColor;
  fIdentFuncTable[314] := FuncConfig;
  fIdentFuncTable[231] := FuncConst;
  fIdentFuncTable[510] := FuncContinue;
  fIdentFuncTable[287] := FuncCoords;
  fIdentFuncTable[11] := FuncCpptext;
  fIdentFuncTable[355] := FuncCross;
  fIdentFuncTable[189] := FuncDefault;
  fIdentFuncTable[454] := FuncDefaultproperties;
  fIdentFuncTable[425] := FuncDelegate;
  fIdentFuncTable[70] := FuncDelete;
  fIdentFuncTable[458] := FuncDependson;
  fIdentFuncTable[696] := FuncDeprecated;
  fIdentFuncTable[120] := FuncDo;
  fIdentFuncTable[60] := FuncDontcollapsecategories;
  fIdentFuncTable[475] := FuncDot;
  fIdentFuncTable[49] := FuncEach;
  fIdentFuncTable[470] := FuncEdfindable;
  fIdentFuncTable[558] := FuncEditconst;
  fIdentFuncTable[286] := FuncEditconstarray;
  fIdentFuncTable[159] := FuncEditinline;
  fIdentFuncTable[596] := FuncEditinlinenew;
  fIdentFuncTable[545] := FuncEditinlinenotify;
  fIdentFuncTable[614] := FuncEditinlineuse;
  fIdentFuncTable[229] := FuncElse;
  fIdentFuncTable[448] := FuncEnum;
  fIdentFuncTable[569] := FuncEnumcount;
  fIdentFuncTable[697] := FuncEvent;
  fIdentFuncTable[605] := FuncExec;
  fIdentFuncTable[698] := FuncExpands;
  fIdentFuncTable[302] := FuncExplicit;
  fIdentFuncTable[224] := FuncExport;
  fIdentFuncTable[164] := FuncExportstructs;
  fIdentFuncTable[491] := FuncExtends;
  fIdentFuncTable[292] := FuncFalse;
  fIdentFuncTable[662] := FuncFinal;
  fIdentFuncTable[498] := FuncFloat;
  fIdentFuncTable[706] := FuncFor;
  fIdentFuncTable[613] := FuncForeach;
  fIdentFuncTable[542] := FuncFunction;
  fIdentFuncTable[330] := FuncGlobal;
  fIdentFuncTable[176] := FuncGlobalconfig;
  fIdentFuncTable[197] := FuncGoto;
  fIdentFuncTable[89] := FuncGuid;
  fIdentFuncTable[606] := FuncHidecategories;
  fIdentFuncTable[278] := FuncHidedropdown;
  fIdentFuncTable[618] := FuncHideparent;
  fIdentFuncTable[445] := FuncIf;
  fIdentFuncTable[344] := FuncIgnores;
  fIdentFuncTable[348] := FuncImport;
  fIdentFuncTable[342] := FuncInit;
  fIdentFuncTable[620] := FuncInput;
  fIdentFuncTable[615] := FuncInsert;
  fIdentFuncTable[648] := FuncInstanced;
  fIdentFuncTable[372] := FuncInt;
  fIdentFuncTable[520] := FuncIntrinsic;
  fIdentFuncTable[205] := FuncInvariant;
  fIdentFuncTable[462] := FuncIterator;
  fIdentFuncTable[6] := FuncLatent;
  fIdentFuncTable[24] := FuncLength;
  fIdentFuncTable[166] := FuncLocal;
  fIdentFuncTable[152] := FuncLocalized;
  fIdentFuncTable[154] := FuncLong;
  fIdentFuncTable[182] := FuncMesh;
  fIdentFuncTable[493] := FuncModel;
  fIdentFuncTable[336] := FuncMutable;
  fIdentFuncTable[611] := FuncName;
  fIdentFuncTable[311] := FuncNative;
  fIdentFuncTable[526] := FuncNativereplication;
  fIdentFuncTable[407] := FuncNew;
  fIdentFuncTable[128] := FuncNoexport;
  fIdentFuncTable[468] := FuncNone;
  fIdentFuncTable[428] := FuncNoteditinlinenew;
  fIdentFuncTable[532] := FuncNotplaceable;
  fIdentFuncTable[389] := FuncNousercreate;
  fIdentFuncTable[548] := FuncOperator;
  fIdentFuncTable[265] := FuncOptional;
  fIdentFuncTable[485] := FuncOut;
  fIdentFuncTable[517] := FuncParseconfig;
  fIdentFuncTable[726] := FuncPerobjectconfig;
  fIdentFuncTable[401] := FuncPlaceable;
  fIdentFuncTable[385] := FuncPlane;
  fIdentFuncTable[575] := FuncPointer;
  fIdentFuncTable[529] := FuncPostoperator;
  fIdentFuncTable[33] := FuncPreoperator;
  fIdentFuncTable[103] := FuncPrivate;
  fIdentFuncTable[134] := FuncProtected;
  fIdentFuncTable[512] := FuncRegister;
  fIdentFuncTable[106] := FuncReliable;
  fIdentFuncTable[121] := FuncRemove;
  fIdentFuncTable[253] := FuncReplication;
  fIdentFuncTable[593] := FuncReturn;
  fIdentFuncTable[440] := FuncRng;
  fIdentFuncTable[178] := FuncRot;
  fIdentFuncTable[94] := FuncRotator;
  fIdentFuncTable[691] := FuncSafereplace;
  fIdentFuncTable[450] := FuncScale;
  fIdentFuncTable[119] := FuncScriptconst;
  fIdentFuncTable[651] := FuncSelf;
  fIdentFuncTable[386] := FuncShowcategories;
  fIdentFuncTable[646] := FuncSimulated;
  fIdentFuncTable[97] := FuncSingular;
  fIdentFuncTable[370] := FuncSkip;
  fIdentFuncTable[295] := FuncSound;
  fIdentFuncTable[142] := FuncState;
  fIdentFuncTable[713] := FuncStatic;
  fIdentFuncTable[213] := FuncStop;
  fIdentFuncTable[729] := FuncString;
  fIdentFuncTable[354] := FuncStruct;
  fIdentFuncTable[626] := FuncSuper;
  fIdentFuncTable[636] := FuncSwitch;
  fIdentFuncTable[345] := FuncTexture;
  fIdentFuncTable[453] := FuncTransient;
  fIdentFuncTable[192] := FuncTravel;
  fIdentFuncTable[714] := FuncTrue;
  fIdentFuncTable[108] := FuncUnreliable;
  fIdentFuncTable[173] := FuncUntil;
  fIdentFuncTable[619] := FuncVar;
  fIdentFuncTable[658] := FuncVect;
  fIdentFuncTable[543] := FuncVector;
  fIdentFuncTable[730] := FuncVoid;
  fIdentFuncTable[303] := FuncWhile;
  fIdentFuncTable[367] := FuncWithin;
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

  fCaseSensitive := False;

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
  fKey2Attri := TSynHighlighterAttributes.Create(SYNS_AttrSecondReservedWord, SYNS_FriendlyAttrSecondReservedWord);
  fKey2Attri.Style:= [fsBold];
  AddAttribute(fKey2Attri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fString2Attri := TSynHighlighterAttributes.Create(SYNS_AttrSingleString, SYNS_FriendlyAttrSingleString);
  AddAttribute(fString2Attri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  fDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrDirective, SYNS_FriendlyAttrDirective);
  AddAttribute(fDirecAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fRange := rsUnknown;
  fDefaultFilter := SYNS_FilterCPP;
end; { Create }

procedure TSynUnrealSyn.AnsiCProc;
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

  while not IsLineEnd(Run) do
    case FLine[Run] of
      '*':
        if fLine[Run + 1] = '/' then
        begin
          inc(Run, 2);
          if fRange = rsDirectiveComment then                              
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

procedure TSynUnrealSyn.AndSymbolProc;
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

procedure TSynUnrealSyn.AsciiCharProc;
begin
  fTokenID := tkString2;
  repeat
    if IsLineEnd(Run) then break;
    if FLine[Run] = #92 then                             {backslash}
        {if we have an escaped single quote it doesn't count}
      if FLine[Run + 1] = #39 then inc(Run);
    inc(Run);
  until FLine[Run] = #39;
  if not IsLineEnd(Run) then inc(Run);
end;

procedure TSynUnrealSyn.BraceCloseProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
  FExtTokenID := xtkBraceClose;
end;

procedure TSynUnrealSyn.BraceOpenProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
  FExtTokenID := xtkBraceOpen;
end;

procedure TSynUnrealSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run + 1] = #10 then Inc(Run);
end;

procedure TSynUnrealSyn.ColonProc;
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

procedure TSynUnrealSyn.CommaProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkComma;
end;

procedure TSynUnrealSyn.DirectiveProc;
begin
  if IsLineEnd(Run) then
  begin
    if (Run <= 0) then
      fRange := rsUnknown;
    NextProcedure;
  end
  else
  begin
    fTokenID := tkDirective;
    while TRUE do
      case fLine[Run] of
        '/': // comment?
          begin
            if fLine[Run + 1] = '/' then // is end of directive as well
              break
            else if fLine[Run + 1] = '*' then
            begin // might be embedded only
              fRange := rsDirectiveComment;
              break;
            end else
              Inc(Run);
          end;
        #0, #10, #13:
          begin
            fRange := rsUnknown;
            break;
          end;
        else Inc(Run);
      end;
  end;
end;

procedure TSynUnrealSyn.EqualProc;
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

procedure TSynUnrealSyn.GreaterProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
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

procedure TSynUnrealSyn.QuestionProc;
begin
  fTokenID := tkSymbol;                {conditional}
  FExtTokenID := xtkQuestion;
  inc(Run);
end;

procedure TSynUnrealSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do inc(Run);
end;

procedure TSynUnrealSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynUnrealSyn.LowerProc;
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

procedure TSynUnrealSyn.MinusProc;
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

procedure TSynUnrealSyn.ModSymbolProc;
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

procedure TSynUnrealSyn.NotSymbolProc;
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

procedure TSynUnrealSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynUnrealSyn.NumberProc;

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

procedure TSynUnrealSyn.OrSymbolProc;
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

procedure TSynUnrealSyn.PlusProc;
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

procedure TSynUnrealSyn.PointProc;
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

procedure TSynUnrealSyn.RoundCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkRoundClose;
  dec(FRoundCount);
end;

procedure TSynUnrealSyn.RoundOpenProc;
begin
  inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundOpen;
  inc(FRoundCount);
end;

procedure TSynUnrealSyn.SemiColonProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSemiColon;
end;

procedure TSynUnrealSyn.SlashProc;
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
        if fRange <> rsDirectiveComment then                               
          fRange := rsAnsiC;
        inc(Run, 2);
        while not IsLineEnd(Run) do
          case fLine[Run] of
            '*':
              if fLine[Run + 1] = '/' then
              begin
                inc(Run, 2);
                if fRange = rsDirectiveComment then
                  fRange := rsDirective
                else
                  fRange := rsUnKnown;
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

procedure TSynUnrealSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynUnrealSyn.SquareCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSquareClose;
  dec(FSquareCount);
end;

procedure TSynUnrealSyn.SquareOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSquareOpen;
  inc(FSquareCount);
end;

procedure TSynUnrealSyn.StarProc;
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

procedure TSynUnrealSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then inc(Run, 2);
  repeat
    if IsLineEnd(Run) then break;
    if FLine[Run] = #92 then                             {backslash}
        case FLine[Run + 1] of
          #10: inc(Run);               {line continuation character}
          #34: inc(Run);               {escaped quote doesn't count}
          #92: inc(Run);
        end;
    inc(Run);
  until FLine[Run] = #34;
  if not IsLineEnd(Run) then inc(Run);
end;

procedure TSynUnrealSyn.DollarSignProc;
begin
  fTokenID := tkSymbol;
  inc(run);
end;


procedure TSynUnrealSyn.TildeProc;
begin
  inc(Run);                            {bitwise complement}
  fTokenId := tkSymbol;
  FExtTokenID := xtkBitComplement;
end;

procedure TSynUnrealSyn.XOrSymbolProc;
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

procedure TSynUnrealSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynUnrealSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsAnsiC, rsDirectiveComment: AnsiCProc;
    rsDirective: DirectiveProc;
  else
    begin
      fRange := rsUnknown;
      NextProcedure
    end;
  end;
  inherited;
end;

procedure TSynUnrealSyn.NextProcedure;
begin
  case fLine[Run] of
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

function TSynUnrealSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynUnrealSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynUnrealSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynUnrealSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynUnrealSyn.GetExtTokenID: TxtkTokenKind;
begin
  Result := FExtTokenID;
end;


function TSynUnrealSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCPP;
end; { IsFilterStored }


function TSynUnrealSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkDirective: Result := fDirecAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkKey2: Result := fKey2Attri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkString2: Result := fString2Attri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fInvalidAttri;
    else Result := nil;
  end;
end;

function TSynUnrealSyn.GetTokenKind: integer;
begin
  Result := Ord(GetTokenID);
end;

procedure TSynUnrealSyn.ResetRange;
begin
  fRange:= rsUnknown;
end;

procedure TSynUnrealSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynUnrealSyn.EnumUserSettings(settings: TStrings);
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

function TSynUnrealSyn.UseUserSettings(settingIndex: integer): boolean;
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
      if settingIndex >= sl.Count then Result := false
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
        tmpStringAttri    .Assign(fStringAttri);
        tmpNumberAttri    .Assign(fNumberAttri);
        tmpKeyAttri       .Assign(fKeyAttri);
        tmpSymbolAttri    .Assign(fSymbolAttri);
        tmpCommentAttri   .Assign(fCommentAttri);
        tmpIdentifierAttri.Assign(fIdentifierAttri);
        tmpInvalidAttri   .Assign(fInvalidAttri);
        tmpSpaceAttri     .Assign(fSpaceAttri);
        tmpDirecAttri     .Assign(fDirecAttri);
        Result := ReadCPPBSetting(sl[settingIndex],fCommentAttri,'Comment')       and
                  ReadCPPBSetting(sl[settingIndex],fIdentifierAttri,'Identifier') and
                  ReadCPPBSetting(sl[settingIndex],fInvalidAttri,'Illegal Char')  and
                  ReadCPPBSetting(sl[settingIndex],fKeyAttri,'Reserved word')     and
                  ReadCPPBSetting(sl[settingIndex],fNumberAttri,'Integer')        and
                  ReadCPPBSetting(sl[settingIndex],fSpaceAttri,'Whitespace')      and
                  ReadCPPBSetting(sl[settingIndex],fStringAttri,'String')         and
                  ReadCPPBSetting(sl[settingIndex],fSymbolAttri,'Symbol')         and
                  ReadCPPBSetting(sl[settingIndex],fDirecAttri,'Preprocessor');
        if not Result then begin
          fStringAttri    .Assign(tmpStringAttri);
          fString2Attri   .Assign(tmpStringAttri);
          fNumberAttri    .Assign(tmpNumberAttri);
          fKeyAttri       .Assign(tmpKeyAttri);
          fKey2Attri      .Assign(tmpKeyAttri);
          fSymbolAttri    .Assign(tmpSymbolAttri);
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
{$ENDIF}

begin
{$IFDEF SYN_CLX}
  Result := False;
{$ELSE}
  Result := ReadCPPBSettings(settingIndex);
{$ENDIF}
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
