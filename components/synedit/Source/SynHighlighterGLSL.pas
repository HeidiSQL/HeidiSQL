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

unit SynHighlighterGLSL;

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
  TtkTokenKind = (tkComment, tkDirective, tkIdentifier, tkInterfaceQualifier,
    tkInternalFunction, tkKey, tkNull, tkNumber, tkSpace, tkString, tkSymbol,
    tkUnknown, tkChar, tkFloat, tkHex, tkOctal);

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

  TRangeState = (rsUnknown, rsAnsiC, rsDirective, rsDirectiveComment,
    rsMultiLineString, rsMultiLineDirective);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer; MayBe: PWideChar): TtkTokenKind of object;

  TSynGLSLSyn = class(TSynCustomHighlighter)
  private
    FAsmStart: Boolean;
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FExtTokenID: TxtkTokenKind;
    FIdentFuncTable: array[0..2010] of TIdentFuncTableFunc;
    FInternalFuncTable: array[0..1050] of TIdentFuncTableFunc;
    FInterfaceQualifierAttri: TSynHighlighterAttributes;
    FInternalFunctionsAttri: TSynHighlighterAttributes;
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
    function AltFunc(Index: Integer; Maybe: PWideChar): TtkTokenKind;
    function AltFinalFunc(Index: Integer; Maybe: PWideChar): TtkTokenKind;
    function InternalFunc(Index: Integer; MayBe: PWideChar): TtkTokenKind;
    function KeyWordFunc(Index: Integer; MayBe: PWideChar): TtkTokenKind;
    function InterfaceQualifierFunc(Index: Integer; MayBe: PWideChar): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function HashInternal(Str: PWideChar): Cardinal;
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

    property ExtTokenID: TxtkTokenKind read GetExtTokenID;
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property DirecAttri: TSynHighlighterAttributes read FDirecAttri
      write FDirecAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property InterfaceQualifierAttri: TSynHighlighterAttributes
      read FInterfaceQualifierAttri write FInterfaceQualifierAttri;
    property InternalFunctions: TSynHighlighterAttributes
      read FInternalFunctionsAttri write FInternalFunctionsAttri;
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
  // as this language is case-insensitive keywords *must* be in lowercase
  GKeyWords: array[0..239] of UnicodeString = (
    'active', 'asm', 'atomic_uint', 'attribute', 'bool', 'break', 'buffer',
    'bvec2', 'bvec3', 'bvec4', 'case', 'cast', 'centroid', 'class', 'coherent',
    'common', 'const', 'continue', 'def', 'default', 'discard', 'dmat2',
    'dmat2x2', 'dmat2x3', 'dmat2x4', 'dmat3', 'dmat3x2', 'dmat3x3', 'dmat3x4',
    'dmat4', 'dmat4x2', 'dmat4x3', 'dmat4x4', 'do', 'double', 'dvec2', 'dvec3',
    'dvec4', 'else', 'enum', 'extern', 'external', 'false', 'filter', 'fixed',
    'flat', 'float', 'for', 'fvec2', 'fvec3', 'fvec4', 'goto', 'half', 'highp',
    'hvec2', 'hvec3', 'hvec4', 'if', 'iimage1d', 'iimage1darray', 'iimage2d',
    'iimage2darray', 'iimage2dms', 'iimage2dmsarray', 'iimage2drect',
    'iimage3d', 'iimagebuffer', 'iimagecube', 'iimagecubearray', 'image1d',
    'image1darray', 'image2d', 'image2darray', 'image2dms', 'image2dmsarray',
    'image2drect', 'image3d', 'imagebuffer', 'imagecubearray', 'in', 'inline',
    'inout', 'input', 'int', 'interface', 'invariant', 'isampler1d',
    'isampler1darray', 'isampler2d', 'isampler2darray', 'isampler2dms',
    'isampler2dmsarray', 'isampler2drect', 'isampler3d', 'isamplerbuffer',
    'isamplercube', 'isamplercubearray', 'isubpassinput', 'isubpassinputms',
    'itexture1d', 'itexture1darray', 'itexture2d', 'itexture2darray',
    'itexture2dms', 'itexture2dmsarray', 'itexture2drect', 'itexture3d',
    'itexturebuffer', 'itexturecube', 'itexturecubearray', 'ivec2', 'ivec3',
    'ivec4', 'layout', 'long', 'lowp', 'mat2', 'mat2x2', 'mat2x3', 'mat2x4',
    'mat3', 'mat3x2', 'mat3x3', 'mat3x4', 'mat4', 'mat4x2', 'mat4x3', 'mat4x4',
    'mediump', 'namespace', 'noinline', 'noperspective', 'out', 'output',
    'partition', 'patch', 'precise', 'precision', 'public', 'readonly',
    'resource', 'restrict', 'return', 'sample', 'sampler', 'sampler1d',
    'sampler1darray', 'sampler1darrayshadow', 'sampler1dshadow', 'sampler2d',
    'sampler2darray', 'sampler2darrayshadow', 'sampler2dms', 'sampler2dmsarray',
    'sampler2drectshadow', 'sampler2dshadow', 'sampler3d', 'sampler3drect',
    'samplerbuffer', 'samplercubearray', 'samplercubearrayshadow',
    'samplercubeshadow', 'samplershadow', 'shared', 'short', 'sizeof', 'smooth',
    'static', 'struct', 'subpassinput', 'subpassinputms', 'subroutine',
    'superp', 'switch', 'template', 'texture1d', 'texture1darray', 'texture2d',
    'texture2darray', 'texture2dms', 'texture2dmsarray', 'texture2drect',
    'texture3d', 'texturebuffer', 'texturecube', 'texturecubearray', 'this',
    'true', 'type', 'uimage1d', 'uimage1darray', 'uimage2d', 'uimage2darray',
    'uimage2dms', 'uimage2dmsarray', 'uimage2drect', 'uimage3dimagecube',
    'uimagebuffer', 'uimagecube', 'uimagecubearray', 'uint', 'uniform', 'union',
    'unsigned', 'usampler1d', 'usampler1darray', 'usampler2d',
    'usampler2darraysampler2drect', 'usampler2dms', 'usampler2dmsarray',
    'usampler2drect', 'usampler3dsamplercube', 'usamplerbuffer', 'usamplercube',
    'usamplercubearray', 'using', 'usubpassinput', 'usubpassinputms',
    'utexture1d', 'utexture1darray', 'utexture2d', 'utexture2darray',
    'utexture2dms', 'utexture2dmsarray', 'utexture2drect', 'utexture3d',
    'utexturebuffer', 'utexturecube', 'utexturecubearray', 'uvec2', 'uvec3',
    'uvec4', 'varying', 'vec2', 'vec3', 'vec4', 'void', 'volatile', 'while',
    'writeonly'
  );

  GInternalFunctions: array[0..160] of UnicodeString = (
    'abs', 'acos', 'all', 'allinvocations', 'allinvocationsequal', 'any',
    'anyinvocation', 'asin', 'atan', 'atomicadd', 'atomicand', 'atomiccompswap',
    'atomiccounter', 'atomiccounteradd', 'atomiccounterand',
    'atomiccountercompswap', 'atomiccounterdecrement', 'atomiccounterexchange',
    'atomiccounterincrement', 'atomiccountermax', 'atomiccountermin',
    'atomiccounteror', 'atomiccountersubtract', 'atomiccounterxor',
    'atomicexchange', 'atomicmax', 'atomicmin', 'atomicor', 'atomicxor',
    'barrier', 'bitcount', 'bitfieldextract', 'bitfieldinsert',
    'bitfieldreverse', 'ceil', 'clamp', 'cos', 'cross', 'degrees',
    'determinant', 'dfdx', 'dfdxcoarse', 'dfdxfine', 'dfdy', 'dfdycoarse',
    'dfdyfine', 'distance', 'dot', 'emitstreamvertex', 'emitvertex',
    'endprimitive', 'endstreamprimitive', 'equal', 'exp', 'exp2', 'faceforward',
    'findlsb', 'findmsb', 'floor', 'fract', 'ftransform', 'fwidth',
    'fwidthcoarse', 'fwidthfine', 'greaterthan', 'greaterthanequal',
    'groupmemorybarrier', 'imageatomicadd', 'imageatomicand',
    'imageatomiccompswap', 'imageatomicexchange', 'imageatomicmax',
    'imageatomicmin', 'imageatomicor', 'imageatomicxor', 'imageload',
    'imagesamples', 'imagesize', 'imagestore', 'imulextended',
    'interpolateatcentroid', 'interpolateatoffset', 'interpolateatsample',
    'inverse', 'inversesqrt', 'length', 'lessthan', 'lessthanequal', 'log',
    'log2', 'matrixcompmult', 'max', 'memorybarrier',
    'memorybarrieratomiccounter', 'memorybarrierbuffer', 'memorybarrierimage',
    'memorybarriershared', 'min', 'mix', 'mod', 'noise1', 'noise2', 'noise3',
    'noise4', 'normalize', 'not', 'notequal', 'outerproduct', 'pow', 'radians',
    'reflect', 'refract', 'shadow2d', 'shadow2dproj', 'shadowld',
    'shadowldproj', 'sign', 'sin', 'smoothstep', 'sqrt', 'step', 'subpassload',
    'tan', 'texelfetch', 'texelfetchoffset', 'texture', 'texture1d',
    'texture1dlod', 'texture1dproj', 'texture1dprojlod', 'texture2d',
    'texture2dlod', 'texture2dproj', 'texture2dprojlod', 'texture3d',
    'texture3dlod', 'texture3dproj', 'texture3dprojlod', 'texturecube',
    'texturecubelod', 'texturegather', 'texturegatheroffset',
    'texturegatheroffsets', 'texturegrad', 'texturegradoffset', 'texturelod',
    'texturelodoffset', 'textureoffset', 'textureproj', 'textureprojgrad',
    'textureprojgradoffset', 'textureprojlod', 'textureprojlodoffset',
    'textureprojoffset', 'texturequerylevels', 'texturequerylod', 'texturesize',
    'transpose', 'uaddcarry', 'umulextended', 'usubborrow'
  );

  GKeyIndices: array[0..2010] of Integer = (
    -1, -1, 22, -1, -1, 65, -1, -1, -1, -1, -1, 129, 8, -1, 177, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 226, 23, -1, -1, -1, 182, -1, 33, 98, -1, -1, 9, -1, -1,
    -1, 200, -1, -1, -1, -1, -1, -1, -1, -1, 24, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 108, 72, -1, -1, -1, -1, -1, -1, -1, -1, 163, 69, -1, 195, -1,
    -1, 85, -1, -1, -1, -1, -1, -1, 79, 186, -1, 71, -1, -1, -1, -1, 68, -1, -1,
    2, -1, 146, 96, -1, -1, -1, 76, -1, -1, -1, -1, 215, 219, -1, 30, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 204, 51,
    -1, 31, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 6, 206, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 32, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 239,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 95, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 180, -1, -1,
    -1, 172, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 211, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 61, 145, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 149, -1, -1, -1, -1, 114, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 156, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 140, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 81, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 197, -1, -1, -1, -1, -1, 192, -1, -1, -1, -1, -1, -1, -1, 1, 228, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 67, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 221, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 75, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 3, -1, -1,
    -1, -1, 99, -1, -1, -1, -1, -1, 125, 135, -1, -1, -1, -1, -1, -1, -1, 101,
    -1, -1, -1, -1, -1, -1, -1, -1, 187, -1, 0, -1, -1, 126, 106, -1, -1, -1,
    -1, -1, -1, -1, -1, 63, -1, -1, -1, -1, -1, 91, -1, -1, -1, -1, -1, -1, 127,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 167, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 117, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 113, -1, -1, -1,
    -1, 118, -1, 13, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 150, -1, -1, -1, 119, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 115,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 232, -1, 130, -1, 165,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 12, 46, -1, -1, -1, -1, -1, -1, -1, 155, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 222, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 86, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 88, -1, -1, 171, -1, 15, -1,
    -1, -1, -1, -1, -1, 77, -1, -1, 93, -1, -1, -1, 217, -1, -1, -1, -1, -1, 41,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 223, -1, -1, -1, -1, -1, -1, -1, -1, -1, 199, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 176, -1, -1, -1, -1, -1, 208, -1, -1,
    -1, 105, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 159, -1,
    -1, -1, -1, -1, 80, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 64, -1, -1, -1, -1, 168, -1, -1, -1, -1, -1, 40, -1, -1, -1, -1, -1, -1,
    -1, -1, 16, -1, 83, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 92, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 87, -1, 236, 166, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 38, -1, -1, -1,
    -1, -1, -1, -1, 131, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 160, -1, -1, -1, -1, -1, -1, -1, -1, -1, 142, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 196, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 110, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 111, -1, -1,
    -1, 179, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    138, 112, -1, -1, -1, -1, -1, -1, -1, -1, 26, -1, -1, -1, -1, -1, -1, -1,
    109, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 27, -1, -1,
    134, -1, -1, -1, -1, 107, -1, -1, 178, -1, -1, -1, -1, 147, -1, -1, -1, -1,
    -1, 136, 28, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    229, -1, 173, -1, -1, -1, -1, 139, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 230, 54, 66, -1, 233, -1, 14, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 152, -1, -1, -1, 18, -1, -1, 231, 55, -1, -1, 234, -1, -1, -1,
    -1, -1, -1, -1, -1, 94, -1, 194, -1, -1, -1, -1, -1, -1, -1, -1, 56, -1, -1,
    235, -1, 116, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 89, -1, -1, -1,
    -1, 44, -1, -1, -1, -1, -1, 120, -1, -1, 193, 20, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 153, -1, 124, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 39, -1, -1, -1, -1, -1, -1, -1, 148, -1, -1, -1, -1, -1, -1,
    207, -1, -1, -1, -1, -1, -1, -1, -1, -1, 227, 104, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 132, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 161,
    -1, -1, -1, -1, -1, -1, -1, -1, 73, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 53, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 19, -1, -1, 82, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 4, -1, -1, 213, 121, 214, -1, 157, 170, -1, -1, -1,
    -1, 181, -1, -1, -1, -1, 100, -1, -1, -1, -1, -1, -1, 48, -1, 122, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 49,
    -1, 123, -1, -1, -1, -1, -1, -1, 103, -1, -1, -1, -1, -1, 164, -1, -1, -1,
    78, -1, -1, -1, 50, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 17, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    144, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 10, -1, -1, -1, -1, -1, 162, 169, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 70, -1,
    -1, -1, -1, -1, -1, -1, -1, 90, -1, 43, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    205, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 184, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 141, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 47,
    -1, -1, -1, -1, 183, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 35, -1, -1, -1,
    -1, -1, -1, 102, -1, -1, -1, -1, -1, 189, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 36, -1, -1, -1, -1, 191, -1, -1, -1, -1, -1, 224, -1, -1, -1, -1, -1,
    -1, 203, -1, -1, -1, -1, 37, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 188, -1, -1, 34, -1, 62, -1, -1, -1, -1, -1, -1,
    -1, -1, 158, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 128, -1, 84, -1,
    42, 209, 185, -1, -1, -1, 45, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    137, -1, 198, -1, -1, -1, -1, -1, -1, -1, -1, 210, -1, -1, 151, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 216, -1,
    5, 174, -1, -1, -1, 143, -1, 52, 74, -1, -1, 201, -1, -1, -1, -1, -1, -1,
    -1, 237, -1, 190, -1, -1, -1, 202, -1, -1, -1, -1, -1, -1, -1, 11, 154, 218,
    -1, -1, -1, -1, -1, -1, -1, 57, -1, -1, -1, -1, -1, -1, 220, -1, 133, -1,
    -1, -1, -1, -1, -1, 97, -1, -1, -1, -1, -1, 225, -1, -1, -1, -1, 212, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 59, -1, -1, -1, 21, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 238, -1, -1, -1, -1, -1, -1, -1, 25,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 58, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 29, 7, 60, -1, -1, -1, -1, -1, -1, -1, -1, 175
  );

  GInternalFunctionIndices: array[0..1050] of Integer = (
    -1, -1, -1, -1, -1, -1, -1, 50, 63, -1, 34, -1, 5, -1, 118, -1, 17, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 11, -1, -1, -1, -1, -1, 134, -1, -1, -1, -1, 65,
    -1, -1, 19, -1, 141, -1, 137, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 48,
    -1, 40, 99, -1, -1, 73, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 43, -1,
    -1, -1, 24, -1, -1, -1, -1, -1, 71, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    145, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 106, 25, -1, -1,
    -1, 52, 109, -1, -1, -1, -1, -1, 35, -1, -1, -1, -1, -1, 159, -1, -1, -1,
    -1, -1, -1, -1, -1, 98, 90, 120, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 64,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 104, -1, 87, -1, -1, -1, -1, 42, -1, -1,
    -1, 13, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 14, -1, -1, -1, -1, -1, -1, -1, -1, -1, 155, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 67, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 45, -1, -1, 68, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 108, 39, 160, 9, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 138, -1, -1, -1, -1, -1, -1, -1, 10, -1,
    -1, -1, -1, -1, -1, 56, -1, -1, -1, -1, -1, -1, -1, 51, -1, -1, -1, -1, 31,
    -1, -1, -1, -1, -1, -1, -1, 100, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 101, -1, 130, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 102, -1, -1, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 103,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 128, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 147, -1, -1, -1, -1, 127,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 143, -1, -1, -1, 2, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 119, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 133, -1, -1, -1, 29, -1, 96,
    -1, -1, -1, 53, -1, -1, -1, -1, -1, 131, -1, -1, -1, -1, -1, 12, 88, -1, -1,
    -1, -1, 41, -1, -1, -1, -1, -1, -1, -1, -1, -1, 111, 125, -1, -1, -1, 150,
    -1, -1, -1, -1, -1, -1, 60, -1, -1, -1, -1, -1, 0, -1, -1, -1, -1, -1, 116,
    -1, -1, 139, 20, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 92, 66, -1,
    -1, 135, -1, -1, -1, -1, 82, 115, 16, -1, -1, 91, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 89, -1, 124, -1, 72, -1, -1, -1, -1, -1, -1, 77, 123,
    47, -1, -1, 151, -1, -1, -1, 37, -1, -1, -1, -1, -1, -1, -1, -1, -1, 33, -1,
    -1, -1, -1, -1, -1, 136, 27, -1, 44, -1, -1, -1, -1, -1, -1, 85, -1, 26, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 7, -1, 76, -1, -1, -1, -1, -1, -1, -1,
    55, -1, 81, -1, -1, -1, -1, -1, 46, -1, -1, -1, 8, 61, -1, -1, -1, -1, -1,
    -1, 126, -1, -1, 80, -1, -1, -1, -1, 22, -1, -1, -1, 78, -1, -1, -1, 54, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 114, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 154, -1, 149, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 59, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 58,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 110, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 94, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 3, -1, -1, -1, -1, -1, -1, -1,
    -1, 157, -1, -1, -1, -1, -1, -1, -1, -1, 148, -1, -1, -1, -1, -1, -1, 121,
    -1, -1, -1, -1, 140, -1, -1, -1, -1, -1, 156, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 95, -1, -1, -1, -1, -1, 142, -1, -1, -1, -1, -1, -1, 23, -1, -1, -1,
    -1, -1, -1, -1, 62, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 117, -1, -1, -1, -1, -1, -1, -1, -1, 93, -1, -1, 38, 153, -1,
    -1, -1, 74, -1, -1, -1, -1, 122, 75, -1, -1, -1, -1, -1, -1, -1, -1, 49, -1,
    -1, -1, -1, 18, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 28, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 84, -1, 36, 158, -1, -1, -1, -1, -1, -1, -1, 86, -1, 4, -1,
    113, -1, -1, -1, -1, -1, -1, 152, -1, -1, -1, -1, -1, -1, -1, -1, -1, 129,
    -1, 57, 112, -1, -1, -1, 83, -1, -1, 105, -1, -1, 32, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 21, -1, 70, -1, -1, 144, -1,
    -1, -1, -1, 15, -1, -1, -1, -1, -1, -1, 132, -1, 107, -1, -1, -1, -1, -1,
    -1, -1, 30, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 97, -1, -1, 146, 79,
    69
  );

{$Q-}
function TSynGLSLSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 875 + Ord(Str^) * 23;
    inc(Str);
  end;
  Result := Result mod 2011;
  FStringLen := Str - FToIdent;
end;

function TSynGLSLSyn.HashInternal(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 703 + Ord(Str^) * 16;
    inc(Str);
  end;
  Result := Result mod 1051;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynGLSLSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Key: Cardinal;
begin
  FToIdent := MayBe;
  Key := HashKey(MayBe);
  if Key <= High(FIdentFuncTable) then
    Result := FIdentFuncTable[Key](GKeyIndices[Key], Maybe)
  else
  begin
    Key := HashInternal(MayBe);
    if Key <= High(FInternalFuncTable) then
      Result := FInternalFuncTable[Key](GInternalFunctionIndices[Key], Maybe)
    else
      Result := tkIdentifier;
  end;
end;

procedure TSynGLSLSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if GKeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  for i := Low(FInternalFuncTable) to High(FInternalFuncTable) do
    if GInternalFunctionIndices[i] = -1 then
      FInternalFuncTable[i] := AltFinalFunc;

  FIdentFuncTable[83] := InterfaceQualifierFunc;
  FIdentFuncTable[143] := InterfaceQualifierFunc;
  FIdentFuncTable[460] := InterfaceQualifierFunc;
  FIdentFuncTable[1312] := InterfaceQualifierFunc;
  FIdentFuncTable[1878] := InterfaceQualifierFunc;

  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if @FIdentFuncTable[i] = nil then
      FIdentFuncTable[i] := KeyWordFunc;

  for i := Low(FInternalFuncTable) to High(FInternalFuncTable) do
    if @FInternalFuncTable[i] = nil then
      FInternalFuncTable[i] := InternalFunc;
end;

function TSynGLSLSyn.InterfaceQualifierFunc(Index: Integer; Maybe: PWideChar): TtkTokenKind;
begin
  if IsCurrentToken(GKeyWords[Index]) then
    Result := tkInterfaceQualifier
  else
    Result := tkIdentifier
end;

function TSynGLSLSyn.AltFinalFunc(Index: Integer;
  Maybe: PWideChar): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynGLSLSyn.AltFunc(Index: Integer; Maybe: PWideChar): TtkTokenKind;
var
  Key: Cardinal;
begin
  Key := HashInternal(MayBe);
  if Key <= High(FInternalFuncTable) then
    Result := FInternalFuncTable[Key](GInternalFunctionIndices[Key], Maybe)
  else
    Result := tkIdentifier;
end;

function TSynGLSLSyn.KeyWordFunc(Index: Integer; Maybe: PWideChar): TtkTokenKind;
begin
  if IsCurrentToken(GKeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

function TSynGLSLSyn.InternalFunc(Index: Integer; Maybe: PWideChar): TtkTokenKind;
begin
  if IsCurrentToken(GInternalFunctions[Index]) then
    Result := tkInternalFunction
  else
    Result := tkIdentifier
end;

constructor TSynGLSLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style:= [fsItalic];
  FCommentAttri.Foreground := clGray;
  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  FInterfaceQualifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrInterfaceQualifier, SYNS_FriendlyAttrInterfaceQualifier);
  FInterfaceQualifierAttri.Style:= [fsBold];
  FInternalFunctionsAttri := TSynHighlighterAttributes.Create(SYNS_AttrInternalFunction, SYNS_FriendlyAttrInternalFunction);
  FInternalFunctionsAttri.Foreground := clGreen;
  FInvalidAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Foreground := clNavy;
  FKeyAttri.Style:= [fsBold];
  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clMaroon;
  FCharAttri := TSynHighlighterAttributes.Create(SYNS_AttrCharacter, SYNS_FriendlyAttrCharacter);
  FFloatAttri := TSynHighlighterAttributes.Create(SYNS_AttrFloat, SYNS_FriendlyAttrFloat);
  FFloatAttri.Foreground := clOlive;
  FHexAttri := TSynHighlighterAttributes.Create(SYNS_AttrHexadecimal, SYNS_FriendlyAttrHexadecimal);
  FOctalAttri := TSynHighlighterAttributes.Create(SYNS_AttrOctal, SYNS_FriendlyAttrOctal);
  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  FDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);

  AddAttribute(FCommentAttri);
  AddAttribute(FIdentifierAttri);
  AddAttribute(FInterfaceQualifierAttri);
  AddAttribute(FInternalFunctionsAttri);
  AddAttribute(FInvalidAttri);
  AddAttribute(FKeyAttri);
  AddAttribute(FNumberAttri);
  AddAttribute(FCharAttri);
  AddAttribute(FFloatAttri);
  AddAttribute(FHexAttri);
  AddAttribute(FOctalAttri);
  AddAttribute(FSpaceAttri);
  AddAttribute(FStringAttri);
  AddAttribute(FDirecAttri);
  AddAttribute(FSymbolAttri);

  SetAttributesOnChange(DefHighlightChange);

  InitIdent;
  FRange := rsUnknown;
  FAsmStart := False;
  FDefaultFilter := SYNS_FilterGLSL;
end;

procedure TSynGLSLSyn.AnsiCProc;
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
          if (FRange = rsDirectiveComment) and
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

procedure TSynGLSLSyn.AndSymbolProc;
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

procedure TSynGLSLSyn.AsciiCharProc;
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

procedure TSynGLSLSyn.AtSymbolProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

procedure TSynGLSLSyn.BraceCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkBraceClose;
end;

procedure TSynGLSLSyn.BraceOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkBraceOpen;
end;

procedure TSynGLSLSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run + 1] = #10 then Inc(Run);
end;

procedure TSynGLSLSyn.ColonProc;
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

procedure TSynGLSLSyn.CommaProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkComma;
end;

procedure TSynGLSLSyn.DirectiveProc;
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

procedure TSynGLSLSyn.DirectiveEndProc;
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

procedure TSynGLSLSyn.EqualProc;
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

procedure TSynGLSLSyn.GreaterProc;
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

procedure TSynGLSLSyn.QuestionProc;
begin
  FTokenID := tkSymbol;                {conditional}
  FExtTokenID := xtkQuestion;
  Inc(Run);
end;

procedure TSynGLSLSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + Run));
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do Inc(Run);
end;

procedure TSynGLSLSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynGLSLSyn.LowerProc;
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

procedure TSynGLSLSyn.MinusProc;
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

procedure TSynGLSLSyn.ModSymbolProc;
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

procedure TSynGLSLSyn.NotSymbolProc;
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

procedure TSynGLSLSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynGLSLSyn.NumberProc;

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

procedure TSynGLSLSyn.OrSymbolProc;
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

procedure TSynGLSLSyn.PlusProc;
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

procedure TSynGLSLSyn.PointProc;
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

procedure TSynGLSLSyn.RoundCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundClose;
end;

procedure TSynGLSLSyn.RoundOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundOpen;
end;

procedure TSynGLSLSyn.SemiColonProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSemiColon;
end;

procedure TSynGLSLSyn.SlashProc;
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
        while FLine[Run] <> #0 do
          case FLine[Run] of
            '*':
              if FLine[Run + 1] = '/' then
              begin
                Inc(Run, 2);
                if FRange = rsDirectiveComment then
                  FRange := rsMultiLineDirective
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

procedure TSynGLSLSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynGLSLSyn.SquareCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSquareClose;
end;

procedure TSynGLSLSyn.SquareOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSquareOpen;
end;

procedure TSynGLSLSyn.StarProc;
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

procedure TSynGLSLSyn.StringProc;
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

procedure TSynGLSLSyn.StringEndProc;
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

procedure TSynGLSLSyn.TildeProc;
begin
  Inc(Run);                            {bitwise complement}
  FTokenID := tkSymbol;
  FExtTokenID := xtkBitComplement;
end;

procedure TSynGLSLSyn.XOrSymbolProc;
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

procedure TSynGLSLSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynGLSLSyn.Next;
begin
  FAsmStart := False;
  FTokenPos := Run;
  case FRange of
    rsAnsiC, rsDirectiveComment: AnsiCProc;
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

function TSynGLSLSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynGLSLSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynGLSLSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynGLSLSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynGLSLSyn.GetExtTokenID: TxtkTokenKind;
begin
  Result := FExtTokenID;
end;

function TSynGLSLSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  FTokenID := GetTokenID;
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkDirective: Result := FDirecAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkInterfaceQualifier: Result := FInterfaceQualifierAttri;
    tkInternalFunction: Result := FInternalFunctionsAttri;
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

function TSynGLSLSyn.GetTokenKind: Integer;
begin
  Result := Ord(GetTokenID);
end;

procedure TSynGLSLSyn.ResetRange;
begin
  FRange:= rsUnknown;
end;

procedure TSynGLSLSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynGLSLSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterGLSL;
end;

class function TSynGLSLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangGLSL;
end;

function TSynGLSLSyn.GetSampleSource: UnicodeString;
begin
  Result :=
    'const int foo = 5;'#13#10 +
    'uniform vec2 bar;'#13#10 +
    'uniform sampler2d tex;'#13#10#13#10 +
    'float rand(vec2 co){'#13#10 +
    'return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);'#13#10 +
    '}'#13#10#13#10 +
    'void main()'#13#10 +
    '{'#13#10 +
    '  uint value = 21; // comment'#13#10 +
    '  float number = abs(bar.x);'#13#10 +
    '  if (foo < sin(3.14 * value))'#13#10 +
    '    value = cos(12 * value);'#13#10 +
    '  vec4 colors = texture(tex, bar.xy);'#13#10 +
    '  for(int i = bar.x; i < bar.y; ++i)'#13#10 +
    '  {'#13#10 +
    '  }'#13#10 +
    '}';
end;

class function TSynGLSLSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangGLSL;
end;

initialization
{$IFNDEF SYN_GLSLB_1}
  RegisterPlaceableHighlighter(TSynGLSLSyn);
{$ENDIF}
end.
