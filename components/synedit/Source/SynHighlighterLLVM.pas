{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: SynHighlighterLLVM.pas, released 2013-03-30.
Description: Syntax Parser/Highlighter
The initial author of this file is Christian-W. Budde.
Copyright (c) 2013, all rights reserved.

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

$Id: $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

unit SynHighlighterLLVM;

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
  TtkTokenKind = (
    tkBoolean,
    tkComment,
    tkConstant,
    tkFloat,
    tkHex,
    tkIdentifier,
    tkInstruction,
    tkKey,
    tkLabel,
    tkNumber,
    tkNull,
    tkSpace,
    tkString,
    tkSymbol,
    tkType,
    tkUnnamedIdentifier,
    tkUnknown);

  TRangeState = (rsUnKnown, rsSingleComment, rsString);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynLLVMIRSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..1552] of TIdentFuncTableFunc;
    fBooleanAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fConstantAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fInstructionAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fLabelAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fTypesAttri: TSynHighlighterAttributes;
    function HashKey(Str: PWideChar): Cardinal;
    function FuncBoolean(Index: Integer): TtkTokenKind;
    function FuncConstant(Index: Integer): TtkTokenKind;
    function FuncInstruction(Index: Integer): TtkTokenKind;
    function FuncKey(Index: Integer): TtkTokenKind;
    function FuncType(Index: Integer): TtkTokenKind;
    procedure IdentProc;
    procedure UnknownProc;
    function AltFunc(Index: Integer): TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure IntegerTypeProc;
    procedure SingleCommentOpenProc;
    procedure SingleCommentProc;
    procedure StringOpenProc;
    procedure StringProc;
    procedure AtTypeProc;
    procedure PercentTypeProc;
    procedure NumberProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetFriendlyLanguageName: UnicodeString; override;
    class function GetLanguageName: string; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetKeyWords(TokenKind: Integer): UnicodeString; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property BooleanAttribute: TSynHighlighterAttributes read fBooleanAttri write fBooleanAttri;
    property CommentAttribute: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property ConstantAttribute: TSynHighlighterAttributes read fConstantAttri write fConstantAttri;
    property IdentifierAttribute: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property InstructionAttribute: TSynHighlighterAttributes read fInstructionAttri write fInstructionAttri;
    property KeywordAttribute: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property LabelAttribute: TSynHighlighterAttributes read fLabelAttri write fLabelAttri;
    property NumberAttribute: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property SpaceAttribute: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttribute: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property TypesAttribute: TSynHighlighterAttributes read fTypesAttri write fTypesAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  // as this language is case-insensitive keywords *must* be in lowercase
  KeyWords: array[0..216] of UnicodeString = (
    'acq_rel', 'acquire', 'add', 'addrspace', 'alias', 'align', 'alignstack',
    'alloca', 'alwaysinline', 'and', 'appending', 'arcp', 'arm_aapcs_vfpcc', 
    'arm_aapcscc', 'arm_apcscc', 'ashr', 'asm', 'atomic', 'atomicrmw',
    'available_externally', 'bitcast', 'blockaddress', 'br', 'byval', 'c', 
    'call', 'catch', 'cc', 'ccc', 'cleanup', 'cmpxchg', 'coldcc', 'common', 
    'constant', 'datalayout', 'declare', 'default', 'define', 'deplibs', 
    'dllexport', 'dllimport', 'double', 'eq', 'exact', 'except', 'extern_weak',
    'external', 'extractelement', 'extractvalue', 'fadd', 'false', 'fast',
    'fastcc', 'fcmp', 'fdiv', 'fence', 'filter', 'float', 'fmul', 'fp128',
    'fpext', 'fptosi', 'fptoui', 'fptrunc', 'free', 'frem', 'fsub', 'gc',
    'getelementptr', 'global', 'half', 'hidden', 'icmp', 'inbounds',
    'indirectbr', 'initialexec', 'inlinehint', 'inreg', 'insertelement', 
    'insertvalue', 'intel_ocl_bicc', 'inteldialect', 'internal', 'inttoptr', 
    'invoke', 'label', 'landingpad', 'linker_private', 'linker_private_weak', 
    'linker_private_weak_def_auto', 'linkonce', 'linkonce_odr', 
    'linkonce_odr_auto_hide', 'load', 'localdynamic', 'localexec', 'lshr',
    'malloc', 'max', 'metadata', 'min', 'minsize', 'module', 'monotonic', 
    'msp430_intrcc', 'mul', 'naked', 'nand', 'ne', 'nest', 'ninf', 'nnan', 
    'noalias', 'nocapture', 'noimplicitfloat', 'noinline', 'nonlazybind', 
    'noredzone', 'noreturn', 'nounwind', 'nsw', 'nsz', 'null', 'nuw', 'oeq', 
    'oge', 'ogt', 'ole', 'olt', 'one', 'opaque', 'optsize', 'or', 'ord', 
    'personality', 'phi', 'ppc_fp128', 'private', 'protected', 'ptrtoint',
    'ptx_device', 'ptx_kernel', 'readnone', 'readonly', 'release', 'resume', 
    'ret', 'returns_twice', 'sanitize_address', 'sanitize_memory', 
    'sanitize_thread', 'sdiv', 'section', 'select', 'seq_cst', 'sext', 'sge',
    'sgt', 'shl', 'shufflevector', 'sideeffect', 'signext', 'singlethread', 
    'sitofp', 'sle', 'slt', 'spir_func', 'spir_kernel', 'srem', 'sret', 'ssp', 
    'sspreq', 'sspstrong', 'store', 'sub', 'switch', 'tail', 'target', 
    'thread_local', 'to', 'triple', 'true', 'trunc', 'type', 'udiv', 'ueq', 
    'uge', 'ugt', 'uitofp', 'ule', 'ult', 'umax', 'umin', 'undef', 'une', 
    'unnamed_addr', 'uno', 'unordered', 'unreachable', 'unwind', 'urem', 
    'uwtable', 'va_arg', 'void', 'volatile', 'weak', 'weak_odr', 
    'x86_fastcallcc', 'x86_fp80', 'x86_mmx', 'x86_stdcallcc', 'x86_thiscallcc', 
    'xchg', 'xor', 'zeroext', 'zeroinitializer', 'zext'
  );

  KeyIndices: array[0..1552] of Integer = (
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 124, -1, -1, 
    64, 40, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 11, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 9, -1, -1, -1, -1, -1, 183, -1, -1, -1, 168, -1, -1, 
    79, -1, -1, -1, -1, 186, -1, -1, -1, -1, -1, 209, 37, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 166, -1, -1, -1, -1, -1, -1, -1, 
    211, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 8, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 100, 62, -1, 
    -1, -1, -1, -1, -1, 91, -1, -1, -1, -1, -1, -1, -1, 33, -1, -1, -1, -1, -1, 
    -1, 182, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 169, -1, -1, -1, -1, 26, 78, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 116, 143, 93, -1, -1, -1, -1, -1, 165, -1, -1, 
    132, -1, -1, -1, -1, 195, -1, -1, -1, -1, 41, -1, -1, -1, -1, -1, 173, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 36, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 119, -1, -1, 146, -1, -1, -1, -1, -1, -1, 205, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 120, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 151, -1, -1, -1, -1, -1, -1, -1, -1, 85, -1, -1, -1, -1, 
    207, -1, -1, -1, 111, -1, -1, -1, -1, -1, -1, 128, -1, -1, -1, -1, 106, -1, 
    -1, 23, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 89, -1, -1, 161, -1, -1, -1, 
    -1, -1, -1, -1, 17, -1, -1, -1, -1, -1, -1, 24, -1, -1, -1, -1, -1, 10, -1, 
    133, -1, -1, 122, 65, -1, -1, 53, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 170, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 197, 144, -1, 
    -1, -1, -1, -1, -1, 57, -1, -1, -1, -1, 189, -1, -1, -1, -1, -1, -1, -1, 
    159, -1, -1, -1, -1, -1, -1, -1, -1, 59, -1, 35, -1, -1, 131, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 99, -1, -1, -1, -1, -1, 147, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 77, -1, 196, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 141, -1, -1, -1, -1, -1, 188, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 202, -1, -1, -1, -1, -1, 32, -1, -1, -1, 
    -1, 187, -1, -1, -1, -1, -1, -1, -1, -1, -1, 191, -1, -1, -1, -1, -1, 18, 
    -1, -1, -1, -1, -1, 74, -1, -1, -1, -1, -1, -1, -1, -1, 81, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 39, -1, -1, -1, -1, -1, -1, 
    -1, -1, 25, -1, -1, -1, -1, -1, 199, 185, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 55, 129, -1, 12, -1, -1, -1, 54, -1, 215, -1, 
    -1, -1, -1, -1, -1, -1, 115, -1, -1, -1, -1, -1, -1, -1, 109, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 94, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 145, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 75, -1, -1, -1, 138, -1, -1, 160, -1, -1, -1, -1, -1, -1, -1, 34, -1, 
    -1, -1, -1, -1, 162, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 180, -1, -1, -1, -1, -1, -1, -1, -1, 153, -1, -1, -1, -1, -1, -1, -1, 
    203, 88, -1, -1, -1, 42, -1, 50, -1, -1, 45, 80, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 137, -1, -1, 73, 167, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 130, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 82, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 2, -1, -1, 71, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    52, -1, -1, -1, -1, -1, -1, 90, -1, -1, -1, -1, -1, -1, 201, -1, -1, -1, -1, 
    -1, -1, -1, -1, 113, -1, -1, -1, -1, -1, -1, -1, -1, -1, 48, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 156, -1, -1, -1, -1, -1, -1, -1, -1, -1, 61, -1, 
    -1, 5, -1, 3, -1, 190, -1, -1, -1, -1, -1, -1, -1, 212, -1, -1, 174, -1, -1, 
    28, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 178, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 83, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 87, -1, -1, -1, -1, -1, -1, 98, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 193, -1, -1, -1, 21, -1, -1, 121, -1, -1, 214, -1, 
    84, 70, -1, -1, 47, -1, -1, -1, -1, -1, 38, -1, 16, -1, -1, -1, -1, -1, -1, 
    125, -1, -1, -1, -1, -1, -1, 134, 181, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 154, 123, -1, -1, -1, -1, 
    -1, 216, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    206, -1, -1, -1, -1, -1, -1, -1, -1, 49, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 104, -1, -1, -1, -1, -1, -1, -1, 31, -1, -1, -1, 30, 213, -1, 
    -1, -1, -1, -1, 46, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 118, -1, -1, -1, -1, -1, -1, -1, -1, 68, -1, -1, 136, -1, -1, -1, -1, 
    -1, -1, -1, 6, 102, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 135, -1, 
    -1, -1, 66, 105, -1, -1, 198, -1, -1, -1, -1, -1, -1, -1, -1, 172, -1, 19,
    -1, -1, 114, -1, -1, -1, -1, -1, -1, 175, -1, -1, -1, -1, -1, -1, -1, -1, 
    117, 194, -1, -1, 72, -1, -1, -1, -1, -1, 152, -1, -1, -1, -1, -1, -1, -1, 
    -1, 107, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 29, -1, -1, 15, -1, 
    171, -1, -1, 192, -1, 200, -1, -1, 148, -1, -1, 86, 76, 63, -1, 14, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 164, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 67, -1, -1, 
    -1, -1, -1, -1, -1, -1, 27, -1, -1, -1, -1, -1, 155, 184, -1, 97, -1, -1, 
    -1, -1, 149, -1, 176, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 4, -1, -1, -1, -1, 163, -1, -1, -1, -1, -1,
    210, -1, 44, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 108, -1, 
    -1, -1, -1, -1, -1, 157, -1, -1, 142, -1, 7, 51, -1, 177, -1, -1, -1, -1, 
    69, -1, -1, -1, -1, -1, -1, -1, 22, -1, 127, 204, -1, -1, 158, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 140, -1, 179, -1, -1, -1, 58, -1, -1, 208, 139, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 92, 43, -1, -1, 110, 0, -1, -1, -1, 96, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 20, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 112, 
    126, 95, -1, -1, -1, -1, -1, 13, -1, -1, -1, -1, -1, -1, 150, -1, -1, -1, 
    -1, -1, 56, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 103, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 101, -1, 60 
  );

constructor TSynLLVMIRSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCaseSensitive := True;

  fBooleanAttri := TSynHighLighterAttributes.Create(SYNS_AttrBoolean, SYNS_FriendlyAttrBoolean);
  fBooleanAttri.Foreground := clNavy;
  AddAttribute(fBooleanAttri);

  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Foreground := $B0A060;
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);

  fConstantAttri := TSynHighLighterAttributes.Create(SYNS_AttrConstant, SYNS_FriendlyAttrConstant);
  fConstantAttri.Foreground := clNavy;
  AddAttribute(fConstantAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  fIdentifierAttri.Foreground := $D560BB;
  AddAttribute(fIdentifierAttri);

  fInstructionAttri := TSynHighLighterAttributes.Create(SYNS_AttrInstructions, SYNS_FriendlyAttrInstructions);
  fInstructionAttri.Foreground := $207000;
  fInstructionAttri.Style := [fsBold];
  AddAttribute(fInstructionAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Foreground := $207000;
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);

  fLabelAttri := TSynHighLighterAttributes.Create(SYNS_AttrLabel, SYNS_FriendlyAttrLAbel);
  fLabelAttri.Foreground := $702000;
  fLabelAttri.Style := [fsBold];
  AddAttribute(fLabelAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  fNumberAttri.Foreground := $70A040;
  AddAttribute(fNumberAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Foreground := $A07040;
  AddAttribute(fStringAttri);

  fTypesAttri := TSynHighLighterAttributes.Create(SYNS_AttrBasicTypes, SYNS_FriendlyAttrBasicTypes);
  fTypesAttri.Foreground := $002090;
  fTypesAttri.Style := [fsBold];
  AddAttribute(fTypesAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterLLVMIR;
  fRange := rsUnknown;
end;

procedure TSynLLVMIRSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[1458] := FuncKey; // acq95rel
  fIdentFuncTable[659] := FuncKey; // acquire
  fIdentFuncTable[843] := FuncInstruction; // add
  fIdentFuncTable[916] := FuncKey; // addrspace
  fIdentFuncTable[1346] := FuncKey; // alias
  fIdentFuncTable[914] := FuncKey; // align
  fIdentFuncTable[1158] := FuncKey; // alignstack
  fIdentFuncTable[1385] := FuncInstruction; // alloca
  fIdentFuncTable[107] := FuncKey; // alwaysinline
  fIdentFuncTable[44] := FuncInstruction; // and
  fIdentFuncTable[372] := FuncKey; // appending
  fIdentFuncTable[32] := FuncInstruction; // arcp
  fIdentFuncTable[624] := FuncKey; // arm95aapcs95vfpcc
  fIdentFuncTable[1512] := FuncKey; // arm95aapcscc
  fIdentFuncTable[1261] := FuncKey; // arm95apcscc
  fIdentFuncTable[1244] := FuncInstruction; // ashr
  fIdentFuncTable[1019] := FuncKey; // asm
  fIdentFuncTable[359] := FuncKey; // atomic
  fIdentFuncTable[557] := FuncInstruction; // atomicrmw
  fIdentFuncTable[1191] := FuncKey; // available95externally
  fIdentFuncTable[1489] := FuncInstruction; // bitcast
  fIdentFuncTable[999] := FuncKey; // blockaddress
  fIdentFuncTable[1401] := FuncInstruction; // br
  fIdentFuncTable[337] := FuncKey; // byval
  fIdentFuncTable[366] := FuncKey; // c
  fIdentFuncTable[598] := FuncInstruction; // call
  fIdentFuncTable[179] := FuncKey; // catch
  fIdentFuncTable[1308] := FuncKey; // cc
  fIdentFuncTable[932] := FuncKey; // ccc
  fIdentFuncTable[1241] := FuncKey; // cleanup
  fIdentFuncTable[1115] := FuncInstruction; // cmpxchg
  fIdentFuncTable[1111] := FuncKey; // coldcc
  fIdentFuncTable[536] := FuncKey; // common
  fIdentFuncTable[145] := FuncKey; // constant
  fIdentFuncTable[723] := FuncKey; // datalayout
  fIdentFuncTable[458] := FuncKey; // declare
  fIdentFuncTable[238] := FuncKey; // default
  fIdentFuncTable[69] := FuncKey; // define
  fIdentFuncTable[1017] := FuncKey; // deplibs
  fIdentFuncTable[589] := FuncKey; // dllexport
  fIdentFuncTable[20] := FuncKey; // dllimport
  fIdentFuncTable[217] := FuncType; // double
  fIdentFuncTable[767] := FuncInstruction; // eq
  fIdentFuncTable[1454] := FuncInstruction; // exact
  fIdentFuncTable[1359] := FuncKey; // except
  fIdentFuncTable[772] := FuncKey; // extern95weak
  fIdentFuncTable[1122] := FuncKey; // external
  fIdentFuncTable[1011] := FuncInstruction; // extractelement
  fIdentFuncTable[889] := FuncInstruction; // extractvalue
  fIdentFuncTable[1090] := FuncInstruction; // fadd
  fIdentFuncTable[769] := FuncBoolean; // false
  fIdentFuncTable[1386] := FuncInstruction; // fast
  fIdentFuncTable[856] := FuncKey; // fastcc
  fIdentFuncTable[381] := FuncInstruction; // fcmp
  fIdentFuncTable[628] := FuncInstruction; // fdiv
  fIdentFuncTable[621] := FuncInstruction; // fence
  fIdentFuncTable[1525] := FuncKey; // filter
  fIdentFuncTable[434] := FuncType; // float
  fIdentFuncTable[1424] := FuncInstruction; // fmul
  fIdentFuncTable[456] := FuncType; // fp128
  fIdentFuncTable[1552] := FuncInstruction; // fpext
  fIdentFuncTable[911] := FuncInstruction; // fptosi
  fIdentFuncTable[130] := FuncInstruction; // fptoui
  fIdentFuncTable[1259] := FuncInstruction; // fptrunc
  fIdentFuncTable[19] := FuncInstruction; // free
  fIdentFuncTable[378] := FuncInstruction; // frem
  fIdentFuncTable[1176] := FuncInstruction; // fsub
  fIdentFuncTable[1299] := FuncKey; // gc
  fIdentFuncTable[1147] := FuncInstruction; // getelementptr
  fIdentFuncTable[1393] := FuncKey; // global
  fIdentFuncTable[1008] := FuncType; // half
  fIdentFuncTable[846] := FuncKey; // hidden
  fIdentFuncTable[1214] := FuncInstruction; // icmp
  fIdentFuncTable[789] := FuncInstruction; // inbounds
  fIdentFuncTable[563] := FuncInstruction; // indirectbr
  fIdentFuncTable[708] := FuncKey; // initialexec
  fIdentFuncTable[1258] := FuncKey; // inlinehint
  fIdentFuncTable[493] := FuncKey; // inreg
  fIdentFuncTable[180] := FuncInstruction; // insertelement
  fIdentFuncTable[57] := FuncInstruction; // insertvalue
  fIdentFuncTable[773] := FuncKey; // intel95ocl95bicc
  fIdentFuncTable[572] := FuncKey; // inteldialect
  fIdentFuncTable[828] := FuncKey; // internal
  fIdentFuncTable[965] := FuncInstruction; // inttoptr
  fIdentFuncTable[1007] := FuncInstruction; // invoke
  fIdentFuncTable[313] := FuncType; // label
  fIdentFuncTable[1257] := FuncInstruction; // landingpad
  fIdentFuncTable[977] := FuncKey; // linker95private
  fIdentFuncTable[763] := FuncKey; // linker95private95weak
  fIdentFuncTable[348] := FuncKey; // linker95private95weak95def95auto
  fIdentFuncTable[863] := FuncKey; // linkonce
  fIdentFuncTable[137] := FuncKey; // linkonce95odr
  fIdentFuncTable[1453] := FuncKey; // linkonce95odr95auto95hide
  fIdentFuncTable[198] := FuncInstruction; // load
  fIdentFuncTable[671] := FuncKey; // localdynamic
  fIdentFuncTable[1506] := FuncKey; // localexec
  fIdentFuncTable[1462] := FuncInstruction; // lshr
  fIdentFuncTable[1317] := FuncInstruction; // malloc
  fIdentFuncTable[984] := FuncInstruction; // max
  fIdentFuncTable[476] := FuncType; // metadata
  fIdentFuncTable[129] := FuncInstruction; // min
  fIdentFuncTable[1550] := FuncKey; // minsize
  fIdentFuncTable[1159] := FuncKey; // module
  fIdentFuncTable[1540] := FuncKey; // monotonic
  fIdentFuncTable[1103] := FuncKey; // msp43095intrcc
  fIdentFuncTable[1177] := FuncInstruction; // mul
  fIdentFuncTable[334] := FuncKey; // naked
  fIdentFuncTable[1229] := FuncInstruction; // nand
  fIdentFuncTable[1373] := FuncInstruction; // ne
  fIdentFuncTable[646] := FuncKey; // nest
  fIdentFuncTable[1457] := FuncInstruction; // ninf
  fIdentFuncTable[322] := FuncInstruction; // nnan
  fIdentFuncTable[1504] := FuncKey; // noalias
  fIdentFuncTable[879] := FuncKey; // nocapture
  fIdentFuncTable[1194] := FuncKey; // noimplicitfloat
  fIdentFuncTable[638] := FuncKey; // noinline
  fIdentFuncTable[196] := FuncKey; // nonlazybind
  fIdentFuncTable[1210] := FuncKey; // noredzone
  fIdentFuncTable[1138] := FuncKey; // noreturn
  fIdentFuncTable[248] := FuncKey; // nounwind
  fIdentFuncTable[285] := FuncInstruction; // nsw
  fIdentFuncTable[1002] := FuncInstruction; // nsz
  fIdentFuncTable[377] := FuncConstant; // null
  fIdentFuncTable[1057] := FuncInstruction; // nuw
  fIdentFuncTable[16] := FuncInstruction; // oeq
  fIdentFuncTable[1026] := FuncInstruction; // oge
  fIdentFuncTable[1505] := FuncInstruction; // ogt
  fIdentFuncTable[1403] := FuncInstruction; // ole
  fIdentFuncTable[329] := FuncInstruction; // olt
  fIdentFuncTable[622] := FuncInstruction; // one
  fIdentFuncTable[801] := FuncType; // opaque
  fIdentFuncTable[461] := FuncKey; // optsize
  fIdentFuncTable[207] := FuncInstruction; // or
  fIdentFuncTable[374] := FuncInstruction; // ord
  fIdentFuncTable[1033] := FuncKey; // personality
  fIdentFuncTable[1172] := FuncInstruction; // phi
  fIdentFuncTable[1150] := FuncType; // ppc95fp128
  fIdentFuncTable[786] := FuncKey; // private
  fIdentFuncTable[712] := FuncKey; // protected
  fIdentFuncTable[1428] := FuncInstruction; // ptrtoint
  fIdentFuncTable[1418] := FuncKey; // ptx95device
  fIdentFuncTable[507] := FuncKey; // ptx95kernel
  fIdentFuncTable[1383] := FuncKey; // readnone
  fIdentFuncTable[197] := FuncKey; // readonly
  fIdentFuncTable[427] := FuncKey; // release
  fIdentFuncTable[697] := FuncInstruction; // resume
  fIdentFuncTable[251] := FuncInstruction; // ret
  fIdentFuncTable[482] := FuncKey; // returns95twice
  fIdentFuncTable[1254] := FuncKey; // sanitize95address
  fIdentFuncTable[1322] := FuncKey; // sanitize95memory
  fIdentFuncTable[1519] := FuncKey; // sanitize95thread
  fIdentFuncTable[304] := FuncInstruction; // sdiv
  fIdentFuncTable[1220] := FuncKey; // section
  fIdentFuncTable[754] := FuncInstruction; // select
  fIdentFuncTable[1056] := FuncKey; // seq95cst
  fIdentFuncTable[1314] := FuncInstruction; // sext
  fIdentFuncTable[901] := FuncInstruction; // sge
  fIdentFuncTable[1380] := FuncInstruction; // sgt
  fIdentFuncTable[1407] := FuncInstruction; // shl
  fIdentFuncTable[447] := FuncInstruction; // shufflevector
  fIdentFuncTable[715] := FuncKey; // sideeffect
  fIdentFuncTable[351] := FuncKey; // signext
  fIdentFuncTable[729] := FuncKey; // singlethread
  fIdentFuncTable[1351] := FuncInstruction; // sitofp
  fIdentFuncTable[1278] := FuncInstruction; // sle
  fIdentFuncTable[204] := FuncInstruction; // slt
  fIdentFuncTable[86] := FuncInstruction; // spir95func
  fIdentFuncTable[790] := FuncInstruction; // spir95kernel
  fIdentFuncTable[54] := FuncInstruction; // srem
  fIdentFuncTable[174] := FuncKey; // sret
  fIdentFuncTable[397] := FuncKey; // ssp
  fIdentFuncTable[1246] := FuncKey; // sspreq
  fIdentFuncTable[1189] := FuncKey; // sspstrong
  fIdentFuncTable[223] := FuncInstruction; // store
  fIdentFuncTable[929] := FuncInstruction; // sub
  fIdentFuncTable[1201] := FuncInstruction; // switch
  fIdentFuncTable[1324] := FuncKey; // tail
  fIdentFuncTable[1388] := FuncKey; // target
  fIdentFuncTable[952] := FuncKey; // thread95local
  fIdentFuncTable[1420] := FuncKey; // to
  fIdentFuncTable[745] := FuncKey; // triple
  fIdentFuncTable[1034] := FuncBoolean; // true
  fIdentFuncTable[152] := FuncInstruction; // trunc
  fIdentFuncTable[50] := FuncInstruction; // type
  fIdentFuncTable[1315] := FuncInstruction; // udiv
  fIdentFuncTable[605] := FuncInstruction; // ueq
  fIdentFuncTable[62] := FuncInstruction; // uge
  fIdentFuncTable[541] := FuncInstruction; // ugt
  fIdentFuncTable[513] := FuncInstruction; // uitofp
  fIdentFuncTable[439] := FuncInstruction; // ule
  fIdentFuncTable[918] := FuncInstruction; // ult
  fIdentFuncTable[551] := FuncInstruction; // umax
  fIdentFuncTable[1249] := FuncInstruction; // umin
  fIdentFuncTable[995] := FuncConstant; // undef
  fIdentFuncTable[1211] := FuncInstruction; // une
  fIdentFuncTable[212] := FuncKey; // unnamed95addr
  fIdentFuncTable[495] := FuncInstruction; // uno
  fIdentFuncTable[426] := FuncKey; // unordered
  fIdentFuncTable[1180] := FuncInstruction; // unreachable
  fIdentFuncTable[604] := FuncInstruction; // unwind
  fIdentFuncTable[1251] := FuncInstruction; // urem
  fIdentFuncTable[870] := FuncKey; // uwtable
  fIdentFuncTable[530] := FuncInstruction; // va95arg
  fIdentFuncTable[762] := FuncType; // void
  fIdentFuncTable[1404] := FuncKey; // volatile
  fIdentFuncTable[258] := FuncKey; // weak
  fIdentFuncTable[1081] := FuncKey; // weak95odr
  fIdentFuncTable[318] := FuncKey; // x8695fastcallcc
  fIdentFuncTable[1427] := FuncType; // x8695fp80
  fIdentFuncTable[68] := FuncType; // x8695mmx
  fIdentFuncTable[1357] := FuncKey; // x8695stdcallcc
  fIdentFuncTable[94] := FuncKey; // x8695thiscallcc
  fIdentFuncTable[926] := FuncInstruction; // xchg
  fIdentFuncTable[1116] := FuncInstruction; // xor
  fIdentFuncTable[1005] := FuncKey; // zeroext
  fIdentFuncTable[630] := FuncConstant; // zeroinitializer
  fIdentFuncTable[1063] := FuncInstruction; // zext
end;

{$Q-}
function TSynLLVMIRSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 359 + Ord(Str^) * 239;
    Inc(Str);
  end;
  Result := Result mod 1553;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynLLVMIRSyn.FuncBoolean(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBoolean
  else
    Result := tkIdentifier;
end;

function TSynLLVMIRSyn.FuncConstant(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkConstant
  else
    Result := tkIdentifier;
end;

function TSynLLVMIRSyn.FuncInstruction(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkInstruction
  else
    Result := tkIdentifier;
end;

function TSynLLVMIRSyn.FuncKey(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynLLVMIRSyn.FuncType(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkType
  else
    Result := tkIdentifier;
end;

function TSynLLVMIRSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynLLVMIRSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynLLVMIRSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynLLVMIRSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynLLVMIRSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
    inc(Run);
end;

procedure TSynLLVMIRSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynLLVMIRSyn.IntegerTypeProc;
begin
  case FLine[Succ(Run)] of
    '0'..'9':
      begin
        fTokenID := tkType;
        repeat
          Inc(Run);
          case fLine[Run] of
            '0'..'9':;
          else
            Exit;
          end;
        until IsLineEnd(Run);
      end
  else
    IdentProc;
  end;
end;

procedure TSynLLVMIRSyn.AtTypeProc;
begin
  // @ = global identifiers
  fTokenID := tkUnknown;

  Inc(Run);
  if IsLineEnd(Run) then
    Exit;

  case fLine[Run] of
    '0'..'9': fTokenID := tkUnnamedIdentifier;
    '-', '_', 'A'..'Z', 'a'..'z': fTokenID := tkIdentifier;
    '"':
      begin
        Inc(Run);
        StringProc;
        fTokenID := tkIdentifier;
        Exit;
      end;
  end;

  repeat
    Inc(Run);
    case fLine[Run] of
      '0'..'9', '-', '_', '.', 'A'..'Z', 'a'..'z':;
    else
      Exit;
    end;
  until IsLineEnd(Run);
end;

procedure TSynLLVMIRSyn.PercentTypeProc;
begin
  // % = local identifiers
  fTokenID := tkUnknown;

  Inc(Run);
  if IsLineEnd(Run) then
    Exit;

  case fLine[Run] of
    '0'..'9': fTokenID := tkUnnamedIdentifier;
    '-', '_', '.', 'A'..'Z', 'a'..'z': fTokenID := tkIdentifier;
    '"':
      begin
        Inc(Run);
        StringProc;
        fTokenID := tkIdentifier;
        Exit;
      end;
  end;

  repeat
    Inc(Run);
    case fLine[Run] of
      '0'..'9', '-', '_', '.', 'A'..'Z', 'a'..'z':;
    else
      Exit;
    end;
  until IsLineEnd(Run);
end;

procedure TSynLLVMIRSyn.SingleCommentOpenProc;
begin
  Inc(Run);
  fRange := rsSingleComment;
  SingleCommentProc;
  fTokenID := tkComment;
end;

procedure TSynLLVMIRSyn.SingleCommentProc;
begin
  fTokenID := tkComment;
  while not IsLineEnd(Run) do
    Inc(Run);
end;

procedure TSynLLVMIRSyn.StringOpenProc;
begin
  Inc(Run);
  fRange := rsString;
  StringProc;
  fTokenID := tkString;
end;

procedure TSynLLVMIRSyn.StringProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      fTokenID := tkString;
      repeat
        if (fLine[Run] = '"') then
        begin
          Inc(Run, 1);
          fRange := rsUnKnown;
          Break;
        end;
        if not IsLineEnd(Run) then
          Inc(Run);
      until IsLineEnd(Run);
    end;
  end;
end;

procedure TSynLLVMIRSyn.NumberProc;

  function IsNumberChar(Run: Integer): Boolean;
  begin
    case fLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f', '.', 'x', 'X', '-', '+':
        Result := True;
      else
        Result := False;
    end;
  end;

  function IsDigitPlusMinusChar(Run: Integer): Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '+', '-':
        Result := True;
      else
        Result := False;
    end;
  end;

  function IsHexDigit(Run: Integer): Boolean;
  begin
    case fLine[Run] of
      '0'..'9', 'a'..'f', 'A'..'F':
        Result := True;
      else
        Result := False;
    end;
  end;

  function IsAlphaUncerscore(Run: Integer): Boolean;
  begin
    case fLine[Run] of
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
  fTokenID := tkNumber;
  while IsNumberChar(Run) do
  begin
    case FLine[Run] of
      '.':
        if FLine[Succ(Run)] = '.' then
          Break
        else
          if (fTokenID <> tkHex) then
            fTokenID := tkFloat
          else // invalid
          begin
            fTokenID := tkUnknown;
            Exit;
          end;
      '-', '+':
        begin
          if fTokenID <> tkFloat then // number <> float. an arithmetic operator
            Exit;
          if not CharInSet(FLine[Pred(Run)], ['e', 'E']) then
            Exit; // number = float, but no exponent. an arithmetic operator
          if not IsDigitPlusMinusChar(Succ(Run)) then // invalid
          begin
            Inc(Run);
            fTokenID := tkUnknown;
            Exit;
          end
        end;
      '0'..'9': ;
      'a'..'d', 'f', 'A'..'D', 'F':
        if fTokenID <> tkHex then // invalid char
          Break;
      'e', 'E':
        if (fTokenID <> tkHex) then
          if CharInSet(FLine[Pred(Run)], ['0'..'9']) then // exponent
          begin
            for i := idx1 to Pred(Run) do
              if CharInSet(FLine[i], ['e', 'E']) then // too many exponents
              begin
                fTokenID := tkUnknown;
                Exit;
              end;
            if not IsDigitPlusMinusChar(Succ(Run)) then
              Break
            else
              fTokenID := tkFloat
          end
          else // invalid char
            Break;
      'x', 'X':
        if (Run = Succ(idx1)) and   // 0x... 'x' must be second char
           (FLine[idx1] = '0') and  // 0x...
           IsHexDigit(Succ(Run)) then // 0x... must be continued with a number
             fTokenID := tkHex
           else // invalid char
           begin
             if not IsIdentChar(fLine[Succ(Run)]) and
                CharInSet(FLine[Succ(idx1)], ['x', 'X']) then
             begin
               Inc(Run); // highlight 'x' too
               fTokenID := tkUnknown;
             end;
             Break;
           end;
    end; // case
    Inc(Run);
  end; // while
  if IsAlphaUncerscore(Run) then
    fTokenID := tkUnknown;
end;

procedure TSynLLVMIRSyn.IdentProc;
begin
  fTokenID := IdentKind(fLine + Run);
  Inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do
    Inc(Run);

  if fLine[Run] = ':' then
  begin
    fTokenID := tkLabel;
    Inc(Run);
  end;
end;

procedure TSynLLVMIRSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynLLVMIRSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsString: StringProc;
  else
    case fLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      ';': SingleCommentOpenProc;
      '"': StringOpenProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      '0'..'9': NumberProc;
      'A'..'Z', 'a'..'h', 'j'..'z', '_': IdentProc;
      'i': IntegerTypeProc;
      '@': AtTypeProc;
      '%': PercentTypeProc;
    else
      UnknownProc;
    end;
  end;
  inherited;
end;

function TSynLLVMIRSyn.GetDefaultAttribute(Index: Integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynLLVMIRSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynLLVMIRSyn.GetKeyWords(TokenKind: Integer): UnicodeString;
begin
  Result := 
    'acq_rel,acquire,add,addrspace,alias,align,alignstack,alloca,alwaysinl' +
    'ine,and,appending,arcp,arm_aapcs_vfpcc,arm_aapcscc,arm_apcscc,ashr,asm' +
    ',atomic,atomicrmw,available_externally,bitcast,blockaddress,br,byval,c' +
    ',call,catch,cc,ccc,cleanup,cmpxchg,coldcc,common,constant,datalayout,d' +
    'eclare,default,define,deplibs,dllexport,dllimport,double,eq,exact,exce' +
    'pt,extern_weak,external,extractelement,extractvalue,fadd,false,fast,fa' +
    'stcc,fcmp,fdiv,fence,filter,float,fmul,fp128,fpext,fptosi,fptoui,fptru' +
    'nc,free,frem,fsub,gc,getelementptr,global,half,hidden,icmp,inbounds,in' +
    'directbr,initialexec,inlinehint,inreg,insertelement,insertvalue,intel_' +
    'ocl_bicc,inteldialect,internal,inttoptr,invoke,label,landingpad,linker' +
    '_private,linker_private_weak,linker_private_weak_def_auto,linkonce,lin' +
    'konce_odr,linkonce_odr_auto_hide,load,localdynamic,localexec,lshr,mall' +
    'oc,max,metadata,min,minsize,module,monotonic,msp430_intrcc,mul,naked,n' +
    'and,ne,nest,ninf,nnan,noalias,nocapture,noimplicitfloat,noinline,nonla' +
    'zybind,noredzone,noreturn,nounwind,nsw,nsz,null,nuw,oeq,oge,ogt,ole,ol' +
    't,one,opaque,optsize,or,ord,personality,phi,ppc_fp128,private,protecte' +
    'd,ptrtoint,ptx_device,ptx_kernel,readnone,readonly,release,resume,ret,' +
    'returns_twice,sanitize_address,sanitize_memory,sanitize_thread,sdiv,se' +
    'ction,select,seq_cst,sext,sge,sgt,shl,shufflevector,sideeffect,signext' +
    ',singlethread,sitofp,sle,slt,spir_func,spir_kernel,srem,sret,ssp,sspre' +
    'q,sspstrong,store,sub,switch,tail,target,thread_local,to,triple,true,t' +
    'runc,type,udiv,ueq,uge,ugt,uitofp,ule,ult,umax,umin,undef,une,unnamed_' +
    'addr,uno,unordered,unreachable,unwind,urem,uwtable,va_arg,void,volatil' +
    'e,weak,weak_odr,x86_fastcallcc,x86_fp80,x86_mmx,x86_stdcallcc,x86_this' +
    'callcc,xchg,xor,zeroext,zeroinitializer,zext';
end;

function TSynLLVMIRSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynLLVMIRSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkBoolean: Result := fBooleanAttri;
    tkComment: Result := fCommentAttri;
    tkConstant: Result := fConstantAttri;
    tkIdentifier, tkUnnamedIdentifier: Result := fIdentifierAttri;
    tkInstruction: Result := fInstructionAttri;
    tkKey: Result := fKeyAttri;
    tkLabel: Result := fLabelAttri;
    tkNumber, tkFloat, tkHex: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkType: Result := fTypesAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynLLVMIRSyn.GetTokenKind: Integer;
begin
  Result := Ord(fTokenId);
end;

function TSynLLVMIRSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', '.', '0'..'9', 'a'..'z', 'A'..'Z':
      Result := True;
    else
      Result := False;
  end;
end;

function TSynLLVMIRSyn.GetSampleSource: UnicodeString;
begin
  Result :=
    '; Declare the string constant as global constant' + #10#13 + '@.msg = ' +
    'internal constant [13 x i8] c"Hello World!\00"' + #10#13 + #10#13 +
    '; External declaration of puts function' + #10#13 + 'declare i32 ' +
    '@puts(i8*)' + #10#13 + #10#13 + '; Definition of main function' + #10#13 +
    'define i32 @main() {' + #10#13 + 'entry:' + #10#13 + #9 +
    '; Convert [13 x i8]* to i8 *...' + #10#13 + #9 +
    '%cast210 = getelementptr inbounds ([13 x i8]* @.msg, i32 0, i32 0)' +
    #10#13 + #10#13 + #9 + '; Call puts function to write out the string to ' +
    'stdout' + #10#13 + #9 + 'call i32 @puts(i8* %cast210)' + #10#13 +
    #9 + 'ret i32 0' + #10#13 + '}';

end;

function TSynLLVMIRSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterLLVMIR;
end;

class function TSynLLVMIRSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangLLVMIR;
end;

class function TSynLLVMIRSyn.GetLanguageName: string;
begin
  Result := SYNS_LangLLVMIR;
end;

procedure TSynLLVMIRSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynLLVMIRSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynLLVMIRSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynLLVMIRSyn);
{$ENDIF}
end.
