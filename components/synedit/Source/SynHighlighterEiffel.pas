{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: SynHighlighterEiffel.pas, released 2004-03-08.
Description: Eiffel Syntax Parser/Highlighter
The initial author of this file is Massimo Maria Ghisalberti (nissl).
Unicode translation by Maël Hörz.
Copyright (c) 2004, all rights reserved.

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

$Id: SynHighlighterEiffel.pas,v 1.3.2.8 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}
{
@abstract(Provides an Eiffel highlighter for SynEdit)
@author(Massimo Maria Ghisalberti (nissl@mammuth.it, nissl@linee.it - www.linee.it)
@created(03-08-2004)
@lastmod(03-08-2004)
The SynHighlighterEiffel unit provides SynEdit with an Eiffel highlighter.
}

unit SynHighlighterEiffel;

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
    tkBasicTypes,
    tkComment,
    tkIdentifier,
    tkKey,
    tkLace,
    tkNull,
    tkOperatorAndSymbols,
    tkPredefined,
    tkResultValue,
    tkSpace,
    tkString,
    tkUnknown);

  TRangeState = (rsUnknown, rsEiffelComment, rsString, rsOperatorAndSymbolProc);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynEiffelSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FIdentFuncTable: array[0..502] of TIdentFuncTableFunc;
    FBasicTypesAttri: TSynHighlighterAttributes;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FLaceAttri: TSynHighlighterAttributes;
    FOperatorAndSymbolsAttri: TSynHighlighterAttributes;
    FPredefinedAttri: TSynHighlighterAttributes;
    FResultValueAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function OperatorFunc(Index: Integer): TtkTokenKind;
    function Func37u(Index: Integer): TtkTokenKind;
    function FuncAdapt(Index: Integer): TtkTokenKind;
    function FuncAlias(Index: Integer): TtkTokenKind;
    function FuncAll(Index: Integer): TtkTokenKind;
    function FuncAnd(Index: Integer): TtkTokenKind;
    function FuncArray(Index: Integer): TtkTokenKind;
    function FuncAs(Index: Integer): TtkTokenKind;
    function FuncAssertion(Index: Integer): TtkTokenKind;
    function FuncBit(Index: Integer): TtkTokenKind;
    function FuncBoolean(Index: Integer): TtkTokenKind;
    function FuncCharacter(Index: Integer): TtkTokenKind;
    function FuncCheck(Index: Integer): TtkTokenKind;
    function FuncClass(Index: Integer): TtkTokenKind;
    function FuncCluster(Index: Integer): TtkTokenKind;
    function FuncColon(Index: Integer): TtkTokenKind;
    function FuncComma(Index: Integer): TtkTokenKind;
    function FuncCreation(Index: Integer): TtkTokenKind;
    function FuncCurrent(Index: Integer): TtkTokenKind;
    function FuncDebug(Index: Integer): TtkTokenKind;
    function FuncDefault(Index: Integer): TtkTokenKind;
    function FuncDeferred(Index: Integer): TtkTokenKind;
    function FuncDo(Index: Integer): TtkTokenKind;
    function FuncDouble(Index: Integer): TtkTokenKind;
    function FuncElse(Index: Integer): TtkTokenKind;
    function FuncElseif(Index: Integer): TtkTokenKind;
    function FuncEnd(Index: Integer): TtkTokenKind;
    function FuncEnsure(Index: Integer): TtkTokenKind;
    function FuncExclude(Index: Integer): TtkTokenKind;
    function FuncExecutable(Index: Integer): TtkTokenKind;
    function FuncExpanded(Index: Integer): TtkTokenKind;
    function FuncExport(Index: Integer): TtkTokenKind;
    function FuncExternal(Index: Integer): TtkTokenKind;
    function FuncFalse(Index: Integer): TtkTokenKind;
    function FuncFeature(Index: Integer): TtkTokenKind;
    function FuncFrom(Index: Integer): TtkTokenKind;
    function FuncFrozen(Index: Integer): TtkTokenKind;
    function FuncGenerate(Index: Integer): TtkTokenKind;
    function FuncIdentifier(Index: Integer): TtkTokenKind;
    function FuncIf(Index: Integer): TtkTokenKind;
    function FuncIgnore(Index: Integer): TtkTokenKind;
    function FuncImplies(Index: Integer): TtkTokenKind;
    function FuncInclude(Index: Integer): TtkTokenKind;
    function FuncInclude95path(Index: Integer): TtkTokenKind;
    function FuncIndexing(Index: Integer): TtkTokenKind;
    function FuncInfix(Index: Integer): TtkTokenKind;
    function FuncInherit(Index: Integer): TtkTokenKind;
    function FuncInspect(Index: Integer): TtkTokenKind;
    function FuncInteger(Index: Integer): TtkTokenKind;
    function FuncInvariant(Index: Integer): TtkTokenKind;
    function FuncIs(Index: Integer): TtkTokenKind;
    function FuncLike(Index: Integer): TtkTokenKind;
    function FuncLocal(Index: Integer): TtkTokenKind;
    function FuncLoop(Index: Integer): TtkTokenKind;
    function FuncMake(Index: Integer): TtkTokenKind;
    function FuncNo(Index: Integer): TtkTokenKind;
    function FuncNot(Index: Integer): TtkTokenKind;
    function FuncObject(Index: Integer): TtkTokenKind;
    function FuncObsolete(Index: Integer): TtkTokenKind;
    function FuncOld(Index: Integer): TtkTokenKind;
    function FuncOnce(Index: Integer): TtkTokenKind;
    function FuncOptimize(Index: Integer): TtkTokenKind;
    function FuncOption(Index: Integer): TtkTokenKind;
    function FuncOr(Index: Integer): TtkTokenKind;
    function FuncPointer(Index: Integer): TtkTokenKind;
    function FuncPrecompiled(Index: Integer): TtkTokenKind;
    function FuncPrecursor(Index: Integer): TtkTokenKind;
    function FuncPrefix(Index: Integer): TtkTokenKind;
    function FuncReal(Index: Integer): TtkTokenKind;
    function FuncRedefine(Index: Integer): TtkTokenKind;
    function FuncRename(Index: Integer): TtkTokenKind;
    function FuncRequire(Index: Integer): TtkTokenKind;
    function FuncRescue(Index: Integer): TtkTokenKind;
    function FuncResult(Index: Integer): TtkTokenKind;
    function FuncRetry(Index: Integer): TtkTokenKind;
    function FuncRoot(Index: Integer): TtkTokenKind;
    function FuncSelect(Index: Integer): TtkTokenKind;
    function FuncSeparate(Index: Integer): TtkTokenKind;
    function FuncString(Index: Integer): TtkTokenKind;
    function FuncStrip(Index: Integer): TtkTokenKind;
    function FuncSystem(Index: Integer): TtkTokenKind;
    function FuncThen(Index: Integer): TtkTokenKind;
    function FuncTrace(Index: Integer): TtkTokenKind;
    function FuncTrue(Index: Integer): TtkTokenKind;
    function FuncUndefine(Index: Integer): TtkTokenKind;
    function FuncUnique(Index: Integer): TtkTokenKind;
    function FuncUntil(Index: Integer): TtkTokenKind;
    function FuncUse(Index: Integer): TtkTokenKind;
    function FuncVariant(Index: Integer): TtkTokenKind;
    function FuncVisible(Index: Integer): TtkTokenKind;
    function FuncVoid(Index: Integer): TtkTokenKind;
    function FuncWhen(Index: Integer): TtkTokenKind;
    function FuncXor(Index: Integer): TtkTokenKind;
    function FuncYes(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure IdentProc;
    procedure InitIdent;
    procedure OperatorAndSymbolProc;
    procedure UnknownProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure EiffelCommentOpenProc;
    procedure EiffelCommentProc;
    procedure StringOpenProc;
    procedure StringProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetKeyWords(TokenKind: Integer): UnicodeString; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    function IsOperatorChar(AChar: WideChar): Boolean;
  published
    property BasicTypesAttri: TSynHighlighterAttributes read FBasicTypesAttri write FBasicTypesAttri;
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property LaceAttri: TSynHighlighterAttributes read FLaceAttri write FLaceAttri;
    property OperatorAndSymbolsAttri: TSynHighlighterAttributes read FOperatorAndSymbolsAttri write FOperatorAndSymbolsAttri;
    property PredefinedAttri: TSynHighlighterAttributes read FPredefinedAttri write FPredefinedAttri;
    property ResultValueAttri: TSynHighlighterAttributes read FResultValueAttri write FResultValueAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  KeyWords: array[0..118] of UnicodeString = (
    '-', '!', '#', '$', '%u', '&', '(', ')', '*', '.', '/', '//', '/=', ':', 
    ':=', ';', '@', '[', '\\', ']', '^', '|', '+', '<', '<>', '=', '>', 'adapt', 
    'alias', 'all', 'and', 'array', 'as', 'assertion', 'bit', 'boolean', 
    'character', 'check', 'class', 'cluster', 'colon', 'comma', 'creation', 
    'current', 'debug', 'default', 'deferred', 'do', 'double', 'else', 'elseif', 
    'end', 'ensure', 'exclude', 'executable', 'expanded', 'export', 'external', 
    'false', 'feature', 'from', 'frozen', 'generate', 'identifier', 'if', 
    'ignore', 'implies', 'include', 'include_path', 'indexing', 'infix', 
    'inherit', 'inspect', 'integer', 'invariant', 'is', 'like', 'local', 'loop', 
    'make', 'no', 'not', 'object', 'obsolete', 'old', 'once', 'optimize', 
    'option', 'or', 'pointer', 'precompiled', 'precursor', 'prefix', 'real', 
    'redefine', 'rename', 'require', 'rescue', 'result', 'retry', 'root', 
    'select', 'separate', 'string', 'strip', 'system', 'then', 'trace', 'true', 
    'undefine', 'unique', 'until', 'use', 'variant', 'visible', 'void', 'when', 
    'xor', 'yes' 
  );

  KeyIndices: array[0..502] of Integer = (
    -1, 49, -1, -1, -1, 97, 69, 85, -1, -1, -1, 106, -1, -1, 37, -1, -1, 63, -1, 
    92, -1, -1, -1, -1, 108, 82, 16, -1, -1, -1, -1, -1, 86, -1, 0, -1, -1, 66, 
    -1, -1, -1, -1, 91, 98, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 30, 13, -1, 
    -1, -1, -1, -1, -1, -1, 61, -1, -1, -1, -1, -1, -1, -1, 76, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 58, -1, -1, -1, -1, 110, -1, 1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 31, -1, -1, -1, -1, -1, -1, -1, 9, 
    -1, -1, -1, -1, -1, -1, 68, 88, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 15, 105, -1, -1, -1, 51, -1, -1, 6, -1, 96, -1, -1, 17, -1, -1, 55, -1, 
    -1, -1, -1, -1, 117, -1, -1, -1, 77, -1, -1, -1, -1, -1, -1, 56, -1, -1, -1, 
    -1, 62, -1, 59, -1, -1, -1, -1, -1, -1, 79, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 83, 10, 95, -1, 113, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 65, 18, 23, -1, -1, -1, 35, -1, -1, -1, 7, -1, -1, 32, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 21, 90, -1, 103, -1, -1, 80, -1, 
    -1, -1, -1, 2, -1, 34, -1, -1, -1, -1, -1, -1, 41, -1, 27, 112, -1, -1, -1, 
    33, -1, 44, -1, 50, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 104, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 25, -1, -1, -1, 93, -1, -1, -1, 8, 46, 102, -1, 
    -1, 19, 87, -1, -1, -1, -1, 43, -1, -1, -1, -1, -1, -1, -1, 84, 53, -1, -1, 
    -1, 71, -1, -1, 11, -1, 3, 107, 67, -1, 64, 47, -1, -1, -1, -1, -1, 24, -1, 
    -1, -1, 114, -1, -1, -1, 116, -1, -1, -1, -1, 81, 75, -1, -1, -1, -1, -1, 
    -1, -1, 100, -1, -1, -1, -1, -1, 54, -1, -1, 26, 115, -1, -1, -1, -1, -1, 
    78, 22, 36, -1, 74, -1, 20, -1, -1, 42, -1, 99, -1, -1, -1, -1, -1, -1, -1, 
    73, -1, 52, -1, -1, 29, -1, -1, -1, -1, -1, -1, -1, -1, 60, -1, 4, 94, -1, 
    -1, 40, -1, -1, 39, -1, -1, -1, -1, 45, -1, 12, -1, -1, -1, 72, 38, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 109, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 28, 48, -1, -1, -1, -1, -1, 101, -1, 118, 
    -1, -1, 57, -1, -1, -1, -1, -1, 14, -1, -1, -1, -1, -1, -1, 5, -1, -1, -1, 
    -1, -1, -1, -1, -1, 70, -1, 89, -1, -1, 111, -1 
  );

{$Q-}
function TSynEiffelSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) or IsOperatorChar(Str^) do
  begin
    Result := Result * 543 + Ord(Str^) * 79;
    Inc(Str);
  end;
  Result := Result mod 503;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynEiffelSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynEiffelSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[34] := OperatorFunc;
  FIdentFuncTable[92] := OperatorFunc;
  FIdentFuncTable[250] := OperatorFunc;
  FIdentFuncTable[329] := OperatorFunc;
  FIdentFuncTable[413] := Func37u;
  FIdentFuncTable[487] := OperatorFunc;
  FIdentFuncTable[142] := OperatorFunc;
  FIdentFuncTable[221] := OperatorFunc;
  FIdentFuncTable[300] := OperatorFunc;
  FIdentFuncTable[113] := OperatorFunc;
  FIdentFuncTable[192] := OperatorFunc;
  FIdentFuncTable[327] := OperatorFunc;
  FIdentFuncTable[427] := OperatorFunc;
  FIdentFuncTable[55] := OperatorFunc;
  FIdentFuncTable[480] := OperatorFunc;
  FIdentFuncTable[134] := OperatorFunc;
  FIdentFuncTable[26] := OperatorFunc;
  FIdentFuncTable[147] := OperatorFunc;
  FIdentFuncTable[212] := OperatorFunc;
  FIdentFuncTable[305] := OperatorFunc;
  FIdentFuncTable[384] := OperatorFunc;
  FIdentFuncTable[239] := OperatorFunc;
  FIdentFuncTable[379] := OperatorFunc;
  FIdentFuncTable[213] := OperatorFunc;
  FIdentFuncTable[340] := OperatorFunc;
  FIdentFuncTable[292] := OperatorFunc;
  FIdentFuncTable[371] := OperatorFunc;
  FIdentFuncTable[261] := FuncAdapt;
  FIdentFuncTable[462] := FuncAlias;
  FIdentFuncTable[402] := FuncAll;
  FIdentFuncTable[54] := FuncAnd;
  FIdentFuncTable[105] := FuncArray;
  FIdentFuncTable[224] := FuncAs;
  FIdentFuncTable[266] := FuncAssertion;
  FIdentFuncTable[252] := FuncBit;
  FIdentFuncTable[217] := FuncBoolean;
  FIdentFuncTable[380] := FuncCharacter;
  FIdentFuncTable[14] := FuncCheck;
  FIdentFuncTable[432] := FuncClass;
  FIdentFuncTable[420] := FuncCluster;
  FIdentFuncTable[417] := FuncColon;
  FIdentFuncTable[259] := FuncComma;
  FIdentFuncTable[387] := FuncCreation;
  FIdentFuncTable[311] := FuncCurrent;
  FIdentFuncTable[268] := FuncDebug;
  FIdentFuncTable[425] := FuncDefault;
  FIdentFuncTable[301] := FuncDeferred;
  FIdentFuncTable[334] := FuncDo;
  FIdentFuncTable[463] := FuncDouble;
  FIdentFuncTable[1] := FuncElse;
  FIdentFuncTable[270] := FuncElseif;
  FIdentFuncTable[139] := FuncEnd;
  FIdentFuncTable[399] := FuncEnsure;
  FIdentFuncTable[320] := FuncExclude;
  FIdentFuncTable[368] := FuncExecutable;
  FIdentFuncTable[150] := FuncExpanded;
  FIdentFuncTable[167] := FuncExport;
  FIdentFuncTable[474] := FuncExternal;
  FIdentFuncTable[85] := FuncFalse;
  FIdentFuncTable[174] := FuncFeature;
  FIdentFuncTable[411] := FuncFrom;
  FIdentFuncTable[63] := FuncFrozen;
  FIdentFuncTable[172] := FuncGenerate;
  FIdentFuncTable[17] := FuncIdentifier;
  FIdentFuncTable[333] := FuncIf;
  FIdentFuncTable[211] := FuncIgnore;
  FIdentFuncTable[37] := FuncImplies;
  FIdentFuncTable[331] := FuncInclude;
  FIdentFuncTable[120] := FuncInclude95path;
  FIdentFuncTable[6] := FuncIndexing;
  FIdentFuncTable[496] := FuncInfix;
  FIdentFuncTable[324] := FuncInherit;
  FIdentFuncTable[431] := FuncInspect;
  FIdentFuncTable[397] := FuncInteger;
  FIdentFuncTable[382] := FuncInvariant;
  FIdentFuncTable[354] := FuncIs;
  FIdentFuncTable[71] := FuncLike;
  FIdentFuncTable[160] := FuncLocal;
  FIdentFuncTable[378] := FuncLoop;
  FIdentFuncTable[181] := FuncMake;
  FIdentFuncTable[245] := FuncNo;
  FIdentFuncTable[353] := FuncNot;
  FIdentFuncTable[25] := FuncObject;
  FIdentFuncTable[191] := FuncObsolete;
  FIdentFuncTable[319] := FuncOld;
  FIdentFuncTable[7] := FuncOnce;
  FIdentFuncTable[32] := FuncOptimize;
  FIdentFuncTable[306] := FuncOption;
  FIdentFuncTable[121] := FuncOr;
  FIdentFuncTable[498] := FuncPointer;
  FIdentFuncTable[240] := FuncPrecompiled;
  FIdentFuncTable[42] := FuncPrecursor;
  FIdentFuncTable[19] := FuncPrefix;
  FIdentFuncTable[296] := FuncReal;
  FIdentFuncTable[414] := FuncRedefine;
  FIdentFuncTable[193] := FuncRename;
  FIdentFuncTable[144] := FuncRequire;
  FIdentFuncTable[5] := FuncRescue;
  FIdentFuncTable[43] := FuncResult;
  FIdentFuncTable[389] := FuncRetry;
  FIdentFuncTable[362] := FuncRoot;
  FIdentFuncTable[469] := FuncSelect;
  FIdentFuncTable[302] := FuncSeparate;
  FIdentFuncTable[242] := FuncString;
  FIdentFuncTable[282] := FuncStrip;
  FIdentFuncTable[135] := FuncSystem;
  FIdentFuncTable[11] := FuncThen;
  FIdentFuncTable[330] := FuncTrace;
  FIdentFuncTable[24] := FuncTrue;
  FIdentFuncTable[452] := FuncUndefine;
  FIdentFuncTable[90] := FuncUnique;
  FIdentFuncTable[501] := FuncUntil;
  FIdentFuncTable[262] := FuncUse;
  FIdentFuncTable[195] := FuncVariant;
  FIdentFuncTable[344] := FuncVisible;
  FIdentFuncTable[372] := FuncVoid;
  FIdentFuncTable[348] := FuncWhen;
  FIdentFuncTable[156] := FuncXor;
  FIdentFuncTable[471] := FuncYes;
end;

function TSynEiffelSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynEiffelSyn.OperatorFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkOperatorAndSymbols
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func37u(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPredefined
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncAdapt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncAlias(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncAll(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncAnd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncArray(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBasicTypes
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncAs(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncAssertion(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncBit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPredefined
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncBoolean(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBasicTypes
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncCharacter(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBasicTypes
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncCheck(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncClass(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncCluster(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncColon(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncComma(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncCreation(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncCurrent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPredefined
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncDebug(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncDefault(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncDeferred(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncDo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncDouble(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBasicTypes
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncElse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncElseif(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncEnd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncEnsure(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncExclude(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncExecutable(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncExpanded(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncExport(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncExternal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncFalse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPredefined
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncFeature(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncFrom(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncFrozen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncGenerate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncIdentifier(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncIf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncIgnore(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncImplies(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncInclude(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncInclude95path(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncIndexing(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncInfix(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncInherit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncInspect(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncInteger(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBasicTypes
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncInvariant(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncIs(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncLike(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncLocal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncLoop(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncMake(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncNo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncNot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncObject(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncObsolete(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncOld(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncOnce(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncOptimize(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncOption(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncOr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncPointer(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBasicTypes
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncPrecompiled(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncPrecursor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPredefined
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncPrefix(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncReal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBasicTypes
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncRedefine(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncRename(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncRequire(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncRescue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncResult(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkResultValue
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncRetry(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncRoot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncSelect(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncSeparate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncString(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBasicTypes
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncStrip(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPredefined
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncSystem(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncThen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncTrace(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncTrue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPredefined
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncUndefine(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncUnique(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPredefined
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncUntil(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncUse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncVariant(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncVisible(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncVoid(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPredefined
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncWhen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncXor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.FuncYes(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

procedure TSynEiffelSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynEiffelSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynEiffelSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynEiffelSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynEiffelSyn.OperatorAndSymbolProc;
begin
  FTokenID := tkIdentifier;
  if FLine[Run] = #33 then
    begin
      FRange := rsOperatorAndSymbolProc;
      FTokenID := tkOperatorAndSymbols;
      Inc(Run);
      Exit;
    end;
  if CharInSet(FLine[Run], [#35..#44]) then
    begin
      FRange := rsOperatorAndSymbolProc;
      FTokenID := tkOperatorAndSymbols;
      Inc(Run);
      Exit;
    end;
  if CharInSet(FLine[Run], [#46..#47]) then
    begin
      FRange := rsOperatorAndSymbolProc;
      FTokenID := tkOperatorAndSymbols;
      Inc(Run);
      Exit;
    end;
  if CharInSet(FLine[Run], [#58..#64]) then
    begin
      FRange := rsOperatorAndSymbolProc;
      FTokenID := tkOperatorAndSymbols;
      Inc(Run);
      Exit;
    end;
  if CharInSet(FLine[Run], [#91..#96]) then
    begin
      FRange := rsOperatorAndSymbolProc;
      FTokenID := tkOperatorAndSymbols;
      Inc(Run);
      Exit;
    end;
  if CharInSet(FLine[Run], [#123..#127]) then
    begin
      FRange := rsOperatorAndSymbolProc;
      FTokenID := tkOperatorAndSymbols;
      Inc(Run);
      Exit;
    end;
end;

procedure TSynEiffelSyn.EiffelCommentOpenProc;
begin
  Inc(Run);
  if (FLine[Run - 1] = '-') and (FLine[Run] = '-') then
    begin
      FRange := rsEiffelComment;
      EiffelCommentProc;
      FTokenID := tkComment;
    end
  else
    FTokenID := tkOperatorAndSymbols;
end;

procedure TSynEiffelSyn.EiffelCommentProc;
begin
  FTokenID := tkComment;
  repeat
    if not IsLineEnd(Run) then
      Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynEiffelSyn.StringOpenProc;
begin
  Inc(Run);
  FRange := rsString;
  StringProc;
  FTokenID := tkString;
end;

procedure TSynEiffelSyn.StringProc;
begin
  FTokenID := tkString;
  repeat
    if (FLine[Run] = '"') then
      begin
        Inc(Run, 1);
        FRange := rsUnknown;
        Break;
      end;
    if not IsLineEnd(Run) then
      Inc(Run);
  until IsLineEnd(Run);
end;

constructor TSynEiffelSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FBasicTypesAttri := TSynHighLighterAttributes.Create(SYNS_AttrBasicTypes, SYNS_FriendlyAttrBasicTypes);
  FBasicTypesAttri.Style := [fsBold];
  FBasicTypesAttri.Foreground := clBlue;
  AddAttribute(FBasicTypesAttri);

  FCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  FCommentAttri.Foreground := clTeal;
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  FIdentifierAttri.Foreground := clMaroon;
  AddAttribute(FIdentifierAttri);

  FKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  FKeyAttri.Foreground := clNavy;
  AddAttribute(FKeyAttri);

  FLaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrLace, SYNS_FriendlyAttrLace);
  FLaceAttri.Style := [fsBold];
  FLaceAttri.Foreground := clNavy;
  AddAttribute(FLaceAttri);

  FOperatorAndSymbolsAttri := TSynHighLighterAttributes.Create(SYNS_AttrOperatorAndSymbols, SYNS_FriendlyAttrOperatorAndSymbols);
  FOperatorAndSymbolsAttri.Style := [fsBold];
  FOperatorAndSymbolsAttri.Foreground := clOlive;
  AddAttribute(FOperatorAndSymbolsAttri);

  FPredefinedAttri := TSynHighLighterAttributes.Create(SYNS_AttrPredefined, SYNS_FriendlyAttrPredefined);
  FPredefinedAttri.Style := [fsBold];
  FPredefinedAttri.Foreground := clRed;
  AddAttribute(FPredefinedAttri);

  FResultValueAttri := TSynHighLighterAttributes.Create(SYNS_AttrResultValue, SYNS_FriendlyAttrResultValue);
  FResultValueAttri.Style := [fsBold];
  FResultValueAttri.Foreground := clPurple;
  AddAttribute(FResultValueAttri);

  FSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Style := [fsItalic];
  FStringAttri.Foreground := clGray;
  AddAttribute(FStringAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  FDefaultFilter := SYNS_FilterEiffel;
  FRange := rsUnknown;
end;

procedure TSynEiffelSyn.IdentProc;
begin
  FTokenID := IdentKind(FLine + Run);
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
end;

procedure TSynEiffelSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynEiffelSyn.Next;
begin
  FTokenPos := Run;
  FRange := rsUnknown;
  case FLine[Run] of
    #33, #35..#44, #46..#47, #58..#64, #91..#96, #123..#127: OperatorAndSymbolProc;
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    '-': EiffelCommentOpenProc;
    '"': StringOpenProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    'A'..'Z', 'a'..'z': IdentProc;
    else UnknownProc;
  end;
  inherited;
end;

function TSynEiffelSyn.GetDefaultAttribute(Index: Integer): TSynHighLighterAttributes;
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

function TSynEiffelSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynEiffelSyn.GetKeyWords(TokenKind: Integer): UnicodeString;
begin
  Result :=
    '-,!,#,$,%U,&,(,),*,.,/,//,/=,:,:=,;,@,[,\\,],^,|,+,<,<>,=,>,adapt,ali' +
    'as,all,and,Array,as,assertion,BIT,boolean,character,check,class,cluste' +
    'r,colon,comma,creation,current,debug,default,deferred,do,double,else,e' +
    'lseif,end,ensure,exclude,executable,expanded,export,external,false,fea' +
    'ture,from,frozen,generate,identifier,if,ignore,implies,include,include' +
    '_path,indexing,infix,inherit,inspect,integer,invariant,is,like,local,l' +
    'oop,make,no,not,object,obsolete,old,once,optimize,option,or,pointer,pr' +
    'ecompiled,precursor,prefix,real,redefine,rename,require,rescue,result,' +
    'retry,root,select,separate,string,strip,system,then,trace,true,undefin' +
    'e,unique,until,use,variant,visible,void,when,xor,yes';
end;

function TSynEiffelSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynEiffelSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkBasicTypes: Result := FBasicTypesAttri;
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkLace: Result := FLaceAttri;
    tkOperatorAndSymbols: Result := FOperatorAndSymbolsAttri;
    tkPredefined: Result := FPredefinedAttri;
    tkResultValue: Result := FResultValueAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkUnknown: Result := FIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynEiffelSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

function TSynEiffelSyn.GetSampleSource: UnicodeString;
begin
  Result := '-- Eiffel sample source from SmartEiffel'#13#10 +
    'class FIBONACCI'#13#10 +
    '-- Eiffel comment'#13#10 +
    'creation make'#13#10 +
    #13#10 +
    'feature'#13#10 +
    #13#10 +
    '   make is'#13#10 +
    '      do'#13#10 +
    '         if argument_count /= 1 or else'#13#10 +
    '            not argument(1).is_integer'#13#10 +
    '          then'#13#10 +
    '            io.put_string("Usage: ");'#13#10 +
    '            io.put_string(argument(0));'#13#10 +
    '            io.put_string(" <Integer_value>%N");'#13#10 +
    '            die_with_code(exit_failure_code);'#13#10 +
    '         end;'#13#10 +
    '         io.put_integer(fibonacci(argument(1).to_integer));'#13#10 +
    '         io.put_new_line;'#13#10 +
    '      end;'#13#10 +
    '   -- Eiffel comment'#13#10 +
    '   fibonacci(i: INTEGER): INTEGER is'#13#10 +
    '      require -- Eiffel comment'#13#10 +
    '         i >= 0'#13#10 +
    '      do'#13#10 +
    '         if i = 0 then'#13#10 +
    '            Result := 1;'#13#10 +
    '         elseif i = 1 then'#13#10 +
    '            Result := 1;'#13#10 +
    '         else'#13#10 +
    '            Result := fibonacci(i - 1) + fibonacci(i - 2) ;'#13#10 +
    '         end;'#13#10 +
    '      end;'#13#10 +
    #13#10 +
    'end';
end;

function TSynEiffelSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterEiffel;
end;

class function TSynEiffelSyn.GetLanguageName: string;
begin
  Result := SYNS_LangEiffel;
end;

procedure TSynEiffelSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynEiffelSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynEiffelSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynEiffelSyn.IsOperatorChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '-', '!', '#', '$', '%', '&', '(', ')', '*', '.', '/',
    ':', ';', '@', '[', '\', ']', '^', '|', '+', '<', '=', '>':
      Result := True
    else
      Result := False;
  end;
end;

class function TSynEiffelSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangEiffel;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynEiffelSyn);
{$ENDIF}
end.
