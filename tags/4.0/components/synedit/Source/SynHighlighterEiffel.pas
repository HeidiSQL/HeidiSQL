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

{$IFNDEF QSYNHIGHLIGHTEREIFFEL}
unit SynHighlighterEiffel;
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
  SynUnicode,
{$ENDIF}
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

  TRangeState = (rsUnKnown, rsEiffelComment, rsString, rsOperatorAndSymbolProc);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynEiffelSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..502] of TIdentFuncTableFunc;
    fBasicTypesAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fLaceAttri: TSynHighlighterAttributes;
    fOperatorAndSymbolsAttri: TSynHighlighterAttributes;
    fPredefinedAttri: TSynHighlighterAttributes;
    fResultValueAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
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
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetKeyWords(TokenKind: Integer): UnicodeString; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    procedure Next; override;
    function IsOperatorChar(AChar: WideChar): Boolean;
  published
    property BasicTypesAttri: TSynHighlighterAttributes read fBasicTypesAttri write fBasicTypesAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property LaceAttri: TSynHighlighterAttributes read fLaceAttri write fLaceAttri;
    property OperatorAndSymbolsAttri: TSynHighlighterAttributes read fOperatorAndSymbolsAttri write fOperatorAndSymbolsAttri;
    property PredefinedAttri: TSynHighlighterAttributes read fPredefinedAttri write fPredefinedAttri;
    property ResultValueAttri: TSynHighlighterAttributes read fResultValueAttri write fResultValueAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

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
    inc(Str);
  end;
  Result := Result mod 503;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynEiffelSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynEiffelSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[34] := OperatorFunc;
  fIdentFuncTable[92] := OperatorFunc;
  fIdentFuncTable[250] := OperatorFunc;
  fIdentFuncTable[329] := OperatorFunc;
  fIdentFuncTable[413] := Func37u;
  fIdentFuncTable[487] := OperatorFunc;
  fIdentFuncTable[142] := OperatorFunc;
  fIdentFuncTable[221] := OperatorFunc;
  fIdentFuncTable[300] := OperatorFunc;
  fIdentFuncTable[113] := OperatorFunc;
  fIdentFuncTable[192] := OperatorFunc;
  fIdentFuncTable[327] := OperatorFunc;
  fIdentFuncTable[427] := OperatorFunc;
  fIdentFuncTable[55] := OperatorFunc;
  fIdentFuncTable[480] := OperatorFunc;
  fIdentFuncTable[134] := OperatorFunc;
  fIdentFuncTable[26] := OperatorFunc;
  fIdentFuncTable[147] := OperatorFunc;
  fIdentFuncTable[212] := OperatorFunc;
  fIdentFuncTable[305] := OperatorFunc;
  fIdentFuncTable[384] := OperatorFunc;
  fIdentFuncTable[239] := OperatorFunc;
  fIdentFuncTable[379] := OperatorFunc;
  fIdentFuncTable[213] := OperatorFunc;
  fIdentFuncTable[340] := OperatorFunc;
  fIdentFuncTable[292] := OperatorFunc;
  fIdentFuncTable[371] := OperatorFunc;
  fIdentFuncTable[261] := FuncAdapt;
  fIdentFuncTable[462] := FuncAlias;
  fIdentFuncTable[402] := FuncAll;
  fIdentFuncTable[54] := FuncAnd;
  fIdentFuncTable[105] := FuncArray;
  fIdentFuncTable[224] := FuncAs;
  fIdentFuncTable[266] := FuncAssertion;
  fIdentFuncTable[252] := FuncBit;
  fIdentFuncTable[217] := FuncBoolean;
  fIdentFuncTable[380] := FuncCharacter;
  fIdentFuncTable[14] := FuncCheck;
  fIdentFuncTable[432] := FuncClass;
  fIdentFuncTable[420] := FuncCluster;
  fIdentFuncTable[417] := FuncColon;
  fIdentFuncTable[259] := FuncComma;
  fIdentFuncTable[387] := FuncCreation;
  fIdentFuncTable[311] := FuncCurrent;
  fIdentFuncTable[268] := FuncDebug;
  fIdentFuncTable[425] := FuncDefault;
  fIdentFuncTable[301] := FuncDeferred;
  fIdentFuncTable[334] := FuncDo;
  fIdentFuncTable[463] := FuncDouble;
  fIdentFuncTable[1] := FuncElse;
  fIdentFuncTable[270] := FuncElseif;
  fIdentFuncTable[139] := FuncEnd;
  fIdentFuncTable[399] := FuncEnsure;
  fIdentFuncTable[320] := FuncExclude;
  fIdentFuncTable[368] := FuncExecutable;
  fIdentFuncTable[150] := FuncExpanded;
  fIdentFuncTable[167] := FuncExport;
  fIdentFuncTable[474] := FuncExternal;
  fIdentFuncTable[85] := FuncFalse;
  fIdentFuncTable[174] := FuncFeature;
  fIdentFuncTable[411] := FuncFrom;
  fIdentFuncTable[63] := FuncFrozen;
  fIdentFuncTable[172] := FuncGenerate;
  fIdentFuncTable[17] := FuncIdentifier;
  fIdentFuncTable[333] := FuncIf;
  fIdentFuncTable[211] := FuncIgnore;
  fIdentFuncTable[37] := FuncImplies;
  fIdentFuncTable[331] := FuncInclude;
  fIdentFuncTable[120] := FuncInclude95path;
  fIdentFuncTable[6] := FuncIndexing;
  fIdentFuncTable[496] := FuncInfix;
  fIdentFuncTable[324] := FuncInherit;
  fIdentFuncTable[431] := FuncInspect;
  fIdentFuncTable[397] := FuncInteger;
  fIdentFuncTable[382] := FuncInvariant;
  fIdentFuncTable[354] := FuncIs;
  fIdentFuncTable[71] := FuncLike;
  fIdentFuncTable[160] := FuncLocal;
  fIdentFuncTable[378] := FuncLoop;
  fIdentFuncTable[181] := FuncMake;
  fIdentFuncTable[245] := FuncNo;
  fIdentFuncTable[353] := FuncNot;
  fIdentFuncTable[25] := FuncObject;
  fIdentFuncTable[191] := FuncObsolete;
  fIdentFuncTable[319] := FuncOld;
  fIdentFuncTable[7] := FuncOnce;
  fIdentFuncTable[32] := FuncOptimize;
  fIdentFuncTable[306] := FuncOption;
  fIdentFuncTable[121] := FuncOr;
  fIdentFuncTable[498] := FuncPointer;
  fIdentFuncTable[240] := FuncPrecompiled;
  fIdentFuncTable[42] := FuncPrecursor;
  fIdentFuncTable[19] := FuncPrefix;
  fIdentFuncTable[296] := FuncReal;
  fIdentFuncTable[414] := FuncRedefine;
  fIdentFuncTable[193] := FuncRename;
  fIdentFuncTable[144] := FuncRequire;
  fIdentFuncTable[5] := FuncRescue;
  fIdentFuncTable[43] := FuncResult;
  fIdentFuncTable[389] := FuncRetry;
  fIdentFuncTable[362] := FuncRoot;
  fIdentFuncTable[469] := FuncSelect;
  fIdentFuncTable[302] := FuncSeparate;
  fIdentFuncTable[242] := FuncString;
  fIdentFuncTable[282] := FuncStrip;
  fIdentFuncTable[135] := FuncSystem;
  fIdentFuncTable[11] := FuncThen;
  fIdentFuncTable[330] := FuncTrace;
  fIdentFuncTable[24] := FuncTrue;
  fIdentFuncTable[452] := FuncUndefine;
  fIdentFuncTable[90] := FuncUnique;
  fIdentFuncTable[501] := FuncUntil;
  fIdentFuncTable[262] := FuncUse;
  fIdentFuncTable[195] := FuncVariant;
  fIdentFuncTable[344] := FuncVisible;
  fIdentFuncTable[372] := FuncVoid;
  fIdentFuncTable[348] := FuncWhen;
  fIdentFuncTable[156] := FuncXor;
  fIdentFuncTable[471] := FuncYes;
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
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynEiffelSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynEiffelSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
    inc(Run);
end;

procedure TSynEiffelSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynEiffelSyn.OperatorAndSymbolProc;
begin
  fTokenID := tkIdentifier;
  if fLine[Run] = #33 then
    begin
      fRange := rsOperatorAndSymbolProc;
      fTokenID := tkOperatorAndSymbols;
      Inc(Run);
      Exit;
    end;
  if CharInSet(fLine[Run], [#35..#44]) then
    begin
      fRange := rsOperatorAndSymbolProc;
      fTokenID := tkOperatorAndSymbols;
      Inc(Run);
      Exit;
    end;
  if CharInSet(fLine[Run], [#46..#47]) then
    begin
      fRange := rsOperatorAndSymbolProc;
      fTokenID := tkOperatorAndSymbols;
      Inc(Run);
      Exit;
    end;
  if CharInSet(fLine[Run], [#58..#64]) then
    begin
      fRange := rsOperatorAndSymbolProc;
      fTokenID := tkOperatorAndSymbols;
      Inc(Run);
      Exit;
    end;
  if CharInSet(fLine[Run], [#91..#96]) then
    begin
      fRange := rsOperatorAndSymbolProc;
      fTokenID := tkOperatorAndSymbols;
      Inc(Run);
      Exit;
    end;
  if CharInSet(fLine[Run], [#123..#127]) then
    begin
      fRange := rsOperatorAndSymbolProc;
      fTokenID := tkOperatorAndSymbols;
      Inc(Run);
      Exit;
    end;
end;

procedure TSynEiffelSyn.EiffelCommentOpenProc;
begin
  Inc(Run);
  if (fLine[Run - 1] = '-') and (fLine[Run] = '-') then
    begin
      fRange := rsEiffelComment;
      EiffelCommentProc;
      fTokenID := tkComment;
    end
  else
    fTokenID := tkOperatorAndSymbols;
end;

procedure TSynEiffelSyn.EiffelCommentProc;
begin
  fTokenID := tkComment;
  repeat
    if not IsLineEnd(Run) then
      Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynEiffelSyn.StringOpenProc;
begin
  Inc(Run);
  fRange := rsString;
  StringProc;
  fTokenID := tkString;
end;

procedure TSynEiffelSyn.StringProc;
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

constructor TSynEiffelSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  fBasicTypesAttri := TSynHighLighterAttributes.Create(SYNS_AttrBasicTypes, SYNS_FriendlyAttrBasicTypes);
  fBasicTypesAttri.Style := [fsBold];
  fBasicTypesAttri.Foreground := clBlue;
  AddAttribute(fBasicTypesAttri);

  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clTeal;
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  fIdentifierAttri.Foreground := clMaroon;
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := clNavy;
  AddAttribute(fKeyAttri);

  fLaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrLace, SYNS_FriendlyAttrLace);
  fLaceAttri.Style := [fsBold];
  fLaceAttri.Foreground := clNavy;
  AddAttribute(fLaceAttri);

  fOperatorAndSymbolsAttri := TSynHighLighterAttributes.Create(SYNS_AttrOperatorAndSymbols, SYNS_FriendlyAttrOperatorAndSymbols);
  fOperatorAndSymbolsAttri.Style := [fsBold];
  fOperatorAndSymbolsAttri.Foreground := clOlive;
  AddAttribute(fOperatorAndSymbolsAttri);

  fPredefinedAttri := TSynHighLighterAttributes.Create(SYNS_AttrPredefined, SYNS_FriendlyAttrPredefined);
  fPredefinedAttri.Style := [fsBold];
  fPredefinedAttri.Foreground := clRed;
  AddAttribute(fPredefinedAttri);

  fResultValueAttri := TSynHighLighterAttributes.Create(SYNS_AttrResultValue, SYNS_FriendlyAttrResultValue);
  fResultValueAttri.Style := [fsBold];
  fResultValueAttri.Foreground := clPurple;
  AddAttribute(fResultValueAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Style := [fsItalic];
  fStringAttri.Foreground := clGray;
  AddAttribute(fStringAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterEiffel;
  fRange := rsUnknown;
end;

procedure TSynEiffelSyn.IdentProc;
begin
  fTokenID := IdentKind(fLine + Run);
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do
    Inc(Run);
end;

procedure TSynEiffelSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynEiffelSyn.Next;
begin
  fTokenPos := Run;
  fRange := rsUnknown;
  case fLine[Run] of
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

function TSynEiffelSyn.GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
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

function TSynEiffelSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
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
  Result := fTokenId;
end;

function TSynEiffelSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkBasicTypes: Result := fBasicTypesAttri;
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkLace: Result := fLaceAttri;
    tkOperatorAndSymbols: Result := fOperatorAndSymbolsAttri;
    tkPredefined: Result := fPredefinedAttri;
    tkResultValue: Result := fResultValueAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynEiffelSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
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
  Result := fDefaultFilter <> SYNS_FilterEiffel;
end;

class function TSynEiffelSyn.GetLanguageName: string;
begin
  Result := SYNS_LangEiffel;
end;

procedure TSynEiffelSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynEiffelSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynEiffelSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
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
