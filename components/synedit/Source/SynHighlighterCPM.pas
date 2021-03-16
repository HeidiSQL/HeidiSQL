{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterCPM.pas, released 2001-08-14.
The Initial Author of this file is Pieter Polak.
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

$Id: SynHighlighterCPM.pas,v 1.16.2.6 2008/09/14 16:24:59 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

unit SynHighlighterCPM;

{$I SynEdit.inc}

interface

uses
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
  SysUtils,
  Classes;

Type
  TtkTokenKind = (
    tkComment,
    tkIdentifier,
    tkKey,
    tkNull,
    tkSpace,
    tkSQLKey,
    tkString,
    tkSymbol,
    tkSpecialVar,
    tkSystem,
    tkVariable,
    tkNumber,
    tkUnknown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TRangeState = (rsBraceComment, rsUnknown);

type
  TSynCPMSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FCommentLevel: Integer;
    FTokenID: TtkTokenKind;
    FIdentFuncTable: array[0..796] of TIdentFuncTableFunc;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FSQLKeyAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FSpecialVarAttri: TSynHighlighterAttributes;
    FSystemAttri: TSynHighlighterAttributes;
    FVariableAttri: TSynHighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function FuncAllentities(Index: Integer): TtkTokenKind;
    function FuncAllproducts(Index: Integer): TtkTokenKind;
    function FuncAllproperties(Index: Integer): TtkTokenKind;
    function FuncAllqualityproperties(Index: Integer): TtkTokenKind;
    function FuncAllsuppliers(Index: Integer): TtkTokenKind;
    function FuncAssign(Index: Integer): TtkTokenKind;
    function FuncBegin(Index: Integer): TtkTokenKind;
    function FuncBlock(Index: Integer): TtkTokenKind;
    function FuncCase(Index: Integer): TtkTokenKind;
    function FuncCategory(Index: Integer): TtkTokenKind;
    function FuncCenterstr(Index: Integer): TtkTokenKind;
    function FuncCharreplacestr(Index: Integer): TtkTokenKind;
    function FuncCharrlenstr(Index: Integer): TtkTokenKind;
    function FuncCharrllenstr(Index: Integer): TtkTokenKind;
    function FuncChr(Index: Integer): TtkTokenKind;
    function FuncClient(Index: Integer): TtkTokenKind;
    function FuncConstants(Index: Integer): TtkTokenKind;
    function FuncContinue(Index: Integer): TtkTokenKind;
    function FuncCopyfile(Index: Integer): TtkTokenKind;
    function FuncCountry(Index: Integer): TtkTokenKind;
    function FuncDecr(Index: Integer): TtkTokenKind;
    function FuncDefinition(Index: Integer): TtkTokenKind;
    function FuncDistinct_execute(Index: Integer): TtkTokenKind;
    function FuncDivide(Index: Integer): TtkTokenKind;
    function FuncElse(Index: Integer): TtkTokenKind;
    function FuncEmptysheet(Index: Integer): TtkTokenKind;
    function FuncEnd(Index: Integer): TtkTokenKind;
    function FuncEntitycode(Index: Integer): TtkTokenKind;
    function FuncEqualstring(Index: Integer): TtkTokenKind;
    function FuncEqualvalue(Index: Integer): TtkTokenKind;
    function FuncExecute(Index: Integer): TtkTokenKind;
    function FuncFileappend(Index: Integer): TtkTokenKind;
    function FuncFileassign(Index: Integer): TtkTokenKind;
    function FuncFileclose(Index: Integer): TtkTokenKind;
    function FuncFilecopy(Index: Integer): TtkTokenKind;
    function FuncFiledate(Index: Integer): TtkTokenKind;
    function FuncFiledelete(Index: Integer): TtkTokenKind;
    function FuncFileend(Index: Integer): TtkTokenKind;
    function FuncFileexists(Index: Integer): TtkTokenKind;
    function FuncFilereadln(Index: Integer): TtkTokenKind;
    function FuncFilereset(Index: Integer): TtkTokenKind;
    function FuncFilerewrite(Index: Integer): TtkTokenKind;
    function FuncFilesize(Index: Integer): TtkTokenKind;
    function FuncFilesort(Index: Integer): TtkTokenKind;
    function FuncFiletime(Index: Integer): TtkTokenKind;
    function FuncFilewriteln(Index: Integer): TtkTokenKind;
    function FuncFilterstr(Index: Integer): TtkTokenKind;
    function FuncFirstinstance(Index: Integer): TtkTokenKind;
    function FuncFlow(Index: Integer): TtkTokenKind;
    function FuncFold(Index: Integer): TtkTokenKind;
    function FuncForeign(Index: Integer): TtkTokenKind;
    function FuncGlobalconstants(Index: Integer): TtkTokenKind;
    function FuncGlobals(Index: Integer): TtkTokenKind;
    function FuncGlobalvariables(Index: Integer): TtkTokenKind;
    function FuncGroupdown(Index: Integer): TtkTokenKind;
    function FuncGroupfooter(Index: Integer): TtkTokenKind;
    function FuncGroupheader(Index: Integer): TtkTokenKind;
    function FuncGroupkey(Index: Integer): TtkTokenKind;
    function FuncGroupup(Index: Integer): TtkTokenKind;
    function FuncIf(Index: Integer): TtkTokenKind;
    function FuncInclude(Index: Integer): TtkTokenKind;
    function FuncIncr(Index: Integer): TtkTokenKind;
    function FuncLanguage(Index: Integer): TtkTokenKind;
    function FuncLastinstance(Index: Integer): TtkTokenKind;
    function FuncLeftstr(Index: Integer): TtkTokenKind;
    function FuncLength(Index: Integer): TtkTokenKind;
    function FuncLlenstr(Index: Integer): TtkTokenKind;
    function FuncLocal(Index: Integer): TtkTokenKind;
    function FuncLocasestr(Index: Integer): TtkTokenKind;
    function FuncLoop(Index: Integer): TtkTokenKind;
    function FuncLowerlevelstoo(Index: Integer): TtkTokenKind;
    function FuncLtrunc(Index: Integer): TtkTokenKind;
    function FuncMatching(Index: Integer): TtkTokenKind;
    function FuncMember(Index: Integer): TtkTokenKind;
    function FuncMerge(Index: Integer): TtkTokenKind;
    function FuncMessagedlg(Index: Integer): TtkTokenKind;
    function FuncMetaflow(Index: Integer): TtkTokenKind;
    function FuncMidstr(Index: Integer): TtkTokenKind;
    function FuncMultiply(Index: Integer): TtkTokenKind;
    function FuncNextinstance(Index: Integer): TtkTokenKind;
    function FuncNextrepeatinstance(Index: Integer): TtkTokenKind;
    function FuncOf(Index: Integer): TtkTokenKind;
    function FuncOptions(Index: Integer): TtkTokenKind;
    function FuncOrganisation(Index: Integer): TtkTokenKind;
    function FuncOutput(Index: Integer): TtkTokenKind;
    function FuncParam(Index: Integer): TtkTokenKind;
    function FuncParent(Index: Integer): TtkTokenKind;
    function FuncParseinc(Index: Integer): TtkTokenKind;
    function FuncPdriver(Index: Integer): TtkTokenKind;
    function FuncPrevinstance(Index: Integer): TtkTokenKind;
    function FuncPrevrepeatinstance(Index: Integer): TtkTokenKind;
    function FuncPrinter(Index: Integer): TtkTokenKind;
    function FuncPrintfile(Index: Integer): TtkTokenKind;
    function FuncPropertygroup(Index: Integer): TtkTokenKind;
    function FuncRastr(Index: Integer): TtkTokenKind;
    function FuncRaval(Index: Integer): TtkTokenKind;
    function FuncReadinstance(Index: Integer): TtkTokenKind;
    function FuncReadrepeatinstance(Index: Integer): TtkTokenKind;
    function FuncRepeat(Index: Integer): TtkTokenKind;
    function FuncRepeatcount(Index: Integer): TtkTokenKind;
    function FuncReportlevel(Index: Integer): TtkTokenKind;
    function FuncRightstr(Index: Integer): TtkTokenKind;
    function FuncRlenstr(Index: Integer): TtkTokenKind;
    function FuncRoot(Index: Integer): TtkTokenKind;
    function FuncRound(Index: Integer): TtkTokenKind;
    function FuncShowmessage(Index: Integer): TtkTokenKind;
    function FuncSkipemtpty(Index: Integer): TtkTokenKind;
    function FuncSortdown(Index: Integer): TtkTokenKind;
    function FuncSortkey(Index: Integer): TtkTokenKind;
    function FuncSortup(Index: Integer): TtkTokenKind;
    function FuncSql_add(Index: Integer): TtkTokenKind;
    function FuncSql_asfloat(Index: Integer): TtkTokenKind;
    function FuncSql_asstring(Index: Integer): TtkTokenKind;
    function FuncSql_create(Index: Integer): TtkTokenKind;
    function FuncSql_dump(Index: Integer): TtkTokenKind;
    function FuncSql_eof(Index: Integer): TtkTokenKind;
    function FuncSql_execute(Index: Integer): TtkTokenKind;
    function FuncSql_free(Index: Integer): TtkTokenKind;
    function FuncSql_mladd(Index: Integer): TtkTokenKind;
    function FuncSql_mlmultiadd(Index: Integer): TtkTokenKind;
    function FuncSql_next(Index: Integer): TtkTokenKind;
    function FuncSql_setvar(Index: Integer): TtkTokenKind;
    function FuncSqr(Index: Integer): TtkTokenKind;
    function FuncStripstr(Index: Integer): TtkTokenKind;
    function FuncStroptions(Index: Integer): TtkTokenKind;
    function FuncStrpos(Index: Integer): TtkTokenKind;
    function FuncSubtract(Index: Integer): TtkTokenKind;
    function FuncSum(Index: Integer): TtkTokenKind;
    function FuncSupplier(Index: Integer): TtkTokenKind;
    function FuncSuppliesofmembers(Index: Integer): TtkTokenKind;
    function FuncThen(Index: Integer): TtkTokenKind;
    function FuncTrunc(Index: Integer): TtkTokenKind;
    function FuncUpcasestr(Index: Integer): TtkTokenKind;
    function FuncUsedby(Index: Integer): TtkTokenKind;
    function FuncV_date(Index: Integer): TtkTokenKind;
    function FuncV_false(Index: Integer): TtkTokenKind;
    function FuncV_nonereal(Index: Integer): TtkTokenKind;
    function FuncV_par_language(Index: Integer): TtkTokenKind;
    function FuncV_par_language_count(Index: Integer): TtkTokenKind;
    function FuncV_par_language_fields(Index: Integer): TtkTokenKind;
    function FuncV_time(Index: Integer): TtkTokenKind;
    function FuncV_true(Index: Integer): TtkTokenKind;
    function FuncVariables(Index: Integer): TtkTokenKind;
    function FuncVaroptions(Index: Integer): TtkTokenKind;
    function FuncWhile(Index: Integer): TtkTokenKind;
    function FuncZerorlenstr(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure CRProc;
    procedure LFProc;
    procedure SemiColonProc;
    procedure SymbolProc;
    procedure NumberProc;
    procedure BraceOpenProc;
    procedure IdentProc;
    procedure VariableProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure BraceCommentProc;
  protected
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
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property SQLKeyAttri: TSynHighlighterAttributes read FSQLKeyAttri write FSQLKeyAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property SpecialVarAttri: TSynHighlighterAttributes read FSpecialVarAttri write FSpecialVarAttri;
    property SystemAttri: TSynHighlighterAttributes read FSystemAttri write FSystemAttri;
    property VariableAttri: TSynHighlighterAttributes read FVariableAttri write FVariableAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  KeyWords: array[0..145] of UnicodeString = (
    'allentities', 'allproducts', 'allproperties', 'allqualityproperties', 
    'allsuppliers', 'assign', 'begin', 'block', 'case', 'category', 'centerstr', 
    'charreplacestr', 'charrlenstr', 'charrllenstr', 'chr', 'client', 
    'constants', 'continue', 'copyfile', 'country', 'decr', 'definition', 
    'distinct_execute', 'divide', 'else', 'emptysheet', 'end', 'entitycode', 
    'equalstring', 'equalvalue', 'execute', 'fileappend', 'fileassign', 
    'fileclose', 'filecopy', 'filedate', 'filedelete', 'fileend', 'fileexists', 
    'filereadln', 'filereset', 'filerewrite', 'filesize', 'filesort', 
    'filetime', 'filewriteln', 'filterstr', 'firstinstance', 'flow', 'fold', 
    'foreign', 'globalconstants', 'globals', 'globalvariables', 'groupdown', 
    'groupfooter', 'groupheader', 'groupkey', 'groupup', 'if', 'include', 
    'incr', 'language', 'lastinstance', 'leftstr', 'length', 'llenstr', 'local', 
    'locasestr', 'loop', 'lowerlevelstoo', 'ltrunc', 'matching', 'member', 
    'merge', 'messagedlg', 'metaflow', 'midstr', 'multiply', 'nextinstance', 
    'nextrepeatinstance', 'of', 'options', 'organisation', 'output', 'param', 
    'parent', 'parseinc', 'pdriver', 'previnstance', 'prevrepeatinstance', 
    'printer', 'printfile', 'propertygroup', 'rastr', 'raval', 'readinstance', 
    'readrepeatinstance', 'repeat', 'repeatcount', 'reportlevel', 'rightstr', 
    'rlenstr', 'root', 'round', 'showmessage', 'skipemtpty', 'sortdown', 
    'sortkey', 'sortup', 'sql_add', 'sql_asfloat', 'sql_asstring', 'sql_create', 
    'sql_dump', 'sql_eof', 'sql_execute', 'sql_free', 'sql_mladd', 
    'sql_mlmultiadd', 'sql_next', 'sql_setvar', 'sqr', 'stripstr', 'stroptions', 
    'strpos', 'subtract', 'sum', 'supplier', 'suppliesofmembers', 'then', 
    'trunc', 'upcasestr', 'usedby', 'v_date', 'v_false', 'v_nonereal', 
    'v_par_language', 'v_par_language_count', 'v_par_language_fields', 'v_time', 
    'v_true', 'variables', 'varoptions', 'while', 'zerorlenstr' 
  );

  KeyIndices: array[0..796] of Integer = (
    -1, -1, -1, -1, -1, -1, -1, -1, 45, -1, 26, -1, -1, -1, -1, -1, 74, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 25, 85, -1, -1, -1, 58, -1, 51, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 4, 43, 30, -1, 54, 127, -1, -1, -1, 136, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 56, 38, -1, 32, -1, -1, -1, -1, -1, -1, 
    -1, 133, 65, -1, 96, -1, -1, -1, 144, -1, -1, -1, -1, -1, -1, -1, -1, 89, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 10, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 23, -1, -1, -1, 35, -1, -1, 5, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 75, 41, -1, -1, 36, -1, -1, -1, -1, -1, -1, 143, -1, 
    -1, 105, -1, -1, -1, -1, -1, 86, 142, 99, -1, 131, -1, -1, -1, -1, -1, -1, 
    8, -1, -1, -1, -1, 83, -1, -1, 67, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 53, 27, -1, -1, -1, -1, -1, -1, 102, -1, -1, 
    -1, -1, -1, -1, -1, 2, -1, -1, 28, -1, 24, 141, -1, -1, 101, -1, -1, 134, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 111, -1, 100, -1, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 44, 135, -1, 117, -1, 77, -1, 37, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 69, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 42, 7, -1, 
    109, -1, -1, -1, -1, -1, -1, -1, 107, -1, -1, -1, 113, -1, -1, 0, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 13, -1, 73, 34, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 132, -1, -1, -1, 123, -1, -1, -1, -1, -1, 
    63, -1, 48, -1, -1, -1, -1, -1, -1, -1, -1, -1, 140, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 66, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 84, -1, 
    -1, -1, -1, 95, -1, -1, -1, -1, -1, -1, -1, 71, 138, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 93, 110, -1, -1, 80, -1, -1, 137, -1, -1, -1, 91, -1, 60, -1, 
    -1, 62, -1, -1, -1, -1, -1, -1, -1, -1, -1, 82, -1, -1, -1, -1, -1, 29, -1, 
    -1, 122, -1, -1, -1, -1, 39, -1, 61, -1, -1, -1, -1, -1, 6, -1, -1, -1, -1, 
    -1, -1, 22, 130, -1, -1, -1, -1, -1, 81, -1, 57, -1, -1, 20, 121, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 94, -1, 31, -1, -1, -1, -1, 
    -1, 47, -1, -1, 108, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 33, -1, 
    -1, -1, 64, -1, -1, 1, -1, 118, -1, -1, -1, -1, -1, -1, 87, 49, -1, -1, -1, 
    -1, -1, 79, -1, -1, -1, -1, -1, -1, 119, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    46, -1, -1, -1, -1, 125, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    106, -1, 97, -1, 68, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 40, -1, -1, 72, 70, 88, -1, 12, -1, -1, -1, -1, -1, -1, -1, 124, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 114, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 92, 
    -1, -1, 59, -1, -1, -1, -1, -1, 11, -1, -1, 104, -1, -1, -1, -1, -1, -1, -1, 
    18, 78, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 17, -1, 129, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 112, -1, -1, 98, -1, 116, 120, -1, 21, -1, 9, -1, 
    -1, -1, 19, -1, -1, -1, 50, -1, -1, -1, 126, -1, -1, 55, -1, 145, -1, -1, 
    -1, -1, 52, 139, -1, 14, -1, -1, 115, -1, -1, -1, 90, -1, -1, -1, 128, -1, 
    -1, -1, 103, -1, -1, -1, -1, -1, 3, -1, -1, 76, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 16, -1, -1, -1 
  );

{$Q-}
function TSynCPMSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 841 + Ord(Str^) * 268;
    Inc(Str);
  end;
  Result := Result mod 797;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynCPMSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynCPMSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[314] := FuncAllentities;
  FIdentFuncTable[528] := FuncAllproducts;
  FIdentFuncTable[212] := FuncAllproperties;
  FIdentFuncTable[774] := FuncAllqualityproperties;
  FIdentFuncTable[46] := FuncAllsuppliers;
  FIdentFuncTable[127] := FuncAssign;
  FIdentFuncTable[462] := FuncBegin;
  FIdentFuncTable[297] := FuncBlock;
  FIdentFuncTable[169] := FuncCase;
  FIdentFuncTable[728] := FuncCategory;
  FIdentFuncTable[106] := FuncCenterstr;
  FIdentFuncTable[663] := FuncCharreplacestr;
  FIdentFuncTable[607] := FuncCharrlenstr;
  FIdentFuncTable[326] := FuncCharrllenstr;
  FIdentFuncTable[753] := FuncChr;
  FIdentFuncTable[251] := FuncClient;
  FIdentFuncTable[793] := FuncConstants;
  FIdentFuncTable[694] := FuncContinue;
  FIdentFuncTable[674] := FuncCopyfile;
  FIdentFuncTable[732] := FuncCountry;
  FIdentFuncTable[481] := FuncDecr;
  FIdentFuncTable[726] := FuncDefinition;
  FIdentFuncTable[469] := FuncDistinct_execute;
  FIdentFuncTable[120] := FuncDivide;
  FIdentFuncTable[217] := FuncElse;
  FIdentFuncTable[26] := FuncEmptysheet;
  FIdentFuncTable[10] := FuncEnd;
  FIdentFuncTable[197] := FuncEntitycode;
  FIdentFuncTable[215] := FuncEqualstring;
  FIdentFuncTable[446] := FuncEqualvalue;
  FIdentFuncTable[48] := FuncExecute;
  FIdentFuncTable[499] := FuncFileappend;
  FIdentFuncTable[69] := FuncFileassign;
  FIdentFuncTable[521] := FuncFileclose;
  FIdentFuncTable[329] := FuncFilecopy;
  FIdentFuncTable[124] := FuncFiledate;
  FIdentFuncTable[142] := FuncFiledelete;
  FIdentFuncTable[273] := FuncFileend;
  FIdentFuncTable[67] := FuncFileexists;
  FIdentFuncTable[454] := FuncFilereadln;
  FIdentFuncTable[600] := FuncFilereset;
  FIdentFuncTable[139] := FuncFilerewrite;
  FIdentFuncTable[296] := FuncFilesize;
  FIdentFuncTable[47] := FuncFilesort;
  FIdentFuncTable[266] := FuncFiletime;
  FIdentFuncTable[8] := FuncFilewriteln;
  FIdentFuncTable[561] := FuncFilterstr;
  FIdentFuncTable[505] := FuncFirstinstance;
  FIdentFuncTable[356] := FuncFlow;
  FIdentFuncTable[538] := FuncFold;
  FIdentFuncTable[736] := FuncForeign;
  FIdentFuncTable[33] := FuncGlobalconstants;
  FIdentFuncTable[750] := FuncGlobals;
  FIdentFuncTable[196] := FuncGlobalvariables;
  FIdentFuncTable[50] := FuncGroupdown;
  FIdentFuncTable[743] := FuncGroupfooter;
  FIdentFuncTable[66] := FuncGroupheader;
  FIdentFuncTable[478] := FuncGroupkey;
  FIdentFuncTable[31] := FuncGroupup;
  FIdentFuncTable[657] := FuncIf;
  FIdentFuncTable[427] := FuncInclude;
  FIdentFuncTable[456] := FuncIncr;
  FIdentFuncTable[430] := FuncLanguage;
  FIdentFuncTable[354] := FuncLastinstance;
  FIdentFuncTable[525] := FuncLeftstr;
  FIdentFuncTable[78] := FuncLength;
  FIdentFuncTable[379] := FuncLlenstr;
  FIdentFuncTable[177] := FuncLocal;
  FIdentFuncTable[583] := FuncLocasestr;
  FIdentFuncTable[285] := FuncLoop;
  FIdentFuncTable[604] := FuncLowerlevelstoo;
  FIdentFuncTable[403] := FuncLtrunc;
  FIdentFuncTable[603] := FuncMatching;
  FIdentFuncTable[328] := FuncMember;
  FIdentFuncTable[16] := FuncMerge;
  FIdentFuncTable[138] := FuncMessagedlg;
  FIdentFuncTable[777] := FuncMetaflow;
  FIdentFuncTable[271] := FuncMidstr;
  FIdentFuncTable[675] := FuncMultiply;
  FIdentFuncTable[544] := FuncNextinstance;
  FIdentFuncTable[418] := FuncNextrepeatinstance;
  FIdentFuncTable[476] := FuncOf;
  FIdentFuncTable[440] := FuncOptions;
  FIdentFuncTable[174] := FuncOrganisation;
  FIdentFuncTable[390] := FuncOutput;
  FIdentFuncTable[27] := FuncParam;
  FIdentFuncTable[158] := FuncParent;
  FIdentFuncTable[537] := FuncParseinc;
  FIdentFuncTable[605] := FuncPdriver;
  FIdentFuncTable[93] := FuncPrevinstance;
  FIdentFuncTable[760] := FuncPrevrepeatinstance;
  FIdentFuncTable[425] := FuncPrinter;
  FIdentFuncTable[654] := FuncPrintfile;
  FIdentFuncTable[414] := FuncPropertygroup;
  FIdentFuncTable[497] := FuncRastr;
  FIdentFuncTable[395] := FuncRaval;
  FIdentFuncTable[80] := FuncReadinstance;
  FIdentFuncTable[581] := FuncReadrepeatinstance;
  FIdentFuncTable[721] := FuncRepeat;
  FIdentFuncTable[160] := FuncRepeatcount;
  FIdentFuncTable[249] := FuncReportlevel;
  FIdentFuncTable[221] := FuncRightstr;
  FIdentFuncTable[204] := FuncRlenstr;
  FIdentFuncTable[768] := FuncRoot;
  FIdentFuncTable[666] := FuncRound;
  FIdentFuncTable[152] := FuncShowmessage;
  FIdentFuncTable[579] := FuncSkipemtpty;
  FIdentFuncTable[307] := FuncSortdown;
  FIdentFuncTable[508] := FuncSortkey;
  FIdentFuncTable[299] := FuncSortup;
  FIdentFuncTable[415] := FuncSql_add;
  FIdentFuncTable[247] := FuncSql_asfloat;
  FIdentFuncTable[718] := FuncSql_asstring;
  FIdentFuncTable[311] := FuncSql_create;
  FIdentFuncTable[635] := FuncSql_dump;
  FIdentFuncTable[756] := FuncSql_eof;
  FIdentFuncTable[723] := FuncSql_execute;
  FIdentFuncTable[269] := FuncSql_free;
  FIdentFuncTable[530] := FuncSql_mladd;
  FIdentFuncTable[551] := FuncSql_mlmultiadd;
  FIdentFuncTable[724] := FuncSql_next;
  FIdentFuncTable[482] := FuncSql_setvar;
  FIdentFuncTable[449] := FuncSqr;
  FIdentFuncTable[348] := FuncStripstr;
  FIdentFuncTable[615] := FuncStroptions;
  FIdentFuncTable[566] := FuncStrpos;
  FIdentFuncTable[740] := FuncSubtract;
  FIdentFuncTable[51] := FuncSum;
  FIdentFuncTable[764] := FuncSupplier;
  FIdentFuncTable[696] := FuncSuppliesofmembers;
  FIdentFuncTable[470] := FuncThen;
  FIdentFuncTable[162] := FuncTrunc;
  FIdentFuncTable[344] := FuncUpcasestr;
  FIdentFuncTable[77] := FuncUsedby;
  FIdentFuncTable[224] := FuncV_date;
  FIdentFuncTable[267] := FuncV_false;
  FIdentFuncTable[55] := FuncV_nonereal;
  FIdentFuncTable[421] := FuncV_par_language;
  FIdentFuncTable[404] := FuncV_par_language_count;
  FIdentFuncTable[751] := FuncV_par_language_fields;
  FIdentFuncTable[366] := FuncV_time;
  FIdentFuncTable[218] := FuncV_true;
  FIdentFuncTable[159] := FuncVariables;
  FIdentFuncTable[149] := FuncVaroptions;
  FIdentFuncTable[84] := FuncWhile;
  FIdentFuncTable[745] := FuncZerorlenstr;
end;

function TSynCPMSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynCPMSyn.FuncAllentities(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncAllproducts(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncAllproperties(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncAllqualityproperties(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncAllsuppliers(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncAssign(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncBegin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncBlock(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncCase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncCategory(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncCenterstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncCharreplacestr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncCharrlenstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncCharrllenstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncChr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncClient(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncConstants(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncContinue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncCopyfile(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncCountry(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncDecr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncDefinition(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncDistinct_execute(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncDivide(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncElse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncEmptysheet(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncEnd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncEntitycode(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncEqualstring(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncEqualvalue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncExecute(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncFileappend(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncFileassign(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncFileclose(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncFilecopy(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncFiledate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncFiledelete(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncFileend(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncFileexists(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncFilereadln(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncFilereset(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncFilerewrite(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncFilesize(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncFilesort(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncFiletime(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncFilewriteln(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncFilterstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncFirstinstance(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncFlow(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncFold(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncForeign(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncGlobalconstants(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncGlobals(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncGlobalvariables(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncGroupdown(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncGroupfooter(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncGroupheader(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncGroupkey(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncGroupup(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncIf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncInclude(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncIncr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncLanguage(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncLastinstance(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncLeftstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncLength(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncLlenstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncLocal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncLocasestr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncLoop(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncLowerlevelstoo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncLtrunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncMatching(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncMember(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncMerge(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncMessagedlg(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncMetaflow(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncMidstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncMultiply(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncNextinstance(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncNextrepeatinstance(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncOf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncOptions(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncOrganisation(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncOutput(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncParam(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncParent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncParseinc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncPdriver(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncPrevinstance(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncPrevrepeatinstance(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncPrinter(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncPrintfile(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncPropertygroup(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncRastr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncRaval(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncReadinstance(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncReadrepeatinstance(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncRepeat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncRepeatcount(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncReportlevel(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncRightstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncRlenstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncRoot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncRound(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncShowmessage(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncSkipemtpty(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncSortdown(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncSortkey(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncSortup(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncSql_add(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncSql_asfloat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncSql_asstring(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncSql_create(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncSql_dump(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncSql_eof(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncSql_execute(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncSql_free(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncSql_mladd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncSql_mlmultiadd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncSql_next(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncSql_setvar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSQLKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncSqr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncStripstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncStroptions(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncStrpos(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncSubtract(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncSum(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncSupplier(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncSuppliesofmembers(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncThen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncTrunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncUpcasestr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncUsedby(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncV_date(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSpecialVar
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncV_false(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSpecialVar
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncV_nonereal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSpecialVar
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncV_par_language(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSpecialVar
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncV_par_language_count(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSpecialVar
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncV_par_language_fields(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSpecialVar
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncV_time(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSpecialVar
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncV_true(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSpecialVar
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncVariables(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncVaroptions(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncWhile(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.FuncZerorlenstr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSystem
  else
    Result := tkIdentifier;
end;

constructor TSynCPMSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clNavy;
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Foreground := clGreen;
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);
  
  FSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FSQLKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrSQLKey, SYNS_FriendlyAttrSQLKey);
  FSQLKeyAttri.ForeGround := clTeal;
  FSQLKeyAttri.Style := [fsBold];
  AddAttribute(FSQLKeyAttri);

  FStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);

  FSymbolAttri := TSynHighLighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  FSpecialVarAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpecialVariable, SYNS_FriendlyAttrSpecialVariable);
  FSpecialVarAttri.Style := [fsBold];
  AddAttribute(FSpecialVarAttri);

  FSystemAttri := TSynHighlighterAttributes.Create(SYNS_AttrSystem, SYNS_FriendlyAttrSystem);
  FSystemAttri.Foreground := $000080FF;
  FSystemAttri.Style := [fsBold];
  AddAttribute(FSystemAttri);

  FVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  FVariableAttri.Foreground := clMaroon;
  AddAttribute(FVariableAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  FRange := rsUnknown;
  FCommentLevel := 0;
  FDefaultFilter := SYNS_FilterCPM;
end; { Create }

procedure TSynCPMSyn.BraceOpenProc;
begin
  FRange := rsBraceComment;
  BraceCommentProc;
  FTokenID := tkComment;
end; { BraceOpenProc }

procedure TSynCPMSyn.IdentProc;
begin
  FTokenID := IdentKind(FLine + Run);
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
end; { IdentProc }

procedure TSynCPMSyn.VariableProc;
begin
  FTokenID := IdentKind((FLine + Run));
  if (FTokenID = tkIdentifier) then
  begin
    if (FLine[Run + 1] = '_') then
      FTokenID := tkVariable
  end;
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
end; { VariableProc }

procedure TSynCPMSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end; { NullProc }

procedure TSynCPMSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end; { SpaceProc }

procedure TSynCPMSyn.StringProc;
begin
  FTokenID := tkString;
  repeat
    Inc(Run);
  until IsLineEnd(Run) or (FLine[Run] = '"');
  if (FLine[Run] = '"') then
  begin
    Inc(Run);
    if (FLine[Run] = '"') then
      Inc(Run);
  end;
end; { StringProc }

procedure TSynCPMSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end; { UnknownProc }

procedure TSynCPMSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsBraceComment: BraceCommentProc;
  else
    case FLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      '"': StringProc;
      '0'..'9': NumberProc;
      'A'..'Z', 'a'..'z', '_':
        case FLine[Run] of
          'V', 'v', 'S', 's': VariableProc;
          else
            IdentProc;
        end;
      '{': BraceOpenProc;
      '}', '!', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~':
      begin
        case FLine[Run] of
          ';': SemiColonProc;
          else
            SymbolProc;
        end;
      end;
    else
      UnknownProc;
    end;
  end;
  inherited;
end; { Next }

function TSynCPMSyn.GetDefaultAttribute(Index: Integer): TSynHighLighterAttributes;
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
end; { GetDefaultAttribute }

function TSynCPMSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end; { GetEol }

function TSynCPMSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end; { GetTokenID }

function TSynCPMSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkSQLKey: Result := FSQLKeyAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkSpecialVar: Result := FSpecialVarAttri;
    tkSystem: Result := FSystemAttri;
    tkVariable: Result := FVariableAttri;
    tkUnknown: Result := FIdentifierAttri;
  else
    Result := nil;
  end;
end; { GetTokenAttribute }

function TSynCPMSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end; { GetTokenKind }

class function TSynCPMSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCPM;
end;

procedure TSynCPMSyn.BraceCommentProc;
begin
  case FLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      FTokenID := tkComment;
      repeat
        if FLine[Run] = '{' then
          Inc(FCommentLevel)
        else if FLine[Run] = '}' then
        begin
          Dec(FCommentLevel);
          if (FCommentLevel < 1) then
          begin
            Inc(Run);
            FRange := rsUnknown;
            FCommentLevel := 0;
            Break;
          end;
        end;
        Inc(Run);
      until IsLineEnd(Run);
    end;
  end;
end; { BraceCommentProc }

procedure TSynCPMSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end; { CRProc }

procedure TSynCPMSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end; { LFProc }

function TSynCPMSyn.GetSampleSource: UnicodeString;
begin
  Result := '{ COAS Product Manager report (RDF) }'#13#10 +
            'PARAM'#13#10 +
            '  LANGUAGE;'#13#10 +
            '  CONTINUE;'#13#10 +
            'END; { Param }'#13#10 +
            #13#10 +
            'GLOBALS'#13#10 +
            '  LANGUAGE = LOCAL;'#13#10 +
            'END; { Globals }'#13#10 +
            #13#10 +
            'DEFINITION BLOCK "MAIN"'#13#10 +
            'VARIABLES'#13#10 +
            '  S_Query = "";'#13#10 +
            '  V_OraErr = -1;'#13#10 +
            '  V_Count;'#13#10 +
            'BEGIN'#13#10 +
            '  ASSIGN(S_Query, "SELECT * FROM DUAL");'#13#10 +
            '  SQL_CREATE(V_OraErr, S_Query);'#13#10 +
            '  ASSIGN(V_Count, V_NoneReal);'#13#10 +
            'END;';
end; { GetSampleSource }

function TSynCPMSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterCPM;
end; { IsFilterStored }

procedure TSynCPMSyn.SemiColonProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end; { SemiColonProc }

procedure TSynCPMSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', '.', 'e', 'E':
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
      '.': if FLine[Run + 1] = '.' then
             Break;
    end;
    Inc(Run);
  end;
end; { NumberProc }

procedure TSynCPMSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end; { SymbolProc }

procedure TSynCPMSyn.ResetRange;
begin
  inherited;
  FRange := rsUnknown;
  FCommentLevel := 0;
end; { ResetRange }

procedure TSynCPMSyn.SetRange(Value: Pointer);
var
  AValue: LongInt;
begin
  inherited;
  AValue := Longint(Value);
  FCommentLevel := AValue div $10000;
  FRange := TRangeState(AValue mod $10000);
end; { SetRange }

function TSynCPMSyn.GetRange: Pointer;
begin
  Result := Pointer((FCommentLevel * $10000) + Integer(FRange));
end; { GetRange }

class function TSynCPMSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangCPM;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynCPMSyn);
{$ENDIF}
end.
