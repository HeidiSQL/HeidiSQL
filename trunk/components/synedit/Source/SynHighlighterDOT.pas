{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: SynHighlighterDOT.pas, released 2002-11-30.
Description: DOT Syntax Parser/Highlighter
The initial author of this file is nissl (nissl@tiscali.it, nissl@mammuth.it)
Unicode translation by Maël Hörz.
Copyright (c) 2002, all rights reserved.

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

$Id: SynHighlighterDOT.pas,v 1.3.2.6 2005/11/27 22:22:44 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}
{
@abstract(Provides a ATT DOT highlighter for SynEdit)
@author(Massimo Maria Ghisalberti (nissl@mammuth.it))
@created(november 2002)
@lastmod(2002-11-30)
The SynHighlighterDOT unit provides SynEdit with a DOT Graph Drawing (.dot) highlighter.
The highlighter formats DOT source code ref.: http://www.research.att.com/sw/tools/graphviz/.
}

{$IFNDEF QSYNHIGHLIGHTERDOT}
unit SynHighlighterDOT;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QControls,
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
{$ELSE}
  Windows,
  Controls,
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (
    tkArrowHead,
    tkAttribute,
    tkComment,
    tkDirections,
    tkIdentifier,
    tkKey,
    tkNull,
    tkShape,
    tkSpace,
    tkString,
    tkUnknown,
    tkValue,
    tkSymbol);

  TRangeState = (rsUnKnown, rsCStyleComment, rsString);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynDOTSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..786] of TIdentFuncTableFunc;
    fArrowHeadAttri: TSynHighlighterAttributes;
    fAttributeAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fDirectionsAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fShapeAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fValueAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function FuncAll(Index: Integer): TtkTokenKind;
    function FuncAppendix(Index: Integer): TtkTokenKind;
    function FuncArrowhead(Index: Integer): TtkTokenKind;
    function FuncArrowsize(Index: Integer): TtkTokenKind;
    function FuncArrowtail(Index: Integer): TtkTokenKind;
    function FuncAuto(Index: Integer): TtkTokenKind;
    function FuncBack(Index: Integer): TtkTokenKind;
    function FuncBgcolor(Index: Integer): TtkTokenKind;
    function FuncBold(Index: Integer): TtkTokenKind;
    function FuncBoth(Index: Integer): TtkTokenKind;
    function FuncBottomlabel(Index: Integer): TtkTokenKind;
    function FuncBox(Index: Integer): TtkTokenKind;
    function FuncCenter(Index: Integer): TtkTokenKind;
    function FuncCircle(Index: Integer): TtkTokenKind;
    function FuncClusterrank(Index: Integer): TtkTokenKind;
    function FuncColor(Index: Integer): TtkTokenKind;
    function FuncComment(Index: Integer): TtkTokenKind;
    function FuncCompound(Index: Integer): TtkTokenKind;
    function FuncConcentrate(Index: Integer): TtkTokenKind;
    function FuncConstraint(Index: Integer): TtkTokenKind;
    function FuncDecorate(Index: Integer): TtkTokenKind;
    function FuncDiamond(Index: Integer): TtkTokenKind;
    function FuncDigraph(Index: Integer): TtkTokenKind;
    function FuncDir(Index: Integer): TtkTokenKind;
    function FuncDistortion(Index: Integer): TtkTokenKind;
    function FuncDot(Index: Integer): TtkTokenKind;
    function FuncDotted(Index: Integer): TtkTokenKind;
    function FuncDoublecircle(Index: Integer): TtkTokenKind;
    function FuncDoubleoctagon(Index: Integer): TtkTokenKind;
    function FuncE(Index: Integer): TtkTokenKind;
    function FuncEdge(Index: Integer): TtkTokenKind;
    function FuncEgg(Index: Integer): TtkTokenKind;
    function FuncEllipse(Index: Integer): TtkTokenKind;
    function FuncFalse(Index: Integer): TtkTokenKind;
    function FuncFill(Index: Integer): TtkTokenKind;
    function FuncFillcolor(Index: Integer): TtkTokenKind;
    function FuncFilled(Index: Integer): TtkTokenKind;
    function FuncFixedsize(Index: Integer): TtkTokenKind;
    function FuncFontcolor(Index: Integer): TtkTokenKind;
    function FuncFontname(Index: Integer): TtkTokenKind;
    function FuncFontpath(Index: Integer): TtkTokenKind;
    function FuncFontsize(Index: Integer): TtkTokenKind;
    function FuncForward(Index: Integer): TtkTokenKind;
    function FuncGlobal(Index: Integer): TtkTokenKind;
    function FuncGraph(Index: Integer): TtkTokenKind;
    function FuncGroup(Index: Integer): TtkTokenKind;
    function FuncHeadlabel(Index: Integer): TtkTokenKind;
    function FuncHeadport(Index: Integer): TtkTokenKind;
    function FuncHeadurl(Index: Integer): TtkTokenKind;
    function FuncHeight(Index: Integer): TtkTokenKind;
    function FuncHexagon(Index: Integer): TtkTokenKind;
    function FuncHouse(Index: Integer): TtkTokenKind;
    function FuncId(Index: Integer): TtkTokenKind;
    function FuncInv(Index: Integer): TtkTokenKind;
    function FuncInvdot(Index: Integer): TtkTokenKind;
    function FuncInvhouse(Index: Integer): TtkTokenKind;
    function FuncInvodot(Index: Integer): TtkTokenKind;
    function FuncInvtrapezium(Index: Integer): TtkTokenKind;
    function FuncInvtriangle(Index: Integer): TtkTokenKind;
    function FuncLabel(Index: Integer): TtkTokenKind;
    function FuncLabelangle(Index: Integer): TtkTokenKind;
    function FuncLabeldistance(Index: Integer): TtkTokenKind;
    function FuncLabelfloat(Index: Integer): TtkTokenKind;
    function FuncLabelfontcolor(Index: Integer): TtkTokenKind;
    function FuncLabelfontname(Index: Integer): TtkTokenKind;
    function FuncLabelfontsize(Index: Integer): TtkTokenKind;
    function FuncLabeljust(Index: Integer): TtkTokenKind;
    function FuncLabelloc(Index: Integer): TtkTokenKind;
    function FuncLayer(Index: Integer): TtkTokenKind;
    function FuncLayers(Index: Integer): TtkTokenKind;
    function FuncLhead(Index: Integer): TtkTokenKind;
    function FuncLtail(Index: Integer): TtkTokenKind;
    function FuncMargin(Index: Integer): TtkTokenKind;
    function FuncMax(Index: Integer): TtkTokenKind;
    function FuncMcircle(Index: Integer): TtkTokenKind;
    function FuncMclimit(Index: Integer): TtkTokenKind;
    function FuncMdiamond(Index: Integer): TtkTokenKind;
    function FuncMerged(Index: Integer): TtkTokenKind;
    function FuncMin(Index: Integer): TtkTokenKind;
    function FuncMinimum(Index: Integer): TtkTokenKind;
    function FuncMinlen(Index: Integer): TtkTokenKind;
    function FuncMrecord(Index: Integer): TtkTokenKind;
    function FuncMsquare(Index: Integer): TtkTokenKind;
    function FuncMultiples(Index: Integer): TtkTokenKind;
    function FuncN(Index: Integer): TtkTokenKind;
    function FuncNe(Index: Integer): TtkTokenKind;
    function FuncNode(Index: Integer): TtkTokenKind;
    function FuncNodesep(Index: Integer): TtkTokenKind;
    function FuncNone(Index: Integer): TtkTokenKind;
    function FuncNormal(Index: Integer): TtkTokenKind;
    function FuncNslimit(Index: Integer): TtkTokenKind;
    function FuncNw(Index: Integer): TtkTokenKind;
    function FuncOctagon(Index: Integer): TtkTokenKind;
    function FuncOdot(Index: Integer): TtkTokenKind;
    function FuncOnto(Index: Integer): TtkTokenKind;
    function FuncOrdering(Index: Integer): TtkTokenKind;
    function FuncOrientation(Index: Integer): TtkTokenKind;
    function FuncPage(Index: Integer): TtkTokenKind;
    function FuncPagedir(Index: Integer): TtkTokenKind;
    function FuncParallelogram(Index: Integer): TtkTokenKind;
    function FuncPeripheries(Index: Integer): TtkTokenKind;
    function FuncPlaintext(Index: Integer): TtkTokenKind;
    function FuncPoint(Index: Integer): TtkTokenKind;
    function FuncPolygon(Index: Integer): TtkTokenKind;
    function FuncQuantum(Index: Integer): TtkTokenKind;
    function FuncRank(Index: Integer): TtkTokenKind;
    function FuncRankdir(Index: Integer): TtkTokenKind;
    function FuncRanksep(Index: Integer): TtkTokenKind;
    function FuncRatio(Index: Integer): TtkTokenKind;
    function FuncRecord(Index: Integer): TtkTokenKind;
    function FuncRegular(Index: Integer): TtkTokenKind;
    function FuncRemincross(Index: Integer): TtkTokenKind;
    function FuncRotate(Index: Integer): TtkTokenKind;
    function FuncS(Index: Integer): TtkTokenKind;
    function FuncSame(Index: Integer): TtkTokenKind;
    function FuncSamehead(Index: Integer): TtkTokenKind;
    function FuncSametail(Index: Integer): TtkTokenKind;
    function FuncSamplepoints(Index: Integer): TtkTokenKind;
    function FuncSe(Index: Integer): TtkTokenKind;
    function FuncSearchsize(Index: Integer): TtkTokenKind;
    function FuncSection(Index: Integer): TtkTokenKind;
    function FuncShape(Index: Integer): TtkTokenKind;
    function FuncShapefile(Index: Integer): TtkTokenKind;
    function FuncSides(Index: Integer): TtkTokenKind;
    function FuncSink(Index: Integer): TtkTokenKind;
    function FuncSize(Index: Integer): TtkTokenKind;
    function FuncSkew(Index: Integer): TtkTokenKind;
    function FuncSource(Index: Integer): TtkTokenKind;
    function FuncStrict(Index: Integer): TtkTokenKind;
    function FuncStyle(Index: Integer): TtkTokenKind;
    function FuncSubgraph(Index: Integer): TtkTokenKind;
    function FuncSw(Index: Integer): TtkTokenKind;
    function FuncTaillabel(Index: Integer): TtkTokenKind;
    function FuncTailport(Index: Integer): TtkTokenKind;
    function FuncTailurl(Index: Integer): TtkTokenKind;
    function FuncToplabel(Index: Integer): TtkTokenKind;
    function FuncTrapezium(Index: Integer): TtkTokenKind;
    function FuncTriangle(Index: Integer): TtkTokenKind;
    function FuncTripleoctagon(Index: Integer): TtkTokenKind;
    function FuncTrue(Index: Integer): TtkTokenKind;
    function FuncUrl(Index: Integer): TtkTokenKind;
    function FuncW(Index: Integer): TtkTokenKind;
    function FuncWeight(Index: Integer): TtkTokenKind;
    function FuncWhen(Index: Integer): TtkTokenKind;
    function FuncWidth(Index: Integer): TtkTokenKind;
    function FuncZ(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure IdentProc;
    procedure UnknownProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure CStyleCommentOpenProc;
    procedure CStyleCommentProc;
    procedure StringOpenProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure DirectionsProc;
  protected
    function GetSampleSource: WideString; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: WideString; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetKeyWords(TokenKind: Integer): WideString; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
     function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property ArrowHeadAttri: TSynHighlighterAttributes read fArrowHeadAttri write fArrowHeadAttri;
    property AttributeAttri: TSynHighlighterAttributes read fAttributeAttri write fAttributeAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property DirectionsAttri: TSynHighlighterAttributes read fDirectionsAttri write fDirectionsAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property ShapeAttri: TSynHighlighterAttributes read fShapeAttri write fShapeAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property ValueAttri: TSynHighlighterAttributes read fValueAttri write fValueAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  KeyWords: array[0..145] of WideString = (
    'all', 'appendix', 'arrowhead', 'arrowsize', 'arrowtail', 'auto', 'back', 
    'bgcolor', 'bold', 'both', 'bottomlabel', 'box', 'center', 'circle', 
    'clusterrank', 'color', 'comment', 'compound', 'concentrate', 'constraint', 
    'decorate', 'diamond', 'digraph', 'dir', 'distortion', 'dot', 'dotted', 
    'doublecircle', 'doubleoctagon', 'e', 'edge', 'egg', 'ellipse', 'false', 
    'fill', 'fillcolor', 'filled', 'fixedsize', 'fontcolor', 'fontname', 
    'fontpath', 'fontsize', 'forward', 'global', 'graph', 'group', 'headlabel', 
    'headport', 'headurl', 'height', 'hexagon', 'house', 'id', 'inv', 'invdot', 
    'invhouse', 'invodot', 'invtrapezium', 'invtriangle', 'label', 'labelangle', 
    'labeldistance', 'labelfloat', 'labelfontcolor', 'labelfontname', 
    'labelfontsize', 'labeljust', 'labelloc', 'layer', 'layers', 'lhead', 
    'ltail', 'margin', 'max', 'mcircle', 'mclimit', 'mdiamond', 'merged', 'min', 
    'minimum', 'minlen', 'mrecord', 'msquare', 'multiples', 'n', 'ne', 'node', 
    'nodesep', 'none', 'normal', 'nslimit', 'nw', 'octagon', 'odot', 'onto', 
    'ordering', 'orientation', 'page', 'pagedir', 'parallelogram', 
    'peripheries', 'plaintext', 'point', 'polygon', 'quantum', 'rank', 
    'rankdir', 'ranksep', 'ratio', 'record', 'regular', 'remincross', 'rotate', 
    's', 'same', 'samehead', 'sametail', 'samplepoints', 'se', 'searchsize', 
    'section', 'shape', 'shapefile', 'sides', 'sink', 'size', 'skew', 'source', 
    'strict', 'style', 'subgraph', 'sw', 'taillabel', 'tailport', 'tailurl', 
    'toplabel', 'trapezium', 'triangle', 'tripleoctagon', 'true', 'url', 'w', 
    'weight', 'when', 'width', 'z' 
  );

  KeyIndices: array[0..786] of Integer = (
    -1, -1, -1, -1, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 141, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 88, 50, -1, -1, -1, -1, -1, 
    -1, -1, -1, 40, -1, -1, -1, -1, 4, -1, -1, -1, -1, 90, -1, 3, -1, 110, 86, 
    -1, -1, 49, 23, -1, 92, -1, -1, -1, 15, -1, 122, -1, -1, 28, -1, 78, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 85, -1, 27, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 140, -1, 103, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 142, -1, 7, -1, 0, 
    -1, -1, 97, -1, -1, -1, -1, -1, 43, -1, -1, -1, 131, -1, -1, -1, 5, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 101, -1, 10, -1, 
    47, 68, -1, 132, -1, -1, 52, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2, -1, 
    -1, -1, 64, -1, -1, 124, -1, -1, -1, -1, -1, -1, 87, -1, -1, -1, 12, -1, 84, 
    -1, -1, -1, 46, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 42, -1, 38, -1, -1, -1, 143, -1, -1, -1, 145, 
    106, -1, 127, -1, -1, -1, 99, 75, -1, -1, 102, -1, 58, -1, -1, 56, -1, -1, 
    -1, -1, 9, -1, -1, -1, -1, -1, 22, -1, 73, -1, -1, -1, 17, -1, 54, 112, -1, 
    -1, -1, -1, -1, -1, -1, 113, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 96, -1, 
    21, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 69, 116, -1, -1, 32, -1, 
    -1, -1, -1, -1, -1, -1, 16, -1, -1, -1, -1, -1, 126, -1, -1, -1, -1, -1, -1, 
    -1, 71, -1, -1, -1, -1, -1, -1, -1, -1, -1, 137, -1, -1, 117, -1, -1, -1, 
    -1, -1, -1, -1, -1, 111, 93, -1, -1, -1, -1, 108, -1, -1, 119, -1, -1, -1, 
    -1, 29, -1, -1, -1, -1, -1, -1, -1, -1, 89, -1, -1, -1, -1, 76, -1, -1, -1, 
    -1, -1, -1, -1, 77, -1, -1, 104, -1, -1, -1, -1, -1, -1, 33, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 26, -1, -1, -1, 79, -1, 19, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 39, -1, -1, -1, 115, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 109, 35, -1, -1, 70, -1, -1, 57, -1, 72, -1, 
    -1, 83, -1, -1, -1, -1, 130, -1, -1, -1, 18, -1, 118, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 81, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 61, 
    37, 1, -1, -1, -1, -1, 138, -1, -1, -1, -1, -1, 129, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 14, -1, -1, 8, -1, -1, -1, 125, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 91, -1, -1, -1, -1, -1, 60, -1, -1, -1, -1, -1, -1, -1, 
    95, -1, -1, -1, -1, 136, -1, -1, 20, -1, 62, -1, -1, -1, -1, 134, -1, -1, 
    -1, 63, -1, -1, -1, 121, 80, -1, -1, -1, -1, -1, -1, 135, -1, -1, 120, -1, 
    -1, -1, 53, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 31, -1, -1, -1, -1, -1, 
    -1, 24, -1, -1, 139, 67, -1, -1, 59, -1, -1, 36, -1, -1, -1, -1, -1, -1, -1, 
    -1, 128, 34, -1, -1, -1, -1, -1, -1, -1, -1, 65, -1, 114, -1, -1, -1, -1, 
    -1, -1, -1, 55, -1, -1, 94, -1, -1, 13, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 41, -1, -1, -1, -1, -1, -1, -1, 44, -1, 
    -1, -1, -1, -1, 74, -1, 51, 144, -1, -1, 82, 98, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 100, 66, -1, 25, -1, -1, -1, 45, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 48, -1, -1, 
    6, 105, -1, -1, 133, 123, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 107, -1, -1, -1, -1, -1, -1, -1, -1, -1, 30, -1, -1, -1 
  );

{$Q-}
function TSynDOTSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 63 + Ord(Str^) * 331;
    inc(Str);
  end;
  Result := Result mod 787;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynDOTSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynDOTSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[132] := FuncAll;
  fIdentFuncTable[509] := FuncAppendix;
  fIdentFuncTable[188] := FuncArrowhead;
  fIdentFuncTable[72] := FuncArrowsize;
  fIdentFuncTable[65] := FuncArrowtail;
  fIdentFuncTable[149] := FuncAuto;
  fIdentFuncTable[752] := FuncBack;
  fIdentFuncTable[130] := FuncBgcolor;
  fIdentFuncTable[536] := FuncBold;
  fIdentFuncTable[266] := FuncBoth;
  fIdentFuncTable[169] := FuncBottomlabel;
  fIdentFuncTable[4] := FuncBox;
  fIdentFuncTable[206] := FuncCenter;
  fIdentFuncTable[666] := FuncCircle;
  fIdentFuncTable[533] := FuncClusterrank;
  fIdentFuncTable[85] := FuncColor;
  fIdentFuncTable[327] := FuncComment;
  fIdentFuncTable[278] := FuncCompound;
  fIdentFuncTable[481] := FuncConcentrate;
  fIdentFuncTable[425] := FuncConstraint;
  fIdentFuncTable[573] := FuncDecorate;
  fIdentFuncTable[302] := FuncDiamond;
  fIdentFuncTable[272] := FuncDigraph;
  fIdentFuncTable[79] := FuncDir;
  fIdentFuncTable[621] := FuncDistortion;
  fIdentFuncTable[726] := FuncDot;
  fIdentFuncTable[419] := FuncDotted;
  fIdentFuncTable[104] := FuncDoublecircle;
  fIdentFuncTable[90] := FuncDoubleoctagon;
  fIdentFuncTable[377] := FuncE;
  fIdentFuncTable[783] := FuncEdge;
  fIdentFuncTable[614] := FuncEgg;
  fIdentFuncTable[319] := FuncEllipse;
  fIdentFuncTable[409] := FuncFalse;
  fIdentFuncTable[641] := FuncFill;
  fIdentFuncTable[461] := FuncFillcolor;
  fIdentFuncTable[631] := FuncFilled;
  fIdentFuncTable[508] := FuncFixedsize;
  fIdentFuncTable[237] := FuncFontcolor;
  fIdentFuncTable[435] := FuncFontname;
  fIdentFuncTable[60] := FuncFontpath;
  fIdentFuncTable[685] := FuncFontsize;
  fIdentFuncTable[235] := FuncForward;
  fIdentFuncTable[141] := FuncGlobal;
  fIdentFuncTable[693] := FuncGraph;
  fIdentFuncTable[730] := FuncGroup;
  fIdentFuncTable[212] := FuncHeadlabel;
  fIdentFuncTable[171] := FuncHeadport;
  fIdentFuncTable[749] := FuncHeadurl;
  fIdentFuncTable[78] := FuncHeight;
  fIdentFuncTable[51] := FuncHexagon;
  fIdentFuncTable[701] := FuncHouse;
  fIdentFuncTable[177] := FuncId;
  fIdentFuncTable[603] := FuncInv;
  fIdentFuncTable[280] := FuncInvdot;
  fIdentFuncTable[660] := FuncInvhouse;
  fIdentFuncTable[261] := FuncInvodot;
  fIdentFuncTable[467] := FuncInvtrapezium;
  fIdentFuncTable[258] := FuncInvtriangle;
  fIdentFuncTable[628] := FuncLabel;
  fIdentFuncTable[557] := FuncLabelangle;
  fIdentFuncTable[507] := FuncLabeldistance;
  fIdentFuncTable[575] := FuncLabelfloat;
  fIdentFuncTable[584] := FuncLabelfontcolor;
  fIdentFuncTable[192] := FuncLabelfontname;
  fIdentFuncTable[650] := FuncLabelfontsize;
  fIdentFuncTable[724] := FuncLabeljust;
  fIdentFuncTable[625] := FuncLabelloc;
  fIdentFuncTable[172] := FuncLayer;
  fIdentFuncTable[315] := FuncLayers;
  fIdentFuncTable[464] := FuncLhead;
  fIdentFuncTable[341] := FuncLtail;
  fIdentFuncTable[469] := FuncMargin;
  fIdentFuncTable[274] := FuncMax;
  fIdentFuncTable[699] := FuncMcircle;
  fIdentFuncTable[253] := FuncMclimit;
  fIdentFuncTable[391] := FuncMdiamond;
  fIdentFuncTable[399] := FuncMerged;
  fIdentFuncTable[92] := FuncMin;
  fIdentFuncTable[423] := FuncMinimum;
  fIdentFuncTable[589] := FuncMinlen;
  fIdentFuncTable[493] := FuncMrecord;
  fIdentFuncTable[705] := FuncMsquare;
  fIdentFuncTable[472] := FuncMultiples;
  fIdentFuncTable[208] := FuncN;
  fIdentFuncTable[102] := FuncNe;
  fIdentFuncTable[75] := FuncNode;
  fIdentFuncTable[202] := FuncNodesep;
  fIdentFuncTable[50] := FuncNone;
  fIdentFuncTable[386] := FuncNormal;
  fIdentFuncTable[70] := FuncNslimit;
  fIdentFuncTable[551] := FuncNw;
  fIdentFuncTable[81] := FuncOctagon;
  fIdentFuncTable[364] := FuncOdot;
  fIdentFuncTable[663] := FuncOnto;
  fIdentFuncTable[565] := FuncOrdering;
  fIdentFuncTable[300] := FuncOrientation;
  fIdentFuncTable[135] := FuncPage;
  fIdentFuncTable[706] := FuncPagedir;
  fIdentFuncTable[252] := FuncParallelogram;
  fIdentFuncTable[723] := FuncPeripheries;
  fIdentFuncTable[167] := FuncPlaintext;
  fIdentFuncTable[256] := FuncPoint;
  fIdentFuncTable[117] := FuncPolygon;
  fIdentFuncTable[402] := FuncQuantum;
  fIdentFuncTable[753] := FuncRank;
  fIdentFuncTable[246] := FuncRankdir;
  fIdentFuncTable[773] := FuncRanksep;
  fIdentFuncTable[369] := FuncRatio;
  fIdentFuncTable[460] := FuncRecord;
  fIdentFuncTable[74] := FuncRegular;
  fIdentFuncTable[363] := FuncRemincross;
  fIdentFuncTable[281] := FuncRotate;
  fIdentFuncTable[289] := FuncS;
  fIdentFuncTable[652] := FuncSame;
  fIdentFuncTable[439] := FuncSamehead;
  fIdentFuncTable[316] := FuncSametail;
  fIdentFuncTable[354] := FuncSamplepoints;
  fIdentFuncTable[483] := FuncSe;
  fIdentFuncTable[372] := FuncSearchsize;
  fIdentFuncTable[599] := FuncSection;
  fIdentFuncTable[588] := FuncShape;
  fIdentFuncTable[87] := FuncShapefile;
  fIdentFuncTable[757] := FuncSides;
  fIdentFuncTable[195] := FuncSink;
  fIdentFuncTable[540] := FuncSize;
  fIdentFuncTable[333] := FuncSkew;
  fIdentFuncTable[248] := FuncSource;
  fIdentFuncTable[640] := FuncStrict;
  fIdentFuncTable[520] := FuncStyle;
  fIdentFuncTable[477] := FuncSubgraph;
  fIdentFuncTable[145] := FuncSw;
  fIdentFuncTable[174] := FuncTaillabel;
  fIdentFuncTable[756] := FuncTailport;
  fIdentFuncTable[580] := FuncTailurl;
  fIdentFuncTable[596] := FuncToplabel;
  fIdentFuncTable[570] := FuncTrapezium;
  fIdentFuncTable[351] := FuncTriangle;
  fIdentFuncTable[514] := FuncTripleoctagon;
  fIdentFuncTable[624] := FuncTrue;
  fIdentFuncTable[115] := FuncUrl;
  fIdentFuncTable[39] := FuncW;
  fIdentFuncTable[128] := FuncWeight;
  fIdentFuncTable[241] := FuncWhen;
  fIdentFuncTable[702] := FuncWidth;
  fIdentFuncTable[245] := FuncZ;
end;

function TSynDOTSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynDOTSyn.FuncAll(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncAppendix(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncArrowhead(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncArrowsize(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncArrowtail(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncAuto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncBack(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncBgcolor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncBold(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncBoth(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncBottomlabel(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncBox(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncCenter(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncCircle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncClusterrank(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncColor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncComment(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncCompound(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncConcentrate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncConstraint(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncDecorate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncDiamond(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncDigraph(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncDir(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncDistortion(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncDot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArrowHead
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncDotted(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncDoublecircle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncDoubleoctagon(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncE(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncEdge(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncEgg(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncEllipse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncFalse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncFill(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncFillcolor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncFilled(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue  // TODO: ANSI source isn't clear if tkValue or tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncFixedsize(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncFontcolor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncFontname(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncFontpath(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncFontsize(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncForward(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncGlobal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncGraph(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncGroup(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncHeadlabel(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncHeadport(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncHeadurl(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncHeight(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncHexagon(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncHouse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncId(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncInv(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArrowHead
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncInvdot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArrowHead
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncInvhouse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncInvodot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArrowHead
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncInvtrapezium(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncInvtriangle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLabel(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLabelangle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLabeldistance(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLabelfloat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLabelfontcolor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLabelfontname(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLabelfontsize(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLabeljust(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLabelloc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLayer(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute  // TODO: ANSI source isn't clear if tkAttribute or tkKey
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLayers(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute  // TODO: ANSI source isn't clear if tkAttribute or tkKey
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLhead(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLtail(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMargin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMax(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMcircle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMclimit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMdiamond(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMerged(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMinimum(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMinlen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMrecord(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMsquare(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMultiples(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncN(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncNe(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncNode(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncNodesep(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncNone(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else if IsCurrentToken(KeyWords[Index]) then
    Result := tkArrowHead
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncNormal(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArrowHead
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncNslimit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncNw(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncOctagon(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncOdot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArrowHead
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncOnto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncOrdering(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncOrientation(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncPage(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncPagedir(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncParallelogram(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncPeripheries(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncPlaintext(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncPoint(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncPolygon(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncQuantum(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncRank(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncRankdir(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncRanksep(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncRatio(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncRecord(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncRegular(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncRemincross(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncRotate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncS(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSame(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSamehead(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSametail(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSamplepoints(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSe(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSearchsize(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSection(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncShape(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncShapefile(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSides(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSink(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSize(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSkew(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSource(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncStrict(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncStyle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSubgraph(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSw(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncTaillabel(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncTailport(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncTailurl(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncToplabel(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncTrapezium(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncTriangle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncTripleoctagon(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncTrue(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncUrl(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncW(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncWeight(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncWhen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncWidth(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncZ(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

procedure TSynDOTSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynDOTSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynDOTSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
    inc(Run);
end;

procedure TSynDOTSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynDOTSyn.DirectionsProc;
begin
  Inc(Run);
  if (fLine[Run] = '>') or (fLine[Run] = '-') then
  begin
    fTokenID := tkDirections;
    inc(Run);
  end
  else
    fTokenID := tkSymbol;
end;

procedure TSynDOTSyn.CStyleCommentOpenProc;
begin
  Inc(Run);
  if fLine[Run] = '/' then
  begin
    fTokenID := tkComment;
    inc(Run, 2);
    while not IsLineEnd(Run) do Inc(Run);
    Exit;
  end;
  if fLine[Run] = '*' then
  begin
    fRange := rsCStyleComment;
    CStyleCommentProc;
    fTokenID := tkComment;
  end
  else
    fTokenID := tkIdentifier;
end;

procedure TSynDOTSyn.CStyleCommentProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    else
    begin
      fTokenID := tkComment;
      repeat
        if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then
        begin
          Inc(Run, 2);
          fRange := rsUnKnown;
          Break;
        end;
        if not IsLineEnd(Run) then
          Inc(Run);
      until IsLineEnd(Run);
    end;
  end;
end;

procedure TSynDOTSyn.StringOpenProc;
begin
  Inc(Run);
  fRange := rsString;
  StringProc;
  fTokenID := tkString;
end;

procedure TSynDOTSyn.StringProc;
begin
  fTokenID := tkString;
  repeat
    if fLine[Run] = '''' then
    begin
      Inc(Run, 1);
      fRange := rsUnKnown;
      Break;
    end;
    if not IsLineEnd(Run) then
      Inc(Run);
  until IsLineEnd(Run);
end;

constructor TSynDOTSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  fArrowHeadAttri := TSynHighLighterAttributes.Create(SYNS_AttrArrowHead, SYNS_FriendlyAttrArrowHead);
  fArrowHeadAttri.Foreground := clRed;
  AddAttribute(fArrowHeadAttri);

  fAttributeAttri := TSynHighLighterAttributes.Create(SYNS_AttrAttribute, SYNS_FriendlyAttrAttribute);
  AddAttribute(fAttributeAttri);

  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clNavy;
  AddAttribute(fCommentAttri);

  fDirectionsAttri := TSynHighLighterAttributes.Create(SYNS_AttrDirections, SYNS_FriendlyAttrDirections);
  fDirectionsAttri.Style := [fsBold];
  fDirectionsAttri.Foreground := clYellow;
  AddAttribute(fDirectionsAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);

  fShapeAttri := TSynHighLighterAttributes.Create(SYNS_AttrShape, SYNS_FriendlyAttrShape);
  fShapeAttri.Style := [fsBold];
  fShapeAttri.Foreground := clRed;
  AddAttribute(fShapeAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);

  fValueAttri := TSynHighLighterAttributes.Create(SYNS_AttrValue, SYNS_FriendlyAttrValue);
  fValueAttri.Style := [fsItalic];
  fValueAttri.Foreground := clRed;
  AddAttribute(fValueAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  fSymbolAttri.Style := [fsBold];
  fSymbolAttri.Foreground := clGreen;
  AddAttribute(fSymbolAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterDOT;
  fRange := rsUnknown;
end;

procedure TSynDOTSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do
    Inc(Run);
end;

procedure TSynDOTSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynDOTSyn.SymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynDOTSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsCStyleComment: CStyleCommentProc;
  else
    begin
      fRange := rsUnknown;
      case fLine[Run] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
        '/': CStyleCommentOpenProc;
        '-': DirectionsProc;
        '''': StringOpenProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        'A'..'Z', 'a'..'z', '_': IdentProc;
        '~', '{', '}', ',', '(', ')', '[', ']', '<', '>', ':', '?', ';', '!', '=': SymbolProc;
        else UnknownProc;
      end;
    end;
  end;
  inherited;
end;

function TSynDOTSyn.GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynDOTSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynDOTSyn.GetKeyWords(TokenKind: Integer): WideString;
begin
  Result :=
    '--,->,all,appendix,arrowhead,arrowsize,arrowtail,auto,back,bgcolor,bo' +
    'ld,both,bottomlabel,box,center,circle,clusterrank,color,comment,compou' +
    'nd,concentrate,constraint,decorate,diamond,digraph,dir,distortion,dot,' +
    'dotted,doublecircle,doubleoctagon,e,edge,egg,ellipse,false,fill,fillco' +
    'lor,filled,fixedsize,fontcolor,fontname,fontpath,fontsize,forward,glob' +
    'al,graph,group,headlabel,headport,headURL,height,hexagon,house,id,inv,' +
    'invdot,invhouse,invodot,invtrapezium,invtriangle,label,labelangle,labe' +
    'ldistance,labelfloat,labelfontcolor,labelfontname,labelfontsize,labelj' +
    'ust,labelloc,layer,layers,lhead,ltail,margin,max,mcircle,mclimit,mdiam' +
    'ond,merged,min,minimum,minlen,mrecord,msquare,multiples,n,ne,node,node' +
    'sep,none,normal,nslimit,nw,octagon,odot,onto,ordering,orientation,page' +
    ',pagedir,parallelogram,peripheries,plaintext,point,polygon,quantum,ran' +
    'k,rankdir,ranksep,ratio,record,regular,remincross,rotate,s,same,samehe' +
    'ad,sametail,samplepoints,se,searchsize,section,shape,shapefile,sides,s' +
    'ink,size,skew,source,strict,style,subgraph,sw,taillabel,tailport,tailU' +
    'RL,toplabel,trapezium,triangle,tripleoctagon,true,url,w,weight,when,wi' +
    'dth,z';
end;

function TSynDOTSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynDOTSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkArrowHead: Result := fArrowHeadAttri;
    tkAttribute: Result := fAttributeAttri;
    tkComment: Result := fCommentAttri;
    tkDirections: Result := fDirectionsAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkShape: Result := fShapeAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkValue: Result := fValueAttri;
    tkUnknown: Result := fIdentifierAttri;
    tkSymbol: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynDOTSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynDOTSyn.GetSampleSource: WideString;
begin
  Result :=
    '// ATT DOT Graphic description language'#13#10 +
    'digraph asde91 {'#13#10 +
    '  ranksep=.75; size = "7.5,7.5";'#13#10 +
    '  {'#13#10 +
    '      node [shape=plaintext, fontsize=16];'#13#10 +
    '      /* the time-line graph */'#13#10 +
    '      past -> 1978 -> 1980 -> 1982 -> 1983 -> 1985 -> 1986 ->'#13#10 +
    '      1987 -> 1988 -> 1989 -> 1990 -> "future";'#13#10 +
    '      /* ancestor programs */'#13#10 +
    '      "Bourne sh"; "make"; "SCCS"; "yacc"; "cron"; "Reiser cpp";'#13#10 +
    '      "Cshell"; "emacs"; "build"; "vi"; "<curses>"; "RCS"; "C*";'#13#10 +
    '  }'#13#10 +
    '      { rank = same;'#13#10 +
    '      "Software IS"; "Configuration Mgt"; "Architecture & Libraries";'#13#10 +
    '      "Process";'#13#10 +
    '  };'#13#10 +
    '    node [shape=box];'#13#10 +
    '    { rank = same; "past"; "SCCS"; "make"; "Bourne sh"; "yacc"; "cron"; }'#13#10 +
    '    { rank = same; 1978; "Reiser cpp"; "Cshell"; }'#13#10 +
    '    { rank = same; 1980; "build"; "emacs"; "vi"; }'#13#10 +
    '    { rank = same; 1982; "RCS"; "<curses>"; "IMX"; "SYNED"; }'#13#10 +
    '    { rank = same; 1983; "ksh"; "IFS"; "TTU"; }'#13#10 +
    '    { rank = same; 1985; "nmake"; "Peggy"; }'#13#10 +
    '    { rank = same; 1986; "C*"; "ncpp"; "ksh-i"; "<curses-i>"; "PG2"; }'#13#10 +
    '    { rank = same; 1987; "Ansi cpp"; "nmake 2.0"; "3D File System"; "fdelta";'#13#10 +
    '        "DAG"; "CSAS";}'#13#10 +
    '    { rank = same; 1988; "CIA"; "SBCS"; "ksh-88"; "PEGASUS/PML"; "PAX";'#13#10 +
    '        "backtalk"; }'#13#10 +
    '    { rank = same; 1989; "CIA++"; "APP"; "SHIP"; "DataShare"; "ryacc";'#13#10 +
    '        "Mosaic"; }'#13#10 +
    '    { rank = same; 1990; "libft"; "CoShell"; "DIA"; "IFS-i"; "kyacc"; "sfio";'#13#10 +
    '        "yeast"; "ML-X"; "DOT"; }'#13#10 +
    '    { rank = same; "future"; "Adv. Software Technology"; }'#13#10 +
    '    "PEGASUS/PML" -> "ML-X";'#13#10 +
    '    "SCCS" -> "nmake";'#13#10 +
    '    "SCCS" -> "3D File System";'#13#10 +
    '    "SCCS" -> "RCS";'#13#10 +
    '    "make" -> "nmake";'#13#10 +
    '    "make" -> "build";'#13#10 +
    '}';
end;

function TSynDOTSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterDOT;
end;

function TSynDOTSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', 'A'..'Z', 'a'..'z':
      Result := True;
    else
      Result := False;
  end;
end;

class function TSynDOTSyn.GetLanguageName: string;
begin
  Result := SYNS_LangDOT;
end;

procedure TSynDOTSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynDOTSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynDOTSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

class function TSynDOTSyn.GetFriendlyLanguageName: WideString;
begin
  Result := SYNS_FriendlyLangDOT;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynDOTSyn);
{$ENDIF}
end.
