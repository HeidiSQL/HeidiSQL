{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterCSS.pas, released 2000-04-14.
The Original Code is based on the mwCSSSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Tony De Buys.
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

$Id: SynHighlighterCss.pas,v 1.11 2002/04/09 00:26:30 jrx Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a CSS syntax highlighter for SynEdit)
@author(Tony de Buys)
@created(1999)
@lastmod(2000-06-23)
The SynHighlighterCss unit provides SynEdit with a Cascading Style Sheets syntax highlighter.
Thanks to Martin Waldenburg.
}
unit SynHighlighterCss;

{$I SynEdit.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF SYN_CLX}
  Qt, QControls, QGraphics,
  {$ELSE}
  Windows, Messages, Controls, Graphics, Registry,
  {$ENDIF}
  SynEditTypes, SynEditHighlighter;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkUnknown);

  TRangeState = (rsUnknown, rsCStyle);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

type
  TSynCssSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..255] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    function Func16: TtkTokenKind;
    function Func18: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func24: TtkTokenKind;    
    function Func26: TtkTokenKind;
    function Func29: TtkTokenKind;
    function Func30: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func34: TtkTokenKind;            
    function Func36: TtkTokenKind;
    function Func39: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func43: TtkTokenKind;
    function Func45: TtkTokenKind;
    function Func51: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func54: TtkTokenKind;
    function Func55: TtkTokenKind;
    function Func56: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func58: TtkTokenKind;    
    function Func59: TtkTokenKind;
    function Func60: TtkTokenKind;
    function Func61: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func63: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func65: TtkTokenKind;
    function Func67: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func70: TtkTokenKind;
    function Func72: TtkTokenKind;    
    function Func74: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func79: TtkTokenKind;
    function Func80: TtkTokenKind;
    function Func81: TtkTokenKind;
    function Func82: TtkTokenKind;
    function Func83: TtkTokenKind;
    function Func85: TtkTokenKind;
    function Func86: TtkTokenKind;
    function Func87: TtkTokenKind;
    function Func88: TtkTokenKind;
    function Func90: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func93: TtkTokenKind;
    function Func94: TtkTokenKind;
    function Func95: TtkTokenKind;
    function Func96: TtkTokenKind;
    function Func97: TtkTokenKind;
    function Func98: TtkTokenKind;
    function Func99: TtkTokenKind;    
    function Func100: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func103: TtkTokenKind;
    function Func105: TtkTokenKind;
    function Func106: TtkTokenKind;
    function Func107: TtkTokenKind;
    function Func108: TtkTokenKind;
    function Func110: TtkTokenKind;
    function Func111: TtkTokenKind;
    function Func112: TtkTokenKind;
    function Func113: TtkTokenKind;    
    function Func114: TtkTokenKind;
    function Func115: TtkTokenKind;
    function Func116: TtkTokenKind;
    function Func117: TtkTokenKind;
    function Func118: TtkTokenKind;
    function Func120: TtkTokenKind;
    function Func121: TtkTokenKind;
    function Func122: TtkTokenKind;
    function Func124: TtkTokenKind;
    function Func126: TtkTokenKind;
    function Func128: TtkTokenKind;
    function Func129: TtkTokenKind;
    function Func130: TtkTokenKind;
    function Func131: TtkTokenKind;
    function Func134: TtkTokenKind;
    function Func136: TtkTokenKind;
    function Func137: TtkTokenKind;
    function Func138: TtkTokenKind;
    function Func139: TtkTokenKind;
    function Func140: TtkTokenKind;
    function Func141: TtkTokenKind;
    function Func144: TtkTokenKind;
    function Func148: TtkTokenKind;
    function Func149: TtkTokenKind;
    function Func150: TtkTokenKind;
    function Func151: TtkTokenKind;
    function Func152: TtkTokenKind;    
    function Func154: TtkTokenKind;
    function Func156: TtkTokenKind;
    function Func158: TtkTokenKind;
    function Func160: TtkTokenKind;
    function Func164: TtkTokenKind;
    function Func166: TtkTokenKind;
    function Func167: TtkTokenKind;
    function Func169: TtkTokenKind;
    function Func172: TtkTokenKind;
    function Func173: TtkTokenKind;
    function Func174: TtkTokenKind;
    function Func178: TtkTokenKind;
    function Func182: TtkTokenKind;
    function Func187: TtkTokenKind;
    function Func190: TtkTokenKind;
    function Func194: TtkTokenKind;
    function Func195: TtkTokenKind;
    function Func199: TtkTokenKind;
    function Func200: TtkTokenKind;
    function Func210: TtkTokenKind;
    function Func213: TtkTokenKind;
    function Func220: TtkTokenKind;
    function Func250: TtkTokenKind;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure CStyleCommentProc;
    procedure DashProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RoundOpenProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: String; override;
  public
    {$IFNDEF SYN_CPPB_1} class {$ENDIF}                                         //mh 2000-07-14
    function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: String; LineNumber: Integer); override;
    function GetToken: string; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ReSetRange; override;
    property IdentChars;

    function KeyHash2(ToHash: PChar): Integer;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
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
  SynEditStrConst;

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable : array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do begin
    case I of
      'a'..'z', 'A'..'Z', '-', '_', '0'..'9': Identifiers[I] := True;
    else
      Identifiers[I] := False;
    end;
    J := UpCase(I);
    if I in ['a'..'z', 'A'..'Z', '-', '_'] then
      mHashTable[I] := Ord(J) - 64
    else
      mHashTable[I] := 0;
  end;
end;

procedure TSynCssSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@FIdentFuncTable);
  for I := Low(FIdentFuncTable) to High(FIdentFuncTable) do begin
    pF^ := AltFunc;
    Inc(pF);
  end;
  FIdentFuncTable[16] := Func16;
  FIdentFuncTable[18] := Func18;
  FIdentFuncTable[19] := Func19;
  FIdentFuncTable[23] := Func23;
  FIdentFuncTable[24] := Func24;
  FIdentFuncTable[26] := Func26;
  FIdentFuncTable[29] := Func29;
  FIdentFuncTable[30] := Func30;
  FIdentFuncTable[32] := Func32;
  FIdentFuncTable[34] := Func34;
  FIdentFuncTable[36] := Func36;
  FIdentFuncTable[39] := Func39;
  FIdentFuncTable[40] := Func40;
  FIdentFuncTable[43] := Func43;
  FIdentFuncTable[45] := Func45;
  FIdentFuncTable[51] := Func51;
  FIdentFuncTable[52] := Func52;
  FIdentFuncTable[54] := Func54;
  FIdentFuncTable[55] := Func55;
  FIdentFuncTable[56] := Func56;
  FIdentFuncTable[57] := Func57;
  FIdentFuncTable[58] := Func58;
  FIdentFuncTable[59] := Func59;
  FIdentFuncTable[60] := Func60;
  FIdentFuncTable[61] := Func61;
  FIdentFuncTable[62] := Func62;
  FIdentFuncTable[63] := Func63;
  FIdentFuncTable[64] := Func64;
  FIdentFuncTable[65] := Func65;
  FIdentFuncTable[67] := Func67;
  FIdentFuncTable[69] := Func69;
  FIdentFuncTable[70] := Func70;
  FIdentFuncTable[72] := Func72;
  FIdentFuncTable[74] := Func74;
  FIdentFuncTable[76] := Func76;
  FIdentFuncTable[78] := Func78;
  FIdentFuncTable[79] := Func79;
  FIdentFuncTable[80] := Func80;
  FIdentFuncTable[81] := Func81;
  FIdentFuncTable[82] := Func82;
  FIdentFuncTable[83] := Func83;
  FIdentFuncTable[85] := Func85;
  FIdentFuncTable[86] := Func86;
  FIdentFuncTable[87] := Func87;
  FIdentFuncTable[88] := Func88;
  FIdentFuncTable[90] := Func90;
  FIdentFuncTable[91] := Func91;
  FIdentFuncTable[93] := Func93;
  FIdentFuncTable[94] := Func94;
  FIdentFuncTable[95] := Func95;
  FIdentFuncTable[96] := Func96;
  FIdentFuncTable[97] := Func97;
  FIdentFuncTable[98] := Func98;
  FIdentFuncTable[99] := Func99;
  FIdentFuncTable[100] := Func100;
  FIdentFuncTable[101] := Func101;
  FIdentFuncTable[102] := Func102;
  FIdentFuncTable[103] := Func103;
  FIdentFuncTable[105] := Func105;
  FIdentFuncTable[106] := Func106;
  FIdentFuncTable[107] := Func107;
  FIdentFuncTable[108] := Func108;
  FIdentFuncTable[110] := Func110;
  FIdentFuncTable[111] := Func111;
  FIdentFuncTable[112] := Func112;
  FIdentFuncTable[113] := Func113;
  FIdentFuncTable[114] := Func114;
  FIdentFuncTable[115] := Func115;
  FIdentFuncTable[116] := Func116;
  FIdentFuncTable[117] := Func117;
  FIdentFuncTable[118] := Func118;
  FIdentFuncTable[120] := Func120;
  FIdentFuncTable[121] := Func121;
  FIdentFuncTable[122] := Func122;
  FIdentFuncTable[124] := Func124;
  FIdentFuncTable[126] := Func126;
  FIdentFuncTable[128] := Func128;
  FIdentFuncTable[129] := Func129;
  FIdentFuncTable[130] := Func130;
  FIdentFuncTable[131] := Func131;
  FIdentFuncTable[134] := Func134;
  FIdentFuncTable[136] := Func136;
  FIdentFuncTable[137] := Func137;
  FIdentFuncTable[138] := Func138;
  FIdentFuncTable[139] := Func139;
  FIdentFuncTable[140] := Func140;
  FIdentFuncTable[141] := Func141;
  FIdentFuncTable[144] := Func144;
  FIdentFuncTable[148] := Func148;
  FIdentFuncTable[149] := Func149;
  FIdentFuncTable[150] := Func150;
  FIdentFuncTable[151] := Func151;
  FIdentFuncTable[152] := Func152;
  FIdentFuncTable[154] := Func154;
  FIdentFuncTable[156] := Func156;
  FIdentFuncTable[158] := Func158;
  FIdentFuncTable[160] := Func160;
  FIdentFuncTable[164] := Func164;
  FIdentFuncTable[166] := Func166;
  FIdentFuncTable[167] := Func167;
  FIdentFuncTable[169] := Func169;
  FIdentFuncTable[172] := Func172;
  FIdentFuncTable[173] := Func173;
  FIdentFuncTable[174] := Func174;
  FIdentFuncTable[178] := Func178;
  FIdentFuncTable[182] := Func182;
  FIdentFuncTable[187] := Func187;
  FIdentFuncTable[190] := Func190;
  FIdentFuncTable[194] := Func194;
  FIdentFuncTable[195] := Func195;
  FIdentFuncTable[199] := Func199;
  FIdentFuncTable[200] := Func200;
  FIdentFuncTable[210] := Func210;
  FIdentFuncTable[213] := Func213;
  FIdentFuncTable[220] := Func220;
  FIdentFuncTable[250] := Func250;
end;

function TSynCssSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['a'..'z', 'A'..'Z', '0'..'9', '-', '_'] do begin
    Inc(Result, mHashTable[ToHash^]);
    Inc(ToHash);
  end;
  FStringLen := ToHash - FToIdent;
end;

function TSynCssSyn.KeyComp(const aKey: string): Boolean;
var
  iI  : Integer;
  Temp: PChar;
begin
  Temp := FToIdent;
  if Length(aKey) = FStringLen then begin
    Result := True;
    for iI := 1 to fStringLen do begin
      if mHashTable[Temp^] <> mHashTable[aKey[iI]] then begin
        Result := False;
        Break;
      end;
      Inc(Temp);
    end;
  end else
    Result := False;
end;

function TSynCssSyn.Func16: TtkTokenKind;
begin
  if KeyComp('cm') or KeyComp('deg') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func18: TtkTokenKind;
begin
  if KeyComp('em') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func19: TtkTokenKind;
begin
  if KeyComp('pc') or KeyComp('s') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func23: TtkTokenKind;
begin
  if KeyComp('in') or KeyComp('rad') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func24: TtkTokenKind;
begin
  if KeyComp('b-box') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func26: TtkTokenKind;
begin
  if KeyComp('mm') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func29: TtkTokenKind;
begin
  if KeyComp('page') or KeyComp('cue') or KeyComp('ex') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func30: TtkTokenKind;
begin
  if KeyComp('grad') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func32: TtkTokenKind;
begin
  if KeyComp('ms') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func34: TtkTokenKind;
begin
  if KeyComp('Hz') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func36: TtkTokenKind;
begin
  if KeyComp('pt') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func39: TtkTokenKind;
begin
  if KeyComp('clear') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func40: TtkTokenKind;
begin
  if KeyComp('px') or KeyComp('clip') or KeyComp('src') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func43: TtkTokenKind;
begin
  if KeyComp('left') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func45: TtkTokenKind;
begin
  if KeyComp('ime-mode') or KeyComp('kHz') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func51: TtkTokenKind;
begin
  if KeyComp('top') or KeyComp('panose-1') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func52: TtkTokenKind;
begin
  if KeyComp('speak') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func54: TtkTokenKind;
begin
  if KeyComp('float') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func55: TtkTokenKind;
begin
  if KeyComp('font') or KeyComp('padding') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func56: TtkTokenKind;
begin
  if KeyComp('pitch') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func57: TtkTokenKind;
begin
  if KeyComp('height') or KeyComp('run-in') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func58: TtkTokenKind;
begin
  if KeyComp('line-break') or KeyComp('cap-height') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func59: TtkTokenKind;
begin
  if KeyComp('size') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func60: TtkTokenKind;
begin
  if KeyComp('cue-after') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func61: TtkTokenKind;
begin
  if KeyComp('cue-before') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func62: TtkTokenKind;
begin
  if KeyComp('margin') or KeyComp('pause') or KeyComp('right') or
    KeyComp('marks') or KeyComp('border') or KeyComp('x-height') or
    KeyComp('ascent') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func63: TtkTokenKind;
begin
  if KeyComp('color') or KeyComp('z-index') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func64: TtkTokenKind;
begin
  if KeyComp('width') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func65: TtkTokenKind;
begin
  if KeyComp('stemh') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func67: TtkTokenKind;
begin
  if KeyComp('slope') or KeyComp('baseline') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func69: TtkTokenKind;
begin
  if KeyComp('zoom') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func70: TtkTokenKind;
begin
  if KeyComp('filter') or KeyComp('descent') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func72: TtkTokenKind;
begin
  if KeyComp('top-line') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func74: TtkTokenKind;
begin
  if KeyComp('speak-header') or KeyComp('min-height') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func76: TtkTokenKind;
begin
  if KeyComp('max-height') or KeyComp('unicode-bidi') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func78: TtkTokenKind;
begin
  if KeyComp('page-break-after') or KeyComp('word-break') or
    KeyComp('line-height') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func79: TtkTokenKind;
begin
  if KeyComp('padding-left') or KeyComp('page-break-before') or
    KeyComp('stemv') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func80: TtkTokenKind;
begin
  if KeyComp('behavior') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func81: TtkTokenKind;
begin
  if KeyComp('speech-rate') or KeyComp('min-width') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func82: TtkTokenKind;
begin
  if KeyComp('pitch-range') or KeyComp('mathline') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func83: TtkTokenKind;
begin
  if KeyComp('max-width') or KeyComp('widths') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func85: TtkTokenKind;
begin
  if KeyComp('bottom') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func86: TtkTokenKind;
begin
  if KeyComp('margin-left') or KeyComp('border-left') or KeyComp('display') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func87: TtkTokenKind;
begin
  if KeyComp('padding-top') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func88: TtkTokenKind;
begin
  if KeyComp('page-break-inside') or KeyComp('volume') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func90: TtkTokenKind;
begin
  if KeyComp('white-space') or KeyComp('ruby-align') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func91: TtkTokenKind;
begin
  if KeyComp('orphans') or KeyComp('content') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func93: TtkTokenKind;
begin
  if KeyComp('text-align') or KeyComp('pause-after') or KeyComp('widows') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func94: TtkTokenKind;
begin
  if KeyComp('margin-top') or KeyComp('border-top') or KeyComp('pause-before') or
    KeyComp('cursor') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func95: TtkTokenKind;
begin
  if KeyComp('font-size') or KeyComp('richness') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func96: TtkTokenKind;
begin
  if KeyComp('background') or KeyComp('caption-side') or KeyComp('counter') or
    KeyComp('outline') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func97: TtkTokenKind;
begin
  if KeyComp('quotes') or KeyComp('direction') or KeyComp('unicode-range') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func98: TtkTokenKind;
begin
  if KeyComp('padding-right') or KeyComp('azimuth') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func99: TtkTokenKind;
begin
  if KeyComp('word-wrap') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func100: TtkTokenKind;
begin
  if KeyComp('stress') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func101: TtkTokenKind;
begin
  if KeyComp('voice-family') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func102: TtkTokenKind;
begin
  if KeyComp('font-family') or KeyComp('units-per-em') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func103: TtkTokenKind;
begin
  if KeyComp('elevation') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func105: TtkTokenKind;
begin
  if KeyComp('margin-right') or KeyComp('border-right') or
    KeyComp('centerline') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func106: TtkTokenKind;
begin
  if KeyComp('border-color') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func107: TtkTokenKind;
begin
  if KeyComp('border-width') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func108: TtkTokenKind;
begin
  if KeyComp('font-weight') or KeyComp('play-during') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func110: TtkTokenKind;
begin
  if KeyComp('word-spacing') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func111: TtkTokenKind;
begin
  if KeyComp('empty-cells') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func112: TtkTokenKind;
begin
  if KeyComp('background-image') or KeyComp('border-spacing') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func113: TtkTokenKind;
begin
  if KeyComp('layout-grid') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func114: TtkTokenKind;
begin
  if KeyComp('vertical-align') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func115: TtkTokenKind;
begin
  if KeyComp('table-layout') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func116: TtkTokenKind;
begin
  if KeyComp('text-indent') or KeyComp('overflow') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func117: TtkTokenKind;
begin
  if KeyComp('font-style') or KeyComp('position') or KeyComp('speak-numeral') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func118: TtkTokenKind;
begin
  if KeyComp('marker-offset') or KeyComp('writing-mode') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func120: TtkTokenKind;
begin
  if KeyComp('text-shadow') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func121: TtkTokenKind;
begin
  if KeyComp('font-variant') or KeyComp('padding-bottom') or KeyComp('overflow-x') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func122: TtkTokenKind;
begin
  if KeyComp('list-style') or KeyComp('overflow-y') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func124: TtkTokenKind;
begin
  if KeyComp('border-style') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func126: TtkTokenKind;
begin
  if KeyComp('border-collapse') or KeyComp('definition-src') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func128: TtkTokenKind;
begin
  if KeyComp('margin-bottom') or KeyComp('border-bottom') or
    KeyComp('text-kashida-space') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func129: TtkTokenKind;
begin
  if KeyComp('font-stretch') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func130: TtkTokenKind;
begin
  if KeyComp('border-left-color') or KeyComp('letter-spacing') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func131: TtkTokenKind;
begin
  if KeyComp('border-left-width') or KeyComp('layout-grid-mode') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func134: TtkTokenKind;
begin
  if KeyComp('layout-grid-line') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func136: TtkTokenKind;
begin
  if KeyComp('visibility') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func137: TtkTokenKind;
begin
  if KeyComp('ruby-overhang') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func138: TtkTokenKind;
begin
  if KeyComp('border-top-color') or KeyComp('list-style-image') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func139: TtkTokenKind;
begin
  if KeyComp('border-top-width') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func140: TtkTokenKind;
begin
  if KeyComp('background-color') or KeyComp('outline-color') or
    KeyComp('scrollbar-face-color') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func141: TtkTokenKind;
begin
  if KeyComp('outline-width') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func144: TtkTokenKind;
begin
  if KeyComp('counter-reset') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func148: TtkTokenKind;
begin
  if KeyComp('border-left-style') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func149: TtkTokenKind;
begin
  if KeyComp('border-right-color') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func150: TtkTokenKind;
begin
  if KeyComp('border-right-width') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func151: TtkTokenKind;
begin
  if KeyComp('font-size-adjust') or KeyComp('text-autospace') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func152: TtkTokenKind;
begin
  if KeyComp('scrollbar-base-color') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func154: TtkTokenKind;
begin
  if KeyComp('text-decoration') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func156: TtkTokenKind;
begin
  if KeyComp('border-top-style') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func158: TtkTokenKind;
begin
  if KeyComp('outline-style') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func160: TtkTokenKind;
begin
  if KeyComp('layout-grid-type') or KeyComp('text-justify') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func164: TtkTokenKind;
begin
  if KeyComp('ruby-position') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func166: TtkTokenKind;
begin
  if KeyComp('scrollbar-3d-light-color') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func167: TtkTokenKind;
begin
  if KeyComp('border-right-style') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func169: TtkTokenKind;
begin
  if KeyComp('list-style-type') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func172: TtkTokenKind;
begin
  if KeyComp('border-bottom-color') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func173: TtkTokenKind;
begin
  if KeyComp('border-bottom-width') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func174: TtkTokenKind;
begin
  if KeyComp('text-transform') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func178: TtkTokenKind;
begin
  if KeyComp('counter-increment') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func182: TtkTokenKind;
begin
  if KeyComp('background-attachment') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func187: TtkTokenKind;
begin
  if KeyComp('speak-punctuation') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func190: TtkTokenKind;
begin
  if KeyComp('border-bottom-style') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func194: TtkTokenKind;
begin
  if KeyComp('background-position') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func195: TtkTokenKind;
begin
  if KeyComp('scrollbar-shadow-color') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func199: TtkTokenKind;
begin
  if KeyComp('background-position-x') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func200: TtkTokenKind;
begin
  if KeyComp('background-position-y') or KeyComp('scrollbar-arrow-color') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func210: TtkTokenKind;
begin
  if KeyComp('scrollbar-dark-shadow-color') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func213: TtkTokenKind;
begin
  if KeyComp('scrollbar-highlight-color') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func220: TtkTokenKind;
begin
  if KeyComp('list-style-position') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func250: TtkTokenKind;
begin
  if KeyComp('text-underline-position') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynCssSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  FToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 255 then
    Result := fIdentFuncTable[HashKey]
  else
    Result := tkIdentifier;
end;

procedure TSynCssSyn.MakeMethodTables;
var
  chI: Char;
begin
  for chI := #0 to #255 do
    case chI of
      '{', '}'                    : FProcTable[chI] := AsciiCharProc;
      #13                         : FProcTable[chI] := CRProc;
      '-'                         : FProcTable[chI] := DashProc;
      'A'..'Z', 'a'..'z', '_'     : FProcTable[chI] := IdentProc;
      '#', '$'                    : FProcTable[chI] := IntegerProc;
      #10                         : FProcTable[chI] := LFProc;
      #0                          : FProcTable[chI] := NullProc;
      '0'..'9'                    : FProcTable[chI] := NumberProc;
      ')', '('                    : FProcTable[chI] := RoundOpenProc;
      '/'                         : FProcTable[chI] := SlashProc;
      #1..#9, #11, #12, #14..#32  : FProcTable[chI] := SpaceProc;
      #39                         : FProcTable[chI] := StringProc;
    else
      FProcTable[chI] := UnknownProc;
    end;
end;

constructor TSynCssSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterCSS;
  fRange := rsUnknown;
end;

procedure TSynCssSyn.SetLine(NewValue: String; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynCssSyn.AsciiCharProc;
begin
  FTokenID := tkString;
  Inc(Run);
  while FLine[Run] in ['0'..'9'] do
    Inc(Run);
end;

procedure TSynCssSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynCssSyn.CStyleCommentProc;
begin
  if FLine[Run] = #0 then
    FTokenID := tkNull
  else begin
    FTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then begin
        FRange := rsUnKnown;
        Inc(Run, 2);
        Break;
      end;
      Inc(Run);
    until FLine[Run] in [#0, #10, #13];
  end;
end;

procedure TSynCssSyn.DashProc;
begin
  if (FLine[Run - 1] = ' ') then begin
    if (FLine[Run + 1] in ['0'..'9', 'a'..'z', 'A'..'Z']) then
      FTokenID := tkNumber
    else if (FLine[Run + 1] = '.') and (FLine[Run + 2] in ['0'..'9', 'a'..'z', 'A'..'Z']) then begin
      FTokenID := tkNumber;
      Inc(Run);
    end;
  end else
    FTokenID := tkIdentifier;
  Inc(Run);
end;

procedure TSynCssSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + Run));
  Inc(Run, FStringLen);
  while Identifiers[FLine[Run]] do
    Inc(Run);
end;

procedure TSynCssSyn.IntegerProc;
begin
  Inc(Run);
  FTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f'] do
    Inc(Run);
end;

procedure TSynCssSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynCssSyn.NullProc;
begin
  FTokenID := tkNull;
end;

procedure TSynCssSyn.NumberProc;
begin
  Inc(Run);
  FTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'e', 'E'] do begin
    if ((FLine[Run] = '.') and (FLine[Run + 1] = '.')) or
       ((FLine[Run] = 'e') and ((FLine[Run + 1] = 'x') or (FLine[Run + 1] = 'm'))) then
      Break;
    Inc(Run);
  end;
end;

procedure TSynCssSyn.RoundOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynCssSyn.SlashProc;
begin
  Inc(Run);
  if fLine[Run] = '*' then begin
    FTokenID := tkComment;
    FRange := rsCStyle;
    Inc(Run);
    if not (FLine[Run] in [#0, #10, #13]) then
      CStyleCommentProc;
  end else
    FTokenID := tkSymbol;
end;

procedure TSynCssSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do
    Inc(Run);
end;

procedure TSynCssSyn.StringProc;
begin
  FTokenID := tkString;
  if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then
    Inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
    end;
    Inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then
    Inc(Run);
end;

procedure TSynCssSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run,2)
  else
{$ENDIF}
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynCssSyn.Next;
begin
  FTokenPos := Run;
  if FRange = rsCStyle then
    CStyleCommentProc
  else
    FProcTable[FLine[Run]];
end;

function TSynCssSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT   : Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD   : Result := FKeyAttri;
    SYN_ATTR_STRING    : Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL    : Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynCssSyn.GetEol: Boolean;
begin
  Result := (FTokenID = tkNull);
end;

function TSynCssSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynCssSyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - FTokenPos;
  SetString(Result, (FLine + FTokenPos), Len);
end;

function TSynCssSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenId;
end;

function TSynCssSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment   : Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey       : Result := FKeyAttri;
    tkNumber    : Result := FNumberAttri;
    tkSpace     : Result := FSpaceAttri;
    tkString    : Result := FStringAttri;
    tkSymbol    : Result := FSymbolAttri;
    tkUnknown   : Result := FIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynCssSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynCssSyn.GetTokenPos: Integer;
begin
  Result := FTokenPos;
end;

procedure TSynCssSyn.ReSetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynCssSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynCssSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}                                             //mh 2000-07-14
function TSynCssSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCSS;
end;

function TSynCssSyn.GetSampleSource: String;
begin
  Result := '/* Syntax highlighting */'#13#10 +
    'body'#13#10 +
    '{'#13#10 +
    '  font-family: "Arial";'#13#10 +
    '  font-size: 12pt;'#13#10 +
    '}';
end;

function TSynCSSSyn.KeyHash2(ToHash: PChar): Integer;
begin
  Result := KeyHash(ToHash);
end;


initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}                                                            //mh 2000-07-14
  RegisterPlaceableHighlighter(TSynCssSyn);
{$ENDIF}
end.
