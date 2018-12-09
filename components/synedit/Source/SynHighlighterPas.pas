{------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPas.pas, released 2000-04-17.
The Original Code is based on the mwPasSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Martin Waldenburg.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
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

$Id: SynHighlighterPas.pas,v 1.27.2.10 2009/02/23 15:43:50 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Pascal/Delphi syntax highlighter for SynEdit)
@author(Martin Waldenburg)
@created(1998, converted to SynEdit 2000-04-07)
@lastmod(2004-03-19)
The SynHighlighterPas unit provides SynEdit with a Object Pascal syntax highlighter.
Two extra properties included (DelphiVersion, PackageSource):
  DelphiVersion - Allows you to enable/disable the highlighting of various
                  language enhancements added in the different Delphi versions.
  PackageSource - Allows you to enable/disable the highlighting of package keywords
}

{$IFNDEF QSYNHIGHLIGHTERPAS}
unit SynHighlighterPas;
{$ENDIF}

{$I SynEdit.Inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
  QSynUnicode,
{$ELSE}
  Windows,
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
{$ENDIF}
  SysUtils,
{$IFDEF SYN_CodeFolding}
  SynEditCodeFolding,
  SynRegExpr,
{$ENDIF}
  Classes;

type
  TtkTokenKind = (tkAsm, tkComment, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown, tkFloat, tkHex, tkDirec, tkChar);

  TRangeState = (rsANil, rsAnsi, rsAnsiAsm, rsAsm, rsBor, rsBorAsm, rsProperty,
    rsExports, rsDirective, rsDirectiveAsm, rsUnknown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TDelphiVersion = (dvDelphi1, dvDelphi2, dvDelphi3, dvDelphi4, dvDelphi5,
    dvDelphi6, dvDelphi7, dvDelphi8, dvDelphi2005);

const
  LastDelphiVersion = dvDelphi2005;
  BDSVersionPrefix = 'BDS';

type
{$IFDEF SYN_CodeFolding}
  TSynPasSyn = class(TSynCustomCodeFoldingHighlighter)
{$ELSE}
  TSynPasSyn = class(TSynCustomHighlighter)
{$ENDIF}
  private
    FAsmStart: Boolean;
    FRange: TRangeState;
    FIdentFuncTable: array[0..388] of TIdentFuncTableFunc;
    FTokenID: TtkTokenKind;
    FStringAttri: TSynHighlighterAttributes;
    FCharAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FFloatAttri: TSynHighlighterAttributes;
    FHexAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FAsmAttri: TSynHighlighterAttributes;
    FCommentAttri: TSynHighlighterAttributes;
    FDirecAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FDelphiVersion: TDelphiVersion;
    FPackageSource: Boolean;
{$IFDEF SYN_CodeFolding}
    RE_BlockBegin : TRegExpr;
    RE_BlockEnd : TRegExpr;
    RE_Code: TRegExpr;
{$ENDIF}
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function FuncAsm(Index: Integer): TtkTokenKind;
    function FuncAutomated(Index: Integer): TtkTokenKind;
    function FuncCdecl(Index: Integer): TtkTokenKind;
    function FuncContains(Index: Integer): TtkTokenKind;
    function FuncDeprecated(Index: Integer): TtkTokenKind;
    function FuncDispid(Index: Integer): TtkTokenKind;
    function FuncDispinterface(Index: Integer): TtkTokenKind;
    function FuncEnd(Index: Integer): TtkTokenKind;
    function FuncExports(Index: Integer): TtkTokenKind;
    function FuncFinal(Index: Integer): TtkTokenKind;
    function FuncFinalization(Index: Integer): TtkTokenKind;
    function FuncHelper(Index: Integer): TtkTokenKind;
    function FuncImplements(Index: Integer): TtkTokenKind;
    function FuncIndex(Index: Integer): TtkTokenKind;
    function FuncName(Index: Integer): TtkTokenKind;
    function FuncNodefault(Index: Integer): TtkTokenKind;
    function FuncOperator(Index: Integer): TtkTokenKind;
    function FuncOverload(Index: Integer): TtkTokenKind;
    function FuncPackage(Index: Integer): TtkTokenKind;
    function FuncPlatform(Index: Integer): TtkTokenKind;
    function FuncProperty(Index: Integer): TtkTokenKind;
    function FuncRead(Index: Integer): TtkTokenKind;
    function FuncReadonly(Index: Integer): TtkTokenKind;
    function FuncReintroduce(Index: Integer): TtkTokenKind;
    function FuncRequires(Index: Integer): TtkTokenKind;
    function FuncResourcestring(Index: Integer): TtkTokenKind;
    function FuncSafecall(Index: Integer): TtkTokenKind;
    function FuncSealed(Index: Integer): TtkTokenKind;
    function FuncStdcall(Index: Integer): TtkTokenKind;
    function FuncStored(Index: Integer): TtkTokenKind;
    function FuncStringresource(Index: Integer): TtkTokenKind;
    function FuncThreadvar(Index: Integer): TtkTokenKind;
    function FuncWrite(Index: Integer): TtkTokenKind;
    function FuncWriteonly(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure AddressOpProc;
    procedure AsciiCharProc;
    procedure AnsiProc;
    procedure BorProc;
    procedure BraceOpenProc;
    procedure ColonOrGreaterProc;
    procedure CRProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PointProc;
    procedure RoundOpenProc;
    procedure SemicolonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    procedure SetDelphiVersion(const Value: TDelphiVersion);
    procedure SetPackageSource(const Value: Boolean);
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetCapabilities: TSynHighlighterCapabilities; override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override; 
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function UseUserSettings(VersionIndex: Integer): Boolean; override;
    procedure EnumUserSettings(DelphiVersions: TStrings); override;
{$IFDEF SYN_CodeFolding}
    procedure ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine: Integer; ToLine: Integer); override;
    procedure AdjustFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings); override;
{$ENDIF}
  published
    property AsmAttri: TSynHighlighterAttributes read FAsmAttri write FAsmAttri;
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property DirectiveAttri: TSynHighlighterAttributes read FDirecAttri
      write FDirecAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property FloatAttri: TSynHighlighterAttributes read FFloatAttri
      write FFloatAttri;
    property HexAttri: TSynHighlighterAttributes read FHexAttri
      write FHexAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri
      write FStringAttri;
    property CharAttri: TSynHighlighterAttributes read FCharAttri
      write FCharAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
    property DelphiVersion: TDelphiVersion read FDelphiVersion write SetDelphiVersion
      default LastDelphiVersion;
    property PackageSource: Boolean read FPackageSource write SetPackageSource default True;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  // if the language is case-insensitive keywords *must* be in lowercase
  KeyWords: array[0..110] of UnicodeString = (
    'absolute', 'abstract', 'and', 'array', 'as', 'asm', 'assembler',
    'automated', 'begin', 'case', 'cdecl', 'class', 'const', 'constructor',
    'contains', 'default', 'deprecated', 'destructor', 'dispid',
    'dispinterface', 'div', 'do', 'downto', 'dynamic', 'else', 'end', 'except',
    'export', 'exports', 'external', 'far', 'file', 'final', 'finalization',
    'finally', 'for', 'forward', 'function', 'goto', 'helper', 'if',
    'implementation', 'implements', 'in', 'index', 'inherited',
    'initialization', 'inline', 'interface', 'is', 'label', 'library',
    'message', 'mod', 'name', 'near', 'nil', 'nodefault', 'not', 'object', 'of',
    'on', 'operator', 'or', 'out', 'overload', 'override', 'package', 'packed',
    'pascal', 'platform', 'private', 'procedure', 'program', 'property',
    'protected', 'public', 'published', 'raise', 'read', 'readonly', 'record',
    'register', 'reintroduce', 'repeat', 'requires', 'resourcestring',
    'safecall', 'sealed', 'set', 'shl', 'shr', 'stdcall', 'stored', 'string',
    'stringresource', 'then', 'threadvar', 'to', 'try', 'type', 'unit', 'until',
    'uses', 'var', 'virtual', 'while', 'with', 'write', 'writeonly', 'xor'
  );

  KeyIndices: array[0..388] of Integer = (
    -1, -1, -1, 105, -1, 51, -1, 108, -1, -1, -1, -1, -1, 75, -1, -1, 46, -1,
    -1, 103, -1, -1, -1, -1, 55, -1, -1, -1, -1, 76, -1, -1, 96, 14, -1, 31, 3,
    102, -1, -1, -1, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, 78, -1, -1, 25, -1,
    -1, 56, 65, 95, -1, -1, -1, 34, -1, 85, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 22, -1, -1, -1, -1, -1, -1, 80, -1, -1, -1, -1, 50, -1, -1, 109, 98, -1,
    86, -1, 13, -1, -1, -1, 107, -1, -1, 60, -1, 0, 64, -1, -1, -1, -1, 8, 10,
    -1, -1, -1, 67, -1, -1, -1, 74, -1, 17, -1, 73, 69, -1, 68, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 16, -1, -1, 23, 39, -1, 35, 30, -1, -1, -1, 70, -1, 37,
    -1, -1, 89, 71, 84, 72, -1, 29, 40, -1, -1, -1, 32, -1, -1, -1, 94, -1, -1,
    87, -1, -1, -1, -1, -1, -1, 77, -1, -1, -1, -1, -1, -1, 11, 57, 41, 6, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 24, -1, -1, -1, -1, 97, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 44, 12, -1, -1, 101, -1, 58, -1, -1, -1, 99, -1, -1,
    -1, -1, 53, 20, -1, -1, -1, 36, -1, -1, 63, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 45, -1, -1, -1, -1, 27, -1, -1, -1, -1, -1, 59,
    -1, 110, -1, 15, -1, 52, -1, -1, -1, -1, 5, 48, -1, -1, -1, 81, -1, 28, -1,
    -1, -1, 2, -1, 1, -1, 106, -1, -1, -1, -1, 90, -1, 83, -1, -1, -1, -1, -1,
    79, -1, -1, 33, 62, -1, -1, -1, -1, -1, -1, 4, -1, -1, -1, -1, -1, -1, 88,
    61, 54, -1, 42, -1, -1, -1, 66, -1, -1, -1, 92, 100, -1, -1, -1, -1, -1, 18,
    -1, -1, 26, 47, 38, -1, -1, 93, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    9, -1, 91, -1, -1, -1, -1, -1, -1, 49, -1, 21, -1, -1, -1, -1, -1, -1, 43,
    -1, 82, -1, 19, 104, -1, -1, -1, -1, -1
  );

{$Q-}
function TSynPasSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 812 + Ord(Str^) * 76;
    Inc(Str);
  end;
  Result := Result mod 389;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynPasSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynPasSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  FIdentFuncTable[275] := FuncAsm;
  FIdentFuncTable[41] := FuncAutomated;
  FIdentFuncTable[112] := FuncCdecl;
  FIdentFuncTable[33] := FuncContains;
  FIdentFuncTable[137] := FuncDeprecated;
  FIdentFuncTable[340] := FuncDispid;
  FIdentFuncTable[382] := FuncDispinterface;
  FIdentFuncTable[54] := FuncEnd;
  FIdentFuncTable[282] := FuncExports;
  FIdentFuncTable[163] := FuncFinal;
  FIdentFuncTable[306] := FuncFinalization;
  FIdentFuncTable[141] := FuncHelper;
  FIdentFuncTable[325] := FuncImplements;
  FIdentFuncTable[214] := FuncIndex;
  FIdentFuncTable[323] := FuncName;
  FIdentFuncTable[185] := FuncNodefault;
  FIdentFuncTable[307] := FuncOperator;
  FIdentFuncTable[58] := FuncOverload;
  FIdentFuncTable[116] := FuncPackage;
  FIdentFuncTable[148] := FuncPlatform;
  FIdentFuncTable[120] := FuncProperty;
  FIdentFuncTable[303] := FuncRead;
  FIdentFuncTable[83] := FuncReadonly;
  FIdentFuncTable[297] := FuncReintroduce;
  FIdentFuncTable[65] := FuncRequires;
  FIdentFuncTable[94] := FuncResourcestring;
  FIdentFuncTable[170] := FuncSafecall;
  FIdentFuncTable[321] := FuncSealed;
  FIdentFuncTable[333] := FuncStdcall;
  FIdentFuncTable[348] := FuncStored;
  FIdentFuncTable[59] := FuncStringresource;
  FIdentFuncTable[204] := FuncThreadvar;
  FIdentFuncTable[7] := FuncWrite;
  FIdentFuncTable[91] := FuncWriteonly;

  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if @FIdentFuncTable[i] = nil then
      FIdentFuncTable[i] := KeyWordFunc;
end;

function TSynPasSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier
end;

function TSynPasSyn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

function TSynPasSyn.FuncAsm(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    Result := tkKey;
    FRange := rsAsm;
    FAsmStart := True;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncAutomated(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncCdecl(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi2) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncContains(Index: Integer): TtkTokenKind;
begin
  if PackageSource and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncDeprecated(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi6) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncDispid(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncDispinterface(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncEnd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    Result := tkKey;
    FRange := rsUnknown;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncExports(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    Result := tkKey;
    FRange := rsExports;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncFinal(Index: Integer): TtkTokenKind;
begin
 if (DelphiVersion >= dvDelphi8) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncFinalization(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi2) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncHelper(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi8) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncImplements(Index: Integer): TtkTokenKind;
begin
  if (FRange = rsProperty) and (DelphiVersion >= dvDelphi4) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncIndex(Index: Integer): TtkTokenKind;
begin
  if (FRange in [rsProperty, rsExports]) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncName(Index: Integer): TtkTokenKind;
begin
  if (FRange = rsExports) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncNodefault(Index: Integer): TtkTokenKind;
begin
  if (FRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncOperator(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi8) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncOverload(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi4) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncPackage(Index: Integer): TtkTokenKind;
begin
  if PackageSource and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncPlatform(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi6) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncProperty(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    Result := tkKey;
    FRange := rsProperty;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncRead(Index: Integer): TtkTokenKind;
begin
  if (FRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncReadonly(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and (FRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncReintroduce(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi4) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncRequires(Index: Integer): TtkTokenKind;
begin
  if PackageSource and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncResourcestring(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncSafecall(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncSealed(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi8) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncStdcall(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi2) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncStored(Index: Integer): TtkTokenKind;
begin
  if (FRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncStringresource(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncThreadvar(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncWrite(Index: Integer): TtkTokenKind;
begin
  if (FRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncWriteonly(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and (FRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

constructor TSynPasSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaseSensitive := False;

  FDelphiVersion := LastDelphiVersion;
  FPackageSource := True;

  FAsmAttri := TSynHighlighterAttributes.Create(SYNS_AttrAssembler, SYNS_FriendlyAttrAssembler);
  AddAttribute(FAsmAttri);
  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style:= [fsItalic];
  AddAttribute(FCommentAttri);
  FDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  FDirecAttri.Style:= [fsItalic];
  AddAttribute(FDirecAttri);
  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style:= [fsBold];
  AddAttribute(FKeyAttri);
  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);
  FFloatAttri := TSynHighlighterAttributes.Create(SYNS_AttrFloat, SYNS_FriendlyAttrFloat);
  AddAttribute(FFloatAttri);
  FHexAttri := TSynHighlighterAttributes.Create(SYNS_AttrHexadecimal, SYNS_FriendlyAttrHexadecimal);
  AddAttribute(FHexAttri);
  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);
  FCharAttri := TSynHighlighterAttributes.Create(SYNS_AttrCharacter, SYNS_FriendlyAttrCharacter);
  AddAttribute(FCharAttri);
  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);

  InitIdent;
  FRange := rsUnknown;
  FAsmStart := False;
  FDefaultFilter := SYNS_FilterPascal;

{$IFDEF SYN_CodeFolding}
  RE_BlockBegin := TRegExpr.Create;
  RE_BlockBegin.Expression := '\b(begin|record|class)\b';
  RE_BlockBegin.ModifierI := True;

  RE_BlockEnd := TRegExpr.Create;
  RE_BlockEnd.Expression := '\bend\b';
  RE_BlockEnd.ModifierI := True;

  RE_Code := TRegExpr.Create;
  RE_Code.Expression := '^\s*(function|procedure)\b';
  RE_Code.ModifierI := True;
{$ENDIF}
end;

destructor TSynPasSyn.Destroy; 
begin 
{$IFDEF SYN_CodeFolding} 
  FreeAndNil(RE_BlockBegin); 
  FreeAndNil(RE_BlockEnd); 
  FreeAndNil(RE_Code); 
{$ENDIF} 
  inherited; 
end; 

procedure TSynPasSyn.AddressOpProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] = '@' then Inc(Run);
end;

procedure TSynPasSyn.AsciiCharProc;

  function IsAsciiChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', '$', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;
  
begin
  FTokenID := tkChar;
  Inc(Run);
  while IsAsciiChar do
    Inc(Run);
end;

procedure TSynPasSyn.BorProc;
begin
  case FLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      if FRange in [rsDirective, rsDirectiveAsm] then
        FTokenID := tkDirec
      else
        FTokenID := tkComment;
      repeat
        if FLine[Run] = '}' then
        begin
          Inc(Run);
          if FRange in [rsBorAsm, rsDirectiveAsm] then
            FRange := rsAsm
          else
            FRange := rsUnknown;
          Break;
        end;
        Inc(Run);
      until IsLineEnd(Run);
    end;
  end;
end;

procedure TSynPasSyn.BraceOpenProc;
begin
  if (FLine[Run + 1] = '$') then
  begin
    if FRange = rsAsm then
      FRange := rsDirectiveAsm
    else
      FRange := rsDirective;
  end
  else
  begin
    if FRange = rsAsm then
      FRange := rsBorAsm
    else
      FRange := rsBor;
  end;
  BorProc;
end;

procedure TSynPasSyn.ColonOrGreaterProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] = '=' then Inc(Run);
end;

procedure TSynPasSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynPasSyn.IdentProc;
begin
  FTokenID := IdentKind(FLine + Run);
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
end;

procedure TSynPasSyn.IntegerProc;

  function IsIntegerChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;
  
begin
  Inc(Run);
  FTokenID := tkHex;
  while IsIntegerChar do
    Inc(Run);
end;

procedure TSynPasSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynPasSyn.LowerProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if (FLine[Run] = '=') or (FLine[Run] = '>') then
    Inc(Run);
end;

procedure TSynPasSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynPasSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', '.', 'e', 'E', '-', '+':
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
          Break
        else
          FTokenID := tkFloat;
      'e', 'E': FTokenID := tkFloat;
      '-', '+':
        begin
          if FTokenID <> tkFloat then // arithmetic
            Break;
          if (FLine[Run - 1] <> 'e') and (FLine[Run - 1] <> 'E') then
            Break; //float, but it ends here
        end;
    end;
    Inc(Run);
  end;
end; 

procedure TSynPasSyn.PointProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if (FLine[Run] = '.') or (FLine[Run - 1] = ')') then
    Inc(Run);
end; 

procedure TSynPasSyn.AnsiProc;
begin
  case FLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    FTokenID := tkComment;
    repeat
      if (FLine[Run] = '*') and (FLine[Run + 1] = ')') then begin
        Inc(Run, 2);
        if FRange = rsAnsiAsm then
          FRange := rsAsm
        else
          FRange := rsUnknown;
        Break;
      end;
      Inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynPasSyn.RoundOpenProc;
begin
  Inc(Run);
  case FLine[Run] of
    '*':
      begin
        Inc(Run);
        if FRange = rsAsm then
          FRange := rsAnsiAsm
        else
          FRange := rsAnsi;
        FTokenID := tkComment;
        if not IsLineEnd(Run) then
          AnsiProc;
      end;
    '.':
      begin
        Inc(Run);
        FTokenID := tkSymbol;
      end;
  else
    FTokenID := tkSymbol;
  end;
end;

procedure TSynPasSyn.SemicolonProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  if FRange in [rsProperty, rsExports] then
    FRange := rsUnknown;
end;

procedure TSynPasSyn.SlashProc;
begin
  Inc(Run);
  if (FLine[Run] = '/') and (FDelphiVersion > dvDelphi1) then
  begin
    FTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end
  else
    FTokenID := tkSymbol;
end;

procedure TSynPasSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynPasSyn.StringProc;
begin
  FTokenID := tkString;
  Inc(Run);
  while not IsLineEnd(Run) do
  begin
    if FLine[Run] = #39 then begin
      Inc(Run);
      if FLine[Run] <> #39 then
        Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynPasSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynPasSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynPasSyn.Next;
begin
  FAsmStart := False;
  FTokenPos := Run;
  case FRange of
    rsAnsi, rsAnsiAsm:
      AnsiProc;
    rsBor, rsBorAsm, rsDirective, rsDirectiveAsm:
      BorProc;
    else
      case FLine[Run] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        '#': AsciiCharProc;
        '$': IntegerProc;
        #39: StringProc;
        '0'..'9': NumberProc;
        'A'..'Z', 'a'..'z', '_': IdentProc;
        '{': BraceOpenProc;
        '}', '!', '"', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~':
          begin
            case FLine[Run] of
              '(': RoundOpenProc;
              '.': PointProc;
              ';': SemicolonProc;
              '/': SlashProc;
              ':', '>': ColonOrGreaterProc;
              '<': LowerProc;
              '@': AddressOpProc;
              else
                 SymbolProc;
            end;
          end;
        else
          UnknownProc;
      end;
  end;
  inherited;
end;

function TSynPasSyn.GetDefaultAttribute(Index: Integer):
  TSynHighlighterAttributes;
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

function TSynPasSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynPasSyn.GetTokenID: TtkTokenKind;
begin
  if not FAsmStart and (FRange = rsAsm)
    and not (FTokenID in [tkNull, tkComment, tkDirec, tkSpace])
  then
    Result := tkAsm
  else
    Result := FTokenID;
end;

function TSynPasSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkAsm: Result := FAsmAttri;
    tkComment: Result := FCommentAttri;
    tkDirec: Result := FDirecAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkFloat: Result := FFloatAttri;
    tkHex: Result := FHexAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkChar: Result := FCharAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynPasSyn.GetTokenKind: Integer;
begin
  Result := Ord(GetTokenID);
end;

function TSynPasSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

{$IFDEF SYN_CodeFolding}
type
  TRangeStates = set of TRangeState;

Const
  FT_Standard = 1;  // begin end, class end, record end
  FT_Comment = 11;
  FT_Asm = 12;
  FT_HereDocDouble = 13;
  FT_HereDocSingle = 14;
  FT_ConditionalDirective = 15;
  FT_CodeDeclaration = 16;
  FT_CodeDeclarationWithBody = 17;
  FT_Implementation = 18;

procedure TSynPasSyn.ScanForFoldRanges(FoldRanges: TSynFoldRanges;
  LinesToScan: TStrings; FromLine, ToLine: Integer);
var
  CurLine: String;
  Line: Integer;

  function BlockDelimiter(Line: Integer): Boolean;
  var
    Index: Integer;
  begin
    Result := False;

    if RE_BlockBegin.Exec(CurLine) then
    begin
      // Char must have proper highlighting (ignore stuff inside comments...)
      Index :=  RE_BlockBegin.MatchPos[0];
      if GetHighlighterAttriAtRowCol(LinesToScan, Line, Index) <> fCommentAttri then
      begin
        // And ignore lines with both opening and closing chars in them
        Re_BlockEnd.InputString := CurLine;
        if not RE_BlockEnd.Exec(Index + 1) then begin
          FoldRanges.StartFoldRange(Line + 1, FT_Standard);
          Result := True;
        end;
      end;
    end else if RE_BlockEnd.Exec(CurLine) then
    begin
      Index :=  RE_BlockBegin.MatchPos[0];
      if GetHighlighterAttriAtRowCol(LinesToScan, Line, Index) <> fCommentAttri then
      begin
        FoldRanges.StopFoldRange(Line + 1, FT_Standard);
        Result := True;
      end;
    end;
  end;

  function FoldRegion(Line: Integer): Boolean;
  var
    S: string;
  begin
    Result := False;
    S := TrimLeft(CurLine);
    if Uppercase(Copy(S, 1, 8)) = '{$REGION' then
    begin
      FoldRanges.StartFoldRange(Line + 1, FoldRegionType);
      Result := True;
    end
    else if Uppercase(Copy(S, 1, 11)) = '{$ENDREGION' then
    begin
      FoldRanges.StopFoldRange(Line + 1, FoldRegionType);
      Result := True;
    end;
  end;

  function ConditionalDirective(Line: Integer): Boolean;
  var
    S: string;
  begin
    Result := False;
    S := TrimLeft(CurLine);
    if Uppercase(Copy(S, 1, 7)) = '{$IFDEF' then
    begin
      FoldRanges.StartFoldRange(Line + 1, FT_ConditionalDirective);
      Result := True;
    end
    else if Uppercase(Copy(S, 1, 7)) = '{$ENDIF' then
    begin
      FoldRanges.StopFoldRange(Line + 1, FT_ConditionalDirective);
      Result := True;
    end;
  end;

  function IsMultiLineStatement(Line : integer; Ranges: TRangeStates;
     Fold : Boolean; FoldType: Integer = 1): Boolean;
  begin
    Result := True;
    if TRangeState(GetLineRange(LinesToScan, Line)) in Ranges then
    begin
      if Fold and not (TRangeState(GetLineRange(LinesToScan, Line - 1)) in Ranges) then
        FoldRanges.StartFoldRange(Line + 1, FoldType)
      else
        FoldRanges.NoFoldInfo(Line + 1);
    end
    else if Fold and (TRangeState(GetLineRange(LinesToScan, Line - 1)) in Ranges) then
    begin
      FoldRanges.StopFoldRange(Line + 1, FoldType);
    end else
      Result := False;
  end;

begin
  for Line := FromLine to ToLine do
  begin
    // Deal first with Multiline statements
    if IsMultiLineStatement(Line, [rsAnsi], True, FT_Comment) or
       IsMultiLineStatement(Line, [rsAsm, rsAnsiAsm, rsBorAsm, rsDirectiveAsm], True, FT_Asm) or
       IsMultiLineStatement(Line, [rsBor], True, FT_Comment) or
       IsMultiLineStatement(Line, [rsDirective], False)
    then
      Continue;

    CurLine := LinesToScan[Line];

    // Skip empty lines
    if CurLine = '' then begin
      FoldRanges.NoFoldInfo(Line + 1);
      Continue;
    end;

    //  Deal with ConditionalDirectives
    if ConditionalDirective(Line) then
      Continue;

    // Find Fold regions
    if FoldRegion(Line) then
      Continue;

    // Implementation
    if Uppercase(TrimLeft(CurLine)) = 'IMPLEMENTATION' then
      FoldRanges.StartFoldRange(Line +1, FT_Implementation)
    // Functions and procedures
    else if RE_Code.Exec(CurLine) then
      FoldRanges.StartFoldRange(Line +1, FT_CodeDeclaration)
    // Find begin or end  (Fold Type 1)
    else if not BlockDelimiter(Line) then
      FoldRanges.NoFoldInfo(Line + 1);
  end; //for Line
end;

procedure TSynPasSyn.AdjustFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings);
{
   Provide folding for procedures and functions included nested ones.
}
Var
  i, j, SkipTo: Integer;
  ImplementationIndex: Integer;
  FoldRange: TSynFoldRange;
begin
  ImplementationIndex := - 1;
  for i  := FoldRanges.Ranges.Count - 1 downto 0 do
  begin
    if FoldRanges.Ranges.List[i].FoldType = FT_Implementation then
      ImplementationIndex := i
    else if FoldRanges.Ranges.List[i].FoldType = FT_CodeDeclaration then
    begin
      if ImplementationIndex >= 0 then begin
        // Code declaration in the Interface part of a unit
        FoldRanges.Ranges.Delete(i);
        Dec(ImplementationIndex);
        continue;
      end;
      // Examine the following ranges
      SkipTo := 0;
      j := i + 1;
      while J < FoldRanges.Ranges.Count do begin
        FoldRange := FoldRanges.Ranges.List[j];
        Inc(j);
        case FoldRange.FoldType of
          // Nested procedure or function
          FT_CodeDeclarationWithBody:
            begin
              SkipTo := FoldRange.ToLine;
              continue;
            end;
          FT_Standard:
          // possibly begin end;
            if FoldRange.ToLine <= SkipTo then
              Continue
            else if RE_BlockBegin.Exec(LinesToScan[FoldRange.FromLine - 1]) then
            begin
              if LowerCase(RE_BlockBegin.Match[0]) = 'begin' then
              begin
                // function or procedure followed by begin end block
                // Adjust ToLine
                FoldRanges.Ranges.List[i].ToLine := FoldRange.ToLine;
                FoldRanges.Ranges.List[i].FoldType := FT_CodeDeclarationWithBody;
                break
              end else
              begin
                // class or record declaration follows, so
                FoldRanges.Ranges.Delete(i);
                break;
               end;
            end else
              Assert(False, 'TSynDWSSyn.AdjustFoldRanges');
        else
          begin
            if FoldRange.ToLine <= SkipTo then
              Continue
            else begin
              // Otherwise delete
              // eg. function definitions within a class definition
              FoldRanges.Ranges.Delete(i);
              break
            end;
          end;
        end;
      end;
    end;
  end;
  if ImplementationIndex >= 0 then
    // Looks better without it
    //FoldRanges.Ranges.List[ImplementationIndex].ToLine := LinesToScan.Count;
    FoldRanges.Ranges.Delete(ImplementationIndex);
end;
{$ENDIF}

procedure TSynPasSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynPasSyn.ResetRange;
begin
  FRange:= rsUnknown;
end;

procedure TSynPasSyn.EnumUserSettings(DelphiVersions: TStrings);

  procedure LoadKeyVersions(const Key, Prefix: string);
  var
    Versions: TStringList;
    i: Integer;
  begin
    with TBetterRegistry.Create do
    begin
      try
        RootKey := HKEY_LOCAL_MACHINE;
        if OpenKeyReadOnly(Key) then
        begin
          try
            Versions := TStringList.Create;
            try
              GetKeyNames(Versions);
              for i := 0 to Versions.Count - 1 do
                DelphiVersions.Add(Prefix + Versions[i]);
            finally
              FreeAndNil(Versions);
            end;
          finally
            CloseKey;
          end;
        end;
      finally
        Free;
      end;
    end;
  end;

begin
  { returns the user settings that exist in the registry }
{$IFNDEF SYN_CLX}
  // See UseUserSettings below where these strings are used
  LoadKeyVersions('\SOFTWARE\Borland\Delphi', '');
  LoadKeyVersions('\SOFTWARE\Borland\BDS', BDSVersionPrefix);
  LoadKeyVersions('\SOFTWARE\CodeGear\BDS', BDSVersionPrefix);
  LoadKeyVersions('\SOFTWARE\Embarcadero\BDS', BDSVersionPrefix);

{$ENDIF}
end;

function TSynPasSyn.UseUserSettings(VersionIndex: Integer): Boolean;
// Possible parameter values:
//   index into TStrings returned by EnumUserSettings
// Possible return values:
//   True : settings were read and used
//   False: problem reading settings or invalid version specified - old settings
//          were preserved

{$IFNDEF SYN_CLX}
  function ReadDelphiSettings(settingIndex: Integer): Boolean;

    function ReadDelphiSetting(settingTag: string; attri: TSynHighlighterAttributes; key: string): Boolean;
    var
      Version: Currency;
      VersionStr: string;

      function ReadDelphi2Or3(settingTag: string; attri: TSynHighlighterAttributes; name: string): Boolean;
      var
        i: Integer;
      begin
        for i := 1 to Length(name) do
          if name[i] = ' ' then name[i] := '_';
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
                '\Software\Borland\Delphi\'+settingTag+'\Highlight',name,True);
      end; { ReadDelphi2Or3 }

      function ReadDelphi4OrMore(settingTag: string; attri: TSynHighlighterAttributes; key: string): Boolean;
      begin
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
               '\Software\Borland\Delphi\'+settingTag+'\Editor\Highlight',key,False);
      end; { ReadDelphi4OrMore }

      function ReadDelphi8To2007(settingTag: string; attri: TSynHighlighterAttributes; key: string): Boolean;
      begin
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
               '\Software\Borland\BDS\'+settingTag+'\Editor\Highlight',key,False);
      end; { ReadDelphi8OrMore }

      function ReadDelphi2009OrMore(settingTag: string; attri: TSynHighlighterAttributes; key: string): Boolean;
      begin
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
               '\Software\CodeGear\BDS\'+settingTag+'\Editor\Highlight',key,False);
      end; { ReadDelphi2009OrMore }

      function ReadDelphiXEOrMore(settingTag: string; attri: TSynHighlighterAttributes; key: string): Boolean;
      begin
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
               '\Software\Embarcadero\BDS\'+settingTag+'\Editor\Highlight',key,False);
      end; { ReadDelphi2009OrMore }


    begin { ReadDelphiSetting }
      try
        if Pos('BDS', settingTag) = 1 then // BDS product
        begin
          VersionStr := Copy(settingTag, Length(BDSVersionPrefix) + 1, 999);
          Version := 0;
          if not TryStrToCurr(StringReplace(VersionStr, '.', {$IFDEF SYN_COMPILER_15_UP}FormatSettings.{$ENDIF}DecimalSeparator, []), Version) then
          begin
            Result := False;
            Exit;
          end;
          if Version >= 8 then
            Result := ReadDelphiXEOrMore(VersionStr, attri, key)
          else
            if Version >= 6 then
              Result := ReadDelphi2009OrMore(VersionStr, attri, key)
            else
              Result := ReadDelphi8To2007(VersionStr, attri, key);
        end
        else begin // Borland Delphi 7 or earlier
          if (settingTag[1] = '2') or (settingTag[1] = '3')
            then Result := ReadDelphi2Or3(settingTag, attri, key)
            else Result := ReadDelphi4OrMore(settingTag, attri, key);
        end;
      except Result := False; end;
    end; { ReadDelphiSetting }

  var
    tmpAsmAttri, tmpCommentAttri, tmpIdentAttri, tmpKeyAttri, tmpNumberAttri,
    tmpSpaceAttri, tmpStringAttri, tmpSymbolAttri: TSynHighlighterAttributes;
    iVersions: TStringList;
    iVersionTag: string;
  begin { ReadDelphiSettings }
    {$IFDEF SYN_COMPILER_7_UP}
    {$IFNDEF SYN_COMPILER_9_UP}
    Result := False; // Silence the compiler warning
    {$ENDIF}
    {$ENDIF}
    iVersions := TStringList.Create;
    try
      EnumUserSettings(iVersions);
      if (settingIndex < 0) or (settingIndex >= iVersions.Count) then
      begin
        Result := False;
        Exit;
      end;
      iVersionTag := iVersions[settingIndex];
    finally
      iVersions.Free;
    end;
    tmpAsmAttri     := TSynHighlighterAttributes.Create('', '');
    tmpCommentAttri := TSynHighlighterAttributes.Create('', '');
    tmpIdentAttri   := TSynHighlighterAttributes.Create('', '');
    tmpKeyAttri     := TSynHighlighterAttributes.Create('', '');
    tmpNumberAttri  := TSynHighlighterAttributes.Create('', '');
    tmpSpaceAttri   := TSynHighlighterAttributes.Create('', '');
    tmpStringAttri  := TSynHighlighterAttributes.Create('', '');
    tmpSymbolAttri  := TSynHighlighterAttributes.Create('', '');

    Result := ReadDelphiSetting(iVersionTag, tmpAsmAttri,'Assembler') and
      ReadDelphiSetting(iVersionTag, tmpCommentAttri,'Comment') and
      ReadDelphiSetting(iVersionTag, tmpIdentAttri,'Identifier') and
      ReadDelphiSetting(iVersionTag, tmpKeyAttri,'Reserved word') and
      ReadDelphiSetting(iVersionTag, tmpNumberAttri,'Number') and
      ReadDelphiSetting(iVersionTag, tmpSpaceAttri,'Whitespace') and
      ReadDelphiSetting(iVersionTag, tmpStringAttri,'String') and
      ReadDelphiSetting(iVersionTag, tmpSymbolAttri,'Symbol');

    if Result then
    begin
      FAsmAttri.AssignColorAndStyle(tmpAsmAttri);
      FCharAttri.AssignColorAndStyle(tmpStringAttri); { Delphi lacks Char attribute }
      FCommentAttri.AssignColorAndStyle(tmpCommentAttri);
      FDirecAttri.AssignColorAndStyle(tmpCommentAttri); { Delphi lacks Directive attribute }
      FFloatAttri.AssignColorAndStyle(tmpNumberAttri); { Delphi lacks Float attribute }
      FHexAttri.AssignColorAndStyle(tmpNumberAttri); { Delphi lacks Hex attribute }
      FIdentifierAttri.AssignColorAndStyle(tmpIdentAttri);
      FKeyAttri.AssignColorAndStyle(tmpKeyAttri);
      FNumberAttri.AssignColorAndStyle(tmpNumberAttri);
      FSpaceAttri.AssignColorAndStyle(tmpSpaceAttri);
      FStringAttri.AssignColorAndStyle(tmpStringAttri);
      FSymbolAttri.AssignColorAndStyle(tmpSymbolAttri);
    end;
    tmpAsmAttri.Free;
    tmpCommentAttri.Free;
    tmpIdentAttri.Free;
    tmpKeyAttri.Free;
    tmpNumberAttri.Free;
    tmpSpaceAttri.Free;
    tmpStringAttri.Free;
    tmpSymbolAttri.Free;
  end;
{$ENDIF}

begin
{$IFNDEF SYN_CLX}
  Result := ReadDelphiSettings(VersionIndex);
{$ELSE}
  Result := False;
{$ENDIF}
end;

function TSynPasSyn.GetSampleSource: UnicodeString;                                   
begin
  Result := '{ Syntax highlighting }'#13#10 +
             'procedure TForm1.Button1Click(Sender: TObject);'#13#10 +
             'var'#13#10 +
             '  Number, I, X: Integer;'#13#10 +
             'begin'#13#10 +
             '  Number := 123456;'#13#10 +
             '  Caption := ''The Number is'' + #32 + IntToStr(Number);'#13#10 +
             '  for I := 0 to Number do'#13#10 +
             '  begin'#13#10 +
             '    Inc(X);'#13#10 +
             '    Dec(X);'#13#10 +
             '    X := X + 1.0;'#13#10 +
             '    X := X - $5E;'#13#10 +
             '  end;'#13#10 +
             '  {$R+}'#13#10 +
             '  asm'#13#10 +
             '    mov AX, 1234H'#13#10 +
             '    mov Number, AX'#13#10 +
             '  end;'#13#10 +
             '  {$R-}'#13#10 +
             'end;';
end;


class function TSynPasSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPascal;
end;

class function TSynPasSyn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

function TSynPasSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterPascal;
end;

procedure TSynPasSyn.SetDelphiVersion(const Value: TDelphiVersion);
begin
  if FDelphiVersion <> Value then
  begin
    FDelphiVersion := Value;
    if (FDelphiVersion < dvDelphi3) and FPackageSource then
      FPackageSource := False;
    DefHighlightChange(Self);
  end;
end;

procedure TSynPasSyn.SetPackageSource(const Value: Boolean);
begin
  if FPackageSource <> Value then
  begin
    FPackageSource := Value;
    if FPackageSource and (FDelphiVersion < dvDelphi3) then
      FDelphiVersion := dvDelphi3;
    DefHighlightChange(Self);
  end;
end;

class function TSynPasSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangPascal;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynPasSyn);
{$ENDIF}
end.

