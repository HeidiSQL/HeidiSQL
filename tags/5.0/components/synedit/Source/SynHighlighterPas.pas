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

{$I SynEdit.inc}

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
  Classes;

type
  TtkTokenKind = (tkAsm, tkComment, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown, tkFloat, tkHex, tkDirec, tkChar);

  TRangeState = (rsANil, rsAnsi, rsAnsiAsm, rsAsm, rsBor, rsBorAsm, rsProperty,
    rsExports, rsDirective, rsDirectiveAsm, rsUnKnown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

  TDelphiVersion = (dvDelphi1, dvDelphi2, dvDelphi3, dvDelphi4, dvDelphi5,
    dvDelphi6, dvDelphi7, dvDelphi8, dvDelphi2005);

const
  LastDelphiVersion = dvDelphi2005;
  BDSVersionPrefix = 'BDS';

type
  TSynPasSyn = class(TSynCustomHighlighter)
  private
    fAsmStart: Boolean;
    fRange: TRangeState;
    fIdentFuncTable: array[0..388] of TIdentFuncTableFunc;
    fTokenID: TtkTokenKind;
    fStringAttri: TSynHighlighterAttributes;
    fCharAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fFloatAttri: TSynHighlighterAttributes;                                                 
    fHexAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fAsmAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fDirecAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fDelphiVersion: TDelphiVersion;
    fPackageSource: Boolean;
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
  published
    property AsmAttri: TSynHighlighterAttributes read fAsmAttri write fAsmAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property DirectiveAttri: TSynHighlighterAttributes read fDirecAttri
      write fDirecAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property FloatAttri: TSynHighlighterAttributes read fFloatAttri
      write fFloatAttri;
    property HexAttri: TSynHighlighterAttributes read fHexAttri
      write fHexAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property CharAttri: TSynHighlighterAttributes read fCharAttri
      write fCharAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property DelphiVersion: TDelphiVersion read fDelphiVersion write SetDelphiVersion
      default LastDelphiVersion;
    property PackageSource: Boolean read fPackageSource write SetPackageSource default True;
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
    inc(Str);
  end;
  Result := Result mod 389;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynPasSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynPasSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[275] := FuncAsm;
  fIdentFuncTable[41] := FuncAutomated;
  fIdentFuncTable[112] := FuncCdecl;
  fIdentFuncTable[33] := FuncContains;
  fIdentFuncTable[137] := FuncDeprecated;
  fIdentFuncTable[340] := FuncDispid;
  fIdentFuncTable[382] := FuncDispinterface;
  fIdentFuncTable[54] := FuncEnd;
  fIdentFuncTable[282] := FuncExports;
  fIdentFuncTable[163] := FuncFinal;
  fIdentFuncTable[306] := FuncFinalization;
  fIdentFuncTable[141] := FuncHelper;
  fIdentFuncTable[325] := FuncImplements;
  fIdentFuncTable[214] := FuncIndex;
  fIdentFuncTable[323] := FuncName;
  fIdentFuncTable[185] := FuncNodefault;
  fIdentFuncTable[307] := FuncOperator;
  fIdentFuncTable[58] := FuncOverload;
  fIdentFuncTable[116] := FuncPackage;
  fIdentFuncTable[148] := FuncPlatform;
  fIdentFuncTable[120] := FuncProperty;
  fIdentFuncTable[303] := FuncRead;
  fIdentFuncTable[83] := FuncReadonly;
  fIdentFuncTable[297] := FuncReintroduce;
  fIdentFuncTable[65] := FuncRequires;
  fIdentFuncTable[94] := FuncResourcestring;
  fIdentFuncTable[170] := FuncSafecall;
  fIdentFuncTable[321] := FuncSealed;
  fIdentFuncTable[333] := FuncStdcall;
  fIdentFuncTable[348] := FuncStored;
  fIdentFuncTable[59] := FuncStringresource;
  fIdentFuncTable[204] := FuncThreadvar;
  fIdentFuncTable[7] := FuncWrite;
  fIdentFuncTable[91] := FuncWriteonly;

  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if @fIdentFuncTable[i] = nil then
      fIdentFuncTable[i] := KeyWordFunc;
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
    fRange := rsAsm;
    fAsmStart := True;
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
    fRange := rsUnknown;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncExports(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    Result := tkKey;
    fRange := rsExports;
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
  if (fRange = rsProperty) and (DelphiVersion >= dvDelphi4) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncIndex(Index: Integer): TtkTokenKind;
begin
  if (fRange in [rsProperty, rsExports]) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncName(Index: Integer): TtkTokenKind;
begin
  if (fRange = rsExports) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncNodefault(Index: Integer): TtkTokenKind;
begin
  if (fRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
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
    fRange := rsProperty;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncRead(Index: Integer): TtkTokenKind;
begin
  if (fRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncReadonly(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and (fRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
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
  if (fRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
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
  if (fRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.FuncWriteonly(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and (fRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

constructor TSynPasSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCaseSensitive := False;

  fDelphiVersion := LastDelphiVersion;
  fPackageSource := True;

  fAsmAttri := TSynHighlighterAttributes.Create(SYNS_AttrAssembler, SYNS_FriendlyAttrAssembler);
  AddAttribute(fAsmAttri);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);
  fDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  fDirecAttri.Style:= [fsItalic];
  AddAttribute(fDirecAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fFloatAttri := TSynHighlighterAttributes.Create(SYNS_AttrFloat, SYNS_FriendlyAttrFloat);
  AddAttribute(fFloatAttri);
  fHexAttri := TSynHighlighterAttributes.Create(SYNS_AttrHexadecimal, SYNS_FriendlyAttrHexadecimal);
  AddAttribute(fHexAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fCharAttri := TSynHighlighterAttributes.Create(SYNS_AttrCharacter, SYNS_FriendlyAttrCharacter);
  AddAttribute(fCharAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);

  InitIdent;
  fRange := rsUnknown;
  fAsmStart := False;
  fDefaultFilter := SYNS_FilterPascal;
end;

procedure TSynPasSyn.AddressOpProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '@' then inc(Run);
end;

procedure TSynPasSyn.AsciiCharProc;

  function IsAsciiChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '$', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;
  
begin
  fTokenID := tkChar;
  Inc(Run);
  while IsAsciiChar do
    Inc(Run);
end;

procedure TSynPasSyn.BorProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      if fRange in [rsDirective, rsDirectiveAsm] then
        fTokenID := tkDirec
      else
        fTokenID := tkComment;
      repeat
        if fLine[Run] = '}' then
        begin
          Inc(Run);
          if fRange in [rsBorAsm, rsDirectiveAsm] then
            fRange := rsAsm
          else
            fRange := rsUnKnown;
          break;
        end;
        Inc(Run);
      until IsLineEnd(Run);
    end;
  end;
end;

procedure TSynPasSyn.BraceOpenProc;
begin
  if (fLine[Run + 1] = '$') then
  begin
    if fRange = rsAsm then
      fRange := rsDirectiveAsm
    else
      fRange := rsDirective;
  end
  else
  begin
    if fRange = rsAsm then
      fRange := rsBorAsm
    else
      fRange := rsBor;
  end;
  BorProc;
end;

procedure TSynPasSyn.ColonOrGreaterProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '=' then inc(Run);
end;

procedure TSynPasSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynPasSyn.IdentProc;
begin
  fTokenID := IdentKind(fLine + Run);
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do
    Inc(Run);
end;

procedure TSynPasSyn.IntegerProc;

  function IsIntegerChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;
  
begin
  inc(Run);
  fTokenID := tkHex;
  while IsIntegerChar do
    Inc(Run);
end;

procedure TSynPasSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynPasSyn.LowerProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if (fLine[Run] = '=') or (fLine[Run] = '>') then
    Inc(Run);
end;

procedure TSynPasSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynPasSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', 'e', 'E', '-', '+':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(Run);
  fTokenID := tkNumber;
  while IsNumberChar do
  begin
    case fLine[Run] of
      '.':
        if fLine[Run + 1] = '.' then
          Break
        else
          fTokenID := tkFloat;
      'e', 'E': fTokenID := tkFloat;
      '-', '+':
        begin
          if fTokenID <> tkFloat then // arithmetic
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
  fTokenID := tkSymbol;
  inc(Run);
  if (fLine[Run] = '.') or (fLine[Run - 1] = ')') then
    Inc(Run);
end; 

procedure TSynPasSyn.AnsiProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = ')') then begin
        Inc(Run, 2);
        if fRange = rsAnsiAsm then
          fRange := rsAsm
        else
          fRange := rsUnKnown;
        break;
      end;
      Inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynPasSyn.RoundOpenProc;
begin
  Inc(Run);
  case fLine[Run] of
    '*':
      begin
        Inc(Run);
        if fRange = rsAsm then
          fRange := rsAnsiAsm
        else
          fRange := rsAnsi;
        fTokenID := tkComment;
        if not IsLineEnd(Run) then
          AnsiProc;
      end;
    '.':
      begin
        inc(Run);
        fTokenID := tkSymbol;
      end;
  else
    fTokenID := tkSymbol;
  end;
end;

procedure TSynPasSyn.SemicolonProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fRange in [rsProperty, rsExports] then
    fRange := rsUnknown;
end;

procedure TSynPasSyn.SlashProc;
begin
  Inc(Run);
  if (fLine[Run] = '/') and (fDelphiVersion > dvDelphi1) then
  begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end
  else
    fTokenID := tkSymbol;
end;

procedure TSynPasSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynPasSyn.StringProc;
begin
  fTokenID := tkString;
  Inc(Run);
  while not IsLineEnd(Run) do
  begin
    if fLine[Run] = #39 then begin
      Inc(Run);
      if fLine[Run] <> #39 then
        break;
    end;
    Inc(Run);
  end;
end;

procedure TSynPasSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynPasSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynPasSyn.Next;
begin
  fAsmStart := False;
  fTokenPos := Run;
  case fRange of
    rsAnsi, rsAnsiAsm:
      AnsiProc;
    rsBor, rsBorAsm, rsDirective, rsDirectiveAsm:
      BorProc;
    else
      case fLine[Run] of
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
            case fLine[Run] of
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

function TSynPasSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynPasSyn.GetTokenID: TtkTokenKind;
begin
  if not fAsmStart and (fRange = rsAsm)
    and not (fTokenId in [tkNull, tkComment, tkDirec, tkSpace])
  then
    Result := tkAsm
  else
    Result := fTokenId;
end;

function TSynPasSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkAsm: Result := fAsmAttri;
    tkComment: Result := fCommentAttri;
    tkDirec: Result := fDirecAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkFloat: Result := fFloatAttri;
    tkHex: Result := fHexAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkChar: Result := fCharAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fSymbolAttri;
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
  Result := Pointer(fRange);
end;

procedure TSynPasSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynPasSyn.ResetRange;
begin
  fRange:= rsUnknown;
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

    begin { ReadDelphiSetting }
      try
        if Pos('BDS', settingTag) = 1 then // BDS product
        begin
          VersionStr := Copy(settingTag, Length(BDSVersionPrefix) + 1, 999);
          Version := 0;
          if not TryStrToCurr(StringReplace(VersionStr, '.', DecimalSeparator, []), Version) then
          begin
            Result := False;
            Exit;
          end;
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
      fAsmAttri.AssignColorAndStyle(tmpAsmAttri);
      fCharAttri.AssignColorAndStyle(tmpStringAttri); { Delphi lacks Char attribute }
      fCommentAttri.AssignColorAndStyle(tmpCommentAttri);
      fDirecAttri.AssignColorAndStyle(tmpCommentAttri); { Delphi lacks Directive attribute }
      fFloatAttri.AssignColorAndStyle(tmpNumberAttri); { Delphi lacks Float attribute }
      fHexAttri.AssignColorAndStyle(tmpNumberAttri); { Delphi lacks Hex attribute }
      fIdentifierAttri.AssignColorAndStyle(tmpIdentAttri);
      fKeyAttri.AssignColorAndStyle(tmpKeyAttri);
      fNumberAttri.AssignColorAndStyle(tmpNumberAttri);
      fSpaceAttri.AssignColorAndStyle(tmpSpaceAttri);
      fStringAttri.AssignColorAndStyle(tmpStringAttri);
      fSymbolAttri.AssignColorAndStyle(tmpSymbolAttri);
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
  Result := fDefaultFilter <> SYNS_FilterPascal;
end;

procedure TSynPasSyn.SetDelphiVersion(const Value: TDelphiVersion);
begin
  if fDelphiVersion <> Value then
  begin
    fDelphiVersion := Value;
    if (fDelphiVersion < dvDelphi3) and fPackageSource then
      fPackageSource := False;
    DefHighlightChange(Self);
  end;
end;

procedure TSynPasSyn.SetPackageSource(const Value: Boolean);
begin
  if fPackageSource <> Value then
  begin
    fPackageSource := Value;
    if fPackageSource and (fDelphiVersion < dvDelphi3) then
      fDelphiVersion := dvDelphi3;
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

