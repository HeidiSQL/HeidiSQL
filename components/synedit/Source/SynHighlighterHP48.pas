{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterHP48.pas, released 2000-06-23.
The Original Code is based on the cbHPSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Cyrille de Brebisson.
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

$Id: SynHighlighterHP48.pas,v 1.10.2.9 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides SynEdit with a HP48 assembler syntax highlighter.)
@author(Cyrille de Brebisson <cyrille_de-brebisson@aus.hp.com>, converted to SynEdit by David Muir <dhm@dmsoftware.co.uk>)
@created(1998-12, converted to SynEdit 2000-06-23)
@lastmod(2012-09-12)
The unit SynHighlighterHP48 provides SynEdit with a HP48 assembler highlighter.
}

unit SynHighlighterHP48;

{$I SynEdit.inc}

interface

uses
  Windows,
  Graphics,
  SynEditHighlighter,
  SynUnicode,
  SysUtils,
  Classes;

const
  NbSubList = 128;

type
  TSpeedStringList = class;

  TSpeedListObject = class
  protected
    FName: UnicodeString;
    FSpeedList: TSpeedStringList;
    FObject: TObject;
    procedure SetName(const Value: UnicodeString); virtual;
  public
    property Name: UnicodeString read FName write SetName;
    constructor Create(name: UnicodeString);
    destructor Destroy; override;
    property SpeedList: TSpeedStringList read FSpeedList write FSpeedList;
    property Pointer: TObject read FObject write FObject;
  end;

  PSpeedListObjects = ^TSpeedListObjects;
  TSpeedListObjects = array[0..0] of TSpeedListObject;

  TSpeedStringList = class
  private
    function GetText: UnicodeString;
    procedure SetText(const Value: UnicodeString);
    function GetInObject(Index: Integer): TObject;
    procedure SetInObject(Index: Integer; const Value: TObject);
  protected
    FOnChange: TNotifyEvent;
    FSumOfUsed: array[0..NbSubList - 1] of Integer;
    FDatasUsed: array[0..NbSubList - 1] of Integer;
    FDatas: array[0..NbSubList - 1] of PSpeedListObjects;
    FLengthDatas: array[0..NbSubList - 1] of Integer;
    procedure Changed; virtual;
    function Get(Index: Integer): UnicodeString; virtual;
    function GetObject(Index: Integer): TSpeedListObject;
    function GetCount: Integer;
    function GetStringList: TUnicodeStrings;
    procedure SetStringList(const Value: TUnicodeStrings);
  public
    procedure NameChange(const obj: TSpeedListObject; const NewName: UnicodeString);
    procedure ObjectDeleted(const obj: TSpeedListObject);

    destructor Destroy; override;
    constructor Create;
    function AddObj(const Value: TSpeedListObject): Integer;
    function Add(const Value: UnicodeString): TSpeedListObject;
    procedure Clear;
    function Find(const Name: UnicodeString): TSpeedListObject;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Objects[Index: Integer]: TSpeedListObject read GetObject;
    property InObject[Index: Integer]: TObject read GetInObject write SetInObject;
    property Strings[Index: Integer]: UnicodeString read Get; default;
    property Count: Integer read GetCount;
    property StringList: TUnicodeStrings read GetStringList write SetStringList;
    property Text: UnicodeString read GetText write SetText;
  end;

  TtkTokenKind = (tkNull, tkAsmKey, tkAsm, tkAsmComment, tksAsmKey, tksAsm,
    tksAsmComment, tkRplKey, tkRpl, tkRplComment);

  TRangeState = (rsRpl, rsComRpl, rssasm1, rssasm2, rssasm3, rsAsm, rsComAsm2,
    rsComAsm1);

  TSynHP48Syn = class(TSynCustomHighLighter)
  private
    FTockenKind: TtkTokenKind;
    FRange: TRangeState;
    FAttribs: array[TtkTokenKind] of TSynHighlighterAttributes;
    FRplKeyWords: TSpeedStringList;
    FAsmKeyWords: TSpeedStringList;
    FSAsmNoField: TSpeedStringList;
    FBaseRange: TRangeState;
    function GetAttrib(Index: Integer): TSynHighlighterAttributes;
    procedure SetAttrib(Index: Integer; Value: TSynHighlighterAttributes);

    function NullProc: TtkTokenKind;
    function SpaceProc: TtkTokenKind;
    function ParOpenProc: TtkTokenKind;
    function RplComProc: TtkTokenKind;
    function AsmComProc(c: WideChar): TtkTokenKind;
    function PersentProc: TtkTokenKind;
    function IdentProc: TtkTokenKind;
    function SlashProc: TtkTokenKind;
    function SasmProc1: TtkTokenKind;
    function SasmProc2: TtkTokenKind;
    function SasmProc3: TtkTokenKind;
    procedure EndOfToken;
    procedure SetHighLightChange;
    function Next1: TtkTokenKind;
    procedure Next2(tkk: TtkTokenKind);
    function GetTokenFromRange: TtkTokenKind;
    function StarProc: TtkTokenKind;
  protected
    function GetAttribCount: Integer; override;
    function GetAttribute(idx: Integer): TSynHighlighterAttributes; override;
    function IsFilterStored: Boolean; override;
    function IsLineEnd(Run: Integer): Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    procedure DoSetLine(const Value: UnicodeString; LineNumber: Integer); override;
    procedure Next; override;

    function GetToken: UnicodeString; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;

    function GetRange: Pointer; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;

    function SaveToRegistry(RootKey: HKEY; Key: string): Boolean; override;
    function LoadFromRegistry(RootKey: HKEY; Key: string): Boolean; override;

    procedure Assign(Source: TPersistent); override;
    property AsmKeyWords: TSpeedStringList read FAsmKeyWords;
    property SAsmFoField: TSpeedStringList read FSAsmNoField;
    property RplKeyWords: TSpeedStringList read FRplKeyWords;
  published
    property AsmKey: TSynHighlighterAttributes index Ord(tkAsmKey)
      read GetAttrib write SetAttrib;
    property AsmTxt: TSynHighlighterAttributes index Ord(tkAsm)
      read GetAttrib write SetAttrib;
    property AsmComment: TSynHighlighterAttributes index Ord(tkAsmComment)
      read GetAttrib write SetAttrib;
    property sAsmKey: TSynHighlighterAttributes index Ord(tksAsmKey)
      read GetAttrib write SetAttrib;
    property sAsmTxt: TSynHighlighterAttributes index Ord(tksAsm)
      read GetAttrib write SetAttrib;
    property sAsmComment: TSynHighlighterAttributes index Ord(tksAsmComment)
      read GetAttrib write SetAttrib;
    property RplKey: TSynHighlighterAttributes index Ord(tkRplKey)
      read GetAttrib write SetAttrib;
    property RplTxt: TSynHighlighterAttributes index Ord(tkRpl)
      read GetAttrib write SetAttrib;
    property RplComment: TSynHighlighterAttributes index Ord(tkRplComment)
      read GetAttrib write SetAttrib;
    property BaseRange: TRangeState read FBaseRange write FBaseRange;
  end;

implementation

uses
{$IFDEF UNICODE}
  WideStrUtils,
{$ENDIF}
  SynEditStrConst;

const
  DefaultAsmKeyWords: UnicodeString = '!RPL'#13#10'ENDCODE'#13#10'{'#13#10'}'#13#10 +
  'GOTO'#13#10'GOSUB'#13#10'GOSBVL'#13#10'GOVLNG'#13#10'GOLONG'#13#10'SKIP' +
    #13#10'SKIPYES' + #13#10'->'#13#10'SKUB'#13#10'SKUBL'#13#10'SKC'#13#10'SKNC'#13#10'SKELSE' +
    #13#10'SKEC'#13#10'SKENC'#13#10'SKLSE'#13#10 + 'GOTOL'#13#10'GOSUBL'#13#10 +
    'RTN'#13#10'RTNC'#13#10'RTNNC'#13#10'RTNSC'#13#10'RTNCC'#13#10'RTNSXM'#13#10'RTI';
  OtherAsmKeyWords: array[0..5] of UnicodeString = ('UP', 'EXIT', 'UPC', 'EXITC', 'UPNC', 'EXITNC');
  DefaultRplKeyWords: UnicodeString =
    'CODEM'#13#10'ASSEMBLEM'#13#10'CODE'#13#10'ASSEMBLE'#13#10'IT'#13#10'ITE'#13#10'case'#13#10'::'#13#10';'#13#10'?SEMI'#13#10''''#13#10'#=case'#13#10'{'#13#10'}'#13#10'NAMELESS'#13#10'LOCAL'#13#10'LOCALNAME'#13#10'LABEL'#13#10 +
    'LOCALLABEL'#13#10'xNAME'#13#10'tNAME' + 'COLA'#13#10'NULLNAME'#13#10'xROMID'#13#10'#0=ITE'#13#10'#<ITE'#13#10'#=ITE'#13#10'#>ITE'#13#10'2''RCOLARPITE'#13#10'ANDITE'#13#10'COLAITE'#13#10'COLARPITE'#13#10'DUP#0=ITE'#13#10 +
    'EQITE'#13#10'ITE'#13#10'RPITE'#13#10'SysITE'#13#10'UNxSYMRPITE'#13#10'UserITE'#13#10'snnSYMRPITE'#13#10'snsSYMRPITE'#13#10'ssnSYMRPITE'#13#10'sssSYMRPITE'#13#10'$_EXIT'#13#10'DA1OK?NOTIT'#13#10'DA2aOK?NOTIT'#13#10 +
    'DA2bOK?NOTIT'#13#10'DA3OK?NOTIT'#13#10'DO#EXIT'#13#10'DO$EXIT'#13#10'DO%EXIT'#13#10'DOHXSEXIT'#13#10'DUP#0=IT'#13#10'EQIT'#13#10'GCDHEULPEXIT'#13#10'GSPLIT'#13#10'NOT_IT'#13#10'POINTEXIT'#13#10'POLYARIT'#13#10'RPIT'#13#10 +
    'parleftIT'#13#10'parrightIT'#13#10''''#13#10'IT'#13#10'ITE'#13#10'SEMI'#13#10'UNTIL'#13#10'LOOP'#13#10'?SEMI'#13#10'NOT?SEMI'#13#10'#0=case'#13#10'#1=case'#13#10'#<>case'#13#10'#<case'#13#10'#=case'#13#10'#=casedrop'#13#10 +
    '#=casedrpfls'#13#10'#>2case'#13#10'#>33case'#13#10'#>case'#13#10'%-1=case'#13#10'%0=case'#13#10'%1=case'#13#10'%2=case'#13#10'AEQ1stcase'#13#10'AEQopscase'#13#10'ANDNOTcase'#13#10'ANDcase'#13#10'C%-1=case'#13#10 +
    'C%0=case'#13#10'C%1=case'#13#10'C%2=case'#13#10'COLANOTcase'#13#10'COLAcase'#13#10'DUP#0=case'#13#10'EQUALNOTcase'#13#10'EQUALcase'#13#10'EQUALcasedrop'#13#10'EQUALcasedrp'#13#10'EQcase'#13#10'EQcaseDROP'#13#10 +
    'EQcasedrop'#13#10'EnvNGcase'#13#10'M-1stcasechs'#13#10'MEQ*case'#13#10'MEQ+case'#13#10'MEQ-case'#13#10'MEQ/case'#13#10'MEQ1stcase'#13#10'MEQCHScase'#13#10'MEQFCNcase'#13#10'MEQINVcase'#13#10'MEQSQcase'#13#10'MEQ^case'#13#10 +
    'MEQopscase'#13#10'Mid1stcase'#13#10'NOTBAKcase'#13#10'NOTLIBcase'#13#10'NOTLISTcase'#13#10'NOTMATRIXcase'#13#10'NOTROMPcase'#13#10'NOTSECOcase'#13#10'NOTTYPEcase'#13#10'NOTcase'#13#10'NOTcase2DROP'#13#10'NOTcase2drop'#13#10 +
    'NOTcaseDROP'#13#10'NOTcaseFALSE'#13#10'NOTcaseTRUE'#13#10'NOTcasedrop'#13#10'NULLargcase'#13#10'NcaseSIZEERR'#13#10'NcaseTYPEERR'#13#10'NoEdit?case'#13#10'ORcase'#13#10'OVER#=case'#13#10'REALcase'#13#10'REQcase'#13#10 +
    'REQcasedrop'#13#10'Z-1=case'#13#10'Z0=case'#13#10'Z1=case'#13#10'accNBAKcase'#13#10'accNLIBcase'#13#10'case'#13#10'case2DROP'#13#10'case2drop'#13#10'case2drpfls'#13#10'caseDEADKEY'#13#10'caseDROP'#13#10'caseDoBadKey'#13#10 +
    'caseDrpBadKy'#13#10'caseERRJMP'#13#10'caseFALSE'#13#10'caseSIZEERR'#13#10'caseTRUE'#13#10'casedrop'#13#10'casedrpfls'#13#10'casedrptru'#13#10'caseout'#13#10'cxcasecheck'#13#10'dARRYcase'#13#10'dIDNTNcase'#13#10'dLISTcase'#13#10 +
    'dMATRIXcase'#13#10'dREALNcase'#13#10'dREALcase'#13#10'dZINTNcase'#13#10'delimcase'#13#10'estcase'#13#10'idntcase'#13#10'idntlamcase'#13#10'j#-1=case'#13#10'j#0=case'#13#10'j#1=case'#13#10'j%-1=case'#13#10'j%0=case'#13#10 +
    'j%1=case'#13#10'jEQcase'#13#10'jZ-1=case'#13#10'jZ0=case'#13#10'jZ1=case'#13#10'namelscase'#13#10'need''case'#13#10'negrealcase'#13#10'ngsizecase'#13#10'nonopcase'#13#10'nonrmcase'#13#10'num#-1=case'#13#10'num#0=case'#13#10 +
    'num#1=case'#13#10'num-1=case'#13#10'num0=case'#13#10'num0case'#13#10'num1=case'#13#10'num2=case'#13#10'numb1stcase'#13#10'rebuildcase'#13#10'tok=casedrop'#13#10'wildcase'#13#10'zerdercase'#13#10;
  SasmNoField: UnicodeString = 'LOOP'#13#10'RTNSXM'#13#10'RTN'#13#10'RTNSC'#13#10'RTNCC'#13#10'SETDEC'#13#10'SETHEX'#13#10'RSTK=C'#13#10'C=RSTK'#13#10'CLRST'#13#10'C=ST'#13#10'ST=C'#13#10'CSTEX'#13#10 +
  'RTI'#13#10'R0=A'#13#10'R1=A'#13#10'R2=A'#13#10'R3=A'#13#10'R4=A'#13#10'R0=C'#13#10'R1=C'#13#10'R2=C'#13#10'R3=C'#13#10'R4=C'#13#10'A=R0'#13#10'A=R1'#13#10'A=R2'#13#10'A=R3'#13#10'A=R4'#13#10 +
    'C=R0'#13#10'C=R1'#13#10'C=R2'#13#10'C=R3'#13#10'C=R4'#13#10'AR0EX'#13#10'AR1EX'#13#10'AR2EX'#13#10'AR3EX'#13#10'AR4EX'#13#10'CR0EX'#13#10'CR1EX'#13#10'CR2EX'#13#10'CR3EX'#13#10'CR4EX'#13#10 +
    'D0=A'#13#10'D0=C'#13#10'D1=A'#13#10'D1=C'#13#10'AD0EX'#13#10'AD1EX'#13#10'CD0EX'#13#10'CD1EX'#13#10'D0=AS'#13#10'D1=AS'#13#10'D0=CS'#13#10'D1=CD'#13#10'CD1XS'#13#10'CD0XS'#13#10'AD1XS'#13#10'AD0XS'#13#10 +
    'RTNC'#13#10'RTNNC'#13#10'OUT=CS'#13#10'OUT=C'#13#10'A=IN'#13#10'C=IN'#13#10'SHUTDN'#13#10'INTON'#13#10'C=ID'#13#10'CONFIG'#13#10'UNCNFG'#13#10'RSI'#13#10'PC=(A)'#13#10'PC=(C)'#13#10'INTOFF'#13#10 +
    'C+P+1'#13#10'RESET'#13#10'SREQ?'#13#10'ASLC'#13#10'BSLC'#13#10'CSLC'#13#10'DSLC'#13#10'ASRC'#13#10'BSRC'#13#10'CSRC'#13#10'DSRC'#13#10'ASRB'#13#10'BSRB'#13#10'CSRB'#13#10'DSRB'#13#10'PC=A'#13#10'PC=C'#13#10 +
    'A=PC'#13#10'C=PC'#13#10'APCEX'#13#10'CPCEX'#13#10'XM=0'#13#10'SB=0'#13#10'SR=0'#13#10'MP=0'#13#10'CLRHST'#13#10'?XM=0'#13#10'?SR=0'#13#10'?MP=0'#13#10'?SB=0'#13#10'RTNYES'#13#10'SKIPYES{'#13#10'{'#13#10'}'#13#10'UP'#13#10'EXIT'#13#10'EXITNC'#13#10'EXITC'#13#10'UPC'#13#10'UPNC' +
    '}SKELSE{'#13#10'SKC{'#13#10'SKNC{'#13#10'SKUB{'#13#10'SKUBL{'#13#10'SKIPC{'#13#10'SKIPNC{'#13#10'EXIT2'#13#10'EXIT3'#13#10'UP2'#13#10'UP3'#13#10'}SKLSE{'#13#10'}SKEC{'#13#10'}SKENC{'#13#10;

function StringCrc(S: UnicodeString): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to length(s) do begin
    Result := (Result shr 4) xor (((Result xor ord(s[i])) and $F) * $1081);
    Result := (Result shr 4) xor (((Result xor (ord(s[i]) shr 4)) and $F) * $1081);
  end;
end;

{ TSpeedListObject }

constructor TSpeedListObject.create(name: UnicodeString);
begin
  inherited create;
  FName := name;
end;

destructor TSpeedListObject.destroy;
begin
  if Assigned(FSpeedList) then
    FSpeedList.ObjectDeleted(Self);
  inherited destroy;
end;

procedure TSpeedListObject.SetName(const Value: UnicodeString);
begin
  FName := Value;
  if FSpeedList <> nil then
    FSpeedList.NameChange(Self, Value);
end;

{ TSpeedStringList }

function TSpeedStringList.AddObj(const Value: TSpeedListObject): Integer;
var
  crc: Integer;
  i: Integer;
begin
  crc := StringCrc(Value.Name) mod High(FDatas) + 1;
  if FDatasUsed[crc] = FLengthDatas[crc] then
  begin
    ReallocMem(FDatas[crc], (FLengthDatas[crc] * 2 + 1) * SizeOf(FDatas[1][0]));
    FLengthDatas[crc] := FLengthDatas[crc] * 2 + 1;
  end;
  FDatas[crc][FDatasUsed[crc]] := Value;
  Result := FSumOfUsed[crc] + FDatasUsed[crc];
  Inc(FDatasUsed[crc]);
  for i := crc + 1 to High(FSumOfUsed) do
    Inc(FSumOfUsed[i]);
  Value.SpeedList := Self;
end;

function TSpeedStringList.Add(const Value: UnicodeString): TSpeedListObject;
begin
  Result := TSpeedListObject.Create(value);
  AddObj(Result);
end;

procedure TSpeedStringList.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSpeedStringList.Clear;
var
  i, j: Integer;
begin
  for i := Low(FDatas) to High(FDatas) do
  begin
    for j := 0 to FDatasUsed[i] - 1 do
      FDatas[i][j].Free;
    FDatasUsed[i] := 0;
    ReallocMem(FDatas[i], 0);
    FLengthDatas[i] := 0;
    FSumOfUsed[i] := 0;
  end;
  Changed;
end;

constructor TSpeedStringList.create;
var
  i: Integer;
begin
  inherited Create;
  for i := Low(FDatas) to high(FDatas) do
  begin
    FSumOfUsed[i] := 0;
    FDatasUsed[i] := 0;
    FLengthDatas[i] := 0;
    FDatas[i] := nil;
  end;
end;

destructor TSpeedStringList.Destroy;
begin
  Clear;
  inherited destroy;
end;

function TSpeedStringList.Find(const name: UnicodeString): TSpeedListObject;
var
  crc: Integer;
  i: Integer;
begin
  crc := StringCrc(name) mod High(FDatas) + 1;
  for i := 0 to FDatasUsed[crc] - 1 do
    if FDatas[crc][i].name = name then begin
      Result := FDatas[crc][i];
      Exit;
    end;
  Result := nil;
end;

function TSpeedStringList.Get(Index: Integer): UnicodeString;
var
  i: Integer;
begin
  for i := Low(FSumOfUsed) + 1 to High(FSumOfUsed) do
    if Index > FSumOfUsed[i] then begin
      Result := FDatas[i - 1][Index - FSumOfUsed[i - 1]].name;
      Exit;
    end;
  Result := '';
end;

function TSpeedStringList.GetCount: Integer;
begin
  Result := FSumOfUsed[High(FDatas)] + FDatasUsed[High(FDatas)];
end;

function TSpeedStringList.GetInObject(Index: Integer): TObject;
var
  i: Integer;
begin
  for i := Low(FSumOfUsed) + 1 to High(FSumOfUsed) do
    if Index > FSumOfUsed[i] then begin
      Result := FDatas[i - 1][Index - FSumOfUsed[i - 1]].Pointer;
      Exit;
    end;
  Result := nil;
end;

function TSpeedStringList.GetObject(Index: Integer): TSpeedListObject;
var
  i: Integer;
begin
  for i := Low(FSumOfUsed) + 1 to High(FSumOfUsed) do
    if Index > FSumOfUsed[i] then begin
      Result := FDatas[i - 1][Index - FSumOfUsed[i - 1]];
      Exit;
    end;
  Result := nil;
end;

function TSpeedStringList.GetStringList: TUnicodeStrings;
var
  i, j: Integer;
begin
  Result := TUnicodeStringList.Create;
  for i := Low(FDatas) to High(FDatas) do
    for j := 0 to FDatasUsed[i] - 1 do
      Result.add(FDatas[i][j].name);
end;

function TSpeedStringList.GetText: UnicodeString;
begin
  with StringList do begin
    Result := Text;
    Free;
  end;
end;

procedure TSpeedStringList.NameChange(const Obj: TSpeedListObject; const NewName: UnicodeString);
var
  crc: Integer;
  i: Integer;
  j: Integer;
begin
  crc := StringCrc(obj.Name) mod High(FDatas) + 1;
  for i := 0 to FDatasUsed[crc] - 1 do
    if FDatas[crc][i] = Obj then begin
      for j := i + 1 to FDatasUsed[crc] - 1 do
        FDatas[i - 1] := FDatas[i];
      for j := crc + 1 to High(FDatas) do
        Dec(FSumOfUsed[j]);
      if FDatasUsed[crc] < FLengthDatas[crc] div 2 then begin
        ReallocMem(FDatas[crc], FDatasUsed[crc] * SizeOf(FDatas[crc][0]));
        FLengthDatas[crc] := FDatasUsed[crc];
      end;
      AddObj(Obj);
      Exit;
    end;
end;

procedure TSpeedStringList.ObjectDeleted(const obj: TSpeedListObject);
var
  crc: Integer;
  i: Integer;
  j: Integer;
begin
  crc := StringCrc(obj.Name) mod High(FDatas) + 1;
  for i := 0 to FDatasUsed[crc] - 1 do
    if FDatas[crc][i] = Obj then begin
      for j := i + 1 to FDatasUsed[crc] - 1 do
        if i > 0 then
          FDatas[i - 1] := FDatas[i];
      for j := crc + 1 to High(FDatas) do
        Dec(FSumOfUsed[j]);
      Obj.FSpeedList := nil;
      Exit;
    end;
end;

procedure TSpeedStringList.SetInObject(Index: Integer;
  const Value: TObject);
var
  i: Integer;
begin
  for i := Low(FSumOfUsed) + 1 to High(FSumOfUsed) do
    if Index > FSumOfUsed[i] then begin
      FDatas[i - 1][Index - FSumOfUsed[i - 1]].Pointer := value;
      Exit;
    end;
end;

procedure TSpeedStringList.SetStringList(const value: TUnicodeStrings);
var
  i: Integer;
begin
  clear;
  for i := 0 to Value.Count - 1 do
    AddObj(TSpeedListObject.Create(value[i]));
end;

procedure TSpeedStringList.SetText(const Value: UnicodeString);
var
  s: TUnicodeStrings;
begin
  s := TUnicodeStringList.Create;
  try
    s.Text := Value;
    StringList := s;
  finally
    s.Free;
  end;
end;

{ TSynHP48Syn }

constructor TSynHP48Syn.Create(AOwner: TComponent);
var
  j, k: Integer;
begin
  FAttribs[tkNull] := TSynHighlighterAttributes.Create(SYNS_AttrNull, SYNS_FriendlyAttrNull);
  FAttribs[tkAsmKey] := TSynHighlighterAttributes.Create(SYNS_AttrAsmKey, SYNS_FriendlyAttrAsmKey);
  FAttribs[tkAsm] := TSynHighlighterAttributes.Create(SYNS_AttrAsm, SYNS_FriendlyAttrAsm);
  FAttribs[tkAsmComment] := TSynHighlighterAttributes.Create(SYNS_AttrAsmComment, SYNS_FriendlyAttrAsmComment);
  FAttribs[tksAsmKey] := TSynHighlighterAttributes.Create(SYNS_AttrSASMKey, SYNS_FriendlyAttrSASMKey);
  FAttribs[tksAsm] := TSynHighlighterAttributes.Create(SYNS_AttrSASM, SYNS_FriendlyAttrSASM);
  FAttribs[tksAsmComment] := TSynHighlighterAttributes.Create(SYNS_AttrSASMComment, SYNS_FriendlyAttrSASMComment);
  FAttribs[tkRplKey] := TSynHighlighterAttributes.Create(SYNS_AttrRplKey, SYNS_FriendlyAttrRplKey);
  FAttribs[tkRpl] := TSynHighlighterAttributes.Create(SYNS_AttrRpl, SYNS_FriendlyAttrRpl);
  FAttribs[tkRplComment] := TSynHighlighterAttributes.Create(SYNS_AttrRplComment, SYNS_FriendlyAttrRplComment);

  inherited Create(AOwner);
  SetHighlightChange;
  FAsmKeyWords := TSpeedStringList.Create;
  FAsmKeyWords.Text := DefaultAsmKeyWords;
  for j := Low(OtherAsmKeyWords) to High(OtherAsmKeyWords) do begin
    FAsmKeyWords.AddObj(TSpeedListObject.Create(OtherAsmKeyWords[j]));
    for k := 1 to 8 do
      FAsmKeyWords.AddObj(TSpeedListObject.Create(OtherAsmKeyWords[j] + IntToStr(k)));
  end;
  FRplKeyWords := TSpeedStringList.Create;
  FRplKeyWords.Text := DefaultRplKeyWords;
  FSAsmNoField := TSpeedStringList.Create;
  FSAsmNoField.Text := SAsmNoField;
  BaseRange := rsRpl;
  FRange := rsRpl;
  FDefaultFilter := SYNS_FilterHP48;
end; { Create }

destructor TSynHP48Syn.Destroy;
var
  i: TtkTokenKind;
begin
  for i := Low(TtkTokenKind) to High(TtkTokenKind) do
    FAttribs[i].Free;
  FAsmKeyWords.Free;
  FRplKeyWords.Free;
  FSAsmNoField.Free;
  inherited Destroy;
end; { Destroy }

function TSynHP48Syn.AsmComProc(c: WideChar): TtkTokenKind;
begin
  Result := tkAsmComment;
  if (Run > Length(FLineStr)) then
    Result := NullProc
  else
    while Run <= Length(FLineStr) do
      if ((Run = 1) or (FLineStr[Run - 1] <= ' ')) and
        (FLineStr[Run] = '*') and
        ((Run < Length(FLineStr)) and (FLineStr[Run + 1] = c)) and
        ((Run + 1 = Length(FLineStr)) or (FLineStr[Run + 2] <= ' ')) then begin
        Inc(Run, 2);
        FRange := rsAsm;
        Break;
      end
      else
        Inc(Run);
end;

function TSynHP48Syn.RplComProc: TtkTokenKind;
begin
  Result := tkRplComment;
  if (Run > Length(FLineStr)) then
    Result := NullProc
  else
    while Run <= Length(FLineStr) do
      if FLineStr[Run] = ')' then begin
        Inc(Run);
        FRange := rsRpl;
        Break;
      end
      else
        Inc(Run);
end;

function TSynHP48Syn.SlashProc: TtkTokenKind;
begin
  if FRange = rsRpl then
    Result := IdentProc
  else if ((Run = 1) or (FLineStr[Run - 1] <= ' ')) and
    (FLineStr[Run] = '/') and
    (Run < Length(FLineStr)) and
    (FLineStr[Run + 1] = '*') and
    ((Run + 1 = Length(FLineStr)) or (FLineStr[Run + 2] <= ' ')) then begin
    Inc(Run, 2);
    Result := tkAsmComment;
    FRange := rsComAsm2;
  end
  else if (Run < Length(FLineStr)) and (FLineStr[Run + 1] = '/') then begin
    Inc(Run, 2);
    Result := tkAsmComment;
    while (Run <= Length(FLineStr)) do
      if CharInSet(FLineStr[Run], [#10, #13]) then
      begin
        Inc(Run);
        Break;
      end
      else
        Inc(Run);
  end
  else
    Result := IdentProc
end;

function TSynHP48Syn.ParOpenProc: TtkTokenKind;
begin
  if FRange = rsRpl then
    if ((Run = 1) and ((Length(FLineStr) = 1) or (FLineStr[Run + 1] <= ' '))) or
      ((FLineStr[Run - 1] <= ' ') and ((Length(FLineStr) = Run) or (FLineStr[Run + 1] <= ' '))) then begin
      Inc(Run);
      Result := tkRplComment;
      FRange := rsComRpl;
    end
    else
      Result := IdentProc
  else if ((Run = 1) or (FLineStr[Run - 1] <= ' ')) and
    (FLineStr[Run] = '(') and
    (Run < Length(FLineStr)) and
    (FLineStr[Run + 1] = '*') and
    ((Run + 2 > Length(FLineStr)) or (FLineStr[Run + 2] <= ' ')) then begin
    Inc(Run, 2);
    Result := tkAsmComment;
    FRange := rsComAsm1;
  end
  else
    Result := IdentProc
end;

function TSynHP48Syn.PersentProc: TtkTokenKind;
begin
  if FRange = rsAsm then begin
    Inc(Run);
    Result := tkAsmComment;
    while (Run <= Length(FLineStr)) do
      case FLineStr[Run] of
        #10, #13: begin
            Inc(Run);
            Break;
          end;
      else
        Inc(Run);
      end;
  end
  else
    Result := IdentProc;
end;

function TSynHP48Syn.StarProc: TtkTokenKind;
begin
  if FRange = rsRpl then begin
    Inc(Run);
    Result := tkRplComment;
    while (Run <= Length(FLineStr)) do
      case FLineStr[Run] of
        #10, #13: begin
            Inc(Run);
            Break;
          end;
      else
        Inc(Run);
      end;
  end
  else
    Result := IdentProc;
end;

function TSynHP48Syn.IdentProc: TtkTokenKind;
var
  i: Integer;
  s: UnicodeString;
begin
  i := Run;
  EndOfToken;
  s := Copy(FLineStr, i, Run - i);
  if FRange = rsAsm then
    if FAsmKeyWords.Find(s) <> nil then
      if (s = '!RPL') or (s = 'ENDCODE') then begin
        FRange := rsRpl;
        Result := tkAsmKey;
      end
      else
        Result := tkAsmKey
    else if FLineStr[i] <> '*' then
      Result := tkAsm
    else
      Result := tkAsmKey
  else if FRplKeyWords.Find(s) <> nil then
    if (s = 'CODEM') or (s = 'ASSEMBLEM') then begin
      FRange := rsAsm;
      Result := tkAsmKey;
    end
    else if (s = 'CODE') or (s = 'ASSEMBLE') then begin
      FRange := rssAsm1;
      Result := tksAsmKey;
    end
    else
      Result := tkRplKey
  else
    Result := tkRpl;
end;

function TSynHP48Syn.GetTokenFromRange: TtkTokenKind;
begin
  case FRange of
    rsAsm: Result := tkAsm;
    rssAsm1: Result := tksAsmKey;
    rssAsm2: Result := tksAsm;
    rssAsm3: Result := tksAsmComment;
    rsRpl: Result := tkRpl;
    rsComRpl: Result := tkRplComment;
    rsComAsm1, rsComAsm2: Result := tkAsmComment;
  else
    Result := tkNull;
  end;
end;

function TSynHP48Syn.NullProc: TtkTokenKind;
begin
  Result := tkNull;
  Inc(Run);
end;

function TSynHP48Syn.SpaceProc: TtkTokenKind;
begin
  Inc(Run);
  while (Run <= Length(FLineStr)) and CharInSet(FLineStr[Run], [#1..#32]) do
    Inc(Run);
  Result := GetTokenFromRange;
end;

function TSynHP48Syn.Next1: TtkTokenKind;
begin
  FTokenPos := Run - 1;
  if Run > Length(FLineStr) then
    Result := NullProc
  else if FRange = rsComRpl then
    Result := RplComProc
  else if FRange = rsComAsm1 then
    Result := AsmComProc(')')
  else if FRange = rsComAsm2 then
    Result := AsmComProc('/')
  else if FRange = rssasm1 then
    Result := SasmProc1
  else if FRange = rssasm2 then
    Result := sasmproc2
  else if FRange = rssasm3 then
    Result := sasmproc3
  else if CharInSet(FLineStr[Run], [#1..#32]) then
    Result := SpaceProc
  else if FLineStr[Run] = '(' then
    Result := ParOpenProc
  else if FLineStr[Run] = '%' then
    Result := PersentProc
  else if FLineStr[Run] = '/' then
    Result := SlashProc
  else if (Run = 1) and (FRange = rsRpl) and (FLineStr[1] = '*') then
    Result := StarProc
  else
    Result := IdentProc;
end;

procedure TSynHP48Syn.Next2(tkk: TtkTokenKind);
begin
  FTockenKind := tkk;
end;

procedure TSynHP48Syn.Next;
begin
  Next2(Next1);
  inherited;
end;

function TSynHP48Syn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 2;
end;

function TSynHP48Syn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

procedure TSynHP48Syn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynHP48Syn.ResetRange;
begin
  FRange := BaseRange;
end;

function TSynHP48Syn.GetAttrib(Index: Integer): TSynHighlighterAttributes;
begin
  Result := FAttribs[TtkTokenKind(Index)];
end;

procedure TSynHP48Syn.SetAttrib(Index: Integer; Value: TSynHighlighterAttributes);
begin
  FAttribs[TtkTokenKind(Index)].Assign(Value);
end;

procedure TSynHP48Syn.EndOfToken;
begin
  while (Run <= Length(FLineStr)) and (FLineStr[Run] > ' ') do
    Inc(Run);
end;

function TSynHP48Syn.LoadFromRegistry(RootKey: HKEY; Key: string): Boolean;
var
  r: TBetterRegistry;
begin
  r := TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKeyReadOnly(Key) then begin
      if r.ValueExists('AsmKeyWordList')
        then AsmKeywords.Text := r.ReadString('AsmKeyWordList');
      if r.ValueExists('RplKeyWordList')
        then RplKeywords.Text := r.ReadString('RplKeyWordList');
      Result := inherited LoadFromRegistry(RootKey, Key);
    end
    else
      Result := False;
  finally
    r.Free;
  end;
end;

function TSynHP48Syn.SaveToRegistry(RootKey: HKEY; Key: string): Boolean;
var
  r: TBetterRegistry;
begin
  r := TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKey(Key, true) then begin
      {$IFNDEF SYN_COMPILER_25_UP}
      Result := true;
      {$ENDIF}
      r.WriteString('AsmKeyWordList', AsmKeywords.Text);
      r.WriteString('RplKeyWordList', RplKeywords.Text);
      Result := inherited SaveToRegistry(RootKey, Key);
    end
    else
      Result := False;
  finally
    r.Free;
  end;
end;

procedure TSynHP48Syn.Assign(Source: TPersistent);
var
  i: TtkTokenKind;
begin
  if Source is TSynHP48Syn then begin
    for i := Low(FAttribs) to High(FAttribs) do begin
      FAttribs[i].Background := TSynHP48Syn(source).FAttribs[i].Background;
      FAttribs[i].Foreground := TSynHP48Syn(source).FAttribs[i].Foreground;
      FAttribs[i].Style := TSynHP48Syn(source).FAttribs[i].Style;
    end;
    AsmKeyWords.Text := TSynHP48Syn(source).AsmKeyWords.Text;
    RplKeyWords.Text := TSynHP48Syn(source).RplKeyWords.Text;
  end
  else
    inherited Assign(Source);
end;

function TSynHP48Syn.GetAttribCount: Integer;
begin
  Result := Ord(High(FAttribs)) - Ord(Low(FAttribs)) + 1;
end;

function TSynHP48Syn.GetAttribute(idx: Integer): TSynHighlighterAttributes;
begin // sorted by name
  if (idx <= Ord(High(TtkTokenKind))) then
    Result := FAttribs[TtkTokenKind(idx)]
  else
    Result := nil;
end;

function TSynHP48Syn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterHP48;
end;

class function TSynHP48Syn.GetLanguageName: string;
begin
  Result := SYNS_LangHP48;
end;

procedure TSynHP48Syn.SetHighLightChange;
var
  i: TtkTokenKind;
begin
  for i := Low(FAttribs) to High(FAttribs) do begin
    FAttribs[i].OnChange := DefHighLightChange;
    FAttribs[i].InternalSaveDefaultValues;
  end;
end;

function TSynHP48Syn.SasmProc1: TtkTokenKind;
var
  i: Integer;
  s: UnicodeString;
begin
  Result := tksAsmKey;
  if Run > Length(FLineStr) then
    Exit;
  if FLineStr[Run] = '*' then begin
    FRange := rssasm3;
    Result := tksAsmComment;
    Exit;
  end;
  if FLineStr[Run] >= ' ' then begin
    i := Run;
    while (Run <= Length(FLineStr)) and (FLineStr[Run] > ' ') do
      Inc(Run);
    s := Copy(FLineStr, i, Run - i);
    if (s = 'RPL') or (s = 'ENDCODE') then begin
      FRange := rsRpl;
      Exit;
    end;
  end;
  while (Run <= Length(FLineStr)) and (FLineStr[Run] <= ' ') and (FLineStr[Run] <> #10) do
    Inc(Run);
  if Run <= Length(FLineStr) then
    FRange := rssasm2
  else
    FRange := rssasm1;
end;

function TSynHP48Syn.SasmProc2: TtkTokenKind;
var
  i: Integer;
  s: UnicodeString;
begin
  Result := tksAsm;
  while (Run <= Length(FLineStr)) and (FLineStr[Run] <= ' ') and (FLineStr[Run] <> #10) do
    Inc(Run);
  if Run > 30 then begin
    FRange := rssasm3;
    Exit;
  end;
  i := Run;
  while (Run <= Length(FLineStr)) and (FLineStr[Run] > ' ') do
    Inc(Run);
  s := Copy(FLineStr, i, Run - i);
  if (s = 'ENDCODE') or (s = 'RPL') then begin
    FRange := rsRpl;
    Result := tksAsmKey;
  end
  else begin
    if FSAsmNoField.Find(s) = nil then begin
      while (Run <= Length(FLineStr)) and (FLineStr[Run] <= ' ') and (FLineStr[Run] <> #10) do
        Inc(Run);
      while (Run <= Length(FLineStr)) and (FLineStr[Run] > ' ') do
        Inc(Run);
      while (Run <= Length(FLineStr)) and (FLineStr[Run] <= ' ') and (FLineStr[Run] <> #10) do
        Inc(Run);
    end;
    if Run <= Length(FLineStr) then
      FRange := rssasm3
    else
      FRange := rssasm1;
  end;
end;

function TSynHP48Syn.SasmProc3: TtkTokenKind;
begin
  Result := tksAsmComment;
  while (Run <= Length(FLineStr)) and (FLineStr[Run] <> #10) do
    Inc(Run);
  if Run <= Length(FLineStr) then Inc(Run);
  FRange := rssasm1;
end;

function TSynHP48Syn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  Result := GetAttrib(Ord(FTockenKind));
end;

function TSynHP48Syn.GetTokenKind: Integer;
begin
  Result := Ord(FTockenKind);
end;

function TSynHP48Syn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  Result := nil;
end;

// reimplement functions to handle the non-standard use of 1-based Run
// (instead of the standard 0-based Run)

procedure TSynHP48Syn.DoSetLine(const Value: UnicodeString;
  LineNumber: Integer);
begin
  inherited;
  Run := 1;
  fOldRun := Run;
end;

function TSynHP48Syn.GetToken: UnicodeString;
var
  Len: Integer;
begin
  Len := (Run - 1) - FTokenPos;
  SetLength(Result, Len);
  if Len > 0 then
    WStrLCopy(@Result[1], fCasedLine + FTokenPos, Len);
end;

function TSynHP48Syn.IsLineEnd(Run: Integer): Boolean;
begin
  Result := (Run - 1 >= FLineLen) or (FLine[Run - 1] = #10) or (FLine[Run - 1] = #13);
end;

class function TSynHP48Syn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangHP48;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynHP48Syn);
{$ENDIF}
end.
