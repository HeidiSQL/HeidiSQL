{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterKix.pas, released 2000-05-05.
The Original Code is based on the jsKixSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Jeff D. Smith.
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

$Id: SynHighlighterKix.pas,v 1.12.2.6 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Kix syntax highlighter for SynEdit)
@author(Jeff D. Smith)
@created(1999, converted to SynEdit 2000-05-05)
@lastmod(2000-06-23)
The SynHighlighterKix unit provides SynEdit with a Kix script file syntax highlighter.
}

unit SynHighlighterKix;

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
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkMiscellaneous, tkNull,
    tkNumber, tkSpace, tkString, tkSymbol, tkVariable, tkUnknown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynKixSyn = class(TSynCustomHighlighter)
  private
    FTokenID: TtkTokenKind;
    FIdentFuncTable: array[0..970] of TIdentFuncTableFunc;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FMiscellaneousAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FVariableAttri: TSynHighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure AsciiCharProc;
    procedure VariableProc;
    procedure CRProc;
    procedure IdentProc;
    procedure MacroProc;
    procedure PrintProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure CommentProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
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
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property MiscellaneousAttri: TSynHighlighterAttributes
      read FMiscellaneousAttri write FMiscellaneousAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri
      write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
    property VariableAttri: TSynHighlighterAttributes read FVariableAttri
      write FVariableAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  KeyWords: array[0..168] of UnicodeString = (
    'addkey', 'addprinterconnection', 'addprogramgroup', 'addprogramitem', 
    'address', 'asc', 'at', 'backupeventlog', 'beep', 'big', 'box', 'break', 
    'call', 'case', 'cd', 'chr', 'cleareventlog', 'close', 'cls', 'color', 
    'comment', 'comparefiletimes', 'cookie1', 'copy', 'curdir', 'date', 'day', 
    'dectohex', 'del', 'delkey', 'delprinterconnection', 'delprogramgroup', 
    'delprogramitem', 'deltree', 'delvalue', 'dim', 'dir', 'display', 'do', 
    'domain', 'dos', 'else', 'endif', 'endselect', 'enumgroup', 'enumkey', 
    'enumlocalgroup', 'enumvalue', 'error', 'execute', 'exist', 'existkey', 
    'exit', 'expandenvironmentvars', 'flushkb', 'fullname', 'get', 
    'getdiskspace', 'getfileattr', 'getfilesize', 'getfiletime', 
    'getfileversion', 'gets', 'global', 'go', 'gosub', 'goto', 'homedir', 
    'homedrive', 'homeshr', 'hostname', 'if', 'ingroup', 'instr', 'inwin', 
    'ipaddress', 'kix', 'lanroot', 'lcase', 'ldomain', 'ldrive', 'len', 'lm', 
    'loadhive', 'loadkey', 'logevent', 'logoff', 'longhomedir', 'loop', 
    'lserver', 'ltrim', 'maxpwage', 'md', 'mdayno', 'messagebox', 'month', 
    'monthno', 'olecallfunc', 'olecallproc', 'olecreateobject', 'oleenumobject', 
    'olegetobject', 'olegetproperty', 'olegetsubobject', 'oleputproperty', 
    'olereleaseobject', 'open', 'password', 'play', 'primarygroup', 'priv', 
    'pwage', 'quit', 'ras', 'rd', 'readline', 'readprofilestring', 'readtype', 
    'readvalue', 'redirectoutput', 'return', 'rnd', 'rserver', 'rtrim', 'run', 
    'savekey', 'scriptdir', 'select', 'sendkeys', 'sendmessage', 'serror', 
    'set', 'setascii', 'setconsole', 'setdefaultprinter', 'setfileattr', 
    'setfocus', 'setl', 'setm', 'settime', 'setwallpaper', 'shell', 
    'showprogramgroup', 'shutdown', 'sid', 'site', 'sleep', 'small', 'srnd', 
    'startdir', 'substr', 'syslang', 'time', 'ucase', 'unloadhive', 'until', 
    'use', 'userid', 'userlang', 'val', 'wdayno', 'while', 'wksta', 'writeline', 
    'writeprofilestring', 'writevalue', 'wuserid', 'ydayno', 'year' 
  );

  KeyIndices: array[0..970] of Integer = (
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 95, -1, -1, -1, -1, -1, -1, 10, 
    -1, 29, 25, -1, -1, -1, 151, -1, -1, 22, -1, -1, -1, -1, -1, -1, -1, 64, -1, 
    -1, 76, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 97, 135, -1, -1, -1, 89, 
    -1, -1, -1, -1, -1, 48, -1, -1, -1, 164, -1, -1, -1, -1, -1, -1, -1, 52, -1, 
    -1, -1, -1, -1, 153, -1, 17, -1, -1, -1, -1, -1, -1, -1, 18, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 67, -1, -1, 101, -1, -1, -1, -1, -1, -1, 111, 159, 
    -1, -1, -1, -1, -1, -1, -1, -1, 63, -1, -1, -1, -1, -1, -1, -1, -1, 15, -1, 
    0, -1, -1, -1, -1, -1, 96, -1, -1, 133, -1, -1, 117, 129, -1, -1, -1, 9, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 66, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 36, -1, -1, 88, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 11, -1, -1, -1, 
    -1, -1, 150, -1, 72, -1, -1, -1, -1, -1, -1, 142, 94, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 80, 137, -1, -1, 118, -1, -1, 112, 
    -1, 85, -1, -1, -1, 2, -1, -1, -1, -1, -1, -1, 70, 30, -1, -1, -1, -1, -1, 
    -1, -1, -1, 157, -1, 90, -1, 24, 91, -1, 131, -1, -1, -1, -1, -1, 147, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    43, -1, -1, -1, -1, -1, -1, -1, 161, -1, -1, -1, -1, -1, -1, 165, -1, -1, 
    -1, -1, -1, -1, -1, 44, -1, -1, -1, -1, -1, -1, -1, 78, -1, -1, 127, 158, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 109, -1, 
    -1, -1, 116, 100, -1, -1, -1, -1, -1, -1, -1, 119, -1, -1, -1, -1, -1, -1, 
    -1, -1, 93, -1, -1, -1, -1, -1, -1, 41, 79, -1, 156, -1, -1, 7, -1, -1, -1, 
    -1, -1, 12, -1, -1, -1, -1, -1, -1, -1, 74, -1, -1, -1, -1, -1, -1, 81, -1, 
    31, -1, 148, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 39, -1, -1, -1, -1, -1, 
    32, -1, 121, -1, -1, 86, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 68, -1, -1, -1, 105, -1, -1, -1, -1, 
    -1, -1, 33, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 138, -1, -1, -1, 
    -1, -1, -1, -1, -1, 61, -1, -1, -1, -1, -1, -1, -1, -1, 59, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 168, 160, -1, -1, -1, -1, 
    -1, -1, -1, 26, -1, 14, -1, -1, 108, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 132, -1, -1, 50, -1, -1, 126, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 141, -1, -1, -1, -1, -1, -1, -1, 130, 84, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 71, -1, -1, -1, -1, 45, 
    107, 13, -1, -1, -1, 65, -1, -1, -1, -1, 34, -1, -1, -1, -1, 143, -1, -1, 
    -1, 128, -1, 73, 134, 27, -1, -1, -1, -1, -1, 120, -1, 57, -1, -1, -1, 51, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 82, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    123, -1, -1, -1, -1, -1, -1, -1, 46, -1, -1, -1, -1, 49, -1, -1, -1, -1, -1, 
    54, 77, -1, -1, 98, -1, -1, -1, -1, -1, 113, -1, -1, 104, -1, 1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 163, -1, 136, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 6, -1, 19, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 38, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 5, -1, -1, -1, 102, 
    -1, -1, 23, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    92, -1, -1, -1, -1, -1, -1, -1, -1, 146, -1, -1, -1, -1, 103, -1, 99, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 140, -1, -1, -1, -1, 155, 56, 115, -1, -1, 
    -1, -1, -1, -1, 162, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 125, -1, -1, -1, -1, 42, 58, -1, -1, -1, -1, -1, -1, -1, 167, -1, 
    -1, -1, 87, -1, -1, -1, 53, -1, -1, -1, -1, -1, -1, -1, 47, -1, -1, -1, -1, 
    16, -1, -1, -1, -1, -1, -1, -1, -1, -1, 35, 154, -1, 75, -1, 110, -1, 83, 
    -1, -1, -1, -1, -1, 3, -1, -1, -1, -1, -1, 144, -1, -1, 8, -1, -1, -1, 114, 
    -1, -1, -1, 152, -1, -1, -1, -1, 20, 145, 60, -1, -1, 28, -1, 55, -1, -1, 
    -1, -1, -1, 124, -1, -1, -1, -1, 106, -1, -1, -1, -1, 139, -1, -1, -1, 69, 
    -1, -1, 122, 166, -1, 62, 149, 21, 37, -1, -1, -1, -1, 40, -1, -1, -1, -1, 
    -1, -1, -1 
  );

{$Q-}
function TSynKixSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 949 + Ord(Str^) * 246;
    Inc(Str);
  end;
  Result := Result mod 971;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynKixSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynKixSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if @FIdentFuncTable[i] = nil then
      FIdentFuncTable[i] := KeyWordFunc;
end;

function TSynKixSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynKixSyn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

constructor TSynKixSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);
  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey, SYNS_FriendlyAttrKey);
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);
  FMiscellaneousAttri := TSynHighlighterAttributes.Create(SYNS_AttrMiscellaneous, SYNS_FriendlyAttrMiscellaneous);
  AddAttribute(FMiscellaneousAttri);
  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);
  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);
  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  FVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  AddAttribute(FVariableAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  FDefaultFilter := SYNS_FilterKIX;
end;

procedure TSynKixSyn.AsciiCharProc;
begin
  FTokenID := tkString;
  Inc(Run);
  while CharInSet(FLine[Run], ['0'..'9']) do Inc(Run);
end;

procedure TSynKixSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then Inc(Run);
end;

procedure TSynKixSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + Run));
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do Inc(Run);
end;

procedure TSynKixSyn.MacroProc;

  function IsMacroChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', 'A'..'Z', 'a'..'z':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(Run);
  FTokenID := tkMiscellaneous;
  while IsMacroChar do Inc(Run);
end;

procedure TSynKixSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynKixSyn.PrintProc;
begin
  FTokenID := tkKey;
  Inc(Run);
end;

procedure TSynKixSyn.VariableProc;
begin
  FTokenID := tkVariable;
  Inc(run);
  while IsIdentChar(FLine[Run]) do Inc(run);
end;

procedure TSynKixSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynKixSyn.NumberProc;

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
      '.':
        if FLine[Run + 1] = '.' then Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynKixSyn.CommentProc;
begin
  FTokenID := tkComment;
  repeat
    Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynKixSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynKixSyn.StringProc;
var
  C: WideChar;
begin
  FTokenID := tkString;
  C := FLine[run];
  repeat
    case FLine[Run] of
      #0, #10, #13:
        Break;
    end;
    Inc(Run);
  until FLine[Run] = C;
  if FLine[Run] <> #0 then Inc(Run);
end;

procedure TSynKixSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynKixSyn.Next;
begin
  FTokenPos := Run;
  case FLine[Run] of
    '#': AsciiCharProc;
    #13: CRProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    #10: LFProc;
    #0: NullProc;
    '0'..'9': NumberProc;
    ';': CommentProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    '"','''': StringProc;
    '@': MacroProc;
    '?': PrintProc;
    '$': VariableProc;
    else UnknownProc;
  end;
  inherited;
end;

function TSynKixSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
    else Result := nil;
  end;
end;

function TSynKixSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynKixSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynKixSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkMiscellaneous: Result := FMiscellaneousAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkVariable: Result := FVariableAttri;
    tkUnknown: Result := FIdentifierAttri;
    else
      Result := nil;
  end;
end;

function TSynKixSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

class function TSynKixSyn.GetLanguageName: string;
begin
  Result := SYNS_LangKIX;
end;

function TSynKixSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterKIX;
end;

function TSynKixSyn.GetSampleSource: UnicodeString;
begin
  Result :=
    '; KiXtart sample source'#13#10 +
    'break on'#13#10 +
    'color b/n'#13#10 +
    #13#10 +
    'AT(1, 30) "Hello World!"'#13#10 +
    '$USERID = @USERID'#13#10 +
    'AT(1, 30) $USERID';
end;

class function TSynKixSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangKIX;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynKixSyn);
{$ENDIF}
end.
