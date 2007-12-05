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

$Id: SynHighlighterKix.pas,v 1.12.2.5 2005/11/27 22:22:45 maelh Exp $

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

{$IFNDEF QSYNHIGHLIGHTERKIX}
unit SynHighlighterKix;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
{$ELSE}
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
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
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..970] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fMiscellaneousAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
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
    function GetSampleSource: WideString; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: WideString; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property MiscellaneousAttri: TSynHighlighterAttributes
      read fMiscellaneousAttri write fMiscellaneousAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property VariableAttri: TSynHighlighterAttributes read fVariableAttri
      write fVariableAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  KeyWords: array[0..168] of WideString = (
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
    inc(Str);
  end;
  Result := Result mod 971;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynKixSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynKixSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if @fIdentFuncTable[i] = nil then
      fIdentFuncTable[i] := KeyWordFunc;
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

  fCaseSensitive := False;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey, SYNS_FriendlyAttrKey);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fMiscellaneousAttri := TSynHighlighterAttributes.Create(SYNS_AttrMiscellaneous, SYNS_FriendlyAttrMiscellaneous);
  AddAttribute(fMiscellaneousAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  fVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  AddAttribute(fVariableAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterKIX;
end;

procedure TSynKixSyn.AsciiCharProc;
begin
  fTokenID := tkString;
  inc(Run);
  while FLine[Run] in [WideChar('0')..WideChar('9')] do inc(Run);
end;

procedure TSynKixSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then inc(Run);
end;

procedure TSynKixSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do inc(Run);
end;

procedure TSynKixSyn.MacroProc;

  function IsMacroChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', 'A'..'Z', 'a'..'z':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  inc(Run);
  fTokenID := tkMiscellaneous;
  while IsMacroChar do inc(Run);
end;

procedure TSynKixSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynKixSyn.PrintProc;
begin
  fTokenID := tkKey;
  inc(Run);
end;

procedure TSynKixSyn.VariableProc;
begin
  fTokenId := tkVariable;
  inc(run);
  while IsIdentChar(FLine[Run]) do inc(run);
end;

procedure TSynKixSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynKixSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', 'e', 'E':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  inc(Run);
  fTokenID := tkNumber;
  while IsNumberChar do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSynKixSyn.CommentProc;
begin
  fTokenID := tkComment;
  repeat
    inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynKixSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynKixSyn.StringProc;
var
  C: WideChar;
begin
  fTokenID := tkString;
  C := fline[run];
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = C;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynKixSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynKixSyn.Next;
begin
  fTokenPos := Run;
  case fLine[Run] of
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
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
    else Result := nil;
  end;
end;

function TSynKixSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynKixSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynKixSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkMiscellaneous: Result := fMiscellaneousAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkVariable: Result := fVariableAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynKixSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

class function TSynKixSyn.GetLanguageName: string;
begin
  Result := SYNS_LangKIX;
end;

function TSynKixSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterKIX;
end;

function TSynKixSyn.GetSampleSource: WideString;
begin
  Result := '; KiXtart sample source'#13#10 +
            'break on'#13#10 +
            'color b/n'#13#10 +
            #13#10 +
            'AT(1, 30) "Hello World!"'#13#10 +
            '$USERID = @USERID'#13#10 +
            'AT(1, 30) $USERID';
end;

class function TSynKixSyn.GetFriendlyLanguageName: WideString;
begin
  Result := SYNS_FriendlyLangKIX;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynKixSyn);
{$ENDIF}
end.
