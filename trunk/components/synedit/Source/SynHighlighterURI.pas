{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterURI.pas, released 2003-04-10.
The initial author of this file is Maël Hörz.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynHighlighterURI.pas,v 1.16.2.8 2006/08/19 16:12:12 maelh Exp $

You may retrieve the latest version of SynEdit from the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}
{
@abstract(Provides an URI syntax highlighter for SynEdit)
@author(Maël Hörz)
@created(2003)
@lastmod(2004-03-19)
http://www.mh-net.de.vu

The SynHighlighterURI unit implements an URI syntax highlighter for SynEdit.

Recognition of URIs is based on the information provided in the document
"Uniform Resource Identifiers (URI): Generic Syntax" of "The Internet Society",
that can be found at http://www.ietf.org/rfc/rfc2396.txt.

Also interesting is http://www.freesoft.org/CIE/RFC/1738/33.htm which describes
general URL syntax and major protocols.

these protocols are recognized:
-------------------------------
http://
https://
ftp://
mailto:
news: or news://
nntp://
telnet://
gopher://
prospero://
wais://

as well as commonly used shorthands:
------------------------------------
someone@somewhere.org
www.host.org
}

{$IFNDEF QSYNHIGHLIGHTERURI}
unit SynHighlighterURI;
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
  TtkTokenKind = (tkNull, tkSpace, tkFtpLink, tkGopherLink,
    tkHttpLink, tkHttpsLink, tkMailtoLink, tkNewsLink, tkNntpLink,
    tkProsperoLink, tkTelnetLink, tkWaisLink, tkWebLink, tkUnknown, tkNullChar);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Key: Integer): TtkTokenKind of object;

  TAlreadyVisitedURIFunc = function (URI: WideString): Boolean of object;

  TSynURISyn = class(TSynCustomHighlighter)
  private
    fMayBeProtocol: PWideChar;
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..15] of TIdentFuncTableFunc;
    fIdentifierAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fURIAttri: TSynHighlighterAttributes;
    fVisitedURIAttri: TSynHighlighterAttributes;
    fAlreadyVisitedURI: TAlreadyVisitedURIFunc;

    function HashKey(Str: PWideChar): Integer;
    procedure InitIdent;

    procedure CRProc;
    procedure LFProc;
    procedure NullProc;
    procedure ProtocolProc;
    procedure SpaceProc;
    procedure UnknownProc;

    function AltFunc(Key: Integer): TtkTokenKind;
    function FuncFtp(Key: Integer): TtkTokenKind;
    function FuncGopher(Key: Integer): TtkTokenKind;
    function FuncHttp(Key: Integer): TtkTokenKind;
    function FuncHttps(Key: Integer): TtkTokenKind;
    function FuncMailto(Key: Integer): TtkTokenKind;
    function FuncNews(Key: Integer): TtkTokenKind;
    function FuncNntp(Key: Integer): TtkTokenKind;
    function FuncProspero(Key: Integer): TtkTokenKind;
    function FuncTelnet(Key: Integer): TtkTokenKind;
    function FuncWais(Key: Integer): TtkTokenKind;
    function FuncWeb(Key: Integer): TtkTokenKind;

    function IsAlphaNum(AChar: WideChar): Boolean;
    function IsMark(AChar: WideChar): Boolean;
    function IsReserved(AChar: WideChar): Boolean;
    function IsUnreserved(AChar: WideChar): Boolean;
    function IsURIChar(AChar: WideChar): Boolean;
    function IsNeverAtEnd(AChar: WideChar): Boolean;
    function IsEMailAddressChar(AChar: WideChar): Boolean;
    function IsNeverAtEMailAddressEnd(AChar: WideChar): Boolean;

    function IsValidEmailAddress: Boolean;
    function IsValidURI: Boolean;
    function IsValidWebLink: Boolean;

    procedure SetURIAttri(const Value: TSynHighlighterAttributes);
    procedure SetVisitedURIAttri(const Value: TSynHighlighterAttributes);
  protected
    function GetSampleSource: WideString; override;
    function IsCurrentToken(const Token: WideString): Boolean; override;
    function IsFilterStored: Boolean; override;
    procedure SetAlreadyVisitedURIFunc(Value: TAlreadyVisitedURIFunc);
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: WideString; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property URIAttri: TSynHighlighterAttributes read fURIAttri write SetURIAttri;
    property VisitedURIAttri: TSynHighlighterAttributes read fVisitedURIAttri
      write SetVisitedURIAttri;
  end;

const
  SYN_ATTR_URI = 6;
  SYN_ATTR_VISITEDURI = 7;

implementation

uses
{$IFDEF SYN_CLX}
  QSynUnicode,
  QSynEditStrConst, SynUnicode;
{$ELSE}
  SynUnicode,
  SynEditStrConst;
{$ENDIF}

const
  KeyWords: array[0..15] of WideString = (
    '', 'http://', '', 'https://', 'news:', 'gopher://', '', 'prospero://',
    'news://', 'www', 'nntp://', 'ftp://', 'wais://', '', 'telnet://', 'mailto:'
  );

function TSynURISyn.HashKey(Str: PWideChar): Integer;
begin
  Result := 0;
  while Str^ in [WideChar('A')..WideChar('Z'), WideChar('a')..WideChar('z')] do 
  begin
    Result := (Result * 3 + Ord(Str^) div 9) mod 16;
    inc(Str);
  end;

  if Str^ = ':' then
  begin
    Result := (Result * 3 + Ord(Str^) div 9) mod 16;
    inc(Str);
  end;

  if Str^ = '/' then
  begin
    Result := (Result * 3 + Ord(Str^) div 9) mod 16;
    inc(Str);
  end;

  if Str^ = '/' then
  begin
    Result := (Result * 3 + Ord(Str^) div 9) mod 16;
    inc(Str);
  end;

  fStringLen := Str - fMayBeProtocol;
end;

procedure TSynURISyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    fIdentFuncTable[i] := AltFunc;
  
  fIdentFuncTable[11] := FuncFtp;
  fIdentFuncTable[5] := FuncGopher;
  fIdentFuncTable[1] := FuncHttp;
  fIdentFuncTable[3] := FuncHttps;
  fIdentFuncTable[15] := FuncMailto;
  fIdentFuncTable[4] := FuncNews;
  fIdentFuncTable[8] := FuncNews;
  fIdentFuncTable[10] := FuncNntp;
  fIdentFuncTable[7] := FuncProspero;
  fIdentFuncTable[14] := FuncTelnet;
  fIdentFuncTable[12] := FuncWais;
  fIdentFuncTable[9] := FuncWeb;
end;

function TSynURISyn.IsCurrentToken(const Token: WideString): Boolean;
var
  I: Integer;
  Temp: PWideChar;
begin
  Temp := fMayBeProtocol;
  if Length(Token) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do
    begin
      if Temp^ <> Token[i] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end
  else
    Result := False;
end;

function TSynURISyn.AltFunc(Key: Integer): TtkTokenKind;
begin
  Result := tkUnknown;
end;

constructor TSynURISyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCaseSensitive := False;

  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);

  fURIAttri := TSynHighlighterAttributes.Create(SYNS_AttrURI, SYNS_FriendlyAttrURI);
  fURIAttri.Foreground := clBlue;
  fURIAttri.Style := [fsUnderline];
  AddAttribute(fURIAttri);

  fVisitedURIAttri := TSynHighlighterAttributes.Create(SYNS_AttrVisitedURI, SYNS_FriendlyAttrVisitedURI);
  fVisitedURIAttri.Foreground := clPurple;
  fVisitedURIAttri.Style := [fsUnderline];
  AddAttribute(fVisitedURIAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterURI;
end;

destructor TSynURISyn.Destroy; 
begin
  inherited;
  //the other attributes are automatically freed because of AddAttribute()
  fSpaceAttri.Free;
  fIdentifierAttri.Free;
end;

procedure TSynURISyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynURISyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynURISyn.NullProc;
begin
  if Run < fLineLen + 1 then
  begin
    inc(Run);
    fTokenID := tkNullChar;
  end
  else
    fTokenID := tkNull
end;

procedure TSynURISyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynURISyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynURISyn.Next;
begin
  fTokenPos := Run;
  case fLine[Run] of
    #13: CRProc;
    #10: LFProc;
    #0: NullProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    'A'..'Z', 'a'..'z': ProtocolProc;
    else
      UnknownProc;
  end;
  inherited;
end;

function TSynURISyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_URI: Result := fURIAttri;
    SYN_ATTR_VISITEDURI: Result := fVisitedURIAttri;
  else
    Result := nil;
  end;
end;

function TSynURISyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynURISyn.GetTokenAttribute: TSynHighlighterAttributes;
var
  Visited: Boolean;
begin
  case GetTokenID of
    tkSpace: Result := fSpaceAttri;
    tkFtpLink, tkGopherLink, tkHttpLink, tkHttpsLink, tkMailtoLink, tkNewsLink,
    tkNntpLink, tkProsperoLink, tkTelnetLink, tkWaisLink, tkWebLink:
    begin
      Visited := False;
      if Assigned(FAlreadyVisitedURI) then
        Visited := FAlreadyVisitedURI(GetToken);
      if Visited then
        Result := fVisitedURIAttri
      else
        Result := fURIAttri;
    end;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynURISyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynURISyn.GetTokenKind: Integer;
begin
  Result := Ord(fTokenId);
end;

class function TSynURISyn.GetLanguageName: string;
begin
  Result := SYNS_LangURI;
end;

function TSynURISyn.GetSampleSource: WideString;
begin
  Result := 'Universal Resource Identifier highlighting'#13#10#13#10 +
            'http://www.somewhere.org'#13#10 +
            'ftp://superhost.org/downloads/gems.zip'#13#10 +
            'www.w3c.org'#13#10 +
            'mailto:big@lebowski.edu'#13#10 +
            'douglas@adams.lod'#13#10 +
            'news:comp.lang.pascal.borland';
end;

function TSynURISyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterURI;
end;

function TSynURISyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := SynIsCharAlphaNumeric(AChar);
end;

procedure TSynURISyn.SetAlreadyVisitedURIFunc(Value: TAlreadyVisitedURIFunc);
begin
  FAlreadyVisitedURI := Value;
end;

procedure TSynURISyn.SetURIAttri(const Value: TSynHighlighterAttributes);
begin
  fURIAttri.Assign(Value);
end;

procedure TSynURISyn.SetVisitedURIAttri(const Value: TSynHighlighterAttributes);
begin
  fVisitedURIAttri.Assign(Value);
end;

procedure TSynURISyn.ProtocolProc;
var
  Key: Integer;
begin
  if IsValidEmailAddress then
    fTokenID := tkMailtoLink
  else
  begin
    fMayBeProtocol := fLine + Run;
    Key := HashKey(fMayBeProtocol);
    inc(Run, fStringLen);

    if Key <= 15 then
      fTokenID := fIdentFuncTable[Key](Key)
    else
      fTokenID := tkUnknown;
  end;
end;

function TSynURISyn.FuncFtp(Key: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Key]) and IsValidURI then
    Result := tkFtpLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.FuncGopher(Key: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Key]) and IsValidURI then
    Result := tkGopherLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.FuncHttp(Key: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Key]) and IsValidURI then
    Result := tkHttpLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.FuncHttps(Key: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Key]) and IsValidURI then
    Result := tkHttpsLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.FuncMailto(Key: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Key]) and IsValidURI then
    Result := tkMailtoLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.FuncNews(Key: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Key]) and IsValidURI then
    Result := tkNewsLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.FuncNntp(Key: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Key]) and IsValidURI then
    Result := tkNntpLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.FuncProspero(Key: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Key]) and IsValidURI then
    Result := tkProsperoLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.FuncTelnet(Key: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Key]) and IsValidURI then
    Result := tkTelnetLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.FuncWais(Key: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Key]) and IsValidURI then
    Result := tkWaisLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.FuncWeb(Key: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Key]) and IsValidWebLink then
    Result := tkWebLink
  else
    Result := tkUnknown;
end;


function TSynURISyn.IsAlphaNum(AChar: WideChar): Boolean;
begin
  Result := SynIsCharAlphaNumeric(AChar);
end;

function TSynURISyn.IsMark(AChar: WideChar): Boolean;
begin
  case AChar of
    '-', '_', '.', '!', '~', '*', '''', '(' , ')':
      Result := True;
    else
      Result := False;
  end;
end;

function TSynURISyn.IsReserved(AChar: WideChar): Boolean;
begin
  case AChar of
    ';', '/', '?', ':', '@', '&', '=', '+', '$', ',', '%', '#':
      Result := True;
    else
      Result := False;
  end;
end;

function TSynURISyn.IsUnreserved(AChar: WideChar): Boolean;
begin
  Result := IsAlphaNum(AChar) or IsMark(AChar);
end;

function TSynURISyn.IsURIChar(AChar: WideChar): Boolean;
begin
  Result := IsReserved(AChar) or IsUnreserved(AChar);
end;

function TSynURISyn.IsNeverAtEnd(AChar: WideChar): Boolean;
begin
  Result := (IsMark(AChar) and (AChar <> '''')) or
            (IsReserved(AChar) and (AChar <> '/') and (AChar <> '$'));
end;

function TSynURISyn.IsEMailAddressChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '.', '_', '-', '@':
      Result := True;
    else
      Result := IsAlphaNum(AChar);
  end;
end;

function TSynURISyn.IsNeverAtEMailAddressEnd(AChar: WideChar): Boolean;
begin
  Result := (AChar = '.') or (AChar = '@');
end;

function TSynURISyn.IsValidEmailAddress: Boolean;
var
  StartPos, AtPos, DotPos: Integer;
begin
  StartPos := Run;

  AtPos := -1;
  DotPos := -1;
  while IsEMailAddressChar(fLine[Run]) do
  begin
    if fLine[Run] = '@' then
      AtPos := Run
    else if fLine[Run] = '.' then
      // reject array of dots: "neighbour" dots are not allowed
      if (Run = StartPos) or (DotPos >= 0) and (DotPos = Run - 1) then
        break
      else
        DotPos := Run;
    Inc(Run);
  end;

  while (Run > StartPos) and (IsNeverAtEMailAddressEnd(fLine[Run - 1])) do
    dec(Run);

  while (DotPos >= Run) or (DotPos > -1) and (fLine[DotPos] <> '.') do
    Dec(DotPos);

  Result := (StartPos < AtPos) and (AtPos < Run - 1) and (DotPos > AtPos + 1);
  if not Result then Run := StartPos;
end;

function TSynURISyn.IsValidURI: Boolean;
var
  ProtocolEndPos, DotPos: Integer;

  function IsRelativePath: Boolean;
  begin
    Result := (DotPos - 1 >= 0) and
      ((fLine[DotPos - 1] = '/') and (fLine[DotPos + 2] = '/')) or
      ((fLine[DotPos - 1] = '\') and (fLine[DotPos + 2] = '\'));
  end;

begin
  ProtocolEndPos := Run;

  DotPos := -1;
  while IsURIChar(fLine[Run]) do
  begin
    if fLine[Run] = '.' then
      // reject array of dots: "neighbour" dots are not allowed
      if (DotPos >= 0) and (DotPos = Run - 1) and not IsRelativePath then
        break
      else
        DotPos := Run;
    inc(Run);
  end;

  while (Run > ProtocolEndPos) and IsNeverAtEnd(fLine[Run - 1]) do
    dec(Run);

  Result := Run > ProtocolEndPos;
end;

function TSynURISyn.IsValidWebLink: Boolean;
var
  WWWEndPos, DotPos, SecondDotPos: Integer;

  function IsRelativePath: Boolean;
  begin
    Result := (DotPos - 1 >= 0) and
      ((fLine[DotPos - 1] = '/') and (fLine[DotPos + 2] = '/')) or
      ((fLine[DotPos - 1] = '\') and (fLine[DotPos + 2] = '\'));
  end;

begin
  WWWEndPos := Run;

  DotPos := -1;
  SecondDotPos := -1;
  while IsURIChar(fLine[Run]) do
  begin
    if fLine[Run] = '.' then
      // reject array of dots: "neighbour" dots are not allowed
      if (DotPos >= 0) and (DotPos = Run - 1) and not IsRelativePath then
        break
      else
      begin
        DotPos := Run;
        if SecondDotPos = -2 then SecondDotPos := DotPos;
        if SecondDotPos = -1 then SecondDotPos := -2;
      end;
    inc(Run);
  end;

  while (Run > WWWEndPos) and IsNeverAtEnd(fLine[Run - 1]) do
    dec(Run);

  Result := (Run > WWWEndPos) and (fLine[WWWEndPos] = '.') and
            (SecondDotPos > WWWEndPos + 1) and (SecondDotPos < Run);
end;

class function TSynURISyn.GetFriendlyLanguageName: WideString;
begin
  Result := SYNS_FriendlyLangURI;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynURISyn);
{$ENDIF}
end.
