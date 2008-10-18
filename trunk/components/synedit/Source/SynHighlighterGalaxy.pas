{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterGalaxy.pas, released 2000-04-07.
The Original Code is based on the mkGalaxySyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Martijn van der Kooij.
Unicode translation by Ma�l H�rz.
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

$Id: SynHighlighterGalaxy.pas,v 1.12.2.8 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Galaxy highlighter for SynEdit)
@author(Martijn van der Kooij, converted to SynEdit by David Muir <dhm@dmsoftware.co.uk>)
@created(May 1999, converted to SynEdit June 19, 2000)
@lastmod(2000-06-23)
The SynHighlighterGalaxy unit provides SynEdit with a Galaxy highlighter.
Galaxy is a PBEM game for 10 to 500+ players, to see it wokring goto: http://members.tripod.com/~erisande/kooij.html .
The keywords in the string list KeyWords have to be in lowercase and sorted.
}

{$IFNDEF QSYNHIGHLIGHTERGALAXY}
unit SynHighlighterGalaxy;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditHighlighter,
  QSynUnicode,
{$ELSE}
  Windows,
  Graphics,
  SynEditHighlighter,
  SynUnicode,
{$ENDIF}
  SysUtils, Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkSpace, tkMessage,
    tkUnknown);

  TRangeState = (rsUnKnown, rsMessageStyle);

type
  TSynGalaxySyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    FTokenID: TtkTokenKind;
    fMessageAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyWords: TUnicodeStrings;
    procedure PointCommaProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure MessageStyleProc;
    procedure SetKeyWords(const Value: TUnicodeStrings);
  protected
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    function IsKeyword(const AKeyword: UnicodeString): Boolean; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    {$IFNDEF SYN_CLX}
    function SaveToRegistry(RootKey: HKEY; Key: string): boolean; override;
    function LoadFromRegistry(RootKey: HKEY; Key: string): boolean; override;
    {$ENDIF}
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property KeyWords: TUnicodeStrings read fKeyWords write SetKeyWords;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property MessageAttri: TSynHighlighterAttributes read fMessageAttri
      write fMessageAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

function TSynGalaxySyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
   '_', '0'..'9', 'a'..'z', 'A'..'Z', '#':
      Result := True;
    else
      Result := False;
  end;
end;

function TSynGalaxySyn.IsKeyword(const AKeyword: UnicodeString): Boolean;
var
  First, Last, I, Compare: Integer;
  Token: UnicodeString;
begin
  First := 0;
  Last := fKeywords.Count - 1;
  Result := False;
  Token := SynWideUpperCase(AKeyword);
  while First <= Last do
  begin
    I := (First + Last) shr 1;
    Compare := WideCompareStr(fKeywords[i], Token);
    if Compare = 0 then
    begin
      Result := True;
      break;
    end else
      if Compare < 0 then First := I + 1 else Last := I - 1;
  end;
end; { IsKeyWord }

constructor TSynGalaxySyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  fKeyWords := TUnicodeStringList.Create;
  TUnicodeStringList(fKeyWords).Sorted := True;
  TUnicodeStringList(fKeyWords).Duplicates := dupIgnore;
  TUnicodeStringList(fKeyWords).CommaText :=
    '#end,#galaxy,a,anonymous,autounload,b,battleprotocol,c,cap,cargo,col,' +
    'compress,d,drive,e,emp,f,fleet,fleettables,g,galaxytv,gplus,groupforecast,' +
    'h,i,j,l,m,machinereport,mat,n,namecase,no,o,options,p,planetforecast,' +
    'prodtable,produce,q,r,routesforecast,s,send,shields,shiptypeforecast,' +
    'sortgroups,t,twocol,u,underscores,v,w,war,weapons,x,y,z';
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fMessageAttri := TSynHighlighterAttributes.Create(SYNS_AttrMessage, SYNS_FriendlyAttrMessage);
  AddAttribute(fMessageAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);

  fRange := rsUnknown;
  fDefaultFilter := SYNS_FilterGalaxy;
end; { Create }

destructor TSynGalaxySyn.Destroy;
begin
  fKeyWords.Free;
  inherited Destroy;
end; { Destroy }

procedure TSynGalaxySyn.MessageStyleProc;
begin
  fTokenID := tkMessage;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        exit;
      end;
    #10:
      begin
        LFProc;
        exit;
      end;

    #13:
      begin
        CRProc;
        exit;
      end;
  end;

  if (Run = 0) and (FLine[Run] = '@') then begin
    fRange := rsUnKnown;
    inc(Run);
  end else
    while FLine[Run] <> #0 do
      inc(Run);
end;

procedure TSynGalaxySyn.PointCommaProc;                                         
begin
  fTokenID := tkComment;
  fRange := rsUnknown;
  repeat
    inc(Run);
  until fLine[Run] = #0;
end;

procedure TSynGalaxySyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynGalaxySyn.IdentProc;
begin
  while IsIdentChar(fLine[Run]) do
    Inc(Run);
  if IsKeyWord(GetToken) then
    fTokenId := tkKey
  else
    fTokenId := tkIdentifier;
end;

procedure TSynGalaxySyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynGalaxySyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynGalaxySyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynGalaxySyn.StringProc;
begin
  if (Run = 0) and (fTokenID <> tkMessage) then
  begin
    fTokenID := tkMessage;
    fRange := rsMessageStyle;
  end;
  inc(Run);
end;

procedure TSynGalaxySyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnKnown;
end;

procedure TSynGalaxySyn.Next;
begin
  fTokenPos := Run;
  if fRange = rsMessageStyle then
    MessageStyleProc
  else
    case fLine[Run] of
      ';': PointCommaProc;                                      
      #13: CRProc;
      '#','A'..'Z', 'a'..'z', '_': IdentProc;
      #10: LFProc;
      #0: NullProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      '@': StringProc;
      else UnknownProc;
    end;
  inherited;
end;

function TSynGalaxySyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynGalaxySyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynGalaxySyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynGalaxySyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynGalaxySyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkMessage: Result := fMessageAttri;
    tkSpace: Result := fSpaceAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynGalaxySyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

procedure TSynGalaxySyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynGalaxySyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynGalaxySyn.SetKeyWords(const Value: TUnicodeStrings);
var
  i: Integer;
begin
  if Value <> nil then
    begin
      Value.BeginUpdate;
      for i := 0 to Value.Count - 1 do
        Value[i] := SynWideUpperCase(Value[i]);
      Value.EndUpdate;
    end;
  fKeyWords.Assign(Value);
  DefHighLightChange(nil);
end;

function TSynGalaxySyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterGalaxy;
end;

class function TSynGalaxySyn.GetLanguageName: string;
begin
  Result := SYNS_LangGalaxy;
end;

{$IFNDEF SYN_CLX}
function TSynGalaxySyn.LoadFromRegistry(RootKey: HKEY; Key: string): boolean;
var
  r: TBetterRegistry;
begin
  r:= TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKeyReadOnly(Key) then
    begin
      if r.ValueExists('KeyWords') then KeyWords.Text:= r.ReadString('KeyWords');
      Result := inherited LoadFromRegistry(RootKey, Key);
    end
    else
      Result := False;
  finally
    r.Free;
  end;
end;

function TSynGalaxySyn.SaveToRegistry(RootKey: HKEY; Key: string): boolean;
var
  r: TBetterRegistry;
begin
  r:= TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKey(Key,true) then
    begin
      Result := true;
      r.WriteString('KeyWords', KeyWords.Text);
      Result := inherited SaveToRegistry(RootKey, Key);
    end
    else
      Result := false;
  finally
    r.Free;
  end;
end;
{$ENDIF}

class function TSynGalaxySyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangGalaxy;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynGalaxySyn);
{$ENDIF}
end.
