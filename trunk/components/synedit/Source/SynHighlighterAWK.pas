{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterAWK.pas, released 2000-06-18.
The Original Code is based on the hkAWKSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Hideo Koiso.
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

$Id: SynHighlighterAWK.pas,v 1.10.2.5 2006/05/21 11:59:34 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a AWK Script highlighter for SynEdit)
@author(Hideo Koiso <sprhythm@fureai.or.jp>, converted to SynEdit by David Muir <dhm@dmsoftware.co.uk>)
@created(7 November 1999, converted to SynEdit April 18, 2000)
@lastmod(June 19, 2000)
The SynHighlighterAWK unit provides SynEdit with a AWK Script (.awk) highlighter.
}

{$IFNDEF QSYNHIGHLIGHTERAWK}
unit SynHighlighterAWK;
{$ENDIF}

interface

{$I SynEdit.inc}

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
  QSynUnicode,
{$ELSE}
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkInterFunc, tkKey, tkNull,
    tkNumber, tkSpace, tkString, tkSymbol, tkSysVar, tkUnknown);

  TSynAWKSyn = class(TSynCustomHighLighter)
  private
    AWKSyntaxList: TWideStringList;
    FTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fInterFuncAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fSysVarAttri: TSynHighlighterAttributes;
    procedure AndProc;
    procedure CommentProc;
    procedure CRProc;
    procedure ExclamProc;
    procedure FieldRefProc;
    procedure IdentProc;
    procedure LFProc;
    procedure MakeSyntaxList;
    procedure MinusProc;
    procedure NullProc;
    procedure OpInputProc;
    procedure OrProc;
    procedure PlusProc;
    procedure QuestionProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure NumberProc;
    procedure BraceProc;
  protected
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: WideString; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    property InterFuncAttri: TSynHighlighterAttributes read fInterFuncAttri
      write fInterFuncAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property SysVarAttri: TSynHighlighterAttributes read fSysVarAttri
      write fSysVarAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

procedure TSynAWKSyn.MakeSyntaxList;
begin
  with AWKSyntaxList do
  begin
    Sorted := True;

    { *** Preferably sort and put previously. *** }
    AddObject('ARGC', TObject(tkSysVar));
    AddObject('ARGIND', TObject(tkSysVar)); { GNU Extention }
    AddObject('ARGV', TObject(tkSysVar));
    AddObject('atan2', TObject(tkInterFunc));
    AddObject('BEGIN', TObject(tkKey));
    AddObject('break', TObject(tkKey));
    AddObject('close', TObject(tkInterFunc));
    AddObject('continue', TObject(tkKey));
    AddObject('CONVFMT', TObject(tkSysVar)); { POSIX Extention }
    AddObject('cos', TObject(tkInterFunc));
    AddObject('delete', TObject(tkInterFunc));
    AddObject('do', TObject(tkKey));
    AddObject('else', TObject(tkKey));
    AddObject('END', TObject(tkKey));
    AddObject('ENVIRON', TObject(tkSysVar));
    AddObject('ERRNO', TObject(tkSysVar)); { GNU Extention }
    AddObject('exit', TObject(tkKey));
    AddObject('exp', TObject(tkInterFunc));
    AddObject('FIELDWIDTH', TObject(tkSysVar)); { GNU Extention }
    AddObject('FILENAME', TObject(tkSysVar));
    AddObject('FNR', TObject(tkSysVar));
    AddObject('for', TObject(tkKey));
    AddObject('FS', TObject(tkSysVar));
    AddObject('function', TObject(tkKey));
    AddObject('getline', TObject(tkKey));
    AddObject('gsub', TObject(tkInterFunc));
    AddObject('if', TObject(tkKey));
    AddObject('IGNORECASE', TObject(tkSysVar));
    AddObject('index', TObject(tkInterFunc));
    AddObject('int', TObject(tkInterFunc));
    AddObject('jindex', TObject(tkInterFunc)); { jgawk }
    AddObject('jlength', TObject(tkInterFunc)); { jgawk }
    AddObject('jsubstr', TObject(tkInterFunc)); { jgawk }
    AddObject('length', TObject(tkInterFunc));
    AddObject('log', TObject(tkInterFunc));
    AddObject('match', TObject(tkInterFunc));
    AddObject('next', TObject(tkUnknown)); { & next file (GNU Extention) }
    AddObject('NF', TObject(tkSysVar));
    AddObject('NR', TObject(tkSysVar));
    AddObject('OFMT', TObject(tkSysVar));
    AddObject('OFS', TObject(tkSysVar));
    AddObject('ORS', TObject(tkSysVar));
    AddObject('print', TObject(tkKey));
    AddObject('printf', TObject(tkInterFunc));
    AddObject('rand', TObject(tkInterFunc));
    AddObject('return', TObject(tkKey));
    AddObject('RLENGTH', TObject(tkSysVar));
    AddObject('RS', TObject(tkSysVar));
    AddObject('RSTART', TObject(tkSysVar));
    AddObject('sin', TObject(tkInterFunc));
    AddObject('split', TObject(tkInterFunc));
    AddObject('sprintf', TObject(tkInterFunc));
    AddObject('sqrt', TObject(tkInterFunc));
    AddObject('srand', TObject(tkInterFunc));
    AddObject('strftime', TObject(tkInterFunc)); { GNU Extention }
    AddObject('sub', TObject(tkInterFunc));
    AddObject('SUBSEP', TObject(tkSysVar));
    AddObject('substr', TObject(tkInterFunc));
    AddObject('system', TObject(tkInterFunc));
    AddObject('systime', TObject(tkInterFunc)); { GNU Extention }
    AddObject('tolower', TObject(tkInterFunc));
    AddObject('toupper', TObject(tkInterFunc));
    AddObject('while', TObject(tkKey));
  end;
end;

procedure TSynAWKSyn.BraceProc;
begin
  fTokenID := tkIdentifier;
  Inc(Run);
end;

procedure TSynAWKSyn.NumberProc;
begin
  fTokenID := tkNumber;
  Inc(Run);
  while (fLine[Run] in [WideChar('0')..WideChar('9')]) do
    Inc(Run);
end;

procedure TSynAWKSyn.IdentProc;
var
  i: Integer;
  idx: Integer;
  s: WideString;
begin
  i := Run;
  while (fLine[i] in [WideChar('a')..WideChar('z'), WideChar('A')..WideChar('Z')]) do
    Inc(i);
  SetLength(s, i - Run);
  StrLCopyW(PWideChar(s), fLine + Run, i - Run);
  Run := i;
  if AWKSyntaxList.Find(s, idx) and (AWKSyntaxList.Strings[idx] = s) then
  begin
    fTokenID := TtkTokenKind(AWKSyntaxList.Objects[idx]);
    if (fTokenID = tkUnKnown) then
    begin
      fTokenID := tkKey;
      if (fLine[i] = ' ') then
      begin
        while (fLine[i] = ' ') do
          Inc(i);
        if (fLine[i + 0] = 'f') and
          (fLine[i + 1] = 'i') and
          (fLine[i + 2] = 'l') and
          (fLine[i + 3] = 'e') and
          (fLine[i + 4] in [WideChar(#0)..WideChar(#32), WideChar(';')]) then
        begin
          Run := (i + 4);
        end;
      end;
    end;
  end
  else
    fTokenID := tkIdentifier;
end;

procedure TSynAWKSyn.Next;
begin
  fTokenPos := Run;
  case fLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    '"', #$27: StringProc; { "..." }
    '(', ')', '[', ']': BraceProc; { (, ), [ and ] }
    '#': CommentProc; { # ... }
    '$': FieldRefProc; { $0 .. $9 }
    '+': PlusProc; { +, ++ and += }
    '-': MinusProc; { -, -- and -= }
    '!': ExclamProc; { ! and !~ }
    '?': QuestionProc; { ?: }
    '|': OrProc; { || }
    '&': AndProc; { && }
    '*', '/', '%', '^', '<', '=', '>': OpInputProc; { *=, /=, %= ... etc. }
    'a'..'z', 'A'..'Z': IdentProc;
    '0'..'9': NumberProc;
    else SymbolProc;
  end;
  inherited;
end;

procedure TSynAWKSyn.StringProc;
begin
  repeat
    Inc(Run);
    if (fLine[Run] = '"') and (fLine[Run - 1] <> '\') then
    begin
      fTokenID := tkString;
      Inc(Run);
      Exit;
    end;
  until (fLine[Run] in [WideChar(#0)..WideChar(#31)]);
  fTokenID := tkIdentifier;
end;

procedure TSynAWKSyn.CommentProc;
begin
  fTokenID := tkComment;
  while not IsLineEnd(Run) do
    Inc(Run);
end;

procedure TSynAWKSyn.FieldRefProc;

  function IsAlphaNumChar(Run: Integer): Boolean;
  begin
    case fLine[Run] of
      '0'..'9', 'a'..'z', 'A'..'Z':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(Run);
  if (fLine[Run] in [WideChar('0')..WideChar('9')]) and
    not IsAlphaNumChar(Run + 1) then
  begin
    fTokenID := tkSymbol;
    Inc(Run);
  end
  else
    fTokenID := tkIdentifier;
end;

procedure TSynAWKSyn.SymbolProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
end;

procedure TSynAWKSyn.PlusProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if (fLine[Run] in [WideChar('+'), WideChar('=')]) then
    Inc(Run);
end;

procedure TSynAWKSyn.MinusProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if (fLine[Run] in [WideChar('-'), WideChar('=')]) then
    Inc(Run);
end;

procedure TSynAWKSyn.OpInputProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if (fLine[Run] = '=') then
    Inc(Run);
end;

procedure TSynAWKSyn.ExclamProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if (fLine[Run] in [WideChar('='), WideChar('~')]) then
    Inc(Run);
end;

procedure TSynAWKSyn.QuestionProc;
begin
  Inc(Run);
  if (fLine[Run] = ':') then
  begin
    fTokenID := tkSymbol;
    Inc(Run);
  end
  else
    fTokenID := tkIdentifier;
end;

procedure TSynAWKSyn.OrProc;
begin
  Inc(Run);
  if (fLine[Run] = '|') then
  begin
    fTokenID := tkSymbol;
    Inc(Run);
  end
  else
    fTokenID := tkIdentifier;
end;

procedure TSynAWKSyn.AndProc;
begin
  Inc(Run);
  if (fLine[Run] = '&') then
  begin
    fTokenID := tkSymbol;
    Inc(Run);
  end
  else
    fTokenID := tkIdentifier;
end;

constructor TSynAWKSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := True;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Foreground := clBlue;
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fInterFuncAttri := TSynHighlighterAttributes.Create(SYNS_AttrInternalFunction, SYNS_FriendlyAttrInternalFunction);
  fInterFuncAttri.Foreground := $00408080;
  fInterFuncAttri.Style := [fsBold];
  AddAttribute(fInterFuncAttri);

  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Foreground := $00FF0080;
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Foreground := clTeal;
  AddAttribute(fStringAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  fSymbolAttri.Style := [fsBold];
  AddAttribute(fSymbolAttri);

  fSysVarAttri := TSynHighlighterAttributes.Create(SYNS_AttrSystemValue, SYNS_FriendlyAttrSystemValue);
  fSysVarAttri.Foreground := $000080FF;
  fSysVarAttri.Style := [fsBold];
  AddAttribute(fSysVarAttri);

  SetAttributesOnChange(DefHighlightChange);

  AWKSyntaxList := TWideStringList.Create;
  MakeSyntaxList;

  fDefaultFilter := SYNS_FilterAWK;
end;

destructor TSynAWKSyn.Destroy;
begin
  AWKSyntaxList.Free;

  inherited Destroy;
end;

procedure TSynAWKSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynAWKSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynAWKSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynAWKSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

function TSynAWKSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynAWKSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynAWKSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynAWKSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkInterFunc: Result := fInterFuncAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkSysVar: Result := fSysVarAttri;
  else
    Result := nil;
  end;
end;

function TSynAWKSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynAWKSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterAWK;
end;

class function TSynAWKSyn.GetLanguageName: string;
begin
  Result := SYNS_LangAWK;
end;

class function TSynAWKSyn.GetFriendlyLanguageName: WideString;
begin
  Result := SYNS_FriendlyLangAWK;
end;

{$IFNDEF SYN_CPPB_1}
initialization
  RegisterPlaceableHighlighter(TSynAWKSyn);
{$ENDIF}
end.
