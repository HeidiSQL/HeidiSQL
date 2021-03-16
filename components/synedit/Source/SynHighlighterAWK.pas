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

$Id: SynHighlighterAWK.pas,v 1.10.2.6 2008/09/14 16:24:59 maelh Exp $

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

unit SynHighlighterAWK;

interface

{$I SynEdit.inc}

uses
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkInterFunc, tkKey, tkNull,
    tkNumber, tkSpace, tkString, tkSymbol, tkSysVar, tkUnknown);

  TSynAWKSyn = class(TSynCustomHighLighter)
  private
    AWKSyntaxList: TUnicodeStringList;
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FInterFuncAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FSysVarAttri: TSynHighlighterAttributes;
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
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    property InterFuncAttri: TSynHighlighterAttributes read FInterFuncAttri
      write FInterFuncAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
    property SysVarAttri: TSynHighlighterAttributes read FSysVarAttri
      write FSysVarAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri
      write FStringAttri;
  end;

implementation

uses
{$IFDEF UNICODE}
  WideStrUtils,
{$ENDIF}
  SynEditStrConst;

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
  FTokenID := tkIdentifier;
  Inc(Run);
end;

procedure TSynAWKSyn.NumberProc;
begin
  FTokenID := tkNumber;
  Inc(Run);
  while CharInSet(FLine[Run], ['0'..'9']) do
    Inc(Run);
end;

procedure TSynAWKSyn.IdentProc;
var
  i: Integer;
  idx: Integer;
  s: UnicodeString;
begin
  i := Run;
  while CharInSet(FLine[i], ['a'..'z', 'A'..'Z']) do
    Inc(i);
  SetLength(s, i - Run);
  WStrLCopy(PWideChar(s), FLine + Run, i - Run);
  Run := i;
  if AWKSyntaxList.Find(s, idx) and (AWKSyntaxList.Strings[idx] = s) then
  begin
    FTokenID := TtkTokenKind(AWKSyntaxList.Objects[idx]);
    if (FTokenID = tkUnKnown) then
    begin
      FTokenID := tkKey;
      if (FLine[i] = ' ') then
      begin
        while (FLine[i] = ' ') do
          Inc(i);
        if (FLine[i + 0] = 'f') and
          (FLine[i + 1] = 'i') and
          (FLine[i + 2] = 'l') and
          (FLine[i + 3] = 'e') and
          CharInSet(FLine[i + 4], [#0..#32, ';']) then
        begin
          Run := (i + 4);
        end;
      end;
    end;
  end
  else
    FTokenID := tkIdentifier;
end;

procedure TSynAWKSyn.Next;
begin
  FTokenPos := Run;
  case FLine[Run] of
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
    if (FLine[Run] = '"') and (FLine[Run - 1] <> '\') then
    begin
      FTokenID := tkString;
      Inc(Run);
      Exit;
    end;
  until CharInSet(FLine[Run], [#0..#31]);
  FTokenID := tkIdentifier;
end;

procedure TSynAWKSyn.CommentProc;
begin
  FTokenID := tkComment;
  while not IsLineEnd(Run) do
    Inc(Run);
end;

procedure TSynAWKSyn.FieldRefProc;

  function IsAlphaNumChar(Run: Integer): Boolean;
  begin
    case FLine[Run] of
      '0'..'9', 'a'..'z', 'A'..'Z':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(Run);
  if CharInSet(FLine[Run], ['0'..'9']) and not IsAlphaNumChar(Run + 1) then
  begin
    FTokenID := tkSymbol;
    Inc(Run);
  end
  else
    FTokenID := tkIdentifier;
end;

procedure TSynAWKSyn.SymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
end;

procedure TSynAWKSyn.PlusProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(FLine[Run], ['+', '=']) then
    Inc(Run);
end;

procedure TSynAWKSyn.MinusProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(FLine[Run], ['-', '=']) then
    Inc(Run);
end;

procedure TSynAWKSyn.OpInputProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if (FLine[Run] = '=') then
    Inc(Run);
end;

procedure TSynAWKSyn.ExclamProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(FLine[Run], ['=', '~']) then
    Inc(Run);
end;

procedure TSynAWKSyn.QuestionProc;
begin
  Inc(Run);
  if (FLine[Run] = ':') then
  begin
    FTokenID := tkSymbol;
    Inc(Run);
  end
  else
    FTokenID := tkIdentifier;
end;

procedure TSynAWKSyn.OrProc;
begin
  Inc(Run);
  if (FLine[Run] = '|') then
  begin
    FTokenID := tkSymbol;
    Inc(Run);
  end
  else
    FTokenID := tkIdentifier;
end;

procedure TSynAWKSyn.AndProc;
begin
  Inc(Run);
  if (FLine[Run] = '&') then
  begin
    FTokenID := tkSymbol;
    Inc(Run);
  end
  else
    FTokenID := tkIdentifier;
end;

constructor TSynAWKSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clBlue;
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FInterFuncAttri := TSynHighlighterAttributes.Create(SYNS_AttrInternalFunction, SYNS_FriendlyAttrInternalFunction);
  FInterFuncAttri.Foreground := $00408080;
  FInterFuncAttri.Style := [fsBold];
  AddAttribute(FInterFuncAttri);

  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Foreground := $00FF0080;
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clTeal;
  AddAttribute(FStringAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  FSymbolAttri.Style := [fsBold];
  AddAttribute(FSymbolAttri);

  FSysVarAttri := TSynHighlighterAttributes.Create(SYNS_AttrSystemValue, SYNS_FriendlyAttrSystemValue);
  FSysVarAttri.Foreground := $000080FF;
  FSysVarAttri.Style := [fsBold];
  AddAttribute(FSysVarAttri);

  SetAttributesOnChange(DefHighlightChange);

  AWKSyntaxList := TUnicodeStringList.Create;
  MakeSyntaxList;

  FDefaultFilter := SYNS_FilterAWK;
end;

destructor TSynAWKSyn.Destroy;
begin
  AWKSyntaxList.Free;

  inherited Destroy;
end;

procedure TSynAWKSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then Inc(Run);
end;

procedure TSynAWKSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynAWKSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynAWKSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

function TSynAWKSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynAWKSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynAWKSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynAWKSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkInterFunc: Result := FInterFuncAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkSysVar: Result := FSysVarAttri;
  else
    Result := nil;
  end;
end;

function TSynAWKSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

function TSynAWKSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterAWK;
end;

class function TSynAWKSyn.GetLanguageName: string;
begin
  Result := SYNS_LangAWK;
end;

class function TSynAWKSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangAWK;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynAWKSyn);
{$ENDIF}
end.
