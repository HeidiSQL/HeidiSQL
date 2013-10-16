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

$Id: SynHighlighterDWS.pas,v 1.11 2011/12/28 09:24:20 Egg Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a DWScript syntax highlighter for SynEdit)
}

unit SynHighlighterDWS;

{$I SynEdit.inc}

interface

uses
  Windows,
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SysUtils,
  Classes,
  Character;

type
  TtkTokenKind = (tkAsm, tkComment, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown, tkFloat, tkHex, tkDirec, tkChar);

  TRangeState = (rsANil, rsAnsi, rsAnsiAsm, rsAsm, rsBor, rsBorAsm, rsProperty,
    rsExports, rsDirective, rsDirectiveAsm, rsHereDocSingle, rsHereDocDouble, rsUnKnown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function : TtkTokenKind of object;

type
   TAnsiStringList = class(TStringList)
     function CompareStrings(const S1, S2: string): Integer; override;
   end;

type
  TSynDWSSyn = class(TSynCustomHighlighter)
  private
    fAsmStart: Boolean;
    fRange: TRangeState;
    fCommentClose : Char;
    fIdentFuncTable: array[0..388] of TIdentFuncTableFunc;
    fKeyWords : TAnsiStringList;
    fKeyWords_PropertyScoped : TAnsiStringList;
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
    function AltFunc: TtkTokenKind;
    function KeyWordFunc: TtkTokenKind;
    function FuncAsm: TtkTokenKind;
    function FuncEnd: TtkTokenKind;
    function FuncPropertyScoped: TtkTokenKind;
    function FuncProperty: TtkTokenKind;
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
    procedure StringAposProc;
    procedure StringAposMultiProc;
    procedure StringQuoteProc;
    procedure SymbolProc;
    procedure UnknownProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
    function IsCurrentToken(const Token: UnicodeString): Boolean; override;

  public
    class function GetCapabilities: TSynHighlighterCapabilities; override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function IsIdentChar(AChar: WideChar): Boolean; override;

    procedure LoadDelphiStyle; virtual;
    // ^^^
    // This routine can be called to install a Delphi style of colors
    // and highlighting. It modifies the basic TSynDWSSyn to reproduce
    // the most recent Delphi editor highlighting.

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
  end;

implementation

uses
  SynEditStrConst;

const
   // if the language is case-insensitive keywords *must* be in lowercase
   cKeyWords: array[1..95] of UnicodeString = (
      'abstract', 'and', 'array', 'as', 'asm',
      'begin', 'break', 'case', 'cdecl', 'class', 'const', 'constructor',
      'contains', 'continue', 'deprecated', 'destructor',
      'div', 'do', 'downto', 'else', 'end', 'ensure', 'except', 'exit',
      'export', 'exports', 'external', 'final', 'finalization',
      'finally', 'for', 'forward', 'function', 'helper', 'if',
      'implementation', 'implements', 'implies', 'in', 'inherited',
      'initialization', 'inline', 'interface', 'is', 'lambda', 'lazy', 'library',
      'message', 'method', 'mod', 'new', 'nil', 'not', 'object', 'of',
      'old', 'on', 'operator', 'or', 'overload', 'override',
      'pascal', 'partial', 'private', 'procedure', 'program', 'property',
      'protected', 'public', 'published', 'raise', 'record',
      'register', 'reintroduce', 'repeat', 'require', 'resourcestring',
      'sar', 'sealed', 'set', 'shl', 'shr', 'static', 'step',
      'then', 'to', 'try', 'type', 'unit', 'until',
      'uses', 'var', 'virtual', 'while', 'xor'
  );
  cKeyWords_PropertyScoped: array [0..4] of UnicodeString = (
      'default', 'index', 'read', 'stored', 'write'
  );

function TAnsiStringList.CompareStrings(const S1, S2: string): Integer;
begin
   Result:=CompareText(S1, S2);
end;

function TSynDWSSyn.HashKey(Str: PWideChar): Cardinal;
var
   c : Word;
begin
   Result:=0;
   while IsIdentChar(Str^) do begin
      c:=Ord(Str^);
      if c in [Ord('A')..Ord('Z')] then
         c := c + (Ord('a')-Ord('A'));
      Result := Result * 692 + c * 171;
      inc(Str);
   end;
   fStringLen := Str - fToIdent;
   Result := Result mod Cardinal(Length(fIdentFuncTable));
end;

function TSynDWSSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Key: Cardinal;
begin
  fToIdent := MayBe;
  Key := HashKey(MayBe);
  if Key <= High(fIdentFuncTable) then
    Result := fIdentFuncTable[Key]
  else
    Result := tkIdentifier;
end;

procedure TSynDWSSyn.InitIdent;

   procedure SetIdentFunc(h : Integer; const func : TIdentFuncTableFunc);
   begin
      fIdentFuncTable[h]:=func;
   end;

var
  i : Integer;
begin
   for i:=Low(cKeyWords) to High(cKeyWords) do begin
      SetIdentFunc(HashKey(@cKeyWords[i][1]), KeyWordFunc);
      fKeyWords.Add(cKeyWords[i]);
   end;

   for i:=0 to High(cKeyWords_PropertyScoped) do begin
      SetIdentFunc(HashKey(@cKeyWords_PropertyScoped[i][1]), FuncPropertyScoped);
      fKeyWords_PropertyScoped.Add(cKeyWords_PropertyScoped[i]);
   end;

   for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
      if @fIdentFuncTable[i] = nil then
         fIdentFuncTable[i] := AltFunc;

   SetIdentFunc(HashKey('asm'), FuncAsm);
   SetIdentFunc(HashKey('end'), FuncEnd);
   SetIdentFunc(HashKey('property'), FuncProperty);

   fKeyWords.Sorted:=True;
end;

function TSynDWSSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier
end;

function TSynDWSSyn.KeyWordFunc: TtkTokenKind;
var
   buf : String;
begin
   SetString(buf, fToIdent, fStringLen);
   if (fKeyWords.IndexOf(buf)>=0) and (FLine[Run - 1] <> '&') then
      Result := tkKey
   else Result := tkIdentifier
end;

function TSynDWSSyn.FuncAsm: TtkTokenKind;
begin
   if IsCurrentToken('asm') then begin
      Result := tkKey;
      fRange := rsAsm;
      fAsmStart := True;
   end else Result:=KeyWordFunc;
end;

function TSynDWSSyn.FuncEnd: TtkTokenKind;
begin
   if IsCurrentToken('end') then begin
      Result := tkKey;
      fRange := rsUnknown;
   end else Result:=KeyWordFunc;
end;

// FuncPropertyScoped
//
function TSynDWSSyn.FuncPropertyScoped: TtkTokenKind;
var
   buf : String;
begin
   SetString(buf, fToIdent, fStringLen);
   if (fRange = rsProperty) and (fKeyWords_PropertyScoped.IndexOf(buf)>=0) then
      Result:=tkKey
   else Result:=KeyWordFunc;
end;

function TSynDWSSyn.FuncProperty: TtkTokenKind;
begin
   if IsCurrentToken('property') then begin
      Result := tkKey;
      fRange := rsProperty;
   end else Result:=KeyWordFunc;
end;

constructor TSynDWSSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCaseSensitive := True; // bypass automatic lowercase, we handle it here

  fAsmAttri := TSynHighlighterAttributes.Create(SYNS_AttrAssembler, SYNS_FriendlyAttrAssembler);
  fAsmAttri.Foreground:=RGB(128, 0, 0);
  AddAttribute(fAsmAttri);

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Foreground:=clGreen;
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);

  fDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  fDirecAttri.Foreground := TColor($808000);
  fDirecAttri.Style:= [fsItalic];
  AddAttribute(fDirecAttri);

  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  fNumberAttri.Foreground := clBlue;
  AddAttribute(fNumberAttri);

  fFloatAttri := TSynHighlighterAttributes.Create(SYNS_AttrFloat, SYNS_FriendlyAttrFloat);
  fFloatAttri.Foreground := clBlue;
  AddAttribute(fFloatAttri);

  fHexAttri := TSynHighlighterAttributes.Create(SYNS_AttrHexadecimal, SYNS_FriendlyAttrHexadecimal);
  fHexAttri.Foreground := clBlue;
  AddAttribute(fHexAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Foreground := clBlue;
  AddAttribute(fStringAttri);

  fCharAttri := TSynHighlighterAttributes.Create(SYNS_AttrCharacter, SYNS_FriendlyAttrCharacter);
  fCharAttri.Foreground := clBlue;
  AddAttribute(fCharAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  fSymbolAttri.Foreground := clNavy;
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);

  fKeyWords:=TAnsiStringList.Create;
  fKeyWords_PropertyScoped:=TAnsiStringList.Create;

  InitIdent;
  fRange := rsUnknown;
  fAsmStart := False;
  fDefaultFilter := SYNS_FilterDWS;
end;

// Destroy
//
destructor TSynDWSSyn.Destroy;
begin
   inherited;
   fKeyWords.Free;
   fKeyWords_PropertyScoped.Free;
end;

procedure TSynDWSSyn.AddressOpProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '@' then inc(Run);
end;

procedure TSynDWSSyn.AsciiCharProc;

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
  if fLine[run]='''' then
      StringAposMultiProc
  else begin
     while IsAsciiChar do
       Inc(Run);
  end;
end;

procedure TSynDWSSyn.BorProc;
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

procedure TSynDWSSyn.BraceOpenProc;
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

procedure TSynDWSSyn.ColonOrGreaterProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '=' then inc(Run);
end;

procedure TSynDWSSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynDWSSyn.IdentProc;
begin
  fTokenID := IdentKind(fLine + Run);
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do
    Inc(Run);
end;

procedure TSynDWSSyn.IntegerProc;

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

procedure TSynDWSSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynDWSSyn.LoadDelphiStyle;


   procedure AddKeyword( const AName : string );
   var
     I : integer;
   begin
     I := HashKey( @AName[1] );
     fIdentFuncTable[I]:= KeyWordFunc;
     fKeyWords.Add(AName);
   end;

   procedure RemoveKeyword( const AName : string );
   var
     I : integer;
   begin
     I := fKeyWords.IndexOf(AName);
     if I <> -1 then
       fKeywords.Delete( I );
   end;

const
  clID = clNavy;
  clString = clBlue;
  clComment = clGreen;
  cKeywordsToAdd: array[0..0] of UnicodeString = (
      'string');
  cKeywordsToRemove: array[0..1] of UnicodeString = (
      'break', 'exit');
var
  i : integer;
begin
  // This routine can be called to install a Delphi style of colors
  // and highlighting. It modifies the basic TSynDWSSyn to reproduce
  // the most recent Delphi editor highlighting.

  // Delphi colors...
  KeyAttri.Foreground := clID;
  StringAttri.Foreground := clString;
  CommentAttri.Foreground := clComment;

  // These are keywords highlighted in Delphi but not in TSynDWSSyn ..
  for i:=Low(cKeywordsToAdd) to High(cKeywordsToAdd) do
    AddKeyword( cKeywordsToAdd[i] );

  // These are keywords highlighted in TSynDWSSyn but not in Delphi...
  for i:=Low(cKeywordsToRemove) to High(cKeywordsToRemove) do
    RemoveKeyword( cKeywordsToRemove[i] );
end;

procedure TSynDWSSyn.LowerProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if (fLine[Run] = '=') or (fLine[Run] = '>') then
    Inc(Run);
end;

procedure TSynDWSSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynDWSSyn.NumberProc;

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

procedure TSynDWSSyn.PointProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if (fLine[Run] = '.') or (fLine[Run - 1] = ')') then
    Inc(Run);
end;

procedure TSynDWSSyn.AnsiProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = fCommentClose) then begin
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

procedure TSynDWSSyn.RoundOpenProc;
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
        fCommentClose := ')';
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

procedure TSynDWSSyn.SemicolonProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fRange in [rsProperty, rsExports] then
    fRange := rsUnknown;
end;

procedure TSynDWSSyn.SlashProc;
begin
  Inc(Run);
  case fLine[Run] of
    '/': begin
      fTokenID := tkComment;
      repeat
        Inc(Run);
      until IsLineEnd(Run);
    end;
    '*':
      begin
        Inc(Run);
        if fRange = rsAsm then
          fRange := rsAnsiAsm
        else
          fRange := rsAnsi;
        fTokenID := tkComment;
        fCommentClose := '/';
        if not IsLineEnd(Run) then
          AnsiProc;
      end;
  else
    fTokenID := tkSymbol;
  end;
end;

procedure TSynDWSSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynDWSSyn.StringAposProc;
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

procedure TSynDWSSyn.StringAposMultiProc;
begin
  fTokenID := tkString;
  Inc(Run);
  fRange := rsHereDocSingle;
  while not IsLineEnd(Run) do
  begin
    if fLine[Run] = '''' then begin
      Inc(Run);
      if fLine[Run] <> '''' then
        fRange := rsUnknown;
        break;
    end;
    Inc(Run);
  end;
end;

procedure TSynDWSSyn.StringQuoteProc;
begin
  fTokenID := tkString;
  Inc(Run);
  fRange := rsHereDocDouble;
  while not IsLineEnd(Run) do
  begin
    if fLine[Run] = '"' then begin
      Inc(Run);
      if fLine[Run] <> '"' then
        fRange := rsUnknown;
        break;
    end;
    Inc(Run);
  end;
end;

procedure TSynDWSSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynDWSSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynDWSSyn.Next;
begin
   fAsmStart := False;
   fTokenPos := Run;
   case fRange of
      rsAnsi, rsAnsiAsm:
         AnsiProc;
      rsBor, rsBorAsm, rsDirective, rsDirectiveAsm:
         BorProc;
      rsHereDocSingle:
         StringAposMultiProc;
      rsHereDocDouble:
         StringQuoteProc;
   else
      case fLine[Run] of
         #0: NullProc;
         #10: LFProc;
         #13: CRProc;
         #1..#9, #11, #12, #14..#32: SpaceProc;
         '#': AsciiCharProc;
         '$': IntegerProc;
         #39: StringAposProc;
         '"': StringQuoteProc;
         '0'..'9': NumberProc;
         'A'..'Z', 'a'..'z', '_': IdentProc;
         '{': BraceOpenProc;
         '}', '!', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~': begin
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
         #$0080..#$FFFF :
            if {$IFDEF SYN_COMPILER_18_UP}Char(fLine[Run]).IsLetterOrDigit{$ELSE}TCharacter.IsLetterOrDigit(fLine[Run]){$ENDIF} then
               IdentProc
            else UnknownProc;
      else
         UnknownProc;
      end;
   end;
   inherited;
end;

function TSynDWSSyn.GetDefaultAttribute(Index: Integer):
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

function TSynDWSSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynDWSSyn.GetTokenID: TtkTokenKind;
begin
  if not fAsmStart and (fRange = rsAsm)
    and not (fTokenId in [tkNull, tkComment, tkDirec, tkSpace])
  then
    Result := tkAsm
  else
    Result := fTokenId;
end;

function TSynDWSSyn.GetTokenAttribute: TSynHighlighterAttributes;
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

function TSynDWSSyn.GetTokenKind: Integer;
begin
  Result := Ord(GetTokenID);
end;

function TSynDWSSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

procedure TSynDWSSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynDWSSyn.ResetRange;
begin
  fRange:= rsUnknown;
end;

function TSynDWSSyn.GetSampleSource: UnicodeString;
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


class function TSynDWSSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPascal;
end;

class function TSynDWSSyn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

function TSynDWSSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterPascal;
end;

// IsCurrentToken
//
function TSynDWSSyn.IsCurrentToken(const Token: UnicodeString): Boolean;
var
   i : Integer;
   temp : PWideChar;
begin
   temp := fToIdent;
   if Length(Token) = fStringLen then begin
      Result := True;
      for i := 1 to fStringLen do begin
         if     (temp^ <> Token[i])
            and (   (temp^>'z')
                 or (UpCase(temp^)<>UpCase(Token[i])))  then begin
            Result := False;
            break;
         end;
         inc(temp);
      end;
   end else Result := False;
end;

// IsIdentChar
//
function TSynDWSSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
   if Ord(AChar)<=$7F then
      Result := AnsiChar(AChar) in ['_', '0'..'9', 'A'..'Z', 'a'..'z']
   else
      Result := {$IFDEF SYN_COMPILER_18_UP}AChar.IsLetterOrDigit{$ELSE}TCharacter.IsLetterOrDigit(AChar){$ENDIF};
end;

class function TSynDWSSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangPascal;
end;

initialization

{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynDWSSyn);
{$ENDIF}

end.

