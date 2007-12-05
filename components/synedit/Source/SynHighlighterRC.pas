{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterRC.pas, released 2004-06-12.
The initial author of this file is Yiannis Mandravellos.
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

$Id: SynHighlighterRC.pas,v 1.6.2.7 2005/11/27 22:22:45 maelh Exp $

You may retrieve the latest version of SynEdit from the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

{$IFNDEF QSYNHIGHLIGHTERRC}
unit SynHighlighterRC;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics, 
  QSynEditTypes, 
  QSynEditHighlighter, 
{$ELSE}
  Windows, Controls, 
  Graphics, 
  SynEditTypes, 
  SynEditHighlighter, 
{$ENDIF}
  SysUtils,
  Classes;

type
 TtkTokenKind = (tkComment, tkDirective, tkIdentifier, tkKey, tkNull,
                 tkNumber, tkSpace, tkString, tkSymbol, tkUnknown);

 TRangeState = (rsUnknown, rsDirective, rsComment);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

 TSynRCSyn = class(TSynCustomHighlighter)
  private
   fRange: TRangeState;
   fTokenID: TtkTokenKind;
   fIdentFuncTable: array[0..240] of TIdentFuncTableFunc;
   fCommentAttri: TSynHighlighterAttributes;
   fDirecAttri: TSynHighlighterAttributes;
   fIdentifierAttri: TSynHighlighterAttributes;
   fKeyAttri: TSynHighlighterAttributes;
   fNumberAttri: TSynHighlighterAttributes;
   fSpaceAttri: TSynHighlighterAttributes;
   fStringAttri: TSynHighlighterAttributes;
   fSymbolAttri: TSynHighlighterAttributes;
   function AltFunc(Index: Integer): TtkTokenKind;
   function KeyWordFunc(Index: Integer): TtkTokenKind;
   function HashKey(Str: PWideChar): Cardinal;
   function IdentKind(MayBe: PWideChar): TtkTokenKind;
   procedure InitIdent;
   procedure CommentProc;
   procedure CRProc;
   procedure DirectiveProc;
   procedure IdentProc;
   procedure LFProc;
   procedure NullProc;
   procedure NumberProc;
   procedure QuoteProc;
   procedure SlashProc;
   procedure SpaceProc;
   procedure SymbolProc;
   procedure UnknownProc;
  protected
   function GetSampleSource: WideString; override;
   function IsFilterStored: Boolean; override;
  public
   class function GetCapabilities: TSynHighlighterCapabilities; override;
   class function GetLanguageName: string; override;
   class function GetFriendlyLanguageName: WideString; override;
  public
   constructor Create(aOwner: TComponent); override;
   destructor Destroy; override;
   function GetDefaultAttribute(index: integer): TSynHighlighterAttributes; override;
   function GetEol: boolean; override;
   function GetRange: pointer; override;
   function GetTokenID: TtkTokenKind;
   function GetTokenAttribute: TSynHighlighterAttributes; override;
   function GetTokenKind: integer; override;
   procedure Next; override;
   procedure SetRange(value: pointer); override;
   procedure ResetRange; override;
   function UseUserSettings(SettingIndex: integer): boolean; override;
   procedure EnumUserSettings(Settings: TStrings); override;
  published
   property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
   property DirecAttri: TSynHighlighterAttributes read fDirecAttri write fDirecAttri;
   property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
   property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
   property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
   property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
   property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
   property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
 end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  KeyWords: array[0..77] of WideString = (
    'ACCELERATORS', 'ALT', 'ASCII', 'AUTO3STATE', 'AUTOCHECKBOX', 
    'AUTORADIOBUTTON', 'BITMAP', 'BLOCK', 'CAPTION', 'CHARACTERISTICS', 
    'CHECKBOX', 'CHECKED', 'CLASS', 'COMBOBOX', 'COMMENTS', 'COMPANYNAME', 
    'CONTROL', 'CTEXT', 'CURSOR', 'DEFPUSHBUTTON', 'DIALOG', 'DIALOGEX', 
    'DISCARDABLE', 'EDITTEXT', 'EXSTYLE', 'FILEDESCRIPTION', 'FILEFLAGS', 
    'FILEFLAGSMASK', 'FILEOS', 'FILESUBTYPE', 'FILETYPE', 'FILEVERSION', 
    'FIXED', 'FONT', 'GRAYED', 'GROUPBOX', 'HELP', 'ICON', 'IMPURE', 'INACTIVE', 
    'INTERNALNAME', 'LANGUAGE', 'LEGALCOPYRIGHT', 'LEGALTRADEMARKS', 'LISTBOX', 
    'LOADONCALL', 'LTEXT', 'MENU', 'MENUBARBREAK', 'MENUBREAK', 'MENUEX', 
    'MENUITEM', 'MESSAGETABLE', 'MOVEABLE', 'NOINVERT', 'ORIGINALFILENAME', 
    'POPUP', 'PRELOAD', 'PRIVATEBUILD', 'PRODUCTNAME', 'PRODUCTVERSION', 'PURE', 
    'PUSHBOX', 'PUSHBUTTON', 'RADIOBUTTON', 'RCDATA', 'RTEXT', 'SCROLLBAR', 
    'SEPARATOR', 'SHIFT', 'SPECIALBUILD', 'STATE3', 'STRINGTABLE', 'STYLE', 
    'VALUE', 'VERSION', 'VERSIONINFO', 'VIRTKEY' 
  );

  KeyIndices: array[0..240] of Integer = (
    -1, -1, -1, 35, -1, 57, 54, -1, -1, -1, 74, -1, -1, -1, 64, -1, -1, -1, -1, 
    9, 68, -1, 41, -1, -1, 10, -1, -1, 13, 24, -1, -1, -1, 42, -1, -1, -1, -1, 
    -1, 61, -1, -1, 20, 67, -1, -1, -1, -1, -1, -1, -1, -1, 2, -1, -1, 23, -1, 
    -1, -1, -1, -1, 48, -1, 12, -1, -1, -1, -1, -1, -1, -1, 75, 73, 14, -1, 77, 
    -1, 4, 63, -1, -1, -1, -1, 65, 19, 27, -1, 31, 38, -1, -1, -1, -1, -1, 50, 
    -1, -1, -1, 28, -1, -1, -1, -1, -1, -1, -1, 8, 6, 18, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 49, 76, -1, 59, -1, -1, 52, 47, 29, -1, -1, -1, 
    -1, -1, -1, -1, 56, -1, -1, 44, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 1, -1, -1, 71, 17, 32, 34, -1, 45, -1, -1, -1, 70, -1, 3, 
    -1, 62, 43, 5, -1, -1, 33, 0, 51, 16, 69, -1, -1, -1, 39, -1, -1, 7, -1, 11, 
    -1, -1, -1, 21, -1, 40, -1, -1, 36, -1, -1, -1, -1, -1, -1, -1, -1, -1, 53, 
    -1, 26, -1, 66, 25, -1, -1, 72, -1, -1, 60, 15, -1, -1, -1, -1, 55, -1, -1, 
    -1, 30, -1, -1, -1, 46, -1, 58, -1, 37, 22, -1 
  );

{ TSynRCSyn }

{$Q-}
function TSynRCSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 25 + Ord(Str^) * 298;
    inc(Str);
  end;
  Result := Result mod 241;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynRCSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynRCSyn.InitIdent;
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

function TSynRCSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynRCSyn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

constructor TSynRCSyn.Create(aOwner: TComponent);
begin
  inherited;

  fCaseSensitive := True;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  AddAttribute(fCommentAttri);

  fDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  AddAttribute(fDirecAttri);

  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fRange := rsUnknown;
  fDefaultFilter := SYNS_FilterRC;
end;

destructor TSynRCSyn.Destroy;
begin
  inherited;
end;

procedure TSynRCSyn.QuoteProc;
begin
  fTokenId:= tkString;
  repeat
   inc(Run);
  until IsLineEnd(Run) or (fLine[Run] = #34);
  if fLine[Run] = #34 then
    inc(Run);
end;

procedure TSynRCSyn.SlashProc;
begin
  case fLine[Run + 1] of
   #13: CRPRoc;
   #10: LFProc;
   '/':
    begin
      fTokenId := tkComment;
      inc(Run, 2);
      while not IsLineEnd(Run) do inc(Run);
    end;
   '*':
    begin
      fTokenID := tkComment;
      fRange := rsComment;
      inc(Run, 2);
      while fLine[Run] <> #0 do
       case fLine[Run] of
        '*':
         if fLine[Run + 1] = '/' then
          begin
            inc(Run, 2);
            fRange := rsUnknown;
            break;
          end
         else inc(Run);
        #10, #13: break;
       else
        inc(Run);
       end;
    end;
  else
   fTokenId := tkSymbol;
   inc(Run);  
  end
end;

procedure TSynRCSyn.CommentProc;
begin
  fTokenId := tkComment;
  case fLine[Run] of
   #0: NullProc;
  #13: CRProc;
  #10: LFProc;
  else
   fTokenId := tkComment;
   repeat
    if (fLine[Run] = '*') and (fLine[Run +1] = '/') then
     begin
       inc(Run, 2);
       fRange := rsUnknown;
       break;
     end
    else
     inc(Run);
   until IsLineEnd(Run);
  end;
end;

procedure TSynRCSyn.DirectiveProc;
begin
  fTokenId := tkDirective;
  repeat
   if (fLine[Run] = '/') then
    begin
      if fLine[Run +1] = '/' then
       begin
         fRange := rsUnknown;
         exit;
       end
      else
       if fLine[Run +1] = '*' then
        begin
          fRange := rsComment;
          exit;
        end
    end;
   inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynRCSyn.IdentProc;
begin
  fTokenId := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do inc(Run);
end;

procedure TSynRCSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
   inc(Run);
end;

procedure TSynRCSyn.LFProc;
begin
  inc(Run);
  fTokenID := tkSpace;
end;

procedure TSynRCSyn.SpaceProc;
begin
  inc(Run);
  fTokenId := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynRCSyn.NullProc;
begin
  fTokenId := tkNull;
  inc(Run);
end;

procedure TSynRCSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', 'u', 'U', 'x', 'X',
      'A'..'F', 'a'..'f', 'L', 'l', '-', '+':
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
     case fLine[Run] of
      '.': if fLine[Run + 1] = '.' then break;
     end;
     inc(Run);
   end;
end;

procedure TSynRCSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynRCSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynRCSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsDirective: DirectiveProc;
    rsComment: CommentProc;
    else
      case fLine[Run] of
        #0: NullProc;
        #13: CRProc;
        #10: LFProc;
        '/': SlashProc;
        '"': QuoteProc;
        '#': DirectiveProc;
        'A'..'Z', 'a'..'z', '_': IdentProc;
        '0'..'9': NumberProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        '|', ',', '{', '}': SymbolProc;
        else UnknownProc;
      end;
  end;
  inherited;
end;

function TSynRCSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynRCSyn.GetEol: boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynRCSyn.GetRange: pointer;
begin
  Result := pointer(fRange);
end;

function TSynRCSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenID;
end;

function TSynRCSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkDirective: Result := fDirecAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fSymbolAttri;
    else Result := nil;
  end;
end;

function TSynRCSyn.GetTokenKind: Integer;
begin
  Result := ord(GetTokenID);
end;

procedure TSynRCSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynRCSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynRCSyn.EnumUserSettings(Settings: TStrings);
begin
  // ** ??
end;

function TSynRCSyn.UseUserSettings(SettingIndex: integer): boolean;
begin
  Result := False;
end;

class function TSynRCSyn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities;
end;

function TSynRCSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterRC;
end;

class function TSynRCSyn.GetLanguageName: string;
begin
  Result := SYNS_LangRC;
end;

function TSynRCSyn.GetSampleSource: WideString;
begin
  Result := '';
end;

class function TSynRCSyn.GetFriendlyLanguageName: WideString;
begin
  Result := SYNS_FriendlyLangRC;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynRCSyn);
{$ENDIF}
end.
