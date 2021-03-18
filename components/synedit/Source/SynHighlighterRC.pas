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

$Id: SynHighlighterRC.pas,v 1.6.2.8 2008/09/14 16:25:02 maelh Exp $

You may retrieve the latest version of SynEdit from the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

unit SynHighlighterRC;

{$I SynEdit.inc}

interface

uses
  Windows, Controls,
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
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
   FRange: TRangeState;
   FTokenID: TtkTokenKind;
   FIdentFuncTable: array[0..240] of TIdentFuncTableFunc;
   FCommentAttri: TSynHighlighterAttributes;
   FDirecAttri: TSynHighlighterAttributes;
   FIdentifierAttri: TSynHighlighterAttributes;
   FKeyAttri: TSynHighlighterAttributes;
   FNumberAttri: TSynHighlighterAttributes;
   FSpaceAttri: TSynHighlighterAttributes;
   FStringAttri: TSynHighlighterAttributes;
   FSymbolAttri: TSynHighlighterAttributes;
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
   function GetSampleSource: UnicodeString; override;
   function IsFilterStored: Boolean; override;
  public
   class function GetCapabilities: TSynHighlighterCapabilities; override;
   class function GetLanguageName: string; override;
   class function GetFriendlyLanguageName: UnicodeString; override;
  public
   constructor Create(aOwner: TComponent); override;
   destructor Destroy; override;
   function GetDefaultAttribute(index: Integer): TSynHighlighterAttributes; override;
   function GetEol: Boolean; override;
   function GetRange: Pointer; override;
   function GetTokenID: TtkTokenKind;
   function GetTokenAttribute: TSynHighlighterAttributes; override;
   function GetTokenKind: Integer; override;
   procedure Next; override;
   procedure SetRange(value: Pointer); override;
   procedure ResetRange; override;
   function UseUserSettings(SettingIndex: Integer): Boolean; override;
   procedure EnumUserSettings(Settings: TStrings); override;
  published
   property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
   property DirecAttri: TSynHighlighterAttributes read FDirecAttri write FDirecAttri;
   property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
   property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
   property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
   property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
   property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
   property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
 end;

implementation

uses
  SynEditStrConst;

const
  KeyWords: array[0..77] of UnicodeString = (
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
    Inc(Str);
  end;
  Result := Result mod 241;
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynRCSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynRCSyn.InitIdent;
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

  FCaseSensitive := True;

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  AddAttribute(FCommentAttri);

  FDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  AddAttribute(FDirecAttri);

  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  FRange := rsUnknown;
  FDefaultFilter := SYNS_FilterRC;
end;

destructor TSynRCSyn.Destroy;
begin
  inherited;
end;

procedure TSynRCSyn.QuoteProc;
begin
  FTokenID:= tkString;
  repeat
   Inc(Run);
  until IsLineEnd(Run) or (FLine[Run] = #34);
  if FLine[Run] = #34 then
    Inc(Run);
end;

procedure TSynRCSyn.SlashProc;
begin
  case FLine[Run + 1] of
   #13: CRPRoc;
   #10: LFProc;
   '/':
    begin
      FTokenID := tkComment;
      Inc(Run, 2);
      while not IsLineEnd(Run) do Inc(Run);
    end;
   '*':
    begin
      FTokenID := tkComment;
      FRange := rsComment;
      Inc(Run, 2);
      while FLine[Run] <> #0 do
       case FLine[Run] of
        '*':
         if FLine[Run + 1] = '/' then
          begin
            Inc(Run, 2);
            FRange := rsUnknown;
            Break;
          end
         else Inc(Run);
        #10, #13:
          Break;
       else
        Inc(Run);
       end;
    end;
  else
   FTokenID := tkSymbol;
   Inc(Run);
  end
end;

procedure TSynRCSyn.CommentProc;
begin
  FTokenID := tkComment;
  case FLine[Run] of
   #0: NullProc;
  #13: CRProc;
  #10: LFProc;
  else
   FTokenID := tkComment;
   repeat
    if (FLine[Run] = '*') and (FLine[Run +1] = '/') then
     begin
       Inc(Run, 2);
       FRange := rsUnknown;
       Break;
     end
    else
     Inc(Run);
   until IsLineEnd(Run);
  end;
end;

procedure TSynRCSyn.DirectiveProc;
begin
  FTokenID := tkDirective;
  repeat
   if (FLine[Run] = '/') then
    begin
      if FLine[Run +1] = '/' then
       begin
         FRange := rsUnknown;
         Exit;
       end
      else
       if FLine[Run +1] = '*' then
        begin
          FRange := rsComment;
          Exit;
        end
    end;
   Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynRCSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + Run));
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do Inc(Run);
end;

procedure TSynRCSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
   Inc(Run);
end;

procedure TSynRCSyn.LFProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
end;

procedure TSynRCSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynRCSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynRCSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', '.', 'u', 'U', 'x', 'X',
      'A'..'F', 'a'..'f', 'L', 'l', '-', '+':
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
        if FLine[Run + 1] = '.' then
          Break;
     end;
     Inc(Run);
   end;
end;

procedure TSynRCSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynRCSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynRCSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsDirective: DirectiveProc;
    rsComment: CommentProc;
    else
      case FLine[Run] of
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
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
    else Result := nil;
  end;
end;

function TSynRCSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynRCSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynRCSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynRCSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkDirective: Result := FDirecAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FSymbolAttri;
    else Result := nil;
  end;
end;

function TSynRCSyn.GetTokenKind: Integer;
begin
  Result := Ord(GetTokenID);
end;

procedure TSynRCSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynRCSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynRCSyn.EnumUserSettings(Settings: TStrings);
begin
  // ** ??
end;

function TSynRCSyn.UseUserSettings(SettingIndex: Integer): Boolean;
begin
  Result := False;
end;

class function TSynRCSyn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities;
end;

function TSynRCSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterRC;
end;

class function TSynRCSyn.GetLanguageName: string;
begin
  Result := SYNS_LangRC;
end;

function TSynRCSyn.GetSampleSource: UnicodeString;
begin
  Result := '';
end;

class function TSynRCSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangRC;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynRCSyn);
{$ENDIF}
end.
