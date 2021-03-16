{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterIni.pas, released 2000-04-21.
The Original Code is based on the izIniSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Igor P. Zenkov.
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

$Id: SynHighlighterIni.pas,v 1.13.2.5 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides an Ini-files highlighter for SynEdit)
@author(Igor P. Zenkov, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(1999-11-02, converted to SynEdit 2000-04-21)
@lastmod(2000-04-21)
The SynHighlighterIni unit provides SynEdit with an Ini-files highlighter.
Thanks to Primoz Gabrijelcic, Martin Waldenburg and Michael Hieke.
}

unit SynHighlighterIni;

{$I SynEdit.inc}

interface

uses
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
  Classes;

type
  TtkTokenKind = (tkComment, tkText, tkSection, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown);

type
  TSynIniSyn = class(TSynCustomHighlighter)
  private
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynHighlighterAttributes;
    FTextAttri: TSynHighlighterAttributes;
    FSectionAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    procedure SectionOpenProc;
    procedure KeyProc;
    procedure CRProc;
    procedure EqualProc;
    procedure TextProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SemiColonProc;
    procedure SpaceProc;
    procedure StringProc;  // ""
    procedure StringProc1; // ''
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
    property TextAttri: TSynHighlighterAttributes read FTextAttri
      write FTextAttri;
    property SectionAttri: TSynHighlighterAttributes read FSectionAttri
      write FSectionAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri
      write FKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri
      write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
  end;

implementation

uses
  SynEditStrConst;

constructor TSynIniSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  FCommentAttri.Foreground := clGreen;
  AddAttribute(FCommentAttri);
  FTextAttri := TSynHighlighterAttributes.Create(SYNS_AttrText, SYNS_FriendlyAttrText);
  AddAttribute(FTextAttri);
  FSectionAttri := TSynHighlighterAttributes.Create(SYNS_AttrSection, SYNS_FriendlyAttrSection);
  FSectionAttri.Style := [fsBold];
  AddAttribute(FSectionAttri);
  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey, SYNS_FriendlyAttrKey);
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

  FDefaultFilter := SYNS_FilterINI;
end; { Create }

procedure TSynIniSyn.SectionOpenProc;
begin
  // if it is not column 0 mark as tkText and get out of here
  if Run > 0 then
  begin
    FTokenID := tkText;
    Inc(Run);
    Exit;
  end;

  // this is column 0 ok it is a Section
  FTokenID := tkSection;
  Inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      ']':
        begin
          Inc(Run);
          Break
        end;
      #10, #13:
        Break;
    else Inc(Run);
    end;
end;

procedure TSynIniSyn.CRProc;
begin
  FTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: Inc(Run, 2);
    else Inc(Run);
  end;
end;

procedure TSynIniSyn.EqualProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynIniSyn.KeyProc;
begin
  FTokenID := tkKey;
  Inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      '=':
        Break;
      #10, #13:
        Break;
      else Inc(Run);
    end;
end;

procedure TSynIniSyn.TextProc;

  function IsTextChar: Boolean;
  begin
    case fLine[Run] of
      'a'..'z', 'A'..'Z', '0'..'9':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  if Run = 0 then
    KeyProc
  else
  begin
    FTokenID := tkText;
    Inc(Run);
    while FLine[Run] <> #0 do
      if IsTextChar then
        Inc(Run)
      else
        Break;
  end;
end;

procedure TSynIniSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynIniSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynIniSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', 'e', 'E':
        Result := True;
      else
        Result := False;
    end;
  end;

  function IsAlphaChar: Boolean;
  begin
    case fLine[Run] of
      'a'..'z', 'A'..'Z':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  if Run = 0 then
    KeyProc
  else
  begin
    Inc(Run);
    FTokenID := tkNumber;
    while IsNumberChar do Inc(Run);
    if IsAlphaChar then TextProc;
  end;
end;

// ;
procedure TSynIniSyn.SemiColonProc;
begin
  // if it is not column 0 mark as tkText and get out of here
  if Run > 0 then
  begin
    FTokenID := tkText;
    Inc(Run);
    Exit;
  end;

  // this is column 0 ok it is a comment
  FTokenID := tkComment;
  Inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      #10, #13:
        Break;
      else
        Inc(Run);
    end;
end;

procedure TSynIniSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

// ""
procedure TSynIniSyn.StringProc;
begin
  FTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then Inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13:
        Break;
    end;
    Inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then Inc(Run);
end;

// ''
procedure TSynIniSyn.StringProc1;
begin
  FTokenID := tkString;
  if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then Inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13:
        Break;
    end;
    Inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then Inc(Run);
end;

procedure TSynIniSyn.Next;
begin
  FTokenPos := Run;
  case fLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    #34: StringProc;
    #39: StringProc1;
    '0'..'9': NumberProc;
    #59: SemiColonProc;
    #61: EqualProc;
    #91: SectionOpenProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    else TextProc;
  end;
  inherited;
end;

function TSynIniSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynIniSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynIniSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynIniSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkText: Result := FTextAttri;
    tkSection: Result := FSectionAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FTextAttri;
    else Result := nil;
  end;
end;

function TSynIniSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

function TSynIniSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterINI;
end;

class function TSynIniSyn.GetLanguageName: string;
begin
  Result := SYNS_LangINI;
end;

function TSynIniSyn.GetSampleSource: UnicodeString;
begin
  Result :=
    '; Syntax highlighting'#13#10+
    '[Section]'#13#10+
    'Key=value'#13#10+
    'String="Arial"'#13#10+
    'Number=123456';
end;

class function TSynIniSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangINI;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynIniSyn);
{$ENDIF}
end.
