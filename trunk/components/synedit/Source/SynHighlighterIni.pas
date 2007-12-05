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

$Id: SynHighlighterIni.pas,v 1.13.2.4 2005/11/27 22:22:44 maelh Exp $

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

{$IFNDEF QSYNHIGHLIGHTERINI}
unit SynHighlighterIni;
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
  Classes;

type
  TtkTokenKind = (tkComment, tkText, tkSection, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown);

type
  TSynIniSyn = class(TSynCustomHighlighter)
  private
    FTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fTextAttri: TSynHighlighterAttributes;
    fSectionAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
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
    property TextAttri: TSynHighlighterAttributes read fTextAttri
      write fTextAttri;
    property SectionAttri: TSynHighlighterAttributes read fSectionAttri
      write fSectionAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri
      write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

constructor TSynIniSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clGreen;
  AddAttribute(fCommentAttri);
  fTextAttri := TSynHighlighterAttributes.Create(SYNS_AttrText, SYNS_FriendlyAttrText);
  AddAttribute(fTextAttri);
  fSectionAttri := TSynHighlighterAttributes.Create(SYNS_AttrSection, SYNS_FriendlyAttrSection);
  fSectionAttri.Style := [fsBold];
  AddAttribute(fSectionAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey, SYNS_FriendlyAttrKey);
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

  fDefaultFilter := SYNS_FilterINI;
end; { Create }

procedure TSynIniSyn.SectionOpenProc;
begin
  // if it is not column 0 mark as tkText and get out of here
  if Run > 0 then
  begin
    fTokenID := tkText;
    inc(Run);
    Exit;
  end;

  // this is column 0 ok it is a Section
  fTokenID := tkSection;
  inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      ']':
        begin
          inc(Run);
          break
        end;
      #10: break;
      #13: break;
    else inc(Run);
    end;
end;

procedure TSynIniSyn.CRProc;
begin
  fTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: inc(Run, 2);
    else inc(Run);
  end;
end;

procedure TSynIniSyn.EqualProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynIniSyn.KeyProc;
begin
  fTokenID := tkKey;
  inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      '=': break;
      #10: break;
      #13: break;
      else inc(Run);
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
    fTokenID := tkText;
    inc(Run);
    while FLine[Run] <> #0 do
      if IsTextChar then
        inc(Run)
      else
        break;
  end;
end;

procedure TSynIniSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynIniSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
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
    inc(Run);
    fTokenID := tkNumber;
    while IsNumberChar do inc(Run);
    if IsAlphaChar then TextProc;
  end;
end;

// ;
procedure TSynIniSyn.SemiColonProc;
begin
  // if it is not column 0 mark as tkText and get out of here
  if Run > 0 then
  begin
    fTokenID := tkText;
    inc(Run);
    Exit;
  end;

  // this is column 0 ok it is a comment
  fTokenID := tkComment;
  inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      #10: break;
      #13: break;
      else inc(Run);
    end;
end;

procedure TSynIniSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

// ""
procedure TSynIniSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then inc(Run);
end;

// ''
procedure TSynIniSyn.StringProc1;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynIniSyn.Next;
begin
  fTokenPos := Run;
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

function TSynIniSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynIniSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynIniSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynIniSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkText: Result := fTextAttri;
    tkSection: Result := fSectionAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fTextAttri;
    else Result := nil;
  end;
end;

function TSynIniSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynIniSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterINI;
end;

class function TSynIniSyn.GetLanguageName: string;
begin
  Result := SYNS_LangINI;
end;

function TSynIniSyn.GetSampleSource: WideString;
begin
  Result := '; Syntax highlighting'#13#10+
            '[Section]'#13#10+
            'Key=value'#13#10+
            'String="Arial"'#13#10+
            'Number=123456';
end;

{$IFNDEF SYN_CPPB_1}
class function TSynIniSyn.GetFriendlyLanguageName: WideString;
begin
  Result := SYNS_FriendlyLangINI;
end;

initialization
  RegisterPlaceableHighlighter(TSynIniSyn);
{$ENDIF}
end.
