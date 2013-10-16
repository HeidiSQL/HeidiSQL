{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterEnhCSS.pas, released 2001-10-28
Initial modifications to this CSS Highlighter were made by Ashley Brown,
ashley@ashleybrown.co.uk.

The Original Code is based on the SynHighlighterHTML.pas, released 2000-04-10 - 
this in turn was based on the hkHTMLSyn.pas file from the mwEdit component suite
by Martin Waldenburg and other developers, the Initial Author of this file is
Hideo Koiso.
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

You may retrieve the latest version of SynEdit from the SynEdit home page,
located at http://SynEdit.SourceForge.net

You may retrieve the latest version of this file from
http://www.ashleybrown.co.uk/synedit/

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides an improved CSS highlighter for SynEdit)
@author(Ashley Brown, based on HTML highlighter by Hideo Koiso and converted to SynEdit by Michael Hieke)
@created(2001-10-28)
@lastmod(2003-05-11)
The SynHighlighterEnhCSS unit provides SynEdit with an improved CSS highlighter.

http://www.ashleybrown.co.uk/
ashley@ashleybrown.co.uk
}

{$IFNDEF QSYNHIGHLIGHTERCSS}
unit SynHighlighterCSS;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
  QSynHighlighterHashEntries,
  QSynUnicode,
{$ELSE}
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynHighlighterHashEntries,
  SynUnicode,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkProperty, tkKey, tkNull, tkSpace, tkString,
    tkSymbol, tkText, tkUndefProperty, tkValue, tkColor, tkNumber, tkImportant);

  TRangeState = (rsComment, rsKey, rsParam, rsText, rsUnKnown, rsValue);

  TSynCssSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fCommentRange: TRangeState;
    fTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fPropertyAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fColorAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fTextAttri: TSynHighlighterAttributes;
    fValueAttri: TSynHighlighterAttributes;
    fUndefPropertyAttri: TSynHighlighterAttributes;
    fImportantPropertyAttri: TSynHighlighterAttributes;
    fKeywords: TSynHashEntryList;
    procedure DoAddKeyword(AKeyword: UnicodeString; AKind: integer);
    function HashKey(Str: PWideChar): Integer;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure TextProc;
    procedure CommentProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure SemiProc;
    procedure StartValProc;
    procedure NumberProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure HashProc;
    procedure SlashProc;
    procedure ExclamProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
    procedure NextProcedure;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property PropertyAttri: TSynHighlighterAttributes read fPropertyAttri
      write fPropertyAttri;
    property ColorAttri: TSynHighlighterAttributes read fColorAttri
      write fColorAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property TextAttri: TSynHighlighterAttributes read fTextAttri
      write fTextAttri;
    property ValueAttri: TSynHighlighterAttributes read fValueAttri
      write fValueAttri;
    property UndefPropertyAttri: TSynHighlighterAttributes read fUndefPropertyAttri
      write fUndefPropertyAttri;
    property ImportantPropertyAttri: TSynHighlighterAttributes read fImportantPropertyAttri
      write fImportantPropertyAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
   Properties_CSS1 : UnicodeString =
                      'background'
                     +',background-attachment'
                     +',background-color'
                     +',background-image'
                     +',background-position'
                     +',background-repeat'
                     +',border'
                     +',border-bottom'
                     +',border-bottom-color'
                     +',border-bottom-style'
                     +',border-bottom-width'
                     +',border-color'
                     +',border-left'
                     +',border-left-color'
                     +',border-left-style'
                     +',border-left-width'
                     +',border-right'
                     +',border-right-color'
                     +',border-right-style'
                     +',border-right-width'
                     +',border-style'
                     +',border-top'
                     +',border-top-color'
                     +',border-top-style'
                     +',border-top-width'
                     +',border-width'
                     +',clear'
                     +',color'
                     +',display'
                     +',float'
                     +',font'
                     +',font-family'
                     +',font-size'
                     +',font-style'
                     +',font-variant'
                     +',font-weight'
                     +',height'
                     +',letter-spacing'
                     +',line-height'
                     +',list-style'
                     +',list-style-image'
                     +',list-style-position'
                     +',list-style-type'
                     +',margin'
                     +',margin-bottom'
                     +',margin-left'
                     +',margin-right'
                     +',margin-top'
                     +',padding'
                     +',padding-bottom'
                     +',padding-left'
                     +',padding-right'
                     +',padding-top'
                     +',text-align'
                     +',text-decoration'
                     +',text-indent'
                     +',text-transform'
                     +',vertical-align'
                     +',white-space'
                     +',width'
                     +',word-spacing';
   Properties_CSS2 : UnicodeString =
                      'border-collapse'
                     +',border-spacing'
                     +',bottom'
                     +',caption-side'
                     +',clip'
                     +',content'
                     +',counter-increment'
                     +',counter-reset'
                     +',cursor'
                     +',direction'
                     +',empty-cells'
                     +',left'
                     +',max-height'
                     +',max-width'
                     +',min-height'
                     +',min-width'
                     +',orphans'
                     +',outline'
                     +',outline-color'
                     +',outline-style'
                     +',outline-width'
                     +',overflow'
                     +',page-break-after'
                     +',page-break-before'
                     +',page-break-inside'
                     +',position'
                     +',quotes'
                     +',right'
                     +',table-layout'
                     +',top'
                     +',unicode-bidi'
                     +',visibility'
                     +',widows'
                     +',z-index';
   Properties_CSS2_Aural : UnicodeString =
                      'azimuth'
                     +',cue'
                     +',cue-after'
                     +',cue-before'
                     +',elevation'
                     +',pause'
                     +',pause-after'
                     +',pause-before'
                     +',pitch'
                     +',pitch-range'
                     +',play-during'
                     +',richness'
                     +',speak'
                     +',speak-header'
                     +',speak-numeral'
                     +',speak-punctuation'
                     +',speech-rate'
                     +',stress'
                     +',voice-family'
                     +',volume';
   Properties_CSS3 : UnicodeString =
                      '@font-face'
                     +',@keyframes'
                     +',alignment-adjust'
                     +',alignment-baseline'
                     +',animation'
                     +',animation-delay'
                     +',animation-direction'
                     +',animation-duration'
                     +',animation-iteration-count'
                     +',animation-name'
                     +',animation-play-state'
                     +',animation-timing-function'
                     +',appearance'
                     +',backface-visibility'
                     +',background-clip'
                     +',background-origin'
                     +',background-size'
                     +',baseline-shift'
                     +',bookmark-label'
                     +',bookmark-level'
                     +',bookmark-target'
                     +',border-bottom-left-radius'
                     +',border-bottom-right-radius'
                     +',border-image'
                     +',border-image-outset'
                     +',border-image-repeat'
                     +',border-image-slice'
                     +',border-image-source'
                     +',border-image-width'
                     +',border-radius'
                     +',border-top-left-radius'
                     +',border-top-right-radius'
                     +',box-align'
                     +',box-decoration-break'
                     +',box-direction'
                     +',box-flex'
                     +',box-flex-group'
                     +',box-lines'
                     +',box-ordinal-group'
                     +',box-orient'
                     +',box-pack'
                     +',box-shadow'
                     +',box-sizing'
                     +',color-profile'
                     +',column-count'
                     +',column-fill'
                     +',column-gap'
                     +',column-rule'
                     +',column-rule-color'
                     +',column-rule-style'
                     +',column-rule-width'
                     +',columns'
                     +',column-span'
                     +',column-width'
                     +',crop'
                     +',dominant-baseline'
                     +',drop-initial-after-adjust'
                     +',drop-initial-after-align'
                     +',drop-initial-before-adjust'
                     +',drop-initial-before-align'
                     +',drop-initial-size'
                     +',drop-initial-value'
                     +',fit'
                     +',fit-position'
                     +',float-offset'
                     +',font-size-adjust'
                     +',font-stretch'
                     +',grid-columns'
                     +',grid-rows'
                     +',hanging-punctuation'
                     +',hyphenate-after'
                     +',hyphenate-before'
                     +',hyphenate-character'
                     +',hyphenate-lines'
                     +',hyphenate-resource'
                     +',hyphens'
                     +',icon'
                     +',image-orientation'
                     +',image-resolution'
                     +',inline-box-align'
                     +',line-stacking'
                     +',line-stacking-ruby'
                     +',line-stacking-shift'
                     +',line-stacking-strategy'
                     +',mark'
                     +',mark-after'
                     +',mark-before'
                     +',marks'
                     +',marquee-direction'
                     +',marquee-play-count'
                     +',marquee-speed'
                     +',marquee-style'
                     +',move-to'
                     +',nav-down'
                     +',nav-index'
                     +',nav-left'
                     +',nav-right'
                     +',nav-up'
                     +',opacity'
                     +',outline-offset'
                     +',overflow-style'
                     +',overflow-x'
                     +',overflow-y'
                     +',page'
                     +',page-policy'
                     +',perspective'
                     +',perspective-origin'
                     +',phonemes'
                     +',punctuation-trim'
                     +',rendering-intent'
                     +',resize'
                     +',rest'
                     +',rest-after'
                     +',rest-before'
                     +',rotation'
                     +',rotation-point'
                     +',ruby-align'
                     +',ruby-overhang'
                     +',ruby-position'
                     +',ruby-span'
                     +',size'
                     +',string-set'
                     +',target'
                     +',target-name'
                     +',target-new'
                     +',target-position'
                     +',text-align-last'
                     +',text-height'
                     +',text-justify'
                     +',text-outline'
                     +',text-overflow'
                     +',text-shadow'
                     +',text-wrap'
                     +',transform'
                     +',transform-origin'
                     +',transform-style'
                     +',transition'
                     +',transition-delay'
                     +',transition-duration'
                     +',transition-property'
                     +',transition-timing-function'
                     +',voice-balance'
                     +',voice-duration'
                     +',voice-pitch'
                     +',voice-pitch-range'
                     +',voice-rate'
                     +',voice-stress'
                     +',voice-volume'
                     +',word-break'
                     +',word-wrap';


{ TSynCssSyn }

{$Q-}
function TSynCssSyn.HashKey(Str: PWideChar): Integer;
begin
  Result := 0;
  while CharInSet(Str^, ['a'..'z', 'A'..'Z', '_', '-']) do
  begin
    if Str^ <> '-' then
    case Str^ of
      '_': Inc(Result, 27);
      '-': Inc(Result, 28);
      else Inc(Result, Ord(SynWideUpperCase(Str^)[1]) - 64);
    end;
    Inc(Str);
  end;
  while CharInSet(Str^, ['0'..'9']) do
  begin
    Inc(Result, Ord(Str^) - Ord('0'));
    Inc(Str);
  end;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynCssSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
begin
  fToIdent := MayBe;
  Entry := fKeywords[HashKey(MayBe)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > fStringLen then
      break
    else if Entry.KeywordLen = fStringLen then
      if IsCurrentToken(Entry.Keyword) then
      begin
        Result := TtkTokenKind(Entry.Kind);
        exit;
      end;
    Entry := Entry.Next;
  end;
  Result := tkUndefProperty;
end;

procedure TSynCssSyn.DoAddKeyword(AKeyword: UnicodeString; AKind: Integer);
var
  HashValue: Integer;
begin
  HashValue := HashKey(PWideChar(AKeyword));
  fKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

constructor TSynCssSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  fKeywords := TSynHashEntryList.Create;
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  AddAttribute(fCommentAttri);

  fPropertyAttri := TSynHighlighterAttributes.Create(SYNS_AttrProperty, SYNS_FriendlyAttrProperty);
  fPropertyAttri.Style := [fsBold];
  AddAttribute(fPropertyAttri);

  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := $00ff0080;
  AddAttribute(fKeyAttri);

  fUndefPropertyAttri := TSynHighlighterAttributes.Create(
    SYNS_AttrUndefinedProperty, SYNS_FriendlyAttrUndefinedProperty);
  fUndefPropertyAttri.Style := [fsBold];
  fUndefPropertyAttri.Foreground := $00ff0080;
  AddAttribute(fUndefPropertyAttri);

  fImportantPropertyAttri := TSynHighlighterAttributes.Create(
    'Important', 'Important Marker');
  fImportantPropertyAttri.Style := [fsBold];
  fImportantPropertyAttri.Foreground := clRed;
  AddAttribute(fImportantPropertyAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fColorAttri := TSynHighlighterAttributes.Create(SYNS_AttrColor, SYNS_FriendlyAttrColor);
  AddAttribute(fColorAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);

  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);

  fTextAttri := TSynHighlighterAttributes.Create(SYNS_AttrText, SYNS_FriendlyAttrText);
  AddAttribute(fTextAttri);

  fValueAttri := TSynHighlighterAttributes.Create(SYNS_AttrValue, SYNS_FriendlyAttrValue);
  fValueAttri.Foreground := $00ff8000;
  AddAttribute(fValueAttri);

  SetAttributesOnChange(DefHighlightChange);

  // TODO: differentiating tkProperty for CSS1, CSS2 & CSS3 highlighting
  EnumerateKeywords(Ord(tkProperty), Properties_CSS1, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkProperty), Properties_CSS2, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkProperty), Properties_CSS2_Aural, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkProperty), Properties_CSS3, IsIdentChar, DoAddKeyword);

  fRange := rsText;
  fDefaultFilter := SYNS_FilterCSS;
end;

destructor TSynCssSyn.Destroy;
begin
  fKeywords.Free;
  inherited Destroy;
end;

procedure TSynCssSyn.BraceCloseProc;
begin
  fRange := rsText;
  fTokenId := tkSymbol;
  Inc(Run);
end;

procedure TSynCssSyn.CommentProc;
begin
  if fLine[Run] = #0 then
    NullProc
  else
  begin
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then
      begin
        fRange := fCommentRange;
        inc(Run, 2);
        break;
      end;
      inc(Run);
    until IsLineEnd(Run)
  end;
end;

procedure TSynCssSyn.BraceOpenProc;
begin
  Inc(Run);
  fRange := rsParam;
  fTokenID := tkSymbol;
end;

procedure TSynCssSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynCssSyn.SemiProc;
begin
  fRange := rsUnknown;
  fTokenID := tkSymbol;
  Inc(Run);
end;

procedure TSynCssSyn.StartValProc;
begin
  fRange := rsValue;
  fTokenID := tkSymbol;
  Inc(Run);
end;

procedure TSynCssSyn.NumberProc;
begin
  if (FLine[Run] = '-') and not CharInSet(FLine[Run + 1], ['0'..'9']) then
    IdentProc
  else
  begin
    inc(Run);
    fTokenID := tkNumber;
    while CharInSet(FLine[Run], ['0'..'9', '.']) do
    begin
      case FLine[Run] of
        '.':
          if FLine[Run + 1] = '.' then break;
      end;
      inc(Run);
    end;
  end;
end;

procedure TSynCssSyn.IdentProc;
begin
  case fRange of
    rsKey:
      begin
        fRange := rsParam;
        fTokenID := tkKey;
        Inc(Run, fStringLen);
      end;
    rsValue:
      begin
        fTokenID := tkValue;

        while not IsLineEnd(Run) and
          not CharInSet(fLine[Run], ['}', ';', ',', ' ']) do
        begin
          Inc(Run);
        end;

        if IsLineEnd(Run) or CharInSet(fLine[Run], ['}', ';']) then
          fRange := rsParam;
      end;
    else
      fTokenID := IdentKind((fLine + Run));
      repeat
        Inc(Run);
      until (fLine[Run] <= #32) or CharInSet(fLine[Run], [':', '"', '}', ';']);
  end;
end;

procedure TSynCssSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynCssSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynCssSyn.TextProc;

  function IsStopChar: Boolean;
  begin
    case fLine[Run] of
      #0..#31, '{', '/':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  if IsStopChar then
  begin
    NextProcedure;
    exit;
  end;

  fTokenID := tkKey;
  while not IsStopChar do Inc(Run);
end;

procedure TSynCssSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynCssSyn.StringProc;
begin
  fTokenID := tkString;
  Inc(Run);  // first '"'
  while not (IsLineEnd(Run) or (fLine[Run] = '"')) do Inc(Run);
  if fLine[Run] = '"' then Inc(Run);  // last '"'
end;

procedure TSynCssSyn.HashProc;

  function IsHexChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  fTokenID := tkColor;
  Inc(Run);  // '#'
  while IsHexChar do Inc(Run);
end;

procedure TSynCssSyn.ExclamProc;
begin
  if (fLine[Run + 1] = 'i') and
    (fLine[Run + 2] = 'm') and
    (fLine[Run + 3] = 'p') and
    (fLine[Run + 4] = 'o') and
    (fLine[Run + 5] = 'r') and
    (fLine[Run + 6] = 't') and
    (fLine[Run + 7] = 'a') and
    (fLine[Run + 8] = 'n') and
    (fLine[Run + 9] = 't') then
  begin
    fTokenID := tkImportant;
    Inc(Run, 10);
  end
  else
    IdentProc;
end;

procedure TSynCssSyn.SlashProc;
begin
  inc(Run);
  if fLine[Run] = '*' then
  begin
    fTokenID := tkComment;
    fCommentRange := fRange;
    fRange := rsComment;
    inc(Run);
    if not IsLineEnd(Run) then
      CommentProc;
  end
  else
    fTokenID := tkSymbol;
end;

procedure TSynCssSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsText:
      TextProc;
    rsComment:
      CommentProc;
    else
      NextProcedure;
  end;
  inherited;
end;

procedure TSynCssSyn.NextProcedure;
begin
  case fLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    '"': StringProc;
    '#': HashProc;
    '{': BraceOpenProc;
    '}': BraceCloseProc;
    ':', ',': StartValProc;
    ';': SemiProc;
    '0'..'9', '-', '.': NumberProc;
    '/': SlashProc;
    '!': ExclamProc;
    else IdentProc;
  end;
end;

function TSynCssSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    else Result := nil;
  end;
end;

function TSynCssSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynCssSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynCssSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkProperty: Result := fPropertyAttri;
    tkKey: Result := fKeyAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkText: Result := fTextAttri;
    tkUndefProperty: Result := fUndefPropertyAttri;
    tkImportant: Result := fImportantPropertyAttri;
    tkValue: Result := fValueAttri;
    tkColor: Result := fColorAttri;
    tkNumber: Result := fNumberAttri;
    else Result := nil;
  end;
end;

function TSynCssSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynCssSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

procedure TSynCssSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynCssSyn.ResetRange;
begin
  fRange:= rsText;
end;

function TSynCssSyn.GetSampleSource: UnicodeString;
begin
  Result := '/* Syntax Highlighting */'#13#10 +
        'body { font-family: Tahoma, Verdana, Arial, Helvetica, sans-serif; font-size: 8pt }'#13#10 +
        'H1 { font-size: 18pt; color: #000099; made-up-property: 1 }';
end; { GetSampleSource }

class function TSynCssSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCSS;
end;

function TSynCssSyn.IsFilterStored: boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCSS;
end;

function TSynCssSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', '-', '0'..'9', 'A'..'Z', 'a'..'z':
      Result := True;
    else
      Result := False;
  end;
end;

class function TSynCssSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangCSS;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynCssSyn);
{$ENDIF}
end.
