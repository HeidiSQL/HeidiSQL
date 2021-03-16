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

unit SynHighlighterCSS;

{$I SynEdit.inc}

interface

uses
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynHighlighterHashEntries,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkAtRule, tkProperty, tkSelector, tkSelectorAttrib,
    tkNull, tkSpace, tkString, tkSymbol, tkText, tkUndefProperty, tkValue,
    tkColor, tkNumber, tkImportant);

  TRangeState = (rsComment, rsSelector, rsDeclaration, rsUnknown, rsProperty,
    rsValue, rsAttrib, rsParameter);

  TSynCssSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FCommentRange: TRangeState;
    FParameterRange: TRangeState;
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynHighlighterAttributes;
    FPropertyAttri: TSynHighlighterAttributes;
    FAttributeAttri: TSynHighlighterAttributes;
    FSelectorAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FColorAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FTextAttri: TSynHighlighterAttributes;
    FValueAttri: TSynHighlighterAttributes;
    FUndefPropertyAttri: TSynHighlighterAttributes;
    FImportantPropertyAttri: TSynHighlighterAttributes;
    FAtRuleAttri: TSynHighlighterAttributes;
    FKeywords: TSynHashEntryList;
    procedure DoAddKeyword(AKeyword: UnicodeString; AKind: Integer);
    function HashKey(Str: PWideChar): Integer;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure AtRuleProc;
    procedure SelectorProc;
    procedure AttributeProc;
    procedure CommentProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure ParenOpenProc;
    procedure ParenCloseProc;
    procedure BracketOpenProc;
    procedure BracketCloseProc;
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
    procedure GreaterProc;
    procedure PlusProc;
    procedure TildeProc;
    procedure PipeProc;
    procedure CircumflexProc;
    procedure AttrContainProc;
    procedure EqualProc;
    procedure ExclamProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
    procedure NextDeclaration;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
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
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property PropertyAttri: TSynHighlighterAttributes read FPropertyAttri
      write FPropertyAttri;
    property ColorAttri: TSynHighlighterAttributes read FColorAttri
      write FColorAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property AtRuleAttri: TSynHighlighterAttributes read FAtRuleAttri
      write FAtRuleAttri;
    property SelectorAttri: TSynHighlighterAttributes read FSelectorAttri
      write FSelectorAttri;
    property AttributeAttri: TSynHighlighterAttributes read FAttributeAttri
      write FAttributeAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri
      write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
    property TextAttri: TSynHighlighterAttributes read FTextAttri
      write FTextAttri;
    property ValueAttri: TSynHighlighterAttributes read FValueAttri
      write FValueAttri;
    property UndefPropertyAttri: TSynHighlighterAttributes read FUndefPropertyAttri
      write FUndefPropertyAttri;
    property ImportantPropertyAttri: TSynHighlighterAttributes read FImportantPropertyAttri
      write FImportantPropertyAttri;
  end;

implementation

uses
  SynEditStrConst;

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
                     +',@font-feature-values'
                     +',@keyframes'
                     +',align-content'
                     +',align-items'
                     +',align-self'
                     +',alignment-adjust'
                     +',alignment-baseline'
                     +',animation'
                     +',animation-delay'
                     +',animation-direction'
                     +',animation-duration'
                     +',animation-fill-mode'
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
                     +',break-after'
                     +',break-before'
                     +',break-inside'
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
                     +',filter'
                     +',fit'
                     +',fit-position'
                     +',float-offset'
                     +',flex'
                     +',flex-basis'
                     +',flex-direction'
                     +',flex-flow'
                     +',flex-grow'
                     +',flex-shrink'
                     +',flex-wrap'
                     +',font-size-adjust'
                     +',font-feature-setting'
                     +',font-kerning'
                     +',font-language-override'
                     +',font-synthesis'
                     +',font-variant-alternates'
                     +',font-variant-caps'
                     +',font-variant-east-asian'
                     +',font-variant-ligatures'
                     +',font-variant-numeric'
                     +',font-variant-position'
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
                     +',image-rendering'
                     +',image-resolution'
                     +',ime-mode'
                     +',justify-content'
                     +',inline-box-align'
                     +',line-break'
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
                     +',mask'
                     +',mask-type'
                     +',move-to'
                     +',nav-down'
                     +',nav-index'
                     +',nav-left'
                     +',nav-right'
                     +',nav-up'
                     +',object-fit'
                     +',object-position'
                     +',opacity'
                     +',order'
                     +',outline-offset'
                     +',overflow-style'
                     +',overflow-x'
                     +',overflow-y'
                     +',overflow-wrap'
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
                     +',tab-size'
                     +',target'
                     +',target-name'
                     +',target-new'
                     +',target-position'
                     +',text-align-last'
                     +',text-combine-horizontal'
                     +',text-decoration-color'
                     +',text-decoration-line'
                     +',text-decoration-style'
                     +',text-height'
                     +',text-justify'
                     +',text-orientation'
                     +',text-outline'
                     +',text-overflow'
                     +',text-shadow'
                     +',text-underline-position'
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
                     +',word-wrap'
                     +',writing-mode';

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
  FStringLen := Str - FToIdent;
end;
{$Q+}

function TSynCssSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
begin
  FToIdent := MayBe;
  Entry := FKeywords[HashKey(MayBe)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > FStringLen then
      Break
    else if Entry.KeywordLen = FStringLen then
      if IsCurrentToken(Entry.Keyword) then
      begin
        Result := TtkTokenKind(Entry.Kind);
        Exit;
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
  FKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

constructor TSynCssSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FKeywords := TSynHashEntryList.Create;
  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  AddAttribute(FCommentAttri);

  FPropertyAttri := TSynHighlighterAttributes.Create(SYNS_AttrProperty, SYNS_FriendlyAttrProperty);
  FPropertyAttri.Style := [fsBold];
  AddAttribute(FPropertyAttri);

  FSelectorAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FSelectorAttri.Style := [fsBold];
  FSelectorAttri.Foreground := $00ff0080;
  AddAttribute(FSelectorAttri);

  FAttributeAttri := TSynHighlighterAttributes.Create(SYNS_AttrAttribute, SYNS_FriendlyAttrAttribute);
  FAttributeAttri.Style := [];
  FAttributeAttri.Foreground := $00ff0080;
  AddAttribute(FAttributeAttri);

  FAtRuleAttri := TSynHighlighterAttributes.Create(SYNS_AttrAtRules, SYNS_FriendlyAttrAttribute);
  FAtRuleAttri.Style := [];
  FAtRuleAttri.Foreground := $00808000;
  AddAttribute(FAtRuleAttri);

  FUndefPropertyAttri := TSynHighlighterAttributes.Create(
    SYNS_AttrUndefinedProperty, SYNS_FriendlyAttrUndefinedProperty);
  FUndefPropertyAttri.Style := [fsBold];
  FUndefPropertyAttri.Foreground := $00ff0080;
  AddAttribute(FUndefPropertyAttri);

  FImportantPropertyAttri := TSynHighlighterAttributes.Create(
    'Important', 'Important Marker');
  FImportantPropertyAttri.Style := [fsBold];
  FImportantPropertyAttri.Foreground := clRed;
  AddAttribute(FImportantPropertyAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FColorAttri := TSynHighlighterAttributes.Create(SYNS_AttrColor, SYNS_FriendlyAttrColor);
  AddAttribute(FColorAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);

  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  FTextAttri := TSynHighlighterAttributes.Create(SYNS_AttrText, SYNS_FriendlyAttrText);
  AddAttribute(FTextAttri);

  FValueAttri := TSynHighlighterAttributes.Create(SYNS_AttrValue, SYNS_FriendlyAttrValue);
  FValueAttri.Foreground := $00ff8000;
  AddAttribute(FValueAttri);

  SetAttributesOnChange(DefHighlightChange);

  // TODO: differentiating tkProperty for CSS1, CSS2 & CSS3 highlighting
  EnumerateKeywords(Ord(tkProperty), Properties_CSS1, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkProperty), Properties_CSS2, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkProperty), Properties_CSS2_Aural, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkProperty), Properties_CSS3, IsIdentChar, DoAddKeyword);

  FRange := rsSelector;
  FDefaultFilter := SYNS_FilterCSS;
end;

destructor TSynCssSyn.Destroy;
begin
  FKeywords.Free;
  inherited Destroy;
end;

procedure TSynCssSyn.AttributeProc;

  function IsStopChar: Boolean;
  begin
    case FLine[Run] of
      #0..#31, ']', '~', '^', '$', '*', '|', '=':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  if IsStopChar then
  begin
    case FLine[Run] of
      #0..#31, '{', '/': NextDeclaration;
      ']': BracketCloseProc;
      '~': TildeProc;
      '|': PipeProc;
      '=': EqualProc;
      '^': CircumflexProc;
      '*': AttrContainProc;
    end;
    Exit;
  end;

  FTokenID := tkSelectorAttrib;
  while not IsStopChar do
    Inc(Run);
end;

procedure TSynCssSyn.BraceCloseProc;
begin
  FRange := rsSelector;
  FTokenID := tkSymbol;
  Inc(Run);
end;

procedure TSynCssSyn.BraceOpenProc;
begin
  Inc(Run);
  FRange := rsDeclaration;
  FTokenID := tkSymbol;
end;

procedure TSynCssSyn.BracketCloseProc;
begin
  FTokenID := tkSymbol;
  FRange := rsSelector;
  Inc(Run);
end;

procedure TSynCssSyn.BracketOpenProc;
begin
  Inc(Run);
  FRange := rsAttrib;
  FTokenID := tkSymbol;
end;

procedure TSynCssSyn.CircumflexProc;
begin
  Inc(Run);
  if FLine[Run] = '=' then
  begin
    Inc(Run);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynCssSyn.AttrContainProc;
begin
  Inc(Run);
  if FLine[Run] = '=' then
  begin
    Inc(Run);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynCssSyn.CommentProc;
begin
  if FLine[Run] = #0 then
    NullProc
  else
  begin
    FTokenID := tkComment;
    repeat
      if (FLine[Run] = '*') and (FLine[Run + 1] = '/') then
      begin
        FRange := FCommentRange;
        Inc(Run, 2);
        Break;
      end;
      Inc(Run);
    until IsLineEnd(Run)
  end;
end;

procedure TSynCssSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then Inc(Run);
end;

procedure TSynCssSyn.SemiProc;
begin
  FRange := rsUnknown;
  FTokenID := tkSymbol;
  Inc(Run);
end;

procedure TSynCssSyn.StartValProc;
begin
  FRange := rsValue;
  FTokenID := tkSymbol;
  Inc(Run);
end;

procedure TSynCssSyn.NumberProc;
begin
  if (FLine[Run] = '-') and not CharInSet(FLine[Run + 1], ['0'..'9']) then
    IdentProc
  else
  begin
    Inc(Run);
    FTokenID := tkNumber;
    while CharInSet(FLine[Run], ['0'..'9', '.']) do
    begin
      case FLine[Run] of
        '.':
          if FLine[Run + 1] = '.' then Break;
      end;
      Inc(Run);
    end;
  end;
end;

procedure TSynCssSyn.ParenCloseProc;
begin
  FRange := FParameterRange;
  FTokenID := tkSymbol;
  Inc(Run);
end;

procedure TSynCssSyn.ParenOpenProc;
begin
  Inc(Run);
  FParameterRange := FRange;
  FRange := rsParameter;
  FTokenID := tkSymbol;
end;

procedure TSynCssSyn.PipeProc;
begin
  Inc(Run);
  if FLine[Run] = '=' then
  begin
    Inc(Run);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynCssSyn.PlusProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynCssSyn.IdentProc;
begin
  case FRange of
    rsProperty:
      begin
        FRange := rsDeclaration;
        FTokenID := tkSelector;
        Inc(Run, FStringLen);
      end;
    rsValue, rsParameter:
      begin
        FTokenID := tkValue;

        while not IsLineEnd(Run) and
          not CharInSet(FLine[Run], ['(', ')', '}', ';', ',', ' ']) do
        begin
          Inc(Run);
        end;

        if IsLineEnd(Run) or CharInSet(FLine[Run], ['}', ';']) then
          FRange := rsDeclaration;
      end;
    else
      FTokenID := IdentKind((FLine + Run));
      repeat
        Inc(Run);
      until (FLine[Run] <= #32) or CharInSet(FLine[Run], [':', '"', '}', ';']);
  end;
end;

procedure TSynCssSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynCssSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynCssSyn.AtRuleProc;

  function IsStopChar: Boolean;
  begin
    case FLine[Run] of
      #0..#31, '{', ';':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  if IsStopChar then
  begin
    case FLine[Run] of
      #0..#31, '{', ';': SelectorProc;
    end;
    Exit;
  end;

  FTokenID := tkAtRule;
  while not IsStopChar do
    Inc(Run);
end;

procedure TSynCssSyn.SelectorProc;

  function IsStopChar: Boolean;
  begin
    case FLine[Run] of
      #0..#31, '{', '/', '[', ']', '>', '+', '~':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  if FLine[Run] = '}' then
  begin
    Inc(Run);
    FTokenID := tkSymbol;
    Exit;
  end;

  if FLine[Run] = '@' then
  begin
    Inc(Run);
    AtRuleProc;
    Exit;
  end;

  if IsStopChar then
  begin
    case FLine[Run] of
      #0..#31, '{', '/': NextDeclaration;
      '[': BracketOpenProc;
      ']': BracketCloseProc;
      '>': GreaterProc;
      '+': PlusProc;
      '~': TildeProc;
    end;
    Exit;
  end;

  FTokenID := tkSelector;
  while not IsStopChar do
    Inc(Run);
end;

procedure TSynCssSyn.TildeProc;
begin
  Inc(Run);
  if FLine[Run] = '=' then
  begin
    Inc(Run);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynCssSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynCssSyn.StringProc;
begin
  FTokenID := tkString;
  Inc(Run);  // first '"'
  while not (IsLineEnd(Run) or (FLine[Run] = '"')) do Inc(Run);
  if FLine[Run] = '"' then Inc(Run);  // last '"'
end;

procedure TSynCssSyn.HashProc;

  function IsHexChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  FTokenID := tkColor;
  Inc(Run);  // '#'
  while IsHexChar do Inc(Run);
end;

procedure TSynCssSyn.EqualProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynCssSyn.ExclamProc;
begin
  if (FLine[Run + 1] = 'i') and
    (FLine[Run + 2] = 'm') and
    (FLine[Run + 3] = 'p') and
    (FLine[Run + 4] = 'o') and
    (FLine[Run + 5] = 'r') and
    (FLine[Run + 6] = 't') and
    (FLine[Run + 7] = 'a') and
    (FLine[Run + 8] = 'n') and
    (FLine[Run + 9] = 't') then
  begin
    FTokenID := tkImportant;
    Inc(Run, 10);
  end
  else
    IdentProc;
end;

procedure TSynCssSyn.SlashProc;
begin
  Inc(Run);
  if FLine[Run] = '*' then
  begin
    FTokenID := tkComment;
    FCommentRange := FRange;
    FRange := rsComment;
    Inc(Run);
    if not IsLineEnd(Run) then
      CommentProc;
  end
  else
    FTokenID := tkSymbol;
end;

procedure TSynCssSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsSelector:
      SelectorProc;
    rsAttrib:
      AttributeProc;
    rsComment:
      CommentProc;
    else
      NextDeclaration;
  end;

  inherited;
end;

procedure TSynCssSyn.NextDeclaration;
begin
  case FLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    '"': StringProc;
    '#': HashProc;
    '{': BraceOpenProc;
    '}': BraceCloseProc;
    '(': ParenOpenProc;
    ')': ParenCloseProc;
    ':', ',': StartValProc;
    ';': SemiProc;
    '0'..'9', '-', '.': NumberProc;
    '/': SlashProc;
    '!': ExclamProc;
    else IdentProc;
  end;
end;

function TSynCssSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_KEYWORD: Result := FSelectorAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    else Result := nil;
  end;
end;

function TSynCssSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynCssSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynCssSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkAtRule: Result := FAtRuleAttri;
    tkProperty: Result := FPropertyAttri;
    tkSelector: Result := FSelectorAttri;
    tkSelectorAttrib: Result := FAttributeAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkText: Result := FTextAttri;
    tkUndefProperty: Result := FUndefPropertyAttri;
    tkImportant: Result := FImportantPropertyAttri;
    tkValue: Result := FValueAttri;
    tkColor: Result := FColorAttri;
    tkNumber: Result := FNumberAttri;
    else Result := nil;
  end;
end;

function TSynCssSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynCssSyn.GreaterProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

function TSynCssSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

procedure TSynCssSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynCssSyn.ResetRange;
begin
  FRange:= rsSelector;
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
  Result := FDefaultFilter <> SYNS_FilterCSS;
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
