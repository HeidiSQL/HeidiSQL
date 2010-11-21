{------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterTex.pas, released 2002-09-18.
Author of this file is Soeren Sproessig.
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

$Id: SynHighlighterTeX.pas,v 1.5.2.5 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of this file from sproessig@bs-webdesign.de

The unit SynHighlighterTeX provides SynEdit with a TeX highlighter.

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNHIGHLIGHTERTEX}
unit SynHighlighterTeX;
{$ENDIF}

{$I SynEdit.inc}

interface

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
  Classes;

type
  TtkTokenKind = (tkBrace, tkBracket, tkNull, tkSpace, tkText, tkComment,
                  tkControlSequence, tkMathMode);

type
  TSynTeXSyn = class(TSynCustomHighlighter)
  private
    fTokenID: TtkTokenKind;
    fTextAttri: TSynHighlighterAttributes;
    fControlSequenceAttri: TSynHighlighterAttributes;
    fMathmodeAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fBracketAttri: TSynHighlighterAttributes;
    fBraceAttri: TSynHighlighterAttributes;

    function CreateHighlighterAttributes(Name: string; FriendlyName: UnicodeString;
      Foreground, Background: TColor; FontStyles: TFontStyles): TSynHighlighterAttributes;
    procedure CRProc;
    procedure TextProc;
    procedure LFProc;
    procedure NullProc;
    procedure CommentProc;
    procedure SpaceProc;
    procedure ControlSequenceProc;
    procedure BraceOpenProc;
    procedure BraceCloseProc;
    procedure BracketOpenProc;
    procedure BracketCloseProc;
    procedure MathmodeProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;    
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
    property CommentAttri : TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property TextAttri: TSynHighlighterAttributes read fTextAttri
      write fTextAttri;
    property ControlSequenceAttri: TSynHighlighterAttributes read
fControlSequenceAttri
      write fControlSequenceAttri;
    property MathmodeAttri: TSynHighlighterAttributes read fMathmodeAttri
      write fMathmodeAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property BraceAttri: TSynHighlighterAttributes read fBraceAttri
      write fBraceAttri;
    property BracketAttri: TSynHighlighterAttributes read fBracketAttri
      write fBracketAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

constructor TSynTeXSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCommentAttri := CreateHighlighterAttributes(SYNS_AttrComment, SYNS_FriendlyAttrComment, clTeal, clNone, []);
  AddAttribute(fCommentAttri);

  fTextAttri := CreateHighlighterAttributes(SYNS_AttrText, SYNS_FriendlyAttrText, clBlack, clNone, []);
  AddAttribute(fTextAttri);

  fMathmodeAttri := CreateHighlighterAttributes(SYNS_AttrMathmode, SYNS_FriendlyAttrMathmode, clOlive, clNone,
    [fsbold]);
  AddAttribute(fMathmodeAttri);

  fSpaceAttri := CreateHighlighterAttributes(SYNS_AttrSpace, SYNS_FriendlyAttrSpace, clNone, clWhite, []);
  AddAttribute(fSpaceAttri);

  fControlSequenceAttri := CreateHighlighterAttributes(SYNS_AttrTexCommand, SYNS_FriendlyAttrTexCommand, clBlue,
    clWhite, [fsBold]);
  AddAttribute(fControlSequenceAttri);

  fBracketAttri := CreateHighlighterAttributes(SYNS_AttrSquareBracket, SYNS_FriendlyAttrSquareBracket, clPurple,
    clNone, []);
  AddAttribute(fBracketAttri);

  fBraceAttri:= CreateHighlighterAttributes(SYNS_AttrRoundBracket, SYNS_FriendlyAttrRoundBracket, clRed,
    clNone, [fsBold]);
  AddAttribute(fBraceAttri);

  SetAttributesOnChange(DefHighlightChange);
  fDefaultFilter := SYNS_FilterTeX;
end;  { Create }

procedure TSynTeXSyn.CRProc;
begin
  fTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: inc(Run, 2);
    else inc(Run);
  end;
end;  { CRProc }


procedure TSynTeXSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;  { SpaceProc }

procedure TSynTeXSyn.TextProc;
begin
  fTokenID := tkText;
  inc(Run);
end;  { TextProc }

procedure TSynTeXSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;  { SpaceProc }

procedure TSynTeXSyn.BraceOpenProc;
begin
  fTokenID := tkBrace;
  inc(Run);
end;  { BraceOpen }

procedure TSynTeXSyn.BraceCloseProc;
begin
  fTokenID := tkBrace;
  inc(Run);
end;  { BraceClose }

procedure TSynTeXSyn.BracketOpenProc;
begin
  fTokenID := tkBracket;
  inc(Run);
end;  { BracketOpen }

procedure TSynTeXSyn.BracketCloseProc;
begin
  fTokenID := tkBracket;
  inc(Run);
end;  { BracketClose }

procedure TSynTeXSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;  { NullProc }

procedure TSynTeXSyn.CommentProc;
begin
 fTokenID := tkComment;
 repeat
    case fLine[Run] of
      #0, #10: Break;
    end;
    inc(Run);
  until fLine[Run] = #13;
  Exit;
end;  { CommentProc }

procedure TSynTeXSyn.MathModeProc;
begin
 fTokenID := tkMathMode;
 Inc(Run);
end;  { MathModeProc }

procedure TSynTeXSyn.ControlSequenceProc;
begin
 fTokenID := tkControlSequence;
 repeat
   case fLine[Run] of
     #0..#31: Break;  //No Control Chars !
     #48..#57: Break;  //No Numbers !
     #33..#47, #58..#64,               //Just the Characters that
     #91, #93,#94, #123,              //only can follow to '\'
     #125, #126:
       begin
         if (fLine[Run-1]='\') then
           Inc(Run,1);
         Break;
       end;
   end;
   Inc(Run);
 until fLine[Run] = #32;
 exit;
end;  { ControlSequenceProc }

procedure TSynTeXSyn.Next;
begin
  fTokenPos := Run;
  case  fLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    #37: CommentProc;
    #92: ControlSequenceProc;
    #123: BraceOpenProc;
    #125: BraceCloseProc;
    #91: BracketOpenProc;
    #93: BracketCloseProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    #36: MathmodeProc;
    else TextProc;
  end;
  inherited;
end;  { Next }

function TSynTeXSyn.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    else Result := nil;
  end;
end;

function TSynTeXSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;  { GetDefaultAttribute }

function TSynTeXSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;  { GetTokenID }

function TSynTeXSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkText: Result := fTextAttri;
    tkControlSequence: Result := fControlSequenceAttri;
    tkMathMode: Result := fMathmodeAttri;
    tkSpace: Result := fSpaceAttri;
    tkBrace: Result := fBraceAttri;
    tkBracket: Result := fBracketAttri;
  else
    Result := nil;
  end;
end;  { GetTokenAttribute }

function TSynTeXSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;  { GetTokenKind }

function TSynTeXSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterTeX;
end;

class function TSynTeXSyn.GetLanguageName: string;
begin
  Result := SYNS_LangTeX;
end;  { GetLanguageName }

function TSynTeXSyn.CreateHighlighterAttributes(Name: string; FriendlyName: UnicodeString;
  Foreground, Background: TColor; FontStyles: TFontStyles): TSynHighlighterAttributes;
begin
  Result := TSynHighlighterAttributes.Create(Name, FriendlyName);
  if Foreground <> clNone then Result.Foreground := Foreground;
  if Background <> clNone then Result.Background := Background;
  Result.Style := FontStyles;
end;

function TSynTeXSyn.GetSampleSource: UnicodeString;
begin
  Result:='\documentclass[a4paper]{article}'+#13#10+
          '% LaTeX sample source'+#13#10+
          '\begin{document}'+#13#10+
          'Here is a formula: $ (2x + 3)*5y $'+#13#10+
          '\end{document}';
end;

{$IFNDEF SYN_CPPB_1}
class function TSynTeXSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangTeX;
end;

initialization
  RegisterPlaceableHighlighter(TSynTeXSyn);
{$ENDIF}
end.
