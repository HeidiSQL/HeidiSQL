{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterXML.pas, released 2000-11-20.
The Initial Author of this file is Jeff Rafter.
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

$Id: SynHighlighterXML.pas,v 1.11.2.6 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

History:
-------------------------------------------------------------------------------
2000-11-30 Removed mHashTable and MakeIdentTable per Michael Hieke

Known Issues:
- Nothing is really constrained (properly) to valid name chars
- Entity Refs are not constrained to valid name chars
- Support for "Combining Chars and Extender Chars" in names are lacking
- The internal DTD is not parsed (and not handled correctly)
-------------------------------------------------------------------------------}
{
@abstract(Provides an XML highlighter for SynEdit)
@author(Jeff Rafter-- Phil 4:13, based on SynHighlighterHTML by Hideo Koiso)
@created(2000-11-17)
@lastmod(2001-03-12)
The SynHighlighterXML unit provides SynEdit with an XML highlighter.
}

unit SynHighlighterXML;

interface

{$I SynEdit.inc}

uses
  Windows, Messages, Controls, Graphics, Registry,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
  SysUtils,
{$IFDEF SYN_CodeFolding}
  SynEditCodeFolding,
{$ENDIF}
  Classes;

type
  TtkTokenKind = (tkAposAttrValue, tkAposEntityRef, tkAttribute, tkCDATA,
    tkComment, tkElement, tkEntityRef, tkEqual, tkNull, tkProcessingInstruction,
    tkQuoteAttrValue, tkQuoteEntityRef, tkSpace, tkSymbol, tkText,
    //
    tknsAposAttrValue, tknsAposEntityRef, tknsAttribute, tknsEqual,
    tknsQuoteAttrValue, tknsQuoteEntityRef,
    //These are unused at the moment
    tkDocType
    {tkDocTypeAposAttrValue, tkDocTypeAposEntityRef, tkDocTypeAttribute,
     tkDocTypeElement, tkDocTypeEqual tkDocTypeQuoteAttrValue,
     tkDocTypeQuoteEntityRef}
  );

  TRangeState = (rsAposAttrValue, rsAPosEntityRef, rsAttribute, rsCDATA,
    rsComment, rsElement, rsEntityRef, rsEqual, rsProcessingInstruction,
    rsQuoteAttrValue, rsQuoteEntityRef, rsText,
    //
    rsnsAposAttrValue, rsnsAPosEntityRef, rsnsEqual, rsnsQuoteAttrValue,
    rsnsQuoteEntityRef,
    //These are unused at the moment
    rsDocType, rsDocTypeSquareBraces
    {rsDocTypeAposAttrValue, rsDocTypeAposEntityRef, rsDocTypeAttribute,
     rsDocTypeElement, rsDocTypeEqual, rsDocTypeQuoteAttrValue,
     rsDocTypeQuoteEntityRef}
  );

{$IFDEF SYN_CodeFolding}
  TSynXMLSyn = class(TSynCustomCodeFoldingHighlighter)
{$ELSE}
  TSynXMLSyn = class(TSynCustomHighlighter)
{$ENDIF}
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FElementAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FTextAttri: TSynHighlighterAttributes;
    FEntityRefAttri: TSynHighlighterAttributes;
    FProcessingInstructionAttri: TSynHighlighterAttributes;
    FCDATAAttri: TSynHighlighterAttributes;
    FCommentAttri: TSynHighlighterAttributes;
    FDocTypeAttri: TSynHighlighterAttributes;
    FAttributeAttri: TSynHighlighterAttributes;
    FnsAttributeAttri: TSynHighlighterAttributes;
    FAttributeValueAttri: TSynHighlighterAttributes;
    FnsAttributeValueAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FWantBracesParsed: Boolean;
    procedure NullProc;
    procedure CarriageReturnProc;
    procedure LineFeedProc;
    procedure SpaceProc;
    procedure LessThanProc;
    procedure GreaterThanProc;
    procedure CommentProc;
    procedure ProcessingInstructionProc;
    procedure DocTypeProc;
    procedure CDATAProc;
    procedure TextProc;
    procedure ElementProc;
    procedure AttributeProc;
    procedure QAttributeValueProc;
    procedure AAttributeValueProc;
    procedure EqualProc;
    procedure IdentProc;
    procedure NextProcedure;
    function NextTokenIs(Token: UnicodeString): Boolean;
    procedure EntityRefProc;
    procedure QEntityRefProc;
    procedure AEntityRefProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
    function IsNameChar: Boolean; virtual;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
{$IFDEF SYN_CodeFolding}
    procedure ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine: Integer; ToLine: Integer); override;
{$ENDIF}
  published
    property ElementAttri: TSynHighlighterAttributes read FElementAttri
      write FElementAttri;
    property AttributeAttri: TSynHighlighterAttributes read FAttributeAttri
      write FAttributeAttri;
    property NamespaceAttributeAttri: TSynHighlighterAttributes
      read FnsAttributeAttri write FnsAttributeAttri;
    property AttributeValueAttri: TSynHighlighterAttributes
      read FAttributeValueAttri write FAttributeValueAttri;
    property NamespaceAttributeValueAttri: TSynHighlighterAttributes
      read FnsAttributeValueAttri write FnsAttributeValueAttri;
    property TextAttri: TSynHighlighterAttributes read FTextAttri
      write FTextAttri;
    property CDATAAttri: TSynHighlighterAttributes read FCDATAAttri
      write FCDATAAttri;
    property EntityRefAttri: TSynHighlighterAttributes read FEntityRefAttri
      write FEntityRefAttri;
    property ProcessingInstructionAttri: TSynHighlighterAttributes
      read FProcessingInstructionAttri write FProcessingInstructionAttri;
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property DocTypeAttri: TSynHighlighterAttributes read FDocTypeAttri
      write FDocTypeAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
    property WantBracesParsed : Boolean read FWantBracesParsed
      write FWantBracesParsed default True;
  end;

implementation

uses
  SynEditStrConst;

constructor TSynXMLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

  FElementAttri := TSynHighlighterAttributes.Create(SYNS_AttrElementName, SYNS_FriendlyAttrElementName);
  FTextAttri := TSynHighlighterAttributes.Create(SYNS_AttrText, SYNS_FriendlyAttrText);
  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrWhitespace, SYNS_FriendlyAttrWhitespace);
  FEntityRefAttri := TSynHighlighterAttributes.Create(SYNS_AttrEntityReference, SYNS_FriendlyAttrEntityReference);
  FProcessingInstructionAttri := TSynHighlighterAttributes.Create(
    SYNS_AttrProcessingInstr, SYNS_FriendlyAttrProcessingInstr);
  FCDATAAttri := TSynHighlighterAttributes.Create(SYNS_AttrCDATASection, SYNS_FriendlyAttrCDATASection);
  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FDocTypeAttri := TSynHighlighterAttributes.Create(SYNS_AttrDOCTYPESection, SYNS_FriendlyAttrDOCTYPESection);
  FAttributeAttri := TSynHighlighterAttributes.Create(SYNS_AttrAttributeName, SYNS_FriendlyAttrAttributeName);
  FnsAttributeAttri := TSynHighlighterAttributes.Create(
    SYNS_AttrNamespaceAttrName, SYNS_FriendlyAttrNamespaceAttrName);
  FAttributeValueAttri := TSynHighlighterAttributes.Create(
    SYNS_AttrAttributeValue, SYNS_FriendlyAttrAttributeValue);
  FnsAttributeValueAttri := TSynHighlighterAttributes.Create(
    SYNS_AttrNamespaceAttrValue, SYNS_FriendlyAttrNamespaceAttrValue);
  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);

  FElementAttri.Foreground := clMaroon;
  FElementAttri.Style := [fsBold];

  FDocTypeAttri.Foreground := clblue;
  FDocTypeAttri.Style := [fsItalic];

  FCDATAAttri.Foreground := clOlive;
  FCDATAAttri.Style := [fsItalic];

  FEntityRefAttri.Foreground := clblue;
  FEntityRefAttri.Style := [fsbold];

  FProcessingInstructionAttri.Foreground:= clblue;
  FProcessingInstructionAttri.Style:= [];

  FTextAttri.Foreground := clBlack;
  FTextAttri.Style := [fsBold];

  FAttributeAttri.Foreground := clMaroon;
  FAttributeAttri.Style := [];

  FnsAttributeAttri.Foreground := clRed;
  FnsAttributeAttri.Style := [];

  FAttributeValueAttri.Foreground := clNavy;
  FAttributeValueAttri.Style := [fsBold];

  FnsAttributeValueAttri.Foreground := clRed;
  FnsAttributeValueAttri.Style := [fsBold];

  FCommentAttri.Background := clSilver;
  FCommentAttri.Foreground := clGray;
  FCommentAttri.Style := [fsbold, fsItalic];

  FSymbolAttri.Foreground := clblue;
  FSymbolAttri.Style := [];

  AddAttribute(FSymbolAttri);
  AddAttribute(FProcessingInstructionAttri);
  AddAttribute(FDocTypeAttri);
  AddAttribute(FCommentAttri);
  AddAttribute(FElementAttri);
  AddAttribute(FAttributeAttri);
  AddAttribute(FnsAttributeAttri);
  AddAttribute(FAttributeValueAttri);
  AddAttribute(FnsAttributeValueAttri);
  AddAttribute(FEntityRefAttri);
  AddAttribute(FCDATAAttri);
  AddAttribute(FSpaceAttri);
  AddAttribute(FTextAttri);

  SetAttributesOnChange(DefHighlightChange);

  FRange := rsText;
  FDefaultFilter := SYNS_FilterXML;
end;

procedure TSynXMLSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynXMLSyn.CarriageReturnProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then Inc(Run);
end;

procedure TSynXMLSyn.LineFeedProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynXMLSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while FLine[Run] <= #32 do
  begin
    if CharInSet(FLine[Run], [#0, #9, #10, #13]) then Break;
    Inc(Run);
  end;
end;

procedure TSynXMLSyn.LessThanProc;
begin
  Inc(Run);
  if (FLine[Run] = '/') then
    Inc(Run);

  if (FLine[Run] = '!') then
  begin
    if NextTokenIs('--') then
    begin
      FTokenID := tkSymbol;
      FRange := rsComment;
      Inc(Run, 3);
    end
    else if NextTokenIs('DOCTYPE') then
    begin
      FTokenID := tkDocType;
      FRange := rsDocType;
      Inc(Run, 7);
    end
    else if NextTokenIs('[CDATA[') then
    begin
      FTokenID := tkCDATA;
      FRange := rsCDATA;
      Inc(Run, 7);
    end
    else
    begin
      FTokenID := tkSymbol;
      FRange := rsElement;
      Inc(Run);
    end;
  end
  else if FLine[Run]= '?' then
  begin
    FTokenID := tkProcessingInstruction;
    FRange := rsProcessingInstruction;
    Inc(Run);
  end
  else
  begin
    FTokenID := tkSymbol;
    FRange := rsElement;
  end;
end;

procedure TSynXMLSyn.GreaterThanProc;
begin
  FTokenID := tkSymbol;
  FRange:= rsText;
  Inc(Run);
end;

procedure TSynXMLSyn.CommentProc;
begin
  if (FLine[Run] = '-') and (FLine[Run + 1] = '-') and (FLine[Run + 2] = '>') then
  begin
    FTokenID := tkSymbol;
    FRange := rsText;
    Inc(Run, 3);
    Exit;
  end;

  FTokenID := tkComment;

  if IsLineEnd(Run) then
  begin
    NextProcedure;
    Exit;
  end;

  while not IsLineEnd(Run) do
  begin
    if (FLine[Run] = '-') and (FLine[Run + 1] = '-') and (FLine[Run + 2] = '>') then
    begin
      FRange := rsComment;
      Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynXMLSyn.ProcessingInstructionProc;
begin
  FTokenID := tkProcessingInstruction;
  if IsLineEnd(Run) then
  begin
    NextProcedure;
    Exit;
  end;

  while not IsLineEnd(Run) do
  begin
    if (FLine[Run] = '>') and (FLine[Run - 1] = '?')
    then
    begin
      FRange := rsText;
      Inc(Run);
      Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynXMLSyn.DocTypeProc;
begin
  FTokenID := tkDocType;

  if IsLineEnd(Run) then
  begin
    NextProcedure;
    Exit;
  end;

  case FRange of
    rsDocType:
      begin
        while not IsLineEnd(Run) do
        begin
          case FLine[Run] of
            '[': begin
                   while True do
                   begin
                     Inc(Run);
                     case FLine[Run] of
                       ']':
                         begin
                           Inc(Run);
                           Exit;
                         end;
                       #0, #10, #13:
                         begin
                           FRange := rsDocTypeSquareBraces;
                           Exit;
                         end;
                     end;
                   end;
                 end;
            '>': begin
                   FRange := rsAttribute;
                   Inc(Run);
                   Break;
                 end;
          end;
          Inc(Run);
        end;
    end;
    rsDocTypeSquareBraces:
      begin
        while not IsLineEnd(Run) do
        begin
          if (FLine[Run] = ']') then
          begin
            FRange := rsDocType;
            Inc(Run);
            Exit;
          end;
          Inc(Run);
        end;
      end;
  end;
end;

procedure TSynXMLSyn.CDATAProc;
begin
  FTokenID := tkCDATA;
  if IsLineEnd(Run) then
  begin
    NextProcedure;
    Exit;
  end;

  while not IsLineEnd(Run) do
  begin
    if (FLine[Run] = '>') and (FLine[Run - 1] = ']')
    then
    begin
      FRange := rsText;
      Inc(Run);
      Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynXMLSyn.ElementProc;
begin
  if FLine[Run] = '/' then Inc(Run);
  while IsNameChar do Inc(Run);
  FRange := rsAttribute;
  FTokenID := tkElement;
end;

procedure TSynXMLSyn.AttributeProc;
begin
  //Check if we are starting on a closing quote
  if CharInSet(FLine[Run], [#34, #39]) then
  begin
    FTokenID := tkSymbol;
    FRange := rsAttribute;
    Inc(Run);
    Exit;
  end;
  //Read the name
  while IsNameChar do Inc(Run);
  //Check if this is an xmlns: attribute
  if (Pos('xmlns', GetToken) > 0) then
  begin
    FTokenID := tknsAttribute;
    FRange := rsnsEqual;
  end
  else
  begin
    FTokenID := tkAttribute;
    FRange := rsEqual;
  end;
end;

procedure TSynXMLSyn.EqualProc;
begin
  if FRange = rsnsEqual then
    FTokenID := tknsEqual
  else
    FTokenID := tkEqual;

  while not IsLineEnd(Run) do
  begin
    if (FLine[Run] = '/') then
    begin
      FTokenID := tkSymbol;
      FRange := rsElement;
      Inc(Run);
      Exit;
    end
    else if (FLine[Run] = #34) then
    begin
      if FRange = rsnsEqual then
        FRange := rsnsQuoteAttrValue
      else
        FRange := rsQuoteAttrValue;
      Inc(Run);
      Exit;
    end
    else if (FLine[Run] = #39) then
    begin
      if FRange = rsnsEqual then
        FRange := rsnsAPosAttrValue
      else
        FRange := rsAPosAttrValue;
      Inc(Run);
      Exit;
    end;
    Inc(Run);
  end;
end;

procedure TSynXMLSyn.QAttributeValueProc;
begin
  if FRange = rsnsQuoteAttrValue then
    FTokenID := tknsQuoteAttrValue
  else
    FTokenID := tkQuoteAttrValue;

  while not (IsLineEnd(Run) or (FLine[Run] = '&') or (FLine[Run] = #34)) do
    Inc(Run);

  if FLine[Run] = '&' then
  begin
    if FRange = rsnsQuoteAttrValue then
      FRange := rsnsQuoteEntityRef
    else
      FRange := rsQuoteEntityRef;
    Exit;
  end
  else if FLine[Run] <> #34 then
    Exit;

  FRange := rsAttribute;
end;

procedure TSynXMLSyn.AAttributeValueProc;
begin
  if FRange = rsnsAPosAttrValue then
    FTokenID := tknsAPosAttrValue
  else
    FTokenID := tkAPosAttrValue;

  while not (IsLineEnd(Run) or (FLine[Run] = '&') or (FLine[Run] = #39)) do
    Inc(Run);

  if FLine[Run] = '&' then
  begin
    if FRange = rsnsAPosAttrValue then
      FRange := rsnsAPosEntityRef
    else
      FRange := rsAPosEntityRef;
    Exit;
  end
  else if FLine[Run] <> #39 then
    Exit;

  FRange := rsAttribute;
end;

procedure TSynXMLSyn.TextProc;
begin
  if (FLine[Run] <= #31) or (FLine[Run] = '<') then
  begin
    NextProcedure;
    Exit;
  end;

  FTokenID := tkText;
  while not ((FLine[Run] <= #31) or (FLine[Run] = '<') or (FLine[Run] = '&')) do
    Inc(Run);

  if (FLine[Run] = '&') then
  begin
    FRange := rsEntityRef;
    Exit;
  end;
end;

procedure TSynXMLSyn.EntityRefProc;
begin
  FTokenID := tkEntityRef;
  FRange := rsEntityRef;
  while not ((FLine[Run] <= #32) or (FLine[Run] = ';')) do Inc(Run);
  if (FLine[Run] = ';') then Inc(Run);
  FRange := rsText;
end;

procedure TSynXMLSyn.QEntityRefProc;
begin
  if FRange = rsnsQuoteEntityRef then
    FTokenID := tknsQuoteEntityRef
  else
    FTokenID := tkQuoteEntityRef;

  while not ((FLine[Run] <= #32) or (FLine[Run] = ';')) do Inc(Run);
  if (FLine[Run] = ';') then Inc(Run);

  if FRange = rsnsQuoteEntityRef then
    FRange := rsnsQuoteAttrValue
  else
    FRange := rsQuoteAttrValue;
end;

procedure TSynXMLSyn.AEntityRefProc;
begin
  if FRange = rsnsAPosEntityRef then
    FTokenID := tknsAPosEntityRef
  else
    FTokenID := tkAPosEntityRef;

  while not ((FLine[Run] <= #32) or (FLine[Run] = ';')) do Inc(Run);
  if (FLine[Run] = ';') then Inc(Run);

  if FRange = rsnsAPosEntityRef then
    FRange := rsnsAPosAttrValue
  else
    FRange := rsAPosAttrValue;
end;

procedure TSynXMLSyn.IdentProc;
begin
  case FRange of
    rsElement:
      begin
        ElementProc;
      end;
    rsAttribute:
      begin
        AttributeProc;
      end;
    rsEqual, rsnsEqual:
      begin
        EqualProc;
      end;
    rsQuoteAttrValue, rsnsQuoteAttrValue:
      begin
        QAttributeValueProc;
      end;
    rsAposAttrValue, rsnsAPosAttrValue:
      begin
        AAttributeValueProc;
      end;
    rsQuoteEntityRef, rsnsQuoteEntityRef:
      begin
        QEntityRefProc;
      end;
    rsAposEntityRef, rsnsAPosEntityRef:
      begin
        AEntityRefProc;
      end;
    rsEntityRef:
      begin
        EntityRefProc;
      end;
    else ;
  end;
end;

procedure TSynXMLSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsText: TextProc;
    rsComment: CommentProc;
    rsProcessingInstruction: ProcessingInstructionProc;
    rsDocType, rsDocTypeSquareBraces: DocTypeProc;
    rsCDATA: CDATAProc;
    else NextProcedure;
  end;
  // ensure that one call of Next is enough to reach next token
  if (fOldRun = Run) and not GetEol then Next;
  inherited;
end;

procedure TSynXMLSyn.NextProcedure;
begin
  case FLine[Run] of
    #0: NullProc;
    #10: LineFeedProc;
    #13: CarriageReturnProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    '<': LessThanProc;
    '>': GreaterThanProc;
    else IdentProc;
  end;
end;

function TSynXMLSyn.NextTokenIs(Token: UnicodeString): Boolean;
var
  I, Len: Integer;
begin
  Result := True;
  Len := Length(Token);
  for I := 1 to Len do
    if (FLine[Run + I] <> Token[I]) then
    begin
      Result:= False;
      Break;
    end;
end;

function TSynXMLSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FAttributeAttri;
    SYN_ATTR_KEYWORD: Result := FElementAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynXMLSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynXMLSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynXMLSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkElement: Result:= FElementAttri;
    tkAttribute: Result:= FAttributeAttri;
    tknsAttribute: Result:= FnsAttributeAttri;
    tkEqual: Result:= FSymbolAttri;
    tknsEqual: Result:= FSymbolAttri;
    tkQuoteAttrValue: Result:= FAttributeValueAttri;
    tkAPosAttrValue: Result:= FAttributeValueAttri;
    tknsQuoteAttrValue: Result:= FnsAttributeValueAttri;
    tknsAPosAttrValue: Result:= FnsAttributeValueAttri;
    tkText: Result:= FTextAttri;
    tkCDATA: Result:= FCDATAAttri;
    tkEntityRef: Result:= FEntityRefAttri;
    tkQuoteEntityRef: Result:= FEntityRefAttri;
    tkAposEntityRef: Result:= FEntityRefAttri;
    tknsQuoteEntityRef: Result:= FEntityRefAttri;
    tknsAposEntityRef: Result:= FEntityRefAttri;
    tkProcessingInstruction: Result:= FProcessingInstructionAttri;
    tkComment: Result:= FCommentAttri;
    tkDocType: Result:= FDocTypeAttri;
    tkSymbol: Result:= FSymbolAttri;
    tkSpace: Result:= FSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynXMLSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

function TSynXMLSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

procedure TSynXMLSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynXMLSyn.ResetRange;
begin
  FRange := rsText;
end;

{$IFDEF SYN_CodeFolding}
procedure TSynXMLSyn.ScanForFoldRanges(FoldRanges: TSynFoldRanges;
  LinesToScan: TStrings; FromLine, ToLine: Integer);
var
  Line: Integer;
  CurLine: string;
  RunPos: Integer;
  IsClosing: Boolean;
  IndentLevel, CurLevel: Integer;
begin
  IndentLevel := 0;
  for Line := FromLine to ToLine do
  begin
    CurLine := LinesToScan[Line];
    RunPos := 1;

    // skip empty lines
    if CurLine = '' then
    begin
      FoldRanges.NoFoldInfo(Line + 1);
      Continue;
    end;

    CurLevel := 0;
    while RunPos <= Length(CurLine) do
    begin
      // scan for open tag
      if CurLine[RunPos] = '<' then
      begin
        Inc(RunPos);
        if RunPos = Length(CurLine) then
          break;

        // get tag type (prolog, closing or malformed
        case CurLine[RunPos] of
          '?' :
            begin
              // skip to end and continue
              Inc(RunPos);
              while RunPos <= Length(CurLine) do
              begin
                if CurLine[RunPos] = '?' then
                begin
                  break;
                end;
                Inc(RunPos);
              end;
              Continue;
            end;

          '/' :
            begin
              Inc(RunPos);
              if RunPos = Length(CurLine) then
                break;
              IsClosing := True;
            end;
          '>' :
            begin
              // malformed tag (without any tag name) -> skip
              Inc(RunPos);
              continue;
            end;
          else
            IsClosing := False;
        end;

        // scan for tag end-marker
        while RunPos <= Length(CurLine) do
        begin

          if CurLine[RunPos] = '>' then
          begin
            // decrease the current level if it's a closing tag
            if IsClosing then
              Dec(CurLevel)
            else
            // eventually increase the current level if it's not an empty tag
            if not (CurLine[RunPos - 1] = '/') then
              Inc(CurLevel);

            Inc(RunPos);
            break;
          end;

          Inc(RunPos);
        end;
      end;
      Inc(RunPos);
    end;

    // check whether the current level has changed, otherwise continue
    if (CurLevel = 0) then
    begin
      FoldRanges.NoFoldInfo(Line + 1);
      Continue;
    end;

    // the level has changed, but in what direction?
    if CurLevel > 0 then
    begin
      // start a new fold range for the next indent level
      IndentLevel := IndentLevel + CurLevel;
      FoldRanges.StartFoldRange(Line + 1, 1, IndentLevel);
    end
    else
    begin
      // stop the fold range for the current indent level and decrease
      FoldRanges.StopFoldRange(Line + 1, 1, IndentLevel);
      IndentLevel := IndentLevel + CurLevel;
    end;
  end; //for Line
end;
{$ENDIF}

function TSynXMLSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterXML;
end;

{ TODO: In fact every Number also non-arabics and every letter also German umlauts
  can be used. Something like IsAlphaNumericCharW should be used instead. }
function TSynXMLSyn.IsNameChar: Boolean;
begin
  case FLine[Run] of
    '0'..'9', 'a'..'z', 'A'..'Z', '_', '.', ':', '-':
      Result := True;
    else if FLine[Run] > 'À' then // TODO: this here is very vague, see above
      Result := True
    else
      Result := False;
  end;
end;

class function TSynXMLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangXML;
end;

function TSynXMLSyn.GetSampleSource: UnicodeString;
begin
  Result :=
    '<?xml version="1.0"?>'#13#10+
    '<!DOCTYPE root ['#13#10+
    '  ]>'#13#10+
    '<!-- Comment -->'#13#10+
    '<root version="&test;">'#13#10+
    '  <![CDATA[ **CDATA section** ]]>'#13#10+
    '</root>';
end;

class function TSynXMLSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangXML;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynXMLSyn);
{$ENDIF}
end.
