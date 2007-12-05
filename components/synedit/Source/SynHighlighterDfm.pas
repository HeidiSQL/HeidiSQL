{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterDfm.pas, released 2000-04-14.
The Original Code is based on the dmDfmSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is David H. Muir.
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

$Id: SynHighlighterDfm.pas,v 1.16.2.6 2006/05/21 11:59:35 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Delphi Form Source highlighter for SynEdit)
@author(David Muir <david@loanhead45.freeserve.co.uk>)
@created(April 13, 2000)
@lastmod(2000-06-23)
The SynHighlighterDfm unit provides SynEdit with a Delphi Form Source (.dfm) highlighter.
The highlighter formats form source code similar to when forms are viewed as text in the Delphi editor.
}

{$IFNDEF QSYNHIGHLIGHTERDFM}
unit SynHighlighterDfm;
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
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkUnknown);

  TRangeState = (rsANil, rsComment, rsUnKnown);

type
  TSynDfmSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    FTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    procedure AltProc;
    procedure AsciiCharProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CommentProc;
    procedure CRProc;
    procedure EndProc;
    procedure IntegerProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure ObjectProc;
    procedure InheritedProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
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
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
  end;

function LoadDFMFile2Strings(const AFile: WideString; AStrings: TWideStrings;
  var WasText: Boolean): Integer; overload;
function LoadDFMFile2Strings(const AFile: string; AStrings: TStrings;
  var WasText: Boolean): Integer; overload;
function SaveStrings2DFMFile(AStrings: TWideStrings;
  const AFile: WideString): Integer; overload;
function SaveStrings2DFMFile(AStrings: TStrings;
  const AFile: string): Integer; overload;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

{ A couple of useful Delphi Form functions }

function LoadDFMFile2Strings(const AFile: WideString; AStrings: TWideStrings;
  var WasText: Boolean): Integer;
var
  Src, Dest: TStream;
  origFormat: TStreamOriginalFormat;
begin
  Result := 0;
  WasText := FALSE;
  AStrings.Clear;
  try
    Src := TWideFileStream.Create(AFile, fmOpenRead or fmShareDenyWrite);
    try
      Dest := TMemoryStream.Create;
      try
        origFormat := sofUnknown;
        ObjectResourceToText(Src, Dest, origFormat);
        WasText := origFormat = sofText;
        Dest.Seek(0, soFromBeginning);
        AStrings.LoadFromStream(Dest);
      finally
        Dest.Free;
      end;
    finally
      Src.Free;
    end;
  except
    on E: EInOutError do Result := -E.ErrorCode;
    else Result := -1;
  end;
end;

function LoadDFMFile2Strings(const AFile: string; AStrings: TStrings;
  var WasText: Boolean): Integer;
var
  Src, Dest: TStream;
{$IFDEF SYN_COMPILER_5_UP}
  origFormat: TStreamOriginalFormat;
{$ENDIF}
begin
  Result := 0;
  WasText := FALSE;
  AStrings.Clear;
  try
    Src := TFileStream.Create(AFile, fmOpenRead or fmShareDenyWrite);
    try
      Dest := TMemoryStream.Create;
      try
{$IFDEF SYN_COMPILER_5_UP}
        origFormat := sofUnknown;
        ObjectResourceToText(Src, Dest, origFormat);
        WasText := origFormat = sofText;
{$ELSE}
        ObjectResourceToText(Src, Dest);
{$ENDIF}
        Dest.Seek(0, soFromBeginning);
        AStrings.LoadFromStream(Dest);
      finally
        Dest.Free;
      end;
    finally
      Src.Free;
    end;
  except
    on E: EInOutError do Result := -E.ErrorCode;
    else Result := -1;
  end;
end;

function SaveStrings2DFMFile(AStrings: TWideStrings; const AFile: WideString): Integer;
var
  Src, Dest: TStream;
  OldSaveUnicode: Boolean;
begin
  Result := 0;
  try
    Src := TMemoryStream.Create;
    try
      OldSaveUnicode := AStrings.SaveUnicode;
      AStrings.SaveUnicode := False;
      AStrings.SaveToStream(Src);
      AStrings.SaveUnicode := OldSaveUnicode;
      Src.Seek(0, soFromBeginning);
      Dest := TWideFileStream.Create(AFile, fmCreate);
      try
        ObjectTextToResource(Src, Dest);
      finally
        Dest.Free;
      end;
    finally
      Src.Free;
    end;
  except
    on E: EInOutError do Result := -E.ErrorCode;
    else Result := -1;
  end;
end;

function SaveStrings2DFMFile(AStrings: TStrings; const AFile: string): Integer;
var
  Src, Dest: TStream;
begin
  Result := 0;
  try
    Src := TMemoryStream.Create;
    try
      AStrings.SaveToStream(Src);
      Src.Seek(0, soFromBeginning);
      Dest := TFileStream.Create(AFile, fmCreate);
      try
        ObjectTextToResource(Src, Dest);
      finally
        Dest.Free;
      end;
    finally
      Src.Free;
    end;
  except
    on E: EInOutError do Result := -E.ErrorCode;
    else Result := -1;
  end;
end;

{ TSynDfmSyn }

constructor TSynDfmSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey, SYNS_FriendlyAttrKey);
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
  fDefaultFilter := SYNS_FilterDFM;
  fRange := rsUnknown;
end;

procedure TSynDfmSyn.AltProc;
begin
  fTokenID := tkIdentifier;
  repeat
    Inc(Run);
  until not IsIdentChar(fLine[Run]);
end;

procedure TSynDfmSyn.AsciiCharProc;
begin
  fTokenID := tkString;
  repeat
    Inc(Run);
  until not (fLine[Run] in [WideChar('0')..WideChar('9')]);
end;

procedure TSynDfmSyn.BraceCloseProc;
begin
  inc(Run);
  fRange := rsUnknown;
  fTokenId := tkIdentifier;
end;

procedure TSynDfmSyn.BraceOpenProc;
begin
  fRange := rsComment;
  CommentProc;
end;

procedure TSynDfmSyn.CommentProc;
begin
  fTokenID := tkComment;
  repeat
    inc(Run);
    if fLine[Run] = '}' then begin
      Inc(Run);
      fRange := rsUnknown;
      break;
    end;
  until IsLineEnd(Run);
end;

procedure TSynDfmSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if (fLine[Run] = #10) then Inc(Run);
end;

procedure TSynDfmSyn.EndProc;
begin
  if (fLine[Run + 1] in [WideChar('n'), WideChar('N')]) and
     (fLine[Run + 2] in [WideChar('d'), WideChar('D')]) and
     not IsIdentChar(fLine[Run + 3])
  then begin
    fTokenID := tkKey;
    Inc(Run, 3);
  end else
    AltProc;
end;

procedure TSynDfmSyn.IntegerProc;

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
  fTokenID := tkNumber;
  repeat
    inc(Run);
  until not IsIntegerChar;
end;

procedure TSynDfmSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynDfmSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynDfmSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', 'e', 'E':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  fTokenID := tkNumber;
  repeat
    Inc(Run);
    if fLine[Run] = '.' then
    begin
      if fLine[Run + 1] <> '.' then Inc(Run);
      break;
    end;
  until not IsNumberChar;
end;

procedure TSynDfmSyn.ObjectProc;
begin
  if (fLine[Run + 1] in [WideChar('b'), WideChar('B')]) and
     (fLine[Run + 2] in [WideChar('j'), WideChar('J')]) and
     (fLine[Run + 3] in [WideChar('e'), WideChar('E')]) and
     (fLine[Run + 4] in [WideChar('c'), WideChar('C')]) and
     (fLine[Run + 5] in [WideChar('t'), WideChar('T')]) and
     not IsIdentChar(fLine[Run + 6])
  then
  begin
    fTokenID := tkKey;
    Inc(Run, 6);
  end
  else
    AltProc;
end;

procedure TSynDfmSyn.InheritedProc;
begin
  if (fLine[Run + 1] in [WideChar('n'), WideChar('N')]) and
     (fLine[Run + 2] in [WideChar('h'), WideChar('H')]) and
     (fLine[Run + 3] in [WideChar('e'), WideChar('E')]) and
     (fLine[Run + 4] in [WideChar('r'), WideChar('R')]) and
     (fLine[Run + 5] in [WideChar('i'), WideChar('I')]) and
     (fLine[Run + 6] in [WideChar('t'), WideChar('T')]) and
     (fLine[Run + 7] in [WideChar('e'), WideChar('E')]) and
     (fLine[Run + 8] in [WideChar('d'), WideChar('D')]) and
     not IsIdentChar(fLine[Run + 9])
  then
  begin
    fTokenID := tkKey;
    Inc(Run, 9);
  end
  else if (fLine[Run + 1] in [WideChar('n'), WideChar('N')]) and
          (fLine[Run + 2] in [WideChar('l'), WideChar('L')]) and
          (fLine[Run + 3] in [WideChar('i'), WideChar('I')]) and
          (fLine[Run + 4] in [WideChar('n'), WideChar('N')]) and
          (fLine[Run + 5] in [WideChar('e'), WideChar('E')]) and
          not IsIdentChar(fLine[Run + 6])
  then
  begin
    fTokenID := tkKey;
    Inc(Run, 6);
  end
  else
    AltProc;
end;

procedure TSynDfmSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or IsLineEnd(Run);
end;

procedure TSynDfmSyn.StringProc;
begin
  fTokenID := tkString;
  repeat
    Inc(Run);
    if fLine[Run] = '''' then begin
      Inc(Run);
      if fLine[Run] <> '''' then break
    end;
  until IsLineEnd(Run);
end;

procedure TSynDfmSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynDfmSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynDfmSyn.Next;
begin
  fTokenPos := Run;
  if fRange = rsComment then
  begin
    if fLine[Run] = #0 then
      NullProc
    else
      CommentProc;
  end
  else
    case fLine[Run] of
      '#': AsciiCharProc;
      '}': BraceCloseProc;
      '{': BraceOpenProc;
      #13: CRProc;
      'A'..'Z', 'a'..'z', '_':
        if fLine[Run] in [WideChar('e'), WideChar('E')] then
          EndProc
        else if fLine[Run] in [WideChar('o'), WideChar('O')] then
          ObjectProc
        else if fLine[Run] in [WideChar('i'), WideChar('I')] then
          InheritedProc
        else
          AltProc;
      '$': IntegerProc;
      #10: LFProc;
      #0: NullProc;
      '0'..'9': NumberProc;
      '(', ')', '/', '=', '<', '>', '.', ',', '[', ']': SymbolProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      #39: StringProc;
      else UnknownProc;
    end;
  inherited;
end;

function TSynDfmSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TSynDfmSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynDfmSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynDfmSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynDfmSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynDfmSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenID);
end;

procedure TSynDfmSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynDfmSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynDfmSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterDFM;
end;

class function TSynDfmSyn.GetLanguageName: string;
begin
  Result := SYNS_LangDfm;
end;

function TSynDfmSyn.GetSampleSource: WideString;
begin
  Result := '{ Delphi/C++ Builder Form Definitions }'#13#10 +
            'object TestForm: TTestForm'#13#10 +
            '  Left = 273'#13#10 +
            '  Top = 103'#13#10 +
            '  Caption = ''SynEdit sample source'''#13#10 +
            'end';
end; { GetSampleSource }

class function TSynDfmSyn.GetFriendlyLanguageName: WideString;
begin
  Result := SYNS_FriendlyLangDfm;
end;

{$IFNDEF SYN_CPPB_1}
initialization
  RegisterPlaceableHighlighter(TSynDfmSyn);
{$ENDIF}
end.
