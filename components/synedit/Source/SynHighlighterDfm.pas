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

$Id: SynHighlighterDfm.pas,v 1.16.2.7 2008/09/14 16:25:00 maelh Exp $

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

unit SynHighlighterDfm;

{$I SynEdit.inc}

interface

uses
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkUnknown);

  TRangeState = (rsANil, rsComment, rsUnknown);

type
  TSynDfmSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
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
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri
      write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
  end;

function LoadDFMFile2Strings(const AFile: UnicodeString; AStrings: TUnicodeStrings;
  var WasText: Boolean): Integer; {$IFNDEF UNICODE} overload; {$ENDIF}
{$IFNDEF UNICODE}
function LoadDFMFile2Strings(const AFile: string; AStrings: TStrings;
  var WasText: Boolean): Integer; overload;
{$ENDIF}
function SaveStrings2DFMFile(AStrings: TUnicodeStrings;
  const AFile: UnicodeString): Integer; {$IFNDEF UNICODE} overload; {$ENDIF}
{$IFNDEF UNICODE}
function SaveStrings2DFMFile(AStrings: TStrings;
  const AFile: string): Integer; overload;
{$ENDIF}

implementation

uses
  SynEditStrConst;

{ A couple of useful Delphi Form functions }

function LoadDFMFile2Strings(const AFile: UnicodeString; AStrings: TUnicodeStrings;
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

{$IFNDEF UNICODE}
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
{$ENDIF}

function SaveStrings2DFMFile(AStrings: TUnicodeStrings; const AFile: UnicodeString): Integer;
var
  Src, Dest: TStream;
{$IFNDEF UNICODE}
  OldSaveUnicode: Boolean;
{$ENDIF}
begin
  Result := 0;
  try
    Src := TMemoryStream.Create;
    try
{$IFNDEF UNICODE}
      OldSaveUnicode := AStrings.SaveUnicode;
      AStrings.SaveUnicode := False;
{$ENDIF}
      AStrings.SaveToStream(Src);
{$IFNDEF UNICODE}
      AStrings.SaveUnicode := OldSaveUnicode;
{$ENDIF}
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

{$IFNDEF UNICODE}
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
{$ENDIF}

{ TSynDfmSyn }

constructor TSynDfmSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);
  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey, SYNS_FriendlyAttrKey);
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
  FDefaultFilter := SYNS_FilterDFM;
  FRange := rsUnknown;
end;

procedure TSynDfmSyn.AltProc;
begin
  FTokenID := tkIdentifier;
  repeat
    Inc(Run);
  until not IsIdentChar(FLine[Run]);
end;

procedure TSynDfmSyn.AsciiCharProc;
begin
  FTokenID := tkString;
  repeat
    Inc(Run);
  until not CharInSet(FLine[Run], ['0'..'9']);
end;

procedure TSynDfmSyn.BraceCloseProc;
begin
  Inc(Run);
  FRange := rsUnknown;
  FTokenID := tkIdentifier;
end;

procedure TSynDfmSyn.BraceOpenProc;
begin
  FRange := rsComment;
  CommentProc;
end;

procedure TSynDfmSyn.CommentProc;
begin
  FTokenID := tkComment;
  repeat
    Inc(Run);
    if FLine[Run] = '}' then begin
      Inc(Run);
      FRange := rsUnknown;
      Break;
    end;
  until IsLineEnd(Run);
end;

procedure TSynDfmSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if (FLine[Run] = #10) then Inc(Run);
end;

procedure TSynDfmSyn.EndProc;
begin
  if CharInSet(FLine[Run + 1], ['n', 'N']) and
     CharInSet(FLine[Run + 2], ['d', 'D']) and
     not IsIdentChar(FLine[Run + 3])
  then
  begin
    FTokenID := tkKey;
    Inc(Run, 3);
  end
  else
    AltProc;
end;

procedure TSynDfmSyn.IntegerProc;

  function IsIntegerChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  FTokenID := tkNumber;
  repeat
    Inc(Run);
  until not IsIntegerChar;
end;

procedure TSynDfmSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynDfmSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynDfmSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', 'e', 'E':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  FTokenID := tkNumber;
  repeat
    Inc(Run);
    if FLine[Run] = '.' then
    begin
      if FLine[Run + 1] <> '.' then Inc(Run);
      Break;
    end;
  until not IsNumberChar;
end;

procedure TSynDfmSyn.ObjectProc;
begin
  if CharInSet(FLine[Run + 1], ['b', 'B']) and
     CharInSet(FLine[Run + 2], ['j', 'J']) and
     CharInSet(FLine[Run + 3], ['e', 'E']) and
     CharInSet(FLine[Run + 4], ['c', 'C']) and
     CharInSet(FLine[Run + 5], ['t', 'T']) and
     not IsIdentChar(FLine[Run + 6])
  then
  begin
    FTokenID := tkKey;
    Inc(Run, 6);
  end
  else
    AltProc;
end;

procedure TSynDfmSyn.InheritedProc;
begin
  if CharInSet(FLine[Run + 1], ['n', 'N']) and
     CharInSet(FLine[Run + 2], ['h', 'H']) and
     CharInSet(FLine[Run + 3], ['e', 'E']) and
     CharInSet(FLine[Run + 4], ['r', 'R']) and
     CharInSet(FLine[Run + 5], ['i', 'I']) and
     CharInSet(FLine[Run + 6], ['t', 'T']) and
     CharInSet(FLine[Run + 7], ['e', 'E']) and
     CharInSet(FLine[Run + 8], ['d', 'D']) and
     not IsIdentChar(FLine[Run + 9])
  then
  begin
    FTokenID := tkKey;
    Inc(Run, 9);
  end
  else if CharInSet(FLine[Run + 1], ['n', 'N']) and
          CharInSet(FLine[Run + 2], ['l', 'L']) and
          CharInSet(FLine[Run + 3], ['i', 'I']) and
          CharInSet(FLine[Run + 4], ['n', 'N']) and
          CharInSet(FLine[Run + 5], ['e', 'E']) and
          not IsIdentChar(FLine[Run + 6])
  then
  begin
    FTokenID := tkKey;
    Inc(Run, 6);
  end
  else
    AltProc;
end;

procedure TSynDfmSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until (FLine[Run] > #32) or IsLineEnd(Run);
end;

procedure TSynDfmSyn.StringProc;
begin
  FTokenID := tkString;
  repeat
    Inc(Run);
    if FLine[Run] = '''' then
    begin
      Inc(Run);
      if FLine[Run] <> '''' then
        Break
    end;
  until IsLineEnd(Run);
end;

procedure TSynDfmSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynDfmSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynDfmSyn.Next;
begin
  FTokenPos := Run;
  if FRange = rsComment then
  begin
    if FLine[Run] = #0 then
      NullProc
    else
      CommentProc;
  end
  else
    case FLine[Run] of
      '#': AsciiCharProc;
      '}': BraceCloseProc;
      '{': BraceOpenProc;
      #13: CRProc;
      'A'..'Z', 'a'..'z', '_':
        if CharInSet(FLine[Run], ['e', 'E']) then
          EndProc
        else if CharInSet(FLine[Run], ['o', 'O']) then
          ObjectProc
        else if CharInSet(FLine[Run], ['i', 'I']) then
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

function TSynDfmSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynDfmSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynDfmSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynDfmSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynDfmSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynDfmSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynDfmSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynDfmSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynDfmSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterDFM;
end;

class function TSynDfmSyn.GetLanguageName: string;
begin
  Result := SYNS_LangDfm;
end;

function TSynDfmSyn.GetSampleSource: UnicodeString;
begin
  Result := '{ Delphi/C++ Builder Form Definitions }'#13#10 +
            'object TestForm: TTestForm'#13#10 +
            '  Left = 273'#13#10 +
            '  Top = 103'#13#10 +
            '  Caption = ''SynEdit sample source'''#13#10 +
            'end';
end; { GetSampleSource }

class function TSynDfmSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangDfm;
end;

{$IFNDEF SYN_CPPB_1}
initialization
  RegisterPlaceableHighlighter(TSynDfmSyn);
{$ENDIF}
end.
