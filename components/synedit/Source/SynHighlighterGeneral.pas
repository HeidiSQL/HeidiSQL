{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterGeneral.pas, released 2000-04-07.
The Original Code is based on the mwGeneralSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Martin Waldenburg.
Portions written by Martin Waldenburg are copyright 1999 Martin Waldenburg.
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

$Id: SynHighlighterGeneral.pas,v 1.12 2011/04/14 15:12:54 Egg Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a customizable highlighter for SynEdit)
@author(Martin Waldenburg, converted to SynEdit by Michael Hieke)
@created(1999)
@lastmod(2000-06-23)
The SynHighlighterGeneral unit provides a customizable highlighter for SynEdit.
}

unit SynHighlighterGeneral;

{$I SynEdit.inc}

interface

uses
  Windows,
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber,
    tkPreprocessor, tkSpace, tkString, tkSymbol, tkUnknown);

  TCommentStyle = (csAnsiStyle, csPasStyle, csCStyle, csAsmStyle, csBasStyle,
    csCPPStyle);
  TCommentStyles = set of TCommentStyle;

  TRangeState = (rsANil, rsAnsi, rsPasStyle, rsCStyle, rsString, rsUnknown);

  TStringDelim = (sdSingleQuote, sdDoubleQuote, sdSingleAndDoubleQuote);

  TGetTokenAttributeEvent = procedure (attribute : TSynHighlighterAttributes) of object;

const
   cDefaultIdentChars = '_0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
                         'abcdefghijklmnopqrstuvwxyz';

type
  TSynGeneralSyn = class(TSynCustomHighlighter)
  private
    FIdentChars: UnicodeString;
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FPreprocessorAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FKeyWords: TUnicodeStrings;
    FComments: TCommentStyles;
    FStringDelim: TStringDelim;
    FDetectPreprocessor: Boolean;
    FOnGetTokenAttribute: TGetTokenAttributeEvent;
    FStringMultiLine : Boolean;
    FStringDelimChar: WideChar;
    procedure AsciiCharProc;
    procedure BraceOpenProc;
    procedure PointCommaProc;
    procedure CRProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RoundOpenProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure AnsiProc;
    procedure PasStyleProc;
    procedure CStyleProc;
    procedure SetKeyWords(const Value: TUnicodeStrings);
    procedure SetComments(Value: TCommentStyles);
    function GetStringDelim: TStringDelim;
    procedure SetStringDelim(const Value: TStringDelim);
    function GetIdentifierChars: UnicodeString;
    procedure SetIdentifierChars(const Value: UnicodeString);
    function StoreIdentChars : Boolean;
    procedure SetDetectPreprocessor(Value: Boolean);
    procedure SetStringMultiLine(const Value: Boolean);
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
    function IsStringDelim(aChar : WideChar) : Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetCharBeforeToken(offset : Integer = -1) : WideChar;
    function GetCharAfterToken(offset : Integer = 1) : WideChar;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    function IsKeyword(const AKeyword: UnicodeString): Boolean; override;
    function IsWordBreakChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;

    function SaveToRegistry(RootKey: HKEY; Key: string): Boolean; override;
    function LoadFromRegistry(RootKey: HKEY; Key: string): Boolean; override;

    property OnGetTokenAttribute : TGetTokenAttributeEvent read FOnGetTokenAttribute write FOnGetTokenAttribute;
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
    property Comments: TCommentStyles read FComments write SetComments default [];
    property DetectPreprocessor: Boolean read FDetectPreprocessor write SetDetectPreprocessor;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property IdentifierChars: UnicodeString read GetIdentifierChars write SetIdentifierChars stored StoreIdentChars;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property KeyWords: TUnicodeStrings read FKeyWords write SetKeyWords;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property PreprocessorAttri: TSynHighlighterAttributes read FPreprocessorAttri write FPreprocessorAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property StringDelim: TStringDelim read GetStringDelim write SetStringDelim default sdSingleQuote;
    property StringMultiLine: Boolean read FStringMultiLine write SetStringMultiLine;
  end;

implementation

uses
  SynEditStrConst;

constructor TSynGeneralSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKeyWords := TUnicodeStringList.Create;
  TUnicodeStringList(FKeyWords).Sorted := True;
  TUnicodeStringList(FKeyWords).Duplicates := dupIgnore;
  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);
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
  FPreprocessorAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  AddAttribute(FPreprocessorAttri);
  SetAttributesOnChange(DefHighlightChange);

  FStringDelim := sdSingleQuote;
  FIdentChars := cDefaultIdentChars;
  FRange := rsUnknown;
end; { Create }

destructor TSynGeneralSyn.Destroy;
begin
  FKeyWords.Free;
  inherited Destroy;
end; { Destroy }

function TSynGeneralSyn.IsIdentChar(AChar: WideChar): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Length(FIdentChars) do
    if AChar = FIdentChars[i] then
    begin
      Result := True;
      Exit;
    end;
end;

function TSynGeneralSyn.IsKeyword(const AKeyword: UnicodeString): Boolean;
var
  First, Last, I, Compare: Integer;
  Token: UnicodeString;
begin
  First := 0;
  Last := FKeyWords.Count - 1;
  Result := False;
  Token := SynWideUpperCase(AKeyword);
  while First <= Last do
  begin
    I := (First + Last) shr 1;
    Compare := WideCompareText(FKeyWords[i], Token);
    if Compare = 0 then
    begin
      Result := True;
      Break;
    end
    else if Compare < 0 then
      First := I + 1
    else
      Last := I - 1;
  end;
end; { IsKeyWord }

function TSynGeneralSyn.IsWordBreakChar(AChar: WideChar): Boolean;
begin
  Result := inherited IsWordBreakChar(AChar) and not IsIdentChar(AChar);
end;

procedure TSynGeneralSyn.AnsiProc;
begin
  case FLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    FTokenID := tkComment;
    repeat
      if (FLine[Run] = '*') and (FLine[Run + 1] = ')') then
      begin
        FRange := rsUnknown;
        Inc(Run, 2);
        Break;
      end;
      Inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynGeneralSyn.PasStyleProc;
begin
  case FLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    FTokenID := tkComment;
    repeat
      if FLine[Run] = '}' then
      begin
        FRange := rsUnknown;
        Inc(Run);
        Break;
      end;
      Inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynGeneralSyn.CStyleProc;
begin
  case FLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    FTokenID := tkComment;
    repeat
      if (FLine[Run] = '*') and (FLine[Run + 1] = '/') then
      begin
        FRange := rsUnknown;
        Inc(Run, 2);
        Break;
      end;
      Inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynGeneralSyn.AsciiCharProc;
begin
  if FDetectPreprocessor then
  begin
    FTokenID := tkPreprocessor;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end
  else
  begin
    FTokenID := tkString;
    repeat
      Inc(Run);
    until not CharInSet(FLine[Run], ['0'..'9']);
  end;
end;

procedure TSynGeneralSyn.BraceOpenProc;
begin
  if csPasStyle in FComments then
  begin
    FTokenID := tkComment;
    FRange := rsPasStyle;
    Inc(Run);
    while FLine[Run] <> #0 do
      case FLine[Run] of
        '}':
          begin
            FRange := rsUnknown;
            Inc(Run);
            Break;
          end;
        #10, #13:
          Break;
      else
        Inc(Run);
      end;
  end
  else
  begin
    Inc(Run);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynGeneralSyn.PointCommaProc;
begin
  if (csASmStyle in FComments) or (csBasStyle in FComments) then
  begin
    FTokenID := tkComment;
    FRange := rsUnknown;
    Inc(Run);
    while FLine[Run] <> #0 do
    begin
      FTokenID := tkComment;
      Inc(Run);
    end;
  end
  else
  begin
    Inc(Run);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynGeneralSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then Inc(Run);
end;

procedure TSynGeneralSyn.IdentProc;
begin
  while IsIdentChar(FLine[Run]) do Inc(Run);
  if IsKeyWord(GetToken) then
    FTokenID := tkKey
  else
    FTokenID := tkIdentifier;
end;

procedure TSynGeneralSyn.IntegerProc;

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
  Inc(Run);
  FTokenID := tkNumber;
  while IsIntegerChar do Inc(Run);
end;

procedure TSynGeneralSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynGeneralSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynGeneralSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', '.', 'e', 'E', 'x':
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
      'x':
        begin // handle C style hex numbers
          IntegerProc;
          Break;
        end;
      '.':
        if FLine[Run + 1] = '.' then Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynGeneralSyn.RoundOpenProc;
begin
  Inc(Run);
  if csAnsiStyle in FComments then
  begin
    case FLine[Run] of
      '*':
        begin
          FTokenID := tkComment;
          FRange := rsAnsi;
          Inc(Run);
          while FLine[Run] <> #0 do
            case FLine[Run] of
              '*':
                if FLine[Run + 1] = ')' then
                begin
                  FRange := rsUnknown;
                  Inc(Run, 2);
                  Break;
                end else Inc(Run);
              #10, #13:
                Break;
              else
                Inc(Run);
            end;
        end;
      '.':
        begin
          Inc(Run);
          FTokenID := tkSymbol;
        end;
    else
      begin
        FTokenID := tkSymbol;
      end;
    end;
  end else FTokenID := tkSymbol;
end;

procedure TSynGeneralSyn.SlashProc;
begin
  Inc(Run);
  case FLine[Run] of
    '/':
      begin
        if csCPPStyle in FComments then
        begin
          FTokenID := tkComment;
          Inc(Run);
          while FLine[Run] <> #0 do
          begin
            case FLine[Run] of
              #10, #13: Break;
            end;
            Inc(Run);
          end;
        end
        else
          FTokenID := tkSymbol;
      end;
    '*':
      begin
        if csCStyle in FComments then
        begin
          FTokenID := tkComment;
          FRange := rsCStyle;
          Inc(Run);
          while FLine[Run] <> #0 do
            case FLine[Run] of
              '*':
                if FLine[Run + 1] = '/' then
                begin
                  FRange := rsUnknown;
                  Inc(Run, 2);
                  Break;
                end else Inc(Run);
              #10, #13:
                Break;
              else
                Inc(Run);
            end;
        end
        else
          FTokenID := tkSymbol;
      end;
    else
      FTokenID := tkSymbol;
  end;
end;

procedure TSynGeneralSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynGeneralSyn.StringProc;
begin
  FTokenID := tkString;
  FRange := rsString;
  if IsStringDelim(FLine[Run + 1]) and IsStringDelim(FLine[Run + 2]) then
    Inc(Run, 2);

  // eventualy store the string delimiter
  if FStringDelimChar = #0 then
    FStringDelimChar := FLine[Run];

  Inc(Run);
  while not IsLineEnd(Run) do
  begin
    if FLine[Run] = FStringDelimChar then
    begin
      Inc(Run);
      if FLine[Run] <> FStringDelimChar then
      begin
        FRange := rsUnknown;
        FStringDelimChar := #0;
        Break;
      end;
    end;
    Inc(Run);
  end;

  if IsLineEnd(Run) and (not FStringMultiLine) then
  begin
    FRange := rsUnknown;
    FStringDelimChar := #0;
  end;
end;

procedure TSynGeneralSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynGeneralSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsAnsi: AnsiProc;
    rsPasStyle: PasStyleProc;
    rsCStyle: CStyleProc;
    rsString: StringProc;
  else
    if IsStringDelim(FLine[Run]) then
      StringProc
    else
      case FLine[Run] of
        '#':
          AsciiCharProc;
        '{':
          BraceOpenProc;
        ';':
          PointCommaProc;
        #13:
          CRProc;
        'A'..'Z', 'a'..'z', '_':
          IdentProc;
        '$':
          IntegerProc;
        #10:
          LFProc;
        #0:
          NullProc;
        '0'..'9':
          NumberProc;
        '(':
          RoundOpenProc;
        '/':
          SlashProc;
        #1..#9, #11, #12, #14..#32:
          SpaceProc;
        else
          UnknownProc;
      end;
  end;
  inherited;
end;

function TSynGeneralSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynGeneralSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynGeneralSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynGeneralSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

// GetCharBeforeToken
//
function TSynGeneralSyn.GetCharBeforeToken(offset: Integer = -1): WideChar;
begin
  if FTokenPos + offset >= 0 then
    Result := FLine[FTokenPos + offset]
  else
    Result := #0;
end;

// GetCharAfterToken
//
function TSynGeneralSyn.GetCharAfterToken(offset: Integer = 1): WideChar;
begin
  Result := FLine[FTokenPos + offset];
end;

function TSynGeneralSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkPreprocessor: Result := FPreprocessorAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FSymbolAttri;
  else
    Result := nil;
  end;
  if Assigned(FOnGetTokenAttribute) then
    FOnGetTokenAttribute(Result);
end;

function TSynGeneralSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynGeneralSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynGeneralSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynGeneralSyn.SetKeyWords(const Value: TUnicodeStrings);
var
  i: Integer;
begin
  if Value <> nil then
  begin
    Value.BeginUpdate;
    for i := 0 to Value.Count - 1 do
      Value[i] := SynWideUpperCase(Value[i]);
    Value.EndUpdate;
  end;

  TUnicodeStringList(FKeyWords).Sorted := False;
  FKeyWords.Assign(Value);
  TUnicodeStringList(FKeyWords).Sorted := True;

  DefHighLightChange(nil);
end;

procedure TSynGeneralSyn.SetComments(Value: TCommentStyles);
begin
  if FComments <> Value then
  begin
    FComments := Value;
    DefHighLightChange(Self);
  end;
end;

class function TSynGeneralSyn.GetLanguageName: string;
begin
  Result := SYNS_LangGeneral;
end;

function TSynGeneralSyn.LoadFromRegistry(RootKey: HKEY; Key: string): Boolean;
var
  r: TBetterRegistry;
begin
  r := TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKeyReadOnly(Key) then begin
      if r.ValueExists('KeyWords') then KeyWords.Text:= r.ReadString('KeyWords');
      Result := inherited LoadFromRegistry(RootKey, Key);
    end
    else
      Result := False;
  finally
    r.Free;
  end;
end;

function TSynGeneralSyn.SaveToRegistry(RootKey: HKEY; Key: string): Boolean;
var
  r: TBetterRegistry;
begin
  r := TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKey(Key,true) then begin
      {$IFNDEF SYN_COMPILER_25_UP}
      Result := true;
      {$ENDIF}
      r.WriteString('KeyWords', KeyWords.Text);
      Result := inherited SaveToRegistry(RootKey, Key);
    end
    else
      Result := False;
  finally
    r.Free;
  end;
end;

function TSynGeneralSyn.GetStringDelim: TStringDelim;
begin
  Result := FStringDelim;
end;

procedure TSynGeneralSyn.SetStringDelim(const Value: TStringDelim);
begin
  if FStringDelim <> Value then
  begin
    FStringDelim := Value;
    DefHighLightChange(Self);
  end;
end;

procedure TSynGeneralSyn.SetStringMultiLine(const Value: Boolean);
begin
  if FStringMultiLine <> Value then
  begin
    FStringMultiLine := Value;
    DefHighLightChange(Self);
  end;
end;

function TSynGeneralSyn.GetIdentifierChars: UnicodeString;
begin
  Result := FIdentChars;
end;

procedure TSynGeneralSyn.SetIdentifierChars(const Value: UnicodeString);
begin
  FIdentChars := Value;
end;

function TSynGeneralSyn.StoreIdentChars : Boolean;
begin
  Result := (FIdentChars <> cDefaultIdentChars);
end;

procedure TSynGeneralSyn.SetDetectPreprocessor(Value: Boolean);
begin
  if Value <> FDetectPreprocessor then
  begin
    FDetectPreprocessor := Value;
    DefHighlightChange(Self);
  end;
end;

class function TSynGeneralSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangGeneral;
end;

// IsStringDelim
//
function TSynGeneralSyn.IsStringDelim(aChar : WideChar): Boolean;
begin
  case FStringDelim of
    sdSingleQuote:
      Result := (aChar = '''');
    sdDoubleQuote:
      Result := (aChar = '"');
  else
    Result := (aChar = '''') or (aChar = '"');
  end;
end;

initialization
{$IFNDEF SYN_CPPB_1}                                                    
  RegisterPlaceableHighlighter(TSynGeneralSyn);
{$ENDIF}
end.
