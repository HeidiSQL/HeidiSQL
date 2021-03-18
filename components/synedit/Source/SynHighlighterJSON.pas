{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterJSON.pas, released 2015-01-14.
The Initial Author of this file is Christian-W. Budde.
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

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net
}

unit SynHighlighterJSON;

{$I SynEdit.inc}

interface

uses
  Graphics,
  Registry,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
  SysUtils, Classes;

type
  TtkTokenKind = (tkString, tkReserved, tkNull, tkNumber, tkSpace,
    tkSymbol, tkUnknown);

  TRangeState = (rsUnknown, rsAttribute, rsObjectValue, rsArrayValue);

type
  TSynJSONSyn = class(TSynCustomHighLighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FReservedAttri: TSynHighlighterAttributes;
    FAttributeAttri: TSynHighlighterAttributes;
    FValueAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    procedure CloseArrayProc;
    procedure CloseObjectProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure CRProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OpenArrayProc;
    procedure OpenObjectProc;
    procedure ReservedWordProc;
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
    property AttributeAttri: TSynHighlighterAttributes read FAttributeAttri
      write FAttributeAttri;
    property ReservedAttri: TSynHighlighterAttributes read FReservedAttri
      write FReservedAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
    property ValueAttri: TSynHighlighterAttributes read FValueAttri
      write FValueAttri;
  end;

implementation

uses
  SynEditStrConst;


{ TSynJSONSyn }

constructor TSynJSONSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

  // Attribute
  FAttributeAttri := TSynHighlighterAttributes.Create(SYNS_AttrAttribute,
    SYNS_FriendlyAttrAttribute);
  FAttributeAttri.Foreground := clNavy;
  AddAttribute(FAttributeAttri);

  // reserved words ("true", "false", "null")
  FReservedAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord,
    SYNS_FriendlyAttrReservedWord);
  FReservedAttri.Style := [fsBold];
  AddAttribute(FReservedAttri);

  // numbers
  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber,
    SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clRed;
  AddAttribute(FNumberAttri);

  // spaces
  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace,
    SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  // symbols
  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol,
    SYNS_FriendlyAttrSymbol);
  FSymbolAttri.Foreground := clGreen;
  AddAttribute(FSymbolAttri);

  // Value
  FValueAttri := TSynHighlighterAttributes.Create(SYNS_AttrValue,
    SYNS_FriendlyAttrValue);
  FValueAttri.Foreground := clBlue;
  AddAttribute(FValueAttri);

  SetAttributesOnChange(DefHighlightChange);
  FDefaultFilter := SYNS_FilterJSON;
  FRange := rsUnknown;
end;

procedure TSynJSONSyn.CloseArrayProc;
begin
  SymbolProc;
  FRange := rsUnknown;
end;

procedure TSynJSONSyn.CloseObjectProc;
begin
  SymbolProc;
  FRange := rsUnknown;
end;

procedure TSynJSONSyn.ColonProc;
begin
  SymbolProc;
  FRange := rsObjectValue;
end;

procedure TSynJSONSyn.CommaProc;
begin
  SymbolProc;
  if FRange = rsObjectValue then
    FRange := rsAttribute;
end;

procedure TSynJSONSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then Inc(Run);
end;

procedure TSynJSONSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynJSONSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynJSONSyn.NumberProc;

  function ExpectDigit: Boolean;
  begin
    Result := CharInSet(FLine[Run], ['0'..'9']);
    while CharInSet(FLine[Run], ['0'..'9']) do
      Inc(Run);
  end;

begin
  FTokenID := tkNumber;

  if FLine[Run] = '-' then
    Inc(Run);

  // ensure that a zero is followed by a dot
  if FLine[Run] = '0' then
    if FLine[Run + 1] <> '.' then
    begin
      FTokenID := tkUnknown;
      while (FLine[Run] <> #32) and not IsLineEnd(Run) do Inc(Run);
      Exit;
    end;

  // at least any digit must appear here
  if not ExpectDigit then
  begin
    FTokenID := tkUnknown;
    while (FLine[Run] <> #32) and not IsLineEnd(Run) do Inc(Run);
    Exit;
  end;

  // check for dot
  if FLine[Run] = '.' then
  begin
    // advance
    Inc(Run);

    // at least any digit must appear after a dot!
    if not ExpectDigit then
    begin
      FTokenID := tkUnknown;
      while (FLine[Run] <> #32) and not IsLineEnd(Run) do Inc(Run);
      Exit;
    end;
  end;

  // check for an exponent
  if CharInSet(FLine[Run], ['e', 'E']) then
  begin
    Inc(Run);

    // allow +/- here
    if CharInSet(FLine[Run], ['+', '-']) then
      Inc(Run);

    // at least any digit must appear here
    if not ExpectDigit then
    begin
      FTokenID := tkUnknown;
      while (FLine[Run] <> #32) and not IsLineEnd(Run) do Inc(Run);
      Exit;
    end;
  end;
end;

procedure TSynJSONSyn.OpenArrayProc;
begin
  SymbolProc;
  FRange := rsArrayValue;
end;

procedure TSynJSONSyn.OpenObjectProc;
begin
  SymbolProc;
  FRange := rsAttribute;
end;

procedure TSynJSONSyn.ReservedWordProc;

  procedure SkipToken;
  begin
    while (FLine[Run] <> #32) and (FLine[Run] <> ',') and not IsLineEnd(Run) do
      Inc(Run);
  end;

begin
  FTokenID := tkUnknown;
  case FLine[Run] of
    'n':
      if (FLine[Run + 1] = 'u') and
         (FLine[Run + 2] = 'l') and
         (FLine[Run + 3] = 'l') then
      begin
        FTokenID := tkReserved;
        Inc(Run, 4);
      end
      else
        SkipToken;
    't':
      if (FLine[Run + 1] = 'r') and
         (FLine[Run + 2] = 'u') and
         (FLine[Run + 3] = 'e') then
      begin
        FTokenID := tkReserved;
        Inc(Run, 4);
      end
      else
        SkipToken;
    'f':
      if (FLine[Run + 1] = 'a') and
         (FLine[Run + 2] = 'l') and
         (FLine[Run + 3] = 's') and
         (FLine[Run + 4] = 'e') then
      begin
        FTokenID := tkReserved;
        Inc(Run, 5);
      end
      else
        SkipToken;
    else
      SkipToken;
  end;
end;

procedure TSynJSONSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynJSONSyn.StringProc;

  function IsHex(Digit: AnsiChar): Boolean; overload;
  begin 
    Result := CharInSet(Digit, ['0'..'9', 'A'..'F', 'a'..'f']); 
  end; 

  function IsHex(Digit: WideChar): Boolean; overload;
  begin
    Result := CharInSet(Digit, ['0'..'9', 'A'..'F', 'a'..'f']);
  end;

begin
  FTokenID := tkString;

  repeat
    Inc(Run);
    case FLine[Run] of
      '"':
        begin
          Inc(Run);
          Break;
        end;
      '\':
        case FLine[Run + 1] of
          '"', '/', '\', 'b', 'f', 'n', 'r', 't':
            Inc(Run);
          'u':
            begin
              Inc(Run);
              if not (IsHex(FLine[Run + 1]) and IsHex(FLine[Run + 2]) and
                IsHex(FLine[Run + 3]) and IsHex(FLine[Run + 4])) then
              begin
                // a 4 hex digit is expected
                FTokenID := tkUnknown;
                while not CharInSet(FLine[Run], [#32, '"']) and not IsLineEnd(Run) do
                  Inc(Run);
                Exit;
              end;
              Inc(Run, 4);
            end;
        end;
    end;
  until IsLineEnd(Run);
end;

procedure TSynJSONSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynJSONSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynJSONSyn.Next;
begin
  FTokenPos := Run;
  case FLine[Run] of
    #0: NullProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    #10: LFProc;
    #13: CRProc;
    '0'..'9', '-': NumberProc;
    't',
    'f',
    'n' : ReservedWordProc;
    '"': StringProc;
    ':': ColonProc;
    '{': OpenObjectProc;
    '[': OpenArrayProc;
    '}': CloseObjectProc;
    ']': CloseArrayProc;
    ',' : CommaProc;
    else UnknownProc;
  end;

  inherited;
end;

function TSynJSONSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_KEYWORD: Result := FReservedAttri;
    SYN_ATTR_IDENTIFIER: Result := FAttributeAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
    SYN_ATTR_STRING: Result := FValueAttri;
  else
    Result := nil;
  end;
end;

function TSynJSONSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynJSONSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynJSONSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynJSONSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkString:
      if FRange in [rsObjectValue, rsArrayValue] then
        Result := FValueAttri
      else
        Result := FAttributeAttri;
    tkReserved: Result := FReservedAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FAttributeAttri;
    else Result := nil;
  end;
end;

function TSynJSONSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynJSONSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynJSONSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynJSONSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterJSON;
end;

class function TSynJSONSyn.GetLanguageName: string;
begin
  Result := SYNS_LangJSON;
end;

function TSynJSONSyn.GetSampleSource: UnicodeString;
begin
  Result :=
    '{'#13#10 +
    '  "firstName": "John",'#13#10 +
    '  "lastName": "Smith",'#13#10 +
    '  "isAlive": true,'#13#10 +
    '  "age": 25,'#13#10 +
    '  "height_cm": 167.6,'#13#10 +
    '  "address": {'#13#10 +
    '    "streetAddress": "21 2nd Street",'#13#10 +
    '    "city": "New York",'#13#10 +
    '    "state": "NY",'#13#10 +
    '    "postalCode": "10021-3100"'#13#10 +
    '  },'#13#10 +
    '  "phoneNumbers": ['#13#10 +
    '    {'#13#10 +
    '      "type": "home",'#13#10 +
    '      "number": "212 555-1234"'#13#10 +
    '    },'#13#10 +
    '    {'#13#10 +
    '      "type": "office",'#13#10 +
    '      "number": "646 555-4567"'#13#10 +
    '    }'#13#10 +
    '  ],'#13#10 +
    '  "face": "\uD83D\uDE02",'#13#10 +
    '  "children": [],'#13#10 +
    '  "spouse": null'#13#10 +
    '}';
end;

class function TSynJSONSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangJSON;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynJSONSyn);
{$ENDIF}
end.
