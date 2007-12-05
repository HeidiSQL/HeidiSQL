{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPython.pas, released 2000-06-23.
The Original Code is based on the odPySyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Olivier Deckmyn.
Portions created by M.Utku Karatas and Dennis Chuah.
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

$Id: SynHighlighterPython.pas,v 1.18.2.6 2006/05/21 11:59:35 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(A Python language highlighter for SynEdit)
@author(Olivier Deckmyn, converted to SynEdit by David Muir <dhmn@dmsoftware.co.uk>)
@created(unknown, converted to SynEdit on 2000-06-23)
@lastmod(2003-02-13)
The SynHighlighterPython implements a highlighter for Python for the SynEdit projects.
}

{$IFNDEF QSYNHIGHLIGHTERPYTHON}
unit SynHighlighterPython;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditHighlighter,
  QSynEditTypes,
  QSynUnicode,  
{$ELSE}
  Graphics,
  SynEditHighlighter,
  SynEditTypes,
  SynUnicode,
{$ENDIF}
  SysUtils,
  Classes;

const
  ALPHA_CHARS = ['_', 'a'..'z', 'A'..'Z'];

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkNonKeyword, tkTrippleQuotedString,
    tkSystemDefined, tkHex, tkOct, tkFloat, tkUnknown);

  TRangeState = (rsANil, rsComment, rsUnKnown, rsMultilineString, rsMultilineString2,
                 rsMultilineString3 //this is to indicate if a string is made multiline by backslash char at line end (as in C++ highlighter)
                );

type
  TSynPythonSyn = class(TSynCustomHighLighter)
  private
    fStringStarter: WideChar;  // used only for rsMultilineString3 stuff
    fRange: TRangeState;
    FTokenID: TtkTokenKind;
    FKeywords: TWideStringList;
    fStringAttri: TSynHighlighterAttributes;
    fDocStringAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fHexAttri: TSynHighlighterAttributes;
    fOctalAttri: TSynHighlighterAttributes;
    fFloatAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNonKeyAttri: TSynHighlighterAttributes;
    fSystemAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fErrorAttri: TSynHighlighterAttributes;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure SymbolProc;
    procedure CRProc;
    procedure CommentProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SpaceProc;
    procedure PreStringProc;
    procedure UnicodeStringProc;
    procedure StringProc;
    procedure String2Proc;
    procedure StringEndProc(EndChar: WideChar);
    procedure UnknownProc;
  protected
    function GetSampleSource: WideString; override;
    function IsFilterStored: Boolean; override;
    function GetKeywordIdentifiers: TWideStringList;
    property Keywords: TWideStringList read FKeywords;
    property TokenID: TtkTokenKind read FTokenID;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: WideString; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    property NonKeyAttri: TSynHighlighterAttributes read fNonKeyAttri
      write fNonKeyAttri;
    property SystemAttri: TSynHighlighterAttributes read fSystemAttri
      write fSystemAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
    write fNumberAttri;
    property HexAttri: TSynHighlighterAttributes read fHexAttri
      write fHexAttri;
    property OctalAttri: TSynHighlighterAttributes read fOctalAttri
      write fOctalAttri;
    property FloatAttri: TSynHighlighterAttributes read fFloatAttri
      write fFloatAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
    write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
    write fStringAttri;
    property DocStringAttri: TSynHighlighterAttributes read fDocStringAttri
      write fDocStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
    write fSymbolAttri;
    property ErrorAttri: TSynHighlighterAttributes read fErrorAttri
      write fErrorAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

var
  GlobalKeywords: TWideStringList;

function TSynPythonSyn.GetKeywordIdentifiers: TWideStringList;
const
  // No need to localise keywords!

  // List of keywords
  KEYWORDCOUNT = 29;
  KEYWORDS: array [1..KEYWORDCOUNT] of WideString =
    (
    'and',
    'assert',
    'break',
    'class',
    'continue',
    'def',
    'del',
    'elif',
    'else',
    'except',
    'exec',
    'finally',
    'for',
    'from',
    'global',
    'if',
    'import',
    'in',
    'is',
    'lambda',
    'not',
    'or',
    'pass',
    'print',
    'raise',
    'return',
    'try',
    'while',
    'yield'
    );

  // List of non-keyword identifiers
  NONKEYWORDCOUNT = 66;
  NONKEYWORDS: array [1..NONKEYWORDCOUNT] of WideString =
    (
    '__future__',
    '__import__',
    'abs',
    'apply',
    'as',
    'buffer',
    'callable',
    'chr',
    'cmp',
    'coerce',
    'compile',
    'complex',
    'delattr',
    'dict',
    'dir',
    'divmod',
    'eval',
    'execfile',
    'False',
    'file',
    'filter',
    'float',
    'getattr',
    'globals',
    'hasattr',
    'hash',
    'help',
    'hex',
    'id',
    'input',
    'int',
    'intern',
    'isinstance',
    'issubclass',
    'iter',
    'len',
    'list',
    'locals',
    'long',
    'None',
    'NotImplemented',
    'map',
    'max',
    'min',
    'oct',
    'open',
    'ord',
    'pow',
    'range',
    'raw_input',
    'reduce',
    'reload',
    'repr',
    'round',
    'self',
    'setattr',
    'slice',
    'str',
    'True',
    'tuple',
    'type',
    'unichr',
    'unicode',
    'vars',
    'xrange',
    'zip'
    );
var
  f: Integer;
begin
  if not Assigned (GlobalKeywords) then
  begin
    // Create the string list of keywords - only once
    GlobalKeywords := TWideStringList.Create;

    for f := 1 to KEYWORDCOUNT do
      GlobalKeywords.AddObject(KEYWORDS[f], Pointer(Ord(tkKey)));
    for f := 1 to NONKEYWORDCOUNT do
      GlobalKeywords.AddObject(NONKEYWORDS[f], Pointer(Ord(tkNonKeyword)));
  end; // if
  Result := GlobalKeywords;
end;

function TSynPythonSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  i: Integer;
  temp: PWideChar;
  s: WideString;
begin
  // Extract the identifier out - it is assumed to terminate in a
  //   non-alphanumeric character
  fToIdent := MayBe;
  temp := MayBe;
  while IsIdentChar(temp^) do
    Inc(temp);
  fStringLen := temp - fToIdent;

  // Check to see if it is a keyword
  SetString(s, fToIdent, fStringLen);
  if FKeywords.Find(s, i) then
  begin
    // TWideStringList is not case sensitive!
    if s <> FKeywords[i] then
      i := -1;
  end
  else
    i := -1;

  if i <> -1 then
    Result := TtkTokenKind(FKeywords.Objects[i])

  // Check if it is a system identifier (__*__)
  else if (fStringLen >= 5) and
     (MayBe[0] = '_') and (MayBe[1] = '_') and (MayBe[2] <> '_') and
     (MayBe[fStringLen - 1] = '_') and (MayBe[fStringLen - 2] = '_') and
     (MayBe[fStringLen - 3] <> '_') then
    Result := tkSystemDefined

  // Else, hey, it is an ordinary run-of-the-mill identifier!
  else
    Result := tkIdentifier;
end;
  
constructor TSynPythonSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := True;

  FKeywords := TWideStringList.Create;
  FKeywords.Sorted := True; 
  FKeywords.Duplicates := dupError;
  FKeywords.Assign (GetKeywordIdentifiers);

  fRange := rsUnknown;
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Foreground := clGray;
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNonKeyAttri := TSynHighlighterAttributes.Create (SYNS_AttrNonReservedKeyword, SYNS_FriendlyAttrNonReservedKeyword);
  fNonKeyAttri.Foreground := clNavy;
  fNonKeyAttri.Style := [fsBold];
  AddAttribute (fNonKeyAttri);
  fSystemAttri := TSynHighlighterAttributes.Create (SYNS_AttrSystem, SYNS_FriendlyAttrSystem);
  fSystemAttri.Style := [fsBold];
  AddAttribute (fSystemAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  fNumberAttri.Foreground := clBlue;
  AddAttribute(fNumberAttri);
  fHexAttri := TSynHighlighterAttributes.Create(SYNS_AttrHexadecimal, SYNS_FriendlyAttrHexadecimal);
  fHexAttri.Foreground := clBlue;
  AddAttribute(fHexAttri);
  fOctalAttri := TSynHighlighterAttributes.Create(SYNS_AttrOctal, SYNS_FriendlyAttrOctal);
  fOctalAttri.Foreground := clBlue;
  AddAttribute(fOctalAttri);
  fFloatAttri := TSynHighlighterAttributes.Create(SYNS_AttrFloat, SYNS_FriendlyAttrFloat);
  fFloatAttri.Foreground := clBlue;
  AddAttribute(fFloatAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Foreground := clBlue;
  AddAttribute(fStringAttri);
  fDocStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrDocumentation, SYNS_FriendlyAttrDocumentation);
  fDocStringAttri.Foreground := clTeal;
  AddAttribute(fDocStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  fErrorAttri := TSynHighlighterAttributes.Create(SYNS_AttrSyntaxError, SYNS_FriendlyAttrSyntaxError);
  fErrorAttri.Foreground := clRed;
  AddAttribute(fErrorAttri);
  SetAttributesOnChange(DefHighlightChange);
  fDefaultFilter := SYNS_FilterPython;
end; { Create }

destructor TSynPythonSyn.Destroy;
begin
  FKeywords.Free;
  inherited;
end;

procedure TSynPythonSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynPythonSyn.CRProc;
begin
  fTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: inc(Run, 2);
  else
    inc(Run);
  end;
end;

procedure TSynPythonSyn.CommentProc;
begin
  fTokenID := tkComment;
  inc(Run);
  while not IsLineEnd(Run) do
    inc(Run);
end;

procedure TSynPythonSyn.GreaterProc;
begin
  case FLine[Run + 1] of
    '=': begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPythonSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
end;

procedure TSynPythonSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynPythonSyn.LowerProc;
begin
  case FLine[Run + 1] of
    '=': begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>': begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end
  else begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPythonSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynPythonSyn.NumberProc;
type
  TNumberState =
    (
    nsStart,
    nsDotFound,
    nsFloatNeeded,
    nsHex,
    nsOct,
    nsExpFound
    );

var
  temp: WideChar;
  State: TNumberState;

  function CheckSpecialCases: Boolean;
  begin
    case temp of
      // Look for dot (.)
      '.': begin
        // .45
        if FLine[Run] in [WideChar('0')..WideChar('9')] then
        begin
          Inc (Run);
          fTokenID := tkFloat;
          State := nsDotFound;

        // Non-number dot
        end else begin
          // Ellipsis
          if (FLine[Run] = '.') and (FLine[Run+1] = '.') then
            Inc (Run, 2);
          fTokenID := tkSymbol;
          Result := False;
          Exit;
        end; // if
      end; // DOT

      // Look for zero (0)
      '0': begin
        temp := FLine[Run];
        // 0x123ABC
        if temp in [WideChar('x'), WideChar('X')] then begin
          Inc (Run);
          fTokenID := tkHex;
          State := nsHex;
        // 0.45
        end else if temp = '.' then begin
          Inc (Run);
          State := nsDotFound;
          fTokenID := tkFloat;
        end else if temp in [WideChar('0')..WideChar('9')] then begin
          Inc (Run);
          // 0123 or 0123.45
          if temp in [WideChar('0')..WideChar('7')] then begin
            fTokenID := tkOct;
            State := nsOct;
          // 0899.45
          end else begin
            fTokenID := tkFloat;
            State := nsFloatNeeded;
          end; // if
        end; // if
      end; // ZERO
    end; // case

    Result := True;
  end; // CheckSpecialCases

  function HandleBadNumber: Boolean;
  begin
    Result := False;
    fTokenID := tkUnknown;
    // Ignore all tokens till end of "number"
    while IsIdentChar(FLine[Run]) or (FLine[Run] = '.') do
      Inc (Run);
  end; // HandleBadNumber

  function HandleExponent: Boolean;
  begin
    State := nsExpFound;
    fTokenID := tkFloat;
    // Skip e[+/-]
    if FLine[Run+1] in [WideChar('+'), WideChar('-')] then
      Inc (Run);
    // Invalid token : 1.0e
    if not (FLine[Run+1] in [WideChar('0')..WideChar('9')]) then begin
      Inc (Run);
      Result := HandleBadNumber;
      Exit;
    end; // if

    Result := True;
  end; // HandleExponent

  function HandleDot: Boolean;
  begin
    // Check for ellipsis
    Result := (FLine[Run+1] <> '.') or (FLine[Run+2] <> '.');
    if Result then begin
      State := nsDotFound;
      fTokenID := tkFloat;
    end; // if
  end; // HandleDot

  function CheckStart: Boolean;
  begin
    // 1234
    if temp in [WideChar('0')..WideChar('9')] then begin
      Result := True;
    //123e4
    end else if temp in [WideChar('e'), WideChar('E')] then begin
      Result := HandleExponent;
    // 123.45j
    end else if temp in [WideChar('j'), WideChar('J')] then begin
      Inc (Run);
      fTokenID := tkFloat;
      Result := False;
    // 123.45
    end else if temp = '.' then begin
      Result := HandleDot;
    // Error!
    end else if IsIdentChar(temp) then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckStart

  function CheckDotFound: Boolean;
  begin
    // 1.0e4
    if temp in [WideChar('e'), WideChar('E')] then begin
      Result := HandleExponent;
    // 123.45
    end else if temp in [WideChar('0')..WideChar('9')] then begin
      Result := True;
    // 123.45j
    end else if temp in [WideChar('j'), WideChar('J')] then begin
      Inc (Run);
      Result := False;
    // 123.45.45: Error!
    end else if temp = '.' then begin
      Result := False;
      if HandleDot then
        HandleBadNumber;
    // Error!
    end else if IsIdentChar(temp) then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckDotFound

  function CheckFloatNeeded: Boolean;
  begin
    // 091.0e4
    if temp in [WideChar('e'), WideChar('E')] then begin
      Result := HandleExponent;
    // 0912345
    end else if temp in [WideChar('0')..WideChar('9')] then begin
      Result := True;
    // 09123.45
    end else if temp = '.' then begin
      Result := HandleDot or HandleBadNumber; // Bad octal
    // 09123.45j
    end else if temp in [WideChar('j'), WideChar('J')] then begin
      Inc (Run);
      Result := False;
    // End of number (error: Bad oct number) 0912345
    end else begin
      Result := HandleBadNumber;
    end;
  end; // CheckFloatNeeded

  function CheckHex: Boolean;
  begin
    // 0x123ABC
    if temp in [WideChar('a')..WideChar('f'), WideChar('A')..WideChar('F'),
      WideChar('0')..WideChar('9')] then
    begin
      Result := True;
    // 0x123ABCL
    end else if temp in [WideChar('l'), WideChar('L')] then begin
      Inc (Run);
      Result := False;
    // 0x123.45: Error!
    end else if temp = '.' then begin
      Result := False;
      if HandleDot then
        HandleBadNumber;
    // Error!
    end else if IsIdentChar(temp) then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckHex

  function CheckOct: Boolean;
  begin
    // 012345
    if temp in [WideChar('0')..WideChar('9')] then begin
      if not (temp in [WideChar('0')..WideChar('7')]) then begin
        State := nsFloatNeeded;
        fTokenID := tkFloat;
      end; // if
      Result := True;
    // 012345L
    end else if temp in [WideChar('l'), WideChar('L')] then begin
      Inc (Run);
      Result := False;
    // 0123e4
    end else if temp in [WideChar('e'), WideChar('E')] then begin
      Result := HandleExponent;
    // 0123j
    end else if temp in [WideChar('j'), WideChar('J')] then begin
      Inc (Run);
      fTokenID := tkFloat;
      Result := False;
    // 0123.45
    end else if temp = '.' then begin
      Result := HandleDot;
    // Error!
    end else if IsIdentChar(temp) then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckOct

  function CheckExpFound: Boolean;
  begin
    // 1e+123
    if temp in [WideChar('0')..WideChar('9')] then begin
      Result := True;
    // 1e+123j
    end else if temp in [WideChar('j'), WideChar('J')] then begin
      Inc (Run);
      Result := False;
    // 1e4.5: Error!
    end else if temp = '.' then begin
      Result := False;
      if HandleDot then
        HandleBadNumber;
    // Error!
    end else if IsIdentChar(temp) then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckExpFound

begin
  State := nsStart;
  fTokenID := tkNumber;

  temp := FLine[Run];
  Inc (Run);

  // Special cases
  if not CheckSpecialCases then
    Exit;

  // Use a state machine to parse numbers
  while True do begin
    temp := FLine[Run];

    case State of
      nsStart:
        if not CheckStart then Exit;
      nsDotFound:
        if not CheckDotFound then Exit;
      nsFloatNeeded:
        if not CheckFloatNeeded then Exit;
      nsHex:
        if not CheckHex then Exit;
      nsOct:
        if not CheckOct then Exit;
      nsExpFound:
        if not CheckExpFound then Exit;
    end; // case

    Inc (Run);
  end; // while
end;

procedure TSynPythonSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynPythonSyn.String2Proc;
var
  fBackslashCount: Integer;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = '"') and (FLine[Run + 2] = '"') then
  begin
    fTokenID := tkTrippleQuotedString;
    inc(Run, 3);

    fRange := rsMultilineString2;
    while fLine[Run] <> #0 do
    begin
      case fLine[Run] of

        '\':begin
               { If we're looking at a backslash, and the following character is an
               end quote, and it's preceeded by an odd number of backslashes, then
               it shouldn't mark the end of the string.  If it's preceeded by an
               even number, then it should. !!!THIS RULE DOESNT APPLY IN RAW STRINGS}
               if FLine[Run + 1] = '"' then
                 begin
                   fBackslashCount := 1;

                   while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
                     fBackslashCount := fBackslashCount + 1;

                   if (fBackslashCount mod 2 = 1) then inc(Run)
               end;
               inc(Run);
            end;// '\':

        '"':
          if (fLine[Run + 1] = '"') and (fLine[Run + 2] = '"') then begin
            fRange := rsUnKnown;
            inc(Run, 3);
            exit;
          end else
            inc(Run);
        #10: exit;
        #13: exit;
        else
          inc(Run);
      end;
    end;
  end
      else //if short string
  repeat
    case FLine[Run] of
      #0, #10, #13:
        begin
          if FLine[Run-1] = '\' then
          begin
            fStringStarter := '"';
            fRange := rsMultilineString3;
          end;
          Break;
        end;
      {The same backslash stuff above...}
      '\':begin
             if FLine[Run + 1] = '"' then
               begin
                 fBackslashCount := 1;

                 while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
                   fBackslashCount := fBackslashCount + 1;

                 if (fBackslashCount mod 2 = 1) then inc(Run)
             end;
             inc(Run);
          end;// '\':

      else inc(Run);
    end; //case
  until (FLine[Run] = '"');
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynPythonSyn.PreStringProc;
var
  temp: WideChar;
begin
  // Handle python raw strings
  // r""
  temp := FLine[Run + 1];
  if temp = '''' then
  begin
    Inc (Run);
    StringProc;
  end
  else if temp = '"' then
  begin
    Inc (Run);
    String2Proc;
  end
  else
  begin
    // If not followed by quote char, must be ident
    IdentProc;
  end; // if
end;

procedure TSynPythonSyn.UnicodeStringProc;
begin
  // Handle python raw and unicode strings
  // Valid syntax: u"", or ur""
  if (FLine[Run + 1] in [WideChar('r'), WideChar('R')]) and
    (FLine[Run + 2] in [WideChar(''''), WideChar('"')]) then
  begin
    // for ur, Remove the "u" and...
    Inc (Run);
  end;
  // delegate to raw strings
  PreStringProc;
end;

procedure TSynPythonSyn.StringProc;
var
  fBackslashCount: Integer;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then begin
    fTokenID := tkTrippleQuotedString;
    inc(Run, 3);

    fRange:=rsMultilineString;
    while fLine[Run] <> #0 do begin
      case fLine[Run] of

        '\': begin
             { If we're looking at a backslash, and the following character is an
             end quote, and it's preceeded by an odd number of backslashes, then
             it shouldn't mark the end of the string.  If it's preceeded by an
             even number, then it should. !!!THIS RULE DOESNT APPLY IN RAW STRINGS}
              if FLine[Run + 1] = #39 then
                begin
                  fBackslashCount := 1;

                  while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
                    fBackslashCount := fBackslashCount + 1;

                  if (fBackslashCount mod 2 = 1) then inc(Run)
              end;
              inc(Run);
            end;// '\':

        #39:
          if (fLine[Run + 1] = #39) and (fLine[Run + 2] = #39) then begin
            fRange := rsUnKnown;
            inc(Run, 3);
            EXIT;
          end else
            inc(Run);
        #10: EXIT;
        #13: EXIT;
        else
          inc(Run);
      end;
    end;
  end
      else //if short string
  repeat
    case FLine[Run] of
      #0, #10, #13 : begin
        if FLine[Run-1] = '\' then begin
          fStringStarter := #39;
          fRange := rsMultilineString3;
        end;
        BREAK;
        end;

      {The same backslash stuff above...}
      '\':begin
             if FLine[Run + 1] = #39 then
               begin
                 fBackslashCount := 1;

                 while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
                   fBackslashCount := fBackslashCount + 1;

                 if (fBackslashCount mod 2 = 1) then inc(Run)
             end;
             inc(Run);
          end;// '\':

      else inc(Run);
    end; //case
  until (FLine[Run] = #39);
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynPythonSyn.StringEndProc(EndChar: WideChar);
var
  fBackslashCount: Integer;
begin
  if fRange = rsMultilineString3 then
    fTokenID := tkString
  else
    fTokenID := tkTrippleQuotedString;

  case FLine[Run] of
    #0:
      begin
        NullProc;
        EXIT;
      end;
    #10:
      begin
        LFProc;
        EXIT;
    end;
    #13:
      begin
        CRProc;
        EXIT;
      end;
  end;

  if fRange = rsMultilineString3 then begin
    repeat
      if FLine[Run]=fStringStarter then begin
        inc(Run);
        fRange:=rsUnknown;
        EXIT;
      end else if FLine[Run]='\' then ;  {The same backslash stuff above...}
          begin
             if FLine[Run + 1] = fStringStarter then
               begin
                 fBackslashCount := 1;

                 while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
                   fBackslashCount := fBackslashCount + 1;

                 if (fBackslashCount mod 2 = 1) then inc(Run);
             end;
           end;// if FLine[Run]...

      inc(Run);
    until IsLineEnd(Run);
    if FLine[Run-1]<>'\' then begin
      fRange:=rsUnknown;
      EXIT;
    end;
  end else
  repeat
    if FLine[Run] = '\' then
    begin
       if FLine[Run + 1] = EndChar then
         begin
           fBackslashCount := 1;

           while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
             fBackslashCount := fBackslashCount + 1;

           if (fBackslashCount mod 2 = 1) then inc(Run, 2);
       end;
     end;// if FLine[Run]...
    if (FLine[Run]=EndChar) and (FLine[Run+1]=EndChar) and (FLine[Run+2]=EndChar) then begin
      inc(Run,3);
      fRange:=rsUnknown;
      EXIT;
    end;
    inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynPythonSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynPythonSyn.Next;
begin
  fTokenPos := Run;

  case fRange of
    rsMultilineString:
      StringEndProc(#39);
    rsMultilineString2:
      StringEndProc('"');
    rsMultilineString3:
      StringEndProc(fStringStarter);
    else
      case fLine[Run] of
        '&', '}', '{', ':', ',', ']', '[', '*', '`',
        '^', ')', '(', ';', '/', '=', '-', '+', '!', '\',
        '%', '|', '~' :
          SymbolProc;
        #13: CRProc;
        '#': CommentProc;
        '>': GreaterProc;
        'A'..'Q', 'S', 'T', 'V'..'Z', 'a'..'q', 's', 't', 'v'..'z', '_': IdentProc;
        #10: LFProc;
        '<': LowerProc;
        #0: NullProc;
        '.', '0'..'9': NumberProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        'r', 'R': PreStringProc;
        'u', 'U': UnicodeStringProc;
        '''': StringProc;
        '"': String2Proc;
        else UnknownProc;
      end;
  end;
  inherited;
end;

function TSynPythonSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynPythonSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynPythonSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynPythonSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynPythonSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNonKeyword: Result := fNonKeyAttri;
    tkSystemDefined: Result := fSystemAttri;
    tkNumber: Result := fNumberAttri;
    tkHex: Result := fHexAttri;
    tkOct: Result := fOctalAttri;
    tkFloat: Result := fFloatAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkTrippleQuotedString: Result := fDocStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fErrorAttri;
  else
    Result := nil;
  end;
end;

function TSynPythonSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

procedure TSynPythonSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynPythonSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynPythonSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterPython;
end;

class function TSynPythonSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPython;
end;

function TSynPythonSyn.GetSampleSource: WideString;
begin
  Result :=
    '#!/usr/local/bin/python'#13#10 +
    'import string, sys'#13#10 +
    '""" If no arguments were given, print a helpful message """'#13#10 +
    'if len(sys.argv)==1:'#13#10 +
    '    print ''Usage: celsius temp1 temp2 ...'''#13#10 +
    '    sys.exit(0)';
end;

class function TSynPythonSyn.GetFriendlyLanguageName: WideString;
begin
  Result := SYNS_FriendlyLangPython;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynPythonSyn);
{$ENDIF}
finalization
  GlobalKeywords.Free;
end.
