{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPHP.pas, released 2000-04-21.
The Original Code is based on the wmPHPSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Willo van der Merwe.
"Heredoc" syntax highlighting implementation by Marko Njezic.
Unicode translation by Ma?l H?rz.
PHP5 keywords added by CodehunterWorks.
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

$Id: SynHighlighterPHP.pas,v 1.22.3.0 2012/09/11 16:25:00 codehunterworks Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a PHP syntax highlighter for SynEdit)
@author(Willo van der Merwe <willo@wack.co.za>, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(1999, converted to SynEdit 2000-04-21)
@lastmod(2000-06-23)
The SynHighlighterPHP unit provides SynEdit with a PHP syntax highlighter.
Thanks to Martin Waldenburg.
}

unit SynHighlighterPHP;

{$I SynEdit.inc}

interface

uses
  Graphics,
  Registry,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull,
    tkNumber, tkSpace, tkString, tkSymbol, tkUnknown, tkVariable);

{$IFDEF SYN_HEREDOC}
  TRangeState = (rsUnknown, rsString39, rsString34, rsComment, rsVarExpansion,
    rsHeredoc);

  TRangePointer = packed record
    case Boolean of
      True: (Ptr: Pointer);
      False: (Range: Byte; Length: Byte; Checksum: Word);
    end;
{$ELSE}
  TRangeState = (rsUnknown, rsString39, rsString34, rsComment, rsVarExpansion);
{$ENDIF}

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynPHPSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
{$IFDEF SYN_HEREDOC}
    FHeredocLength: Byte;
    FHeredocChecksum: Word;
{$ENDIF}
    FTokenID: TtkTokenKind;
    FIdentFuncTable: array[0..255] of TIdentFuncTableFunc;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FVariableAttri: TSynHighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure AndSymbolProc;
    procedure AtSymbolProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure MultiplyProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure PoundProc;
    procedure QuestionProc;
    procedure RemainderSymbolProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StringProc;
    procedure VarExpansionProc;
    procedure TildeProc;
    procedure VariableProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
    procedure AnsiCProc;
    procedure String39Proc;
    procedure String34Proc;
{$IFDEF SYN_HEREDOC}
    procedure HeredocProc;
{$ENDIF}
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
    procedure NextProcedure;
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
    property VariableAttri: TSynHighlighterAttributes read FVariableAttri
      write FVariableAttri;
  end;

implementation

uses
  SynEditMiscProcs,
  SynEditStrConst;

const
  KeyWords: array[0..73] of UnicodeString = (
    '__class__', '__dir__', '__file__', '__function__', '__halt_compiler',
    '__line__', '__method__', '__namespace__', 'abstract', 'and', 'array', 'as',
    'break', 'case', 'catch', 'class', 'clone', 'const', 'continue', 'declare',
    'default', 'die', 'do', 'echo', 'else', 'elseif', 'empty', 'enddeclare',
    'endfor', 'endforeach', 'endif', 'endswitch', 'endwhile', 'eval', 'exit',
    'extends', 'false', 'final', 'for', 'foreach', 'function', 'global', 'goto',
    'if', 'implements', 'include', 'include_once', 'instanceof', 'interface',
    'isset', 'list', 'namespace', 'new', 'null', 'old_function', 'or', 'print',
    'private', 'protected', 'public', 'require', 'require_once', 'return',
    'static', 'switch', 'synedit', 'throw', 'true', 'try', 'unset', 'use',
    'var', 'while', 'xor'
  );

  KeyIndices: array[0..222] of Integer = (
    -1, -1, 69, -1, -1, 1, 19, -1, -1, -1, -1, -1, -1, 35, -1, 17, -1, -1, 53,
    6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 46, -1, -1, -1,
    52, 36, -1, -1, 66, -1, 62, -1, 38, 15, 44, -1, -1, -1, -1, 32, -1, -1, 24,
    48, -1, -1, 56, 45, 65, 40, -1, -1, -1, -1, -1, -1, -1, 67, -1, -1, -1, -1,
    -1, 60, -1, -1, -1, -1, -1, 31, 11, -1, 33, 20, 49, -1, -1, -1, 21, -1, -1,
    -1, 54, -1, -1, -1, -1, -1, 29, -1, 64, -1, 23, -1, -1, 14, -1, -1, 42, -1,
    -1, 0, 25, 50, -1, 58, 4, 27, -1, -1, 7, -1, -1, -1, -1, -1, 63, -1, 34, -1,
    -1, -1, -1, -1, -1, -1, -1, 28, 13, 47, 51, -1, -1, 2, -1, 37, -1, -1, 71,
    3, -1, 30, -1, 43, -1, -1, -1, -1, 57, 8, -1, -1, -1, -1, 41, 10, -1, 12,
    72, -1, -1, -1, -1, -1, -1, 73, -1, -1, -1, -1, 5, -1, 22, -1, -1, -1, 70,
    9, 18, -1, -1, -1, -1, -1, 59, 26, -1, -1, 16, -1, 68, -1, 61, -1, -1, -1,
    39, -1, -1, -1, -1, -1, -1, -1, -1, 55, -1, -1, -1
  );

{$Q-}
function TSynPHPSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 252 + Ord(Str^) * 595;
    Inc(Str);
  end;
  Result := Result mod 223;
  FStringLen := Str - FToIdent;
end;{$Q+}

function TSynPHPSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Key: Cardinal;
begin
  FToIdent := MayBe;
  Key := HashKey(MayBe);
  if Key <= High(FIdentFuncTable) then
    Result := FIdentFuncTable[Key](KeyIndices[Key])
  else
    Result := tkIdentifier;
end;

procedure TSynPHPSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = -1 then
      FIdentFuncTable[i] := AltFunc;

  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if @FIdentFuncTable[i] = nil then
      FIdentFuncTable[i] := KeyWordFunc;
end;

function TSynPHPSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynPHPSyn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then begin
    Result := tkKey;
  end else begin
    Result := tkIdentifier;
  end;
end;

constructor TSynPHPSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

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
  FVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  AddAttribute(FVariableAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  FDefaultFilter := SYNS_FilterPHP;
  FRange := rsUnknown;
end;

procedure TSynPHPSyn.AndSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {and assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
    '&':                               {conditional and}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {and}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.AtSymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynPHPSyn.BraceCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynPHPSyn.BraceOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynPHPSyn.CRProc;
begin
  FTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: Inc(Run, 2);
  else
    Inc(Run);
  end;
end;

procedure TSynPHPSyn.ColonProc;
begin
  Inc(Run);                            {colon - conditional}
  FTokenID := tkSymbol;
end;

procedure TSynPHPSyn.CommaProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynPHPSyn.EqualProc;
begin
  case FLine[Run + 1] of
    '=':                               {logical equal}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
    '>':                               {Hash operator}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {assign}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.GreaterProc;
begin
  case FLine[Run + 1] of
    '=':                               {greater than or equal to}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
    '>':
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {greater than}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + Run));
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do Inc(Run);
end;

procedure TSynPHPSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynPHPSyn.LowerProc;
{$IFDEF SYN_HEREDOC}
var
  i, Len : Integer;
{$ENDIF}
begin
  case FLine[Run + 1] of
    '=':                               {less than or equal to}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
    '<':
      begin
        FTokenID := tkSymbol;
{$IFDEF SYN_HEREDOC}
        if (FLine[Run + 2] = '<') and IsIdentChar(FLine[Run + 3]) then
        begin
          Inc(Run, 3);

          i := Run;
          while IsIdentChar(FLine[i]) do Inc(i);
          Len := i - Run;

          if Len > 255 then
          begin
            FTokenID := tkUnknown;
            Exit;
          end;

          FRange := rsHeredoc;
          FHeredocLength := Len;
          FHeredocChecksum := CalcFCS(FLine[Run], Len);

          Inc(Run, Len);
          FTokenID := tkString;
        end
        else
{$ENDIF}
        if FLine[Run + 2] = '=' then   {shift left assign}
        begin
          Inc(Run, 3)
        end
        else                           {shift left}
        begin
          Inc(Run, 2);
        end;
      end;
  else                                 {less than}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.MinusProc;
begin
  case FLine[Run + 1] of
    '=':                               {subtract assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
    '-':                               {decrement}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
    '>':                               {Class operator}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {subtract}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.MultiplyProc;
begin
  case FLine[Run + 1] of
    '=':                               {multiply assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {multiply}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.NotSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {not equal}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {logical complement}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynPHPSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', '.', '-', 'l', 'L', 'x', 'X', 'A'..'F', 'a'..'f':
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
      '.':
        if FLine[Run + 1] = '.' then
          Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynPHPSyn.OrSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {inclusive or assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
    '|':                               {conditional or}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {inclusive or}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.PlusProc;
begin
  case FLine[Run + 1] of
    '=':                               {add assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
    '+':                               {increment}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {add}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.PointProc;
begin
  Inc(Run);                            {point}
  FTokenID := tkSymbol;
end;

procedure TSynPHPSyn.PoundProc;
begin
  repeat
    Inc(Run);
  until IsLineEnd(Run);
  FTokenID := tkComment;
end;

procedure TSynPHPSyn.QuestionProc;
begin
  FTokenID := tkSymbol;                {question mark - conditional}
  Inc(Run);
end;

procedure TSynPHPSyn.RemainderSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {remainder assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {remainder}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.RoundCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynPHPSyn.RoundOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynPHPSyn.SemiColonProc;
begin
  Inc(Run);                            {semicolon}
  FTokenID := tkSymbol;
end;

procedure TSynPHPSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '/':                               {c++ style comments}
      begin
        Inc(Run, 2);
        FTokenID := tkComment;
        while not IsLineEnd(Run) do
          Inc(Run);
      end;
    '*':
      begin
        FRange := rsComment;
        Inc(Run);
        FTokenID := tkComment;       {c style comment}

        Inc(Run);
        while not IsLineEnd(Run) do
          if FLine[Run] = '*' then
          begin
            if FLine[Run + 1] = '/' then
            begin
              FRange := rsUnknown;
              Inc(Run, 2);
              Break;
            end
            else
              Inc(Run)
          end
          else
            Inc(Run);
      end;
    '=':                               {division assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {division}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynPHPSyn.SquareCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynPHPSyn.SquareOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynPHPSyn.StringProc;

  function IsEscaped: Boolean;
  var
    iFirstSlashPos: Integer;
  begin
    iFirstSlashPos := Run -1;
    while (iFirstSlashPos > 0) and (FLine[iFirstSlashPos] = '\') do
      Dec(iFirstSlashPos);
    Result := (Run - iFirstSlashPos + 1) mod 2 <> 0;
  end;

var
  iCloseChar: WideChar;
begin
  if IsLineEnd(Run) and (FTokenPos = Run) then
  begin
    NextProcedure;
    Exit;
  end;
  FTokenID := tkString;
  if FRange = rsString39 then
    iCloseChar := #39
  else
    iCloseChar := #34;
  while not IsLineEnd(Run) do
  begin
    if (FLine[Run] = iCloseChar) and not IsEscaped then
      Break;
    if (FLine[Run] = '$') and (iCloseChar = '"') and
      ((FLine[Run + 1] = '{') or IsIdentChar(FLine[Run + 1])) then
    begin
      if (Run > 1) and (FLine[Run -1] = '{') then { complex syntax }
        Dec(Run);
      if not IsEscaped then
      begin
        { break the token to process the variable }
        FRange := rsVarExpansion;
        Exit;
      end
      else if FLine[Run] = '{' then
        Inc(Run); { restore Run if we previously deincremented it }
    end;
    Inc(Run);
  end;
  if (FLine[Run] = iCloseChar) then
    FRange := rsUnknown;
  if not IsLineEnd(Run) then Inc(Run);
end;

procedure TSynPHPSyn.VarExpansionProc;
type
  TExpansionSyntax = (esNormal, esComplex, esBrace);
var
  iSyntax: TExpansionSyntax;
  iOpenBraces: Integer;
  iOpenBrackets: Integer;
  iTempRun: Integer;
begin
  FRange := rsString34; { var expansion only occurs in double quoted strings }
  FTokenID := tkVariable;
  if FLine[Run] = '{' then
  begin
    iSyntax := esComplex;
    Inc(Run, 2); { skips '{$' }
  end
  else
  begin
    Inc( Run );
    if FLine[Run] = '{' then
    begin
      iSyntax := esBrace;
      Inc(Run);
    end
    else
      iSyntax := esNormal;
  end;
  if iSyntax in [esBrace, esComplex] then
  begin
    iOpenBraces := 1;
    while not IsLineEnd(Run) do
    begin
      if FLine[Run] = '}' then
      begin
        Dec(iOpenBraces);
        if iOpenBraces = 0 then
        begin
          Inc(Run);
          Break;
        end;
      end;
      if FLine[Run] = '{' then
        Inc(iOpenBraces);
      Inc(Run);
    end;
  end
  else
  begin
    while IsIdentChar(FLine[Run]) do
      Inc(Run);
    iOpenBrackets := 0;
    iTempRun := Run;
    { process arrays and objects }
    while not IsLineEnd(iTempRun) do
    begin
      if FLine[iTempRun] = '[' then
      begin
        Inc( iTempRun );
        if FLine[iTempRun] = #39 then
        begin
          Inc(iTempRun);
          while not IsLineEnd(iTempRun) and (FLine[iTempRun] <> #39) do
            Inc(iTempRun);
          if (FLine[iTempRun] = #39) and (FLine[iTempRun + 1 ] = ']') then
          begin
            Inc(iTempRun, 2);
            Run := iTempRun;
            continue;
          end
          else
            Break;
        end
        else
          Inc(iOpenBrackets);
      end
      else if (FLine[iTempRun] = '-') and (FLine[iTempRun +1] = '>') then
        Inc(iTempRun, 2)
      else
        Break;

      if not IsIdentChar(FLine[iTempRun]) then
        Break
      else
        repeat
          Inc(iTempRun);
        until not IsIdentChar(FLine[iTempRun]);

      while FLine[iTempRun] = ']' do
      begin
        if iOpenBrackets = 0 then
          Break;
        Dec(iOpenBrackets);
        Inc(iTempRun);
      end;
      if iOpenBrackets = 0 then
        Run := iTempRun;
    end;
  end;
end;

procedure TSynPHPSyn.TildeProc;
begin
  Inc(Run);                            {bitwise complement}
  FTokenID := tkSymbol;
end;

procedure TSynPHPSyn.VariableProc;
begin
  FTokenID := tkVariable;
  Inc(Run);
  while IsIdentChar(FLine[Run]) do Inc(Run);
end;

procedure TSynPHPSyn.XOrSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {xor assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else                                 {xor}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynPHPSyn.AnsiCProc;
begin
  FTokenID := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        Exit;
      end;
    #10:
      begin
        LFProc;
        Exit;
      end;
    #13:
      begin
        CRProc;
        Exit;
      end;
  end;

  while not IsLineEnd(Run) do
    if FLine[Run] = '*' then
    begin
      if FLine[Run + 1] = '/' then
      begin
        Inc(Run, 2);
        FRange := rsUnknown;
        Break;
      end
      else
        Inc(Run);
    end
    else
      Inc(Run);
end;

procedure TSynPHPSyn.String39Proc;
begin
  FRange := rsString39;
  Inc( Run );
  StringProc;
end;

procedure TSynPHPSyn.String34Proc;
begin
  FRange := rsString34;
  Inc( Run );
  StringProc;
end;

{$IFDEF SYN_HEREDOC}
procedure TSynPHPSyn.HeredocProc;

  procedure SkipToEOL;
  begin
    case FLine[Run] of
       #0: NullProc;
      #10: LFProc;
      #13: CRProc;
    else
      repeat
        Inc(Run);
      until IsLineEnd(Run);
    end;
  end;

var
  i: Integer;
begin
  if IsLineEnd(Run) and (FTokenPos = Run) then
  begin
    NextProcedure;
    Exit;
  end;
  FTokenID := tkString;

  if Run = 0 then
  begin
    i := 0;

    while not (IsLineEnd(FLine[i]) or (FLine[i] = ';')) do
    begin
      if i > FHeredocLength then
      begin
        SkipToEOL;
        Exit;
      end;
      Inc(i);
    end;

    if i <> FHeredocLength then
    begin
      SkipToEOL;
      Exit;
    end;

    if (CalcFCS(FLine[0], i) = FHeredocChecksum) then
    begin
      FRange := rsUnknown;
      Run := i;
      Exit;
    end;
  end;

  SkipToEOL;
end;
{$ENDIF}

procedure TSynPHPSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsComment: AnsiCProc;
    rsString39, rsString34: StringProc;
    rsVarExpansion: VarExpansionProc;
{$IFDEF SYN_HEREDOC}
    rsHeredoc: HeredocProc;
{$ENDIF}
    else
    begin
      FRange := rsUnknown;
      NextProcedure;
    end;
  end;

  // ensure that one call of Next is enough to reach next token
  if (fOldRun = Run) and not GetEol then Next;

  inherited;
end;

procedure TSynPHPSyn.NextProcedure;
begin
  case FLine[Run] of
    '&': AndSymbolProc;
    #39: String39Proc; // single quote
    '@': AtSymbolProc;
    '}': BraceCloseProc;
    '{': BraceOpenProc;
    #13: CRProc;
    ':': ColonProc;
    ',': CommaProc;
    '=': EqualProc;
    '>': GreaterProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    #10: LFProc;
    '<': LowerProc;
    '-': MinusProc;
    '*': MultiplyProc;
    '!': NotSymbolProc;
    #0: NullProc;
    '0'..'9': NumberProc;
    '|': OrSymbolProc;
    '+': PlusProc;
    '.': PointProc;
    '#': PoundProc;
    '?': QuestionProc;
    '%': RemainderSymbolProc;
    ')': RoundCloseProc;
    '(': RoundOpenProc;
    ';': SemiColonProc;
    '/': SlashProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    ']': SquareCloseProc;
    '[': SquareOpenProc;
    #34: String34Proc; // double quote
    '~': TildeProc;
    '$': VariableProc;
    '^': XOrSymbolProc;
    else UnknownProc;
  end;
end;

function TSynPHPSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynPHPSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynPHPSyn.GetRange: Pointer;
{$IFDEF SYN_HEREDOC}
var
  RangePointer: TRangePointer;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  RangePointer.Range := Ord(FRange);
  RangePointer.Length := 0;
  RangePointer.Checksum := 0;
  if FRange = rsHeredoc then
  begin
    RangePointer.Length := FHeredocLength;
    RangePointer.Checksum := FHeredocChecksum;
  end;
  Result := RangePointer.Ptr;
{$ELSE}
  Result := Pointer(FRange);
{$ENDIF}
end;

function TSynPHPSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynPHPSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkVariable: Result := FVariableAttri;
    tkUnknown: Result := FIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynPHPSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynPHPSyn.ResetRange;
begin
  FRange := rsUnknown;
{$IFDEF SYN_HEREDOC}
  FHeredocLength := 0;
  FHeredocChecksum := 0;
{$ENDIF}
end;

procedure TSynPHPSyn.SetRange(Value: Pointer);
{$IFDEF SYN_HEREDOC}
var
  RangePointer: TRangePointer;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  RangePointer := TRangePointer(Value);
  FRange := TRangeState(RangePointer.Range);
  FHeredocLength := 0;
  FHeredocChecksum := 0;
  if FRange = rsHeredoc then
  begin
    FHeredocLength := RangePointer.Length;
    FHeredocChecksum := RangePointer.Checksum;
  end;
{$ELSE}
  FRange := TRangeState(Value);
{$ENDIF}
end;

function TSynPHPSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterPHP;
end;

class function TSynPHPSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPHP;
end;

function TSynPHPSyn.GetSampleSource: UnicodeString;
begin
  Result := '// Syntax highlighting'#13#10+
            'function printNumber()'#13#10+
            '{'#13#10+
            '  $number = 1234;'#13#10+
            '  print "The number is $number";'#13#10+
            '  for ($i = 0; $i <= $number; $i++)'#13#10+
            '  {'#13#10+
            '    $x++;'#13#10+
            '    $x--;'#13#10+
            '    $x += 1.0;'#13#10+
            '  }'#13#10+
            '}';

end;

class function TSynPHPSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangPHP;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynPHPSyn);
{$ENDIF}
end.
