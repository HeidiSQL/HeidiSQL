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

$Id: SynHighlighterPHP.pas,v 1.22.2.7 2005/12/16 20:09:37 maelh Exp $

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

{$IFNDEF QSYNHIGHLIGHTERPHP}
unit SynHighlighterPHP;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
{$ELSE}
  Graphics,
  Registry,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull,
    tkNumber, tkSpace, tkString, tkSymbol, tkUnknown, tkVariable);

{$IFDEF SYN_HEREDOC}
  TRangeState = (rsUnKnown, rsString39, rsString34, rsComment, rsVarExpansion,
    rsHeredoc);

  TRangePointer = packed record
    case Boolean of
      True: (Ptr: Pointer);
      False: (Range: Byte; Length: Byte; Checksum: Word);
    end;
{$ELSE}
  TRangeState = (rsUnKnown, rsString39, rsString34, rsComment, rsVarExpansion);
{$ENDIF}

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynPHPSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
{$IFDEF SYN_HEREDOC}
    fHeredocLength: Byte;
    fHeredocChecksum: Word;
{$ENDIF}
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..136] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
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
    function GetSampleSource: WideString; override;
    function IsFilterStored: Boolean; override;
    procedure NextProcedure;
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
    property VariableAttri: TSynHighlighterAttributes read fVariableAttri
      write fVariableAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditMiscProcs,
  QSynEditStrConst;
{$ELSE}
  SynEditMiscProcs,
  SynEditStrConst;
{$ENDIF}

const
  KeyWords: array[0..55] of WideString = (
    '__file__', '__line__', 'and', 'array', 'break', 'case', 'cfunction', 
    'class', 'const', 'continue', 'default', 'die', 'do', 'double', 'echo', 
    'else', 'elseif', 'empty', 'endfor', 'endif', 'endswitch', 'endwhile', 
    'eval', 'exit', 'extends', 'false', 'float', 'for', 'foreach', 'function', 
    'global', 'highlight_file', 'highlight_string', 'if', 'include', 'int', 
    'integer', 'isset', 'list', 'new', 'object', 'old_function', 'or', 'print', 
    'real', 'require', 'return', 'show_source', 'static', 'string', 'switch', 
    'true', 'unset', 'var', 'while', 'xor' 
  );

  KeyIndices: array[0..136] of Integer = (
    -1, 25, -1, -1, 14, 28, -1, -1, 47, 33, -1, -1, -1, -1, 16, -1, -1, -1, -1,
    7, 41, -1, -1, -1, 40, -1, 24, -1, -1, -1, -1, 26, 20, 9, -1, 6, -1, -1, 29,
    -1, -1, 1, 15, 34, -1, -1, -1, -1, -1, -1, 31, 10, -1, -1, 2, -1, -1, 48,
    -1, 30, -1, 42, 13, -1, -1, 27, -1, -1, 37, 38, -1, -1, 0, 5, -1, 4, 32, 49,
    -1, 53, 54, -1, -1, -1, 18, 39, -1, -1, -1, -1, 55, 19, -1, -1, -1, 12, -1,
    35, 8, -1, 50, 46, -1, 17, -1, -1, -1, -1, 43, -1, 51, 11, -1, -1, 52, -1,
    44, -1, -1, -1, -1, -1, 23, 22, 21, -1, -1, 45, 36, -1, -1, -1, -1, -1, -1,
    -1, 3
  );

{$Q-}
function TSynPHPSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 596 + Ord(Str^) * 413;
    inc(Str);
  end;
  Result := Result mod 137;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynPHPSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Key: Cardinal;
begin
  fToIdent := MayBe;
  Key := HashKey(MayBe);
  if Key <= High(fIdentFuncTable) then
    Result := fIdentFuncTable[Key](KeyIndices[Key])
  else
    Result := tkIdentifier;
end;

procedure TSynPHPSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if @fIdentFuncTable[i] = nil then
      fIdentFuncTable[i] := KeyWordFunc;
end;

function TSynPHPSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynPHPSyn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

constructor TSynPHPSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
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
  fVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  AddAttribute(fVariableAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterPHP;
  fRange := rsUnknown;
end;

procedure TSynPHPSyn.AndSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {and assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '&':                               {conditional and}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {and}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.AtSymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynPHPSyn.BraceCloseProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynPHPSyn.BraceOpenProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynPHPSyn.CRProc;
begin
  fTokenID := tkSpace;
  Case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TSynPHPSyn.ColonProc;
begin
  inc(Run);                            {colon - conditional}
  fTokenID := tkSymbol;
end;

procedure TSynPHPSyn.CommaProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynPHPSyn.EqualProc;
begin
  case FLine[Run + 1] of
    '=':                               {logical equal}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>':                               {Hash operator}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {assign}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.GreaterProc;
begin
  case FLine[Run + 1] of
    '=':                               {greater than or equal to}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>':
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {greater than}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do inc(Run);
end;

procedure TSynPHPSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
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
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '<':
      begin
        fTokenID := tkSymbol;
{$IFDEF SYN_HEREDOC}
        if (FLine[Run + 2] = '<') and IsIdentChar(FLine[Run + 3]) then
        begin
          inc(Run, 3);

          i := Run;
          while IsIdentChar(FLine[i]) do Inc(i);
          Len := i - Run;

          if Len > 255 then
          begin
            fTokenID := tkUnknown;
            Exit;
          end;

          fRange := rsHeredoc;
          fHeredocLength := Len;
          fHeredocChecksum := CalcFCS(FLine[Run], Len);

          Inc(Run, Len);
          fTokenID := tkString;
        end
        else
{$ENDIF}
        if FLine[Run + 2] = '=' then   {shift left assign}
        begin
          inc(Run, 3)
        end
        else                           {shift left}
        begin
          inc(Run, 2);
        end;
      end;
  else                                 {less than}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.MinusProc;
begin
  case FLine[Run + 1] of
    '=':                               {subtract assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '-':                               {decrement}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>':                               {Class operator}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {subtract}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.MultiplyProc;
begin
  case FLine[Run + 1] of
    '=':                               {multiply assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {multiply}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.NotSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {not equal}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {logical complement}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynPHPSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', '-', 'l', 'L', 'x', 'X', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  inc(Run);
  fTokenID := tkNumber;
  while IsNumberChar do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSynPHPSyn.OrSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {inclusive or assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '|':                               {conditional or}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {inclusive or}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.PlusProc;
begin
  case FLine[Run + 1] of
    '=':                               {add assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '+':                               {increment}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {add}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.PointProc;
begin
  inc(Run);                            {point}
  fTokenID := tkSymbol;
end;

procedure TSynPHPSyn.PoundProc;
begin
  repeat
    inc(Run);
  until IsLineEnd(Run);
  fTokenID := tkComment;
end;

procedure TSynPHPSyn.QuestionProc;
begin
  fTokenID := tkSymbol;                {question mark - conditional}
  inc(Run);
end;

procedure TSynPHPSyn.RemainderSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {remainder assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {remainder}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.RoundCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynPHPSyn.RoundOpenProc;
begin
  inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynPHPSyn.SemiColonProc;
begin
  inc(Run);                            {semicolon}
  fTokenID := tkSymbol;
end;

procedure TSynPHPSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '/':                               {c++ style comments}
      begin
        inc(Run, 2);
        fTokenID := tkComment;
        while not IsLineEnd(Run) do
          inc(Run);
      end;
    '*':
      begin
        fRange := rsComment;
        inc(Run);
        fTokenID := tkComment;       {c style comment}

        inc(Run);
        while not IsLineEnd(Run) do
          if fLine[Run] = '*' then
          begin
            if fLine[Run + 1] = '/' then
            begin
              fRange := rsUnKnown;
              inc(Run, 2);
              break;
            end
            else
              inc(Run)
          end
          else
            inc(Run);
      end;
    '=':                               {division assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {division}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynPHPSyn.SquareCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynPHPSyn.SquareOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
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
  if IsLineEnd(Run) and (fTokenPos = Run) then
  begin
    NextProcedure;
    Exit;
  end;
  fTokenID := tkString;
  if fRange = rsString39 then
    iCloseChar := #39
  else
    iCloseChar := #34;
  while not IsLineEnd(Run) do
  begin
    if (FLine[Run] = iCloseChar) and not IsEscaped then
      break;
    if (FLine[Run] = '$') and (iCloseChar = '"') and
      ((FLine[Run + 1] = '{') or IsIdentChar(FLine[Run + 1])) then
    begin
      if (Run > 1) and (FLine[Run -1] = '{') then { complex syntax }
        Dec(Run);
      if not IsEscaped then
      begin
        { break the token to process the variable }
        fRange := rsVarExpansion;
        Exit;
      end
      else if FLine[Run] = '{' then
        Inc(Run); { restore Run if we previously deincremented it }
    end;
    Inc(Run);
  end;
  if (FLine[Run] = iCloseChar) then
    fRange := rsUnKnown;
  if not IsLineEnd(Run) then inc(Run);
end;

procedure TSynPHPSyn.VarExpansionProc;
type
  TExpansionSyntax = (esNormal, esComplex, esBrace);
var
  iSyntax: TExpansionSyntax;
  iOpenBraces: integer;
  iOpenBrackets: integer;
  iTempRun: integer;
begin
  fRange := rsString34; { var expansion only occurs in double quoted strings }
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
          break;
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
          if (FLine[iTempRun] = #39) and (fLine[iTempRun + 1 ] = ']') then
          begin
            Inc(iTempRun, 2);
            Run := iTempRun;
            continue;
          end
          else
            break;
        end
        else
          Inc(iOpenBrackets);
      end
      else if (FLine[iTempRun] = '-') and (FLine[iTempRun +1] = '>') then
        Inc(iTempRun, 2)
      else
        break;

      if not IsIdentChar(FLine[iTempRun]) then
        break
      else
        repeat
          Inc(iTempRun);
        until not IsIdentChar(FLine[iTempRun]);

      while FLine[iTempRun] = ']' do
      begin
        if iOpenBrackets = 0 then
          break;
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
  inc(Run);                            {bitwise complement}
  fTokenId := tkSymbol;
end;

procedure TSynPHPSyn.VariableProc;
begin
  fTokenID := tkVariable;
  inc(Run);
  while IsIdentChar(fLine[Run]) do inc(Run);
end;

procedure TSynPHPSyn.XOrSymbolProc;
begin
  Case FLine[Run + 1] of
    '=':                               {xor assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {xor}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynPHPSyn.AnsiCProc;
begin
  fTokenID := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        exit;
      end;
    #10:
      begin
        LFProc;
        exit;
      end;
    #13:
      begin
        CRProc;
        exit;
      end;
  end;

  while not IsLineEnd(Run) do
    if FLine[Run] = '*' then
    begin
      if fLine[Run + 1] = '/' then
      begin
        inc(Run, 2);
        fRange := rsUnKnown;
        break;
      end
      else
        inc(Run);
    end
    else
      inc(Run);
end;

procedure TSynPHPSyn.String39Proc;
begin
  fRange := rsString39;
  Inc( Run );
  StringProc;
end;

procedure TSynPHPSyn.String34Proc;
begin
  fRange := rsString34;
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
        inc(Run);
      until IsLineEnd(Run);
    end;
  end;

var
  i: Integer;
begin
  if IsLineEnd(Run) and (fTokenPos = Run) then
  begin
    NextProcedure;
    Exit;
  end;
  fTokenID := tkString;

  if Run = 0 then
  begin
    i := 0;

    while not (IsLineEnd(FLine[i]) or (FLine[i] = ';')) do
    begin
      if i > fHeredocLength then
      begin
        SkipToEOL;
        Exit;
      end;
      Inc(i);
    end;

    if i <> fHeredocLength then
    begin
      SkipToEOL;
      Exit;
    end;

    if (CalcFCS(FLine[0], i) = fHeredocChecksum) then
    begin
      fRange := rsUnknown;
      Run := i;
      Exit;
    end;
  end;

  SkipToEOL;
end;
{$ENDIF}

procedure TSynPHPSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsComment: AnsiCProc;
    rsString39, rsString34: StringProc;
    rsVarExpansion: VarExpansionProc;
{$IFDEF SYN_HEREDOC}
    rsHeredoc: HeredocProc;
{$ENDIF}
    else
    begin
      fRange := rsUnknown;
      NextProcedure;
    end;
  end;

  // ensure that one call of Next is enough to reach next token
  if (fOldRun = Run) and not GetEol then Next;

  inherited;
end;

procedure TSynPHPSyn.NextProcedure;
begin
  case fLine[Run] of
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

function TSynPHPSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TSynPHPSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynPHPSyn.GetRange: Pointer;
{$IFDEF SYN_HEREDOC}
var
  RangePointer: TRangePointer;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  RangePointer.Range := Ord(fRange);
  RangePointer.Length := 0;
  RangePointer.Checksum := 0;
  if fRange = rsHeredoc then
  begin
    RangePointer.Length := fHeredocLength;
    RangePointer.Checksum := fHeredocChecksum;
  end;
  Result := RangePointer.Ptr;
{$ELSE}
  Result := Pointer(fRange);
{$ENDIF}
end;

function TSynPHPSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynPHPSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkVariable: Result := fVariableAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynPHPSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

procedure TSynPHPSyn.ResetRange;
begin
  fRange := rsUnknown;
{$IFDEF SYN_HEREDOC}
  fHeredocLength := 0;
  fHeredocChecksum := 0;
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
  fRange := TRangeState(RangePointer.Range);
  fHeredocLength := 0;
  fHeredocChecksum := 0;
  if fRange = rsHeredoc then
  begin
    fHeredocLength := RangePointer.Length;
    fHeredocChecksum := RangePointer.Checksum;
  end;
{$ELSE}
  fRange := TRangeState(Value);
{$ENDIF}
end;

function TSynPHPSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterPHP;
end;

class function TSynPHPSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPHP;
end;

function TSynPHPSyn.GetSampleSource: WideString;
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

class function TSynPHPSyn.GetFriendlyLanguageName: WideString;
begin
  Result := SYNS_FriendlyLangPHP;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynPHPSyn);
{$ENDIF}
end.
