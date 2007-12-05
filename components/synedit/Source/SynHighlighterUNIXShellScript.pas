{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterUNIXShellScript.pas, released 2001-11-13.
The Initial Author of this file is Stefan Ascher.
Portions by Jan Verhoeven (http://jansfreeware.com/jfdelphi.htm)
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

$Id: SynHighlighterUNIXShellScript.pas,v 1.7.2.10 2006/08/19 16:12:12 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a UNIX Shell Script highlighter for SynEdit)
@author(Stefan Ascher <stievie2002@yahoo.com>)
@created(10 November 2001)
@lastmod(2001-11-13)
The SynHighlighterUNIXShellScript unit provides SynEdit with a UNIX Shell Script highlighter.
}

{$IFNDEF QSYNHIGHLIGHTERUNIXSHELLSCRIPT}
unit SynHighlighterUNIXShellScript;
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
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSecondKey,
    tkSpace, tkString, tkSymbol, tkVariable, tkUnknown);

{$IFDEF SYN_HEREDOC}
  TRangeState = (rsUnknown, rsHeredoc, rsIndentedHeredoc);

  TRangePointer = packed record
    case Boolean of
      True: (Ptr: Pointer);
      False: (Range: Byte; Length: Byte; Checksum: Word);
    end;
{$ELSE}
  TRangeState = (rsUnknown);
{$ENDIF}

type
  TSynUNIXShellScriptSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
{$IFDEF SYN_HEREDOC}
    fHeredocLength: Byte;
    fHeredocChecksum: Word;
{$ENDIF}
    FTokenID: TtkTokenKind;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSecondKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fVarAttri: TSynHighlighterAttributes;
    procedure BraceOpenProc;
    procedure PointCommaProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RoundOpenProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SymbolProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure DollarProc;
    procedure DotProc;
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
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function IsKeyword(const AKeyword: WideString): Boolean; override;
    function IsSecondKeyWord(AToken: WideString): Boolean;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property SecondKeyAttri: TSynHighlighterAttributes read fSecondKeyAttri
      write fSecondKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property VarAttri: TSynHighlighterAttributes read fVarAttri
      write fVarAttri;
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
  ShellScriptKeys: array[0..109] of WideString = (
    'awk', 'banner', 'basename', 'bdiff', 'bg', 'break', 'case', 'cat', 'cc',
    'cd', 'chdir', 'chgrp', 'chmod', 'chown', 'clear', 'compress', 'continue',
    'cp', 'cpio', 'cut', 'date', 'dd', 'df', 'diff', 'do', 'done', 'dtpad',
    'echo', 'elif', 'else', 'esac', 'eval', 'exit', 'export', 'expr', 'fg',
    'fi', 'finger', 'fold', 'for', 'ftp', 'g++', 'gcc', 'getopts', 'grep',
    'gzip', 'hash', 'head', 'if', 'in', 'jobs', 'kill', 'ld', 'ln', 'login',
    'ls', 'make', 'mkdir', 'mt', 'mv', 'newgrp', 'nohup', 'od', 'paste', 'perl',
    'pg', 'ping', 'pr', 'ps', 'pwd', 'rcp', 'read', 'remsh', 'return', 'rm',
    'rsh', 'rwho', 'sed', 'set', 'sh', 'shift', 'stop', 'strings', 'strip',
    'sync', 'tail', 'tar', 'telnet', 'test', 'then', 'times', 'tput', 'trap',
    'true', 'tty', 'type', 'ulimit', 'umask', 'unset', 'until', 'uudecode',
    'uuencode', 'vi', 'wait', 'wc', 'while', 'who', 'xtern', 'zcat', 'zip'
  );

  ShellScriptSecondKeys: array[0..22] of WideString = (
    'cdpath', 'editor', 'home', 'ifs', 'lang', 'lc_messages', 'lc_type',
    'ld_library_path', 'logname', 'mail', 'mailcheck', 'mailpath', 'manpath',
    'path', 'ps1', 'ps2', 'pwd', 'shacct', 'shell', 'shlib_path', 'term',
    'termcap', 'tz'
  );

function TSynUNIXShellScriptSyn.IsKeyword(const AKeyword: WideString): Boolean;
var
  First, Last, I, Compare: Integer;
  Token: WideString;
begin
  First := 0;
  Last := High(ShellScriptKeys);
  Result := False;
  Token := SynWideLowerCase(AKeyword);

  while First <= Last do
  begin
    I := (First + Last) shr 1;
    Compare := WideCompareStr(ShellScriptKeys[I], Token);
    if Compare = 0 then
    begin
      Result := True;
      break;
    end
    else
      if Compare < 0 then First := I + 1 else Last := I - 1;
  end;
end; { IsKeyWord }

function TSynUNIXShellScriptSyn.IsSecondKeyWord(AToken: WideString): Boolean;
var
  First, Last, I, Compare: Integer;
  Token: WideString;
begin
  First := 0;
  Last := High(ShellScriptSecondKeys);
  Result := False;
  Token := SynWideLowerCase(AToken);
  while First <= Last do
  begin
    I := (First + Last) shr 1;
    Compare := WideCompareStr(ShellScriptSecondKeys[i], Token);
    if Compare = 0 then
    begin
      Result := True;
      break;
    end
    else
      if Compare < 0 then First := I + 1 else Last := I - 1;
  end;
end; { IsSecondKeyWord }

constructor TSynUNIXShellScriptSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCaseSensitive := False;
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Foreground := clGreen;
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Foreground := clNavy;
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fSecondKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrSecondReservedWord, SYNS_FriendlyAttrSecondReservedWord);
  AddAttribute(fSecondKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  fNumberAttri.Foreground := clBlue;
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Foreground := clMaroon;
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  fSymbolAttri.Foreground := clRed;
  AddAttribute(fSymbolAttri);
  fVarAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  fVarAttri.Foreground := clPurple;
  AddAttribute(fVarAttri);
  SetAttributesOnChange(DefHighlightChange);

  fRange := rsUnknown;
  fDefaultFilter := SYNS_FilterUNIXShellScript;
end; { Create }

destructor TSynUNIXShellScriptSyn.Destroy;
begin
  inherited Destroy;
end; { Destroy }

procedure TSynUNIXShellScriptSyn.DollarProc;
var
  cc: WideChar;
begin
  inc(Run);
  fTokenID := tkVariable;
  if IsLineEnd(Run) then Exit;
  cc := FLine[Run];
  inc(Run);
  if (cc = '{') then
  begin
    // ${var}
    while IsIdentChar(FLine[Run]) do
    begin
      if IsLineEnd(Run) then break;
      inc(Run);
    end;
    if FLine[Run] = '}' then Inc(Run);
  end
  else
    // $var
    while IsIdentChar(FLine[Run]) do
      inc(Run);
end;

procedure TSynUNIXShellScriptSyn.DotProc;

  function TestDot: Boolean;
  var
    i: Integer;
  begin
    result := False;
    i := Run;
    inc(i);
    while (FLine[i] in [WideChar('a')..WideChar('z'), WideChar('A')..WideChar('Z')]) do
      inc(i);
    if i > (Run + 1) then
      Result := True;
    if Result then
      Run := i;
  end;
  
begin
  // Don't highlight filenames like filename.zip
  if TestDot then
    fTokenID := tkIdentifier
  else
  begin
    inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynUNIXShellScriptSyn.BraceOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynUNIXShellScriptSyn.PointCommaProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynUNIXShellScriptSyn.CRProc;
begin
  fTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: inc(Run, 2);
    else inc(Run);
  end;
end;

procedure TSynUNIXShellScriptSyn.IdentProc;
begin
  while IsIdentChar(fLine[Run]) do inc(Run);
  if IsKeyWord(GetToken) then
  begin
    fTokenId := tkKey;
    Exit;
  end
  else
    fTokenId := tkIdentifier;
    
  if IsSecondKeyWord(GetToken) then
    fTokenId := tkSecondKey
  else if fLine[Run] = '=' then
    FTokenID := tkVariable
  else
    fTokenId := tkIdentifier;
end;

procedure TSynUNIXShellScriptSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynUNIXShellScriptSyn.LowerProc;
{$IFDEF SYN_HEREDOC}

  // In UNIX Shell, Heredoc delimiter can be pretty much anything and the list
  // of alpha-numeric characters is extended with a few common special characters
  function IsAlphaNumChar(Run: Integer): Boolean;
  begin
    case fLine[Run] of
      'A'..'Z', 'a'..'z', '0'..'9', '_', '-', '+', '!', '#', '%':
        Result := True;
      else
        Result := False;
    end;
  end;

var
  i, Len, SkipRun: Integer;
  IndentedHeredoc: Boolean;
  QuoteChar: WideChar;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  if FLine[Run + 1] = '<' then
  begin
    fTokenID := tkSymbol;

    SkipRun := 0;
    QuoteChar := #0;
    if (FLine[Run + 2] = '-') and (FLine[Run + 3] in
      [WideChar('"'), WideChar(''''), WideChar('`'), WideChar('\')]) then
    begin
      SkipRun := 2;
      if FLine[Run + 3] <> '\' then
        QuoteChar := FLine[Run + 3];
    end
    else if (FLine[Run + 2] in
      [WideChar('-'), WideChar('"'), WideChar(''''), WideChar('`'), WideChar('\')]) then
    begin
      SkipRun := 1;
      if not (FLine[Run + 2] in [WideChar('-'), WideChar('\')]) then
        QuoteChar := FLine[Run + 2];
    end;
    IndentedHeredoc := (SkipRun > 0) and (FLine[Run + 2] = '-');

    if IsAlphaNumChar(Run + SkipRun + 2) then
    begin
      inc(Run, 2);

      i := Run;
      while IsAlphaNumChar(SkipRun + i) do Inc(i);
      Len := i - Run;

      if Len > 255 then
      begin
        fTokenID := tkUnknown;
        Exit;
      end;

      if (QuoteChar <> #0) and (FLine[Run + SkipRun + Len] <> QuoteChar) then
      begin
        fTokenID := tkUnknown;
        Exit;
      end;

      if IndentedHeredoc then
        fRange := rsIndentedHeredoc
      else
        fRange := rsHeredoc;
      fHeredocLength := Len;
      fHeredocChecksum := CalcFCS(FLine[Run + SkipRun], Len);

      Inc(Run, SkipRun + Len);
      fTokenID := tkString;
    end
    else
      inc(Run, 2);
  end
  else
{$ENDIF}
  begin
    inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynUNIXShellScriptSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynUNIXShellScriptSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', 'e', 'E':
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

procedure TSynUNIXShellScriptSyn.RoundOpenProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynUNIXShellScriptSyn.SlashProc;
begin
  if FLine[Run] = '#' then
  begin
    // Perl Styled Comment
    inc(Run);
    fTokenID := tkComment;
    while not IsLineEnd(Run) do
    begin
      inc(Run);
    end;
  end
  else
  begin
    inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynUNIXShellScriptSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynUNIXShellScriptSyn.StringProc;
var
  QuoteChar: WideChar;
begin
// Single and Double Quotes.

  fTokenID := tkString;
  QuoteChar := FLine[Run];      // either " or '
  if (FLine[Run + 1] = QuoteChar) and (FLine[Run + 2] = QuoteChar)
    then inc(Run, 2);
  repeat
    if IsLineEnd(Run) then break;
    inc(Run);
  until FLine[Run] = QuoteChar;
  if not IsLineEnd(Run) then inc(Run);
end;

procedure TSynUNIXShellScriptSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

{$IFDEF SYN_HEREDOC}
procedure TSynUNIXShellScriptSyn.HeredocProc;

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

  if fRange = rsIndentedHeredoc then
    while FLine[Run] in [WideChar(#9), WideChar(#32)] do Inc(Run);

  if ((Run = 0) and (fRange = rsHeredoc)) or (fRange = rsIndentedHeredoc) then
  begin
    i := 0;

    while not IsLineEnd(FLine[Run + i]) do
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

    if (CalcFCS(FLine[Run], i) = fHeredocChecksum) then
    begin
      fRange := rsUnknown;
      Run := Run + i;
      Exit;
    end;
  end;

  SkipToEOL;
end;
{$ENDIF}

procedure TSynUNIXShellScriptSyn.Next;
begin
  fTokenPos := Run;
{$IFDEF SYN_HEREDOC}
  if fRange in [rsHeredoc, rsIndentedHeredoc] then
    HeredocProc
  else
{$ENDIF}
    NextProcedure;
  inherited;
end;

procedure TSynUNIXShellScriptSyn.NextProcedure;
begin
  case fLine[Run] of
    '<': LowerProc;
    '#': SlashProc;
    '{': BraceOpenProc;
    ';': PointCommaProc;
    '.': DotProc;
    #13: CRProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    #10: LFProc;
    #0: NullProc;
    '0'..'9': NumberProc;
    '(': RoundOpenProc;
    '/': SlashProc;
    '$': DollarProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    #34, #39: StringProc;
    '}', ')', '!', '%', '&',':','@','[',']','^','`','~': SymbolProc;
    else UnknownProc;
  end;
end;

function TSynUNIXShellScriptSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TSynUNIXShellScriptSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynUNIXShellScriptSyn.GetRange: Pointer;
{$IFDEF SYN_HEREDOC}
var
  RangePointer: TRangePointer;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  RangePointer.Range := Ord(fRange);
  RangePointer.Length := 0;
  RangePointer.Checksum := 0;
  if fRange in [rsHeredoc, rsIndentedHeredoc] then
  begin
    RangePointer.Length := fHeredocLength;
    RangePointer.Checksum := fHeredocChecksum;
  end;
  Result := RangePointer.Ptr;
{$ELSE}
  Result := Pointer(fRange);
{$ENDIF}
end;

function TSynUNIXShellScriptSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynUNIXShellScriptSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkSecondKey: Result := fSecondKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkVariable: Result := fVarAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynUNIXShellScriptSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

procedure TSynUNIXShellScriptSyn.ResetRange;
begin
  fRange := rsUnknown;
{$IFDEF SYN_HEREDOC}
  fHeredocLength := 0;
  fHeredocChecksum := 0;
{$ENDIF}
end;

procedure TSynUNIXShellScriptSyn.SetRange(Value: Pointer);
{$IFDEF SYN_HEREDOC}
var
  RangePointer : TRangePointer;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  RangePointer := TRangePointer(Value);
  fRange := TRangeState(RangePointer.Range);
  fHeredocLength := 0;
  fHeredocChecksum := 0;
  if fRange in [rsHeredoc, rsIndentedHeredoc] then
  begin
    fHeredocLength := RangePointer.Length;
    fHeredocChecksum := RangePointer.Checksum;
  end;
{$ELSE}
  fRange := TRangeState(Value);
{$ENDIF}
end;

function TSynUNIXShellScriptSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterUNIXShellScript;
end;

class function TSynUNIXShellScriptSyn.GetLanguageName: string;
begin
  Result := SYNS_LangNameUNIXShellScript;
end;

function TSynUNIXShellScriptSyn.GetSampleSource: WideString;
begin
  Result := '######################################'#13#10 +
            '# Here is a comment about some stuff #'#13#10 +
            '######################################'#13#10 +
            ''#13#10 +
            'case $BUILD_MODE in'#13#10 +
            '  full )'#13#10 +
            '      MyFirstFunction'#13#10 +
            '      ;;'#13#10 +
            '  rekit)'#13#10 +
            '      MySecondFunction'#13#10 +
            '    ;;'#13#10 +
            '  installer)'#13#10 +
            '      MyThirdFunction'#13#10 +
            '    ;;'#13#10 +
            'esac';
end;

class function TSynUNIXShellScriptSyn.GetFriendlyLanguageName: WideString;
begin
  Result := SYNS_FriendlyLangNameUNIXShellScript;
end;

procedure TSynUNIXShellScriptSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynUNIXShellScriptSyn);
{$ENDIF}
end.
