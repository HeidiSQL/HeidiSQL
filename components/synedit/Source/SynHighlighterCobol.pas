{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: SynHighlighterCobol.pas, released 2002-08-26.
Description: COBOL Syntax Parser/Highlighter
The author of this file is Andrey Ustinov.
Copyright (c) 2002 Software Mining, http://www.softwaremining.com/.
Unicode translation by Maël Hörz.
All rights reserved.

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

$Id: SynHighlighterCobol.pas,v 1.5.2.7 2008/09/14 16:24:59 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

unit SynHighlighterCobol;

{$I SynEdit.inc}

interface

uses
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynHighlighterHashEntries,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (
    tkComment,
    tkIdentifier,
    tkAIdentifier,
    tkPreprocessor,
    tkKey,
    tkBoolean,
    tkNull,
    tkNumber,
    tkSpace,
    tkString,
    tkSequence,
    tkIndicator,
    tkTagArea,
    tkDebugLines,
    tkUnknown);

  TRangeState = (rsUnknown,
                 rsQuoteString, rsApostString,
                 rsPseudoText,
                 rsQuoteStringMayBe, rsApostStringMayBe);

type
  TSynCobolSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FIndicator: WideChar;

    FCodeStartPos: LongInt;
    FCodeMediumPos: LongInt;
    FCodeEndPos: LongInt;

    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FAIdentifierAttri: TSynHighlighterAttributes;
    FPreprocessorAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FBooleanAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSequenceAttri: TSynHighlighterAttributes;
    FIndicatorAttri: TSynHighlighterAttributes;
    FTagAreaAttri: TSynHighlighterAttributes;
    FDebugLinesAttri: TSynHighlighterAttributes;
    FKeywords: TSynHashEntryList;
    procedure DoAddKeyword(AKeyword: UnicodeString; AKind: Integer);
    function HashKey(Str: PWideChar): Integer;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure IdentProc;
    procedure UnknownProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure NumberProc;
    procedure PointProc;
    procedure StringOpenProc;
    procedure StringProc;
    procedure StringEndProc;
    procedure FirstCharsProc;
    procedure LastCharsProc;
    procedure CommentProc;
    procedure DebugProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
    procedure NextProcedure;

    procedure SetCodeStartPos(Value: LongInt);
    procedure SetCodeMediumPos(Value: LongInt);
    procedure SetCodeEndPos(Value: LongInt);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property AreaAIdentifierAttri: TSynHighlighterAttributes read FAIdentifierAttri write FAIdentifierAttri;
    property PreprocessorAttri: TSynHighlighterAttributes read FPreprocessorAttri write FPreprocessorAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property BooleanAttri: TSynHighlighterAttributes read FBooleanAttri write FBooleanAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property SequenceAttri: TSynHighlighterAttributes read FSequenceAttri write FSequenceAttri;
    property IndicatorAttri: TSynHighlighterAttributes read FIndicatorAttri write FIndicatorAttri;
    property TagAreaAttri: TSynHighlighterAttributes read FTagAreaAttri write FTagAreaAttri;
    property DebugLinesAttri: TSynHighlighterAttributes read FDebugLinesAttri write FDebugLinesAttri;

    property AreaAStartPos: LongInt read FCodeStartPos write SetCodeStartPos;
    property AreaBStartPos: LongInt read FCodeMediumPos write SetCodeMediumPos;
    property CodeEndPos: LongInt read FCodeEndPos write SetCodeEndPos;
  end;

implementation

uses
  SynEditStrConst;

const
  BooleanWords: UnicodeString =
    'false, true';

  KeyWords: UnicodeString =
    'accept, access, acquire, add, address, advancing, after, all, allowing, ' +
    'alphabet, alphabetic, alphabetic-lower, alphabetic-upper, alphanumeric, ' +
    'alphanumeric-edited, also, alter, alternate, and, any, apply, are, ' +
    'area, areas, area-value, arithmetic, ascending, assign, at, author, ' +
    'auto, automatic, auto-skip, background-color, background-colour, ' +
    'backward, b-and, beep, before, beginning, bell, b-exor, binary, bit, ' +
    'bits, blank, b-less, blink, block, b-not, boolean, b-or, bottom, by, ' +
    'call, cancel, cd, cf, ch, chain, chaining, changed, character, ' +
    'characters, class, clock-units, close, cobol, code, code-set, col, ' +
    'collating, color, column, comma, command-line, commit, commitment, ' +
    'common, communication, comp, comp-0, comp-1, comp-2, comp-3, comp-4, ' +
    'comp-5, comp-6, comp-7, comp-8, comp-9, computational, computational-0, ' +
    'computational-1, computational-2, computational-3, computational-4, ' +
    'computational-5, computational-6, computational-7, computational-8, ' +
    'computational-9, computational-x, compute, comp-x, com-reg, ' +
    'configuration, connect, console, contained, contains, content, ' +
    'continue, control-area, controls, converting, corr, corresponding, ' +
    'count, crt, crt-under, currency, current, cursor, cycle, data, date, ' +
    'date-compiled, date-written, day, day-of-week, db, ' +
    'db-access-control-key, dbcs, db-data-name, db-exception, ' +
    'db-format-name, db-record-name, db-set-name, db-status, de, ' +
    'debug-contents, debugging, debug-item, debug-line, debug-name, ' +
    'debug-sub-1, debug-sub-2, debug-sub-3, decimal-point, declaratives, ' +
    'default, delimited, delimiter, depending, descending, destination, ' +
    'detail, disable, disconnect, disk, display, display-1, display-2, ' +
    'display-3, display-4, display-5, display-6, display-7, display-8, ' +
    'display-9, divide, division, down, drop, duplicate, duplicates, ' +
    'dynamic, egcs, egi, else, emi, empty, empty-check, enable, end, ' +
    'end-accept, end-add, end-call, end-compute, end-delete, end-disable, ' +
    'end-divide, end-enable, end-evaluate, end-if, ending, end-multiply, ' +
    'end-of-page, end-perform, end-read, end-receive, end-return, ' +
    'end-rewrite, end-search, end-send, end-start, end-string, end-subtract, ' +
    'end-transceive, end-unstring, end-write, enter, entry, environment, ' +
    'eop, equal, equals, erase, error, escape, esi, evaluate, every, exact, ' +
    'exceeds, exception, excess-3, exclusive, exec, execute, exhibit, exit, ' +
    'extend, external, externally-described-key, fd, fetch, file, ' +
    'file-control, file-id, filler, final, find, finish, first, fixed, ' +
    'footing, for, foreground-color, foreground-colour, form, format, free, ' +
    'from, full, function, generate, get, giving, global, go, goback, ' +
    'greater, group, heading, highlight, id, identification, if, in, index, ' +
    'index-1, index-2, index-3, index-4, index-5, index-6, index-7, index-8, ' +
    'index-9, indexed, indic, indicate, indicator, indicators, initial, ' +
    'initialize, initiate, input, input-output, inspect, installation, into, ' +
    'invalid, i-o, i-o-control, is, japanese, just, justified, kanji, keep, ' +
    'kept, key, keyboard, last, ld, leading, left, left-justify, length, ' +
    'length-check, less, like, limit, limits, linage, linage-counter, line, ' +
    'line-counter, lines, linkage, locally, lock, manual, member, memory, ' +
    'merge, message, mode, modified, modify, modules, more-labels, move, ' +
    'multiple, multiply, name, native, negative, next, no, no-echo, none, ' +
    'normal, not, number, numeric, numeric-edited, object-computer, occurs, ' +
    'of, off, omitted, on, only, open, optional, or, order, organization, ' +
    'other, output, overflow, owner, packed-decimal, padding, page, ' +
    'page-counter, palette, paragraph, password, perform, pf, ph, pic, ' +
    'picture, plus, pointer, position, positive, present, previous, printer, ' +
    'printer-1, printing, print-switch, prior, procedure, procedures, ' +
    'proceed, process, processing, program, program-id, prompt, protected, ' +
    'purge, queue, random, range, rd, read, realm, receive, reconnect, ' +
    'record, recording, record-name, records, redefines, reel, reference, ' +
    'references, relation, relative, release, remainder, removal, renames, ' +
    'repeated, replacing, report, reporting, reports, required, rerun, ' +
    'reserve, retaining, retrieval, return, return-code, reversed, ' +
    'reverse-video, rewind, rewrite, rf, rh, right, right-justify, rollback, ' +
    'rolling, rounded, run, same, screen, sd, search, section, secure, ' +
    'security, segment, segment-limit, select, send, sentence, separate, ' +
    'sequence, sequential, session-id, set, shared, shift-in, shift-out, ' +
    'sign, size, sort, sort-control, sort-core-size, sort-file-size, ' +
    'sort-merge, sort-message, sort-mode-size, sort-return, source, ' +
    'source-computer, space-fill, special-names, standard, standard-1, ' +
    'standard-2, standard-3, standard-4, start, starting, status, stop, ' +
    'store, string, subfile, subprogram, sub-queue-1, sub-queue-2, ' +
    'sub-queue-3, sub-schema, subtract, sum, suppress, switch, switch-1, ' +
    'switch-2, switch-3, switch-4, switch-5, switch-6, switch-7, switch-8, ' +
    'symbolic, sync, synchronized, table, tally, tallying, tape, tenant, ' +
    'terminal, terminate, test, text, than, then, through, thru, time, ' +
    'timeout, times, to, top, trailing, trailing-sign, transaction, ' +
    'transceive, type, underline, unequal, unit, unlock, unstring, until, ' +
    'up, update, upon, usage, usage-mode, user, using, valid, validate, ' +
    'value, values, variable, varying, wait, when, when-compiled, with, ' +
    'within, words, working-storage, write, write-only, zero-fill';

  PreprocessorWords: UnicodeString =
    'basis, cbl, control, copy, delete, eject, insert, ready, reload, ' +
    'replace, reset, service, skip1, skip2, skip3, title, trace, use';

  StringWords: UnicodeString =
    'high-value, high-values, low-value, low-values, null, nulls, quote, ' +
    'quotes, space, spaces, zero, zeroes, zeros';

  // Ambigious means that a simple string comparision is not enough
  AmbigiousWords: UnicodeString =
    'label';

const
  StringChars: array[TRangeState] of WideChar = (#0, '"', '''', '=',  '"', '''');

procedure TSynCobolSyn.DoAddKeyword(AKeyword: UnicodeString; AKind: Integer);
var
  HashValue: Integer;
begin
  HashValue := HashKey(PWideChar(AKeyword));
  FKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

function TSynCobolSyn.HashKey(Str: PWideChar): Integer;
var
  InternalRun: LongInt;

  function GetOrd: Integer;
  begin
    case Str^ of
      'a'..'z': Result := 1 + Ord(Str^) - Ord('a');
      'A'..'Z': Result := 1 + Ord(Str^) - Ord('A');
      '0'..'9': Result := 28 + Ord(Str^) - Ord('0');
      '-': Result := 27;
      else Result := 0;
    end
  end;

begin
  InternalRun := Run;
  Result := 0;

  while IsIdentChar(Str^) and (InternalRun <= FCodeEndPos) do
  begin
{$IFOPT Q-}
    Result := 7 * Result + GetOrd;
{$ELSE}
    Result := (7 * Result + GetOrd) and $FFFFFF;
{$ENDIF}
    Inc(Str);
    Inc(InternalRun);
  end;

  Result := Result and $FF; // 255
  FStringLen := Str - FToIdent;
end;

function TSynCobolSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
  I: Integer;
begin
  FToIdent := MayBe;
  Entry := FKeywords[HashKey(MayBe)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > FStringLen then
      Break
    else if Entry.KeywordLen = FStringLen then
      if IsCurrentToken(Entry.Keyword) then
      begin
        Result := TtkTokenKind(Entry.Kind);

        if Result = tkUnknown then // handling of "ambigious" words 
        begin
          if IsCurrentToken('label') then
          begin
            I := Run + Length('label');
            while FLine[I] = ' ' do
              Inc(I);
            if (WStrLComp(PWideChar(@FLine[I]), 'record', Length('record')) = 0)
              and (I + Length('record') - 1 <= FCodeEndPos) then
                Result := tkKey
              else
                Result := tkPreprocessor;
          end
          else
            Result := tkIdentifier;
        end;
        
        Exit;
      end;
    Entry := Entry.Next;
  end;
  Result := tkIdentifier;
end;

procedure TSynCobolSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until not CharInSet(FLine[Run], [#1..#32]);
end;

procedure TSynCobolSyn.FirstCharsProc;
var
  I: Integer;
begin
  if IsLineEnd(Run) then
    NextProcedure
  else if Run < FCodeStartPos - 1 then
  begin
    FTokenID := tkSequence;
    repeat
      Inc(Run);
    until (Run = FCodeStartPos - 1) or IsLineEnd(Run);
  end
  else
  begin
    FTokenID := tkIndicator;
    case FLine[Run] of
      '*', '/', 'D', 'd': FIndicator := FLine[Run];
      '-': if FRange in [rsQuoteStringMayBe, rsApostStringMayBe] then
           begin
             I := Run + 1;
             while FLine[I] = ' ' do
               Inc(I);
             if (WStrLComp(PWideChar(@FLine[I]), PWideChar(UnicodeStringOfChar(StringChars[FRange], 2)), 2) <> 0)
               or (I + 1 > FCodeEndPos) then
                 FRange := rsUnknown;
           end;
    end;
    Inc(Run);
  end;
end;

procedure TSynCobolSyn.LastCharsProc;
begin
  if IsLineEnd(Run) then
    NextProcedure
  else
  begin
    FTokenID := tkTagArea;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynCobolSyn.CommentProc;
begin
  FIndicator := #0;

  if IsLineEnd(Run) then
    NextProcedure
  else
  begin
    FTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run) or (Run > FCodeEndPos);
  end;
end;

procedure TSynCobolSyn.DebugProc;
begin
  FIndicator := #0;

  if IsLineEnd(Run) then
    NextProcedure
  else
  begin
    FTokenID := tkDebugLines;
    repeat
      Inc(Run);
    until IsLineEnd(Run) or (Run > FCodeEndPos);
  end;
end;

procedure TSynCobolSyn.PointProc;
begin
  if (Run < FCodeEndPos) and CharInSet(FLine[Run + 1], ['0'..'9', 'e', 'E']) then
    NumberProc
  else
    UnknownProc;
end;

procedure TSynCobolSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', '.', 'e', 'E', '-', '+':
        Result := True;
      else
        Result := False;
    end;
  end;

var
  fFloat: Boolean;
begin
  FTokenID := tkNumber;
  Inc(Run);
  fFloat := False;

  while IsNumberChar and (Run <= FCodeEndPos) do
  begin
    case FLine[Run] of
      '.':
        if not CharInSet(FLine[Run + 1], ['0'..'9', 'e', 'E']) then
          Break
        else
          fFloat := True;
      'e', 'E':
          if not CharInSet(FLine[Run - 1], ['0'..'9', '.']) then
            Break
          else fFloat := True;
      '-', '+':
        begin
          if not fFloat or not CharInSet(FLine[Run - 1], ['e', 'E']) then
            Break;
        end;
    end;
    Inc(Run);
  end;
end;

procedure TSynCobolSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynCobolSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynCobolSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynCobolSyn.StringOpenProc;
begin
  case FLine[Run] of
    '"': FRange := rsQuoteString;
    '''': FRange := rsApostString;
    else
      if FLine[Run + 1] = '=' then
      begin
        FRange := rsPseudoText;
        Inc(Run);
      end
      else
      begin
        UnknownProc;
        Exit;
      end;
  end;

  Inc(Run);
  StringProc;
  FTokenID := tkString;
end;

procedure TSynCobolSyn.StringProc;
begin
  FTokenID := tkString;

  if Run <= FCodeEndPos then
  repeat
    if (FLine[Run] = StringChars[FRange])
      and ((FLine[Run] <> '=') or ((Run > 0) and (FLine[Run - 1] = '='))) then
    begin
      if (Run = FCodeEndPos) and (FRange in [rsQuoteString, rsApostString]) then
        Inc(FRange, 3)
      else
        FRange := rsUnknown;
      Inc(Run);
      Break;
    end;
    if not IsLineEnd(Run) then
      Inc(Run);
  until IsLineEnd(Run) or (Run > FCodeEndPos);
end;

procedure TSynCobolSyn.StringEndProc;
begin
  if IsLineEnd(Run) then
    NextProcedure
  else
  begin
    FTokenID := tkString;

    if (FRange <> rsPseudoText) and (Run <= FCodeEndPos) then
    repeat
      if (FLine[Run] = StringChars[FRange]) then
      begin
        if FRange in [rsQuoteString, rsApostString] then
          Inc(Run)
        else
        begin
          Inc(Run, 2);
          Dec(FRange, 3);
        end;
        Break;
      end;
      Inc(Run);
    until IsLineEnd(Run) or (Run > FCodeEndPos);

    StringProc;
  end;
end;

constructor TSynCobolSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := False;

  FKeywords := TSynHashEntryList.Create;

  FCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  FCommentAttri.Foreground := clGray;
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FAIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrAreaAIdentifier, SYNS_FriendlyAttrAreaAIdentifier);
  FAIdentifierAttri.Foreground := clTeal;
  FAIdentifierAttri.Style := [fsBold];
  AddAttribute(FAIdentifierAttri);

  FPreprocessorAttri := TSynHighLighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  FPreprocessorAttri.Foreground := clMaroon;
  AddAttribute(FPreprocessorAttri);

  FKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FNumberAttri := TSynHighLighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clGreen;
  AddAttribute(FNumberAttri);

  FBooleanAttri := TSynHighLighterAttributes.Create(SYNS_AttrBoolean, SYNS_FriendlyAttrBoolean);
  FBooleanAttri.Foreground := clGreen;
  AddAttribute(FBooleanAttri);

  FSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clBlue;
  AddAttribute(FStringAttri);

  FSequenceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSequence, SYNS_FriendlyAttrSequence);
  FSequenceAttri.Foreground := clDkGray;
  AddAttribute(FSequenceAttri);

  FIndicatorAttri := TSynHighLighterAttributes.Create(SYNS_AttrIndicator, SYNS_FriendlyAttrIndicator);
  FIndicatorAttri.Foreground := clRed;
  AddAttribute(FIndicatorAttri);

  FTagAreaAttri := TSynHighLighterAttributes.Create(SYNS_AttrTagArea, SYNS_FriendlyAttrTagArea);
  FTagAreaAttri.Foreground := clMaroon;
  AddAttribute(FTagAreaAttri);

  FDebugLinesAttri := TSynHighLighterAttributes.Create(SYNS_AttrDebugLines, SYNS_FriendlyAttrDebugLines);
  FDebugLinesAttri.Foreground := clDkGray;
  AddAttribute(FDebugLinesAttri);
  SetAttributesOnChange(DefHighlightChange);

  FDefaultFilter := SYNS_FilterCOBOL;
  FRange := rsUnknown;
  FIndicator := #0;

  FCodeStartPos := 7;
  FCodeMediumPos := 11;
  FCodeEndPos := 71;

  EnumerateKeywords(Ord(tkBoolean), BooleanWords, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkKey), KeyWords, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkPreprocessor), PreprocessorWords, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkString), StringWords, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkUnknown), AmbigiousWords, IsIdentChar, DoAddKeyword);
end;

destructor TSynCobolSyn.Destroy;
begin
  FKeywords.Free;
  inherited Destroy;
end;

procedure TSynCobolSyn.IdentProc;
begin
  if CharInSet(FLine[Run], ['x', 'g', 'X', 'G'])
    and (Run < FCodeEndPos) and CharInSet(FLine[Run + 1], ['"', '''']) then
  begin
    Inc(Run);
    StringOpenProc;
  end
  else
  begin
    FTokenID := IdentKind((FLine + Run));
    if (FTokenID = tkIdentifier) and (Run < FCodeMediumPos) then
      FTokenID := tkAIdentifier;
    Inc(Run, FStringLen);

    while IsIdentChar(FLine[Run]) and (Run <= FCodeEndPos) do
      Inc(Run);
  end;
end;

procedure TSynCobolSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynCobolSyn.Next;
begin
  FTokenPos := Run;

  if FTokenPos < FCodeStartPos then
    FirstCharsProc
  else
    case FIndicator of
      '*', '/': CommentProc;
      'D', 'd': DebugProc;
      else
        if FTokenPos > FCodeEndPos then
          LastCharsProc
        else
          case FRange of
            rsQuoteString..rsApostStringMayBe: StringEndProc;
          else
            begin
              FRange := rsUnknown;
              NextProcedure;
            end;
          end;
    end;
  inherited;
end;

procedure TSynCobolSyn.NextProcedure;
begin
  case FLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    '"': StringOpenProc;
    '''': StringOpenProc;
    '=': StringOpenProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    '.': PointProc;
    '0'..'9': NumberProc;
    'A'..'Z', 'a'..'z': IdentProc;
    else UnknownProc;
  end;
end;

function TSynCobolSyn.GetDefaultAttribute(Index: Integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER:  Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynCobolSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynCobolSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynCobolSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkAIdentifier: Result := FAIdentifierAttri;
    tkPreprocessor: Result := FPreprocessorAttri;
    tkKey: Result := FKeyAttri;
    tkBoolean: Result := FBooleanAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSequence: Result := FSequenceAttri;
    tkIndicator: Result := FIndicatorAttri;
    tkTagArea: Result := FTagAreaAttri;
    tkDebugLines: Result := FDebugLinesAttri;
    tkUnknown: Result := FIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynCobolSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

function TSynCobolSyn.GetSampleSource: UnicodeString;
begin
  Result := '000100* This is a sample file to be used to show all TSynCobolSyn''s'#13#10 +
            '000200* features.'#13#10 +
            '000300* This isn''t a valid COBOL program.'#13#10 +
            '000400'#13#10 +
            '000500* 1. Supported COBOL features.'#13#10 +
            '000600'#13#10 +
            '000700* 1.1  Sequence area.'#13#10 +
            '000800*    First six columns in COBOL are reserved for enumeration'#13#10 +
            '000900*    of source lines.'#13#10 +
            '001000* 1.2  Indicator area.'#13#10 +
            '001100*    7th column in COBOL is reserved for special markers like ''*'''#13#10 +
            '001200*    or ''D''.'#13#10 +
            '001300* 1.3  Comment lines.'#13#10 +
            '001400*    Any line started from ''*'' in 7th column is a comment.'#13#10 +
            '001500*    No separate word highlighting will be done by the editor.'#13#10 +
            '001600* 1.4  Debug lines.'#13#10 +
            '001700D    Any line started from ''D'' will be treated as containing debug'#13#10 +
            '001800D    commands. No separate word highlighting will be done'#13#10 +
            '001900D    by the editor.'#13#10 +
            '002000* 1.5  Tag area.'#13#10 +
            '002100*    Only columns from 8th till 72th can be used for COBOL        TAG_AREA'#13#10 +
            '002200*    program. Columns beyond the 72th one may be used by some     TAG_AREA'#13#10 +
            '002300*    COBOL compilers to tag the code in some internal way.        TAG_AREA'#13#10 +
            '002400* 1.6  Area A identifiers.'#13#10 +
            '002500*    In area A (from 8th column till'#13#10 +
            '002600*    11th one) you should type only sections''/paragraphs'' names.'#13#10 +
            '002700*    For example "SOME" is a section name:'#13#10 +
            '002800 SOME SECTION.'#13#10 +
            '002900* 1.7  Preprocessor directives.'#13#10 +
            '003000*    For example "COPY" is a preprocessor directive:'#13#10 +
            '003100     COPY "PRD-DATA.SEL".'#13#10 +
            '003200* 1.8  Key words.'#13#10 +
            '003300*    For example "ACCEPT" and "AT" are COBOL key words:'#13#10 +
            '003400     ACCEPT WS-ENTRY AT 2030.'#13#10 +
            '003500* 1.9  Boolean constants.'#13#10 +
            '003600*    These are "TRUE" and "FALSE" constants. For example:'#13#10 +
            '003700     EVALUATE TRUE.'#13#10 +
            '003800* 1.10 Numbers.'#13#10 +
            '003900*    Here are the examples of numbers:'#13#10 +
            '004000 01  WSV-TEST-REC.'#13#10 +
            '004100     03  WSV-INT-T	       PIC 9(5) VALUE 12345.'#13#10 +
            '004200     03  WSV-PRICES              PIC 9(4)V99 COMP-3 VALUE 0000.33. 		'#13#10 +
            '004300     03  WSV-Z-PRICES            PIC Z(5)9.99- VALUE -2.12. 		'#13#10 +
            '004400     03  WSV-STORE-DATE          PIC 9(4)V99E99 VALUE 0001.33E02.'#13#10 +
            '004500* 1.11 Strings.'#13#10 +
            '004600*    The following types of strings are supported:'#13#10 +
            '004700*    1.11.1 Quoted strings.'#13#10 +
            '004800         MOVE "The name of field is ""PRODUCT""" TO WS-ERR-MESS.'#13#10 +
            '004900         MOVE ''The name of field is ''''PRODUCT'''''' TO WS-ERR-MESS.'#13#10 +
            '005000*    1.11.2 Pseudo-text.'#13#10 +
            '005100         COPY'#13#10 +
            '005200             REPLACING ==+00001== BY  +2'#13#10 +
            '005300                       == 1 ==    BY  -3.'#13#10 +
            '005400*    1.11.3 Figurative constants.'#13#10 +
            '005500*        For example "SPACES" is figurative constant:'#13#10 +
            '005600             DISPLAY SPACES UPON CRT.'#13#10 +
            '005700* 1.12 Continued lines.'#13#10 +
            '005800*    Only continued strings are supported. For example:'#13#10 +
            '005900         MOVE "The name of figurative constant field is'#13#10 +
            '006000-"SPACES" TO WS-ERR-MESS.'#13#10 +
            '006100*    Or (a single quotation mark in 72th column):'#13#10 +
            '005900         MOVE "The name of figurative constant field is  ""SPACES"'#13#10 +
            '006000-""" TO WS-ERR-MESS.'#13#10 +
            '006100'#13#10 +
            '006200* 2. Unsupported COBOL features.'#13#10 +
            '006300'#13#10 +
            '006400* 2.1 Continued lines.'#13#10 +
            '006500*    Continuation of key words is not supported. For example,'#13#10 +
            '006600*    the following COBOL code is valid but TSynCobolSyn won''t'#13#10 +
            '006700*    highlight "VALUE" keyword properly:'#13#10 +
            '006800     03  WSV-STORE-DATE                         PIC 9(4)V99E99 VAL'#13#10 +
            '006900-UE 0001.33E02.'#13#10 +
            '007000* 2.2 Identifiers started from digits.'#13#10 +
            '007100*    They are valid in COBOL but won''t be highlighted properly'#13#10 +
            '007200*    by TSynCobolSyn. For example, "000-main" is a paragraph'#13#10 +
            '007300*    name and should be highlighted as Area A identifier:'#13#10 +
            '007400 000-main.'#13#10 +
            '007500* 2.3 Comment entries in optional paragraphs'#13#10 +
            '007600*    The so called comment-entries in the optional paragraphs'#13#10 +
            '007700*    of the Identification Division are not supported and won''t'#13#10 +
            '007800*    be highlighted properly.';
end;

function TSynCobolSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterCOBOL;
end;

function TSynCobolSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '-', '0'..'9', 'a'..'z', 'A'..'Z':
      Result := True;
    else
      Result := False;              
  end;
end;

procedure TSynCobolSyn.SetCodeStartPos(Value: LongInt);
begin
  if Value < FCodeMediumPos then
    FCodeStartPos := Value
  else
    FCodeStartPos := FCodeMediumPos;
end;

procedure TSynCobolSyn.SetCodeMediumPos(Value: LongInt);
begin
  if (FCodeStartPos <= Value) and (Value <= FCodeEndPos) then
    FCodeMediumPos := Value
  else
    if Value > FCodeEndPos
    then FCodeMediumPos := FCodeEndPos
    else FCodeMediumPos := FCodeStartPos;
end;

procedure TSynCobolSyn.SetCodeEndPos(Value: LongInt);
begin
  if Value > FCodeMediumPos then
    FCodeEndPos := Value
  else
    FCodeEndPos := FCodeMediumPos;
end;

class function TSynCobolSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCOBOL;
end;

procedure TSynCobolSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynCobolSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynCobolSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

class function TSynCobolSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangCOBOL;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynCobolSyn);
{$ENDIF}
end.
