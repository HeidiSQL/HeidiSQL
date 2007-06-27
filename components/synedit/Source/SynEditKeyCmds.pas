{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditKeyCmds.pas, released 2000-04-07.
The Original Code is based on the mwKeyCmds.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Brad Stowers.
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

$Id: SynEditKeyCmds.pas,v 1.14 2002/04/08 21:40:21 jrx Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditKeyCmds;

{$I SynEdit.inc}

interface

uses
  Classes,
{$IFDEF SYN_CLX}
  QMenus,
{$ELSE}
  Menus,
{$ENDIF}
  SysUtils;

const
  //****************************************************************************
  // NOTE!  If you add an editor command, you must also update the
  //    EditorCommandStrs constant array in implementation section below, or the
  //    command will not show up in the IDE.
  //****************************************************************************

  // "Editor Commands".  Key strokes are translated from a table into these
  // I used constants instead of a set so that additional commands could be
  // added in descendants (you can't extend a set)

  // There are two ranges of editor commands: the ecViewXXX commands are always
  // valid, while the ecEditXXX commands are ignored when the editor is in
  // read-only mode

  ecNone             =    0; // Nothing. Useful for user event to handle command
  ecViewCommandFirst =    0;
  ecViewCommandLast  =  500;
  ecEditCommandFirst =  501;
  ecEditCommandLast  = 1000;

  ecLeft            = 1;    // Move cursor left one char
  ecRight           = 2;    // Move cursor right one char
  ecUp              = 3;    // Move cursor up one line
  ecDown            = 4;    // Move cursor down one line
  ecWordLeft        = 5;    // Move cursor left one word
  ecWordRight       = 6;    // Move cursor right one word
  ecLineStart       = 7;    // Move cursor to beginning of line
  ecLineEnd         = 8;    // Move cursor to end of line
  ecPageUp          = 9;    // Move cursor up one page
  ecPageDown        = 10;   // Move cursor down one page
  ecPageLeft        = 11;   // Move cursor right one page
  ecPageRight       = 12;   // Move cursor left one page
  ecPageTop         = 13;   // Move cursor to top of page
  ecPageBottom      = 14;   // Move cursor to bottom of page
  ecEditorTop       = 15;   // Move cursor to absolute beginning
  ecEditorBottom    = 16;   // Move cursor to absolute end
  ecGotoXY          = 17;   // Move cursor to specific coordinates, Data = PPoint

//******************************************************************************
// Maybe the command processor should just take a boolean that signifies if
// selection is affected or not?
//******************************************************************************

  ecSelection       = 100;  // Add this to ecXXX command to get equivalent
                            // command, but with selection enabled. This is not
                            // a command itself.
  // Same as commands above, except they affect selection, too
  ecSelLeft         = ecLeft + ecSelection;
  ecSelRight        = ecRight + ecSelection;
  ecSelUp           = ecUp + ecSelection;
  ecSelDown         = ecDown + ecSelection;
  ecSelWordLeft     = ecWordLeft + ecSelection;
  ecSelWordRight    = ecWordRight + ecSelection;
  ecSelLineStart    = ecLineStart + ecSelection;
  ecSelLineEnd      = ecLineEnd + ecSelection;
  ecSelPageUp       = ecPageUp + ecSelection;
  ecSelPageDown     = ecPageDown + ecSelection;
  ecSelPageLeft     = ecPageLeft + ecSelection;
  ecSelPageRight    = ecPageRight + ecSelection;
  ecSelPageTop      = ecPageTop + ecSelection;
  ecSelPageBottom   = ecPageBottom + ecSelection;
  ecSelEditorTop    = ecEditorTop + ecSelection;
  ecSelEditorBottom = ecEditorBottom + ecSelection;
  ecSelGotoXY       = ecGotoXY + ecSelection;  // Data = PPoint

  ecSelectAll       = 199;  // Select entire contents of editor, cursor to end

  ecCopy            = 201;  // Copy selection to clipboard

  ecScrollUp        = 211;  // Scroll up one line leaving cursor position unchanged.
  ecScrollDown      = 212;  // Scroll down one line leaving cursor position unchanged.
  ecScrollLeft      = 213;  // Scroll left one char leaving cursor position unchanged.
  ecScrollRight     = 214;  // Scroll right one char leaving cursor position unchanged.

  ecInsertMode      = 221;  // Set insert mode
  ecOverwriteMode   = 222;  // Set overwrite mode
  ecToggleMode      = 223;  // Toggle ins/ovr mode

  ecNormalSelect    = 231;  // Normal selection mode
  ecColumnSelect    = 232;  // Column selection mode
  ecLineSelect      = 233;  // Line selection mode

  ecMatchBracket    = 250;  // Go to matching bracket

  ecGotoMarker0     = 301;  // Goto marker
  ecGotoMarker1     = 302;  // Goto marker
  ecGotoMarker2     = 303;  // Goto marker
  ecGotoMarker3     = 304;  // Goto marker
  ecGotoMarker4     = 305;  // Goto marker
  ecGotoMarker5     = 306;  // Goto marker
  ecGotoMarker6     = 307;  // Goto marker
  ecGotoMarker7     = 308;  // Goto marker
  ecGotoMarker8     = 309;  // Goto marker
  ecGotoMarker9     = 310;  // Goto marker
  ecSetMarker0      = 351;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker1      = 352;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker2      = 353;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker3      = 354;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker4      = 355;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker5      = 356;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker6      = 357;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker7      = 358;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker8      = 359;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker9      = 360;  // Set marker, Data = PPoint - X, Y Pos

  ecContextHelp     = 490;  // Help on Word, Data = Word

  ecDeleteLastChar  = 501;  // Delete last char (i.e. backspace key)
  ecDeleteChar      = 502;  // Delete char at cursor (i.e. delete key)
  ecDeleteWord      = 503;  // Delete from cursor to end of word
  ecDeleteLastWord  = 504;  // Delete from cursor to start of word
  ecDeleteBOL       = 505;  // Delete from cursor to beginning of line
  ecDeleteEOL       = 506;  // Delete from cursor to end of line
  ecDeleteLine      = 507;  // Delete current line
  ecClearAll        = 508;  // Delete everything
  ecLineBreak       = 509;  // Break line at current position, move caret to new line
  ecInsertLine      = 510;  // Break line at current position, leave caret
  ecChar            = 511;  // Insert a character at current position

  ecImeStr          = 550;  // Insert character(s) from IME

  ecUndo            = 601;  // Perform undo if available
  ecRedo            = 602;  // Perform redo if available
  ecCut             = 603;  // Cut selection to clipboard
  ecPaste           = 604;  // Paste clipboard to current position

  ecBlockIndent     = 610;  // Indent selection
  ecBlockUnindent   = 611;  // Unindent selection
  ecTab             = 612;  // Tab key
  ecShiftTab        = 613;  // Shift+Tab key

  ecAutoCompletion  = 650;

  ecUpperCase       = 620;
  ecLowerCase       = 621;
  ecToggleCase      = 622;
  ecTitleCase       = 623;

  ecString          = 630;  //Insert a whole string

  ecGotFocus        = 700;
  ecLostFocus       = 701;

  ecUserFirst       = 1001; // Start of user-defined commands

type
  ESynKeyError = class(Exception);

  TSynEditorCommand = type word;

  TSynEditKeyStroke = class(TCollectionItem)
  private
    FKey: word;          // Virtual keycode, i.e. VK_xxx
    FShift: TShiftState;
    FKey2: word;
    FShift2: TShiftState;
    FCommand: TSynEditorCommand;
    function GetShortCut: TShortCut;
    function GetShortCut2: TShortCut;
    procedure SetCommand(const Value: TSynEditorCommand);
    procedure SetKey(const Value: word);
    procedure SetKey2(const Value: word);
    procedure SetShift(const Value: TShiftState);
    procedure SetShift2(const Value: TShiftState);
    procedure SetShortCut(const Value: TShortCut);
    procedure SetShortCut2(const Value: TShortCut);
  protected
{$IFDEF SYN_COMPILER_3_UP}
    function GetDisplayName: string; override;
{$ENDIF}
  public
    procedure Assign(Source: TPersistent); override;
{begin}                                                                         //ac 2000-07-05
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
{end}                                                                           //ac 2000-07-05
    // No duplicate checking is done if assignment made via these properties!
    property Key: word read FKey write SetKey;
    property Key2: word read FKey2 write SetKey2;
    property Shift: TShiftState read FShift write SetShift;
    property Shift2: TShiftState read FShift2 write SetShift2;
  published
    property Command: TSynEditorCommand read FCommand write SetCommand;
    property ShortCut: TShortCut read GetShortCut write SetShortCut
      default 0;                                                                //mh 2000-11-07
    property ShortCut2: TShortCut read GetShortCut2 write SetShortCut2
      default 0;                                                                //mh 2000-11-07
  end;

  TSynEditKeyStrokes = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TSynEditKeyStroke;
    procedure SetItem(Index: Integer; Value: TSynEditKeyStroke);
  protected
{$IFDEF SYN_COMPILER_3_UP}
    function GetOwner: TPersistent; override;
{$ENDIF}
  public
    constructor Create(AOwner: TPersistent);
    function Add: TSynEditKeyStroke;
    procedure Assign(Source: TPersistent); override;
    function FindCommand(Cmd: TSynEditorCommand): integer;
    function FindKeycode(Code: word; SS: TShiftState): integer;
    function FindKeycode2(Code1: word; SS1: TShiftState;
      Code2: word; SS2: TShiftState): integer;
    function FindShortcut(SC: TShortcut): integer;
    function FindShortcut2(SC, SC2: TShortcut): integer;
    procedure LoadFromStream(AStream: TStream);                                 //ac 2000-07-05
    procedure ResetDefaults;
    procedure SaveToStream(AStream: TStream);                                   //ac 2000-07-05
  public
    property Items[Index: Integer]: TSynEditKeyStroke read GetItem
      write SetItem; default;
  end;

// These are mainly for the TSynEditorCommand property editor, but could be
// useful elsewhere.
function EditorCommandToDescrString(Cmd: TSynEditorCommand): string;
function EditorCommandToCodeString(Cmd: TSynEditorCommand): string;
procedure GetEditorCommandValues(Proc: TGetStrProc);
procedure GetEditorCommandExtended(Proc: TGetStrProc);
function IdentToEditorCommand(const Ident: string; var Cmd: longint): boolean;
function EditorCommandToIdent(Cmd: longint; var Ident: string): boolean;
function ConvertCodeStringToExtended(AString : String) : String;
function ConvertExtendedToCodeString(AString : String) : String;
function ConvertExtendedToCommand(AString : String) : TSynEditorCommand;
function ConvertCodeStringToCommand(AString : String) : TSynEditorCommand;
function IndexToEditorCommand(const AIndex: Integer) : Integer;

implementation

uses
{$IFDEF SYN_CLX}
  kTextDrawer,
  Types,
  Qt,
{$ELSE}
  Windows,
{$ENDIF}
  SynEditStrConst;

{ Command mapping routines }

{$IFDEF SYN_COMPILER_2}
// This is defined in D3/C3 and up.
type
  TIdentMapEntry = record
    Value: TSynEditorCommand;
    Name: string;
  end;
{$ENDIF}

const
  EditorCommandStrs: array[0..95] of TIdentMapEntry = (
    (Value: ecNone; Name: 'ecNone'),
    (Value: ecLeft; Name: 'ecLeft'),
    (Value: ecRight; Name: 'ecRight'),
    (Value: ecUp; Name: 'ecUp'),
    (Value: ecDown; Name: 'ecDown'),
    (Value: ecWordLeft; Name: 'ecWordLeft'),
    (Value: ecWordRight; Name: 'ecWordRight'),
    (Value: ecLineStart; Name: 'ecLineStart'),
    (Value: ecLineEnd; Name: 'ecLineEnd'),
    (Value: ecPageUp; Name: 'ecPageUp'),
    (Value: ecPageDown; Name: 'ecPageDown'),
    (Value: ecPageLeft; Name: 'ecPageLeft'),
    (Value: ecPageRight; Name: 'ecPageRight'),
    (Value: ecPageTop; Name: 'ecPageTop'),
    (Value: ecPageBottom; Name: 'ecPageBottom'),
    (Value: ecEditorTop; Name: 'ecEditorTop'),
    (Value: ecEditorBottom; Name: 'ecEditorBottom'),
    (Value: ecGotoXY; Name: 'ecGotoXY'),
    (Value: ecSelLeft; Name: 'ecSelLeft'),
    (Value: ecSelRight; Name: 'ecSelRight'),
    (Value: ecSelUp; Name: 'ecSelUp'),
    (Value: ecSelDown; Name: 'ecSelDown'),
    (Value: ecSelWordLeft; Name: 'ecSelWordLeft'),
    (Value: ecSelWordRight; Name: 'ecSelWordRight'),
    (Value: ecSelLineStart; Name: 'ecSelLineStart'),
    (Value: ecSelLineEnd; Name: 'ecSelLineEnd'),
    (Value: ecSelPageUp; Name: 'ecSelPageUp'),
    (Value: ecSelPageDown; Name: 'ecSelPageDown'),
    (Value: ecSelPageLeft; Name: 'ecSelPageLeft'),
    (Value: ecSelPageRight; Name: 'ecSelPageRight'),
    (Value: ecSelPageTop; Name: 'ecSelPageTop'),
    (Value: ecSelPageBottom; Name: 'ecSelPageBottom'),
    (Value: ecSelEditorTop; Name: 'ecSelEditorTop'),
    (Value: ecSelEditorBottom; Name: 'ecSelEditorBottom'),
    (Value: ecSelGotoXY; Name: 'ecSelGotoXY'),
    (Value: ecSelectAll; Name: 'ecSelectAll'),
    (Value: ecDeleteLastChar; Name: 'ecDeleteLastChar'),
    (Value: ecDeleteChar; Name: 'ecDeleteChar'),
    (Value: ecDeleteWord; Name: 'ecDeleteWord'),
    (Value: ecDeleteLastWord; Name: 'ecDeleteLastWord'),
    (Value: ecDeleteBOL; Name: 'ecDeleteBOL'),
    (Value: ecDeleteEOL; Name: 'ecDeleteEOL'),
    (Value: ecDeleteLine; Name: 'ecDeleteLine'),
    (Value: ecClearAll; Name: 'ecClearAll'),
    (Value: ecLineBreak; Name: 'ecLineBreak'),
    (Value: ecInsertLine; Name: 'ecInsertLine'),
    (Value: ecChar; Name: 'ecChar'),
    (Value: ecImeStr; Name: 'ecImeStr'),
    (Value: ecUndo; Name: 'ecUndo'),
    (Value: ecRedo; Name: 'ecRedo'),
    (Value: ecCut; Name: 'ecCut'),
    (Value: ecCopy; Name: 'ecCopy'),
    (Value: ecPaste; Name: 'ecPaste'),
    (Value: ecScrollUp; Name: 'ecScrollUp'),
    (Value: ecScrollDown; Name: 'ecScrollDown'),
    (Value: ecScrollLeft; Name: 'ecScrollLeft'),
    (Value: ecScrollRight; Name: 'ecScrollRight'),
    (Value: ecInsertMode; Name: 'ecInsertMode'),
    (Value: ecOverwriteMode; Name: 'ecOverwriteMode'),
    (Value: ecToggleMode; Name: 'ecToggleMode'),
    (Value: ecBlockIndent; Name: 'ecBlockIndent'),
    (Value: ecBlockUnindent; Name: 'ecBlockUnindent'),
    (Value: ecTab; Name: 'ecTab'),
    (Value: ecShiftTab; Name: 'ecShiftTab'),
    (Value: ecMatchBracket; Name: 'ecMatchBracket'),
    (Value: ecNormalSelect; Name: 'ecNormalSelect'),
    (Value: ecColumnSelect; Name: 'ecColumnSelect'),
    (Value: ecLineSelect; Name: 'ecLineSelect'),
    (Value: ecAutoCompletion; Name: 'ecAutoCompletion'),
    (Value: ecUserFirst; Name: 'ecUserFirst'),
    (Value: ecContextHelp; Name: 'ecContextHelp'),				// jj 2001-07-19
    (Value: ecGotoMarker0; Name: 'ecGotoMarker0'),
    (Value: ecGotoMarker1; Name: 'ecGotoMarker1'),
    (Value: ecGotoMarker2; Name: 'ecGotoMarker2'),
    (Value: ecGotoMarker3; Name: 'ecGotoMarker3'),
    (Value: ecGotoMarker4; Name: 'ecGotoMarker4'),
    (Value: ecGotoMarker5; Name: 'ecGotoMarker5'),
    (Value: ecGotoMarker6; Name: 'ecGotoMarker6'),
    (Value: ecGotoMarker7; Name: 'ecGotoMarker7'),
    (Value: ecGotoMarker8; Name: 'ecGotoMarker8'),
    (Value: ecGotoMarker9; Name: 'ecGotoMarker9'),
    (Value: ecSetMarker0; Name: 'ecSetMarker0'),
    (Value: ecSetMarker1; Name: 'ecSetMarker1'),
    (Value: ecSetMarker2; Name: 'ecSetMarker2'),
    (Value: ecSetMarker3; Name: 'ecSetMarker3'),
    (Value: ecSetMarker4; Name: 'ecSetMarker4'),
    (Value: ecSetMarker5; Name: 'ecSetMarker5'),
    (Value: ecSetMarker6; Name: 'ecSetMarker6'),
    (Value: ecSetMarker7; Name: 'ecSetMarker7'),
    (Value: ecSetMarker8; Name: 'ecSetMarker8'),
    (Value: ecSetMarker9; Name: 'ecSetMarker9'),
    (Value: ecUpperCase; Name: 'ecUpperCase'),
    (Value: ecLowerCase; Name: 'ecLowerCase'),
    (Value: ecToggleCase; Name: 'ecToggleCase'),
    (Value: ecTitleCase; Name: 'ecTitleCase'),
    (Value: ecString; Name:'ecString'));


procedure GetEditorCommandValues(Proc: TGetStrProc);
var
  i: integer;
begin
  for i := Low(EditorCommandStrs) to High(EditorCommandStrs) do
    Proc(EditorCommandStrs[I].Name);
end;

procedure GetEditorCommandExtended(Proc: TGetStrProc);
var
  i: integer;
begin
  for i := Low(EditorCommandStrs) to High(EditorCommandStrs) do
    Proc(ConvertCodeStringToExtended(EditorCommandStrs[I].Name));
end;

function IdentToEditorCommand(const Ident: string; var Cmd: longint): boolean;
{$IFDEF SYN_COMPILER_2}
var
  I: Integer;
{$ENDIF}
begin
{$IFDEF SYN_COMPILER_2}
  Result := FALSE;
  for I := Low(EditorCommandStrs) to High(EditorCommandStrs) do
    if CompareText(EditorCommandStrs[I].Name, Ident) = 0 then
    begin
      Result := TRUE;
      Cmd := EditorCommandStrs[I].Value;
      break;
    end;
{$ELSE}
    Result := IdentToInt(Ident, Cmd, EditorCommandStrs);
{$ENDIF}
end;

function EditorCommandToIdent(Cmd: longint; var Ident: string): boolean;
{$IFDEF SYN_COMPILER_2}
var
  I: Integer;
{$ENDIF}
begin
{$IFDEF SYN_COMPILER_2}
  Result := FALSE;
  for I := Low(EditorCommandStrs) to High(EditorCommandStrs) do
    if EditorCommandStrs[I].Value = Cmd then
    begin
      Result := TRUE;
      Ident := EditorCommandStrs[I].Name;
      break;
    end;
{$ELSE}
  Result := IntToIdent(Cmd, Ident, EditorCommandStrs);
{$ENDIF}
end;

function EditorCommandToDescrString(Cmd: TSynEditorCommand): string;
begin
  // Doesn't do anything yet.
  Result := '';
end;

function EditorCommandToCodeString(Cmd: TSynEditorCommand): string;
begin
  if not EditorCommandToIdent(Cmd, Result) then
    Result := IntToStr(Cmd);
end;

{ TSynEditKeyStroke }

procedure TSynEditKeyStroke.Assign(Source: TPersistent);
begin
  if Source is TSynEditKeyStroke then
  begin
    Command := TSynEditKeyStroke(Source).Command;
    Key := TSynEditKeyStroke(Source).Key;
    Key2 := TSynEditKeyStroke(Source).Key2;
    Shift := TSynEditKeyStroke(Source).Shift;
    Shift2 := TSynEditKeyStroke(Source).Shift2;
  end else
    inherited Assign(Source);
end;

{$IFDEF SYN_COMPILER_3_UP}
function TSynEditKeyStroke.GetDisplayName: string;
begin
  Result := EditorCommandToCodeString(Command) + ' - ' + ShortCutToText(ShortCut);
  if ShortCut <> 0 then
    Result := Result + ' ' + ShortCutToText(ShortCut2);
  if Result = '' then
    Result := inherited GetDisplayName;
end;
{$ENDIF}

function TSynEditKeyStroke.GetShortCut: TShortCut;
begin
{$IFDEF SYN_CLX}
  Result := QMenus.ShortCut(Key, Shift);
{$ELSE}
  Result := Menus.ShortCut(Key, Shift);
{$ENDIF}
end;

procedure TSynEditKeyStroke.SetCommand(const Value: TSynEditorCommand);
begin
  if Value <> FCommand then
    FCommand := Value;
end;

procedure TSynEditKeyStroke.SetKey(const Value: word);
begin
  if Value <> FKey then
    FKey := Value;
end;

procedure TSynEditKeyStroke.SetShift(const Value: TShiftState);
begin
  if Value <> FShift then
    FShift := Value;
end;

procedure TSynEditKeyStroke.SetShortCut(const Value: TShortCut);
var
  NewKey: Word;
  NewShift: TShiftState;
  Dup: integer;
begin
  // Duplicate values of no shortcut are OK.
  if Value <> 0 then
  begin
    // Check for duplicate shortcut in the collection and disallow if there is.
    Dup := TSynEditKeyStrokes(Collection).FindShortcut2(Value, Key2);
    if (Dup <> -1) and (Dup <> Self.Index) then
      begin
      raise ESynKeyError.Create(SYNS_EDuplicateShortCut);
      end;
  end;

{$IFDEF SYN_CLX}  //js 06-04-2002 use qmenus, not menus in clx
  QMenus.ShortCutToKey(Value, NewKey, NewShift);
{$ELSE}
  Menus.ShortCutToKey(Value, NewKey, NewShift);
{$ENDIF}

  if (NewKey <> Key) or (NewShift <> Shift) then
  begin
    Key := NewKey;
    Shift := NewShift;
  end;
end;

procedure TSynEditKeyStroke.SetKey2(const Value: word);
begin
  if Value <> FKey2 then
    FKey2 := Value;
end;

procedure TSynEditKeyStroke.SetShift2(const Value: TShiftState);
begin
  if Value <> FShift2 then
    FShift2 := Value;
end;

procedure TSynEditKeyStroke.SetShortCut2(const Value: TShortCut);
var
  NewKey: Word;
  NewShift: TShiftState;
  Dup: integer;
begin
  // Duplicate values of no shortcut are OK.
  if Value <> 0 then
  begin
    // Check for duplicate shortcut in the collection and disallow if there is.
    Dup := TSynEditKeyStrokes(Collection).FindShortcut2(Key, Value);
    if (Dup <> -1) and (Dup <> Self.Index) then
      raise ESynKeyError.Create(SYNS_EDuplicateShortCut);
  end;

{$IFDEF SYN_CLX}
  QMenus.ShortCutToKey(Value, NewKey, NewShift);
{$ELSE}
  Menus.ShortCutToKey(Value, NewKey, NewShift);
{$ENDIF}
  if (NewKey <> Key2) or (NewShift <> Shift2) then
  begin
    Key2 := NewKey;
    Shift2 := NewShift;
  end;
end;

function TSynEditKeyStroke.GetShortCut2: TShortCut;
begin
{$IFDEF SYN_CLX}
  Result := QMenus.ShortCut(Key2, Shift2);
{$ELSE}
  Result := Menus.ShortCut(Key2, Shift2);
{$ENDIF}
end;

{begin}                                                                         //ac 2000-07-05
procedure TSynEditKeyStroke.LoadFromStream(AStream: TStream);
begin
  with AStream do begin
    Read(fKey, SizeOf(fKey));
    Read(fShift, SizeOf(fShift));
    Read(fKey2, SizeOf(fKey2));
    Read(fShift2, SizeOf(fShift2));
    Read(fCommand, SizeOf(fCommand));
  end;
end;

procedure TSynEditKeyStroke.SaveToStream(AStream: TStream);
begin
  with AStream do begin
    Write(fKey, SizeOf(fKey));
    Write(fShift, SizeOf(fShift));
    Write(fKey2, SizeOf(fKey2));
    Write(fShift2, SizeOf(fShift2));
    Write(fCommand, SizeOf(fCommand));
  end;
end;
{end}                                                                           //ac 2000-07-05

{ TSynEditKeyStrokes }

function TSynEditKeyStrokes.Add: TSynEditKeyStroke;
begin
  Result := TSynEditKeyStroke(inherited Add);
end;

procedure TSynEditKeyStrokes.Assign(Source: TPersistent);
var
  x: integer;
begin
  if Source is TSynEditKeyStrokes then
  begin
    Clear;
    for x := 0 to TSynEditKeyStrokes(Source).Count-1 do
    begin
      with Add do
        Assign(TSynEditKeyStrokes(Source)[x]);
    end;
  end else
    inherited Assign(Source);
end;

constructor TSynEditKeyStrokes.Create(AOwner: TPersistent);
begin
  inherited Create(TSynEditKeyStroke);
  FOwner := AOwner;
end;

function TSynEditKeyStrokes.FindCommand(Cmd: TSynEditorCommand): integer;
var
  x: integer;
begin
  Result := -1;
  for x := 0 to Count-1 do
    if Items[x].Command = Cmd then
    begin
      Result := x;
      break;
    end;
end;

function TSynEditKeyStrokes.FindKeycode(Code: word; SS: TShiftState): integer;
var
  x: integer;
begin
  Result := -1;
  for x := 0 to Count-1 do
    if (Items[x].Key = Code) and (Items[x].Shift = SS) and (Items[x].Key2 = 0)
    then begin
      Result := x;
      break;
    end;
end;

function TSynEditKeyStrokes.FindKeycode2(Code1: word; SS1: TShiftState;
  Code2: word; SS2: TShiftState): integer;
var
  x: integer;
begin
  Result := -1;
  for x := 0 to Count-1 do
    if (Items[x].Key = Code1) and (Items[x].Shift = SS1) and
       (Items[x].Key2 = Code2) and (Items[x].Shift2 = SS2) then
    begin
      Result := x;
      break;
    end;
end;

function TSynEditKeyStrokes.FindShortcut(SC: TShortcut): integer;
var
  x: integer;
begin
  Result := -1;
  for x := 0 to Count-1 do
    if Items[x].Shortcut = SC then
    begin
      Result := x;
      break;
    end;
end;

function TSynEditKeyStrokes.FindShortcut2(SC, SC2: TShortcut): integer;
var
  x: integer;
begin
  Result := -1;
  for x := 0 to Count-1 do
    if (Items[x].Shortcut = SC) and (Items[x].Shortcut2 = SC2) then
    begin
      Result := x;
      break;
    end;
end;

function TSynEditKeyStrokes.GetItem(Index: Integer): TSynEditKeyStroke;
begin
 Result := TSynEditKeyStroke(inherited GetItem(Index));
end;

{$IFDEF SYN_COMPILER_3_UP}
function TSynEditKeyStrokes.GetOwner: TPersistent;
begin
  Result := FOwner;
end;
{$ENDIF}

{begin}                                                                         //ac 2000-07-05
procedure TSynEditKeyStrokes.LoadFromStream(AStream: TStream);
var
  Num: integer;
begin
  Clear;
  AStream.Read(Num, SizeOf(Num));
  while Num > 0 do begin
    with Add do
      LoadFromStream(AStream);
    Dec(Num);
  end;
end;
{end}                                                                           //ac 2000-07-05

procedure TSynEditKeyStrokes.ResetDefaults;

  procedure AddKey(const ACmd: TSynEditorCommand; const AKey: word;
     const AShift: TShiftState);
  begin
    with Add do
    begin
      Key := AKey;
      Shift := AShift;
      Command := ACmd;
    end;
  end;

begin
  Clear;

{$IFDEF SYN_CLX}
  AddKey(ecUp, Key_UP, []);
  AddKey(ecSelUp, Key_UP, [ssShift]);
  AddKey(ecScrollUp, Key_UP, [ssCtrl]);
  AddKey(ecDown, Key_DOWN, []);
  AddKey(ecSelDown, Key_DOWN, [ssShift]);
  AddKey(ecScrollDown, Key_DOWN, [ssCtrl]);
  AddKey(ecLeft, Key_LEFT, []);
  AddKey(ecSelLeft, Key_LEFT, [ssShift]);
  AddKey(ecWordLeft, Key_LEFT, [ssCtrl]);
  AddKey(ecSelWordLeft, Key_LEFT, [ssShift,ssCtrl]);
  AddKey(ecRight, Key_RIGHT, []);
  AddKey(ecSelRight, Key_RIGHT, [ssShift]);
  AddKey(ecWordRight, Key_RIGHT, [ssCtrl]);
  AddKey(ecSelWordRight, Key_RIGHT, [ssShift,ssCtrl]);
  AddKey(ecPageDown, Key_NEXT, []);
  AddKey(ecSelPageDown, Key_NEXT, [ssShift]);
  AddKey(ecPageBottom, Key_NEXT, [ssCtrl]);
  AddKey(ecSelPageBottom, Key_NEXT, [ssShift,ssCtrl]);
  AddKey(ecPageUp, Key_PRIOR, []);
  AddKey(ecSelPageUp, Key_PRIOR, [ssShift]);
  AddKey(ecPageTop, Key_PRIOR, [ssCtrl]);
  AddKey(ecSelPageTop, Key_PRIOR, [ssShift,ssCtrl]);
  AddKey(ecLineStart, Key_HOME, []);
  AddKey(ecSelLineStart, Key_HOME, [ssShift]);
  AddKey(ecEditorTop, Key_HOME, [ssCtrl]);
  AddKey(ecSelEditorTop, Key_HOME, [ssShift,ssCtrl]);
  AddKey(ecLineEnd, Key_END, []);
  AddKey(ecSelLineEnd, Key_END, [ssShift]);
  AddKey(ecEditorBottom, Key_END, [ssCtrl]);
  AddKey(ecSelEditorBottom, Key_END, [ssShift,ssCtrl]);
  AddKey(ecToggleMode, Key_INSERT, []);
  AddKey(ecCopy, Key_INSERT, [ssCtrl]);
  AddKey(ecPaste, Key_INSERT, [ssShift]);
  AddKey(ecDeleteChar, Key_DELETE, []);
  AddKey(ecCut, Key_DELETE, [ssShift]);
  AddKey(ecDeleteLastChar, Key_BACKSpace, []);
  AddKey(ecDeleteLastWord, Key_BACKSpace, [ssCtrl]);
  AddKey(ecUndo, Key_BACKSpace, [ssAlt]);
  AddKey(ecRedo, Key_BACKSpace, [ssAlt,ssShift]);
  AddKey(ecLineBreak, Key_RETURN, []);
//js:this is already defined after endif::  AddKey(ecInsertLine, ord('N'), [ssCtrl]);
  AddKey(ecTab, Key_TAB, []);
  AddKey(ecShiftTab, Key_TAB, [ssShift]);
{$ELSE}
  AddKey(ecUp, VK_UP, []);
  AddKey(ecSelUp, VK_UP, [ssShift]);
  AddKey(ecScrollUp, VK_UP, [ssCtrl]);
  AddKey(ecDown, VK_DOWN, []);
  AddKey(ecSelDown, VK_DOWN, [ssShift]);
  AddKey(ecScrollDown, VK_DOWN, [ssCtrl]);
  AddKey(ecLeft, VK_LEFT, []);
  AddKey(ecSelLeft, VK_LEFT, [ssShift]);
  AddKey(ecWordLeft, VK_LEFT, [ssCtrl]);
  AddKey(ecSelWordLeft, VK_LEFT, [ssShift,ssCtrl]);
  AddKey(ecRight, VK_RIGHT, []);
  AddKey(ecSelRight, VK_RIGHT, [ssShift]);
  AddKey(ecWordRight, VK_RIGHT, [ssCtrl]);
  AddKey(ecSelWordRight, VK_RIGHT, [ssShift,ssCtrl]);
  AddKey(ecPageDown, VK_NEXT, []);
  AddKey(ecSelPageDown, VK_NEXT, [ssShift]);
  AddKey(ecPageBottom, VK_NEXT, [ssCtrl]);
  AddKey(ecSelPageBottom, VK_NEXT, [ssShift,ssCtrl]);
  AddKey(ecPageUp, VK_PRIOR, []);
  AddKey(ecSelPageUp, VK_PRIOR, [ssShift]);
  AddKey(ecPageTop, VK_PRIOR, [ssCtrl]);
  AddKey(ecSelPageTop, VK_PRIOR, [ssShift,ssCtrl]);
  AddKey(ecLineStart, VK_HOME, []);
  AddKey(ecSelLineStart, VK_HOME, [ssShift]);
  AddKey(ecEditorTop, VK_HOME, [ssCtrl]);
  AddKey(ecSelEditorTop, VK_HOME, [ssShift,ssCtrl]);
  AddKey(ecLineEnd, VK_END, []);
  AddKey(ecSelLineEnd, VK_END, [ssShift]);
  AddKey(ecEditorBottom, VK_END, [ssCtrl]);
  AddKey(ecSelEditorBottom, VK_END, [ssShift,ssCtrl]);
  AddKey(ecToggleMode, VK_INSERT, []);
  AddKey(ecCopy, VK_INSERT, [ssCtrl]);
  AddKey(ecCut, VK_DELETE, [ssShift]);
  AddKey(ecPaste, VK_INSERT, [ssShift]);
  AddKey(ecDeleteChar, VK_DELETE, []);
  AddKey(ecDeleteLastChar, VK_BACK, []);
  AddKey(ecDeleteLastChar, VK_BACK, [ssShift]);                                 //jr 2000-09-23
  AddKey(ecDeleteLastWord, VK_BACK, [ssCtrl]);
  AddKey(ecUndo, VK_BACK, [ssAlt]);
  AddKey(ecRedo, VK_BACK, [ssAlt,ssShift]);
  AddKey(ecLineBreak, VK_RETURN, []);
  AddKey(ecLineBreak, VK_RETURN, [ssShift]);                                    //jr 2001-07-24
  AddKey(ecTab, VK_TAB, []);
  AddKey(ecShiftTab, VK_TAB, [ssShift]);
  AddKey(ecContextHelp, VK_F1, [ssCtrl]);                                       // jj 2001-07-19
{$ENDIF}
  AddKey(ecSelectAll, ord('A'), [ssCtrl]);
  AddKey(ecCopy, ord('C'), [ssCtrl]);
  AddKey(ecPaste, ord('V'), [ssCtrl]);
  AddKey(ecCut, ord('X'), [ssCtrl]);
  AddKey(ecBlockIndent, ord('I'), [ssCtrl,ssShift]);
  AddKey(ecBlockUnindent, ord('U'), [ssCtrl,ssShift]);
  AddKey(ecLineBreak, ord('M'), [ssCtrl]);
  AddKey(ecInsertLine, ord('N'), [ssCtrl]);
  AddKey(ecDeleteWord, ord('T'), [ssCtrl]);
  AddKey(ecDeleteLine, ord('Y'), [ssCtrl]);
  AddKey(ecDeleteEOL, ord('Y'), [ssCtrl,ssShift]);
  AddKey(ecUndo, ord('Z'), [ssCtrl]);
  AddKey(ecRedo, ord('Z'), [ssCtrl,ssShift]);
  AddKey(ecGotoMarker0, ord('0'), [ssCtrl]);
  AddKey(ecGotoMarker1, ord('1'), [ssCtrl]);
  AddKey(ecGotoMarker2, ord('2'), [ssCtrl]);
  AddKey(ecGotoMarker3, ord('3'), [ssCtrl]);
  AddKey(ecGotoMarker4, ord('4'), [ssCtrl]);
  AddKey(ecGotoMarker5, ord('5'), [ssCtrl]);
  AddKey(ecGotoMarker6, ord('6'), [ssCtrl]);
  AddKey(ecGotoMarker7, ord('7'), [ssCtrl]);
  AddKey(ecGotoMarker8, ord('8'), [ssCtrl]);
  AddKey(ecGotoMarker9, ord('9'), [ssCtrl]);
  AddKey(ecSetMarker0, ord('0'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker1, ord('1'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker2, ord('2'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker3, ord('3'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker4, ord('4'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker5, ord('5'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker6, ord('6'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker7, ord('7'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker8, ord('8'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker9, ord('9'), [ssCtrl,ssShift]);
  AddKey(ecNormalSelect, ord('N'), [ssCtrl,ssShift]);
  AddKey(ecColumnSelect, ord('C'), [ssCtrl,ssShift]);
  AddKey(ecLineSelect, ord('L'), [ssCtrl,ssShift]);
  AddKey(ecMatchBracket, ord('B'), [ssCtrl,ssShift]);
end;

procedure TSynEditKeyStrokes.SetItem(Index: Integer; Value: TSynEditKeyStroke);
begin
 inherited SetItem(Index, Value);
end;

{begin}                                                                         //ac 2000-07-05
procedure TSynEditKeyStrokes.SaveToStream(AStream: TStream);
var
  i, Num: integer;
begin
  Num := Count;
  AStream.Write(Num, SizeOf(Num));
  for i := 0 to Num - 1 do
    Items[i].SaveToStream(AStream);
end;
{end}                                                                           //ac 2000-07-05

{begin}                                                                         //ddh 10/16/01 "English" Code Strings
Function ConvertCodeStringToExtended(AString : String) : String;
VAR i : integer;
    WorkStr : String;
begin
  if pos('ec', AString) = 1 then
  begin
    delete(AString,1,2);
    WorkStr := '';

    for i := length(AString) downto 1 do
      if (AString[i] in ['A'..'Z', '0'..'9']) and (i > 1) and
         not(AString[i - 1] in ['A'..'Z', '0'..'9']) then
      begin
        WorkStr := ' ' + AString[i] + WorkStr
      end else WorkStr := AString[i] + WorkStr;

    trim(WorkStr);

    i := pos('Sel ', WorkStr);
    while i <> 0 do
    begin
      Delete(WorkStr,i,Length('Sel '));
      Insert('Select ',WorkStr,i);
      i := pos('Sel ', WorkStr);
    end;

    i := pos('Marker ', WorkStr);
    while i <> 0 do
    begin
      Delete(WorkStr,i,Length('Marker '));
      Insert('Bookmark ',WorkStr,i);
      i := pos('Marker ', WorkStr);
    end;

    Result := trim(WorkStr);
  end else Result := AString;
end;

Function ConvertExtendedToCodeString(AString : String) : String;
VAR i : integer;
    WorkStr : String;
begin
  if pos('ec', AString) = 1 then
  begin
    result := AString;
    exit;
  end;

  WorkStr := AString;

  i := pos('Select ', WorkStr);
  while i <> 0 do
  begin
    Delete(WorkStr,i,Length('Select '));
    Insert('Sel ',WorkStr,i);
    i := pos('Select ', WorkStr);
  end;

  i := pos('Bookmark ', WorkStr);
  while i <> 0 do
  begin
    Delete(WorkStr,i,Length('Bookmark '));
    Insert('Marker ',WorkStr,i);
    i := pos('Bookmark ', WorkStr);
  end;

  i := pos(' ', WorkStr);
  While i <> 0 do
  begin
    delete(WorkStr,i,1);
    i := pos(' ', WorkStr);
  end;

  Result := 'ec' + WorkStr;
end;

function IndexToEditorCommand(const AIndex: Integer) : Integer;
begin
  Result := EditorCommandStrs[AIndex].Value;
end;

function ConvertExtendedToCommand(AString : String) : TSynEditorCommand;
begin
  Result := ConvertCodeStringToCommand(ConvertExtendedToCodeString(AString));
end;

function ConvertCodeStringToCommand(AString : String) : TSynEditorCommand;
var I: Integer;
begin
  Result := ecNone;

  AString := Uppercase(AString);
  for i := Low(EditorCommandStrs) to High(EditorCommandStrs) do
    if Uppercase(EditorCommandStrs[i].Name) = AString then
    begin
      Result := EditorCommandStrs[i].Value;
      break;
    end;
end;


{end}                                                                           //ddh 10/16/01 "English" code strings

initialization
  RegisterIntegerConsts(TypeInfo(TSynEditorCommand), IdentToEditorCommand,
     EditorCommandToIdent);
end.
