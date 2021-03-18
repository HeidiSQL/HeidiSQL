{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynMemo.pas, released 2000-04-07.
The Original Code is based on mwCustomEdit.pas by Martin Waldenburg, part of
the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
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

$Id: SynMemo.pas,v 1.15.2.3 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
  - several EM_XXX messages aren't handled yet;
  - EM_XXX messages aren't implemented on CLX, although this could be useful;
-------------------------------------------------------------------------------}

unit SynMemo;

{$I SynEdit.inc}

interface

uses
  RichEdit,
  Windows,
  Messages,
  SynEdit,
  SynEditTextBuffer,
  SynEditTypes,
  SysUtils,
  Classes;

type
  TSynMemo = class(TSynEdit)
  private
    // EM_XXX see winuser.h (PSDK August 2001)
    procedure EMGetSel(var Message: TMessage); message EM_GETSEL;
    procedure EMSetSel(var Message: TMessage); message EM_SETSEL;
    procedure EMGetModify(var Message: TMessage); message EM_GETMODIFY;
    procedure EMSetModify(var Message: TMessage); message EM_SETMODIFY;
    procedure EMGetLineCount(var Message: TMessage); message EM_GETLINECOUNT;
    procedure EMGetSelText(var Message: TMessage); message EM_GETSELTEXT;       //richedit.h
    procedure EMReplaceSel(var Message: TMessage); message EM_REPLACESEL;
    procedure EMGetLine(var Message: TMessage); message EM_GETLINE;
    procedure EMCanUndo(var Message: TMessage); message EM_CANUNDO;
    procedure EMUndo(var Message: TMessage); message EM_UNDO;
    procedure EMGetFirstVisibleLine(var Message: TMessage); message EM_GETFIRSTVISIBLELINE;
    procedure EMCharFromPos(var Message: TMessage); message EM_CHARFROMPOS;
  end;

implementation

uses
{$IFDEF SYN_COMPILER_18_UP}
  AnsiStrings,
{$ENDIF}
{$IFDEF UNICODE}
  WideStrUtils,
{$ENDIF}
  SynUnicode,
  SynEditMiscProcs;

{ TSynMemo }

// EM_GETSEL
// wParam = (WPARAM) (LPDWORD) lpdwStart;      // receives starting position
// lParam = (LPARAM) (LPDWORD) lpdwEnd;        // receives ending position
procedure TSynMemo.EMGetSel(var Message: TMessage);
var
  s, e: Integer;
begin
  s := GetSelStart;
  e := GetSelEnd;
  if Message.wParam <> 0 then PDWORD(Message.wParam)^ := s;
  if Message.lParam <> 0 then PDWORD(Message.lParam)^ := e;
  Message.Result := MakeLong(s, e)
end;

// EM_SETSEL
// wParam = (WPARAM) (INT) nStart;             // starting position
// lParam = (LPARAM) (INT) nEnd;               // ending position
procedure TSynMemo.EMSetSel(var Message: TMessage);
begin
  SetSelStart(Message.wParam);
  SetSelEnd(Message.lParam);
end;

procedure TSynMemo.EMSetModify(var Message: TMessage);
begin
  Modified := Message.wParam <> 0;
end;

procedure TSynMemo.EMGetModify(var Message: TMessage);
begin
  Message.Result := Integer(Modified);
end;

procedure TSynMemo.EMGetLineCount(var Message: TMessage);
begin
  //(WPARAM) wParam,      // not used; must be zero
  //(LPARAM) lParam       // not used; must be zero
  Message.Result := Lines.Count;
end;

procedure TSynMemo.EMGetSelText(var Message: TMessage);
begin
  if Message.lParam <> 0 then
  begin
    if IsWindowUnicode(Handle) then
      WStrLCopy(PWideChar(Message.lParam), PWideChar(SelText), Length(SelText))
    else
      {$IFDEF SYN_COMPILER_18_UP}AnsiStrings.{$ENDIF}StrLCopy(PAnsiChar(Message.lParam), PAnsiChar(AnsiString(SelText)), Length(SelText));
    Message.Result := Length(SelText);
  end;                          
end;


// EM_REPLACESEL
// fCanUndo = (BOOL) wParam ;                  // flag that specifies whether replacement can be undone
// lpszReplace = (LPCTSTR) lParam ;            // pointer to replacement text string
// see PasteFromClipboard CF_TEXT - use common function ?
// or use SetSelText/SetSelTextPrimitive (no undo)
procedure TSynMemo.EMReplaceSel(var Message: TMessage);
var
  StartOfBlock: TBufferCoord;
  EndOfBlock: TBufferCoord;
begin
  if ReadOnly then Exit;
  DoOnPaintTransient(ttBefore);
  BeginUndoBlock;
  try
    if SelAvail and (Message.WParam <> 0){???} then
      UndoList.AddChange(crDelete, BlockBegin, BlockEnd, SelText, SelectionMode);
    StartOfBlock := BlockBegin;
    EndOfBlock := BlockEnd;
    BlockBegin := StartOfBlock;
    BlockEnd := EndOfBlock;
    LockUndo;
    try
      if IsWindowUnicode(Handle) then
        SelText := PWideChar(Message.lParam)
      else
        SelText := UnicodeString(PAnsiChar(Message.lParam))
    finally
      UnlockUndo;
    end;
    if (Message.WParam <> 0){???} then
      UndoList.AddChange(crPaste, StartOfBlock, BlockEnd, SelText, smNormal);
  finally
    EndUndoBlock;
  end;
  EnsureCursorPosVisible;
  // Selection should have changed...
  StatusChanged([scSelection]); 

  DoOnPaintTransient(ttAfter);
end;

// wParam = line number
// lParam = line string (PAnsiChar/PWideChar)
// no terminating #0
procedure TSynMemo.EMGetLine(var Message: TMessage);
var
  DestAnsi, SourceAnsi: PAnsiChar;
  DestWide, SourceWide: PWideChar;
begin
  if {$IFNDEF SYN_COMPILER_16_UP}(Message.WParam >= 0) and {$ENDIF}(Integer(Message.WParam) < Lines.Count) then
  begin
    if IsWindowUnicode(Handle) then
    begin
      DestWide := PWideChar(Message.LParam);
      SourceWide := PWideChar(Lines[Message.WParam]);
      WStrLCopy(DestWide, SourceWide, PWord(Message.LParam)^);
      Message.Result := WStrLen(DestWide);
    end
    else
    begin
      DestAnsi := PAnsiChar(Message.LParam);
      SourceAnsi := PAnsiChar(AnsiString(Lines[Message.WParam]));
      {$IFDEF SYN_COMPILER_18_UP}AnsiStrings.{$ENDIF}StrLCopy(DestAnsi, SourceAnsi, PWord(Message.LParam)^);
      Message.Result := {$IFDEF SYN_COMPILER_18_UP}AnsiStrings.{$ENDIF}StrLen(DestAnsi);
    end
  end
  else
    Message.Result := 0;
end;

//(WPARAM) wParam,    // not used; must be zero
//(LPARAM) lParam     // not used; must be zero
procedure TSynMemo.EMCanUndo(var Message: TMessage);
begin
  Message.Result := Integer(CanUndo);
end;

//(WPARAM) wParam,    // not used; must be zero
//(LPARAM) lParam     // not used; must be zero
procedure TSynMemo.EMUndo(var Message: TMessage);
begin
  Message.Result := Integer(CanUndo);
  Undo;
end;

//(WPARAM) wParam,          // not used; must be zero
//(LPARAM) lParam           // not used; must be zero
procedure TSynMemo.EMGetFirstVisibleLine(var Message: TMessage);
begin
  Message.Result := TopLine;
end;

//(WPARAM) wParam,    // not used; must be zero
//(LPARAM) lParam     // point coordinates
procedure TSynMemo.EMCharFromPos(var Message: TMessage);
var
  vPos: TBufferCoord;
  i: Integer;
begin
  vPos := DisplayToBufferPos(PixelsToRowColumn(Message.LParamLo, Message.LParamHi));

  Dec(vPos.Line);
  if vPos.Line >= Lines.Count then 
    vPos.Char := 1
  else if vPos.Char > Length(Lines[vPos.Line]) then
    vPos.Char := Length(Lines[vPos.Line]) + 1; // ???

  i := vPos.Line;
  while i > 0 do
  begin
    Dec(i);
    Inc(vPos.Char, Length(Lines[i]) + 2);
  end;

  //todo: this can't be right, CharIndex can easily overflow
  Message.Result := MakeLong(vPos.Char{CharIndex}, vPos.Line{Line zero based});
end;

end.
