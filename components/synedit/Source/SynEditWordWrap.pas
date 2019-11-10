{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is SynEditWordWrap.pas by Flávio Etrusco, released 2003-12-11.
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

$Id: SynEditWordWrap.pas,v 1.8.2.6 2008/09/14 16:24:59 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
//todo: Use a single implementation of ReWrapLines that takes starting line and number of lines to rewrap
//todo: Tweak code to try finding better wrapping points. Some support by the highlighters will be needed, probably.
//todo: Document the code
//todo: The length of the last Row of a Line could be calculated from the Line length instead of being stored. This would be only useful when most of the lines aren't wrapped.

{$IFNDEF QSYNEDITWORDWRAP}
unit SynEditWordWrap;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
  SynEditTypes,
  SynEditTextBuffer,
  SynEdit,
  SysUtils,
  Classes;

var
  // Accumulate/hide whitespace at EOL (at end of wrapped rows, actually)
  OldWhitespaceBehaviour: Boolean = False;

const
  MaxIndex = MaxInt div 16;

type
  TLineIndex = 0..MaxIndex;
  TRowIndex = 0..MaxIndex;
  TRowLength = Word;

  TRowIndexArray = array [TLineIndex] of TRowIndex;
  PRowIndexArray = ^TRowIndexArray;

  TRowLengthArray = array [TRowIndex] of TRowLength;
  PRowLengthArray = ^TRowLengthArray;

  {$IFNDEF SYN_COMPILER_4_UP}
  TSysCharSet = set of Char;
  {$ENDIF}

  // For clarity, I'll refer to buffer coordinates as 'Line' and
  // 'Char' and to display (wrapped) coordinates as 'Row' and 'Column'.

  // FLineOffsets[n] is the index of the first row of the [n+1]th line.
  // e.g. Starting row of first line (0) is 0. Starting row of second line (1)
  // is FLineOffsets[0]. Clear?

  TSynWordWrapPlugin = class(TInterfacedObject, ISynEditBufferPlugin)
  private
    FLineOffsets: PRowIndexArray;
    FRowLengths: PRowLengthArray;
    FLineCapacity: Integer;
    FRowCapacity: Integer;
    FLineCount: Integer;

    FEditor: TCustomSynEdit;
    FMinRowLength: TRowLength;
    FMaxRowLength: TRowLength;
    procedure GrowLines(aMinSize: Integer);
    procedure MoveLines(aStart: TLineIndex; aMoveBy: Integer);
    procedure GrowRows(aMinSize: Integer);
    procedure MoveRows(aStart: TRowIndex; aMoveBy: Integer);
    procedure SetEmpty;
  protected
    procedure WrapLines;
    function ReWrapLine(aIndex: TLineIndex): Integer;
    procedure TrimArrays;
    property LineOffsets: PRowIndexArray read FLineOffsets;
    property RowLengths: PRowLengthArray read FRowLengths;
    property Editor: TCustomSynEdit read FEditor;
  public
    constructor Create(aOwner: TCustomSynEdit);
    destructor Destroy; override;
    property LineCount: Integer read FLineCount;
    { ISynEditBufferPlugin }
    function BufferToDisplayPos(const aPos: TBufferCoord): TDisplayCoord;
    function DisplayToBufferPos(const aPos: TDisplayCoord): TBufferCoord;
    function RowCount: Integer;
    function GetRowLength(aRow: Integer): Integer;
    function LinesInserted(aIndex: Integer; aCount: Integer): Integer;
    function LinesDeleted(aIndex: Integer; aCount: Integer): Integer;
    function LinesPutted(aIndex: Integer; aCount: Integer): Integer;
    procedure Reset;
    procedure DisplayChanged; 
  end;

implementation

uses
  SynUnicode,
{$IFDEF SYN_COMPILER_6_UP}
  RTLConsts,
{$ELSE}
  Consts,
{$ENDIF}
{$IFNDEF SYN_COMPILER_4_UP}
  SynEditMiscProcs,
{$ENDIF}
  Math;

{ TSynWordWrapPlugin }

function TSynWordWrapPlugin.BufferToDisplayPos(
  const aPos: TBufferCoord): TDisplayCoord;
var
  vStartRow: Integer; // first row of the line
  cRow: Integer;
  vRowLen: Integer;
begin
  Assert(aPos.Char > 0);
  Assert(aPos.Line > 0);
  if LineCount < aPos.Line then
  begin
    // beyond EOF
    Result.Column := aPos.Char;
    Result.Row := RowCount + (aPos.Line - LineCount);
    Exit;
  end;
  if aPos.Line = 1 then
    vStartRow := 0
  else
    vStartRow := FLineOffsets[aPos.Line - 2];
  vRowLen := 0;
  for cRow := vStartRow to FLineOffsets[aPos.Line - 1] - 1 do
  begin
    Inc(vRowLen, FRowLengths[cRow]);
    if aPos.Char <= vRowLen then
    begin
      Result.Column := aPos.Char - vRowLen + FRowLengths[cRow];
      Result.Row := cRow + 1;
      Exit;
    end;
  end;
  // beyond EOL
  Result.Column := aPos.Char - vRowLen + FRowLengths[FLineOffsets[aPos.Line - 1] - 1];
  Result.Row := FLineOffsets[aPos.Line - 1];
end;

constructor TSynWordWrapPlugin.Create(aOwner: TCustomSynEdit);
begin
  inherited Create; // just to work as reminder in case I revert it to a TComponent... 
  if aOwner = nil then
    raise Exception.Create( 'Owner of TSynWordWrapPlugin must be a TCustomSynEdit' );
  FEditor := aOwner;
  Reset;
end;

destructor TSynWordWrapPlugin.Destroy;
begin
  inherited;
  FreeMem(FLineOffsets);
  FreeMem(FRowLengths);
end;

procedure TSynWordWrapPlugin.DisplayChanged;
begin
  if Editor.CharsInWindow <> FMaxRowLength then
    Reset;
end;

function TSynWordWrapPlugin.DisplayToBufferPos(
  const aPos: TDisplayCoord): TBufferCoord;
var
  cLine: Integer;
  cRow: Integer;
begin
  Assert(aPos.Column > 0);
  Assert(aPos.Row > 0);
  if aPos.Row > RowCount then
  begin
    // beyond EOF
    Result.Char := aPos.Column;
    Result.Line := aPos.Row - RowCount + LineCount;
    Exit;
  end;
  //todo: use a binary search or something smarter
  for cLine := LineCount - 2 downto 0 do
    if aPos.Row > FLineOffsets[cLine] then
    begin
      Result.Line := cLine + 2;
      if aPos.Row = FLineOffsets[cLine + 1] then //last row of line
        Result.Char := Min(aPos.Column, FMaxRowLength + 1)
      else
        Result.Char := Min(aPos.Column, FRowLengths[aPos.Row - 1] + 1);
      for cRow := FLineOffsets[cLine] to aPos.Row - 2 do
        Inc(Result.Char, FRowLengths[cRow]);
      Exit;
    end;
  // first line
  Result.Line := 1;
  if aPos.Row = FLineOffsets[0] then //last row of line
    Result.Char := Min(aPos.Column, FMaxRowLength + 1)
  else
    Result.Char := Min(aPos.Column, FRowLengths[aPos.Row - 1] + 1);
  for cRow := 0 to aPos.Row - 2 do
    Inc(Result.Char, FRowLengths[cRow]);
end;

function TSynWordWrapPlugin.GetRowLength(aRow: Integer): Integer;
// aRow is 1-based...
begin
  if (aRow <= 0) or (aRow > RowCount) then
    TList.Error(SListIndexError, aRow);
  Result := FRowLengths[aRow - 1];
end;

procedure TSynWordWrapPlugin.GrowLines(aMinSize: Integer);
const
  vStepSize = 256;
begin
  Assert(aMinSize > 0);
  if aMinSize > FLineCapacity then
  begin
    aMinSize := aMinSize + vStepSize - (aMinSize mod vStepSize);
    ReallocMem(FLineOffsets, aMinSize * SizeOf(TRowIndex));
    FLineCapacity := aMinSize;
  end;
end;

procedure TSynWordWrapPlugin.GrowRows(aMinSize: Integer);
const
  vStepSize = 512;
begin
  Assert(aMinSize > 0);
  if aMinSize > FRowCapacity then
  begin
    aMinSize := aMinSize + vStepSize - (aMinSize mod vStepSize);
    ReallocMem(FRowLengths, aMinSize * SizeOf(TRowLength));
    FRowCapacity := aMinSize;
  end;
end;

function TSynWordWrapPlugin.LinesDeleted(aIndex: Integer; aCount: Integer): Integer;
var
  vStartRow: Integer;
  vEndRow: Integer;
  cLine: Integer;
begin
  if FMaxRowLength = 0 then
  begin
    Result := 0;
    Exit;
  end;
  Assert(aIndex >= 0);
  Assert(aCount >= 1);
  Assert(aIndex + aCount <= LineCount);

  if aIndex = 0 then
    vStartRow := 0
  else
    vStartRow := FLineOffsets[aIndex - 1];
  vEndRow := FLineOffsets[aIndex + aCount - 1];
  Result := vEndRow - vStartRow;
  // resize FRowLengths
  if vEndRow < RowCount then
    MoveRows(vEndRow, -Result);
  // resize FLineOffsets
  MoveLines(aIndex + aCount, -aCount);
  Dec(FLineCount, aCount);
  // update offsets
  for cLine := aIndex to LineCount - 1 do
    Dec(FLineOffsets[cLine], Result);
end;

function TSynWordWrapPlugin.LinesInserted(aIndex: Integer; aCount: Integer): Integer;
var
  vPrevOffset: TRowIndex;
  cLine: Integer;
begin
  if FMaxRowLength = 0 then
  begin
    Result := 0;
    Exit;
  end;
  Assert(aIndex >= 0);
  Assert(aCount >= 1);
  Assert(aIndex <= LineCount);
  // resize FLineOffsets
  GrowLines(LineCount + aCount);
  if aIndex < LineCount then // no need for MoveLines if inserting at LineCount (TSynEditStringList.Add)
  begin
    Inc(FLineCount, aCount); // FLineCount must be updated before calling MoveLines()
    MoveLines(aIndex, aCount);
  end
  else
    Inc(FLineCount, aCount);
  // set offset to same as previous line (i.e. the line has 0 rows)
  if aIndex = 0 then
    vPrevOffset := 0
  else
    vPrevOffset := FLineOffsets[aIndex - 1];
  for cLine := aIndex to aIndex + aCount - 1 do
    FLineOffsets[cLine] := vPrevOffset;
  // Rewrap
  Result := 0;
  for cLine := aIndex to aIndex + aCount - 1 do
    Inc(Result, ReWrapLine(cLine));
end;

function TSynWordWrapPlugin.LinesPutted(aIndex: Integer; aCount: Integer): Integer;
var
  cLine: Integer;
begin
  if FMaxRowLength = 0 then
  begin
    Result := 0;
    Exit;
  end;
  Assert(aIndex >= 0);
  Assert(aCount >= 1);
  Assert(aIndex + aCount <= LineCount);
  // Rewrap
  Result := 0;
  for cLine := aIndex to aIndex + aCount - 1 do
    Inc(Result, ReWrapLine(cLine));
end;

procedure TSynWordWrapPlugin.MoveLines(aStart: TLineIndex; aMoveBy: Integer);
var
  vMoveCount: Integer;
begin
  Assert(aMoveBy <> 0);
  Assert(aStart + aMoveBy >= 0);
  Assert(aStart + aMoveBy < LineCount);
  vMoveCount := LineCount - aStart;
  if aMoveBy > 0 then
    Dec(vMoveCount, aMoveBy);
  Move(FLineOffsets[aStart], FLineOffsets[aStart + aMoveBy],
    vMoveCount * SizeOf(TRowIndex));
end;

procedure TSynWordWrapPlugin.MoveRows(aStart: TRowIndex; aMoveBy: Integer);
var
  vMoveCount: Integer;
begin
  Assert(aMoveBy <> 0);
  Assert(aStart + aMoveBy >= 0);
  Assert(aStart + aMoveBy < RowCount);
  vMoveCount := RowCount - aStart;
  if aMoveBy > 0 then
    Dec(vMoveCount, aMoveBy);
  Move(FRowLengths[aStart], FRowLengths[aStart + aMoveBy],
    vMoveCount * SizeOf(TRowLength));
end;

procedure TSynWordWrapPlugin.Reset;
begin
  Assert(Editor.CharsInWindow >= 0);

  FMaxRowLength := Min(Editor.MaxScrollWidth, Editor.CharsInWindow); // see github issue #129
  FMinRowLength := Editor.CharsInWindow - (Editor.CharsInWindow div 3);

  if FMinRowLength <= 0 then
    FMinRowLength := 1;

  WrapLines;
end;

function TSynWordWrapPlugin.ReWrapLine(aIndex: TLineIndex): Integer;
// Returns RowCount delta (how many wrapped lines were added or removed by this change).
var
  vMaxNewRows: Cardinal;
  vLine: UnicodeString;
  vLineRowCount: Integer; //numbers of rows parsed in this line
  vTempRowLengths: PRowLengthArray;
  vRowBegin: PWideChar;
  vLineEnd: PWideChar;
  vRowEnd: PWideChar;
  vRunner: PWideChar;
  vRowMinEnd: PWideChar;
  vLastVisibleChar: PWideChar;

  vStartRow: Integer; // first row of the line
  vOldNextRow: Integer; // first row of the next line, before the change
  cLine: Integer;

  p : PRowIndexArray;
begin
  // ****** First parse the new string using an auxiliar array *****
  vLine := TSynEditStringList(Editor.Lines).ExpandedStrings[aIndex];
  vLine := Editor.ExpandAtWideGlyphs(vLine);
  // Pre-allocate a buffer for rowlengths
  vMaxNewRows := ((Length(vLine) - 1) div FMinRowLength) + 1;
  vTempRowLengths := AllocMem(vMaxNewRows * SizeOf(TRowLength));
  try
    vLineRowCount := 0;
    vRowBegin := PWideChar(vLine);
    vRowEnd := vRowBegin + FMaxRowLength;
    vLineEnd := vRowBegin + Length(vLine);
    while vRowEnd < vLineEnd do
    begin
      if OldWhitespaceBehaviour and CharInSet(vRowEnd^, [#32, #9]) then
      begin
        repeat
          Inc(vRowEnd);
        until not CharInSet(vRowEnd^, [#32, #9]);
      end
      else
      begin
        vRowMinEnd := vRowBegin + FMinRowLength;
        vRunner := vRowEnd;
        while vRunner > vRowMinEnd do
        begin
          if Editor.IsWordBreakChar(vRunner^) then
          begin
            vRowEnd := vRunner;
            Break;
          end;
          Dec(vRunner);
        end;
      end;
      // Check TRowLength overflow
      if OldWhitespaceBehaviour and (vRowEnd - vRowBegin > High(TRowLength)) then
      begin
        vRowEnd := vRowBegin + High(TRowLength);
        vRowMinEnd := vRowEnd - (High(TRowLength) mod Editor.TabWidth);
        while (vRowEnd^ = #9) and (vRowEnd > vRowMinEnd) do
          Dec(vRowEnd);
      end;

      // do not cut wide glyphs in half
      if vRowEnd > vRowBegin then
      begin
        vLastVisibleChar := vRowEnd - 1;
        while (vLastVisibleChar^ = FillerChar) and (vLastVisibleChar > vRowBegin) do
          Dec(vLastVisibleChar);
        vRowEnd := vLastVisibleChar + 1;
      end;

      // Finally store the rowlength
      vTempRowLengths[vLineRowCount] := vRowEnd - vRowBegin;

      Inc(vLineRowCount);
      vRowBegin := vRowEnd;
      Inc(vRowEnd, FMaxRowLength);
    end; //endwhile vRowEnd < vLineEnd
    if (vLineEnd > vRowBegin) or (Length(vLine) = 0) then
    begin
      vTempRowLengths[vLineRowCount] := vLineEnd - vRowBegin;
      Inc(vLineRowCount);
    end;

    // ****** Then updates the main arrays ******
    if aIndex = 0 then
      vStartRow := 0
    else
      vStartRow := FLineOffsets[aIndex - 1];
    vOldNextRow := FLineOffsets[aIndex];
    Result := vLineRowCount - (vOldNextRow - vStartRow);
    if Result <> 0 then
    begin
      // MoveRows depends on RowCount, so we need some special processing...
      if Result > 0 then
      begin
        // ...if growing, update offsets (and thus RowCount) before rowlengths
        GrowRows(RowCount + Result);
        if Result = 1 then begin
          // EG: this makes Schlemiel run twice as fast, but doesn't solve
          // the algorithmic issue if someone can spend some time looking
          // at the big picture... there are huge speedups to be made by
          // eliminating this loop
          p:=FLineOffsets;
          for cLine := aIndex to LineCount - 1 do
             Inc(p[cLine])
        end else begin
          p:=FLineOffsets;
          for cLine := aIndex to LineCount - 1 do
            Inc(p[cLine], Result);
        end;
        if vOldNextRow < RowCount - Result then
          MoveRows(vOldNextRow, Result);
      end
      else
      begin
        // ...if shrinking, update offsets after rowlengths
        if vOldNextRow < RowCount then
          MoveRows(vOldNextRow, Result);
        for cLine := aIndex to LineCount - 1 do
          Inc(FLineOffsets[cLine], Result);
      end;
    end;
    Move(vTempRowLengths[0], FRowLengths[vStartRow], vLineRowCount * SizeOf(TRowLength));
  finally
    FreeMem(vTempRowLengths);
  end;
end;

procedure TSynWordWrapPlugin.WrapLines;
var
  cRow: Integer;
  cLine: Integer;
  vLine: UnicodeString;
  vMaxNewRows: Integer;
  vRowBegin: PWideChar;
  vLineEnd: PWideChar;
  vRowEnd: PWideChar;
  vRunner: PWideChar;
  vRowMinEnd: PWideChar;
  vLastVisibleChar: PWideChar;
begin
  if (Editor.Lines.Count = 0) or (FMaxRowLength <= 0) then
  begin
    SetEmpty;
    Exit;
  end;

  GrowLines(Editor.Lines.Count);
  GrowRows(Editor.Lines.Count);

  cRow := 0;
  for cLine := 0 to Editor.Lines.Count - 1 do
  begin
    vLine := TSynEditStringList(Editor.Lines).ExpandedStrings[cLine];
    vLine := Editor.ExpandAtWideGlyphs(vLine);

    vMaxNewRows := ((Length(vLine) - 1) div FMinRowLength) + 1;
    GrowRows(cRow + vMaxNewRows);

    vRowBegin := PWideChar(vLine);
    vRowEnd := vRowBegin + FMaxRowLength;
    vLineEnd := vRowBegin + Length(vLine);
    while vRowEnd < vLineEnd do
    begin
      if OldWhitespaceBehaviour and CharInSet(vRowEnd^, [#32, #9]) then
      begin
        repeat
          Inc(vRowEnd);
        until not CharInSet(vRowEnd^, [#32, #9]);
      end
      else
      begin
        vRowMinEnd := vRowBegin + FMinRowLength;
        vRunner := vRowEnd;
        while vRunner > vRowMinEnd do
        begin
          if Editor.IsWordBreakChar(vRunner^) then
          begin
            vRowEnd := vRunner;
            Break;
          end;
          Dec(vRunner);
        end;
      end;

      if OldWhitespaceBehaviour and (vRowEnd - vRowBegin > High(TRowLength)) then
      begin
        vRowEnd := vRowBegin + High(TRowLength);
        vRowMinEnd := vRowEnd - (High(TRowLength) mod Editor.TabWidth);
        while (vRowEnd^ = #9) and (vRowEnd > vRowMinEnd) do
          Dec(vRowEnd);
      end;

      // do not cut wide glyphs in half
      if vRowEnd > vRowBegin then
      begin
        vLastVisibleChar := vRowEnd - 1;
        while (vLastVisibleChar^ = FillerChar) and (vLastVisibleChar > vRowBegin) do
          Dec(vLastVisibleChar);
        vRowEnd := vLastVisibleChar + 1;
      end;

      FRowLengths[cRow] := vRowEnd - vRowBegin;

      Inc(cRow);
      vRowBegin := vRowEnd;
      Inc(vRowEnd, FMaxRowLength);
    end;
    if (vLineEnd > vRowBegin) or (Length(vLine) = 0) then
    begin
      FRowLengths[cRow] := vLineEnd - vRowBegin;
      Inc(cRow);
    end;
    FLineOffsets[cLine] := cRow;
  end;
  FLineCount := Editor.Lines.Count;
end;

function TSynWordWrapPlugin.RowCount: Integer;
begin
  if LineCount > 0 then
    Result := FLineOffsets[LineCount - 1]
  else
    Result := 0;
end;

procedure TSynWordWrapPlugin.SetEmpty;
begin
  FLineCount := 0;
  // free unsused memory
  TrimArrays;
end;

procedure TSynWordWrapPlugin.TrimArrays;
begin
  ReallocMem(FLineOffsets, LineCount * SizeOf(TRowIndex));
  FLineCapacity := LineCount;
  ReallocMem(FRowLengths, RowCount * SizeOf(TRowLength));
  FRowCapacity := RowCount;
end;

end.
