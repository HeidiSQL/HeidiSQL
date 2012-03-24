{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditTextBuffer.pas, released 2000-04-07.
The Original Code is based on parts of mwCustomEdit.pas by Martin Waldenburg,
part of the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
Unicode translation by Ma�l H�rz.
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

$Id: SynEditTextBuffer.pas,v 1.14 2011/12/28 09:24:20 Egg Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
//todo: Avoid calculating expanded string unncessarily (just calculate expandedLength instead).

{$IFNDEF QSYNEDITTEXTBUFFER}
unit SynEditTextBuffer;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  kTextDrawer,
  Types,
  QSynEditTypes,
  QSynEditMiscProcs,
  QSynUnicode,
{$ELSE}
  Windows,
  SynEditTypes,
  SynEditMiscProcs,
  SynUnicode,
{$ENDIF}
  Classes,
  SysUtils,
  Graphics;

type
  TSynEditRange = pointer;

  TSynEditStringFlag = (sfHasTabs, sfHasNoTabs, sfExpandedLengthUnknown);
  TSynEditStringFlags = set of TSynEditStringFlag;

  PSynEditStringRec = ^TSynEditStringRec;
  TSynEditStringRec = record
    {$IFDEF OWN_UnicodeString_MEMMGR}
    FString: PWideChar; // "array of WideChar";
    {$ELSE}
    FString: UnicodeString;
    {$ENDIF OWN_UnicodeString_MEMMGR}
    fObject: TObject;
    fRange: TSynEditRange;
    fExpandedLength: Integer;
    fCharIndex : Integer;
    fFlags: TSynEditStringFlags;
  end;

  TSynEditTwoWideChars = record
    One, Two : WideChar;
  end;
  PSynEditTwoWideChars = ^TSynEditTwoWideChars;

const
  SynEditStringRecSize = SizeOf(TSynEditStringRec);
  MaxSynEditStrings = MaxInt div SynEditStringRecSize;

  NullRange = TSynEditRange(-1);

type
  PSynEditStringRecList = ^TSynEditStringRecList;
  TSynEditStringRecList = array[0..MaxSynEditStrings - 1] of TSynEditStringRec;

  TStringListChangeEvent = procedure(Sender: TObject; Index: Integer;
    Count: integer) of object;

  TExpandAtWideGlyphsFunc = function (const S: UnicodeString): UnicodeString of object;

  TSynEditFileFormat = (sffDos, sffUnix, sffMac, sffUnicode); // DOS: CRLF, UNIX: LF, Mac: CR, Unicode: LINE SEPARATOR

  TSynEditStringList = class(TUnicodeStrings)
  private
    fList: PSynEditStringRecList;
    fCount: integer;
    fCapacity: integer;
    fFileFormat: TSynEditFileFormat;
    fAppendNewLineAtEOF: Boolean;
    fConvertTabsProc: TConvertTabsProcEx;
    fIndexOfLongestLine: integer;
    fTabWidth: integer;
    FExpandAtWideGlyphsFunc: TExpandAtWideGlyphsFunc;
    FCharIndexesAreValid : Boolean;
    fOnChange: TNotifyEvent;
    fOnChanging: TNotifyEvent;
    fOnCleared: TNotifyEvent;
    fOnDeleted: TStringListChangeEvent;
    fOnInserted: TStringListChangeEvent;
    fOnPutted: TStringListChangeEvent;
    function ExpandString(Index: integer): UnicodeString;
    function GetExpandedString(Index: integer): UnicodeString;
    function GetExpandedStringLength(Index: integer): integer;
    function GetLengthOfLongestLine: Integer;
    function GetRange(Index: integer): TSynEditRange;
    procedure Grow;
    procedure InsertItem(Index: integer; const S: UnicodeString);
    procedure PutRange(Index: integer; ARange: TSynEditRange);
    procedure SetFileFormat(const Value: TSynEditFileFormat);
    {$IFDEF OWN_UnicodeString_MEMMGR}
    procedure SetListString(Index: Integer; const S: UnicodeString);
    {$ENDIF OWN_UnicodeString_MEMMGR}
  protected
    FStreaming: Boolean;
    function Get(Index: Integer): UnicodeString; override;
    function GetCapacity: integer;
      {$IFDEF SYN_COMPILER_3_UP} override; {$ENDIF}
    function GetCount: integer; override;
    function GetObject(Index: integer): TObject; override;
    function GetTextStr: UnicodeString; override;
    procedure Put(Index: integer; const S: UnicodeString); override;
    procedure PutObject(Index: integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: integer);
      {$IFDEF SYN_COMPILER_3_UP} override; {$ENDIF}
    procedure SetTabWidth(Value: integer);
    procedure SetUpdateState(Updating: Boolean); override;
    procedure UpdateCharIndexes;
  public
    constructor Create(AExpandAtWideGlyphsFunc: TExpandAtWideGlyphsFunc);
    destructor Destroy; override;
    function Add(const S: UnicodeString): integer; override;
    procedure AddStrings(Strings: TUnicodeStrings); override;
    procedure Clear; override;
    procedure Delete(Index: integer); override;
    procedure DeleteLines(Index, NumLines: integer);
    procedure Exchange(Index1, Index2: integer); override;
    procedure Insert(Index: integer; const S: UnicodeString); override;
    procedure InsertLines(Index, NumLines: integer);
    procedure InsertStrings(Index: integer; NewStrings: TUnicodeStrings);
    procedure InsertText(Index: integer; NewText: UnicodeString);
{$IFDEF UNICODE}
    procedure SaveToStream(Stream: TStream; Encoding: TEncoding); override;
    function GetSeparatedText(Separators: UnicodeString): UnicodeString;
{$ELSE}
    procedure SaveToStream(Stream: TStream; WithBOM: Boolean = True); override;
{$ENDIF}
    procedure SetTextStr(const Value: UnicodeString); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure FontChanged;
    function LineCharLength(Index : Integer) : Integer;
    function LineCharIndex(Index : Integer) : Integer;

    property AppendNewLineAtEOF: Boolean read fAppendNewLineAtEOF write fAppendNewLineAtEOF;

    property FileFormat: TSynEditFileFormat read fFileFormat write SetFileFormat;
    property ExpandedStrings[Index: integer]: UnicodeString read GetExpandedString;
    property ExpandedStringLengths[Index: integer]: integer read GetExpandedStringLength;
    property LengthOfLongestLine: Integer read GetLengthOfLongestLine;
    property Ranges[Index: integer]: TSynEditRange read GetRange write PutRange;
    property TabWidth: integer read fTabWidth write SetTabWidth;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property OnChanging: TNotifyEvent read fOnChanging write fOnChanging;
    property OnCleared: TNotifyEvent read fOnCleared write fOnCleared;
    property OnDeleted: TStringListChangeEvent read fOnDeleted write fOnDeleted;
    property OnInserted: TStringListChangeEvent read fOnInserted
      write fOnInserted;
    property OnPutted: TStringListChangeEvent read fOnPutted write fOnPutted;
  end;

  ESynEditStringList = class(Exception);

  TSynChangeReason = (crInsert, crPaste, crDragDropInsert,
    // Note: several undo entries can be chained together via the ChangeNumber
    // see also TCustomSynEdit.[Begin|End]UndoBlock methods
    crDeleteAfterCursor, crDelete,
    crLineBreak, crIndent, crUnindent,
    crSilentDelete, crSilentDeleteAfterCursor,
    crAutoCompleteBegin, crAutoCompleteEnd,
    crPasteBegin, crPasteEnd, // for pasting, since it might do a lot of operations
    crSpecial1Begin, crSpecial1End,
    crSpecial2Begin, crSpecial2End,
    crCaret,      // just restore the Caret, allowing better Undo behavior
    crSelection,  // restore Selection
    crNothing,
    crGroupBreak,
    crDeleteAll,
    crWhiteSpaceAdd // for undo/redo of adding a character past EOL and repositioning the caret
    );

  TSynEditUndoItem = class(TPersistent)
  protected
    fChangeReason: TSynChangeReason;
    fChangeSelMode: TSynSelectionMode;
    fChangeStartPos: TBufferCoord;
    fChangeEndPos: TBufferCoord;
    fChangeStr: UnicodeString;
    fChangeNumber: integer;                                                     
  public
    procedure Assign(Source: TPersistent); override;
    property ChangeReason: TSynChangeReason read fChangeReason;
    property ChangeSelMode: TSynSelectionMode read fChangeSelMode;
    property ChangeStartPos: TBufferCoord read fChangeStartPos;
    property ChangeEndPos: TBufferCoord read fChangeEndPos;
    property ChangeStr: UnicodeString read fChangeStr;
    property ChangeNumber: integer read fChangeNumber;
  end;

  TSynEditUndoList = class(TPersistent)
  protected
    fBlockChangeNumber: integer;
    fBlockCount: integer;
    fFullUndoImposible: boolean;
    fItems: TList;
    fLockCount: integer;
    fMaxUndoActions: integer;
    fNextChangeNumber: integer;
    fInitialChangeNumber: integer;
    fInsideRedo: boolean;
    fOnAddedUndo: TNotifyEvent;
    procedure EnsureMaxEntries;
    function GetCanUndo: boolean;
    function GetItemCount: integer;
    procedure SetMaxUndoActions(Value: integer);
    procedure SetInitialState(const Value: boolean);
    function GetInitialState: boolean;
    function GetItems(Index: Integer): TSynEditUndoItem;
    procedure SetItems(Index: Integer; const Value: TSynEditUndoItem);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddChange(AReason: TSynChangeReason; const AStart, AEnd: TBufferCoord;
      const ChangeText: UnicodeString; SelMode: TSynSelectionMode);
    procedure BeginBlock;                                                       
    procedure Clear;
    procedure EndBlock;
    procedure Lock;
    function PeekItem: TSynEditUndoItem;
    function PopItem: TSynEditUndoItem;
    procedure PushItem(Item: TSynEditUndoItem);
    procedure Unlock;
    function LastChangeReason: TSynChangeReason;
  public
    procedure Assign(Source: TPersistent); override;
    procedure AddGroupBreak;
    procedure DeleteItem(AIndex: Integer);
    property BlockChangeNumber: integer read fBlockChangeNumber
      write fBlockChangeNumber;
    property CanUndo: boolean read GetCanUndo;
    property FullUndoImpossible: boolean read fFullUndoImposible;
    property InitialState: boolean read GetInitialState write SetInitialState;
    property Items[Index: Integer]: TSynEditUndoItem read GetItems write SetItems;
    property ItemCount: integer read GetItemCount;
    property BlockCount: integer read fBlockCount;
    property MaxUndoActions: integer read fMaxUndoActions
      write SetMaxUndoActions;
    property InsideRedo: boolean read fInsideRedo write fInsideRedo;
    property OnAddedUndo: TNotifyEvent read fOnAddedUndo write fOnAddedUndo;
  end;

implementation

{$IFDEF SYN_COMPILER_3_UP}
resourcestring
{$ELSE}
const
{$ENDIF}
  SListIndexOutOfBounds = 'Invalid stringlist index %d';
  SInvalidCapacity = 'Stringlist capacity cannot be smaller than count';

{ TSynEditStringList }

procedure ListIndexOutOfBounds(Index: integer);
begin
  raise ESynEditStringList.CreateFmt(SListIndexOutOfBounds, [Index]);
end;

constructor TSynEditStringList.Create(AExpandAtWideGlyphsFunc: TExpandAtWideGlyphsFunc);
begin
  inherited Create;
  FExpandAtWideGlyphsFunc := AExpandAtWideGlyphsFunc;
  SetFileFormat(sffDos);
  fIndexOfLongestLine := -1;
  TabWidth := 8;
end;

destructor TSynEditStringList.Destroy;
begin
  fOnChange := nil;
  fOnChanging := nil;
  inherited Destroy;
  {$IFDEF OWN_UnicodeString_MEMMGR}
  fOnCleared := nil;
  Clear;
  {$ELSE}
  if fCount <> 0 then
    Finalize(fList^[0], fCount);
  fCount := 0;
  SetCapacity(0);
  {$ENDIF OWN_UnicodeString_MEMMGR}
end;

function TSynEditStringList.Add(const S: UnicodeString): integer;
begin
  BeginUpdate;
  Result := fCount;
  InsertItem(Result, S);
  if Assigned(OnInserted) then
    OnInserted(Self, Result, 1);
  EndUpdate;
end;

procedure TSynEditStringList.AddStrings(Strings: TUnicodeStrings);
var
  i, FirstAdded: integer;
begin
  if Strings.Count > 0 then begin
    fIndexOfLongestLine := -1;
    BeginUpdate;
    try
      i := fCount + Strings.Count;
      if i > fCapacity then
        SetCapacity((i + 15) and (not 15));
      FirstAdded := fCount;
      for i := 0 to Strings.Count - 1 do begin
        with fList^[fCount] do begin
          Pointer(fString) := nil;
          {$IFDEF OWN_UnicodeString_MEMMGR}
          SetListString(fCount, Strings[i]);
          {$ELSE}
          fString := Strings[i];
          {$ENDIF OWN_UnicodeString_MEMMGR}
          fObject := Strings.Objects[i];
          fRange := NullRange;
          fExpandedLength := -1;
          fFlags := [sfExpandedLengthUnknown];
        end;
        Inc(fCount);
      end;
      if Assigned(OnInserted) then
        OnInserted(Self, FirstAdded, Strings.Count);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSynEditStringList.Clear;
{$IFDEF OWN_UnicodeString_MEMMGR}
var
  I: Integer;
{$ENDIF OWN_UnicodeString_MEMMGR}
begin
  if fCount <> 0 then begin
    BeginUpdate;
    {$IFDEF OWN_UnicodeString_MEMMGR}
    for I := 0 to FCount - 1 do
      with FList[I] do
        if TDynWideCharArray(FString) <> nil then
          TDynWideCharArray(FString) := nil;
    {$ELSE}
    Finalize(fList^[0], fCount);
    {$ENDIF OWN_UnicodeString_MEMMGR}
    fCount := 0;
    SetCapacity(0);
    if Assigned(fOnCleared) then
      fOnCleared(Self);
    EndUpdate;
  end;
  fIndexOfLongestLine := -1;
end;

procedure TSynEditStringList.Delete(Index: integer);
begin
  if (Index < 0) or (Index > fCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  {$IFDEF OWN_UnicodeString_MEMMGR}
  SetListString(Index, '');
  {$ELSE}
  Finalize(fList^[Index]);
  {$ENDIF OWN_UnicodeString_MEMMGR}
  Dec(fCount);
  if Index < fCount then begin
    System.Move(fList^[Index + 1], fList^[Index],
      (fCount - Index) * SynEditStringRecSize);
    {$IFDEF OWN_UnicodeString_MEMMGR}
    Pointer(FList[fCount].fString) := nil; // avoid freeing the string, the address is now used in another element
    {$ENDIF OWN_UnicodeString_MEMMGR}
  end;
  fIndexOfLongestLine := -1;
  if Assigned(fOnDeleted) then
    fOnDeleted( Self, Index, 1 );
  EndUpdate;
end;

procedure TSynEditStringList.DeleteLines(Index, NumLines: Integer);
var
  LinesAfter: integer;
{$IFDEF OWN_UnicodeString_MEMMGR}
  I: Integer;
{$ENDIF OWN_UnicodeString_MEMMGR}
begin
  if NumLines > 0 then begin
    if (Index < 0) or (Index > fCount) then
      ListIndexOutOfBounds(Index);
    LinesAfter := fCount - (Index + NumLines - 1);
    if LinesAfter < 0 then
      NumLines := fCount - Index - 1;
    {$IFDEF OWN_UnicodeString_MEMMGR}
    for I := Index to Index + NumLines - 1 do
      with FList[I] do
        if TDynWideCharArray(FString) <> nil then
          TDynWideCharArray(FString) := nil;
    {$ELSE}
    Finalize(fList^[Index], NumLines);
    {$ENDIF OWN_UnicodeString_MEMMGR}

    if LinesAfter > 0 then begin
      BeginUpdate;
      try
        System.Move(fList^[Index + NumLines], fList^[Index],
          LinesAfter * SynEditStringRecSize);
      finally
        EndUpdate;
      end;
    end;
    Dec(fCount, NumLines);
    if Assigned(fOnDeleted) then
      fOnDeleted( Self, Index, NumLines );
  end;
end;

procedure TSynEditStringList.Exchange(Index1, Index2: integer);
var
  Temp: TSynEditStringRec;
begin
  if (Index1 < 0) or (Index1 >= fCount) then
    ListIndexOutOfBounds(Index1);
  if (Index2 < 0) or (Index2 >= fCount) then
    ListIndexOutOfBounds(Index2);
  BeginUpdate;
  Temp := fList^[Index1];
  fList^[Index1] := fList^[Index2];
  fList^[Index2] := Temp;
  if fIndexOfLongestLine = Index1 then
    fIndexOfLongestLine := Index2
  else if fIndexOfLongestLine = Index2 then
    fIndexOfLongestLine := Index1;
  EndUpdate;
end;

function TSynEditStringList.ExpandString(Index: integer): UnicodeString;
var
  HasTabs: Boolean;
begin
  with fList^[Index] do
    {$IFDEF OWN_UnicodeString_MEMMGR}
    if Length(TDynWideCharArray(FString)) = 0 then
    {$ELSE}
    if Length(FString) = 0 then
    {$ENDIF}
    begin
      Result := '';
      Exclude(fFlags, sfExpandedLengthUnknown);
      Exclude(fFlags, sfHasTabs);
      Include(fFlags, sfHasNoTabs);
      fExpandedLength := 0;
    end
    else
    begin
      Result := fConvertTabsProc(fstring, fTabWidth, HasTabs);
      fExpandedLength := Length(FExpandAtWideGlyphsFunc(Result));
      Exclude(fFlags, sfExpandedLengthUnknown);
      Exclude(fFlags, sfHasTabs);
      Exclude(fFlags, sfHasNoTabs);
      if HasTabs then
        Include(fFlags, sfHasTabs)
      else
        Include(fFlags, sfHasNoTabs);
    end;
end;

function TSynEditStringList.Get(Index: integer): UnicodeString;
{$IFDEF OWN_UnicodeString_MEMMGR}
var
  Len: Integer;
{$ENDIF OWN_UnicodeString_MEMMGR}
begin
  if Cardinal(Index)<Cardinal(fCount) then
    {$IFDEF OWN_UnicodeString_MEMMGR}
    with FList[Index] do
    begin
      Len := Length(TDynWideCharArray(FString));
      if Len > 0 then
      begin
        SetLength(Result, Len - 1); // exclude #0
        if Result <> '' then
          System.Move(FString^, Result[1], Len * SizeOf(WideChar));
      end
      else
        Result := '';
    end
    {$ELSE}
    Result := fList^[Index].fString
    {$ENDIF OWN_UnicodeString_MEMMGR}
  else
    Result := '';
end;

procedure TSynEditStringList.UpdateCharIndexes;
var
  i, n : Integer;
  p : PSynEditStringRec;
begin
  FCharIndexesAreValid:=True;
  if fCount=0 then Exit;
  p:=@fList^[0];
  n:=0;
  for i:=1 to fCount do begin
    p.fCharIndex:=n;
    Inc(n, Length(p.FString));
    Inc(p);
  end;
end;

function TSynEditStringList.LineCharLength(Index : Integer) : Integer;
begin
  if Cardinal(Index)<Cardinal(fCount) then
    Result:=Length(fList^[Index].fString)
  else Result:=0;
end;

function TSynEditStringList.LineCharIndex(Index : Integer) : Integer;
begin
  if Cardinal(Index)<Cardinal(fCount) then begin
    if not FCharIndexesAreValid then
      UpdateCharIndexes;
    Result:=fList^[Index].fCharIndex;
  end else Result:=0;
end;

function TSynEditStringList.GetCapacity: integer;
begin
  Result := fCapacity;
end;

function TSynEditStringList.GetCount: integer;
begin
  Result := fCount;
end;

function TSynEditStringList.GetExpandedString(Index: Integer): UnicodeString;
begin
  if (Index >= 0) and (Index < fCount) then
  begin
    if sfHasNoTabs in fList^[Index].fFlags then
      Result := Get(Index)
    else
      Result := ExpandString(Index);
  end else
    Result := '';
end;

function TSynEditStringList.GetExpandedStringLength(Index: integer): integer;
begin
  if (Index >= 0) and (Index < fCount) then
  begin
    if sfExpandedLengthUnknown in fList^[Index].fFlags then
      Result := Length( ExpandedStrings[index] )
    else
      Result := fList^[Index].fExpandedLength;
  end
  else
    Result := 0;
end;

function TSynEditStringList.GetLengthOfLongestLine: Integer;
var
  i, MaxLen: Integer;
  PRec: PSynEditStringRec;
begin
  if fIndexOfLongestLine < 0 then
  begin
    MaxLen := 0;
    if fCount > 0 then
    begin
      PRec := @fList^[0];
      for i := 0 to fCount - 1 do
      begin
        if sfExpandedLengthUnknown in PRec^.fFlags then
          ExpandString(i);
        if PRec^.fExpandedLength > MaxLen then
        begin
          MaxLen := PRec^.fExpandedLength;
          fIndexOfLongestLine := i;
        end;
        Inc(PRec);
      end;
    end;
  end;
  if (fIndexOfLongestLine >= 0) and (fIndexOfLongestLine < fCount) then
    Result := fList^[fIndexOfLongestLine].fExpandedLength
  else
    Result := 0;
end;

function TSynEditStringList.GetObject(Index: integer): TObject;
begin
  if (Index >= 0) and (Index < fCount) then
    Result := fList^[Index].fObject
  else
    Result := nil;
end;

function TSynEditStringList.GetRange(Index: integer): TSynEditRange;
begin
  if (Index >= 0) and (Index < fCount) then
    Result := fList^[Index].fRange
  else
    Result := nil;
end;

{$IFDEF UNICODE}
function TSynEditStringList.GetSeparatedText(Separators: UnicodeString): UnicodeString;
{Optimized by Eric Grange}
var
  I, L, Size, LineBreakSize: Integer;
  P, PLineBreak: PChar;
  PRec: PSynEditStringRec;
begin
  if fCount = 0 then begin
     Result := '';
     exit;
  end;
  LineBreakSize := Length(Separators);
  PLineBreak := Pointer(Separators);

  // compute buffer size
  Size :=   (fCount-1) * LineBreakSize
          + LineCharIndex( fCount-1 )
          + Length( fList^[fCount-1].FString );
  SetLength(Result, Size);

  P := Pointer(Result);
  PRec := @fList^[0];

  // handle 1st line separately (to avoid trailing line break)
  L := Length(PRec.FString);
  if L <> 0 then
  begin
    System.Move(Pointer(PRec.FString)^, P^, L * SizeOf(Char));
    Inc(P, L);
  end;
  Inc(PRec);

  for I := 1 to fCount-1 do
  begin
    case LineBreakSize of
      0 : ;
      1 : begin
        P^ := PLineBreak^;
        Inc(P);
      end;
      2 : begin
        PSynEditTwoWideChars(P)^ := PSynEditTwoWideChars(PLineBreak)^;
        Inc(P, 2);
      end;
    else
      System.Move(PLineBreak^, P^, LineBreakSize * SizeOf(Char));
      Inc(P, LineBreakSize);
    end;
    if Pointer( PRec.FString ) <> nil then
    begin
      L := Length(PRec.FString);
      System.Move(Pointer(PRec.FString)^, P^, L * SizeOf(Char));
      Inc(P, L);
    end;
    Inc(PRec);
  end;
end;
{$ENDIF}

function TSynEditStringList.GetTextStr: UnicodeString;
var
  LB: UnicodeString;
begin
  if not FStreaming then
  begin
    Result := GetSeparatedText(sLineBreak);
  end
  else
  begin
    case FileFormat of
      sffDos:
        LB := WideCRLF;
      sffUnix:
        LB := WideLF;
      sffMac:
        LB := WideCR;
      sffUnicode:
        LB := WideLineSeparator;
    end;
    Result := GetSeparatedText(LB);
    if AppendNewLineAtEOF then
      Result := Result + LB;
  end;
end;

procedure TSynEditStringList.Grow;
var
  Delta: Integer;
begin
  if fCapacity > 64 then
    Delta := fCapacity div 4
  else
    Delta := 16;
  SetCapacity(fCapacity + Delta);
end;

procedure TSynEditStringList.Insert(Index: integer; const S: UnicodeString);
begin
  if (Index < 0) or (Index > fCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  InsertItem(Index, S);
  if Assigned(fOnInserted) then
    fOnInserted( Self, Index, 1 );
  EndUpdate;
end;

procedure TSynEditStringList.InsertItem(Index: Integer; const S: UnicodeString);
begin
  BeginUpdate;
  if fCount = fCapacity then
    Grow;
  if Index < fCount then
  begin
    System.Move(fList^[Index], fList^[Index + 1],
      (fCount - Index) * SynEditStringRecSize);
  end;
  fIndexOfLongestLine := -1;                                                    
  with fList^[Index] do
  begin
    Pointer(fString) := nil;
    {$IFDEF OWN_UnicodeString_MEMMGR}
    SetListString(Index, S);
    {$ELSE}
    fString := S;
    {$ENDIF OWN_UnicodeString_MEMMGR}
    fObject := nil;
    fRange := NullRange;
    fExpandedLength := -1;
    fFlags := [sfExpandedLengthUnknown];
  end;
  Inc(fCount);
  EndUpdate;
end;

procedure TSynEditStringList.InsertLines(Index, NumLines: Integer);
var
  c_Line: Integer;
begin
  if (Index < 0) or (Index > fCount) then
    ListIndexOutOfBounds(Index);
  if NumLines > 0 then
  begin
    BeginUpdate;
    try
      SetCapacity(fCount + NumLines);
      if Index < fCount then
      begin
        System.Move(fList^[Index], fList^[Index + NumLines],
          (fCount - Index) * SynEditStringRecSize);
      end;
      for c_Line := Index to Index + NumLines -1 do
        with fList^[c_Line] do
        begin
          Pointer(fString) := nil;
          fObject := nil;
          fRange := NullRange;
          fExpandedLength := -1;
          fFlags := [sfExpandedLengthUnknown];
        end;
      Inc(fCount, NumLines);
      if Assigned(OnInserted) then
        OnInserted(Self, Index, NumLines);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSynEditStringList.InsertStrings(Index: integer;
  NewStrings: TUnicodeStrings);
var
  i, Cnt: integer;
begin
  Cnt := NewStrings.Count;
  if Cnt = 0 then exit;

  BeginUpdate;
  try
    InsertLines(Index, Cnt);
    for i := 0 to Cnt - 1 do
      Strings[Index + i] := NewStrings[i];
  finally
    EndUpdate;
  end;
end;

procedure TSynEditStringList.InsertText(Index: integer;
  NewText: UnicodeString);
var
  TmpStringList: TUnicodeStringList;
begin
  if NewText = '' then exit;

  TmpStringList := TUnicodeStringList.Create;
  try
    TmpStringList.Text := NewText;
    InsertStrings(Index, TmpStringList);
  finally
    TmpStringList.Free;
  end;
end;

procedure TSynEditStringList.LoadFromStream(Stream: TStream);
begin
  FStreaming := True;
  inherited;
  FStreaming := False;
end;

{$IFDEF UNICODE}
procedure TSynEditStringList.SaveToStream(Stream: TStream; Encoding: TEncoding);
begin
  FStreaming := True;
  inherited;
  FStreaming := False;
end;
{$ELSE}
procedure TSynEditStringList.SaveToStream(Stream: TStream; WithBOM: Boolean);
begin
  FStreaming := True;
  inherited;
  FStreaming := False;
end;
{$ENDIF}

procedure TSynEditStringList.Put(Index: integer; const S: UnicodeString);
begin
  if (Index = 0) and (fCount = 0) or (fCount = Index) then
    Add(S)
  else begin
    if Cardinal(Index)>=Cardinal(fCount) then
      ListIndexOutOfBounds(Index);
    BeginUpdate;
    fIndexOfLongestLine := -1;
    with fList^[Index] do begin
      Include(fFlags, sfExpandedLengthUnknown);
      Exclude(fFlags, sfHasTabs);
      Exclude(fFlags, sfHasNoTabs);
      {$IFDEF OWN_UnicodeString_MEMMGR}
        SetListString(Index, S);
      {$ELSE}
      fString := S;
      {$ENDIF OWN_UnicodeString_MEMMGR}
    end;
    if Assigned(fOnPutted) then
      fOnPutted( Self, Index, 1 );
    EndUpdate;
  end;
end;

procedure TSynEditStringList.PutObject(Index: integer; AObject: TObject);
begin
  if Cardinal(Index)>=Cardinal(fCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  fList^[Index].fObject := AObject;
  EndUpdate;
end;

procedure TSynEditStringList.PutRange(Index: integer; ARange: TSynEditRange);
begin
  if Cardinal(Index)>=Cardinal(fCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  fList^[Index].fRange := ARange;
  EndUpdate;
end;

procedure TSynEditStringList.SetCapacity(NewCapacity: integer);
{$IFDEF OWN_UnicodeString_MEMMGR}
var
  I : integer;
{$ENDIF OWN_UnicodeString_MEMMGR}
begin
  if NewCapacity < Count then
    EListError.Create( SInvalidCapacity );
  ReallocMem(fList, NewCapacity * SynEditStringRecSize);
  {$IFDEF OWN_UnicodeString_MEMMGR}
  for I := fCount to NewCapacity - 1 do
    Pointer(fList[I].fString) := nil;  // so that it does not get freed
  {$ENDIF OWN_UnicodeString_MEMMGR}
  fCapacity := NewCapacity;
end;

procedure TSynEditStringList.SetFileFormat(const Value: TSynEditFileFormat);
begin
  fFileFormat := Value;
{$IFDEF UNICODE}
  case FileFormat of
    sffDos:
      LineBreak := WideCRLF;
    sffUnix:
      LineBreak := WideLF;
    sffMac:
      LineBreak := WideCR;
    sffUnicode:
      LineBreak := WideLineSeparator;
  end;
{$ENDIF}
end;

{$IFDEF OWN_UnicodeString_MEMMGR}
procedure TSynEditStringList.SetListString(Index: Integer; const S: UnicodeString);
var
  Len: Integer;
  A: TDynWideCharArray;
begin
  with FList[Index] do
  begin
    Pointer(A) := TDynWideCharArray(FString);
    if A <> nil then
      A := nil; // free memory

    Len := Length(S);
    if Len > 0 then
    begin
      SetLength(A, Len + 1); // include #0
      System.Move(S[1], A[0], Len * SizeOf(WideChar));
      A[Len] := #0;
    end;

    FString := PWideChar(A);
    Pointer(A) := nil; // do not release the array on procedure exit
  end;
end;
{$ENDIF OWN_UnicodeString_MEMMGR}

procedure TSynEditStringList.SetTabWidth(Value: integer);
var
  i: integer;
begin
  if Value <> fTabWidth then begin
    fTabWidth := Value;
    fConvertTabsProc := GetBestConvertTabsProcEx(fTabWidth);
    fIndexOfLongestLine := -1;
    for i := 0 to fCount - 1 do
      with fList^[i] do begin
        fExpandedLength := -1;
        Exclude(fFlags, sfHasNoTabs);
        Include(fFlags, sfExpandedLengthUnknown);
      end;
  end;
end;

procedure TSynEditStringList.SetTextStr(const Value: UnicodeString);
var
  S: UnicodeString;
  Size: Integer;
  P, Start, Pmax: PWideChar;
  fCR, fLF, fLINESEPARATOR: Boolean;
begin
  fLINESEPARATOR := False;
  fCR := False;
  fLF := False;
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P <> nil then
    begin
      Size := Length(Value);
      Pmax := @Value[Size];
      while (P <= Pmax) do
      begin
        Start := P;
        while (P^ <> WideCR) and (P^ <> WideLF) and (P^ <> WideLineSeparator) and (P <= Pmax) do
        begin
          Inc(P);
        end;
        if P<>Start then
        begin
          SetString(S, Start, P - Start);
          InsertItem(fCount, S);
        end else InsertItem(fCount, '');
        if P^ = WideLineSeparator then
        begin
          fLINESEPARATOR := True;
          Inc(P);
        end;
        if P^ = WideCR then
        begin
          fCR := True;
          Inc(P);
        end;
        if P^ = WideLF then
        begin
          fLF := True;
          Inc(P);
        end;
      end;
      // keep the old format of the file
      if not AppendNewLineAtEOF and
        (CharInSet(Value[Size], [#10, #13]) or (Value[Size] = WideLineSeparator))
      then
        InsertItem(fCount, '');
    end;
    if Assigned(OnInserted) then
      OnInserted(Self, 0, fCount);
  finally
    EndUpdate;
  end;
  if fLINESEPARATOR then
    FileFormat := sffUnicode
  else if fCR and not fLF then
    FileFormat := sffMac
  else if fLF and not fCR then
    FileFormat := sffUnix
  else
    FileFormat := sffDos;
end;

procedure TSynEditStringList.SetUpdateState(Updating: Boolean);
begin
  FCharIndexesAreValid:=False;
  if Updating then begin
    if Assigned(fOnChanging) then
      fOnChanging(Self);
  end else begin
    if Assigned(fOnChange) then
      fOnChange(Self);
  end;
end;

procedure TSynEditStringList.FontChanged;
var
  i: Integer;
begin
  fIndexOfLongestLine := -1;
  for i := 0 to fCount - 1 do
    with fList^[i] do
    begin
      fExpandedLength := -1;
      Exclude(fFlags, sfHasNoTabs);
      Include(fFlags, sfExpandedLengthUnknown);
    end;
end;

{ TSynEditUndoItem }

procedure TSynEditUndoItem.Assign(Source: TPersistent);
begin
  if (Source is TSynEditUndoItem) then
  begin
    fChangeReason:=TSynEditUndoItem(Source).fChangeReason;
    fChangeSelMode:=TSynEditUndoItem(Source).fChangeSelMode;
    fChangeStartPos:=TSynEditUndoItem(Source).fChangeStartPos;
    fChangeEndPos:=TSynEditUndoItem(Source).fChangeEndPos;
    fChangeStr:=TSynEditUndoItem(Source).fChangeStr;
    fChangeNumber:=TSynEditUndoItem(Source).fChangeNumber;
  end
  else
    inherited Assign(Source);
end;


{ TSynEditUndoList }

constructor TSynEditUndoList.Create;
begin
  inherited Create;
  fItems := TList.Create;
  fMaxUndoActions := 1024;
  fNextChangeNumber := 1;
  fInsideRedo := False;
end;

destructor TSynEditUndoList.Destroy;
begin
  Clear;
  fItems.Free;
  inherited Destroy;
end;

procedure TSynEditUndoList.Assign(Source: TPersistent);
var
  i: Integer;
  UndoItem: TSynEditUndoItem;
begin
  if (Source is TSynEditUndoList) then
  begin
    Clear;
    for i:=0 to TSynEditUndoList(Source).fItems.Count-1 do
    begin
      UndoItem:=TSynEditUndoItem.Create;
      UndoItem.Assign(TSynEditUndoList(Source).fItems[i]);
      fItems.Add(UndoItem);
    end;
    fBlockChangeNumber:=TSynEditUndoList(Source).fBlockChangeNumber;
    fBlockCount:=TSynEditUndoList(Source).fBlockCount;
    fFullUndoImposible:=TSynEditUndoList(Source).fFullUndoImposible;
    fLockCount:=TSynEditUndoList(Source).fLockCount;
    fMaxUndoActions:=TSynEditUndoList(Source).fMaxUndoActions;
    fNextChangeNumber:=TSynEditUndoList(Source).fNextChangeNumber;
    fInsideRedo:=TSynEditUndoList(Source).fInsideRedo;
  end
  else
    inherited Assign(Source);
end;

procedure TSynEditUndoList.AddChange(AReason: TSynChangeReason; const AStart,
  AEnd: TBufferCoord; const ChangeText: UnicodeString; SelMode: TSynSelectionMode);
var
  NewItem: TSynEditUndoItem;
begin
  if fLockCount = 0 then begin
    NewItem := TSynEditUndoItem.Create;
    try
      with NewItem do begin
        fChangeReason := AReason;
        fChangeSelMode := SelMode;
        fChangeStartPos := AStart;
        fChangeEndPos := AEnd;
        fChangeStr := ChangeText;
        if fBlockChangeNumber <> 0 then
          fChangeNumber := fBlockChangeNumber
        else begin
          fChangeNumber := fNextChangeNumber;
          if fBlockCount = 0 then begin
            Inc(fNextChangeNumber);
            if fNextChangeNumber = 0 then
              Inc(fNextChangeNumber);
          end;
        end;
      end;
      PushItem(NewItem);
    except
      NewItem.Free;
      raise;
    end;
  end;
end;

procedure TSynEditUndoList.BeginBlock;
begin
  Inc(fBlockCount);
  fBlockChangeNumber := fNextChangeNumber;
end;

procedure TSynEditUndoList.Clear;
var
  i: integer;
begin
  for i := 0 to fItems.Count - 1 do
    TSynEditUndoItem(fItems[i]).Free;
  fItems.Clear;
  fFullUndoImposible := False;
end;

procedure TSynEditUndoList.EndBlock;
var
  iBlockID: integer;
begin
  if fBlockCount > 0 then begin
    Dec(fBlockCount);
    if fBlockCount = 0 then begin
      iBlockID := fBlockChangeNumber;
      fBlockChangeNumber := 0;
      Inc(fNextChangeNumber);
      if fNextChangeNumber = 0 then
        Inc(fNextChangeNumber);
      if (fItems.Count > 0) and (PeekItem.ChangeNumber = iBlockID) and
        Assigned(OnAddedUndo) then
      begin
        OnAddedUndo( Self );
      end;
    end;
  end;
end;

procedure TSynEditUndoList.EnsureMaxEntries;
var
  Item: TSynEditUndoItem;
begin
  if fItems.Count > fMaxUndoActions then 
  begin
    fFullUndoImposible := True;                                                 
    while fItems.Count > fMaxUndoActions do begin
      Item := fItems[0];
      Item.Free;
      fItems.Delete(0);
    end;
  end;
end;

function TSynEditUndoList.GetCanUndo: boolean;
begin
  Result := fItems.Count > 0;
end;

function TSynEditUndoList.GetItemCount: integer;
begin
  Result := fItems.Count;
end;

procedure TSynEditUndoList.Lock;
begin
  Inc(fLockCount);
end;

function TSynEditUndoList.PeekItem: TSynEditUndoItem;
var
  iLast: integer;
begin
  Result := nil;
  iLast := fItems.Count - 1;
  if iLast >= 0 then
    Result := fItems[iLast];
end;

function TSynEditUndoList.PopItem: TSynEditUndoItem;
var
  iLast: integer;
begin
  Result := nil;
  iLast := fItems.Count - 1;
  if iLast >= 0 then begin
    Result := fItems[iLast];
    fItems.Delete(iLast);
  end;
end;

procedure TSynEditUndoList.PushItem(Item: TSynEditUndoItem);
begin
  if Assigned(Item) then begin
    fItems.Add(Item);
    EnsureMaxEntries;
    if (Item.ChangeReason <> crGroupBreak) and Assigned(OnAddedUndo) then
      OnAddedUndo(Self);
  end;
end;

procedure TSynEditUndoList.SetMaxUndoActions(Value: integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> fMaxUndoActions then begin
    fMaxUndoActions := Value;
    EnsureMaxEntries;
  end;
end;

procedure TSynEditUndoList.Unlock;
begin
  if fLockCount > 0 then
    Dec(fLockCount);
end;

function TSynEditUndoList.LastChangeReason: TSynChangeReason;
begin
  if fItems.Count = 0 then
    result := crNothing
  else
    result := TSynEditUndoItem(fItems[fItems.Count - 1]).fChangeReason;
end;

procedure TSynEditUndoList.AddGroupBreak;
var
  vDummy: TBufferCoord;
begin
  //Add the GroupBreak even if ItemCount = 0. Since items are stored in
  //reverse order in TCustomSynEdit.fRedoList, a GroupBreak could be lost.
  if LastChangeReason <> crGroupBreak then
  begin
    AddChange(crGroupBreak, vDummy, vDummy, '', smNormal);
  end;
end;

procedure TSynEditUndoList.SetInitialState(const Value: boolean);
begin
  if Value then
  begin
    if ItemCount = 0 then
      fInitialChangeNumber := 0
    else
      fInitialChangeNumber := PeekItem.ChangeNumber;
  end
  else
    if ItemCount = 0 then
    begin
      if fInitialChangeNumber = 0 then
        fInitialChangeNumber := -1;
    end
    else if PeekItem.ChangeNumber = fInitialChangeNumber then
      fInitialChangeNumber := -1;
end;

function TSynEditUndoList.GetInitialState: boolean;
begin
  if ItemCount = 0 then
    Result := fInitialChangeNumber = 0
  else
    Result := PeekItem.ChangeNumber = fInitialChangeNumber;
end;

function TSynEditUndoList.GetItems(Index: Integer): TSynEditUndoItem;
begin
  Result := TSynEditUndoItem(fItems[Index]);
end;

procedure TSynEditUndoList.SetItems(Index: Integer;
  const Value: TSynEditUndoItem);
begin
  fItems[Index] := Value;
end;

procedure TSynEditUndoList.DeleteItem(AIndex: Integer);
begin
  TSynEditUndoItem(fItems[AIndex]).Free;
  fItems.Delete(AIndex);
end;

end.
