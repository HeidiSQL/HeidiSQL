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
  Windows,
  SynEditTypes,
  SynEditMiscProcs,
  SynUnicode,
  Classes,
  SysUtils,
  Graphics;

type
  TSynEditRange = Pointer;

  TSynEditStringFlag = (sfHasTabs, sfHasNoTabs, sfExpandedLengthUnknown,
    sfModified, sfSaved);
  TSynEditStringFlags = set of TSynEditStringFlag;

  PSynEditStringRec = ^TSynEditStringRec;
  TSynEditStringRec = record
    {$IFDEF OWN_UnicodeString_MEMMGR}
    FString: PWideChar; // "array of WideChar";
    {$ELSE}
    FString: UnicodeString;
    {$ENDIF OWN_UnicodeString_MEMMGR}
    FObject: TObject;
    FRange: TSynEditRange;
    FExpandedLength: Integer;
    fCharIndex : Integer;
    FFlags: TSynEditStringFlags;
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
    Count: Integer) of object;

  TExpandAtWideGlyphsFunc = function (const S: UnicodeString): UnicodeString of object;

  TSynEditFileFormat = (sffDos, sffUnix, sffMac, sffUnicode); // DOS: CRLF, UNIX: LF, Mac: CR, Unicode: LINE SEPARATOR

  TSynEditModification = (smOriginal, smModified, smSaved);

  TSynEditStringList = class(TUnicodeStrings)
  private
    FList: PSynEditStringRecList;
    FCount: Integer;
    FCapacity: Integer;
    FFileFormat: TSynEditFileFormat;
    FAppendNewLineAtEOF: Boolean;
    FConvertTabsProc: TConvertTabsProcEx;
    FIndexOfLongestLine: Integer;
    FTabWidth: Integer;
    FExpandAtWideGlyphsFunc: TExpandAtWideGlyphsFunc;
    FCharIndexesAreValid : Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOnCleared: TNotifyEvent;
    FOnDeleted: TStringListChangeEvent;
    FOnInserted: TStringListChangeEvent;
    FOnPutted: TStringListChangeEvent;
    function ExpandString(Index: Integer): UnicodeString;
    function GetExpandedString(Index: Integer): UnicodeString;
    function GetExpandedStringLength(Index: Integer): Integer;
    function GetLengthOfLongestLine: Integer;
    function GetRange(Index: Integer): TSynEditRange;
    function GetModification(Index: Integer): TSynEditModification;
    procedure Grow;
    procedure InsertItem(Index: Integer; const S: UnicodeString);
    procedure PutRange(Index: Integer; ARange: TSynEditRange);
    procedure SetFileFormat(const Value: TSynEditFileFormat);
    {$IFDEF OWN_UnicodeString_MEMMGR}
    procedure SetListString(Index: Integer; const S: UnicodeString);
    {$ENDIF OWN_UnicodeString_MEMMGR}
  protected
    FStreaming: Boolean;
    function Get(Index: Integer): UnicodeString; override;
    function GetCapacity: Integer;
      {$IFDEF SYN_COMPILER_3_UP} override; {$ENDIF}
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    function GetTextStr: UnicodeString; override;
    procedure Put(Index: Integer; const S: UnicodeString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer);
      {$IFDEF SYN_COMPILER_3_UP} override; {$ENDIF}
    procedure SetTabWidth(Value: Integer);
    procedure SetUpdateState(Updating: Boolean); override;
    procedure UpdateCharIndexes;
  public
    constructor Create(AExpandAtWideGlyphsFunc: TExpandAtWideGlyphsFunc);
    destructor Destroy; override;
    function Add(const S: UnicodeString): Integer; override;
    procedure AddStrings(Strings: TUnicodeStrings); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure DeleteLines(Index, NumLines: Integer);
    procedure Exchange(Index1, Index2: Integer); override;
    procedure Insert(Index: Integer; const S: UnicodeString); override;
    procedure InsertLines(Index, NumLines: Integer);
    procedure InsertStrings(Index: Integer; NewStrings: TUnicodeStrings);
    procedure InsertText(Index: Integer; NewText: UnicodeString);
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

    procedure MarkModifiedLinesAsSaved;
    procedure ResetModificationIndicator;

    property AppendNewLineAtEOF: Boolean read FAppendNewLineAtEOF write FAppendNewLineAtEOF;

    property FileFormat: TSynEditFileFormat read FFileFormat write SetFileFormat;
    property ExpandedStrings[Index: Integer]: UnicodeString read GetExpandedString;
    property ExpandedStringLengths[Index: Integer]: Integer read GetExpandedStringLength;
    property LengthOfLongestLine: Integer read GetLengthOfLongestLine;
    property Ranges[Index: Integer]: TSynEditRange read GetRange write PutRange;
    property Modification[Index: Integer]: TSynEditModification read GetModification;
    property TabWidth: Integer read FTabWidth write SetTabWidth;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnCleared: TNotifyEvent read FOnCleared write FOnCleared;
    property OnDeleted: TStringListChangeEvent read FOnDeleted write FOnDeleted;
    property OnInserted: TStringListChangeEvent read FOnInserted
      write FOnInserted;
    property OnPutted: TStringListChangeEvent read FOnPutted write FOnPutted;
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
    FChangeReason: TSynChangeReason;
    FChangeSelMode: TSynSelectionMode;
    FChangeStartPos: TBufferCoord;
    FChangeEndPos: TBufferCoord;
    FChangeStr: UnicodeString;
    FChangeNumber: Integer;
  public
    procedure Assign(Source: TPersistent); override;
    property ChangeReason: TSynChangeReason read FChangeReason;
    property ChangeSelMode: TSynSelectionMode read FChangeSelMode;
    property ChangeStartPos: TBufferCoord read FChangeStartPos;
    property ChangeEndPos: TBufferCoord read FChangeEndPos;
    property ChangeStr: UnicodeString read FChangeStr;
    property ChangeNumber: Integer read FChangeNumber;
  end;

  TSynEditUndoList = class(TPersistent)
  protected
    FBlockChangeNumber: Integer;
    FBlockCount: Integer;
    FFullUndoImposible: Boolean;
    FItems: TList;
    FLockCount: Integer;
    FMaxUndoActions: Integer;
    FNextChangeNumber: Integer;
    FInitialChangeNumber: Integer;
    FInsideRedo: Boolean;
    FOnAddedUndo: TNotifyEvent;
    procedure EnsureMaxEntries;
    function GetCanUndo: Boolean;
    function GetItemCount: Integer;
    procedure SetMaxUndoActions(Value: Integer);
    procedure SetInitialState(const Value: Boolean);
    function GetInitialState: Boolean;
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
    property BlockChangeNumber: Integer read FBlockChangeNumber
      write FBlockChangeNumber;
    property CanUndo: Boolean read GetCanUndo;
    property FullUndoImpossible: Boolean read FFullUndoImposible;
    property InitialState: Boolean read GetInitialState write SetInitialState;
    property Items[Index: Integer]: TSynEditUndoItem read GetItems write SetItems;
    property ItemCount: Integer read GetItemCount;
    property BlockCount: Integer read FBlockCount;
    property MaxUndoActions: Integer read FMaxUndoActions
      write SetMaxUndoActions;
    property InsideRedo: Boolean read FInsideRedo write FInsideRedo;
    property OnAddedUndo: TNotifyEvent read FOnAddedUndo write FOnAddedUndo;
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

procedure ListIndexOutOfBounds(Index: Integer);
begin
  raise ESynEditStringList.CreateFmt(SListIndexOutOfBounds, [Index]);
end;

constructor TSynEditStringList.Create(AExpandAtWideGlyphsFunc: TExpandAtWideGlyphsFunc);
begin
  inherited Create;
  FExpandAtWideGlyphsFunc := AExpandAtWideGlyphsFunc;
  SetFileFormat(sffDos);
  FIndexOfLongestLine := -1;
  TabWidth := 8;
end;

destructor TSynEditStringList.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  inherited Destroy;
  {$IFDEF OWN_UnicodeString_MEMMGR}
  FOnCleared := nil;
  Clear;
  {$ELSE}
  if FCount <> 0 then
    Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
  {$ENDIF OWN_UnicodeString_MEMMGR}
end;

function TSynEditStringList.Add(const S: UnicodeString): Integer;
begin
  BeginUpdate;
  Result := FCount;
  InsertItem(Result, S);
  if Assigned(OnInserted) then
    OnInserted(Self, Result, 1);
  EndUpdate;
end;

procedure TSynEditStringList.AddStrings(Strings: TUnicodeStrings);
var
  i, FirstAdded: Integer;
begin
  if Strings.Count > 0 then begin
    FIndexOfLongestLine := -1;
    BeginUpdate;
    try
      i := FCount + Strings.Count;
      if i > FCapacity then
        SetCapacity((i + 15) and (not 15));
      FirstAdded := FCount;
      for i := 0 to Strings.Count - 1 do begin
        with FList^[FCount] do begin
          Pointer(FString) := nil;
          {$IFDEF OWN_UnicodeString_MEMMGR}
          SetListString(FCount, Strings[i]);
          {$ELSE}
          FString := Strings[i];
          {$ENDIF OWN_UnicodeString_MEMMGR}
          FObject := Strings.Objects[i];
          FRange := NullRange;
          FExpandedLength := -1;
          FFlags := [sfExpandedLengthUnknown];
        end;
        Inc(FCount);
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
  if FCount <> 0 then begin
    BeginUpdate;
    {$IFDEF OWN_UnicodeString_MEMMGR}
    for I := 0 to FCount - 1 do
      with FList[I] do
        if TDynWideCharArray(FString) <> nil then
          TDynWideCharArray(FString) := nil;
    {$ELSE}
    Finalize(FList^[0], FCount);
    {$ENDIF OWN_UnicodeString_MEMMGR}
    FCount := 0;
    SetCapacity(0);
    if Assigned(FOnCleared) then
      FOnCleared(Self);
    EndUpdate;
  end;
  FIndexOfLongestLine := -1;
end;

procedure TSynEditStringList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  {$IFDEF OWN_UnicodeString_MEMMGR}
  SetListString(Index, '');
  {$ELSE}
  Finalize(FList^[Index]);
  {$ENDIF OWN_UnicodeString_MEMMGR}
  Dec(FCount);
  if Index < FCount then begin
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SynEditStringRecSize);
    {$IFDEF OWN_UnicodeString_MEMMGR}
    Pointer(FList[FCount].FString) := nil; // avoid freeing the string, the address is now used in another element
    {$ENDIF OWN_UnicodeString_MEMMGR}
  end;
  FIndexOfLongestLine := -1;
  if Assigned(FOnDeleted) then
    FOnDeleted( Self, Index, 1 );
  EndUpdate;
end;

procedure TSynEditStringList.DeleteLines(Index, NumLines: Integer);
var
  LinesAfter: Integer;
{$IFDEF OWN_UnicodeString_MEMMGR}
  I: Integer;
{$ENDIF OWN_UnicodeString_MEMMGR}
begin
  if NumLines > 0 then begin
    if (Index < 0) or (Index >= FCount) then
      ListIndexOutOfBounds(Index);
    LinesAfter := FCount - (Index + NumLines);
    if LinesAfter < 0 then
      NumLines := FCount - Index;
    {$IFDEF OWN_UnicodeString_MEMMGR}
    for I := Index to Index + NumLines - 1 do
      with FList[I] do
        if TDynWideCharArray(FString) <> nil then
          TDynWideCharArray(FString) := nil;
    {$ELSE}
    Finalize(FList^[Index], NumLines);
    {$ENDIF OWN_UnicodeString_MEMMGR}

    if LinesAfter > 0 then begin
      BeginUpdate;
      try
        System.Move(FList^[Index + NumLines], FList^[Index],
          LinesAfter * SynEditStringRecSize);
      finally
        EndUpdate;
      end;
    end;
    Dec(FCount, NumLines);
    if Assigned(FOnDeleted) then
      FOnDeleted( Self, Index, NumLines );
  end;
end;

procedure TSynEditStringList.Exchange(Index1, Index2: Integer);
var
  Temp: TSynEditStringRec;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    ListIndexOutOfBounds(Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    ListIndexOutOfBounds(Index2);
  BeginUpdate;
  Temp := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Temp;
  if FIndexOfLongestLine = Index1 then
    FIndexOfLongestLine := Index2
  else if FIndexOfLongestLine = Index2 then
    FIndexOfLongestLine := Index1;
  EndUpdate;
end;

function TSynEditStringList.ExpandString(Index: Integer): UnicodeString;
var
  HasTabs: Boolean;
begin
  with FList^[Index] do
    {$IFDEF OWN_UnicodeString_MEMMGR}
    if Length(TDynWideCharArray(FString)) = 0 then
    {$ELSE}
    if Length(FString) = 0 then
    {$ENDIF}
    begin
      Result := '';
      Exclude(FFlags, sfExpandedLengthUnknown);
      Exclude(FFlags, sfHasTabs);
      Include(FFlags, sfHasNoTabs);
      FExpandedLength := 0;
    end
    else
    begin
      Result := FConvertTabsProc(FString, FTabWidth, HasTabs);
      FExpandedLength := Length(FExpandAtWideGlyphsFunc(Result));
      Exclude(FFlags, sfExpandedLengthUnknown);
      Exclude(FFlags, sfHasTabs);
      Exclude(FFlags, sfHasNoTabs);
      if HasTabs then
        Include(FFlags, sfHasTabs)
      else
        Include(FFlags, sfHasNoTabs);
    end;
end;

function TSynEditStringList.Get(Index: Integer): UnicodeString;
{$IFDEF OWN_UnicodeString_MEMMGR}
var
  Len: Integer;
{$ENDIF OWN_UnicodeString_MEMMGR}
begin
  if Cardinal(Index)<Cardinal(FCount) then
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
    Result := FList^[Index].FString
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
  if FCount=0 then Exit;
  p:=@FList^[0];
  n:=0;
  for i:=1 to FCount do begin
    p.fCharIndex:=n;
    Inc(n, Length(p.FString));
    Inc(p);
  end;
end;

function TSynEditStringList.LineCharLength(Index : Integer) : Integer;
begin
  if Cardinal(Index)<Cardinal(FCount) then
    Result:=Length(FList^[Index].FString)
  else Result:=0;
end;

function TSynEditStringList.LineCharIndex(Index : Integer) : Integer;
begin
  if Cardinal(Index)<Cardinal(FCount) then begin
    if not FCharIndexesAreValid then
      UpdateCharIndexes;
    Result:=FList^[Index].fCharIndex;
  end else Result:=0;
end;

function TSynEditStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TSynEditStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TSynEditStringList.GetExpandedString(Index: Integer): UnicodeString;
begin
  if (Index >= 0) and (Index < FCount) then
  begin
    if sfHasNoTabs in FList^[Index].FFlags then
      Result := Get(Index)
    else
      Result := ExpandString(Index);
  end else
    Result := '';
end;

function TSynEditStringList.GetExpandedStringLength(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < FCount) then
  begin
    if sfExpandedLengthUnknown in FList^[Index].FFlags then
      Result := Length( ExpandedStrings[index] )
    else
      Result := FList^[Index].FExpandedLength;
  end
  else
    Result := 0;
end;

function TSynEditStringList.GetLengthOfLongestLine: Integer;
var
  i, MaxLen: Integer;
  PRec: PSynEditStringRec;
begin
  if FIndexOfLongestLine < 0 then
  begin
    MaxLen := 0;
    if FCount > 0 then
    begin
      PRec := @FList^[0];
      for i := 0 to FCount - 1 do
      begin
        if sfExpandedLengthUnknown in PRec^.FFlags then
          ExpandString(i);
        if PRec^.FExpandedLength > MaxLen then
        begin
          MaxLen := PRec^.FExpandedLength;
          FIndexOfLongestLine := i;
        end;
        Inc(PRec);
      end;
    end;
  end;
  if (FIndexOfLongestLine >= 0) and (FIndexOfLongestLine < FCount) then
    Result := FList^[FIndexOfLongestLine].FExpandedLength
  else
    Result := 0;
end;

function TSynEditStringList.GetModification(
  Index: Integer): TSynEditModification;
begin
  Result := smOriginal;
  if (Index >= 0) and (Index < FCount) then
    if sfSaved in FList^[Index].FFlags then
      Result := smSaved
    else
    if sfModified in FList^[Index].FFlags then
      Result := smModified;
end;

function TSynEditStringList.GetObject(Index: Integer): TObject;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FList^[Index].FObject
  else
    Result := nil;
end;

function TSynEditStringList.GetRange(Index: Integer): TSynEditRange;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FList^[Index].FRange
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
  if FCount = 0 then
  begin
    Result := '';
    Exit;
  end;
  LineBreakSize := Length(Separators);
  PLineBreak := Pointer(Separators);

  // compute buffer size
  Size :=   (FCount - 1) * LineBreakSize
          + LineCharIndex( FCount - 1 )
          + Length( FList^[FCount - 1].FString );
  SetLength(Result, Size);

  P := Pointer(Result);
  PRec := @FList^[0];

  // handle 1st line separately (to avoid trailing line break)
  L := Length(PRec.FString);
  if L <> 0 then
  begin
    System.Move(Pointer(PRec.FString)^, P^, L * SizeOf(Char));
    Inc(P, L);
  end;
  Inc(PRec);

  for I := 1 to FCount-1 do
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
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    Delta := 16;
  SetCapacity(FCapacity + Delta);
end;

procedure TSynEditStringList.Insert(Index: Integer; const S: UnicodeString);
begin
  if (Index < 0) or (Index > FCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  InsertItem(Index, S);
  if Assigned(FOnInserted) then
    FOnInserted( Self, Index, 1 );
  EndUpdate;
end;

procedure TSynEditStringList.InsertItem(Index: Integer; const S: UnicodeString);
begin
  BeginUpdate;
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
  begin
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SynEditStringRecSize);
  end;
  FIndexOfLongestLine := -1;
  with FList^[Index] do
  begin
    Pointer(FString) := nil;
    {$IFDEF OWN_UnicodeString_MEMMGR}
    SetListString(Index, S);
    {$ELSE}
    FString := S;
    {$ENDIF OWN_UnicodeString_MEMMGR}
    FObject := nil;
    FRange := NullRange;
    FExpandedLength := -1;
    FFlags := [sfExpandedLengthUnknown];
    Include(FFlags, sfModified);
  end;
  Inc(FCount);
  EndUpdate;
end;

procedure TSynEditStringList.InsertLines(Index, NumLines: Integer);
var
  c_Line: Integer;
begin
  if (Index < 0) or (Index > FCount) then
    ListIndexOutOfBounds(Index);
  if NumLines > 0 then
  begin
    BeginUpdate;
    try
      SetCapacity(FCount + NumLines);
      if Index < FCount then
      begin
        System.Move(FList^[Index], FList^[Index + NumLines],
          (FCount - Index) * SynEditStringRecSize);
      end;
      for c_Line := Index to Index + NumLines -1 do
        with FList^[c_Line] do
        begin
          Pointer(FString) := nil;
          FObject := nil;
          FRange := NullRange;
          FExpandedLength := -1;
          FFlags := [sfExpandedLengthUnknown];
        end;
      Inc(FCount, NumLines);
      if Assigned(OnInserted) then
        OnInserted(Self, Index, NumLines);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSynEditStringList.InsertStrings(Index: Integer;
  NewStrings: TUnicodeStrings);
var
  i, Cnt: Integer;
begin
  Cnt := NewStrings.Count;
  if Cnt = 0 then Exit;

  BeginUpdate;
  try
    InsertLines(Index, Cnt);
    for i := 0 to Cnt - 1 do
      Strings[Index + i] := NewStrings[i];
  finally
    EndUpdate;
  end;
end;

procedure TSynEditStringList.InsertText(Index: Integer;
  NewText: UnicodeString);
var
  TmpStringList: TUnicodeStringList;
begin
  if NewText = '' then Exit;

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
  ResetModificationIndicator;
end;

procedure TSynEditStringList.MarkModifiedLinesAsSaved;
var
  Index: Integer;
begin
  for Index := 0 to FCount - 1 do
    if sfModified in FList^[Index].FFlags then
      Include(FList^[Index].FFlags, sfSaved);
end;

procedure TSynEditStringList.ResetModificationIndicator;
var
  Index: Integer;
begin
  for Index := 0 to FCount - 1 do
  begin
    Exclude(FList^[Index].FFlags, sfModified);
    Exclude(FList^[Index].FFlags, sfSaved);
  end;
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

procedure TSynEditStringList.Put(Index: Integer; const S: UnicodeString);
begin
  if (Index = 0) and (FCount = 0) or (FCount = Index) then
    Add(S)
  else begin
    if Cardinal(Index)>=Cardinal(FCount) then
      ListIndexOutOfBounds(Index);
    BeginUpdate;
    FIndexOfLongestLine := -1;
    with FList^[Index] do begin
      Include(FFlags, sfExpandedLengthUnknown);
      Exclude(FFlags, sfHasTabs);
      Exclude(FFlags, sfHasNoTabs);
      Include(FFlags, sfModified);
      Exclude(FFlags, sfSaved);
      {$IFDEF OWN_UnicodeString_MEMMGR}
        SetListString(Index, S);
      {$ELSE}
      FString := S;
      {$ENDIF OWN_UnicodeString_MEMMGR}
    end;
    if Assigned(FOnPutted) then
      FOnPutted( Self, Index, 1 );
    EndUpdate;
  end;
end;

procedure TSynEditStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if Cardinal(Index)>=Cardinal(FCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  FList^[Index].FObject := AObject;
  EndUpdate;
end;

procedure TSynEditStringList.PutRange(Index: Integer; ARange: TSynEditRange);
begin
  if Cardinal(Index)>=Cardinal(FCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  FList^[Index].FRange := ARange;
  EndUpdate;
end;

procedure TSynEditStringList.SetCapacity(NewCapacity: Integer);
{$IFDEF OWN_UnicodeString_MEMMGR}
var
  I : Integer;
{$ENDIF OWN_UnicodeString_MEMMGR}
begin
  if NewCapacity < Count then
    EListError.Create( SInvalidCapacity );
  ReallocMem(FList, NewCapacity * SynEditStringRecSize);
  {$IFDEF OWN_UnicodeString_MEMMGR}
  for I := FCount to NewCapacity - 1 do
    Pointer(FList[I].FString) := nil;  // so that it does not get freed
  {$ENDIF OWN_UnicodeString_MEMMGR}
  FCapacity := NewCapacity;
end;

procedure TSynEditStringList.SetFileFormat(const Value: TSynEditFileFormat);
begin
  FFileFormat := Value;
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
    Pointer(A) := nil; // do not release the array on procedure Exit
  end;
end;
{$ENDIF OWN_UnicodeString_MEMMGR}

procedure TSynEditStringList.SetTabWidth(Value: Integer);
var
  i: Integer;
begin
  if Value <> FTabWidth then begin
    FTabWidth := Value;
    FConvertTabsProc := GetBestConvertTabsProcEx(FTabWidth);
    FIndexOfLongestLine := -1;
    for i := 0 to FCount - 1 do
      with FList^[i] do begin
        FExpandedLength := -1;
        Exclude(FFlags, sfHasNoTabs);
        Include(FFlags, sfExpandedLengthUnknown);
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
          InsertItem(FCount, S);
        end else InsertItem(FCount, '');
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
        InsertItem(FCount, '');
    end;
    if Assigned(OnInserted) and (FCount > 0) then
      OnInserted(Self, 0, FCount);
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
    if Assigned(FOnChanging) then
      FOnChanging(Self);
  end else begin
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSynEditStringList.FontChanged;
var
  i: Integer;
begin
  FIndexOfLongestLine := -1;
  for i := 0 to FCount - 1 do
    with FList^[i] do
    begin
      FExpandedLength := -1;
      Exclude(FFlags, sfHasNoTabs);
      Include(FFlags, sfExpandedLengthUnknown);
    end;
end;

{ TSynEditUndoItem }

procedure TSynEditUndoItem.Assign(Source: TPersistent);
begin
  if (Source is TSynEditUndoItem) then
  begin
    FChangeReason:=TSynEditUndoItem(Source).FChangeReason;
    FChangeSelMode:=TSynEditUndoItem(Source).FChangeSelMode;
    FChangeStartPos:=TSynEditUndoItem(Source).FChangeStartPos;
    FChangeEndPos:=TSynEditUndoItem(Source).FChangeEndPos;
    FChangeStr:=TSynEditUndoItem(Source).FChangeStr;
    FChangeNumber:=TSynEditUndoItem(Source).FChangeNumber;
  end
  else
    inherited Assign(Source);
end;


{ TSynEditUndoList }

constructor TSynEditUndoList.Create;
begin
  inherited Create;
  FItems := TList.Create;
  FMaxUndoActions := 1024;
  FNextChangeNumber := 1;
  FInsideRedo := False;
end;

destructor TSynEditUndoList.Destroy;
begin
  Clear;
  FItems.Free;
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
    for i:=0 to TSynEditUndoList(Source).FItems.Count-1 do
    begin
      UndoItem:=TSynEditUndoItem.Create;
      UndoItem.Assign(TSynEditUndoList(Source).FItems[i]);
      FItems.Add(UndoItem);
    end;
    FBlockChangeNumber:=TSynEditUndoList(Source).FBlockChangeNumber;
    FBlockCount:=TSynEditUndoList(Source).FBlockCount;
    FFullUndoImposible:=TSynEditUndoList(Source).FFullUndoImposible;
    FLockCount:=TSynEditUndoList(Source).FLockCount;
    FMaxUndoActions:=TSynEditUndoList(Source).FMaxUndoActions;
    FNextChangeNumber:=TSynEditUndoList(Source).FNextChangeNumber;
    FInsideRedo:=TSynEditUndoList(Source).FInsideRedo;
  end
  else
    inherited Assign(Source);
end;

procedure TSynEditUndoList.AddChange(AReason: TSynChangeReason; const AStart,
  AEnd: TBufferCoord; const ChangeText: UnicodeString; SelMode: TSynSelectionMode);
var
  NewItem: TSynEditUndoItem;
begin
  if FLockCount = 0 then begin
    NewItem := TSynEditUndoItem.Create;
    try
      with NewItem do begin
        FChangeReason := AReason;
        FChangeSelMode := SelMode;
        FChangeStartPos := AStart;
        FChangeEndPos := AEnd;
        FChangeStr := ChangeText;
        if FBlockChangeNumber <> 0 then
          FChangeNumber := FBlockChangeNumber
        else begin
          FChangeNumber := FNextChangeNumber;
          if FBlockCount = 0 then begin
            Inc(FNextChangeNumber);
            if FNextChangeNumber = 0 then
              Inc(FNextChangeNumber);
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
  Inc(FBlockCount);
  FBlockChangeNumber := FNextChangeNumber;
end;

procedure TSynEditUndoList.Clear;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    TSynEditUndoItem(FItems[i]).Free;
  FItems.Clear;
  FFullUndoImposible := False;
end;

procedure TSynEditUndoList.EndBlock;
var
  iBlockID: Integer;
begin
  if FBlockCount > 0 then begin
    Dec(FBlockCount);
    if FBlockCount = 0 then begin
      iBlockID := FBlockChangeNumber;
      FBlockChangeNumber := 0;
      Inc(FNextChangeNumber);
      if FNextChangeNumber = 0 then
        Inc(FNextChangeNumber);
      if (FItems.Count > 0) and (PeekItem.ChangeNumber = iBlockID) and
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
  if FItems.Count > FMaxUndoActions then
  begin
    FFullUndoImposible := True;
    while FItems.Count > FMaxUndoActions do begin
      Item := FItems[0];
      Item.Free;
      FItems.Delete(0);
    end;
  end;
end;

function TSynEditUndoList.GetCanUndo: Boolean;
begin
  Result := FItems.Count > 0;
end;

function TSynEditUndoList.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TSynEditUndoList.Lock;
begin
  Inc(FLockCount);
end;

function TSynEditUndoList.PeekItem: TSynEditUndoItem;
var
  iLast: Integer;
begin
  Result := nil;
  iLast := FItems.Count - 1;
  if iLast >= 0 then
    Result := FItems[iLast];
end;

function TSynEditUndoList.PopItem: TSynEditUndoItem;
var
  iLast: Integer;
begin
  Result := nil;
  iLast := FItems.Count - 1;
  if iLast >= 0 then begin
    Result := FItems[iLast];
    FItems.Delete(iLast);
  end;
end;

procedure TSynEditUndoList.PushItem(Item: TSynEditUndoItem);
begin
  if Assigned(Item) then begin
    FItems.Add(Item);
    EnsureMaxEntries;
    if (Item.ChangeReason <> crGroupBreak) and Assigned(OnAddedUndo) then
      OnAddedUndo(Self);
  end;
end;

procedure TSynEditUndoList.SetMaxUndoActions(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FMaxUndoActions then begin
    FMaxUndoActions := Value;
    EnsureMaxEntries;
  end;
end;

procedure TSynEditUndoList.Unlock;
begin
  if FLockCount > 0 then
    Dec(FLockCount);
end;

function TSynEditUndoList.LastChangeReason: TSynChangeReason;
begin
  if FItems.Count = 0 then
    Result := crNothing
  else
    Result := TSynEditUndoItem(FItems[FItems.Count - 1]).FChangeReason;
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

procedure TSynEditUndoList.SetInitialState(const Value: Boolean);
begin
  if Value then
  begin
    if ItemCount = 0 then
      FInitialChangeNumber := 0
    else
      FInitialChangeNumber := PeekItem.ChangeNumber;
  end
  else
    if ItemCount = 0 then
    begin
      if FInitialChangeNumber = 0 then
        FInitialChangeNumber := -1;
    end
    else if PeekItem.ChangeNumber = FInitialChangeNumber then
      FInitialChangeNumber := -1;
end;

function TSynEditUndoList.GetInitialState: Boolean;
begin
  if ItemCount = 0 then
    Result := FInitialChangeNumber = 0
  else
    Result := PeekItem.ChangeNumber = FInitialChangeNumber;
end;

function TSynEditUndoList.GetItems(Index: Integer): TSynEditUndoItem;
begin
  Result := TSynEditUndoItem(FItems[Index]);
end;

procedure TSynEditUndoList.SetItems(Index: Integer;
  const Value: TSynEditUndoItem);
begin
  FItems[Index] := Value;
end;

procedure TSynEditUndoList.DeleteItem(AIndex: Integer);
begin
  TSynEditUndoItem(FItems[AIndex]).Free;
  FItems.Delete(AIndex);
end;

end.
