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

$Id: SynEditTextBuffer.pas,v 1.27 2002/05/03 13:11:25 harmeister Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditTextBuffer;

{$I SynEdit.inc}

interface

uses
  Classes, SysUtils,
{$IFDEF SYN_CLX}
  kTextDrawer,
  Types,
{$ELSE}
  Windows,
{$ENDIF}
  SynEditTypes,
  SynEditMiscProcs;

type
{begin}                                                                         //mh 2000-10-10
  TSynEditRange = pointer;

{begin}                                                                         //mh 2000-10-19
  TSynEditStringFlag = (sfHasTabs, sfHasNoTabs, sfExpandedLengthUnknown);
  TSynEditStringFlags = set of TSynEditStringFlag;
{end}                                                                           //mh 2000-10-19

  PSynEditStringRec = ^TSynEditStringRec;
  TSynEditStringRec = record
    fString: string;
    fObject: TObject;
    fRange: TSynEditRange;
    fWraped: Boolean;                                                           //Fiala 2001-12-17
{begin}                                                                         //mh 2000-10-19
    fExpandedLength: integer;
    fFlags: TSynEditStringFlags;
{end}                                                                           //mh 2000-10-19
  end;

const
  SynEditStringRecSize = SizeOf(TSynEditStringRec);
  MaxSynEditStrings = MaxInt div SynEditStringRecSize;

  NullRange = TSynEditRange(-1);

type
  PSynEditStringRecList = ^TSynEditStringRecList;
  TSynEditStringRecList = array[0..MaxSynEditStrings - 1] of TSynEditStringRec;

  TStringListIndexEvent = procedure(Index: Integer) of object;
  TStringListInsertedEvent = procedure(Index: Integer; const s: String) of object;

  TSynEditFileFormat = (sffDos, sffUnix, sffMac); // DOS: CRLF, UNIX: LF, Mac: CR

  TSynEditStringList = class(TStrings)
  private
    fWordWrap : Boolean;                                                        //Fiala 2001-12-17
    fWordWrapWidth : Integer;                                                   //Fiala 2001-12-17
    fList: PSynEditStringRecList;
    fCount: integer;
    fCapacity: integer;
    fFileFormat: TSynEditFileFormat;
    fAppendNewLineAtEOF: Boolean;                                               //gbn 2002-04-25
{begin}                                                                         //mh 2000-10-19
    fConvertTabsProc: TConvertTabsProcEx;
    fIndexOfLongestLine: integer;
    fTabWidth: integer;
{end}                                                                           //mh 2000-10-19
    fOnChange: TNotifyEvent;
    fOnChanging: TNotifyEvent;
    function WrapString(var InputString: String) : String;                      //Fiala 2001-12-17
    procedure WrapLine(const LineNumber: Integer);                              //Fiala 2001-12-17
{begin}                                                                         //mh 2000-10-19
    function ExpandedString(Index: integer): string;
    function GetExpandedString(Index: integer): string;
    function GetLengthOfLongestLine: integer;
{end}                                                                           //mh 2000-10-19
    function GetRange(Index: integer): TSynEditRange;
    procedure Grow;
    procedure InsertItem(Index: integer; const S: string);
    procedure PutRange(Index: integer; ARange: TSynEditRange);
    procedure SetWordWrap(const Value: boolean);                                //Fiala 2001-12-17
  protected
    fLongestLineIndex: integer;                                                 //mh 2000-10-19
    fOnAdded: TStringListInsertedEvent;
    fOnCleared: TNotifyEvent;
    fOnDeleted: TStringListIndexEvent;
    fOnInserted: TStringListInsertedEvent;
    fOnPutted: TStringListInsertedEvent;

    function Get(Index: integer): string; override;
    function GetCapacity: integer;
      {$IFDEF SYN_COMPILER_3_UP} override; {$ENDIF}                             //mh 2000-10-18
    function GetCount: integer; override;
    function GetObject(Index: integer): TObject; override;
    procedure Put(Index: integer; const S: string); override;
    procedure PutObject(Index: integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: integer);
      {$IFDEF SYN_COMPILER_3_UP} override; {$ENDIF}                             //mh 2000-10-18
    procedure SetTabWidth(Value: integer);                                      //mh 2000-10-19
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const S: string): integer; override;
    procedure AddStrings(Strings: TStrings); override;
    procedure Clear; override;
    procedure Delete(Index: integer); override;
    procedure DeleteLines(Index, NumLines: integer);                            // DJLP 2000-11-01
    procedure Exchange(Index1, Index2: integer); override;
    procedure Insert(Index: integer; const S: string); override;
    procedure InsertLines(Index, NumLines: integer);                            // DJLP 2000-11-01
    procedure InsertStrings(Index: integer; NewStrings: TStrings);              // DJLP 2000-11-01
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
{begin}                                                                         //Fiala 2001-12-17
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure DoWrapLines(const FromLine, ToLine: Integer);
    procedure DoUnWrapLines(const FromLine, ToLine: Integer);
    function ReWrapLine(const LineNumber: Integer): Integer;
    function IsLineWraped(const LineNumber: Integer): Boolean;
    procedure DoWordWrap;
    procedure DoWordUnWrap;
    property AppendNewLineAtEOF: Boolean read fAppendNewLineAtEOF write fAppendNewLineAtEOF;  //gbn 2002-04-25

    property WordWrap: boolean read FWordWrap write SetWordWrap;
    property WordWrapWidth: integer read FWordWrapWidth write fWordWrapWidth;
    property FileFormat: TSynEditFileFormat read fFileFormat write fFileFormat;
{end}                                                                           //Fiala 2001-12-17
{begin}                                                                         //mh 2000-10-19
    property ExpandedStrings[Index: integer]: string read GetExpandedString;
    property LengthOfLongestLine: integer read GetLengthOfLongestLine;
{end}                                                                           //mh 2000-10-19
    property Ranges[Index: integer]: TSynEditRange read GetRange write PutRange;
    property TabWidth: integer read fTabWidth write SetTabWidth;                //mh 2000-10-19
    property OnAdded: TStringListInsertedEvent read fOnAdded write fOnAdded;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property OnChanging: TNotifyEvent read fOnChanging write fOnChanging;
    property OnCleared: TNotifyEvent read fOnCleared write fOnCleared;
    property OnDeleted: TStringListIndexEvent read fOnDeleted write fOnDeleted;
    property OnInserted: TStringListInsertedEvent read fOnInserted
      write fOnInserted;
    property OnPutted: TStringListInsertedEvent read fOnPutted write fOnPutted;
  end;

  ESynEditStringList = class(Exception);
{end}                                                                           //mh 2000-10-10

  TSynChangeReason = (crInsert, crPaste, crDragDropInsert,
    // Note: crSelDelete and crDragDropDelete have been deleted, because
    //   several undo entries can be chained together now via the ChangeNumber
    //   see also TCustomSynEdit.[Begin|End]UndoBlock methods
    crDeleteAfterCursor, crDelete, {crSelDelete, crDragDropDelete, }            //mh 2000-11-20
    crLineBreak, crIndent, crUnindent,
    crSilentDelete, crSilentDeleteAfterCursor,                                  //mh 2000-10-30
    crAutoCompleteBegin, crAutoCompleteEnd,                                     //DDH 10/16/01 for AutoComplete
    crSpecial1Begin, crSpecial1End,                                             //DDH 10/16/01 for Special1
    crSpecial2Begin, crSpecial2End,                                             //DDH 10/16/01 for Special2
    crNothing,
    crGroupBreak,                                                               //ek 2000-11-04
    crDeleteAll, crWrap, crUnWrap,                                              //Fiala 2001-12-17
    crWhiteSpaceAdd                                                             //DDH 2001-1-7 for undo/redo of adding a character past EOL and repositioning the caret
    );

  TSynEditUndoItem = class(TPersistent)
  protected
    fChangeReason: TSynChangeReason;
    fChangeSelMode: TSynSelectionMode;
    fChangeStartPos: TPoint;
    fChangeEndPos: TPoint;
    fChangeStr: string;
    fChangeNumber: integer;                                                     //sbs 2000-11-19
  public
    procedure Assign(Source: TPersistent); override;
  { public properties }
    property ChangeReason: TSynChangeReason read fChangeReason;
    property ChangeSelMode: TSynSelectionMode read fChangeSelMode;
    property ChangeStartPos: TPoint read fChangeStartPos;
    property ChangeEndPos: TPoint read fChangeEndPos;
    property ChangeStr: string read fChangeStr;
    property ChangeNumber: integer read fChangeNumber;
  end;

  TSynEditUndoList = class(TPersistent)
  private
    fBlockChangeNumber: integer;                                                //sbs 2000-11-19
    fBlockCount: integer;                                                       //sbs 2000-11-19
    fFullUndoImposible: boolean;                                                //mh 2000-10-03
    fItems: TList;
    fLockCount: integer;
    fMaxUndoActions: integer;
    fNextChangeNumber: integer;                                                 //sbs 2000-11-19
    fOnAddedUndo: TNotifyEvent;
    procedure EnsureMaxEntries;
    function GetCanUndo: boolean;
    function GetItemCount: integer;
    procedure SetMaxUndoActions(Value: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddChange(AReason: TSynChangeReason; AStart, AEnd: TPoint;
      ChangeText: string; SelMode: TSynSelectionMode);
    procedure BeginBlock;                                                       //sbs 2000-11-19
    procedure Clear;
    procedure EndBlock;                                                         //sbs 2000-11-19
    procedure Lock;
    function PeekItem: TSynEditUndoItem;
    function PopItem: TSynEditUndoItem;
    procedure PushItem(Item: TSynEditUndoItem);
    procedure Unlock;
    function GetChangeReason: TSynChangeReason;
  public
    procedure Assign(Source: TPersistent); override;
    procedure AddGroupBreak;                                                    //ek 2000-11-04
    property BlockChangeNumber: integer read fBlockChangeNumber                 //sbs 2000-11-19
      write fBlockChangeNumber;
    property CanUndo: boolean read GetCanUndo;
    property FullUndoImpossible: boolean read fFullUndoImposible;               //mh 2000-10-03
    property ItemCount: integer read GetItemCount;
    property MaxUndoActions: integer read fMaxUndoActions
      write SetMaxUndoActions;
    property OnAddedUndo: TNotifyEvent read fOnAddedUndo write fOnAddedUndo;
  end;

implementation

{$IFDEF SYN_COMPILER_3_UP}                                                      //mh 2000-10-18
resourcestring
{$ELSE}
const
{$ENDIF}
  SListIndexOutOfBounds = 'Invalid stringlist index %d';

{ TSynEditFiler }

type
  TSynEditFiler = class(TObject)
  protected
    fBuffer: PChar;
    fBufPtr: Cardinal;
    fBufSize: Cardinal;
    fFileFormat: TSynEditFileFormat;
    fFiler: TFileStream;
    procedure Flush; virtual;
    procedure SetBufferSize(NewSize: Cardinal);
  public
    constructor Create;
    destructor Destroy; override;
  public
    property FileFormat: TSynEditFileFormat read fFileFormat write fFileFormat;
  end;

constructor TSynEditFiler.Create;
const
  kByte = 1024;
begin
  inherited Create;
  fFileFormat := sffUnix;
  SetBufferSize(16 * kByte);
  fBuffer[0] := #0;
end;

destructor TSynEditFiler.Destroy;
begin
  Flush;
  fFiler.Free;
  SetBufferSize(0);
  inherited Destroy;
end;

procedure TSynEditFiler.Flush;
begin
end;

procedure TSynEditFiler.SetBufferSize(NewSize: Cardinal);
begin
  if NewSize <> fBufSize then begin
    ReallocMem(fBuffer, NewSize);
    fBufSize := NewSize;
  end;
end;

{ TSynEditFileReader }

{type
  TSynEditFileReader = class(TSynEditFiler)
  protected
    fFilePos: Cardinal;
    fFileSize: Cardinal;
    procedure FillBuffer;
  public
    constructor Create(const FileName: string);
    function EOF: boolean;
    function ReadLine: string;
  end;

constructor TSynEditFileReader.Create(const FileName: string);
begin
  inherited Create;
  fFiler := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  fFileSize := fFiler.Size;
  fFiler.Seek(0, soFromBeginning);
end;

function TSynEditFileReader.EOF: boolean;
begin
  Result := (fBuffer[fBufPtr] = #0) and (fFilePos >= fFileSize);
end;

procedure TSynEditFileReader.FillBuffer;
var
  Count: Cardinal;
begin
  if fBufPtr >= fBufSize - 1 then
    fBufPtr := 0;
  Count := fFileSize - fFilePos;
  if Count >= fBufSize - fBufPtr then
    Count := fBufSize - fBufPtr - 1;
  fFiler.ReadBuffer(fBuffer[fBufPtr], Count);
  fBuffer[fBufPtr + Count] := #0;
  fFilePos := fFilePos + Count;
  fBufPtr := 0;
end;

function TSynEditFileReader.ReadLine: string;
var
  E, P, S: PChar;
begin
  repeat
    S := PChar(@fBuffer[fBufPtr]);
    if S[0] = #0 then begin
      FillBuffer;
      S := PChar(@fBuffer[0]);
    end;
    E := PChar(@fBuffer[fBufSize]);
    P := S;
    while P + 2 < E do begin
      case P[0] of
        #10, #13:
          begin
            SetString(Result, S, P - S);
            if P[0] = #13 then
            begin
              if P[1] = #10 then
              begin
                fFileFormat := sffDos;
                Inc(P);
              end
              else
                fFileFOrmat := sffMac;
            end;
            Inc(P);
            fBufPtr := P - fBuffer;
            exit;
          end;
        #0:
          if fFilePos >= fFileSize then begin
            fBufPtr := P - fBuffer;
            SetString(Result, S, P - S);
            exit;
          end;
      end;
      Inc(P);
    end;
    // put the partial string to the start of the buffer, and refill the buffer
    Inc(P);
    if S > fBuffer then
      StrLCopy(fBuffer, S, P - S);
    fBufPtr := P - S;
    fBuffer[fBufPtr] := #0;
    // if line is longer than half the buffer then grow it first
    if 2 * Cardinal(P - S) > fBufSize then
      SetBufferSize(fBufSize + fBufSize);
  until FALSE;
end;
}
{ TSynEditFileWriter }

type
  TSynEditFileWriter = class(TSynEditFiler)
  protected
    procedure Flush; override;
  public
    constructor Create(const FileName: string);
//    procedure WriteLine(const S: string);
    procedure WriteLine(const S: string; const FileFormat: TSynEditFileFormat); //Fiala
    procedure Write(const S: String);                                           //GBN 2002-04-16
  end;

constructor TSynEditFileWriter.Create(const FileName: string);
begin
  inherited Create;
  fFiler := TFileStream.Create(FileName, fmCreate);
  fFiler.Seek(0, soFromBeginning);
end;

procedure TSynEditFileWriter.Flush;
begin
  if fBufPtr > 0 then begin
    fFiler.WriteBuffer(fBuffer[0], fBufPtr);
    fBufPtr := 0;
  end;
end;

//GBN 2002-04-16
procedure TSynEditFileWriter.Write(const S: String);
var L: Cardinal;
begin
  L := Length(S);
  repeat
    if fBufPtr + L <= fBufSize then begin
      if L > 0 then begin
        Move(S[1], fBuffer[fBufPtr], L);
        fBufPtr := fBufPtr + L;
      end;
      exit;
    end;
    Flush;
    if L > fBufSize then
      SetBufferSize(L);
  until False;
end;

procedure TSynEditFileWriter.WriteLine(const S: string;
  const FileFormat: TSynEditFileFormat);                                               //Fiala 2001-12-17
var
  L, NL: Cardinal;
begin
  L := Length(S);
  NL := 1 + Ord(fFileFormat = sffDos);
  repeat
    if fBufPtr + L + NL <= fBufSize then begin
      if L > 0 then begin
        Move(S[1], fBuffer[fBufPtr], L);
        fBufPtr := fBufPtr + L;
      end;
      if (fFileFormat <> sffUnix) then
      begin
        fBuffer[fBufPtr] := #13; // CR
        Inc(fBufPtr);
      end;
      if (fFileFormat <> sffMac) then
      begin
        fBuffer[fBufPtr] := #10; // LF
        Inc(fBufPtr);
      end;
      Exit;
    end;
    Flush;
    if L + NL > fBufSize then
      SetBufferSize(L + NL);
  until FALSE;
end;

{ TSynEditStringList }

procedure ListIndexOutOfBounds(Index: integer);
begin
  raise ESynEditStringList.CreateFmt(SListIndexOutOfBounds, [Index]);
end;

constructor TSynEditStringList.Create;
begin
  inherited Create;
  fAppendNewLineAtEOF:=true; //Retain current behavior        gbn 2002-04-25
  fWordWrap := False;
  fWordWrapWidth := 80;
  fFileFormat := sffDos;
{begin}                                                                         //mh 2000-10-19
  fIndexOfLongestLine := -1;
  TabWidth := 8;
{end}                                                                           //mh 2000-10-19
end;

destructor TSynEditStringList.Destroy;
begin
  fOnChange := nil;
  fOnChanging := nil;
  inherited Destroy;
  if fCount <> 0 then
    Finalize(fList^[0], fCount);
  fCount := 0;
  SetCapacity(0);
end;

function TSynEditStringList.Add(const S: string): integer;
begin
  BeginUpdate;
  Result := fCount;
  InsertItem(Result, S);
  if Assigned(fOnAdded) then
    fOnAdded(Result, S);
  EndUpdate;
end;

procedure TSynEditStringList.AddStrings(Strings: TStrings);
var
  i, FirstAdded: integer;
begin
{begin}                                                                         //mh 2000-10-19
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
          fString := Strings[i];
          fObject := Strings.Objects[i];
          fRange := NullRange;
          fExpandedLength := -1;
          fFlags := [sfExpandedLengthUnknown];
        end;
        Inc(fCount);
      end;
      if Assigned(fOnAdded) then
        fOnAdded(FirstAdded, '');
    finally
      EndUpdate;
    end;
  end;
{end}                                                                           //mh 2000-10-19
end;

procedure TSynEditStringList.Clear;
begin
  if fCount <> 0 then begin
    BeginUpdate;
    Finalize(fList^[0], fCount);
    fCount := 0;
    SetCapacity(0);
    if Assigned(fOnCleared) then
      fOnCleared(Self);
    EndUpdate;
  end;
  fIndexOfLongestLine := -1;                                                    //mh 2000-10-19
end;

procedure TSynEditStringList.Delete(Index: integer);
begin
  if (Index < 0) or (Index > fCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  Finalize(fList^[Index]);
  Dec(fCount);
  if Index < fCount then begin
    System.Move(fList^[Index + 1], fList^[Index],
      (fCount - Index) * SynEditStringRecSize);
  end;
  fIndexOfLongestLine := -1;                                                    //mh 2000-10-19
  if Assigned(fOnDeleted) then
    fOnDeleted(Index);
  EndUpdate;
end;

{begin}                                                                         // DJLP 2000-11-01
procedure TSynEditStringList.DeleteLines(Index, NumLines: Integer);
var
  LinesAfter: integer;
begin
  if NumLines > 0 then begin
    if (Index < 0) or (Index > fCount) then
      ListIndexOutOfBounds(Index);
    LinesAfter := fCount - (Index + NumLines - 1);
    if LinesAfter < 0 then
      NumLines := fCount - Index - 1;
    Finalize(fList^[Index], NumLines);

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
      fOnDeleted(Index);
  end;
end;
{end}                                                                           // DJLP 2000-11-01

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
{begin}                                                                         //mh 2000-10-19
  if fIndexOfLongestLine = Index1 then
    fIndexOfLongestLine := Index2
  else if fIndexOfLongestLine = Index2 then
    fIndexOfLongestLine := Index1;
{end}                                                                           //mh 2000-10-19
  EndUpdate;
end;

{begin}                                                                         //mh 2000-10-19
function TSynEditStringList.ExpandedString(Index: integer): string;
var
  HasTabs: boolean;
begin
  with fList^[Index] do
    if fString = '' then begin
      Result := '';
      Exclude(fFlags, sfExpandedLengthUnknown);
      Exclude(fFlags, sfHasTabs);
      Include(fFlags, sfHasNoTabs);
      fExpandedLength := 0;
    end else begin
      Result := fConvertTabsProc(fString, fTabWidth, HasTabs);
      fExpandedLength := Length(Result);
      Exclude(fFlags, sfExpandedLengthUnknown);
      Exclude(fFlags, sfHasTabs);
      Exclude(fFlags, sfHasNoTabs);
      if HasTabs then
        Include(fFlags, sfHasTabs)
      else
        Include(fFlags, sfHasNoTabs);
    end;
end;
{end}                                                                           //mh 2000-10-19

function TSynEditStringList.Get(Index: integer): string;
begin
  if (Index >= 0) and (Index < fCount) then
    Result := fList^[Index].fString
  else
    Result := '';
end;

function TSynEditStringList.GetCapacity: integer;
begin
  Result := fCapacity;
end;

function TSynEditStringList.GetCount: integer;
begin
  Result := fCount;
end;

{begin}                                                                         //mh 2000-10-19
function TSynEditStringList.GetExpandedString(Index: integer): string;
begin
  if (Index >= 0) and (Index < fCount) then begin
    if sfHasNoTabs in fList^[Index].fFlags then
      Result := fList^[Index].fString
    else
      Result := ExpandedString(Index);
  end else
    Result := '';
end;

function TSynEditStringList.GetLengthOfLongestLine: integer;                    //mh 2000-10-19
var
  i, MaxLen: integer;
  PRec: PSynEditStringRec;
  s: string;
begin
  if fIndexOfLongestLine < 0 then begin
    MaxLen := 0;
    if fCount > 0 then begin
      PRec := @fList^[0];
      for i := 0 to fCount - 1 do begin
        if sfExpandedLengthUnknown in PRec^.fFlags then
          s := ExpandedString(i);
        if PRec^.fExpandedLength > MaxLen then begin
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
{end}                                                                           //mh 2000-10-19

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

procedure TSynEditStringList.Insert(Index: integer; const S: string);
begin
  if (Index < 0) or (Index > fCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  InsertItem(Index, S);
  if Assigned(fOnInserted) then
    fOnInserted(Index, S);
  EndUpdate;
end;

procedure TSynEditStringList.InsertItem(Index: integer; const S: string);
var                                                                             //Fiala 2001-12-17
  s1: string;                                                                   //Fiala 2001-12-17
  tmpIndex: Integer;
begin
  BeginUpdate;
  s1 := s;                                                                      //Fiala
  tmpIndex := Index;                                                            //Fiala
  repeat                                                                        //Fiala
    if fCount = fCapacity then
      Grow;
    if tmpIndex < fCount then begin
      System.Move(fList^[tmpIndex], fList^[tmpIndex + 1],
        (fCount - tmpIndex) * SynEditStringRecSize);
    end;
    fIndexOfLongestLine := -1;                                                  //mh 2000-10-19
    with fList^[tmpIndex] do begin
      Pointer(fString) := nil;
      fString := WrapString(s1);                                                //Fiala
      fObject := nil;
      fRange := NullRange;
      fWraped := (Index <> tmpIndex);                                           //Fiala
{begin}                                                                         //mh 2000-10-19
      fExpandedLength := -1;
      fFlags := [sfExpandedLengthUnknown];
{end}                                                                           //mh 2000-10-19
    end;
    Inc(tmpIndex);
    Inc(fCount);
  until Length(s1) = 0;                                                         //Fiala
  EndUpdate;
(*//Old Code, left for reference
  BeginUpdate;
  if fCount = fCapacity then
    Grow;
  if Index < fCount then begin
    System.Move(fList^[Index], fList^[Index + 1],
      (fCount - Index) * SynEditStringRecSize);
  end;
  fIndexOfLongestLine := -1;                                                    //mh 2000-10-19
  with fList^[Index] do begin
    Pointer(fString) := nil;
    fString := S;
    fObject := nil;
    fRange := NullRange;
{begin}                                                                         //mh 2000-10-19
    fExpandedLength := -1;
    fFlags := [sfExpandedLengthUnknown];
{end}                                                                           //mh 2000-10-19
  end;
  Inc(fCount);
  EndUpdate;
*)
end;

{begin}                                                                         // DJLP 2000-11-01
procedure TSynEditStringList.InsertLines(Index, NumLines: integer);
begin
  if NumLines > 0 then begin
    BeginUpdate;
    try
      SetCapacity(fCount + NumLines);
      if Index < fCount then begin
        System.Move(fList^[Index], fList^[Index + NumLines],
          (fCount - Index) * SynEditStringRecSize);
      end;
      FillChar(fList^[Index], NumLines * SynEditStringRecSize, 0);
      Inc(fCount, NumLines);
      if Assigned(fOnAdded) then
        fOnAdded(Index, '');
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSynEditStringList.InsertStrings(Index: integer;
  NewStrings: TStrings);
var
  i, Cnt: integer;
begin
  Cnt := NewStrings.Count;
  if Cnt > 0 then begin
    BeginUpdate;
    try
    InsertLines(Index, Cnt);
    for i := 0 to Cnt - 1 do
      Strings[Index + i] := NewStrings[i];
    finally
      EndUpdate;
    end;
  end;
end;
{end}                                                                           // DJLP 2000-11-01

procedure TSynEditStringList.LoadFromFile(const FileName: string);
var
//  Reader: TSynEditFileReader;                                                 //Fiala
  Stream: TStream;
{begin}                                                                         //Fiala
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;

(*//Old Code, for reference
  Reader := TSynEditFileReader.Create(FileName);
  try
    BeginUpdate;
    try
      Clear;
      while not Reader.EOF do
        Add(Reader.ReadLine);
      fFileFormat := Reader.FileFormat;
    finally
      EndUpdate;
    end;
  finally
    Reader.Free;
  end;
*)
end;

{begin}                                                                         //Fiala 2001-12-17
procedure TSynEditStringList.LoadFromStream(Stream: TStream);
var
  Size: Integer;
  S, S1: string;
  P, Start: PChar;
  fCR, fLF: Boolean;
  iPos: Integer;
begin
  fCR := False;
  fLF := False;
  try
    BeginUpdate;
    Size := Stream.Size;
    Stream.Position := 0;
    SetString(S, nil, Size);
    Stream.Read(Pointer(S)^, Size);

    Clear;
    P := Pointer(S);
    if P <> nil then
    begin
      iPos := 0;
      while (iPos < Size) do // (P^ <> #0) do
      begin
        Start := P;
        while not (P^ in [#0, #10, #13]) do
        begin
          Inc(P);
          Inc(iPos);
        end;
        SetString(S1, Start, P - Start);
        Add(S1);
        if (P^ = #13) then
        begin
          fCR := True;
          Inc(P);
          Inc(iPos);
        end;
        if (P^ = #10) then
        begin
          fLF := True;
          Inc(P);
          Inc(iPos);
        end;
        if (P^ = #0) then
        begin
          Inc(P);
          Inc(iPos);
        end;
      end;
    end;
  finally
    EndUpdate;
  end;
  if fCR and not fLF then
    fFileFormat := sffMac
  else if fLF and not fCR then
    fFileFormat := sffUnix
  else
    fFileFormat := sffDos;
end;

procedure TSynEditStringList.SaveToStream(Stream: TStream);
var
  S, S1: string;
  I, L, Size: Integer;
  P: PChar;
  LineEndLength: Integer;
begin
  Size := 0;
  if FileFormat in [sffMac, sffUnix] then
    LineEndLength := 1
  else
    LineEndLength := 2;
  for I := 0 to Count - 1 do Inc(Size, Length(Strings[I]) + LineEndLength);
  SetString(S, nil, Size);
  P := Pointer(S);
  for I := 0 to Count - 1 do begin
    S1 := Strings[I];
    L := Length(S1);
    if L <> 0 then
    begin
      System.Move(Pointer(S1)^, P^, L);
      Inc(P, L);
    end;
    //GBN 2002-04-16
    //Do not add new line to last line
    if (I < Count-1) or (AppendNewLineAtEOF) then begin
      if FileFormat = sffMac then begin
        P^ := #13;
        Inc(P);
      end else
      if FileFormat = sffUnix then begin
        P^ := #10;
        Inc(P);
      end else begin
        P^ := #13;
        Inc(P);
        P^ := #10;
        Inc(P);
      end;
    end;
  end;
  Stream.WriteBuffer(Pointer(S)^, Length(S));
end;
{end}                                                                           //Fiala 2001-12-17

procedure TSynEditStringList.Put(Index: integer; const S: string);
begin
  if (Index = 0) and (fCount = 0) or (fCount = Index) then                      //Fiala 2001-12-17
    Add(S)
  else begin
    if (Index < 0) or (Index >= fCount) then
      ListIndexOutOfBounds(Index);
    BeginUpdate;
{begin}                                                                         //mh 2000-10-19
    fIndexOfLongestLine := -1;
    with fList^[Index] do begin
      Include(fFlags, sfExpandedLengthUnknown);
      Exclude(fFlags, sfHasTabs);
      Exclude(fFlags, sfHasNoTabs);
      fString := S;
    end;
{end}                                                                           //mh 2000-10-19
    if Assigned(fOnPutted) then
      fOnPutted(Index, S);
    EndUpdate;
  end;
end;

procedure TSynEditStringList.PutObject(Index: integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= fCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  fList^[Index].fObject := AObject;
  EndUpdate;
end;

procedure TSynEditStringList.PutRange(Index: integer; ARange: TSynEditRange);
begin
  if (Index < 0) or (Index >= fCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  fList^[Index].fRange := ARange;
  EndUpdate;
end;

{begin}                                                                         //Fiala 2001-12-17
procedure TSynEditStringList.SaveToFile(const FileName: string);
var
  Writer: TSynEditFileWriter;
  i, j: integer;
  s: string;
begin
  if fWordWrap then DoWordUnWrap;
  Writer := TSynEditFileWriter.Create(FileName);
  try
    Writer.FileFormat := fFileFormat;
    i := 0;
    while i < fCount do begin
      s := Get(i);
      j := i + 1;
      while fWordWrap and (j < fCount) and fList^[j].fWraped do begin
        s := s + Get(j);
        inc(j);
      end;
      i := j;
      //GBN 2002-04-16
      if (i<fCount) or (AppendNewLineAtEOF) then Writer.WriteLine(s, fFileFormat)
      else Writer.Write(S);
    end;
  finally
    Writer.Free;
  end;
  if fWordWrap then DoWordWrap;
end;
{end}                                                                           //Fiala 2001-12-17
(*//Old code, for reference
procedure TSynEditStringList.SaveToFile(const FileName: string);
var
  Writer: TSynEditFileWriter;
  i: integer;
begin
  Writer := TSynEditFileWriter.Create(FileName);
  try
    Writer.FileFormat := fFileFormat;
    for i := 0 to fCount - 1 do
      Writer.WriteLine(Get(i));
  finally
    Writer.Free;
  end;
end;
*)
procedure TSynEditStringList.SetCapacity(NewCapacity: integer);
begin
  ReallocMem(fList, NewCapacity * SynEditStringRecSize);
  fCapacity := NewCapacity;
end;

{begin}                                                                         //mh 2000-10-19
procedure TSynEditStringList.SetTabWidth(Value: integer);
var
  i: integer;
begin
  if Value <> fTabWidth then begin
    fTabWidth := Value;
    fConvertTabsProc := GetBestConvertTabsProcEx(fTabWidth);
    fIndexOfLongestLine := -1;
{begin}                                                                         //mh 2000-11-08
    for i := 0 to fCount - 1 do
      with fList^[i] do begin
        fExpandedLength := -1;
        Exclude(fFlags, sfHasNoTabs);
        Include(fFlags, sfExpandedLengthUnknown);
      end;
{end}                                                                           //mh 2000-11-08
  end;
end;
{end}                                                                           //mh 2000-10-19

procedure TSynEditStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then begin
    if Assigned(fOnChanging) then
      fOnChanging(Self);
  end else begin
    if Assigned(fOnChange) then
      fOnChange(Self);
  end;
end;
{end}                                                                           //mh 2000-10-10

{begin}                                                                         //Fiala 2001-12-17
{ InputString is cutted from left side, cuted part is returned as Result}
function TSynEditStringList.WrapString(var InputString: String): String;
const
  WrapableChars = [' ',#9,';','>',','];
var
  i : Integer;
begin
  if not fWordWrap then begin
    Result := InputString;
    InputString := '';
    Exit;
  end;
  if Length(InputString) > fWordWrapWidth then begin
    i := fWordWrapWidth;
    { trying find WrapableChars to the left }
    while i > 0 do begin
      if InputString[i] in WrapableChars
        then Break;
      Dec(i);
    end;
    { on the left is one long word. Try to find on the right side }
    if i = 0 then begin
      i := fWordWrapWidth;
      while i < Length(InputString) do begin
        if InputString[i] in WrapableChars
          then Break;
        Inc(i);
      end;
    end;
  end
  else i:= Length(InputString);
  Result := Copy(InputString, 1, i);
  InputString := Copy(InputString, i + 1, Length(InputString));
end;

procedure TSynEditStringList.SetWordWrap(const Value: boolean);
begin
  if fWordWrap <> Value then begin
    FWordWrap := Value;
    if Value
      then DoWordWrap
      else DoWordUnWrap;
  end;
end;

{ Return wraping of all lines }
procedure TSynEditStringList.DoWordUnWrap;
begin
  DoUnWrapLines(0, fCount - 1);
end;

{ Wrap all lines }
procedure TSynEditStringList.DoWordWrap;
begin
  DoWrapLines(0, fCount - 1);
end;

{ wrap lines in visible windows - for write changing}
procedure TSynEditStringList.DoWrapLines(const FromLine: Integer; const ToLine: Integer);
var
  i: Integer;
begin
  BeginUpdate;
  try
    for i := ToLine downto FromLine do
      WrapLine(i);
  finally
    EndUpdate;
  end;
end;

procedure TSynEditStringList.DoUnWrapLines(const FromLine, ToLine: Integer);
var
  i : Integer;
  s : string;
begin
  BeginUpdate;
  try
    s := '';
    for i := ToLine downto FromLine do begin
      if fList^[i].fWraped then begin
        s := Strings[i] + s;
        Delete(i);
      end
      else begin
        with fList^[i] do begin
          fString := fString + s;
          fExpandedLength := -1;
          fFlags := [sfExpandedLengthUnknown];
        end;
        s := '';
      end;
    end;
  finally
    EndUpdate;
  end;
end;

function TSynEditStringList.ReWrapLine(const LineNumber: Integer): Integer;
var
  i, LineBegin, LineEnd : Integer;
  s : string;
begin
  Result := LineNumber;
  if not fWordWrap then Exit;
  BeginUpdate;
  { first we must find begin of wraped line }
  LineBegin := LineNumber;
  while (LineBegin >= 0) and fList^[LineBegin].fWraped do Dec(LineBegin);
  Result := LineBegin;
  { now we find end of wraped line }
  LineEnd := LineNumber;
  while (LineEnd < fCount - 1) and fList^[LineEnd + 1].fWraped do Inc(LineEnd);
  { and we make back one long line }
  s := '';
  for i := LineEnd downto LineBegin do begin
    if fList^[i].fWraped then begin
      s := Strings[i] + s;
      Delete(i);
    end
    else fList^[i].fString := fList^[i].fString + s;
  end;
  { finally wrap this line again }
  WrapLine(LineBegin);
  EndUpdate;
end;

procedure TSynEditStringList.WrapLine(const LineNumber: Integer);
var
  s, s1: string;
  tmpIndex: Integer;
begin
  s := fList^[LineNumber].fString;
  with fList^[LineNumber] do begin
    fString := WrapString(s);
    fExpandedLength := -1;
    fFlags := [sfExpandedLengthUnknown];
  end;
  if Length(s) > 0 then begin
    s1 := s;
    tmpIndex := LineNumber ;
    while Length(s1) > 0 do begin
      Inc(tmpIndex);
      if fCount = fCapacity then
        Grow;
      if tmpIndex < fCount then begin
        System.Move(fList^[tmpIndex], fList^[tmpIndex + 1],
        (fCount - tmpIndex) * SynEditStringRecSize);
      end;
      with fList^[tmpIndex] do begin
        Pointer(fString) := nil;
        fString := WrapString(s1);
        fObject := nil;
        fRange := NullRange;
        fWraped := True;
        fExpandedLength := -1;
        fFlags := [sfExpandedLengthUnknown];
      end;
      Inc(fCount);
    end;
  end;
  fIndexOfLongestLine := -1;
end;

function TSynEditStringList.IsLineWraped(const LineNumber: Integer): Boolean;
begin
  Result := fList^[LineNumber].fWraped;
end;
{end}                                                                           //Fiala 2001-12-17

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
  fNextChangeNumber := 1;                                                       //sbs 2000-11-19
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
    fItems.Clear;
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
  end
  else
    inherited Assign(Source);
end;

procedure TSynEditUndoList.AddChange(AReason: TSynChangeReason; AStart,
  AEnd: TPoint; ChangeText: string; SelMode: TSynSelectionMode);
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
{begin}                                                                         //sbs 2000-11-19
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
{end}                                                                           //sbs 2000-11-19
      end;
      PushItem(NewItem);
    except
      NewItem.Free;
      raise;
    end;
  end;
end;

{begin}                                                                         //sbs 2000-11-19
procedure TSynEditUndoList.BeginBlock;
begin
  Inc(fBlockCount);
  fBlockChangeNumber := fNextChangeNumber;
end;
{end}                                                                           //sbs 2000-11-19

procedure TSynEditUndoList.Clear;
var
  i: integer;
begin
  for i := 0 to fItems.Count - 1 do
    TSynEditUndoItem(fItems[i]).Free;
  fItems.Clear;
  fFullUndoImposible := FALSE;                                                  //mh 2000-10-03
end;

{begin}                                                                         //sbs 2000-11-19
procedure TSynEditUndoList.EndBlock;
begin
  if fBlockCount > 0 then begin
    Dec(fBlockCount);                                                     
    if fBlockCount = 0 then begin                                         
      fBlockChangeNumber := 0;
      Inc(fNextChangeNumber);
      if fNextChangeNumber = 0 then
        Inc(fNextChangeNumber);
    end;
  end;
end;
{end}                                                                           //sbs 2000-11-19

procedure TSynEditUndoList.EnsureMaxEntries;
var
  Item: TSynEditUndoItem;
begin
  if fItems.Count > fMaxUndoActions then begin                                  //mh 2000-10-03
    fFullUndoImposible := TRUE;                                                 //mh 2000-10-03
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
    if (Item.ChangeReason<>crGroupBreak) and Assigned(OnAddedUndo) then         //ek 2000-11-04
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

function TSynEditUndoList.GetChangeReason: TSynChangeReason;
begin
  if fItems.Count = 0 then
    result := crNothing
  else
    result := TSynEditUndoItem(fItems[fItems.Count - 1]).fChangeReason;
end;


procedure TSynEditUndoList.AddGroupBreak;
begin
  if (PeekItem<>nil) and (PeekItem.ChangeReason<>crGroupBreak) then begin
    AddChange(crGroupBreak, Point(0,0), Point(0,0), '', smNormal);
  end;
end;

end.

