/// Synopse extended TMemo visual component
// - licensed under a MPL/GPL/LGPL tri-license; version 2.26
unit SynMemoEx;

{
    This file is part of Synopse extended TMemo

    Synopse Synopse SynLZ Compression. Copyright (C) 2012 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse SynLZ Compression.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2012
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****


  Based on TMemoEx, by R&A Library 2.03  (c) R&A, 1996-2000.

  R&A Library seems now unsupported. Web site is dead, as is their email address.
  We decided to put this unit, mostly rewritten, under a MPL/GPL/LGPL tri-license.

  * full code rewrite for speed, stability and new features
  * regroup in one unit w/out RA Consts
  * no expandTabs: too slow
  * FAttrs are calculated on the fly: spare RAM on syntax highlight (only text is stored)
  * don't use FLines.text on often-call methods (insertText, deleteSelected...)
  * fClipProtect: clipBoard Copy/Cut limited to 2K for others programs (copyrights)
  * find (text) command
  * a lot of bug fixes

}


interface

{.$define CLIPBOARDPROTECT} // if defined, ClipProtect truncs clipboard to 2KB
// use only with ONE TMemoEx at once

{$IFNDEF MEMOEX_NOEDITOR}
{$DEFINE MEMOEX_EDITOR}           {if not MEMOEX_EDITOR then mode = Viewer}
{$ENDIF}
{$DEFINE MEMOEX_DEFLAYOUT}        {set default keyboard layout}
{$IFNDEF MEMOEX_NOUNDO}
{$DEFINE MEMOEX_UNDO}             {enable undo}
{$ENDIF}
{$IFNDEF MEMOEX_NOCOMPLETION}
{$DEFINE MEMOEX_COMPLETION}       {enable code completion}
{$ENDIF}

{$IFNDEF MEMOEX_EDITOR}
{$UNDEF MEMOEX_DEFLAYOUT}
{$UNDEF MEMOEX_UNDO}
{$UNDEF MEMOEX_COMPLETION}
{$ENDIF MEMOEX_EDITOR}

{$define RA_D4H} // Delphi 4 and UP

{ $D-,L-} // avoid jumping in the source for any EComplete exceptions e.g.

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ExtCtrls, StdCtrls, ClipBrd, Menus;



const

  RAEditorCompletionChars = #8+
    '_0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm'+
    '…÷” ≈Õ√ÿŸ«’⁄‘€¬¿œ–ŒÀƒ∆›ﬂ◊—Ã»“‹¡ﬁ®ÈˆÛÍÂÌ„¯˘Áı˙Ù˚‚‡ÔÓÎ‰Ê˝ˇ˜ÒÏËÚ¸·˛∏';
  Separators : set of char = [#00,' ','-',#13, #10,'.',',','/','\',
    ':','+','%','*','(',')',';','=','{','}','[',']', '{', '}', '|', '!', '@'];

  GutterRightMargin = 2;

  WM_EDITCOMMAND = WM_USER + $101;

  RA_EX_STYLE_DEFAULT = 0;

  RA_CASE_CONVERT_UPPER   = 0;
  RA_CASE_CONVERT_LOWER   = 1;
  RA_CASE_CONVERT_INVERT  = 2;

type
  TRAControlScrollBar95 = class
  private
    FKind: TScrollBarKind;
    FPosition: Integer;
    FMin: Integer;
    FMax: Integer;
    FSmallChange: TScrollBarInc;
    FLargeChange: TScrollBarInc;
    FPage : integer;
    FHandle : hWnd;
    FOnScroll: TScrollEvent;
   // FVisible : boolean;
    procedure SetParam(index, Value: Integer);
   // procedure SetVisible(Value : boolean);
   // procedure SetLargeChange(Value: TScrollBarInc);
  protected
    procedure Scroll(ScrollCode: TScrollCode; var ScrollPos: Integer); dynamic;
  public
    constructor Create;
    procedure SetParams(AMin, AMax, APosition, APage : integer);
    procedure DoScroll(var Message: TWMScroll);

    property Kind: TScrollBarKind read FKind write FKind default sbHorizontal;
    property SmallChange: TScrollBarInc read FSmallChange write FSmallChange default 1;
    property LargeChange: TScrollBarInc read FLargeChange write FLargeChange default 1;
    property Min  : Integer index 0 read FMin write SetParam default 0;
    property Max  : Integer index 1 read FMax write SetParam default 100;
    property Position : Integer index 2 read FPosition write SetParam default 0;
    property Page : integer index 3 read FPage write SetParam;
    property Handle : hWnd read FHandle write FHandle;
    property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
   // property Visible : boolean read FVisible write SetVisible;
  end;

  TCellRect = record
    Width: integer;
    Height: integer;
  end;

  PLineAttr = ^TLineAttr;
  TLineAttr = packed record
    FC, BC: TColor;
    case integer of
    0: (Style: TFontStyles;
        ex_style: byte;
        underlined: boolean);
    1: (LastInteger: integer);
  end;

  TCustomMemoEx = class;

  TWordUnderCursor = record // for TOnWordClick
    Text: string;
    Style: integer;
    CaretX, CaretY,
    ParaIndex, ParaOffset: integer;
    TextStart: integer;
    Shift: TShiftState;
  end;

  TLineAttrs = array of TLineAttr;
  TSelAttrs  = array of boolean;
  TOnGetLineAttr = procedure (Sender: TObject; const Line: string; index: integer;
    const SelAttrs: TSelAttrs; var Attrs: TLineAttrs) of object;
  TOnChangeStatus = TNotifyEvent;
  TOnChangeClipboardState = procedure (Sender: TObject; const CanPaste: boolean) of object;
  TOnWordClick = procedure (Sender: TObject; const Clicked: TWordUnderCursor) of object;
  TOnMouseOver = procedure (Sender: TObject; WordStyle: word; var _Cursor: TCursor) of object;
  TOnBreakLine = procedure (Sender: TObject; const Original: string; var _New: string) of object;
  TOnConcatLine = procedure (Sender: TObject; const Original: string; var _New: string) of object;
  TOnTextInsert = procedure (Sender: TObject; var Text: string) of object;
  TOnCaseConversion = function (Sender: TObject; Conversion: byte; const Text: string): string of object;
  TOnInsertBlock = function (Sender: TObject; var Text: string): boolean of object;
  TOnSaveBlock = procedure (Sender: TObject; const Text: string) of object;
  TOnInsertMacro = function (Sender: TObject; MacroID: integer): string of object;
  TOnBlockOperation = function (Sender: TObject; MacroID: integer; const Text: string): string of object;
  TOnSetCaretPos = procedure (Sender: TObject; CaretX, CaretY: integer) of object;
  TOnClipboardPaste = function (Sender: TObject): boolean of object;

  {$IFDEF MEMOEX_COMPLETION}
  TOnPreprocessCompletion = function (Sender: TObject; const ID, Text: string): string of object;
  {$ENDIF}

  PAutoChangeWord = ^TAutoChangeWord;
  TAutoChangeWord = record
    OldWord, NewWord: string;
  end;

  PParagraph = ^TParagraph;
  TParagraph = record
    FPreCount,
    FCount: integer; // length(FString) = FStrings[0..FCount-1]
    FStrings: array of string;
    FObject: TObject;
  end;

  TEditorStrings = class(TStrings)
  private
    FMemoEx: TCustomMemoEx;
    FList: array of TParagraph;
    FParaLinesCount, FCount: integer;
    FOnChanging: TNotifyEvent;

    FOnAfterLoad: TNotifyEvent;
    FOnBeforeSave: TNotifyEvent;

    procedure Recount(Index: integer);
    function _GetString(ParaIndex: integer): string;
    procedure _PutString(ParaIndex: integer; const S: string);
    procedure ReformatParagraph(ParaIndex: integer);
    procedure Reformat;
    procedure CheckLength(const st: string);

    procedure Grow;
    procedure InsertItem(Index: integer; const S: string);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: integer): string; override; 
    function GetParaString(Index: integer): string;
    function GetParagraph(Index: integer): PParagraph;
    procedure Put(Index: integer; const S: string); override;
    procedure PutParaString(Index: integer; const S: string);
    procedure PutObject(Index: Integer; AObject: TObject); override;
    function GetObject(Index: Integer): TObject; override;
    procedure SetUpdateState(Updating: Boolean); override;
    function GetTextStr: string; override;
    procedure SetTextStr(const Value: string); override;
    procedure SetInternal(Index: integer; const Value: string);
    procedure SetInternalParaStr(Index: integer; const Value: string);
    function GetCount: Integer; override; // make compiler happy

    function AddParaStr(ParaIndex: integer; const S: string): integer;

    procedure ReLine; // complete line with spaces until caret X pos

    property Internal[Index: integer]: string write SetInternal;
    property InternalParaStrings[Index: integer]: string write SetInternalParaStr;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    function Add(const S: string): integer; override;
    procedure Delete(Index: integer); override;
    procedure Insert(Index: integer; const S: string); override;

    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;

    procedure SetLockText(const Text: string);
    function GetTextLength: integer; // fast get length(Text) value
    function HasText: boolean; // true if Text<>''
    procedure Index2ParaIndex(Index: integer; out Para, ParaIndex: integer);
    function GetParagraphByIndex(Index: integer; out ParaIndex, IndexOffs: integer): string;
    procedure Caret2Paragraph(X, Y: integer; out ParaIndex, IndexOffs: integer);
    procedure Paragraph2Caret(ParaIndex, IndexOffs: integer; out X, Y: integer);
    function GetParaOffs(ParaIndex: integer): integer; // in global Text[] string

    property ParaLineCount: integer read FParaLinesCount;
    property ParaStrings[Index: integer {=ParaY}]: string read GetParaString write PutParaString;
    property Paragraphs[Index: integer]: PParagraph read GetParagraph; // = FList[index]
//    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

  TModifiedAction = (maInsert, maDelete);

  TBookMark = record
    X, Y: integer;
    Valid: boolean;
  end;
  TBookMarkNum = 0..9;
  TBookMarks = array[TBookMarkNum] of TBookMark;

  TEditorClient = class
  private
    FMemoEx: TCustomMemoEx;
    Top: integer;
    function Left: integer;
    function Height: integer;
    function Width: integer;
    function ClientWidth: integer;
    function ClientHeight: integer;
    function ClientRect: TRect;
    function BoundsRect: TRect;
    function GetCanvas: TCanvas;
    property Canvas: TCanvas read GetCanvas;
  end;

  TGutter = class
  private
    FMemoEx: TCustomMemoEx;
  public
    procedure Paint;
    procedure Invalidate;
  end;

  TOnPaintGutter = procedure(Sender: TObject; Canvas: TCanvas; const Rect: TRect) of object;

  TEditCommand = word;

  TMacro = string; { uses as buffer }

  TEditKey = class
  public
    Key1, Key2: Word;
    Shift1, Shift2: TShiftState;
    Command: TEditCommand;
    constructor Create(const ACommand: TEditCommand; const AKey1: word;
      const AShift1: TShiftState);
    constructor Create2(const ACommand: TEditCommand; const AKey1: word;
      const AShift1: TShiftState; const AKey2: word;
      const AShift2: TShiftState);
  end;

  TKeyboard = class
  private
    List: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const ACommand: TEditCommand; const AKey1: word;
      const AShift1: TShiftState);
    procedure Add2(const ACommand: TEditCommand; const AKey1: word;
      const AShift1: TShiftState; const AKey2: word;
      const AShift2: TShiftState);
    procedure Clear;
    function Command(const AKey: word; const AShift: TShiftState): TEditCommand;
    function Command2(const AKey1: word; const AShift1: TShiftState;
      const AKey2: word; const AShift2: TShiftState): TEditCommand;
    {$IFDEF MEMOEX_DEFLAYOUT}
    procedure SetDefLayout;
    {$ENDIF MEMOEX_DEFLAYOUT}
  end;

  EMemoExError = class(Exception);

  {$IFDEF MEMOEX_UNDO}
  TUndoBuffer = class;

  TUndo = class
  private
    FMemoEx: TCustomMemoEx;
    function UndoBuffer: TUndoBuffer;
  public
    constructor Create(const AMemoEx: TCustomMemoEx);
    procedure Undo; dynamic; abstract;
    procedure Redo; dynamic; abstract;
  end;

  TUndoBuffer = class(TList)
  private
    FMemoEx: TCustomMemoEx;
    FPtr: integer;
    FCancelUndo, InUndo: boolean;
    function IsNewGroup(const AUndo: TUndo): boolean;
  public
    constructor Create;
    procedure Add(AUndo: TUndo);
    function LastUndo: TUndo;
    procedure Undo;
    procedure Redo;
    procedure Clear; override;
    procedure Delete;
  end;
  {$ENDIF MEMOEX_UNDO}

  {$IFDEF MEMOEX_COMPLETION}
  TCompletion = class;
  TOnCompletion = procedure(Sender: TObject; var Cancel: boolean) of object;
  {$ENDIF MEMOEX_COMPLETION}

  TTabStop = (tsTabStop, tsAutoIndent);

  {*** TCustomMemoEx }

  TCustomMemoEx = class(TCustomControl)
  private
    { internal objects }
    FLines: TEditorStrings;
    scbHorz: TRAControlScrollBar95;
    scbVert: TRAControlScrollBar95;
    EditorClient: TEditorClient;
    FGutter: TGutter;
    FKeyboard: TKeyboard;
    FBookMarks: TBookMarks;
    FUpdateLock: integer;
    {$IFDEF MemoEx_UNDO}
    FUndoBuffer: TUndoBuffer;
    FGroupUndo: boolean;
    {$ENDIF MEMOEX_UNDO}
    {$IFDEF MEMOEX_COMPLETION}
    FCompletion: TCompletion; 
    {$ENDIF MEMOEX_COMPLETION}

    { internal - Columns and rows attributes }
    FCols, FRows: integer;
    FLeftCol, FTopRow: integer;
    // FLeftColMax, FTopRowMax : integer;
    FLastVisibleCol, FLastVisibleRow: integer;
    FCaretX, FCaretY: integer;
    FVisibleColCount: integer;
    FVisibleRowCount: integer;

    { internal - other flags and attributes }
    FAllRepaint: boolean;
    FCellRect: TCellRect;
    {$IFDEF MEMOEX_EDITOR}
    IgnoreKeyPress: boolean;
    {$ENDIF MEMOEX_EDITOR}
    WaitSecondKey: Boolean;
    Key1: Word;
    Shift1: TShiftState;

    { internal - selection attributes }
    FSelected: boolean;
    FSelBlock: boolean; 
    FSelBegX, FSelBegY, FSelEndX, FSelEndY: integer;
    FUpdateSelBegX, FUpdateSelEndX, FUpdateSelBegY, FUpdateSelEndY: integer;
    FSelStartX, FSelStartY: integer;
    FclSelectBC, FclSelectFC: TColor;

    { mouse support }
    timerScroll: TTimer;
    MouseMoveY, MouseMoveXX, MouseMoveYY: integer;

    { internal }
    FTabPos: array of boolean;
    FTabStops: string;

    { internal - primary for TIReader support }
    FEditBuffer: string;
    FPEditBuffer: PChar;
    FEditBufferSize: integer;

    FCompound: integer;
    { FMacro - buffer of TEditCommand, each command represents by two chars }
    FMacro: TMacro;
    FDefMacro: TMacro;

    { visual attributes - properties }
    FBorderStyle: TBorderStyle;
    FGutterColor: TColor;
    FGutterWidth: integer;
    FRightMarginVisible: boolean;
    FRightMargin: integer;
    FRightMarginColor: TColor;
    FScrollBars: TScrollStyle;
    FDoubleClickLine: boolean;
    FSmartTab: Boolean;
    FBackSpaceUnindents: Boolean;
    FAutoIndent: Boolean;
    FKeepTrailingBlanks: Boolean;
    FCursorBeyondEOF: Boolean;
    FCursorBeyondEOL: Boolean;
    { FInclusive - Inclusive mode }
    FInclusive: Boolean;

    { non-visual attributes - properties }
    FInsertMode: boolean;
    FReadOnly: boolean;
    FModified: boolean;
    FRecording: boolean;

    { Events }
    FOnGetLineAttr: TOnGetLineAttr;
    FOnChange: TNotifyEvent;
    FOnSelectionChange: TNotifyEvent;
    FOnChangeStatus: TOnChangeStatus;
    FOnChangeClipboardState: TOnChangeClipboardState;
    FOnScroll: TNotifyEvent;
    FOnResize: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnPaintGutter: TOnPaintGutter;
    FOnWordClick: TOnWordClick;
    FOnMouseOver: TOnMouseOver;
    FOnBreakLine: TOnBreakLine;
    FOnConcatLine: TOnConcatLine;
    FOnTextInsert: TOnTextInsert;
    FOnCaseConversion: TOnCaseConversion;
    FOnInsertBlock: TOnInsertBlock;
    FOnSaveBlock: TOnSaveBlock;
    FOnInsertMacro: TOnInsertMacro;
    FOnBlockOperation: TOnBlockOperation;
    FOnSetCaretPos: TOnSetCaretPos;

    {$IFDEF MEMOEX_COMPLETION}
    FOnCompletionIdentifer: TOnCompletion;
    FOnCompletionTemplate: TOnCompletion;
    FOnCompletionDrawItem: TDrawItemEvent;
    FOnCompletionMeasureItem: TMeasureItemEvent;
    FOnPreprocessCompletion: TOnPreprocessCompletion;
    {$ENDIF MEMOEX_COMPLETION}

    FDrawBitmap: TBitmap;
    FFont: TFont;
    FWantTabs: boolean;
    FWordWrap: boolean;
    FStripInvisible: boolean;
    FParaX, FParaY: integer;
    NextClipViewer: THandle;
    scbVertWidth, scbHorzHeight: integer;
    Max_X: integer;

    mouse_down,
    mouse_dragged,
    double_clicked: boolean;
    FWordUnderCursor: TWordUnderCursor;
    FClipPasteRtfBackSlashConvert: boolean;
{$ifdef CLIPBOARDPROTECT} // ClipProtect will trunc clipboard to 2KB
    FClip: string; // AB
    FClipProtect: boolean;
{$endif}
    FOnClipboardPaste: TOnClipboardPaste; // AB
    procedure SetMax_X(const Value: integer);
    procedure UpdateEditorSize(const FullUpdate: boolean = true; const RepaintGutter: boolean = true);
    procedure RedrawFrom(YFrom: integer);
    function RepaintParagraph(LineIndex: integer): integer;

    {$IFDEF MEMOEX_COMPLETION}
    procedure DoCompletionIdentifer(var Cancel: boolean);
    procedure DoCompletionTemplate(var Cancel: boolean);
    function DoPreprocessCompletion(const ID, OldText: string): string;
    {$ENDIF MEMOEX_COMPLETION}

    procedure ScrollTimer(Sender: TObject);

    procedure ReLine;
    function GetDefTabStop(const X: integer; const Next: Boolean): integer;
    function GetTabStop(const X, Y: integer; const What: TTabStop;
      const Next: Boolean): integer;
    function GetBackStop(const X, Y: integer): integer;

    procedure TextAllChangedInternal(const Unselect: Boolean);

    { property }
    procedure SetGutterWidth(AWidth: integer);
    procedure SetGutterColor(AColor: TColor);
    procedure SetFont(Value: TFont);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetLines(ALines: TEditorStrings);
    function GetRealOffs(DefOffs, Index: integer): integer;
    function GetSelStart: integer;
    procedure SetSelStart(const ASelStart: integer);
    procedure SetSelLength(const ASelLength: integer);
    function GetSelLength: integer;
    procedure SetMode(index: integer; Value: boolean);
    procedure SetCaretPosition(const index, Pos: integer);
    procedure SetCols(ACols: integer);
    procedure SetRows(ARows: integer);
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetRightMarginVisible(Value: boolean);
    procedure SetRightMargin(Value: integer);
    procedure SetRightMarginColor(Value: TColor);

    function ExtractStringWithStyle(XX, YY: integer; const From: string; Style: word;
      const LineAttrs: TLineAttrs; out start: integer): string;
    procedure GetWordUnderCursor(X, Y: integer; aShift: TShiftState=[]);

    function GetAfterLoad: TNotifyEvent;
    procedure SetAfterLoad(Value: TNotifyEvent);
    function GetBeforeSave: TNotifyEvent;
    procedure SetBeforeSave(Value: TNotifyEvent);

    procedure SetWordWrap(Value: boolean);
    procedure SetStripInvisible(Value: boolean);
    procedure SetSelectedText(Value: boolean);

    procedure FontChanged(Sender: TObject);
    procedure SetTopRow(const Value: integer);
  protected
    SelAttrs_Size: integer;
    SelAttrs: TSelAttrs;

    property FSelectedText: boolean read FSelected write SetSelectedText;

    procedure Resize; {$IFDEF RA_D4H}override; {$ELSE}dynamic; {$ENDIF RA_D4H}
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var
      ScrollPos: integer);
    procedure Scroll(const Vert: boolean; const ScrollPos: integer);
    procedure PaintLine(const Line: integer; ColBeg, ColEnd: integer);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    {$IFDEF MEMOEX_EDITOR}
    procedure KeyPress(var Key: Char); override;
    procedure InsertChar(const Key: Char);
    {$ENDIF MEMOEX_EDITOR}

    procedure SetSel(const ASelX, ASelY: integer);
    function GetAttrDelta(StartFrom, EndTo: integer; const LineAttrs: TLineAttrs): integer;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure DblClick; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: integer; MousePos: TPoint): boolean; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
//    procedure DrawRightMargin;
    procedure PaintSelection;
    procedure SetUnSelected;
    procedure Mouse2Cell(const X, Y: integer; var CX, CY: integer);
    procedure Mouse2Caret(const X, Y: integer; var CX, CY: integer);
    procedure CaretCoord(const X, Y: integer; var CX, CY: integer);
    function PosFromMouse(const X, Y: integer): integer;
    procedure SetLockText(const Text: string);
//    function ExpandTabs(const S: string): string;

    {$IFDEF MEMOEX_UNDO}
    procedure CantUndo;
    {$ENDIF MEMOEX_UNDO}

    procedure SetCaretInternal(X, Y: integer);
    procedure ValidateEditBuffer;
    procedure SetXY(X, Y: integer);

    {$IFDEF MEMOEX_EDITOR}
    procedure ChangeBookMark(const BookMark: TBookMarkNum; const Valid:
      boolean);
    procedure InsertText(const Text: string);
    {$ENDIF MEMOEX_EDITOR}
    procedure BeginRecord;
    procedure EndRecord(var AMacro: TMacro);
    procedure PlayMacro(const AMacro: TMacro);
    function YinBounds(AY: integer): boolean;
    function DoChangeCase(const st: string; Conversion: byte): string;

    { triggers for descendants }
    procedure Changed; dynamic;
    procedure TextAllChanged; dynamic;
    procedure StatusChanged; dynamic;
    procedure SelectionChanged; dynamic;
    procedure ClipboardChanged; dynamic;
    procedure GetLineAttr(Line, LineIdx, LineOffs, LineLen, ColBeg, ColEnd: integer;
       const ALine: string; var FAttrs: TLineAttrs); virtual;
    procedure GutterPaint(Canvas: TCanvas; const Rect: TRect); dynamic;
    procedure BookmarkChanged(BookMark: integer); dynamic;
    {$IFDEF MEMOEX_COMPLETION}
    procedure CompletionIdentifer(var Cancel: boolean); dynamic;
    procedure CompletionTemplate(var Cancel: boolean); dynamic;
    {$ENDIF MEMOEX_COMPLETION}
    property Gutter: TGutter read FGutter;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;

    procedure WndProc(var Message: TMessage); override;

    procedure SetLeftTop(ALeftCol, ATopRow: integer);
    procedure ClipBoardCopy;
    procedure ClipBoardPaste;
    procedure ClipBoardCut;
    procedure DeleteSelected;
    function CalcCellRect(const X, Y: integer): TRect;
    procedure SetCaret(const X, Y: integer);
    procedure SetCaretAtParaPos(const ParaIndex, IndexOffs: integer);
    procedure CaretFromPos(Pos: integer; var X, Y: integer);
    function PosFromCaret(X, Y: integer): integer;
    procedure PaintCaret(bShow: boolean);
    function GetTextLen: integer;
    function GetSelText: string;
    procedure SetSelText(const AValue: string);
    function GetWordOnCaret: string;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure MakeRowVisible(ARow: integer);

    procedure Command(ACommand: TEditCommand); virtual;
    procedure PostCommand(ACommand: TEditCommand);
    {$IFDEF MEMOEX_EDITOR}
    procedure InsertTextAtCurrentPos(const _Text: string);
    function FindNext(const text: string; ignCase: boolean): boolean;
    procedure ReplaceWord(const NewString: string);
    procedure ReplaceWord2(const NewString: string);
    {$ENDIF}
    procedure BeginCompound;
    procedure EndCompound;

    function GetText(Position: longint; Buffer: PChar; Count: longint): longint;

    function IsUndoEmpty: boolean;

    procedure MouseWheelScroll(Delta: integer);

{$ifdef CLIPBOARDPROTECT} // ClipProtect will trunc clipboard to 2KB
    property ClipProtect: boolean read FClipProtect write FClipProtect; // AB
{$endif}
    property ClipPasteRtfBackSlashConvert: boolean // AB: rtf
      read FClipPasteRtfBackSlashConvert write FClipPasteRtfBackSlashConvert;
    property LeftCol: integer read FLeftCol;
    property TopRow: integer read FTopRow write SetTopRow;
    property VisibleColCount: integer read FVisibleColCount;
    property VisibleRowCount: integer read FVisibleRowCount;
    property LastVisibleCol: integer read FLastVisibleCol;
    property LastVisibleRow: integer read FLastVisibleRow;
    property Cols: integer read FCols write SetCols;
    property Rows: integer read FRows write SetRows;
    property CaretX: integer index 0 read FCaretX write SetCaretPosition;
    property CaretY: integer index 1 read FCaretY write SetCaretPosition;
    property Modified: boolean read FModified write FModified;
    property SelStart: integer read GetSelStart write SetSelStart;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelText: string read GetSelText write SetSelText;
    property SelectedText: boolean read FSelected;
    property BookMarks: TBookMarks read FBookMarks;
    property Keyboard: TKeyboard read FKeyboard;
    property CellRect: TCellRect read FCellRect;
    {$IFDEF MEMOEX_UNDO}
    property UndoBuffer: TUndoBuffer read FUndoBuffer;
    {$ENDIF MEMOEX_UNDO}
    property Recording: boolean read FRecording;
    property WordUnderCursor: TWordUnderCursor read FWordUnderCursor;
    property SelBegY: integer read FSelBegY;
    property SelEndY: integer read FSelEndY;
  public { published in descendants }
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Lines: TEditorStrings read FLines write SetLines;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property Cursor default crIBeam;
    property Color default clWindow;

    property Font: TFont read FFont write SetFont;

    property GutterWidth: integer read FGutterWidth write SetGutterWidth;
    property GutterColor: TColor read FGutterColor write SetGutterColor default clBtnFace;
    property RightMarginVisible: boolean read FRightMarginVisible write SetRightMarginVisible default true;
    property RightMargin: integer read FRightMargin write SetRightMargin default 80;
    property RightMarginColor: TColor read FRightMarginColor write SetRightMarginColor default clBtnFace;
    property InsertMode: boolean index 0 read FInsertMode write SetMode default true;
    property ReadOnly: boolean index 1 read FReadOnly write SetMode default false;
    property DoubleClickLine: boolean read FDoubleClickLine write FDoubleClickLine default false;
    {$IFDEF MEMOEX_COMPLETION}
    property Completion: TCompletion read FCompletion write FCompletion;
    {$ENDIF MEMOEX_COMPLETION}
    property TabStops: string read FTabStops write FTabStops;
    property SmartTab: Boolean read FSmartTab write FSmartTab default true;
    property BackSpaceUnindents: Boolean read FBackSpaceUnindents write FBackSpaceUnindents default true;
    property AutoIndent: Boolean read FAutoIndent write FAutoIndent default true;
    property KeepTrailingBlanks: Boolean read FKeepTrailingBlanks write FKeepTrailingBlanks default false;
    property CursorBeyondEOF: Boolean read FCursorBeyondEOF write FCursorBeyondEOF default false;
    property CursorBeyondEOL: Boolean read FCursorBeyondEOL write FCursorBeyondEOL default false;
    property SelForeColor: TColor read FclSelectFC write FclSelectFC;
    property SelBackColor: TColor read FclSelectBC write FclSelectBC;

    property StripInvisible: boolean read FStripInvisible write SetStripInvisible default false;
    property WantTabs: boolean read FWantTabs write FWantTabs default true;
    property WordWrap: boolean read FWordWrap write SetWordWrap default true;

    property OnAfterLoad: TNotifyEvent read GetAfterLoad write SetAfterLoad;
    property OnBeforeSave: TNotifyEvent read GetBeforeSave write SetBeforeSave;

    property OnGetLineAttr: TOnGetLineAttr read FOnGetLineAttr write FOnGetLineAttr;
    property OnChangeStatus: TOnChangeStatus read FOnChangeStatus write FOnChangeStatus;
    property OnChangeClipboardState: TOnChangeClipboardState read FOnChangeClipboardState write FOnChangeClipboardState;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnPaintGutter: TOnPaintGutter read FOnPaintGutter write FOnPaintGutter;
    property OnMouseOver: TOnMouseOver read FOnMouseOver write FOnMouseOver;
    property OnWordClick: TOnWordClick read FOnWordClick write FOnWordClick;
    property OnBreakLine: TOnBreakLine read FOnBreakLine write FOnBreakLine;
    property OnConcatLine: TOnConcatLine read FOnConcatLine write FOnConcatLine;
    property OnTextInsert: TOnTextInsert read FOnTextInsert write FOnTextInsert;
    property OnCaseConversion: TOnCaseConversion read FOnCaseConversion write FOnCaseConversion;
    property OnInsertBlock: TOnInsertBlock read FOnInsertBlock write FOnInsertBlock;
    property OnSaveBlock: TOnSaveBlock read FOnSaveBlock write FOnSaveBlock;
    property OnInsertMacro: TOnInsertMacro read FOnInsertMacro write FOnInsertMacro;
    property OnBlockOperation: TOnBlockOperation read FOnBlockOperation write FOnBlockOperation;
    property OnSetCaretPos: TOnSetCaretPos read FOnSetCaretPos write FOnSetCaretPos;
    property OnClipboardPaste: TOnClipboardPaste read FOnClipboardPaste write FOnClipboardPaste;
    {$IFDEF MEMOEX_COMPLETION}
    property OnCompletionIdentifer: TOnCompletion read FOnCompletionIdentifer write FOnCompletionIdentifer;
    property OnCompletionTemplate: TOnCompletion read FOnCompletionTemplate write FOnCompletionTemplate;
    property OnCompletionDrawItem: TDrawItemEvent read FOnCompletionDrawItem write FOnCompletionDrawItem;
    property OnCompletionMeasureItem: TMeasureItemEvent read FOnCompletionMeasureItem write FOnCompletionMeasureItem;
    property OnPreprocessCompletion: TOnPreprocessCompletion read FOnPreprocessCompletion write FOnPreprocessCompletion;
    {$ENDIF MEMOEX_COMPLETION}
    property DockManager;
  end;

  TMemoEx = class(TCustomMemoEx)
  published
    property TabOrder;
    property BorderStyle;
    property Lines;
    property ScrollBars;
    property GutterWidth;
    property GutterColor;
    property RightMarginVisible;
    property RightMargin;
    property RightMarginColor;
    property InsertMode;
    property ReadOnly;
    property DoubleClickLine;
    {$IFDEF MEMOEX_COMPLETION}
    property Completion;
    {$ENDIF MEMOEX_COMPLETION}
    property TabStops;
    property SmartTab;
    property BackSpaceUnindents;
    property AutoIndent;
    property KeepTrailingBlanks;
    property CursorBeyondEOF;
    property CursorBeyondEOL;
    property SelForeColor;
    property SelBackColor;

    property StripInvisible;

    property OnAfterLoad;
    property OnBeforeSave;

    property OnEnter;
    property OnExit;
    property OnGetLineAttr;
    property OnChangeStatus;
    property OnChangeClipboardState;
    property OnScroll;
    property OnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnChange;
    property OnSelectionChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaintGutter;
    property OnMouseOver;
    property OnWordClick;
    property OnBreakLine;
    property OnConcatLine;
    property OnTextInsert;
    property OnCaseConversion;
    property OnInsertBlock;
    property OnSaveBlock;
    property OnInsertMacro;
    property OnBlockOperation;
    property OnSetCaretPos;
    {$IFDEF MEMOEX_COMPLETION}
    property OnCompletionIdentifer;
    property OnCompletionTemplate;
    property OnCompletionDrawItem;
    property OnCompletionMeasureItem;
    property OnPreprocessCompletion;
    {$ENDIF MEMOEX_COMPLETION}

    { TCustomControl }
    property Align;
    property Enabled;
    property Color;
    property Ctl3D;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property Visible;
    {$IFDEF RA_D4H}
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property UseDockManager default true;
    property DockSite;
    property DragKind;
    property ParentBiDiMode;

    property WantTabs default true;
    property WordWrap default true;

    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
    {$ENDIF RA_D4H}
  end;

  {$IFDEF MEMOEX_COMPLETION}
  TCompletionList = (cmIdentifers, cmTemplates);

  TCompletion = class(TPersistent)
  private
    FMemoEx: TCustomMemoEx;
    FPopupList: TListBox;
    FAutoChange: TStrings;
    FAutoChangeList: TList;
    FIdentifers: TStrings;
    FTemplates: TStrings;
    FItems: TStringList;
    FItemIndex: integer;
    FMode: TCompletionList;
    FDefMode: TCompletionList;
    FItemHeight: integer;
    FTimer: TTimer;
    FEnabled: boolean;
    FVisible: boolean;
    FDropDownCount: integer;
    FDropDownWidth: integer;
    FListBoxStyle: TListBoxStyle;
    FCaretChar: char;
    FCRLF: string;
    FSeparator: string;
    function DoKeyDown(Key: Word; Shift: TShiftState): boolean;
    procedure DoKeyPress(Key: Char);
    procedure OnTimer(Sender: TObject);
    procedure FindSelItem(var Eq: boolean);
    procedure ReplaceWord(const ANewString: string);

    function Cmp1(const S1, S2: string): integer;
    function Cmp2(const S1, S2: string): boolean;

    procedure AutoChangeChanged(Sender: TObject);
    procedure ClearAutoChangeList;
    procedure UpdateAutoChange;
    procedure SetStrings(index: integer; AValue: TStrings);
    function GetItemIndex: integer;
    procedure SetItemIndex(AValue: integer);
    function GetInterval: cardinal;
    procedure SetInterval(AValue: cardinal);
    procedure MakeItems;
    function GetItems: TStrings;
  public
    constructor Create2(AMemoEx: TCustomMemoEx);
    destructor Destroy; override;
    procedure DropDown(const AMode: TCompletionList; const ShowAlways: boolean);
    procedure DoCompletion(const AMode: TCompletionList);
    procedure CloseUp(const Apply: boolean);
    procedure SelectItem;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property Visible: boolean read FVisible write FVisible;
    property Mode: TCompletionList read FMode write FMode;
    property Items: TStringList read FItems;
  published
    property DropDownCount: integer read FDropDownCount write FDropDownCount
      default 6;
    property DropDownWidth: integer read FDropDownWidth write FDropDownWidth
      default 300;
    property Enabled: boolean read FEnabled write FEnabled default false;
    property Separator: string read FSeparator write FSeparator;
    property Identifers: TStrings index 0 read FIdentifers write SetStrings;
    property Templates: TStrings index 1 read FTemplates write SetStrings;
    property AutoChange: TStrings index 2 read FAutoChange write SetStrings;
    property ItemHeight: integer read FItemHeight write FItemHeight;
    property Interval: cardinal read GetInterval write SetInterval;
    property ListBoxStyle: TListBoxStyle read FListBoxStyle write FListBoxStyle;
    property CaretChar: char read FCaretChar write FCaretChar;
    property CRLF: string read FCRLF write FCRLF;
  end;
  {$ENDIF MEMOEX_COMPLETION}

const

 { Editor commands }

  ecCharFirst = $00;
  ecCharLast = $FF;
  ecCommandFirst = $100;
  ecUser = $8000; { use this for descendants }

  {Cursor}
  ecLeft = ecCommandFirst + 1;
  ecUp = ecLeft + 1;
  ecRight = ecLeft + 2;
  ecDown = ecLeft + 3;
  {Cursor with select}
  ecSelLeft = ecCommandFirst + 9;
  ecSelUp = ecSelLeft + 1;
  ecSelRight = ecSelLeft + 2;
  ecSelDown = ecSelLeft + 3;
  {Cursor position change according to word}
  ecPrevWord = ecSelDown + 1;
  ecNextWord = ecPrevWord + 1;
  ecSelPrevWord = ecPrevWord + 2;
  ecSelNextWord = ecPrevWord + 3;
  ecSelWord = ecPrevWord + 4;

  ecWindowTop = ecSelWord + 1;
  ecWindowBottom = ecWindowTop + 1;
  ecPrevPage = ecWindowTop + 2;
  ecNextPage = ecWindowTop + 3;
  ecSelPrevPage = ecWindowTop + 4;
  ecSelNextPage = ecWindowTop + 5;

  ecBeginLine = ecSelNextPage + 1;
  ecEndLine = ecBeginLine + 1;
  ecBeginDoc = ecBeginLine + 2;
  ecEndDoc = ecBeginLine + 3;
  ecSelBeginLine = ecBeginLine + 4;
  ecSelEndLine = ecBeginLine + 5;
  ecSelBeginDoc = ecBeginLine + 6;
  ecSelEndDoc = ecBeginLine + 7;
  ecSelAll = ecBeginLine + 8;

  ecScrollLineUp = ecSelAll + 1;
  ecScrollLineDown = ecScrollLineUp + 1;

  ecInsertPara = ecCommandFirst + 101;
  ecBackspace = ecInsertPara + 1;
  ecDelete = ecInsertPara + 2;
  ecChangeInsertMode = ecInsertPara + 3;
  ecTab = ecInsertPara + 4;
  ecBackTab = ecInsertPara + 5;
  ecIndent = ecInsertPara + 6;
  ecUnindent = ecInsertPara + 7;

  ecDeleteSelected = ecInsertPara + 10;
  ecClipboardCopy = ecInsertPara + 11;
  ecClipboardCut = ecClipboardCopy + 1;
  ecClipBoardPaste = ecClipboardCopy + 2;

  ecDeleteLine = ecClipBoardPaste + 1;
  ecDeleteWord = ecDeleteLine + 1;

  ecToUpperCase = ecDeleteLine + 2;
  ecToLowerCase = ecToUpperCase + 1;
  ecChangeCase = ecToUpperCase + 2;

  ecUndo = ecChangeCase + 1;
  ecRedo = ecUndo + 1;
  ecBeginCompound = ecUndo + 2; { not implemented }
  ecEndCompound = ecUndo + 3; { not implemented }

  ecBeginUpdate = ecUndo + 4;
  ecEndUpdate = ecUndo + 5;

  ecSetBookmark0 = ecEndUpdate + 1;
  ecSetBookmark1 = ecSetBookmark0 + 1;
  ecSetBookmark2 = ecSetBookmark0 + 2;
  ecSetBookmark3 = ecSetBookmark0 + 3;
  ecSetBookmark4 = ecSetBookmark0 + 4;
  ecSetBookmark5 = ecSetBookmark0 + 5;
  ecSetBookmark6 = ecSetBookmark0 + 6;
  ecSetBookmark7 = ecSetBookmark0 + 7;
  ecSetBookmark8 = ecSetBookmark0 + 8;
  ecSetBookmark9 = ecSetBookmark0 + 9;

  ecGotoBookmark0 = ecSetBookmark9 + 1;
  ecGotoBookmark1 = ecGotoBookmark0 + 1;
  ecGotoBookmark2 = ecGotoBookmark0 + 2;
  ecGotoBookmark3 = ecGotoBookmark0 + 3;
  ecGotoBookmark4 = ecGotoBookmark0 + 4;
  ecGotoBookmark5 = ecGotoBookmark0 + 5;
  ecGotoBookmark6 = ecGotoBookmark0 + 6;
  ecGotoBookmark7 = ecGotoBookmark0 + 7;
  ecGotoBookmark8 = ecGotoBookmark0 + 8;
  ecGotoBookmark9 = ecGotoBookmark0 + 9;

  ecCompletionIdentifers = ecGotoBookmark9 + 1;
  ecCompletionTemplates = ecCompletionIdentifers + 1;

  ecRecordMacro = ecCompletionTemplates + 1;
  ecPlayMacro = ecRecordMacro + 1;
  ecBeginRecord = ecRecordMacro + 2;
  ecEndRecord = ecRecordMacro + 3;

  ecSaveBlock = ecEndRecord + 1;
  ecInsertBlock = ecSaveBlock + 1;

  ecInsertMacro0 = ecInsertBlock + 1;
  ecInsertMacro1 = ecInsertMacro0 + 1;
  ecInsertMacro2 = ecInsertMacro0 + 2;
  ecInsertMacro3 = ecInsertMacro0 + 3;
  ecInsertMacro4 = ecInsertMacro0 + 4;
  ecInsertMacro5 = ecInsertMacro0 + 5;
  ecInsertMacro6 = ecInsertMacro0 + 6;
  ecInsertMacro7 = ecInsertMacro0 + 7;
  ecInsertMacro8 = ecInsertMacro0 + 8;
  ecInsertMacro9 = ecInsertMacro0 + 9;
  ecInsertMacroA = ecInsertMacro0 + 10;
  ecInsertMacroB = ecInsertMacro0 + 11;
  ecInsertMacroC = ecInsertMacro0 + 12;
  ecInsertMacroD = ecInsertMacro0 + 13;
  ecInsertMacroE = ecInsertMacro0 + 14;
  ecInsertMacroF = ecInsertMacro0 + 15;
  ecInsertMacroG = ecInsertMacro0 + 16;
  ecInsertMacroH = ecInsertMacro0 + 17;
  ecInsertMacroI = ecInsertMacro0 + 18;
  ecInsertMacroJ = ecInsertMacro0 + 19;
  ecInsertMacroK = ecInsertMacro0 + 20;
  ecInsertMacroL = ecInsertMacro0 + 21;
  ecInsertMacroM = ecInsertMacro0 + 22;
  ecInsertMacroN = ecInsertMacro0 + 23;
  ecInsertMacroO = ecInsertMacro0 + 24;
  ecInsertMacroP = ecInsertMacro0 + 25;
  ecInsertMacroQ = ecInsertMacro0 + 26;
  ecInsertMacroR = ecInsertMacro0 + 27;
  ecInsertMacroS = ecInsertMacro0 + 28;
  ecInsertMacroT = ecInsertMacro0 + 29;
  ecInsertMacroU = ecInsertMacro0 + 30;
  ecInsertMacroV = ecInsertMacro0 + 31;
  ecInsertMacroW = ecInsertMacro0 + 32;
  ecInsertMacroX = ecInsertMacro0 + 33;
  ecInsertMacroY = ecInsertMacro0 + 34;
  ecInsertMacroZ = ecInsertMacro0 + 35;

  ecBlockOpA = ecInsertMacroZ + 1;
  ecBlockOpB = ecBlockOpA + 1;
  ecBlockOpC = ecBlockOpA + 2;
  ecBlockOpD = ecBlockOpA + 3;
  ecBlockOpE = ecBlockOpA + 4;
  ecBlockOpF = ecBlockOpA + 5;
  ecBlockOpG = ecBlockOpA + 6;
  ecBlockOpH = ecBlockOpA + 7;
  ecBlockOpI = ecBlockOpA + 8;
  ecBlockOpJ = ecBlockOpA + 9;
  ecBlockOpK = ecBlockOpA + 10;
  ecBlockOpL = ecBlockOpA + 11;
  ecBlockOpM = ecBlockOpA + 12;
  ecBlockOpN = ecBlockOpA + 13;
  ecBlockOpO = ecBlockOpA + 14;
  ecBlockOpP = ecBlockOpA + 15;
  ecBlockOpQ = ecBlockOpA + 16;
  ecBlockOpR = ecBlockOpA + 17;
  ecBlockOpS = ecBlockOpA + 18;
  ecBlockOpT = ecBlockOpA + 19;
  ecBlockOpU = ecBlockOpA + 20;
  ecBlockOpV = ecBlockOpA + 21;
  ecBlockOpW = ecBlockOpA + 22;
  ecBlockOpX = ecBlockOpA + 23;
  ecBlockOpY = ecBlockOpA + 24;
  ecBlockOpZ = ecBlockOpA + 25;

  ecBackword = ecBlockOpZ + 1;
  ecScrollPageUp = ecBackword + 1;
  ecScrollPageDown = ecScrollPageUp + 1;

  twoKeyCommand = High(word);

const
  __Brackets = ['(',')','[',']','{','}'];
  __StdWordDelims = [#0..' ',',','.',';','\',':','''','`']{ + __Brackets};

procedure Register;

function Max(x,y:integer):integer;
function Min(x,y:integer):integer;


implementation

uses
  Consts, RTLConsts;

function PosEx(const SubStr, S: AnsiString; Offset: Cardinal = 1): Integer;
// Faster Equivalent of Delphi 7 StrUtils.PosEx
asm
  push    ebx
  push    esi
  push    edx              {@Str}
  test    eax, eax
  jz      @@NotFound       {Exit if SubStr = ''}
  test    edx, edx
  jz      @@NotFound       {Exit if Str = ''}
  mov     esi, ecx
  mov     ecx,[edx-4]     {Length(Str)}
  mov     ebx,[eax-4]     {Length(SubStr)}
  add     ecx, edx
  sub     ecx, ebx        {Max Start Pos for Full Match}
  lea     edx,[edx+esi-1] {Set Start Position}
  cmp     edx, ecx
  jg      @@NotFound       {StartPos > Max Start Pos}
  cmp     ebx, 1           {Length(SubStr)}
  jle     @@SingleChar     {Length(SubStr) <= 1}
  push    edi
  push    ebp
  lea     edi,[ebx-2]     {Length(SubStr) - 2}
  mov     esi, eax
  movzx   ebx,[eax]       {Search Character}
@@Loop:                    {Compare 2 Characters per Loop}
  cmp     bl,[edx]
  jne     @@NotChar1
  mov     ebp, edi         {Remainder}
@@Char1Loop:
  movzx   eax, word ptr [esi+ebp]
  cmp     ax,[edx+ebp]
  jne     @@NotChar1
  sub     ebp, 2
  jnc     @@Char1Loop
  pop     ebp
  pop     edi
  jmp     @@SetResult
@@NotChar1:
  cmp     bl,[edx+1]
  jne     @@NotChar2
  mov     ebp, edi         {Remainder}
@@Char2Loop:
  movzx   eax, word ptr [esi+ebp]
  cmp     ax,[edx+ebp+1]
  jne     @@NotChar2
  sub     ebp, 2
  jnc     @@Char2Loop
  pop     ebp
  pop     edi
  jmp     @@CheckResult
@@NotChar2:
  lea     edx,[edx+2]
  cmp     edx, ecx         {Next Start Position <= Max Start Position}
  jle     @@Loop
  pop     ebp
  pop     edi
  jmp     @@NotFound
@@SingleChar:
  jl      @@NotFound       {Needed for Zero-Length Non-NIL Strings}
  movzx   eax,[eax]       {Search Character}
@@CharLoop:
  cmp     al,[edx]
  je      @@SetResult
  cmp     al,[edx+1]
  je      @@CheckResult
  lea     edx,[edx+2]
  cmp     edx, ecx
  jle     @@CharLoop
@@NotFound:
  xor     eax, eax
  pop     edx
  pop     esi
  pop     ebx
  ret
@@CheckResult:             {Check within AnsiString}
  cmp     edx, ecx
  jge     @@NotFound
  add     edx, 1
@@SetResult:
  pop     ecx              {@Str}
  pop     esi
  pop     ebx
  neg     ecx
  lea     eax,[edx+ecx+1]
end;



function StringReplaceAll(const S, OldPattern, NewPattern: AnsiString): AnsiString;
// fast replacement of StringReplace(S, OldPattern, NewPattern,[rfReplaceAll]);
procedure Process(j: integer);
var i: integer;
begin
  Result := '';
  i := 1;
  repeat
    Result := Result+Copy(S,i,j-i)+NewPattern;
    i := j+length(OldPattern);
    j := PosEx(OldPattern, S, i);
    if j=0 then begin
      Result := Result+Copy(S, i, maxInt);
      break;
    end;
  until false;
end;
var j: integer;
begin
  j := PosEx(OldPattern, S);
  if j=0 then
    result := S else
    Process(j);
end;

const
  StIdSymbols = ['_', '0'..'9', 'A'..'Z', 'a'..'z', '¿'..'ﬂ', '‡'..'ˇ'];
  _StIdSymbols = ['>','<','''', '"', '`','!','@','#','$','%','^','&','*','/','?'] + __Brackets + StIdSymbols + [#127..#255];
  _AutoChangePunctuation = [' ','`','~','!','@','#','$','%','^','&','*','(',')','_','-','+','=',';',':','''','"','[',']','{','}',',','.','/','?','<','>'];

function Max(x,y:integer):integer;
begin
  if x > y then
    Result := x else
    Result := y;
end;

function Min(x,y:integer):integer;
begin
  if x < y then
    Result := x else
    Result := y;
end;

function HasChar(Ch : Char; const S : string) : boolean;
begin
  Result := PosEx(Ch, S) > 0;
end;

function GetWordOnPosEx(const S : string; const P : integer; out iBeg, iEnd : integer) : string;
begin
  Result := '';
  if (P > Length(S)) or (P < 1) then
    exit;
  iBeg := P;
  if S[P] in Separators then
    if (P < 1) or (S[P-1] in Separators) then
      inc(iBeg)
    else if not (S[P-1] in Separators) then
      dec(iBeg);
  while iBeg >= 1 do
    if S[iBeg] in Separators then
      break else
      dec(iBeg);
  inc(iBeg);
  iEnd := P;
  while iEnd <= Length(S) do
    if S[iEnd] in Separators then
      break else
      inc(iEnd);
  if iEnd > iBeg then
    Result := Copy(S, iBeg, iEnd - iBeg) else
    Result := S[P];
end;


var
  CF_MEMOEX: integer = 0; // indicates MemoEx clipboard

function _CopyToClipboard(Handle: THandle; const SelText: string; FontCharset: TFontCharset): boolean;
var
  Data: HGLOBAL;
  _Text: PChar;
  fmt: UINT;
begin
  Result := false;
  if SelText <> '' then
  begin
    Data := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT, length(SelText) + 1);
    if Data <> 0 then
    begin
      _Text := GlobalLock(Data);
      if Assigned(_Text) then
      begin
        StrCopy(_Text, PChar(SelText));
        GlobalUnlock(Data);
        if FontCharset = OEM_CHARSET then
          fmt := CF_OEMTEXT else
          fmt := CF_TEXT;
        if OpenClipboard(Handle) then
        begin
          EmptyClipboard;
          SetClipboardData(fmt, Data);
          SetClipboardData(CF_MEMOEX,0); // indicates from MemoEx
          CloseClipboard;
          Result := true;
        end
          else GlobalFree(Data);
      end
        else GlobalFree(Data);
    end;
  end;
end;

function _PasteFromClipboard(Handle: THandle; FontCharset: TFontCharset;
  {$ifdef CLIPBOARDPROTECT}const InternalClip: string;{$endif}
  DeleteCRLF: boolean = true): string;
var
  fmtText, fmtOEMText: boolean;
  Data: HGLOBAL;
  fmt: UINT;
  _Text: PChar;
  Txt: string;
  i: integer;
begin
{$ifdef CLIPBOARDPROTECT} // ClipProtect will trunc clipboard to 2KB
  if IsClipboardFormatAvailable(CF_MEMOEX) and (InternalClip<>'') then begin
    result := InternalClip;
    exit;
  end;
{$endif}
  Result := '';
  fmtText := IsClipboardFormatAvailable(CF_TEXT);
  fmtOEMText := IsClipboardFormatAvailable(CF_OEMTEXT);
  if fmtText or fmtOEMText then
    if OpenClipboard(Handle) then
    begin
      if not fmtText then
        fmt := CF_OEMTEXT else
        fmt := CF_TEXT;
      Data := GetClipboardData(fmt);
      if Data <> 0 then begin
        _Text := GlobalLock(Data);
        if Assigned(_Text) then begin
          Txt := _Text;
          if (FontCharset = OEM_CHARSET) and (fmt = CF_TEXT) then
            CharToOEM(PChar(Txt), PChar(Txt)) else
          if (FontCharset = DEFAULT_CHARSET) and (fmt = CF_OEMTEXT) then
            OEMToChar(PChar(Txt), PChar(Txt));
          if DeleteCRLF then begin
            i := 1;
            while i <= length(Txt) do
              if Txt[i] in [#10, #13] then
                System.Delete(Txt, i, 1) else begin
                if Txt[i]<' ' then // prevent TAB bug
                  Txt[i] := ' ';
                inc(i);
              end;
          end else
            for i := 1 to length(Txt) do
              if Txt[i] in [#1..#9,#11,#12,#14..#31] then // prevent TAB bug
                Txt[i] := ' ';
          Result := Txt;
          GlobalUnlock(Data);
        end;
      end;
      CloseClipboard;
    end;
end;

function GetWordOnPos(const S : string; const P : integer) : string;
var i, Beg : integer;
begin
  Result := '';
  if (P > Length(S)) or (P < 1) then exit;
  for i := P downto 1 do
    if S[i] in Separators then
      break;
  Beg := i + 1;
  for i := P to Length(S) do
    if S[i] in Separators then
      break;
  if i > Beg then
    Result := Copy(S, Beg, i-Beg) else
    Result := S[P];
end;

function SubStr(const S : string; const index : integer; const Separator : string) : string;
// used on word completion
var i : integer;
    pB, pE : PChar;
begin
  Result := '';
  if (index < 0) or ((index = 0) and (Length(S) > 0) and (S[1] = Separator)) then exit;
  pB := PChar(S);
  for i := 1 to index do begin
    pB := StrPos(pB, PChar(Separator));
    if pB = nil then exit;
    pB := pB+Length(Separator);
  end;
  pE := StrPos(pB+1, PChar(Separator));
  if pE = nil then pE := PChar(S)+Length(S);
  if not (StrLIComp(pB, PChar(Separator), Length(Separator)) = 0) then
    SetString(Result, pB, pE-pB);
end;

function KeyPressed(VK: integer) : boolean;
begin
  Result := GetKeyState(VK) and $8000 = $8000;
end;


{ TRAControlScrollBar95 }

constructor TRAControlScrollBar95.Create;
begin
  FPage := 1;
  FSmallChange := 1;
  FLargeChange := 1;
end;

const
  SBKIND : array[TScrollBarKind] of integer = (SB_HORZ, SB_VERT);

procedure TRAControlScrollBar95.SetParams(AMin, AMax, APosition, APage : integer);
var
  SCROLLINFO : TSCROLLINFO;
begin
  if AMax < AMin then
    raise EInvalidOperation.Create(SScrollBarRange);
  if APosition < AMin then APosition := AMin;
  if APosition > AMax then APosition := AMax;
  if Handle > 0 then begin
    with SCROLLINFO do begin
      cbSize := sizeof(TSCROLLINFO);
      fMask := SIF_DISABLENOSCROLL;
      if (AMin >= 0) or (AMax >= 0) then fMask := fMask or SIF_RANGE;
      if APosition >= 0 then fMask := fMask or SIF_POS;
      if APage >= 0 then fMask := fMask or SIF_PAGE;
      nPos := APosition;
      nMin := AMin;
      nMax := AMax;
      nPage := APage;
    end;
    SetScrollInfo(
      Handle,         // handle of window with scroll bar
      SBKIND[Kind] ,  // scroll bar flag
      SCROLLINFO,     // pointer to structure with scroll parameters
      true            // redraw flag
    );
  end;
end;

procedure TRAControlScrollBar95.SetParam(index, Value: Integer);
begin
  case index of
    0 : FMin := Value;
    1 : FMax := Value;
    2 : FPosition := Value;
    3 : FPage := Value;
  end;
  if FMax < FMin then
    raise EInvalidOperation.Create(SScrollBarRange);
  if FPosition < FMin then FPosition := FMin;
  if FPosition > FMax then FPosition := FMax;
  SetParams(FMin, FMax, FPosition, FPage);
end;

procedure TRAControlScrollBar95.DoScroll(var Message: TWMScroll);
var
  ScrollPos: Integer;
  NewPos: Longint;
  ScrollInfo: TScrollInfo;
begin
  with Message do
  begin
    NewPos := FPosition;
    case TScrollCode(ScrollCode) of
      scLineUp:
        Dec(NewPos, FSmallChange);
      scLineDown:
        Inc(NewPos, FSmallChange);
      scPageUp:
        Dec(NewPos, FLargeChange);
      scPageDown:
        Inc(NewPos, FLargeChange);
      scPosition, scTrack:
        with ScrollInfo do
        begin
          cbSize := SizeOf(ScrollInfo);
          fMask := SIF_ALL;
          GetScrollInfo(Handle, SBKIND[Kind], ScrollInfo);
          NewPos := nTrackPos;
        end;
      scTop:
        NewPos := FMin;
      scBottom:
        NewPos := FMax;
    end;
    if NewPos < FMin then NewPos := FMin;
    if NewPos > FMax then NewPos := FMax;
    ScrollPos := NewPos;
    Scroll(TScrollCode(ScrollCode), ScrollPos);
  end;
  Position := ScrollPos;
end;

procedure TRAControlScrollBar95.Scroll(ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if Assigned(FOnScroll) then
    FOnScroll(Self, ScrollCode, ScrollPos);
end;



{$IFDEF MEMOEX_UNDO}

type

  TCaretUndo = class(TUndo)
  private
    FCaretX, FCaretY: integer;
  public
    constructor Create(const AMemoEx: TCustomMemoEx; const ACaretX, ACaretY: integer);
    procedure Undo; override;
    procedure Redo; override;
  end;

  TInsertUndo = class(TCaretUndo)
  private
    FText: string;
    FOffset, FParaOffset: integer;
  public
    constructor Create(const AMemoEx: TCustomMemoEx; const ACaretX, ACaretY: integer;
      const AText: string);
    procedure Undo; override;
  end;

  TReLineUndo = class(TInsertUndo);

  TInsertTabUndo = class(TInsertUndo);

  TOverwriteUndo = class(TCaretUndo)
  private
    FOldText, FNewText: string;
    FOffset: integer;
  public
    constructor Create(const AMemoEx: TCustomMemoEx; const ACaretX, ACaretY: integer;
      const AOldText, ANewText: string);
    procedure Undo; override;
  end;

  TDeleteUndo = class(TInsertUndo)
  public
    procedure Undo; override;
  end;

  TDeleteTrailUndo = class(TDeleteUndo);

  TBackspaceUndo = class(TDeleteUndo)
  public
    procedure Undo; override;
  end;

  TReplaceUndo = class(TCaretUndo)
  private
    FBeg, FEnd: integer;
    FText, FNewText: string;
  public
    constructor Create(const AMemoEx: TCustomMemoEx; const ACaretX, ACaretY: integer;
      const ABeg, AEnd: integer; const AText, ANewText: string);
    procedure Undo; override;
  end;

  TDeleteSelectedUndo = class(TDeleteUndo)
  private
    FSelBlock: boolean; { vertical block }
    FSelBegX, FSelBegY, FSelEndX, FSelEndY, FSelOffs: integer;
  public
    constructor Create(const AMemoEx: TCustomMemoEx; const ACaretX, ACaretY: integer;
      const AText: string; const ASelBlock: boolean; const ASelBegX, ASelBegY, ASelEndX,
      ASelEndY, ASelOffs: integer);
    procedure Undo; override;
  end;

  TSelectUndo = class(TCaretUndo)
  private
    FSelBlock: boolean; { vertical block }
    FSelBegX, FSelBegY, FSelEndX, FSelEndY: integer;
  public
    constructor Create(const AMemoEx: TCustomMemoEx; const ACaretX, ACaretY: integer;
      const ASelBlock: boolean; const ASelBegX, ASelBegY, ASelEndX, ASelEndY: integer);
    procedure Undo; override;
  end;

  TUnselectUndo = class(TSelectUndo);

  TBeginCompoundUndo = class(TUndo)
  public
    procedure Undo; override;
  end;

  TEndCompoundUndo = class(TBeginCompoundUndo);

{$ENDIF MEMOEX_UNDO}

procedure Err;
begin
  MessageBeep(0);
end;

function FindNotBlankCharPos(const S: string): integer;
begin
  for result := 1 to Length(S) do
    if S[result] <> ' ' then
      Exit;
  Result := 1;
end;

function ANSIChangeCase(const S: string): string;
var i: integer;
    Up: ANSIChar;
begin
  Result := S;
  for i := 1 to Length(Result) do begin
    Up := upcase(Result[i]);
    if Result[i] = Up then
      Result[i] := chr(ord(Result[i])+32) else
      Result[i] := Up;
  end;
end;

function StringDynArrayGetSize(V: PIntegerArray; FCount: integer): integer;
// Siz := StringDynArrayGetSize(pointer(FStrings),FCount);
var i: integer;
begin
  result := 0;
  for i := 0 to FCount-1 do 
    if V^[i]<>0 then // very fast inc(Result, length(FStrings[i-1])
      inc(result,PInteger(V^[i]-4)^);
end;

function StringDynArrayToPChar(V: PIntegerArray; FCount: integer; P: PChar): PChar;
// StringDynArrayToPChar(pointer(FStrings),FCount,pointer(Result))
var i, Size: integer;
begin
  for i := 0 to FCount-1 do
  if V^[i]<>0 then begin
    size := PInteger(V^[i]-4)^;
    move(pointer(V^[i])^,P^,size);
    inc(P,size);
  end;
  result := P;
end;


{ TEditorStrings }

procedure TEditorStrings.LoadFromFile(const FileName: string);
begin
  BeginUpdate;
  inherited LoadFromFile(FileName);
  if Assigned(FOnAfterLoad) then
    FOnAfterLoad(FMemoEx);
  EndUpdate;
end;

procedure TEditorStrings.SaveToFile(const FileName: string);
begin
  if Assigned(FOnBeforeSave) then
    FOnBeforeSave(FMemoEx);
  inherited SaveToFile(FileName);
end;

procedure TEditorStrings.Recount(Index: integer);
var i: integer;
begin
  if FCount=0 then exit;
  if Index=0 then begin
    FList[0].FPreCount := 0;
    inc(Index);
  end;
  for i := Index to FCount - 1 do
    with FList[i-1] do
      FList[i].FPreCount := FPreCount + FCount;
end;

procedure TEditorStrings.Index2ParaIndex(Index: integer; out Para, ParaIndex: integer);
var L, H, I: integer;
begin
 if (not FMemoEx.FWordWrap) or (FParaLinesCount = FCount) then begin
    Para := Index;
    if Para > FCount - 1 then
      Para := FCount - 1;
    ParaIndex := 0;
  end else
  begin
    { fast find paragraph index using MinMax (binary search) algo }
    Para := -1;
    ParaIndex := -1;
    L := 0;
    H := FCount - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      with FList[I] do
      if Index > FPreCount + FCount - 1 then
        L := I + 1 else begin
        H := I - 1;
        if (Index <= FPreCount + FCount - 1) and (Index >= FPreCount) then
        begin // found
          Para := I;
          ParaIndex := Index - FPreCount;
          break;
        end;
      end;
    end;
  end;
end;

procedure ListIndexError(Index: integer);
// outside procedure -> no temp string -> less heap
function ReturnAddr: Pointer;
asm
  MOV     EAX,[EBP+4]
end;
begin
  raise EStringListError.CreateFmt(SListIndexError, [Index]) at ReturnAddr;
end;

function TEditorStrings.GetParagraphByIndex(
  Index: integer; out ParaIndex, IndexOffs: integer): string;
var _P, _PI: integer;
begin
  IndexOffs := 0;
  ParaIndex := 0;
  Result := '';
  Index2ParaIndex(Index, _P, _PI);
  if (_P = -1) or (_PI = -1) then
    ListIndexError(Index);
  ParaIndex := _P;
  result := _GetString(_P);
  IndexOffs := StringDynArrayGetSize(pointer(FList[_P].FStrings),_PI);
end;

procedure TEditorStrings.Caret2Paragraph(
  X, Y: integer; out ParaIndex, IndexOffs: integer);
var _P, _PI: integer;
begin
  ParaIndex := 0;
  IndexOffs := 0;
  Index2ParaIndex(Y, _P, _PI);
  if (_P = -1) or (_PI = -1) then
    ListIndexError(Y);
  ParaIndex := _P;
  IndexOffs := X+StringDynArrayGetSize(pointer(FList[_P].FStrings),_PI);
end;

procedure TEditorStrings.Paragraph2Caret(
  ParaIndex, IndexOffs: integer; out X, Y: integer);
var i, j, k: integer;
    found: boolean;
begin
  X := 0;
  Y := ParaIndex;
  found := false;
  k := 0;
  for i := 0 to FCount - 1 do
  with Flist[i] do begin
    if i >= Y then begin
      for j := 0 to FCount - 1 do begin
        inc(X, length(FStrings[j]));
        if X >= IndexOffs then begin
          found := true;
          Y := k + j;
          X := IndexOffs - (X - length(FStrings[j]));
          break;
        end;
      end;
      if found then break;
    end;
    inc(k, FCount);
  end;
  if not found then begin
    if X > 0 then begin
      Y := k;
      X := length(FList[Y].FStrings[FList[Y].FCount - 1]);
      exit;
    end;
    Y := FCount - 1;
    if Y >= 0 then
      X := length(FList[Y].FStrings[FList[Y].FCount - 1]) else begin
      X := 0;
      Y := 0;
    end;
  end;
end;

function TEditorStrings.GetParaOffs(ParaIndex: integer): integer;
var i: integer;
begin
  Result := ParaIndex*2;
  for i := 0 to ParaIndex -1 do
  with FList[i] do
    inc(Result, StringDynArrayGetSize(pointer(FStrings),FCount));
end;

procedure TEditorStrings.ReformatParagraph(ParaIndex: integer);
var // full rewrite by AB: much faster and use less ram
  s: string;
  c, d, b: PChar;
  L: integer;
begin
  with FList[ParaIndex] do begin
    dec(FParaLinesCount, FCount);
    FPreCount := 0;
    if FMemoEx.FWordWrap then begin
      s := _GetString(ParaIndex); // whole paragraph text in s
      L := FMemoEx.FRightMargin;
      if length(s)<=L then begin
        if FCount>1 then begin
          SetLength(FStrings,1);
          FCount := 1;
          FStrings[0] := s;
        end;
      end else begin
        FCount := 0;
        c := @s[1]; // UniqueString() because we change FStrings[0] below
        d := c;
        b := c;
        while (c^<>#0) do begin
          if c^=' ' then
            b := c; // b = last ' '
          if ((c-d)>=L) then begin
            inc(FCount);
            if FCount>=length(FStrings) then
              Setlength(FStrings,FCount+10);
            if b=d then
              b := d+L; // if no ' '
            SetString(FStrings[FCount-1],d,b-d+1);
            c := b+1;
            b := c;
            d := c;
            if c^=#0 then break;
          end;
          inc(c);
        end;
        if d<>c then begin // append last chars (if any) as last line
          inc(FCount);
          SetString(FStrings[FCount-1],d,c-d);
        end;
        Setlength(FStrings,FCount); // set exact line count
      end;
    end else if FCount>1 then begin // something to reformat only if FCount>1
      s := _GetString(ParaIndex);
      SetLength(FStrings,1);
      FCount := 1;
      FStrings[0] := s;
    end;
    inc(FParaLinesCount, FCount);
  end;
end;

procedure TEditorStrings.Reformat;
var
  i: integer;
begin
  for i := 0 to FCount - 1 do
    ReformatParagraph(i);
  Recount(0);
  Changed;
end;

procedure TEditorStrings.CheckLength(const st: string);
begin
  if length(st) > FMemoEx.Max_X then
    FMemoEx.SetMax_X(length(st) + 1);
end;

constructor TEditorStrings.Create;
begin
  inherited Create;
  FParaLinesCount := 0;
  FOnAfterLoad := nil;
  FOnBeforeSave := nil;
end;

destructor TEditorStrings.Destroy;
begin
//  FOnChange := nil;
  FOnChanging := nil;
  inherited Destroy;
end;

function TEditorStrings.Add(const S: string): integer;
begin
  Result := FCount;
  InsertItem(Result, S);
end;

procedure TEditorStrings.Delete(Index: integer);
begin
  if (Index < 0) or (Index >= FCount) then
    ListIndexError(Index);
  Changing;
  dec(FParaLinesCount, FList[Index].FCount);
  Dec(FCount);
  Finalize(FList[Index]); // avoid memory bug: release deleted FStrings
  if Index < FCount then begin
    System.Move(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(TParagraph));
    FillChar(FList[FCount],sizeof(TParagraph),0); // avoid memory bug
    Recount(Index);
  end;
  Changed;
end;

procedure TEditorStrings.Insert(Index: integer; const S: string);
begin
  if (Index < 0) or (Index > FCount) then
    ListIndexError(Index);
  InsertItem(Index, S);
end;

procedure TEditorStrings.InsertItem(Index: integer; const S: string);
begin
  Changing;
  if FCount = length(FList) then
    Grow;
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1], (FCount - Index) * SizeOf(TParagraph));
  fillchar(FList[Index],sizeof(TParagraph),0); // avoid memory bug
  Inc(FCount);
  AddParaStr(Index, S);
  Changed;
end;

{$WARNINGS OFF}
function TEditorStrings.AddParaStr(ParaIndex: integer; const S: string): integer;
begin
  if (ParaIndex < 0) or (ParaIndex >= FCount) then
    ListIndexError(ParaIndex);
  with FList[ParaIndex] do begin
    inc(FCount);
    inc(FParaLinesCount);
    SetLength(FStrings, FCount);
    FStrings[FCount - 1] := S;
    CheckLength(S);
  end;
  ReformatParagraph(ParaIndex);
  Recount(ParaIndex);
  Changed;
end;
{$WARNINGS ON}

procedure TEditorStrings.Changed;
begin
  if (csLoading in FMemoEx.ComponentState) then exit;
  if FMemoEx.FUpdateLock = 0 then begin
    FMemoEx.TextAllChanged;
    if Assigned(FMemoEx.FOnChange) then
      FMemoEx.FOnChange(Self);
  end;
end;

procedure TEditorStrings.Changing;
begin
  if (FMemoEx.FUpdateLock = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TEditorStrings.Clear;
begin
  if FCount <> 0 then begin
    Changing;
    FCount := 0;
    FParaLinesCount := 0;
    Finalize(FList); // free all memory
    Changed;
  end;
  fMemoEx.SetCaret(0,0);
end;

procedure TEditorStrings.BeginUpdate;
begin
  inc(FMemoEx.FUpdateLock);
end;

procedure TEditorStrings.EndUpdate;
begin
  dec(FMemoEx.FUpdateLock);
  Changed;
end;

function TEditorStrings._GetString(ParaIndex: integer): string;
begin
  with FList[ParaIndex] do
  if FCount>1 then begin
    SetString(Result,nil,StringDynArrayGetSize(pointer(FStrings),FCount));
    StringDynArrayToPChar(pointer(FStrings),FCount,pointer(Result));
  end else
    result := FStrings[0];
end;

function TEditorStrings.Get(Index: integer): string;
begin
  if cardinal(Index)>=cardinal(FCount) then
    ListIndexError(Index);
  Result := _GetString(Index);
end;

function TEditorStrings.GetParagraph(Index: integer): PParagraph;
begin
  if cardinal(Index) >= cardinal(FCount) then
    ListIndexError(Index);
  Result := @FList[Index];
end;

function TEditorStrings.GetParaString(Index: integer): string;
var _P, _PI: integer;
begin
  if not FMemoEx.FWordWrap or (FParaLinesCount = FCount) then
    Result := Get(Index) else begin
    Index2ParaIndex(Index, _P, _PI);
    if (_P = -1) or (_PI = -1) then
      ListIndexError(Index);
    Result := FList[_P].FStrings[_PI];
  end;
end;

procedure TEditorStrings.Grow;
var Delta: integer;
begin
  Delta := length(FList);
  if Delta>64*4 then
    inc(Delta,Delta shr 2) else
    inc(Delta,64); // AB
  SetLength(FList,Delta);
end;

procedure TEditorStrings._PutString(ParaIndex: integer; const S: string);
var
  old_count, old_precount: integer;
begin
  old_count := FList[ParaIndex].FCount;
  old_precount := FList[ParaIndex].FPreCount;
  dec(FParaLinesCount, FList[ParaIndex].FCount);
  with FList[ParaIndex] do begin
    FCount := 1;
    SetLength(FStrings, 1);
    FStrings[0] := S;
    FObject := nil;
  end;
  inc(FParaLinesCount);
  ReformatParagraph(ParaIndex);
  if old_count <> FList[ParaIndex].FCount then
    Recount(ParaIndex) else
    FList[ParaIndex].FPreCount := old_precount;
end;

procedure TEditorStrings.Put(Index: integer; const S: string);
begin
  if (Index < 0) or (Index >= FCount) then ListIndexError(Index);
  CheckLength(S);
  Changing;
  _PutString(Index, S);
  Changed;
end;

procedure TEditorStrings.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then ListIndexError(Index);
  FList[Index].FObject := AObject;
end;

function TEditorStrings.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then ListIndexError(Index);
  result := FList[Index].FObject;
end;

procedure TEditorStrings.PutParaString(Index: integer; const S: string);
var _P, _PI: integer;
    old_count, old_precount: integer;
begin
  if not FMemoEx.FWordWrap then
    Put(Index, S) else begin
    Index2ParaIndex(Index, _P, _PI);
    if (_P = -1) or (_PI = -1) then
      ListIndexError(Index);
    Changing;
    old_count := FList[_P].FCount;
    old_precount := FList[_P].FPreCount;
    CheckLength(S);
    FList[_P].FStrings[_PI] := S;
    ReformatParagraph(_P);
    if old_count <> FList[_P].FCount then
      Recount(_P) else
      FList[_P].FPreCount := old_precount;
    Changed;
  end;
end;

procedure TEditorStrings.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

procedure TEditorStrings.SetTextStr(const Value: string);
begin
  inc(FMemoEx.FUpdateLock);
  inherited SetTextStr(Value);
  dec(FMemoEx.FUpdateLock);
  if FMemoEx.FUpdateLock = 0 then
  begin
    {$IFDEF MEMOEX_EDITOR}FMemoEx.CantUndo;{$ENDIF MEMOEX_EDITOR}
    FMemoEx.TextAllChanged;
    FMemoEx.SetCaretInternal(0, 0);
  end;
end;

procedure TEditorStrings.ReLine;
// complete line with spaces if caret X is after real chars
var L: integer;
begin
  inc(FMemoEx.FUpdateLock);
  try
    {$IFDEF MEMOEX_UNDO}
    if FParaLinesCount = 0 then
      L := FMemoEx.FCaretX else
      L := Length(ParaStrings[FParaLinesCount - 1]);
    while FMemoEx.FCaretY > FParaLinesCount - 1 do
    begin
      if FParaLinesCount > 0 then
        TReLineUndo.Create(FMemoEx, L, FMemoEx.FCaretY, #13#10);
      L := 0;
      Add('');
    end;
    {$ENDIF MEMOEX_UNDO}
    if FMemoEx.FCaretX > Length(ParaStrings[FMemoEx.FCaretY]) then
    begin
      L := FMemoEx.FCaretX - Length(ParaStrings[FMemoEx.FCaretY]);
      {$IFDEF MEMOEX_UNDO}
      TReLineUndo.Create(FMemoEx, Length(ParaStrings[FMemoEx.FCaretY]),
        FMemoEx.FCaretY, StringOfChar(' ',L));
      {$ENDIF MEMOEX_UNDO}
      PutParaString(FMemoEx.FCaretY, ParaStrings[FMemoEx.FCaretY] + StringOfChar(' ',L));
    end;
  finally
    dec(FMemoEx.FUpdateLock);
  end;
end; { ReLine }

procedure TEditorStrings.SetLockText(const Text: string);
begin
  inc(FMemoEx.FUpdateLock);
  try
    inherited SetTextStr(Text);
  finally
    dec(FMemoEx.FUpdateLock);
  end;
end;

procedure TEditorStrings.SetInternalParaStr(Index: integer; const Value: string);
begin
  inc(FMemoEx.FUpdateLock);
  try
    PutParaString(Index, Value);
  finally
    dec(FMemoEx.FUpdateLock);
  end;
end;

procedure TEditorStrings.SetInternal(Index: integer; const Value: string);
begin
  inc(FMemoEx.FUpdateLock);
  try
    Put(Index, Value);
  finally
    dec(FMemoEx.FUpdateLock);
  end;
end;

function TEditorStrings.GetTextStr: string;
var i,sizetot: integer;
  P: PChar;
begin
//  result := inherited GetTextStr; exit; 8 times slower than code below
  SizeTot := GetTextLength;
  Setlength(Result, Sizetot);
  P := Pointer(Result);
  for i := 0 to FCount-1 do
  with FList[i] do begin
    P := StringDynArrayToPChar(pointer(FStrings),FCount,P); // line
    pWord(P)^ := $0A0D; inc(P,2); // CRLF
  end;
  assert((p-pointer(result))=sizeTot);
end;

function TEditorStrings.GetCount: Integer;
begin
  result := FCount;
end;

function TEditorStrings.GetTextLength: integer;
var i: integer;
begin
  Result := FCount*2; // CRLF char size
  for i := 0 to FCount-1 do
  with FList[i] do
    inc(Result,StringDynArrayGetSize(pointer(FStrings),FCount));
end;

function TEditorStrings.HasText: boolean;
var i: integer;
begin
  result := true;
  for i := 0 to FCount-1 do
  with FList[i] do
    if StringDynArrayGetSize(pointer(FStrings),FCount)<>0 then
      exit;
  result := false;
end;

{ TEditorClient }

function TEditorClient.GetCanvas: TCanvas;
begin
  Result := FMemoEx.Canvas;
end;

function TEditorClient.Left: integer;
begin
  Result := FMemoEx.GutterWidth + GutterRightMargin;
end;

function TEditorClient.Height: integer;
begin
  Result := FMemoEx.ClientHeight;
end;

function TEditorClient.Width: integer;
begin
  Result := Max(FMemoEx.ClientWidth - Left, 0);
end;

function TEditorClient.ClientWidth: integer;
begin
  Result := Width;
end;

function TEditorClient.ClientHeight: integer;
begin
  Result := Height;
end;

function TEditorClient.ClientRect: TRect;
begin
  Result := Bounds(Left, Top, Width, Height);
end;

function TEditorClient.BoundsRect: TRect;
begin
  Result := Bounds(0, 0, Width, Height);
end;

procedure TGutter.Invalidate;
begin
  Paint;
end;

procedure TGutter.Paint;
var Rect: TRect;
begin
  with FMemoEx, Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := FGutterColor;
    Rect.Left := 0;
    Rect.Top := EditorClient.Top;
    Rect.Right := GutterWidth;
    Rect.Bottom := Rect.Top+EditorClient.Height;
    FillRect(Rect);
    Pen.Width := 1;
    Pen.Color := Color;
    MoveTo(GutterWidth - 2, EditorClient.Top);
    LineTo(GutterWidth - 2, EditorClient.Top + EditorClient.Height);
    Pen.Width := 2;
    MoveTo(GutterWidth + 1, EditorClient.Top);
    LineTo(GutterWidth + 1, EditorClient.Top + EditorClient.Height);
    Pen.Width := 1;
    Pen.Color := clGray;
    MoveTo(GutterWidth - 1, EditorClient.Top);
    LineTo(GutterWidth - 1, EditorClient.Top + EditorClient.Height);
  end;
  with FMemoEx do
    GutterPaint(Canvas, Rect);
end;




constructor TCustomMemoEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents {, csOpaque}, csDoubleClicks, csReplicatable];
  FInsertMode := true;
  FReadOnly := false;
  FWantTabs := true;
  FLines := TEditorStrings.Create;
  FLines.FMemoEx := Self;
  FKeyboard := TKeyboard.Create;
  FRows := 1;
  FCols := 1;

  {$IFDEF MEMOEX_UNDO}
  FUndoBuffer := TUndoBuffer.Create;
  FUndoBuffer.FMemoEx := Self;
  FGroupUndo := true;
  {$ENDIF MEMOEX_UNDO}

  FDrawBitmap := TBitmap.Create;

  FFont := TFont.Create;
  with FFont do
  begin
    Name := 'Courier New';
    Size := 10;
    Pitch := fpFixed;
    OnChange := FontChanged;
  end;

  FRightMarginVisible := true;
  FRightMargin := 80;
  FBorderStyle := bsSingle;
  Ctl3d := true;
  Height := 40;
  Width := 150;
  ParentColor := false;
  Cursor := crIBeam;
  TabStop := true;
  FTabStops := '8';
  FSmartTab := true;
  FBackSpaceUnindents := true;
  FAutoIndent := true;
  FKeepTrailingBlanks := false;
  FCursorBeyondEOF := false;
  FCursorBeyondEOL := true;

  FWordWrap := true;

  FScrollBars := ssBoth;
  scbHorz := TRAControlScrollBar95.Create;
  scbVert := TRAControlScrollBar95.Create;
  scbVert.Kind := sbVertical;
  scbHorz.OnScroll := ScrollBarScroll;
  scbVert.OnScroll := ScrollBarScroll;

  Color := clWindow;
  FGutterColor := clBtnFace;
  FclSelectBC := clHighLight;
  FclSelectFC := clHighLightText;
  FRightMarginColor := clSilver;

  EditorClient := TEditorClient.Create;
  EditorClient.FMemoEx := Self;
  FGutter := TGutter.Create;
  FGutter.FMemoEx := Self;

  FLeftCol := 0;
  FTopRow := 0;
  FSelected := false;
  FCaretX := 0;
  FCaretY := 0;

  timerScroll := TTimer.Create(Self);
  timerScroll.Enabled := false;
  timerScroll.Interval := 100;
  timerScroll.OnTimer := ScrollTimer;

  SelAttrs_Size := 0;

  FTabPos := nil;
  SetMax_X(512);

  mouse_down := false;
  double_clicked := false;
  mouse_dragged := false;

  {$IFDEF MEMOEX_EDITOR}

  {$IFDEF MEMOEX_DEFLAYOUT}
  FKeyboard.SetDefLayout;
  {$ENDIF MEMOEX_DEFLAYOUT}

  {$IFDEF MEMOEX_COMPLETION}
  FCompletion := TCompletion.Create2(Self);
  {$ENDIF MEMOEX_COMPLETION}

  {$ENDIF MEMOEX_EDITOR}
end;

destructor TCustomMemoEx.Destroy;
begin
  FLines.Free;
  scbHorz.Free;
  scbVert.Free;
  EditorClient.Free;
  FKeyboard.Free;
  {$IFDEF MEMOEX_EDITOR}
  {$IFDEF MEMOEX_COMPLETION}
  FCompletion.Free;
  {$ENDIF MEMOEX_COMPLETION}
  {$ENDIF MEMOEX_EDITOR}
  FGutter.Free;
  FDrawBitmap.Free;
  FFont.Free;
  SelAttrs := nil;
  FTabPos := nil;
  {$IFDEF MEMOEX_UNDO}
  FreeAndNil(FUndoBuffer);
  {$ENDIF MEMOEX_UNDO}
  inherited Destroy;
end;

procedure TCustomMemoEx.Invalidate;
begin
  if (csLoading in ComponentState) then exit;
  if FUpdateLock = 0 then inherited;
end;

procedure TCustomMemoEx.Loaded;
begin
  inherited Loaded;
  scbVertWidth := GetSystemMetrics(SM_CXVSCROLL);
  scbHorzHeight := GetSystemMetrics(SM_CYHSCROLL);
  NextClipViewer := SetClipboardViewer(Handle);
  UpdateEditorSize;
  Changed;
  SelectionChanged;
  ClipboardChanged;
  FModified := false;
  {$IFDEF MEMOEX_COMPLETION}
  FCompletion.UpdateAutoChange;
  {$ENDIF}
end;

procedure TCustomMemoEx.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array [TBorderStyle] of cardinal = (0, WS_BORDER);
  ScrollStyles: array [TScrollStyle] of cardinal = (0, WS_HSCROLL, WS_VSCROLL,
                                                      WS_HSCROLL or WS_VSCROLL);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle] or ScrollStyles[FScrollBars];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TCustomMemoEx.Resize;
begin
  if not (csLoading in ComponentState) then
  begin
    UpdateEditorSize(true, false);
    Invalidate;
  end;
end;

procedure TCustomMemoEx.CreateWnd;
begin
  inherited CreateWnd;
  if FScrollBars in [ssHorizontal, ssBoth] then scbHorz.Handle := Handle;
  if FScrollBars in [ssVertical, ssBoth] then scbVert.Handle := Handle;
  FAllRepaint := true;
end;

procedure TCustomMemoEx.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCustomMemoEx.PaintSelection;
var
  iR: integer;
begin
  for iR := FUpdateSelBegY to FUpdateSelEndY do PaintLine(iR, -1, -1);
end;

procedure TCustomMemoEx.SetUnSelected;
begin
  if FSelected then
  begin
    FSelectedText := false;
    {$IFDEF MEMOEX_UNDO}
    TUnselectUndo.Create(Self, FCaretX, FCaretY, FSelBlock, FSelBegX, FSelBegY,
      FSelEndX, FSelEndY);
    {$ENDIF MEMOEX_UNDO}
    PaintSelection;
  end;
end;

function IsRectEmpty(R: TRect): boolean;
begin
  Result := (R.Top = R.Bottom) and (R.Left = R.Right);
end;

function TCustomMemoEx.CalcCellRect(const X, Y: integer): TRect;
begin
  Result := Bounds(
    EditorClient.Left + X * FCellRect.Width + 1,
    EditorClient.Top + Y * FCellRect.Height,
    FCellRect.Width,
    FCellRect.Height)
end;

procedure TCustomMemoEx.Paint;
var iR: integer;
    ECR: TRect;
    BX, EX, BY, EY: integer;
begin
  if FUpdateLock > 0 then exit;
  {$IFDEF MEMOEX_NOOPTIMIZE}
  FAllRepaint := true;
  {$ENDIF}
  PaintCaret(false);
  ECR := EditorClient.Canvas.ClipRect;
  OffsetRect(ECR, -FGutterWidth, 0);
  if FAllRepaint then
    ECR := EditorClient.BoundsRect;
  BX := ECR.Left div FCellRect.Width - 1;
  EX := ECR.Right div FCellRect.Width + 1;
  BY := ECR.Top div FCellRect.Height;
  EY := ECR.Bottom div FCellRect.Height + 1;
  for iR := BY to EY do
    PaintLine(FTopRow + iR, FLeftCol + BX, FLeftCol + EX);
  PaintCaret(true);
  FGutter.Paint;
  FAllRepaint := false;
end;

procedure TCustomMemoEx.BeginUpdate;
begin
  if self=nil then exit;
  inc(FUpdateLock);
end;

procedure TCustomMemoEx.EndUpdate;
begin
  if self=nil then exit;
  if FUpdateLock = 0 then Exit; { Error ? }
  dec(FUpdateLock);
  if FUpdateLock = 0 then
  begin
    FAllRepaint := true;
    UpdateEditorSize(false);
    Changed;
    StatusChanged;
    Invalidate;
  end;
end;

{ FTabPos... }
procedure TCustomMemoEx.SetMax_X(const Value: integer);
begin
  Max_X := Value;
  SetLength(FTabPos, Max_X);
end;

{
  FullUpdate
}
procedure TCustomMemoEx.UpdateEditorSize(const FullUpdate: boolean = true; const RepaintGutter: boolean = true);
const
  BiggestSymbol = 'W';
var Size: TSize;
begin
  if (csLoading in ComponentState) then exit;
  if FullUpdate then
  begin
    EditorClient.Canvas.Font := Font;
    EditorClient.Canvas.Font.Style := [fsBold, fsItalic];
    Size := EditorClient.Canvas.TextExtent(BiggestSymbol);
    FCellRect.Width := Max(1, Size.cx);
    FCellRect.Height := Max(1, Size.cy);
    EditorClient.Canvas.Font := Font;
    FDrawBitmap.Canvas.Font.Assign(Font);
    FDrawBitmap.Canvas.Brush.Assign(EditorClient.Canvas.Brush);
    FDrawBitmap.Width := Width;
    FDrawBitmap.Height := FCellRect.Height;
  end;
  FVisibleColCount := Trunc(EditorClient.ClientWidth / FCellRect.Width);
  FVisibleRowCount := Trunc(EditorClient.ClientHeight / FCellRect.Height);
  FLastVisibleCol := FLeftCol + FVisibleColCount - 1;
  FLastVisibleRow := FTopRow + FVisibleRowCount - 1;
  FCols := -1;
  FRows := -1;
  Rows := FLines.ParaLineCount;
  if FWordWrap then
    Cols := FRightMargin else
    Cols := Max_X;
  if RepaintGutter then
    FGutter.Invalidate;
end;

procedure TCustomMemoEx.PaintLine(const Line: integer; ColBeg, ColEnd: integer);
var Ch: string;
  R: TRect;
  F, k, x, i, j, iC, jC, SL, MX, PX, PY: integer;
  T, S: string;
  FAttrs: TLineAttrs;
  LA, LB: TLineAttr;
begin
  if (Line < FTopRow) or (Line > FTopRow + FVisibleRowCount) or (FUpdateLock > 0) then
    exit;
  if ColBeg < FLeftCol then
    ColBeg := FLeftCol;
  if (ColEnd < 0) or (ColEnd > FLeftCol + FVisibleColCount) then
    ColEnd := FLeftCol + FVisibleColCount;
  ColEnd := Min(ColEnd, Max_X - 1);
  j := 0;
  i := ColBeg;
  if (Line > -1) and (Line < FLines.ParaLineCount) then
    with FDrawBitmap do begin
      T := FLines.GetParagraphByIndex(Line, PY, PX);
      S := FLines.ParaStrings[Line];
      if not FWordWrap then begin
        iC := ColBeg;
        jC := ColEnd;
      end else begin
        iC := 0;
        jC := length(T);
      end;
      GetLineAttr(PY, Line, PX, length(S), iC, jC, T, FAttrs);
      Canvas.Brush.Color := Color;
      Canvas.FillRect(Bounds(EditorClient.Left, 0, 1, FCellRect.Height));

      SL := Length(S);
      if SL > ColEnd then
        MX := ColEnd else
        MX := SL;

      if FStripInvisible and FReadOnly and (ColBeg > 0) then begin
        x := PX + ColBeg;
        if x >= SelAttrs_Size then
          x := SelAttrs_Size - 1;
        for k := PX to x do
          if FAttrs[k].FC = FAttrs[k].BC then
            inc(j);
      end;

      while i < MX do
        with Canvas do begin
          iC := i + 1;
          jC := iC + 1;
          if iC <= SL then
            Ch := S[iC] else
            Ch := ' ';
          if (iC + PX > SelAttrs_Size) or (jC + PX > SelAttrs_Size) then break;
          LA := FAttrs[iC + PX - 1];
          if SelAttrs[iC + PX - 1] then
            with LA do begin
              FC := FclSelectFC;
              BC := FclSelectBC;
            end;
          while (jC <= MX) and (jC + PX <= SelAttrs_Size) do begin
            // append chars with same attrs into Ch
            LB := FAttrs[jC + PX - 1];
            if SelAttrs[jC + PX - 1] then
              with LB do begin
                FC := FclSelectFC;
                BC := FclSelectBC;
              end;
            if (LA.FC=LB.FC) and (LA.BC=LB.BC) and
               (LA.LastInteger=LB.LastInteger) then begin
              if jC <= SL then
                Ch := Ch + S[jC] else
                Ch := Ch + ' ';
              inc(jC);
            end
              else break;
          end;
          if (not ((LA.BC = LA.FC) and FStripInvisible)) or not ReadOnly then begin
            // draw Ch into bitmap
            Brush.Color := LA.BC;
            Font.Color := LA.FC;
            Font.Style := LA.Style;
            R.Left := EditorClient.Left + (i-FLeftCol-j)*FCellRect.Width + 1;
            R.Top := 0;
            R.Right := R.Left+FCellRect.Width * Length(Ch);
            R.Bottom := FCellRect.Height;
            FillRect(R);
            TextOut(R.Left, 0, Ch);
          end
            else inc(j, length(Ch));
          i := jC - 1;
        end;
    end else begin
      FDrawBitmap.Canvas.Brush.Color := Color;
      FDrawBitmap.Canvas.FillRect(Bounds(EditorClient.Left, 0, 1, FCellRect.Height));
    end;

  R := Bounds(CalcCellRect(i - j - FLeftCol, Line - FTopRow).Left,
              0, (FLeftCol + FVisibleColCount - i{ - j} + 10) * FCellRect.Width,
              FCellRect.Height);
  FDrawBitmap.Canvas.Brush.Color := Color;
  FDrawBitmap.Canvas.FillRect(R);

  R := Bounds(EditorClient.Left, (Line - FTopRow) * FCellRect.Height,
              (FVisibleColCount + 2) * FCellRect.Width, FCellRect.Height);

  if FRightMarginVisible and (FRightMargin > FLeftCol) and
     (FRightMargin < FLastVisibleCol + 3) then
    with FDrawBitmap.Canvas do begin
      Pen.Color := FRightMarginColor;
      F := CalcCellRect(FRightMargin - FLeftCol, 0).Left;
      MoveTo(F, 0);
      LineTo(F, FCellRect.Height);
    end;

  BitBlt(EditorClient.Canvas.Handle, R.Left, R.Top, R.Right - R.Left,
          FCellRect.Height, FDrawBitmap.Canvas.Handle, R.Left, 0, SRCCOPY );
end;


procedure TCustomMemoEx.GetLineAttr(Line, LineIdx, LineOffs, LineLen, ColBeg, ColEnd: integer;
   const ALine: string; var FAttrs: TLineAttrs);
procedure SetAttrs(A: pLineAttr; count: integer);
var i: integer;
begin
  assert(sizeof(TLineAttr)=12);
  for i := 1 to Count do begin
    A^.FC := clWindowText;
    A^.BC := clWindow;
    A^.LastInteger := 0;
    inc(A);
  end;
end;
procedure ChangeSelectedAttr;
  procedure DoChange(const iBeg, iEnd: integer);
  var i: integer;
  begin
    if (iBeg + LineOffs < SelAttrs_Size) and (iEnd + LineOffs < SelAttrs_Size) then
      for i := iBeg + LineOffs to iEnd + LineOffs do
        SelAttrs[i] := true;
  end;
begin
  if SelAttrs_Size > 0 then
     FillChar(SelAttrs[0], SelAttrs_Size, 0);
  if not FSelected then exit;
  if (LineIdx = FSelBegY) and (LineIdx = FSelEndY) then
    DoChange(FSelBegX, Min(LineLen - 1, FSelEndX - 1 + integer(FInclusive))) else
    begin
      if LineIdx = FSelBegY then
        DoChange(FSelBegX, LineLen - 1);
      if (LineIdx > FSelBegY) and (LineIdx < FSelEndY) then
        DoChange(0, LineLen - 1);
      if LineIdx = FSelEndY then
        DoChange(0, Min(LineLen - 1, FSelEndX - 1 + integer(FInclusive)));
    end
end;
begin
  if SelAttrs_Size <> length(ALine) + 1 then begin
    SelAttrs_Size := length(ALine) + 1;
    SetLength(SelAttrs, SelAttrs_Size);
  end;
  ChangeSelectedAttr;

  SetLength(FAttrs, SelAttrs_Size);
  SetAttrs(@Fattrs[0],SelAttrs_Size);

  if (ALine <> '') and Assigned(FOnGetLineAttr) then
    FOnGetLineAttr(Self, ALine, Line, SelAttrs, FAttrs);
end;



procedure TCustomMemoEx.ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: integer);
begin
  case ScrollCode of
    scLineUp..scPageDown, scTrack:
      if Sender = scbVert then
        Scroll(true, ScrollPos) else
      if Sender = scbHorz then
        Scroll(false, ScrollPos);
  end;
end;

procedure TCustomMemoEx.Scroll(const Vert: boolean; const ScrollPos: integer);
var
  R, RClip, RUpdate: TRect;
  OldFTopRow: integer;
begin
  if FUpdateLock = 0 then
  begin
    PaintCaret(false);
    if Vert then
    begin {Vertical Scroll}
      OldFTopRow := FTopRow;
      FTopRow := ScrollPos;
      if Abs((OldFTopRow - ScrollPos) * FCellRect.Height) < EditorClient.Height then
      begin
        R := EditorClient.ClientRect;
        R.Bottom := R.Top + CellRect.Height * (FVisibleRowCount + 1); {??}
        RClip := R;
        ScrollDC(
          EditorClient.Canvas.Handle,                   // handle of device context
          0,                                            // horizontal scroll units
          (OldFTopRow - ScrollPos) * FCellRect.Height,  // vertical scroll units
          R,                                            // address of structure for scrolling rectangle
          RClip,                                        // address of structure for clipping rectangle
          0,                                            // handle of scrolling region
          @RUpdate                                      // address of structure for update rectangle
          );
        InvalidateRect(Handle, @RUpdate, false);
        if Assigned(OnPaintGutter) then
          Gutter.Paint;
      end
        else Invalidate;
      Update;
    end
      else {Horizontal Scroll}
    begin
      FLeftCol := ScrollPos;
      Invalidate;
    end;
  end
    else { FUpdateLock > 0 }
  begin
    if Vert then FTopRow := ScrollPos
      else FLeftCol := ScrollPos;
  end;
  FLastVisibleRow := FTopRow + FVisibleRowCount - 1;
  FLastVisibleCol := FLeftCol + FVisibleColCount - 1;
  if FUpdateLock = 0 then
  begin
//    DrawRightMargin;
    PaintCaret(true);
  end;
  if Assigned(FOnScroll) then
    FOnScroll(Self);
end;



procedure TCustomMemoEx.PaintCaret(bShow: boolean);
begin
  if not bShow then
    HideCaret(Handle) else
  if Focused then
  begin
    with CalcCellRect(FCaretX - FLeftCol, FCaretY - FTopRow) do
      SetCaretPos(Left - 1, Top + 1);
    ShowCaret(Handle)
  end;
end;

procedure TCustomMemoEx.SetCaretInternal(X, Y: integer);
begin
  if (X = FCaretX) and (Y = FCaretY) then
    exit;
  if not FCursorBeyondEOF then
    Y := Min(Y, FLines.ParaLineCount - 1);
  Y := Max(Y, 0);
  X := Min(X, Max_X);
  X := Max(X, 0);
  if Y < FTopRow then
    SetLeftTop(FLeftCol, Y) else
  if Y > Max(FLastVisibleRow, 0) then
    SetLeftTop(FLeftCol, Y - FVisibleRowCount + 1);
  if X < 0 then
    X := 0;
  if X < FLeftCol then
    SetLeftTop(X, FTopRow) else
  if X > FLastVisibleCol then
    SetLeftTop(X - FVisibleColCount {+ 1}, FTopRow);
  with CalcCellRect(X - FLeftCol, Y - FTopRow) do
    SetCaretPos(Left - 1, Top + 1);
  if (FCaretX <> X) or (FCaretY <> Y) then
  begin
    FCaretX := X;
    FCaretY := Y;
    if Assigned(OnSetCaretPos) then
      OnSetCaretPos(Self, X, Y);
    StatusChanged;
  end;
  FCaretX := X;
  FCaretY := Y;
end;

procedure TCustomMemoEx.SetCaret(const X, Y: integer);
begin
  if (X = FCaretX) and (Y = FCaretY) then
    exit;
  {$IFDEF MEMOEX_UNDO}
  TCaretUndo.Create(Self, FCaretX, FCaretY);
  {$ENDIF MEMOEX_UNDO}
  SetCaretInternal(X, Y);
  if FUpdateLock = 0 then
    StatusChanged;
end;

procedure TCustomMemoEx.SetCaretAtParaPos(const ParaIndex,IndexOffs: integer);
var X,Y: integer;
begin
  Lines.Paragraph2Caret(ParaIndex,IndexOffs,X,Y);
  SetCaret(X,Y);
end;

procedure TCustomMemoEx.SetCaretPosition(const index, Pos: integer);
begin
  if index = 0 then
    SetCaret(Pos, FCaretY) else
    SetCaret(FCaretX, Pos)
end;



procedure TCustomMemoEx.KeyDown(var Key: Word; Shift: TShiftState);
var
  Form: TCustomForm;
{$IFDEF MEMOEX_EDITOR}
  Com: word;
{$ENDIF MEMOEX_EDITOR}
begin
  {$IFDEF MEMOEX_COMPLETION}
  if FCompletion.FVisible then
    if FCompletion.DoKeyDown(Key, Shift) then exit
      else
  else
    FCompletion.FTimer.Enabled := false;
  {$ENDIF MEMOEX_COMPLETION}

  if (Key = VK_TAB) and ((Shift = []) or (Shift = [ssShift])) then
    if ((FReadOnly) or (not FWantTabs)) then
    begin
      Form := GetParentForm(Self);
      if Assigned(Form) then
      begin
        Key := 0;
        if Shift = [] then Form.Perform(WM_NEXTDLGCTL, 0, 0)
          else Form.Perform(WM_NEXTDLGCTL, 1, 0);
      end;
      exit;
    end;

  {$IFDEF MEMOEX_EDITOR}
  if WaitSecondKey then
  begin
    Com := FKeyboard.Command2(Key1, Shift1, Key, Shift);
    WaitSecondKey := false;
    IgnoreKeyPress := true;
  end
    else
  begin
    inherited KeyDown(Key, Shift);
    Key1 := Key;
    Shift1 := Shift;
    Com := FKeyboard.Command(Key, Shift);
    if Com = twoKeyCommand then
    begin
      IgnoreKeyPress := true;
      WaitSecondKey := true;
    end
      else IgnoreKeyPress := Com > 0;
  end;
  if (Com > 0) and (Com <> twoKeyCommand) then
  begin
    Key := 0;
    Shift := [];
    Command(Com);
  end;
  {$IFDEF MEMOEX_COMPLETION}
  if (Com = ecBackSpace) then FCompletion.DoKeyPress(#8);
  {$ENDIF MEMOEX_COMPLETION}
  {$ENDIF MEMOEX_EDITOR}
end;

{$IFDEF MEMOEX_EDITOR}

procedure TCustomMemoEx.ReLine;
begin
  FLines.ReLine;
end; { ReLine }

procedure TCustomMemoEx.KeyPress(var Key: Char);
begin
  if IgnoreKeyPress then
  begin
    IgnoreKeyPress := false;
    exit
  end;
  if FReadOnly then exit;
  PaintCaret(false);
  inherited KeyPress(Key);

  Command(ord(Key));

  PaintCaret(true);
end;

function AutoChangeCompare(Item1, Item2: pointer): integer;
var
  i, j: integer;
begin
  i := length(PAutoChangeWord(Item1)^.OldWord);
  j := length(PAutoChangeWord(Item2)^.OldWord);
  if i = j then Result := 0
  else
    if i > j then Result := 1
    else Result := -1;
end;

procedure TCustomMemoEx.InsertChar(const Key: Char);

  {$IFDEF MEMOEX_COMPLETION}
  function GetAutoChangeWord(const CurrentWord: string; var NewWord: string): boolean;
  var
    i, j, k: integer;
    s, t: string;
  begin
    Result := false;
    t := DoChangeCase(CurrentWord, RA_CASE_CONVERT_LOWER);
    j := length(t);
    for i := 0 to FCompletion.FAutoChangeList.Count - 1 do
    begin
      s := PAutoChangeWord(FCompletion.FAutoChangeList[i])^.OldWord;
      k := length(s);
      if j < k then break
      else
        if j = k then
          if t = s then
          begin
            Result := true;
            NewWord := PAutoChangeWord(FCompletion.FAutoChangeList[i])^.NewWord;
            break;
          end;
    end;
  end;
  {$ENDIF}

var
  S: string;
  {$IFDEF MEMOEX_COMPLETION}
  T, old_str, new_str: string;
  k1, k2, str_pos: integer;
  AutoChanged, AddKeyToNewStr: boolean;
  {$ENDIF}
  oldChar: string;
  i, _X, _Y, Y: integer;
  b: boolean; 
begin
  ReLine;
  case Key of
    #32..#255:
      begin
        {$IFDEF MEMOEX_COMPLETION}
        if not HasChar(Key, RAEditorCompletionChars) then
          FCompletion.DoKeyPress(Key);
        {$ENDIF MEMOEX_COMPLETION}
        begin
          DeleteSelected;
          FLines.Caret2Paragraph(FCaretX, FCaretY, FParaY, FParaX);
          {$IFDEF MEMOEX_COMPLETION}
          new_str := '';
          old_str := '';
          str_pos := 0;
          if (Key in _AutoChangePunctuation) and (FCompletion.FAutoChangeList.Count > 0) then
          begin
            S := FLines[FParaY];
            AutoChanged := false;
            AddKeyToNewStr := false;
            str_pos := FParaX - 1;
            k1 := length(PAutoChangeWord(FCompletion.FAutoChangeList[0])^.OldWord);
            k2 := length(PAutoChangeWord(FCompletion.FAutoChangeList[FCompletion.FAutoChangeList.Count - 1])^.OldWord);
            while (str_pos > -1) and (FParaX - str_pos <= k2) do
            begin
              if FParaX - str_pos >= k1 then
              begin
                old_str := System.Copy(S, str_pos + 1, FParaX - str_pos);
                AutoChanged := GetAutoChangeWord(old_str, new_str); //  Á‡ÏÂÌˇÂÏ ÔÓ‰ÒÚÓÍÛ ·ÂÁ ÁÌ‡Í‡?
                if not AutoChanged then
                  AutoChanged := GetAutoChangeWord(old_str + Key, new_str) //  Á‡ÏÂÌˇÂÏ ÔÓ‰ÒÚÓÍÛ ÒÓ ÁÌ‡ÍÓÏ?
                else AddKeyToNewStr := true;
                if AutoChanged then
                  if ((str_pos > 0) and (S[str_pos] in _AutoChangePunctuation)) or (str_pos = 0) then break
                  else AutoChanged := false;
              end;
              dec(str_pos);
            end;
            if AutoChanged then
              if AddKeyToNewStr then
                if GetAutoChangeWord(Key, T) then new_str := new_str + T
                else new_str := new_str + Key
              else
            else
              begin
                AutoChanged := GetAutoChangeWord(Key, new_str);
                if AutoChanged then
                begin
                  str_pos := FParaX;  //  Á‡ÏÂÌˇÂÏ 1, ÚÓÎ¸ÍÓ ˜ÚÓ ‚‚Â‰ÂÌÌ˚È, ÁÌ‡Í
                  old_str := '';
                end;
              end;
          end
          else AutoChanged := false;
          if AutoChanged then
          begin
            {
              str_pos
              S
              old_str
              new_str
            }

            {$IFDEF MEMOEX_UNDO}
            //  undo
            BeginCompound;
            TCaretUndo.Create(Self, FCaretX, FCaretY);
            FLines.Paragraph2Caret(FParaY, str_pos, _X, _Y);
            if (length(old_str) + integer(not FInsertMode) > 0) and (length(S) > 0) then
            begin
              if FInsertMode then T := old_str
              else T := old_str + S[FParaX + 1];
              TDeleteUndo.Create(Self, _X, _Y, T);
            end;
            if length(new_str) > 0 then TInsertUndo.Create(Self, _X, _Y, new_str);
            EndCompound;
            {$ENDIF}

            k1 := FLines.Paragraphs[FParaY].FCount;
            System.Delete(S, str_pos + 1, length(old_str) + integer(not FInsertMode));
            System.Insert(new_str, S, str_pos + 1);
            FLines.Internal[FParaY] := S;
            B := k1 <> FLines.Paragraphs[FParaY].FCount;
            FParaX := str_pos + length(new_str);
          end
          else
          {$ENDIF MEMOEX_COMPLETION}
            begin
              S := FLines.ParaStrings[FCaretY];
              if FInsertMode then
              begin
                {$IFDEF MEMOEX_UNDO}
                TInsertUndo.Create(Self, FCaretX, FCaretY, Key);
                {$ENDIF MEMOEX_UNDO}
                Insert(Key, S, FCaretX + 1);
              end
              else
                begin
                  if FCaretX + 1 <= Length(S) then
                  begin
                    oldChar := S[FCaretX + 1];
                    S[FCaretX + 1] := Key;
                  end
                  else
                    begin
                      oldChar := '';
                      S := S + Key;
                    end;
                  {$IFDEF MEMOEX_UNDO}
                  TOverwriteUndo.Create(Self, FCaretX, FCaretY, oldChar, Key);
                  {$ENDIF MEMOEX_UNDO}
                end;

              Y := FCaretY;
              i := FLines.Paragraphs[FParaY].FCount;
              FLines.InternalParaStrings[Y] := S;
              inc(FParaX);

              B := i <> FLines.Paragraphs[FParaY].FCount;
            end;

          i := RepaintParagraph(FCaretY);

          if B then
          begin
            UpdateEditorSize(false);
            RedrawFrom(i + 1);
          end;

          FLines.Paragraph2Caret(FParaY, FParaX, _X, _Y);
          SetCaretInternal(_X, _Y);

          Changed;
        end;
        {$IFDEF MEMOEX_COMPLETION}
        if HasChar(Key, RAEditorCompletionChars) then
          FCompletion.DoKeyPress(Key);
        {$ENDIF MEMOEX_COMPLETION}
      end;
  end;
end;

{$ENDIF MEMOEX_EDITOR}

procedure TCustomMemoEx.RedrawFrom(YFrom: integer);
var i: integer;
begin
  for i := YFrom - 1 to FLastVisibleRow + 1 do
    PaintLine(i, -1, -1);
end;

function TCustomMemoEx.RepaintParagraph(LineIndex: integer): integer;
var
  P, PI, i, j, k: integer;
begin
  FLines.Index2ParaIndex(LineIndex, P, PI);
  j := LineIndex - PI;
  k := j + FLines.FList[P].FCount - 1;
  if j < FTopRow - 1 then
    j := FTopRow - 1;
  j := Max(0, j);
  if k > FLastVisibleRow + 1 then
    k := FLastVisibleRow + 1;
  Result := k;
  for i := j to k do
    PaintLine(i, -1, -1);
end;

function TCustomMemoEx.IsUndoEmpty: boolean;
begin
{$IFDEF MEMOEX_EDITOR} Result := FUndoBuffer.FPtr < 0;{$endif}
end;

function TCustomMemoEx.YinBounds(AY: integer): boolean;
begin
  Result := (AY > -1) and (AY < FLines.ParaLineCount);
end;

function TCustomMemoEx.DoChangeCase(const st: string;
  Conversion: byte): string;
begin
  if Assigned(FOnCaseConversion) then
    Result := FOnCaseConversion(Self, Conversion, st) else
    case Conversion of
      RA_CASE_CONVERT_UPPER:
        Result := ANSIUpperCase(st);
      RA_CASE_CONVERT_LOWER:
        Result := ANSILowerCase(st);
      else
        Result := ANSIChangeCase(st);
    end;
end;

{$IFDEF MEMOEX_EDITOR}
type
  EComplete = class(EAbort);

procedure TCustomMemoEx.Command(ACommand: TEditCommand);
var
  X, Y: integer;
  {$IFDEF MEMOEX_UNDO}
  CaretUndo: boolean;
  {$ENDIF MEMOEX_UNDO}

type
  TPr = procedure of object;

  procedure DoAndCorrectXY(Pr: TPr);
  begin
    Pr;
    X := FCaretX;
    Y := FCaretY;
    {$IFDEF MEMOEX_COMPLETION}
    CaretUndo := false;
    {$ENDIF MEMOEX_COMPLETION}
  end;

  function Com(const Args: array of TEditCommand): boolean;
  var
    i: integer;
  begin
    Result := true;
    for i := 0 to High(Args) do
      if Args[i] = ACommand then
        exit;
    Result := false;
  end;

  procedure SetSel1(X, Y: integer);
  begin
    SetSel(X, Y);
    {$IFDEF MEMOEX_UNDO}
    CaretUndo := false;
    {$ENDIF MEMOEX_UNDO}
  end;

  procedure SetSelText1(S: string);
  begin
    SelText := S;
    CaretUndo := false;
  end;

  procedure Complete;
  begin
    raise EComplete.Create('');
  end;

var
  F, _Y: integer;
  S, S2, T: string;
  B: boolean;
  iBeg, iEnd: integer;
begin
  X := FCaretX;
  Y := FCaretY;

  {$IFDEF MEMOEX_UNDO}
  CaretUndo := true;
  {$ENDIF MEMOEX_UNDO}

  PaintCaret(false);

 { macro recording }
  if FRecording and not Com([ecRecordMacro, ecBeginCompound]) and (FCompound = 0) then
    FMacro := FMacro + Char(Lo(ACommand)) + Char(Hi(ACommand));

  try
    try
      case ACommand of

        { caret movements }

        ecLeft, ecRight, ecSelLeft, ecSelRight:
          begin
            if Com([ecSelLeft, ecSelRight]) and not FSelected then SetSel1(X, Y);
            B := Com([ecLeft, ecSelLeft]);
            if B then dec(X) else inc(X);
            if (not FCursorBeyondEOL) and (YinBounds(Y)) then
            begin
              _Y := 0;
              if (B) and (X < 0) then _Y := -1
                else
              if (not B) and (X > length(FLines.ParaStrings[Y])) then _Y := 1;
              if (_Y <> 0) and (YinBounds(Y + _Y)) then
              begin
                Y := Y + _Y;
                if B then X := length(FLines.ParaStrings[Y])
                  else X := 0;
              end
                else
              if X > length(FLines.ParaStrings[Y]) then X := length(FLines.ParaStrings[Y]);
            end
              else if not CursorBeyondEOL then X := 0;
            if Com([ecSelLeft, ecSelRight]) then SetSel1(X, Y)
              else SetUnSelected;
          end;
        ecUp, ecDown, ecSelUp, ecSelDown:
          if (Com([ecUp, ecSelUp]) and (Y > 0)) or (Com([ecDown, ecSelDown]) and ((Y < FRows - 1) or (FCursorBeyondEOF))) then
          begin
            if Com([ecSelUp, ecSelDown]) and not FSelected then SetSel1(X, Y);
            if Com([ecUp, ecSelUp]) then dec(Y)
              else inc(Y);
            if (not FCursorBeyondEOL) and (YinBounds(Y)) then
              if X > length(FLines.ParaStrings[Y]) then X := length(FLines.ParaStrings[Y]);
            if Com([ecSelUp, ecSelDown]) then SetSel1(X, Y)
              else SetUnSelected;
          end;
        ecPrevWord, ecSelPrevWord:
          if FLines.ParaLineCount > 0 then
          begin
            S := FLines.ParaStrings[Y];
            if X > length(S) then X := length(S);
            if X = 0 then
              if Y > 0 then
              begin
                dec(Y);
                X := length(FLines.ParaStrings[Y]);
              end
                else
              else
            begin
              if (ACommand = ecSelPrevWord) and not FSelected then SetSel1(FCaretX, FCaretY);
              B := false;
              for F := X - 1 downto 0 do
                if B then
                  if (S[F + 1] in Separators) then
                  begin
                    X := F + 1;
                    break;
                  end
                    else
                else
                  if not (S[F + 1] in Separators) then B := true;
              if X = FCaretX then X := 0;
              if ACommand = ecSelPrevWord then SetSel1(X, Y)
                else SetUnselected;
              if (not B) and (X = 0) and (Y > 0) then
              begin
                FCaretX := X;
                Command(ACommand);
                Complete;
              end;
            end;
          end;
        ecNextWord, ecSelNextWord:
          if FLines.ParaLineCount > 0 then
          begin
            if X >= length(FLines.ParaStrings[Y]) then
            begin
              if Y < FLines.ParaLineCount - 1 then
              begin
                inc(Y);
                X := 0;
                if length(FLines.ParaStrings[Y]) > 0 then
                  if FLines.ParaStrings[Y][X + 1] = #32 then
                  begin
                    FCaretX := X;
                    FCaretY := Y;
                    Command(ACommand);
                    Complete;
                  end;
              end;
            end
              else
            begin
              if (ACommand = ecSelNextWord) and not FSelected then SetSel1(FCaretX, FCaretY);
              S := FLines.ParaStrings[Y];
              B := false;
              for F := X to Length(S) - 1 do
                if B then
                  if not (S[F + 1] in Separators) then
                  begin
                    X := F;
                    break;
                  end
                    else
                else
                  if (S[F + 1] in Separators) then B := true;
              if X = FCaretX then
              begin
                B := X <> length(S);
                X := length(S);
              end;
              if ACommand = ecSelNextWord then SetSel1(X, Y)
                else SetUnselected;
              if (not B) and (X = length(S)) and (Y < FLines.ParaLineCount - 1) then
              begin
                FCaretX := X;
                Command(ACommand);
                Complete;
              end;
            end;
          end;
        ecScrollLineUp, ecScrollLineDown,
        ecScrollPageUp, ecScrollPageDown:
          begin
            if not ((ACommand = ecScrollLineDown) and (Y >= FLines.ParaLineCount - 1)
              and (Y = FTopRow)) then
            begin
              case ACommand of
                ecScrollLineUp:
                  F := -1;
                ecScrollLineDown:
                  F := 1;
                ecScrollPageUp:
                  F := -scbVert.LargeChange;
                else
                  F := scbVert.LargeChange;
              end;
              scbVert.Position := scbVert.Position + F;
              Scroll(true, scbVert.Position);
            end;
            if Y < FTopRow then
              Y := FTopRow else
            if Y > FLastVisibleRow then
              Y := FLastVisibleRow;
          end;
        ecBeginLine, ecSelBeginLine, ecBeginDoc, ecSelBeginDoc,
          ecEndLine, ecSelEndLine, ecEndDoc, ecSelEndDoc:
          begin
            if Com([ecSelBeginLine, ecSelBeginDoc, ecSelEndLine, ecSelEndDoc]) and not FSelected then
              SetSel1(FCaretX, Y);
            if Com([ecBeginLine, ecSelBeginLine]) then X := 0
              else
            if Com([ecBeginDoc, ecSelBeginDoc]) then
            begin
              X := 0;
              Y := 0;
              SetLeftTop(0, 0);
            end
              else
            if Com([ecEndLine, ecSelEndLine]) then
            begin
              if FLines.ParaLineCount > 0 then X := Length(FLines.ParaStrings[Y])
                else X := 0;
            end
              else
            if Com([ecEndDoc, ecSelEndDoc]) then
              if FLines.ParaLineCount > 0 then
              begin
                Y := FLines.ParaLineCount - 1;
                X := Length(FLines.ParaStrings[Y]);
                SetLeftTop(X - FVisibleColCount, Y - FVisibleRowCount + 1{ div 2});
              end;
            if Com([ecSelBeginLine, ecSelBeginDoc, ecSelEndLine, ecSelEndDoc]) then SetSel1(X, Y)
              else SetUnSelected;
          end;
        ecPrevPage:
          begin
            scbVert.Position := scbVert.Position - scbVert.LargeChange;
            Scroll(true, scbVert.Position);
            Y := Y - FVisibleRowCount;
            SetUnSelected;
          end;
        ecNextPage:
          begin
            scbVert.Position := scbVert.Position + scbVert.LargeChange;
            Scroll(true, scbVert.Position);
            Y := Y + FVisibleRowCount;
            SetUnSelected;
          end;
        ecSelPrevPage:
          begin
            BeginUpdate;
            SetSel1(X, Y);
            scbVert.Position := scbVert.Position - scbVert.LargeChange;
            Scroll(true, scbVert.Position);
            Y := Y - FVisibleRowCount;
            SetSel1(X, Y);
            EndUpdate;
          end;
        ecSelNextPage:
          begin
            BeginUpdate;
            SetSel1(X, Y);
            scbVert.Position := scbVert.Position + scbVert.LargeChange;
            Scroll(true, scbVert.Position);
            Y := Y + FVisibleRowCount;
            if Y <= FLines.ParaLineCount - 1 then SetSel1(X, Y)
              else SetSel1(X, FLines.ParaLineCount - 1);
            EndUpdate;
          end;
        ecSelWord:
          if not FSelected and (GetWordOnPosEx(FLines.ParaStrings[Y] + ' ', X + 1, iBeg, iEnd) <> '') then
          begin
            SetSel1(iBeg - 1, Y);
            SetSel1(iEnd - 1, Y);
            X := iEnd - 1;
          end;

        ecWindowTop:
          begin
            Y := FTopRow;
            if (not FCursorBeyondEOL) and (YinBounds(Y)) then
              if X > length(FLines.ParaStrings[Y]) then
                X := length(FLines.ParaStrings[Y]);
            SetUnSelected;
          end;
        ecWindowBottom:
          begin
            Y := FTopRow + FVisibleRowCount - 1;
            if (not FCursorBeyondEOL) and (YinBounds(Y)) then
              if X > length(FLines.ParaStrings[Y]) then
                X := length(FLines.ParaStrings[Y]);
            SetUnSelected;
          end;

        { editing }

        {$IFDEF MEMOEX_EDITOR}
        ecCharFirst..ecCharLast:
          if not FReadOnly then
          begin
            InsertChar(Char(ACommand - ecCharFirst));
//            Changed; // AB
            Complete;
          end;
        ecInsertPara:
          if not FReadOnly then
          begin
            DeleteSelected;
            ReLine;
            FLines.Caret2Paragraph(X, Y, FParaY, FParaX);
            S := FLines[FParaY];
            S2 := Copy(S, FParaX + 1, length(S));
            T := S2;
            if Assigned(FOnBreakLine) then FOnBreakLine(Self, S, S2);
            if S2 = T then
            begin
              {$IFDEF MEMOEX_UNDO}
              BeginCompound;
              TInsertUndo.Create(Self, FCaretX, FCaretY, #13#10);
              CaretUndo := false;
              {$ENDIF MEMOEX_UNDO}

              if FAutoIndent then F := length(S2) - length(TrimLeft(S2));
              FLines.Insert(FParaY + 1, S2);
              FLines.Internal[FParaY] := Copy(S, 1, FParaX);
              inc(Y);
              { smart tab }
              if (FAutoIndent) and (Trim(FLines.ParaStrings[FCaretY]) <> '')
              { (FLines.ParaStrings[FCaretY][1] = ' ')) or
                ((Trim(FLines.ParaStrings[FCaretY]) = '') and (X > 0)))} then
              begin
                X := GetTabStop(0, Y, tsAutoIndent, true);
                if X > F then
                begin
                  {$IFDEF MEMOEX_UNDO}
                  TInsertUndo.Create(Self, 0, Y, StringOfChar(' ',X - F));
                  {$ENDIF MEMOEX_UNDO}
                  FLines.Internal[FParaY + 1] := StringOfChar(' ',X - F) + S2;
                end;
              end
                else
              if (FAutoIndent) and (S2 = '') then X := length(FLines.ParaStrings[FCaretY])
                else X := 0;
              {$IFDEF MEMOEX_UNDO}
              EndCompound;
              {$ENDIF MEMOEX_UNDO}
            end
              else
            begin
              T := Copy(S, 1, FParaX) + #13#10 + S2 + #13#10;
              F := FLines.GetParaOffs(FParaY);
              S2 := FLines.Text;
              System.Delete(S2, F + 1, length(S) + 2);
              System.Insert(T, S2, F + 1);

              FLines.Paragraph2Caret(FParaY, 0, F, _Y);

              {$IFDEF MEMOEX_UNDO}
              CaretUndo := false;
              BeginCompound;
              TCaretUndo.Create(Self, FCaretX, FCaretY);
              TDeleteUndo.Create(Self, 0, _Y, S + #13#10);
              TInsertUndo.Create(Self, 0, _Y, T);
              EndCompound;
              {$ENDIF MEMOEX_UNDO}

              FLines.SetLockText(S2);
              inc(Y);
              X := 0;
            end;
            UpdateEditorSize(false);
            { Invalidate }
            F := RepaintParagraph(FCaretY);
            RedrawFrom(F + 1);
            Changed;
          end;
        ecBackword:
          if not FReadOnly then
          begin
            if length(FLines.ParaStrings[Y]) > 0 then
            begin
              Command(ecBeginCompound);
              Command(ecBeginUpdate);
              Command(ecSelPrevWord);
              Command(ecDeleteSelected);
              Command(ecEndUpdate);
              Command(ecEndCompound);
            end
              else Command(ecBackspace);
            Complete;
          end;
        ecBackspace:
          if not FReadOnly then
          begin
            if FSelected then begin
              DoAndCorrectXY(DeleteSelected);
              Changed;
            end
            else begin
              ReLine;
              FLines.Caret2Paragraph(X, Y, FParaY, FParaX);
              if X > 0 then
              begin
                if FBackSpaceUnindents then
                  X := GetBackStop(FCaretX, FCaretY) else
                  X := FCaretX - 1;
                S := Copy(FLines.ParaStrings[FCaretY], X + 1, FCaretX - X);
                dec(FParaX, length(S));
                F := FLines.Paragraphs[FParaY].FCount;
                FLines.InternalParaStrings[Y] := Copy(FLines.ParaStrings[Y], 1, X) +
                     Copy(FLines.ParaStrings[Y], FCaretX + 1, Length(FLines.ParaStrings[Y]));
                FLines.Paragraph2Caret(FParaY, FParaX, X, Y);
                {$IFDEF MEMOEX_UNDO}
                TBackspaceUndo.Create(Self, X, Y, S);
                CaretUndo := false;
                {$ENDIF MEMOEX_UNDO}
                B := F <> FLines.Paragraphs[FParaY].FCount;
                F := RepaintParagraph(Y);
                if B then
                begin
                  UpdateEditorSize(false);
                  RedrawFrom(F + 1);
                end;
                Changed;
              end
                else
              if Y > 0 then
              begin
                if FParaX > 0 then
                begin
                  T := FLines[FParaY];
                  S := Copy(T, FParaX, 1);

                  System.Delete(T, FParaX, 1);
                  FLines.Internal[FParaY] := T;
                  dec(FParaX);
                  FLines.Paragraph2Caret(FParaY, FParaX, X, Y);

                  {$IFDEF MEMOEX_UNDO}
                  TBackspaceUndo.Create(Self, X, Y, S);
                  CaretUndo := false;
                  {$ENDIF MEMOEX_UNDO}
                end
                else
                  if FParaY > 0 then begin
                    inc(FUpdateLock);
                    S := FLines[FParaY - 1];
                    S2 := FLines[FParaY];
                    if Assigned(FOnConcatLine) then FOnConcatLine(Self, S, S2);
                    {$IFDEF MEMOEX_UNDO}
                    CaretUndo := false;
                    FLines.Paragraph2Caret(FParaY - 1, 0, F, _Y);
                    BeginCompound;
                    TCaretUndo.Create(Self, X, Y);
                    TDeleteUndo.Create(Self, 0, _Y, FLines[FParaY - 1] + #13#10 + FLines[FParaY] + #13#10);
                    TInsertUndo.Create(Self, 0, _Y, S + S2 + #13#10);
                    EndCompound;
                    {$ENDIF MEMOEX_UNDO}
                    FLines.Internal[FParaY - 1] := S + S2;
                    FLines.Delete(FParaY);
                    dec(FUpdateLock);
                    FLines.Paragraph2Caret(FParaY - 1, length(S), X, Y);
                  end
                  else Complete;
                UpdateEditorSize(false);
                F := RepaintParagraph(Y);
                RedrawFrom(F + 1);
                Changed;
              end;
            end;
          end;
        ecDelete:
          if not FReadOnly then
          begin
            if FLines.ParaLineCount = 0 then FLines.Add('');
            if FSelected then
            begin
              DoAndCorrectXY(DeleteSelected);
              Changed;
            end
              else
            begin
              ReLine;
              FLines.Caret2Paragraph(X, Y, FParaY, FParaX);
              if X < Length(FLines.ParaStrings[Y]) then
              begin
                {$IFDEF MEMOEX_UNDO}
                TDeleteUndo.Create(Self, FCaretX, FCaretY, FLines.ParaStrings[Y] [X + 1]);
                CaretUndo := false;
                {$ENDIF MEMOEX_UNDO}

                F := FLines.Paragraphs[FParaY].FCount;

                FLines.InternalParaStrings[Y] := Copy(FLines.ParaStrings[Y], 1, X) +
                  Copy(FLines.ParaStrings[Y], X + 2, Length(FLines.ParaStrings[Y]));
                FLines.Paragraph2Caret(FParaY, FParaX, X, Y);

                B := F <> FLines.Paragraphs[FParaY].FCount;
                F := RepaintParagraph(Y);
                if B then
                begin
                  UpdateEditorSize(false);
                  RedrawFrom(F + 1);
                end;

                Changed;
              end
                else
              if (Y >= 0) and (Y <= FLines.ParaLineCount - 2) then
              begin
                S := FLines[FParaY];
                if FParaX < length(S) then
                begin
                  {$IFDEF MEMOEX_UNDO}
                  TDeleteUndo.Create(Self, FCaretX, FCaretY, System.Copy(S, FParaX + 1, 1));
                  CaretUndo := false;
                  {$ENDIF MEMOEX_UNDO}

                  System.Delete(S, FParaX + 1, 1);
                  FLines.Internal[FParaY] := S;
                  FLines.Paragraph2Caret(FParaY, FParaX, X, Y);
                end
                  else
                begin
                  inc(FUpdateLock);

                  S := FLines[FParaY];
                  S2 := FLines[FParaY + 1];
                  if Assigned(FOnConcatLine) then FOnConcatLine(Self, S, S2);

                  {$IFDEF MEMOEX_UNDO}
                  CaretUndo := false;
                  FLines.Paragraph2Caret(FParaY, 0, F, _Y);

                  BeginCompound;
                  TCaretUndo.Create(Self, X, Y);
                  TDeleteUndo.Create(Self, 0, _Y, FLines[FParaY] + #13#10 + FLines[FParaY + 1] + #13#10);
                  TInsertUndo.Create(Self, 0, _Y, S + S2 + #13#10);
                  EndCompound;
                  {$ENDIF MEMOEX_UNDO}

                  FLines.Internal[FParaY] := S + S2;
                  FLines.Delete(FParaY + 1);

                  dec(FUpdateLock);
                  FLines.Paragraph2Caret(FParaY, length(S), X, Y);
                end;

                UpdateEditorSize(false);
                F := RepaintParagraph(FCaretY);
                RedrawFrom(F + 1);

                Changed;
              end;
            end;
          end;
        ecTab, ecBackTab:
          if not FReadOnly then
          begin
            if FSelected then
            begin
              if ACommand = ecTab then PostCommand(ecIndent)
                else PostCommand(ecUnindent);
            end
              else
            begin
              ReLine;
              X := GetTabStop(FCaretX, FCaretY, tsTabStop, ACommand = ecTab);
              if (ACommand = ecTab) and FInsertMode then
              begin
                S := FLines.ParaStrings[FCaretY];
                FLines.Caret2Paragraph(FCaretX, FCaretY, FParaY, FParaX);
                S2 := StringOfChar(' ',X - FCaretX);

                {$IFDEF MEMOEX_UNDO}
                TInsertTabUndo.Create(Self, FCaretX, FCaretY, S2);
                CaretUndo := false;
                {$ENDIF MEMOEX_UNDO}

                Insert(S2, S, FCaretX + 1);

                F := FLines.Paragraphs[FParaY].FCount;

                FLines.InternalParaStrings[FCaretY] := S;
                inc(FParaX, X - FCaretX);
                FLines.Paragraph2Caret(FParaY, FParaX, X, Y);

                B := F <> FLines.Paragraphs[FParaY].FCount;
                F := RepaintParagraph(FCaretY);
                if B then
                begin
                  UpdateEditorSize(false);
                  RedrawFrom(F + 1);
                end;

                Changed;
              end
                else    // ????
            end;
          end;
        ecIndent:
          if not FReadOnly and FSelected and (FSelBegY <> FSelEndY) and
            (FSelBegX = 0) and (FSelEndX = 0) then
          begin
            F := FindNotBlankCharPos(FLines.ParaStrings[FCaretY]);
            S2 := StringOfChar(' ',GetDefTabStop(F, true) - FCaretX);
            S := SelText;
            S := StringReplaceAll(S, #13#10, #13#10 + S2);
            Delete(S, Length(S) - Length(S2) + 1, Length(S2));
            SetSelText1(S2 + S)
          end;
        ecUnIndent:
          if not FReadOnly and FSelected and (FSelBegY <> FSelEndY) and
            (FSelBegX = 0) and (FSelEndX = 0) then
          begin
            F := FindNotBlankCharPos(FLines.ParaStrings[FCaretY]);
            S2 := StringOfChar(' ',GetDefTabStop(F, true) - FCaretX);
            S := SelText;
            S := StringReplaceAll(S, #13#10 + S2, #13#10);
            for iBeg := 1 to Length(S2) do
              if S[1] = ' ' then
                Delete(S, 1, 1)
              else
                Break;
            SetSelText1(S);
          end;
        ecChangeInsertMode:
          begin
            FInsertMode := not FInsertMode;
            StatusChanged;
          end;
        ecClipBoardCut:
          if not FReadOnly then DoAndCorrectXY(ClipBoardCut);
        {$ENDIF MEMOEX_EDITOR}
        ecClipBoardCopy:
          ClipBoardCopy;
        {$IFDEF MEMOEX_EDITOR}
        ecClipBoardPaste:
          if not FReadOnly then DoAndCorrectXY(ClipBoardPaste);
        ecDeleteSelected:
          if not FReadOnly and FSelected then
            DoAndCorrectXY(DeleteSelected);
        ecDeleteWord:
          if not FReadOnly then
          begin
            if length(FLines.ParaStrings[Y]) = 0 then
              Command(ecDelete) else begin
              Command(ecBeginCompound);
              Command(ecBeginUpdate);
              Command(ecSelNextWord);
              Command(ecDeleteSelected);
              Command(ecEndUpdate);
              Command(ecEndCompound);
              Complete;
            end;
          end;
        ecDeleteLine:
          if (not FReadOnly) and (Y >= 0) and (Y <= FLines.ParaLineCount - 1) then
          begin
            FLines.Index2ParaIndex(Y, F, _Y);
            B := (not FWordWrap) or (_Y = FLines.Paragraphs[F].FCount - 1);
            Command(ecBeginCompound);
            Command(ecBeginUpdate);
            Command(ecBeginLine);
            Command(ecSelEndLine);
            Command(ecDeleteSelected);
            if B then Command(ecDelete);
            Command(ecEndUpdate);
            Command(ecEndCompound);
            Complete;
          end;
        ecSelAll:
          begin
            Command(ecBeginCompound);
            Command(ecBeginUpdate);
            Command(ecBeginDoc);
            Command(ecSelEndDoc);
            Command(ecEndUpdate);
            Command(ecEndCompound);
            Complete;
          end;
        ecToUpperCase:
          if (not FReadOnly) and (FSelected) then
            SelText := DoChangeCase(SelText, RA_CASE_CONVERT_UPPER);
        ecToLowerCase:
          if (not FReadOnly) and (FSelected) then
            SelText := DoChangeCase(SelText, RA_CASE_CONVERT_LOWER);
        ecChangeCase:
          if (not FReadOnly) and (FSelected) then
            SelText := DoChangeCase(SelText, RA_CASE_CONVERT_INVERT);
        {$ENDIF MEMOEX_EDITOR}
        {$IFDEF MEMOEX_UNDO}
        ecUndo:
          if not FReadOnly then
          begin
            FUndoBuffer.Undo;
            PaintCaret(true);
            Complete;
          end;
        ecRedo:
          if not FReadOnly then
          begin
            FUndoBuffer.Redo;
            PaintCaret(true);
            Complete;
          end;
        ecBeginCompound:
          BeginCompound;
        ecEndCompound:
          EndCompound;
        {$ENDIF MEMOEX_UNDO}

        ecSetBookmark0..ecSetBookmark9:
          ChangeBookMark(ACommand - ecSetBookmark0, true);
        ecGotoBookmark0..ecGotoBookmark9:
          begin
            ChangeBookMark(ACommand - ecGotoBookmark0, false);
            X := FCaretX;
            Y := FCaretY;
          end;
        ecInsertMacro0..ecInsertMacroZ:
          if (Assigned(FOnInsertMacro)) and (not FReadOnly) then
          begin
            S := FOnInsertMacro(Self, ACommand - ecInsertMacro0);
            if S = '' then exit;
            InsertTextAtCurrentPos(S);
            PaintCaret(true);
            Complete;
          end;
        ecBlockOpA..ecBlockOpZ:
          if (not FReadOnly) and (Assigned(FOnBlockOperation)) and (FSelected) then
            SelText := FOnBlockOperation(Self, ACommand - ecBlockOpA, SelText);
        {$IFDEF MEMOEX_COMPLETION}
        ecCompletionIdentifers:
          if not FReadOnly then
          begin
            FCompletion.DoCompletion(cmIdentifers);
            PaintCaret(true);
            Complete;
          end;
        ecCompletionTemplates:
          if not FReadOnly then
          begin
            FCompletion.DoCompletion(cmTemplates);
            PaintCaret(true);
            Complete;
          end;
        {$ENDIF MEMOEX_COMPLETION}
        ecBeginUpdate:
          BeginUpdate;
        ecEndUpdate:
          EndUpdate;

        ecRecordMacro:
          if FRecording then
            EndRecord(FDefMacro)
          else
            BeginRecord;
        ecPlayMacro:
          begin
            PlayMacro(FDefMacro);
            Complete;
          end;
        ecSaveBlock:
          if (FSelected) and (Assigned(FOnSaveBlock)) then FOnSaveBlock(Self, SelText);
        ecInsertBlock:
          if (not FReadOnly) and (Assigned(FOnInsertBlock)) then
          begin
            if FOnInsertBlock(Self, S) then InsertTextAtCurrentPos(S);
            PaintCaret(true);
            Complete;
          end;
      end;

      {$IFDEF MEMOEX_UNDO}
      if CaretUndo then
        SetCaret(X, Y) else
        SetCaretInternal(X, Y);
      {$ELSE}
      SetCaret(X, Y);
      {$ENDIF MEMOEX_UNDO}
    except
      on E: EComplete do { OK };
    end;
  finally
    // dec(FUpdateLock);
    PaintCaret(true);
  end;
end;
{$ENDIF}

procedure TCustomMemoEx.PostCommand(ACommand: TEditCommand);
begin
  PostMessage(Handle, WM_EDITCOMMAND, ACommand, 0);
end; { PostCommand }

procedure TCustomMemoEx.ClipboardChanged;
begin
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then exit;
  if Assigned(FOnChangeClipboardState) then
    FOnChangeClipboardState(Self, IsClipboardFormatAvailable(CF_TEXT) or IsClipboardFormatAvailable(CF_OEMTEXT));
end;

procedure TCustomMemoEx.WndProc(var Message: TMessage);
var
  Form: TCustomForm;
  pt, temp: TPoint;
begin
  case Message.Msg of
  {$IFNDEF RA_D4H}
  WM_SIZE:
    begin
      inherited WndProc(Message);
      if not (csLoading in ComponentState) then Resize;
      exit;
    end;
  {$ENDIF RA_D4H}
  CM_COLORCHANGED:
    begin
      Message.Result := 0;
      Invalidate;
      exit;
    end;
  WM_MOUSEWHEEL:
    begin
      MouseWheelHandler(Message);
      Message.Result := 0;
      exit;
    end;
  WM_SYSCHAR:
    if Message.wParam = VK_BACK then
    begin
      Message.Result := 0;
      exit;
    end;
  WM_ERASEBKGND:
    begin
      {$IFDEF MEMOEX_NOOPTIMIZE}
      inherited WndProc(Message);
      Message.Result := 1;
      {$ELSE}
      Message.Result := 0;
      {$ENDIF}
      exit;
    end;
  WM_SETFOCUS:
    begin
      Form := GetParentForm(Self);
      if (Form <> nil) and (not Form.SetFocusedControl(Self)) then exit;
      CreateCaret(Handle, 0, 2, CellRect.Height - 2);
      PaintCaret(true);
      DoEnter;
    end;
  WM_KILLFOCUS:
    begin
      if csFocusing in ControlState then exit;
      {$IFDEF MEMOEX_COMPLETION}
      if FCompletion.FVisible then FCompletion.CloseUp(false);
      {$ENDIF MEMOEX_COMPLETION}
      DestroyCaret;
      DoExit;
    end;
  WM_GETDLGCODE:
    begin
      inherited WndProc(Message);
      TWMGetDlgCode(Message).Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
      if FWantTabs then TWMGetDlgCode(Message).Result := TWMGetDlgCode(Message).Result or DLGC_WANTTAB;
      exit;
    end;
  WM_HSCROLL:
    begin
      scbHorz.DoScroll(TWMHScroll(Message));
      exit;
    end;
  WM_VSCROLL:
    begin
      scbVert.DoScroll(TWMVScroll(Message));
      exit;
    end;
  WM_SETTINGCHANGE:
    begin
      scbVertWidth := GetSystemMetrics(SM_CXVSCROLL);
      scbHorzHeight := GetSystemMetrics(SM_CYHSCROLL);
    end;
  WM_EDITCOMMAND:
    begin
      Command(Message.WParam);
      Message.Result := ord(true);
      exit;
    end;
  WM_CHANGECBCHAIN:
    begin
      Message.Result := 0;
      if TWMChangeCBChain(Message).Remove = NextClipViewer then NextClipViewer := TWMChangeCBChain(Message).Next
        else SendMessage(NextClipViewer, WM_CHANGECBCHAIN, TWMChangeCBChain(Message).Remove, TWMChangeCBChain(Message).Next);
      exit;
    end;
  WM_DRAWCLIPBOARD:
    begin
      ClipboardChanged;
      SendMessage(NextClipViewer, WM_DRAWCLIPBOARD, 0, 0);
      exit;
    end;
  WM_DESTROY:
      ChangeClipboardChain(Handle, NextClipViewer);
  WM_CONTEXTMENU:
    begin
      pt := SmallPointToPoint(TWMContextMenu(Message).Pos);
      if pt.X < 0 then temp := pt
      else temp := ScreenToClient(pt);
      if PtInRect(ClientRect, temp) then
        GetWordUnderCursor(temp.X, temp.Y);
    end;
  WM_COPY:
    begin
      PostCommand(ecClipboardCopy);
      Message.Result := ord(true);
      exit;
    end;
  {$IFDEF MEMOEX_EDITOR}
  WM_CUT:
    begin
      if not FReadOnly then PostCommand(ecClipboardCut);
      Message.Result := ord(true);
      exit;
    end;
  WM_PASTE:
    begin
      if not FReadOnly then PostCommand(ecClipBoardPaste);
      Message.Result := ord(true);
      exit;
    end;
  {$ENDIF}
  end;
  inherited WndProc(Message);
end;

{$IFDEF MEMOEX_EDITOR}

procedure TCustomMemoEx.SetXY(X, Y: integer);
var X1, Y1: integer;
begin
  X1 := FLeftCol;
  Y1 := FTopRow;
  if (Y < FTopRow) or (Y > FLastVisibleRow) then
    Y1 := Y - (FVisibleRowCount div 2);
  if (X < FLeftCol) or (X > FVisibleColCount) then
    X1 := X - (FVisibleColCount div 2);
  SetLeftTop(X1, Y1);
  SetCaret(X, Y);
end;

procedure TCustomMemoEx.ChangeBookMark(const BookMark: TBookMarkNum; const
  Valid: boolean);
begin
  if Valid then
    with FBookMarks[Bookmark] do
    if Valid and (Y = FCaretY) then
      Valid := false else begin
      X := FCaretX;
      Y := FCaretY;
      Valid := true;
    end
  else
    with FBookMarks[Bookmark] do
    if Valid then
      SetXY(X, Y);
  BookmarkChanged(BookMark);
end;
{$ENDIF}

procedure TCustomMemoEx.BookmarkChanged(BookMark: integer);
begin
  FGutter.Invalidate;
end;

procedure TCustomMemoEx.SelectionChanged;
begin
  if not (csLoading in ComponentState) then
    if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
end;

procedure TCustomMemoEx.SetSel(const ASelX, ASelY: integer);

  procedure UpdateSelected;
  var
    iR: integer;
  begin
    if FUpdateLock = 0 then
    begin
      if (FUpdateSelBegY <> FSelBegY) or (FUpdateSelBegX <> FSelBegX) then
        for iR := Min(FUpdateSelBegY, FSelBegY) to Max(FUpdateSelBegY, FSelBegY) do
          PaintLine(iR, -1, -1);
      if (FUpdateSelEndY <> FSelEndY) or (FUpdateSelEndX <> FSelEndX) then
        for iR := Min(FUpdateSelEndY, FSelEndY) to Max(FUpdateSelEndY, FSelEndY) do
          PaintLine(iR, -1, -1);
      SelectionChanged;
    end;
  end;

var
  SelX, SelY: integer;
begin
  if ASelX < 0 then
    SelX := 0 else
    SelX := ASelX;
  if ASelY < 0 then
    SelY := 0 else
    SelY := ASelY;
  if not FSelected then
  begin
    FSelStartX := SelX;
    FSelStartY := SelY;
    FSelEndX := SelX;
    FSelEndY := SelY;
    FSelBegX := SelX;
    FSelBegY := SelY;
    FSelected := true;
  end
    else
  begin
    {$IFDEF MEMOEX_UNDO}
    TSelectUndo.Create(Self, FCaretX, FCaretY, FSelBlock, FSelBegX, FSelBegY,
      FSelEndX, FSelEndY);
    {$ENDIF MEMOEX_UNDO}
    FUpdateSelBegX := FSelBegX;
    FUpdateSelBegY := FSelBegY;
    FUpdateSelEndX := FSelEndX;
    FUpdateSelEndY := FSelEndY;
    if SelY <= FSelStartY then
      FSelBegY := SelY;
    if SelY >= FSelStartY then
      FSelEndY := SelY;
    if (SelY < FSelStartY) or ((SelY = FSelStartY) and (SelX <= FSelStartX)) then
    begin
      FSelBegX := SelX;
      FSelEndX := FSelStartX;
      FSelEndY := FSelStartY;
    end
      else
    if (SelY > FSelStartY) or ((SelY = FSelStartY) and (SelX >= FSelStartX)) then
    begin
      FSelBegX := FSelStartX;
      FSelBegY := FSelStartY;
      FSelEndX := SelX;
    end;
    if FSelBegY < 0 then
      FSelBegY := 0;
    FSelected := true;
    if FCompound = 0 then
      UpdateSelected;
  end;
  if FUpdateSelBegY > FSelBegY then
    FUpdateSelBegY := FSelBegY;
  if FUpdateSelEndY < FSelEndY then
    FUpdateSelEndY := FSelEndY;
end;


procedure TCustomMemoEx.Mouse2Cell(const X, Y: integer; var CX, CY: integer);
begin
  CX := Round((X - EditorClient.Left) / FCellRect.Width);
  CY := (Y - EditorClient.Top) div FCellRect.Height;
end;

procedure TCustomMemoEx.Mouse2Caret(const X, Y: integer; var CX, CY: integer);
begin
  Mouse2Cell(X, Y, CX, CY);
  if CX < 0 then CX := 0;
  if CY < 0 then CY := 0;
  CX := CX + FLeftCol;
  CY := CY + FTopRow;
  if CX > FLastVisibleCol then
    CX := FLastVisibleCol;
  if CY > FLines.ParaLineCount - 1 then
    CY := FLines.ParaLineCount - 1;
end;

procedure TCustomMemoEx.CaretCoord(const X, Y: integer; var CX, CY: integer);
begin
  CX := X - FLeftCol;
  CY := Y - FTopRow;
  if CX < 0 then CX := 0;
  if CY < 0 then CY := 0;
  CX := FCellRect.Width * CX;
  CY := FCellRect.Height * CY;
end;

function TCustomMemoEx.ExtractStringWithStyle(XX, YY: integer; const From: string;
  Style: word; const LineAttrs: TLineAttrs; out start: integer): string;
var i: integer;
    last: integer;
begin
  if Style <> RA_EX_STYLE_DEFAULT then begin
    start := XX;
    last := XX;
    if XX <= length(From) then
      for i := XX downto 0 do
        if LineAttrs[i].ex_style = Style then
          start := i else
          break;
    for i := XX + 1 to length(From) - 1 do
      if LineAttrs[i].ex_style = Style then
        last := i else
        break;
    result := copy(From,start+1,last-start+1);
  end else
    start := XX;
end;

{ strip invisible }
function TCustomMemoEx.GetAttrDelta(StartFrom, EndTo: integer; const LineAttrs: TLineAttrs): integer;
var  i, j: integer;
begin
  Result := 0;
  if (ReadOnly) and (FStripInvisible) then begin
    j := EndTo;
    i := StartFrom;
    while (i <= j) and (i < SelAttrs_Size) do begin
      if LineAttrs[i].FC = LineAttrs[i].BC then begin
        inc(Result);
        inc(j);
      end;
      inc(i);
    end;
  end;
end;

function TCustomMemoEx.DoMouseWheel(Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint): boolean;
begin
  MouseWheelScroll(WheelDelta);
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

procedure TCustomMemoEx.MouseWheelScroll(Delta: integer);
var
  i: integer;
begin
  i := Mouse.WheelScrollLines;
  if Delta > 0 then i := -i;
  scbVert.Position := scbVert.Position + i;
  Scroll(true, scbVert.Position);
end;

procedure TCustomMemoEx.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
var
  XX, YY: integer;
begin
  if double_clicked then
  begin
    double_clicked := false;
    exit;
  end;
  {$IFDEF MEMOEX_COMPLETION}
  if FCompletion.FVisible then FCompletion.CloseUp(false);
  {$ENDIF MEMOEX_COMPLETION}
  if Button <> mbRight then begin // right click = popup menu -> no caret change
    mouse_down := true;
    mouse_dragged := false;
    Mouse2Caret(X, Y, XX, YY);
    PaintCaret(false);
    if (Button = mbLeft) and (not (ssShift in Shift)) then SetUnSelected;
    SetFocus;
    if YinBounds(YY) then begin
      if not FCursorBeyondEOL then
        if XX > length(FLines.ParaStrings[YY]) then
          XX := length(FLines.ParaStrings[YY]);
      if (ssShift in Shift) and (SelLength = 0) then
        SetSel(FCaretX, FCaretY);
      SetCaret(XX, YY);
      if ssShift in Shift then
        SetSel(XX, YY);
    end;
    PaintCaret(true);
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomMemoEx.DblClick;
var
  i, PY, PX, iBeg, iEnd: integer;
  FAttrs: TLineAttrs;
  s: string;
begin
  double_clicked := true;
  if Assigned(FOnDblClick) then FOnDblClick(Self);
  if FDoubleClickLine then begin
    PaintCaret(false);
    SetSel(0, FCaretY);
    if FCaretY = FLines.ParaLineCount - 1 then begin
      SetSel(Length(FLines.ParaStrings[FCaretY]), FCaretY);
      SetCaret(Length(FLines.ParaStrings[FCaretY]), FCaretY);
    end else begin
      SetSel(0, FCaretY + 1);
      SetCaret(0, FCaretY + 1);
    end;
    PaintCaret(true);
  end
    else
  if YinBounds(FCaretY) then begin
    s := FLines.GetParagraphByIndex(FCaretY, PY, PX);
    GetLineAttr(PY, FCaretY, PX, length(s), 0, length(s), s, FAttrs);
    i := GetAttrDelta(PX, FCaretX + PX, FAttrs);
    if GetWordOnPosEx(FLines.ParaStrings[FCaretY] + ' ', FCaretX + 1 + i,
      iBeg, iEnd) <> '' then begin
      PaintCaret(false);
      SetSel(iBeg - 1, FCaretY);
      SetSel(iEnd - 1, FCaretY);
      SetCaret(iEnd - 1 - i, FCaretY);
      PaintCaret(true);
    end;
  end;
end;

procedure TCustomMemoEx.GetWordUnderCursor(X, Y: integer; aShift: TShiftState=[]);
var XX, YY, PX, PY, i: integer;
    s: string;
    FAttrs: TLineAttrs;
    start, delta: integer;
begin
  Mouse2Caret(X, Y, XX, YY);
  if YinBounds(YY) then begin
    s := FLines.GetParagraphByIndex(YY, PY, PX);
    GetLineAttr(PY, YY, PX, length(s), 0, length(s), s, FAttrs);
    delta := GetAttrDelta(PX, XX + PX, FAttrs);
    i := XX + PX + delta;
    with FWordUnderCursor do begin
      if (i > 0) and (i < SelAttrs_Size) then begin
        Style := FAttrs[i - 1].ex_style;
        Text := ExtractStringWithStyle(i, YY, s, Style, FAttrs, start);
        TextStart := FLines.GetParaOffs(PY) + start+1 - delta;
      end else begin
        Text := ''; // mark no word found
        TextStart := 0;
      end;
      CaretX := XX;
      CaretY := YY;
      ParaIndex := PY;
      ParaOffset := PX;
      Shift := aShift;
    end;
  end;
end;

procedure TCustomMemoEx.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  timerScroll.Enabled := false;
  mouse_down := false;
  if (Button = mbLeft) and (not mouse_dragged) then
    if Assigned(FOnWordClick) then begin
      GetWordUnderCursor(X, Y, Shift);
      if FWordUnderCursor.Text <> '' then
        FOnWordClick(Self, FWordUnderCursor);
    end;
  inherited MouseUp(Button, Shift, X, Y)
end;

procedure TCustomMemoEx.MouseMove(Shift: TShiftState; X, Y: integer);
var
  C: TCursor;
  S: string;
  i, PY, PX: integer;
  FAttrs: TLineAttrs;
begin
  MouseMoveY := Y;
  Mouse2Caret(X, Y, MouseMoveXX, MouseMoveYY);
  if X < FGutterWidth then Cursor := crArrow;
  if (Shift = [ssLeft]) and (mouse_down) then
  begin
    mouse_dragged := true;
    Cursor := crIBeam;
    PaintCaret(false);
    if MouseMoveYY <= FLastVisibleRow then
    begin
      if not FCursorBeyondEOL then
        if YinBounds(MouseMoveYY) then
          if MouseMoveXX > length(FLines.ParaStrings[MouseMoveYY]) then MouseMoveXX := length(FLines.ParaStrings[MouseMoveYY])
          else
        else MouseMoveXX := 0;
      SetSel(MouseMoveXX, MouseMoveYY);
      SetCaret(MouseMoveXX, MouseMoveYY);
    end;
    timerScroll.Enabled := (Y < 0) or (Y > ClientHeight);
    PaintCaret(true);
  end
    else
  if (Assigned(FOnMouseOver)) and (YinBounds(MouseMoveYY)) then
  begin
    S := FLines.GetParagraphByIndex(MouseMoveYY, PY, PX);
    GetLineAttr(PY, MouseMoveYY, PX, length(S), 0, MouseMoveXX + 1 + PX, S, FAttrs);
    i := MouseMoveXX + PX + GetAttrDelta(PX, MouseMoveXX + PX, FAttrs) - 1;
    if i < SelAttrs_Size then begin
      C := crIBeam;
      FOnMouseOver(Self, FAttrs[i].ex_style, C);
      if C <> Cursor then Cursor := C;
    end else
      Cursor := crIBeam;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TCustomMemoEx.ScrollTimer(Sender: TObject);
begin
  if (MouseMoveY < 0) or (MouseMoveY > ClientHeight) then
  begin
    if (MouseMoveY < -20) then
      dec(MouseMoveYY, FVisibleRowCount) else
    if (MouseMoveY < 0) then
      dec(MouseMoveYY) else
    if (MouseMoveY > ClientHeight + 20) then
      inc(MouseMoveYY, FVisibleRowCount) else
    if (MouseMoveY > ClientHeight) then
      inc(MouseMoveYY);
    PaintCaret(false);
    SetSel(MouseMoveXX, MouseMoveYY);
    SetCaret(MouseMoveXX, MouseMoveYY);
    PaintCaret(true);
  end;
end;


function TCustomMemoEx.GetRealOffs(DefOffs, Index: integer): integer;
var
  l: integer;
begin
  Result := DefOffs;
  if (Index > -1) and (Index < FLines.FParaLinesCount) then
  begin
    l := length(FLines.ParaStrings[Index]);
    if l > 0 then
      if Result > l then
        Result := l else
    else Result := 0;
  end;
end;

function TCustomMemoEx.GetSelText: string;
var sb, se: integer;
begin
  Result := '';
  if not FSelected then exit;
  if not FSelBlock then begin
    if (FSelBegY < 0) or (FSelBegY > FLines.ParaLineCount - 1) or (FSelEndY < 0) or
      (FSelEndY > FLines.ParaLineCount - 1) then begin Err; Exit; end;
    sb := GetRealOffs(FSelBegX, FSelBegY);
    se := GetRealOffs(FSelEndX, FSelEndY);
    if (se = sb) and (FSelBegY = FSelEndY) then exit;
    if FSelBegY<>FSelEndY then begin
      sb := PosFromCaret(sb, FSelBegY);
      se := PosFromCaret(se, FSelEndY);
      Result := System.Copy(FLines.Text,
        sb + 1, se - sb + integer(FInclusive));
    end else begin // AB faster if without #13
      result := system.Copy(FLines.ParaStrings[FSelBegY],
        sb + 1, se - sb + integer(FInclusive));
    end;
  end;
end;

function TCustomMemoEx.GetSelLength: integer;
begin
//  Result := Length(GetSelText);
  Result := 0;
  if not FSelected then exit;
  if not FSelBlock then begin
    if (FSelBegY < 0) or (FSelBegY > FLines.ParaLineCount - 1) or (FSelEndY < 0) or
      (FSelEndY > FLines.ParaLineCount - 1) then Exit;
    result := PosFromCaret(FSelEndX, FSelEndY) - PosFromCaret(FSelBegX, FSelBegY)
       + integer(FInclusive);
//    result := GetRealOffs(FSelEndX, FSelEndY) - GetRealOffs(FSelBegX, FSelBegY) + integer(FInclusive);
  end;
end;

procedure TCustomMemoEx.SetSelText(const AValue: string);
begin
  BeginUpdate;
  try
    BeginCompound;
    DeleteSelected;
    if AValue <> '' then
    begin
      InsertText(AValue);
      FSelectedText := true;
      SelStart := PosFromCaret(FSelBegX, FSelBegY) + 1;
      SelLength := Length(AValue);
    end;
    EndCompound;
  finally
    EndUpdate;
  end;
end;

procedure TCustomMemoEx.ClipBoardCopy;
var s: string;
begin
  if not FSelBlock then begin
    s := GetSelText;
{$ifdef CLIPBOARDPROTECT} // ClipProtect will trunc clipboard to 2KB
    FClip := s;
    if FClipProtect and (length(s)>2000) then
      setLength(s,2000);
{$endif}
    _CopyToClipboard(Handle, s, Font.Charset);
  end;
end;

{$IFDEF MEMOEX_EDITOR}

procedure TCustomMemoEx.ReplaceWord(const NewString: string);
var
  iBeg, iEnd: integer;

  function GetWordOnPos2(S: string; P: integer): string;
  begin
    Result := '';
    if P < 1 then exit;
    if (S[P] in Separators) and ((P < 1) or (S[P - 1] in Separators)) then
      inc(P);
    iBeg := P;
    while iBeg >= 1 do if S[iBeg] in Separators then break else dec(iBeg);
    inc(iBeg);
    iEnd := P;
    while iEnd <= Length(S) do if S[iEnd] in Separators then break else inc(iEnd);
    if iEnd > iBeg then
      Result := Copy(S, iBeg, iEnd - iBeg) else
      Result := S[P];
  end;

var
  S, W: string;
  X: integer;
begin
  PaintCaret(false);
  BeginUpdate;
  S := FLines.ParaStrings[FCaretY];
  while FCaretX > Length(S) do
    S := S + ' ';
  W := Trim(GetWordOnPos2(S, FCaretX));
  if W = '' then
  begin
    iBeg := FCaretX + 1;
    iEnd := FCaretX
  end;
  {$IFDEF MEMOEX_UNDO}
  CantUndo;
  //TReplaceUndo.Create(Self, FCaretX - Length(W), FCaretY, iBeg, iEnd, W, NewString);
  {$ENDIF MEMOEX_UNDO}
  Delete(S, iBeg, iEnd - iBeg);
  Insert(NewString, S, iBeg);
  FLines.InternalParaStrings[FCaretY] := S;
  X := iBeg + Length(NewString) - 1;
  PaintLine(FCaretY, -1, -1);
  SetCaretInternal(X, FCaretY);
  Changed;
  EndUpdate;
  PaintCaret(true);
end;

procedure TCustomMemoEx.ReplaceWord2(const NewString: string);
var S, S1, W: string;
    P, X, Y: integer;
    iBeg, iEnd: integer;
    NewCaret: integer;
begin
  PaintCaret(false);
  if FCaretX > Length(FLines.ParaStrings[FCaretY]) then
    FLines.InternalParaStrings[FCaretY] := FLines.ParaStrings[FCaretY] +
      StringOfChar(' ',FCaretX - Length(FLines.ParaStrings[FCaretY]));
  S := FLines.Text;
  P := PosFromCaret(FCaretX, FCaretY);
  W := Trim(GetWordOnPosEx(S, P, iBeg, iEnd));
  if W = '' then
  begin
    iBeg := P + 1;
    iEnd := P
  end;
  S1 := NewString;
  NewCaret := Length(NewString);
  {$IFDEF MEMOEX_UNDO}
  TReplaceUndo.Create(Self, FCaretX, FCaretY, iBeg, iEnd, W, S1);
  {$ENDIF MEMOEX_UNDO}
  Delete(S, iBeg, iEnd - iBeg);
  Insert(S1, S, iBeg);
  FLines.Text := S; 
  CaretFromPos(iBeg + NewCaret - 1, X, Y);
  SetCaretInternal(X, Y);
  Changed;
  PaintCaret(true);
end;

procedure TCustomMemoEx.InsertText(const Text: string);
var
  S: string;
  P: integer;
  X, Y: integer;
begin
  if Text <> '' then   begin
    PaintCaret(false);
    BeginUpdate;
    Reline;
    {$IFDEF MEMOEX_UNDO}
    TInsertUndo.Create(Self, FCaretX, FCaretY, Text);
    {$ENDIF MEMOEX_UNDO}
    P := PosFromCaret(FCaretX, FCaretY);
    if PosEx(#13,text)>0 then begin // insert with #13 -> old slow method
      S := FLines.Text;
      Insert(Text, S, P + 1);
      FLines.SetLockText(S);
    end else begin // new fast method from AB
      S := FLines.ParaStrings[FCaretY];
      while FCaretX > Length(S) do
        S := S + ' ';
      insert(text,S,FCaretX+1);
      FLines.InternalParaStrings[FCaretY] := S; // will call reformat paragraph
    end;
    CaretFromPos(P + Length(Text), X, Y);
    SetCaretInternal(X, Y);
    Changed;
    EndUpdate;
    PaintCaret(true);
  end;
end;

procedure TCustomMemoEx.InsertTextAtCurrentPos(const _Text: string);
var S: string;
begin
  BeginUpdate;
  S := AdjustLineBreaks(_Text);
  if Assigned(FOnTextInsert) then
    FOnTextInsert(Self, S);
  DeleteSelected;
  InsertText(S);
  EndUpdate;
  scbVert.Position := FTopRow;
end;

function CountChar(P: PChar; Ch: char): integer;
begin
  result := 0;
  if P<>nil then
  while P^<>#0 do begin
    if P^=Ch then
      inc(result);
    inc(P);
  end;
end;

function RtfBackSlash(const Text: string): string;
procedure Replace(S,D: PChar);
begin // faster than PosEx()+Insert()
  repeat
    if S^='\' then begin
      D[0] := '\';
      D[1] := '\';
      inc(D,2);
      inc(S);
    end else
    if S^=#0 then begin
      D^ := S^;
      break;
    end else begin
      D^ := S^;
      inc(D);
      inc(S);
    end;
  until false;
end;
var i: integer;
begin
  i := CountChar(pointer(Text),'\');
  if i=0 then
    result := Text else begin
    SetLength(result,length(Text)+i);
    Replace(pointer(Text),pointer(result));
  end;
end;

procedure TCustomMemoEx.ClipBoardPaste;
var ClipS: string;
begin
  if ReadOnly then exit;
  if Assigned(OnClipboardPaste) and OnClipboardPaste(Self) then
    Exit;
  ClipS := _PasteFromClipboard(Handle, Font.Charset, {$ifdef CLIPBOARDPROTECT}FClip,{$endif} false);
  if ClipPasteRtfBackSlashConvert and
     not IsClipboardFormatAvailable(CF_MEMOEX) then
    InsertTextAtCurrentPos(RtfBackSlash(ClipS)) else // not from TMemoEx -> '\' -> '\\'
    InsertTextAtCurrentPos(ClipS);
end;

procedure TCustomMemoEx.ClipBoardCut;
begin
  ClipBoardCopy;
  DeleteSelected;
end;

procedure TCustomMemoEx.DeleteSelected;
var S, S1: string;
    iBeg, X, Y: integer;
begin
  if FSelected then
  begin
    S1 := GetSelText;
    FSelectedText := false;
    if S1 = '' then exit;
    PaintCaret(false);

    iBeg := PosFromCaret(FSelBegX, FSelBegY);
    {$IFDEF MEMOEX_UNDO}
    TDeleteSelectedUndo.Create(Self, FCaretX, FCaretY, S1, FSelBlock,
      FSelBegX, FSelBegY, FSelEndX, FSelEndY, iBeg);
    {$ENDIF MEMOEX_UNDO}
    if FSelBegY<>FSelEndY then begin // delete with #13 -> old slow method
      S := FLines.Text;
      Delete(S, iBeg + 1, length(S1));
      FLines.SetLockText(S);
    end else begin // new fast method from AB
      s := FLines.ParaStrings[FSelBegY];
      while FSelBegX > Length(S) do
        S := S + ' ';
      Delete(S, FSelBegX + 1, length(S1));
      FLines.InternalParaStrings[FSelBegY] := S; // contient reformat paragraph
    end;
    CaretFromPos(iBeg, X, Y);
    SetCaretInternal(X, Y);
    Changed;
    UpdateEditorSize(false);
    if FUpdateLock = 0 then Invalidate;
    PaintCaret(true);
  end;
end;
{$ENDIF MEMOEX_EDITOR}


procedure TCustomMemoEx.SetGutterWidth(AWidth: integer);
begin
  if FGutterWidth <> AWidth then
  begin
    FGutterWidth := AWidth;
    UpdateEditorSize;
    Invalidate;
  end;
end;

procedure TCustomMemoEx.SetGutterColor(AColor: TColor);
begin
  if FGutterColor <> AColor then
  begin
    FGutterColor := AColor;
    FGutter.Invalidate;
  end;
end;

procedure TCustomMemoEx.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TCustomMemoEx.SetLines(ALines: TEditorStrings);
begin
  if ALines <> nil then
    FLines.Assign(ALines);
  CantUndo;
end;

procedure TCustomMemoEx.TextAllChanged;
begin
  TextAllChangedInternal(true);
end;

procedure TCustomMemoEx.TextAllChangedInternal(const Unselect: Boolean);
begin
  if Unselect then
    FSelectedText := false;
  UpdateEditorSize(false);
  if (Showing) and (FUpdateLock = 0) then
    Invalidate;
end;

procedure TCustomMemoEx.SetCols(ACols: integer);
begin
  if FCols <> ACols then
  begin
    FCols := Max(ACols, 1);
    if FCols > FVisibleColCount then
    begin
      scbHorz.Max := FCols;
      scbHorz.Min := 0;
    end
      else
    begin
      scbHorz.Min := 0;
      scbHorz.Max := 0;
    end;
{    if FVisibleColCount < scbHorz.Max then scbHorz.Page := scbHorz.Max - (scbHorz.Max div Max(1, FVisibleColCount)) * FVisibleColCount
      else scbHorz.Page := 1;}
    scbHorz.LargeChange := FVisibleColCount;
  end;
end;

procedure TCustomMemoEx.SetRows(ARows: integer);
begin
  if FRows <> ARows then
  begin
    FRows := Max(ARows, 1);
    if FRows > FVisibleRowCount then
    begin
      scbVert.Max := FRows - FVisibleRowCount;
      scbVert.Min := 0;
    end
      else
    begin
      scbVert.Min := 0;
      scbVert.Max := 0;
    end;
{    if FVisibleRowCount < scbVert.Max then scbVert.Page := scbVert.Max div Max(1, FVisibleRowCount + FVisibleRowCount div 4)
      else scbVert.Page := 1;}
    scbVert.LargeChange := FVisibleRowCount;
  end;
end;

procedure TCustomMemoEx.SetLeftTop(ALeftCol, ATopRow: integer);
begin
  if ALeftCol < 0 then
    ALeftCol := 0;
  if FLeftCol <> ALeftCol then
  begin
    scbHorz.Position := ALeftCol;
    Scroll(false, ALeftCol);
  end;
  if ATopRow < 0 then
    ATopRow := 0;
  if FTopRow <> ATopRow then
  begin
    scbVert.Position := ATopRow;
    Scroll(true, ATopRow);
  end;
end;

procedure TCustomMemoEx.SetScrollBars(Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    RecreateWnd;
    UpdateEditorSize;
  end;
end;

procedure TCustomMemoEx.SetRightMarginVisible(Value: boolean);
begin
  if FRightMarginVisible <> Value then
  begin
    FRightMarginVisible := Value;
    Invalidate;
  end;
end;

procedure TCustomMemoEx.SetRightMargin(Value: integer);
begin
  if FRightMargin <> Value then
  begin
    FRightMargin := Value;
    FLines.Reformat;
  end;
end;

procedure TCustomMemoEx.SetRightMarginColor(Value: TColor);
begin
  if FRightMarginColor <> Value then
  begin
    FRightMarginColor := Value;
    Invalidate;
  end;
end;


procedure TCustomMemoEx.Changed;
begin
  FModified := true;
  FPEditBuffer := nil;
  if Assigned(FOnChange) then
    FOnChange(Self);
  StatusChanged;
end;

procedure TCustomMemoEx.StatusChanged;
begin
  if not ((csDestroying in ComponentState) or (csLoading in ComponentState)) then
    if Assigned(FOnChangeStatus) then
      FOnChangeStatus(Self);
end;

procedure TCustomMemoEx.CaretFromPos(Pos: integer; var X, Y: integer);
var i, j, k: integer;
begin
  k := 0;
  X := -1;
  Y := -1;
  for i := 0 to FLines.Count - 1 do
  with FLines.FList[i] do begin
    for j := 0 to FCount - 1 do begin
      inc(Y);
      inc(k, length(FStrings[j]));
      if k >= Pos then begin
        X := Pos - (k - length(FStrings[j]));
        exit;
      end;
    end;
    inc(k, 2);
  end;
  Y := FLines.ParaLineCount - 1;
  if Y >= 0 then X := length(FLines.ParaStrings[Y]);
end;

function TCustomMemoEx.PosFromCaret(X, Y: integer): integer;
var i, j, k: integer;
begin
  if Y > FLines.ParaLineCount - 1 then // if after eof -> get max len
    Result := GetTextLen else
  if Y < 0 then // before bof -> -1
    Result := -1 else begin
    Result := 0; // get position
    k := 0;
    for i := 0 to FLines.FCount - 1 do
    with FLines.FList[i] do begin
      if k + (FCount - 1) < Y then begin
        inc(Result,StringDynArrayGetSize(pointer(FStrings),FCount)+2);
        inc(k, FCount);
      end else begin
        for j := 0 to FCount - 1 do
          if k + j < Y then
           inc(Result, length(FStrings[j]));
        inc(Result, X);
        break;
      end;
    end;
  end;
end;

function TCustomMemoEx.PosFromMouse(const X, Y: integer): integer;
var
  X1, Y1: integer;
begin
  Mouse2Caret(X, Y, X1, Y1);
  if (X1 < 0) or (Y1 < 0) then
    Result := -1 else
    Result := PosFromCaret(X1, Y1);
end;

function TCustomMemoEx.GetTextLen: integer;
var i: integer;
begin
  Result := FLines.FCount*2; // #13+#10 for each line
  for i := 0 to FLines.FCount-1 do
  with FLines.FList[i] do
    inc(Result,StringDynArrayGetSize(pointer(FStrings),FCount));
end;

function TCustomMemoEx.GetSelStart: integer;
begin
  if FSelectedText then
    Result := PosFromCaret(GetRealOffs(FSelBegX, FSelBegY), FSelBegY) + 1 else
    Result := PosFromCaret(GetRealOffs(FCaretX, FCaretY), FCaretY) + 1;
end;

procedure TCustomMemoEx.SetSelStart(const ASelStart: integer);
begin
  FSelectedText := true;
  CaretFromPos(ASelStart - 1, FSelBegX, FSelBegY);
  SetCaretInternal(FSelBegX, FSelBegY);
  SetSelLength(0);
  MakeRowVisible(FSelBegY);
end;

procedure TCustomMemoEx.MakeRowVisible(ARow: integer);
begin
  if (ARow < FTopRow) or (ARow > FLastVisibleRow) then
  begin
    ARow := FCaretY - Trunc(VisibleRowCount / 2);
    if ARow < 0 then ARow := 0;
    SetLeftTop(FLeftCol, ARow);
  end;
end;

procedure TCustomMemoEx.SetSelLength(const ASelLength: integer);
begin
  FSelectedText := ASelLength > 0;
  CaretFromPos(SelStart + ASelLength - 1, FSelEndX, FSelEndY);
  FUpdateSelBegY := FSelBegY;
  FUpdateSelEndY := FSelEndY;
  SetCaretInternal(FSelEndX, FSelEndY);
  Invalidate;
end;

procedure TCustomMemoEx.SetLockText(const Text: string);
begin
  FLines.SetLockText(Text);
end;

procedure TCustomMemoEx.GutterPaint(Canvas: TCanvas; const Rect: TRect);
begin
  if Assigned(FOnPaintGutter) then
    FOnPaintGutter(Self, Canvas, Rect);
end;

procedure TCustomMemoEx.SetMode(index: integer; Value: boolean);
var
  PB: ^boolean;
begin
  case index of
    0: PB := @FInsertMode;
  else {1 :}
    PB := @FReadOnly;
  end;
  if PB^ <> Value then
  begin
    PB^ := Value;
    if index = 1 then
      Invalidate;
    StatusChanged;
  end;
end;

function TCustomMemoEx.GetWordOnCaret: string;
begin
  Result := GetWordOnPos(FLines.ParaStrings[CaretY], CaretX+1);
end;

function TCustomMemoEx.GetTabStop(const X, Y: integer; const What: TTabStop;
  const Next: Boolean): integer;

  procedure UpdateTabStops;
  var
    S: string;
    j, i: integer;

    function ProcessString: boolean;
    begin
      Result := false;
      if (What = tsTabStop) and (length(S) > 0) then FTabPos[length(S) - 1] := true;
      while i <= length(S) do
      begin
        if S[i] = ' ' then
        begin
          FTabPos[i - 1] := true;
          if i >= X then Result := true;
        end;
        inc(i);
      end;
    end;

  begin
    FillChar(FTabPos[0], Max_X, false);
    if (FSmartTab) and (What = tsTabStop) then
    begin
      j := 1;
      i := 1;
      while Y - j >= 0 do
      begin
        S := TrimRight(FLines.ParaStrings[Y-j]);
        if ProcessString then break;
        if i >= Max_X div 4 then Break;
        if j >= FVisibleRowCount*2 then Break;
        inc(j);
      end;
    end
    else
      if (What=tsAutoIndent) and FAutoIndent then
      begin
        FLines.Index2ParaIndex(Y, i, j);
        if i - 1 >= 0 then
        begin
          S := FLines[i-1];
          i := 1;
          ProcessString;
        end;
      end;
  end;

var
  i: integer;
begin
  UpdateTabStops;
  Result := X;
  if Next then
  begin
    for i := X + 1 to High(FTabPos) do
      if (not FTabPos[i - 1]) and (What = tsAutoIndent) then
      begin
        Result := i - 1;
        exit;
      end
      else
        if (not FTabPos[i]) and (i > 0) then
          if FTabPos[i - 1] then
          begin
            Result := i;
            Exit;
          end;
    if Result = X then
      Result := GetDefTabStop(X, true);
  end
  else
    if Result = X then
      Result := GetDefTabStop(X, false);
end;

function TCustomMemoEx.GetDefTabStop(const X: integer; const Next: Boolean): integer;
var S: string;
    i: integer;
begin
  S := Trim(SubStr(FTabStops, 0, ' '));
  try
    i := StrToInt(S);
  except
    i := 8;
  end;
  if i = 0 then
    Result := X else
    if i > X then
      Result := i else
      if X mod i = 0 then
        Result := X + i else
        Result := ((X div i) + 1) * i;
end;

function TCustomMemoEx.GetBackStop(const X, Y: integer): integer;

  procedure UpdateBackStops;
  var
    S: string;
    j, i, k: integer;
  begin
    j := 1;
    i := X - 1;
    FillChar(FTabPos[0], Max_X, false);
    FTabPos[0] := true;
    while Y - j >= 0 do
    begin
      S := FLines.ParaStrings[Y - j];
      for k := 1 to Min(Length(S), i) do { Iterate }
        if S[k] <> ' ' then
        begin
          i := k;
          FTabPos[i - 1] := true;
          Break;
        end;
      if i = 1 then Break;
      if j >= FVisibleRowCount * 2 then Break;
      inc(j);
    end;
  end;

var i: integer;
    S: string;
begin
  Result := X - 1;
  S := TrimRight(FLines.ParaStrings[Y]);
  if (Trim(Copy(S, 1, X)) = '') and
    ((X + 1 > Length(S)) or (S[X + 1] <> ' ')) then
  begin
    UpdateBackStops;
    for i := X downto 0 do
      if FTabPos[i] then
      begin
        Result := i;
        Exit;
      end;
  end;
end;

procedure TCustomMemoEx.BeginCompound;
begin
  inc(FCompound);
  TBeginCompoundUndo.Create(Self);
end;

procedure TCustomMemoEx.EndCompound;
begin
  TEndCompoundUndo.Create(Self);
  dec(FCompound);
end;

procedure TCustomMemoEx.BeginRecord;
begin
  FMacro := '';
  FRecording := true;
  StatusChanged;
end;

procedure TCustomMemoEx.EndRecord(var AMacro: TMacro);
begin
  FRecording := false;
  AMacro := FMacro;
  StatusChanged;
end;

procedure TCustomMemoEx.PlayMacro(const AMacro: TMacro);
var
  i: integer;
begin
  BeginUpdate;
  BeginCompound;
  try
    i := 1;
    while i < Length(AMacro) do
    begin
      Command(byte(AMacro[i]) + byte(AMacro[i + 1]) shl 8);
      inc(i, 2);
    end;
  finally
    EndCompound;
    EndUpdate;
  end;
end;


constructor TEditKey.Create(const ACommand: TEditCommand; const AKey1: word;
  const AShift1: TShiftState);
begin
  Key1 := AKey1;
  Shift1 := AShift1;
  Command := ACommand;
end;

constructor TEditKey.Create2(const ACommand: TEditCommand; const AKey1: word;
  const AShift1: TShiftState; const AKey2: word; const AShift2: TShiftState);
begin
  Key1 := AKey1;
  Shift1 := AShift1;
  Key2 := AKey2;
  Shift2 := AShift2;
  Command := ACommand;
end;

constructor TKeyboard.Create;
begin
  List := TList.Create;
end;

destructor TKeyboard.Destroy;
begin
  Clear;
  List.Free;
end;

procedure TKeyboard.Add(const ACommand: TEditCommand; const AKey1: word;
  const AShift1: TShiftState);
begin
  List.Add(TEditKey.Create(ACommand, AKey1, AShift1));
end;

procedure TKeyboard.Add2(const ACommand: TEditCommand; const AKey1: word;
  const AShift1: TShiftState; const AKey2: word; const AShift2: TShiftState);
begin
  List.Add(TEditKey.Create2(ACommand, AKey1, AShift1, AKey2, AShift2));
end;

procedure TKeyboard.Clear;
var i: integer;
begin
  for i := 0 to List.Count - 1 do
    TObject(List[i]).Free;
  List.Clear;
end;

function TKeyboard.Command(const AKey: word; const AShift: TShiftState):
  TEditCommand;
var i: integer;
begin
  Result := 0;
  for i := 0 to List.Count - 1 do
    with TEditKey(List[i]) do
      if (Key1 = AKey) and (Shift1 = AShift) then
      begin
        if Key2 = 0 then
          Result := Command else
          Result := twoKeyCommand;
        Exit;
      end;
end;

function TKeyboard.Command2(const AKey1: word; const AShift1: TShiftState;
  const AKey2: word; const AShift2: TShiftState): TEditCommand;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to List.Count - 1 do
    with TEditKey(List[i]) do
      if (Key1 = AKey1) and (Shift1 = AShift1) and
        (Key2 = AKey2) and (Shift2 = AShift2) then
      begin
        Result := Command;
        Exit;
      end;
end;

{$IFDEF MEMOEX_EDITOR}
{$IFDEF MEMOEX_DEFLAYOUT}

procedure TKeyboard.SetDefLayout;
begin
  Clear;

  Add(ecLeft, VK_LEFT, []);
  Add(ecRight, VK_RIGHT, []);
  Add(ecUp, VK_UP, []);
  Add(ecDown, VK_DOWN, []);
  Add(ecSelLeft, VK_LEFT, [ssShift]);
  Add(ecSelRight, VK_RIGHT, [ssShift]);
  Add(ecSelUp, VK_UP, [ssShift]);
  Add(ecSelDown, VK_DOWN, [ssShift]);
  Add(ecBeginLine, VK_HOME, []);
  Add(ecSelBeginLine, VK_HOME, [ssShift]);
  Add(ecBeginDoc, VK_HOME, [ssCtrl]);
  Add(ecSelBeginDoc, VK_HOME, [ssCtrl, ssShift]);
  Add(ecEndLine, VK_END, []);
  Add(ecSelEndLine, VK_END, [ssShift]);
  Add(ecEndDoc, VK_END, [ssCtrl]);
  Add(ecSelEndDoc, VK_END, [ssCtrl, ssShift]);
  Add(ecPrevWord, VK_LEFT, [ssCtrl]);
  Add(ecNextWord, VK_RIGHT, [ssCtrl]);
  Add(ecSelPrevWord, VK_LEFT, [ssCtrl, ssShift]);
  Add(ecSelNextWord, VK_RIGHT, [ssCtrl, ssShift]);
  Add(ecSelAll, ord('A'), [ssCtrl]);

  Add(ecWindowTop, VK_PRIOR, [ssCtrl]);
  Add(ecWindowBottom, VK_NEXT, [ssCtrl]);
  Add(ecPrevPage, VK_PRIOR, []);
  Add(ecNextPage, VK_NEXT, []);
  Add(ecSelPrevPage, VK_PRIOR, [ssShift]);
  Add(ecSelNextPage, VK_NEXT, [ssShift]);
  Add(ecScrollLineUp, VK_UP, [ssCtrl]);
  Add(ecScrollLineDown, VK_DOWN, [ssCtrl]);

  Add(ecChangeInsertMode, VK_INSERT, []);

  Add(ecInsertPara, VK_RETURN, []);
  Add(ecBackspace, VK_BACK, []);
  Add(ecBackspace, VK_BACK, [ssShift]);
  Add(ecBackword, VK_BACK, [ssCtrl]);
  Add(ecDelete, VK_DELETE, []);
  Add(ecTab, VK_TAB, []);
  Add(ecBackTab, VK_TAB, [ssShift]);
  Add(ecDeleteSelected, VK_DELETE, [ssCtrl]);

  Add(ecClipboardCopy, VK_INSERT, [ssCtrl]);
  Add(ecClipboardCut, VK_DELETE, [ssShift]);
  Add(ecClipBoardPaste, VK_INSERT, [ssShift]);
  Add(ecClipboardCopy, ord('C'), [ssCtrl]);
  Add(ecClipboardCut, ord('X'), [ssCtrl]);
  Add(ecClipBoardPaste, ord('V'), [ssCtrl]);

  Add(ecSetBookmark0, ord('0'), [ssCtrl, ssShift]);
  Add(ecSetBookmark1, ord('1'), [ssCtrl, ssShift]);
  Add(ecSetBookmark2, ord('2'), [ssCtrl, ssShift]);
  Add(ecSetBookmark3, ord('3'), [ssCtrl, ssShift]);
  Add(ecSetBookmark4, ord('4'), [ssCtrl, ssShift]);
  Add(ecSetBookmark5, ord('5'), [ssCtrl, ssShift]);
  Add(ecSetBookmark6, ord('6'), [ssCtrl, ssShift]);
  Add(ecSetBookmark7, ord('7'), [ssCtrl, ssShift]);
  Add(ecSetBookmark8, ord('8'), [ssCtrl, ssShift]);
  Add(ecSetBookmark9, ord('9'), [ssCtrl, ssShift]);

  Add(ecGotoBookmark0, ord('0'), [ssCtrl]);
  Add(ecGotoBookmark1, ord('1'), [ssCtrl]);
  Add(ecGotoBookmark2, ord('2'), [ssCtrl]);
  Add(ecGotoBookmark3, ord('3'), [ssCtrl]);
  Add(ecGotoBookmark4, ord('4'), [ssCtrl]);
  Add(ecGotoBookmark5, ord('5'), [ssCtrl]);
  Add(ecGotoBookmark6, ord('6'), [ssCtrl]);
  Add(ecGotoBookmark7, ord('7'), [ssCtrl]);
  Add(ecGotoBookmark8, ord('8'), [ssCtrl]);
  Add(ecGotoBookmark9, ord('9'), [ssCtrl]);

  Add2(ecSetBookmark0, ord('K'), [ssCtrl], ord('0'), []);
  Add2(ecSetBookmark0, ord('K'), [ssCtrl], ord('0'), [ssCtrl]);
  Add2(ecSetBookmark1, ord('K'), [ssCtrl], ord('1'), []);
  Add2(ecSetBookmark1, ord('K'), [ssCtrl], ord('1'), [ssCtrl]);
  Add2(ecSetBookmark2, ord('K'), [ssCtrl], ord('2'), []);
  Add2(ecSetBookmark2, ord('K'), [ssCtrl], ord('2'), [ssCtrl]);
  Add2(ecSetBookmark3, ord('K'), [ssCtrl], ord('3'), []);
  Add2(ecSetBookmark3, ord('K'), [ssCtrl], ord('3'), [ssCtrl]);
  Add2(ecSetBookmark4, ord('K'), [ssCtrl], ord('4'), []);
  Add2(ecSetBookmark4, ord('K'), [ssCtrl], ord('4'), [ssCtrl]);
  Add2(ecSetBookmark5, ord('K'), [ssCtrl], ord('5'), []);
  Add2(ecSetBookmark5, ord('K'), [ssCtrl], ord('5'), [ssCtrl]);
  Add2(ecSetBookmark6, ord('K'), [ssCtrl], ord('6'), []);
  Add2(ecSetBookmark6, ord('K'), [ssCtrl], ord('6'), [ssCtrl]);
  Add2(ecSetBookmark7, ord('K'), [ssCtrl], ord('7'), []);
  Add2(ecSetBookmark7, ord('K'), [ssCtrl], ord('7'), [ssCtrl]);
  Add2(ecSetBookmark8, ord('K'), [ssCtrl], ord('8'), []);
  Add2(ecSetBookmark8, ord('K'), [ssCtrl], ord('8'), [ssCtrl]);
  Add2(ecSetBookmark9, ord('K'), [ssCtrl], ord('9'), []);
  Add2(ecSetBookmark9, ord('K'), [ssCtrl], ord('9'), [ssCtrl]);

  Add2(ecGotoBookmark0, ord('Q'), [ssCtrl], ord('0'), []);
  Add2(ecGotoBookmark0, ord('Q'), [ssCtrl], ord('0'), [ssCtrl]);
  Add2(ecGotoBookmark1, ord('Q'), [ssCtrl], ord('1'), []);
  Add2(ecGotoBookmark1, ord('Q'), [ssCtrl], ord('1'), [ssCtrl]);
  Add2(ecGotoBookmark2, ord('Q'), [ssCtrl], ord('2'), []);
  Add2(ecGotoBookmark2, ord('Q'), [ssCtrl], ord('2'), [ssCtrl]);
  Add2(ecGotoBookmark3, ord('Q'), [ssCtrl], ord('3'), []);
  Add2(ecGotoBookmark3, ord('Q'), [ssCtrl], ord('3'), [ssCtrl]);
  Add2(ecGotoBookmark4, ord('Q'), [ssCtrl], ord('4'), []);
  Add2(ecGotoBookmark4, ord('Q'), [ssCtrl], ord('4'), [ssCtrl]);
  Add2(ecGotoBookmark5, ord('Q'), [ssCtrl], ord('5'), []);
  Add2(ecGotoBookmark5, ord('Q'), [ssCtrl], ord('5'), [ssCtrl]);
  Add2(ecGotoBookmark6, ord('Q'), [ssCtrl], ord('6'), []);
  Add2(ecGotoBookmark6, ord('Q'), [ssCtrl], ord('6'), [ssCtrl]);
  Add2(ecGotoBookmark7, ord('Q'), [ssCtrl], ord('7'), []);
  Add2(ecGotoBookmark7, ord('Q'), [ssCtrl], ord('7'), [ssCtrl]);
  Add2(ecGotoBookmark8, ord('Q'), [ssCtrl], ord('8'), []);
  Add2(ecGotoBookmark8, ord('Q'), [ssCtrl], ord('8'), [ssCtrl]);
  Add2(ecGotoBookmark9, ord('Q'), [ssCtrl], ord('9'), []);
  Add2(ecGotoBookmark9, ord('Q'), [ssCtrl], ord('9'), [ssCtrl]);

  Add2(ecInsertMacro0, ord('S'), [ssCtrl], ord('0'), [ssCtrl]);
  Add2(ecInsertMacro0, ord('S'), [ssCtrl], ord('0'), []);
  Add2(ecInsertMacro1, ord('S'), [ssCtrl], ord('1'), [ssCtrl]);
  Add2(ecInsertMacro1, ord('S'), [ssCtrl], ord('1'), []);
  Add2(ecInsertMacro2, ord('S'), [ssCtrl], ord('2'), [ssCtrl]);
  Add2(ecInsertMacro2, ord('S'), [ssCtrl], ord('2'), []);
  Add2(ecInsertMacro3, ord('S'), [ssCtrl], ord('3'), [ssCtrl]);
  Add2(ecInsertMacro3, ord('S'), [ssCtrl], ord('3'), []);
  Add2(ecInsertMacro4, ord('S'), [ssCtrl], ord('4'), [ssCtrl]);
  Add2(ecInsertMacro4, ord('S'), [ssCtrl], ord('4'), []);
  Add2(ecInsertMacro5, ord('S'), [ssCtrl], ord('5'), [ssCtrl]);
  Add2(ecInsertMacro5, ord('S'), [ssCtrl], ord('5'), []);
  Add2(ecInsertMacro6, ord('S'), [ssCtrl], ord('6'), [ssCtrl]);
  Add2(ecInsertMacro6, ord('S'), [ssCtrl], ord('6'), []);
  Add2(ecInsertMacro7, ord('S'), [ssCtrl], ord('7'), [ssCtrl]);
  Add2(ecInsertMacro7, ord('S'), [ssCtrl], ord('7'), []);
  Add2(ecInsertMacro8, ord('S'), [ssCtrl], ord('8'), [ssCtrl]);
  Add2(ecInsertMacro8, ord('S'), [ssCtrl], ord('8'), []);
  Add2(ecInsertMacro9, ord('S'), [ssCtrl], ord('9'), [ssCtrl]);
  Add2(ecInsertMacro9, ord('S'), [ssCtrl], ord('9'), []);
  Add2(ecInsertMacroA, ord('S'), [ssCtrl], ord('A'), [ssCtrl]);
  Add2(ecInsertMacroA, ord('S'), [ssCtrl], ord('A'), []);
  Add2(ecInsertMacroB, ord('S'), [ssCtrl], ord('B'), [ssCtrl]);
  Add2(ecInsertMacroB, ord('S'), [ssCtrl], ord('B'), []);
  Add2(ecInsertMacroC, ord('S'), [ssCtrl], ord('C'), [ssCtrl]);
  Add2(ecInsertMacroC, ord('S'), [ssCtrl], ord('C'), []);
  Add2(ecInsertMacroD, ord('S'), [ssCtrl], ord('D'), [ssCtrl]);
  Add2(ecInsertMacroD, ord('S'), [ssCtrl], ord('D'), []);
  Add2(ecInsertMacroE, ord('S'), [ssCtrl], ord('E'), [ssCtrl]);
  Add2(ecInsertMacroE, ord('S'), [ssCtrl], ord('E'), []);
  Add2(ecInsertMacroF, ord('S'), [ssCtrl], ord('F'), [ssCtrl]);
  Add2(ecInsertMacroF, ord('S'), [ssCtrl], ord('F'), []);
  Add2(ecInsertMacroG, ord('S'), [ssCtrl], ord('G'), [ssCtrl]);
  Add2(ecInsertMacroG, ord('S'), [ssCtrl], ord('G'), []);
  Add2(ecInsertMacroH, ord('S'), [ssCtrl], ord('H'), [ssCtrl]);
  Add2(ecInsertMacroH, ord('S'), [ssCtrl], ord('H'), []);
  Add2(ecInsertMacroI, ord('S'), [ssCtrl], ord('I'), [ssCtrl]);
  Add2(ecInsertMacroI, ord('S'), [ssCtrl], ord('I'), []);
  Add2(ecInsertMacroJ, ord('S'), [ssCtrl], ord('J'), [ssCtrl]);
  Add2(ecInsertMacroJ, ord('S'), [ssCtrl], ord('J'), []);
  Add2(ecInsertMacroK, ord('S'), [ssCtrl], ord('K'), [ssCtrl]);
  Add2(ecInsertMacroK, ord('S'), [ssCtrl], ord('K'), []);
  Add2(ecInsertMacroL, ord('S'), [ssCtrl], ord('L'), [ssCtrl]);
  Add2(ecInsertMacroL, ord('S'), [ssCtrl], ord('L'), []);
  Add2(ecInsertMacroM, ord('S'), [ssCtrl], ord('M'), [ssCtrl]);
  Add2(ecInsertMacroM, ord('S'), [ssCtrl], ord('M'), []);
  Add2(ecInsertMacroN, ord('S'), [ssCtrl], ord('N'), [ssCtrl]);
  Add2(ecInsertMacroN, ord('S'), [ssCtrl], ord('N'), []);
  Add2(ecInsertMacroO, ord('S'), [ssCtrl], ord('O'), [ssCtrl]);
  Add2(ecInsertMacroO, ord('S'), [ssCtrl], ord('O'), []);
  Add2(ecInsertMacroP, ord('S'), [ssCtrl], ord('P'), [ssCtrl]);
  Add2(ecInsertMacroP, ord('S'), [ssCtrl], ord('P'), []);
  Add2(ecInsertMacroQ, ord('S'), [ssCtrl], ord('Q'), [ssCtrl]);
  Add2(ecInsertMacroQ, ord('S'), [ssCtrl], ord('Q'), []);
  Add2(ecInsertMacroR, ord('S'), [ssCtrl], ord('R'), [ssCtrl]);
  Add2(ecInsertMacroR, ord('S'), [ssCtrl], ord('R'), []);
  Add2(ecInsertMacroS, ord('S'), [ssCtrl], ord('S'), [ssCtrl]);
  Add2(ecInsertMacroS, ord('S'), [ssCtrl], ord('S'), []);
  Add2(ecInsertMacroT, ord('S'), [ssCtrl], ord('T'), [ssCtrl]);
  Add2(ecInsertMacroT, ord('S'), [ssCtrl], ord('T'), []);
  Add2(ecInsertMacroU, ord('S'), [ssCtrl], ord('U'), [ssCtrl]);
  Add2(ecInsertMacroU, ord('S'), [ssCtrl], ord('U'), []);
  Add2(ecInsertMacroV, ord('S'), [ssCtrl], ord('V'), [ssCtrl]);
  Add2(ecInsertMacroV, ord('S'), [ssCtrl], ord('V'), []);
  Add2(ecInsertMacroW, ord('S'), [ssCtrl], ord('W'), [ssCtrl]);
  Add2(ecInsertMacroW, ord('S'), [ssCtrl], ord('W'), []);
  Add2(ecInsertMacroX, ord('S'), [ssCtrl], ord('X'), [ssCtrl]);
  Add2(ecInsertMacroX, ord('S'), [ssCtrl], ord('X'), []);
  Add2(ecInsertMacroY, ord('S'), [ssCtrl], ord('Y'), [ssCtrl]);
  Add2(ecInsertMacroY, ord('S'), [ssCtrl], ord('Y'), []);
  Add2(ecInsertMacroZ, ord('S'), [ssCtrl], ord('Z'), [ssCtrl]);
  Add2(ecInsertMacroZ, ord('S'), [ssCtrl], ord('Z'), []);

  {$IFDEF MEMOEX_UNDO}
  Add(ecUndo, ord('Z'), [ssCtrl]);
  Add(ecUndo, VK_BACK, [ssAlt]);
  {$ENDIF MEMOEX_UNDO}

  {$IFDEF MEMOEX_COMPLETION}
  Add(ecCompletionIdentifers, VK_SPACE, [ssCtrl]);
  Add(ecCompletionTemplates, ord('J'), [ssCtrl]);
  {$ENDIF MEMOEX_COMPLETION}

  { cursor movement - default and classic }
  Add2(ecBeginDoc, ord('Q'), [ssCtrl], ord('R'), []);
  Add2(ecEndDoc, ord('Q'), [ssCtrl], ord('C'), []);

  Add2(ecBeginLine, ord('Q'), [ssCtrl], ord('S'), []);
  Add2(ecEndLine, ord('Q'), [ssCtrl], ord('D'), []);

  Add2(ecWindowTop, ord('Q'), [ssCtrl], ord('E'), []);
  Add2(ecWindowBottom, ord('Q'), [ssCtrl], ord('X'), []);

  Add2(ecWindowTop, ord('Q'), [ssCtrl], ord('T'), []);
  Add2(ecWindowBottom, ord('Q'), [ssCtrl], ord('U'), []);

  Add(ecDeleteWord, ord('T'), [ssCtrl]);
  Add(ecInsertPara, ord('N'), [ssCtrl]);
  Add(ecDeleteLine, ord('Y'), [ssCtrl]);

  Add2(ecSelWord, ord('K'), [ssCtrl], ord('T'), [ssCtrl]);
  Add2(ecToUpperCase, ord('K'), [ssCtrl], ord('O'), [ssCtrl]);
  Add2(ecToLowerCase, ord('K'), [ssCtrl], ord('N'), [ssCtrl]);
  Add2(ecChangeCase, ord('O'), [ssCtrl], ord('U'), [ssCtrl]);

  Add2(ecInsertBlock, ord('K'), [ssCtrl], ord('R'), [ssCtrl]);
  Add2(ecSaveBlock, ord('K'), [ssCtrl], ord('W'), [ssCtrl]);

  Add(ecRecordMacro, ord('R'), [ssCtrl,ssShift]);
  Add(ecPlayMacro, ord('P'), [ssCtrl]);

  Add2(ecBlockOpA, ord('B'), [ssCtrl], ord('A'), [ssCtrl]);
  Add2(ecBlockOpA, ord('B'), [ssCtrl], ord('A'), []);
  Add2(ecBlockOpB, ord('B'), [ssCtrl], ord('B'), [ssCtrl]);
  Add2(ecBlockOpB, ord('B'), [ssCtrl], ord('B'), []);
  Add2(ecBlockOpC, ord('B'), [ssCtrl], ord('C'), [ssCtrl]);
  Add2(ecBlockOpC, ord('B'), [ssCtrl], ord('C'), []);
  Add2(ecBlockOpD, ord('B'), [ssCtrl], ord('D'), [ssCtrl]);
  Add2(ecBlockOpD, ord('B'), [ssCtrl], ord('D'), []);
  Add2(ecBlockOpE, ord('B'), [ssCtrl], ord('E'), [ssCtrl]);
  Add2(ecBlockOpE, ord('B'), [ssCtrl], ord('E'), []);
  Add2(ecBlockOpF, ord('B'), [ssCtrl], ord('F'), [ssCtrl]);
  Add2(ecBlockOpF, ord('B'), [ssCtrl], ord('F'), []);
  Add2(ecBlockOpG, ord('B'), [ssCtrl], ord('G'), [ssCtrl]);
  Add2(ecBlockOpG, ord('B'), [ssCtrl], ord('G'), []);
  Add2(ecBlockOpH, ord('B'), [ssCtrl], ord('H'), [ssCtrl]);
  Add2(ecBlockOpH, ord('B'), [ssCtrl], ord('H'), []);
  Add2(ecBlockOpI, ord('B'), [ssCtrl], ord('I'), [ssCtrl]);
  Add2(ecBlockOpI, ord('B'), [ssCtrl], ord('I'), []);
  Add2(ecBlockOpJ, ord('B'), [ssCtrl], ord('J'), [ssCtrl]);
  Add2(ecBlockOpJ, ord('B'), [ssCtrl], ord('J'), []);
  Add2(ecBlockOpK, ord('B'), [ssCtrl], ord('K'), [ssCtrl]);
  Add2(ecBlockOpK, ord('B'), [ssCtrl], ord('K'), []);
  Add2(ecBlockOpL, ord('B'), [ssCtrl], ord('L'), [ssCtrl]);
  Add2(ecBlockOpL, ord('B'), [ssCtrl], ord('L'), []);
  Add2(ecBlockOpM, ord('B'), [ssCtrl], ord('M'), [ssCtrl]);
  Add2(ecBlockOpM, ord('B'), [ssCtrl], ord('M'), []);
  Add2(ecBlockOpN, ord('B'), [ssCtrl], ord('N'), [ssCtrl]);
  Add2(ecBlockOpN, ord('B'), [ssCtrl], ord('N'), []);
  Add2(ecBlockOpO, ord('B'), [ssCtrl], ord('O'), [ssCtrl]);
  Add2(ecBlockOpO, ord('B'), [ssCtrl], ord('O'), []);
  Add2(ecBlockOpP, ord('B'), [ssCtrl], ord('P'), [ssCtrl]);
  Add2(ecBlockOpP, ord('B'), [ssCtrl], ord('P'), []);
  Add2(ecBlockOpQ, ord('B'), [ssCtrl], ord('Q'), [ssCtrl]);
  Add2(ecBlockOpQ, ord('B'), [ssCtrl], ord('Q'), []);
  Add2(ecBlockOpR, ord('B'), [ssCtrl], ord('R'), [ssCtrl]);
  Add2(ecBlockOpR, ord('B'), [ssCtrl], ord('R'), []);
  Add2(ecBlockOpS, ord('B'), [ssCtrl], ord('S'), [ssCtrl]);
  Add2(ecBlockOpS, ord('B'), [ssCtrl], ord('S'), []);
  Add2(ecBlockOpT, ord('B'), [ssCtrl], ord('T'), [ssCtrl]);
  Add2(ecBlockOpT, ord('B'), [ssCtrl], ord('T'), []);
  Add2(ecBlockOpU, ord('B'), [ssCtrl], ord('U'), [ssCtrl]);
  Add2(ecBlockOpU, ord('B'), [ssCtrl], ord('U'), []);
  Add2(ecBlockOpV, ord('B'), [ssCtrl], ord('V'), [ssCtrl]);
  Add2(ecBlockOpV, ord('B'), [ssCtrl], ord('V'), []);
  Add2(ecBlockOpW, ord('B'), [ssCtrl], ord('W'), [ssCtrl]);
  Add2(ecBlockOpW, ord('B'), [ssCtrl], ord('W'), []);
  Add2(ecBlockOpX, ord('B'), [ssCtrl], ord('X'), [ssCtrl]);
  Add2(ecBlockOpX, ord('B'), [ssCtrl], ord('X'), []);
  Add2(ecBlockOpY, ord('B'), [ssCtrl], ord('Y'), [ssCtrl]);
  Add2(ecBlockOpY, ord('B'), [ssCtrl], ord('Y'), []);
  Add2(ecBlockOpZ, ord('B'), [ssCtrl], ord('Z'), [ssCtrl]);
  Add2(ecBlockOpZ, ord('B'), [ssCtrl], ord('Z'), []);
end;
{$ENDIF MEMOEX_DEFLAYOUT}


{$IFDEF MEMOEX_UNDO}

procedure RedoNotImplemented;
begin
  raise EMemoExError.Create('Redo not yet implemented');
end;

procedure TCustomMemoEx.CantUndo;
begin
  FUndoBuffer.Clear;
end;

{
  UNDO
}
constructor TUndoBuffer.Create;
begin
  FCancelUndo := false;
  FPtr := -1;
end;

procedure TUndoBuffer.Add(AUndo: TUndo);
begin
  if (self=nil) or InUndo then exit;
  if FCancelUndo then
    Clear else
    FMemoEx.StatusChanged;
  while (Count > 0) and (FPtr < Count - 1) do begin
    TUndo(Items[FPtr + 1]).Free;
    inherited Delete(FPtr + 1);
  end;
  inherited Add(AUndo);
  FPtr := Count - 1;
end;

procedure TUndoBuffer.Undo;
var
  UndoClass: TClass;
  Compound: integer;
begin
  InUndo := true;
  try
    if LastUndo <> nil then
    begin
      Compound := 0;
      UndoClass := LastUndo.ClassType;
      while (LastUndo <> nil) and
        ((UndoClass = LastUndo.ClassType) or
        (LastUndo is TDeleteTrailUndo) or
        (LastUndo is TReLineUndo) or
        (Compound > 0)) do
      begin
        if LastUndo.ClassType = TBeginCompoundUndo then
        begin
          dec(Compound);
          UndoClass := nil;
        end
        else if LastUndo.ClassType = TEndCompoundUndo then
          inc(Compound);
        LastUndo.Undo;
        dec(FPtr);
        if (UndoClass = TDeleteTrailUndo) or
          (UndoClass = TReLineUndo) then
          UndoClass := LastUndo.ClassType;
        if not FMemoEx.FGroupUndo then break;
        // FMemoEx.Paint; {DEBUG !!!!!!!!!}
      end;
      if FMemoEx.FUpdateLock = 0 then
      begin
        FMemoEx.TextAllChangedInternal(false);
        FMemoEx.Changed;
      end;
    end;
  finally
    InUndo := false;
  end;
end;

procedure TUndoBuffer.Redo;
begin
  { DEBUG !!!! }
  inc(FPtr);
  LastUndo.Redo;
end;

procedure TUndoBuffer.Clear;
var i: integer;
begin // AB: memory leak correction
  for i := 0 to Count-1 do
    TUndo(Items[i]).Free;
  inherited;
  FCancelUndo := false;
  FMemoEx.StatusChanged;
end;

procedure TUndoBuffer.Delete;
begin
  if Count > 0 then begin
    TUndo(Items[Count - 1]).Free;
    inherited Delete(Count - 1);
  end;
end;

function TUndoBuffer.LastUndo: TUndo;
begin
  if (FPtr >= 0) and (Count > 0) then
    Result := TUndo(Items[FPtr]) else
    Result := nil;
end;

function TUndoBuffer.IsNewGroup(const AUndo: TUndo): boolean;
begin
  Result := (LastUndo = nil) or (LastUndo.ClassType <> AUndo.ClassType)
end;


{* TUndo}

constructor TUndo.Create(const AMemoEx: TCustomMemoEx);
begin
  FMemoEx := AMemoEx;
  UndoBuffer.Add(Self);
end;

function TUndo.UndoBuffer: TUndoBuffer;
begin
  if FMemoEx <> nil then
    Result := FMemoEx.FUndoBuffer
  else
    Result := nil;
end;
{* TUndo}

{* TCaretUndo}

constructor TCaretUndo.Create(const AMemoEx: TCustomMemoEx;
                              const ACaretX, ACaretY: integer);
begin
  inherited Create(AMemoEx);
  FCaretX := ACaretX;
  FCaretY := ACaretY;
end;

procedure TCaretUndo.Undo;
begin
  with UndoBuffer do
  begin
    dec(FPtr);
    while FMemoEx.FGroupUndo and (FPtr >= 0) and not IsNewGroup(Self) do
      dec(FPtr);
    inc(FPtr);
    with TCaretUndo(Items[FPtr]) do
      FMemoEx.SetCaretInternal(FCaretX, FCaretY);
  end;
end;

procedure TCaretUndo.Redo;
begin
  RedoNotImplemented;
end;
{# TCaretUndo}

{* TInsertUndo}

constructor TInsertUndo.Create(const AMemoEx: TCustomMemoEx;
                               const ACaretX, ACaretY: integer; const AText: string);
var
  i: integer;
begin
  inherited Create(AMemoEx, ACaretX, ACaretY);
  FText := AText;
  AMemoEx.FLines.Caret2Paragraph(ACaretX, ACaretY, i, FParaOffset);
  FOffset := AMemoEx.FLines.GetParaOffs(i) + FParaOffset;
end;

procedure TInsertUndo.Undo;
var S, Text: string;
    _P, _PI: integer;
begin
  Text := '';
  with UndoBuffer do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      Text := TInsertUndo(LastUndo).FText + Text;
      dec(FPtr);
      if not FMemoEx.FGroupUndo then break;
    end;
    inc(FPtr);
  end;
  with TInsertUndo(UndoBuffer.Items[UndoBuffer.FPtr]) do begin
    if PosEx(#13,text)>0 then begin
      S := FMemoEx.FLines.Text;
      Delete(S, FOffset + 1, Length(Text));
      FMemoEx.FLines.SetLockText(S);
    end else begin // new fast method by AB
      FMemoEx.FLines.Index2ParaIndex(FCaretY, _P, _PI);
      s := FMemoEX.FLines[_P];
      delete(s,FParaOffset+1,length(Text));
      FMemoEx.FLines[_P] := S; // contient reformat paragraph
    end;
    FMemoEx.SetCaretInternal(FCaretX, FCaretY);
  end;
end;
{# TInsertUndo}

{ TOverwriteUndo }

constructor TOverwriteUndo.Create(const AMemoEx: TCustomMemoEx;
  const ACaretX, ACaretY: integer; const AOldText, ANewText: string);
var
  i, j: integer;
begin
  inherited Create(AMemoEx, ACaretX, ACaretY);
  FOldText := AOldText;
  FNewText := ANewText;
  AMemoEx.FLines.Caret2Paragraph(ACaretX, ACaretY, i, j);
  FOffset := AMemoEx.FLines.GetParaOffs(i) + j;
end;

procedure TOverwriteUndo.Undo;
var
  S, OldText, NewText: string;
begin
  NewText := '';
  OldText := '';
  with UndoBuffer do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      OldText := TOverwriteUndo(LastUndo).FOldText + OldText;
      NewText := TOverwriteUndo(LastUndo).FNewText + NewText;
      dec(FPtr);
      if not FMemoEx.FGroupUndo then break;
    end;
    inc(FPtr);
  end;
  with TOverwriteUndo(UndoBuffer.Items[UndoBuffer.FPtr]) do begin
    S := FMemoEx.FLines.Text;
    Delete(S, FOffset + 1, Length(NewText));
    Insert(OldText, S, FOffset + 1);
    FMemoEx.FLines.SetLockText(S);
    FMemoEx.SetCaretInternal(FCaretX, FCaretY);
  end;
end;

{* TDeleteUndo}

procedure TDeleteUndo.Undo;
var
  X, Y: integer;
  S, Text: string;
  iBeg: integer;
begin
  Text := '';
  X := -1;
  Y := -1;
  with UndoBuffer do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      if (X = -1) or (Y = -1) then
      begin
        X := TDeleteUndo(LastUndo).FCaretX;
        Y := TDeleteUndo(LastUndo).FCaretY;
      end;
      Text := TDeleteUndo(LastUndo).FText + Text;
      dec(FPtr);
      if not FMemoEx.FGroupUndo then break;
    end;
    inc(FPtr);
  end;
  if (X <> -1) and (Y <> -1) then
    with TDeleteUndo(UndoBuffer.Items[UndoBuffer.FPtr]) do
    begin
      S := FMemoEx.FLines.Text;
      iBeg := FMemoEx.PosFromCaret(X, Y);
      Insert(Text, S, iBeg + 1);
      FMemoEx.FLines.SetLockText(S);
      FMemoEx.CaretFromPos(iBeg, X, Y);
      FMemoEx.SetCaretInternal(X, Y);
    end;
end;
{# TDeleteUndo}

{* TBackspaceUndo}

procedure TBackspaceUndo.Undo;
var
  S, Text: string;
  iBeg: integer;
  X, Y: integer;
begin
  Text := '';
  X := -1;
  Y := -1;
  with UndoBuffer do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      if (X = -1) or (Y = -1) then
      begin
        X := TDeleteUndo(LastUndo).FCaretX;
        Y := TDeleteUndo(LastUndo).FCaretY;
      end;
      Text := Text + TDeleteUndo(LastUndo).FText;
      dec(FPtr);
      if not FMemoEx.FGroupUndo then break;
    end;
    inc(FPtr);
  end;
  if (X <> -1) and (Y <> -1) then
    with TDeleteUndo(UndoBuffer.Items[UndoBuffer.FPtr]) do
    begin
      S := FMemoEx.FLines.Text;
      iBeg := FMemoEx.PosFromCaret(X, Y);
      Insert(Text, S, iBeg + 1);
      FMemoEx.FLines.SetLockText(S);
      FMemoEx.CaretFromPos(iBeg + length(Text), X, Y);
      FMemoEx.SetCaretInternal(X, Y);
    end;
end;
{# TBackspaceUndo}

{* TReplaceUndo}

constructor TReplaceUndo.Create(const AMemoEx: TCustomMemoEx; const ACaretX, ACaretY:
  integer; const ABeg, AEnd: integer; const AText, ANewText: string);
begin
  inherited Create(AMemoEx, ACaretX, ACaretY);
  FBeg := ABeg;
  FEnd := AEnd;
  FText := AText;
  FNewText := ANewText;
end;

procedure TReplaceUndo.Undo;
var
  S: string;
begin
  S := FMemoEx.FLines.Text;
  Delete(S, FBeg, Length(FNewText));
  Insert(FText, S, FBeg);
  FMemoEx.FLines.SetLockText(S);
  FMemoEx.SetCaretInternal(FCaretX, FCaretY);
end;
{# TReplaceUndo}

{* TDeleteSelectedUndo}

constructor TDeleteSelectedUndo.Create(const AMemoEx: TCustomMemoEx; const ACaretX,
  ACaretY: integer; const AText: string; const ASelBlock: boolean; const ASelBegX, ASelBegY,
  ASelEndX, ASelEndY, ASelOffs: integer);
begin
  inherited Create(AMemoEx, ACaretX, ACaretY, AText);
  FSelBlock := ASelBlock;
  FSelBegX := ASelBegX;
  FSelBegY := ASelBegY;
  FSelEndX := ASelEndX;
  FSelEndY := ASelEndY;
  FSelOffs := ASelOffs;
end;

procedure TDeleteSelectedUndo.Undo;
var S, Text: string;
begin
  Text := '';
  with UndoBuffer do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      Text := TDeleteUndo(LastUndo).FText + Text;
      dec(FPtr);
      if not FMemoEx.FGroupUndo then break;
    end;
    inc(FPtr);
  end;
  with TDeleteUndo(UndoBuffer.Items[UndoBuffer.FPtr]) do
  begin
    S := FMemoEx.FLines.Text;
//    iBeg := FMemoEx.PosFromCaret(FSelBegX, FSelBegY); BUG car dÈcalage si wordwrap
    Insert(Text, S, FSelOffs + 1);
    {‚˚‰ÂÎËÚ¸ FSelBegX, FSelBegY}
    FMemoEx.FLines.SetLockText(S);
    FMemoEx.FSelBlock := FSelBlock;
    FMemoEx.FSelBegX := FSelBegX;
    FMemoEx.FSelBegY := FSelBegY;
    FMemoEx.FSelEndX := FSelEndX;
    FMemoEx.FSelEndY := FSelEndY;
    FMemoEx.FSelectedText := Length(FText) > 0;
    FMemoEx.SetCaretInternal(FCaretX, FCaretY);
  end;
end;
{# TDeleteSelectedUndo}

{* TSelectUndo}

constructor TSelectUndo.Create(const AMemoEx: TCustomMemoEx; const ACaretX,
  ACaretY: integer; const ASelBlock: boolean; const ASelBegX, ASelBegY, ASelEndX,
  ASelEndY: integer);
begin
  inherited Create(AMemoEx, ACaretX, ACaretY);
  FSelBlock := ASelBlock;
  FSelBegX := ASelBegX;
  FSelBegY := ASelBegY;
  FSelEndX := ASelEndX;
  FSelEndY := ASelEndY;
end;

procedure TSelectUndo.Undo;
begin
  FMemoEx.FSelectedText := (FSelBegX <> FSelEndX) or (FSelBegY <> FSelEndY);
  FMemoEx.FSelBegX := FSelBegX;
  FMemoEx.FSelBegY := FSelBegY;
  FMemoEx.FSelEndX := FSelEndX;
  FMemoEx.FSelEndY := FSelEndY;
  FMemoEx.FSelBlock := FSelBlock;
  FMemoEx.SetCaretInternal(FCaretX, FCaretY);
end;
{# TSelectUndo}

procedure TBeginCompoundUndo.Undo;
begin
  { nothing }
end;

{$ENDIF MEMOEX_UNDO}



{$IFDEF MEMOEX_COMPLETION}

procedure TCustomMemoEx.CompletionIdentifer(var Cancel: boolean);
begin
  {abstract}
end;

procedure TCustomMemoEx.CompletionTemplate(var Cancel: boolean);
begin
  {abstract}
end;

procedure TCustomMemoEx.DoCompletionIdentifer(var Cancel: boolean);
begin
  CompletionIdentifer(Cancel);
  if Assigned(FOnCompletionIdentifer) then
   FOnCompletionIdentifer(Self, Cancel);
end;

procedure TCustomMemoEx.DoCompletionTemplate(var Cancel: boolean);
begin
  CompletionTemplate(Cancel);
  if Assigned(FOnCompletionTemplate) then
    FOnCompletionTemplate(Self, Cancel);
end;

function TCustomMemoEx.DoPreprocessCompletion(const ID, OldText: string): string;
begin
  if Assigned(FOnPreprocessCompletion) then
    Result := FOnPreprocessCompletion(Self, ID, OldText) else
    Result := OldText;
end;

type
  TMemoExCompletionList = class(TListBox)
  private
    FTimer: TTimer;
    YY: integer;
    // HintWindow : THintWindow;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure WMCancelMode(var Message: TMessage); message WM_CancelMode;
    procedure OnTimer(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      override;
    procedure DrawItem(Index: integer; Rect: TRect; State: TOwnerDrawState);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TCompletion.Create2(AMemoEx: TCustomMemoEx);
begin
  inherited Create;
  FMemoEx := AMemoEx;
  FPopupList := TMemoExCompletionList.Create(FMemoEx);
  FItemHeight := FPopupList.ItemHeight;
  FDropDownCount := 6;
  FDropDownWidth := 300;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := false;
  FTimer.Interval := 800;
  FTimer.OnTimer := OnTimer;
  FIdentifers := TStringList.Create;
  FTemplates := TStringList.Create;
  FItems := TStringList.Create;
  FAutoChange := TStringList.Create;
  TStringList(FAutoChange).OnChange := AutoChangeChanged;
  FAutoChangeList := TList.Create;
  FDefMode := cmIdentifers;
  FCaretChar := '|';
  FCRLF := '/n';
  FSeparator := '=';
end;

destructor TCompletion.Destroy;
begin
  inherited Destroy;
  FPopupList.Free;
  FIdentifers.Free;
  FTemplates.Free;
  FItems.Free;
  FAutoChange.Free;
  ClearAutoChangeList;
  FAutoChangeList.Free;
  FTimer.Free;
end;

function TCompletion.GetItems: TStrings;
begin
  case FMode of
    cmIdentifers: Result := FIdentifers;
    cmTemplates: Result := FTemplates;
  else
    Result := nil;
  end;
end;

procedure TCompletion.ReplaceWord(const ANewString: string);
var
  S, S1, W, NewString: string;
  P, X, Y: integer;
  iBeg, iEnd: integer;
  NewCaret, LNum, CX, CY, i: integer;
begin
  with FMemoEx do
  begin
    PaintCaret(false);
    BeginUpdate;
    ReLine;
    S := FLines.Text;
    P := PosFromCaret(FCaretX, FCaretY);
    W := Trim(GetWordOnPosEx(S, P, iBeg, iEnd));
    LNum := 0;
    if W = '' then
    begin
      iBeg := P + 1;
      iEnd := P
    end;
    CaretFromPos(iBeg, CX, CY);
    if CX < 1 then CX := FCaretX + 1;
    NewString := DoPreprocessCompletion(W, ANewString);
    case FMode of
      cmIdentifers:
        begin
          S1 := NewString;
          NewCaret := Length(NewString);
        end;
      cmTemplates:
        begin
          S1 := StringReplaceAll(NewString, FCRLF, #13#10 + StringOfChar(' ',CX - 1));
          S1 := StringReplaceAll(S1, FCaretChar, '');
          NewCaret := Pos(FCaretChar, NewString) - 1;
          if NewCaret = -1 then NewCaret := Length(NewString);
          for i := 1 to NewCaret do
            if S1[i] = #13 then inc(LNum);
        end
    else
      raise EMemoExError.Create('Invalid MemoEx Completion Mode');
    end;
    {$IFDEF MEMOEX_UNDO}
    TReplaceUndo.Create(FMemoEx, FCaretX, FCaretY, iBeg, iEnd, W, S1);
    {$ENDIF MEMOEX_UNDO}
{    //  LW := Length(W);
    if FSelected then
    begin
      if (FSelBegY <= FCaretY) or (FCaretY >= FSelEndY) then
        // ÒÍÓÂÍÚËÓ‚‡Ú¸ LW ..
    end;}
    Delete(S, iBeg, iEnd - iBeg);
    Insert(S1, S, iBeg);
    FLines.SetLockText(S);
    CaretFromPos(iBeg - 1 + (CX - 1) * LNum + NewCaret, X, Y);
    SetCaretInternal(X, Y);
    FMemoEx.TextAllChanged; // Invalidate; {!!!}
    Changed;
    EndUpdate;
    PaintCaret(true);
  end;
end;

procedure TCompletion.DoKeyPress(Key: Char);
begin
  if FVisible then
    if HasChar(Key, RAEditorCompletionChars) then
      SelectItem
    else
      CloseUp(true)
  else if FEnabled then
    FTimer.Enabled := true;
end;

function TCompletion.DoKeyDown(Key: Word; Shift: TShiftState): boolean;
begin
  Result := true;
  case Key of
    VK_ESCAPE: CloseUp(false);
    VK_RETURN: CloseUp(true);
    VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT:
      FPopupList.Perform(WM_KEYDOWN, Key, 0);
  else
    Result := false;
  end;
end;

procedure TCompletion.DoCompletion(const AMode: TCompletionList);
var
  Eq: boolean;
  Cancel: boolean;
begin
  if FMemoEx.FReadOnly then exit;
  if FPopupList.Visible then CloseUp(false);
  FMode := AMode;
  case FMode of
    cmIdentifers: DropDown(AMode, true);
    cmTemplates:
      begin
        Cancel := false;
        // FMemoEx.DoCompletionIdentifer(Cancel);
        FMemoEx.DoCompletionTemplate(Cancel);
        if Cancel or (FTemplates.Count = 0) then exit;
        MakeItems;
        FindSelItem(Eq);
        if Eq then
          ReplaceWord(SubStr(FItems[ItemIndex], 2, FSeparator))
        else
          DropDown(AMode, true);
      end;
  end;
end;

procedure TCompletion.DropDown(const AMode: TCompletionList; const ShowAlways:
  boolean);
var
  ItemCount: integer;
  P: TPoint;
  Y: integer;
  PopupWidth, PopupHeight: integer;
  SysBorderWidth, SysBorderHeight: integer;
  R: TRect;
  Cancel: boolean;
  Eq: boolean;
begin
  CloseUp(false);
  FMode := AMode;
  with FMemoEx do
  begin
    Cancel := false;
    case FMode of
      cmIdentifers: FMemoEx.DoCompletionIdentifer(Cancel);
      cmTemplates:
        FMemoEx.DoCompletionTemplate(Cancel)
    end;
    MakeItems;
    FindSelItem(Eq);
    // Cancel := not Visible and (ItemIndex = -1);
    if Cancel or (FItems.Count = 0) or (((ItemIndex = -1) or Eq) and not
      ShowAlways) then exit;
    FPopupList.Items := FItems;
    FPopupList.ItemHeight := FItemHeight;
    FVisible := true;
    SetItemIndex(FItemIndex);
    if FListBoxStyle in [lbStandard] then
      FPopupList.Style := lbOwnerDrawFixed
    else
      FPopupList.Style := FListBoxStyle;
    FPopupList.OnMeasureItem := FMemoEx.FOnCompletionMeasureItem;
    FPopupList.OnDrawItem := FMemoEx.FOnCompletionDrawItem;

    ItemCount := FItems.Count;
    SysBorderWidth := GetSystemMetrics(SM_CXBORDER);
    SysBorderHeight := GetSystemMetrics(SM_CYBORDER);
    R := CalcCellRect(FCaretX - FLeftCol, FCaretY - FTopRow + 1);
    P := R.TopLeft;
    P.X := ClientOrigin.X + P.X;
    P.Y := ClientOrigin.Y + P.Y;
    Dec(P.X, 2 * SysBorderWidth);
    Dec(P.Y, SysBorderHeight);
    if ItemCount > FDropDownCount then ItemCount := FDropDownCount;
    PopupHeight := ItemHeight * ItemCount + 2;
    Y := P.Y;
    if (Y + PopupHeight) > Screen.Height then
    begin
      Y := P.Y - PopupHeight - FCellRect.Height + 1;
      if Y < 0 then Y := P.Y;
    end;
    PopupWidth := FDropDownWidth;
    if PopupWidth = 0 then PopupWidth := Width + 2 * SysBorderWidth;
  end;
  FPopupList.Left := P.X;
  FPopupList.Top := Y;
  FPopupList.Width := PopupWidth;
  FPopupList.Height := PopupHeight;
  SetWindowPos(FPopupList.Handle, HWND_TOP, P.X, Y, 0, 0,
    SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
  FPopupList.Visible := true;
end;

function TCompletion.Cmp1(const S1, S2: string): integer;
var T1, T2: string;
begin
  T1 := FMemoEx.DoChangeCase(S1, RA_CASE_CONVERT_LOWER);
  T2 := FMemoEx.DoChangeCase(S2, RA_CASE_CONVERT_LOWER);
  Result := StrLIComp(PChar(T1), PChar(T2), Length(T2));
end;

function TCompletion.Cmp2(const S1, S2: string): boolean;
var T1, T2: string;
begin
  T1 := FMemoEx.DoChangeCase(S1, RA_CASE_CONVERT_LOWER);
  T2 := FMemoEx.DoChangeCase(S2, RA_CASE_CONVERT_LOWER);
  Result := SameText(T1, T2);
end;

procedure TCompletion.MakeItems;
var i: integer;
    S: string;
begin
  FItems.Clear;
  case FMode of
    cmIdentifers:
      for i := 0 to FIdentifers.Count - 1 do
        FItems.Add(FIdentifers[i]);
    cmTemplates:
      begin
        with FMemoEx do
          S := GetWordOnPos(FLines.ParaStrings[CaretY], CaretX);
        for i := 0 to FTemplates.Count - 1 do
          if Cmp1(FTemplates[i], S) = 0 then
            FItems.Add(FTemplates[i]);
        if FItems.Count = 0 then FItems.Assign(FTemplates);
      end;
  end;
end;

procedure TCompletion.FindSelItem(var Eq: boolean);
  function FindFirst(Ss: TSTrings; S: string): integer;
  var i: integer;
  begin
    for i := 0 to Ss.Count - 1 do
      if Cmp1(Ss[i], S) = 0 then
      begin
        Result := i;
        exit;
      end;
    Result := -1;
  end;
var S: string;
begin
  with FMemoEx do
    if FLines.Count > 0 then
      S := GetWordOnPos(FLines.ParaStrings[CaretY], CaretX) else
      S := '';
  if Trim(S) = '' then
    ItemIndex := -1 else
    ItemIndex := FindFirst(FItems, S);
  Eq := (ItemIndex > -1) and Cmp2(Trim(SubStr(FItems[ItemIndex], 0, FSeparator)), S);
end;

procedure TCompletion.SelectItem;
var Cancel: boolean;
    Param: boolean;
begin
  FindSelItem(Param);
  Cancel := not Visible and (ItemIndex = -1);
  case FMode of
    cmIdentifers: FMemoEx.DoCompletionIdentifer(Cancel);
    cmTemplates: FMemoEx.DoCompletionTemplate(Cancel);
  end;
  if Cancel or (GetItems.Count = 0) then CloseUp(false);
end;

procedure TCompletion.CloseUp(const Apply: boolean);
begin
  FItemIndex := ItemIndex;
  FPopupList.Visible := false;
  //  (FPopupList as TMemoExCompletionList). HintWindow.ReleaseHandle;
  FVisible := false;
  FTimer.Enabled := false;
  if Apply and (ItemIndex > -1) then
    case FMode of
      cmIdentifers: ReplaceWord(SubStr(FItems[ItemIndex], 0, FSeparator));
      cmTemplates: ReplaceWord(SubStr(FItems[ItemIndex], 2, FSeparator));
    end;
end;

procedure TCompletion.OnTimer(Sender: TObject);
begin
  DropDown(FDefMode, false);
end;

procedure TCompletion.ClearAutoChangeList;
var i: integer;
begin
  for i := 0 to FAutoChangeList.Count - 1 do
    Dispose(FAutoChangeList[i]);
  FAutoChangeList.Clear;
end;

procedure TCompletion.UpdateAutoChange;
begin
  AutoChangeChanged(FAutoChange);
end;

procedure TCompletion.AutoChangeChanged(Sender: TObject);
  procedure AddAutoChangeWord(const OldWord, NewWord: string);
  var ACW: PAutoChangeWord;
  begin
    if OldWord <> '' then
    begin
      New(ACW);
      ACW.OldWord := FMemoEx.DoChangeCase(OldWord, RA_CASE_CONVERT_LOWER);
      ACW.NewWord := NewWord;
      FAutoChangeList.Add(ACW);
    end;
  end;
var i: integer;
begin
  ClearAutoChangeList;
  for i := 0 to FAutoChange.Count - 1 do
    AddAutoChangeWord(SubStr(FAutoChange.Strings[i], 0, FSeparator),
                      SubStr(FAutoChange.Strings[i], 1, FSeparator));
  FAutoChangeList.Sort(AutoChangeCompare);
end;

procedure TCompletion.SetStrings(index: integer; AValue: TStrings);
begin
  case index of
    0: FIdentifers.Assign(AValue);
    1: FTemplates.Assign(AValue);
    2: FAutoChange.Assign(AValue);
  end;
end;

function TCompletion.GetItemIndex: integer;
begin
  Result := FItemIndex;
  if FVisible then
    Result := FPopupList.ItemIndex;
end;

procedure TCompletion.SetItemIndex(AValue: integer);
begin
  FItemIndex := AValue;
  if FVisible then
    FPopupList.ItemIndex := FItemIndex;
end;

function TCompletion.GetInterval: cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TCompletion.SetInterval(AValue: cardinal);
begin
  FTimer.Interval := AValue;
end;


constructor TMemoExCompletionList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Left := -1000;
  Visible := false;
  TabStop := false;
  ParentFont := false;
  Parent := Owner as TCustomMemoEx;
  Ctl3D := false;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := false;
  FTimer.Interval := 200;
  FTimer.OnTimer := OnTimer;
  Style := lbOwnerDrawFixed;
  ItemHeight := 13;
  //  HintWindow := THintWindow.Create(Self);
end;

destructor TMemoExCompletionList.Destroy;
begin
  FTimer.Free;
  //  HintWindow.Free;
  inherited Destroy;
end;

procedure TMemoExCompletionList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style {or WS_POPUP} or WS_BORDER;
    ExStyle := ExStyle or WS_EX_TOOLWINDOW;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;
end;

procedure TMemoExCompletionList.CreateWnd;
begin
  inherited CreateWnd;
  if not (csDesigning in ComponentState) then
    Windows.SetParent(Handle, 0);
  //  CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0); {??}
end;

procedure TMemoExCompletionList.DestroyWnd;
begin
  inherited DestroyWnd;
  //  HintWindow.ReleaseHandle;
end;

procedure TMemoExCompletionList.MouseMove(Shift: TShiftState; X, Y: integer);
var
  F: integer;
begin
  YY := Y;
  F := ItemAtPos(Point(X, Y), true);
  if KeyPressed(VK_LBUTTON) then
  begin
    F := ItemAtPos(Point(X, Y), true);
    if F > -1 then ItemIndex := F;
    FTimer.Enabled := (Y < 0) or (Y > ClientHeight);
    if (Y < -ItemHeight) or (Y > ClientHeight + ItemHeight) then
      FTimer.Interval := 50
    else
      FTimer.Interval := 200;
  end;
  if (F > -1) and not FTimer.Enabled then
  begin
    //Application.CancelHint;
   // Hint := Items[F];
  //  HintWindow.ActivateHint(Bounds(ClientOrigin.X + X, ClientOrigin.Y + Y, 300, ItemHeight), Items[F]);
  end;
end;

procedure TMemoExCompletionList.MouseDown(Button: TMouseButton; Shift:
  TShiftState; X, Y: integer);
var
  F: integer;
begin
  MouseCapture := true;
  F := ItemAtPos(Point(X, Y), true);
  if F > -1 then ItemIndex := F;
end;

procedure TMemoExCompletionList.MouseUp(Button: TMouseButton; Shift:
  TShiftState; X, Y: integer);
begin
  MouseCapture := false;
  (Owner as TCustomMemoEx).FCompletion.CloseUp(
    (Button = mbLeft) and PtInRect(ClientRect, Point(X, Y)));
end;

procedure TMemoExCompletionList.OnTimer(Sender: TObject);
begin
  if (YY < 0) then
    Perform(WM_VSCROLL, SB_LINEUP, 0)
  else if (YY > ClientHeight) then
    Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

procedure TMemoExCompletionList.WMCancelMode(var Message: TMessage);
begin
  (Owner as TCustomMemoEx).FCompletion.CloseUp(false);
end;

procedure TMemoExCompletionList.CMHintShow(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TMemoExCompletionList.DrawItem(Index: integer; Rect: TRect; State:
  TOwnerDrawState);
var
  Offset, W: integer;
  S: string;
begin
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, Index, Rect, State)
  else
  begin
    Canvas.FillRect(Rect);
    Offset := 3;
    with (Owner as TCustomMemoEx).FCompletion do
      case FMode of
        cmIdentifers:
          Canvas.TextOut(Rect.Left + Offset, Rect.Top, SubStr(Items[Index], 1,
            Separator));
        cmTemplates:
          begin
            Canvas.TextOut(Rect.Left + Offset, Rect.Top, SubStr(Items[Index], 1,
              Separator));
            Canvas.Font.Style := [fsBold];
            S := SubStr(Items[Index], 0, Separator);
            W := Canvas.TextWidth(S);
            Canvas.TextOut(Rect.Right - 2 * Offset - W, Rect.Top, S);
          end;
      end;
  end;
end;

{$ENDIF MEMOEX_COMPLETION}

{$ENDIF MEMOEX_EDITOR}


{ TIEditReader support }

procedure TCustomMemoEx.ValidateEditBuffer;
begin
  if FPEditBuffer = nil then
  begin
    FEditBuffer := Lines.Text;
    FPEditBuffer := PChar(FEditBuffer);
    FEditBufferSize := Length(FEditBuffer);
  end;
end; { ValidateEditBuffer }

function TCustomMemoEx.GetText(Position: longint; Buffer: PChar;
  Count: longint): longint;
begin
  ValidateEditBuffer;
  if Position <= FEditBufferSize then
  begin
    Result := Min(FEditBufferSize - Position, Count);
    Move(FPEditBuffer[Position], Buffer[0], Result);
  end
  else
    Result := 0;
end;

procedure TCustomMemoEx.SetWordWrap(Value: boolean);
var p, x,y: integer; // AB : don't loose position
begin
  if Value <> FWordWrap then begin
    p := PosFromCaret(CaretX,CaretY);
    FWordWrap := Value;
    FLines.Reformat;
    CantUndo;                     
    CaretFromPos(p,x,y);
    SetCaret(0,Y); // aller en dÈbut de ligne (sinon risque de bug)
  end;
end;

procedure TCustomMemoEx.SetStripInvisible(Value: boolean);
begin
  if Value <> FStripInvisible then
  begin
    FStripInvisible := Value;
    if FReadOnly then Invalidate;
  end;
end;

procedure TCustomMemoEx.FontChanged(Sender: TObject);
begin
  UpdateEditorSize;
  Invalidate;
end;

{
  OnAfterLoad event
}
function TCustomMemoEx.GetAfterLoad: TNotifyEvent;
begin
  Result := FLines.FOnAfterLoad;
end;

procedure TCustomMemoEx.SetAfterLoad(Value: TNotifyEvent);
begin
  FLines.FOnAfterLoad := Value;
end;

{
  OnBeforeSave event
}
function TCustomMemoEx.GetBeforeSave: TNotifyEvent;
begin
  Result := FLines.FOnBeforeSave;
end;

procedure TCustomMemoEx.SetBeforeSave(Value: TNotifyEvent);
begin
  FLines.FOnBeforeSave := Value;
end;

procedure TCustomMemoEx.SetSelectedText(Value: boolean);
begin
  if FSelected <> Value then begin
    FSelected := Value;
    SelectionChanged;
  end;
end;

function TCustomMemoEx.FindNext(const text: string; ignCase: boolean): boolean;
var X, Found, para, paraIndex: integer;
    UpText: string;
begin
  if ignCase then
    UpText := UpperCase(Text);
  SetUnSelected;
  X := FCaretX+1; // ignore chars for first string
  FLines.Index2ParaIndex(FCaretY,para,paraIndex);
  while para<FLines.FCount do // search in all paragraphs
  with FLines.FList[para] do begin
    while paraIndex<FCount do begin // search in this paragraph
      if ignCase then
        Found := PosEx(UpText,UpperCase(FStrings[paraIndex]),X) else
        Found := PosEx(text,FStrings[paraIndex],X);
      if Found>0 then begin
        SetSel(Found-1,FPreCount+paraIndex);
        SetSel(FSelStartX+length(text),FSelStartY);
        SetCaret(FSelEndX,FSelEndY);
        result := true;
        exit;
      end;
      X := 1;         // now whole lines are searched
      inc(paraIndex); // next line of this paragraph
    end;
    inc(para);      // next paragraph
    paraIndex := 0; // begin with first line
  end;
  result := false;  // not found
end;

procedure Register;
begin
  RegisterComponents('Standard', [TMemoEx]);
end;

procedure TCustomMemoEx.SetTopRow(const Value: integer);
begin
  SetLeftTop(0,Value);
end;

procedure TCustomMemoEx.DoContextPopup(MousePos: TPoint;
  var Handled: Boolean);
var aPopupMenu: TPopupMenu;
begin
  if InvalidPoint(MousePos) then begin
    // in case of menu from keyboard -> show popupmenu at caret position
    aPopupMenu := GetPopupMenu;
    if (aPopupMenu=nil) or not aPopupMenu.AutoPopup then
      exit;
    SendCancelMode(nil);
    aPopupMenu.PopupComponent := Self;
    with ClientToScreen(CalcCellRect(FCaretX-FLeftCol,FCaretY-FTopRow).TopLeft) do
      aPopupMenu.Popup(X,Y);
    Handled := true;
  end else
    inherited;
end;

initialization
  CF_MEMOEX := RegisterClipBoardFormat('CF_MEMOEX');
end.
