{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEdit.pas, released 2000-04-07.
The Original Code is based on mwCustomEdit.pas by Martin Waldenburg, part of
the mwEdit component suite.
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

$Id: SynEdit.pas,v 1.32.1 2012/19/09 10:50:00 CodehunterWorks Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
- Undo is buggy when dealing with Hard Tabs (when inserting text after EOL and
  when trimming).

-------------------------------------------------------------------------------}
//todo: remove SynEdit Clipboard Format?
//todo: in WordWrap mode, parse lines only once in PaintLines()
//todo: Remove checks for WordWrap. Must abstract the behaviour with the plugins instead.
//todo: Move WordWrap glyph to the WordWrap plugin.
//todo: remove fShowSpecChar variable
//todo: remove the several Undo block types?

{$IFNDEF QSYNEDIT}
unit SynEdit;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  {$IFDEF SYN_LINUX}
  Xlib,
  {$ENDIF}
  Qt,
  Types,
  QControls,
  QGraphics,
  QForms,
  QStdCtrls,
  QExtCtrls,
  QSynUnicode,
{$ELSE}
  Controls,
  Contnrs,
  Graphics,
  Forms,
  StdCtrls,
  ExtCtrls,
  Windows,
  Messages,
  {$IFDEF SYN_COMPILER_4_UP}
  StdActns,
  Dialogs,
  {$ENDIF}
  {$IFDEF SYN_COMPILER_7}
  Themes,
  {$ENDIF}
  {$IFDEF SYN_COMPILER_17_UP}
  UITypes,
  {$ENDIF}
  SynUnicode,
{$ENDIF}
{$IFDEF SYN_CLX}
  kTextDrawer,
  QSynEditTypes,
  QSynEditKeyConst,
  QSynEditMiscProcs,
  QSynEditMiscClasses,
  QSynEditTextBuffer,
  QSynEditKeyCmds,
  QSynEditHighlighter,
  QSynEditKbdHandler,
{$ELSE}
  Imm,
  SynTextDrawer,
  SynEditTypes,
  SynEditKeyConst,
  SynEditMiscProcs,
  SynEditMiscClasses,
  SynEditTextBuffer,
  SynEditKeyCmds,
  SynEditHighlighter,
  SynEditKbdHandler,
{$ENDIF}
{$IFDEF UNICODE}
  WideStrUtils,
{$ENDIF}
  Math,
  SysUtils,
  Classes;

const
{$IFNDEF SYN_COMPILER_3_UP}
   // not defined in all Delphi versions
  WM_MOUSEWHEEL = $020A;
{$ENDIF}

   // maximum scroll range
  MAX_SCROLL = 32767;

  // Max number of book/gutter marks returned from GetEditMarksForLine - that
  // really should be enough.
  MAX_MARKS = 16;

  SYNEDIT_CLIPBOARD_FORMAT = 'SynEdit Control Block Type';

var
  SynEditClipboardFormat: UINT;

type
	TBufferCoord = SynEditTypes.TBufferCoord;
	TDisplayCoord = SynEditTypes.TDisplayCoord;

{$IFDEF SYN_CLX}
  TSynBorderStyle = bsNone..bsSingle;
{$ELSE}
  TSynBorderStyle = TBorderStyle;
{$ENDIF}

  TSynReplaceAction = (raCancel, raSkip, raReplace, raReplaceAll);

  ESynEditError = class(ESynError);

  TDropFilesEvent = procedure(Sender: TObject; X, Y: Integer; AFiles: TUnicodeStrings)
    of object;

  THookedCommandEvent = procedure(Sender: TObject; AfterProcessing: Boolean;
    var Handled: Boolean; var Command: TSynEditorCommand; var AChar: WideChar;
    Data: pointer; HandlerData: pointer) of object;

  TPaintEvent = procedure(Sender: TObject; ACanvas: TCanvas) of object;

  TProcessCommandEvent = procedure(Sender: TObject;
    var Command: TSynEditorCommand; var AChar: WideChar; Data: pointer) of object;

  TReplaceTextEvent = procedure(Sender: TObject; const ASearch, AReplace:
    UnicodeString; Line, Column: Integer; var Action: TSynReplaceAction) of object;

  TSpecialLineColorsEvent = procedure(Sender: TObject; Line: Integer;
    var Special: Boolean; var FG, BG: TColor) of object;

  TTransientType = (ttBefore, ttAfter);
  TPaintTransient = procedure(Sender: TObject; Canvas: TCanvas;
    TransientType: TTransientType) of object;

  TScrollEvent = procedure(Sender: TObject; ScrollBar: TScrollBarKind) of object;

  TGutterGetTextEvent = procedure(Sender: TObject; aLine: Integer;
    var aText: UnicodeString) of object;

  TGutterPaintEvent = procedure(Sender: TObject; aLine: Integer;
    X, Y: Integer) of object;

  TSynEditCaretType = (ctVerticalLine, ctHorizontalLine, ctHalfBlock, ctBlock);

  TSynStateFlag = (sfCaretChanged, sfScrollbarChanged, sfLinesChanging,
    sfIgnoreNextChar, sfCaretVisible, sfDblClicked, sfPossibleGutterClick,
    sfWaitForDragging, sfInsideRedo, sfGutterDragging);

  TSynStateFlags = set of TSynStateFlag;

  TScrollHintFormat = (shfTopLineOnly, shfTopToBottom);

  TSynEditorOption = (
    eoAltSetsColumnMode,       //Holding down the Alt Key will put the selection mode into columnar format
    eoAutoIndent,              //Will indent the caret on new lines with the same amount of leading white space as the preceding line
    eoAutoSizeMaxScrollWidth,  //Automatically resizes the MaxScrollWidth property when inserting text
    eoDisableScrollArrows,     //Disables the scroll bar arrow buttons when you can't scroll in that direction any more
    eoDragDropEditing,         //Allows you to select a block of text and drag it within the document to another location
    eoDropFiles,               //Allows the editor accept OLE file drops
    eoEnhanceHomeKey,          //enhances home key positioning, similar to visual studio
    eoEnhanceEndKey,           //enhances End key positioning, similar to JDeveloper
    eoGroupUndo,               //When undoing/redoing actions, handle all continous changes of the same kind in one call instead undoing/redoing each command separately
    eoHalfPageScroll,          //When scrolling with page-up and page-down commands, only scroll a half page at a time
    eoHideShowScrollbars,      //if enabled, then the scrollbars will only show when necessary.  If you have ScrollPastEOL, then it the horizontal bar will always be there (it uses MaxLength instead)
    eoKeepCaretX,              //When moving through lines w/o Cursor Past EOL, keeps the X position of the cursor
    eoNoCaret,                 //Makes it so the caret is never visible
    eoNoSelection,             //Disables selecting text
    eoRightMouseMovesCursor,   //When clicking with the right mouse for a popup menu, move the cursor to that location
    eoScrollByOneLess,         //Forces scrolling to be one less
    eoScrollHintFollows,       //The scroll hint follows the mouse when scrolling vertically
    eoScrollPastEof,           //Allows the cursor to go past the end of file marker
    eoScrollPastEol,           //Allows the cursor to go past the last character into the white space at the end of a line
    eoShowScrollHint,          //Shows a hint of the visible line numbers when scrolling vertically
    eoShowSpecialChars,        //Shows the special Characters
    eoSmartTabDelete,          //similar to Smart Tabs, but when you delete characters
    eoSmartTabs,               //When tabbing, the cursor will go to the next non-white space character of the previous line
    eoSpecialLineDefaultFg,    //disables the foreground text color override when using the OnSpecialLineColor event
    eoTabIndent,               //When active <Tab> and <Shift><Tab> act as block indent, unindent when text is selected
    eoTabsToSpaces,            //Converts a tab character to a specified number of space characters
    eoTrimTrailingSpaces       //Spaces at the end of lines will be trimmed and not saved
    );

  TSynEditorOptions = set of TSynEditorOption;

  TSynFontSmoothMethod = (fsmNone, fsmAntiAlias, fsmClearType);

const
  SYNEDIT_DEFAULT_OPTIONS = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey,
    eoScrollPastEol, eoShowScrollHint, eoSmartTabs, eoTabsToSpaces,
    eoSmartTabDelete, eoGroupUndo];

{$IFNDEF SYN_CLX}
type
  TCreateParamsW = record
    Caption: PWideChar;
    Style: DWORD;
    ExStyle: DWORD;
    X, Y: Integer;
    Width, Height: Integer;
    WndParent: HWnd;
    Param: Pointer;
    WindowClass: TWndClassW;
    WinClassName: array[0..63] of WideChar;
    InternalCaption: UnicodeString;
  end;
{$ENDIF}

type
// use scAll to update a statusbar when another TCustomSynEdit got the focus
  TSynStatusChange = (scAll, scCaretX, scCaretY, scLeftChar, scTopLine,
    scInsertMode, scModified, scSelection, scReadOnly);
  TSynStatusChanges = set of TSynStatusChange;

  TContextHelpEvent = procedure(Sender: TObject; word: UnicodeString)
    of object;

  TStatusChangeEvent = procedure(Sender: TObject; Changes: TSynStatusChanges)
    of object;

  TMouseCursorEvent = procedure(Sender: TObject; const aLineCharPos: TBufferCoord;
    var aCursor: TCursor) of object;

  TCustomSynEdit = class;

  TSynEditMark = class
  protected
    fLine, fChar, fImage: Integer;
    fEdit: TCustomSynEdit;
    fVisible: Boolean;
    fInternalImage: Boolean;
    fBookmarkNum: Integer;
    function GetEdit: TCustomSynEdit; virtual;
    procedure SetChar(const Value: Integer); virtual;
    procedure SetImage(const Value: Integer); virtual;
    procedure SetLine(const Value: Integer); virtual;
    procedure SetVisible(const Value: Boolean);
    procedure SetInternalImage(const Value: Boolean);
    function GetIsBookmark: Boolean;
  public
    constructor Create(AOwner: TCustomSynEdit);
    property Line: Integer read fLine write SetLine;
    property Char: Integer read fChar write SetChar;
    property Edit: TCustomSynEdit read fEdit;
    property ImageIndex: Integer read fImage write SetImage;
    property BookmarkNumber: Integer read fBookmarkNum write fBookmarkNum;
    property Visible: Boolean read fVisible write SetVisible;
    property InternalImage: Boolean read fInternalImage write SetInternalImage;
    property IsBookmark: Boolean read GetIsBookmark;
  end;

  TPlaceMarkEvent = procedure(Sender: TObject; var Mark: TSynEditMark)
    of object;

  TSynEditMarks = array[1..MAX_MARKS] of TSynEditMark;

  { A list of mark objects. Each object cause a litle picture to be drawn in the gutter. }
  TSynEditMarkList = class(TObjectList)            // It makes more sence to derive from TObjectList,
  protected                                        // as it automatically frees its members
    fEdit: TCustomSynEdit;
    fOnChange: TNotifyEvent;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function GetItem(Index: Integer): TSynEditMark;
    procedure SetItem(Index: Integer; Item: TSynEditMark);
    property OwnsObjects;                          // This is to hide the inherited property,
  public                                           // because TSynEditMarkList always owns the marks
    constructor Create(AOwner: TCustomSynEdit);
    function First: TSynEditMark;
    function Last: TSynEditMark;
    function Extract(Item: TSynEditMark): TSynEditMark;
    procedure ClearLine(line: Integer);
    procedure GetMarksForLine(line: Integer; var Marks: TSynEditMarks);
    procedure Place(mark: TSynEditMark);
  public
    property Items[Index: Integer]: TSynEditMark read GetItem write SetItem; default;
    property Edit: TCustomSynEdit read fEdit;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGutterClickEvent = procedure(Sender: TObject; Button: TMouseButton;
    X, Y, Line: Integer; Mark: TSynEditMark) of object;

  // aIndex parameters of Line notifications are 0-based.
  // aRow parameter of GetRowLength() is 1-based.
  ISynEditBufferPlugin = interface
    // conversion methods
    function BufferToDisplayPos(const aPos: TBufferCoord): TDisplayCoord;
    function DisplayToBufferPos(const aPos: TDisplayCoord): TBufferCoord;
    function RowCount: Integer;
    function GetRowLength(aRow: Integer): Integer;
    // plugin notifications
    function LinesInserted(aIndex: Integer; aCount: Integer): Integer;
    function LinesDeleted(aIndex: Integer; aCount: Integer): Integer;
    function LinesPutted(aIndex: Integer; aCount: Integer): Integer;
    // font or size change
    procedure DisplayChanged;
    // pretty clear, heh?
    procedure Reset;
  end;

  TSynEditPlugin = class(TObject)
  private
    fOwner: TCustomSynEdit;
  protected
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: Integer); virtual;
    procedure PaintTransient(ACanvas: TCanvas; ATransientType: TTransientType); virtual;
    procedure LinesInserted(FirstLine, Count: Integer); virtual;
    procedure LinesDeleted(FirstLine, Count: Integer); virtual;
  protected
    property Editor: TCustomSynEdit read fOwner;
  public
    constructor Create(AOwner: TCustomSynEdit);
    destructor Destroy; override;
  end;

{$IFDEF SYN_COMPILER_6_UP}
  TCustomSynEditSearchNotFoundEvent = procedure(Sender: TObject;
    FindText: UnicodeString) of object;
{$ENDIF}

  TCustomSynEdit = class(TCustomControl)
  private
{$IFNDEF SYN_CLX}
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMCaptureChanged(var Msg: TMessage); message WM_CAPTURECHANGED;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure WMClear(var Msg: TMessage); message WM_CLEAR;
    procedure WMCopy(var Message: TMessage); message WM_COPY;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMGetText(var Msg: TWMGetText); message WM_GETTEXT;
    procedure WMGetTextLength(var Msg: TWMGetTextLength); message WM_GETTEXTLENGTH;
    procedure WMHScroll(var Msg: TWMScroll); message WM_HSCROLL;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMSetText(var Msg: TWMSetText); message WM_SETTEXT;
    procedure WMImeChar(var Msg: TMessage); message WM_IME_CHAR;
    procedure WMImeComposition(var Msg: TMessage); message WM_IME_COMPOSITION;
    procedure WMImeNotify(var Msg: TMessage); message WM_IME_NOTIFY;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMUndo(var Msg: TMessage); message WM_UNDO;
    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
{$ENDIF}
{$IFNDEF SYN_COMPILER_6_UP}
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
{$ENDIF}
  private
    fAlwaysShowCaret: Boolean;
    fBlockBegin: TBufferCoord;
    fBlockEnd: TBufferCoord;
    fCaretX: Integer;
    fLastCaretX: integer;
    fCaretY: Integer;
    fCharsInWindow: Integer;
    fCharWidth: Integer;
    fFontDummy: TFont;
    fFontSmoothing: TSynFontSmoothMethod;
    fInserting: Boolean;
    fLines: TUnicodeStrings;
    fOrigLines: TUnicodeStrings;
    fOrigUndoList: TSynEditUndoList;
    fOrigRedoList: TSynEditUndoList;
    fLinesInWindow: Integer;
    fLeftChar: Integer;
    fMaxScrollWidth: Integer;
    fPaintLock: Integer;
    fReadOnly: Boolean;
    fRightEdge: Integer;
    fRightEdgeColor: TColor;
    fScrollHintColor: TColor;
    fScrollHintFormat: TScrollHintFormat;
    FScrollBars: TScrollStyle;
    fTextHeight: Integer;
    fTextOffset: Integer;
    fTopLine: Integer;
    fHighlighter: TSynCustomHighlighter;
    fSelectedColor: TSynSelectedColor;
    fActiveLineColor: TColor;
    fUndoList: TSynEditUndoList;
    fRedoList: TSynEditUndoList;
    fBookMarks: array[0..9] of TSynEditMark; // these are just references, fMarkList is the owner
    fMouseDownX: Integer;
    fMouseDownY: Integer;
    fBookMarkOpt: TSynBookMarkOpt;
    fBorderStyle: TSynBorderStyle;
    fHideSelection: Boolean;
    fMouseWheelAccumulator: Integer;
    fOverwriteCaret: TSynEditCaretType;
    fInsertCaret: TSynEditCaretType;
    fCaretOffset: TPoint;
    fKeyStrokes: TSynEditKeyStrokes;
    fModified: Boolean;
    fMarkList: TSynEditMarkList;
    fExtraLineSpacing: Integer;
    fSelectionMode: TSynSelectionMode;
    fActiveSelectionMode: TSynSelectionMode; //mode of the active selection
    fWantReturns: Boolean;
    fWantTabs: Boolean;
    fWordWrapPlugin: ISynEditBufferPlugin;
    fWordWrapGlyph: TSynGlyph;
    fCaretAtEOL: Boolean; // used by wordwrap

    fGutter: TSynGutter;
    fTabWidth: Integer;
    fTextDrawer: TheTextDrawer;
    fInvalidateRect: TRect;
    fStateFlags: TSynStateFlags;
    fOptions: TSynEditorOptions;
    fStatusChanges: TSynStatusChanges;
    fLastKey: word;
    fLastShiftState: TShiftState;
    fSearchEngine: TSynEditSearchCustom;
    fHookedCommandHandlers: TObjectList;
    fKbdHandler: TSynEditKbdHandler;
    fFocusList: TList;
    fPlugins: TObjectList;
    fScrollTimer: TTimer;
    fScrollDeltaX, fScrollDeltaY: Integer;
    // event handlers
    fOnChange: TNotifyEvent;
    fOnClearMark: TPlaceMarkEvent;
    fOnCommandProcessed: TProcessCommandEvent;
    fOnDropFiles: TDropFilesEvent;
    fOnGutterClick: TGutterClickEvent;
    FOnKeyPressW: TKeyPressWEvent;
    fOnMouseCursor: TMouseCursorEvent;
    fOnPaint: TPaintEvent;
    fOnPlaceMark: TPlaceMarkEvent;
    fOnProcessCommand: TProcessCommandEvent;
    fOnProcessUserCommand: TProcessCommandEvent;
    fOnReplaceText: TReplaceTextEvent;
    fOnSpecialLineColors: TSpecialLineColorsEvent;
    fOnContextHelp: TContextHelpEvent;
    fOnPaintTransient: TPaintTransient;
    fOnScroll: TScrollEvent;
    fOnGutterGetText: TGutterGetTextEvent;
    fOnGutterPaint: TGutterPaintEvent;

    fOnStatusChange: TStatusChangeEvent;
    fShowSpecChar: Boolean;
    FPaintTransientLock: Integer;
    FIsScrolling: Boolean;

    fChainListCleared: TNotifyEvent;
    fChainListDeleted: TStringListChangeEvent;
    fChainListInserted: TStringListChangeEvent;
    fChainListPutted: TStringListChangeEvent;
    fChainLinesChanging: TNotifyEvent;
    fChainLinesChanged: TNotifyEvent;
    fChainedEditor: TCustomSynEdit;
    fChainUndoAdded: TNotifyEvent;
    fChainRedoAdded: TNotifyEvent;

    FAdditionalWordBreakChars: TSysCharSet;
    FAdditionalIdentChars: TSysCharSet;

{$IFDEF SYN_COMPILER_6_UP}
    fSearchNotFound: TCustomSynEditSearchNotFoundEvent;
    OnFindBeforeSearch: TNotifyEvent;
    OnReplaceBeforeSearch: TNotifyEvent;
    OnCloseBeforeSearch: TNotifyEvent;
    SelStartBeforeSearch: integer;
    SelLengthBeforeSearch: integer;
{$ENDIF}

{$IFNDEF SYN_CLX}
    FWindowProducedMessage: Boolean;
{$ENDIF}

{$IFDEF SYN_LINUX}
    FDeadKeysFixed: Boolean;
{$ENDIF}

{$IFDEF SYN_CLX}
    FHScrollBar : TSynEditScrollBar;
    FVScrollBar : TSynEditScrollBar;
    procedure ScrollEvent(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
{$ENDIF}

    procedure BookMarkOptionsChanged(Sender: TObject);
    procedure ComputeCaret(X, Y: Integer);
    procedure ComputeScroll(X, Y: Integer);
    procedure DoHomeKey(Selection:boolean);
    procedure DoEndKey(Selection: Boolean);
    procedure DoLinesDeleted(FirstLine, Count: integer);
    procedure DoLinesInserted(FirstLine, Count: integer);
    procedure DoShiftTabKey;
    procedure DoTabKey;
    procedure DoCaseChange(const Cmd : TSynEditorCommand);
    function FindHookedCmdEvent(AHandlerProc: THookedCommandEvent): integer;
    procedure SynFontChanged(Sender: TObject);
    function GetBlockBegin: TBufferCoord;
    function GetBlockEnd: TBufferCoord;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCaretXY: TBufferCoord;
    function GetDisplayX: Integer;
    function GetDisplayY: Integer;
    function GetDisplayXY: TDisplayCoord;
    function GetDisplayLineCount: Integer;
    function GetFont: TFont;
    function GetHookedCommandHandlersCount: Integer;
    function GetLineText: UnicodeString;
    function GetMaxUndo: Integer;
    function GetOptions: TSynEditorOptions;
    function GetSelAvail: Boolean;
    function GetSelTabBlock: Boolean;
    function GetSelTabLine: Boolean;
    function GetSelText: UnicodeString;
    function SynGetText: UnicodeString;
    function GetWordAtCursor: UnicodeString;
    function GetWordAtMouse: UnicodeString;
    function GetWordWrap: Boolean;
    procedure GutterChanged(Sender: TObject);
    function LeftSpaces(const Line: UnicodeString): Integer;
    function LeftSpacesEx(const Line: UnicodeString; WantTabs: Boolean): Integer;
    function GetLeftSpacing(CharCount: Integer; WantTabs: Boolean): UnicodeString;
    procedure LinesChanging(Sender: TObject);
    procedure MoveCaretAndSelection(const ptBefore, ptAfter: TBufferCoord;
      SelectionCommand: Boolean);
    procedure MoveCaretHorz(DX: Integer; SelectionCommand: Boolean);
    procedure MoveCaretVert(DY: Integer; SelectionCommand: Boolean);
    procedure PluginsAfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: Integer);
    procedure ReadAddedKeystrokes(Reader: TReader);
    procedure ReadRemovedKeystrokes(Reader: TReader);
    function ScanFrom(Index: Integer): Integer;
    procedure ScrollTimerHandler(Sender: TObject);
    procedure SelectedColorsChanged(Sender: TObject);
    procedure SetBlockBegin(Value: TBufferCoord);
    procedure SetBlockEnd(Value: TBufferCoord);
    procedure SetBorderStyle(Value: TSynBorderStyle);
    procedure SetCaretX(Value: Integer);
    procedure SetCaretY(Value: Integer);
    procedure InternalSetCaretX(Value: Integer);
    procedure InternalSetCaretY(Value: Integer);
    procedure SetInternalDisplayXY(const aPos: TDisplayCoord);
    procedure SetActiveLineColor(Value: TColor);
    procedure SetExtraLineSpacing(const Value: Integer);
    procedure SetFont(const Value: TFont);
    procedure SetGutter(const Value: TSynGutter);
    procedure SetGutterWidth(Value: Integer);
    procedure SetHideSelection(const Value: Boolean);
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure SetInsertCaret(const Value: TSynEditCaretType);
    procedure SetInsertMode(const Value: Boolean);
    procedure SetKeystrokes(const Value: TSynEditKeyStrokes);
    procedure SetLeftChar(Value: Integer);
    procedure SetLines(Value: TUnicodeStrings);
    procedure SetLineText(Value: UnicodeString);
    procedure SetMaxScrollWidth(Value: Integer);
    procedure SetMaxUndo(const Value: Integer);
    procedure SetModified(Value: Boolean);
    procedure SetOptions(Value: TSynEditorOptions);
    procedure SetOverwriteCaret(const Value: TSynEditCaretType);
    procedure SetRightEdge(Value: Integer);
    procedure SetRightEdgeColor(Value: TColor);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetSearchEngine(Value: TSynEditSearchCustom);
    procedure SetSelectionMode(const Value: TSynSelectionMode);
    procedure SetActiveSelectionMode(const Value: TSynSelectionMode);
    procedure SetSelTextExternal(const Value: UnicodeString);
    procedure SetTabWidth(Value: Integer);
    procedure SynSetText(const Value: UnicodeString);
    procedure SetTopLine(Value: Integer);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetWordWrapGlyph(const Value: TSynGlyph);
    procedure WordWrapGlyphChange(Sender: TObject);
    procedure SizeOrFontChanged(bFont: boolean);
    procedure ProperSetLine(ALine: Integer; const ALineText: UnicodeString);
    procedure UpdateModifiedStatus;
    procedure UndoRedoAdded(Sender: TObject);
    procedure UpdateLastCaretX;
    procedure UpdateScrollBars;
    procedure WriteAddedKeystrokes(Writer: TWriter);
    procedure WriteRemovedKeystrokes(Writer: TWriter);
    procedure SetAdditionalIdentChars(const Value: TSysCharSet);
    procedure SetAdditionalWordBreakChars(const Value: TSysCharSet);

{$IFDEF SYN_COMPILER_6_UP}
    procedure DoSearchFindFirstExecute(Action: TSearchFindFirst);
    procedure DoSearchFindExecute(Action: TSearchFind);
    procedure DoSearchReplaceExecute(Action: TSearchReplace);
    procedure DoSearchFindNextExecute(Action: TSearchFindNext);
    procedure FindDialogFindFirst(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    function SearchByFindDialog(FindDialog: TFindDialog) : bool;
    procedure FindDialogClose(Sender: TObject);
{$ENDIF}
  protected
    FIgnoreNextChar: Boolean;
    FCharCodeString: string;
{$IFDEF SYN_COMPILER_6_UP}
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
{$ENDIF}
{$IFDEF SYN_CLX}
    procedure Resize; override;
    function GetClientOrigin: TPoint; override;
    function GetClientRect: TRect; override;
    function WidgetFlags: Integer; override;
    procedure KeyString(var S: UnicodeString; var Handled: Boolean); override;
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: UnicodeString): Boolean; override;
{$ELSE}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure InvalidateRect(const aRect: TRect; aErase: Boolean); virtual;
{$ENDIF}
    procedure DblClick; override;
    procedure DecPaintLock;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoChange; virtual;
{$IFDEF SYN_CLX}
    procedure DoKeyPressW(var Key: WideChar);
{$ELSE}
    procedure DoKeyPressW(var Message: TWMKey);
{$ENDIF}
    procedure DragCanceled; override;
    procedure DragOver(Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    function GetReadOnly: boolean; virtual;
    procedure HighlighterAttrChanged(Sender: TObject);
    procedure IncPaintLock;
    procedure InitializeCaret;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyPressW(var Key: WideChar); virtual;
    procedure LinesChanged(Sender: TObject); virtual;
    procedure ListCleared(Sender: TObject);
    procedure ListDeleted(Sender: TObject; aIndex: Integer; aCount: Integer);
    procedure ListInserted(Sender: TObject; Index: Integer; aCount: Integer);
    procedure ListPutted(Sender: TObject; Index: Integer; aCount: Integer);
    //helper procs to chain list commands
    procedure ChainListCleared(Sender: TObject);
    procedure ChainListDeleted(Sender: TObject; aIndex: Integer; aCount: Integer);
    procedure ChainListInserted(Sender: TObject; aIndex: Integer; aCount: Integer);
    procedure ChainListPutted(Sender: TObject; aIndex: Integer; aCount: Integer);
    procedure ChainLinesChanging(Sender: TObject);
    procedure ChainLinesChanged(Sender: TObject);
    procedure ChainUndoRedoAdded(Sender: TObject);
    procedure ScanRanges;
    procedure Loaded; override;
    procedure MarkListChange(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure NotifyHookedCommandHandlers(AfterProcessing: Boolean;
      var Command: TSynEditorCommand; var AChar: WideChar; Data: pointer); virtual;
    procedure Paint; override;
    procedure PaintGutter(const AClip: TRect; const aFirstRow,
      aLastRow: Integer); virtual;
    procedure PaintTextLines(AClip: TRect; const aFirstRow, aLastRow,
      FirstCol, LastCol: Integer); virtual;
    procedure RecalcCharExtent;
    procedure RedoItem;
    procedure InternalSetCaretXY(const Value: TBufferCoord); virtual;
    procedure SetCaretXY(const Value: TBufferCoord); virtual;
    procedure SetCaretXYEx(CallEnsureCursorPos: Boolean; Value: TBufferCoord); virtual;
    procedure SetFontSmoothing(AValue: TSynFontSmoothMethod);
    procedure SetName(const Value: TComponentName); override;
    procedure SetReadOnly(Value: boolean); virtual;
    procedure SetWantReturns(Value: Boolean);
    procedure SetSelTextPrimitive(const Value: UnicodeString);
    procedure SetSelTextPrimitiveEx(PasteMode: TSynSelectionMode; Value: PWideChar;
      AddToUndoList: Boolean);
    procedure SetWantTabs(Value: Boolean);
    procedure StatusChanged(AChanges: TSynStatusChanges);
    // If the translations requires Data, memory will be allocated for it via a
    // GetMem call.  The client must call FreeMem on Data if it is not NIL.
    function TranslateKeyCode(Code: word; Shift: TShiftState;
      var Data: pointer): TSynEditorCommand;
    procedure UndoItem;
    procedure UpdateMouseCursor; virtual;
  protected
    fGutterWidth: Integer;
    fInternalImage: TSynInternalImage;
    fSingleLineMode: boolean;
    procedure HideCaret;
    procedure ShowCaret;
    procedure DoOnClearBookmark(var Mark: TSynEditMark); virtual;
    procedure DoOnCommandProcessed(Command: TSynEditorCommand; AChar: WideChar;
      Data: pointer); virtual;
    // no method DoOnDropFiles, intercept the WM_DROPFILES instead
    procedure DoOnGutterClick(Button: TMouseButton; X, Y: Integer); virtual;
    procedure DoOnPaint; virtual;
    procedure DoOnPaintTransientEx(TransientType: TTransientType; Lock: Boolean); virtual;
    procedure DoOnPaintTransient(TransientType: TTransientType); virtual;

    procedure DoOnPlaceMark(var Mark: TSynEditMark); virtual;
    procedure DoOnProcessCommand(var Command: TSynEditorCommand;
      var AChar: WideChar; Data: pointer); virtual;
    function DoOnReplaceText(const ASearch, AReplace: UnicodeString;
      Line, Column: Integer): TSynReplaceAction; virtual;
    function DoOnSpecialLineColors(Line: Integer;
      var Foreground, Background: TColor): Boolean; virtual;
    procedure DoOnStatusChange(Changes: TSynStatusChanges); virtual;
    function GetSelEnd: integer;
    function GetSelStart: integer;
    function GetSelLength: integer;
    procedure SetSelEnd(const Value: integer);
    procedure SetSelStart(const Value: integer);
    procedure SetSelLength(const Value: integer);
    procedure SetAlwaysShowCaret(const Value: Boolean);
    function ShrinkAtWideGlyphs(const S: UnicodeString; First: Integer;
      var CharCount: Integer): UnicodeString;
    procedure LinesHookChanged;
    property InternalCaretX: Integer write InternalSetCaretX;
    property InternalCaretY: Integer write InternalSetCaretY;
    property InternalCaretXY: TBufferCoord write InternalSetCaretXY;
    property FontSmoothing: TSynFontSmoothMethod read fFontSmoothing write SetFontSmoothing;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelEnd: Integer read GetSelEnd write SetSelEnd;
    property AlwaysShowCaret: Boolean read FAlwaysShowCaret
                                      write SetAlwaysShowCaret;
    procedure UpdateCaret;
{$IFDEF SYN_COMPILER_4_UP}
    procedure AddKey(Command: TSynEditorCommand; Key1: word; SS1: TShiftState;
      Key2: word = 0; SS2: TShiftState = []);
{$ELSE}
    procedure AddKey(Command: TSynEditorCommand; Key1: word; SS1: TShiftState;
      Key2: word; SS2: TShiftState);
{$ENDIF}
    procedure BeginUndoBlock;
    procedure BeginUpdate;
    function CaretInView: Boolean;
    function CharIndexToRowCol(Index: Integer): TBufferCoord;
    procedure Clear;
    procedure ClearAll;
    procedure ClearBookMark(BookMark: Integer);
    procedure ClearSelection;
    procedure CommandProcessor(Command: TSynEditorCommand; AChar: WideChar;
      Data: pointer); virtual;
    procedure ClearUndo;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure DoCopyToClipboard(const SText: UnicodeString);
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure EndUndoBlock;
    procedure EndUpdate;
    procedure EnsureCursorPosVisible;
    procedure EnsureCursorPosVisibleEx(ForceToMiddle: Boolean;
      EvenIfVisible: Boolean = False);
    procedure FindMatchingBracket; virtual;
    function GetMatchingBracket: TBufferCoord; virtual;
    function GetMatchingBracketEx(const APoint: TBufferCoord): TBufferCoord; virtual;
{$IFDEF SYN_COMPILER_4_UP}
    function ExecuteAction(Action: TBasicAction): Boolean; override;
{$ENDIF}
    procedure ExecuteCommand(Command: TSynEditorCommand; AChar: WideChar;
      Data: pointer); virtual;
    function ExpandAtWideGlyphs(const S: UnicodeString): UnicodeString;
    function GetBookMark(BookMark: Integer; var X, Y: Integer): Boolean;
    function GetHighlighterAttriAtRowCol(const XY: TBufferCoord; var Token: UnicodeString;
      var Attri: TSynHighlighterAttributes): Boolean;
    function GetHighlighterAttriAtRowColEx(const XY: TBufferCoord; var Token: UnicodeString;
      var TokenType, Start: Integer;
      var Attri: TSynHighlighterAttributes): boolean;
    function GetPositionOfMouse(out aPos: TBufferCoord): Boolean;
    function GetWordAtRowCol(XY: TBufferCoord): UnicodeString;
    procedure GotoBookMark(BookMark: Integer); virtual;
    procedure GotoLineAndCenter(ALine: Integer); virtual;
    function IsIdentChar(AChar: WideChar): Boolean; virtual;
    function IsWhiteChar(AChar: WideChar): Boolean; virtual;
    function IsWordBreakChar(AChar: WideChar): Boolean; virtual;

    // Codehunter patch: Make InsertBlock, DoBlockIndent, DoBlockUnindent public
    procedure InsertBlock(const BB, BE: TBufferCoord; ChangeStr: PWideChar; AddToUndoList: Boolean);
    // Codehunter patch: Added UnifiedSelection
    function UnifiedSelection: TBufferBlock;
    procedure DoBlockIndent;
    procedure DoBlockUnindent;

    procedure InvalidateGutter;
    procedure InvalidateGutterLine(aLine: integer);
    procedure InvalidateGutterLines(FirstLine, LastLine: integer);
    procedure InvalidateLine(Line: integer);
    procedure InvalidateLines(FirstLine, LastLine: integer);
    procedure InvalidateSelection;
    function IsBookmark(BookMark: Integer): Boolean;
    function IsPointInSelection(const Value: TBufferCoord): Boolean;
    procedure LockUndo;
    function BufferToDisplayPos(const p: TBufferCoord): TDisplayCoord;
    function DisplayToBufferPos(const p: TDisplayCoord): TBufferCoord;
    function LineToRow(aLine: Integer): Integer;
    function RowToLine(aRow: Integer): Integer;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure PasteFromClipboard;

    function NextWordPos: TBufferCoord; virtual;
    function NextWordPosEx(const XY: TBufferCoord): TBufferCoord; virtual;
    function WordStart: TBufferCoord; virtual;
    function WordStartEx(const XY: TBufferCoord): TBufferCoord; virtual;
    function WordEnd: TBufferCoord; virtual;
    function WordEndEx(const XY: TBufferCoord): TBufferCoord; virtual;
    function PrevWordPos: TBufferCoord; virtual;
    function PrevWordPosEx(const XY: TBufferCoord): TBufferCoord; virtual;

    function PixelsToRowColumn(aX, aY: Integer): TDisplayCoord;
    function PixelsToNearestRowColumn(aX, aY: Integer): TDisplayCoord;
    procedure Redo;
    procedure RegisterCommandHandler(const AHandlerProc: THookedCommandEvent;
      AHandlerData: pointer);
    function RowColumnToPixels(const RowCol: TDisplayCoord): TPoint;
    function RowColToCharIndex(RowCol: TBufferCoord): Integer;
    function SearchReplace(const ASearch, AReplace: UnicodeString;
      AOptions: TSynSearchOptions): Integer;
    procedure SelectAll;
    procedure SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
    procedure SetCaretAndSelection(const ptCaret, ptBefore, ptAfter: TBufferCoord);
    procedure SetDefaultKeystrokes; virtual;
    procedure SetSelWord;
    procedure SetWordBlock(Value: TBufferCoord);
    procedure Undo;
    procedure UnlockUndo;
    procedure UnregisterCommandHandler(AHandlerProc: THookedCommandEvent);
{$IFDEF SYN_COMPILER_4_UP}
    function UpdateAction(Action: TBasicAction): Boolean; override;
{$ENDIF}
    procedure SetFocus; override;

    procedure AddKeyUpHandler(aHandler: TKeyEvent);
    procedure RemoveKeyUpHandler(aHandler: TKeyEvent);
    procedure AddKeyDownHandler(aHandler: TKeyEvent);
    procedure RemoveKeyDownHandler(aHandler: TKeyEvent);
    procedure AddKeyPressHandler(aHandler: TKeyPressWEvent);
    procedure RemoveKeyPressHandler(aHandler: TKeyPressWEvent);
    procedure AddFocusControl(aControl: TWinControl);
    procedure RemoveFocusControl(aControl: TWinControl);
    procedure AddMouseDownHandler(aHandler: TMouseEvent);
    procedure RemoveMouseDownHandler(aHandler: TMouseEvent);
    procedure AddMouseUpHandler(aHandler: TMouseEvent);
    procedure RemoveMouseUpHandler(aHandler: TMouseEvent);
    procedure AddMouseCursorHandler(aHandler: TMouseCursorEvent);
    procedure RemoveMouseCursorHandler(aHandler: TMouseCursorEvent);

{$IFDEF SYN_CLX}
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; override;
{$ELSE}
    procedure WndProc(var Msg: TMessage); override;
{$ENDIF}
    procedure SetLinesPointer(ASynEdit: TCustomSynEdit);
    procedure RemoveLinesPointer;
    procedure HookTextBuffer(aBuffer: TSynEditStringList;
      aUndo, aRedo: TSynEditUndoList);
    procedure UnHookTextBuffer;
  public
    property AdditionalIdentChars: TSysCharSet read FAdditionalIdentChars write SetAdditionalIdentChars;
    property AdditionalWordBreakChars: TSysCharSet read FAdditionalWordBreakChars write SetAdditionalWordBreakChars;
    property BlockBegin: TBufferCoord read GetBlockBegin write SetBlockBegin;
    property BlockEnd: TBufferCoord read GetBlockEnd write SetBlockEnd;
    property CanPaste: Boolean read GetCanPaste;
    property CanRedo: Boolean read GetCanRedo;
    property CanUndo: Boolean read GetCanUndo;
    property CaretX: Integer read fCaretX write SetCaretX;
    property CaretY: Integer read fCaretY write SetCaretY;
    property CaretXY: TBufferCoord read GetCaretXY write SetCaretXY;
    property ActiveLineColor: TColor read fActiveLineColor
      write SetActiveLineColor default clNone;
    property DisplayX: Integer read GetDisplayX;
    property DisplayY: Integer read GetDisplayY;
    property DisplayXY: TDisplayCoord read GetDisplayXY;
    property DisplayLineCount: Integer read GetDisplayLineCount;
    property CharsInWindow: Integer read fCharsInWindow;
    property CharWidth: Integer read fCharWidth;
    property Color;
    property Font: TFont read GetFont write SetFont;
    property Highlighter: TSynCustomHighlighter
      read fHighlighter write SetHighlighter;
    property LeftChar: Integer read fLeftChar write SetLeftChar;
    property LineHeight: Integer read fTextHeight;
    property LinesInWindow: Integer read fLinesInWindow;
    property LineText: UnicodeString read GetLineText write SetLineText;
    property Lines: TUnicodeStrings read fLines write SetLines;
    property Marks: TSynEditMarkList read fMarkList;
    property MaxScrollWidth: Integer read fMaxScrollWidth write SetMaxScrollWidth
      default 1024;
    property Modified: Boolean read fModified write SetModified;
    property PaintLock: Integer read fPaintLock;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property SearchEngine: TSynEditSearchCustom read fSearchEngine write SetSearchEngine;
    property SelAvail: Boolean read GetSelAvail;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelTabBlock: Boolean read GetSelTabBlock;
    property SelTabLine: Boolean read GetSelTabLine;
    property SelText: UnicodeString read GetSelText write SetSelTextExternal;
    property StateFlags: TSynStateFlags read fStateFlags;
    property Text: UnicodeString read SynGetText write SynSetText;
    property TopLine: Integer read fTopLine write SetTopLine;
    property WordAtCursor: UnicodeString read GetWordAtCursor;
    property WordAtMouse: UnicodeString read GetWordAtMouse;
    property UndoList: TSynEditUndoList read fUndoList;
    property RedoList: TSynEditUndoList read fRedoList;
  public
    property OnProcessCommand: TProcessCommandEvent
      read FOnProcessCommand write FOnProcessCommand;

    property BookMarkOptions: TSynBookMarkOpt
      read fBookMarkOpt write fBookMarkOpt;
    property BorderStyle: TSynBorderStyle read FBorderStyle write SetBorderStyle
      default bsSingle;
    property ExtraLineSpacing: Integer
      read fExtraLineSpacing write SetExtraLineSpacing default 0;
    property Gutter: TSynGutter read fGutter write SetGutter;
    property HideSelection: Boolean read fHideSelection write SetHideSelection
      default False;
    property InsertCaret: TSynEditCaretType read FInsertCaret
      write SetInsertCaret default ctVerticalLine;
    property InsertMode: boolean read fInserting write SetInsertMode
      default true;
    property IsScrolling : Boolean read FIsScrolling;
    property Keystrokes: TSynEditKeyStrokes
      read FKeystrokes write SetKeystrokes stored False;
    property MaxUndo: Integer read GetMaxUndo write SetMaxUndo default 1024;
    property Options: TSynEditorOptions read GetOptions write SetOptions
      default SYNEDIT_DEFAULT_OPTIONS;
    property OverwriteCaret: TSynEditCaretType read FOverwriteCaret
      write SetOverwriteCaret default ctBlock;
    property RightEdge: Integer read fRightEdge write SetRightEdge default 80;
    property RightEdgeColor: TColor
      read fRightEdgeColor write SetRightEdgeColor default clSilver;
    property ScrollHintColor: TColor read fScrollHintColor
      write fScrollHintColor default clInfoBk;
    property ScrollHintFormat: TScrollHintFormat read fScrollHintFormat
      write fScrollHintFormat default shfTopLineOnly;
    property ScrollBars: TScrollStyle
      read FScrollBars write SetScrollBars default ssBoth;
    property SelectedColor: TSynSelectedColor
      read FSelectedColor write FSelectedColor;
    property SelectionMode: TSynSelectionMode
      read FSelectionMode write SetSelectionMode default smNormal;
    property ActiveSelectionMode: TSynSelectionMode read fActiveSelectionMode
      write SetActiveSelectionMode stored False;
    property TabWidth: integer read fTabWidth write SetTabWidth default 8;
    property WantReturns: boolean read fWantReturns write SetWantReturns default True;
    property WantTabs: boolean read fWantTabs write SetWantTabs default False;
    property WordWrap: boolean read GetWordWrap write SetWordWrap default False;
    property WordWrapGlyph: TSynGlyph read fWordWrapGlyph write SetWordWrapGlyph;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClearBookmark: TPlaceMarkEvent read fOnClearMark
      write fOnClearMark;
    property OnCommandProcessed: TProcessCommandEvent
      read fOnCommandProcessed write fOnCommandProcessed;
    property OnContextHelp: TContextHelpEvent
      read fOnContextHelp write fOnContextHelp;
    property OnDropFiles: TDropFilesEvent read fOnDropFiles write fOnDropFiles;
    property OnGutterClick: TGutterClickEvent
      read fOnGutterClick write fOnGutterClick;
    property OnGutterGetText: TGutterGetTextEvent read fOnGutterGetText
      write fOnGutterGetText;
    property OnGutterPaint: TGutterPaintEvent read fOnGutterPaint
      write fOnGutterPaint;
    property OnMouseCursor: TMouseCursorEvent read fOnMouseCursor
      write fOnMouseCursor;
    property OnKeyPress: TKeyPressWEvent read FOnKeyPressW write FOnKeyPressW;
    property OnPaint: TPaintEvent read fOnPaint write fOnPaint;
    property OnPlaceBookmark: TPlaceMarkEvent
      read FOnPlaceMark write FOnPlaceMark;
    property OnProcessUserCommand: TProcessCommandEvent
      read FOnProcessUserCommand write FOnProcessUserCommand;
    property OnReplaceText: TReplaceTextEvent read fOnReplaceText
      write fOnReplaceText;
    property OnSpecialLineColors: TSpecialLineColorsEvent
      read fOnSpecialLineColors write fOnSpecialLineColors;
    property OnStatusChange: TStatusChangeEvent
      read fOnStatusChange write fOnStatusChange;
    property OnPaintTransient: TPaintTransient
      read fOnPaintTransient write fOnPaintTransient;
    property OnScroll: TScrollEvent
      read fOnScroll write fOnScroll;
  published
    property Cursor default crIBeam;
{$IFDEF SYN_COMPILER_6_UP}
    property OnSearchNotFound: TCustomSynEditSearchNotFoundEvent
      read fSearchNotFound write fSearchNotFound;
{$ENDIF}
    property SingleLineMode: boolean read fSingleLineMode write fSingleLineMode;
  end;

  TSynEdit = class(TCustomSynEdit)
  published
    // inherited properties
    property Align;
{$IFDEF SYN_COMPILER_4_UP}
    property Anchors;
    property Constraints;
{$ENDIF}
    property Color;
    property ActiveLineColor;
{$IFDEF SYN_CLX}
{$ELSE}
    property Ctl3D;
    property ParentCtl3D;
{$ENDIF}
    property Enabled;
    property Font;
    property Height;
    property Name;
    property ParentColor default False;
    property ParentFont default False;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property Width;
    // inherited events
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
{$IFDEF SYN_CLX}
{$ELSE}
{$IFDEF SYN_COMPILER_4_UP}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
{$ENDIF}
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
    // TCustomSynEdit properties
    property BookMarkOptions;
    property BorderStyle;
    property ExtraLineSpacing;
    property Gutter;
    property HideSelection;
    property Highlighter;
{$IFNDEF SYN_CLX}
    property ImeMode;
    property ImeName;
{$ENDIF}
    property InsertCaret;
    property InsertMode;
    property Keystrokes;
    property Lines;
    property MaxScrollWidth;
    property MaxUndo;
    property Options;
    property OverwriteCaret;
    property ReadOnly;
    property RightEdge;
    property RightEdgeColor;
    property ScrollHintColor;
    property ScrollHintFormat;
    property ScrollBars;
    property SearchEngine;
    property SelectedColor;
    property SelectionMode;
    property TabWidth;
    property WantReturns;
    property WantTabs;
    property WordWrap;
    property WordWrapGlyph;
    // TCustomSynEdit events
    property OnChange;
    property OnClearBookmark;
    property OnCommandProcessed;
    property OnContextHelp;
    property OnDropFiles;
    property OnGutterClick;
    property OnGutterGetText;
    property OnGutterPaint;
    property OnMouseCursor;
    property OnPaint;
    property OnPlaceBookmark;
    property OnProcessCommand;
    property OnProcessUserCommand;
    property OnReplaceText;
    property OnScroll;
    property OnSpecialLineColors;
    property OnStatusChange;
    property OnPaintTransient;

    property FontSmoothing;
  end;

implementation

{$R SynEdit.res}

uses
{$IFDEF SYN_COMPILER_6_UP}
  Consts,
{$ENDIF}
{$IFDEF SYN_COMPILER_18_UP}
  AnsiStrings,
{$ENDIF}
{$IFDEF SYN_CLX}
  QStdActns,
  QClipbrd,
  QSynEditWordWrap,
  QSynEditStrConst;
{$ELSE}
  Clipbrd,
  ShellAPI,
  SynEditWordWrap,
  SynEditStrConst;
{$ENDIF}

{$IFDEF SYN_CLX}
const
  FrameWidth = 2; { the border width when BoderStyle = bsSingle (until we support TWidgetStyle...)  }
{$ENDIF}

function CeilOfIntDiv(Dividend: Cardinal; Divisor: Word): Word;
Var
  Remainder: Word;
begin
  DivMod(Dividend,  Divisor, Result, Remainder);
  if Remainder > 0 then
    Inc(Result);
end;

function TrimTrailingSpaces(const S: UnicodeString): UnicodeString;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and ((S[I] = #32) or (S[I] = #9)) do
    Dec(I);
  Result := Copy(S, 1, I);
end;

{ THookedCommandHandlerEntry }

type
  THookedCommandHandlerEntry = class(TObject)
  private
    fEvent: THookedCommandEvent;
    fData: pointer;
    constructor Create(AEvent: THookedCommandEvent; AData: pointer);
    function Equals(AEvent: THookedCommandEvent): Boolean; {$IFDEF UNICODE} reintroduce; {$ENDIF}
  end;

constructor THookedCommandHandlerEntry.Create(AEvent: THookedCommandEvent;
  AData: pointer);
begin
  inherited Create;
  fEvent := AEvent;
  fData := AData;
end;

function THookedCommandHandlerEntry.Equals(AEvent: THookedCommandEvent): Boolean;
begin
  with TMethod(fEvent) do
    Result := (Code = TMethod(AEvent).Code) and (Data = TMethod(AEvent).Data);
end;

{ TCustomSynEdit }

function TCustomSynEdit.PixelsToNearestRowColumn(aX, aY: Integer): TDisplayCoord;
// Result is in display coordinates
var
  f: Single;
begin
{$IFDEF SYN_CLX}
  with ClientRect.TopLeft do
  begin
    Dec(aX, X);
    Dec(aY, Y);
  end;
{$ENDIF}
  f := (aX - fGutterWidth - 2) / fCharWidth;
  // don't return a partially visible last line
  if aY >= fLinesInWindow * fTextHeight then
  begin
    aY := fLinesInWindow * fTextHeight - 1;
    if aY < 0 then
      aY := 0;
  end;
  Result.Column := Max(1, LeftChar + Round(f));
  Result.Row := Max(1, TopLine + (aY div fTextHeight));
end;

function TCustomSynEdit.PixelsToRowColumn(aX, aY: Integer): TDisplayCoord;
begin
{$IFDEF SYN_CLX}
  with ClientRect.TopLeft do
  begin
    Dec(aX, X);
    Dec(aY, Y);
  end;
{$ENDIF}
  Result.Column := Max(1, LeftChar + ((aX - fGutterWidth - 2) div fCharWidth));
  Result.Row := Max(1, TopLine + (aY div fTextHeight));
end;

function TCustomSynEdit.RowColumnToPixels(const RowCol: TDisplayCoord): TPoint;
begin
  Result.X := (RowCol.Column-1) * fCharWidth + fTextOffset;
  Result.Y := (RowCol.Row - fTopLine) * fTextHeight;
{$IFDEF SYN_CLX}
  with ClientRect.TopLeft do
  begin
    Inc(Result.X, X);
    Inc(Result.Y, Y);
  end;
{$ENDIF}
end;

procedure TCustomSynEdit.ComputeCaret(X, Y: Integer);
//X,Y are pixel coordinates
var
  vCaretNearestPos : TDisplayCoord;
begin
  vCaretNearestPos := PixelsToNearestRowColumn(X, Y);
  vCaretNearestPos.Row := MinMax(vCaretNearestPos.Row, 1, DisplayLineCount);
  SetInternalDisplayXY(vCaretNearestPos);
end;

procedure TCustomSynEdit.ComputeScroll(X, Y: Integer);
//X,Y are pixel coordinates
var
  iScrollBounds: TRect; { relative to the client area }
begin
  { don't scroll if dragging text from other control }
  if (not MouseCapture) and (not Dragging) then
  begin
    fScrollTimer.Enabled := False;
    Exit;
  end;

  iScrollBounds := Bounds(fGutterWidth, 0, fCharsInWindow * fCharWidth,
    fLinesInWindow * fTextHeight);
  if BorderStyle = bsNone then
    InflateRect(iScrollBounds, -2, -2);

  if X < iScrollBounds.Left then
    fScrollDeltaX := (X - iScrollBounds.Left) div fCharWidth - 1
  else if X >= iScrollBounds.Right then
    fScrollDeltaX := (X - iScrollBounds.Right) div fCharWidth + 1
  else
    fScrollDeltaX := 0;

  if Y < iScrollBounds.Top then
    fScrollDeltaY := (Y - iScrollBounds.Top) div fTextHeight - 1
  else if Y >= iScrollBounds.Bottom then
    fScrollDeltaY := (Y - iScrollBounds.Bottom) div fTextHeight + 1
  else
    fScrollDeltaY := 0;

  fScrollTimer.Enabled := (fScrollDeltaX <> 0) or (fScrollDeltaY <> 0);
end;

procedure TCustomSynEdit.DoCopyToClipboard(const SText: UnicodeString);
{$IFNDEF SYN_CLX}
var
  Mem: HGLOBAL;
  P: PByte;
  SLen: Integer;
  Success: Boolean;
  RetryCount: Integer;
{$ENDIF}
begin
  if SText = '' then Exit;
  SetClipboardText(SText);
{$IFDEF SYN_CLX}
end;
{$ELSE}
  SLen := Length(SText);
  // Open and Close are the only TClipboard methods we use because TClipboard
  // is very hard (impossible) to work with if you want to put more than one
  // format on it at a time.
  RetryCount := 0;
  while not Success do
  try
    Clipboard.Open;
    Success := True;
  except
    on Exception do begin
      Inc(RetryCount);
      if RetryCount < 3 then
        Sleep(RetryCount * 100)
      else
        raise;
    end;
  end;
  try
    // Copy it in our custom format so we know what kind of block it is.
    // That effects how it is pasted in.
    // This format is kept as ANSI to be compatible with programs using the
    // ANSI version of Synedit.
    Mem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE,
      sizeof(TSynSelectionMode) + SLen + 1);
    if Mem <> 0 then
    begin
      P := GlobalLock(Mem);
      try
        if P <> nil then
        begin
          // Our format:  TSynSelectionMode value followed by Ansi-text.
          PSynSelectionMode(P)^ := fActiveSelectionMode;
          inc(P, SizeOf(TSynSelectionMode));
          Move(PAnsiChar(AnsiString(SText))^, P^, SLen + 1);
          SetClipboardData(SynEditClipboardFormat, Mem);
        end;
      finally
        GlobalUnlock(Mem);
      end;
    end;
    // Don't free Mem!  It belongs to the clipboard now, and it will free it
    // when it is done with it.
  finally
    Clipboard.Close;
  end;
end;
{$ENDIF}

procedure TCustomSynEdit.CopyToClipboard;
var
  SText: UnicodeString;
  ChangeTrim: Boolean;
begin
  if SelAvail then
  begin
    ChangeTrim := (fActiveSelectionMode = smColumn) and (eoTrimTrailingSpaces in Options);
    try
      if ChangeTrim then
        Exclude(fOptions, eoTrimTrailingSpaces);
      SText := SelText;
    finally
      if ChangeTrim then
        Include(fOptions, eoTrimTrailingSpaces);
    end;
    DoCopyToClipboard(SText);
  end;
end;

procedure TCustomSynEdit.CutToClipboard;
begin
  if not ReadOnly and SelAvail then
  begin
    BeginUndoBlock;
    try
      DoCopyToClipboard(SelText);
      SelText := '';
    finally
      EndUndoBlock;
    end;
  end;
end;

constructor TCustomSynEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fLines := TSynEditStringList.Create(ExpandAtWideGlyphs);
  fOrigLines := fLines;
  with TSynEditStringList(fLines) do
  begin
    OnChange := LinesChanged;
    OnChanging := LinesChanging;
    OnCleared := ListCleared;
    OnDeleted := ListDeleted;
    OnInserted := ListInserted;
    OnPutted := ListPutted;
  end;
  fFontDummy := TFont.Create;
  fUndoList := TSynEditUndoList.Create;
  fUndoList.OnAddedUndo := UndoRedoAdded;
  fOrigUndoList := fUndoList;
  fRedoList := TSynEditUndoList.Create;
  fRedoList.OnAddedUndo := UndoRedoAdded;
  fOrigRedoList := fRedoList;
  fSingleLineMode := false;

{$IFDEF SYN_COMPILER_4_UP}
{$IFDEF SYN_CLX}
{$ELSE}
  DoubleBuffered := False;
{$ENDIF}
{$ENDIF}
  fActiveLineColor := clNone;
  fSelectedColor := TSynSelectedColor.Create;
  fSelectedColor.OnChange := SelectedColorsChanged;
  fBookMarkOpt := TSynBookMarkOpt.Create(Self);
  fBookMarkOpt.OnChange := BookMarkOptionsChanged;
// fRightEdge has to be set before FontChanged is called for the first time
  fRightEdge := 80;
  fGutter := TSynGutter.Create;
  fGutter.OnChange := GutterChanged;
  fGutterWidth := fGutter.Width;
  fWordWrapGlyph := TSynGlyph.Create(HINSTANCE, 'SynEditWrapped', clLime);
  fWordWrapGlyph.OnChange := WordWrapGlyphChange;
  fTextOffset := fGutterWidth + 2;
  ControlStyle := ControlStyle + [csOpaque, csSetCaption];
{$IFDEF SYN_COMPILER_7_UP}
  {$IFNDEF SYN_CLX}
    ControlStyle := ControlStyle + [csNeedsBorderPaint];
  {$ENDIF}
{$ENDIF}
  Height := 150;
  Width := 200;
  Cursor := crIBeam;
  Color := clWindow;
{$IFDEF SYN_WIN32}
  fFontDummy.Name := 'Courier New';
  fFontDummy.Size := 10;
{$ENDIF}
{$IFDEF SYN_KYLIX}
  fFontDummy.Name := 'adobe-courier';
  if fFontDummy.Name = 'adobe-courier' then
    fFontDummy.Size := 12
  else begin
    fFontDummy.Name := 'terminal';
    fFontDummy.Size := 14;
  end;
{$ENDIF}
{$IFDEF SYN_COMPILER_3_UP}
  fFontDummy.CharSet := DEFAULT_CHARSET;
{$ENDIF}
  fTextDrawer := TheTextDrawer.Create([fsBold], fFontDummy);
  Font.Assign(fFontDummy);
  Font.OnChange := SynFontChanged;
  ParentFont := False;
  ParentColor := False;
  TabStop := True;
  fInserting := True;
  fMaxScrollWidth := 1024;
  fScrollBars := ssBoth;
  fBorderStyle := bsSingle;
  fInsertCaret := ctVerticalLine;
  fOverwriteCaret := ctBlock;
  FSelectionMode := smNormal;
  fActiveSelectionMode := smNormal;
  fFocusList := TList.Create;
  fKbdHandler := TSynEditKbdHandler.Create;
  fKeystrokes := TSynEditKeyStrokes.Create(Self);
  fMarkList := TSynEditMarkList.Create(self);
  fMarkList.OnChange := MarkListChange;
  SetDefaultKeystrokes;
  fRightEdgeColor := clSilver;
  fWantReturns := True;
  fWantTabs := False;
  fTabWidth := 8;
  fLeftChar := 1;
  fTopLine := 1;
  fCaretX := 1;
  fLastCaretX := 1;
  fCaretY := 1;
  fBlockBegin.Char := 1;
  fBlockBegin.Line := 1;
  fBlockEnd := fBlockBegin;
  fOptions := SYNEDIT_DEFAULT_OPTIONS;
  fScrollTimer := TTimer.Create(Self);
  fScrollTimer.Enabled := False;
  fScrollTimer.Interval := 100;
  fScrollTimer.OnTimer := ScrollTimerHandler;

{$IFDEF SYN_CLX}
  InputKeys := [ikArrows, ikChars, ikReturns, ikEdit, ikNav, ikEsc];

  FHScrollBar := TSynEditScrollbar.Create(self);
  FHScrollBar.Kind := sbHorizontal;
  FHScrollBar.Height := CYHSCROLL;
  FHScrollBar.OnScroll := ScrollEvent;
  FVScrollBar := TSynEditScrollbar.Create(self);
  FVScrollBar.Kind := sbVertical;
  FVScrollBar.Width := CXVSCROLL;
  FVScrollBar.OnScroll := ScrollEvent;

  // Set parent after BOTH scrollbars are created.
  FHScrollBar.Parent := Self;
  FHScrollBar.Color := clScrollBar;
  FVScrollBar.Parent := Self;
  FVScrollBar.Color := clScrollBar;
{$ENDIF}
  fScrollHintColor := clInfoBk;
  fScrollHintFormat := shfTopLineOnly;

  SynFontChanged(nil);
end;

{$IFNDEF SYN_CLX}
procedure TCustomSynEdit.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
  // Clear WindowText to avoid it being used as Caption, or else window creation will
  // fail if it's bigger than 64KB. It's useless to set the Caption anyway.
  StrDispose(WindowText);
  WindowText := nil;
  inherited CreateParams(Params);
  with Params do
  begin
    WindowClass.Style := WindowClass.Style and not ClassStylesOff;
    Style := Style or BorderStyles[fBorderStyle] or WS_CLIPCHILDREN;

    if NewStyleControls and Ctl3D and (fBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;

{$IFNDEF UNICODE}
    if not (csDesigning in ComponentState) then
    begin
      // Necessary for unicode support, especially IME won't work else
      if Win32PlatformIsUnicode then
        WindowClass.lpfnWndProc := @DefWindowProcW;
    end;
{$ENDIF}
  end;
end;
{$ENDIF}

procedure TCustomSynEdit.DecPaintLock;
var
  vAuxPos: TDisplayCoord;
begin
  Assert(fPaintLock > 0);
  Dec(fPaintLock);
  if (fPaintLock = 0) and HandleAllocated then
  begin
    if sfScrollbarChanged in fStateFlags then
      UpdateScrollbars;
    // Locks the caret inside the visible area
    if WordWrap and ([scCaretX,scCaretY] * fStatusChanges <> []) then
    begin
      vAuxPos := DisplayXY;
      // This may happen in the last row of a line or in rows which length is
      // greater than CharsInWindow (Tabs and Spaces are allowed beyond
      // CharsInWindow while wrapping the lines)
      if (vAuxPos.Column > CharsInWindow +1) and (CharsInWindow > 0) then
      begin
        if fCaretAtEOL then
          fCaretAtEOL := False
        else
        begin
          if scCaretY in fStatusChanges then
          begin
            vAuxPos.Column := CharsInWindow + 1;
            fCaretX := DisplayToBufferPos(vAuxPos).Char;
            Include(fStatusChanges,scCaretX);
            UpdateLastCaretX;
          end;
        end;
        Include(fStateFlags, sfCaretChanged);
      end;
    end;
    if sfCaretChanged in fStateFlags then
      UpdateCaret;
    if fStatusChanges <> [] then
      DoOnStatusChange(fStatusChanges);
  end;
end;

destructor TCustomSynEdit.Destroy;
begin
  Highlighter := nil;
  if (fChainedEditor <> nil) or (fLines <> fOrigLines) then
    RemoveLinesPointer;

  inherited Destroy;

  // free listeners while other fields are still valid

  // do not use FreeAndNil, it first nils and then freey causing problems with
  // code accessing fHookedCommandHandlers while destruction
  fHookedCommandHandlers.Free;
  fHookedCommandHandlers := nil;
  // do not use FreeAndNil, it first nils and then frees causing problems with
  // code accessing fPlugins while destruction
  fPlugins.Free;
  fPlugins := nil;

  fMarkList.Free;
  fBookMarkOpt.Free;
  fKeyStrokes.Free;
  fKbdHandler.Free;
  fFocusList.Free;
  fSelectedColor.Free;
  fOrigUndoList.Free;
  fOrigRedoList.Free;
  fGutter.Free;
  fWordWrapGlyph.Free;
  fTextDrawer.Free;
  fInternalImage.Free;
  fFontDummy.Free;
  fOrigLines.Free;
end;

function TCustomSynEdit.GetBlockBegin: TBufferCoord;
begin
  if (fBlockEnd.Line < fBlockBegin.Line)
    or ((fBlockEnd.Line = fBlockBegin.Line) and (fBlockEnd.Char < fBlockBegin.Char))
  then
    Result := fBlockEnd
  else
    Result := fBlockBegin;
end;

function TCustomSynEdit.GetBlockEnd: TBufferCoord;
begin
  if (fBlockEnd.Line < fBlockBegin.Line)
    or ((fBlockEnd.Line = fBlockBegin.Line) and (fBlockEnd.Char < fBlockBegin.Char))
  then
    Result := fBlockBegin
  else
    Result := fBlockEnd;
end;

procedure TCustomSynEdit.SynFontChanged(Sender: TObject);
begin
  RecalcCharExtent;
  SizeOrFontChanged(True);
end;

function TCustomSynEdit.GetFont: TFont;
begin
  Result := inherited Font;
end;

function TCustomSynEdit.GetLineText: UnicodeString;
begin
  if (CaretY >= 1) and (CaretY <= Lines.Count) then
    Result := Lines[CaretY - 1]
  else
    Result := '';
end;

function TCustomSynEdit.GetSelAvail: Boolean;
begin
  Result := (fBlockBegin.Char <> fBlockEnd.Char) or
    ((fBlockBegin.Line <> fBlockEnd.Line) and (fActiveSelectionMode <> smColumn));
end;

function TCustomSynEdit.GetSelTabBlock: Boolean;
begin
  Result := (fBlockBegin.Line <> fBlockEnd.Line) and (fActiveSelectionMode <> smColumn);
end;

function TCustomSynEdit.GetSelTabLine: Boolean;
begin
  Result := (BlockBegin.Char <= 1) and (BlockEnd.Char > length(Lines[CaretY - 1])) and SelAvail;
end;

function TCustomSynEdit.GetSelText: UnicodeString;

  function CopyPadded(const S: UnicodeString; Index, Count: Integer): UnicodeString;
  var
    SrcLen: Integer;
    DstLen: Integer;
    i: Integer;
    P: PWideChar;
  begin
    SrcLen := Length(S);
    DstLen := Index + Count;
    if SrcLen >= DstLen then
      Result := Copy(S, Index, Count)
    else begin
      SetLength(Result, DstLen);
      P := PWideChar(Result);
      WStrCopy(P, PWideChar(Copy(S, Index, Count)));
      Inc(P, Length(S));
      for i := 0 to DstLen - Srclen - 1 do
        P[i] := #32;
    end;
  end;

  procedure CopyAndForward(const S: UnicodeString; Index, Count: Integer; var P:
    PWideChar);
  var
    pSrc: PWideChar;
    SrcLen: Integer;
    DstLen: Integer;
  begin
    SrcLen := Length(S);
    if (Index <= SrcLen) and (Count > 0) then
    begin
      Dec(Index);
      pSrc := PWideChar(S) + Index;
      DstLen := Min(SrcLen - Index, Count);
      Move(pSrc^, P^, DstLen * sizeof(WideChar));
      Inc(P, DstLen);
      P^ := #0;
    end;
  end;

  function CopyPaddedAndForward(const S: UnicodeString; Index, Count: Integer;
    var P: PWideChar): Integer;
  var
    OldP: PWideChar;
    Len, i: Integer;
  begin
    Result := 0;
    OldP := P;
    CopyAndForward(S, Index, Count, P);
    Len := Count - (P - OldP);
    if not (eoTrimTrailingSpaces in Options) then
    begin
      for i := 0 to Len - 1 do
        P[i] := #32;
      Inc(P, Len);
    end
    else
      Result:= Len;
  end;

var
  First, Last, TotalLen: Integer;
  ColFrom, ColTo: Integer;
  I: Integer;
  l, r: Integer;
  s: UnicodeString;
  P: PWideChar;
  cRow: Integer;
  vAuxLineChar: TBufferCoord;
  vAuxRowCol: TDisplayCoord;
  vTrimCount: Integer;
begin
  if not SelAvail then
    Result := ''
  else begin
    ColFrom := BlockBegin.Char;
    First := BlockBegin.Line - 1;
    //
    ColTo := BlockEnd.Char;
    Last := BlockEnd.Line - 1;
    //
    TotalLen := 0;
    case fActiveSelectionMode of
      smNormal:
        if (First = Last) then
          Result := Copy(Lines[First], ColFrom, ColTo - ColFrom)
        else begin
          // step1: calculate total length of result string
          TotalLen := Max(0, Length(Lines[First]) - ColFrom + 1);
          for i := First + 1 to Last - 1 do
            Inc(TotalLen, Length(Lines[i]));
          Inc(TotalLen, ColTo - 1);
          Inc(TotalLen, Length(SLineBreak) * (Last - First));
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PWideChar(Result);
          CopyAndForward(Lines[First], ColFrom, MaxInt, P);

          CopyAndForward(SLineBreak, 1, MaxInt, P);

          for i := First + 1 to Last - 1 do
          begin
            CopyAndForward(Lines[i], 1, MaxInt, P);
            CopyAndForward(SLineBreak, 1, MaxInt, P);
          end;
          CopyAndForward(Lines[Last], 1, ColTo - 1, P);
        end;
      smColumn:
        begin
          with BufferToDisplayPos(BlockBegin) do
          begin
            First := Row;
            ColFrom := Column;
          end;
          with BufferToDisplayPos(BlockEnd) do
          begin
            Last := Row;
            ColTo := Column;
          end;
          if ColFrom > ColTo then
            SwapInt(ColFrom, ColTo);
          // step1: pre-allocate string large enough for worst case
          TotalLen := ((ColTo - ColFrom) + Length(sLineBreak)) *
            (Last - First +1);
          SetLength(Result, TotalLen);
          P := PWideChar(Result);

          // step2: copy chunks to the pre-allocated string
          TotalLen := 0;
          for cRow := First to Last do
          begin
            vAuxRowCol.Row := cRow;
            vAuxRowCol.Column := ColFrom;
            vAuxLineChar := DisplayToBufferPos(vAuxRowCol);
            l := vAuxLineChar.Char;
            s := Lines[vAuxLineChar.Line - 1];
            vAuxRowCol.Column := ColTo;
            r := DisplayToBufferPos(vAuxRowCol).Char;

            vTrimCount := CopyPaddedAndForward(s, l, r - l, P);
            TotalLen := TotalLen + (r - l) - vTrimCount + Length(sLineBreak);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          SetLength(Result, TotalLen - Length(sLineBreak));
        end;
      smLine:
        begin
          // If block selection includes LastLine,
          // line break code(s) of the last line will not be added.
          // step1: calculate total length of result string
          for i := First to Last do
            Inc(TotalLen, Length(Lines[i]) + Length(SLineBreak));
          if Last = Lines.Count then
            Dec(TotalLen, Length(SLineBreak));
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PWideChar(Result);
          for i := First to Last - 1 do
          begin
            CopyAndForward(Lines[i], 1, MaxInt, P);
            CopyAndForward(SLineBreak, 1, MaxInt, P);
          end;
          CopyAndForward(Lines[Last], 1, MaxInt, P);
          if (Last + 1) < Lines.Count then
            CopyAndForward(SLineBreak, 1, MaxInt, P);
        end;
    end;
  end;
end;

function TCustomSynEdit.SynGetText: UnicodeString;
begin
  Result := Lines.Text;
end;

function TCustomSynEdit.GetWordAtCursor: UnicodeString;
begin
   Result:=GetWordAtRowCol(CaretXY);
end;

procedure TCustomSynEdit.HideCaret;
begin
  if sfCaretVisible in fStateFlags then
{$IFDEF SYN_CLX}
    kTextDrawer.HideCaret(Self);
{$ELSE}
    if Windows.HideCaret(Handle) then
{$ENDIF}
      Exclude(fStateFlags, sfCaretVisible);
end;

procedure TCustomSynEdit.IncPaintLock;
begin
  inc(fPaintLock);
end;

procedure TCustomSynEdit.InvalidateGutter;
begin
  InvalidateGutterLines(-1, -1);
end;

procedure TCustomSynEdit.InvalidateGutterLine(aLine: Integer);
begin
  if (aLine < 1) or (aLine > Lines.Count) then
    Exit;

  InvalidateGutterLines(aLine, aLine);
end;

procedure TCustomSynEdit.InvalidateGutterLines(FirstLine, LastLine: integer);
// note: FirstLine and LastLine don't need to be in correct order
var
  rcInval: TRect;
begin
  if Visible and HandleAllocated then
    if (FirstLine = -1) and (LastLine = -1) then
    begin
      rcInval := Rect(0, 0, fGutterWidth, ClientHeight);
{$IFDEF SYN_CLX}
      with GetClientRect do
        OffsetRect(rcInval, Left, Top);
{$ENDIF}
      if sfLinesChanging in fStateFlags then
        UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
      else
        InvalidateRect(rcInval, False);
    end
    else begin
      { find the visible lines first }
      if (LastLine < FirstLine) then
        SwapInt(LastLine, FirstLine);
      if WordWrap then
      begin
        FirstLine := LineToRow(FirstLine);
        if LastLine <= Lines.Count then
          LastLine := LineToRow(LastLine)
        else
          LastLine := MaxInt;
      end;
      FirstLine := Max(FirstLine, TopLine);
      LastLine := Min(LastLine, TopLine + LinesInWindow);
      { any line visible? }
      if (LastLine >= FirstLine) then
      begin
        rcInval := Rect(0, fTextHeight * (FirstLine - TopLine),
          fGutterWidth, fTextHeight * (LastLine - TopLine + 1));
{$IFDEF SYN_CLX}
        with GetClientRect do
          OffsetRect(rcInval, Left, Top);
{$ENDIF}
        if sfLinesChanging in fStateFlags then
          UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
        else
          InvalidateRect(rcInval, False);
      end;
    end;
end;

procedure TCustomSynEdit.InvalidateLines(FirstLine, LastLine: integer);
// note: FirstLine and LastLine don't need to be in correct order
var
  rcInval: TRect;
begin
  if Visible and HandleAllocated then
    if (FirstLine = -1) and (LastLine = -1) then
    begin
      rcInval := ClientRect;
      Inc(rcInval.Left, fGutterWidth);
      if sfLinesChanging in fStateFlags then
        UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
      else
        InvalidateRect(rcInval, False);
    end
    else begin
      FirstLine := Max(FirstLine,1);
      LastLine := Max(LastLine,1);
      { find the visible lines first }
      if (LastLine < FirstLine) then
        SwapInt(LastLine, FirstLine);

      if LastLine >= Lines.Count then
        LastLine := MaxInt; // paint empty space beyond last line

      if WordWrap then
      begin
        FirstLine := LineToRow(FirstLine);
        // Could avoid this conversion if (First = Last) and
        // (Length < CharsInWindow) but the dependency isn't worth IMO.
        if LastLine < Lines.Count then
          LastLine := LineToRow(LastLine + 1) - 1;
      end;

      // TopLine is in display coordinates, so FirstLine and LastLine must be
      // converted previously.
      FirstLine := Max(FirstLine, TopLine);
      LastLine := Min(LastLine, TopLine + LinesInWindow);

      { any line visible? }
      if (LastLine >= FirstLine) then
      begin
        rcInval := Rect(fGutterWidth, fTextHeight * (FirstLine - TopLine),
          ClientWidth, fTextHeight * (LastLine - TopLine + 1));
{$IFDEF SYN_CLX}
        with GetClientRect do
          OffsetRect(rcInval, Left, Top);
{$ENDIF}
        if sfLinesChanging in fStateFlags then
          UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
        else
          InvalidateRect(rcInval, False);
      end;
    end;
end;

procedure TCustomSynEdit.InvalidateSelection;
begin
  InvalidateLines(BlockBegin.Line, BlockEnd.Line);
end;

{$IFDEF SYN_COMPILER_5}
function TryStrToInt(const S: string; out Value: Integer): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;
{$ENDIF}

procedure TCustomSynEdit.KeyUp(var Key: Word; Shift: TShiftState);
{$IFDEF SYN_LINUX}
var
  Code: Byte;
{$ENDIF}
{$IFNDEF SYN_CLX}
var
  CharCode: Integer;
  KeyMsg: TWMKey;
{$ENDIF}
begin
  {$IFDEF SYN_LINUX}
  // uniform Keycode: key has the same value wether Shift is pressed or not
  if Key <= 255 then
  begin
    Code := XKeysymToKeycode(Xlib.PDisplay(QtDisplay), Key);
    Key := XKeycodeToKeysym(Xlib.PDisplay(QtDisplay), Code, 0);
    if AnsiChar(Key) in ['a'..'z'] then Key := Ord(UpCase(AnsiChar(Key)));
  end;
  {$ENDIF}

  {$IFNDEF SYN_CLX}
  if (ssAlt in Shift) and (Key >= VK_NUMPAD0) and (Key <= VK_NUMPAD9) then
    FCharCodeString := FCharCodeString + IntToStr(Key - VK_NUMPAD0);

  if Key = VK_MENU then
  begin
    if (FCharCodeString <> '') and TryStrToInt(FCharCodeString, CharCode) and
      (CharCode >= 256) and (CharCode <= 65535) then
    begin
      KeyMsg.Msg := WM_CHAR;
      KeyMsg.CharCode := CharCode;
      KeyMsg.Unused := 0;
      KeyMsg.KeyData := 0;
      DoKeyPressW(KeyMsg);
      FIgnoreNextChar := True;
    end;
    FCharCodeString := '';
  end;
  {$ENDIF}

  inherited;
  fKbdHandler.ExecuteKeyUp(Self, Key, Shift);
end;

procedure TCustomSynEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  Data: pointer;
  C: WideChar;
  Cmd: TSynEditorCommand;
  {$IFDEF SYN_LINUX}
  Code: Byte;
  {$ENDIF}
begin
  {$IFDEF SYN_LINUX}
  // uniform Keycode: key has the same value wether Shift is pressed or not
  if Key <= 255 then
  begin
    Code := XKeysymToKeycode(Xlib.PDisplay(QtDisplay), Key);
    Key := XKeycodeToKeysym(Xlib.PDisplay(QtDisplay), Code, 0);
    if AnsiChar(Key) in ['a'..'z'] then Key := Ord(UpCase(AnsiChar(Key)));
  end;
  {$ENDIF}
  inherited;
  fKbdHandler.ExecuteKeyDown(Self, Key, Shift);

  Data := nil;
  C := #0;
  try
    Cmd := TranslateKeyCode(Key, Shift, Data);
    if Cmd <> ecNone then begin
      Key := 0; // eat it.
      Include(fStateFlags, sfIgnoreNextChar);
      CommandProcessor(Cmd, C, Data);
    end
    else
      Exclude(fStateFlags, sfIgnoreNextChar);
  finally
    if Data <> nil then
      FreeMem(Data);
  end;
end;

procedure TCustomSynEdit.Loaded;
begin
  inherited Loaded;
  GutterChanged(Self);
  UpdateScrollBars;
end;

procedure TCustomSynEdit.KeyPress(var Key: Char);
{$IFDEF SYN_CLX}
var
  KeyW: WideChar;
{$ENDIF}
begin
{$IFDEF SYN_CLX}
  KeyW := WideChar(Key);
  DoKeyPressW(KeyW);
  if KeyW > High(AnsiChar) then
    Key := #0
  else
    Key := AnsiChar(KeyW);
{$ELSE}
  // for Windows, don't do anything here
{$ENDIF}
end;

{$IFDEF SYN_CLX}
procedure TCustomSynEdit.DoKeyPressW(var Key: WideChar);
begin
  if (csNoStdEvents in ControlStyle) then Exit;

  if (Key <> #0) and Assigned(FOnKeyPressW) then
    FOnKeyPressW(Self, Key);

  if WideChar(Key) <> #0 then
    KeyPressW(Key);
end;
{$ELSE}
type
  TAccessWinControl = class(TWinControl);

{.$MESSAGE 'Check what must be adapted in DoKeyPressW and related methods'}
procedure TCustomSynEdit.DoKeyPressW(var Message: TWMKey);
var
  Form: TCustomForm;
  Key: WideChar;
begin
  if FIgnoreNextChar then
  begin
    FIgnoreNextChar := False;
    Exit;
  end;

  Key := WideChar(Message.CharCode);

  Form := GetParentForm(Self);
  if (Form <> nil) and (Form <> TWinControl(Self)) and Form.KeyPreview and
    (Key <= High(AnsiChar)) and TAccessWinControl(Form).DoKeyPress(Message)
  then
    Exit;
  Key := WideChar(Message.CharCode);

  if (csNoStdEvents in ControlStyle) then Exit;

  if Assigned(FOnKeyPressW) then
    FOnKeyPressW(Self, Key);

  if WideChar(Key) <> #0 then
    KeyPressW(Key);
end;
{$ENDIF}

procedure TCustomSynEdit.KeyPressW(var Key: WideChar);
begin
  // don't fire the event if key is to be ignored
  if not (sfIgnoreNextChar in fStateFlags) then
  begin
    fKbdHandler.ExecuteKeyPress(Self, Key);
    CommandProcessor(ecChar, Key, nil);
  end
  else
    // don't ignore further keys
    Exclude(fStateFlags, sfIgnoreNextChar);
end;

function TCustomSynEdit.LeftSpaces(const Line: UnicodeString): Integer;
begin
  Result := LeftSpacesEx(Line, False);
end;

function TCustomSynEdit.LeftSpacesEx(const Line: UnicodeString; WantTabs: Boolean): Integer;
var
  p: PWideChar;
begin
  p := PWideChar(Line);
  if Assigned(p) and (eoAutoIndent in fOptions) then
  begin
    Result := 0;
    while (p^ >= #1) and (p^ <= #32) do
    begin
      if (p^ = #9) and WantTabs then
        Inc(Result, TabWidth)
      else
        Inc(Result);
      Inc(p);
    end;
  end
  else
    Result := 0;
end;

function TCustomSynEdit.GetLeftSpacing(CharCount: Integer; WantTabs: Boolean): UnicodeString;
begin
  if WantTabs and not(eoTabsToSpaces in Options) and (CharCount >= TabWidth) then
    Result := UnicodeStringOfChar(#9, CharCount div TabWidth) +
      UnicodeStringOfChar(#32, CharCount mod TabWidth)
  else
    Result := UnicodeStringOfChar(#32, CharCount);
end;

procedure TCustomSynEdit.LinesChanging(Sender: TObject);
begin
  Include(fStateFlags, sfLinesChanging);
end;

procedure TCustomSynEdit.LinesChanged(Sender: TObject);
var
  vOldMode: TSynSelectionMode;
begin
  Exclude(fStateFlags, sfLinesChanging);
  if HandleAllocated then
  begin
    UpdateScrollBars;
    vOldMode := fActiveSelectionMode;
    SetBlockBegin(CaretXY);
    fActiveSelectionMode := vOldMode;
    InvalidateRect(fInvalidateRect, False);
    FillChar(fInvalidateRect, SizeOf(TRect), 0);
    if fGutter.ShowLineNumbers and fGutter.AutoSize then
      fGutter.AutoSizeDigitCount(Lines.Count);
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;
  end;
end;

procedure TCustomSynEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  bWasSel: Boolean;
  bStartDrag: Boolean;
  TmpBegin, TmpEnd: TBufferCoord;
begin
  {$IFDEF SYN_CLX}
  if not PtInRect(GetClientRect, Point(X,Y)) then
    Exit;
  {$ENDIF}

  TmpBegin := FBlockBegin;
  TmpEnd := FBlockEnd;

  bWasSel := False;
  bStartDrag := False;
  if Button = mbLeft then
  begin
    if SelAvail then
    begin
      //remember selection state, as it will be cleared later
      bWasSel := True;
      fMouseDownX := X;
      fMouseDownY := Y;
    end;
  end;

  inherited MouseDown(Button, Shift, X, Y);

  if (Button = mbLeft) and (ssDouble in Shift) then Exit;

  fKbdHandler.ExecuteMouseDown(Self, Button, Shift, X, Y);

  if (Button in [mbLeft, mbRight]) then
  begin
    if Button = mbRight then
    begin
      if (eoRightMouseMovesCursor in Options) and
         (SelAvail and not IsPointInSelection(DisplayToBufferPos(PixelsToRowColumn(X, Y)))
         or not SelAvail) then
      begin
        InvalidateSelection;
        FBlockEnd := FBlockBegin;
        ComputeCaret(X, Y);
      end
      else
        Exit;
    end
    else
      ComputeCaret(X, Y);
  end;

  if Button = mbLeft then
  begin
    //I couldn't track down why, but sometimes (and definately not all the time)
    //the block positioning is lost.  This makes sure that the block is
    //maintained in case they started a drag operation on the block
    FBlockBegin := TmpBegin;
    FBlockEnd := TmpEnd;

    MouseCapture := True;
    //if mousedown occurred in selected block begin drag operation
    Exclude(fStateFlags, sfWaitForDragging);
    if bWasSel and (eoDragDropEditing in fOptions) and (X >= fGutterWidth + 2)
      and (SelectionMode = smNormal) and IsPointInSelection(DisplayToBufferPos(PixelsToRowColumn(X, Y))) then
    begin
      bStartDrag := True
    end;
  end;

  if (Button = mbLeft) and bStartDrag then
    Include(fStateFlags, sfWaitForDragging)
  else
  begin
    if not (sfDblClicked in fStateFlags) then
    begin
      if ssShift in Shift then
        //BlockBegin and BlockEnd are restored to their original position in the
        //code from above and SetBlockEnd will take care of proper invalidation
        SetBlockEnd(CaretXY)
      else
      begin
        if (eoAltSetsColumnMode in Options) and (fActiveSelectionMode <> smLine) then
        begin
          if ssAlt in Shift then
            SelectionMode := smColumn
          else
            SelectionMode := smNormal;
        end;
        //Selection mode must be set before calling SetBlockBegin
        SetBlockBegin(CaretXY);
      end;
    end;
  end;

  if (X < fGutterWidth) then
    Include(fStateFlags, sfPossibleGutterClick);
  if (sfPossibleGutterClick in fStateFlags) and (Button = mbRight) then
  begin
    DoOnGutterClick(Button, X, Y)
  end;

  SetFocus;
{$IFNDEF SYN_CLX}
  Windows.SetFocus(Handle);
{$ENDIF}
end;

procedure TCustomSynEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TDisplayCoord;
begin
{$IFDEF SYN_CLX}
  if not InDragDropOperation then
    UpdateMouseCursor;
{$ENDIF}
  inherited MouseMove(Shift, x, y);
  if MouseCapture and (sfWaitForDragging in fStateFlags) then
  begin
    if (Abs(fMouseDownX - X) >= GetSystemMetrics(SM_CXDRAG))
      or (Abs(fMouseDownY - Y) >= GetSystemMetrics(SM_CYDRAG)) then
    begin
      Exclude(fStateFlags, sfWaitForDragging);
      BeginDrag(False);
{$IFDEF SYN_CLX}
      MouseCapture := False;
{$ENDIF}
    end;
  end
  else if (ssLeft in Shift) and MouseCapture then
  begin
    // should we begin scrolling?
    ComputeScroll(X, Y);
    { compute new caret }
    P := PixelsToNearestRowColumn(X, Y);
    P.Row := MinMax(P.Row, 1, DisplayLineCount);
    if fScrollDeltaX <> 0 then
      P.Column := DisplayX;
    if fScrollDeltaY <> 0 then
      P.Row := DisplayY;
    InternalCaretXY := DisplayToBufferPos(P);
    BlockEnd := CaretXY;
    if (sfPossibleGutterClick in fStateFlags) and (FBlockBegin.Line <> CaretXY.Line) then
      Include(fStateFlags, sfGutterDragging);
  end;
end;

procedure TCustomSynEdit.ScrollTimerHandler(Sender: TObject);
var
  iMousePos: TPoint;
  C: TDisplayCoord;
  X, Y: Integer;
  vCaret: TBufferCoord;
begin
  GetCursorPos( iMousePos );
  iMousePos := ScreenToClient( iMousePos );
  C := PixelsToRowColumn( iMousePos.X, iMousePos.Y );
  C.Row := MinMax(C.Row, 1, DisplayLineCount);
  if fScrollDeltaX <> 0 then
  begin
    LeftChar := LeftChar + fScrollDeltaX;
    X := LeftChar;
    if fScrollDeltaX > 0 then  // scrolling right?
      Inc(X, CharsInWindow);
    C.Column := X;
  end;
  if fScrollDeltaY <> 0 then
  begin
{$IFDEF SYN_CLX}
    if ssShift in Application.KeyState then
{$ELSE}
    if GetKeyState(SYNEDIT_SHIFT) < 0 then
{$ENDIF}
      TopLine := TopLine + fScrollDeltaY * LinesInWindow
    else
      TopLine := TopLine + fScrollDeltaY;
    Y := TopLine;
    if fScrollDeltaY > 0 then  // scrolling down?
      Inc(Y, LinesInWindow - 1);
    C.Row := MinMax(Y, 1, DisplayLineCount);
  end;
  vCaret := DisplayToBufferPos(C);
  if (CaretX <> vCaret.Char) or (CaretY <> vCaret.Line) then
  begin
    // changes to line / column in one go
    IncPaintLock;
    try
      InternalCaretXY := vCaret;
      // if MouseCapture is True we're changing selection. otherwise we're dragging
      if MouseCapture then
        SetBlockEnd(CaretXY);
    finally
      DecPaintLock;
    end;
  end;
  ComputeScroll(iMousePos.x, iMousePos.y);
end;

procedure TCustomSynEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  fKbdHandler.ExecuteMouseUp(Self, Button, Shift, X, Y);

  fScrollTimer.Enabled := False;
  if (Button = mbRight) and (Shift = [ssRight]) and Assigned(PopupMenu) then
    exit;
  MouseCapture := False;
  if (sfPossibleGutterClick in fStateFlags) and (X < fGutterWidth) and (Button <> mbRight) then
  begin
    DoOnGutterClick(Button, X, Y)
  end
  else if fStateFlags * [sfDblClicked, sfWaitForDragging] = [sfWaitForDragging] then
  begin
    ComputeCaret(X, Y);
    if not(ssShift in Shift) then
      SetBlockBegin(CaretXY);
    SetBlockEnd(CaretXY);
    Exclude(fStateFlags, sfWaitForDragging);
  end;
  Exclude(fStateFlags, sfDblClicked);
  Exclude(fStateFlags, sfPossibleGutterClick);
  Exclude(fStateFlags, sfGutterDragging);
end;

procedure TCustomSynEdit.DoOnGutterClick(Button: TMouseButton; X, Y: Integer);
var
  i     : Integer;
  offs  : Integer;
  line  : Integer;
  allmrk: TSynEditMarks;
  mark  : TSynEditMark;
begin
  if Assigned(fOnGutterClick) then
  begin
    line := DisplayToBufferPos(PixelsToRowColumn(X,Y)).Line;
    if line <= Lines.Count then
    begin
      Marks.GetMarksForLine(line, allmrk);
      offs := 0;
      mark := nil;
      for i := 1 to MAX_MARKS do
      begin
        if assigned(allmrk[i]) then
        begin
          Inc(offs, BookMarkOptions.XOffset);
          if X < offs then
          begin
            mark := allmrk[i];
            break;
          end;
        end;
      end; //for
      fOnGutterClick(Self, Button, X, Y, line, mark);
    end;
  end;
end;

procedure TCustomSynEdit.Paint;
var
  rcClip, rcDraw: TRect;
  nL1, nL2, nC1, nC2: Integer;
{$IFDEF SYN_CLX}
  iRestoreViewPort: Boolean;
  iClientRect: TRect;
  iClientRegion: QRegionH;
  iClip: QRegionH;
{$ENDIF}
begin
{$IFDEF SYN_CLX}
  { draws the lower-right corner of the scrollbars }
  if FHScrollBar.Visible and FVScrollBar.Visible then
  begin
    Canvas.Brush.Color := FHScrollBar.Color;
    Canvas.FillRect(Bounds(FVScrollBar.Left, FHScrollBar.Top,
      FVScrollBar.Width, FHScrollBar.Height));
  end;
  { validates the NC area }
  iClientRect := GetClientRect;
  iClientRegion := QRegion_create(@iClientRect, QRegionRegionType_Rectangle);
  iClip := QPainter_clipRegion(Canvas.Handle);
  QRegion_intersect(iClip, iClip, iClientRegion);
  QRegion_destroy(iClientRegion);
  if BorderStyle <> bsNone then
  begin
    { draws the border }
    iClientRect := Rect( 0, 0, Width, Height );
    QClxDrawUtil_DrawWinPanel(Canvas.Handle, @iClientRect,
      Palette.ColorGroup(cgActive), True, QBrushH(0));
    { sets transformation to ignore NC area }
    OffsetRect(iClientRect, FrameWidth, FrameWidth);
    QPainter_setViewport(Canvas.Handle, @iClientRect);
    iRestoreViewPort := True;
  end
  else
    iRestoreViewPort := False;
  { Compute the invalidated rect. }
  rcClip := Canvas.ClipRect;
  OffsetRect(rcClip, - iClientRect.Left, - iClientRect.Top);
{$ELSE}
  // Get the invalidated rect. Compute the invalid area in lines / columns.
  rcClip := Canvas.ClipRect;
{$ENDIF}
  // columns
  nC1 := LeftChar;
  if (rcClip.Left > fGutterWidth + 2) then
    Inc(nC1, (rcClip.Left - fGutterWidth - 2) div CharWidth);
  nC2 := LeftChar +
    (rcClip.Right - fGutterWidth - 2 + CharWidth - 1) div CharWidth;
  // lines
  nL1 := Max(TopLine + rcClip.Top div fTextHeight, TopLine);
  nL2 := MinMax(TopLine + (rcClip.Bottom + fTextHeight - 1) div fTextHeight,
    1, DisplayLineCount);
  // Now paint everything while the caret is hidden.
  HideCaret;
  try
    // First paint the gutter area if it was (partly) invalidated.
    if (rcClip.Left < fGutterWidth) then
    begin
      rcDraw := rcClip;
      rcDraw.Right := fGutterWidth;
      PaintGutter(rcDraw, nL1, nL2);
    end;
    // Then paint the text area if it was (partly) invalidated.
    if (rcClip.Right > fGutterWidth) then
    begin
      rcDraw := rcClip;
      rcDraw.Left := Max(rcDraw.Left, fGutterWidth);
      PaintTextLines(rcDraw, nL1, nL2, nC1, nC2);
    end;
    PluginsAfterPaint(Canvas, rcClip, nL1, nL2);
{$IFDEF SYN_CLX}
    if iRestoreViewPort then
      QPainter_setViewport(Canvas.Handle, 0, 0, Width, Height);
{$ENDIF}
    // If there is a custom paint handler call it.
    DoOnPaint;
    DoOnPaintTransient(ttAfter);
  finally
    UpdateCaret;
  end;
end;

procedure TCustomSynEdit.PaintGutter(const AClip: TRect;
  const aFirstRow, aLastRow: Integer);

  procedure DrawMark(aMark: TSynEditMark; var aGutterOff: Integer;
    aMarkRow: Integer);
  begin
    if (not aMark.InternalImage) and Assigned(fBookMarkOpt.BookmarkImages) then
    begin
      if aMark.ImageIndex <= fBookMarkOpt.BookmarkImages.Count then
      begin
        if aMark.IsBookmark = BookMarkOptions.DrawBookmarksFirst then
          aGutterOff := 0
        else if aGutterOff = 0 then
          aGutterOff := fBookMarkOpt.XOffset;
        with fBookMarkOpt do
          BookmarkImages.Draw(Canvas, LeftMargin + aGutterOff,
            (aMarkRow - TopLine) * fTextHeight, aMark.ImageIndex);
        Inc(aGutterOff, fBookMarkOpt.XOffset);
      end;
    end
    else begin
      if aMark.ImageIndex in [0..9] then
      begin
        if not Assigned(fInternalImage) then
        begin
          fInternalImage := TSynInternalImage.Create(HINSTANCE,
            'SynEditInternalImages', 10);
        end;
        if aGutterOff = 0 then
        begin
          fInternalImage.Draw(Canvas, aMark.ImageIndex,
            fBookMarkOpt.LeftMargin + aGutterOff,
            (aMarkRow - TopLine) * fTextHeight, fTextHeight);
        end;
        Inc(aGutterOff, fBookMarkOpt.XOffset);
      end;
    end;
  end;

var
  cLine: Integer;
  cMark: Integer;
  rcLine: TRect;
  aGutterOffs: PIntArray;
  bHasOtherMarks: Boolean;
  s: UnicodeString;
  vFirstLine: Integer;
  vLastLine: Integer;
  vMarkRow: Integer;
  vGutterRow: Integer;
  vLineTop: Integer;
{$IFNDEF SYN_CLX}
  dc: HDC;
  TextSize: TSize;
{$ENDIF}
begin
  vFirstLine := RowToLine(aFirstRow);
  vLastLine := RowToLine(aLastRow);
  //todo: Does the following comment still apply?
  // Changed to use fTextDrawer.BeginDrawing and fTextDrawer.EndDrawing only
  // when absolutely necessary.  Note: Never change brush / pen / font of the
  // canvas inside of this block (only through methods of fTextDrawer)!
  // If we have to draw the line numbers then we don't want to erase
  // the background first. Do it line by line with TextRect instead
  // and fill only the area after the last visible line.
{$IFDEF SYN_CLX}
{$ELSE}
  dc := Canvas.Handle;
{$ENDIF}

  if fGutter.Gradient then
    SynDrawGradient(Canvas, fGutter.GradientStartColor, fGutter.GradientEndColor,
      fGutter.GradientSteps, Rect(0, 0, fGutterWidth, ClientHeight), True);

  Canvas.Brush.Color := fGutter.Color;

  if fGutter.ShowLineNumbers then
  begin
    if fGutter.UseFontStyle then
      fTextDrawer.SetBaseFont(fGutter.Font)
    else
      fTextDrawer.Style := [];
{$IFDEF SYN_CLX}
    fTextDrawer.BeginDrawing(canvas);
{$ELSE}
    fTextDrawer.BeginDrawing(dc);
{$ENDIF}
    try
      if fGutter.UseFontStyle then
        fTextDrawer.SetForeColor(fGutter.Font.Color)
      else
        fTextDrawer.SetForeColor(Self.Font.Color);
      fTextDrawer.SetBackColor(fGutter.Color);

      // prepare the rect initially
      rcLine := AClip;
      rcLine.Right := Max(rcLine.Right, fGutterWidth - 2);
      rcLine.Bottom := rcLine.Top;

      for cLine := vFirstLine to vLastLine do
      begin
        vLineTop := (LineToRow(cLine) - TopLine) * fTextHeight;
        if WordWrap and not fGutter.Gradient then
        begin
          // erase space between wrapped lines (from previous line to current one)
          rcLine.Top := rcLine.Bottom;
          rcLine.Bottom := vLineTop;
          with rcLine do
            fTextDrawer.ExtTextOut(Left, Top, [tooOpaque], rcLine, '', 0);
        end;
        // next line rect
        rcLine.Top := vLineTop;
        rcLine.Bottom := rcLine.Top + fTextHeight;

        s := fGutter.FormatLineNumber(cLine);
        if Assigned(OnGutterGetText) then
          OnGutterGetText(Self, cLine, s);
{$IFDEF SYN_CLX}
        if fGutter.Gradient then
          Canvas.Brush.Style := bsClear
        else
          Canvas.Brush.Style := bsSolid;
        Canvas.FillRect(rcLine);
        Canvas.TextRect(rcLine, fGutter.LeftOffset, rcLine.Top, s);
        // restore brush
        if fGutter.Gradient then
          Canvas.Brush.Style := bsSolid;
{$ELSE}
        TextSize := GetTextSize(DC, PWideChar(s), Length(s));
        if fGutter.Gradient then
        begin
          SetBkMode(DC, TRANSPARENT);
          Windows.ExtTextOutW(DC, (fGutterWidth - fGutter.RightOffset - 2) - TextSize.cx,
            rcLine.Top + ((fTextHeight - Integer(TextSize.cy)) div 2), 0,
            @rcLine, PWideChar(s), Length(s), nil);
          SetBkMode(DC, OPAQUE);
        end
        else
          Windows.ExtTextOutW(DC, (fGutterWidth - fGutter.RightOffset - 2) - TextSize.cx,
            rcLine.Top + ((fTextHeight - Integer(TextSize.cy)) div 2), ETO_OPAQUE,
            @rcLine, PWideChar(s), Length(s), nil);
{$ENDIF}
      end;
      // now erase the remaining area if any
      if (AClip.Bottom > rcLine.Bottom) and not fGutter.Gradient then
      begin
        rcLine.Top := rcLine.Bottom;
        rcLine.Bottom := AClip.Bottom;
        with rcLine do
          fTextDrawer.ExtTextOut(Left, Top, [tooOpaque], rcLine, '', 0);
      end;
    finally
      fTextDrawer.EndDrawing;
      if fGutter.UseFontStyle then
        fTextDrawer.SetBaseFont(Self.Font);
    end;
  end
  else if not fGutter.Gradient then
    Canvas.FillRect(AClip);

{$IFDEF SYN_WIN32}
  // draw word wrap glyphs transparently over gradient
  if fGutter.Gradient then
    Canvas.Brush.Style := bsClear;
{$ENDIF}
  // paint wrapped line glyphs
  if WordWrap and fWordWrapGlyph.Visible then
    for cLine := aFirstRow to aLastRow do
      if LineToRow(RowToLine(cLine)) <> cLine then
        fWordWrapGlyph.Draw(Canvas,
                            (fGutterWidth - fGutter.RightOffset - 2) - fWordWrapGlyph.Width,
                            (cLine - TopLine) * fTextHeight, fTextHeight);
{$IFDEF SYN_WIN32}
  // restore brush
  if fGutter.Gradient then
    Canvas.Brush.Style := bsSolid;
{$ENDIF}

  // the gutter separator if visible
  if (fGutter.BorderStyle <> gbsNone) and (AClip.Right >= fGutterWidth - 2) then
    with Canvas do
    begin
      Pen.Color := fGutter.BorderColor;
      Pen.Width := 1;
      with AClip do
      begin
        if fGutter.BorderStyle = gbsMiddle then
        begin
          MoveTo(fGutterWidth - 2, Top);
          LineTo(fGutterWidth - 2, Bottom);
          Pen.Color := fGutter.Color;
        end;
        MoveTo(fGutterWidth - 1, Top);
        LineTo(fGutterWidth - 1, Bottom);
      end;
    end;

  // now the gutter marks
  if BookMarkOptions.GlyphsVisible and (Marks.Count > 0)
    and (vLastLine >= vFirstLine) then
  begin
    aGutterOffs := AllocMem((aLastRow - aFirstRow + 1) * SizeOf(Integer));
    try
      // Instead of making a two pass loop we look while drawing the bookmarks
      // whether there is any other mark to be drawn
      bHasOtherMarks := False;
      for cMark := 0 to Marks.Count - 1 do with Marks[cMark] do
        if Visible and (Line >= vFirstLine) and (Line <= vLastLine) then
        begin
          if IsBookmark <> BookMarkOptions.DrawBookmarksFirst then
            bHasOtherMarks := True
          else begin
            vMarkRow := LineToRow(Line);
            if vMarkRow >= aFirstRow then
              DrawMark(Marks[cMark], aGutterOffs[vMarkRow - aFirstRow], vMarkRow);
          end
        end;
      if bHasOtherMarks then
        for cMark := 0 to Marks.Count - 1 do with Marks[cMark] do
        begin
          if Visible and (IsBookmark <> BookMarkOptions.DrawBookmarksFirst)
            and (Line >= vFirstLine) and (Line <= vLastLine) then
          begin
            vMarkRow := LineToRow(Line);
            if vMarkRow >= aFirstRow then
              DrawMark(Marks[cMark], aGutterOffs[vMarkRow - aFirstRow], vMarkRow);
          end;
        end;
      if Assigned(OnGutterPaint) then
        for cLine := vFirstLine to vLastLine do
        begin
          vGutterRow := LineToRow(cLine);
          OnGutterPaint(Self, cLine, aGutterOffs[vGutterRow - aFirstRow],
            (vGutterRow - TopLine) * LineHeight);
        end;
    finally
      FreeMem(aGutterOffs);
    end;
  end
  else if Assigned(OnGutterPaint) then
  begin
    for cLine := vFirstLine to vLastLine do
    begin
      vGutterRow := LineToRow(cLine);
      OnGutterPaint(Self, cLine, 0, (vGutterRow - TopLine) * LineHeight);
    end;
  end;
end;

// Inserts filling chars into a string containing chars that display as glyphs
// wider than an average glyph. (This is often the case with Asian glyphs, which
// are usually wider than latin glpyhs)
// This is only to simplify paint-operations and has nothing to do with
// multi-byte chars.
function TCustomSynEdit.ExpandAtWideGlyphs(const S: UnicodeString): UnicodeString;
var
  i, j, CountOfAvgGlyphs: Integer;
begin
  Result := S;
  j := 0;
  SetLength(Result, Length(S) * 2); // speed improvement
  for i := 1 to Length(S) do
  begin
    inc(j);
    CountOfAvgGlyphs := CeilOfIntDiv(fTextDrawer.TextWidth(S[i]), fCharWidth);

    if j + CountOfAvgGlyphs > Length(Result) then
      SetLength(Result, Length(Result) + 128);

    // insert CountOfAvgGlyphs filling chars
    while CountOfAvgGlyphs > 1 do
    begin
      Result[j] := FillerChar;
      inc(j);
      dec(CountOfAvgGlyphs);
    end;

    Result[j] := S[i];
  end;

  SetLength(Result, j);
end;

// does the opposite of ExpandAtWideGlyphs
function TCustomSynEdit.ShrinkAtWideGlyphs(const S: UnicodeString; First: Integer;
  var CharCount: Integer): UnicodeString;
var
  i, j: Integer;
begin
  SetLength(Result, Length(S));

  i := First;
  j := 0;
  while i < First + CharCount do
  begin
    inc(j);
    while S[i] = FillerChar do
      inc(i);
    Result[j] := S[i];
    inc(i);
  end;

  SetLength(Result, j);
  CharCount := j;
end;

procedure TCustomSynEdit.PaintTextLines(AClip: TRect; const aFirstRow, aLastRow,
  FirstCol, LastCol: Integer);
var
  bDoRightEdge: Boolean; // right edge
  nRightEdge: Integer;
    // selection info
  bAnySelection: Boolean; // any selection visible?
  vSelStart: TDisplayCoord; // start of selected area
  vSelEnd: TDisplayCoord; // end of selected area
    // info about normal and selected text and background colors
  bSpecialLine, bLineSelected, bCurrentLine: Boolean;
  colFG, colBG: TColor;
  colSelFG, colSelBG: TColor;
    // info about selection of the current line
  nLineSelStart, nLineSelEnd: Integer;
  bComplexLine: Boolean;
    // painting the background and the text
  rcLine, rcToken: TRect;
  TokenAccu: record
    // Note: s is not managed as a string, it will only grow!!!
    // Never use AppendStr or "+", use Len and MaxLen instead and
    // copy the string chars directly. This is for efficiency.
    Len, MaxLen, CharsBefore: Integer;
    s: UnicodeString;
    TabString: UnicodeString;
    FG, BG: TColor;
    Style: TFontStyles;
  end;
{$IFNDEF SYN_CLX}
  dc: HDC;
{$ENDIF}
  SynTabGlyphString: UnicodeString;

  vFirstLine: Integer;
  vLastLine: Integer;

{ local procedures }

  function colEditorBG: TColor;
  var
    iAttri: TSynHighlighterAttributes;
  begin
    if (ActiveLineColor <> clNone) and (bCurrentLine) then
      Result := ActiveLineColor
    else begin
      Result := Color;
      if Highlighter <> nil then
      begin
        iAttri := Highlighter.WhitespaceAttribute;
        if (iAttri <> nil) and (iAttri.Background <> clNone) then
          Result := iAttri.Background;
      end;
    end;
  end;

  procedure ComputeSelectionInfo;
  var
    vStart: TBufferCoord;
    vEnd: TBufferCoord;
  begin
    bAnySelection := False;
    // Only if selection is visible anyway.
    if not HideSelection or Self.Focused then
    begin
      bAnySelection := True;
      // Get the *real* start of the selected area.
      if fBlockBegin.Line < fBlockEnd.Line then
      begin
        vStart := fBlockBegin;
        vEnd := fBlockEnd;
      end
      else if fBlockBegin.Line > fBlockEnd.Line then
      begin
        vEnd := fBlockBegin;
        vStart := fBlockEnd;
      end
      else if fBlockBegin.Char <> fBlockEnd.Char then
      begin
        // No selection at all, or it is only on this line.
        vStart.Line := fBlockBegin.Line;
        vEnd.Line := vStart.Line;
        if fBlockBegin.Char < fBlockEnd.Char then
        begin
          vStart.Char := fBlockBegin.Char;
          vEnd.Char := fBlockEnd.Char;
        end
        else
        begin
          vStart.Char := fBlockEnd.Char;
          vEnd.Char := fBlockBegin.Char;
        end;
      end
      else
        bAnySelection := False;
      // If there is any visible selection so far, then test if there is an
      // intersection with the area to be painted.
      if bAnySelection then
      begin
        // Don't care if the selection is not visible.
        bAnySelection := (vEnd.Line >= vFirstLine) and (vStart.Line <= vLastLine);
        if bAnySelection then
        begin
          // Transform the selection from text space into screen space
          vSelStart := BufferToDisplayPos(vStart);
          vSelEnd := BufferToDisplayPos(vEnd);
          // In the column selection mode sort the begin and end of the selection,
          // this makes the painting code simpler.
          if (fActiveSelectionMode = smColumn) and (vSelStart.Column > vSelEnd.Column) then
            SwapInt(vSelStart.Column, vSelEnd.Column);
        end;
      end;
    end;
  end;

  procedure SetDrawingColors(Selected: Boolean);
  begin
    with fTextDrawer do
      if Selected then
      begin
        SetBackColor(colSelBG);
        SetForeColor(colSelFG);
        Canvas.Brush.Color := colSelBG;
      end
      else begin
        SetBackColor(colBG);
        SetForeColor(colFG);
        Canvas.Brush.Color := colBG;
      end;
  end;

  function ColumnToXValue(Col: Integer): Integer;
  begin
    Result := fTextOffset + Pred(Col) * fCharWidth;
  end;

  //todo: Review SpecialChars and HardTabs painting. Token parameter of PaintToken procedure could very probably be passed by reference.

  // Note: The PaintToken procedure will take care of invalid parameters
  // like empty token rect or invalid indices into TokenLen.
  // CharsBefore tells if Token starts at column one or not
  procedure PaintToken(Token: UnicodeString;
    TokenLen, CharsBefore, First, Last: Integer);
  var
    Text: UnicodeString;
    Counter, nX, nCharsToPaint: Integer;
    sTabbedToken: UnicodeString;
    DoTabPainting: Boolean;
    i, TabStart, TabLen, CountOfAvgGlyphs, VisibleGlyphPart, FillerCount,
    NonFillerPos: Integer;
    rcTab: TRect;
  const
    ETOOptions = [tooOpaque, tooClipped];
  begin
    sTabbedToken := Token;
    DoTabPainting := False;

    Counter := Last - CharsBefore;
    while Counter > First - CharsBefore - 1 do
    begin
      if Length(Token) >= Counter then
      begin
        if fShowSpecChar and (Token[Counter] = #32) then
          Token[Counter] := SynSpaceGlyph
        else if Token[Counter] = #9 then
        begin
          Token[Counter] := #32;  //Tabs painted differently if necessary
          DoTabPainting := fShowSpecChar;
        end;
      end;
      Dec(Counter);
    end;

    if (Last >= First) and (rcToken.Right > rcToken.Left) then
    begin
      nX := ColumnToXValue(First);

      Dec(First, CharsBefore);
      Dec(Last, CharsBefore);

      if (First > TokenLen) then
      begin
        nCharsToPaint := 0;
        Text := '';
      end
      else
      begin
        FillerCount := 0;
        NonFillerPos := First;
        while Token[NonFillerPos] = FillerChar do
        begin
          inc(FillerCount);
          inc(NonFillerPos);
        end;

        CountOfAvgGlyphs := CeilOfIntDiv(fTextDrawer.TextWidth(Token[NonFillerPos]) , fCharWidth);

        // first visible part of the glyph (1-based)
        // (the glyph is visually sectioned in parts of size fCharWidth)
        VisibleGlyphPart := CountOfAvgGlyphs - FillerCount;

        // clip off invisible parts
        nX := nX - fCharWidth * (VisibleGlyphPart - 1);

        nCharsToPaint := Min(Last - First + 1, TokenLen - First + 1);

        // clip off partially visible glyphs at line end
        if WordWrap then
          while nX + fCharWidth * nCharsToPaint > ClientWidth do
          begin
            dec(nCharsToPaint);
            while (nCharsToPaint > 0) and (Token[First + nCharsToPaint - 1] = FillerChar) do
              dec(nCharsToPaint);
          end;

        // same as copy(Token, First, nCharsToPaint) and remove filler chars
        Text := ShrinkAtWideGlyphs(Token, First, nCharsToPaint);
      end;

      fTextDrawer.ExtTextOut(nX, rcToken.Top, ETOOptions, rcToken,
        PWideChar(Text), nCharsToPaint);

      if DoTabPainting then
      begin
        // fix everything before the FirstChar
        for i := 1 to First - 1 do               // wipe the text out so we don't
          if sTabbedToken[i] = #9 then           // count it out of the range
            sTabbedToken[i] := #32;              // we're looking for

        TabStart := pos(#9, sTabbedToken);
        rcTab.Top := rcToken.Top;
        rcTab.Bottom := rcToken.Bottom;
        while (TabStart > 0) and (TabStart >= First) and (TabStart <= Last) do
        begin
          TabLen := 1;
          while (TabStart + CharsBefore + TabLen - 1) mod FTabWidth <> 0 do inc(TabLen);
          Text := SynTabGlyphString;

          nX := ColumnToXValue(CharsBefore + TabStart + (TabLen div 2) - 1);
          if TabLen mod 2 = 0 then
            nX := nX + (fCharWidth div 2)
          else nX := nX + fCharWidth;

          rcTab.Left := nX;
          rcTab.Right := nX + fTextDrawer.GetCharWidth;

          fTextDrawer.ExtTextOut(nX, rcTab.Top, ETOOptions, rcTab,
            PWideChar(Text), 1);

          for i := 0 to TabLen - 1 do           //wipe the text out so we don't
            sTabbedToken[TabStart + i] := #32;  //count it again

          TabStart := pos(#9, sTabbedToken);
        end;
      end;
      rcToken.Left := rcToken.Right;
    end;
  end;

{$IFNDEF SYN_CLX}
  procedure AdjustEndRect;
  // trick to avoid clipping the last pixels of text in italic,
  // see also AdjustLastCharWidth() in TheTextDrawer.ExtTextOut()
  var
    LastChar: Cardinal;
    NormalCharWidth, RealCharWidth: Integer;
    CharInfo: TABC;
    tm: TTextMetricA;
  begin
    LastChar := Ord(TokenAccu.s[TokenAccu.Len]);
    NormalCharWidth := fTextDrawer.TextWidth(WideChar(LastChar));
    RealCharWidth := NormalCharWidth;
    if Win32PlatformIsUnicode then
    begin
      if GetCharABCWidthsW(Canvas.Handle, LastChar, LastChar, CharInfo) then
      begin
        RealCharWidth := CharInfo.abcA + Integer(CharInfo.abcB);
        if CharInfo.abcC >= 0 then
          Inc(RealCharWidth, CharInfo.abcC);
      end
      else if LastChar < Ord(High(AnsiChar)) then
      begin
        GetTextMetricsA(Canvas.Handle, tm);
        RealCharWidth := tm.tmAveCharWidth + tm.tmOverhang;
      end;
    end
    else if WideChar(LastChar) <= High(AnsiChar) then
    begin
      if GetCharABCWidths(Canvas.Handle, LastChar, LastChar, CharInfo) then
      begin
        RealCharWidth := CharInfo.abcA + Integer(CharInfo.abcB);
        if CharInfo.abcC >= 0 then
          Inc(RealCharWidth, CharInfo.abcC);
      end
      else if LastChar < Ord(High(AnsiChar)) then
      begin
        GetTextMetricsA(Canvas.Handle, tm);
        RealCharWidth := tm.tmAveCharWidth + tm.tmOverhang;
      end;
    end;

    if RealCharWidth > NormalCharWidth then
      Inc(rcToken.Left, RealCharWidth - NormalCharWidth);
  end;
{$ENDIF}

  procedure PaintHighlightToken(bFillToEOL: Boolean);
  var
    bComplexToken: Boolean;
    nC1, nC2, nC1Sel, nC2Sel: Integer;
    bU1, bSel, bU2: Boolean;
    nX1, nX2: Integer;
  begin
    // Compute some helper variables.
    nC1 := Max(FirstCol, TokenAccu.CharsBefore + 1);
    nC2 := Min(LastCol, TokenAccu.CharsBefore + TokenAccu.Len + 1);
    if bComplexLine then
    begin
      bU1 := (nC1 < nLineSelStart);
      bSel := (nC1 < nLineSelEnd) and (nC2 >= nLineSelStart);
      bU2 := (nC2 >= nLineSelEnd);
      bComplexToken := bSel and (bU1 or bU2);
    end
    else
    begin
      bU1 := False; // to shut up Compiler warning Delphi 2
      bSel := bLineSelected;
      bU2 := False; // to shut up Compiler warning Delphi 2
      bComplexToken := False;
    end;
    // Any token chars accumulated?
    if (TokenAccu.Len > 0) then
    begin
      // Initialize the colors and the font style.
      if not bSpecialLine then
      begin
        colBG := TokenAccu.BG;
        colFG := TokenAccu.FG;
      end;

      if bSpecialLine and (eoSpecialLineDefaultFg in fOptions) then
        colFG := TokenAccu.FG;

      fTextDrawer.SetStyle(TokenAccu.Style);
      // Paint the chars
      if bComplexToken then
      begin
        // first unselected part of the token
        if bU1 then
        begin
          SetDrawingColors(False);
          rcToken.Right := ColumnToXValue(nLineSelStart);
          with TokenAccu do
            PaintToken(s, Len, CharsBefore, nC1, nLineSelStart);
        end;
        // selected part of the token
        SetDrawingColors(True);
        nC1Sel := Max(nLineSelStart, nC1);
        nC2Sel := Min(nLineSelEnd, nC2);
        rcToken.Right := ColumnToXValue(nC2Sel);
        with TokenAccu do
          PaintToken(s, Len, CharsBefore, nC1Sel, nC2Sel);
        // second unselected part of the token
        if bU2 then
        begin
          SetDrawingColors(False);
          rcToken.Right := ColumnToXValue(nC2);
          with TokenAccu do
            PaintToken(s, Len, CharsBefore, nLineSelEnd, nC2);
        end;
      end
      else
      begin
        SetDrawingColors(bSel);
        rcToken.Right := ColumnToXValue(nC2);
        with TokenAccu do
          PaintToken(s, Len, CharsBefore, nC1, nC2);
      end;
    end;

    // Fill the background to the end of this line if necessary.
    if bFillToEOL and (rcToken.Left < rcLine.Right) then
    begin
      if not bSpecialLine then colBG := colEditorBG;
      if bComplexLine then
      begin
        nX1 := ColumnToXValue(nLineSelStart);
        nX2 := ColumnToXValue(nLineSelEnd);
        if (rcToken.Left < nX1) then
        begin
          SetDrawingColors(False);
          rcToken.Right := nX1;
{$IFNDEF SYN_CLX}
          if (TokenAccu.Len > 0) and (TokenAccu.Style <> []) then
            AdjustEndRect;
{$ENDIF}
          Canvas.FillRect(rcToken);
          rcToken.Left := nX1;
        end;
        if (rcToken.Left < nX2) then
        begin
          SetDrawingColors(True);
          rcToken.Right := nX2;
{$IFNDEF SYN_CLX}
          if (TokenAccu.Len > 0) and (TokenAccu.Style <> []) then
            AdjustEndRect;
{$ENDIF}
          Canvas.FillRect(rcToken);
          rcToken.Left := nX2;
        end;
        if (rcToken.Left < rcLine.Right) then
        begin
          SetDrawingColors(False);
          rcToken.Right := rcLine.Right;
{$IFNDEF SYN_CLX}
          if (TokenAccu.Len > 0) and (TokenAccu.Style <> []) then
            AdjustEndRect;
{$ENDIF}
          Canvas.FillRect(rcToken);
        end;
      end
      else
      begin
        SetDrawingColors(bLineSelected);
        rcToken.Right := rcLine.Right;
{$IFNDEF SYN_CLX}
        if (TokenAccu.Len > 0) and (TokenAccu.Style <> []) then
          AdjustEndRect;
{$ENDIF}
        Canvas.FillRect(rcToken);
      end;
    end;
  end;

  // Store the token chars with the attributes in the TokenAccu
  // record. This will paint any chars already stored if there is
  // a (visible) change in the attributes.
  procedure AddHighlightToken(const Token: UnicodeString;
    CharsBefore, TokenLen: Integer;
    Foreground, Background: TColor;
    Style: TFontStyles);
  var
    bCanAppend: Boolean;
    bSpacesTest, bIsSpaces: Boolean;
    i: Integer;

    function TokenIsSpaces: Boolean;
    var
      pTok: PWideChar;
    begin
      if not bSpacesTest then
      begin
        bSpacesTest := True;
        pTok := PWideChar(Token);
        while pTok^ <> #0 do
        begin
          if pTok^ <> #32 then
            break;
          Inc(pTok);
        end;
        bIsSpaces := pTok^ = #0;
      end;
      Result := bIsSpaces;
    end;

  begin
    if (Background = clNone) or
      ((ActiveLineColor <> clNone) and (bCurrentLine)) then
    begin
      Background := colEditorBG;
    end;
    if Foreground = clNone then Foreground := Font.Color;
    // Do we have to paint the old chars first, or can we just append?
    bCanAppend := False;
    bSpacesTest := False;
    if (TokenAccu.Len > 0) then
    begin
      // font style must be the same or token is only spaces
      if (TokenAccu.Style = Style)
        or (not (fsUnderline in Style) and not (fsUnderline in TokenAccu.Style)
        and TokenIsSpaces) then
      begin
        // either special colors or same colors
        if (bSpecialLine and not (eoSpecialLineDefaultFg in fOptions)) or bLineSelected or
          // background color must be the same and
          ((TokenAccu.BG = Background) and
          // foreground color must be the same or token is only spaces
          ((TokenAccu.FG = Foreground) or TokenIsSpaces)) then
        begin
          bCanAppend := True;
        end;
      end;
      // If we can't append it, then we have to paint the old token chars first.
      if not bCanAppend then
        PaintHighlightToken(False);
    end;
    // Don't use AppendStr because it's more expensive.
    if bCanAppend then
    begin
      if (TokenAccu.Len + TokenLen > TokenAccu.MaxLen) then
      begin
        TokenAccu.MaxLen := TokenAccu.Len + TokenLen + 32;
        SetLength(TokenAccu.s, TokenAccu.MaxLen);
      end;
      for i := 1 to TokenLen do
        TokenAccu.s[TokenAccu.Len + i] := Token[i];
      Inc(TokenAccu.Len, TokenLen);
    end
    else
    begin
      TokenAccu.Len := TokenLen;
      if (TokenAccu.Len > TokenAccu.MaxLen) then
      begin
        TokenAccu.MaxLen := TokenAccu.Len + 32;
        SetLength(TokenAccu.s, TokenAccu.MaxLen);
      end;
      for i := 1 to TokenLen do
        TokenAccu.s[i] := Token[i];
      TokenAccu.CharsBefore := CharsBefore;
      TokenAccu.FG := Foreground;
      TokenAccu.BG := Background;
      TokenAccu.Style := Style;
    end;
  end;

  procedure PaintLines;
  var
    nLine: Integer; // line index for the loop
    cRow: Integer;
    sLine: UnicodeString; // the current line (tab expanded)
    sLineExpandedAtWideGlyphs: UnicodeString;
    sToken: UnicodeString; // highlighter token info
    nTokenPos, nTokenLen: Integer;
    attr: TSynHighlighterAttributes;
    vAuxPos: TDisplayCoord;
    vFirstChar: Integer;
    vLastChar: Integer;
    vStartRow: Integer;
    vEndRow: Integer;
  begin
    // Initialize rcLine for drawing. Note that Top and Bottom are updated
    // inside the loop. Get only the starting point for this.
    rcLine := AClip;
    rcLine.Left := fGutterWidth + 2;
    rcLine.Bottom := (aFirstRow - TopLine) * fTextHeight;
    // Make sure the token accumulator string doesn't get reassigned to often.
    if Assigned(fHighlighter) then
    begin
      TokenAccu.MaxLen := Max(128, fCharsInWindow);
      SetLength(TokenAccu.s, TokenAccu.MaxLen);
    end;
    // Now loop through all the lines. The indices are valid for Lines.
    for nLine := vFirstLine to vLastLine do
    begin
      sLine := TSynEditStringList(Lines).ExpandedStrings[nLine - 1];
      sLineExpandedAtWideGlyphs := ExpandAtWideGlyphs(sLine);
      // determine whether will be painted with ActiveLineColor
      bCurrentLine := CaretY = nLine;
      // Initialize the text and background colors, maybe the line should
      // use special values for them.
      colFG := Font.Color;
      colBG := colEditorBG;
      bSpecialLine := DoOnSpecialLineColors(nLine, colFG, colBG);
      if bSpecialLine then
      begin
        // The selection colors are just swapped, like seen in Delphi.
        colSelFG := colBG;
        colSelBG := colFG;
      end
      else
      begin
        colSelFG := fSelectedColor.Foreground;
        colSelBG := fSelectedColor.Background;
      end;

      vStartRow := Max(LineToRow(nLine), aFirstRow);
      vEndRow := Min(LineToRow(nLine + 1) - 1, aLastRow);
      for cRow := vStartRow to vEndRow do
      begin
        if WordWrap then
        begin
          vAuxPos.Row := cRow;
          if Assigned(fHighlighter) then
            vAuxPos.Column := FirstCol
          else
            // When no highlighter is assigned, we must always start from the
            // first char in a row and PaintToken will do the actual clipping
            vAuxPos.Column := 1;
          vFirstChar := fWordWrapPlugin.DisplayToBufferPos(vAuxPos).Char;
          vAuxPos.Column := LastCol;
          vLastChar := fWordWrapPlugin.DisplayToBufferPos(vAuxPos).Char;
        end
        else
        begin
          vFirstChar := FirstCol;
          vLastChar := LastCol;
        end;
        // Get the information about the line selection. Three different parts
        // are possible (unselected before, selected, unselected after), only
        // unselected or only selected means bComplexLine will be False. Start
        // with no selection, compute based on the visible columns.
        bComplexLine := False;
        nLineSelStart := 0;
        nLineSelEnd := 0;
        // Does the selection intersect the visible area?
        if bAnySelection and (cRow >= vSelStart.Row) and (cRow <= vSelEnd.Row) then
        begin
          // Default to a fully selected line. This is correct for the smLine
          // selection mode and a good start for the smNormal mode.
          nLineSelStart := FirstCol;
          nLineSelEnd := LastCol + 1;
          if (fActiveSelectionMode = smColumn) or
            ((fActiveSelectionMode = smNormal) and (cRow = vSelStart.Row)) then
          begin
            if (vSelStart.Column > LastCol) then
            begin
              nLineSelStart := 0;
              nLineSelEnd := 0;
            end
            else if (vSelStart.Column > FirstCol) then
            begin
              nLineSelStart := vSelStart.Column;
              bComplexLine := True;
            end;
          end;
          if (fActiveSelectionMode = smColumn) or
            ((fActiveSelectionMode = smNormal) and (cRow = vSelEnd.Row)) then
          begin
            if (vSelEnd.Column < FirstCol) then
            begin
              nLineSelStart := 0;
              nLineSelEnd := 0;
            end
            else if (vSelEnd.Column < LastCol) then
            begin
              nLineSelEnd := vSelEnd.Column;
              bComplexLine := True;
            end;
          end;
        end; //endif bAnySelection

        // Update the rcLine rect to this line.
        rcLine.Top := rcLine.Bottom;
        Inc(rcLine.Bottom, fTextHeight);

        bLineSelected := not bComplexLine and (nLineSelStart > 0);
        rcToken := rcLine;

        if not Assigned(fHighlighter) or not fHighlighter.Enabled then
        begin
          // Remove text already displayed (in previous rows)
          if (vFirstChar <> FirstCol) or (vLastChar <> LastCol) then
            sToken := Copy(sLineExpandedAtWideGlyphs, vFirstChar, vLastChar - vFirstChar)
          else
            sToken := Copy(sLineExpandedAtWideGlyphs, 1, vLastChar);
          if fShowSpecChar and (Length(sLineExpandedAtWideGlyphs) < vLastChar) then
            sToken := sToken + SynLineBreakGlyph;
          nTokenLen := Length(sToken);
          if bComplexLine then
          begin
            SetDrawingColors(False);
            rcToken.Left := Max(rcLine.Left, ColumnToXValue(FirstCol));
            rcToken.Right := Min(rcLine.Right, ColumnToXValue(nLineSelStart));
            PaintToken(sToken, nTokenLen, 0, FirstCol, nLineSelStart);
            rcToken.Left := Max(rcLine.Left, ColumnToXValue(nLineSelEnd));
            rcToken.Right := Min(rcLine.Right, ColumnToXValue(LastCol));
            PaintToken(sToken, nTokenLen, 0, nLineSelEnd, LastCol);
            SetDrawingColors(True);
            rcToken.Left := Max(rcLine.Left, ColumnToXValue(nLineSelStart));
            rcToken.Right := Min(rcLine.Right, ColumnToXValue(nLineSelEnd));
            PaintToken(sToken, nTokenLen, 0, nLineSelStart, nLineSelEnd - 1);
          end
          else
          begin
            SetDrawingColors(bLineSelected);
            PaintToken(sToken, nTokenLen, 0, FirstCol, LastCol);
          end;
        end
        else
        begin
          // Initialize highlighter with line text and range info. It is
          // necessary because we probably did not scan to the end of the last
          // line - the internal highlighter range might be wrong.
          if nLine = 1 then
            fHighlighter.ResetRange
          else if fSingleLineMode then
            fHighlighter.ResetRange
          else
            fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[nLine - 2]);
          fHighlighter.SetLineExpandedAtWideGlyphs(sLine, sLineExpandedAtWideGlyphs,
            nLine - 1);
          // Try to concatenate as many tokens as possible to minimize the count
          // of ExtTextOutW calls necessary. This depends on the selection state
          // or the line having special colors. For spaces the foreground color
          // is ignored as well.
          TokenAccu.Len := 0;
          nTokenPos := 0;
          nTokenLen := 0;
          attr := nil;
          // Test first whether anything of this token is visible.
          while not fHighlighter.GetEol do
          begin
            nTokenPos := fHighlighter.GetExpandedTokenPos;
            sToken := fHighlighter.GetExpandedToken;
            nTokenLen := Length(sToken);
            if nTokenPos + nTokenLen >= vFirstChar then
            begin
              if nTokenPos + nTokenLen > vLastChar then
              begin
                if nTokenPos > vLastChar then
                  break;
                if WordWrap then
                  nTokenLen := vLastChar - nTokenPos - 1
                else
                  nTokenLen := vLastChar - nTokenPos;
              end;
              // Remove offset generated by tokens already displayed (in previous rows)
              Dec(nTokenPos, vFirstChar - FirstCol);
              // It's at least partially visible. Get the token attributes now.
              attr := fHighlighter.GetTokenAttribute;
              if Assigned(attr) then
                AddHighlightToken(sToken, nTokenPos, nTokenLen, attr.Foreground,
                  attr.Background, attr.Style)
              else
                AddHighlightToken(sToken, nTokenPos, nTokenLen, colFG, colBG,
                  Font.Style);
            end;
            // Let the highlighter scan the next token.
            fHighlighter.Next;
          end;
          // Draw anything that's left in the TokenAccu record. Fill to the end
          // of the invalid area with the correct colors.
          if fShowSpecChar and fHighlighter.GetEol then
          begin
            if (attr = nil) or (attr <> fHighlighter.CommentAttribute) then
               attr := fHighlighter.WhitespaceAttribute;
            AddHighlightToken(SynLineBreakGlyph, nTokenPos + nTokenLen, 1,
              attr.Foreground, attr.Background, []);
          end;
          PaintHighlightToken(True);
        end;
        // Now paint the right edge if necessary. We do it line by line to reduce
        // the flicker. Should not cost very much anyway, compared to the many
        // calls to ExtTextOutW.
        if bDoRightEdge then
        begin
          Canvas.MoveTo(nRightEdge, rcLine.Top);
          Canvas.LineTo(nRightEdge, rcLine.Bottom + 1);
        end;
      end; //endfor cRow
      bCurrentLine := False;
    end; //endfor cLine
  end;

{ end local procedures }

begin
  vFirstLine := RowToLine(aFirstRow);
  vLastLine := RowToLine(aLastRow);

  bCurrentLine := False;
  // If the right edge is visible and in the invalid area, prepare to paint it.
  // Do this first to realize the pen when getting the dc variable.
  SynTabGlyphString := SynTabGlyph;
  bDoRightEdge := False;
  if (fRightEdge > 0) then
  begin // column value
    nRightEdge := fTextOffset + fRightEdge * fCharWidth; // pixel value
    if (nRightEdge >= AClip.Left) and (nRightEdge <= AClip.Right) then
    begin
      bDoRightEdge := True;
      Canvas.Pen.Color := fRightEdgeColor;
      Canvas.Pen.Width := 1;
    end;
  end;
{$IFDEF SYN_CLX}
{$ELSE}
  // Do everything else with API calls. This (maybe) realizes the new pen color.
  dc := Canvas.Handle;
{$ENDIF}
  // If anything of the two pixel space before the text area is visible, then
  // fill it with the component background color.
  if (AClip.Left < fGutterWidth + 2) then
  begin
    rcToken := AClip;
    rcToken.Left := Max(AClip.Left, fGutterWidth);
    rcToken.Right := fGutterWidth + 2;
    // Paint whole left edge of the text with same color.
    // (value of WhiteAttribute can vary in e.g. MultiSyn)
    if Highlighter <> nil then
      Highlighter.ResetRange;
    Canvas.Brush.Color := colEditorBG;
    Canvas.FillRect(rcToken);
    // Adjust the invalid area to not include this area.
    AClip.Left := rcToken.Right;
  end;
  // Paint the visible text lines. To make this easier, compute first the
  // necessary information about the selected area: is there any visible
  // selected area, and what are its lines / columns?
  if (vLastLine >= vFirstLine) then
  begin
    ComputeSelectionInfo;
    fTextDrawer.Style := Font.Style;
{$IFDEF SYN_CLX}
    fTextDrawer.BeginDrawing(Canvas);
{$ELSE}
    fTextDrawer.BeginDrawing(dc);
{$ENDIF}
    try
      PaintLines;
    finally
      fTextDrawer.EndDrawing;
    end;
  end;
  // If there is anything visible below the last line, then fill this as well.
  rcToken := AClip;
  rcToken.Top := (aLastRow - TopLine + 1) * fTextHeight;
  if (rcToken.Top < rcToken.Bottom) then
  begin
    if Highlighter <> nil then
      Highlighter.ResetRange;
    Canvas.Brush.Color := colEditorBG;
    Canvas.FillRect(rcToken);
    // Draw the right edge if necessary.
    if bDoRightEdge then
    begin
      Canvas.MoveTo(nRightEdge, rcToken.Top);
      Canvas.LineTo(nRightEdge, rcToken.Bottom + 1);
    end;
  end;
end;

procedure TCustomSynEdit.PasteFromClipboard;
var
  AddPasteEndMarker: boolean;
  vStartOfBlock: TBufferCoord;
  vEndOfBlock: TBufferCoord;
  StoredPaintLock: Integer;
  PasteMode: TSynSelectionMode;
{$IFNDEF SYN_CLX}
  Mem: HGLOBAL;
  P: PByte;
{$ENDIF}
begin
  if not CanPaste then
    exit;
  DoOnPaintTransient(ttBefore);
  BeginUndoBlock;
  AddPasteEndMarker := False;
  PasteMode := SelectionMode;
  try
{$IFNDEF SYN_CLX}
    // Check for our special format and read PasteMode.
    // The text is ignored as it is ANSI-only to stay compatible with programs
    // using the ANSI version of SynEdit.
    //
    // Instead we take the text stored in CF_UNICODETEXT or CF_TEXT.
    if Clipboard.HasFormat(SynEditClipboardFormat) then
    begin
      Clipboard.Open;
      try
        Mem := Clipboard.GetAsHandle(SynEditClipboardFormat);
        P := GlobalLock(Mem);
        try
          if P <> nil then
            PasteMode := PSynSelectionMode(P)^;
        finally
          GlobalUnlock(Mem);
        end
      finally
        Clipboard.Close;
      end;
    end;
{$ENDIF}
    fUndoList.AddChange(crPasteBegin, BlockBegin, BlockEnd, '', smNormal);
    AddPasteEndMarker := True;
    if SelAvail then
    begin
      fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, SelText,
        fActiveSelectionMode);
    end
    else
      ActiveSelectionMode := SelectionMode;

    if SelAvail then
    begin
      vStartOfBlock := BlockBegin;
      vEndOfBlock := BlockEnd;
      fBlockBegin := vStartOfBlock;
      fBlockEnd := vEndOfBlock;

      // Pasting always occurs at column 0 when current selection is
      // smLine type
      if fActiveSelectionMode = smLine then
        vStartOfBlock.Char := 1;
    end
    else
      vStartOfBlock := CaretXY;

    SetSelTextPrimitiveEx(PasteMode, PWideChar(GetClipboardText), True);
    vEndOfBlock := BlockEnd;
    if PasteMode = smNormal then
      fUndoList.AddChange(crPaste, vStartOfBlock, vEndOfBlock, SelText,
        PasteMode)
    else if PasteMode = smColumn then
      // Do nothing. Moved to InsertColumn
    else if PasteMode = smLine then
      if CaretX = 1 then
        fUndoList.AddChange(crPaste, BufferCoord(1, vStartOfBlock.Line),
          BufferCoord(CharsInWindow, vEndOfBlock.Line - 1), SelText, smLine)
      else
        fUndoList.AddChange(crPaste, BufferCoord(1, vStartOfBlock.Line),
          vEndOfBlock, SelText, smNormal);
  finally
    if AddPasteEndMarker then
      fUndoList.AddChange(crPasteEnd, BlockBegin, BlockEnd, '', smNormal);
    EndUndoBlock;
  end;

  // ClientRect can be changed by UpdateScrollBars if eoHideShowScrollBars
  // is enabled
  if eoHideShowScrollBars in Options then
  begin
    StoredPaintLock := fPaintLock;
    try
      fPaintLock := 0;
      UpdateScrollBars;
    finally
      fPaintLock := StoredPaintLock;
    end;
  end;

  EnsureCursorPosVisible;
  // Selection should have changed...
  StatusChanged([scSelection]);
  DoOnPaintTransient(ttAfter);
end;

procedure TCustomSynEdit.SelectAll;
var
  LastPt: TBufferCoord;
begin
  LastPt.Char := 1;
  LastPt.Line := Lines.Count;
  if LastPt.Line > 0 then
    Inc(LastPt.Char, Length(Lines[LastPt.Line - 1]))
  else
    LastPt.Line  := 1;
  SetCaretAndSelection(LastPt, BufferCoord(1, 1), LastPt);
  // Selection should have changed...
  StatusChanged([scSelection]);
end;

procedure TCustomSynEdit.SetBlockBegin(Value: TBufferCoord);
var
  nInval1, nInval2: Integer;
  SelChanged: Boolean;
begin
  ActiveSelectionMode := SelectionMode;
  if (eoScrollPastEol in Options) and not WordWrap then
    Value.Char := MinMax(Value.Char, 1, fMaxScrollWidth + 1)
  else
    Value.Char := Max(Value.Char, 1);
  Value.Line := MinMax(Value.Line, 1, Lines.Count);
  if (fActiveSelectionMode = smNormal) then
    if (Value.Line >= 1) and (Value.Line <= Lines.Count) then
      Value.Char := Min(Value.Char, Length(Lines[Value.Line - 1]) + 1)
    else
      Value.Char := 1;
  if SelAvail then
  begin
    if fBlockBegin.Line < fBlockEnd.Line then
    begin
      nInval1 := Min(Value.Line, fBlockBegin.Line);
      nInval2 := Max(Value.Line, fBlockEnd.Line);
    end
    else
    begin
      nInval1 := Min(Value.Line, fBlockEnd.Line);
      nInval2 := Max(Value.Line, fBlockBegin.Line);
    end;
    fBlockBegin := Value;
    fBlockEnd := Value;
    InvalidateLines(nInval1, nInval2);
    SelChanged := True;
  end
  else
  begin
    SelChanged :=
      (fBlockBegin.Char <> Value.Char) or (fBlockBegin.Line <> Value.Line) or
      (fBlockEnd.Char <> Value.Char) or (fBlockEnd.Line <> Value.Line);
    fBlockBegin := Value;
    fBlockEnd := Value;
  end;
  if SelChanged then
    StatusChanged([scSelection]);
end;

procedure TCustomSynEdit.SetBlockEnd(Value: TBufferCoord);
var
  nLine: Integer;
begin
  ActiveSelectionMode := SelectionMode;
  if not (eoNoSelection in Options) then
  begin
    if (eoScrollPastEol in Options) and not WordWrap then
      Value.Char := MinMax(Value.Char, 1, fMaxScrollWidth + 1)
    else
      Value.Char := Max(Value.Char, 1);
    Value.Line := MinMax(Value.Line, 1, Lines.Count);
    if (fActiveSelectionMode = smNormal) then
      if (Value.Line >= 1) and (Value.Line <= Lines.Count) then
        Value.Char := Min(Value.Char, Length(Lines[Value.Line - 1]) + 1)
      else
        Value.Char := 1;
    if (Value.Char <> fBlockEnd.Char) or (Value.Line <> fBlockEnd.Line) then
    begin
      if (Value.Char <> fBlockEnd.Char) or (Value.Line <> fBlockEnd.Line) then
      begin
        if (fActiveSelectionMode = smColumn) and (Value.Char <> fBlockEnd.Char) then
        begin
          InvalidateLines(
            Min(fBlockBegin.Line, Min(fBlockEnd.Line, Value.Line)),
            Max(fBlockBegin.Line, Max(fBlockEnd.Line, Value.Line)));
          fBlockEnd := Value;
        end
        else begin
          nLine := fBlockEnd.Line;
          fBlockEnd := Value;
          if (fActiveSelectionMode <> smColumn) or (fBlockBegin.Char <> fBlockEnd.Char) then
            InvalidateLines(nLine, fBlockEnd.Line);
        end;
        StatusChanged([scSelection]);
      end;
    end;
  end;
end;

procedure TCustomSynEdit.SetCaretX(Value: Integer);
var
  vNewCaret: TBufferCoord;
begin
  vNewCaret.Char := Value;
  vNewCaret.Line := CaretY;
  SetCaretXY(vNewCaret);
end;

procedure TCustomSynEdit.SetCaretY(Value: Integer);
var
  vNewCaret: TBufferCoord;
begin
  vNewCaret.Line := Value;
  vNewCaret.Char := CaretX;
  SetCaretXY(vNewCaret);
end;

procedure TCustomSynEdit.InternalSetCaretX(Value: Integer);
var
  vNewCaret: TBufferCoord;
begin
  vNewCaret.Char := Value;
  vNewCaret.Line := CaretY;
  InternalSetCaretXY(vNewCaret);
end;

procedure TCustomSynEdit.InternalSetCaretY(Value: Integer);
var
  vNewCaret: TBufferCoord;
begin
  vNewCaret.Line := Value;
  vNewCaret.Char := CaretX;
  InternalSetCaretXY(vNewCaret);
end;

function TCustomSynEdit.GetCaretXY: TBufferCoord;
begin
  Result.Char := CaretX;
  Result.Line := CaretY;
end;

function TCustomSynEdit.GetDisplayX: Integer;
begin
  Result := DisplayXY.Column;
end;

function TCustomSynEdit.GetDisplayY: Integer;
begin
  if not WordWrap then
    Result := CaretY
  else
    Result := DisplayXY.Row;
end;

Function TCustomSynEdit.GetDisplayXY: TDisplayCoord;
begin
  Result := BufferToDisplayPos(CaretXY);
  if WordWrap and fCaretAtEOL then
  begin
    if Result.Column = 1 then
    begin
      Dec(Result.Row);
      Result.Column := fWordWrapPlugin.GetRowLength(Result.Row) +1;
    end
    else begin
      // Work-around situations where fCaretAtEOL should have been updated because of
      //text change (it's only valid when Column = 1). Updating it in ProperSetLine()
      //would probably be the right thing, but...
      fCaretAtEOL := False;
    end;
  end;
end;

procedure TCustomSynEdit.SetCaretXY(const Value: TBufferCoord);
//there are two setCaretXY methods.  One Internal, one External.  The published
//property CaretXY (re)sets the block as well
begin
  IncPaintLock;
  try
    Include(fStatusChanges, scSelection);
    SetCaretXYEx(True, Value);
    if SelAvail then
      InvalidateSelection;
    fBlockBegin.Char := fCaretX;
    fBlockBegin.Line := fCaretY;
    fBlockEnd := fBlockBegin;
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.InternalSetCaretXY(const Value: TBufferCoord);
begin
  SetCaretXYEx(True, Value);
end;

procedure TCustomSynEdit.UpdateLastCaretX;
begin
  fLastCaretX := DisplayX;
end;

procedure TCustomSynEdit.SetCaretXYEx(CallEnsureCursorPos: Boolean; Value: TBufferCoord);
var
  nMaxX: Integer;
  vTriggerPaint: boolean;
begin
  fCaretAtEOL := False;
  vTriggerPaint := HandleAllocated;
  if vTriggerPaint then
    DoOnPaintTransient(ttBefore);
  if WordWrap then
    nMaxX := MaxInt
  else
    nMaxX := MaxScrollWidth + 1;
  if Value.Line > Lines.Count then
    Value.Line := Lines.Count;
  if Value.Line < 1 then
  begin
    // this is just to make sure if Lines stringlist should be empty
    Value.Line := 1;
    if not (eoScrollPastEol in fOptions) then
      nMaxX := 1;
  end
  else
  begin
    if not (eoScrollPastEol in fOptions) then
      nMaxX := Length(Lines[Value.Line - 1]) + 1;
  end;
  if (Value.Char > nMaxX) and (not(eoScrollPastEol in Options) or
    not(eoAutoSizeMaxScrollWidth in Options)) then
  begin
    Value.Char := nMaxX;
  end;
  if Value.Char < 1 then
    Value.Char := 1;
  if (Value.Char <> fCaretX) or (Value.Line <> fCaretY) then
  begin
    IncPaintLock;
    try
      // simply include the flags, fPaintLock is > 0
      if fCaretX <> Value.Char then
      begin
        fCaretX := Value.Char;
        Include(fStatusChanges, scCaretX);
      end;
      if fCaretY <> Value.Line then
      begin
        if ActiveLineColor <> clNone then
        begin
          InvalidateLine(Value.Line);
          InvalidateLine(fCaretY);
        end;
        fCaretY := Value.Line;
        Include(fStatusChanges, scCaretY);
      end;
      // Call UpdateLastCaretX before DecPaintLock because the event handler it
      // calls could raise an exception, and we don't want fLastCaretX to be
      // left in an undefined state if that happens.
      UpdateLastCaretX;
      if CallEnsureCursorPos then
        EnsureCursorPosVisible;
      Include(fStateFlags, sfCaretChanged);
      Include(fStateFlags, sfScrollbarChanged);
    finally
      DecPaintLock;
    end;
  end
  else begin
    // Also call UpdateLastCaretX if the caret didn't move. Apps don't know
    // anything about fLastCaretX and they shouldn't need to. So, to avoid any
    // unwanted surprises, always update fLastCaretX whenever CaretXY is
    // assigned to.
    // Note to SynEdit developers: If this is undesirable in some obscure
    // case, just save the value of fLastCaretX before assigning to CaretXY and
    // restore it afterward as appropriate.
    UpdateLastCaretX;
  end;
  if vTriggerPaint then
    DoOnPaintTransient(ttAfter);
end;

function TCustomSynEdit.CaretInView: Boolean;
var
  vCaretRowCol: TDisplayCoord;
begin
  vCaretRowCol := DisplayXY;
  Result := (vCaretRowCol.Column >= LeftChar)
    and (vCaretRowCol.Column <= LeftChar + CharsInWindow)
    and (vCaretRowCol.Row >= TopLine)
    and (vCaretRowCol.Row <= TopLine + LinesInWindow);
end;

procedure TCustomSynEdit.SetActiveLineColor(Value: TColor);
begin
  if (fActiveLineColor<>Value) then
  begin
    fActiveLineColor:=Value;
    InvalidateLine(CaretY);
  end;
end;

procedure TCustomSynEdit.SetFont(const Value: TFont);
{$IFDEF SYN_CLX}
{$ELSE}
var
  DC: HDC;
  Save: THandle;
  Metrics: TTextMetric;
  AveCW, MaxCW: Integer;
{$ENDIF}
begin
{$IFDEF SYN_CLX}
  inherited Font := Value;
{$ELSE}
  DC := GetDC(0);
  Save := SelectObject(DC, Value.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, Save);
  ReleaseDC(0, DC);
  with Metrics do
  begin
    AveCW := tmAveCharWidth;
    MaxCW := tmMaxCharWidth;
  end;
  case AveCW = MaxCW of
    True: inherited Font := Value;
    False:
      begin
        with fFontDummy do
        begin
          Color := Value.Color;
          Pitch := fpFixed;
          Size := Value.Size;
          Style := Value.Style;
          Name := Value.Name;
        end;
        inherited Font := fFontDummy;
      end;
  end;
{$ENDIF}
  TSynEditStringList(fLines).FontChanged;
  if fGutter.ShowLineNumbers then
    GutterChanged(Self);
end;

procedure TCustomSynEdit.SetGutterWidth(Value: Integer);
begin
  Value := Max(Value, 0);
  if fGutterWidth <> Value then
  begin
    fGutterWidth := Value;
    fTextOffset := fGutterWidth + 2 - (LeftChar - 1) * fCharWidth;
    if HandleAllocated then
    begin
      fCharsInWindow := Max(ClientWidth - fGutterWidth - 2, 0) div fCharWidth;
      if WordWrap then
        fWordWrapPlugin.DisplayChanged;
      UpdateScrollBars;
      Invalidate;
    end;
  end;
end;

procedure TCustomSynEdit.SetLeftChar(Value: Integer);
var
  MaxVal: Integer;
  iDelta: Integer;
  iTextArea: TRect;
begin
  if WordWrap then
    Value := 1;

  if eoScrollPastEol in Options then
  begin
    if eoAutoSizeMaxScrollWidth in Options then
      MaxVal := MaxInt - CharsInWindow
    else
      MaxVal := MaxScrollWidth - CharsInWindow + 1
  end
  else
  begin
    MaxVal := TSynEditStringList(Lines).LengthOfLongestLine;
    if MaxVal > CharsInWindow then
      MaxVal := MaxVal - CharsInWindow + 1
    else
      MaxVal := 1;
  end;
  Value := MinMax(Value, 1, MaxVal);
  if Value <> fLeftChar then
  begin
    iDelta := fLeftChar - Value;
    fLeftChar := Value;
    fTextOffset := fGutterWidth + 2 - (LeftChar - 1) * fCharWidth;
    if Abs(iDelta) < CharsInWindow then
    begin
      iTextArea := ClientRect;
      Inc(iTextArea.Left, fGutterWidth + 2);
{$IFDEF SYN_CLX}
      ScrollWindow(Self, iDelta * CharWidth, 0, @iTextArea);
{$ELSE}
      ScrollWindow(Handle, iDelta * CharWidth, 0, @iTextArea, @iTextArea);
{$ENDIF}
    end
    else
      InvalidateLines(-1, -1);
    if (Options >= [eoAutoSizeMaxScrollWidth, eoScrollPastEol]) and
      (MaxScrollWidth < LeftChar + CharsInWindow) then
    begin
      MaxScrollWidth := LeftChar + CharsInWindow
    end
    else
      UpdateScrollBars;
    StatusChanged([scLeftChar]);
  end;
end;

procedure TCustomSynEdit.SetLines(Value: TUnicodeStrings);
begin
  Lines.Assign(Value);
end;

procedure TCustomSynEdit.SetLineText(Value: UnicodeString);
begin
  if (CaretY >= 1) and (CaretY <= Max(1, Lines.Count)) then
    Lines[CaretY - 1] := Value;
end;

procedure TCustomSynEdit.SetFontSmoothing(AValue: TSynFontSmoothMethod);
const
  NONANTIALIASED_QUALITY = 3;
  ANTIALIASED_QUALITY    = 4;
  CLEARTYPE_QUALITY      = 5;
var
  bMethod: Byte;
  lf: TLogFont;
begin
  if fFontSmoothing <> AValue then begin
    fFontSmoothing:= AValue;
    case fFontSmoothing of
      fsmAntiAlias:
        bMethod:= ANTIALIASED_QUALITY;
      fsmClearType:
        bMethod:= CLEARTYPE_QUALITY;
      else // fsmNone also
        bMethod:= NONANTIALIASED_QUALITY;
    end;
    GetObject(Font.Handle, SizeOf(TLogFont), @lf);
    lf.lfQuality:= bMethod;
    Font.Handle:= CreateFontIndirect(lf);
  end;
end;

procedure TCustomSynEdit.SetName(const Value: TComponentName);
var
  TextToName: Boolean;
begin
  TextToName := (ComponentState * [csDesigning, csLoading] = [csDesigning])
    and (TrimRight(Text) = Name);
  inherited SetName(Value);
  if TextToName then
    Text := Value;
end;

procedure TCustomSynEdit.SetScrollBars(const Value: TScrollStyle);
begin
  if (FScrollBars <> Value) then
  begin
    FScrollBars := Value;
    UpdateScrollBars;
    Invalidate;
  end;
end;

procedure TCustomSynEdit.SetSelTextPrimitive(const Value: UnicodeString);
begin
  SetSelTextPrimitiveEx(fActiveSelectionMode, PWideChar(Value), True);
end;

// This is really a last minute change and I hope I did it right.
// Reason for this modification: next two lines will loose the CaretX position
// if eoScrollPastEol is not set in Options. That is not really a good idea
// as we would typically want the cursor to stay where it is.
// To fix this (in the absence of a better idea), I changed the code in
// DeleteSelection not to trim the string if eoScrollPastEol is not set.
procedure TCustomSynEdit.SetSelTextPrimitiveEx(PasteMode: TSynSelectionMode;
  Value: PWideChar; AddToUndoList: Boolean);
var
  BB, BE: TBufferCoord;
  TempString: UnicodeString;

  procedure DeleteSelection;
  var
    x, MarkOffset, MarkOffset2: Integer;
    UpdateMarks: Boolean;
  begin
    UpdateMarks := False;
    MarkOffset := 0;
    MarkOffset2 := 0;
    case fActiveSelectionMode of
      smNormal:
        begin
          if Lines.Count > 0 then
          begin
              // Create a string that contains everything on the first line up
              // to the selection mark, and everything on the last line after
              // the selection mark.
            if BB.Char > 1 then
              MarkOffset2 := 1;
            TempString := Copy(Lines[BB.Line - 1], 1, BB.Char - 1) +
              Copy(Lines[BE.Line - 1], BE.Char, MaxInt);
              // Delete all lines in the selection range.
            TSynEditStringList(Lines).DeleteLines(BB.Line, BE.Line - BB.Line);
              // Put the stuff that was outside of selection back in.
            if Options >= [eoScrollPastEol, eoTrimTrailingSpaces] then
              TempString := TrimTrailingSpaces(TempString);
            Lines[BB.Line - 1] := TempString;
          end;
          UpdateMarks := True;
          InternalCaretXY := BB;
        end;
      smColumn:
        begin
            // swap X if needed
          if BB.Char > BE.Char then
            SwapInt(Integer(BB.Char), Integer(BE.Char));

          for x := BB.Line - 1 to BE.Line - 1 do
          begin
            TempString := Lines[x];
            Delete(TempString, BB.Char, BE.Char - BB.Char);
            ProperSetLine(x, TempString);
          end;
          // Lines never get deleted completely, so keep caret at end.
          InternalCaretXY := BufferCoord(BB.Char, fBlockEnd.Line);
          // Column deletion never removes a line entirely, so no mark
          // updating is needed here.
        end;
      smLine:
        begin
          if BE.Line = Lines.Count then
          begin
            Lines[BE.Line - 1] := '';
            for x := BE.Line - 2 downto BB.Line - 1 do
              Lines.Delete(x);
          end
          else begin
            for x := BE.Line - 1 downto BB.Line - 1 do
              Lines.Delete(x);
          end;
          // smLine deletion always resets to first column.
          InternalCaretXY := BufferCoord(1, BB.Line);
          UpdateMarks := TRUE;
          MarkOffset := 1;
        end;
    end;
    // Update marks
    if UpdateMarks then
      DoLinesDeleted(BB.Line + MarkOffset2, BE.Line - BB.Line + MarkOffset);
  end;

  procedure InsertText;

    function CountLines(p: PWideChar): Integer;
    begin
      Result := 0;
      while p^ <> #0 do
      begin
        if p^ = #13 then
          Inc(p);
        if p^ = #10 then
          Inc(p);
        Inc(Result);
        p := GetEOL(p);
      end;
    end;

    function InsertNormal: Integer;
    var
      sLeftSide: UnicodeString;
      sRightSide: UnicodeString;
      Str: UnicodeString;
      Start: PWideChar;
      P: PWideChar;
    begin
      Result := 0;
      sLeftSide := Copy(LineText, 1, CaretX - 1);
      if CaretX - 1 > Length(sLeftSide) then
      begin
        sLeftSide := sLeftSide + UnicodeStringOfChar(#32,
          CaretX - 1 - Length(sLeftSide));
      end;
      sRightSide := Copy(LineText, CaretX, Length(LineText) - (CaretX - 1));
      // step1: insert the first line of Value into current line
      Start := PWideChar(Value);
      P := GetEOL(Start);
      if P^ <> #0 then
      begin
        Str := sLeftSide + Copy(Value, 1, P - Start);
        ProperSetLine(CaretY - 1, Str);
        TSynEditStringList(Lines).InsertLines(CaretY, CountLines(P));
      end
      else begin
        Str := sLeftSide + Value + sRightSide;
        ProperSetLine(CaretY -1, Str);
      end;
      // step2: insert left lines of Value
      while P^ <> #0 do
      begin
        if P^ = #13 then
          Inc(P);
        if P^ = #10 then
          Inc(P);
        Inc(fCaretY);
        Include(fStatusChanges, scCaretY);
        Start := P;
        P := GetEOL(Start);
        if P = Start then
        begin
          if p^ <> #0 then
            Str := ''
          else
            Str := sRightSide;
        end
        else begin
          SetString(Str, Start, P - Start);
          if p^ = #0 then
            Str := Str + sRightSide
        end;
        ProperSetLine(CaretY -1, Str);
        Inc(Result);
      end;
      if eoTrimTrailingSpaces in Options then
        if sRightSide = '' then
          fCaretX := GetExpandedLength(Str, TabWidth) + 1
        else
          fCaretX := 1 + Length(Lines[CaretY - 1]) - Length(TrimTrailingSpaces(sRightSide))
      else fCaretX := 1 + Length(Lines[CaretY - 1]) - Length(sRightSide);
      StatusChanged([scCaretX]);
    end;

    function InsertColumn: Integer;
    var
      Str: UnicodeString;
      Start: PWideChar;
      P: PWideChar;
      Len: Integer;
      InsertPos: Integer;
      LineBreakPos: TBufferCoord;
    begin
      Result := 0;
      // Insert string at current position
      InsertPos := CaretX;
      Start := PWideChar(Value);
      repeat
        P := GetEOL(Start);
        if P <> Start then
        begin
          SetLength(Str, P - Start);
          Move(Start^, Str[1], (P - Start) * sizeof(WideChar));
          if CaretY > Lines.Count then
          begin
            Inc(Result);
            TempString := UnicodeStringOfChar(#32, InsertPos - 1) + Str;
            Lines.Add('');
            if AddToUndoList then
            begin
              LineBreakPos.Line := CaretY -1;
              LineBreakPos.Char := Length(Lines[CaretY - 2]) + 1;
              fUndoList.AddChange(crLineBreak, LineBreakPos, LineBreakPos, '', smNormal);
            end;
          end
          else begin
            TempString := Lines[CaretY - 1];
            Len := Length(TempString);
            if Len < InsertPos then
            begin
              TempString :=
                TempString + UnicodeStringOfChar(#32, InsertPos - Len - 1) + Str
            end
            else
                Insert(Str, TempString, InsertPos);
          end;
          ProperSetLine(CaretY - 1, TempString);
          // Add undo change here from PasteFromClipboard
          if AddToUndoList then
          begin
            fUndoList.AddChange(crPaste, BufferCoord(InsertPos, CaretY),
               BufferCoord(InsertPos + (P - Start), CaretY), '', fActiveSelectionMode);
          end;
        end;
        if P^ = #13 then
        begin
          Inc(P);
          if P^ = #10 then
            Inc(P);
          Inc(fCaretY);
          Include(fStatusChanges, scCaretY);
        end;
        Start := P;
      until P^ = #0;
      Inc(fCaretX, Length(Str));
      Include(fStatusChanges, scCaretX);
    end;

    function InsertLine: Integer;
    var
      Start: PWideChar;
      P: PWideChar;
      Str: UnicodeString;
      n: Integer;
    begin
      Result := 0;
      fCaretX := 1;
      // Insert string before current line
      Start := PWideChar(Value);
      repeat
        P := GetEOL(Start);
        if P <> Start then
        begin
          SetLength(Str, P - Start);
          Move(Start^, Str[1], (P - Start) * sizeof(WideChar));
        end
        else
          Str := '';
        if (P^ = #0) then
        begin
          n := Lines.Count;
          if (n >= CaretY) then
            Lines[CaretY - 1] := Str + Lines[CaretY - 1]
          else
            Lines.Add(Str);
          if eoTrimTrailingSpaces in Options then
            Lines[CaretY - 1] := TrimTrailingSpaces(Lines[CaretY - 1]);
          fCaretX := 1 + Length(Str);
        end
        else begin
          //--------- KV from SynEditStudio
          if (CaretY = Lines.Count) or InsertMode then
          begin
            Lines.Insert(CaretY -1, '');
            Inc(Result);
          end;
          //---------
          ProperSetLine(CaretY - 1, Str);
          Inc(fCaretY);
          Include(fStatusChanges, scCaretY);
          Inc(Result);
          if P^ = #13 then
            Inc(P);
          if P^ = #10 then
            Inc(P);
          Start := P;
        end;
      until P^ = #0;
      StatusChanged([scCaretX]);
    end;

  var
    StartLine: Integer;
    StartCol: Integer;
    InsertedLines: Integer;
  begin
    if Value = '' then
      Exit;

    StartLine := CaretY;
    StartCol := CaretX;
    case PasteMode of
      smNormal:
        InsertedLines := InsertNormal;
      smColumn:
        InsertedLines := InsertColumn;
      smLine:
        InsertedLines := InsertLine;
    else
      InsertedLines := 0;
    end;
    // We delete selected based on the current selection mode, but paste
    // what's on the clipboard according to what it was when copied.
    // Update marks
    if InsertedLines > 0 then
    begin
      if (PasteMode = smNormal) and (StartCol > 1) then
        Inc(StartLine);
      DoLinesInserted(StartLine, InsertedLines);
    end;
    // Force caret reset
    InternalCaretXY := CaretXY;
  end;

begin
  IncPaintLock;
  Lines.BeginUpdate;
  try
    BB := BlockBegin;
    BE := BlockEnd;
    if SelAvail then
    begin
      DeleteSelection;
      InternalCaretXY := BB;
    end;
    if (Value <> nil) and (Value[0] <> #0) then
      InsertText;
    if CaretY < 1 then
      InternalCaretY := 1;
  finally
    Lines.EndUpdate;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.SynSetText(const Value: UnicodeString);
begin
  Lines.Text := Value;
end;

procedure TCustomSynEdit.SetTopLine(Value: Integer);
var
  Delta: Integer;
{$IFDEF SYN_CLX}
  iClip: TRect;
{$ENDIF}
begin
  if (eoScrollPastEof in Options) then
    Value := Min(Value, DisplayLineCount)
  else
    Value := Min(Value, DisplayLineCount - fLinesInWindow + 1);
  Value := Max(Value, 1);
  if Value <> TopLine then
  begin
    Delta := TopLine - Value;
    fTopLine := Value;
    if Abs(Delta) < fLinesInWindow then
{$IFDEF SYN_CLX}
    begin
      iClip := GetClientRect;
      ScrollWindow(Self, 0, fTextHeight * Delta, @iClip);
    end
{$ELSE}
      ScrollWindow(Handle, 0, fTextHeight * Delta, nil, nil)
{$ENDIF}
    else
      Invalidate;

    UpdateWindow(Handle);
    UpdateScrollBars;
    StatusChanged([scTopLine]);
  end;
end;

procedure TCustomSynEdit.ShowCaret;
begin
  if not (eoNoCaret in Options) and not (sfCaretVisible in fStateFlags) then
  begin
{$IFDEF SYN_CLX}
    kTextDrawer.ShowCaret(Self);
{$ELSE}
    if Windows.ShowCaret(Handle) then
{$ENDIF}
      Include(fStateFlags, sfCaretVisible);
  end;
end;

procedure TCustomSynEdit.UpdateCaret;
var
  CX, CY: Integer;
  iClientRect: TRect;
  vCaretDisplay: TDisplayCoord;
  vCaretPix: TPoint;
{$IFNDEF SYN_CLX}
  cf: TCompositionForm;
{$ENDIF}
begin
  if (PaintLock <> 0) or not (Focused or FAlwaysShowCaret) then
    Include(fStateFlags, sfCaretChanged)
  else
  begin
    Exclude(fStateFlags, sfCaretChanged);
    vCaretDisplay := DisplayXY;
    if WordWrap and (vCaretDisplay.Column > CharsInWindow + 1) then
      vCaretDisplay.Column := CharsInWindow + 1;
    vCaretPix := RowColumnToPixels(vCaretDisplay);
    CX := vCaretPix.X + FCaretOffset.X;
    CY := vCaretPix.Y + FCaretOffset.Y;
    iClientRect := GetClientRect;
    Inc(iClientRect.Left, fGutterWidth);
    if (CX >= iClientRect.Left) and (CX < iClientRect.Right)
      and (CY >= iClientRect.Top) and (CY < iClientRect.Bottom) then
    begin
      SetCaretPos(CX, CY);
      ShowCaret;
    end
    else
    begin
      SetCaretPos(CX, CY);
      HideCaret;
    end;
{$IFNDEF SYN_CLX}
    cf.dwStyle := CFS_POINT;
    cf.ptCurrentPos := Point(CX, CY);
    ImmSetCompositionWindow(ImmGetContext(Handle), @cf);
{$ENDIF}
  end;
end;

procedure TCustomSynEdit.UpdateScrollBars;
var
  nMaxScroll: Integer;
{$IFNDEF SYN_CLX}
  ScrollInfo: TScrollInfo;
  iRightChar: Integer;
{$ELSE}
  iClientRect: TRect;

  procedure CalcScrollbarsVisible;
  begin
    if not HandleAllocated or (PaintLock <> 0) then
      Include(fStateFlags, sfScrollbarChanged)
    else begin
      Exclude(fStateFlags, sfScrollbarChanged);
      if fScrollBars <> ssNone then
      begin
        if (fScrollBars in [ssBoth, ssHorizontal]) and (not WordWrap) then
        begin
          if eoScrollPastEol in Options then
            nMaxScroll := MaxScrollWidth
          else
            nMaxScroll := Max(TSynEditStringList(Lines).LengthOfLongestLine, 1);

          FHScrollBar.Min := 1;
          FHScrollBar.Max := nMaxScroll; // Qt handles values above MAX_SCROLL
          FHScrollBar.Position := LeftChar;
          FHScrollBar.LargeChange := CharsInWindow - Ord(eoScrollByOneLess in fOptions);

          if eoHideShowScrollbars in Options then
            FHScrollBar.Visible := nMaxScroll > CharsInWindow
          else FHScrollBar.Visible := True;

        end
        else
          FHScrollBar.Visible := False;

        if fScrollBars in [ssBoth, ssVertical] then
        begin
          nMaxScroll := DisplayLineCount;
          if eoScrollPastEof in Options then
            Inc(nMaxScroll, LinesInWindow - 1);

          FVScrollBar.Min := 1;
          FVScrollBar.Max := Max(1, nMaxScroll);
          FVScrollBar.LargeChange := LinesInWindow shr Ord(eoHalfPageScroll in fOptions);
          FVScrollBar.Position := TopLine;

          if eoHideShowScrollbars in Options then
            FVScrollBar.Visible := nMaxScroll > LinesInWindow
          else
            FVScrollBar.Visible := True;
        end
        else
          FVScrollBar.Visible:=FALSE;
      end
      else
      begin
        FHScrollBar.Visible := False;
        FVScrollBar.Visible := False;
      end;
    end;
  end;

{$ENDIF}
begin
{$IFNDEF SYN_CLX}
  if not HandleAllocated or (PaintLock <> 0) then
    Include(fStateFlags, sfScrollbarChanged)
  else begin
    Exclude(fStateFlags, sfScrollbarChanged);
    if fScrollBars <> ssNone then
    begin
      ScrollInfo.cbSize := SizeOf(ScrollInfo);
      ScrollInfo.fMask := SIF_ALL;
      if not(eoHideShowScrollbars in Options) then
      begin
        ScrollInfo.fMask := ScrollInfo.fMask or SIF_DISABLENOSCROLL;
      end;

      if Visible then SendMessage(Handle, WM_SETREDRAW, 0, 0);

      if (fScrollBars in [{$IFDEF SYN_COMPILER_17_UP}TScrollStyle.{$ENDIF}ssBoth, {$IFDEF SYN_COMPILER_17_UP}TScrollStyle.{$ENDIF}ssHorizontal]) and not WordWrap then
      begin
        if eoScrollPastEol in Options then
          nMaxScroll := MaxScrollWidth
        else
          nMaxScroll := Max(TSynEditStringList(Lines).LengthOfLongestLine, 1);
        if nMaxScroll <= MAX_SCROLL then
        begin
          ScrollInfo.nMin := 1;
          ScrollInfo.nMax := nMaxScroll;
          ScrollInfo.nPage := CharsInWindow;
          ScrollInfo.nPos := LeftChar;
        end
        else begin
          ScrollInfo.nMin := 0;
          ScrollInfo.nMax := MAX_SCROLL;
          ScrollInfo.nPage := MulDiv(MAX_SCROLL, CharsInWindow, nMaxScroll);
          ScrollInfo.nPos := MulDiv(MAX_SCROLL, LeftChar, nMaxScroll);
        end;

        ShowScrollBar(Handle, SB_HORZ, not(eoHideShowScrollbars in Options) or
          (ScrollInfo.nMin = 0) or (ScrollInfo.nMax > CharsInWindow));
        SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);

        //Now for the arrows
        if (eoDisableScrollArrows in Options) or (nMaxScroll <= CharsInWindow) then
        begin
          iRightChar := LeftChar + CharsInWindow -1;
          if (LeftChar <= 1) and (iRightChar >= nMaxScroll) then
          begin
            EnableScrollBar(Handle, SB_HORZ, ESB_DISABLE_BOTH);
          end
          else begin
            EnableScrollBar(Handle, SB_HORZ, ESB_ENABLE_BOTH);
            if (LeftChar <= 1) then
              EnableScrollBar(Handle, SB_HORZ, ESB_DISABLE_LEFT)
            else if iRightChar >= nMaxScroll then
              EnableScrollBar(Handle, SB_HORZ, ESB_DISABLE_RIGHT)
          end;
        end
        else
          EnableScrollBar(Handle, SB_HORZ, ESB_ENABLE_BOTH);
      end
      else
        ShowScrollBar(Handle, SB_HORZ, False);

      if fScrollBars in [ssBoth, ssVertical] then
      begin
        nMaxScroll := DisplayLineCount;
        if (eoScrollPastEof in Options) then
          Inc(nMaxScroll, LinesInWindow - 1);
        if nMaxScroll <= MAX_SCROLL then
        begin
          ScrollInfo.nMin := 1;
          ScrollInfo.nMax := Max(1, nMaxScroll);
          ScrollInfo.nPage := LinesInWindow;
          ScrollInfo.nPos := TopLine;
        end
        else begin
          ScrollInfo.nMin := 0;
          ScrollInfo.nMax := MAX_SCROLL;
          ScrollInfo.nPage := MulDiv(MAX_SCROLL, LinesInWindow, nMaxScroll);
          ScrollInfo.nPos := MulDiv(MAX_SCROLL, TopLine, nMaxScroll);
        end;

        ShowScrollBar(Handle, SB_VERT, not(eoHideShowScrollbars in Options) or
          (ScrollInfo.nMin = 0) or (ScrollInfo.nMax > LinesInWindow));
        SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);

        if (eoDisableScrollArrows in Options) or (nMaxScroll <= LinesInWindow) then
        begin
          if (TopLine <= 1) and (nMaxScroll <= LinesInWindow) then
          begin
            EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_BOTH);
          end
          else begin
            EnableScrollBar(Handle, SB_VERT, ESB_ENABLE_BOTH);
            if (TopLine <= 1) then
              EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_UP)
            else if ((DisplayLineCount - TopLine - LinesInWindow + 1) = 0) then
              EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_DOWN);
          end;
        end
        else
          EnableScrollBar(Handle, SB_VERT, ESB_ENABLE_BOTH);

        if Visible then SendMessage(Handle, WM_SETREDRAW, -1, 0);
        if fPaintLock=0 then
           Invalidate;

      end
      else
        ShowScrollBar(Handle, SB_VERT, False);

    end {endif fScrollBars <> ssNone}
    else
      ShowScrollBar(Handle, SB_BOTH, False);
  end;
{$ELSE}
  if FHScrollBar<>nil then
    begin
      CalcScrollBarsVisible;

      iClientRect := GetClientRect;

      FHScrollBar.Left := iClientRect.Left;
      FHScrollBar.Top := iClientRect.Bottom;
      FHScrollBar.Width := iClientRect.Right - iClientRect.Left;

      FVScrollBar.Top := iClientRect.Top;
      FVScrollBar.Left := iClientRect.Right;
      FVScrollBar.Height := iClientRect.Bottom - iClientRect.Top;
    end;
{$ENDIF}
end;

{$IFDEF SYN_CLX}
procedure TCustomSynEdit.ScrollEvent(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  ScrollKind: TScrollBarKind;
begin
  if Sender = FHScrollBar then
  begin
    ScrollKind := sbHorizontal;
    LeftChar := ScrollPos;
  end
  else if Sender = FVScrollBar then
  begin
    ScrollKind := sbVertical;
    TopLine := ScrollPos;
  end
  else
    Exit;
  if Visible and CanFocus and not (csDesigning in ComponentState) then
    SetFocus
  else
    UpdateCaret;
  if Assigned(OnScroll) then OnScroll(Self,ScrollKind);
end;

function TCustomSynEdit.GetClientRect: TRect;
begin
  Result := Inherited GetClientRect;
  if FHScrollBar.Visible then
    Result.Bottom := Result.Bottom - CYHSCROLL;
  if FVScrollBar.Visible then
    Result.Right := Result.Right - CXVSCROLL;
  if BorderStyle <> bsNone then
    InflateRect(Result, -FrameWidth, -FrameWidth);
end;

function TCustomSynEdit.GetClientOrigin: TPoint;
begin
  Result := inherited GetClientOrigin;
  if BorderStyle <> bsNone then
  begin
    Inc(Result.X, FrameWidth);
    Inc(Result.Y, FrameWidth);
  end;
end;

procedure TCustomSynEdit.Resize;
begin
  inherited Resize;
  SizeOrFontChanged(False);
end;

function TCustomSynEdit.WidgetFlags: Integer;
begin
  Result := Integer(WidgetFlags_WRepaintNoErase);
end;
{$ENDIF}

{$IFDEF SYN_COMPILER_6_UP}
function TCustomSynEdit.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
const
  WHEEL_DIVISOR = 120; // Mouse Wheel standard
var
  iWheelClicks: Integer;
  iLinesToScroll: Integer;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if Result then
    Exit;
{$IFDEF SYN_CLX}
  if ssCtrl in Application.KeyState then
{$ELSE}
  if GetKeyState(SYNEDIT_CONTROL) < 0 then
{$ENDIF}
    iLinesToScroll := LinesInWindow shr Ord(eoHalfPageScroll in fOptions)
  else
    iLinesToScroll := 3;
  Inc(fMouseWheelAccumulator, WheelDelta);
  iWheelClicks := fMouseWheelAccumulator div WHEEL_DIVISOR;
  fMouseWheelAccumulator := fMouseWheelAccumulator mod WHEEL_DIVISOR;
  TopLine := TopLine - iWheelClicks * iLinesToScroll;
  Update;
  if Assigned(OnScroll) then OnScroll(Self,sbVertical);
  Result := True;
end;
{$ENDIF}

{$IFDEF SYN_CLX}
procedure TCustomSynEdit.KeyString(var S: UnicodeString; var Handled: Boolean);
var
  i: Integer;
begin
  inherited;
  Handled := True;
  for i := 1 to Length(S) do
    DoKeyPressW(S[i]);
end;

function TCustomSynEdit.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: UnicodeString): Boolean;
begin
  if ((Key = Key_Return) or (Key = Key_Enter)) then
    Result := WantReturns
  else
    Result := inherited NeedKey(Key, Shift, KeyText);
end;
{$ENDIF SYN_CLX}

{$IFNDEF SYN_CLX}
procedure TCustomSynEdit.WMCaptureChanged(var Msg: TMessage);
begin
  fScrollTimer.Enabled := False;
  inherited;
end;

procedure TCustomSynEdit.WMChar(var Msg: TWMChar);
begin
{$IFNDEF UNICODE}
  if not Win32PlatformIsUnicode then
    Msg.CharCode := Word(KeyUnicode(AnsiChar(Msg.CharCode)));
{$ENDIF}

  DoKeyPressW(Msg);
end;

procedure TCustomSynEdit.WMClear(var Msg: TMessage);
begin
  if not ReadOnly then
    SelText := '';
end;

procedure TCustomSynEdit.WMCopy(var Message: TMessage);
begin
  CopyToClipboard;
  Message.Result := ord(True);
end;

procedure TCustomSynEdit.WMCut(var Message: TMessage);
begin
  if not ReadOnly then
    CutToClipboard;
  Message.Result := ord(True);
end;

procedure TCustomSynEdit.WMDropFiles(var Msg: TMessage);
var
  i, iNumberDropped: Integer;
  FileNameA: array[0..MAX_PATH - 1] of AnsiChar;
  FileNameW: array[0..MAX_PATH - 1] of WideChar;
  Point: TPoint;
  FilesList: TUnicodeStringList;
begin
  try
    if Assigned(fOnDropFiles) then
    begin
      FilesList := TUnicodeStringList.Create;
      try
        iNumberDropped := DragQueryFile(THandle(Msg.wParam), Cardinal(-1),
          nil, 0);
        DragQueryPoint(THandle(Msg.wParam), Point);

        if Win32PlatformIsUnicode then
          for i := 0 to iNumberDropped - 1 do
          begin
            DragQueryFileW(THandle(Msg.wParam), i, FileNameW,
              sizeof(FileNameW) div 2);
            FilesList.Add(FileNameW)
          end
        else
          for i := 0 to iNumberDropped - 1 do
          begin
            DragQueryFileA(THandle(Msg.wParam), i, FileNameA,
              sizeof(FileNameA));
            FilesList.Add(UnicodeString(FileNameA))
          end;
        fOnDropFiles(Self, Point.X, Point.Y, FilesList);
      finally
        FilesList.Free;
      end;
    end;
  finally
    Msg.Result := 0;
    DragFinish(THandle(Msg.wParam));
  end;
end;

procedure TCustomSynEdit.WMDestroy(var Message: TWMDestroy);
begin
  {$IFDEF UNICODE}
  // assign WindowText here, otherwise the VCL will call GetText twice
  if WindowText = nil then
     WindowText := Lines.GetText;
  {$ENDIF}
  inherited;
end;

procedure TCustomSynEdit.WMEraseBkgnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TCustomSynEdit.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := Msg.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
  if fWantTabs then
    Msg.Result := Msg.Result or DLGC_WANTTAB;
  if fWantReturns then
    Msg.Result := Msg.Result or DLGC_WANTALLKEYS;
end;

procedure TCustomSynEdit.WMGetText(var Msg: TWMGetText);
begin
  if HandleAllocated and IsWindowUnicode(Handle) then
  begin
    WStrLCopy(PWideChar(Msg.Text), PWideChar(Text), Msg.TextMax - 1);
    Msg.Result := WStrLen(PWideChar(Msg.Text));
  end
  else
  begin
   {$IFDEF SYN_COMPILER_18_UP}AnsiStrings.{$ENDIF}StrLCopy(PAnsiChar(Msg.Text), PAnsiChar(AnsiString(Text)), Msg.TextMax - 1);
    Msg.Result := {$IFDEF SYN_COMPILER_18_UP}AnsiStrings.{$ENDIF}StrLen(PAnsiChar(Msg.Text));
  end;
end;

procedure TCustomSynEdit.WMGetTextLength(var Msg: TWMGetTextLength);
begin
{$IFDEF SYN_COMPILER_4_UP}
  // Avoid (useless) temporary copy of WindowText while window is recreated
  // because of docking.
  if csDocking in ControlState then
    Msg.Result := 0
  else
{$ENDIF}
    Msg.Result := Length(Text);
end;

procedure TCustomSynEdit.WMHScroll(var Msg: TWMScroll);
var
  iMaxWidth: integer;
begin
  Msg.Result := 0;
  case Msg.ScrollCode of
      // Scrolls to start / end of the line
    SB_LEFT: LeftChar := 1;
    SB_RIGHT:
      if eoScrollPastEol in Options then
        LeftChar := MaxScrollWidth - CharsInWindow +1
      else
        // Simply set LeftChar property to the LengthOfLongestLine,
        // it would do the range checking and constrain the value if necessary
        LeftChar := TSynEditStringList(Lines).LengthOfLongestLine;
      // Scrolls one char left / right
    SB_LINERIGHT: LeftChar := LeftChar + 1;
    SB_LINELEFT: LeftChar := LeftChar - 1;
      // Scrolls one page of chars left / right
    SB_PAGERIGHT: LeftChar := LeftChar
      + (fCharsInWindow - Ord(eoScrollByOneLess in fOptions));
    SB_PAGELEFT: LeftChar := LeftChar
      - (fCharsInWindow - Ord(eoScrollByOneLess in fOptions));
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
    begin
      FIsScrolling := True;
      if eoScrollPastEol in Options then
        iMaxWidth := MaxScrollWidth
      else
        iMaxWidth := Max(TSynEditStringList(Lines).LengthOfLongestLine, 1);
      if iMaxWidth > MAX_SCROLL then
        LeftChar := MulDiv(iMaxWidth, Msg.Pos, MAX_SCROLL)
      else
        LeftChar := Msg.Pos;
    end;
    SB_ENDSCROLL: FIsScrolling := False;
  end;
  if Assigned(OnScroll) then OnScroll(Self,sbHorizontal);
end;

function IsWindows98orLater: Boolean;
begin
  Result := (Win32MajorVersion > 4) or
    (Win32MajorVersion = 4) and (Win32MinorVersion > 0);
end;

procedure TCustomSynEdit.WMImeChar(var Msg: TMessage);
begin
  // do nothing here, the IME string is retrieved in WMImeComposition

  // Handling the WM_IME_CHAR message stops Windows from sending WM_CHAR
  // messages while using the IME
end;

procedure TCustomSynEdit.WMImeComposition(var Msg: TMessage);
var
  imc: HIMC;
  PW: PWideChar;
  PA: PAnsiChar;
  PWLength: Integer;
  ImeCount: Integer;
begin
  if (Msg.LParam and GCS_RESULTSTR) <> 0 then
  begin
    imc := ImmGetContext(Handle);
    try
      if IsWindows98orLater then
      begin
        ImeCount := ImmGetCompositionStringW(imc, GCS_RESULTSTR, nil, 0);
        // ImeCount is always the size in bytes, also for Unicode
        GetMem(PW, ImeCount + sizeof(WideChar));
        try
          ImmGetCompositionStringW(imc, GCS_RESULTSTR, PW, ImeCount);
          PW[ImeCount div sizeof(WideChar)] := #0;
          CommandProcessor(ecImeStr, #0, PW);
        finally
          FreeMem(PW);
        end;
      end
      else
      begin
        ImeCount := ImmGetCompositionStringA(imc, GCS_RESULTSTR, nil, 0);
        // ImeCount is always the size in bytes, also for Unicode
        GetMem(PA, ImeCount + sizeof(AnsiChar));
        try
          ImmGetCompositionStringA(imc, GCS_RESULTSTR, PA, ImeCount);
          PA[ImeCount] := #0;

          PWLength := MultiByteToWideChar(DefaultSystemCodePage, 0, PA, ImeCount,
            nil, 0);
          GetMem(PW, (PWLength + 1) * sizeof(WideChar));
          try
            MultiByteToWideChar(DefaultSystemCodePage, 0, PA, ImeCount,
              PW, PWLength);
            CommandProcessor(ecImeStr, #0, PW);
          finally
            FreeMem(PW);
          end;
        finally
          FreeMem(PA);
        end;
      end;
    finally
      ImmReleaseContext(Handle, imc);
    end;
  end;
  inherited;
end;

procedure TCustomSynEdit.WMImeNotify(var Msg: TMessage);
var
  imc: HIMC;
  LogFontW: TLogFontW;
  LogFontA: TLogFontA;
begin
  with Msg do
  begin
    case WParam of
      IMN_SETOPENSTATUS:
        begin
          imc := ImmGetContext(Handle);
          if imc <> 0 then
          begin
            if IsWindows98orLater then
            begin
              GetObjectW(Font.Handle, SizeOf(TLogFontW), @LogFontW);
              ImmSetCompositionFontW(imc, @LogFontW);
            end
            else
            begin
              GetObjectA(Font.Handle, SizeOf(TLogFontA), @LogFontA);
              ImmSetCompositionFontA(imc, @LogFontA);
            end;
            ImmReleaseContext(Handle, imc);
          end;
        end;
    end;
  end;
  inherited;
end;

procedure TCustomSynEdit.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  CommandProcessor(ecLostFocus, #0, nil);
  //Added check for focused to prevent caret disappearing problem
  if Focused or FAlwaysShowCaret then
    exit;
  HideCaret;
  Windows.DestroyCaret;
  if FHideSelection and SelAvail then
    InvalidateSelection;
end;

procedure TCustomSynEdit.WMPaste(var Message: TMessage);
begin
  if not ReadOnly then
    PasteFromClipboard;
  Message.Result := ord(True);
end;

procedure TCustomSynEdit.WMCancelMode(var Message:TMessage);
begin

end;

procedure TCustomSynEdit.WMSetFocus(var Msg: TWMSetFocus);
begin
  CommandProcessor(ecGotFocus, #0, nil);

  InitializeCaret;
  if FHideSelection and SelAvail then
    InvalidateSelection;
end;

procedure TCustomSynEdit.WMSetText(var Msg: TWMSetText);
begin
  Msg.Result := 1;
  try
    if HandleAllocated and IsWindowUnicode(Handle) then
      Text := PWideChar(Msg.Text)
    else
      Text := UnicodeString(PAnsiChar(Msg.Text));
  except
    Msg.Result := 0;
    raise
  end
end;

procedure TCustomSynEdit.WMSize(var Msg: TWMSize);
begin
  inherited;
  SizeOrFontChanged(False);
end;

procedure TCustomSynEdit.WMUndo(var Msg: TMessage);
begin
  Undo;
end;

var
  ScrollHintWnd: THintWindow;

function GetScrollHint: THintWindow;
begin
  if ScrollHintWnd = nil then
    ScrollHintWnd := HintWindowClass.Create(Application);
  Result := ScrollHintWnd;
end;

procedure TCustomSynEdit.WMVScroll(var Msg: TWMScroll);
var
  s: string;
  rc: TRect;
  pt: TPoint;
  ScrollHint: THintWindow;
  ButtonH: Integer;
  ScrollInfo: TScrollInfo;
begin
  Msg.Result := 0;
  case Msg.ScrollCode of
      // Scrolls to start / end of the text
    SB_TOP: TopLine := 1;
    SB_BOTTOM: TopLine := DisplayLineCount;
      // Scrolls one line up / down
    SB_LINEDOWN: TopLine := TopLine + 1;
    SB_LINEUP: TopLine := TopLine - 1;
      // Scrolls one page of lines up / down
    SB_PAGEDOWN: TopLine := TopLine
      + (fLinesInWindow - Ord(eoScrollByOneLess in fOptions));
    SB_PAGEUP: TopLine := TopLine
      - (fLinesInWindow - Ord(eoScrollByOneLess in fOptions));
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
      begin
        FIsScrolling := True;
        if DisplayLineCount > MAX_SCROLL then
          TopLine := MulDiv(LinesInWindow + DisplayLineCount - 1, Msg.Pos,
            MAX_SCROLL)
        else
          TopLine := Msg.Pos;

        if eoShowScrollHint in fOptions then
        begin
          ScrollHint := GetScrollHint;
          ScrollHint.Color := fScrollHintColor;
          case FScrollHintFormat of
            shfTopLineOnly:
              s := Format(SYNS_ScrollInfoFmtTop, [RowToLine(TopLine)]);
            else
              s := Format(SYNS_ScrollInfoFmt, [RowToLine(TopLine),
                RowToLine(TopLine + Min(LinesInWindow, DisplayLineCount-TopLine))]);
          end;

{$IFDEF SYN_COMPILER_3_UP}
          rc := ScrollHint.CalcHintRect(200, s, nil);
{$ELSE}
          rc := Rect(0, 0, TextWidth(ScrollHint.Canvas, s) + 6,
            TextHeight(ScrollHint.Canvas, s) + 4);
{$ENDIF}
          if eoScrollHintFollows in fOptions then
          begin
            ButtonH := GetSystemMetrics(SM_CYVSCROLL);

            FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
            ScrollInfo.cbSize := SizeOf(ScrollInfo);
            ScrollInfo.fMask := SIF_ALL;
            GetScrollInfo(Handle, SB_VERT, ScrollInfo);

            pt := ClientToScreen(Point(ClientWidth - rc.Right - 4,
              ((rc.Bottom - rc.Top) shr 1) +                                    //half the size of the hint window
              Round((ScrollInfo.nTrackPos / ScrollInfo.nMax) *                  //The percentage of the page that has been scrolled
                    (ClientHeight - (ButtonH * 2)))                             //The height minus the arrow buttons
                   + ButtonH));                                                 //The height of the top button
          end
          else
            pt := ClientToScreen(Point(ClientWidth - rc.Right - 4, 10));

          OffsetRect(rc, pt.x, pt.y);
          ScrollHint.ActivateHint(rc, s);
{$IFDEF SYN_COMPILER_3}
          SendMessage(ScrollHint.Handle, WM_NCPAINT, 1, 0);
{$ENDIF}
{$IFNDEF SYN_COMPILER_3_UP}
          ScrollHint.Invalidate;
{$ENDIF}
          ScrollHint.Update;
        end;
      end;
      // Ends scrolling
    SB_ENDSCROLL:
      begin
        FIsScrolling := False;
      if eoShowScrollHint in fOptions then
        ShowWindow(GetScrollHint.Handle, SW_HIDE);
  end;
  end;
  Update;
  if Assigned(OnScroll) then OnScroll(Self,sbVertical);
end;
{$ENDIF}

function TCustomSynEdit.ScanFrom(Index: Integer): Integer;
var
  iRange: TSynEditRange;
begin
  Result := Index;
  if Result >= Lines.Count then Exit;

  if Result = 0 then
    fHighlighter.ResetRange
  else
    fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[Result - 1]);

  repeat
    fHighlighter.SetLine(Lines[Result], Result);
    fHighlighter.NextToEol;
    iRange := fHighlighter.GetRange;
    if TSynEditStringList(Lines).Ranges[Result] = iRange then
      Exit; // avoid the final Decrement
    TSynEditStringList(Lines).Ranges[Result] := iRange;
    Inc(Result);
  until (Result = Lines.Count);
  Dec(Result);
end;

procedure TCustomSynEdit.ListCleared(Sender: TObject);
begin
  if WordWrap then
    fWordWrapPlugin.Reset;

  ClearUndo;
  // invalidate the *whole* client area
  FillChar(fInvalidateRect, SizeOf(TRect), 0);
  Invalidate;
  // set caret and selected block to start of text
  CaretXY := BufferCoord(1, 1);
  // scroll to start of text
  TopLine := 1;
  LeftChar := 1;
  Include(fStatusChanges, scAll);
end;

procedure TCustomSynEdit.ListDeleted(Sender: TObject; aIndex: Integer;
  aCount: Integer);
begin
  if Assigned(fHighlighter) and (Lines.Count > 0) then
    ScanFrom(aIndex);

  if WordWrap then
    fWordWrapPlugin.LinesDeleted(aIndex, aCount);

  InvalidateLines(aIndex + 1, MaxInt);
  InvalidateGutterLines(aIndex + 1, MaxInt);
end;

procedure TCustomSynEdit.ListInserted(Sender: TObject; Index: Integer;
  aCount: Integer);
var
  L: Integer;
  vLastScan: Integer;
begin
  if Assigned(fHighlighter) and (Lines.Count > 0) then
  begin
    vLastScan := Index;
    repeat
      vLastScan := ScanFrom(vLastScan);
      Inc(vLastScan);
    until vLastScan >= Index + aCount;
  end;

  if WordWrap then
    fWordWrapPlugin.LinesInserted(Index, aCount);

  InvalidateLines(Index + 1, MaxInt);
  InvalidateGutterLines(Index + 1, MaxInt);

  if (eoAutoSizeMaxScrollWidth in fOptions) then
  begin
    L := TSynEditStringList(Lines).ExpandedStringLengths[Index];
    if L > MaxScrollWidth then
      MaxScrollWidth := L;
  end;
end;

procedure TCustomSynEdit.ListPutted(Sender: TObject; Index: Integer;
  aCount: Integer);
var
  L: Integer;
  vEndLine: Integer;
begin
  vEndLine := Index +1;
  if WordWrap then
  begin
    if fWordWrapPlugin.LinesPutted(Index, aCount) <> 0 then
      vEndLine := MaxInt;
    InvalidateGutterLines(Index + 1, vEndLine);
  end;
  if Assigned(fHighlighter) then
  begin
    vEndLine := Max(vEndLine, ScanFrom(Index) + 1);
    // If this editor is chained then the real owner of text buffer will probably
    // have already parsed the changes, so ScanFrom will return immediately.
    if fLines <> fOrigLines then
      vEndLine := MaxInt;
  end;
  InvalidateLines(Index + 1, vEndLine);

  if (eoAutoSizeMaxScrollWidth in fOptions) then
  begin
    L := TSynEditStringList(Lines).ExpandedStringLengths[Index];
    if L > MaxScrollWidth then
      MaxScrollWidth := L;
  end;
end;

procedure TCustomSynEdit.ScanRanges;
var
  i: Integer;
begin
  if Assigned(fHighlighter) and (Lines.Count > 0) then begin
    fHighlighter.ResetRange;
    i := 0;
    repeat
      fHighlighter.SetLine(Lines[i], i);
      fHighlighter.NextToEol;
      TSynEditStringList(Lines).Ranges[i] := fHighlighter.GetRange;
      Inc(i);
    until i >= Lines.Count;
  end;
end;

procedure TCustomSynEdit.SetWordBlock(Value: TBufferCoord);
var
  vBlockBegin: TBufferCoord;
  vBlockEnd: TBufferCoord;
  TempString: UnicodeString;

  procedure CharScan;
  var
    cRun: Integer;
  begin
    { search BlockEnd }
    vBlockEnd.Char := Length(TempString);
    for cRun := Value.Char to Length(TempString) do
      if not IsIdentChar(TempString[cRun]) then
      begin
        vBlockEnd.Char := cRun;
        Break;
      end;
    { search BlockBegin }
    vBlockBegin.Char := 1;
    for cRun := Value.Char - 1 downto 1 do
      if not IsIdentChar(TempString[cRun]) then
      begin
        vBlockBegin.Char := cRun + 1;
        Break;
      end;
  end;

begin
  if (eoScrollPastEol in Options) and not WordWrap then
    Value.Char := MinMax(Value.Char, 1, fMaxScrollWidth + 1)
  else
    Value.Char := Max(Value.Char, 1);
  Value.Line := MinMax(Value.Line, 1, Lines.Count);
  TempString := Lines[Value.Line - 1] + #0; //needed for CaretX = LineLength + 1
  if Value.Char > Length(TempString) then
  begin
    InternalCaretXY := BufferCoord(Length(TempString), Value.Line);
    exit;
  end;

  CharScan;

  vBlockBegin.Line := Value.Line;
  vBlockEnd.Line := Value.Line;
  SetCaretAndSelection(vBlockEnd, vBlockBegin, vBlockEnd);
  InvalidateLine(Value.Line);
  StatusChanged([scSelection]);
end;

procedure TCustomSynEdit.DblClick;
var
  ptMouse: TPoint;
begin
  GetCursorPos(ptMouse);
  ptMouse := ScreenToClient(ptMouse);
  if ptMouse.X >= fGutterWidth + 2 then
  begin
    if not (eoNoSelection in fOptions) then
      SetWordBlock(CaretXY);
    inherited;
    Include(fStateFlags, sfDblClicked);
    MouseCapture := False;
  end
  else
    inherited;
end;

function TCustomSynEdit.GetCanUndo: Boolean;
begin
  result := not ReadOnly and fUndoList.CanUndo;
end;

function TCustomSynEdit.GetCanRedo: Boolean;
begin
  result := not ReadOnly and fRedoList.CanUndo;
end;

function TCustomSynEdit.GetCanPaste;
begin
  Result := not ReadOnly and ClipboardProvidesText;
end;

procedure TCustomSynEdit.InsertBlock(const BB, BE: TBufferCoord; ChangeStr: PWideChar;
  AddToUndoList: Boolean);
// used by BlockIndent and Redo
begin
  SetCaretAndSelection(BB, BB, BE);
  ActiveSelectionMode := smColumn;
  SetSelTextPrimitiveEx(smColumn, ChangeStr, AddToUndoList);
  StatusChanged([scSelection]);
end;

procedure TCustomSynEdit.Redo;

  procedure RemoveGroupBreak;
  var
    Item: TSynEditUndoItem;
    OldBlockNumber: Integer;
  begin
    if fRedoList.LastChangeReason = crGroupBreak then
    begin
      OldBlockNumber := UndoList.BlockChangeNumber;
      Item := fRedoList.PopItem;
      try
        UndoList.BlockChangeNumber := Item.ChangeNumber;
        fUndoList.AddGroupBreak;
      finally
        UndoList.BlockChangeNumber := OldBlockNumber;
        Item.Free;
      end;
      UpdateModifiedStatus;
    end;
  end;

var
  Item: TSynEditUndoItem;
  OldChangeNumber: integer;
  SaveChangeNumber: integer;
  FLastChange : TSynChangeReason;
  FAutoComplete: Boolean;
  FPasteAction: Boolean;
  FSpecial1: Boolean;
  FSpecial2: Boolean;
  FKeepGoing: Boolean;
begin
  if ReadOnly then
    exit;

  FLastChange := FRedoList.LastChangeReason;
  FAutoComplete := FLastChange = crAutoCompleteBegin;
  FPasteAction := FLastChange = crPasteBegin;
  FSpecial1 := FLastChange = crSpecial1Begin;
  FSpecial2 := FLastChange = crSpecial2Begin;

  Item := fRedoList.PeekItem;
  if Item <> nil then
  begin
    OldChangeNumber := Item.ChangeNumber;
    SaveChangeNumber := fUndoList.BlockChangeNumber;
    fUndoList.BlockChangeNumber := Item.ChangeNumber;
    try
      repeat
        RedoItem;
        Item := fRedoList.PeekItem;
        if Item = nil then
          FKeepGoing := False
        else begin
          if FAutoComplete then
             FKeepGoing:= (FRedoList.LastChangeReason <> crAutoCompleteEnd)
          else if FPasteAction then
             FKeepGoing:= (FRedoList.LastChangeReason <> crPasteEnd)
          else if FSpecial1 then
             FKeepGoing := (FRedoList.LastChangeReason <> crSpecial1End)
          else if FSpecial2 then
             FKeepGoing := (FRedoList.LastChangeReason <> crSpecial2End)
          else if Item.ChangeNumber = OldChangeNumber then
             FKeepGoing := True
          else begin
            FKeepGoing := ((eoGroupUndo in FOptions) and
              (FLastChange = Item.ChangeReason) and
              not(FLastChange in [crIndent, crUnindent]));
          end;
          FLastChange := Item.ChangeReason;
        end;
      until not(FKeepGoing);

      //we need to eat the last command since it does nothing and also update modified status...
      if (FAutoComplete and (FRedoList.LastChangeReason = crAutoCompleteEnd)) or
         (FPasteAction and (FRedoList.LastChangeReason = crPasteEnd)) or
         (FSpecial1 and (FRedoList.LastChangeReason = crSpecial1End)) or
         (FSpecial2 and (FRedoList.LastChangeReason = crSpecial2End)) then
      begin
        RedoItem;
        UpdateModifiedStatus;
      end;

    finally
      fUndoList.BlockChangeNumber := SaveChangeNumber;
    end;
    RemoveGroupBreak;
  end;
end;

procedure TCustomSynEdit.RedoItem;
var
  Item: TSynEditUndoItem;
  Run, StrToDelete: PWideChar;
  Len: Integer;
  TempString: UnicodeString;
  CaretPt: TBufferCoord;
  ChangeScrollPastEol: boolean;
  BeginX: integer;
begin
  ChangeScrollPastEol := not (eoScrollPastEol in Options);
  Item := fRedoList.PopItem;
  if Assigned(Item) then
  try
    ActiveSelectionMode := Item.ChangeSelMode;
    IncPaintLock;
    Include(fOptions, eoScrollPastEol);
    fUndoList.InsideRedo := True;
    case Item.ChangeReason of
      crCaret:
        begin
          fUndoList.AddChange(Item.ChangeReason, CaretXY, CaretXY, '', fActiveSelectionMode);
          InternalCaretXY := Item.ChangeStartPos;
        end;
      crSelection:
        begin
          fUndoList.AddChange(Item.ChangeReason, BlockBegin, BlockEnd, '', fActiveSelectionMode);
          SetCaretAndSelection(CaretXY, Item.ChangeStartPos, Item.ChangeEndPos);
        end;
      crInsert, crPaste, crDragDropInsert:
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeStartPos);
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PWideChar(Item.ChangeStr),
            False);
          InternalCaretXY := Item.ChangeEndPos;
          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, SelText, Item.ChangeSelMode);
          if Item.ChangeReason = crDragDropInsert then begin
            SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
              Item.ChangeEndPos);
          end;
        end;
      crDeleteAfterCursor, crSilentDeleteAfterCursor:
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          TempString := SelText;
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PWideChar(Item.ChangeStr),
            False);
          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, TempString, Item.ChangeSelMode);
          InternalCaretXY := Item.ChangeEndPos;
        end;
      crDelete, crSilentDelete:
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          TempString := SelText;
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PWideChar(Item.ChangeStr),
            False);
          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, TempString, Item.ChangeSelMode);
          InternalCaretXY := Item.ChangeStartPos;
        end;
      crLineBreak:
        begin
          CaretPt := Item.ChangeStartPos;
          SetCaretAndSelection(CaretPt, CaretPt, CaretPt);
          CommandProcessor(ecLineBreak, #13, nil);
        end;
      crIndent:
        begin
          SetCaretAndSelection(Item.ChangeEndPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, Item.ChangeStr, Item.ChangeSelMode);
        end;
       crUnindent :
         begin // re-delete the (raggered) column
           // Delete string
           StrToDelete := PWideChar(Item.ChangeStr);
           InternalCaretY := Item.ChangeStartPos.Line;
          if Item.ChangeSelMode = smColumn then
            BeginX := Min(Item.ChangeStartPos.Char, Item.ChangeEndPos.Char)
          else
            BeginX := 1;
           repeat
             Run := GetEOL(StrToDelete);
             if Run <> StrToDelete then
             begin
               Len := Run - StrToDelete;
               if Len > 0 then
               begin
                 TempString := Lines[CaretY - 1];
                 Delete(TempString, BeginX, Len);
                 Lines[CaretY - 1] := TempString;
               end;
             end
             else
               Len := 0;
             if Run^ = #13 then
             begin
               Inc(Run);
               if Run^ = #10 then
                 Inc(Run);
               Inc(fCaretY);
             end;
             StrToDelete := Run;
           until Run^ = #0;
          if Item.ChangeSelMode = smColumn then
            SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
              Item.ChangeEndPos)
          else begin
            // restore selection
            CaretPt.Char := Item.ChangeStartPos.Char - fTabWidth;
            CaretPt.Line := Item.ChangeStartPos.Line;
            SetCaretAndSelection( CaretPt, CaretPt,
              BufferCoord(Item.ChangeEndPos.Char - Len, Item.ChangeEndPos.Line) );
          end;
           // add to undo list
           fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
             Item.ChangeEndPos, Item.ChangeStr, Item.ChangeSelMode);
         end;
      crWhiteSpaceAdd:
        begin
          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
             Item.ChangeEndPos, '', Item.ChangeSelMode);
          SetCaretAndSelection(Item.ChangeEndPos, Item.ChangeEndPos,
            Item.ChangeEndPos);
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PWideChar(Item.ChangeStr), True);
          InternalCaretXY := Item.ChangeStartPos;
        end;
    end;
  finally
    fUndoList.InsideRedo := False;
    if ChangeScrollPastEol then
      Exclude(fOptions, eoScrollPastEol);
    Item.Free;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.Undo;

  procedure RemoveGroupBreak;
  var
    Item: TSynEditUndoItem;
    OldBlockNumber: Integer;
  begin
    if fUndoList.LastChangeReason = crGroupBreak then
    begin
      OldBlockNumber := RedoList.BlockChangeNumber;
      try
        Item := fUndoList.PopItem;
        RedoList.BlockChangeNumber := Item.ChangeNumber;
        Item.Free;
        fRedoList.AddGroupBreak;
      finally
        RedoList.BlockChangeNumber := OldBlockNumber;
      end;
    end;
  end;

var
  Item: TSynEditUndoItem;
  OldChangeNumber: integer;
  SaveChangeNumber: integer;
  FLastChange : TSynChangeReason;
  FAutoComplete: Boolean;
  FPasteAction: Boolean;
  FSpecial1: Boolean;
  FSpecial2: Boolean;
  FKeepGoing: Boolean;
begin
  if ReadOnly then
    exit;

  RemoveGroupBreak;

  FLastChange := FUndoList.LastChangeReason;
  FAutoComplete := FLastChange = crAutoCompleteEnd;
  FPasteAction := FLastChange = crPasteEnd;
  FSpecial1 := FLastChange = crSpecial1End;
  FSpecial2 := FLastChange = crSpecial2End;

  Item := fUndoList.PeekItem;
  if Item <> nil then
  begin
    OldChangeNumber := Item.ChangeNumber;
    SaveChangeNumber := fRedoList.BlockChangeNumber;
    fRedoList.BlockChangeNumber := Item.ChangeNumber;

    try
      repeat
        UndoItem;
        Item := fUndoList.PeekItem;
        if Item = nil then
          FKeepGoing := False
        else begin
          if FAutoComplete then
             FKeepGoing := (FUndoList.LastChangeReason <> crAutoCompleteBegin)
          else if FPasteAction then
             FKeepGoing := (FUndoList.LastChangeReason <> crPasteBegin)
          else if FSpecial1 then
             FKeepGoing := (FUndoList.LastChangeReason <> crSpecial1Begin)
          else if FSpecial2 then
             FKeepGoing := (FUndoList.LastChangeReason <> crSpecial2Begin)
          else if Item.ChangeNumber = OldChangeNumber then
             FKeepGoing := True
          else begin
            FKeepGoing := ((eoGroupUndo in FOptions) and
              (FLastChange = Item.ChangeReason) and
              not(FLastChange in [crIndent, crUnindent]));
          end;
          FLastChange := Item.ChangeReason;
        end;
      until not(FKeepGoing);

      //we need to eat the last command since it does nothing and also update modified status...
      if (FAutoComplete and (FUndoList.LastChangeReason = crAutoCompleteBegin)) or
         (FPasteAction and (FUndoList.LastChangeReason = crPasteBegin)) or
         (FSpecial1 and (FUndoList.LastChangeReason = crSpecial1Begin)) or
         (FSpecial2 and (FUndoList.LastChangeReason = crSpecial2Begin)) then
      begin
        UndoItem;
        UpdateModifiedStatus;
       end;

    finally
      fRedoList.BlockChangeNumber := SaveChangeNumber;
    end;
  end;
end;

procedure TCustomSynEdit.UndoItem;
var
  Item: TSynEditUndoItem;
  TmpPos: TBufferCoord;
  TmpStr: UnicodeString;
  ChangeScrollPastEol: Boolean;
  BeginX: Integer;
begin
  ChangeScrollPastEol := not (eoScrollPastEol in Options);
  Item := fUndoList.PopItem;
  if Assigned(Item) then
  try
    ActiveSelectionMode := Item.ChangeSelMode;
    IncPaintLock;
    Include(fOptions, eoScrollPastEol);
    case Item.ChangeReason of
      crCaret:
        begin
          fRedoList.AddChange(Item.ChangeReason, CaretXY, CaretXY, '', fActiveSelectionMode);
          InternalCaretXY := Item.ChangeStartPos;
        end;
      crSelection:
        begin
          fRedoList.AddChange(Item.ChangeReason, BlockBegin, BlockEnd, '', fActiveSelectionMode);
          SetCaretAndSelection(CaretXY, Item.ChangeStartPos, Item.ChangeEndPos);
        end;
      crInsert, crPaste, crDragDropInsert:
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          TmpStr := SelText;
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PWideChar(Item.ChangeStr),
            False);
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, TmpStr, Item.ChangeSelMode);
          InternalCaretXY := Item.ChangeStartPos;
        end;
      crDeleteAfterCursor, crDelete,
      crSilentDelete, crSilentDeleteAfterCursor,
      crDeleteAll:
        begin
          // If there's no selection, we have to set
          // the Caret's position manualy.
          if Item.ChangeSelMode = smColumn then
            TmpPos := BufferCoord(
              Min(Item.ChangeStartPos.Char, Item.ChangeEndPos.Char),
              Min(Item.ChangeStartPos.Line, Item.ChangeEndPos.Line))
          else
            TmpPos := TBufferCoord(MinPoint(
              TPoint(Item.ChangeStartPos), TPoint(Item.ChangeEndPos)));
          if (Item.ChangeReason in [crDeleteAfterCursor,
            crSilentDeleteAfterCursor]) and (TmpPos.Line > Lines.Count) then
          begin
            InternalCaretXY := BufferCoord(1, Lines.Count);
            fLines.Add('');
          end;
          CaretXY := TmpPos;
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PWideChar(Item.ChangeStr),
            False );
          if Item.ChangeReason in [crDeleteAfterCursor,
            crSilentDeleteAfterCursor]
          then
            TmpPos := Item.ChangeStartPos
          else
            TmpPos := Item.ChangeEndPos;
          if Item.ChangeReason in [crSilentDelete, crSilentDeleteAfterCursor]
          then
            InternalCaretXY := TmpPos
          else begin
            SetCaretAndSelection(TmpPos, Item.ChangeStartPos,
              Item.ChangeEndPos);
          end;
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, '', Item.ChangeSelMode);
          if Item.ChangeReason = crDeleteAll then begin
            InternalCaretXY := BufferCoord(1, 1);
            fBlockEnd := BufferCoord(1, 1);
          end;
          EnsureCursorPosVisible;
        end;
      crLineBreak:
        begin
          // If there's no selection, we have to set
          // the Caret's position manualy.
          InternalCaretXY := Item.ChangeStartPos;
          if CaretY > 0 then
          begin
            TmpStr := Lines.Strings[CaretY - 1];
            if (Length(TmpStr) < CaretX - 1)
              and (LeftSpaces(Item.ChangeStr) = 0)
            then
              TmpStr := TmpStr + UnicodeStringOfChar(#32, CaretX - 1 - Length(TmpStr));
            ProperSetLine(CaretY - 1, TmpStr + Item.ChangeStr);
            Lines.Delete(Item.ChangeEndPos.Line);
          end
          else
            ProperSetLine(CaretY - 1, Item.ChangeStr);
          DoLinesDeleted(CaretY + 1, 1);
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, '', Item.ChangeSelMode);
        end;
      crIndent:
        begin
          SetCaretAndSelection(Item.ChangeEndPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, Item.ChangeStr, Item.ChangeSelMode);
        end;
       crUnindent: // reinsert the (raggered) column that was deleted
         begin
           // reinsert the string
          if Item.ChangeSelMode <> smColumn then
            InsertBlock(BufferCoord(1, Item.ChangeStartPos.Line),
              BufferCoord(1, Item.ChangeEndPos.Line),
              PWideChar(Item.ChangeStr), False)
          else
          begin
            BeginX := Min( Item.ChangeStartPos.Char, Item.ChangeEndPos.Char );
            InsertBlock(BufferCoord(BeginX, Item.ChangeStartPos.Line),
              BufferCoord(BeginX, Item.ChangeEndPos.Line),
              PWideChar(Item.ChangeStr), False);
          end;
           SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
             Item.ChangeEndPos);
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
             Item.ChangeEndPos, Item.ChangeStr, Item.ChangeSelMode);
        end;
      crWhiteSpaceAdd:
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          TmpStr := SelText;
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PWideChar(Item.ChangeStr), True);
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, TmpStr, Item.ChangeSelMode);
          InternalCaretXY := Item.ChangeStartPos;
        end;
    end;
  finally
    if ChangeScrollPastEol then
      Exclude(fOptions, eoScrollPastEol);
    Item.Free;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.ClearBookMark(BookMark: Integer);
begin
  if (BookMark in [0..9]) and assigned(fBookMarks[BookMark]) then
  begin
    DoOnClearBookmark(fBookMarks[BookMark]);
    FMarkList.Remove(fBookMarks[Bookmark]);
    fBookMarks[BookMark] := nil;
  end
end;

procedure TCustomSynEdit.GotoBookMark(BookMark: Integer);
var
  iNewPos: TBufferCoord;
begin
  if (BookMark in [0..9]) and
     assigned(fBookMarks[BookMark]) and
     (fBookMarks[BookMark].Line <= fLines.Count)
  then
  begin
    iNewPos.Char := fBookMarks[BookMark].Char;
    iNewPos.Line := fBookMarks[BookMark].Line;
    //call it this way instead to make sure that the caret ends up in the middle
    //if it is off screen (like Delphi does with bookmarks)
    SetCaretXYEx(False, iNewPos);
    EnsureCursorPosVisibleEx(True);
    if SelAvail then
      InvalidateSelection;
    fBlockBegin.Char := fCaretX;
    fBlockBegin.Line := fCaretY;
    fBlockEnd := fBlockBegin;
  end;
end;

procedure TCustomSynEdit.GotoLineAndCenter(ALine: Integer);
begin
  SetCaretXYEx( False, BufferCoord(1, ALine) );
  if SelAvail then
    InvalidateSelection;
  fBlockBegin.Char := fCaretX;
  fBlockBegin.Line := fCaretY;
  fBlockEnd := fBlockBegin;
  EnsureCursorPosVisibleEx(True);
end;

procedure TCustomSynEdit.SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
var
  mark: TSynEditMark;
begin
  if (BookMark in [0..9]) and (Y >= 1) and (Y <= Max(1, fLines.Count)) then
  begin
    mark := TSynEditMark.Create(self);
    with mark do
    begin
      Line := Y;
      Char := X;
      ImageIndex := Bookmark;
      BookmarkNumber := Bookmark;
      Visible := True;
      InternalImage := (fBookMarkOpt.BookmarkImages = nil);
    end;
    DoOnPlaceMark(Mark);
    if (mark <> nil) then
    begin
      if assigned(fBookMarks[BookMark]) then
        ClearBookmark(BookMark);
      fBookMarks[BookMark] := mark;
      FMarkList.Add(fBookMarks[BookMark]);
    end;
  end;
end;

{$IFNDEF SYN_CLX}
function IsTextMessage(Msg: UINT): Boolean;
begin
  Result := (Msg = WM_SETTEXT) or (Msg = WM_GETTEXT) or (Msg = WM_GETTEXTLENGTH);
end;

procedure TCustomSynEdit.WndProc(var Msg: TMessage);
const
  ALT_KEY_DOWN = $20000000;
begin
  // Prevent Alt-Backspace from beeping
  if (Msg.Msg = WM_SYSCHAR) and (Msg.wParam = VK_BACK) and
    (Msg.lParam and ALT_KEY_DOWN <> 0)
  then
    Msg.Msg := 0;

  // handle direct WndProc calls that could happen through VCL-methods like Perform
  if HandleAllocated and IsWindowUnicode(Handle) then
    if not FWindowProducedMessage then
    begin
      FWindowProducedMessage := True;
      if IsTextMessage(Msg.Msg) then
      begin
        with Msg do
          Result := SendMessageA(Handle, Msg, wParam, lParam);
        Exit;
      end;
    end
    else
      FWindowProducedMessage := False;

  inherited;
end;
{$ENDIF}

procedure TCustomSynEdit.ChainListCleared(Sender: TObject);
begin
  if Assigned(fChainListCleared) then
    fChainListCleared(Sender);
  TSynEditStringList(fOrigLines).OnCleared(Sender);
end;

procedure TCustomSynEdit.ChainListDeleted(Sender: TObject; aIndex: Integer;
  aCount: Integer);
begin
  if Assigned(fChainListDeleted) then
    fChainListDeleted(Sender, aIndex, aCount);
  TSynEditStringList(fOrigLines).OnDeleted(Sender, aIndex, aCount);
end;

procedure TCustomSynEdit.ChainListInserted(Sender: TObject; aIndex: Integer;
  aCount: Integer);
begin
  if Assigned(fChainListInserted) then
    fChainListInserted(Sender, aIndex, aCount);
  TSynEditStringList(fOrigLines).OnInserted(Sender, aIndex, aCount);
end;

procedure TCustomSynEdit.ChainListPutted(Sender: TObject; aIndex: Integer;
  aCount: Integer);
begin
  if Assigned(fChainListPutted) then
    fChainListPutted(Sender, aIndex, aCount);
  TSynEditStringList(fOrigLines).OnPutted(Sender, aIndex, aCount);
end;

procedure TCustomSynEdit.ChainLinesChanging(Sender: TObject);
begin
  if Assigned(fChainLinesChanging) then
    fChainLinesChanging(Sender);
  TSynEditStringList(fOrigLines).OnChanging(Sender);
end;

procedure TCustomSynEdit.ChainLinesChanged(Sender: TObject);
begin
  if Assigned(fChainLinesChanged) then
    fChainLinesChanged(Sender);
  TSynEditStringList(fOrigLines).OnChange(Sender);
end;

procedure TCustomSynEdit.ChainUndoRedoAdded(Sender: TObject);
var
  iList: TSynEditUndoList;
  iHandler: TNotifyEvent;
begin
  if Sender = fUndoList then
  begin
    iList := fOrigUndoList;
    iHandler := fChainUndoAdded;
  end
  else { if Sender = fRedoList then }
  begin
    iList := fOrigRedoList;
    iHandler := fChainRedoAdded;
  end;
  if Assigned(iHandler) then
    iHandler(Sender);
  iList.OnAddedUndo(Sender);
end;

procedure TCustomSynEdit.UnHookTextBuffer;
var
  vOldWrap: Boolean;
begin
  Assert(fChainedEditor = nil);
  if fLines = fOrigLines then
    Exit;

  vOldWrap := WordWrap;
  WordWrap := False;

  //first put back the real methods
  with TSynEditStringList(fLines) do
  begin
    OnCleared := fChainListCleared;
    OnDeleted := fChainListDeleted;
    OnInserted := fChainListInserted;
    OnPutted := fChainListPutted;
    OnChanging := fChainLinesChanging;
    OnChange := fChainLinesChanged;
  end;
  fUndoList.OnAddedUndo := fChainUndoAdded;
  fRedoList.OnAddedUndo := fChainRedoAdded;

  fChainListCleared := nil;
  fChainListDeleted := nil;
  fChainListInserted := nil;
  fChainListPutted := nil;
  fChainLinesChanging := nil;
  fChainLinesChanged := nil;
  fChainUndoAdded := nil;

  //make the switch
  fLines := fOrigLines;
  fUndoList := fOrigUndoList;
  fRedoList := fOrigRedoList;
  LinesHookChanged;

  WordWrap := vOldWrap;
end;

procedure TCustomSynEdit.HookTextBuffer(aBuffer: TSynEditStringList;
  aUndo, aRedo: TSynEditUndoList);
var
  vOldWrap: Boolean;
begin
  Assert(fChainedEditor = nil);
  Assert(fLines = fOrigLines);

  vOldWrap := WordWrap;
  WordWrap := False;

  if fChainedEditor <> nil then
    RemoveLinesPointer
  else if fLines <> fOrigLines then
    UnHookTextBuffer;

  //store the current values and put in the chained methods
  fChainListCleared := aBuffer.OnCleared;
    aBuffer.OnCleared := ChainListCleared;
  fChainListDeleted := aBuffer.OnDeleted;
    aBuffer.OnDeleted := ChainListDeleted;
  fChainListInserted := aBuffer.OnInserted;
    aBuffer.OnInserted := ChainListInserted;
  fChainListPutted := aBuffer.OnPutted;
    aBuffer.OnPutted := ChainListPutted;
  fChainLinesChanging := aBuffer.OnChanging;
    aBuffer.OnChanging := ChainLinesChanging;
  fChainLinesChanged := aBuffer.OnChange;
    aBuffer.OnChange := ChainLinesChanged;

  fChainUndoAdded := aUndo.OnAddedUndo;
    aUndo.OnAddedUndo := ChainUndoRedoAdded;
  fChainRedoAdded := aRedo.OnAddedUndo;
    aRedo.OnAddedUndo := ChainUndoRedoAdded;

  //make the switch
  fLines := aBuffer;
  fUndoList := aUndo;
  fRedoList := aRedo;
  LinesHookChanged;

  WordWrap := vOldWrap;
end;

procedure TCustomSynEdit.LinesHookChanged;
var
  iLongestLineLength: Integer;
begin
  Invalidate;
  if eoAutoSizeMaxScrollWidth in fOptions then
  begin
    iLongestLineLength := TSynEditStringList(Lines).LengthOfLongestLine;
    if iLongestLineLength > MaxScrollWidth then
      MaxScrollWidth := iLongestLineLength;
  end;
  UpdateScrollBars;
end;

procedure TCustomSynEdit.SetLinesPointer(ASynEdit: TCustomSynEdit);
begin
  HookTextBuffer(TSynEditStringList(ASynEdit.Lines),
    ASynEdit.UndoList, ASynEdit.RedoList);

  fChainedEditor := ASynEdit;
  ASynEdit.FreeNotification(Self);
end;

procedure TCustomSynEdit.RemoveLinesPointer;
begin
  {$IFDEF SYN_COMPILER_5_UP}
  if Assigned(fChainedEditor) then
    RemoveFreeNotification(fChainedEditor);
  {$ENDIF}
  fChainedEditor := nil;

  UnHookTextBuffer;
end;

{$IFDEF SYN_CLX}
function TCustomSynEdit.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
begin
  Result := inherited EventFilter(Sender, Event);
  case QEvent_type(Event) of
    QEventType_FocusIn:
      begin
        {$IFDEF SYN_LINUX}
        if not FDeadKeysFixed then
        begin
          FDeadKeysFixed := True;
          with TEdit.Create(Self) do
          begin
            Parent := Self;
            BorderStyle := bsNone;
            Color := Self.Color;
            ReadOnly := True;
            Top := ClientRect.Top;
            Left := ClientRect.Left + fGutterWidth + 2;
            Show;
            SetFocus;
            Free;
          end;
          SetFocus;
        end
        else
        {$ENDIF}
        begin
          InitializeCaret;
          if FHideSelection and SelAvail then
            InvalidateSelection;
        end;
      end;
    QEventType_FocusOut:
      begin
        HideCaret;
        kTextDrawer.DestroyCaret;
        if FHideSelection and SelAvail then
          InvalidateSelection;
        EndDrag(False);
      end;
  end;
end;
{$ENDIF}

procedure TCustomSynEdit.DragCanceled;
begin
  fScrollTimer.Enabled := False;
  inherited;
end;

procedure TCustomSynEdit.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  vNewPos: TDisplayCoord;
begin
  inherited;
  if (Source is TCustomSynEdit) and not ReadOnly then
  begin
    Accept := True;
    //Ctrl is pressed => change cursor to indicate copy instead of move
{$IFDEF SYN_CLX}
{$ELSE}
    if GetKeyState(VK_CONTROL) < 0 then
      DragCursor := crMultiDrag
    else
      DragCursor := crDrag;
{$ENDIF}
    if Dragging then //if the drag source is the SynEdit itself
    begin
      if State = dsDragLeave then //restore prev caret position
        ComputeCaret(FMouseDownX, FMouseDownY)
      else
      begin
        vNewPos := PixelsToNearestRowColumn(X, Y);
        vNewPos.Column := MinMax(vNewPos.Column, LeftChar, LeftChar + CharsInWindow - 1);
        vNewPos.Row := MinMax(vNewPos.Row, TopLine, TopLine + LinesInWindow - 1);
        InternalCaretXY := DisplayToBufferPos(vNewPos);
        ComputeScroll(X, Y);
      end;
    end
    else //if is dragging from another SynEdit
      ComputeCaret(X, Y); //position caret under the mouse cursor
  end;
end;

procedure TCustomSynEdit.DragDrop(Source: TObject; X, Y: Integer);
var
  vNewCaret: TBufferCoord;
  DoDrop, DropAfter, DropMove: Boolean;
  vBB, vBE: TBufferCoord;
  DragDropText: UnicodeString;
  ChangeScrollPastEOL: Boolean;
begin
  if not ReadOnly  and (Source is TCustomSynEdit)
    and TCustomSynEdit(Source).SelAvail then
  begin
    IncPaintLock;
    try
      inherited;
      ComputeCaret(X, Y);
      vNewCaret := CaretXY;
      // if from other control then move when SHIFT, else copy
      // if from Self then copy when CTRL, else move
      if Source <> Self then
      begin
{$IFDEF SYN_CLX}
        DropMove := ssShift in Application.KeyState;
{$ELSE}
        DropMove := GetKeyState(VK_SHIFT) < 0;
{$ENDIF}
        DoDrop := True;
        DropAfter := False;
      end
      else
      begin
{$IFDEF SYN_CLX}
        DropMove := not(ssCtrl in Application.KeyState);
{$ELSE}
        DropMove := GetKeyState(VK_CONTROL) >= 0;
{$ENDIF}
        vBB := BlockBegin;
        vBE := BlockEnd;
        DropAfter := (vNewCaret.Line > vBE.Line)
          or ((vNewCaret.Line = vBE.Line) and ((vNewCaret.Char > vBE.Char) or
          ((not DropMove) and (vNewCaret.Char = vBE.Char))));
        DoDrop := DropAfter or (vNewCaret.Line < vBB.Line)
          or ((vNewCaret.Line = vBB.Line) and ((vNewCaret.Char < vBB.Char) or
          ((not DropMove) and (vNewCaret.Char = vBB.Char))));
      end;
      if DoDrop then begin
        BeginUndoBlock;
        try
          DragDropText := TCustomSynEdit(Source).SelText;
          // delete the selected text if necessary
          if DropMove then
          begin
            if Source <> Self then
              TCustomSynEdit(Source).SelText := ''
            else
            begin
              SelText := '';
              // adjust horizontal drop position
              if DropAfter and (vNewCaret.Line = vBE.Line) then
                Dec(vNewCaret.Char, vBE.Char - vBB.Char);
              // adjust vertical drop position
              if DropAfter and (vBE.Line > vBB.Line) then
                Dec(vNewCaret.Line, vBE.Line - vBB.Line);
            end;
          end;
          //todo: this is probably already done inside SelText
          // insert the selected text
          ChangeScrollPastEOL := not (eoScrollPastEol in fOptions);
          try
            if ChangeScrollPastEOL then
              Include(fOptions, eoScrollPastEol);
            InternalCaretXY := vNewCaret;
            BlockBegin := vNewCaret;
            { Add the text. Undo is locked so the action is recorded as crDragDropInsert
            instead of crInsert (code right bellow). }
            Assert(not SelAvail);
            LockUndo;
            try
              SelText := DragDropText;
            finally
              UnlockUndo;
            end;
          finally
            if ChangeScrollPastEOL then
              Exclude(fOptions, eoScrollPastEol);
          end;
          // save undo information
          if Source = Self then
          begin
            fUndoList.AddChange(crDragDropInsert, vNewCaret, BlockEnd, SelText,
              fActiveSelectionMode);
          end
          else begin
            fUndoList.AddChange(crInsert, vNewCaret, BlockEnd,
              SelText, fActiveSelectionMode);
          end;
          BlockEnd := CaretXY;
          CommandProcessor(ecSelGotoXY, #0, @vNewCaret);
        finally
          EndUndoBlock;
        end;
      end;
    finally
      DecPaintLock;
    end;
  end
  else
    inherited;
end;

procedure TCustomSynEdit.SetRightEdge(Value: Integer);
begin
  if fRightEdge <> Value then
  begin
    fRightEdge := Value;
    Invalidate;
  end;
end;

procedure TCustomSynEdit.SetRightEdgeColor(Value: TColor);
var
  nX: Integer;
  rcInval: TRect;
begin
  if fRightEdgeColor <> Value then
  begin
    fRightEdgeColor := Value;
    if HandleAllocated then
    begin
      nX := fTextOffset + fRightEdge * fCharWidth;
      rcInval := Rect(nX - 1, 0, nX + 1, Height);
      InvalidateRect(rcInval, False);
    end;
  end;
end;

function TCustomSynEdit.GetMaxUndo: Integer;
begin
  result := fUndoList.MaxUndoActions;
end;

procedure TCustomSynEdit.SetMaxUndo(const Value: Integer);
begin
  if Value > -1 then
  begin
    fUndoList.MaxUndoActions := Value;
    fRedoList.MaxUndoActions := Value;
  end;
end;

procedure TCustomSynEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = fSearchEngine then
    begin
      SearchEngine := nil;
    end;

    if AComponent = fHighlighter then
    begin
      Highlighter := nil;
    end;

    if AComponent = fChainedEditor then
    begin
      RemoveLinesPointer;
    end;

    if (fBookmarkOpt <> nil) then
      if (AComponent = fBookmarkOpt.BookmarkImages) then
      begin
        fBookmarkOpt.BookmarkImages := nil;
        InvalidateGutterLines(-1, -1);
      end;
  end;
end;

procedure TCustomSynEdit.SetHighlighter(const Value: TSynCustomHighlighter);
begin
  if Value <> fHighlighter then
  begin
    if Assigned(fHighlighter) then
    begin
      fHighlighter.UnhookAttrChangeEvent(HighlighterAttrChanged);
{$IFDEF SYN_COMPILER_5_UP}
      fHighlighter.RemoveFreeNotification(Self);
{$ENDIF}
    end;
    if Assigned(Value) then
    begin
      Value.HookAttrChangeEvent(HighlighterAttrChanged);
      Value.FreeNotification(Self);
    end;
    fHighlighter := Value;
    if not(csDestroying in ComponentState) then
      HighlighterAttrChanged(fHighlighter);
  end;
end;

procedure TCustomSynEdit.SetBorderStyle(Value: TSynBorderStyle);
begin
  if fBorderStyle <> Value then
  begin
    fBorderStyle := Value;
{$IFDEF SYN_CLX}
    Resize;
    Invalidate;
{$ELSE}
    RecreateWnd;
{$ENDIF}
  end;
end;

procedure TCustomSynEdit.SetHideSelection(const Value: Boolean);
begin
  if fHideSelection <> Value then
  begin
    FHideSelection := Value;
    InvalidateSelection;
  end;
end;

procedure TCustomSynEdit.SetInsertMode(const Value: Boolean);
begin
  if fInserting <> Value then
  begin
    fInserting := Value;
    if not (csDesigning in ComponentState) then
      // Reset the caret.
      InitializeCaret;
    StatusChanged([scInsertMode]);
  end;
end;

procedure TCustomSynEdit.InitializeCaret;
var
  ct: TSynEditCaretType;
  cw, ch: Integer;
begin
  // CreateCaret automatically destroys the previous one, so we don't have to
  // worry about cleaning up the old one here with DestroyCaret.
  // Ideally, we will have properties that control what these two carets look like.
  if InsertMode then
    ct := FInsertCaret
  else
    ct := FOverwriteCaret;
  case ct of
    ctHorizontalLine:
      begin
        cw := fCharWidth;
        ch := 2;
        FCaretOffset := Point(0, fTextHeight - 2);
      end;
    ctHalfBlock:
      begin
        cw := fCharWidth;
        ch := (fTextHeight - 2) div 2;
        FCaretOffset := Point(0, ch);
      end;
    ctBlock:
      begin
        cw := fCharWidth;
        ch := fTextHeight - 2;
        FCaretOffset := Point(0, 0);
      end;
    else
    begin // ctVerticalLine
      cw := 2;
      ch := fTextHeight - 2;
      FCaretOffset := Point(-1, 0);
    end;
  end;
  Exclude(fStateFlags, sfCaretVisible);

  if Focused or FAlwaysShowCaret then
  begin
  {$IFDEF SYN_CLX}
    CreateCaret(self, 0, cw, ch);
  {$ELSE}
    CreateCaret(Handle, 0, cw, ch);
  {$ENDIF}
    UpdateCaret;
  end;
end;

procedure TCustomSynEdit.SetInsertCaret(const Value: TSynEditCaretType);
begin
  if FInsertCaret <> Value then
  begin
    FInsertCaret := Value;
    InitializeCaret;
  end;
end;

procedure TCustomSynEdit.SetOverwriteCaret(const Value: TSynEditCaretType);
begin
  if FOverwriteCaret <> Value then
  begin
    FOverwriteCaret := Value;
    InitializeCaret;
  end;
end;

procedure TCustomSynEdit.SetMaxScrollWidth(Value: Integer);
begin
  Value := MinMax(Value, 1, MaxInt - 1);
  if MaxScrollWidth <> Value then
  begin
    fMaxScrollWidth := Value;
    if eoScrollPastEol in Options then
      UpdateScrollBars;
  end;
end;

procedure TCustomSynEdit.EnsureCursorPosVisible;
begin
  EnsureCursorPosVisibleEx(False);
end;

procedure TCustomSynEdit.EnsureCursorPosVisibleEx(ForceToMiddle: Boolean;
  EvenIfVisible: Boolean = False);
var
  TmpMiddle: Integer;
  VisibleX: Integer;
  vCaretRow: Integer;
begin
  HandleNeeded;
  IncPaintLock;
  try
    // Make sure X is visible
    VisibleX := DisplayX;
    if VisibleX < LeftChar then
      LeftChar := VisibleX
    else if VisibleX >= CharsInWindow + LeftChar then
      LeftChar := VisibleX - CharsInWindow + 1
    else
      LeftChar := LeftChar;

    // Make sure Y is visible
    vCaretRow := DisplayY;
    if ForceToMiddle then
    begin
      if vCaretRow < (TopLine - 1) then
      begin
        TmpMiddle := LinesInWindow div 2;
        if vCaretRow - TmpMiddle < 0 then
          TopLine := 1
        else
          TopLine := vCaretRow - TmpMiddle + 1;
      end
      else if vCaretRow > (TopLine + (LinesInWindow - 2)) then
      begin
        TmpMiddle := LinesInWindow div 2;
        TopLine := vCaretRow - (LinesInWindow - 1) + TmpMiddle;
      end
     { Forces to middle even if visible in viewport }
      else if EvenIfVisible then
      begin
        TmpMiddle := fLinesInWindow div 2;
        TopLine := vCaretRow - TmpMiddle + 1;
      end;
    end
    else begin
      if vCaretRow < TopLine then
        TopLine := vCaretRow
      else if vCaretRow > TopLine + Max(1, LinesInWindow) - 1 then
        TopLine := vCaretRow - (LinesInWindow - 1)
      else
        TopLine := TopLine;
    end;
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.SetKeystrokes(const Value: TSynEditKeyStrokes);
begin
  if Value = nil then
    FKeystrokes.Clear
  else
    FKeystrokes.Assign(Value);
end;

procedure TCustomSynEdit.SetDefaultKeystrokes;
begin
  FKeystrokes.ResetDefaults;
end;

// If the translations requires Data, memory will be allocated for it via a
// GetMem call.  The client must call FreeMem on Data if it is not NIL.

function TCustomSynEdit.TranslateKeyCode(Code: word; Shift: TShiftState;
  var Data: pointer): TSynEditorCommand;
var
  i: Integer;
{$IFNDEF SYN_COMPILER_3_UP}
const
  VK_ACCEPT = $30;
{$ENDIF}
begin
  i := KeyStrokes.FindKeycode2(fLastKey, fLastShiftState, Code, Shift);
  if i >= 0 then
    Result := KeyStrokes[i].Command
  else begin
    i := Keystrokes.FindKeycode(Code, Shift);
    if i >= 0 then
      Result := Keystrokes[i].Command
    else
      Result := ecNone;
  end;
{$IFDEF SYN_CLX}
  if Result = ecNone then
{$ELSE}
  if (Result = ecNone) and (Code >= VK_ACCEPT) and (Code <= VK_SCROLL) then
{$ENDIF}
  begin
    fLastKey := Code;
    fLastShiftState := Shift;
  end
  else
  begin
    fLastKey := 0;
    fLastShiftState := [];
  end;
end;

procedure TCustomSynEdit.CommandProcessor(Command: TSynEditorCommand;
  AChar: WideChar; Data: pointer);
begin
  // first the program event handler gets a chance to process the command
  DoOnProcessCommand(Command, AChar, Data);
  if Command <> ecNone then
  begin
    // notify hooked command handlers before the command is executed inside of
    // the class
    NotifyHookedCommandHandlers(False, Command, AChar, Data);
    // internal command handler
    if (Command <> ecNone) and (Command < ecUserFirst) then
      ExecuteCommand(Command, AChar, Data);
    // notify hooked command handlers after the command was executed inside of
    // the class
    if Command <> ecNone then
      NotifyHookedCommandHandlers(True, Command, AChar, Data);
  end;
  DoOnCommandProcessed(Command, AChar, Data);
end;

procedure TCustomSynEdit.ExecuteCommand(Command: TSynEditorCommand; AChar: WideChar;
  Data: pointer);

  procedure SetSelectedTextEmpty;
  var
    vSelText: UnicodeString;
    vUndoBegin, vUndoEnd: TBufferCoord;
  begin
    vUndoBegin := fBlockBegin;
    vUndoEnd := fBlockEnd;
    vSelText := SelText;
    SetSelTextPrimitive('');
    if (vUndoBegin.Line < vUndoEnd.Line) or (
      (vUndoBegin.Line = vUndoEnd.Line) and (vUndoBegin.Char < vUndoEnd.Char)) then
    begin
      fUndoList.AddChange(crDelete, vUndoBegin, vUndoEnd, vSelText,
        fActiveSelectionMode);
    end
    else
    begin
      fUndoList.AddChange(crDeleteAfterCursor, vUndoBegin, vUndoEnd, vSelText,
        fActiveSelectionMode);
    end;
  end;

  procedure ForceCaretX(aCaretX: integer);
  var
    vRestoreScroll: boolean;
  begin
    vRestoreScroll := not (eoScrollPastEol in fOptions);
    Include(fOptions, eoScrollPastEol);
    try
      InternalCaretX := aCaretX;
    finally
      if vRestoreScroll then
        Exclude(fOptions, eoScrollPastEol);
    end;
  end;

var
  CX: Integer;
  Len: Integer;
  Temp: UnicodeString;
  Temp2: UnicodeString;
  Helper: UnicodeString;
  TabBuffer: UnicodeString;
  SpaceBuffer: UnicodeString;
  SpaceCount1: Integer;
  SpaceCount2: Integer;
  BackCounter: Integer;
  StartOfBlock: TBufferCoord;
  EndOfBlock: TBufferCoord;
  bChangeScroll: Boolean;
  moveBkm: Boolean;
  WP: TBufferCoord;
  Caret: TBufferCoord;
  CaretNew: TBufferCoord;
  counter: Integer;
  InsDelta: Integer;
  iUndoBegin, iUndoEnd: TBufferCoord;
  vCaretRow: Integer;
  vTabTrim: integer;
  s: UnicodeString;
  i: Integer;
begin
  IncPaintLock;
  try
    case Command of
// horizontal caret movement or selection
      ecLeft, ecSelLeft:
        MoveCaretHorz(-1, Command = ecSelLeft);
      ecRight, ecSelRight:
        MoveCaretHorz(1, Command = ecSelRight);
      ecPageLeft, ecSelPageLeft:
        MoveCaretHorz(-CharsInWindow, Command = ecSelPageLeft);
      ecPageRight, ecSelPageRight:
        MoveCaretHorz(CharsInWindow, Command = ecSelPageRight);
      ecLineStart, ecSelLineStart:
        begin
          DoHomeKey(Command = ecSelLineStart);
        end;
      ecLineEnd, ecSelLineEnd:
        DoEndKey(Command = ecSelLineEnd);
// vertical caret movement or selection
      ecUp, ecSelUp:
        begin
          MoveCaretVert(-1, Command = ecSelUp);
          Update;
        end;
      ecDown, ecSelDown:
        begin
          MoveCaretVert(1, Command = ecSelDown);
          Update;
        end;
      ecPageUp, ecSelPageUp, ecPageDown, ecSelPageDown:
        begin
          counter := fLinesInWindow shr Ord(eoHalfPageScroll in fOptions);
          if eoScrollByOneLess in fOptions then
            Dec(counter);
          if (Command in [ecPageUp, ecSelPageUp]) then
            counter := -counter;
          TopLine := TopLine + counter;
          MoveCaretVert(counter, Command in [ecSelPageUp, ecSelPageDown]);
          Update;
        end;
      ecPageTop, ecSelPageTop:
        begin
          CaretNew := DisplayToBufferPos(
            DisplayCoord(DisplayX, TopLine) );
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelPageTop);
          Update;
        end;
      ecPageBottom, ecSelPageBottom:
        begin
          CaretNew := DisplayToBufferPos(
            DisplayCoord(DisplayX, TopLine + LinesInWindow -1) );
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelPageBottom);
          Update;
        end;
      ecEditorTop, ecSelEditorTop:
        begin
          CaretNew.Char := 1;
          CaretNew.Line := 1;
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelEditorTop);
          Update;
        end;
      ecEditorBottom, ecSelEditorBottom:
        begin
          CaretNew.Char := 1;
          CaretNew.Line := Lines.Count;
          if (CaretNew.Line > 0) then
            CaretNew.Char := Length(Lines[CaretNew.Line - 1]) + 1;
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelEditorBottom);
          Update;
        end;
// goto special line / column position
      ecGotoXY, ecSelGotoXY:
        if Assigned(Data) then
        begin
          MoveCaretAndSelection(CaretXY, TBufferCoord(Data^), Command = ecSelGotoXY);
          Update;
        end;
// word selection
      ecWordLeft, ecSelWordLeft:
        begin
          CaretNew := PrevWordPos;
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelWordLeft);
        end;
      ecWordRight, ecSelWordRight:
        begin
          CaretNew := NextWordPos;
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelWordRight);
        end;
      ecSelWord:
      	begin
      	  SetSelWord;
      	end;
      ecSelectAll:
        begin
          SelectAll;
        end;
      ecDeleteLastChar:
        if not ReadOnly then begin
          DoOnPaintTransientEx(ttBefore,true);
          try
            if SelAvail then
              SetSelectedTextEmpty
            else begin
              Temp := LineText;
              TabBuffer := TSynEditStringList(Lines).ExpandedStrings[CaretY - 1];
              Len := Length(Temp);
              Caret := CaretXY;
              vTabTrim := 0;
              if CaretX > Len + 1 then
              begin
                Helper := '';
                if eoSmartTabDelete in fOptions then
                begin
                  //It's at the end of the line, move it to the length
                  if Len > 0 then
                    InternalCaretX := Len + 1
                  else begin
                    //move it as if there were normal spaces there
                    SpaceCount1 := CaretX - 1;
                    SpaceCount2 := 0;
                    // unindent
                    if SpaceCount1 > 0 then
                    begin
                      BackCounter := CaretY - 2;
                      //It's better not to have if statement inside loop
                      if (eoTrimTrailingSpaces in Options) and (Len = 0) then
                        while BackCounter >= 0 do
                        begin
                          SpaceCount2 := LeftSpacesEx(Lines[BackCounter], True);
                          if SpaceCount2 < SpaceCount1 then
                            break;
                          Dec(BackCounter);
                        end
                      else
                        while BackCounter >= 0 do
                        begin
                          SpaceCount2 := LeftSpaces(Lines[BackCounter]);
                          if SpaceCount2 < SpaceCount1 then
                            break;
                          Dec(BackCounter);
                        end;
                      if (BackCounter = -1) and (SpaceCount2 > SpaceCount1) then
                        SpaceCount2 := 0;
                    end;
                    if SpaceCount2 = SpaceCount1 then
                      SpaceCount2 := 0;
                    fCaretX := fCaretX - (SpaceCount1 - SpaceCount2);
                    UpdateLastCaretX;
                    fStateFlags := fStateFlags + [sfCaretChanged];
                    StatusChanged([scCaretX]);
                  end;
                end
                else begin
                  // only move caret one column
                  InternalCaretX := CaretX - 1;
                end;
              end else if CaretX = 1 then begin
                // join this line with the last line if possible
                if CaretY > 1 then
                begin
                  InternalCaretY := CaretY - 1;
                  InternalCaretX := Length(Lines[CaretY - 1]) + 1;
                  Lines.Delete(CaretY);
                  DoLinesDeleted(CaretY+1, 1);
                  if eoTrimTrailingSpaces in Options then
                    Temp := TrimTrailingSpaces(Temp);

                  LineText := LineText + Temp;
                  Helper := #13#10;
                end;
              end
              else begin
                // delete text before the caret
                SpaceCount1 := LeftSpaces(Temp);
                SpaceCount2 := 0;
                if (Temp[CaretX - 1] <= #32) and (SpaceCount1 = CaretX - 1) then
                begin
                  if eoSmartTabDelete in fOptions then
                  begin
                    // unindent
                    if SpaceCount1 > 0 then
                    begin
                      BackCounter := CaretY - 2;
                      while BackCounter >= 0 do
                      begin
                        SpaceCount2 := LeftSpaces(Lines[BackCounter]);
                        if SpaceCount2 < SpaceCount1 then
                          break;
                        Dec(BackCounter);
                      end;
                      if (BackCounter = -1) and (SpaceCount2 > SpaceCount1) then
                        SpaceCount2 := 0;
                    end;
                    if SpaceCount2 = SpaceCount1 then
                      SpaceCount2 := 0;
                    Helper := Copy(Temp, 1, SpaceCount1 - SpaceCount2);
                    Delete(Temp, 1, SpaceCount1 - SpaceCount2);
                  end
                  else begin
                    SpaceCount2 := SpaceCount1;
                    //how much till the next tab column
                    BackCounter  := (DisplayX - 1) mod FTabWidth;
                    if BackCounter = 0 then BackCounter := FTabWidth;

                    SpaceCount1 := 0;
                    CX := DisplayX - BackCounter;
                    while (SpaceCount1 < FTabWidth) and
                          (SpaceCount1 < BackCounter) and
                          (TabBuffer[CX] <> #9) do
                    begin
                      Inc(SpaceCount1);
                      Inc(CX);
                    end;
                    {$IFOPT R+}
                    // Avoids an exception when compiled with $R+.
                    // 'CX' can be 'Length(TabBuffer)+1', which isn't an AV and evaluates
                    //to #0. But when compiled with $R+, Delphi raises an Exception.
                    if CX <= Length(TabBuffer) then
                    {$ENDIF}
                    if TabBuffer[CX] = #9 then
                      SpaceCount1 := SpaceCount1 + 1;

                    if SpaceCount2 = SpaceCount1 then
                    begin
                      Helper := Copy(Temp, 1, SpaceCount1);
                      Delete(Temp, 1, SpaceCount1);
                    end
                    else begin
                      Helper := Copy(Temp, SpaceCount2 - SpaceCount1 + 1, SpaceCount1);
                      Delete(Temp, SpaceCount2 - SpaceCount1 + 1, SpaceCount1);
                    end;
                    SpaceCount2 := 0;
                  end;
                  fCaretX := fCaretX - (SpaceCount1 - SpaceCount2);
                  UpdateLastCaretX;
                  // Stores the previous "expanded" CaretX if the line contains tabs.
                  if (eoTrimTrailingSpaces in Options) and (Len <> Length(TabBuffer)) then
                    vTabTrim := CharIndex2CaretPos(CaretX, TabWidth, Temp);
                  ProperSetLine(CaretY - 1, Temp);
                  fStateFlags := fStateFlags + [sfCaretChanged];
                  StatusChanged([scCaretX]);
                  // Calculates a delta to CaretX to compensate for trimmed tabs.
                  if vTabTrim <> 0 then
                    if Length(Temp) <> Length(LineText) then
                      Dec(vTabTrim, CharIndex2CaretPos(CaretX, TabWidth, LineText))
                    else
                      vTabTrim := 0;
                end
                else begin
                  // delete char
                  counter := 1;
                  InternalCaretX := CaretX - counter;
                  // Stores the previous "expanded" CaretX if the line contains tabs.
                  if (eoTrimTrailingSpaces in Options) and (Len <> Length(TabBuffer)) then
                    vTabTrim := CharIndex2CaretPos(CaretX, TabWidth, Temp);
                  Helper := Copy(Temp, CaretX, counter);
                  Delete(Temp, CaretX, counter);
                  ProperSetLine(CaretY - 1, Temp);
                  // Calculates a delta to CaretX to compensate for trimmed tabs.
                  if vTabTrim <> 0 then
                    if Length(Temp) <> Length(LineText) then
                      Dec(vTabTrim, CharIndex2CaretPos(CaretX, TabWidth, LineText))
                    else
                      vTabTrim := 0;
                end;
              end;
              if (Caret.Char <> CaretX) or (Caret.Line <> CaretY) then
              begin
                fUndoList.AddChange(crSilentDelete, CaretXY, Caret, Helper,
                  smNormal);
                if vTabTrim <> 0 then
                  ForceCaretX(CaretX + vTabTrim);
              end;
            end;
            EnsureCursorPosVisible;
          finally
            DoOnPaintTransientEx(ttAfter,true);
          end;
        end;
      ecDeleteChar:
        if not ReadOnly then begin
          DoOnPaintTransient(ttBefore);

          if SelAvail then
            SetSelectedTextEmpty
          else begin
            // Call UpdateLastCaretX. Even though the caret doesn't move, the
            // current caret position should "stick" whenever text is modified.
            UpdateLastCaretX;
            Temp := LineText;
            Len := Length(Temp);
            if CaretX <= Len then
            begin
              // delete char
              counter := 1;
              Helper := Copy(Temp, CaretX, counter);
              Caret.Char := CaretX + counter;
              Caret.Line := CaretY;
              Delete(Temp, CaretX, counter);
              ProperSetLine(CaretY - 1, Temp);
            end
            else begin
              // join line with the line after
              if CaretY < Lines.Count then
              begin
                Helper := UnicodeStringOfChar(#32, CaretX - 1 - Len);
                ProperSetLine(CaretY - 1, Temp + Helper + Lines[CaretY]);
                Caret.Char := 1;
                Caret.Line := CaretY + 1;
                Helper := #13#10;
                Lines.Delete(CaretY);
                DoLinesDeleted(CaretY +1, 1);
              end;
            end;
            if (Caret.Char <> CaretX) or (Caret.Line <> CaretY) then
            begin
              fUndoList.AddChange(crSilentDeleteAfterCursor, CaretXY, Caret,
                Helper, smNormal);
            end;
          end;
          DoOnPaintTransient(ttAfter);
        end;
      ecDeleteWord, ecDeleteEOL:
        if not ReadOnly then begin
          DoOnPaintTransient(ttBefore);
          Len := Length(LineText);
          if Command = ecDeleteWord then
          begin
            WP := WordEnd;
            Temp := LineText;
            if (WP.Char < CaretX) or ((WP.Char = CaretX) and (WP.Line < fLines.Count)) then
            begin
              if WP.Char > Len then
              begin
                Inc(WP.Line);
                WP.Char := 1;
                Temp := Lines[WP.Line - 1];
              end
              else if Temp[WP.Char] <> #32 then
                Inc(WP.Char);
            end;
            {$IFOPT R+}
            Temp := Temp + #0;
            {$ENDIF}
            if Temp <> '' then
              while Temp[WP.Char] = #32 do
                Inc(WP.Char);
          end
          else begin
            WP.Char := Len + 1;
            WP.Line := CaretY;
          end;
          if (WP.Char <> CaretX) or (WP.Line <> CaretY) then
          begin
            SetBlockBegin(CaretXY);
            SetBlockEnd(WP);
            ActiveSelectionMode := smNormal;
            Helper := SelText;
            SetSelTextPrimitive(UnicodeStringOfChar(' ', CaretX - BlockBegin.Char));
            fUndoList.AddChange(crSilentDeleteAfterCursor, CaretXY, WP,
              Helper, smNormal);
            InternalCaretXY := CaretXY;
          end;
        end;
      ecDeleteLastWord, ecDeleteBOL:
        if not ReadOnly then begin
          DoOnPaintTransient(ttBefore);
          if Command = ecDeleteLastWord then
            WP := PrevWordPos
          else begin
            WP.Char := 1;
            WP.Line := CaretY;
          end;
          if (WP.Char <> CaretX) or (WP.Line <> CaretY) then
          begin
            SetBlockBegin(CaretXY);
            SetBlockEnd(WP);
            ActiveSelectionMode := smNormal;
            Helper := SelText;
            SetSelTextPrimitive('');
            fUndoList.AddChange(crSilentDelete, WP, CaretXY, Helper,
              smNormal);
            InternalCaretXY := WP;
          end;
          DoOnPaintTransient(ttAfter);
        end;
      ecDeleteLine:
        if not ReadOnly and (Lines.Count > 0) and not ((CaretY = Lines.Count) and (Length(Lines[CaretY - 1]) = 0))
        then begin
          DoOnPaintTransient(ttBefore);
          if SelAvail then
            SetBlockBegin(CaretXY);
          Helper := LineText;
          if CaretY = Lines.Count then
          begin
            Lines[CaretY - 1] := '';
            fUndoList.AddChange(crSilentDeleteAfterCursor, BufferCoord(1, CaretY),
              BufferCoord(Length(Helper) + 1, CaretY), Helper, smNormal);
          end
          else begin
            Lines.Delete(CaretY - 1);
            Helper := Helper + #13#10;
            fUndoList.AddChange(crSilentDeleteAfterCursor, BufferCoord(1, CaretY),
              BufferCoord(1, CaretY + 1), Helper, smNormal);
            DoLinesDeleted(CaretY, 1);
          end;
          InternalCaretXY := BufferCoord(1, CaretY); // like seen in the Delphi editor
        end;
      ecClearAll:
        begin
          if not ReadOnly then ClearAll;
        end;
      ecInsertLine,
      ecLineBreak:
        if not ReadOnly then begin
          UndoList.BeginBlock;
          try
          if SelAvail then
          begin
            Helper := SelText;
            iUndoBegin := fBlockBegin;
            iUndoEnd := fBlockEnd;
            SetSelTextPrimitive('');
            fUndoList.AddChange(crDelete, iUndoBegin, iUndoEnd, Helper,
              fActiveSelectionMode);
          end;
          Temp := LineText;
          Temp2 := Temp;
// This is sloppy, but the Right Thing would be to track the column of markers
// too, so they could be moved depending on whether they are after the caret...
          InsDelta := Ord(CaretX = 1);
          Len := Length(Temp);
          if Len > 0 then
          begin
            if Len >= CaretX then
            begin
              if CaretX > 1 then
              begin
                Temp := Copy(LineText, 1, CaretX - 1);
                SpaceCount1 := LeftSpacesEx(Temp,true);
                Delete(Temp2, 1, CaretX - 1);
                Lines.Insert(CaretY, GetLeftSpacing(SpaceCount1, True) + Temp2);
                ProperSetLine(CaretY - 1, Temp);
                fUndoList.AddChange(crLineBreak, CaretXY, CaretXY, Temp2,
                  smNormal);
                if Command = ecLineBreak then
                  InternalCaretXY := BufferCoord(
                    Length(GetLeftSpacing(SpaceCount1,true)) + 1,
                    CaretY + 1);
              end
              else begin
                Lines.Insert(CaretY - 1, '');
                fUndoList.AddChange(crLineBreak, CaretXY, CaretXY, Temp2,
                  smNormal);
                if Command = ecLineBreak then
                  InternalCaretY := CaretY + 1;
              end;
            end
            else begin
              SpaceCount2 := 0;
              BackCounter := CaretY;
              if eoAutoIndent in Options then
              begin
                repeat
                  Dec(BackCounter);
                  Temp := Lines[BackCounter];
                  SpaceCount2 := LeftSpaces(Temp);
                until (BackCounter = 0) or (Temp <> '');
              end;
              Lines.Insert(CaretY, '');
              Caret := CaretXY;

              fUndoList.AddChange(crLineBreak, Caret, Caret, '', smNormal);   //KV
              if Command = ecLineBreak then
              begin
                InternalCaretXY := BufferCoord(1, CaretY +1);
                if SpaceCount2 > 0 then
                begin
                  SpaceBuffer := Copy(Lines[BackCounter], 1, SpaceCount2);
                  for i := 1 to Length(SpaceBuffer) do
                    if SpaceBuffer[i] = #9 then
                      CommandProcessor(ecTab, #0, nil)
                    else
                      CommandProcessor(ecChar, SpaceBuffer[i], nil);
                end;
              end;
            end;
          end
          else begin
            if fLines.Count = 0 then
              fLines.Add('');
            SpaceCount2 := 0;
            if eoAutoIndent in Options then
            begin
              BackCounter := CaretY - 1;
              while BackCounter >= 0 do
              begin
                SpaceCount2 := LeftSpacesEx(Lines[BackCounter],True);
                if Length(Lines[BackCounter]) > 0 then break;
                dec(BackCounter);
              end;
            end;
            Lines.Insert(CaretY - 1, '');
            fUndoList.AddChange(crLineBreak, CaretXY, CaretXY, '', smNormal);
            if Command = ecLineBreak then
              InternalCaretX := SpaceCount2 + 1;
            if Command = ecLineBreak then
              InternalCaretY := CaretY + 1;
          end;
          DoLinesInserted(CaretY - InsDelta, 1);
          BlockBegin := CaretXY;
          BlockEnd   := CaretXY;
          EnsureCursorPosVisible;
          UpdateLastCaretX;
          finally
            UndoList.EndBlock;
          end;
        end;
      ecTab:
        if not ReadOnly then DoTabKey;
      ecShiftTab:
        if not ReadOnly then DoShiftTabKey;
      ecMatchBracket:
        FindMatchingBracket;
      ecChar:
      // #127 is Ctrl + Backspace, #32 is space
        if not ReadOnly and (AChar >= #32) and (AChar <> #127) then
        begin
          if SelAvail then
          begin
            BeginUndoBlock;
            try
              Helper := SelText;
              iUndoBegin := fBlockBegin;
              iUndoEnd := fBlockEnd;
              StartOfBlock := BlockBegin;
              if fActiveSelectionMode = smLine then
                StartOfBlock.Char := 1;
              fUndoList.AddChange(crDelete, iUndoBegin, iUndoEnd, Helper,
                fActiveSelectionMode);
              SetSelTextPrimitive(AChar);
              if fActiveSelectionMode <> smColumn then
              begin
                fUndoList.AddChange(crInsert, StartOfBlock, BlockEnd, '',
                  smNormal);
              end;
            finally
              EndUndoBlock;
            end;
          end
          else
          begin
            SpaceCount2 := 0;
            Temp := LineText;
            Len := Length(Temp);
            if Len < CaretX then
            begin
              if (Len > 0) then
                SpaceBuffer := UnicodeStringOfChar(#32, CaretX - Len - Ord(fInserting))
              else
                SpaceBuffer := GetLeftSpacing(CaretX - Len - Ord(fInserting), True);
              SpaceCount2 := Length(SpaceBuffer);

              Temp := Temp + SpaceBuffer;
            end;
            // Added the check for whether or not we're in insert mode.
            // If we are, we append one less space than we would in overwrite mode.
            // This is because in overwrite mode we have to put in a final space
            // character which will be overwritten with the typed character.  If we put the
            // extra space in in insert mode, it would be left at the end of the line and
            // cause problems unless eoTrimTrailingSpaces is set.
            bChangeScroll := not (eoScrollPastEol in fOptions);
            try
              if bChangeScroll then Include(fOptions, eoScrollPastEol);
              StartOfBlock := CaretXY;

              if fInserting then
              begin
                if not WordWrap and not (eoAutoSizeMaxScrollWidth in Options)
                   and (CaretX > MaxScrollWidth) then
                begin
                  Exit;
                end;
                Insert(AChar, Temp, CaretX);
                if (eoTrimTrailingSpaces in Options) and ((AChar = #9) or (AChar = #32)) and (Length(TrimTrailingSpaces(LineText)) = 0) then
                  InternalCaretX := GetExpandedLength(Temp, TabWidth) + 1
                else
                begin
                  if Len = 0 then
                    InternalCaretX := Length(Temp) + 1
                  else
                    InternalCaretX := CaretX + 1;
                end;
                ProperSetLine(CaretY - 1, Temp);
                if SpaceCount2 > 0 then
                begin
                  BeginUndoBlock;
                  try
                    //if we inserted spaces with this char, we need to account for those
                    //in the X Position
                    StartOfBlock.Char := StartOfBlock.Char - SpaceCount2;
                    EndOfBlock := CaretXY;
                    EndOfBlock.Char := EndOfBlock.Char - 1;
                    //The added whitespace
                    fUndoList.AddChange(crWhiteSpaceAdd, EndOfBlock, StartOfBlock, '',
                      smNormal);
                    StartOfBlock.Char := StartOfBlock.Char + SpaceCount2;

                    fUndoList.AddChange(crInsert, StartOfBlock, CaretXY, '',
                      smNormal);
                  finally
                    EndUndoBlock;
                  end;
                end
                else begin
                  fUndoList.AddChange(crInsert, StartOfBlock, CaretXY, '',
                    smNormal);
                end;
              end
              else begin
// Processing of case character covers on LeadByte.
                counter := 1;
                Helper := Copy(Temp, CaretX, counter);
                Temp[CaretX] := AChar;
                CaretNew.Char := CaretX + counter;
                CaretNew.Line := CaretY;
                ProperSetLine(CaretY - 1, Temp);
                fUndoList.AddChange(crInsert, StartOfBlock, CaretNew, Helper,
                  smNormal);
                InternalCaretX := CaretX + 1;
              end;
              if CaretX >= LeftChar + fCharsInWindow then
                LeftChar := LeftChar + Min(25, fCharsInWindow - 1);
            finally
              if bChangeScroll then Exclude(fOptions, eoScrollPastEol);
            end;
          end;
          DoOnPaintTransient(ttAfter);
        end;
      ecUpperCase,
      ecLowerCase,
      ecToggleCase,
      ecTitleCase,
      ecUpperCaseBlock,
      ecLowerCaseBlock,
      ecToggleCaseBlock:
        if not ReadOnly then DoCaseChange(Command);
      ecUndo:
        begin
          if not ReadOnly then Undo;
        end;
      ecRedo:
        begin
          if not ReadOnly then Redo;
        end;
      ecGotoMarker0..ecGotoMarker9:
        begin
          if BookMarkOptions.EnableKeys then
            GotoBookMark(Command - ecGotoMarker0);
        end;
      ecSetMarker0..ecSetMarker9:
        begin
          if BookMarkOptions.EnableKeys then
          begin
            CX := Command - ecSetMarker0;
            if Assigned(Data) then
              Caret := TBufferCoord(Data^)
            else
              Caret := CaretXY;
            if assigned(fBookMarks[CX]) then
            begin
              moveBkm := (fBookMarks[CX].Line <> Caret.Line);
              ClearBookMark(CX);
              if moveBkm then
                SetBookMark(CX, Caret.Char, Caret.Line);
            end
            else
              SetBookMark(CX, Caret.Char, Caret.Line);
          end; // if BookMarkOptions.EnableKeys
        end;
      ecCut:
        begin
          if (not ReadOnly) and SelAvail then
            CutToClipboard;
        end;
      ecCopy:
        begin
          CopyToClipboard;
        end;
      ecPaste:
        begin
          if not ReadOnly then PasteFromClipboard;
        end;
      ecScrollUp, ecScrollDown:
        begin
          vCaretRow := DisplayY;
          if (vCaretRow < TopLine) or (vCaretRow >= TopLine + LinesInWindow) then
            // If the caret is not in view then, like the Delphi editor, move
            // it in view and do nothing else
            EnsureCursorPosVisible
          else begin
            if Command = ecScrollUp then
            begin
              TopLine := TopLine - 1;
              if vCaretRow > TopLine + LinesInWindow - 1 then
                MoveCaretVert((TopLine + LinesInWindow - 1) - vCaretRow, False);
            end
            else begin
              TopLine := TopLine + 1;
              if vCaretRow < TopLine then
                MoveCaretVert(TopLine - vCaretRow, False);
            end;
            EnsureCursorPosVisible;
            Update;
          end;
        end;
      ecScrollLeft:
        begin
          LeftChar := LeftChar - 1;
          // todo: The following code was commented out because it is not MBCS or hard-tab safe.
          //if CaretX > LeftChar + CharsInWindow then
          //  InternalCaretX := LeftChar + CharsInWindow;
          Update;
        end;
      ecScrollRight:
        begin
          LeftChar := LeftChar + 1;
          // todo: The following code was commented out because it is not MBCS or hard-tab safe.
          //if CaretX < LeftChar then
          //  InternalCaretX := LeftChar;
          Update;
        end;
      ecInsertMode:
        begin
          InsertMode := True;
        end;
      ecOverwriteMode:
        begin
          InsertMode := False;
        end;
      ecToggleMode:
        begin
          InsertMode := not InsertMode;
        end;
      ecBlockIndent:
        if not ReadOnly then DoBlockIndent;
      ecBlockUnindent:
        if not ReadOnly then DoBlockUnindent;
      ecNormalSelect:
        SelectionMode := smNormal;
      ecColumnSelect:
        SelectionMode := smColumn;
      ecLineSelect:
        SelectionMode := smLine;
      ecContextHelp:
        begin
          if Assigned (fOnContextHelp) then
            fOnContextHelp (self,WordAtCursor);
        end;
      ecImeStr:
        if not ReadOnly then
        begin
          SetString(S, PWideChar(Data), WStrLen(Data));
          if SelAvail then
          begin
            BeginUndoBlock;
            try
              fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, Helper,
                smNormal);
              StartOfBlock := fBlockBegin;
              SetSelTextPrimitive(s);
              fUndoList.AddChange(crInsert, fBlockBegin, fBlockEnd, Helper,
                smNormal);
            finally
              EndUndoBlock;
            end;
            InvalidateGutterLines(-1, -1);
          end
          else
          begin
            Temp := LineText;
            Len := Length(Temp);
            if Len < CaretX then
              Temp := Temp + UnicodeStringOfChar(#32, CaretX - Len - 1);
            bChangeScroll := not (eoScrollPastEol in fOptions);
            try
              if bChangeScroll then Include(fOptions, eoScrollPastEol);
              StartOfBlock := CaretXY;
              Len := Length(s);
              if not fInserting then
              begin
                Helper := Copy(Temp, CaretX, Len);
                Delete(Temp, CaretX, Len);
              end;
              Insert(s, Temp, CaretX);
              InternalCaretX := (CaretX + Len);
              ProperSetLine(CaretY - 1, Temp);
              if fInserting then
                Helper := '';
              fUndoList.AddChange(crInsert, StartOfBlock, CaretXY, Helper,
                smNormal);
              if CaretX >= LeftChar + fCharsInWindow then
                LeftChar := LeftChar + min(25, fCharsInWindow - 1);
            finally
              if bChangeScroll then Exclude(fOptions, eoScrollPastEol);
            end;
          end;
        end;
    end;
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.DoOnCommandProcessed(Command: TSynEditorCommand;
  AChar: WideChar; Data: pointer);
begin
  if Assigned(fOnCommandProcessed) then
    fOnCommandProcessed(Self, Command, AChar, Data);
end;

procedure TCustomSynEdit.DoOnProcessCommand(var Command: TSynEditorCommand;
  var AChar: WideChar; Data: pointer);
begin
  if Command < ecUserFirst then
  begin
    if Assigned(FOnProcessCommand) then
      FOnProcessCommand(Self, Command, AChar, Data);
  end
  else begin
    if Assigned(FOnProcessUserCommand) then
      FOnProcessUserCommand(Self, Command, AChar, Data);
  end;
end;

procedure TCustomSynEdit.ClearAll;
begin
  Lines.Clear;
  fMarkList.Clear; // fMarkList.Clear also frees all bookmarks,
  FillChar(fBookMarks, sizeof(fBookMarks), 0); // so fBookMarks should be cleared too
  fUndoList.Clear;
  fRedoList.Clear;
  Modified := False;
end;

procedure TCustomSynEdit.ClearSelection;
begin
  if SelAvail then
    SelText := '';
end;

function TCustomSynEdit.NextWordPosEx(const XY: TBufferCoord): TBufferCoord;
var
  CX, CY, LineLen: Integer;
  Line: UnicodeString;
begin
  CX := XY.Char;
  CY := XY.Line;

  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then
  begin
    Line := Lines[CY - 1];

    LineLen := Length(Line);
    if CX >= LineLen then
    begin
      // find first IdentChar or multibyte char in the next line
      if CY < Lines.Count then
      begin
        Line := Lines[CY];
        Inc(CY);
        CX := StrScanForCharInCategory(Line, 1, IsIdentChar);
        if CX = 0 then
          Inc(CX);
      end;
    end
    else
    begin
      // find next word-break-char if current char is an IdentChar
      if IsIdentChar(Line[CX]) then
        CX := StrScanForCharInCategory(Line, CX, IsWordBreakChar);
      // if word-break-char found, find the next IdentChar
      if CX > 0 then
        CX := StrScanForCharInCategory(Line, CX, IsIdentChar);
      // if one of those failed just position at the end of the line
      if CX = 0 then
        CX := LineLen + 1;
    end;
  end;
  Result.Char := CX;
  Result.Line := CY;
end;

function TCustomSynEdit.WordStartEx(const XY: TBufferCoord): TBufferCoord;
var
  CX, CY: Integer;
  Line: UnicodeString;
begin
  CX := XY.Char;
  CY := XY.Line;
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then
  begin
    Line := Lines[CY - 1];
    CX := Min(CX, Length(Line) + 1);

    if CX > 1 then
    begin  // only find previous char, if not already on start of line
      // if previous char isn't a word-break-char search for the last IdentChar
      if not IsWordBreakChar(Line[CX - 1]) then
        CX := StrRScanForCharInCategory(Line, CX - 1, IsWordBreakChar) + 1;
    end;
  end;
  Result.Char := CX;
  Result.Line := CY;
end;

function TCustomSynEdit.WordEndEx(const XY: TBufferCoord): TBufferCoord;
var
  CX, CY: Integer;
  Line: UnicodeString;
begin
  CX := XY.Char;
  CY := XY.Line;
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then
  begin
    Line := Lines[CY - 1];

    CX := StrScanForCharInCategory(Line, CX, IsWordBreakChar);
    // if no word-break-char is found just position at the end of the line
    if CX = 0 then
      CX := Length(Line) + 1;
  end;
  Result.Char := CX;
  Result.Line := CY;
end;

function TCustomSynEdit.PrevWordPosEx(const XY: TBufferCoord): TBufferCoord;
var
  CX, CY: Integer;
  Line: UnicodeString;
begin
  CX := XY.Char;
  CY := XY.Line;
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then
  begin
    Line := Lines[CY - 1];
    CX := Min(CX, Length(Line) + 1);

    if CX <= 1 then
    begin
      // find last IdentChar in the previous line
      if CY > 1 then
      begin
        Dec(CY);
        Line := Lines[CY - 1];
        CX := Length(Line) + 1;
      end;
    end
    else
    begin
      // if previous char is a word-break-char search for the last IdentChar
      if IsWordBreakChar(Line[CX - 1]) then
        CX := StrRScanForCharInCategory(Line, CX - 1, IsIdentChar);
      if CX > 0 then
        // search for the first IdentChar of this "word"
        CX := StrRScanForCharInCategory(Line, CX - 1, IsWordBreakChar) + 1;
      if CX = 0 then
      begin
        // else just position at the end of the previous line
        if CY > 1 then
        begin
          Dec(CY);
          Line := Lines[CY - 1];
          CX := Length(Line) + 1;
        end
        else
          CX := 1;
      end;
    end;
  end;
  Result.Char := CX;
  Result.Line := CY;
end;

procedure TCustomSynEdit.SetSelectionMode(const Value: TSynSelectionMode);
begin
  if FSelectionMode <> Value then
  begin
    fSelectionMode := Value;
    ActiveSelectionMode := Value;
  end;
end;

procedure TCustomSynEdit.SetActiveSelectionMode(const Value: TSynSelectionMode);
begin
  if fActiveSelectionMode <> Value then
  begin
    if SelAvail then
      InvalidateSelection;
    fActiveSelectionMode := Value;
    if SelAvail then
      InvalidateSelection;
    StatusChanged([scSelection]);
  end;
end;

procedure TCustomSynEdit.SetAdditionalIdentChars(const Value: TSysCharSet);
begin
  FAdditionalIdentChars := Value;
end;

procedure TCustomSynEdit.SetAdditionalWordBreakChars(const Value: TSysCharSet);
begin
  FAdditionalWordBreakChars := Value;
end;

procedure TCustomSynEdit.BeginUndoBlock;
begin
  fUndoList.BeginBlock;
end;

procedure TCustomSynEdit.BeginUpdate;
begin
  IncPaintLock;
end;

procedure TCustomSynEdit.EndUndoBlock;
begin
  fUndoList.EndBlock;
end;

procedure TCustomSynEdit.EndUpdate;
begin
  DecPaintLock;
end;

procedure TCustomSynEdit.AddKey(Command: TSynEditorCommand;
  Key1: word; SS1: TShiftState; Key2: word; SS2: TShiftState);
var
  Key: TSynEditKeyStroke;
begin
  Key := Keystrokes.Add;
  Key.Command := Command;
  Key.Key := Key1;
  Key.Shift := SS1;
  Key.Key2 := Key2;
  Key.Shift2 := SS2;
end;

{ Called by FMarkList if change }
procedure TCustomSynEdit.MarkListChange(Sender: TObject);
begin
  InvalidateGutter;
end;

function TCustomSynEdit.GetSelStart: integer;
begin
  if GetSelAvail then
    Result := RowColToCharIndex(BlockBegin)
  else
    Result := RowColToCharIndex(CaretXY);
end;

procedure TCustomSynEdit.SetAlwaysShowCaret(const Value: Boolean);
begin
  if FAlwaysShowCaret <> Value then
  begin
    FAlwaysShowCaret := Value;
    if not(csDestroying in ComponentState) and  not(focused) then
    begin
      if Value then
      begin
        InitializeCaret;
      end
      else
      begin
        HideCaret;
      {$IFDEF SYN_CLX}
        kTextDrawer.DestroyCaret;
      {$ELSE}
        Windows.DestroyCaret;
      {$ENDIF}
      end;
    end;
  end;
end;

procedure TCustomSynEdit.SetSelStart(const Value: Integer);
begin
  { if we don't call HandleNeeded, CharsInWindow may be 0 and LeftChar will
  be set to CaretX }
  HandleNeeded;
  InternalCaretXY := CharIndexToRowCol(Value);
  BlockBegin := CaretXY;
end;

function TCustomSynEdit.GetSelEnd: Integer;
begin
  if GetSelAvail then
    Result := RowColToCharIndex(Blockend)
  else
    Result := RowColToCharIndex(CaretXY);
end;

procedure TCustomSynEdit.SetSelEnd(const Value: Integer);
begin
  HandleNeeded;
  BlockEnd := CharIndexToRowCol( Value );
end;

procedure TCustomSynEdit.SetSelWord;
begin
  SetWordBlock(CaretXY);
end;

procedure TCustomSynEdit.SetExtraLineSpacing(const Value: Integer);
begin
  fExtraLineSpacing := Value;
  SynFontChanged(self);
end;

function TCustomSynEdit.GetBookMark(BookMark: Integer; var X, Y: Integer):
  Boolean;
var
  i: Integer;
begin
  Result := False;
  if assigned(Marks) then
    for i := 0 to Marks.Count - 1 do
      if Marks[i].IsBookmark and (Marks[i].BookmarkNumber = BookMark) then
      begin
        X := Marks[i].Char;
        Y := Marks[i].Line;
        Result := True;
        Exit;
      end;
end;

function TCustomSynEdit.IsBookmark(BookMark: Integer): Boolean;
var
  x, y: Integer;
begin
  Result := GetBookMark(BookMark, x, y);
end;

procedure TCustomSynEdit.ClearUndo;
begin
  fUndoList.Clear;
  fRedoList.Clear;
end;

procedure TCustomSynEdit.SetSelTextExternal(const Value: UnicodeString);
var
  StartOfBlock, EndOfBlock: TBufferCoord;
begin
  BeginUndoBlock;
  try
    if SelAvail then
    begin
      fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd,
        SelText, fActiveSelectionMode);
    end
    else
      ActiveSelectionMode := SelectionMode;
    StartOfBlock := BlockBegin;
    EndOfBlock := BlockEnd;
    fBlockBegin := StartOfBlock;
    fBlockEnd := EndOfBlock;
    SetSelTextPrimitive(Value);
    if (Value <> '') and (fActiveSelectionMode <> smColumn) then
      fUndoList.AddChange(crInsert, StartOfBlock, BlockEnd, '', fActiveSelectionMode);
  finally
    EndUndoBlock;
  end;
end;

procedure TCustomSynEdit.SetGutter(const Value: TSynGutter);
begin
  fGutter.Assign(Value);
end;

procedure TCustomSynEdit.GutterChanged(Sender: TObject);
var
  nW: Integer;
begin
  if not (csLoading in ComponentState) then
  begin
    if fGutter.ShowLineNumbers and fGutter.AutoSize then
      fGutter.AutoSizeDigitCount(Lines.Count);
    if fGutter.UseFontStyle then
    begin
      fTextDrawer.SetBaseFont(fGutter.Font);
      nW := fGutter.RealGutterWidth(fTextDrawer.CharWidth);
      fTextDrawer.SetBaseFont(Font);
    end
    else
      nW := fGutter.RealGutterWidth(fCharWidth);
    if nW = fGutterWidth then
      InvalidateGutter
    else
      SetGutterWidth(nW);
  end;
end;

procedure TCustomSynEdit.LockUndo;
begin
  fUndoList.Lock;
  fRedoList.Lock;
end;

procedure TCustomSynEdit.UnlockUndo;
begin
  fUndoList.Unlock;
  fRedoList.Unlock;
end;

function TCustomSynEdit.UnifiedSelection: TBufferBlock;
begin
  if BlockBegin.Line > BlockEnd.Line then begin
    result.BeginLine:= BlockEnd.Line;
    result.EndLine:= BlockBegin.Line;
  end else begin
    result.BeginLine:= BlockBegin.Line;
    result.EndLine:= BlockEnd.Line;
  end;
  if BlockBegin.Char > BlockEnd.Char then begin
    result.BeginChar:= BlockEnd.Char;
    result.EndChar:= BlockBegin.Char;
  end else begin
    result.BeginChar:= BlockBegin.Char;
    result.EndChar:= BlockEnd.Char;
  end;
end;


{$IFNDEF SYN_COMPILER_6_UP}
procedure TCustomSynEdit.WMMouseWheel(var Msg: TMessage);
var
  nDelta: Integer;
  nWheelClicks: Integer;
{$IFNDEF SYN_COMPILER_4_UP}
const
  LinesToScroll = 3;
  WHEEL_DELTA = 120;
  WHEEL_PAGESCROLL = MAXDWORD;
  SPI_GETWHEELSCROLLLINES = 104;
{$ENDIF}
begin
  if csDesigning in ComponentState then
    exit;

	Msg.Result := 1;

{$IFDEF SYN_COMPILER_4_UP}
  // In some occasions Windows will not properly initialize mouse wheel, but
  // will still keep sending WM_MOUSEWHEEL message. Calling inherited procedure
  // will re-initialize related properties (i.e. Mouse.WheelScrollLines)
  inherited;
{$ENDIF}

  if GetKeyState(VK_CONTROL) >= 0 then
  begin
{$IFDEF SYN_COMPILER_4_UP}
    nDelta := Mouse.WheelScrollLines
{$ELSE}
    if not SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @nDelta, 0) then
      nDelta := LinesToScroll;
{$ENDIF}
  end
  else
    nDelta := LinesInWindow shr Ord(eoHalfPageScroll in fOptions);

  Inc(fMouseWheelAccumulator, SmallInt(Msg.wParamHi));
  nWheelClicks := fMouseWheelAccumulator div WHEEL_DELTA;
  fMouseWheelAccumulator := fMouseWheelAccumulator mod WHEEL_DELTA;
  if (nDelta = Integer(WHEEL_PAGESCROLL)) or (nDelta > LinesInWindow) then
    nDelta := LinesInWindow;
  TopLine := TopLine - (nDelta * nWheelClicks);
  Update;
  if Assigned(OnScroll) then OnScroll(Self,sbVertical);
end;
{$ENDIF}

{$IFNDEF SYN_CLX}
procedure TCustomSynEdit.WMSetCursor(var Msg: TWMSetCursor);
begin
  if (Msg.HitTest = HTCLIENT) and (Msg.CursorWnd = Handle) and
    not(csDesigning in ComponentState) then
  begin
    UpdateMouseCursor;
  end
  else
    inherited;
end;
{$ENDIF}

procedure TCustomSynEdit.SetTabWidth(Value: Integer);
begin
  Value := MinMax(Value, 1, 256);
  if (Value <> fTabWidth) then begin
    fTabWidth := Value;
    TSynEditStringList(Lines).TabWidth := Value;
    Invalidate; // to redraw text containing tab chars
    if WordWrap then
    begin
      fWordWrapPlugin.Reset;
      InvalidateGutter;
    end;
  end;
end;

procedure TCustomSynEdit.SelectedColorsChanged(Sender: TObject);
begin
  InvalidateSelection;
end;

// find / replace

function TCustomSynEdit.SearchReplace(const ASearch, AReplace: UnicodeString;
  AOptions: TSynSearchOptions): Integer;
var
  ptStart, ptEnd: TBufferCoord; // start and end of the search range
  ptCurrent: TBufferCoord; // current search position
  nSearchLen, nReplaceLen, n, nFound: integer;
  nInLine: integer;
  bBackward, bFromCursor: boolean;
  bPrompt: boolean;
  bReplace, bReplaceAll: boolean;
  bEndUndoBlock: boolean;
  nAction: TSynReplaceAction;
  iResultOffset: Integer;

  function InValidSearchRange(First, Last: Integer): Boolean;
  begin
    Result := True;
    if (fActiveSelectionMode = smNormal) or not (ssoSelectedOnly in AOptions) then
    begin
      if ((ptCurrent.Line = ptStart.Line) and (First < ptStart.Char)) or
        ((ptCurrent.Line = ptEnd.Line) and (Last > ptEnd.Char))
      then
        Result := False;
    end
    else
    if (fActiveSelectionMode = smColumn) then
      // solves bug in search/replace when smColumn mode active and no selection
      Result := (First >= ptStart.Char) and (Last <= ptEnd.Char) or (ptEnd.Char - ptStart.Char < 1);
  end;

begin
  if not Assigned(fSearchEngine) then
    raise ESynEditError.Create('No search engine has been assigned');

  Result := 0;
  // can't search for or replace an empty string
  if Length(ASearch) = 0 then exit;
  // get the text range to search in, ignore the "Search in selection only"
  // option if nothing is selected
  bBackward := (ssoBackwards in AOptions);
  bPrompt := (ssoPrompt in AOptions);
  bReplace := (ssoReplace in AOptions);
  bReplaceAll := (ssoReplaceAll in AOptions);
  bFromCursor := not (ssoEntireScope in AOptions);
  if not SelAvail then Exclude(AOptions, ssoSelectedOnly);
  if (ssoSelectedOnly in AOptions) then begin
    ptStart := BlockBegin;
    ptEnd := BlockEnd;
    // search the whole line in the line selection mode
    if (fActiveSelectionMode = smLine) then
    begin
      ptStart.Char := 1;
      ptEnd.Char := Length(Lines[ptEnd.Line - 1]) + 1;
    end
    else if (fActiveSelectionMode = smColumn) then
      // make sure the start column is smaller than the end column
      if (ptStart.Char > ptEnd.Char) then
        SwapInt(Integer(ptStart.Char), Integer(ptEnd.Char));
    // ignore the cursor position when searching in the selection
    if bBackward then
      ptCurrent := ptEnd
    else
      ptCurrent := ptStart;
  end
  else
  begin
    ptStart.Char := 1;
    ptStart.Line := 1;
    ptEnd.Line := Lines.Count;
    ptEnd.Char := Length(Lines[ptEnd.Line - 1]) + 1;
    if bFromCursor then
      if bBackward then ptEnd := CaretXY else ptStart := CaretXY;
    if bBackward then ptCurrent := ptEnd else ptCurrent := ptStart;
  end;
  // initialize the search engine
  fSearchEngine.Options := AOptions;
  fSearchEngine.Pattern := ASearch;
  // search while the current search position is inside of the search range
  nReplaceLen := 0;
  DoOnPaintTransient(ttBefore);
  if bReplaceAll and not bPrompt then
  begin
    IncPaintLock;
    BeginUndoBlock;
    bEndUndoBlock := True;
  end
  else
    bEndUndoBlock := False;
  try
    while (ptCurrent.Line >= ptStart.Line) and (ptCurrent.Line <= ptEnd.Line) do
    begin
      nInLine := fSearchEngine.FindAll(Lines[ptCurrent.Line - 1]);
      iResultOffset := 0;
      if bBackward then
        n := Pred(fSearchEngine.ResultCount)
      else
        n := 0;
      // Operate on all results in this line.
      while nInLine > 0 do
      begin
        // An occurrence may have been replaced with a text of different length
        nFound := fSearchEngine.Results[n] + iResultOffset;
        nSearchLen := fSearchEngine.Lengths[n];
        if bBackward then Dec(n) else Inc(n);
        Dec(nInLine);
        // Is the search result entirely in the search range?
        if not InValidSearchRange(nFound, nFound + nSearchLen) then continue;
        Inc(Result);
        // Select the text, so the user can see it in the OnReplaceText event
        // handler or as the search result.

        ptCurrent.Char := nFound;
        BlockBegin := ptCurrent;
        // Be sure to use the Ex version of CursorPos so that it appears in the middle if necessary
        SetCaretXYEx(False, BufferCoord(1, ptCurrent.Line));
        EnsureCursorPosVisibleEx(True);
        Inc(ptCurrent.Char, nSearchLen);
        BlockEnd := ptCurrent;
        InternalCaretXY := ptCurrent;
        if bBackward then InternalCaretXY := BlockBegin else InternalCaretXY := ptCurrent;
        // If it's a search only we can leave the procedure now.
        if not (bReplace or bReplaceAll) then exit;
        // Prompt and replace or replace all.  If user chooses to replace
        // all after prompting, turn off prompting.
        if bPrompt and Assigned(fOnReplaceText) then
        begin
          nAction := DoOnReplaceText(ASearch, AReplace, ptCurrent.Line, nFound);
          if nAction = raCancel then
            exit;
        end
        else
          nAction := raReplace;
        if nAction = raSkip then
          Dec(Result)
        else begin
          // user has been prompted and has requested to silently replace all
          // so turn off prompting
          if nAction = raReplaceAll then begin
            if not bReplaceAll or bPrompt then
            begin
              bReplaceAll := True;
              IncPaintLock;
            end;
            bPrompt := False;
            if bEndUndoBlock = false then
              BeginUndoBlock;
            bEndUndoBlock:= true;
          end;
          // Allow advanced substition in the search engine
          SelText := fSearchEngine.Replace(SelText, AReplace);
          nReplaceLen := CaretX - nFound;
        end;
        // fix the caret position and the remaining results
        if not bBackward then begin
          InternalCaretX := nFound + nReplaceLen;
          if (nSearchLen <> nReplaceLen) and (nAction <> raSkip) then
          begin
            Inc(iResultOffset, nReplaceLen - nSearchLen);
            if (fActiveSelectionMode <> smColumn) and (CaretY = ptEnd.Line) then
            begin
              Inc(ptEnd.Char, nReplaceLen - nSearchLen);
              BlockEnd := ptEnd;
            end;
          end;
        end;
        if not bReplaceAll then
          exit;
      end;
      // search next / previous line
      if bBackward then
        Dec(ptCurrent.Line)
      else
        Inc(ptCurrent.Line);
    end;
  finally
    if bReplaceAll and not bPrompt then DecPaintLock;
    if bEndUndoBlock then EndUndoBlock;
    DoOnPaintTransient( ttAfter );
  end;
end;

function TCustomSynEdit.IsPointInSelection(const Value: TBufferCoord): boolean;
var
  ptBegin, ptEnd: TBufferCoord;
begin
  ptBegin := BlockBegin;
  ptEnd := BlockEnd;
  if (Value.Line >= ptBegin.Line) and (Value.Line <= ptEnd.Line) and
    ((ptBegin.Line <> ptEnd.Line) or (ptBegin.Char <> ptEnd.Char)) then
  begin
    if fActiveSelectionMode = smLine then
      Result := True
    else if (fActiveSelectionMode = smColumn) then
    begin
      if (ptBegin.Char > ptEnd.Char) then
        Result := (Value.Char >= ptEnd.Char) and (Value.Char < ptBegin.Char)
      else if (ptBegin.Char < ptEnd.Char) then
        Result := (Value.Char >= ptBegin.Char) and (Value.Char < ptEnd.Char)
      else
        Result := False;
    end
    else
      Result := ((Value.Line > ptBegin.Line) or (Value.Char >= ptBegin.Char)) and
        ((Value.Line < ptEnd.Line) or (Value.Char < ptEnd.Char));
  end
  else
    Result := False;
end;

procedure TCustomSynEdit.SetFocus;
begin
  if (fFocusList.Count > 0) then
  begin
    if TWinControl (fFocusList.Last).CanFocus then
      TWinControl (fFocusList.Last).SetFocus;
    exit;
  end;
  inherited;
end;

procedure TCustomSynEdit.UpdateMouseCursor;

{$IFDEF SYN_CLX}
  procedure SetCursor(aCursor: QCursorH);
  begin
    QWidget_setCursor(Handle, aCursor);
  end;
{$ENDIF}

var
  ptCursor: TPoint;
  ptLineCol: TBufferCoord;
  iNewCursor: TCursor;
begin
  GetCursorPos(ptCursor);
  ptCursor := ScreenToClient(ptCursor);
{$IFDEF SYN_CLX}
  //handle the scrollbars junction in the bottom-right corner
  if not PtInRect(ClientRect, ptCursor) then
  begin
    QWidget_setCursor(Handle, Screen.Cursors[crDefault]);
    Exit;
  end;
{$ENDIF}
  if (ptCursor.X < fGutterWidth) then
    SetCursor(Screen.Cursors[fGutter.Cursor])
  else begin
    ptLineCol := DisplayToBufferPos(PixelsToRowColumn(ptCursor.X, ptCursor.Y));
    if (eoDragDropEditing in fOptions) and (not MouseCapture) and IsPointInSelection(ptLineCol) then
      iNewCursor := crArrow
    else
      iNewCursor := Cursor;
    if Assigned(OnMouseCursor) then
      OnMouseCursor(Self, ptLineCol, iNewCursor);
    fKbdHandler.ExecuteMouseCursor(Self, ptLineCol, iNewCursor);
    SetCursor(Screen.Cursors[iNewCursor]);
  end;
end;

procedure TCustomSynEdit.BookMarkOptionsChanged(Sender: TObject);
begin
  InvalidateGutter;
end;

function TCustomSynEdit.GetOptions: TSynEditorOptions;
begin
  Result := fOptions;
end;

procedure TCustomSynEdit.SetOptions(Value: TSynEditorOptions);
const
  ScrollOptions = [eoDisableScrollArrows,eoHideShowScrollbars,
    eoScrollPastEof,eoScrollPastEol];
var
{$IFNDEF SYN_CLX}
  bSetDrag: Boolean;
{$ENDIF}
  TmpBool: Boolean;
  bUpdateScroll: Boolean;
  vTempBlockBegin, vTempBlockEnd : TBufferCoord;
begin
  if (Value <> fOptions) then
  begin
{$IFNDEF SYN_CLX}
    bSetDrag := (eoDropFiles in fOptions) <> (eoDropFiles in Value);
{$ENDIF}

    if not (eoScrollPastEol in Options) then
      LeftChar := LeftChar;
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;

    bUpdateScroll := (Options * ScrollOptions) <> (Value * ScrollOptions);

    fOptions := Value;

    // constrain caret position to MaxScrollWidth if eoScrollPastEol is enabled
    InternalCaretXY := CaretXY;
    if (eoScrollPastEol in Options) then
    begin
      vTempBlockBegin := BlockBegin;
      vTempBlockEnd := BlockEnd;
      SetBlockBegin(vTempBlockBegin);
      SetBlockEnd(vTempBlockEnd);
    end;

{$IFDEF SYN_CLX}
{$ELSE}
    // (un)register HWND as drop target
    if bSetDrag and not (csDesigning in ComponentState) and HandleAllocated then
      DragAcceptFiles(Handle, (eoDropFiles in fOptions));
{$ENDIF}
    TmpBool := eoShowSpecialChars in Value;
    if TmpBool <> fShowSpecChar then
    begin
      fShowSpecChar := TmpBool;
      Invalidate;
    end;
    if bUpdateScroll then
      UpdateScrollBars;
  end;
end;

procedure TCustomSynEdit.SizeOrFontChanged(bFont: boolean);
begin
  if HandleAllocated and (fCharWidth <> 0) then
  begin
    fCharsInWindow := Max(ClientWidth - fGutterWidth - 2, 0) div fCharWidth;
    fLinesInWindow := ClientHeight div fTextHeight;
    if WordWrap then
    begin
      fWordWrapPlugin.DisplayChanged;
      Invalidate;
    end;
    if bFont then
    begin
      if Gutter.ShowLineNumbers then
        GutterChanged(Self)
      else
        UpdateScrollbars;
      InitializeCaret;
      Exclude(fStateFlags, sfCaretChanged);
      Invalidate;
    end
    else
      UpdateScrollbars;
    Exclude(fStateFlags, sfScrollbarChanged);
    if not (eoScrollPastEol in Options) then
      LeftChar := LeftChar;
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;
  end;
end;

procedure TCustomSynEdit.MoveCaretHorz(DX: Integer; SelectionCommand: Boolean);
var
  ptO, ptDst: TBufferCoord;
  s: UnicodeString;
  nLineLen: Integer;
  bChangeY: Boolean;
  vCaretRowCol: TDisplayCoord;
begin
  if WordWrap then
  begin
    if DX > 0 then
    begin
      if fCaretAtEOL then
      begin
        fCaretAtEOL := False;
        UpdateLastCaretX;
        IncPaintLock;
        Include(fStateFlags, sfCaretChanged);
        DecPaintLock;
        Exit;
      end;
    end
    else
    begin // DX < 0. Handle ecLeft/ecPageLeft at BOL.
      if (not fCaretAtEOL) and (CaretX > 1) and (DisplayX = 1) then
      begin
        fCaretAtEOL := True;
        UpdateLastCaretX;
        if DisplayX > CharsInWindow +1 then
          SetInternalDisplayXY( DisplayCoord(CharsInWindow +1, DisplayY) )
        else begin
          IncPaintLock;
          Include(fStateFlags, sfCaretChanged);
          DecPaintLock;
        end;
        Exit;
      end;
    end;
  end;
  ptO := CaretXY;
  ptDst := ptO;
  s := LineText;
  nLineLen := Length(s);
  // only moving or selecting one char can change the line
  bChangeY := not (eoScrollPastEol in fOptions);
  if bChangeY and (DX = -1) and (ptO.Char = 1) and (ptO.Line > 1) then
  begin
    // end of previous line
    Dec(ptDst.Line);
    ptDst.Char := Length(Lines[ptDst.Line - 1]) + 1;
  end
  else if bChangeY and (DX = 1) and (ptO.Char > nLineLen) and (ptO.Line < Lines.Count) then
  begin
    // start of next line
    Inc(ptDst.Line);
    ptDst.Char := 1;
  end
  else begin
    ptDst.Char := Max(1, ptDst.Char + DX);
    // don't go past last char when ScrollPastEol option not set
    if (DX > 0) and bChangeY then
      ptDst.Char := Min(ptDst.Char, nLineLen + 1);
  end;
  // set caret and block begin / end
  MoveCaretAndSelection(fBlockBegin, ptDst, SelectionCommand);
  // if caret is beyond CharsInWindow move to next row (this means there are
  // spaces/tabs at the end of the row)
  if WordWrap and (DX > 0) and (CaretX < Length(LineText)) then
  begin
    vCaretRowCol := DisplayXY;
    if (vCaretRowCol.Column = 1) and (LineToRow(CaretY) <> vCaretRowCol.Row) then
    begin
      fCaretAtEOL := True;
      UpdateLastCaretX;
    end
    else if vCaretRowCol.Column > CharsInWindow +1 then
    begin
      Inc(vCaretRowCol.Row);
      vCaretRowCol.Column := 1;
      InternalCaretXY := DisplayToBufferPos(vCaretRowCol);
    end;
  end;
end;

procedure TCustomSynEdit.MoveCaretVert(DY: Integer; SelectionCommand: Boolean);
var
  ptO, ptDst, vEOLTestPos: TDisplayCoord;
  vDstLineChar: TBufferCoord;
  SaveLastCaretX: Integer;
begin
  ptO := DisplayXY;
  ptDst := ptO;

  Inc(ptDst.Row, DY);
  if DY >= 0 then
  begin
    if RowToLine(ptDst.Row) > Lines.Count then
      ptDst.Row := Max(1, DisplayLineCount);
  end
  else begin
    if ptDst.Row < 1 then
      ptDst.Row := 1;
  end;

  if (ptO.Row <> ptDst.Row) then
  begin
    if eoKeepCaretX in Options then
      ptDst.Column := fLastCaretX;
  end;
  vDstLineChar := DisplayToBufferPos(ptDst);
  SaveLastCaretX := fLastCaretX;

  // set caret and block begin / end
  IncPaintLock;
  MoveCaretAndSelection(fBlockBegin, vDstLineChar, SelectionCommand);
  if WordWrap then
  begin
    vEOLTestPos := BufferToDisplayPos(vDstLineChar);
    fCaretAtEOL := (vEOLTestPos.Column = 1) and (vEOLTestPos.Row <> ptDst.Row);
  end;
  DecPaintLock;

  // Restore fLastCaretX after moving caret, since UpdateLastCaretX, called by
  // SetCaretXYEx, changes them. This is the one case where we don't want that.
  fLastCaretX := SaveLastCaretX;
end;

procedure TCustomSynEdit.MoveCaretAndSelection(const ptBefore, ptAfter: TBufferCoord;
  SelectionCommand: Boolean);
begin
  if (eoGroupUndo in FOptions) and UndoList.CanUndo then
    fUndoList.AddGroupBreak;

  IncPaintLock;
  if SelectionCommand then
  begin
    if not SelAvail then
      SetBlockBegin(ptBefore);
    SetBlockEnd(ptAfter);
  end
  else
    SetBlockBegin(ptAfter);
  InternalCaretXY := ptAfter;
  DecPaintLock;
end;

procedure TCustomSynEdit.SetCaretAndSelection(const ptCaret, ptBefore,
  ptAfter: TBufferCoord);
var
  vOldMode: TSynSelectionMode;
begin
  vOldMode := fActiveSelectionMode;
  IncPaintLock;
  try
    InternalCaretXY := ptCaret;
    SetBlockBegin(ptBefore);
    SetBlockEnd(ptAfter);
  finally
    ActiveSelectionMode := vOldMode;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.RecalcCharExtent;
const
  iFontStyles: array[0..3] of TFontStyles = ([], [fsItalic], [fsBold],
    [fsItalic, fsBold]);
var
  iHasStyle: array[0..3] of Boolean;
  cAttr: Integer;
  cStyle: Integer;
  iCurr: TFontStyles;
begin
  FillChar(iHasStyle, SizeOf(iHasStyle), 0);
  if Assigned(fHighlighter) and (fHighlighter.AttrCount > 0) then begin
    for cAttr := 0 to fHighlighter.AttrCount - 1 do
    begin
      iCurr := fHighlighter.Attribute[cAttr].Style * [fsItalic, fsBold];
      for cStyle := 0 to 3 do
        if iCurr = iFontStyles[cStyle] then
        begin
          iHasStyle[cStyle] := True;
          break;
        end;
    end;
  end
  else begin
    iCurr := Font.Style * [fsItalic, fsBold];
    for cStyle := 0 to 3 do
      if iCurr = iFontStyles[cStyle] then
      begin
        iHasStyle[cStyle] := True;
        break;
      end;
  end;

  fTextHeight := 0;
  fCharWidth := 0;
  fTextDrawer.BaseFont := Self.Font;
  for cStyle := 0 to 3 do
    if iHasStyle[cStyle] then
    begin
      fTextDrawer.BaseStyle := iFontStyles[cStyle];
      fTextHeight := Max(fTextHeight, fTextDrawer.CharHeight);
      fCharWidth := Max(fCharWidth, fTextDrawer.CharWidth);
    end;
  Inc(fTextHeight, fExtraLineSpacing);
end;

procedure TCustomSynEdit.HighlighterAttrChanged(Sender: TObject);
begin
  RecalcCharExtent;
  if Sender is TSynCustomHighlighter then
  begin
    Lines.BeginUpdate;
    try
      ScanRanges;
    finally
      Lines.EndUpdate;
    end;
  end
  else
    Invalidate;
  SizeOrFontChanged(True);
end;

procedure TCustomSynEdit.StatusChanged(AChanges: TSynStatusChanges);
begin
  fStatusChanges := fStatusChanges + AChanges;
  if PaintLock = 0 then
    DoOnStatusChange(fStatusChanges);
end;

procedure TCustomSynEdit.DoCaseChange(const Cmd: TSynEditorCommand);

  function ToggleCase(const aStr: UnicodeString): UnicodeString;
  var
    i: Integer;
    sLower: UnicodeString;
  begin
    Result := SynWideUpperCase(aStr);
    sLower := SynWideLowerCase(aStr);
    for i := 1 to Length(aStr) do
    begin
      if Result[i] = aStr[i] then
        Result[i] := sLower[i];
    end;
  end;

var
  w: UnicodeString;
  oldCaret, oldBlockBegin, oldBlockEnd: TBufferCoord;
  bHadSel : Boolean;
begin
  Assert((Cmd >= ecUpperCase) and (Cmd <= ecToggleCaseBlock));
  if SelAvail then
  begin
    bHadSel := True;
    oldBlockBegin := BlockBegin;
    oldBlockEnd := BlockEnd;
  end
  else begin
    bHadSel := False;
  end;
  oldCaret := CaretXY;
  try
    if Cmd < ecUpperCaseBlock then
    begin
      { word commands }
      SetSelWord;
      if SelText = '' then
      begin
        { searches a previous word }
        InternalCaretXY := PrevWordPos;
        SetSelWord;
        if SelText = '' then
        begin
          { try once more since PrevWordPos may have failed last time.
          (PrevWordPos "points" to the end of the previous line instead of the
          beggining of the previous word if invoked (e.g.) when CaretX = 1) }
          InternalCaretXY := PrevWordPos;
          SetSelWord;
        end;
      end;
    end
    else begin
      { block commands }
      if not SelAvail then
      begin
        if CaretX <= Length(LineText) then
          MoveCaretHorz(1, True)
        else if CaretY < Lines.Count then
          InternalCaretXY := BufferCoord(1, CaretY +1);
      end;
    end;

    w := SelText;
    if w <> '' then
    begin
      case Cmd of
        ecUpperCase, ecUpperCaseBlock:
          w := SynWideUpperCase(w);
        ecLowerCase, ecLowerCaseBlock:
          w := SynWideLowerCase(w);
        ecToggleCase, ecToggleCaseBlock:
          w := ToggleCase(w);
        ecTitleCase:
          w := SynWideUpperCase(w[1]) + SynWideLowerCase(Copy(w, 2, Length(w)));
      end;
      BeginUndoBlock;
      try
        if bHadSel then
          fUndoList.AddChange(crSelection, oldBlockBegin, oldBlockEnd, '', fActiveSelectionMode)
        else
          fUndoList.AddChange(crSelection, oldCaret, oldCaret, '', fActiveSelectionMode);
        fUndoList.AddChange(crCaret, oldCaret, oldCaret, '', fActiveSelectionMode);
        SelText := w;
      finally
        EndUndoBlock;
      end;
    end;
  finally
    { "word" commands do not restore Selection }
    if bHadSel and (Cmd >= ecUpperCaseBlock) then
    begin
      BlockBegin := oldBlockBegin;
      BlockEnd := oldBlockEnd;
    end;
    { "block" commands with empty Selection move the Caret }
    if bHadSel or (Cmd < ecUpperCaseBlock) then
      CaretXY := oldCaret;
  end;
end;

procedure TCustomSynEdit.DoTabKey;
var
  StartOfBlock: TBufferCoord;
  i, MinLen, iLine: integer;
  PrevLine, Spaces: UnicodeString;
  p: PWideChar;
  NewCaretX: integer;
  ChangeScroll: Boolean;
  nPhysX, nDistanceToTab, nSpacesToNextTabStop : Integer;
  OldSelTabLine, vIgnoreSmartTabs: Boolean;
begin
  // Provide Visual Studio like block indenting
  OldSelTabLine := SelTabLine;
  if (eoTabIndent in Options) and ((SelTabBlock) or (OldSelTabLine)) then
  begin
    DoBlockIndent;
    if OldSelTabLine then
    begin
      if fBlockBegin.Char < fBlockEnd.Char then
        FBlockBegin.Char := 1
      else
        fBlockEnd.Char := 1;
    end;
    exit;
  end;
  i := 0;
  iLine := 0;
  MinLen := 0;
  vIgnoreSmartTabs := False;
  if eoSmartTabs in fOptions then
  begin
    iLine := CaretY - 1;
    if (iLine > 0) and (iLine < Lines.Count) then
    begin
      Dec(iLine);
      repeat
        //todo: rethink it
        MinLen := DisplayToBufferPos(DisplayCoord(
          BufferToDisplayPos(CaretXY).Column, LineToRow(iLine + 1))).Char;
        PrevLine := Lines[iLine];
        if (Length(PrevLine) >= MinLen) then begin
          p := @PrevLine[MinLen];
          // scan over non-whitespaces
          repeat
            if (p^ = #9) or (p^ = #32) then break;
            Inc(i);
            Inc(p);
          until p^ = #0;
          // scan over whitespaces
          if p^ <> #0 then
            repeat
              if (p^ <> #9) and (p^ <> #32) then break;
              Inc(i);
              Inc(p);
            until p^ = #0;
          break;
        end;
        Dec(iLine);
      until iLine < 0;
    end
    else
      vIgnoreSmartTabs := True;
  end;
  fUndoList.BeginBlock;
  try
    if SelAvail then
    begin
      fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, SelText,
        fActiveSelectionMode);
      SetSelTextPrimitive('');
    end;
    StartOfBlock := CaretXY;

    if i = 0 then
    begin
      if (eoTabsToSpaces in fOptions) then
      begin
        i := TabWidth - (StartOfBlock.Char - 1) mod TabWidth;
        if i = 0 then
          i := TabWidth;
      end
      else
        i := TabWidth;
    end;

    if eoTabsToSpaces in fOptions then
    begin
      Spaces := UnicodeStringOfChar(#32, i);
      NewCaretX := StartOfBlock.Char + i;
    end
    else if (eoTrimTrailingSpaces in Options) and (StartOfBlock.Char > Length(LineText)) then
    begin
      // work-around for trimming Tabs
      nPhysX := BufferToDisplayPos(CaretXY).Column;
      if (eoSmartTabs in fOptions) and not vIgnoreSmartTabs and (iLine > -1) then
      begin
        i := BufferToDisplayPos( BufferCoord(MinLen+i, iLine+1) ).Column;
        nDistanceToTab := i - nPhysX;
      end
      else
        nDistanceToTab := TabWidth - ((nPhysX - 1) mod TabWidth);
      NewCaretX := StartOfBlock.Char + nDistanceToTab;
    end
    else begin
      if (eoSmartTabs in fOptions) and not vIgnoreSmartTabs and (iLine > -1) then
      begin
        Spaces := Copy(fLines[CaretXY.Line - 1], 1, CaretXY.Char - 1);
        while Pos(#9, Spaces) > 0 do
          Delete(Spaces, Pos(#9, Spaces), 1);
        Spaces := WideTrim(Spaces);

        //smart tabs are only in the front of the line *NOT IN THE MIDDLE*
        if Spaces = '' then
        begin
          i := BufferToDisplayPos( BufferCoord(MinLen+i, iLine+1) ).Column;

          nPhysX := DisplayX;
          nDistanceToTab := i - nPhysX;
          nSpacesToNextTabStop := TabWidth - ((nPhysX - 1) mod TabWidth);
          if nSpacesToNextTabStop <= nDistanceToTab then begin
            Spaces := #9;
            Dec(nDistanceToTab, nSpacesToNextTabStop);
          end;
          while nDistanceToTab >= TabWidth do begin
            Spaces := Spaces + #9;
            Dec(nDistanceToTab, TabWidth);
          end;
          if nDistanceToTab > 0 then
            Spaces := Spaces + UnicodeStringOfChar(#32, nDistanceToTab);
        end else
          Spaces := #9;
      end
      else begin
        Spaces := #9;
      end;
      if (eoTrimTrailingSpaces in Options) and (Length(TrimTrailingSpaces(LineText)) = 0) then
        NewCaretX := StartOfBlock.Char + GetExpandedLength(Spaces, TabWidth)
      else
        NewCaretX := StartOfBlock.Char + Length(Spaces);
    end;

    SetSelTextPrimitive(Spaces);
    // Undo is already handled in SetSelText when SelectionMode is Column
    if fActiveSelectionMode <> smColumn then
    begin
      fUndoList.AddChange(crInsert, StartOfBlock, CaretXY, SelText,
        fActiveSelectionMode);
    end;
  finally
    fUndoList.EndBlock;
  end;

  ChangeScroll := not(eoScrollPastEol in fOptions);
  try
    Include(fOptions, eoScrollPastEol);
    InternalCaretX := NewCaretX;
  finally
    if ChangeScroll then
      Exclude(fOptions, eoScrollPastEol);
  end;

  EnsureCursorPosVisible;
end;

procedure TCustomSynEdit.DoShiftTabKey;
// shift-tab key handling
var
  NewX: Integer;
  Line: UnicodeString;
  LineLen: Integer;
  DestX: Integer;

  MaxLen, iLine: Integer;
  PrevLine, OldSelText: UnicodeString;
  p: PWideChar;
  OldCaretXY: TBufferCoord;
  ChangeScroll: Boolean;
begin
  // Provide Visual Studio like block indenting
  if (eoTabIndent in Options) and ((SelTabBlock) or (SelTabLine)) then
  begin
    DoBlockUnIndent;
    exit;
  end;

  NewX := CaretX;

  if (NewX <> 1) and (eoSmartTabs in fOptions) then
  begin
    iLine := CaretY - 1;
    if (iLine > 0) and (iLine < Lines.Count) then
    begin
      Dec(iLine);
      MaxLen := CaretX - 1;
      repeat
        PrevLine := Lines[iLine];
        if (Length(PrevLine) >= MaxLen) then
        begin
          p := @PrevLine[MaxLen];
          // scan over whitespaces
          repeat
            if p^ <> #32 then break;
            Dec(NewX);
            Dec(p);
          until NewX = 1;
          // scan over non-whitespaces
          if NewX <> 1 then
            repeat
              if p^ = #32 then break;
              Dec(NewX);
              Dec(p);
            until NewX = 1;
          break;
        end;
        Dec(iLine);
      until iLine < 0;
    end;
  end;

  if NewX = CaretX then
  begin
    Line := LineText;
    LineLen := Length(Line);

    // find real un-tab position

    DestX := ((CaretX - 2) div TabWidth) * TabWidth + 1;
    if NewX > LineLen then
      NewX := DestX
    else if (NewX > DestX) and (Line[NewX - 1] = #9) then
      dec(NewX)
    else begin
      while (NewX > DestX) and ((NewX - 1 > LineLen) or (Line[NewX - 1] = #32)) do
        dec(NewX);
    end;
  end;

  // perform un-tab
  if (NewX <> CaretX) then
  begin
    SetBlockBegin(BufferCoord(NewX, CaretY));
    SetBlockEnd(CaretXY);
    OldCaretXY := CaretXY;

    OldSelText := SelText;
    SetSelTextPrimitive('');

    fUndoList.AddChange(crSilentDelete, BufferCoord(NewX, CaretY),
      OldCaretXY, OldSelText, smNormal);

    // KV
    ChangeScroll := not(eoScrollPastEol in fOptions);
    try
      Include(fOptions, eoScrollPastEol);
      InternalCaretX := NewX;
    finally
      if ChangeScroll then
        Exclude(fOptions, eoScrollPastEol);
    end;
  end;
end;

procedure TCustomSynEdit.DoHomeKey(Selection: Boolean);

  function LastCharInRow: Integer;
  var
    vPos: TDisplayCoord;
  begin
    if fLines.Count = 0 then
      Result := 1
    else
    begin
      vPos := DisplayXY;
      vPos.Column := Min(CharsInWindow, fWordWrapPlugin.GetRowLength(vPos.Row) + 1);
      Result := DisplayToBufferPos(vPos).Char;
    end;
  end;

var
  newX: Integer;
  first_nonblank: Integer;
  s: UnicodeString;
  vNewPos: TDisplayCoord;
  vMaxX: Integer;
begin
  // home key enhancement
  if (eoEnhanceHomeKey in fOptions) and (LineToRow(CaretY) = DisplayY) then
  begin
    s := fLines[CaretXY.Line - 1];

    first_nonblank := 1;
    if WordWrap then
      vMaxX := LastCharInRow() -1
    else
      vMaxX := Length(s);
    while (first_nonblank <= vMaxX) and
      CharInSet(s[first_nonblank], [#32, #9])
    do
      inc(first_nonblank);
    dec(first_nonblank);

    newX := CaretXY.Char - 1;

    if (newX > first_nonblank) or (newX = 0) then
      newX := first_nonblank + 1
    else
      newX := 1;
  end
  else
    newX := 1;

  if WordWrap then
  begin
    vNewPos.Row := DisplayY;
    vNewPos.Column := BufferToDisplayPos(BufferCoord(newX, CaretY)).Column;
    MoveCaretAndSelection(CaretXY, DisplayToBufferPos(vNewPos), Selection);
  end
  else
    MoveCaretAndSelection(CaretXY, BufferCoord(newX, CaretY), Selection);
end;

procedure TCustomSynEdit.DoEndKey(Selection: Boolean);

  function CaretInLastRow: Boolean;
  var
    vLastRow: Integer;
  begin
    if not WordWrap then
      Result := True
    else
    begin
      vLastRow := LineToRow(CaretY + 1) - 1;
      // This check allows good behaviour with empty rows (this can be useful in a diff app ;-)
      while (vLastRow > 1)
        and (fWordWrapPlugin.GetRowLength(vLastRow) = 0)
        and (RowToLine(vLastRow) = CaretY) do
      begin
        Dec(vLastRow);
      end;
      Result := DisplayY = vLastRow;
    end;
  end;

  function FirstCharInRow: Integer;
  var
    vPos: TDisplayCoord;
  begin
    vPos.Row := DisplayY;
    vPos.Column := 1;
    Result := DisplayToBufferPos(vPos).Char;
  end;

var
  vText: UnicodeString;
  vLastNonBlank: Integer;
  vNewX: Integer;
  vNewCaret: TDisplayCoord;
  vMinX: Integer;
  vEnhance: Boolean;
begin
  if (eoEnhanceEndKey in fOptions) and CaretInLastRow then
  begin
    vEnhance := True;
    vText := LineText;
    vLastNonBlank := Length(vText);
    if WordWrap then
      vMinX := FirstCharInRow() - 1
    else
      vMinX := 0;
    while (vLastNonBlank > vMinX) and CharInSet(vText[vLastNonBlank], [#32, #9]) do
      Dec(vLastNonBlank);

    vNewX := CaretX - 1;
    if vNewX = vLastNonBlank then
      vNewX := Length(LineText) + 1
    else
      vNewX := vLastNonBlank + 1;
  end
  else
  begin
    vNewX := Length(LineText) + 1;
    vEnhance := False;
  end;

  if WordWrap then
  begin
    vNewCaret.Row := DisplayY;
    if vEnhance then
      vNewCaret.Column := BufferToDisplayPos(BufferCoord(vNewX, CaretY)).Column
    else
      vNewCaret.Column := fWordWrapPlugin.GetRowLength(vNewCaret.Row) + 1;
    vNewCaret.Column := Min(CharsInWindow + 1, vNewCaret.Column);
    MoveCaretAndSelection(CaretXY, DisplayToBufferPos(vNewCaret), Selection);
    // Updates fCaretAtEOL flag.
    SetInternalDisplayXY(vNewCaret);
  end
  else
    MoveCaretAndSelection(CaretXY,
      BufferCoord(vNewX, CaretY), Selection);
end;

{$IFNDEF SYN_CLX}
procedure TCustomSynEdit.CreateWnd;
begin
  inherited;

{$IFNDEF UNICODE}
  if not (csDesigning in ComponentState) then
  begin
    // "redefine" window-procedure to get Unicode messages
    if Win32PlatformIsUnicode then
      SetWindowLongW(Handle, GWL_WNDPROC, Integer(GetWindowLongA(Handle, GWL_WNDPROC)));
  end;
{$ENDIF}

  if (eoDropFiles in fOptions) and not (csDesigning in ComponentState) then
    DragAcceptFiles(Handle, True);

  UpdateScrollBars;
end;

procedure TCustomSynEdit.DestroyWnd;
begin
  if (eoDropFiles in fOptions) and not (csDesigning in ComponentState) then
    DragAcceptFiles(Handle, False);

{$IFNDEF UNICODE}
  if not (csDesigning in ComponentState) then
  begin
    // restore window-procedure to what VCL expects
    if Win32PlatformIsUnicode then
      SetWindowLongA(Handle, GWL_WNDPROC, Integer(GetWindowLongW(Handle, GWL_WNDPROC)));
  end;
{$ENDIF}

{$IFDEF UNICODE}
  // assign WindowText here, otherwise the VCL will call GetText twice
  if WindowText = nil then
     WindowText := Lines.GetText;
{$ENDIF}
  inherited;
end;

procedure TCustomSynEdit.InvalidateRect(const aRect: TRect; aErase: Boolean);
begin
  Windows.InvalidateRect(Handle, @aRect, aErase);
end;
{$ENDIF}

procedure TCustomSynEdit.DoBlockIndent;
var
  OrgCaretPos: TBufferCoord;
  BB, BE: TBufferCoord;
  Run, StrToInsert: PWideChar;
  e, x, i, InsertStrLen: Integer;
  Spaces: UnicodeString;
  OrgSelectionMode: TSynSelectionMode;
  InsertionPos: TBufferCoord;
begin
  OrgSelectionMode := fActiveSelectionMode;
  OrgCaretPos := CaretXY;

  StrToInsert := nil;
  if SelAvail then
  try
    // keep current selection detail
    BB := BlockBegin;
    BE := BlockEnd;

    // build text to insert
    if (BE.Char = 1) then
    begin
      e := BE.Line - 1;
      x := 1;
    end
    else begin
      e := BE.Line;
      if eoTabsToSpaces in Options then
        x := CaretX + FTabWidth
      else x := CaretX + 1;
    end;
    if (eoTabsToSpaces in Options) then
    begin
      InsertStrLen := (FTabWidth + 2) * (e - BB.Line) + FTabWidth + 1;
      //               chars per line * lines-1    + last line + null char
      StrToInsert := WStrAlloc(InsertStrLen);
      Run := StrToInsert;
      Spaces := UnicodeStringOfChar(#32, FTabWidth);
    end
    else begin
      InsertStrLen:= 3 * (e - BB.Line) + 2;
      //         #9#13#10 * lines-1 + (last line's #9 + null char)
      StrToInsert := WStrAlloc(InsertStrLen);
      Run := StrToInsert;
      Spaces := #9;
    end;
    for i := BB.Line to e-1 do
    begin
      WStrCopy(Run, PWideChar(Spaces + #13#10));
      Inc(Run, Length(spaces) + 2);
    end;
    WStrCopy(Run, PWideChar(Spaces));

    fUndoList.BeginBlock;
    try
      InsertionPos.Line := BB.Line;
      if fActiveSelectionMode = smColumn then
        InsertionPos.Char := Min(BB.Char, BE.Char)
      else
        InsertionPos.Char := 1;
      InsertBlock(InsertionPos, InsertionPos, StrToInsert, True);
      fUndoList.AddChange(crIndent, BB, BE, '', smColumn);
      //We need to save the position of the end block for redo
      fUndoList.AddChange(crIndent,
        BufferCoord(BB.Char + length(Spaces), BB.Line),
        BufferCoord(BE.Char + length(Spaces), BE.Line),
        '', smColumn);
    finally
      fUndoList.EndBlock;
    end;

    //adjust the x position of orgcaretpos appropriately
    OrgCaretPos.Char := X;
  finally
    if BE.Char > 1 then
      Inc(BE.Char, Length(Spaces));
    WStrDispose(StrToInsert);
    SetCaretAndSelection(OrgCaretPos,
      BufferCoord(BB.Char + Length(Spaces), BB.Line), BE);
    ActiveSelectionMode := OrgSelectionMode;
  end;
end;

procedure TCustomSynEdit.DoBlockUnindent;
var
  OrgCaretPos,
  BB, BE: TBufferCoord;
  Line, Run,
  FullStrToDelete,
  StrToDelete: PWideChar;
  Len, x, StrToDeleteLen, i, TmpDelLen, FirstIndent, LastIndent, e: Integer;
  TempString: UnicodeString;
  OrgSelectionMode: TSynSelectionMode;
  SomethingToDelete: Boolean;

  function GetDelLen: Integer;
  var
    Run: PWideChar;
  begin
    Result := 0;
    Run := Line;
    //Take care of tab character
    if Run[0] = #9 then
    begin
      Result := 1;
      SomethingToDelete := True;
      exit;
    end;
    //Deal with compound tabwidths  Sometimes they have TabChars after a few
    //spaces, yet we need to delete the whole tab width even though the char
    //count might not be FTabWidth because of the TabChar
    while (Run[0] = #32) and (Result < FTabWidth) do
    begin
      Inc(Result);
      Inc(Run);
      SomethingToDelete := True;
    end;
    if (Run[0] = #9) and (Result < FTabWidth) then
      Inc(Result);
  end;

begin
  OrgSelectionMode := fActiveSelectionMode;
  Len := 0;
  LastIndent := 0;
  if SelAvail then
  begin
    // store current selection detail
    BB := BlockBegin;
    BE := BlockEnd;
    OrgCaretPos := CaretXY;
    x := fCaretX;

    // convert selection to complete lines
    if BE.Char = 1 then
      e := BE.Line - 1
    else
      e := BE.Line;

    // build string to delete
    StrToDeleteLen := (FTabWidth + 2) * (e - BB.Line) + FTabWidth + 1;
    //                chars per line * lines-1    + last line + null char
    StrToDelete := WStrAlloc(StrToDeleteLen);
    StrToDelete[0] := #0;
    SomethingToDelete := False;
    for i := BB.Line to e-1 do
    begin
       Line := PWideChar(Lines[i - 1]);
       //'Line' is 0-based, 'BB.x' is 1-based, so the '-1'
       //And must not increment 'Line' pointer by more than its 'Length'
       if fActiveSelectionMode = smColumn then
         Inc(Line, MinIntValue([BB.Char - 1, BE.Char - 1, Length(Lines[i - 1])]));
       //Instead of doing a UnicodeStringOfChar, we need to get *exactly* what was
       //being deleted incase there is a TabChar
       TmpDelLen := GetDelLen;
       WStrCat(StrToDelete, PWideChar(Copy(Line, 1, TmpDelLen)));
       WStrCat(StrToDelete, PWideChar(UnicodeString(#13#10)));
       if (fCaretY = i) and (x <> 1) then
         x := x - TmpDelLen;
    end;
    Line := PWideChar(Lines[e - 1]);
    if fActiveSelectionMode = smColumn then
      Inc(Line, MinIntValue([BB.Char - 1, BE.Char - 1, Length(Lines[e - 1])]));
    TmpDelLen := GetDelLen;
    WStrCat(StrToDelete, PWideChar(Copy(Line, 1, TmpDelLen)));
    if (fCaretY = e) and (x <> 1) then
      x := x - TmpDelLen;

    FirstIndent := -1;
    FullStrToDelete := nil;
    // Delete string
    if SomethingToDelete then
    begin
      FullStrToDelete := StrToDelete;
      InternalCaretY := BB.Line;
      if fActiveSelectionMode <> smColumn then
        i := 1
      else
        i := Min(BB.Char, BE.Char);
      repeat
        Run := GetEOL(StrToDelete);
        if Run <> StrToDelete then
        begin
          Len := Run - StrToDelete;
          if FirstIndent = -1 then
            FirstIndent := Len;
          if Len > 0 then
          begin
            TempString := Lines[CaretY - 1];
            Delete(TempString, i, Len);
            Lines[CaretY - 1] := TempString;
          end;
        end;
        if Run^ = #13 then
        begin
          Inc(Run);
          if Run^ = #10 then
            Inc(Run);
          Inc(fCaretY);
        end;
        StrToDelete := Run;
      until Run^ = #0;
      LastIndent := Len;
      fUndoList.AddChange(crUnindent, BB, BE, FullStrToDelete, fActiveSelectionMode);
    end;
    // restore selection
    if FirstIndent = -1 then
      FirstIndent := 0;
    //adjust the x position of orgcaretpos appropriately
    if fActiveSelectionMode = smColumn then
      SetCaretAndSelection(OrgCaretPos, BB, BE)
    else
    begin
      OrgCaretPos.Char := X;
      Dec(BB.Char, FirstIndent);
      Dec(BE.Char, LastIndent);
      SetCaretAndSelection(OrgCaretPos, BB, BE);
    end;
    ActiveSelectionMode := OrgSelectionMode;
    if FullStrToDelete <> nil then
      WStrDispose(FullStrToDelete)
    else
      WStrDispose(StrToDelete);
  end;
end;

{$IFDEF SYN_COMPILER_4_UP}
function TCustomSynEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  if Action is TEditAction then
  begin
    Result := Focused;
    if Result then
    begin
      if Action is TEditCut then
        CommandProcessor(ecCut, ' ', nil)
      else if Action is TEditCopy then
        CommandProcessor(ecCopy, ' ', nil)
      else if Action is TEditPaste then
        CommandProcessor(ecPaste, ' ', nil)
{$IFDEF SYN_COMPILER_5_UP}
      else if Action is TEditDelete then
      begin
        if SelAvail then
          ClearSelection
        else
          CommandProcessor(ecDeleteChar, ' ', nil)
      end
{$IFDEF SYN_CLX}
{$ELSE}
      else if Action is TEditUndo then
        CommandProcessor(ecUndo, ' ', nil)
{$ENDIF}
      else if Action is TEditSelectAll then
        CommandProcessor(ecSelectAll, ' ', nil);
{$ENDIF}
    end
  end
{$IFDEF SYN_COMPILER_6_UP}
  else if Action is TSearchAction then
  begin
    Result := Focused;
    if Action is TSearchFindFirst then
      DoSearchFindFirstExecute(TSearchFindFirst(Action))
    else if Action is TSearchFind then
      DoSearchFindExecute(TSearchFind(Action))
    else if Action is TSearchReplace then
      DoSearchReplaceExecute(TSearchReplace(Action));
  end
  else if Action is TSearchFindNext then
  begin
    Result := Focused;
    DoSearchFindNextExecute(TSearchFindNext(Action))
  end
{$ENDIF}
  else
    Result := inherited ExecuteAction(Action);
end;

function TCustomSynEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  if Action is TEditAction then
  begin
    Result := Focused;
    if Result then
    begin
      if Action is TEditCut then
        TEditAction(Action).Enabled := SelAvail and not ReadOnly
      else if Action is TEditCopy then
        TEditAction(Action).Enabled := SelAvail
      else if Action is TEditPaste then
        TEditAction(Action).Enabled := CanPaste
{$IFDEF SYN_COMPILER_5_UP}
      else if Action is TEditDelete then
        TEditAction(Action).Enabled := not ReadOnly
{$IFDEF SYN_CLX}
{$ELSE}
      else if Action is TEditUndo then
        TEditAction(Action).Enabled := CanUndo
{$ENDIF}
      else if Action is TEditSelectAll then
        TEditAction(Action).Enabled := True;
{$ENDIF}
    end;
{$IFDEF SYN_COMPILER_6_UP}
  end else if Action is TSearchAction then
  begin
    Result := Focused;
    if Result then
    begin
      if Action is TSearchFindFirst then
        TSearchAction(Action).Enabled := (Text<>'') and assigned(fSearchEngine)
      else if Action is TSearchFind then
        TSearchAction(Action).Enabled := (Text<>'') and assigned(fSearchEngine)
      else if Action is TSearchReplace then
        TSearchAction(Action).Enabled := (Text<>'') and assigned(fSearchEngine);
    end;
  end else if Action is TSearchFindNext then
  begin
    Result := Focused;
    if Result then
      TSearchAction(Action).Enabled := (Text<>'')
        and (TSearchFindNext(Action).SearchFind <> nil)
        and (TSearchFindNext(Action).SearchFind.Dialog.FindText <> '');
{$ENDIF}
  end
  else
    Result := inherited UpdateAction(Action);
end;
{$ENDIF}

procedure TCustomSynEdit.SetModified(Value: Boolean);
begin
  if Value <> fModified then begin
    fModified := Value;
    if (eoGroupUndo in Options) and (not Value) and UndoList.CanUndo then
      UndoList.AddGroupBreak;
    UndoList.InitialState := not Value;
    StatusChanged([scModified]);
  end;
end;

function TCustomSynEdit.DoOnSpecialLineColors(Line: Integer; var Foreground,
  Background: TColor): Boolean;
begin
  Result := False;
  if Assigned(fOnSpecialLineColors) then
    fOnSpecialLineColors(Self, Line, Result, Foreground, Background);
end;

procedure TCustomSynEdit.InvalidateLine(Line: Integer);
var
  rcInval: TRect;
begin
  if (not HandleAllocated) or (Line < 1) or (Line > Lines.Count) or (not Visible) then
    Exit;

  if WordWrap then
  begin
    InvalidateLines(Line, Line);
    Exit;
  end;

  if (Line >= TopLine) and (Line <= TopLine + LinesInWindow) then
  begin
    // invalidate text area of this line
    rcInval := Rect(fGutterWidth, fTextHeight * (Line - TopLine), ClientWidth, 0);
    rcInval.Bottom := rcInval.Top + fTextHeight;
{$IFDEF SYN_CLX}
    with GetClientRect do
      OffsetRect(rcInval, Left, Top);
{$ENDIF}
    if sfLinesChanging in fStateFlags then
      UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
    else
      InvalidateRect(rcInval, False);
  end;
end;

function TCustomSynEdit.GetReadOnly: Boolean;
begin
  Result := fReadOnly;
end;

procedure TCustomSynEdit.SetReadOnly(Value: Boolean);
begin
  if fReadOnly <> Value then
  begin
    fReadOnly := Value;
    StatusChanged([scReadOnly]);
  end;
end;

procedure TCustomSynEdit.FindMatchingBracket;
begin
  InternalCaretXY := GetMatchingBracket;
end;

function TCustomSynEdit.GetMatchingBracket: TBufferCoord;
begin
  Result := GetMatchingBracketEx(CaretXY);
end;

function TCustomSynEdit.GetMatchingBracketEx(const APoint: TBufferCoord): TBufferCoord;
const
  Brackets: array[0..7] of WideChar = ('(', ')', '[', ']', '{', '}', '<', '>');
var
  Line: UnicodeString;
  i, PosX, PosY, Len: Integer;
  Test, BracketInc, BracketDec: WideChar;
  NumBrackets: Integer;
  vDummy: UnicodeString;
  attr: TSynHighlighterAttributes;
  p: TBufferCoord;
  isCommentOrString: Boolean;
begin
  Result.Char := 0;
  Result.Line := 0;
  // get char at caret
  PosX := APoint.Char;
  PosY := APoint.Line;
  Line := Lines[APoint.Line - 1];
  if Length(Line) >= PosX then
  begin
    Test := Line[PosX];
    // is it one of the recognized brackets?
    for i := Low(Brackets) to High(Brackets) do
      if Test = Brackets[i] then
      begin
        // this is the bracket, get the matching one and the direction
        BracketInc := Brackets[i];
        BracketDec := Brackets[i xor 1]; // 0 -> 1, 1 -> 0, ...
        // search for the matching bracket (that is until NumBrackets = 0)
        NumBrackets := 1;
        if Odd(i) then
        begin
          repeat
            // search until start of line
            while PosX > 1 do
            begin
              Dec(PosX);
              Test := Line[PosX];
              p.Char := PosX;
              p.Line := PosY;
              if (Test = BracketInc) or (Test = BracketDec) then
              begin
                if GetHighlighterAttriAtRowCol(p, vDummy, attr) then
                  isCommentOrString := (attr = Highlighter.StringAttribute) or
                    (attr = Highlighter.CommentAttribute)
                else
                  isCommentOrString := False;
                if (Test = BracketInc) and (not isCommentOrString) then
                  Inc(NumBrackets)
                else if (Test = BracketDec) and (not isCommentOrString) then
                begin
                  Dec(NumBrackets);
                  if NumBrackets = 0 then
                  begin
                    // matching bracket found, set caret and bail out
                    Result := P;
                    exit;
                  end;
                end;
              end;
            end;
            // get previous line if possible
            if PosY = 1 then break;
            Dec(PosY);
            Line := Lines[PosY - 1];
            PosX := Length(Line) + 1;
          until False;
        end
        else begin
          repeat
            // search until end of line
            Len := Length(Line);
            while PosX < Len do
            begin
              Inc(PosX);
              Test := Line[PosX];
              p.Char := PosX;
              p.Line := PosY;
              if (Test = BracketInc) or (Test = BracketDec) then
              begin
                if GetHighlighterAttriAtRowCol(p, vDummy, attr) then
                  isCommentOrString := (attr = Highlighter.StringAttribute) or
                    (attr = Highlighter.CommentAttribute)
                else
                  isCommentOrString := False;
                if (Test = BracketInc) and (not isCommentOrString) then
                  Inc(NumBrackets)
                else if (Test = BracketDec)and (not isCommentOrString) then
                begin
                  Dec(NumBrackets);
                  if NumBrackets = 0 then
                  begin
                    // matching bracket found, set caret and bail out
                    Result := P;
                    exit;
                  end;
                end;
              end;
            end;
            // get next line if possible
            if PosY = Lines.Count then
              Break;
            Inc(PosY);
            Line := Lines[PosY - 1];
            PosX := 0;
          until False;
        end;
        // don't test the other brackets, we're done
        break;
      end;
  end;
end;

function TCustomSynEdit.GetHighlighterAttriAtRowCol(const XY: TBufferCoord;
  var Token: UnicodeString; var Attri: TSynHighlighterAttributes): Boolean;
var
  TmpType, TmpStart: Integer;
begin
  Result := GetHighlighterAttriAtRowColEx(XY, Token, TmpType, TmpStart, Attri);
end;

function TCustomSynEdit.GetHighlighterAttriAtRowColEx(const XY: TBufferCoord;
  var Token: UnicodeString; var TokenType, Start: Integer;
  var Attri: TSynHighlighterAttributes): boolean;
var
  PosX, PosY: Integer;
  Line: UnicodeString;
begin
  PosY := XY.Line - 1;
  if Assigned(Highlighter) and (PosY >= 0) and (PosY < Lines.Count) then
  begin
    Line := Lines[PosY];
    if PosY = 0 then
      Highlighter.ResetRange
    else
      Highlighter.SetRange(TSynEditStringList(Lines).Ranges[PosY - 1]);
    Highlighter.SetLine(Line, PosY);
    PosX := XY.Char;
    if (PosX > 0) and (PosX <= Length(Line)) then
      while not Highlighter.GetEol do
      begin
        Start := Highlighter.GetTokenPos + 1;
        Token := Highlighter.GetToken;
        if (PosX >= Start) and (PosX < Start + Length(Token)) then
        begin
          Attri := Highlighter.GetTokenAttribute;
          TokenType := Highlighter.GetTokenKind;
          Result := True;
          exit;
        end;
        Highlighter.Next;
      end;
  end;
  Token := '';
  Attri := nil;
  Result := False;
end;

function TCustomSynEdit.FindHookedCmdEvent(AHandlerProc: THookedCommandEvent): Integer;
var
  Entry: THookedCommandHandlerEntry;
begin
  Result := GetHookedCommandHandlersCount - 1;
  while Result >= 0 do
  begin
    Entry := THookedCommandHandlerEntry(fHookedCommandHandlers[Result]);
    if Entry.Equals(AHandlerProc) then
      break;
    Dec(Result);
  end;
end;

function TCustomSynEdit.GetHookedCommandHandlersCount: Integer;
begin
  if Assigned(fHookedCommandHandlers) then
    Result := fHookedCommandHandlers.Count
  else
    Result := 0;
end;

procedure TCustomSynEdit.RegisterCommandHandler(
  const AHandlerProc: THookedCommandEvent; AHandlerData: pointer);
begin
  if not Assigned(AHandlerProc) then
  begin
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.Create('Event handler is NIL in RegisterCommandHandler');
{$ENDIF}
    exit;
  end;
  if not Assigned(fHookedCommandHandlers) then
    fHookedCommandHandlers := TObjectList.Create;
  if FindHookedCmdEvent(AHandlerProc) = -1 then
    fHookedCommandHandlers.Add(THookedCommandHandlerEntry.Create(
      AHandlerProc, AHandlerData))
  else
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.CreateFmt('Event handler (%p, %p) already registered',
      [TMethod(AHandlerProc).Data, TMethod(AHandlerProc).Code]);
{$ENDIF}
end;

procedure TCustomSynEdit.UnregisterCommandHandler(AHandlerProc:
  THookedCommandEvent);
var
  i: Integer;
begin
  if not Assigned(AHandlerProc) then
  begin
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.Create('Event handler is NIL in UnregisterCommandHandler');
{$ENDIF}
    exit;
  end;
  i := FindHookedCmdEvent(AHandlerProc);
  if i > -1 then
    fHookedCommandHandlers.Delete(i)
  else
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.CreateFmt('Event handler (%p, %p) is not registered',
      [TMethod(AHandlerProc).Data, TMethod(AHandlerProc).Code]);
{$ENDIF}
end;

procedure TCustomSynEdit.NotifyHookedCommandHandlers(AfterProcessing: Boolean;
  var Command: TSynEditorCommand; var AChar: WideChar; Data: pointer);
var
  Handled: Boolean;
  i: Integer;
  Entry: THookedCommandHandlerEntry;
begin
  Handled := False;
  for i := 0 to GetHookedCommandHandlersCount - 1 do
  begin
    Entry := THookedCommandHandlerEntry(fHookedCommandHandlers[i]);
    // NOTE: Command should NOT be set to ecNone, because this might interfere
    // with other handlers.  Set Handled to False instead (and check its value
    // to not process the command twice).
    Entry.fEvent(Self, AfterProcessing, Handled, Command, AChar, Data,
      Entry.fData);
  end;
  if Handled then
    Command := ecNone;
end;

procedure TCustomSynEdit.DoOnClearBookmark(var Mark: TSynEditMark);
begin
  if Assigned(fOnClearMark) then
    fOnClearMark(Self, Mark);
end;

procedure TCustomSynEdit.DoOnPaintTransientEx(TransientType: TTransientType; Lock: Boolean);
var
  DoTransient: Boolean;
  i: Integer;
begin
  DoTransient:=(FPaintTransientLock=0);
  if Lock then
  begin
    if (TransientType=ttBefore) then inc(FPaintTransientLock)
    else
    begin
      dec(FPaintTransientLock);
      DoTransient:=(FPaintTransientLock=0);
    end;
  end;

  if DoTransient then
  begin
    // plugins
    if fPlugins <> nil then
      for i := 0 to fPlugins.Count - 1 do
        TSynEditPlugin(fPlugins[i]).PaintTransient(Canvas, TransientType);
    // event
    if Assigned(fOnPaintTransient) then
    begin
      Canvas.Font.Assign(Font);
      Canvas.Brush.Color := Color;
      HideCaret;
      try
        fOnPaintTransient(Self, Canvas, TransientType);
      finally
        ShowCaret;
      end;
    end;
  end;
end;

procedure TCustomSynEdit.DoOnPaintTransient(TransientType: TTransientType);
begin
  DoOnPaintTransientEx(TransientType, False);
end;

procedure TCustomSynEdit.DoOnPaint;
begin
  if Assigned(fOnPaint) then
  begin
    Canvas.Font.Assign(Font);
    Canvas.Brush.Color := Color;
    fOnPaint(Self, Canvas);
  end;
end;

procedure TCustomSynEdit.DoOnPlaceMark(var Mark: TSynEditMark);
begin
  if Assigned(fOnPlaceMark) then
    fOnPlaceMark(Self, Mark);
end;

function TCustomSynEdit.DoOnReplaceText(const ASearch, AReplace: UnicodeString;
  Line, Column: Integer): TSynReplaceAction;
begin
  Result := raCancel;
  if Assigned(fOnReplaceText) then
    fOnReplaceText(Self, ASearch, AReplace, Line, Column, Result);
end;

procedure TCustomSynEdit.DoOnStatusChange(Changes: TSynStatusChanges);
begin
  if Assigned(fOnStatusChange) then
  begin
    fOnStatusChange(Self, fStatusChanges);
    fStatusChanges := [];
  end;
end;

procedure TCustomSynEdit.UpdateModifiedStatus;
begin
  Modified := not UndoList.InitialState;
end;

procedure TCustomSynEdit.UndoRedoAdded(Sender: TObject);
begin
  UpdateModifiedStatus;

  // we have to clear the redo information, since adding undo info removes
  // the necessary context to undo earlier edit actions
  if (Sender = fUndoList) and not fUndoList.InsideRedo and
     (fUndoList.PeekItem<>nil) and (fUndoList.PeekItem.ChangeReason<>crGroupBreak) then
    fRedoList.Clear;
  if TSynEditUndoList(Sender).BlockCount = 0 then
    DoChange;
end;

function TCustomSynEdit.GetWordAtRowCol(XY: TBufferCoord): UnicodeString;
var
  Line: UnicodeString;
  Len, Stop: Integer;
begin
  Result := '';
  if (XY.Line >= 1) and (XY.Line <= Lines.Count) then
  begin
    Line := Lines[XY.Line - 1];
    Len := Length(Line);
    if Len = 0 then Exit;
    if (XY.Char >= 1) and (XY.Char <= Len + 1) and IsIdentChar(Line[XY.Char]) then
    begin
      Stop := XY.Char;
      while (Stop <= Len) and IsIdentChar(Line[Stop]) do
        Inc(Stop);
      while (XY.Char > 1) and IsIdentChar(Line[XY.Char - 1]) do
        Dec(XY.Char);
      if Stop > XY.Char then
        Result := Copy(Line, XY.Char, Stop - XY.Char);
    end;
  end;
end;

function TCustomSynEdit.BufferToDisplayPos(const p: TBufferCoord): TDisplayCoord;
// BufferToDisplayPos takes a position in the text and transforms it into
// the row and column it appears to be on the screen
var
  s: UnicodeString;
  i, L: Integer;
  x, CountOfAvgGlyphs: Integer;
begin
  Canvas.Font := Font;

  Result := TDisplayCoord(p);
  if p.Line - 1 < Lines.Count then
  begin
    s := Lines[p.Line - 1];
    l := Length(s);
    x := 0;
    for i := 1 to p.Char - 1 do begin
      if (i <= l) and (s[i] = #9) then
        inc(x, TabWidth - (x mod TabWidth))
      else if i <= l then
      begin
        CountOfAvgGlyphs := CeilOfIntDiv(fTextDrawer.TextWidth(s[i]) , fCharWidth);
        inc(x, CountOfAvgGlyphs);
      end
      else
        inc(x);
    end;
    Result.Column := x + 1;
  end;
  if WordWrap then
    Result := fWordWrapPlugin.BufferToDisplayPos(TBufferCoord(Result));
end;

function TCustomSynEdit.DisplayToBufferPos(const p: TDisplayCoord): TBufferCoord;
// DisplayToBufferPos takes a position on screen and transfrom it
// into position of text
var
  s: UnicodeString;
  i, L: Integer;
  x, CountOfAvgGlyphs: Integer;
begin
  Canvas.Font := Font;

  if WordWrap then
    Result := fWordWrapPlugin.DisplayToBufferPos(p)
  else
    Result := TBufferCoord(p);
  if Result.Line <= lines.Count then
  begin
    s := Lines[Result.Line -1];
    l := Length(s);
    x := 0;
    i := 0;

    while x < Result.Char  do
    begin
      inc(i);
      if (i <= l) and (s[i] = #9) then
        inc(x, TabWidth - (x mod TabWidth))
      else if i <= l then
      begin
        CountOfAvgGlyphs := CeilOfIntDiv(fTextDrawer.TextWidth(s[i]) , fCharWidth);
        inc(x, CountOfAvgGlyphs);
      end
      else
        inc(x);
    end;
    Result.Char := i;
  end;
end;

procedure TCustomSynEdit.DoLinesDeleted(FirstLine, Count: Integer);
var
  i: Integer;
begin
  // gutter marks
  for i := 0 to Marks.Count - 1 do
    if Marks[i].Line >= FirstLine + Count then
      Marks[i].Line := Marks[i].Line - Count
    else if Marks[i].Line > FirstLine then
      Marks[i].Line := FirstLine;

  // plugins
  if fPlugins <> nil then
    for i := 0 to fPlugins.Count - 1 do
      TSynEditPlugin(fPlugins[i]).LinesDeleted(FirstLine, Count);
end;

procedure TCustomSynEdit.DoLinesInserted(FirstLine, Count: Integer);
var
  i: Integer;
begin
  // gutter marks
  for i := 0 to Marks.Count - 1 do
    if Marks[i].Line >= FirstLine then
      Marks[i].Line := Marks[i].Line + Count;

  // plugins
  if fPlugins <> nil then
    for i := 0 to fPlugins.Count - 1 do
      TSynEditPlugin(fPlugins[i]).LinesInserted(FirstLine, Count);
end;

procedure TCustomSynEdit.PluginsAfterPaint(ACanvas: TCanvas; const AClip: TRect;
  FirstLine, LastLine: Integer);
var
  i: Integer;
begin
  if fPlugins <> nil then
    for i := 0 to fPlugins.Count - 1 do
      TSynEditPlugin(fPlugins[i]).AfterPaint(ACanvas, AClip, FirstLine, LastLine);
end;

procedure TCustomSynEdit.ProperSetLine(ALine: Integer; const ALineText: UnicodeString);
begin
  if eoTrimTrailingSpaces in Options then
    Lines[ALine] := TrimTrailingSpaces(ALineText)
  else
    Lines[ALine] := ALineText;
end;

procedure TCustomSynEdit.AddKeyUpHandler(aHandler: TKeyEvent);
begin
  fKbdHandler.AddKeyUpHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveKeyUpHandler(aHandler: TKeyEvent);
begin
  fKbdHandler.RemoveKeyUpHandler(aHandler);
end;

procedure TCustomSynEdit.AddKeyDownHandler(aHandler: TKeyEvent);
begin
  fKbdHandler.AddKeyDownHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveKeyDownHandler(aHandler: TKeyEvent);
begin
  fKbdHandler.RemoveKeyDownHandler(aHandler);
end;

procedure TCustomSynEdit.AddKeyPressHandler(aHandler: TKeyPressWEvent);
begin
  fKbdHandler.AddKeyPressHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveKeyPressHandler(aHandler: TKeyPressWEvent);
begin
  fKbdHandler.RemoveKeyPressHandler(aHandler);
end;

procedure TCustomSynEdit.AddFocusControl(aControl: TWinControl);
begin
  fFocusList.Add(aControl);
end;

procedure TCustomSynEdit.RemoveFocusControl(aControl: TWinControl);
begin
  fFocusList.Remove(aControl);
end;

function TCustomSynEdit.IsIdentChar(AChar: WideChar): Boolean;
begin
  if Assigned(Highlighter) then
    Result := Highlighter.IsIdentChar(AChar)
  else
    Result := AChar >= #33;

  if Assigned(Highlighter) then
    Result := Result or CharInSet(AChar, Highlighter.AdditionalIdentChars)
  else
    Result := Result or CharInSet(AChar, Self.AdditionalIdentChars);

  Result := Result and not IsWordBreakChar(AChar);
end;

function TCustomSynEdit.IsWhiteChar(AChar: WideChar): Boolean;
begin
  if Assigned(Highlighter) then
    Result := Highlighter.IsWhiteChar(AChar)
  else
    case AChar of
    #0..#32:
      Result := True;
    else
      Result := not (IsIdentChar(AChar) or IsWordBreakChar(AChar))
    end
end;

function TCustomSynEdit.IsWordBreakChar(AChar: WideChar): Boolean;
begin
  if Assigned(Highlighter) then
    Result := Highlighter.IsWordBreakChar(AChar)
  else
    case AChar of
      #0..#32, '.', ',', ';', ':', '"', '''', '�', '`', '�', '^', '!', '?', '&',
      '$', '@', '�', '%', '#', '~', '[', ']', '(', ')', '{', '}', '<', '>',
      '-', '=', '+', '*', '/', '\', '|':
        Result := True;
      else
        Result := False;
    end;

  if Assigned(Highlighter) then
  begin
    Result := Result or CharInSet(AChar, Highlighter.AdditionalWordBreakChars);
    Result := Result and not CharInSet(AChar, Highlighter.AdditionalIdentChars);
  end
  else
  begin
    Result := Result or CharInSet(AChar, Self.AdditionalWordBreakChars);
    Result := Result and not CharInSet(AChar, Self.AdditionalIdentChars);
  end;
end;

procedure TCustomSynEdit.SetSearchEngine(Value: TSynEditSearchCustom);
begin
  if (fSearchEngine <> Value) then
  begin
    fSearchEngine := Value;
    if Assigned(fSearchEngine) then
      fSearchEngine.FreeNotification(Self);
  end;
end;

function TCustomSynEdit.NextWordPos: TBufferCoord;
begin
  Result := NextWordPosEx(CaretXY);
end;

function TCustomSynEdit.WordStart: TBufferCoord;
begin
  Result := WordStartEx(CaretXY);
end;

function TCustomSynEdit.WordEnd: TBufferCoord;
begin
  Result := WordEndEx(CaretXY);
end;

function TCustomSynEdit.PrevWordPos: TBufferCoord;
begin
  Result := PrevWordPosEx(CaretXY);
end;

function TCustomSynEdit.GetPositionOfMouse(out aPos: TBufferCoord): Boolean;
// Get XY caret position of mouse. Returns False if point is outside the
// region of the SynEdit control.
var
  Point: TPoint;
begin
  GetCursorPos(Point);                    // mouse position (on screen)
  Point := Self.ScreenToClient(Point);    // convert to SynEdit coordinates
  { Make sure it fits within the SynEdit bounds }
  if (Point.X < 0) or (Point.Y < 0) or (Point.X > Self.Width) or (Point.Y> Self.Height) then
  begin
    Result := False;
    Exit;
  end;

  { inside the editor, get the word under the mouse pointer }
  aPos := DisplayToBufferPos(PixelsToRowColumn(Point.X, Point.Y));
  Result := True;
end;

function TCustomSynEdit.GetWordAtMouse: UnicodeString;
var
  Point: TBufferCoord;
begin
  { Return the word under the mouse }
  if GetPositionOfMouse(Point) then        // if point is valid
    Result := Self.GetWordAtRowCol(Point); // return the point at the mouse position
end;

function TCustomSynEdit.CharIndexToRowCol(Index: Integer): TBufferCoord;
{ Index is 0-based; Result.x and Result.y are 1-based }
var
  x, y, Chars: Integer;
begin
  x := 0;
  y := 0;
  Chars := 0;
  while y < Lines.Count do
  begin
    x := Length(Lines[y]);
    if Chars + x + 2 > Index then
    begin
      x := Index - Chars;
      break;
    end;
    Inc(Chars, x + 2);
    x := 0;
    Inc(y);
  end;
  Result.Char := x + 1;
  Result.Line := y + 1;
end;

function TCustomSynEdit.RowColToCharIndex(RowCol: TBufferCoord): Integer;
{ Row and Col are 1-based; Result is 0-based }
var
  synEditStringList : TSynEditStringList;
begin
  RowCol.Line := Min(Lines.Count, RowCol.Line) - 1;
  synEditStringList := (FLines as TSynEditStringList);
  // CharIndexToRowCol assumes a line break size of two
  Result :=  synEditStringList.LineCharIndex(RowCol.Line)
           + RowCol.Line * 2 + (RowCol.Char -1);
end;

procedure TCustomSynEdit.Clear;
{ just to attain interface compatibility with TMemo }
begin
  ClearAll;
end;

function TCustomSynEdit.GetSelLength: Integer;
begin
  if SelAvail then
    Result := RowColToCharIndex(BlockEnd) - RowColToCharIndex(BlockBegin)
  else
    Result := 0;
end;

procedure TCustomSynEdit.SetSelLength(const Value: Integer);
var
  iNewCharIndex: Integer;
  iNewBegin: TBufferCoord;
  iNewEnd: TBufferCoord;
begin
  iNewCharIndex := RowColToCharIndex(BlockBegin) + Value;
  if (Value >= 0) or (iNewCharIndex < 0) then
  begin
    if iNewCharIndex < 0 then
    begin
      iNewEnd.Char := Length(Lines[Lines.Count - 1]) + 1;
      iNewEnd.Line := Lines.Count;
    end
    else
      iNewEnd := CharIndexToRowCol(iNewCharIndex);
    SetCaretAndSelection(iNewEnd, BlockBegin, iNewEnd);
  end
  else begin
    iNewBegin := CharIndexToRowCol(iNewCharIndex);
    SetCaretAndSelection(iNewBegin, iNewBegin, BlockBegin);
  end;
end;

procedure TCustomSynEdit.DefineProperties(Filer: TFiler);

{$IFDEF SYN_COMPILER_6_UP}
  function CollectionsEqual(C1, C2: TCollection): Boolean;
  begin
    Result := Classes.CollectionsEqual(C1, C2, nil, nil);
  end;
{$ENDIF}

  function HasKeyData: Boolean;
  var
    iDefKeys: TSynEditKeyStrokes;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := not CollectionsEqual(Keystrokes,
        TCustomSynEdit(Filer.Ancestor).Keystrokes);
    end
    else begin
      iDefKeys := TSynEditKeyStrokes.Create(nil);
      try
        iDefKeys.ResetDefaults;
        Result := not CollectionsEqual(Keystrokes, iDefKeys);
      finally
        iDefKeys.Free;
      end;
    end;
  end;

var
  iSaveKeyData: Boolean;
begin
  inherited;
{$IFNDEF UNICODE}
  UnicodeDefineProperties(Filer, Self);
{$ENDIF}
  iSaveKeyData := HasKeyData;
  Filer.DefineProperty('RemovedKeystrokes', ReadRemovedKeystrokes,
    WriteRemovedKeystrokes, iSaveKeyData);
  Filer.DefineProperty('AddedKeystrokes', ReadAddedKeystrokes, WriteAddedKeystrokes,
    iSaveKeyData);
end;

procedure TCustomSynEdit.DoChange;
begin
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

procedure TCustomSynEdit.ReadAddedKeystrokes(Reader: TReader);
var
  iAddKeys: TSynEditKeyStrokes;
  cKey: Integer;
begin
  if Reader.NextValue = vaCollection then
    Reader.ReadValue
  else
    Exit;
  iAddKeys := TSynEditKeyStrokes.Create(Self);
  try
    Reader.ReadCollection(iAddKeys);
    for cKey := 0 to iAddKeys.Count -1 do
      Keystrokes.Add.Assign(iAddKeys[cKey]);
  finally
    iAddKeys.Free;
  end;
end;

procedure TCustomSynEdit.ReadRemovedKeystrokes(Reader: TReader);
var
  iDelKeys: TSynEditKeyStrokes;
  cKey: Integer;
  iKey: TSynEditKeyStroke;
  iToDelete: Integer;
begin
  if Reader.NextValue = vaCollection then
    Reader.ReadValue
  else
    Exit;
  iDelKeys := TSynEditKeyStrokes.Create(nil);
  try
    Reader.ReadCollection(iDelKeys);
    for cKey := 0 to iDelKeys.Count -1 do
    begin
      iKey := iDelKeys[cKey];
      iToDelete := Keystrokes.FindShortcut2(iKey.ShortCut, iKey.ShortCut2);
      if (iToDelete >= 0) and (Keystrokes[iToDelete].Command = iKey.Command) then
        Keystrokes[iToDelete].Free;
    end;
  finally
    iDelKeys.Free;
  end;
end;

procedure TCustomSynEdit.WriteAddedKeystrokes(Writer: TWriter);
var
  iDefaultKeys: TSynEditKeyStrokes;
  iAddedKeys: TSynEditKeyStrokes;
  cKey: Integer;
  iKey: TSynEditKeyStroke;
  iDelIndex: Integer;
begin
  iDefaultKeys := TSynEditKeyStrokes.Create(nil);
  try
    if Writer.Ancestor <> nil then
      iDefaultKeys.Assign(TSynEdit(Writer.Ancestor).Keystrokes)
    else
      iDefaultKeys.ResetDefaults;
    iAddedKeys := TSynEditKeyStrokes.Create(nil);
    try
      for cKey := 0 to Keystrokes.Count -1 do
      begin
        iKey := Keystrokes[cKey];
        iDelIndex := iDefaultKeys.FindShortcut2(iKey.ShortCut, iKey.ShortCut2);
        //if it's not a default keystroke, add it
        if (iDelIndex < 0) or (iDefaultKeys[iDelIndex].Command <> iKey.Command) then
          iAddedKeys.Add.Assign(iKey);
      end;
      Writer.WriteCollection(iAddedKeys);
    finally
      iAddedKeys.Free;
    end;
  finally
    iDefaultKeys.Free;
  end;
end;

procedure TCustomSynEdit.WriteRemovedKeystrokes(Writer: TWriter);
var
  iRemovedKeys: TSynEditKeyStrokes;
  cKey: Integer;
  iKey: TSynEditKeyStroke;
  iFoundAt: Integer;
begin
  iRemovedKeys := TSynEditKeyStrokes.Create(nil);
  try
    if Writer.Ancestor <> nil then
      iRemovedKeys.Assign(TSynEdit(Writer.Ancestor).Keystrokes)
    else
      iRemovedKeys.ResetDefaults;
    cKey := 0;
    while cKey < iRemovedKeys.Count do
    begin
      iKey := iRemovedKeys[cKey];
      iFoundAt := Keystrokes.FindShortcut2(iKey.ShortCut, iKey.ShortCut2);
      if (iFoundAt >= 0) and (Keystrokes[iFoundAt].Command = iKey.Command) then
        iKey.Free //if exists in Keystrokes, then shouldn't be in "removed" list
      else
        Inc(cKey);
    end;
    Writer.WriteCollection(iRemovedKeys);
  finally
    iRemovedKeys.Free;
  end;
end;

procedure TCustomSynEdit.AddMouseDownHandler(aHandler: TMouseEvent);
begin
  fKbdHandler.AddMouseDownHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveMouseDownHandler(aHandler: TMouseEvent);
begin
  fKbdHandler.RemoveMouseDownHandler(aHandler);
end;

procedure TCustomSynEdit.AddMouseUpHandler(aHandler: TMouseEvent);
begin
  fKbdHandler.AddMouseUpHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveMouseUpHandler(aHandler: TMouseEvent);
begin
  fKbdHandler.RemoveMouseUpHandler(aHandler);
end;

procedure TCustomSynEdit.AddMouseCursorHandler(aHandler: TMouseCursorEvent);
begin
  fKbdHandler.AddMouseCursorHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveMouseCursorHandler(aHandler: TMouseCursorEvent);
begin
  fKbdHandler.RemoveMouseCursorHandler(aHandler);
end;

{$IFDEF SYN_COMPILER_6_UP}
procedure TCustomSynEdit.DoSearchFindFirstExecute(Action: TSearchFindFirst);
begin
  OnFindBeforeSearch := Action.Dialog.OnFind;
  OnCloseBeforeSearch := Action.Dialog.OnClose;
  SelStartBeforeSearch := SelStart; SelLengthBeforeSearch := SelLength;

  Action.Dialog.OnFind := FindDialogFindFirst;
  Action.Dialog.OnClose := FindDialogClose;
  Action.Dialog.Execute();
end;

procedure TCustomSynEdit.DoSearchFindExecute(Action: TSearchFind);
begin
  OnFindBeforeSearch := Action.Dialog.OnFind;
  OnCloseBeforeSearch := Action.Dialog.OnClose;

  Action.Dialog.OnFind := FindDialogFind;
  Action.Dialog.OnClose := FindDialogClose;
  Action.Dialog.Execute();
end;

procedure TCustomSynEdit.DoSearchReplaceExecute(Action: TSearchReplace);
begin
  OnFindBeforeSearch := Action.Dialog.OnFind;
  OnReplaceBeforeSearch := Action.Dialog.OnReplace;
  OnCloseBeforeSearch := Action.Dialog.OnClose;

  Action.Dialog.OnFind := FindDialogFind;
  Action.Dialog.OnReplace := FindDialogFind;
  Action.Dialog.OnClose := FindDialogClose;
  Action.Dialog.Execute();
end;

procedure TCustomSynEdit.DoSearchFindNextExecute(Action: TSearchFindNext);
begin
  SearchByFindDialog(Action.SearchFind.Dialog);
end;

procedure TCustomSynEdit.FindDialogFindFirst(Sender: TObject);
begin
  TFindDialog(Sender).CloseDialog;

  if (SelStart = SelStartBeforeSearch) and (SelLength = SelLengthBeforeSearch) then
  begin
    SelStart := 0;
    SelLength := 0;
  end;

  if Sender is TFindDialog then
    if not SearchByFindDialog(TFindDialog(Sender)) and (SelStart = 0) and (SelLength = 0) then
    begin
      SelStart := SelStartBeforeSearch;
      SelLength := SelLengthBeforeSearch;
    end;
end;

procedure TCustomSynEdit.FindDialogFind(Sender: TObject);
begin
  if Sender is TFindDialog then
    SearchByFindDialog(TFindDialog(Sender));
end;

function TCustomSynEdit.SearchByFindDialog(FindDialog: TFindDialog) : bool;
var
  Options :TSynSearchOptions;
  ReplaceText, MessageText :String;
  OldSelStart, OldSelLength: integer;
begin
  if (frReplaceAll in FindDialog.Options) then Options := [ssoReplaceAll]
  else if (frReplace in FindDialog.Options) then Options := [ssoReplace]
  else Options := [ssoSelectedOnly];

  if (frMatchCase in FindDialog.Options) then Options := Options + [ssoMatchCase];
  if (frWholeWord in FindDialog.Options) then Options := Options + [ssoWholeWord];
  if (not (frDown in FindDialog.Options)) then Options := Options + [ssoBackwards];

  if (ssoSelectedOnly in Options)
    then ReplaceText := ''
    else ReplaceText := TReplaceDialog(FindDialog).ReplaceText;

  OldSelStart := SelStart; OldSelLength := SelLength;
  if (UpperCase(SelText) = UpperCase(FindDialog.FindText)) and not (frReplace in FindDialog.Options) then
    SelStart := SelStart + SelLength
  else
    SelLength := 0;

  Result := SearchReplace(FindDialog.FindText, ReplaceText, Options) > 0;
  if not Result then
  begin
    SelStart := OldSelStart; SelLength := OldSelLength;
    if Assigned(OnSearchNotFound) then
      OnSearchNotFound(self, FindDialog.FindText)
    else
    begin
      MessageText := Format(STextNotFound, [FindDialog.FindText]);
      ShowMessage(MessageText);
    end;
  end
  else if (frReplace in FindDialog.Options) then
  begin
    SelStart := SelStart - Length(FindDialog.FindText) - 1;
    SelLength := Length(FindDialog.FindText) + 1;
  end;
end;

procedure TCustomSynEdit.FindDialogClose(Sender: TObject);
begin
  TFindDialog(Sender).OnFind := OnFindBeforeSearch;
  if Sender is TReplaceDialog then
    TReplaceDialog(Sender).OnReplace := OnReplaceBeforeSearch;
  TFindDialog(Sender).OnClose := OnCloseBeforeSearch;
end;
{$ENDIF}

function TCustomSynEdit.GetWordWrap: Boolean;
begin
  Result := fWordWrapPlugin <> nil;
end;

procedure TCustomSynEdit.SetWordWrap(const Value: Boolean);
var
  vTempBlockBegin, vTempBlockEnd : TBufferCoord;
  vOldTopLine: Integer;
  vShowCaret: Boolean;
begin
  if WordWrap <> Value then
  begin
    Invalidate; // better Invalidate before changing LeftChar and TopLine
    vShowCaret := CaretInView;
    vOldTopLine := RowToLine(TopLine);
    if Value then
    begin
      fWordWrapPlugin := TSynWordWrapPlugin.Create(Self);
      LeftChar := 1;
    end
    else
      fWordWrapPlugin := nil;
    TopLine := LineToRow(vOldTopLine);
    UpdateScrollBars;

    // constrain caret position to MaxScrollWidth if eoScrollPastEol is enabled
    if (eoScrollPastEol in Options) then
    begin
      InternalCaretXY := CaretXY;
      vTempBlockBegin := BlockBegin;
      vTempBlockEnd := BlockEnd;
      SetBlockBegin(vTempBlockBegin);
      SetBlockEnd(vTempBlockEnd);
    end;
    if vShowCaret then
      EnsureCursorPosVisible;
  end;
end;

function TCustomSynEdit.GetDisplayLineCount: Integer;
begin
  if fWordWrapPlugin = nil then
    Result := Lines.Count
  else if Lines.Count = 0 then
    Result := 0
  else begin
    Result := fWordWrapPlugin.RowCount;
  end;
end;

function TCustomSynEdit.LineToRow(aLine: Integer): Integer;
var
  vBufferPos: TBufferCoord;
begin
  if not WordWrap then
    Result := aLine
  else begin
    vBufferPos.Char := 1;
    vBufferPos.Line := aLine;
    Result := BufferToDisplayPos(vBufferPos).Row;
  end;
end;

function TCustomSynEdit.RowToLine(aRow: Integer): Integer;
var
  vDisplayPos: TDisplayCoord;
begin
  if not WordWrap then
    Result := aRow
  else begin
    vDisplayPos.Column := 1;
    vDisplayPos.Row := aRow;
    Result := DisplayToBufferPos(vDisplayPos).Line;
  end;
end;

procedure TCustomSynEdit.SetInternalDisplayXY(const aPos: TDisplayCoord);
begin
  IncPaintLock;
  InternalCaretXY := DisplayToBufferPos(aPos);
  fCaretAtEOL := WordWrap and (aPos.Row <= fWordWrapPlugin.RowCount) and
    (aPos.Column > fWordWrapPlugin.GetRowLength(aPos.Row)) and
    (DisplayY <> aPos.Row);
  DecPaintLock;
  UpdateLastCaretX;
end;

procedure TCustomSynEdit.SetWantReturns(Value: Boolean);
begin
  fWantReturns := Value;
  {$IFDEF SYN_CLX}
  if fWantReturns then
    InputKeys := InputKeys + [ikReturns]
  else
    InputKeys := InputKeys - [ikReturns];
  {$ENDIF}
end;

procedure TCustomSynEdit.SetWantTabs(Value: Boolean);
begin
  fWantTabs := Value;
  {$IFDEF SYN_CLX}
  if fWantTabs then
    InputKeys := InputKeys + [ikTabs]
  else
    InputKeys := InputKeys - [ikTabs];
  {$ENDIF}
end;

procedure TCustomSynEdit.SetWordWrapGlyph(const Value: TSynGlyph);
begin
  fWordWrapGlyph.Assign(Value);
end;

procedure TCustomSynEdit.WordWrapGlyphChange(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    InvalidateGutter;
end;


{ TSynEditMark }

function TSynEditMark.GetEdit: TCustomSynEdit;
begin
  if FEdit <> nil then try
    if FEdit.Marks.IndexOf(self) = -1 then
      FEdit := nil;
  except
    FEdit := nil;
  end;
  Result := FEdit;
end;

function TSynEditMark.GetIsBookmark: Boolean;
begin
  Result := (fBookmarkNum >= 0);
end;

procedure TSynEditMark.SetChar(const Value: Integer);
begin
  FChar := Value;
end;

procedure TSynEditMark.SetImage(const Value: Integer);
begin
  FImage := Value;
  if fVisible and Assigned(fEdit) then
    fEdit.InvalidateGutterLines(fLine, fLine);
end;

procedure TSynEditMark.SetInternalImage(const Value: Boolean);
begin
  fInternalImage := Value;
  if fVisible and Assigned(fEdit) then
    fEdit.InvalidateGutterLines(fLine, fLine);
end;

procedure TSynEditMark.SetLine(const Value: Integer);
begin
  if fVisible and Assigned(fEdit) then
  begin
    if fLine > 0 then
      fEdit.InvalidateGutterLines(fLine, fLine);
    fLine := Value;
    fEdit.InvalidateGutterLines(fLine, fLine);
  end
  else
    fLine := Value;
end;

procedure TSynEditMark.SetVisible(const Value: Boolean);
begin
  if fVisible <> Value then
  begin
    fVisible := Value;
    if Assigned(fEdit) then
      fEdit.InvalidateGutterLines(fLine, fLine);
  end;
end;

constructor TSynEditMark.Create(AOwner: TCustomSynEdit);
begin
  inherited Create;
  fBookmarkNum := -1;
  fEdit := AOwner;
end;

{ TSynEditMarkList }

procedure TSynEditMarkList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TSynEditMarkList.GetItem(Index: Integer): TSynEditMark;
begin
  Result := TSynEditMark(inherited GetItem(Index));
end;

procedure TSynEditMarkList.SetItem(Index: Integer; Item: TSynEditMark);
begin
  inherited SetItem(Index, Item);
end;

constructor TSynEditMarkList.Create(AOwner: TCustomSynEdit);
begin
  inherited Create;
  fEdit := AOwner;
end;

function TSynEditMarkList.First: TSynEditMark;
begin
  Result := TSynEditMark(inherited First);
end;

function TSynEditMarkList.Last: TSynEditMark;
begin
  result := TSynEditMark(inherited Last);
end;

function TSynEditMarkList.Extract(Item: TSynEditMark): TSynEditMark;
begin
  Result := TSynEditMark(inherited Extract(Item));
end;

procedure TSynEditMarkList.ClearLine(Line: Integer);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if not Items[i].IsBookmark and (Items[i].Line = Line) then
      Delete(i);
end;

procedure TSynEditMarkList.GetMarksForLine(line: Integer; var marks: TSynEditMarks);
//Returns up to maxMarks book/gutter marks for a chosen line.
var
  cnt: Integer;
  i: Integer;
begin
  FillChar(marks, SizeOf(marks), 0);
  cnt := 0;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Line = line then
    begin
      Inc(cnt);
      marks[cnt] := Items[i];
      if cnt = MAX_MARKS then break;
    end;
  end;
end;

procedure TSynEditMarkList.Place(mark: TSynEditMark);
begin
  if assigned(fEdit) then
    if Assigned(fEdit.OnPlaceBookmark) then
      fEdit.OnPlaceBookmark(fEdit, mark);
  if assigned(mark) then
    Add(mark);
end;

{ TSynEditPlugin }

constructor TSynEditPlugin.Create(AOwner: TCustomSynEdit);
begin
  inherited Create;
  if AOwner <> nil then
  begin
    fOwner := AOwner;
    if fOwner.fPlugins = nil then
      fOwner.fPlugins := TObjectList.Create;
    fOwner.fPlugins.Add(Self);
  end;
end;

destructor TSynEditPlugin.Destroy;
begin
  if fOwner <> nil then
    fOwner.fPlugins.Extract(Self); // we are being destroyed, fOwner should not free us
  inherited Destroy;
end;

procedure TSynEditPlugin.AfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: Integer);
begin
  // nothing
end;

procedure TSynEditPlugin.PaintTransient(ACanvas: TCanvas; ATransientType: TTransientType);
begin
  // nothing
end;

procedure TSynEditPlugin.LinesInserted(FirstLine, Count: Integer);
begin
  // nothing
end;

procedure TSynEditPlugin.LinesDeleted(FirstLine, Count: Integer);
begin
  // nothing
end;

{$IFNDEF UNICODE}
{$IFNDEF SYN_CLX}
var
  GetMsgHook: HHOOK;

function GetMsgProc(Code: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
{$IFNDEF SYN_COMPILER_9_UP}
  WndProc: Pointer;
{$ENDIF}
  WinCtrl: TWinControl;
begin
  WinCtrl := TCustomSynEdit(FindControl(PMsg(lParam)^.hWnd));
  if WinCtrl is TCustomSynEdit then
  begin
    TCustomSynEdit(WinCtrl).FWindowProducedMessage := True;

{$IFNDEF SYN_COMPILER_9_UP}
    if Code = HC_ACTION then
    begin
      with PMsg(lParam)^ do
        case message of
          WM_CHAR:
            begin
              if wParam > Ord(High(AnsiChar)) then
                if IsWindowUnicode(hWnd) then
                begin
                  WndProc := Pointer(GetWindowLong(hWnd, GWL_WNDPROC));
                  CallWindowProcW(WndProc, hWnd, WM_CHAR, wParam, lParam);
                  Message := WM_NULL;
                end;
            end;
        end;
    end;
{$ENDIF}

  end;

  Result := CallNextHookEx(GetMsgHook, Code, wParam, lParam);
end;
{$ENDIF}
{$ENDIF}

initialization
{$IFNDEF SYN_CLX}
{$IFNDEF UNICODE}
  if Win32PlatformIsUnicode and not (csDesigning in Application.ComponentState) then
  begin
    // Hooking GetMessage/PeekMessage-calls is necessary as the use of
    // PeekMessageA in TApplication.ProcessMessage mutilates Unicode-messages.
    GetMsgHook := SetWindowsHookExW(WH_GETMESSAGE, GetMsgProc, 0,
      GetCurrentThreadId);
  end
  else
    GetMsgHook := 0;
{$ENDIF}
  SynEditClipboardFormat := RegisterClipboardFormat(SYNEDIT_CLIPBOARD_FORMAT);
{$ENDIF}

finalization
{$IFNDEF SYN_CLX}
{$IFNDEF UNICODE}
  if Win32PlatformIsUnicode and (GetMsgHook <> 0) then
    UnhookWindowsHookEx(GetMsgHook);
{$ENDIF}
{$ENDIF}

end.
