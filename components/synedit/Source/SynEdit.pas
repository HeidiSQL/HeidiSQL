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
//todo: remove FShowSpecChar variable
//todo: remove the several Undo block types?

unit SynEdit;

{$I SynEdit.inc}

interface

uses
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
  Types, UITypes,
  {$ENDIF}
  Imm,
  SynUnicode,
  SynTextDrawer,
  SynEditTypes,
  SynEditKeyConst,
  SynEditMiscProcs,
  SynEditMiscClasses,
  SynEditTextBuffer,
  SynEditKeyCmds,
  SynEditHighlighter,
  SynEditKbdHandler,
{$IFDEF SYN_CodeFolding}
  SynEditCodeFolding,
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
{$IFNDEF SYN_COMPILER_7_UP}
  // not defined in all Delphi versions
  WS_EX_COMPOSITED = $02000000;
{$ENDIF}

   // maximum scroll range
  MAX_SCROLL = 32767;

  // Max number of book/gutter marks returned from GetEditMarksForLine - that
  // really should be enough.
  MAX_MARKS = 16;

  SYNEDIT_CLIPBOARD_FORMAT = 'SynEdit Control Block Type';

  // Reconversion string.
  IMR_COMPOSITIONWINDOW         =  $0001;
  IMR_CANDIDATEWINDOW           =  $0002;
  IMR_COMPOSITIONFONT           =  $0003;
  IMR_RECONVERTSTRING           =  $0004;
  IMR_CONFIRMRECONVERTSTRING    =  $0005;
  IMR_QUERYCHARPOSITION         =  $0006;
  IMR_DOCUMENTFEED              =  $0007;

  SCS_SETSTR                    =  GCS_COMPREADSTR or GCS_COMPSTR;
  SCS_CHANGEATTR                =  GCS_COMPREADATTR or GCS_COMPATTR;
  SCS_CHANGECLAUSE              =  GCS_COMPREADCLAUSE or GCS_COMPCLAUSE;
  SCS_SETRECONVERTSTRING        =  $00010000;
  SCS_QUERYRECONVERTSTRING      =  $00020000;

var
  SynEditClipboardFormat: UINT;

type
  TBufferCoord = SynEditTypes.TBufferCoord;
  TDisplayCoord = SynEditTypes.TDisplayCoord;

  TSynBorderStyle = TBorderStyle;

  TSynReplaceAction = (raCancel, raSkip, raReplace, raReplaceAll);

  ESynEditError = class(ESynError);

  TDropFilesEvent = procedure(Sender: TObject; X, Y: Integer; AFiles: TUnicodeStrings)
    of object;

  THookedCommandEvent = procedure(Sender: TObject; AfterProcessing: Boolean;
    var Handled: Boolean; var Command: TSynEditorCommand; var AChar: WideChar;
    Data, HandlerData: Pointer) of object;

  TPaintEvent = procedure(Sender: TObject; ACanvas: TCanvas) of object;

  TProcessCommandEvent = procedure(Sender: TObject;
    var Command: TSynEditorCommand; var AChar: WideChar; Data: Pointer) of object;

  TReplaceTextEvent = procedure(Sender: TObject; const ASearch, AReplace:
    UnicodeString; Line, Column: Integer; var Action: TSynReplaceAction) of object;

  TSpecialLineColorsEvent = procedure(Sender: TObject; Line: Integer;
    var Special: Boolean; var FG, BG: TColor) of object;
  TSpecialTokenAttributesEvent = procedure(Sender: TObject; ALine, APos: Integer; const AToken: string;
    var ASpecial: Boolean; var FG, BG: TColor; var AStyle: TFontStyles) of object;

  TTransientType = (ttBefore, ttAfter);
  TPaintTransient = procedure(Sender: TObject; Canvas: TCanvas;
    TransientType: TTransientType) of object;

  TScrollEvent = procedure(Sender: TObject; ScrollBar: TScrollBarKind) of object;

  TGutterGetTextEvent = procedure(Sender: TObject; aLine: Integer;
    var aText: UnicodeString) of object;

  TGutterPaintEvent = procedure(Sender: TObject; aLine: Integer;
    X, Y: Integer) of object;

  TSynEditCaretType = (ctVerticalLine, ctHorizontalLine, ctHalfBlock, ctBlock, ctVerticalLine2);

  TSynStateFlag = (sfCaretChanged, sfScrollbarChanged, sfLinesChanging,
    sfIgnoreNextChar, sfCaretVisible, sfDblClicked, sfPossibleGutterClick,
    sfWaitForDragging, sfInsideRedo, sfGutterDragging, sfMouseCaptured);

  TSynStateFlags = set of TSynStateFlag;

  TScrollHintFormat = (shfTopLineOnly, shfTopToBottom);

  TSynHintMode = (shmDefault, shmToken);

  TGetTokenHintEvent = procedure(Sender: TObject; Coords: TBufferCoord; const Token: string;
    TokenType: Integer; Attri: TSynHighlighterAttributes; var HintText: string) of object;

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

type
// use scAll to update a statusbar when another TCustomSynEdit got the focus
  TSynStatusChange = (scAll, scCaretX, scCaretY, scLeftChar, scTopLine,
    scInsertMode, scModified, scSelection, scReadOnly);
  TSynStatusChanges = set of TSynStatusChange;

  TContextHelpEvent = procedure(Sender: TObject; Word: UnicodeString)
    of object;

  TStatusChangeEvent = procedure(Sender: TObject; Changes: TSynStatusChanges)
    of object;

  TMouseCursorEvent = procedure(Sender: TObject; const aLineCharPos: TBufferCoord;
    var aCursor: TCursor) of object;

{$IFDEF SYN_CodeFolding}
  TScanForFoldRangesEvent = procedure(Sender: TObject;
    FoldRanges: TSynFoldRanges; LinesToScan: TStrings;
    FromLine : Integer; ToLine : Integer) of object;
{$ENDIF}

  TCustomSynEdit = class;

  TSynEditMark = class
  protected
    FLine, FChar, FImage: Integer;
    FEdit: TCustomSynEdit;
    FVisible: Boolean;
    FInternalImage: Boolean;
    FBookmarkNum: Integer;
    function GetEdit: TCustomSynEdit; virtual;
    procedure SetChar(const Value: Integer); virtual;
    procedure SetImage(const Value: Integer); virtual;
    procedure SetLine(const Value: Integer); virtual;
    procedure SetVisible(const Value: Boolean);
    procedure SetInternalImage(const Value: Boolean);
    function GetIsBookmark: Boolean;
  public
    constructor Create(AOwner: TCustomSynEdit);
    property Line: Integer read FLine write SetLine;
    property Char: Integer read FChar write SetChar;
    property Edit: TCustomSynEdit read FEdit;
    property ImageIndex: Integer read FImage write SetImage;
    property BookmarkNumber: Integer read FBookmarkNum write FBookmarkNum;
    property Visible: Boolean read FVisible write SetVisible;
    property InternalImage: Boolean read FInternalImage write SetInternalImage;
    property IsBookmark: Boolean read GetIsBookmark;
  end;

  TPlaceMarkEvent = procedure(Sender: TObject; var Mark: TSynEditMark)
    of object;

  TSynEditMarks = array[1..MAX_MARKS] of TSynEditMark;

  { A list of mark objects. Each object cause a litle picture to be drawn in the gutter. }
  TSynEditMarkList = class(TObjectList)            // It makes more sence to derive from TObjectList,
  protected                                        // as it automatically frees its members
    FEdit: TCustomSynEdit;
    FOnChange: TNotifyEvent;
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
    property Edit: TCustomSynEdit read FEdit;
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
    FOwner: TCustomSynEdit;
  protected
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: Integer); virtual;
    procedure PaintTransient(ACanvas: TCanvas; ATransientType: TTransientType); virtual;
    procedure LinesInserted(FirstLine, Count: Integer); virtual;
    procedure LinesDeleted(FirstLine, Count: Integer); virtual;
  protected
    property Editor: TCustomSynEdit read FOwner;
  public
    constructor Create(AOwner: TCustomSynEdit);
    destructor Destroy; override;
  end;

{$IFDEF SYN_COMPILER_6_UP}
  TCustomSynEditSearchNotFoundEvent = procedure(Sender: TObject;
    FindText: UnicodeString) of object;
{$ENDIF}

  // Reconversion string.
  PReconvertString = ^TReconvertString;
  TReconvertString = record
    dwSize: DWord;
    dwVersion: DWord;
    dwStrLen: DWord;
    dwStrOffset: DWord;
    dwCompStrLen: DWord;
    dwCompStrOffset: DWord;
    dwTargetStrLen: DWord;
    dwTargetStrOffset: DWord;
  end;


  TCustomSynEdit = class(TCustomControl)
  private
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
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
    procedure WMImeRequest(var Message: TMessage); message WM_IME_REQUEST;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMUndo(var Msg: TMessage); message WM_UNDO;
    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
{$IFNDEF SYN_COMPILER_6_UP}
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
{$ENDIF}
  private
{$IFDEF SYN_CodeFolding}
    fUseCodeFolding : Boolean;
    fCodeFolding: TSynCodeFolding;
    fAllFoldRanges: TSynFoldRanges;
{$ENDIF}
    FAlwaysShowCaret: Boolean;
    FBlockBegin: TBufferCoord;
    FBlockEnd: TBufferCoord;
    FCaretX: Integer;
    FLastCaretX: Integer;
    FCaretY: Integer;
    FCharsInWindow: Integer;
    FCharWidth: Integer;
    FFontDummy: TFont;
    FFontSmoothing: TSynFontSmoothMethod;
    FHintMode: TSynHintMode;
    FInserting: Boolean;
    FLines: TUnicodeStrings;
    FOrigLines: TUnicodeStrings;
    FOrigUndoList: TSynEditUndoList;
    FOrigRedoList: TSynEditUndoList;
    FLinesInWindow: Integer;
    FLeftChar: Integer;
    FMaxScrollWidth: Integer;
    FPaintLock: Integer;
    FReadOnly: Boolean;
    FRightEdge: Integer;
    FRightEdgeColor: TColor;
    FScrollHintColor: TColor;
    FScrollHintFormat: TScrollHintFormat;
    FScrollBars: TScrollStyle;
    FTextHeight: Integer;
    FTextOffset: Integer;
    FTopLine: Integer;
    FHighlighter: TSynCustomHighlighter;
    FSelectedColor: TSynSelectedColor;
    FActiveLineColor: TColor;
    FUndoList: TSynEditUndoList;
    FRedoList: TSynEditUndoList;
    FBookMarks: array[0..9] of TSynEditMark; // these are just references, FMarkList is the owner
    FMouseDownX: Integer;
    FMouseDownY: Integer;
    FBookMarkOpt: TSynBookMarkOpt;
    FBorderStyle: TSynBorderStyle;
    FHideSelection: Boolean;
    FMouseWheelAccumulator: Integer;
    FOverwriteCaret: TSynEditCaretType;
    FInsertCaret: TSynEditCaretType;
    FCaretOffset: TPoint;
    FKeyStrokes: TSynEditKeyStrokes;
    FModified: Boolean;
    FMarkList: TSynEditMarkList;
    FExtraLineSpacing: Integer;
    FSelectionMode: TSynSelectionMode;
    FActiveSelectionMode: TSynSelectionMode; //mode of the active selection
    FWantReturns: Boolean;
    FWantTabs: Boolean;
    FWordWrapPlugin: ISynEditBufferPlugin;
    FWordWrapGlyph: TSynGlyph;
    FCaretAtEOL: Boolean; // used by wordwrap

    FGutter: TSynGutter;
    FTabWidth: Integer;
    FTextDrawer: TSynTextDrawer;
    FInvalidateRect: TRect;
    FStateFlags: TSynStateFlags;
    FOptions: TSynEditorOptions;
    FStatusChanges: TSynStatusChanges;
    FLastKey: Word;
    FLastShiftState: TShiftState;
    FSearchEngine: TSynEditSearchCustom;
    FHookedCommandHandlers: TObjectList;
    FKbdHandler: TSynEditKbdHandler;
    FFocusList: TList;
    FPlugins: TObjectList;
    FScrollTimer: TTimer;
    FScrollDeltaX, fScrollDeltaY: Integer;
    // event handlers
    FOnChange: TNotifyEvent;
    FOnClearMark: TPlaceMarkEvent;
    FOnCommandProcessed: TProcessCommandEvent;
    FOnDropFiles: TDropFilesEvent;
    FOnGutterClick: TGutterClickEvent;
    FOnKeyPressW: TKeyPressWEvent;
    FOnMouseCursor: TMouseCursorEvent;
    FOnPaint: TPaintEvent;
    FOnPlaceMark: TPlaceMarkEvent;
    FOnProcessCommand: TProcessCommandEvent;
    FOnProcessUserCommand: TProcessCommandEvent;
    FOnReplaceText: TReplaceTextEvent;
    FOnSpecialLineColors: TSpecialLineColorsEvent;
    FOnSpecialTokenAttributes: TSpecialTokenAttributesEvent;
    FOnContextHelp: TContextHelpEvent;
    FOnPaintTransient: TPaintTransient;
    FOnScroll: TScrollEvent;
    FOnTokenHint: TGetTokenHintEvent;
    FOnGutterGetText: TGutterGetTextEvent;
    FOnGutterPaint: TGutterPaintEvent;
    FOnStatusChange: TStatusChangeEvent;
{$IFDEF SYN_CodeFolding}
    fOnScanForFoldRanges : TScanForFoldRangesEvent;
{$ENDIF}

    FShowSpecChar: Boolean;
    FPaintTransientLock: Integer;
    FIsScrolling: Boolean;

    FChainListCleared: TNotifyEvent;
    FChainListDeleted: TStringListChangeEvent;
    FChainListInserted: TStringListChangeEvent;
    FChainListPutted: TStringListChangeEvent;
    FChainLinesChanging: TNotifyEvent;
    FChainLinesChanged: TNotifyEvent;
    FChainedEditor: TCustomSynEdit;
    FChainUndoAdded: TNotifyEvent;
    FChainRedoAdded: TNotifyEvent;

    FAdditionalWordBreakChars: TSysCharSet;
    FAdditionalIdentChars: TSysCharSet;

{$IFDEF SYN_COMPILER_6_UP}
    FSearchNotFound: TCustomSynEditSearchNotFoundEvent;
    FOnFindBeforeSearch: TNotifyEvent;
    FOnReplaceBeforeSearch: TNotifyEvent;
    FOnCloseBeforeSearch: TNotifyEvent;
    FSelStartBeforeSearch: Integer;
    FSelLengthBeforeSearch: Integer;
{$ENDIF}

    FWindowProducedMessage: Boolean;

{$IFDEF SYN_LINUX}
    FDeadKeysFixed: Boolean;
{$ENDIF}

{$IFDEF SYN_CodeFolding}
    procedure ReScanForFoldRanges(FromLine : Integer; ToLine : Integer);
    procedure FullFoldScan;
    procedure ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine : Integer; ToLine : Integer);
{$ENDIF}
    procedure BookMarkOptionsChanged(Sender: TObject);
    procedure ComputeCaret(X, Y: Integer);
    procedure ComputeScroll(X, Y: Integer);
    procedure DoHomeKey(Selection: Boolean);
    procedure DoEndKey(Selection: Boolean);
    procedure DoLinesDeleted(FirstLine, Count: Integer);
    procedure DoLinesInserted(FirstLine, Count: Integer);
    procedure DoShiftTabKey;
    procedure DoTabKey;
    procedure DoCaseChange(const Cmd : TSynEditorCommand);
    function FindHookedCmdEvent(AHandlerProc: THookedCommandEvent): Integer;
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
    function LeftSpacesEx(const Line: UnicodeString; WantTabs: Boolean; CalcAlways : Boolean = False): Integer;
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
    procedure SetAdditionalIdentChars(const Value: TSysCharSet);
    procedure SetAdditionalWordBreakChars(const Value: TSysCharSet);
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
    procedure SizeOrFontChanged(bFont: Boolean);
    procedure ProperSetLine(ALine: Integer; const ALineText: UnicodeString);
    procedure UpdateModifiedStatus;
    procedure UndoRedoAdded(Sender: TObject);
    procedure UpdateLastCaretX;
    procedure UpdateScrollBars;
    procedure WriteAddedKeystrokes(Writer: TWriter);
    procedure WriteRemovedKeystrokes(Writer: TWriter);

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
{$IFDEF SYN_CodeFolding}
    procedure SetUseCodeFolding(const Value: Boolean);
    procedure OnCodeFoldingChange(Sender: TObject);
    function GetCollapseMarkRect(Row: Integer; Line: Integer = -1): TRect;
    function GetFoldShapeRect(Row: Integer): TRect;
{$ENDIF}
  protected
    FIgnoreNextChar: Boolean;
    FCharCodeString: string;
{$IFDEF SYN_COMPILER_6_UP}
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
{$ENDIF}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure InvalidateRect(const aRect: TRect; aErase: Boolean); virtual;
    procedure DblClick; override;
    procedure DecPaintLock;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoChange; virtual;
    procedure DoKeyPressW(var Message: TWMKey);
    procedure DragCanceled; override;
    procedure DragOver(Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    function GetReadOnly: Boolean; virtual;
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
      var Command: TSynEditorCommand; var AChar: WideChar; Data: Pointer); virtual;
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
    procedure SetReadOnly(Value: Boolean); virtual;
    procedure SetWantReturns(Value: Boolean);
    procedure SetSelTextPrimitive(const Value: UnicodeString);
    procedure SetSelTextPrimitiveEx(PasteMode: TSynSelectionMode; Value: PWideChar;
      AddToUndoList: Boolean);
    procedure SetWantTabs(Value: Boolean);
    procedure StatusChanged(AChanges: TSynStatusChanges);
    // If the translations requires Data, memory will be allocated for it via a
    // GetMem call.  The client must call FreeMem on Data if it is not NIL.
    function TranslateKeyCode(Code: Word; Shift: TShiftState;
      var Data: Pointer): TSynEditorCommand;
    procedure UndoItem;
    procedure UpdateMouseCursor; virtual;
  protected
    FGutterWidth: Integer;
    FInternalImage: TSynInternalImage;
    fSingleLineMode: boolean;
    procedure HideCaret;
    procedure ShowCaret;
    procedure DoOnClearBookmark(var Mark: TSynEditMark); virtual;
    procedure DoOnCommandProcessed(Command: TSynEditorCommand; AChar: WideChar;
      Data: Pointer); virtual;
    // no method DoOnDropFiles, intercept the WM_DROPFILES instead
    procedure DoOnGutterClick(Button: TMouseButton; X, Y: Integer); virtual;
    procedure DoOnPaint; virtual;
    procedure DoOnPaintTransientEx(TransientType: TTransientType; Lock: Boolean); virtual;
    procedure DoOnPaintTransient(TransientType: TTransientType); virtual;

    procedure DoOnPlaceMark(var Mark: TSynEditMark); virtual;
    procedure DoOnProcessCommand(var Command: TSynEditorCommand;
      var AChar: WideChar; Data: Pointer); virtual;
    function DoOnReplaceText(const ASearch, AReplace: UnicodeString;
      Line, Column: Integer): TSynReplaceAction; virtual;
    function DoOnSpecialLineColors(Line: Integer;
      var Foreground, Background: TColor): Boolean; virtual;
    procedure DoOnSpecialTokenAttributes(ALine, APos: Integer; const AToken: string; var FG, BG: TColor;
      var AStyle: TFontStyles);
    procedure DoOnStatusChange(Changes: TSynStatusChanges); virtual;
    function GetSelEnd: Integer;
    function GetSelStart: Integer;
    function GetSelLength: Integer;
    procedure SetSelEnd(const Value: Integer);
    procedure SetSelStart(const Value: Integer);
    procedure SetSelLength(const Value: Integer);
    procedure SetAlwaysShowCaret(const Value: Boolean);
    function ShrinkAtWideGlyphs(const S: UnicodeString; First: Integer;
      var CharCount: Integer): UnicodeString;
    procedure LinesHookChanged;
    procedure FontSmoothingChanged;
    property InternalCaretX: Integer write InternalSetCaretX;
    property InternalCaretY: Integer write InternalSetCaretY;
    property InternalCaretXY: TBufferCoord write InternalSetCaretXY;
    property FontSmoothing: TSynFontSmoothMethod read FFontSmoothing write SetFontSmoothing;
//++ DPI-Aware
    procedure ChangeScale(M, D: Integer{$if CompilerVersion >= 31}; isDpiChange: Boolean{$ifend}); override;
//-- DPI-Aware
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
    procedure AddKey(Command: TSynEditorCommand; Key1: Word; SS1: TShiftState;
      Key2: Word = 0; SS2: TShiftState = []);
{$ELSE}
    procedure AddKey(Command: TSynEditorCommand; Key1: Word; SS1: TShiftState;
      Key2: Word; SS2: TShiftState);
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
      Data: Pointer); virtual;
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
      Data: Pointer); virtual;
    function ExpandAtWideGlyphs(const S: UnicodeString): UnicodeString;
    function GetBookMark(BookMark: Integer; var X, Y: Integer): Boolean;
    function GetHighlighterAttriAtRowCol(const XY: TBufferCoord; var Token: UnicodeString;
      var Attri: TSynHighlighterAttributes): Boolean;
    function GetHighlighterAttriAtRowColEx(const XY: TBufferCoord; var Token: UnicodeString;
      var TokenType, Start: Integer;
      var Attri: TSynHighlighterAttributes): Boolean;
    function GetPositionOfMouse(out aPos: TBufferCoord): Boolean;
    function GetWordAtRowCol(XY: TBufferCoord): UnicodeString;
    procedure GotoBookMark(BookMark: Integer); virtual;
    procedure GotoLineAndCenter(ALine: Integer); virtual;
    function IsIdentChar(AChar: WideChar): Boolean; virtual;
    function IsWhiteChar(AChar: WideChar): Boolean; virtual;
    function IsWordBreakChar(AChar: WideChar): Boolean; virtual;

    procedure InsertBlock(const BB, BE: TBufferCoord; ChangeStr: PWideChar; AddToUndoList: Boolean);
    procedure InsertLine(const BB, BE: TBufferCoord; ChangeStr: PWideChar; AddToUndoList: Boolean);
    function UnifiedSelection: TBufferBlock;
    procedure DoBlockIndent;
    procedure DoBlockUnindent;

    procedure InvalidateGutter;
    procedure InvalidateGutterLine(aLine: Integer);
    procedure InvalidateGutterLines(FirstLine, LastLine: Integer);
    procedure InvalidateLine(Line: Integer);
    procedure InvalidateLines(FirstLine, LastLine: Integer);
    procedure InvalidateSelection;
    procedure MarkModifiedLinesAsSaved;
    procedure ResetModificationIndicator;
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
      AHandlerData: Pointer);
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

    procedure WndProc(var Msg: TMessage); override;
    procedure SetLinesPointer(ASynEdit: TCustomSynEdit);
    procedure RemoveLinesPointer;
    procedure HookTextBuffer(aBuffer: TSynEditStringList;
      aUndo, aRedo: TSynEditUndoList);
    procedure UnHookTextBuffer;
{$IFDEF SYN_CodeFolding}
    procedure CollapseAll;
    procedure UncollapseAll;
    procedure Collapse(FoldRangeIndex: Integer; Invalidate:Boolean = True);
    procedure Uncollapse(FoldRangeIndex: Integer; Invalidate:Boolean = True);
    procedure UncollapseAroundLine(Line: Integer);
    procedure CollapseNearest;
    procedure UncollapseNearest;
    procedure CollapseLevel(Level : integer);
    procedure UnCollapseLevel(Level : integer);
    procedure CollapseFoldType(FoldType : Integer);
    procedure UnCollapseFoldType(FoldType : Integer);
{$ENDIF}
  public
    property AdditionalIdentChars: TSysCharSet read FAdditionalIdentChars write SetAdditionalIdentChars;
    property AdditionalWordBreakChars: TSysCharSet read FAdditionalWordBreakChars write SetAdditionalWordBreakChars;
    property BlockBegin: TBufferCoord read GetBlockBegin write SetBlockBegin;
    property BlockEnd: TBufferCoord read GetBlockEnd write SetBlockEnd;
    property CanPaste: Boolean read GetCanPaste;
    property CanRedo: Boolean read GetCanRedo;
    property CanUndo: Boolean read GetCanUndo;
    property CaretX: Integer read FCaretX write SetCaretX;
    property CaretY: Integer read FCaretY write SetCaretY;
    property CaretXY: TBufferCoord read GetCaretXY write SetCaretXY;
    property ActiveLineColor: TColor read FActiveLineColor
      write SetActiveLineColor default clNone;
    property DisplayX: Integer read GetDisplayX;
    property DisplayY: Integer read GetDisplayY;
    property DisplayXY: TDisplayCoord read GetDisplayXY;
    property DisplayLineCount: Integer read GetDisplayLineCount;
    property CharsInWindow: Integer read FCharsInWindow;
    property CharWidth: Integer read FCharWidth;
    property Color;
    property Font: TFont read GetFont write SetFont;
    property Highlighter: TSynCustomHighlighter
      read FHighlighter write SetHighlighter;
    property HintMode: TSynHintMode read FHintMode write FHintMode default shmDefault;
    property LeftChar: Integer read FLeftChar write SetLeftChar;
    property LineHeight: Integer read FTextHeight;
    property LinesInWindow: Integer read FLinesInWindow;
    property LineText: UnicodeString read GetLineText write SetLineText;
    property Lines: TUnicodeStrings read FLines write SetLines;
    property Marks: TSynEditMarkList read FMarkList;
    property MaxScrollWidth: Integer read FMaxScrollWidth write SetMaxScrollWidth
      default 1024;
    property Modified: Boolean read FModified write SetModified;
    property PaintLock: Integer read FPaintLock;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property SearchEngine: TSynEditSearchCustom read FSearchEngine write SetSearchEngine;
    property SelAvail: Boolean read GetSelAvail;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelTabBlock: Boolean read GetSelTabBlock;
    property SelTabLine: Boolean read GetSelTabLine;
    property SelText: UnicodeString read GetSelText write SetSelTextExternal;
    property StateFlags: TSynStateFlags read FStateFlags;
    property Text: UnicodeString read SynGetText write SynSetText;
    property TopLine: Integer read FTopLine write SetTopLine;
    property WordAtCursor: UnicodeString read GetWordAtCursor;
    property WordAtMouse: UnicodeString read GetWordAtMouse;
    property UndoList: TSynEditUndoList read FUndoList;
    property RedoList: TSynEditUndoList read FRedoList;
  public
    property OnProcessCommand: TProcessCommandEvent
      read FOnProcessCommand write FOnProcessCommand;

{$IFDEF SYN_CodeFolding}
    property CodeFolding: TSynCodeFolding read fCodeFolding write fCodeFolding;
    property UseCodeFolding: Boolean read fUseCodeFolding write SetUseCodeFolding;
    property AllFoldRanges: TSynFoldRanges read fAllFoldRanges;
{$ENDIF}
    property BookMarkOptions: TSynBookMarkOpt
      read FBookMarkOpt write FBookMarkOpt;
    property BorderStyle: TSynBorderStyle read FBorderStyle write SetBorderStyle
      default bsSingle;
    property ExtraLineSpacing: Integer
      read FExtraLineSpacing write SetExtraLineSpacing default 0;
    property Gutter: TSynGutter read FGutter write SetGutter;
    property HideSelection: Boolean read FHideSelection write SetHideSelection
      default False;
    property InsertCaret: TSynEditCaretType read FInsertCaret
      write SetInsertCaret default ctVerticalLine;
    property InsertMode: Boolean read FInserting write SetInsertMode
      default true;
    property IsScrolling : Boolean read FIsScrolling;
    property Keystrokes: TSynEditKeyStrokes
      read FKeyStrokes write SetKeystrokes stored False;
    property MaxUndo: Integer read GetMaxUndo write SetMaxUndo default 1024;
    property Options: TSynEditorOptions read GetOptions write SetOptions
      default SYNEDIT_DEFAULT_OPTIONS;
    property OverwriteCaret: TSynEditCaretType read FOverwriteCaret
      write SetOverwriteCaret default ctBlock;
    property RightEdge: Integer read FRightEdge write SetRightEdge default 80;
    property RightEdgeColor: TColor
      read FRightEdgeColor write SetRightEdgeColor default clSilver;
    property ScrollHintColor: TColor read FScrollHintColor
      write FScrollHintColor default clInfoBk;
    property ScrollHintFormat: TScrollHintFormat read FScrollHintFormat
      write FScrollHintFormat default shfTopLineOnly;
    property ScrollBars: TScrollStyle
      read FScrollBars write SetScrollBars default ssBoth;
    property SelectedColor: TSynSelectedColor
      read FSelectedColor write FSelectedColor;
    property SelectionMode: TSynSelectionMode
      read FSelectionMode write SetSelectionMode default smNormal;
    property ActiveSelectionMode: TSynSelectionMode read FActiveSelectionMode
      write SetActiveSelectionMode stored False;
    property TabWidth: Integer read FTabWidth write SetTabWidth default 8;
    property WantReturns: Boolean read FWantReturns write SetWantReturns default True;
    property WantTabs: Boolean read FWantTabs write SetWantTabs default False;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap default False;
    property WordWrapGlyph: TSynGlyph read FWordWrapGlyph write SetWordWrapGlyph;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClearBookmark: TPlaceMarkEvent read FOnClearMark
      write FOnClearMark;
    property OnCommandProcessed: TProcessCommandEvent
      read FOnCommandProcessed write FOnCommandProcessed;
    property OnContextHelp: TContextHelpEvent
      read FOnContextHelp write FOnContextHelp;
    property OnDropFiles: TDropFilesEvent read FOnDropFiles write FOnDropFiles;
    property OnGutterClick: TGutterClickEvent
      read FOnGutterClick write FOnGutterClick;
    property OnGutterGetText: TGutterGetTextEvent read FOnGutterGetText
      write FOnGutterGetText;
    property OnGutterPaint: TGutterPaintEvent read FOnGutterPaint
      write FOnGutterPaint;
    property OnMouseCursor: TMouseCursorEvent read FOnMouseCursor
      write FOnMouseCursor;
    property OnKeyPress: TKeyPressWEvent read FOnKeyPressW write FOnKeyPressW;
    property OnPaint: TPaintEvent read FOnPaint write FOnPaint;
    property OnPlaceBookmark: TPlaceMarkEvent
      read FOnPlaceMark write FOnPlaceMark;
    property OnProcessUserCommand: TProcessCommandEvent
      read FOnProcessUserCommand write FOnProcessUserCommand;
    property OnReplaceText: TReplaceTextEvent read FOnReplaceText
      write FOnReplaceText;
    property OnSpecialLineColors: TSpecialLineColorsEvent
      read FOnSpecialLineColors write FOnSpecialLineColors;
    property OnSpecialTokenAttributes: TSpecialTokenAttributesEvent
      read FOnSpecialTokenAttributes write FOnSpecialTokenAttributes;
    property OnStatusChange: TStatusChangeEvent
      read FOnStatusChange write FOnStatusChange;
    property OnPaintTransient: TPaintTransient
      read FOnPaintTransient write FOnPaintTransient;
    property OnScroll: TScrollEvent
      read FOnScroll write FOnScroll;
    property OnTokenHint: TGetTokenHintEvent read FOnTokenHint write FOnTokenHint;
{$IFDEF SYN_CodeFolding}
    property OnScanForFoldRanges: TScanForFoldRangesEvent
      read fOnScanForFoldRanges write fOnScanForFoldRanges;
{$ENDIF}
  published
    property Cursor default crIBeam;
{$IFDEF SYN_COMPILER_6_UP}
    property OnSearchNotFound: TCustomSynEditSearchNotFoundEvent
      read FSearchNotFound write FSearchNotFound;
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
    property Ctl3D;
    property ParentCtl3D;
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
{$IFDEF SYN_COMPILER_4_UP}
    property OnEndDock;
    property OnStartDock;
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
{$IFDEF SYN_CodeFolding}
    property CodeFolding;
    property UseCodeFolding;
{$ENDIF}
    property BookMarkOptions;
    property BorderStyle;
    property ExtraLineSpacing;
    property Gutter;
    property HideSelection;
    property Highlighter;
    property HintMode;
    property ImeMode;
    property ImeName;
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
    property OnTokenHint;
    property OnPaintTransient;
{$IFDEF SYN_CodeFolding}
    property OnScanForFoldRanges;
{$ENDIF}

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
  Clipbrd,
  ShellAPI,
  SynEditWordWrap,
  SynEditStrConst;

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
    FEvent: THookedCommandEvent;
    FData: Pointer;
    constructor Create(AEvent: THookedCommandEvent; AData: Pointer);
    function Equals(AEvent: THookedCommandEvent): Boolean; {$IFDEF UNICODE} reintroduce; {$ENDIF}
  end;

constructor THookedCommandHandlerEntry.Create(AEvent: THookedCommandEvent;
  AData: Pointer);
begin
  inherited Create;
  FEvent := AEvent;
  FData := AData;
end;

function THookedCommandHandlerEntry.Equals(AEvent: THookedCommandEvent): Boolean;
begin
  with TMethod(FEvent) do
    Result := (Code = TMethod(AEvent).Code) and (Data = TMethod(AEvent).Data);
end;

{ TCustomSynEdit }

function TCustomSynEdit.PixelsToNearestRowColumn(aX, aY: Integer): TDisplayCoord;
// Result is in display coordinates
var
  f: Single;
begin
  f := (aX - FGutterWidth - 2) / FCharWidth;
  // don't return a partially visible last line
  if aY >= FLinesInWindow * FTextHeight then
  begin
    aY := FLinesInWindow * FTextHeight - 1;
    if aY < 0 then
      aY := 0;
  end;
  Result.Column := Max(1, LeftChar + Round(f));
  Result.Row := Max(1, TopLine + (aY div FTextHeight));
end;

function TCustomSynEdit.PixelsToRowColumn(aX, aY: Integer): TDisplayCoord;
begin
  Result.Column := Max(1, LeftChar + ((aX - FGutterWidth - 2) div FCharWidth));
  Result.Row := Max(1, TopLine + (aY div FTextHeight));
end;

function TCustomSynEdit.RowColumnToPixels(const RowCol: TDisplayCoord): TPoint;
begin
  Result.X := (RowCol.Column-1) * FCharWidth + FTextOffset;
  Result.Y := (RowCol.Row - FTopLine) * FTextHeight;
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
    FScrollTimer.Enabled := False;
    Exit;
  end;

  iScrollBounds := Bounds(FGutterWidth, 0, FCharsInWindow * FCharWidth,
    FLinesInWindow * FTextHeight);
  if BorderStyle = bsNone then
    InflateRect(iScrollBounds, -2, -2);

  if X < iScrollBounds.Left then
    FScrollDeltaX := (X - iScrollBounds.Left) div FCharWidth - 1
  else if X >= iScrollBounds.Right then
    FScrollDeltaX := (X - iScrollBounds.Right) div FCharWidth + 1
  else
    FScrollDeltaX := 0;

  if Y < iScrollBounds.Top then
    fScrollDeltaY := (Y - iScrollBounds.Top) div FTextHeight - 1
  else if Y >= iScrollBounds.Bottom then
    fScrollDeltaY := (Y - iScrollBounds.Bottom) div FTextHeight + 1
  else
    fScrollDeltaY := 0;

  FScrollTimer.Enabled := (FScrollDeltaX <> 0) or (fScrollDeltaY <> 0);
end;

procedure TCustomSynEdit.DoCopyToClipboard(const SText: UnicodeString);
var
  Mem: HGLOBAL;
  P: PByte;
  SLen: Integer;
begin
  if SText = '' then Exit;
  SetClipboardText(SText);
  SLen := Length(SText);
  // Open and Close are the only TClipboard methods we use because TClipboard
  // is very hard (impossible) to work with if you want to put more than one
  // format on it at a time.
  Clipboard.Open;
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
          PSynSelectionMode(P)^ := FActiveSelectionMode;
          Inc(P, SizeOf(TSynSelectionMode));
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

procedure TCustomSynEdit.CopyToClipboard;
var
  SText: UnicodeString;
  ChangeTrim: Boolean;
begin
  if SelAvail then
  begin
    ChangeTrim := (FActiveSelectionMode = smColumn) and (eoTrimTrailingSpaces in Options);
    try
      if ChangeTrim then
        Exclude(FOptions, eoTrimTrailingSpaces);
      SText := SelText;
    finally
      if ChangeTrim then
        Include(FOptions, eoTrimTrailingSpaces);
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
  FLines := TSynEditStringList.Create(ExpandAtWideGlyphs);
  FOrigLines := FLines;
  with TSynEditStringList(FLines) do
  begin
    OnChange := LinesChanged;
    OnChanging := LinesChanging;
    OnCleared := ListCleared;
    OnDeleted := ListDeleted;
    OnInserted := ListInserted;
    OnPutted := ListPutted;
  end;
  FFontDummy := TFont.Create;
  FUndoList := TSynEditUndoList.Create;
  FUndoList.OnAddedUndo := UndoRedoAdded;
  FOrigUndoList := FUndoList;
  FRedoList := TSynEditUndoList.Create;
  FRedoList.OnAddedUndo := UndoRedoAdded;
  FOrigRedoList := FRedoList;
  fSingleLineMode := false;

{$IFDEF SYN_COMPILER_4_UP}
  DoubleBuffered := False;
{$ENDIF}
  FActiveLineColor := clNone;
  FSelectedColor := TSynSelectedColor.Create;
  FSelectedColor.OnChange := SelectedColorsChanged;
  FBookMarkOpt := TSynBookMarkOpt.Create(Self);
  FBookMarkOpt.OnChange := BookMarkOptionsChanged;
// FRightEdge has to be set before FontChanged is called for the first time
  FRightEdge := 80;
  FGutter := TSynGutter.Create;
  FGutter.OnChange := GutterChanged;
  FGutterWidth := FGutter.Width;
  FWordWrapGlyph := TSynGlyph.Create(HINSTANCE, 'SynEditWrapped', clLime);
  FWordWrapGlyph.OnChange := WordWrapGlyphChange;
  FTextOffset := FGutterWidth + 2;
  ControlStyle := ControlStyle + [csOpaque, csSetCaption];
{$IFDEF SYN_COMPILER_7_UP}
  ControlStyle := ControlStyle + [csNeedsBorderPaint];
{$ENDIF}
  Height := 150;
  Width := 200;
  Cursor := crIBeam;
  Color := clWindow;
{$IFDEF MSWINDOWS}
  FFontDummy.Name := 'Courier New';
  FFontDummy.Size := 10;
{$ENDIF}
{$IFDEF SYN_COMPILER_3_UP}
  FFontDummy.CharSet := DEFAULT_CHARSET;
{$ENDIF}
  FTextDrawer := TSynTextDrawer.Create([fsBold], FFontDummy);
  Font.Assign(FFontDummy);
  Font.OnChange := SynFontChanged;
  ParentFont := False;
  ParentColor := False;
  TabStop := True;
  FInserting := True;
  FMaxScrollWidth := 1024;
  FScrollBars := ssBoth;
  FBorderStyle := bsSingle;
  FHintMode := shmDefault;
  FInsertCaret := ctVerticalLine;
  FOverwriteCaret := ctBlock;
  FSelectionMode := smNormal;
  FActiveSelectionMode := smNormal;
  FFocusList := TList.Create;
  FKbdHandler := TSynEditKbdHandler.Create;
  FKeyStrokes := TSynEditKeyStrokes.Create(Self);
  FMarkList := TSynEditMarkList.Create(self);
  FMarkList.OnChange := MarkListChange;
  SetDefaultKeystrokes;
  FRightEdgeColor := clSilver;
  FWantReturns := True;
  FWantTabs := False;
  FTabWidth := 8;
  FLeftChar := 1;
  FTopLine := 1;
  FCaretX := 1;
  FLastCaretX := 1;
  FCaretY := 1;
  FBlockBegin.Char := 1;
  FBlockBegin.Line := 1;
  FBlockEnd := FBlockBegin;
  FOptions := SYNEDIT_DEFAULT_OPTIONS;
  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.Enabled := False;
  FScrollTimer.Interval := 100;
  FScrollTimer.OnTimer := ScrollTimerHandler;

  FScrollHintColor := clInfoBk;
  FScrollHintFormat := shfTopLineOnly;
{$IFDEF SYN_CodeFolding}
  FCodeFolding := TSynCodeFolding.Create;
  FCodeFolding.OnChange := OnCodeFoldingChange;
  FAllFoldRanges := TSynFoldRanges.Create;
{$ENDIF}

  SynFontChanged(nil);
end;

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
    Style := Style or BorderStyles[FBorderStyle] or WS_CLIPCHILDREN;

    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
      // avoid flicker while scrolling or resizing
      if not (csDesigning in ComponentState) and CheckWin32Version(5, 1) then
        ExStyle := ExStyle or WS_EX_COMPOSITED;
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

procedure TCustomSynEdit.DecPaintLock;
var
  vAuxPos: TDisplayCoord;
begin
  Assert(FPaintLock > 0);
  Dec(FPaintLock);
  if (FPaintLock = 0) and HandleAllocated then
  begin
    if sfScrollbarChanged in FStateFlags then
      UpdateScrollbars;
    // Locks the caret inside the visible area
    if WordWrap and ([scCaretX,scCaretY] * FStatusChanges <> []) then
    begin
      vAuxPos := DisplayXY;
      // This may happen in the last row of a line or in rows which length is
      // greater than CharsInWindow (Tabs and Spaces are allowed beyond
      // CharsInWindow while wrapping the lines)
      if (vAuxPos.Column > CharsInWindow +1) and (CharsInWindow > 0) then
      begin
        if FCaretAtEOL then
          FCaretAtEOL := False
        else
        begin
          if scCaretY in FStatusChanges then
          begin
            vAuxPos.Column := CharsInWindow + 1;
            FCaretX := DisplayToBufferPos(vAuxPos).Char;
            Include(FStatusChanges,scCaretX);
            UpdateLastCaretX;
          end;
        end;
        Include(FStateFlags, sfCaretChanged);
      end;
    end;
    if sfCaretChanged in FStateFlags then
      UpdateCaret;
    if FStatusChanges <> [] then
      DoOnStatusChange(FStatusChanges);
  end;
end;

destructor TCustomSynEdit.Destroy;
begin
  Highlighter := nil;
  if (FChainedEditor <> nil) or (FLines <> FOrigLines) then
    RemoveLinesPointer;

  inherited Destroy;

  // free listeners while other fields are still valid

  // do not use FreeAndNil, it first nils and then freey causing problems with
  // code accessing FHookedCommandHandlers while destruction
  FHookedCommandHandlers.Free;
  FHookedCommandHandlers := nil;
  // do not use FreeAndNil, it first nils and then frees causing problems with
  // code accessing FPlugins while destruction
  FPlugins.Free;
  FPlugins := nil;

  FMarkList.Free;
  FBookMarkOpt.Free;
  FKeyStrokes.Free;
  FKbdHandler.Free;
  FFocusList.Free;
  FSelectedColor.Free;
  FOrigUndoList.Free;
  FOrigRedoList.Free;
  FGutter.Free;
  FWordWrapGlyph.Free;
  FTextDrawer.Free;
  FInternalImage.Free;
  FFontDummy.Free;
  FOrigLines.Free;
{$IFDEF SYN_CodeFolding}
  fCodeFolding.Free;
  fAllFoldRanges.Free;
{$ENDIF}
end;

function TCustomSynEdit.GetBlockBegin: TBufferCoord;
begin
  if (FBlockEnd.Line < FBlockBegin.Line)
    or ((FBlockEnd.Line = FBlockBegin.Line) and (FBlockEnd.Char < FBlockBegin.Char))
  then
    Result := FBlockEnd
  else
    Result := FBlockBegin;
end;

function TCustomSynEdit.GetBlockEnd: TBufferCoord;
begin
  if (FBlockEnd.Line < FBlockBegin.Line)
    or ((FBlockEnd.Line = FBlockBegin.Line) and (FBlockEnd.Char < FBlockBegin.Char))
  then
    Result := FBlockBegin
  else
    Result := FBlockEnd;
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
  Result := (FBlockBegin.Char <> FBlockEnd.Char) or
    ((FBlockBegin.Line <> FBlockEnd.Line) and (FActiveSelectionMode <> smColumn));
end;

function TCustomSynEdit.GetSelTabBlock: Boolean;
begin
  Result := (FBlockBegin.Line <> FBlockEnd.Line) and (FActiveSelectionMode <> smColumn);
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
      Result := Len;
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
    case FActiveSelectionMode of
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
   Result := GetWordAtRowCol(CaretXY);
end;

procedure TCustomSynEdit.HideCaret;
begin
  if sfCaretVisible in FStateFlags then
    if Windows.HideCaret(Handle) then
      Exclude(FStateFlags, sfCaretVisible);
end;

procedure TCustomSynEdit.IncPaintLock;
begin
  Inc(FPaintLock);
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

procedure TCustomSynEdit.InvalidateGutterLines(FirstLine, LastLine: Integer);
// note: FirstLine and LastLine don't need to be in correct order
var
  rcInval: TRect;
begin
  if Visible and HandleAllocated then
    if (FirstLine = -1) and (LastLine = -1) then
    begin
      rcInval := Rect(0, 0, FGutterWidth, ClientHeight);
      if sfLinesChanging in FStateFlags then
        UnionRect(fInvalidateRect, rcInval, fInvalidateRect)
      else
        InvalidateRect(rcInval, False);
    end
    else begin
      { find the visible lines first }
      if (LastLine < FirstLine) then
        SwapInt(LastLine, FirstLine);
{$IFDEF SYN_CodeFolding}
      if UseCodeFolding or WordWrap then
{$ELSE}
      if WordWrap then
{$ENDIF}
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
        rcInval := Rect(0, FTextHeight * (FirstLine - TopLine),
          FGutterWidth, FTextHeight * (LastLine - TopLine + 1));
        if sfLinesChanging in FStateFlags then
          UnionRect(fInvalidateRect, rcInval, fInvalidateRect)
        else
          InvalidateRect(rcInval, False);
      end;
    end;
end;

procedure TCustomSynEdit.InvalidateLines(FirstLine, LastLine: Integer);
// note: FirstLine and LastLine don't need to be in correct order
var
  rcInval: TRect;
begin
  if Visible and HandleAllocated then
    if (FirstLine = -1) and (LastLine = -1) then
    begin
      rcInval := ClientRect;
      Inc(rcInval.Left, FGutterWidth);
      if sfLinesChanging in FStateFlags then
//++ Flicker Reduction
        UnionRect(fInvalidateRect, rcInval, fInvalidateRect)
//-- Flicker Reduction
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

{$IFDEF SYN_CodeFolding}
      if UseCodeFolding or WordWrap then
      begin
        FirstLine := LineToRow(FirstLine);
        // Could avoid this conversion if (First = Last) and
        // (Length < CharsInWindow) but the dependency isn't worth IMO.
        if LastLine < Lines.Count then begin
          if UseCodeFolding then
            LastLine := LineToRow(LastLine)
          else
            LastLine := LineToRow(LastLine + 1) - 1;
        end;
      end;
{$ELSE}
      if WordWrap then
      begin
        FirstLine := LineToRow(FirstLine);
        // Could avoid this conversion if (First = Last) and
        // (Length < CharsInWindow) but the dependency isn't worth IMO.
        if LastLine < Lines.Count then
          LastLine := LineToRow(LastLine + 1) - 1;
      end;
{$ENDIF}

      // TopLine is in display coordinates, so FirstLine and LastLine must be
      // converted previously.
      FirstLine := Max(FirstLine, TopLine);
      LastLine := Min(LastLine, TopLine + LinesInWindow);

      { any line visible? }
      if (LastLine >= FirstLine) then
      begin
        rcInval := Rect(FGutterWidth, FTextHeight * (FirstLine - TopLine),
          ClientWidth, FTextHeight * (LastLine - TopLine + 1));
        if sfLinesChanging in FStateFlags then
          UnionRect(fInvalidateRect, rcInval, fInvalidateRect)
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
var
  CharCode: Integer;
  KeyMsg: TWMKey;
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

  inherited;
  FKbdHandler.ExecuteKeyUp(Self, Key, Shift);
end;

procedure TCustomSynEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  Data: Pointer;
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
  FKbdHandler.ExecuteKeyDown(Self, Key, Shift);

  Data := nil;
  C := #0;
  try
    Cmd := TranslateKeyCode(Key, Shift, Data);
    if Cmd <> ecNone then begin
      Key := 0; // eat it.
      Include(FStateFlags, sfIgnoreNextChar);
      CommandProcessor(Cmd, C, Data);
    end
    else
      Exclude(FStateFlags, sfIgnoreNextChar);
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
begin
end;

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

procedure TCustomSynEdit.KeyPressW(var Key: WideChar);
begin
  // don't fire the event if key is to be ignored
  if not (sfIgnoreNextChar in FStateFlags) then
  begin
    FKbdHandler.ExecuteKeyPress(Self, Key);
    CommandProcessor(ecChar, Key, nil);
  end
  else
    // don't ignore further keys
    Exclude(FStateFlags, sfIgnoreNextChar);
end;

function TCustomSynEdit.LeftSpaces(const Line: UnicodeString): Integer;
begin
  Result := LeftSpacesEx(Line, False);
end;

function TCustomSynEdit.LeftSpacesEx(const Line: UnicodeString; WantTabs: Boolean; CalcAlways : Boolean = False): Integer;
var
  p: PWideChar;
begin
  p := PWideChar(UnicodeString(Line));
  if Assigned(p) and ((eoAutoIndent in fOptions) or CalcAlways) then
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
  Include(FStateFlags, sfLinesChanging);
end;

procedure TCustomSynEdit.LinesChanged(Sender: TObject);
var
  vOldMode: TSynSelectionMode;
begin
{$IFDEF SYN_CodeFolding}
  if (sfLinesChanging in fStateFlags) and fAllFoldRanges.StopScanning(fLines) then
  begin
    if Assigned(fHighlighter) and (fHighlighter is TSynCustomCodeFoldingHighlighter) then
      TSynCustomCodeFoldingHighlighter(fHighlighter).AdjustFoldRanges(AllFoldRanges,
      fLines);
    InvalidateGutter;
    Include(fStateFlags, sfScrollbarChanged);
  end;
{$ENDIF}
  Exclude(FStateFlags, sfLinesChanging);
  if HandleAllocated then
  begin
//++ Flicker Reduction
//    UpdateScrollBars;
//-- Flicker Reduction
    vOldMode := FActiveSelectionMode;
    SetBlockBegin(CaretXY);
    FActiveSelectionMode := vOldMode;
    InvalidateRect(FInvalidateRect, False);
    FillChar(FInvalidateRect, SizeOf(TRect), 0);
    if FGutter.ShowLineNumbers and FGutter.AutoSize then
      FGutter.AutoSizeDigitCount(Lines.Count);
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
      FMouseDownX := X;
      FMouseDownY := Y;
    end;
  end;

  inherited MouseDown(Button, Shift, X, Y);

  if (Button = mbLeft) and (ssDouble in Shift) then Exit;

  FKbdHandler.ExecuteMouseDown(Self, Button, Shift, X, Y);

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
    //For some reason SynEdit could get to a state where MouseCapture was True
    //while just scrolling the window. That resulted in contents being painted
    //while vertical scrollbar was moved. To make sure that we paint only when
    //a MouseDown has happened inside the SynEdit Window, an extra flag is used.
    Include(FStateFlags, sfMouseCaptured);
    //if mousedown occurred in selected block begin drag operation
    Exclude(FStateFlags, sfWaitForDragging);
    if bWasSel and (eoDragDropEditing in FOptions) and (X >= FGutterWidth + 2)
      and (SelectionMode = smNormal) and IsPointInSelection(DisplayToBufferPos(PixelsToRowColumn(X, Y))) then
    begin
      bStartDrag := True
    end;
  end;

  if (Button = mbLeft) and bStartDrag then
    Include(FStateFlags, sfWaitForDragging)
  else
  begin
    if not (sfDblClicked in FStateFlags) then
    begin
      if ssShift in Shift then
        //BlockBegin and BlockEnd are restored to their original position in the
        //code from above and SetBlockEnd will take care of proper invalidation
        SetBlockEnd(CaretXY)
      else
      begin
        if (eoAltSetsColumnMode in Options) and (FActiveSelectionMode <> smLine) then
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

  if (X < FGutterWidth) then
    Include(FStateFlags, sfPossibleGutterClick);
  if (sfPossibleGutterClick in FStateFlags) and (Button = mbRight) then
  begin
    DoOnGutterClick(Button, X, Y)
  end;

  SetFocus;
  Windows.SetFocus(Handle);
end;

procedure TCustomSynEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TDisplayCoord;
begin
  inherited MouseMove(Shift, x, y);
  if MouseCapture and (sfWaitForDragging in FStateFlags) then
  begin
    if (Abs(FMouseDownX - X) >= GetSystemMetrics(SM_CXDRAG))
      or (Abs(FMouseDownY - Y) >= GetSystemMetrics(SM_CYDRAG)) then
    begin
      Exclude(FStateFlags, sfWaitForDragging);
      BeginDrag(False);
    end;
  end
  else if (ssLeft in Shift) and MouseCapture and (sfMouseCaptured in FStateFlags) then
  begin
    // should we begin scrolling?
    ComputeScroll(X, Y);
    { compute new caret }
    P := PixelsToNearestRowColumn(X, Y);
    P.Row := MinMax(P.Row, 1, DisplayLineCount);
    if FScrollDeltaX <> 0 then
      P.Column := DisplayX;
    if fScrollDeltaY <> 0 then
      P.Row := DisplayY;
    InternalCaretXY := DisplayToBufferPos(P);
    BlockEnd := CaretXY;
    if (sfPossibleGutterClick in FStateFlags) and (FBlockBegin.Line <> CaretXY.Line) then
      Include(FStateFlags, sfGutterDragging);
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
  if FScrollDeltaX <> 0 then
  begin
    LeftChar := LeftChar + FScrollDeltaX;
    X := LeftChar;
    if FScrollDeltaX > 0 then  // scrolling right?
      Inc(X, CharsInWindow);
    C.Column := X;
  end;
  if fScrollDeltaY <> 0 then
  begin
    if GetKeyState(SYNEDIT_SHIFT) < 0 then
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
      if MouseCapture and (sfMouseCaptured in FStateFlags) then
        SetBlockEnd(CaretXY);
    finally
      DecPaintLock;
    end;
  end;
  ComputeScroll(iMousePos.x, iMousePos.y);
end;

procedure TCustomSynEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
{$IFDEF SYN_CodeFolding}
Var
  ptLineCol: TBufferCoord;
  ptRowCol: TDisplayCoord;
  Index: Integer;
  Rect: TRect;
{$ENDIF}
begin
  inherited MouseUp(Button, Shift, X, Y);
  FKbdHandler.ExecuteMouseUp(Self, Button, Shift, X, Y);

  FScrollTimer.Enabled := False;
  if (Button = mbRight) and (Shift = [ssRight]) and Assigned(PopupMenu) then
    Exit;
  MouseCapture := False;
  Exclude(FStateFlags, sfMouseCaptured);
  if (sfPossibleGutterClick in FStateFlags) and (X < FGutterWidth) and (Button <> mbRight) then
    DoOnGutterClick(Button, X, Y)
  else
  if FStateFlags * [sfDblClicked, sfWaitForDragging] = [sfWaitForDragging] then
  begin
    ComputeCaret(X, Y);
    if not(ssShift in Shift) then
      SetBlockBegin(CaretXY);
    SetBlockEnd(CaretXY);
    Exclude(FStateFlags, sfWaitForDragging);
  end;
  Exclude(FStateFlags, sfDblClicked);
  Exclude(FStateFlags, sfPossibleGutterClick);
  Exclude(FStateFlags, sfGutterDragging);
{$IFDEF SYN_CodeFolding}
  ptRowCol := PixelsToRowColumn(X, Y);
  ptLineCol := DisplayToBufferPos(ptRowCol);

  if UseCodeFolding and CodeFolding.ShowHintMark and
    fAllFoldRanges.CollapsedFoldStartAtLine(ptLineCol.Line, Index) then
  begin
    Rect := GetCollapseMarkRect(ptRowCol.Row, ptLineCol.Line);
    if PtInRect(Rect, Point(X,Y)) then
      Uncollapse(Index);
  end;
{$ENDIF}
end;

procedure TCustomSynEdit.DoOnGutterClick(Button: TMouseButton; X, Y: Integer);
var
  i     : Integer;
  offs  : Integer;
  line  : Integer;
  allmrk: TSynEditMarks;
  mark  : TSynEditMark;
{$IFDEF SYN_CodeFolding}
  Index : integer;
  RowColumn: TDisplayCoord;
begin
  RowColumn := PixelsToRowColumn(X, Y);
  Line := RowToLine(RowColumn.Row);

  // Check if we clicked on a folding thing
  if UseCodeFolding then begin
    if AllFoldRanges.FoldStartAtLine(Line, Index) then
    begin
      // See if we actually clicked on the rectangle...
      if PtInRect(GetFoldShapeRect(RowColumn.Row), Point(X, Y)) then begin
        if AllFoldRanges.Ranges[Index].Collapsed then
          Uncollapse(Index)
        else
          Collapse(Index);
        Exit;
      end;
    end;
  end;
{$ELSE}
begin
{$ENDIF}
  if Assigned(FOnGutterClick) then
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
            Break;
          end;
        end;
      end; //for
      FOnGutterClick(Self, Button, X, Y, line, mark);
    end;
  end;
end;

procedure TCustomSynEdit.Paint;
var
  rcClip, rcDraw: TRect;
  nL1, nL2, nC1, nC2: Integer;
begin
  // Get the invalidated rect. Compute the invalid area in lines / columns.
  rcClip := Canvas.ClipRect;
  // columns
  nC1 := LeftChar;
  if (rcClip.Left > FGutterWidth + 2) then
    Inc(nC1, (rcClip.Left - FGutterWidth - 2) div CharWidth);
  nC2 := LeftChar +
    (rcClip.Right - FGutterWidth - 2 + CharWidth - 1) div CharWidth;
  // lines
  nL1 := Max(TopLine + rcClip.Top div FTextHeight, TopLine);
  nL2 := MinMax(TopLine + (rcClip.Bottom + FTextHeight - 1) div FTextHeight,
    1, DisplayLineCount);

  // Now paint everything while the caret is hidden.
  HideCaret;
  try
    // First paint the gutter area if it was (partly) invalidated.
    if (rcClip.Left < FGutterWidth) then
    begin
      rcDraw := rcClip;
      rcDraw.Right := FGutterWidth;
      PaintGutter(rcDraw, nL1, nL2);
    end;
    // Then paint the text area if it was (partly) invalidated.
    if (rcClip.Right > FGutterWidth) then
    begin
      rcDraw := rcClip;
      rcDraw.Left := Max(rcDraw.Left, FGutterWidth);
      PaintTextLines(rcDraw, nL1, nL2, nC1, nC2);
    end;

    // consider paint lock (inserted by CWBudde, 30th of July 2015)
    if PaintLock = 0 then
      PluginsAfterPaint(Canvas, rcClip, nL1, nL2);
    // If there is a custom paint handler call it.
    if PaintLock = 0 then
      DoOnPaint;
    if PaintLock = 0 then
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
    if (not aMark.InternalImage) and Assigned(FBookMarkOpt.BookmarkImages) then
    begin
      if aMark.ImageIndex <= FBookMarkOpt.BookmarkImages.Count then
      begin
        if aMark.IsBookmark = BookMarkOptions.DrawBookmarksFirst then
          aGutterOff := 0
        else if aGutterOff = 0 then
          aGutterOff := FBookMarkOpt.XOffset;
        with FBookMarkOpt do
          BookmarkImages.Draw(Canvas, LeftMargin + aGutterOff,
            (aMarkRow - TopLine) * FTextHeight, aMark.ImageIndex);
        Inc(aGutterOff, FBookMarkOpt.XOffset);
      end;
    end
    else begin
      if aMark.ImageIndex in [0..9] then
      begin
        if not Assigned(FInternalImage) then
        begin
          FInternalImage := TSynInternalImage.Create(HINSTANCE,
            'SynEditInternalImages', 10);
//++ DPI-Aware
          if Screen.PixelsPerInch >= 120 then
            fInternalImage.ChangeScale(Screen.PixelsPerInch, 96);
//-- DPI-Aware
        end;
        if aGutterOff = 0 then
        begin
          FInternalImage.Draw(Canvas, aMark.ImageIndex,
            FBookMarkOpt.LeftMargin + aGutterOff,
            (aMarkRow - TopLine) * FTextHeight, FTextHeight);
        end;
        Inc(aGutterOff, FBookMarkOpt.XOffset);
      end;
    end;
  end;

  procedure DrawModification(Color: TColor; Top, Bottom: Integer);
  var
    OldColor: TColor;
    OldStyle: TBrushStyle;
  begin
    FTextDrawer.SetBackColor(Color);

    OldStyle := Canvas.Brush.Style;
    OldColor := Canvas.Brush.Color;

    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Color;

    Canvas.FillRect(Rect(FGutterWidth - FGutter.RightOffset - FGutter.ModificationBarWidth, Top,
      FGutterWidth - FGutter.RightOffset, Bottom));

    Canvas.Brush.Style := OldStyle;
    Canvas.Brush.Color := OldColor;

    FTextDrawer.SetBackColor(FGutter.Color);
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
  vTextOffset: Integer;
  dc: HDC;
  TextSize: TSize;
{$IFDEF SYN_CodeFolding}
  vLine: Integer;
  cRow : Integer;
  rcFold: TRect;
  x: Integer;
  FoldRange: TSynFoldRange;
  Index : Integer;
{$ENDIF}
begin
  vFirstLine := RowToLine(aFirstRow);
  vLastLine := RowToLine(aLastRow);
  //todo: Does the following comment still apply?
  // Changed to use FTextDrawer.BeginDrawing and FTextDrawer.EndDrawing only
  // when absolutely necessary.  Note: Never change brush / pen / font of the
  // canvas inside of this block (only through methods of FTextDrawer)!
  // If we have to draw the line numbers then we don't want to erase
  // the background first. Do it line by line with TextRect instead
  // and fill only the area after the last visible line.
  dc := Canvas.Handle;

  if FGutter.Gradient then
    SynDrawGradient(Canvas, FGutter.GradientStartColor, FGutter.GradientEndColor,
      FGutter.GradientSteps, Rect(0, 0, FGutterWidth, ClientHeight), True);

  Canvas.Brush.Color := FGutter.Color;

  if FGutter.ShowLineNumbers then
  begin
    if FGutter.UseFontStyle then
      FTextDrawer.SetBaseFont(FGutter.Font)
    else
      FTextDrawer.Style := [];
    FTextDrawer.BeginDrawing(dc);
    try
      if FGutter.UseFontStyle then
        FTextDrawer.SetForeColor(FGutter.Font.Color)
      else
        FTextDrawer.SetForeColor(Self.Font.Color);
      FTextDrawer.SetBackColor(FGutter.Color);

      // prepare the rect initially
      rcLine := AClip;
      rcLine.Right := Max(rcLine.Right, FGutterWidth - 2);
      rcLine.Bottom := rcLine.Top;

      for cLine := vFirstLine to vLastLine do
      begin
{$IFDEF SYN_CodeFolding}
        if UseCodeFolding and AllFoldRanges.FoldHidesLine(cLine, Index) then
          continue;
{$ENDIF}
        vLineTop := (LineToRow(cLine) - TopLine) * FTextHeight;
        if WordWrap and not FGutter.Gradient then
        begin
          // erase space between wrapped lines (from previous line to current one)
          rcLine.Top := rcLine.Bottom;
          rcLine.Bottom := vLineTop;
          with rcLine do
            FTextDrawer.ExtTextOut(Left, Top, [tooOpaque], rcLine, '', 0);
        end;
        // next line rect
        rcLine.Top := vLineTop;
        rcLine.Bottom := rcLine.Top + FTextHeight;

        s := FGutter.FormatLineNumber(cLine);
        if Assigned(OnGutterGetText) then
          OnGutterGetText(Self, cLine, s);
        TextSize := GetTextSize(DC, PWideChar(s), Length(s));
        vTextOffset := (FGutterWidth - FGutter.RightOffset - FGutter.RightMargin) - TextSize.cx;
        if FGutter.ShowModification then
          vTextOffset := vTextOffset - FGutter.ModificationBarWidth;

        if FGutter.Gradient then
        begin
          SetBkMode(DC, TRANSPARENT);
          Windows.ExtTextOutW(DC, vTextOffset,
            rcLine.Top + ((FTextHeight - Integer(TextSize.cy)) div 2), 0,
            @rcLine, PWideChar(s), Length(s), nil);
          SetBkMode(DC, OPAQUE);
        end
        else
          Windows.ExtTextOutW(DC, vTextOffset,
            rcLine.Top + ((FTextHeight - Integer(TextSize.cy)) div 2), ETO_OPAQUE,
            @rcLine, PWideChar(s), Length(s), nil);

        // eventually draw modifications
        if FGutter.ShowModification then
          case TSynEditStringList(FLines).Modification[cLine - 1] of
            smModified:
              DrawModification(FGutter.ModificationColorModified, rcLine.Top, rcLine.Bottom);
            smSaved:
              DrawModification(FGutter.ModificationColorSaved, rcLine.Top, rcLine.Bottom);
          end;
      end;
      // now erase the remaining area if any
      if (AClip.Bottom > rcLine.Bottom) and not FGutter.Gradient then
      begin
        rcLine.Top := rcLine.Bottom;
        rcLine.Bottom := AClip.Bottom;
        with rcLine do
          FTextDrawer.ExtTextOut(Left, Top, [tooOpaque], rcLine, '', 0);
      end;
    finally
      FTextDrawer.EndDrawing;
      if FGutter.UseFontStyle then
        FTextDrawer.SetBaseFont(Self.Font);
    end;
  end
  else
  begin
    if not FGutter.Gradient then
      Canvas.FillRect(AClip);

    if FGutter.ShowModification then
      for cLine := vFirstLine to vLastLine do
      begin
        vLineTop := (LineToRow(cLine) - TopLine) * FTextHeight;
        case TSynEditStringList(FLines).Modification[cLine - 1] of
          smModified:
            DrawModification(FGutter.ModificationColorModified, vLineTop, vLineTop + fTextHeight);
          smSaved:
            DrawModification(FGutter.ModificationColorSaved, vLineTop, vLineTop + fTextHeight);
        end;
      end;
  end;

{$IFDEF MSWINDOWS}
  // draw Word wrap glyphs transparently over gradient
  if FGutter.Gradient then
    Canvas.Brush.Style := bsClear;
{$ENDIF}
  // paint wrapped line glyphs
  if WordWrap and FWordWrapGlyph.Visible then
    for cLine := aFirstRow to aLastRow do
      if LineToRow(RowToLine(cLine)) <> cLine then
        FWordWrapGlyph.Draw(Canvas,
                            (FGutterWidth - FGutter.RightOffset - 2) - FWordWrapGlyph.Width,
                            (cLine - TopLine) * FTextHeight, FTextHeight);
{$IFDEF MSWINDOWS}
  // restore brush
  if FGutter.Gradient then
    Canvas.Brush.Style := bsSolid;
{$ENDIF}

{$IFDEF SYN_CodeFolding}
  // Draw the folding lines and squares
  if UseCodeFolding then begin
    for cRow := aFirstRow to aLastRow do begin
      vLine := RowToLine(cRow);
      if (vLine > Lines.Count) and not (Lines.Count = 0) then
        break;

      rcFold := GetFoldShapeRect(cRow);

      Canvas.Pen.Color := fCodeFolding.FolderBarLinesColor;

      // Any fold ranges beginning on this line?
      if AllFoldRanges.FoldStartAtLine(vLine, Index) then begin
        FoldRange := AllFoldRanges.Ranges[Index];
        Canvas.Brush.Color := fCodeFolding.FolderBarLinesColor;
        Canvas.FrameRect(rcFold);

        // Paint minus sign
        Canvas.Pen.Color := fCodeFolding.FolderBarLinesColor;
        Canvas.MoveTo(rcFold.Left + 2, rcFold.Top + ((rcFold.Bottom - rcFold.Top) div 2));
        Canvas.LineTo(rcFold.Right - 2, rcFold.Top + ((rcFold.Bottom - rcFold.Top) div 2));

        // Paint vertical line of plus sign
        if FoldRange.Collapsed then begin
          x := rcFold.Left + ((rcFold.Right - rcFold.Left) div 2);
          Canvas.MoveTo(x, rcFold.Top + 2);
          Canvas.LineTo(x, rcFold.Bottom - 2);
        end
        else
        // Draw the bottom part of a line
        begin
          x := rcFold.Left + ((rcFold.Right - rcFold.Left) div 2);
          Canvas.MoveTo(x, rcFold.Bottom);
          Canvas.LineTo(x, (cRow - fTopLine + 1) * LineHeight);
        end;
      end
      else begin
        // Need to paint a line end?
        if AllFoldRanges.FoldEndAtLine(vLine, Index) then begin
          x := rcFold.Left + ((rcFold.Right - rcFold.Left) div 2);
          Canvas.MoveTo(x,  (cRow - fTopLine) * LineHeight);
          Canvas.LineTo(x, rcFold.Top + ((rcFold.Bottom - rcFold.Top) div 2));
          Canvas.LineTo(rcFold.Right, rcFold.Top + ((rcFold.Bottom - rcFold.Top) div 2));
        end;
        // Need to paint a line?
        if AllFoldRanges.FoldAroundLine(vLine, Index) then begin
          x := rcFold.Left + ((rcFold.Right - rcFold.Left) div 2);
          Canvas.MoveTo(x, (cRow - fTopLine) * LineHeight);
          Canvas.LineTo(x, (cRow - fTopLine + 1) * LineHeight);
        end;
      end;
    end;
  end;
{$ENDIF}

  // the gutter separator if visible
  if (FGutter.BorderStyle <> gbsNone) and (AClip.Right >= FGutterWidth - 2) then
    with Canvas do
    begin
      Pen.Color := FGutter.BorderColor;
      Pen.Width := 1;
      with AClip do
      begin
        if FGutter.BorderStyle = gbsMiddle then
        begin
          MoveTo(FGutterWidth - 2, Top);
          LineTo(FGutterWidth - 2, Bottom);
          Pen.Color := FGutter.Color;
        end;
        MoveTo(FGutterWidth - 1, Top);
        LineTo(FGutterWidth - 1, Bottom);
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
{$IFDEF SYN_CodeFolding}
        if Visible and (Line >= vFirstLine) and (Line <= vLastLine) and (Line <= FLines.Count)
           and not (UseCodeFolding and AllFoldRanges.FoldHidesLine(Line, Index))
        then
{$ELSE}
        if Visible and (Line >= vFirstLine) and (Line <= vLastLine) then
{$ENDIF}
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
{$IFDEF SYN_CodeFolding}
            and (Line >= vFirstLine) and (Line <= vLastLine) and (Line <= FLines.Count)
            and not (UseCodeFolding and AllFoldRanges.FoldHidesLine(Line, Index))  then
{$ELSE}
            and (Line >= vFirstLine) and (Line <= vLastLine) then
{$ENDIF}
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
    Inc(j);
    CountOfAvgGlyphs := CeilOfIntDiv(FTextDrawer.TextWidth(S[i]), FCharWidth);

    if j + CountOfAvgGlyphs > Length(Result) then
      SetLength(Result, Length(Result) + 128);

    // insert CountOfAvgGlyphs filling chars
    while CountOfAvgGlyphs > 1 do
    begin
      Result[j] := FillerChar;
      Inc(j);
      Dec(CountOfAvgGlyphs);
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
    Inc(j);
    while S[i] = FillerChar do
      Inc(i);
    Result[j] := S[i];
    Inc(i);
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
  dc: HDC;
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
      if FBlockBegin.Line < FBlockEnd.Line then
      begin
        vStart := FBlockBegin;
        vEnd := FBlockEnd;
      end
      else if FBlockBegin.Line > FBlockEnd.Line then
      begin
        vEnd := FBlockBegin;
        vStart := FBlockEnd;
      end
      else if FBlockBegin.Char <> FBlockEnd.Char then
      begin
        // No selection at all, or it is only on this line.
        vStart.Line := FBlockBegin.Line;
        vEnd.Line := vStart.Line;
        if FBlockBegin.Char < FBlockEnd.Char then
        begin
          vStart.Char := FBlockBegin.Char;
          vEnd.Char := FBlockEnd.Char;
        end
        else
        begin
          vStart.Char := FBlockEnd.Char;
          vEnd.Char := FBlockBegin.Char;
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
          if (FActiveSelectionMode = smColumn) and (vSelStart.Column > vSelEnd.Column) then
            SwapInt(vSelStart.Column, vSelEnd.Column);
        end;
      end;
    end;
  end;

  procedure SetDrawingColors(Selected: Boolean);
  begin
    with FTextDrawer do
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
    Result := FTextOffset + Pred(Col) * FCharWidth;
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
        if FShowSpecChar and (Token[Counter] = #32) then
          Token[Counter] := SynSpaceGlyph
        else if Token[Counter] = #9 then
        begin
          Token[Counter] := #32;  //Tabs painted differently if necessary
          DoTabPainting := FShowSpecChar;
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
          Inc(FillerCount);
          Inc(NonFillerPos);
        end;

        CountOfAvgGlyphs := CeilOfIntDiv(FTextDrawer.TextWidth(Token[NonFillerPos]) , FCharWidth);

        // first visible part of the glyph (1-based)
        // (the glyph is visually sectioned in parts of size FCharWidth)
        VisibleGlyphPart := CountOfAvgGlyphs - FillerCount;

        // clip off invisible parts
        nX := nX - FCharWidth * (VisibleGlyphPart - 1);

        nCharsToPaint := Min(Last - First + 1, TokenLen - First + 1);

        // clip off partially visible glyphs at line end
        if WordWrap then
          while nX + FCharWidth * nCharsToPaint > ClientWidth do
          begin
            Dec(nCharsToPaint);
            while (nCharsToPaint > 0) and (Token[First + nCharsToPaint - 1] = FillerChar) do
              Dec(nCharsToPaint);
          end;

        // same as copy(Token, First, nCharsToPaint) and remove filler chars
        Text := ShrinkAtWideGlyphs(Token, First, nCharsToPaint);
      end;

      FTextDrawer.ExtTextOut(nX, rcToken.Top, ETOOptions, rcToken,
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
          while (TabStart + CharsBefore + TabLen - 1) mod FTabWidth <> 0 do Inc(TabLen);
          Text := SynTabGlyphString;

          nX := ColumnToXValue(CharsBefore + TabStart + (TabLen div 2) - 1);
          if TabLen mod 2 = 0 then
            nX := nX + (FCharWidth div 2)
          else nX := nX + FCharWidth;

          rcTab.Left := nX;
          rcTab.Right := nX + FTextDrawer.GetCharWidth;

          FTextDrawer.ExtTextOut(nX, rcTab.Top, ETOOptions, rcTab,
            PWideChar(Text), 1);

          for i := 0 to TabLen - 1 do           //wipe the text out so we don't
            sTabbedToken[TabStart + i] := #32;  //count it again

          TabStart := pos(#9, sTabbedToken);
        end;
      end;
      rcToken.Left := rcToken.Right;
    end;
  end;

  procedure AdjustEndRect;
  // trick to avoid clipping the last pixels of text in italic,
  // see also AdjustLastCharWidth() in TSynTextDrawer.ExtTextOut()
  var
    LastChar: Cardinal;
    NormalCharWidth, RealCharWidth: Integer;
    CharInfo: TABC;
    tm: TTextMetricA;
  begin
    LastChar := Ord(TokenAccu.s[TokenAccu.Len]);
    NormalCharWidth := FTextDrawer.TextWidth(WideChar(LastChar));
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

      if bSpecialLine and (eoSpecialLineDefaultFg in FOptions) then
        colFG := TokenAccu.FG;

      FTextDrawer.SetStyle(TokenAccu.Style);
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
          if (TokenAccu.Len > 0) and (TokenAccu.Style <> []) then
            AdjustEndRect;
          Canvas.FillRect(rcToken);
          rcToken.Left := nX1;
        end;
        if (rcToken.Left < nX2) then
        begin
          SetDrawingColors(True);
          rcToken.Right := nX2;
          if (TokenAccu.Len > 0) and (TokenAccu.Style <> []) then
            AdjustEndRect;
          Canvas.FillRect(rcToken);
          rcToken.Left := nX2;
        end;
        if (rcToken.Left < rcLine.Right) then
        begin
          SetDrawingColors(False);
          rcToken.Right := rcLine.Right;
          if (TokenAccu.Len > 0) and (TokenAccu.Style <> []) then
            AdjustEndRect;
          Canvas.FillRect(rcToken);
        end;
      end
      else
      begin
        SetDrawingColors(bLineSelected);
        rcToken.Right := rcLine.Right;
        if (TokenAccu.Len > 0) and (TokenAccu.Style <> []) then
          AdjustEndRect;
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
            Break;
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
        if (bSpecialLine and not (eoSpecialLineDefaultFg in FOptions)) or bLineSelected or
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

{$IFDEF SYN_CodeFolding}
  procedure PaintFoldAttributes;
  var
    i, TabSteps, LineIndent, LastNonBlank, X, Y, cRow, vLine: Integer;
    DottedPen, OldPen: HPEN;
    DottedPenDesc: LOGBRUSH;
    CollapsedTo : integer;
    HintRect : TRect;
  begin
    // Paint indent guides. Use folds to determine indent value of these
    // Use a separate loop so we can use a custom pen
    if not UseCodeFolding then
      Exit;

    // Paint indent guides using custom pen
    if fCodeFolding.IndentGuides then begin
      DottedPenDesc.lbStyle := BS_SOLID;
      DottedPenDesc.lbColor := fCodeFolding.IndentGuidesColor;
      DottedPen := ExtCreatePen(PS_COSMETIC or PS_ALTERNATE, 1, DottedPenDesc, 0, nil);
      try
        OldPen := SelectObject(Canvas.Handle, DottedPen);

        // Now loop through all the lines. The indices are valid for Lines.
        for cRow := aFirstRow to aLastRow do begin
          vLine := RowToLine(cRow);
          if (vLine > Lines.Count) and not (Lines.Count = 0) then
            break;

          // Set vertical coord
          Y := (LineToRow(vLine) - TopLine) * fTextHeight; // limit inside clip rect
          if (fTextHeight mod 2 = 1) and (vLine mod 2 = 0) then // even
            Inc(Y);

        // Get next nonblank line
          LastNonBlank := cRow;
          while (RowToLine(LastNonBlank) <= fLines.Count)
            and (TrimLeft(fLines[RowToLine(LastNonBlank)-1]) = '') do
            Inc(LastNonBlank);
          LineIndent := LeftSpacesEx(fLines[RowToLine(LastNonBlank)-1], True, True);

   // Step horizontal coord
          TabSteps := TabWidth;
          while TabSteps < LineIndent do begin
            X := TabSteps * CharWidth + fTextOffset - 2;
            if TabSteps >= fLeftChar then begin
              // Move to top of vertical line
              Canvas.MoveTo(X, Y);
              Inc(Y, fTextHeight);

              // Draw down and move back up
              Canvas.LineTo(X, Y);
              Dec(Y, fTextHeight);
            end;
            Inc(TabSteps, TabWidth);
          end;
        end;

        // Reset pen
        SelectObject(Canvas.Handle, OldPen);
      finally
        DeleteObject(DottedPen);
      end;
    end;

    // Paint collapsed lines using changed pen
    if fCodeFolding.ShowCollapsedLine or fCodeFolding.ShowHintMark then begin
      Canvas.Pen.Color := fCodeFolding.CollapsedLineColor;

      CollapsedTo := 0;
      for i := 0 to fAllFoldRanges.Count - 1 do begin
        with fAllFoldRanges.Ranges[i] do begin
          if FromLine > vLastLine then
            break;
          if Collapsed and (FromLine > CollapsedTo) and (FromLine >= vFirstLine) then
          begin
            if fCodeFolding.ShowCollapsedLine then
            begin
              // Get starting and end points
              Y := (LineToRow(FromLine) - TopLine + 1) * fTextHeight - 1;
              Canvas.MoveTo(AClip.Left, Y);
              Canvas.LineTo(AClip.Right, Y);
            end;
            if fCodeFolding.ShowHintMark then
            begin
              HintRect := GetCollapseMarkRect(LineToRow(FromLine), FromLine);
              if InRange(HintRect.Left, 1, ClientWidth-1) then
              begin
                fTextDrawer.BeginDrawing(Canvas.Handle);
                SetBkMode(Canvas.Handle, TRANSPARENT);
                fTextDrawer.SetForeColor(fCodeFolding.CollapsedLineColor);
                with HintRect do
                  ftextDrawer.ExtTextOut(Left + 2 * CharWidth div 7,
                    Top - LineHeight div 5, [], HintRect, '...', 3);
                SetBkMode(Canvas.Handle, OPAQUE);
                Canvas.Pen.Width := IfThen(LineHeight > 30, 2, 1);
                Canvas.Brush.Style := bsClear;
                Inc(HintRect.Top, LineHeight div 7);
                Canvas.Rectangle(HintRect);
                Canvas.Brush.Style := bsSolid;
                Canvas.Pen.Width := 1;
                fTextDrawer.EndDrawing;
              end;
            end;
          end;
          if Collapsed then
            CollapsedTo := Max(CollapsedTo, ToLine);
        end;
      end;
    end;
  end;
{$ENDIF}

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
    TokenFG, TokenBG: TColor;
    TokenStyle: TFontStyles;
  begin
    // Initialize rcLine for drawing. Note that Top and Bottom are updated
    // inside the loop. Get only the starting point for this.
    rcLine := AClip;
    rcLine.Left := FGutterWidth + 2;
    rcLine.Bottom := (aFirstRow - TopLine) * FTextHeight;
    // Make sure the token accumulator string doesn't get reassigned to often.
    if Assigned(FHighlighter) then
    begin
      TokenAccu.MaxLen := Max(128, FCharsInWindow);
      SetLength(TokenAccu.s, TokenAccu.MaxLen);
    end;
    // Now loop through all the lines. The indices are valid for Lines.
    for nLine := vFirstLine to vLastLine do
    begin
{$IFDEF SYN_CodeFolding}
      if UseCodeFolding and AllFoldRanges.FoldHidesLine(nLine) then
        continue;
{$ENDIF}
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
        colSelFG := FSelectedColor.Foreground;
        colSelBG := FSelectedColor.Background;
      end;

      vStartRow := Max(LineToRow(nLine), aFirstRow);
      vEndRow := Min(LineToRow(nLine + 1) - 1, aLastRow);
{$IFDEF SYN_CodeFolding}
      vEndRow := Max(vEndRow, vStartRow);
{$ENDIF}
      for cRow := vStartRow to vEndRow do
      begin
        if WordWrap then
        begin
          vAuxPos.Row := cRow;
          if Assigned(FHighlighter) then
            vAuxPos.Column := FirstCol
          else
            // When no highlighter is assigned, we must always start from the
            // first char in a row and PaintToken will do the actual clipping
            vAuxPos.Column := 1;
          vFirstChar := FWordWrapPlugin.DisplayToBufferPos(vAuxPos).Char;
          vAuxPos.Column := LastCol;
          vLastChar := FWordWrapPlugin.DisplayToBufferPos(vAuxPos).Char;
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
          if (FActiveSelectionMode = smColumn) or
            ((FActiveSelectionMode = smNormal) and (cRow = vSelStart.Row)) then
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
          if (FActiveSelectionMode = smColumn) or
            ((FActiveSelectionMode = smNormal) and (cRow = vSelEnd.Row)) then
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
        Inc(rcLine.Bottom, FTextHeight);

        bLineSelected := not bComplexLine and (nLineSelStart > 0);
        rcToken := rcLine;

        if not Assigned(FHighlighter) or not FHighlighter.Enabled then
        begin
          // Remove text already displayed (in previous rows)
          if (vFirstChar <> FirstCol) or (vLastChar <> LastCol) then
            sToken := Copy(sLineExpandedAtWideGlyphs, vFirstChar, vLastChar - vFirstChar)
          else
            sToken := Copy(sLineExpandedAtWideGlyphs, 1, vLastChar);
          if FShowSpecChar and (Length(sLineExpandedAtWideGlyphs) < vLastChar) then
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
            FHighlighter.ResetRange
          else if fSingleLineMode then
            fHighlighter.ResetRange
          else
            FHighlighter.SetRange(TSynEditStringList(Lines).Ranges[nLine - 2]);
          FHighlighter.SetLineExpandedAtWideGlyphs(sLine, sLineExpandedAtWideGlyphs,
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
          while not FHighlighter.GetEol do
          begin
            nTokenPos := FHighlighter.GetExpandedTokenPos;
            sToken := FHighlighter.GetExpandedToken;
            nTokenLen := Length(sToken);
            if nTokenPos + nTokenLen >= vFirstChar then
            begin
              if nTokenPos + nTokenLen > vLastChar then
              begin
                if nTokenPos > vLastChar then
                  Break;
                if WordWrap then
                  nTokenLen := vLastChar - nTokenPos - 1
                else
                  nTokenLen := vLastChar - nTokenPos;
              end;
              // Remove offset generated by tokens already displayed (in previous rows)
              Dec(nTokenPos, vFirstChar - FirstCol);
              // It's at least partially visible. Get the token attributes now.
              attr := FHighlighter.GetTokenAttribute;
              if Assigned(attr) then
              begin
                TokenFG := attr.Foreground;
                TokenBG := attr.Background;
                TokenStyle := attr.Style;
              end
              else
              begin
                TokenFG := colFG;
                TokenBG := colBG;
                TokenStyle := Font.Style;
              end;
              DoOnSpecialTokenAttributes(nLine, nTokenPos, sToken, TokenFG, TokenBG, TokenStyle);
              AddHighlightToken(sToken, nTokenPos, nTokenLen, TokenFG, TokenBG, TokenStyle);
            end;
            // Let the highlighter scan the next token.
            FHighlighter.Next;
          end;
          // Draw anything that's left in the TokenAccu record. Fill to the end
          // of the invalid area with the correct colors.
          if FShowSpecChar and FHighlighter.GetEol then
          begin
            if (attr = nil) or (attr <> FHighlighter.CommentAttribute) then
               attr := FHighlighter.WhitespaceAttribute;
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
  if (FRightEdge > 0) then
  begin // column value
    nRightEdge := FTextOffset + FRightEdge * FCharWidth; // pixel value
    if (nRightEdge >= AClip.Left) and (nRightEdge <= AClip.Right) then
    begin
      bDoRightEdge := True;
      Canvas.Pen.Color := FRightEdgeColor;
      Canvas.Pen.Width := 1;
    end;
  end;
  // Do everything else with API calls. This (maybe) realizes the new pen color.
  dc := Canvas.Handle;

  // If anything of the two pixel space before the text area is visible, then
  // fill it with the component background color.
  if (AClip.Left < FGutterWidth + 2) then
  begin
    rcToken := AClip;
    rcToken.Left := Max(AClip.Left, FGutterWidth);
    rcToken.Right := FGutterWidth + 2;
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
    FTextDrawer.Style := Font.Style;
    FTextDrawer.BeginDrawing(dc);
    try
      PaintLines;
    finally
      FTextDrawer.EndDrawing;
    end;
  end;
  // If there is anything visible below the last line, then fill this as well.
  rcToken := AClip;
  rcToken.Top := (aLastRow - TopLine + 1) * FTextHeight;
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

  {$IFDEF SYN_CodeFolding}
  // This messes with pen colors, so draw after right margin has been drawn
  PaintFoldAttributes;
{$ENDIF}
end;

procedure TCustomSynEdit.PasteFromClipboard;
var
  AddPasteEndMarker: Boolean;
  vStartOfBlock: TBufferCoord;
  vEndOfBlock: TBufferCoord;
  StoredPaintLock: Integer;
  PasteMode: TSynSelectionMode;
  Mem: HGLOBAL;
  P: PByte;
begin
  if not CanPaste then
    Exit;
  DoOnPaintTransient(ttBefore);
  BeginUndoBlock;
  AddPasteEndMarker := False;
  PasteMode := SelectionMode;
  try
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

    FUndoList.AddChange(crPasteBegin, BlockBegin, BlockEnd, '', smNormal);
    AddPasteEndMarker := True;
    if SelAvail then
    begin
      FUndoList.AddChange(crDelete, FBlockBegin, FBlockEnd, SelText,
        FActiveSelectionMode);
    end
    else
      ActiveSelectionMode := SelectionMode;

    if SelAvail then
    begin
      vStartOfBlock := BlockBegin;
      vEndOfBlock := BlockEnd;
      FBlockBegin := vStartOfBlock;
      FBlockEnd := vEndOfBlock;

      // Pasting always occurs at column 0 when current selection is
      // smLine type
      if FActiveSelectionMode = smLine then
        vStartOfBlock.Char := 1;
    end
    else
      vStartOfBlock := CaretXY;

    Inc(FPaintTransientLock);
    SetSelTextPrimitiveEx(PasteMode, PWideChar(GetClipboardText), True);
    Dec(FPaintTransientLock);
    vEndOfBlock := BlockEnd;
    if PasteMode = smNormal then
      FUndoList.AddChange(crPaste, vStartOfBlock, vEndOfBlock, SelText,
        PasteMode)
    else if PasteMode = smColumn then
      // Do nothing. Moved to InsertColumn
    else if PasteMode = smLine then
      if CaretX = 1 then
        FUndoList.AddChange(crPaste, BufferCoord(1, vStartOfBlock.Line),
          BufferCoord(CharsInWindow, vEndOfBlock.Line - 1), SelText, smLine)
      else
        FUndoList.AddChange(crPaste, BufferCoord(1, vStartOfBlock.Line),
          vEndOfBlock, SelText, smNormal);
  finally
    if AddPasteEndMarker then
      FUndoList.AddChange(crPasteEnd, BlockBegin, BlockEnd, '', smNormal);
    EndUndoBlock;
  end;

  // ClientRect can be changed by UpdateScrollBars if eoHideShowScrollBars
  // is enabled
  if eoHideShowScrollBars in Options then
  begin
    StoredPaintLock := FPaintLock;
    try
      FPaintLock := 0;
      UpdateScrollBars;
    finally
      FPaintLock := StoredPaintLock;
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
    Value.Char := MinMax(Value.Char, 1, FMaxScrollWidth + 1)
  else
    Value.Char := Max(Value.Char, 1);
  Value.Line := MinMax(Value.Line, 1, Lines.Count);
  if (FActiveSelectionMode = smNormal) then
    if (Value.Line >= 1) and (Value.Line <= Lines.Count) then
      Value.Char := Min(Value.Char, Length(Lines[Value.Line - 1]) + 1)
    else
      Value.Char := 1;
  if SelAvail then
  begin
    if FBlockBegin.Line < FBlockEnd.Line then
    begin
      nInval1 := Min(Value.Line, FBlockBegin.Line);
      nInval2 := Max(Value.Line, FBlockEnd.Line);
    end
    else
    begin
      nInval1 := Min(Value.Line, FBlockEnd.Line);
      nInval2 := Max(Value.Line, FBlockBegin.Line);
    end;
    FBlockBegin := Value;
    FBlockEnd := Value;
    InvalidateLines(nInval1, nInval2);
    SelChanged := True;
  end
  else
  begin
    SelChanged :=
      (FBlockBegin.Char <> Value.Char) or (FBlockBegin.Line <> Value.Line) or
      (FBlockEnd.Char <> Value.Char) or (FBlockEnd.Line <> Value.Line);
    FBlockBegin := Value;
    FBlockEnd := Value;
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
      Value.Char := MinMax(Value.Char, 1, FMaxScrollWidth + 1)
    else
      Value.Char := Max(Value.Char, 1);
    Value.Line := MinMax(Value.Line, 1, Lines.Count);
    if (FActiveSelectionMode = smNormal) then
      if (Value.Line >= 1) and (Value.Line <= Lines.Count) then
        Value.Char := Min(Value.Char, Length(Lines[Value.Line - 1]) + 1)
      else
        Value.Char := 1;
    if (Value.Char <> FBlockEnd.Char) or (Value.Line <> FBlockEnd.Line) then
    begin
      if (Value.Char <> FBlockEnd.Char) or (Value.Line <> FBlockEnd.Line) then
      begin
        if (FActiveSelectionMode = smColumn) and (Value.Char <> FBlockEnd.Char) then
        begin
          InvalidateLines(
            Min(FBlockBegin.Line, Min(FBlockEnd.Line, Value.Line)),
            Max(FBlockBegin.Line, Max(FBlockEnd.Line, Value.Line)));
          FBlockEnd := Value;
        end
        else begin
          nLine := FBlockEnd.Line;
          FBlockEnd := Value;
          if (FActiveSelectionMode <> smColumn) or (FBlockBegin.Char <> FBlockEnd.Char) then
            InvalidateLines(nLine, FBlockEnd.Line);
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
{$IFDEF SYN_CodeFolding}
  if not WordWrap and not UseCodeFolding then
{$ELSE}
  if not WordWrap then
{$ENDIF}
    Result := CaretY
  else
    Result := DisplayXY.Row;
end;

Function TCustomSynEdit.GetDisplayXY: TDisplayCoord;
begin
  Result := BufferToDisplayPos(CaretXY);
  if WordWrap and FCaretAtEOL then
  begin
    if Result.Column = 1 then
    begin
      Dec(Result.Row);
      Result.Column := FWordWrapPlugin.GetRowLength(Result.Row) +1;
    end
    else begin
      // Work-around situations where FCaretAtEOL should have been updated because of
      //text change (it's only valid when Column = 1). Updating it in ProperSetLine()
      //would probably be the right thing, but...
      FCaretAtEOL := False;
    end;
  end;
end;

procedure TCustomSynEdit.SetCaretXY(const Value: TBufferCoord);
//there are two setCaretXY methods.  One Internal, one External.  The published
//property CaretXY (re)sets the block as well
begin
  IncPaintLock;
  try
    Include(FStatusChanges, scSelection);
    SetCaretXYEx(True, Value);
    if SelAvail then
      InvalidateSelection;
    FBlockBegin.Char := FCaretX;
    FBlockBegin.Line := FCaretY;
    FBlockEnd := FBlockBegin;
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
  FLastCaretX := DisplayX;
end;

procedure TCustomSynEdit.SetCaretXYEx(CallEnsureCursorPos: Boolean; Value: TBufferCoord);
var
  nMaxX: Integer;
  vTriggerPaint: Boolean;
begin
  FCaretAtEOL := False;
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
    if not (eoScrollPastEol in FOptions) then
      nMaxX := 1;
  end
  else
  begin
    if not (eoScrollPastEol in FOptions) then
      nMaxX := Length(Lines[Value.Line - 1]) + 1;
  end;
  if (Value.Char > nMaxX) and (not(eoScrollPastEol in Options) or
    not(eoAutoSizeMaxScrollWidth in Options)) then
  begin
    Value.Char := nMaxX;
  end;
  if Value.Char < 1 then
    Value.Char := 1;
  if (Value.Char <> FCaretX) or (Value.Line <> FCaretY) then
  begin
    IncPaintLock;
    try
      // simply include the flags, FPaintLock is > 0
      if FCaretX <> Value.Char then
      begin
        FCaretX := Value.Char;
        Include(FStatusChanges, scCaretX);
      end;
      if FCaretY <> Value.Line then
      begin
        if ActiveLineColor <> clNone then
        begin
          InvalidateLine(Value.Line);
          InvalidateLine(FCaretY);
        end;
        FCaretY := Value.Line;
        Include(FStatusChanges, scCaretY);
{$IFDEF SYN_CodeFolding}
        UncollapseAroundLine(fCaretY);
{$ENDIF}
      end;
      // Call UpdateLastCaretX before DecPaintLock because the event handler it
      // calls could raise an exception, and we don't want fLastCaretX to be
      // left in an undefined state if that happens.
      UpdateLastCaretX;
      if CallEnsureCursorPos then
        EnsureCursorPosVisible;
      Include(FStateFlags, sfCaretChanged);
//++ Flicker Reduction
//      Include(fStateFlags, sfScrollbarChanged);
//-- Flicker Reduction
    finally
      DecPaintLock;
    end;
  end
  else begin
    // Also call UpdateLastCaretX if the caret didn't move. Apps don't know
    // anything about FLastCaretX and they shouldn't need to. So, to avoid any
    // unwanted surprises, always update FLastCaretX whenever CaretXY is
    // assigned to.
    // Note to SynEdit developers: If this is undesirable in some obscure
    // case, just save the value of FLastCaretX before assigning to CaretXY and
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
  if (FActiveLineColor <> Value) then
  begin
    FActiveLineColor := Value;
    InvalidateLine(CaretY);
  end;
end;

procedure TCustomSynEdit.SetFont(const Value: TFont);
var
  DC: HDC;
  Save: THandle;
  Metrics: TTextMetric;
  AveCW, MaxCW: Integer;
begin
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
        with FFontDummy do
        begin
          Color := Value.Color;
          Pitch := fpFixed;
          Size := Value.Size;
          Style := Value.Style;
          Name := Value.Name;
        end;
        inherited Font := FFontDummy;
      end;
  end;

  TSynEditStringList(FLines).FontChanged;
  if FGutter.ShowLineNumbers then
    GutterChanged(Self);
end;

procedure TCustomSynEdit.SetGutterWidth(Value: Integer);
begin
  Value := Max(Value, 0);
  if FGutterWidth <> Value then
  begin
    FGutterWidth := Value;
    FTextOffset := FGutterWidth + 2 - (LeftChar - 1) * FCharWidth;
    if HandleAllocated then
    begin
      FCharsInWindow := Max(ClientWidth - FGutterWidth - 2, 0) div FCharWidth;
      if WordWrap then
        FWordWrapPlugin.DisplayChanged;
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
  if Value <> FLeftChar then
  begin
    iDelta := FLeftChar - Value;
    FLeftChar := Value;
    FTextOffset := FGutterWidth + 2 - (LeftChar - 1) * FCharWidth;
    if Abs(iDelta) < CharsInWindow then
    begin
      iTextArea := ClientRect;
      Inc(iTextArea.Left, FGutterWidth + 2);
      ScrollWindow(Handle, iDelta * CharWidth, 0, @iTextArea, @iTextArea);
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
begin
  if FFontSmoothing <> AValue then
  begin
    FFontSmoothing := AValue;
    FontSmoothingChanged;
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
  SetSelTextPrimitiveEx(FActiveSelectionMode, PWideChar(Value), True);
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
    Count: Integer;
  begin
    UpdateMarks := False;
    MarkOffset := 0;
    MarkOffset2 := 0;
    case FActiveSelectionMode of
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
          InternalCaretXY := BufferCoord(BB.Char, FBlockEnd.Line);
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
    begin
      Count := BE.Line - BB.Line + MarkOffset;
      if Count > 0 then
        DoLinesDeleted(BB.Line + MarkOffset2, Count);
    end;
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
        Inc(FCaretY);
        Include(FStatusChanges, scCaretY);
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
          FCaretX := GetExpandedLength(Str, TabWidth) + 1
        else
          FCaretX := 1 + Length(Lines[CaretY - 1]) - Length(TrimTrailingSpaces(sRightSide))
      else FCaretX := 1 + Length(Lines[CaretY - 1]) - Length(sRightSide);
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
              FUndoList.AddChange(crLineBreak, LineBreakPos, LineBreakPos, '', smNormal);
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
            FUndoList.AddChange(crPaste, BufferCoord(InsertPos, CaretY),
               BufferCoord(InsertPos + (P - Start), CaretY), '', FActiveSelectionMode);
          end;
        end;
        if P^ = #13 then
        begin
          Inc(P);
          if P^ = #10 then
            Inc(P);
          Inc(FCaretY);
          Include(FStatusChanges, scCaretY);
        end;
        Start := P;
      until P^ = #0;
      Inc(FCaretX, Length(Str));
      Include(FStatusChanges, scCaretX);
    end;

    function InsertLine: Integer;
    var
      Start: PWideChar;
      P: PWideChar;
      Str: UnicodeString;
      n: Integer;
    begin
      Result := 0;
      FCaretX := 1;
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
          FCaretX := 1 + Length(Str);
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
          Inc(FCaretY);
          Include(FStatusChanges, scCaretY);
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
begin
  if (eoScrollPastEof in Options) then
    Value := Min(Value, DisplayLineCount)
  else
    Value := Min(Value, DisplayLineCount - FLinesInWindow + 1);
  Value := Max(Value, 1);
  if Value <> TopLine then
  begin
    Delta := TopLine - Value;
    FTopLine := Value;
    if Abs(Delta) < FLinesInWindow then
      ScrollWindow(Handle, 0, FTextHeight * Delta, nil, nil)
    else
      Invalidate;

    UpdateWindow(Handle);
    UpdateScrollBars;
    StatusChanged([scTopLine]);
  end;
end;

{$IFDEF SYN_CodeFolding}
procedure TCustomSynEdit.SetUseCodeFolding(const Value: Boolean);
Var
  ValidValue : Boolean;
begin
  ValidValue := Value and ((Assigned(fHighlighter) and
      (fHighlighter is TSynCustomCodeFoldingHighlighter))
        or Assigned(fOnScanForFoldRanges));

  if fUseCodeFolding <> ValidValue then
  begin
    AllFoldRanges.Reset;
    fUseCodeFolding := ValidValue;
    Invalidate; // better Invalidate before changing LeftChar and TopLine
    if ValidValue then
    begin
      // !!Mutually exclusive with WordWrap to reduce complexity
      WordWrap := False;
      FullFoldScan;
    end;
    OnCodeFoldingChange(Self);
    InvalidateGutter;
  end;
end;

procedure TCustomSynEdit.OnCodeFoldingChange(Sender: TObject);
begin
  if fUseCodeFolding then
  // The fold shape is drawn in a square 2 * Gutter.RightMargin
  //  to the right of RightOffset and 2 * Gutter.RightMagin to the left of
  //  fGuttterWidth.  It is centered vertically.
  //  Gutter.RightMargin is 2 at 96 DPI
    Gutter.RightOffset := CodeFolding.GutterShapeSize + 3 * Gutter.RightMargin
  else
    Gutter.RightOffset := Gutter.RightMargin;
  Invalidate;
end;

function TCustomSynEdit.GetCollapseMarkRect(Row, Line: Integer): TRect;
begin
  Result := Rect(0, 0, 0, 0);

  if not UseCodeFolding then
    Exit;

  if Line < 0 then
    Line := RowToLine(Row);

  if not AllFoldRanges.CollapsedFoldStartAtLine(Line) then
    Exit;

  { Prepare rect }
  with Result do
  begin
    Top := (Row - fTopLine) * fTextHeight + 1;
    Bottom := Top + fTextHeight - 2;
  end;

  Result.Left := fTextOffset +
    (TSynEditStringList(fLines).ExpandedStringLengths[Line-1] + 1) * fCharWidth;

  { Fix rect }
  if eoShowSpecialChars in fOptions then
    Inc(Result.Left, fCharWidth);

  // Deal wwth horizontal Scroll
  Result.Left := Max(Result.Left, fGutterWidth + fCharWidth);

  Result.Right := Result.Left + fCharWidth * 3 +  4 * (fCharWidth div 7);
end;

function TCustomSynEdit.GetFoldShapeRect(Row: Integer): TRect;
begin
  // Form a square rect for the square the user can click on
  // The fold shape is drawn in a square 4 pixels to the right of RightOffset
  // 4 pixels from the fGuttterWidth.  It is  vertically centered within a line.
  Result.Left := fGutterWidth - CodeFolding.GutterShapeSize - 2 * Gutter.RightMargin;
  Result.Right := Result.Left + CodeFolding.GutterShapeSize;
  Result.Top := (Row - fTopLine) * LineHeight;
  // make a square rect
  Result.Top := Result.Top + ((LineHeight - (Result.Right - Result.Left)) div 2);
  Result.Bottom := Result.Top + (Result.Right - Result.Left);
end;
{$ENDIF}

procedure TCustomSynEdit.ShowCaret;
begin
  if not (eoNoCaret in Options) and not (sfCaretVisible in FStateFlags) then
  begin
    if Windows.ShowCaret(Handle) then
      Include(FStateFlags, sfCaretVisible);
  end;
end;

procedure TCustomSynEdit.UpdateCaret;
var
  CX, CY: Integer;
  iClientRect: TRect;
  vCaretDisplay: TDisplayCoord;
  vCaretPix: TPoint;
  cf: TCompositionForm;
  vSelStartPix: TPoint;
begin
  if (PaintLock <> 0) or not (Focused or FAlwaysShowCaret) then
    Include(FStateFlags, sfCaretChanged)
  else
  begin
    Exclude(FStateFlags, sfCaretChanged);
    vCaretDisplay := DisplayXY;
    if WordWrap and (vCaretDisplay.Column > CharsInWindow + 1) then
      vCaretDisplay.Column := CharsInWindow + 1;
    vCaretPix := RowColumnToPixels(vCaretDisplay);
    CX := vCaretPix.X + FCaretOffset.X;
    CY := vCaretPix.Y + FCaretOffset.Y;
    iClientRect := GetClientRect;
    Inc(iClientRect.Left, FGutterWidth);
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
    if (Self.SelAvail = False) then
    begin
      cf.dwStyle := CFS_POINT;
      cf.ptCurrentPos := Point(CX, CY);
      ImmSetCompositionWindow(ImmGetContext(Handle), @cf);
    end
    else
    begin
      vSelStartPix := Self.RowColumnToPixels(BufferToDisplayPos(Self.BlockBegin));
      Self.SetImeCompositionWindow(Self.Font, vSelStartPix.X, vSelStartPix.Y);
    end;
  end;
end;

procedure TCustomSynEdit.UpdateScrollBars;
var
  nMaxScroll: Integer;
  ScrollInfo: TScrollInfo;
  iRightChar: Integer;
begin
  if not HandleAllocated or (PaintLock <> 0) then
    Include(FStateFlags, sfScrollbarChanged)
  else begin
    Exclude(FStateFlags, sfScrollbarChanged);
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
        if FPaintLock=0 then
           Invalidate;

      end
      else
        ShowScrollBar(Handle, SB_VERT, False);

    end {endif fScrollBars <> ssNone}
    else
      ShowScrollBar(Handle, SB_BOTH, False);
  end;
end;

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
  if GetKeyState(SYNEDIT_CONTROL) < 0 then
    iLinesToScroll := LinesInWindow shr Ord(eoHalfPageScroll in FOptions)
  else
    iLinesToScroll := 3;
  Inc(FMouseWheelAccumulator, WheelDelta);
  iWheelClicks := FMouseWheelAccumulator div WHEEL_DIVISOR;
  FMouseWheelAccumulator := FMouseWheelAccumulator mod WHEEL_DIVISOR;
  TopLine := TopLine - iWheelClicks * iLinesToScroll;
  Update;
  if Assigned(OnScroll) then OnScroll(Self,sbVertical);
  Result := True;
end;
{$ENDIF}

{$IFDEF SYN_COMPILER_12_UP}
type
  PHintInfo = Controls.PHintInfo;
{$ENDIF}

procedure TCustomSynEdit.CMHintShow(var Msg: TMessage);
var
  FoundHint: Boolean;
  MouseCoords, TokenCoords: TBufferCoord;
  TokenStr: UnicodeString;
  TokenHint: string;
  TokenType, Start: Integer;
  Attri: TSynHighlighterAttributes;
  D: TDisplayCoord;
  P1, P2: TPoint;
  TokenRect: TRect;
begin
  if FHintMode = shmToken then
  begin
    FoundHint := False;
    if Assigned(FOnTokenHint) and GetPositionOfMouse(MouseCoords) and
      GetHighlighterAttriAtRowColEx(MouseCoords, TokenStr, TokenType, Start, Attri) then
    begin
      TokenCoords.Char := Start;
      TokenCoords.Line := MouseCoords.Line;
      FOnTokenHint(Self, TokenCoords, TokenStr, TokenType, Attri, TokenHint);
      FoundHint := TokenHint <> '';
    end;

    if FoundHint then
    begin
      D := BufferToDisplayPos(TokenCoords);
      P1 := RowColumnToPixels(D);
      P2.X := P1.X + Length(TokenStr) * CharWidth;
      P2.Y := P1.Y + FTextHeight;
      TokenRect.TopLeft := P1;
      TokenRect.BottomRight := P2;

      InflateRect(TokenRect, 2, 2);
      with PHintInfo(Msg.LParam)^ do
      begin
        HintStr := TokenHint;
        CursorRect := TokenRect;
        HintData := nil;
      end;
      Msg.Result := 0;
    end
    else
      Msg.Result := 1;
  end
  else
    inherited;
end;

procedure TCustomSynEdit.WMCaptureChanged(var Msg: TMessage);
begin
  FScrollTimer.Enabled := False;
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
  {$IFNDEF UNICODE}
  FileNameA: array[0..MAX_PATH - 1] of AnsiChar;
  {$ENDIF}
  FileNameW: array[0..MAX_PATH - 1] of WideChar;
  Point: TPoint;
  FilesList: TUnicodeStringList;
begin
  try
    if Assigned(FOnDropFiles) then
    begin
      FilesList := TUnicodeStringList.Create;
      try
        iNumberDropped := DragQueryFile(THandle(Msg.wParam), Cardinal(-1),
          nil, 0);
        DragQueryPoint(THandle(Msg.wParam), Point);

{$IFNDEF UNICODE}
        if Win32PlatformIsUnicode then
{$ENDIF}
          for i := 0 to iNumberDropped - 1 do
          begin
            DragQueryFileW(THandle(Msg.wParam), i, FileNameW,
              sizeof(FileNameW) div 2);
            FilesList.Add(FileNameW)
{$IFNDEF UNICODE}
          end
        else
          for i := 0 to iNumberDropped - 1 do
          begin
            DragQueryFileA(THandle(Msg.wParam), i, FileNameA,
              sizeof(FileNameA));
            FilesList.Add(UnicodeString(FileNameA))
{$ENDIF}
          end;
        FOnDropFiles(Self, Point.X, Point.Y, FilesList);
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
  if FWantTabs then
    Msg.Result := Msg.Result or DLGC_WANTTAB;
  if FWantReturns then
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
  iMaxWidth: Integer;
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
      + (FCharsInWindow - Ord(eoScrollByOneLess in FOptions));
    SB_PAGELEFT: LeftChar := LeftChar
      - (FCharsInWindow - Ord(eoScrollByOneLess in FOptions));
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

procedure TCustomSynEdit.WMImeRequest(var Message: TMessage);
var
  pReconvert: PReconvertString;
  TargetText: string;
  TargetByteLength: Integer;
  pTarget: PChar;
  H: HIMC;
begin
  case Message.WParam of
    IMR_RECONVERTSTRING:
      begin
        // Reconversion string
        if (Self.SelLength <> 0) then
        begin
          TargetText := Self.SelText;
        end
        else
        begin
          if (Self.Lines.Count >= Self.CaretY - 1) then
            TargetText := Self.Lines[Self.CaretY - 1]
          else
            TargetText := '';
        end;
        TargetByteLength := Length(TargetText) * sizeof(Char);

        if (Message.LParam = 0) then
        begin
          // 1st time (get buffer size (bytes))
          // Select only one row
          if (Self.BlockBegin.Line = Self.BlockEnd.Line) then
            Message.Result := Sizeof(TReconvertString) + TargetByteLength
          else
            Message.Result := 0;
        end
        else
        begin
          // 2nd time
          pReconvert := Pointer(Message.LParam);
          pReconvert.dwSize := Sizeof(TReconvertString);
          pReconvert.dwVersion := 0;
          pReconvert.dwStrLen := Length(TargetText);
          pReconvert.dwStrOffset := Sizeof(TReconvertString);

          pTarget := Pointer(Message.LParam + Sizeof(TReconvertString));
          move(TargetText[1], pTarget^, TargetByteLength);

          if (Self.SelLength <> 0) then
          begin
            pReconvert.dwTargetStrLen := 0;
            pReconvert.dwTargetStrOffset := 0;
            pReconvert.dwCompStrLen := Length(TargetText);
            pReconvert.dwCompStrOffset := 0;
          end
          else
          begin
            pReconvert.dwTargetStrLen := 0;
            pReconvert.dwTargetStrOffset := (Self.CaretX - 1) * sizeof(Char);
            H := Imm32GetContext(Handle);
            try
              ImmSetCompositionString(H, SCS_QUERYRECONVERTSTRING, pReconvert, Sizeof(TReconvertString) + TargetByteLength, nil, 0);
              if (pReconvert.dwCompStrLen <> 0) then
              begin
                Self.CaretX := pReconvert.dwCompStrOffset div sizeof(Char) + 1;
                Self.SelStart := RowColToCharIndex(Self.CaretXY);
                Self.SelLength := pReconvert.dwCompStrLen;
              end;
            finally
              Imm32ReleaseContext(Handle, H);
            end;
          end;
          Message.Result := Sizeof(TReconvertString) + TargetByteLength;
        end;
      end;
    IMR_DOCUMENTFEED:
      begin
        // Notifies an application when the selected IME needs the converted string from the application.
        if (Self.Lines.Count >= Self.CaretY) then
          TargetText := Self.Lines[Self.CaretY]
        else
          TargetText := '';
        if (Message.LParam = 0) then
        begin
          // 1st time (get line size (bytes))
          Message.Result := Sizeof(TReconvertString) + Length(TargetText) * sizeof(Char);
        end
        else
        begin
          // 2nd time
          pReconvert := Pointer(Message.LParam);
          pReconvert.dwSize := Sizeof(TReconvertString);
          pReconvert.dwVersion := 0;
          pReconvert.dwStrLen := Length(TargetText);
          pReconvert.dwStrOffset := Sizeof(TReconvertString);
          pReconvert.dwCompStrLen := 0;
          pReconvert.dwCompStrOffset := 0;
          pReconvert.dwTargetStrLen := 0;
          pReconvert.dwTargetStrOffset := (Self.CaretX - 1) * sizeof(Char);

          pTarget := Pointer(Message.LParam + Sizeof(TReconvertString));
          move(TargetText[1], pTarget^, Length(TargetText) * sizeof(Char));

          Message.Result := Sizeof(TReconvertString) + Length(TargetText) * sizeof(Char);
        end;
      end;
  end;
end;

procedure TCustomSynEdit.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  CommandProcessor(ecLostFocus, #0, nil);
  //Added check for focused to prevent caret disappearing problem
  if Focused or FAlwaysShowCaret then
    Exit;
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
      + (FLinesInWindow - Ord(eoScrollByOneLess in FOptions));
    SB_PAGEUP: TopLine := TopLine
      - (FLinesInWindow - Ord(eoScrollByOneLess in FOptions));
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

        if eoShowScrollHint in FOptions then
        begin
          ScrollHint := GetScrollHint;
          ScrollHint.Color := FScrollHintColor;
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
          if eoScrollHintFollows in FOptions then
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
      if eoShowScrollHint in FOptions then
        ShowWindow(GetScrollHint.Handle, SW_HIDE);
  end;
  end;
  Update;
  if Assigned(OnScroll) then OnScroll(Self,sbVertical);
end;

function TCustomSynEdit.ScanFrom(Index: Integer): Integer;
var
  iRange: TSynEditRange;
begin
  Result := Index;
  if Result >= Lines.Count then Exit;

  if Result = 0 then
    FHighlighter.ResetRange
  else
    FHighlighter.SetRange(TSynEditStringList(Lines).Ranges[Result - 1]);

  repeat
    FHighlighter.SetLine(Lines[Result], Result);
    FHighlighter.NextToEol;
    iRange := FHighlighter.GetRange;
    if TSynEditStringList(Lines).Ranges[Result] = iRange then
      Exit; // avoid the final decrement
    TSynEditStringList(Lines).Ranges[Result] := iRange;
    Inc(Result);
  until (Result = Lines.Count);
  Dec(Result);
end;

procedure TCustomSynEdit.ListCleared(Sender: TObject);
begin
  if WordWrap then
    FWordWrapPlugin.Reset;

{$IFDEF SYN_CodeFolding}
  if UseCodeFolding then
    AllFoldRanges.Reset;
{$ENDIF}

  ClearUndo;
  // invalidate the *whole* client area
  FillChar(FInvalidateRect, SizeOf(TRect), 0);
  Invalidate;
  // set caret and selected block to start of text
  CaretXY := BufferCoord(1, 1);
  // scroll to start of text
  TopLine := 1;
  LeftChar := 1;
  Include(FStatusChanges, scAll);
end;

procedure TCustomSynEdit.ListDeleted(Sender: TObject; aIndex: Integer;
  aCount: Integer);
{$IFDEF SYN_CodeFolding}
Var
  vLastScan: Integer;
begin
  vLastScan := aIndex;
  if Assigned(fHighlighter) and (Lines.Count > 0) then
    vLastScan := ScanFrom(aIndex);

  if UseCodeFolding then begin
    AllFoldRanges.LinesDeleted(aIndex, aCount);
    // Scan the same lines the highlighter has scanned
    ReScanForFoldRanges(aIndex, vLastScan);
    InvalidateGutter;
  end;
{$ELSE}
begin
{$ENDIF}
  if Assigned(FHighlighter) and (Lines.Count > 0) then
    ScanFrom(aIndex);

  if WordWrap then
    FWordWrapPlugin.LinesDeleted(aIndex, aCount);

  InvalidateLines(aIndex + 1, MaxInt);
  InvalidateGutterLines(aIndex + 1, MaxInt);
//++ Flicker Reduction
  Include(fStateFlags, sfScrollbarChanged);
//-- Flicker Reduction
end;

procedure TCustomSynEdit.ListInserted(Sender: TObject; Index: Integer;
  aCount: Integer);
var
  L: Integer;
  vLastScan: Integer;
{$IFDEF SYN_CodeFolding}
  FoldIndex: Integer;
begin
  vLastScan := Index;
{$ELSE}
begin
{$ENDIF}
  if Assigned(FHighlighter) and (Lines.Count > 0) then
  begin
    vLastScan := Index;
    repeat
      vLastScan := ScanFrom(vLastScan);
      Inc(vLastScan);
    until vLastScan >= Index + aCount;
  end;

{$IFDEF SYN_CodeFolding}
  if UseCodeFolding then begin
    if fAllFoldRanges.CollapsedFoldStartAtLine(Index, FoldIndex) then
      // insertion starts at collapsed fold
      Uncollapse(FoldIndex);
    AllFoldRanges.LinesInserted(Index, aCount);
    // Scan the same lines the highlighter has scanned
    ReScanForFoldRanges(Index, vLastScan-1);
  end;
{$ENDIF}

  if WordWrap then
    FWordWrapPlugin.LinesInserted(Index, aCount);

  InvalidateLines(Index + 1, MaxInt);
  InvalidateGutterLines(Index + 1, MaxInt);
//++ Flicker Reduction
  Include(fStateFlags, sfScrollbarChanged);
//-- Flicker Reduction

  if (eoAutoSizeMaxScrollWidth in FOptions) then
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
{$IFDEF SYN_CodeFolding}
  vLastScan: Integer;
  FoldIndex: Integer;
{$ENDIF}
begin
  vEndLine := Index +1;
  if WordWrap then
  begin
    if FWordWrapPlugin.LinesPutted(Index, aCount) <> 0 then
      vEndLine := MaxInt;
    InvalidateGutterLines(Index + 1, vEndLine);
  end;
{$IFDEF SYN_CodeFolding}
  vLastScan := Index;
  if Assigned(fHighlighter) then
  begin
    vLastScan := ScanFrom(Index);
    vEndLine := Max(vEndLine, vLastScan + 1);
{$ELSE}
  if Assigned(FHighlighter) then
  begin
    vEndLine := Max(vEndLine, ScanFrom(Index) + 1);
{$ENDIF}
    // If this editor is chained then the real owner of text buffer will probably
    // have already parsed the changes, so ScanFrom will return immediately.
    if FLines <> FOrigLines then
      vEndLine := MaxInt;
  end;

  {$IFDEF SYN_CodeFolding}
  if fUseCodeFolding then begin
    if fAllFoldRanges.CollapsedFoldStartAtLine(Index + 1, FoldIndex) then
      // modification happens at collapsed fold
      Uncollapse(FoldIndex);
    AllFoldRanges.LinesPutted(Index, aCount);
    // Scan the same lines the highlighter has scanned
    ReScanForFoldRanges(Index, vLastScan);
  end;
{$ENDIF}

  InvalidateLines(Index + 1, vEndLine);
  InvalidateGutterLines(Index + 1, vEndLine);

  if (eoAutoSizeMaxScrollWidth in FOptions) then
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
  if Assigned(FHighlighter) and (Lines.Count > 0) then begin
    FHighlighter.ResetRange;
    i := 0;
    repeat
      FHighlighter.SetLine(Lines[i], i);
      FHighlighter.NextToEol;
      TSynEditStringList(Lines).Ranges[i] := FHighlighter.GetRange;
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
    Value.Char := MinMax(Value.Char, 1, FMaxScrollWidth + 1)
  else
    Value.Char := Max(Value.Char, 1);
  Value.Line := MinMax(Value.Line, 1, Lines.Count);
  TempString := Lines[Value.Line - 1] + #0; //needed for CaretX = LineLength + 1
  if Value.Char > Length(TempString) then
  begin
    InternalCaretXY := BufferCoord(Length(TempString), Value.Line);
    Exit;
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
  if ptMouse.X >= FGutterWidth + 2 then
  begin
    if not (eoNoSelection in FOptions) then
      SetWordBlock(CaretXY);
    inherited;
    Include(FStateFlags, sfDblClicked);
    Exclude(FStateFlags, sfMouseCaptured);
    MouseCapture := False;
  end
  else
    inherited;
end;

function TCustomSynEdit.GetCanUndo: Boolean;
begin
  Result := not ReadOnly and FUndoList.CanUndo;
end;

function TCustomSynEdit.GetCanRedo: Boolean;
begin
  Result := not ReadOnly and FRedoList.CanUndo;
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

procedure TCustomSynEdit.InsertLine(const BB, BE: TBufferCoord; ChangeStr: PWideChar;
  AddToUndoList: Boolean);
begin
  SetCaretAndSelection(BB, BB, BE);
  ActiveSelectionMode := smLine;
  SetSelTextPrimitiveEx(smLine, ChangeStr, AddToUndoList);
  StatusChanged([scSelection]);
end;

procedure TCustomSynEdit.Redo;

  procedure RemoveGroupBreak;
  var
    Item: TSynEditUndoItem;
    OldBlockNumber: Integer;
  begin
    if FRedoList.LastChangeReason = crGroupBreak then
    begin
      OldBlockNumber := UndoList.BlockChangeNumber;
      Item := FRedoList.PopItem;
      try
        UndoList.BlockChangeNumber := Item.ChangeNumber;
        FUndoList.AddGroupBreak;
      finally
        UndoList.BlockChangeNumber := OldBlockNumber;
        Item.Free;
      end;
      UpdateModifiedStatus;
    end;
  end;

var
  Item: TSynEditUndoItem;
  OldChangeNumber: Integer;
  SaveChangeNumber: Integer;
  FLastChange : TSynChangeReason;
  FAutoComplete: Boolean;
  FPasteAction: Boolean;
  FSpecial1: Boolean;
  FSpecial2: Boolean;
  FKeepGoing: Boolean;
begin
  if ReadOnly then
    Exit;

  FLastChange := FRedoList.LastChangeReason;
  FAutoComplete := FLastChange = crAutoCompleteBegin;
  FPasteAction := FLastChange = crPasteBegin;
  FSpecial1 := FLastChange = crSpecial1Begin;
  FSpecial2 := FLastChange = crSpecial2Begin;

  Item := FRedoList.PeekItem;
  if Item <> nil then
  begin
    OldChangeNumber := Item.ChangeNumber;
    SaveChangeNumber := FUndoList.BlockChangeNumber;
    FUndoList.BlockChangeNumber := Item.ChangeNumber;
    try
      repeat
        RedoItem;
        Item := FRedoList.PeekItem;
        if Item = nil then
          FKeepGoing := False
        else begin
          if FAutoComplete then
             FKeepGoing := (FRedoList.LastChangeReason <> crAutoCompleteEnd)
          else if FPasteAction then
             FKeepGoing := (FRedoList.LastChangeReason <> crPasteEnd)
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
      FUndoList.BlockChangeNumber := SaveChangeNumber;
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
  ChangeScrollPastEol: Boolean;
  BeginX: Integer;
begin
  ChangeScrollPastEol := not (eoScrollPastEol in Options);
  Item := FRedoList.PopItem;
  if Assigned(Item) then
  try
    ActiveSelectionMode := Item.ChangeSelMode;
    IncPaintLock;
    Include(FOptions, eoScrollPastEol);
    FUndoList.InsideRedo := True;
    case Item.ChangeReason of
      crCaret:
        begin
          FUndoList.AddChange(Item.ChangeReason, CaretXY, CaretXY, '', FActiveSelectionMode);
          InternalCaretXY := Item.ChangeStartPos;
        end;
      crSelection:
        begin
          FUndoList.AddChange(Item.ChangeReason, BlockBegin, BlockEnd, '', FActiveSelectionMode);
          SetCaretAndSelection(CaretXY, Item.ChangeStartPos, Item.ChangeEndPos);
        end;
      crInsert, crPaste, crDragDropInsert:
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeStartPos);
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PWideChar(Item.ChangeStr),
            False);
          InternalCaretXY := Item.ChangeEndPos;
          FUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
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
          FUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
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
          FUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
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
          FUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
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
               Inc(FCaretY);
             end;
             StrToDelete := Run;
           until Run^ = #0;
          if Item.ChangeSelMode = smColumn then
            SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
              Item.ChangeEndPos)
          else begin
            // restore selection
            CaretPt.Char := Item.ChangeStartPos.Char - FTabWidth;
            CaretPt.Line := Item.ChangeStartPos.Line;
            SetCaretAndSelection( CaretPt, CaretPt,
              BufferCoord(Item.ChangeEndPos.Char - Len, Item.ChangeEndPos.Line) );
          end;
           // add to undo list
           FUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
             Item.ChangeEndPos, Item.ChangeStr, Item.ChangeSelMode);
         end;
      crWhiteSpaceAdd:
        begin
          FUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
             Item.ChangeEndPos, '', Item.ChangeSelMode);
          SetCaretAndSelection(Item.ChangeEndPos, Item.ChangeEndPos,
            Item.ChangeEndPos);
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PWideChar(Item.ChangeStr), True);
          InternalCaretXY := Item.ChangeStartPos;
        end;
    end;
  finally
    FUndoList.InsideRedo := False;
    if ChangeScrollPastEol then
      Exclude(FOptions, eoScrollPastEol);
    Item.Free;
    DecPaintLock;
  end;
end;

{$IFDEF SYN_CodeFolding}
procedure TCustomSynEdit.Collapse(FoldRangeIndex: Integer; Invalidate:Boolean);
begin
  AllFoldRanges.Ranges.List[FoldRangeIndex].Collapsed := True;

  with AllFoldRanges.Ranges[FoldRangeIndex] do
  begin
    // Extract caret from fold
    if (fCaretY > FromLine) and (fCaretY <= ToLine) then
      CaretXY := BufferCoord(Length(Lines[FromLine - 1]) + 1, FromLine);

    if Invalidate then begin
      // Redraw the collapsed line and below
      InvalidateLines(FromLine, MaxInt);

      // Redraw fold mark
      InvalidateGutterLines(FromLine, MaxInt);

      UpdateScrollBars;
    end else
      // Update Scrollbars
      Include(fStateFlags, sfScrollbarChanged);
  end;
end;

procedure TCustomSynEdit.CollapseAll;
var
  i: Integer;
begin
  if not fUseCodeFolding then Exit;
    for i := fAllFoldRanges.Count - 1 downto 0 do
      Collapse(i, False);

  InvalidateLines(-1, -1);
  InvalidateGutterLines(-1, -1);

  EnsureCursorPosVisible;
end;


procedure TCustomSynEdit.CollapseLevel(Level: integer);
Var
  i : integer;
  RangeIndices : TArray<Integer>;
begin
  if not fUseCodeFolding then Exit;
  RangeIndices := AllFoldRanges.FoldsAtLevel(Level);
  for i := Low(RangeIndices) to High(RangeIndices) do
    Collapse(RangeIndices[i], False);

  InvalidateLines(-1, -1);
  InvalidateGutterLines(-1, -1);

  EnsureCursorPosVisible;
end;

procedure TCustomSynEdit.CollapseNearest;
Var
  Index : integer;
begin
  if not fUseCodeFolding then Exit;
  if AllFoldRanges.FoldAroundLineEx(CaretY, False, True, True, Index) then
    Collapse(Index);

    EnsureCursorPosVisible;
end;

procedure TCustomSynEdit.CollapseFoldType(FoldType : Integer);
Var
  i : integer;
  RangeIndices : TArray<Integer>;
begin
  if not fUseCodeFolding then Exit;
  RangeIndices := AllFoldRanges.FoldsOfType(FoldType);
  for i := Low(RangeIndices) to High(RangeIndices) do
    Collapse(RangeIndices[i],False);

  InvalidateLines(-1, -1);
  InvalidateGutterLines(-1, -1);

  EnsureCursorPosVisible;
end;

procedure TCustomSynEdit.Uncollapse(FoldRangeIndex: Integer; Invalidate:Boolean);
begin
    AllFoldRanges.Ranges.List[FoldRangeIndex].Collapsed := False;

  if Invalidate then with AllFoldRanges.Ranges[FoldRangeIndex] do
  begin
    // Redraw the uncollapsed line and below
    InvalidateLines(FromLine, MaxInt);

    // Redraw fold marks
    InvalidateGutterLines(FromLine, MaxInt);

    // Make sure we can see the cursor
    // EnsureCursorPosVisible;

    UpdateScrollBars;
  end else
    // Update Scrollbars
    Include(fStateFlags, sfScrollbarChanged);
end;

procedure TCustomSynEdit.UncollapseAroundLine(Line: Integer);
var
  Index: Integer;
begin
  if not fUseCodeFolding then Exit;
  // Open up the closed folds around the focused line until we can see the line we're looking for
  while AllFoldRanges.FoldHidesLine(line, Index) do
    Uncollapse(Index);
end;

procedure TCustomSynEdit.UnCollapseLevel(Level: integer);
Var
  i : integer;
  RangeIndices : TArray<Integer>;
begin
  if not fUseCodeFolding then Exit;
  RangeIndices := AllFoldRanges.FoldsAtLevel(Level);
  for i := Low(RangeIndices) to High(RangeIndices) do
    Uncollapse(RangeIndices[i], False);

  InvalidateLines(-1, -1);
  InvalidateGutterLines(-1, -1);

  EnsureCursorPosVisible;
end;

procedure TCustomSynEdit.UncollapseNearest;
Var
  Index : integer;
begin
  if not fUseCodeFolding then Exit;
  if AllFoldRanges.CollapsedFoldStartAtLine(CaretY, Index) then
    Uncollapse(Index);

  EnsureCursorPosVisible;
end;

procedure TCustomSynEdit.UnCollapseFoldType(FoldType : Integer);
Var
  i : integer;
  RangeIndices : TArray<Integer>;
begin
  if not fUseCodeFolding then Exit;
  RangeIndices := AllFoldRanges.FoldsOfType(FoldType);
  for i := Low(RangeIndices) to High(RangeIndices) do
    Uncollapse(RangeIndices[i], False);

  InvalidateLines(-1, -1);
  InvalidateGutterLines(-1, -1);

  EnsureCursorPosVisible;
end;

procedure TCustomSynEdit.UncollapseAll;
var
  i: Integer;
begin
  if not fUseCodeFolding then Exit;
  for i := fAllFoldRanges.Count - 1 downto 0 do
      Uncollapse(i, False);

  InvalidateLines(-1, -1);
  InvalidateGutterLines(-1, -1);

  EnsureCursorPosVisible;
end;
{$ENDIF}

procedure TCustomSynEdit.Undo;

  procedure RemoveGroupBreak;
  var
    Item: TSynEditUndoItem;
    OldBlockNumber: Integer;
  begin
    if FUndoList.LastChangeReason = crGroupBreak then
    begin
      OldBlockNumber := RedoList.BlockChangeNumber;
      try
        Item := FUndoList.PopItem;
        RedoList.BlockChangeNumber := Item.ChangeNumber;
        Item.Free;
        FRedoList.AddGroupBreak;
      finally
        RedoList.BlockChangeNumber := OldBlockNumber;
      end;
    end;
  end;

var
  Item: TSynEditUndoItem;
  OldChangeNumber: Integer;
  SaveChangeNumber: Integer;
  FLastChange : TSynChangeReason;
  FAutoComplete: Boolean;
  FPasteAction: Boolean;
  FSpecial1: Boolean;
  FSpecial2: Boolean;
  FKeepGoing: Boolean;
begin
  if ReadOnly then
    Exit;

  RemoveGroupBreak;

  FLastChange := FUndoList.LastChangeReason;
  FAutoComplete := FLastChange = crAutoCompleteEnd;
  FPasteAction := FLastChange = crPasteEnd;
  FSpecial1 := FLastChange = crSpecial1End;
  FSpecial2 := FLastChange = crSpecial2End;

  Item := FUndoList.PeekItem;
  if Item <> nil then
  begin
    OldChangeNumber := Item.ChangeNumber;
    SaveChangeNumber := FRedoList.BlockChangeNumber;
    FRedoList.BlockChangeNumber := Item.ChangeNumber;

    try
      repeat
        UndoItem;
        Item := FUndoList.PeekItem;
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
      FRedoList.BlockChangeNumber := SaveChangeNumber;
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
  Item := FUndoList.PopItem;
  if Assigned(Item) then
  try
    ActiveSelectionMode := Item.ChangeSelMode;
    IncPaintLock;
    Include(FOptions, eoScrollPastEol);
    case Item.ChangeReason of
      crCaret:
        begin
          FRedoList.AddChange(Item.ChangeReason, CaretXY, CaretXY, '', FActiveSelectionMode);
          InternalCaretXY := Item.ChangeStartPos;
        end;
      crSelection:
        begin
          FRedoList.AddChange(Item.ChangeReason, BlockBegin, BlockEnd, '', FActiveSelectionMode);
          SetCaretAndSelection(CaretXY, Item.ChangeStartPos, Item.ChangeEndPos);
        end;
      crInsert, crPaste, crDragDropInsert:
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          TmpStr := SelText;
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PWideChar(Item.ChangeStr),
            False);
          FRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
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
            FLines.Add('');
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
          FRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, '', Item.ChangeSelMode);
          if Item.ChangeReason = crDeleteAll then begin
            InternalCaretXY := BufferCoord(1, 1);
            FBlockEnd := BufferCoord(1, 1);
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
          FRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, '', Item.ChangeSelMode);
        end;
      crIndent:
        begin
          SetCaretAndSelection(Item.ChangeEndPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          FRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
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
          FRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
             Item.ChangeEndPos, Item.ChangeStr, Item.ChangeSelMode);
        end;
      crWhiteSpaceAdd:
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          TmpStr := SelText;
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PWideChar(Item.ChangeStr), True);
          FRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, TmpStr, Item.ChangeSelMode);
          InternalCaretXY := Item.ChangeStartPos;
        end;
    end;
  finally
    if ChangeScrollPastEol then
      Exclude(FOptions, eoScrollPastEol);
    Item.Free;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.ClearBookMark(BookMark: Integer);
begin
  if (BookMark in [0..9]) and assigned(FBookMarks[BookMark]) then
  begin
    DoOnClearBookmark(FBookMarks[BookMark]);
    FMarkList.Remove(FBookMarks[Bookmark]);
    FBookMarks[BookMark] := nil;
  end
end;

procedure TCustomSynEdit.GotoBookMark(BookMark: Integer);
var
  iNewPos: TBufferCoord;
begin
  if (BookMark in [0..9]) and
     assigned(FBookMarks[BookMark]) and
     (FBookMarks[BookMark].Line <= FLines.Count)
  then
  begin
    iNewPos.Char := FBookMarks[BookMark].Char;
    iNewPos.Line := FBookMarks[BookMark].Line;
    //call it this way instead to make sure that the caret ends up in the middle
    //if it is off screen (like Delphi does with bookmarks)
    SetCaretXYEx(False, iNewPos);
    EnsureCursorPosVisibleEx(True);
    if SelAvail then
      InvalidateSelection;
    FBlockBegin.Char := FCaretX;
    FBlockBegin.Line := FCaretY;
    FBlockEnd := FBlockBegin;
  end;
end;

procedure TCustomSynEdit.GotoLineAndCenter(ALine: Integer);
begin
  SetCaretXYEx( False, BufferCoord(1, ALine) );
  if SelAvail then
    InvalidateSelection;
  FBlockBegin.Char := FCaretX;
  FBlockBegin.Line := FCaretY;
  FBlockEnd := FBlockBegin;
  EnsureCursorPosVisibleEx(True);
end;

procedure TCustomSynEdit.SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
var
  mark: TSynEditMark;
begin
  if (BookMark in [0..9]) and (Y >= 1) and (Y <= Max(1, FLines.Count)) then
  begin
    mark := TSynEditMark.Create(self);
    with mark do
    begin
      Line := Y;
      Char := X;
      ImageIndex := Bookmark;
      BookmarkNumber := Bookmark;
      Visible := True;
      InternalImage := (FBookMarkOpt.BookmarkImages = nil);
    end;
    DoOnPlaceMark(Mark);
    if (mark <> nil) then
    begin
      if assigned(FBookMarks[BookMark]) then
        ClearBookmark(BookMark);
      FBookMarks[BookMark] := mark;
      FMarkList.Add(FBookMarks[BookMark]);
    end;
  end;
end;

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

procedure TCustomSynEdit.ChainListCleared(Sender: TObject);
begin
  if Assigned(FChainListCleared) then
    FChainListCleared(Sender);
  TSynEditStringList(FOrigLines).OnCleared(Sender);
end;

procedure TCustomSynEdit.ChainListDeleted(Sender: TObject; aIndex: Integer;
  aCount: Integer);
begin
  if Assigned(FChainListDeleted) then
    FChainListDeleted(Sender, aIndex, aCount);
  TSynEditStringList(FOrigLines).OnDeleted(Sender, aIndex, aCount);
end;

procedure TCustomSynEdit.ChainListInserted(Sender: TObject; aIndex: Integer;
  aCount: Integer);
begin
  if Assigned(FChainListInserted) then
    FChainListInserted(Sender, aIndex, aCount);
  TSynEditStringList(FOrigLines).OnInserted(Sender, aIndex, aCount);
end;

procedure TCustomSynEdit.ChainListPutted(Sender: TObject; aIndex: Integer;
  aCount: Integer);
begin
  if Assigned(FChainListPutted) then
    FChainListPutted(Sender, aIndex, aCount);
  TSynEditStringList(FOrigLines).OnPutted(Sender, aIndex, aCount);
end;

procedure TCustomSynEdit.ChainLinesChanging(Sender: TObject);
begin
  if Assigned(FChainLinesChanging) then
    FChainLinesChanging(Sender);
  TSynEditStringList(FOrigLines).OnChanging(Sender);
end;

procedure TCustomSynEdit.ChainLinesChanged(Sender: TObject);
begin
  if Assigned(FChainLinesChanged) then
    FChainLinesChanged(Sender);
  TSynEditStringList(FOrigLines).OnChange(Sender);
end;

procedure TCustomSynEdit.ChainUndoRedoAdded(Sender: TObject);
var
  iList: TSynEditUndoList;
  iHandler: TNotifyEvent;
begin
  if Sender = FUndoList then
  begin
    iList := FOrigUndoList;
    iHandler := FChainUndoAdded;
  end
  else { if Sender = FRedoList then }
  begin
    iList := FOrigRedoList;
    iHandler := FChainRedoAdded;
  end;
  if Assigned(iHandler) then
    iHandler(Sender);
  iList.OnAddedUndo(Sender);
end;

//++ DPI-Aware
procedure TCustomSynEdit.ChangeScale(M, D: Integer{$if CompilerVersion >= 31}; isDpiChange: Boolean{$ifend});
begin
  {$if CompilerVersion >= 31}if isDpiChange then begin{$ifend}
    if Assigned(fGutter) then fGutter.ChangeScale(M,D);
    if Assigned(fBookMarkOpt) then fBookMarkOpt.ChangeScale(M, D);
    if Assigned(fWordWrapGlyph) then fWordWrapGlyph.ChangeScale(M, D);
  {$if CompilerVersion >= 31}end;{$ifend}
  inherited ChangeScale(M, D{$if CompilerVersion >= 31}, isDpiChange{$ifend});
 end;
//-- DPI-Aware

procedure TCustomSynEdit.UnHookTextBuffer;
var
  vOldWrap: Boolean;
begin
  Assert(FChainedEditor = nil);
  if FLines = FOrigLines then
    Exit;

  vOldWrap := WordWrap;
  WordWrap := False;

  //first put back the real methods
  with TSynEditStringList(FLines) do
  begin
    OnCleared := FChainListCleared;
    OnDeleted := FChainListDeleted;
    OnInserted := FChainListInserted;
    OnPutted := FChainListPutted;
    OnChanging := FChainLinesChanging;
    OnChange := FChainLinesChanged;
  end;
  FUndoList.OnAddedUndo := FChainUndoAdded;
  FRedoList.OnAddedUndo := FChainRedoAdded;

  FChainListCleared := nil;
  FChainListDeleted := nil;
  FChainListInserted := nil;
  FChainListPutted := nil;
  FChainLinesChanging := nil;
  FChainLinesChanged := nil;
  FChainUndoAdded := nil;

  //make the switch
  FLines := FOrigLines;
  FUndoList := FOrigUndoList;
  FRedoList := FOrigRedoList;
  LinesHookChanged;

  WordWrap := vOldWrap;
end;

procedure TCustomSynEdit.HookTextBuffer(aBuffer: TSynEditStringList;
  aUndo, aRedo: TSynEditUndoList);
var
  vOldWrap: Boolean;
begin
  Assert(FChainedEditor = nil);
  Assert(FLines = FOrigLines);

  vOldWrap := WordWrap;
  WordWrap := False;

  if FChainedEditor <> nil then
    RemoveLinesPointer
  else if FLines <> FOrigLines then
    UnHookTextBuffer;

  //store the current values and put in the chained methods
  FChainListCleared := aBuffer.OnCleared;
    aBuffer.OnCleared := ChainListCleared;
  FChainListDeleted := aBuffer.OnDeleted;
    aBuffer.OnDeleted := ChainListDeleted;
  FChainListInserted := aBuffer.OnInserted;
    aBuffer.OnInserted := ChainListInserted;
  FChainListPutted := aBuffer.OnPutted;
    aBuffer.OnPutted := ChainListPutted;
  FChainLinesChanging := aBuffer.OnChanging;
    aBuffer.OnChanging := ChainLinesChanging;
  FChainLinesChanged := aBuffer.OnChange;
    aBuffer.OnChange := ChainLinesChanged;

  FChainUndoAdded := aUndo.OnAddedUndo;
    aUndo.OnAddedUndo := ChainUndoRedoAdded;
  FChainRedoAdded := aRedo.OnAddedUndo;
    aRedo.OnAddedUndo := ChainUndoRedoAdded;

  //make the switch
  FLines := aBuffer;
  FUndoList := aUndo;
  FRedoList := aRedo;
  LinesHookChanged;

  WordWrap := vOldWrap;
end;

procedure TCustomSynEdit.LinesHookChanged;
var
  iLongestLineLength: Integer;
begin
  Invalidate;
  if eoAutoSizeMaxScrollWidth in FOptions then
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

  FChainedEditor := ASynEdit;
  ASynEdit.FreeNotification(Self);
end;

procedure TCustomSynEdit.RemoveLinesPointer;
begin
  {$IFDEF SYN_COMPILER_5_UP}
  if Assigned(FChainedEditor) then
    RemoveFreeNotification(FChainedEditor);
  {$ENDIF}
  FChainedEditor := nil;

  UnHookTextBuffer;
end;

procedure TCustomSynEdit.DragCanceled;
begin
  FScrollTimer.Enabled := False;
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
    if GetKeyState(VK_CONTROL) < 0 then
      DragCursor := crMultiDrag
    else
      DragCursor := crDrag;

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
        DropMove := GetKeyState(VK_SHIFT) < 0;
        DoDrop := True;
        DropAfter := False;
      end
      else
      begin
        DropMove := GetKeyState(VK_CONTROL) >= 0;
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
          ChangeScrollPastEOL := not (eoScrollPastEol in FOptions);
          try
            if ChangeScrollPastEOL then
              Include(FOptions, eoScrollPastEol);
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
              Exclude(FOptions, eoScrollPastEol);
          end;
          // save undo information
          if Source = Self then
          begin
            FUndoList.AddChange(crDragDropInsert, vNewCaret, BlockEnd, SelText,
              FActiveSelectionMode);
          end
          else begin
            FUndoList.AddChange(crInsert, vNewCaret, BlockEnd,
              SelText, FActiveSelectionMode);
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
  if FRightEdge <> Value then
  begin
    FRightEdge := Value;
    Invalidate;
  end;
end;

procedure TCustomSynEdit.SetRightEdgeColor(Value: TColor);
var
  nX: Integer;
  rcInval: TRect;
begin
  if FRightEdgeColor <> Value then
  begin
    FRightEdgeColor := Value;
    if HandleAllocated then
    begin
      nX := FTextOffset + FRightEdge * FCharWidth;
      rcInval := Rect(nX - 1, 0, nX + 1, Height);
      InvalidateRect(rcInval, False);
    end;
  end;
end;

function TCustomSynEdit.GetMaxUndo: Integer;
begin
  result := FUndoList.MaxUndoActions;
end;

procedure TCustomSynEdit.SetMaxUndo(const Value: Integer);
begin
  if Value > -1 then
  begin
    FUndoList.MaxUndoActions := Value;
    FRedoList.MaxUndoActions := Value;
  end;
end;

procedure TCustomSynEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FSearchEngine then
    begin
      SearchEngine := nil;
    end;

    if AComponent = FHighlighter then
    begin
      Highlighter := nil;
    end;

    if AComponent = FChainedEditor then
    begin
      RemoveLinesPointer;
    end;

    if (FBookMarkOpt <> nil) then
      if (AComponent = FBookMarkOpt.BookmarkImages) then
      begin
        FBookMarkOpt.BookmarkImages := nil;
        InvalidateGutterLines(-1, -1);
      end;
  end;
end;

procedure TCustomSynEdit.SetHighlighter(const Value: TSynCustomHighlighter);
{$IFDEF SYN_CodeFolding}
Var
  OldUseCodeFolding : Boolean;
{$ENDIF}
begin
  if Value <> FHighlighter then
  begin
    if Assigned(FHighlighter) then
    begin
      FHighlighter.UnhookAttrChangeEvent(HighlighterAttrChanged);
{$IFDEF SYN_COMPILER_5_UP}
      FHighlighter.RemoveFreeNotification(Self);
{$ENDIF}
    end;
    if Assigned(Value) then
    begin
      Value.HookAttrChangeEvent(HighlighterAttrChanged);
      Value.FreeNotification(Self);
    end;
    FHighlighter := Value;
    if not(csDestroying in ComponentState) then
      HighlighterAttrChanged(FHighlighter);

{$IFDEF SYN_CodeFolding}
    //  Disable Code Folding if not supported by highlighter
    OldUseCodeFolding := fUseCodeFolding;
    UseCodeFolding := False;
    UseCodeFolding := OldUseCodeFolding;
    if fHighlighter is TSynCustomCodeFoldingHighlighter then
      TSynCustomCodeFoldingHighlighter(fHighlighter).InitFoldRanges(fAllFoldRanges);
{$ENDIF}
  end;
end;

procedure TCustomSynEdit.SetBorderStyle(Value: TSynBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCustomSynEdit.SetHideSelection(const Value: Boolean);
begin
  if FHideSelection <> Value then
  begin
    FHideSelection := Value;
    InvalidateSelection;
  end;
end;

procedure TCustomSynEdit.SetInsertMode(const Value: Boolean);
begin
  if FInserting <> Value then
  begin
    FInserting := Value;
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
        cw := FCharWidth;
        ch := 2;
        FCaretOffset := Point(0, FTextHeight - 2);
      end;
    ctHalfBlock:
      begin
        cw := FCharWidth;
        ch := (FTextHeight - 2) div 2;
        FCaretOffset := Point(0, ch);
      end;
    ctBlock:
      begin
        cw := FCharWidth;
        ch := FTextHeight - 2;
        FCaretOffset := Point(0, 0);
      end;
    ctVerticalLine2:
      begin
      cw := 2;
      ch := FTextHeight + 1;
      FCaretOffset := Point(0, 0);
      end;
    else
    begin // ctVerticalLine
      cw := 2;
      ch := FTextHeight - 2;
      FCaretOffset := Point(-1, 0);
    end;
  end;
  Exclude(FStateFlags, sfCaretVisible);

  if Focused or FAlwaysShowCaret then
  begin
    CreateCaret(Handle, 0, cw, ch);
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
    FMaxScrollWidth := Value;
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
        TmpMiddle := FLinesInWindow div 2;
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
    FKeyStrokes.Clear
  else
    FKeyStrokes.Assign(Value);
end;

procedure TCustomSynEdit.SetDefaultKeystrokes;
begin
  FKeyStrokes.ResetDefaults;
end;

// If the translations requires Data, memory will be allocated for it via a
// GetMem call.  The client must call FreeMem on Data if it is not NIL.

function TCustomSynEdit.TranslateKeyCode(Code: Word; Shift: TShiftState;
  var Data: Pointer): TSynEditorCommand;
var
  i: Integer;
{$IFNDEF SYN_COMPILER_3_UP}
const
  VK_ACCEPT = $30;
{$ENDIF}
begin
  i := KeyStrokes.FindKeycode2(FLastKey, FLastShiftState, Code, Shift);
  if i >= 0 then
    Result := KeyStrokes[i].Command
  else begin
    i := Keystrokes.FindKeycode(Code, Shift);
    if i >= 0 then
      Result := Keystrokes[i].Command
    else
      Result := ecNone;
  end;
  if (Result = ecNone) and (Code >= VK_ACCEPT) and (Code <= VK_SCROLL) then
  begin
    FLastKey := Code;
    FLastShiftState := Shift;
  end
  else
  begin
    FLastKey := 0;
    FLastShiftState := [];
  end;
end;

procedure TCustomSynEdit.CommandProcessor(Command: TSynEditorCommand;
  AChar: WideChar; Data: Pointer);
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
  Data: Pointer);

  procedure SetSelectedTextEmpty;
  var
    vSelText: UnicodeString;
    vUndoBegin, vUndoEnd: TBufferCoord;
  begin
    vUndoBegin := FBlockBegin;
    vUndoEnd := FBlockEnd;
    vSelText := SelText;
    SetSelTextPrimitive('');
    if (vUndoBegin.Line < vUndoEnd.Line) or (
      (vUndoBegin.Line = vUndoEnd.Line) and (vUndoBegin.Char < vUndoEnd.Char)) then
    begin
      FUndoList.AddChange(crDelete, vUndoBegin, vUndoEnd, vSelText,
        FActiveSelectionMode);
    end
    else
    begin
      FUndoList.AddChange(crDeleteAfterCursor, vUndoBegin, vUndoEnd, vSelText,
        FActiveSelectionMode);
    end;
  end;

  procedure ForceCaretX(aCaretX: Integer);
  var
    vRestoreScroll: Boolean;
  begin
    vRestoreScroll := not (eoScrollPastEol in FOptions);
    Include(FOptions, eoScrollPastEol);
    try
      InternalCaretX := aCaretX;
    finally
      if vRestoreScroll then
        Exclude(FOptions, eoScrollPastEol);
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
  vTabTrim: Integer;
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
          counter := FLinesInWindow shr Ord(eoHalfPageScroll in FOptions);
          if eoScrollByOneLess in FOptions then
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
// Word selection
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
                if eoSmartTabDelete in FOptions then
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
                      if (eoTrimTrailingSpaces in Options) then
                        while BackCounter >= 0 do
                        begin
                          SpaceCount2 := LeftSpacesEx(Lines[BackCounter], True);
                          if (SpaceCount2 > 0) and (SpaceCount2 < SpaceCount1) then
                            Break;
                          Dec(BackCounter);
                        end
                      else
                        while BackCounter >= 0 do
                        begin
                          SpaceCount2 := LeftSpaces(Lines[BackCounter]);
                          if (SpaceCount2 > 0) and (SpaceCount2 < SpaceCount1) then
                            Break;
                          Dec(BackCounter);
                        end;
                      if (BackCounter = -1) and (SpaceCount2 > SpaceCount1) then
                        SpaceCount2 := 0;
                    end;
                    if SpaceCount2 = SpaceCount1 then
                      SpaceCount2 := 0;
                    FCaretX := FCaretX - (SpaceCount1 - SpaceCount2);
                    UpdateLastCaretX;
                    FStateFlags := FStateFlags + [sfCaretChanged];
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
                  if eoSmartTabDelete in FOptions then
                  begin
                    // unindent
                    if SpaceCount1 > 0 then
                    begin
                      BackCounter := CaretY - 2;
                      //It's better not to have if statement inside loop
                      if (eoTrimTrailingSpaces in Options) then
                        while BackCounter >= 0 do
                        begin
                          SpaceCount2 := LeftSpacesEx(Lines[BackCounter], True);
                          if (SpaceCount2 > 0) and (SpaceCount2 < SpaceCount1) then
                            Break;
                          Dec(BackCounter);
                        end
                      else
                        while BackCounter >= 0 do
                        begin
                          SpaceCount2 := LeftSpaces(Lines[BackCounter]);
                          if (SpaceCount2 > 0) and (SpaceCount2 < SpaceCount1) then
                            Break;
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
                  FCaretX := FCaretX - (SpaceCount1 - SpaceCount2);
                  UpdateLastCaretX;
                  // Stores the previous "expanded" CaretX if the line contains tabs.
                  if (eoTrimTrailingSpaces in Options) and (Len <> Length(TabBuffer)) then
                    vTabTrim := CharIndex2CaretPos(CaretX, TabWidth, Temp);
                  ProperSetLine(CaretY - 1, Temp);
                  FStateFlags := FStateFlags + [sfCaretChanged];
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
                FUndoList.AddChange(crSilentDelete, CaretXY, Caret, Helper,
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
              FUndoList.AddChange(crSilentDeleteAfterCursor, CaretXY, Caret,
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
            if (WP.Char < CaretX) or ((WP.Char = CaretX) and (WP.Line < FLines.Count)) then
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
            FUndoList.AddChange(crSilentDeleteAfterCursor, CaretXY, WP,
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
            FUndoList.AddChange(crSilentDelete, WP, CaretXY, Helper,
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
            FUndoList.AddChange(crSilentDeleteAfterCursor, BufferCoord(1, CaretY),
              BufferCoord(Length(Helper) + 1, CaretY), Helper, smNormal);
          end
          else begin
            Lines.Delete(CaretY - 1);
            Helper := Helper + #13#10;
            FUndoList.AddChange(crSilentDeleteAfterCursor, BufferCoord(1, CaretY),
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
            iUndoBegin := FBlockBegin;
            iUndoEnd := FBlockEnd;
            SetSelTextPrimitive('');
            FUndoList.AddChange(crDelete, iUndoBegin, iUndoEnd, Helper,
              FActiveSelectionMode);
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
                FUndoList.AddChange(crLineBreak, CaretXY, CaretXY, Temp2,
                  smNormal);
                if Command = ecLineBreak then
                  InternalCaretXY := BufferCoord(
                    Length(GetLeftSpacing(SpaceCount1,true)) + 1,
                    CaretY + 1);
              end
              else begin
                Lines.Insert(CaretY - 1, '');
                FUndoList.AddChange(crLineBreak, CaretXY, CaretXY, Temp2,
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

              FUndoList.AddChange(crLineBreak, Caret, Caret, '', smNormal);   //KV
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
            if FLines.Count = 0 then
              FLines.Add('');
            SpaceCount2 := 0;
            if eoAutoIndent in Options then
            begin
              BackCounter := CaretY - 1;
              while BackCounter >= 0 do
              begin
                SpaceCount2 := LeftSpacesEx(Lines[BackCounter],True);
                if Length(Lines[BackCounter]) > 0 then
                  Break;
                Dec(BackCounter);
              end;
            end;
            Lines.Insert(CaretY - 1, '');
            FUndoList.AddChange(crLineBreak, CaretXY, CaretXY, '', smNormal);
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
              iUndoBegin := FBlockBegin;
              iUndoEnd := FBlockEnd;
              StartOfBlock := BlockBegin;
              if FActiveSelectionMode = smLine then
                StartOfBlock.Char := 1;
              FUndoList.AddChange(crDelete, iUndoBegin, iUndoEnd, Helper,
                FActiveSelectionMode);
              SetSelTextPrimitive(AChar);
              if FActiveSelectionMode <> smColumn then
              begin
                FUndoList.AddChange(crInsert, StartOfBlock, BlockEnd, '',
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
                SpaceBuffer := UnicodeStringOfChar(#32, CaretX - Len - Ord(FInserting))
              else
                SpaceBuffer := GetLeftSpacing(CaretX - Len - Ord(FInserting), True);
              SpaceCount2 := Length(SpaceBuffer);

              Temp := Temp + SpaceBuffer;
            end;
            // Added the check for whether or not we're in insert mode.
            // If we are, we append one less space than we would in overwrite mode.
            // This is because in overwrite mode we have to put in a final space
            // character which will be overwritten with the typed character.  If we put the
            // extra space in in insert mode, it would be left at the end of the line and
            // cause problems unless eoTrimTrailingSpaces is set.
            bChangeScroll := not (eoScrollPastEol in FOptions);
            try
              if bChangeScroll then Include(FOptions, eoScrollPastEol);
              StartOfBlock := CaretXY;

              if FInserting then
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
                    FUndoList.AddChange(crWhiteSpaceAdd, EndOfBlock, StartOfBlock, '',
                      smNormal);
                    StartOfBlock.Char := StartOfBlock.Char + SpaceCount2;

                    FUndoList.AddChange(crInsert, StartOfBlock, CaretXY, '',
                      smNormal);
                  finally
                    EndUndoBlock;
                  end;
                end
                else begin
                  FUndoList.AddChange(crInsert, StartOfBlock, CaretXY, '',
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
                FUndoList.AddChange(crInsert, StartOfBlock, CaretNew, Helper,
                  smNormal);
                InternalCaretX := CaretX + 1;
              end;
              if CaretX >= LeftChar + FCharsInWindow then
                LeftChar := LeftChar + Min(25, FCharsInWindow - 1);
            finally
              if bChangeScroll then Exclude(FOptions, eoScrollPastEol);
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
      ecToggleCaseBlock,
      ecTitleCaseBlock:
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
            if assigned(FBookMarks[CX]) then
            begin
              moveBkm := (FBookMarks[CX].Line <> Caret.Line);
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
          if Assigned (FOnContextHelp) then
            FOnContextHelp (self,WordAtCursor);
        end;
      ecImeStr:
        if not ReadOnly then
        begin
          SetString(S, PWideChar(Data), WStrLen(Data));
          if SelAvail then
          begin
            BeginUndoBlock;
            try
              FUndoList.AddChange(crDelete, FBlockBegin, FBlockEnd, SelText,
                smNormal);
              StartOfBlock := BlockBegin;
              EndOfBlock.Line := BlockBegin.Line;
              EndOfBlock.Char := BlockBegin.Char + Length(s);
              SetSelTextPrimitive(s);
              FUndoList.AddChange(crInsert, StartOfBlock, EndOfBlock, '',
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
            bChangeScroll := not (eoScrollPastEol in FOptions);
            try
              if bChangeScroll then Include(FOptions, eoScrollPastEol);
              StartOfBlock := CaretXY;
              Len := Length(s);
              if not FInserting then
              begin
                Helper := Copy(Temp, CaretX, Len);
                Delete(Temp, CaretX, Len);
              end;
              Insert(s, Temp, CaretX);
              InternalCaretX := (CaretX + Len);
              ProperSetLine(CaretY - 1, Temp);
              if FInserting then
                Helper := '';
              FUndoList.AddChange(crInsert, StartOfBlock, CaretXY, Helper,
                smNormal);
              if CaretX >= LeftChar + FCharsInWindow then
                LeftChar := LeftChar + min(25, FCharsInWindow - 1);
            finally
              if bChangeScroll then Exclude(FOptions, eoScrollPastEol);
            end;
          end;
        end;
{$IFDEF SYN_CodeFolding}
      ecFoldAll: begin CollapseAll; end;
      ecUnfoldAll: begin UncollapseAll; end;
      ecFoldNearest: begin CollapseNearest; end;
      ecUnfoldNearest: begin UncollapseNearest; end;
      ecFoldLevel1: begin CollapseLevel(1); end;
      ecFoldLevel2: begin CollapseLevel(2); end;
      ecFoldLevel3: begin CollapseLevel(3); end;
      ecUnfoldLevel1: begin UncollapseLevel(1); end;
      ecUnfoldLevel2: begin UncollapseLevel(2); end;
      ecUnfoldLevel3: begin UncollapseLevel(3); end;
      ecFoldRegions: begin CollapseFoldType(FoldRegionType) end;
      ecUnfoldRegions: begin UnCollapseFoldType(FoldRegionType) end;
{$ENDIF}
    end;
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.DoOnCommandProcessed(Command: TSynEditorCommand;
  AChar: WideChar; Data: Pointer);
begin
  if Assigned(FOnCommandProcessed) then
    FOnCommandProcessed(Self, Command, AChar, Data);
end;

procedure TCustomSynEdit.DoOnProcessCommand(var Command: TSynEditorCommand;
  var AChar: WideChar; Data: Pointer);
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
  FMarkList.Clear; // FMarkList.Clear also frees all bookmarks,
  FillChar(FBookMarks, sizeof(FBookMarks), 0); // so FBookMarks should be cleared too
  FUndoList.Clear;
  FRedoList.Clear;
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
      // find next Word-break-char if current char is an IdentChar
      if IsIdentChar(Line[CX]) then
        CX := StrScanForCharInCategory(Line, CX, IsWordBreakChar);
      // if Word-break-char found, find the next IdentChar
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
    // if no Word-break-char is found just position at the end of the line
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
      // if previous char is a Word-break-char search for the last IdentChar
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
    FSelectionMode := Value;
    ActiveSelectionMode := Value;
  end;
end;

procedure TCustomSynEdit.SetActiveSelectionMode(const Value: TSynSelectionMode);
begin
  if FActiveSelectionMode <> Value then
  begin
    if SelAvail then
      InvalidateSelection;
    FActiveSelectionMode := Value;
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
  FUndoList.BeginBlock;
end;

procedure TCustomSynEdit.BeginUpdate;
begin
  IncPaintLock;
end;

procedure TCustomSynEdit.EndUndoBlock;
begin
  FUndoList.EndBlock;
end;

procedure TCustomSynEdit.EndUpdate;
begin
  DecPaintLock;
end;

procedure TCustomSynEdit.AddKey(Command: TSynEditorCommand;
  Key1: Word; SS1: TShiftState; Key2: Word; SS2: TShiftState);
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

procedure TCustomSynEdit.MarkModifiedLinesAsSaved;
begin
  TSynEditStringList(FLines).MarkModifiedLinesAsSaved;
  if FGutter.ShowModification then
    InvalidateGutter;
end;

function TCustomSynEdit.GetSelStart: Integer;
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
        Windows.DestroyCaret;
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
  FExtraLineSpacing := Value;
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
  FUndoList.Clear;
  FRedoList.Clear;
end;

procedure TCustomSynEdit.SetSelTextExternal(const Value: UnicodeString);
var
  StartOfBlock, EndOfBlock: TBufferCoord;
begin
  BeginUndoBlock;
  try
    if SelAvail then
    begin
      FUndoList.AddChange(crDelete, FBlockBegin, FBlockEnd,
        SelText, FActiveSelectionMode);
    end
    else
      ActiveSelectionMode := SelectionMode;
    StartOfBlock := BlockBegin;
    EndOfBlock := BlockEnd;
    FBlockBegin := StartOfBlock;
    FBlockEnd := EndOfBlock;
    SetSelTextPrimitive(Value);
    if (Value <> '') and (FActiveSelectionMode <> smColumn) then
      FUndoList.AddChange(crInsert, StartOfBlock, BlockEnd, '', FActiveSelectionMode);
  finally
    EndUndoBlock;
  end;
end;

procedure TCustomSynEdit.SetGutter(const Value: TSynGutter);
begin
  FGutter.Assign(Value);
end;

procedure TCustomSynEdit.GutterChanged(Sender: TObject);
var
  nW: Integer;
begin
  if not (csLoading in ComponentState) then
  begin
    if FGutter.ShowLineNumbers and FGutter.AutoSize then
      FGutter.AutoSizeDigitCount(Lines.Count);
    if FGutter.UseFontStyle then
    begin
      FTextDrawer.SetBaseFont(FGutter.Font);
      nW := FGutter.RealGutterWidth(FTextDrawer.CharWidth);
      FTextDrawer.SetBaseFont(Font);
    end
    else
      nW := FGutter.RealGutterWidth(FCharWidth);
    if nW = FGutterWidth then
      InvalidateGutter
    else
      SetGutterWidth(nW);
  end;
end;

procedure TCustomSynEdit.LockUndo;
begin
  FUndoList.Lock;
  FRedoList.Lock;
end;

procedure TCustomSynEdit.UnlockUndo;
begin
  FUndoList.Unlock;
  FRedoList.Unlock;
end;

function TCustomSynEdit.UnifiedSelection: TBufferBlock;
begin
  if BlockBegin.Line > BlockEnd.Line then begin
    Result.BeginLine := BlockEnd.Line;
    Result.EndLine := BlockBegin.Line;
  end else begin
    Result.BeginLine := BlockBegin.Line;
    Result.EndLine := BlockEnd.Line;
  end;
  if BlockBegin.Char > BlockEnd.Char then begin
    Result.BeginChar := BlockEnd.Char;
    Result.EndChar := BlockBegin.Char;
  end else begin
    Result.BeginChar := BlockBegin.Char;
    Result.EndChar := BlockEnd.Char;
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
    Exit;

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
    nDelta := LinesInWindow shr Ord(eoHalfPageScroll in FOptions);

  Inc(FMouseWheelAccumulator, SmallInt(Msg.wParamHi));
  nWheelClicks := FMouseWheelAccumulator div WHEEL_DELTA;
  FMouseWheelAccumulator := FMouseWheelAccumulator mod WHEEL_DELTA;
  if (nDelta = Integer(WHEEL_PAGESCROLL)) or (nDelta > LinesInWindow) then
    nDelta := LinesInWindow;
  TopLine := TopLine - (nDelta * nWheelClicks);
  Update;
  if Assigned(OnScroll) then OnScroll(Self,sbVertical);
end;
{$ENDIF}

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

procedure TCustomSynEdit.SetTabWidth(Value: Integer);
begin
  Value := MinMax(Value, 1, 256);
  if (Value <> FTabWidth) then begin
    FTabWidth := Value;
    TSynEditStringList(Lines).TabWidth := Value;
    Invalidate; // to redraw text containing tab chars
    if WordWrap then
    begin
      FWordWrapPlugin.Reset;
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
  nSearchLen, nReplaceLen, n, nFound: Integer;
  nInLine: Integer;
  bBackward, bFromCursor: Boolean;
  bPrompt: Boolean;
  bReplace, bReplaceAll: Boolean;
  bEndUndoBlock: Boolean;
  nAction: TSynReplaceAction;
  iResultOffset: Integer;

  function InValidSearchRange(First, Last: Integer): Boolean;
  begin
    Result := True;
    if (FActiveSelectionMode = smNormal) or not (ssoSelectedOnly in AOptions) then
    begin
      if ((ptCurrent.Line = ptStart.Line) and (First < ptStart.Char)) or
        ((ptCurrent.Line = ptEnd.Line) and (Last > ptEnd.Char))
      then
        Result := False;
    end
    else
    if (FActiveSelectionMode = smColumn) then
      // solves bug in search/replace when smColumn mode active and no selection
      Result := (First >= ptStart.Char) and (Last <= ptEnd.Char) or (ptEnd.Char - ptStart.Char < 1);
  end;

begin
  if not Assigned(FSearchEngine) then
    raise ESynEditError.Create('No search engine has been assigned');

  Result := 0;
  // can't search for or replace an empty string
  if Length(ASearch) = 0 then Exit;
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
    if (FActiveSelectionMode = smLine) then
    begin
      ptStart.Char := 1;
      ptEnd.Char := Length(Lines[ptEnd.Line - 1]) + 1;
    end
    else if (FActiveSelectionMode = smColumn) then
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
  FSearchEngine.Options := AOptions;
  FSearchEngine.Pattern := ASearch;
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
  Inc(FPaintTransientLock);
  try
    while (ptCurrent.Line >= ptStart.Line) and (ptCurrent.Line <= ptEnd.Line) do
    begin
      nInLine := FSearchEngine.FindAll(Lines[ptCurrent.Line - 1]);
      iResultOffset := 0;
      if bBackward then
        n := Pred(FSearchEngine.ResultCount)
      else
        n := 0;
      // Operate on all results in this line.
      while nInLine > 0 do
      begin
        // An occurrence may have been replaced with a text of different length
        nFound := FSearchEngine.Results[n] + iResultOffset;
        nSearchLen := FSearchEngine.Lengths[n];
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
        if not (bReplace or bReplaceAll) then Exit;
        // Prompt and replace or replace all.  If user chooses to replace
        // all after prompting, turn off prompting.
        if bPrompt and Assigned(FOnReplaceText) then
        begin
          nAction := DoOnReplaceText(ASearch, AReplace, ptCurrent.Line, nFound);
          if nAction = raCancel then
            Exit;
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
            if bEndUndoBlock = False then
              BeginUndoBlock;
            bEndUndoBlock := true;
          end;
          // Allow advanced substition in the search engine
          SelText := FSearchEngine.Replace(SelText, AReplace);
          nReplaceLen := CaretX - nFound;
        end;
        // fix the caret position and the remaining results
        if not bBackward then begin
          InternalCaretX := nFound + nReplaceLen;
          if (nSearchLen <> nReplaceLen) and (nAction <> raSkip) then
          begin
            Inc(iResultOffset, nReplaceLen - nSearchLen);
            if (FActiveSelectionMode <> smColumn) and (CaretY = ptEnd.Line) then
            begin
              Inc(ptEnd.Char, nReplaceLen - nSearchLen);
              BlockEnd := ptEnd;
            end;
          end;
        end;
        if not bReplaceAll then
          Exit;
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
    Dec(FPaintTransientLock);
    DoOnPaintTransient( ttAfter );
  end;
end;

function TCustomSynEdit.IsPointInSelection(const Value: TBufferCoord): Boolean;
var
  ptBegin, ptEnd: TBufferCoord;
begin
  ptBegin := BlockBegin;
  ptEnd := BlockEnd;
  if (Value.Line >= ptBegin.Line) and (Value.Line <= ptEnd.Line) and
    ((ptBegin.Line <> ptEnd.Line) or (ptBegin.Char <> ptEnd.Char)) then
  begin
    if FActiveSelectionMode = smLine then
      Result := True
    else if (FActiveSelectionMode = smColumn) then
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
  if (FFocusList.Count > 0) then
  begin
    if TWinControl (FFocusList.Last).CanFocus then
      TWinControl (FFocusList.Last).SetFocus;
    Exit;
  end;
  inherited;
end;

procedure TCustomSynEdit.UpdateMouseCursor;

var
  ptCursor: TPoint;
  ptLineCol: TBufferCoord;
  iNewCursor: TCursor;
{$IFDEF SYN_CodeFolding}
  ptRowCol: TDisplayCoord;
  Rect: TRect;
{$ENDIF}
begin
  GetCursorPos(ptCursor);
  ptCursor := ScreenToClient(ptCursor);
{$IFDEF SYN_CodeFolding}
  ptRowCol := PixelsToRowColumn(ptCursor.X, ptCursor.Y);
  ptLineCol := DisplayToBufferPos(ptRowCol);
  if (ptCursor.X < fGutterWidth) then begin
    if UseCodeFolding and
      fAllFoldRanges.FoldStartAtLine(ptLineCol.Line) then
    begin
      Rect := GetFoldShapeRect(ptRowCol.Row);
      if PtInRect(Rect, ptCursor) then
        SetCursor(Screen.Cursors[crHandPoint])
      else
        SetCursor(Screen.Cursors[fGutter.Cursor]);
    end else
      SetCursor(Screen.Cursors[fGutter.Cursor])
  end else begin
    if (eoDragDropEditing in fOptions) and (not MouseCapture) and IsPointInSelection(ptLineCol) then
      iNewCursor := crArrow
    else if UseCodeFolding and CodeFolding.ShowHintMark and
      fAllFoldRanges.CollapsedFoldStartAtLine(ptLineCol.Line) then
    begin
      Rect := GetCollapseMarkRect(ptRowCol.Row, ptLineCol.Line);
      if PtInRect(Rect, ptCursor) then
        iNewCursor := crHandPoint;
    end else
{$ELSE}
  if (ptCursor.X < FGutterWidth) then
    SetCursor(Screen.Cursors[FGutter.Cursor])
  else begin
    ptLineCol := DisplayToBufferPos(PixelsToRowColumn(ptCursor.X, ptCursor.Y));
    if (eoDragDropEditing in FOptions) and (not MouseCapture) and not (sfMouseCaptured in FStateFlags) and IsPointInSelection(ptLineCol) then
      iNewCursor := crArrow
    else
{$ENDIF}
      iNewCursor := Cursor;
    if Assigned(OnMouseCursor) then
      OnMouseCursor(Self, ptLineCol, iNewCursor);
    FKbdHandler.ExecuteMouseCursor(Self, ptLineCol, iNewCursor);
    SetCursor(Screen.Cursors[iNewCursor]);
  end;
end;

procedure TCustomSynEdit.BookMarkOptionsChanged(Sender: TObject);
begin
  InvalidateGutter;
end;

function TCustomSynEdit.GetOptions: TSynEditorOptions;
begin
  Result := FOptions;
end;

procedure TCustomSynEdit.SetOptions(Value: TSynEditorOptions);
const
  ScrollOptions = [eoDisableScrollArrows,eoHideShowScrollbars,
    eoScrollPastEof,eoScrollPastEol];
var
  bSetDrag: Boolean;
  TmpBool: Boolean;
  bUpdateScroll: Boolean;
  vTempBlockBegin, vTempBlockEnd : TBufferCoord;
begin
  if (Value <> FOptions) then
  begin
    bSetDrag := (eoDropFiles in FOptions) <> (eoDropFiles in Value);

    if not (eoScrollPastEol in Options) then
      LeftChar := LeftChar;
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;

    bUpdateScroll := (Options * ScrollOptions) <> (Value * ScrollOptions);

    FOptions := Value;

    // constrain caret position to MaxScrollWidth if eoScrollPastEol is enabled
    InternalCaretXY := CaretXY;
    if (eoScrollPastEol in Options) then
    begin
      vTempBlockBegin := BlockBegin;
      vTempBlockEnd := BlockEnd;
      SetBlockBegin(vTempBlockBegin);
      SetBlockEnd(vTempBlockEnd);
    end;

    // (un)register HWND as drop target
    if bSetDrag and not (csDesigning in ComponentState) and HandleAllocated then
      DragAcceptFiles(Handle, (eoDropFiles in FOptions));

    TmpBool := eoShowSpecialChars in Value;
    if TmpBool <> FShowSpecChar then
    begin
      FShowSpecChar := TmpBool;
      Invalidate;
    end;
    if bUpdateScroll then
      UpdateScrollBars;
  end;
end;

procedure TCustomSynEdit.SizeOrFontChanged(bFont: Boolean);
begin
  if HandleAllocated and (FCharWidth <> 0) then
  begin
    FCharsInWindow := Max(ClientWidth - FGutterWidth - 2, 0) div FCharWidth;
    FLinesInWindow := ClientHeight div FTextHeight;
    if WordWrap then
    begin
      FWordWrapPlugin.DisplayChanged;
      Invalidate;
    end;
    if bFont then
    begin
      if Gutter.ShowLineNumbers then
        GutterChanged(Self)
      else
        UpdateScrollbars;
      InitializeCaret;
      Exclude(FStateFlags, sfCaretChanged);
      Invalidate;
    end
    else
      UpdateScrollbars;
    Exclude(FStateFlags, sfScrollbarChanged);
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
      if FCaretAtEOL then
      begin
        FCaretAtEOL := False;
        UpdateLastCaretX;
        IncPaintLock;
        Include(FStateFlags, sfCaretChanged);
        DecPaintLock;
        Exit;
      end;
    end
    else
    begin // DX < 0. Handle ecLeft/ecPageLeft at BOL.
      if (not FCaretAtEOL) and (CaretX > 1) and (DisplayX = 1) then
      begin
        FCaretAtEOL := True;
        UpdateLastCaretX;
        if DisplayX > CharsInWindow +1 then
          SetInternalDisplayXY( DisplayCoord(CharsInWindow +1, DisplayY) )
        else begin
          IncPaintLock;
          Include(FStateFlags, sfCaretChanged);
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
  bChangeY := not (eoScrollPastEol in FOptions);
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
  MoveCaretAndSelection(FBlockBegin, ptDst, SelectionCommand);
  // if caret is beyond CharsInWindow move to next row (this means there are
  // spaces/tabs at the end of the row)
  if WordWrap and (DX > 0) and (CaretX < Length(LineText)) then
  begin
    vCaretRowCol := DisplayXY;
    if (vCaretRowCol.Column = 1) and (LineToRow(CaretY) <> vCaretRowCol.Row) then
    begin
      FCaretAtEOL := True;
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
      ptDst.Column := FLastCaretX;
  end;
  vDstLineChar := DisplayToBufferPos(ptDst);
  SaveLastCaretX := FLastCaretX;

  // set caret and block begin / end
  IncPaintLock;
  MoveCaretAndSelection(FBlockBegin, vDstLineChar, SelectionCommand);
  if WordWrap then
  begin
    vEOLTestPos := BufferToDisplayPos(vDstLineChar);
    FCaretAtEOL := (vEOLTestPos.Column = 1) and (vEOLTestPos.Row <> ptDst.Row);
  end;
  DecPaintLock;

  // Restore FLastCaretX after moving caret, since UpdateLastCaretX, called by
  // SetCaretXYEx, changes them. This is the one case where we don't want that.
  FLastCaretX := SaveLastCaretX;
end;

procedure TCustomSynEdit.MoveCaretAndSelection(const ptBefore, ptAfter: TBufferCoord;
  SelectionCommand: Boolean);
begin
  if (eoGroupUndo in FOptions) and UndoList.CanUndo then
    FUndoList.AddGroupBreak;

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
  vOldMode := FActiveSelectionMode;
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
  Done: Boolean;
begin
  FillChar(iHasStyle, SizeOf(iHasStyle), 0);
  if Assigned(FHighlighter) and (FHighlighter.AttrCount > 0) then
  begin
    for cAttr := 0 to FHighlighter.AttrCount - 1 do
    begin
      iCurr := FHighlighter.Attribute[cAttr].Style * [fsItalic, fsBold];
      for cStyle := 0 to 3 do
        if iCurr = iFontStyles[cStyle] then
        begin
          iHasStyle[cStyle] := True;
          Break;
        end;
    end;
  end
  else
  begin
    iCurr := Font.Style * [fsItalic, fsBold];
    for cStyle := 0 to 3 do
      if iCurr = iFontStyles[cStyle] then
      begin
        iHasStyle[cStyle] := True;
        Break;
      end;
  end;

  Done := False;
  FTextHeight := 0;
  FCharWidth := 0;
  FTextDrawer.BaseFont := Self.Font;

  if not Done then
    for cStyle := 0 to 3 do
      if iHasStyle[cStyle] then
      begin
        FTextDrawer.BaseStyle := iFontStyles[cStyle];
        FTextHeight := Max(FTextHeight, FTextDrawer.CharHeight);
        FCharWidth := Max(FCharWidth, FTextDrawer.CharWidth);
      end;
  Inc(FTextHeight, FExtraLineSpacing);
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
  FStatusChanges := FStatusChanges + AChanges;
  if PaintLock = 0 then
    DoOnStatusChange(FStatusChanges);
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

  function TitleCase(const aStr: UnicodeString): UnicodeString;
  var
    i: Integer;
  begin
   Result := SynWideLowerCase(aStr);
   for i := 1 to Length(Result) do
   if (i = 1) or IsWordBreakChar(Result[i-1]) then Result[i] := SynWideUpperCase(Result[i])[1];
  end;

var
  w: UnicodeString;
  oldCaret, oldBlockBegin, oldBlockEnd: TBufferCoord;
  bHadSel : Boolean;
begin
  Assert((Cmd >= ecUpperCase) and (Cmd <= ecTitleCaseBlock));
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
      { Word commands }
      SetSelWord;
      if SelText = '' then
      begin
        { searches a previous Word }
        InternalCaretXY := PrevWordPos;
        SetSelWord;
        if SelText = '' then
        begin
          { try once more since PrevWordPos may have failed last time.
          (PrevWordPos "points" to the end of the previous line instead of the
          beggining of the previous Word if invoked (e.g.) when CaretX = 1) }
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
        ecTitleCase, ecTitleCaseBlock:
          w := TitleCase(w);
      end;
      BeginUndoBlock;
      try
        if bHadSel then
          FUndoList.AddChange(crSelection, oldBlockBegin, oldBlockEnd, '', FActiveSelectionMode)
        else
          FUndoList.AddChange(crSelection, oldCaret, oldCaret, '', FActiveSelectionMode);
        FUndoList.AddChange(crCaret, oldCaret, oldCaret, '', FActiveSelectionMode);
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
  i, MinLen, iLine: Integer;
  PrevLine, Spaces: UnicodeString;
  p: PWideChar;
  NewCaretX: Integer;
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
      if FBlockBegin.Char < FBlockEnd.Char then
        FBlockBegin.Char := 1
      else
        FBlockEnd.Char := 1;
    end;
    Exit;
  end;
  i := 0;
  iLine := 0;
  MinLen := 0;
  vIgnoreSmartTabs := False;
  if eoSmartTabs in FOptions then
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
            if (p^ = #9) or (p^ = #32) then
              Break;
            Inc(i);
            Inc(p);
          until p^ = #0;
          // scan over whitespaces
          if p^ <> #0 then
            repeat
              if (p^ <> #9) and (p^ <> #32) then Break;
              Inc(i);
              Inc(p);
            until p^ = #0;
          Break;
        end;
        Dec(iLine);
      until iLine < 0;
    end
    else
      vIgnoreSmartTabs := True;
  end;
  FUndoList.BeginBlock;
  try
    if SelAvail then
    begin
      FUndoList.AddChange(crDelete, FBlockBegin, FBlockEnd, SelText,
        FActiveSelectionMode);
      SetSelTextPrimitive('');
    end;
    StartOfBlock := CaretXY;

    if i = 0 then
    begin
      if (eoTabsToSpaces in FOptions) then
      begin
        i := TabWidth - (StartOfBlock.Char - 1) mod TabWidth;
        if i = 0 then
          i := TabWidth;
      end
      else
        i := TabWidth;
    end;

    if eoTabsToSpaces in FOptions then
    begin
      Spaces := UnicodeStringOfChar(#32, i);
      NewCaretX := StartOfBlock.Char + i;
    end
    else if (eoTrimTrailingSpaces in Options) and (StartOfBlock.Char > Length(LineText)) then
    begin
      // work-around for trimming Tabs
      nPhysX := BufferToDisplayPos(CaretXY).Column;
      if (eoSmartTabs in FOptions) and not vIgnoreSmartTabs and (iLine > -1) then
      begin
        i := BufferToDisplayPos( BufferCoord(MinLen+i, iLine+1) ).Column;
        nDistanceToTab := i - nPhysX;
      end
      else
        nDistanceToTab := TabWidth - ((nPhysX - 1) mod TabWidth);
      NewCaretX := StartOfBlock.Char + nDistanceToTab;
    end
    else begin
      if (eoSmartTabs in FOptions) and not vIgnoreSmartTabs and (iLine > -1) then
      begin
        Spaces := Copy(FLines[CaretXY.Line - 1], 1, CaretXY.Char - 1);
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
    if FActiveSelectionMode <> smColumn then
    begin
      FUndoList.AddChange(crInsert, StartOfBlock, CaretXY, SelText,
        FActiveSelectionMode);
    end;
  finally
    FUndoList.EndBlock;
  end;

  ChangeScroll := not(eoScrollPastEol in FOptions);
  try
    Include(FOptions, eoScrollPastEol);
    InternalCaretX := NewCaretX;
  finally
    if ChangeScroll then
      Exclude(FOptions, eoScrollPastEol);
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
    Exit;
  end;

  NewX := CaretX;

  if (NewX <> 1) and (eoSmartTabs in FOptions) then
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
            if p^ <> #32 then Break;
            Dec(NewX);
            Dec(p);
          until NewX = 1;
          // scan over non-whitespaces
          if NewX <> 1 then
            repeat
              if p^ = #32 then Break;
              Dec(NewX);
              Dec(p);
            until NewX = 1;
          Break;
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
      Dec(NewX)
    else begin
      while (NewX > DestX) and ((NewX - 1 > LineLen) or (Line[NewX - 1] = #32)) do
        Dec(NewX);
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

    FUndoList.AddChange(crSilentDelete, BufferCoord(NewX, CaretY),
      OldCaretXY, OldSelText, smNormal);

    // KV
    ChangeScroll := not(eoScrollPastEol in FOptions);
    try
      Include(FOptions, eoScrollPastEol);
      InternalCaretX := NewX;
    finally
      if ChangeScroll then
        Exclude(FOptions, eoScrollPastEol);
    end;
  end;
end;

procedure TCustomSynEdit.DoHomeKey(Selection: Boolean);

  function LastCharInRow: Integer;
  var
    vPos: TDisplayCoord;
  begin
    if FLines.Count = 0 then
      Result := 1
    else
    begin
      vPos := DisplayXY;
      vPos.Column := Min(CharsInWindow, FWordWrapPlugin.GetRowLength(vPos.Row) + 1);
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
  if (eoEnhanceHomeKey in FOptions) and (LineToRow(CaretY) = DisplayY) then
  begin
    s := FLines[CaretXY.Line - 1];

    first_nonblank := 1;
    if WordWrap then
      vMaxX := LastCharInRow() -1
    else
      vMaxX := Length(s);
    while (first_nonblank <= vMaxX) and
      CharInSet(s[first_nonblank], [#32, #9])
    do
      Inc(first_nonblank);
    Dec(first_nonblank);

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
        and (FWordWrapPlugin.GetRowLength(vLastRow) = 0)
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
  if (eoEnhanceEndKey in FOptions) and CaretInLastRow then
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
      vNewCaret.Column := FWordWrapPlugin.GetRowLength(vNewCaret.Row) + 1;
    vNewCaret.Column := Min(CharsInWindow + 1, vNewCaret.Column);
    MoveCaretAndSelection(CaretXY, DisplayToBufferPos(vNewCaret), Selection);
    // Updates FCaretAtEOL flag.
    SetInternalDisplayXY(vNewCaret);
  end
  else
    MoveCaretAndSelection(CaretXY,
      BufferCoord(vNewX, CaretY), Selection);
end;

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

  if (eoDropFiles in FOptions) and not (csDesigning in ComponentState) then
    DragAcceptFiles(Handle, True);

  UpdateScrollBars;
end;

procedure TCustomSynEdit.DestroyWnd;
begin
  if (eoDropFiles in FOptions) and not (csDesigning in ComponentState) then
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
  OrgSelectionMode := FActiveSelectionMode;
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
      InsertStrLen := 3 * (e - BB.Line) + 2;
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

    FUndoList.BeginBlock;
    try
      InsertionPos.Line := BB.Line;
      if FActiveSelectionMode = smColumn then
        InsertionPos.Char := Min(BB.Char, BE.Char)
      else
        InsertionPos.Char := 1;
      InsertBlock(InsertionPos, InsertionPos, StrToInsert, True);
      FUndoList.AddChange(crIndent, BB, BE, '', smColumn);
      //We need to save the position of the end block for redo
      FUndoList.AddChange(crIndent,
        BufferCoord(BB.Char + length(Spaces), BB.Line),
        BufferCoord(BE.Char + length(Spaces), BE.Line),
        '', smColumn);
    finally
      FUndoList.EndBlock;
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
      Exit;
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
  OrgSelectionMode := FActiveSelectionMode;
  Len := 0;
  LastIndent := 0;
  if SelAvail then
  begin
    // store current selection detail
    BB := BlockBegin;
    BE := BlockEnd;
    OrgCaretPos := CaretXY;
    x := FCaretX;

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
       if FActiveSelectionMode = smColumn then
         Inc(Line, MinIntValue([BB.Char - 1, BE.Char - 1, Length(Lines[i - 1])]));
       //Instead of doing a UnicodeStringOfChar, we need to get *exactly* what was
       //being deleted incase there is a TabChar
       TmpDelLen := GetDelLen;
       WStrCat(StrToDelete, PWideChar(Copy(Line, 1, TmpDelLen)));
       WStrCat(StrToDelete, PWideChar(UnicodeString(#13#10)));
       if (FCaretY = i) and (x <> 1) then
         x := x - TmpDelLen;
    end;
    Line := PWideChar(Lines[e - 1]);
    if FActiveSelectionMode = smColumn then
      Inc(Line, MinIntValue([BB.Char - 1, BE.Char - 1, Length(Lines[e - 1])]));
    TmpDelLen := GetDelLen;
    WStrCat(StrToDelete, PWideChar(Copy(Line, 1, TmpDelLen)));
    if (FCaretY = e) and (x <> 1) then
      x := x - TmpDelLen;

    FirstIndent := -1;
    FullStrToDelete := nil;
    // Delete string
    if SomethingToDelete then
    begin
      FullStrToDelete := StrToDelete;
      InternalCaretY := BB.Line;
      if FActiveSelectionMode <> smColumn then
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
          Inc(FCaretY);
        end;
        StrToDelete := Run;
      until Run^ = #0;
      LastIndent := Len;
      FUndoList.AddChange(crUnindent, BB, BE, FullStrToDelete, FActiveSelectionMode);
    end;
    // restore selection
    if FirstIndent = -1 then
      FirstIndent := 0;
    //adjust the x position of orgcaretpos appropriately
    if FActiveSelectionMode = smColumn then
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
      else if Action is TEditUndo then
        CommandProcessor(ecUndo, ' ', nil)
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
      else if Action is TEditUndo then
        TEditAction(Action).Enabled := CanUndo
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
        TSearchAction(Action).Enabled := (Text<>'') and assigned(FSearchEngine)
      else if Action is TSearchFind then
        TSearchAction(Action).Enabled := (Text<>'') and assigned(FSearchEngine)
      else if Action is TSearchReplace then
        TSearchAction(Action).Enabled := (Text<>'') and assigned(FSearchEngine);
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
  if Value <> FModified then begin
    FModified := Value;
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
  if Assigned(FOnSpecialLineColors) then
    FOnSpecialLineColors(Self, Line, Result, Foreground, Background);
end;

procedure TCustomSynEdit.DoOnSpecialTokenAttributes(ALine, APos: Integer; const AToken: string; var FG, BG: TColor;
  var AStyle: TFontStyles);
var
  Special: Boolean;
begin
  if Assigned(FOnSpecialTokenAttributes) then
  begin
    Special := False;
    FOnSpecialTokenAttributes(Self, ALine, APos, AToken, Special, FG, BG, AStyle);
  end;
end;

procedure TCustomSynEdit.InvalidateLine(Line: Integer);
var
  rcInval: TRect;
begin
  if (not HandleAllocated) or (Line < 1) or (Line > Lines.Count) or (not Visible) then
    Exit;

{$IFDEF SYN_CodeFolding}
  if UseCodeFolding or WordWrap then
{$ELSE}
  if WordWrap then
{$ENDIF}
  begin
    InvalidateLines(Line, Line);
    Exit;
  end;

  if (Line >= TopLine) and (Line <= TopLine + LinesInWindow) then
  begin
    // invalidate text area of this line
    rcInval := Rect(FGutterWidth, FTextHeight * (Line - TopLine), ClientWidth, 0);
    rcInval.Bottom := rcInval.Top + FTextHeight;
    if sfLinesChanging in FStateFlags then
      UnionRect(fInvalidateRect, rcInval, fInvalidateRect)
    else
      InvalidateRect(rcInval, False);
  end;
end;

function TCustomSynEdit.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TCustomSynEdit.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    StatusChanged([scReadOnly]);
  end;
end;

procedure TCustomSynEdit.FindMatchingBracket;
begin
  InternalCaretXY := GetMatchingBracket;
end;

procedure TCustomSynEdit.FontSmoothingChanged;
const
  NONANTIALIASED_QUALITY = 3;
  ANTIALIASED_QUALITY    = 4;
  CLEARTYPE_QUALITY      = 5;
var
  bMethod: Byte;
  lf: TLogFont;
begin
  case FFontSmoothing of
    fsmAntiAlias:
      bMethod := ANTIALIASED_QUALITY;
    fsmClearType:
      bMethod := CLEARTYPE_QUALITY;
    else // fsmNone also
      bMethod := NONANTIALIASED_QUALITY;
  end;
  GetObject(Font.Handle, SizeOf(TLogFont), @lf);
  lf.lfQuality := bMethod;
  Font.Handle := CreateFontIndirect(lf);
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
                    Exit;
                  end;
                end;
              end;
            end;
            // get previous line if possible
            if PosY = 1 then Break;
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
                    Exit;
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
        Break;
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
  var Attri: TSynHighlighterAttributes): Boolean;
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
          Exit;
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
    Entry := THookedCommandHandlerEntry(FHookedCommandHandlers[Result]);
    if Entry.Equals(AHandlerProc) then
      Break;
    Dec(Result);
  end;
end;

function TCustomSynEdit.GetHookedCommandHandlersCount: Integer;
begin
  if Assigned(FHookedCommandHandlers) then
    Result := FHookedCommandHandlers.Count
  else
    Result := 0;
end;

procedure TCustomSynEdit.RegisterCommandHandler(
  const AHandlerProc: THookedCommandEvent; AHandlerData: Pointer);
begin
  if not Assigned(AHandlerProc) then
  begin
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.Create('Event handler is NIL in RegisterCommandHandler');
{$ENDIF}
    Exit;
  end;
  if not Assigned(FHookedCommandHandlers) then
    FHookedCommandHandlers := TObjectList.Create;
  if FindHookedCmdEvent(AHandlerProc) = -1 then
    FHookedCommandHandlers.Add(THookedCommandHandlerEntry.Create(
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
    Exit;
  end;
  i := FindHookedCmdEvent(AHandlerProc);
  if i > -1 then
    FHookedCommandHandlers.Delete(i)
  else
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.CreateFmt('Event handler (%p, %p) is not registered',
      [TMethod(AHandlerProc).Data, TMethod(AHandlerProc).Code]);
{$ENDIF}
end;

procedure TCustomSynEdit.NotifyHookedCommandHandlers(AfterProcessing: Boolean;
  var Command: TSynEditorCommand; var AChar: WideChar; Data: Pointer);
var
  Handled: Boolean;
  i: Integer;
  Entry: THookedCommandHandlerEntry;
begin
  Handled := False;
  for i := 0 to GetHookedCommandHandlersCount - 1 do
  begin
    Entry := THookedCommandHandlerEntry(FHookedCommandHandlers[i]);
    // NOTE: Command should NOT be set to ecNone, because this might interfere
    // with other handlers.  Set Handled to False instead (and check its value
    // to not process the command twice).
    Entry.FEvent(Self, AfterProcessing, Handled, Command, AChar, Data,
      Entry.FData);
  end;
  if Handled then
    Command := ecNone;
end;

procedure TCustomSynEdit.DoOnClearBookmark(var Mark: TSynEditMark);
begin
  if Assigned(FOnClearMark) then
    FOnClearMark(Self, Mark);
end;

procedure TCustomSynEdit.DoOnPaintTransientEx(TransientType: TTransientType; Lock: Boolean);
var
  DoTransient: Boolean;
  i: Integer;
begin
  DoTransient :=(FPaintTransientLock=0);
  if Lock then
  begin
    if (TransientType=ttBefore) then Inc(FPaintTransientLock)
    else
    begin
      Dec(FPaintTransientLock);
      DoTransient :=(FPaintTransientLock=0);
    end;
  end;

  if DoTransient then
  begin
    // plugins
    if FPlugins <> nil then
      for i := 0 to FPlugins.Count - 1 do
        TSynEditPlugin(FPlugins[i]).PaintTransient(Canvas, TransientType);
    // event
    if Assigned(FOnPaintTransient) then
    begin
      Canvas.Font.Assign(Font);
      Canvas.Brush.Color := Color;
      HideCaret;
      try
        FOnPaintTransient(Self, Canvas, TransientType);
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
  if Assigned(FOnPaint) then
  begin
    Canvas.Font.Assign(Font);
    Canvas.Brush.Color := Color;
    FOnPaint(Self, Canvas);
  end;
end;

procedure TCustomSynEdit.DoOnPlaceMark(var Mark: TSynEditMark);
begin
  if Assigned(FOnPlaceMark) then
    FOnPlaceMark(Self, Mark);
end;

function TCustomSynEdit.DoOnReplaceText(const ASearch, AReplace: UnicodeString;
  Line, Column: Integer): TSynReplaceAction;
begin
  Result := raCancel;
  if Assigned(FOnReplaceText) then
    FOnReplaceText(Self, ASearch, AReplace, Line, Column, Result);
end;

procedure TCustomSynEdit.DoOnStatusChange(Changes: TSynStatusChanges);
begin
  if Assigned(FOnStatusChange) then
  begin
    FOnStatusChange(Self, FStatusChanges);
    FStatusChanges := [];
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
  if (Sender = FUndoList) and not FUndoList.InsideRedo and
     (FUndoList.PeekItem<>nil) and (FUndoList.PeekItem.ChangeReason<>crGroupBreak) then
    FRedoList.Clear;
  if TSynEditUndoList(Sender).BlockCount = 0 then
    DoChange;
end;

function TCustomSynEdit.GetWordAtRowCol(XY: TBufferCoord): UnicodeString;

// TODO: consider removing the use of Low/High(string) since this code is anyway not zero-based strings ready
{$IFNDEF SYN_COMPILER_17_UP}
  function Low(AStr: UnicodeString): Integer; {$IFDEF SYN_COMPILER_9_UP}inline;{$ENDIF}
  begin
    Result := 1;
  end;

  function High(AStr: UnicodeString): Integer; {$IFDEF SYN_COMPILER_9_UP}inline;{$ENDIF}
  begin
    Result := Length(AStr);
  end;
{$ENDIF}

var
  Line: UnicodeString;
  Start, Stop: Integer;
begin
  Result := '';
  if (XY.Line >= 1) and (XY.Line <= Lines.Count) then
  begin
    Line := Lines[XY.Line - 1];
    if (Length(Line) > 0) and
       ((XY.Char >= Low(Line)) and (XY.Char <= High(Line))) and
       IsIdentChar(Line[XY.Char]) then
    begin
       Start := XY.Char;
       while (Start > Low(Line)) and IsIdentChar(Line[Start - 1]) do
          Dec(Start);

       Stop := XY.Char + 1;
       while (Stop <= High(Line)) and IsIdentChar(Line[Stop]) do
          Inc(Stop);

       Result := Copy(Line, Start, Stop - Start);
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
        Inc(x, TabWidth - (x mod TabWidth))
      else if i <= l then
      begin
        CountOfAvgGlyphs := CeilOfIntDiv(FTextDrawer.TextWidth(s[i]), FCharWidth);
        Inc(x, CountOfAvgGlyphs);
      end
      else
        Inc(x);
    end;
    Result.Column := x + 1;
  end;
  if WordWrap then
    Result := FWordWrapPlugin.BufferToDisplayPos(TBufferCoord(Result));
{$IFDEF SYN_CodeFolding}
  if UseCodeFolding then
    Result.Row := fAllFoldRanges.FoldLineToRow(Result.Row)
{$ENDIF}
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
    Result := FWordWrapPlugin.DisplayToBufferPos(p)
  else
    Result := TBufferCoord(p);
{$IFDEF SYN_CodeFolding}
  if UseCodeFolding then
    Result.Line := fAllFoldRanges.FoldRowToLine(p.Row);
{$ENDIF}

  if Result.Line <= lines.Count then
  begin
    s := Lines[Result.Line - 1];
    l := Length(s);
    x := 0;
    i := 0;

    while x < Result.Char  do
    begin
      Inc(i);
      if (i <= l) and (s[i] = #9) then
        Inc(x, TabWidth - (x mod TabWidth))
      else if i <= l then
      begin
        CountOfAvgGlyphs := CeilOfIntDiv(FTextDrawer.TextWidth(s[i]), FCharWidth);
        Inc(x, CountOfAvgGlyphs);
      end
      else
        Inc(x);
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
  if FPlugins <> nil then
    for i := 0 to FPlugins.Count - 1 do
      TSynEditPlugin(FPlugins[i]).LinesDeleted(FirstLine, Count);
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
  if FPlugins <> nil then
    for i := 0 to FPlugins.Count - 1 do
      TSynEditPlugin(FPlugins[i]).LinesInserted(FirstLine, Count);
end;

procedure TCustomSynEdit.PluginsAfterPaint(ACanvas: TCanvas; const AClip: TRect;
  FirstLine, LastLine: Integer);
var
  i: Integer;
begin
  if FPlugins <> nil then
    for i := 0 to FPlugins.Count - 1 do
      TSynEditPlugin(FPlugins[i]).AfterPaint(ACanvas, AClip, FirstLine, LastLine);
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
  FKbdHandler.AddKeyUpHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveKeyUpHandler(aHandler: TKeyEvent);
begin
  FKbdHandler.RemoveKeyUpHandler(aHandler);
end;

procedure TCustomSynEdit.AddKeyDownHandler(aHandler: TKeyEvent);
begin
  FKbdHandler.AddKeyDownHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveKeyDownHandler(aHandler: TKeyEvent);
begin
  FKbdHandler.RemoveKeyDownHandler(aHandler);
end;

procedure TCustomSynEdit.AddKeyPressHandler(aHandler: TKeyPressWEvent);
begin
  FKbdHandler.AddKeyPressHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveKeyPressHandler(aHandler: TKeyPressWEvent);
begin
  FKbdHandler.RemoveKeyPressHandler(aHandler);
end;

procedure TCustomSynEdit.AddFocusControl(aControl: TWinControl);
begin
  FFocusList.Add(aControl);
end;

procedure TCustomSynEdit.RemoveFocusControl(aControl: TWinControl);
begin
  FFocusList.Remove(aControl);
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
      #0..#32, '.', ',', ';', ':', '"', '''', WideChar(#$00B4), '`',
      WideChar(#$00B0), '^', '!', '?', '&', '$', '@', WideChar(#$00A7), '%', 
      '#', '~', '[', ']', '(', ')', '{', '}', '<', '>', '-', '=', '+', '*', 
      '/', '\', '|':
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
  if (FSearchEngine <> Value) then
  begin
    FSearchEngine := Value;
    if Assigned(FSearchEngine) then
      FSearchEngine.FreeNotification(Self);
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

  { inside the editor, get the Word under the mouse pointer }
  aPos := DisplayToBufferPos(PixelsToRowColumn(Point.X, Point.Y));
  Result := True;
end;

function TCustomSynEdit.GetWordAtMouse: UnicodeString;
var
  Point: TBufferCoord;
begin
  { Return the Word under the mouse }
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
      Break;
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
  RowCol.Line := Max(0, Min(Lines.Count, RowCol.Line) - 1);
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
  if Assigned(FOnChange) then
    FOnChange(Self);
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
  FKbdHandler.AddMouseDownHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveMouseDownHandler(aHandler: TMouseEvent);
begin
  FKbdHandler.RemoveMouseDownHandler(aHandler);
end;

procedure TCustomSynEdit.AddMouseUpHandler(aHandler: TMouseEvent);
begin
  FKbdHandler.AddMouseUpHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveMouseUpHandler(aHandler: TMouseEvent);
begin
  FKbdHandler.RemoveMouseUpHandler(aHandler);
end;

procedure TCustomSynEdit.ResetModificationIndicator;
begin
  TSynEditStringList(FLines).ResetModificationIndicator;
  if FGutter.ShowModification then
    InvalidateGutter;
end;

{$IFDEF SYN_CodeFolding}

procedure TCustomSynEdit.FullFoldScan;
begin
  if UseCodeFolding then
  begin
    ReScanForFoldRanges(0, fLines.Count -1);
  end;
end;

procedure TCustomSynEdit.ReScanForFoldRanges(FromLine : Integer; ToLine : Integer);
Var
  AdjustedToLine: Integer;
begin
  AdjustedToLine := Max(Min(ToLine, Lines.Count - 1), FromLine);
  fAllFoldRanges.StartScanning;
  ScanForFoldRanges(fAllFoldRanges, fLines, FromLine, AdjustedToLine);
  {  StopScanning recreates AllFoldRanges.
     Normally at this point (sfLinesChanging in fStateFlags) = True
     and StopScanning will be called when LinesChanged is executed }
  if not (sfLinesChanging in fStateFlags) and fAllFoldRanges.StopScanning(fLines) then
  begin
    if Assigned(fHighlighter) and (fHighlighter is TSynCustomCodeFoldingHighlighter) then
      TSynCustomCodeFoldingHighlighter(fHighlighter).AdjustFoldRanges(AllFoldRanges,
      fLines);
    InvalidateGutter;
    Include(fStateFlags, sfScrollbarChanged);
  end;
end;

procedure TCustomSynEdit.ScanForFoldRanges(FoldRanges: TSynFoldRanges;
  LinesToScan: TStrings; FromLine : Integer; ToLine : Integer);
begin
  if Assigned(fHighlighter) and (fHighlighter is TSynCustomCodeFoldingHighlighter) then
    TSynCustomCodeFoldingHighlighter(fHighlighter).ScanForFoldRanges(FoldRanges,
      LinesToScan, FromLine, ToLine);

  if Assigned(fOnScanForFoldRanges) then
     fOnScanForFoldRanges(Self, FoldRanges, LinesToScan, FromLine, ToLine);
end;
{$ENDIF}

procedure TCustomSynEdit.AddMouseCursorHandler(aHandler: TMouseCursorEvent);
begin
  FKbdHandler.AddMouseCursorHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveMouseCursorHandler(aHandler: TMouseCursorEvent);
begin
  FKbdHandler.RemoveMouseCursorHandler(aHandler);
end;

{$IFDEF SYN_COMPILER_6_UP}
procedure TCustomSynEdit.DoSearchFindFirstExecute(Action: TSearchFindFirst);
begin
  FOnFindBeforeSearch := Action.Dialog.OnFind;
  FOnCloseBeforeSearch := Action.Dialog.OnClose;
  FSelStartBeforeSearch := SelStart; FSelLengthBeforeSearch := SelLength;

  Action.Dialog.OnFind := FindDialogFindFirst;
  Action.Dialog.OnClose := FindDialogClose;
  Action.Dialog.Execute();
end;

procedure TCustomSynEdit.DoSearchFindExecute(Action: TSearchFind);
begin
  FOnFindBeforeSearch := Action.Dialog.OnFind;
  FOnCloseBeforeSearch := Action.Dialog.OnClose;

  Action.Dialog.OnFind := FindDialogFind;
  Action.Dialog.OnClose := FindDialogClose;
  Action.Dialog.Execute();
end;

procedure TCustomSynEdit.DoSearchReplaceExecute(Action: TSearchReplace);
begin
  FOnFindBeforeSearch := Action.Dialog.OnFind;
  FOnReplaceBeforeSearch := Action.Dialog.OnReplace;
  FOnCloseBeforeSearch := Action.Dialog.OnClose;

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

  if (SelStart = FSelStartBeforeSearch) and (SelLength = FSelLengthBeforeSearch) then
  begin
    SelStart := 0;
    SelLength := 0;
  end;

  if Sender is TFindDialog then
    if not SearchByFindDialog(TFindDialog(Sender)) and (SelStart = 0) and (SelLength = 0) then
    begin
      SelStart := FSelStartBeforeSearch;
      SelLength := FSelLengthBeforeSearch;
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
  ReplaceText, MessageText: string;
  OldSelStart, OldSelLength: Integer;
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
  TFindDialog(Sender).OnFind := FOnFindBeforeSearch;
  if Sender is TReplaceDialog then
    TReplaceDialog(Sender).OnReplace := FOnReplaceBeforeSearch;
  TFindDialog(Sender).OnClose := FOnCloseBeforeSearch;
end;
{$ENDIF}

function TCustomSynEdit.GetWordWrap: Boolean;
begin
  Result := FWordWrapPlugin <> nil;
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
{$IFDEF SYN_CodeFolding}
     // !!Mutually exclusive with CodeFolding to reduce complexity
    if Value and not UseCodeFolding then
{$ELSE}
    if Value then
{$ENDIF}
    begin
      FWordWrapPlugin := TSynWordWrapPlugin.Create(Self);
      LeftChar := 1;
    end
    else
      FWordWrapPlugin := nil;
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
{$IFDEF SYN_CodeFolding}
  if fWordWrapPlugin = nil then begin
    if fUseCodeFolding then
      Result := LineToRow(Lines.Count)
     else
      Result := Lines.Count
  end else if Lines.Count = 0 then
{$ELSE}
  if FWordWrapPlugin = nil then
    Result := Lines.Count
  else if Lines.Count = 0 then
{$ENDIF}
    Result := 0
  else begin
    Result := FWordWrapPlugin.RowCount;
  end;
end;

function TCustomSynEdit.LineToRow(aLine: Integer): Integer;
var
  vBufferPos: TBufferCoord;
begin
{$IFDEF SYN_CodeFolding}
  if not WordWrap and not UseCodeFolding then
{$ELSE}
  if not WordWrap then
{$ENDIF}
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
{$IFDEF SYN_CodeFolding}
  if not WordWrap and not UseCodeFolding then
{$ELSE}
  if not WordWrap then
{$ENDIF}
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
  FCaretAtEOL := WordWrap and (aPos.Row <= FWordWrapPlugin.RowCount) and
    (aPos.Column > FWordWrapPlugin.GetRowLength(aPos.Row)) and
    (DisplayY <> aPos.Row);
  DecPaintLock;
  UpdateLastCaretX;
end;

procedure TCustomSynEdit.SetWantReturns(Value: Boolean);
begin
  FWantReturns := Value;
end;

procedure TCustomSynEdit.SetWantTabs(Value: Boolean);
begin
  FWantTabs := Value;
end;

procedure TCustomSynEdit.SetWordWrapGlyph(const Value: TSynGlyph);
begin
  FWordWrapGlyph.Assign(Value);
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
  Result := (FBookmarkNum >= 0);
end;

procedure TSynEditMark.SetChar(const Value: Integer);
begin
  FChar := Value;
end;

procedure TSynEditMark.SetImage(const Value: Integer);
begin
  FImage := Value;
  if FVisible and Assigned(FEdit) then
    FEdit.InvalidateGutterLines(FLine, FLine);
end;

procedure TSynEditMark.SetInternalImage(const Value: Boolean);
begin
  FInternalImage := Value;
  if FVisible and Assigned(FEdit) then
    FEdit.InvalidateGutterLines(FLine, FLine);
end;

procedure TSynEditMark.SetLine(const Value: Integer);
begin
  if FVisible and Assigned(FEdit) then
  begin
    if FLine > 0 then
      FEdit.InvalidateGutterLines(FLine, FLine);
    FLine := Value;
    FEdit.InvalidateGutterLines(FLine, FLine);
  end
  else
    FLine := Value;
end;

procedure TSynEditMark.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if Assigned(FEdit) then
      FEdit.InvalidateGutterLines(FLine, FLine);
  end;
end;

constructor TSynEditMark.Create(AOwner: TCustomSynEdit);
begin
  inherited Create;
  FBookmarkNum := -1;
  FEdit := AOwner;
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
  FEdit := AOwner;
end;

function TSynEditMarkList.First: TSynEditMark;
begin
  Result := TSynEditMark(inherited First);
end;

function TSynEditMarkList.Last: TSynEditMark;
begin
  Result := TSynEditMark(inherited Last);
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
      if cnt = MAX_MARKS then
        Break;
    end;
  end;
end;

procedure TSynEditMarkList.Place(mark: TSynEditMark);
begin
  if assigned(FEdit) then
    if Assigned(FEdit.OnPlaceBookmark) then
      FEdit.OnPlaceBookmark(FEdit, mark);
  if assigned(mark) then
    Add(mark);
end;

{ TSynEditPlugin }

constructor TSynEditPlugin.Create(AOwner: TCustomSynEdit);
begin
  inherited Create;
  if AOwner <> nil then
  begin
    FOwner := AOwner;
    if FOwner.FPlugins = nil then
      FOwner.FPlugins := TObjectList.Create;
    FOwner.FPlugins.Add(Self);
  end;
end;

destructor TSynEditPlugin.Destroy;
begin
  if FOwner <> nil then
    FOwner.FPlugins.Extract(Self); // we are being destroyed, FOwner should not free us
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

initialization
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

finalization
{$IFNDEF UNICODE}
  if Win32PlatformIsUnicode and (GetMsgHook <> 0) then
    UnhookWindowsHookEx(GetMsgHook);
{$ENDIF}

end.
