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

$Id: SynEdit.pas,v 1.139 2002/05/24 08:52:18 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:

- Initial WordWrap code is there, but it is incomplete and does not function
  Don't use it yet.
-------------------------------------------------------------------------------}

unit SynEdit;

{$I SynEdit.inc}

interface

uses
  SysUtils,
  Classes,
{$IFDEF SYN_CLX}
  Qt,
  Types,
  QControls,
  QGraphics,
  QForms,
  QStdCtrls,
  QExtCtrls,
  Math,
{$ELSE}
  Controls,
  Graphics,
  Forms,
  StdCtrls,
  ExtCtrls,
  Windows,
  Messages,
{$ENDIF}
{$IFDEF SYN_MBCSSUPPORT}
  Imm,
{$ENDIF}
  SynEditTypes,
  SynEditKeyConst,
  SynEditMiscProcs,
  SynEditMiscClasses,
  SynEditTextBuffer,
  SynEditKeyCmds,
  SynEditSearch,
  SynEditHighlighter,
  SynEditKbdHandler,
{$IFDEF SYN_CLX}
  kTextDrawer
{$ELSE}
  SynTextDrawer
{$ENDIF}
  ;

const
  DIGIT = ['0'..'9'];
// ALPHA            = ['A'..'Z', 'a'..'z'];
// break these up because we exceed the 4 byte limit when combined.
  ALPHA_UC = ['A'..'Z'];
  ALPHA_LC = ['a'..'z'];

{$IFNDEF SYN_COMPILER_3_UP}
   // not defined in all Delphi versions
  WM_MOUSEWHEEL = $020A;
{$ENDIF}

   // maximum scroll range
  MAX_SCROLL = 32767;

// Max number of book/gutter marks returned from GetEditMarksForLine - that
// really should be enough.
  maxMarks = 16;

  SYNEDIT_CLIPBOARD_FORMAT = 'SynEdit Control Block Type';

var
  SynEditClipboardFormat: UINT;

{$IFDEF SYN_MBCSSUPPORT}
{$IFNDEF SYN_COMPILER_4_UP}
{Windows.pas in D4}
const
  C3_NONSPACING = 1; { nonspacing character }
  C3_DIACRITIC = 2; { diacritic mark }
  C3_VOWELMARK = 4; { vowel mark }
  C3_SYMBOL = 8; { symbols }
  C3_KATAKANA = $0010; { katakana character }
  C3_HIRAGANA = $0020; { hiragana character }
  C3_HALFWIDTH = $0040; { half width character }
  C3_FULLWIDTH = $0080; { full width character }
  C3_IDEOGRAPH = $0100; { ideographic character }
  C3_KASHIDA = $0200; { Arabic kashida character }
  C3_LEXICAL = $0400; { lexical character }
  C3_ALPHA = $8000; { any linguistic char (C1_ALPHA) }
  C3_NOTAPPLICABLE = 0; { ctype 3 is not applicable }
{$ENDIF}
{$ENDIF}

type
  TSynSearchOption = (ssoMatchCase, ssoWholeWord, ssoBackwards,
    ssoEntireScope, ssoSelectedOnly, ssoReplace, ssoReplaceAll, ssoPrompt);
  TSynSearchOptions = set of TSynSearchOption;

  TSynReplaceAction = (raCancel, raSkip, raReplace, raReplaceAll);

  ESynEditError = class(Exception);

  TDropFilesEvent = procedure(Sender: TObject; X, Y: integer; AFiles: TStrings)
    of object;

  THookedCommandEvent = procedure(Sender: TObject; AfterProcessing: boolean;
    var Handled: boolean; var Command: TSynEditorCommand; var AChar: char;
    Data: pointer; HandlerData: pointer) of object;

  TPaintEvent = procedure(Sender: TObject; ACanvas: TCanvas) of object;

  TProcessCommandEvent = procedure(Sender: TObject;
    var Command: TSynEditorCommand; var AChar: char; Data: pointer) of object;

  TReplaceTextEvent = procedure(Sender: TObject; const ASearch, AReplace:
    string; Line, Column: integer; var Action: TSynReplaceAction) of object;

  TSpecialLineColorsEvent = procedure(Sender: TObject; Line: integer;
    var Special: boolean; var FG, BG: TColor) of object;

  TTransientType=(ttBefore,ttAfter);                                            //GBN 2001-10-23
  TPaintTransient = procedure(Sender: TObject; Canvas: TCanvas;
    TransientType: TTransientType) of object;

  TSynEditCaretType = (ctVerticalLine, ctHorizontalLine, ctHalfBlock, ctBlock);

  TSynStateFlag = (sfCaretChanged, sfScrollbarChanged, sfLinesChanging,
    sfIgnoreNextChar, sfCaretVisible, sfDblClicked, sfPossibleGutterClick,
    sfWaitForDragging, sfInsideRedo);                                           //mh 2000-10-30

  TScrollHintFormat = (shfTopLineOnly, shfTopToBottom);

  TSynStateFlags = set of TSynStateFlag;

  TSynEditorOption = (
    eoAltSetsColumnMode,       //Holding down the Alt Key will put the selection mode into columnar format
    eoAutoIndent,              //Will indent the caret on new lines with the same amount of leading white space as the preceding line
    eoAutoSizeMaxLeftChar,     //Automatically resizes the max left char when adding lines
    eoDisableScrollArrows,     //Disables the scroll bar arrow buttons when you can't scroll in that direction any more
    eoDragDropEditing,         //Allows you to select a block of text and drag it within the document to another location
    eoDropFiles,               //Allows the editor accept OLE file drops
    eoEnhanceHomeKey,          //enhances home key positioning, similar to visual studio
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

const
  SYNEDIT_DEFAULT_OPTIONS = [eoAutoIndent, eoDragDropEditing, eoScrollPastEol,
    eoShowScrollHint, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces,
    eoSmartTabDelete, eoGroupUndo];

type
// use scAll to update a statusbar when another TCustomSynEdit got the focus
  TSynStatusChange = (scAll, scCaretX, scCaretY, scLeftChar, scTopLine,
    scInsertMode, scModified, scSelection, scReadOnly);
  TSynStatusChanges = set of TSynStatusChange;

  TContextHelpEvent = procedure(Sender: TObject; word : string)
    of object;

  TStatusChangeEvent = procedure(Sender: TObject; Changes: TSynStatusChanges)
    of object;

  TCustomSynEdit = class;

  TSynEditMark = class
  protected
    fLine, fColumn, fImage: Integer;
    fEdit: TCustomSynEdit;
    fVisible: boolean;
    fInternalImage: boolean;
    fBookmarkNum: integer;
    function GetEdit: TCustomSynEdit; virtual;
    procedure SetColumn(const Value: Integer); virtual;
    procedure SetImage(const Value: Integer); virtual;
    procedure SetLine(const Value: Integer); virtual;
    procedure SetVisible(const Value: boolean);
    procedure SetInternalImage(const Value: boolean);
    function GetIsBookmark: boolean;
  public
    constructor Create(AOwner: TCustomSynEdit);
    property Line: integer read fLine write SetLine;
    property Column: integer read fColumn write SetColumn;
    property Edit: TCustomSynEdit read fEdit;
    property ImageIndex: integer read fImage write SetImage;
    property BookmarkNumber: integer read fBookmarkNum write fBookmarkNum;
    property Visible: boolean read fVisible write SetVisible;
    property InternalImage: boolean read fInternalImage write SetInternalImage;
    property IsBookmark: boolean read GetIsBookmark;
  end;

  TPlaceMarkEvent = procedure(Sender: TObject; var Mark: TSynEditMark)
    of object;

  TSynEditMarks = array[1..maxMarks] of TSynEditMark;

  { A list of mark objects. Each object cause a litle picture to be drawn in the
    gutter. }
  TSynEditMarkList = class(TList)
  protected
    fEdit: TCustomSynEdit;
    fOnChange: TNotifyEvent;
    procedure DoChange;
    function Get(Index: Integer): TSynEditMark;
    procedure Put(Index: Integer; Item: TSynEditMark);
  public
    constructor Create(AOwner: TCustomSynEdit);
    destructor Destroy; override;
    function Add(Item: TSynEditMark): Integer;
    procedure ClearLine(line: integer);
    procedure Delete(Index: Integer);
    function First: TSynEditMark;
    procedure GetMarksForLine(line: integer; var Marks: TSynEditMarks);
    procedure Insert(Index: Integer; Item: TSynEditMark);
    function Last: TSynEditMark;
    procedure Place(mark: TSynEditMark);
    function Remove(Item: TSynEditMark): Integer;
  public
    property Items[Index: Integer]: TSynEditMark read Get write Put; default;
    property Edit: TCustomSynEdit read fEdit;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGutterClickEvent = procedure(Sender: TObject; X, Y, Line: integer;
    mark: TSynEditMark) of object;

  TSynEditPlugin = class(TObject)
  private
    fOwner: TCustomSynEdit;
  protected
    procedure AfterPaint(ACanvas: TCanvas; AClip: TRect;
      FirstLine, LastLine: integer); virtual; abstract;
    procedure LinesInserted(FirstLine, Count: integer); virtual; abstract;
    procedure LinesDeleted(FirstLine, Count: integer); virtual; abstract;
  protected
    property Editor: TCustomSynEdit read fOwner;                                //mh 2000-11-10
  public
    constructor Create(AOwner: TCustomSynEdit);
    destructor Destroy; override;
  end;

  TCustomSynEdit = class(TCustomControl)
  private
{$IFDEF SYN_CLX}
{$ELSE}
    procedure WMCaptureChanged(var Msg: TMessage); message WM_CAPTURECHANGED;   //jr 2001-01-06
    procedure WMCopy(var Message: TMessage); message WM_COPY;                   //pp 2001-08-28
    procedure WMCut(var Message: TMessage); message WM_CUT;                     //pp 2001-08-28
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMHScroll(var Msg: TWMScroll); message WM_HSCROLL;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;                 //pp 2001-08-28
    procedure WMCancelMode(var Message:TMessage); message WM_CANCELMODE;        //ddh 2001-12-13
{$IFDEF SYN_MBCSSUPPORT}
    procedure WMImeComposition(var Msg: TMessage); message WM_IME_COMPOSITION;
    procedure WMImeNotify(var Msg: TMessage); message WM_IME_NOTIFY;
{$ENDIF}
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
{$ENDIF}
  private
    fBlockBegin: TPoint;
    fBlockEnd: TPoint;
    fCaretX: Integer;
    fLastCaretX: integer;                                                       //mh 2000-10-19
    fCaretY: Integer;
    fCharsInWindow: Integer;
    fCharWidth: Integer;
    fFontDummy: TFont;
{$IFDEF SYN_MBCSSUPPORT}
    fImeCount: Integer;
    fMBCSStepAside: Boolean;
{$ENDIF}
    fInserting: Boolean;
    fLines: TStrings;
    fLinesInWindow: Integer;
    fLeftChar: Integer;
    fMaxLeftChar: Integer;
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
    fUndoList: TSynEditUndoList;
    fRedoList: TSynEditUndoList;
    fBookMarks: array[0..9] of TSynEditMark;
    fMouseDownX: integer;
    fMouseDownY: integer;
    fBookMarkOpt: TSynBookMarkOpt;
    fBorderStyle: TBorderStyle;
    fHideSelection: boolean;
    fMouseWheelAccumulator: integer;
    fOverwriteCaret: TSynEditCaretType;
    fInsertCaret: TSynEditCaretType;
    fCaretOffset: TPoint;
    fKeyStrokes: TSynEditKeyStrokes;
    fModified: Boolean;
    fMarkList: TSynEditMarkList;
    fExtraLineSpacing: integer;
    fSelectionMode: TSynSelectionMode;
    fWantTabs: boolean;
    fGutter: TSynGutter;
    fTabWidth: integer;
    fTextDrawer: TheTextDrawer;
    fInvalidateRect: TRect;
    fStateFlags: TSynStateFlags;
    fOptions: TSynEditorOptions;
    fStatusChanges: TSynStatusChanges;
    fLastKey: word;
    fLastShiftState: TShiftState;
    fTSearch: TSynEditSearch;
    fOnSearchEngineOverride: TSynEditSearchOverride;
    fHookedCommandHandlers: TList;
    fKbdHandler: TSynEditKbdHandler;
    fFocusList: TList;
    fPlugins: TList;
    fScrollTimer: TTimer;
    fScrollDeltaX, fScrollDeltaY: Integer;
    // event handlers
    fOnChange: TNotifyEvent;
    fOnClearMark: TPlaceMarkEvent;
    fOnCommandProcessed: TProcessCommandEvent;
    fOnDropFiles: TDropFilesEvent;
    fOnGutterClick: TGutterClickEvent;
    fOnPaint: TPaintEvent;
    fOnPlaceMark: TPlaceMarkEvent;
    fOnProcessCommand: TProcessCommandEvent;
    FOnCancelMode: TNotifyEvent;
    fOnProcessUserCommand: TProcessCommandEvent;
    fOnReplaceText: TReplaceTextEvent;
    fOnSpecialLineColors: TSpecialLineColorsEvent;
    fOnContextHelp: TContextHelpEvent;
    fOnPaintTransient: TPaintTransient;                                         //GBN 2001-10-23

    fOnStatusChange: TStatusChangeEvent;
    fWordWrap: Boolean;                                                         //Fiala 2001-12-17
    fShowSpecChar: Boolean;                                                     //Fiala 2001-12-17
    fIsAltSetColumnMode: Boolean;                                               //Fiala 2001-12-17
    fIsPasting : Boolean;                                                       //jcr 2002-04-13
    FPaintTransientLock: Integer;                                               //GBN 2002-03-23
{$IFDEF SYN_CLX}
    FHScrollBar : TSynEditScrollBar;
    FVScrollBar : TSynEditScrollBar;

    procedure ModifyScrollBar(ScrollBar: TScrollBarKind; ScrollCode: TScrollCode; Pos: Cardinal; UseRightToLeft: Boolean);
    procedure ScrollEvent(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
{$ENDIF}

    procedure BookMarkOptionsChanged(Sender: TObject);
    procedure ComputeCaret(X, Y: Integer);
    procedure DoBlockIndent;
    procedure DoBlockUnindent;
    procedure DoLinesDeleted(FirstLine, Count: integer);
    procedure DoLinesInserted(FirstLine, Count: integer);
    procedure DoTabKey;
    procedure DoCaseChange(const Cmd : TSynEditorCommand);                      //DDH 2002-02-11
    function FindHookedCmdEvent(AHandlerProc: THookedCommandEvent): integer;
    procedure FontChanged(Sender: TObject);
    function GetBlockBegin: TPoint;
    function GetBlockEnd: TPoint;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCaretXY: TPoint;
    function GetDisplayX: Integer;                                              //GN 10/16/01
    function GetDisplayY: Integer;                                              //GN 10/16/01
    function GetDisplayXY: TPoint;                                              //GN 10/16/01
    function GetFont: TFont;
    function GetHookedCommandHandlersCount: integer;
    function GetLineText: string;
    function GetMaxUndo: Integer;
    function GetSelAvail: Boolean;
    function GetSelTabBlock: Boolean;
    function GetSelTabLine: Boolean;
    function GetSelText: string;
    function GetText: string;
    function GetWordAtCursor : string;
    procedure GutterChanged(Sender: TObject);
    procedure InsertBlock(BB, BE: TPoint; ChangeStr: PChar);
    procedure InsertBlockEx(BB, BE: TPoint; ChangeStr: PChar; AddToUndoList: Boolean);
    function IsPointInSelection(Value: TPoint): boolean;
    function LeftSpaces(const Line: string): Integer;
    function LeftSpacesEx(const Line: string; WantTabs: Boolean): Integer;
    function GetLeftSpacing(CharCount: Integer; WantTabs: Boolean): String;
    procedure LinesChanging(Sender: TObject);
    procedure LinesChanged(Sender: TObject);
    procedure LockUndo;
    procedure MoveCaretAndSelection(ptBefore, ptAfter: TPoint;
      SelectionCommand: boolean);
    procedure MoveCaretHorz(DX: integer; SelectionCommand: boolean);
    procedure MoveCaretVert(DY: integer; SelectionCommand: boolean);
    procedure PluginsAfterPaint(ACanvas: TCanvas; AClip: TRect;
      FirstLine, LastLine: integer);
    function ScanFrom(Index: integer): integer;
    procedure ScrollTimerHandler(Sender: TObject);
    procedure SelectedColorsChanged(Sender: TObject);
    procedure SetWordWrap(Value: boolean);                                      //Fiala 2001-12-17
    procedure SetBlockBegin(Value: TPoint);
    procedure SetBlockEnd(Value: TPoint);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetCaretAndSelection(ptCaret, ptBefore, ptAfter: TPoint);
    procedure SetCaretX(Value: Integer);
    procedure SetCaretY(Value: Integer);
    procedure SetExtraLineSpacing(const Value: integer);
    procedure SetFont(const Value: TFont);
    procedure SetGutter(const Value: TSynGutter);
    procedure SetGutterWidth(Value: Integer);
    procedure SetHideSelection(const Value: boolean);
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure SetInsertCaret(const Value: TSynEditCaretType);
    procedure SetInsertMode(const Value: boolean);
    procedure SetKeystrokes(const Value: TSynEditKeyStrokes);
    procedure SetOnKeyDown(const Value : TKeyEvent);
    function GetOnKeyDown : TKeyEvent;
    procedure SetOnKeyPress(const Value : TKeyPressEvent);
    function GetOnKeyPress : TKeyPressEvent;
    procedure SetLeftChar(Value: Integer);
    procedure SetLines(Value: TStrings);
    procedure SetLineText(Value: string);
    procedure SetMaxLeftChar(Value: integer);
    procedure SetMaxUndo(const Value: Integer);
    procedure SetModified(Value: boolean);
    procedure SetOptions(Value: TSynEditorOptions);
    procedure SetOverwriteCaret(const Value: TSynEditCaretType);
    procedure SetRightEdge(Value: Integer);
    procedure SetRightEdgeColor(Value: TColor);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetSelectionMode(const Value: TSynSelectionMode);
    procedure SetSelText(const Value: string);
    procedure SetSelTextExternal(const Value: string);
    procedure SetTabWidth(Value: integer);
    procedure SetText(const Value: string);
    procedure SetTopLine(Value: Integer);
    procedure SetWantTabs(const Value: boolean);
    procedure SetWordBlock(Value: TPoint);
    procedure SizeOrFontChanged(bFont: boolean);
    procedure StatusChanged(AChanges: TSynStatusChanges);
    procedure TrimmedSetLine(ALine: integer; ALineText: string);
    procedure UndoRedoAdded(Sender: TObject);
    procedure UnlockUndo;
//    procedure UpdateCaret;                                                    //DDH 10/16/01 moved to public
    procedure UpdateLastCaretX;                                                 //jr 2002-04-26
    procedure UpdateScrollBars;
    procedure DoShiftTabKey;                                                    //shift-tab key handling
    procedure DoHomeKey(Selection:boolean);                                     //home key enhancement
  protected
{$IFDEF SYN_CLX}
    procedure Resize; override;
    function GetClientRect: TRect; override;
{$ELSE}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
{$ENDIF}
    procedure DblClick; override;
    procedure DecPaintLock;
    procedure DragOver(Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
//    procedure FindMatchingBracket; virtual;                                   //GBN 2001-10-23 Moved to public
    function GetReadOnly: boolean; virtual;
    procedure HighlighterAttrChanged(Sender: TObject);
    procedure IncPaintLock;
    procedure InitializeCaret;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure ListAdded(Index: integer; const S: String);                       // mh 2000-10-10
    procedure ListCleared(Sender: TObject);
    procedure ListDeleted(Index: integer);
    procedure ListInserted(Index: integer; const S: String);
    procedure ListPutted(Index: integer; const S: String);
    procedure ListScanRanges(Sender: TObject);
    procedure Loaded; override;
    procedure MarkListChange(Sender: TObject);
{$IFDEF SYN_MBCSSUPPORT}
    procedure MBCSGetSelRangeInLineWhenColumnSelectionMode(const s: string;
      var ColFrom, ColTo: Integer);
{$ENDIF}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure NotifyHookedCommandHandlers(AfterProcessing: boolean;
      var Command: TSynEditorCommand; var AChar: char; Data: pointer); virtual;
    procedure Paint; override;
    procedure PaintGutter(AClip: TRect; FirstLine, LastLine: integer); virtual;
    procedure PaintTextLines(AClip: TRect; FirstLine, LastLine,
      FirstCol, LastCol: integer); virtual;
    procedure RecalcCharExtent;
    procedure RedoItem;                                                         //sbs 2000-11-19
    procedure SetCaretXY(Value: TPoint); virtual;
    procedure SetCaretXYEx(CallEnsureCursorPos: Boolean; Value: TPoint); virtual;
    procedure SetName(const Value: TComponentName); override;
    procedure SetReadOnly(Value: boolean); virtual;
    procedure SetSelTextPrimitive(PasteMode: TSynSelectionMode; Value: PChar;
      Tag: PInteger);
    procedure SetSelTextPrimitiveEx(PasteMode: TSynSelectionMode; Value: PChar;
      Tag: PInteger; AddToUndoList: Boolean);
    // If the translations requires Data, memory will be allocated for it via a
    // GetMem call.  The client must call FreeMem on Data if it is not NIL.
    function TranslateKeyCode(Code: word; Shift: TShiftState;
      var Data: pointer): TSynEditorCommand;
    procedure UndoItem;                                                         //sbs 2000-11-19
  protected
    fGutterWidth: Integer;
    fInternalImage: TSynInternalImage;
    FAlwaysShowCaret: Boolean;                                                  //DDH 10/16/01
    property WordWrap: boolean read FWordWrap write SetWordWrap;                //Fiala 2001-12-17
    procedure HideCaret;
    procedure ShowCaret;
    procedure DoOnClearBookmark(var Mark: TSynEditMark); virtual;
    procedure DoOnCommandProcessed(Command: TSynEditorCommand; AChar: char;
      Data: pointer); virtual;
    // no method DoOnDropFiles, intercept the WM_DROPFILES instead
    procedure DoOnGutterClick(X, Y: integer); virtual;
    procedure DoOnPaint; virtual;
    procedure DoOnPaintTransientEx(TransientType: TTransientType; Lock: Boolean); virtual;
    procedure DoOnPaintTransient(TransientType: TTransientType); virtual;       //GBN 2001-10-23

    procedure DoOnPlaceMark(var Mark: TSynEditMark); virtual;
    procedure DoOnProcessCommand(var Command: TSynEditorCommand;
      var AChar: char; Data: pointer); virtual;
    function DoOnReplaceText(const ASearch, AReplace: string;
      Line, Column: integer): TSynReplaceAction; virtual;
    function DoOnSpecialLineColors(Line: integer;
      var Foreground, Background: TColor): boolean; virtual;
    procedure DoOnStatusChange(Changes: TSynStatusChanges); virtual;
    function GetSelEnd: integer;                                                //DDH 10/16/01
    function GetSelStart: integer;                                              //DDH 10/16/01
    procedure SetSelEnd(const Value: integer);                                  //DDH 10/16/01
    procedure SetSelStart(const Value: integer);                                //DDH 10/16/01
    procedure SetAlwaysShowCaret(const Value: Boolean);                         //DDH 10/16/01
    procedure PrepareIdentChars(var IdentChars, WhiteChars: TSynIdentChars);    //DDH 11/01/01
  public
    property Canvas;                                                            //DDH 10/16/01
    property SelStart: Integer read GetSelStart write SetSelStart;              //DDH 10/16/01
    property SelEnd: Integer read GetSelEnd write SetSelEnd;                    //DDH 10/16/01
    property AlwaysShowCaret: Boolean read FAlwaysShowCaret                     //DDH 10/16/01
                                      write SetAlwaysShowCaret;
    procedure UpdateCaret;                                                      //DDH 10/16/01 moved from protected to public (needed in Completion Proposal)
{$IFDEF SYN_COMPILER_4_UP}                                                      //DDH 10/17/01 change from Flávio Etrusco
    procedure AddKey(Command: TSynEditorCommand; Key1: word; SS1: TShiftState;
      Key2: word = 0; SS2: TShiftState = []);
{$ELSE}
    procedure AddKey(Command: TSynEditorCommand; Key1: word; SS1: TShiftState;
      Key2: word; SS2: TShiftState);
{$ENDIF}
    procedure BeginUndoBlock;                                                   //sbs 2000-11-19
    procedure BeginUpdate;
    function CaretXPix: Integer;
    function CaretYPix: Integer;
    procedure ClearAll;
    procedure ClearBookMark(BookMark: Integer);
    procedure ClearSelection;
    procedure CommandProcessor(Command: TSynEditorCommand; AChar: char;
      Data: pointer); virtual;
    procedure ClearUndo;
    procedure CopyToClipboard;
    constructor Create(AOwner: TComponent); override;
    procedure CutToClipboard;
    destructor Destroy; override;
    procedure DoCopyToClipboard(const SText: string);
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure EndUndoBlock;                                                     //sbs 2000-11-19
    procedure EndUpdate;
    procedure EnsureCursorPosVisible;
    procedure EnsureCursorPosVisibleEx(ForceToMiddle: Boolean);
    procedure FindMatchingBracket; virtual;                                     //GBN 2001-10-23 Moved to public
    function GetMatchingBracket: TPoint; virtual;                               //GBN 2001-10-23
    function GetMatchingBracketEx(APoint: TPoint;
                                  AdjustForTabs: Boolean): TPoint; virtual;     //DDH 2001-10-23
{$IFDEF SYN_COMPILER_4_UP}
    function ExecuteAction(Action: TBasicAction): boolean; override;
{$ENDIF}
    procedure ExecuteCommand(Command: TSynEditorCommand; AChar: char;
      Data: pointer); virtual;
    function GetBookMark(BookMark: integer; var X, Y: integer): boolean;
    function GetHighlighterAttriAtRowCol(XY: TPoint; var Token: string;
      var Attri: TSynHighlighterAttributes): boolean;
    function GetHighlighterAttriAtRowColEx(XY: TPoint; var Token: string;
      var TokenType, Start: Integer;
      var Attri: TSynHighlighterAttributes): boolean;                           //DDH 10/16/01
    function GetWordAtRowCol(XY: TPoint): string;
    procedure GotoBookMark(BookMark: Integer);
    function IdentChars: TSynIdentChars;                                        //DDH 10/17/01 from Flávio Etrusco
    procedure InvalidateGutter;
    // note: FirstLine and LastLine don't need to be in correct order
    procedure InvalidateGutterLines(FirstLine, LastLine: integer);
    procedure InvalidateLine(Line: integer);
    procedure InvalidateLines(FirstLine, LastLine: integer);                    //Fiala 2001-12-17 moved to public
    function IsBookmark(BookMark: integer): boolean;
    function LogicalToPhysicalPos(p: TPoint): TPoint;
    function PhysicalToLogicalPos(p: TPoint): TPoint;                           //sblbg 2001-12-17
    function NextWordPos: TPoint; virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure PasteFromClipboard;
    function WordStart: TPoint; virtual;
    function WordEnd: TPoint; virtual;
    function PrevWordPos: TPoint; virtual;
    function PixelsToRowColumn(Pixels: TPoint): TPoint;
    procedure Redo;
    procedure RegisterCommandHandler(AHandlerProc: THookedCommandEvent;
      AHandlerData: pointer);
    function RowColumnToPixels(RowCol: TPoint): TPoint;
    function SearchReplace(const ASearch, AReplace: string;
      AOptions: TSynSearchOptions): integer;
    procedure SelectAll;
    procedure SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
    procedure SetDefaultKeystrokes; virtual;
    procedure SetOptionFlag(Flag: TSynEditorOption; Value: boolean);
    procedure SetSelWord;
    procedure Undo;
    procedure UnregisterCommandHandler(AHandlerProc: THookedCommandEvent);
{$IFDEF SYN_COMPILER_4_UP}
    function UpdateAction(Action: TBasicAction): boolean; override;
{$ENDIF}

    procedure SetFocus; override;

    procedure AddKeyDownHandler (aHandler : TKeyDownProc);
    procedure RemoveKeyDownHandler (aHandler : TKeyDownProc);
    procedure AddKeyPressHandler (aHandler : TKeyPressProc);
    procedure RemoveKeyPressHandler (aHandler : TKeyPressProc);
    procedure AddFocusControl (aControl: TWinControl);
    procedure RemoveFocusControl (aControl: TWinControl);

{$IFDEF SYN_CLX}
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; override;
{$ELSE}
    procedure WndProc(var Msg: TMessage); override;
{$ENDIF}
  public
    property BlockBegin: TPoint read GetBlockBegin write SetBlockBegin;
    property BlockEnd: TPoint read GetBlockEnd write SetBlockEnd;
    property CanPaste: Boolean read GetCanPaste;
    property CanRedo: boolean read GetCanRedo;
    property CanUndo: boolean read GetCanUndo;
    property CaretX: Integer read fCaretX write SetCaretX;
    property CaretY: Integer read fCaretY write SetCaretY;
    property CaretXY: TPoint read GetCaretXY write SetCaretXY;
    property DisplayX: Integer read GetDisplayX;                                //GN 10/16/01
    property DisplayY: Integer read GetDisplayY;                                //GN 10/16/01
    property DisplayXY: TPoint read GetDisplayXY;                               //GN 10/16/01
    property CharsInWindow: Integer read fCharsInWindow;
    property CharWidth: integer read fCharWidth;
    property Color;
    property Font: TFont read GetFont write SetFont;
    property Highlighter: TSynCustomHighlighter
      read fHighlighter write SetHighlighter;
    property LeftChar: Integer read fLeftChar write SetLeftChar;
    property LineHeight: integer read fTextHeight;
    property LinesInWindow: Integer read fLinesInWindow;
    property LineText: string read GetLineText write SetLineText;
    property Lines: TStrings read fLines write SetLines;
    property Marks: TSynEditMarkList read fMarkList;
    property MaxLeftChar: integer read fMaxLeftChar write SetMaxLeftChar
      default 1024;
    property Modified: Boolean read fModified write SetModified;
    property PaintLock: Integer read fPaintLock;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default FALSE;
    property SelAvail: Boolean read GetSelAvail;
    property SelTabBlock: Boolean read GetSelTabBlock;
    property SelTabLine: Boolean read GetSelTabLine;
    property SelText: string read GetSelText write SetSelTextExternal;
    property Text: string read GetText write SetText;
    property TopLine: Integer read fTopLine write SetTopLine;
    property WordAtCursor: string read GetWordAtCursor;
    property UndoList: TSynEditUndoList read fUndoList;
    property RedoList: TSynEditUndoList read fRedoList;
  public
    property OnKeyDown: TKeyEvent read GetOnKeyDown write SetOnKeyDown;
    property OnKeyPress: TKeyPressEvent read GetOnKeyPress write SetOnKeyPress;
    property OnCancelMode: TNotifyEvent read FOnCancelMode write FOnCancelMode;
    property OnProcessCommand: TProcessCommandEvent
      read FOnProcessCommand write FOnProcessCommand;
    property Options: TSynEditorOptions read fOptions write SetOptions        //DDH 10/16/01 Moved to public from protected for SynAutoComplete
      default SYNEDIT_DEFAULT_OPTIONS;

    property BookMarkOptions: TSynBookMarkOpt
      read fBookMarkOpt write fBookMarkOpt;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle
      default bsSingle;
    property ExtraLineSpacing: integer
      read fExtraLineSpacing write SetExtraLineSpacing default 0;
    property Gutter: TSynGutter read fGutter write SetGutter;
    property HideSelection: boolean read fHideSelection write SetHideSelection
      default false;
    property InsertCaret: TSynEditCaretType read FInsertCaret
      write SetInsertCaret default ctVerticalLine;
    property InsertMode: boolean read fInserting write SetInsertMode
      default true;
    property Keystrokes: TSynEditKeyStrokes
      read FKeystrokes write SetKeystrokes;
    property MaxUndo: Integer read GetMaxUndo write SetMaxUndo default 1024;
//    property Options: TSynEditorOptions read fOptions write SetOptions        //DDH 10/16/01 Moved to public from protected for SynAutoComplete
//      default SYNEDIT_DEFAULT_OPTIONS;
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
    property TabWidth: integer read fTabWidth write SetTabWidth default 8;
    property WantTabs: boolean read fWantTabs write SetWantTabs default FALSE;
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
      read fOnPaintTransient write fOnPaintTransient;                           //GBN 2001-10-23
    property OnSearchEngineOverride: TSynEditSearchOverride
      read fOnSearchEngineOverride write fOnSearchEngineOverride;               //DDH 2002-04-12
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
{$IFDEF SYN_CLX}
{$ELSE}
    property Ctl3D;
    property ParentCtl3D;
{$ENDIF}
    property Enabled;
    property Font;
    property Height;
    property Name;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property Visible;
    property Width;
//    property WordWrap;                                                          //Fiala 2001-12-17  //When it works, uncomment this.  It currently *does not work*
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
    property OnStartDrag;
    // TCustomSynEdit properties
    property BookMarkOptions;
    property BorderStyle;
    property ExtraLineSpacing;
    property Gutter;
    property HideSelection;
    property Highlighter;
    property InsertCaret;
    property InsertMode;
    property Keystrokes;
    property Lines;
    property MaxLeftChar;
    property MaxUndo;
    property Options;
    property OverwriteCaret;
    property ReadOnly;
    property RightEdge;
    property RightEdgeColor;
    property ScrollHintColor;
    property ScrollHintFormat;
    property ScrollBars;
    property SelectedColor;
    property SelectionMode;
    property TabWidth;
    property WantTabs;
    // TCustomSynEdit events
    property OnChange;
    property OnClearBookmark;
    property OnCommandProcessed;
    property OnContextHelp;
    property OnDropFiles;
    property OnGutterClick;
    property OnPaint;
    property OnPlaceBookmark;
    property OnProcessCommand;
    property OnProcessUserCommand;
    property OnReplaceText;
    property OnSpecialLineColors;
    property OnStatusChange;
    property OnPaintTransient;
    property OnSearchEngineOverride;
  end;

implementation

{$R SynEdit.res}

uses
{$IFDEF SYN_CLX}
  QStdActns,
  QClipbrd,
{$ELSE}
{$IFDEF SYN_COMPILER_4_UP}
  StdActns,
{$ENDIF}
  Clipbrd,
  ShellAPI,
{$ENDIF}
  SynEditStrConst;

function TrimTrailingSpaces(const S: string): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] = #32) do Dec(I);
  Result := Copy(S, 1, I);
end;

function Roundoff(X: Extended): Longint;
begin
  if (x >= 0) then begin
    Result := Trunc(x + 0.5)
  end else begin
    Result := Trunc(x - 0.5);
  end;
end;

{ THookedCommandHandlerEntry }

type
  THookedCommandHandlerEntry = class(TObject)
  private
    fEvent: THookedCommandEvent;
    fData: pointer;
    constructor Create(AEvent: THookedCommandEvent; AData: pointer);
    function Equals(AEvent: THookedCommandEvent): boolean;
  end;

constructor THookedCommandHandlerEntry.Create(AEvent: THookedCommandEvent;
  AData: pointer);
begin
  inherited Create;
  fEvent := AEvent;
  fData := AData;
end;

function THookedCommandHandlerEntry.Equals(AEvent: THookedCommandEvent): boolean;
begin
  with TMethod(fEvent) do
    Result := (Code = TMethod(AEvent).Code) and (Data = TMethod(AEvent).Data);
end;

{ TCustomSynEdit }

function TCustomSynEdit.PixelsToRowColumn(Pixels: TPoint): TPoint;
var
  s: string;
  f: Single;
begin
  f := (Pixels.X + (fLeftChar) * fCharWidth - fGutterWidth - 2)
    / fCharWidth;
  // don't return a partially visible last line
  if Pixels.Y >= fLinesInWindow * fTextHeight then begin
    Pixels.Y := fLinesInWindow * fTextHeight - 1;
    if Pixels.Y < 0 then Pixels.Y := 0;
  end;
  Result := Point(Roundoff(f), Pixels.Y div fTextHeight + TopLine);

  // now fix up for any TAB characters in the line
  Result := PhysicalToLogicalPos(Result);                                       // sblbg 2001-12-17

{$IFDEF SYN_MBCSSUPPORT}
  if (Result.Y >= 1) and (Result.Y <= Lines.Count) then begin
    s := Lines[Result.Y - 1];
    if (Length(s) >= Result.x) and (ByteType(s, Result.X) = mbTrailByte) then
      if Frac(f) >= 0.5 then
        Dec(Result.X)
      else
        Inc(Result.X);
  end;
{$ENDIF}
end;

function TCustomSynEdit.RowColumnToPixels(rowcol: TPoint): TPoint;
Var P :TPoint;
begin
  P := LogicalToPhysicalPos(RowCol);                                            // sblbg 2001-12-17
  Result.X := (P.X-1) * fCharWidth + fTextOffset;
  Result.Y := (RowCol.Y - fTopLine) * fTextHeight;
end;

procedure TCustomSynEdit.ComputeCaret(X, Y: Integer);
begin
  CaretXY := PixelsToRowColumn(Point(X, Y));
end;

procedure TCustomSynEdit.DoCopyToClipboard(const SText: string);
{$IFDEF SYN_CLX}
begin
  Clipboard.AsText := SText;
end;
{$ELSE}
var
  Mem: HGLOBAL;
  P: PChar;
  SLen: integer;
  Failed: boolean;
begin
  if SText <> '' then begin
    Failed := TRUE; // assume the worst.
    SLen := Length(SText);
    // Open and Close are the only TClipboard methods we use because TClipboard
    // is very hard (impossible) to work with if you want to put more than one
    // format on it at a time.
    Clipboard.Open;
    try
      // Clear anything already on the clipboard.
      EmptyClipboard;
      // Put it on the clipboard as normal text format so it can be pasted into
      // things like notepad or Delphi.
      Mem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, SLen + 1);
      if Mem <> 0 then begin
        P := GlobalLock(Mem);
        try
          if P <> nil then begin
            Move(PChar(SText)^, P^, SLen + 1);
            // Put it on the clipboard in text format
            Failed := SetClipboardData(CF_TEXT, Mem) = 0;
          end;
        finally
          GlobalUnlock(Mem);
        end;
      end;
      // Don't free Mem!  It belongs to the clipboard now, and it will free it
      // when it is done with it.
      if not Failed then begin
        // Copy it in our custom format so we know what kind of block it is.
        // That effects how it is pasted in.
        Mem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, SLen +
          SizeOf(TSynSelectionMode) + 1);
        P := GlobalLock(Mem);
        try
          if P <> nil then begin
            // Our format:  TSynSelectionMode value followed by text.
            PSynSelectionMode(P)^ := SelectionMode;
            inc(P, SizeOf(TSynSelectionMode));
            Move(PChar(SText)^, P^, SLen + 1);
            Failed := SetClipboardData(SynEditClipboardFormat, Mem) = 0;
          end;
        finally
          GlobalUnlock(Mem);
        end;
        // Don't free Mem!  It belongs to the clipboard now, and it will free it
        // when it is done with it.
      end;
    finally
      Clipboard.Close;
      if Failed then
        raise ESynEditError.Create('Clipboard copy operation failed');
    end;
  end;
end;
{$ENDIF}

procedure TCustomSynEdit.CopyToClipboard;
var
  SText: string;
begin
  if SelAvail then begin
    SText := SelText;
    DoCopyToClipboard(SText);
  end;
end;

procedure TCustomSynEdit.CutToClipboard;
var
  SText: string;
begin
  if not ReadOnly and SelAvail then begin                                       //jcr 2001-01-16
    SText := SelText;
    DoCopyToClipboard(SText);
    fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, SText, SelectionMode);
    LockUndo;
    SelText := '';
    UnlockUndo;
  end;
end;

constructor TCustomSynEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{begin}                                                                         //mh 2000-10-10
//  fLines := TSynEditList.Create;
  fLines := TSynEditStringList.Create;
//  with TSynEditList(fLines) do begin
  with TSynEditStringList(fLines) do begin
    OnAdded := ListAdded;
    OnChange := LinesChanged;
    OnChanging := LinesChanging;
    OnCleared := ListCleared;
    OnDeleted := ListDeleted;
    OnInserted := ListInserted;
    OnPutted := ListPutted;
//    OnScanRanges := ListScanRanges;
  end;
{end}                                                                           //mh 2000-10-10
  fFontDummy := TFont.Create;
  fUndoList := TSynEditUndoList.Create;
  fUndoList.OnAddedUndo := UndoRedoAdded;
  fRedoList := TSynEditUndoList.Create;
  fRedoList.OnAddedUndo := UndoRedoAdded;
{$IFDEF SYN_COMPILER_4_UP}
{$IFDEF SYN_CLX}
{$ELSE}
  DoubleBuffered := false;
{$ENDIF}
{$ENDIF}
  fSelectedColor := TSynSelectedColor.Create;
  fSelectedColor.OnChange := SelectedColorsChanged;
  fBookMarkOpt := TSynBookMarkOpt.Create(Self);
  fBookMarkOpt.OnChange := BookMarkOptionsChanged;
// fRightEdge has to be set before FontChanged is called for the first time
  fRightEdge := 80;
  fGutter := TSynGutter.Create;
  fGutter.OnChange := GutterChanged;
  fGutterWidth := fGutter.Width;
  fTextOffset := fGutterWidth + 2;
  ControlStyle := ControlStyle + [csOpaque, csSetCaption];
  Height := 150;
  Width := 200;
  Cursor := crIBeam;
{$IFDEF SYN_WIN32}
  Color := clWindow;
{$ENDIF}
{$IFDEF SYN_CLX}
  Color := clNormalLight;
{$ENDIF}
{$IFDEF SYN_WIN32}
  fFontDummy.Name := 'Courier New';
  fFontDummy.Size := 10;
{$ENDIF}
{$IFDEF SYN_KYLIX}
  fFontDummy.Name := 'terminal';
  fFontDummy.Size := 14;
{$ENDIF}
{$IFDEF SYN_COMPILER_3_UP}
  fFontDummy.CharSet := DEFAULT_CHARSET;
{$ENDIF}
  fWordWrap := false;                                                           //Fiala 2001-12-17
  fShowSpecChar := False;                                                       //Fiala 2001-12-17
  fTextDrawer := TheTextDrawer.Create([fsBold], fFontDummy);
  Font.Assign(fFontDummy);
  Font.OnChange := FontChanged;
  FontChanged(nil);
  ParentFont := False;
  ParentColor := False;
  TabStop := True;
  fInserting := True;
  fMaxLeftChar := 1024;
  fScrollBars := ssBoth;
  fBorderStyle := bsSingle;
  fInsertCaret := ctVerticalLine;
  fOverwriteCaret := ctBlock;
  FSelectionMode := smNormal;
  fFocusList := TList.Create;
  fKbdHandler := TSynEditKbdHandler.Create(Self);
  fKeystrokes := TSynEditKeyStrokes.Create(Self);
  fMarkList := TSynEditMarkList.Create(self);
  fMarkList.OnChange := MarkListChange;
  SetDefaultKeystrokes;
  fRightEdgeColor := clSilver;
{$IFDEF SYN_MBCSSUPPORT}
  fImeCount := 0;
  fMBCSStepAside := False;
{$ENDIF}
  fWantTabs := False;
  fTabWidth := 8;
  fLeftChar := 1;
  fTopLine := 1;
  fCaretX := 1;
  fLastCaretX := 1;                                                             //mh 2000-10-19
  fCaretY := 1;
  fBlockBegin := Point(1, 1);
  fBlockEnd := fBlockBegin;
  // find / replace
//  fTSearch := TSynEditSearch.Create;                                          //ddh 2002-4-112 moved to SearchReplace
  fOptions := SYNEDIT_DEFAULT_OPTIONS;
  fScrollTimer := TTimer.Create(Self);
  fScrollTimer.Enabled := False;
  fScrollTimer.Interval := 100;
  fScrollTimer.OnTimer := ScrollTimerHandler;

{$IFDEF SYN_CLX}
  InputKeys := [ikAll];

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
  fIsAltSetColumnMode := False;                                                 //Fiala 2001-12-17
  fIsPasting:= False;
end;

{$IFDEF SYN_CLX}
{$ELSE}
procedure TCustomSynEdit.CreateParams(var Params: TCreateParams);
const
  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL,
    WS_HSCROLL or WS_VSCROLL);
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
  inherited CreateParams(Params);
  with Params do begin
    WindowClass.Style := WindowClass.Style and not ClassStylesOff;
    if not(eoHideShowScrollbars in Options) then
    begin
      Style := Style or ScrollBar[FScrollBars] or BorderStyles[fBorderStyle]
        or WS_CLIPCHILDREN
    end else Style := Style or BorderStyles[fBorderStyle] or WS_CLIPCHILDREN;

    if NewStyleControls and Ctl3D and (fBorderStyle = bsSingle) then begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;
{$ENDIF}

procedure TCustomSynEdit.DecPaintLock;
begin
  Dec(fPaintLock);
  if (fPaintLock = 0) and HandleAllocated then begin
    if sfScrollbarChanged in fStateFlags then
      UpdateScrollbars;
    if sfCaretChanged in fStateFlags then
      UpdateCaret;
    if fStatusChanges <> [] then
      DoOnStatusChange(fStatusChanges);
  end;
end;

destructor TCustomSynEdit.Destroy;
var
  i: integer;
begin
  Highlighter := nil;
  // free listeners while other fields are still valid
  if Assigned(fHookedCommandHandlers) then begin
    for i := 0 to fHookedCommandHandlers.Count - 1 do
      THookedCommandHandlerEntry(fHookedCommandHandlers[i]).Free;
    FreeAndNil(fHookedCommandHandlers);
  end;
  if fPlugins <> nil then begin
    for i := fPlugins.Count - 1 downto 0 do
      TSynEditPlugin(fPlugins[i]).Free;
    fPlugins.Free;
  end;
  fScrollTimer.Free;
  if assigned(fTSearch) then
    fTSearch.Free;
  fMarkList.Free;
  fBookMarkOpt.Free;
  fBookMarkOpt := nil;
  fKeyStrokes.Free;
  fKbdHandler.Free;
  fFocusList.Free;
  fSelectedColor.Free;
  fUndoList.Free;
  fRedoList.Free;
  fGutter.Free;
  fTextDrawer.Free;
  fInternalImage.Free;
  fFontDummy.Free;
  Lines.Free;
  inherited Destroy;
end;

function TCustomSynEdit.GetBlockBegin: TPoint;
begin
  if (fBlockEnd.Y < fBlockBegin.Y)
    or ((fBlockEnd.Y = fBlockBegin.Y) and (fBlockEnd.X < fBlockBegin.X))
  then
    Result := fBlockEnd
  else
    Result := fBlockBegin;
end;

function TCustomSynEdit.GetBlockEnd: TPoint;
begin
  if (fBlockEnd.Y < fBlockBegin.Y)
    or ((fBlockEnd.Y = fBlockBegin.Y) and (fBlockEnd.X < fBlockBegin.X))
  then
    Result := fBlockBegin
  else
    Result := fBlockEnd;
end;

function TCustomSynEdit.CaretXPix: Integer;
var
  p: TPoint;
begin
  p := Point(fCaretX, fCaretY);
  Result := RowColumnToPixels(p).X;
end;

function TCustomSynEdit.CaretYPix: Integer;
begin
  Result := RowColumnToPixels(Point(1, fCaretY)).Y;
end;

procedure TCustomSynEdit.FontChanged(Sender: TObject);
begin
  RecalcCharExtent;
  SizeOrFontChanged(TRUE);
end;

function TCustomSynEdit.GetFont: TFont;
begin
  Result := inherited Font;
end;

function TCustomSynEdit.GetLineText: string;
begin
  if (CaretY >= 1) and (CaretY <= Lines.Count) then
    Result := Lines[CaretY - 1]
  else
    Result := '';
end;

function TCustomSynEdit.GetSelAvail: Boolean;
begin
  Result := (fBlockBegin.X <> fBlockEnd.X) or
    ((fBlockBegin.Y <> fBlockEnd.Y) and (fSelectionMode <> smColumn));
end;

function TCustomSynEdit.GetSelTabBlock: Boolean;
begin
  Result := (fBlockBegin.Y <> fBlockEnd.Y) and (fSelectionMode <> smColumn);
end;

function TCustomSynEdit.GetSelTabLine: Boolean;
begin
  Result := (BlockBegin.X <= 1) and (BlockEnd.X > length(Lines[CaretY - 1])) and SelAvail;
end;

function TCustomSynEdit.GetSelText: string;

  function CopyPadded(const S: string; Index, Count: integer): string;
  var
    SrcLen: Integer;
    DstLen: integer;
    P: PChar;
  begin
    SrcLen := Length(S);
    DstLen := Index + Count;
    if SrcLen >= DstLen then
      Result := Copy(S, Index, Count)
    else begin
      SetLength(Result, DstLen);
      P := PChar(Result);
      StrPCopy(P, Copy(S, Index, Count));
      Inc(P, Length(S));
      FillChar(P^, DstLen - Srclen, $20);
    end;
  end;

  procedure CopyAndForward(const S: string; Index, Count: Integer; var P:
    PChar);
  var
    pSrc: PChar;
    SrcLen: Integer;
    DstLen: Integer;
  begin
    SrcLen := Length(S);
    if (Index <= SrcLen) and (Count > 0) then begin
      Dec(Index);
      pSrc := PChar(S) + Index;
      DstLen := Min(SrcLen - Index, Count);
      Move(pSrc^, P^, DstLen);
      Inc(P, DstLen);
      P^ := #0;
    end;
  end;

  function CopyPaddedAndForward(const S: string; Index, Count: Integer;
    var P: PChar): Integer;
  var
    OldP: PChar;
    Len: Integer;
  begin
    Result := 0;
    OldP := P;
    CopyAndForward(S, Index, Count, P);
    Len := Count - (P - OldP);
    if not (eoTrimTrailingSpaces in Options)
    then begin
      FillChar(P^, Len, #$20);
      Inc(P, Len);
    end else
      Result:= Len;
  end;

const
  sLineBreak = #$0D#$0A;
var
  First, Last, TotalLen: Integer;
  ColFrom, ColTo: Integer;
  I: Integer;
{$IFDEF SYN_MBCSSUPPORT}
  l, r: Integer;
  s: string;
{$ELSE}
  ColLen: integer;
{$ENDIF}
  P: PChar;
begin
  if not SelAvail then
    Result := ''
  else begin
    with BlockBegin do begin
      ColFrom := X;
      First := Y - 1;
    end;
    with BlockEnd do begin
      ColTo := X;
      Last := Y - 1;
    end;
    TotalLen := 0;
    case SelectionMode of
      smNormal:
        if (First = Last) then
          Result := Copy(Lines[First], ColFrom, ColTo - ColFrom)
        else begin
          // step1: calclate total length of result string
          TotalLen := Max(0, Length(Lines[First]) - ColFrom + 1);
          for i := First + 1 to Last - 1 do
            Inc(TotalLen, Length(Lines[i]));
          Inc(TotalLen, ColTo - 1);
          Inc(TotalLen, Length(sLineBreak) * (Last - First));
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PChar(Result);
          CopyAndForward(Lines[First], ColFrom, MaxInt, P);
          CopyAndForward(sLineBreak, 1, MaxInt, P);
          for i := First + 1 to Last - 1 do begin
            CopyAndForward(Lines[i], 1, MaxInt, P);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          CopyAndForward(Lines[Last], 1, ColTo - 1, P);
        end;
      smColumn:
        begin
          if ColFrom > ColTo then
            SwapInt(ColFrom, ColTo);
          // step1: calclate total length of result string
{$IFNDEF SYN_MBCSSUPPORT}
          ColLen := ColTo - ColFrom;
          TotalLen := ColLen + (ColLen + Length(sLineBreak)) * (Last - First);
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PChar(Result);
          for i := First to Last - 1 do begin
            TotalLen:= TotalLen - CopyPaddedAndForward(Lines[i], ColFrom, ColLen, P);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          TotalLen:= TotalLen - CopyPaddedAndForward(Lines[Last], ColFrom, ColLen, P);
          SetLength(Result, TotalLen);
{$ELSE} //SYN_MBCSSUPPORT
          for i := First to Last do begin
            s := Lines[i];
            l := ColFrom;
            r := ColTo;
            MBCSGetSelRangeInLineWhenColumnSelectionMode(s, l, r);
            Inc(TotalLen, r - l);
          end;
          Inc(TotalLen, Length(sLineBreak) * (Last - First));
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PChar(Result);
          for i := First to Last - 1 do begin
            s := Lines[i];
            l := ColFrom;
            r := ColTo;
            MBCSGetSelRangeInLineWhenColumnSelectionMode(s, l, r);
            TotalLen:= TotalLen - CopyPaddedAndForward(s, l, r - l, P);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          s := Lines[Last];
          l := ColFrom;
          r := ColTo;
          MBCSGetSelRangeInLineWhenColumnSelectionMode(s, l, r);
          TotalLen := TotalLen - CopyPaddedAndForward(Lines[Last], l, r - l, P);
          SetLength(Result, TotalLen);
{$ENDIF}
        end;
      smLine:
        begin
          // If block selection includes LastLine,
          // line break code(s) of the last line will not be added.
          // step1: calclate total length of result string
          for i := First to Last do
            Inc(TotalLen, Length(Lines[i]) + Length(sLineBreak));
          if Last = Lines.Count then
            Dec(TotalLen, Length(sLineBreak));
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PChar(Result);
          for i := First to Last - 1 do begin
            CopyAndForward(Lines[i], 1, MaxInt, P);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          CopyAndForward(Lines[Last], 1, MaxInt, P);
          if (Last + 1) < Lines.Count then
            CopyAndForward(sLineBreak, 1, MaxInt, P);
        end;
    end;
  end;
end;

function TCustomSynEdit.GetText: string;
begin
  Result := Lines.Text;
end;

function TCustomSynEdit.GetWordAtCursor : string;
begin
  SetBlockBegin (WordStart);
  SetBlockEnd (WordEnd);
  Result := SelText;
  SetBlockBegin (CaretXY);
  SetBlockend (CaretXY);
end;

procedure TCustomSynEdit.HideCaret;
begin
  if sfCaretVisible in fStateFlags then
{$IFDEF SYN_CLX}
{$ELSE}
    if Windows.HideCaret(Handle) then
      Exclude(fStateFlags, sfCaretVisible);
{$ENDIF}
end;

{$IFDEF SYN_MBCSSUPPORT}
procedure TCustomSynEdit.WMImeComposition(var Msg: TMessage);
var
  imc: HIMC;
  p: PChar;
begin
  if ((Msg.LParam and GCS_RESULTSTR) <> 0) then begin
    imc := ImmGetContext(Handle);
    try
      fImeCount := ImmGetCompositionString(imc, GCS_RESULTSTR, nil, 0);
      GetMem(p, fImeCount + 1);
      try
        ImmGetCompositionString(imc, GCS_RESULTSTR, p, fImeCount + 1);
        p[fImeCount] := #0;
        CommandProcessor(ecImeStr, #0, p);
      finally
        FreeMem(p, fImeCount + 1);
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
  logFont: TLogFont;
begin
  with Msg do begin
    case WParam of
      IMN_SETOPENSTATUS:
        begin
          imc := ImmGetContext(Handle);
          if (imc <> 0) then begin
            GetObject(Font.Handle, SizeOf(TLogFont), @logFont);
            ImmSetCompositionFont(imc, @logFont);

            ImmReleaseContext(Handle, imc);
          end;
        end;
    end;
  end;
  inherited;
end;
{$ENDIF}

procedure TCustomSynEdit.IncPaintLock;
begin
  inc(fPaintLock);
end;

procedure TCustomSynEdit.InvalidateGutter;
begin
  InvalidateGutterLines(-1, -1);
end;

procedure TCustomSynEdit.InvalidateGutterLines(FirstLine, LastLine: integer);
var
  rcInval: TRect;
begin
  if Visible and HandleAllocated then
    if (FirstLine = -1) and (LastLine = -1) then
    begin
      rcInval := Rect(0, 0, fGutterWidth, ClientHeight);
      if sfLinesChanging in fStateFlags then
        UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
      else
{$IFDEF SYN_CLX}
        InvalidateRect(rcInval, False);
{$ELSE}
        InvalidateRect(Handle, @rcInval, False);
{$ENDIF}
    end else begin
      { find the visible lines first }
      if (LastLine < FirstLine) then SwapInt(LastLine, FirstLine);
      FirstLine := Max(FirstLine, TopLine);
      LastLine := Min(LastLine, TopLine + LinesInWindow);
      { any line visible? }
      if (LastLine >= FirstLine) then begin
        rcInval := Rect(0, fTextHeight * (FirstLine - TopLine),
          fGutterWidth, fTextHeight * (LastLine - TopLine + 1));
        if sfLinesChanging in fStateFlags then
          UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
        else
{$IFDEF SYN_CLX}
          InvalidateRect(rcInval, FALSE);
{$ELSE}
          InvalidateRect(Handle, @rcInval, FALSE);
{$ENDIF}
      end;
    end;
end;

procedure TCustomSynEdit.InvalidateLines(FirstLine, LastLine: integer);
var
  rcInval: TRect;
begin
  if Visible and HandleAllocated then
    if (FirstLine = -1) and (LastLine = -1) then begin
      rcInval := ClientRect;
      rcInval.Left := fGutterWidth;
      if sfLinesChanging in fStateFlags then
        UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
      else
{$IFDEF SYN_CLX}
        InvalidateRect(rcInval, FALSE);
{$ELSE}
        InvalidateRect(Handle, @rcInval, FALSE);
{$ENDIF}
    end else begin
      { find the visible lines first }
      if (LastLine < FirstLine) then SwapInt(LastLine, FirstLine);
      FirstLine := Max(FirstLine, TopLine);
      LastLine := Min(LastLine, TopLine + LinesInWindow);
      { any line visible? }
      if (LastLine >= FirstLine) then begin
        rcInval := Rect(fGutterWidth, fTextHeight * (FirstLine - TopLine),
          ClientWidth, fTextHeight * (LastLine - TopLine + 1));
        if sfLinesChanging in fStateFlags then
          UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
        else
{$IFDEF SYN_CLX}
          InvalidateRect(rcInval, FALSE);
{$ELSE}
          InvalidateRect(Handle, @rcInval, FALSE);
{$ENDIF}
      end;
    end;
end;

procedure TCustomSynEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  Data: pointer;
  C: char;
  Cmd: TSynEditorCommand;
begin
  inherited;
  Data := nil;
  C := #0;
  try
    Cmd := TranslateKeyCode(Key, Shift, Data);
    if Cmd <> ecNone then begin
      Key := 0; // eat it.
      Include(fStateFlags, sfIgnoreNextChar);
      CommandProcessor(Cmd, C, Data);
    end else
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
begin
{$IFDEF SYN_MBCSSUPPORT}
  if (fImeCount > 0) then begin
    Dec(fImeCount);
    Exit;
  end;
{$ENDIF}
  // don't fire the event if key is to be ignored
  if not (sfIgnoreNextChar in fStateFlags) then begin
    fKbdHandler.ExecuteKeyPress (Self,Key);
    CommandProcessor(ecChar, Key, nil);
  end else
    // don't ignore further keys
    Exclude(fStateFlags, sfIgnoreNextChar);
end;

//GBN 2002-03-06 Changes for tabs with autoindent starts here
function TCustomSynEdit.LeftSpaces(const Line: string): Integer;
begin
  Result:=LeftSpacesEx(Line,false);
end;

function TCustomSynEdit.LeftSpacesEx(const Line: string; WantTabs: Boolean): Integer;
var
  p: PChar;
begin
  p := pointer(Line);
  if Assigned(p) and (eoAutoIndent in fOptions) then begin
    Result := 0;
    while p^ in [#1..#32] do begin
      if (p^=#9) and (WantTabs) then inc(Result,TabWidth)
      else Inc(Result);
      Inc(p);
    end;
  end else
    Result := 0;
end;

function TCustomSynEdit.GetLeftSpacing(CharCount: Integer; WantTabs: Boolean): String;
begin
  if (WantTabs) and (not (eoTabsToSpaces in Options)) and (CharCount>=TabWidth) then
      Result:=StringOfChar(#9,CharCount div TabWidth)+StringOfChar(#32,CharCount mod TabWidth)
  else Result:=StringOfChar(#32,CharCount);
end;
//GBN 2002-03-06 Changes for tabs with autoindent ends here

procedure TCustomSynEdit.LinesChanging(Sender: TObject);
begin
  Include(fStateFlags, sfLinesChanging);
end;

procedure TCustomSynEdit.LinesChanged(Sender: TObject);
begin
  Exclude(fStateFlags, sfLinesChanging);
  if HandleAllocated then begin
    UpdateScrollBars;
    SetBlockBegin(CaretXY);
{$IFDEF SYN_CLX}
    InvalidateRect(fInvalidateRect, False);
{$ELSE}
    InvalidateRect(Handle, @fInvalidateRect, False);
{$ENDIF}
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
  bWasSel: boolean;
  bStartDrag: boolean;
  iPoint: TPoint;
  StartCaret: TPoint;
begin
  if SelAvail and (Button = mbRight) and
    ((eoRightMouseMovesCursor in Options) and
     not IsPointInSelection(PixelsToRowColumn(Point(X, Y)))) then
  begin
    FBlockEnd := FBlockBegin;
    InvalidateLines(-1, -1);
  end;

  StartCaret.X := FBlockBegin.X;
  StartCaret.Y := FBlockBegin.Y;

  if (Button <> mbLeft) and SelAvail then                                       //jr 2002-04-26
    exit;
  bWasSel := false;
  bStartDrag := FALSE;
  if Button = mbLeft then begin
    if ssDouble in Shift then Exit;
    if SelAvail then begin
        //remember selection state, as it will be cleared later
      bWasSel := true;
      fMouseDownX := X;
      fMouseDownY := Y;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);

  if (Button = mbLeft) or                                                       //jr 2002-04-26
     ((Button = mbRight) and (eoRightMouseMovesCursor in Options)) then         //DDH 10/16/01
    ComputeCaret(X, Y);

  if Button = mbLeft then begin
    MouseCapture := True;
      //if mousedown occured in selected block then begin drag operation
    Exclude(fStateFlags, sfWaitForDragging);
    if bWasSel and (eoDragDropEditing in fOptions) and (X >= fGutterWidth + 2)
      and (SelectionMode = smNormal) and IsPointInSelection(CaretXY)
    then
      bStartDrag := TRUE                                                        //DDH 10/17/01 begin from Flávio Etrusco
    else begin
      { should be called _instead_ of ComputeCaret, just works as notification
      for the plugins right now }
      iPoint := CaretXY;
      CommandProcessor( ecGotoXY, #0, @iPoint );
    end;                                                                        //DDH 10/17/01 end from Flávio Etrusco

  end;
  if (Button = mbLeft) and bStartDrag then
    Include(fStateFlags, sfWaitForDragging)
  else begin
    if not (sfDblClicked in fStateFlags) then begin
      if ssShift in Shift then
      begin
          //BlockBegin and BlockEnd are at the caret position right now
          //Trick them back to the original position so that the lines will
          //be invalidated correctly for the selection
          FBlockBegin.X := StartCaret.X;
          FBlockBegin.Y := StartCaret.Y;
          FBlockEnd.X := StartCaret.X;
          FBlockEnd.Y := StartCaret.Y;

          SetBlockEnd(CaretXY);
      end else begin
        SetBlockBegin(CaretXY);
{begin}                                                                         //mh 2000-11-20
        if (eoAltSetsColumnMode in Options) and (SelectionMode <> smLine) then
        begin
          if ssAlt in Shift then
            SelectionMode := smColumn
          else
            SelectionMode := smNormal;
        end;
{end}                                                                           //mh 2000-11-20
      end;
    end;
  end;
  if (fMouseDownX < fGutterWidth) then
    Include(fStateFlags, sfPossibleGutterClick);
{$IFDEF SYN_CLX}
  SetFocus;
{$ELSE}
  Windows.SetFocus(Handle);
{$ENDIF}
end;

procedure TCustomSynEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;                                                                    //jr 2001-01-06
  Z: integer;
begin
  inherited MouseMove(Shift, x, y);
  if MouseCapture and (sfWaitForDragging in fStateFlags) then begin
    if (Abs(fMouseDownX - X) >= GetSystemMetrics(SM_CXDRAG))
      or (Abs(fMouseDownY - Y) >= GetSystemMetrics(SM_CYDRAG))
    then begin
      Exclude(fStateFlags, sfWaitForDragging);
      BeginDrag(false);
    end;
  end else if (ssLeft in Shift) and MouseCapture then begin
{begin}                                                                         //jr 2001-01-06
(*
    if (X >= fGutterWidth) and (X < ClientWidth)
      and (Y >= 0) and (Y < ClientHeight)
    then
      ComputeCaret(X, Y);
*)
    P := PixelsToRowColumn(Point(X, Y));
    if (X < fGutterWidth) or (X >= ClientWidth) then
      P.X := fCaretX;
    if (Y < 0) or (Y >= ClientHeight) then
      P.Y := fCaretY;
    CaretXY := P;
{end}                                                                           //jr 2001-01-06
    SetBlockEnd(CaretXY);
    // should we begin scrolling?
    Dec(X, fGutterWidth);
    // calculate chars past right
    Z := X - (fCharsInWindow * fCharWidth);
    if Z > 0 then
      Inc(Z, fCharWidth);
    fScrollDeltaX := Max(Z div fCharWidth, 0);
    if fScrollDeltaX = 0 then begin
      // calculate chars past left
      Z := X;
      if Z < 0 then
        Dec(Z, fCharWidth);
      fScrollDeltaX := Min(Z div fCharWidth, 0);
    end;
    // calculate lines past bottom
    Z := Y - (fLinesInWindow * fTextHeight);
    if Z > 0 then
      Inc(Z, fTextHeight);
    fScrollDeltaY := Max(Z div fTextHeight, 0);
    if fScrollDeltaY = 0 then begin
      // calculate lines past top
      Z := Y;
      if Z < 0 then
        Dec(Z, fTextHeight);
      fScrollDeltaY := Min(Z div fTextHeight, 0);
    end;
    fScrollTimer.Enabled := (fScrollDeltaX <> 0) or (fScrollDeltaY <> 0);
  end;
end;

procedure TCustomSynEdit.ScrollTimerHandler(Sender: TObject);
var
  C: TPoint;
  X, Y: Integer;
begin
  GetCursorPos(C);
  C := PixelsToRowColumn(ScreenToClient(C));
  // changes to line / column in one go
  IncPaintLock;
  try
    if fScrollDeltaX <> 0 then begin
      LeftChar := LeftChar + fScrollDeltaX;
      X := LeftChar;
      if fScrollDeltaX > 0 then  // scrolling right?
        Inc(X, CharsInWindow);
      CaretXY := Point(X, C.Y);
      SetBlockEnd(CaretXY);
    end;
    if fScrollDeltaY <> 0 then
    begin
{$IFDEF SYN_CLX}
{$ELSE}
      if GetKeyState(SYNEDIT_SHIFT) < 0 then
        TopLine := TopLine + fScrollDeltaY * LinesInWindow
      else
{$ENDIF}
        TopLine := TopLine + fScrollDeltaY;
      Y := TopLine;
      if fScrollDeltaY > 0 then  // scrolling down?
        Inc(Y, LinesInWindow - 1);
      CaretXY := Point(C.X, Y);
      SetBlockEnd(CaretXY);
    end;
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  fScrollTimer.Enabled := False;
  if (Button = mbRight) and (Shift = [ssRight]) and Assigned(PopupMenu) then
    exit;
  MouseCapture := False;
  if (sfPossibleGutterClick in fStateFlags) and (X < fGutterWidth) then
    DoOnGutterClick(X, Y)
  else
  if fStateFlags * [sfDblClicked, sfWaitForDragging] = [sfWaitForDragging] then
  begin
    ComputeCaret(X, Y);
    SetBlockBegin(CaretXY);
    SetBlockEnd(CaretXY);
    Exclude(fStateFlags, sfWaitForDragging);
  end;
  Exclude(fStateFlags, sfDblClicked);
  Exclude(fStateFlags, sfPossibleGutterClick);
end;

procedure TCustomSynEdit.DoOnGutterClick(X, Y: integer);
var
  i     : integer;
  offs  : integer;
  line  : integer;
  allmrk: TSynEditMarks;
  mark  : TSynEditMark;
begin
  if Assigned(fOnGutterClick) then begin
    line := PixelsToRowColumn(Point(X, Y)).Y;
    if line <= Lines.Count then begin
      Marks.GetMarksForLine(line, allmrk);
      offs := 0;
      mark := nil;
      for i := 1 to maxMarks do begin
        if assigned(allmrk[i]) then begin
          Inc(offs, BookMarkOptions.XOffset);
          if X < offs then begin
            mark := allmrk[i];
            break;
          end;
        end;
      end; //for
      fOnGutterClick(Self, X, Y, line, mark);
    end;
  end;
end;

procedure TCustomSynEdit.Paint;
var
  rcClip, rcDraw: TRect;
  nL1, nL2, nC1, nC2: integer;
begin
  // Get the invalidated rect. Compute the invalid area in lines / columns.
  rcClip := Canvas.ClipRect;
  // columns
  nC1 := LeftChar;
  if (rcClip.Left > fGutterWidth + 2) then
    Inc(nC1, (rcClip.Left - fGutterWidth - 2) div CharWidth);
  nC2 := nC1 +
    (rcClip.Right - fGutterWidth - 2 + CharWidth - 1) div CharWidth;
  // lines
  nL1 := Max(TopLine + rcClip.Top div fTextHeight, TopLine);
  nL2 := Min(TopLine + (rcClip.Bottom + fTextHeight - 1) div fTextHeight,
    Lines.Count);
  // Now paint everything while the caret is hidden.
  HideCaret;
  try
    // First paint the gutter area if it was (partly) invalidated.
    if (rcClip.Left < fGutterWidth) then begin
      rcDraw := rcClip;
      rcDraw.Right := fGutterWidth;
      PaintGutter(rcDraw, nL1, nL2);
    end;
    // Then paint the text area if it was (partly) invalidated.
    if (rcClip.Right > fGutterWidth) then begin
      rcDraw := rcClip;
      rcDraw.Left := Max(rcDraw.Left, fGutterWidth);
      PaintTextLines(rcDraw, nL1, nL2, nC1, nC2);
    end;
    PluginsAfterPaint(Canvas, rcDraw, nL1, nL2);
    // If there is a custom paint handler call it.
    DoOnPaint;
    DoOnPaintTransient(ttAfter);                                                //GBN 2001-10-23

  finally
    UpdateCaret;
  end;
end;

procedure TCustomSynEdit.PaintGutter(AClip: TRect; FirstLine, LastLine: integer);
var
  i, iLine: integer;
  wrapLine:integer;                                                             //Fiala 2001-12-17
  rcLine: TRect;
  bHasOtherMarks: boolean;
  aGutterOffs: PIntArray;
  s: string;
{$IFDEF SYN_WIN32}
  dc: HDC;
{$ENDIF}
  pSize: TSize;                                                                 //DDH 10/16/01

  procedure DrawMark(iMark: integer);
  var
    iLine: integer;
  begin
    if Assigned(fBookMarkOpt.BookmarkImages) and not Marks[i].InternalImage then
    begin
      if Marks[iMark].ImageIndex <= fBookMarkOpt.BookmarkImages.Count then begin
        iLine := Marks[iMark].Line - TopLine;
//        if Marks[iMark].IsBookmark then
        if Marks[iMark].IsBookmark = BookMarkOptions.DrawBookmarksFirst then    //mh 2000-10-12
          aGutterOffs^[iLine] := 0
        else if aGutterOffs^[iLine] = 0 then
          aGutterOffs^[iLine] := fBookMarkOpt.XOffset;
        with fBookMarkOpt do
          BookmarkImages.Draw(Canvas, LeftMargin + aGutterOffs^[iLine],
            iLine * fTextHeight, Marks[iMark].ImageIndex);
        Inc(aGutterOffs^[iLine], fBookMarkOpt.XOffset);
      end;
    end else
    begin
      if Marks[iMark].ImageIndex in [0..9] then begin
        iLine := Marks[iMark].Line - TopLine;
        if not Assigned(fInternalImage) then begin
          fInternalImage := TSynInternalImage.Create('SynEditInternalImages',
            10);
        end;
        if aGutterOffs^[iLine] = 0 then
        begin
          fInternalImage.DrawMark(Canvas, Marks[iMark].ImageIndex,
            fBookMarkOpt.LeftMargin + aGutterOffs^[iLine],
            iLine * fTextHeight, fTextHeight);
        end;
        Inc(aGutterOffs^[iLine], fBookMarkOpt.XOffset);
      end;
    end;
  end;

begin
  if (FirstLine = 1) and (LastLine = 0) then
    LastLine := 1;
  // Changed to use fTextDrawer.BeginDrawing and fTextDrawer.EndDrawing only
  // when absolutely necessary.  Note: Never change brush / pen / font of the
  // canvas inside of this block (only through methods of fTextDrawer)!
  Canvas.Brush.Color := Gutter.Color;
  // If we have to draw the line numbers then we don't want to erase
  // the background first. Do it line by line with TextRect instead
  // and fill only the area after the last visible line.
{$IFDEF SYN_CLX}
{$ELSE}
  dc := Canvas.Handle;
{$ENDIF}
  if fGutter.ShowLineNumbers then
  begin
{$IFDEF SYN_CLX}
    fTextDrawer.BeginDrawing(canvas);
{$ELSE}
    fTextDrawer.BeginDrawing(dc);
{$ENDIF}
    try
      //DDH 10/16/01 Start
//      fTextDrawer.SetBackColor(fGutter.Color);
//      fTextDrawer.SetForeColor(Self.Font.Color);
//      if fGutter.UseFontStyle then
//        fTextDrawer.Style := Font.Style
//      else
//        fTextDrawer.Style := [];
      if fGutter.UseFontStyle then
      begin
        fTextDrawer.SetBaseFont(fGutter.Font);
        fTextDrawer.SetForeColor(fGutter.Font.Color)
      end
      else begin
        fTextDrawer.SetForeColor(self.Font.Color);
        fTextDrawer.Style := [];
      end;
      fTextDrawer.SetBackColor(fGutter.Color);

      // prepare the rect initially
      rcLine := AClip;
      rcLine.Right := Max(rcLine.Right, fGutterWidth - 2);
      rcLine.Bottom := (FirstLine - TopLine) * fTextHeight;
{begin}                                                                         //Fiala 2001-12-17
      if fWordWrap and (fLines.Count > 1) then
      begin
        { firs we must count real lines from begin }
        wrapLine := 0;
        for i := 0 to FirstLine - 1 do
          if not TSynEditStringList(fLines).IsLineWraped(i) then Inc(wrapLine);
        { now we will paint line numbers }
        for i := FirstLine to LastLine do begin
          rcLine.Top := rcLine.Bottom;
          Inc(rcLine.Bottom, fTextHeight);
          if not TSynEditStringList(fLines).IsLineWraped(i-1) then begin
            s := fGutter.FormatLineNumber(wrapLine);
            Inc(wrapLine);
          end
          else
            s := '';
{$IFDEF SYN_CLX}
          Canvas.Brush.Style := bsSolid;
          Canvas.FillRect(rcLine);
          Canvas.TextRect(rcLine, fGutter.LeftOffset, rcLine.Top, s);
{$ELSE}
          GetTextExtentPoint32(DC, PChar(s), Length(s), pSize);
          Windows.ExtTextOut(DC, (fGutterWidth - fGutter.RightOffset - 2) - pSize.cx,
            rcLine.Top + ((fTextHeight - pSize.cy) shr 1), ETO_OPAQUE,
            @rcLine, PChar(s), Length(s), nil);
{$ENDIF}
        end;
      end
{end}                                                                           //Fiala 2001-12-17
      else
      begin
        for iLine := FirstLine to LastLine do begin
          // next line rect
          rcLine.Top := rcLine.Bottom;
          Inc(rcLine.Bottom, fTextHeight);
          // erase the background and draw the line number string in one go
          s := fGutter.FormatLineNumber(iLine);
{$IFDEF SYN_CLX}
          Canvas.Brush.Style := bsSolid;
          Canvas.FillRect(rcLine);
          Canvas.TextRect(rcLine, fGutter.LeftOffset, rcLine.Top, s);
{$ELSE}
          GetTextExtentPoint32(DC, PChar(s), Length(s), pSize);
          Windows.ExtTextOut(DC, (fGutterWidth - fGutter.RightOffset - 2) - pSize.cx,
            rcLine.Top + ((fTextHeight - pSize.cy) shr 1), ETO_OPAQUE,
            @rcLine, PChar(s), Length(s), nil);
          //DDH 10/16/01 End
{$ENDIF}
        end;
      end;
      // now erase the remaining area if any
      if AClip.Bottom > rcLine.Bottom then begin
        rcLine.Top := rcLine.Bottom;
        rcLine.Bottom := AClip.Bottom;
        with rcLine do
          fTextDrawer.ExtTextOut(Left, Top, ETO_OPAQUE, rcLine, nil, 0);
      end;
    finally
      fTextDrawer.EndDrawing;
      //DDH 10/16/01 Begin
      if fGutter.UseFontStyle then
        fTextDrawer.SetBaseFont(Self.Font);
      //DDH 10/16/01 End
    end;
  end
  else
{$IFDEF SYN_CLX}
    InternalFillRect(canvas, AClip);
{$ELSE}
    InternalFillRect(dc, AClip);
{$ENDIF}

  // the gutter separator if visible
  if AClip.Right >= fGutterWidth - 2 then
    with Canvas do
    begin
      Pen.Color := Color;   // clWindow; //clBtnHighlight;                      //AC 10/16/01
      Pen.Width := 1;
      with AClip do
      begin
        MoveTo(fGutterWidth - 2, Top);
        LineTo(fGutterWidth - 2, Bottom);
        Pen.Color := fGutter.Color;  //clBtnShadow;                             //AC 10/16/01
        MoveTo(fGutterWidth - 1, Top);
        LineTo(fGutterWidth - 1, Bottom);
      end;
    end;
  // now the gutter marks
  if BookMarkOptions.GlyphsVisible and (Marks.Count > 0)
    and (LastLine >= FirstLine)
  then begin
    aGutterOffs := AllocMem((LastLine - TopLine + 1) * SizeOf(integer));
    try
      // Instead of making a two pass loop we look while drawing the bookmarks
      // whether there is any other mark to be drawn
      bHasOtherMarks := FALSE;
      for i := 0 to Marks.Count - 1 do with Marks[i] do
        if Visible and (Line >= FirstLine) and (Line <= LastLine) then
        begin
          if IsBookmark <> BookMarkOptions.DrawBookmarksFirst then              //mh 2000-10-12
            bHasOtherMarks := TRUE
          else
            DrawMark(i);
        end;
      if bHasOtherMarks then
        for i := 0 to Marks.Count - 1 do with Marks[i] do
        begin
          if Visible and (IsBookmark <> BookMarkOptions.DrawBookmarksFirst)     //mh 2000-10-12
            and (Line >= FirstLine) and (Line <= LastLine)
          then
            DrawMark(i);
        end;
    finally
      FreeMem(aGutterOffs);
    end;
  end;
end;

procedure TCustomSynEdit.PaintTextLines(AClip: TRect; FirstLine, LastLine,
  FirstCol, LastCol: integer);
var
  bDoRightEdge: boolean; // right edge
  nRightEdge: integer;
    // selection info
  bAnySelection: boolean; // any selection visible?
  nSelL1, nSelCol1: integer; // start of selected area
  nSelL2, nSelCol2: integer; // end of selected area
    // info about normal and selected text and background colors
  bSpecialLine, bLineSelected: boolean;
  colFG, colBG: TColor;
  colSelFG, colSelBG: TColor;
    // info about selction of the current line
  nSelStart, nSelEnd: integer;
  bComplexLine: boolean;
    // painting the background and the text
  rcLine, rcToken: TRect;
  TokenAccu: record
    // Note: s is not managed as a string, it will only grow!!!
    // Never use AppendStr or "+", use Len and MaxLen instead and
    // copy the string chars directly. This is for efficiency.
    Len, MaxLen, CharsBefore: integer;
    s: string;
    TabString: String;
    FG, BG: TColor;
    Style: TFontStyles;
  end;
{$IFDEF SYN_WIN32}
  dc: HDC;
{$ENDIF}
  SynTabGlyphString: String;                                                    //DDH 2001-12-17

{ local procedures }

  function colEditorBG: TColor;
  var
    iAttri: TSynHighlighterAttributes;
  begin
    Result := Color;
    if Highlighter <> nil then begin
      iAttri := Highlighter.WhitespaceAttribute;
      if (iAttri <> nil) and (iAttri.Background <> clNone) then
        Result := iAttri.Background;
    end;
  end;

  procedure ComputeSelectionInfo;
  var
    p: TPoint;
  begin
    bAnySelection := FALSE;
    // Only if selection is visible anyway.
    if (not HideSelection or Self.Focused) then begin
      bAnySelection := TRUE;
      // Get the *real* start of the selected area.
      if (fBlockBegin.Y < fBlockEnd.Y) then begin
        nSelL1 := fBlockBegin.Y;
        nSelCol1 := fBlockBegin.X;
        nSelL2 := fBlockEnd.Y;
        nSelCol2 := fBlockEnd.X;
      end else if (fBlockBegin.Y > fBlockEnd.Y) then begin
        nSelL2 := fBlockBegin.Y;
        nSelCol2 := fBlockBegin.X;
        nSelL1 := fBlockEnd.Y;
        nSelCol1 := fBlockEnd.X;
      end else if (fBlockBegin.X <> fBlockEnd.X) then begin
        // No selection at all, or it is only on this line.
        nSelL1 := fBlockBegin.Y;
        nSelL2 := nSelL1;
        if (fBlockBegin.X < fBlockEnd.X) then begin
          nSelCol1 := fBlockBegin.X;
          nSelCol2 := fBlockEnd.X;
        end else begin
          nSelCol2 := fBlockBegin.X;
          nSelCol1 := fBlockEnd.X;
        end;
      end else
        bAnySelection := FALSE;
      // If there is any visible selection so far, then test if there is an
      // intersection with the area to be painted.
      if bAnySelection then begin
      // Don't care if the selection is not visible.
        bAnySelection := (nSelL2 >= FirstLine) and (nSelL1 <= LastLine);
      // In the column selection mode sort the begin and end of the selection,
      // this makes the painting code simpler.
        if (SelectionMode = smColumn) and (nSelCol1 > nSelCol2) then
          SwapInt(nSelCol1, nSelCol2);
        if bAnySelection then begin
          // Transform the selection from text space into screen space
          p := LogicalToPhysicalPos(Point(nSelCol1, nSelL1));
          nSelCol1 := p.x;
          nSelL1 := p.y;
          p := LogicalToPhysicalPos(point(nSelCol2, nSelL2));
          nSelCol2 := p.x;
          nSelL2 := p.y;
        end;
      end;
    end;
  end;

  procedure SetDrawingColors(Selected: boolean);
  begin
    with fTextDrawer do
      if Selected then begin
        SetBackColor(colSelBG);
        SetForeColor(colSelFG);
      end else begin
        SetBackColor(colBG);
        SetForeColor(colFG);
      end;
  end;

  function ColumnToXValue(Col: integer): integer;
  begin
    Result := fTextOffset + Pred(Col) * fCharWidth;
  end;

  // CharsBefore tells if Token starts at column one or not

  procedure PaintToken(Token: string;
    TokenLen, CharsBefore, First, Last: integer);
  var
    pszText: PChar;
    Counter, nX, nCharsToPaint: integer;
    sTabbedToken: String;
    DoTabPainting: Boolean;
    i, TabStart, TabLen : Integer;
    rcTab: TRect;
  const
    ETOOptions = ETO_CLIPPED or ETO_OPAQUE;
  begin
    sTabbedToken := Token;
    DoTabPainting := False;

    Counter := Last - CharsBefore;
    while Counter > First - CharsBefore - 1 do
    begin
      if (Length(Token) >= Counter) then
      begin
        if (fShowSpecChar) and (Token[Counter] = #32) then Token[Counter] := SynSpaceGlyph;
        if (Token[Counter] = TSynTabChar) then     // plpolak
        begin
          Token[Counter] := #32;  //Tabs painted differently if necessary
          DoTabPainting := fShowSpecChar;
        end;
      end;
      Dec(Counter);
    end;

    if (Last >= First) and (rcToken.Right > rcToken.Left) then begin
      nX := ColumnToXValue(First);
      Dec(First, CharsBefore);
      Dec(Last, CharsBefore);
      if (First > TokenLen) then begin
        pszText := nil;
        nCharsToPaint := 0;
      end else begin
{$IFDEF SYN_MBCSSUPPORT}
        if (First > 1) and (ByteType(Token, First) = mbTrailByte) then begin
          Dec(First);
          Dec(nX, fCharWidth);
        end;
{$ENDIF}
        pszText := PChar(@Token[First]);
        nCharsToPaint := Min(Last - First + 1, TokenLen - First + 1);
      end;
      fTextDrawer.ExtTextOut(nX, rcToken.Top, ETOOptions, rcToken,
        pszText, nCharsToPaint);

      if DoTabPainting then
      begin
        //fix everything before the FirstChar
          for i := 1 to First - 1 do               //wipe the text out so we don't
            if sTabbedToken[i] = TSynTabChar then  //count it out of the range
              sTabbedToken[i] := #32;              //we're looking for

        TabStart := pos(TSynTabChar, sTabbedToken);
        rcTab.Top := rcToken.Top;
        rcTab.Bottom := rcToken.Bottom;
        while (TabStart > 0) and (TabStart >= First) and (TabStart <= Last) do
        begin
          TabLen := 1;
          While (TabStart + CharsBefore + TabLen - 1) mod FTabWidth <> 0 do inc(TabLen);
          pszText := PChar(@SynTabGlyphString[1]);

          nX := ColumnToXValue(CharsBefore + TabStart + (TabLen div 2) - 1);
          if TabLen mod 2 = 0 then
            nX := nX + (fCharWidth div 2)
          else nX := nX + fCharWidth;

          rcTab.Left := nX;
          rcTab.Right := nX + fTextDrawer.GetCharWidth;

          fTextDrawer.ExtTextOut(nX, rcTab.Top, ETOOptions, rcTab,
            pszText, 1);

          for i := 0 to TabLen - 1 do           //wipe the text out so we don't
            sTabbedToken[TabStart + i] := #32;  //count it again

          TabStart := pos(TSynTabChar, sTabbedToken);
        end;
      end;
      rcToken.Left := rcToken.Right;
    end;
  end;

  procedure PaintHighlightToken(bFillToEOL: boolean);
  var
    bComplexToken: boolean;
    nC1, nC2, nC1Sel, nC2Sel: integer;
    bU1, bSel, bU2: boolean;
    nX1, nX2: integer;
  begin
    // Compute some helper variables.
    nC1 := Max(FirstCol, TokenAccu.CharsBefore + 1);
    nC2 := Min(LastCol, TokenAccu.CharsBefore + TokenAccu.Len + 1);
    if bComplexLine then begin
      bU1 := (nC1 < nSelStart);
      bSel := (nC1 < nSelEnd) and (nC2 >= nSelStart);
      bU2 := (nC2 >= nSelEnd);
      bComplexToken := bSel and (bU1 or bU2);
    end else begin
      bU1 := FALSE; // to shut up Compiler warning Delphi 2
      bSel := bLineSelected;
      bU2 := FALSE; // to shut up Compiler warning Delphi 2
      bComplexToken := FALSE;
    end;
    // Any token chars accumulated?
    if (TokenAccu.Len > 0) then begin
      // Initialize the colors and the font style.
      if not bSpecialLine then begin
        colBG := TokenAccu.BG;
        colFG := TokenAccu.FG;
      end;

      if bSpecialLine and (eoSpecialLineDefaultFg in fOptions) then             //EK 10/16/01
        colFG := TokenAccu.FG;

      fTextDrawer.SetStyle(TokenAccu.Style);
      // Paint the chars
      if bComplexToken then begin
        // first unselected part of the token
        if bU1 then begin
          SetDrawingColors(FALSE);
          rcToken.Right := ColumnToXValue(nSelStart);
          with TokenAccu do PaintToken(s, Len, CharsBefore, nC1, nSelStart);
        end;
        // selected part of the token
        SetDrawingColors(TRUE);
        nC1Sel := Max(nSelStart, nC1);
        nC2Sel := Min(nSelEnd, nC2);
        rcToken.Right := ColumnToXValue(nC2Sel);
        with TokenAccu do PaintToken(s, Len, CharsBefore, nC1Sel, nC2Sel);
        // second unselected part of the token
        if bU2 then begin
          SetDrawingColors(FALSE);
          rcToken.Right := ColumnToXValue(nC2);
          with TokenAccu do PaintToken(s, Len, CharsBefore, nSelEnd, nC2);
        end;
      end else begin
        SetDrawingColors(bSel);
        rcToken.Right := ColumnToXValue(nC2);

        with TokenAccu do PaintToken(s, Len, CharsBefore, nC1, nC2);
      end;
    end;

    // Fill the background to the end of this line if necessary.
    if bFillToEOL and (rcToken.Left < rcLine.Right) then
    begin
      if not bSpecialLine then colBG := colEditorBG;
      if bComplexLine then
      begin
        nX1 := ColumnToXValue(nSelStart);
        nX2 := ColumnToXValue(nSelEnd);
        if (rcToken.Left < nX1) then
        begin
          SetDrawingColors(FALSE);
          rcToken.Right := nX1;
{$IFDEF SYN_CLX}
          InternalFillRect(Canvas, rcToken);
{$ELSE}
          InternalFillRect(dc, rcToken);
{$ENDIF}
          rcToken.Left := nX1;
        end;
        if (rcToken.Left < nX2) then
        begin
          SetDrawingColors(TRUE);
          rcToken.Right := nX2;
{$IFDEF SYN_CLX}
          InternalFillRect(Canvas, rcToken);
{$ELSE}
          InternalFillRect(dc, rcToken);
{$ENDIF}
          rcToken.Left := nX2;
        end;
        if (rcToken.Left < rcLine.Right) then begin
          SetDrawingColors(FALSE);
          rcToken.Right := rcLine.Right;
{$IFDEF SYN_CLX}
          InternalFillRect(Canvas, rcToken);
{$ELSE}
          InternalFillRect(dc, rcToken);
{$ENDIF}
        end;
      end
      else
      begin
        SetDrawingColors(bLineSelected);
        rcToken.Right := rcLine.Right;
{$IFDEF SYN_CLX}
        InternalFillRect(Canvas, rcToken);
{$ELSE}
        InternalFillRect(dc, rcToken);
{$ENDIF}
      end;
    end;
  end;

  procedure AddHighlightToken(const Token: AnsiString;
    CharsBefore, TokenLen: integer;
    Foreground, Background: TColor;
    Style: TFontStyles);
  var
    bCanAppend: boolean;
    bSpacesTest, bIsSpaces: boolean;
    i: integer;

    function TokenIsSpaces: boolean;
    var
      pTok: PChar;
    begin
      if not bSpacesTest then begin
        bSpacesTest := TRUE;
        pTok := PChar(Token);
        while (pTok^ <> #0) do begin
          if (pTok^ <> #32) then break;
          Inc(pTok);
        end;
        bIsSpaces := (pTok^ = #0);
      end;
      Result := bIsSpaces;
    end;

  begin
    if Background = clNone then Background := colEditorBG;
    if Foreground = clNone then Foreground := Font.Color;
    // Do we have to paint the old chars first, or can we just append?
    bCanAppend := FALSE;
    bSpacesTest := FALSE;
    if (TokenAccu.Len > 0) then begin
      // font style must be the same or token is only spaces
      if (TokenAccu.Style = Style)
        or (not (fsUnderline in Style) and not (fsUnderline in TokenAccu.Style)
          and TokenIsSpaces)
      then
//EK 10/16/01 Begin
      // either special colors or same colors
        if (bSpecialLine and not (eoSpecialLineDefaultFg in fOptions)) or bLineSelected or // <- implementation
        // background color must be the same and
        ((TokenAccu.BG = Background) and
          // foreground color must be the same or token is only spaces
          ((TokenAccu.FG = Foreground) or TokenIsSpaces))
          then bCanAppend := TRUE;
//EK 10/16/01 End
      // If we can't append it, then we have to paint the old token chars first.
      if not bCanAppend then
        PaintHighlightToken(FALSE);
    end;
    // Don't use AppendStr because it's more expensive.
    if bCanAppend then begin
      if (TokenAccu.Len + TokenLen > TokenAccu.MaxLen) then begin
        TokenAccu.MaxLen := TokenAccu.Len + TokenLen + 32;
        SetLength(TokenAccu.s, TokenAccu.MaxLen);
      end;
      for i := 1 to TokenLen do
        TokenAccu.s[TokenAccu.Len + i] := Token[i];
      Inc(TokenAccu.Len, TokenLen);
    end else begin
      TokenAccu.Len := TokenLen;
      if (TokenAccu.Len > TokenAccu.MaxLen) then begin
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
    nLine: integer; // line index for the loop
    sLine: string; // the current line (expanded)
//    pConvert: TConvertTabsProc;                                               //mh 2000-10-19
    sToken: string; // highlighter token info
    nTokenPos, nTokenLen: integer;
    attr: TSynHighlighterAttributes;
//    i: Integer;                                                                 //Fiala 2001-12-17
  begin
    // Initialize rcLine for drawing. Note that Top and Bottom are updated
    // inside the loop. Get only the starting point for this.
    rcLine := AClip;
    rcLine.Bottom := (FirstLine - TopLine) * fTextHeight;
    // Make sure the token accumulator string doesn't get reassigned to often.
    if Assigned(fHighlighter) then begin
      TokenAccu.MaxLen := Max(128, fCharsInWindow);
      SetLength(TokenAccu.s, TokenAccu.MaxLen);
    end;
{begin}                                                                         //mh 2000-10-19
    // Find the fastest function for the tab expansion.
//    pConvert := GetBestConvertTabsProc(fTabWidth);
    // Now loop through all the lines. The indices are valid for Lines.
    for nLine := FirstLine to LastLine do begin
      // Get the expanded line.
//      sLine := pConvert(Lines[nLine - 1], fTabWidth);
      sLine := TSynEditStringList(Lines).ExpandedStrings[nLine - 1];
{end}                                                                           //mh 2000-10-19
      // Get the information about the line selection. Three different parts
      // are possible (unselected before, selected, unselected after), only
      // unselected or only selected means bComplexLine will be FALSE. Start
      // with no selection, compute based on the visible columns.
      bComplexLine := FALSE;
      nSelStart := 0;
      nSelEnd := 0;
      // Does the selection intersect the visible area?
      if bAnySelection and (nLine >= nSelL1) and (nLine <= nSelL2) then begin
        // Default to a fully selected line. This is correct for the smLine
        // selection mode and a good start for the smNormal mode.
        nSelStart := FirstCol;
        nSelEnd := LastCol + 1;
        if (SelectionMode = smColumn) or
          ((SelectionMode = smNormal) and (nLine = nSelL1))
        then
          if (nSelCol1 > LastCol) then begin
            nSelStart := 0;
            nSelEnd := 0;
          end else if (nSelCol1 > FirstCol) then begin
            nSelStart := nSelCol1;
            bComplexLine := TRUE;
          end;
        if (SelectionMode = smColumn) or
          ((SelectionMode = smNormal) and (nLine = nSelL2))
        then
          if (nSelCol2 < FirstCol) then begin
            nSelStart := 0;
            nSelEnd := 0;
          end else if (nSelCol2 < LastCol) then begin
            nSelEnd := nSelCol2;
            bComplexLine := TRUE;
          end;
{$IFDEF SYN_MBCSSUPPORT}
        if (SelectionMode = smColumn) then
          MBCSGetSelRangeInLineWhenColumnSelectionMode(sLine, nSelStart,
            nSelEnd);
{$ENDIF}
      end;
      // Update the rcLine rect to this line.
      rcLine.Top := rcLine.Bottom;
      Inc(rcLine.Bottom, fTextHeight);
      // Initialize the text and background colors, maybe the line should
      // use special values for them.
      colFG := Font.Color;
      colBG := colEditorBG;
      bSpecialLine := DoOnSpecialLineColors(nLine, colFG, colBG);
      if bSpecialLine then begin
        // The selection colors are just swapped, like seen in Delphi.
        colSelFG := colBG;
        colSelBG := colFG;
      end else begin
        colSelFG := fSelectedColor.Foreground;
        colSelBG := fSelectedColor.Background;
      end;
      // Paint the lines depending on the assigned highlighter.
      bLineSelected := not bComplexLine and (nSelStart > 0);
      rcToken := rcLine;
      if not(Assigned(fHighlighter)) or not(fHighlighter.Enabled) then begin     //DDH 2001-10-23 Added the enabled property
        // Note: The PaintToken procedure will take care of invalid parameters
        // like empty token rect or invalid indices into sLine.
        nTokenLen := Length(sLine);
        if bComplexLine then begin
          SetDrawingColors(FALSE);
          rcToken.Left := Max(rcLine.Left, ColumnToXValue(FirstCol));
          rcToken.Right := Min(rcLine.Right, ColumnToXValue(nSelStart));
          PaintToken(sLine, nTokenLen, 0, FirstCol, nSelStart);
          rcToken.Left := Max(rcLine.Left, ColumnToXValue(nSelEnd));
          rcToken.Right := Min(rcLine.Right, ColumnToXValue(LastCol));
          PaintToken(sLine, nTokenLen, 0, nSelEnd, LastCol);
          SetDrawingColors(TRUE);
          rcToken.Left := Max(rcLine.Left, ColumnToXValue(nSelStart));
          rcToken.Right := Min(rcLine.Right, ColumnToXValue(nSelEnd));
          PaintToken(sLine, nTokenLen, 0, nSelStart, nSelEnd);
        end else begin
          SetDrawingColors(bLineSelected);
{begin}                                                                         //Fiala 2001-12-17
          if fShowSpecChar then
          begin
            if not fWordWrap or (nLine = fLines.Count)
                or not TSynEditStringList(Lines).IsLineWraped(nLine)
            then begin
              sLine := sLine + SynLineBreakGlyph;
              Inc(nTokenLen);
            end;
          end;
{end}                                                                           //Fiala 2001-12-17
          PaintToken(sLine, nTokenLen, 0, FirstCol, LastCol);
        end;
      end else begin
        // Initialize highlighter with line text and range info. It is
        // necessary because we probably did not scan to the end of the last
        // line - the internal highlighter range might be wrong.
//        fHighlighter.SetRange(Lines.Objects[nLine - 1]);
        fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[nLine - 1]);     //mh 2000-10-10
        fHighlighter.SetLine(sLine, nLine - 1);
        // Try to concatenate as many tokens as possible to minimize the count
        // of ExtTextOut calls necessary. This depends on the selection state
        // or the line having special colors. For spaces the foreground color
        // is ignored as well.
        TokenAccu.Len := 0;
{begin}                                                                         //Fiala 2001-12-17
        nTokenPos := 0;
        nTokenLen := 0;
{end}                                                                           //Fiala 2001-12-17
        attr := nil;

        while not fHighlighter.GetEol do begin
          // Test first whether anything of this token is visible.
          nTokenPos := fHighlighter.GetTokenPos; // zero-based
          sToken := fHighlighter.GetToken;
          nTokenLen := Length(sToken);
          if (nTokenPos + nTokenLen >= FirstCol) and (nTokenPos <= LastCol) then begin
            // It's at least partially visible. Get the token attributes now.
            attr := fHighlighter.GetTokenAttribute;
            // Store the token chars with the attributes in the TokenAccu
            // record. This will paint any chars already stored if there is
            // a (visible) change in the attributes.
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
{begin}                                                                         //Fiala 2001-12-17
        if fShowSpecChar then
          if not fWordWrap or (nLine = fLines.Count)
                or not TSynEditStringList(Lines).IsLineWraped(nLine) then
        begin
          if assigned(attr) and (attr = fHighlighter.CommentAttribute) then
          begin
            AddHighlightToken(SynLineBreakGlyph, nTokenPos + nTokenLen, 1, attr.Foreground,
              attr.Background, []);
          end else begin
            AddHighlightToken(SynLineBreakGlyph, nTokenPos + nTokenLen, 1, fHighlighter.WhitespaceAttribute.Foreground,
              fHighlighter.WhitespaceAttribute.Background, []);
          end;
        end;
{end}                                                                           //Fiala 2001-12-17
        PaintHighlightToken(TRUE);
      end;
      // Now paint the right edge if necessary. We do it line by line to reduce
      // the flicker. Should not cost very much anyway, compared to the many
      // calls to ExtTextOut.
      if bDoRightEdge then
      begin
{$IFDEF SYN_CLX}
        Canvas.MoveTo(nRightEdge, rcLine.Top);
        Canvas.LineTo(nRightEdge, rcLine.Bottom + 1);
{$ELSE}
        Windows.MoveToEx(dc, nRightEdge, rcLine.Top, nil);
        Windows.LineTo(dc, nRightEdge, rcLine.Bottom + 1);
{$ENDIF}
      end;
    end;
  end;

{ end local procedures }

begin
  // If the right edge is visible and in the invalid area, prepare to paint it.
  // Do this first to realize the pen when getting the dc variable.
  SynTabGlyphString := SynTabGlyph;
  bDoRightEdge := FALSE;
  if (fRightEdge > 0) then begin // column value
    nRightEdge := fTextOffset + fRightEdge * fCharWidth; // pixel value
    if (nRightEdge >= AClip.Left) and (nRightEdge <= AClip.Right) then begin
      bDoRightEdge := TRUE;
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
{$IFDEF SYN_CLX}
    Canvas.Brush.Color := ColorToRGB(colEditorBG);
    InternalFillRect(Canvas, rcToken);
{$ELSE}
    SetBkColor(dc, ColorToRGB(colEditorBG));
    InternalFillRect(dc, rcToken);
{$ENDIF}
    // Adjust the invalid area to not include this area.
    AClip.Left := rcToken.Right;
  end;
  // Paint the visible text lines. To make this easier, compute first the
  // necessary information about the selected area: is there any visible
  // selected area, and what are its lines / columns?
  // Moved to two local procedures to make it easier to read.
  if (LastLine >= FirstLine) then begin
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
  rcToken.Top := (LastLine - TopLine + 1) * fTextHeight;
  if (rcToken.Top < rcToken.Bottom) then
  begin
{$IFDEF SYN_CLX}
    Canvas.Brush.Color := ColorToRGB(colEditorBG);
    InternalFillRect(Canvas, rcToken);
{$ELSE}
    SetBkColor(dc, ColorToRGB(colEditorBG));
    InternalFillRect(dc, rcToken);
{$ENDIF}
    // Draw the right edge if necessary.
    if bDoRightEdge then
    begin
{$IFDEF SYN_CLX}
      Canvas.MoveTo(nRightEdge, rcToken.Top);
      Canvas.LineTo(nRightEdge, rcToken.Bottom + 1);
{$ELSE}
      Windows.MoveToEx(dc, nRightEdge, rcToken.Top, nil);
      Windows.LineTo(dc, nRightEdge, rcToken.Bottom + 1);
{$ENDIF}
    end;
  end;
end;

procedure TCustomSynEdit.PasteFromClipboard;
var
  StartOfBlock: TPoint;
  EndOfBlock: TPoint;
  oldWrap: Boolean;//js 07-04-2002 gives error in clx version if defined after $ELSE
{$IFDEF SYN_CLX}
{$ELSE}
  PasteMode: TSynSelectionMode;
  Mem: HGLOBAL;
  P: PChar;
  Tag: Integer;
{$ENDIF}
begin
  if ReadOnly then                                                              //jcr 2001-01-16
    exit;
  DoOnPaintTransient(ttBefore);                                                 //GBN 2001-10-23
  BeginUndoBlock;                                                               //mh 2000-11-20
  oldWrap := fWordWrap;                                                         //Fiala 2001-12-17
  fIsPasting:= true;                                                            //jcr 2002-04-13
  try
{$IFDEF SYN_CLX}
{$ELSE}
    // Check for our special format first.
    if Clipboard.HasFormat(SynEditClipboardFormat) then begin
      Clipboard.Open;
      try
        Mem := Clipboard.GetAsHandle(SynEditClipboardFormat);
        P := GlobalLock(Mem);
        if P <> nil then begin
          if SelAvail then begin
//            fUndoList.AddChange(crSelDelete, fBlockBegin, fBlockEnd, SelText,
//              SelectionMode);
            fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, SelText,      //mh 2000-11-20
              SelectionMode);
          end;
          {begin}                                                               //Fiala 2001-12-17
          if fWordWrap then begin
            fUndoList.AddChange(crUnWrap, CaretXY, CaretXY, '', SelectionMode);
            WordWrap := False;
          end;
          {end}                                                                 //Fiala 2001-12-17

          // Our format: SelectionMode value followed by text.
          // See CopyToClipboard
          PasteMode := PSynSelectionMode(P)^;
          inc(P, SizeOf(TSynSelectionMode));
          if SelAvail then begin
            StartOfBlock := minPoint(fBlockBegin, fBlockEnd);
            EndOfBlock := maxPoint(fBlockBegin, fBlockEnd);
            fBlockBegin := StartOfBlock;
            fBlockEnd := EndOfBlock;
            if SelectionMode = smLine then
              // Pasting always occurs at column 0 when current selection is
              // smLine type
              StartOfBlock.X := 1;
          end else
            StartOfBlock := Point(CaretX, CaretY);
          Tag := 0;

          // problems with paste in smLineMode, better switch to normal
//          if PasteMode = smLine then PasteMode := smNormal;                     //Fiala 2001-12-17

          SetSelTextPrimitive(PasteMode, P, @Tag);
          EndOfBlock := BlockEnd;
          if PasteMode = smNormal then
            fUndoList.AddChange(crPaste, StartOfBlock, EndOfBlock, SelText,
              PasteMode)
          else if PasteMode = smColumn then
            // Do nothing moved to InsertColumn                                 //DDH 2001-11-2 from Jeff Rafter
          else if PasteMode = smLine then
            if CaretX = 1 then
              fUndoList.AddChange(crPaste, Point(1, StartOfBlock.y),
                Point(CharsInWindow, EndOfBlock.y - 1), SelText, smLine)
            else
              fUndoList.AddChange(crPaste, Point(1, StartOfBlock.y),
                EndOfBlock, SelText, smNormal);

          if PasteMode = smColumn then
            CaretXY := Point(Min(StartOfBlock.X, EndOfBlock.X),
              Max(StartOfBlock.Y, EndOfBlock.Y) + 1);
        end else
          raise ESynEditError.Create('Clipboard paste operation failed.');
      finally
        Clipboard.Close;
      end;
    // If our special format isn't there, check for regular text format.
    end else if Clipboard.HasFormat(CF_TEXT) then begin
{$ENDIF}
      // Normal text is much easier...
      if SelAvail then begin
//        fUndoList.AddChange(crSelDelete, fBlockBegin, fBlockEnd, SelText,
//          SelectionMode);
        fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, SelText,          //mh 2000-11-20
          SelectionMode);
      end;
      StartOfBlock := minPoint(fBlockBegin, fBlockEnd);
      EndOfBlock := maxPoint(fBlockBegin, fBlockEnd);
      fBlockBegin := StartOfBlock;
      fBlockEnd := EndOfBlock;
      LockUndo;
      SelText := Clipboard.AsText;
      UnlockUndo;
      fUndoList.AddChange(crPaste, StartOfBlock, BlockEnd, SelText, smNormal);
{$IFDEF SYN_CLX}
{$ELSE}
    end;
{$ENDIF}
  finally
{begin}                                                                         //Fiala 2001-12-17
    if OldWrap then begin
      WordWrap := True;
      fUndoList.AddChange(crWrap, CaretXY, CaretXY, '', smNormal);
    end;
{end}                                                                           //Fiala 2001-12-17
    EndUndoBlock;                                                               //mh 2000-11-20
    fIsPasting:= False;                                                         //jcr 2002-04-13
  end;
  EnsureCursorPosVisible;
  // Selection should have changed...
  StatusChanged([scSelection]);
  DoOnPaintTransient(ttAfter);                                                  //GBN 2001-10-23
end;

{begin}                                                                         //Fiala 2001-12-17
{procedure TCustomSynEdit.SetShowSpecChar(Value: boolean);
begin
  fShowSpecChar := Value;
  Paint;
end;
}
procedure TCustomSynEdit.SetWordWrap(Value: boolean);
var
  hl: TSynCustomHighlighter;
  tmpCaret: TPoint;
  i, j: Integer;
  oldTopDif : Integer;
begin
  if fWordWrap = Value then Exit;
  fWordWrap := Value;
  { we must disable select column mode with wordwrap }
  if fWordWrap then begin
    if (eoAltSetsColumnMode in fOptions) then begin
      fIsAltSetColumnMode := True;
      Exclude(fOptions, eoAltSetsColumnMode);
    end;
    if fSelectionMode = smColumn then SelectionMode := smNormal;
  { now we enable again column mode }
  end else
    if fIsAltSetColumnMode then begin
      fIsAltSetColumnMode := False;
      Include(fOptions, eoAltSetsColumnMode);
    end;
  { we must switch off highlighter. in other case wraped line will not be highlighted }
  hl := fHighlighter;
  if hl <> nil then SetHighlighter(nil);
  { remember position }
  tmpCaret := GetCaretXY;
  oldTopDif := tmpCaret.y - fTopLine;
  { before unwraping we find new caret position }
  if not fWordWrap then begin
    if (tmpCaret.y > 1) and TSynEditStringList(fLines).IsLineWraped(tmpCaret.y - 1) then
     { find new position for CaretX }
      repeat
        Dec(tmpCaret.y);
        i := Length(TSynEditStringList(fLines).ExpandedStrings[tmpCaret.y - 1]);
        tmpCaret.x := tmpCaret.x + i;
      until (tmpCaret.y = 0) or not TSynEditStringList(fLines).IsLineWraped(tmpCaret.y - 1);
    { now we move CaretY up to new position}
    for i := tmpCaret.y - 2 downto 0 do
      if TSynEditStringList(fLines).IsLineWraped(i) then Dec(tmpCaret.y);
  end;
  { now do wraping or unwraping }
  TSynEditStringList(fLines).WordWrapWidth := fCharsInWindow;
  TSynEditStringList(fLines).WordWrap := Value;

  { after wraping we find new caret position in several steps }
  if fWordWrap then begin
    { 1. move CaretY to real line }
    i := 0;
    j := tmpCaret.y - 1;
    while (i < fLines.Count-1) and (j > 0) do begin
      if TSynEditStringList(fLines).IsLineWraped(i)
        then Inc(tmpCaret.y)
        else Dec(j);
      Inc(i);
    end;
    { 2. find new caret position }
    repeat
      i := Length(Lines[tmpCaret.y - 1]) + 1;
      if tmpCaret.x > i then begin
        tmpCaret.x := tmpCaret.x - i + 1;
        Inc(tmpCaret.y);
      end
    until tmpCaret.x <= i;
    LeftChar := 1;
  end;
  CaretXY := tmpCaret;
  TopLine := tmpCaret.y - oldTopDif;

  EnsureCursorPosVisible;
  { return back HighLighter, if was set before }
  if hl <> nil then SetHighlighter(hl);
  UpdateScrollBars;
  Invalidate;
end;
{end}                                                                           //Fiala 2001-12-17


procedure TCustomSynEdit.SelectAll;
var
  LastPt: TPoint;
begin
  LastPt := Point(1, Lines.Count);
  if LastPt.y > 0 then
    Inc(LastPt.x, Length(Lines[LastPt.y - 1]))
  else
    LastPt.y  := 1;
  SetCaretAndSelection(LastPt, Point(1, 1), LastPt);
  // Selection should have changed...
  StatusChanged([scSelection]);
end;

procedure TCustomSynEdit.SetBlockBegin(Value: TPoint);
var
  nInval1, nInval2: integer;
  SelChanged: boolean;
begin
  Value.x := MinMax(Value.x, 1, fMaxLeftChar);
  Value.y := MinMax(Value.y, 1, Lines.Count);
  if (SelectionMode = smNormal) then
    if (Value.y >= 1) and (Value.y <= Lines.Count) then
      Value.x := Min(Value.x, Length(Lines[Value.y - 1]) + 1)
    else
      Value.x := 1;
  if SelAvail then begin
    if fBlockBegin.Y < fBlockEnd.Y then begin
      nInval1 := Min(Value.Y, fBlockBegin.Y);
      nInval2 := Max(Value.Y, fBlockEnd.Y);
    end else begin
      nInval1 := Min(Value.Y, fBlockEnd.Y);
      nInval2 := Max(Value.Y, fBlockBegin.Y);
    end;
    fBlockBegin := Value;
    fBlockEnd := Value;
    InvalidateLines(nInval1, nInval2);
    SelChanged := TRUE;
  end else begin
    SelChanged := (fBlockBegin.X <> Value.X) or (fBlockBegin.Y <> Value.Y) or
                  (fBlockEnd.X <> Value.X) or (fBlockEnd.Y <> Value.Y);
    fBlockBegin := Value;
    fBlockEnd := Value;
  end;
  if SelChanged then
    StatusChanged([scSelection]);
end;

procedure TCustomSynEdit.SetBlockEnd(Value: TPoint);
var
  nLine: integer;
{$IFDEF SYN_MBCSSUPPORT}
  s: string;
{$ENDIF}
begin
  if not (eoNoSelection in Options) then begin
    Value.x := MinMax(Value.x, 1, fMaxLeftChar);
    Value.y := MinMax(Value.y, 1, Lines.Count);
    if (SelectionMode = smNormal) then
      if (Value.y >= 1) and (Value.y <= Lines.Count) then
        Value.x := Min(Value.x, Length(Lines[Value.y - 1]) + 1)
      else
        Value.x := 1;
    if (Value.X <> fBlockEnd.X) or (Value.Y <> fBlockEnd.Y) then begin
{$IFDEF SYN_MBCSSUPPORT}
      if Value.Y <= Lines.Count then begin
        s := Lines[Value.Y - 1];
        if (Length(s) >= Value.X) and (mbTrailByte = ByteType(s, Value.X)) then
          Dec(Value.X);
      end;
{$ENDIF}
      if (Value.X <> fBlockEnd.X) or (Value.Y <> fBlockEnd.Y) then begin
        if (SelectionMode = smColumn) and (Value.X <> fBlockEnd.X) then begin
          InvalidateLines(
            Min(fBlockBegin.Y, Min(fBlockEnd.Y, Value.Y)),
            Max(fBlockBegin.Y, Max(fBlockEnd.Y, Value.Y)));
          fBlockEnd := Value;
        end else begin
          nLine := fBlockEnd.Y;
          fBlockEnd := Value;
          if (SelectionMode <> smColumn) or (fBlockBegin.X <> fBlockEnd.X) then
            InvalidateLines(nLine, fBlockEnd.Y);
        end;
        StatusChanged([scSelection]);
      end;
    end;
  end;
end;

procedure TCustomSynEdit.SetCaretX(Value: Integer);
begin
  SetCaretXY(Point(Value, CaretY));
end;

procedure TCustomSynEdit.SetCaretY(Value: Integer);
begin
  SetCaretXY(Point(CaretX, Value));                                             //jr 2002-04-26
end;

function TCustomSynEdit.GetCaretXY: TPoint;
begin
  Result := Point(CaretX, CaretY);
end;
//GN 10/16/01 begin

function TCustomSynEdit.GetDisplayX: Integer;
var
 P : TPoint;
begin
  P := LogicalToPhysicalPos(CaretXY);                                           //sblbg 2001-12-17
  Result := P.X;
end;

function TCustomSynEdit.GetDisplayY: Integer;
begin
  Result := CaretY;
end;

Function TCustomSynEdit.GetDisplayXY: TPoint;
begin
  Result := Point(DisplayX, DisplayY);
end;
//GN 10/16/01 End
procedure TCustomSynEdit.SetCaretXY(Value: TPoint);                             //DDH 10/16/01 begin
begin
  SetCaretXYEx(True, Value);
end;

procedure TCustomSynEdit.UpdateLastCaretX;                                      //jr 2002-04-26
begin
{$IFDEF SYN_MBCSSUPPORT}
  fMBCSStepAside := False;
{$ENDIF}
  fLastCaretX := DisplayX;
end;

procedure TCustomSynEdit.SetCaretXYEx(CallEnsureCursorPos: Boolean; Value: TPoint);
var
  nMaxX: integer;
begin
  DoOnPaintTransient(ttBefore);                                                 //GBN 2001-10-23
  nMaxX := fMaxLeftChar;
  if Value.Y > Lines.Count then
    Value.Y := Lines.Count;
  if Value.Y < 1 then begin
    // this is just to make sure if Lines stringlist should be empty
    Value.Y := 1;
    if not (eoScrollPastEol in fOptions) then
      nMaxX := 1;
  end else begin
    if not (eoScrollPastEol in fOptions) then
      nMaxX := Length(Lines[Value.Y - 1]) + 1;
  end;
  if Value.X > nMaxX then
    Value.X := nMaxX;
  if Value.X < 1 then
    Value.X := 1;
  if (Value.X <> fCaretX) or (Value.Y <> fCaretY) then begin
    IncPaintLock;
    try
      // simply include the flags, fPaintLock is > 0
      if fCaretX <> Value.X then begin
        fCaretX := Value.X;
        Include(fStatusChanges, scCaretX);
      end;
      if fCaretY <> Value.Y then begin
        fCaretY := Value.Y;
        Include(fStatusChanges, scCaretY);
      end;
      // Call UpdateLastCaretX before DecPaintLock because the event handler it
      // calls could raise an exception, and we don't want fLastCaretX to be
      // left in an undefined state if that happens.
      UpdateLastCaretX;                                                         //jr 2002-04-26
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
    UpdateLastCaretX;                                                           //jr 2002-04-26
  end;
  DoOnPaintTransient(ttAfter);                                                  //GBN 2001-10-23
end;                                                                            //DDH 10/06/01 end

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
  with Metrics do begin
    AveCW := tmAveCharWidth;
    MaxCW := tmMaxCharWidth;
  end;
  case AveCW = MaxCW of
    True: inherited Font := Value;
    False:
      begin
        with fFontDummy do begin
          Color := Value.Color;
          Pitch := fpFixed;
          Size := Value.Size;
          Style := Value.Style;
        end;
        inherited Font := fFontDummy;
      end;
  end;
{$ENDIF}
  if fGutter.ShowLineNumbers then
    GutterChanged(Self);
end;

procedure TCustomSynEdit.SetGutterWidth(Value: Integer);
begin
  Value := Max(Value, 0);
  if fGutterWidth <> Value then begin
    fGutterWidth := Value;
    fTextOffset := fGutterWidth + 2 - (LeftChar - 1) * fCharWidth;
    if HandleAllocated then begin
      fCharsInWindow := (ClientWidth - fGutterWidth - 2) div fCharWidth;
      UpdateScrollBars;
      Invalidate;
    end;
  end;
end;

procedure TCustomSynEdit.SetLeftChar(Value: Integer);
{begin}                                                                         //mh 2000-10-19
var
  MaxVal: integer;
begin
  if eoScrollPastEol in Options then
    MaxVal := fMaxLeftChar
  else
    MaxVal := TSynEditStringList(Lines).LengthOfLongestLine;
  Value := Min(Value, MaxVal - fCharsInWindow + 1);
{end}                                                                           //mh 2000-10-19
  Value := Max(Value, 1);
  if Value <> fLeftChar then begin
    fLeftChar := Value;
    fTextOffset := fGutterWidth + 2 - (LeftChar - 1) * fCharWidth;
    UpdateScrollBars;
    InvalidateLines(-1, -1);
    StatusChanged([scLeftChar]);
  end;
end;

procedure TCustomSynEdit.SetLines(Value: TStrings);
begin
//  if HandleAllocated then                                                     //DDH 2001-12-13 per request of Flávio Etrusco
    Lines.Assign(Value);
end;

procedure TCustomSynEdit.SetLineText(Value: string);
begin
  if (CaretY >= 1) and (CaretY <= Max(1, Lines.Count)) then
    Lines[CaretY - 1] := Value;
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
{$IFDEF SYN_CLX}
{$ELSE}
    RecreateWnd;
{$ENDIF}
    UpdateScrollBars;
    Invalidate;
  end;
end;

procedure TCustomSynEdit.SetSelText(const Value: string);
begin
  if not ReadOnly then                                                          //jcr 2001-01-16
    SetSelTextPrimitive(FSelectionMode, PChar(Value), nil);
end;

// This is really a last minute change and I hope I did it right.
// Reason for this modification: next two lines will loose the CaretX position
// if eoScrollPastEol is not set in Options. That is not really a good idea
// as we would typically want the cursor to stay where it is.
// To fix this (in the absence of a better idea), I changed the code in
// DeleteSelection not to trim the string if eoScrollPastEol is not set.

procedure TCustomSynEdit.SetSelTextPrimitive(PasteMode: TSynSelectionMode;
  Value: PChar; Tag: PInteger);
begin
  SetSelTextPrimitiveEx(PasteMode, Value, Tag, True);
end;

procedure TCustomSynEdit.SetSelTextPrimitiveEx(PasteMode: TSynSelectionMode;
  Value: PChar; Tag: PInteger; AddToUndoList: Boolean);
var
  BB, BE: TPoint;
  TempString: string;

  procedure DeleteSelection;
  var
    x, MarkOffset: Integer;
    UpdateMarks: boolean;
{$IFDEF SYN_MBCSSUPPORT}
    l, r: Integer;
{$ENDIF}
  begin
    UpdateMarks := FALSE;
    MarkOffset := 0;
    case SelectionMode of
      smNormal:
        begin
          if Lines.Count > 0 then begin
              // Create a string that contains everything on the first line up
              // to the selection mark, and everything on the last line after
              // the selection mark.
            TempString := Copy(Lines[BB.Y - 1], 1, BB.X - 1) +
              Copy(Lines[BE.Y - 1], BE.X, MaxInt);
              // Delete all lines in the selection range.
            TSynEditStringList(Lines).DeleteLines(BB.Y, BE.Y - BB.Y);
              // Put the stuff that was outside of selection back in.
            if Options * [eoScrollPastEol, eoTrimTrailingSpaces]
              = [eoScrollPastEol, eoTrimTrailingSpaces]
            then
              TempString := TrimTrailingSpaces(TempString);
            Lines[BB.Y - 1] := TempString;
          end;
          UpdateMarks := TRUE;
          CaretXY := BB;
        end;
      smColumn:
        begin
            // swap X if needed
          if BB.X > BE.X then
{$IFDEF SYN_COMPILER_3_UP}
            SwapInt(BB.X, BE.X);
{$ELSE}
          begin
            x := BB.X;
            BB.X := BE.X;
            BE.X := x;
          end;
{$ENDIF}
          for x := BB.Y - 1 to BE.Y - 1 do begin
            TempString := Lines[x];
{$IFNDEF SYN_MBCSSUPPORT}
            Delete(TempString, BB.X, BE.X - BB.X);
{$ELSE}
            l := BB.X;
            r := BE.X;
            MBCSGetSelRangeInLineWhenColumnSelectionMode(TempString, l, r);
            Delete(TempString, l, r - l);
{$ENDIF}
            TrimmedSetLine(x, TempString);
          end;
            // Lines never get deleted completely, so keep caret at end.
          CaretXY := Point(BB.X, fBlockEnd.Y);
            // Column deletion never removes a line entirely, so no mark
            // updating is needed here.
        end;
      smLine:
        begin
          if BE.Y = Lines.Count then begin
            Lines[BE.Y - 1] := '';
            for x := BE.Y - 2 downto BB.Y - 1 do
              Lines.Delete(x);
          end else
            for x := BE.Y - 1 downto BB.Y - 1 do
              Lines.Delete(x);
            // smLine deletion always resets to first column.
          CaretXY := Point(1, BB.Y);
          UpdateMarks := TRUE;
          MarkOffset := 1;
        end;
    end;
    // Update marks
    if UpdateMarks then
      DoLinesDeleted(BB.Y, BE.Y - BB.Y + MarkOffset);
  end;

  procedure InsertText;

    function CountLines(p: PChar): integer;
    begin
      Result := 0;
      while p^ <> #0 do begin
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
      sLeftSide: string;
      sRightSide: string;
      Str: string;
      Start: PChar;
      P: PChar;
    begin
      Result := 0;
      sLeftSide := Copy(LineText, 1, CaretX - 1);
      if CaretX - 1 > Length(sLeftSide) then begin
        sLeftSide := sLeftSide + StringOfChar(#32,
          CaretX - 1 - Length(sLeftSide));
      end;
      sRightSide := Copy(LineText, CaretX, Length(LineText) - (CaretX - 1));
      // step1: insert the first line of Value into current line
      Start := PChar(Value);
      P := GetEOL(Start);
      if P^ <> #0 then begin
        TrimmedSetLine(CaretY - 1, sLeftSide + Copy(Value, 1, P - Start));
        TSynEditStringList(Lines).InsertLines(CaretY, CountLines(P));
      end else
        TrimmedSetLine(CaretY - 1, sLeftSide + Value + sRightSide);
      // step2: insert left lines of Value
      while P^ <> #0 do begin
        if P^ = #13 then
          Inc(P);
        if P^ = #10 then
          Inc(P);
        Inc(fCaretY);
        Start := P;
        P := GetEOL(Start);
        if P = Start then begin
          if p^ <> #0 then
            Lines[CaretY - 1] := ''
          else
            Lines[CaretY - 1] := sRightSide;
        end else begin
          SetString(Str, Start, P - Start);
          if p^ <> #0 then
            Lines[CaretY - 1] := Str
          else
            Lines[CaretY - 1] := Str + sRightSide
        end;
        if eoTrimTrailingSpaces in Options then
          Lines[CaretY - 1] := TrimTrailingSpaces(Lines[CaretY - 1]);
        Inc(Result);
      end;
      if eoTrimTrailingSpaces in Options then
        fCaretX := 1 + Length(Lines[CaretY - 1]) - Length(TrimTrailingSpaces(sRightSide))
      else fCaretX := 1 + Length(Lines[CaretY - 1]) - Length(sRightSide);
      StatusChanged([scCaretX]);
    end;

    function InsertColumn: Integer;
    var
      Str: string;
      Start: PChar;
      P: PChar;
      Len: Integer;
      InsertPos: Integer;
    begin
      // Insert string at current position
      InsertPos := CaretX;
      Start := PChar(Value);
      repeat
        P := GetEOL(Start);
        if P <> Start then begin
          SetLength(Str, P - Start);
          Move(Start^, Str[1], P - Start);
          if CaretY > Lines.Count then
            Lines.Add(StringOfChar(#32, InsertPos - 1) + Str)
          else begin
            TempString := Lines[CaretY - 1];
            Len := Length(TempString);
            if Len < InsertPos then begin
              TempString :=
                TempString + StringOfChar(#32, InsertPos - Len - 1) + Str
            end else begin
{$IFDEF SYN_MBCSSUPPORT}
              if mbTrailByte = ByteType(TempString, InsertPos) then
                Insert(Str, TempString, InsertPos + 1)
              else
{$ENDIF}
                Insert(Str, TempString, InsertPos);
            end;
            TrimmedSetLine(CaretY - 1, TempString);
          end;
          // Add undo change here from PasteFromClipboard                       //DDH 2001-11-02 from Jeff Rafter
          if AddToUndoList or fIsPasting then                                   //jcr 2002-04-13 changed to check fIsPasting
            fUndoList.AddChange(crPaste, Point(InsertPos, CaretY),
               Point(InsertPos+(P-Start), CaretY), '',  SelectionMode);         //jcr 2002-04-13 changed smNormal to SelectionMode
        end;
        if Tag <> nil then
          Tag^ := P - Start;
        if P^ = #13 then begin
          Inc(P);
          if P^ = #10 then
            Inc(P);
          Inc(fCaretY);
        end;
        Start := P;
      until P^ = #0;
      Inc(fCaretX, Length(Str));
      Result := 0;
    end;

    function InsertLine: Integer;
    var
      Start: PChar;
      P: PChar;
      Str: string;
      n: Integer;
    begin
      Result := 0;
      fCaretX := 1;
      // Insert string before current line
      Start := PChar(Value);
      repeat
        P := GetEOL(Start);
        if P <> Start then begin
          SetLength(Str, P - Start);
          Move(Start^, Str[1], P - Start);
        end else
          Str := '';
        if (P^ = #0) then begin
          n := Lines.Count;
          if (n >= CaretY) then
            Lines[CaretY - 1] := Str + Lines[CaretY - 1]
          else
            Lines.Add(Str);
          if eoTrimTrailingSpaces in Options then
            Lines[CaretY - 1] := TrimTrailingSpaces(Lines[CaretY - 1]);
          fCaretX := 1 + Length(Str);
        end else begin
          TrimmedSetLine(CaretY - 1, Str);
          Inc(fCaretY);
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
    InsertedLines: Integer;
  begin
    if Value = '' then
      Exit;

    // Using a TStringList to do this would be easier, but if we're dealing
    // with a large block of text, it would be very inefficient.  Consider:
    // Assign Value parameter to TStringList.Text: that parses through it and
    // creates a copy of the string for each line it finds.  That copy is passed
    // to the Add method, which in turn creates a copy.  Then, when you actually
    // use an item in the list, that creates a copy to return to you.  That's
    // 3 copies of every string vs. our one copy below.  I'd prefer no copies,
    // but we aren't set up to work with PChars that well.

    StartLine := CaretY;
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
      DoLinesInserted(StartLine, InsertedLines);
    // Force caret reset
    CaretXY := CaretXY;
  end;

begin
  IncPaintLock;
  Lines.BeginUpdate;
  try
    BB := BlockBegin;
    BE := BlockEnd;
    if SelAvail then                                                            //DDH 10/16/01 (This was changed per the email of Scott Mattes)
    begin                                                                       //
      DeleteSelection;                                                          //
      CaretXY := BB;                                                            //!! Modified to restore caret pos
    end;                                                                        //
    if (Value <> nil) and (Value[0] <> #0) then
      InsertText;
    if CaretY < 1 then
      CaretY := 1;
  finally
    Lines.EndUpdate;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.SetText(const Value: string);
begin
  Lines.Text := Value;
end;

procedure TCustomSynEdit.SetTopLine(Value: Integer);
var
  Delta: Integer;
{$IFDEF SYN_CLX}
  src,dest : TRect;
  invalid  : TRect;
{$ENDIF}

begin
  // don't use MinMax here, it will fail in design mode (Lines.Count is zero,
  // but the painting code relies on TopLine >= 1)
  if (eoScrollPastEof in Options) then
    Value := Min(Value, Lines.Count)
  else
    Value := Min(Value, Lines.Count + 1 - fLinesInWindow);
  Value := Max(Value, 1);
  if Value <> TopLine then begin
    Delta := TopLine - Value;
    fTopLine := Value;
    UpdateScrollBars;
{$IFDEF SYN_CLX}
{TODO: Look at making scrolling work a bit better}
{
    if Abs(Delta) < fLinesInWindow then
      begin
        src := ClientRect;
        dest := src;
        invalid := src;

        Delta := fTextHeight * Delta;

        if Delta<0 then
          begin
            src.Top := src.Top + Delta;
            dest.Bottom := dest.Bottom - Delta;
            invalid.Top := dest.Bottom + 1;
          end
        else
          begin
            src.Bottom := src.Bottom - Delta;
            dest.Top := dest.Top + Delta;
            invalid.Bottom := dest.Top - 1;
          end;

        if sfCaretVisible in fStateFlags then
          kTextDrawer.HideCaret(self);

        try
          Canvas.CopyRect(dest, Canvas, src);
          InvalidateRect(invalid, FALSE);
        finally
          if sfCaretVisible in fStateFlags then
            kTextDrawer.ShowCaret(self);
        end;
      end
    else
}
      Invalidate;
{$ELSE}
    if Abs(Delta) < fLinesInWindow then
      ScrollWindow(Handle, 0, fTextHeight * Delta, nil, nil)
    else
      Invalidate;
{$ENDIF}
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
{$IFDEF SYN_MBCSSUPPORT}
  cf: TCompositionForm;
{$ENDIF}
begin
  if (PaintLock <> 0) or (not(Focused) and not(FAlwaysShowCaret)) then          //DDH 10/16/01 for AlwaysShowCaret
    Include(fStateFlags, sfCaretChanged)
  else begin
    Exclude(fStateFlags, sfCaretChanged);
    CX := CaretXPix + FCaretOffset.X;
    CY := CaretYPix + FCaretOffset.Y + 1;
    if (CX >= fGutterWidth) and (CX < ClientWidth)
      and (CY >= 0) and (CY < ClientHeight)
    then begin
      SetCaretPos(CX, CY);
      ShowCaret;
    end else begin
      HideCaret;
      SetCaretPos(CX, CY);
    end;
{$IFDEF SYN_MBCSSUPPORT}
    cf.dwStyle := CFS_POINT;
    cf.ptCurrentPos := Point(CX, CY);
    ImmSetCompositionWindow(ImmGetContext(Handle), @cf);
{$ENDIF}
  end;
end;

procedure TCustomSynEdit.UpdateScrollBars;
var
  nMaxScroll: integer;
{$IFNDEF SYN_CLX}
  ScrollInfo: TScrollInfo;
  LeftBuffer: Integer;
{$ELSE}

  procedure CalcScrollbarsVisible;
  begin
    if not HandleAllocated or (PaintLock <> 0) then
      Include(fStateFlags, sfScrollbarChanged)
    else begin
      Exclude(fStateFlags, sfScrollbarChanged);
      if fScrollBars <> ssNone then begin
        if fScrollBars in [ssBoth, ssHorizontal] then begin
  {begin}                                                                         //mh 2000-10-19
          if eoScrollPastEol in Options then
            FHScrollBar.Max := fMaxLeftChar
          else
            FHScrollBar.Max := TSynEditStringList(Lines).LengthOfLongestLine;
  {end}
          FHScrollBar.Position := LeftChar;

          if eoHideShowScrollbars in Options then
            FHScrollBar.Visible := TSynEditStringList(Lines).LengthOfLongestLine > fCharsInWindow
          else FHScrollBar.Visible := TRUE;

          FHScrollBar.LargeChange := CharsInWindow;
        end
        else
          FHScrollBar.Visible := FALSE;

        if fScrollBars in [ssBoth, ssVertical] then begin
          nMaxScroll := Lines.Count;
          if (eoScrollPastEof in Options) then
            Inc(nMaxScroll, LinesInWindow - 1);
          if nMaxScroll <= MAX_SCROLL then begin
            FVScrollBar.Max := Max(1, nMaxScroll);
            FVScrollBar.LargeChange := LinesInWindow;
            FVScrollBar.Position := TopLine;
          end else begin
            FVScrollBar.Min := 0;
            FVScrollBar.Max := MAX_SCROLL;

            FVScrollBar.LargeChange := MulDiv(MAX_SCROLL, LinesInWindow, nMaxScroll);
            FVScrollBar.Position := MulDiv(MAX_SCROLL, TopLine, nMaxScroll);
          end;
          if eoHideShowScrollbars in Options then
          begin
            FVScrollBar.Visible := Lines.Count > LinesInWindow;
          end else FVScrollBar.Visible := TRUE;
        end
        else
        FVScrollBar.Visible:=FALSE;
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

      ScrollInfo.nMin := 1;
      ScrollInfo.nTrackPos := 1; //this actually gets ignored

      if fScrollBars in [ssBoth, ssHorizontal] then begin
{begin}                                                                         //mh 2000-10-19
//        ScrollInfo.nMax := fMaxLeftChar;
        if not(eoDisableScrollArrows in Options) then
          LeftBuffer := 0
        else LeftBuffer := LeftChar;

        if eoScrollPastEol in Options then
          ScrollInfo.nMax := fMaxLeftChar
        else
          ScrollInfo.nMax := Max(TSynEditStringList(Lines).LengthOfLongestLine, ScrollInfo.nMin);
{end}                                                                           //mh 2000-10-19
        ScrollInfo.nPage := CharsInWindow;
        ScrollInfo.nPos := LeftChar;

        if (ScrollInfo.nMax = ScrollInfo.nMin) and
           (eoHideShowScrollbars in Options) then
        begin
          ScrollInfo.nMax := ScrollInfo.nMax + 1;
          ScrollInfo.nPos := ScrollInfo.nMin;
        end;

        SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);

        //Now for the aarows
        if (eoDisableScrollArrows in Options) or (LeftBuffer + CharsInWindow >= ScrollInfo.nMax) then
        begin
          if (LeftChar <= 1) and (LeftBuffer + CharsInWindow >= ScrollInfo.nMax) then
          begin
            EnableScrollBar(Handle, SB_HORZ, ESB_DISABLE_BOTH);
          end else begin
            EnableScrollBar(Handle, SB_HORZ, ESB_ENABLE_BOTH);
            if (LeftChar <= 1) then
              EnableScrollBar(Handle, SB_HORZ, ESB_DISABLE_LEFT)
            else if ((LeftBuffer + CharsInWindow) > ScrollInfo.nMax) then
              EnableScrollBar(Handle, SB_HORZ, ESB_DISABLE_RIGHT);
          end;
        end else EnableScrollBar(Handle, SB_HORZ, ESB_ENABLE_BOTH);
      end;

      if fScrollBars in [ssBoth, ssVertical] then begin
        nMaxScroll := Lines.Count;
        if (eoScrollPastEof in Options) then
          Inc(nMaxScroll, LinesInWindow - 1);
        if nMaxScroll <= MAX_SCROLL then begin
          ScrollInfo.nMax := Max(1, nMaxScroll);
          ScrollInfo.nPage := LinesInWindow;
          ScrollInfo.nPos := TopLine;
        end else begin
          ScrollInfo.nMin := 0;
          ScrollInfo.nMax := MAX_SCROLL;
          ScrollInfo.nPage := MulDiv(MAX_SCROLL, LinesInWindow, nMaxScroll);
          ScrollInfo.nPos := MulDiv(MAX_SCROLL, TopLine, nMaxScroll);
        end;

        if (ScrollInfo.nMax = ScrollInfo.nMin) and
           (eoHideShowScrollbars in Options) then
        begin
          ScrollInfo.nMax := ScrollInfo.nMax + 1;
          ScrollInfo.nPos := ScrollInfo.nMin;
        end;

        SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);

        if (eoDisableScrollArrows in Options) or (Lines.Count <= LinesInWindow) then
        begin
          if (TopLine <= 1) and (Lines.Count <= LinesInWindow) then
          begin
            EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_BOTH);
          end else begin
            EnableScrollBar(Handle, SB_VERT, ESB_ENABLE_BOTH);
            if (TopLine <= 1) then
              EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_UP)
            else if ((Lines.Count - TopLine - LinesInWindow + 1) = 0) then
              EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_DOWN);
          end;
        end else EnableScrollBar(Handle, SB_VERT, ESB_ENABLE_BOTH);
      end;


    end;
  end;
{$ELSE}
  if FHScrollBar<>nil then
    begin
//      FHScrollBar.Visible := False;
//      FVScrollBar.Visible := False;
      CalcScrollBarsVisible;
      if FHScrollBar.Visible xor FVScrollBar.Visible then
        CalcScrollBarsVisible;

{
      if eoHideShowScrollbars in Options then
      begin
        CanShow := TSynEditStringList(Lines).LengthOfLongestLine >= CharsInWindow;
      end
      else
        CanShow := True;
}
      FHScrollBar.Top := Height - FHScrollBar.Height;
      FHScrollBar.Left := 0;
      FHScrollBar.Width := Width;
      if FVScrollBar.Visible then
        FHScrollBar.Width := FHScrollBar.Width - FVScrollBar.Width;
      FVScrollBar.Top := 0;
      FVScrollBar.Left := Width - FVScrollBar.Width;
      FVScrollBar.Height := Height;
      if FHScrollBar.Visible then
        FVScrollBar.Height := FVScrollBar.Height - FHScrollBar.Height;

    end;
{$ENDIF}
end;

{$IFDEF SYN_CLX}
procedure TCustomSynEdit.ModifyScrollBar(ScrollBar: TScrollBarKind; ScrollCode: TScrollCode; Pos: Cardinal;
  UseRightToLeft: Boolean);
var
  NewTopLeft, MaxTopLeft: TPoint;
  RTLFactor: Integer;

  function Min: Longint;
  begin
    result := 0;
  end;

  function Max: Longint;
  begin
    if ScrollBar = sbHorizontal then Result := MaxTopLeft.X
    else Result := MaxTopLeft.Y;
  end;

  function PageUp: Longint;
  begin
    if ScrollBar = sbHorizontal then
      Result := LeftChar-CharsInWindow else
      Result := TopLine-LinesInWindow;
    if Result < 0 then Result := 0;
  end;

  function PageDown: Longint;
  begin
    if ScrollBar = sbHorizontal then
      Result := LeftChar+CharsInWindow else
      Result := TopLine+LinesInWindow;
    if Result < 0 then Result := 0;
  end;

  function CalcScrollBar(Value, ARTLFactor: Longint): Longint;
  begin
    Result := Value;
    case ScrollCode of
      scLineUp:
        Dec(Result, ARTLFactor);
      scLineDown:
        Inc(Result, ARTLFactor);
      scPageUp:
        Dec(Result, PageUp * ARTLFactor);
      scPageDown:
        Inc(Result, PageDown * ARTLFactor);
      scPosition, scTrack:
        if (ScrollCode = scPosition) then
        begin
          if (ARTLFactor = 1) then
            Result := Min + Int64(Pos) * (Max - Min) div Int64(MaxShortInt)
          else
            Result := Max - Int64(Pos) * (Max - Min) div Int64(MaxShortInt);
        end;
      scBottom:
        Result := Max;
      scTop:
        Result := Min;
    end;
  end;

var
  Temp: Longint;
begin
  RTLFactor := 1;
  if Visible and CanFocus and TabStop and not (csDesigning in ComponentState) then
    SetFocus;

  if (ScrollBar = sbHorizontal) then
  begin
//    ModifyPixelScrollBar(ScrollCode, Pos);
//    Exit;
  end;

  if MaxLeftChar>0 then
    MaxTopLeft.X := MaxLeftChar - 1
  else
    MaxTopLeft.X := MaxInt-1;

  MaxTopLeft.Y := Lines.Count - 1;

  NewTopLeft.X := LeftChar;
  NewTopLeft.Y := TopLine;

  if ScrollBar = sbHorizontal then
    NewTopLeft.X := CalcScrollBar(NewTopLeft.X, RTLFactor)
  else
    NewTopLeft.Y := CalcScrollBar(NewTopLeft.Y, 1);

  NewTopLeft.X := Math.Min(MaxTopLeft.X, NewTopLeft.X);
  NewTopLeft.Y := Math.Min(MaxTopLeft.Y, NewTopLeft.Y);
  if (NewTopLeft.X <>  LeftChar) or (NewTopLeft.Y <> TopLine) then
    begin
      LeftChar := NewTopLeft.X;
      TopLine := NewTopLeft.Y;
    end;
end;


procedure TCustomSynEdit.ScrollEvent(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if Sender = FHScrollBar then
    ModifyScrollBar(sbHorizontal, ScrollCode, ScrollPos, True);
  if Sender = FVScrollBar then
    ModifyScrollBar(sbVertical, ScrollCode, ScrollPos, True);
end;

function TCustomSynEdit.GetClientRect: TRect;
begin
  Result := Inherited GetClientRect;

  if fScrollBars in [ssBoth, ssHorizontal] then
    Result.Bottom := Result.Bottom - CYHSCROLL;
  if fScrollBars in [ssBoth, ssVertical] then
    Result.Right := Result.Right - CXVSCROLL;
end;

procedure TCustomSynEdit.Resize;
begin
  inherited Resize;

  SizeOrFontChanged(FALSE);
end;

{$ENDIF}

{$IFDEF SYN_CLX}
{$ELSE}
{begin}                                                                         //jr 2001-01-06
procedure TCustomSynEdit.WMCaptureChanged(var Msg: TMessage);
begin
  fScrollTimer.Enabled := False;
  inherited;
end;
{end}                                                                           //jr 2001-01-06

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
  i, iNumberDropped: integer;
  szPathName: array[0..260] of char;
  Point: TPoint;
  FilesList: TStringList;
begin
  try
    if Assigned(fOnDropFiles) then begin
      FilesList := TStringList.Create;
      try
        iNumberDropped := DragQueryFile(THandle(Msg.wParam), Cardinal(-1),
          nil, 0);
        DragQueryPoint(THandle(Msg.wParam), Point);

        for i := 0 to iNumberDropped - 1 do begin
          DragQueryFile(THandle(Msg.wParam), i, szPathName,
            SizeOf(szPathName));
          FilesList.Add(szPathName);
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

procedure TCustomSynEdit.WMEraseBkgnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TCustomSynEdit.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
  if fWantTabs and (GetKeyState(VK_CONTROL) >= 0) then
    Msg.Result := Msg.Result or DLGC_WANTTAB;
end;

procedure TCustomSynEdit.WMHScroll(var Msg: TWMScroll);
begin
  case Msg.ScrollCode of
      // Scrolls to start / end of the line
    SB_TOP: LeftChar := 1;
    SB_BOTTOM: LeftChar := MaxLeftChar;
      // Scrolls one char left / right
    SB_LINEDOWN: LeftChar := LeftChar + 1;
    SB_LINEUP: LeftChar := LeftChar - 1;
      // Scrolls one page of chars left / right
    SB_PAGEDOWN: LeftChar := LeftChar
      + (fCharsInWindow - Ord(eoScrollByOneLess in fOptions));
    SB_PAGEUP: LeftChar := LeftChar
      - (fCharsInWindow - Ord(eoScrollByOneLess in fOptions));
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK: LeftChar := Msg.Pos;
  end;
end;

procedure TCustomSynEdit.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  CommandProcessor( ecLostFocus, #0, nil );
  //GBN 2002-03-23
  //Added check for focused to prevent caret disappearing problem
  if FAlwaysShowCaret or Focused then exit;                                                //DDH 10/16/01
  HideCaret;
  Windows.DestroyCaret;
  if FHideSelection and SelAvail then
    Invalidate;
end;

procedure TCustomSynEdit.WMPaste(var Message: TMessage);
begin
  if not ReadOnly then
    PasteFromClipboard;
  Message.Result := ord(True);
end;

procedure TCustomSynEdit.WMCancelMode(var Message:TMessage);                    //ddh 2001-12-13
begin

end;

procedure TCustomSynEdit.WMSetFocus(var Msg: TWMSetFocus);
begin
  CommandProcessor( ecGotFocus, #0, nil );

  InitializeCaret;
  if FHideSelection and SelAvail then
    Invalidate;
end;

procedure TCustomSynEdit.WMSize(var Msg: TWMSize);
begin
  inherited;
  SizeOrFontChanged(FALSE);
//  SetLeftChar(LeftChar);                                                      //mh 2000-10-19
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
  s: ShortString;
  rc: TRect;
  pt: TPoint;
  ScrollHint: THintWindow;
  ButtonH: Integer;                                                             //DDH 10/16/01
  ScrollInfo: TScrollInfo;                                                      //DDH 10/16/01
begin
  case Msg.ScrollCode of
      // Scrolls to start / end of the text
    SB_TOP: TopLine := 1;
    SB_BOTTOM: TopLine := Lines.Count;
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
        if Lines.Count > MAX_SCROLL then
          TopLine := MulDiv(LinesInWindow + Lines.Count - 1, Msg.Pos,
            MAX_SCROLL)
        else
          TopLine := Msg.Pos;

        if eoShowScrollHint in fOptions then begin
          ScrollHint := GetScrollHint;
          //DDH 10/16/01 Start
          ScrollHint.Color := fScrollHintColor;
          case FScrollHintFormat of
            shfTopLineOnly : s := Format(SYNS_ScrollInfoFmtTop, [TopLine]);
            else s := Format(SYNS_ScrollInfoFmt, [TopLine,
                      TopLine + Min(LinesInWindow, Lines.Count - TopLine)]);
          end;

//          ScrollHint.Color := Application.HintColor;
//          s := Format(SYNS_ScrollInfoFmt, [TopLine]);
          //DDH 10/16/01 End
{$IFDEF SYN_COMPILER_3_UP}
          rc := ScrollHint.CalcHintRect(200, s, nil);
{$ELSE}
          rc := Rect(0, 0, ScrollHint.Canvas.TextWidth(s) + 6,
            ScrollHint.Canvas.TextHeight(s) + 4);
{$ENDIF}
//DDH 10/16/01 Start
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
//DDH 10/16/01 End

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
      if eoShowScrollHint in fOptions then
        ShowWindow(GetScrollHint.Handle, SW_HIDE);
  end;
  Update;
end;
{$ENDIF}

function TCustomSynEdit.ScanFrom(Index: integer): integer;
begin
  Result := Index;
  if Index >= Lines.Count - 1 then Exit;
  fHighlighter.SetLine(Lines[Result], Result);
  inc(Result);
  fHighlighter.NextToEol;
{begin}                                                                         //mh 2000-10-10
//  while fHighlighter.GetRange <> fLines.Objects[Result] do begin
  while fHighlighter.GetRange <> TSynEditStringList(Lines).Ranges[Result] do
  begin
//    Lines.Objects[Result] := fHighlighter.GetRange;
    TSynEditStringList(Lines).Ranges[Result] := fHighlighter.GetRange;
{end}                                                                           //mh 2000-10-10
    fHighlighter.SetLine(Lines[Result], Result);
    fHighlighter.NextToEol;
    inc(Result);
    if Result = Lines.Count then
      break;
  end;
  Dec(Result);
end;

{begin}                                                                         //mh 2000-10-10
(*
procedure TCustomSynEdit.ListAdded(Sender: TObject);
var
  LastIndex: Integer;
begin
  if Assigned(fHighlighter) then begin
    if Lines.Count > 1 then begin
      LastIndex := Lines.Count - 1;
      fHighlighter.SetRange(Lines.Objects[LastIndex - 1]);
      fHighlighter.SetLine(Lines[LastIndex - 1], LastIndex - 1);
      fHighlighter.NextToEol;
      Lines.Objects[LastIndex] := fHighlighter.GetRange;
    end else begin
      fHighlighter.ReSetRange;
      Lines.Objects[0] := fHighlighter.GetRange;
    end;
  end;
  LastIndex := Lines.Count;
  InvalidateLine(LastIndex);
*)
procedure TCustomSynEdit.ListAdded(Index: integer; const S: String);
begin
  if Assigned(fHighlighter) then begin
    if (Index > 0) then begin
      fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[Index - 1]);
      ScanFrom(Index - 1);
    end else begin
      fHighlighter.ResetRange;
      TSynEditStringList(Lines).Ranges[0] := fHighlighter.GetRange;
      if (Lines.Count > 1) then
        ScanFrom(0);
    end;
  end;
  InvalidateLines(Index + 1, MaxInt);
  InvalidateGutterLines(Index + 1, MaxInt);

  if (eoAutoSizeMaxLeftChar in fOptions) then
  begin
    if ((Length(S) + 1) > fMaxLeftChar) then
      MaxLeftChar := Length(S) + 1;
  end;
end;
{end}                                                                           //mh 2000-10-10

procedure TCustomSynEdit.ListCleared(Sender: TObject);
begin
  ClearUndo;
  // invalidate the *whole* client area
  FillChar(fInvalidateRect, SizeOf(TRect), 0);
  Invalidate;
  // set caret and selected block to start of text
  SetBlockBegin(Point(1, 1));
  SetCaretXY(Point(1, 1));
  // scroll to start of text
  TopLine := 1;
  LeftChar := 1;
  Include(fStatusChanges, scAll);
end;

procedure TCustomSynEdit.ListDeleted(Index: Integer);
begin
  if Assigned(fHighlighter) and (Lines.Count >= 1) then
    if (Index > 0) then begin
{begin}                                                                         //mh 2000-10-10
//      fHighlighter.SetRange(Lines.Objects[Index - 1]);
      fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[Index - 1]);
      ScanFrom(Index - 1);
    end else begin
      fHighlighter.ResetRange;
//      Lines.Objects[0] := fHighlighter.GetRange;
      TSynEditStringList(Lines).Ranges[0] := fHighlighter.GetRange;
{end}                                                                           //mh 2000-10-10
      if (Lines.Count > 1) then
        ScanFrom(0);
    end;
  InvalidateLines(Index + 1, MaxInt);
  InvalidateGutterLines(Index + 1, MaxInt);
end;

procedure TCustomSynEdit.ListInserted(Index: Integer; const S: String);
begin
  if Assigned(fHighlighter) and (Lines.Count >= 1) then
    if (Index > 0) then begin
{begin}                                                                         //mh 2000-10-10
//      fHighlighter.SetRange(Lines.Objects[Index - 1]);
      fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[Index - 1]);
      ScanFrom(Index - 1);
    end else begin
      fHighlighter.ReSetRange;
//      Lines.Objects[0] := fHighlighter.GetRange;
      TSynEditStringList(Lines).Ranges[0] := fHighlighter.GetRange;
{end}                                                                           //mh 2000-10-10
      if (Lines.Count > 1) then
        ScanFrom(0);
    end;
  InvalidateLines(Index + 1, TopLine + LinesInWindow);
  InvalidateGutterLines(Index + 1, TopLine + LinesInWindow);

  if (eoAutoSizeMaxLeftChar in fOptions) then
  begin
    if ((Length(S) + 1) > fMaxLeftChar) then
      MaxLeftChar := Length(S) + 1;
  end;
end;

procedure TCustomSynEdit.ListPutted(Index: Integer; const S: String);
begin
  if Assigned(fHighlighter) then
  begin
//    fHighlighter.SetRange(Lines.Objects[Index]);
    fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[Index]);             //mh 2000-10-10
    InvalidateLines(Index + 1, ScanFrom(Index) + 1);
  end
  else
    InvalidateLines(Index + 1, Index + 1);

  if (eoAutoSizeMaxLeftChar in fOptions) then
  begin
    if ((Length(S) + 1) >= fMaxLeftChar) then
      MaxLeftChar := Length(S) + 2;
  end;
end;

procedure TCustomSynEdit.ListScanRanges(Sender: TObject);
var
  i: integer;
begin
  if Assigned(fHighlighter) and (Lines.Count > 0) then begin
    fHighlighter.ResetRange;
{begin}                                                                         //mh 2000-10-10
(*
    Lines.Objects[0] := fHighlighter.GetRange;
    i := 1;
    while (i < Lines.Count) do begin
      fHighlighter.SetRange(Lines.Objects[i - 1]);
      fHighlighter.SetLine(Lines[i - 1], i - 1);
      fHighlighter.NextToEol;
      Lines.Objects[i] := fHighlighter.GetRange;
      Inc(i);
    end;
*)
    i := 0;
    repeat
      TSynEditStringList(Lines).Ranges[i] := fHighlighter.GetRange;
      fHighlighter.SetLine(Lines[i], i);
      fHighlighter.NextToEol;
      Inc(i);
    until i >= Lines.Count;
{end}                                                                           //mh 2000-10-10
  end;
end;

{$IFDEF SYN_MBCSSUPPORT}
type
  TStringType = (stNone, stHalfNumAlpha, stHalfSymbol, stHalfKatakana,
    stWideNumAlpha, stWideSymbol, stWideKatakana, stHiragana, stIdeograph,
    stControl, stKashida);

{  }

function IsStringType(Value: Word): TStringType;
begin
  Result := stNone;

  if (Value = C3_SYMBOL) then begin
    (***  Controls  ***)
    Result := stControl;
  end else
    if ((Value and C3_HALFWIDTH) <> 0) then begin
    (*** singlebyte ***)
      if (Value = C3_HALFWIDTH) or
        (Value = (C3_ALPHA or C3_HALFWIDTH)) then begin { Number & Alphabet }
        Result := stHalfNumAlpha;
      end else
        if ((Value and C3_SYMBOL) <> 0) or
          ((Value and C3_LEXICAL) <> 0) then begin { Symbol }
          Result := stHalfSymbol;
        end else
          if ((Value and C3_KATAKANA) <> 0) then begin { Japanese-KATAKANA }
            Result := stHalfKatakana;
          end;
    end else begin
    (*** doublebyte ***)
      if (Value = C3_FULLWIDTH) or
        (Value = (C3_ALPHA or C3_FULLWIDTH)) then begin { Number & Alphabet }
        Result := stWideNumAlpha;
      end
      else if ((Value and C3_SYMBOL) <> 0) or
          ((Value and C3_LEXICAL) <> 0) then begin { Symbol }
        Result := stWideSymbol;
      end
      else if ((Value and C3_KATAKANA) <> 0) then begin { Japanese-KATAKANA }
        Result := stWideKatakana;
      end
      else if ((Value and C3_HIRAGANA) <> 0) then begin { Japanese-HIRAGANA }
        Result := stHiragana;
      end
      else if ((Value and C3_IDEOGRAPH) <> 0) then begin { Ideograph }
        Result := stIdeograph;
      end;
    end;
end;

{  }

procedure TCustomSynEdit.SetWordBlock(Value: TPoint);
var
  i: Integer;
  Runner: TPoint;
  TempString: string;
  IdChars: TSynIdentChars;

  procedure MultiBlockScan;
  var
    i: Integer;
    wideX: Integer;
    cType: PWordArray;
    cLeng: Integer;
    stc: TStringType;
  begin
    wideX := ByteToCharIndex(TempString, Value.X - 1);

    cLeng := ByteToCharLen(TempString, Length(TempString));
    GetMem(cType, SizeOf(Word) * cLeng);
    try
      if not GetStringTypeEx(LOCALE_SYSTEM_DEFAULT, CT_CTYPE3,
        PChar(TempString), Length(TempString), cType^)
      then
        exit;
      stc := IsStringType(cType^[wideX]);
      if (stc = stControl) then
        exit;
      { search BlockEnd }
{begin}                                                                         //Fiala 2001-12-17
      for i := wideX to cLeng do
        if not IsCharAlphaNumeric(TempString[i]) then begin
//      for i := wideX + 1 to cLeng - 1 do
//        if (IsStringType(cType^[i]) <> stc) then begin
//          Runner.Y := (i + 1);
          Runner.Y := i;
{end}                                                                           //Fiala 2001-12-17
          Break;
        end;
//      Runner.Y := (i + 1);                                                    //Fiala 2001-12-17
      if Runner.Y > cLeng then Runner.Y := cLeng;
      { search BlockBegin }
      for i := wideX downto 0 do
        if not IsCharAlphaNumeric(TempString[i]) then begin
{begin}                                                                         //Fiala 2001-12-17
//      for i := wideX - 1 downto 0 do
//        if (IsStringType(cType^[i]) <> stc) then begin
//          Runner.X := (i + 2);
          Runner.X := i + 1;
{end}                                                                           //Fiala 2001-12-17
          Break;
        end;
      Runner.X := CharToByteIndex(TempString, Runner.X);
      Runner.Y := CharToByteIndex(TempString, Runner.Y);
    finally
      FreeMem(cType);
    end;
  end;

begin
  Value.x := MinMax(Value.x, 1, fMaxLeftChar);
  Value.y := MinMax(Value.y, 1, Lines.Count);
  TempString := (Lines[Value.Y - 1] + #$0);
  if (Value.X >= Length(TempString)) then begin
    CaretXY := Point(Length(TempString), Value.Y);
    exit;
  end;
  if (fHighlighter <> nil) and
    (ByteType(TempString, Value.X) <> mbLeadByte) then begin
    Runner := Point(0, Length(TempString));
    IdChars := fHighlighter.IdentChars;
    { search BlockEnd }
    for i := Value.X to Length(TempString) - 1 do begin
      if not (TempString[i] in IdChars) then begin
        Runner.Y := i;
        Break;
      end;
    end;
    { search BlockBegin }
    for i := Value.X - 1 downto 1 do begin
      if not (TempString[i] in IdChars) then begin
        Runner.X := (i + 1);
        Break;
      end;
    end;
  end else
    MultiBlockScan;
  SetCaretAndSelection(Point(Runner.Y, Value.Y), Point(Runner.X, Value.Y),
    Point(Runner.Y, Value.Y));
  InvalidateLine(Value.Y);
  StatusChanged([scSelection]);
end;

{$ELSE}

procedure TCustomSynEdit.SetWordBlock(Value: TPoint);
var
  Runner: TPoint;
  TempString: string;
  IdChars: TSynIdentChars;
begin
  Value.x := MinMax(Value.x, 1, fMaxLeftChar);
  Value.y := MinMax(Value.y, 1, Lines.Count);
  TempString := Lines[Value.Y - 1];
  if TempString = '' then exit;
  // Click on right side of text
  if Length(TempString) < Value.X then Value.X := Length(TempString);
  Runner := Value;
  if fHighlighter <> nil then
    IdChars := fHighlighter.IdentChars
  else
    IDchars := [#33..#255];
  if not (TempString[Runner.X] in IdChars) then begin
    // no word under cursor and next char right is not start of a word
    if (Runner.X > 1) and (not (TempString[Runner.X] in IdChars)) then begin
      // find end of word on the left side
      while Runner.X > 0 do begin
        if (TempString[Runner.X] in IdChars) then break;
        Dec(Runner.X);
      end;
    end;
    // no word on the left side, so look to the right side
    if not (TempString[Runner.X] in IdChars) then begin
      Runner := Value;
      while Runner.X < fMaxLeftChar do begin
        if (TempString[Runner.X] in IdChars) then break;
        Inc(Runner.X);
      end;
      if Runner.X > fMaxLeftChar then
        exit;
    end;
    Value := Runner;
  end;
  while Runner.X > 0 do begin
    if not (TempString[Runner.X] in IdChars) then break;
    Dec(Runner.X);
  end;
  Inc(Runner.X);
  if Runner.X < 1 then Runner.X := 1;
  fBlockBegin := Runner;
  Runner := Value;
  while (Runner.X < fMaxLeftChar) and                                           //DDH 10/19/01
        (Runner.X<=Length(TempString)) do
  begin
    if not (TempString[Runner.X] in IdChars) then break;
    Inc(Runner.X);
  end;
  if Runner.X > fMaxLeftChar then Runner.X := fMaxLeftChar;
  fBlockEnd := Runner;
// set caret to the end of selected block
  CaretXY := Runner;
  InvalidateLine(Value.Y);
  StatusChanged([scSelection]);
end;
{$ENDIF}

procedure TCustomSynEdit.DblClick;
var
  ptMouse: TPoint;
begin
  GetCursorPos(ptMouse);
  ptMouse := ScreenToClient(ptMouse);
  if ptMouse.X >= fGutterWidth + 2 then begin
    if not (eoNoSelection in fOptions) then
      SetWordBlock(CaretXY);
    inherited;
    Include(fStateFlags, sfDblClicked);
    MouseCapture := FALSE;
  end else
    inherited;
end;

function TCustomSynEdit.GetCanUndo: Boolean;
begin
  result := not ReadOnly and fUndoList.CanUndo;                                 //jcr 2001-01-16
end;

function TCustomSynEdit.GetCanRedo: Boolean;
begin
  result := not ReadOnly and fRedoList.CanUndo;                                 //jcr 2001-01-16
end;

function TCustomSynEdit.GetCanPaste;
begin
{$IFDEF SYN_CLX}
  Result := not ReadOnly and Clipboard.Provides(CF_TEXT);
{$ELSE}
  Result := not ReadOnly and Clipboard.HasFormat(CF_TEXT)                       //jcr 2001-01-16
    or Clipboard.HasFormat(SynEditClipboardFormat)
{$ENDIF}
end;

procedure TCustomSynEdit.InsertBlock(BB, BE: TPoint; ChangeStr: PChar);
begin
  InsertBlockEx(BB, BE, ChangeStr, True);
end;

procedure TCustomSynEdit.InsertBlockEx(BB, BE: TPoint; ChangeStr: PChar; AddToUndoList: Boolean);
// used by BlockIndent and Redo
begin
  SetCaretAndSelection(BB, BB, BE);
  fSelectionMode := smColumn;
  SetSelTextPrimitiveEx(smColumn, ChangeStr, nil, AddToUndoList);
  StatusChanged([scSelection]);
end;

procedure TCustomSynEdit.Redo;
{begin}                                                                         //sbs 2000-11-19
var
  Item: TSynEditUndoItem;
  OldChangeNumber: integer;
  SaveChangeNumber: integer;
  FLastChange : TSynChangeReason;                                               //DDH 10/16/01 for GroupUndo
  FAutoComplete: Boolean;                                                       //DDH 10/16/01 for AutoComplete
  FSpecial1: Boolean;                                                           //DDH 10/16/01 for Special1
  FSpecial2: Boolean;                                                           //DDH 10/16/01 for Special2
  FKeepGoing: Boolean;                                                          //DDH 10/16/01 for GroupUndo and AutoComplete

  procedure RemoveGroupBreak;                                                   //ek 2000-11-04
  begin
    while (fRedoList.PeekItem<>nil) and (fRedoList.PeekItem.ChangeReason=crGroupBreak) do begin
      fRedoList.PopItem.Free;
      fUndoList.AddGroupBreak;
    end;
  end;

begin
  if ReadOnly then                                                              //jcr 2001-01-16
    exit;

  RemoveGroupBreak;                                                             //ek 2000-11-04
  FLastChange := FRedoList.GetChangeReason;
  FAutoComplete := FLastChange = crAutoCompleteBegin;
  FSpecial1 := FLastChange = crSpecial1Begin;
  FSpecial2 := FLastChange = crSpecial2Begin;

  Item := fRedoList.PeekItem;
  if Item <> nil then begin
    OldChangeNumber := Item.ChangeNumber;
    SaveChangeNumber := fUndoList.BlockChangeNumber;
    fUndoList.BlockChangeNumber := Item.ChangeNumber;
    try
      repeat
        RedoItem;
        Item := fRedoList.PeekItem;
        if Item <> nil then
        begin                                                                   //DDH 10/16/01 for GroupUndo and AutoComplete
          if FAutoComplete then
             FKeepGoing:= (FRedoList.GetChangeReason <> crAutoCompleteEnd)
          else if FSpecial1 then
             FKeepGoing := (FRedoList.GetChangeReason <> crSpecial1End)
          else if FSpecial2 then
             FKeepGoing := (FRedoList.GetChangeReason <> crSpecial2End)
          else if Item.ChangeNumber = OldChangeNumber then
             FKeepGoing := True
          else FKeepGoing := ((eoGroupUndo in FOptions) and
                             not(FLastChange in [crIndent, crUnindent]) and
                             (FLastChange = FRedoList.GetChangeReason));
        end else FKeepGoing := False;
      until not(FKeepGoing);

      RemoveGroupBreak;                                                         //ek 2000-11-04

      if FAutoComplete then
      begin                                                                     //we need to eat the last command since it does nothing.
        if FRedoList.GetChangeReason = crAutoCompleteEnd then
          RedoItem;
      end else if FSpecial1 then
      begin                                                                     //we need to eat the last command since it does nothing.
        if FRedoList.GetChangeReason = crSpecial1End then
          RedoItem;
      end else if FSpecial2 then
      begin                                                                     //we need to eat the last command since it does nothing.
        if FRedoList.GetChangeReason = crSpecial2End then
          RedoItem;
      end;

    finally
      fUndoList.BlockChangeNumber := SaveChangeNumber;
    end;
  end;
end;

procedure TCustomSynEdit.RedoItem;
{end}                                                                           //sbs 2000-11-19
var
  Item: TSynEditUndoItem;
  OldSelMode: TSynSelectionMode;
  Run, StrToDelete: PChar;
  Len: integer;
  TempString: string;
  CaretPt: TPoint;
  ChangeScrollPastEol: boolean;                                                 //mh 2000-10-30
begin
  OldSelMode := SelectionMode;
  ChangeScrollPastEol := not (eoScrollPastEol in Options);                      //mh 2000-10-30
  Item := fRedoList.PopItem;
  if Assigned(Item) then try
    SelectionMode := Item.ChangeSelMode;
    IncPaintLock;
    Include(fOptions, eoScrollPastEol);                                         //mh 2000-10-30
    Include(fStateFlags, sfInsideRedo);                                         //mh 2000-10-30
    case Item.ChangeReason of
{begin}                                                                         //Fiala 2001-12-17
      crWrap:
        WordWrap := False;
      crUnWrap:
        WordWrap := True;
{end}                                                                           //Fiala 2001-12-17

      crInsert, crPaste, crDragDropInsert:
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeStartPos);
          SetSelTextPrimitive(Item.ChangeSelMode, PChar(Item.ChangeStr), nil);
          CaretXY := Item.ChangeEndPos;                                         //mh 2000-10-30
          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, GetSelText, Item.ChangeSelMode);
{begin}                                                                         //mh 2000-11-20
          if Item.ChangeReason = crDragDropInsert then begin
            SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
              Item.ChangeEndPos);
          end;
{end}                                                                           //mh 2000-11-20
        end;
      crDeleteAfterCursor, crSilentDeleteAfterCursor:                           //mh 2000-10-30
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, GetSelText, Item.ChangeSelMode);
          SetSelTextPrimitive(Item.ChangeSelMode, PChar(Item.ChangeStr), nil);
          CaretXY := Item.ChangeStartPos;
        end;
      crDelete, {crDragDropDelete, crSelDelete, }crSilentDelete:                //mh 2000-10-30, 2000-11-20
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, GetSelText, Item.ChangeSelMode);
          SetSelTextPrimitive(Item.ChangeSelMode, PChar(Item.ChangeStr), nil);
          CaretXY := Item.ChangeStartPos;
{begin}                                                                         //mh 2000-11-20
(*
          // process next entry? This is awkward, and should be replaced by
          // undoitems maintaining a single linked list of connected items...
          ItemNext := fRedoList.PeekItem;
          if {(Item.ChangeReason = crSelDelete) or }
            ((Item.ChangeReason = crDragDropDelete) and Assigned(ItemNext)
              and (ItemNext.ChangeReason = crDragDropInsert))
          then
            Redo;
*)
{end}                                                                           //mh 2000-11-20
        end;
      crLineBreak:
{begin}                                                                         //sbs 2000-11-20
//        CommandProcessor(ecLineBreak, #13, nil);
        begin
          CaretPt := Item.ChangeStartPos;
          SetCaretAndSelection(CaretPt, CaretPt, CaretPt);
          CommandProcessor(ecLineBreak, #13, nil);
        end;
{end}                                                                           //sbs 2000-11-20
      crIndent:
        begin
          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, Item.ChangeStr, Item.ChangeSelMode);
          // restore the selection
          SetCaretAndSelection(Item.ChangeEndPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
        end;
       crUnindent :
         begin // re-delete the (raggered) column
           // add to undo list
           fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
             Item.ChangeEndPos, Item.ChangeStr, smColumn);
           // Delete string
           StrToDelete := PChar(Item.ChangeStr);
           CaretY := Item.ChangeStartPos.Y;
           repeat
             Run := GetEOL(StrToDelete);
             if Run <> StrToDelete then begin
               Len := Run - StrToDelete;
               TempString := Lines[CaretY - 1];
               if Len > 0 then
                 Delete(TempString, 1, Len);
               Lines[CaretY - 1] := TempString;
             end else
               Len := 0;
             if Run^ = #13 then begin
               Inc(Run);
               if Run^ = #10 then
                 Inc(Run);
               Inc(fCaretY);
             end;
             StrToDelete := Run;
           until Run^ = #0;
           // restore selection
           CaretPt := Point(Item.ChangeStartPos.x - fTabWidth,
             Item.ChangeStartPos.y);
           SetCaretAndSelection(CaretPt, CaretPt,
             Point(Item.ChangeEndPos.x - Len, Item.ChangeEndPos.y));
         end;
      crWhiteSpaceAdd:
        begin
          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
             Item.ChangeEndPos, '', Item.ChangeSelMode);
          SetCaretAndSelection(Item.ChangeEndPos, Item.ChangeEndPos,
            Item.ChangeEndPos);
          SetSelTextPrimitive(Item.ChangeSelMode, PChar(Item.ChangeStr), nil);
          CaretXY := Item.ChangeStartPos;
        end;
    end;
  finally
    SelectionMode := OldSelMode;
    Exclude(fStateFlags, sfInsideRedo);                                         //mh 2000-10-30
    if ChangeScrollPastEol then                                                 //mh 2000-10-30
      Exclude(fOptions, eoScrollPastEol);
    Item.Free;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.Undo;
{begin}                                                                         //sbs 2000-11-19
var
  Item: TSynEditUndoItem;
  OldChangeNumber: integer;
  SaveChangeNumber: integer;
  FLastChange : TSynChangeReason;                                               //DDH 10/16/01 for GroupUndo
  FAutoComplete: Boolean;                                                       //DDH 10/16/01 for AutoComplete
  FSpecial1: Boolean;                                                           //DDH 10/16/01 for Special1
  FSpecial2: Boolean;                                                           //DDH 10/16/01 for Special2
  FKeepGoing: Boolean;

  procedure RemoveGroupBreak;                                                   //ek 2000-11-04
  begin
    while (fUndoList.PeekItem<>nil) and (fUndoList.PeekItem.ChangeReason=crGroupBreak) do begin
      fUndoList.PopItem.Free;
      fRedoList.AddGroupBreak;
    end;
  end;

begin
  if ReadOnly then                                                              //jcr 2001-01-16
    exit;

  RemoveGroupBreak;                                                             //ek 2000-11-04

  FLastChange := FUndoList.GetChangeReason;
  FAutoComplete := FLastChange = crAutoCompleteEnd;
  FSpecial1 := FLastChange = crSpecial1End;
  FSpecial2 := FLastChange = crSpecial2End;

  Item := fUndoList.PeekItem;
  if Item <> nil then begin
    OldChangeNumber := Item.ChangeNumber;
    SaveChangeNumber := fRedoList.BlockChangeNumber;
    fRedoList.BlockChangeNumber := Item.ChangeNumber;

    try
      repeat
        UndoItem;
        Item := fUndoList.PeekItem;
        if Item <> nil then
        begin                                                                   //DDH 10/16/01 for GroupUndo and AutoComplete
          if FAutoComplete then
             FKeepGoing := (FUndoList.GetChangeReason <> crAutoCompleteBegin)
          else if FSpecial1 then
             FKeepGoing := (FUndoList.GetChangeReason <> crSpecial1Begin)
          else if FSpecial2 then
             FKeepGoing := (FUndoList.GetChangeReason <> crSpecial2Begin)
          else if Item.ChangeNumber = OldChangeNumber then
          begin
             FKeepGoing := True;
          end else FKeepGoing := ((eoGroupUndo in FOptions) and
                                 not(FLastChange in [crIndent, crUnindent]) and
                                 (FLastChange = FUndoList.GetChangeReason));
        end else FKeepGoing := False;
      until not(FKeepGoing);

      RemoveGroupBreak;                                                         //ek 2000-11-04

      if FAutoComplete then
      begin                                                                     //we need to eat the last command since it does nothing.
        if FUndoList.GetChangeReason = crAutoCompleteBegin then
          UndoItem;
      end else if FSpecial1 then
      begin                                                                     //we need to eat the last command since it does nothing.
        if FUndoList.GetChangeReason = crSpecial1Begin then
          UndoItem;
      end else if FSpecial2 then
      begin                                                                     //we need to eat the last command since it does nothing.
        if FUndoList.GetChangeReason = crSpecial2Begin then
          UndoItem;
      end;
    finally
      fRedoList.BlockChangeNumber := SaveChangeNumber;
    end;
  end;
end;

procedure TCustomSynEdit.UndoItem;
{end}                                                                           //sbs 2000-11-19
var
  Item: TSynEditUndoItem;
  OldSelMode: TSynSelectionMode;
  TmpPos: TPoint;
  TmpStr: string;
  ChangeScrollPastEol: boolean;                                                 //mh 2000-10-30
begin
  OldSelMode := SelectionMode;
  ChangeScrollPastEol := not (eoScrollPastEol in Options);                      //mh 2000-10-30
  Item := fUndoList.PopItem;
  if Assigned(Item) then try
    SelectionMode := Item.ChangeSelMode;
    IncPaintLock;
    Include(fOptions, eoScrollPastEol);                                         //mh 2000-10-30
    case Item.ChangeReason of
{begin}                                                                         //Fiala 2001-12-17
      crWrap:
        WordWrap := False;
      crUnWrap:
        WordWrap := True;
{end}                                                                           //Fiala 2001-12-17
      crInsert, crPaste, crDragDropInsert:
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, GetSelText, Item.ChangeSelMode);
          SetSelTextPrimitive(Item.ChangeSelMode, PChar(Item.ChangeStr), nil);
          CaretXY := Item.ChangeStartPos;
{begin}                                                                         //mh 2000-11-20
(*
          // process next entry? This is awkward, and should be replaced by
          // undoitems maintaining a single linked list of connected items...
          ItemNext := fUndoList.PeekItem;
          if Assigned(ItemNext) and
            ((ItemNext.ChangeReason = crSelDelete) or
            ((ItemNext.ChangeReason = crDragDropDelete)
               and (Item.ChangeReason = crDragDropInsert)))
          then
            Undo;
*)
{end}                                                                           //mh 2000-11-20
        end;
      crDeleteAfterCursor, crDelete, {crDragDropDelete, crSelDelete, }          //mh 2000-11-20
      crSilentDelete, crSilentDeleteAfterCursor,                                //mh 2000-10-30
      crDeleteAll:                                                              //Fiala 2001-12-17
        begin
          // If there's no selection, we have to set
          // the Caret's position manualy.
          if Item.ChangeSelMode = smColumn then
            TmpPos := Point(Min(Item.ChangeStartPos.X, Item.ChangeEndPos.X),
              Min(Item.ChangeStartPos.Y, Item.ChangeEndPos.Y))
          else
            TmpPos := minPoint(Item.ChangeStartPos, Item.ChangeEndPos);
          if (Item.ChangeReason in [crDeleteAfterCursor,
            crSilentDeleteAfterCursor]) and (TmpPos.Y > Lines.Count)            //mh 2000-10-30
          then begin
            CaretXY := Point(1, Lines.Count);
            // this stinks!!!
//            CommandProcessor(ecLineBreak, #13, nil);
            fLines.Add('');                                                     //DDH 10/16/01 bug fix by Slavek Rydval
          end;
          SetCaretAndSelection(TmpPos, TmpPos, TmpPos);
          SetSelTextPrimitive(Item.ChangeSelMode, PChar(Item.ChangeStr), nil);
{begin}                                                                         //mh 2000-10-30
          if Item.ChangeReason in [crDeleteAfterCursor,
            crSilentDeleteAfterCursor]
          then
            TmpPos := Item.ChangeStartPos
          else
            TmpPos := Item.ChangeEndPos;
          if Item.ChangeReason in [crSilentDelete, crSilentDeleteAfterCursor]
          then
            CaretXY := TmpPos
          else begin
            SetCaretAndSelection(TmpPos, Item.ChangeStartPos,
              Item.ChangeEndPos);
          end;
{end}                                                                           //mh 2000-10-30
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, '', Item.ChangeSelMode);
{begin}                                                                         //Fiala 2001-12-17
          if Item.ChangeReason = crDeleteAll then begin
            CaretXY := Point(1, 1);
            fBlockEnd := Point(1, 1);
          end;
{end}                                                                           //Fiala 2001-12-17
          EnsureCursorPosVisible;
        end;
      crLineBreak:
        begin
          // If there's no selection, we have to set
          // the Caret's position manualy.
          CaretXY := Item.ChangeStartPos;
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, '', Item.ChangeSelMode);
          if CaretY > 0 then begin
            TmpStr := Lines.Strings[CaretY - 1];
            if (Length(TmpStr) < CaretX - 1)
              and (LeftSpaces(Item.ChangeStr) = 0)
            then
              TmpStr := TmpStr + StringOfChar(#32, CaretX - 1 - Length(TmpStr));
            Lines.Delete(Item.ChangeEndPos.y);
          end;
          CaretXY := Item.ChangeStartPos;
          TrimmedSetLine(CaretY - 1, TmpStr + Item.ChangeStr);
          DoLinesDeleted(CaretY, 1);
        end;
      crIndent:
        begin
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, Item.ChangeStr, Item.ChangeSelMode);
          // restore the selection
          SetCaretAndSelection(Item.ChangeEndPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
        end;
       crUnindent: // reinsert the (raggered) column that was deleted
         begin
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
             Item.ChangeEndPos, Item.ChangeStr, Item.ChangeSelMode);
           // reinsert the string
           InsertBlockEx(Point(1, Item.ChangeStartPos.y),
             Point(1, Item.ChangeEndPos.y), PChar(Item.ChangeStr), False);
           SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
             Item.ChangeEndPos);
         end;
      crWhiteSpaceAdd:
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, GetSelText, Item.ChangeSelMode);
          SetSelTextPrimitive(Item.ChangeSelMode, PChar(Item.ChangeStr), nil);
          CaretXY := Item.ChangeStartPos;
        end;
    end;
  finally
    SelectionMode := OldSelMode;
    if ChangeScrollPastEol then                                                 //mh 2000-10-30
      Exclude(fOptions, eoScrollPastEol);
    Item.Free;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.ClearBookMark(BookMark: Integer);
begin
  if (BookMark in [0..9]) and assigned(fBookMarks[BookMark]) then begin
    DoOnClearBookmark(fBookMarks[BookMark]);
    FMarkList.Remove(fBookMarks[Bookmark]);
    fBookMarks[BookMark].Free;
    fBookMarks[BookMark] := nil;
  end
end;

procedure TCustomSynEdit.GotoBookMark(BookMark: Integer);
begin
  if (BookMark in [0..9]) and
     assigned(fBookMarks[BookMark]) and
     (fBookMarks[BookMark].Line <= fLines.Count)
  then
  begin
    SetCaretXYEx(False,                                                         //DDH 10/16/01 Begin
                 Point(fBookMarks[BookMark].Column, fBookMarks[BookMark].Line));

    if SelAvail then
    begin
      fBlockBegin := Point(fCaretX, fCaretY);
      fBlockEnd := fBlockBegin;
    end;

    EnsureCursorPosVisibleEx(True);                                             //DDH 10/16/01 End
  end;
end;

procedure TCustomSynEdit.SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
var
  mark: TSynEditMark;
begin
  if (BookMark in [0..9]) and (Y >= 1) and (Y <= Max(1, fLines.Count)) then
  begin
    mark := TSynEditMark.Create(self);
    with mark do begin
      Line := Y;
      Column := X;
      ImageIndex := Bookmark;
      BookmarkNumber := Bookmark;
      Visible := true;
      InternalImage := (fBookMarkOpt.BookmarkImages = nil);
    end;
    DoOnPlaceMark(Mark);
    if (mark <> nil) then begin
      if assigned(fBookMarks[BookMark]) then
        ClearBookmark(BookMark);
      fBookMarks[BookMark] := mark;
      FMarkList.Add(fBookMarks[BookMark]);
    end;
  end;
end;

{$IFDEF SYN_CLX}
{$ELSE}
procedure TCustomSynEdit.WndProc(var Msg: TMessage);
// Prevent Alt-Backspace from beeping
const
  ALT_KEY_DOWN = $20000000;
begin
  if (Msg.Msg = WM_SYSCHAR) and (Msg.wParam = VK_BACK) and
    (Msg.lParam and ALT_KEY_DOWN <> 0)
  then
    Msg.Msg := 0
  else
    inherited;
end;
{$ENDIF}

{$IFDEF SYN_CLX}
function TCustomSynEdit.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
begin
  case QEvent_type(Event) of
    QEventType_FocusIn:
      begin
        InitializeCaret;
        if FHideSelection and SelAvail then
          Invalidate;
      end;
    QEventType_FocusOut:
      begin
        HideCaret;
        kTextDrawer.DestroyCaret;
        if FHideSelection and SelAvail then
          Invalidate;
      end;
  end;

  result := inherited EventFilter(Sender, Event);
end;
{$ENDIF}

procedure TCustomSynEdit.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited;
  if (Source is TCustomSynEdit) and not TCustomSynEdit(Source).ReadOnly then
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
    if State = dsDragLeave then //restore prev caret position
      ComputeCaret(FMouseDownX, FMouseDownY)
    else //position caret under the mouse cursor
      ComputeCaret(X, Y);
  end;
end;

procedure TCustomSynEdit.DragDrop(Source: TObject; X, Y: Integer);
var
  NewCaret: TPoint;
  DoDrop, DropAfter, DropMove: boolean;
  BB, BE: TPoint;
  DragDropText: string;
  Adjust: integer;
  ChangeScrollPastEOL: boolean;
  iPoint: TPoint;                                                               //DDH 10/17/01 from Flávio Etrusco
begin
  if not ReadOnly  and (Source is TCustomSynEdit)
    and TCustomSynEdit(Source).SelAvail
  then begin
    IncPaintLock;
    try
      inherited;
      ComputeCaret(X, Y);
      NewCaret := CaretXY;
      // if from other control then move when SHIFT, else copy
      // if from Self then copy when CTRL, else move
      if Source <> Self then
      begin
{$IFDEF SYN_CLX}
        DropMove := FALSE;
{$ELSE}
        DropMove := GetKeyState(VK_SHIFT) < 0;
{$ENDIF}
        DoDrop := TRUE;
        DropAfter := FALSE;
      end
      else
      begin
{$IFDEF SYN_CLX}
        DropMove := False;
{$ELSE}
        DropMove := GetKeyState(VK_CONTROL) >= 0;
{$ENDIF}
        BB := BlockBegin;
        BE := BlockEnd;
        DropAfter := (NewCaret.Y > BE.Y)
          or ((NewCaret.Y = BE.Y) and (NewCaret.X > BE.X));
        DoDrop := DropAfter or (NewCaret.Y < BB.Y)
          or ((NewCaret.Y = BB.Y) and (NewCaret.X < BB.X));
      end;
      if DoDrop then begin
        BeginUndoBlock;                                                         //mh 2000-11-20
        try
          DragDropText := TCustomSynEdit(Source).SelText;
          // delete the selected text if necessary
          if DropMove then begin
            if Source <> Self then
              TCustomSynEdit(Source).SelText := ''
            else begin
//              fUndoList.AddChange(crDragDropDelete, fBlockBegin, fBlockEnd,
              fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd,             //mh 2000-11-20
                DragDropText, SelectionMode);
              LockUndo;
              try
                SelText := '';
              finally
                UnlockUndo;
              end;
              // adjust horizontal drop position
              if DropAfter and (NewCaret.Y = BE.Y) then begin
                if BB.Y = BE.Y then
                  Adjust := BE.X - BB.X
                else
                  Adjust := BE.X - 1;
                Dec(NewCaret.X, Adjust);
              end;
              // adjust vertical drop position
              if DropAfter and (BE.Y > BB.Y) then
                Dec(NewCaret.Y, BE.Y - BB.Y);
            end;
          end;
          // insert the selected text
          ChangeScrollPastEOL := not (eoScrollPastEol in fOptions);
          try
            if ChangeScrollPastEOL then
              Include(fOptions, eoScrollPastEol);
            CaretXY := NewCaret;
            BlockBegin := NewCaret;
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
          if Source = Self then begin
            fUndoList.AddChange(crDragDropInsert, NewCaret, BlockEnd, SelText,
              SelectionMode);
          end else begin
            fUndoList.AddChange(crInsert, NewCaret, BlockEnd,
              SelText, SelectionMode);
          end;
          BlockEnd := CaretXY;                                                  //DDH 10/17/01 begin from Flávio Etrusco
          iPoint := NewCaret;
          CommandProcessor( ecSelGotoXY, #0, @iPoint );                         //DDH 10/17/01 end from Flávio Etrusco
        finally
          EndUndoBlock;
        end;
      end;
    finally
      DecPaintLock;
    end;
  end else
    inherited;
end;

procedure TCustomSynEdit.SetRightEdge(Value: Integer);
begin
  if fRightEdge <> Value then begin
    fRightEdge := Value;
    Invalidate;
  end;
end;

procedure TCustomSynEdit.SetRightEdgeColor(Value: TColor);
var
  nX: integer;
  rcInval: TRect;
begin
  if fRightEdgeColor <> Value then begin
    fRightEdgeColor := Value;
    if HandleAllocated then begin
      nX := fTextOffset + fRightEdge * fCharWidth;
      rcInval := Rect(nX - 1, 0, nX + 1, ClientHeight);
{$IFDEF SYN_CLX}
      InvalidateRect(rcInval, FALSE);
{$ELSE}
      InvalidateRect(Handle, @rcInval, FALSE);
{$ENDIF}
    end;
  end;
end;

function TCustomSynEdit.GetMaxUndo: Integer;
begin
  result := fUndoList.MaxUndoActions;
end;

procedure TCustomSynEdit.SetMaxUndo(const Value: Integer);
begin
  if Value > -1 then begin
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
    if AComponent = fHighlighter then
    begin
      fHighlighter := nil;
{begin}                                                                         //mh 2000-10-01
      if not (csDestroying in ComponentState) then
      begin
        RecalcCharExtent;
        SizeOrFontChanged(TRUE);                                                //jr 2000-10-01
        Invalidate;
      end;
{end}                                                                           //mh 2000-10-01
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
      fHighlighter.UnhookAttrChangeEvent(HighlighterAttrChanged);
    if Assigned(Value) then
    begin
      Value.HookAttrChangeEvent(HighlighterAttrChanged);
      Value.FreeNotification(Self);
    end;
    fHighlighter := Value;
    RecalcCharExtent;
    Lines.BeginUpdate;
    try
      ListScanRanges(Self);
    finally
      Lines.EndUpdate;
    end;
    SizeOrFontChanged(TRUE);
  end;
end;

procedure TCustomSynEdit.SetBorderStyle(Value: TBorderStyle);
begin
  if fBorderStyle <> Value then
  begin
    fBorderStyle := Value;
{$IFDEF SYN_CLX}
    Invalidate;
{$ELSE}
    RecreateWnd;
{$ENDIF}
  end;
end;

procedure TCustomSynEdit.SetHideSelection(const Value: boolean);
begin
  if fHideSelection <> Value then begin
    FHideSelection := Value;
    Invalidate;
  end;
end;

procedure TCustomSynEdit.SetInsertMode(const Value: boolean);
begin
  if fInserting <> Value then begin
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
  cw, ch: integer;
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
    else begin // ctVerticalLine
      cw := 2;
      ch := fTextHeight - 2;
      FCaretOffset := Point(-1, 0);
    end;
  end;
  Exclude(fStateFlags, sfCaretVisible);

  if (Focused) or (FAlwaysShowCaret) then
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
  if FInsertCaret <> Value then begin
    FInsertCaret := Value;
    InitializeCaret;
  end;
end;

procedure TCustomSynEdit.SetOverwriteCaret(const Value: TSynEditCaretType);
begin
  if FOverwriteCaret <> Value then begin
    FOverwriteCaret := Value;
    InitializeCaret;
  end;
end;

procedure TCustomSynEdit.SetMaxLeftChar(Value: integer);
begin
  Value := MinMax(Value, 1, MAX_SCROLL); // horz scrolling is only 16 bit
  if fMaxLeftChar <> Value then begin
    fMaxLeftChar := Value;
    Invalidate;
  end;
end;

procedure TCustomSynEdit.EnsureCursorPosVisible;
begin
  EnsureCursorPosVisibleEx(False);
end;

procedure TCustomSynEdit.EnsureCursorPosVisibleEx(ForceToMiddle: Boolean);
VAR TmpMiddle : Integer;
    VisibleX  : Integer;                                                        //GBN 2001-10-24
    LogicalPos: TPoint;                                                         //sblbg 2001-12-17

begin
  IncPaintLock;
  try
    // Make sure X is visible

    //Changed all CaretX to VisibleX in block below
    LogicalPos := LogicalToPhysicalPos(CaretXY);                                //sblbg 2001-12-17
                                                                                //GBN 2001-10-24
    VisibleX := LogicalPos.X;                                                   //sblbg 2001-12-17

    if VisibleX < LeftChar then
      LeftChar := VisibleX
    else if VisibleX > CharsInWindow + LeftChar then
      LeftChar := VisibleX - CharsInWindow + 1
    else
      LeftChar := LeftChar;                                                     //mh 2000-10-19

    // Make sure Y is visible
//DDH 10/16/01 Start
    if ForceToMiddle then
    begin
      if fCaretY < (TopLine - 1) then
      begin
        TmpMiddle := LinesInWindow div 2;
        if fCaretY - TmpMiddle < 0 then
          TopLine := 1
        else TopLine := fCaretY - TmpMiddle + 1;
      end else if fCaretY > (TopLine + (LinesInWindow - 2)) then
      begin
        TmpMiddle := LinesInWindow div 2;
        TopLine := fCaretY - (LinesInWindow - 1) + TmpMiddle;
      end;
    end else begin
      if CaretY < TopLine then
        TopLine := CaretY
      else if CaretY > TopLine + Max(1, LinesInWindow) - 1 then                   //mh 2000-10-19
        TopLine := CaretY - (LinesInWindow - 1)
      else
        TopLine := TopLine;                                                       //mh 2000-10-19
    end;
//DDH 10/16/01 End
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
  i: integer;
  pCaret: PPoint; // added 1/29/2002 JCH
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
  // added 1/29/2002 JCH
  if (Result >= ecSetMarker0) and (Result <= ecSetMarker9) then
  begin
    GetMem(pCaret, sizeof(TPoint));
    pCaret^.x := CaretXY.x;
    pCaret^.y := CaretXY.y;
    Data := pCaret;
  end;
end;

procedure TCustomSynEdit.CommandProcessor(Command: TSynEditorCommand;
  AChar: char; Data: pointer);
begin
  // first the program event handler gets a chance to process the command
  DoOnProcessCommand(Command, AChar, Data);
  if Command <> ecNone then begin
    // notify hooked command handlers before the command is executed inside of
    // the class
    NotifyHookedCommandHandlers(FALSE, Command, AChar, Data);
    // internal command handler
    if (Command <> ecNone) and (Command < ecUserFirst) then
      ExecuteCommand(Command, AChar, Data);
    // notify hooked command handlers after the command was executed inside of
    // the class
    NotifyHookedCommandHandlers(TRUE, Command, AChar, Data);
  end;
  DoOnCommandProcessed(Command, AChar, Data);
end;

procedure TCustomSynEdit.ExecuteCommand(Command: TSynEditorCommand; AChar: char;
  Data: pointer);
const
  ALPHANUMERIC = DIGIT + ALPHA_UC + ALPHA_LC;
  SEL_MODE: array[ecNormalSelect..ecLineSelect] of TSynSelectionMode = (
    smNormal, smColumn, smLine);
var
  CX: Integer;
  Len: Integer;
  Temp: string;
  Temp2: string;
  Helper: string;
  TabBuffer: string;
  SpaceBuffer: String;
  SpaceCount1: Integer;
  SpaceCount2: Integer;
  SpaceCount3: Integer;
  ModCounter: Integer;
  BackCounter: Integer;
  StartOfBlock: TPoint;
  bChangeScroll: boolean;
  moveBkm: boolean;
  WP: TPoint;
  Caret: TPoint;
  CaretNew: TPoint;
  OldSelMode: TSynSelectionMode;
{begin}                                                                         //Fiala
  i: integer;
  EndOfBlock: TPoint;
{  OldWordWrap: Boolean; }
{end}                                                                           //Fiala
{$IFDEF SYN_MBCSSUPPORT}
  s: string;
{$ENDIF}
  counter: Integer;
  InsDelta: integer;

{begin}                                                                         //mh 2000-10-30
  procedure SetSelectedTextEmpty;
  begin
    if (fBlockBegin.Y < fBlockEnd.Y)
      or ((fBlockBegin.Y = fBlockEnd.Y) and (fBlockBegin.X < fBlockEnd.X))
    then
      fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, SelText,
        SelectionMode)
    else
      fUndoList.AddChange(crDeleteAfterCursor, fBlockEnd, fBlockBegin, SelText,
        SelectionMode);
    SetSelText('');
  end;
{end}                                                                           //mh 2000-10-30

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
{begin}                                                                         //mh 2000-10-19
      ecLineStart, ecSelLineStart:
        begin
//          MoveCaretAndSelection(CaretXY, Point(1, CaretY),
//            Command = ecSelLineStart);
          DoHomeKey(Command = ecSelLineStart);                                  //EK 10/16/01
        end;
      ecLineEnd, ecSelLineEnd:
        begin
          MoveCaretAndSelection(CaretXY, Point(1 + Length(LineText), CaretY),
            Command = ecSelLineEnd);
        end;
{end}                                                                           //mh 2000-10-19
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
          MoveCaretAndSelection(CaretXY, Point(CaretX, TopLine),
            Command = ecSelPageTop);
          Update;
        end;
      ecPageBottom, ecSelPageBottom:
        begin
          CaretNew := Point(CaretX, TopLine + LinesInWindow - 1);
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelPageBottom);
          Update;
        end;
      ecEditorTop, ecSelEditorTop:
        begin
          MoveCaretAndSelection(CaretXY, Point(1, 1), Command = ecSelEditorTop);
          Update;
        end;
      ecEditorBottom, ecSelEditorBottom:
        begin
          CaretNew := Point(1, Lines.Count);
          if (CaretNew.Y > 0) then
            CaretNew.X := Length(Lines[CaretNew.Y - 1]) + 1;
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelEditorBottom);
          Update;
        end;
// goto special line / column position
      ecGotoXY, ecSelGotoXY:
        if Assigned(Data) then begin
          MoveCaretAndSelection(CaretXY, PPoint(Data)^, Command = ecSelGotoXY);
          Update;
        end;
// word selection
      ecWordLeft, ecSelWordLeft:
        begin
          Caret := CaretXY;
          CaretNew := PrevWordPos;
          MoveCaretAndSelection(Caret, CaretNew, Command = ecSelWordLeft);
        end;
      ecWordRight, ecSelWordRight:
        begin
          Caret := CaretXY;
          CaretNew := NextWordPos;
          MoveCaretAndSelection(Caret, CaretNew, Command = ecSelWordRight);
        end;
      ecSelectAll:
        begin
          SelectAll;
        end;
{begin}                                                                         //mh 2000-10-30
      ecDeleteLastChar:
        if not ReadOnly then begin
          DoOnPaintTransientEx(ttBefore,true);                                  //GBN 2002-03-23
          try
            if SelAvail then
              SetSelectedTextEmpty
            else begin
              Temp := LineText;
              TabBuffer := TSynEditStringList(Lines).ExpandedStrings[CaretY - 1];
              Len := Length(Temp);
              Caret := CaretXY;
              if CaretX > Len + 1 then begin
                Helper := '';
                if eoSmartTabDelete in fOptions then                              //DDH 10/16/01 begin
                begin
                  //It's at the end of the line, move it to the length
                  if Len > 0 then
                    CaretX := Len + 1
                  else begin
                    //move it as if there were normal spaces there
                    SpaceCount1 := CaretX - 1;
                    SpaceCount2 := 0;
                    // unindent
                    if SpaceCount1 > 0 then begin
                      BackCounter := CaretY - 2;
                      while BackCounter >= 0 do begin
                        SpaceCount2 := LeftSpaces(Lines[BackCounter]);
                        if SpaceCount2 < SpaceCount1 then
                          break;
                        Dec(BackCounter);
                      end;
                    end;
                    if SpaceCount2 = SpaceCount1 then
                      SpaceCount2 := 0;
                    fCaretX := fCaretX - (SpaceCount1 - SpaceCount2);
                    UpdateLastCaretX;                                           //jr 2002-04-26
                    fStateFlags := fStateFlags + [sfCaretChanged];
                    StatusChanged([scCaretX]);
                  end;
                end else begin
                  // only move caret one column
                  CaretX := CaretX - 1;
                end;                                                              //DDH 10/16/01 end
              end else if CaretX = 1 then begin
                // join this line with the last line if possible
                if CaretY > 1 then begin
                  CaretY := CaretY - 1;
                  CaretX := Length(Lines[CaretY - 1]) + 1;
                  Lines.Delete(CaretY);
                  DoLinesDeleted(CaretY, 1);
                  if eoTrimTrailingSpaces in Options then
                    Temp := TrimTrailingSpaces(Temp);
                  LineText := LineText + Temp;
                  Helper := #13#10;
                end;
              end else begin
                // delete text before the caret
                SpaceCount1 := LeftSpaces(Temp);
                SpaceCount2 := 0;
                if (Temp[CaretX - 1] <= #32) and (SpaceCount1 = CaretX - 1) then
                begin
                  if eoSmartTabDelete in fOptions then                            //DDH 10/16/01 Begin
                  begin
                    // unindent
                    if SpaceCount1 > 0 then begin
                      BackCounter := CaretY - 2;
                      while BackCounter >= 0 do begin
                        SpaceCount2 := LeftSpaces(Lines[BackCounter]);
                        if SpaceCount2 < SpaceCount1 then
                          break;
                        Dec(BackCounter);
                      end;
                    end;
                    if SpaceCount2 = SpaceCount1 then
                      SpaceCount2 := 0;
                    Helper := Copy(Temp, 1, SpaceCount1 - SpaceCount2);
                    Delete(Temp, 1, SpaceCount1 - SpaceCount2);
                  end else begin
                    SpaceCount2 := SpaceCount1;
                    //how much till the next tab column
                    BackCounter  := (DisplayX - 1) mod FTabWidth;
                    if BackCounter = 0 then BackCounter := FTabWidth;

                    SpaceCount3 := LeftSpaces(TabBuffer);
                    SpaceCount1 := 0;
                    ModCounter := 0;
                    while (SpaceCount1 < FTabWidth) and
                          (ModCounter < BackCounter) and
                          (TabBuffer[DisplayX - BackCounter + ModCounter] <> #9) do
                    begin
                      Inc(SpaceCount1);
                      Inc(ModCounter);
                    end;
                    if TabBuffer[DisplayX - BackCounter + ModCounter] = #9 then
                      SpaceCount1 := SpaceCount1 + 1;

                    if SpaceCount2 = SpaceCount1 then
                    begin
                      Helper := Copy(Temp, 1, SpaceCount1);
                      Delete(Temp, 1, SpaceCount1);
                    end else begin
                      Helper := Copy(Temp, SpaceCount2 - SpaceCount1 + 1, SpaceCount1);
                      Delete(Temp, SpaceCount2 - SpaceCount1 + 1, SpaceCount1);
                    end;
                    SpaceCount2 := 0;
                  end;                                                            //DDH 10/16/01 End
                  TrimmedSetLine(CaretY - 1, Temp);
                  fCaretX := fCaretX - (SpaceCount1 - SpaceCount2);
                  UpdateLastCaretX;                                             //jr 2002-04-26
                  fStateFlags := fStateFlags + [sfCaretChanged];
                  StatusChanged([scCaretX]);
                end else begin
                  // delete char
                  counter := 1;
  {$IFDEF SYN_MBCSSUPPORT}
                  if (CaretX >= 3) and (ByteType(Temp, CaretX - 2) = mbLeadByte) then
                    Inc(counter);
  {$ENDIF}
                  CaretX := CaretX - counter;
                  Helper := Copy(Temp, CaretX, counter);
                  Delete(Temp, CaretX, counter);
                  TrimmedSetLine(CaretY - 1, Temp);
                end;
              end;
              if (Caret.X <> CaretX) or (Caret.Y <> CaretY) then begin
                fUndoList.AddChange(crSilentDelete, CaretXY, Caret, Helper,
                  smNormal);
              end;
            end;
  {begin}                                                                         //Fiala
            if fWordWrap then
            begin
              if (Length(Lines[CaretY-1]) < CharsInWindow) then
              begin
                TSynEditStringList(fLines).ReWrapLine(CaretY-1);
                while CaretX-1 > Length(Lines[CaretY-1]) do
                begin
                  CaretX := CaretX - Length(Lines[CaretY-1]);
                  CaretY := CaretY + 1;
                end;
                InvalidateGutterLines(-1, -1);
              end;
            end;
            EnsureCursorPosVisible;
  {end}                                                                           //Fiala
          finally
            DoOnPaintTransientEx(ttAfter,true);                                   //GBN 2002-03-23
          end;
        end;
      ecDeleteChar:
        if not ReadOnly then begin
          DoOnPaintTransient(ttBefore);                                         //GBN 2001-10-23
          if SelAvail then
            SetSelectedTextEmpty
          else begin
            // Call UpdateLastCaretX. Even though the caret doesn't move, the
            // current caret position should "stick" whenever text is modified.
            UpdateLastCaretX;                                                   //jr 2002-04-26
            Temp := LineText;
            Len := Length(Temp);
            if CaretX <= Len then begin
              // delete char
              counter := 1;
{$IFDEF SYN_MBCSSUPPORT}
              if ByteType(Temp, CaretX) = mbLeadByte then
                Inc(counter);
{$ENDIF}
              Helper := Copy(Temp, CaretX, counter);
              Caret := Point(CaretX + counter, CaretY);
              Delete(Temp, CaretX, counter);
              TrimmedSetLine(CaretY - 1, Temp);
            end else begin
              // join line with the line after
              if CaretY < Lines.Count then begin
                Helper := StringOfChar(#32, CaretX - 1 - Len);
                TrimmedSetLine(CaretY - 1, Temp + Helper + Lines[CaretY]);
                Caret := Point(1, CaretY + 1);
                Helper := #13#10;
                Lines.Delete(CaretY);
                DoLinesDeleted(CaretY - 1, 1);
              end;
            end;
            if (Caret.X <> CaretX) or (Caret.Y <> CaretY) then begin
              fUndoList.AddChange(crSilentDeleteAfterCursor, CaretXY, Caret,
                Helper, smNormal);
            end;
          end;
{begin}                                                                         //Fiala
          if fWordWrap and (Length(Lines[CaretY-1]) < CharsInWindow)
          then begin
            TSynEditStringList(fLines).ReWrapLine(CaretY-1);
            while CaretX -1 > Length(Lines[CaretY-1]) do begin
              CaretX := CaretX - Length(Lines[CaretY-1]);
              CaretY := CaretY + 1;
            end;
            InvalidateGutterLines(-1, -1);
          end;
{end}                                                                           //Fiala

          DoOnPaintTransient(ttAfter);                                          //GBN 2001-10-23
        end;
      ecDeleteWord, ecDeleteEOL:
        if not ReadOnly then begin
          DoOnPaintTransient(ttBefore);                                         //GBN 2001-10-23
          Len := Length(LineText);
          if Command = ecDeleteWord then begin
            if CaretX > Len + 1 then
              CaretX := Len + 1;
            WP := NextWordPos;
          end else
            WP := Point(Len + 1, CaretY);
          if (WP.X <> CaretX) or (WP.Y <> CaretY) then begin
            OldSelMode := fSelectionMode;
            try
              fSelectionMode := smNormal;
              SetBlockBegin(CaretXY);
              SetBlockEnd(WP);
              fUndoList.AddChange(crSilentDeleteAfterCursor, CaretXY, WP,
                SelText, smNormal);
              SetSelText('');
            finally
              fSelectionMode := OldSelMode;
            end;
            CaretXY := CaretXY;
          end;
        end;
      ecDeleteLastWord, ecDeleteBOL:
        if not ReadOnly then begin
          DoOnPaintTransient(ttBefore);                                         //GBN 2001-10-23
          if Command = ecDeleteLastWord then
            WP := PrevWordPos
          else
            WP := Point(1, CaretY);
          if (WP.X <> CaretX) or (WP.Y <> CaretY) then begin
            OldSelMode := fSelectionMode;
            try
              fSelectionMode := smNormal;
              SetBlockBegin(CaretXY);
              SetBlockEnd(WP);
              fUndoList.AddChange(crSilentDelete, WP, CaretXY, SelText,
                smNormal);
              SetSelText('');
            finally
              fSelectionMode := OldSelMode;
            end;
            CaretXY := WP;
          end;
          DoOnPaintTransient(ttAfter);                                          //GBN 2001-10-23
        end;
{end}                                                                           //mh 2000-10-30
      ecDeleteLine:
        if not ReadOnly and not ((Lines.Count = 1) and (Length(Lines[0]) = 0))
        then begin
          DoOnPaintTransient(ttBefore);                                         //GBN 2001-10-23
          if SelAvail then
            SetBlockBegin(CaretXY);
          if Lines.Count = 1 then begin
            fUndoList.AddChange(crDeleteAfterCursor, Point(1, CaretY),
              CaretXY, LineText, smNormal);
            Lines[0] := '';
          end else begin
{begin}                                                                         //Fiala
            //Repair problem with deleting last line and UndoIt
            if CaretY < fLines.Count
              then fUndoList.AddChange(crDeleteAfterCursor, Point(1, CaretY),
                CaretXY, LineText + #13#10, smNormal)
              else fUndoList.AddChange(crDeleteAfterCursor, Point(1, CaretY),
                CaretXY, LineText, smNormal);
//            fUndoList.AddChange(crDeleteAfterCursor, Point(1, CaretY),
//                CaretXY, LineText + #13#10, smNormal)
{end}                                                                           //Fiala
            Lines.Delete(CaretY - 1);
          end;
          DoLinesDeleted(CaretY - 1, 1);
          CaretXY := Point(1, CaretY); // like seen in the Delphi editor
        end;
      ecClearAll:
        begin
          if not ReadOnly then ClearAll;
        end;
      ecInsertLine,
      ecLineBreak:
        if not ReadOnly then begin
          if SelAvail then begin
            fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, SelText,
              SelectionMode);
            SetSelText('');
          end;
//          SpaceCount2 := 0;  //GBN 2002-04-27, eliminate compiler warning
          Temp := LineText;
          Temp2 := Temp; //LineText;
// This is sloppy, but the Right Thing would be to track the column of markers
// too, so they could be moved depending on whether they are after the caret...
          InsDelta := Ord(CaretX = 1);
          Len := Length(Temp);
          if Len > 0 then begin
            if Len >= CaretX then begin
              if CaretX > 1 then begin
                Temp := Copy(LineText, 1, CaretX - 1);
                SpaceCount1 := LeftSpacesEx(Temp,true);                         //GBN 2002-03-06
                TrimmedSetLine(CaretY - 1, Temp);
                Delete(Temp2, 1, CaretX - 1);
                Lines.Insert(CaretY, GetLeftSpacing(SpaceCount1,true) + Temp2);
                fUndoList.AddChange(crLineBreak, CaretXY, CaretXY, Temp2,
                  smNormal);
                if Command = ecLineBreak then
                  CaretXY := Point(length(GetLeftSpacing(SpaceCount1,true)) + 1, CaretY + 1); //GBN 2002-03-06
              end else begin
                Lines.Insert(CaretY - 1, '');
                fUndoList.AddChange(crLineBreak, CaretXY, CaretXY, Temp2,
                  smNormal);
                if Command = ecLineBreak then
                  CaretY := CaretY + 1;
              end;
            end else begin
{begin}                                                                         //mh 2000-10-06
(*
              BackCounter := CaretY - 1;
              while BackCounter >= 0 do begin
                SpaceCount2 := LeftSpaces(Lines[BackCounter]);
                if Length(Lines[BackCounter]) > 0 then break;
                dec(BackCounter);
              end;
              fUndoList.AddChange(crLineBreak, CaretXY, CaretXY, '', smNormal);
              if Command = ecLineBreak then
                CaretX := SpaceCount2 + 1;
              if (Command = ecInsertLine) or (eoScrollPastEol in fOptions) then
                Lines.Insert(CaretY, '');
              if Command = ecLineBreak then begin
                Inc(fCaretY);
                StatusChanged([scCaretY]);
              end;
              if (Command = ecLineBreak) and
                (fOptions * [eoAutoIndent, eoScrollPastEol] = [eoAutoIndent])
              then begin
                Lines.Insert(CaretY - 1, StringOfChar(' ', SpaceCount2));
                CaretX := SpaceCount2 + 1;
              end;
*)
              fUndoList.AddChange(crLineBreak, CaretXY, CaretXY, '', smNormal);
              SpaceCount2 := 0;
              BackCounter := CaretY;  {moved from IF}                           //Fiala
              if eoAutoIndent in Options then begin
//                BackCounter := CaretY;
                repeat
                  Dec(BackCounter);
                  Temp := Lines[BackCounter];
                  SpaceCount2 := LeftSpaces(Temp);
                until (BackCounter = 0) or (Temp <> '');
              end;
              Lines.Insert(CaretY, '');
              if Command = ecLineBreak then begin
                if SpaceCount2 > 0 then
{begin}                                                                         //Fiala
                  Lines[CaretY] := Copy(Lines[BackCounter], 1, SpaceCount2);
//                Lines[CaretY] := StringOfChar(' ', SpaceCount2);
{end}                                                                           //Fiala
                CaretXY := Point(SpaceCount2 + 1, CaretY + 1);
              end;
{end}                                                                           //mh 2000-10-06
            end;
          end else begin
            if fLines.Count = 0 then
              fLines.Add('');
            SpaceCount2 := 0;                       //GBN 2002/04/12
            if eoAutoIndent in Options then begin   //GBN 2002/04/12
              BackCounter := CaretY - 1;
              while BackCounter >= 0 do begin
                SpaceCount2 := LeftSpacesEx(Lines[BackCounter],True); //GBN 2002/04/12
                if Length(Lines[BackCounter]) > 0 then break;
                dec(BackCounter);
              end;
            end;
            fUndoList.AddChange(crLineBreak, CaretXY, CaretXY, '', smNormal);
            if Command = ecLineBreak then
              CaretX := SpaceCount2 + 1;
            Lines.Insert(CaretY - 1, '');
            if Command = ecLineBreak then
              CaretY := CaretY + 1;
          end;
          DoLinesInserted(CaretY - InsDelta, 1);
          BlockBegin := CaretXY;                                                //DDH 10/16/01
          BlockEnd   := CaretXY;                                                //DDH 10/16/01
          EnsureCursorPosVisible;
          UpdateLastCaretX;                                                     //jr 2002-04-26
        end;
      ecTab:
        if not ReadOnly then DoTabKey;
      ecShiftTab:
        if not ReadOnly then DoShiftTabKey;                                     //EK 10/16/01
      ecMatchBracket:
        FindMatchingBracket;
      ecChar:
      // #127 is Ctrl + Backspace, #32 is space
        if not ReadOnly and (AChar >= #32) and (AChar <> #127) then begin
          if SelAvail then begin
            BeginUndoBlock;
            try
{begin}                                                                         //mh 2000-11-20
//            fUndoList.AddChange(crSelDelete, fBlockBegin, fBlockEnd, SelText,
              fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, SelText,
                SelectionMode);
              StartOfBlock := BlockBegin;
              if SelectionMode = smLine then
                StartOfBlock.X := 1;
              SetSelText(AChar);
              fUndoList.AddChange(crInsert, StartOfBlock, fBlockEnd, '',
                smNormal);
            finally
              EndUndoBlock;
            end;
{end}                                                                           //mh 2000-11-20
          end else begin
            SpaceCount2 := 0;          
            Temp := LineText;
            Len := Length(Temp);
            if Len < CaretX then
            begin
              //Hard tab fix GBN 2002/04/12
              if (Len>0) then
                SpaceBuffer := StringOfChar(#32, CaretX - Len - Ord(fInserting))
              else
                SpaceBuffer := GetLeftSpacing(CaretX - Len - Ord(fInserting),true);  //GBN 2002/04/12
              SpaceCount2 := length(SpaceBuffer);                        //GBN 2002/04/12

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

              if fInserting then begin
                Insert(AChar, Temp, CaretX);
                if (Len=0) then CaretX:=length(Temp)+1                          //GBN 2002/04/12
                else CaretX := CaretX + 1;                                      //GBN 2002/04/12
                TrimmedSetLine(CaretY - 1, Temp);
                if SpaceCount2 > 0 then                                         //GBN 2002/04/12
                begin
                  BeginUndoBlock;
                  try
                    //if we inserted spaces with this char, we need to account for those in the X Position
                    StartOfBlock.X := StartOfBlock.X - SpaceCount2;             //GBN 2002/04/12
                    EndOfBlock := CaretXY;
                    EndOfBlock.X := EndOfBlock.X - 1;
                    //The added whitespace
                    fUndoList.AddChange(crWhiteSpaceAdd, EndOfBlock, StartOfBlock, '',
                      smNormal);
                    StartOfBlock.X := StartOfBlock.X + SpaceCount2;             //GBN 2002/04/12

                    fUndoList.AddChange(crInsert, StartOfBlock, CaretXY, '',
                      smNormal);
                  finally
                    EndUndoBlock;
                  end;
                end else begin
                  fUndoList.AddChange(crInsert, StartOfBlock, CaretXY, '',
                    smNormal);
                end;
              end else begin
// Processing of case character covers on LeadByte.
                counter := 1;
{$IFDEF SYN_MBCSSUPPORT}
                if (ByteType(Temp, CaretX) = mbLeadByte) then begin
                  Inc(counter);
                end;
{$ENDIF}
                Helper := Copy(Temp, CaretX, counter);
                Temp[CaretX] := AChar;
{$IFDEF SYN_MBCSSUPPORT}
                if (counter > 1) then begin
                  Temp[CaretX + 1] := #32;
                end;
{$ENDIF}
                CaretNew := Point((CaretX + counter), CaretY);
                TrimmedSetLine(CaretY - 1, Temp);
                fUndoList.AddChange(crInsert, StartOfBlock, CaretNew, Helper,
                  smNormal);
                CaretX := CaretX + 1;
              end;
              if CaretX >= LeftChar + fCharsInWindow then
                LeftChar := LeftChar + Min(25, fCharsInWindow - 1);
            finally
              if bChangeScroll then Exclude(fOptions, eoScrollPastEol);
            end;
          end;
{begin}                                                                         //Fiala
          Caret := CaretXY;
          if fWordWrap then begin
            { insert char something inside line }
            if (Length(Lines[Caret.y-1])+1 >= fCharsInWindow)
                and (Caret.x < fCharsInWindow) and (Caret.y > 1)
            then begin
              i := Length(Lines[Caret.y - 2]);
              TSynEditStringList(fLines).ReWrapLine(Caret.y-1);
              Caret.x := Caret.x + i - Length(Lines[Caret.y - 2]);
              CaretXY := Caret;
              InvalidateLines(-1, -1);
              InvalidateGutterLines(-1, -1);
            end;
            { insert char to end of line }
            if Caret.x >= Length(Lines[Caret.y-1]) then begin
              TSynEditStringList(fLines).ReWrapLine(Caret.y-1);
              while Caret.x - 1 > Length(Lines[Caret.y-1]) do begin
                Caret.x := Caret.x - Length(Lines[Caret.y-1]);
                Inc(Caret.y);
              end;
              LeftChar := 1;
              CaretXY := Caret;
              InvalidateGutterLines(-1, -1);
            end;
          end;
{end}                                                                           //Fiala
          DoOnPaintTransient(ttAfter);                                          //GBN 2001-10-23
        end;
      ecUpperCase,
      ecLowerCase,
      ecToggleCase,
      ecTitleCase:
        if not ReadOnly then DoCaseChange(Command);                             //DDH 2002-02-11
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
          if BookMarkOptions.EnableKeys then begin
            CX := Command - ecSetMarker0;
            if Assigned(Data) then
              Caret := TPoint(Data^)
            else
              Caret := CaretXY;
            if assigned(fBookMarks[CX]) then begin
              moveBkm := (fBookMarks[CX].Line <> Caret.y);
              ClearBookMark(CX);
              if moveBkm then
                SetBookMark(CX, Caret.x, Caret.y);
            end else
              SetBookMark(CX, Caret.x, Caret.y);
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
          if (CaretY < TopLine) or (CaretY >= TopLine + LinesInWindow) then
            // If the caret is not in view then, like the Delphi editor, move
            // it in view and do nothing else
            EnsureCursorPosVisible                                              //jr 2002-04-26
          else begin
            if Command = ecScrollUp then begin
              TopLine := TopLine - 1;
              if CaretY > TopLine + LinesInWindow - 1 then
                MoveCaretVert((TopLine + LinesInWindow - 1) - CaretY, False);   //jr 2002-04-26
            end
            else begin
              TopLine := TopLine + 1;
              if CaretY < TopLine then
                MoveCaretVert(TopLine - CaretY, False);                         //jr 2002-04-26
            end;
            EnsureCursorPosVisible;                                             //jr 2002-04-27
            Update;
          end;
        end;
      ecScrollLeft:
        begin
          LeftChar := LeftChar - 1;
          // FIXME: The following code was commented out because it is not      //jr 2002-04-27
          // MBCS or hard-tab safe.
          //if CaretX > LeftChar + CharsInWindow then
          //  CaretX := LeftChar + CharsInWindow;
          Update;
        end;
      ecScrollRight:
        begin
          LeftChar := LeftChar + 1;
          // FIXME: The following code was commented out because it is not      //jr 2002-04-27
          // MBCS or hard-tab safe.
          //if CaretX < LeftChar then
          //  CaretX := LeftChar;
          Update;
        end;
      ecInsertMode:
        begin
          InsertMode := TRUE;
        end;
      ecOverwriteMode:
        begin
          InsertMode := FALSE;
        end;
      ecToggleMode:
        begin
          InsertMode := not InsertMode;
        end;
      ecBlockIndent:
        if not ReadOnly then DoBlockIndent;
      ecBlockUnindent:
        if not ReadOnly then DoBlockUnindent;
      ecNormalSelect,
      ecColumnSelect,
      ecLineSelect:
        begin
          if WordWrap and (Command = ecColumnSelect)                            //Fiala
            then Command := ecNormalSelect;                                     //Fiala
          SelectionMode := SEL_MODE[Command];
        end;
      ecContextHelp:                                                             // jj 2001-07-19
        begin
          if Assigned (fOnContextHelp) then
            fOnContextHelp (self,WordAtCursor);
        end;
{$IFDEF SYN_MBCSSUPPORT}
      ecImeStr:
        if not ReadOnly then begin
          SetString(s, PChar(Data), StrLen(Data));
          if SelAvail then begin
            BeginUndoBlock;                                                     //Fiala 2001-12-17
            fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, Helper,
              smNormal);
            StartOfBlock := fBlockBegin;
            SetSelText(s);
            fUndoList.AddChange(crInsert, fBlockBegin, fBlockEnd, Helper,
              smNormal);
            EndUndoBlock;                                                       //Fiala 2001-12-17
            InvalidateGutterLines(-1, -1);                                      //Fiala 2001-12-17
          end else begin
            Temp := LineText;
            Len := Length(Temp);
            if Len < CaretX then
              Temp := Temp + StringOfChar(#32, CaretX - Len);
            bChangeScroll := not (eoScrollPastEol in fOptions);
            try
              if bChangeScroll then Include(fOptions, eoScrollPastEol);
              StartOfBlock := CaretXY;
// Processing of case character covers on LeadByte.
              Len := Length(s);
              if not fInserting then begin
                i := (CaretX + Len);
                if (ByteType(Temp, i) = mbTrailByte) then begin
                  s := s + Temp[i - 1];
                  Helper := Copy(Temp, CaretX, Len - 1);
                end else
                  Helper := Copy(Temp, CaretX, Len);
                Delete(Temp, CaretX, Len);
              end;
              Insert(s, Temp, CaretX);
              CaretX := (CaretX + Len);
              TrimmedSetLine(CaretY - 1, Temp);
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
{$ENDIF}
{begin}                                                                         //Fiala
(*      ecReplaceAll:
        if not ReadOnly then begin
          SetString(s, PChar(Data), StrLen(Data));
          BeginUndoBlock;
          OldWordWrap := fWordWrap;
          if OldWordWrap then begin
            fUndoList.AddChange(crUnWrap, CaretXY, CaretXY, '', SelectionMode);
            WordWrap := False;
          end;
          CaretXY := Point(1, 1);
          StartOfBlock := Point(1, 1);
          EndOfBlock   := Point(Length(fLines.Strings[fLines.Count - 1]) + 1, fLines.Count);
          fUndoList.AddChange(crDeleteAll, StartOfBlock, EndOfBlock, fLines.Text,
            smNormal);
          SelectAll;
          SetSelText(s);
          EndOfBlock   := Point(Length(fLines.Strings[fLines.Count - 1]) + 1, fLines.Count);
          fUndoList.AddChange(crPaste, StartOfBlock, EndOfBlock, Helper,
            smNormal);
          if OldWordWrap then begin
            WordWrap := True;
            fUndoList.AddChange(crWrap, CaretXY, CaretXY, '', smNormal);
          end;
          EndUndoBlock;
          InvalidateGutterLines(-1, -1);
        end*)
{end}                                                                           //Fiala

    end;
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.DoOnCommandProcessed(Command: TSynEditorCommand;
  AChar: char; Data: pointer);
begin
  if Assigned(fOnCommandProcessed) then
    fOnCommandProcessed(Self, Command, AChar, Data);
end;

procedure TCustomSynEdit.DoOnProcessCommand(var Command: TSynEditorCommand;
  var AChar: char; Data: pointer);
begin
  if Command < ecUserFirst then begin
    if Assigned(FOnProcessCommand) then
      FOnProcessCommand(Self, Command, AChar, Data);
  end else begin
    if Assigned(FOnProcessUserCommand) then
      FOnProcessUserCommand(Self, Command, AChar, Data);
  end;
end;

procedure TCustomSynEdit.ClearAll;
begin
  Lines.Clear;
{begin}                                                                         //Fiala 2001-12-17
  fMarkList.Clear;
  fUndoList.Clear;
  Modified := False;
{end}                                                                           //Fiala 2001-12-17
end;

procedure TCustomSynEdit.ClearSelection;
begin
  if SelAvail then
    SelText := '';
end;

function TCustomSynEdit.NextWordPos: TPoint;
var
  CX, CY, LineLen, MultiPos: integer;
  Line: string;
  IdentChars, WhiteChars: TSynIdentChars;
begin
  CX := CaretX;
  CY := CaretY;

  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then begin
    Line := Lines[CY - 1];

    PrepareIdentChars(IdentChars, WhiteChars);

    LineLen := Length(Line);
    if CX >= LineLen then begin
      // find first IdentChar or multibyte char in the next line
      if CY < Lines.Count then begin
        Line := Lines[CY];
        Inc(CY);
{$IFDEF SYN_MBCSSUPPORT}
        MultiPos := StrScanForMultiByteChar(Line, 1);
{$ELSE}
        MultiPos := 0;
{$ENDIF}
        CX := StrScanForCharInSet(Line, 1, IdentChars);
        // stop on any multibyte chars
        if (MultiPos > 0) and ((CX = 0) or (CX > MultiPos)) then
          CX := MultiPos;
        if CX = 0 then
          Inc(CX);
      end;
    end else begin
{$IFDEF SYN_MBCSSUPPORT}
      MultiPos := StrScanForMultiByteChar(Line, CX + 1);
      // find next "whitespace" if current char is an IdentChar
      if (Line[CX] in IdentChars) and (ByteType(Line, CX) = mbSingleByte) then
        CX := StrScanForCharInSet(Line, CX, WhiteChars);
{$ELSE}
      MultiPos := 0;
      // find next "whitespace" if current char is an IdentChar
      if Line[CX] in IdentChars then
        CX := StrScanForCharInSet(Line, CX, WhiteChars);
{$ENDIF}
      // if "whitespace" found, find the next IdentChar
      if CX > 0 then
        CX := StrScanForCharInSet(Line, CX, IdentChars);
      // stop on any multibyte chars
      if (MultiPos > 0) and ((CX = 0) or (CX > MultiPos)) then
        CX := MultiPos;
      // if one of those failed just position at the end of the line
      if CX = 0 then
        CX := LineLen + 1;
    end;
  end;
  Result := Point(CX, CY);
end;

function TCustomSynEdit.WordStart: TPoint;
var
  CX, CY: integer;
  Line: string;
  IdentChars, WhiteChars: TSynIdentChars;
begin
  CX := CaretX;
  CY := CaretY;
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then begin
    Line := Lines[CY - 1];
    CX := Min(CX, Length(Line) + 1);

    PrepareIdentChars(IdentChars, WhiteChars);

    if CX > 1 then begin  // only find previous char, if not already on start of line
      // if previous char isn't a "whitespace" search for the last IdentChar
      if not (Line[CX - 1] in WhiteChars) then
        CX := StrRScanForCharInSet(Line, CX - 1, WhiteChars) + 1;
    end;
  end;
  Result := Point(CX, CY);
end;

function TCustomSynEdit.WordEnd: TPoint;
var
  CX, CY: integer;
  Line: string;
  IdentChars, WhiteChars: TSynIdentChars;
begin
  CX := CaretX;
  CY := CaretY;
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then
  begin
    Line := Lines[CY - 1];

    PrepareIdentChars(IdentChars, WhiteChars);

    CX := StrScanForCharInSet(Line, CX, WhiteChars);
    // if no "whitespace" is found just position at the end of the line
    if CX = 0 then
      CX := Length(Line) + 1;
  end;
  Result := Point(CX,CY);
end;

function TCustomSynEdit.PrevWordPos: TPoint;
var
  CX, CY, MultiPos: integer;
  Line: string;
  IdentChars, WhiteChars: TSynIdentChars;
begin
  CX := CaretX;
  CY := CaretY;
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then begin
    Line := Lines[CY - 1];
    CX := Min(CX, Length(Line) + 1);

    PrepareIdentChars(IdentChars, WhiteChars);

    if CX <= 1 then begin
      // find last IdentChar in the previous line
      if CY > 1 then begin
        Dec(CY);
        Line := Lines[CY - 1];
        CX := Length(Line) + 1;
      end;
    end else begin
{$IFDEF SYN_MBCSSUPPORT}
      MultiPos := StrRScanForMultiByteChar(Line, CX - 1);
      // if previous char is a "whitespace" search for the last IdentChar
      if (Line[CX - 1] in WhiteChars) and (ByteType(Line, CX - 1) = mbSingleByte) then
        CX := StrRScanForCharInSet(Line, CX - 1, IdentChars);
{$ELSE}
      MultiPos := 0;
      // if previous char is a "whitespace" search for the last IdentChar
      if Line[CX - 1] in WhiteChars then
        CX := StrRScanForCharInSet(Line, CX - 1, IdentChars);
{$ENDIF}
      if CX > 0 then
        // search for the first IdentChar of this "word"
        CX := StrRScanForCharInSet(Line, CX - 1, WhiteChars) + 1;
      // stop on any multibyte chars
      if (MultiPos > 0) and ((CX = 0) or (CX < MultiPos)) then
        CX := MultiPos;
      if CX = 0 then begin
        // else just position at the end of the previous line
        if CY > 1 then begin
          Dec(CY);
          Line := Lines[CY - 1];
          CX := Length(Line) + 1;
        end else
          CX := 1;
      end;
    end;
  end;
  Result := Point(CX, CY);
end;

procedure TCustomSynEdit.SetSelectionMode(const Value: TSynSelectionMode);
begin
  if FSelectionMode <> Value then begin
    FSelectionMode := Value;
    if SelAvail then
      Invalidate;
    StatusChanged([scSelection]);
  end;
end;

{begin}                                                                         //sbs 2000-11-19
procedure TCustomSynEdit.BeginUndoBlock;
begin
  fUndoList.BeginBlock;
end;
{end}                                                                           //sbs 2000-11-19

procedure TCustomSynEdit.BeginUpdate;
begin
  IncPaintLock;
end;

{begin}                                                                         //sbs 2000-11-19
procedure TCustomSynEdit.EndUndoBlock;
begin
  fUndoList.EndBlock;
end;
{end}                                                                           //sbs 2000-11-19

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

function TCustomSynEdit.GetSelStart: integer;                                   //DDH 10/16/01 Begin

function llen(data: string): integer;
  begin
    result := length(Data) + 2;
  end;
var
  loop: integer;
  x, y: integer;
begin
  if SelAvail then
  begin
    x := BlockBegin.X;
    y := BlockBegin.Y;
  end
  else
  begin
    x := CaretX;
    y := CaretY;
  end;

  result := 0;
  loop := 0;
  while loop < (Y - 1) do
  begin
    Result := result + llen(lines.strings[loop]);
    inc(loop);
  end;

  result := result + Min(X, length(lines.strings[loop]) + 1);
end;

procedure TCustomSynEdit.PrepareIdentChars(var IdentChars,
  WhiteChars: TSynIdentChars);
var WordBreakChars: TSynIdentChars;
begin
  if Assigned(Highlighter) then
  begin
    IdentChars := Highlighter.IdentChars;
    WordBreakChars := Highlighter.WordBreakChars;
  end else begin
    IdentChars := [#33..#255];
    WordBreakChars := TSynWordBreakChars;
  end;
  IdentChars := IdentChars - WordBreakChars;
  WhiteChars := [#1..#255] - IdentChars;
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
      {$ELSE}
        Windows.DestroyCaret;
      {$ENDIF}
      end;
    end;
  end;
end;

procedure TCustomSynEdit.SetSelStart(const Value: integer);

function llen(data: string): integer;
  begin
    result := length(Data) + 2;
  end;
var
  loop: integer;
  count: integer;
begin
  loop := 0;
  count := 0;
  while (count + llen(lines.strings[loop]) < value) do begin
    count := count + llen(lines.strings[loop]);
    inc(loop);
  end;
{  CaretX := value - count;
  CaretY := loop + 1;

  fBlockBegin.X := CaretX;
  fBlockBegin.Y := CaretY;}

  //This seems the same as above, but uses the other fixes inside of SetCaretXY
  //to adjust the cursor pos correctly.
  CaretXY := Point(value - count, loop + 1);                                    // awb 2002-01-12
  BlockBegin := CaretXY;                                                        // awb 2002-01-12
end;

function TCustomSynEdit.GetSelEnd: integer;

function llen(data: string): integer;
  begin
    result := length(Data) + 2;
  end;
var
  loop: integer;
  x, y: integer;
begin
  if SelAvail then
  begin
    x := BlockEnd.x;
    y := BlockEnd.y;
  end else begin
    x := CaretX;
    y := CaretY;
  end;

  result := 0;
  loop := 0;
  while loop < (y - 1) do begin
    Result := result + llen(lines.strings[loop]);
    inc(loop);
  end;
  result := result + x;
end;

procedure TCustomSynEdit.SetSelEnd(const Value: integer);

function llen(data: string): integer;
  begin
    result := length(Data) + 2;
  end;
var
  p: tpoint;
  loop: integer;
  count: integer;
begin
  loop := 0;
  count := 0;
  while (count + llen(lines.strings[loop]) < value) do begin
    count := count + llen(lines.strings[loop]);
    inc(loop);
  end;
  p.X := value - count; p.y := loop + 1;
  Blockend := p;
end;                                                                            //DDH 10/16/01 End

procedure TCustomSynEdit.SetSelWord;
begin
  SetWordBlock(CaretXY);
end;

procedure TCustomSynEdit.SetExtraLineSpacing(const Value: integer);
begin
  fExtraLineSpacing := Value;
  FontChanged(self);
end;

function TCustomSynEdit.GetBookMark(BookMark: integer; var X, Y: integer):
  boolean;
var
  i: integer;
begin
  Result := false;
  if assigned(Marks) then
    for i := 0 to Marks.Count - 1 do
      if Marks[i].IsBookmark and (Marks[i].BookmarkNumber = BookMark) then begin
        X := Marks[i].Column;
        Y := Marks[i].Line;
        Result := true;
        Exit;
      end;
end;

function TCustomSynEdit.IsBookmark(BookMark: integer): boolean;
var
  x, y: integer;
begin
  Result := GetBookMark(BookMark, x, y);
end;

procedure TCustomSynEdit.ClearUndo;
begin
  fUndoList.Clear;
  fRedoList.Clear;
end;

procedure TCustomSynEdit.SetSelTextExternal(const Value: string);
var
  StartOfBlock, EndOfBlock: TPoint;
begin
{begin}                                                                         //mh 2000-11-20
  BeginUndoBlock;
  try
    if SelAvail then begin
      fUndoList.AddChange({crSelDelete} crDelete, fBlockBegin, fBlockEnd,
        GetSelText, SelectionMode);
    end;
    StartOfBlock := minPoint(fBlockBegin, fBlockEnd);
    EndOfBlock := maxPoint(fBlockBegin, fBlockEnd);
    fBlockBegin := StartOfBlock;
    fBlockEnd := EndOfBlock;
    LockUndo;
    SetSelText(Value);
    UnlockUndo;
    fUndoList.AddChange(crInsert, StartOfBlock, BlockEnd, SelText, SelectionMode); //jcr 2002-04-13 changed smNormal override to SelctionMode
  finally
    EndUndoBlock;
  end;
{end}                                                                           //mh 2000-11-20
end;

procedure TCustomSynEdit.SetGutter(const Value: TSynGutter);
begin
  fGutter.Assign(Value);
end;

procedure TCustomSynEdit.GutterChanged(Sender: TObject);
var
  nW: integer;
begin
  if not (csLoading in ComponentState) then begin
    if fGutter.ShowLineNumbers and fGutter.AutoSize then
      fGutter.AutoSizeDigitCount(Lines.Count);
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
  fRedoList.Lock
end;

procedure TCustomSynEdit.UnlockUndo;
begin
  fUndoList.Unlock;
  fRedoList.Unlock;
end;

{$IFDEF SYN_CLX}
{$ELSE}
procedure TCustomSynEdit.WMMouseWheel(var Msg: TMessage);
var
  nDelta: integer;
  nWheelClicks: integer;
{$IFNDEF SYN_COMPILER_4_UP}
const
  LinesToScroll = 3;
  WHEEL_DELTA = 120;
  WHEEL_PAGESCROLL = MAXDWORD;
{$ENDIF}
begin
  if csDesigning in ComponentState then
    exit;

  if GetKeyState(VK_CONTROL) >= 0 then
{$IFDEF SYN_COMPILER_4_UP}
    nDelta := Mouse.WheelScrollLines
{$ELSE}
    nDelta := LinesToScroll
{$ENDIF}
  else
    nDelta := LinesInWindow shr Ord(eoHalfPageScroll in fOptions);

  Inc(fMouseWheelAccumulator, SmallInt(Msg.wParamHi));
  nWheelClicks := fMouseWheelAccumulator div WHEEL_DELTA;
  fMouseWheelAccumulator := fMouseWheelAccumulator mod WHEEL_DELTA;
  if (nDelta = integer(WHEEL_PAGESCROLL)) or (nDelta > LinesInWindow) then
    nDelta := LinesInWindow;
  TopLine := TopLine - (nDelta * nWheelClicks);
  Update;
end;
{$ENDIF}

procedure TCustomSynEdit.SetWantTabs(const Value: boolean);
begin
  fWantTabs := Value;
end;

procedure TCustomSynEdit.SetTabWidth(Value: integer);
begin
  Value := MinMax(Value, 1{0}, 256);                                            //lt 2000-10-19
  if (Value <> fTabWidth) then begin
    fTabWidth := Value;
    TSynEditStringList(Lines).TabWidth := Value;                                //mh 2000-10-19
    Invalidate; // to redraw text containing tab chars
  end;
end;

procedure TCustomSynEdit.SelectedColorsChanged(Sender: TObject);
begin
  Invalidate;
end;

// find / replace

function TCustomSynEdit.SearchReplace(const ASearch, AReplace: string;
  AOptions: TSynSearchOptions): integer;
var
  ptStart, ptEnd: TPoint; // start and end of the search range
  ptCurrent: TPoint; // current search position
  nSearchLen, nReplaceLen, n, nFound: integer;
  nInLine: integer;
  bBackward, bFromCursor: boolean;
  bPrompt: boolean;
  bReplace, bReplaceAll: boolean;
  bEndUndoBlock: boolean;                                                       //jcr 2002-04-13
  nAction: TSynReplaceAction;
  TmpPoint : TPoint;                                                            //DDH 10/16/01

  function InValidSearchRange(First, Last: integer): boolean;
  begin
    Result := TRUE;
    case fSelectionMode of
      smNormal:
        if ((ptCurrent.Y = ptStart.Y) and (First < ptStart.X)) or
          ((ptCurrent.Y = ptEnd.Y) and (Last > ptEnd.X)) then Result := FALSE;
      smColumn:                                                                 //EK 10/16/01
        // solves bug in search/replace when smColumn mode active and no selection
        Result := (First >= ptStart.X) and (Last <= ptEnd.X) or (ptEnd.X-ptStart.X<1); //jcr 2002-04-13 This needs to be <1 not <2
    end;
  end;

begin
  if Assigned(fOnSearchEngineOverride) then
    fOnSearchEngineOverride(fTSearch);

  if not assigned(fTSearch) then
  begin
    fTSearch := TSynEditSearch.Create;
  end;

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
    if (fSelectionMode = smLine) then begin
      ptStart.X := 1;
      ptEnd.X := Length(Lines[ptEnd.Y - 1]) + 1;
    end else if (fSelectionMode = smColumn) then
      // make sure the start column is smaller than the end column
      if (ptStart.X > ptEnd.X) then begin
        nFound := ptStart.X;
        ptStart.X := ptEnd.X;
        ptEnd.X := nFound;
      end;
    // ignore the cursor position when searching in the selection
    if bBackward then ptCurrent := ptEnd else ptCurrent := ptStart;
  end else begin
    ptStart := Point(1, 1);
    ptEnd.Y := Lines.Count;
    ptEnd.X := Length(Lines[ptEnd.Y - 1]) + 1;
    if bFromCursor then
      if bBackward then ptEnd := CaretXY else ptStart := CaretXY;
    if bBackward then ptCurrent := ptEnd else ptCurrent := ptStart;
  end;
  // initialize the search engine
  fTSearch.Sensitive := ssoMatchCase in AOptions;
  fTSearch.Whole := ssoWholeWord in AOptions;
  fTSearch.Pattern := ASearch;
  // search while the current search position is inside of the search range
  nSearchLen := Length(ASearch);
  nReplaceLen := Length(AReplace);
  if bReplaceAll then IncPaintLock;
  bEndUndoBlock:= false;                                                        //jcr 2002-04-13
  if bReplaceAll and (not bPrompt) then                                         //
  begin                                                                         //
    BeginUndoBlock;                                                             //
    bEndUndoBlock:= true;                                                       //
  end;                                                                          //
  try
    while (ptCurrent.Y >= ptStart.Y) and (ptCurrent.Y <= ptEnd.Y) do begin
      nInLine := fTSearch.FindAll(Lines[ptCurrent.Y - 1]);
      if bBackward then n := Pred(fTSearch.ResultCount) else n := 0;
      // Operate on all results in this line.
      while nInLine > 0 do begin
        nFound := fTSearch.Results[n];
        if bBackward then Dec(n) else Inc(n);
        Dec(nInLine);
        // Is the search result entirely in the search range?
        if not InValidSearchRange(nFound, nFound + nSearchLen) then continue;
        Inc(Result);
        // Select the text, so the user can see it in the OnReplaceText event
        // handler or as the search result.

        //ddh the caret is set twice to make sure that both sides are visible   //DDH 10/16/01 Start
//        BlockBegin := ptCurrent;          // bug!
        ptCurrent.X := nFound;
        BlockBegin := ptCurrent;
        TmpPoint := ptCurrent;
        TmpPoint.x := 1;
        //Be sure to use the Ex version of CursorPos so that it appears in the middle if necessary
        fCaretX := TmpPoint.X;
        fCaretY := TmpPoint.Y;
        EnsureCursorPosVisibleEx(True);
        Inc(ptCurrent.X, nSearchLen);
        BlockEnd := ptCurrent;
        CaretXY := ptCurrent;                                                   //DDH 10/16/01 End
        if bBackward then CaretXY := BlockBegin else CaretXY := ptCurrent;
        // If it's a search only we can leave the procedure now.
        if not (bReplace or bReplaceAll) then exit;
        // Prompt and replace or replace all.  If user chooses to replace
        // all after prompting, turn off prompting.
        if bPrompt and Assigned(fOnReplaceText) then begin
          nAction := DoOnReplaceText(ASearch, AReplace, ptCurrent.Y, nFound);
          if nAction = raCancel then exit;
        end else
          nAction := raReplace;
        if not (nAction = raSkip) then begin
            // user has been prompted and has requested to silently replace all
            // so turn off prompting
          if nAction = raReplaceAll then begin
            if not bReplaceAll then begin
              bReplaceAll := TRUE;
              IncPaintLock;
            end;
            bPrompt := False;
            BeginUndoBlock;                                                   //jcr 2002-04-13
            bEndUndoBlock:= true;                                             //
          end;
          SetSelTextExternal(AReplace);
        end;
        // fix the caret position and the remaining results
        if not bBackward then begin
          CaretX := nFound + nReplaceLen;
          if (nSearchLen <> nReplaceLen) and (nAction <> raSkip) then
            fTSearch.FixResults(nFound, nSearchLen - nReplaceLen);

          if (fSelectionMode<>smColumn) and (CaretY=ptEnd.Y) then begin         //EK 10/16/01 begin
            ptEnd:=Point(ptEnd.X+nReplaceLen-nSearchLen, ptEnd.Y);
            BlockEnd:=ptEnd;
          end;                                                                  //EK 10/16/01 End
        end;
        if not bReplaceAll then
          exit;
      end;
      // search next / previous line
      if bBackward then Dec(ptCurrent.Y) else Inc(ptCurrent.Y);
    end;
  finally
    if bReplaceAll then DecPaintLock;
    if bEndUndoBlock then EndUndoBlock;                                         //jcr 2002-04-13
  end;
end;

{$IFDEF SYN_MBCSSUPPORT}

procedure TCustomSynEdit.MBCSGetSelRangeInLineWhenColumnSelectionMode(
  const s: string; var ColFrom, ColTo: Integer);
  // --ColFrom and ColTo are in/out parameter. their range
  //    will be from 1 to MaxInt.
  // --a range of selection means:  Copy(s, ColFrom, ColTo - ColFrom);
  //    be careful what ColTo means.
var
  Len: Integer;
begin
  Len := Length(s);
  if (0 < ColFrom) and (ColFrom <= Len) then
    if mbTrailByte = ByteType(s, ColFrom) then
      Inc(ColFrom);
  if (0 < ColTo) and (ColTo <= Len) then
    if mbTrailByte = ByteType(s, ColTo) then
      Inc(ColTo);
end;

{$ENDIF}

function TCustomSynEdit.IsPointInSelection(Value: TPoint): boolean;
var
  ptBegin, ptEnd: TPoint;
begin
  ptBegin := BlockBegin;
  ptEnd := BlockEnd;
  if (Value.Y >= ptBegin.Y) and (Value.Y <= ptEnd.Y) and
    ((ptBegin.Y <> ptEnd.Y) or (ptBegin.X <> ptEnd.X))
    then begin
    if SelectionMode = smLine then
      Result := TRUE
    else if (SelectionMode = smColumn) then begin
      if (ptBegin.X > ptEnd.X) then
        Result := (Value.X >= ptEnd.X) and (Value.X < ptBegin.X)
      else if (ptBegin.X < ptEnd.X) then
        Result := (Value.X >= ptBegin.X) and (Value.X < ptEnd.X)
      else
        Result := FALSE;
    end else
      Result := ((Value.Y > ptBegin.Y) or (Value.X >= ptBegin.X)) and
        ((Value.Y < ptEnd.Y) or (Value.X < ptEnd.X));
  end else
    Result := FALSE;
end;

procedure TCustomSynEdit.SetFocus;
begin
  if (fFocusList.Count > 0) then
  begin
    TWinControl (fFocusList.Last).SetFocus;
    exit;
  end;
  inherited;
end;

{$IFDEF SYN_CLX}
{$ELSE}
procedure TCustomSynEdit.WMSetCursor(var Msg: TWMSetCursor);
var
  ptCursor, ptLineCol: TPoint;
begin
  GetCursorPos(ptCursor);
  ptCursor := ScreenToClient(ptCursor);
  if (ptCursor.X < fGutterWidth) then
    SetCursor(Screen.Cursors[fGutter.Cursor])
  else begin
    ptLineCol.X := (LeftChar * fCharWidth + ptCursor.X - fGutterWidth - 2)
      div fCharWidth;
    ptLineCol.Y := TopLine + ptCursor.Y div fTextHeight;
    if (eoDragDropEditing in fOptions) and IsPointInSelection(ptLineCol) then
      SetCursor(Screen.Cursors[crDefault])
    else
      inherited;
  end;
end;
{$ENDIF}

procedure TCustomSynEdit.BookMarkOptionsChanged(Sender: TObject);
begin
  InvalidateGutter;
end;

procedure TCustomSynEdit.SetOptions(Value: TSynEditorOptions);
var
  bSetDrag: boolean;
  TmpBool: Boolean;
begin
  if (Value <> fOptions) then
  begin
    bSetDrag := (eoDropFiles in fOptions) <> (eoDropFiles in Value);

//mh 2000-10-19
    if not (eoScrollPastEol in Options) then
      LeftChar := LeftChar;
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;
//mh 2000-10-19

    fOptions := Value;
    // Reset column position in case Cursor is past EOL.
    if not (eoScrollPastEol in fOptions) then
      CaretX := CaretX;
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
      Paint;
    end;
  end;
end;

procedure TCustomSynEdit.SetOptionFlag(Flag: TSynEditorOption; Value: boolean);
var TmpOptions: TSynEditorOptions;
begin
  TmpOptions:=fOptions;
  if (Value <> (Flag in TmpOptions)) then
  begin
    if Value then Include(TmpOptions, Flag) else Exclude(TmpOptions, Flag);
    SetOptions(TmpOptions);
//DDH: the original SetOptionFlag had this, but I don't think it's necessary
//    EnsureCursorPosVisible;
  end;
end;

procedure TCustomSynEdit.SizeOrFontChanged(bFont: boolean);
begin
  if HandleAllocated and (fCharWidth <> 0) then begin  // DJLP - for kylix port
    fCharsInWindow := (ClientWidth - fGutterWidth - 2) div fCharWidth;
    fLinesInWindow := ClientHeight div fTextHeight;
    if bFont then begin
      if Gutter.ShowLineNumbers then
        GutterChanged(Self)
      else
        UpdateScrollbars;
      InitializeCaret;
      Exclude(fStateFlags, sfCaretChanged);
      Invalidate;
    end else begin
      UpdateScrollbars;
    end;
    Exclude(fStateFlags, sfScrollbarChanged);
{begin}                                                                         //mh 2000-10-19
    if not (eoScrollPastEol in Options) then
      LeftChar := LeftChar;
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;
{end}                                                                           //mh 2000-10-19
  end;
end;

procedure TCustomSynEdit.MoveCaretHorz(DX: integer; SelectionCommand: boolean);
var
  ptO, ptDst: TPoint;
  s: string;
  nLineLen: integer;
  bChangeY: boolean;
begin
  ptO := CaretXY;
  ptDst := ptO;
  s := LineText;
  nLineLen := Length(s);
  // only moving or selecting one char can change the line
  bChangeY := not (eoScrollPastEol in fOptions);
  if bChangeY and (DX = -1) and (ptO.X = 1) and (ptO.Y > 1) then begin
    // end of previous line
    Dec(ptDst.Y);
    ptDst.X := Length(Lines[ptDst.Y - 1]) + 1;
  end else
    if bChangeY and (DX = 1) and (ptO.X > nLineLen) and (ptO.Y < Lines.Count)
      then begin
    // start of next line
      Inc(ptDst.Y);
      ptDst.X := 1;
    end else begin
      ptDst.X := Max(1, ptDst.X + DX);
      // don't go past last char when ScrollPastEol option not set
      if (DX > 0) and bChangeY then ptDst.X := Min(ptDst.X, nLineLen + 1);
{$IFDEF SYN_MBCSSUPPORT}
      // prevent from getting inside of a doublebyte char
      if (ptDst.X > 1) and (ptDst.X <= nLineLen) then begin
        DX := ptDst.X - ptO.X;
        if (DX < 0) then begin
          if ByteType(s, ptDst.X) = mbTrailByte then Dec(ptDst.X);
        end else if (DX > 0) then begin
          if ByteType(s, ptDst.X) = mbTrailByte then Inc(ptDst.X);
        end;
      end;
    end;
{$ELSE}
    end;
{$ENDIF}
  // set caret and block begin / end
  MoveCaretAndSelection(ptO, ptDst, SelectionCommand);
end;

procedure TCustomSynEdit.MoveCaretVert(DY: integer; SelectionCommand: boolean);
var
  ptO, ptDst: TPoint;
{$IFDEF SYN_MBCSSUPPORT}
  NewStepAside: Boolean;
  s: string;
{$ENDIF}
  SaveLastCaretX: Integer;
begin
  ptO := LogicalToPhysicalPos(CaretXY);                                         // sblbg 2001-12-17
//  dec(ptO.x);                                                                   // sblbg 2001-12-17

  ptDst := ptO;
  with ptDst do begin
    Inc(Y, DY);
    if DY >= 0 then begin
      if (Y > Lines.Count) or (ptO.Y > Y) then
        Y := Lines.Count;
    end else
      if (Y < 1) or (ptO.Y < Y) then
        Y := 1;
  end;
  if (ptO.Y <> ptDst.Y) then begin
    if eoKeepCaretX in Options then                                             //mh 2000-10-19
      ptDst.X := fLastCaretX ;                                                  //mh 2000-10-19
//      ptDst.X := fLastCaretX - 1;                                               //sblbg 2001-12-19
  end;

  ptDst := PhysicalToLogicalPos(ptDst);                                         // sblbg 2001-12-17
  ptO := PhysicalToLogicalPos(ptO);                                             // sblbg 2001-12-17


{$IFDEF SYN_MBCSSUPPORT}
  if (ptO.Y <> ptDst.Y) then begin
    if fMBCSStepAside and not (eoKeepCaretX in Options) then
      Inc(ptDst.X);
    NewStepAside := False;
    s := Lines[ptDst.Y - 1];
    if (ptDst.X <= Length(s)) then
      if (ByteType(s, ptDst.X) = mbTrailByte) then begin
        NewStepAside := True;
        Dec(ptDst.X);
      end;
  end
  else
    NewStepAside := fMBCSStepAside;
{$ENDIF}
  SaveLastCaretX := fLastCaretX;

  // set caret and block begin / end
  MoveCaretAndSelection(ptO, ptDst, SelectionCommand);

  // Set fMBCSStepAside and restore fLastCaretX after moving caret, since
  // UpdateLastCaretX, called by SetCaretXYEx, changes them. This is the one
  // case where we don't want that.
{$IFDEF SYN_MBCSSUPPORT}
  fMBCSStepAside := NewStepAside;
{$ENDIF}
  fLastCaretX := SaveLastCaretX;                                                //jr 2002-04-26
end;

procedure TCustomSynEdit.MoveCaretAndSelection(ptBefore, ptAfter: TPoint;
  SelectionCommand: boolean);
begin
  if (eoGroupUndo in FOptions) then
    fUndoList.AddGroupBreak;                                                    //ek 2000-11-04

  IncPaintLock;
  if SelectionCommand then begin
    if not SelAvail then SetBlockBegin(ptBefore);
    SetBlockEnd(ptAfter);
  end else
    SetBlockBegin(ptAfter);
  CaretXY := ptAfter;
  DecPaintLock;
end;

procedure TCustomSynEdit.SetCaretAndSelection(ptCaret, ptBefore,
  ptAfter: TPoint);
begin
  IncPaintLock;
  CaretXY := ptCaret;
  SetBlockBegin(ptBefore);
  SetBlockEnd(ptAfter);
  DecPaintLock;
end;

procedure TCustomSynEdit.RecalcCharExtent;

  function UsesFontStyle(AStyle: TFontStyle): boolean;
  var
    i: integer;
  begin
    if Assigned(fHighlighter) then begin
      for i := 0 to Pred(fHighlighter.AttrCount) do
        if AStyle in fHighlighter.Attribute[i].Style then begin
          Result := TRUE;
          exit;
        end;
      Result := FALSE;
    end else
      Result := AStyle in Font.Style;
  end;

const
  BoldStyles: array[boolean] of TFontStyles = ([], [fsBold]);
  ItalicStyles: array[boolean] of TFontStyles = ([], [fsItalic]);
begin
  with fTextDrawer do begin
    BaseFont := Self.Font;
    BaseStyle := ItalicStyles[UsesFontStyle(fsItalic)];
    fTextHeight := CharHeight + fExtraLineSpacing;
    BaseStyle := BoldStyles[UsesFontStyle(fsBold)];
    fCharWidth := CharWidth;
  end;
end;

procedure TCustomSynEdit.HighlighterAttrChanged(Sender: TObject);
begin
  RecalcCharExtent;
  SizeOrFontChanged(TRUE);                                                      //jr 2000-10-01
  Invalidate;
end;

procedure TCustomSynEdit.StatusChanged(AChanges: TSynStatusChanges);
begin
  fStatusChanges := fStatusChanges + AChanges;
  if PaintLock = 0 then
    DoOnStatusChange(fStatusChanges);
end;

procedure TCustomSynEdit.DoCaseChange(const Cmd : TSynEditorCommand);           //DDH 2002-02-11
  function ToggleCase(const aStr : string) : string;
  var
    i : Integer;
    s, s1, s2 : string;
  begin
    Result := '';
    s1 := AnsiUpperCase(aStr);
    s2 := AnsiLowerCase(aStr);
    for i := 1 to Length(aStr) do
    begin
      s := aStr[i];
      if s = s1[i] then
        Result := Result + s2[i]
      else
        Result := Result + s1[i];
    end;
  end;
var
  w : string;
  oldCaret, oldBB, oldBE : TPoint;
  bNoSel : Boolean;
begin
  oldCaret := CaretXY;
  try
    case Cmd of
      ecUpperCase, ecLowerCase, ecTitleCase : begin
        w := WordAtCursor;
        if Cmd = ecUpperCase then
          w := AnsiUpperCase(w)
        else if Cmd = ecLowerCase then
          w := AnsiLowerCase(w)
        else
          w := AnsiUpperCase(Copy(w, 1, 1)) + AnsiLowerCase(Copy(w, 2, Length(w)));
          SetBlockBegin(WordStart);
          SetBlockEnd(WordEnd);
          SelText := w;
          SetBlockBegin(CaretXY);
          SetBlockend(CaretXY);
      end;
      ecToggleCase : begin
        bNoSel := False;
        oldBB := GetBlockBegin;
        oldBE := GetBlockEnd;
        if SelText = '' then
        begin
          bNoSel := True;
          SetBlockBegin(CaretXY);
          SetBlockEnd(Point(CaretX+1, CaretY));
          oldCaret.x := oldCaret.x + 1;
        end;
        w := SelText;
        w := ToggleCase(w);
        SelText := w;
        if bNoSel then
        begin
          SetBlockBegin(CaretXY);
          SetBlockEnd(CaretXY);
        end
        else
        begin
          SetBlockBegin(oldBB);
          SetBlockEnd(oldBE);
        end;
      end;
    end;
  finally
    CaretXY := oldCaret;
  end;
end;

procedure TCustomSynEdit.DoTabKey;
var
  StartOfBlock: TPoint;
  i, MinLen, iLine: integer;
  PrevLine, Spaces : string;
  p: PChar;
  NewCaretX: integer;                                                           //mh 2000-10-01
  ChangeScroll: boolean;                                                        //mh 2000-10-01
  nPhysX, nDistanceToTab, nSpacesToNextTabStop : Integer;
  OldSelTabLine: Boolean;
begin
  //GBN 2002-04-24 Provide Visual Studio like block indenting
  OldSelTabLine := SelTabLine;
  if (eoTabIndent in Options) and ((SelTabBlock) or (OldSelTabLine)) then
  begin
    DoBlockIndent;
    if OldSelTabLine then
    begin
      if fBlockBegin.X < fBlockEnd.X then
        FBlockBegin.X := 1
      else fBlockEnd.X := 1;
    end;
    exit;
  end;
  i := 0;
  iLine := 0;
  MinLen := 0;
  if eoSmartTabs in fOptions then begin
    iLine := CaretY - 1;
    if (iLine > 0) and (iLine < Lines.Count) then begin
      Dec(iLine);
      repeat
        MinLen := PhysicalToLogicalPos(
          Point(LogicalToPhysicalPos(CaretXY).x, iLine+1)).x;
        PrevLine := Lines[iLine];
        if fWordWrap then
          while (iLine > 0) and
                TSynEditStringList(fLines).IsLineWraped(iLine)
          do begin
            Dec(iLine);
            PrevLine := Lines[iLine] + PrevLine;
          end;
        if (Length(PrevLine) >= MinLen) then begin
          p := @PrevLine[MinLen];
          // scan over non-whitespaces
          repeat
            if p^ in [#9, #32] then break;
            Inc(i);
            Inc(p);
          until p^ = #0;
          // scan over whitespaces
          if p^ <> #0 then
            repeat
              if not (p^ in [#9, #32]) then break;
              Inc(i);
              Inc(p);
            until p^ = #0;
          break;
        end;
        Dec(iLine);
      until iLine < 0;
    end;
  end;
  if i = 0 then begin
    if (eoTabsToSpaces in fOptions) then begin
      i := TabWidth - (CaretX - 1) mod TabWidth;
      if i = 0 then i := TabWidth;
    end else i := TabWidth;
  end;

  fUndoList.BeginBlock;
  try
    if SelAvail then begin
      fUndoList.AddChange( crDelete, fBlockBegin, fBlockEnd, SelText,
        SelectionMode );
      SetSelText( '' );
    end;
    StartOfBlock := CaretXY;

    if eoTabsToSpaces in fOptions then begin
      Spaces := StringOfChar(#32, i);
    end
    else begin
      if eoSmartTabs in fOptions then begin
        i := LogicalToPhysicalPos(Point(MinLen+i, iLine+1)).x;
        nPhysX := LogicalToPhysicalPos(CaretXY).x;
        nDistanceToTab := i - nPhysX;
        nSpacesToNextTabStop := TabWidth - ((nPhysX - 1) mod TabWidth);
        if nSpacesToNextTabStop <= nDistanceToTab then begin
          Spaces := TSynTabChar;
          Dec(nDistanceToTab, nSpacesToNextTabStop);
        end;
        while nDistanceToTab >= TabWidth do begin
          Spaces := Spaces + TSynTabChar;
          Dec(nDistanceToTab, TabWidth);
        end;
        if nDistanceToTab > 0 then
          Spaces := Spaces + StringOfChar(#32, nDistanceToTab);
      end
      else begin
        Spaces := TSynTabChar;
      end;
    end;
    NewCaretX := StartOfBlock.X + length(Spaces);

    SetSelText(Spaces);
    fUndoList.AddChange( crInsert, StartOfBlock, CaretXY, GetSelText,
      SelectionMode );
  finally
    fUndoList.EndBlock;
  end;

  ChangeScroll := not (eoScrollPastEol in fOptions);
  try
    Include(fOptions, eoScrollPastEol);
    CaretX := NewCaretX;
  finally
    if ChangeScroll then
      Exclude(fOptions, eoScrollPastEol);
  end;
  EnsureCursorPosVisible;
end;

//DDH 10/16/01 Start from Eden Kirin + Smart Tab delete from DDH
procedure TCustomSynEdit.DoShiftTabKey;
var
  NewX        :integer;
  OldSelMode  :TSynSelectionMode;
  Line        :string;
  LineLen     :integer;
  DestX       :integer;

  MaxLen, iLine: integer;
  PrevLine     : string;
  p: PChar;
begin
  //GBN 2002-04-24 Provide Visual Studio like block indenting
  if (eoTabIndent in Options) and ((SelTabBlock) or (SelTabLine)) then
  begin
    DoBlockUnIndent;
    exit;
  end;
    
  NewX:=CaretX;

  if (NewX <> 1) and (eoSmartTabs in fOptions) then
  begin
    iLine := CaretY - 1;
    if (iLine > 0) and (iLine < Lines.Count) then begin
      Dec(iLine);
      MaxLen := CaretX - 1;
      repeat
// NOTE after throwing in real tabs we have to use:
//      PrevLine := pConvert(Lines[iLine], TabWidth);
        PrevLine := Lines[iLine];
        if (Length(PrevLine) >= MaxLen) then begin
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
    Line:=LineText;
    LineLen:=Length(Line);

    // find real un-tab position

    DestX:=((CaretX-2) div TabWidth)*TabWidth+1;
    while (NewX>DestX) and ((NewX-1>LineLen) or (Line[NewX-1]=#32)) do
      dec(NewX);

  end;

  // perform un-tab
  if (NewX<>CaretX) then begin
    OldSelMode := fSelectionMode;
    try
      SetBlockBegin(Point(NewX,CaretY));
      SetBlockEnd(CaretXY);

      fUndoList.AddChange(crDeleteAfterCursor, Point(NewX,CaretY), CaretXY, SelText,
        smNormal);

      SetSelText('');
      CaretX:=NewX;
    finally
      fSelectionMode := OldSelMode;
    end;
  end;
end;

procedure TCustomSynEdit.DoHomeKey(Selection:boolean);
var
  newX           :integer;
  first_nonblank :integer;
  s              :string;
begin
  if (eoEnhanceHomeKey in fOptions) then begin
    s:=fLines[CaretXY.Y-1];

    first_nonblank:=1;
    while (first_nonblank<Length(s)) and (s[first_nonblank] in [#32, #9]) do
      inc(first_nonblank);
    dec(first_nonblank);

    newX:=CaretXY.X-1;

    if (newX>first_nonblank) or (newX=0) then
      newX:=first_nonblank+1
    else
      newX:=1;
  end else
    newX:=1;

  MoveCaretAndSelection(CaretXY, Point(newX, CaretY), Selection);
end;

//DDH 10/16/01 End from Eden Kirin

{$IFDEF SYN_CLX}
{$ELSE}
procedure TCustomSynEdit.CreateWnd;
begin
  inherited;
  if (eoDropFiles in fOptions) and not (csDesigning in ComponentState) then
    DragAcceptFiles(Handle, TRUE);
end;

procedure TCustomSynEdit.DestroyWnd;
begin
  if (eoDropFiles in fOptions) and not (csDesigning in ComponentState) then
    DragAcceptFiles(Handle, FALSE);
  inherited;
end;
{$ENDIF}

procedure TCustomSynEdit.DoBlockIndent;
var
  OrgCaretPos,
  BB,BE            : TPoint;
  Run,
  StrToInsert      : PChar;
  e,x,
  i,InsertStrLen   : integer;
  Spaces           : String;
  OrgSelectionMode : TSynSelectionMode;
begin
  OrgSelectionMode := fSelectionMode;
  OrgCaretPos := CaretXY;

  StrToInsert := nil;
  if SelAvail then
  try
    // keep current selection detail
    BB := BlockBegin;
    BE := BlockEnd;

    // build text to insert
    if (BE.X = 1) then begin
      e := BE.y - 1;
      x := 1;
    end else begin
      e := BE.y;
      if eoTabsToSpaces in Options then
        x := CaretX + FTabWidth
      else x := CaretX + 1;
    end;
    if (eoTabsToSpaces in Options) then
    begin
      InsertStrLen := (FTabWidth + 2) * (e - BB.y) + FTabWidth + 1;
      //               chars per line * lines-1    + last line + null char
      StrToInsert := StrAlloc(InsertStrLen);
      Run := StrToInsert;
      Spaces := StringOfChar(#32, FTabWidth);
    end else begin
      InsertStrLen:= 3 * (e - BB.y) + 2;
      //         #9#13#10 * lines-1 + (last line's #9 + null char)
      StrToInsert := StrAlloc(InsertStrLen);
      Run := StrToInsert;
      Spaces := #9;
    end;
    for i := BB.Y to e-1 do
    begin
      StrPCopy(Run, Spaces+#13#10);
      Inc(Run,length(spaces)+2);
    end;
    StrPCopy(Run, Spaces);

    fUndoList.BeginBlock;
    fUndoList.AddChange(crIndent, BB, BE, '', smColumn);
    InsertBlock(Point(1,BB.y),Point(1,BB.y),StrToInsert);
    //We need to save the position of the end block for redo
    fUndoList.AddChange(crIndent, Point(BB.x + length(Spaces), BB.y), Point(BE.x + length(Spaces), BE.y), '', smColumn);
    fUndoList.EndBlock;

    //adjust the x position of orgcaretpos appropriately
    OrgCaretPos.X := X;
  finally
    StrDispose(StrToInsert);
    fSelectionMode := OrgSelectionMode;
    SetCaretAndSelection(OrgCaretPos, Point(BB.x + Length(Spaces), BB.y),
      Point(BE.x + length(Spaces), BE.y));
  end;
end;

procedure TCustomSynEdit.DoBlockUnindent;
var
  OrgCaretPos,
  BB, BE: TPoint;
  Line, Run,
  FullStrToDelete,
  StrToDelete: PChar;
  Len,
  x, StrToDeleteLen,
  i, TmpDelLen,
  FirstIndent,
  LastIndent,
  e : integer;
  TempString: String;
  OrgSelectionMode : TSynSelectionMode;
  SomethingToDelete : Boolean;

  function GetDelLen : integer;
  var
    Run : PChar;
    ExitEarly : Boolean;
  begin
    Result := 0;
    Run := Line;
    //GBN 2002/04/10 Take care of tab character
    if (Run[0]=#9) then
    begin
      Result:=1;
      SomethingToDelete := True;
      exit;
    end;
    ExitEarly := False;
    //Deal with compound tabwidths  Sometimes they have TabChars after a few
    //spaces, yet we need to delete the whole tab width even though the char
    //count might not be FTabWidth because of the TabChar
    while (Run[0] in [#32, #9]) and (Result < FTabWidth) do
    begin
      if Run[0] = #9 then ExitEarly := True;
      Inc(Result);
      Inc(Run);
      SomethingToDelete := True;
      if ExitEarly then Exit;
    end;
  end;

begin
  OrgSelectionMode := fSelectionMode;
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
    if BE.X = 1 then
      e := BE.y - 1
    else
      e := BE.y;

    // build string to delete
    StrToDeleteLen := (FTabWidth + 2) * (e - BB.y) + FTabWidth + 1;
    //                chars per line * lines-1    + last line + null char
    StrToDelete := StrAlloc(StrToDeleteLen);
    StrToDelete[0] := #0;
    SomethingToDelete := False;
    for i := BB.Y to e-1 do
    begin
       Line := PChar(Lines[i-1]);
       //Instead of doing a StringOfChar, we need to get *exactly* what was
       //being deleted incase there is a TabChar
       TmpDelLen := GetDelLen;
       StrCat(StrToDelete,PChar(Copy(Line, 1, TmpDelLen)));
       StrCat(StrToDelete, PChar(#13#10));
       if (fCaretY = i) and (x <> 1) then
         x := x - TmpDelLen;
    end;
    Line := PChar(Lines[e-1]);
    TmpDelLen := GetDelLen;
    StrCat(StrToDelete,PChar(Copy(Line, 1, TmpDelLen)));
    if (fCaretY = e) and (x <> 1) then
      x := x - TmpDelLen;

    FirstIndent := -1;
    // Delete string
    if SomethingToDelete then
    begin
      FullStrToDelete := StrToDelete;
      CaretY := BB.Y;
      repeat
        Run := GetEOL(StrToDelete);
        if Run <> StrToDelete then
        begin
          Len := Run - StrToDelete;
          if FirstIndent = -1 then
            FirstIndent := Len;
          TempString := Lines[CaretY - 1];
          if Len > 0 then
            Delete(TempString, 1, Len);
          Lines[CaretY - 1] := TempString;
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
      fUndoList.AddChange(crUnindent, BB, BE, FullStrToDelete, smColumn);
    end;
    // restore selection
    fSelectionMode := OrgSelectionMode;
    if FirstIndent = -1 then
      FirstIndent := 0;

    //adjust the x position of orgcaretpos appropriately
    OrgCaretPos.X := X;

    SetCaretAndSelection(OrgCaretPos, Point(BB.x - FirstIndent, BB.Y), Point(BE.x - LastIndent, BE.y));
  end;
end;

{$IFDEF SYN_COMPILER_4_UP}
function TCustomSynEdit.ExecuteAction(Action: TBasicAction): boolean;
begin
  if Action is TEditAction then
  begin
    Result := TRUE;
    if Action is TEditCut then
      CutToClipboard
    else if Action is TEditCopy then
      CopyToClipboard
    else if Action is TEditPaste then
      PasteFromClipboard
{$IFDEF SYN_COMPILER_5_UP}
    else if Action is TEditDelete then
      ClearSelection
{$IFDEF SYN_CLX}
{$ELSE}
    else if Action is TEditUndo then
      Undo
{$ENDIF}
    else if Action is TEditSelectAll then
      SelectAll;
{$ENDIF}
  end else
    Result := inherited ExecuteAction(Action);
end;

function TCustomSynEdit.UpdateAction(Action: TBasicAction): boolean;
begin
  if Action is TEditAction then
  begin
    Result := Focused;
    if Result then
    begin
      if (Action is TEditCut) or (Action is TEditCopy) then
        TEditAction(Action).Enabled := SelAvail
      else if Action is TEditPaste then
        TEditAction(Action).Enabled := CanPaste
{$IFDEF SYN_COMPILER_5_UP}
      else if Action is TEditDelete then
        TEditAction(Action).Enabled := TRUE
{$IFDEF SYN_CLX}
{$ELSE}
      else if Action is TEditUndo then
        TEditAction(Action).Enabled := CanUndo
{$ENDIF}
      else if Action is TEditSelectAll then
        TEditAction(Action).Enabled := TRUE;
{$ENDIF}
    end;
  end else
    Result := inherited UpdateAction(Action);
end;
{$ENDIF}

procedure TCustomSynEdit.SetModified(Value: boolean);
begin
  if Value <> fModified then begin
    fModified := Value;
    StatusChanged([scModified]);
  end;
end;

function TCustomSynEdit.DoOnSpecialLineColors(Line: integer; var Foreground,
  Background: TColor): boolean;
begin
  Result := FALSE;
  if Assigned(fOnSpecialLineColors) then
    fOnSpecialLineColors(Self, Line, Result, Foreground, Background);
end;

procedure TCustomSynEdit.InvalidateLine(Line: integer);
var
  rcInval: TRect;
begin
  if Visible and (Line >= TopLine) and (Line <= TopLine + LinesInWindow) and
     (Line <= Lines.Count) and HandleAllocated
  then begin
    // we invalidate gutter and text area of this line
    rcInval := Rect(0, fTextHeight * (Line - TopLine), ClientWidth, 0);
    rcInval.Bottom := rcInval.Top + fTextHeight;
    if sfLinesChanging in fStateFlags then
      UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
    else
{$IFDEF SYN_CLX}
      InvalidateRect(rcInval, False);
{$ELSE}
      InvalidateRect(Handle, @rcInval, False);
{$ENDIF}
  end;
end;

function TCustomSynEdit.GetReadOnly: boolean;
begin
  Result := fReadOnly;
end;

procedure TCustomSynEdit.SetReadOnly(Value: boolean);
begin
  if fReadOnly <> Value then begin
    fReadOnly := Value;
    StatusChanged([scReadOnly]);
  end;
end;

procedure TCustomSynEdit.FindMatchingBracket;                                   //GBN 2001-10-23
var P: TPoint;
begin
  P := GetMatchingBracket;
  if (P.X>0) and (P.Y>0) then CaretXY := P;
end;

function TCustomSynEdit.GetMatchingBracket: TPoint;
begin
  Result := GetMatchingBracketEx(Point(CaretX, CaretY), False);
end;

function TCustomSynEdit.GetMatchingBracketEx(APoint: TPoint;
                                             AdjustForTabs: Boolean): TPoint;   //DDH 2001-10-23
const
  Brackets: array[0..7] of char = ('(', ')', '[', ']', '{', '}', '<', '>');     //DDH 10/17/01 from Flávio Etrusco

  procedure AdjustPosForTabs(VAR XPos: Integer; YPos: Integer);
  var i : Integer;
      TmpStr : String;
  begin
    TmpStr := Copy(Lines[YPos - 1], 1, XPos);
    i := pos(TSynTabChar, TmpStr);
    While i > 0 do
    begin
      inc(XPos, (TabWidth - 1));
      TmpStr := Copy(TmpStr, i + 1, length(TmpStr));
      i := pos(TSynTabChar, TmpStr);
    end;
  end;

var
  Line: string;
  i, PosX, PosY, Len: integer;
  Test, BracketInc, BracketDec: char;
  NumBrackets: integer;
  dumbstr:string;
  attr:TSynHighlighterAttributes;
  p:TPoint;
  isCommentOrString:boolean;
begin
  Result.X := 0;
  Result.Y := 0;
  // get char at caret
  PosX := APoint.X;
  PosY := APoint.Y;
  Line := LineText;
  if Length(Line) >= PosX then begin
    Test := Line[PosX];
    // is it one of the recognized brackets?
    for i := Low(Brackets) to High(Brackets) do
      if Test = Brackets[i] then begin
        // this is the bracket, get the matching one and the direction
        BracketInc := Brackets[i];
        BracketDec := Brackets[i xor 1]; // 0 -> 1, 1 -> 0, ...
        // search for the matching bracket (that is until NumBrackets = 0)
        NumBrackets := 1;
        if Odd(i) then begin
          repeat
            // search until start of line
            while PosX > 1 do begin
              Dec(PosX);
              Test := Line[PosX];
              p.x := PosX;
              p.y := PosY;
              if (Test = BracketInc) or (Test = BracketDec) then
              begin
                if GetHighlighterAttriAtRowCol(p, dumbstr, attr) then
                  isCommentOrString:=
                   (attr = Highlighter.StringAttribute) or (attr=Highlighter.CommentAttribute)
                else isCommentOrString:=false;
                if (Test = BracketInc) and (not isCommentOrString) then
                  Inc(NumBrackets)
                else if (Test = BracketDec) and (not isCommentOrString) then
                begin
                  Dec(NumBrackets);
                  if NumBrackets = 0 then
                  begin
                    // matching bracket found, set caret and bail out
                    if AdjustForTabs then
                      AdjustPosForTabs(PosX,PosY);
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
          until FALSE;
        end else begin
          repeat
            // search until end of line
            Len := Length(Line);
            while PosX < Len do begin
              Inc(PosX);
              Test := Line[PosX];
              p.x:=PosX;
              p.y:=PosY;
              if (Test = BracketInc) or (Test = BracketDec) then
              begin
                if GetHighlighterAttriAtRowCol(p,dumbstr,attr) then
                  isCommentOrString:=
                    (attr=Highlighter.StringAttribute) or (attr=Highlighter.CommentAttribute)
                else isCommentOrString:=false;
                if (Test = BracketInc) and (not isCommentOrString) then
                  Inc(NumBrackets)
                else if (Test = BracketDec)and (not isCommentOrString) then
                begin
                  Dec(NumBrackets);
                  if NumBrackets = 0 then
                  begin
                    // matching bracket found, set caret and bail out
                    if AdjustForTabs then
                      AdjustPosForTabs(PosX,PosY);
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

function TCustomSynEdit.GetHighlighterAttriAtRowCol(XY: TPoint;
  var Token: string; var Attri: TSynHighlighterAttributes): boolean;
VAR TmpType, TmpStart: Integer;
begin
  Result := GetHighlighterAttriAtRowColEx(XY, Token, TmpType, TmpStart, Attri);
end;

function TCustomSynEdit.GetHighlighterAttriAtRowColEx(XY: TPoint;
  var Token: string; var TokenType, Start: Integer;
  var Attri: TSynHighlighterAttributes): boolean;                               //DDH 10/16/01 (added TokenType, Start)
var
  PosX, PosY: integer;
  Line: string;
begin
  PosY := XY.Y;
  if Assigned(Highlighter) and (PosY >= 1) and (PosY <= Lines.Count) then
  begin
    Line := Lines[PosY - 1];
//    Highlighter.SetRange(Lines.Objects[PosY - 1]);
    Highlighter.SetRange(TSynEditStringList(Lines).Ranges[PosY - 1]);
    Highlighter.SetLine(Line, PosY);
    PosX := XY.X;
    if (PosX > 0) and (PosX <= Length(Line)) then
      while not Highlighter.GetEol do begin
        Start := Highlighter.GetTokenPos + 1;
        Token := Highlighter.GetToken;
        if (PosX >= Start) and (PosX < Start + Length(Token)) then begin
          Attri := Highlighter.GetTokenAttribute;
          TokenType := Highlighter.GetTokenKind;
          Result := TRUE;
          exit;
        end;
        Highlighter.Next;
      end;
  end;
  Token := '';
  Attri := nil;
  Result := FALSE;
end;

function TCustomSynEdit.FindHookedCmdEvent(AHandlerProc: THookedCommandEvent):
  integer;
var
  Entry: THookedCommandHandlerEntry;
begin
  Result := GetHookedCommandHandlersCount - 1;
  while Result >= 0 do begin
    Entry := THookedCommandHandlerEntry(fHookedCommandHandlers[Result]);
    if Entry.Equals(AHandlerProc) then
      break;
    Dec(Result);
  end;
end;

function TCustomSynEdit.GetHookedCommandHandlersCount: integer;
begin
  if Assigned(fHookedCommandHandlers) then
    Result := fHookedCommandHandlers.Count
  else
    Result := 0;
end;

procedure TCustomSynEdit.RegisterCommandHandler(AHandlerProc:
  THookedCommandEvent; AHandlerData: pointer);
begin
  if not Assigned(AHandlerProc) then begin
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.Create('Event handler is NIL in RegisterCommandHandler');
{$ENDIF}
    exit;
  end;
  if not Assigned(fHookedCommandHandlers) then
    fHookedCommandHandlers := TList.Create;
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
  i: integer;
begin
  if not Assigned(AHandlerProc) then begin
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.Create('Event handler is NIL in UnregisterCommandHandler');
{$ENDIF}
    exit;
  end;
  i := FindHookedCmdEvent(AHandlerProc);
  if i > -1 then begin
    THookedCommandHandlerEntry(fHookedCommandHandlers[i]).Free;
    fHookedCommandHandlers.Delete(i);
  end else
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.CreateFmt('Event handler (%p, %p) is not registered',
      [TMethod(AHandlerProc).Data, TMethod(AHandlerProc).Code]);
{$ENDIF}
end;

procedure TCustomSynEdit.NotifyHookedCommandHandlers(AfterProcessing: boolean;
  var Command: TSynEditorCommand; var AChar: char; Data: pointer);
var
  Handled: boolean;
  i: integer;
  Entry: THookedCommandHandlerEntry;
begin
  Handled := FALSE;
  for i := 0 to GetHookedCommandHandlersCount - 1 do begin
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
var DoTransient: Boolean;
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

  if DoTransient and Assigned(fOnPaintTransient) then begin
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

procedure TCustomSynEdit.DoOnPaintTransient(TransientType: TTransientType);     //GBN 2001-10-23
begin
  DoOnPaintTransientEx(TransientType,false);
end;

procedure TCustomSynEdit.DoOnPaint;
begin
  if Assigned(fOnPaint) then begin
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

function TCustomSynEdit.DoOnReplaceText(const ASearch, AReplace: string;
  Line, Column: integer): TSynReplaceAction;
begin
  Result := raCancel;
  if Assigned(fOnReplaceText) then
    fOnReplaceText(Self, ASearch, AReplace, Line, Column, Result);
end;

procedure TCustomSynEdit.DoOnStatusChange(Changes: TSynStatusChanges);
begin
  if Assigned(fOnStatusChange) then begin
    fOnStatusChange(Self, fStatusChanges);
    fStatusChanges := [];
  end;
end;

procedure TCustomSynEdit.UndoRedoAdded(Sender: TObject);
var FOldModified : Boolean;                                                     //FDE: Bug Fix
begin
                                                                                //FDE 10/16/01 Start
  if (Sender = fUndoList) and (fUndoList.ItemCount = 1) and                     //FDE: Bug Fix
      not(sfInsideRedo in fStateFlags) then
  begin
    FOldModified := Modified;
  end else FOldModified := False;

  Modified := fUndoList.CanUndo or FOldModified or fUndoList.FullUndoImpossible;//FDE 10/16/01 End;

  // we have to clear the redo information, since adding undo info removes
  // the necessary context to undo earlier edit actions
  if (Sender = fUndoList) and not (sfInsideRedo in fStateFlags) and                                //mh 2000-10-30
     (fUndoList.PeekItem<>nil) and (fUndoList.PeekItem.ChangeReason<>crGroupBreak) then            //ek 2000-11-04
    fRedoList.Clear;
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

function TCustomSynEdit.GetWordAtRowCol(XY: TPoint): string;
var
  Line: string;
  IdChars: TSynIdentChars;
  Len, Stop: integer;
begin
  Result := '';
  if (XY.Y >= 1) and (XY.Y <= Lines.Count) then begin
    Line := Lines[XY.Y - 1];
    Len := Length(Line);
    if (XY.X >= 1) and (XY.X <= Len + 1) then begin
      if Assigned(Highlighter) then
        IdChars := Highlighter.IdentChars
      else
        IdChars := ['a'..'z', 'A'..'Z'];
      Stop := XY.X;
      while (Stop <= Len) and (Line[Stop] in IdChars) do
        Inc(Stop);
      while (XY.X > 1) and (Line[XY.X - 1] in IdChars) do
        Dec(XY.X);
      if Stop > XY.X then
        Result := Copy(Line, XY.X, Stop - XY.X);
    end;
  end;
end;

// LogicalToPhysicalPos takes a position in the text and transforms it into
// the row and column it appears to be on the screen
function TCustomSynEdit.LogicalToPhysicalPos(p: TPoint): TPoint;
var
  s: string;
  i, L: integer;
  x: integer;
begin
  if p.Y - 1 < Lines.Count then begin
    s := Lines[p.Y - 1];
    l := Length(s);
    x := 0;
    for i := 1 to p.x - 1 do begin
      if (i <= l) and (s[i] = TSynTabChar) then
        inc(x, TabWidth - (x mod TabWidth))
      else
        inc(x);
    end;
    p.x := x + 1;
  end;
  Result := p;
end;

// PhysicalToLogicalPos takes a position on screen and transfrom it
// into position of text
function TCustomSynEdit.PhysicalToLogicalPos(p: TPoint): TPoint;                // sblbg 2001-12-17
var
  s: string;
  i, L: integer;
  x: integer;
begin
  if p.Y <= lines.Count then begin
    s := Lines[p.Y - 1];
    l := Length(s);
    x := 0;
    i := 0;

    while x < p.X  do begin
      inc(i);
      if (i <= l) and (s[i] = TSynTabChar) then
        inc(x, TabWidth - (x mod TabWidth))
      else
        inc(x);
    end;
    p.X := i;
  end;
  Result := p;
end;

procedure TCustomSynEdit.DoLinesDeleted(FirstLine, Count: integer);
var
  i: integer;
begin
  // gutter marks
  for i := 0 to Marks.Count - 1 do begin
    if Marks[i].Line >= FirstLine + Count then
      Marks[i].Line := Marks[i].Line - Count
    else if Marks[i].Line > FirstLine then
      Marks[i].Line := FirstLine;
  end;
  // plugins
  if fPlugins <> nil then begin
    for i := 0 to fPlugins.Count - 1 do
      TSynEditPlugin(fPlugins[i]).LinesDeleted(FirstLine, Count);
  end;
end;

procedure TCustomSynEdit.DoLinesInserted(FirstLine, Count: integer);
var
  i: integer;
begin
  // gutter marks
  for i := 0 to Marks.Count - 1 do begin
    if Marks[i].Line >= FirstLine then
      Marks[i].Line := Marks[i].Line + Count;
  end;
  // plugins
  if fPlugins <> nil then begin
    for i := 0 to fPlugins.Count - 1 do
      TSynEditPlugin(fPlugins[i]).LinesInserted(FirstLine, Count);
  end;
end;

procedure TCustomSynEdit.PluginsAfterPaint(ACanvas: TCanvas; AClip: TRect;
  FirstLine, LastLine: integer);
var
  i: integer;
begin
  if fPlugins <> nil then
    for i := 0 to fPlugins.Count - 1 do begin
      TSynEditPlugin(fPlugins[i]).AfterPaint(ACanvas, AClip, FirstLine,
        LastLine);
    end;
end;

procedure TCustomSynEdit.TrimmedSetLine(ALine: integer; ALineText: string);
begin
  if eoTrimTrailingSpaces in Options then
    Lines[ALine] := TrimTrailingSpaces(ALineText)
  else
    Lines[ALine] := ALineText;
end;

procedure TCustomSynEdit.SetOnKeyDown(const Value : TKeyEvent);
begin
  fKbdHandler.OnKeyDown := Value;
end;

function TCustomSynEdit.GetOnKeyDown : TKeyEvent;
begin
  Result := fKbdHandler.OnKeyDown;
end;

procedure TCustomSynEdit.SetOnKeyPress(const Value : TKeyPressEvent);
begin
  fKbdHandler.OnKeyPress := Value;
end;

function TCustomSynEdit.GetOnKeyPress : TKeyPressEvent;
begin
  Result := fKbdHandler.OnKeyPress;
end;

procedure TCustomSynEdit.AddKeyDownHandler (aHandler : TKeyDownProc);
begin
  fKbdHandler.AddKeyDownHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveKeyDownHandler (aHandler : TKeyDownProc);
begin
  fKbdHandler.RemoveKeyDownHandler(aHandler);
end;

procedure TCustomSynEdit.AddKeyPressHandler (aHandler : TKeyPressProc);
begin
  fKbdHandler.AddKeyPressHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveKeyPressHandler (aHandler : TKeyPressProc);
begin
  fKbdHandler.RemoveKeyPressHandler(aHandler);
end;

procedure TCustomSynEdit.AddFocusControl (aControl: TWinControl);
begin
  fFocusList.Add(aControl);
end;

procedure TCustomSynEdit.RemoveFocusControl (aControl: TWinControl);
begin
  fFocusList.Remove(aControl);
end;

function TCustomSynEdit.IdentChars: TSynIdentChars;                             //DDH 10/17/01 from Flávio Etrusco
begin
  if Highlighter <> nil then
    Result := Highlighter.IdentChars
  else
    Result := [#33..#255];
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

function TSynEditMark.GetIsBookmark: boolean;
begin
  Result := (fBookmarkNum >= 0);
end;

procedure TSynEditMark.SetColumn(const Value: Integer);
begin
  FColumn := Value;
end;

procedure TSynEditMark.SetImage(const Value: Integer);
begin
  FImage := Value;
  if fVisible and Assigned(fEdit) then
    fEdit.InvalidateGutterLines(fLine, fLine);
end;

procedure TSynEditMark.SetInternalImage(const Value: boolean);
begin
  fInternalImage := Value;
  if fVisible and Assigned(fEdit) then
    fEdit.InvalidateGutterLines(fLine, fLine);
end;

procedure TSynEditMark.SetLine(const Value: Integer);
begin
  if fVisible and Assigned(fEdit) then begin
    if fLine > 0 then
      fEdit.InvalidateGutterLines(fLine, fLine);
    fLine := Value;
    fEdit.InvalidateGutterLines(fLine, fLine);
  end else
    fLine := Value;
end;

procedure TSynEditMark.SetVisible(const Value: boolean);
begin
  if fVisible <> Value then begin
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

function TSynEditMarkList.Add(Item: TSynEditMark): Integer;
begin
  Result := inherited Add(Item);
  DoChange;
end;

procedure TSynEditMarkList.ClearLine(Line: integer);
var
  i: integer;
begin
  for i := Count - 1 downto 0 do
    if not Items[i].IsBookmark and (Items[i].Line = Line) then Delete(i);
end;

constructor TSynEditMarkList.Create(AOwner: TCustomSynEdit);
begin
  inherited Create;
  fEdit := AOwner;
end;

destructor TSynEditMarkList.Destroy;
var
  i: integer;
begin
  for i := 0 to Pred(Count) do
    Get(i).Free;
  inherited Destroy;
end;

procedure TSynEditMarkList.Delete(Index: Integer);
begin
  inherited Delete(Index);
  DoChange;
end;

procedure TSynEditMarkList.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TSynEditMarkList.First: TSynEditMark;
begin
  result := inherited First;
end;

function TSynEditMarkList.Get(Index: Integer): TSynEditMark;
begin
  result := inherited Get(Index);
end;

//Returns up to maxMarks book/gutter marks for a chosen line.

procedure TSynEditMarkList.GetMarksForLine(line: integer;
  var marks: TSynEditMarks);
var
  cnt: integer;
  i: integer;
begin
  FillChar(marks, SizeOf(marks), 0);
  cnt := 0;
  for i := 0 to Count - 1 do begin
    if Items[i].Line = line then begin
      Inc(cnt);
      marks[cnt] := Items[i];
      if cnt = maxMarks then break;
    end;
  end;
end;

procedure TSynEditMarkList.Insert(Index: Integer; Item: TSynEditMark);
begin
  inherited Insert(Index, Item);
  DoChange;
end;

function TSynEditMarkList.Last: TSynEditMark;
begin
  result := inherited Last;
end;

procedure TSynEditMarkList.Place(mark: TSynEditMark);
begin
  if assigned(fEdit) then
    if assigned(fEdit.OnPlaceBookmark) then fEdit.OnPlaceBookmark(fEdit, mark);
  if assigned(mark) then
    Add(mark);
  DoChange;
end;

procedure TSynEditMarkList.Put(Index: Integer; Item: TSynEditMark);
begin
  inherited Put(Index, Item);
  DoChange;
end;

function TSynEditMarkList.Remove(Item: TSynEditMark): Integer;
begin
  result := inherited Remove(Item);
  DoChange;
end;

{ TSynEditPlugin }

constructor TSynEditPlugin.Create(AOwner: TCustomSynEdit);
begin
  inherited Create;
  if AOwner <> nil then begin
    fOwner := AOwner;
    if fOwner.fPlugins = nil then
      fOwner.fPlugins := TList.Create;
    fOwner.fPlugins.Add(Self);
  end;
end;

destructor TSynEditPlugin.Destroy;
begin
  if fOwner <> nil then
    fOwner.fPlugins.Remove(Self);
  inherited Destroy;
end;

initialization
{$IFDEF SYN_CLX}
{$ELSE}
  SynEditClipboardFormat := RegisterClipboardFormat(SYNEDIT_CLIPBOARD_FORMAT);
{$ENDIF}
end. 
