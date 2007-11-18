
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntStdCtrls;

{$INCLUDE TntCompilers.inc}

interface

{ TODO: Implement TCustomListBox.KeyPress, OnDataFind. }

uses
  Windows, Messages, Classes, Controls, TntControls, StdCtrls, Graphics,
  TntClasses, TntSysUtils;

{TNT-WARN TCustomEdit}
type
  TTntCustomEdit = class(TCustomEdit{TNT-ALLOW TCustomEdit})
  private
    FPasswordChar: WideChar;
    procedure SetSelText(const Value: WideString);
    function GetText: WideString;
    procedure SetText(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsHintStored: Boolean;
    function GetPasswordChar: WideChar;
    procedure SetPasswordChar(const Value: WideChar);
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetSelStart: Integer; reintroduce; virtual;
    procedure SetSelStart(const Value: Integer); reintroduce; virtual;
    function GetSelLength: Integer; reintroduce; virtual;
    procedure SetSelLength(const Value: Integer); reintroduce; virtual;
    function GetSelText: WideString; reintroduce; virtual;
    property PasswordChar: WideChar read GetPasswordChar write SetPasswordChar default #0;
  public
    property SelText: WideString read GetSelText write SetSelText;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property Text: WideString read GetText write SetText;
  published
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  end;

{TNT-WARN TEdit}
  TTntEdit = class(TTntCustomEdit)
  published
    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF COMPILER_9_UP}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    {$IFDEF COMPILER_10_UP}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

type
  TTntCustomMemo = class;

  TTntMemoStrings = class(TTntStrings)
  protected
    FMemo: TCustomMemo{TNT-ALLOW TCustomMemo};
    FMemoLines: TStrings{TNT-ALLOW TStrings};
    FRichEditMode: Boolean;
    FLineBreakStyle: TTntTextLineBreakStyle;
    function Get(Index: Integer): WideString; override;
    function GetCount: Integer; override;
    function GetTextStr: WideString; override;
    procedure Put(Index: Integer; const S: WideString); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create;
    procedure SetTextStr(const Value: WideString); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: WideString); override;
  end;

{TNT-WARN TCustomMemo}
  TTntCustomMemo = class(TCustomMemo{TNT-ALLOW TCustomMemo})
  private
    FLines: TTntStrings;
    procedure SetSelText(const Value: WideString);
    function GetText: WideString;
    procedure SetText(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsHintStored: Boolean;
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure SetLines(const Value: TTntStrings); virtual;
    function GetSelStart: Integer; reintroduce; virtual;
    procedure SetSelStart(const Value: Integer); reintroduce; virtual;
    function GetSelLength: Integer; reintroduce; virtual;
    procedure SetSelLength(const Value: Integer); reintroduce; virtual;
    function GetSelText: WideString; reintroduce;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SelText: WideString read GetSelText write SetSelText;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property Text: WideString read GetText write SetText;
    property Lines: TTntStrings read FLines write SetLines;
  published
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  end;

{TNT-WARN TMemo}
  TTntMemo = class(TTntCustomMemo)
  published
    property Align;
    property Alignment;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property Lines;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WantReturns;
    property WantTabs;
    property WordWrap;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF COMPILER_9_UP}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    {$IFDEF COMPILER_10_UP}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TTntComboBoxStrings = class(TTntStrings)
  protected
    function Get(Index: Integer): WideString; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    ComboBox: TCustomComboBox{TNT-ALLOW TCustomComboBox};
    function Add(const S: WideString): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function IndexOf(const S: WideString): Integer; override;
    procedure Insert(Index: Integer; const S: WideString); override;
  end;

type
  TWMCharMsgHandler = procedure(var Message: TWMChar) of object;

{$IFDEF DELPHI_7} // fix for Delphi 7 only
{ TD7PatchedComboBoxStrings }
type
  TD7PatchedComboBoxStrings = class(TCustomComboBoxStrings)
  protected
    function Get(Index: Integer): string{TNT-ALLOW string}; override;
  public
    function Add(const S: string{TNT-ALLOW string}): Integer; override;
    procedure Insert(Index: Integer; const S: string{TNT-ALLOW string}); override;
  end;
{$ENDIF}

type
  ITntComboFindString = interface
    ['{63BEBEF4-B1A2-495A-B558-7487B66F6827}']
    function FindString(const Value: WideString; StartPos: Integer): Integer;
  end;

{TNT-WARN TCustomComboBox}
type
  TTntCustomComboBox = class(TCustomComboBox{TNT-ALLOW TCustomComboBox},
    IWideCustomListControl)
  private
    FItems: TTntStrings;
    FSaveItems: TTntStrings;
    FSaveItemIndex: Integer;
    FFilter: WideString;
    FLastTime: Cardinal;
    function GetItems: TTntStrings;
    function GetSelStart: Integer;
    procedure SetSelStart(const Value: Integer);
    function GetSelLength: Integer;
    procedure SetSelLength(const Value: Integer);
    function GetSelText: WideString;
    procedure SetSelText(const Value: WideString);
    function GetText: WideString;
    procedure SetText(const Value: WideString);
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsHintStored: Boolean;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure DestroyWnd; override;
    function GetAutoComplete_UniqueMatchOnly: Boolean; dynamic;
    function GetAutoComplete_PreserveDataEntryCase: Boolean; dynamic;
    procedure DoEditCharMsg(var Message: TWMChar); virtual;
    procedure CreateWnd; override;
    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure KeyPress(var Key: AnsiChar); override;
    {$IFDEF DELPHI_7} // fix for Delphi 7 only
    function GetItemsClass: TCustomComboBoxStringsClass; override;
    {$ENDIF}
    procedure SetItems(const Value: TTntStrings); reintroduce; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopySelection(Destination: TCustomListControl); override;
    procedure AddItem(const Item: WideString; AObject: TObject); reintroduce; virtual;
  public
    property SelText: WideString read GetSelText write SetSelText;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property Text: WideString read GetText write SetText;
    property Items: TTntStrings read GetItems write SetItems;
  published
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  end;

{TNT-WARN TComboBox}
  TTntComboBox = class(TTntCustomComboBox)
  published
    property Align;
    property AutoComplete default True;
    {$IFDEF COMPILER_9_UP}
    property AutoCompleteDelay default 500;
    {$ENDIF}
    property AutoDropDown default False;
    {$IFDEF COMPILER_7_UP}
    property AutoCloseUp default False;
    {$ENDIF}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ItemIndex default -1;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    {$IFDEF COMPILER_10_UP}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
    property Items; { Must be published after OnMeasureItem }
  end;

  TLBGetWideDataEvent = procedure(Control: TWinControl; Index: Integer;
    var Data: WideString) of object;

  TAccessCustomListBox = class(TCustomListBox{TNT-ALLOW TCustomListBox});

  TTntListBoxStrings = class(TTntStrings)
  private
    FListBox: TAccessCustomListBox;
    function GetListBox: TCustomListBox{TNT-ALLOW TCustomListBox};
    procedure SetListBox(const Value: TCustomListBox{TNT-ALLOW TCustomListBox});
  protected
    procedure Put(Index: Integer; const S: WideString); override;
    function Get(Index: Integer): WideString; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    function Add(const S: WideString): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function IndexOf(const S: WideString): Integer; override;
    procedure Insert(Index: Integer; const S: WideString); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    property ListBox: TCustomListBox{TNT-ALLOW TCustomListBox} read GetListBox write SetListBox;
  end;

{TNT-WARN TCustomListBox}
type
  TTntCustomListBox = class(TCustomListBox{TNT-ALLOW TCustomListBox}, IWideCustomListControl)
  private
    FItems: TTntStrings;
    FSaveItems: TTntStrings;
    FSaveTopIndex: Integer;
    FSaveItemIndex: Integer;
    FOnData: TLBGetWideDataEvent;
    procedure SetItems(const Value: TTntStrings);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsHintStored: Boolean;
    procedure LBGetText(var Message: TMessage); message LB_GETTEXT;
    procedure LBGetTextLen(var Message: TMessage); message LB_GETTEXTLEN;
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    property OnData: TLBGetWideDataEvent read FOnData write FOnData;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopySelection(Destination: TCustomListControl); override;
    procedure AddItem(const Item: WideString; AObject: TObject); reintroduce; virtual;
    property Items: TTntStrings read FItems write SetItems;
  published
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  end;

{TNT-WARN TListBox}
  TTntListBox = class(TTntCustomListBox)
  published
    property Style;
    property AutoComplete;
    {$IFDEF COMPILER_9_UP}
    property AutoCompleteDelay;
    {$ENDIF}
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property Items;
    property MultiSelect;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollWidth;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnData;
    property OnDataFind;
    property OnDataObject;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    {$IFDEF COMPILER_9_UP}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    {$IFDEF COMPILER_10_UP}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

{TNT-WARN TCustomLabel}
  TTntCustomLabel = class(TCustomLabel{TNT-ALLOW TCustomLabel})
  private
    function GetCaption: TWideCaption;
    procedure SetCaption(const Value: TWideCaption);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsCaptionStored: Boolean;
    function IsHintStored: Boolean;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetLabelText: WideString; reintroduce; virtual;
    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
    property Caption: TWideCaption read GetCaption write SetCaption stored IsCaptionStored;
  published
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  end;

{TNT-WARN TLabel}
  TTntLabel = class(TTntCustomLabel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color {$IFDEF COMPILER_7_UP} nodefault {$ENDIF};
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    {$IFDEF COMPILER_9_UP}
    property EllipsisPosition;
    {$ENDIF}
    property Enabled;
    property FocusControl;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    {$IFDEF COMPILER_9_UP}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
  end;

{TNT-WARN TButton}
  TTntButton = class(TButton{TNT-ALLOW TButton})
  private
    function GetCaption: TWideCaption;
    procedure SetCaption(const Value: TWideCaption);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsCaptionStored: Boolean;
    function IsHintStored: Boolean;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  published
    property Caption: TWideCaption read GetCaption write SetCaption stored IsCaptionStored;
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  end;

{TNT-WARN TCustomCheckBox}
  TTntCustomCheckBox = class(TCustomCheckBox{TNT-ALLOW TCustomCheckBox})
  private
    function GetCaption: TWideCaption;
    procedure SetCaption(const Value: TWideCaption);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsCaptionStored: Boolean;
    function IsHintStored: Boolean;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  public
    property Caption: TWideCaption read GetCaption write SetCaption stored IsCaptionStored;
  published
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  end;

{TNT-WARN TCheckBox}
  TTntCheckBox = class(TTntCustomCheckBox)
  published
    property Action;
    property Align;
    property Alignment;
    property AllowGrayed;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Checked;
    property Color {$IFDEF COMPILER_7_UP} nodefault {$ENDIF};
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property State;
    property TabOrder;
    property TabStop;
    property Visible;
    {$IFDEF COMPILER_7_UP}
    property WordWrap;
    {$ENDIF}
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF COMPILER_9_UP}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    {$IFDEF COMPILER_10_UP}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

{TNT-WARN TRadioButton}
  TTntRadioButton = class(TRadioButton{TNT-ALLOW TRadioButton})
  private
    function GetCaption: TWideCaption;
    procedure SetCaption(const Value: TWideCaption);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsCaptionStored: Boolean;
    function IsHintStored: Boolean;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  published
    property Caption: TWideCaption read GetCaption write SetCaption stored IsCaptionStored;
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  end;

{TNT-WARN TScrollBar}
  TTntScrollBar = class(TScrollBar{TNT-ALLOW TScrollBar})
  private
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsHintStored: Boolean;
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  published
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  end;

{TNT-WARN TCustomGroupBox}
  TTntCustomGroupBox = class(TCustomGroupBox{TNT-ALLOW TCustomGroupBox})
  private
    function GetCaption: TWideCaption;
    procedure SetCaption(const Value: TWideCaption);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsCaptionStored: Boolean;
    function IsHintStored: Boolean;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure Paint; override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    property Caption: TWideCaption read GetCaption write SetCaption stored IsCaptionStored;
  published
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  end;

{TNT-WARN TGroupBox}
  TTntGroupBox = class(TTntCustomGroupBox)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    {$IFDEF COMPILER_10_UP}
    property Padding;
    {$ENDIF}
    {$IFDEF COMPILER_7_UP}
    property ParentBackground default True;
    {$ENDIF}
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    {$IFDEF COMPILER_9_UP}
    property OnAlignInsertBefore;
    property OnAlignPosition;
    {$ENDIF}
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDockDrop;
    property OnDockOver;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    {$IFDEF COMPILER_9_UP}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    {$IFDEF COMPILER_10_UP}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

{TNT-WARN TCustomStaticText}
  TTntCustomStaticText = class(TCustomStaticText{TNT-ALLOW TCustomStaticText})
  private
    procedure AdjustBounds;
    function GetCaption: TWideCaption;
    procedure SetCaption(const Value: TWideCaption);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsCaptionStored: Boolean;
    function IsHintStored: Boolean;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure Loaded; override;
    procedure SetAutoSize(AValue: boolean); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    property Caption: TWideCaption read GetCaption write SetCaption stored IsCaptionStored;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  end;

{TNT-WARN TStaticText}
  TTntStaticText = class(TTntCustomStaticText)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    property Caption;
    property Color {$IFDEF COMPILER_7_UP} nodefault {$ENDIF};
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property TabOrder;
    property TabStop;
    {$IFDEF COMPILER_7_UP}
    property Transparent;
    {$ENDIF}
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    {$IFDEF COMPILER_9_UP}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    {$IFDEF COMPILER_10_UP}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure TntCombo_AfterInherited_CreateWnd(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox};
  Items: TTntStrings; var FSaveItems: TTntStrings; FSaveItemIndex: integer; PreInheritedAnsiText: AnsiString);
procedure TntCombo_BeforeInherited_DestroyWnd(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox};
  Items: TTntStrings; var FSaveItems: TTntStrings; ItemIndex: integer; var FSaveItemIndex: integer;
    var SavedText: WideString);
function TntCombo_ComboWndProc(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox};
  var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer; DoEditCharMsg: TWMCharMsgHandler): Boolean;
function TntCombo_CNCommand(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}; Items: TTntStrings; var Message: TWMCommand): Boolean;
function TntCombo_GetSelStart(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}): Integer;
procedure TntCombo_SetSelStart(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}; const Value: Integer);
function TntCombo_GetSelLength(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}): Integer;
procedure TntCombo_SetSelLength(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}; const Value: Integer);
function TntCombo_GetSelText(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}): WideString;
procedure TntCombo_SetSelText(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}; const Value: WideString);
procedure TntCombo_BeforeKeyPress(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}; var SaveAutoComplete: Boolean);
procedure TntCombo_AfterKeyPress(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}; var SaveAutoComplete: Boolean);
procedure TntCombo_DropDown_PreserveSelection(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox});
procedure TntComboBox_AddItem(Items: TTntStrings; const Item: WideString; AObject: TObject);
procedure TntComboBox_CopySelection(Items: TTntStrings; ItemIndex: Integer;
  Destination: TCustomListControl);
procedure TntCombo_AutoSearchKeyPress(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox};
  Items: TTntStrings; var Message: TWMChar; var FFilter: WideString; var FLastTime: Cardinal);
procedure TntCombo_AutoCompleteKeyPress(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox};
  Items: TTntStrings; var Message: TWMChar;
    AutoComplete_UniqueMatchOnly, AutoComplete_PreserveDataEntryCase: Boolean);
procedure TntCombo_DefaultDrawItem(Canvas: TCanvas; Index: Integer; Rect: TRect;
  State: TOwnerDrawState; Items: TTntStrings);

procedure TntCustomEdit_CreateWindowHandle(Edit: TCustomEdit{TNT-ALLOW TCustomEdit}; const Params: TCreateParams);
procedure TntCustomEdit_AfterInherited_CreateWnd(Edit: TCustomEdit{TNT-ALLOW TCustomEdit}; var FPasswordChar: WideChar);
function TntCustomEdit_GetSelStart(Edit: TCustomEdit{TNT-ALLOW TCustomEdit}): Integer;
procedure TntCustomEdit_SetSelStart(Edit: TCustomEdit{TNT-ALLOW TCustomEdit}; const Value: Integer);
function TntCustomEdit_GetSelLength(Edit: TCustomEdit{TNT-ALLOW TCustomEdit}): Integer;
procedure TntCustomEdit_SetSelLength(Edit: TCustomEdit{TNT-ALLOW TCustomEdit}; const Value: Integer);
function TntCustomEdit_GetSelText(Edit: TCustomEdit{TNT-ALLOW TCustomEdit}): WideString;
procedure TntCustomEdit_SetSelText(Edit: TCustomEdit{TNT-ALLOW TCustomEdit}; const Value: WideString);
function TntCustomEdit_GetPasswordChar(Edit: TCustomEdit{TNT-ALLOW TCustomEdit}; var FPasswordChar: WideChar): WideChar;
procedure TntCustomEdit_SetPasswordChar(Edit: TCustomEdit{TNT-ALLOW TCustomEdit}; var FPasswordChar: WideChar; const Value: WideChar);


function TntMemo_LineStart(Handle: THandle; Index: Integer): Integer;
function TntMemo_LineLength(Handle: THandle; Index: Integer; StartPos: Integer = -1): Integer;

procedure TntListBox_AfterInherited_CreateWnd(ListBox: TCustomListBox{TNT-ALLOW TCustomListBox};
  var FSaveItems: TTntStrings; FItems: TTntStrings; FSaveTopIndex, FSaveItemIndex: Integer);
procedure TntListBox_BeforeInherited_DestroyWnd(ListBox: TCustomListBox{TNT-ALLOW TCustomListBox};
  var FSaveItems: TTntStrings; const FItems: TTntStrings; var FSaveTopIndex, FSaveItemIndex: Integer);
procedure TntListBox_DrawItem_Text(ListBox: TCustomListBox{TNT-ALLOW TCustomListBox}; Items: TTntStrings; Index: Integer; Rect: TRect);
procedure TntListBox_AddItem(Items: TTntStrings; const Item: WideString; AObject: TObject);
procedure TntListBox_CopySelection(ListBox: TCustomListbox{TNT-ALLOW TCustomListbox};
  Items: TTntStrings; Destination: TCustomListControl);
function TntCustomListBox_LBGetText(ListBox: TCustomListBox{TNT-ALLOW TCustomListBox}; OnData: TLBGetWideDataEvent; var Message: TMessage): Boolean;
function TntCustomListBox_LBGetTextLen(ListBox: TCustomListBox{TNT-ALLOW TCustomListBox}; OnData: TLBGetWideDataEvent; var Message: TMessage): Boolean;

function TntLabel_DoDrawText(Control: TCustomLabel{TNT-ALLOW TCustomLabel}; var Rect: TRect; Flags: Integer; const GetLabelText: WideString): Boolean;
procedure TntLabel_CMDialogChar(Control: TCustomLabel{TNT-ALLOW TCustomLabel}; var Message: TCMDialogChar; const Caption: WideString);

procedure TntButton_CMDialogChar(Button: TButton{TNT-ALLOW TButton}; var Message: TCMDialogChar);

implementation

uses
  Forms, SysUtils, Consts, RichEdit, ComStrs,
  RTLConsts, {$IFDEF THEME_7_UP} Themes, {$ENDIF}
  TntForms, TntGraphics, TntActnList, TntWindows,
  {$IFDEF COMPILER_9_UP} WideStrUtils, {$ENDIF} TntWideStrUtils;

{ TTntCustomEdit }

procedure TntCustomEdit_CreateWindowHandle(Edit: TCustomEdit{TNT-ALLOW TCustomEdit}; const Params: TCreateParams);
var
  P: TCreateParams;
begin
  if SysLocale.FarEast
  and (not Win32PlatformIsUnicode)
  and ((Params.Style and ES_READONLY) <> 0) then begin
    // Work around Far East Win95 API/IME bug.
    P := Params;
    P.Style := P.Style and (not ES_READONLY);
    CreateUnicodeHandle(Edit, P, 'EDIT');
    if Edit.HandleAllocated then
      SendMessage(Edit.Handle, EM_SETREADONLY, Ord(True), 0);
  end else
    CreateUnicodeHandle(Edit, Params, 'EDIT');
end;

procedure TntCustomEdit_AfterInherited_CreateWnd(Edit: TCustomEdit{TNT-ALLOW TCustomEdit}; var FPasswordChar: WideChar);
var
  PasswordChar: WideChar;
begin
  PasswordChar := TntCustomEdit_GetPasswordChar(Edit, FPasswordChar);
  if Win32PlatformIsUnicode then
    SendMessageW(Edit.Handle, EM_SETPASSWORDCHAR, Ord(PasswordChar), 0);
end;

function TntCustomEdit_GetSelStart(Edit: TCustomEdit{TNT-ALLOW TCustomEdit}): Integer;
begin
  if Win32PlatformIsUnicode then
    Result := Edit.SelStart
  else
    Result := Length(WideString(Copy(Edit.Text, 1, Edit.SelStart)));
end;

procedure TntCustomEdit_SetSelStart(Edit: TCustomEdit{TNT-ALLOW TCustomEdit}; const Value: Integer);
begin
  if Win32PlatformIsUnicode then
    Edit.SelStart := Value
  else
    Edit.SelStart := Length(AnsiString(Copy(TntControl_GetText(Edit), 1, Value)));
end;

function TntCustomEdit_GetSelLength(Edit: TCustomEdit{TNT-ALLOW TCustomEdit}): Integer;
begin
  if Win32PlatformIsUnicode then
    Result := Edit.SelLength
  else
    Result := Length(TntCustomEdit_GetSelText(Edit));
end;

procedure TntCustomEdit_SetSelLength(Edit: TCustomEdit{TNT-ALLOW TCustomEdit}; const Value: Integer);
var
  StartPos: Integer;
begin
  if Win32PlatformIsUnicode then
    Edit.SelLength := Value
  else begin
    StartPos := TntCustomEdit_GetSelStart(Edit);
    Edit.SelLength := Length(AnsiString(Copy(TntControl_GetText(Edit), StartPos + 1, Value)));
  end;
end;

function TntCustomEdit_GetSelText(Edit: TCustomEdit{TNT-ALLOW TCustomEdit}): WideString;
begin
  if Win32PlatformIsUnicode then
    Result := Copy(TntControl_GetText(Edit), Edit.SelStart + 1, Edit.SelLength)
  else
    Result := Edit.SelText
end;

procedure TntCustomEdit_SetSelText(Edit: TCustomEdit{TNT-ALLOW TCustomEdit}; const Value: WideString);
begin
  if Win32PlatformIsUnicode then
    SendMessageW(Edit.Handle, EM_REPLACESEL, 0, Longint(PWideChar(Value)))
  else
    Edit.SelText := Value;
end;

function WideCharToAnsiChar(const C: WideChar): AnsiChar;
begin
  if C <= High(AnsiChar) then
    Result := AnsiChar(C)
  else
    Result := '*';
end;

type TAccessCustomEdit = class(TCustomEdit{TNT-ALLOW TCustomEdit});

function TntCustomEdit_GetPasswordChar(Edit: TCustomEdit{TNT-ALLOW TCustomEdit}; var FPasswordChar: WideChar): WideChar;
begin
  if TAccessCustomEdit(Edit).PasswordChar <> WideCharToAnsiChar(FPasswordChar) then
    FPasswordChar := WideChar(TAccessCustomEdit(Edit).PasswordChar);
  Result := FPasswordChar;
end;

procedure TntCustomEdit_SetPasswordChar(Edit: TCustomEdit{TNT-ALLOW TCustomEdit}; var FPasswordChar: WideChar; const Value: WideChar);
var
  SaveWindowHandle: Integer;
  PasswordCharSetHere: Boolean;
begin
  if TntCustomEdit_GetPasswordChar(Edit, FPasswordChar) <> Value then
  begin
    FPasswordChar := Value;
    PasswordCharSetHere := Win32PlatformIsUnicode and Edit.HandleAllocated;
    SaveWindowHandle := TAccessCustomEdit(Edit).WindowHandle;
    try
      if PasswordCharSetHere then
        TAccessCustomEdit(Edit).WindowHandle := 0; // this prevents TCustomEdit from actually changing it
      TAccessCustomEdit(Edit).PasswordChar := WideCharToAnsiChar(FPasswordChar);
    finally
      TAccessCustomEdit(Edit).WindowHandle := SaveWindowHandle;
    end;
    if PasswordCharSetHere then
    begin
      Assert(Win32PlatformIsUnicode);
      Assert(Edit.HandleAllocated);
      SendMessageW(Edit.Handle, EM_SETPASSWORDCHAR, Ord(FPasswordChar), 0);
      Edit.Invalidate;
    end;
  end;
end;

procedure TTntCustomEdit.CreateWindowHandle(const Params: TCreateParams);
begin
  TntCustomEdit_CreateWindowHandle(Self, Params);
end;

procedure TTntCustomEdit.CreateWnd;
begin
  inherited;
  TntCustomEdit_AfterInherited_CreateWnd(Self, FPasswordChar);
end;

procedure TTntCustomEdit.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntCustomEdit.GetSelStart: Integer;
begin
  Result := TntCustomEdit_GetSelStart(Self);
end;

procedure TTntCustomEdit.SetSelStart(const Value: Integer);
begin
  TntCustomEdit_SetSelStart(Self, Value);
end;

function TTntCustomEdit.GetSelLength: Integer;
begin
  Result := TntCustomEdit_GetSelLength(Self);
end;

procedure TTntCustomEdit.SetSelLength(const Value: Integer);
begin
  TntCustomEdit_SetSelLength(Self, Value);
end;

function TTntCustomEdit.GetSelText: WideString;
begin
  Result := TntCustomEdit_GetSelText(Self);
end;

procedure TTntCustomEdit.SetSelText(const Value: WideString);
begin
  TntCustomEdit_SetSelText(Self, Value);
end;

function TTntCustomEdit.GetPasswordChar: WideChar;
begin
  Result := TntCustomEdit_GetPasswordChar(Self, FPasswordChar);
end;

procedure TTntCustomEdit.SetPasswordChar(const Value: WideChar);
begin
  TntCustomEdit_SetPasswordChar(Self, FPasswordChar, Value);
end;

function TTntCustomEdit.GetText: WideString;
begin
  Result := TntControl_GetText(Self);
end;

procedure TTntCustomEdit.SetText(const Value: WideString);
begin
  TntControl_SetText(Self, Value);
end;

function TTntCustomEdit.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self);
end;

function TTntCustomEdit.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TTntCustomEdit.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntCustomEdit.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntCustomEdit.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

{ TTntMemoStrings }

constructor TTntMemoStrings.Create;
begin
  inherited;
  FLineBreakStyle := tlbsCRLF;
end;

function TTntMemoStrings.GetCount: Integer;
begin
  Result := FMemoLines.Count;
end;

function TntMemo_LineStart(Handle: THandle; Index: Integer): Integer;
begin
  Assert(Win32PlatformIsUnicode);
  Result := SendMessageW(Handle, EM_LINEINDEX, Index, 0);
end;

function TntMemo_LineLength(Handle: THandle; Index: Integer; StartPos: Integer = -1): Integer;
begin
  Assert(Win32PlatformIsUnicode);
  if StartPos = -1 then
    StartPos := TntMemo_LineStart(Handle, Index);
  if StartPos < 0 then
    Result := 0
  else
    Result := SendMessageW(Handle, EM_LINELENGTH, StartPos, 0);
end;

function TTntMemoStrings.Get(Index: Integer): WideString;
var
  Len: Integer;
begin
  if (not IsWindowUnicode(FMemo.Handle)) then
    Result := FMemoLines[Index]
  else begin
    SetLength(Result, TntMemo_LineLength(FMemo.Handle, Index));
    if Length(Result) > 0 then begin
      if Length(Result) > High(Word) then
        raise EOutOfResources.Create(SOutlineLongLine);
      Word((PWideChar(Result))^) := Length(Result);
      Len := SendMessageW(FMemo.Handle, EM_GETLINE, Index, Longint(PWideChar(Result)));
      SetLength(Result, Len);
    end;
  end;
end;

procedure TTntMemoStrings.Put(Index: Integer; const S: WideString);
var
  StartPos: Integer;
begin
  if (not IsWindowUnicode(FMemo.Handle)) then
    FMemoLines[Index] := S
  else begin
    StartPos := TntMemo_LineStart(FMemo.Handle, Index);
    if StartPos >= 0 then
    begin
      SendMessageW(FMemo.Handle, EM_SETSEL, StartPos, StartPos + TntMemo_LineLength(FMemo.Handle, Index));
      SendMessageW(FMemo.Handle, EM_REPLACESEL, 0, Longint(PWideChar(S)));
    end;
  end;
end;

procedure TTntMemoStrings.Insert(Index: Integer; const S: Widestring);

  function RichEditSelStartW: Integer;
  var
    CharRange: TCharRange;
  begin
    SendMessageW(FMemo.Handle, EM_EXGETSEL, 0, Longint(@CharRange));
    Result := CharRange.cpMin;
  end;

var
  StartPos, LineLen: Integer;
  Line: WideString;
begin
  if (not IsWindowUnicode(FMemo.Handle)) then
    FMemoLines.Insert(Index, S)
  else begin
    if Index >= 0 then
    begin
      StartPos := TntMemo_LineStart(FMemo.Handle, Index);
      if StartPos >= 0 then
        Line := S + CRLF
      else begin
        StartPos := TntMemo_LineStart(FMemo.Handle, Index - 1);
        LineLen := TntMemo_LineLength(FMemo.Handle, Index - 1);
        if LineLen = 0 then
          Exit;
        Inc(StartPos, LineLen);
        Line := CRLF + s;
      end;
      SendMessageW(FMemo.Handle, EM_SETSEL, StartPos, StartPos);

      if (FRichEditMode)
      and (FLineBreakStyle <> tlbsCRLF) then begin
        Line := TntAdjustLineBreaks(Line, FLineBreakStyle);
        if Line = CR then
          Line := CRLF; { This helps a ReadOnly RichEdit 4.1 control to insert a blank line. }
        SendMessageW(FMemo.Handle, EM_REPLACESEL, 0, Longint(PWideChar(Line)));
        if Line = CRLF then
          Line := CR;
      end else
        SendMessageW(FMemo.Handle, EM_REPLACESEL, 0, Longint(PWideChar(Line)));

      if (FRichEditMode)
      and (RichEditSelStartW <> (StartPos + Length(Line))) then
        raise EOutOfResources.Create(sRichEditInsertError);
    end;
  end;
end;

procedure TTntMemoStrings.Delete(Index: Integer);
begin
  FMemoLines.Delete(Index);
end;

procedure TTntMemoStrings.Clear;
begin
  FMemoLines.Clear;
end;

type TAccessStrings = class(TStrings{TNT-ALLOW TStrings});

procedure TTntMemoStrings.SetUpdateState(Updating: Boolean);
begin
  TAccessStrings(FMemoLines).SetUpdateState(Updating);
end;

function TTntMemoStrings.GetTextStr: WideString;
begin
  if (not FRichEditMode) then
    Result := TntControl_GetText(FMemo)
  else
    Result := inherited GetTextStr;
end;

procedure TTntMemoStrings.SetTextStr(const Value: WideString);
var
  NewText: WideString;
begin
  NewText := TntAdjustLineBreaks(Value, FLineBreakStyle);
  if NewText <> GetTextStr then begin
    FMemo.HandleNeeded;
    TntControl_SetText(FMemo, NewText);
  end;
end;

{ TTntCustomMemo }

constructor TTntCustomMemo.Create(AOwner: TComponent);
begin
  inherited;
  FLines := TTntMemoStrings.Create;
  TTntMemoStrings(FLines).FMemo := Self;
  TTntMemoStrings(FLines).FMemoLines := TCustomMemo{TNT-ALLOW TCustomMemo}(Self).Lines;
end;

destructor TTntCustomMemo.Destroy;
begin
  FreeAndNil(FLines);
  inherited;
end;

procedure TTntCustomMemo.SetLines(const Value: TTntStrings);
begin
  FLines.Assign(Value);
end;

procedure TTntCustomMemo.CreateWindowHandle(const Params: TCreateParams);
begin
  TntCustomEdit_CreateWindowHandle(Self, Params);
end;

procedure TTntCustomMemo.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntCustomMemo.GetSelStart: Integer;
begin
  Result := TntCustomEdit_GetSelStart(Self);
end;

procedure TTntCustomMemo.SetSelStart(const Value: Integer);
begin
  TntCustomEdit_SetSelStart(Self, Value);
end;

function TTntCustomMemo.GetSelLength: Integer;
begin
  Result := TntCustomEdit_GetSelLength(Self);
end;

procedure TTntCustomMemo.SetSelLength(const Value: Integer);
begin
  TntCustomEdit_SetSelLength(Self, Value);
end;

function TTntCustomMemo.GetSelText: WideString;
begin
  Result := TntCustomEdit_GetSelText(Self);
end;

procedure TTntCustomMemo.SetSelText(const Value: WideString);
begin
  TntCustomEdit_SetSelText(Self, Value);
end;

function TTntCustomMemo.GetText: WideString;
begin
  Result := TntControl_GetText(Self);
end;

procedure TTntCustomMemo.SetText(const Value: WideString);
begin
  TntControl_SetText(Self, Value);
end;

function TTntCustomMemo.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self);
end;

function TTntCustomMemo.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TTntCustomMemo.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntCustomMemo.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntCustomMemo.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

{$IFDEF DELPHI_7} // fix for Delphi 7 only
function TD7PatchedComboBoxStrings.Get(Index: Integer): string{TNT-ALLOW string};
var
  Len: Integer;
begin
  Len := SendMessage(ComboBox.Handle, CB_GETLBTEXTLEN, Index, 0);
  if Len > 0 then
  begin
    SetLength(Result, Len);
    SendMessage(ComboBox.Handle, CB_GETLBTEXT, Index, Longint(PChar{TNT-ALLOW PChar}(Result)));
  end
  else
    SetLength(Result, 0);
end;

function TD7PatchedComboBoxStrings.Add(const S: string{TNT-ALLOW string}): Integer;
begin
  Result := SendMessage(ComboBox.Handle, CB_ADDSTRING, 0, Longint(PChar{TNT-ALLOW PChar}(S)));
  if Result < 0 then
    raise EOutOfResources.Create(SInsertLineError);
end;

procedure TD7PatchedComboBoxStrings.Insert(Index: Integer; const S: string{TNT-ALLOW string});
begin
  if SendMessage(ComboBox.Handle, CB_INSERTSTRING, Index,
    Longint(PChar{TNT-ALLOW PChar}(S))) < 0 then
    raise EOutOfResources.Create(SInsertLineError);
end;
{$ENDIF}

{ TTntComboBoxStrings }

function TTntComboBoxStrings.GetCount: Integer;
begin
  Result := ComboBox.Items.Count;
end;

function TTntComboBoxStrings.Get(Index: Integer): WideString;
var
  Len: Integer;
begin
  if (not IsWindowUnicode(ComboBox.Handle)) then
    Result := ComboBox.Items[Index]
  else begin
    Len := SendMessageW(ComboBox.Handle, CB_GETLBTEXTLEN, Index, 0);
    if Len = CB_ERR then
      Result := ''
    else begin
      SetLength(Result, Len + 1);
      Len := SendMessageW(ComboBox.Handle, CB_GETLBTEXT, Index, Longint(PWideChar(Result)));
      if Len = CB_ERR then
        Result := ''
       else
        Result := PWideChar(Result);
    end;
  end;
end;

function TTntComboBoxStrings.GetObject(Index: Integer): TObject;
begin
  Result := ComboBox.Items.Objects[Index];
end;

procedure TTntComboBoxStrings.PutObject(Index: Integer; AObject: TObject);
begin
  ComboBox.Items.Objects[Index] := AObject;
end;

function TTntComboBoxStrings.Add(const S: WideString): Integer;
begin
  if (not IsWindowUnicode(ComboBox.Handle)) then
    Result := ComboBox.Items.Add(S)
  else begin
    Result := SendMessageW(ComboBox.Handle, CB_ADDSTRING, 0, Longint(PWideChar(S)));
    if Result < 0 then
      raise EOutOfResources.Create(SInsertLineError);
  end;
end;

procedure TTntComboBoxStrings.Insert(Index: Integer; const S: WideString);
begin
  if (not IsWindowUnicode(ComboBox.Handle)) then
    ComboBox.Items.Insert(Index, S)
  else begin
    if SendMessageW(ComboBox.Handle, CB_INSERTSTRING, Index, Longint(PWideChar(S))) < 0 then
      raise EOutOfResources.Create(SInsertLineError);
  end;
end;

procedure TTntComboBoxStrings.Delete(Index: Integer);
begin
  ComboBox.Items.Delete(Index);
end;

procedure TTntComboBoxStrings.Clear;
var
  S: WideString;
begin
  S := TntControl_GetText(ComboBox);
  SendMessage(ComboBox.Handle, CB_RESETCONTENT, 0, 0);
  TntControl_SetText(ComboBox, S);
  ComboBox.Update;
end;

procedure TTntComboBoxStrings.SetUpdateState(Updating: Boolean);
begin
  TAccessStrings(ComboBox.Items).SetUpdateState(Updating);
end;

function TTntComboBoxStrings.IndexOf(const S: WideString): Integer;
begin
  if (not IsWindowUnicode(ComboBox.Handle)) then
    Result := ComboBox.Items.IndexOf(S)
  else
    Result := SendMessageW(ComboBox.Handle, CB_FINDSTRINGEXACT, -1, LongInt(PWideChar(S)));
end;

{ TTntCustomComboBox }

type TAccessCustomComboBox = class(TCustomComboBox{TNT-ALLOW TCustomComboBox});

procedure TntCombo_AfterInherited_CreateWnd(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox};
  Items: TTntStrings; var FSaveItems: TTntStrings; FSaveItemIndex: integer; PreInheritedAnsiText: AnsiString);
begin
  if (not Win32PlatformIsUnicode) then begin
    TAccessCustomComboBox(Combo).Text := PreInheritedAnsiText;
  end else begin
    with TAccessCustomComboBox(Combo) do
    begin
      if ListHandle <> 0 then begin
        // re-extract FDefListProc as a Unicode proc
        SetWindowLongA(ListHandle, GWL_WNDPROC, Integer(FDefListProc));
        FDefListProc := Pointer(GetWindowLongW(ListHandle, GWL_WNDPROC));
        // override with FListInstance as a Unicode proc
        SetWindowLongW(ListHandle, GWL_WNDPROC, Integer(FListInstance));
      end;
      SetWindowLongW(EditHandle, GWL_WNDPROC, GetWindowLong(EditHandle, GWL_WNDPROC));
    end;
    if FSaveItems <> nil then
    begin
      Items.Assign(FSaveItems);
      FreeAndNil(FSaveItems);
      if FSaveItemIndex <> -1 then
      begin
        if Items.Count < FSaveItemIndex then FSaveItemIndex := Items.Count;
        SendMessage(Combo.Handle, CB_SETCURSEL, FSaveItemIndex, 0);
      end;
    end;
    TntControl_SetText(Combo, TntControl_GetStoredText(Combo, TAccessCustomComboBox(Combo).Text));
  end;
end;

procedure TntCombo_BeforeInherited_DestroyWnd(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox};
  Items: TTntStrings; var FSaveItems: TTntStrings; ItemIndex: integer; var FSaveItemIndex: integer;
    var SavedText: WideString);
begin
  Assert(not (csDestroyingHandle in Combo.ControlState));
  if (Win32PlatformIsUnicode) then begin
    SavedText := TntControl_GetText(Combo);
    if (Items.Count > 0) then
    begin
      FSaveItems := TTntStringList.Create;
      FSaveItems.Assign(Items);
      FSaveItemIndex:= ItemIndex;
      Items.Clear; { This keeps TCustomComboBox from creating its own FSaveItems. (this kills the original ItemIndex) }
    end;
  end;
end;

function TntCombo_ComboWndProc(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox};
  var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer; DoEditCharMsg: TWMCharMsgHandler): Boolean;

  procedure CallDefaultWindowProc;
  begin
    with Message do begin { call default wnd proc }
      if IsWindowUnicode(ComboWnd) then
        Result := CallWindowProcW(ComboProc, ComboWnd, Msg, WParam, LParam)
      else
        Result := CallWindowProcA(ComboProc, ComboWnd, Msg, WParam, LParam);
    end;
  end;

  function DoWideKeyPress(Message: TWMChar): Boolean;
  begin
    DoEditCharMsg(Message);
    Result := (Message.CharCode = 0);
  end;

begin
  Result := False;
  try
    if (Message.Msg = WM_CHAR) then begin
      // WM_CHAR
      Result := True;
      if IsWindowUnicode(ComboWnd) then
        MakeWMCharMsgSafeForAnsi(Message);
      try
        if TAccessCustomComboBox(Combo).DoKeyPress(TWMKey(Message)) then Exit;
        if DoWideKeyPress(TWMKey(Message)) then Exit;
      finally
        if IsWindowUnicode(ComboWnd) then
          RestoreWMCharMsg(Message);
      end;
      with TWMKey(Message) do begin
        if ((CharCode = VK_RETURN) or (CharCode = VK_ESCAPE)) and Combo.DroppedDown then begin
          Combo.DroppedDown := False;
          Exit;
        end;
      end;
      CallDefaultWindowProc;
    end else if (IsWindowUnicode(ComboWnd)) then begin
      // UNICODE
      if IsTextMessage(Message.Msg)
      or (Message.Msg = EM_REPLACESEL)
      or (Message.Msg = WM_IME_COMPOSITION)
      then begin
        // message w/ text parameter
        Result := True;
        CallDefaultWindowProc;
      end else if (Message.Msg = WM_IME_CHAR) then begin
        // WM_IME_CHAR
        Result := True;
        with Message do { convert to WM_CHAR }
          Result := SendMessageW(ComboWnd, WM_CHAR, WParam, LParam);
      end;
    end;
  except
    Application.HandleException(Combo);
  end;
end;

function TntCombo_CNCommand(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}; Items: TTntStrings; var Message: TWMCommand): Boolean;
begin
  Result := False;
  if Message.NotifyCode = CBN_SELCHANGE then begin
    Result := True;
    TntControl_SetText(Combo, Items[Combo.ItemIndex]);
    TAccessCustomComboBox(Combo).Click;
    TAccessCustomComboBox(Combo).Select;
  end;
end;

function TntCombo_GetSelStart(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}): Integer;
begin
  if Win32PlatformIsUnicode then
    Result := Combo.SelStart
  else
    Result := Length(WideString(Copy(TAccessCustomComboBox(Combo).Text, 1, Combo.SelStart)));
end;

procedure TntCombo_SetSelStart(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}; const Value: Integer);
begin
  if Win32PlatformIsUnicode then
    Combo.SelStart := Value
  else
    Combo.SelStart := Length(AnsiString(Copy(TntControl_GetText(Combo), 1, Value)));
end;

function TntCombo_GetSelLength(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}): Integer;
begin
  if Win32PlatformIsUnicode then
    Result := Combo.SelLength
  else
    Result := Length(TntCombo_GetSelText(Combo));
end;

procedure TntCombo_SetSelLength(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}; const Value: Integer);
var
  StartPos: Integer;
begin
  if Win32PlatformIsUnicode then
    Combo.SelLength := Value
  else begin
    StartPos := TntCombo_GetSelStart(Combo);
    Combo.SelLength := Length(AnsiString(Copy(TntControl_GetText(Combo), StartPos + 1, Value)));
  end;
end;

function TntCombo_GetSelText(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}): WideString;
begin
  if Win32PlatformIsUnicode then begin
    Result := '';
    if TAccessCustomComboBox(Combo).Style < csDropDownList then
      Result := Copy(TntControl_GetText(Combo), Combo.SelStart + 1, Combo.SelLength);
  end else
    Result := Combo.SelText
end;

procedure TntCombo_SetSelText(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}; const Value: WideString);
begin
  if Win32PlatformIsUnicode then begin
    if TAccessCustomComboBox(Combo).Style < csDropDownList then
    begin
      Combo.HandleNeeded;
      SendMessageW(TAccessCustomComboBox(Combo).EditHandle, EM_REPLACESEL, 0, Longint(PWideChar(Value)));
    end;
  end else
    Combo.SelText := Value
end;

procedure TntCombo_BeforeKeyPress(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}; var SaveAutoComplete: Boolean);
begin
  SaveAutoComplete := TAccessCustomComboBox(Combo).AutoComplete;
  TAccessCustomComboBox(Combo).AutoComplete := False;
end;

procedure TntCombo_AfterKeyPress(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}; var SaveAutoComplete: Boolean);
begin
  TAccessCustomComboBox(Combo).AutoComplete := SaveAutoComplete;
end;

procedure TntCombo_DropDown_PreserveSelection(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox});
var
  OldSelStart, OldSelLength: Integer;
  OldText: WideString;
begin
  OldText := TntControl_GetText(Combo);
  OldSelStart := TntCombo_GetSelStart(Combo);
  OldSelLength := TntCombo_GetSelLength(Combo);
  Combo.DroppedDown := True;
  TntControl_SetText(Combo, OldText);
  TntCombo_SetSelStart(Combo, OldSelStart);
  TntCombo_SetSelLength(Combo ,OldSelLength);
end;

procedure TntComboBox_AddItem(Items: TTntStrings; const Item: WideString; AObject: TObject);
begin
  Items.AddObject(Item, AObject);
end;

procedure TntComboBox_CopySelection(Items: TTntStrings; ItemIndex: Integer;
  Destination: TCustomListControl);
begin
  if ItemIndex <> -1 then
    WideListControl_AddItem(Destination, Items[ItemIndex], Items.Objects[ItemIndex]);
end;

function TntCombo_FindString(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox};
  StartPos: Integer; const Text: WideString): Integer;
var
  ComboFindString: ITntComboFindString;
begin
  if Combo.GetInterface(ITntComboFindString, ComboFindString) then
    Result := ComboFindString.FindString(Text, StartPos)
  else if IsWindowUnicode(Combo.Handle) then
    Result := SendMessageW(Combo.Handle, CB_FINDSTRING, StartPos, Integer(PWideChar(Text)))
  else
    Result := SendMessageA(Combo.Handle, CB_FINDSTRING, StartPos, Integer(PAnsiChar(AnsiString(Text))))
end;

function TntCombo_FindUniqueString(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox};
  StartPos: Integer; const Text: WideString): Integer;
var
  Match_1, Match_2: Integer;
begin
  Result := CB_ERR;
  Match_1 := TntCombo_FindString(Combo, -1, Text);
  if Match_1 <> CB_ERR then begin
    Match_2 := TntCombo_FindString(Combo, Match_1, Text);
    if Match_2 = Match_1 then
      Result := Match_1;
  end;
end;

function TntCombo_AutoSelect(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}; Items: TTntStrings;
  const SearchText: WideString; UniqueMatchOnly: Boolean; UseDataEntryCase: Boolean): Boolean;
var
  Idx: Integer;
  ValueChange: Boolean;
begin
  if UniqueMatchOnly then
    Idx := TntCombo_FindUniqueString(Combo, -1, SearchText)
  else
    Idx := TntCombo_FindString(Combo, -1, SearchText);
  Result := (Idx <> CB_ERR);
  if Result then begin
    if TAccessCustomComboBox(Combo).Style = csDropDown then
      ValueChange := not WideSameStr(TntControl_GetText(Combo), Items[Idx])
    else
      ValueChange := Idx <> Combo.ItemIndex;
    {$IFDEF COMPILER_7_UP}
    // auto-closeup
    if Combo.AutoCloseUp and (Items.IndexOf(SearchText) <> -1) then
      Combo.DroppedDown := False;
    {$ENDIF}
    // select item
    Combo.ItemIndex := Idx;
    // update edit
    if (TAccessCustomComboBox(Combo).Style in [csDropDown, csSimple]) then begin
      if UseDataEntryCase then begin
        // preserve case of characters as they are entered
        TntControl_SetText(Combo, SearchText + Copy(Items[Combo.ItemIndex], Length(SearchText) + 1, MaxInt));
      end else begin
        TntControl_SetText(Combo, Items[Idx]);
      end;
      // select the rest of the string
      TntCombo_SetSelStart(Combo, Length(SearchText));
      TntCombo_SetSelLength(Combo, Length(TntControl_GetText(Combo)) - TntCombo_GetSelStart(Combo));
    end;
    // notify events
    if ValueChange then begin
      TAccessCustomComboBox(Combo).Click;
      TAccessCustomComboBox(Combo).Select;
    end;
  end;
end;

procedure TntCombo_AutoSearchKeyPress(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox};
  Items: TTntStrings; var Message: TWMChar; var FFilter: WideString; var FLastTime: Cardinal);
var
  Key: WideChar;
begin
  if TAccessCustomComboBox(Combo).Style in [csSimple, csDropDown] then
    exit;
  if not Combo.AutoComplete then
    exit;
  Key := GetWideCharFromWMCharMsg(Message);
  try
    case Ord(Key) of
      VK_ESCAPE:
        exit;
      VK_TAB:
        if Combo.AutoDropDown and Combo.DroppedDown then
          Combo.DroppedDown := False;
      VK_BACK:
        Delete(FFilter, Length(FFilter), 1);
      else begin
        if Combo.AutoDropDown and (not Combo.DroppedDown) then
          Combo.DroppedDown := True;
        // reset FFilter if it's been too long (1.25 sec) { Windows XP is actually 2 seconds! }
        if GetTickCount - FLastTime >= 1250 then
          FFilter := '';
        FLastTime := GetTickCount;
        // if AutoSelect works, remember new FFilter
        if TntCombo_AutoSelect(Combo, Items, FFilter + Key, False, True) then begin
          FFilter := FFilter + Key;
          Key := #0;
        end;
      end;
    end;
  finally
    SetWideCharForWMCharMsg(Message, Key);
  end;
end;

procedure TntCombo_AutoCompleteKeyPress(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox};
  Items: TTntStrings; var Message: TWMChar;
    AutoComplete_UniqueMatchOnly, AutoComplete_PreserveDataEntryCase: Boolean);
var
  Key: WideChar;
  FindText: WideString;
begin
  Assert(TAccessCustomComboBox(Combo).Style in [csSimple, csDropDown], 'Internal Error: TntCombo_AutoCompleteKeyPress is only for csSimple and csDropDown style combo boxes.');
  if not Combo.AutoComplete then exit;
  Key := GetWideCharFromWMCharMsg(Message);
  try
    case Ord(Key) of
      VK_ESCAPE:
        exit;
      VK_TAB:
        if Combo.AutoDropDown and Combo.DroppedDown then
          Combo.DroppedDown := False;
      VK_BACK:
        exit;
      else begin
        if Combo.AutoDropDown and (not Combo.DroppedDown) then
          TntCombo_DropDown_PreserveSelection(Combo);
        // AutoComplete only if the selection is at the very end
        if ((TntCombo_GetSelStart(Combo) + TntCombo_GetSelLength(Combo))
            = Length(TntControl_GetText(Combo))) then
        begin
          FindText := Copy(TntControl_GetText(Combo), 1, TntCombo_GetSelStart(Combo)) + Key;
          if TntCombo_AutoSelect(Combo, Items, FindText, AutoComplete_UniqueMatchOnly, AutoComplete_PreserveDataEntryCase) then
          begin
            Key := #0;
          end;
        end;
      end;
    end;
  finally
    SetWideCharForWMCharMsg(Message, Key);
  end;
end;

//--
constructor TTntCustomComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TTntComboBoxStrings.Create;
  TTntComboBoxStrings(FItems).ComboBox := Self;
end;

destructor TTntCustomComboBox.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FSaveItems);
  inherited;
end;

procedure TTntCustomComboBox.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, 'COMBOBOX');
end;

procedure TTntCustomComboBox.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

procedure TTntCustomComboBox.CreateWnd;
var
  PreInheritedAnsiText: AnsiString;
begin
  PreInheritedAnsiText := TAccessCustomComboBox(Self).Text;
  inherited;
  TntCombo_AfterInherited_CreateWnd(Self, Items, FSaveItems, FSaveItemIndex, PreInheritedAnsiText);
end;

procedure TTntCustomComboBox.DestroyWnd;
var
  SavedText: WideString;
begin
  if not (csDestroyingHandle in ControlState) then begin { avoid recursion when parent is TToolBar and system font changes. }
    TntCombo_BeforeInherited_DestroyWnd(Self, Items, FSaveItems, ItemIndex, FSaveItemIndex, SavedText);
    inherited;
    TntControl_SetStoredText(Self, SavedText);
  end;
end;

procedure TTntCustomComboBox.ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer);
begin
  if not TntCombo_ComboWndProc(Self, Message, ComboWnd, ComboProc, DoEditCharMsg) then
    inherited;
end;

procedure TTntCustomComboBox.KeyPress(var Key: AnsiChar);
var
  SaveAutoComplete: Boolean;
begin
  TntCombo_BeforeKeyPress(Self, SaveAutoComplete);
  try
    inherited;
  finally
    TntCombo_AfterKeyPress(Self, SaveAutoComplete);
  end;
end;

procedure TTntCustomComboBox.DoEditCharMsg(var Message: TWMChar);
begin
  TntCombo_AutoCompleteKeyPress(Self, Items, Message,
    GetAutoComplete_UniqueMatchOnly, GetAutoComplete_PreserveDataEntryCase);
end;

procedure TTntCustomComboBox.WMChar(var Message: TWMChar);
begin
  TntCombo_AutoSearchKeyPress(Self, Items, Message, FFilter, FLastTime);
  if Message.CharCode <> 0 then
    inherited;
end;

procedure TntCombo_DefaultDrawItem(Canvas: TCanvas; Index: Integer; Rect: TRect;
  State: TOwnerDrawState; Items: TTntStrings);
begin
  Canvas.FillRect(Rect);
  if Index >= 0 then
    WideCanvasTextOut(Canvas, Rect.Left + 2, Rect.Top, Items[Index]);
end;

procedure TTntCustomComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  TControlCanvas(Canvas).UpdateTextFlags;
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, Index, Rect, State)
  else
    TntCombo_DefaultDrawItem(Canvas, Index, Rect, State, Items);
end;

function TTntCustomComboBox.GetItems: TTntStrings;
begin
  Result := FItems;
end;

procedure TTntCustomComboBox.SetItems(const Value: TTntStrings);
begin
  FItems.Assign(Value);
end;

function TTntCustomComboBox.GetSelStart: Integer;
begin
  Result := TntCombo_GetSelStart(Self);
end;

procedure TTntCustomComboBox.SetSelStart(const Value: Integer);
begin
  TntCombo_SetSelStart(Self, Value);
end;

function TTntCustomComboBox.GetSelLength: Integer;
begin
  Result := TntCombo_GetSelLength(Self);
end;

procedure TTntCustomComboBox.SetSelLength(const Value: Integer);
begin
  TntCombo_SetSelLength(Self, Value);
end;

function TTntCustomComboBox.GetSelText: WideString;
begin
  Result := TntCombo_GetSelText(Self);
end;

procedure TTntCustomComboBox.SetSelText(const Value: WideString);
begin
  TntCombo_SetSelText(Self, Value);
end;

function TTntCustomComboBox.GetText: WideString;
begin
  Result := TntControl_GetText(Self);
end;

procedure TTntCustomComboBox.SetText(const Value: WideString);
begin
  TntControl_SetText(Self, Value);
end;

procedure TTntCustomComboBox.CNCommand(var Message: TWMCommand);
begin
  if not TntCombo_CNCommand(Self, Items, Message) then
    inherited;
end;

function TTntCustomComboBox.GetAutoComplete_PreserveDataEntryCase: Boolean;
begin
  Result := True;
end;

function TTntCustomComboBox.GetAutoComplete_UniqueMatchOnly: Boolean;
begin
  Result := False;
end;

function TTntCustomComboBox.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self)
end;

function TTntCustomComboBox.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TTntCustomComboBox.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntCustomComboBox.AddItem(const Item: WideString; AObject: TObject);
begin
  TntComboBox_AddItem(Items, Item, AObject);
end;

procedure TTntCustomComboBox.CopySelection(Destination: TCustomListControl);
begin
  TntComboBox_CopySelection(Items, ItemIndex, Destination);
end;

procedure TTntCustomComboBox.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntCustomComboBox.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

{$IFDEF DELPHI_7} // fix for Delphi 7 only
function TTntCustomComboBox.GetItemsClass: TCustomComboBoxStringsClass;
begin
  Result := TD7PatchedComboBoxStrings;
end;
{$ENDIF}

{ TTntListBoxStrings }

function TTntListBoxStrings.GetListBox: TCustomListBox{TNT-ALLOW TCustomListBox};
begin
  Result := TCustomListBox{TNT-ALLOW TCustomListBox}(FListBox);
end;

procedure TTntListBoxStrings.SetListBox(const Value: TCustomListBox{TNT-ALLOW TCustomListBox});
begin
  FListBox := TAccessCustomListBox(Value);
end;

function TTntListBoxStrings.GetCount: Integer;
begin
  Result := ListBox.Items.Count;
end;

function TTntListBoxStrings.Get(Index: Integer): WideString;
var
  Len: Integer;
begin
  if (not IsWindowUnicode(ListBox.Handle)) then
    Result := ListBox.Items[Index]
  else begin
    Len := SendMessageW(ListBox.Handle, LB_GETTEXTLEN, Index, 0);
    if Len = LB_ERR then
      Error(SListIndexError, Index)
    else begin
      SetLength(Result, Len + 1);
      Len := SendMessageW(ListBox.Handle, LB_GETTEXT, Index, Longint(PWideChar(Result)));
      if Len = LB_ERR then
        Result := ''
       else
        Result := PWideChar(Result);
    end;
  end;
end;

function TTntListBoxStrings.GetObject(Index: Integer): TObject;
begin
  Result := ListBox.Items.Objects[Index];
end;

procedure TTntListBoxStrings.Put(Index: Integer; const S: WideString);
var
  I: Integer;
  TempData: Longint;
begin
  I := ListBox.ItemIndex;
  TempData := FListBox.InternalGetItemData(Index);
  // Set the Item to 0 in case it is an object that gets freed during Delete
  FListBox.InternalSetItemData(Index, 0);
  Delete(Index);
  InsertObject(Index, S, nil);
  FListBox.InternalSetItemData(Index, TempData);
  ListBox.ItemIndex := I;
end;

procedure TTntListBoxStrings.PutObject(Index: Integer; AObject: TObject);
begin
  ListBox.Items.Objects[Index] := AObject;
end;

function TTntListBoxStrings.Add(const S: WideString): Integer;
begin
  if (not IsWindowUnicode(ListBox.Handle)) then
    Result := ListBox.Items.Add(S)
  else begin
    Result := SendMessageW(ListBox.Handle, LB_ADDSTRING, 0, Longint(PWideChar(S)));
    if Result < 0 then
      raise EOutOfResources.Create(SInsertLineError);
  end;
end;

procedure TTntListBoxStrings.Insert(Index: Integer; const S: WideString);
begin
  if (not IsWindowUnicode(ListBox.Handle)) then
    ListBox.Items.Insert(Index, S)
  else begin
    if SendMessageW(ListBox.Handle, LB_INSERTSTRING, Index, Longint(PWideChar(S))) < 0 then
      raise EOutOfResources.Create(SInsertLineError);
  end;
end;

procedure TTntListBoxStrings.Delete(Index: Integer);
begin
  FListBox.DeleteString(Index);
end;

procedure TTntListBoxStrings.Exchange(Index1, Index2: Integer);
var
  TempData: Longint;
  TempString: WideString;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempData := FListBox.InternalGetItemData(Index1);
    Strings[Index1] := Strings[Index2];
    FListBox.InternalSetItemData(Index1, FListBox.InternalGetItemData(Index2));
    Strings[Index2] := TempString;
    FListBox.InternalSetItemData(Index2, TempData);
    if ListBox.ItemIndex = Index1 then
      ListBox.ItemIndex := Index2
    else if ListBox.ItemIndex = Index2 then
      ListBox.ItemIndex := Index1;
  finally
    EndUpdate;
  end;
end;

procedure TTntListBoxStrings.Clear;
begin
  FListBox.ResetContent;
end;

procedure TTntListBoxStrings.SetUpdateState(Updating: Boolean);
begin
  TAccessStrings(ListBox.Items).SetUpdateState(Updating);
end;

function TTntListBoxStrings.IndexOf(const S: WideString): Integer;
begin
  if (not IsWindowUnicode(ListBox.Handle)) then
    Result := ListBox.Items.IndexOf(S)
  else
    Result := SendMessageW(ListBox.Handle, LB_FINDSTRINGEXACT, -1, LongInt(PWideChar(S)));
end;

procedure TTntListBoxStrings.Move(CurIndex, NewIndex: Integer);
var
  TempData: Longint;
  TempString: WideString;
begin
  BeginUpdate;
  FListBox.FMoving := True;
  try
    if CurIndex <> NewIndex then
    begin
      TempString := Get(CurIndex);
      TempData := FListBox.InternalGetItemData(CurIndex);
      FListBox.InternalSetItemData(CurIndex, 0);
      Delete(CurIndex);
      Insert(NewIndex, TempString);
      FListBox.InternalSetItemData(NewIndex, TempData);
    end;
  finally
    FListBox.FMoving := False;
    EndUpdate;
  end;
end;

//-- list box helper procs

procedure TntListBox_AfterInherited_CreateWnd(ListBox: TCustomListBox{TNT-ALLOW TCustomListBox};
  var FSaveItems: TTntStrings; FItems: TTntStrings; FSaveTopIndex, FSaveItemIndex: Integer);
begin
  if FSaveItems <> nil then
  begin
    FItems.Assign(FSaveItems);
    FreeAndNil(FSaveItems);
    ListBox.TopIndex := FSaveTopIndex;
    ListBox.ItemIndex := FSaveItemIndex;
  end;
end;

procedure TntListBox_BeforeInherited_DestroyWnd(ListBox: TCustomListBox{TNT-ALLOW TCustomListBox};
  var FSaveItems: TTntStrings; const FItems: TTntStrings; var FSaveTopIndex, FSaveItemIndex: Integer);
begin
  if (FItems.Count > 0)
  and (not (TAccessCustomListBox(ListBox).Style in [lbVirtual, lbVirtualOwnerDraw]))
  then begin
    FSaveItems := TTntStringList.Create;
    FSaveItems.Assign(FItems);
    FSaveTopIndex := ListBox.TopIndex;
    FSaveItemIndex := ListBox.ItemIndex;
    ListBox.Items.Clear; { This keeps TCustomListBox from creating its own FSaveItems. (this kills the original ItemIndex) }
  end;
end;

procedure TntListBox_DrawItem_Text(ListBox: TCustomListBox{TNT-ALLOW TCustomListBox}; Items: TTntStrings; Index: Integer; Rect: TRect);
var
  Flags: Integer;
  Canvas: TCanvas;
begin
  Canvas := TAccessCustomListBox(ListBox).Canvas;
  Canvas.FillRect(Rect);
  if Index < Items.Count then
  begin
    Flags := ListBox.DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
    if not ListBox.UseRightToLeftAlignment then
      Inc(Rect.Left, 2)
    else
      Dec(Rect.Right, 2);
    Tnt_DrawTextW(Canvas.Handle, PWideChar(Items[Index]), Length(Items[Index]), Rect, Flags);
  end;
end;

procedure TntListBox_AddItem(Items: TTntStrings; const Item: WideString; AObject: TObject);
begin
  Items.AddObject(PWideChar(Item), AObject);
end;

procedure TntListBox_CopySelection(ListBox: TCustomListbox{TNT-ALLOW TCustomListbox};
  Items: TTntStrings; Destination: TCustomListControl);
var
  I: Integer;
begin
  if ListBox.MultiSelect then
  begin
    for I := 0 to Items.Count - 1 do
      if ListBox.Selected[I] then
        WideListControl_AddItem(Destination, PWideChar(Items[I]), Items.Objects[I]);
  end
  else
    if Listbox.ItemIndex <> -1 then
      WideListControl_AddItem(Destination, PWideChar(Items[ListBox.ItemIndex]), Items.Objects[ListBox.ItemIndex]);
end;

function TntCustomListBox_GetOwnerData(ListBox: TCustomListBox{TNT-ALLOW TCustomListBox}; OnData: TLBGetWideDataEvent; Index: Integer; out Data: WideString): Boolean;
var
  AnsiData: AnsiString;
begin
  Result := False;
  Data := '';
  if (Index > -1) and (Index < ListBox.Count) then begin
    if Assigned(OnData) then begin
      OnData(ListBox, Index, Data);
      Result := True;
    end else if Assigned(TAccessCustomListBox(ListBox).OnData) then begin
      AnsiData := '';
      TAccessCustomListBox(ListBox).OnData(ListBox, Index, AnsiData);
      Data := AnsiData;
      Result := True;
    end;
  end;
end;

function TntCustomListBox_LBGetText(ListBox: TCustomListBox{TNT-ALLOW TCustomListBox}; OnData: TLBGetWideDataEvent; var Message: TMessage): Boolean;
var
  S: WideString;
  AnsiS: AnsiString;
begin
  if TAccessCustomListBox(ListBox).Style in [lbVirtual, lbVirtualOwnerDraw] then
  begin
    Result := True;
    if TntCustomListBox_GetOwnerData(ListBox, OnData, Message.WParam, S) then begin
      if Win32PlatformIsUnicode then begin
        WStrCopy(PWideChar(Message.LParam), PWideChar(S));
        Message.Result := Length(S);
      end else begin
        AnsiS := S;
        StrCopy{TNT-ALLOW StrCopy}(PAnsiChar(Message.LParam), PAnsiChar(AnsiS));
        Message.Result := Length(AnsiS);
      end;
    end
    else
      Message.Result := LB_ERR;
  end
  else
    Result := False;
end;

function TntCustomListBox_LBGetTextLen(ListBox: TCustomListBox{TNT-ALLOW TCustomListBox}; OnData: TLBGetWideDataEvent; var Message: TMessage): Boolean;
var
  S: WideString;
begin
  if TAccessCustomListBox(ListBox).Style in [lbVirtual, lbVirtualOwnerDraw] then
  begin
    Result := True;
    if TntCustomListBox_GetOwnerData(ListBox, OnData, Message.WParam, S) then begin
      if Win32PlatformIsUnicode then
        Message.Result := Length(S)
      else
        Message.Result := Length(AnsiString(S));
    end else
      Message.Result := LB_ERR;
  end
  else
    Result := False;
end;

{ TTntCustomListBox }

constructor TTntCustomListBox.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TTntListBoxStrings.Create;
  TTntListBoxStrings(FItems).ListBox := Self;
end;

destructor TTntCustomListBox.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FSaveItems);
  inherited;
end;

procedure TTntCustomListBox.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, 'LISTBOX');
end;

procedure TTntCustomListBox.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

procedure TTntCustomListBox.CreateWnd;
begin
  inherited;
  TntListBox_AfterInherited_CreateWnd(Self, FSaveItems, FItems, FSaveTopIndex, FSaveItemIndex);
end;

procedure TTntCustomListBox.DestroyWnd;
begin
  TntListBox_BeforeInherited_DestroyWnd(Self, FSaveItems, FItems, FSaveTopIndex, FSaveItemIndex);
  inherited;
end;

procedure TTntCustomListBox.SetItems(const Value: TTntStrings);
begin
  FItems.Assign(Value);
end;

procedure TTntCustomListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, Index, Rect, State)
  else
    TntListBox_DrawItem_Text(Self, Items, Index, Rect);
end;

function TTntCustomListBox.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self)
end;

function TTntCustomListBox.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TTntCustomListBox.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntCustomListBox.AddItem(const Item: WideString; AObject: TObject);
begin
  TntListBox_AddItem(Items, Item, AObject);
end;

procedure TTntCustomListBox.CopySelection(Destination: TCustomListControl);
begin
  TntListBox_CopySelection(Self, Items, Destination);
end;

procedure TTntCustomListBox.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntCustomListBox.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

procedure TTntCustomListBox.LBGetText(var Message: TMessage);
begin
  if not TntCustomListBox_LBGetText(Self, OnData, Message) then
    inherited;
end;

procedure TTntCustomListBox.LBGetTextLen(var Message: TMessage);
begin
  if not TntCustomListBox_LBGetTextLen(Self, OnData, Message) then
    inherited;
end;

// --- label helper procs

type TAccessCustomLabel = class(TCustomLabel{TNT-ALLOW TCustomLabel});

function TntLabel_DoDrawText(Control: TCustomLabel{TNT-ALLOW TCustomLabel}; var Rect: TRect; Flags: Integer; const GetLabelText: WideString): Boolean;
{$IFDEF COMPILER_9_UP}
const
  EllipsisStr = '...';
  Ellipsis: array[TEllipsisPosition] of Longint = (0, DT_PATH_ELLIPSIS,
    DT_END_ELLIPSIS, DT_WORD_ELLIPSIS);
{$ENDIF}
var
  Text: WideString;
  ShowAccelChar: Boolean;
  Canvas: TCanvas;
  {$IFDEF COMPILER_9_UP}
  DText: WideString;
  NewRect: TRect;
  Height: Integer;
  Delim: Integer;
  {$ENDIF}
begin
  Result := False;
  if Win32PlatformIsUnicode then begin
    Result := True;
    Text := GetLabelText;
    ShowAccelChar := TAccessCustomLabel(Control).ShowAccelChar;
    Canvas := Control.Canvas;
    if (Flags and DT_CALCRECT <> 0) and ((Text = '') or ShowAccelChar and
      (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';
    if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;
    Flags := Control.DrawTextBiDiModeFlags(Flags);
    Canvas.Font := TAccessCustomLabel(Control).Font;
    {$IFDEF COMPILER_9_UP}
    if (TAccessCustomLabel(Control).EllipsisPosition <> epNone)
    and (not TAccessCustomLabel(Control).AutoSize) then
    begin
      DText := Text;
      Flags := Flags and not (DT_EXPANDTABS or DT_CALCRECT);
      Flags := Flags or Ellipsis[TAccessCustomLabel(Control).EllipsisPosition];
      if TAccessCustomLabel(Control).WordWrap
      and (TAccessCustomLabel(Control).EllipsisPosition in [epEndEllipsis, epWordEllipsis]) then
      begin
        repeat
          NewRect := Rect;
          Dec(NewRect.Right, WideCanvasTextWidth(Canvas, EllipsisStr));
          Tnt_DrawTextW(Canvas.Handle, PWideChar(DText), Length(DText), NewRect, Flags or DT_CALCRECT);
          Height := NewRect.Bottom - NewRect.Top;
          if (Height > TAccessCustomLabel(Control).ClientHeight)
          and (Height > Canvas.Font.Height) then
          begin
            Delim := WideLastDelimiter(' '#9, Text);
            if Delim = 0 then
              Delim := Length(Text);
            Dec(Delim);
            Text := Copy(Text, 1, Delim);
            DText := Text + EllipsisStr;
            if Text = '' then
              Break;
          end else
            Break;
        until False;
      end;
      if Text <> '' then
        Text := DText;
    end;
    {$ENDIF}
    if not Control.Enabled then
    begin
      OffsetRect(Rect, 1, 1);
      Canvas.Font.Color := clBtnHighlight;
      Tnt_DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Rect, Flags);
      OffsetRect(Rect, -1, -1);
      Canvas.Font.Color := clBtnShadow;
      Tnt_DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Rect, Flags);
    end
    else
      Tnt_DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Rect, Flags);
  end;
end;

procedure TntLabel_CMDialogChar(Control: TCustomLabel{TNT-ALLOW TCustomLabel}; var Message: TCMDialogChar; const Caption: WideString);
var
  FocusControl: TWinControl;
  ShowAccelChar: Boolean;
begin
  FocusControl := TAccessCustomLabel(Control).FocusControl;
  ShowAccelChar := TAccessCustomLabel(Control).ShowAccelChar;
  if (FocusControl <> nil) and Control.Enabled and ShowAccelChar and
    IsWideCharAccel(Message.CharCode, Caption) then
    with FocusControl do
      if CanFocus then
      begin
        SetFocus;
        Message.Result := 1;
      end;
end;

{ TTntCustomLabel }

procedure TTntCustomLabel.CMDialogChar(var Message: TCMDialogChar);
begin
  TntLabel_CMDialogChar(Self, Message, Caption);
end;

function TTntCustomLabel.IsCaptionStored: Boolean;
begin
  Result := TntControl_IsCaptionStored(Self)
end;

function TTntCustomLabel.GetCaption: TWideCaption;
begin
  Result := TntControl_GetText(Self);
end;

procedure TTntCustomLabel.SetCaption(const Value: TWideCaption);
begin
  TntControl_SetText(Self, Value);
end;

procedure TTntCustomLabel.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntCustomLabel.GetLabelText: WideString;
begin
  Result := Caption;
end;

procedure TTntCustomLabel.DoDrawText(var Rect: TRect; Flags: Integer);
begin
  if not TntLabel_DoDrawText(Self, Rect, Flags, GetLabelText) then
    inherited;
end;

function TTntCustomLabel.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self)
end;

function TTntCustomLabel.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TTntCustomLabel.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntCustomLabel.CMHintShow(var Message: TMessage);
begin
  ProcessCMHintShowMsg(Message);
  inherited;
end;

procedure TTntCustomLabel.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntCustomLabel.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

{ TTntButton }

procedure TntButton_CMDialogChar(Button: TButton{TNT-ALLOW TButton}; var Message: TCMDialogChar);
begin
  with Message do
    if IsWideCharAccel(Message.CharCode, TntControl_GetText(Button))
    and Button.CanFocus then
    begin
      Button.Click;
      Result := 1;
    end else
      Button.Broadcast(Message);
end;

procedure TTntButton.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, 'BUTTON');
end;

procedure TTntButton.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

procedure TTntButton.CMDialogChar(var Message: TCMDialogChar);
begin
  TntButton_CMDialogChar(Self, Message);
end;

function TTntButton.IsCaptionStored: Boolean;
begin
  Result := TntControl_IsCaptionStored(Self)
end;

function TTntButton.GetCaption: TWideCaption;
begin
  Result := TntControl_GetText(Self)
end;

procedure TTntButton.SetCaption(const Value: TWideCaption);
begin
  TntControl_SetText(Self, Value);
end;

function TTntButton.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self)
end;

function TTntButton.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TTntButton.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

{ TTntCustomCheckBox }

procedure TTntCustomCheckBox.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, 'BUTTON');
end;

procedure TTntCustomCheckBox.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

procedure TTntCustomCheckBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsWideCharAccel(Message.CharCode, Caption)
    and CanFocus then
    begin
      SetFocus;
      if Focused then Toggle;
      Result := 1;
    end else
      Broadcast(Message);
end;

function TTntCustomCheckBox.IsCaptionStored: Boolean;
begin
  Result := TntControl_IsCaptionStored(Self)
end;

function TTntCustomCheckBox.GetCaption: TWideCaption;
begin
  Result := TntControl_GetText(Self)
end;

procedure TTntCustomCheckBox.SetCaption(const Value: TWideCaption);
begin
  TntControl_SetText(Self, Value);
end;

function TTntCustomCheckBox.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self)
end;

function TTntCustomCheckBox.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TTntCustomCheckBox.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntCustomCheckBox.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntCustomCheckBox.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

{ TTntRadioButton }

procedure TTntRadioButton.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, 'BUTTON');
end;

procedure TTntRadioButton.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

procedure TTntRadioButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsWideCharAccel(Message.CharCode, Caption)
    and CanFocus then
    begin
      SetFocus;
      Result := 1;
    end else
      Broadcast(Message);
end;

function TTntRadioButton.IsCaptionStored: Boolean;
begin
  Result := TntControl_IsCaptionStored(Self);
end;

function TTntRadioButton.GetCaption: TWideCaption;
begin
  Result := TntControl_GetText(Self)
end;

procedure TTntRadioButton.SetCaption(const Value: TWideCaption);
begin
  TntControl_SetText(Self, Value);
end;

function TTntRadioButton.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self)
end;

function TTntRadioButton.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TTntRadioButton.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntRadioButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntRadioButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

{ TTntScrollBar }

procedure TTntScrollBar.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, 'SCROLLBAR');
end;

procedure TTntScrollBar.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntScrollBar.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self)
end;

function TTntScrollBar.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TTntScrollBar.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntScrollBar.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntScrollBar.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

{ TTntCustomGroupBox }

procedure TTntCustomGroupBox.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, '');
end;

procedure TTntCustomGroupBox.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

procedure TTntCustomGroupBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsWideCharAccel(Message.CharCode, Caption)
    and CanFocus then
    begin
      SelectFirst;
      Result := 1;
    end else
      Broadcast(Message);
end;

function TTntCustomGroupBox.IsCaptionStored: Boolean;
begin
  Result := TntControl_IsCaptionStored(Self);
end;

function TTntCustomGroupBox.GetCaption: TWideCaption;
begin
  Result := TntControl_GetText(Self)
end;

procedure TTntCustomGroupBox.SetCaption(const Value: TWideCaption);
begin
  TntControl_SetText(Self, Value);
end;

procedure TTntCustomGroupBox.Paint;

  {$IFDEF THEME_7_UP}
  procedure PaintThemedGroupBox;
  var
    CaptionRect: TRect;
    OuterRect: TRect;
    Size: TSize;
    Box: TThemedButton;
    Details: TThemedElementDetails;
  begin
    with Canvas do begin
      if Caption <> '' then
      begin
        GetTextExtentPoint32W(Handle, PWideChar(Caption), Length(Caption), Size);
        CaptionRect := Rect(0, 0, Size.cx, Size.cy);
        if not UseRightToLeftAlignment then
          OffsetRect(CaptionRect, 8, 0)
        else
          OffsetRect(CaptionRect, Width - 8 - CaptionRect.Right, 0);
      end
      else
        CaptionRect := Rect(0, 0, 0, 0);

      OuterRect := ClientRect;
      OuterRect.Top := (CaptionRect.Bottom - CaptionRect.Top) div 2;
      with CaptionRect do
        ExcludeClipRect(Handle, Left, Top, Right, Bottom);
      if Enabled then
        Box := tbGroupBoxNormal
      else
        Box := tbGroupBoxDisabled;
      Details := ThemeServices.GetElementDetails(Box);
      ThemeServices.DrawElement(Handle, Details, OuterRect);

      SelectClipRgn(Handle, 0);
      if Text <> '' then
        ThemeServices.DrawText{TNT-ALLOW DrawText}(Handle, Details, Caption, CaptionRect, DT_LEFT, 0);
    end;
  end;
  {$ENDIF}

  procedure PaintGroupBox;
  var
    H: Integer;
    R: TRect;
    Flags: Longint;
  begin
    with Canvas do begin
      H := WideCanvasTextHeight(Canvas, '0');
      R := Rect(0, H div 2 - 1, Width, Height);
      if Ctl3D then
      begin
        Inc(R.Left);
        Inc(R.Top);
        Brush.Color := clBtnHighlight;
        FrameRect(R);
        OffsetRect(R, -1, -1);
        Brush.Color := clBtnShadow;
      end else
        Brush.Color := clWindowFrame;
      FrameRect(R);
      if Caption <> '' then
      begin
        if not UseRightToLeftAlignment then
          R := Rect(8, 0, 0, H)
        else
          R := Rect(R.Right - WideCanvasTextWidth(Canvas, Caption) - 8, 0, 0, H);
        Flags := DrawTextBiDiModeFlags(DT_SINGLELINE);
        Tnt_DrawTextW(Handle, PWideChar(Caption), Length(Caption), R, Flags or DT_CALCRECT);
        Brush.Color := Color;
        Tnt_DrawTextW(Handle, PWideChar(Caption), Length(Caption), R, Flags);
      end;
    end;
  end;

begin
  if (not Win32PlatformIsUnicode) then
    inherited
  else
  begin
    Canvas.Font := Self.Font;
    {$IFDEF THEME_7_UP}
    if ThemeServices.ThemesEnabled then
      PaintThemedGroupBox
    else
      PaintGroupBox;
    {$ELSE}
    PaintGroupBox;
    {$ENDIF}
  end;
end;

function TTntCustomGroupBox.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self)
end;

function TTntCustomGroupBox.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self);
end;

procedure TTntCustomGroupBox.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntCustomGroupBox.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntCustomGroupBox.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

{ TTntCustomStaticText }

constructor TTntCustomStaticText.Create(AOwner: TComponent);
begin
  inherited;
  AdjustBounds;
end;

procedure TTntCustomStaticText.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
end;

procedure TTntCustomStaticText.CMTextChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
end;

procedure TTntCustomStaticText.Loaded;
begin
  inherited;
  AdjustBounds;
end;

procedure TTntCustomStaticText.SetAutoSize(AValue: boolean);
begin
  inherited;
  if AValue then
    AdjustBounds;
end;

procedure TTntCustomStaticText.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, 'STATIC');
end;

procedure TTntCustomStaticText.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

procedure TTntCustomStaticText.CMDialogChar(var Message: TCMDialogChar);
begin
  if (FocusControl <> nil) and Enabled and ShowAccelChar and
    IsWideCharAccel(Message.CharCode, Caption) then
    with FocusControl do
    if CanFocus then
    begin
      SetFocus;
      Message.Result := 1;
    end;
end;

function TTntCustomStaticText.IsCaptionStored: Boolean;
begin
  Result := TntControl_IsCaptionStored(Self)
end;

procedure TTntCustomStaticText.AdjustBounds;
var
  DC: HDC;
  SaveFont: HFont;
  TextSize: TSize;
begin
  if not (csReading in ComponentState) and AutoSize then
  begin
    DC := GetDC(0);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextExtentPoint32W(DC, PWideChar(Caption), Length(Caption), TextSize);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);
    SetBounds(Left, Top,
      TextSize.cx + (GetSystemMetrics(SM_CXBORDER) * 4),
      TextSize.cy + (GetSystemMetrics(SM_CYBORDER) * 4));
  end;
end;

function TTntCustomStaticText.GetCaption: TWideCaption;
begin
  Result := TntControl_GetText(Self)
end;

procedure TTntCustomStaticText.SetCaption(const Value: TWideCaption);
begin
  TntControl_SetText(Self, Value);
end;

function TTntCustomStaticText.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self)
end;

function TTntCustomStaticText.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TTntCustomStaticText.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntCustomStaticText.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntCustomStaticText.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

end.
