
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntDBCtrls;

{$INCLUDE compilers.inc}

interface

uses
  Classes, Windows, Messages, DB, DBCtrls, Controls, StdCtrls,
  TntClasses, TntStdCtrls, TntControls, TntComCtrls, TntExtCtrls;

type
{TNT-WARN TPaintControl}
  TTntPaintControl = class
  private
    FOwner: TWinControl;
    FClassName: WideString;
    FHandle: HWnd;
    FObjectInstance: Pointer;
    FDefWindowProc: Pointer;
    FCtl3dButton: Boolean;
    function GetHandle: HWnd;
    procedure SetCtl3DButton(Value: Boolean);
    procedure WndProc(var Message: TMessage);
  public
    constructor Create(AOwner: TWinControl; const ClassName: WideString);
    destructor Destroy; override;
    procedure DestroyHandle;
    property Ctl3DButton: Boolean read FCtl3dButton write SetCtl3dButton;
    property Handle: HWnd read GetHandle;
  end;

type
{TNT-WARN TDBEdit}
  TTntDBEdit = class(TDBEdit{TNT-ALLOW TDBEdit})
  private
    InheritedDataChange: TNotifyEvent;
    FPasswordChar: WideChar;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsHintStored: Boolean;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    function GetTextMargins: TPoint;
    function GetPasswordChar: WideChar;
    procedure SetPasswordChar(const Value: WideChar);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
  private
    function GetSelStart: Integer; reintroduce; virtual;
    procedure SetSelStart(const Value: Integer); reintroduce; virtual;
    function GetSelLength: Integer; reintroduce; virtual;
    procedure SetSelLength(const Value: Integer); reintroduce; virtual;
    function GetSelText: WideString; reintroduce;
    procedure SetSelText(const Value: WideString);
    function GetText: WideString;
    procedure SetText(const Value: WideString);
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    property SelText: WideString read GetSelText write SetSelText;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property Text: WideString read GetText write SetText;
  published
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
    property PasswordChar: WideChar read GetPasswordChar write SetPasswordChar default #0;
  end;

{TNT-WARN TDBText}
  TTntDBText = class(TDBText{TNT-ALLOW TDBText})
  private
    FDataLink: TFieldDataLink;
    InheritedDataChange: TNotifyEvent;
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsHintStored: Boolean;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    function GetCaption: TWideCaption;
    function IsCaptionStored: Boolean;
    procedure SetCaption(const Value: TWideCaption);
    function GetFieldText: WideString;
    procedure DataChange(Sender: TObject);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetLabelText: WideString; reintroduce; virtual;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Caption: TWideCaption read GetCaption write SetCaption stored IsCaptionStored;
  published
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  end;

{TNT-WARN TDBComboBox}
  TTntCustomDBComboBox = class(TDBComboBox{TNT-ALLOW TDBComboBox},
    IWideCustomListControl)
  private
    FDataLink: TFieldDataLink;
    FFilter: WideString;
    FLastTime: Cardinal;
    procedure UpdateData(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure SetReadOnly;
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsHintStored: Boolean;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
  private
    FItems: TTntStrings;
    FSaveItems: TTntStrings;
    FSaveItemIndex: integer;
    function GetItems: TTntStrings;
    procedure SetItems(const Value: TTntStrings); reintroduce;
    function GetSelStart: Integer;
    procedure SetSelStart(const Value: Integer);
    function GetSelLength: Integer;
    procedure SetSelLength(const Value: Integer);
    function GetSelText: WideString;
    procedure SetSelText(const Value: WideString);
    function GetText: WideString;
    procedure SetText(const Value: WideString);

    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  protected
    procedure DataChange(Sender: TObject);
    function GetAutoComplete_UniqueMatchOnly: Boolean; dynamic;
    function GetAutoComplete_PreserveDataEntryCase: Boolean; dynamic;
    procedure DoEditCharMsg(var Message: TWMChar); virtual;
    function GetFieldValue: Variant; virtual;
    procedure SetFieldValue(const Value: Variant); virtual;
    function GetComboValue: Variant; virtual; abstract;
    procedure SetComboValue(const Value: Variant); virtual; abstract;
    {$IFDEF DELPHI_7} // fix for Delphi 7 only
    function GetItemsClass: TCustomComboBoxStringsClass; override;
    {$ENDIF}
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure WndProc(var Message: TMessage); override;
    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer); override;
    procedure KeyPress(var Key: AnsiChar); override;
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
  published
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
    property Items: TTntStrings read GetItems write SetItems;
  end;

  TTntDBComboBox = class(TTntCustomDBComboBox)
  protected
    function GetFieldValue: Variant; override;
    procedure SetFieldValue(const Value: Variant); override;
    function GetComboValue: Variant; override;
    procedure SetComboValue(const Value: Variant); override;
  end;

type
{TNT-WARN TDBCheckBox}
  TTntDBCheckBox = class(TDBCheckBox{TNT-ALLOW TDBCheckBox})
  private
    function GetCaption: TWideCaption;
    procedure SetCaption(const Value: TWideCaption);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsCaptionStored: Boolean;
    function IsHintStored: Boolean;
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure Toggle; override;
  published
    property Caption: TWideCaption read GetCaption write SetCaption stored IsCaptionStored;
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  end;

{TNT-WARN TDBRichEdit}
  TTntDBRichEdit = class(TTntCustomRichEdit)
  private
    FDataLink: TFieldDataLink;
    FAutoDisplay: Boolean;
    FFocused: Boolean;
    FMemoLoaded: Boolean;
    FDataSave: AnsiString;
    procedure BeginEditing;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: WideString;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: WideString);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure SetAutoDisplay(Value: Boolean);
    procedure SetFocused(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
  protected
    procedure InternalLoadMemo; dynamic;
    procedure InternalSaveMemo; dynamic;
  protected
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: AnsiChar); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure LoadMemo; virtual;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DataField: WideString read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property HideScrollBars;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PlainText;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
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
    property OnResizeRequest;
    property OnSelectionChange;
    property OnProtectChange;
    property OnSaveClipboard;
    property OnStartDock;
    property OnStartDrag;
  end;

type
{TNT-WARN TDBMemo}
  TTntDBMemo = class(TTntCustomMemo)
  private
    FDataLink: TFieldDataLink;
    FAutoDisplay: Boolean;
    FFocused: Boolean;
    FMemoLoaded: Boolean;
    FPaintControl: TTntPaintControl;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: WideString;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: WideString);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure SetAutoDisplay(Value: Boolean);
    procedure SetFocused(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMUndo(var Message: TMessage); message WM_UNDO;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char{TNT-ALLOW Char}); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure LoadMemo; virtual;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DataField: WideString read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;    
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
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

{ TDBRadioGroup }
type
  TTntDBRadioGroup = class(TTntCustomRadioGroup)
  private
    FDataLink: TFieldDataLink;
    FValue: WideString;
    FValues: TTntStrings;
    FInSetValue: Boolean;
    FOnChange: TNotifyEvent;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function GetDataField: WideString;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    function GetButtonValue(Index: Integer): WideString;
    procedure SetDataField(const Value: WideString);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure SetValue(const Value: WideString);
    procedure SetItems(Value: TTntStrings);
    procedure SetValues(Value: TTntStrings);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure Change; dynamic;
    procedure Click; override;
    procedure KeyPress(var Key: Char{TNT-ALLOW Char}); override;
    function CanModify: Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property DataLink: TFieldDataLink read FDataLink;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
    property ItemIndex;
    property Value: WideString read FValue write SetValue;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property DataField: WideString read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Items write SetItems;
    {$IFDEF COMPILER_7_UP}
    property ParentBackground;
    {$ENDIF}
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Values: TTntStrings read FValues write SetValues;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF COMPILER_10_UP}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  Forms, SysUtils, Graphics, Variants, TntDB,
  TntActnList, TntGraphics, TntSysUtils, RichEdit, Mask;

function FieldIsBlobLike(Field: TField): Boolean;
begin
  Result := False;
  if Assigned(Field) then begin
    if (Field.IsBlob)
    or (Field.DataType in [Low(TBlobType).. High(TBlobType)]) then
      Result := True
    else if (Field is TWideStringField{TNT-ALLOW TWideStringField})
    and (Field.Size = MaxInt) then
      Result := True; { wide string field filling in for a blob field }
  end;
end;

{ TTntPaintControl }

type
  TAccessWinControl = class(TWinControl);

constructor TTntPaintControl.Create(AOwner: TWinControl; const ClassName: WideString);
begin
  FOwner := AOwner;
  FClassName := ClassName;
end;

destructor TTntPaintControl.Destroy;
begin
  DestroyHandle;
end;

procedure TTntPaintControl.DestroyHandle;
begin
  if FHandle <> 0 then DestroyWindow(FHandle);
  Classes.FreeObjectInstance(FObjectInstance);
  FHandle := 0;
  FObjectInstance := nil;
end;

function TTntPaintControl.GetHandle: HWnd;
var
  Params: TCreateParams;
begin
  if FHandle = 0 then
  begin
    FObjectInstance := Classes.MakeObjectInstance(WndProc);
    TAccessWinControl(FOwner).CreateParams(Params);
    Params.Style := Params.Style and not (WS_HSCROLL or WS_VSCROLL);
    if (not Win32PlatformIsUnicode) then begin
      with Params do
        FHandle := CreateWindowEx(ExStyle, PAnsiChar(AnsiString(FClassName)),
          PAnsiChar(TAccessWinControl(FOwner).Text), Style or WS_VISIBLE,
          X, Y, Width, Height, Application.Handle, 0, HInstance, nil);
      FDefWindowProc := Pointer(GetWindowLong(FHandle, GWL_WNDPROC));
      SetWindowLong(FHandle, GWL_WNDPROC, Integer(FObjectInstance));
    end else begin
      with Params do
        FHandle := CreateWindowExW(ExStyle, PWideChar(FClassName),
          PWideChar(TntControl_GetText(FOwner)), Style or WS_VISIBLE,
          X, Y, Width, Height, Application.Handle, 0, HInstance, nil);
      FDefWindowProc := Pointer(GetWindowLongW(FHandle, GWL_WNDPROC));
      SetWindowLongW(FHandle, GWL_WNDPROC, Integer(FObjectInstance));
    end;
    SendMessage(FHandle, WM_SETFONT, Integer(TAccessWinControl(FOwner).Font.Handle), 1);
  end;
  Result := FHandle;
end;

procedure TTntPaintControl.SetCtl3DButton(Value: Boolean);
begin
  if FHandle <> 0 then DestroyHandle;
  FCtl3DButton := Value;
end;

procedure TTntPaintControl.WndProc(var Message: TMessage);
begin
  with Message do
    if (Msg >= CN_CTLCOLORMSGBOX) and (Msg <= CN_CTLCOLORSTATIC) then
      Result := FOwner.Perform(Msg, WParam, LParam)
    else if (not Win32PlatformIsUnicode) then
      Result := CallWindowProcA(FDefWindowProc, FHandle, Msg, WParam, LParam)
    else
      Result := CallWindowProcW(FDefWindowProc, FHandle, Msg, WParam, LParam);
end;

{ THackFieldDataLink }
type
  THackFieldDataLink_D6_D7_D9 = class(TDataLink)
  protected
    FxxxField: TField;
    FxxxFieldName: string{TNT-ALLOW string};
    FxxxControl: TComponent;
    FxxxEditing: Boolean;
    FModified: Boolean;
  end;

{$IFDEF COMPILER_6}  // verified against VCL source in Delphi 6 and BCB 6
  THackFieldDataLink = THackFieldDataLink_D6_D7_D9;
{$ENDIF}
{$IFDEF DELPHI_7}    // verified against VCL source in Delphi 7
  THackFieldDataLink = THackFieldDataLink_D6_D7_D9;
{$ENDIF}
{$IFDEF DELPHI_9}    // verified against VCL source in Delphi 9
  THackFieldDataLink = THackFieldDataLink_D6_D7_D9;
{$ENDIF}
{$IFDEF DELPHI_10}    // verified against VCL source in Delphi 10
  THackFieldDataLink = class(TDataLink)
  protected
    FxxxField: TField;
    FxxxFieldName: WideString;
    FxxxControl: TComponent;
    FxxxEditing: Boolean;
    FModified: Boolean;
  end;
{$ENDIF}

{ TTntDBEdit }

type
  THackDBEdit_D6_D7_D9 = class(TCustomMaskEdit)
  protected
    FDataLink: TFieldDataLink;
    FCanvas: TControlCanvas;
    FAlignment: TAlignment;
    FFocused: Boolean;
  end;

{$IFDEF COMPILER_6}   // verified against VCL source in Delphi 6 and BCB 6
  THackDBEdit = THackDBEdit_D6_D7_D9;
{$ENDIF}
{$IFDEF DELPHI_7}     // verified against VCL source in Delphi 7
  THackDBEdit = THackDBEdit_D6_D7_D9;
{$ENDIF}
{$IFDEF DELPHI_9}     // verified against VCL source in Delphi 9
  THackDBEdit = THackDBEdit_D6_D7_D9;
{$ENDIF}
{$IFDEF DELPHI_10}     // verified against VCL source in Delphi 10
  THackDBEdit = THackDBEdit_D6_D7_D9;
{$ENDIF}

constructor TTntDBEdit.Create(AOwner: TComponent);
begin
  inherited;
  InheritedDataChange := THackDBEdit(Self).FDataLink.OnDataChange;
  THackDBEdit(Self).FDataLink.OnDataChange := DataChange;
  THackDBEdit(Self).FDataLink.OnUpdateData := UpdateData;
end;

procedure TTntDBEdit.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, 'EDIT');
end;

procedure TTntDBEdit.CreateWnd;
begin
  inherited;
  TntCustomEdit_AfterInherited_CreateWnd(Self, FPasswordChar);
end;

procedure TTntDBEdit.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntDBEdit.GetSelStart: Integer;
begin
  Result := TntCustomEdit_GetSelStart(Self);
end;

procedure TTntDBEdit.SetSelStart(const Value: Integer);
begin
  TntCustomEdit_SetSelStart(Self, Value);
end;

function TTntDBEdit.GetSelLength: Integer;
begin
  Result := TntCustomEdit_GetSelLength(Self);
end;

procedure TTntDBEdit.SetSelLength(const Value: Integer);
begin
  TntCustomEdit_SetSelLength(Self, Value);
end;

function TTntDBEdit.GetSelText: WideString;
begin
  Result := TntCustomEdit_GetSelText(Self);
end;

procedure TTntDBEdit.SetSelText(const Value: WideString);
begin
  TntCustomEdit_SetSelText(Self, Value);
end;

function TTntDBEdit.GetPasswordChar: WideChar;
begin
  Result := TntCustomEdit_GetPasswordChar(Self, FPasswordChar)
end;

procedure TTntDBEdit.SetPasswordChar(const Value: WideChar);
begin
  TntCustomEdit_SetPasswordChar(Self, FPasswordChar, Value);
end;

function TTntDBEdit.GetText: WideString;
begin
  Result := TntControl_GetText(Self);
end;

procedure TTntDBEdit.SetText(const Value: WideString);
begin
  TntControl_SetText(Self, Value);
end;

procedure TTntDBEdit.DataChange(Sender: TObject);
begin
  with THackDBEdit(Self), Self do begin
    if Field = nil then
      InheritedDataChange(Sender)
    else begin
      if FAlignment <> Field.Alignment then
      begin
        EditText := '';  {forces update}
        FAlignment := Field.Alignment;
      end;
      EditMask := Field.EditMask;
      if not (csDesigning in ComponentState) then
      begin
        if (Field.DataType in [ftString, ftWideString]) and (MaxLength = 0) then
          MaxLength := Field.Size;
      end;
      if FFocused and FDataLink.CanModify then
        Text := GetWideText(Field)
      else
      begin
        Text := GetWideDisplayText(Field);
        if FDataLink.Editing and THackFieldDataLink(FDataLink).FModified then
          Modified := True;
      end;
    end;
  end;
end;

procedure TTntDBEdit.UpdateData(Sender: TObject);
begin
  ValidateEdit;
  SetWideText(Field, Text);
end;

procedure TTntDBEdit.CMEnter(var Message: TCMEnter);
var
  SaveFarEast: Boolean;
begin
  SaveFarEast := SysLocale.FarEast;
  try
    SysLocale.FarEast := False;
    inherited; // inherited tries to work around Win95 FarEast bug, but introduces others
  finally
    SysLocale.FarEast := SaveFarEast;
  end;
end;

function TTntDBEdit.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self);
end;

function TTntDBEdit.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TTntDBEdit.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntDBEdit.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntDBEdit.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

procedure TTntDBEdit.WMPaint(var Message: TWMPaint);
const
  AlignStyle : array[Boolean, TAlignment] of DWORD =
   ((WS_EX_LEFT, WS_EX_RIGHT, WS_EX_LEFT),
    (WS_EX_RIGHT, WS_EX_LEFT, WS_EX_LEFT));
var
  ALeft: Integer;
  _Margins: TPoint;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: WideString;
  AAlignment: TAlignment;
  I: Integer;
begin
  with THackDBEdit(Self), Self do begin
    AAlignment := FAlignment;
    if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
    if ((AAlignment = taLeftJustify) or FFocused) and (not (csPaintCopy in ControlState))
    or (not Win32PlatformIsUnicode) then
    begin
      inherited;
      Exit;
    end;
  { Since edit controls do not handle justification unless multi-line (and
    then only poorly) we will draw right and center justify manually unless
    the edit has the focus. }
    if FCanvas = nil then
    begin
      FCanvas := TControlCanvas.Create;
      FCanvas.Control := Self;
    end;
    DC := Message.DC;
    if DC = 0 then DC := BeginPaint(Handle, PS);
    FCanvas.Handle := DC;
    try
      FCanvas.Font := Font;
      with FCanvas do
      begin
        R := ClientRect;
        if not (NewStyleControls and Ctl3D) and (BorderStyle = bsSingle) then
        begin
          Brush.Color := clWindowFrame;
          FrameRect(R);
          InflateRect(R, -1, -1);
        end;
        Brush.Color := Color;
        if not Enabled then
          Font.Color := clGrayText;
        if (csPaintCopy in ControlState) and (Field <> nil) then
        begin
          S := GetWideDisplayText(Field);
          case CharCase of
            ecUpperCase:
              S := Tnt_WideUpperCase(S);
            ecLowerCase:
              S := Tnt_WideLowerCase(S);
          end;
        end else
          S := Text { EditText? };
        if PasswordChar <> #0 then
          for I := 1 to Length(S) do S[I] := PasswordChar;
        _Margins := GetTextMargins;
        case AAlignment of
          taLeftJustify: ALeft := _Margins.X;
          taRightJustify: ALeft := ClientWidth - WideCanvasTextWidth(FCanvas, S) - _Margins.X - 1;
        else
          ALeft := (ClientWidth - WideCanvasTextWidth(FCanvas, S)) div 2;
        end;
        if SysLocale.MiddleEast then UpdateTextFlags;
        WideCanvasTextRect(FCanvas, R, ALeft, _Margins.Y, S);
      end;
    finally
      FCanvas.Handle := 0;
      if Message.DC = 0 then EndPaint(Handle, PS);
    end;
  end;
end;

function TTntDBEdit.GetTextMargins: TPoint;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  if NewStyleControls then
  begin
    if BorderStyle = bsNone then I := 0 else
      if Ctl3D then I := 1 else I := 2;
    Result.X := SendMessage(Handle, EM_GETMARGINS, 0, 0) and $0000FFFF + I;
    Result.Y := I;
  end else
  begin
    if BorderStyle = bsNone then I := 0 else
    begin
      DC := GetDC(0);
      GetTextMetrics(DC, SysMetrics);
      SaveFont := SelectObject(DC, Font.Handle);
      GetTextMetrics(DC, Metrics);
      SelectObject(DC, SaveFont);
      ReleaseDC(0, DC);
      I := SysMetrics.tmHeight;
      if I > Metrics.tmHeight then I := Metrics.tmHeight;
      I := I div 4;
    end;
    Result.X := I;
    Result.Y := I;
  end;
end;

{ TTntDBText }

constructor TTntDBText.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TDataLink(Perform(CM_GETDATALINK, 0, 0)) as TFieldDataLink;
  InheritedDataChange := FDataLink.OnDataChange;
  FDataLink.OnDataChange := DataChange;
end;

destructor TTntDBText.Destroy;
begin
  FDataLink := nil;
  inherited;
end;

procedure TTntDBText.CMDialogChar(var Message: TCMDialogChar);
begin
  TntLabel_CMDialogChar(Self, Message, Caption);
end;

function TTntDBText.IsCaptionStored: Boolean;
begin
  Result := TntControl_IsCaptionStored(Self)
end;

function TTntDBText.GetCaption: TWideCaption;
begin
  Result := TntControl_GetText(Self);
end;

procedure TTntDBText.SetCaption(const Value: TWideCaption);
begin
  TntControl_SetText(Self, Value);
end;

procedure TTntDBText.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntDBText.GetLabelText: WideString;
begin
  if csPaintCopy in ControlState then
    Result := GetFieldText
  else
    Result := Caption;
end;

procedure TTntDBText.DoDrawText(var Rect: TRect; Flags: Integer);
begin
  if not TntLabel_DoDrawText(Self, Rect, Flags, GetLabelText) then
    inherited;
end;

function TTntDBText.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self);
end;

function TTntDBText.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TTntDBText.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntDBText.CMHintShow(var Message: TMessage);
begin
  ProcessCMHintShowMsg(Message);
  inherited;
end;

procedure TTntDBText.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntDBText.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

function TTntDBText.GetFieldText: WideString;
begin
  if Field <> nil then
    Result := GetWideDisplayText(Field)
  else
    if csDesigning in ComponentState then Result := Name else Result := '';
end;

procedure TTntDBText.DataChange(Sender: TObject);
begin
  Caption := GetFieldText;
end;

{ TTntCustomDBComboBox }

constructor TTntCustomDBComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TTntComboBoxStrings.Create;
  TTntComboBoxStrings(FItems).ComboBox := Self;
  FDataLink := TDataLink(Perform(CM_GETDATALINK, 0, 0)) as TFieldDataLink;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnEditingChange := EditingChange;
end;

destructor TTntCustomDBComboBox.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FSaveItems);
  FDataLink := nil;
  inherited;
end;

procedure TTntCustomDBComboBox.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, 'COMBOBOX');
end;

procedure TTntCustomDBComboBox.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

type
  TAccessCustomComboBox = class(TCustomComboBox{TNT-ALLOW TCustomComboBox});

procedure TTntCustomDBComboBox.CreateWnd;
var
  PreInheritedAnsiText: AnsiString;
begin
  PreInheritedAnsiText := TAccessCustomComboBox(Self).Text;
  inherited;
  TntCombo_AfterInherited_CreateWnd(Self, Items, FSaveItems, FSaveItemIndex, PreInheritedAnsiText);
end;

procedure TTntCustomDBComboBox.DestroyWnd;
var
  SavedText: WideString;
begin
  if not (csDestroyingHandle in ControlState) then begin { avoid recursion when parent is TToolBar and system font changes. }
    TntCombo_BeforeInherited_DestroyWnd(Self, Items, FSaveItems, ItemIndex, FSaveItemIndex, SavedText);
    inherited;
    TntControl_SetStoredText(Self, SavedText);
  end;
end;

procedure TTntCustomDBComboBox.SetReadOnly;
begin
  if (Style in [csDropDown, csSimple]) and HandleAllocated then
    SendMessage(EditHandle, EM_SETREADONLY, Ord(not FDataLink.CanModify), 0);
end;

procedure TTntCustomDBComboBox.EditingChange(Sender: TObject);
begin
  SetReadOnly;
end;

procedure TTntCustomDBComboBox.CMEnter(var Message: TCMEnter);
var
  SaveFarEast: Boolean;
begin
  SaveFarEast := SysLocale.FarEast;
  try
    SysLocale.FarEast := False;
    inherited; // inherited tries to work around Win95 FarEast bug, but introduces others
  finally
    SysLocale.FarEast := SaveFarEast;
  end;
end;

procedure TTntCustomDBComboBox.WndProc(var Message: TMessage);
begin
  if (not (csDesigning in ComponentState))
  and (Message.Msg = CB_SHOWDROPDOWN)
  and (Message.WParam = 0)
  and (not FDataLink.Editing) then begin
    DataChange(Self); {Restore text}
    Dispatch(Message); {Do NOT call inherited!}
  end else
    inherited WndProc(Message);
end;

procedure TTntCustomDBComboBox.ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer);
begin
  if not TntCombo_ComboWndProc(Self, Message, ComboWnd, ComboProc, DoEditCharMsg) then
    inherited;
end;

procedure TTntCustomDBComboBox.KeyPress(var Key: AnsiChar);
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

procedure TTntCustomDBComboBox.DoEditCharMsg(var Message: TWMChar);
begin
  TntCombo_AutoCompleteKeyPress(Self, Items, Message,
    GetAutoComplete_UniqueMatchOnly, GetAutoComplete_PreserveDataEntryCase);
end;

procedure TTntCustomDBComboBox.WMChar(var Message: TWMChar);
begin
  TntCombo_AutoSearchKeyPress(Self, Items, Message, FFilter, FLastTime);
  inherited;
end;

function TTntCustomDBComboBox.GetItems: TTntStrings;
begin
  Result := FItems;
end;

procedure TTntCustomDBComboBox.SetItems(const Value: TTntStrings);
begin
  FItems.Assign(Value);
  DataChange(Self);
end;

function TTntCustomDBComboBox.GetSelStart: Integer;
begin
  Result := TntCombo_GetSelStart(Self);
end;

procedure TTntCustomDBComboBox.SetSelStart(const Value: Integer);
begin
  TntCombo_SetSelStart(Self, Value);
end;

function TTntCustomDBComboBox.GetSelLength: Integer;
begin
  Result := TntCombo_GetSelLength(Self);
end;

procedure TTntCustomDBComboBox.SetSelLength(const Value: Integer);
begin
  TntCombo_SetSelLength(Self, Value);
end;

function TTntCustomDBComboBox.GetSelText: WideString;
begin
  Result := TntCombo_GetSelText(Self);
end;

procedure TTntCustomDBComboBox.SetSelText(const Value: WideString);
begin
  TntCombo_SetSelText(Self, Value);
end;

function TTntCustomDBComboBox.GetText: WideString;
begin
  Result := TntControl_GetText(Self);
end;

procedure TTntCustomDBComboBox.SetText(const Value: WideString);
begin
  TntControl_SetText(Self, Value);
end;

procedure TTntCustomDBComboBox.CNCommand(var Message: TWMCommand);
begin
  if not TntCombo_CNCommand(Self, Items, Message) then
    inherited;
end;

function TTntCustomDBComboBox.GetFieldValue: Variant;
begin
  Result := Field.Value;
end;

procedure TTntCustomDBComboBox.SetFieldValue(const Value: Variant);
begin
  Field.Value := Value;
end;

procedure TTntCustomDBComboBox.DataChange(Sender: TObject);
begin
  if not (Style = csSimple) and DroppedDown then Exit;
  if Field <> nil then
    SetComboValue(GetFieldValue)
  else
    if csDesigning in ComponentState then
      SetComboValue(Name)
    else
      SetComboValue(Null);
end;

procedure TTntCustomDBComboBox.UpdateData(Sender: TObject);
begin
  SetFieldValue(GetComboValue);
end;

function TTntCustomDBComboBox.GetAutoComplete_PreserveDataEntryCase: Boolean;
begin
  Result := True;
end;

function TTntCustomDBComboBox.GetAutoComplete_UniqueMatchOnly: Boolean;
begin
  Result := False;
end;

function TTntCustomDBComboBox.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self);
end;

function TTntCustomDBComboBox.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TTntCustomDBComboBox.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntCustomDBComboBox.AddItem(const Item: WideString; AObject: TObject);
begin
  TntComboBox_AddItem(Items, Item, AObject);
end;

procedure TTntCustomDBComboBox.CopySelection(Destination: TCustomListControl);
begin
  TntComboBox_CopySelection(Items, ItemIndex, Destination);
end;

procedure TTntCustomDBComboBox.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntCustomDBComboBox.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

{$IFDEF DELPHI_7} // fix for Delphi 7 only
function TTntCustomDBComboBox.GetItemsClass: TCustomComboBoxStringsClass;
begin
  Result := TD7PatchedComboBoxStrings;
end;
{$ENDIF}

{ TTntDBComboBox }

function TTntDBComboBox.GetFieldValue: Variant;
begin
  Result := GetWideText(Field);
end;

procedure TTntDBComboBox.SetFieldValue(const Value: Variant);
begin
  SetWideText(Field, Value);
end;

procedure TTntDBComboBox.SetComboValue(const Value: Variant);
var
  I: Integer;
  Redraw: Boolean;
  OldValue: WideString;
  NewValue: WideString;
begin
  OldValue := VarToWideStr(GetComboValue);
  NewValue := VarToWideStr(Value);

  if NewValue <> OldValue then
  begin
    if Style <> csDropDown then
    begin
      Redraw := (Style <> csSimple) and HandleAllocated;
      if Redraw then Items.BeginUpdate;
      try
        if NewValue = '' then I := -1 else I := Items.IndexOf(NewValue);
        ItemIndex := I;
      finally
        Items.EndUpdate;
      end;
      if I >= 0 then Exit;
    end;
    if Style in [csDropDown, csSimple] then Text := NewValue;
  end;
end;

function TTntDBComboBox.GetComboValue: Variant;
var
  I: Integer;
begin
  if Style in [csDropDown, csSimple] then Result := Text else
  begin
    I := ItemIndex;
    if I < 0 then Result := '' else Result := Items[I];
  end;
end;

{ TTntDBCheckBox }

procedure TTntDBCheckBox.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, 'BUTTON');
end;

procedure TTntDBCheckBox.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntDBCheckBox.IsCaptionStored: Boolean;
begin
  Result := TntControl_IsCaptionStored(Self);
end;

function TTntDBCheckBox.GetCaption: TWideCaption;
begin
  Result := TntControl_GetText(Self)
end;

procedure TTntDBCheckBox.SetCaption(const Value: TWideCaption);
begin
  TntControl_SetText(Self, Value);
end;

function TTntDBCheckBox.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self);
end;

function TTntDBCheckBox.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TTntDBCheckBox.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntDBCheckBox.Toggle;
var
  FDataLink: TDataLink;
begin
  inherited;
  FDataLink := TDataLink(Perform(CM_GETDATALINK, 0, 0)) as TFieldDataLink;
  FDataLink.UpdateRecord;
end;

procedure TTntDBCheckBox.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntDBCheckBox.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

{ TTntDBRichEdit }

constructor TTntDBRichEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited ReadOnly := True;
  FAutoDisplay := True;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TTntDBRichEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TTntDBRichEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self)
end;

procedure TTntDBRichEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TTntDBRichEdit.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

procedure TTntDBRichEdit.BeginEditing;
begin
  if not FDataLink.Editing then
  try
    if FieldIsBlobLike(Field) then
      FDataSave := Field.AsString{TNT-ALLOW AsString};
    FDataLink.Edit;
  finally
    FDataSave := '';
  end;
end;

procedure TTntDBRichEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if FMemoLoaded then
  begin
    if (Key = VK_DELETE) or (Key = VK_BACK) or
      ((Key = VK_INSERT) and (ssShift in Shift)) or
      (((Key = Ord('V')) or (Key = Ord('X'))) and (ssCtrl in Shift)) then
      BeginEditing;
  end;
end;

procedure TTntDBRichEdit.KeyPress(var Key: AnsiChar);
begin
  inherited KeyPress(Key);
  if FMemoLoaded then
  begin
    if (Key in [#32..#255]) and (Field <> nil) and
      not Field.IsValidChar(Key) then
    begin
      MessageBeep(0);
      Key := #0;
    end;
    case Key of
      ^H, ^I, ^J, ^M, ^V, ^X, #32..#255:
        BeginEditing;
      #27:
        FDataLink.Reset;
    end;
  end else
  begin
    if Key = #13 then LoadMemo;
    Key := #0;
  end;
end;

procedure TTntDBRichEdit.Change;
begin
  if FMemoLoaded then
    FDataLink.Modified;
  FMemoLoaded := True;
  inherited Change;
end;

procedure TTntDBRichEdit.CNNotify(var Message: TWMNotify);
begin
  inherited;
  if Message.NMHdr^.code = EN_PROTECTED then
    Message.Result := 0 { allow the operation (otherwise the control might appear stuck) }
end;

function TTntDBRichEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TTntDBRichEdit.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TTntDBRichEdit.GetDataField: WideString;
begin
  Result := FDataLink.FieldName;
end;

procedure TTntDBRichEdit.SetDataField(const Value: WideString);
begin
  FDataLink.FieldName := Value;
end;

function TTntDBRichEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TTntDBRichEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TTntDBRichEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TTntDBRichEdit.InternalLoadMemo;
var
  Stream: TStringStream{TNT-ALLOW TStringStream};
begin
  if PlainText then
    Text := GetAsWideString(Field)
  else begin
    Stream := TStringStream{TNT-ALLOW TStringStream}.Create(Field.AsString{TNT-ALLOW AsString});
    try
      Lines.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;

procedure TTntDBRichEdit.LoadMemo;
begin
  if not FMemoLoaded and Assigned(Field) and FieldIsBlobLike(Field) then
  begin
    try
      InternalLoadMemo;
      FMemoLoaded := True;
    except
      { Rich Edit Load failure }
      on E:EOutOfResources do
        Lines.Text := WideFormat('(%s)', [E.Message]);
    end;
    EditingChange(Self);
  end;
end;

procedure TTntDBRichEdit.DataChange(Sender: TObject);
begin
  if Field <> nil then
    if FieldIsBlobLike(Field) then
    begin
      if FAutoDisplay or (FDataLink.Editing and FMemoLoaded) then
      begin
        { Check if the data has changed since we read it the first time }
        if (FDataSave <> '') and (FDataSave = Field.AsString{TNT-ALLOW AsString}) then Exit;
        FMemoLoaded := False;
        LoadMemo;
      end else
      begin
        Text := WideFormat('(%s)', [Field.DisplayName]);
        FMemoLoaded := False;
      end;
    end else
    begin
      if FFocused and FDataLink.CanModify then
        Text := GetWideText(Field)
      else
        Text := GetWideDisplayText(Field);
      FMemoLoaded := True;
    end
  else
  begin
    if csDesigning in ComponentState then Text := Name else Text := '';
    FMemoLoaded := False;
  end;
  if HandleAllocated then
    RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_FRAME);
end;

procedure TTntDBRichEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not (FDataLink.Editing and FMemoLoaded);
end;

procedure TTntDBRichEdit.InternalSaveMemo;
var
  Stream: TStringStream{TNT-ALLOW TStringStream};
begin
  if PlainText then
    SetAsWideString(Field, Text)
  else begin
    Stream := TStringStream{TNT-ALLOW TStringStream}.Create('');
    try
      Lines.SaveToStream(Stream);
      Field.AsString{TNT-ALLOW AsString} := Stream.DataString;
    finally
      Stream.Free;
    end;
  end;
end;

procedure TTntDBRichEdit.UpdateData(Sender: TObject);
begin
  if FieldIsBlobLike(Field) then
    InternalSaveMemo
  else
    SetAsWideString(Field, Text);
end;

procedure TTntDBRichEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if not Assigned(Field) or not FieldIsBlobLike(Field) then
      FDataLink.Reset;
  end;
end;

procedure TTntDBRichEdit.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
end;

procedure TTntDBRichEdit.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  SetFocused(False);
  inherited;
end;

procedure TTntDBRichEdit.SetAutoDisplay(Value: Boolean);
begin
  if FAutoDisplay <> Value then
  begin
    FAutoDisplay := Value;
    if Value then LoadMemo;
  end;
end;

procedure TTntDBRichEdit.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if not FMemoLoaded then LoadMemo else inherited;
end;

procedure TTntDBRichEdit.WMCut(var Message: TMessage);
begin
  BeginEditing;
  inherited;
end;

procedure TTntDBRichEdit.WMPaste(var Message: TMessage);
begin
  BeginEditing;
  inherited;
end;

procedure TTntDBRichEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TTntDBRichEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TTntDBRichEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

{ TTntDBMemo }

constructor TTntDBMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited ReadOnly := True;
  ControlStyle := ControlStyle + [csReplicatable];
  FAutoDisplay := True;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  FPaintControl := TTntPaintControl.Create(Self, 'EDIT');
end;

destructor TTntDBMemo.Destroy;
begin
  FPaintControl.Free;
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TTntDBMemo.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TTntDBMemo.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TTntDBMemo.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

procedure TTntDBMemo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if FMemoLoaded then
  begin
    if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
      FDataLink.Edit;
  end;
end;

procedure TTntDBMemo.KeyPress(var Key: Char{TNT-ALLOW Char});
begin
  inherited KeyPress(Key);
  if FMemoLoaded then
  begin
    if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
      not FDataLink.Field.IsValidChar(Key) then
    begin
      MessageBeep(0);
      Key := #0;
    end;
    case Key of
      ^H, ^I, ^J, ^M, ^V, ^X, #32..#255:
        FDataLink.Edit;
      #27:
        FDataLink.Reset;
    end;
  end else
  begin
    if Key = #13 then LoadMemo;
    Key := #0;
  end;
end;

procedure TTntDBMemo.Change;
begin
  if FMemoLoaded then FDataLink.Modified;
  FMemoLoaded := True;
  inherited Change;
end;

function TTntDBMemo.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TTntDBMemo.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TTntDBMemo.GetDataField: WideString;
begin
  Result := FDataLink.FieldName;
end;

procedure TTntDBMemo.SetDataField(const Value: WideString);
begin
  FDataLink.FieldName := Value;
end;

function TTntDBMemo.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TTntDBMemo.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TTntDBMemo.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TTntDBMemo.LoadMemo;
begin
  if not FMemoLoaded and Assigned(FDataLink.Field) and FieldIsBlobLike(FDataLink.Field) then
  begin
    try
      Lines.Text := GetAsWideString(FDataLink.Field);
      FMemoLoaded := True;
    except
      { Memo too large }
      on E:EInvalidOperation do
        Lines.Text := WideFormat('(%s)', [E.Message]);
    end;
    EditingChange(Self);
  end;
end;

procedure TTntDBMemo.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    if FieldIsBlobLike(FDataLink.Field) then
    begin
      if FAutoDisplay or (FDataLink.Editing and FMemoLoaded) then
      begin
        FMemoLoaded := False;
        LoadMemo;
      end else
      begin
        Text := WideFormat('(%s)', [FDataLink.Field.DisplayName]);
        FMemoLoaded := False;
        EditingChange(Self);
      end;
    end else
    begin
      if FFocused and FDataLink.CanModify then
        Text := GetWideText(FDataLink.Field)
      else
        Text := GetWideDisplayText(FDataLink.Field);
      FMemoLoaded := True;
    end
  else
  begin
    if csDesigning in ComponentState then Text := Name else Text := '';
    FMemoLoaded := False;
  end;
  if HandleAllocated then
    RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_FRAME);
end;

procedure TTntDBMemo.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not (FDataLink.Editing and FMemoLoaded);
end;

procedure TTntDBMemo.UpdateData(Sender: TObject);
begin
  SetAsWideString(FDataLink.Field, Text);
end;

procedure TTntDBMemo.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if not Assigned(FDataLink.Field) or not FieldIsBlobLike(FDataLink.Field) then
      FDataLink.Reset;
  end;
end;

procedure TTntDBMemo.WndProc(var Message: TMessage);
begin
  with Message do
    if (Msg = WM_CREATE) or (Msg = WM_WINDOWPOSCHANGED) or
      (Msg = CM_FONTCHANGED) then FPaintControl.DestroyHandle;
  inherited;
end;

procedure TTntDBMemo.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
end;

procedure TTntDBMemo.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  SetFocused(False);
  inherited;
end;

procedure TTntDBMemo.SetAutoDisplay(Value: Boolean);
begin
  if FAutoDisplay <> Value then
  begin
    FAutoDisplay := Value;
    if Value then LoadMemo;
  end;
end;

procedure TTntDBMemo.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if not FMemoLoaded then LoadMemo else inherited;
end;

procedure TTntDBMemo.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TTntDBMemo.WMUndo(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TTntDBMemo.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TTntDBMemo.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

procedure TTntDBMemo.WMPaint(var Message: TWMPaint);
var
  S: WideString;
begin
  if not (csPaintCopy in ControlState) then
    inherited
  else begin
    if FDataLink.Field <> nil then
      if FieldIsBlobLike(FDataLink.Field) then
      begin
        if FAutoDisplay then
          S := TntAdjustLineBreaks(GetAsWideString(FDataLink.Field)) else
          S := WideFormat('(%s)', [FDataLink.Field.DisplayName]);
      end else
        S := GetWideDisplayText(FDataLink.Field);
    if (not Win32PlatformIsUnicode) then
      SendMessageA(FPaintControl.Handle, WM_SETTEXT, 0, Integer(PAnsiChar(AnsiString(S))))
    else begin
      SendMessageW(FPaintControl.Handle, WM_SETTEXT, 0, Integer(PWideChar(S)));
    end;
    SendMessage(FPaintControl.Handle, WM_ERASEBKGND, Integer(Message.DC), 0);
    SendMessage(FPaintControl.Handle, WM_PAINT, Integer(Message.DC), 0);
  end;
end;

function TTntDBMemo.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TTntDBMemo.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

{ TTntDBRadioGroup }

constructor TTntDBRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FValues := TTntStringList.Create;
end;

destructor TTntDBRadioGroup.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  FValues.Free;
  inherited Destroy;
end;

procedure TTntDBRadioGroup.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TTntDBRadioGroup.UseRightToLeftAlignment: Boolean;
begin
  Result := inherited UseRightToLeftAlignment;
end;

procedure TTntDBRadioGroup.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    Value := GetWideText(FDataLink.Field) else
    Value := '';
end;

procedure TTntDBRadioGroup.UpdateData(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    SetWideText(FDataLink.Field, Value);
end;

function TTntDBRadioGroup.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TTntDBRadioGroup.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TTntDBRadioGroup.GetDataField: WideString;
begin
  Result := FDataLink.FieldName;
end;

procedure TTntDBRadioGroup.SetDataField(const Value: WideString);
begin
  FDataLink.FieldName := Value;
end;

function TTntDBRadioGroup.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TTntDBRadioGroup.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TTntDBRadioGroup.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TTntDBRadioGroup.GetButtonValue(Index: Integer): WideString;
begin
  if (Index < FValues.Count) and (FValues[Index] <> '') then
    Result := FValues[Index]
  else if Index < Items.Count then
    Result := Items[Index]
  else
    Result := '';
end;

procedure TTntDBRadioGroup.SetValue(const Value: WideString);
var
  WasFocused: Boolean;
  I, Index: Integer;
begin
  if FValue <> Value then
  begin
    FInSetValue := True;
    try
      WasFocused := (ItemIndex > -1) and (Buttons[ItemIndex].Focused);
      Index := -1;
      for I := 0 to Items.Count - 1 do
        if Value = GetButtonValue(I) then
        begin
          Index := I;
          Break;
        end;
      ItemIndex := Index;
      // Move the focus rect along with the selected index
      if WasFocused then
        Buttons[ItemIndex].SetFocus;
    finally
      FInSetValue := False;
    end;
    FValue := Value;
    Change;
  end;
end;

procedure TTntDBRadioGroup.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    if ItemIndex >= 0 then
      (Controls[ItemIndex] as TTntRadioButton).SetFocus else
      (Controls[0] as TTntRadioButton).SetFocus;
    raise;
  end;
  inherited;
end;

procedure TTntDBRadioGroup.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

procedure TTntDBRadioGroup.Click;
begin
  if not FInSetValue then
  begin
    inherited Click;
    if ItemIndex >= 0 then Value := GetButtonValue(ItemIndex);
    if FDataLink.Editing then FDataLink.Modified;
  end;
end;

procedure TTntDBRadioGroup.SetItems(Value: TTntStrings);
begin
  Items.Assign(Value);
  DataChange(Self);
end;

procedure TTntDBRadioGroup.SetValues(Value: TTntStrings);
begin
  FValues.Assign(Value);
  DataChange(Self);
end;

procedure TTntDBRadioGroup.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TTntDBRadioGroup.KeyPress(var Key: Char{TNT-ALLOW Char});
begin
  inherited KeyPress(Key);
  case Key of
    #8, ' ': FDataLink.Edit;
    #27: FDataLink.Reset;
  end;
end;

function TTntDBRadioGroup.CanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

function TTntDBRadioGroup.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (DataLink <> nil) and
    DataLink.ExecuteAction(Action);
end;

function TTntDBRadioGroup.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (DataLink <> nil) and
    DataLink.UpdateAction(Action);
end;

end.
