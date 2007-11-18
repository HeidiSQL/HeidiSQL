
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntExtCtrls;

{$INCLUDE TntCompilers.inc}

interface

uses
  Classes, Messages, Controls, ExtCtrls, TntClasses, TntControls, TntStdCtrls, TntGraphics;

type
{TNT-WARN TShape}
  TTntShape = class(TShape{TNT-ALLOW TShape})
  private
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsHintStored: Boolean;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  published
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  end;

{TNT-WARN TPaintBox}
  TTntPaintBox = class(TPaintBox{TNT-ALLOW TPaintBox})
  private
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsHintStored: Boolean;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  published
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  end;

{TNT-WARN TImage}
  TTntImage = class(TImage{TNT-ALLOW TImage})
  private
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsHintStored: Boolean;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    function GetPicture: TTntPicture;
    procedure SetPicture(const Value: TTntPicture);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
    property Picture: TTntPicture read GetPicture write SetPicture;
  end;

{TNT-WARN TBevel}
  TTntBevel = class(TBevel{TNT-ALLOW TBevel})
  private
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsHintStored: Boolean;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  published
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  end;

{TNT-WARN TCustomPanel}
  TTntCustomPanel = class(TCustomPanel{TNT-ALLOW TCustomPanel})
  private
    function GetCaption: TWideCaption;
    procedure SetCaption(const Value: TWideCaption);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsCaptionStored: Boolean;
    function IsHintStored: Boolean;
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

{TNT-WARN TPanel}
  TTntPanel = class(TTntCustomPanel)
  public
    property DockManager;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    {$IFDEF COMPILER_10_UP}
    property Padding;
    {$ENDIF}
    property ParentBiDiMode;
    {$IFDEF COMPILER_7_UP}
    property ParentBackground;
    {$ENDIF}
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    {$IFDEF COMPILER_9_UP}
    property VerticalAlignment;
    {$ENDIF}
    property Visible;
    {$IFDEF COMPILER_9_UP}
    property OnAlignInsertBefore;
    property OnAlignPosition;
    {$ENDIF}
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
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
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

{TNT-WARN TCustomControlBar}
  TTntCustomControlBar = class(TCustomControlBar{TNT-ALLOW TCustomControlBar})
  private
    function IsHintStored: Boolean;
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  published
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  end;

{TNT-WARN TControlBar}
  TTntControlBar = class(TTntCustomControlBar)
  public
    property Canvas;
  published
    property Align;
    property Anchors;
    property AutoDock;
    property AutoDrag;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BorderWidth;
    property Color {$IFDEF COMPILER_7_UP} nodefault {$ENDIF};
    property Constraints;
    {$IFDEF COMPILER_10_UP}
    property CornerEdge;
    {$ENDIF}
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    {$IFDEF COMPILER_10_UP}
    property DrawingStyle;
    {$ENDIF}
    property Enabled;
    {$IFDEF COMPILER_10_UP}
    property GradientDirection;
    property GradientEndColor;
    property GradientStartColor;
    {$ENDIF}
    {$IFDEF COMPILER_7_UP}
    property ParentBackground default True;
    {$ENDIF}
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property Picture;
    property PopupMenu;
    property RowSize;
    property RowSnap;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    {$IFDEF COMPILER_9_UP}
    property OnAlignInsertBefore;
    property OnAlignPosition;
    {$ENDIF}
    property OnBandDrag;
    property OnBandInfo;
    property OnBandMove;
    property OnBandPaint;
    {$IFDEF COMPILER_9_UP}
    property OnBeginBandMove;
    property OnEndBandMove;
    {$ENDIF}
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
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
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

{TNT-WARN TCustomRadioGroup}
  TTntCustomRadioGroup = class(TTntCustomGroupBox)
  private
    FButtons: TList;
    FItems: TTntStrings;
    FItemIndex: Integer;
    FColumns: Integer;
    FReading: Boolean;
    FUpdating: Boolean;
    function GetButtons(Index: Integer): TTntRadioButton;
    procedure ArrangeButtons;
    procedure ButtonClick(Sender: TObject);
    procedure ItemsChange(Sender: TObject);
    procedure SetButtonCount(Value: Integer);
    procedure SetColumns(Value: Integer);
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(Value: TTntStrings);
    procedure UpdateButtons;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    procedure Loaded; override;
    procedure ReadState(Reader: TReader); override;
    function CanModify: Boolean; virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property Columns: Integer read FColumns write SetColumns default 1;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    property Items: TTntStrings read FItems write SetItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FlipChildren(AllLevels: Boolean); override;
    property Buttons[Index: Integer]: TTntRadioButton read GetButtons;
  end;

{TNT-WARN TRadioGroup}
  TTntRadioGroup = class(TTntCustomRadioGroup)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Color;
    property Columns;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ItemIndex;
    property Items;
    property Constraints;
    property ParentBiDiMode;
    {$IFDEF COMPILER_7_UP}
    property ParentBackground default True;
    {$ENDIF}
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDock;
    property OnStartDrag;
  end;

{TNT-WARN TSplitter}
  TTntSplitter = class(TSplitter{TNT-ALLOW TSplitter})
  private
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsHintStored: Boolean;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  published
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  end;

implementation

uses
  Windows, Graphics, Forms, {$IFDEF THEME_7_UP} Themes, {$ENDIF}
  TntSysUtils, TntWindows, TntActnList;

{ TTntShape }

procedure TTntShape.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntShape.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self)
end;

function TTntShape.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TTntShape.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntShape.CMHintShow(var Message: TMessage);
begin
  ProcessCMHintShowMsg(Message);
  inherited;
end;

procedure TTntShape.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntShape.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

{ TTntPaintBox }

procedure TTntPaintBox.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntPaintBox.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self)
end;

function TTntPaintBox.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TTntPaintBox.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntPaintBox.CMHintShow(var Message: TMessage);
begin
  ProcessCMHintShowMsg(Message);
  inherited;
end;

procedure TTntPaintBox.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntPaintBox.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

type
{$IFDEF COMPILER_6}    // verified against VCL source in Delphi 6 and BCB 6
  THackImage = class(TGraphicControl)
  protected
    FPicture: TPicture{TNT-ALLOW TPicture};
  end;
{$ENDIF}
{$IFDEF DELPHI_7}      // verified against VCL source in Delphi 7
  THackImage = class(TGraphicControl)
  protected
    FPicture: TPicture{TNT-ALLOW TPicture};
  end;
{$ENDIF}
{$IFDEF DELPHI_9}      // verified against VCL source in Delphi 9
  THackImage = class(TGraphicControl)
  private
    FPicture: TPicture{TNT-ALLOW TPicture};
  end;
{$ENDIF}
{$IFDEF DELPHI_10}      // verified against VCL source in Delphi 10
  THackImage = class(TGraphicControl)
  private
    FPicture: TPicture{TNT-ALLOW TPicture};
  end;
{$ENDIF}

{ TTntImage }

constructor TTntImage.Create(AOwner: TComponent);
var
  OldPicture: TPicture{TNT-ALLOW TPicture};
begin
  inherited;
  OldPicture := THackImage(Self).FPicture;
  THackImage(Self).FPicture := TTntPicture.Create;
  Picture.OnChange := OldPicture.OnChange;
  Picture.OnProgress := OldPicture.OnProgress;
  OldPicture.Free;
end;

function TTntImage.GetPicture: TTntPicture;
begin
  Result := inherited Picture as TTntPicture;
end;

procedure TTntImage.SetPicture(const Value: TTntPicture);
begin
  inherited Picture := Value;
end;

procedure TTntImage.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntImage.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self)
end;

function TTntImage.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TTntImage.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntImage.CMHintShow(var Message: TMessage);
begin
  ProcessCMHintShowMsg(Message);
  inherited;
end;

procedure TTntImage.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntImage.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

{ TTntBevel }

procedure TTntBevel.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntBevel.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self)
end;

function TTntBevel.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TTntBevel.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntBevel.CMHintShow(var Message: TMessage);
begin
  ProcessCMHintShowMsg(Message);
  inherited;
end;

procedure TTntBevel.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntBevel.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

{ TTntCustomPanel }

procedure TTntCustomPanel.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, '');
end;

procedure TTntCustomPanel.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntCustomPanel.IsCaptionStored: Boolean;
begin
  Result := TntControl_IsCaptionStored(Self);
end;

function TTntCustomPanel.GetCaption: TWideCaption;
begin
  Result := TntControl_GetText(Self)
end;

procedure TTntCustomPanel.SetCaption(const Value: TWideCaption);
begin
  TntControl_SetText(Self, Value);
end;

procedure TTntCustomPanel.Paint;
const
  Alignments: array[TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  Rect: TRect;
  TopColor, BottomColor: TColor;
  FontHeight: Integer;
  Flags: Longint;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;

begin
  if (not Win32PlatformIsUnicode) then
    inherited
  else begin
    Rect := GetClientRect;
    if BevelOuter <> bvNone then
    begin
      AdjustColors(BevelOuter);
      Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
    end;
    {$IFDEF THEME_7_UP}
    if ThemeServices.ThemesEnabled {$IFDEF COMPILER_7_UP} and ParentBackground {$ENDIF} then
      InflateRect(Rect, -BorderWidth, -BorderWidth)
    else
    {$ENDIF}
    begin
      Frame3D(Canvas, Rect, Color, Color, BorderWidth);
    end;
    if BevelInner <> bvNone then
    begin
      AdjustColors(BevelInner);
      Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
    end;
    with Canvas do
    begin
      {$IFDEF THEME_7_UP}
      if not ThemeServices.ThemesEnabled {$IFDEF COMPILER_7_UP} or not ParentBackground {$ENDIF} then
      {$ENDIF}
      begin
        Brush.Color := Color;
        FillRect(Rect);
      end;
      Brush.Style := bsClear;
      Font := Self.Font;
      FontHeight := WideCanvasTextHeight(Canvas, 'W');
      with Rect do
      begin
        Top := ((Bottom + Top) - FontHeight) div 2;
        Bottom := Top + FontHeight;
      end;
      Flags := DT_EXPANDTABS or DT_VCENTER or Alignments[Alignment];
      Flags := DrawTextBiDiModeFlags(Flags);
      Tnt_DrawTextW(Handle, PWideChar(Caption), -1, Rect, Flags);
    end;
  end;
end;

function TTntCustomPanel.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self)
end;

function TTntCustomPanel.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self);
end;

procedure TTntCustomPanel.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntCustomPanel.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntCustomPanel.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

{ TTntCustomControlBar }

procedure TTntCustomControlBar.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, '');
end;

procedure TTntCustomControlBar.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntCustomControlBar.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self);
end;

function TTntCustomControlBar.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self);
end;

procedure TTntCustomControlBar.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntCustomControlBar.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntCustomControlBar.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

{ TTntGroupButton }

type
  TTntGroupButton = class(TTntRadioButton)
  private
    FInClick: Boolean;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char{TNT-ALLOW Char}); override;
  public
    constructor InternalCreate(RadioGroup: TTntCustomRadioGroup);
    destructor Destroy; override;
  end;

constructor TTntGroupButton.InternalCreate(RadioGroup: TTntCustomRadioGroup);
begin
  inherited Create(RadioGroup);
  RadioGroup.FButtons.Add(Self);
  Visible := False;
  Enabled := RadioGroup.Enabled;
  ParentShowHint := False;
  OnClick := RadioGroup.ButtonClick;
  Parent := RadioGroup;
end;

destructor TTntGroupButton.Destroy;
begin
  TTntCustomRadioGroup(Owner).FButtons.Remove(Self);
  inherited Destroy;
end;

procedure TTntGroupButton.CNCommand(var Message: TWMCommand);
begin
  if not FInClick then
  begin
    FInClick := True;
    try
      if ((Message.NotifyCode = BN_CLICKED) or
        (Message.NotifyCode = BN_DOUBLECLICKED)) and
        TTntCustomRadioGroup(Parent).CanModify then
        inherited;
    except
      Application.HandleException(Self);
    end;
    FInClick := False;
  end;
end;

procedure TTntGroupButton.KeyPress(var Key: Char{TNT-ALLOW Char});
begin
  inherited KeyPress(Key);
  TTntCustomRadioGroup(Parent).KeyPress(Key);
  if (Key = #8) or (Key = ' ') then
  begin
    if not TTntCustomRadioGroup(Parent).CanModify then Key := #0;
  end;
end;

procedure TTntGroupButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  TTntCustomRadioGroup(Parent).KeyDown(Key, Shift);
end;

{ TTntCustomRadioGroup }

constructor TTntCustomRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csSetCaption, csDoubleClicks {$IFDEF COMPILER_7_UP}, csParentBackground {$ENDIF}];
  FButtons := TList.Create;
  FItems := TTntStringList.Create;
  TTntStringList(FItems).OnChange := ItemsChange;
  FItemIndex := -1;
  FColumns := 1;
end;

destructor TTntCustomRadioGroup.Destroy;
begin
  SetButtonCount(0);
  TTntStringList(FItems).OnChange := nil;
  FItems.Free;
  FButtons.Free;
  inherited Destroy;
end;

procedure TTntCustomRadioGroup.FlipChildren(AllLevels: Boolean);
begin
  { The radio buttons are flipped using BiDiMode }
end;

procedure TTntCustomRadioGroup.ArrangeButtons;
var
  ButtonsPerCol, ButtonWidth, ButtonHeight, TopMargin, I: Integer;
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
  DeferHandle: THandle;
  ALeft: Integer;
begin
  if (FButtons.Count <> 0) and not FReading then
  begin
    DC := GetDC(0);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);
    ButtonsPerCol := (FButtons.Count + FColumns - 1) div FColumns;
    ButtonWidth := (Width - 10) div FColumns;
    I := Height - Metrics.tmHeight - 5;
    ButtonHeight := I div ButtonsPerCol;
    TopMargin := Metrics.tmHeight + 1 + (I mod ButtonsPerCol) div 2;
    DeferHandle := BeginDeferWindowPos(FButtons.Count);
    try
      for I := 0 to FButtons.Count - 1 do
        with TTntGroupButton(FButtons[I]) do
        begin
          BiDiMode := Self.BiDiMode;
          ALeft := (I div ButtonsPerCol) * ButtonWidth + 8;
          if UseRightToLeftAlignment then
            ALeft := Self.ClientWidth - ALeft - ButtonWidth;
          DeferHandle := DeferWindowPos(DeferHandle, Handle, 0,
            ALeft,
            (I mod ButtonsPerCol) * ButtonHeight + TopMargin,
            ButtonWidth, ButtonHeight,
            SWP_NOZORDER or SWP_NOACTIVATE);
          Visible := True;
        end;
    finally
      EndDeferWindowPos(DeferHandle);
    end;
  end;
end;

procedure TTntCustomRadioGroup.ButtonClick(Sender: TObject);
begin
  if not FUpdating then
  begin
    FItemIndex := FButtons.IndexOf(Sender);
    Changed;
    Click;
  end;
end;

procedure TTntCustomRadioGroup.ItemsChange(Sender: TObject);
begin
  if not FReading then
  begin
    if FItemIndex >= FItems.Count then FItemIndex := FItems.Count - 1;
    UpdateButtons;
  end;
end;

procedure TTntCustomRadioGroup.Loaded;
begin
  inherited Loaded;
  ArrangeButtons;
end;

procedure TTntCustomRadioGroup.ReadState(Reader: TReader);
begin
  FReading := True;
  inherited ReadState(Reader);
  FReading := False;
  UpdateButtons;
end;

procedure TTntCustomRadioGroup.SetButtonCount(Value: Integer);
begin
  while FButtons.Count < Value do TTntGroupButton.InternalCreate(Self);
  while FButtons.Count > Value do TTntGroupButton(FButtons.Last).Free;
end;

procedure TTntCustomRadioGroup.SetColumns(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value > 16 then Value := 16;
  if FColumns <> Value then
  begin
    FColumns := Value;
    ArrangeButtons;
    Invalidate;
  end;
end;

procedure TTntCustomRadioGroup.SetItemIndex(Value: Integer);
begin
  if FReading then FItemIndex := Value else
  begin
    if Value < -1 then Value := -1;
    if Value >= FButtons.Count then Value := FButtons.Count - 1;
    if FItemIndex <> Value then
    begin
      if FItemIndex >= 0 then
        TTntGroupButton(FButtons[FItemIndex]).Checked := False;
      FItemIndex := Value;
      if FItemIndex >= 0 then
        TTntGroupButton(FButtons[FItemIndex]).Checked := True;
    end;
  end;
end;

procedure TTntCustomRadioGroup.SetItems(Value: TTntStrings);
begin
  FItems.Assign(Value);
end;

procedure TTntCustomRadioGroup.UpdateButtons;
var
  I: Integer;
begin
  SetButtonCount(FItems.Count);
  for I := 0 to FButtons.Count - 1 do
    TTntGroupButton(FButtons[I]).Caption := FItems[I];
  if FItemIndex >= 0 then
  begin
    FUpdating := True;
    TTntGroupButton(FButtons[FItemIndex]).Checked := True;
    FUpdating := False;
  end;
  ArrangeButtons;
  Invalidate;
end;

procedure TTntCustomRadioGroup.CMEnabledChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  for I := 0 to FButtons.Count - 1 do
    TTntGroupButton(FButtons[I]).Enabled := Enabled;
end;

procedure TTntCustomRadioGroup.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ArrangeButtons;
end;

procedure TTntCustomRadioGroup.WMSize(var Message: TWMSize);
begin
  inherited;
  ArrangeButtons;
end;

function TTntCustomRadioGroup.CanModify: Boolean;
begin
  Result := True;
end;

procedure TTntCustomRadioGroup.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

function TTntCustomRadioGroup.GetButtons(Index: Integer): TTntRadioButton;
begin
  Result := TTntRadioButton(FButtons[Index]);
end;

{ TTntSplitter }

procedure TTntSplitter.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntSplitter.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self)
end;

function TTntSplitter.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TTntSplitter.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntSplitter.CMHintShow(var Message: TMessage);
begin
  ProcessCMHintShowMsg(Message);
  inherited;
end;

procedure TTntSplitter.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntSplitter.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

end.
