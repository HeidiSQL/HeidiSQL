// **************************************************************************************************
//
// Unit Vcl.Styles.NC
// unit for the VCL Styles Utils
// https://github.com/RRUZ/vcl-styles-utils/
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Vcl.Styles.NC.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2014-2019 Rodrigo Ruz V.
// All Rights Reserved.
//
// **************************************************************************************************

unit Vcl.Styles.NC;

interface
{$IF RTLVersion>=24}
  {$LEGACYIFEND ON}
{$IFEND}
uses
  System.Classes,
  System.Generics.Collections,
  System.Types,
  Winapi.Windows,
  Winapi.Messages,
  Vcl.ImgList,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Controls,
  Vcl.Menus,
  Vcl.StdCtrls,
  Vcl.GraphUtil,
  Vcl.Forms,
  Vcl.Styles.Utils.Misc;

type
  TNCControl      = class;
  //TListNCButtons = TObjectList<TNCButton>;
  TNCControls     = class;

  TListNCControls = class(TCollection)
  private
    FOwner: TNCControls;
    function GetItem(Index: Integer): TNCControl;
    procedure SetItem(Index: Integer; Value: TNCControl);
    function Add: TNCControl;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    function AddEx<T : TCollectionItem>: T;
    function Insert(Index: Integer): TNCControl;
    property Items[Index: Integer]: TNCControl read GetItem write SetItem; default;
  end;

  TNCControls = class(TComponent)
  private
    FControls: TListNCControls;
    FStyleServices: TCustomStyleServices;
    FVisible: Boolean;
    FForm: TCustomForm;
    FShowSystemMenu: Boolean;
    FFormBorderSize: TRect;
    FImages: TCustomImageList;
    FShowCaption: Boolean;
    FLastPoint : TPoint;
    FActiveTabControlIndex, FHotControlIndex, FPressedControlIndex, FControlUpIndex : Integer;
    function GetStyleServices: TCustomStyleServices;
    procedure SetStyleServices(const Value: TCustomStyleServices);
    procedure SetVisible(const Value: Boolean);
    function GetControl(Index: Integer): TNCControl;
    function GetCount: Integer;
    procedure SetImages(const Value: TCustomImageList);
    procedure SetShowSystemMenu(const Value: Boolean);
    procedure SetShowCaption(const Value: Boolean);
    function GetActiveTabButtonIndex: Integer;
    procedure SetActiveTabButtonIndex(const Value: Integer);
    property FormBorderSize : TRect read FFormBorderSize write FFormBorderSize;
    property Form : TCustomForm read FForm;
  protected
    function  GetNCControlIndex(P: TPoint) : Integer;
    function  PointInNCControl(P: TPoint)  : Boolean;
    property LastPoint : TPoint read FLastPoint;
  public
    property Controls : TListNCControls read FControls;
    property ControlsList[index : Integer] : TNCControl read GetControl; default;
    property ControlsCount  : Integer read GetCount;
    property StyleServices : TCustomStyleServices read GetStyleServices write SetStyleServices;
    procedure Invalidate;
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
  published
    property ActiveTabButtonIndex : Integer read GetActiveTabButtonIndex write SetActiveTabButtonIndex;
    property LNCControl : TListNCControls read FControls;
    property Visible : Boolean read FVisible write SetVisible default True;
    property Images: TCustomImageList read FImages write SetImages;
    property ShowSystemMenu : Boolean read FShowSystemMenu write SetShowSystemMenu default True;
    property ShowCaption  : Boolean read FShowCaption  write SetShowCaption default True;
  end;

  TNCControl = class(TCollectionItem)
  private
    FFont: TFont;
    FEnabled: Boolean;
    FWidth: Integer;
    FVisible: Boolean;
    FTop: Integer;
    FHeight: Integer;
    FLeft : Integer;
    FNCControls  :  TNCControls;
    FCaption: string;
    FHint: string;
    FShowHint: Boolean;
    FName: TComponentName;
    FTag: NativeInt;
    FCaptionAligmentFlags: Cardinal;
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetBoundsRect: TRect;
    procedure SetBoundsRect(const Value: TRect);
    procedure SetHeight(const Value: Integer);
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetFont(const Value: TFont);
    procedure SetShowHint(const Value: Boolean);
    procedure SetName(const Value: TComponentName);
    procedure DrawControl(ACanvas: TCanvas; AMouseInControl, Pressed: Boolean); virtual;
  protected
    procedure Handle_WMNCLButtonDown(var Message: TWMNCHitMessage); virtual;
    procedure Handle_WMNCLButtonUp(var Message: TWMNCHitMessage); virtual;
    procedure Handle_WMNCMouseMove(var Message: TWMNCHitMessage); virtual;
  published
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); virtual;
    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Font: TFont read FFont write SetFont;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Hint: string read FHint write FHint;
    property ShowHint: Boolean read FShowHint write SetShowHint;
    property Name: TComponentName read FName write SetName stored False;
    property Tag: NativeInt read FTag write FTag default 0;
    property CaptionAligmentFlags : Cardinal read FCaptionAligmentFlags write FCaptionAligmentFlags;
    property Caption :  string read FCaption write FCaption;
    function GetAs<T> : T;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property NCControls: TNCControls read FNCControls;
    property BoundsRect: TRect read GetBoundsRect write SetBoundsRect;
  end;


  TNCButton  = class(TNCControl)
  public type
    TNCButtonStyle = (nsPushButton, nsTranparent, nsSplitButton, nsSplitTrans, nsAlpha, nsGradient, nsTab, nsEdge, nsFrame);
    TNCImageStyle  = (isNormal, isGray, isGrayHot);
  private
    FDropDown : Boolean;
    FStyle: TNCButtonStyle;
    FImageAlignment: TImageAlignment;
    FPressedImageIndex: TImageIndex;
    FDropDownMenu: TPopupMenu;
    FOnDropDownClick: TNotifyEvent;
    FDisabledImageIndex: TImageIndex;
    FImageMargins: TImageMargins;
    FImageIndex: TImageIndex;
    FHotImageIndex: TImageIndex;
    FImageStyle: TNCImageStyle;
    FOnClick: TNotifyEvent;
    FHintWindow : THintWindow;
    FAlphaColor: TColor;
    FAlphaHotColor: TColor;
    FFontColor: TColor;
    FHotFontColor: TColor;
    FStartColor: TColor;
    FEndColor: TColor;
    FDirection: TGradientDirection;
    FDefaultFontAwesomeSize: Integer;
    FUseFontAwesome: Boolean;
    FAwesomeHotFontColor: TColor;
    FAwesomeFontColor: TColor;
    FOnClose: TNotifyEvent;
    procedure DrawControl(ACanvas: TCanvas; AMouseInControl, Pressed: Boolean); override;
    procedure SetStyle(const Value: TNCButtonStyle);
    procedure SetDisabledImageIndex(const Value: TImageIndex);
    procedure SetDropDownMenu(const Value: TPopupMenu);
    procedure SetHotImageIndex(const Value: TImageIndex);
    procedure SetImageAlignment(const Value: TImageAlignment);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImageMargins(const Value: TImageMargins);
    procedure SetPressedImageIndex(const Value: TImageIndex);
    procedure DrawControlText(Canvas: TCanvas; Details: TThemedElementDetails; const S: string; var R: TRect; Flags: Cardinal; AColor : TColor = clNone);
    procedure SetImageStyle(const Value: TNCImageStyle);
    procedure ShowHintWindow(X, Y : Integer);
    procedure HideHintWindow;
    function GetTabIndex: Integer;
    procedure SetUseFontAwesome(const Value: Boolean);
  protected
    procedure Handle_WMNCLButtonDown(var Message: TWMNCHitMessage); override;
    procedure Handle_WMNCLButtonUp(var Message: TWMNCHitMessage); override;
    procedure Handle_WMNCMouseMove(var Message: TWMNCHitMessage); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property TabIndex : Integer read GetTabIndex;
  published
    property Style: TNCButtonStyle read FStyle write SetStyle;
    property ImageStyle: TNCImageStyle read FImageStyle write SetImageStyle;
    property DisabledImageIndex: TImageIndex read FDisabledImageIndex write SetDisabledImageIndex;
    property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropDownMenu;
    property HotImageIndex: TImageIndex read FHotImageIndex write SetHotImageIndex;
    property ImageAlignment: TImageAlignment read FImageAlignment write SetImageAlignment;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
    property ImageMargins: TImageMargins read FImageMargins write SetImageMargins;
    property PressedImageIndex: TImageIndex read FPressedImageIndex write SetPressedImageIndex;

    property AlphaColor : TColor read FAlphaColor write FAlphaColor;
    property AlphaHotColor : TColor read FAlphaHotColor write FAlphaHotColor;
    property FontColor : TColor read FFontColor write FFontColor;
    property HotFontColor : TColor read FHotFontColor write FHotFontColor;

    property StartColor : TColor read FStartColor write FStartColor;
    property EndColor : TColor read FEndColor write FEndColor;
    property Direction : TGradientDirection read FDirection write FDirection;

    property UseFontAwesome : Boolean read FUseFontAwesome write SetUseFontAwesome;
    property DefaultFontAwesomeSize : Integer read FDefaultFontAwesomeSize write FDefaultFontAwesomeSize;
    property AwesomeFontColor : TColor read FAwesomeFontColor write FAwesomeFontColor;
    property AwesomeHotFontColor : TColor read FAwesomeHotFontColor write FAwesomeHotFontColor;
    property OnDropDownClick: TNotifyEvent read FOnDropDownClick write FOnDropDownClick;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

  TFormStyleNCControls = class(TFormStyleHook)
  strict private
    FNCControls : TNCControls;
  private
    function GetNCControls : TNCControls;
  protected
    procedure WMNCMouseMove(var Message: TWMNCHitMessage); message WM_NCMOUSEMOVE;
    procedure WMNCLButtonDown(var Message: TWMNCHitMessage); message WM_NCLBUTTONDOWN;
    procedure WMNCLButtonUp(var Message: TWMNCHitMessage); message WM_NCLBUTTONUP;
    procedure WMNCLButtonDblClk(var Message: TWMNCHitMessage); message WM_NCLBUTTONDBLCLK;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure PaintNCControls(Canvas: TCanvas; ARect : TRect);
    procedure PaintNC(Canvas: TCanvas); override;
  strict protected
    procedure MouseLeave; override;
    procedure MouseEnter; override;
    procedure Restore; override;
    procedure Maximize; override;
    procedure Minimize; override;
  public
    property NCControls : TNCControls read GetNCControls;
    constructor Create(AControl: TWinControl); override;
    destructor Destroy; override;
  end;


   TFormStyleNCSettings = class
  private
    class var FUseThinBorder: Boolean;
   public
     class property UseThinBorder : Boolean read FUseThinBorder write FUseThinBorder;
   end;

{
 TODO
   Add more buttons styles  (colors, gradient, glow, link)
   Add hot effects (glow, menu sel)
   Add support for TAction
}

implementation

uses
 DDetours,
 Winapi.CommCtrl,
 System.SysUtils,
 System.Rtti,
 Winapi.UxTheme,
 Vcl.Styles.Utils.Graphics,
 Vcl.Styles.FontAwesome,
 Vcl.Styles.FormStyleHooks;

type
  THintWindowClass = class(THintWindow);
  TCustomFormClass = class(TCustomForm);
  TFormStyleHookClass = class(TFormStyleHook);
  TStyleHookList = TList<TStyleHookClass>;

  TStyleHookDictionary = TDictionary<TClass, TStyleHookList>;
  TCustomStyleEngineHelper = Class Helper for TCustomStyleEngine
  public
    class function GetRegisteredStyleHooks : TStyleHookDictionary;
  end;


var
  Trampoline_TFormStyleHook_GetBorderSize : function (Self : TFormStyleHook) : TRect;
  Trampoline_TFormStyleHook_GetRegion     : function (Self : TFormStyleHook) : HRgn;

class function TCustomStyleEngineHelper.GetRegisteredStyleHooks: TStyleHookDictionary;
begin
  with Self do
    Result := FRegisteredStyleHooks;
end;

function  IsStyleHookRegistered(ControlClass: TClass; StyleHookClass: TStyleHookClass) : Boolean;
var
  List : TStyleHookList;
begin
 Result:=False;
    if TCustomStyleEngine.GetRegisteredStyleHooks.ContainsKey(ControlClass) then
    begin
      List := TCustomStyleEngine.GetRegisteredStyleHooks[ControlClass];
      Result:=List.IndexOf(StyleHookClass) <> -1;
    end;
end;

function  GetRegisteredStylesHooks(ControlClass: TClass) : TStyleHookList;
begin
   Result := nil;
   if TCustomStyleEngine.GetRegisteredStyleHooks.ContainsKey(ControlClass) then
     Result := TCustomStyleEngine.GetRegisteredStyleHooks[ControlClass];
end;

{ TNCControls }

constructor TNCControls.Create(AOwner: TComponent);
begin
  if not (AOwner is TCustomForm) then Raise EAbort.Create('TNCControls only must be created in forms');
  inherited Create(AOwner);
  FForm  := TCustomForm(AOwner);
  FControls  := TListNCControls.Create(Self);

  FHotControlIndex := -1;
  FControlUpIndex := -1;
  FPressedControlIndex := -1;

  FStyleServices := nil;
  FImages  := nil;
  FVisible := True;
  FShowCaption    := True;
  FShowSystemMenu := True;
  if not IsStyleHookRegistered(AOwner.ClassType, TFormStyleNCControls) then
    TStyleManager.Engine.RegisterStyleHook(AOwner.ClassType, TFormStyleNCControls);
  FForm.Perform(CM_RECREATEWND, 0, 0);
  FActiveTabControlIndex := 0;
end;

destructor TNCControls.Destroy;
begin
  FControls.Free;
  inherited;
end;


function TNCControls.GetActiveTabButtonIndex: Integer;
begin
 Result := FActiveTabControlIndex;
end;

function TNCControls.GetControl(Index: Integer): TNCControl;
begin
  Result := FControls[Index];
end;

function TNCControls.GetCount: Integer;
begin
  Result := FControls.Count;
end;

function TNCControls.GetStyleServices: TCustomStyleServices;
begin
  Result := FStyleServices;
  if (Result = nil) then
    Result := Vcl.Themes.StyleServices;
end;

procedure TNCControls.Invalidate;
begin
  if FForm.HandleAllocated then
    SendMessage(FForm.Handle, WM_NCPAINT, 0, 0);
end;

procedure TNCControls.SetActiveTabButtonIndex(const Value: Integer);

  //Refactor This -> TNCButton can't be called from here
  function GetMaxTabIndex: Integer;
  var
    i: Integer;
  begin
    Result := -1;
    For i := 0 to (FControls.Count - 1) do
      if (FControls[i] is TNCButton) and (FControls[i].GetAs<TNCButton>.Style = nsTab) then
        Inc(Result);
  end;

var
  lmax: Integer;
begin
  lmax := GetMaxTabIndex;
  if (Value <> FActiveTabControlIndex) and (Value >= 0) and (lmax >= 0) and (Value <= lmax) then
  begin
    FActiveTabControlIndex := Value;
    Invalidate;
  end;

end;

procedure TNCControls.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
end;

procedure TNCControls.SetShowCaption(const Value: Boolean);
begin
  if Value <> FShowCaption then
  begin
    FShowCaption := Value;
    Invalidate;
  end;
end;

procedure TNCControls.SetShowSystemMenu(const Value: Boolean);
begin
  if Value <> FShowSystemMenu then
  begin
    FShowSystemMenu := Value;
    Invalidate;
  end;
end;

procedure TNCControls.SetStyleServices(const Value: TCustomStyleServices);
begin
  if Value <> FStyleServices then
  begin
    FStyleServices := Value;
    if IsWindowVisible(TCustomFormClass(FForm).Handle) then
      PostMessage(TCustomFormClass(FForm).Handle, CM_CUSTOMSTYLECHANGED, 0, 0)
    else
      SendMessage(TCustomFormClass(FForm).Handle, CM_CUSTOMSTYLECHANGED, 0, 0);
  end;
end;

procedure TNCControls.SetVisible(const Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    Invalidate;
  end;
end;

function  TNCControls.GetNCControlIndex(P: TPoint) : Integer;
var
  i: Integer;
begin
 Result := -1;
  for i := 0 to FControls.Count - 1 do
    if FControls[i].Visible and PtInRect(FControls[i].BoundsRect, P) then
      Exit(i);
end;

function  TNCControls.PointInNCControl(P: TPoint)  : Boolean;
begin
  Result := GetNCControlIndex(P) >= 0;
end;

{ TNCButton }

constructor TNCButton.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FDropDown := False;
  FImageMargins := TImageMargins.Create;
  FDisabledImageIndex:= -1;
  FPressedImageIndex := -1;
  FImageIndex        := -1;
  FUseFontAwesome    := False;
  FDefaultFontAwesomeSize := 16; //16
  FHotImageIndex     :=-1;
  FImageAlignment    := iaLeft;
  FStyle             := nsPushButton;
  FImageStyle        := isNormal;
  FDropDownMenu      := nil;
  FOnDropDownClick   := nil;
  FOnClick := nil;
  FOnClose := nil;
  FHintWindow := THintWindow.Create(nil);
  FAlphaColor := FNCControls.StyleServices.GetSystemColor(clBtnFace);
  FDirection := TGradientDirection.gdHorizontal;

  FFontColor :=  StyleServices.GetSystemColor(clBtnText);
  FHotFontColor :=  StyleServices.GetSystemColor(clHighlight);

  FAwesomeFontColor := StyleServices.GetSystemColor(clBtnText);
  FAwesomeHotFontColor := StyleServices.GetSystemColor(clHighlight);
end;

destructor TNCButton.Destroy;
begin
  FreeAndNil(FHintWindow);
  FreeAndNil(FImageMargins);
  inherited;
end;


procedure TNCButton.DrawControlText(Canvas: TCanvas; Details: TThemedElementDetails; const S: string; var R: TRect; Flags: Cardinal; AColor : TColor = clNone);
var
  ThemeTextColor: TColor;
  TextFormat: TTextFormatFlags;
  LStyleServices : TCustomStyleServices;
begin
  Canvas.Font := Font;
  LStyleServices := NCControls.StyleServices;
  TextFormat := TTextFormatFlags(Flags);
  if LStyleServices.GetElementColor(Details, ecTextColor, ThemeTextColor) then
  begin
    if AColor <> clNone then
     Canvas.Font.Color := AColor
    else
     Canvas.Font.Color := ThemeTextColor;
    LStyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat, Canvas.Font.Color);
  end
  else
  begin
    Canvas.Refresh;
    LStyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat);
  end;
end;


function TNCButton.GetTabIndex: Integer;
var
  i : Integer;
begin
  Result := -1;
  for i  := 0 to NCControls.ControlsCount-1 do
  if NCControls.LNCControl[i] is TNCButton then
  begin
   if NCControls.LNCControl[i].GetAs<TNCButton>.Style = nsTab then
    Inc(Result);

   if NCControls[i] = Self then
     Break;
  end;
end;

procedure DoDrawGrayImage(hdcDst: HDC; himl: HIMAGELIST; ImageIndex, X, Y: Integer);
var
  pimldp: TImageListDrawParams;
begin
  FillChar(pimldp, SizeOf(pimldp), #0);
  pimldp.fState := ILS_SATURATE;
  pimldp.cbSize := SizeOf(pimldp);
  pimldp.hdcDst := hdcDst;
  pimldp.himl := himl;
  pimldp.i := ImageIndex;
  pimldp.x := X;
  pimldp.y := Y;
  ImageList_DrawIndirect(@pimldp);
end;

procedure FrameRect(ACanvas: TCanvas;const R: TRect; Color1, Color2 : TColor);
begin
  with ACanvas do
  begin
    Pen.Color := Color1;
    PolyLine([Point(R.Left, R.Bottom), Point(R.Left, R.Top), Point(R.Right, R.Top)]);
    Pen.Color := Color2;
    PolyLine([Point(R.Right, R.Top), Point(R.Right, R.Bottom), Point(R.Left, R.Bottom)]);
  end;
end;


procedure TNCButton.Handle_WMNCLButtonDown(var Message: TWMNCHitMessage);
var
  i : Integer;
begin
  // process click on buttton  with the nstab style
  if (FNCControls.FPressedControlIndex >= 0) and  (FNCControls[FNCControls.FPressedControlIndex] is TNCButton) and (FNCControls[FNCControls.FPressedControlIndex].GetAs<TNCButton>.Style = nsTab) then
  begin
    FNCControls.FActiveTabControlIndex := -1;
    For i := 0 to FNCControls.FPressedControlIndex do
      if  (FNCControls[i] is TNCButton) and (FNCControls[i].GetAs<TNCButton>.Style = nsTab) then
        Inc(FNCControls.FActiveTabControlIndex);
  end;
end;

procedure TNCButton.Handle_WMNCLButtonUp(var Message: TWMNCHitMessage);
var
  LNCButtton : TNCButton;
  LRect : TRect;
  P : TPoint;
  i, X, Y : Integer;
begin
  i := FNCControls.FControlUpIndex;

  if ((Message.HitTest = HTTOP) or (Message.HitTest = HTCAPTION)) and (i >= 0)  and (NCControls.LNCControl[i] is TNCButton) then
  begin
    LRect := Rect(0, 0, 0, 0);
    if (NCControls <> nil) then
    begin
      LRect := NCControls.LNCControl[i].BoundsRect;
      LRect.Left := LRect.Right - 15;
    end;

    P := NCControls.LastPoint;

    LNCButtton := NCControls.LNCControl[i].GetAs<TNCButton>;

    if (LNCButtton.Enabled) and Assigned(LNCButtton.FOnDropDownClick) and
      (LNCButtton.Style in [nsSplitButton, nsSplitTrans]) and PtInRect(LRect, P) then
      LNCButtton.FOnDropDownClick(LNCButtton)
    else if (LNCButtton.Enabled) and Assigned(LNCButtton.FDropDownMenu) and PtInRect(LRect, P) then
    begin
      X := NCControls.Form.Left + LNCButtton.BoundsRect.Left;
      if NCControls.Form.BiDiMode = TBiDiMode.bdRightToLeft then
       Inc(X, 250);

      Y := NCControls.Form.Top + LNCButtton.BoundsRect.Bottom;
      // OutputDebugString(PChar(Format('Popup X %d Y %d', [X, Y])));

      LNCButtton.FDropDownMenu.Popup(X, Y);
      // if LNCButtton.FDropDownMenu.BiDiMode = bdLeftToRight then
      // LNCButtton.FDropDownMenu.Popup(X, Y)
      // else
      // begin
      // LPoint := Point(X, Y);
      // TrackPopupMenu(LNCButtton.FDropDownMenu.Handle, TPM_RIGHTALIGN or TPM_TOPALIGN, LPoint.X, LPoint.Y, 0, PopupList.Window, nil);
      // end;
    end
    else if (LNCButtton.Style = nsTab) and Assigned(LNCButtton.FOnClose) and PtInRect(LRect, P) then
    begin
      if LNCButtton.ShowHint then
        LNCButtton.HideHintWindow();

      if not IsIconic(TCustomFormClass(NCControls.Form).Handle) then
        LNCButtton.FOnClose(LNCButtton);
    end
    else if (LNCButtton.Enabled) and Assigned(LNCButtton.FOnClick) then
    begin
      if LNCButtton.ShowHint then
        LNCButtton.HideHintWindow();

      if not IsIconic(TCustomFormClass(NCControls.Form).Handle) then
        LNCButtton.FOnClick(LNCButtton);
    end;
  end;
end;

procedure TNCButton.Handle_WMNCMouseMove(var Message: TWMNCHitMessage);
var
  P : TPoint;
  I : Integer;
begin
  P := NCControls.LastPoint;

  if ((Message.HitTest = HTTOP) or (Message.HitTest = HTCAPTION) or
    (not NCControls.ShowSystemMenu and (Message.HitTest = HTSYSMENU))) and NCControls.PointInNCControl(P) then
  begin

    if NCControls.FHotControlIndex <> NCControls.GetNCControlIndex(P) then
    begin
      NCControls.FHotControlIndex := NCControls.GetNCControlIndex(P);

      for i := 0 to NCControls.ControlsCount - 1 do
        if (NCControls.FHotControlIndex <> i) and NCControls.LNCControl[i].ShowHint and (NCControls.LNCControl[i] is TNCButton) then
          NCControls.LNCControl[i].GetAs<TNCButton>.HideHintWindow();

      if not IsIconic(TCustomFormClass(NCControls.Form).Handle) then
        if NCControls.LNCControl[NCControls.FHotControlIndex].ShowHint and (NCControls.LNCControl[NCControls.FHotControlIndex] is TNCButton) then
          NCControls.LNCControl[NCControls.FHotControlIndex].GetAs<TNCButton>.ShowHintWindow(Message.XCursor, Message.YCursor);

      NCControls.Invalidate();
    end;
  end
  else
  //if NCControls.FHotControlIndex <> -1 then
  begin
    for i := 0 to NCControls.ControlsCount - 1 do
      if NCControls.LNCControl[i].ShowHint and (NCControls.LNCControl[i] is TNCButton) then
        NCControls.LNCControl[i].GetAs<TNCButton>.HideHintWindow();

    NCControls.FHotControlIndex := -1;
    NCControls.Invalidate();
    //OutputDebugString(PChar('Ping '+FormatDateTime('hh:nn:ss.zzz', Now)));
  end;
end;

procedure TNCButton.DrawControl(ACanvas: TCanvas; AMouseInControl, Pressed: Boolean);
var
  Details, Details2:  TThemedElementDetails;
  ButtonRect, DrawRect, LRect, LRectAwesome : TRect;
  IW, IH, IX, IY: Integer;
  SaveIndex: Integer;
  X, Y, I, LImgIndex: Integer;
  BCaption: String;
  LStyleServices : TCustomStyleServices;
  LColor, LColor1, LColor2, ThemeTextColor : TColor;
  FButtonState: TThemedWindow;

  function GetBorderSize: TRect;
  var
    Size: TSize;
    Details: TThemedElementDetails;
    Detail: TThemedWindow;
  begin
    Result := Rect(0, 0, 0, 0);
    if NCControls.Form.BorderStyle = bsNone then Exit;

    if not LStyleServices.Available then Exit;
    {caption height}
    if (NCControls.Form.BorderStyle <> bsToolWindow) and
       (NCControls.Form.BorderStyle <> bsSizeToolWin) then
      Detail := twCaptionActive
    else
      Detail := twSmallCaptionActive;
    Details := LStyleServices.GetElementDetails(Detail);
    LStyleServices.GetElementSize(0, Details, esActual, Size);
    Result.Top := Size.cy;
    {left border width}
    if (NCControls.Form.BorderStyle <> bsToolWindow) and
       (NCControls.Form.BorderStyle <> bsSizeToolWin) then
      Detail := twFrameLeftActive
    else
      Detail := twSmallFrameLeftActive;
    Details := LStyleServices.GetElementDetails(Detail);
    LStyleServices.GetElementSize(0, Details, esActual, Size);
    Result.Left := Size.cx;
    {right border width}
    if (NCControls.Form.BorderStyle <> bsToolWindow) and
       (NCControls.Form.BorderStyle <> bsSizeToolWin) then
      Detail := twFrameRightActive
    else
      Detail := twSmallFrameRightActive;
    Details := LStyleServices.GetElementDetails(Detail);
    LStyleServices.GetElementSize(0, Details, esActual, Size);
    Result.Right := Size.cx;
    {bottom border height}
    if (FNCControls.Form.BorderStyle <> bsToolWindow) and
       (NCControls.Form.BorderStyle <> bsSizeToolWin) then
      Detail := twFrameBottomActive
    else
      Detail := twSmallFrameBottomActive;
    Details := LStyleServices.GetElementDetails(Detail);
    LStyleServices.GetElementSize(0, Details, esActual, Size);
    Result.Bottom := Size.cy;
  end;

  function GetTopOffset: Integer;
  var
    P: TPoint;
  begin
    P.X := NCControls.Form.Left + NCControls.Form.Width div 2;
    P.Y := NCControls.Form.Top + NCControls.Form.Height div 2;
    Result := Screen.MonitorFromPoint(P).WorkareaRect.Top;
    if NCControls.Form.Top < Result then Result := Result - NCControls.Form.Top else Result := 0;
  end;

  procedure CorrectRightButtonRect(var AButtonRect: TRect);
  var
    TopOffset, RightOffset: Integer;
    BS: TRect;
  begin
    if (NCControls.Form.WindowState = wsMaximized) and (TCustomFormClass(NCControls.Form).FormStyle <> fsMDIChild) and (ButtonRect.Width > 0) then
    begin
      BS := GetBorderSize;
      TopOffset := GetTopOffset;
      RightOffset := -BS.Right;
      if ButtonRect.Top < TopOffset then
      begin
        TopOffset := TopOffset - ButtonRect.Top;
        OffsetRect(ButtonRect, RightOffset, TopOffset);
        TopOffset := ButtonRect.Bottom - BS.Top;
        if TopOffset > 0 then
          OffsetRect(ButtonRect, 0, -TopOffset);
      end;
    end;
  end;

  procedure CorrectLeftButtonRect(var AButtonRect: TRect);
  var
    TopOffset, LeftOffset: Integer;
    BS: TRect;
  begin
    if (NCControls.Form.WindowState = wsMaximized) and (TCustomFormClass(NCControls.Form).FormStyle <> fsMDIChild) and (ButtonRect.Width > 0) then
    begin
      BS := GetBorderSize;
      TopOffset := GetTopOffset;
      LeftOffset := BS.Left;
      if ButtonRect.Top < TopOffset then
      begin
        TopOffset := TopOffset - ButtonRect.Top;
        OffsetRect(ButtonRect, LeftOffset, TopOffset);
        TopOffset := ButtonRect.Bottom - BS.Top;
        if TopOffset > 0 then
          OffsetRect(ButtonRect, 0, -TopOffset);
      end;
    end;
  end;

  function GetButtonCloseRect(Index: Integer): TRect;
  var
    Details : TThemedElementDetails;
    R, ButtonR : TRect;
  begin
    R := BoundsRect;
    if R.Left < 0 then Exit;

    if Index = TabIndex then
      InflateRect(R, 0, 2);

    Result := R;
    Details := LStyleServices.GetElementDetails(twSmallCloseButtonNormal);
    if not LStyleServices.GetElementContentRect(0, Details, Result, ButtonR) then
      ButtonR := Rect(0, 0, 0, 0);

    Result.Left := Result.Right - (ButtonR.Width) - 3;
    Result.Width:= ButtonR.Width;
    Result.Top :=  Result.Top + 3;
  end;

begin

  LStyleServices := NCControls.StyleServices;
  BCaption := FCaption;
  LImgIndex := FImageIndex;
  if not Enabled then
  begin
    Details := LStyleServices.GetElementDetails(tbPushButtonDisabled);
    if DisabledImageIndex <> -1 then
      LImgIndex := FDisabledImageIndex;
  end
  else if Pressed then
  begin
    Details := LStyleServices.GetElementDetails(tbPushButtonPressed);
    if PressedImageIndex <> -1 then
      LImgIndex := FPressedImageIndex;
  end
  else if AMouseInControl then
  begin
    Details := LStyleServices.GetElementDetails(tbPushButtonHot);
    if HotImageIndex <> -1 then
      LImgIndex := FHotImageIndex;
  end
  else if Enabled then
    Details := LStyleServices.GetElementDetails(tbPushButtonNormal);

  DrawRect := BoundsRect; // ClientRect;

  if FStyle = nsTab then
    if (NCControls.Form.WindowState = wsMaximized) and (TCustomFormClass(NCControls.Form).FormStyle <> fsMDIChild) then
    else
      DrawRect.Height := NCControls.FormBorderSize.Top - DrawRect.Top;

  if FStyle = nsAlpha then
  begin
    LColor := ACanvas.Pen.Color;
    try

      ButtonRect := DrawRect;
      {$IF CompilerVersion > 23}
      if not StyleServices.HasElementFixedPosition(Details) then
      {$IFEND}
        CorrectLeftButtonRect(ButtonRect);
      DrawRect := ButtonRect;

      ACanvas.Pen.Color := clNone;

      if (AMouseInControl) and (FAlphaHotColor <> clNone) then
      begin
        LRect := DrawRect;
        InflateRect(LRect, -1, -1);
        AlphaBlendFillCanvas(ACanvas.Handle, FAlphaHotColor, LRect, 96);
        ACanvas.Pen.Color := FAlphaHotColor;
      end
      else
      if FAlphaColor <> clNone  then
      begin
        LRect := DrawRect;
        InflateRect(LRect, -1, -1);
        AlphaBlendFillCanvas(ACanvas.Handle, FAlphaColor, LRect, 96);
        ACanvas.Pen.Color := FAlphaColor;
      end;

      if ACanvas.Pen.Color <> clNone then
      begin
        LRect := DrawRect;
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Rectangle(LRect.Left, LRect.Top, LRect.Left +  LRect.Width,  LRect.Top + LRect.Height);
      end;
    finally
      ACanvas.Pen.Color:=LColor;
    end;
  end
  else
  if FStyle = nsGradient then
  begin
    LColor := ACanvas.Pen.Color;
    try

      ButtonRect := DrawRect;
      {$IF CompilerVersion > 23}if not StyleServices.HasElementFixedPosition(Details) then {$IFEND}
        CorrectLeftButtonRect(ButtonRect);
      DrawRect := ButtonRect;


      if AMouseInControl then
        GradientFillCanvas(ACanvas, FEndColor, FStartColor, DrawRect, FDirection)
      else
        GradientFillCanvas(ACanvas, FStartColor, FEndColor, DrawRect, FDirection);

      if AMouseInControl then
        ACanvas.Pen.Color := FEndColor
      else
        ACanvas.Pen.Color := FStartColor;

      ACanvas.Brush.Style := bsClear;

      LRect := DrawRect;
      ACanvas.Rectangle(LRect.Left, LRect.Top, LRect.Left + LRect.Width, LRect.Top + LRect.Height);
    finally
      ACanvas.Pen.Color := LColor;
    end;
  end
  else if FStyle = nsTab then
  begin
    // if AMouseInControl then
    // Details := LStyleServices.GetElementDetails(ttTabItemHot)
    // else
    if Pressed or (NCControls.FActiveTabControlIndex = TabIndex) then
      Details := LStyleServices.GetElementDetails(ttTabItemSelected)
    else if not Enabled then
      Details := LStyleServices.GetElementDetails(ttTabItemDisabled)
    else
      Details := LStyleServices.GetElementDetails(ttTabItemNormal);

    ButtonRect := DrawRect;
{$IF CompilerVersion > 23} if not StyleServices.HasElementFixedPosition(Details) then {$IFEND}
      CorrectLeftButtonRect(ButtonRect);
    DrawRect := ButtonRect;

    LStyleServices.DrawElement(ACanvas.Handle, Details, DrawRect);

    if @FOnClose <> nil then
    begin
      if AMouseInControl then
       FButtonState := twSmallCloseButtonHot
      else
      if Index = TabIndex then
       FButtonState := twSmallCloseButtonNormal
      else
       FButtonState := twSmallCloseButtonDisabled;

      Details2 := LStyleServices.GetElementDetails(FButtonState);
      LRect := GetButtonCloseRect(Index);
      if LRect.Bottom - LRect.Top > 0 then
        LStyleServices.DrawElement(ACanvas.Handle, Details2, LRect);
    end;
  end
  else if FStyle = nsEdge then
  begin
    ButtonRect := DrawRect;
{$IF CompilerVersion > 23} if not StyleServices.HasElementFixedPosition(Details) then {$IFEND}
      CorrectLeftButtonRect(ButtonRect);
    DrawRect := ButtonRect;

    LStyleServices.DrawElement(ACanvas.Handle, LStyleServices.GetElementDetails(tbGroupBoxNormal), DrawRect);
  end
  else if FStyle = nsFrame then
  begin

    ButtonRect := DrawRect;
{$IF CompilerVersion > 23} if not StyleServices.HasElementFixedPosition(Details) then {$IFEND}
      CorrectLeftButtonRect(ButtonRect);
    DrawRect := ButtonRect;

    LColor1 := LStyleServices.GetSystemColor(clBtnShadow);
    LColor2 := LStyleServices.GetSystemColor(clBtnHighlight);

    LColor := LColor1;
    LColor1 := LColor2;
    with DrawRect do
      FrameRect(ACanvas, Rect(Left + 1, Top + 1, Left + Width - 1, Top + Height - 1), LColor1, LColor2);

    LColor2 := LColor;
    LColor1 := LColor;
    with DrawRect do
      FrameRect(ACanvas, Rect(Left + 0, Top + 0, Left + Width - 2, Top + Height - 2), LColor1, LColor2);

  end
  else
  if Enabled and (FStyle in [{nsTranparent,} nsSplitTrans]) and AMouseInControl then
  begin
    ButtonRect := DrawRect;
    {$IF CompilerVersion > 23}if not StyleServices.HasElementFixedPosition(Details) then {$IFEND}
      CorrectLeftButtonRect(ButtonRect);
    DrawRect := ButtonRect;

    Details := LStyleServices.GetElementDetails(tmMenuBarItemHot);
    LStyleServices.DrawElement(ACanvas.Handle, Details, DrawRect);
  end
  else
  if (FStyle <> nsTranparent) and (FStyle <> nsSplitTrans) then
  begin
    ButtonRect := DrawRect;
    {$IF CompilerVersion > 23}if not StyleServices.HasElementFixedPosition(Details) then {$IFEND}
      CorrectLeftButtonRect(ButtonRect);
    DrawRect := ButtonRect;

    LStyleServices.DrawElement(ACanvas.Handle, Details, DrawRect);
  end;

  if FUseFontAwesome and (LImgIndex >= 0) then
  begin

    IW := FDefaultFontAwesomeSize;
    IH := FDefaultFontAwesomeSize;

    ButtonRect := DrawRect;
    {$IF CompilerVersion > 23}if not StyleServices.HasElementFixedPosition(Details) then {$IFEND}
      CorrectLeftButtonRect(ButtonRect);

    DrawRect := ButtonRect;

    if FStyle = nsSplitButton then
      IX := DrawRect.Left + ((DrawRect.Width - 15)  - IW) div 2
    else
      IX := DrawRect.Left + (DrawRect.Width  - IW) div 2;

    IY := DrawRect.Top  + (DrawRect.Height - IH) div 2;

       case FImageAlignment of
          iaLeft:
            begin
              IX := DrawRect.Left + 2;
              Inc(IX, ImageMargins.Left);
              Inc(IY, ImageMargins.Top);
              Dec(IY, ImageMargins.Bottom);
              Inc(DrawRect.Left, {IX +} IW + ImageMargins.Right);
            end;

          iaRight:
            begin
              IX := DrawRect.Right - IW - 2;
              Dec(IX, ImageMargins.Right);
              Dec(IX, ImageMargins.Left);
              Inc(IY, ImageMargins.Top);
              Dec(IY, ImageMargins.Bottom);
              DrawRect.Right := IX;
            end;

          iaTop:
            begin
              IX := {DrawRect.Left +} ({DrawRect.Width -} IW) div 2;
              Inc(IX, ImageMargins.Left);
              Dec(IX, ImageMargins.Right);
              IY := DrawRect.Top + 2;
              Inc(IY, ImageMargins.Top);
              Inc(DrawRect.Top, IY + IH + ImageMargins.Bottom);
            end;

          iaBottom:
            begin
              IX := {DrawRect.Left +} ({DrawRect.Width -} IW) div 2;
              Inc(IX, ImageMargins.Left);
              Dec(IX, ImageMargins.Right);
              IY := DrawRect.Bottom - IH - 2;
              Dec(IY, ImageMargins.Bottom);
              Dec(IY, ImageMargins.Top);
              DrawRect.Bottom := IY;
            end;
       end;

//    if AMouseInControl then
//      Dec(IY);
    LRectAwesome := Rect(IX, IY, IX + IW + 2, IY + IH + 2);
  end
  else
  if (LImgIndex >= 0) and (NCControls.FImages <> nil) and (NCControls.FImages.Handle <> 0) and
     ImageList_GetIconSize(NCControls.FImages.Handle, IW, IH) then
  begin

    ButtonRect := DrawRect;
    {$IF CompilerVersion > 23}if not StyleServices.HasElementFixedPosition(Details) then {$IFEND}
      CorrectLeftButtonRect(ButtonRect);

    DrawRect := ButtonRect;

    IX := DrawRect.Left + (DrawRect.Width  - IW) div 2;
    IY := DrawRect.Top  + (DrawRect.Height - IH) div 2;

       case FImageAlignment of
          iaLeft:
            begin
              IX := DrawRect.Left + 2;
              Inc(IX, ImageMargins.Left);
              Inc(IY, ImageMargins.Top);
              Dec(IY, ImageMargins.Bottom);
              Inc(DrawRect.Left, {IX +} IW + ImageMargins.Right);
            end;

          iaRight:
            begin
              IX := DrawRect.Right - IW - 2;
              Dec(IX, ImageMargins.Right);
              Dec(IX, ImageMargins.Left);
              Inc(IY, ImageMargins.Top);
              Dec(IY, ImageMargins.Bottom);
              DrawRect.Right := IX;
            end;

          iaTop:
            begin
              IX := {DrawRect.Left +} ({DrawRect.Width -} IW) div 2;
              Inc(IX, ImageMargins.Left);
              Dec(IX, ImageMargins.Right);
              IY := DrawRect.Top + 2;
              Inc(IY, ImageMargins.Top);
              Inc(DrawRect.Top, IY + IH + ImageMargins.Bottom);
            end;

          iaBottom:
            begin
              IX := {DrawRect.Left +} ({DrawRect.Width -} IW) div 2;
              Inc(IX, ImageMargins.Left);
              Dec(IX, ImageMargins.Right);
              IY := DrawRect.Bottom - IH - 2;
              Dec(IY, ImageMargins.Bottom);
              Dec(IY, ImageMargins.Top);
              DrawRect.Bottom := IY;
            end;
       end;

//    if AMouseInControl then
//      Dec(IY);
    if Enabled and ((FImageStyle = isNormal) or ((FImageStyle = isGrayHot) and AMouseInControl)) then
      ImageList_Draw(NCControls.FImages.Handle, LImgIndex, ACanvas.Handle, IX, IY, ILD_NORMAL)
    else
      DoDrawGrayImage(ACanvas.Handle, NCControls.FImages.Handle, LImgIndex, IX, IY);
  end;

    if (FStyle in [nsSplitButton, nsSplitTrans]) then
    begin
      LRect := DrawRect;
      Dec(DrawRect.Right, 15);

      if AMouseInControl  then
       ThemeTextColor := FHotFontColor
      else
       ThemeTextColor := FFontColor;

      if Length(BCaption) > 0 then
      begin
        ButtonRect := DrawRect;
        {$IF CompilerVersion > 23}if not StyleServices.HasElementFixedPosition(Details) then {$IFEND}
          CorrectLeftButtonRect(ButtonRect);
        DrawRect := ButtonRect;

        DrawControlText(ACanvas, Details, BCaption, DrawRect, DT_VCENTER or DT_CENTER, ThemeTextColor);
      end;

       if FUseFontAwesome and (FImageIndex >= 0) then
       begin
        if AMouseInControl  then
         ThemeTextColor := FAwesomeHotFontColor
        else
         ThemeTextColor := FAwesomeFontColor;

         FontAwesome.DrawChar(ACanvas.Handle, LImgIndex, LRectAwesome, FDefaultFontAwesomeSize, ThemeTextColor, 0, FImageAlignment);
       end;

      if FDropDown then
      begin
        Details := LStyleServices.GetElementDetails(tbPushButtonPressed);
        SaveIndex := SaveDC(ACanvas.Handle);
        try
          IntersectClipRect(ACanvas.Handle, Width - 15, 0, Width, Height);
          DrawRect := Rect(Width - 30, 0, Width, Height);

          ButtonRect := DrawRect;
          {$IF CompilerVersion > 23}if not StyleServices.HasElementFixedPosition(Details) then {$IFEND}
            CorrectLeftButtonRect(ButtonRect);
          DrawRect := ButtonRect;

          LStyleServices.DrawElement(ACanvas.Handle, Details, DrawRect);
        finally
          RestoreDC(ACanvas.Handle, SaveIndex);
        end;
      end;

      with ACanvas do
      begin

         LColor := Pen.Color;
         // draw split line
         if FStyle <> nsSplitTrans then
         begin
           Pen.Color := LStyleServices.GetSystemColor(clBtnShadow);
           MoveTo(LRect.Right - 15, LRect.Top + 3);
           LineTo(LRect.Right - 15, LRect.Bottom - 3);
           if Enabled then
             Pen.Color := LStyleServices.GetSystemColor(clBtnHighLight)
           else
             Pen.Color := Font.Color;
           MoveTo(LRect.Right - 14, LRect.Top + 3);
           LineTo(LRect.Right - 14, LRect.Bottom - 3);
         end;


        // draw arrow

        if (FStyle = nsSplitTrans) and (not AMouseInControl)  then
         Pen.Color := FFontColor
        else
         Pen.Color := FHotFontColor;

        X := LRect.Right - 8;
        Y := LRect.Top + (Height div 2) + 1;
        for i := 3 downto 0 do
        begin
          MoveTo(X - I, Y - I);
          LineTo(X + I + 1, Y - I);
        end;

        Pen.Color := LColor;
      end;

    end
    else
    begin
      if AMouseInControl then
        ThemeTextColor := FHotFontColor
      else
        ThemeTextColor := FFontColor;

//      ButtonRect := DrawRect;
//      if not StyleServices.HasElementFixedPosition(Details) then
//        CorrectLeftButtonRect(ButtonRect);
//      DrawRect := ButtonRect;

      if (FCaptionAligmentFlags and DT_CENTER) <> DT_CENTER then
        DrawRect.Left := DrawRect.Left + 5;


      DrawControlText(ACanvas, Details, BCaption, DrawRect, FCaptionAligmentFlags, ThemeTextColor);

     if FUseFontAwesome and (FImageIndex >= 0) then
     begin
        if AMouseInControl  then
         ThemeTextColor := FAwesomeHotFontColor
        else
         ThemeTextColor := FAwesomeFontColor;

       //InflateRect(LRectAwesome, 2, 2);
       FontAwesome.DrawChar(ACanvas.Handle, LImgIndex, LRectAwesome, FDefaultFontAwesomeSize, ThemeTextColor, 0, FImageAlignment);
     end;
    end;
end;


procedure TNCButton.SetDisabledImageIndex(const Value: TImageIndex);
begin
  FDisabledImageIndex := Value;
end;

procedure TNCButton.SetDropDownMenu(const Value: TPopupMenu);
begin
  FDropDownMenu := Value;
end;

procedure TNCButton.SetHotImageIndex(const Value: TImageIndex);
begin
  FHotImageIndex := Value;
end;

procedure TNCButton.SetImageAlignment(const Value: TImageAlignment);
begin
  FImageAlignment := Value;
end;

procedure TNCButton.SetImageIndex(const Value: TImageIndex);
begin
  FImageIndex := Value;
end;

procedure TNCButton.SetImageMargins(const Value: TImageMargins);
begin
  FImageMargins := Value;
end;


procedure TNCButton.SetImageStyle(const Value: TNCImageStyle);
begin
  FImageStyle := Value;
end;

procedure TNCButton.SetPressedImageIndex(const Value: TImageIndex);
begin
  FPressedImageIndex := Value;
end;

procedure TNCButton.SetStyle(const Value: TNCButtonStyle);
begin
  FStyle := Value;
end;

procedure TNCButton.SetUseFontAwesome(const Value: Boolean);
begin
  FUseFontAwesome := Value;
end;

procedure TNCButton.ShowHintWindow(X, Y : Integer);
begin
  if (THintWindowClass(FHintWindow).WindowHandle = 0) then
  begin
    FHintWindow.Visible := False;
    FHintWindow.Color       := NCControls.StyleServices.GetSystemColor(clInfoBk);
    FHintWindow.Font.Color  := NCControls.StyleServices.GetSystemColor(clInfoText);
    FHintWindow.Caption := Hint;
    FHintWindow.ParentWindow := Application.Handle;
    FHintWindow.Left:= NCControls.FForm.Left + BoundsRect.Left;
    FHintWindow.Top := NCControls.FForm.Top +  BoundsRect.Bottom+5;
    FHintWindow.Show;
  end;
end;

procedure TNCButton.HideHintWindow;
begin
  if THintWindowClass(FHintWindow).WindowHandle <> 0 then
    FHintWindow.ReleaseHandle;
end;

{ TFormStyleNCControls }
constructor TFormStyleNCControls.Create(AControl: TWinControl);
begin
  inherited;
  FNCControls := nil;
end;

destructor TFormStyleNCControls.Destroy;
begin
  inherited;
end;

function TFormStyleNCControls.GetNCControls: TNCControls;
var
  i: Integer;
begin
  Result := FNCControls;
  if Result = nil then
    for i := 0 to Form.ComponentCount - 1 do
      if Form.Components[i] is TNCControls then
      begin
        FNCControls := TNCControls(Form.Components[i]);
        Exit(FNCControls);
      end;
end;

procedure TFormStyleNCControls.Maximize;
begin
  if Handle <> 0 then
  begin
    FNCControls.FPressedControlIndex := -1;
    FNCControls.FHotControlIndex := -1;
  end;
  inherited;
end;

procedure TFormStyleNCControls.Minimize;
begin
  if Handle <> 0 then
  begin
    FNCControls.FPressedControlIndex := -1;
    FNCControls.FHotControlIndex := -1;
  end;
  inherited;
end;

procedure TFormStyleNCControls.MouseEnter;
begin
  inherited;
  FNCControls.FPressedControlIndex := -1;
end;

procedure TFormStyleNCControls.MouseLeave;
begin
  inherited;
  if FNCControls.FHotControlIndex <> -1 then
  begin
    FNCControls.FHotControlIndex := -1;
    FNCControls.FPressedControlIndex := -1;
    if Form.BorderStyle <> bsNone then
      InvalidateNC;
  end;
end;

procedure TFormStyleNCControls.Restore;
begin
  FNCControls.FPressedControlIndex := -1;
  FNCControls.FHotControlIndex := -1;
  inherited;
end;

procedure TFormStyleNCControls.PaintNCControls(Canvas: TCanvas; ARect: TRect);
var
  LCurrent: Integer;
  LNCControl: TNCControl;
begin
  if (NCControls <> nil) and (NCControls.ControlsCount > 0) and (NCControls.Visible){ and (NCControls.Form.Visible) } and (NCControls.FControls.UpdateCount = 0) then
    for LCurrent := 0 to NCControls.ControlsCount - 1 do
    begin
      LNCControl := NCControls.Controls[LCurrent];
      if LNCControl.Visible and (LNCControl.BoundsRect.Right <= ARect.Right) then
        LNCControl.DrawControl(Canvas, FNCControls.FHotControlIndex = LCurrent, FNCControls.FPressedControlIndex = LCurrent);
    end
  else;
end;

procedure TFormStyleNCControls.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
  LHitTest: Integer;
begin
  if (NCControls <> nil) and (NCControls.Visible) then
  begin
{$IF CompilerVersion>23}
    if (TStyleManager.FormBorderStyle = fbsCurrentStyle) and (seBorder in Form.StyleElements) then
{$IFEND}
    begin
      P := _NormalizePoint(Point(Message.XPos, Message.YPos));
      LHitTest := _GetHitTest(P);
      if (LHitTest <> HTSYSMENU) or ((LHitTest = HTSYSMENU) and NCControls.ShowSystemMenu) then
      begin
        Message.Result := LHitTest;
        Handled := True;
      end
      else
      begin
        Message.Result := WM_NULL;
        Handled := True;
      end;
    end;
  end
  else
    inherited;
end;

// Avoid maximize or restore on DblClk

procedure TFormStyleNCControls.WMNCLButtonDblClk(var Message: TWMNCHitMessage);
var
  P: TPoint;
begin
  if (NCControls <> nil) and (NCControls.Visible) then
  begin
    P := _NormalizePoint(Point(Message.XCursor, Message.YCursor));
    if ((Message.HitTest = HTTOP) or (Message.HitTest = HTCAPTION)) and NCControls.PointInNCControl(P) then
    begin
      Message.Result := 0;
      Message.Msg := WM_NULL;
      Handled := True;
      Exit;
    end;
  end;
  inherited;
end;

procedure TFormStyleNCControls.WMNCLButtonDown(var Message: TWMNCHitMessage);
var
  P: TPoint;
begin
  inherited;
{$IF CompilerVersion>23}
  if not((TStyleManager.FormBorderStyle = fbsCurrentStyle) and (seBorder in Form.StyleElements)) then
  begin
    Handled := False;
    Exit;
  end;
{$IFEND}
  if (NCControls <> nil) and (NCControls.Visible) then
  begin
    P := _NormalizePoint(Point(Message.XCursor, Message.YCursor));
    if ((Message.HitTest = HTTOP) or (Message.HitTest = HTCAPTION)) and FNCControls.PointInNCControl(P) then
    begin
      FNCControls.FPressedControlIndex := FNCControls.GetNCControlIndex(P);

      if (FNCControls.FPressedControlIndex >= 0)  then
       FNCControls[FNCControls.FPressedControlIndex].Handle_WMNCLButtonDown(Message);

      InvalidateNC;
      Message.Result := 0;
      Message.Msg := WM_NULL;
      Handled := True;
    end;
  end;

end;

procedure TFormStyleNCControls.WMNCLButtonUp(var Message: TWMNCHitMessage);
var
  OldIndex: Integer;
  P: TPoint;
  LNCControl: TNCControl;
begin
  inherited;

  if (NCControls <> nil) and (NCControls.Visible) then
  begin
{$IF CompilerVersion>23}
    if not((TStyleManager.FormBorderStyle = fbsCurrentStyle) and (seBorder in Form.StyleElements)) then
    begin
      Handled := False;
      Exit;
    end;
{$IFEND}
    OldIndex := FNCControls.FPressedControlIndex;

    if FNCControls.FPressedControlIndex <> -1 then
    begin
      FNCControls.FPressedControlIndex := -1;
      InvalidateNC;
    end;

    if OldIndex = FNCControls.FHotControlIndex then
    begin
      P := _NormalizePoint(Point(Message.XCursor, Message.YCursor));
      NCControls.FLastPoint := P;
      NCControls.FControlUpIndex := NCControls.GetNCControlIndex(P);
      if NCControls.FControlUpIndex > -1 then
      begin
        LNCControl := NCControls.Controls[NCControls.FControlUpIndex];
        LNCControl.Handle_WMNCLButtonUp(Message);
      end;
    end;

    Message.Result := 0;
    Message.Msg := WM_NULL;
    Handled := True;
  end;
end;

procedure TFormStyleNCControls.WMNCMouseMove(var Message: TWMNCHitMessage);
var
  i: Integer;
  LNCControl : TNCControl;
begin
  inherited;
  if (NCControls <> nil) and (NCControls.Visible) then
  begin
    NCControls.FLastPoint := _NormalizePoint(Point(Message.XCursor, Message.YCursor));
    // OutputDebugString(PChar(Format('Message.HitTest %d XCursor %d YCursor %d  P.X %d  P.Y %d',[Message.HitTest, Message.XCursor, Message.YCursor, P.X, P.Y])));

{$IF CompilerVersion>23}
    if not((TStyleManager.FormBorderStyle = fbsCurrentStyle) and (seBorder in Form.StyleElements)) then
    begin
      Handled := False;
      Exit;
    end;
{$IFEND}
    i := NCControls.GetNCControlIndex(NCControls.LastPoint);
    if (i >= 0) then
    begin
      LNCControl := NCControls[i];
      LNCControl.Handle_WMNCMouseMove(Message);
    end
    else
    //notify to the controls the mouse leave.
    if NCControls.ControlsCount > 0 then
    begin
      LNCControl := NCControls[0];
      LNCControl.Handle_WMNCMouseMove(Message);
    end;

  end;
end;

procedure TFormStyleNCControls.PaintNC(Canvas: TCanvas);
var
  Details, CaptionDetails, IconDetails: TThemedElementDetails;
  Detail: TThemedWindow;
  R, R1, R2, DrawRect, ButtonRect, TextRect: TRect;
  CaptionBuffer: TBitmap;
  FButtonState: TThemedWindow;
  TextFormat: TTextFormat;
  LText: string;
  LStyleServices: TCustomStyleServices;
  TextTopOffset: Integer;

  function GetTopOffset: Integer;
  var
    P: TPoint;
  begin
    P.X := Form.Left + Form.Width div 2;
    P.Y := Form.Top + Form.Height div 2;
    Result := Screen.MonitorFromPoint(P).WorkareaRect.Top;
    if Form.Top < Result then
      Result := Result - Form.Top
    else
      Result := 0;
  end;

  procedure CorrectRightButtonRect(var AButtonRect: TRect);
  var
    TopOffset, RightOffset: Integer;
    BS: TRect;
  begin
    if (Form.WindowState = wsMaximized) and (TCustomFormClass(Form).FormStyle <> fsMDIChild) and (ButtonRect.Width > 0)
    then
    begin
      BS := _GetBorderSize;
      TopOffset := GetTopOffset;
      RightOffset := -BS.Right;
      if ButtonRect.Top < TopOffset then
      begin
        TopOffset := TopOffset - ButtonRect.Top;
        OffsetRect(ButtonRect, RightOffset, TopOffset);
        TopOffset := ButtonRect.Bottom - BS.Top;
        if TopOffset > 0 then
          OffsetRect(ButtonRect, 0, -TopOffset);
      end;
    end;
  end;

  procedure CorrectLeftButtonRect(var AButtonRect: TRect);
  var
    TopOffset, LeftOffset: Integer;
    BS: TRect;
  begin
    if (Form.WindowState = wsMaximized) and (TCustomFormClass(Form).FormStyle <> fsMDIChild) and (ButtonRect.Width > 0)
    then
    begin
      BS := _GetBorderSize;
      TopOffset := GetTopOffset;
      LeftOffset := BS.Left;
      if ButtonRect.Top < TopOffset then
      begin
        TopOffset := TopOffset - ButtonRect.Top;
        OffsetRect(ButtonRect, LeftOffset, TopOffset);
        TopOffset := ButtonRect.Bottom - BS.Top;
        if TopOffset > 0 then
          OffsetRect(ButtonRect, 0, -TopOffset);
      end;
    end;
  end;

begin
  if Form.BorderStyle = bsNone then
  begin
    MainMenuBarHookPaint(Canvas);
    Exit;
  end;

  if NCControls <> nil then
    LStyleServices := NCControls.StyleServices
  else
    LStyleServices := StyleServices;

  _FCloseButtonRect := Rect(0, 0, 0, 0);
  _FMaxButtonRect := Rect(0, 0, 0, 0);
  _FMinButtonRect := Rect(0, 0, 0, 0);
  _FHelpButtonRect := Rect(0, 0, 0, 0);
  _FSysMenuButtonRect := Rect(0, 0, 0, 0);
  _FCaptionRect := Rect(0, 0, 0, 0);

  if not LStyleServices.Available then
    Exit;
  R := _GetBorderSize;
  if (NCControls <> nil) then
    NCControls.FormBorderSize := R;

  if (Form.BorderStyle <> bsToolWindow) and (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if _FFormActive then
      Detail := twCaptionActive
    else
      Detail := twCaptionInActive
  end
  else
  begin
    if _FFormActive then
      Detail := twSmallCaptionActive
    else
      Detail := twSmallCaptionInActive
  end;

  CaptionBuffer := TBitmap.Create;
  try
    CaptionBuffer.SetSize(_FWidth, R.Top);
    // caption
    DrawRect := Rect(0, 0, CaptionBuffer.Width, CaptionBuffer.Height);
    Details := LStyleServices.GetElementDetails(Detail);
    LStyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, DrawRect);
    TextRect := DrawRect;
    CaptionDetails := Details;
    TextTopOffset := 3;

    // icon
    if ((NCControls <> nil) and NCControls.ShowSystemMenu) and (biSystemMenu in TCustomFormClass(Form).BorderIcons) and
      (Form.BorderStyle <> bsDialog) and (Form.BorderStyle <> bsToolWindow) and (Form.BorderStyle <> bsSizeToolWin) then
    begin
      IconDetails := LStyleServices.GetElementDetails(twSysButtonNormal);
      if not LStyleServices.GetElementContentRect(0, IconDetails, DrawRect, ButtonRect) then
        ButtonRect := Rect(0, 0, 0, 0);

      R1 := ButtonRect;

{$IF CompilerVersion > 23}
      if not StyleServices.HasElementFixedPosition(Details) then
      begin
{$IFEND}
        CorrectLeftButtonRect(ButtonRect);
        TextTopOffset := Abs(R1.Top - ButtonRect.Top);
        if TextTopOffset > R.Top then
          TextTopOffset := 3;
{$IF CompilerVersion > 23}
      end
      else
        TextTopOffset := 0;
{$IFEND}
      R1 := Rect(0, 0, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON));
      RectVCenter(R1, ButtonRect);
      if ButtonRect.Width > 0 then
        DrawIconEx(CaptionBuffer.Canvas.Handle, R1.Left, R1.Top, _GetIconFast.Handle, 0, 0, 0, 0, DI_NORMAL);
      Inc(TextRect.Left, ButtonRect.Width + 5);
      _FSysMenuButtonRect := ButtonRect;
    end
    else
      Inc(TextRect.Left, R.Left);

    // buttons
    if (biSystemMenu in TCustomFormClass(Form).BorderIcons) then
    begin
      if (Form.BorderStyle <> bsToolWindow) and (Form.BorderStyle <> bsSizeToolWin) then
      begin
        if (_FPressedButton = HTCLOSE) and (_FHotButton = HTCLOSE) then
          FButtonState := twCloseButtonPushed
        else if _FHotButton = HTCLOSE then
          FButtonState := twCloseButtonHot
        else if _FFormActive then
          FButtonState := twCloseButtonNormal
        else
          FButtonState := twCloseButtonDisabled;
      end
      else
      begin
        if (_FPressedButton = HTCLOSE) and (_FHotButton = HTCLOSE) then
          FButtonState := twSmallCloseButtonPushed
        else if _FHotButton = HTCLOSE then
          FButtonState := twSmallCloseButtonHot
        else if _FFormActive then
          FButtonState := twSmallCloseButtonNormal
        else
          FButtonState := twSmallCloseButtonDisabled;
      end;

      Details := LStyleServices.GetElementDetails(FButtonState);
      if not LStyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect) then
        ButtonRect := Rect(0, 0, 0, 0);

{$IF CompilerVersion > 23} if not StyleServices.HasElementFixedPosition(Details) then {$IFEND}
        CorrectRightButtonRect(ButtonRect);

      LStyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, ButtonRect);

      if ButtonRect.Left > 0 then
        TextRect.Right := ButtonRect.Left;
      _FCloseButtonRect := ButtonRect;
    end;

    if (biMaximize in TCustomFormClass(Form).BorderIcons) and (biSystemMenu in TCustomFormClass(Form).BorderIcons) and
      (Form.BorderStyle <> bsDialog) and (Form.BorderStyle <> bsToolWindow) and (Form.BorderStyle <> bsSizeToolWin) then
    begin
      if Form.WindowState = wsMaximized then
      begin

        if (_FPressedButton = HTMAXBUTTON) and (_FHotButton = HTMAXBUTTON) then
          FButtonState := twRestoreButtonPushed
        else if _FHotButton = HTMAXBUTTON then
          FButtonState := twRestoreButtonHot
        else if _FFormActive then
          FButtonState := twRestoreButtonNormal
        else
          FButtonState := twRestoreButtonDisabled;
      end
      else
      begin
        if (_FPressedButton = HTMAXBUTTON) and (_FHotButton = HTMAXBUTTON) then
          FButtonState := twMaxButtonPushed
        else if _FHotButton = HTMAXBUTTON then
          FButtonState := twMaxButtonHot
        else if _FFormActive then
          FButtonState := twMaxButtonNormal
        else
          FButtonState := twMaxButtonDisabled;
      end;

      Details := LStyleServices.GetElementDetails(FButtonState);

      if not LStyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect) then
        ButtonRect := Rect(0, 0, 0, 0);

{$IF CompilerVersion > 23} if not StyleServices.HasElementFixedPosition(Details) then {$IFEND}
        CorrectRightButtonRect(ButtonRect);

      if ButtonRect.Width > 0 then
        LStyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, ButtonRect);

      if ButtonRect.Left > 0 then
        TextRect.Right := ButtonRect.Left;
      _FMaxButtonRect := ButtonRect;
    end;

    if (biMinimize in TCustomFormClass(Form).BorderIcons) and (biSystemMenu in TCustomFormClass(Form).BorderIcons) and
      (Form.BorderStyle <> bsDialog) and (Form.BorderStyle <> bsToolWindow) and (Form.BorderStyle <> bsSizeToolWin) then
    begin
      if (_FPressedButton = HTMINBUTTON) and (_FHotButton = HTMINBUTTON) then
        FButtonState := twMinButtonPushed
      else if _FHotButton = HTMINBUTTON then
        FButtonState := twMinButtonHot
      else if _FFormActive then
        FButtonState := twMinButtonNormal
      else
        FButtonState := twMinButtonDisabled;

      Details := LStyleServices.GetElementDetails(FButtonState);

      if not LStyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect) then
        ButtonRect := Rect(0, 0, 0, 0);

{$IF CompilerVersion > 23} if not StyleServices.HasElementFixedPosition(Details) then {$IFEND}
        CorrectRightButtonRect(ButtonRect);

      if ButtonRect.Width > 0 then
        LStyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, ButtonRect);

      if ButtonRect.Left > 0 then
        TextRect.Right := ButtonRect.Left;
      _FMinButtonRect := ButtonRect;
    end;

    if (biHelp in TCustomFormClass(Form).BorderIcons) and (biSystemMenu in TCustomFormClass(Form).BorderIcons) and
      ((not(biMaximize in TCustomFormClass(Form).BorderIcons) and not(biMinimize in TCustomFormClass(Form).BorderIcons))
      or (Form.BorderStyle = bsDialog)) then
    begin
      if (_FPressedButton = HTHELP) and (_FHotButton = HTHELP) then
        FButtonState := twHelpButtonPushed
      else if _FHotButton = HTHELP then
        FButtonState := twHelpButtonHot
      else if _FFormActive then
        FButtonState := twHelpButtonNormal
      else
        FButtonState := twHelpButtonDisabled;

      Details := LStyleServices.GetElementDetails(FButtonState);

      if not LStyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect) then
        ButtonRect := Rect(0, 0, 0, 0);

{$IF CompilerVersion > 23} if not StyleServices.HasElementFixedPosition(Details) then {$IFEND}
        CorrectRightButtonRect(ButtonRect);

      if ButtonRect.Width > 0 then
        LStyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, ButtonRect);

      if ButtonRect.Left > 0 then
        TextRect.Right := ButtonRect.Left;
      _FHelpButtonRect := ButtonRect;
    end;

    R2 := TextRect;

    if (NCControls <> nil) and (NCControls.ControlsCount > 0) and (NCControls.Visible) then
      Inc(TextRect.Left, NCControls.Controls[NCControls.ControlsCount - 1].BoundsRect.Right - NCControls.Controls[0].BoundsRect.Left + 10);

    // text
    if (NCControls <> nil) and (IsIconic(TCustomFormClass(Form).Handle) or (NCControls.ShowCaption)) then
    begin

      TextFormat := [tfLeft, tfSingleLine, tfVerticalCenter];
      if Control.UseRightToLeftReading then
        Include(TextFormat, tfRtlReading);

      LText := Text;

      if (TCustomFormClass(Form).WindowState = wsMaximized) and (TCustomFormClass(Form).FormStyle <> fsMDIChild) and
        (TextTopOffset <> 0) and (biSystemMenu in TCustomFormClass(Form).BorderIcons) then
      begin
        Inc(TextRect.Left, R.Left);
        MoveWindowOrg(CaptionBuffer.Canvas.Handle, 0, TextTopOffset);
        LStyleServices.DrawText(CaptionBuffer.Canvas.Handle, CaptionDetails, LText, TextRect, TextFormat);
        MoveWindowOrg(CaptionBuffer.Canvas.Handle, 0, -TextTopOffset);
      end
      else
        LStyleServices.DrawText(CaptionBuffer.Canvas.Handle, CaptionDetails, LText, TextRect, TextFormat);
    end;

    if (NCControls <> nil) and (NCControls.ControlsCount > 0) and (NCControls.Visible) then
      Dec(TextRect.Left, NCControls.Controls[NCControls.ControlsCount - 1].BoundsRect.Right - NCControls.Controls[0].BoundsRect.Left + 10);

    _FCaptionRect := TextRect;

    if not IsIconic(TCustomFormClass(Form).Handle) then
      PaintNCControls(CaptionBuffer.Canvas, R2);

    // caption
    Canvas.Draw(0, 0, CaptionBuffer);
  finally
    CaptionBuffer.Free;
  end;

  // menubar
  MainMenuBarHookPaint(Canvas);

  // left
  if (Form.BorderStyle <> bsToolWindow) and (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if _FFormActive then
      Detail := twFrameLeftActive
    else
      Detail := twFrameLeftInActive
  end
  else
  begin
    if _FFormActive then
      Detail := twSmallFrameLeftActive
    else
      Detail := twSmallFrameLeftInActive
  end;

  DrawRect := Rect(0, R.Top, R.Left, _FHeight - R.Bottom);
  Details := LStyleServices.GetElementDetails(Detail);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    LStyleServices.DrawElement(Canvas.Handle, Details, DrawRect);

  // right
  if (Form.BorderStyle <> bsToolWindow) and (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if _FFormActive then
      Detail := twFrameRightActive
    else
      Detail := twFrameRightInActive
  end
  else
  begin
    if _FFormActive then
      Detail := twSmallFrameRightActive
    else
      Detail := twSmallFrameRightInActive
  end;

  DrawRect := Rect(_FWidth - R.Right, R.Top, _FWidth, _FHeight - R.Bottom);
  Details := LStyleServices.GetElementDetails(Detail);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    LStyleServices.DrawElement(Canvas.Handle, Details, DrawRect);

  // Bottom
  if (Form.BorderStyle <> bsToolWindow) and (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if _FFormActive then
      Detail := twFrameBottomActive
    else
      Detail := twFrameBottomInActive
  end
  else
  begin
    if _FFormActive then
      Detail := twSmallFrameBottomActive
    else
      Detail := twSmallFrameBottomInActive
  end;

  DrawRect := Rect(0, _FHeight - R.Bottom, _FWidth, _FHeight);
  Details := LStyleServices.GetElementDetails(Detail);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    LStyleServices.DrawElement(Canvas.Handle, Details, DrawRect);
end;

// This custom GetBorderSize method is necessary to allow to the NC controls use a custom Style in the title and border area.
function Detour_TFormStyleHook_GetBorderSize(Self: TFormStyleHook): TRect;
var
  Size: TSize;
  Details: TThemedElementDetails;
  Detail: TThemedWindow;
  LStylesServices: TCustomStyleServices;
  LForm: TCustomForm;
begin
  if not(ExecutingInMainThread) then
    Exit(Trampoline_TFormStyleHook_GetBorderSize(Self));

  if (Self is TFormStyleNCControls) and (TFormStyleNCControls(Self).NCControls <> nil) then
  begin
    LStylesServices := TFormStyleNCControls(Self).NCControls.StyleServices;
    LForm := TFormStyleHookClass(Self)._Form;
    Result := Rect(0, 0, 0, 0);
    if LForm.BorderStyle = bsNone then
      Exit;

    if not LStylesServices.Available then
      Exit;
    { caption height }
    if (LForm.BorderStyle <> bsToolWindow) and (LForm.BorderStyle <> bsSizeToolWin) then
      Detail := twCaptionActive
    else
      Detail := twSmallCaptionActive;
    Details := LStylesServices.GetElementDetails(Detail);
    LStylesServices.GetElementSize(0, Details, esActual, Size);
    Result.Top := Size.cy;
    { left border width }
    if (LForm.BorderStyle <> bsToolWindow) and (LForm.BorderStyle <> bsSizeToolWin) then
      Detail := twFrameLeftActive
    else
      Detail := twSmallFrameLeftActive;
    Details := LStylesServices.GetElementDetails(Detail);
    LStylesServices.GetElementSize(0, Details, esActual, Size);
    Result.Left := Size.cx;
    { right border width }
    if (LForm.BorderStyle <> bsToolWindow) and (LForm.BorderStyle <> bsSizeToolWin) then
      Detail := twFrameRightActive
    else
      Detail := twSmallFrameRightActive;
    Details := LStylesServices.GetElementDetails(Detail);
    LStylesServices.GetElementSize(0, Details, esActual, Size);
    Result.Right := Size.cx;
    { bottom border height }
    if (LForm.BorderStyle <> bsToolWindow) and (LForm.BorderStyle <> bsSizeToolWin) then
      Detail := twFrameBottomActive
    else
      Detail := twSmallFrameBottomActive;
    Details := LStylesServices.GetElementDetails(Detail);
    LStylesServices.GetElementSize(0, Details, esActual, Size);
    Result.Bottom := Size.cy;

//    //Result.Top     := Result.Top + 5;
//    Result.Left    := 0;
//    Result.Right   := 0;
//    Result.Bottom  := 0;
    if TFormStyleNCSettings.UseThinBorder then
    begin
      //Don't use 0, because the native resize feature is lost if the border is 0.
      Result.Left    := 1;
      Result.Right   := 1;
      Result.Bottom  := 1;
    end;
  end
  else
    Exit(Trampoline_TFormStyleHook_GetBorderSize(Self));
end;

// This custom GetRegion method is necessary to allow to the NC controls use a custom Style in the title and border area.
function Detour_TFormStyleHook_GetRegion(Self:TFormStyleHook): HRgn;
var
  R: TRect;
  Details: TThemedElementDetails;
  Detail: TThemedWindow;
  LStylesServices: TCustomStyleServices;
  LForm: TCustomForm;
begin
  if not(ExecutingInMainThread) then
    Exit(Trampoline_TFormStyleHook_GetRegion(Self));

  if (Self is TFormStyleNCControls) and (TFormStyleNCControls(Self).NCControls <> nil) then
  begin
    LStylesServices := TFormStyleNCControls(Self).NCControls.StyleServices;
    LForm := TFormStyleHookClass(Self)._Form;
    Result := 0;
    if not LStylesServices.Available then
      Exit;

    R := Rect(0, 0, TFormStyleHookClass(Self)._FWidth, TFormStyleHookClass(Self)._FHeight);
    if (LForm.BorderStyle <> bsToolWindow) and (LForm.BorderStyle <> bsSizeToolWin) then
      Detail := twCaptionActive
    else
      Detail := twSmallCaptionActive;
    Details := LStylesServices.GetElementDetails(Detail);
    LStylesServices.GetElementRegion(Details, R, Result);
  end
  else
    Exit(Trampoline_TFormStyleHook_GetRegion(Self));
end;

{ TListNCButtons }
function TListNCControls.Add: TNCControl;
begin
  Result := TNCControl(inherited Add);
end;


function RttiMethodInvokeEx(const MethodName:string; RttiType : TRttiType; Instance: TValue; const Args: array of TValue): TValue;
var
 Found   : Boolean;
 LMethod : TRttiMethod;
 LIndex  : Integer;
 LParams : TArray<TRttiParameter>;
begin
  Result := nil;
  LMethod := nil;
  Found := False;
  for LMethod in RttiType.GetMethods do
   if SameText(LMethod.Name, MethodName) then
   begin
     LParams := LMethod.GetParameters;
     if Length(Args) = Length(LParams) then
     begin
       Found := True;
       for LIndex := 0 to Length(LParams) - 1 do
       if LParams[LIndex].ParamType.Handle <> Args[LIndex].TypeInfo then
       begin
         Found := False;
         Break;
       end;
     end;

     if Found then Break;
   end;

   if (LMethod <> nil) and Found then
     Result := LMethod.Invoke(Instance, Args)
   else
     raise Exception.CreateFmt('method %s not found',[MethodName]);
end;

function TListNCControls.AddEx<T>: T;
var
  LRttiContext : TRttiContext;
  LRttiType : TRttiType;
  LRttiInstanceType : TRttiInstanceType;
  LValue : TValue;
begin
  LRttiContext := TRttiContext.Create;
  LRttiType := LRttiContext.GetType(TypeInfo(T));
  if (LRttiType <> nil) then
  begin
    LRttiInstanceType := LRttiType.AsInstance;
    LValue := LRttiInstanceType.GetMethod('Create').Invoke(LRttiInstanceType.MetaclassType, [Self]);
    //LValue := RttiMethodInvokeEx('Create', LRttiInstanceType, LRttiInstanceType.MetaclassType, [Self]);
  end;

  Result := LValue.AsType<T>;
end;

constructor TListNCControls.Create(AOwner: TPersistent);
begin
  FOwner := TNCControls(AOwner);
  inherited Create(TNCControl);
end;


destructor TListNCControls.Destroy;
begin
  inherited;
end;

function TListNCControls.GetItem(Index: Integer): TNCControl;
begin
  Result := TNCControl(inherited GetItem(Index));
end;

function TListNCControls.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TListNCControls.Insert(Index: Integer): TNCControl;
begin
  Result := TNCControl(inherited Insert(Index));
end;

procedure TListNCControls.SetItem(Index: Integer; Value: TNCControl);
begin
  inherited SetItem(Index, Value);
end;

{ TNCControl }

constructor TNCControl.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FNCControls := TNCControls(Collection.Owner);
  FEnabled := True;
  FVisible := True;
  FFont := TFont.Create;
  FWidth := 80;
  FTop := 5;
  FCaptionAligmentFlags := DT_VCENTER or DT_CENTER or DT_WORDBREAK;
end;

destructor TNCControl.Destroy;
begin
  FFont.Free;
  inherited;
end;

function TNCControl.GetAs<T>: T;
var
 LValue: TValue;
begin
 LValue := TValue.From(Self);
 Result := LValue.AsType<T>;
end;

function TNCControl.GetBoundsRect: TRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Left + Width;
  Result.Bottom := Top + Height;
end;

function TNCControl.GetEnabled: Boolean;
begin
 Result := FEnabled;
end;

procedure TNCControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FLeft := ALeft;
  FTop := ATop;
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TNCControl.SetBoundsRect(const Value: TRect);
begin
  with Value do SetBounds(Left, Top, Right - Left, Bottom - Top);
end;


procedure TNCControl.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
    FEnabled := Value;
end;

procedure TNCControl.SetFont(const Value: TFont);
begin
  FFont := Value;
end;

procedure TNCControl.SetHeight(const Value: Integer);
begin
  FHeight := Value;
end;

procedure TNCControl.SetLeft(const Value: Integer);
begin
  FLeft := Value;
end;

procedure TNCControl.SetName(const Value: TComponentName);
begin
  FName := Value;

end;

procedure TNCControl.SetShowHint(const Value: Boolean);
begin
  FShowHint := Value;
end;

procedure TNCControl.SetTop(const Value: Integer);
begin
  FTop := Value;
end;

procedure TNCControl.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
    FVisible := Value;
  // TODO : Add parent notification
end;

procedure TNCControl.SetWidth(const Value: Integer);
begin
  FWidth := Value;
end;

procedure TNCControl.DrawControl(ACanvas: TCanvas; AMouseInControl, Pressed: Boolean);
begin
end;

procedure TNCControl.Handle_WMNCLButtonDown(var Message: TWMNCHitMessage);
begin
end;

procedure TNCControl.Handle_WMNCLButtonUp(var Message: TWMNCHitMessage);
begin
end;

procedure TNCControl.Handle_WMNCMouseMove(var Message: TWMNCHitMessage);
begin
end;


initialization

 TFormStyleNCSettings.UseThinBorder := false;
{$IFDEF CPUX86}
  Trampoline_TFormStyleHook_GetBorderSize := InterceptCreate(TFormStyleHookClass(nil)._GetBorderSizeAddr,
  @Detour_TFormStyleHook_GetBorderSize);
{$ENDIF}
Trampoline_TFormStyleHook_GetRegion := InterceptCreate(TFormStyleHookClass(nil)._GetRegionAddr,
  @Detour_TFormStyleHook_GetRegion);

finalization

{$IFDEF CPUX86}
  InterceptRemove(@Trampoline_TFormStyleHook_GetBorderSize);
{$ENDIF}
InterceptRemove(@Trampoline_TFormStyleHook_GetRegion);

end.
