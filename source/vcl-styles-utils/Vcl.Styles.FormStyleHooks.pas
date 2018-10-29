// **************************************************************************************************
//
// Unit Vcl.Styles.FormStyleHooks
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
// The Original Code is Vcl.Styles.FormStyleHooks.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2012-2016 Rodrigo Ruz V.
// All Rights Reserved.
//
// **************************************************************************************************

unit Vcl.Styles.FormStyleHooks;

interface
{$IF RTLVersion>=24}
  {$LEGACYIFEND ON}
{$IFEND}
uses
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Themes,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.Graphics,
{$IF (CompilerVersion >= 31)}
  Vcl.Styles.Utils.Shadow,
{$IFEND}
  Vcl.Forms;

type
  /// <summary> Form Style hook to add image and/or color supoort for the background and non client area
  /// </summary>
  TFormStyleHookBackground = class({$IF (CompilerVersion >= 31)}Vcl.Styles.Utils.Shadow.{$IFEND}TFormStyleHook)
  strict private
  type
    TSettings = class
    strict private
      FColor: TColor;
      FImageLocation: string;
      FBitmap: TBitmap;
      FUseColor: Boolean;
      FUseImage: Boolean;
      FEnabled: Boolean;
      FUseAlpha: Boolean;
      FAlphaValue: Byte;
      procedure SetColor(const Value: TColor);
      procedure SetImageLocation(const Value: string);
      procedure SetUseColor(const Value: Boolean);
      procedure SetUseImage(const Value: Boolean);
    public
      property UseImage: Boolean read FUseImage write SetUseImage;
      property UseColor: Boolean read FUseColor write SetUseColor;
      property Color: TColor read FColor write SetColor;
      property ImageLocation: string read FImageLocation write SetImageLocation;
      property Bitmap: TBitmap read FBitmap;
      property Enabled: Boolean read FEnabled write FEnabled;
      property UseAlpha: Boolean read FUseAlpha write FUseAlpha;
      property AlphaValue: Byte read FAlphaValue write FAlphaValue;
      constructor Create;
      destructor Destroy; override;
    end;
    class var FNCSettings: TSettings;
    class var FBackGroundSettings: TSettings;
    class var FMergeImages: Boolean;
    class Var FSharedBitMap: TBitmap;
    class var FSharedImageLocation: string;
    class procedure SetSharedImageLocation(const Value: string); static;
  protected
    procedure PaintNC(Canvas: TCanvas); override;
    procedure PaintBackground(Canvas: TCanvas); override;
    class constructor Create;
    class destructor Destroy;
  public
    class property SharedImageLocation: string read FSharedImageLocation
      write SetSharedImageLocation;
    class property SharedBitMap: TBitmap read FSharedBitMap write FSharedBitMap;
    class property MergeImages: Boolean read FMergeImages write FMergeImages;
    class property NCSettings: TSettings read FNCSettings;
    class property BackGroundSettings: TSettings read FBackGroundSettings;
  end;

  /// <summary> Form Style hook to disable vcl styles in the non client area
  /// </summary>
  TFormStyleHookNC = class(TMouseTrackControlStyleHook)
  public
    procedure PaintBackground(Canvas: TCanvas); override;
    constructor Create(AControl: TWinControl); override;
  end;

  /// <summary> Form Style hook to add support for the regions in the non client area
  /// </summary>
  TFormStyleHookRgn = class({$IF (CompilerVersion >= 31)}Vcl.Styles.Utils.Shadow.{$IFEND}TFormStyleHook)
  private
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging);
      message WM_WINDOWPOSCHANGING;
  protected
    procedure PaintNC(Canvas: TCanvas); override;
  end;

  TFormStyleHookHelper = class helper for {$IF (CompilerVersion >= 31)}Vcl.Styles.Utils.Shadow.{$IFEND}TFormStyleHook
  private
    function GetFCloseButtonRect: TRect;
    procedure SetFCloseButtonRect(const Value: TRect);
    function GetFCaptionRect: TRect;
    function GetFHelpButtonRect: TRect;
    function GetFMaxButtonRect: TRect;
    function GetFMinButtonRect: TRect;
    function GetFSysMenuButtonRect: TRect;
    procedure SetFCaptionRect(const Value: TRect);
    procedure SetFHelpButtonRect(const Value: TRect);
    procedure SetFMaxButtonRect(const Value: TRect);
    procedure SetFMinButtonRect(const Value: TRect);
    procedure SetFSysMenuButtonRect(const Value: TRect);
    function GetFFormActive: Boolean;
    function GetFWidth: Integer;
    function GetFPressedButton: Integer;
    function GetFHotButton: Integer;
    function GetFHeight: Integer;
    function GetFChangeSizeCalled: Boolean;
    function GetFLeft: Integer;
    function GetFTop: Integer;
    procedure SetFLeft(const Value: Integer);
    procedure SetFTop(const Value: Integer);
    procedure SetFHeight(const Value: Integer);
    procedure SetFWidth(const Value: Integer);
    procedure SetFChangeSizeCalled(const Value: Boolean);
    function GetFRegion: HRGN;
    procedure SetFRegion(const Value: HRGN);
    function GetForm: TCustomForm;
  public
    property _FCloseButtonRect: TRect read GetFCloseButtonRect
      Write SetFCloseButtonRect;
    property _FMaxButtonRect: TRect read GetFMaxButtonRect
      Write SetFMaxButtonRect;
    property _FMinButtonRect: TRect read GetFMinButtonRect
      Write SetFMinButtonRect;
    property _FHelpButtonRect: TRect read GetFHelpButtonRect
      Write SetFHelpButtonRect;
    property _FSysMenuButtonRect: TRect read GetFSysMenuButtonRect
      Write SetFSysMenuButtonRect;
    property _FCaptionRect: TRect read GetFCaptionRect Write SetFCaptionRect;
    function _GetBorderSize: TRect;
    property _FFormActive: Boolean read GetFFormActive;
    property _FChangeSizeCalled: Boolean read GetFChangeSizeCalled
      write SetFChangeSizeCalled;
    property _FWidth: Integer read GetFWidth write SetFWidth;
    property _FHeight: Integer read GetFHeight write SetFHeight;
    property _FLeft: Integer read GetFLeft write SetFLeft;
    property _FTop: Integer read GetFTop write SetFTop;
    property _FPressedButton: Integer read GetFPressedButton;
    property _FHotButton: Integer read GetFHotButton;
    property _FRegion: HRGN read GetFRegion write SetFRegion;
    property _Form: TCustomForm read GetForm;
    procedure MainMenuBarHookPaint(Canvas: TCanvas);
    function _GetIconFast: TIcon;
    procedure _ChangeSize;
    function _NormalizePoint(P: TPoint): TPoint;
    function _GetHitTest(P: TPoint): Integer;
    function _GetBorderSizeAddr: Pointer;
    function _GetRegionAddr: Pointer;
  end;

function RectVCenter(var R: TRect; Bounds: TRect): TRect;

implementation

Uses
  System.SysUtils,
  System.Classes,
  System.Types,
{$IF (CompilerVersion >= 31)}
  System.RTTI,
{$IFEND}
  Winapi.UxTheme,
  Vcl.Imaging.Jpeg,
  Vcl.Imaging.pngimage,
  Vcl.Imaging.GIFImg;

type
  TCustomFormClass = class(TCustomForm);

function RectVCenter(var R: TRect; Bounds: TRect): TRect;
begin
  OffsetRect(R, -R.Left, -R.Top);
  OffsetRect(R, 0, (Bounds.Height - R.Height) div 2);
  OffsetRect(R, Bounds.Left, Bounds.Top);
  Result := R;
end;

{ TFormStyleHookRgn }

procedure TFormStyleHookRgn.WMWindowPosChanging(var Message
  : TWMWindowPosChanging);
var
  Changed: Boolean;

  function GetRegion: HRGN;
  var
    R: TRect;
    Details: TThemedElementDetails;
    Detail: TThemedWindow;
  begin
    Result := 0;
    if not StyleServices.Available then
      Exit;

    R := Rect(0, 0, _FWidth, _FHeight);
    if (Form.BorderStyle <> bsToolWindow) and (Form.BorderStyle <> bsSizeToolWin)
    then
      Detail := twCaptionActive
    else
      Detail := twSmallCaptionActive;
    Details := StyleServices.GetElementDetails(Detail);
    StyleServices.GetElementRegion(Details, R, Result);
  end;

  procedure ChangeSize;
  var
    TempRegion: HRGN;
    FormhRgn: HRGN;
    R: TRect;
  begin
    _FChangeSizeCalled := True;

    if IsIconic(Handle) then
    begin
      R := _GetBorderSize;
      _FHeight := R.Top + R.Bottom;
    end;

    if Form.BorderStyle <> bsNone then
    begin
      TempRegion := _FRegion;
      try
        _FRegion := GetRegion;
        FormhRgn := 0;
        GetWindowRgn(Handle, FormhRgn);
        {
          CombineRgn(FormhRgn, FormhRgn,_FRegion,RGN_OR);
          SetWindowRgn(Handle, FormhRgn, True);
        }
        // SetWindowRgn(Handle, _FRegion, True);
      finally
        if TempRegion <> 0 then
          DeleteObject(TempRegion);
      end;
      _FChangeSizeCalled := False;
    end;
  end;

begin
  CallDefaultProc(TMessage(Message));

  Handled := True;
  Changed := False;

  if _FChangeSizeCalled then
    Exit;

  if (Message.WindowPos^.flags and SWP_NOSIZE = 0) or
    (Message.WindowPos^.flags and SWP_NOMOVE = 0) then
  begin
    if (Message.WindowPos^.flags and SWP_NOMOVE = 0) then
    begin
      _FLeft := Message.WindowPos^.x;
      _FTop := Message.WindowPos^.y;
    end;
    if (Message.WindowPos^.flags and SWP_NOSIZE = 0) then
    begin
      Changed := ((Message.WindowPos^.cx <> _FWidth) or
        (Message.WindowPos^.cy <> _FHeight)) and
        (Message.WindowPos^.flags and SWP_NOSIZE = 0);
      _FWidth := Message.WindowPos^.cx;
      _FHeight := Message.WindowPos^.cy;
    end;
  end;

  if Changed then
  begin
    ChangeSize;
    if Form.BorderStyle <> bsNone then
      InvalidateNC;
  end;
end;

procedure TFormStyleHookRgn.PaintNC(Canvas: TCanvas);
var
  Details, CaptionDetails, IconDetails: TThemedElementDetails;
  Detail: TThemedWindow;
  R, R1, DrawRect, ButtonRect, TextRect: TRect;
  CaptionBuffer: TBitmap;
  FButtonState: TThemedWindow;
  TextFormat: TTextFormat;
  LText: string;
begin

  if Form.BorderStyle = bsNone then
  begin
    MainMenuBarHookPaint(Canvas);
    Exit;
  end;

  { init some parameters }
  _FCloseButtonRect := Rect(0, 0, 0, 0);
  _FMaxButtonRect := Rect(0, 0, 0, 0);
  _FMinButtonRect := Rect(0, 0, 0, 0);
  _FHelpButtonRect := Rect(0, 0, 0, 0);
  _FSysMenuButtonRect := Rect(0, 0, 0, 0);
  _FCaptionRect := Rect(0, 0, 0, 0);

  if not StyleServices.Available then
    Exit;
  R := _GetBorderSize;

  { draw caption }

  if (Form.BorderStyle <> bsToolWindow) and (Form.BorderStyle <> bsSizeToolWin)
  then
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
  CaptionBuffer.SetSize(_FWidth, R.Top);

  { draw caption border }
  DrawRect := Rect(0, 0, CaptionBuffer.Width, CaptionBuffer.Height);
  Details := StyleServices.GetElementDetails(Detail);
  StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, DrawRect);
  TextRect := DrawRect;
  CaptionDetails := Details;

  { draw icon }
  if (biSystemMenu in TCustomFormClass(Form).BorderIcons) and
    (Form.BorderStyle <> bsDialog) and (Form.BorderStyle <> bsToolWindow) and
    (Form.BorderStyle <> bsSizeToolWin) then
  begin
    IconDetails := StyleServices.GetElementDetails(twSysButtonNormal);
    if not StyleServices.GetElementContentRect(0, IconDetails, DrawRect,
      ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    R1 := Rect(0, 0, GetSystemMetrics(SM_CXSMICON),
      GetSystemMetrics(SM_CYSMICON));
    RectVCenter(R1, ButtonRect);
    if ButtonRect.Width > 0 then
      DrawIconEx(CaptionBuffer.Canvas.Handle, R1.Left, R1.Top,
        _GetIconFast.Handle, 0, 0, 0, 0, DI_NORMAL);
    Inc(TextRect.Left, ButtonRect.Width + 5);
    _FSysMenuButtonRect := ButtonRect;
  end
  else
    Inc(TextRect.Left, R.Left);

  { draw buttons }
  if (biSystemMenu in TCustomFormClass(Form).BorderIcons) then
  begin
    if (Form.BorderStyle <> bsToolWindow) and (Form.BorderStyle <> bsSizeToolWin)
    then
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

    Details := StyleServices.GetElementDetails(FButtonState);
    if not StyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect)
    then
      ButtonRect := Rect(0, 0, 0, 0);

    StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, ButtonRect);
    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    _FCloseButtonRect := ButtonRect;
  end;

  if (biMaximize in TCustomFormClass(Form).BorderIcons) and
    (biSystemMenu in TCustomFormClass(Form).BorderIcons) and
    (Form.BorderStyle <> bsDialog) and (Form.BorderStyle <> bsToolWindow) and
    (Form.BorderStyle <> bsSizeToolWin) then
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
    Details := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect)
    then
      ButtonRect := Rect(0, 0, 0, 0);
    if ButtonRect.Width > 0 then
      StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details,
        ButtonRect);
    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    _FMaxButtonRect := ButtonRect;
  end;

  if (biMinimize in TCustomFormClass(Form).BorderIcons) and
    (biSystemMenu in TCustomFormClass(Form).BorderIcons) and
    (Form.BorderStyle <> bsDialog) and (Form.BorderStyle <> bsToolWindow) and
    (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if (_FPressedButton = HTMINBUTTON) and (_FHotButton = HTMINBUTTON) then
      FButtonState := twMinButtonPushed
    else if _FHotButton = HTMINBUTTON then
      FButtonState := twMinButtonHot
    else if _FFormActive then
      FButtonState := twMinButtonNormal
    else
      FButtonState := twMinButtonDisabled;

    Details := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect)
    then
      ButtonRect := Rect(0, 0, 0, 0);
    if ButtonRect.Width > 0 then
      StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details,
        ButtonRect);
    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    _FMinButtonRect := ButtonRect;
  end;

  if (biHelp in TCustomFormClass(Form).BorderIcons) and
    (biSystemMenu in TCustomFormClass(Form).BorderIcons) and
    ((not(biMaximize in TCustomFormClass(Form).BorderIcons) and
    not(biMinimize in TCustomFormClass(Form).BorderIcons)) or
    (Form.BorderStyle = bsDialog)) then
  begin
    if (_FPressedButton = HTHELP) and (_FHotButton = HTHELP) then
      FButtonState := twHelpButtonPushed
    else if _FHotButton = HTHELP then
      FButtonState := twHelpButtonHot
    else if _FFormActive then
      FButtonState := twHelpButtonNormal
    else
      FButtonState := twHelpButtonDisabled;
    Details := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect)
    then
      ButtonRect := Rect(0, 0, 0, 0);
    if ButtonRect.Width > 0 then
      StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details,
        ButtonRect);

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    _FHelpButtonRect := ButtonRect;
  end;

  { draw text }
  TextFormat := [tfLeft, tfSingleLine, tfVerticalCenter];
  if Control.UseRightToLeftReading then
    Include(TextFormat, tfRtlReading);
  // Important: Must retrieve Text prior to calling DrawText as it causes
  // CaptionBuffer.Canvas to free its handle, making the outcome of the call
  // to DrawText dependent on parameter evaluation order.
  LText := Text;
  StyleServices.DrawText(CaptionBuffer.Canvas.Handle, CaptionDetails, LText,
    TextRect, TextFormat);
  _FCaptionRect := TextRect;

  { draw caption buffer }

  Canvas.Draw(0, 0, CaptionBuffer);
  CaptionBuffer.Free;

  { draw menubar }
  MainMenuBarHookPaint(Canvas);

  { draw left border }

  if (Form.BorderStyle <> bsToolWindow) and (Form.BorderStyle <> bsSizeToolWin)
  then
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
  Details := StyleServices.GetElementDetails(Detail);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    StyleServices.DrawElement(Canvas.Handle, Details, DrawRect);

  { draw right border }
  if (Form.BorderStyle <> bsToolWindow) and (Form.BorderStyle <> bsSizeToolWin)
  then
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
  Details := StyleServices.GetElementDetails(Detail);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    StyleServices.DrawElement(Canvas.Handle, Details, DrawRect);

  { draw Bottom border }
  if (Form.BorderStyle <> bsToolWindow) and (Form.BorderStyle <> bsSizeToolWin)
  then
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
  Details := StyleServices.GetElementDetails(Detail);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    StyleServices.DrawElement(Canvas.Handle, Details, DrawRect);
end;

{ TFormStyleHookBackround.TSettings }

constructor TFormStyleHookBackground.TSettings.Create;
begin
  inherited;
  FUseAlpha := False;
  FAlphaValue := 200;
  FEnabled := False;
  FBitmap := TBitmap.Create;
  ImageLocation := '';
  UseImage := False;
end;

destructor TFormStyleHookBackground.TSettings.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TFormStyleHookBackground.TSettings.SetColor(const Value: TColor);
begin
  if Value <> FColor then
    FColor := Value;
end;

procedure TFormStyleHookBackground.TSettings.SetImageLocation
  (const Value: string);
var
  Picture: TPicture;
begin
  FImageLocation := Value;
  if FileExists(Value) then
  begin
    Picture := TPicture.Create;
    try
      Picture.LoadFromFile(Value);
      FBitmap.Width := Picture.Width;
      FBitmap.Height := Picture.Height;
      FBitmap.Canvas.Draw(0, 0, Picture.Graphic);
    finally
      Picture.Free;
    end;
  end;
end;

procedure TFormStyleHookBackground.TSettings.SetUseColor(const Value: Boolean);
begin
  FUseColor := Value;
  FUseImage := not Value;
end;

procedure TFormStyleHookBackground.TSettings.SetUseImage(const Value: Boolean);
begin
  FUseImage := Value;
  FUseColor := not Value;
end;

{ TFormStyleHookBackround }

class constructor TFormStyleHookBackground.Create;
begin
  FMergeImages := False;
  FSharedBitMap := TBitmap.Create;
  FNCSettings := TFormStyleHookBackground.TSettings.Create;
  FBackGroundSettings := TFormStyleHookBackground.TSettings.Create;
end;

class destructor TFormStyleHookBackground.Destroy;
begin
  FreeAndNil(FSharedBitMap);
  FreeAndNil(FNCSettings);
  FreeAndNil(FBackGroundSettings);
end;

class procedure TFormStyleHookBackground.SetSharedImageLocation
  (const Value: string);
var
  Picture: TPicture;
begin
  FSharedImageLocation := Value;
  if FileExists(Value) then
  begin
    Picture := TPicture.Create;
    try
      Picture.LoadFromFile(Value);
      FSharedBitMap.Width := Picture.Width;
      FSharedBitMap.Height := Picture.Height;
      FSharedBitMap.Canvas.Draw(0, 0, Picture.Graphic);
    finally
      Picture.Free;
    end;
  end;
end;

procedure TFormStyleHookBackground.PaintBackground(Canvas: TCanvas);
var
  LRect: TRect;
  RBitmap: TRect;
  L, H: Integer;
begin
  // if the option is not enabled use the default inherited PaintBackground method
  if not BackGroundSettings.Enabled then
    inherited
  else
  begin
    // get he bounds of the control (form)
    LRect := Rect(0, 0, Control.ClientWidth, Control.ClientHeight);
    // use a custom color for the background?
    if BackGroundSettings.UseColor then
    begin
      Canvas.Brush.Color := BackGroundSettings.Color;
      Canvas.FillRect(LRect);
    end
    else
    // use a bitmap
    begin
      // check the size of the bitmap against the control bounds to detrine how the bitmap is drawn
      if not FMergeImages and ((BackGroundSettings.Bitmap.Width < LRect.Width)
        or (BackGroundSettings.Bitmap.Height < LRect.Height)) then
      begin
        Canvas.Brush.Bitmap := BackGroundSettings.Bitmap;
        Canvas.FillRect(LRect);
      end
      else
      begin
        // check if the the background bitmap must be merged with non client area bitmap
        if not FMergeImages then
          Canvas.CopyRect(LRect, BackGroundSettings.Bitmap.Canvas, LRect)
        else
        begin
          RBitmap := LRect;
          H := _GetBorderSize.Top;
          L := _GetBorderSize.Left;
          RBitmap.SetLocation(L, H);
          // Canvas.CopyRect(LRect,BackGroundSettings.Bitmap.Canvas,RBitmap);
          Canvas.CopyRect(LRect, FSharedBitMap.Canvas, RBitmap);
        end;
      end;
    end;
  end;
end;

procedure TFormStyleHookBackground.PaintNC(Canvas: TCanvas);
var
  LDetail: TThemedWindow;
  LDetails, CaptionDetails, IconDetails: TThemedElementDetails;
  R, R1, DrawRect, ButtonRect, TextRect: TRect;
  LBitmap: TBitmap;
  FButtonState: TThemedWindow;
  TextFormat: TTextFormat;
  LText: string;
  SrcBackRect: TRect;

  pblend: TBlendFunction;
  LBitmapPos: TPoint;
  LBitmapSize: TSize;
  LExStyle: DWORD;
{$IF CompilerVersion>23}
  TextTopOffset: Integer;
{$IFEND}
  function GetTopOffset: Integer;
  var
    P: TPoint;
  begin
    P.x := Form.Left + Form.Width div 2;
    P.y := Form.Top + Form.Height div 2;
    Result := Screen.MonitorFromPoint(P).WorkareaRect.Top;
    if Form.Top < Result then
      Result := Result - Form.Top
    else
      Result := 0;
  end;

  procedure CorrectLeftButtonRect(var AButtonRect: TRect);
  var
    TopOffset, LeftOffset: Integer;
    BS: TRect;
  begin
    if (Form.WindowState = wsMaximized) and
      (TCustomFormClass(Form).FormStyle <> fsMDIChild) and (ButtonRect.Width > 0)
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

  procedure CorrectRightButtonRect(var AButtonRect: TRect);
  var
    TopOffset, RightOffset: Integer;
    BS: TRect;
  begin
    if (Form.WindowState = wsMaximized) and
      (TCustomFormClass(Form).FormStyle <> fsMDIChild) and (ButtonRect.Width > 0)
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

begin
  // if the setting is not enabled use the original PaintNC method
  if not NCSettings.Enabled then
  begin
    inherited;
    Exit;
  end;

  // check the border style of the form
  if Form.BorderStyle = bsNone then
  begin
    MainMenuBarHookPaint(Canvas);
    Exit;
  end;

  { init some parameters }
  _FCloseButtonRect := Rect(0, 0, 0, 0);
  _FMaxButtonRect := Rect(0, 0, 0, 0);
  _FMinButtonRect := Rect(0, 0, 0, 0);
  _FHelpButtonRect := Rect(0, 0, 0, 0);
  _FSysMenuButtonRect := Rect(0, 0, 0, 0);
  _FCaptionRect := Rect(0, 0, 0, 0);

  if not StyleServices.Available then
    Exit;
  R := _GetBorderSize;

  { draw caption }
  if (Form.BorderStyle <> bsToolWindow) and (Form.BorderStyle <> bsSizeToolWin)
  then
  begin
    if _FFormActive then
      LDetail := twCaptionActive
    else
      LDetail := twCaptionInActive
  end
  else
  begin
    if _FFormActive then
      LDetail := twSmallCaptionActive
    else
      LDetail := twSmallCaptionInActive
  end;

  LBitmap := TBitmap.Create;
  if FNCSettings.UseAlpha then
    LBitmap.SetSize(_FWidth, _FHeight)
  else
    LBitmap.SetSize(_FWidth, R.Top);

  { draw caption border }
  DrawRect := Rect(0, 0, LBitmap.Width, LBitmap.Height);
  LDetails := StyleServices.GetElementDetails(LDetail);
  // used for draw text in the caption
{$IF CompilerVersion>23}
  TextTopOffset := 3;
{$IFEND}
  // check if a must use a custom color or a bitmap
  if FNCSettings.UseColor then
  begin
    if FNCSettings.UseAlpha then
    begin
      LExStyle := GetWindowLongA(Handle, GWL_EXSTYLE);
      if (LExStyle and WS_EX_LAYERED = 0) then
        SetWindowLong(Handle, GWL_EXSTYLE, LExStyle or WS_EX_LAYERED);

      LBitmap.PixelFormat := pf32bit;
      LBitmap.Canvas.Brush.Color := FNCSettings.Color;
      LBitmap.Canvas.FillRect(DrawRect);
      LBitmapPos := Point(0, 0);
      LBitmapSize.cx := LBitmap.Width;
      LBitmapSize.cy := LBitmap.Height;
      pblend.BlendOp := AC_SRC_OVER;
      pblend.BlendFlags := 0;
      pblend.SourceConstantAlpha := FNCSettings.AlphaValue;
      pblend.AlphaFormat := 0;
    end
    else
    begin
      // use the select color to fill the background of the canvas
      LBitmap.Canvas.Brush.Color := FNCSettings.Color;
      LBitmap.Canvas.FillRect(DrawRect);
    end;

  end
  else
  begin
    // use the bitmap to fill the canvas
    SrcBackRect.Left := 0;
    SrcBackRect.Top := 0;
    SrcBackRect.Width := DrawRect.Width;
    SrcBackRect.Height := DrawRect.Height;
    // SrcBackRect.SetLocation(FNCSettings.Bitmap.Width-DrawRect.Width, 0);
    // SrcBackRect.SetLocation(_GetBorderSize.Width, 0);
    if not FMergeImages then
      LBitmap.Canvas.CopyRect(DrawRect, FNCSettings.Bitmap.Canvas, SrcBackRect)
    else
      LBitmap.Canvas.CopyRect(DrawRect, FSharedBitMap.Canvas, SrcBackRect)
  end;

  TextRect := DrawRect;
  CaptionDetails := LDetails;

  { draw icon }
  if (biSystemMenu in TCustomFormClass(Form).BorderIcons) and
    (Form.BorderStyle <> bsDialog) and (Form.BorderStyle <> bsToolWindow) and
    (Form.BorderStyle <> bsSizeToolWin) then
  begin
    IconDetails := StyleServices.GetElementDetails(twSysButtonNormal);
    if not StyleServices.GetElementContentRect(0, IconDetails, DrawRect,
      ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);

{$IF CompilerVersion > 23.0}
    R1 := ButtonRect;
    if not StyleServices.HasElementFixedPosition(LDetails) then
    begin
      CorrectLeftButtonRect(ButtonRect);
      TextTopOffset := Abs(R1.Top - ButtonRect.Top);
      if TextTopOffset > R.Top then
        TextTopOffset := 3;
    end
    else
      TextTopOffset := 0;
{$IFEND}
    R1 := Rect(0, 0, GetSystemMetrics(SM_CXSMICON),
      GetSystemMetrics(SM_CYSMICON));
    RectVCenter(R1, ButtonRect);
    if ButtonRect.Width > 0 then
      DrawIconEx(LBitmap.Canvas.Handle, R1.Left, R1.Top, _GetIconFast.Handle, 0,
        0, 0, 0, DI_NORMAL);
    Inc(TextRect.Left, ButtonRect.Width + 5);
    _FSysMenuButtonRect := ButtonRect;
  end
  else
    Inc(TextRect.Left, R.Left);

  { draw buttons }
  if (biSystemMenu in TCustomFormClass(Form).BorderIcons) then
  begin
    if (Form.BorderStyle <> bsToolWindow) and (Form.BorderStyle <> bsSizeToolWin)
    then
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

    LDetails := StyleServices.GetElementDetails(FButtonState);
    if not StyleServices.GetElementContentRect(0, LDetails, DrawRect, ButtonRect)
    then
      ButtonRect := Rect(0, 0, 0, 0);

{$IF CompilerVersion > 23.0}
    if not StyleServices.HasElementFixedPosition(LDetails) then
      CorrectRightButtonRect(ButtonRect);
{$IFEND}
    StyleServices.DrawElement(LBitmap.Canvas.Handle, LDetails, ButtonRect);

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    _FCloseButtonRect := ButtonRect;
  end;

  if (biMaximize in TCustomFormClass(Form).BorderIcons) and
    (biSystemMenu in TCustomFormClass(Form).BorderIcons) and
    (Form.BorderStyle <> bsDialog) and (Form.BorderStyle <> bsToolWindow) and
    (Form.BorderStyle <> bsSizeToolWin) then
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
    LDetails := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, LDetails, DrawRect, ButtonRect)
    then
      ButtonRect := Rect(0, 0, 0, 0);

{$IF CompilerVersion > 23.0}
    if not StyleServices.HasElementFixedPosition(LDetails) then
      CorrectRightButtonRect(ButtonRect);
{$IFEND}
    if ButtonRect.Width > 0 then
      StyleServices.DrawElement(LBitmap.Canvas.Handle, LDetails, ButtonRect);
    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    _FMaxButtonRect := ButtonRect;
  end;

  if (biMinimize in TCustomFormClass(Form).BorderIcons) and
    (biSystemMenu in TCustomFormClass(Form).BorderIcons) and
    (Form.BorderStyle <> bsDialog) and (Form.BorderStyle <> bsToolWindow) and
    (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if (_FPressedButton = HTMINBUTTON) and (_FHotButton = HTMINBUTTON) then
      FButtonState := twMinButtonPushed
    else if _FHotButton = HTMINBUTTON then
      FButtonState := twMinButtonHot
    else if _FFormActive then
      FButtonState := twMinButtonNormal
    else
      FButtonState := twMinButtonDisabled;

    LDetails := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, LDetails, DrawRect, ButtonRect)
    then
      ButtonRect := Rect(0, 0, 0, 0);

{$IF CompilerVersion > 23.0}
    if not StyleServices.HasElementFixedPosition(LDetails) then
      CorrectRightButtonRect(ButtonRect);
{$IFEND}
    if ButtonRect.Width > 0 then
      StyleServices.DrawElement(LBitmap.Canvas.Handle, LDetails, ButtonRect);
    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    _FMinButtonRect := ButtonRect;
  end;

  if (biHelp in TCustomFormClass(Form).BorderIcons) and
    (biSystemMenu in TCustomFormClass(Form).BorderIcons) and
    ((not(biMaximize in TCustomFormClass(Form).BorderIcons) and
    not(biMinimize in TCustomFormClass(Form).BorderIcons)) or
    (Form.BorderStyle = bsDialog)) then
  begin
    if (_FPressedButton = HTHELP) and (_FHotButton = HTHELP) then
      FButtonState := twHelpButtonPushed
    else if _FHotButton = HTHELP then
      FButtonState := twHelpButtonHot
    else if _FFormActive then
      FButtonState := twHelpButtonNormal
    else
      FButtonState := twHelpButtonDisabled;
    LDetails := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, LDetails, DrawRect, ButtonRect)
    then
      ButtonRect := Rect(0, 0, 0, 0);

{$IF CompilerVersion > 23.0}
    if not StyleServices.HasElementFixedPosition(LDetails) then
      CorrectRightButtonRect(ButtonRect);
{$IFEND}
    if ButtonRect.Width > 0 then
      StyleServices.DrawElement(LBitmap.Canvas.Handle, LDetails, ButtonRect);

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    _FHelpButtonRect := ButtonRect;
  end;

  { draw text }
  TextFormat := [tfLeft, tfSingleLine, tfVerticalCenter];
  if Control.UseRightToLeftReading then
    Include(TextFormat, tfRtlReading);

  LText := Text;

{$IF CompilerVersion > 23.0}
  if (Form.WindowState = wsMaximized) and
    (TCustomFormClass(Form).FormStyle <> fsMDIChild) and (TextTopOffset <> 0)
    and (biSystemMenu in TCustomFormClass(Form).BorderIcons) then
  begin
    Inc(TextRect.Left, R.Left);
    MoveWindowOrg(LBitmap.Canvas.Handle, 0, TextTopOffset);
    StyleServices.DrawText(LBitmap.Canvas.Handle, CaptionDetails, LText,
      TextRect, TextFormat);
    MoveWindowOrg(LBitmap.Canvas.Handle, 0, -TextTopOffset);
  end
  else
{$IFEND}
    StyleServices.DrawText(LBitmap.Canvas.Handle, CaptionDetails, LText,
      TextRect, TextFormat);
  _FCaptionRect := TextRect;

  { draw caption buffer }

  if FNCSettings.UseAlpha then
    UpdateLayeredWindow(Handle, 0, nil, @LBitmapSize, LBitmap.Canvas.Handle,
      @LBitmapPos, 0, @pblend, ULW_ALPHA)
  else
    Canvas.Draw(0, 0, LBitmap);

  // LBitmap.Free;
  { draw menubar }
  MainMenuBarHookPaint(Canvas); // doesn't suport alpha for now :(

  { draw left border }
  DrawRect := Rect(0, R.Top, R.Left, _FHeight - R.Bottom);
  if DrawRect.Bottom - DrawRect.Top > 0 then
    // use a color?
    if FNCSettings.UseColor then
    begin
      if FNCSettings.UseAlpha then
      begin
        LBitmap.Canvas.Brush.Color := FNCSettings.Color;
        LBitmap.Canvas.FillRect(DrawRect);
      end
      else
      Begin
        Canvas.Brush.Color := FNCSettings.Color;
        Canvas.FillRect(DrawRect);
      End;
    end
    else
    begin
      if FMergeImages then
        if (DrawRect.Height <= FSharedBitMap.Height) and
          (DrawRect.Width <= FSharedBitMap.Width) then
          Canvas.CopyRect(DrawRect, FSharedBitMap.Canvas, DrawRect)
        else
          Canvas.StretchDraw(DrawRect, FSharedBitMap)
      else if (DrawRect.Height <= FNCSettings.Bitmap.Height) and
        (DrawRect.Width <= FNCSettings.Bitmap.Width) then
        Canvas.CopyRect(DrawRect, FNCSettings.Bitmap.Canvas, DrawRect)
      else
        Canvas.StretchDraw(DrawRect, FNCSettings.Bitmap);

    end;

  { draw right border }
  DrawRect := Rect(_FWidth - R.Right, R.Top, _FWidth, _FHeight - R.Bottom);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    // use a color?
    if FNCSettings.UseColor then
    begin
      Canvas.Brush.Color := FNCSettings.Color;
      Canvas.FillRect(DrawRect);
    end
    else
    begin
      if FMergeImages then
        if (DrawRect.Height <= FSharedBitMap.Height) and
          (Control.Width <= FSharedBitMap.Width) then
          Canvas.CopyRect(DrawRect, FSharedBitMap.Canvas, DrawRect)
        else
          Canvas.StretchDraw(DrawRect, FSharedBitMap)
      else if (DrawRect.Height <= FNCSettings.Bitmap.Height) and
        (Control.Width <= FNCSettings.Bitmap.Width) then
        Canvas.CopyRect(DrawRect, FNCSettings.Bitmap.Canvas, DrawRect)
      else
        Canvas.StretchDraw(DrawRect, FNCSettings.Bitmap);
    end;

  { draw Bottom border }
  DrawRect := Rect(0, _FHeight - R.Bottom, _FWidth, _FHeight);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    // use a color?
    if FNCSettings.UseColor then
    begin
      Canvas.Brush.Color := FNCSettings.Color;
      Canvas.FillRect(DrawRect);
    end
    else
    begin
      if FMergeImages then
        if (DrawRect.Height <= FSharedBitMap.Height) and
          (Control.Width <= FSharedBitMap.Width) then
          Canvas.CopyRect(DrawRect, FSharedBitMap.Canvas, DrawRect)
        else
        begin
          SrcBackRect.Left := 0;
          SrcBackRect.Top := 0;
          SrcBackRect.Width := DrawRect.Width;
          SrcBackRect.Height := DrawRect.Height;
          SrcBackRect.SetLocation(FSharedBitMap.Width - DrawRect.Width, 0);
          Canvas.CopyRect(DrawRect, FSharedBitMap.Canvas, SrcBackRect);
        end
      else if (DrawRect.Height <= FNCSettings.Bitmap.Height) and
        (Control.Width <= FNCSettings.Bitmap.Width) then
        Canvas.CopyRect(DrawRect, FNCSettings.Bitmap.Canvas, DrawRect)
      else
      begin
        SrcBackRect.Left := 0;
        SrcBackRect.Top := 0;
        SrcBackRect.Width := DrawRect.Width;
        SrcBackRect.Height := DrawRect.Height;
        SrcBackRect.SetLocation(FNCSettings.Bitmap.Width - DrawRect.Width, 0);
        Canvas.CopyRect(DrawRect, FNCSettings.Bitmap.Canvas, SrcBackRect);
      end;
    end;

  LBitmap.Free;
end;

{ TFormStyleHookNC }

constructor TFormStyleHookNC.Create(AControl: TWinControl);
begin
  inherited;
  OverrideEraseBkgnd := True;
end;

procedure TFormStyleHookNC.PaintBackground(Canvas: TCanvas);
var
  Details: TThemedElementDetails;
  R: TRect;
begin
  if StyleServices.Available then
  begin
    Details.Element := teWindow;
    Details.Part := 0;
    R := Rect(0, 0, Control.ClientWidth, Control.ClientHeight);
    StyleServices.DrawElement(Canvas.Handle, Details, R);
  end;
end;

{ TFormStyleHookHelper }

function TFormStyleHookHelper.GetFCaptionRect: TRect;
begin
  Result := Self.FCaptionRect;
end;

function TFormStyleHookHelper.GetFChangeSizeCalled: Boolean;
begin
  Result := Self.FChangeSizeCalled;
end;

function TFormStyleHookHelper.GetFCloseButtonRect: TRect;
begin
  Result := Self.FCloseButtonRect;
end;

function TFormStyleHookHelper.GetFFormActive: Boolean;
begin
  Result := Self.FFormActive;
end;

function TFormStyleHookHelper.GetFHeight: Integer;
begin
  Result := Self.FHeight;
end;

function TFormStyleHookHelper.GetFHelpButtonRect: TRect;
begin
  Result := Self.FHelpButtonRect;
end;

function TFormStyleHookHelper.GetFHotButton: Integer;
begin
  Result := Self.FHotButton;
end;

function TFormStyleHookHelper.GetFLeft: Integer;
begin
  Result := Self.FLeft;
end;

function TFormStyleHookHelper.GetFMaxButtonRect: TRect;
begin
  Result := Self.FMaxButtonRect;
end;

function TFormStyleHookHelper.GetFMinButtonRect: TRect;
begin
  Result := Self.FMinButtonRect;
end;

function TFormStyleHookHelper.GetForm: TCustomForm;
begin
  Result := Self.Form;
end;

function TFormStyleHookHelper.GetFPressedButton: Integer;
begin
  Result := Self.FPressedButton;
end;

function TFormStyleHookHelper.GetFRegion: HRGN;
begin
  Result := Self.FRegion;
end;

function TFormStyleHookHelper.GetFSysMenuButtonRect: TRect;
begin
  Result := Self.FSysMenuButtonRect;
end;

function TFormStyleHookHelper.GetFTop: Integer;
begin
  Result := Self.FTop;
end;

function TFormStyleHookHelper.GetFWidth: Integer;
begin
  Result := Self.FWidth;
end;

procedure TFormStyleHookHelper.MainMenuBarHookPaint(Canvas: TCanvas);
begin
  if Self.FMainMenuBarHook <> nil then
    Self.FMainMenuBarHook.Paint(Canvas);
end;

procedure TFormStyleHookHelper.SetFCaptionRect(const Value: TRect);
begin
  Self.FCaptionRect := Value;
end;

procedure TFormStyleHookHelper.SetFChangeSizeCalled(const Value: Boolean);
begin
  Self.FChangeSizeCalled := Value;
end;

procedure TFormStyleHookHelper.SetFCloseButtonRect(const Value: TRect);
begin
  Self.FCloseButtonRect := Value;
end;

procedure TFormStyleHookHelper.SetFHeight(const Value: Integer);
begin
  Self.FHeight := Value;
end;

procedure TFormStyleHookHelper.SetFHelpButtonRect(const Value: TRect);
begin
  Self.FHelpButtonRect := Value;
end;

procedure TFormStyleHookHelper.SetFLeft(const Value: Integer);
begin
  Self.FLeft := Value;
end;

procedure TFormStyleHookHelper.SetFMaxButtonRect(const Value: TRect);
begin
  Self.FMaxButtonRect := Value;
end;

procedure TFormStyleHookHelper.SetFMinButtonRect(const Value: TRect);
begin
  Self.FMinButtonRect := Value;
end;

procedure TFormStyleHookHelper.SetFRegion(const Value: HRGN);
begin
  Self.FRegion := Value;
end;

procedure TFormStyleHookHelper.SetFSysMenuButtonRect(const Value: TRect);
begin
  Self.FSysMenuButtonRect := Value;
end;

procedure TFormStyleHookHelper.SetFTop(const Value: Integer);
begin
  Self.FTop := Value;
end;

procedure TFormStyleHookHelper.SetFWidth(const Value: Integer);
begin
  Self.FWidth := Value;
end;

procedure TFormStyleHookHelper._ChangeSize;
begin
  Self.ChangeSize;
end;

function TFormStyleHookHelper._GetBorderSize: TRect;
begin
 Result := Self.GetBorderSize;
end;

function TFormStyleHookHelper._GetBorderSizeAddr: Pointer;
var
  MethodAddr: function: TRect of object;
//  LItem : TRttiType;
//  LMethod : TRttiMethod;
//  InvalidateAddr :  procedure of object;
//  p : Pointer;
begin
//  {$IF (CompilerVersion >= 31)}
//  LItem:= TRttiContext.Create.GetType(TFormStyleHook);
//  LMethod := LItem.GetMethod('Invalidate');
//  p := (pbyte(LMethod.CodeAddress) + 3945);
//  Exit(p);
//  {$ELSE}
  MethodAddr := Self.GetBorderSize;
//  {$IFEND}
  Result := TMethod(MethodAddr).Code;
end;


function TFormStyleHookHelper._GetRegionAddr: Pointer;
var
  MethodAddr: function: HRGN of object;
begin
  MethodAddr := Self.GetRegion;
  Result := TMethod(MethodAddr).Code;
end;

function TFormStyleHookHelper._GetHitTest(P: TPoint): Integer;
begin
  Result := Self.GetHitTest(P);
end;

function TFormStyleHookHelper._GetIconFast: TIcon;
begin
  Result := Self.GetIconFast;
end;

function TFormStyleHookHelper._NormalizePoint(P: TPoint): TPoint;
begin
  Result := Self.NormalizePoint(P);
end;


end.
