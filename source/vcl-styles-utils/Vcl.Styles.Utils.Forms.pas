// **************************************************************************************************
//
// Unit Vcl.Styles.Utils.Forms
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
//
// Portions created by Mahdi Safsafi [SMP3]   e-mail SMP@LIVE.FR
// Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2019 Rodrigo Ruz V.
// All Rights Reserved.
//
// **************************************************************************************************
unit Vcl.Styles.Utils.Forms;


{$I VCL.Styles.Utils.inc}

interface

uses
  System.Classes,
  System.Types,
  System.SysUtils,
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Styles,
  Vcl.Themes,
  Vcl.Dialogs,
  Vcl.Graphics,
  Vcl.Styles.Utils.SysStyleHook,
  {$IFDEF USE_Vcl.Styles.Hooks}
  Vcl.Styles.Hooks,
  {$ENDIF}
  Vcl.Forms,
  Vcl.GraphUtil,
  Vcl.ExtCtrls,
  Vcl.Controls;

type
  TSysScrollingType = (skNone, skTracking, skLineUp, skLineDown, skLineLeft, skLineRight, skPageUp, skPageDown, skPageLeft, skPageRight);

  TSysScrollingStyleHook = class(TMouseTrackSysControlStyleHook)
  private
    FVertScrollBar: Boolean;
    FHorzScrollBar: Boolean;
    FTrackTimer: TTimer;
    FPrevPoint: TPoint;
    FPrevPos: Integer;
    FDownDis: Integer;
    FDownPoint: TPoint;
    FTrackingPos: Integer;
    FTrackingRect: TRect;
    FTracking: Boolean;
    FScrollingType: TSysScrollingType;
    FScrollKind: TScrollBarKind;
    FBtnUpDetail: TThemedScrollBar;
    FBtnDownDetail: TThemedScrollBar;
    FVertBtnSliderDetail: TThemedScrollBar;
    FBtnLeftDetail: TThemedScrollBar;
    FBtnRightDetail: TThemedScrollBar;
    FHorzBtnSliderDetail: TThemedScrollBar;
    FNCMouseDown: Boolean;
    FAllowScrolling: Boolean;
    FLstPos: Integer;
    function GetDefaultScrollBarSize: TSize;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMNCLButtonUp(var Message: TWMNCLButtonUp); message WM_NCLBUTTONUP;
    procedure CMSCROLLTRACKING(var Message: TMessage); message CM_SCROLLTRACKING;
    function GetVertScrollRect: TRect;
    function GetVertUpRect: TRect;
    function GetVertDownRect: TRect;
    function GetVertSliderRect: TRect;
    function GetVertSliderPos: Integer;
    function GetVertThumbSize: Integer;
    function GetVertTrackRect: TRect;
    function GetVertScrollInfo: TScrollInfo;
    function GetVertThumbPosFromPos(const Pos: Integer): Integer;
    function GetVertScrollPosFromPoint(const P: TPoint): Integer;
    function GetHorzThumbPosFromPos(const Pos: Integer): Integer;
    function GetHorzScrollPosFromPoint(const P: TPoint): Integer;
    function GetHorzSliderPos: Integer;
    function GetHorzThumbSize: Integer;
    function GetHorzLeftRect: TRect;
    function GetHorzScrollInfo: TScrollInfo;
    function GetHorzSliderRect: TRect;
    function GetHorzTrackRect: TRect;
    function GetHorzRightRect: TRect;
    function GetHorzScrollRect: TRect;
    function IsLeftScrollBar: Boolean;
    function IsHorzScrollDisabled: Boolean;
    function IsVertScrollDisabled: Boolean;
  protected
    property LstPos : Integer read FLstPos write FLstPos;
    property AllowScrolling : Boolean read FAllowScrolling write FAllowScrolling;
    function NormalizePoint(const P: TPoint): TPoint;
    procedure Scroll(const Kind: TScrollBarKind; const ScrollType: TSysScrollingType; Pos, Delta: Integer); virtual;
    procedure DoScroll(const Kind: TScrollBarKind; const ScrollType: TSysScrollingType; Pos, Delta: Integer);
    procedure DrawHorzScroll(DC: HDC); virtual;
    procedure DrawVertScroll(DC: HDC); virtual;
    procedure DrawSmallRect(DC: HDC; const SmallRect: TRect); virtual;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure StartSliderTrackTimer;
    procedure StopSliderTrackTimer;
    procedure DoSliderTrackTimer(Sender: TObject);
    procedure StartPageTrackTimer;
    procedure StopPageTrackTimer;
    procedure DoPageTrackTimer(Sender: TObject);
    procedure StartLineTrackTimer;
    procedure StopLineTrackTimer;
    procedure DoLineTrackTimer(Sender: TObject);
    procedure InitScrollState;
    procedure PaintNC(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
    property VertScrollRect: TRect read GetVertScrollRect;
    property VertUpRect: TRect read GetVertUpRect;
    property VertDownRect: TRect read GetVertDownRect;
    property VertSliderRect: TRect read GetVertSliderRect;
    property VertTrackRect: TRect read GetVertTrackRect;
    property VertScrollInfo: TScrollInfo read GetVertScrollInfo;
    property HorzScrollRect: TRect read GetHorzScrollRect;
    property HorzLeftRect: TRect read GetHorzLeftRect;
    property HorzRightRect: TRect read GetHorzRightRect;
    property HorzSliderRect: TRect read GetHorzSliderRect;
    property HorzTrackRect: TRect read GetHorzTrackRect;
    property HorzScrollInfo: TScrollInfo read GetHorzScrollInfo;
    property BtnSize: TSize read GetDefaultScrollBarSize;
    property Tracking: Boolean read FTracking;
    property TrackingRect: TRect read FTrackingRect;
    property TrackingPos: Integer read FTrackingPos;
    property LeftScrollBar: Boolean read IsLeftScrollBar;
    property VertScrollDisabled: Boolean read IsVertScrollDisabled;
    property HorzScrollDisabled: Boolean read IsHorzScrollDisabled;
  end;

  TSysDialogStyleHook = class(TSysScrollingStyleHook)
  private
    FFrameActive: Boolean;
    FPressedButton: Integer;
    FHotButton: Integer;
    FIcon: TIcon;
    FIconHandle: HICON;
    FCaptionRect: TRect;
    FSysMenuButtonRect: TRect;
    FRegion: HRGN;
    // FUpdateRegion: Boolean;
    FSysCloseButtonDisabled: Boolean;
    procedure WMPaint(var Message: TMessage); message WM_PAINT;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMNCLButtonUp(var Message: TWMNCLButtonUp); message WM_NCLBUTTONUP;
    procedure WMNCMouseMove(var Message: TWMNCHitMessage); message WM_NCMOUSEMOVE;
    procedure WMNCACTIVATE(var Message: TWMNCActivate); message WM_NCACTIVATE;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMSIZE(var Message: TWMSize); message WM_SIZE;
    procedure WMSetText(var Message: TMessage); message WM_SETTEXT;
    function GetCaptionRect: TRect;
    function GetBorderStyle: TFormBorderStyle;
    function GetBorderIcons: TBorderIcons;
    function GetCloseButtonRect: TRect;
    function GetMaxButtonRect: TRect;
    function GetMinButtonRect: TRect;
    function GetHelpButtonRect: TRect;
    function GetSysMenuButtonRect: TRect;
    function GetWindowState: TWindowState;
    function UseSmallBorder: Boolean;
    function GetRegion: HRGN;
    function GetIcon: TIcon;
    function GetIconFast: TIcon;
    function NormalizePoint(const P: TPoint): TPoint;
    function GetHitTest(const P: TPoint): Integer;
    function IsSysCloseButtonDisabled: Boolean;
    function GetSysMenu: HMENU;
    function GetUpdateRegion: Boolean;
  protected
    procedure DrawBorder(Canvas: TCanvas); override;
    function GetBorderSize: TRect; override;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure Paint(Canvas: TCanvas); override;
    procedure PaintNC(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
    procedure Close; virtual;
    procedure Help; virtual;
    procedure Maximize; virtual;
    procedure Minimize; virtual;
    procedure Restore; virtual;
    property  PressedButton: Integer read FPressedButton write FPressedButton;
    property  HotButton: Integer read FHotButton write FHotButton;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
    property CaptionRect: TRect read GetCaptionRect;
    property UpdateRegion: Boolean read GetUpdateRegion;
    property BorderStyle: TFormBorderStyle read GetBorderStyle;
    property BorderSize: TRect read GetBorderSize;
    property BorderIcons: TBorderIcons read GetBorderIcons;
    Property WindowState: TWindowState read GetWindowState;
    Property CloseButtonRect: TRect read GetCloseButtonRect;
    Property MaxButtonRect: TRect read GetMaxButtonRect;
    Property MinButtonRect: TRect read GetMinButtonRect;
    Property HelpButtonRect: TRect read GetHelpButtonRect;
    property SysMenuButtonRect: TRect read GetSysMenuButtonRect;
    property Icon: TIcon read GetIconFast;
    property SysMenu: HMENU read GetSysMenu;
    property SysCloseButtonDisabled: Boolean read FSysCloseButtonDisabled;
  end;

  {
    Note: The development of this class is not finished yet .
    Only ScrollBar with SIZEBOX is supported !!.
  }
  TSysScrollBarStyleHook = class(TSysStyleHook)
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
  end;

implementation

uses
  Vcl.Styles.Utils.Misc,
  Vcl.Styles.Utils.SysControls;

// -----------------------------------------------------------------------------------
procedure FillDC(const DC: HDC; const R: TRect; const Color: TColor);
var
  Brush: HBRUSH;
begin
  Brush := CreateSolidBrush(Color);
  FillRect(DC, R, Brush);
  DeleteObject(Brush);
end;

function IsItemDisabled(const Menu: HMENU; const Index: Integer): Boolean;
var
  Info: TMenuItemInfo;
begin
  Result := False;
  if (Menu = 0) or (Index < 0) then
    Exit;

  FillChar(Info, sizeof(Info), Char(0));
  Info.cbSize := sizeof(TMenuItemInfo);
  Info.fMask := MIIM_STATE;
  GetMenuItemInfo(Menu, Index, True, Info);
  Result := (Info.fState and MFS_DISABLED = MFS_DISABLED) or (Info.fState and MF_DISABLED = MF_DISABLED) or (Info.fState and MF_GRAYED = MF_GRAYED);
end;

function GetMenuItemPos(const Menu: HMENU; const ID: Integer): Integer;
var
  i: Integer;
  mii: MENUITEMINFO;
begin
  Result := -1;
  if Menu = 0 then
    Exit;
  for i := 0 to GetMenuItemCount(Menu) do
  begin
    FillChar(mii, sizeof(mii), Char(0));
    mii.cbSize := sizeof(mii);
    mii.fMask := MIIM_ID;
    if (GetMenuItemInfo(Menu, i, True, mii)) then
      if mii.wID = Cardinal(ID) then
        Exit(i);
  end;
end;

function IsWindowMsgBox(Handle: HWND): Boolean;
begin
  Result := ((FindWindowEx(Handle, 0, 'Edit', nil) = 0) and (GetDlgItem(Handle, $FFFF) <> 0)) and (GetWindowLongPtr(Handle, GWL_USERDATA) <> 0);
end;

// -----------------------------------------------------------------------------------------
{ TSysDialogStyleHook }

procedure TSysDialogStyleHook.Close;
begin
  if (Handle <> 0) and not(FSysCloseButtonDisabled) then
    SendMessage(Handle, WM_SYSCOMMAND, SC_CLOSE, 0);
end;

constructor TSysDialogStyleHook.Create(AHandle: THandle);
begin
  inherited;
  FRegion := 0;
{$IF CompilerVersion > 23}
  StyleElements := [seFont, seClient, seBorder];
{$ELSE}
  OverridePaint := True;
  OverridePaintNC := True;
  OverrideFont := True;
{$IFEND}
  OverrideEraseBkgnd := True;
  FPressedButton := 0;
  FHotButton := 0;
  FIconHandle := 0;
  FIcon := nil;
  FSysMenuButtonRect := Rect(0, 0, 0, 0);
end;

destructor TSysDialogStyleHook.Destroy;
begin
  if FRegion <> 0 then
    DeleteObject(FRegion);
  if Assigned(FIcon) then
    FreeAndNil(FIcon);
  inherited;
end;

procedure TSysDialogStyleHook.DrawBorder(Canvas: TCanvas);
begin
  //
end;

function TSysDialogStyleHook.GetCaptionRect: TRect;
var
  LDetails: TThemedElementDetails;
  ElementSize: TSize;
  CaptionHeight: Integer;
begin
  Result := Rect(0, 0, SysControl.Width, 0);
  if BorderStyle = bsNone then
    Exit;

  if FFrameActive then
  begin
    if not UseSmallBorder then
      LDetails := StyleServices.GetElementDetails(twCaptionActive)
    else
      LDetails := StyleServices.GetElementDetails(twSmallCaptionActive);
  end
  else
  begin
    if not UseSmallBorder then
      LDetails := StyleServices.GetElementDetails(twCaptionInActive)
    else
      LDetails := StyleServices.GetElementDetails(twSmallCaptionInActive);
  end;
  StyleServices.GetElementSize(0, LDetails, esActual, ElementSize);
  CaptionHeight := ElementSize.Height;
  Result := Rect(0, 0, SysControl.Width, CaptionHeight);

end;

function TSysDialogStyleHook.GetCloseButtonRect: TRect;
var
  FButtonState: TThemedWindow;
  LDetails: TThemedElementDetails;
begin
  Result := Rect(0, 0, 0, 0);
  if (biSystemMenu in BorderIcons) then
  begin
    if not UseSmallBorder then
    begin
      if (FPressedButton = HTCLOSE) and (FHotButton = HTCLOSE) then
        FButtonState := twCloseButtonPushed
      else if FHotButton = HTCLOSE then
        FButtonState := twCloseButtonHot
      else if FFrameActive then
        FButtonState := twCloseButtonNormal
      else
        FButtonState := twCloseButtonDisabled;
    end
    else
    begin
      if (FPressedButton = HTCLOSE) and (FHotButton = HTCLOSE) then
        FButtonState := twSmallCloseButtonPushed
      else if FHotButton = HTCLOSE then
        FButtonState := twSmallCloseButtonHot
      else if FFrameActive then
        FButtonState := twSmallCloseButtonNormal
      else
        FButtonState := twSmallCloseButtonDisabled;
    end;
    LDetails := StyleServices.GetElementDetails(FButtonState);
    if not StyleServices.GetElementContentRect(0, LDetails, CaptionRect, Result) then
      Result := Rect(0, 0, 0, 0);
  end;
end;

function TSysDialogStyleHook.GetHelpButtonRect: TRect;
var
  FButtonState: TThemedWindow;
  LDetails: TThemedElementDetails;
begin
  Result := Rect(0, 0, 0, 0);
  if (biHelp in BorderIcons) and (biSystemMenu in BorderIcons) and ((not(biMaximize in BorderIcons) and not(biMinimize in BorderIcons)) or (BorderStyle = bsDialog)) then
  begin
    if (FPressedButton = HTHELP) and (FHotButton = HTHELP) then
      FButtonState := twHelpButtonPushed
    else if FHotButton = HTHELP then
      FButtonState := twHelpButtonHot
    else if FFrameActive then
      FButtonState := twHelpButtonNormal
    else
      FButtonState := twHelpButtonDisabled;
    LDetails := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, LDetails, CaptionRect, Result) then
      Result := Rect(0, 0, 0, 0);
  end;
end;

function TSysDialogStyleHook.GetHitTest(const P: TPoint): Integer;
begin
  Result := HTCAPTION;
  if CloseButtonRect.Contains(P) then
    Result := HTCLOSE;
  if MaxButtonRect.Contains(P) then
    Result := HTMAXBUTTON;
  if MinButtonRect.Contains(P) then
    Result := HTMINBUTTON;
  if HelpButtonRect.Contains(P) then
    Result := HTHELP;

  if Result <> HTCAPTION then
  begin
    if FHotButton <> Result then
    begin
      FHotButton := Result;
      InvalidateNC;
    end;
    Exit;
  end
  else
  begin
    if FHotButton <> 0 then
    begin
      FHotButton := 0;
      InvalidateNC;
    end;
  end;
end;

function TSysDialogStyleHook.GetMaxButtonRect: TRect;
var
  FButtonState: TThemedWindow;
  LDetails: TThemedElementDetails;
begin
  Result := Rect(0, 0, 0, 0);
  if (biMaximize in BorderIcons) and (biSystemMenu in BorderIcons) and (BorderStyle <> bsDialog) and (BorderStyle <> bsToolWindow) and (BorderStyle <> bsSizeToolWin) then
  begin
    if WindowState = wsMaximized then
    begin
      if (FPressedButton = HTMAXBUTTON) and (FHotButton = HTMAXBUTTON) then
        FButtonState := twRestoreButtonPushed
      else if FHotButton = HTMAXBUTTON then
        FButtonState := twRestoreButtonHot
      else if FFrameActive then
        FButtonState := twRestoreButtonNormal
      else
        FButtonState := twRestoreButtonDisabled;
    end
    else
    begin
      if (FPressedButton = HTMAXBUTTON) and (FHotButton = HTMAXBUTTON) then
        FButtonState := twMaxButtonPushed
      else if FHotButton = HTMAXBUTTON then
        FButtonState := twMaxButtonHot
      else if FFrameActive then
        FButtonState := twMaxButtonNormal
      else
        FButtonState := twMaxButtonDisabled;
    end;
    LDetails := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, LDetails, CaptionRect, Result) then
      Result := Rect(0, 0, 0, 0);
  end;
end;

function TSysDialogStyleHook.GetMinButtonRect: TRect;
var
  FButtonState: TThemedWindow;
  LDetails: TThemedElementDetails;
begin
  Result := Rect(0, 0, 0, 0);
  if (biMinimize in BorderIcons) and (biSystemMenu in BorderIcons) and (BorderStyle <> bsDialog) and (BorderStyle <> bsToolWindow) and (BorderStyle <> bsSizeToolWin) then
  begin
    if (FPressedButton = HTMINBUTTON) and (FHotButton = HTMINBUTTON) then
      FButtonState := twMinButtonPushed
    else if FHotButton = HTMINBUTTON then
      FButtonState := twMinButtonHot
    else if FFrameActive then
      FButtonState := twMinButtonNormal
    else
      FButtonState := twMinButtonDisabled;

    LDetails := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, LDetails, CaptionRect, Result) then
      Result := Rect(0, 0, 0, 0);
  end;

end;

function TSysDialogStyleHook.GetWindowState: TWindowState;
begin
  Result := wsNormal;
  if IsZoomed(Handle) then
    Result := wsMaximized;
  if IsIconic(Handle) then
    Result := wsMinimized;
end;

procedure TSysDialogStyleHook.Help;
begin
  SendMessage(Handle, WM_SYSCOMMAND, SC_CONTEXTHELP, 0)
end;

function TSysDialogStyleHook.IsSysCloseButtonDisabled: Boolean;
var
  i, ID: Integer;
begin
  Result := True;
  if SysMenu > 0 then
  begin
    for i := 0 to GetMenuItemCount(SysMenu) - 1 do
    begin
      ID := GetMenuItemID(SysMenu, i);
      if ID = SC_CLOSE then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

procedure TSysDialogStyleHook.Maximize;
begin
  if Handle <> 0 then
  begin
    FPressedButton := 0;
    FHotButton := 0;

    if IsZoomed(Handle) then
      SendMessage(Handle, WM_SYSCOMMAND, SC_RESTORE, 0)
    else
      SendMessage(Handle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
  end;
end;

procedure TSysDialogStyleHook.Minimize;
begin
  if Handle <> 0 then
  begin
    FPressedButton := 0;
    FHotButton := 0;
    if IsIconic(Handle) then
      SendMessage(Handle, WM_SYSCOMMAND, SC_RESTORE, 0)
    else
      SendMessage(Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
  end;
end;

procedure TSysDialogStyleHook.Paint(Canvas: TCanvas);
begin
  inherited;
  PaintBackground(Canvas);
end;

procedure TSysDialogStyleHook.PaintBackground(Canvas: TCanvas);
begin
  inherited;
end;

function TSysDialogStyleHook.GetBorderIcons: TBorderIcons;
begin
  Result := [];
  with SysControl do
  begin
    if (Style and WS_SYSMENU = WS_SYSMENU) then
      Include(Result, biSystemMenu);
    if (Style and WS_MAXIMIZEBOX = WS_MAXIMIZEBOX) then
      Include(Result, biMaximize);
    if (Style and WS_MINIMIZEBOX = WS_MINIMIZEBOX) then
      Include(Result, biMinimize);
    if (ExStyle and WS_EX_CONTEXTHELP = WS_EX_CONTEXTHELP) and (not(biMaximize in Result)) and (not(biMinimize in Result)) then
      Include(Result, biHelp);
  end;
end;

function TSysDialogStyleHook.GetBorderSize: TRect;
var
  Size: TSize;
  Details: TThemedElementDetails;
  Detail: TThemedWindow;
begin
  {
    Result.Left = Left border width
    Result.Top = Caption height
    Result.Right = Right border width
    Result.Bottom = Bottom border height
  }
  Result := Rect(0, 0, 0, 0);
  if BorderStyle = bsNone then
    Exit;

  if not StyleServices.Available then
    Exit;
  { Caption height }
  if not UseSmallBorder then
    Detail := twCaptionActive
  else
    Detail := twSmallCaptionActive;
  Details := StyleServices.GetElementDetails(Detail);
  StyleServices.GetElementSize(0, Details, esActual, Size);
  Result.Top := Size.cy;
  { Left border width }
  if not UseSmallBorder then
    Detail := twFrameLeftActive
  else
    Detail := twSmallFrameLeftActive;
  Details := StyleServices.GetElementDetails(Detail);
  StyleServices.GetElementSize(0, Details, esActual, Size);
  Result.Left := Size.cx;
  { Right border width }
  if not UseSmallBorder then
    Detail := twFrameRightActive
  else
    Detail := twSmallFrameRightActive;
  Details := StyleServices.GetElementDetails(Detail);
  StyleServices.GetElementSize(0, Details, esActual, Size);
  Result.Right := Size.cx;
  { Bottom border height }
  if not UseSmallBorder then
    Detail := twFrameBottomActive
  else
    Detail := twSmallFrameBottomActive;
  Details := StyleServices.GetElementDetails(Detail);
  StyleServices.GetElementSize(0, Details, esActual, Size);
  Result.Bottom := Size.cy;
end;

function TSysDialogStyleHook.GetBorderStyle: TFormBorderStyle;
begin
  Result := bsNone;
  if not UpdateRegion then
    Exit(bsNone);
  with SysControl do
  begin
    if (Style and WS_OVERLAPPED = WS_OVERLAPPED) or (Style and WS_OVERLAPPEDWINDOW = WS_OVERLAPPEDWINDOW) or (Style and WS_CAPTION = WS_CAPTION) or
      (ExStyle and WS_EX_OVERLAPPEDWINDOW = WS_EX_OVERLAPPEDWINDOW) and (ExStyle and WS_EX_TOOLWINDOW <> WS_EX_TOOLWINDOW) then
    begin
      if (Style and WS_SIZEBOX <> WS_SIZEBOX) and ((Style and WS_MINIMIZEBOX = WS_MAXIMIZE) or (Style and WS_MINIMIZEBOX = WS_MINIMIZEBOX)) then
        Result := bsSingle;

      if (Style and WS_SIZEBOX <> WS_SIZEBOX) and (Style and WS_MINIMIZEBOX <> WS_MAXIMIZE) and (Style and WS_MINIMIZEBOX <> WS_MINIMIZEBOX) then
        Result := bsDialog;

      if (Style and WS_SIZEBOX = WS_SIZEBOX) then
        Result := bsSizeable;
    end
    else if (ExStyle and WS_EX_TOOLWINDOW = WS_EX_TOOLWINDOW) then
    begin
      if (Style and WS_SIZEBOX = WS_SIZEBOX) then
        Result := bsSizeToolWin
      else
        Result := bsToolWindow;
    end
    else
      Result := bsNone;
  end;
end;

function TSysDialogStyleHook.UseSmallBorder: Boolean;
begin
  Result := (BorderStyle = bsToolWindow) or (BorderStyle = bsSizeToolWin);
end;

function TSysDialogStyleHook.GetRegion: HRGN;
var
  R: TRect;
  LDetails: TThemedElementDetails;
  Detail: TThemedWindow;
begin
  Result := 0;
  if not StyleServices.Available then
    Exit;
  { Get Window Region }
  R := Rect(0, 0, SysControl.Width, SysControl.Height);
  if not UseSmallBorder then
    Detail := twCaptionActive
  else
    Detail := twSmallCaptionActive;

  DeleteObject(FRegion);
  LDetails := StyleServices.GetElementDetails(Detail);
  if not StyleServices.GetElementRegion(LDetails, R, Result) then
    FRegion := 0;
end;

function TSysDialogStyleHook.GetSysMenu: HMENU;
begin
  Result := GetSystemMenu(Handle, False);
end;

function TSysDialogStyleHook.GetSysMenuButtonRect: TRect;
var
  LBorderIcons: TBorderIcons;
  LBorderStyle: TBorderStyle;
  IconDetails: TThemedElementDetails;
  ButtonRect, R: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  LBorderStyle := BorderStyle;
  LBorderIcons := BorderIcons;
  if (biSystemMenu in LBorderIcons) and (LBorderStyle <> bsDialog) and (LBorderStyle <> bsToolWindow) and (LBorderStyle <> bsSizeToolWin) then
  begin
    IconDetails := StyleServices.GetElementDetails(twSysButtonNormal);
    if not StyleServices.GetElementContentRect(0, IconDetails, CaptionRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    //R := ButtonRect;
    R := Rect(0, 0, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON));
    RectVCenter(R, ButtonRect);
    Result := ButtonRect;
  end;

end;

function TSysDialogStyleHook.GetUpdateRegion: Boolean;
begin
  with SysControl do
    Result := not((Style and WS_CAPTION <> WS_CAPTION) and (Style and WS_SYSMENU <> WS_SYSMENU) and (Style and WS_SIZEBOX <> WS_SIZEBOX));
end;

function TSysDialogStyleHook.GetIconFast: TIcon;
begin
  if (FIcon = nil) or (FIconHandle = 0) then
    Result := GetIcon
  else
    Result := FIcon;
end;

function TSysDialogStyleHook.GetIcon: TIcon;
var
  IconX, IconY: Integer;
  TmpHandle: THandle;
  Info: TWndClassEx;
  Buffer: array [0 .. 255] of Char;
begin
  TmpHandle := THandle(SendMessage(Handle, WM_GETICON, ICON_SMALL, 0));
  if TmpHandle = 0 then
    TmpHandle := THandle(SendMessage(Handle, WM_GETICON, ICON_BIG, 0));

  if TmpHandle = 0 then
    TmpHandle := THandle(SendMessage(Handle, WM_GETICON, ICON_SMALL2, 0));

  if TmpHandle = 0 then
  begin
    { Get instance }
    GetClassName(Handle, @Buffer, sizeof(Buffer));
    FillChar(Info, sizeof(Info), 0);
    Info.cbSize := sizeof(Info);

    if GetClassInfoEx(GetWindowLong(Handle, GWL_HINSTANCE), @Buffer, Info) then
    begin
      TmpHandle := Info.hIconSm;
      if TmpHandle = 0 then
        TmpHandle := Info.HICON;
    end
  end;

  if FIcon = nil then
    FIcon := TIcon.Create;
  if TmpHandle <> 0 then
  begin
    IconX := GetSystemMetrics(SM_CXSMICON);
    if IconX = 0 then
      IconX := GetSystemMetrics(SM_CXSIZE);
    IconY := GetSystemMetrics(SM_CYSMICON);
    if IconY = 0 then
      IconY := GetSystemMetrics(SM_CYSIZE);
    FIcon.Handle := CopyImage(TmpHandle, IMAGE_ICON, IconX, IconY, 0);
    FIconHandle := TmpHandle;
  end;

  Result := FIcon;
end;

procedure TSysDialogStyleHook.PaintNC(Canvas: TCanvas);
var
  LDetails: TThemedElementDetails;
  CaptionBmp: TBitmap;
  DC: HDC;
  FButtonState: TThemedWindow;
  LCaptionRect, LBorderSize, R: TRect;
  ButtonRect, TextRect: TRect;
  TextTopOffset: Integer;
  IconDetails: TThemedElementDetails;
  LBorderIcons: TBorderIcons;
  LBorderStyle: TFormBorderStyle;
  CaptionDetails: TThemedElementDetails;
  TextFormat: TTextFormat;
  LText: String;
  nPos: Integer;
  LSysMenu: HMENU;
  ItemDisabled: Boolean;
begin
  LBorderStyle := BorderStyle;
  if (LBorderStyle = bsNone) or (WindowState = wsMinimized) then // (WindowState=wsMinimized) avoid bug in windows 8.1  and increase performance
    Exit;

  LBorderIcons := BorderIcons;
  LCaptionRect := CaptionRect;
  CaptionBmp := TBitmap.Create;
  CaptionBmp.SetSize(LCaptionRect.Width, LCaptionRect.Height);
  DC := CaptionBmp.Canvas.Handle;
  TextTopOffset := 0;
  TextRect := Rect(0, 0, 0, 0);;
  ButtonRect := Rect(0, 0, 0, 0);;
  FCaptionRect := Rect(0, 0, 0, 0);
  R := Rect(0, 0, 0, 0);

  { Caption }
  if FFrameActive then
  begin
    if not UseSmallBorder then
      LDetails := StyleServices.GetElementDetails(twCaptionActive)
    else
      LDetails := StyleServices.GetElementDetails(twSmallCaptionActive);
  end
  else
  begin
    if not UseSmallBorder then
      LDetails := StyleServices.GetElementDetails(twCaptionInActive)
    else
      LDetails := StyleServices.GetElementDetails(twSmallCaptionInActive);
  end;
  CaptionDetails := LDetails;
  StyleServices.DrawElement(DC, LDetails, LCaptionRect, nil);

  { Draw icon }

  if (biSystemMenu in LBorderIcons) and (LBorderStyle <> bsDialog) and (LBorderStyle <> bsToolWindow) and (LBorderStyle <> bsSizeToolWin) then
  begin
    IconDetails := StyleServices.GetElementDetails(twSysButtonNormal);
    if not StyleServices.GetElementContentRect(0, IconDetails, LCaptionRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);

    R := Rect(0, 0, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON));
    RectVCenter(R, ButtonRect);

    if ButtonRect.Width > 0 then
      DrawIconEx(CaptionBmp.Canvas.Handle, R.Left, R.Top, GetIconFast.Handle, 0, 0, 0, 0, DI_NORMAL);
    Inc(TextRect.Left, ButtonRect.Width + 5);
    FSysMenuButtonRect := ButtonRect;
  end
  else
    Inc(TextRect.Left, 7);

  { Draw buttons }
  LSysMenu := GetSystemMenu(Handle, False);
  nPos := GetMenuItemPos(LSysMenu, SC_CLOSE);
  ItemDisabled := IsItemDisabled(LSysMenu, nPos);
  if (biSystemMenu in LBorderIcons) and (not ItemDisabled) then
  begin
    if not UseSmallBorder then
    begin
      if (FPressedButton = HTCLOSE) and (FHotButton = HTCLOSE) then
        FButtonState := twCloseButtonPushed
      else if FHotButton = HTCLOSE then
        FButtonState := twCloseButtonHot
      else if FFrameActive then
        FButtonState := twCloseButtonNormal
      else
        FButtonState := twCloseButtonDisabled;
    end
    else
    begin
      if (FPressedButton = HTCLOSE) and (FHotButton = HTCLOSE) then
        FButtonState := twSmallCloseButtonPushed
      else if FHotButton = HTCLOSE then
        FButtonState := twSmallCloseButtonHot
      else if FFrameActive then
        FButtonState := twSmallCloseButtonNormal
      else
        FButtonState := twSmallCloseButtonDisabled;
    end;
    if FSysCloseButtonDisabled then
    begin
      if UseSmallBorder then
        LDetails := StyleServices.GetElementDetails(twSmallCloseButtonNormal)
      else
        LDetails := StyleServices.GetElementDetails(twCloseButtonNormal);
    end
    else
      LDetails := StyleServices.GetElementDetails(FButtonState);
    ButtonRect := CloseButtonRect;
    if (ButtonRect.Width > 0) then
      StyleServices.DrawElement(CaptionBmp.Canvas.Handle, LDetails, ButtonRect);

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
  end;

  if (biMaximize in LBorderIcons) and (biSystemMenu in LBorderIcons) and (LBorderStyle <> bsDialog) and (LBorderStyle <> bsToolWindow) and (LBorderStyle <> bsSizeToolWin) then
  begin
    if WindowState = wsMaximized then
    begin
      if (FPressedButton = HTMAXBUTTON) and (FHotButton = HTMAXBUTTON) then
        FButtonState := twRestoreButtonPushed
      else if FHotButton = HTMAXBUTTON then
        FButtonState := twRestoreButtonHot
      else if FFrameActive then
        FButtonState := twRestoreButtonNormal
      else
        FButtonState := twRestoreButtonDisabled;
    end
    else
    begin
      if (FPressedButton = HTMAXBUTTON) and (FHotButton = HTMAXBUTTON) then
        FButtonState := twMaxButtonPushed
      else if FHotButton = HTMAXBUTTON then
        FButtonState := twMaxButtonHot
      else if FFrameActive then
        FButtonState := twMaxButtonNormal
      else
        FButtonState := twMaxButtonDisabled;
    end;
    LDetails := StyleServices.GetElementDetails(FButtonState);
    ButtonRect := MaxButtonRect;

    if ButtonRect.Width > 0 then
      StyleServices.DrawElement(CaptionBmp.Canvas.Handle, LDetails, ButtonRect);
    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
  end;

  if (biMinimize in LBorderIcons) and (biSystemMenu in LBorderIcons) and (LBorderStyle <> bsDialog) and (LBorderStyle <> bsToolWindow) and (LBorderStyle <> bsSizeToolWin) then
  begin
    if (FPressedButton = HTMINBUTTON) and (FHotButton = HTMINBUTTON) then
      FButtonState := twMinButtonPushed
    else if FHotButton = HTMINBUTTON then
      FButtonState := twMinButtonHot
    else if FFrameActive then
      FButtonState := twMinButtonNormal
    else
      FButtonState := twMinButtonDisabled;

    LDetails := StyleServices.GetElementDetails(FButtonState);
    ButtonRect := MinButtonRect;
    if ButtonRect.Width > 0 then
      StyleServices.DrawElement(CaptionBmp.Canvas.Handle, LDetails, ButtonRect);
    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
  end;

  if (biHelp in LBorderIcons) and (biSystemMenu in LBorderIcons) and ((not(biMaximize in LBorderIcons) and not(biMinimize in LBorderIcons)) or (LBorderStyle = bsDialog)) then
  begin
    if (FPressedButton = HTHELP) and (FHotButton = HTHELP) then
      FButtonState := twHelpButtonPushed
    else if FHotButton = HTHELP then
      FButtonState := twHelpButtonHot
    else if FFrameActive then
      FButtonState := twHelpButtonNormal
    else
      FButtonState := twHelpButtonDisabled;
    LDetails := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, LDetails, LCaptionRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    if ButtonRect.Width > 0 then
      StyleServices.DrawElement(CaptionBmp.Canvas.Handle, LDetails, ButtonRect);

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
  end;

  { draw text }
  TextFormat := [tfLeft, tfSingleLine, tfVerticalCenter];
  if SysControl.BidiMode = bmRightToLeft then
    Include(TextFormat, tfRtlReading);
  // Important: Must retrieve Text prior to calling DrawText as it causes
  // CaptionBuffer.Canvas to free its handle, making the outcome of the call
  // to DrawText dependent on parameter evaluation order.
  LText := SysControl.Text;

  if (WindowState = wsMaximized) // and (FormStyle <> fsMDIChild)
    and (TextTopOffset <> 0) and (biSystemMenu in LBorderIcons) then
  begin
    Inc(TextRect.Left, R.Left);
    MoveWindowOrg(CaptionBmp.Canvas.Handle, 0, TextTopOffset);
    StyleServices.DrawText(CaptionBmp.Canvas.Handle, CaptionDetails, LText, TextRect, TextFormat);
    MoveWindowOrg(CaptionBmp.Canvas.Handle, 0, -TextTopOffset);
  end
  else
    StyleServices.DrawText(CaptionBmp.Canvas.Handle, CaptionDetails, LText, TextRect, TextFormat);

  FCaptionRect := TextRect;

  Canvas.Draw(0, 0, CaptionBmp);
  CaptionBmp.Free;

  DC := Canvas.Handle;
  LBorderSize := BorderSize;

  { Left Border }
  if FFrameActive then
  begin
    if not UseSmallBorder then
      LDetails := StyleServices.GetElementDetails(twFrameLeftActive)
    else
      LDetails := StyleServices.GetElementDetails(twSmallFrameLeftActive);
  end
  else
  begin
    if not UseSmallBorder then
      LDetails := StyleServices.GetElementDetails(twFrameLeftInActive)
    else
      LDetails := StyleServices.GetElementDetails(twSmallFrameLeftInActive);
  end;

  R := Rect(0, LCaptionRect.Height, LBorderSize.Left, SysControl.Height);
  if SysControl.Width > LBorderSize.Left then
    StyleServices.DrawElement(DC, LDetails, R);

  { Right Border }
  if FFrameActive then
  begin
    if not UseSmallBorder then
      LDetails := StyleServices.GetElementDetails(twFrameRightActive)
    else
      LDetails := StyleServices.GetElementDetails(twSmallFrameRightActive);
  end
  else
  begin
    if not UseSmallBorder then
      LDetails := StyleServices.GetElementDetails(twFrameRightInActive)
    else
      LDetails := StyleServices.GetElementDetails(twSmallFrameRightInActive);
  end;
  R := Rect(SysControl.Width - LBorderSize.Right, LCaptionRect.Height, SysControl.Width, SysControl.Height);
  if SysControl.Width > LBorderSize.Right then
    StyleServices.DrawElement(DC, LDetails, R);

  { Bottom Border }
  if FFrameActive then
  begin
    if not UseSmallBorder then
      LDetails := StyleServices.GetElementDetails(twFrameBottomActive)
    else
      LDetails := StyleServices.GetElementDetails(twSmallFrameBottomActive);
  end
  else
  begin
    if not UseSmallBorder then
      LDetails := StyleServices.GetElementDetails(twFrameBottomInActive)
    else
      LDetails := StyleServices.GetElementDetails(twSmallFrameBottomInActive);
  end;
  R := Rect(0, SysControl.Height - LBorderSize.Bottom, SysControl.Width, SysControl.Height);
  StyleServices.DrawElement(DC, LDetails, R);
end;

procedure TSysDialogStyleHook.Restore;
begin
  FPressedButton := 0;
  FHotButton := 0;
  if Handle <> 0 then
    SendMessage(Handle, WM_SYSCOMMAND, SC_RESTORE, 0);
end;

procedure TSysDialogStyleHook.WMNCACTIVATE(var Message: TWMNCActivate);
begin
  Handled := False;
  if not StyleServicesEnabled then
    Exit;

  if not OverridePaintNC then
    Exit;

  FFrameActive := Message.Active;
  InvalidateNC;
  Message.Result := 1;
  Handled := True;
end;

procedure TSysDialogStyleHook.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  Handled := False;
  if (not StyleServicesEnabled) or (not OverridePaintNC) then
    Exit;
  if (BorderStyle = bsNone) or (not UpdateRegion) then
    Exit;
  inherited;
end;

function TSysDialogStyleHook.NormalizePoint(const P: TPoint): TPoint;
var
  WindowPos, ClientPos: TPoint;
begin
  { Convert the point from the screen to the client window . }
  WindowPos := Point(SysControl.Left, SysControl.Top);
  ClientPos := Point(0, 0);
  ClientToScreen(Handle, ClientPos);
  Result := P;
  ScreenToClient(Handle, Result);
  Inc(Result.X, ClientPos.X - WindowPos.X);
  Inc(Result.Y, ClientPos.Y - WindowPos.Y);
end;

procedure TSysDialogStyleHook.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
begin
  Handled := False;
  if (not StyleServicesEnabled) or (not OverridePaintNC) then
    Exit;

  if OverridePaintNC then
  begin
    P := Point(Message.XPos, Message.YPos);
    P := NormalizePoint(P);
    Message.Result := GetHitTest(P);
    if ((Message.Result <> HTCLOSE) and (Message.Result <> HTMAXBUTTON) and (Message.Result <> HTMINBUTTON) and (Message.Result <> HTHELP)) then
    begin
      // Message.Result := CallDefaultProc(TMessage(Message));
      { Check if form can be scrolled . }
      inherited;
      { We need to correct the result after calling the default message . }
      if ((Message.Result = HTCLOSE) or (Message.Result = HTMAXBUTTON) or (Message.Result = HTMINBUTTON) or (Message.Result = HTHELP)) then
        Message.Result := HTCLIENT;
    end;
    Handled := True;
  end;
end;

procedure TSysDialogStyleHook.WMNCLButtonDown(var Message: TWMNCLButtonDown);
var
  P: TPoint;
begin
  Handled := False;
  if (not StyleServicesEnabled) or (not OverridePaintNC) then
    Exit;

  if OverridePaintNC then
  begin
    if (Message.HitTest = HTCLOSE) or (Message.HitTest = HTMAXBUTTON) or (Message.HitTest = HTMINBUTTON) or (Message.HitTest = HTHELP) then
    begin
      FPressedButton := Message.HitTest;
      InvalidateNC;
      SetRedraw(False);
      { For some reason ,we can not handle the WMNCLBUTTONUP message ..
        So we need to handle it inside the WMNCLBUTTONDOWN proc (this proc).
      }
      { Before handling the default message => this proc is WMNCLBUTTONDOWN }
      Message.Result := CallDefaultProc(TMessage(Message));

      { After handling the default message => this proc is WMNCLBUTTONUP }

      SetRedraw(True);
      FPressedButton := 0;
      FHotButton := 0;
      InvalidateNC;
      GetCursorPos(P);
      P := NormalizePoint(P);

      case Message.HitTest of
        HTCLOSE:
          if CloseButtonRect.Contains(P) then
            if Message.Result <> 0 then // only if the app doesn't processes this message
              Close;
        HTMAXBUTTON:
          begin
            if MaxButtonRect.Contains(P) then
            begin
              if WindowState = wsMaximized then
                Restore
              else
                Maximize;
            end;
          end;
        HTMINBUTTON:
          if MinButtonRect.Contains(P) then
            Minimize;
        HTHELP:
          if HelpButtonRect.Contains(P) then
            Help;
      end;
    end
    else
    begin
      inherited;
      Handled := True;
      Exit;
    end;
    Handled := True;
  end;
end;

procedure TSysDialogStyleHook.WMNCLButtonUp(var Message: TWMNCLButtonUp);
begin
  { Reserved for potential updates . }
  Handled := False;
end;

procedure TSysDialogStyleHook.WMNCMouseMove(var Message: TWMNCHitMessage);
begin
  { Reserved for potential updates . }
  Handled := False;
end;

procedure TSysDialogStyleHook.WMPaint(var Message: TMessage);
begin
  if IsWindowMsgBox(Handle) and OverridePaint then
  begin
    inherited;
    Exit;
  end;
  Message.Result := CallDefaultProc(Message);
  Handled := True;
end;

procedure TSysDialogStyleHook.WMSetText(var Message: TMessage);
var
  FRedraw: Boolean;
  LBorderStyle : TFormBorderStyle;
begin
  LBorderStyle := BorderStyle;
  if (LBorderStyle = bsNone) or (WindowState = wsMinimized) or (StyleServices.IsSystemStyle) then
  begin
    Handled := False;
    Exit;
  end;

  FRedraw := True;

  if  IsWindowVisible(Handle) then
  begin
    //Application.ProcessMessages;
    FRedraw := False;
    SetRedraw(False);
  end;

  CallDefaultProc(Message);

  if not FRedraw then
  begin
    SetRedraw(True);
    InvalidateNC;
  end;
  Handled := True;
end;


procedure TSysDialogStyleHook.WMSIZE(var Message: TWMSize);
begin
  Handled := False;
  if (not StyleServicesEnabled) or (not OverridePaintNC) then
    Exit;

  Message.Result := CallDefaultProc(TMessage(Message));

  FRegion := GetRegion;
  if (FRegion <> 0) and (BorderStyle <> bsNone) and UpdateRegion then
    SetWindowRgn(Handle, FRegion, True);
  Handled := True;
end;

procedure TSysDialogStyleHook.WndProc(var Message: TMessage);
var
  DFBW: Integer;
  LBorderSize: TRect;
  LParentHandle: HWND;
begin
  // Addlog(Format('TSysDialogStyleHook $0x%x %s', [SysControl.Handle, WM_To_String(Message.Msg)]));
  case Message.msg of

    WM_WINDOWPOSCHANGED:
      begin
        FSysCloseButtonDisabled := IsSysCloseButtonDisabled;
      end;

    WM_CREATE:
      begin
        Message.Result := CallDefaultProc(Message);
        { DFBW =Default Frame Border Width }
        DFBW := GetSystemMetrics(SM_CXBORDER);
        Inc(DFBW);
        LBorderSize := GetBorderSize;
        if (SysControl.Width > LBorderSize.Left) and (SysControl.Width > LBorderSize.Right) then
          SetWindowPos(Handle, 0, 0, 0, SysControl.Width + DFBW, SysControl.Height + DFBW + 1, SWP_NOMOVE or SWP_NOZORDER or SWP_FRAMECHANGED);
        Exit;
      end;

    WM_DESTROY:
      begin
        { In some situations ..we can not get the ParentHandle
          after processing the default WM_DESTROY message.
          => Save the parent before calling the default message.
        }
        SysControl.Destroyed:=True;
        //OutputDebugString(PChar(Format('TSysDialogStyleHook $0x%x %s', [SysControl.Handle, WM_To_String(Message.Msg)])));
        LParentHandle := ParentHandle;

        if  (LParentHandle>0) and (TSysStyleManager.SysStyleHookList.ContainsKey(LParentHandle)) and  TSysStyleManager.SysStyleHookList.Items[ParentHandle].SysControl.Destroyed  then
         Message.Result :=0
        else
         Message.Result := CallDefaultProc(Message);

        if LParentHandle > 0 then
        begin
          { When destroying the child window ..
            the parent window must be repainted . }
          RedrawWindow(LParentHandle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INTERNALPAINT or RDW_INVALIDATE);
        end;

        Handled := True;
      end;
  end;
  inherited;
end;

{ TSysScrollingStyleHook }
function TSysScrollingStyleHook.NormalizePoint(const P: TPoint): TPoint;
var
  WindowPos, ClientPos: TPoint;
begin
  { Convert the point from the screen to the client window . }
  WindowPos := Point(SysControl.Left, SysControl.Top);
  ClientPos := Point(0, 0);
  ClientToScreen(Handle, ClientPos);
  Result := P;
  ScreenToClient(Handle, Result);
  Inc(Result.X, ClientPos.X - WindowPos.X);
  Inc(Result.Y, ClientPos.Y - WindowPos.Y);
end;

procedure TSysScrollingStyleHook.CMSCROLLTRACKING(var Message: TMessage);
var
  P: TPoint;
  Pos, Delta: Integer;
begin
  if (not OverridePaintNC) or (not StyleServicesEnabled) then
  begin
    Handled := False;
    Exit;
  end;
  P.X := Longint(Word(Message.WParam));
  P.Y := Longint(HiWord(Message.WParam));
  GetCursorPos(P);
  if FScrollKind = sbVertical then
  begin
    if (P.Y >= 0) then
    begin
      Pos := GetVertScrollPosFromPoint(P);
      FTrackingPos := GetVertThumbPosFromPos(Pos);
      Delta := Pos - FPrevPos;
      DrawVertScroll(0); { Draw & take Tracking account . }
      { Do Scroll }
      Scroll(sbVertical, skTracking, Pos, Delta);
      FPrevPos := VertScrollInfo.nPos;
    end;
  end
  else if FScrollKind = sbHorizontal then
  begin
    if (P.X >= 0) then
    begin
      Pos := GetHorzScrollPosFromPoint(P);
      FTrackingPos := GetHorzThumbPosFromPos(Pos);
      Delta := Pos - FPrevPos;
      DrawHorzScroll(0); { Draw & take Tracking account . }
      { Do Scroll }
      Scroll(sbHorizontal, skTracking, Pos, Delta);
      FPrevPos := HorzScrollInfo.nPos;
    end;
  end;
  Handled := True;
end;

constructor TSysScrollingStyleHook.Create(AHandle: THandle);
begin
  inherited;
  FTracking := False;
  FNCMouseDown := False;
  FAllowScrolling := True;
  FTrackingPos := 0;
  FTrackTimer := nil;
  FPrevPoint := Point(-1, -1);
  FPrevPos := 0;
  InitScrollState;
end;

destructor TSysScrollingStyleHook.Destroy;
begin
  if Assigned(FTrackTimer) then
    FreeAndNil(FTrackTimer);
  inherited;
end;

procedure TSysScrollingStyleHook.DoLineTrackTimer(Sender: TObject);
begin
  Scroll(FScrollKind, FScrollingType, 0, 0);
end;

procedure TSysScrollingStyleHook.DoPageTrackTimer(Sender: TObject);
var
  P: TPoint;
begin
  GetCursorPos(P);
  if (not VertSliderRect.Contains(P)) and (not HorzSliderRect.Contains(P)) then
  begin
    DoScroll(FScrollKind, FScrollingType, 0, 0);
  end;
end;

procedure TSysScrollingStyleHook.DoScroll(const Kind: TScrollBarKind; const ScrollType: TSysScrollingType; Pos, Delta: Integer);
begin
  if ScrollType <> skNone then
  begin
    Scroll(Kind, ScrollType, Pos, Delta);
    FPrevPos := VertScrollInfo.nPos;
  end;
end;

procedure TSysScrollingStyleHook.DoSliderTrackTimer(Sender: TObject);
var
  P: TPoint;
begin
  GetCursorPos(P);
  if (FPrevPoint <> P) and (FDownPoint <> P) then
  begin
    SendMessage(Handle, CM_SCROLLTRACKING, MakeWParam(P.X, P.Y), 0);
    FPrevPoint := P;
    FDownPoint := Point(-1, -1);
  end;
end;

procedure TSysScrollingStyleHook.DrawHorzScroll(DC: HDC);
var
  LDetails: TThemedElementDetails;
  R: TRect;
  B: TBitmap;
  BmpDC, LDC: HDC;
  cx, cy, PosX, ThumbSize: Integer;
  P: TPoint;
  Detail: TThemedScrollBar;
begin
  if not FHorzScrollBar then
    Exit;
  LDC := DC;
  R := HorzScrollRect;
  cx := BtnSize.cx;
  cy := BtnSize.cy;
  if R.Width > 0 then
  begin
    B := TBitmap.Create;
    try
      if DC = 0 then
        DC := GetWindowDC(Handle);

      if FVertScrollBar then
      begin
        if not LeftScrollBar then
        begin
          P := Point(R.Right, R.Top);
          P := NormalizePoint(P);
          DrawSmallRect(DC, Rect(P.X, P.Y, P.X + cx, P.Y + cy));
        end
        else
        begin
          P := Point(R.Left, R.Top);
          P := NormalizePoint(P);
          FillDC(DC, Rect(P.X - cx, P.Y, P.X, P.Y + cy), Color);
        end;
      end;

      B.SetSize(R.Width, R.Height);
      BmpDC := B.Canvas.Handle;

      { Draw Track face }
      Detail := tsUpperTrackHorzNormal;
      if (not SysControl.Enabled) or (HorzScrollDisabled) then
        Detail := tsUpperTrackHorzDisabled;
      R := Rect(0, 0, B.Width, B.Height);
      LDetails := StyleServices.GetElementDetails(Detail);
      StyleServices.DrawElement(BmpDC, LDetails, R);
      { Draw Left Button }
      Detail := FBtnLeftDetail;
      if (not SysControl.Enabled) or (HorzScrollDisabled) then
        Detail := tsArrowBtnLeftDisabled;
      R := Rect(0, 0, cx, cy);
      LDetails := StyleServices.GetElementDetails(Detail);
      StyleServices.DrawElement(BmpDC, LDetails, R);
      { Draw Slider Button }
      Detail := FHorzBtnSliderDetail;
      if (not SysControl.Enabled) or (HorzScrollDisabled) then
        Detail := tsThumbBtnHorzDisabled;
      PosX := GetHorzSliderPos;
      ThumbSize := GetHorzThumbSize;
      if FTracking then
        // R := Rect(cx + FTrackingPos, 0, cx + FTrackingPos + ThumbSize, cy)
        R := FTrackingRect
      else
        R := Rect(cx + PosX, 0, cy + PosX + ThumbSize, cy);
      if R.Left < cx then
        R := Rect(cx, 0, cx + ThumbSize, cy);
      if R.Right > (B.Width - cx) then
        R := Rect(B.Width - cx - ThumbSize, 0, B.Width - cx, cy);

      LDetails := StyleServices.GetElementDetails(Detail);
      if not HorzScrollDisabled then
        StyleServices.DrawElement(BmpDC, LDetails, R);

      { Draw Right Button }
      Detail := FBtnRightDetail;
      if (not SysControl.Enabled) or (HorzScrollDisabled) then
        Detail := tsArrowBtnRightDisabled;
      //R := HorzRightRect;
      R := Rect(B.Width - cx, 0, B.Width, cy);
      LDetails := StyleServices.GetElementDetails(Detail);
      StyleServices.DrawElement(BmpDC, LDetails, R);
    finally
      P.X := HorzScrollRect.Left;
      P.Y := HorzScrollRect.Top;
      P := NormalizePoint(P);
      BitBlt(DC, P.X, P.Y, HorzScrollRect.Width, HorzScrollRect.Height, B.Canvas.Handle, 0, 0, SRCCOPY);
      B.Free;
      if LDC = 0 then
        ReleaseDC(Handle, DC);
    end;
  end;

end;

procedure TSysScrollingStyleHook.DrawSmallRect(DC: HDC; const SmallRect: TRect);
var
  sColor: TColor;
begin
  sColor := StyleServices.GetStyleColor(scWindow);
  FillDC(DC, SmallRect, sColor);
end;

procedure TSysScrollingStyleHook.DrawVertScroll(DC: HDC);
var
  LDetails: TThemedElementDetails;
  R: TRect;
  B: TBitmap;
  BmpDC, LDC: HDC;
  cx, cy, PosY, ThumbSize: Integer;
  P: TPoint;
  Detail: TThemedScrollBar;
begin
  if not FVertScrollBar then
    Exit;
  LDC := DC;
  R := VertScrollRect;
  cx := BtnSize.cx;
  cy := BtnSize.cy;
  if R.Width > 0 then
  begin

    B := TBitmap.Create;
    try
      if DC = 0 then
        DC := GetWindowDC(Handle);

      B.SetSize(R.Width, R.Height);
      BmpDC := B.Canvas.Handle;

      { Draw Track face }
      R := Rect(0, 0, B.Width, B.Height);
      Detail := tsUpperTrackVertNormal;
      if (not SysControl.Enabled) or (VertScrollDisabled) then
        Detail := tsUpperTrackHorzDisabled;

      LDetails := StyleServices.GetElementDetails(Detail);
      StyleServices.DrawElement(BmpDC, LDetails, R);

      { Draw UpButton }
      R := Rect(0, 0, cx, cy);
      Detail := FBtnUpDetail;
      if (not SysControl.Enabled) or (VertScrollDisabled) then
        Detail := tsArrowBtnUpDisabled;

      LDetails := StyleServices.GetElementDetails(Detail);
      StyleServices.DrawElement(BmpDC, LDetails, R);

      { Draw SliderButton }
      PosY := GetVertSliderPos;
      ThumbSize := GetVertThumbSize;
      if FTracking then
        // R := Rect(0, FTrackingPos, BtnSize.cx, FTrackingPos + GetVertThumbSize)
        R := FTrackingRect
      else
        R := Rect(0, cy + PosY, cx, cy + PosY + ThumbSize);
      if R.Top < cy then
        R := Rect(0, cy, cx, cy + ThumbSize);
      if R.Bottom > (B.Height - cy) then
        R := Rect(0, B.Height - cy - ThumbSize, cx, B.Height - cy);

      Detail := FVertBtnSliderDetail;
      if (not SysControl.Enabled) or (VertScrollDisabled) then
        Detail := tsThumbBtnVertDisabled;

      LDetails := StyleServices.GetElementDetails(Detail);
      if not VertScrollDisabled then
        StyleServices.DrawElement(BmpDC, LDetails, R);

      { Draw DownButton }
      R := Rect(0, B.Height - cy, cx, B.Height);
      Detail := FBtnDownDetail;
      if (not SysControl.Enabled) or (VertScrollDisabled) then
        Detail := tsArrowBtnDownDisabled;

      LDetails := StyleServices.GetElementDetails(Detail);
      StyleServices.DrawElement(BmpDC, LDetails, R);
    finally
      // Canvas.Draw(VertScrollRect.Left, VertScrollRect.Top, B);
      P.X := VertScrollRect.Left;
      P.Y := VertScrollRect.Top;
      // ScreenToClient(Handle, P);
      P := NormalizePoint(P);
      BitBlt(DC, P.X, P.Y, VertScrollRect.Width, VertScrollRect.Height, B.Canvas.Handle, 0, 0, SRCCOPY);
      B.Free;
      if LDC = 0 then
        ReleaseDC(Handle, DC);
    end;
  end;
end;

function TSysScrollingStyleHook.GetDefaultScrollBarSize: TSize;
begin
  { Return the default ScrollBar button size . }
  Result.cx := GetSystemMetrics(SM_CXVSCROLL);
  Result.cy := GetSystemMetrics(SM_CYVSCROLL);
end;

function TSysScrollingStyleHook.GetHorzLeftRect: TRect;
begin
  with HorzScrollRect do
    Result := Rect(Left, Top, Left + BtnSize.cx, Bottom);
end;

function TSysScrollingStyleHook.GetHorzScrollInfo: TScrollInfo;
begin
  FillChar(Result, sizeof(TScrollInfo), Char(0));
  Result.cbSize := sizeof(TScrollInfo);
  Result.fMask := SIF_ALL;
  Winapi.Windows.GetScrollInfo(Handle, SB_HORZ, Result);
end;

function TSysScrollingStyleHook.GetHorzScrollPosFromPoint(const P: TPoint): Integer;
var
  TrackRect, WinRect: TRect;
  Pos, MaxMin: Integer;
  LInfo: TScrollInfo;
  ThumbSize: Integer;
begin
  LInfo := HorzScrollInfo;
  Pos := P.X - FDownDis;
  WinRect := SysControl.WindowRect;
  TrackRect := HorzTrackRect;
  Dec(Pos, WinRect.Left);
  ThumbSize := GetHorzThumbSize;
  OffsetRect(TrackRect, -WinRect.Left, -WinRect.Top);

  FTrackingRect := Rect(Pos, 0, Pos + ThumbSize, BtnSize.cy);

  MaxMin := LInfo.nMax - LInfo.nMin;
  if MaxMin > 0 then
    Pos := MulDiv(Pos - TrackRect.Left, MaxMin - Integer(LInfo.nPage) + 1, TrackRect.Width - ThumbSize)
  else
    Pos := Pos - TrackRect.Left;
  if Pos < 0 then
    Pos := 0;
  if Pos >= LInfo.nMax - (Integer(LInfo.nPage) - 1) then
    Pos := LInfo.nMax - (Integer(LInfo.nPage) - 1);
  Result := Pos;
end;

function TSysScrollingStyleHook.GetHorzScrollRect: TRect;
var
  WinRect: TRect;
  BorderSize: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  WinRect := SysControl.WindowRect;
  BorderSize := GetBorderSize;
  with WinRect do
  begin
    Result.Left := Left;
    Result.Right := Right;
    Result.Top := Bottom - BtnSize.cy;
    Result.Bottom := Result.Top + BtnSize.cy;
  end;
  if (BorderSize.Left > 0) or (BorderSize.Top > 0) or (BorderSize.Right > 0) or (BorderSize.Bottom > 0) then
  begin
    Result.Left := Result.Left + BorderSize.Left;
    Result.Right := Result.Right - BorderSize.Right;
    Result.Bottom := Result.Bottom - BorderSize.Bottom;
    Result.Top := Result.Bottom - BtnSize.cy;
  end;
  if FVertScrollBar then
  begin
    if not LeftScrollBar then
      Dec(Result.Right, BtnSize.cx)
    else
      Inc(Result.Left, BtnSize.cx)
  end;
end;

function TSysScrollingStyleHook.GetHorzSliderPos: Integer;
begin
  with HorzScrollInfo do
    Result := MulDiv(nPos, HorzTrackRect.Width, nMax - nMin);
end;

function TSysScrollingStyleHook.GetHorzSliderRect: TRect;
var
  ThumbSize: Integer;
  PosX: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  ThumbSize := GetHorzThumbSize;
  PosX := MulDiv(HorzScrollInfo.nPos, HorzTrackRect.Width, HorzScrollInfo.nMax - HorzScrollInfo.nMin);
  with HorzTrackRect do
    Result := Rect(Left + PosX, Top, Left + PosX + ThumbSize, Bottom);
end;

function TSysScrollingStyleHook.GetHorzThumbPosFromPos(const Pos: Integer): Integer;
var
  PosX: Integer;
begin
  with HorzScrollInfo do
  begin
    PosX := MulDiv(Pos, HorzTrackRect.Width, nMax - nMin);
    Result := PosX + BtnSize.cx;
  end;
end;

function TSysScrollingStyleHook.GetHorzThumbSize: Integer;
begin
  with HorzScrollInfo do
  begin
    Result := MulDiv(nPage, HorzScrollRect.Width - (2 * BtnSize.cx), nMax - nMin);
    if Result < BtnSize.cy then
      Result := BtnSize.cy;
  end;
end;

function TSysScrollingStyleHook.GetHorzTrackRect: TRect;
begin
  Result := HorzScrollRect;
  if Result.Width > 0 then
  begin
    Result.Left := Result.Left + GetSystemMetrics(SM_CXHTHUMB);
    Result.Right := Result.Right - GetSystemMetrics(SM_CXHTHUMB);
  end
  else
    Result := Rect(0, 0, 0, 0);
end;

function TSysScrollingStyleHook.GetVertTrackRect: TRect;
begin
  Result := VertScrollRect;
  if Result.Width > 0 then
  begin
    Result.Top := Result.Top + GetSystemMetrics(SM_CYVTHUMB);
    Result.Bottom := Result.Bottom - GetSystemMetrics(SM_CYVTHUMB);
  end
  else
    Result := Rect(0, 0, 0, 0);
end;

function TSysScrollingStyleHook.GetVertDownRect: TRect;
begin
  with VertScrollRect do
    Result := Rect(Left, Bottom - BtnSize.cy, Right, Bottom);
end;

function TSysScrollingStyleHook.GetHorzRightRect: TRect;
begin
  with HorzScrollRect do
    Result := Rect(Right - BtnSize.cx, Top, Right, Bottom);
end;

function TSysScrollingStyleHook.GetVertScrollRect: TRect;
var
  WinRect: TRect;
  BorderSize: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  WinRect := SysControl.WindowRect;
  BorderSize := GetBorderSize;
  with WinRect do
  begin
    if not LeftScrollBar then
    begin
      Result.Left := Right - BtnSize.cx;
      Result.Right := Result.Left + BtnSize.cx;
      Result.Top := Top;
      Result.Bottom := Bottom;
    end
    else
    begin
      Result.Left := Left;
      Result.Right := Left + BtnSize.cx;
      Result.Top := Top;
      Result.Bottom := Bottom;
    end;
  end;
  if (BorderSize.Left >= 0) or (BorderSize.Top >= 0) or (BorderSize.Right >= 0) or (BorderSize.Bottom >= 0) then
  begin
    if not LeftScrollBar then
    begin
      Result.Left := Result.Left - BorderSize.Right;
      Result.Right := Result.Left + BtnSize.cx;
      Result.Top := Result.Top + BorderSize.Top;
      Result.Bottom := Result.Bottom - BorderSize.Bottom;
    end
    else
    begin
      Result.Left := Result.Left + BorderSize.Left;
      Result.Right := Result.Left + BtnSize.cx;
      Result.Top := Result.Top + BorderSize.Top;
      Result.Bottom := Result.Bottom - BorderSize.Bottom;
    end;
  end;
  if FHorzScrollBar then
    Dec(Result.Bottom, BtnSize.cy);
end;

function TSysScrollingStyleHook.GetVertScrollInfo: TScrollInfo;
begin
  FillChar(Result, sizeof(TScrollInfo), Char(0));
  Result.cbSize := sizeof(TScrollInfo);
  Result.fMask := SIF_ALL;
  Winapi.Windows.GetScrollInfo(Handle, SB_VERT, Result);
end;

function TSysScrollingStyleHook.GetVertScrollPosFromPoint(const P: TPoint): Integer;
var
  TrackRect, WinRect: TRect;
  Pos, MaxMin: Integer;
  LInfo: TScrollInfo;
  ThumbSize: Integer;
begin
  LInfo := VertScrollInfo;
  Pos := P.Y - FDownDis;
  WinRect := SysControl.WindowRect;
  TrackRect := VertTrackRect;
  OffsetRect(TrackRect, -WinRect.Left, -WinRect.Top);
  Dec(Pos, WinRect.Top);
  ThumbSize := GetVertThumbSize;

  FTrackingRect := Rect(0, Pos, BtnSize.cx, Pos + ThumbSize);

  MaxMin := LInfo.nMax - LInfo.nMin;
  if MaxMin > 0 then
    Pos := MulDiv(Pos - TrackRect.Top, MaxMin - Integer(LInfo.nPage) + 2, (TrackRect.Height) - ThumbSize)
  else
    Pos := Pos - TrackRect.Top;
  if Pos < 0 then
    Pos := 0;
  if Pos >= LInfo.nMax - (Integer(LInfo.nPage) - 1) then
    Pos := LInfo.nMax - (Integer(LInfo.nPage) - 1);

  Result := Pos;
end;

function TSysScrollingStyleHook.GetVertSliderPos: Integer;
begin
  with VertScrollInfo do
    Result := MulDiv(nPos, VertTrackRect.Height, nMax - nMin);
end;

function TSysScrollingStyleHook.GetVertSliderRect: TRect;
var
  ThumbSize: Integer;
  PosY: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  ThumbSize := GetVertThumbSize;
  PosY := MulDiv(VertScrollInfo.nPos, VertTrackRect.Height, VertScrollInfo.nMax - VertScrollInfo.nMin);
  with VertTrackRect do
    Result := Rect(Left, Top + PosY, Right, Top + PosY + ThumbSize);
end;

function TSysScrollingStyleHook.GetVertThumbPosFromPos(const Pos: Integer): Integer;
var
  PosY: Integer;
begin
  with VertScrollInfo do
  begin
    PosY := MulDiv(Pos, VertTrackRect.Height, nMax - nMin);
    Result := PosY + BtnSize.cy;
  end;
end;

function TSysScrollingStyleHook.GetVertThumbSize: Integer;
begin
  with VertScrollInfo do
  begin
    Result := MulDiv(nPage, VertTrackRect.Height, nMax - nMin);
    if Result < BtnSize.cy then
      Result := BtnSize.cy;
  end;
end;

function TSysScrollingStyleHook.GetVertUpRect: TRect;
begin
  with VertScrollRect Do
    Result := Rect(Left, Top, Right, Top + BtnSize.cy);
end;

procedure TSysScrollingStyleHook.InitScrollState;
begin
  FBtnUpDetail := tsArrowBtnUpNormal;
  FBtnDownDetail := tsArrowBtnDownNormal;
  FVertBtnSliderDetail := tsThumbBtnVertNormal;
  FBtnLeftDetail := tsArrowBtnLeftNormal;
  FBtnRightDetail := tsArrowBtnRightNormal;
  FHorzBtnSliderDetail := tsThumbBtnHorzNormal;
end;

function TSysScrollingStyleHook.IsHorzScrollDisabled: Boolean;
begin
  if FHorzScrollBar then
  begin
    with HorzScrollInfo do
      Result := (Integer(nPage) > nMax);
  end
  else
    Result := False;
end;

function TSysScrollingStyleHook.IsLeftScrollBar: Boolean;
begin
  Result := (SysControl.ExStyle and WS_EX_LEFTSCROLLBAR = WS_EX_LEFTSCROLLBAR);
end;

function TSysScrollingStyleHook.IsVertScrollDisabled: Boolean;
begin
  if FVertScrollBar then
  begin
    with VertScrollInfo do
      Result := (Integer(nPage) > nMax);
  end
  else
    Result := False;
end;

procedure TSysScrollingStyleHook.MouseEnter;
begin
  if FVertScrollBar and (not FNCMouseDown) and (not VertScrollDisabled) then
  begin
    if (FBtnUpDetail <> tsArrowBtnUpNormal) or (FBtnDownDetail <> tsArrowBtnDownNormal) or (FVertBtnSliderDetail <> tsThumbBtnVertNormal) then
    begin
      FBtnUpDetail := tsArrowBtnUpNormal;
      FBtnDownDetail := tsArrowBtnDownNormal;
      FVertBtnSliderDetail := tsThumbBtnVertNormal;
      DrawVertScroll(0);
    end;
  end;
  if FHorzScrollBar and (not FNCMouseDown) and (not HorzScrollDisabled) then
  begin
    if (FBtnLeftDetail <> tsArrowBtnLeftNormal) or (FBtnRightDetail <> tsArrowBtnRightNormal) or (FHorzBtnSliderDetail <> tsThumbBtnHorzNormal) then
    begin
      FBtnLeftDetail := tsArrowBtnLeftNormal;
      FBtnRightDetail := tsArrowBtnRightNormal;
      FHorzBtnSliderDetail := tsThumbBtnHorzNormal;
      DrawHorzScroll(0);
    end;
  end;
end;

procedure TSysScrollingStyleHook.MouseLeave;
begin
  if not FNCMouseDown then
  begin
    if FVertScrollBar and not VertScrollDisabled then
    begin
      if (FBtnUpDetail <> tsArrowBtnUpNormal) or (FBtnDownDetail <> tsArrowBtnDownNormal) or (FVertBtnSliderDetail <> tsThumbBtnVertNormal) then
      begin
        FBtnUpDetail := tsArrowBtnUpNormal;
        FBtnDownDetail := tsArrowBtnDownNormal;
        FVertBtnSliderDetail := tsThumbBtnVertNormal;
        DrawVertScroll(0);
      end;
    end;
    if FHorzScrollBar and not HorzScrollDisabled then
    begin
      if (FBtnLeftDetail <> tsArrowBtnLeftNormal) or (FBtnRightDetail <> tsArrowBtnRightNormal) or (FHorzBtnSliderDetail <> tsThumbBtnHorzNormal) then
      begin
        FBtnLeftDetail := tsArrowBtnLeftNormal;
        FBtnRightDetail := tsArrowBtnRightNormal;
        FHorzBtnSliderDetail := tsThumbBtnHorzNormal;
        DrawHorzScroll(0);
      end;
    end;
  end;
end;

procedure TSysScrollingStyleHook.PaintNC(Canvas: TCanvas);
begin
  if (Canvas.HandleAllocated) and (not FTracking) then
  begin
    if FVertScrollBar then
      DrawVertScroll(Canvas.Handle);
    if FHorzScrollBar then
      DrawHorzScroll(Canvas.Handle);
  end;
end;

procedure TSysScrollingStyleHook.Scroll(const Kind: TScrollBarKind; const ScrollType: TSysScrollingType; Pos, Delta: Integer);
begin
  if Kind = sbVertical then
  begin
    case ScrollType of
      skTracking:
        begin
          FLstPos := Pos;
          FAllowScrolling := True;
          SendMessage(Handle, WM_VSCROLL, MakeWParam(SB_THUMBTRACK, Pos), 0);
          FAllowScrolling := False;
        end;
      skLineUp: SendMessage(Handle, WM_VSCROLL, SB_LINEUP, 0);
      skLineDown: SendMessage(Handle, WM_VSCROLL, SB_LINEDOWN, 0);
      skPageUp: SendMessage(Handle, WM_VSCROLL, SB_PAGEUP, 0);
      skPageDown: SendMessage(Handle, WM_VSCROLL, SB_PAGEDOWN, 0);
    end;
  end
  else if Kind = sbHorizontal then
  begin
    case ScrollType of
      skTracking:
        begin
          FLstPos := Pos;
          FAllowScrolling := True;
          SendMessage(Handle, WM_HSCROLL, MakeWParam(SB_THUMBTRACK, Pos), 0);
          FAllowScrolling := False;
        end;
      skLineLeft: SendMessage(Handle, WM_HSCROLL, SB_LINELEFT, 0);
      skLineRight: SendMessage(Handle, WM_HSCROLL, SB_LINERIGHT, 0);
      skPageLeft: SendMessage(Handle, WM_HSCROLL, SB_PAGELEFT, 0);
      skPageRight: SendMessage(Handle, WM_HSCROLL, SB_PAGERIGHT, 0);
    end;
  end;
end;

procedure TSysScrollingStyleHook.StartLineTrackTimer;
begin
  if Assigned(FTrackTimer) then
  begin
    FTrackTimer.Enabled := False;
    FreeAndNil(FTrackTimer);
  end;

  FTrackTimer := TTimer.Create(nil);
  with FTrackTimer do
  begin
    Interval := 100;
    OnTimer := DoLineTrackTimer;
    Enabled := True;
  end;
end;

procedure TSysScrollingStyleHook.StartPageTrackTimer;
begin
  if Assigned(FTrackTimer) then
  begin
    FTrackTimer.Enabled := False;
    FreeAndNil(FTrackTimer);
  end;

  FTrackTimer := TTimer.Create(nil);
  with FTrackTimer do
  begin
    Interval := 100;
    OnTimer := DoPageTrackTimer;
    Enabled := True;
  end;
end;

procedure TSysScrollingStyleHook.StartSliderTrackTimer;
begin
  if Assigned(FTrackTimer) then
  begin
    FTrackTimer.Enabled := False;
    FreeAndNil(FTrackTimer);
  end;

  FTrackTimer := TTimer.Create(nil);
  with FTrackTimer do
  begin
    Interval := 100;
    OnTimer := DoSliderTrackTimer;
    Enabled := True;
  end;
end;

procedure TSysScrollingStyleHook.StopLineTrackTimer;
begin
  if Assigned(FTrackTimer) then
  begin
    FTrackTimer.Enabled := False;
    FreeAndNil(FTrackTimer);
  end;
end;

procedure TSysScrollingStyleHook.StopPageTrackTimer;
begin
  if Assigned(FTrackTimer) then
  begin
    FTrackTimer.Enabled := False;
    FreeAndNil(FTrackTimer);
  end;
end;

procedure TSysScrollingStyleHook.StopSliderTrackTimer;
begin
  if Assigned(FTrackTimer) then
  begin
    FTrackTimer.Enabled := False;
    FreeAndNil(FTrackTimer);
  end;
end;

procedure TSysScrollingStyleHook.WMNCCalcSize(var Message: TWMNCCalcSize);
var
  OrgStyle, NewStyle: NativeInt;
  BorderSize: TRect;
begin
  if (not OverridePaintNC) or (not StyleServicesEnabled) then
  begin
    Handled := False;
    Exit;
  end;
  BorderSize := GetBorderSize;
  OrgStyle := SysControl.Style;
  NewStyle := SysControl.Style;
  FVertScrollBar := False;
  FHorzScrollBar := False;
  if OrgStyle and WS_VSCROLL = WS_VSCROLL then
  begin
    { Remove the VertScrollBar  . }
    NewStyle := NewStyle and not WS_VSCROLL;
    FVertScrollBar := True;
  end;
  if OrgStyle and WS_HSCROLL = WS_HSCROLL then
  begin
    { Remove the HorzScrollBar  . }
    NewStyle := NewStyle and not WS_HSCROLL;
    FHorzScrollBar := True;
  end;
  if OrgStyle <> NewStyle then
  begin
    SysControl.Style := NewStyle;
    if not HookedDirectly then
      Message.Result := CallDefaultProc(TMessage(Message));
    SysControl.Style := OrgStyle;
  end;
  if FVertScrollBar then
  begin
    { Insert a new VertScrollBar area . }
    if not LeftScrollBar then
      Dec(Message.CalcSize_Params.rgrc[0].Right, BtnSize.cx)
    else
      Inc(Message.CalcSize_Params.rgrc[0].Left, BtnSize.cx);
  end;
  if FHorzScrollBar then
    { Insert a new HorzScrollBar area . }
    Dec(Message.CalcSize_Params.rgrc[0].Bottom, BtnSize.cx);
  if SysControl.HasBorder then
  begin
    Inc(Message.CalcSize_Params.rgrc[0].Left, BorderSize.Left);
    Inc(Message.CalcSize_Params.rgrc[0].Top, BorderSize.Top);
    Dec(Message.CalcSize_Params.rgrc[0].Bottom, BorderSize.Bottom);
    Dec(Message.CalcSize_Params.rgrc[0].Right, BorderSize.Right);
  end;
  Handled := True;
end;

procedure TSysScrollingStyleHook.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
begin
  if (not OverridePaintNC) or (not StyleServicesEnabled) then
  begin
    Handled := False;
    Exit;
  end;
  Message.Result := CallDefaultProc(TMessage(Message));
  P.X := Message.XPos;
  P.Y := Message.YPos;
  { If Mouse on VertScrollBar . }
  if (FVertScrollBar and VertScrollRect.Contains(P)) then
  begin
    { Return HTVSCROLL allow the app to get WM_NCLBUTTONDOWN message . }
    Message.Result := HTVSCROLL;
    if (SysControl.Enabled and not VertScrollDisabled) then
    begin
      { If Mouse pressed then exit . }
      if not FNCMouseDown then
      begin
        if VertUpRect.Contains(P) then
        begin
          { VertUpButton Hot . }
          FVertBtnSliderDetail := tsThumbBtnVertNormal;
          if FBtnUpDetail <> tsArrowBtnUpHot then
          begin
            FBtnUpDetail := tsArrowBtnUpHot;
            DrawVertScroll(0);
          end;
        end
        else if VertDownRect.Contains(P) then
        begin
          { VertDownButton Hot . }
          FVertBtnSliderDetail := tsThumbBtnVertNormal;
          if FBtnDownDetail <> tsArrowBtnDownHot then
          begin
            FBtnDownDetail := tsArrowBtnDownHot;
            DrawVertScroll(0);
          end;
        end
        else if VertSliderRect.Contains(P) then
        begin
          { VertSliderButton Hot . }
          FBtnUpDetail := tsArrowBtnUpNormal;
          FBtnDownDetail := tsArrowBtnDownNormal;
          if FVertBtnSliderDetail <> tsThumbBtnVertHot then
          begin
            FVertBtnSliderDetail := tsThumbBtnVertHot;
            DrawVertScroll(0);
          end;
        end
        else
        begin
          { Update ScrollBar state . }
          if (FBtnUpDetail <> tsArrowBtnUpNormal) or (FBtnDownDetail <> tsArrowBtnDownNormal) or (FVertBtnSliderDetail <> tsThumbBtnVertNormal) then
          begin
            FBtnUpDetail := tsArrowBtnUpNormal;
            FBtnDownDetail := tsArrowBtnDownNormal;
            FVertBtnSliderDetail := tsThumbBtnVertNormal;
            DrawVertScroll(0);
          end
        end;
      end;
    end;
  end;
  { If Mouse on HorzScrollBar . }
  if (FHorzScrollBar and HorzScrollRect.Contains(P)) then
  begin
    { Return HTHSCROLL allow the app to get WM_NCLBUTTONDOWN message . }
    Message.Result := HTHSCROLL;
    if (SysControl.Enabled and not HorzScrollDisabled) then
    begin
      { If Mouse pressed then exit . }
      if not FNCMouseDown then
      begin
        if HorzLeftRect.Contains(P) then
        begin
          { HorzLeftButton Hot . }
          FHorzBtnSliderDetail := tsThumbBtnHorzNormal;
          if FBtnLeftDetail <> tsArrowBtnLeftHot then
          begin
            FBtnLeftDetail := tsArrowBtnLeftHot;
            DrawHorzScroll(0);
          end;
        end
        else if HorzRightRect.Contains(P) then
        begin
          { HorzRightButton Hot . }
          FHorzBtnSliderDetail := tsThumbBtnHorzNormal;
          if FBtnRightDetail <> tsArrowBtnRightHot then
          begin
            FBtnRightDetail := tsArrowBtnRightHot;
            DrawHorzScroll(0);
          end;
        end
        else if HorzSliderRect.Contains(P) then
        begin
          { HorzSliderButton Hot . }
          FBtnLeftDetail := tsArrowBtnLeftNormal;
          FBtnRightDetail := tsArrowBtnRightNormal;
          if FHorzBtnSliderDetail <> tsThumbBtnHorzHot then
          begin
            FHorzBtnSliderDetail := tsThumbBtnHorzHot;
            DrawHorzScroll(0);
          end;
        end
        else
        begin
          { Update ScrollBar state  . }
          if (FBtnLeftDetail <> tsArrowBtnLeftNormal) or (FBtnRightDetail <> tsArrowBtnRightNormal) or (FHorzBtnSliderDetail <> tsThumbBtnHorzNormal) then
          begin
            FBtnLeftDetail := tsArrowBtnLeftNormal;
            FBtnRightDetail := tsArrowBtnRightNormal;
            FHorzBtnSliderDetail := tsThumbBtnHorzNormal;
            DrawHorzScroll(0);
          end
        end;
      end;
    end;
  end;
  Handled := True;
end;

procedure TSysScrollingStyleHook.WMNCLButtonDown(var Message: TWMNCLButtonDown);
var
  P: TPoint;
begin
  if (not OverridePaintNC) or (not StyleServicesEnabled) then
  begin
    Handled := False;
    Exit;
  end;

  if not SysControl.Enabled then
  begin
    Message.Result := CallDefaultProc(TMessage(Message));
    Exit;
  end;

  FNCMouseDown := True;
  FTracking := False;
  if (Message.HitTest = HTVSCROLL) then
  begin
    if VertScrollDisabled then
    begin
      FTracking := False;
      FNCMouseDown := False;
      Message.Result := CallDefaultProc(TMessage(Message));
      FAllowScrolling := True;
      Handled := True;
      Exit;
    end;
    { Vertical ScrollBar }
    FScrollKind := sbVertical;
    GetCursorPos(P);
    { Save the DownPoint }
    FDownPoint := P;
    { The distance between the point & the top of VertSliderButton . }
    FDownDis := P.Y - VertSliderRect.Top;
    { The old ScrollBar Position }
    FPrevPos := VertScrollInfo.nPos;

//    OutputDebugString(PChar(Format('TSysScrollingStyleHook.WMNCLButtonDown P.X %d P.Y %d VertSliderRect.Left %d VertSliderRect.Top %d VertSliderRect.Width %d VertSliderRect.Height %d',
//    [P.X, P.Y, VertSliderRect.Left, VertSliderRect.Top, VertSliderRect.Width, VertSliderRect.Height])));

    if VertSliderRect.Contains(P) then
    begin
      { VertSliderButton pressed . }
      FVertBtnSliderDetail := tsThumbBtnVertPressed;
      StartSliderTrackTimer;
      DrawVertScroll(0); { Need Repaint ==> First painting }
      FTracking := True; { ==> Set it after first painting  . }
      FAllowScrolling := False;
      { Mouse Down }
      Message.Result := CallDefaultProc(TMessage(Message));
      { Mouse Up }
      FAllowScrolling := False;
      StopSliderTrackTimer;
      FVertBtnSliderDetail := tsThumbBtnVertNormal;
      FTracking := False; { Set it before the second painting . }
      DrawVertScroll(0); { Need Repaint ==> Second painting . }
    end
    else if (VertUpRect.Contains(P) or (VertDownRect.Contains(P))) then
    begin
      if VertUpRect.Contains(P) then
      begin
        { VertUpButton pressed . }
        FScrollingType := skLineUp;
        FBtnUpDetail := tsArrowBtnUpPressed;
      end
      else
      begin
        { VertDownButton pressed . }
        FScrollingType := skLineDown;
        FBtnDownDetail := tsArrowBtnDownPressed;
      end;
      DrawVertScroll(0); { Need Repaint . }
      StartLineTrackTimer;
      { Mouse Down }
      Message.Result := CallDefaultProc(TMessage(Message));
      { Mouse Up }
      StopLineTrackTimer;
      FBtnDownDetail := tsArrowBtnDownNormal;
      FBtnUpDetail := tsArrowBtnUpNormal;
      DrawVertScroll(0); { Need Repaint . }
    end
    else
    begin
      FScrollingType := skNone;
      if FDownPoint.Y > VertSliderRect.Bottom then
        FScrollingType := skPageDown;
      if FDownPoint.Y < VertSliderRect.Top then
        FScrollingType := skPageUp;
      DrawVertScroll(0); { Need Repaint . }
      {
        Scrolling from the track rect .
        ==> Not from Slider or Up/Down Button .
      }

      StartPageTrackTimer;
      { Mouse Down }
      Message.Result := CallDefaultProc(TMessage(Message));
      { Mouse Up }
      StopPageTrackTimer;
    end;
    DrawVertScroll(0); { Need Repaint . }
  end
  else if (Message.HitTest = HTHSCROLL) then
  { Horizontal ScrollBar }
  begin
    if HorzScrollDisabled then
    begin
      FTracking := False;
      FNCMouseDown := False;
      Message.Result := CallDefaultProc(TMessage(Message));
      FAllowScrolling := True;
      Handled := True;
      Exit;
    end;
    FScrollKind := sbHorizontal;
    GetCursorPos(P);
    FDownPoint := P;
    { The distance between the point & the left of HorzSliderButton . }
    FDownDis := P.X - HorzSliderRect.Left;
    FPrevPos := HorzScrollInfo.nPos;
    if HorzSliderRect.Contains(P) then
    begin
      FHorzBtnSliderDetail := tsThumbBtnHorzPressed;
      DrawHorzScroll(0); { Need Repaint ==> First painting . }
      FTracking := True; { Set it after first painting . }
      StartSliderTrackTimer;
      { Mouse Down }
      Message.Result := CallDefaultProc(TMessage(Message));
      { Mouse Up }
      StopSliderTrackTimer;
      FHorzBtnSliderDetail := tsThumbBtnHorzNormal;
      FTracking := False; { Set it before second painting . }
      DrawHorzScroll(0); { Need Repaint ==> Second painting. }
    end
    else if (HorzLeftRect.Contains(P) or (HorzRightRect.Contains(P))) then
    begin
      if HorzLeftRect.Contains(P) then
      begin
        FBtnLeftDetail := tsArrowBtnLeftPressed;
        FBtnRightDetail := tsArrowBtnRightNormal;
        FScrollingType := skLineLeft
      end
      else
      begin
        FBtnLeftDetail := tsArrowBtnLeftNormal;
        FBtnRightDetail := tsArrowBtnRightPressed;
        FScrollingType := skLineRight;
      end;
      DrawHorzScroll(0); { Need Repaint . }
      StartLineTrackTimer;
      { Mouse Down }
      Message.Result := CallDefaultProc(TMessage(Message));
      { Mouse Up }
      FBtnLeftDetail := tsArrowBtnLeftNormal;
      FBtnRightDetail := tsArrowBtnRightNormal;
      StopLineTrackTimer;
      DrawHorzScroll(0); { Need Repaint . }
    end
    else
    begin
      FScrollingType := skNone;
      if FDownPoint.X > HorzSliderRect.Right then
        FScrollingType := skPageRight;
      if FDownPoint.X < HorzSliderRect.Right then
        FScrollingType := skPageLeft;
      {
        Scrolling from the track rect .
        ==> Not from Slider or Left/Right Button .
      }
      StartPageTrackTimer;
      { Mouse Down }
      Message.Result := CallDefaultProc(TMessage(Message));
      { Mouse Up }
      StopPageTrackTimer;
    end;
    FTracking := False;
    DrawHorzScroll(0); { Need Repaint . }
  end
  else
  begin
    Message.Result := CallDefaultProc(TMessage(Message));
  end;
  FTracking := False;
  FNCMouseDown := False;
  Handled := True;
  FAllowScrolling := True;
end;

procedure TSysScrollingStyleHook.WMNCLButtonUp(var Message: TWMNCLButtonUp);
begin
  Message.Result := CallDefaultProc(TMessage(Message));
  Handled := True;
end;

procedure TSysScrollingStyleHook.WndProc(var Message: TMessage);
begin
  case Message.msg of

    WM_MOUSEWHEEL:
      begin
        Inherited;
        if FVertScrollBar then
          DrawVertScroll(0);
      end;

    WM_VSCROLL, WM_HSCROLL:
      begin
        if Word(Message.WParam) = SB_THUMBPOSITION then
        begin
          Message.WParam := MakeWParam(SB_THUMBPOSITION, FLstPos);
          CallDefaultProc(Message);
          Exit;
        end;
        if (not OverridePaintNC) or (not StyleServicesEnabled) then
        begin
          CallDefaultProc(Message);
          Exit;
        end;
        // if not FAllowScrolling then
        // Exit;
        inherited;
      end;

    WM_NCMOUSELEAVE, WM_MOUSEMOVE:
      begin
        if (not OverridePaintNC) or (not StyleServicesEnabled) then
        begin
          CallDefaultProc(Message);
          Exit;
        end;
        { Update ScrollBar State }
        if (FVertScrollBar and SysControl.Enabled and (not FNCMouseDown) and (not VertScrollDisabled)) then
        begin
          if (FBtnUpDetail <> tsArrowBtnUpNormal) or (FBtnDownDetail <> tsArrowBtnDownNormal) or (FVertBtnSliderDetail <> tsThumbBtnVertNormal) then
          begin
            FBtnUpDetail := tsArrowBtnUpNormal;
            FBtnDownDetail := tsArrowBtnDownNormal;
            FVertBtnSliderDetail := tsThumbBtnVertNormal;
            DrawVertScroll(0);
          end;
        end;
        if (FHorzScrollBar and SysControl.Enabled and (not FNCMouseDown) and (not HorzScrollDisabled)) then
        begin
          if (FBtnLeftDetail <> tsArrowBtnLeftNormal) or (FBtnRightDetail <> tsArrowBtnRightNormal) or (FHorzBtnSliderDetail <> tsThumbBtnHorzNormal) then
          begin
            FBtnLeftDetail := tsArrowBtnLeftNormal;
            FBtnRightDetail := tsArrowBtnRightNormal;
            FHorzBtnSliderDetail := tsThumbBtnHorzNormal;
            DrawHorzScroll(0);
          end;
        end;
        inherited;
      end;

    WM_PAINT:
      begin
        if (not OverridePaintNC) or (not StyleServicesEnabled) then
        begin
          CallDefaultProc(Message);
          Exit;
        end;

        inherited WndProc(Message);
        { Do not paint while tracking . }
        if (not FTracking) and (OverridePaintNC) then
        begin
          if FVertScrollBar then
            DrawVertScroll(0);
          if FHorzScrollBar then
            DrawHorzScroll(0);
        end;
        Exit;
      end;
  else inherited;
  end;

end;

{ TSysScrollBarStyleHook }

constructor TSysScrollBarStyleHook.Create(AHandle: THandle);
begin
  inherited;
{$IF CompilerVersion > 23}
  StyleElements := [seClient];
{$ELSE}
  OverridePaint := True;
  OverridePaintNC := False;
  OverrideFont := False;
{$IFEND}
end;

destructor TSysScrollBarStyleHook.Destroy;
begin
  inherited;
end;

procedure TSysScrollBarStyleHook.WndProc(var Message: TMessage);
var
  DC: HDC;
  PS: TPaintStruct;
  LDetails: TThemedElementDetails;
begin
  case Message.msg of
    WM_PAINT:
      begin
        if not OverridePaint then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;
        if ((SysControl.Style and SBS_SIZEGRIP = SBS_SIZEGRIP) or (SysControl.Style and SBS_SIZEBOX = SBS_SIZEBOX)) then
        begin
          BeginPaint(Handle, PS);
          try
            DC := GetDC(Handle);
            try
              LDetails := StyleServices.GetElementDetails(tsSizeBoxLeftAlign);
              StyleServices.DrawElement(DC, LDetails, SysControl.ClientRect);
            finally
              ReleaseDC(Handle, DC);
            end;
          finally
            EndPaint(Handle, PS);
          end;
          Exit;
        end

        else
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;
        Exit;
      end;
    WM_ERASEBKGND:
      begin
        if OverridePaint then
        begin
          Message.Result := 1;
          Exit;
        end
        else
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;
      end;
  end;

  inherited;
end;

initialization

 {$IFNDEF USE_Vcl.Styles.Hooks}
 //UseLatestCommonDialogs := False;
 {$ENDIF}

{$IF CompilerVersion >= 30}
 TStyleManager.SystemHooks := TStyleManager.SystemHooks - [shDialogs];
{$IFEND}


  if StyleServices.Available then
  begin
    TSysStyleManager.RegisterSysStyleHook('#32770', TSysDialogStyleHook);
    //TSysStyleManager.RegisterSysStyleHook('HH Parent', TSysDialogStyleHook);
    TSysStyleManager.RegisterSysStyleHook('ScrollBar', TSysScrollBarStyleHook);
  end;

finalization
  TSysStyleManager.UnRegisterSysStyleHook('#32770', TSysDialogStyleHook);
  //TSysStyleManager.UnRegisterSysStyleHook('HH Parent', TSysDialogStyleHook);
  TSysStyleManager.UnRegisterSysStyleHook('ScrollBar', TSysScrollBarStyleHook);
end.
