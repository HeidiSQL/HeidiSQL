unit VirtualTrees.StyleHooks;

// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Alternatively, you may redistribute this library, use and/or modify it under the terms of the
// GNU Lesser General Public License as published by the Free Software Foundation;
// either version 2.1 of the License, or (at your option) any later version.
// You may obtain a copy of the LGPL at http://www.gnu.org/copyleft/.
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The original code is VirtualTrees.pas, released September 30, 2000.
//
// The initial developer of the original code is digital publishing AG (Munich, Germany, www.digitalpublishing.de),
// written by Mike Lischke (public@soft-gems.net, www.soft-gems.net).
//
// Portions created by digital publishing AG are Copyright
// (C) 1999-2001 digital publishing AG. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------


interface

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.UxTheme,

  System.Classes,
  Vcl.Themes,
  Vcl.Controls;

const
  CM_UPDATE_VCLSTYLE_SCROLLBARS = CM_BASE + 2050;

type
  // XE2+ VCL Style
  TVclStyleScrollBarsHook = class(TMouseTrackControlStyleHook)
  strict private type
  {$REGION 'TVclStyleScrollBarWindow'}
      TVclStyleScrollBarWindow = class(TWinControl)strict private FScrollBarWindowOwner: TVclStyleScrollBarsHook;
    FScrollBarVertical: Boolean;
    FScrollBarVisible: Boolean;
    FScrollBarEnabled: Boolean;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
  strict protected
    procedure CreateParams(var Params: TCreateParams);
    override;
  public
    constructor Create(AOwner: TComponent);
    override;
    property ScrollBarWindowOwner: TVclStyleScrollBarsHook read FScrollBarWindowOwner write FScrollBarWindowOwner;
    property ScrollBarVertical: Boolean read FScrollBarVertical write FScrollBarVertical;
    property ScrollBarVisible: Boolean read FScrollBarVisible write FScrollBarVisible;
    property ScrollBarEnabled: Boolean read FScrollBarEnabled write FScrollBarEnabled;
    end;
  {$ENDREGION}
  private
    FHorzScrollBarDownButtonRect: TRect;
    FHorzScrollBarDownButtonState: TThemedScrollBar;
    FHorzScrollBarRect: TRect;
    FHorzScrollBarSliderState: TThemedScrollBar;
    FHorzScrollBarSliderTrackRect: TRect;
    FHorzScrollBarUpButtonRect: TRect;
    FHorzScrollBarUpButtonState: TThemedScrollBar;
    FHorzScrollBarWindow: TVclStyleScrollBarWindow;
    FLeftMouseButtonDown: Boolean;
    FPrevScrollPos: Integer;
    FScrollPos: Single;
    FVertScrollBarDownButtonRect: TRect;
    FVertScrollBarDownButtonState: TThemedScrollBar;
    FVertScrollBarRect: TRect;
    FVertScrollBarSliderState: TThemedScrollBar;
    FVertScrollBarSliderTrackRect: TRect;
    FVertScrollBarUpButtonRect: TRect;
    FVertScrollBarUpButtonState: TThemedScrollBar;
    FVertScrollBarWindow: TVclStyleScrollBarWindow;

    procedure CMUpdateVclStyleScrollbars(var Message: TMessage); message CM_UPDATE_VCLSTYLE_SCROLLBARS;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMKeyDown(var Msg: TMessage); message WM_KEYDOWN;
    procedure WMKeyUp(var Msg: TMessage); message WM_KEYUP;
    procedure WMLButtonDown(var Msg: TWMMouse);  message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg: TWMMouse); message WM_LBUTTONUP;
    procedure WMNCLButtonDown(var Msg: TWMMouse); message WM_NCLBUTTONDOWN;
    procedure WMNCMouseMove(var Msg: TWMMouse); message WM_NCMOUSEMOVE;
    procedure WMNCLButtonUp(var Msg: TWMMouse); message WM_NCLBUTTONUP;
    procedure WMNCPaint(var Msg: TMessage); message WM_NCPAINT;
    procedure WMMouseMove(var Msg: TWMMouse); message WM_MOUSEMOVE;
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure WMVScroll(var Msg: TMessage); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TMessage); message WM_HSCROLL;
    procedure WMCaptureChanged(var Msg: TMessage); message WM_CAPTURECHANGED;
    procedure WMNCLButtonDblClk(var Msg: TWMMouse); message WM_NCLBUTTONDBLCLK;
    procedure WMSize(var Msg: TMessage); message WM_SIZE;
    procedure WMMove(var Msg: TMessage); message WM_MOVE;
    procedure WMPosChanged(var Msg: TMessage); message WM_WINDOWPOSCHANGED;
  protected
    procedure CalcScrollBarsRect; virtual;
    procedure DrawHorzScrollBar(DC: HDC); virtual;
    procedure DrawVertScrollBar(DC: HDC); virtual;
    function GetHorzScrollBarSliderRect: TRect;
    function GetVertScrollBarSliderRect: TRect;
    procedure MouseLeave; override;
    procedure PaintScrollBars; virtual;
    function PointInTreeHeader(const P: TPoint): Boolean;
    procedure UpdateScrollBarWindow;
  public
    constructor Create(AControl: TWinControl); override;
    destructor Destroy; override;
  end;


implementation

uses
  System.SysUtils,
  System.Math,
  System.Types,
  Vcl.Graphics,
  VirtualTrees;

type
  TBaseVirtualTreeCracker = class(TBaseVirtualTree)
  end;


// XE2+ VCL Style
{ TVclStyleScrollBarsHook }

procedure TVclStyleScrollBarsHook.CalcScrollBarsRect;
var
  P: TPoint;
  BorderValue: TSize;
  BarInfo: TScrollBarInfo;
  I: Integer;

  procedure CalcVerticalRects;
  begin
    BarInfo.cbSize := SizeOf(BarInfo);
    GetScrollBarInfo(Handle, Integer(OBJID_VSCROLL), BarInfo);
    FVertScrollBarWindow.Visible :=
      not(STATE_SYSTEM_INVISIBLE and BarInfo.rgstate[0] <> 0);
    FVertScrollBarWindow.Enabled :=
      not(STATE_SYSTEM_UNAVAILABLE and BarInfo.rgstate[0] <> 0);
    if FVertScrollBarWindow.Visible then
    begin
      // ScrollBar Rect
      P := BarInfo.rcScrollBar.TopLeft;
      ScreenToClient(Handle, P);
      FVertScrollBarRect.TopLeft := P;
      P := BarInfo.rcScrollBar.BottomRight;
      ScreenToClient(Handle, P);
      FVertScrollBarRect.BottomRight := P;
      OffsetRect(FVertScrollBarRect, BorderValue.cx, BorderValue.cy);

      I := GetSystemMetrics(SM_CYVTHUMB);
      // Down Button
      FVertScrollBarDownButtonRect := FVertScrollBarRect;
      FVertScrollBarDownButtonRect.Top :=
        FVertScrollBarDownButtonRect.Bottom - I;

      // UP Button
      FVertScrollBarUpButtonRect := FVertScrollBarRect;
      FVertScrollBarUpButtonRect.Bottom := FVertScrollBarUpButtonRect.Top + I;

      FVertScrollBarSliderTrackRect := FVertScrollBarRect;
      Inc(FVertScrollBarSliderTrackRect.Top, I);
      Dec(FVertScrollBarSliderTrackRect.Bottom, I);
    end;
  end;

  procedure CalcHorizontalRects;
  begin
    BarInfo.cbSize := SizeOf(BarInfo);
    GetScrollBarInfo(Handle, Integer(OBJID_HSCROLL), BarInfo);
    FHorzScrollBarWindow.Visible :=
      not(STATE_SYSTEM_INVISIBLE and BarInfo.rgstate[0] <> 0);
    FHorzScrollBarWindow.Enabled :=
      not(STATE_SYSTEM_UNAVAILABLE and BarInfo.rgstate[0] <> 0);
    if FHorzScrollBarWindow.Visible then
    begin
      // ScrollBar Rect
      P := BarInfo.rcScrollBar.TopLeft;
      ScreenToClient(Handle, P);
      FHorzScrollBarRect.TopLeft := P;
      P := BarInfo.rcScrollBar.BottomRight;
      ScreenToClient(Handle, P);
      FHorzScrollBarRect.BottomRight := P;
      OffsetRect(FHorzScrollBarRect, BorderValue.cx, BorderValue.cy);

      I := GetSystemMetrics(SM_CXHTHUMB);
      // Down Button
      FHorzScrollBarDownButtonRect := FHorzScrollBarRect;
      FHorzScrollBarDownButtonRect.Left :=
        FHorzScrollBarDownButtonRect.Right - I;

      // UP Button
      FHorzScrollBarUpButtonRect := FHorzScrollBarRect;
      FHorzScrollBarUpButtonRect.Right := FHorzScrollBarUpButtonRect.Left + I;

      FHorzScrollBarSliderTrackRect := FHorzScrollBarRect;
      Inc(FHorzScrollBarSliderTrackRect.Left, I);
      Dec(FHorzScrollBarSliderTrackRect.Right, I);
    end;
  end;

begin
  BorderValue.cx := 0;
  BorderValue.cy := 0;
  if HasBorder then
    if HasClientEdge then
    begin
      BorderValue.cx := GetSystemMetrics(SM_CXEDGE);
      BorderValue.cy := GetSystemMetrics(SM_CYEDGE);
    end;
  CalcVerticalRects;
  CalcHorizontalRects;

end;

constructor TVclStyleScrollBarsHook.Create(AControl: TWinControl);
begin
  inherited;
  FVertScrollBarWindow := TVclStyleScrollBarWindow.CreateParented
    (GetParent(Control.Handle));
  FVertScrollBarWindow.ScrollBarWindowOwner := Self;
  FVertScrollBarWindow.ScrollBarVertical := True;

  FHorzScrollBarWindow := TVclStyleScrollBarWindow.CreateParented
    (GetParent(Control.Handle));
  FHorzScrollBarWindow.ScrollBarWindowOwner := Self;

  FVertScrollBarSliderState := tsThumbBtnVertNormal;
  FVertScrollBarUpButtonState := tsArrowBtnUpNormal;
  FVertScrollBarDownButtonState := tsArrowBtnDownNormal;
  FHorzScrollBarSliderState := tsThumbBtnHorzNormal;
  FHorzScrollBarUpButtonState := tsArrowBtnLeftNormal;
  FHorzScrollBarDownButtonState := tsArrowBtnRightNormal;
end;

destructor TVclStyleScrollBarsHook.Destroy;
begin
  FVertScrollBarWindow.ScrollBarWindowOwner := nil;
  FreeAndNil(FVertScrollBarWindow);
  FHorzScrollBarWindow.ScrollBarWindowOwner := nil;
  FreeAndNil(FHorzScrollBarWindow);
  inherited;
end;

procedure TVclStyleScrollBarsHook.DrawHorzScrollBar(DC: HDC);
var
  B: TBitmap;
  Details: TThemedElementDetails;
  R: TRect;
begin
  if ((Handle = 0) or (DC = 0)) then
    Exit;
  if FHorzScrollBarWindow.Visible and StyleServices.Available and (seBorder in TBaseVirtualTree(Control).StyleElements) then
  begin
    B := TBitmap.Create;
    try
      B.Width := FHorzScrollBarRect.Width;
      B.Height := FHorzScrollBarRect.Height;
      MoveWindowOrg(B.Canvas.Handle, -FHorzScrollBarRect.Left,
        -FHorzScrollBarRect.Top);
      R := FHorzScrollBarRect;
      R.Left := FHorzScrollBarUpButtonRect.Right;
      R.Right := FHorzScrollBarDownButtonRect.Left;

      Details := StyleServices.GetElementDetails(tsUpperTrackHorzNormal);
      StyleServices.DrawElement(B.Canvas.Handle, Details, R);

      if FHorzScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails(FHorzScrollBarSliderState);
      StyleServices.DrawElement(B.Canvas.Handle, Details,
        GetHorzScrollBarSliderRect);

      if FHorzScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails(FHorzScrollBarUpButtonState)
      else
        Details := StyleServices.GetElementDetails(tsArrowBtnLeftDisabled);
      StyleServices.DrawElement(B.Canvas.Handle, Details,
        FHorzScrollBarUpButtonRect);

      if FHorzScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails
          (FHorzScrollBarDownButtonState)
      else
        Details := StyleServices.GetElementDetails(tsArrowBtnRightDisabled);
      StyleServices.DrawElement(B.Canvas.Handle, Details,
        FHorzScrollBarDownButtonRect);

      MoveWindowOrg(B.Canvas.Handle, FHorzScrollBarRect.Left,
        FHorzScrollBarRect.Top);
      with FHorzScrollBarRect do
        BitBlt(DC, Left, Top, B.Width, B.Height, B.Canvas.Handle, 0, 0,
          SRCCOPY);
    finally
      B.Free;
    end;
  end;
end;

procedure TVclStyleScrollBarsHook.DrawVertScrollBar(DC: HDC);
var
  B: TBitmap;
  Details: TThemedElementDetails;
  R: TRect;
begin
  if ((Handle = 0) or (DC = 0)) then
    Exit;
  if FVertScrollBarWindow.Visible and StyleServices.Available and
    (seBorder in TBaseVirtualTree(Control).StyleElements) then
  begin
    B := TBitmap.Create;
    try
      B.Width := FVertScrollBarRect.Width;
      B.Height := FVertScrollBarWindow.Height;
      MoveWindowOrg(B.Canvas.Handle, -FVertScrollBarRect.Left,
        -FVertScrollBarRect.Top);
      R := FVertScrollBarRect;
      R.Bottom := B.Height + FVertScrollBarRect.Top;
      Details := StyleServices.GetElementDetails(tsUpperTrackVertNormal);
      StyleServices.DrawElement(B.Canvas.Handle, Details, R);
      R.Top := FVertScrollBarUpButtonRect.Bottom;
      R.Bottom := FVertScrollBarDownButtonRect.Top;

      Details := StyleServices.GetElementDetails(tsUpperTrackVertNormal);
      StyleServices.DrawElement(B.Canvas.Handle, Details, R);

      if FVertScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails(FVertScrollBarSliderState);
      StyleServices.DrawElement(B.Canvas.Handle, Details,
        GetVertScrollBarSliderRect);

      if FVertScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails(FVertScrollBarUpButtonState)
      else
        Details := StyleServices.GetElementDetails(tsArrowBtnUpDisabled);
      StyleServices.DrawElement(B.Canvas.Handle, Details,
        FVertScrollBarUpButtonRect);

      if FVertScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails
          (FVertScrollBarDownButtonState)
      else
        Details := StyleServices.GetElementDetails(tsArrowBtnDownDisabled);
      StyleServices.DrawElement(B.Canvas.Handle, Details,
        FVertScrollBarDownButtonRect);

      MoveWindowOrg(B.Canvas.Handle, FVertScrollBarRect.Left,
        FVertScrollBarRect.Top);
      with FVertScrollBarRect do
        BitBlt(DC, Left, Top, B.Width, B.Height - TBaseVirtualTreeCracker(Control).BorderWidth, B.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      B.Free;
    end;
  end;
end;

function TVclStyleScrollBarsHook.GetHorzScrollBarSliderRect: TRect;
var
  P: TPoint;
  BarInfo: TScrollBarInfo;
begin
  if FHorzScrollBarWindow.Visible and FHorzScrollBarWindow.Enabled then
  begin
    BarInfo.cbSize := SizeOf(BarInfo);
    GetScrollBarInfo(Handle, Integer(OBJID_HSCROLL), BarInfo);
    P := BarInfo.rcScrollBar.TopLeft;
    ScreenToClient(Handle, P);
    Result.TopLeft := P;
    P := BarInfo.rcScrollBar.BottomRight;
    ScreenToClient(Handle, P);
    Result.BottomRight := P;
    Result.Left := BarInfo.xyThumbTop;
    Result.Right := BarInfo.xyThumbBottom;
    if HasBorder then
      if HasClientEdge then
        OffsetRect(Result, 2, 2)
      else
        OffsetRect(Result, 1, 1);
  end
  else
    Result := Rect(0, 0, 0, 0);
end;

function TVclStyleScrollBarsHook.GetVertScrollBarSliderRect: TRect;
var
  P: TPoint;
  BarInfo: TScrollBarInfo;
begin
  if FVertScrollBarWindow.Visible and FVertScrollBarWindow.Enabled then
  begin
    BarInfo.cbSize := SizeOf(BarInfo);
    GetScrollBarInfo(Handle, Integer(OBJID_VSCROLL), BarInfo);
    P := BarInfo.rcScrollBar.TopLeft;
    ScreenToClient(Handle, P);
    Result.TopLeft := P;
    P := BarInfo.rcScrollBar.BottomRight;
    ScreenToClient(Handle, P);
    Result.BottomRight := P;
    Result.Top := BarInfo.xyThumbTop;
    Result.Bottom := BarInfo.xyThumbBottom;
    if HasBorder then
      if HasClientEdge then
        OffsetRect(Result, 2, 2)
      else
        OffsetRect(Result, 1, 1);
  end
  else
    Result := Rect(0, 0, 0, 0);
end;

procedure TVclStyleScrollBarsHook.MouseLeave;
begin
  inherited;
  if FVertScrollBarSliderState = tsThumbBtnVertHot then
    FVertScrollBarSliderState := tsThumbBtnVertNormal;

  if FHorzScrollBarSliderState = tsThumbBtnHorzHot then
    FHorzScrollBarSliderState := tsThumbBtnHorzNormal;

  if FVertScrollBarUpButtonState = tsArrowBtnUpHot then
    FVertScrollBarUpButtonState := tsArrowBtnUpNormal;

  if FVertScrollBarDownButtonState = tsArrowBtnDownHot then
    FVertScrollBarDownButtonState := tsArrowBtnDownNormal;

  if FHorzScrollBarUpButtonState = tsArrowBtnLeftHot then
    FHorzScrollBarUpButtonState := tsArrowBtnLeftNormal;

  if FHorzScrollBarDownButtonState = tsArrowBtnRightHot then
    FHorzScrollBarDownButtonState := tsArrowBtnRightNormal;

  PaintScrollBars;
end;

procedure TVclStyleScrollBarsHook.PaintScrollBars;
begin
  FVertScrollBarWindow.Repaint;
  FHorzScrollBarWindow.Repaint;
end;

function TVclStyleScrollBarsHook.PointInTreeHeader(const P: TPoint): Boolean;
begin
  Result := TBaseVirtualTreeCracker(Control).Header.InHeader(P);
end;

procedure TVclStyleScrollBarsHook.UpdateScrollBarWindow;
var
  R: TRect;
  Owner: TBaseVirtualTree;
  HeaderHeight: Integer;
  BorderWidth: Integer;
begin
  Owner := TBaseVirtualTree(Control);
  if (hoVisible in TBaseVirtualTreeCracker(Owner).Header.Options) then
    HeaderHeight := TBaseVirtualTreeCracker(Owner).Header.Height
  else
    HeaderHeight := 0;
  BorderWidth := 0;
  // VertScrollBarWindow

  if FVertScrollBarWindow.Visible and (seBorder in TBaseVirtualTree(Control).StyleElements)
  then
  begin
    R := FVertScrollBarRect;
    if Control.BiDiMode = bdRightToLeft then
    begin
      OffsetRect(R, -R.Left, 0);
      if HasBorder then
        OffsetRect(R, GetSystemMetrics(SM_CXEDGE), 0);
    end;
    if HasBorder then
      BorderWidth := GetSystemMetrics(SM_CYEDGE) * 2;
    ShowWindow(FVertScrollBarWindow.Handle, SW_SHOW);
    SetWindowPos(FVertScrollBarWindow.Handle, HWND_TOP, Control.Left + R.Left +
      TBaseVirtualTreeCracker(Control).BorderWidth, Control.Top + R.Top + HeaderHeight
      + TBaseVirtualTreeCracker(Control).BorderWidth, R.Right - R.Left,
      Control.Height - HeaderHeight - BorderWidth - TBaseVirtualTreeCracker(Control)
      .BorderWidth, SWP_SHOWWINDOW);
  end
  else
    ShowWindow(FVertScrollBarWindow.Handle, SW_HIDE);

  // HorzScrollBarWindow
  if FHorzScrollBarWindow.Visible and (seBorder in TBaseVirtualTree(Control).StyleElements)
  then
  begin
    R := FHorzScrollBarRect;
    if Control.BiDiMode = bdRightToLeft then
      OffsetRect(R, FVertScrollBarRect.Width, 0);
    ShowWindow(FHorzScrollBarWindow.Handle, SW_SHOW);
    SetWindowPos(FHorzScrollBarWindow.Handle, HWND_TOP, Control.Left + R.Left +
      TBaseVirtualTreeCracker(Control).BorderWidth, Control.Top + R.Top +
      TBaseVirtualTreeCracker(Control).BorderWidth + HeaderHeight, R.Right - R.Left,
      R.Bottom - R.Top, SWP_SHOWWINDOW);
  end
  else
    ShowWindow(FHorzScrollBarWindow.Handle, SW_HIDE);
end;

procedure TVclStyleScrollBarsHook.WMCaptureChanged(var Msg: TMessage);
begin
  if FVertScrollBarWindow.Visible and FVertScrollBarWindow.Enabled then
  begin
    if FVertScrollBarUpButtonState = tsArrowBtnUpPressed then
    begin
      FVertScrollBarUpButtonState := tsArrowBtnUpNormal;
      PaintScrollBars;
    end;

    if FVertScrollBarDownButtonState = tsArrowBtnDownPressed then
    begin
      FVertScrollBarDownButtonState := tsArrowBtnDownNormal;
      PaintScrollBars;
    end;
  end;

  if FHorzScrollBarWindow.Visible and FHorzScrollBarWindow.Enabled then
  begin
    if FHorzScrollBarUpButtonState = tsArrowBtnLeftPressed then
    begin
      FHorzScrollBarUpButtonState := tsArrowBtnLeftNormal;
      PaintScrollBars;
    end;

    if FHorzScrollBarDownButtonState = tsArrowBtnRightPressed then
    begin
      FHorzScrollBarDownButtonState := tsArrowBtnRightNormal;
      PaintScrollBars;
    end;
  end;

  CallDefaultProc(TMessage(Msg));
  Handled := True;
end;


  procedure TVclStyleScrollBarsHook.WMEraseBkgnd(var Message: TWMEraseBkgnd);
  begin

     Handled := True;
  end;


procedure TVclStyleScrollBarsHook.WMHScroll(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScrollBars;
  Handled := True;
end;

procedure TVclStyleScrollBarsHook.CMUpdateVclStyleScrollbars
  (var Message: TMessage);
begin
  CalcScrollBarsRect;
  PaintScrollBars;
end;

procedure TVclStyleScrollBarsHook.WMKeyDown(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScrollBars;
  Handled := True;
end;

procedure TVclStyleScrollBarsHook.WMKeyUp(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScrollBars;
  Handled := True;
end;

procedure TVclStyleScrollBarsHook.WMLButtonDown(var Msg: TWMMouse);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScrollBars;
  Handled := True;
end;

procedure TVclStyleScrollBarsHook.WMLButtonUp(var Msg: TWMMouse);
var
  P: TPoint;
begin
  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);
  if not PointInTreeHeader(P) then
  begin
    if FVertScrollBarWindow.Visible then
    begin
      if FVertScrollBarSliderState = tsThumbBtnVertPressed then
      begin
        PostMessage(Handle, WM_VSCROLL,
          Integer(SmallPoint(SB_ENDSCROLL, 0)), 0);
        FLeftMouseButtonDown := False;
        FVertScrollBarSliderState := tsThumbBtnVertNormal;
        PaintScrollBars;
        Handled := True;
        ReleaseCapture;
        Exit;
      end;

      if FVertScrollBarUpButtonState = tsArrowBtnUpPressed then
        FVertScrollBarUpButtonState := tsArrowBtnUpNormal;

      if FVertScrollBarDownButtonState = tsArrowBtnDownPressed then
        FVertScrollBarDownButtonState := tsArrowBtnDownNormal;
    end;

    if FHorzScrollBarWindow.Visible then
    begin
      if FHorzScrollBarSliderState = tsThumbBtnHorzPressed then
      begin
        PostMessage(Handle, WM_HSCROLL,
          Integer(SmallPoint(SB_ENDSCROLL, 0)), 0);
        FLeftMouseButtonDown := False;
        FHorzScrollBarSliderState := tsThumbBtnHorzNormal;
        PaintScrollBars;
        Handled := True;
        ReleaseCapture;
        Exit;
      end;

      if FHorzScrollBarUpButtonState = tsArrowBtnLeftPressed then
        FHorzScrollBarUpButtonState := tsArrowBtnLeftNormal;

      if FHorzScrollBarDownButtonState = tsArrowBtnRightPressed then
        FHorzScrollBarDownButtonState := tsArrowBtnRightNormal;
    end;
    PaintScrollBars;
  end;
  FLeftMouseButtonDown := False;
end;

procedure TVclStyleScrollBarsHook.WMMouseMove(var Msg: TWMMouse);
var
  SF: TScrollInfo;
begin
  inherited;
  if FVertScrollBarSliderState = tsThumbBtnVertPressed then
  begin
    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_VERT, SF);
    if SF.nPos <> Round(FScrollPos) then
      FScrollPos := SF.nPos;

    FScrollPos := FScrollPos + (SF.nMax - SF.nMin) *
      ((Mouse.CursorPos.Y - FPrevScrollPos) /
      FVertScrollBarSliderTrackRect.Height);
    if FScrollPos < SF.nMin then
      FScrollPos := SF.nMin;
    if FScrollPos > SF.nMax then
      FScrollPos := SF.nMax;
    if SF.nPage <> 0 then
      if Round(FScrollPos) > SF.nMax - Integer(SF.nPage) + 1 then
        FScrollPos := SF.nMax - Integer(SF.nPage) + 1;
    FPrevScrollPos := Mouse.CursorPos.Y;
    SF.nPos := Round(FScrollPos);

    SetScrollInfo(Handle, SB_VERT, SF, False);
    PostMessage(Handle, WM_VSCROLL, Integer(SmallPoint(SB_THUMBPOSITION,
      Min(Round(FScrollPos), High(SmallInt)))), 0);
    // Min() prevents range check error

    PaintScrollBars;
    Handled := True;
    Exit;
  end;

  if FHorzScrollBarSliderState = tsThumbBtnHorzPressed then
  begin
    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_HORZ, SF);
    if SF.nPos <> Round(FScrollPos) then
      FScrollPos := SF.nPos;

    FScrollPos := FScrollPos + (SF.nMax - SF.nMin) *
      ((Mouse.CursorPos.X - FPrevScrollPos) /
      FHorzScrollBarSliderTrackRect.Width);
    if FScrollPos < SF.nMin then
      FScrollPos := SF.nMin;
    if FScrollPos > SF.nMax then
      FScrollPos := SF.nMax;
    if SF.nPage <> 0 then
      if Round(FScrollPos) > SF.nMax - Integer(SF.nPage) + 1 then
        FScrollPos := SF.nMax - Integer(SF.nPage) + 1;
    FPrevScrollPos := Mouse.CursorPos.X;
    SF.nPos := Round(FScrollPos);

    SetScrollInfo(Handle, SB_HORZ, SF, False);
    PostMessage(Handle, WM_HSCROLL, Integer(SmallPoint(SB_THUMBPOSITION,
      Round(FScrollPos))), 0);

    PaintScrollBars;
    Handled := True;
    Exit;
  end;

  if FHorzScrollBarSliderState = tsThumbBtnHorzHot then
  begin
    FHorzScrollBarSliderState := tsThumbBtnHorzNormal;
    PaintScrollBars;
  end
  else if FVertScrollBarSliderState = tsThumbBtnVertHot then
  begin
    FVertScrollBarSliderState := tsThumbBtnVertNormal;
    PaintScrollBars;
  end
  else if FHorzScrollBarUpButtonState = tsArrowBtnLeftHot then
  begin
    FHorzScrollBarUpButtonState := tsArrowBtnLeftNormal;
    PaintScrollBars;
  end
  else if FHorzScrollBarDownButtonState = tsArrowBtnRightHot then
  begin
    FHorzScrollBarDownButtonState := tsArrowBtnRightNormal;
    PaintScrollBars;
  end
  else if FVertScrollBarUpButtonState = tsArrowBtnUpHot then
  begin
    FVertScrollBarUpButtonState := tsArrowBtnUpNormal;
    PaintScrollBars;
  end
  else if FVertScrollBarDownButtonState = tsArrowBtnDownHot then
  begin
    FVertScrollBarDownButtonState := tsArrowBtnDownNormal;
    PaintScrollBars;
  end;

  CallDefaultProc(TMessage(Msg));
  if FLeftMouseButtonDown then
    PaintScrollBars;
  Handled := True;
end;

procedure TVclStyleScrollBarsHook.WMMouseWheel(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScrollBars;
  Handled := True;
end;

procedure TVclStyleScrollBarsHook.WMNCLButtonDblClk(var Msg: TWMMouse);
begin
  WMNCLButtonDown(Msg);
end;

procedure TVclStyleScrollBarsHook.WMNCLButtonDown(var Msg: TWMMouse);
var
  P: TPoint;
  SF: TScrollInfo;
begin
  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);

  if HasBorder then
    if HasClientEdge then
    begin
      P.X := P.X + 2;
      P.Y := P.Y + 2;
    end
    else
    begin
      P.X := P.X + 1;
      P.Y := P.Y + 1;
    end;

  if not PointInTreeHeader(P) then
  begin
    if FVertScrollBarWindow.Visible then
    begin
      if PtInRect(GetVertScrollBarSliderRect, P) then
      begin
        FLeftMouseButtonDown := True;
        SF.fMask := SIF_ALL;
        SF.cbSize := SizeOf(SF);
        GetScrollInfo(Handle, SB_VERT, SF);
        // FListPos := SF.nPos;
        FScrollPos := SF.nPos;
        FPrevScrollPos := Mouse.CursorPos.Y;
        FVertScrollBarSliderState := tsThumbBtnVertPressed;
        PaintScrollBars;
        SetCapture(Handle);
        Handled := True;
        Exit;
      end;

      if FVertScrollBarWindow.Enabled then
      begin
        if PtInRect(FVertScrollBarDownButtonRect, P) then
          FVertScrollBarDownButtonState := tsArrowBtnDownPressed;
        if PtInRect(FVertScrollBarUpButtonRect, P) then
          FVertScrollBarUpButtonState := tsArrowBtnUpPressed;
      end;
    end;

    if FHorzScrollBarWindow.Visible then
    begin
      if PtInRect(GetHorzScrollBarSliderRect, P) then
      begin
        FLeftMouseButtonDown := True;
        SF.fMask := SIF_ALL;
        SF.cbSize := SizeOf(SF);
        GetScrollInfo(Handle, SB_HORZ, SF);
        // FListPos := SF.nPos;
        FScrollPos := SF.nPos;
        FPrevScrollPos := Mouse.CursorPos.X;
        FHorzScrollBarSliderState := tsThumbBtnHorzPressed;
        PaintScrollBars;
        SetCapture(Handle);
        Handled := True;
        Exit;
      end;

      if FHorzScrollBarWindow.Enabled then
      begin
        if PtInRect(FHorzScrollBarDownButtonRect, P) then
          FHorzScrollBarDownButtonState := tsArrowBtnRightPressed;
        if PtInRect(FHorzScrollBarUpButtonRect, P) then
          FHorzScrollBarUpButtonState := tsArrowBtnLeftPressed;
      end;
    end;
    FLeftMouseButtonDown := True;
    PaintScrollBars;
  end;
end;

procedure TVclStyleScrollBarsHook.WMNCLButtonUp(var Msg: TWMMouse);
var
  P: TPoint;
  B: Boolean;
begin
  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);

  if HasBorder then
    if HasClientEdge then
    begin
      P.X := P.X + 2;
      P.Y := P.Y + 2;
    end
    else
    begin
      P.X := P.X + 1;
      P.Y := P.Y + 1;
    end;

  B := PointInTreeHeader(P);

  if not B then
  begin
    if FVertScrollBarWindow.Visible then
      if FVertScrollBarWindow.Enabled then
      begin
        if FVertScrollBarSliderState = tsThumbBtnVertPressed then
        begin
          FLeftMouseButtonDown := False;
          FVertScrollBarSliderState := tsThumbBtnVertNormal;
          PaintScrollBars;
          Handled := True;
          Exit;
        end;

        if PtInRect(FVertScrollBarDownButtonRect, P) then
          FVertScrollBarDownButtonState := tsArrowBtnDownHot
        else
          FVertScrollBarDownButtonState := tsArrowBtnDownNormal;

        if PtInRect(FVertScrollBarUpButtonRect, P) then
          FVertScrollBarUpButtonState := tsArrowBtnUpHot
        else
          FVertScrollBarUpButtonState := tsArrowBtnUpNormal;
      end;

    if FHorzScrollBarWindow.Visible then
      if FHorzScrollBarWindow.Enabled then
      begin
        if FHorzScrollBarSliderState = tsThumbBtnHorzPressed then
        begin
          FLeftMouseButtonDown := False;
          FHorzScrollBarSliderState := tsThumbBtnHorzNormal;
          PaintScrollBars;
          Handled := True;
          Exit;
        end;

        if PtInRect(FHorzScrollBarDownButtonRect, P) then
          FHorzScrollBarDownButtonState := tsArrowBtnRightHot
        else
          FHorzScrollBarDownButtonState := tsArrowBtnRightNormal;

        if PtInRect(FHorzScrollBarUpButtonRect, P) then
          FHorzScrollBarUpButtonState := tsArrowBtnLeftHot
        else
          FHorzScrollBarUpButtonState := tsArrowBtnLeftNormal;
      end;
    CallDefaultProc(TMessage(Msg));
  end;

  if not B and (FHorzScrollBarWindow.Visible) or (FVertScrollBarWindow.Visible)
  then
    PaintScrollBars;
  Handled := True;
end;

procedure TVclStyleScrollBarsHook.WMNCMouseMove(var Msg: TWMMouse);
var
  P: TPoint;
  MustUpdateScroll: Boolean;
  B: Boolean;
begin
  inherited;
  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);

  if PointInTreeHeader(P) then
  begin
    CallDefaultProc(TMessage(Msg));
    PaintScrollBars;
    Handled := True;
    Exit;
  end;

  if HasBorder then
    if HasClientEdge then
    begin
      P.X := P.X + 2;
      P.Y := P.Y + 2;
    end
    else
    begin
      P.X := P.X + 1;
      P.Y := P.Y + 1;
    end;

  MustUpdateScroll := False;
  if FVertScrollBarWindow.Enabled then
  begin
    B := PtInRect(GetVertScrollBarSliderRect, P);
    if B and (FVertScrollBarSliderState = tsThumbBtnVertNormal) then
    begin
      FVertScrollBarSliderState := tsThumbBtnVertHot;
      MustUpdateScroll := True;
    end
    else if not B and (FVertScrollBarSliderState = tsThumbBtnVertHot) then
    begin
      FVertScrollBarSliderState := tsThumbBtnVertNormal;
      MustUpdateScroll := True;
    end;

    B := PtInRect(FVertScrollBarDownButtonRect, P);
    if B and (FVertScrollBarDownButtonState = tsArrowBtnDownNormal) then
    begin
      FVertScrollBarDownButtonState := tsArrowBtnDownHot;
      MustUpdateScroll := True;
    end
    else if not B and (FVertScrollBarDownButtonState = tsArrowBtnDownHot) then
    begin
      FVertScrollBarDownButtonState := tsArrowBtnDownNormal;
      MustUpdateScroll := True;
    end;
    B := PtInRect(FVertScrollBarUpButtonRect, P);
    if B and (FVertScrollBarUpButtonState = tsArrowBtnUpNormal) then
    begin
      FVertScrollBarUpButtonState := tsArrowBtnUpHot;
      MustUpdateScroll := True;
    end
    else if not B and (FVertScrollBarUpButtonState = tsArrowBtnUpHot) then
    begin
      FVertScrollBarUpButtonState := tsArrowBtnUpNormal;
      MustUpdateScroll := True;
    end;
  end;

  if FHorzScrollBarWindow.Enabled then
  begin
    B := PtInRect(GetHorzScrollBarSliderRect, P);
    if B and (FHorzScrollBarSliderState = tsThumbBtnHorzNormal) then
    begin
      FHorzScrollBarSliderState := tsThumbBtnHorzHot;
      MustUpdateScroll := True;
    end
    else if not B and (FHorzScrollBarSliderState = tsThumbBtnHorzHot) then
    begin
      FHorzScrollBarSliderState := tsThumbBtnHorzNormal;
      MustUpdateScroll := True;
    end;

    B := PtInRect(FHorzScrollBarDownButtonRect, P);
    if B and (FHorzScrollBarDownButtonState = tsArrowBtnRightNormal) then
    begin
      FHorzScrollBarDownButtonState := tsArrowBtnRightHot;
      MustUpdateScroll := True;
    end
    else if not B and (FHorzScrollBarDownButtonState = tsArrowBtnRightHot) then
    begin
      FHorzScrollBarDownButtonState := tsArrowBtnRightNormal;
      MustUpdateScroll := True;
    end;

    B := PtInRect(FHorzScrollBarUpButtonRect, P);
    if B and (FHorzScrollBarUpButtonState = tsArrowBtnLeftNormal) then
    begin
      FHorzScrollBarUpButtonState := tsArrowBtnLeftHot;
      MustUpdateScroll := True;
    end
    else if not B and (FHorzScrollBarUpButtonState = tsArrowBtnLeftHot) then
    begin
      FHorzScrollBarUpButtonState := tsArrowBtnLeftNormal;
      MustUpdateScroll := True;
    end;
  end;

  if MustUpdateScroll then
    PaintScrollBars;
end;

procedure TVclStyleScrollBarsHook.WMNCPaint(var Msg: TMessage);
begin
 if (tsWindowCreating in TBaseVirtualTree(Control).TreeStates) then
  begin
    CalcScrollBarsRect;
    UpdateScrollBarWindow;
 end;
end;

procedure TVclStyleScrollBarsHook.WMSize(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  CalcScrollBarsRect;
  UpdateScrollBarWindow;
  PaintScrollBars;
  Handled := True;
end;

procedure TVclStyleScrollBarsHook.WMMove(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  if not(tsWindowCreating in TBaseVirtualTree(Control).TreeStates) then
  begin
    CalcScrollBarsRect;
    UpdateScrollBarWindow;
    PaintScrollBars;
  end;
  Handled := True;
end;

procedure TVclStyleScrollBarsHook.WMPosChanged(var Msg: TMessage);
begin
  WMMove(Msg);
end;

procedure TVclStyleScrollBarsHook.WMVScroll(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScrollBars;
  Handled := True;
end;

{ TVclStyleScrollBarsHook.TVclStyleScrollBarWindow }

constructor TVclStyleScrollBarsHook.TVclStyleScrollBarWindow.Create
  (AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOverrideStylePaint];
  FScrollBarWindowOwner := nil;
  FScrollBarVertical := False;
  FScrollBarVisible := False;
  FScrollBarEnabled := False;
end;

procedure TVclStyleScrollBarsHook.TVclStyleScrollBarWindow.CreateParams
  (var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_CHILDWINDOW or WS_CLIPCHILDREN or
    WS_CLIPSIBLINGS;
  Params.ExStyle := Params.ExStyle or WS_EX_NOPARENTNOTIFY;
end;

procedure TVclStyleScrollBarsHook.TVclStyleScrollBarWindow.WMEraseBkgnd
  (var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TVclStyleScrollBarsHook.TVclStyleScrollBarWindow.WMNCHitTest
  (var Msg: TWMNCHitTest);
begin
  Msg.Result := HTTRANSPARENT;
end;

procedure TVclStyleScrollBarsHook.TVclStyleScrollBarWindow.WMPaint
  (var Msg: TWMPaint);
var
  PS: TPaintStruct;
  DC: HDC;
begin
  BeginPaint(Handle, PS);
  try
    if FScrollBarWindowOwner <> nil then
    begin
      DC := GetWindowDC(Handle);
      try
        if FScrollBarVertical then
        begin
          MoveWindowOrg(DC, -FScrollBarWindowOwner.FVertScrollBarRect.Left,
            -FScrollBarWindowOwner.FVertScrollBarRect.Top);
          FScrollBarWindowOwner.DrawVertScrollBar(DC);
        end
        else
        begin
          MoveWindowOrg(DC, -FScrollBarWindowOwner.FHorzScrollBarRect.Left,
            -FScrollBarWindowOwner.FHorzScrollBarRect.Top);
          FScrollBarWindowOwner.DrawHorzScrollBar(DC);
        end;
      finally
        ReleaseDC(Handle, DC);
      end;
    end;
  finally
    EndPaint(Handle, PS);
  end;
end;

initialization

  TCustomStyleEngine.RegisterStyleHook(TVirtualStringTree, TVclStyleScrollBarsHook);
  TCustomStyleEngine.RegisterStyleHook(TVirtualDrawTree, TVclStyleScrollBarsHook);

end.

