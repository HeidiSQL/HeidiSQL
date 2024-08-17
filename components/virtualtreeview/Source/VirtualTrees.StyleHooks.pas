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
{$if CompilerVersion < 34}
  {$DEFINE NOT_USE_VCL_STYLEHOOK}  // Do not use inherited style hook but own code in this class. Needed for older Delphi versions 10.3 and below
{$ifend}

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.UxTheme,
  System.Classes,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Themes,
  Vcl.Forms,
  Vcl.Controls;

const
  CM_UPDATE_VCLSTYLE_SCROLLBARS = CM_BASE + 2050;

type
  // XE2+ VCL Style
  TVclStyleScrollBarsHook = class(TScrollingStyleHook)
  {$ifdef NOT_USE_VCL_STYLEHOOK}
  strict private type
  {$REGION 'TVclStyleScrollBarWindow'}
      TScrollWindow = class(TWinControl)
      strict private
        FStyleHook: TVclStyleScrollBarsHook;
        FVertical: Boolean;
        procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
        procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
        procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
      public
        constructor Create(AOwner: TComponent); override;
        property StyleHook: TVclStyleScrollBarsHook read FStyleHook write FStyleHook;
        property Vertical: Boolean read FVertical write FVertical;
      end;
  {$ENDREGION}
  private
    FHorzScrollWnd: TScrollWindow;
    FLeftButtonDown: Boolean;
    FVertScrollWnd: TScrollWindow;

    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMKeyDown(var Msg: TMessage); message WM_KEYDOWN;
    procedure WMKeyUp(var Msg: TMessage); message WM_KEYUP;
    procedure WMLButtonDown(var Msg: TWMMouse);  message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg: TWMMouse); message WM_LBUTTONUP;
    procedure WMNCLButtonDown(var Msg: TWMMouse); message WM_NCLBUTTONDOWN;
    procedure WMNCLButtonDblClk(var Msg: TWMMouse); message WM_NCLBUTTONDBLCLK;
    procedure WMNCLButtonUp(var Msg: TWMMouse); message WM_NCLBUTTONUP;
    procedure WMNCPaint(var Msg: TMessage); message WM_NCPAINT;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMSize(var Msg: TMessage); message WM_SIZE;
    procedure WMMove(var Msg: TMessage); message WM_MOVE;
    procedure WMPosChanged(var Msg: TMessage); message WM_WINDOWPOSCHANGED;
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure WMCaptureChanged(var Msg: TMessage); message WM_CAPTURECHANGED;
    procedure InitScrollBars;
    procedure WMNCMouseMove(var Msg: TWMMouse); message WM_NCMOUSEMOVE;
    procedure WMMouseMove(var Msg: TWMMouse); message WM_MOUSEMOVE;
    function NCMousePosToClient(const P: TPoint): TPoint;
    function PointInTreeHeader(const P: TPoint): Boolean;
    {$endif}
  private
    procedure CMUpdateVclStyleScrollbars(var Msg: TMessage); message CM_UPDATE_VCLSTYLE_SCROLLBARS;
  protected
    procedure CalcScrollBarsRect; virtual;
    procedure UpdateScroll;{$if CompilerVersion >= 34}override;{$ifend}
    {$ifdef NOT_USE_VCL_STYLEHOOK}
    procedure MouseLeave; override;
    procedure DrawHorzScrollBar(DC: HDC); virtual;
    procedure DrawVertScrollBar(DC: HDC); virtual;
    procedure PaintScroll; override;
    property HorzScrollWnd: TScrollWindow read FHorzScrollWnd;
    property VertScrollWnd: TScrollWindow read FVertScrollWnd;
    property LeftButtonDown: Boolean read FLeftButtonDown;
    {$ifend}
  public
    constructor Create(AControl: TWinControl); override;
    {$ifdef NOT_USE_VCL_STYLEHOOK}
    destructor Destroy; override;
    {$ifend}
    /// Draws an expand arrow like used in the RAD Studio IDE.
    /// The code is not yet dpi-aware.
    class procedure DrawExpandArrow(pBitmap: TBitmap; pExpanded: Boolean; pColor: TColor = clNone);
    property HorzScrollRect;
    property VertScrollRect;
  end;

type
  /// prototype for the global callback VTStyleServicesFunc.
  TVTStyleServicesFunc = function (AControl: TControl = nil): TCustomStyleServices;


var
  /// Callback that can be used to assign an alternative function to supply style services.
  /// Needed for IDE plugins. See pull request #1011
  VTStyleServicesFunc: TVTStyleServicesFunc = nil;


/// Wrapper function for styles services that handles differences between RAD Studio 10.4 and older versions,
/// as well as the case if these controls are used inside the IDE.
function VTStyleServices(AControl: TControl = nil): TCustomStyleServices;


implementation

uses
  System.SysUtils,
  System.Math,
  System.Types,
  VirtualTrees.Header,
  VirtualTrees.Types,
  VirtualTrees.BaseTree;

function VTStyleServices(AControl: TControl = nil): TCustomStyleServices;
begin
  if Assigned(VTStyleServicesFunc) then
    Result := VTStyleServicesFunc(AControl)
  else
    Result := Vcl.Themes.StyleServices{$if CompilerVersion >= 34}(AControl){$ifend};
end;

//----------------------------------------------------------------------------------------------------------------------


type
  TBaseVirtualTreeCracker = class(TBaseVirtualTree)
  end;


// XE2+ VCL Style
{ TVclStyleScrollBarsHook }

procedure TVclStyleScrollBarsHook.CalcScrollBarsRect();

  procedure CalcVerticalRects;
  var
    BarInfo: TScrollBarInfo;
    Ret: BOOL;
  begin
    if not Assigned(VertScrollWnd) then // Might happen, when FInitingScrollBars is set, so InitScrollBars did not yet initialize the members
      Exit;

    BarInfo.cbSize := SizeOf(BarInfo);
    Ret := GetScrollBarInfo(Handle, Integer(OBJID_VSCROLL), BarInfo);
    VertScrollWnd.Visible := (seBorder in Control.StyleElements) and Ret and (not (STATE_SYSTEM_INVISIBLE and BarInfo.rgstate[0] <> 0));
    VertScrollWnd.Enabled := VertScrollWnd.Visible and (not (STATE_SYSTEM_UNAVAILABLE and BarInfo.rgstate[0] <> 0));
  end;

  procedure CalcHorizontalRects;
  var
    BarInfo: TScrollBarInfo;
    Ret: BOOL;
  begin
    if not Assigned(HorzScrollWnd) then // Might happen, when FInitingScrollBars is set, so InitScrollBars did not yet initialize the members
      Exit;

    BarInfo.cbSize := SizeOf(BarInfo);
    Ret := GetScrollBarInfo(Handle, Integer(OBJID_HSCROLL), BarInfo);
    HorzScrollWnd.Visible := (seBorder in Control.StyleElements) and Ret and (not (STATE_SYSTEM_INVISIBLE and BarInfo.rgstate[0] <> 0));
    HorzScrollWnd.Enabled := HorzScrollWnd.Visible and (not (STATE_SYSTEM_UNAVAILABLE and BarInfo.rgstate[0] <> 0));
  end;

begin
  CalcVerticalRects;
  CalcHorizontalRects;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TVclStyleScrollBarsHook.Create(AControl: TWinControl);
begin
  inherited;
  {$ifdef NOT_USE_VCL_STYLEHOOK}
  VertSliderState := tsThumbBtnVertNormal;
  VertUpState := tsArrowBtnUpNormal;
  VertDownState := tsArrowBtnDownNormal;
  HorzSliderState := tsThumbBtnHorzNormal;
  HorzUpState := tsArrowBtnLeftNormal;
  HorzDownState := tsArrowBtnRightNormal;
  {$ifend}
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TVclStyleScrollBarsHook.DrawExpandArrow(pBitmap: TBitmap; pExpanded: Boolean; pColor: TColor);
const
  Size: TRect = (Left: 0; Top: 0; Right: 12; Bottom: 12);
  ArrowPoints: array[Boolean, 0..5] of TPoint = (
    ((X:3; Y:1), (X:8; Y:6), (X:3; Y:11), (X:4; Y:11), (X:9; Y:6), (X:3; Y:0)),
    ((X:1; Y:3), (X:6; Y:8), (X:11; Y:3), (X:11; Y:4), (X:6; Y:9), (X:0; Y:3))
  );
var
  canvas: TCanvas;
begin
  pBitmap.SetSize(Size.Width, Size.Height);
  canvas := pBitmap.Canvas;
  canvas.FillRect(Size);
  if pColor = clNone then
  begin
    if Assigned(VTStyleServicesFunc) then
      canvas.Pen.Color := VTStyleServicesFunc.GetSystemColor(clGrayText)
    else
      canvas.Pen.Color := Vcl.Themes.StyleServices.GetSystemColor(clGrayText)
  end
  else
    canvas.Pen.Color := pColor;
  canvas.Pen.Width := 1;
  canvas.Polyline(ArrowPoints[pExpanded]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.UpdateScroll;
var
  R: TRect;
  HeaderHeight: Integer;
  PaddingSize: Integer;
  BorderSize: Integer;
begin
  if VertScrollWnd = nil then
     InitScrollBars();

  HeaderHeight := 0;
  if (hoVisible in TBaseVirtualTree(Control).Header.Options) then
    Inc(HeaderHeight, TBaseVirtualTree(Control).Header.Height);

  PaddingSize := TBaseVirtualTreeCracker(Control).BorderWidth;
  if TBaseVirtualTreeCracker(Control).BevelKind <> bkNone then
  begin
    if TBaseVirtualTreeCracker(Control).BevelInner <> bvNone then
      Inc(PaddingSize, TBaseVirtualTreeCracker(Control).BevelWidth);
    if TBaseVirtualTreeCracker(Control).BevelOuter <> bvNone then
      Inc(PaddingSize, TBaseVirtualTreeCracker(Control).BevelWidth);
  end;

  BorderSize := 0;
  if HasBorder then
    Inc(BorderSize, GetSystemMetrics(SM_CYEDGE));

  if ((VertScrollWnd <> nil) and not VertScrollWnd.HandleAllocated) or
     ((HorzScrollWnd <> nil) and not HorzScrollWnd.HandleAllocated) then
  begin  // Fixes issue #390
    if VertScrollWnd <> nil then
      FreeAndNil({$ifdef NOT_USE_VCL_STYLEHOOK}FVertScrollWnd{$else}VertScrollWnd{$ifend});
    if HorzScrollWnd <> nil then
      FreeAndNil({$ifdef NOT_USE_VCL_STYLEHOOK}FHorzScrollWnd{$else}HorzScrollWnd{$ifend});

    InitScrollBars;
  end;

  // VertScrollBarWindow
  if Control.HandleAllocated then
  begin
    if VertScrollWnd.Visible then
    begin
      R := VertScrollRect;
      if Control.UseRightToLeftScrollBar then
        OffsetRect(R, -R.Left + BorderSize, 0);

      ShowWindow(VertScrollWnd.Handle, SW_SHOW);
      SetWindowPos(VertScrollWnd.Handle, HWND_TOP,
        Control.Left + R.Left + PaddingSize,
        Control.Top + R.Top + HeaderHeight + PaddingSize,
        R.Width,
        Control.Height - HeaderHeight - ((PaddingSize + BorderSize) * 2), // <> R.Height
        SWP_SHOWWINDOW);
    end else
      ShowWindow(VertScrollWnd.Handle, SW_HIDE);
  end;// if FVertScrollWnd

  // HorzScrollBarWindow
  if Control.HandleAllocated then
  begin
    if HorzScrollWnd.Visible then
    begin
      R := HorzScrollRect;
      if Control.UseRightToLeftScrollBar then
        OffsetRect(R, VertScrollRect.Width, 0);

      ShowWindow(HorzScrollWnd.Handle, SW_SHOW);
      SetWindowPos(HorzScrollWnd.Handle, HWND_TOP,
        Control.Left + R.Left + PaddingSize,
        Control.Top + R.Top + HeaderHeight + PaddingSize,
        R.Width, R.Height, SWP_SHOWWINDOW);
    end else
      ShowWindow(HorzScrollWnd.Handle, SW_HIDE);
  end;// if FHorzScrollWnd
  // ScrollBarWindow Visible/Enabled Control
  CalcScrollBarsRect;

end;

procedure TVclStyleScrollBarsHook.CMUpdateVclStyleScrollbars(var Msg: TMessage);
begin
  CalcScrollBarsRect;
  PaintScroll;
end;

//----------------------------------------------------------------------------------------------------------------------

{$ifdef NOT_USE_VCL_STYLEHOOK}

function TVclStyleScrollBarsHook.NCMousePosToClient(const P: TPoint): TPoint;
begin
  Result := P;
  ScreenToClient(Handle, Result);
  if HasBorder then
  begin
    if HasClientEdge then
      Result.Offset(2, 2)
    else
      Result.Offset(1, 1);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVclStyleScrollBarsHook.PointInTreeHeader(const P: TPoint): Boolean;
begin
  Result := TBaseVirtualTree(Control).Header.InHeader(P);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.DrawHorzScrollBar(DC: HDC);
var
  B: TBitmap;
  Details: TThemedElementDetails;
  R: TRect;
begin
  if ((Handle = 0) or (DC = 0)) then
    Exit;

  if HorzScrollWnd.Visible and StyleServices.Available and (seBorder in Control.StyleElements) then
  begin
    B := TBitmap.Create;
    try
      R := HorzScrollRect;
      B.Width := R.Width;
      B.Height := R.Height;
      MoveWindowOrg(B.Canvas.Handle, -R.Left, -R.Top);

      R.Left := HorzUpButtonRect.Right;
      R.Right := HorzDownButtonRect.Left;
      Details := StyleServices.GetElementDetails(tsUpperTrackHorzNormal);
      StyleServices.DrawElement(B.Canvas.Handle, Details, R{$IF CompilerVersion  >= 34}, nil, VertScrollWnd.CurrentPPI{$IFEND});

      if HorzScrollWnd.Enabled then
        Details := StyleServices.GetElementDetails(HorzSliderState);
      StyleServices.DrawElement(B.Canvas.Handle, Details, HorzSliderRect{$IF CompilerVersion  >= 34}, nil, VertScrollWnd.CurrentPPI{$IFEND});

      if HorzScrollWnd.Enabled then
        Details := StyleServices.GetElementDetails(HorzUpState)
      else
        Details := StyleServices.GetElementDetails(tsArrowBtnLeftDisabled);
      StyleServices.DrawElement(B.Canvas.Handle, Details, HorzUpButtonRect{$IF CompilerVersion  >= 34}, nil, VertScrollWnd.CurrentPPI{$IFEND});

      if HorzScrollWnd.Enabled then
        Details := StyleServices.GetElementDetails(HorzDownState)
      else
        Details := StyleServices.GetElementDetails(tsArrowBtnRightDisabled);
      StyleServices.DrawElement(B.Canvas.Handle, Details, HorzDownButtonRect{$IF CompilerVersion  >= 34}, nil, VertScrollWnd.CurrentPPI{$IFEND});

      R := HorzScrollRect;
      MoveWindowOrg(B.Canvas.Handle, R.Left, R.Top);
      BitBlt(DC, R.Left, R.Top, B.Width, B.Height, B.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      B.Free;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.DrawVertScrollBar(DC: HDC);
var
  B: TBitmap;
  Details: TThemedElementDetails;
  R: TRect;
begin
  if ((Handle = 0) or (DC = 0)) then
    Exit;

  if VertScrollWnd.Visible and StyleServices.Available and (seBorder in Control.StyleElements) then
  begin
    B := TBitmap.Create;
    try
      R := VertScrollRect;
      B.Width := R.Width;
      B.Height := VertScrollWnd.Height; // <> R.Height
      MoveWindowOrg(B.Canvas.Handle, -R.Left, -R.Top);

      R.Bottom := B.Height + R.Top;
      Details := StyleServices.GetElementDetails(tsUpperTrackVertNormal);
      StyleServices.DrawElement(B.Canvas.Handle, Details, R {$IF CompilerVersion  >= 34}, nil, VertScrollWnd.CurrentPPI{$IFEND});

      R.Top := VertUpButtonRect.Bottom;
      R.Bottom := VertDownButtonRect.Top;
      Details := StyleServices.GetElementDetails(tsUpperTrackVertNormal);
      StyleServices.DrawElement(B.Canvas.Handle, Details, R{$IF CompilerVersion  >= 34}, nil, VertScrollWnd.CurrentPPI{$IFEND});

      if VertScrollWnd.Enabled then
        Details := StyleServices.GetElementDetails(VertSliderState);
      StyleServices.DrawElement(B.Canvas.Handle, Details, VertSliderRect{$IF CompilerVersion  >= 34}, nil, VertScrollWnd.CurrentPPI{$IFEND});

      if VertScrollWnd.Enabled then
        Details := StyleServices.GetElementDetails(VertUpState)
      else
        Details := StyleServices.GetElementDetails(tsArrowBtnUpDisabled);
      StyleServices.DrawElement(B.Canvas.Handle, Details, VertUpButtonRect{$IF CompilerVersion  >= 34}, nil, VertScrollWnd.CurrentPPI{$IFEND});

      if VertScrollWnd.Enabled then
        Details := StyleServices.GetElementDetails(VertDownState)
      else
        Details := StyleServices.GetElementDetails(tsArrowBtnDownDisabled);
      StyleServices.DrawElement(B.Canvas.Handle, Details, VertDownButtonRect{$IF CompilerVersion  >= 34}, nil, VertScrollWnd.CurrentPPI{$IFEND});

      R := VertScrollRect;
      MoveWindowOrg(B.Canvas.Handle, R.Left, R.Top);
      BitBlt(DC, R.Left, R.Top, B.Width, B.Height, B.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      B.Free;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.MouseLeave;
begin
  inherited;
  if VertSliderState = tsThumbBtnVertHot then
    VertSliderState := tsThumbBtnVertNormal;

  if HorzSliderState = tsThumbBtnHorzHot then
    HorzSliderState := tsThumbBtnHorzNormal;

  if VertUpState = tsArrowBtnUpHot then
    VertUpState := tsArrowBtnUpNormal;

  if VertDownState = tsArrowBtnDownHot then
    VertDownState := tsArrowBtnDownNormal;

  if HorzUpState = tsArrowBtnLeftHot then
    HorzUpState := tsArrowBtnLeftNormal;

  if HorzDownState = tsArrowBtnRightHot then
    HorzDownState := tsArrowBtnRightNormal;

  PaintScroll;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.WMCaptureChanged(var Msg: TMessage);
begin
  if VertScrollWnd.Visible and VertScrollWnd.Enabled then
  begin
    if VertUpState = tsArrowBtnUpPressed then
    begin
      VertUpState := tsArrowBtnUpNormal;
      PaintScroll;
    end;

    if VertDownState = tsArrowBtnDownPressed then
    begin
      VertDownState := tsArrowBtnDownNormal;
      PaintScroll;
    end;
  end;

  if HorzScrollWnd.Visible and HorzScrollWnd.Enabled then
  begin
    if HorzUpState = tsArrowBtnLeftPressed then
    begin
      HorzUpState := tsArrowBtnLeftNormal;
      PaintScroll;
    end;

    if HorzDownState = tsArrowBtnRightPressed then
    begin
      HorzDownState := tsArrowBtnRightNormal;
      PaintScroll;
    end;
  end;

  CallDefaultProc(TMessage(Msg));
  Handled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TVclStyleScrollBarsHook.Destroy;
begin
  FVertScrollWnd.StyleHook := nil;
  FreeAndNil(FVertScrollWnd);
  FHorzScrollWnd.StyleHook := nil;
  FreeAndNil(FHorzScrollWnd);
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Handled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.PaintScroll;
begin
  if FVertScrollWnd.HandleAllocated then
  begin
    FVertScrollWnd.Repaint;
    RedrawWindow(FVertScrollWnd.Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE); // Fixes issue #698
  end;
  if FHorzScrollWnd.HandleAllocated then
  begin
    FHorzScrollWnd.Repaint;
    RedrawWindow(FHorzScrollWnd.Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE); // Fixes issue #698
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.WMKeyDown(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScroll;
  Handled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.WMKeyUp(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScroll;
  Handled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.WMLButtonDown(var Msg: TWMMouse);
begin
  CallDefaultProc(TMessage(Msg));
  UpdateScroll;
  Handled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.InitScrollBars;
begin
  FVertScrollWnd := TScrollWindow.CreateParented(GetParent(Control.Handle));
  FVertScrollWnd.StyleHook := Self;
  FVertScrollWnd.Vertical := True;

  FHorzScrollWnd := TScrollWindow.CreateParented(GetParent(Control.Handle));
  FHorzScrollWnd.StyleHook := Self;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.WMLButtonUp(var Msg: TWMMouse);
var
  P: TPoint;
begin
  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);
  if not PointInTreeHeader(P) then
  begin
    if VertScrollWnd.Visible then
    begin
      if VertSliderState = tsThumbBtnVertPressed then
      begin
        PostMessage(Handle, WM_VSCROLL, WPARAM(UInt32(SmallPoint(SB_ENDSCROLL, 0))), 0);
        FLeftButtonDown := False;
        VertSliderState := tsThumbBtnVertNormal;
        PaintScroll;
        Handled := True;
        Mouse.Capture := 0;
        Exit;
      end else
      if VertUpState = tsArrowBtnUpPressed then
        VertUpState := tsArrowBtnUpNormal
      else if VertDownState = tsArrowBtnDownPressed then
        VertDownState := tsArrowBtnDownNormal;
    end;

    if FHorzScrollWnd.Visible then
    begin
      if HorzSliderState = tsThumbBtnHorzPressed then
      begin
        PostMessage(Handle, WM_HSCROLL, WPARAM(UInt32(SmallPoint(SB_ENDSCROLL, 0))), 0);
        FLeftButtonDown := False;
        HorzSliderState := tsThumbBtnHorzNormal;
        PaintScroll;
        Handled := True;
        Mouse.Capture := 0;
        Exit;
      end else
      if HorzUpState = tsArrowBtnLeftPressed then
        HorzUpState := tsArrowBtnLeftNormal
      else if HorzDownState = tsArrowBtnRightPressed then
        HorzDownState := tsArrowBtnRightNormal;
    end;
    PaintScroll;
  end;
  FLeftButtonDown := False;
end;

//----------------------------------------------------------------------------------------------------------------------
procedure TVclStyleScrollBarsHook.WMNCLButtonDown(var Msg: TWMMouse);
var
  P: TPoint;
  SF: TScrollInfo;
begin
  P := NCMousePosToClient(Point(Msg.XPos, Msg.YPos));
  if not PointInTreeHeader(P) then
  begin
    if VertScrollWnd.Visible and VertScrollWnd.Enabled then
    begin
      if PtInRect(VertSliderRect, P) then
      begin
        FLeftButtonDown := True;
        SF.fMask := SIF_ALL;
        SF.cbSize := SizeOf(SF);
        GetScrollInfo(Handle, SB_VERT, SF);
        ListPos := SF.nPos;
        ScrollPos := SF.nPos;
        PrevScrollPos := Mouse.CursorPos.Y;
        VertSliderState := tsThumbBtnVertPressed;
        PaintScroll;
        Mouse.Capture := Handle;
        Handled := True;
        Exit;
      end else
      if PtInRect(VertDownButtonRect, P) then
        VertDownState := tsArrowBtnDownPressed
      else if PtInRect(VertUpButtonRect, P) then
        VertUpState := tsArrowBtnUpPressed;
    end;

    if FHorzScrollWnd.Visible and FHorzScrollWnd.Enabled then
    begin
      if PtInRect(HorzSliderRect, P) then
      begin
        FLeftButtonDown := True;
        SF.fMask := SIF_ALL;
        SF.cbSize := SizeOf(SF);
        GetScrollInfo(Handle, SB_HORZ, SF);
        ListPos := SF.nPos;
        ScrollPos := SF.nPos;
        PrevScrollPos := Mouse.CursorPos.X;
        HorzSliderState := tsThumbBtnHorzPressed;
        PaintScroll;
        Mouse.Capture := Handle;
        Handled := True;
        Exit;
      end else
      if PtInRect(HorzDownButtonRect, P) then
        HorzDownState := tsArrowBtnRightPressed
      else if PtInRect(HorzUpButtonRect, P) then
        HorzUpState := tsArrowBtnLeftPressed;
    end;
    FLeftButtonDown := True;
    PaintScroll;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.WMNCLButtonUp(var Msg: TWMMouse);
var
  P: TPoint;
begin
  P := NCMousePosToClient(Point(Msg.XPos, Msg.YPos));
  if not PointInTreeHeader(P) then
  begin
    if VertScrollWnd.Visible and VertScrollWnd.Enabled then
    begin
      if VertSliderState = tsThumbBtnVertPressed then
      begin
        FLeftButtonDown := False;
        VertSliderState := tsThumbBtnVertNormal;
        PaintScroll;
        Handled := True;
        Exit;
      end;

      if PtInRect(VertDownButtonRect, P) then
        VertDownState := tsArrowBtnDownHot
      else
        VertDownState := tsArrowBtnDownNormal;

      if PtInRect(VertUpButtonRect, P) then
        VertUpState := tsArrowBtnUpHot
      else
        VertUpState := tsArrowBtnUpNormal;
    end;

    if FHorzScrollWnd.Visible and FHorzScrollWnd.Enabled then
    begin
      if HorzSliderState = tsThumbBtnHorzPressed then
      begin
        FLeftButtonDown := False;
        HorzSliderState := tsThumbBtnHorzNormal;
        PaintScroll;
        Handled := True;
        Exit;
      end;

      if PtInRect(HorzDownButtonRect, P) then
        HorzDownState := tsArrowBtnRightHot
      else
        HorzDownState := tsArrowBtnRightNormal;

      if PtInRect(HorzUpButtonRect, P) then
        HorzUpState := tsArrowBtnLeftHot
      else
        HorzUpState := tsArrowBtnLeftNormal;
    end;

    CallDefaultProc(TMessage(Msg));
    if (FHorzScrollWnd.Visible) or (FVertScrollWnd.Visible) then
      PaintScroll;
  end;

  Handled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.WMNCLButtonDblClk(var Msg: TWMMouse);
begin
  WMNCLButtonDown(Msg);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.WMNCPaint(var Msg: TMessage);
begin
  //if (tsWindowCreating in TBaseVirtualTree(Control).TreeStates) then
  //  UpdateScrollBarWindow;
  //inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.WMHScroll(var Msg: TWMHScroll);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScroll;
  Handled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.WMMouseWheel(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  CalcScrollBarsRect;
  PaintScroll;
  Handled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.WMMouseMove(var Msg: TWMMouse);
var
  SF: TScrollInfo;
  OverrideMax: Integer;
begin
  if VertSliderState = tsThumbBtnVertPressed then
  begin
    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_VERT, SF);

    OverrideMax := SF.nMax;
    if 0 < SF.nPage then
      OverrideMax := SF.nMax - Integer(SF.nPage) + 1;
    ScrollPos := System.Math.EnsureRange(ListPos + (OverrideMax - SF.nMin) * ((Mouse.CursorPos.Y - PrevScrollPos) / (VertTrackRect.Height - VertSliderRect.Height)),
                                         SF.nMin, OverrideMax);
    SF.fMask := SIF_POS;
    SF.nPos := Round(ScrollPos);
    SetScrollInfo(Handle, SB_VERT, SF, False);
    PostMessage(Handle, WM_VSCROLL, WPARAM(UInt32(SmallPoint(SB_THUMBPOSITION, Min(SF.nPos, High(SmallInt))))), 0);

    PaintScroll;
    Handled := True;
    Exit;
  end else
  if VertSliderState = tsThumbBtnVertHot then
  begin
    VertSliderState := tsThumbBtnVertNormal;
    PaintScroll;
  end;

  if HorzSliderState = tsThumbBtnHorzPressed then
  begin
    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_HORZ, SF);

    OverrideMax := SF.nMax;
    if 0 < SF.nPage then
      OverrideMax := SF.nMax - Integer(SF.nPage) + 1;
    ScrollPos := System.Math.EnsureRange(ListPos + (OverrideMax - SF.nMin) * ((Mouse.CursorPos.X - PrevScrollPos) / (HorzTrackRect.Width - HorzSliderRect.Width)),
                                         SF.nMin, OverrideMax);
    SF.fMask := SIF_POS;
    SF.nPos := Round(ScrollPos);
    SetScrollInfo(Handle, SB_HORZ, SF, False);
    PostMessage(Handle, WM_HSCROLL, WPARAM(UInt32(SmallPoint(SB_THUMBPOSITION, Min(SF.nPos, High(SmallInt))))), 0);

    PaintScroll;
    Handled := True;
    Exit;
  end else
  if HorzSliderState = tsThumbBtnHorzHot then
  begin
    HorzSliderState := tsThumbBtnHorzNormal;
    PaintScroll;
  end;

  if (HorzUpState <> tsArrowBtnLeftPressed) and (HorzUpState = tsArrowBtnLeftHot) then
  begin
    HorzUpState := tsArrowBtnLeftNormal;
    PaintScroll;
  end;

  if (HorzDownState <> tsArrowBtnRightPressed) and (HorzDownState = tsArrowBtnRightHot) then
  begin
    HorzDownState := tsArrowBtnRightNormal;
    PaintScroll;
  end;

  if (VertUpState <> tsArrowBtnUpPressed) and (VertUpState = tsArrowBtnUpHot) then
  begin
    VertUpState := tsArrowBtnUpNormal;
    PaintScroll;
  end;

  if (VertDownState <> tsArrowBtnDownPressed) and (VertDownState = tsArrowBtnDownHot) then
  begin
    VertDownState := tsArrowBtnDownNormal;
    PaintScroll;
  end;

  CallDefaultProc(TMessage(Msg));
  if LeftButtonDown then
    PaintScroll;
  Handled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.WMNCMouseMove(var Msg: TWMMouse);
var
  P: TPoint;
  MustUpdateScroll: Boolean;
  B: Boolean;
begin
  inherited;
  P := NCMousePosToClient(Point(Msg.XPos, Msg.YPos));
  if PointInTreeHeader(P) then
  begin
    CallDefaultProc(TMessage(Msg));
    PaintScroll;
    Handled := True;
    Exit;
  end;

  MustUpdateScroll := False;
  if VertScrollWnd.Visible and VertScrollWnd.Enabled then
  begin
    B := PtInRect(VertSliderRect, P);
    if B and (VertSliderState = tsThumbBtnVertNormal) then
    begin
      VertSliderState := tsThumbBtnVertHot;
      MustUpdateScroll := True;
    end else
    if not B and (VertSliderState = tsThumbBtnVertHot) then
    begin
      VertSliderState := tsThumbBtnVertNormal;
      MustUpdateScroll := True;
    end;

    B := PtInRect(VertDownButtonRect, P);
    if B and (VertDownState = tsArrowBtnDownNormal) then
    begin
      VertDownState := tsArrowBtnDownHot;
      MustUpdateScroll := True;
    end else
    if not B and (VertDownState = tsArrowBtnDownHot) then
    begin
      VertDownState := tsArrowBtnDownNormal;
      MustUpdateScroll := True;
    end;

    B := PtInRect(VertUpButtonRect, P);
    if B and (VertUpState = tsArrowBtnUpNormal) then
    begin
      VertUpState := tsArrowBtnUpHot;
      MustUpdateScroll := True;
    end else
    if not B and (VertUpState = tsArrowBtnUpHot) then
    begin
      VertUpState := tsArrowBtnUpNormal;
      MustUpdateScroll := True;
    end;
  end;

  if HorzScrollWnd.Visible and HorzScrollWnd.Enabled then
  begin
    B := PtInRect(HorzSliderRect, P);
    if B and (HorzSliderState = tsThumbBtnHorzNormal) then
    begin
      HorzSliderState := tsThumbBtnHorzHot;
      MustUpdateScroll := True;
    end else
    if not B and (HorzSliderState = tsThumbBtnHorzHot) then
    begin
      HorzSliderState := tsThumbBtnHorzNormal;
      MustUpdateScroll := True;
    end;

    B := PtInRect(HorzDownButtonRect, P);
    if B and (HorzDownState = tsArrowBtnRightNormal) then
    begin
      HorzDownState := tsArrowBtnRightHot;
      MustUpdateScroll := True;
    end else
    if not B and (HorzDownState = tsArrowBtnRightHot) then
    begin
      HorzDownState := tsArrowBtnRightNormal;
      MustUpdateScroll := True;
    end;

    B := PtInRect(HorzUpButtonRect, P);
    if B and (HorzUpState = tsArrowBtnLeftNormal) then
    begin
      HorzUpState := tsArrowBtnLeftHot;
      MustUpdateScroll := True;
    end else
    if not B and (HorzUpState = tsArrowBtnLeftHot) then
    begin
      HorzUpState := tsArrowBtnLeftNormal;
      MustUpdateScroll := True;
    end;
  end;

  if MustUpdateScroll then
    PaintScroll;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.WMSize(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  UpdateScroll;
  PaintScroll;
  Handled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.WMMove(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  if not(tsWindowCreating in TBaseVirtualTree(Control).TreeStates) then
  begin
    UpdateScroll;
    PaintScroll;
  end;
  Handled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.WMPosChanged(var Msg: TMessage);
begin
  WMMove(Msg);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.WMVScroll(var Msg: TWMVScroll);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScroll;
  Handled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TVclStyleScrollBarsHook.TVclStyleScrollBarWindow }

constructor TVclStyleScrollBarsHook.TScrollWindow.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOverrideStylePaint];
  FStyleHook := nil;
  FVertical := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.TScrollWindow.WMEraseBkgnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.TScrollWindow.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  Msg.Result := HTTRANSPARENT;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVclStyleScrollBarsHook.TScrollWindow.WMPaint(var Msg: TWMPaint);
var
  PS: TPaintStruct;
  DC: HDC;
  R: TRect;
begin
  BeginPaint(Handle, PS);
  try
    if FStyleHook <> nil then
    begin
      DC := GetWindowDC(Handle);
      try
        if FVertical then
        begin
          R := FStyleHook.VertScrollRect;
          MoveWindowOrg(DC, -R.Left, -R.Top);
          FStyleHook.DrawVertScrollBar(DC);
        end else
        begin
          R := FStyleHook.HorzScrollRect;
          MoveWindowOrg(DC, -R.Left, -R.Top);
          FStyleHook.DrawHorzScrollBar(DC);
        end;
      finally
        ReleaseDC(Handle, DC);
      end;
    end;
  finally
    EndPaint(Handle, PS);
  end;
end;
{$ifend}

end.
