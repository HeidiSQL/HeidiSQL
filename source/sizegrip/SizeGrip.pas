{
  SizeGrip.pas

  Delphi component to add a size grip (like if you use a status bar) to the
  lower right corner of any TWinControl (like TForm). "SizeGripThemed.pas"
  is the themed version using the currently selected visual style. See the
  included README.txt for more information and how to use it.

  Version 1.2b - always find the most current version at
  http://flocke.vssd.de/prog/code/pascal/sizegrip/

  Copyright (C) 2005, 2006 Volker Siebert <flocke@vssd.de>
  All rights reserved.

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation
  the rights to use, copy, modify, merge, publish, distribute, sublicense,
  and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.
}

unit SizeGrip;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, System.Types;

type
  TSizeGripStyle = ( sgsClassic, sgsWinXP );

  TSizeGrip = class(TComponent)
  private
    FTargetControl: TWinControl;    // Target control
    FEnabled: boolean;              // Size grip enabled?
    FStyle: TSizeGripStyle;         // Display style?
    FSizeGripRect: TRect;           // Current size grip rectangle
    FOldWndProc: TWndMethod;        // Hooked window procedure
    procedure AttachControl;
    procedure DetachControl;
    procedure SetTargetControl(const Value: TWinControl);
    procedure SetEnabled(const Value: boolean);
    procedure SetNewStyle(const Value: TSizeGripStyle);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure GetGripRect(var Rect: TRect); virtual;
    procedure PaintIt(DC: HDC; const Rect: TRect); virtual;
    procedure NewWndProc(var Msg: TMessage); virtual;
    procedure InvalidateGrip;
    procedure UpdateGrip;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: boolean read FEnabled write SetEnabled default true;
    property TargetControl: TWinControl read FTargetControl write SetTargetControl;
    property Style: TSizeGripStyle read FStyle write SetNewStyle default sgsClassic;
  end;

  TSizeGripXP = class(TSizeGrip)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Style default sgsWinXP;
  end;

procedure Register;

implementation

type
  TWinControlAccess = class(TWinControl);

const
  CEmptyRect: TRect = ( Left: 0; Top: 0; Right: 0; Bottom: 0; );

{ TSizeGrip }

constructor TSizeGrip.Create(AOwner: TComponent);
begin
  inherited;

  FEnabled := true;
  FStyle := sgsClassic;

  if AOwner.ComponentState * [csLoading, csReading] = [] then
  begin
    // Automatically take the owner as the target control
    if AOwner is TWinControl then
      TargetControl := TWinControl(AOwner)
    else if AOwner is TControl then
      TargetControl := TControl(AOwner).Parent;
  end;
end;

destructor TSizeGrip.Destroy;
begin
  TargetControl := nil;
  inherited;
end;

procedure TSizeGrip.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
    if AComponent = FTargetControl then
      TargetControl := nil;
end;

{ Invalidate the current grip rectangle
}
procedure TSizeGrip.InvalidateGrip;
begin
  if (FTargetControl <> nil) and
     (FSizeGripRect.Right > FSizeGripRect.Left) and
     (FSizeGripRect.Bottom > FSizeGripRect.Top) then
    if FTargetControl.HandleAllocated then
      InvalidateRect(FTargetControl.Handle, @FSizeGripRect, TRUE);
end;

{ Update (and invalidate) the current grip rectangle
}
procedure TSizeGrip.UpdateGrip;
begin
  GetGripRect(FSizeGripRect);
  InvalidateGrip;
end;

{ Attach to FTargetControl: subclass to catch WM_SIZE, WM_ERASEBKGND and
  WM_NCHITTEST.
}
procedure TSizeGrip.AttachControl;
begin
  if @FOldWndProc = nil then
    if ([csDesigning, csDestroying] * ComponentState = []) and
       (FTargetControl <> nil) and
       FEnabled and
       ([csDesigning, csDestroying] * FTargetControl.ComponentState = []) then
    begin
      FOldWndProc := FTargetControl.WindowProc;
      FTargetControl.WindowProc := NewWndProc;
      UpdateGrip;
    end;
end;

{ Detach from FTargetControl: remove subclassing.
}
procedure TSizeGrip.DetachControl;
begin
  if @FOldWndProc <> nil then
  begin
    FTargetControl.WindowProc := FOldWndProc;
    FOldWndProc := nil;

    InvalidateGrip;
    FSizeGripRect := CEmptyRect;
  end;
end;

{ Set the target control
}
procedure TSizeGrip.SetTargetControl(const Value: TWinControl);
begin
  if Value <> FTargetControl then
  begin
    if FTargetControl <> nil then
      FTargetControl.RemoveFreeNotification(Self);

    DetachControl;
    FTargetControl := Value;
    AttachControl;

    if FTargetControl <> nil then
      FTargetControl.FreeNotification(Self);
  end;
end;

{ Toggle enabled / disabled flag
}
procedure TSizeGrip.SetEnabled(const Value: boolean);
begin
  if FEnabled <> Value then
  begin
    DetachControl;
    FEnabled := Value;
    AttachControl;
  end;
end;

{ Toggle new style flag
}
procedure TSizeGrip.SetNewStyle(const Value: TSizeGripStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    InvalidateGrip;
  end;
end;

{ The new Window procedure for the attached target control.
}
procedure TSizeGrip.NewWndProc(var Msg: TMessage);
var
  pt: TPoint;
  dc: HDC;
begin
  if (not Assigned(FOldWndProc)) or (FTargetControl = nil) then
    exit;

  case Msg.Msg of
    WM_PAINT: begin
      FOldWndProc(Msg);
      if TWMPaint(Msg).DC = 0 then
      begin
        dc := GetDC(FTargetControl.Handle);
        try
          PaintIt(dc, FSizeGripRect);
        finally
          ReleaseDC(FTargetControl.Handle, dc);
        end;
      end
    end;

    WM_NCHITTEST: begin
      with TWMNcHitTest(Msg) do
        pt := FTargetControl.ScreenToClient(Point(XPos, YPos));
      if not PtInRect(FSizeGripRect, pt) then
        FOldWndProc(TMessage(Msg))
      else if TargetControl.UseRightToLeftScrollBar then
        Msg.Result := HTBOTTOMLEFT
      else
        Msg.Result := HTBOTTOMRIGHT;
    end;

    WM_SIZE: begin
      InvalidateGrip;
      FOldWndProc(Msg);
      UpdateGrip;
    end;

    else
      FOldWndProc(Msg);
  end;
end;

{ Calculate the size grip's rectangle
}
procedure TSizeGrip.GetGripRect(var Rect: TRect);
begin
  if FTargetControl <> nil then
  begin
    Rect := FTargetControl.ClientRect;
    if TargetControl.UseRightToLeftScrollBar then
      Rect.Right := Rect.Left + 15
    else
      Rect.Left := Rect.Right - 15;
    Rect.Top := Rect.Bottom - 15;
  end
  else
    Rect := CEmptyRect;
end;

{ Paint the size grip
}
procedure TSizeGrip.PaintIt(DC: HDC; const Rect: TRect);
const
  StartX = 4;
  StartY = 4;
var
  ch, cm, cs: COLORREF;

  procedure Paint3(clr: COLORREF; delta: integer);
  var
    pen, oldpen: HPen;
  begin
    pen := CreatePen(PS_SOLID, 0, clr);
    try
      oldpen := SelectObject(DC, pen);
      try
        MoveToEx(DC, Rect.Right - delta, Rect.Bottom - 1, nil);
        LineTo(DC, Rect.Right, Rect.Bottom - 1 - delta);
        inc(delta, 4);
        MoveToEx(DC, Rect.Right - delta, Rect.Bottom - 1, nil);
        LineTo(DC, Rect.Right, Rect.Bottom - 1 - delta);
        inc(delta, 4);
        MoveToEx(DC, Rect.Right - delta, Rect.Bottom - 1, nil);
        LineTo(DC, Rect.Right, Rect.Bottom - 1 - delta);
      finally
        SelectObject(DC, oldpen);
      end;
    finally
      DeleteObject(pen);
    end;
  end;

  procedure PaintBox(x, y: integer);
  begin
    SetPixel(DC, x,     y,     cs);
    SetPixel(DC, x + 1, y,     cs);
    SetPixel(DC, x,     y + 1, cs);
    SetPixel(DC, x + 1, y + 1, cm);
    SetPixel(DC, x + 2, y + 1, ch);
    SetPixel(DC, x + 1, y + 2, ch);
    SetPixel(DC, x + 2, y + 2, ch);
  end;

  function MixColors(c1, c2: COLORREF): COLORREF;
  begin
    Result := RGB((GetRValue(c1) + GetRValue(c2)) div 2,
                  (GetGValue(c1) + GetGValue(c2)) div 2,
                  (GetBValue(c1) + GetBValue(c2)) div 2);
  end;

begin
  ch := ColorToRgb(clBtnHighlight);
  cs := ColorToRgb(clBtnShadow);
  // Original look is cm := cs!
  cm := MixColors(ColorToRgb(TWinControlAccess(FTargetControl).Color), cs);

  case FStyle of
    sgsWinXP: begin
      PaintBox(Rect.Right - StartX,     Rect.Bottom - StartY - 8);
      PaintBox(Rect.Right - StartX - 4, Rect.Bottom - StartY - 4);
      PaintBox(Rect.Right - StartX,     Rect.Bottom - StartY - 4);
      PaintBox(Rect.Right - StartX - 8, Rect.Bottom - StartY);
      PaintBox(Rect.Right - StartX - 4, Rect.Bottom - StartY);
      PaintBox(Rect.Right - StartX,     Rect.Bottom - StartY);
    end;

    else begin
      Paint3(cs, 2);
      Paint3(cm, 3);
      Paint3(ch, 4);
    end;
  end;
end;

{ TSizeGripXP }

constructor TSizeGripXP.Create(AOwner: TComponent);
begin
  inherited;
  FStyle := sgsWinXP;
end;

{ Register }

procedure Register;
begin
  RegisterComponents('System', [TSizeGrip, TSizeGripXP]);
end;

end.
