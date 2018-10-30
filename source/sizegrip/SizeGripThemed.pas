{
  SizeGripThemed.pas

  Delphi component to add a size grip (like if you use a status bar) to the
  lower right corner of any TWinControl (like TForm). "SizeGripThemed.pas"
  is the version using the currently selected visual style. See the included
  README.txt for more information and how to use it.

  This unit has been separated from SizeGrip.pas to avoid linking of
  UxTheme and Themes/ThemeSvr if not necessary.

  Version 1.3 - always find the most current version at
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

unit SizeGripThemed;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SizeGrip;

type
  TSizeGripThemed = class(TSizeGrip)
  private
    FThemed: boolean;
    procedure SetThemed(const Value: boolean);
  protected
    procedure GetGripRect(var Rect: TRect); override;
    procedure PaintIt(DC: HDC; const Rect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Themed: boolean read FThemed write SetThemed default true;
  end;

procedure Register;

implementation

{$I DelphiVersion.inc}

uses
  UxTheme,
{$IFDEF DELPHI_9_UP}
  Themes;
{$ELSE}
  // For Delphi 7 and below you need Mike Lischke's theme manager
  // Get it from http://www.lischke-online.de/ThemeManager.php
  TmSchema, ThemeSvr;
{$ENDIF}

{ TSizeGripThemed }

constructor TSizeGripThemed.Create(AOwner: TComponent);
begin
  FThemed := true;
  inherited;
end;

procedure TSizeGripThemed.SetThemed(const Value: boolean);
begin
  if FThemed <> Value then
  begin
    InvalidateGrip;
    FThemed := Value;
    UpdateGrip;
  end;
end;

{ Calculate the size grip's rectangle
}
procedure TSizeGripThemed.GetGripRect(var Rect: TRect);
var
  DC: HDC;
  Size, Size2: TSize;
begin
  if FThemed and ThemeServices.ThemesEnabled then
  begin
    Size.cx := GetSystemMetrics(SM_CXVSCROLL);
    Size.cy := GetSystemMetrics(SM_CYHSCROLL);

    DC := GetDC(TargetControl.Handle);
    try
      if GetThemePartSize(ThemeServices.Theme[teStatus], DC,
                          SP_GRIPPER, 0, nil, TS_TRUE, Size2) = S_OK then
      begin
        if Size2.cx > Size.cx then
          Size.cx := Size2.cx;
        if Size2.cy > Size.cy then
          Size.cy := Size2.cy;
      end;
    finally
      ReleaseDC(TargetControl.Handle, DC);
    end;

    Rect := TargetControl.ClientRect;
    if TargetControl.UseRightToLeftScrollBar then
      Rect.Right := Rect.Left + Size.cx
    else
      Rect.Left := Rect.Right - Size.cx;
    Rect.Top := Rect.Bottom - Size.cy;
  end
  else
    inherited GetGripRect(Rect);
end;

{ Paint the size grip
}
procedure TSizeGripThemed.PaintIt(DC: HDC; const Rect: TRect);
begin
  if (not FThemed) or
     (not ThemeServices.ThemesEnabled) or
     (DrawThemeBackground(ThemeServices.Theme[teStatus], DC,
                          SP_GRIPPER, 0, Rect, @Rect) <> S_OK) then
    inherited PaintIt(DC, Rect);
end;

{ Register }

procedure Register;
begin
  RegisterComponents('System', [TSizeGripThemed]);
end;

end.
