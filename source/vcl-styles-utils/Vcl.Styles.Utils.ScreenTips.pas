// **************************************************************************************************
//
// Unit Vcl.Styles.Utils.ScreenTips
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
unit Vcl.Styles.Utils.ScreenTips;

interface

uses
  System.Classes,
  System.SysUtils,
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Themes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Styles.Utils.SysStyleHook,
  Vcl.Forms,
  Vcl.GraphUtil;

type
  TSysTooltipsStyleHook = class(TSysStyleHook)
  private
    procedure WMPaint(var Message: TMessage); message WM_PAINT;

  protected
    procedure Paint(Canvas: TCanvas); override;
    procedure PaintHint(Canvas: TCanvas; TextRect: TRect);
    procedure WndProc(var Message: TMessage); override;

  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;

  end;

implementation

uses
  Winapi.CommCtrl,
  {$IF CompilerVersion >= 30.0}  //DX Seattle and UP.
  Vcl.SysStyles,
  {$IFEND}
  Vcl.Styles.Utils.SysControls;

{ TSysTooltipsStyleHook }
const
  TTM_ADJUSTRECT = WM_USER + 31;

procedure TSysTooltipsStyleHook.PaintHint(Canvas: TCanvas; TextRect: TRect);
var
  DC: HDC;
  LDetails: TThemedElementDetails;
  BkColor, GradientStartColor, GradientEndColor, TextColor, LColor: TColor;
  Brush: HBRUSH;
  AText: PChar;
begin
  DC := Canvas.Handle;
  BkColor := $00767676;
  GradientStartColor := clWhite;
  GradientEndColor := $EFE4E3;
  TextColor := $00575757;

  if StyleServices.Enabled then
  begin
    LDetails := StyleServices.GetElementDetails(thHintBalloon);
    if StyleServices.GetElementColor(LDetails, ecBorderColor, LColor) and (LColor <> clNone) then
      BkColor := LColor;
    if StyleServices.GetElementColor(LDetails, ecGradientColor1, LColor) and (LColor <> clNone) then
      GradientStartColor := LColor;
    if StyleServices.GetElementColor(LDetails, ecGradientColor2, LColor) and (LColor <> clNone) then
      GradientEndColor := LColor;
    if StyleServices.GetElementColor(LDetails, ecTextColor, LColor) and (LColor <> clNone) then
      TextColor := LColor;
  end;
  { Draw Tooltips Face }
  GradientFillCanvas(Canvas, GradientStartColor, GradientEndColor, SysControl.ClientRect, gdVertical);
  { Draw Tooltips Border }
  Brush := CreateSolidBrush(ColorToRGB(BkColor));
  FrameRect(DC, SysControl.ClientRect, Brush);
  DeleteObject(Brush);
  { Use default font for Tooltips text }
  SelectObject(DC, Screen.HintFont.Handle);
  { Draw Tooltips Text }
  SetBkMode(DC, TRANSPARENT);
  SetTextColor(DC, ColorToRGB(TextColor));
  AText := PChar(SysControl.Text);
  Winapi.Windows.DrawText(DC, AText, -1, TextRect, DT_LEFT);
end;

procedure TSysTooltipsStyleHook.WMPaint(var Message: TMessage);
begin
  CallDefaultProc(Message);
  if (GetWindowLong(Handle, GWL_STYLE) and TTS_BALLOON) = TTS_BALLOON then
    Handled := True
  else
    inherited;
end;

procedure TSysTooltipsStyleHook.WndProc(var Message: TMessage);
begin
  inherited;
end;

constructor TSysTooltipsStyleHook.Create(AHandle: THandle);
begin
  inherited;
{$IF CompilerVersion > 23.0}
  StyleElements := [seClient];
{$ELSE}
  OverridePaint := True;
  OverridePaintNC := False;
  OverrideFont := False;
{$IFEND}
end;

destructor TSysTooltipsStyleHook.Destroy;
begin
  inherited;
end;

procedure TSysTooltipsStyleHook.Paint(Canvas: TCanvas);
Var
  TextRect: TRect;
begin
  { Adjust text rectangle }
  TextRect := SysControl.ClientRect;
  SendMessage(Handle, TTM_ADJUSTRECT, 0, UINT_PTR(@TextRect));
  PaintHint(Canvas, TextRect);
end;

initialization

{$IF CompilerVersion >= 30.0}  //DX Seattle and UP.
  TCustomStyleEngine.UnRegisterSysStyleHook('tooltips_class32', Vcl.SysStyles.TSysTooltipsStyleHook);
{$IFEND}


if StyleServices.Available then
  TSysStyleManager.RegisterSysStyleHook(TOOLTIPS_CLASS, TSysTooltipsStyleHook);

finalization
  TSysStyleManager.UnRegisterSysStyleHook(TOOLTIPS_CLASS, TSysTooltipsStyleHook);

end.
