//**************************************************************************************************
//
// Unit Vcl.Styles.DateTimePickers
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
// The Original Code is Vcl.Styles.DateTimePickers
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2012-2023 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************
unit Vcl.Styles.DateTimePickers;

interface

{$IF RTLVersion>=24}
  {$LEGACYIFEND ON}
{$IFEND}
{$IF (CompilerVersion >= 31)}
    {$MESSAGE ERROR 'This unit is deprecated, Use the Vcl.Styles.Hooks unit Instead'}
{$ELSE}
uses
  Winapi.CommCtrl,
  Winapi.Messages,
  Winapi.uxTheme,
  Vcl.Graphics,
  Vcl.ComCtrls,
  Vcl.Controls;

type
 TDateTimePickerStyleHookFix = class(TDateTimePickerStyleHook)
 private
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure WMPaint(var Message: TMessage); message WM_PAINT;
    procedure SetColorsCalendar;
 public
    procedure PaintBackground(Canvas: TCanvas); override;
    constructor Create(AControl: TWinControl); override;
 end deprecated 'Use the Vcl.Styles.Hooks unit Instead';

implementation

uses
  System.SysUtils,
  System.Classes,
  WinApi.Windows,
  Vcl.Styles,
  Vcl.Themes;

type
 TDateTimePickerStyleHookHelper = class helper for TDateTimePickerStyleHook
  private
    function GetDroppedDown: Boolean;
    procedure SetDroppedDown(const Value: Boolean);
    function GetMouseOnButton: Boolean;
    procedure SetMouseOnButton(const Value: Boolean);
 public
    function GetButtonRect_: TRect;
    property _FDroppedDown: Boolean read GetDroppedDown Write SetDroppedDown;
    property _FMouseOnButton: Boolean read GetMouseOnButton Write SetMouseOnButton;
 end;

{ TDateTimePickerStyleHookHelper }
function TDateTimePickerStyleHookHelper.GetButtonRect_: TRect;
begin
 Result:=Self.GetButtonRect;
end;

function TDateTimePickerStyleHookHelper.GetDroppedDown: Boolean;
begin
 Result:=Self.FDroppedDown;
end;

function TDateTimePickerStyleHookHelper.GetMouseOnButton: Boolean;
begin
 Result:=Self.FMouseOnButton;
end;

procedure TDateTimePickerStyleHookHelper.SetDroppedDown(const Value: Boolean);
begin
 Self.FDroppedDown:=Value;
end;

procedure TDateTimePickerStyleHookHelper.SetMouseOnButton(const Value: Boolean);
begin
 Self.FMouseOnButton:=Value;
end;

{ TDateTimePickerStyleHookFix }
procedure TDateTimePickerStyleHookFix.SetColorsCalendar;
Var
  LTextColor, LBackColor: TColor;
  LDateTimePicker: TDateTimePicker;
begin
   LDateTimePicker:=TDateTimePicker(Control);
   //get the vcl styles colors
   LTextColor:=StyleServices.GetSystemColor(clWindowText);
   LBackColor:=StyleServices.GetSystemColor(clWindow);

   LDateTimePicker.Color:=LBackColor;
   //set the colors of the calendar
   LDateTimePicker.CalColors.BackColor:=LBackColor;
   LDateTimePicker.CalColors.MonthBackColor:=LBackColor;
   LDateTimePicker.CalColors.TextColor:=LTextColor;
   LDateTimePicker.CalColors.TitleBackColor:=LBackColor;
   LDateTimePicker.CalColors.TitleTextColor:=LTextColor;
   LDateTimePicker.CalColors.TrailingTextColor:=LTextColor;
end;

procedure TDateTimePickerStyleHookFix.CNNotify(var Message: TWMNotify);
var
  hwnd: WinAPi.Windows.HWND;
begin
  CallDefaultProc(TMessage(Message));
  if Kind = dtkDate then
  with Message, NMHdr^ do
  begin
    Result := 0;
    case code of

      DTN_DROPDOWN:
        begin
          SetColorsCalendar;
          hwnd := SendMessage(TDateTimePicker(Control).Handle, DTM_GETMONTHCAL, 0,0);
          if (Winapi.uxTheme.GetWindowTheme(hwnd)<>0) then
            Winapi.uxTheme.SetWindowTheme(hwnd, '', '');//disable themes in the drop down window

          _FDroppedDown := True;
          RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_UPDATENOW);
          if not TStyleManager.SystemStyle.Enabled then
          begin
            SetRedraw(False);
            SetTimer(Handle, 1, 300, nil);
          end;
        end;

      DTN_CLOSEUP:
        begin
          _FDroppedDown := False;
          _FMouseOnButton := False;
          RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_UPDATENOW);
        end;
    end;
  end;
  Handled := True;
end;


constructor TDateTimePickerStyleHookFix.Create(AControl: TWinControl);
begin
  inherited;
  OverrideEraseBkgnd:=True;//this indicates which this style hook will call the PaintBackground method when the WM_ERASEBKGND message is sent.
end;

procedure TDateTimePickerStyleHookFix.PaintBackground(Canvas: TCanvas);
begin
  //use the proper style color to paint the background
  Canvas.Brush.Color := StyleServices.GetStyleColor(scEdit);
  Canvas.FillRect(Control.ClientRect);
end;

procedure TDateTimePickerStyleHookFix.WMPaint(var Message: TMessage);
var
  DC: HDC;
  LCanvas: TCanvas;
  LPaintStruct: TPaintStruct;
  LRect: TRect;
  LDetails: TThemedElementDetails;
  sDateTime: string;
begin
  DC := Message.WParam;
  LCanvas := TCanvas.Create;
  try
    if DC <> 0 then
      LCanvas.Handle := DC
    else
      LCanvas.Handle := BeginPaint(Control.Handle, LPaintStruct);

    if not TStyleManager.ActiveStyle.IsSystemStyle and  (Winapi.uxTheme.GetWindowTheme(Control.Handle )<>0) then
      Winapi.uxTheme.SetWindowTheme(Control.Handle, '', '');//disable themes in the calendar

      PaintNC(LCanvas);
      Paint(LCanvas);

    if DateMode = dmUpDown then
      LRect := Rect(2, 2, Control.Width - 2, Control.Height - 2)
    else
      LRect := Rect(2, 2, GetButtonRect_.Left, Control.Height - 2);

    if ShowCheckBox then LRect.Left := LRect.Height + 2;
    IntersectClipRect(LCanvas.Handle, LRect.Left, LRect.Top, LRect.Right, LRect.Bottom);
    Message.wParam := WPARAM(LCanvas.Handle);

    //only works for DateFormat = dfShort
    case TDateTimePicker(Control).Kind of
     dtkDate: sDateTime:=DateToStr(TDateTimePicker(Control).DateTime);
     dtkTime: sDateTime:=TimeToStr(TDateTimePicker(Control).DateTime);
    end;

    //draw the current date/time value
    LDetails := StyleServices.GetElementDetails(teEditTextNormal);
    DrawControlText(LCanvas, LDetails, sDateTime, LRect, DT_VCENTER or DT_LEFT);

    if not TStyleManager.SystemStyle.Enabled then
      Paint(LCanvas);

    Message.WParam := DC;
    if DC = 0 then
      EndPaint(Control.Handle, LPaintStruct);
  finally
    LCanvas.Handle := 0;
    LCanvas.Free;
  end;
  Handled := True;
end;
{$IFEND}
end.
