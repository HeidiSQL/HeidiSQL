// **************************************************************************************************
//
// Unit Vcl.Styles.ColorTabs
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
// The Original Code is Vcl.Styles.ColorTabs
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2012-2019 Rodrigo Ruz V.
// All Rights Reserved.
//
// **************************************************************************************************
unit Vcl.Styles.ColorTabs;

interface

uses
  Winapi.Messages,
  Vcl.Graphics,
  Vcl.ComCtrls;

type
  TTabSheet = class(Vcl.ComCtrls.TTabSheet)
  private
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  end;

  TTabColorControlStyleHook = class(TTabControlStyleHook)
  private
    class var FUseBorder: Boolean;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  protected
    class constructor Create;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure Paint(Canvas: TCanvas); override;
    procedure DrawTab(Canvas: TCanvas; Index: Integer); override;
    class property UseBorder: Boolean read FUseBorder write FUseBorder;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  System.Rtti,
  System.Generics.Collections,
  Winapi.Windows,
  Vcl.Styles,
  Vcl.Themes,
  Vcl.Controls;

type
  TStyleHookList = TList<TStyleHookClass>;

  TPageControlHelper = class helper for TPageControl
  public
    procedure UpdateTab2(Page: Vcl.ComCtrls.TTabSheet);
  end;

  TWinControlClass = class(TWinControl);
  TCustomTabControlClass = class(TCustomTabControl);

  TTabControlStyleHookHelper = class helper for TTabControlStyleHook
  public
    procedure AngleTextOut2(Canvas: TCanvas; Angle: Integer; X, Y: Integer;
      const Text: string);
  end;

  TStyleHookDictionary = TDictionary<TClass, TStyleHookList>;

  TCustomStyleEngineHelper = Class Helper for TCustomStyleEngine
  public
    class function GetRegisteredStyleHooks: TStyleHookDictionary;
  End;

class function TCustomStyleEngineHelper.GetRegisteredStyleHooks
  : TStyleHookDictionary;
var
  p: Pointer;
begin
{$IF (CompilerVersion <31)}
  Result := Self.FRegisteredStyleHooks;
{$ELSE}
  {
    TCustomStyleEngine.FRegisteredStyleHooks:
    00651030 3052AA           xor [edx-$56],dl
    00651033 02F7             add dh,bh
    00651035 097623           or [esi+$23],esi
    TCustomStyleEngine.$ClassInitFlag:
    00651038 FFFF             db $ff $ff
    0065103A FFFF             db $ff $ff
    TCustomStyleEngine.FRegSysStylesList:
    0065103C D037             shl [edi],1
  }
{$IFDEF CPUX64}
  p := Pointer(PByte(@Self.FRegSysStylesList) - 24);
{$ELSE}
  p := Pointer(PByte(@Self.FRegSysStylesList) - 12);
{$ENDIF CPUX64}
  Result := TStyleHookDictionary(p^);
{$IFEND}
end;

function GetBorderColorTab: TColor;
begin
  Result := clBlack;
end;

function GetColorTab(Index: Integer): TColor;
Const
  MaxColors = 9;
  Colors: Array [0 .. MaxColors - 1] of TColor = (6512214, 16755712, 8355381,
    1085522, 115885, 1098495, 1735163, 2248434, 4987610);
begin
  Result := Colors[Index mod MaxColors];
end;

function GetColorTextTab(ThemedTab: TThemedTab): TColor;
Const
  ColorSelected = clYellow;
  ColorHot = clGray;
  ColorNormal = clWhite;
begin
  Result := ColorNormal;
  case ThemedTab of
    ttTabItemSelected, ttTabItemLeftEdgeSelected, ttTabItemBothEdgeSelected,
      ttTabItemRightEdgeSelected:
      Result := ColorSelected;

    ttTabItemHot, ttTabItemLeftEdgeHot, ttTabItemBothEdgeHot,
      ttTabItemRightEdgeHot:
      Result := ColorHot;

    ttTabItemNormal, ttTabItemLeftEdgeNormal, ttTabItemBothEdgeNormal,
      ttTabItemRightEdgeNormal:
      Result := ColorNormal;
  end;
end;

function IsStyleHookRegistered(ControlClass: TClass;
  StyleHookClass: TStyleHookClass): Boolean;
var
  List: TStyleHookList;
begin
  Result := False;
  if TCustomStyleEngine.GetRegisteredStyleHooks.ContainsKey(ControlClass) then
  begin
    List := TCustomStyleEngine.GetRegisteredStyleHooks[ControlClass];
    Result := List.IndexOf(StyleHookClass) <> -1;
  end;
end;

{ TTabSheet }

procedure TTabSheet.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  LRect: TRect;
  LSize: Integer;
  LCanvas: TCanvas;
begin
  // check if the TTabColorControlStyleHook is registered
  if (not IsStyleHookRegistered(TCustomTabControl, TTabColorControlStyleHook))
    and (not IsStyleHookRegistered(TTabControl, TTabColorControlStyleHook)) then
    inherited
  else if (PageControl <> nil) and StyleServices.Enabled and TStyleManager.IsCustomStyleActive
  then
  begin
    GetWindowRect(Handle, LRect);
    OffsetRect(LRect, -LRect.Left, -LRect.Top);
    LSize := ClientToParent(Point(0, 0)).X;
    InflateRect(LRect, LSize, LSize); // remove border
    // create a TCanvas for erase the background, using the DC of the message
    LCanvas := TCanvas.Create;
    try
      LCanvas.Handle := Message.DC;
      LCanvas.Brush.Color := GetColorTab(TabIndex);
      LCanvas.FillRect(LRect);
    finally
      LCanvas.Handle := 0;
      LCanvas.Free;
    end;

    Message.Result := 1;
    if PageControl.ActivePage <> nil then
      PageControl.UpdateTab2(PageControl.ActivePage);
  end
  else
    inherited;
end;

{ TPageControlHelper }

procedure TPageControlHelper.UpdateTab2(Page: Vcl.ComCtrls.TTabSheet);
begin
{$IF (CompilerVersion <31)}
  Self.UpdateTab(Page);
{$ELSE}
  Self.Tabs[Page.TabIndex] := Page.Caption;
{$IFEND}
end;

{ TTabControlStyleHookHelper }

procedure TTabControlStyleHookHelper.AngleTextOut2(Canvas: TCanvas;
  Angle, X, Y: Integer; const Text: string);
var
  LSavedDC: Integer;
begin
  LSavedDC := SaveDC(Canvas.Handle);
  try
    SetBkMode(Canvas.Handle, TRANSPARENT);
    Canvas.Font.Orientation := Angle;
    Canvas.TextOut(X, Y, Text);
  finally
    RestoreDC(Canvas.Handle, LSavedDC);
  end;
end;

{ TTabColorControlStyleHook }
class constructor TTabColorControlStyleHook.Create;
begin
  FUseBorder := True;
end;

procedure TTabColorControlStyleHook.DrawTab(Canvas: TCanvas; Index: Integer);
var
  LDetails: TThemedElementDetails;
  LImageIndex: Integer;
  LThemedTab: TThemedTab;
  LIconRect: TRect;
  R, LayoutR: TRect;
  LImageW, LImageH, DxImage: Integer;
  LTextX, LTextY: Integer;
  LTextColor: TColor;

  procedure DrawControlText(const S: string; var R: TRect; Flags: Cardinal);
  var
    TextFormat: TTextFormatFlags;
  begin
    Canvas.Font := TWinControlClass(Control).Font;
    TextFormat := TTextFormatFlags(Flags);
    Canvas.Font.Color := LTextColor;
    StyleServices.DrawText(Canvas.Handle, LDetails, S, R, TextFormat, Canvas.Font.Color);
  end;

begin
  if (Images <> nil) and (Index < Images.Count) then
  begin
    LImageW := Images.Width;
    LImageH := Images.Height;
    DxImage := 3;
  end
  else
  begin
    LImageW := 0;
    LImageH := 0;
    DxImage := 0;
  end;

  R := TabRect[Index];
  if R.Left < 0 then
    Exit;

  if TabPosition in [tpTop, tpBottom] then
  begin
    if Index = TabIndex then
      InflateRect(R, 0, 2);
  end
  else if Index = TabIndex then
    Dec(R.Left, 2)
  else
    Dec(R.Right, 2);

  Canvas.Font.Assign(TCustomTabControlClass(Control).Font);
  LayoutR := R;
  LThemedTab := ttTabDontCare;
  // Get the type of the active tab
  case TabPosition of
    tpTop:
      begin
        if Index = TabIndex then
          LThemedTab := ttTabItemSelected
        else if (Index = HotTabIndex) and MouseInControl then
          LThemedTab := ttTabItemHot
        else
          LThemedTab := ttTabItemNormal;
      end;
    tpLeft:
      begin
        if Index = TabIndex then
          LThemedTab := ttTabItemLeftEdgeSelected
        else if (Index = HotTabIndex) and MouseInControl then
          LThemedTab := ttTabItemLeftEdgeHot
        else
          LThemedTab := ttTabItemLeftEdgeNormal;
      end;
    tpBottom:
      begin
        if Index = TabIndex then
          LThemedTab := ttTabItemBothEdgeSelected
        else if (Index = HotTabIndex) and MouseInControl then
          LThemedTab := ttTabItemBothEdgeHot
        else
          LThemedTab := ttTabItemBothEdgeNormal;
      end;
    tpRight:
      begin
        if Index = TabIndex then
          LThemedTab := ttTabItemRightEdgeSelected
        else if (Index = HotTabIndex) and MouseInControl then
          LThemedTab := ttTabItemRightEdgeHot
        else
          LThemedTab := ttTabItemRightEdgeNormal;
      end;
  end;

  // draw the tab
  if StyleServices.Available then
  begin
    LDetails := StyleServices.GetElementDetails(LThemedTab);
    // necesary for  DrawControlText

    if FUseBorder then
    begin
      case TabPosition of
        tpTop:
          begin
            InflateRect(R, -1, 0);
            if TabIndex <> Index then
              R.Bottom := R.Bottom + 1
            else
              R.Bottom := R.Bottom - 1;

            Canvas.Brush.Color := GetBorderColorTab;
            Canvas.FillRect(R);

            if TabIndex = Index then
            begin
              InflateRect(R, -1, -1);
              R.Bottom := R.Bottom + 1;
            end
            else
              InflateRect(R, -1, -1);
          end;

        tpBottom:
          begin
            InflateRect(R, -1, 0);
            if TabIndex <> Index then
              R.Bottom := R.Bottom + 1
            else
              R.Top := R.Top + 3;

            Canvas.Brush.Color := GetBorderColorTab;
            Canvas.FillRect(R);

            if TabIndex = Index then
            begin
              InflateRect(R, -1, 0);
              R.Bottom := R.Bottom - 1;
            end
            else
              InflateRect(R, -1, -1);
          end;

        tpLeft:
          begin
            InflateRect(R, 0, -1);

            if TabIndex <> Index then
              R.Left := R.Left + 1
            else
              R.Right := R.Right - 1;

            Canvas.Brush.Color := GetBorderColorTab;
            Canvas.FillRect(R);

            if TabIndex = Index then
            begin
              InflateRect(R, -1, -1);
              R.Right := R.Right + 1;
            end
            else
              InflateRect(R, -1, -1);

          end;

        tpRight:
          begin
            InflateRect(R, 0, -1);

            if TabIndex <> Index then
              // R.Left:=R.Left+1
            else
              R.Left := R.Left + 3;

            Canvas.Brush.Color := GetBorderColorTab;
            Canvas.FillRect(R);

            if TabIndex = Index then
            begin
              InflateRect(R, -1, -1);
              R.Left := R.Left - 1;
            end
            else
              InflateRect(R, -1, -1);

          end;

      end;

      Canvas.Brush.Color := GetColorTab(Index);
      Canvas.FillRect(R);
    end
    else
    Begin
      InflateRect(R, -1, 0);
      // adjust the size of the tab creating blanks space between the tabs
      Canvas.Brush.Color := GetColorTab(Index);
      Canvas.FillRect(R);
    end;

  end;

  // get the index of the image (icon)
  if Control is TCustomTabControl then
    LImageIndex := TCustomTabControlClass(Control).GetImageIndex(Index)
  else
    LImageIndex := Index;

  // draw the image
  if (Images <> nil) and (LImageIndex >= 0) and (LImageIndex < Images.Count)
  then
  begin
    LIconRect := LayoutR;
    case TabPosition of
      tpTop, tpBottom:
        begin
          LIconRect.Left := LIconRect.Left + DxImage;
          LIconRect.Right := LIconRect.Left + LImageW;
          LayoutR.Left := LIconRect.Right;
          LIconRect.Top := LIconRect.Top + (LIconRect.Bottom - LIconRect.Top)
            div 2 - LImageH div 2;
          if (TabPosition = tpTop) and (Index = TabIndex) then
            OffsetRect(LIconRect, 0, -1)
          else if (TabPosition = tpBottom) and (Index = TabIndex) then
            OffsetRect(LIconRect, 0, 1);
        end;
      tpLeft:
        begin
          LIconRect.Bottom := LIconRect.Bottom - DxImage;
          LIconRect.Top := LIconRect.Bottom - LImageH;
          LayoutR.Bottom := LIconRect.Top;
          LIconRect.Left := LIconRect.Left + (LIconRect.Right - LIconRect.Left)
            div 2 - LImageW div 2;
        end;
      tpRight:
        begin
          LIconRect.Top := LIconRect.Top + DxImage;
          LIconRect.Bottom := LIconRect.Top + LImageH;
          LayoutR.Top := LIconRect.Bottom;
          LIconRect.Left := LIconRect.Left + (LIconRect.Right - LIconRect.Left)
            div 2 - LImageW div 2;
        end;
    end;
    if StyleServices.Available then
      StyleServices.DrawIcon(Canvas.Handle, LDetails, LIconRect, Images.Handle,
        LImageIndex);
  end;

  // draw the text of the tab
  if StyleServices.Available then
  begin
    LTextColor := GetColorTextTab(LThemedTab);

    if (TabPosition = tpTop) and (Index = TabIndex) then
      OffsetRect(LayoutR, 0, -1)
    else if (TabPosition = tpBottom) and (Index = TabIndex) then
      OffsetRect(LayoutR, 0, 1);

    if TabPosition = tpLeft then
    begin
      LTextX := LayoutR.Left + (LayoutR.Right - LayoutR.Left) div 2 -
        Canvas.TextHeight(Tabs[Index]) div 2;
      LTextY := LayoutR.Top + (LayoutR.Bottom - LayoutR.Top) div 2 +
        Canvas.TextWidth(Tabs[Index]) div 2;
      Canvas.Font.Color := LTextColor;
      AngleTextOut2(Canvas, 900, LTextX, LTextY, Tabs[Index]);
    end
    else if TabPosition = tpRight then
    begin
      LTextX := LayoutR.Left + (LayoutR.Right - LayoutR.Left) div 2 +
        Canvas.TextHeight(Tabs[Index]) div 2;
      LTextY := LayoutR.Top + (LayoutR.Bottom - LayoutR.Top) div 2 -
        Canvas.TextWidth(Tabs[Index]) div 2;
      Canvas.Font.Color := LTextColor;
      AngleTextOut2(Canvas, -900, LTextX, LTextY, Tabs[Index]);
    end
    else
      DrawControlText(Tabs[Index], LayoutR, DT_VCENTER or DT_CENTER or
        DT_SINGLELINE or DT_NOCLIP);
  end;
end;

procedure TTabColorControlStyleHook.Paint(Canvas: TCanvas);
var
  LRect: TRect;
  LIndex: Integer;
  SavedDC: Integer;
begin
  SavedDC := SaveDC(Canvas.Handle);
  try
    LRect := DisplayRect;
    ExcludeClipRect(Canvas.Handle, LRect.Left, LRect.Top, LRect.Right,
      LRect.Bottom);
    PaintBackground(Canvas);
  finally
    RestoreDC(Canvas.Handle, SavedDC);
  end;

  // Draw tabs , except the active
  for LIndex := 0 to TabCount - 1 do
  begin
    if LIndex = TabIndex then
      Continue;
    DrawTab(Canvas, LIndex);
  end;

  // Draw the body
  case TabPosition of
    tpTop:
      InflateRect(LRect, Control.Width - LRect.Right, Control.Height - LRect.Bottom);
    tpLeft:
      InflateRect(LRect, Control.Width - LRect.Right, Control.Height - LRect.Bottom);
    tpBottom:
      InflateRect(LRect, LRect.Left, LRect.Top);
    tpRight:
      InflateRect(LRect, LRect.Left, LRect.Top);
  end;

  if StyleServices.Available then
  begin
    if FUseBorder then
    begin
      Canvas.Brush.Color := GetBorderColorTab;
      Canvas.Rectangle(LRect.Left, LRect.Top, LRect.Right, LRect.Bottom);

      InflateRect(LRect, -1, -1);
      Canvas.Brush.Color := GetColorTab(TabIndex);
      Canvas.FillRect(LRect);
    end
    else
    begin
      Canvas.Brush.Color := GetColorTab(TabIndex);
      Canvas.FillRect(LRect);
    end;
  end;

  // Draw active tab
  if TabIndex >= 0 then
    DrawTab(Canvas, TabIndex);

  // paint the controls of the tab
  TWinControlClass(Control).PaintControls(Canvas.Handle, nil);
end;

procedure TTabColorControlStyleHook.PaintBackground(Canvas: TCanvas);
var
  LColor: TColor;
begin
  if StyleServices.Available then
  begin

    if Control.Parent is TTabSheet then
      LColor := GetColorTab(TTabSheet(Control.Parent).PageIndex)
    else
      LColor := StyleServices.GetSystemColor(clWindowFrame);

    Canvas.Brush.Color := LColor;
    Canvas.FillRect(Control.ClientRect);
  end;
end;

procedure TTabColorControlStyleHook.WMEraseBkgnd(var Message: TMessage);
var
  LCanvas: TCanvas;
begin
  if (Message.LParam = 1) and StyleServices.Available then
  begin
    LCanvas := TCanvas.Create;
    try
      LCanvas.Handle := HDC(Message.WParam);
      LCanvas.Brush.Color := GetColorTab(TabIndex);
      LCanvas.FillRect(Control.ClientRect);
    finally
      LCanvas.Handle := 0;
      LCanvas.Free;
    end;
  end;
  Message.Result := 1;
  Handled := True;
end;

end.
