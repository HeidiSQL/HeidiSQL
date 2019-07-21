//**************************************************************************************************
//                                                                                                  
// Unit Vcl.PlatformVclStylesActnCtrls                                                              
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
// The Original Code is Vcl.PlatformVclStylesActnCtrls                                              
//                                                                                                  
// The Initial Developer of the Original Code is Rodrigo Ruz V.                                     
// Portions created by Rodrigo Ruz V. are Copyright (C) 2012-2019 Rodrigo Ruz V.
// All Rights Reserved.                                                                             
//                                                                                                  
//**************************************************************************************************

unit Vcl.PlatformVclStylesActnCtrls;

interface

uses
   Vcl.ActnMan,
   Vcl.Buttons,
   Vcl.PlatformDefaultStyleActnCtrls;

type
  TPlatformVclStylesStyle = class(TPlatformDefaultStyleActionBars)
  public
    function GetControlClass(ActionBar: TCustomActionBar; AnItem: TActionClientItem): TCustomActionControlClass; override;
    function GetStyleName: string; override;
  end;

var
  PlatformVclStylesStyle: TPlatformVclStylesStyle;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,
  Winapi.UxTheme,
  Winapi.Windows,
  Vcl.Menus,
  Vcl.ActnMenus,
  Vcl.ActnCtrls,
  Vcl.ThemedActnCtrls,
  Vcl.Forms,
  Vcl.ListActns,
  Vcl.ActnColorMaps,
  Vcl.Themes,
  Vcl.XPActnCtrls,
  Vcl.StdActnMenus,
  Vcl.Graphics;

type
  TActionControlStyle = (csStandard, csXPStyle, csThemed);

  TThemedMenuItemEx = class(Vcl.ThemedActnCtrls.TThemedMenuItem)
  private
    procedure NativeDrawText(DC: HDC; const Text: string; var Rect: TRect; Flags: Longint);
  protected
    procedure DrawText(var Rect: TRect; var Flags: Cardinal; Text: string); override;
  end;

  TThemedMenuButtonEx = class(Vcl.ThemedActnCtrls.TThemedMenuButton)
  private
    procedure NativeDrawText(const Text: string; var Rect: TRect; Flags: Longint);
  protected
    procedure DrawBackground(var PaintRect: TRect); override;
    procedure DrawText(var ARect: TRect; var Flags: Cardinal; Text: string); override;
  end;

  TThemedMenuItemHelper = class Helper for TThemedMenuItem
  private
   function GetPaintRect: TRect;
   property PaintRect: TRect read GetPaintRect;
  end;

  TThemedButtonControlEx = class(TThemedButtonControl)
  protected
    procedure DrawBackground(var PaintRect: TRect); override;
  end;

  TThemedDropDownButtonEx= class(TThemedDropDownButton)
  protected
    procedure DrawBackground(var PaintRect: TRect); override;
  end;

 {$IF (CompilerVersion >=31))}
 {$HINTS OFF}
  TShadowClassThemedMenuItem = class(TCustomMenuItem)
  private
    FCheckRect: TRect;
    FGutterRect: TRect;
    FPaintRect: TRect;
  end;
 {$HINTS ON}
 {$IFEND}

function DoDrawText(DC: HDC; Details: TThemedElementDetails;
  const S: string; var R: TRect; Flags: TTextFormat; Options: TStyleTextOptions): Boolean;
var
  LFlags: Cardinal;
  LColorRef: TColorRef;
begin
    LFlags := TTextFormatFlags(Flags);
    LColorRef := SetTextColor(DC, Vcl.Graphics.ColorToRGB(Options.TextColor));
    try
      Winapi.Windows.DrawText(DC, PChar(S), Length(S), R, LFlags);
    finally
      SetTextColor(DC, LColorRef);
    end;
    Result := True;
end;

function InternalDrawText(DC: HDC; Details: TThemedElementDetails; const S: string; var R: TRect; Flags: TTextFormat; Color: TColor = clNone): Boolean;
var
  //LColor: TColor;
  LOptions: TStyleTextOptions;
begin
  if Color <> clNone then
  begin
    LOptions.Flags := [stfTextColor];
    LOptions.TextColor := Color;
  end
  else
    LOptions.Flags := [];
  Result := DoDrawText(DC, Details, S, R, Flags, LOptions);
end;



{ TThemedMenuItemHelper }
function TThemedMenuItemHelper.GetPaintRect: TRect;
begin
 {$IF (CompilerVersion <31))}
 Result := Self.FPaintRect;
 {$ELSE}
  Result := TShadowClassThemedMenuItem(Self).FPaintRect;
 {$IFEND}
end;

function GetActionControlStyle: TActionControlStyle;
begin
  if TStyleManager.IsCustomStyleActive then
    Result := csThemed
  else
  if TOSVersion.Check(6) then
  begin
    if StyleServices.Theme[teMenu] <> 0 then
      Result := csThemed
    else
      Result := csXPStyle;
  end
  else
  if TOSVersion.Check(5, 1) then
    Result := csXPStyle
  else
    Result := csStandard;
end;

{ TPlatformDefaultStyleActionBarsStyle }

function TPlatformVclStylesStyle.GetControlClass(ActionBar: TCustomActionBar;
  AnItem: TActionClientItem): TCustomActionControlClass;
begin
  if ActionBar is TCustomActionToolBar then
  begin
    if AnItem.HasItems then
      case GetActionControlStyle of
        csStandard: Result := TStandardDropDownButton;
        csXPStyle: Result := TXPStyleDropDownBtn;
      else
        Result := TThemedDropDownButtonEx;
      end
    else
    if (AnItem.Action is TStaticListAction) or (AnItem.Action is TVirtualListAction) then
      Result := TCustomComboControl
    else
    case GetActionControlStyle of
      csStandard: Result := TStandardButtonControl;
      csXPStyle: Result := TXPStyleButton;
    else
      Result := TThemedButtonControlEx;
    end
  end
  else
  if ActionBar is TCustomActionMainMenuBar then
    case GetActionControlStyle of
      csStandard: Result := TStandardMenuButton;
      csXPStyle: Result := TXPStyleMenuButton;
    else
      Result := TThemedMenuButtonEx;
    end
  else
  if ActionBar is TCustomizeActionToolBar then
  begin
    with TCustomizeActionToolbar(ActionBar) do
      if not Assigned(RootMenu) or (AnItem.ParentItem <> TCustomizeActionToolBar(RootMenu).AdditionalItem) then
        case GetActionControlStyle of
          csStandard: Result := TStandardMenuItem;
          csXPStyle: Result := TXPStyleMenuItem;
        else
          Result := TThemedMenuItemEx;
        end
      else
      case GetActionControlStyle of
          csStandard: Result := TStandardAddRemoveItem;
          csXPStyle: Result := TXPStyleAddRemoveItem;
      else
          Result := TThemedAddRemoveItem;
      end
  end
  else
  if ActionBar is TCustomActionPopupMenu then
    case GetActionControlStyle of
      csStandard: Result := TStandardMenuItem;
      csXPStyle: Result := TXPStyleMenuItem;
    else
      Result := TThemedMenuItemEx;
    end
  else
  case GetActionControlStyle of
    csStandard: Result := TStandardButtonControl;
    csXPStyle: Result := TXPStyleButton;
  else
    Result := TThemedButtonControl;
  end
end;

function TPlatformVclStylesStyle.GetStyleName: string;
begin
  Result := 'Platform VclStyles Style';
end;

{ TThemedMenuItemEx }

procedure TThemedMenuItemEx.NativeDrawText(DC: HDC; const Text: string;
  var Rect: TRect; Flags: Integer);
const
  MenuStates: array[Boolean] of TThemedMenu = (tmPopupItemDisabled, tmPopupItemNormal);
var
  LCaption: string;
  LFormats: TTextFormat;
  LColor: TColor;
  LDetails: TThemedElementDetails;
begin
  LFormats := TTextFormatFlags(Flags);
  if Selected and Enabled then
  begin
    LDetails := StyleServices.GetElementDetails(tmPopupItemHot);
    if TOSVersion.Check(5, 1) then
     SetBkMode(DC, Winapi.Windows.TRANSPARENT);
  end
  else
    LDetails := StyleServices.GetElementDetails(MenuStates[Enabled or ActionBar.DesignMode]);

  if not StyleServices.GetElementColor(LDetails, ecTextColor, LColor) or (LColor = clNone) then
    LColor := ActionBar.ColorMap.FontColor;

  LCaption := Text;
  if (tfCalcRect in LFormats) and ( (LCaption = '') or (LCaption[1] = cHotkeyPrefix) and (LCaption[2] = #0) ) then
    LCaption := LCaption + ' ';


  //LNativeStyle.DrawText(DC, LDetails, LCaption, Rect, LFormats, LColor);   //doesn't work when the windows classic theme is applied in the OS
  //StyleServices.DrawText(DC, LDetails, LCaption, Rect, LFormats, LColor); //doesn't work with custom fonts sizes and types
  InternalDrawText(DC, LDetails, LCaption, Rect, LFormats, LColor);
end;

procedure TThemedMenuItemEx.DrawText(var Rect: TRect; var Flags: Cardinal;
  Text: string);
var
  LRect: TRect;
begin
  if Selected and Enabled then
    StyleServices.DrawElement(Canvas.Handle, StyleServices.GetElementDetails(tmPopupItemHot), PaintRect)
  else if Selected then
    StyleServices.DrawElement(Canvas.Handle, StyleServices.GetElementDetails(tmPopupItemDisabledHot), PaintRect);

  if (Parent is TCustomActionBar) and (not ActionBar.PersistentHotkeys) then
    Text := FNoPrefix;
  Canvas.Font := Screen.MenuFont;

  if ActionClient.Default then
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];

  LRect := PaintRect;
  NativeDrawText(Canvas.Handle, Text, LRect, Flags or DT_CALCRECT or DT_NOCLIP);
  OffsetRect(LRect, Rect.Left,
    ((PaintRect.Bottom - PaintRect.Top) - (LRect.Bottom - LRect.Top)) div 2);
  NativeDrawText(Canvas.Handle, Text, LRect, Flags);

  if ShowShortCut and ((ActionClient <> nil) and not ActionClient.HasItems) then
  begin
    Flags := DrawTextBiDiModeFlags(DT_RIGHT);
    LRect := TRect.Create(ShortCutBounds.Left, LRect.Top, ShortCutBounds.Right, LRect.Bottom);
    LRect.Offset(Width, 0);
    NativeDrawText(Canvas.Handle, ActionClient.ShortCutText, LRect, Flags);
  end;
end;

{ TThemedMenuButtonEx }
procedure TThemedMenuButtonEx.NativeDrawText(const Text: string; var Rect: TRect;
  Flags: Integer);
const
  MenuStates: array[Boolean] of TThemedMenu = (tmMenuBarItemNormal, tmMenuBarItemHot);
var
  LCaption: string;
  LFormats: TTextFormat;
  LColor: TColor;
  LDetails: TThemedElementDetails;
begin
  LFormats := TTextFormatFlags(Flags);
  if Enabled then
    LDetails := StyleServices.GetElementDetails(MenuStates[Selected or MouseInControl or ActionBar.DesignMode])
  else
    LDetails := StyleServices.GetElementDetails(tmMenuBarItemDisabled);

  Canvas.Brush.Style := bsClear;
  if Selected then
    Canvas.Font.Color := clHighlightText
  else
    Canvas.Font.Color := clMenuText;

  if not StyleServices.GetElementColor(LDetails, ecTextColor, LColor) or (LColor = clNone) then
    LColor := ActionBar.ColorMap.FontColor;

  LCaption := Text;
  if (tfCalcRect in LFormats) and ( (LCaption = '') or (LCaption[1] = cHotkeyPrefix) and (LCaption[2] = #0) ) then
    LCaption := LCaption + ' ';

  if Enabled then
    LDetails := StyleServices.GetElementDetails(MenuStates[Selected or MouseInControl]);

  //LNativeStyle.DrawText(Canvas.Handle, LDetails, LCaption, Rect, LFormats, LColor);
  InternalDrawText(Canvas.Handle, LDetails, LCaption, Rect, LFormats, LColor);
end;

procedure TThemedMenuButtonEx.DrawBackground(var PaintRect: TRect);
const
  MenuStates: array[Boolean, Boolean] of TThemedMenu =
    ((tmMenuBarItemNormal, tmMenuBarItemPushed), (tmMenuBarItemHot, tmMenuBarItemPushed));
begin
  Canvas.Brush.Color := ActionBar.ColorMap.Color;
  StyleServices.DrawElement(Canvas.Handle, StyleServices.GetElementDetails(MenuStates[MouseInControl, Selected]), PaintRect);
end;

procedure TThemedMenuButtonEx.DrawText(var ARect: TRect; var Flags: Cardinal;
  Text: string);
var
  LRect: TRect;
begin
  if Parent is TCustomActionMainMenuBar then
    if not TCustomActionMainMenuBar(Parent).PersistentHotkeys then
      Text := StripHotkey(Text);
  LRect := ARect;
  Inc(LRect.Left);
  Canvas.Font := Screen.MenuFont;
  NativeDrawText(Text, LRect, Flags or DT_CALCRECT or DT_NOCLIP);
  NativeDrawText(Text, LRect, Flags);
end;

{ TThemedButtonControlEx }

procedure TThemedButtonControlEx.DrawBackground(var PaintRect: TRect);
const
  DisabledState: array[Boolean] of TThemedToolBar = (ttbButtonDisabled, ttbButtonPressed);
  CheckedState: array[Boolean] of TThemedToolBar = (ttbButtonHot, ttbButtonCheckedHot);
var
  SaveIndex: Integer;
begin
  if not StyleServices.IsSystemStyle and ActionClient.Separator then Exit;

  SaveIndex := SaveDC(Canvas.Handle);
  try
    if Enabled and not (ActionBar.DesignMode) then
    begin
      if (MouseInControl or IsChecked) and
         Assigned(ActionClient) {and not ActionClient.Separator)} then
      begin
        StyleServices.DrawElement(Canvas.Handle, StyleServices.GetElementDetails(CheckedState[IsChecked or (FState = bsDown)]), PaintRect);

        if not MouseInControl then
          ;//StyleServices.DrawElement(Canvas.Handle, StyleServices.GetElementDetails(ttbButtonPressed), PaintRect);
      end
      else
        ;//StyleServices.DrawElement(Canvas.Handle, StyleServices.GetElementDetails(ttbButtonNormal), PaintRect);
    end
    else
      StyleServices.DrawElement(Canvas.Handle, StyleServices.GetElementDetails(DisabledState[IsChecked]), PaintRect);

  finally
    RestoreDC(Canvas.Handle, SaveIndex);
  end;
end;

{ TThemedDropDownButtonEx }

procedure TThemedDropDownButtonEx.DrawBackground(var PaintRect: TRect);
const
  CheckedState: array[Boolean] of TThemedToolBar = (ttbButtonHot, ttbButtonCheckedHot);
var
  LIndex : Integer;
begin
  LIndex := SaveDC(Canvas.Handle);
  try
    if Enabled and not (ActionBar.DesignMode) then
    begin
      if (MouseInControl or IsChecked or DroppedDown) and
         (Assigned(ActionClient) and not ActionClient.Separator) then
      begin
        StyleServices.DrawElement(Canvas.Handle, StyleServices.GetElementDetails(CheckedState[IsChecked or (FState = bsDown)]), PaintRect);

       if IsChecked and not MouseInControl then
          StyleServices.DrawElement(Canvas.Handle, StyleServices.GetElementDetails(ttbButtonPressed), PaintRect);
      end
      else
        ;//StyleServices.DrawElement(Canvas.Handle, StyleServices.GetElementDetails(ttbButtonNormal), PaintRect);
    end
    else
      ;//StyleServices.DrawElement(Canvas.Handle, StyleServices.GetElementDetails(DisabledState[IsChecked]), PaintRect);
  finally
    RestoreDC(Canvas.Handle, LIndex);
  end;
end;

initialization
  PlatformVclStylesStyle := TPlatformVclStylesStyle.Create;
  RegisterActnBarStyle(PlatformVclStylesStyle);
  DefaultActnBarStyle :=PlatformVclStylesStyle.GetStyleName;
finalization
  UnregisterActnBarStyle(PlatformVclStylesStyle);
  PlatformVclStylesStyle.Free;
end.

