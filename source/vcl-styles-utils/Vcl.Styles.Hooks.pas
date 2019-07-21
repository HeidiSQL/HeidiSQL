// **************************************************************************************************
//
// Unit Vcl.Styles.Hooks
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
// The Original Code is Vcl.Styles.Hooks.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
//
// Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2019 Rodrigo Ruz V.
//
// Contributor(s): Mahdi Safsafi.
//
// All Rights Reserved.
//
// **************************************************************************************************
unit Vcl.Styles.Hooks;

interface

uses
  WinApi.Windows;

var
  Trampoline_user32_GetSysColorBrush: function(nIndex: Integer): HBRUSH; stdcall;
  Trampoline_user32_GetSysColor: function(nIndex: Integer): DWORD; stdcall;


implementation

{$I VCL.Styles.Utils.inc}

uses
  DDetours,
  System.SyncObjs,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Generics.Collections,
  System.StrUtils,
  WinApi.Messages,
  WinApi.UXTheme,
  Vcl.Graphics,
  Vcl.Styles.Utils.Graphics,
{$IFDEF HOOK_UXTHEME}
  Vcl.Styles.UxTheme,
{$ENDIF HOOK_UXTHEME}
  Vcl.Styles.Utils.SysControls,
  Vcl.Styles.FontAwesome,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Themes,
  Vcl.Styles.Utils.Misc;

type
  TListStyleBrush  = class(TDictionary<Integer, HBRUSH>)
  protected
    procedure ValueNotify(const Value: HBRUSH; Action: TCollectionNotification); override;
  end;

  TSetStyle = procedure(Style: TCustomStyleServices) of object;
  TMonthCalendarClass = class(TMonthCalendar);
  TCommonCalendarClass = class(TCommonCalendar);

var
  VCLStylesBrush: TObjectDictionary<string, TListStyleBrush>;
  VCLStylesLock: TCriticalSection = nil;
  LSetStylePtr: TSetStyle;

  Trampoline_SetStyle  : procedure(Self: TObject; Style: TCustomStyleServices);
  Trampoline_user32_FillRect  : function(hDC: hDC; const lprc: TRect; hbr: HBRUSH): Integer; stdcall;
  Trampoline_user32_DrawEdge  : function(hDC: hDC; var qrc: TRect; edge: UINT; grfFlags: UINT): BOOL;  stdcall = nil;
  Trampoline_user32_DrawFrameControl : function (DC: HDC; Rect: PRect; uType, uState: UINT): BOOL; stdcall = nil;
  Trampoline_user32_LoadIconW  : function (hInstance: HINST; lpIconName: PWideChar): HICON; stdcall = nil;
  {$IFDEF HOOK_UXTHEME}
  Trampoline_user32_LoadImageW: function (hInst: HINST; ImageName: LPCWSTR; ImageType: UINT; X, Y: Integer; Flags: UINT): THandle; stdcall = nil;
  {$ENDIF HOOK_UXTHEME}

{$IFDEF HOOK_TDateTimePicker}
  {$IF CompilerVersion>=29}
  Trampoline_SetWindowTheme: function(hwnd: HWND; pszSubAppName: LPCWSTR; pszSubIdList: LPCWSTR): HRESULT; stdcall;
  {$IFEND CompilerVersion}
{$ENDIF HOOK_TDateTimePicker}


function Detour_DrawEdge(hDC: hDC; var qrc: TRect; edge: UINT; grfFlags: UINT): BOOL; stdcall;
var
  CanDraw: Boolean;
  SaveIndex: Integer;
begin
  if not(ExecutingInMainThread) then
    Exit(Trampoline_user32_DrawEdge(hDC, qrc, edge, grfFlags));

  CanDraw := (not StyleServices.IsSystemStyle) and (TSysStyleManager.Enabled);
  if (CanDraw) and (edge <> BDR_OUTER) and (edge <> BDR_INNER) then
  begin
    SaveIndex := SaveDC(hDC);
    try
      DrawStyleEdge(hDC, qrc, TStyleElementEdges(edge), TStyleElementEdgeFlags(grfFlags));
    finally
      RestoreDC(hDC, SaveIndex);
    end;
    Exit(True);
  end;
  Exit(Trampoline_user32_DrawEdge(hDC, qrc, edge, grfFlags));
end;

function Detour_FillRect(hDC: hDC; const lprc: TRect; hbr: HBRUSH): Integer; stdcall;
begin
  if not(ExecutingInMainThread) or StyleServices.IsSystemStyle or not(TSysStyleManager.Enabled) then
    Exit(Trampoline_user32_FillRect(hDC, lprc, hbr))
  else if (hbr > 0) and (hbr < COLOR_ENDCOLORS + 1) then
    Exit(Trampoline_user32_FillRect(hDC, lprc, GetSysColorBrush(hbr - 1)))
  else
    Exit(Trampoline_user32_FillRect(hDC, lprc, hbr));
end;

function Detour_GetSysColor(nIndex: Integer): DWORD; stdcall;
begin
  if not(ExecutingInMainThread) or StyleServices.IsSystemStyle or not(TSysStyleManager.Enabled) then
    Result := Trampoline_user32_GetSysColor(nIndex)
  else if nIndex = COLOR_HOTLIGHT then
    Result := DWORD(StyleServices.GetSystemColor(clHighlight))
  else
    Result := DWORD(StyleServices.GetSystemColor(TColor(nIndex or Integer($FF000000))));
end;

function Detour_GetSysColorBrush(nIndex: Integer): HBRUSH; stdcall;
var
  LCurrentStyleBrush: TListStyleBrush;
  LBrush: HBRUSH;
  LColor: TColor;
begin
  if not(ExecutingInMainThread) then
    Exit(Trampoline_user32_GetSysColorBrush(nIndex));

  {
    The reason to change the previous code implementation
    is that the win32 graphics may differ with the VCL graphics:
    Eg: TColor is signed in VCL and Unsigned in Win32Api.
    When hooking : keep always using the native way !
    Need Color ?
    Use GetObject with LOGBRUSH ! or use TBrushColorPair !
  }
  VCLStylesLock.Enter;
  try
    if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled then
      Exit(Trampoline_user32_GetSysColorBrush(nIndex))
    else
    begin
      if VCLStylesBrush.ContainsKey(StyleServices.Name) then
        LCurrentStyleBrush := VCLStylesBrush.Items[StyleServices.Name]
      else
      begin
        VCLStylesBrush.Add(StyleServices.Name, TListStyleBrush.Create());
        LCurrentStyleBrush := VCLStylesBrush.Items[StyleServices.Name];
      end;
      if Assigned(LCurrentStyleBrush) then
      begin
        if LCurrentStyleBrush.ContainsKey(nIndex) then
          Exit(LCurrentStyleBrush[nIndex])
        else
        begin
          if nIndex = COLOR_HOTLIGHT then
            LColor := StyleServices.GetSystemColor(clHighlight)
          else
            LColor := StyleServices.GetSystemColor(TColor(nIndex or Integer($FF000000)));

          LBrush := CreateSolidBrush(LColor);
          LCurrentStyleBrush.Add(nIndex, LBrush);
          Exit(LBrush);
        end;
      end;
      Exit(Trampoline_user32_GetSysColorBrush(nIndex));
    end;
  finally
    VCLStylesLock.Leave;
  end;
end;

procedure Detour_SetStyle(Self: TObject; Style: TCustomStyleServices);
var
  I: Integer;
  LActiveStyle: TCustomStyleServices;
begin
  if not(ExecutingInMainThread) then
    begin
      Trampoline_SetStyle(Self, Style);
      exit;
    end;

  LActiveStyle := TStyleManager.ActiveStyle;
  Trampoline_SetStyle(Self, Style);
  if (Style <> LActiveStyle) then
  begin
    for I := 0 to Screen.FormCount - 1 do
      if Screen.Forms[I].HandleAllocated then
        SendMessage(Screen.Forms[I].Handle, WM_SYSCOLORCHANGE, 0, 0);
  end;
end;

//based on JvThemes.DrawThemedFrameControl
function Detour_WinApi_DrawFrameControl(DC: HDC; Rect: PRect; uType, uState: UINT): BOOL; stdcall;
const
  Mask = $00FF;
var
  LRect: TRect;
  LDetails: TThemedElementDetails;
  CanDraw : Boolean;

  LThemedButton: TThemedButton;
  LThemedComboBox: TThemedComboBox;
  LThemedScrollBar: TThemedScrollBar;
begin
  if not(ExecutingInMainThread) then
    Exit(Trampoline_user32_DrawFrameControl(DC, Rect, uType, uState));

  Result := False;
  CanDraw:= (not StyleServices.IsSystemStyle) and (TSysStyleManager.Enabled) and (Rect <> nil);
  if CanDraw then
  begin
    LRect := Rect^;
    case uType of
      DFC_BUTTON:
        case uState and Mask of

          DFCS_BUTTONPUSH:
            begin
              if uState and (DFCS_TRANSPARENT or DFCS_FLAT) = 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  LThemedButton := tbPushButtonDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  LThemedButton := tbPushButtonPressed
                else
                if uState and DFCS_HOT <> 0 then
                  LThemedButton := tbPushButtonHot
                else
                if uState and DFCS_MONO <> 0 then
                  LThemedButton := tbPushButtonDefaulted
                else
                  LThemedButton := tbPushButtonNormal;

                LDetails := StyleServices.GetElementDetails(LThemedButton);
                StyleServices.DrawElement(DC, LDetails, LRect);
                Result := True;
              end;
            end;

          DFCS_BUTTONCHECK:
            begin
              if uState and DFCS_CHECKED <> 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  LThemedButton := tbCheckBoxCheckedDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  LThemedButton := tbCheckBoxCheckedPressed
                else
                if uState and DFCS_HOT <> 0 then
                  LThemedButton := tbCheckBoxCheckedHot
                else
                  LThemedButton := tbCheckBoxCheckedNormal;
              end
              else
              if uState and DFCS_MONO <> 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  LThemedButton := tbCheckBoxMixedDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  LThemedButton := tbCheckBoxMixedPressed
                else
                if uState and DFCS_HOT <> 0 then
                  LThemedButton := tbCheckBoxMixedHot
                else
                  LThemedButton := tbCheckBoxMixedNormal;
              end
              else
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  LThemedButton := tbCheckBoxUncheckedDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  LThemedButton := tbCheckBoxUncheckedPressed
                else
                if uState and DFCS_HOT <> 0 then
                  LThemedButton := tbCheckBoxUncheckedHot
                else
                  LThemedButton := tbCheckBoxUncheckedNormal;
              end;
              LDetails := StyleServices.GetElementDetails(LThemedButton);
              StyleServices.DrawElement(DC, LDetails, LRect);
              Result := True;
            end;

          DFCS_BUTTONRADIO:
            begin
              if uState and DFCS_CHECKED <> 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  LThemedButton := tbRadioButtonCheckedDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  LThemedButton := tbRadioButtonCheckedPressed
                else
                if uState and DFCS_HOT <> 0 then
                  LThemedButton := tbRadioButtonCheckedHot
                else
                  LThemedButton := tbRadioButtonCheckedNormal;
              end
              else
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  LThemedButton := tbRadioButtonUncheckedDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  LThemedButton := tbRadioButtonUncheckedPressed
                else
                if uState and DFCS_HOT <> 0 then
                  LThemedButton := tbRadioButtonUncheckedHot
                else
                  LThemedButton := tbRadioButtonUncheckedNormal;
              end;
              LDetails := StyleServices.GetElementDetails(LThemedButton);
              StyleServices.DrawElement(DC, LDetails, LRect);
              Result := True;
            end;
        end;

      DFC_SCROLL:
        begin
          case uState and Mask of

            DFCS_SCROLLCOMBOBOX:
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  LThemedComboBox := tcDropDownButtonDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  LThemedComboBox := tcDropDownButtonPressed
                else
                if uState and DFCS_HOT <> 0 then
                  LThemedComboBox := tcDropDownButtonHot
                else
                  LThemedComboBox := tcDropDownButtonNormal;

                LDetails := StyleServices.GetElementDetails(LThemedComboBox);
                StyleServices.DrawElement(DC, LDetails, LRect);
                Result := True;
              end;

            DFCS_SCROLLUP:
              if uState and (DFCS_TRANSPARENT {or DFCS_FLAT}) = 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  LThemedScrollBar := tsArrowBtnUpDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  LThemedScrollBar := tsArrowBtnUpPressed
                else
                if uState and DFCS_HOT <> 0 then
                  LThemedScrollBar := tsArrowBtnUpHot
                else
                  LThemedScrollBar := tsArrowBtnUpNormal;

                LDetails := StyleServices.GetElementDetails(LThemedScrollBar);
                StyleServices.DrawElement(DC, LDetails, LRect);
                Result := True;
              end;

            DFCS_SCROLLDOWN:
              if uState and (DFCS_TRANSPARENT {or DFCS_FLAT}) = 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  LThemedScrollBar := tsArrowBtnDownDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  LThemedScrollBar := tsArrowBtnDownPressed
                else
                if uState and DFCS_HOT <> 0 then
                  LThemedScrollBar := tsArrowBtnDownHot
                else
                  LThemedScrollBar := tsArrowBtnDownNormal;

                LDetails := StyleServices.GetElementDetails(LThemedScrollBar);
                StyleServices.DrawElement(DC, LDetails, LRect);
                Result := True;
              end;

            DFCS_SCROLLLEFT:
              if uState and (DFCS_TRANSPARENT {or DFCS_FLAT}) = 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  LThemedScrollBar := tsArrowBtnLeftDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  LThemedScrollBar := tsArrowBtnLeftPressed
                else
                if uState and DFCS_HOT <> 0 then
                  LThemedScrollBar := tsArrowBtnLeftHot
                else
                  LThemedScrollBar := tsArrowBtnLeftNormal;

                LDetails := StyleServices.GetElementDetails(LThemedScrollBar);
                StyleServices.DrawElement(DC, LDetails, LRect);
                Result := True;
              end;

            DFCS_SCROLLRIGHT:
              if uState and (DFCS_TRANSPARENT {or DFCS_FLAT}) = 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  LThemedScrollBar := tsArrowBtnRightDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  LThemedScrollBar := tsArrowBtnRightPressed
                else
                if uState and DFCS_HOT <> 0 then
                  LThemedScrollBar := tsArrowBtnRightHot
                else
                  LThemedScrollBar := tsArrowBtnRightNormal;

                LDetails := StyleServices.GetElementDetails(LThemedScrollBar);
                StyleServices.DrawElement(DC, LDetails, LRect);
                Result := True;
              end;

          end;
        end;
    end;
  end;

  if not Result then
    Exit(Trampoline_user32_DrawFrameControl(DC, Rect, uType, uState));
end;

function GetStyleHighLightColor : TColor;
begin
  if ColorIsBright(StyleServices.GetSystemColor(clBtnFace)) or not ColorIsBright(StyleServices.GetSystemColor(clHighlight)) then
    Result := StyleServices.GetSystemColor(clBtnText)
  else
    Result := StyleServices.GetSystemColor(clHighlight);
end;

function Detour_LoadIconW(_hInstance: HINST; lpIconName: PWideChar): HICON; stdcall;
var
  s : string;
  LIcon : TIcon;
  LHandle : THandle;
  MustRelease : Boolean;

   procedure DrawIcon(const ACode: Word);
   begin
     //DestroyIcon(LHandle);
     Result := FontAwesome.GetIcon(ACode, LIcon.Width, LIcon.Height, GetStyleHighLightColor, StyleServices.GetSystemColor(clBtnFace), 0);
     MustRelease:=False;
   end;

begin
  if not(ExecutingInMainThread) or
     StyleServices.IsSystemStyle or
     not(TSysStyleManager.Enabled) or
     not(TSysStyleManager.HookDialogIcons) then
    Exit(Trampoline_user32_LoadIconW(_hInstance, lpIconName));

  if {(_hInstance>0) and (_hInstance<>HInstance) and} IS_INTRESOURCE(lpIconName) then
  begin
    LIcon := TIcon.Create;
    try
      MustRelease := True;
      LHandle := Trampoline_user32_LoadIconW(_hInstance, lpIconName);
      LIcon.Handle := LHandle;
      Result := LHandle;
      s := IntToStr(NativeUInt(lpIconName));

      //OutputDebugString(PChar('Detour_LoadIconW '+s+ ' Module Name '+GetModuleName(_hInstance)+' _hInstance '+IntToHex(_hInstance, 8) ));
      case NativeUInt(lpIconName) of
       78: DrawIcon(fa_shield);
       81: DrawIcon(fa_info_circle);
       84: DrawIcon(fa_warning);
       98: DrawIcon(fa_minus_circle);
       99: DrawIcon(fa_question_circle);
      end;

      if _hInstance=0 then
      case NativeUInt(lpIconName) of
       32518 : DrawIcon(fa_shield);
       32516 : DrawIcon(fa_info_circle);
       32515 : DrawIcon(fa_warning);
       32513 : DrawIcon(fa_minus_circle);
       32514 : DrawIcon(fa_question_circle);
       32517 : DrawIcon(fa_windows);
      end;
	   
    finally
      if MustRelease then
       LIcon.ReleaseHandle;
      LIcon.Free;
    end;
  end
  else
    Exit(Trampoline_user32_LoadIconW(_hInstance, lpIconName));
end;

{$IFDEF HOOK_UXTHEME}
function Detour_LoadImageW(hInst: HINST; ImageName: LPCWSTR; ImageType: UINT; X, Y: Integer; Flags: UINT): THandle; stdcall;
const
  ExplorerFrame = 'explorerframe.dll';
var
  hModule : WinApi.Windows.HMODULE;
  LBitmap : TBitmap;
  s : string;
  LRect, LRect2 : TRect;
  LBackColor, LColor : TColor;
begin
  if not(ExecutingInMainThread) or StyleServices.IsSystemStyle or not(TSysStyleManager.Enabled) then
    Exit(Trampoline_user32_LoadImageW(hInst, ImageName, ImageType, X, Y, Flags));

                                                                                                                         //w8 - W10
  if (hInst > 0) and (hInst <> HInstance) and (ImageType = IMAGE_ICON) and (X = 16) and (Y = 16) and IS_INTRESOURCE(ImageName) and TOSVersion.Check(6, 2) then
  begin
    s := IntToStr(NativeUInt(ImageName));

    case NativeUInt(ImageName) of
      //W8, W10
      //comctl32.dll
      16817:
        begin
          Exit(FontAwesome.GetIcon(fa_arrow_up, X, Y, StyleServices.GetSystemColor(clBtnText), StyleServices.GetSystemColor(clBtnFace), 0));
        end;
      16818:
        begin
          Exit(FontAwesome.GetIcon(fa_arrow_up, X, Y, StyleServices.GetSystemColor(clGrayText), StyleServices.GetSystemColor(clBtnFace), 0));
        end;
      //W10
      //shell32.dll
      5100:
        begin
          //OutputDebugString(PChar('GetModuleName ' + GetModuleName(hInst)));
          Exit(FontAwesome.GetIcon(fa_thumb_tack, 16, 16, 12, 12, StyleServices.GetSystemColor(clGrayText), StyleServices.GetSystemColor(clWindow), -22, iaRight));
          //Exit(FontAwesome.GetIcon(fa_check_square_o, 16, 16, StyleServices.GetSystemColor(clWindowText), StyleServices.GetSystemColor(clWindow), 0));
        end;
    end;
    Exit(Trampoline_user32_LoadImageW(hInst, ImageName, ImageType, X, Y, Flags));
  end
  else
  if (hInst > 0) and (ImageType = IMAGE_BITMAP) and (X = 0) and (Y = 0) and IS_INTRESOURCE(ImageName) then
  begin
    hModule := GetModuleHandle(ExplorerFrame);
    if (hModule = hInst) then
    begin
      s := IntToStr(NativeUInt(ImageName));
      Result  := Trampoline_user32_LoadImageW(hInst, ImageName, ImageType, X, Y, Flags);
      LBitmap := TBitmap.Create;
      try
        LBitmap.Handle := Result;
        //LBitmap.SaveToFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+s+'.bmp');//SaveToFile('C:\Users\Rodrigo\Desktop\vcl-styles-utils\Vcl Styles Utils New Dialogs (Demo App)\Win32\Debug\Images\'+s+'.bmp');

        //W8 - W10
        if TOSVersion.Check(6, 2) then
        begin
          LBackColor := StyleServices.GetSystemColor(clWindow);
          LRect := Rect(0, 0, LBitmap.Width, LBitmap.Height );
          case NativeUInt(ImageName) of
            // Right Arrow, cross button, refresh, down arrow
            288:
              begin
                 LColor := StyleServices.GetSystemColor(clBtnText);
                 Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);

                 LRect := Rect(0, 0, 16, 16);
                 FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_arrow_right, LRect, LColor);

                 OffsetRect(LRect, 16, 0);
                 FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_remove, LRect, LColor);

                 OffsetRect(LRect, 16, 0);
                 LRect2 := LRect;
                 InflateRect(LRect2, -2, -2);
                 FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_refresh, LRect2, LColor);

                 OffsetRect(LRect, 16 + 2, 0);
                 LRect2 := LRect;
                 InflateRect(LRect2, -2, -2);
                 FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_caret_down, LRect2, LColor);

                 Bitmap32_SetAlphaExceptColor(LBitmap, 255, LBackColor);
              end;
            else
              begin
                Bitmap32_Grayscale(LBitmap);
                _ProcessBitmap32(LBitmap, StyleServices.GetSystemColor(clHighlight), _BlendBurn)
              end;
          end;
        end
        else
        //Windows Vista - W7
        if (TOSVersion.Major = 6) and ((TOSVersion.Minor = 0) or (TOSVersion.Minor = 1)) then
        begin
          LBackColor := StyleServices.GetSystemColor(clWindow);
          LRect := Rect(0, 0, LBitmap.Width, LBitmap.Height );
          case NativeUInt(ImageName) of
           //Magnifier
           34560..34562,  // Aero Enabled
           34563..34568:  // Classic Theme
             begin
               LColor := StyleServices.GetSystemColor(clHighlight);
               Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);
               FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_search, LRect, LColor);
               Bitmap32_SetAlphaExceptColor(LBitmap, 255, LBackColor);
               //Bitmap32_SetAlphaByColor(LBitmap, 255, LColor);
             end;

           //cross button normal
           34569..34571,  // Aero Enabled
           34572..34574:   // Classic Theme
             begin
                LColor:= StyleServices.GetSystemColor(clWindowText);
                Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);
                FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_remove, LRect, LColor);
                Bitmap32_SetAlphaExceptColor(LBitmap, 255, LBackColor);
             end;

           //cross button hot
           34575..34577, // Aero Enabled
           34581..34583, // Aero Enabled
           34578..34580:  // Classic Theme
             begin
               LColor := StyleServices.GetSystemColor(clHighlight);
               Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);
               FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_remove, LRect, LColor);
               Bitmap32_SetAlphaExceptColor(LBitmap, 255, LBackColor);
             end;

           // Aero Enabled
           // Right Arrow, cross button, refresh, down arrow
            288:
              begin
                LColor := StyleServices.GetSystemColor(clBtnText);
                Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);

                LRect:=Rect(0, 0, 16, 16);
                FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_arrow_right, LRect, LColor);

                OffsetRect(LRect, 16, 0);
                FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_remove, LRect, LColor);

                OffsetRect(LRect, 16, 0);
                FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_refresh, LRect, LColor);

                OffsetRect(LRect, 16, 0);
                FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_caret_down, LRect, LColor);

                Bitmap32_SetAlphaExceptColor(LBitmap, 255, LBackColor);
              end;

            // Classic Theme
            // Right Arrow, cross button, refresh, down arrow
            289, 290:
              begin
                LColor := StyleServices.GetSystemColor(clBtnText);
                Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);

                LRect:=Rect(0, 0, 21, 21);
                FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_arrow_right, LRect, LColor);

                OffsetRect(LRect, 21, 0);
                FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_remove, LRect, LColor);

                OffsetRect(LRect, 21, 0);
                FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_refresh, LRect, LColor);

                OffsetRect(LRect, 21, 0);
                FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_caret_down, LRect, LColor);

                Bitmap32_SetAlphaExceptColor(LBitmap, 255, LBackColor);
              end;

            // Aero Enabled
            // navigation buttons (arrows)
            577..579,
            581:
              begin
                case NativeUInt(ImageName) of
                  577 : LColor := StyleServices.GetSystemColor(clBtnText);
                  578 : LColor := StyleServices.GetSystemColor(clHighlight);
                  579 : LColor := StyleServices.GetSystemColor(clGrayText);
                  581 : LColor := StyleServices.GetSystemColor(clBtnText);
                  else
                    LColor:= StyleServices.GetSystemColor(clBtnText);
                end;

                Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);
				
                LRect := Rect(0, 0, 27, 27);
                InflateRect(LRect, -4, -4);
                FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_arrow_left, LRect, LColor);
                OffsetRect(LRect, 27 + 4, 0);
                FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_arrow_right, LRect, LColor);

                Bitmap32_SetAlphaExceptColor(LBitmap, 255, LBackColor);
              end;

            //Classic Theme
            // navigation buttons (arrows)
            582..584:
              begin
                case NativeUInt(ImageName) of
                  582 : LColor := StyleServices.GetSystemColor(clBtnText);
                  583 : LColor := StyleServices.GetSystemColor(clHighlight);
                  584 : LColor := StyleServices.GetSystemColor(clGrayText);
                else
                  LColor := StyleServices.GetSystemColor(clBtnText);
                end;

                Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);

                //left arrow
                LRect := Rect(0, 0, 25, 25);
                InflateRect(LRect, -4, -4);
                FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_arrow_left, LRect, LColor);

                //right arrow
                OffsetRect(LRect, 25 + 4, 0);
                FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_arrow_right, LRect, LColor);

                //dropdown arrow
                LRect := Rect(60, 8, 72, 20);
                FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_caret_down, LRect, LColor);


                Bitmap32_SetAlphaExceptColor(LBitmap, 255, LBackColor);
              end;

            //Aero Enabled
            //background navigation buttons
            280:
              begin
                Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);

                LRect := Rect(0, 8, 12, 20);
                LColor := StyleServices.GetSystemColor(clGrayText);
                OffsetRect(LRect, 58, 0);
                FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_caret_down, LRect, LColor);

                LColor := StyleServices.GetSystemColor(clBtnText);
                OffsetRect(LRect, 70, 0);
                FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_caret_down, LRect, LColor);

                LColor := StyleServices.GetSystemColor(clHighlight);
                OffsetRect(LRect, 70, 0);
                FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_caret_down, LRect, LColor);

                LColor := StyleServices.GetSystemColor(clHighlight);
                OffsetRect(LRect, 70, 0);
                FontAwesome.DrawChar(LBitmap.Canvas.Handle, fa_caret_down, LRect, LColor);

                Bitmap32_SetAlphaExceptColor(LBitmap, 255, LBackColor);
              end;

            //Classic Theme
            //background navigation buttons
            281:
              begin
                Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);
              end;
          end;
        end;
        LBitmap.ReleaseHandle;
      finally
        LBitmap.Free;
      end;
      //OutputDebugString(PChar(Format('Detour_LoadImageW ImageName %s', [s])));
      Exit(Result);
    end;
  end;

  Exit(Trampoline_user32_LoadImageW(hInst, ImageName, ImageType, X, Y, Flags));
end;
{$ENDIF HOOK_UXTHEME}


{$IFDEF HOOK_TDateTimePicker}
  {$IF CompilerVersion>=29}
  function Detour_SetWindowTheme(hwnd: HWND; pszSubAppName: LPCWSTR; pszSubIdList: LPCWSTR): HRESULT; stdcall;
  var
    LControl : TWinControl;
  begin
    if not(ExecutingInMainThread) then
      Exit(Trampoline_SetWindowTheme(hwnd, pszSubAppName, pszSubIdList));

     LControl:= FindControl(hwnd);
     if (pszSubAppName = '') and (pszSubIdList = '') and TStyleManager.IsCustomStyleActive and (LControl<>nil) and (LControl is TMonthCalendar) then
       Exit(S_OK)
     else
       Exit(Trampoline_SetWindowTheme(hwnd, pszSubAppName, pszSubIdList));
  end;
  {$IFEND CompilerVersion}
{$ENDIF HOOK_TDateTimePicker}


//don't hook CreateSolidBrush, because is used internally by GetSysColorBrush
//don't hook CopyImage

{ TListStyleBrush }

//Delete brushes
procedure TListStyleBrush.ValueNotify(const Value: HBRUSH; Action: TCollectionNotification);
begin
  inherited;
  if Action = TCollectionNotification.cnRemoved then
    DeleteObject(Value);
end;

const
  themelib = 'uxtheme.dll';

initialization

 VCLStylesLock := TCriticalSection.Create;
 VCLStylesBrush := TObjectDictionary<string, TListStyleBrush>.Create([doOwnsValues]);

if StyleServices.Available then
begin

{$IFDEF HOOK_TDateTimePicker}
  TCustomStyleEngine.RegisterStyleHook(TDateTimePicker, TStyleHook);
{$ENDIF HOOK_TDateTimePicker}


{$IFDEF HOOK_TProgressBar}
  TCustomStyleEngine.RegisterStyleHook(TProgressBar, TStyleHook);
{$ENDIF HOOK_TProgressBar}
  LSetStylePtr := TStyleManager.SetStyle;

  BeginHooks;
  @Trampoline_user32_GetSysColor := InterceptCreate(user32, 'GetSysColor', @Detour_GetSysColor);
  @Trampoline_user32_GetSysColorBrush := InterceptCreate(user32, 'GetSysColorBrush', @Detour_GetSysColorBrush);
  @Trampoline_user32_FillRect := InterceptCreate(user32, 'FillRect', @Detour_FillRect);
  @Trampoline_user32_DrawEdge := InterceptCreate(user32, 'DrawEdge', @Detour_DrawEdge);
  @Trampoline_user32_DrawFrameControl :=  InterceptCreate(user32, 'DrawFrameControl', @Detour_WinApi_DrawFrameControl);
  @Trampoline_user32_LoadIconW := InterceptCreate(user32, 'LoadIconW', @Detour_LoadIconW);
{$IFDEF HOOK_UXTHEME}
  if TOSVersion.Check(6) then
   @Trampoline_user32_LoadImageW := InterceptCreate(user32, 'LoadImageW', @Detour_LoadImageW);

{$ENDIF HOOK_UXTHEME}

  @Trampoline_SetStyle := InterceptCreate(@LSetStylePtr, @Detour_SetStyle);

{$IFDEF HOOK_TDateTimePicker}
  {$IF CompilerVersion>=29}
  //@Trampoline_TMonthCalendar_CreateWnd := InterceptCreate(@TMonthCalendarClass.CreateWnd, @Detour_TMonthCalendar_CreateWnd);
  @Trampoline_SetWindowTheme := InterceptCreate(themelib, 'SetWindowTheme', @Detour_SetWindowTheme);
  {$IFEND CompilerVersion}
{$ENDIF HOOK_TDateTimePicker}

  EndHooks;
end;

finalization

  BeginUnHooks;
  InterceptRemove(@Trampoline_user32_GetSysColor);
  InterceptRemove(@Trampoline_user32_GetSysColorBrush);
  InterceptRemove(@Trampoline_user32_FillRect);
  InterceptRemove(@Trampoline_user32_DrawEdge);
  InterceptRemove(@Trampoline_user32_DrawFrameControl);
  InterceptRemove(@Trampoline_user32_LoadIconW);

{$IFDEF HOOK_UXTHEME}
  if TOSVersion.Check(6) then
    InterceptRemove(@Trampoline_user32_LoadImageW);
{$ENDIF HOOK_UXTHEME}
  InterceptRemove(@Trampoline_SetStyle);

{$IFDEF HOOK_TDateTimePicker}
  {$IF CompilerVersion>=29}
  InterceptRemove(@Trampoline_SetWindowTheme);
  {$IFEND CompilerVersion}
{$ENDIF HOOK_TDateTimePicker}

  EndUnHooks;
  VCLStylesBrush.Free;
  VCLStylesLock.Free;
  VCLStylesLock := nil;

end.
