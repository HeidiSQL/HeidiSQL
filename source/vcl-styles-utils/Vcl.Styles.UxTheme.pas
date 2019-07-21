// **************************************************************************************************
//
// Unit Vcl.Styles.UxTheme
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
// The Original Code is Vcl.Styles.UxTheme.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2019 Rodrigo Ruz V.
// All Rights Reserved.
//
// **************************************************************************************************
unit Vcl.Styles.UxTheme;

interface

implementation

{ TODO



  done   W8 Check Color text disabled on listview hot item
  done   Test on W10
  done   Test Task Dialogs
  done   fix preview windows background color when no elements are shown
  done   fix background of homegroup folder (and related)
  done   remove unused code of DrawThemeText and DrawThemeTextEx, (replaced by GetThemeColor)
  done   fix navigation buttons  W8, W10
  done   improve drawing navigation buttons W7

  fix related shell dialogs
  Add support for custom titlebars.
  msstyles

  Add support for Classic Theme
  fix menu
  fix popup windows with trackbar


}

{$DEFINE HOOK_Button}
{$DEFINE HOOK_Scrollbar}
{$DEFINE HOOK_TaskDialog}
{$DEFINE HOOK_ProgressBar}
{$DEFINE HOOK_DateTimePicker}
{$DEFINE HOOK_TreeView}
{$DEFINE HOOK_ListView}
{$DEFINE HOOK_ListBox}
{$DEFINE HOOK_ComboBox}
{$DEFINE HOOK_Spin}
{$DEFINE HOOK_EDIT}
{$DEFINE HOOK_Rebar}
{$DEFINE HOOK_ToolBar}
{$DEFINE HOOK_Menu}
{$DEFINE HOOK_TrackBar}
{$DEFINE HOOK_ToolTip}
{$DEFINE HOOK_Tab}
// Undocumented Windows Themes

// Introduced in Windows Vista

{$DEFINE HOOK_CommandModule}
{$DEFINE HOOK_SearchBox}
{$DEFINE HOOK_AddressBand}
{$DEFINE HOOK_PreviewPane}
{$DEFINE HOOK_TRYHARDER}
{$DEFINE HOOK_BREADCRUMBAR}
{$DEFINE HOOK_InfoBar}
// Introduced in Windows 8
{$DEFINE HOOK_Navigation}

uses
  DDetours,
  System.SyncObjs,
  System.SysUtils,
  System.Types,
  System.Classes,
  System.UITypes,
  System.Math,
  System.Generics.Collections,
  System.Generics.Defaults,
  WinApi.Windows,
  WinApi.UxTheme,
  WinApi.Messages,
  Vcl.Graphics,
  Vcl.GraphUtil,
  Vcl.Themes,
  Vcl.Imaging.pngimage,
  Vcl.Styles.Hooks,
  Vcl.Styles.Utils.Graphics,
  Vcl.Styles.FontAwesome,
  Vcl.Styles.Utils.SysControls,
  Vcl.Styles.Utils.Misc;

type
  TDrawThemeBackground = function(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect;
    Foo: Pointer): HRESULT; stdcall;

{$IFDEF HOOK_ProgressBar}

const
  VSCLASS_PROGRESS_INDERTERMINATE = 'Indeterminate::Progress';
{$ENDIF}
{$IFDEF HOOK_ListView}

const
  VSCLASS_ITEMSVIEW_LISTVIEW = 'ItemsView::ListView';
  VSCLASS_ITEMSVIEW_HEADER = 'ItemsView::Header';
  VSCLASS_EXPLORER_LISTVIEW = 'Explorer::ListView';
  VSCLASS_ITEMSVIEW = 'ItemsView';
  VSCLASS_LISTVIEWPOPUP = 'ListViewPopup';
{$ENDIF}
{$IFDEF HOOK_CommandModule}

const
  VSCLASS_COMMANDMODULE = 'CommandModule';
  VSCLASS_CPLCOMMANDMODULE = 'CPLCommandModule::CommandModule';
{$ENDIF}
{$IFDEF HOOK_SearchBox}

const
  VSCLASS_SEARCHEDITBOX = 'SearchEditBox';
  VSCLASS_SEARCHBOX = 'SearchBox';
  VSCLASS_CompositedSEARCHBOX = 'SearchBoxCompositedSearchBox::SearchBox';
  VSCLASS_INACTIVESEARCHBOX = 'InactiveSearchBoxCompositedSearchBox::SearchBox';
{$ENDIF}
{$IFDEF HOOK_AddressBand}

const
  VSCLASS_ADDRESSBAND = 'AddressBand';
{$ENDIF}
{$IFDEF HOOK_PreviewPane}

const
  VSCLASS_PREVIEWPANE = 'PreviewPane';
  VSCLASS_READINGPANE = 'ReadingPane';
{$ENDIF}
{$IFDEF HOOK_TRYHARDER}

const
  VSCLASS_TRYHARDER = 'TryHarder';
{$ENDIF}
{$IFDEF HOOK_BREADCRUMBAR}

const
  VSCLASS_BREADCRUMBAR = 'BreadcrumbBar';
{$ENDIF}
{$IFDEF HOOK_Navigation}

const
  VSCLASS_NAVIGATION = 'Navigation';
  VSCLASS_COMMONITEMSDIALOG = 'CommonItemsDialog';
{$ENDIF}
{$IFDEF HOOK_TreeView}

const
  VSCLASS_PROPERTREE = 'PROPERTREE';
{$ENDIF}
{$IFDEF HOOK_InfoBar}

const
  VSCLASS_INFOBAR = 'InfoBar';
{$ENDIF}
{$IFDEF HOOK_Menu}

const
  MARLETT_RESTORE_CHAR = Char(50);
  MARLETT_MINIMIZE_CHAR = Char(48);
  MARLETT_CLOSE_CHAR = Char(114);
  MARLETT_MAXIMIZE_CHAR = Char(49);
{$ENDIF}

type
  TFuncDrawThemeBackground  =  function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;  const pRect: TRect; Foo: Pointer; Trampoline : TDrawThemeBackground; LThemeClass : string; hwnd : HWND): HRESULT; stdcall;

var
  Trampoline_UxTheme_OpenThemeDataEx       : function(hwnd: HWND; pszClassList: LPCWSTR; dwFlags: DWORD): HTHEME; stdcall = nil;
  Trampoline_UxTheme_OpenThemeData         : function(hwnd: HWND; pszClassList: LPCWSTR): HTHEME; stdcall =  nil;
{$IF CompilerVersion >= 30}
  Trampoline_UxTheme_OpenThemeDataForDPI   : function(hwnd: HWND; pszClassList: LPCWSTR; dpi: UINT): HTHEME; stdcall =  nil;
{$IFEND}


  Trampoline_UxTheme_CloseThemeData        : function(hTheme: HTHEME): HRESULT; stdcall =  nil;
  Trampoline_UxTheme_DrawThemeBackground   : function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect; pClipRect: Pointer): HRESULT; stdcall =  nil;
  Trampoline_UxTheme_DrawThemeBackgroundEx : function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect; pOptions: Pointer): HResult; stdcall =  nil;
  Trampoline_UxTheme_GetThemeColor         : function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var pColor: COLORREF): HRESULT; stdcall = nil;
  Trampoline_UxTheme_GetThemeSysColor      : function(hTheme: HTHEME; iColorId: Integer): COLORREF; stdcall =  nil;
  Trampoline_UxTheme_GetThemeSysColorBrush : function(hTheme: HTHEME; iColorId: Integer): HBRUSH; stdcall =  nil;
  Trampoline_UxTheme_DrawThemeText         : function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;  pszText: LPCWSTR; iCharCount: Integer; dwTextFlags, dwTextFlags2: DWORD; const pRect: TRect): HRESULT; stdcall = nil;
  Trampoline_UxTheme_DrawThemeTextEx       : function(hTheme: HTHEME; hdc: HDC; iPartId: Integer; iStateId: Integer; pszText: LPCWSTR; cchText: Integer; dwTextFlags: DWORD; pRect: PRect; var pOptions: TDTTOpts): HResult; stdcall = nil;
  Trampoline_UxTheme_DrawThemeEdge         : function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pDestRect: TRect; uEdge, uFlags: UINT; pContentRect: PRECT): HRESULT; stdcall = nil;

  THThemesClasses  : TDictionary<HTHEME, string>;
  THThemesHWND     : TDictionary<HTHEME, HWND>;

  FuncsDrawThemeBackground  : TDictionary<string, TFuncDrawThemeBackground>;

  VCLStylesLock    : TCriticalSection = nil;


function Detour_UxTheme_OpenThemeData(hwnd: hwnd; pszClassList: LPCWSTR): hTheme; stdcall;
begin
  if not(ExecutingInMainThread) then
  begin
    Result := Trampoline_UxTheme_OpenThemeData(hwnd, pszClassList);
    exit;
  end;

  // OutputDebugString(PChar('Detour_UxTheme_OpenThemeData '+pszClassList));
  VCLStylesLock.Enter;
  try
    Result := Trampoline_UxTheme_OpenThemeData(hwnd, pszClassList);
    if THThemesClasses.ContainsKey(Result) then
      THThemesClasses.Remove(Result);
    THThemesClasses.Add(Result, pszClassList);

    if THThemesHWND.ContainsKey(Result) then
      THThemesHWND.Remove(Result);
    THThemesHWND.Add(Result, hwnd);
  finally
    VCLStylesLock.Leave;
  end;
  // OutputDebugString(PChar('Detour_UxTheme_OpenThemeData '+pszClassList+' hTheme '+IntToStr(Result)+' Handle '+IntToHex(hwnd, 8)));
end;

{$IF CompilerVersion >= 30}

// HTHEME WINAPI OpenThemeDataForDpi(HWDN   hwnd, PCWSTR pszClassIdList, UINT   dpi);
function Detour_UxTheme_OpenThemeDataForDPI(hwnd: hwnd; pszClassList: LPCWSTR; dpi: UINT): hTheme; stdcall;
begin
  if not(ExecutingInMainThread) then
  begin
    Result := Trampoline_UxTheme_OpenThemeDataForDPI(hwnd, pszClassList, dpi);
    exit;
  end;

  VCLStylesLock.Enter;
  try
    Result := Trampoline_UxTheme_OpenThemeDataForDPI(hwnd, pszClassList, dpi);
    if THThemesClasses.ContainsKey(Result) then
      THThemesClasses.Remove(Result);
    THThemesClasses.Add(Result, pszClassList);

    if THThemesHWND.ContainsKey(Result) then
      THThemesHWND.Remove(Result);
    THThemesHWND.Add(Result, hwnd);
  finally
    VCLStylesLock.Leave;
  end;
end;
{$IFEND}

function Detour_UxTheme_OpenThemeDataEx(hwnd: hwnd; pszClassList: LPCWSTR; dwFlags: DWORD): hTheme; stdcall;
begin
  if not(ExecutingInMainThread) then
  begin
    Result := Trampoline_UxTheme_OpenThemeDataEx(hwnd, pszClassList, dwFlags);
    exit;
  end;

  // OutputDebugString(PChar('Detour_UxTheme_OpenThemeDataEx '+pszClassList));
  VCLStylesLock.Enter;
  try
    Result := Trampoline_UxTheme_OpenThemeDataEx(hwnd, pszClassList, dwFlags);
    if THThemesClasses.ContainsKey(Result) then
      THThemesClasses.Remove(Result);
    THThemesClasses.Add(Result, pszClassList);

    if THThemesHWND.ContainsKey(Result) then
      THThemesHWND.Remove(Result);
    THThemesHWND.Add(Result, hwnd);
  finally
    VCLStylesLock.Leave;
  end;
  // OutputDebugString(PChar('Detour_UxTheme_OpenThemeDataEx '+pszClassList+' hTheme '+IntToStr(Result)+' Handle '+IntToHex(hwnd, 8)));
end;

function GetStyleHighLightColor: TColor;
begin
  if ColorIsBright(StyleServices.GetSystemColor(clBtnFace)) or
    not ColorIsBright(StyleServices.GetSystemColor(clHighlight)) then
    Result := StyleServices.GetSystemColor(clBtnText)
  else
    Result := StyleServices.GetSystemColor(clHighlight);
end;

function GetStyleBtnTextColor: TColor;
begin
  if not StyleServices.GetElementColor(StyleServices.GetElementDetails(tbPushButtonNormal), ecTextColor, Result) then
    Result := StyleServices.GetSystemColor(clBtnText);
end;

function GetStyleMenuTextColor: TColor;
begin
  Result := StyleServices.GetStyleFontColor(sfPopupMenuItemTextNormal);
end;

function GetThemeClass(hTheme: hTheme; iPartId, iStateId: Integer): string;
var
  hThemeNew: WinApi.UxTheme.hTheme;
begin
  Result := '';
  hThemeNew := Trampoline_UxTheme_OpenThemeData(0, VSCLASS_SCROLLBAR);
  if hThemeNew = hTheme then
    exit(VSCLASS_SCROLLBAR)
  else
    CloseThemeData(hThemeNew);

  hThemeNew := Trampoline_UxTheme_OpenThemeData(0, VSCLASS_LISTVIEW);
  if hThemeNew = hTheme then
    exit(VSCLASS_LISTVIEW)
  else
    CloseThemeData(hThemeNew);

  hThemeNew := Trampoline_UxTheme_OpenThemeData(0, VSCLASS_ITEMSVIEW_HEADER);
  if hThemeNew = hTheme then
    exit(VSCLASS_ITEMSVIEW_HEADER)
  else
    CloseThemeData(hThemeNew);

  hThemeNew := Trampoline_UxTheme_OpenThemeData(0, VSCLASS_NAVIGATION);
  if hThemeNew = hTheme then
    exit(VSCLASS_NAVIGATION)
  else
    CloseThemeData(hThemeNew);

  hThemeNew := Trampoline_UxTheme_OpenThemeData(0, VSCLASS_COMMANDMODULE);
  if hThemeNew = hTheme then
    exit(VSCLASS_COMMANDMODULE)
  else
    CloseThemeData(hThemeNew);

  hThemeNew := Trampoline_UxTheme_OpenThemeData(0, VSCLASS_EDIT);
  if hThemeNew = hTheme then
    exit(VSCLASS_EDIT)
  else
    CloseThemeData(hThemeNew);

  hThemeNew := Trampoline_UxTheme_OpenThemeData(0, VSCLASS_LISTBOX);
  if hThemeNew = hTheme then
    exit(VSCLASS_LISTBOX)
  else
    CloseThemeData(hThemeNew);

  hThemeNew := Trampoline_UxTheme_OpenThemeData(0, VSCLASS_BUTTON);
  if hThemeNew = hTheme then
    exit(VSCLASS_BUTTON)
  else
    CloseThemeData(hThemeNew);

  hThemeNew := Trampoline_UxTheme_OpenThemeData(0, VSCLASS_MENU);
  if hThemeNew = hTheme then
    exit(VSCLASS_MENU)
  else
    CloseThemeData(hThemeNew);

  hThemeNew := Trampoline_UxTheme_OpenThemeData(0, VSCLASS_MENUBAND);
  if hThemeNew = hTheme then
    exit(VSCLASS_MENUBAND)
  else
    CloseThemeData(hThemeNew);
end;

function UxTheme_InfoBar(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd): HRESULT; stdcall;
var
  LDetails: TThemedElementDetails;
  LColor: TColor;
begin
  case iPartId of
    1:
      begin
        case iStateId of
          1: // normal
            begin
              LDetails := StyleServices.GetElementDetails(tpPanelBackground);
              StyleServices.GetElementColor(LDetails, ecFillColor, LColor);
              DrawStyleFillRect(hdc, pRect, LColor);
              exit(S_OK);
            end;

          2: // hot
            begin
              LDetails := StyleServices.GetElementDetails(tpPanelBackground);
              StyleServices.GetElementColor(LDetails, ecFillColor, LColor);
              DrawStyleFillRect(hdc, pRect, LColor);
              exit(S_OK);
            end;

          3: // Pressed
            begin
              LDetails := StyleServices.GetElementDetails(tpPanelBackground);
              StyleServices.GetElementColor(LDetails, ecFillColor, LColor);
              DrawStyleFillRect(hdc, pRect, LColor);
              exit(S_OK);
            end;

          4: // selected
            begin
              LDetails := StyleServices.GetElementDetails(tpPanelBackground);
              StyleServices.GetElementColor(LDetails, ecFillColor, LColor);
              DrawStyleFillRect(hdc, pRect, LColor);
              exit(S_OK);
            end;
        end;

      end;
  end;
  // OutputDebugString(PChar(Format('UxTheme_InfoBar class %s hTheme %d iPartId %d iStateId %d', [LThemeClass, hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_BreadCrumBar(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd): HRESULT; stdcall;
begin
  case iPartId of
    1:
      begin
        case iStateId of
          1:
            begin
              // DrawStyleFillRect(hdc, pRect, clGreen);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbButtonNormal), pRect);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tsArrowBtnLeftNormal), pRect);
              exit(S_OK);
            end;

          2:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbButtonNormal), pRect);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tsArrowBtnLeftNormal), pRect);
              exit(S_OK);
            end;

          3: // Pressed
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbButtonPressed), pRect);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tsArrowBtnLeftPressed), pRect);
              exit(S_OK);
            end;

          8: // fade out (hot)
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbButtonHot), pRect);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tsArrowBtnLeftHot), pRect);
              exit(S_OK);
            end;
        end;

      end;
  end;
  // OutputDebugString(PChar(Format('UxTheme_BreadCrumBar class %s hTheme %d iPartId %d iStateId %d', [LThemeClass, hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_TryHarder(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd): HRESULT; stdcall;
begin
  case iPartId of
    0:
      begin
        DrawStyleFillRect(hdc, pRect, StyleServices.GetSystemColor(clWindow));
        exit(S_OK);
      end;
  end;
  // OutputDebugString(PChar(Format('UxTheme_TryHarder class %s hTheme %d iPartId %d iStateId %d', [LThemeClass, hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_Tab(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd): HRESULT; stdcall;
begin
  case iPartId of
    TABP_TOPTABITEM:
      begin
        case iStateId of
          TIS_NORMAL:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttTabItemNormal), pRect);
              // DrawStyleFillRect(hdc, pRect, clGreen);
              exit(S_OK);
            end;

          TIS_HOT:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttTabItemHot), pRect);
              // DrawStyleFillRect(hdc, pRect, clGreen);
              exit(S_OK);
            end;
          TIS_SELECTED:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttTabItemSelected), pRect);
              // DrawStyleFillRect(hdc, pRect, clGreen);
              exit(S_OK);
            end;
        end;

      end;

    TABP_TOPTABITEMLEFTEDGE:
      begin
        case iStateId of
          TILES_NORMAL:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttTabItemLeftEdgeNormal), pRect);
              // DrawStyleFillRect(hdc, pRect, clGreen);
              exit(S_OK);
            end;

          TILES_HOT:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttTabItemLeftEdgeHot), pRect);
              // DrawStyleFillRect(hdc, pRect, clGreen);
              exit(S_OK);
            end;

          TILES_SELECTED:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttTabItemLeftEdgeSelected), pRect);
              // DrawStyleFillRect(hdc, pRect, clGreen);
              exit(S_OK);
            end;

          TILES_DISABLED:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttTabItemLeftEdgeDisabled), pRect);
              // DrawStyleFillRect(hdc, pRect, clGreen);
              exit(S_OK);
            end;

        end;

      end;

    TABP_TOPTABITEMRIGHTEDGE:
      begin
        case iStateId of
          TIRES_NORMAL:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttTabItemRightEdgeNormal), pRect);
              // DrawStyleFillRect(hdc, pRect, clGreen);
              exit(S_OK);
            end;

          TIRES_HOT:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttTabItemRightEdgeHot), pRect);
              // DrawStyleFillRect(hdc, pRect, clGreen);
              exit(S_OK);
            end;

          TIRES_SELECTED:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttTabItemRightEdgeSelected), pRect);
              // DrawStyleFillRect(hdc, pRect, clGreen);
              exit(S_OK);
            end;

          TIRES_DISABLED:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttTabItemRightEdgeDisabled), pRect);
              // DrawStyleFillRect(hdc, pRect, clGreen);
              exit(S_OK);
            end;
        end;

      end;

    TABP_PANE:
      begin
        DrawStyleElement(hdc, StyleServices.GetElementDetails(ttPane), pRect);
        exit(S_OK);
      end;
  end;
  // OutputDebugString(PChar(Format('UxTheme_Tab class %s hTheme %d iPartId %d iStateId %d', [LThemeClass, hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_ListViewPopup(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd): HRESULT; stdcall;
begin
  case iPartId of
    0:
      begin
        DrawStyleFillRect(hdc, pRect, StyleServices.GetSystemColor(clWindow));
        // Windows Vista - W7
        if (TOSVersion.Major = 6) and ((TOSVersion.Minor = 0) or (TOSVersion.Minor = 1)) then
          SetTextColor(hdc, ColorToRGB(StyleServices.GetSystemColor(clWindowText)));
        exit(S_OK);
      end;
  end;
  // OutputDebugString(PChar(Format('UxTheme_ListViewPopup class %s hTheme %d iPartId %d iStateId %d', [LThemeClass, hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_ToolTip(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd): HRESULT; stdcall;
var
  LStartColor, LEndColor: TColor;
  LCanvas: TCanvas;
  SaveIndex: Integer;
begin
  case iPartId of

    TTP_STANDARD:
      begin
        DrawStyleFillRect(hdc, pRect, StyleServices.GetStyleColor(TStyleColor.scPanel));
        exit(S_OK);
      end;

    TTP_BALLOON:
      begin
        // DrawStyleFillRect(hdc, pRect, StyleServices.GetStyleColor(TStyleColor.scPanel));
        LStartColor := GetHighLightColor(StyleServices.GetStyleColor(TStyleColor.scPanel), 10);
        LEndColor := StyleServices.GetStyleColor(TStyleColor.scPanel);

        LCanvas := TCanvas.Create;
        SaveIndex := SaveDC(hdc);
        try
          LCanvas.Handle := hdc;

          GradientFillCanvas(LCanvas, LStartColor, LEndColor, Rect(pRect.Left, pRect.Top, pRect.Width, pRect.Bottom),
            TGradientDirection.gdVertical);
        finally
          LCanvas.Handle := 0;
          LCanvas.Free;
          RestoreDC(hdc, SaveIndex);
        end;
        exit(S_OK);
      end;

    TTP_BALLOONSTEM:
      begin
        // if iStateId = 0 then
        DrawStyleFillRect(hdc, pRect, GetHighLightColor(StyleServices.GetStyleColor(TStyleColor.scPanel), 10));
        exit(S_OK);
      end;
  end;
  // OutputDebugString(PChar(Format('UxTheme_ToolTip class %s hTheme %d iPartId %d iStateId %d', [LThemeClass, hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_TrackBar(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd): HRESULT; stdcall;
begin
  case iPartId of
    TKP_TRACKVERT:
      begin
        case iStateId of
          TKS_NORMAL:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbTrackVert), pRect);
              exit(S_OK);
            end;
        end;
      end;

    TKP_THUMBRIGHT:
      begin
        case iStateId of
          TUS_NORMAL:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbThumbRightNormal), pRect);
              exit(S_OK);
            end;

          TUS_HOT:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbThumbRightHot), pRect);
              exit(S_OK);
            end;

          TUS_PRESSED:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbThumbRightPressed), pRect);
              exit(S_OK);
            end;

          TUS_FOCUSED:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbThumbRightFocused), pRect);
              exit(S_OK);
            end;

          TUS_DISABLED:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbThumbRightDisabled), pRect);
              exit(S_OK);
            end;
        end;
      end;
  end;

  // OutputDebugString(PChar(Format('UxTheme_TrackBar class %s hTheme %d iPartId %d iStateId %d', [LThemeClass, hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_PreviewPane(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd): HRESULT; stdcall;
var
  LColor: TColor;
begin
  case iPartId of
    1:
      begin
        case iStateId of
          // background
          1:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tcpThemedHeader), pRect);
              exit(S_OK);
            end;
        end;
      end;

    3:
      begin
        case iStateId of
          // left border of listview
          0:
            begin
              StyleServices.GetElementColor(StyleServices.GetElementDetails(tpPanelBackground), ecFillColor, LColor);
              DrawStyleFillRect(hdc, pRect, LColor);
              exit(S_OK);
            end;
        end;
      end;

    4:
      begin
        case iStateId of
          // left border of preview pane
          0:
            begin
              StyleServices.GetElementColor(StyleServices.GetElementDetails(tpPanelBackground), ecFillColor, LColor);
              DrawStyleFillRect(hdc, pRect, LColor);
              exit(S_OK);
            end;
        end;
      end;
  end;

  // OutputDebugString(PChar(Format('UxTheme_PreviewPane class %s hTheme %d iPartId %d iStateId %d', [LThemeClass, hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_ToolBar(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd): HRESULT; stdcall;
var
  LDetails: TThemedElementDetails;
  LRect: TRect;

begin
  case iPartId of
    0:
      begin
        case iStateId of

          0:
            begin
              if (hwnd <> 0) then
                DrawStyleParentBackground(hwnd, hdc, pRect);

              LDetails.Element := teToolBar;
              LDetails.Part := 0;
              LDetails.State := 0;
              // DrawStyleFillRect(hdc, pRect, clYellow);
              // DrawStyleElement(hdc, StyleServices.GetElementDetails(tmPopupBackground), pRect);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tmPopupBackground), pRect);
              exit(S_OK);
            end;

        end;
      end;

    TP_BUTTON:
      begin
        case iStateId of

          TS_NORMAL:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbButtonNormal), pRect);
              exit(S_OK);
            end;

          TS_HOT:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbButtonHot), pRect);
              exit(S_OK);
            end;

          TS_PRESSED:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbButtonPressed), pRect);
              exit(S_OK);
            end;

          TS_NEARHOT:
            begin
              // DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbButtonNearHot), pRect);
              // DrawStyleFillRect(hdc, pRect, clRed);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbButtonHot), pRect);
              exit(S_OK);
            end;
        end;
      end;

    TP_SPLITBUTTON:

      begin
        case iStateId of

          TS_NORMAL:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonNormal), pRect);
              exit(S_OK);
            end;

          TS_HOT:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonHot), pRect);
              exit(S_OK);
            end;

          TS_PRESSED:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonPressed), pRect);
              exit(S_OK);
            end;

          TS_NEARHOT:
            begin
              // DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonNearHot), pRect);
              // DrawStyleFillRect(hdc, pRect, clRed);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonHot), pRect);
              exit(S_OK);
            end;

          TS_OTHERSIDEHOT:
            begin
              // DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonOtherSideHot), pRect);
              // DrawStyleFillRect(hdc, pRect, clBlue);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonHot), pRect);
              exit(S_OK);
            end;

        end;
      end;

    TP_SPLITBUTTONDROPDOWN:
      begin
        case iStateId of

          TS_NORMAL:
            begin
              LRect := pRect;
              LRect.Top := LRect.Top + 9;
              LRect.Left := LRect.Left + 3;
              DrawStyleArrow(hdc, TScrollDirection.sdDown, LRect.Location, 3,
                StyleServices.GetSystemColor(clWindowText));
              //
              // DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonDropDownNormal), pRect);
              exit(S_OK);
            end;

          TS_HOT:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonDropDownHot), pRect);
              exit(S_OK);
            end;

          TS_PRESSED:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonDropDownPressed), pRect);
              exit(S_OK);
            end;

          TS_OTHERSIDEHOT:
            begin
              // DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonDropDownOtherSideHot), pRect);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonDropDownHot), pRect);
              exit(S_OK);
            end;
        end;
      end;
  end;
  // OutputDebugString(PChar(Format('UxTheme_ToolBar hTheme %d iPartId %d iStateId %d', [hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_Rebar(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd = 0): HRESULT; stdcall;
begin
  case iPartId of
    RP_BAND:
      begin
        // DrawStyleElement(hdc, StyleServices.GetElementDetails(trBand), pRect);
        DrawStyleFillRect(hdc, pRect, StyleServices.GetSystemColor(clBtnFace));
        exit(S_OK);
      end;

    RP_BACKGROUND:
      begin
        // DrawStyleElement(hdc, StyleServices.GetElementDetails(trBackground), pRect);
        DrawStyleFillRect(hdc, pRect, StyleServices.GetSystemColor(clBtnFace));
        exit(S_OK);
      end
  end;

  // OutputDebugString(PChar(Format('UxTheme_Rebar hTheme %d iPartId %d iStateId %d', [hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_AddressBand(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd): HRESULT; stdcall;
begin
  case iPartId of
    // address bar control
    1:
      begin
        case iStateId of
          // normal
          1:
            begin
              // DrawStyleElement(hdc, StyleServices.GetElementDetails(teEditBorderNoScrollNormal), pRect);
              DrawStyleFillRect(hdc, pRect, StyleServices.GetSystemColor(clWindow));
              DrawStyleRectangle(hdc, pRect, StyleServices.GetSystemColor(clBtnShadow));
              exit(S_OK);
            end;
          // hot
          2:
            begin
              // DrawStyleElement(hdc, StyleServices.GetElementDetails(teEditBorderNoScrollHot), pRect);
              DrawStyleFillRect(hdc, pRect, StyleServices.GetSystemColor(clWindow));
              DrawStyleRectangle(hdc, pRect, StyleServices.GetSystemColor(clBtnShadow));
              exit(S_OK);
            end;
          // editing
          4:
            begin
              // DrawStyleElement(hdc, StyleServices.GetElementDetails(teEditBorderNoScrollFocused), pRect);
              DrawStyleFillRect(hdc, pRect, StyleServices.GetSystemColor(clWindow));
              DrawStyleRectangle(hdc, pRect, StyleServices.GetSystemColor(clBtnShadow));
              exit(S_OK);
            end;

        end;

      end;
  end;

  // OutputDebugString(PChar(Format('UxTheme_AddressBand hTheme %d iPartId %d iStateId %d', [hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_SearchBox(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd): HRESULT; stdcall;
begin
  // OutputDebugString(PChar(Format('UxTheme_SearchBox hTheme %d iPartId %d iStateId %d', [hTheme, iPartId, iStateId])));

  case iPartId of
    // searchbox control
    1:
      begin
        case iStateId of
          // normal
          1:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(teEditBorderNoScrollNormal), pRect);
              exit(S_OK);
            end;
          // hot
          2:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(teEditBorderNoScrollHot), pRect);
              exit(S_OK);
            end;

          4: // editing
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(teEditBorderNoScrollFocused), pRect);
              exit(S_OK);
            end;
        end;
      end;


    // X
    // 2 :
    // begin
    // case iStateId of
    //
    // 1:
    // begin
    // AwesomeFont.DrawChar(hdc, fa_remove, pRect, StyleServices.GetSystemColor(clWindowText));
    // Exit(S_OK);
    // end;
    //
    // 2,  //hot
    // 3:  //pressed
    //
    // begin
    // LColor := StyleServices.GetSystemColor(clHighlight);
    // AwesomeFont.DrawChar(hdc, fa_remove, pRect, LColor);
    // //AlphaBlendRectangle(hdc, LColor, pRect, 32);
    // Exit(S_OK);
    // end;
    // end;
    // end;

    // Magnifier
    3:
      begin
        case iStateId of

          1: // normal
            begin
              FontAwesome.DrawChar(hdc, fa_search, pRect, StyleServices.GetSystemColor(clHighlight));
              exit(S_OK);
            end;
        end;
      end;
  end;

  // OutputDebugString(PChar(Format('UxTheme_SearchBox hTheme %d iPartId %d iStateId %d', [hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_MonthCal(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd = 0): HRESULT; stdcall;
var
  LDetails: TThemedElementDetails;
  SaveIndex: Integer;
  LRect: TRect;
  LStartColor, LEndColor: TColor;
  LCanvas: TCanvas;
begin
  case iPartId of
    MC_BORDERS:
      begin
        DrawStyleElement(hdc, StyleServices.GetElementDetails(teEditBorderNoScrollNormal), pRect);
        exit(S_OK);
      end;

    MC_BACKGROUND, MC_GRIDBACKGROUND:
      begin
        DrawStyleFillRect(hdc, pRect, StyleServices.GetStyleColor(scGrid));
        exit(S_OK);
      end;

    MC_COLHEADERSPLITTER:
      begin
        LStartColor := StyleServices.GetSystemColor(clWindow);
        LEndColor := StyleServices.GetSystemColor(clHighlight);

        LCanvas := TCanvas.Create;
        SaveIndex := SaveDC(hdc);
        try
          LCanvas.Handle := hdc;
          LRect := pRect;

          GradientFillCanvas(LCanvas, LStartColor, LEndColor, Rect(LRect.Left, LRect.Top, LRect.Width div 2,
            LRect.Bottom), TGradientDirection.gdHorizontal);

          GradientFillCanvas(LCanvas, LEndColor, LStartColor, Rect(LRect.Width div 2, LRect.Top, LRect.Width,
            LRect.Bottom), TGradientDirection.gdHorizontal);

        finally
          LCanvas.Handle := 0;
          LCanvas.Free;
          RestoreDC(hdc, SaveIndex);
        end;

        exit(S_OK);
      end;

    MC_GRIDCELLBACKGROUND:
      begin

        LStartColor := StyleServices.GetSystemColor(clHighlight);
        if iStateId = MCGCB_TODAY then
          AlphaBlendRectangle(hdc, LStartColor, pRect, 200)
        else
          AlphaBlendRectangle(hdc, LStartColor, pRect, 96);

        exit(S_OK);
      end;

    MC_NAVNEXT:
      begin
        case iStateId of
          MCNN_NORMAL:
            LDetails := StyleServices.GetElementDetails(tsArrowBtnRightNormal);
          MCNN_HOT:
            LDetails := StyleServices.GetElementDetails(tsArrowBtnRightHot);
          MCNN_PRESSED:
            LDetails := StyleServices.GetElementDetails(tsArrowBtnRightPressed);
          MCNN_DISABLED:
            LDetails := StyleServices.GetElementDetails(tsArrowBtnRightDisabled);
        end;

        DrawStyleElement(hdc, LDetails, pRect);
        exit(S_OK);
      end;

    MC_NAVPREV:
      begin
        case iStateId of
          MCNP_NORMAL:
            LDetails := StyleServices.GetElementDetails(tsArrowBtnLeftNormal);
          MCNP_HOT:
            LDetails := StyleServices.GetElementDetails(tsArrowBtnLeftHot);
          MCNP_PRESSED:
            LDetails := StyleServices.GetElementDetails(tsArrowBtnLeftPressed);
          MCNP_DISABLED:
            LDetails := StyleServices.GetElementDetails(tsArrowBtnLeftDisabled);
        end;

        DrawStyleElement(hdc, LDetails, pRect);
        exit(S_OK);
      end;
  end;

  // OutputDebugString(PChar(Format('UxTheme_MonthCal hTheme %d iPartId %d iStateId %d', [hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_DatePicker(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd = 0): HRESULT; stdcall;
var
  LDetails: TThemedElementDetails;
  LRect: TRect;
  LColor: TColor;
begin
  case iPartId of
    DP_DATEBORDER:
      begin
        case iStateId of
          DPDB_NORMAL:
            LDetails := StyleServices.GetElementDetails(teEditBorderNoScrollNormal);
          DPDB_HOT:
            LDetails := StyleServices.GetElementDetails(teEditBorderNoScrollHot);
          DPDB_FOCUSED:
            LDetails := StyleServices.GetElementDetails(teEditBorderNoScrollFocused);
          DPDB_DISABLED:
            LDetails := StyleServices.GetElementDetails(teEditBorderNoScrollDisabled);
        end;

        DrawStyleElement(hdc, LDetails, pRect);
        exit(S_OK);
      end;

    DP_SHOWCALENDARBUTTONRIGHT:
      begin

        case iStateId of
          DPSCBR_NORMAL:
            LDetails := StyleServices.GetElementDetails(tcBorderNormal);
          DPSCBR_HOT:
            LDetails := StyleServices.GetElementDetails(tcBorderHot);
          DPSCBR_PRESSED:
            LDetails := StyleServices.GetElementDetails(tcBorderHot);
          DPSCBR_DISABLED:
            LDetails := StyleServices.GetElementDetails(tcBorderDisabled);
        end;

        DrawStyleElement(hdc, LDetails, pRect);

        case iStateId of
          DPSCBR_NORMAL:
            begin
              LColor := StyleServices.GetSystemColor(clWindowText);
            end;

          DPSCBR_HOT:
            begin
              LColor := GetStyleHighLightColor;
            end;

          DPSCBR_PRESSED:
            begin
              LColor := GetStyleHighLightColor;
            end;

          DPSCBR_DISABLED:
            begin
              LColor := StyleServices.GetSystemColor(clGrayText);
            end;

        else
          LColor := StyleServices.GetSystemColor(clWindowText);
        end;

        LRect := pRect;
        InflateRect(LRect, -2, -2);
        DrawStyleFillRect(hdc, LRect, StyleServices.GetStyleColor(TStyleColor.scEdit));

        LRect := Rect(0, 0, 14, 14);
        RectVCenter(LRect, pRect);
        OffsetRect(LRect, (pRect.Width - LRect.Width) div 2, 0);
        FontAwesome.DrawChar(hdc, fa_calendar_o, LRect, LColor);

        exit(S_OK);
      end;
  end;

  // OutputDebugString(PChar(Format('UxTheme_DatePicker hTheme %d iPartId %d iStateId %d', [hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_ListView(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd = 0): HRESULT; stdcall;
var
  LDetails: TThemedElementDetails;
  SaveIndex: Integer;
  LCanvas: TCanvas;
  LRect: TRect;
  LColor, LStartColor: TColor;
begin
  case iPartId of
    LVP_LISTITEM:
      begin
        case iStateId of
          LIS_HOT, LISS_HOTSELECTED, LIS_SELECTEDNOTFOCUS, LIS_SELECTED:
            begin
              LColor := StyleServices.GetSystemColor(clHighlight);
              LRect := pRect;
              if not SameText(LThemeClass, VSCLASS_LISTVIEW) then
                InflateRect(LRect, -1, -1);

              if iStateId = LISS_HOTSELECTED then
                AlphaBlendRectangle(hdc, LColor, LRect, 96)
              else
                AlphaBlendRectangle(hdc, LColor, LRect, 50);

              exit(S_OK);
            end;
        end;
      end;

    LVP_LISTDETAIL:
      begin
        case iStateId of

          LIS_NORMAL, LIS_HOT:
            begin
              LColor := StyleServices.GetSystemColor(clHighlight);
              LRect := pRect;
              if not SameText(LThemeClass, VSCLASS_LISTVIEW) then
                InflateRect(LRect, -1, -1);

              if iStateId = LIS_HOT then
                AlphaBlendRectangle(hdc, LColor, LRect, 96)
              else
                AlphaBlendRectangle(hdc, LColor, LRect, 50);

              exit(S_OK);
            end;
        end;

      end;
    LVP_EXPANDBUTTON:
      begin
        case iStateId of
          LVEB_NORMAL:
            LDetails := StyleServices.GetElementDetails(tcpThemedChevronOpenedNormal);
          LVEB_HOVER:
            LDetails := StyleServices.GetElementDetails(tcpThemedChevronOpenedHot);
          LVEB_PUSHED:
            LDetails := StyleServices.GetElementDetails(tcpThemedChevronOpenedPressed);
        else
          LDetails := StyleServices.GetElementDetails(tcpThemedChevronOpenedNormal);
        end;

        SaveIndex := SaveDC(hdc);
        try
          if hwnd <> 0 then
            DrawStyleParentBackground(hwnd, hdc, pRect);
          StyleServices.DrawElement(hdc, LDetails, pRect, nil);
        finally
          RestoreDC(hdc, SaveIndex);
        end;
        exit(S_OK);
      end;

    LVP_COLLAPSEBUTTON:
      begin
        case iStateId of
          LVCB_NORMAL:
            LDetails := StyleServices.GetElementDetails(tcpThemedChevronClosedNormal);
          LVCB_HOVER:
            LDetails := StyleServices.GetElementDetails(tcpThemedChevronClosedHot);
          LVCB_PUSHED:
            LDetails := StyleServices.GetElementDetails(tcpThemedChevronClosedPressed);
        else
          LDetails := StyleServices.GetElementDetails(tcpThemedChevronClosedNormal);
        end;

        SaveIndex := SaveDC(hdc);
        try
          if hwnd <> 0 then
            DrawStyleParentBackground(hwnd, hdc, pRect);
          StyleServices.DrawElement(hdc, LDetails, pRect, nil);
        finally
          RestoreDC(hdc, SaveIndex);
        end;
        exit(S_OK);
      end;
    LVP_GROUPHEADER:
      begin
        case iStateId of
          LVGH_OPENMIXEDSELECTIONHOT, LVGH_OPENSELECTED, LVGH_OPENSELECTEDNOTFOCUSEDHOT, LVGH_OPENSELECTEDHOT,
            LVGH_CLOSEHOT, LVGH_CLOSESELECTEDHOT, LVGH_CLOSESELECTEDNOTFOCUSEDHOT, LVGHL_CLOSESELECTED,
            LVGH_CLOSESELECTEDNOTFOCUSED, LVGH_CLOSEMIXEDSELECTION, LVGH_CLOSEMIXEDSELECTIONHOT, LVGH_OPENHOT:
            begin
              LRect := pRect;
              InflateRect(LRect, -1, -1);
              AlphaBlendRectangle(hdc, StyleServices.GetSystemColor(clHighlight), LRect, 96);
              exit(S_OK);
            end;
        end;

      end;
    LVP_GROUPHEADERLINE:
      begin

        case iStateId of
          LVGHL_CLOSEHOT, LVGHL_OPENHOT, LVGHL_OPENMIXEDSELECTIONHOT, LVGHL_OPENSELECTEDNOTFOCUSEDHOT,
            LVGHL_CLOSESELECTEDNOTFOCUSEDHOT, LVGHL_OPENSELECTED, LVGHL_CLOSESELECTED, LVGHL_CLOSESELECTEDHOT,
            LVGHL_CLOSEMIXEDSELECTION, LVGHL_CLOSEMIXEDSELECTIONHOT, LVGHL_OPENSELECTEDHOT:
            begin
              LColor := StyleServices.GetSystemColor(clWindowText);
              LCanvas := TCanvas.Create;
              SaveIndex := SaveDC(hdc);
              try
                LCanvas.Handle := hdc;
                LRect := pRect;
                GradientFillCanvas(LCanvas, StyleServices.GetSystemColor(LColor),
                  StyleServices.GetStyleColor(TStyleColor.scEdit) { StyleServices.GetSystemColor(clWindow) } , LRect,
                  TGradientDirection.gdHorizontal);
              finally
                LCanvas.Handle := 0;
                LCanvas.Free;
                RestoreDC(hdc, SaveIndex);
              end;

              exit(S_OK);
            end;

          LVGHL_CLOSE, LVGHL_OPENSELECTEDNOTFOCUSED, LVGHL_OPEN, LVGHL_OPENMIXEDSELECTION:
            begin
              LColor := StyleServices.GetSystemColor(clWindowText);
              LCanvas := TCanvas.Create;
              SaveIndex := SaveDC(hdc);
              try
                LCanvas.Handle := hdc;
                LRect := pRect;
                GradientFillCanvas(LCanvas, StyleServices.GetSystemColor(LColor),
                  StyleServices.GetStyleColor(TStyleColor.scEdit), LRect, TGradientDirection.gdHorizontal);
              finally
                LCanvas.Handle := 0;
                LCanvas.Free;
                RestoreDC(hdc, SaveIndex);
              end;

              exit(S_OK);
            end;
        end;

      end;

    LVP_COLUMNDETAIL:
      begin
        LColor := StyleServices.GetSystemColor(clWindow);
        DrawStyleFillRect(hdc, pRect, LColor);
        exit(S_OK)
      end;
  end;

  // OutputDebugString(PChar(Format('UxTheme_ListView hTheme %d iPartId %d iStateId %d', [hTheme, iPartId, iStateId])));
  // DrawStyleFillRect(hdc, pRect, clYellow);
  // Exit(S_OK)
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_Header(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd = 0): HRESULT; stdcall;
var
  LDetails: TThemedElementDetails;
  LRect: TRect;
  LColor: TColor;
  SaveIndex: Integer;
begin
  case iPartId of
    0:
      begin
        DrawStyleElement(hdc, StyleServices.GetElementDetails(tcpThemedHeader), pRect);
        exit(S_OK);
      end;

    HP_HEADERITEM:
      begin
        case iStateId of
          HIS_NORMAL:
            LDetails := StyleServices.GetElementDetails(thHeaderItemNormal);
          HIS_HOT:
            LDetails := StyleServices.GetElementDetails(thHeaderItemHot);
          HIS_PRESSED:
            LDetails := StyleServices.GetElementDetails(thHeaderItemPressed);

          HIS_SORTEDNORMAL:
            LDetails := StyleServices.GetElementDetails(thHeaderItemNormal);
          HIS_SORTEDHOT:
            LDetails := StyleServices.GetElementDetails(thHeaderItemHot);
          HIS_SORTEDPRESSED:
            LDetails := StyleServices.GetElementDetails(thHeaderItemPressed);

          HIS_ICONNORMAL:
            LDetails := StyleServices.GetElementDetails(thHeaderItemNormal);
          HIS_ICONHOT:
            LDetails := StyleServices.GetElementDetails(thHeaderItemHot);
          HIS_ICONPRESSED:
            LDetails := StyleServices.GetElementDetails(thHeaderItemPressed);

          HIS_ICONSORTEDNORMAL:
            LDetails := StyleServices.GetElementDetails(thHeaderItemNormal);
          HIS_ICONSORTEDHOT:
            LDetails := StyleServices.GetElementDetails(thHeaderItemHot);
          HIS_ICONSORTEDPRESSED:
            LDetails := StyleServices.GetElementDetails(thHeaderItemPressed);
        else
          LDetails := StyleServices.GetElementDetails(thHeaderItemNormal);
        end;

        SaveIndex := SaveDC(hdc);
        try
          if hwnd <> 0 then
            DrawStyleParentBackground(hwnd, hdc, pRect);
          StyleServices.DrawElement(hdc, LDetails, pRect, nil);
        finally
          RestoreDC(hdc, SaveIndex);
        end;

        exit(S_OK);
      end;

    HP_HEADERSORTARROW:
      begin
        // case iStateId of
        // HSAS_SORTEDUP    : LDetails := StyleServices.GetElementDetails(thHeaderSortArrowSortedUp);
        // HSAS_SORTEDDOWN  : LDetails := StyleServices.GetElementDetails(thHeaderSortArrowSortedDown);
        // end;

        LColor := GetStyleHighLightColor();
        LRect := pRect;
        LRect.Top := LRect.Top + 3;
        if (iStateId = HSAS_SORTEDUP) then
          DrawStyleArrow(hdc, TScrollDirection.sdUp, LRect.Location, 3, LColor)
        else
          DrawStyleArrow(hdc, TScrollDirection.sdDown, LRect.Location, 3, LColor);

        exit(S_OK);
      end;

    HP_HEADERDROPDOWN:
      begin
        case iStateId of
          HDDS_NORMAL:
            LDetails := StyleServices.GetElementDetails(ttbSplitButtonDropDownNormal);
            // tcDropDownButtonNormal, thHeaderDropDownNormal
          HDDS_SOFTHOT:
            LDetails := StyleServices.GetElementDetails(ttbSplitButtonDropDownHot);
            // tcDropDownButtonHot, thHeaderDropDownSoftHot
          HDDS_HOT:
            LDetails := StyleServices.GetElementDetails(ttbSplitButtonDropDownHot);
            // tcDropDownButtonHot, thHeaderDropDownHot
        end;

        DrawStyleElement(hdc, LDetails, pRect);
        exit(S_OK);
      end;

    HP_HEADERDROPDOWNFILTER:
      begin
        case iStateId of
          HDDFS_NORMAL:
            LDetails := StyleServices.GetElementDetails(ttbSplitButtonDropDownNormal);
            // tcDropDownButtonNormal, thHeaderDropDownNormal
          HDDFS_SOFTHOT:
            LDetails := StyleServices.GetElementDetails(ttbSplitButtonDropDownHot);
            // tcDropDownButtonHot, thHeaderDropDownSoftHot
          HDDFS_HOT:
            LDetails := StyleServices.GetElementDetails(ttbSplitButtonDropDownHot);
            // tcDropDownButtonHot, thHeaderDropDownHot
        end;

        DrawStyleElement(hdc, LDetails, pRect);
        exit(S_OK);
      end;
  end;

  // OutputDebugString(PChar(Format('UxTheme_Header hTheme %d iPartId %d iStateId %d', [hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_ComboBox(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd = 0): HRESULT; stdcall;
var
  LRect: TRect;
begin
  case iPartId of
    CP_BORDER:
      begin
        case iStateId of
          CBB_NORMAL:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tcBorderNormal), pRect);
              exit(S_OK);
            end;
          CBB_HOT:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tcBorderHot), pRect);
              exit(S_OK);
            end;
          CBB_FOCUSED:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tcBorderFocused), pRect);
              exit(S_OK);
            end;
          CBB_DISABLED:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tcBorderDisabled), pRect);
              exit(S_OK);
            end;
        end;
      end;

    CP_DROPDOWNBUTTONRIGHT:
      begin
        LRect := pRect;
        InflateRect(LRect, -2, -2);
        case iStateId of
          CBXSR_NORMAL:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tcDropDownButtonNormal), LRect);
              exit(S_OK);
            end;
          CBXSR_HOT:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tcDropDownButtonHot), LRect);
              exit(S_OK);
            end;
          CBXSR_PRESSED:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tcDropDownButtonPressed), LRect);
              exit(S_OK);
            end;
          CBXSR_DISABLED:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tcDropDownButtonDisabled), LRect);
              exit(S_OK);
            end;
        end;
      end;
  end;

  // OutputDebugString(PChar(Format('UxTheme_ComboBox  class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_Spin(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd = 0): HRESULT; stdcall;
var
  LDetails: TThemedElementDetails;
  LRect: TRect;
  LColor: TColor;
begin
  case iPartId of
    SPNP_UP:
      begin
        case iStateId of
          UPS_NORMAL:
            LDetails := StyleServices.GetElementDetails(tsUpNormal);
          UPS_HOT:
            LDetails := StyleServices.GetElementDetails(tsUpHot);
          UPS_PRESSED:
            LDetails := StyleServices.GetElementDetails(tsUpPressed);
          UPS_DISABLED:
            LDetails := StyleServices.GetElementDetails(tsUpDisabled);
        end;

        LColor := StyleServices.GetStyleFontColor(TStyleFont.sfButtonTextNormal);
        case iStateId of
          UPS_NORMAL:
            LColor := StyleServices.GetStyleFontColor(TStyleFont.sfButtonTextNormal);
          UPS_HOT:
            LColor := StyleServices.GetStyleFontColor(TStyleFont.sfButtonTextHot);
          UPS_PRESSED:
            LColor := StyleServices.GetStyleFontColor(TStyleFont.sfButtonTextPressed);
          UPS_DISABLED:
            LColor := StyleServices.GetStyleFontColor(TStyleFont.sfButtonTextDisabled);
        end;

        DrawStyleElement(hdc, LDetails, pRect);
        LRect := pRect;
        LRect.Top := LRect.Top + 3;
        LRect.Left := LRect.Left + 5;
        DrawStyleArrow(hdc, TScrollDirection.sdUp, LRect.Location, 2, LColor);

        exit(S_OK);
      end;

    SPNP_DOWN:
      begin
        case iStateId of
          DNS_NORMAL:
            LDetails := StyleServices.GetElementDetails(tsDownNormal);
          DNS_HOT:
            LDetails := StyleServices.GetElementDetails(tsDownHot);
          DNS_PRESSED:
            LDetails := StyleServices.GetElementDetails(tsDownPressed);
          DNS_DISABLED:
            LDetails := StyleServices.GetElementDetails(tsDownDisabled);
        end;

        LColor := StyleServices.GetStyleFontColor(TStyleFont.sfButtonTextNormal);
        case iStateId of
          DNS_NORMAL:
            LColor := StyleServices.GetStyleFontColor(TStyleFont.sfButtonTextNormal);
          DNS_HOT:
            LColor := StyleServices.GetStyleFontColor(TStyleFont.sfButtonTextHot);
          DNS_PRESSED:
            LColor := StyleServices.GetStyleFontColor(TStyleFont.sfButtonTextPressed);
          DNS_DISABLED:
            LColor := StyleServices.GetStyleFontColor(TStyleFont.sfButtonTextDisabled);
        end;

        DrawStyleElement(hdc, LDetails, pRect);
        LRect := pRect;
        LRect.Top := LRect.Top + 3;
        LRect.Left := LRect.Left + 5;
        DrawStyleArrow(hdc, TScrollDirection.sdDown, LRect.Location, 2, LColor);
        exit(S_OK);
      end;
  end;

  // OutputDebugString(PChar(Format('UxTheme_Spin  class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_ListBox(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd = 0): HRESULT; stdcall;
var
  LDetails: TThemedElementDetails;
begin
  case iPartId of
    LBCP_BORDER_NOSCROLL:
      begin
        case iStateId of
          LBPSN_NORMAL:
            LDetails := StyleServices.GetElementDetails(teEditBorderNoScrollNormal);
          LBPSN_FOCUSED:
            LDetails := StyleServices.GetElementDetails(teEditBorderNoScrollFocused);
          LBPSN_HOT:
            LDetails := StyleServices.GetElementDetails(teEditBorderNoScrollHot);
          LBPSN_DISABLED:
            LDetails := StyleServices.GetElementDetails(teEditBorderNoScrollDisabled);
        end;

        DrawStyleElement(hdc, LDetails, pRect);
        exit(S_OK);
      end;
  end;

  // OutputDebugString(PChar(Format('UxTheme_ListBox  class %s hTheme %d iPartId %d iStateId %d', [LThemeClass, hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_Navigation(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd): HRESULT; stdcall;
var
  LColor: TColor;
  LCanvas: TCanvas;
  LRect: TRect;
  LBitmap: TBitmap;
  LIcon: Word;
begin
  case iPartId of
    1, // left  button
    2: // right button
      begin

        case iStateId of
          1, // enabled left
          2, // hot  left
          3, // pressed left
          4: // disabled left
            begin
              LBitmap := TBitmap.Create;
              try
                if iPartId = 1 then
                  LIcon := fa_arrow_left
                else
                  LIcon := fa_arrow_right;

                LBitmap.PixelFormat := pf24bit;
                LBitmap.SetSize(pRect.Width, pRect.Height);
                LRect := Rect(0, 0, LBitmap.Width, LBitmap.Height);

                LColor := StyleServices.GetSystemColor(clBtnFace);
                DrawStyleFillRect(LBitmap.Canvas.Handle, pRect, LColor);

                case iStateId of

                  1:
                    begin
                      DrawStyleElement(LBitmap.Canvas.Handle, StyleServices.GetElementDetails(ttbButtonNormal), pRect);
                      // DrawStyleElement(LBitmap.Canvas.Handle, StyleServices.GetElementDetails(tbCommandLinkGlyphNormal), LRect);
                      LRect := Rect(0, 0, 16, 16);
                      RectVCenter(LRect, pRect);
                      OffsetRect(LRect, (pRect.Width - LRect.Width) div 2, 0);
                      FontAwesome.DrawChar(LBitmap.Canvas.Handle, LIcon, LRect,
                        StyleServices.GetSystemColor(clBtnText));
                    end;
                  2:
                    begin
                      DrawStyleElement(LBitmap.Canvas.Handle, StyleServices.GetElementDetails(ttbButtonHot), pRect);
                      // DrawStyleElement(LBitmap.Canvas.Handle, StyleServices.GetElementDetails(tbCommandLinkGlyphHot), LRect);
                      LRect := Rect(0, 0, 16, 16);
                      RectVCenter(LRect, pRect);
                      OffsetRect(LRect, (pRect.Width - LRect.Width) div 2, 0);
                      FontAwesome.DrawChar(LBitmap.Canvas.Handle, LIcon, LRect,
                        StyleServices.GetSystemColor(clHighlight));
                    end;

                  3:
                    begin
                      DrawStyleElement(LBitmap.Canvas.Handle, StyleServices.GetElementDetails(ttbButtonPressed), pRect);
                      // DrawStyleElement(LBitmap.Canvas.Handle, StyleServices.GetElementDetails(tbCommandLinkGlyphPressed), LRect);
                      LRect := Rect(0, 0, 16, 16);
                      RectVCenter(LRect, pRect);
                      OffsetRect(LRect, (pRect.Width - LRect.Width) div 2, 0);
                      FontAwesome.DrawChar(LBitmap.Canvas.Handle, LIcon, LRect,
                        StyleServices.GetSystemColor(clHighlight));
                    end;

                  4:
                    begin
                      DrawStyleElement(LBitmap.Canvas.Handle,
                        StyleServices.GetElementDetails(ttbButtonDisabled), pRect);
                      // DrawStyleElement(LBitmap.Canvas.Handle, StyleServices.GetElementDetails(tbCommandLinkGlyphDisabled), LRect);
                      LRect := Rect(0, 0, 16, 16);
                      RectVCenter(LRect, pRect);
                      OffsetRect(LRect, (pRect.Width - LRect.Width) div 2, 0);
                      FontAwesome.DrawChar(LBitmap.Canvas.Handle, LIcon, LRect,
                        StyleServices.GetSystemColor(clGrayText));
                    end;
                end;

                // FlipBitmap24Horizontal(LBitmap);
                LCanvas := TCanvas.Create;
                try
                  LCanvas.Handle := hdc;
                  LCanvas.Draw(pRect.Left, pRect.Top, LBitmap);
                finally
                  LCanvas.Handle := 0;
                  LCanvas.Free;
                end;

              finally
                LBitmap.Free;
              end;

              exit(S_OK);
            end;
        end;

      end;

    3: // drop down button
      begin
        case iStateId of
          1: // enabled
            begin
              DrawStyleFillRect(hdc, pRect, StyleServices.GetSystemColor(clBtnFace));
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonDropDownNormal), pRect);
              exit(S_OK);
            end;

          2: // hot
            begin
              DrawStyleFillRect(hdc, pRect, StyleServices.GetSystemColor(clBtnFace));
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonDropDownHot), pRect);
              exit(S_OK);
            end;

          3: // pressed
            begin
              DrawStyleFillRect(hdc, pRect, StyleServices.GetSystemColor(clBtnFace));
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonDropDownPressed), pRect);
              exit(S_OK);
            end;

          4: // disabled
            begin
              DrawStyleFillRect(hdc, pRect, StyleServices.GetSystemColor(clBtnFace));
              DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonDropDownDisabled), pRect);
              exit(S_OK);
            end;
        end;

      end;
  end;

  // OutputDebugString(PChar(Format('UxTheme_Navigation  class %s hTheme %d iPartId %d iStateId %d', [LThemeClass, hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_TreeView(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd): HRESULT; stdcall;
var
  LColor: TColor;
  LRect: TRect;
begin
  case iPartId of
    TVP_GLYPH:
      begin
        LColor := StyleServices.GetSystemColor(clWindowText);
        LRect := pRect;
        LRect.Top := LRect.Top + 5;
        LRect.Left := LRect.Left + 5;

        if (iStateId = GLPS_OPENED) or (iStateId = HGLPS_OPENED) then
          DrawStyleArrow(hdc, TScrollDirection.sdDown, LRect.Location, 3, LColor)
        else
          DrawStyleArrow(hdc, TScrollDirection.sdRight, LRect.Location, 3, LColor);

        exit(S_OK);
      end;

    TVP_HOTGLYPH:
      begin
        LColor := StyleServices.GetSystemColor(clHighlightText);
        LRect := pRect;
        LRect.Top := LRect.Top + 5;
        LRect.Left := LRect.Left + 5;

        if (iStateId = HGLPS_OPENED) then
          DrawStyleArrow(hdc, TScrollDirection.sdDown, LRect.Location, 3, LColor)
        else
          DrawStyleArrow(hdc, TScrollDirection.sdRight, LRect.Location, 3, LColor);

        exit(S_OK);
      end;

    TVP_TREEITEM:
      begin
        case iStateId of
          TREIS_HOT, TREIS_SELECTED, TREIS_SELECTEDNOTFOCUS, TREIS_HOTSELECTED:
            begin
              AlphaBlendRectangle(hdc, StyleServices.GetSystemColor(clHighlight), pRect, 96);
              exit(S_OK);
            end;
        end;
      end;
  end;

  // OutputDebugString(PChar(Format('UxTheme_TreeView  class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_Button(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd): HRESULT; stdcall;
var
  LDetails: TThemedElementDetails;
  SaveIndex: Integer;
begin
  case iPartId of

    BP_PUSHBUTTON:
      begin
        case iStateId of
          PBS_NORMAL:
            LDetails := StyleServices.GetElementDetails(tbPushButtonNormal);
          PBS_HOT:
            LDetails := StyleServices.GetElementDetails(tbPushButtonHot);
          PBS_PRESSED:
            LDetails := StyleServices.GetElementDetails(tbPushButtonPressed);
          PBS_DISABLED:
            LDetails := StyleServices.GetElementDetails(tbPushButtonDisabled);
          PBS_DEFAULTED:
            LDetails := StyleServices.GetElementDetails(tbPushButtonDefaulted);
          PBS_DEFAULTED_ANIMATING:
            LDetails := StyleServices.GetElementDetails(tbPushButtonDefaultedAnimating);
        end;

        SaveIndex := SaveDC(hdc);
        try
          if hwnd <> 0 then
            DrawStyleParentBackground(hwnd, hdc, pRect);
          StyleServices.DrawElement(hdc, LDetails, pRect, nil);
        finally
          RestoreDC(hdc, SaveIndex);
        end;

        exit(S_OK);
      end;

    BP_COMMANDLINK:
      begin

        case iStateId of
          CMDLS_NORMAL:
            LDetails := StyleServices.GetElementDetails(tbPushButtonNormal);
          CMDLS_HOT:
            LDetails := StyleServices.GetElementDetails(tbPushButtonHot);
          CMDLS_PRESSED:
            LDetails := StyleServices.GetElementDetails(tbPushButtonPressed);
          CMDLS_DISABLED:
            LDetails := StyleServices.GetElementDetails(tbPushButtonDisabled);
          CMDLS_DEFAULTED:
            LDetails := StyleServices.GetElementDetails(tbPushButtonDefaulted);
          CMDLS_DEFAULTED_ANIMATING:
            LDetails := StyleServices.GetElementDetails(tbPushButtonDefaultedAnimating);
        end;

        SaveIndex := SaveDC(hdc);
        try
          if hwnd <> 0 then
            DrawStyleParentBackground(hwnd, hdc, pRect);
          StyleServices.DrawElement(hdc, LDetails, pRect, nil);
        finally
          RestoreDC(hdc, SaveIndex);
        end;

        exit(S_OK);
      end;

    BP_COMMANDLINKGLYPH:
      begin
        case iStateId of
          CMDLGS_NORMAL:
            LDetails := StyleServices.GetElementDetails(tbCommandLinkGlyphNormal);
          CMDLGS_HOT:
            LDetails := StyleServices.GetElementDetails(tbCommandLinkGlyphHot);
          CMDLGS_PRESSED:
            LDetails := StyleServices.GetElementDetails(tbCommandLinkGlyphPressed);
          CMDLGS_DISABLED:
            LDetails := StyleServices.GetElementDetails(tbCommandLinkGlyphDisabled);
          CMDLGS_DEFAULTED:
            LDetails := StyleServices.GetElementDetails(tbCommandLinkGlyphDefaulted);
        end;

        SaveIndex := SaveDC(hdc);
        try
          if hwnd <> 0 then
            DrawStyleParentBackground(hwnd, hdc, pRect);
          StyleServices.DrawElement(hdc, LDetails, pRect, nil);
        finally
          RestoreDC(hdc, SaveIndex);
        end;

        exit(S_OK);
      end;

    BP_RADIOBUTTON:
      begin
        case iStateId of
          RBS_UNCHECKEDNORMAL:
            LDetails := StyleServices.GetElementDetails(tbRadioButtonUncheckedNormal);
          RBS_UNCHECKEDHOT:
            LDetails := StyleServices.GetElementDetails(tbRadioButtonUncheckedHot);
          RBS_UNCHECKEDPRESSED:
            LDetails := StyleServices.GetElementDetails(tbRadioButtonUncheckedPressed);
          RBS_UNCHECKEDDISABLED:
            LDetails := StyleServices.GetElementDetails(tbRadioButtonUncheckedDisabled);
          RBS_CHECKEDNORMAL:
            LDetails := StyleServices.GetElementDetails(tbRadioButtonCheckedNormal);
          RBS_CHECKEDHOT:
            LDetails := StyleServices.GetElementDetails(tbRadioButtonCheckedHot);
          RBS_CHECKEDPRESSED:
            LDetails := StyleServices.GetElementDetails(tbRadioButtonCheckedPressed);
          RBS_CHECKEDDISABLED:
            LDetails := StyleServices.GetElementDetails(tbRadioButtonCheckedDisabled);
        end;

        DrawStyleElement(hdc, LDetails, pRect);
        exit(S_OK);
      end;

    BP_CHECKBOX:
      begin
        case iStateId of
          CBS_UNCHECKEDNORMAL:
            LDetails := StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);
          CBS_UNCHECKEDHOT:
            LDetails := StyleServices.GetElementDetails(tbCheckBoxUncheckedHot);
          CBS_UNCHECKEDPRESSED:
            LDetails := StyleServices.GetElementDetails(tbCheckBoxUncheckedPressed);
          CBS_UNCHECKEDDISABLED:
            LDetails := StyleServices.GetElementDetails(tbCheckBoxUncheckedDisabled);
          CBS_CHECKEDNORMAL:
            LDetails := StyleServices.GetElementDetails(tbCheckBoxCheckedNormal);
          CBS_CHECKEDHOT:
            LDetails := StyleServices.GetElementDetails(tbCheckBoxCheckedHot);
          CBS_CHECKEDPRESSED:
            LDetails := StyleServices.GetElementDetails(tbCheckBoxCheckedPressed);
          CBS_CHECKEDDISABLED:
            LDetails := StyleServices.GetElementDetails(tbCheckBoxCheckedDisabled);
          CBS_MIXEDNORMAL:
            LDetails := StyleServices.GetElementDetails(tbCheckBoxMixedNormal);
          CBS_MIXEDHOT:
            LDetails := StyleServices.GetElementDetails(tbCheckBoxMixedHot);
          CBS_MIXEDPRESSED:
            LDetails := StyleServices.GetElementDetails(tbCheckBoxMixedPressed);
          CBS_MIXEDDISABLED:
            LDetails := StyleServices.GetElementDetails(tbCheckBoxMixedDisabled);
          { For Windows >= Vista }
          CBS_IMPLICITNORMAL:
            LDetails := StyleServices.GetElementDetails(tbCheckBoxImplicitNormal);
          CBS_IMPLICITHOT:
            LDetails := StyleServices.GetElementDetails(tbCheckBoxImplicitHot);
          CBS_IMPLICITPRESSED:
            LDetails := StyleServices.GetElementDetails(tbCheckBoxImplicitPressed);
          CBS_IMPLICITDISABLED:
            LDetails := StyleServices.GetElementDetails(tbCheckBoxImplicitDisabled);
          CBS_EXCLUDEDNORMAL:
            LDetails := StyleServices.GetElementDetails(tbCheckBoxExcludedNormal);
          CBS_EXCLUDEDHOT:
            LDetails := StyleServices.GetElementDetails(tbCheckBoxExcludedHot);
          CBS_EXCLUDEDPRESSED:
            LDetails := StyleServices.GetElementDetails(tbCheckBoxExcludedPressed);
          CBS_EXCLUDEDDISABLED:
            LDetails := StyleServices.GetElementDetails(tbCheckBoxExcludedDisabled);
        end;

        DrawStyleElement(hdc, LDetails, pRect);
        exit(S_OK);
      end
  end;

  // OutputDebugString(PChar(Format('UxTheme_Button  class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_TaskDialog(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd): HRESULT; stdcall;
var
  LStartColor, LEndColor, LColor: TColor;
  SaveIndex: Integer;
  LDetails: TThemedElementDetails;
  LCanvas: TCanvas;
begin
  case iPartId of

    TDLG_PRIMARYPANEL:
      begin
        // LDetails := StyleServices.GetElementDetails(ttdPrimaryPanel);   //ttdPrimaryPanel  this element is not included in the VCL Styles yet

        LColor := StyleServices.GetStyleColor(scEdit);
        if LColor = StyleServices.GetStyleColor(scBorder) then
          LColor := StyleServices.GetStyleColor(scPanel); // GetShadowColor(LColor, -10);
        DrawStyleFillRect(hdc, pRect, LColor);
        exit(S_OK);
      end;

    TDLG_FOOTNOTEPANE, TDLG_SECONDARYPANEL:
      begin
        // LDetails := StyleServices.GetElementDetails(tpPanelBackground);   //ttdSecondaryPanel  this element is not included in the VCL Styles yet
        StyleServices.GetElementColor(StyleServices.GetElementDetails(tpPanelBackground), ecFillColor, LColor);
        DrawStyleFillRect(hdc, pRect, LColor);
        exit(S_OK);
      end;

    TDLG_EXPANDOBUTTON:
      begin
        case iStateId of
          TDLGEBS_NORMAL:
            LDetails := StyleServices.GetElementDetails(tcpThemedChevronClosedNormal);
          TDLGEBS_HOVER:
            LDetails := StyleServices.GetElementDetails(tcpThemedChevronClosedHot);
          TDLGEBS_PRESSED:
            LDetails := StyleServices.GetElementDetails(tcpThemedChevronClosedPressed);
          TDLGEBS_EXPANDEDNORMAL:
            LDetails := StyleServices.GetElementDetails(tcpThemedChevronOpenedNormal);
          TDLGEBS_EXPANDEDHOVER:
            LDetails := StyleServices.GetElementDetails(tcpThemedChevronOpenedHot);
          TDLGEBS_EXPANDEDPRESSED:
            LDetails := StyleServices.GetElementDetails(tcpThemedChevronOpenedPressed);
        end;

        SaveIndex := SaveDC(hdc);
        try
          if (hwnd <> 0) then
            DrawStyleParentBackground(hwnd, hdc, pRect);
          StyleServices.DrawElement(hdc, LDetails, pRect, nil);
        finally
          RestoreDC(hdc, SaveIndex);
        end;

        exit(S_OK);
      end;

    TDLG_FOOTNOTESEPARATOR:
      begin
        LStartColor := StyleServices.GetSystemColor(clBtnShadow);
        LEndColor := StyleServices.GetSystemColor(clBtnHighlight);

        LCanvas := TCanvas.Create;
        SaveIndex := SaveDC(hdc);
        try
          LCanvas.Handle := hdc;
          LCanvas.Pen.Color := LStartColor;
          LCanvas.MoveTo(pRect.Left, pRect.Top);
          LCanvas.LineTo(pRect.Right, pRect.Top);
          LCanvas.Pen.Color := LEndColor;
          LCanvas.MoveTo(pRect.Left, pRect.Top + 1);
          LCanvas.LineTo(pRect.Right, pRect.Top + 1);
        finally
          LCanvas.Handle := 0;
          LCanvas.Free;
          RestoreDC(hdc, SaveIndex);
        end;

        exit(S_OK);
      end
  end;

  // OutputDebugString(PChar(Format('UxTheme_TaskDialog  class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function UxTheme_ProgressBar(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd): HRESULT; stdcall;
var
  LDetails: TThemedElementDetails;
  SaveIndex: Integer;
begin
  case iPartId of
    PP_BAR:
      LDetails := StyleServices.GetElementDetails(tpBar);
    PP_BARVERT:
      LDetails := StyleServices.GetElementDetails(tpBarVert);
    PP_CHUNK:
      LDetails := StyleServices.GetElementDetails(tpChunk);
    PP_CHUNKVERT:
      LDetails := StyleServices.GetElementDetails(tpChunkVert);

    PP_FILL:
      if SameText(LThemeClass, VSCLASS_PROGRESS) then
        LDetails := StyleServices.GetElementDetails(tpChunk)
        // GetElementDetails(tpChunk);//GetElementDetails(tpFill);   not defined
      else
        LDetails := StyleServices.GetElementDetails(tpBar);
    PP_FILLVERT:
      LDetails := StyleServices.GetElementDetails(tpChunkVert); // GetElementDetails(tpFillVert); not defined

    // Use the Native PP_PULSEOVERLAY part to get better results.
    // PP_PULSEOVERLAY : if SameText(THThemesClasses.Items[hTheme], VSCLASS_PROGRESS) then
    // LDetails := StyleServices.GetElementDetails(tpChunk)//GetElementDetails(tpPulseOverlay);
    // else
    // LDetails := StyleServices.GetElementDetails(tpBar);

    PP_MOVEOVERLAY:
      if SameText(LThemeClass, VSCLASS_PROGRESS) then
        LDetails := StyleServices.GetElementDetails(tpMoveOverlay)
      else
        LDetails := StyleServices.GetElementDetails(tpChunk);

    // PP_PULSEOVERLAYVERT :   LDetails := StyleServices.GetElementDetails(tpPulseOverlayVert);
    // PP_MOVEOVERLAYVERT  :   LDetails := StyleServices.GetElementDetails(tpMoveOverlayVert);

    PP_TRANSPARENTBAR:
      LDetails := StyleServices.GetElementDetails(tpBar); // GetElementDetails(tpTransparentBarNormal); not defined
    PP_TRANSPARENTBARVERT:
      LDetails := StyleServices.GetElementDetails(tpBarVert);
      // GetElementDetails(tpTransparentBarVertNormal); not defined
  else
    begin
      // OutputDebugString(PChar(Format('UxTheme_ProgressBar hTheme %d iPartId %d iStateId %d', [hTheme, iPartId, iStateId])));
      exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
    end;
  end;

  SaveIndex := SaveDC(hdc);
  try
    if hwnd <> 0 then
      DrawStyleParentBackground(hwnd, hdc, pRect);
    StyleServices.DrawElement(hdc, LDetails, pRect, nil);
  finally
    RestoreDC(hdc, SaveIndex);
  end;
  Result := S_OK;
end;

function UxTheme_ScrollBar(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd = 0): HRESULT; stdcall;
var
  LDetails: TThemedElementDetails;
  LScrollDetails: TThemedScrollBar;
begin
  LScrollDetails := tsScrollBarRoot;
  LDetails.Element := TThemedElement.teScrollBar;
  LDetails.Part := iPartId;
  LDetails.State := iStateId;
  LDetails := StyleServices.GetElementDetails(TThemedScrollBar.tsThumbBtnHorzNormal);

  case iPartId of
    SBP_ARROWBTN:
      begin
        case iStateId of
          ABS_UPNORMAL:
            LScrollDetails := tsArrowBtnUpNormal;
          ABS_UPHOT:
            LScrollDetails := tsArrowBtnUpHot;
          ABS_UPPRESSED:
            LScrollDetails := tsArrowBtnUpPressed;
          ABS_UPDISABLED:
            LScrollDetails := tsArrowBtnUpDisabled;
          ABS_DOWNNORMAL:
            LScrollDetails := tsArrowBtnDownNormal;
          ABS_DOWNHOT:
            LScrollDetails := tsArrowBtnDownHot;
          ABS_DOWNPRESSED:
            LScrollDetails := tsArrowBtnDownPressed;
          ABS_DOWNDISABLED:
            LScrollDetails := tsArrowBtnDownDisabled;
          ABS_LEFTNORMAL:
            LScrollDetails := tsArrowBtnLeftNormal;
          ABS_LEFTHOT:
            LScrollDetails := tsArrowBtnLeftHot;
          ABS_LEFTPRESSED:
            LScrollDetails := tsArrowBtnLeftPressed;
          ABS_LEFTDISABLED:
            LScrollDetails := tsArrowBtnLeftDisabled;
          ABS_RIGHTNORMAL:
            LScrollDetails := tsArrowBtnRightNormal;
          ABS_RIGHTHOT:
            LScrollDetails := tsArrowBtnRightHot;
          ABS_RIGHTPRESSED:
            LScrollDetails := tsArrowBtnRightPressed;
          ABS_RIGHTDISABLED:
            LScrollDetails := tsArrowBtnRightDisabled;
          ABS_UPHOVER:
            LScrollDetails := tsArrowBtnUpNormal; // tsArrowBtnUpHover;
          ABS_DOWNHOVER:
            LScrollDetails := tsArrowBtnDownNormal; // tsArrowBtnDownHover;
          ABS_LEFTHOVER:
            LScrollDetails := tsArrowBtnLeftNormal; // tsArrowBtnLeftHover;
          ABS_RIGHTHOVER:
            LScrollDetails := tsArrowBtnRightNormal; // tsArrowBtnRightHover;
        end;
      end;

    SBP_THUMBBTNHORZ:
      begin
        case iStateId of
          SCRBS_NORMAL:
            LScrollDetails := tsThumbBtnHorzNormal;
          SCRBS_HOT:
            LScrollDetails := tsThumbBtnHorzHot;
          SCRBS_PRESSED:
            LScrollDetails := tsThumbBtnHorzPressed;
          SCRBS_DISABLED:
            LScrollDetails := tsThumbBtnHorzDisabled;
          SCRBS_HOVER:
            LScrollDetails := tsThumbBtnHorzNormal;
        end;
      end;

    SBP_THUMBBTNVERT:
      begin
        case iStateId of
          SCRBS_NORMAL:
            LScrollDetails := tsThumbBtnVertNormal;
          SCRBS_HOT:
            LScrollDetails := tsThumbBtnVertHot;
          SCRBS_PRESSED:
            LScrollDetails := tsThumbBtnVertPressed;
          SCRBS_DISABLED:
            LScrollDetails := tsThumbBtnVertDisabled;
          SCRBS_HOVER:
            LScrollDetails := tsThumbBtnVertNormal;
        end;
      end;

    SBP_LOWERTRACKHORZ:
      begin
        case iStateId of
          SCRBS_NORMAL:
            LScrollDetails := tsLowerTrackHorzNormal;
          SCRBS_HOT:
            LScrollDetails := tsLowerTrackHorzHot;
          SCRBS_PRESSED:
            LScrollDetails := tsLowerTrackHorzPressed;
          SCRBS_DISABLED:
            LScrollDetails := tsLowerTrackHorzDisabled;
          SCRBS_HOVER:
            LScrollDetails := tsLowerTrackHorzNormal; // tsLowerTrackHorzHover; //no support for hover
        end;
      end;

    SBP_UPPERTRACKHORZ:
      begin
        case iStateId of
          SCRBS_NORMAL:
            LScrollDetails := tsUpperTrackHorzNormal;
          SCRBS_HOT:
            LScrollDetails := tsUpperTrackHorzHot;
          SCRBS_PRESSED:
            LScrollDetails := tsUpperTrackHorzPressed;
          SCRBS_DISABLED:
            LScrollDetails := tsUpperTrackHorzDisabled;
          SCRBS_HOVER:
            LScrollDetails := tsUpperTrackHorzNormal; // tsUpperTrackHorzHover; //no support for hover
        end;
      end;

    SBP_LOWERTRACKVERT:
      begin
        case iStateId of
          SCRBS_NORMAL:
            LScrollDetails := tsLowerTrackVertNormal;
          SCRBS_HOT:
            LScrollDetails := tsLowerTrackVertHot;
          SCRBS_PRESSED:
            LScrollDetails := tsLowerTrackVertPressed;
          SCRBS_DISABLED:
            LScrollDetails := tsLowerTrackVertDisabled;
          SCRBS_HOVER:
            LScrollDetails := tsLowerTrackVertNormal; // tsLowerTrackVertHover; //no support for hover
        end;
      end;

    SBP_UPPERTRACKVERT:
      begin
        case iStateId of
          SCRBS_NORMAL:
            LScrollDetails := tsUpperTrackVertNormal;
          SCRBS_HOT:
            LScrollDetails := tsUpperTrackVertHot;
          SCRBS_PRESSED:
            LScrollDetails := tsUpperTrackVertPressed;
          SCRBS_DISABLED:
            LScrollDetails := tsUpperTrackVertDisabled;
          SCRBS_HOVER:
            LScrollDetails := tsUpperTrackVertNormal; // tsUpperTrackVertHover; //no support for hover
        end;
      end;

    SBP_SIZEBOX:
      begin
        case iStateId of
          SZB_RIGHTALIGN:
            LScrollDetails := tsSizeBoxRightAlign;
          SZB_LEFTALIGN:
            LScrollDetails := tsSizeBoxLeftAlign;
          SZB_TOPRIGHTALIGN:
            LScrollDetails := tsSizeBoxTopRightAlign;
          SZB_TOPLEFTALIGN:
            LScrollDetails := tsSizeBoxTopLeftAlign;
          SZB_HALFBOTTOMRIGHTALIGN:
            LScrollDetails := tsSizeBoxHalfBottomRightAlign;
          SZB_HALFBOTTOMLEFTALIGN:
            LScrollDetails := tsSizeBoxHalfBottomLeftAlign;
          SZB_HALFTOPRIGHTALIGN:
            LScrollDetails := tsSizeBoxHalfTopRightAlign;
          SZB_HALFTOPLEFTALIGN:
            LScrollDetails := tsSizeBoxHalfTopLeftAlign;
        end;
      end;

    SBP_GRIPPERHORZ:
      begin
        case iStateId of
          SCRBS_NORMAL:
            LScrollDetails := tsGripperHorzNormal;
          SCRBS_HOT:
            LScrollDetails := tsGripperHorzHot;
          SCRBS_PRESSED:
            LScrollDetails := tsGripperHorzPressed;
          SCRBS_DISABLED:
            LScrollDetails := tsGripperHorzDisabled;
          SCRBS_HOVER:
            LScrollDetails := tsGripperHorzHover; // tsGripperHorzHover; //no support for hover
        end;
      end;

    SBP_GRIPPERVERT:
      begin
        case iStateId of
          SCRBS_NORMAL:
            LScrollDetails := tsGripperVertNormal;
          SCRBS_HOT:
            LScrollDetails := tsGripperVertHot;
          SCRBS_PRESSED:
            LScrollDetails := tsGripperVertPressed;
          SCRBS_DISABLED:
            LScrollDetails := tsGripperVertDisabled;
          SCRBS_HOVER:
            LScrollDetails := tsGripperVertNormal; // tsGripperVertHover; //no support for hover
        end;
      end;
  end;

  LDetails := StyleServices.GetElementDetails(LScrollDetails);

  if (iPartId = SBP_THUMBBTNHORZ) then
    StyleServices.DrawElement(hdc, StyleServices.GetElementDetails(tsUpperTrackHorzNormal), pRect, nil)
  else if (iPartId = SBP_THUMBBTNVERT) then
    StyleServices.DrawElement(hdc, StyleServices.GetElementDetails(tsUpperTrackVertNormal), pRect, nil);

  // OutputDebugString(PChar(Format('UxTheme_ScrollBar class %s hTheme %d iPartId %d iStateId %d Left %d Top %d Width %d Height %d',
  // [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId, PRect.Left, prect.Top, prect.Width, prect.Height])));
  StyleServices.DrawElement(hdc, LDetails, pRect, nil);
  exit(S_OK);
end;

function UxTheme_Edit(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd = 0): HRESULT; stdcall;
var
  LDetails: TThemedElementDetails;
begin
  // OutputDebugString(PChar(Format('UxTheme_Edit class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme], hTheme, iPartId, iStateId])));
  case iPartId of

    EP_BACKGROUNDWITHBORDER, EP_EDITBORDER_NOSCROLL:
      begin
        case iStateId of
          EPSN_NORMAL:
            LDetails := StyleServices.GetElementDetails(teEditBorderNoScrollNormal);
          EPSN_HOT:
            LDetails := StyleServices.GetElementDetails(teEditBorderNoScrollHot);
          EPSN_FOCUSED:
            LDetails := StyleServices.GetElementDetails(teEditBorderNoScrollFocused);
          EPSN_DISABLED:
            begin
              // LDetails := StyleServices.GetElementDetails(teEditBorderNoScrollDisabled);
              DrawStyleFillRect(hdc, pRect, StyleServices.GetSystemColor(clWindow));
              exit(S_OK);
            end;
        end;

        DrawStyleElement(hdc, LDetails, pRect);
        exit(S_OK);
      end;

    EP_EDITBORDER_HSCROLL:
      begin
        case iStateId of
          EPSH_NORMAL:
            LDetails := StyleServices.GetElementDetails(teEditBorderHScrollNormal);
          EPSH_HOT:
            LDetails := StyleServices.GetElementDetails(teEditBorderHScrollHot);
          EPSH_FOCUSED:
            LDetails := StyleServices.GetElementDetails(teEditBorderHScrollFocused);
          EPSH_DISABLED:
            LDetails := StyleServices.GetElementDetails(teEditBorderHScrollDisabled);
        end;

        DrawStyleElement(hdc, LDetails, pRect);
        exit(S_OK);
      end;

    EP_EDITBORDER_VSCROLL:
      begin
        case iStateId of
          EPSV_NORMAL:
            LDetails := StyleServices.GetElementDetails(teEditBorderVScrollNormal);
          EPSV_HOT:
            LDetails := StyleServices.GetElementDetails(teEditBorderVScrollHot);
          EPSV_FOCUSED:
            LDetails := StyleServices.GetElementDetails(teEditBorderVScrollFocused);
          EPSV_DISABLED:
            LDetails := StyleServices.GetElementDetails(teEditBorderVScrollDisabled);
        end;

        DrawStyleElement(hdc, LDetails, pRect);
        exit(S_OK);
      end;

    EP_EDITBORDER_HVSCROLL:
      begin
        case iStateId of
          EPSHV_NORMAL:
            LDetails := StyleServices.GetElementDetails(teEditBorderHVScrollNormal);
          EPSHV_HOT:
            LDetails := StyleServices.GetElementDetails(teEditBorderHVScrollHot);
          EPSHV_FOCUSED:
            LDetails := StyleServices.GetElementDetails(teEditBorderHVScrollFocused);
          EPSHV_DISABLED:
            LDetails := StyleServices.GetElementDetails(teEditBorderHVScrollDisabled);
        end;

        DrawStyleElement(hdc, LDetails, pRect);
        exit(S_OK);
      end

  end;
  // OutputDebugString(PChar(Format('UxTheme_Edit class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));

end;

{$IFDEF HOOK_Menu}

procedure DrawMenuSpecialChar(DC: hdc; const Sign: Char; DestRect: TRect; const Bold: Boolean = False;
  const Disabled: Boolean = False);
var
  LogFont: TLogFont;
  pOldFont: HGDIOBJ;
  AFont: HFONT;
  oldColor: COLORREF;
  OldMode: Integer;
begin
  LogFont.lfHeight := DestRect.Height;
  LogFont.lfWidth := 0;
  LogFont.lfEscapement := 0;
  LogFont.lfOrientation := 0;
  if Bold then
    LogFont.lfWeight := FW_BOLD
  else
    LogFont.lfWeight := FW_NORMAL;
  LogFont.lfItalic := 0;
  LogFont.lfUnderline := 0;
  LogFont.lfStrikeOut := 0;
  LogFont.lfCharSet := DEFAULT_CHARSET;
  LogFont.lfOutPrecision := OUT_DEFAULT_PRECIS;
  LogFont.lfClipPrecision := CLIP_DEFAULT_PRECIS;
  LogFont.lfQuality := ANTIALIASED_QUALITY; // DEFAULT_QUALITY;
  LogFont.lfPitchAndFamily := DEFAULT_PITCH;
  LogFont.lfFaceName := 'Marlett';

  if Disabled then
    oldColor := ColorToRGB(StyleServices.GetStyleFontColor(sfPopupMenuItemTextDisabled))
  else
    oldColor := ColorToRGB(StyleServices.GetStyleFontColor(sfPopupMenuItemTextNormal));

  AFont := CreateFontIndirect(LogFont);
  if AFont <> 0 then
    try
      oldColor := SetTextColor(DC, oldColor);
      pOldFont := SelectObject(DC, AFont);
      try
        OldMode := SetBkMode(DC, Transparent);
        WinApi.Windows.DrawText(DC, Sign, 1, DestRect, DT_LEFT or DT_SINGLELINE);
        SetBkMode(DC, OldMode);
        SelectObject(DC, oldColor);
      finally
        if pOldFont <> 0 then
          SelectObject(DC, pOldFont);
      end;
    finally
      DeleteObject(AFont);
    end;
end;

function UxTheme_Menu(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd): HRESULT; stdcall;
var
  LRect, LRect2: TRect;
  LColor: TColor;
  LPRect: System.Types.pRect;
begin

  case iPartId of

    MENU_POPUPBORDERS: // OK
      begin
        DrawStyleElement(hdc, StyleServices.GetElementDetails(tmPopupBorders), pRect);
        exit(S_OK);
      end;

    MENU_POPUPITEM: // OK
      begin
        case iStateId of
          MPI_NORMAL:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tmPopupItemNormal), pRect);
              exit(S_OK);
            end;

          MPI_HOT:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tmPopupItemHot), pRect);
              exit(S_OK);
            end;

          MPI_DISABLED:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tmPopupItemDisabled), pRect);
              exit(S_OK);
            end;

          MPI_DISABLEDHOT:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tmPopupItemHot), pRect);
              exit(S_OK);
            end;
        end;
      end;

    // MENU_BARBACKGROUND :
    // begin
    // case iStateId of
    // MB_ACTIVE      :
    // begin
    // DrawStyleElement(hdc, StyleServices.GetElementDetails(tmMenuBarBackgroundActive), pRect);
    // Exit(S_OK);
    // end;
    //
    // MB_INACTIVE      :
    // begin
    // DrawStyleElement(hdc, StyleServices.GetElementDetails(tmMenuBarBackgroundInactive), pRect);
    // Exit(S_OK);
    // end;
    // end;
    // end;
    //
    // MENU_BARITEM      :
    // begin
    // case iStateId of
    // MBI_NORMAL         :
    // begin
    // DrawStyleElement(hdc, StyleServices.GetElementDetails(tmMenuBarItemNormal), pRect);
    // Exit(S_OK);
    // end;
    //
    // MBI_HOT            :
    // begin
    // DrawStyleElement(hdc, StyleServices.GetElementDetails(tmMenuBarItemHot), pRect);
    // Exit(S_OK);
    // end;
    //
    // MBI_PUSHED         :
    // begin
    // DrawStyleElement(hdc, StyleServices.GetElementDetails(tmMenuBarItemPushed), pRect);
    // Exit(S_OK);
    // end;
    //
    // MBI_DISABLED       :
    // begin
    // DrawStyleElement(hdc,  StyleServices.GetElementDetails(tmMenuBarItemDisabled), pRect);
    // Exit(S_OK);
    // end;
    //
    // MBI_DISABLEDHOT    :
    // begin
    // DrawStyleElement(hdc,  StyleServices.GetElementDetails(tmMenuBarItemDisabledHot), pRect);
    // Exit(S_OK);
    // end;
    //
    // MBI_DISABLEDPUSHED :
    // begin
    // DrawStyleElement(hdc,  StyleServices.GetElementDetails(tmMenuBarItemDisabledPushed), pRect);
    // Exit(S_OK);
    // end;
    // end;
    // end;

    MENU_POPUPSEPARATOR: // ok

      begin
        // W7 Only ??
        // DrawStyleElement(hdc, StyleServices.GetElementDetails(tmPopupItemNormal), pRect);
        DrawStyleElement(hdc, StyleServices.GetElementDetails(tmPopupSeparator), pRect);
        exit(S_OK);
      end;

    MENU_POPUPGUTTER:
      begin
        if Foo <> nil then
        begin
          LPRect := Foo;
          if (LPRect.Width > 0) and (LPRect.Height > 0) then
          begin
            LRect2 := LPRect^;
            // DrawStyleParentBackground(hwnd, hdc, LRect2);
            DrawStyleFillRect(hdc, LRect2, StyleServices.GetSystemColor(clMenu));
            // DrawStyleElement(hdc, StyleServices.GetElementDetails(tmPopupBackground), LRect2);
            exit(S_OK);
          end;
        end;

        DrawStyleFillRect(hdc, pRect, StyleServices.GetSystemColor(clMenu));
        // DrawStyleElement(hdc, StyleServices.GetElementDetails(tmPopupBackground), pRect);
        exit(S_OK);
      end;

    MENU_POPUPBACKGROUND:
      begin
        LPRect := nil;
        if Foo <> nil then
        begin
          LPRect := Foo;
          if (LPRect.Width = 0) or (LPRect.Height = 0) then
            LPRect := nil;
        end;

        if LPRect = nil then
        begin
          DrawStyleElement(hdc, StyleServices.GetElementDetails(tmPopupBackground), pRect);
          exit(S_OK);
        end
        else
        begin
          LRect2 := LPRect^;
          // DrawStyleElement(hdc, StyleServices.GetElementDetails(tmPopupBackground), LRect2);
          // DrawStyleParentBackgroundEx(hwnd, hdc, LRect2);
          DrawStyleFillRect(hdc, LRect2, StyleServices.GetSystemColor(clMenu));
          // Windows Vista - W7
          if (TOSVersion.Major = 6) and ((TOSVersion.Minor = 0) or (TOSVersion.Minor = 1)) then
            SetTextColor(hdc, ColorToRGB(GetStyleMenuTextColor));
          exit(S_OK);
        end;
      end;

    MENU_POPUPSUBMENU: // OK
      begin
        case iStateId of
          MSM_DISABLED, MSM_NORMAL:
            begin
              if iStateId = MSM_DISABLED then
                LColor := StyleServices.GetStyleFontColor(sfPopupMenuItemTextDisabled)
              else
                LColor := StyleServices.GetStyleFontColor(sfPopupMenuItemTextNormal);

              LRect := pRect;
              LRect.Top := LRect.Top + 3;
              DrawStyleArrow(hdc, TScrollDirection.sdRight, LRect.Location, 3, LColor);
              exit(S_OK);
            end;
        end;
      end;

    MENU_POPUPCHECKBACKGROUND:
      begin
        case iStateId of
          MCB_DISABLED: // OK
            begin
              // DrawStyleElement(hdc,  StyleServices.GetElementDetails(tmMenuBarItemNormal), pRect);
              // DrawStyleFillRect(hdc, pRect, clFuchsia);
              exit(S_OK);
            end;

          MCB_NORMAL: // OK
            begin
              // DrawStyleElement(hdc,  StyleServices.GetElementDetails(tmMenuBarItemNormal), pRect);
              // DrawStyleFillRect(hdc, pRect, clBlue);
              exit(S_OK);
            end;

          MCB_BITMAP: // OK
            begin
              // DrawStyleElement(hdc,  StyleServices.GetElementDetails(tmMenuBarItemNormal), pRect);
              // DrawStyleFillRect(hdc, pRect, clGreen);
              exit(S_OK);
            end;
        end;
      end;

    MENU_POPUPCHECK:
      begin
        case iStateId of
          MC_CHECKMARKNORMAL: // OK
            begin
              // DrawStyleFillRect(hdc, pRect, clFuchsia);
              FontAwesome.DrawChar(hdc, fa_check, pRect, StyleServices.GetSystemColor(clMenuText));
              exit(S_OK);
            end;

          MC_CHECKMARKDISABLED: // OK
            begin
              // DrawStyleFillRect(hdc, pRect, clBlue);
              FontAwesome.DrawChar(hdc, fa_check, pRect, StyleServices.GetSystemColor(clGrayText));
              exit(S_OK);
            end;

          MC_BULLETNORMAL: // OK
            begin
              // DrawStyleFillRect(hdc, pRect, clGreen);
              FontAwesome.DrawChar(hdc, fa_circle, pRect, StyleServices.GetSystemColor(clMenuText));
              exit(S_OK);
            end;

          MC_BULLETDISABLED: // OK
            begin
              // DrawStyleFillRect(hdc, pRect, clGreen);
              FontAwesome.DrawChar(hdc, fa_circle, pRect, StyleServices.GetSystemColor(clGrayText));
              exit(S_OK);
            end;
        end;
      end;

    MENU_SYSTEMRESTORE:
      begin
        case iStateId of
          MSYSR_NORMAL:
            begin
              LRect := pRect;
              LRect.Top := LRect.Top + 3;
              LRect.Width := 10;
              LRect.Height := 10;
              DrawMenuSpecialChar(hdc, MARLETT_RESTORE_CHAR, LRect, False, False);
              exit(S_OK);
            end;

          MSYSR_DISABLED:
            begin
              LRect := pRect;
              LRect.Top := LRect.Top + 3;
              LRect.Width := 10;
              LRect.Height := 10;
              DrawMenuSpecialChar(hdc, MARLETT_RESTORE_CHAR, LRect, False, True);
              exit(S_OK);
            end;
        end;
      end;

    MENU_SYSTEMMINIMIZE:
      begin
        case iStateId of
          MSYSMN_NORMAL:
            begin
              LRect := pRect;
              LRect.Top := LRect.Top + 3;
              LRect.Width := 10;
              LRect.Height := 10;
              DrawMenuSpecialChar(hdc, MARLETT_MINIMIZE_CHAR, LRect, False, False);
              exit(S_OK);
            end;

          MSYSMN_DISABLED:
            begin
              LRect := pRect;
              LRect.Top := LRect.Top + 3;
              LRect.Width := 10;
              LRect.Height := 10;
              DrawMenuSpecialChar(hdc, MARLETT_MINIMIZE_CHAR, LRect, False, True);
              exit(S_OK);
            end;
        end;
      end;

    MENU_SYSTEMMAXIMIZE:
      begin
        case iStateId of
          MSYSMX_NORMAL:
            begin
              LRect := pRect;
              LRect.Top := LRect.Top + 3;
              LRect.Width := 10;
              LRect.Height := 10;
              DrawMenuSpecialChar(hdc, MARLETT_MAXIMIZE_CHAR, LRect, False, False);
              exit(S_OK);
            end;

          MSYSMX_DISABLED:
            begin
              LRect := pRect;
              LRect.Top := LRect.Top + 3;
              LRect.Width := 10;
              LRect.Height := 10;
              DrawMenuSpecialChar(hdc, MARLETT_MAXIMIZE_CHAR, LRect, False, True);
              exit(S_OK);
            end;
        end;
      end;

    MENU_SYSTEMCLOSE: // OK
      begin
        case iStateId of
          MSYSC_NORMAL:
            begin
              // DrawStyleElement(hdc,  StyleServices.GetElementDetails(tmSystemCloseNormal), pRect);
              LRect := pRect;
              LRect.Top := LRect.Top + 3;
              LRect.Width := 10;
              LRect.Height := 10;
              DrawMenuSpecialChar(hdc, MARLETT_CLOSE_CHAR, LRect, False, False);
              exit(S_OK);
            end;

          MSYSC_DISABLED:
            begin
              LRect := pRect;
              LRect.Top := LRect.Top + 3;
              LRect.Width := 10;
              LRect.Height := 10;
              DrawMenuSpecialChar(hdc, MARLETT_CLOSE_CHAR, LRect, False, True);
              exit(S_OK);
            end;
        end;
      end;
  end;

  // OutputDebugString(PChar(Format('UxTheme_Menu class %s hTheme %d iPartId %d iStateId %d', [LThemeClass, hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
  // DrawStyleFillRect(hdc, pRect, clBlue);
  // Exit(S_OK);
end;
{$ENDIF}

function UxTheme_CommandModule(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer;
  Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd): HRESULT; stdcall;
var
  LDetails: TThemedElementDetails;
  LColor: TColor;
  LRect: TRect;
begin
  case iPartId of
    // Top Bar
    1:
      begin
        case iStateId of
          0:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tcpThemedHeader), pRect);
              exit(S_OK);
            end;
        end;
      end;

    // Buttons background
    3:
      begin
        case iStateId of
          // normal
          1:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
              exit(S_OK);
            end;
          // Hot
          2:
            begin

              // DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonHot), pRect);
              LColor := StyleServices.GetSystemColor(clHighlight);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
              LRect := pRect;
              InflateRect(LRect, -1, -1);
              DrawStyleRectangle(hdc, LRect, LColor);
              // AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
              exit(S_OK);
            end;

          // pressed
          3:
            begin
              // DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonPressed), pRect);
              LColor := StyleServices.GetSystemColor(clHighlight);
              // AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
              LRect := pRect;
              InflateRect(LRect, -1, -1);
              DrawStyleRectangle(hdc, LRect, LColor);
              exit(S_OK);
            end;

          // focused
          4:
            begin
              // DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonDefaulted), pRect);
              LColor := StyleServices.GetSystemColor(clHighlight);
              // AlphaBlendFillCanvas(hdc, LColor, pRect, 50);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
              LRect := pRect;
              InflateRect(LRect, -1, -1);
              DrawStyleRectangle(hdc, LRect, LColor);
              exit(S_OK);
            end;
        end;
      end;

    // button with dropdown
    4:
      begin
        case iStateId of
          // normal
          1:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
              exit(S_OK);
            end;

          // hot
          2:
            begin
              LColor := StyleServices.GetSystemColor(clHighlight);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
              LRect := pRect;
              InflateRect(LRect, -1, -1);
              DrawStyleRectangle(hdc, LRect, LColor);
              // AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
              // DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonHot), pRect);
              exit(S_OK);
            end;

          // pressed
          3:
            begin
              LColor := StyleServices.GetSystemColor(clHighlight);
              // AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
              LRect := pRect;
              InflateRect(LRect, -1, -1);
              DrawStyleRectangle(hdc, LRect, LColor);
              // DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonPressed), pRect);
              exit(S_OK);
            end;

          // focused
          4:
            begin
              LColor := StyleServices.GetSystemColor(clHighlight);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
              LRect := pRect;
              InflateRect(LRect, -1, -1);
              DrawStyleRectangle(hdc, LRect, LColor);
              // AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
              // DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonDefaulted), pRect);
              exit(S_OK);
            end;

          5: // hot arrow button
            begin
              LColor := StyleServices.GetSystemColor(clHighlight);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
              LRect := pRect;
              InflateRect(LRect, -1, -1);
              DrawStyleRectangle(hdc, LRect, LColor);
              // AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
              // DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonHot), pRect);
              exit(S_OK);
            end;
        end;
      end;

    // arrow button with dropdown - background
    5:
      begin
        case iStateId of
          // normal
          1:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
              exit(S_OK);
            end;
          // hot on arrow
          2:
            begin
              // DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonHot), pRect);
              LColor := StyleServices.GetSystemColor(clHighlight);
              // AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
              LRect := pRect;
              InflateRect(LRect, -1, -1);
              DrawStyleRectangle(hdc, LRect, LColor);
              exit(S_OK);
            end;

          // pressed arrow (button down)
          3:
            begin
              // DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonPressed), pRect);
              LColor := StyleServices.GetSystemColor(clHighlight);
              // AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
              LRect := pRect;
              InflateRect(LRect, -1, -1);
              DrawStyleRectangle(hdc, LRect, LColor);
              exit(S_OK);
            end;

          // selected
          4:
            begin
              // DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonDefaulted), pRect);
              LColor := StyleServices.GetSystemColor(clHighlight);
              // AlphaBlendFillCanvas(hdc, LColor, pRect, 50);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
              LRect := pRect;
              InflateRect(LRect, -1, -1);
              DrawStyleRectangle(hdc, LRect, LColor);
              exit(S_OK);
            end;

          // hot on button
          5:
            begin
              LColor := StyleServices.GetSystemColor(clHighlight);
              // AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
              // DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonHot), pRect);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
              LRect := pRect;
              InflateRect(LRect, -1, -1);
              DrawStyleRectangle(hdc, LRect, LColor);
              exit(S_OK);
            end;

        end;
      end;

    // dropdown arrow
    6:
      begin
        case iStateId of
          0:
            begin
              LRect := pRect;
              LRect.Left := LRect.Left + 5;
              LRect.Width := LRect.Width + 5;
              DrawStyleDownArrow(hdc, LRect, GetStyleBtnTextColor);
              exit(S_OK);
            end;

          // down arrow normal
          1:
            begin
              LRect := pRect;
              LRect.Left := LRect.Left + 2;
              LRect.Width := LRect.Width + 2;
              DrawStyleDownArrow(hdc, LRect, GetStyleBtnTextColor);
              exit(S_OK);
            end;
        end;
      end;

    8: // arrow button - Top Bar of listview
      begin
        case iStateId of
          // normal
          1:
            begin
              LRect := pRect;
              LRect.Left := LRect.Left + 5;
              LRect.Width := LRect.Width + 5;
              DrawStyleDownArrow(hdc, LRect, GetStyleBtnTextColor);
              exit(S_OK);
            end;
          // hot
          2:
            begin
              LRect := pRect;
              LRect.Left := LRect.Left + 5;
              LRect.Width := LRect.Width + 5;
              DrawStyleDownArrow(hdc, LRect, GetStyleBtnTextColor);
              exit(S_OK);
            end;
        end;
      end;

    9: // button -Top Bar of listview
      begin

        case iStateId of
          // normal
          1:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
              exit(S_OK);
            end;

          // hot
          2:
            begin
              LColor := StyleServices.GetSystemColor(clHighlight);
              // AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
              LRect := pRect;
              InflateRect(LRect, -1, -1);
              DrawStyleRectangle(hdc, LRect, LColor);
              exit(S_OK);
            end;

          3: // pressed arrow (button down)
            begin
              LColor := StyleServices.GetSystemColor(clHighlight);
              // AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
              LRect := pRect;
              InflateRect(LRect, -1, -1);
              DrawStyleRectangle(hdc, LRect, LColor);
              exit(S_OK);
            end;

          // selected
          4:
            begin
              LColor := StyleServices.GetSystemColor(clHighlight);
              // AlphaBlendFillCanvas(hdc, LColor, pRect, 50);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
              LRect := pRect;
              InflateRect(LRect, -1, -1);
              DrawStyleRectangle(hdc, LRect, LColor);
              exit(S_OK);
            end;

          // hot on button
          5:
            begin
              LColor := StyleServices.GetSystemColor(clHighlight);
              // AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
              DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
              LRect := pRect;
              InflateRect(LRect, -1, -1);
              DrawStyleRectangle(hdc, LRect, LColor);
              exit(S_OK);
            end;
        end;
      end;

    // Top Bar of listview - background  solid color
    11:
      begin
        case iStateId of
          0:
            begin
              LDetails := StyleServices.GetElementDetails(tpPanelBackground);
              StyleServices.GetElementColor(LDetails, ecFillColor, LColor);
              DrawStyleFillRect(hdc, pRect, LColor);
              exit(S_OK);
            end;
        end;
      end;

    // Top Bar of listview - backgroundimage
    12:
      begin
        case iStateId of
          0:
            begin
              LDetails := StyleServices.GetElementDetails(tpPanelBackground);
              StyleServices.GetElementColor(LDetails, ecFillColor, LColor);
              DrawStyleFillRect(hdc, pRect, LColor);
              exit(S_OK);
            end;
        end;
      end;
  end;

  // OutputDebugString(PChar(Format('UxTheme_CommandModule class %s hTheme %d iPartId %d iStateId %d', [LThemeClass, hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

function Detour_UxTheme_DrawThemeMain(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect;
  Foo: Pointer; Trampoline: TDrawThemeBackground): HRESULT; stdcall;
var
  LThemeClass: string;
  LHWND: hwnd;
  LFuncDrawThemeBackground: TFuncDrawThemeBackground;
begin
  Result := S_FALSE;
  VCLStylesLock.Enter;
  try
    if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled then
    begin
      Result := Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo);
      exit;
    end;

    if not THThemesClasses.ContainsKey(hTheme) then
    begin
      LThemeClass := GetThemeClass(hTheme, iPartId, iStateId);
      if LThemeClass <> '' then
      begin
        THThemesClasses.Add(hTheme, LThemeClass);
        THThemesHWND.Add(hTheme, 0);
      end
      else
      begin
        Result := Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo);
        exit;
      end;
    end
    else
      LThemeClass := THThemesClasses.Items[hTheme];

    LHWND := THThemesHWND.Items[hTheme];

    if FuncsDrawThemeBackground.ContainsKey(LThemeClass) then
    begin
      LFuncDrawThemeBackground := FuncsDrawThemeBackground.Items[LThemeClass];
      Result := LFuncDrawThemeBackground(hTheme, hdc, iPartId, iStateId, pRect, Foo, Trampoline, LThemeClass, LHWND);
    end
    else
    begin
      DrawStyleFillRect(hdc, pRect, clBtnFace);
      Result := S_OK;
    end;
  finally
    VCLStylesLock.Leave;
  end;
end;

function Detour_UxTheme_DrawThemeBackgroundEx(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect;
  pOptions: Pointer): HRESULT; stdcall;
begin
  if not(ExecutingInMainThread) or StyleServices.IsSystemStyle or not(TSysStyleManager.Enabled) then
    exit(Trampoline_UxTheme_DrawThemeBackgroundEx(hTheme, hdc, iPartId, iStateId, pRect, pOptions))
  else
    exit(Detour_UxTheme_DrawThemeMain(hTheme, hdc, iPartId, iStateId, pRect, pOptions,
      Trampoline_UxTheme_DrawThemeBackgroundEx));
end;

function Detour_UxTheme_DrawThemeBackground(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect;
  pClipRect: Pointer): HRESULT; stdcall;
begin
  if not(ExecutingInMainThread) or StyleServices.IsSystemStyle or not(TSysStyleManager.Enabled) then
    exit(Trampoline_UxTheme_DrawThemeBackground(hTheme, hdc, iPartId, iStateId, pRect, pClipRect))
  else
    exit(Detour_UxTheme_DrawThemeMain(hTheme, hdc, iPartId, iStateId, pRect, pClipRect,
      Trampoline_UxTheme_DrawThemeBackground));
end;

function Detour_UxTheme_DrawThemeEdge(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pDestRect: TRect;
  uEdge, uFlags: UINT; pContentRect: pRect): HRESULT; stdcall;
begin
  // OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeEdge  class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
  exit(Trampoline_UxTheme_DrawThemeEdge(hTheme, hdc, iPartId, iStateId, pDestRect, uEdge, uFlags, pContentRect));
end;

function Detour_UxTheme_GetThemeSysColor(hTheme: hTheme; iColorId: Integer): COLORREF; stdcall;
begin
  if not(ExecutingInMainThread) or StyleServices.IsSystemStyle or not(TSysStyleManager.Enabled) then
    Result := Trampoline_UxTheme_GetThemeSysColor(hTheme, iColorId)
  else
    Result := StyleServices.GetSystemColor(iColorId or Integer($FF000000));
end;

function Detour_UxTheme_GetThemeSysColorBrush(hTheme: hTheme; iColorId: Integer): HBRUSH; stdcall;
begin
  // OutputDebugString(PChar(Format('GetThemeSysColorBrush hTheme %d iColorId %d', [hTheme, iColorId])));
  exit(Trampoline_UxTheme_GetThemeSysColorBrush(hTheme, iColorId));
end;

{
  Doesn't affect Menus colors
  Doesn't affect Compressed files font color (blue)
}

function Detour_UxTheme_GetThemeColor(hTheme: hTheme; iPartId, iStateId, iPropId: Integer; var pColor: COLORREF)
  : HRESULT; stdcall;
var
  LThemeClass: string;
  LColor: TColor;
begin
  if not(ExecutingInMainThread) then
    exit(Trampoline_UxTheme_GetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor));

  VCLStylesLock.Enter;
  try
    if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled or not THThemesClasses.ContainsKey(hTheme) then
      exit(Trampoline_UxTheme_GetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor));
    LThemeClass := THThemesClasses.Items[hTheme];
  finally
    VCLStylesLock.Leave;
  end;

  case iPropId of

    TMT_TEXTCOLOR:
      if not SameText(LThemeClass, VSCLASS_TASKDIALOGSTYLE) then
      begin
        Result := Trampoline_UxTheme_GetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
        if (Result = S_OK) and (@Trampoline_user32_GetSysColor <> nil) then
        begin
          // OutputDebugString(PChar(Format('Intercepted Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
          // pColor := ColorToRGB(clRed);
          pColor := ColorToRGB(StyleServices.GetSystemColor(clWindowText));
          // OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
          exit(S_OK);
        end;
      end;
  end;

  // Debug Output: Detour_GetThemeColor Class Communications::Rebar hTheme 65566 iPartId 0 iStateId 0  iPropId 3803 Color   FAFAFA Process ThemedSysControls.exe (13176)
  // Debug Output: Detour_GetThemeColor Class TryHarder hTheme 65579 iPartId 1 iStateId 1  iPropId 3803 Color   5B391E Process ThemedSysControls.exe (13176)
  // Debug Output: Detour_GetThemeColor Class CONTROLPANELSTYLE hTheme 65569 iPartId 7 iStateId 1  iPropId 3803 Color   CC6600 Process ThemedSysControls.exe (13176)

  // Debug Output: Intercepted Detour_GetThemeColor Class CPLCommandModule::CommandModule hTheme 65575 iPartId 3 iStateId 1  iPropId 3803 Color        0 Process ThemedSysControls.exe (14304)
  // Debug Output: Intercepted Detour_GetThemeColor Class InfoBar hTheme 65576 iPartId 2 iStateId 1  iPropId 3803 Color        0 Process ThemedSysControls.exe (14304)

  if LThemeClass <> '' then
  begin
    if SameText(LThemeClass, 'Tooltip') then
    begin
      pColor := clNone;
      case iPartId of
        4:
          case iStateId of
            0:
              pColor := ColorToRGB(StyleServices.GetSystemColor(clHighlight));
          end;
      end;

      if TColor(pColor) = clNone then
      begin
        Result := Trampoline_UxTheme_GetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
        // OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
      end
      else
        Result := S_OK;

    end
    ELSE if SameText(LThemeClass, VSCLASS_CONTROLPANELSTYLE) then
    begin
      pColor := clNone;
      // case iPartId of
      // EP_EDITTEXT :
      // case iStateId  of
      // ETS_CUEBANNER :  pColor:= ColorToRGB(StyleServices.GetSystemColor(clWindowText));
      // end;
      // end;

      if TColor(pColor) = clNone then
      begin
        Result := Trampoline_UxTheme_GetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
        // OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
      end
      else
        Result := S_OK;
    end
    else if SameText(LThemeClass, VSCLASS_EDIT) then
    begin
      pColor := clNone;
      case iPartId of
        EP_EDITTEXT:
          case iStateId of
            ETS_CUEBANNER:
              pColor := ColorToRGB(StyleServices.GetSystemColor(clWindowText));
          end;
      end;

      if TColor(pColor) = clNone then
      begin
        Result := Trampoline_UxTheme_GetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
        // OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
      end
      else
        Result := S_OK;
    end
    else if SameText(LThemeClass, VSCLASS_TRYHARDER) then
    begin
      pColor := clNone;
      case iPartId of

        0:
          case iStateId of
            0:
              pColor := ColorToRGB(StyleServices.GetSystemColor(clWindowText));
          end;

        1:
          case iStateId of
            1:
              pColor := ColorToRGB(StyleServices.GetSystemColor(clWindowText));
          end;
      end;

      if TColor(pColor) = clNone then
      begin
        Result := Trampoline_UxTheme_GetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
        // OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
      end
      else
        Result := S_OK;
    end
    else if SameText(LThemeClass, VSCLASS_MENU) then
    begin
      pColor := clNone;
      // case iPartId of
      //
      // MENU_BARITEM :
      // case iStateId  of
      // 0 :  pColor:= ColorToRGB(clRed);//ColorToRGB(StyleServices.GetSystemColor(clBtnText));
      // end;
      //
      // MENU_POPUPBACKGROUND :
      // case iStateId  of
      // 0 :  pColor:= ColorToRGB(clGreen);//ColorToRGB(StyleServices.GetSystemColor(clBtnText));
      // end;
      //
      // MENU_POPUPITEM :
      // case iStateId  of
      // 0 :  pColor:= ColorToRGB(clFuchsia);//ColorToRGB(StyleServices.GetSystemColor(clBtnText));
      // end;
      //
      // end;

      if TColor(pColor) = clNone then
      begin
        Result := Trampoline_UxTheme_GetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
        // OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
      end
      else
        Result := S_OK;
    end
    else if SameText(VSCLASS_SEARCHEDITBOX, LThemeClass) then
    begin
      pColor := clNone;
      case iPartId of
        1:
          case iStateId of
            2:
              pColor := ColorToRGB(StyleServices.GetSystemColor(clWindowText));
          end;
      end;

      if TColor(pColor) = clNone then
      begin
        Result := Trampoline_UxTheme_GetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
        // OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
      end
      else
        Result := S_OK;
    end
    else if SameText(LThemeClass, VSCLASS_TOOLTIP) then
    begin
      pColor := clNone;
      case iPartId of

        TTP_STANDARD:
          case iStateId of
            0:
              pColor := ColorToRGB(StyleServices.GetSystemColor(clBtnText));
          end;

      end;

      if TColor(pColor) = clNone then
      begin
        Result := Trampoline_UxTheme_GetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
        // OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
      end
      else
        Result := S_OK;
    end
    else if SameText(LThemeClass, VSCLASS_TEXTSTYLE) then
    begin
      pColor := clNone;
      case iPartId of

        TEXT_MAININSTRUCTION:
          case iStateId of
            0:
              begin
                pColor := ColorToRGB(StyleServices.GetSystemColor(clHighlightText));
              end;

          end;

      end;

      if TColor(pColor) = clNone then
      begin
        Result := Trampoline_UxTheme_GetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
        // OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
      end
      else
        Result := S_OK;
    end
    else
{$IFDEF HOOK_ListView}
      if (SameText(LThemeClass, VSCLASS_HEADER) or SameText(LThemeClass, VSCLASS_ITEMSVIEW_HEADER)) then
      begin
        pColor := clNone;
        case iPartId of

          HP_HEADERITEM:
            case iStateId of
              0:
                pColor := ColorToRGB(StyleServices.GetSystemColor(clBtnText));
            end;

        end;

        if TColor(pColor) = clNone then
        begin
          Result := Trampoline_UxTheme_GetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
          // OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
        end
        else
          Result := S_OK;
      end
      else
{$ENDIF}
{$IFDEF HOOK_PreviewPane}
        if SameText(LThemeClass, VSCLASS_READINGPANE) then
        begin
          pColor := clNone;
          case iPartId of

            1: // preview background
              case iStateId of
                0:
                  pColor := ColorToRGB(StyleServices.GetSystemColor(clWindow));
              end;

            2: // preview text
              case iStateId of
                0:
                  pColor := ColorToRGB(StyleServices.GetSystemColor(clWindowText));
              end;
          end;

          if TColor(pColor) = clNone then
          begin
            Result := Trampoline_UxTheme_GetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
            // OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
          end
          else
            Result := S_OK;
        end
        else if SameText(LThemeClass, VSCLASS_PREVIEWPANE) then
        begin
          pColor := clNone;
          case iPartId of

            5:
              case iStateId of
                0:
                  ColorToRGB(StyleServices.GetSystemColor(clHighlight));
              end;

            6:
              case iStateId of
                0:
                  pColor := ColorToRGB(StyleServices.GetSystemColor(clBtnText));
              end;

            7:
              case iStateId of
                1:
                  pColor := GetStyleHighLightColor();
                2:
                  pColor := ColorToRGB(clGreen);
              end;

            8:
              case iStateId of
                0:
                  pColor := ColorToRGB(clRed);
              end;

            9:
              case iStateId of
                1:
                  pColor := ColorToRGB(clBlue);
                2:
                  pColor := ColorToRGB(clYellow);
              end;
          end;

          if TColor(pColor) = clNone then
          begin
            Result := Trampoline_UxTheme_GetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
            // OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
          end
          else
            Result := S_OK;
        end
        else
{$ENDIF}
{$IFDEF HOOK_TaskDialog}
          if SameText(LThemeClass, VSCLASS_TASKDIALOGSTYLE) then
          begin
            pColor := clNone;
            case iPartId of
              TDLG_MAININSTRUCTIONPANE:
                begin
                  pColor := ColorToRGB(GetStyleHighLightColor());
                  if StyleServices.GetStyleColor(scEdit) = StyleServices.GetStyleColor(scBorder) then
                    pColor := ColorToRGB(StyleServices.GetSystemColor(clBtnText));
                end;

              TDLG_CONTENTPANE:
                begin
                  pColor := ColorToRGB(StyleServices.GetSystemColor(clWindowText));
                  if StyleServices.GetStyleColor(scEdit) = StyleServices.GetStyleColor(scBorder) then
                    pColor := ColorToRGB(StyleServices.GetSystemColor(clBtnText));
                end;
              TDLG_EXPANDOTEXT, TDLG_EXPANDEDFOOTERAREA, TDLG_FOOTNOTEPANE, TDLG_VERIFICATIONTEXT,
              // TDLG_RADIOBUTTONPANE,
              TDLG_EXPANDEDCONTENT:
                begin
                  pColor := ColorToRGB(StyleServices.GetSystemColor(clWindowText));
                  if StyleServices.GetStyleColor(scEdit) = StyleServices.GetStyleColor(scBorder) then
                    pColor := ColorToRGB(StyleServices.GetSystemColor(clBtnText));
                end;
            end;

            if TColor(pColor) = clNone then
            begin
              Result := Trampoline_UxTheme_GetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
              // OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
            end
            else
              Result := S_OK;
          end
          else
{$ENDIF}
{$IFDEF HOOK_TreeView}
            if SameText(LThemeClass, VSCLASS_TREEVIEW) or SameText(LThemeClass, VSCLASS_PROPERTREE) or
              SameText(LThemeClass, 'ExplorerNavPane') then
            begin

              pColor := clNone;
              case iPartId of
                0, 2:
                  case iStateId of
                    0:
                      pColor := ColorToRGB(StyleServices.GetSystemColor(clWindow)); // OK

                  end;
              end;

              if TColor(pColor) = clNone then
              begin
                Result := Trampoline_UxTheme_GetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
                // OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
              end
              else
                Result := S_OK;
            end
            else
{$ENDIF}
{$IFDEF HOOK_ListView}
              if (SameText(LThemeClass, VSCLASS_ITEMSVIEW) or SameText(LThemeClass, VSCLASS_LISTVIEW) or
                SameText(LThemeClass, VSCLASS_LISTVIEWSTYLE) or SameText(LThemeClass, VSCLASS_ITEMSVIEW_LISTVIEW) or
                SameText(LThemeClass, VSCLASS_EXPLORER_LISTVIEW)) then
              begin

                pColor := clNone;
                case iPartId of
                  0:
                    case iStateId of
                      0:
                        pColor := ColorToRGB(StyleServices.GetSystemColor(clWindow));
                    end;

                  LVP_LISTITEM:
                    case iStateId of
                      0:
                        pColor := ColorToRGB(clRed);
                    end;

                  LVP_LISTSORTEDDETAIL:
                    case iStateId of
                      1:
                        pColor := ColorToRGB(StyleServices.GetSystemColor(clWindowText));
                      // normal main column (name)
                      2:
                        pColor := ColorToRGB(clWindowText);

                      // SELECTED
                      3:
                        pColor := ColorToRGB(StyleServices.GetSystemColor(clWindowText));

                      // hot text
                      4:
                        pColor := ColorToRGB(StyleServices.GetSystemColor(clWindowText));
                      5:
                        pColor := ColorToRGB(clBlue);
                      6:
                        pColor := ColorToRGB(clYellow);
                      7:
                        pColor := ColorToRGB(clGreen);
                      8:
                        pColor := ColorToRGB(clFuchsia);
                    end;

                  LVP_EMPTYTEXT:
                    case iStateId of
                      0:
                        pColor := ColorToRGB(StyleServices.GetSystemColor(clHighlight));
                    end;

                  LVP_GROUPHEADER:
                    case iStateId of
                      0:
                        pColor := ColorToRGB(StyleServices.GetSystemColor(clWindowText));
                    end;
                end;

                if TColor(pColor) = clNone then
                begin
                  // OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
                  Result := Trampoline_UxTheme_GetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
                end
                else
                  Result := S_OK;
              end
              else
{$ENDIF}
{$IFDEF HOOK_CommandModule}
                if SameText(LThemeClass, VSCLASS_COMMANDMODULE) or SameText(LThemeClass, VSCLASS_CPLCOMMANDMODULE) then
                begin

                  pColor := clNone;
                  case iPartId of

                    // button with dropdown
                    3:
                      case iStateId of
                        1:
                          pColor := ColorToRGB(GetStyleBtnTextColor); // GetStyleHighLightColor;
                        6:
                          pColor := ColorToRGB(clYellow); // StyleServices.GetSystemColor(clBtnShadow);
                      end;

                    4:
                      case iStateId of
                        1:
                          pColor := ColorToRGB(GetStyleBtnTextColor);
                      end;

                    9:
                      case iStateId of
                        1:
                          pColor := ColorToRGB(GetStyleBtnTextColor);
                          // ColorToRGB(StyleServices.GetSystemColor(clBtnText));
                        // Highlight
                        2:
                          pColor := ColorToRGB(GetStyleBtnTextColor);
                          // ColorToRGB(StyleServices.GetSystemColor(clBtnText)); //OK
                        3:
                          pColor := ColorToRGB(GetStyleBtnTextColor);
                          // ColorToRGB(StyleServices.GetSystemColor(clBtnText)); //OK
                        6:
                          pColor := ColorToRGB(clLime); // StyleServices.GetSystemColor(clBtnShadow);
                      end;

                    // header text
                    10:
                      case iStateId of
                        1:
                          pColor := ColorToRGB(GetStyleHighLightColor);
                      end;
                  end;

                  Result := S_OK;
                  // if pColor=clNone then
                  // OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
                end
                else
{$ENDIF}
                begin
                  Result := Trampoline_UxTheme_GetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
                  // pColor:=ColorToRGB(clRed);
                  // Result := S_OK;
                  // OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
                end;
  end
  else
  begin
    Result := Trampoline_UxTheme_GetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
    // OutputDebugString(PChar(Format('Detour_GetThemeColor hTheme %d iPartId %d iStateId %d  Color %8.x', [hTheme, iPartId, iStateId, pColor])));
    // OutputDebugString2(Format('Detour_GetThemeColor hTheme %d iPartId %d iStateId %d  Color %8.x', [hTheme, iPartId, iStateId, pColor]));
  end;

  // OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
end;

procedure _DrawControlText(Canvas: TCanvas; const S: string; var R: TRect; Flags: Cardinal; ThemeTextColor: TColor);
var
  TextFormat: TTextFormatFlags;
begin
  // Canvas.Font := TWinControlClass(Control).Font;
  Canvas.Font.Color := ThemeTextColor;
  TextFormat := TTextFormatFlags(Flags);
  DrawText(Canvas.Handle, S, Length(S), R, TTextFormatFlags(TextFormat));
end;

function Detour_UxTheme_DrawThemeText(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; pszText: LPCWSTR;
  iCharCount: Integer; dwTextFlags, dwTextFlags2: DWORD; const pRect: TRect): HRESULT; stdcall;
var
  LThemeClass: string;
  LThemeClasses: TStrings;
  LDetails: TThemedElementDetails;
  ThemeTextColor: TColor;
  p, SaveIndex: Integer;
  LCanvas: TCanvas;
  plf: LOGFONTW;
  LText: string;
  LRect: TRect;
begin
  if not(ExecutingInMainThread) then
    exit(Trampoline_UxTheme_DrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount, dwTextFlags,
      dwTextFlags2, pRect));

  LThemeClasses := TStringList.Create;
  try
    VCLStylesLock.Enter;
    try
      if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled or (dwTextFlags and DT_CALCRECT <> 0) or
        not THThemesClasses.ContainsKey(hTheme) then
        exit(Trampoline_UxTheme_DrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount, dwTextFlags,
          dwTextFlags2, pRect));

      LThemeClass := THThemesClasses.Items[hTheme];
      ExtractStrings([';'], [], PChar(LThemeClass), LThemeClasses);
      // OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeText hTheme %d iPartId %d iStateId %d  text %s %s', [hTheme, iPartId, iStateId, pszText, LThemeClass])));

      // if Pos('Search', pszText)>0 then
      // OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeText hTheme %d class %s iPartId %d iStateId %d  text %s', [hTheme, LThemeClass, iPartId, iStateId, pszText])));
      //

    finally
      VCLStylesLock.Leave;
    end;

    if (LThemeClass <> '') then
    begin

{$IFDEF HOOK_ToolBar}
      if SameText(LThemeClass, VSCLASS_TOOLBAR) then // OK
      begin
        case iPartId of
          0:
            begin
              case iStateId of
                TS_NORMAL:
                  begin
                    ThemeTextColor := StyleServices.GetSystemColor(clWindowText);
                    // StyleServices.GetSystemColor(clBtnText);
                    LDetails := StyleServices.GetElementDetails(ttbButtonNormal);
                  end;

                TS_HOT:
                  begin
                    // ThemeTextColor := GetStyleHighLightColor();
                    ThemeTextColor := StyleServices.GetSystemColor(clWindowText);
                    LDetails := StyleServices.GetElementDetails(ttbButtonHot);
                  end;

                TS_PRESSED:
                  begin
                    // ThemeTextColor := GetStyleHighLightColor();
                    ThemeTextColor := StyleServices.GetSystemColor(clWindowText);
                    LDetails := StyleServices.GetElementDetails(ttbButtonPressed);
                  end;

                TS_NEARHOT:
                  begin
                    ThemeTextColor := StyleServices.GetSystemColor(clWindowText);
                    // StyleServices.GetSystemColor(clBtnText);
                    LDetails := StyleServices.GetElementDetails(ttbButtonHot);
                  end;

                TS_OTHERSIDEHOT:
                  begin
                    ThemeTextColor := StyleServices.GetSystemColor(clWindowText); // GetStyleHighLightColor();
                    LDetails := StyleServices.GetElementDetails(ttbButtonHot);
                  end;

                TS_DISABLED:
                  begin
                    ThemeTextColor := StyleServices.GetSystemColor(clGrayText);
                    LDetails := StyleServices.GetElementDetails(ttbButtonDisabled);
                  end;

              else
                begin
                  // OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeTextEx hTheme %d iPartId %d iStateId %d  text %s', [hTheme, iPartId, iStateId, pszText])));
                  exit(Trampoline_UxTheme_DrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount,
                    dwTextFlags, dwTextFlags2, pRect));
                end;
              end;

              // OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeText hTheme %d class %s iPartId %d iStateId %d  text %s', [hTheme, LThemeClass, iPartId, iStateId, pszText])));
              LRect := pRect;
              StyleServices.DrawText(hdc, LDetails, string(pszText), LRect, TTextFormatFlags(dwTextFlags),
                ThemeTextColor);
              exit(S_OK);
            end;
        else
          exit(Trampoline_UxTheme_DrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount, dwTextFlags,
            dwTextFlags2, pRect));
        end;
      end
      else
{$ENDIF}
{$IFDEF HOOK_CommandModule}
        if SameText(LThemeClass, VSCLASS_COMMANDMODULE) then
        begin
          // OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeTextEx hTheme %d iPartId %d iStateId %d  text %s', [hTheme, iPartId, iStateId, pszText])));
          exit(Trampoline_UxTheme_DrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount, dwTextFlags,
            dwTextFlags2, pRect));
        end
        else
{$ENDIF}
{$IFDEF HOOK_Menu}
          if SameText(LThemeClass, VSCLASS_MENU) then
          begin
            case iPartId of

              MENU_POPUPITEM:
                begin
                  SaveIndex := SaveDC(hdc);
                  try
                    case iStateId of
                      MPI_NORMAL:
                        LDetails := StyleServices.GetElementDetails(tmPopupItemNormal);
                      MPI_HOT:
                        LDetails := StyleServices.GetElementDetails(tmPopupItemHot);
                      // MPI_PUSHED         : LDetails := StyleServices.GetElementDetails(tmMenuBarItemPushed);
                      MPI_DISABLED:
                        LDetails := StyleServices.GetElementDetails(tmPopupItemDisabled);
                      MPI_DISABLEDHOT:
                        LDetails := StyleServices.GetElementDetails(tmPopupItemDisabledHot);
                      // MPI_DISABLEDPUSHED : LDetails := StyleServices.GetElementDetails(tmMenuBarItemDisabledPushed);
                    else
                      LDetails := StyleServices.GetElementDetails(tmPopupItemNormal);
                    end;

                    LRect := pRect;
                    StyleServices.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
                    LText := string(pszText);

                    StyleServices.DrawText(hdc, LDetails, LText, LRect, TTextFormatFlags(dwTextFlags), ThemeTextColor);
                    exit(S_OK);
                  finally
                    RestoreDC(hdc, SaveIndex);
                  end;
                end;

              MENU_BARITEM:
                begin
                  SaveIndex := SaveDC(hdc);
                  try
                    case iStateId of
                      MBI_NORMAL:
                        LDetails := StyleServices.GetElementDetails(tmPopupItemNormal);
                      MBI_HOT:
                        LDetails := StyleServices.GetElementDetails(tmMenuBarItemHot);
                      MBI_PUSHED:
                        LDetails := StyleServices.GetElementDetails(tmMenuBarItemPushed);
                      MBI_DISABLED:
                        LDetails := StyleServices.GetElementDetails(tmMenuBarItemDisabled);
                      MBI_DISABLEDHOT:
                        LDetails := StyleServices.GetElementDetails(tmMenuBarItemDisabledHot);
                      MBI_DISABLEDPUSHED:
                        LDetails := StyleServices.GetElementDetails(tmMenuBarItemDisabledPushed);
                    end;

                    LRect := pRect;
                    StyleServices.DrawText(hdc, LDetails, string(pszText), LRect, TTextFormatFlags(dwTextFlags),
                      ThemeTextColor);
                    exit(S_OK);
                  finally
                    RestoreDC(hdc, SaveIndex);
                  end;
                end;
            else
              begin
                // OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeTextEx hTheme %d iPartId %d iStateId %d  text %s', [hTheme, iPartId, iStateId, pszText])));
                exit(Trampoline_UxTheme_DrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount, dwTextFlags,
                  dwTextFlags2, pRect));
              end;
            end;
          end
          else
{$ENDIF}
{$IFDEF  HOOK_DateTimePicker}
            if SameText(LThemeClass, VSCLASS_DATEPICKER) then
            begin
              case iPartId of
                DP_DATETEXT:
                  begin
                    case iStateId of
                      DPDT_NORMAL:
                        begin
                          ThemeTextColor := StyleServices.GetSystemColor(clWindowText);
                          LDetails := StyleServices.GetElementDetails(teEditTextNormal);
                        end;
                      DPDT_DISABLED:
                        begin
                          ThemeTextColor := StyleServices.GetSystemColor(clGrayText);
                          LDetails := StyleServices.GetElementDetails(teEditTextDisabled);
                        end;
                      DPDT_SELECTED:
                        begin
                          ThemeTextColor := StyleServices.GetSystemColor(clHighlightText);
                          LDetails := StyleServices.GetElementDetails(tgCellSelected);
                          // Fix issue with selected text color
                        end;
                    end;

                    LRect := pRect;
                    StyleServices.DrawText(hdc, LDetails, string(pszText), LRect, TTextFormatFlags(dwTextFlags),
                      ThemeTextColor);
                    exit(S_OK);
                  end;
              else
                exit(Trampoline_UxTheme_DrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount, dwTextFlags,
                  dwTextFlags2, pRect));
              end;
            end
            else if SameText(LThemeClass, VSCLASS_MONTHCAL) then
            begin
              case iPartId of
                MC_TRAILINGGRIDCELLUPPER, MC_GRIDCELLUPPER, MC_GRIDCELL:
                  begin
                    // case iStateId of
                    // MCGCB_SELECTED           :   LDetails := StyleServices.GetElementDetails(tgCellSelected);
                    // MCGCB_HOT                :   LDetails := StyleServices.GetElementDetails(tgFixedCellHot);
                    // MCGCB_SELECTEDHOT        :   LDetails := StyleServices.GetElementDetails(tgCellSelected);
                    // MCGCB_SELECTEDNOTFOCUSED :   LDetails := StyleServices.GetElementDetails(tgCellSelected);
                    // MCGCB_TODAY              :   LDetails := StyleServices.GetElementDetails(tgFixedCellHot);
                    // else
                    // LDetails := StyleServices.GetElementDetails(tgCellNormal);
                    // end;

                    LDetails := StyleServices.GetElementDetails(tgCellNormal);

                    if not StyleServices.GetElementColor(LDetails, ecTextColor, ThemeTextColor) then
                      ThemeTextColor := StyleServices.GetSystemColor(clBtnText);

                    LCanvas := TCanvas.Create;
                    SaveIndex := SaveDC(hdc);
                    try
                      LCanvas.Handle := hdc;
                      ZeroMemory(@plf, SizeOf(plf));
                      plf.lfHeight := 13;
                      plf.lfCharSet := DEFAULT_CHARSET;
                      StrCopy(plf.lfFaceName, 'Tahoma');
                      LCanvas.Font.Handle := CreateFontIndirect(plf);

                      LText := string(pszText);
                      p := Pos(Chr($A), LText);
                      if p > 1 then
                        LText := Copy(LText, 1, p - 1);

                      LRect := pRect;
                      StyleServices.DrawText(LCanvas.Handle, LDetails, LText, LRect, TTextFormatFlags(dwTextFlags),
                        ThemeTextColor);
                    finally
                      DeleteObject(LCanvas.Font.Handle);
                      LCanvas.Handle := 0;
                      LCanvas.Free;
                      RestoreDC(hdc, SaveIndex);
                    end;

                    // OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeText hTheme %d iPartId %d iStateId %d  text %s', [hTheme, iPartId, iStateId, pszText])));
                    exit(S_OK);
                  end;

                MC_TRAILINGGRIDCELL:
                  begin
                    case iStateId of
                      MCTGC_HOT:
                        LDetails := StyleServices.GetElementDetails(tgFixedCellHot);
                      MCTGC_HASSTATE:
                        LDetails := StyleServices.GetElementDetails(tgCellSelected);
                      MCTGC_HASSTATEHOT:
                        LDetails := StyleServices.GetElementDetails(tgCellSelected);
                      MCTGC_TODAY:
                        LDetails := StyleServices.GetElementDetails(tgFixedCellHot);
                    else
                      LDetails := StyleServices.GetElementDetails(teEditTextDisabled);
                    end;

                    if not StyleServices.GetElementColor(LDetails, ecTextColor, ThemeTextColor) then
                      ThemeTextColor := StyleServices.GetSystemColor(clBtnText);

                    LCanvas := TCanvas.Create;
                    SaveIndex := SaveDC(hdc);
                    try
                      LCanvas.Handle := hdc;
                      ZeroMemory(@plf, SizeOf(plf));
                      plf.lfHeight := 13;
                      plf.lfCharSet := DEFAULT_CHARSET;
                      StrCopy(plf.lfFaceName, 'Tahoma');
                      LCanvas.Font.Handle := CreateFontIndirect(plf);

                      LText := string(pszText);
                      p := Pos(Chr($A), LText);
                      if p > 1 then
                        LText := Copy(LText, 1, p - 1);

                      LRect := pRect;
                      StyleServices.DrawText(LCanvas.Handle, LDetails, LText, LRect, TTextFormatFlags(dwTextFlags),
                        ThemeTextColor);
                    finally
                      DeleteObject(LCanvas.Font.Handle);
                      LCanvas.Handle := 0;
                      LCanvas.Free;
                      RestoreDC(hdc, SaveIndex);
                    end;
                    exit(S_OK);
                  end;
              else
                begin
                  // OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeText hTheme %d iPartId %d iStateId %d  text %s', [hTheme, iPartId, iStateId, pszText])));
                  exit(Trampoline_UxTheme_DrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount,
                    dwTextFlags, dwTextFlags2, pRect));
                end;
              end;
            end
            else
{$ENDIF}
{$IFDEF HOOK_Button}
              if LThemeClasses.IndexOf(VSCLASS_BUTTON) >= 0 then // SameText(LThemeClass, VSCLASS_BUTTON) then
              begin
                case iPartId of
                  BP_PUSHBUTTON:
                    begin
                      case iStateId of
                        PBS_NORMAL:
                          LDetails := StyleServices.GetElementDetails(tbPushButtonNormal);
                        PBS_HOT:
                          LDetails := StyleServices.GetElementDetails(tbPushButtonHot);
                        PBS_PRESSED:
                          LDetails := StyleServices.GetElementDetails(tbPushButtonPressed);
                        PBS_DISABLED:
                          LDetails := StyleServices.GetElementDetails(tbPushButtonDisabled);
                        PBS_DEFAULTED:
                          LDetails := StyleServices.GetElementDetails(tbPushButtonDefaulted);
                        PBS_DEFAULTED_ANIMATING:
                          LDetails := StyleServices.GetElementDetails(tbPushButtonDefaultedAnimating);
                      end;

                      // StyleServices.DrawText(hdc,  LDetails, string(pszText), pRect, dwTextFlags, dwTextFlags2);

                      if not StyleServices.GetElementColor(LDetails, ecTextColor, ThemeTextColor) then
                        ThemeTextColor := StyleServices.GetSystemColor(clBtnText);
                      LRect := pRect;
                      StyleServices.DrawText(hdc, LDetails, string(pszText), LRect, TTextFormatFlags(dwTextFlags),
                        ThemeTextColor);
                      exit(S_OK);
                    end;

                  BP_RADIOBUTTON:
                    begin
                      case iStateId of
                        RBS_UNCHECKEDNORMAL:
                          LDetails := StyleServices.GetElementDetails(tbRadioButtonUncheckedNormal);
                        RBS_UNCHECKEDHOT:
                          LDetails := StyleServices.GetElementDetails(tbRadioButtonUncheckedHot);
                        RBS_UNCHECKEDPRESSED:
                          LDetails := StyleServices.GetElementDetails(tbRadioButtonUncheckedPressed);
                        RBS_UNCHECKEDDISABLED:
                          LDetails := StyleServices.GetElementDetails(tbRadioButtonUncheckedDisabled);
                        RBS_CHECKEDNORMAL:
                          LDetails := StyleServices.GetElementDetails(tbRadioButtonCheckedNormal);
                        RBS_CHECKEDHOT:
                          LDetails := StyleServices.GetElementDetails(tbRadioButtonCheckedHot);
                        RBS_CHECKEDPRESSED:
                          LDetails := StyleServices.GetElementDetails(tbRadioButtonCheckedPressed);
                        RBS_CHECKEDDISABLED:
                          LDetails := StyleServices.GetElementDetails(tbRadioButtonCheckedDisabled);
                      end;

                      if not StyleServices.GetElementColor(LDetails, ecTextColor, ThemeTextColor) then
                        ThemeTextColor := StyleServices.GetSystemColor(clBtnText);
                      LRect := pRect;
                      StyleServices.DrawText(hdc, LDetails, string(pszText), LRect, TTextFormatFlags(dwTextFlags),
                        ThemeTextColor);
                      exit(S_OK);
                    end;

                  BP_COMMANDLINK:
                    begin

                      case iStateId of
                        CMDLS_NORMAL:
                          LDetails := StyleServices.GetElementDetails(tbPushButtonNormal);
                        CMDLS_HOT:
                          LDetails := StyleServices.GetElementDetails(tbPushButtonHot);
                        CMDLS_PRESSED:
                          LDetails := StyleServices.GetElementDetails(tbPushButtonPressed);
                        CMDLS_DISABLED:
                          LDetails := StyleServices.GetElementDetails(tbPushButtonDisabled);
                        CMDLS_DEFAULTED:
                          LDetails := StyleServices.GetElementDetails(tbPushButtonDefaulted);
                        CMDLS_DEFAULTED_ANIMATING:
                          LDetails := StyleServices.GetElementDetails(tbPushButtonDefaultedAnimating);
                      end;

                      LCanvas := TCanvas.Create;
                      SaveIndex := SaveDC(hdc);
                      try
                        LCanvas.Handle := hdc;
                        ZeroMemory(@plf, SizeOf(plf));
                        plf.lfHeight := 14;
                        plf.lfCharSet := DEFAULT_CHARSET;
                        StrCopy(plf.lfFaceName, 'Tahoma');
                        LCanvas.Font.Handle := CreateFontIndirect(plf);
                        if not StyleServices.GetElementColor(LDetails, ecTextColor, ThemeTextColor) then
                          ThemeTextColor := StyleServices.GetSystemColor(clBtnText);
                        LRect := pRect;
                        StyleServices.DrawText(LCanvas.Handle, LDetails, string(pszText), LRect,
                          TTextFormatFlags(dwTextFlags), ThemeTextColor);
                      finally
                        DeleteObject(LCanvas.Font.Handle);
                        LCanvas.Handle := 0;
                        LCanvas.Free;
                        RestoreDC(hdc, SaveIndex);
                      end;

                      exit(S_OK);
                    end
                else
                  begin
                    // OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeText hTheme %d iPartId %d iStateId %d  text %s', [hTheme, iPartId, iStateId, pszText])));
                    exit(Trampoline_UxTheme_DrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount,
                      dwTextFlags, dwTextFlags2, pRect));
                  end;
                end;
              end
              else
{$ENDIF}
              begin
                // OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeText hTheme %d iPartId %d iStateId %d  text %s', [hTheme, iPartId, iStateId, pszText])));
                exit(Trampoline_UxTheme_DrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount, dwTextFlags,
                  dwTextFlags2, pRect));
              end;
    end
    else
      exit(Trampoline_UxTheme_DrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount, dwTextFlags,
        dwTextFlags2, pRect));
  finally
    LThemeClasses.Free;
  end;
end;

function Detour_UxTheme_DrawThemeTextEx(hTheme: hTheme; hdc: hdc; iPartId: Integer; iStateId: Integer; pszText: LPCWSTR;
  cchText: Integer; dwTextFlags: DWORD; pRect: pRect; var pOptions: TDTTOpts): HRESULT; stdcall;
var
  LDetails: TThemedElementDetails;
  ThemeTextColor: TColor;
  SaveIndex: Integer;
  LCanvas: TCanvas;
  LThemeClass: string;
  plf: LOGFONTW;
begin
  if not(ExecutingInMainThread) then
    exit(Trampoline_UxTheme_DrawThemeTextEx(hTheme, hdc, iPartId, iStateId, pszText, cchText, dwTextFlags, pRect,
      pOptions));

  VCLStylesLock.Enter;
  try
    if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled or (dwTextFlags and DT_CALCRECT <> 0) or
      not THThemesClasses.ContainsKey(hTheme) then
      exit(Trampoline_UxTheme_DrawThemeTextEx(hTheme, hdc, iPartId, iStateId, pszText, cchText, dwTextFlags, pRect,
        pOptions));

    LThemeClass := THThemesClasses.Items[hTheme];
  finally
    VCLStylesLock.Leave;
  end;

  if LThemeClass <> '' then
  begin
    if SameText(LThemeClass, VSCLASS_TREEVIEW) then
    begin
      case iPartId of
        1:
          begin
            if iStateId = 2 then
            begin
              LCanvas := TCanvas.Create;
              SaveIndex := SaveDC(hdc);
              try
                LCanvas.Handle := hdc;
                if pOptions.dwFlags AND DTT_FONTPROP <> 0 then
                begin
                  ZeroMemory(@plf, SizeOf(plf));
                  plf.lfHeight := 13;
                  plf.lfCharSet := DEFAULT_CHARSET;
                  StrCopy(plf.lfFaceName, 'Tahoma');
                  LCanvas.Font.Handle := CreateFontIndirect(plf);
                end;
                LDetails := StyleServices.GetElementDetails(tlListItemNormal);
                ThemeTextColor := StyleServices.GetStyleFontColor(sfListItemTextNormal);
                StyleServices.DrawText(LCanvas.Handle, LDetails, string(pszText), pRect^, TTextFormatFlags(dwTextFlags),
                  ThemeTextColor);
              finally
                if pOptions.dwFlags AND DTT_FONTPROP <> 0 then
                  DeleteObject(LCanvas.Font.Handle);
                LCanvas.Handle := 0;
                LCanvas.Free;
                RestoreDC(hdc, SaveIndex);
              end;

              Result := S_OK;
            end
            else
            begin
              // OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeTextEx hTheme %d iPartId %d iStateId %d  text %s', [hTheme, iPartId, iStateId, pszText])));
              exit(Trampoline_UxTheme_DrawThemeTextEx(hTheme, hdc, iPartId, iStateId, pszText, cchText, dwTextFlags,
                pRect, pOptions));
            end;
          end;
      else
        begin
          // OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeTextEx hTheme %d iPartId %d iStateId %d  text %s', [hTheme, iPartId, iStateId, pszText])));
          exit(Trampoline_UxTheme_DrawThemeTextEx(hTheme, hdc, iPartId, iStateId, pszText, cchText, dwTextFlags, pRect,
            pOptions));
        end;
      end;
    end
{$IFDEF HOOK_ListView}
    else if SameText(LThemeClass, VSCLASS_LISTVIEW) or SameText(LThemeClass, VSCLASS_ITEMSVIEW_LISTVIEW) then
    begin
      case iPartId of
        LVP_GROUPHEADER:
          begin
            if iStateId = 0 then
            begin
              LCanvas := TCanvas.Create;
              SaveIndex := SaveDC(hdc);
              try
                LCanvas.Handle := hdc;
                if pOptions.dwFlags AND DTT_FONTPROP <> 0 then
                begin
                  ZeroMemory(@plf, SizeOf(plf));
                  plf.lfHeight := 13;
                  plf.lfCharSet := DEFAULT_CHARSET;
                  StrCopy(plf.lfFaceName, 'Tahoma');
                  LCanvas.Font.Handle := CreateFontIndirect(plf);
                end;
                LDetails := StyleServices.GetElementDetails(tlListItemNormal);
                ThemeTextColor := StyleServices.GetStyleFontColor(sfListItemTextNormal);
                StyleServices.DrawText(LCanvas.Handle, LDetails, string(pszText), pRect^, TTextFormatFlags(dwTextFlags),
                  ThemeTextColor);
              finally
                if pOptions.dwFlags AND DTT_FONTPROP <> 0 then
                  DeleteObject(LCanvas.Font.Handle);
                LCanvas.Handle := 0;
                LCanvas.Free;
                RestoreDC(hdc, SaveIndex);
              end;

              Result := S_OK;
            end
            else
            begin
              // OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeTextEx hTheme %d iPartId %d iStateId %d  text %s', [hTheme, iPartId, iStateId, pszText])));
              exit(Trampoline_UxTheme_DrawThemeTextEx(hTheme, hdc, iPartId, iStateId, pszText, cchText, dwTextFlags,
                pRect, pOptions));
            end;
          end;

      else
        begin
          // OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeTextEx hTheme %d iPartId %d iStateId %d  text %s', [hTheme, iPartId, iStateId, pszText])));
          exit(Trampoline_UxTheme_DrawThemeTextEx(hTheme, hdc, iPartId, iStateId, pszText, cchText, dwTextFlags, pRect,
            pOptions));
        end;
      end;
    end
    else
{$ENDIF}
{$IFDEF HOOK_Button}
      if SameText(LThemeClass, VSCLASS_BUTTON) then
      begin
        case iPartId of
          BP_COMMANDLINK:
            begin
              case iStateId of
                CMDLS_NORMAL:
                  LDetails := StyleServices.GetElementDetails(tbPushButtonNormal);
                CMDLS_HOT:
                  LDetails := StyleServices.GetElementDetails(tbPushButtonHot);
                CMDLS_PRESSED:
                  LDetails := StyleServices.GetElementDetails(tbPushButtonPressed);
                CMDLS_DISABLED:
                  LDetails := StyleServices.GetElementDetails(tbPushButtonDisabled);
                CMDLS_DEFAULTED:
                  LDetails := StyleServices.GetElementDetails(tbPushButtonDefaulted);
                CMDLS_DEFAULTED_ANIMATING:
                  LDetails := StyleServices.GetElementDetails(tbPushButtonDefaultedAnimating);
              end;

              if not StyleServices.GetElementColor(LDetails, ecTextColor, ThemeTextColor) then
                ThemeTextColor := StyleServices.GetSystemColor(clBtnText);

              LCanvas := TCanvas.Create;
              SaveIndex := SaveDC(hdc);
              try
                LCanvas.Handle := hdc;
                if pOptions.dwFlags AND DTT_FONTPROP <> 0 then
                begin
                  // GetThemeSysFont(hTheme, pOptions.iFontPropId, plf);  // is not working
                  ZeroMemory(@plf, SizeOf(plf));
                  plf.lfHeight := 13;
                  plf.lfCharSet := DEFAULT_CHARSET;
                  StrCopy(plf.lfFaceName, 'Tahoma');
                  LCanvas.Font.Handle := CreateFontIndirect(plf);
                end;
                StyleServices.DrawText(LCanvas.Handle, LDetails, string(pszText), pRect^, TTextFormatFlags(dwTextFlags),
                  ThemeTextColor);
              finally
                if pOptions.dwFlags AND DTT_FONTPROP <> 0 then
                  DeleteObject(LCanvas.Font.Handle);
                LCanvas.Handle := 0;
                LCanvas.Free;
                RestoreDC(hdc, SaveIndex);
              end;

              Result := S_OK;
            end
        else
          Result := Trampoline_UxTheme_DrawThemeTextEx(hTheme, hdc, iPartId, iStateId, pszText, cchText, dwTextFlags,
            pRect, pOptions);
        end;
      end
      else
{$ENDIF}
        Result := Trampoline_UxTheme_DrawThemeTextEx(hTheme, hdc, iPartId, iStateId, pszText, cchText, dwTextFlags,
          pRect, pOptions);
  end
  else
    Result := Trampoline_UxTheme_DrawThemeTextEx(hTheme, hdc, iPartId, iStateId, pszText, cchText, dwTextFlags, pRect,
      pOptions);
end;

{$IF CompilerVersion >= 30}

function InterceptCreateOrdinal(const Module: string; MethodName: Integer; const InterceptProc: Pointer;
  ForceLoadModule: Boolean = True; Options: Byte = v1compatibility): Pointer;

var
  pOrgPointer: Pointer;
  LModule: THandle;
begin
  Result := nil;
  LModule := GetModuleHandle(PChar(Module));
  if (LModule = 0) and ForceLoadModule then
    LModule := LoadLibrary(PChar(Module));

  if LModule <> 0 then
  begin
    pOrgPointer := GetProcAddress(LModule, PChar(MethodName));
    if Assigned(pOrgPointer) then
      Result := DDetours.InterceptCreate(pOrgPointer, InterceptProc, Options);
  end;
end;
{$IFEND}

function UxTheme_CommonItemsDialog(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect;
  Foo: Pointer; Trampoline: TDrawThemeBackground; LThemeClass: string; hwnd: hwnd): HRESULT; Stdcall;
begin
  case iPartId of
    1:
      begin
        case iStateId of
          // background
          0:
            begin
              DrawStyleElement(hdc, StyleServices.GetElementDetails(twWindowRoot), pRect);
              exit(S_OK);
            end;
        end;
      end;
  end;

  // OutputDebugString(PChar(Format('UxTheme_CommonItemsDialog class %s hTheme %d iPartId %d iStateId %d', [LThemeClass, hTheme, iPartId, iStateId])));
  exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
end;

const
  themelib = 'uxtheme.dll';

initialization

VCLStylesLock := TCriticalSection.Create;
THThemesClasses := TDictionary<hTheme, string>.Create;
THThemesHWND := TDictionary<hTheme, hwnd>.Create;
FuncsDrawThemeBackground := TDictionary<string, TFuncDrawThemeBackground>.Create(TIStringComparer.Ordinal);

if StyleServices.Available then
begin
{$IFDEF HOOK_InfoBar}
  FuncsDrawThemeBackground.Add(VSCLASS_INFOBAR, @UxTheme_InfoBar);
{$ENDIF}
{$IFDEF HOOK_BREADCRUMBAR}
  FuncsDrawThemeBackground.Add(VSCLASS_BREADCRUMBAR, @UxTheme_BreadCrumBar);
{$ENDIF}
{$IFDEF HOOK_TRYHARDER}
  FuncsDrawThemeBackground.Add(VSCLASS_TRYHARDER, @UxTheme_TryHarder);
{$ENDIF}
{$IFDEF HOOK_Tab}
  FuncsDrawThemeBackground.Add(VSCLASS_TAB, @UxTheme_Tab);
{$ENDIF}
{$IFDEF HOOK_ToolTip}
  FuncsDrawThemeBackground.Add(VSCLASS_TOOLTIP, @UxTheme_ToolTip);
{$ENDIF}
{$IFDEF HOOK_TrackBar}
  FuncsDrawThemeBackground.Add(VSCLASS_TRACKBAR, @UxTheme_TrackBar);
{$ENDIF}
{$IFDEF HOOK_PreviewPane}
  FuncsDrawThemeBackground.Add(VSCLASS_PREVIEWPANE, @UxTheme_PreviewPane);
{$ENDIF}
{$IFDEF HOOK_ToolBar}
  FuncsDrawThemeBackground.Add(VSCLASS_TOOLBAR, @UxTheme_ToolBar);
{$ENDIF}
{$IFDEF HOOK_AddressBand}
  FuncsDrawThemeBackground.Add(VSCLASS_ADDRESSBAND, @UxTheme_AddressBand);
{$ENDIF}
{$IFDEF HOOK_SearchBox}
  FuncsDrawThemeBackground.Add(VSCLASS_SEARCHBOX, @UxTheme_SearchBox);
  FuncsDrawThemeBackground.Add(VSCLASS_CompositedSEARCHBOX, @UxTheme_SearchBox);
  FuncsDrawThemeBackground.Add(VSCLASS_INACTIVESEARCHBOX, @UxTheme_SearchBox);
{$ENDIF}
{$IFDEF HOOK_CommandModule}
  FuncsDrawThemeBackground.Add(VSCLASS_COMMANDMODULE, @UxTheme_CommandModule);
{$ENDIF}
{$IFDEF HOOK_Menu}
  FuncsDrawThemeBackground.Add(VSCLASS_MENU, @UxTheme_Menu);
{$ENDIF}
{$IFDEF HOOK_Rebar}
  FuncsDrawThemeBackground.Add(VSCLASS_REBAR, @UxTheme_Rebar);
{$ENDIF}
{$IFDEF HOOK_Edit}
  FuncsDrawThemeBackground.Add(VSCLASS_EDIT, @UxTheme_Edit);
{$ENDIF}
{$IFDEF HOOK_ListBox}
  FuncsDrawThemeBackground.Add(VSCLASS_LISTBOX, @UxTheme_ListBox);
{$ENDIF}
{$IFDEF HOOK_Spin}
  FuncsDrawThemeBackground.Add(VSCLASS_SPIN, @UxTheme_Spin);
{$ENDIF}
{$IFDEF HOOK_ComboBox}
  FuncsDrawThemeBackground.Add(VSCLASS_COMBOBOX, @UxTheme_ComboBox);
{$ENDIF}
{$IFDEF HOOK_ListView}
  FuncsDrawThemeBackground.Add(VSCLASS_LISTVIEWPOPUP, @UxTheme_ListViewPopup);

  FuncsDrawThemeBackground.Add(VSCLASS_HEADER, @UxTheme_Header);
  FuncsDrawThemeBackground.Add(VSCLASS_ITEMSVIEW_HEADER, @UxTheme_Header);

  FuncsDrawThemeBackground.Add(VSCLASS_LISTVIEW, @UxTheme_ListView);
  FuncsDrawThemeBackground.Add(VSCLASS_ITEMSVIEW, @UxTheme_ListView);
  FuncsDrawThemeBackground.Add(VSCLASS_ITEMSVIEW_LISTVIEW, @UxTheme_ListView);
  FuncsDrawThemeBackground.Add(VSCLASS_EXPLORER_LISTVIEW, @UxTheme_ListView);
{$ENDIF}
{$IFDEF HOOK_DateTimePicker}
  FuncsDrawThemeBackground.Add(VSCLASS_DATEPICKER, @UxTheme_DatePicker);
  FuncsDrawThemeBackground.Add(VSCLASS_MONTHCAL, @UxTheme_MonthCal);
{$ENDIF}
{$IFDEF HOOK_Scrollbar}
  FuncsDrawThemeBackground.Add(VSCLASS_SCROLLBAR, @UxTheme_ScrollBar);
{$ENDIF}
{$IFDEF HOOK_Progressbar}
  FuncsDrawThemeBackground.Add(VSCLASS_PROGRESS, @UxTheme_ProgressBar);
  FuncsDrawThemeBackground.Add(VSCLASS_PROGRESS_INDERTERMINATE, @UxTheme_ProgressBar);
{$ENDIF}
{$IFDEF HOOK_TaskDialog}
  FuncsDrawThemeBackground.Add(VSCLASS_TASKDIALOG, @UxTheme_TaskDialog);
{$ENDIF}
{$IFDEF HOOK_Button}
  FuncsDrawThemeBackground.Add(VSCLASS_BUTTON, @UxTheme_Button);
  FuncsDrawThemeBackground.Add('Button-OK;Button', @UxTheme_Button);
  FuncsDrawThemeBackground.Add('Button-CANCEL;Button', @UxTheme_Button);
{$ENDIF}
{$IFDEF HOOK_TreeView}
  FuncsDrawThemeBackground.Add(VSCLASS_TREEVIEW, @UxTheme_TreeView);
{$ENDIF}
{$IFDEF HOOK_Navigation}
  if TOSVersion.Check(6, 2) then // Windows 8, 10...
  begin
    FuncsDrawThemeBackground.Add(VSCLASS_NAVIGATION, @UxTheme_Navigation);
    FuncsDrawThemeBackground.Add(VSCLASS_COMMONITEMSDIALOG, @UxTheme_CommonItemsDialog);
  end;
{$ENDIF}
  @Trampoline_UxTheme_OpenThemeData := InterceptCreate(themelib, 'OpenThemeData', @Detour_UxTheme_OpenThemeData);
{$IF CompilerVersion >= 30}
  if TOSVersion.Check(10) then
  begin
    @Trampoline_UxTheme_OpenThemeDataForDPI := InterceptCreate(themelib, 'OpenThemeDataForDpi', @Detour_UxTheme_OpenThemeDataForDPI);
    if (@Trampoline_UxTheme_OpenThemeDataForDPI = nil) and (TOSVersion.Build < 15063) then // W10 Creators Update?
      @Trampoline_UxTheme_OpenThemeDataForDPI := InterceptCreateOrdinal(themelib, 129, @Detour_UxTheme_OpenThemeDataForDPI);
  end;
{$IFEND}
  @Trampoline_UxTheme_OpenThemeDataEx := InterceptCreate(themelib, 'OpenThemeDataEx', @Detour_UxTheme_OpenThemeDataEx);
  @Trampoline_UxTheme_DrawThemeBackground := InterceptCreate(themelib, 'DrawThemeBackground', @Detour_UxTheme_DrawThemeBackground);
  @Trampoline_UxTheme_DrawThemeBackgroundEx := InterceptCreate(themelib, 'DrawThemeBackgroundEx', @Detour_UxTheme_DrawThemeBackgroundEx);
  @Trampoline_UxTheme_DrawThemeEdge := InterceptCreate(themelib, 'DrawThemeEdge', @Detour_UxTheme_DrawThemeEdge);

  @Trampoline_UxTheme_DrawThemeText := InterceptCreate(themelib, 'DrawThemeText', @Detour_UxTheme_DrawThemeText);
  @Trampoline_UxTheme_DrawThemeTextEx := InterceptCreate(themelib, 'DrawThemeTextEx', @Detour_UxTheme_DrawThemeTextEx);
  @Trampoline_UxTheme_GetThemeSysColor := InterceptCreate(themelib, 'GetThemeSysColor', @Detour_UxTheme_GetThemeSysColor);
  @Trampoline_UxTheme_GetThemeSysColorBrush := InterceptCreate(themelib, 'GetThemeSysColorBrush', @Detour_UxTheme_GetThemeSysColorBrush);
  @Trampoline_UxTheme_GetThemeColor := InterceptCreate(themelib, 'GetThemeColor', @Detour_UxTheme_GetThemeColor);
end;

finalization

InterceptRemove(@Trampoline_UxTheme_GetThemeSysColor);
InterceptRemove(@Trampoline_UxTheme_GetThemeSysColorBrush);
InterceptRemove(@Trampoline_UxTheme_OpenThemeData);
{$IF CompilerVersion >= 30}
if TOSVersion.Check(10) then
  InterceptRemove(@Trampoline_UxTheme_OpenThemeDataForDPI);
{$IFEND}
InterceptRemove(@Trampoline_UxTheme_OpenThemeDataEx);
InterceptRemove(@Trampoline_UxTheme_GetThemeColor);
InterceptRemove(@Trampoline_UxTheme_DrawThemeBackground);
InterceptRemove(@Trampoline_UxTheme_DrawThemeText);
InterceptRemove(@Trampoline_UxTheme_DrawThemeTextEx);
InterceptRemove(@Trampoline_UxTheme_DrawThemeBackgroundEx);
InterceptRemove(@Trampoline_UxTheme_DrawThemeEdge);

THThemesClasses.Free;
THThemesHWND.Free;
FuncsDrawThemeBackground.Free;

VCLStylesLock.Free;
VCLStylesLock := nil;

end.
