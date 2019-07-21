// **************************************************************************************************
//
// Unit Vcl.Styles.Utils.Menus
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
// Contributors :
//
// gandf https://github.com/gandf
//
// **************************************************************************************************
unit Vcl.Styles.Utils.Menus;

interface

{$DEFINE UseVCLStyleUtilsMenu}
// {$IF CompilerVersion >= 27}    // uncomment these lines if you want to use the VCL Styles Menus Hooks
// {$UNDEF UseVCLStyleUtilsMenu}  // included on XE6-XE8  (Embarcadero Version)
// {$IFEND}                       //

uses
  System.Classes,
  System.Types,
  System.SysUtils,
  System.Math,
  Winapi.Windows,
  Winapi.Messages,
  Winapi.UxTheme,
  Vcl.Themes,
  Vcl.Graphics,
  Vcl.ImgList,
  Vcl.GraphUtil,
  Vcl.Controls,
  Vcl.Menus,
  Vcl.Styles.Utils.SysStyleHook;

const
  { The Undocumented Messages }
  MN_SETHMENU = $01E0;
  MN_GETHMENU = $01E1;
  MN_SIZEWINDOW = $01E2;
  MN_OPENHIERARCHY = $01E3;
  MN_CLOSEHIERARCHY = $01E4;
  MN_SELECTITEM = $01E5;
  MN_CANCELMENUS = $01E6;
  MN_SELECTFIRSTVALIDITEM = $01E7;
  MN_GETPPOPUPMENU = $01EA;
  MN_FINDMENUWINDOWFROMPOINT = $01EB;
  MN_SHOWPOPUPWINDOW = $01EC;
  MN_BUTTONDOWN = $01ED;
  MN_MOUSEMOVE = $01EE;
  MN_BUTTONUP = $01EF;
  MN_SETTIMERTOOPENHIERARCHY = $01F0;
  MN_DBLCLK = $001F1;
  MN_BUTTONDOWN_UP = $FFFFFFFC;
  MN_BUTTONDOWN_DOWN = $FFFFFFFD;
  WM_UAHDESTROYWINDOW = $0090;
  WM_UAHDRAWMENU = $0091;
  WM_UAHDRAWMENUITEM = $0092;
  WM_UAHINITMENU = $0093;
  WM_UAHMEASUREMENUITEM = $0094;
  WM_UAHNCPAINTMENUPOPUP = $0095;

  { MARLETT Font Char Const }
  MARLETT_RESTORE_CHAR = Char(50);
  MARLETT_MINIMIZE_CHAR = Char(48);
  MARLETT_CLOSE_CHAR = Char(114);
  MARLETT_MAXIMIZE_CHAR = Char(49);
  MARLETT_LEFT_ARROW_SCROLL_CHAR = Char(51);
  MARLETT_RIGHT_ARROW_SCROLL_CHAR = Char(52);
  MARLETT_UP_ARROW_SCROLL_CHAR = Char(53);
  MARLETT_DOWN_ARROW_SCROLL_CHAR = Char(54);
  MARLETT_PAGE_DOWN_ARROW_CHAR = Char(55);
  MARLETT_RIGHT_ARROW_SUBMENU_CHAR = Char(56);
  MARLETT_UP_ARROW_CHAR = Char(116);
  MARLETT_DOWN_ARROW_CHAR = Char(117);

type
  TSysPopupStyleHook = class;
  TSysPopupItemState = set of (isHot, isDisabled, isChecked, isDefault);
  TSysPopupItemStyle = (isNormal, isSep, isDropDown);

  TSysPopupStyleHook = class(TSysStyleHook)
  private type
{$REGION 'TSysPopupItem'}
    TSysPopupItem = class
    private
      FIndex: integer;
      FMenu: HMENU;
      FHandle: HWND;
      FSysParent: TSysControl;
      FSysPopupStyleHook: TSysPopupStyleHook;
      function GetItemRect: TRect;
      function IsItemDisabled: Boolean;
      function IsItemContainsSubMenu: Boolean;
      function IsItemSeparator: Boolean;
      function IsItemChecked: Boolean;
      function IsItemDefault: Boolean;
      function GetItemText: String;
      function GetVCLMenuItems: TMenuItem;
      function GetVCLMenuItemsFast: TMenuItem;
      function GetItemBitmap: HBITMAP;
      function IsItemRadioCheck: Boolean;
      // function isItemVisible: Boolean;
      function IsItemOwnerDraw: Boolean;
      function GetItemID: WORD;
      function GetVCLRealItem: TMenuItem;
    public
      constructor Create(SysPopupStyleHook: TSysPopupStyleHook; SysParent: TSysControl; const Index: integer; const Menu: HMENU); virtual;
      Destructor Destroy; override;
      property ID: WORD read GetItemID;
      property ItemRect: TRect read GetItemRect;
      property Disabled: Boolean read IsItemDisabled;
      property Separator: Boolean read IsItemSeparator;
      property HasSubMenu: Boolean read IsItemContainsSubMenu;
      property Checked: Boolean read IsItemChecked;
      // property Visible: Boolean read isItemVisible;
      property RadioCheck: Boolean read IsItemRadioCheck;
      property DefaultItem: Boolean read IsItemDefault;
      property Text: String read GetItemText;
      property OwnerDraw: Boolean read IsItemOwnerDraw;
      property VCLMenuItems: TMenuItem read GetVCLMenuItemsFast;
      property VCLItem: TMenuItem read GetVCLRealItem;
      property Bitmap: HBITMAP read GetItemBitmap;
    end;
{$ENDREGION}

  var
    FOffset : Integer;
    FOffsetCache : Integer;
    FSeparatorHeightCache : Integer;
    FItemHeightCache : Integer;
    FItemsPainted: Boolean;
    FParentSubItemPainted: Boolean;
    FPreviousHotItemIndex: integer;
    FPaintFirstItemFromMenu: Boolean;
    FKeyIndex: integer;
    FSysPopupItem: TSysPopupItem;
    FCount: integer;
    FMenu: HMENU;
    FVCLMenuItems: TMenuItem;
    FNCRect : TRect;
    FEnterWithKeyboard : Boolean;
    FPersistentHotKeys : Boolean;

    FMenuBarHook : TObject;
    function GetMenuFromHandle(AHandle: HWND): HMENU;
    function GetItemsCount: integer;
    procedure MNSELECTITEM(var Message: TMessage); message MN_SELECTITEM;
    procedure WMPRINT(var Message: TMessage); message WM_PRINT;
    function GetSysPopupItem(Index: integer): TSysPopupItem;
    function GetRightToLeft: Boolean;
  protected
    procedure EraseItem(Canvas: TCanvas; const Index: integer; const ItemRect: TRect); virtual;
    procedure DoDrawItem(Canvas: TCanvas; const Index: integer; const ForceIsNotHot: Boolean = False);
    procedure DrawItem(Canvas: TCanvas; const Index: integer; const ItemRect: TRect; const ItemText: String; const State: TSysPopupItemState;
      const Style: TSysPopupItemStyle); Virtual;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure GetNbSeparator(var PNbSeparator: Integer; const PIndex: Integer);
    function GetMenuItemHeight(const Index: Integer) : Integer;
    function GetOffset(UseCache: Boolean) : Integer;
    function ItemIsVisible(const Index: Integer): Boolean;
    procedure SetMaxOffset();
    procedure RefreshMenu();
    procedure GetItemHeight();
    procedure GetSeparatorHeight();
    function GetClientRectHeight(var PValue: TRect): Integer;
    function GetItemClicked(var PButton: Byte; PInitPos: TPoint) : Integer;
    function GetMousePos() : TPoint;
    procedure WndProc(var Message: TMessage); override;
    procedure UpdateColors; override;
    procedure SetOffset(PValue: Integer);
    function GetBottom(const Index: Integer) : Integer;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
    property Menu: HMENU read FMenu;
    property Items[Index: integer]: TSysPopupItem read GetSysPopupItem;
    property Count: integer read FCount;
    property RightToLeft: Boolean read GetRightToLeft;
    property Offset: Integer read FOffset write SetOffset;
  end;

implementation

{
  RANGE CHECKS OFF .
  IMPLICIT_STRING_CAST_LOSS OFF .
}
{$R-,WARN IMPLICIT_STRING_CAST_LOSS OFF}

uses
  Vcl.Forms,
  Vcl.Styles.Utils.Misc,
  Vcl.Styles.Utils.SysControls,
  Vcl.Styles.Utils.Graphics;

type
  TControlClass = Class(TControl);

function GetBmpInfo(hBmp: HBITMAP): Bitmap;
begin
  ZeroMemory(@Result, sizeof(Bitmap));
  GetObject(hBmp, sizeof(Result), @Result);
end;

function GetBitmapHeight(hBmp: HBITMAP): integer;
begin
  Result := GetBmpInfo(hBmp).bmHeight;
end;

function GetBitmapWidth(hBmp: HBITMAP): integer;
begin
  Result := GetBmpInfo(hBmp).bmWidth;
end;

function BmpToIcon(hBmp: HBITMAP): HICON;
var
  Bmp: Bitmap;
  hbmMask: HBITMAP;
  DC: HDC;
  piconinfo: TIconInfo;
  Icon: HICON;
begin
  Icon := 0;
  FillChar(Bmp, sizeof(Bitmap), Char(0));
  if GetObject(hBmp, sizeof(Bitmap), @Bmp) > 0 then
  begin
    DC := GetDC(0);
    if DC <> 0 then
      try
        hbmMask := CreateCompatibleBitmap(DC, Bmp.bmWidth, Bmp.bmHeight);
        if hbmMask <> 0 then
          try
            ZeroMemory(@piconinfo, sizeof(piconinfo));
            piconinfo.fIcon := True;
            piconinfo.hbmColor := hBmp;
            piconinfo.hbmMask := hbmMask;
            Icon := CreateIconIndirect(piconinfo);
          finally
            DeleteObject(hbmMask);
          end;
      finally
        ReleaseDC(0, DC);
      end;
  end;
  Result := Icon;
end;

function GetMenuItemPos(Menu: HMENU; ID: integer): integer;
var
  i: integer;
  pMenuItemInfo: TMenuItemInfo;
begin
  Result := -1;
  if Menu = 0 then
    Exit;
  for i := 0 to GetMenuItemCount(Menu) do
  begin
    FillChar(pMenuItemInfo, sizeof(pMenuItemInfo), Char(0));
    pMenuItemInfo.cbSize := sizeof(pMenuItemInfo);
    pMenuItemInfo.fMask := MIIM_ID;
    if (GetMenuItemInfo(Menu, i, True, pMenuItemInfo)) then
      if pMenuItemInfo.wID = Cardinal(ID) then
        Exit(i);
  end;
end;

function IsItemHILITE(Menu: HMENU; const ItemIndex: integer): Boolean;
var
  pMenuItemInfo: TMenuItemInfo;
begin
  Result := False;
  FillChar(pMenuItemInfo, sizeof(pMenuItemInfo), Char(0));
  pMenuItemInfo.cbSize := sizeof(TMenuItemInfo);
  pMenuItemInfo.fMask := MIIM_STATE;
  if GetMenuItemInfo(Menu, ItemIndex, True, pMenuItemInfo) then
    Result := (pMenuItemInfo.fState and MFS_HILITE) = MFS_HILITE;
end;

{ TSysPopupStyleHook }
constructor TSysPopupStyleHook.Create(AHandle: THandle);
begin
  inherited;
{$IF CompilerVersion > 23}
  StyleElements := [seFont, seClient, seBorder];
{$ELSE}
  OverridePaint := True;
  OverridePaintNC := True;
  OverrideFont := True;
{$IFEND}
  FPreviousHotItemIndex := -1;
  FKeyIndex := -1;
  FItemsPainted := False;
  FSysPopupItem := nil;
  FVCLMenuItems := nil;
  Offset := 0;
  FSeparatorHeightCache := 1;
  FItemHeightCache := 1;
  FEnterWithKeyboard  := False;
  FPersistentHotKeys  := False;
  FMenuBarHook := nil;
  // Font := Screen.MenuFont;
end;

destructor TSysPopupStyleHook.Destroy;
begin
  if Assigned(FSysPopupItem) then
    FreeAndNil(FSysPopupItem);
  inherited;
end;

procedure TSysPopupStyleHook.DoDrawItem(Canvas: TCanvas; const Index: integer; const ForceIsNotHot: Boolean = False);
var
  LRect, LRect2, LItemRect: TRect;
  P, P2: TPoint;
  State: TSysPopupItemState;
  Style: TSysPopupItemStyle;
  LText: String;
  SaveIndex: integer;
  LSysPopupItem: TSysPopupItem;
  ClientRectHeight: Integer;
begin
  if (Index < 0) or (Index > Count - 1) then
    Exit;

  LSysPopupItem := Items[Index];
  LItemRect := LSysPopupItem.ItemRect;
  P2 := Point(LItemRect.Left, LItemRect.Bottom);
  ScreenToClient(Handle, P2);
  P2.Y := P2.Y - GetOffset(True); //add offset

  GetMenuItemRect(0, FMenu, Index, LRect);
 //OutputDebugString(PChar(Format('Index %d  Width %d Height %d Left %d Top %d', [Index, LRect.Width, LRect.Height, LRect.Left, LRect.Top])));
  ClientRectHeight := GetClientRectHeight(LRect2);
  LRect2.Height := ClientRectHeight;

  //prevent draw  not visible items on larger menus
  if not PtInRect (LRect2, P2) then
    Exit;

  P := Point(LItemRect.Left, LItemRect.Top);
  ScreenToClient(Handle, P);
  P2 := P;
  P2.Y := P2.Y - GetOffset(True); //add offset

  if ClientRectHeight>LRect.Height then
    LRect2.Height:=  ClientRectHeight - LRect.Height;

  //prevent draw  not visible items on larger menus
  if not PtInRect (LRect2, P2) then
    Exit;

  LItemRect := Rect(P.X, P.Y, P.X + LItemRect.Width, P.Y + LItemRect.Height);


  if LItemRect.Left < 2 then
    LItemRect.Left := 2;
  inc(LItemRect.Right, 4);
  if LItemRect.Top < 2 then
    inc(LItemRect.Top, 2);
  { Item State }
  State := [];
  if not ForceIsNotHot then
    if index <> FPreviousHotItemIndex then
      Include(State, isHot);
  if LSysPopupItem.Disabled then
    Include(State, isDisabled);
  if LSysPopupItem.Checked then
    Include(State, isChecked);
  if LSysPopupItem.DefaultItem then
    Include(State, isDefault);
  { Item Style }
  Style := isNormal;
  if LSysPopupItem.Separator then
    Style := isSep;
  if LSysPopupItem.HasSubMenu then
    Style := isDropDown;

  LText := '';
  if Style <> isSep then
    LText := LSysPopupItem.Text;

  SaveIndex := SaveDC(Canvas.Handle);
  try
    EraseItem(Canvas, Index, LItemRect);
  finally
    RestoreDC(Canvas.Handle, SaveIndex)
  end;

  SaveIndex := SaveDC(Canvas.Handle);
  try
    DrawItem(Canvas, Index, LItemRect, LText, State, Style);
  finally
    RestoreDC(Canvas.Handle, SaveIndex)
  end;
end;

procedure TSysPopupStyleHook.DrawItem(Canvas: TCanvas; const Index: integer; const ItemRect: TRect; const ItemText: String; const State: TSysPopupItemState;
  const Style: TSysPopupItemStyle);
var
  LTextRect: TRect;
  DC: HDC;

  procedure DrawSubMenu(const ItemRect: TRect);
  var
    LBitmap: TBitmap;
    LSubMenuDetails: TThemedElementDetails;
    LSubMenuDetail: TThemedMenu;
    SubMenuSize: TSize;
    LSubMenuRect: TRect;
  begin
    LSubMenuRect := Rect(0, 0, 0, 0);
    LSubMenuDetail := tmPopupSubMenuNormal;
    if isDisabled in State then
      LSubMenuDetail := tmPopupSubMenuDisabled;
    LSubMenuDetails := StyleServices.GetElementDetails(LSubMenuDetail);
    StyleServices.GetElementSize(DC, LSubMenuDetails, esActual, SubMenuSize);
    if not RightToLeft then
      LSubMenuRect := Rect(ItemRect.Right - SubMenuSize.cx, ItemRect.Top, ItemRect.Right, ItemRect.Top + SubMenuSize.cy)
    else
      LSubMenuRect := Rect(ItemRect.Left + 4, ItemRect.Top, ItemRect.Left + 4 + SubMenuSize.Width, ItemRect.Bottom);
    LBitmap := TBitmap.Create;
    try
      LBitmap.SetSize(SubMenuSize.Width, SubMenuSize.Height);
      LBitmap.Canvas.Brush.Color := clFuchsia;
      LBitmap.Canvas.FillRect(Rect(0, 0, SubMenuSize.Width, SubMenuSize.Height));
      StyleServices.DrawElement(LBitmap.Canvas.Handle, LSubMenuDetails, Rect(0, 0, SubMenuSize.Width, SubMenuSize.Height));
      if RightToLeft then
      begin
        RotateBitmap(LBitmap, DegToRad(180), False, clFuchsia);
        inc(LSubMenuRect.Top, (LBitmap.Height div 2) - 2);
      End
      else
        Dec(LSubMenuRect.Left, 4);

      TransparentBlt(DC, LSubMenuRect.Left, LSubMenuRect.Top, SubMenuSize.Width, SubMenuSize.Height, LBitmap.Canvas.Handle, 0, 0, SubMenuSize.Width,
        SubMenuSize.Height, clFuchsia);
    finally
      LBitmap.Free;
    end;
    Dec(LTextRect.Right, LSubMenuRect.Width);
  end;

  procedure DrawSpecialChar(DC: HDC; const Sign: Char; DestRect: TRect; const Bold: Boolean = False; const Disabled: Boolean = False);
  var
    LogFont: TLogFont;
    pOldFont: HGDIOBJ;
    AFont: HFONT;
    oldColor: COLORREF;
    OldMode: integer;
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
    LogFont.lfQuality := DEFAULT_QUALITY;
    LogFont.lfPitchAndFamily := DEFAULT_PITCH;
    LogFont.lfFaceName := 'Marlett';

    if Disabled then
      oldColor := ColorToRGB(StyleServices.GetStyleFontColor(sfPopupMenuItemTextDisabled))
    else
    begin
      oldColor := ColorToRGB(StyleServices.GetStyleFontColor(sfPopupMenuItemTextNormal));
      if isHot in State then
        oldColor := ColorToRGB(StyleServices.GetStyleFontColor(sfPopupMenuItemTextHot));
      if isDisabled in State then
        oldColor := ColorToRGB(StyleServices.GetStyleFontColor(sfPopupMenuItemTextDisabled));
    end;

    AFont := CreateFontIndirect(LogFont);
    if AFont <> 0 then
      try
        oldColor := SetTextColor(DC, oldColor);
        pOldFont := SelectObject(DC, AFont);
        try
          OldMode := SetBkMode(DC, Transparent);
          Winapi.Windows.DrawText(DC, Sign, 1, DestRect, DT_LEFT or DT_SINGLELINE);
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

var
  LThemedMenu: TThemedMenu;
  LDetails, LDetailsBar: TThemedElementDetails;
  LTextFormat: TTextFormat;
  LSize: TSize;
  LMenuItem: TMenuItem;
  LOwnerDrawState: TOwnerDrawState;
  P, LImageWidth, ImageIndex: integer;
  LImageRect, R: TRect;
  hBmp: HBITMAP;
  BmpHeight, BmpWidth: integer;
  Icon: HICON;
  DisplayCheckedGlyph: Boolean;
  Sign: Char;
  LSysPopupItem: TSysPopupItem;
  sShortCut: String;
  LBitmap: TBitmap;
  LParentMenu: TMenu;
  LBitmapBar: TBitmap;
  ItemRect2: TRect;


begin
  DisplayCheckedGlyph := True;
  ItemRect2 := ItemRect;
  ItemRect2.Top := ItemRect.Top - GetOffset(True); //add offset
  ItemRect2.Height := ItemRect.Height;
  LTextRect := ItemRect2;
  { Fast access . }
  LSysPopupItem := Items[Index]; // Do not destroy !!
  DC := Canvas.Handle;
  R := ItemRect2;
  LThemedMenu := tmPopupItemNormal;
  if isHot in State then
    LThemedMenu := tmPopupItemHot;
  if isDisabled in State then
    LThemedMenu := tmPopupItemDisabled;
  if Style = isSep then
  begin
    LThemedMenu := tmPopupSeparator;
    inc(R.Left, 25);
  end;

  LDetails := StyleServices.GetElementDetails(LThemedMenu);

  if (isHot in State) and (LThemedMenu = tmPopupItemDisabled) then
  begin
    LDetails := StyleServices.GetElementDetails(tmPopupItemHot);
    LBitmap := TBitmap.Create;
    LBitmap.SetSize(R.Width, R.Height);
    LBitmap.PixelFormat := pf32bit;
    try
      Bitmap32_SetAlphaAndColor(LBitmap, 0, 0);
      StyleServices.DrawElement(LBitmap.Canvas.Handle, LDetails, Rect(0, 0, R.Width, R.Height));
      _Darkness32(LBitmap, 30);
      BitBlt(DC, R.Left, R.Top, R.Width, R.Height, LBitmap.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      LBitmap.Free;
    end;
  end
  else
  if (LThemedMenu <> tmPopupItemNormal) and (LThemedMenu <> tmPopupItemDisabled) then
    StyleServices.DrawElement(DC, LDetails, R);

  if Style = isDropDown then
    DrawSubMenu(ItemRect2);

  LImageWidth := 0;
  LMenuItem := LSysPopupItem.VCLMenuItems;
  if LMenuItem <> nil then
    LMenuItem := LSysPopupItem.VCLItem;

  LParentMenu := nil;
  if (LMenuItem <> nil) then
    LParentMenu := LMenuItem.GetParentMenu;
  if (LParentMenu <> nil) and (LParentMenu.OwnerDraw) and (@LMenuItem.OnDrawItem <> nil) then
  begin
    LMenuItem.OnDrawItem(LMenuItem, Canvas, ItemRect2, (isHot in State));
    Exit;
  end;


  if (LParentMenu <> nil) and (LParentMenu.OwnerDraw) and (LMenuItem <> nil) and (@LMenuItem.OnAdvancedDrawItem <> nil) then
  begin
    LOwnerDrawState := [];

    if isHot in State then
      Include(LOwnerDrawState, odSelected);
    if isDisabled in State then
      Include(LOwnerDrawState, odDisabled);
    if isChecked in State then
      Include(LOwnerDrawState, odChecked);
    if isDefault in State then
      Include(LOwnerDrawState, odDefault);

    LMenuItem.OnAdvancedDrawItem(LMenuItem, Canvas, ItemRect2, LOwnerDrawState);
    Exit;
  end;

  if LMenuItem <> nil then
  begin
    { Draw Vcl PopupMenu Bitmap }
    ImageIndex := LMenuItem.ImageIndex;

      if (ImageIndex < 0) and (LMenuItem.Bitmap <> nil) then
      begin
        LBitmap := LMenuItem.Bitmap;
        if (LBitmap.Width = 16) and (LBitmap.Height = 16) then
        begin
          LImageWidth := LBitmap.Width;
          LImageRect := Rect(0, 0, LBitmap.Width, LBitmap.Height);
          RectVCenter(LImageRect, ItemRect2);

          if not RightToLeft then
            OffsetRect(LImageRect, 4, 0)
          else
          begin
            LImageRect.Left := ItemRect2.Right - LBitmap.Width - 4;
            LImageRect.Right := ItemRect2.Right;
          end;

          Canvas.Draw(LImageRect.Left, LImageRect.Top, LBitmap)
        end
        else
        if (LBitmap.Width > 0) and (LBitmap.Height > 0) then
        begin
          LImageWidth := 16;
          LImageRect := Rect(0, 0, 16, 16);
          RectVCenter(LImageRect, ItemRect2);
          if not RightToLeft then
            OffsetRect(LImageRect, 4, 0)
          else
          begin
            LImageRect.Left := ItemRect2.Right - 16 - 4;
            LImageRect.Right := ItemRect2.Right;
          end;

          if (LSysPopupItem.Checked) and (not LSysPopupItem.RadioCheck)  then
          begin
           R:=LImageRect;
           InflateRect(R, 2, 2);
           Canvas.Brush.Style:=bsClear;
           Canvas.Pen.Color  :=StyleServices.GetSystemColor(clHotLight);
           Canvas.Rectangle(R);
          end;

          Canvas.StretchDraw(LImageRect, LBitmap);
        end;

      end
      else
      if (LMenuItem.Parent <> nil) and (LMenuItem.Parent.SubMenuImages <> nil) and (ImageIndex > -1) then
      begin
        LImageWidth := LMenuItem.Parent.SubMenuImages.Width;
        DisplayCheckedGlyph := False;
        LImageRect := Rect(0, 0, LMenuItem.Parent.SubMenuImages.Width, LMenuItem.Parent.SubMenuImages.Height);
        RectVCenter(LImageRect, ItemRect2);

        if not RightToLeft then
          OffsetRect(LImageRect, 4, 0)
        else
        begin
          LImageRect.Left := ItemRect2.Right - LMenuItem.Parent.SubMenuImages.Width - 4;
          LImageRect.Right := ItemRect2.Right;
        end;

        if (LSysPopupItem.Checked) and (not LSysPopupItem.RadioCheck)  then
        begin
         R:=LImageRect;
         InflateRect(R, 2, 2);
         Canvas.Brush.Style:=bsClear;
         Canvas.Pen.Color  :=StyleServices.GetSystemColor(clHotLight);
         Canvas.Rectangle(R);
        end;

        LMenuItem.Parent.SubMenuImages.Draw(Canvas, LImageRect.Left, LImageRect.Top, ImageIndex);
      end
      else
      if (LParentMenu.Images <> nil) and (ImageIndex > -1) then
      begin
        LImageWidth := LParentMenu.Images.Width;
        DisplayCheckedGlyph := False;
        LImageRect := Rect(0, 0, LParentMenu.Images.Width, LParentMenu.Images.Height);
        RectVCenter(LImageRect, ItemRect2);

        if not RightToLeft then
          OffsetRect(LImageRect, 4, 0)
        else
        begin
          LImageRect.Left := ItemRect2.Right - LParentMenu.Images.Width - 4;
          LImageRect.Right := ItemRect2.Right;
        end;

        if (LSysPopupItem.Checked) and (not LSysPopupItem.RadioCheck)  then
        begin
         R:=LImageRect;
         InflateRect(R, 2, 2);
         Canvas.Brush.Style:=bsClear;
         Canvas.Pen.Color  :=StyleServices.GetSystemColor(clHotLight);
         Canvas.Rectangle(R);
        end;

        LParentMenu.Images.Draw(Canvas, LImageRect.Left, LImageRect.Top, ImageIndex, not LSysPopupItem.Disabled);
      end;
  end
  else if LSysPopupItem.Bitmap > 0 then
  begin
    hBmp := LSysPopupItem.Bitmap;
    if hBmp < HBMMENU_POPUP_MINIMIZE + 1 then
    begin
      { Draw System PopupMenu Bitmap }
      DisplayCheckedGlyph := False;

      case hBmp of
        HBMMENU_POPUP_RESTORE:
          Sign := MARLETT_RESTORE_CHAR;
        HBMMENU_POPUP_MINIMIZE, HBMMENU_MBAR_MINIMIZE_D:
          Sign := MARLETT_MINIMIZE_CHAR;
        HBMMENU_POPUP_MAXIMIZE:
          Sign := MARLETT_MAXIMIZE_CHAR;
        HBMMENU_POPUP_CLOSE, HBMMENU_MBAR_CLOSE_D:
          Sign := MARLETT_CLOSE_CHAR;
      else
        Sign := Char(0);
      end;
      if Sign <> #0 then
      begin
        LImageRect := Rect(0, 0, 10, 10);
        R := Rect(ItemRect2.Left, ItemRect2.Top, ItemRect2.Left + 30, ItemRect2.Bottom);
        RectCenter(LImageRect, ItemRect2);
        if not RightToLeft then
          LImageRect.Left := ItemRect2.Left + 6 //Fix center image
        else
        begin
          LImageRect.Left := ItemRect2.Right - 10 - 4;
          LImageRect.Right := ItemRect2.Right;
        end;
        DrawSpecialChar(DC, Sign, LImageRect, False, (isDisabled in State));
      end;
    end
    else
    begin
      { Draw PopupMenu Bitmap }
      BmpWidth := GetBitmapWidth(hBmp);
      BmpHeight := GetBitmapHeight(hBmp);
      if (BmpWidth > 0) and (BmpHeight > 0) then
      begin
        DisplayCheckedGlyph := False;
        LImageRect := Rect(0, 0, BmpWidth, BmpHeight);
        RectVCenter(LImageRect, ItemRect2);
        if not RightToLeft then
          OffsetRect(LImageRect, 4, 0)
        else
        begin
          LImageRect.Left := ItemRect2.Right - BmpWidth - 4;
          LImageRect.Right := ItemRect2.Right;
        end;

        Icon := BmpToIcon(hBmp);
        if Icon <> 0 then
        begin

          if (LSysPopupItem.Checked) and (not LSysPopupItem.RadioCheck)  then
          begin
           R:=LImageRect;
           InflateRect(R, 2, 2);
           Canvas.Brush.Style:=bsClear;
           Canvas.Pen.Color  :=StyleServices.GetSystemColor(clHotLight);
           Canvas.Rectangle(R);
          end;

          DrawIconEX(DC, LImageRect.Left, LImageRect.Top, Icon, BmpWidth, BmpHeight, 0, 0, DI_NORMAL);
          DeleteObject(Icon);
        end;
      end;
    end;
  end;

  if (LSysPopupItem.Checked)  then
  begin
    LThemedMenu := TThemedMenu(integer(tmPopupCheckNormal) + integer(LSysPopupItem.Disabled));
    if LSysPopupItem.RadioCheck then
      LThemedMenu := TThemedMenu(integer(tmPopupBulletNormal) + integer(LSysPopupItem.Disabled));
    LDetails := StyleServices.GetElementDetails(LThemedMenu);
    StyleServices.GetElementSize(DC, LDetails, esActual, LSize);
    LImageRect := Rect(0, 0, LSize.Width, LSize.Height);

    RectVCenter(LImageRect, ItemRect2);
    if DisplayCheckedGlyph then
    begin
      if not RightToLeft then
        OffsetRect(LImageRect, 4, 0)
      else
      begin
        LImageRect.Left := ItemRect2.Right - LSize.Width - 4;
        LImageRect.Right := ItemRect2.Right;
      end;
      StyleServices.DrawElement(DC, LDetails, LImageRect);
    end;
  end;

  { Draw Text }
  LTextFormat := [tfLeft, tfVerticalCenter, tfSingleLine, tfExpandTabs];//, tfHidePrefix];

//  if (LMenuItem.Parent<>nil) then
//   OutputDebugString(PChar(Format('LMenuItem.Parent %s IsItemHILITE %s', [LMenuItem.Parent.Caption, BoolToStr(IsItemHILITE(LMenuItem.Parent.Handle, LMenuItem.Parent.MenuIndex), True)])));

//  if FEnterWithKeyboard then
//    Exclude(LTextFormat, tfHidePrefix);

  if not RightToLeft then
    inc(LTextRect.Left, 28)
  else
  begin
    LTextRect.Left  := ItemRect2.Left;
    LTextRect.Right := ItemRect2.Right - 28;
    Exclude(LTextFormat, tfLeft);
    Include(LTextFormat, tfRtlReading);
    Include(LTextFormat, tfRight);
  end;

  if LImageWidth > 0 then
  begin
    if not RightToLeft then
      LTextRect.Left := ItemRect2.Left + LImageWidth + 8 + 4
    else
    begin
      LTextRect.Left := ItemRect2.Left;
      LTextRect.Right := ItemRect2.Right - LImageWidth - 8;
    end;
  end;

  LDetails := StyleServices.GetElementDetails(tmPopupItemNormal);
  if isHot in State then
    LDetails := StyleServices.GetElementDetails(tmPopupItemHot);
  if isDisabled in State then
    LDetails := StyleServices.GetElementDetails(tmPopupItemDisabled);

  if LSysPopupItem.DefaultItem then
    Canvas.Font.Style := [fsBold];

  if LMenuItem <> nil then
    DrawText(Canvas.Handle, LDetails, ItemText, LTextRect, LTextFormat)
  else
  begin
    sShortCut := '';
    // http://msdn.microsoft.com/en-us/library/ms647553%28v=VS.85%29.aspx#_win32_Menu_Shortcut_Keys
    P := Pos(#9, ItemText);
    if P > 1 then
    begin
      sShortCut := Copy(ItemText, P + 1, length(ItemText) - P);
      DrawText(Canvas.Handle, LDetails, Copy(ItemText, 1, P), LTextRect, LTextFormat)
    end
    else
      DrawText(Canvas.Handle, LDetails, ItemText, LTextRect, LTextFormat)
  end;

  {Draw vertical menu bar}
//  LDetailsBar := StyleServices.GetElementDetails(tmPopupSeparator);
//  LBitmapBar := TBitmap.Create;
//  LBitmapBar.SetSize(LTextRect.Height, LTextRect.Height);
//  LBitmapBar.PixelFormat := pf32bit;
//  LBitmapBar.AlphaFormat := afDefined;
//  LImageRect := Rect(0, 0, LBitmapBar.Width, LBitmapBar.Height);
//  R:=LImageRect;
//  try
//    StyleServices.DrawElement(LBitmapBar.Canvas.Handle, LDetailsBar, Rect(0, 0, R.Width, R.Height));
//    RotateBitmap(LBitmapBar, DegToRad(90), true);
//    BitBlt(DC, LTextRect.Left - 6, LTextRect.Top, 1, LTextRect.Height, LBitmapBar.Canvas.Handle, round(LTextRect.Height / 2), 0, SRCCOPY);
//  finally
//    LBitmapBar.Free;
//  end;

  { Draw ShortCut Text . }
  if LMenuItem <> nil then
  begin
    if LMenuItem.ShortCut <> 0 then
    begin
      sShortCut := ShortCutToText(LMenuItem.ShortCut);
      LTextRect := ItemRect2;
      if RightToLeft then
      begin
        LTextRect.Left := ItemRect2.Left + 14;
        LTextRect.Right := LTextRect.Left + Canvas.TextWidth(sShortCut);
      end
      else
      begin
        LTextRect.Left := ItemRect2.Right - 14 - Canvas.TextWidth(sShortCut);
        LTextRect.Right := ItemRect2.Right;
      end;
      DrawText(Canvas.Handle, LDetails, sShortCut, LTextRect, LTextFormat);
    end;
  end
  else if sShortCut <> '' then
  begin
    LTextRect := ItemRect2;
    if RightToLeft then
    begin
      LTextRect.Left := ItemRect2.Left + 14;
      LTextRect.Right := LTextRect.Left + Canvas.TextWidth(sShortCut);
    end
    else
    begin
      LTextRect.Left := ItemRect2.Right - 14 - Canvas.TextWidth(sShortCut);
      LTextRect.Right := ItemRect2.Right;
    end;
    DrawText(Canvas.Handle, LDetails, sShortCut, LTextRect, LTextFormat);
  end;
end;

procedure TSysPopupStyleHook.EraseItem(Canvas: TCanvas; const Index: integer; const ItemRect: TRect);
var
  LBitmap: TBitmap;
  LOffset: Integer;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(SysControl.Width, SysControl.Height);
    PaintBackground(LBitmap.Canvas);
    LOffset := GetOffset(True);
    BitBlt(Canvas.Handle, ItemRect.Left, ItemRect.Top - LOffset, ItemRect.Width, ItemRect.Height, LBitmap.Canvas.Handle, ItemRect.Left, ItemRect.Top - LOffset, SRCCOPY);
  finally
    LBitmap.Free;
  end;
end;

function TSysPopupStyleHook.GetItemsCount: integer;
begin
  Result := GetMenuItemCount(FMenu);
end;

function TSysPopupStyleHook.GetMenuFromHandle(AHandle: HWND): HMENU;
begin
  Result := HMENU(SendMessage(AHandle, MN_GETHMENU, 0, 0));
end;

function TSysPopupStyleHook.GetRightToLeft: Boolean;
var
  pMenuItemInfo: TMenuItemInfo;
begin
  Result := False;
  FillChar(pMenuItemInfo, sizeof(pMenuItemInfo), Char(0));
  pMenuItemInfo.cbSize := sizeof(TMenuItemInfo);
  pMenuItemInfo.fMask := MIIM_TYPE;
  if GetMenuItemInfo(FMenu, 0, True, pMenuItemInfo) then
    Result := ((pMenuItemInfo.fType and MFT_RIGHTORDER) = MFT_RIGHTORDER) or ((pMenuItemInfo.fType and MFT_RIGHTJUSTIFY) = MFT_RIGHTJUSTIFY);
end;

function TSysPopupStyleHook.GetSysPopupItem(Index: integer): TSysPopupItem;
begin
  Result := nil;
  if (Index > -1) and (index <= Count) then
  begin
    if Assigned(FSysPopupItem) then
      FreeAndNil(FSysPopupItem);
    FSysPopupItem := TSysPopupItem.Create(Self, SysControl, Index, FMenu);
    Result := FSysPopupItem;
  end;
end;

procedure TSysPopupStyleHook.PaintBackground(Canvas: TCanvas);
begin
  StyleServices.DrawElement(Canvas.Handle, StyleServices.GetElementDetails(tmPopupBorders), SysControl.ClientRect);
end;

procedure TSysPopupStyleHook.UpdateColors;
begin
  inherited;
  Font := Screen.MenuFont;
end;

type
  TSubMenuItemInfo = record
    Menu: HMENU;
    WindowHandle: HWND;
    ItemIndex: integer;
  end;

var
  SubMenuItemInfoArray: array of TSubMenuItemInfo;

procedure TSysPopupStyleHook.MNSELECTITEM(var Message: TMessage);
var
  DC: HDC;
  Canvas: TCanvas;
  Index, Index2: integer;
  i: WORD;
  L: integer;
  ParentItem: integer;
  ParentPopup: HWND;
  LMenu: HMENU;
  ArrowHeight: Integer;
  LDetails: TThemedElementDetails;
  R, FClientRect: TRect;
  LBitmap: TBitmap;
  LButton: Byte;
  LInitPos: TPoint;
begin
  { The undocumented MN_SELECTITEM Message:
    This is the most importants message ,
    Windows sends this message every time when the user
    select an item (not clicking, only select) ...
    wParam=Current Item Index .
    lparam= may be it's unused (not sure).
  }
  //
  Handled := False;
  DC := 0;
  Canvas := TCanvas.Create;
  try
    ParentPopup := 0;
    ParentItem := -1;
    DC := GetDC(Handle);
    Canvas.Handle := DC;
    Index := integer(Message.WParam);

    //OutputDebugString(PChar(Format('MNSELECTITEM Index %d', [Index])));

    if Assigned(Font) then
      Canvas.Font := Font;
    { Out of index . }
    if (Index > FCount - 1) or (Index < 0) then
    begin
      { Make sure that wParam hold a valid Item Index .
        if not .. then mouse is not on the PopupMenu
        => Remove Item highlight .
      }
      SetRedraw(True);
      if (FPreviousHotItemIndex > -1) and (FPreviousHotItemIndex < FCount) then
        DoDrawItem(Canvas, FPreviousHotItemIndex);
      FPreviousHotItemIndex := -1;
      Handled := True;
      Exit;
    end;

    if not FItemsPainted then
    begin
      { Items are not painted completely . }
      if Index = 0 then
      begin
        { draw up/dowm button }
        if GetBottom(Count - 1) - SysControl.ClientRect.Bottom > 0 then
        begin
          LBitmap := TBitmap.Create;
          FClientRect := SysControl.ClientRect;
          LBitmap.SetSize(FClientRect.Height, FClientRect.Height);
          LBitmap.PixelFormat := pf32bit;
          LBitmap.AlphaFormat := afDefined;
          if Assigned(Font) then
            LBitmap.Canvas.Font := Font;
          try
            PaintBackground(LBitmap.Canvas);
            FClientRect := SysControl.ClientRect;
            if Offset > 0 then
              LDetails := StyleServices.GetElementDetails(tsArrowBtnUpNormal)
            else
              LDetails := StyleServices.GetElementDetails(tsArrowBtnUpDisabled);
            GetItemHeight();
            ArrowHeight := trunc(FItemHeightCache / 2);
            R := FClientRect;
            R.Height := ArrowHeight;
            StyleServices.DrawElement(LBitmap.Canvas.Handle, LDetails, R);

            if ItemIsVisible(Count - 1) then
              LDetails := StyleServices.GetElementDetails(tsArrowBtnDownDisabled)
            else
              LDetails := StyleServices.GetElementDetails(tsArrowBtnDownNormal);
            R := FClientRect;
            R.Top := R.Top + R.Height - ArrowHeight;
            R.Height := ArrowHeight;
            StyleServices.DrawElement(LBitmap.Canvas.Handle, LDetails, R);

            BitBlt(DC, FClientRect.Left, FClientRect.Top, FClientRect.Width, FClientRect.Height, LBitmap.Canvas.Handle, 0, 0, SRCCOPY);
          finally
            LBitmap.Free;
          end;
        end;
      end;

      FPreviousHotItemIndex := Index;
      DoDrawItem(Canvas, Index);
      if (Index = Count - 1) then
      begin
        FItemsPainted := True;
        FPreviousHotItemIndex := -1;
      end;
      Handled := True;
      Exit;
    end;

    L := length(SubMenuItemInfoArray);
    if L <> 0 then
    begin
      for i := 0 to L - 1 do
      begin
        { Look for SubMenu Parent }
        LMenu := SubMenuItemInfoArray[i].Menu;
        if LMenu = FMenu then
        begin
          ParentPopup := SubMenuItemInfoArray[i].WindowHandle;
          ParentItem := SubMenuItemInfoArray[i].ItemIndex;
          Break;
        end;
      end;
    end;

    if (ParentPopup = Handle) then
      SetRedraw(True) { Allow drawing the current PopupMenu }
    else if ((ParentPopup <> Handle) and (FItemsPainted) and (ParentPopup <> 0)) then
    begin
      {
        if user jump so fast from the parent PopupMenu to the
        Child PopupMenu (SubMenu) , the hot item of parent Popup menu
        will be draw as a normal item (not hot)..
        So we need to repaint the hot item that drop the child popup menu.
      }
      if (not FParentSubItemPainted) and (ParentItem > -1) then
      begin
        SendMessage(ParentPopup, MN_SELECTITEM, ParentItem, 0);
        FParentSubItemPainted := True;
      end;
      { Don't Redraw the parent of the Current PopupMenu }
      // SetRedraw(ParentPopup, False);  //issue #81
    end;

    { if Item can drop a sub Popup Menu }
    if Items[Index].HasSubMenu then
    begin
      L := length(SubMenuItemInfoArray);
      if L = 0 then
      begin
        SetLength(SubMenuItemInfoArray, 1);
        SubMenuItemInfoArray[0].Menu := 0;
        L := 1;
      end;

      LMenu := GetMenuFromHandle(Handle);
      for i := 0 to L - 1 do
        { Avoid duplication }
        if SubMenuItemInfoArray[i].Menu <> LMenu then
        begin
          inc(L);
          SetLength(SubMenuItemInfoArray, L);
          SubMenuItemInfoArray[L - 1].Menu := GetSubMenu(FMenu, Index);
          SubMenuItemInfoArray[L - 1].WindowHandle := Handle;
          SubMenuItemInfoArray[L - 1].ItemIndex := Index;
          Break;
        end;
    end;

    LInitPos.X := 0;
    LInitPos.Y := 0;
    Index2 := GetItemClicked(LButton, LInitPos);
    if Index2 >= 0 then
      Index := Index2;

    { If all Items are painted }
    if FItemsPainted then
    begin
      { In order to show / hide SubMenu ,we need to
        process the default message handler . }
      SetRedraw(False);
      Message.Result := CallDefaultProc(Message);
      SetRedraw(True);
    end;

    if FPreviousHotItemIndex <> Index then
    begin
      { Draw Item normal . }
      DoDrawItem(Canvas, FPreviousHotItemIndex);
      { Draw Item hot . }
      DoDrawItem(Canvas, Index);
      FPreviousHotItemIndex := Index;
    end;

  finally
    Canvas.Handle := 0;
    Canvas.Free;
    if DC <> 0 then
      ReleaseDC(Handle, DC);
  end;
  Handled := True;
end;

procedure TSysPopupStyleHook.WMPRINT(var Message: TMessage);
var
  DC: HDC;
  i: integer;
  Canvas: TCanvas;
begin
  FMenu := GetMenuFromHandle(Handle);
  FCount := GetItemsCount;

  if Message.WParam <> 0 then
    DC := HDC(Message.WParam)
  else
    DC := GetDC(Handle);

  Canvas := TCanvas.Create;
  try
    Canvas.Handle := DC;
    PaintBackground(Canvas);
  finally
    Canvas.Handle := 0;
    Canvas.Free;
    if DC <> HDC(Message.WParam) then
      ReleaseDC(Handle, DC);
  end;

  FEnterWithKeyboard := (GetKeyState(VK_MENU) < 0);

  if Count > -1 then
  begin
    //exit;
    //FCount:=48;

//    for i := 0 to Count - 1 do
//    begin
//     GetMenuItemRect(0, FMenu, i, LRect);
//     OutputDebugString(PChar(Format('Index %d  Width %d Height %d Left %d Top %d', [i, LRect.Width, LRect.Height, LRect.Left, LRect.Top])));
//    end;

    for i := 0 + Offset to Count - 1 do
      PostMessage(Handle, MN_SELECTITEM, i, 0);
  end;
  Handled := True;
end;

function IsItemSeparator(Menu: HMENU; const ItemIndex: integer): Boolean;
var
  pMenuItemInfo: TMenuItemInfo;
begin
  {
    Use this function instead of Items[Index].Separator .
    ==> Fast access in WM_KEYDOWN .
  }
  Result := False;
  FillChar(pMenuItemInfo, sizeof(pMenuItemInfo), Char(0));
  pMenuItemInfo.cbSize := sizeof(TMenuItemInfo);
  pMenuItemInfo.fMask := MIIM_FTYPE;
  if GetMenuItemInfo(Menu, ItemIndex, True, pMenuItemInfo) then
    Result := (pMenuItemInfo.fType and MFT_SEPARATOR) = MFT_SEPARATOR;
end;

procedure TSysPopupStyleHook.GetNbSeparator(var PNbSeparator: Integer; const PIndex: Integer);
var
  i: Integer;
begin
  PNbSeparator := 0;
  GetItemHeight();
  GetSeparatorHeight();
  for i := 0 to PIndex - 1 do
  begin
    if IsItemSeparator(Menu, i) then
      Inc(PNbSeparator);
  end;
end;

function TSysPopupStyleHook.GetMenuItemHeight(const Index: Integer) : Integer;
var
  LItemRect: TRect;
begin
  GetMenuItemRect(0, FMenu, Index, LItemRect);
  Result := LItemRect.Height;
end;

function TSysPopupStyleHook.GetBottom(const Index: Integer) : Integer;
var
  LItemRect: TRect;
  P: TPoint;
begin
  GetMenuItemRect(0, FMenu, Index, LItemRect);
  P := Point(LItemRect.Left, LItemRect.Bottom);
  ScreenToClient(Handle, P);
  Result := P.Y;
end;

function TSysPopupStyleHook.GetOffset(UseCache: Boolean) : Integer;
var
  LNbSeparator: Integer;
begin
  if (Offset = 0) and (GetBottom(Count - 1) <= SysControl.ClientRect.Bottom) then
  begin
    Result := 0;
    exit;
  end;

  if UseCache then
    if FOffsetCache <> 0 then
    begin
      Result := FOffsetCache;
      exit;
    end;

  GetNbSeparator(LNbSeparator, Offset);
  Result := ((Offset - LNbSeparator) * FItemHeightCache) + (LNbSeparator * FSeparatorHeightCache) - trunc(FItemHeightCache / 2);
  if UseCache then
    FOffsetCache := Result;
end;

procedure TSysPopupStyleHook.GetItemHeight();
var
  i: Integer;
begin
  if FItemHeightCache = 1 then
    for i := 0 to Count - 1 do
      if not IsItemSeparator(Menu, i) then
      begin
        FItemHeightCache := GetMenuItemHeight(i);
        exit;
      end;
  if FItemHeightCache = 1 then
    FItemHeightCache := 22;
end;

procedure TSysPopupStyleHook.GetSeparatorHeight();
var
  i: Integer;
begin
  if FSeparatorHeightCache = 1 then
    for i := 0 to Count - 1 do
      if IsItemSeparator(Menu, i) then
      begin
        FSeparatorHeightCache := GetMenuItemHeight(i);
        exit;
      end;
  if FSeparatorHeightCache = 1 then
    FSeparatorHeightCache := 8;
end;

procedure TSysPopupStyleHook.SetMaxOffset();
var
  LGap, LBottom: Integer;
begin
  { Search gap }
  LBottom := GetBottom(Count - 1);
  { No up/down button? }
  LGap :=  LBottom - SysControl.ClientRect.Bottom;
  if LGap <= 0 then
  begin
    Offset := 0;
    exit;
  end;

  { Search height to calc offset}
  GetItemHeight();

  { FItemHeightCache is for 2 buttons up/down }
  LGap :=  LGap + round(FItemHeightCache / 2);
  if LGap <= 0 then
  begin
    Offset := 0;
    exit;
  end;

  Offset := trunc(LGap / FItemHeightCache);

  while not ItemIsVisible(Count - 1) do
    Offset := Offset + 1;
end;

procedure TSysPopupStyleHook.RefreshMenu();
var
  DC: HDC;
  LBitmap: TBitmap;
  i, ArrowHeight: Integer;
  LDetails: TThemedElementDetails;
  R, FClientRect: TRect;
begin
  DC := 0;
  LBitmap := TBitmap.Create;
  FClientRect := SysControl.ClientRect;
  LBitmap.SetSize(FClientRect.Height, FClientRect.Height);
  LBitmap.PixelFormat := pf32bit;
  LBitmap.AlphaFormat := afDefined;
  if Assigned(Font) then
    LBitmap.Canvas.Font := Font;
  try
    DC := GetDC(Handle);
    PaintBackground(LBitmap.Canvas);
    for i := Offset to Count - 1 do
    begin
      if ItemIsVisible(i) then
        DoDrawItem(LBitmap.Canvas, i, True)
      else
        break;
    end;
    { draw up/dowm button }
    FClientRect := SysControl.ClientRect;
    if Offset > 0 then
      LDetails := StyleServices.GetElementDetails(tsArrowBtnUpNormal)
    else
      LDetails := StyleServices.GetElementDetails(tsArrowBtnUpDisabled);
    ArrowHeight := trunc(FItemHeightCache / 2);
    R := FClientRect;
    R.Height := ArrowHeight;
    StyleServices.DrawElement(LBitmap.Canvas.Handle, LDetails, R);

    if ItemIsVisible(Count - 1) then
      LDetails := StyleServices.GetElementDetails(tsArrowBtnDownDisabled)
    else
      LDetails := StyleServices.GetElementDetails(tsArrowBtnDownNormal);
    R := SysControl.ClientRect;
    R.Top := R.Top + R.Height - ArrowHeight;
    R.Height := ArrowHeight;
    StyleServices.DrawElement(LBitmap.Canvas.Handle, LDetails, R);

    BitBlt(DC, FClientRect.Left, FClientRect.Top, FClientRect.Width, FClientRect.Height, LBitmap.Canvas.Handle, 0, 0, SRCCOPY);
  finally
    LBitmap.Free;
    if DC <> 0 then
      ReleaseDC(Handle, DC);
  end;
end;

function TSysPopupStyleHook.ItemIsVisible(const Index: Integer): Boolean;
var
  LRect, LItemRect: TRect;
  P: TPoint;
  ClientRectHeight: Integer;
  LOffset: Integer;
begin
  result := True;
  if (Index < 0) or (Index >= Count) then
    Exit;

  GetMenuItemRect(0, FMenu, Index, LItemRect);
  P := Point(LItemRect.Left, LItemRect.Bottom);
  ScreenToClient(Handle, P);
  LOffset := GetOffset(True);
  P.Y := P.Y - LOffset;

  ClientRectHeight := GetClientRectHeight(LRect);
  LRect.Height := ClientRectHeight;

  //prevent draw  not visible items on larger menus
  Result := PtInRect (LRect, P);

  if Result then
  begin
    P := Point(LItemRect.Left, LItemRect.Top);
    ScreenToClient(Handle, P);
    P.Y := P.Y - LOffset;
    if ClientRectHeight > LItemRect.Height then
      LRect.Height :=  ClientRectHeight - LItemRect.Height;
    Result := PtInRect (LRect, P);
  end;
end;

function TSysPopupStyleHook.GetClientRectHeight(var PValue: TRect): Integer;
begin
  PValue := SysControl.ClientRect;
  Result := PValue.Height;
  { Use offset? }
  if GetBottom(Count - 1) - Result > 0 then
  begin
    GetItemHeight();
    Result := Result - FItemHeightCache;
  end;
end;

procedure TSysPopupStyleHook.SetOffset(PValue: Integer);
begin
  FOffset := PValue;
  FOffsetCache := 0;
end;

function TSysPopupStyleHook.GetItemClicked(var PButton: Byte; PInitPos: TPoint) : Integer;
var
  ArrowHeight, i: Integer;
  R, LItemRect: TRect;
  Pos: TPoint;
begin
  PButton := 0;
  Result := -1;
  if (PInitPos.X = 0) and (PInitPos.Y = 0) then
    Pos := GetMousePos()
  else
  begin
    Pos := PInitPos;
    ScreenToClient(Handle, Pos);
  end;

  R := SysControl.ClientRect;

  if GetBottom(Count - 1) - R.Bottom > 0 then
  begin
    ArrowHeight := trunc(FItemHeightCache / 2);
    if Pos.Y <= ArrowHeight then
    begin
      { up button}
      PButton := 1;
      exit;
    end;

    if Pos.Y >= R.Bottom - ArrowHeight then
    begin
      { down button}
      PButton := 2;
      exit;
    end;
  end;

  Pos.Y := Pos.Y + GetOffset(True);
  ClientToScreen(Handle, Pos);
  for i := Offset to Count - 1 do
  begin
    GetMenuItemRect(0, FMenu, i, LItemRect);
    if PtInRect (LItemRect, Pos) then
    begin
      Result := i;
      exit;
    end;
  end;
  if Result = -1 then
    Result := -1;
end;

function TSysPopupStyleHook.GetMousePos() : TPoint;
begin
  Result := Mouse.CursorPos;
  ScreenToClient(Handle, Result);
end;

procedure TSysPopupStyleHook.WndProc(var Message: TMessage);
var
  i: integer;
  TopWin: HWND;
  TopCntrl: TControl;
  LButton: Byte;
  LInitPos: TPoint;
  Message2: TMessage;
  LSwap: Boolean;

begin
//OutputDebugString(PChar(FormatDateTime('hh:nn:ss.zzz', Now)+' Msg = ' + IntToHex(Message.Msg, 4) + ' wParam = ' + IntToHex(Message.wParam, 8) + ' LParam = ' + IntToHex(Message.lParam, 8)));
//  AddToLog(Message);
//Message.Result := CallDefaultProc(Message);
//Exit;
{

  case Message.Msg of
            WM_KEYFIRST..WM_KEYLAST:
            begin
              FEnterWithKeyboard := True;
            end;
  end;
}
  case Message.Msg of
    MN_SELECTITEM, WM_PRINT:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit; { Do not Dispatch . }
        end;
      end;

    WM_PAINT:
      begin
        if not OverridePaint then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;
        SetRedraw(False);
        Message.Result := CallDefaultProc(Message);
        SetRedraw(True);
        Exit; { Do not Dispatch . }
      end;

    WM_WINDOWPOSCHANGED:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;
        SetTimer(Handle, $93, 100, nil);
      end;

    WM_TIMER:
      begin
        if (FItemsPainted) and (Message.WParam = $93) then
        begin
          { If PopupMenu is droped from MainMenu ,
            MainMenu will send WM_KEYDOWN message
            to the PopupMenu that cause the PopupMenu
            to paint the first item as a hot item instead of
            a normal item .
            I use a timer to solve this problem .
          }
          FPaintFirstItemFromMenu := True;
          KillTimer(Handle, $93);
        end;
        if Message.WParam = $201 then
        { down menu }
        begin
          LSwap := GetSystemMetrics(SM_SWAPBUTTON) <> 0;
          if (LSwap and ((GetKeyState(VK_RBUTTON) and $80) <> 0)) or (not(LSwap) and ((GetKeyState(VK_LBUTTON) and $80) <> 0)) then
          begin
            if not ItemIsVisible(Count - 1) then
            begin
              Offset := Offset + 1;
              RefreshMenu();
            end
            else
              KillTimer(Handle, $201);
          end
          else
            KillTimer(Handle, $201);
        end;
        if Message.WParam = $202 then
        { up menu }
        begin
          LSwap := GetSystemMetrics(SM_SWAPBUTTON) <> 0;
          if (LSwap and ((GetKeyState(VK_RBUTTON) and $80) <> 0)) or (not(LSwap) and ((GetKeyState(VK_LBUTTON) and $80) <> 0)) then
          begin
            if Offset > 0 then
            begin
              Offset := Offset - 1;
              RefreshMenu();
            end
            else
              KillTimer(Handle, $202);
          end
          else
            KillTimer(Handle, $202);
        end;
      end;

    MN_BUTTONDOWN_UP, MN_DBLCLK:
      begin
        { we should calc item/button pressed with mouse position }
        LInitPos.X := 0;
        LInitPos.Y := 0;
        FKeyIndex := GetItemClicked(LButton, LInitPos);
        case LButton of
          0:
          begin
            { Click on MenuItem}
            if FKeyIndex = -1 then
            begin
              Message.Result := 0;
              exit;
            end;
            Message2.Msg := MN_SELECTITEM;
            Message2.wParam := FKeyIndex;
            Message2.lParam := 0;
            Message2.Result := 0;
            CallDefaultProc(Message2);

            Message2.Msg := WM_KEYDOWN;
            Message2.wParam := VK_RETURN;
            CallDefaultProc(Message2);

            Message2.Msg := WM_KEYUP;
            CallDefaultProc(Message2);
            exit;
          end;
          1:
          begin
            { Scroll up menu }
            if Offset > 0 then
            begin
              FKeyIndex := -1;
              Offset := Offset - 1;
              RefreshMenu();
              if FKeyIndex <> FPreviousHotItemIndex then
                SendMessage(Handle, MN_SELECTITEM, FKeyIndex, 0);
              Message.Result := 0;
              SetTimer(Handle, $202, 150, nil);
              exit;
            end
            else
            begin
              Message.Result := 0;
              exit;
            end;
          end;
          2:
          begin
            { Scroll down }
            if not ItemIsVisible(Count - 1) then
            begin
              Offset := Offset + 1;
              FKeyIndex := -1;
              RefreshMenu();
              if FKeyIndex <> FPreviousHotItemIndex then
                SendMessage(Handle, MN_SELECTITEM, FKeyIndex, 0);
              SetTimer(Handle, $201, 150, nil);
              Message.Result := 0;
              exit;
            end
            else
            begin
              Message.Result := 0;
              exit;
            end;
          end;
        end;
      end;

    WM_KEYDOWN:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;

        FEnterWithKeyboard := True;

        FMenu := GetMenuFromHandle(Handle);
        if FPreviousHotItemIndex <> -1 then
          FKeyIndex := FPreviousHotItemIndex;

        case Message.WParam of

          VK_DOWN:
            if FPaintFirstItemFromMenu then
            begin
              if FKeyIndex >= GetMenuItemCount(Menu) - 1 then
                FKeyIndex := -1;

              Inc(FKeyIndex);
              { If the Current Item is Separator then
                find the next valid item .
              }
              if IsItemSeparator(Menu, FKeyIndex) then
                for i := FKeyIndex to GetMenuItemCount(Menu) - 1 do
                  if (not IsItemSeparator(Menu, i)) then
                  begin
                    FKeyIndex := i;
                    Break;
                  end;
              if not ItemIsVisible(FKeyIndex) then
              begin
                if FKeyIndex <= 0 then
                begin
                  if Offset <> 0 then
                  begin
                    Offset := 0;
                  end;
                end
                else
                begin
                  if Offset >= GetMenuItemCount(Menu) then
                    Offset := GetMenuItemCount(Menu) - 1
                  else
                  begin
                    Offset := Offset + 1;
                    while not ItemIsVisible(FKeyIndex) do
                      Offset := Offset + 1;
                  end;
                end;
                RefreshMenu();
              end;

              SendMessage(Handle, MN_SELECTITEM, FKeyIndex, 0);
              Message.Result := 0;
            end;

          VK_UP:
            begin
              if (FKeyIndex <= 0) or (FKeyIndex > GetMenuItemCount(Menu)) then
                FKeyIndex := GetMenuItemCount(Menu);

              Dec(FKeyIndex);
              { If the Current Item is Separator then
                find the next valid item .
              }
              if IsItemSeparator(Menu, FKeyIndex) then
                for i := FKeyIndex downto 0 do
                  if not IsItemSeparator(Menu, i) then
                  begin
                    FKeyIndex := i;
                    Break;
                  end;
              if not ItemIsVisible(FKeyIndex) then
              begin
                if FKeyIndex <= 0 then
                begin
                  if Offset <> 0 then
                  begin
                    Offset := 0
                  end;
                end
                else
                begin
                  Offset := Offset - 1;
                  if Offset < 0 then
                  begin
                    { Calc new offset value }
                    SetMaxOffset();
                  end
                  else
                  begin
                    while not ItemIsVisible(FKeyIndex) do
                      Offset := Offset - 1;
                  end;
                end;
                RefreshMenu();
              end;

              SendMessage(Handle, MN_SELECTITEM, FKeyIndex, 0);
              Message.Result := 0;
            end;

        else
          { Calling the Default Message will cause
            the WM_PAINT Message to be Sent to the PopupMenu Window }
          Message.Result := CallDefaultProc(Message);
        end;
        Exit;
      end;

    WM_ERASEBKGND:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;
        SendMessage(Handle, WM_PRINT, Message.WParam, Message.lParam);
        Message.Result := 1;
        Exit; { Do not Dispatch . }
      end;

    WM_PRINTCLIENT:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;
        SendMessage(Handle, WM_PRINT, Message.WParam, Message.lParam);
        Exit;
      end;

    WM_NCCALCSIZE, WM_NCPAINT:
      begin
        if Message.Msg= WM_NCCALCSIZE then
        begin
          if TWMNCCalcSize(Message).CalcValidRects then
          begin
            FNCRect := TWMNCCalcSize(Message).CalcSize_Params.rgrc[0];
            //Message.Result := CallDefaultProc(Message);
            //LRect := TWMNCCalcSize(Message).CalcSize_Params.rgrc0;
            //OutputDebugString(PChar(Format('LRect.Height %d WParam %d', [FNCRect.Height, Message.WParam])));
          end;
        end;

        if (not OverridePaint) or (not OverridePaintNC) then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;
        if not StyleServicesEnabled then
        begin
          Handled := False;
          Exit;
        end;
        Exit; { Do not Dispatch . }
      end;

    WM_DESTROY:
      begin
        TopWin := GetForegroundWindow;
        if TopWin > 0 then
        begin
          { The parent window that host menu should be repained !! }
          if IsVCLControl(TopWin) then
          begin
            TopCntrl := FindControl(TopWin);
            if Assigned(TopCntrl) then
            begin
              {
                Must use TControl.Refresh to allow invalidating
                others no TWinControl !
              }
              TopCntrl.Refresh;
            end;
          end
          else if IsControlHooked(TopWin) then
          begin
            // AddToLog(IntToStr(TopWin));
            InvalidateRect(TopWin, nil, False);
            UpdateWindow(TopWin);
          end;
        end;
        FVCLMenuItems := nil;
        SetLength(SubMenuItemInfoArray, 0);
        SubMenuItemInfoArray := nil;
        Handled := False;
      end;

    //
    // WM_NCDESTROY :
    // begin
    // end;

  end;
  inherited;
end;

{ TSysPopupItem }

constructor TSysPopupStyleHook.TSysPopupItem.Create(SysPopupStyleHook: TSysPopupStyleHook; SysParent: TSysControl; const Index: integer; const Menu: HMENU);
begin
  inherited Create;
  FSysPopupStyleHook := SysPopupStyleHook;
  FMenu := Menu;
  FHandle := SysParent.Handle;
  FSysParent := SysParent;
  FIndex := Index;
end;

destructor TSysPopupStyleHook.TSysPopupItem.Destroy;
begin
  inherited;
end;

function TSysPopupStyleHook.TSysPopupItem.GetItemBitmap: HBITMAP;
var
  pMenuItemInfo: TMenuItemInfo;
begin
  Result := 0;
  FillChar(pMenuItemInfo, sizeof(pMenuItemInfo), Char(0));
  pMenuItemInfo.cbSize := sizeof(TMenuItemInfo);
  pMenuItemInfo.fMask := MIIM_CHECKMARKS or MIIM_BITMAP;
  if GetMenuItemInfo(FMenu, FIndex, True, pMenuItemInfo) then
  begin
    Result := pMenuItemInfo.hbmpItem;
    if Result = 0 then
      Result := pMenuItemInfo.hbmpUnchecked;
  end;
end;

function TSysPopupStyleHook.TSysPopupItem.GetItemID: WORD;
begin
  Result := 0;
  if (FMenu > 0) and (FIndex > -1) then
    Result := GetMenuItemID(FMenu, FIndex);
end;

function TSysPopupStyleHook.TSysPopupItem.GetItemRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if (FMenu > 0) and (FIndex > -1) then
    GetMenuItemRect(0, FMenu, FIndex, Result);
end;

function TSysPopupStyleHook.TSysPopupItem.GetItemText: String;
var
  Buffer: PChar;
  StrSize: integer;
  pMenuItemInfo: MENUITEMINFO;
begin

  if VCLItem <> nil then
  begin
    Result := VCLItem.Caption;
    Exit;
  end;

  { Note:
    The GetMenuString function has been superseded.
    Use the GetMenuItemInfo function to retrieve the menu item text.
  }

  Result := '';

  FillChar(pMenuItemInfo, SizeOf(MENUITEMINFO), Char(0));
  pMenuItemInfo.cbSize := SizeOf(MENUITEMINFO);
  pMenuItemInfo.fMask := MIIM_STRING or MIIM_FTYPE;
  pMenuItemInfo.dwTypeData := nil;
  if GetMenuItemInfo(FMenu, FIndex, True, pMenuItemInfo) then
  begin
     //Fix for shell menus on W10
    if (VCLMenuItems = nil) or (not (pMenuItemInfo.fType and MFT_OWNERDRAW = MFT_OWNERDRAW)) then
    begin
      { The Size needed for the Buffer . }
      StrSize := pMenuItemInfo.cch * 2 + 2;
      GetMem(Buffer, StrSize);
      try
        pMenuItemInfo.dwTypeData := Buffer;
        { inc cch to get the last char . }
        inc(pMenuItemInfo.cch);
        if GetMenuItemInfo(FMenu, FIndex, True, pMenuItemInfo) then
          Result := String(Buffer);
      finally
        // OutputDebugString(PChar('StrSize '+IntToStr(StrSize)));
        FreeMem(Buffer, StrSize);
      end;
      Exit;
    end
    else
    begin
      { if the item is owner draw then we need another way to get
        the item text since , when setting an item to ownerdraw windows
        will destroy the dwTypeData that hold the text . }
      FillChar(pMenuItemInfo, sizeof(MENUITEMINFO), Char(0));
      pMenuItemInfo.cbSize := sizeof(MENUITEMINFO);
      pMenuItemInfo.fMask := MIIM_DATA;
      if GetMenuItemInfo(FMenu, FIndex, True, pMenuItemInfo) then
        Result := String(PChar(pMenuItemInfo.dwItemData));
    end;
  end;
end;

function TSysPopupStyleHook.TSysPopupItem.GetVCLRealItem: TMenuItem;
var
  i: integer;
  VisibleItems: TList;
  LVCLMenuItems: TMenuItem;
begin
  {
    Return the real menu item .
    If MenuItem has the Visible property set to false
    windows will delete this item but the VCL will not delete
    the item from Items property .And thats can cause the item to be painted !
    Do not access VCLMenuItems.Items[Index] directly
    => Instead , use this one : VCLItem .
  }
  VisibleItems := nil;
  Result := nil;
  LVCLMenuItems := VCLMenuItems;
  if LVCLMenuItems <> nil then
  begin
    VisibleItems := TList.Create;
    for i := 0 to LVCLMenuItems.Count - 1 do
    begin
      if LVCLMenuItems.Items[i].Visible then
        VisibleItems.Add(LVCLMenuItems.Items[i]);
    end;
  end;
  if Assigned(VisibleItems) then
  begin
    if (VisibleItems.Count > 0) and (FIndex < VisibleItems.Count) then
      Result := VisibleItems.Items[FIndex];
    FreeAndNil(VisibleItems);
  end;
end;

function TSysPopupStyleHook.TSysPopupItem.GetVCLMenuItems: TMenuItem;
var
  i, j: integer;
  LPopupMenu: TPopupMenu;
  LForm: TCustomForm;
  LMenuItem: TMenuItem;

  function GetChildPopup(Comp: TComponent): TMenuItem;
  var
    k: integer;
  begin
    Result := nil;
    if Assigned(Comp) then
    begin
      for k := 0 to Comp.ComponentCount - 1 do
      begin

        if Comp.Components[k] is TPopupMenu then
        begin
          LPopupMenu := TPopupMenu(Comp.Components[k]);
          if LPopupMenu.Handle = FMenu then
            Exit(LPopupMenu.Items);
        end
        else if Comp.Components[k] is TMenuItem then
        begin
          LMenuItem := TMenuItem(Comp.Components[k]);
          if LMenuItem.Handle = FMenu then
            Exit(LMenuItem);
        end;

        if Comp.Components[k].ComponentCount > 0 then
          Result := GetChildPopup(Comp.Components[k]);
        if Assigned(Result) then
          Exit;
      end;
    end;
  end;

  function GetMenuItem(AMenu: TMenuItem): TMenuItem;
  var
    LMenuItem: TMenuItem;
    i: integer;
  begin
    if (AMenu.Handle = FMenu) then
      Result := AMenu
    else
    begin
      Result := nil;
      i := 0;
      while (Result = nil) and (i < AMenu.Count) do
      begin
        LMenuItem := AMenu.Items[i];
        Result := GetMenuItem(LMenuItem);
        inc(i);
      end;
    end;
  end;

begin
  Result := nil;

  for i := 0 to PopupList.Count - 1 do
    if TPopupMenu(PopupList.Items[i]).Handle = FMenu then
      Exit(TPopupMenu(PopupList.Items[i]).Items);

  for i := 0 to Screen.FormCount - 1 do
  begin
    LForm := Screen.Forms[i];
    for j := 0 to LForm.ComponentCount - 1 do
    begin

      if LForm.Components[j] is TMenuItem then
      begin
        LMenuItem := TMenuItem(LForm.Components[j]);
        Result := GetMenuItem(LMenuItem);
        if Assigned(Result) then
          Exit;
      end
      else if LForm.Components[j] is TPopupMenu then
      begin
        LPopupMenu := TPopupMenu(LForm.Components[j]);
        if LPopupMenu.Handle = FMenu then
          Exit(LPopupMenu.Items);

        Result := GetMenuItem(LPopupMenu.Items);
        if Assigned(Result) then
          Exit;
      end
      else
      begin
        Result := GetChildPopup(LForm.Components[j]);
        if Assigned(Result) then
          Exit;
      end;
    end;
  end;
end;

function TSysPopupStyleHook.TSysPopupItem.GetVCLMenuItemsFast: TMenuItem;
begin
  if Assigned(FSysPopupStyleHook.FVCLMenuItems) then
    Result := FSysPopupStyleHook.FVCLMenuItems
  else
  begin
    Result := GetVCLMenuItems;
    FSysPopupStyleHook.FVCLMenuItems := Result;
  end;
end;

function TSysPopupStyleHook.TSysPopupItem.IsItemDisabled: Boolean;
var
  pMenuItemInfo: TMenuItemInfo;
begin
  Result := False;
  FillChar(pMenuItemInfo, sizeof(pMenuItemInfo), Char(0));
  pMenuItemInfo.cbSize := sizeof(TMenuItemInfo);
  pMenuItemInfo.fMask := MIIM_STATE;
  if GetMenuItemInfo(FMenu, FIndex, True, pMenuItemInfo) then
    Result := (pMenuItemInfo.fState and MFS_DISABLED = MFS_DISABLED) or (pMenuItemInfo.fState and MF_DISABLED = MF_DISABLED) or
      (pMenuItemInfo.fState and MF_GRAYED = MF_GRAYED);
end;

function TSysPopupStyleHook.TSysPopupItem.IsItemOwnerDraw: Boolean;
var
  pMenuItemInfo: TMenuItemInfo;
begin
  Result := False;
  FillChar(pMenuItemInfo, sizeof(MENUITEMINFO), Char(0));
  pMenuItemInfo.cbSize := sizeof(MENUITEMINFO);
  pMenuItemInfo.fMask := MIIM_FTYPE;
  pMenuItemInfo.dwTypeData := nil;
  if GetMenuItemInfo(FMenu, FIndex, True, pMenuItemInfo) then
    Result := (pMenuItemInfo.fType and MFT_OWNERDRAW = MFT_OWNERDRAW);
end;

function TSysPopupStyleHook.TSysPopupItem.IsItemRadioCheck: Boolean;
var
  pMenuItemInfo: TMenuItemInfo;
begin
  Result := False;
  FillChar(pMenuItemInfo, sizeof(pMenuItemInfo), Char(0));
  pMenuItemInfo.cbSize := sizeof(TMenuItemInfo);
  pMenuItemInfo.fMask := MIIM_FTYPE;
  if GetMenuItemInfo(FMenu, FIndex, True, pMenuItemInfo) then
    Result := (pMenuItemInfo.fType and MFT_RADIOCHECK) = MFT_RADIOCHECK;
end;

function TSysPopupStyleHook.TSysPopupItem.IsItemChecked: Boolean;
var
  pMenuItemInfo: TMenuItemInfo;
begin
  Result := False;
  FillChar(pMenuItemInfo, sizeof(pMenuItemInfo), Char(0));
  pMenuItemInfo.cbSize := sizeof(TMenuItemInfo);
  pMenuItemInfo.fMask := MIIM_STATE;
  if GetMenuItemInfo(FMenu, FIndex, True, pMenuItemInfo) then
    Result := (pMenuItemInfo.fState and MFS_CHECKED) = MFS_CHECKED;
end;

function TSysPopupStyleHook.TSysPopupItem.IsItemContainsSubMenu: Boolean;
begin
  Result := (GetSubMenu(FMenu, FIndex) > 0);
end;

function TSysPopupStyleHook.TSysPopupItem.IsItemDefault: Boolean;
var
  pMenuItemInfo: TMenuItemInfo;
begin
  Result := False;
  FillChar(pMenuItemInfo, sizeof(pMenuItemInfo), Char(0));
  pMenuItemInfo.cbSize := sizeof(TMenuItemInfo);
  pMenuItemInfo.fMask := MIIM_STATE;
  if GetMenuItemInfo(FMenu, FIndex, True, pMenuItemInfo) then
    Result := (pMenuItemInfo.fState and MFS_DEFAULT) = MFS_DEFAULT;
end;

function TSysPopupStyleHook.TSysPopupItem.IsItemSeparator: Boolean;
var
  pMenuItemInfo: TMenuItemInfo;
begin
  FillChar(pMenuItemInfo, sizeof(pMenuItemInfo), Char(0));
  pMenuItemInfo.cbSize := sizeof(TMenuItemInfo);
  pMenuItemInfo.fMask := MIIM_FTYPE;
  Result := False;
  if (FIndex > -1) and (FIndex < GetMenuItemCount(FMenu) - 1) then
  begin
    if GetMenuItemInfo(FMenu, FIndex, True, pMenuItemInfo) then
      Result := (pMenuItemInfo.fType and MFT_SEPARATOR) = MFT_SEPARATOR;
  end;
end;

initialization

SubMenuItemInfoArray := nil;

{$IFDEF UseVCLStyleUtilsMenu}
{$IF CompilerVersion >= 27} // Disable XE6-XE7 menu syshooks
TStyleManager.SystemHooks := TStyleManager.SystemHooks - [shMenus];
{$IFEND CompilerVersion}
if StyleServices.Available then
  TSysStyleManager.RegisterSysStyleHook('#32768', TSysPopupStyleHook);
{$ENDIF UseVCLStyleUtilsMenu}

finalization

{$IFDEF UseVCLStyleUtilsMenu}
  TSysStyleManager.UnRegisterSysStyleHook('#32768', TSysPopupStyleHook);
{$ENDIF UseVCLStyleUtilsMenu}

end.
