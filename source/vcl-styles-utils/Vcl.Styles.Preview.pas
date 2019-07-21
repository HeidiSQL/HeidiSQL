unit Vcl.Styles.Preview;

interface

Uses
  System.Classes, System.Generics.Collections, Winapi.Windows, Vcl.Styles,
  Vcl.Themes, Vcl.Forms, Vcl.Graphics, Vcl.Controls, Vcl.ExtCtrls;

type
  TPreviewType = (ptOriginal, ptTabs);

  TVisualStylePreview = class(TCustomControl)
    protected
      FStyle           : TCustomStyleServices;
      FIcon            : HICON;
      FCaption         : TCaption;
      FRegion          : HRGN;
      FBitmap          : TBitmap;
      FPreviewType     : TPreviewType;
      FFormBorderSize  : TRect;
      FBkgColor        : TColor;
      FUnavailableText : string;
      FSelectedText    : string;
      FHotText         : string;
      FNormalText      : string;
      FDisabledText    : string;
      FPressedText     : string;
      FButtonText      : string;
      FFileMenuText    : string;
      FEditMenuText    : string;
      FViewMenuText    : string;
      FHelpMenuText    : string;

      procedure SetStyle(const aStyle : TCustomStyleServices);

      function  GetFormBorderSize : TRect;
      function  GetMainMenuRect : TRect;
      function  GetTabsRect : TRect;

      function  GetCaptionHeight : integer;
      function  GetLeftFormBorderWidth : integer;
      function  GetRightFormBorderWidth : integer;
      function  GetBottomFormBorderHeight : integer;
      function  RectVCenter(var aRect : TRect; aBounds : TRect): TRect;

      procedure DrawCaption;
      procedure DrawFormBorders;
      procedure DrawMainMenu;
      procedure DrawToolButtons;
      procedure DrawButtons;
      procedure DrawTabs;
      procedure DrawDefaultPanel;
      procedure DrawOriginalPreview;
      procedure DrawTabsPreview;

      procedure Paint; override;

    public
      constructor Create(AControl: TComponent); override;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;

      property Icon            : HICON                read FIcon            write FIcon;
      property Style           : TCustomStyleServices read FStyle           write SetStyle;
      property Caption         : TCaption             read FCaption         write FCaption;
      property Bitmap          : TBitmap              read FBitmap          write FBitmap;
      property UnavailableText : string               read FUnavailableText write FUnavailableText;
      property SelectedText    : string               read FSelectedText    write FSelectedText;
      property HotText         : string               read FHotText         write FHotText;
      property NormalText      : string               read FNormalText      write FNormalText;
      property DisabledText    : string               read FDisabledText    write FDisabledText;
      property PressedText     : string               read FPressedText     write FPressedText;
      property ButtonText      : string               read FButtonText      write FButtonText;
      property FileMenuText    : string               read FFileMenuText    write FFileMenuText;
      property EditMenuText    : string               read FEditMenuText    write FEditMenuText;
      property ViewMenuText    : string               read FViewMenuText    write FViewMenuText;
      property HelpMenuText    : string               read FHelpMenuText    write FHelpMenuText;

    published
      property PreviewType     : TPreviewType         read FPreviewType     write FPreviewType;
      property BkgColor        : TColor               read FBkgColor        write FBkgColor;
      property Align;
      property Anchors;
      property Visible;
  end;

implementation

uses
  System.SysUtils, System.Types, System.UITypes;

const
  ORIGINAL_PPI = 96;

constructor TVisualStylePreview.Create(AControl: TComponent);
begin
  inherited Create(AControl);

  FRegion         := 0;
  FStyle          := nil;
  FCaption        := '';
  FIcon           := 0;
  FBitmap         := nil;
  FPreviewType    := ptOriginal;
  FFormBorderSize := rect(0, 0, 0, 0);
  FBkgColor       := clNone;

  FUnavailableText := 'Preview not available';
  FSelectedText    := 'Selected';
  FHotText         := 'Hot';
  FNormalText      := 'Normal';
  FDisabledText    := 'Disabled';
  FPressedText     := 'Pressed';
  FButtonText      := 'ToolButton';
  FFileMenuText    := 'File';
  FEditMenuText    := 'Edit';
  FViewMenuText    := 'View';
  FHelpMenuText    := 'Help';
end;

destructor TVisualStylePreview.Destroy;
begin
  try
    if (FRegion <> 0) then
      begin
        DeleteObject(FRegion);
        FRegion := 0;
      end;

    if (FBitmap <> nil) then FreeAndNil(FBitmap);
    if (FStyle  <> nil) then FreeAndNil(FStyle);
    if (FStyle  <> nil) then FreeAndNil(FStyle);
  finally
    inherited Destroy;
  end;
end;

procedure TVisualStylePreview.AfterConstruction;
begin
  inherited AfterConstruction;

  FBitmap             := TBitmap.Create;
  FBitmap.PixelFormat := pf32bit;
end;

procedure TVisualStylePreview.SetStyle(const aStyle : TCustomStyleServices);
begin
  if (FStyle <> nil) then FreeAndNil(FStyle);

  FStyle := aStyle;
  Refresh;
end;

function TVisualStylePreview.GetCaptionHeight : integer;
var
  LSize    : TSize;
  LDetails : TThemedElementDetails;
begin
  LDetails := FStyle.GetElementDetails(twCaptionActive);
  FStyle.GetElementSize(0, LDetails, esActual, LSize);
  Result := LSize.cy;
end;

function TVisualStylePreview.GetLeftFormBorderWidth : integer;
var
  LSize    : TSize;
  LDetails : TThemedElementDetails;
begin
  LDetails := FStyle.GetElementDetails(twFrameLeftActive);
  FStyle.GetElementSize(0, LDetails, esActual, LSize);
  Result := LSize.cx;
end;

function TVisualStylePreview.GetRightFormBorderWidth : integer;
var
  LSize    : TSize;
  LDetails : TThemedElementDetails;
begin
  LDetails  := FStyle.GetElementDetails(twFrameRightActive);
  FStyle.GetElementSize(0, LDetails, esActual, LSize);
  Result := LSize.cx;
end;

function TVisualStylePreview.GetBottomFormBorderHeight : integer;
var
  LSize    : TSize;
  LDetails : TThemedElementDetails;
begin
  LDetails   := FStyle.GetElementDetails(twFrameBottomActive);
  FStyle.GetElementSize(0, LDetails, esActual, LSize);
  Result := LSize.cy;
end;

function TVisualStylePreview.GetFormBorderSize: TRect;
begin
  Result.Top    := GetCaptionHeight;
  Result.Left   := GetLeftFormBorderWidth;
  Result.Right  := GetRightFormBorderWidth;
  Result.Bottom := GetBottomFormBorderHeight;
end;

function TVisualStylePreview.GetMainMenuRect : TRect;
const
  MENU_ITEM_HEIGHT = 20;
begin
  Result.Left   := FFormBorderSize.Left;
  Result.Top    := FFormBorderSize.Top;
  Result.Right  := FBitmap.Width - FFormBorderSize.Right;
  Result.Bottom := Result.Top + MulDiv(MENU_ITEM_HEIGHT, screen.PixelsPerInch, ORIGINAL_PPI);
end;

function TVisualStylePreview.GetTabsRect : TRect;
const
  TABS_HEIGHT = 27;
begin
  Result.Left   := FFormBorderSize.Left;
  Result.Top    := FFormBorderSize.Top;
  Result.Right  := FBitmap.Width - FFormBorderSize.Right;
  Result.Bottom := Result.Top + MulDiv(TABS_HEIGHT, screen.PixelsPerInch, ORIGINAL_PPI);
end;

function TVisualStylePreview.RectVCenter(var aRect : TRect; aBounds : TRect): TRect;
begin
  OffsetRect(aRect, - aRect.Left, - aRect.Top);
  OffsetRect(aRect, 0, (aBounds.Height - aRect.Height) div 2);
  OffsetRect(aRect, aBounds.Left, aBounds.Top);

  Result := aRect;
end;

procedure TVisualStylePreview.DrawDefaultPanel;
var
  LDetails : TThemedElementDetails;
  LColor   : TColor;
  LRect    : TRect;
begin
  LRect := rect(0, 0, FBitmap.Width, FBitmap.Height);

  if (csDesigning in ComponentState) then
    begin
      if (FBkgColor <> clNone) then
        FBitmap.Canvas.Brush.Color := FBkgColor
       else
        FBitmap.Canvas.Brush.Color := clWhite;

      FBitmap.Canvas.Brush.Style := bsSolid;
      FBitmap.Canvas.FillRect(LRect);
      exit;
    end;

  if (FBkgColor <> clNone) then
    LColor := FBkgColor
   else
    begin
      LDetails := StyleServices.GetElementDetails(tpPanelBackground);

      if not(StyleServices.GetElementColor(LDetails, ecFillColor, LColor)) then
        LColor := GetSysColor(COLOR_BTNFACE);
    end;

  FBitmap.Canvas.Brush.Color := LColor;
  FBitmap.Canvas.Brush.Style := bsSolid;
  FBitmap.Canvas.FillRect(LRect);

  if (length(FUnavailableText) > 0) then
    begin
      if not(StyleServices.GetElementColor(LDetails, ecTextColor, LColor)) then
        LColor := GetSysColor(COLOR_BTNTEXT);

      FBitmap.Canvas.Font.Color := LColor;
      FBitmap.Canvas.TextRect(LRect, FUnavailableText, [tfVerticalCenter, tfCenter, tfSingleLine]);
    end;
end;

procedure TVisualStylePreview.DrawCaption;
var
  LClientRect     : TRect;
  LCaptionRect    : TRect;
  LTextRect       : TRect;
  LIconRect       : TRect;
  LButtonRect     : TRect;
  LDetails        : TThemedElementDetails;
  LCaptionDetails : TThemedElementDetails;
  LIconDetails    : TThemedElementDetails;
  LRegion         : HRGN;
begin
  LClientRect  := ClientRect;
  LCaptionRect := Rect(0, 0, FBitmap.Width, FFormBorderSize.Top);

  //Draw background
  LDetails.Element := teWindow;
  LDetails.Part    := 0;
  FStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, LClientRect);

  //Draw caption border
  LDetails := FStyle.GetElementDetails(twCaptionActive);

  LRegion := FRegion;
  try
    FStyle.GetElementRegion(LDetails, LClientRect, FRegion);
    SetWindowRgn(Handle, FRegion, True);
  finally
    if (LRegion <> 0) then DeleteObject(LRegion);
  end;

  FStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, LCaptionRect);
  LTextRect       := LCaptionRect;
  LCaptionDetails := LDetails;

  //Draw icon
  LIconDetails := FStyle.GetElementDetails(twSysButtonNormal);
  LIconRect    := Rect(0, 0, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON));

  if not(FStyle.GetElementContentRect(0, LIconDetails, LCaptionRect, LButtonRect)) then
    LButtonRect := Rect(0, 0, 0, 0);

  RectVCenter(LIconRect, LButtonRect);

  if (LButtonRect.Width > 0) and (FIcon <> 0) then
    DrawIconEx(FBitmap.Canvas.Handle, LIconRect.Left, LIconRect.Top, FIcon, 0, 0, 0, 0, DI_NORMAL);

  Inc(LTextRect.Left, LButtonRect.Width + MulDiv(5, screen.PixelsPerInch, ORIGINAL_PPI));

  //Draw buttons

  //Close button
  LDetails := FStyle.GetElementDetails(twCloseButtonNormal);
  if FStyle.GetElementContentRect(0, LDetails, LCaptionRect, LButtonRect) then
   FStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, LButtonRect);

  //Maximize button
  LDetails := FStyle.GetElementDetails(twMaxButtonNormal);
  if FStyle.GetElementContentRect(0, LDetails, LCaptionRect, LButtonRect) then
    FStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, LButtonRect);

  //Minimize button
  LDetails := FStyle.GetElementDetails(twMinButtonNormal);

  if FStyle.GetElementContentRect(0, LDetails, LCaptionRect, LButtonRect) then
    FStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, LButtonRect);

  //Help button
  LDetails := FStyle.GetElementDetails(twHelpButtonNormal);
  if FStyle.GetElementContentRect(0, LDetails, LCaptionRect, LButtonRect) then
    FStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, LButtonRect);

  if (LButtonRect.Left > 0) then LTextRect.Right := LButtonRect.Left;

  //Draw text
  FStyle.DrawText(FBitmap.Canvas.Handle, LCaptionDetails, FCaption, LTextRect, [tfLeft, tfSingleLine, tfVerticalCenter]);
end;

procedure TVisualStylePreview.DrawFormBorders;
var
  LRect    : TRect;
  LDetails : TThemedElementDetails;
begin
  //Draw left border
  LRect    := Rect(0, FFormBorderSize.Top, FFormBorderSize.Left, FBitmap.Height - FFormBorderSize.Bottom);
  LDetails := FStyle.GetElementDetails(twFrameLeftActive);
  FStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, LRect);

  //Draw right border
  LRect    := Rect(FBitmap.Width - FFormBorderSize.Right, FFormBorderSize.Top, FBitmap.Width, FBitmap.Height - FFormBorderSize.Bottom);
  LDetails := FStyle.GetElementDetails(twFrameRightActive);
  FStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, LRect);

  //Draw Bottom border
  LRect    := Rect(0, FBitmap.Height - FFormBorderSize.Bottom, FBitmap.Width, FBitmap.Height);
  LDetails := FStyle.GetElementDetails(twFrameBottomActive);
  FStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, LRect);
end;

procedure TVisualStylePreview.DrawMainMenu;
const
  MENU_ITEM_WIDTH = 30;
var
  LMenuRect : TRect;
  LItemRect : TRect;
  LDetails  : TThemedElementDetails;
  LColor    : TColor;
  LWidth    : integer;
begin
  LMenuRect := GetMainMenuRect;

  LDetails := FStyle.GetElementDetails(tmMenuBarBackgroundActive);
  FStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, LMenuRect);

  LDetails := FStyle.GetElementDetails(tmMenuBarItemNormal);
  FStyle.GetElementColor(LDetails, ecTextColor, LColor);

  LWidth := MulDiv(MENU_ITEM_WIDTH, screen.PixelsPerInch, ORIGINAL_PPI);

  LItemRect.Left   := LMenuRect.Left + MulDiv(10, screen.PixelsPerInch, ORIGINAL_PPI);
  LItemRect.Top    := LMenuRect.Top  + MulDiv(3, screen.PixelsPerInch, ORIGINAL_PPI);
  LItemRect.Right  := LItemRect.Left + LWidth;
  LItemRect.Bottom := LMenuRect.Bottom;
  FStyle.DrawText(FBitmap.Canvas.Handle, LDetails, FFileMenuText, LItemRect, [tfLeft], LColor);

  LItemRect.Left   := LItemRect.Right;
  LItemRect.Right  := LItemRect.Left + LWidth;
  FStyle.DrawText(FBitmap.Canvas.Handle, LDetails, FEditMenuText, LItemRect, [tfLeft], LColor);

  LItemRect.Left   := LItemRect.Right;
  LItemRect.Right  := LItemRect.Left + LWidth;
  FStyle.DrawText(FBitmap.Canvas.Handle, LDetails, FViewMenuText, LItemRect, [tfLeft], LColor);

  LItemRect.Left   := LItemRect.Right;
  LItemRect.Right  := LItemRect.Left + LWidth;
  FStyle.DrawText(FBitmap.Canvas.Handle, LDetails, FHelpMenuText, LItemRect, [tfLeft], LColor);
end;

procedure TVisualStylePreview.DrawToolButtons;
const
  BUTTON_WIDTH  = 75;
  BUTTON_HEIGHT = 25;
  PANEL_PADDING = 10;
var
  LMenuRect   : TRect;
  LButtonRect : TRect;
  LDetails    : TThemedElementDetails;
  LColor      : TColor;
  i           : integer;
  LWidth      : integer;
  LHeight     : integer;
  LPadding    : integer;
begin
  LMenuRect := GetMainMenuRect;
  LWidth    := MulDiv(BUTTON_WIDTH,  screen.PixelsPerInch, ORIGINAL_PPI);
  LHeight   := MulDiv(BUTTON_HEIGHT, screen.PixelsPerInch, ORIGINAL_PPI);
  LPadding  := MulDiv(PANEL_PADDING, screen.PixelsPerInch, ORIGINAL_PPI);

  LButtonRect.Left   := FFormBorderSize.Left + LPadding;
  LButtonRect.Top    := LMenuRect.Bottom + LPadding;
  LButtonRect.Right  := LButtonRect.Left + LWidth;
  LButtonRect.Bottom := LButtonRect.Top  + LHeight;

  for i := 1 to 3 do
    begin
      LDetails := FStyle.GetElementDetails(ttbButtonNormal);
      FStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, LButtonRect);

      FStyle.GetElementColor(LDetails, ecTextColor, LColor);
      FStyle.DrawText(FBitmap.Canvas.Handle, LDetails, FButtonText + IntToStr(i), LButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), LColor);

      LButtonRect.Left  := LButtonRect.Right;
      LButtonRect.Right := LButtonRect.Left + LWidth;
    end;
end;

procedure TVisualStylePreview.DrawButtons;
const
  BUTTON_WIDTH  = 75;
  BUTTON_HEIGHT = 25;
  PANEL_PADDING = 10;
var
  LButtonRect : TRect;
  LDetails    : TThemedElementDetails;
  LColor      : TColor;
  i           : integer;
  LCaption    : string;
  LWidth      : integer;
  LHeight     : integer;
  LPadding    : integer;
begin
  LWidth    := MulDiv(BUTTON_WIDTH,  screen.PixelsPerInch, ORIGINAL_PPI);
  LHeight   := MulDiv(BUTTON_HEIGHT, screen.PixelsPerInch, ORIGINAL_PPI);
  LPadding  := MulDiv(PANEL_PADDING, screen.PixelsPerInch, ORIGINAL_PPI);

  LButtonRect.Left   := FFormBorderSize.Left + LPadding;
  LButtonRect.Right  := LButtonRect.Left + LWidth;
  LButtonRect.Bottom := FBitmap.Height - FFormBorderSize.Bottom - LPadding;
  LButtonRect.Top    := LButtonRect.Bottom - LHeight;

  for i := 1 to 4 do
    begin
      case i of
        1 :
          begin
            LDetails := FStyle.GetElementDetails(tbPushButtonNormal);
            LCaption := FNormalText;
          end;

        2 :
          begin
            LDetails := FStyle.GetElementDetails(tbPushButtonHot);
            LCaption := FHotText;
          end;

        3 :
          begin
            LDetails := FStyle.GetElementDetails(tbPushButtonPressed);
            LCaption := FPressedText;
          end;

        4 :
          begin
            LDetails := FStyle.GetElementDetails(tbPushButtonDisabled);
            LCaption := FDisabledText;
          end;
      end;

      FStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, LButtonRect);
      FStyle.GetElementColor(LDetails, ecTextColor, LColor);
      FStyle.DrawText(FBitmap.Canvas.Handle, LDetails, LCaption, LButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), LColor);

      LButtonRect.Left  := LButtonRect.Right + LPadding;
      LButtonRect.Right := LButtonRect.Left  + LWidth;
    end;
end;

procedure TVisualStylePreview.DrawTabs;
const
  TAB_WIDTH  = 80;
  TAB_OFFSET = 3;
var
  LDetails  : TThemedElementDetails;
  LTabsRect : TRect;
  LItemRect : TRect;
  LWidth    : integer;
  LColor    : TColor;
  LFlags    : TTextFormat;
  LOffset   : integer;
begin
  LWidth    := MulDiv(TAB_WIDTH,  screen.PixelsPerInch, ORIGINAL_PPI);
  LOffset   := MulDiv(TAB_OFFSET, screen.PixelsPerInch, ORIGINAL_PPI);
  LTabsRect := GetTabsRect;
  LFlags    := TTextFormatFlags(DT_VCENTER or DT_CENTER);
  LColor    := clBlack;

  // Tabs background
  LDetails := StyleServices.GetElementDetails(ttPane);
  FStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, LTabsRect);


  // Selected tab
  LItemRect       := LTabsRect;
  LItemRect.Right := LItemRect.Left + LWidth;

  LDetails := StyleServices.GetElementDetails(ttTabItemSelected);
  FStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, LItemRect);
  FStyle.GetElementColor(LDetails, ecTextColor, LColor);
  FStyle.DrawText(FBitmap.Canvas.Handle, LDetails, FSelectedText, LItemRect, LFlags, LColor);


  // Hot tab
  LItemRect.Left  := succ(LItemRect.Right);
  LItemRect.Right := LItemRect.Left + LWidth;
  LItemRect.Top   := LTabsRect.Top  + LOffset;  // unselected tabs are slightly shorter

  LDetails := StyleServices.GetElementDetails(ttTabItemHot);
  FStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, LItemRect);
  FStyle.GetElementColor(LDetails, ecTextColor, LColor);
  FStyle.DrawText(FBitmap.Canvas.Handle, LDetails, FHotText, LItemRect, LFlags, LColor);


  // Normal tab
  LItemRect.Left  := succ(LItemRect.Right);
  LItemRect.Right := LItemRect.Left + LWidth;

  LDetails := StyleServices.GetElementDetails(ttTabItemNormal);
  FStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, LItemRect);
  FStyle.GetElementColor(LDetails, ecTextColor, LColor);
  FStyle.DrawText(FBitmap.Canvas.Handle, LDetails, FNormalText, LItemRect, LFlags, LColor);
end;

procedure TVisualStylePreview.DrawOriginalPreview;
begin
  FFormBorderSize := GetFormBorderSize;

  DrawCaption;
  DrawFormBorders;
  DrawMainMenu;
  DrawToolButtons;
  DrawButtons;
end;

procedure TVisualStylePreview.DrawTabsPreview;
begin
  FFormBorderSize := GetFormBorderSize;

  DrawCaption;
  DrawFormBorders;
  DrawTabs;
  DrawButtons;
end;

procedure TVisualStylePreview.Paint;
begin
  FBitmap.SetSize(ClientRect.Width, ClientRect.Height);

  if (FStyle = nil) then
    DrawDefaultPanel
   else
    case FPreviewType of
      ptOriginal : DrawOriginalPreview;
      ptTabs     : DrawTabsPreview;
    end;

  Canvas.Draw(0, 0, FBitmap);
end;

end.
