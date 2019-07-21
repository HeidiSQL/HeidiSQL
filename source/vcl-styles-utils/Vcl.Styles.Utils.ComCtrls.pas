//**************************************************************************************************
//
// Unit Vcl.Styles.Utils.ComCtrls
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
//************************************************************************************************
unit Vcl.Styles.Utils.ComCtrls;

{$I VCL.Styles.Utils.inc}

interface

uses
  System.Classes,
  System.Types,
  System.SysUtils,
  Winapi.Windows,
  Winapi.Messages,
  Winapi.CommCtrl,
  Winapi.RichEdit,
  Vcl.Styles,
  Vcl.Themes,
  Vcl.Graphics,
  {$IFDEF USE_Vcl.Styles.Hooks}
  Vcl.Styles.Hooks,
  {$ENDIF}
  Vcl.Styles.Utils.SysStyleHook,
  Vcl.Styles.Utils.StdCtrls,
  Vcl.Forms,
  Vcl.ImgList,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.Styles.Utils.Forms,
  Vcl.Controls;

type
  TSysListViewStyleHook = class(TSysScrollingStyleHook)
  private type
{$REGION 'TSysHeaderStyleHook'}
    TSysHeaderStyleHook = class(TMouseTrackSysControlStyleHook)
    private type
{$REGION 'TSysSection'}
      TSysSection = class
      private
        FIndex: Integer;
        FColumnIndex: Integer;
        FImageIndex: Integer;
        FImageListHandle: THandle;
        FText: String;
        FSectionRect: TRect;
        FHeaderHandle: THandle;
        FHasSplitButton: Boolean;
        FTextFormat: TTextFormat;
        FBitmapOnRight: Boolean;
        FShowImage: Boolean;
        FDropDownRect: TRect;
      protected
        procedure DoGetSectionInfo;
      public
        constructor Create(SysParent: TSysControl; Index: Integer); virtual;
        Destructor Destroy; override;
        property Text: string read FText;
        property ImageListHandle: THandle read FImageListHandle;
        property ImageIndex: Integer read FImageIndex;
        property SectionRect: TRect read FSectionRect;
        property ColumnIndex: Integer read FColumnIndex;
        property ShowImage: Boolean read FShowImage;
        property BitmapOnRight: Boolean read FBitmapOnRight;
        property TextFormat: TTextFormat read FTextFormat;
        property HasSplitButton: Boolean read FHasSplitButton;
        property DropDownRect: TRect read FDropDownRect;
      end;
{$ENDREGION}
    private
      FPressedSection: Integer;
      FMouseDown: Boolean;
      FSysSection: TSysSection;
      FListViewStyleHook: TSysListViewStyleHook;
      function GetButtonsCount: Integer;
      function GetItem(Index: Integer): TSysSection;
    protected
      procedure MouseLeave; override;
      procedure WndProc(var Message: TMessage); override;
      procedure Paint(Canvas: TCanvas); override;
      procedure PaintBackground(Canvas: TCanvas); override;

    public
      constructor Create(AHandle: THandle); override;
      Destructor Destroy; override;
      property ButtonsCount: Integer read GetButtonsCount;
      property Items[Index: Integer]: TSysSection read GetItem;
    end;
{$ENDREGION}
  private
    FHeaderHandle: THandle;
    FHeaderStyleHook: TSysHeaderStyleHook;
  protected
    procedure Scroll(const Kind: TScrollBarKind; const ScrollType: TSysScrollingType; Pos, Delta: Integer); override;
    procedure UpdateColors; override;
    procedure WndProc(var Message: TMessage); override;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure Paint(Canvas: TCanvas); override;

  public
    procedure SetSelectedColumn(iCol: Integer);
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
    property HeaderHandle: THandle read FHeaderHandle write FHeaderHandle;
  end;

  TSysTreeViewStyleHook = class(TSysScrollingStyleHook)
  protected
    procedure Scroll(const Kind: TScrollBarKind; const ScrollType: TSysScrollingType; Pos, Delta: Integer); override;
    procedure UpdateColors; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
  end;

  TSysTabControlStyleHook = class(TMouseTrackSysControlStyleHook)
  private
    FHotTabIndex: Integer;
    function GetDisplayRect: TRect;
    function GetTabCount: Integer;
    function GetTabIndex: Integer;
    function GetImages: TCustomImageList;
    function GetTabRect(Index: Integer): TRect;
    function GetTabPosition: TTabPosition;
    function GetTabs(Index: Integer): string;
    procedure AngleTextOut(Canvas: TCanvas; const Angle, X, Y: Integer; const Text: string);
  protected
    procedure DrawTab(Canvas: TCanvas; const Index: Integer);
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure Paint(Canvas: TCanvas); override;
    procedure PaintNC(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
    property DisplayRect: TRect read GetDisplayRect;
    property TabCount: Integer read GetTabCount;
    property TabIndex: Integer read GetTabIndex;
    property Images: TCustomImageList read GetImages;
    property TabRect[Index: Integer]: TRect read GetTabRect;
    property TabPosition: TTabPosition read GetTabPosition;
    property Tabs[Index: Integer]: string read GetTabs;
  end;

  TSysRichEditStyleHook = class(TSysScrollingStyleHook)
  strict private

    procedure EMSetBkgndColor(var Message: TMessage); message EM_SETBKGNDCOLOR;
    procedure EMSetCharFormat(var Message: TMessage); message EM_SETCHARFORMAT;
  strict private
    FBackColor: TColor;
  protected
    procedure UpdateColors; override;
    procedure WndProc(var Message: TMessage); override;
    function GetBorderSize: TRect; override;
  public
    property BackColor: TColor read FBackColor write FBackColor;
    constructor Create(AHandle: THandle); override;
  end;

type
  TSysToolbarButtonState = set of (bsEnabled, bsPressed, bsChecked, bsHidden);
  TSysToolbarButtonStyle = set of (bsBtn, bsSep, bsCheck, bsGroup, bsCheckGroup, bsDropDown);

  TSysReBarStyleHook = class(TSysStyleHook)
  strict private
    function GetBandText(const Index: Integer): string;
    function GetBandRect(const Index: Integer): TRect;
    function GetBandBorder(const Index: Integer): TRect;
    function GetBandCount: Integer;
  strict protected
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure Paint(Canvas: TCanvas); override;
    procedure PaintNC(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
  end;

  TSysStatusBarStyleHook = class(TSysStyleHook)
  strict protected
    procedure Paint(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
  end;

  TSysTrackBarStyleHook = class(TSysStyleHook)
  strict private
    FMouseOnThumb: Boolean;
    FThumbPressed: Boolean;
  strict protected
    procedure Paint(Canvas: TCanvas); override;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
  end;

  TSysToolbarStyleHook = class(TMouseTrackSysControlStyleHook)
  private type
{$REGION 'TSysToolbarButton'}
    TSysToolbarButton = class
    private
      FParent: TSysControl;
      FIndex: Integer;
      FText: String;
      FImageIndex: Integer;
      FState: TSysToolbarButtonState;
      FStyle: TSysToolbarButtonStyle;
      function GetItemRect: TRect;
      procedure DoGetItemInfo;
      function GetDropDownWidth: Integer;
    public
      constructor Create(SysParent: TSysControl; Index: Integer); virtual;
      Destructor Destroy; override;
      property ItemRect: TRect read GetItemRect;
      property Parent: TSysControl read FParent;
      property Text: String Read FText;
      Property ImageIndex: Integer read FImageIndex;
      property State: TSysToolbarButtonState read FState;
      property Style: TSysToolbarButtonStyle read FStyle;
      property DropDownWidth: Integer read GetDropDownWidth;
    end;
{$ENDREGION}

  var
    FImages: TImageList;
    FDisabledImages: TImageList;
    FSysToolbarButton: TSysToolbarButton;
    FButtonsPainted: Boolean;
    function GetItem(Index: Integer): TSysToolbarButton;
    function GetCount: Integer;
    function IsToolbarTransparent: Boolean;
    function IsToolbarFlat: Boolean;
    function GetShowText: Boolean;
    function IsToolbarList: Boolean;
    function IsToolbarWrapable: Boolean;
  protected
    procedure ApplyImageList;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure Paint(Canvas: TCanvas); override;
    procedure PaintNC(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
    property Items[index: Integer]: TSysToolbarButton read GetItem;
    property Count: Integer read GetCount;
    Property Flat: Boolean Read IsToolbarFlat;
    Property Transparent: Boolean Read IsToolbarTransparent;
    property ShowText: Boolean read GetShowText;
    property List: Boolean read IsToolbarList;
    property Wrapable: Boolean read IsToolbarWrapable;
  end;

  TSysProgressBarStyleHook = class(TSysStyleHook)
  strict private
    FStep: Integer;
    // FLastPos : Integer;
    FOrientation: TProgressBarOrientation;
    FTimer: TTimer;
    procedure TimerAction(Sender: TObject);
    function GetBarRect: TRect;
    function GetBorderWidth: Integer;
    function GetMax: Integer;
    function GetMin: Integer;
    function GetOrientation: TProgressBarOrientation;
    function GetPercent: Single;
    function GetPosition: Integer;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
  strict protected
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure PaintBar(Canvas: TCanvas); virtual;
    procedure PaintFrame(Canvas: TCanvas); virtual;
    procedure Paint(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
    property BarRect: TRect read GetBarRect;
    property BorderWidth: Integer read GetBorderWidth;
    property Max: Integer read GetMax;
    property Min: Integer read GetMin;
    property Orientation: TProgressBarOrientation read GetOrientation;
    property Position: Integer read GetPosition;
  public
    constructor Create(AHandle: THandle); override;
    destructor Destroy; override;
  end;

  TSysUpDownStyleHook = class(TMouseTrackSysControlStyleHook)
  strict private
    FLeftPressed, FRightPressed: Boolean;
    FMouseOnLeft, FMouseOnRight: Boolean;
    function GetOrientation: TUDOrientation;
    procedure WMLButtonDblClk(var Message: TWMMouse); message WM_LBUTTONDBLCLK;
    procedure WMLButtonDown(var Message: TWMMouse); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMMouse); message WM_LBUTTONUP;
    procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
  protected
    procedure Paint(Canvas: TCanvas); override;
    procedure MouseLeave; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    destructor Destroy; override;
  end;

  TSysLinkStyleHook = class(TSysStaticStyleHook)
  private
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
  protected
    procedure PaintNC(Canvas: TCanvas); override;
    procedure Paint(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
  end;

implementation

uses
  // IOUtils,
  Vcl.Styles.Utils.SysControls;

//
// procedure Addlog(const Msg: string);
// begin
// TFile.AppendAllText('C:\Test\log.txt',Format('%s %s %s',[FormatDateTime('hh:nn:ss.zzz', Now),  msg, sLineBreak]));
// end;

{ TSysListViewStyleHook }

constructor TSysListViewStyleHook.Create(AHandle: THandle);
begin
  inherited;
  FHeaderStyleHook := nil;
  FHeaderHandle := 0;
{$IF CompilerVersion > 23}
  StyleElements := [seFont, seBorder];
{$ELSE}
  OverridePaint := False;
  OverridePaintNC := True;
  OverrideFont := True;
{$IFEND}
  OverrideEraseBkgnd := True;
  SendMessage(Handle, WM_NOTIFY, 0, 0);
end;

procedure TSysListViewStyleHook.Scroll(const Kind: TScrollBarKind; const ScrollType: TSysScrollingType; Pos, Delta: Integer);
var
  R: TRect;
begin
  if ScrollType = skTracking then
  begin
    if Kind = sbVertical then
    begin
      if ListView_GetView(Handle) = LVS_REPORT then
      begin
        R := Rect(0, 0, 0, 0);
        ListView_GetItemRect(Handle, 0, R, LVIR_BOUNDS);
        Delta := Delta * R.Height;
      end;
      ListView_Scroll(Handle, 0, Delta);
    end;
    if Kind = sbHorizontal then
    begin
      if ListView_GetView(Handle) = LVS_LIST then
      begin
        R := TRect.Empty;
        ListView_GetItemRect(Handle, 0, R, LVIR_BOUNDS);
        Delta := Delta * R.Width;
      end;
      ListView_Scroll(Handle, Delta, 0);
    end;
  end
  else
    inherited;
end;

procedure TSysListViewStyleHook.SetSelectedColumn(iCol: Integer);
begin
  ListView_SetSelectedColumn(Handle, iCol);
end;

destructor TSysListViewStyleHook.Destroy;
begin
  if Assigned(FHeaderStyleHook) then
    FreeAndNil(FHeaderStyleHook);
  inherited;
end;

procedure TSysListViewStyleHook.UpdateColors;
begin
  inherited;
  if OverrideEraseBkgnd then
    Color := StyleServices.GetStyleColor(scListView)
  else
    Color := clWindow;
  if OverrideFont then
    FontColor := StyleServices.GetSystemColor(clWindowText)
  else
    FontColor := clWindowText;

  ListView_SetBkColor(Handle, ColorToRGB(Color));
  ListView_SetTextBkColor(Handle, ColorToRGB(Color));
  ListView_SetTextColor(Handle, ColorToRGB(FontColor));

end;

procedure TSysListViewStyleHook.WndProc(var Message: TMessage);
begin
  case Message.Msg of

    WM_CREATE, LVM_UPDATE:
      begin
        Message.Result := CallDefaultProc(Message);
        UpdateColors;
        SetSelectedColumn(-1);
        Exit;
      end;

    WM_ERASEBKGND:
      begin
        UpdateColors;
        SetSelectedColumn(-1);
        Message.Result := CallDefaultProc(Message);
        Exit;
      end;

    WM_NOTIFY:
      begin
        if not Assigned(FHeaderStyleHook) then
        begin
          HeaderHandle := ListView_GetHeader(Handle);
          if (HeaderHandle <> 0) then
          begin
            FHeaderStyleHook := TSysHeaderStyleHook.Create(HeaderHandle);
            FHeaderStyleHook.FListViewStyleHook := Self;
          end;
        end;

        if (Message.WParam <> 0) or (Message.LParam <> 0) then
          Message.Result := CallDefaultProc(Message);
        Exit;
      end;
  else inherited;
  end;

end;

{ TSysListViewStyleHook.TSysHeaderStyleHook }

constructor TSysListViewStyleHook.TSysHeaderStyleHook.Create(AHandle: THandle);
begin
  inherited;
{$IF CompilerVersion > 23}
  StyleElements := [seClient];
{$ELSE}
  OverridePaint := True;
  OverridePaintNC := False;
  OverrideFont := False;
{$IFEND}
  FPressedSection := -1;
  FSysSection := nil;
end;

destructor TSysListViewStyleHook.TSysHeaderStyleHook.Destroy;
begin
  if Assigned(FSysSection) then
    FreeAndNil(FSysSection);
  inherited;
end;

function TSysListViewStyleHook.TSysHeaderStyleHook.GetButtonsCount: Integer;
begin
  Result := Header_GetItemCount(Handle);
end;

function TSysListViewStyleHook.TSysHeaderStyleHook.GetItem(Index: Integer): TSysSection;
begin
  Result := nil;
  if (Index > -1) and (index < ButtonsCount) then
  begin
    if Assigned(FSysSection) then
      FreeAndNil(FSysSection);
    FSysSection := TSysSection.Create(SysControl, Index);
    Result := FSysSection;
  end;
end;

procedure TSysListViewStyleHook.TSysHeaderStyleHook.MouseLeave;
begin
  Invalidate;
end;

procedure TSysListViewStyleHook.TSysHeaderStyleHook.Paint(Canvas: TCanvas);
var
  i: Integer;
  Bmp: TBitmap;
  LImageList: TImageList;
  R, TxtRect, ImgRect: TRect;
  LSectionRect: TRect;
  LTextFormat: TTextFormat;
  LText: String;
  LSplitDetails, LDetails: TThemedElementDetails;
  DC: HDC;
  SectionHot: Boolean;
  LDropDownRect: TRect;
  P: TPoint;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.SetSize(SysControl.Width, SysControl.Height);
    Bmp.Canvas.Brush.Color := Color;
    R := Rect(0, 0, Bmp.Width, Bmp.Height);
    Bmp.Canvas.FillRect(R);
    DC := Bmp.Canvas.Handle;

    LDetails := StyleServices.GetElementDetails(thHeaderItemNormal);
    StyleServices.DrawElement(DC, LDetails, R);

    for i := 0 to ButtonsCount - 1 do
    begin
      with Items[i] do
      begin
        LSectionRect := SectionRect;
        LTextFormat := TextFormat;
        LText := Text;
        LDropDownRect := DropDownRect;
      end;
      SectionHot := False;
      if (MouseInControl) and (not FMouseDown) then
      begin
        GetCursorPos(P);
        ScreenToClient(Handle, P);
        if LSectionRect.Contains(P) then
          SectionHot := True;
      end;

      LDetails := StyleServices.GetElementDetails(thHeaderItemNormal);
      if SectionHot then
        LDetails := StyleServices.GetElementDetails(thHeaderItemHot);
      if FPressedSection = i then
        LDetails := StyleServices.GetElementDetails(thHeaderItemPressed);
      StyleServices.DrawElement(DC, LDetails, LSectionRect);

      TxtRect := LSectionRect;
      inc(TxtRect.Left, 4);

      if Items[i].HasSplitButton then
      begin
        LSplitDetails := StyleServices.GetElementDetails(ttbDropDownButtonGlyphHot);;
        R := LDropDownRect;
        if SectionHot then
        begin
          StyleServices.DrawElement(DC, LSplitDetails, R);
          with Bmp.Canvas do
          begin
            Pen.Color := StyleServices.GetSystemColor(clBtnShadow);
            MoveTo(R.Left, 3);
            LineTo(R.Left, R.Height - 3);
            Pen.Color := StyleServices.GetSystemColor(clBtnHighLight);
            MoveTo(R.Left - 1, 3);
            LineTo(R.Left - 1, R.Height - 3);
          end;
        end;
        dec(TxtRect.Right, R.Width);
      end;

      if (Items[i].ShowImage) and (Items[i].ImageListHandle > 0) then
      begin
        LImageList := TImageList.Create(nil);
        try
          LImageList.Handle := Items[i].ImageListHandle;
          LImageList.Masked := True;
          LImageList.BkColor := clNone; { Transparent bitmap }
          R := LSectionRect;
          ImgRect := Rect(0, 0, LImageList.Width, LImageList.Height);
          ImgRect := RectCenter(ImgRect, R);
          if not Items[i].BitmapOnRight then
          begin
            ImgRect.Left := R.Left + 2;
            ImgRect.Right := ImgRect.Left + 2 + LImageList.Width;
            inc(TxtRect.Left, ImgRect.Width + 2);
          end
          else
          begin
            ImgRect.Left := LSectionRect.Right - LImageList.Width - 2;
            ImgRect.Right := LSectionRect.Right;
            TxtRect.Right := TxtRect.Right - ImgRect.Width - 2;
          end;
          LImageList.Draw(Bmp.Canvas, ImgRect.Left, ImgRect.Top, Items[i].ImageIndex);
        finally
          LImageList.Free;
        end;
      end;

      include(LTextFormat, tfSingleLine);
      include(LTextFormat, tfVerticalCenter);
      StyleServices.DrawText(DC, LDetails, LText, TxtRect, LTextFormat);
    end;
    Canvas.Draw(0, 0, Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TSysListViewStyleHook.TSysHeaderStyleHook.PaintBackground(Canvas: TCanvas);
begin
  // inherited;
  { Leave this block clean . }
end;

procedure TSysListViewStyleHook.Paint(Canvas: TCanvas);
begin
  { Leave this block clean . }
end;

procedure TSysListViewStyleHook.PaintBackground(Canvas: TCanvas);
begin
  { Leave this block clean . }
end;

procedure TSysListViewStyleHook.TSysHeaderStyleHook.WndProc(var Message: TMessage);
var
  Info: THDHitTestInfo;
begin
  case Message.Msg of

    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      begin
        FMouseDown := True;
        Info.Point.X := TWMMouse(Message).XPos;
        Info.Point.Y := TWMMouse(Message).YPos;
        SendMessage(Handle, HDM_HITTEST, 0, IntPtr(@Info));

        if (Info.Flags and HHT_ONDIVIDER = 0) and (Info.Flags and HHT_ONDIVOPEN = 0) then
          FPressedSection := Info.item
        else
          FPressedSection := -1;
      end;

    WM_LBUTTONUP, WM_RBUTTONUP:
      begin
        FMouseDown := False;
        FPressedSection := -1;
      end;

  end;
  inherited;

end;

{ TSysListViewStyleHook.TSysHeaderStyleHook.TSysSection }

constructor TSysListViewStyleHook.TSysHeaderStyleHook.TSysSection.Create(SysParent: TSysControl; Index: Integer);
begin
  inherited Create;
  FTextFormat := [];
  FIndex := Index;
  FText := '';
  FImageListHandle := 0;
  FImageIndex := -1;
  FColumnIndex := -1;
  FSectionRect := TRect.Empty;
  FDropDownRect := TRect.Empty;
  FHasSplitButton := False;
  FShowImage := False;
  FHeaderHandle := SysParent.Handle;
  DoGetSectionInfo;
end;

destructor TSysListViewStyleHook.TSysHeaderStyleHook.TSysSection.Destroy;
begin

  inherited;
end;

procedure TSysListViewStyleHook.TSysHeaderStyleHook.TSysSection.DoGetSectionInfo;
var
  SectionOrder: array of Integer;
  R: TRect;
  item: THDItem;
  Buffer: array [0 .. 255] of Char;
  LRtlReading: Boolean;
begin
  FillChar(Buffer, 255, Char(0));
  SetLength(SectionOrder, Header_GetItemCount(FHeaderHandle));
  Header_GetOrderArray(FHeaderHandle, Header_GetItemCount(FHeaderHandle), Pointer(SectionOrder));
  FColumnIndex := SectionOrder[FIndex];
  Header_GetItemRect(FHeaderHandle, ColumnIndex, @R);
  FSectionRect := R;
  FillChar(item, sizeof(item), 0);
  item.mask := HDI_TEXT or HDI_FORMAT or HDI_IMAGE;
  item.pszText := @Buffer;
  item.cchTextMax := Length(Buffer);
  if Header_GetItem(FHeaderHandle, FColumnIndex, item) then
  begin
    with item do
    begin
      FImageIndex := iImage;
      FText := String(pszText);
      FHasSplitButton := (fmt and HDF_SPLITBUTTON = HDF_SPLITBUTTON);
      LRtlReading := (fmt and HDF_RTLREADING = HDF_RTLREADING);
      FTextFormat := [];
      if (fmt and HDF_LEFT = HDF_LEFT) then
        include(FTextFormat, tfLeft)
      else if (fmt and HDF_RIGHT = HDF_RIGHT) then
        include(FTextFormat, tfRight)
      else if (fmt and HDF_CENTER = HDF_CENTER) then
        include(FTextFormat, tfCenter);

      if LRtlReading then
        include(FTextFormat, tfRtlReading);
      FBitmapOnRight := (fmt and HDF_BITMAP_ON_RIGHT = HDF_BITMAP_ON_RIGHT);

      FShowImage := (FImageIndex > -1) and (fmt and HDF_BITMAP = HDF_BITMAP);
    end;
  end;
  R := TRect.Empty;
  if Header_GetItemDropDownRect(FHeaderHandle, FIndex, R) then
    FDropDownRect := R;
  FImageListHandle := Header_GetImageList(FHeaderHandle);
end;

{ TSysTreeViewStyleHook }


constructor TSysTreeViewStyleHook.Create(AHandle: THandle);
begin
  inherited;
{$IF CompilerVersion > 23}
  StyleElements := [seFont{, seBorder}];  //Allow to the Vcl.Styles.Hook handle the NC and scroll paint
{$ELSE}
  OverrideFont := True;
  OverridePaintNC := False; //Allow to the Vcl.Styles.Hook handle the NC and scroll paint
{$IFEND}
  OverrideEraseBkgnd := True;
end;

destructor TSysTreeViewStyleHook.Destroy;
begin

  inherited;
end;

procedure TSysTreeViewStyleHook.Scroll(const Kind: TScrollBarKind;
  const ScrollType: TSysScrollingType; Pos, Delta: Integer);
begin
  if Kind = sbVertical then
  begin
    case ScrollType of
      skTracking:
        begin
          LstPos := Pos;
          //OutputDebugString(PChar(Format('sbVertical Pos %d Delta %d AllowScrolling %s', [Pos, Delta, BooltoStr(AllowScrolling, True)])));
          AllowScrolling := True;
          SendMessage(Handle, WM_VSCROLL, MakeWParam(SB_THUMBTRACK, Pos), 0);
          AllowScrolling := False;
          //OutputDebugString(PChar(Format('sbVertical Pos %d Delta %d', [Pos, Delta])));
        end;
      skLineUp: SendMessage(Handle, WM_VSCROLL, SB_LINEUP, 0);
      skLineDown: SendMessage(Handle, WM_VSCROLL, SB_LINEDOWN, 0);
      skPageUp: SendMessage(Handle, WM_VSCROLL, SB_PAGEUP, 0);
      skPageDown: SendMessage(Handle, WM_VSCROLL, SB_PAGEDOWN, 0);
    end;
  end
  else
  if Kind = sbHorizontal then
  begin
    case ScrollType of
      skTracking:
        begin
          LstPos := Pos;
          //OutputDebugString(PChar(Format('sbHorizontal Pos %d Delta %d AllowScrolling %s', [Pos, Delta, BooltoStr(AllowScrolling, True)])));
          AllowScrolling := True;
          SendMessage(Handle, WM_HSCROLL, MakeWParam(SB_THUMBTRACK, Pos), 0);
          AllowScrolling := False;
          //OutputDebugString(PChar(Format('sbHorizontal Pos %d Delta %d', [Pos, Delta])));
        end;
      skLineLeft: SendMessage(Handle, WM_HSCROLL, SB_LINELEFT, 0);
      skLineRight: SendMessage(Handle, WM_HSCROLL, SB_LINERIGHT, 0);
      skPageLeft: SendMessage(Handle, WM_HSCROLL, SB_PAGELEFT, 0);
      skPageRight: SendMessage(Handle, WM_HSCROLL, SB_PAGERIGHT, 0);
    end;
  end;
end;

procedure TSysTreeViewStyleHook.UpdateColors;
begin
  inherited;
  if OverrideEraseBkgnd then
    Color := StyleServices.GetStyleColor(scTreeView)
  else
    Color := clWhite;

  if OverrideFont then
    FontColor := StyleServices.GetSystemColor(clWindowText)
  else
    FontColor := clWindowText;
end;

procedure TSysTreeViewStyleHook.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_ERASEBKGND:
      begin
        UpdateColors;

        if (Longint(TreeView_GetBkColor(Handle))<>ColorToRGB(Color)) then
          TreeView_SetBkColor(Handle, ColorToRGB(Color));

        if (Longint(TreeView_GetTextColor(Handle))<>ColorToRGB(FontColor)) then
         TreeView_SetTextColor(Handle, ColorToRGB(FontColor));

        Message.Result := CallDefaultProc(Message);
        Exit;
      end;
  else inherited;
  end;
end;

{ TSysTabControlStyleHook }

procedure TSysTabControlStyleHook.AngleTextOut(Canvas: TCanvas; const Angle, X, Y: Integer; const Text: string);
var
  SaveIndex: Integer;
begin
  SaveIndex := SaveDC(Canvas.Handle);
  try
    SetBkMode(Canvas.Handle, Transparent);
    Canvas.Font.Orientation := Angle;
    Canvas.TextOut(X, Y, Text);
  finally
    RestoreDC(Canvas.Handle, SaveIndex);
  end;

end;

constructor TSysTabControlStyleHook.Create(AHandle: THandle);
begin
  inherited;
{$IF CompilerVersion > 23}
  StyleElements := [seClient, seFont];
{$ELSE}
  OverridePaint := True;
  OverridePaintNC := False;
  OverrideFont := True;
{$IFEND}
  // OverrideEraseBkgnd:=True;
  FHotTabIndex := -1;
end;

destructor TSysTabControlStyleHook.Destroy;
begin

  inherited;
end;

function TSysTabControlStyleHook.GetDisplayRect: TRect;
begin
  //Result := Rect(0, 0, 0, 0);
  Result := SysControl.ClientRect;
  SendMessage(Handle, TCM_ADJUSTRECT, 0, IntPtr(@Result));
  inc(Result.Top, 2);
end;

function TSysTabControlStyleHook.GetImages: TCustomImageList;
begin
  Result := nil;
end;

function TSysTabControlStyleHook.GetTabCount: Integer;
begin
  Result := SendMessage(Handle, TCM_GETITEMCOUNT, 0, 0);
end;

function TSysTabControlStyleHook.GetTabIndex: Integer;
begin
  Result := SendMessage(Handle, TCM_GETCURSEL, 0, 0);
end;

function TSysTabControlStyleHook.GetTabPosition: TTabPosition;
begin
  Result := tpTop;
end;

function TSysTabControlStyleHook.GetTabRect(Index: Integer): TRect;
begin
  Result := Rect(0, 0, 0, 0);
  TabCtrl_GetItemRect(Handle, Index, Result);
end;

function TSysTabControlStyleHook.GetTabs(Index: Integer): string;
var
  TCItem: TTCItem;
  Buffer: array [0 .. 254] of Char;
begin
  FillChar(TCItem, sizeof(TCItem), 0);

  TCItem.mask := TCIF_TEXT;
  TCItem.pszText := @Buffer;
  TCItem.cchTextMax := sizeof(Buffer);
  if SendMessageW(Handle, TCM_GETITEMW, Index, IntPtr(@TCItem)) <> 0 then
    Result := TCItem.pszText
  else
    Result := '';

end;

procedure TSysTabControlStyleHook.Paint(Canvas: TCanvas);
var
  R: TRect;
  i, SaveIndex: Integer;
  Details: TThemedElementDetails;
begin
  SaveIndex := SaveDC(Canvas.Handle);
  try
    R := DisplayRect;
    ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    PaintBackground(Canvas);
  finally
    RestoreDC(Canvas.Handle, SaveIndex);
  end;
  { Draw tabs }
  for i := 0 to TabCount - 1 do
  begin
    // if I = TabIndex then
    // Continue;
    DrawTab(Canvas, i);
  end;
  case TabPosition of
    tpTop: InflateRect(R, SysControl.Width - R.Right, SysControl.Height - R.Bottom);
    tpLeft: InflateRect(R, SysControl.Width - R.Right, SysControl.Height - R.Bottom);
    tpBottom: InflateRect(R, R.Left, R.Top);
    tpRight: InflateRect(R, R.Left, R.Top);
  end;

  if StyleServices.Available then
  begin
    Details := StyleServices.GetElementDetails(ttPane);
    StyleServices.DrawElement(Canvas.Handle, Details, R);
  end;
  { Draw active tab }
  if TabIndex >= 0 then
    DrawTab(Canvas, TabIndex);

end;

procedure TSysTabControlStyleHook.DrawTab(Canvas: TCanvas; const Index: Integer);
var
  R, LayoutR, GlyphR: TRect;
  ImageWidth, ImageHeight, ImageStep, TX, TY: Integer;
  DrawState: TThemedTab;
  Details: TThemedElementDetails;
  ThemeTextColor: TColor;
  FImageIndex: Integer;
begin
  if (Images <> nil) and (Index < Images.Count) then
  begin
    ImageWidth := Images.Width;
    ImageHeight := Images.Height;
    ImageStep := 3;
  end
  else
  begin
    ImageWidth := 0;
    ImageHeight := 0;
    ImageStep := 0;
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
    dec(R.Left, 2)
  else
    dec(R.Right, 2);

  // Canvas.Font.Assign(TCustomTabControl(Control).Font);
  LayoutR := R;
  DrawState := ttTabDontCare;
  case TabPosition of
    tpTop:
      begin
        if Index = TabIndex then
          DrawState := ttTabItemSelected
        else if (Index = FHotTabIndex) and MouseInControl then
          DrawState := ttTabItemHot
        else
          DrawState := ttTabItemNormal;
      end;
    tpLeft:
      begin
        if Index = TabIndex then
          DrawState := ttTabItemLeftEdgeSelected
        else if (Index = FHotTabIndex) and MouseInControl then
          DrawState := ttTabItemLeftEdgeHot
        else
          DrawState := ttTabItemLeftEdgeNormal;
      end;
    tpBottom:
      begin
        if Index = TabIndex then
          DrawState := ttTabItemBothEdgeSelected
        else if (Index = FHotTabIndex) and MouseInControl then
          DrawState := ttTabItemBothEdgeHot
        else
          DrawState := ttTabItemBothEdgeNormal;
      end;
    tpRight:
      begin
        if Index = TabIndex then
          DrawState := ttTabItemRightEdgeSelected
        else if (Index = FHotTabIndex) and MouseInControl then
          DrawState := ttTabItemRightEdgeHot
        else
          DrawState := ttTabItemRightEdgeNormal;
      end;
  end;

  if StyleServices.Available then
  begin
    Details := StyleServices.GetElementDetails(DrawState);
    StyleServices.DrawElement(Canvas.Handle, Details, R);
  end;

  { Image }

  FImageIndex := Index;

  if (Images <> nil) and (FImageIndex >= 0) and (FImageIndex < Images.Count) then
  begin
    GlyphR := LayoutR;
    case TabPosition of
      tpTop, tpBottom:
        begin
          GlyphR.Left := GlyphR.Left + ImageStep;
          GlyphR.Right := GlyphR.Left + ImageWidth;
          LayoutR.Left := GlyphR.Right;
          GlyphR.Top := GlyphR.Top + (GlyphR.Bottom - GlyphR.Top) div 2 - ImageHeight div 2;
          if (TabPosition = tpTop) and (Index = TabIndex) then
            OffsetRect(GlyphR, 0, -1)
          else if (TabPosition = tpBottom) and (Index = TabIndex) then
            OffsetRect(GlyphR, 0, 1);
        end;
      tpLeft:
        begin
          GlyphR.Bottom := GlyphR.Bottom - ImageStep;
          GlyphR.Top := GlyphR.Bottom - ImageHeight;
          LayoutR.Bottom := GlyphR.Top;
          GlyphR.Left := GlyphR.Left + (GlyphR.Right - GlyphR.Left) div 2 - ImageWidth div 2;
        end;
      tpRight:
        begin
          GlyphR.Top := GlyphR.Top + ImageStep;
          GlyphR.Bottom := GlyphR.Top + ImageHeight;
          LayoutR.Top := GlyphR.Bottom;
          GlyphR.Left := GlyphR.Left + (GlyphR.Right - GlyphR.Left) div 2 - ImageWidth div 2;
        end;
    end;
    if StyleServices.Available then
      StyleServices.DrawIcon(Canvas.Handle, Details, GlyphR, Images.Handle, FImageIndex);
  end;

  { Text }
  if StyleServices.Available then
  begin
    if (TabPosition = tpTop) and (Index = TabIndex) then
      OffsetRect(LayoutR, 0, -1)
    else if (TabPosition = tpBottom) and (Index = TabIndex) then
      OffsetRect(LayoutR, 0, 1);

    if TabPosition = tpLeft then
    begin
      TX := LayoutR.Left + (LayoutR.Right - LayoutR.Left) div 2 - Canvas.TextHeight(Tabs[Index]) div 2;
      TY := LayoutR.Top + (LayoutR.Bottom - LayoutR.Top) div 2 + Canvas.TextWidth(Tabs[Index]) div 2;
      if StyleServices.GetElementColor(Details, ecTextColor, ThemeTextColor) then
        Canvas.Font.Color := ThemeTextColor;
      AngleTextOut(Canvas, 900, TX, TY, Tabs[Index]);
    end
    else if TabPosition = tpRight then
    begin
      TX := LayoutR.Left + (LayoutR.Right - LayoutR.Left) div 2 + Canvas.TextHeight(Tabs[Index]) div 2;
      TY := LayoutR.Top + (LayoutR.Bottom - LayoutR.Top) div 2 - Canvas.TextWidth(Tabs[Index]) div 2;
      if StyleServices.GetElementColor(Details, ecTextColor, ThemeTextColor) then
        Canvas.Font.Color := ThemeTextColor;
      AngleTextOut(Canvas, -900, TX, TY, Tabs[Index]);
    end
    else
      StyleServices.DrawText(Canvas.Handle, Details, Tabs[Index], LayoutR, [tfSingleLine, tfVerticalCenter, tfCenter, tfNoClip]);
    // DrawControlText(Canvas, Details, Tabs[Index], LayoutR,
    // DT_VCENTER or DT_CENTER or DT_SINGLELINE or DT_NOCLIP);
  end;
end;

procedure TSysTabControlStyleHook.PaintBackground(Canvas: TCanvas);
begin
  inherited;
end;

procedure TSysTabControlStyleHook.PaintNC(Canvas: TCanvas);
begin
  inherited;
end;

procedure TSysTabControlStyleHook.WndProc(var Message: TMessage);
begin
  // Addlog(Format('TSysTabControlStyleHook $0x%x %s', [SysControl.Handle, WM_To_String(Message.Msg)]));

  // case Message.Msg of
  // WM_MOUSEMOVE:
  // begin
  //
  // end;
  // else
  // inherited;
  // end;
  inherited;
end;

{ TSysToolbarStyleHook }

{$REGION 'TSysToolbarStyleHook'}

constructor TSysToolbarStyleHook.Create(AHandle: THandle);
begin
  inherited;
{$IF CompilerVersion > 23}
  StyleElements := [seClient, seFont];
{$ELSE}
  OverridePaint := True;
  OverrideFont := True;
{$IFEND}
  OverrideEraseBkgnd := False;
  FImages := nil;
  FDisabledImages := nil;
  FSysToolbarButton := nil;
  FButtonsPainted := False;
end;

destructor TSysToolbarStyleHook.Destroy;
begin
  if Assigned(FImages) then
    FreeAndNil(FImages);
  if Assigned(FDisabledImages) then
    FreeAndNil(FDisabledImages);
  if Assigned(FSysToolbarButton) then
    FreeAndNil(FSysToolbarButton);
  inherited;
end;

function TSysToolbarStyleHook.GetItem(Index: Integer): TSysToolbarButton;
begin
  Result := nil;
  if (Index > -1) and (index <= Count) then
  begin
    if Assigned(FSysToolbarButton) then
      FreeAndNil(FSysToolbarButton);
    FSysToolbarButton := TSysToolbarButton.Create(SysControl, Index);
    Result := FSysToolbarButton;
  end;
end;

function TSysToolbarStyleHook.GetShowText: Boolean;
begin
  Result := (SysControl.Style and BTNS_SHOWTEXT = BTNS_SHOWTEXT);
end;

function TSysToolbarStyleHook.IsToolbarFlat: Boolean;
begin
  { MSDN :
    In a flat toolbar, both the toolbar and the buttons are transparent
    and hot-tracking is enabled.
  }
  Result := (SysControl.Style and TBSTYLE_FLAT = TBSTYLE_FLAT)
end;

function TSysToolbarStyleHook.IsToolbarList: Boolean;
begin
  Result := (SysControl.Style and TBSTYLE_LIST = TBSTYLE_LIST);
end;

function TSysToolbarStyleHook.IsToolbarTransparent: Boolean;
begin
  { MSDN:
    In a transparent toolbar, the toolbar is transparent but the buttons are not.
  }
  Result := (SysControl.Style and TBSTYLE_TRANSPARENT = TBSTYLE_TRANSPARENT)
end;

function TSysToolbarStyleHook.IsToolbarWrapable: Boolean;
begin
  Result := (SysControl.Style and TBSTYLE_WRAPABLE = TBSTYLE_WRAPABLE)
end;

function TSysToolbarStyleHook.GetCount: Integer;
begin
  Result := SendMessage(Handle, TB_BUTTONCOUNT, 0, 0);
end;

procedure TSysToolbarStyleHook.ApplyImageList;
var
  H: Cardinal;
begin
  H := SendMessage(Handle, TB_GETIMAGELIST, 0, 0);
  if (H <> 0) and (FImages = nil) then
  begin
    FImages := TImageList.Create(nil);
    FImages.ShareImages := True;
    FImages.Handle := H;
  end;
  H := SendMessage(Handle, TB_GETDISABLEDIMAGELIST, 0, 0);
  if (H <> 0) and (FDisabledImages = nil) then
  begin
    FDisabledImages := TImageList.Create(nil);
    FDisabledImages.ShareImages := True;
    FDisabledImages.Handle := H;
  end;
end;

procedure TSysToolbarStyleHook.Paint(Canvas: TCanvas);
var
  i: Integer;
  ItemRect, R, R2: TRect;
  LDetails: TThemedElementDetails;
  DC: HDC;
  LButtonHot: Boolean;
  P: TPoint;
  LStyle: TSysToolbarButtonStyle;
  LState: TSysToolbarButtonState;
  Bmp: TBitmap;
  ImgRect, TxtRect: TRect;
  LText: String;
  LImageIndex, LDropDownWidth: Integer;
  TxtFlags: DWORD;
  TxtFormat: TTextFormat;
begin

  Bmp := TBitmap.Create;
  try
    ApplyImageList;
    if Assigned(FImages) then
    begin
      FImages.Masked := True;
      FImages.BkColor := clNone; { Transparent bitmap }
    end;
    ImgRect := Rect(0, 0, 0, 0);
    TxtRect := Rect(0, 0, 0, 0);
    Bmp.SetSize(SysControl.Width, SysControl.Height);
    R := Rect(0, 0, Bmp.Width, Bmp.Height);
    // Bmp.Canvas.Brush.Color := StyleServices.GetStyleColor(scWindow);
    // Bmp.Canvas.FillRect(R);
    DC := Bmp.Canvas.Handle;
    DrawParentBackground(DC);

    TxtFlags := 0;
    if (SysControl.Style and TBSTYLE_NOPREFIX = TBSTYLE_NOPREFIX) then
      TxtFlags := DT_NOPREFIX;

    if Flat or Transparent then
    begin
      { Dont paint the toolbar background => the toolbar is transparent . }
    end
    else
    begin
      { Toolbar is not transparent }
      LDetails.Element := teToolBar;
      LDetails.Part := 0;
      LDetails.State := 0;
      if StyleServices.HasTransparentParts(LDetails) then
        StyleServices.DrawParentBackground(Handle, DC, LDetails, False);
      StyleServices.DrawElement(DC, LDetails, R);
    end;
  except
    Bmp.Free;
    Exit;
  end;

  try
    { Draw toolbar buttons }
    for i := 0 to Count - 1 do
    begin
      if i = Count - 1 then
        FButtonsPainted := True;

      ItemRect := Items[i].ItemRect;
      with Items[i] do
      begin
        LState := State;
        LStyle := Style;
        LText := Text;
        LImageIndex := ImageIndex;
        LDropDownWidth := DropDownWidth;
      end;

      LButtonHot := False;
      if not(bsHidden in LState) then
      begin
        if MouseInControl then
        begin
          GetCursorPos(P);
          ScreenToClient(Handle, P);
          if ItemRect.Contains(P) then
            LButtonHot := True;
        end;

        if (bsEnabled in LState) then
          LDetails := StyleServices.GetElementDetails(ttbButtonNormal)
        else
          LDetails := StyleServices.GetElementDetails(ttbButtonDisabled);
        if (LButtonHot) and (bsEnabled in LState) then
        begin
          LDetails := StyleServices.GetElementDetails(ttbButtonHot);
        end;
        if (bsPressed in LState) and (bsEnabled in LState) then
          LDetails := StyleServices.GetElementDetails(ttbButtonPressed);

        if bsChecked in LState then
          LDetails := StyleServices.GetElementDetails(ttbButtonChecked);

        if not(bsSep in LStyle) then
        begin
          if Flat then
          begin
            // Bmp.Canvas.FillRect(ItemRect);
            DrawParentBackground(DC, @ItemRect);
            if (LButtonHot or (bsPressed in LState) or (bsChecked in LState)) and (bsEnabled in LState) then
            begin
              StyleServices.DrawElement(DC, LDetails, ItemRect);
            end;
          end
          else
            StyleServices.DrawElement(DC, LDetails, ItemRect);
        end
        else
        begin
          LDetails := StyleServices.GetElementDetails(ttbSeparatorNormal);
          StyleServices.DrawElement(DC, LDetails, ItemRect);
        end;

        if not(bsSep in LStyle) then
        begin
          R := ItemRect;
          ImgRect := TRect.Empty;
          if Assigned(FImages) then
            ImgRect := Rect(0, 0, FImages.Width, FImages.Height);
          ImgRect := CenteredRect(R, ImgRect);

          if bsDropDown in LStyle then
          begin
            { If button is DropDown then draw the button glyph. }
            R := ItemRect;
            R := Rect(R.Right - LDropDownWidth, R.Top, R.Right, R.Bottom);
            if bsEnabled in LState then
              LDetails := StyleServices.GetElementDetails(ttbDropDownButtonGlyphNormal)
            else
              LDetails := StyleServices.GetElementDetails(ttbDropDownButtonGlyphDisabled);
            if (LButtonHot and (bsEnabled in LState)) then
              LDetails := StyleServices.GetElementDetails(ttbDropDownButtonGlyphHot);
            if ((bsPressed in LState) and (bsEnabled in LState)) then
              LDetails := StyleServices.GetElementDetails(ttbDropDownButtonGlyphPressed);
            StyleServices.DrawElement(DC, LDetails, R);

            { Adjust bitmap position }

            if Assigned(FImages) then
              ImgRect := Rect(0, 0, FImages.Width, FImages.Height);
            R := ItemRect;
            R.Right := R.Right - LDropDownWidth;
            ImgRect := CenteredRect(R, ImgRect);
            inc(ImgRect.Left, 2);
          end;

          { Adjust bitmap & Text positions }
          if Wrapable then
          begin
            R := Rect(0, 0, 0, 0);
            if (ShowText and not List) then
            begin
              Winapi.Windows.DrawText(DC, LText, -1, R, DT_CENTER or DT_CALCRECT);
            end;
            ImgRect.Offset(0, -R.Height);
          end
          else if List then
          begin
            R := Rect(0, 0, 0, 0);
            if ShowText then
            begin
              Winapi.Windows.DrawText(DC, LText, -1, R, DT_CENTER or DT_CALCRECT or TxtFlags);
            end;
            ImgRect := Rect(0, 0, FImages.Width, FImages.Height);
            R2 := ItemRect;
            dec(R2.Right, R.Width + 2);
            ImgRect := CenteredRect(R2, ImgRect);
          end;

          { Draw Bitmap }
          if (LImageIndex > -1) and (Assigned(FImages)) then
          begin
            if bsEnabled in LState then
              FImages.DrawingStyle := Vcl.ImgList.TDrawingStyle.dsNormal
            else
              FImages.DrawingStyle := Vcl.ImgList.TDrawingStyle.dsSelected;
            FImages.Draw(Bmp.Canvas, ImgRect.Left, ImgRect.Top, LImageIndex);
          end;

          { Draw Text }
          TxtRect := Rect(0, 0, 0, 0);
          if ShowText then
          begin
            if not List then
            begin
              { Text appear under the button bitmap }
              if (ImgRect.Width > 0) and (LImageIndex > -1) then
                TxtRect := Rect(ItemRect.Left, ImgRect.Bottom, ItemRect.Right, ItemRect.Bottom)
              else
                TxtRect := ItemRect;
              if LText <> '' then
                DrawTextCentered(DC, LDetails, TxtRect, LText, TxtFlags);
            end
            else
            begin
              { List }
              { Text appear to the right of the button bitmap }
              if (ImgRect.Width > 0) and (LImageIndex > -1) then
                TxtRect := Rect(ImgRect.Right + 2, ItemRect.Top, ItemRect.Right, ItemRect.Bottom)
              else
                TxtRect := ItemRect;
              TxtFormat := [tfCenter, tfVerticalCenter, tfSingleLine, tfLeft];
              if TxtFlags <> 0 then
                include(TxtFormat, tfNoPrefix);
              if LText <> '' then
                StyleServices.DrawText(DC, LDetails, LText, TxtRect, TxtFormat);
            end;
          end;
        end;
      end;
    end;
    Canvas.Draw(0, 0, Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TSysToolbarStyleHook.PaintBackground(Canvas: TCanvas);
begin
  inherited;

end;

procedure TSysToolbarStyleHook.PaintNC(Canvas: TCanvas);
begin
  inherited;
end;

procedure TSysToolbarStyleHook.WndProc(var Message: TMessage);
begin
  inherited;
end;
{$ENDREGION}
{$REGION 'TSysToolbarButton'}
{ TSysToolbarStyleHook.TSysToolbarButton }

constructor TSysToolbarStyleHook.TSysToolbarButton.Create(SysParent: TSysControl; Index: Integer);
begin
  FIndex := Index;
  FParent := SysParent;
  FText := '';
  FImageIndex := -1;
  FState := [];
  FStyle := [];
  DoGetItemInfo;
end;

destructor TSysToolbarStyleHook.TSysToolbarButton.Destroy;
begin
  inherited;
end;

Procedure TSysToolbarStyleHook.TSysToolbarButton.DoGetItemInfo;
const
  BufferSize = 255;
var
  TB: TTBButton;
  Buffer: array [0 .. BufferSize - 1] of Char;
  BtnInfo: TTBButtonInfo;
begin
  FillChar(Buffer, BufferSize, Char(0));
  FillChar(TB, sizeof(TB), 0);
  SendMessage(FParent.Handle, TB_GETBUTTON, FIndex, IntPtr(@TB));
  FillChar(BtnInfo, sizeof(BtnInfo), Char(0));
  BtnInfo.cbSize := sizeof(TTBButtonInfo);
  BtnInfo.dwMask := TBIF_STATE or TBIF_STYLE or TBIF_IMAGE or TBIF_TEXT;
  BtnInfo.cchText := BufferSize;
  BtnInfo.pszText := @Buffer;
  SendMessage(FParent.Handle, TB_GETBUTTONINFO, TB.idCommand, LParam(@BtnInfo));
  BtnInfo.fsStyle := TB.fsStyle;
  SendMessage(FParent.Handle, TB_GETBUTTONTEXT, TB.idCommand, LParam(BtnInfo.pszText));
  FText := String(Buffer);
  FImageIndex := BtnInfo.iImage;
  with BtnInfo do
  begin
    { Button State }
    if fsState and TBSTATE_ENABLED = TBSTATE_ENABLED then
      include(FState, bsEnabled);
    if fsState and TBSTATE_PRESSED = TBSTATE_PRESSED then
      include(FState, bsPressed);
    if fsState and TBSTATE_CHECKED = TBSTATE_CHECKED then
      include(FState, bsChecked);
    if fsState and TBSTATE_HIDDEN = TBSTATE_HIDDEN then
      include(FState, bsHidden);

    { Button Style }
    if fsStyle and TBSTYLE_BUTTON = TBSTYLE_BUTTON then
      include(FStyle, bsBtn);
    if fsStyle and TBSTYLE_SEP = TBSTYLE_SEP then
      include(FStyle, bsSep);
    if fsStyle and TBSTYLE_CHECK = TBSTYLE_CHECK then
      include(FStyle, bsCheck);
    if fsStyle and TBSTYLE_GROUP = TBSTYLE_GROUP then
      include(FStyle, bsGroup);
    if fsStyle and TBSTYLE_CHECKGROUP = TBSTYLE_CHECKGROUP then
      include(FStyle, bsCheckGroup);
    if (fsStyle and TBSTYLE_DROPDOWN = TBSTYLE_DROPDOWN) or (fsStyle and BTNS_WHOLEDROPDOWN = BTNS_WHOLEDROPDOWN) then
      include(FStyle, bsDropDown);

  end;

end;

function TSysToolbarStyleHook.TSysToolbarButton.GetItemRect: TRect;
begin
  Result := TRect.Empty;
  if not BOOL(SendMessage(FParent.Handle, TB_GETITEMRECT, FIndex, LParam(@Result))) then
    Result := TRect.Empty;
end;

function TSysToolbarStyleHook.TSysToolbarButton.GetDropDownWidth: Integer;
var
  R: TRect;
begin
  if BOOL(SendMessage(FParent.Handle, TB_GETITEMDROPDOWNRECT, FIndex, LParam(@R))) then
    Result := R.Right - R.Left
  else
    Result := 15; // default width when runtime themes are enabled
end;

{$ENDREGION}
{ TSysProgressBarStyleHook }

constructor TSysProgressBarStyleHook.Create(AHandle: THandle);
begin
  inherited;
  if (SysControl.Style And PBS_VERTICAL) <> 0 then
    FOrientation := pbVertical
  else
    FOrientation := pbHorizontal;
  // DoubleBuffered := True;
  OverridePaint := True;
  // OverrideEraseBkgnd :=True;
  // FLastPos:=-1;
  FStep := 0;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 100;
  FTimer.Enabled := False;
  // if ((SysControl.Style And PBS_MARQUEE) <> 0) then
  begin
    FTimer.OnTimer := TimerAction;
    FTimer.Enabled := True;
  end;
end;

destructor TSysProgressBarStyleHook.Destroy;
begin
  FTimer.Free;
  inherited;
end;

function TSysProgressBarStyleHook.GetBarRect: TRect;
begin
  Result := TRect.Create(0, 0, SysControl.Width, SysControl.Height);
  InflateRect(Result, -BorderWidth, -BorderWidth);
end;

function TSysProgressBarStyleHook.GetBorderWidth: Integer;
begin
  Result := 0;
end;

function TSysProgressBarStyleHook.GetMax: Integer;
begin
  Result := SendMessage(Handle, PBM_GetRange, 0, 0);
end;

function TSysProgressBarStyleHook.GetMin: Integer;
begin
  Result := SendMessage(Handle, PBM_GetRange, 1, 0);
end;

function TSysProgressBarStyleHook.GetOrientation: TProgressBarOrientation;
begin
  Result := pbHorizontal;
  if (Handle <> 0) and (GetWindowLong(Handle, GWL_STYLE) and PBS_VERTICAL = PBS_VERTICAL) then
    Result := pbVertical;
end;

function TSysProgressBarStyleHook.GetPercent: Single;
var
  LMin, LMax, LPos: Integer;
begin
  LMin := Min;
  LMax := Max;
  LPos := Position;
  if (LMin >= 0) and (LPos >= LMin) and (LMax >= LPos) and (LMax - LMin <> 0) then
    Result := (LPos - LMin) / (LMax - LMin)
  else
    Result := 0;
end;

function TSysProgressBarStyleHook.GetPosition: Integer;
begin
  Result := SendMessage(Handle, PBM_GETPOS, 0, 0);
end;

procedure TSysProgressBarStyleHook.Paint(Canvas: TCanvas);
var
  LDetails: TThemedElementDetails;
begin

  // if ((SysControl.Style And PBS_MARQUEE) <> 0) or ((FLastPos=-1)  or (Position<FLastPos)) then
  // begin
  if StyleServices.Available then
  begin
    LDetails.Element := teProgress;
    if StyleServices.HasTransparentParts(LDetails) then
      StyleServices.DrawParentBackground(Handle, Canvas.Handle, LDetails, False);
  end;

  PaintFrame(Canvas);
  // end;
  PaintBar(Canvas);
end;

procedure TSysProgressBarStyleHook.PaintBackground(Canvas: TCanvas);
begin
  inherited;
end;

procedure TSysProgressBarStyleHook.PaintBar(Canvas: TCanvas);
var
  FillR, LRect: TRect;
  LWidth, LPos: Integer;
  LDetails: TThemedElementDetails;
begin
  LRect := BarRect;

  if ((SysControl.Style And PBS_MARQUEE) <> 0) then
  begin
    InflateRect(LRect, -2, -2);
    if Orientation = pbHorizontal then
      LWidth := LRect.Width
    else
      LWidth := LRect.Height;

    LPos := Round(LWidth * 0.05);
    FillR := LRect;
    if Orientation = pbHorizontal then
    begin
      FillR.Right := FillR.Left + LPos;
      LDetails := StyleServices.GetElementDetails(tpChunk);
    end
    else
    begin
      FillR.Top := FillR.Bottom - LPos;
      LDetails := StyleServices.GetElementDetails(tpChunkVert);
    end;

    FillR.SetLocation(FStep * FillR.Width, FillR.Top);
    StyleServices.DrawElement(Canvas.Handle, LDetails, FillR);
    // Inc(FStep,1);
    // if FStep mod 20=0 then
    // FStep:=0;
  end
  else
  begin
    InflateRect(LRect, -2, -2);
    if Orientation = pbHorizontal then
      LWidth := LRect.Width
    else
      LWidth := LRect.Height;
    LPos := Round(LWidth * GetPercent);
    // FLastPos := GetPosition;
    FillR := LRect;
    if Orientation = pbHorizontal then
    begin
      FillR.Right := FillR.Left + LPos;
      LDetails := StyleServices.GetElementDetails(tpChunk);
    end
    else
    begin
      FillR.Top := FillR.Bottom - LPos;
      LDetails := StyleServices.GetElementDetails(tpChunkVert);
    end;
    StyleServices.DrawElement(Canvas.Handle, LDetails, FillR);
  end;
end;

procedure TSysProgressBarStyleHook.PaintFrame(Canvas: TCanvas);
var
  R: TRect;
  Details: TThemedElementDetails;
begin
  if not StyleServices.Available then
    Exit;
  R := BarRect;
  if Orientation = pbHorizontal then
    Details := StyleServices.GetElementDetails(tpBar)
  else
    Details := StyleServices.GetElementDetails(tpBarVert);
  StyleServices.DrawElement(Canvas.Handle, Details, R);
end;

procedure TSysProgressBarStyleHook.TimerAction(Sender: TObject);
var
  LCanvas: TCanvas;
  LHandle: THandle;
begin
  // if StyleServices.Available and ((SysControl.Style And PBS_MARQUEE) <> 0) then
  // begin
  LHandle := 0;
  LCanvas := TCanvas.Create;
  try
    LHandle := GetWindowDC(Self.Handle);

    if LHandle<>0 then
    begin
      LCanvas.Handle := LHandle;

      if SysControl.Visible then
      begin
        PaintFrame(LCanvas);
        PaintBar(LCanvas);
      end;
    end;

      inc(FStep, 1);
      if FStep mod 20 = 0 then
        FStep := 0;

  finally
   if LHandle<>0 then
    ReleaseDC(Handle, LHandle);
    LCanvas.Handle := 0;
    LCanvas.Free;
  end;

  // end
  // else
  // FTimer.Enabled := False;
end;

procedure TSysProgressBarStyleHook.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  Message.Result := 0;
  Handled := True;
end;

procedure TSysProgressBarStyleHook.WndProc(var Message: TMessage);
begin
  // Addlog(Format('TSysProgressBarStyleHook $0x%x %s', [SysControl.Handle, WM_To_String(Message.Msg)]));
  //
  case Message.Msg of
    WM_TIMER:; // avoid flicker in progress bar and memory increased;

  else inherited;
  end;

end;

{ TSysRichEditStyleHook }

constructor TSysRichEditStyleHook.Create(AHandle: THandle);
begin
  inherited;

{$IF CompilerVersion > 23}
  StyleElements := [seBorder];
{$ELSE}
  OverridePaintNC := True;
  OverrideFont := False;
{$IFEND}
end;

procedure TSysRichEditStyleHook.EMSetBkgndColor(var Message: TMessage);
begin
  Message.LParam := Color;
  Handled := False;
end;

function TSysRichEditStyleHook.GetBorderSize: TRect;
begin
  if SysControl.HasBorder then
    Result := Rect(2, 2, 2, 2);
end;

procedure TSysRichEditStyleHook.UpdateColors;
var
  cf: TCharFormat2;
const
  TextColor: array [Boolean] of TStyleFont = (sfEditBoxTextDisabled, sfEditBoxTextNormal);
  BkColor: array [Boolean] of TStyleColor = (scEditDisabled, scEdit);
begin
  Color := ColorToRGB(StyleServices.GetStyleColor(scEdit));
  FontColor := ColorToRGB(StyleServices.GetStyleFontColor(TextColor[SysControl.Enabled]));
  BackColor := ColorToRGB(StyleServices.GetStyleColor(BkColor[SysControl.Enabled]));

  ZeroMemory(@cf, sizeof(TCharFormat2));
  cf.cbSize := sizeof(TCharFormat2);
  cf.dwMask := CFM_ALL;
  { Need to send this message .. }
  SendMessage(Handle, EM_SETBKGNDCOLOR, 0, 0);
  SendMessage(Handle, EM_GETCHARFORMAT, SCF_DEFAULT, LParam(@cf));
  SendMessage(Handle, EM_SETCHARFORMAT, SCF_DEFAULT, LParam(@cf));
end;

procedure TSysRichEditStyleHook.EMSetCharFormat(var Message: TMessage);
type
  PCharFormat2 = ^TCharFormat2;
var
  Format: PCharFormat2;
begin
  Format := PCharFormat2(Message.LParam);
  Format.crTextColor := FontColor;
  Format.crBackColor := BackColor;
  Format.dwEffects := Format.dwEffects and not CFE_AUTOCOLOR;
  Handled := False;
end;

procedure TSysRichEditStyleHook.WndProc(var Message: TMessage);
begin
  inherited;
end;

{ TSysReBarStyleHook }

constructor TSysReBarStyleHook.Create(AHandle: THandle);
begin
  inherited;
  OverrideEraseBkgnd := True;
  OverridePaint := True;
  OverridePaintNC := True;
end;

function TSysReBarStyleHook.GetBandBorder(const Index: Integer): TRect;
begin
  SendMessage(Handle, RB_GETBANDBORDERS, Index, IntPtr(@Result));
end;

function TSysReBarStyleHook.GetBandCount: Integer;
begin
  Result := SendMessage(Handle, RB_GETBANDCOUNT, 0, 0);
end;

function TSysReBarStyleHook.GetBandRect(const Index: Integer): TRect;
begin
  Result := Rect(0, 0, 0, 0);
  SendMessage(Handle, RB_GETRECT, Index, IntPtr(@Result));
end;

function SizeOfReBarBandInfo: Integer;
var
  ReBarBandInfo: TReBarBandInfo;
begin
  ZeroMemory(@ReBarBandInfo, sizeof(ReBarBandInfo));
  if GetComCtlVersion >= $60001 then
    Result := sizeof(TReBarBandInfo)
  else
    // Platforms prior to Vista do not support the fields rcChevronLocation & uChevronState
    Result := sizeof(ReBarBandInfo) - sizeof(ReBarBandInfo.rcChevronLocation) - sizeof(ReBarBandInfo.uChevronState);
end;

function TSysReBarStyleHook.GetBandText(const Index: Integer): string;
const
  BufSize = 255;
var
  Info: TReBarBandInfo;
  Buffer: array [0 .. BufSize - 1] of Char;
begin
  FillChar(Info, sizeof(Info), 0);
  Info.cbSize := SizeOfReBarBandInfo;
  // Size differs depending on OS and ComCtl32.dll version
  Info.fMask := RBBIM_TEXT;
  Info.lpText := @Buffer;
  Info.cch := BufSize;
  if BOOL(SendMessage(Handle, RB_GETBANDINFO, Index, IntPtr(@Info))) then
    Result := Info.lpText
  else
    Result := '';
end;

procedure TSysReBarStyleHook.Paint(Canvas: TCanvas);
var
  i: Integer;
  R, Margin, LTextRect: TRect;
  S: string;
  Details: TThemedElementDetails;
begin
  for i := 0 to GetBandCount - 1 do
  begin
    R := GetBandRect(i);
    Margin := GetBandBorder(i);
    InflateRect(R, 1, 1);
    if R.Top < 0 then
      R.Top := 0;
    if R.Left < 0 then
      R.Left := 0;
    if R.Right > SysControl.ClientRect.Right then
      R.Right := SysControl.ClientRect.Right;
    if R.Bottom > SysControl.ClientRect.Bottom then
      R.Bottom := SysControl.ClientRect.Bottom;
    { band }
    Details := StyleServices.GetElementDetails(trBand);
    StyleServices.DrawElement(Canvas.Handle, Details, R);
    { text }
    LTextRect := Rect(R.Left + 10, R.Top, R.Left + Margin.Left, R.Bottom);

    S := GetBandText(i);
    if S <> '' then
      DrawControlText(Canvas, Details, S, LTextRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE);

    { gripper }
    R := Rect(R.Left + 2, R.Top + 2, R.Left + 6, R.Bottom - 2);
    Details := StyleServices.GetElementDetails(trGripper);
    StyleServices.DrawElement(Canvas.Handle, Details, R);
  end;
end;

procedure TSysReBarStyleHook.PaintBackground(Canvas: TCanvas);
var
  LRect: TRect;
  LDetails: TThemedElementDetails;
begin
  LRect := Rect(0, 0, SysControl.ClientWidth, SysControl.ClientHeight);
  InflateRect(LRect, 2, 2);
  LDetails.Element := teToolBar;
  LDetails.Part := 0;
  if StyleServices.HasTransparentParts(LDetails) then
    StyleServices.DrawParentBackground(Handle, Canvas.Handle, LDetails, False);
  StyleServices.DrawElement(Canvas.Handle, LDetails, LRect);
end;

procedure TSysReBarStyleHook.PaintNC(Canvas: TCanvas);
var
  LDetails: TThemedElementDetails;
begin
  ExcludeClipRect(Canvas.Handle, 2, 2, SysControl.Width - 2, SysControl.Height - 2);
  Canvas.Brush.Color := StyleServices.ColorToRGB(clBtnFace);
  Canvas.FillRect(Rect(0, 0, SysControl.Width, SysControl.Height));
  LDetails.Element := teToolBar;
  LDetails.Part := 0;
  StyleServices.DrawElement(Canvas.Handle, LDetails, Rect(0, 0, SysControl.Width, SysControl.Height));
end;

procedure TSysReBarStyleHook.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_SIZE:
      begin
        CallDefaultProc(Message);
        Invalidate;
        Handled := True;
      end;
  else inherited;
  end;
end;

{ TSysStatusBarStyleHook }

constructor TSysStatusBarStyleHook.Create(AHandle: THandle);
begin
  inherited;
  OverridePaint := True;
  // DoubleBuffered := True;
end;

procedure TSysStatusBarStyleHook.Paint(Canvas: TCanvas);
const
  AlignStyles: array [TAlignment] of Integer = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  R, R1: TRect;
  Res, Count, i: Integer;
  Idx, Flags: Cardinal;
  Details: TThemedElementDetails;
  LText: string;
  Borders: array [0 .. 2] of Integer;
begin
  Details := StyleServices.GetElementDetails(tsStatusRoot);
  StyleServices.DrawElement(Canvas.Handle, Details, Rect(0, 0, SysControl.Width, SysControl.Height));

  if SendMessage(Handle, SB_ISSIMPLE, 0, 0) > 0 then
  begin
    R := SysControl.ClientRect;
    FillChar(Borders, sizeof(Borders), 0);
    SendMessage(Handle, SB_GETBORDERS, 0, IntPtr(@Borders));
    R.Left := Borders[0] + Borders[2];
    R.Top := Borders[1];
    R.Bottom := R.Bottom - Borders[1];
    R.Right := R.Right - Borders[2];

    Details := StyleServices.GetElementDetails(tsPane);
    StyleServices.DrawElement(Canvas.Handle, Details, R);

    R1 := SysControl.ClientRect;
    R1.Left := R1.Right - R.Height;
    Details := StyleServices.GetElementDetails(tsGripper);
    StyleServices.DrawElement(Canvas.Handle, Details, R1);
    Details := StyleServices.GetElementDetails(tsPane);
    SetLength(LText, Word(SendMessage(Handle, SB_GETTEXTLENGTH, 0, 0)));
    if Length(LText) > 0 then
    begin
      SendMessage(Handle, SB_GETTEXT, 0, IntPtr(@LText[1]));
      Flags := SysControl.DrawTextBiDiModeFlags(DT_LEFT);
      DrawControlText(Canvas, Details, LText, R, Flags);
    end;
  end
  else
  begin
    Count := SendMessage(Handle, SB_GETPARTS, 0, 0);
    for i := 0 to Count - 1 do
    begin
      R := Rect(0, 0, 0, 0);
      SendMessage(Handle, SB_GETRECT, i, IntPtr(@R));
      if IsRectEmpty(R) then
        Exit;
      Details := StyleServices.GetElementDetails(tsPane);
      StyleServices.DrawElement(Canvas.Handle, Details, R);
      if i = Count - 1 then
      begin
        R1 := SysControl.ClientRect;
        R1.Left := R1.Right - R.Height;
        Details := StyleServices.GetElementDetails(tsGripper);
        StyleServices.DrawElement(Canvas.Handle, Details, R1);
      end;
      Details := StyleServices.GetElementDetails(tsPane);
      InflateRect(R, -1, -1);

      Flags := SysControl.DrawTextBiDiModeFlags(DT_LEFT);
      Idx := i;
      SetLength(LText, Word(SendMessage(Handle, SB_GETTEXTLENGTH, Idx, 0)));
      if Length(LText) > 0 then
      begin
        Res := SendMessage(Handle, SB_GETTEXT, Idx, IntPtr(@LText[1]));
        if (Res and SBT_OWNERDRAW = 0) then
          DrawControlText(Canvas, Details, LText, R, Flags);
      end;
    end;
  end;
end;

procedure TSysStatusBarStyleHook.WndProc(var Message: TMessage);
begin
  inherited;
end;

{ TSysTrackBarStyleHook }

constructor TSysTrackBarStyleHook.Create(AHandle: THandle);
begin
  inherited;
  OverridePaint := True;
  // OverrideEraseBkgnd :=True;
  DoubleBuffered := True;
  FThumbPressed := False;
end;

procedure TSysTrackBarStyleHook.Paint(Canvas: TCanvas);
var
  LDetails: TThemedElementDetails;
  TrackBarStyle: Cardinal;
  LThemedTrackBar: TThemedTrackBar;
  i, TickCount, TickStart, TickEnd, TickPos: Integer;
  LRect: TRect;
  LRect2: TRect;
  LThumbRect: TRect;
begin
  if not StyleServices.Available then
    Exit;

  LThemedTrackBar := ttbTrackBarDontCare;
  { Track }
  TrackBarStyle := GetWindowLong(Handle, GWL_STYLE);
  SendMessage(Handle, TBM_GETCHANNELRECT, 0, IntPtr(@LRect));
  if TrackBarStyle and TBS_VERT = 0 then
  begin
    LDetails := StyleServices.GetElementDetails(ttbTrack);
    StyleServices.DrawElement(Canvas.Handle, LDetails, LRect);
  end
  else
  begin
    LRect2 := LRect;
    LRect.Left := LRect2.Top;
    LRect.Top := LRect2.Left;
    LRect.Right := LRect2.Bottom;
    LRect.Bottom := LRect2.Right;
    LDetails := StyleServices.GetElementDetails(ttbTrackVert);
    StyleServices.DrawElement(Canvas.Handle, LDetails, LRect);
  end;

  SendMessage(Handle, TBM_GETCHANNELRECT, 0, IntPtr(@LRect));
  SendMessage(Handle, TBM_GETTHUMBRECT, 0, IntPtr(@LThumbRect));

  // Ticks
  if TrackBarStyle and TBS_NOTICKS = 0 then
  begin
    TickCount := SendMessage(Handle, TBM_GETNUMTICS, 0, 0);

    Canvas.Pen.Color := StyleServices.ColorToRGB(clBtnText);

    // First
    if TrackBarStyle and TBS_VERT = 0 then
    begin
      TickPos := LRect.Left + LThumbRect.Width div 2;
      if (TrackBarStyle and TBS_TOP = TBS_TOP) or (TrackBarStyle and TBS_BOTH = TBS_BOTH) then
      begin
        Canvas.MoveTo(TickPos, LRect.Top - 7);
        Canvas.LineTo(TickPos, LRect.Top - 3);
      end;
      if (TrackBarStyle and TBS_TOP = 0) or (TrackBarStyle and TBS_BOTH = TBS_BOTH) then
      begin
        Canvas.MoveTo(TickPos, LRect.Bottom + 3);
        Canvas.LineTo(TickPos, LRect.Bottom + 7);
      end;
      TickStart := TickPos;
    end
    else
    begin
      TickPos := LRect.Left + LThumbRect.Height div 2;
      if (TrackBarStyle and TBS_TOP = TBS_TOP) or (TrackBarStyle and TBS_BOTH = TBS_BOTH) then
      begin
        Canvas.MoveTo(LRect.Top - 7, TickPos);
        Canvas.LineTo(LRect.Top - 3, TickPos);
      end;
      if (TrackBarStyle and TBS_TOP = 0) or (TrackBarStyle and TBS_BOTH = TBS_BOTH) then
      begin
        Canvas.MoveTo(LRect.Bottom + 3, TickPos);
        Canvas.LineTo(LRect.Bottom + 7, TickPos);
      end;
      TickStart := TickPos;
    end;
    // last
    if TrackBarStyle and TBS_VERT = 0 then
    begin
      TickPos := LRect.Right - LThumbRect.Width div 2;
      if (TrackBarStyle and TBS_TOP = TBS_TOP) or (TrackBarStyle and TBS_BOTH = TBS_BOTH) then
      begin
        Canvas.MoveTo(TickPos, LRect.Top - 7);
        Canvas.LineTo(TickPos, LRect.Top - 3);
      end;
      if (TrackBarStyle and TBS_TOP = 0) or (TrackBarStyle and TBS_BOTH = TBS_BOTH) then
      begin
        Canvas.MoveTo(TickPos, LRect.Bottom + 3);
        Canvas.LineTo(TickPos, LRect.Bottom + 7);
      end;
      TickEnd := TickPos;
    end
    else
    begin
      TickPos := LRect.Right - LThumbRect.Height div 2;
      if (TrackBarStyle and TBS_TOP = TBS_TOP) or (TrackBarStyle and TBS_BOTH = TBS_BOTH) then
      begin
        Canvas.MoveTo(LRect.Top - 7, TickPos);
        Canvas.LineTo(LRect.Top - 3, TickPos);
      end;
      if (TrackBarStyle and TBS_TOP = 0) or (TrackBarStyle and TBS_BOTH = TBS_BOTH) then
      begin
        Canvas.MoveTo(LRect.Bottom + 3, TickPos);
        Canvas.LineTo(LRect.Bottom + 7, TickPos);
      end;
      TickEnd := TickPos;
    end;
    // ticks
    for i := 1 to TickCount - 1 do
    begin
      TickPos := TickStart + Round((TickEnd - TickStart) * (i / (TickCount - 1)));
      if TrackBarStyle and TBS_VERT = 0 then
      begin
        if (TrackBarStyle and TBS_TOP = TBS_TOP) or (TrackBarStyle and TBS_BOTH = TBS_BOTH) then
        begin
          Canvas.MoveTo(TickPos, LRect.Top - 6);
          Canvas.LineTo(TickPos, LRect.Top - 3);
        end;
        if (TrackBarStyle and TBS_TOP = 0) or (TrackBarStyle and TBS_BOTH = TBS_BOTH) then
        begin
          Canvas.MoveTo(TickPos, LRect.Bottom + 3);
          Canvas.LineTo(TickPos, LRect.Bottom + 6);
        end;
      end
      else
      begin
        if (TrackBarStyle and TBS_TOP = TBS_TOP) or (TrackBarStyle and TBS_BOTH = TBS_BOTH) then
        begin
          Canvas.MoveTo(LRect.Top - 6, TickPos);
          Canvas.LineTo(LRect.Top - 3, TickPos);
        end;
        if (TrackBarStyle and TBS_TOP = 0) or (TrackBarStyle and TBS_BOTH = TBS_BOTH) then
        begin
          Canvas.MoveTo(LRect.Bottom + 3, TickPos);
          Canvas.LineTo(LRect.Bottom + 6, TickPos);
        end;
      end;
    end;
  end;

  // Thumb
  if TrackBarStyle and TBS_NOTHUMB = 0 then
  begin
    SendMessage(Handle, TBM_GETTHUMBRECT, 0, IntPtr(@LRect));
    if not SysControl.Enabled then
    begin
      if TrackBarStyle and TBS_VERT = 0 then
      begin
        if TrackBarStyle and TBS_BOTH = TBS_BOTH then
          LThemedTrackBar := ttbThumbDisabled
        else if TrackBarStyle and TBS_TOP = TBS_TOP then
          LThemedTrackBar := ttbThumbTopDisabled
        else if TrackBarStyle and TBS_BOTTOM = TBS_BOTTOM then
          LThemedTrackBar := ttbThumbBottomDisabled;
      end
      else
      begin
        LThemedTrackBar := ttbThumbRightDisabled;
        if TrackBarStyle and TBS_TOP = TBS_TOP then
          LThemedTrackBar := ttbThumbLeftDisabled
        else if TrackBarStyle and TBS_BOTH = TBS_BOTH then
          LThemedTrackBar := ttbThumbVertDisabled;
      end;
    end
    else if FThumbPressed then
    begin
      if TrackBarStyle and TBS_VERT = 0 then
      begin
        if TrackBarStyle and TBS_BOTH = TBS_BOTH then
          LThemedTrackBar := ttbThumbPressed
        else if TrackBarStyle and TBS_TOP = TBS_TOP then
          LThemedTrackBar := ttbThumbTopPressed
        else if TrackBarStyle and TBS_BOTTOM = TBS_BOTTOM then
          LThemedTrackBar := ttbThumbBottomPressed;
      end
      else
      begin
        LThemedTrackBar := ttbThumbRightPressed;
        if TrackBarStyle and TBS_TOP = TBS_TOP then
          LThemedTrackBar := ttbThumbLeftPressed
        else if TrackBarStyle and TBS_BOTH = TBS_BOTH then
          LThemedTrackBar := ttbThumbVertPressed;
      end;
    end
    else if FMouseOnThumb then
    begin
      if TrackBarStyle and TBS_VERT = 0 then
      begin
        if TrackBarStyle and TBS_BOTH = TBS_BOTH then
          LThemedTrackBar := ttbThumbHot
        else if TrackBarStyle and TBS_TOP = TBS_TOP then
          LThemedTrackBar := ttbThumbTopHot
        else if TrackBarStyle and TBS_BOTTOM = TBS_BOTTOM then
          LThemedTrackBar := ttbThumbBottomHot;
      end
      else
      begin
        LThemedTrackBar := ttbThumbRightHot;
        if TrackBarStyle and TBS_TOP = TBS_TOP then
          LThemedTrackBar := ttbThumbLeftHot
        else if TrackBarStyle and TBS_BOTH = TBS_BOTH then
          LThemedTrackBar := ttbThumbVertHot;
      end;
    end
    else
    begin
      if TrackBarStyle and TBS_VERT = 0 then
      begin
        if TrackBarStyle and TBS_BOTH = TBS_BOTH then
          LThemedTrackBar := ttbThumbNormal
        else if TrackBarStyle and TBS_TOP = TBS_TOP then
          LThemedTrackBar := ttbThumbTopNormal
        else if TrackBarStyle and TBS_BOTTOM = TBS_BOTTOM then
          LThemedTrackBar := ttbThumbBottomNormal;
      end
      else
      begin
        LThemedTrackBar := ttbThumbRightNormal;
        if TrackBarStyle and TBS_TOP = TBS_TOP then
          LThemedTrackBar := ttbThumbLeftNormal
        else if TrackBarStyle and TBS_BOTH = TBS_BOTH then
          LThemedTrackBar := ttbThumbVertNormal;
      end;
    end;

    LDetails := StyleServices.GetElementDetails(LThemedTrackBar);
    StyleServices.DrawElement(Canvas.Handle, LDetails, LRect);
  end;

  if Focused then
    Canvas.DrawFocusRect(Rect(0, 0, SysControl.Width, SysControl.Height));
end;

procedure TSysTrackBarStyleHook.PaintBackground(Canvas: TCanvas);
var
  LDetails: TThemedElementDetails;
begin
  LDetails.Element := teTrackBar;
  StyleServices.DrawParentBackground(Handle, Canvas.Handle, LDetails, False);
end;

procedure TSysTrackBarStyleHook.WndProc(var Message: TMessage);
var
  LRect: TRect;
  NewValue: Boolean;
begin
  // Addlog(Format('TSysTrackBarStyleHook $0x%x %s', [SysControl.Handle, WM_To_String(Message.Msg)]));

  case Message.Msg of
    // WM_KEYUP,
    WM_VSCROLL, WM_HSCROLL, TBM_SETPOS:
      begin
        Invalidate;
        // CallDefaultProc(Message);
      end;

    WM_MOUSEMOVE:
      if GetWindowLong(Handle, GWL_STYLE) and TBS_NOTHUMB = 0 then
      begin
        SendMessage(Handle, TBM_GETTHUMBRECT, 0, IntPtr(@LRect));
        NewValue := PtInRect(LRect, Point(TWMMouse(Message).XPos, TWMMouse(Message).YPos));
        if NewValue <> FMouseOnThumb then
        begin
          FMouseOnThumb := NewValue;
          Invalidate;
        end;
      end;
    WM_LBUTTONUP:
      if GetWindowLong(Handle, GWL_STYLE) and TBS_NOTHUMB = 0 then
      begin
        FThumbPressed := False;
        Invalidate;
      end;
    WM_LBUTTONDOWN:
      if GetWindowLong(Handle, GWL_STYLE) and TBS_NOTHUMB = 0 then
      begin
        SendMessage(Handle, TBM_GETTHUMBRECT, 0, IntPtr(@LRect));
        if PtInRect(LRect, Point(TWMMouse(Message).XPos, TWMMouse(Message).YPos)) then
          FThumbPressed := True;
        Invalidate;
      end;

  else inherited;
  end;
end;

{ TSysUpDownStyleHook }

constructor TSysUpDownStyleHook.Create(AHandle: THandle);
begin
  inherited;
  OverridePaint := True;
  DoubleBuffered := True;
end;

destructor TSysUpDownStyleHook.Destroy;
begin
  inherited;
end;

function TSysUpDownStyleHook.GetOrientation: TUDOrientation;
begin
  if SysControl.Style and UDS_HORZ = UDS_HORZ then
    Result := udHorizontal
  else
    Result := udVertical;
end;

procedure TSysUpDownStyleHook.MouseLeave;
begin
  FMouseOnLeft := False;
  FMouseOnRight := False;
  Invalidate;
end;

procedure TSysUpDownStyleHook.Paint(Canvas: TCanvas);
var
  R: TRect;
  DrawState: TThemedScrollBar;
  Details: TThemedElementDetails;
begin
  if not StyleServices.Available then
    Exit;

  StyleServices.DrawParentBackground(Handle, Canvas.Handle, Details, False);

  if GetOrientation = udHorizontal then
  begin
    R := SysControl.ClientRect;
    R.Right := R.Left + R.Width div 2;
    if FLeftPressed then
      DrawState := tsArrowBtnLeftPressed
    else if FMouseOnLeft and MouseInControl then
      DrawState := tsArrowBtnLeftHot
    else
      DrawState := tsArrowBtnLeftNormal;

    Details := StyleServices.GetElementDetails(DrawState);
    StyleServices.DrawElement(Canvas.Handle, Details, R);

    R := SysControl.ClientRect;
    R.Left := R.Right - R.Width div 2;
    if FRightPressed then
      DrawState := tsArrowBtnRightPressed
    else if FMouseOnRight and MouseInControl then
      DrawState := tsArrowBtnRightHot
    else
      DrawState := tsArrowBtnRightNormal;

    Details := StyleServices.GetElementDetails(DrawState);
    StyleServices.DrawElement(Canvas.Handle, Details, R);
  end
  else
  begin
    R := SysControl.ClientRect;
    R.Bottom := R.Top + R.Height div 2;
    if FLeftPressed then
      DrawState := tsArrowBtnUpPressed
    else if FMouseOnLeft and MouseInControl then
      DrawState := tsArrowBtnUpHot
    else
      DrawState := tsArrowBtnUpNormal;

    Details := StyleServices.GetElementDetails(DrawState);
    StyleServices.DrawElement(Canvas.Handle, Details, R);

    R := SysControl.ClientRect;
    R.Top := R.Bottom - R.Height div 2;

    if FRightPressed then
      DrawState := tsArrowBtnDownPressed
    else if FMouseOnRight and MouseInControl then
      DrawState := tsArrowBtnDownHot
    else
      DrawState := tsArrowBtnDownNormal;

    Details := StyleServices.GetElementDetails(DrawState);
    StyleServices.DrawElement(Canvas.Handle, Details, R);
  end;
end;

procedure TSysUpDownStyleHook.WMLButtonDblClk(var Message: TWMMouse);
var
  R: TRect;
begin
  SetRedraw(False);
  CallDefaultProc(TMessage(Message));
  SetRedraw(True);
  if GetOrientation = udHorizontal then
  begin
    R := SysControl.ClientRect;
    R.Right := R.Left + R.Width div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FLeftPressed := True
    else
      FLeftPressed := False;

    R := SysControl.ClientRect;
    R.Left := R.Right - R.Width div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FRightPressed := True
    else
      FRightPressed := False;
  end
  else
  begin
    R := SysControl.ClientRect;
    R.Bottom := R.Top + R.Height div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FLeftPressed := True
    else
      FLeftPressed := False;

    R := SysControl.ClientRect;
    R.Top := R.Bottom - R.Height div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FRightPressed := True
    else
      FRightPressed := False;
  end;
  Invalidate;
  Handled := True;
end;

procedure TSysUpDownStyleHook.WMLButtonDown(var Message: TWMMouse);
var
  R: TRect;
begin
  SetRedraw(False);
  CallDefaultProc(TMessage(Message));
  SetRedraw(True);

  if GetOrientation = udHorizontal then
  begin
    R := SysControl.ClientRect;
    R.Right := R.Left + R.Width div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FLeftPressed := True
    else
      FLeftPressed := False;

    R := SysControl.ClientRect;
    R.Left := R.Right - R.Width div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FRightPressed := True
    else
      FRightPressed := False;
  end
  else
  begin
    R := SysControl.ClientRect;
    R.Bottom := R.Top + R.Height div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FLeftPressed := True
    else
      FLeftPressed := False;

    R := SysControl.ClientRect;
    R.Top := R.Bottom - R.Height div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FRightPressed := True
    else
      FRightPressed := False;
  end;

  Invalidate;
  Handled := True;
end;

procedure TSysUpDownStyleHook.WMLButtonUp(var Message: TWMMouse);
begin
  SetRedraw(False);
  CallDefaultProc(TMessage(Message));
  SetRedraw(True);
  FLeftPressed := False;
  FRightPressed := False;
  Invalidate;
  Handled := True;
end;

procedure TSysUpDownStyleHook.WMMouseMove(var Message: TWMMouse);
var
  R: TRect;
  FOldMouseOnLeft, FOldMouseOnRight: Boolean;
begin
  inherited;
  CallDefaultProc(TMessage(Message));

  FOldMouseOnLeft := FMouseOnLeft;
  FOldMouseOnRight := FMouseOnRight;

  if GetOrientation = udHorizontal then
  begin
    R := SysControl.ClientRect;
    R.Right := R.Left + R.Width div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FMouseOnLeft := True
    else
      FMouseOnLeft := False;

    R := SysControl.ClientRect;
    R.Left := R.Right - R.Width div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FMouseOnRight := True
    else
      FMouseOnRight := False;
  end
  else
  begin
    R := SysControl.ClientRect;
    R.Bottom := R.Top + R.Height div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FMouseOnLeft := True
    else
      FMouseOnLeft := False;

    R := SysControl.ClientRect;
    R.Top := R.Bottom - R.Height div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FMouseOnRight := True
    else
      FMouseOnRight := False;
  end;

  if (FOldMouseOnLeft <> FMouseOnLeft) and (FOldMouseOnRight <> FMouseOnRight) then
    Invalidate;

  Handled := True;
end;

procedure TSysUpDownStyleHook.WndProc(var Message: TMessage);
begin
  inherited;
end;



{ TSysLinkStyleHook }
{
Debug Output: TSysLinkStyleHook WM_WINDOWPOSCHANGING Process ThemedSysControls.exe (1800)
Debug Output: TSysLinkStyleHook WM_NCCALCSIZE Process ThemedSysControls.exe (1800)
Debug Output: TSysLinkStyleHook WM_CHILDACTIVATE Process ThemedSysControls.exe (1800)
Debug Output: TSysLinkStyleHook WM_WINDOWPOSCHANGED Process ThemedSysControls.exe (1800)
Debug Output: TSysLinkStyleHook Unknown(067C) Process ThemedSysControls.exe (1800)
}
constructor TSysLinkStyleHook.Create(AHandle: THandle);
var
  Style: DWORD;
begin
  Style := GetWindowLongPtr(AHandle, GWL_STYLE);
  if (Style and SS_ICON <> SS_ICON) and (Style and SS_BITMAP <> SS_BITMAP) then

    inherited;

{$IF CompilerVersion > 23}
  StyleElements := [seFont, seBorder, seClient];
{$ELSE}
  OverridePaint := True;
  OverridePaintNC := True;
  OverrideFont := True;
{$IFEND}
  UpdateColors;
end;

destructor TSysLinkStyleHook.Destroy;
begin

  inherited;
end;

procedure TSysLinkStyleHook.Paint(Canvas: TCanvas);
const
  States: array [Boolean] of TThemedTextLabel = (ttlTextLabelDisabled,
    ttlTextLabelNormal);
var
  LDetails: TThemedElementDetails;
  LRect: TRect;
  s : string;
begin
  LRect := SysControl.ClientRect;
  if GetBkMode(Canvas.Handle) = TRANSPARENT then
  begin
    LDetails := StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);
    StyleServices.DrawParentBackground(Handle, Canvas.Handle, LDetails, False);
    Canvas.Brush.Style := bsClear;
  end
  else
  begin
    Canvas.Brush.Color := StyleServices.GetStyleColor(scWindow);
    Canvas.FillRect(LRect);
  end;

  LDetails := StyleServices.GetElementDetails(States[SysControl.Enabled]);
  Canvas.Font := SysControl.Font;
  s:=SysControl.Text;
  //OutputDebugString(PChar('Text  '+s));
  DrawText(Canvas.Handle, LDetails, s, LRect, TextFormat);
end;


procedure TSysLinkStyleHook.PaintNC(Canvas: TCanvas);
var
  LRect: TRect;
  LBitMap: TBitmap;
begin
  if IsFrameOrLine then
  begin
    LRect := Rect(0, 0, SysControl.Width, SysControl.Height);
    LBitMap := TBitmap.Create;
    try
      LBitMap.Width := LRect.Width;
      LBitMap.Height := LRect.Height;
      Frame3D(LBitMap.Canvas, LRect, StyleServices.ColorToRGB(clBtnShadow),
        StyleServices.ColorToRGB(clBtnHighLight), 1);
      ExcludeClipRect(Canvas.Handle, 1, 1, SysControl.Width - 1,
        SysControl.Height - 1);
      Canvas.Draw(0, 0, LBitMap);
    finally
      LBitMap.Free;
    end;
  end;
end;

procedure TSysLinkStyleHook.WMNCCalcSize(var Message: TWMNCCalcSize);
begin

end;

procedure TSysLinkStyleHook.WndProc(var Message: TMessage);
begin
  //OutputDebugString(PChar('TSysLinkStyleHook '+WM_To_String(Message.Msg)+' Handle '+IntToHex(SysControl.Handle, 8)));

  case Message.Msg of

//   $067C :
//      begin
//        CallDefaultProc(Message);
//        if SysControl.Visible then
//          Invalidate;
//      end;

    WM_SETTEXT:
      begin
        CallDefaultProc(Message);
        if SysControl.Visible then
          Invalidate;
      end;

    WM_ENABLE:
      if SysControl.Visible then
        Invalidate;

    WM_PAINT:
      begin
        if OverridePaint and StyleServicesEnabled then
        begin
          if (IsText and (Length(SysControl.Text) > 0)) then
            inherited
          else
            CallDefaultProc(Message);
        end
        else
          CallDefaultProc(Message);
      end;

  else
    inherited;
  end;
end;

initialization

if StyleServices.Available then
begin
  with TSysStyleManager do
  begin
    RegisterSysStyleHook(TOOLBARCLASSNAME, TSysToolbarStyleHook);
    RegisterSysStyleHook(WC_LISTVIEW, TSysListViewStyleHook);
    RegisterSysStyleHook(WC_TABCONTROL, TSysTabControlStyleHook);
    RegisterSysStyleHook(WC_TREEVIEW, TSysTreeViewStyleHook);
    {$IFNDEF USE_Vcl.Styles.Hooks}
    RegisterSysStyleHook(PROGRESS_CLASS, TSysProgressBarStyleHook);
    {$ENDIF}
    RegisterSysStyleHook('RichEdit20A', TSysRichEditStyleHook);
    RegisterSysStyleHook('RichEdit20W', TSysRichEditStyleHook);
    RegisterSysStyleHook('RichEdit30A', TSysRichEditStyleHook);
    RegisterSysStyleHook('RichEdit30W', TSysRichEditStyleHook);
    RegisterSysStyleHook('RichEdit41A', TSysRichEditStyleHook);
    RegisterSysStyleHook('RichEdit41W', TSysRichEditStyleHook);
    RegisterSysStyleHook('RichEdit50A', TSysRichEditStyleHook);
    RegisterSysStyleHook('RichEdit50W', TSysRichEditStyleHook);
    RegisterSysStyleHook(REBARCLASSNAME, TSysReBarStyleHook);
    RegisterSysStyleHook(STATUSCLASSNAME, TSysStatusBarStyleHook);
    RegisterSysStyleHook(TRACKBAR_CLASS, TSysTrackBarStyleHook);
    RegisterSysStyleHook(UPDOWN_CLASS, TSysUpDownStyleHook);
  end;
end;

finalization

with TSysStyleManager do
begin
  UnRegisterSysStyleHook(TOOLBARCLASSNAME, TSysToolbarStyleHook);
  UnRegisterSysStyleHook(WC_LISTVIEW, TSysListViewStyleHook);
  UnRegisterSysStyleHook(WC_TABCONTROL, TSysTabControlStyleHook);
  UnRegisterSysStyleHook(WC_TREEVIEW, TSysTreeViewStyleHook);
  {$IFNDEF USE_Vcl.Styles.Hooks}
  UnRegisterSysStyleHook(PROGRESS_CLASS, TSysProgressBarStyleHook);
  {$ENDIF}

  UnRegisterSysStyleHook('RichEdit20A', TSysRichEditStyleHook);
  UnRegisterSysStyleHook('RichEdit20W', TSysRichEditStyleHook);
  UnRegisterSysStyleHook('RichEdit30A', TSysRichEditStyleHook);
  UnRegisterSysStyleHook('RichEdit30W', TSysRichEditStyleHook);
  UnRegisterSysStyleHook('RichEdit41A', TSysRichEditStyleHook);
  UnRegisterSysStyleHook('RichEdit41W', TSysRichEditStyleHook);
  UnRegisterSysStyleHook('RichEdit50A', TSysRichEditStyleHook);
  UnRegisterSysStyleHook('RichEdit50W', TSysRichEditStyleHook);
  UnRegisterSysStyleHook(REBARCLASSNAME, TSysReBarStyleHook);
  UnRegisterSysStyleHook(STATUSCLASSNAME, TSysStatusBarStyleHook);
  UnRegisterSysStyleHook(TRACKBAR_CLASS, TSysTrackBarStyleHook);
  UnRegisterSysStyleHook(UPDOWN_CLASS, TSysUpDownStyleHook);
end;

end.
